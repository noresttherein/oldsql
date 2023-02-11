package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.KeyKin.Ensign
import net.noresttherein.oldsql.model.Kin.{Delayed, Derived, Present}
import net.noresttherein.oldsql.model.KinFactory.{BaseDerivedKinFactory, BaseKinFactory, DerivedKinFactory}






/** A universal `Kin` implementation which always carries two information:
  * a [[net.noresttherein.oldsql.model.KeyKin.key key]], which is implementation specific identification of
  * the referenced logical value, and [[net.noresttherein.oldsql.model.KeyKin.ensign ensign]],
  * which is a discriminator value by which various implementations can recognise that a given `KeyKin` instance
  * carries a compatible key. Every [[net.noresttherein.oldsql.model.KinFactory KinFactory]] created
  * by the companion object requires a unique ensign, which is then passed to every created kin instance.
  * @see [[net.noresttherein.oldsql.model.KeyKin.DerivedKeyKin]]
  * @author Marcin Mościcki
  */
trait KeyKin[+T] extends Kin[T] {
	type Key
	val key :Key
	def ensign :Ensign[Key]

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[KeyKin[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :KeyKin[_] if other canEqual this =>
			ensign == other.ensign && key == other.key && toOption == other.toOption
		case _ => false
	}

	override def hashCode :Int = (ensign.hashCode * 31 + key.hashCode) * 31 + toOption.hashCode

	override def toString :String = toOption match {
		case Some(value) => "Present(" + ensign + "#" + key + "->" + value + ")"
		case _ if isMissing => "Missing(" + ensign + "#" + key + ")"
		case _ if isNonexistent => "Nonexistent(" + ensign + "#" + key + ")"
		case _ => "Absent(" + ensign + "#" + key + ")"
	}
}






object KeyKin {

	def apply[K, T](ensign :Ensign[K], key :K, value :Option[T], presumedMissing :Boolean = false) :KeyKin[T] =
		if (value.isDefined)
			new EagerKeyKin[K, T](ensign, key, value)
		else if (presumedMissing)
			missing(ensign, key)
		else new EagerKeyKin(ensign, key, value)

	def present[K, T](ensign :Ensign[K], key :K, value :T) :KeyKin[T] =
		new EagerKeyKin(ensign, key, Some(value))

	def absent[K](ensign :Ensign[K], key :K) :KeyKin[Nothing] =
		new EagerKeyKin(ensign, key, None)

	def missing[K](ensign :Ensign[K], key :K) :KeyKin[Nothing] =
		new EagerKeyKin(ensign, key, None) {
			override def isMissing = true
		}

	def nonexistent[K](ensign :Ensign[K], key :K) :KeyKin[Nothing] =
		new EagerKeyKin(ensign, key, None) {
			override def isNonexistent :Boolean = true
		}

	def delay[K, T](ensign :Ensign[K], key :K, value : => Option[T]) :KeyKin[T] =
		new LazyKeyKin(ensign, key, () => value)

	def unapply[K, E, T](kin :Kin[T])(implicit ensign :Ensign[K]) :Opt[K] =
		kin match {
			case key :KeyKin[_] if key.ensign == ensign => Got(key.key.asInstanceOf[K])
			case _ => Lack
		}


	/** A factory of references to entity of type `E` by keys `K` in some unspecified manner, which must
	  * be known to the client code from context. Returned factory creates instances
	  * of [[net.noresttherein.oldsql.model.Kin Kin]]`[P]`, but the value type can be changed
	  * to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `P`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
	  * a supertype of the 'real' factory [[net.noresttherein.oldsql.model.KeyKin.required required]],
	  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
	  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin instead of throwing
	  * a `NonexistentEntityException` as the latter one.
	  * @param ensign a discriminator value embedded in every kin in order to distinguish between instances
	  *               having different interpretations (different ways of interpreting the key `K`)
	  * @param key    an extractor function for keys of referenced entities `E`.
	  * @see [[net.noresttherein.oldsql.model.KeyKin.required[K,E]* apply]]`(ensign)(key)`.
	  */
	def apply[K, E](ensign :Ensign[K])(key :E => Option[K]) :KinFactory[K, E, E] =
		new KeyKinFactory[K, E, E](key)(ensign, ComposedOf.itself)

	/** A factory of references to entity of type `E` by keys `K` in some unspecified manner, which must
	  * be known to the client code from context.
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.One One]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
	  * and all obtained through it by adapting to other composite types, create only instances
	  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
	  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
	  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @param ensign a discriminator value embedded in every kin in order to distinguish between instances
	  *               having different interpretations (different ways of interpreting the key `K`)
	  * @param key    an extractor function for keys of referenced entities `E`.
	  * @see [[net.noresttherein.oldsql.model.KeyKin.apply[K,E]* apply]]`(ensign)(key)`.
	  */
	def required[K, E](ensign :Ensign[K])(key :E => Option[K]) :DerivedKinFactory[K, E, E] =
		new DerivedKeyKinFactory[K, E, E](ensign, key)


	/** An ensign manifesting the affiliation of a [[net.noresttherein.oldsql.model.KeyKin KeyKin]]: all
	  * kin from the same source, sharing the key type and interpretation share the same emblem.
	  * It serves a role which would normally require a dedicated `Kin` subclass, specific to an implementation
	  * of resolving its value. An implicit value of it is used by
	  * [[net.noresttherein.oldsql.model.KeyKin.unapply KeyKin.unapply]] to cast
	  * [[net.noresttherein.oldsql.model.KeyKin.key key]] of a matched `KeyKin` to the declared type `K`.
	  * It is typically unique to a [[net.noresttherein.oldsql.model.KinFactory KinFactory]] and used as its
	  * equivalency token.
	  * @tparam K the `key` type of all `KeyKin` carrying this ensign.
	  */
	class Ensign[K](val name :String) extends Serializable { factory =>
		def delay[E, T](key :K, value : => Option[T])(implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.delay(this, key, value)

		def apply[E, T](key :K, value :Option[T])(implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin(this, key, value)

		def apply[E, T](key :K)(implicit composition :T ComposableFrom E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.absent(this, key)

		def apply[E, T](key :K, value :T)(implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.present(this, key, value)

		def present[E, T](key :K, value :T)(implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.present(this, key, value)

		def absent[E, T](key :K)(implicit composition :T ComposableFrom E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.absent(this, key)

		def missing[E, T](key :K)(implicit composition :T ComposableFrom E) :DerivedKeyKin[K, E, T] =
			DerivedKeyKin.missing(this, key)


		def unapply[T](kin :Kin[T]) :Opt[K] = kin match {
			case key :KeyKin[_] if key.ensign == this => Got(key.key.asInstanceOf[K])
			case _ => Lack
		}

		override def equals(that :Any) :Boolean = that match {
			case ensign :Ensign[_] => ensign.name == name
			case _ => false
		}

		override def hashCode :Int = name.hashCode

		override def toString :String = name
	}



	abstract class DerivedKeyKin[K, E, T](override val ensign :Ensign[K], override val key :K)
	                                     (implicit override val composition :T ComposableFrom E)
		extends KeyKin[T] with Derived[E, T] {
		override type Key = K
	}

	object DerivedKeyKin {
		type Of[K, E] = { type Kin[T] = DerivedKeyKin[K, E, T] }

		def apply[K, E, T](ensign :Ensign[K], key :K, value :Option[T])
		                  (implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			new DerivedKeyKin[K, E, T](ensign, key)(composite.composer) {
				override val toOption = value

				override def items = toOption.map(composite.decomposer(_))
			}

		def present[K, E, T](ensign :Ensign[K], key :K, value :T)
		                    (implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			new DerivedKeyKin[K, E, T](ensign, key)(composite.composer) {
				override val get = value

				override def toOption = Some(get)

				override def items = Some(composite.decomposer(get))
			}

		def absent[K, E, T](ensign :Ensign[K], key :K)(implicit composition :T ComposableFrom E) :DerivedKeyKin[K, E, T] =
			missing(ensign, key)
//			new DerivedKeyKin[K, E, T](ensign, key) {
//				override def toOption = None
//
//				override def items = None
//			}

		def missing[K, E, T](ensign :Ensign[K], key :K)(implicit composition :T ComposableFrom E) :DerivedKeyKin[K, E, T] =
			new DerivedKeyKin[K, E, T](ensign, key) {
				override def toOption = None
				override def items = None
				override def isMissing = true
			}

		def delay[K, E, T](ensign :Ensign[K], key :K, value : => Option[T])
		                  (implicit composite :T ComposedOf E) :DerivedKeyKin[K, E, T] =
			new DerivedKeyKin[K, E, T](ensign, key)(composite.composer) {
				override lazy val toOption = value

				override def items = toOption.map(composite.decomposer(_))
			}


		def apply[K, E](ensign :Ensign[K])(key :E => Option[K]) :DerivedKinFactory[K, E, E] =
			new DerivedKeyKinFactory[K, E, E](ensign, key)

	}


	private class EagerKeyKin[K, T](override val ensign :Ensign[K], override val key :K, override val toOption :Option[T])
		extends KeyKin[T] { outer =>
		override type Key = K

		override def properties[E, X, C](prop :PropertyPath[E, X], as :C ComposableFrom X)
		                                (implicit decomposition :T DecomposableTo E) :Derived[X, C] =
			new EagerKeyKin(ensign, key, toOption.map(t => as(decomposition(t).map(prop.fun)))) with Derived[X, C] {
				override def composition = as //ComposableFrom.Properties(prop)(as)
				override def items = outer.toOption.map(decomposition(_).map(prop.fun))
			}
	}

	private class LazyKeyKin[K, T](override val ensign :Ensign[K], override val key :K, value :() => Option[T])
		extends Delayed[T](value) with KeyKin[T] {
		override type Key = K
	}



	private class KeyKinFactory[K, E, T](key :E => Option[K])
	                                    (implicit ensign :Ensign[K], override val result :T ComposedOf E)
		extends BaseKinFactory[K, E, T]
	{
		override def delay(key :K, value : => Option[T]) :Kin[T] = KeyKin.delay(ensign, key, value)

		override def absent(key :K) :Kin[T] = KeyKin.absent(ensign, key)
		override def missing(key :K) :Kin[T] = KeyKin.missing(ensign, key)

		override def keyFrom(item :E) :Opt[K] = key(item)

		override def keyOf(kin :Kin[T]) :Opt[K] =
			KeyKin.unapply(kin) orElse (Present.unapply(kin) match {
				case Got(x) => keyFor(x)
				case _ => Lack
			})


		override def required :KinFactory[K, E, T] = new DerivedKeyKinFactory(ensign, key)

		override def as[Y](implicit composition :ComposedOf[Y, E]) :KinFactory[K, E, Y] =
			new KeyKinFactory[K, E, Y](key)(ensign, composition)

		override def equivalencyToken :Any = ensign

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case fac :KeyKinFactory[_, _, _] if fac canEqual this =>
				fac.equivalencyToken == equivalencyToken && fac.result == result
			case _ => false
		}

		override def hashCode :Int = ensign.hashCode * 31 + result.hashCode

		override def toString :String = composition.toString + "?(" + ensign + ")"
	}



	private class DerivedKeyKinFactory[K, E, T](ensign :Ensign[K], key :E => Option[K])
	                                           (implicit override val result :T ComposedOf E)
		extends BaseDerivedKinFactory[K, E, T]
	{
		override def delay(key :K, value : => Option[T]) :DerivedKeyKin[K, E, T] = ensign.delay(key, value)
		override def apply(key :K, value :Option[T]) :DerivedKeyKin[K, E, T] = ensign(key, value)
		override def present(key :K, value :T) :DerivedKeyKin[K, E, T] = ensign(key, value)
		override def missing(key :K) :DerivedKeyKin[K, E, T] = ensign.missing(key)

		override def keyFrom(item :E) :Opt[K] = key(item)

		override def keyOf(kin :Kin[T]) :Opt[K] =
			ensign.unapply(kin) orElse (Present.unapply(kin) match {
				case Got(x) => keyFor(x)
				case _ => Lack
			})

		override def notRequired = new KeyKinFactory[K, E, T](key)(ensign, result)

		override def as[Y](implicit composition :Y ComposedOf E) :DerivedKinFactory[K, E, Y] =
			new DerivedKeyKinFactory[K, E, Y](ensign, key)


		override def equivalencyToken :Any = ensign

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case fac :DerivedKeyKinFactory[_, _, _] if fac canEqual this =>
				fac.equivalencyToken == equivalencyToken && fac.result == result
			case _ => false
		}

		override def hashCode :Int = ensign.hashCode * 31 + result.hashCode

		override def toString :String = composition.toString + "(" + ensign + ")"
	}

}


