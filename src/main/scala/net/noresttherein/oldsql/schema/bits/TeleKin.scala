package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory}
import net.noresttherein.oldsql.model.Kin.{Derived, Present}
import net.noresttherein.oldsql.model.KinFactory.{BaseDerivedKinFactory, DerivedKinFactory}






/** A representation of a `Kin` for a collection of `Kin[E]` as a `Kin` for a composite `T ComposedOf E`.
  * Intuitively, this is used to implement ''many-to-many'' relationships, with the inner `Kin` mapping to rows
  * in an intermediate join table. It can be either completely absent (if `key` is absent), contain a collection
  * `Iterable[Kin[E]]` of absent, present or mixed kin. This `Kin` is present ''iff''
  * `key` is present and all kin inside it are also present. It can be matched specifically
  * with its companion object [[net.noresttherein.oldsql.model.TeleKin$ TeleKin]], but instances
  * with a present ''key'' (not necessarily present themselves) will also be matched by the more generic
  * [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]].
  * @tparam T exposed collection type with elements of type `E`.
  * @tparam E the entity type mapped to the table being the other side of the relationship and the item type
  *           of `T`.
  */
trait TeleKin[E, +T] extends Derived[E, T] {
	val key :Kin[Iterable[Kin[E]]]

	override def parts :Iterable[Derived[E, _]] =
		if (key.isPresent) key.get.map(_.recompose[E, E])
		else this::Nil

	override lazy val items :Option[Iterable[E]] = key.toOption match {
		case Some(kin) if kin.forall(_.isPresent) => Some(kin.map(_.get))
		case _ => None
	}

	override lazy val toOption :Option[T] = key.toOption match {
		case Some(kin) if kin.forall(_.isPresent) => Some(composition(kin.map(_.get)))
		case _ => None
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[TeleKin[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case broker :TeleKin[_, _] if broker canEqual this => key == broker.key && toOption == broker.toOption
		case _ => false
	}

	override lazy val hashCode :Int = key.hashCode * 31 + toOption.hashCode

	override def toString :String = toOption match {
		case Some(value) => "Present{" + key + "}(" + value + ")"
		case _ if isMissing => "Missing[" + composition + "]{" + key + "}"
		case _ if isNonexistent => "Nonexistent[" + composition + "]{" + key + "}"
		case _ => "Absent[" + composition + "]{" + key + "}"
	}
}






object TeleKin {

	def apply[E, T](key :Kin[Iterable[Kin[E]]])(implicit composition :T ComposableFrom E) :TeleKin[E, T] =
		if (key.isUnknown)
			throw new IllegalArgumentException("Unknown cannot be proxied with TeleKin.")
		else if (key.isNonexistent)
			throw new IllegalArgumentException("Nonexistent cannot be proxied with TeleKin.")
		else new BrokerKin[E, T](key)

	def apply[E, T](key :Kin[Iterable[Kin[E]]], value :Option[T])
	               (implicit composition :T ComposableFrom E) :TeleKin[E, T] =
		if (value.isEmpty) TeleKin(key)
		else new BrokerKin[E, T](key) {
			override lazy val toOption = value
		}

	def delay[E, T](key :Kin[Iterable[Kin[E]]], value: => Option[T])
	               (implicit composition :T ComposableFrom E) :TeleKin[E, T] =
		if (key.isUnknown)
			throw new IllegalArgumentException("Unknown cannot be proxied with TeleKin.")
		else if (key.isNonexistent)
			throw new IllegalArgumentException("Nonexistent cannot be proxied with TeleKin.")
		else
			new BrokerKin[E, T](key) {
				override lazy val toOption =
					if (value.isDefined) value
					else key.toOption match {
						case Some(kin) if kin.forall(_.isPresent) => Some(composition(kin.map(_.get)))
						case _ => None
					}
			}


	def unapply[E, T](kin :Derived[E, T]) :Opt[Kin[Iterable[Kin[E]]]] = kin match {
		case broker :TeleKin[E, T] => Got(broker.key)
		case _ => Lack
	}

	def unapply[T](kin :Kin[T]) :Opt[(Kin[Iterable[Kin[E]]], T ComposableFrom E) forSome { type E }] =
		kin match {
			case broker :TeleKin[e, T] => Got((broker.key, broker.composition))
			case _ => Lack
		}


	private class BrokerKin[E, T](override val key :Kin[Iterable[Kin[E]]])
	                             (implicit override val composition :T ComposableFrom E)
		extends TeleKin[E, T]





	class TeleKinFactory[K, K2, E, T](keyFactory :KinFactory[K, Kin[E], Iterable[Kin[E]]],
	                                  valueFactory :KinFactory[K2, E, E])
	                                 (implicit override val result :T ComposedOf E)
		extends BaseDerivedKinFactory[Kin[Iterable[Kin[E]]], E, T]
	{
		override def delay(key :Kin[Iterable[Kin[E]]], value : => Option[T]) :Derived[E, T] =
			TeleKin.delay(key, value)

		override def apply(key :Kin[Iterable[Kin[E]]], value :Option[T]) :Derived[E, T] = TeleKin(key, value)

		override def missing(key :Kin[Iterable[Kin[E]]]) :Derived[E, T] = TeleKin(key)

		override def present(value :T) :Derived[E, T] =
			TeleKin(Derived.present(result.decomposer(value).map(Derived.one)))


		override def keyFrom(item :E) :Opt[Kin[Iterable[Kin[E]]]] =
			Got(keyFactory.present(valueFactory.present(item)::Nil))

		override def keyFrom(items :Iterable[E]) :Opt[Kin[Iterable[Kin[E]]]] =
			Got(keyFactory.present(items.map(valueFactory.present)))

		override def keyOf(kin :Kin[T]) :Opt[Kin[Iterable[Kin[E]]]] = kin match {
			case Derived(TeleKin(k)) => Got(k.asInstanceOf[Kin[Iterable[Kin[E]]]])
			case Present(result.decomposer(values)) =>
				Got(keyFactory.present(values.map(valueFactory.present)))
			case _ => Lack
		}

//		override def required :KinFactory[Kin[Iterable[Kin[E]]], E, T] =
//			new TeleKinFactory[K, K2, E, T](keyFactory.required, valueFactory.required)
//				with RequiredKinFactory[Kin[Iterable[Kin[E]]], E, T, Kin[T]]
//
//		override def isRequired :Boolean = false

		override def as[Y](implicit composition :Y ComposedOf E) :DerivedKinFactory[Kin[Iterable[Kin[E]]], E, Y] =
			new TeleKinFactory[K, K2, E, Y](keyFactory, valueFactory)


		override def equivalencyToken :Any = (keyFactory, valueFactory)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TeleKinFactory[_, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :TeleKinFactory[_, _, _, _] if other canEqual this =>
				other.equivalencyToken == equivalencyToken && other.result == result
			case _ => false
		}

		override def hashCode :Int = (keyFactory.hashCode * 31 + valueFactory.hashCode) * 31 + result.hashCode

		override def toString :String =
			result.toString + (if (isRequired) "(" else "?(") + keyFactory + " -> " + valueFactory + ")"
	}

}
