package net.noresttherein.oldsql.schema.bits

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.Kin.{Absent, Derived, Present, Unknown}
import net.noresttherein.oldsql.model.KinFactory.{BaseDerivedKinFactory, BaseKinFactory, DerivedKinFactory}






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
  */ //todo: move to model, it uses nothing from the schema package
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

	def apply[E, T](key :Kin[Iterable[Kin[E]]])(implicit composition :T ComposableFrom E) :Derived[E, T] =
		if (key.isUnknown)
			Unknown()
		else if (key.isNonexistent)
			throw new IllegalArgumentException("Nonexistent cannot be proxied with TeleKin.")
		else new BrokerKin[E, T](key)

	def apply[E, T](key :Kin[Iterable[Kin[E]]], value :Option[T])
	               (implicit composition :T ComposableFrom E) :Derived[E, T] =
		if (value.isEmpty)
			TeleKin(key)
		else if (key.isNonexistent)
			throw new IllegalArgumentException("Nonexistent cannot be proxied with TeleKin.")
		else if (key.isUnknown)
			throw new IllegalArgumentException("Cannot create an Unknown present TeleKin.")
		else new BrokerKin[E, T](key) { //if key is Unknown, it is treated as a dumb present instance
			override lazy val toOption = value
		}

	def delay[E, T](key :Kin[Iterable[Kin[E]]], value: => Option[T])
	               (implicit composition :T ComposableFrom E) :Derived[E, T] =
//		if (key.isUnknown)
//			throw new IllegalArgumentException("Unknown cannot be proxied with TeleKin.")
		if (key.isNonexistent)
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



	/** A factory of references to values of a ''many-to-many'' relationship residing in another table,
	  * which is indirectly linked through an intermediate join table.
	  * [[net.noresttherein.oldsql.schema.bits.TeleKin TeleKin]] are simply flattened
	  * `Kin[Iterable[Kin[E]]]` and this factory handles them by composing two separate kin factories:
	  * for the final `Kin[E]` based on a `K2` key property value of the subject of the intermediate table,
	  * and one to the ''value of said kin property'' in all rows of the intermediate table matching key `K`.
	  *
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin Kin]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
	  * a supertype of the 'real' factory [[net.noresttherein.oldsql.model.Kin.Property.required required]],
	  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
	  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin instead of throwing
	  * a `NonexistentEntityException` as the latter one.
	  * @param keyFactory   a factory of kin to a variable number of `Kin[E]`. In the context of a ''many-to-many''
	  *                     relationship with a join table with foreign keys to two (or more) tables, this will likely
	  *                     be a factory of [[net.noresttherein.oldsql.model.Kin.Property.required Property]] `Kin`
	  *                     applying the selector `Link => Kin[E]` of the appropriate foreign key property
	  *                     to all entities from the join table referenced by yet another factory of `Kin[Link]`.
	  * @param valueFactory a factory of `Kin[E]` referencing the entities from the other side of the relationship,
	  *                     presumably by a foreign key in the join table.
	  * @see [[net.noresttherein.oldsql.schema.bits.TeleKin.required[K,K2,E]* apply]]`(keyFactory, valueFactory)`.
	  */
	def apply[K, K2, E](keyFactory :KinFactory[K, Kin[E], Iterable[Kin[E]]], valueFactory :KinFactory[K2, E, E])
			:KinFactory[Kin[Iterable[Kin[E]]], E, E] =
		new TeleKinFactory[K, K2, E, E](keyFactory, valueFactory)

	/** A factory of references to values of a ''many-to-many'' relationship residing in another table,
	  * which is indirectly linked through an intermediate join table.
	  * [[net.noresttherein.oldsql.schema.bits.TeleKin TeleKin]] are simply flattened
	  * `Kin[Iterable[Kin[E]]]` and this factory handles them by composing two separate kin factories:
	  * for the final `Kin[E]` based on a `K2` key property value of the subject of the intermediate table,
	  * and one to the ''value of said kin property'' in all rows of the intermediate table matching key `K`.
	  *
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.One One]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
	  * and all obtained through it by adapting to other composite types, create only instances
	  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
	  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
	  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @param keyFactory   a factory of kin to a variable number of `Kin[E]`. In the context of a ''many-to-many''
	  *                     relationship with a join table with foreign keys to two (or more) tables, this will likely
	  *                     be a factory of [[net.noresttherein.oldsql.model.Kin.Property.required Property]] `Kin`
	  *                     applying the selector `Link => Kin[E]` of the appropriate foreign key property
	  *                     to all entities from the join table referenced by yet another factory of `Kin[Link]`.
	  * @param valueFactory a factory of `Kin[E]` referencing the entities from the other side of the relationship,
	  *                     presumably by a foreign key in the join table.
	  * @see [[net.noresttherein.oldsql.schema.bits.TeleKin.apply[K,K2,E]* apply]]`(keyFactory, valueFactory)`.
	  */
	def required[K, K2, E](keyFactory :KinFactory[K, Kin[E], Iterable[Kin[E]]], valueFactory :KinFactory[K2, E, E])
			:DerivedKinFactory[Kin[Iterable[Kin[E]]], E, E] =
		new RequiredTeleKinFactory[K, K2, E, E](keyFactory, valueFactory)

	/** A factory of references to values of a ''many-to-many'' relationship residing in another table,
	  * which is indirectly linked through an intermediate join table.
	  * [[net.noresttherein.oldsql.schema.bits.TeleKin TeleKin]] are simply flattened
	  * `Kin[Iterable[Kin[E]]]` and this factory handles them by composing two separate kin factories:
	  * for the final `Kin[E]` based on a `K2` key property value of the subject of the intermediate table,
	  * and one to all rows of the intermediate table matching key `K`, which define the relationship.
	  *
	  * Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.One One]]`[E]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
	  * and all obtained through it by adapting to other composite types, create only instances
	  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
	  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
	  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @param linkKin   a factory for kin to mapped rows `J` of the relationship table referencing the 'owner'/origin
	  *                  of the relationship by a foreign key `K`.
	  * @param property  a property selector corresponding to the foreign key to the target table in the relationship
	  *                  table.
	  * @param targetKin a factory for kin to the target table based on the foreign key `K2` in the relationship
	  *                  table.
	  * @see [[net.noresttherein.oldsql.schema.bits.TeleKin.apply[K,K2,E]* apply]]`(keyFactory, valueFactory)`.
	  */
	def required[K, J :TypeTag, K2, E](linkKin :DerivedKinFactory[K, J, Iterable[Kin[J]]], property :J => Kin[E],
	                                   targetKin :KinFactory[K2, E, E])
			:DerivedKinFactory[Kin[Iterable[Kin[E]]], E, E] =
		required(Kin.Property(linkKin, property).in[Iterable], targetKin)




	private class BrokerKin[E, T](override val key :Kin[Iterable[Kin[E]]])
	                             (implicit override val composition :T ComposableFrom E)
		extends TeleKin[E, T]



	private class TeleKinFactory[K, K2, E, T](keyFactory :KinFactory[K, Kin[E], Iterable[Kin[E]]],
	                                          valueFactory :KinFactory[K2, E, E])
	                                         (implicit override val result :T ComposedOf E)
		extends BaseKinFactory[Kin[Iterable[Kin[E]]], E, T]
	{
		override def delay(key :Kin[Iterable[Kin[E]]], value : => Option[T]) :Derived[E, T] =
			TeleKin.delay(key, value)

		override def apply(key :Kin[Iterable[Kin[E]]], value :Option[T]) :Derived[E, T] = TeleKin(key, value)

		override def missing(key :Kin[Iterable[Kin[E]]]) :Derived[E, T] = key match {
			case Absent() => TeleKin(key)
			case Present(vals) if vals.forall(_.isAbsent) => TeleKin(key) //includes empty collections
			case _ => keyFactory.keyOf(key) match {
				case Got(k) => TeleKin(keyFactory.missing(k))
				case _ => TeleKin(key, None)
			}
		}

		override def absent(key :Kin[Iterable[Kin[E]]]) :Derived[E, T] = missing(key)

		override def present(value :T) :Derived[E, T] =
			TeleKin(Derived.present(result.decomposer(value).map(Derived.one)))


		override def keyFrom(item :E) :Opt[Kin[Iterable[Kin[E]]]] =
			Got(keyFactory.present(valueFactory.present(item)::Nil))

		override def keyFrom(items :Iterable[E]) :Opt[Kin[Iterable[Kin[E]]]] =
			Got(keyFactory.present(items.map(valueFactory.present)))

		override def keyOf(kin :Kin[T]) :Opt[Kin[Iterable[Kin[E]]]] = kin match {
			case Derived(TeleKin(k)) => Got(k)
			case Present(result.decomposer(values)) =>
				Got(keyFactory.present(values.map(valueFactory.present)))
			case _ => Lack
		}

		override def isRequired :Boolean = false

		override def required :DerivedKinFactory[Kin[Iterable[Kin[E]]], E, T] =
			new RequiredTeleKinFactory[K, K2, E, T](keyFactory.required, valueFactory.required)

		override def notRequired :KinFactory[Kin[Iterable[Kin[E]]], E, T] =
			if (isRequired) new TeleKinFactory[K, K2, E, T](keyFactory, valueFactory) else this


		override def as[Y](implicit composition :Y ComposedOf E) :KinFactory[Kin[Iterable[Kin[E]]], E, Y] =
			new TeleKinFactory[K, K2, E, Y](keyFactory, valueFactory)


		override def equivalencyToken :Any = (keyFactory, valueFactory)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TeleKinFactory[_, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :TeleKinFactory[_, _, _, _] if canEqual(other) && (other canEqual this) =>
				other.equivalencyToken == equivalencyToken && other.result == result
			case _ => false
		}

		override def hashCode :Int = (keyFactory.hashCode * 31 + valueFactory.hashCode) * 31 + result.hashCode

		override def toString :String =
			result.toString + (if (isRequired) "(" else "?(") + keyFactory + " -> " + valueFactory + ")"
	}



 	private class RequiredTeleKinFactory[K, K2, E, T](keyFactory :KinFactory[K, Kin[E], Iterable[Kin[E]]],
	                                                  valueFactory :KinFactory[K2, E, E])
	                                                 (implicit override val result :T ComposedOf E)
		extends TeleKinFactory[K, K2, E, T](keyFactory, valueFactory)
		   with BaseDerivedKinFactory[Kin[Iterable[Kin[E]]], E, T]
	{
		override def as[Y](implicit composition :Y ComposedOf E) :DerivedKinFactory[Kin[Iterable[Kin[E]]], E, Y] =
			new RequiredTeleKinFactory[K, K2, E, Y](keyFactory, valueFactory)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[RequiredTeleKinFactory[_, _, _, _]]
	}

}
