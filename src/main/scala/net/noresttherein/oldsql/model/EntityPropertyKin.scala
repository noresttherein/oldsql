package net.noresttherein.oldsql.model

import scala.reflect.runtime.universe.{typeOf, TypeTag}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.Kin.{Derived, Nonexistent, One, Present}
import net.noresttherein.oldsql.model.KinFactory.{DerivedKinFactory, RequiredKinFactory}






/** A factory and matcher for [[net.noresttherein.oldsql.model.Kin kin]] for values of properties of other kin.
  * It uses [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Property Property]] and
  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Properties Properties]] compositions which
  * simply apply the given property function to another entity, or to all individual elements the argument kin
  * decomposes to, collecting the result. It is unique in that this process is obviously not reversible and
  * there is no matching [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]]. This means
  * that having a value of a certain property typically tells one nothing about which entity it came from,
  * and some methods of the [[net.noresttherein.oldsql.model.Kin.Derived Derived]] and
  * [[net.noresttherein.oldsql.model.GenericKinFactory GenericKinFactory]] come with restrictions.
  */
object EntityPropertyKin {

	/** Create a kin for a collection of values of property `property :P` of all individual elements `E`
	  * comprising the composite type `X` from kin `owners`.
	  */
	@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
	def apply[E, X, P, T](owners :Kin[X], property :PropertyPath[E, P])
	                     (implicit decomposition :X ComposedOf E, composition :T ComposableFrom P)
			:Derived[E, T] =
		apply(owners.recompose, property)

	/** Create a kin for a collection of values of property `property :P` of all individual elements `E`
	  * comprising the composite type `X` from kin `owners`.
	  */
	@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
	def apply[E, X, P, T](owners :Derived[E, X], property :PropertyPath[E, P])
	                     (implicit composition :T ComposableFrom P) :Derived[E, T] =
		new EntityPropertyKin[E, X, P, T](owners, property)

	/** Create a kin for the value of property `property` of the value `E` of kin `owner`. */
	@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
	def one[E, T](owner :Kin[E], property :PropertyPath[E, T]) :Derived[E, T] =
		new SingularPropertyKin(owner.recompose[E, E], property)

	/** Create a lazy kin for a collection of values of property `property :P` of all individual elements `E`
	  * comprising the composite type `X` from kin `owners`.
	  */
	@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
	def delay[E, X, P, T](owners :Derived[E, X], property :PropertyPath[E, P], value: => Option[T])
	                     (implicit composition :T ComposableFrom P) :Derived[E, T] =
		new EntityPropertyKin(owners, property) {
			override lazy val toOption = value orElse owner.items.map {
				items => propertyComposition(items.view.map(property.fun))
			}
		}

	/** Create a lazy kin for the value of property `property` of the value `E` of kin `owner`. */
	@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
	def delay[E, T](owner :Kin[E], property :PropertyPath[E, T], value: => Option[T]) :Derived[E, T] =
		if (owner.isUnknown)
			throw new IllegalArgumentException(s"Cannot create a kin for property $property of Unknown.")
		else
			new SingularPropertyKin[E, T](owner.recompose, property) {
				override lazy val toOption = value orElse owner.items.map {
					items => propertyComposition(items.view.map(property.fun))
				}
			}

	/** Matches kin instances created by this factory (or, indirectly,
	  * by [[net.noresttherein.oldsql.model.Kin.property property]] and
	  * [[net.noresttherein.oldsql.model.Kin.properties properties]] methods of `Kin`).
	  * Returns the kin for the owner(s) of the property, the property itself, and composition type class
	  * for assembling the result type ''from the value(s) of the property'' (which is not the same as
	  * [[net.noresttherein.oldsql.model.Kin.Derived.composition composition]] property of `Derived`).
	  */
	def unapply[E, T](kin :Derived[E, T])
			:Opt[(Derived[E, _], PropertyPath[E, P], T ComposableFrom P)] forSome { type X; type P } =
		kin match {
			case prop :EntityPropertyKin[E, x, p, T] => Got((prop.owner, prop.property, prop.propertyComposition))
			case _ => Lack
		}

	/** Matches kin instances created by this factory (or, indirectly,
	  * by [[net.noresttherein.oldsql.model.Kin.property property]] and
	  * [[net.noresttherein.oldsql.model.Kin.properties properties]] methods of `Kin`).
	  * Returns the kin for the owner(s) of the property, the property itself, and composition type class
	  * for assembling the result type ''from the value(s) of the property'' (which is not the same as
	  * [[net.noresttherein.oldsql.model.Kin.Derived.composition composition]] property of `Derived`).
	  */
	def unapply[T](kin :Kin[T])
			:Opt[(Derived[E, _], PropertyPath[E, P], T ComposableFrom P) forSome { type E; type P }] =
		kin match {
			case prop :EntityPropertyKin[e, x, p, T] => Got((prop.owner, prop.property, prop.propertyComposition))
			case _ => Lack
		}


	/** A factory of references to the value of property `property` of entity `E` referenced by `Kin` handled
	  * by `factory`. Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin Kin]]`[P]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `P`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
	  * a supertype of the 'real' factory [[net.noresttherein.oldsql.model.EntityPropertyKin.required required]],
	  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
	  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin instead of throwing
	  * a `NonexistentEntityException` as the latter one.
	  * @see [[net.noresttherein.oldsql.model.EntityPropertyKin.required[K,E,P,Y]* apply]]`(factory, property)`.
	  */
	def apply[K, E, P, Y, T](factory :DerivedKinFactory[K, E, Y], property :E => P)
	                        (implicit composition :T ComposableFrom P, entity :TypeTag[E]) :KinFactory[K, E, T] =
		new EntityPropertiesKinFactory[K, E, Y, P, T](factory, PropertyPath(property))

	/** A factory of references to the value of property `property` of entity `E` referenced by `Kin` handled
	  * by `factory`. Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[E, P]`,
	  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
	  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
	  * and all obtained through it by adapting to other composite types, create only instances
	  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
	  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
	  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @see [[net.noresttherein.oldsql.model.EntityPropertyKin.apply[K,E,P,Y]* apply]]`(factory, property)`.
	  */
	def required[K, E, P, Y, T](factory :DerivedKinFactory[K, E, Y], property :E => P)
	                           (implicit composition :T ComposableFrom P, entity :TypeTag[E])
			:DerivedKinFactory[K, E, T] =
		new EntityPropertiesDerivedFactory[K, E, Y, P, T](factory, PropertyPath(property))



	private class SingularPropertyKin[E, T](kin :Derived[E, E], prop :PropertyPath[E, T])
		extends EntityPropertyKin[E, E, T, T](kin, prop)
	{
		override val composition = ComposableFrom.Property(property)
		override lazy val toOption = kin.toOption.map(prop.fun)
		override def toString = kin.toString + "." + prop
	}



	private class EntityPropertiesKinFactory[K, E, X, P, T]
	                                        (entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P],
	                                         implicit override val composition :T ComposableFrom E)
	                                        (implicit propertyComposer :T ComposableFrom P, tag :TypeTag[E])
		extends KinFactory[K, E, T]
	{ factory =>
		def this(entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P])
		        (implicit propertyComposer :T ComposableFrom P, tag :TypeTag[E]) =
			this(entities, property, ComposableFrom.Properties(property))

		private final val argType = typeOf[E]

		override def delay(key :K, value: => Option[T]) :Derived[E, T] =
			EntityPropertyKin.delay(entities.absent(key), property, value)

		//we use delay because toOption is a lazy val anyway
		override def apply(key :K, value: Option[T]) :Derived[E, T] = delay(key, value)

		override def present(value :T) :Derived[E, T] =
			new Present[T] with Derived[E, T] {//fixme: many places assume isPresent -> items.isDefined
				implicit override val composition = factory.composition
				override val get = value
				override def items = Some(Iterable.empty)
//				override def property[Y, U >: T](property :PropertyPath[U, Y]) :One[Y] =
//					EntityPropertyKin.one(this, property)
			}

		override def missing(key :K) :Derived[E, T] = EntityPropertyKin(entities.missing(key), property)
		override def absent(key :K) :Derived[E, T] = EntityPropertyKin(entities.missing(key), property)
		override def nonexistent = Nonexistent()

		override def decompose(value :T) = Lack
		override def itemsOf(kin :Kin[T]) :Opt[Iterable[E]] = Lack

		override def keyFor(value :T) :Opt[K] = Lack
		override def keyFrom(item :E) :Opt[K] = entities.keyFrom(item)
		override def keyFrom(items :Iterable[E]) :Opt[K] = entities.keyFrom(items)

		override def keyOf(kin :Kin[T]) :Opt[K] = kin match {
			//owner.Element <:< prop.definedFor, so elements are safe to pass of as E even if owner is not (invariance)
			case EntityPropertyKin(owner :Derived[E @unchecked, _], prop, _) //unchecked type not necessarily true, but^
				if prop.definedFor <:< argType && property == prop => owner.items match {
				case Some(items) => keyFrom(items)
				case _ => Lack
			}
			case _ => Lack
		}

		override def isRequired = false

		override def required :KinFactory[K, E, T] =
			if (isRequired) this else new EntityPropertiesDerivedFactory(entities, property, composition)

		override def notRequired :KinFactory[K, E, T] =
			if (isRequired) this else new EntityPropertiesKinFactory(entities, property, composition)

		override def as[Y](implicit composition :Y ComposedOf E) :DerivedKinFactory[K, E, Y] = entities.as[Y]

		override def equivalencyToken :Any = (entities, property, composition)

		final private def friendEntities = entities
		final private def friendProperty = property
		final private def friendPropertyComposer = propertyComposer

		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[EntityPropertiesKinFactory[_, _, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :EntityPropertiesKinFactory[_, _, _, _, _] if canEqual(other) && (other canEqual this) =>
				argType =:= other.argType && property == other.friendProperty &&
					propertyComposer == other.friendPropertyComposer && entities == other.friendEntities
			case _ => false
		}

		override def hashCode :Int = (property.hashCode * 31 + propertyComposer.hashCode) * 31 + entities.hashCode

		override def toString :String =
			propertyComposer.toString + (if (isRequired) "(" else "?(") + entities + "." + property + ")"
	}



	private class EntityPropertiesDerivedFactory[K, E, X, P, T]
	                                            (entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P],
	                                             implicit override val composition :T ComposableFrom E)
	                                            (implicit propertyComposer :T ComposableFrom P, tag :TypeTag[E])
		extends EntityPropertiesKinFactory[K, E, X, P, T](entities, property, composition)
			with DerivedKinFactory[K, E, T] with RequiredKinFactory[K, E, T, Derived[E, T]]
	{
		def this(entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P])
		        (implicit propertyComposer :T ComposableFrom P, tag :TypeTag[E]) =
			this(entities, property, ComposableFrom.Properties(property))

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[EntityPropertiesDerivedFactory[_, _, _, _, _]]
	}

}






/** A kin for a property `T` (possibly a chained one) of an entity `E` referenced by another kin. */
private class EntityPropertyKin[E, X, P, +T](val owner :Derived[E, X], val property :PropertyPath[E, P])
                                            (implicit val propertyComposition :T ComposableFrom P)
	extends Derived[E, T]
{ //consider: having a TypeTag[E]
	override def isPresent :Boolean = owner.isPresent
	override def isMissing :Boolean = owner.isMissing
	override def isNonexistent :Boolean = owner.isNonexistent
	override val composition :T ComposableFrom E = ComposableFrom.Properties(property)
	override def parts :Iterable[Derived[E, _]] = owner.parts
	override def items :Option[Iterable[E]] = owner.items

	override lazy val toOption :Option[T] = owner.items.map {
		items => propertyComposition(items.view.map(property.fun))
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[EntityPropertyKin[_, _, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case prop :EntityPropertyKin[_, _, _, _] if prop canEqual this =>
			prop.propertyComposition == prop.propertyComposition && prop.property == property && prop.owner == owner
		case _ => false
	}

	override lazy val hashCode :Int = (owner.hashCode * 31 + property.hashCode) * 31 + propertyComposition.hashCode

	override def toString :String = owner.toString + ".properties[" + propertyComposition + "](_." + property + ")"
}
