package com.hcore.clientapi.rest.mappings


import com.hcore.clientapi.rest.items.Item._
import com.hcore.clientapi.rest.items.ItemMeta.Property.BasicProperty
import com.hcore.clientapi.rest.items.ItemMeta.{ItemProperty, PropertyType}
import com.hcore.clientapi.rest.items._
import com.hcore.ogre.model.Restriction.{PropertyEquals, PropertyIn}
import com.hcore.ogre.model.Reference
import com.hcore.ogre.model.Reference._
import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.collection.breakOut
import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.util.Try


abstract class ItemRepresentationSupport[I, E :TypeTag] extends ItemRepresentation[I, E] {

	private def entityName = {
		val typeName = typeTag[E].tpe.toString
		typeName.substring(typeName.lastIndexOf('.')+1)
	}
	override def toString = s"~${entityName}"

	protected implicit def self: ItemRepresentationSupport[I, E] = this


	protected implicit def itemAsOne[I2, E2](item :Item[I2])(implicit repr :ItemRepresentation[I2, E2]) :Reference[E2] =
		repr.one(item)

	protected implicit def itemAsMaybe[I2, E2](item :Item[I2])(implicit repr :ItemRepresentation[I2, E2]) :Reference[Option[E2]] =
		repr.maybe(item)

	protected def itemsAsReference[I2, E2, C<:Iterable[E2]](items :Items[I2])(implicit repr :ItemRepresentation[I2, E2], cbf :CanBuildFrom[_, E2, C]) :Reference[C] =
		repr.many(items)

	protected implicit def itemsAsSet[I2, E2](items :Items[I2])(implicit repr :ItemRepresentation[I2, E2]) :Reference[Set[E2]] =
		repr.many(items)

	protected implicit def itemsAsSeq[I2, E2](items :Items[I2])(implicit repr :ItemRepresentation[I2, E2]) :Reference[Seq[E2]] =
		repr.many(items)


	protected implicit def oneAsItem[I2, E2](one :Reference[E2])(implicit repr :ItemRepresentation[I2, E2]) :Item[I2] =
		repr.item(one)

	protected implicit def manyAsItems[I2, E2](ref :Reference[Iterable[E2]])(implicit repr :ItemRepresentation[I2, E2]) :Items[I2] =
		repr.items(ref)

//	protected implicit def seqAsItems[I2, E2](ref :Reference[Seq[E2]])(implicit repr :EntityRepresentation[I2, E2]) :Items[I2] =
//		repr.items(ref)
	protected implicit def itemOptionAsOneOption[I2, E2](opt :Option[Item[I2]])(implicit repr :ItemRepresentation[I2, E2]) :Option[Reference[E2]] =
		opt.map(itemAsOne(_))

	protected implicit def oneOptionAsItemOption[I2, E2](opt :Option[Reference[E2]])(implicit repr :ItemRepresentation[I2, E2]) :Option[Item[I2]] =
		opt.map(oneAsItem(_))



	override def one(ref: Item[I]): Reference[E] = ref match {
		case ValueItem(item) => Single(entity(item))
		case PropertyItem(PropertyMapping(prop), value) => prop.one(value)
		case AbsentItem => Unknown()
		case _ => throw new IllegalArgumentException(s"Unknown item reference type :$ref. This is a bug. Known properties are $properties")
	}

	override def maybe(ref: Item[I]): Reference[Option[E]] = ref match { //todo: AbsentItem - NothingReference?
//		case ValueItem(item) => IdRef.maybe[E](entity(item))
		case PropertyItem(PropertyMapping(prop), value) => prop.maybe(value)
		case _ => throw new IllegalArgumentException(s"Unknown item reference type :$ref. This is a bug. Known properties are $properties")
	}

	override def many[T<:Iterable[E]](ref: Items[I])(implicit cbf :CanBuildFrom[_, E, T]): Reference[T] = ref match {
		case ItemsByProperty(PropertyMapping(prop), value) =>
			prop.many[T](value)
		case ItemsByPropertyValues(PropertyMapping(prop), values) =>
			prop.manyMulti[T](values)
		case ItemsValues(items) =>
			val entities = items.map(entity)
			Reference((cbf() ++= entities).result())
		case ItemsAll => All.as[T]
		case ItemsAbsent => Unknown[T]()
		case _ => throw new IllegalArgumentException(s"Unknown items reference type :$ref. This is a bug. Known properties are $properties")
	}

	override def seq(ref: Items[I]): Reference[Seq[E]] = many[Seq[E]](ref)




	override def item(ref: Reference[E]): Item[I] = ref match {
		case Full(value) => Item(mapEntity(value))
		case Unknown() => Item()
		case Single(PropertyEquals(property, value)) if property.isApplicableTo[E] =>
			mapping(property.asInstanceOf[PropertyChain[E, Any]]).item(value)
//		case UniquePropertyReference(property, value) => mapping(property).item(value)
		case _ => throw new IllegalArgumentException(s"Can't create item out of unknown reference $ref. This is a bug.")
	}

	override def items[T <: Iterable[E]](ref: Reference[T]): Items[I] = ref match {
		case Full(values) => Items(values.map(mapEntity)(breakOut))
		case Unknown() => Items()
		case Satisfying(PropertyEquals(property, value)) =>
			mapping(property.asInstanceOf[PropertyChain[E, _]]).items(value)
		case Satisfying(PropertyIn(property, values)) =>
			mapping(property.asInstanceOf[PropertyChain[E, _]]).itemsMulti(values.toSeq)
		case _ => throw new IllegalArgumentException(s"Can't create items out of unknown reference $ref. This is a bug.")
	}




	override def property(name :String) :Option[PropertyMapping[_, _]] = super.property(name).map(_.asInstanceOf[PropertyMapping[_, _]])

	override def property(prop: Property[I, _]): PropertyChain[E, _] = prop match {
		case PropertyMapping(p) => p.property
		case _ => throw new IllegalArgumentException(s"passed property $prop was not created by this instance ($self)")
//		case _ => property(prop.name).map(_.onEntity) getOrElse {
//			throw new IllegalArgumentException(s"Couldn't map property $prop to an entity property")
//		}
	}


	override def foreignProperty(prop: ItemProperty[I, _]): (E) => Reference[Any] = prop match {
		case SingleRefMapping(p) => p.onEntity
		case _ => throw new IllegalArgumentException(s"passed property $prop was not created by this instance ($self)")
//		case _ => property(prop.name).map(_.onEntity.asInstanceOf[E=>Reference[_]]) getOrElse {
//			throw new IllegalArgumentException(s"Couldn't map property $prop to an entity property")
//		}
	}

	override def referenceProperty(prop: Property[I, _]): Option[(E) => Reference[Any]] = prop match {
		case PropertyMapping(p) => p.asReference
		case _ => None
	}


	override def mapProperty(property: PropertyChain[E, _]): Property[I, _] = mapping(property)

	private def mapping(prop: PropertyChain[E, _]): PropertyMapping[_, _] =
		propertyMappings.getOrElse(prop, {
			throw new IllegalArgumentException(s"property $prop is not a part of api mapping $self. Known properties are $properties")
		})


	override def target[T](property: ItemProperty[I, T]): ItemRepresentation[T, _] = property match {
		case SingleRefMapping(p) => p.target
		case _ => throw new IllegalArgumentException(s"passed property $property was not created by this instance ($self). Known properties are $properties")
	}





	trait PropertyMapping[+X, Y] extends ItemMeta.Property[I, Y] {

		protected[ItemRepresentationSupport] val onEntity :E => X
		protected[ItemRepresentationSupport] lazy val property = PropertyChain(onEntity)

		protected[this] val itemValue :X=>Y
		protected[this] val entityValue :Y=>X

		protected[ItemRepresentationSupport] def item(value :Any) :Item[I] =
			Item(this, itemValue(value.asInstanceOf[X]))

		protected[ItemRepresentationSupport] def items(value :Any) :Items[I] =
			Items(this, itemValue(value.asInstanceOf[X]))

		protected[ItemRepresentationSupport] def itemsMulti(values :Seq[Any]) :Items[I] =
			Items(this, values.map(v=> itemValue(v.asInstanceOf[X])))

		protected[ItemRepresentationSupport] def one(value :Any) :Reference[E] =
			Single(PropertyEquals(property)).empty(entityValue(value.asInstanceOf[Y]))

		protected[ItemRepresentationSupport] def maybe(value :Any) :Reference[Option[E]] =
			Single(PropertyEquals(property)).in[Option].empty(entityValue(value.asInstanceOf[Y]))

		protected[ItemRepresentationSupport] def many[T<:Iterable[E]](value :Any)(implicit cbf :CanBuildFrom[_, E, T]) :Reference[T] =
			Satisfying(PropertyEquals(property)).as[T].empty(entityValue(value.asInstanceOf[Y]))

		protected[ItemRepresentationSupport] def manyMulti[T<:Iterable[E]](values :Seq[Any])(implicit cbf :CanBuildFrom[_, E, T]) :Reference[T] =
			Satisfying(PropertyEquals(property).in).as[T].empty(values.map(v => entityValue(v.asInstanceOf[Y])).toSet)



		def asReference :Option[E=>Reference[_]] = None

		private[ItemRepresentationSupport] def meta = self

		override def toString = Try({s"[$self.$name<~>${property.name} :$resultTypeName]"}).toOption.getOrElse(s"[$self.$name :$resultTypeName]")
	}


	class SingleRefMapping[X, Y](val onEntity :E=>Reference[X], onItem :I=>Item[Y], name :String)(
			implicit override val target :ItemRepresentation[Y, X]
		) extends ItemProperty(onItem, name)(target) with PropertyMapping[Reference[X], Item[Y]]
	{
		protected[this] val itemValue = target.item(_:Reference[X])
		protected[this] val entityValue = target.one(_:Item[Y])

		private def join[Z](fun :X=>Z) = onEntity andThen (_.join) andThen fun

		override def andThen[Z](next: Property[Y, Z]): Property[I, Z] = {
			val prop = target.property(next)

			def toEntity(key :Z) = target.one(Item(next, key)) match {
				case Single(PropertyEquals(_, res)) => res
				case ref => throw new RuntimeException(s"Unable to map key $key of property $next to entity key - mapped reference is not a property reference: $ref")
			}

			def toItem(key :Any) = target.item(Satisfying(PropertyEquals(prop)).empty(key)) match {
				case PropertyItem(_, res) => res.asInstanceOf[Z]
				case item => throw new RuntimeException(s"Unable to map entity key $key of property $next to item key - mapped item is: $item")
			}
			self.map(join(prop.apply), compose(next.apply), append(next.name))(next.tpe, toEntity _, toItem _)
		}


		override def andThen[Z](next: ItemProperty[Y, Z]): ItemProperty[I, Z] =
			self.one(join(target.foreignProperty(next)), compose(next.apply), append(next.name))(
				target.target(next).asInstanceOf[ItemRepresentation[Z, Any]]
			)

		override def asReference: Option[(E) => Reference[_]] = Some(onEntity)

		override def toString = Try({s"[$self.$name<~>${property.name} ->$target]"}).toOption.getOrElse(s"[$self.$name->$target]")
	}


	class MultiRefMapping[X, Y, T<:Iterable[X]](val onEntity :E=>Reference[T], onItem :I=>Items[Y], name :String)(
			implicit target :ItemRepresentation[Y, X], cbf :CanBuildFrom[_, X, T]
		) extends BasicProperty(onItem, name) with PropertyMapping[Reference[T], Items[Y]]
	{
		def this(onEntity :E=>Reference[T], onItem :I=>Items[Y])(
				implicit target :ItemRepresentation[Y, X], cbf :CanBuildFrom[_, X, T], tag :TypeTag[I]) =
			this(onEntity, onItem, PropertyChain(onItem).name)

		override protected[this] val itemValue: (Reference[T]) => Items[Y] = target.items(_)
		override protected[this] val entityValue: (Items[Y]) => Reference[T] = target.many[T](_)

		override def asReference: Option[(E) => Reference[_]] = Some(onEntity)

		override def toString = Try({s"[$self.$name<~>${property.name}->$target]"}).toOption.getOrElse(s"[$self.$name->$target]")
	}

	object PropertyMapping {
		def unapply[T](property :Property[I, T]) = property match {
			case p :ItemRepresentationSupport[_, _]#PropertyMapping[_, _] if p.meta==self =>
				Some(p.asInstanceOf[PropertyMapping[_, T]])
			case _ => None
		}

	}

	object SingleRefMapping {
		def unapply[T](property :ItemProperty[I, T]) = property match {
			case p :ItemRepresentationSupport[_, _]#SingleRefMapping[_, _] if p.meta==self =>
				Some(p.asInstanceOf[SingleRefMapping[_, T]])
			case _ => None
		}

	}





	protected def prop[X :PropertyType](entityProperty :E=>X, itemProperty :I=>X, name :String) :PropertyMapping[X, X] =
		new BasicProperty(itemProperty, name) with PropertyMapping[X, X]
		{
			val onEntity = entityProperty
			protected[this] val itemValue = identity[X] _
			protected[this] val entityValue = identity[X] _
		}

	protected def prop[X](entityProperty :E=>X, itemProperty :I=>X)(implicit tpe :PropertyType[X], tag :TypeTag[I]) :PropertyMapping[X, X] =
		prop(entityProperty, itemProperty, PropertyChain(itemProperty).name)




	protected def map[X, Y :PropertyType](entityProperty :E=>X, itemProperty :I=>Y, name :String)(implicit toEntity :Y=>X, toItem :X=>Y) :PropertyMapping[X, Y] =
		new BasicProperty(itemProperty, name) with PropertyMapping[X, Y] {
			val onEntity = entityProperty
			protected[this] val itemValue = toItem
			protected[this] val entityValue = toEntity
		}

	protected def map[X, Y:PropertyType](entityProperty :E=>X, itemProperty :I=>Y)(implicit toEntity :Y=>X, toItem :X=>Y, tag :TypeTag[I]) :PropertyMapping[X, Y] =
		map(entityProperty, itemProperty, PropertyChain(itemProperty).name)




	protected def one[X, Y](entityProperty :E=>Reference[X], itemProperty :I=>Item[Y], name :String)(implicit repr :ItemRepresentation[Y, X]) :SingleRefMapping[X, Y] =
		new SingleRefMapping(entityProperty, itemProperty, name)

	protected def one[X, Y](entityProperty :E=>Reference[X], itemProperty :I=>Item[Y])(implicit repr :ItemRepresentation[Y, X], tag :TypeTag[I]) :SingleRefMapping[X, Y] =
		one[X, Y](entityProperty, itemProperty, PropertyChain(itemProperty).name)




	protected def many[X, Y, T<:Iterable[X]](entityProperty  :E=>Reference[T], itemProperty :I=>Items[Y], name :String)(
			implicit target :ItemRepresentation[Y, X], cbf :CanBuildFrom[_, X, T]) :PropertyMapping[Reference[T], Items[Y]] =
//		map(entityProperty, itemProperty, name)(ItemMeta.propertyType[Items[Y]], target.Reference[T], target.items(_))
		new MultiRefMapping[X, Y, T](entityProperty, itemProperty, name)

	protected def many[X, Y, T<:Iterable[X]](entityProperty  :E=>Reference[T], itemProperty :I=>Items[Y])(
			implicit target :ItemRepresentation[Y, X], cbf :CanBuildFrom[_, X, T], tag :TypeTag[I]) :PropertyMapping[Reference[T], Items[Y]] =
//		map(entityProperty, itemProperty)(ItemMeta.propertyType[Items[Y]], target.Reference[T], target.items(_), tag)
		new MultiRefMapping[X, Y, T](entityProperty, itemProperty)



	protected def seq[X, Y](entityProperty  :E=>Reference[Seq[X]], itemProperty :I=>Items[Y], name :String)(
			implicit target :ItemRepresentation[Y, X]) :PropertyMapping[Reference[Seq[X]], Items[Y]] =
		many[X, Y, Seq[X]](entityProperty, itemProperty, name)

	protected def seq[X, Y](entityProperty  :E=>Reference[Seq[X]], itemProperty :I=>Items[Y])(
			implicit target :ItemRepresentation[Y, X], tag :TypeTag[I]) :PropertyMapping[Reference[Seq[X]], Items[Y]] =
		many[X, Y, Seq[X]](entityProperty, itemProperty)

	protected def set[X, Y](entityProperty  :E=>Reference[Set[X]], itemProperty :I=>Items[Y], name :String)(
			implicit target :ItemRepresentation[Y, X]) :PropertyMapping[Reference[Set[X]], Items[Y]] =
		many[X, Y, Set[X]](entityProperty, itemProperty, name)

	protected def set[X, Y](entityProperty  :E=>Reference[Set[X]], itemProperty :I=>Items[Y])(
			implicit target :ItemRepresentation[Y, X], tag :TypeTag[I]) :PropertyMapping[Reference[Set[X]], Items[Y]] =
		many[X, Y, Set[X]](entityProperty, itemProperty)
	




	object Implicits {
		implicit def anyToOpt[X](value :X) :Option[X] = Option(value)
		implicit def optToNullable[X>:Null](value :Option[X]) :X = value.orNull
	}

	//todo: Sets? or at least faster lookup
	override def unique :Seq[PropertyMapping[_, _]]
	override def referable :Seq[PropertyMapping[_, _]]
	override def references :Seq[PropertyMapping[Reference[_], _]]
	override def properties :Seq[PropertyMapping[_, _]]

	protected lazy val propertyMappings = properties.groupBy(_.property).map {
			case (itemProp, Seq(entityProp)) => (itemProp, entityProp)
		}.toMap[PropertyChain[E, _], PropertyMapping[_, _]]

	protected def noDups(props :Seq[PropertyMapping[_, _]]) :Seq[PropertyMapping[_, _]] = props match {
		case Seq() => Seq()
		case Seq(first, rest @_*) => first +: noDups(rest.filterNot(first eq _))
	}

}
