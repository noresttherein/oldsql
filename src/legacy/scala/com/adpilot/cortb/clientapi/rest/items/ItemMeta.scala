package com.adpilot.cortb.clientapi.rest.items

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id
import com.adpilot.cortb.clientapi.rest.items.ItemMeta.PropertyType.TransportValue
import com.adpilot.cortb.clientapi.util.{OptionOps, ObjectProperty}

import scala.reflect.runtime.universe.TypeTag
import scala.util.Try

/** Meta information about an 'Item' class - a class recognized by the API as a separate entity.
  * Being a separate entity means that it can occur as type parameter of Item[_] and Items[_] - reference classes which can be represented in json by either actual objects, 
  * or filters identifying real objects stored in the database. Being an 'Item' will generally (but not necessarily) mean that they can be directly accessed by a separate url path 
  * and represented by a separate last in the database.
  * 
  * @tparam E an 'Item' class representing a separate entity 
  */
trait ItemMeta[E] {
	import ItemMeta._

	/** Properties of item E which are unique for each instance and can be used to create a unique reference - Item[E] */
	def unique :Seq[Property[E, _]]
	/** Properties of item E which can be filtered by - Items[E] can be created to refer to all instances with a given value of the property */
	def referable :Seq[Property[E, _]]
	/** All properties of item E which are items themselves, i.e. are of type Item[_] or Items[_]. They have their own ItemMeta and can be represented in json as either actual objects or references/filters to items of a given type. */
	def references :Seq[Property[E, _]]
	/** All properties of item E which are recognized by the API (can be part of some url parameters or refered to by Item[_]/Items[_]). This includes all properties on other properties lists of this item, but not necessarily or properties of class E*/
	def properties :Seq[Property[E, _]]
	

//	def idProperty :Property[E, Option[Id]]
	/** Find a property with a given name. The name may denote a transitive property, where actual properties are separated by '.'. 
	  * It checks the properties list for a property which name is either equal to a prefix of the passed string, and if it represents another Item, 
	  * it uses appropriate ItemMeta to resolve any remaining sufix.
	  */
	def property(name :String) :Option[Property[E, _]] =
		properties.flatMap(_.property(name)).headOption

	/** Find a property with a given name. The name may denote a transitive property, where actual properties are separated by '.'.
	  * It checks the properties list for a property which name is either equal to a prefix of the passed string, and if it represents another Item,
	  * it uses appropriate ItemMeta to resolve any remaining sufix.
	  * @throws IllegalArgumentException if there is no registered property of the given name
	  */
	def apply(name :String) :Property[E, _] = property(name) getOrElse {
		throw new IllegalArgumentException(s"property $name is not a part of $this")
	}

	/** An empty reference, representing a value of type E which is 'unknown/not changed/not relevant */
	def item() :Item[E] = Item()
	/** A reference pointing to an actual instance */
	def item(value :E) :Item[E] = Item(value)
	/** Areference identifying an instance of E by its (or its owner's, if it is not a separate database entity) id */
	def item(id :Id) :Item[E] = Item[E](id)

	/** A reference identifying an instance of E by a value of its unique property
	  * @param property name of a direct property of this instance, present on the unique properties list 
	  * @param value value of the property of the referenced instance of E, of type understandable by json 
	  * @return a reference representing an instance of E by an appropriate key-value pair
	  */
	def item(property :String, value :TransportValue) :Item[E] =
		unique.find(_.name==property).flatMap(_.item(value)) getOrElse (
			throw new IllegalArgumentException(s"Unknown property $property=$value of item $this")
		)

	
	/** An empty reference, representing a collection of instances of E which is unknown/not changed/not relevant */
	def items() :Items[E] = Items()

	/**  A reference to an actual collection of instances of E */ 
	def items(values :Iterable[E]) :Items[E] = Items(values.toSeq)

	/**A reference identifying all instances of E with a given value for the given property
	  * @param property name of a direct property of this instance, present on the referable properties list 
	  * @param value value of the property of the referenced instances of E, of a type understandable by json 
	  * @return a reference representing instances of E by an appropriate key-value pair
	  */
	def items(property :String, value :TransportValue) :Items[E] =
		referable.find(_.name==property).flatMap(_.items(value)) getOrElse (
			throw new IllegalArgumentException(s"Wrong property $property=$value of item $this")
		)

	/** A reference identifying all instances of E whose value of the given property falls into the given set 
	  * @param property name of a direct property of this instance, present on the referable properties list 
	  * @param values allowed values of the property of referenced instances of E, of a (same) type understandable by json 
	  * @return a reference representing instances of E by an appropriate key-value pair
	  */
	def items(property :String, values :Seq[TransportValue]) :Items[E] =
		referable.find(_.name==property).flatMap(_.items(values)) getOrElse (
			throw new IllegalArgumentException(s"Wrong property $property in $values of item $this")
		)


//	val Referable = Unapply[String, Property[E, _]](property => referable.find(_.name==property))
//	val Unique = Unapply[String, Property[E, _]](property => unique.find(_.name==property))

}




object ItemMeta {
	import OptionOps._

	def apply[X :ItemMeta] = implicitly[ItemMeta[X]]


	/** Represents a property with value type T of Item E that API is aware of. This includes all properties which can be used
	  * to refer to instances, can be searched by, or refer to other items - separate API entities. It doesn't necessarily include
	  * all properties present in an instance of E and its json representation.
	  * 
	  * @tparam E owner type of this property
	  * @tparam T value type of this property
	  */
	trait Property[E, T] {
		/** Return the value of this property for the given item instance*/ 
		def apply(obj :E) :T
		
		/** Name of this property, as exported by the API in url parameters */
		def name :String
		
		/** Type descriptor for this property used to marshal and unmarshal its values*/
		def tpe :PropertyType[T]

		/** A reference to an instance of E with a given value for this property 
 		  * @param value expected unique value for this property of a type understandable by json
		  */
		final def item(value :TransportValue) :Option[Item[E]] = tpe.read(value).map(Item(this, _))
		
		/** A reference to an instance of E with a given value for this property 
		  * @param value expected unique value for this property as present in a url parameter
		  */
		final def item(value :String) :Option[Item[E]] = tpe.parse(value).map(Item(this, _))

		/** A reference to all instances of E with a given value for this property
		  * @param value expected unique value for this property of a type understandable by json
		  */
		final def items(value :TransportValue) :Option[Items[E]] = tpe.read(value).map(Items(this, _))

		/** A reference to all instances of E with a given value for this property
		  * @param value a value for this property as present in a url parameter
		  */
		final def items(value :String) :Option[Items[E]] = tpe.parse(value).map(Items(this, _))

		/** A reference to all instances of E for which the value of this property falls in the given set.
		  * @param values values for this property as present in url parameters
		  */
		final def items(values :Seq[String]) :Option[Items[E]] = values.flatMap(tpe.parse) match {
			case parsed if parsed.size==values.size => Some(Items(this, parsed))
			case _ => None
		}

		/** A reference to all instances of E for which the value of this property falls in the given set.
		  * @param values values for this property as a type understandable by json
		  */
		final def items(values :Iterable[TransportValue]) :Option[Items[E]] = values.flatMap(tpe.read).toSeq match {
			case read if read.size==values.size => Some(Items(this, read))
			case _ => None
		}


		
		
		private[ItemMeta] def nest[X](prop :ItemProperty[X, E]) :Property[X, T] = prop.andThen(this)
		
		/** Resolve the name as a possibly transitive property of the owner of this property, starting with this property */
		def property(name :String) :Option[Property[E, _]] =
			if (name==this.name) Some(this)
			else None


		override def toString = name

	}
	

	object Property {
		/** Create a property of E with value type T of the given name and represented by the passed function.
		  * @param property function returning the property of its argument which the created instance should refer to
		  * @param name a name of this property
		  * @tparam E owner type of the created property
		  * @tparam T value type of the created property
		  */
		protected[ItemMeta] def apply[E, T :PropertyType](property :E=>T, name :String) :Property[E, T] =
			new BasicProperty(property, name)//, propertyType[T], ItemMeta[E])

		/** Create a property of E with value type T represented by the passed function.
		  * The name of this property will be discovered by reflection and mocking from the methods called by the passed function
		  * @param property function returning the property of its argument which the created instance should refer to
		  * @tparam E owner type of the created property
		  * @tparam T value type of the created property
		  */
		protected[ItemMeta] def apply[E :TypeTag, T :PropertyType](property :E=>T) :Property[E, T] =
			new BasicProperty(property, ObjectProperty(property).name)//, propertyType[T], ItemMeta[E])

		class BasicProperty[E, T](property :E=>T, val name :String)(implicit val tpe :PropertyType[T]) extends Property[E, T] {
			override def apply(obj: E): T = property(obj)
		}

	}

	/** A property with a value type of Item[T], where T is a separate item with an associated ItemMeta */
	class ItemProperty[E, T :ItemMeta](prop :E=>Item[T], val name :String) extends Property[E, Item[T]] {
		implicit val target = ItemMeta[T]

		if (target==null) //initialization order problem most probably
			throw new IllegalArgumentException(s"target is null for $name!")

		val tpe = PropertyType.FKItemProperty[T]

		override def apply(obj: E): Item[T] = prop(obj)

		override def property(name :String) :Option[Property[E, _]] =
			if (name==this.name) 
				Some(this)
			else if (name.startsWith(this.name) && name(this.name.length)=='.') {
				andThen(name.substring(this.name.length+1))
			}else None

		override private[ItemMeta] def nest[X](prop: ItemProperty[X, E]): Property[X, Item[T]] = prop.andThen(this)
		
		/** Create a transitive property being a concatanation of this property and a property of its value type */
		def andThen(name :String) :Option[Property[E, _]] = target.property(name).map(_.nest(this))
		
		/** Concatante this property and a property of referenced type into a transitive property (double dispatch alternative method)*/
		def andThen[X](prop :Property[T, X]) :Property[E, X] = 
			Property(compose(prop.apply), append(prop.name))(prop.tpe)

		/** Concatante this property and a property of referenced type into a transitive ItemProperty (double dispatch alternative method)*/
		def andThen[X](prop :ItemProperty[T, X]) :ItemProperty[E, X] =
			ItemProperty(compose(prop.apply), append(prop.name))(prop.target)
		
//		def andThen[X](prop :ItemsProperty[T, X]) :ItemsProperty[E, X] =
//			ItemsProperty(compose(prop.apply), append(prop.name))
		
		/** (This property, as a function E=>Item[T]) andThen fun. Calls Item.get - assumes that the value of this property for the passed argument refers to an actual instance of T (rather than a reference)*/
		protected def compose[X](fun :T=>X) = prop andThen (_.get) andThen fun
		
		/** Name of a transitive property starting with this property followed by a property with the specified name */
		protected def append(name :String) :String = this.name +"."+name
		
		override def toString = s"[$name -> $target]"
	}
	
	
	object ItemProperty {
		/** Create a property of E with value type of Item[T] with the given name and represented by the passed function.
		  * @param property function returning the property of its argument which the created instance should refer to
		  * @param name a name of this property
		  * @tparam E owner type of the created property
		  * @tparam T item type referenced by this property
		  */
		protected[ItemMeta] def apply[E, T :ItemMeta](property :E=>Item[T], name :String) :ItemProperty[E, T] =
			new ItemProperty(property, name)

		/** Create a property of E with value type of Item[T] represented by the passed function. The name of the property
		  * will be the concatenated names of properties in the chained call of the passed function, as discovered via reflection and mocking
		  * @param property function returning the property of its argument which the created instance should refer to
		  * @tparam E owner type of the created property
		  * @tparam T item type referenced by this property
		  */
		protected[ItemMeta] def apply[E :TypeTag, T :ItemMeta](property :E=>Item[T]) :ItemProperty[E, T] =
			new ItemProperty(property, ObjectProperty(property).name)

//		def unapply[E, T](property: Property[E, T]) = property match {
//			case p :ItemProperty[E, T] => Some(p)
//			case _ => None
//		}
	}

	/** Type descriptor for property values, knowing how to parse a value from a url parameter or a json-supported type. This class doesn't depend on any Json model library */
	trait PropertyType[T] { base =>
		/** Obtain a value from an url parameter*/
		def parse(value :String) :Option[T]

		/** format the value to a string which can be used as a url parameter */
		def format(value :T) :String

		/** Create a value from a json supported type */
		def read(value :TransportValue) :Option[T]

		/** Marshall the value to a json supported type */
		def write(value :T) :TransportValue

		/** Create a new property type descriptor for a type which can be bidirectionally mapped to this type. 
		  * 
		  * @param there Create a value of requested type out of an instance of this property type
		  * @param back Create a value of this property type out of an instance of requested type
		  * @tparam X type of the requested property type descriptor
		  * @return a property type descriptor delegating all calls to this instance 
		  */
		def map[X](there :T=>X)(back :X=>T) :PropertyType[X] = new PropertyType[X] {
			def parse(value: String): Option[X] = base.parse(value).map(there)
			def read(value: TransportValue): Option[X] = base.read(value).map(there)

			def write(value: X): TransportValue = base.write(back(value))
			def format(value: X): String = base.format(back(value))
		}
	}


	object PropertyType {

		/** Special property value type which are understood by json directly, without any marshalling or unmarshalling.
		  * Instances exist for all types which can be held by TransportType boxes and none other. 
		  * Can be used to create new property types via 'map' method.
		  * 
		  * @param test identity function T=>T lifted to a partial function (for example, { case x:BigDecimal => x }
		  * @param fromString parse the value from a url parameter
		  * @param toString format the value to a url parameter 
		  * @tparam T
		  */
		sealed class TransportType[T](test :PartialFunction[Any, T])(val fromString :String=>T, toString :T=>String = (_:T).toString) extends PropertyType[T] {
//			private val cast = as[T](identity)

			private[PropertyType] def is(value :Any) = test.isDefinedAt(value)

			private[PropertyType] def cast(value :Any) = test.lift(value)

			override def write(value: T): TransportValue = TransportValue(value)(this)

			override def read(value: TransportValue): Option[T] = test(value.value).providing(test.isDefinedAt(value.value))


			override def format(value: T): String =
				if (value==null) null
				else toString(value)

			override def parse(value: String): Option[T] = Try(fromString(value)).toOption

		}
		
		object TransportType {
			def apply[X :TransportType] = implicitly[TransportType[X]]
			
			implicit val BigDecimalType = new TransportType({ case x :BigDecimal => x })(BigDecimal(_))
			
			implicit val StringType = new TransportType({ case x :String => x })(identity)
			
			implicit val BooleanType = new TransportType({ case x :Boolean => x })(_.toBoolean)

		}
		/** A value which can be handled by json without marshalling or unmarshalling */
		class TransportValue private (val value :Any) extends AnyVal


		object TransportValue {
			/** Represents a null/unidentified value type in json */
			val Null = new TransportValue(null)

			/** Creates a holder for the given value, requiring evidence proving that it can be understood by json */
			def apply[T :TransportType](value :T) :TransportValue = new TransportValue(value)
		}

		def apply[T :PropertyType] :PropertyType[T] = implicitly[PropertyType[T]]

		import TransportType._
		implicit def TransportProperty[T :TransportType] :PropertyType[T] = implicitly[TransportType[T]]

		implicit val IntProperty = BigDecimalType.map(_.toInt)(BigDecimal(_))
		implicit val LongProperty = BigDecimalType.map(_.toLong)(BigDecimal(_))
		implicit val ShortProperty = BigDecimalType.map(_.toShort)(BigDecimal(_))
		implicit val DoubleProperty = BigDecimalType.map(_.toDouble)(BigDecimal(_))
		implicit val FloatProperty = BigDecimalType.map(_.toFloat)(BigDecimal(_))
		implicit val ByteProperty = BigDecimalType.map(_.toByte)(BigDecimal(_))

		implicit val IdProperty = LongProperty.map(Id(_))(_.value)
		implicit def IdItemProperty[T] = IdProperty.map(IdItem[T](_))(_.id)
		implicit def FKItemProperty[T] :PropertyType[Item[T]] = IdProperty.map(Item[T](_)){ case IdItem(id) => id }


		implicit def OptionProperty[T :PropertyType] :PropertyType[Option[T]] = new PropertyType[Option[T]] {
			val base = PropertyType[T]

			def parse(value: String): Option[Option[T]] =
				if (value==null) Some(None)
				else base.parse(value).map(Some(_))

			def format(value: Option[T]): String = value.map(base.format).orNull


			def read(value: TransportValue): Option[Option[T]] =
				if (value.value==null) Some(None)
				else base.read(value).map(Some(_))

			def write(value: Option[T]): TransportValue = value match {
				case None => TransportValue.Null
				case Some(x) => base.write(x)
			}

		}

		/** A dummy instance allowing creating of properties with values of type Items[T].
		  * This instance doesn't support any methods declared by PropertyType!
		  * Marshalling and unmarshalling to/from json and url parameters must be handled by any code using the Property instances
		  * @tparam T target item type
		  * @return an instance throwing UnsupportedOperationException if any of its methods is called
		  */
		implicit def ItemsProperty[T] :PropertyType[Items[T]] = new PropertyType[Items[T]] {
			override def parse(value: String): Option[Items[T]] = 
				throw new UnsupportedOperationException("PropertyType[Items[T]].parse")

			override def format(value: Items[T]): String =
				throw new UnsupportedOperationException("PropertyType[Items[T]].format")


			override def read(value: TransportValue): Option[Items[T]] =
				throw new UnsupportedOperationException("PropertyType[Items[T]].read")

			override def write(value: Items[T]): TransportValue =
				throw new UnsupportedOperationException("PropertyType[Items[T]].write")

		}
	}



}