package com.adpilot.cortb.clientapi.prototype.repository.entities.meta

import com.adpilot.cortb.clientapi.util.ObjectProperty.AnyProperty
import com.adpilot.cortb.clientapi.util.ObjectProperty

import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.TypeTag


trait Reference[+T] {
	def toOpt :Option[T]
	
	def canEqual(that :Any) = that.isInstanceOf[Reference[_]]

	override def equals(that :Any) = that match {
		case a:Reference[_] if a.canEqual(this) => true
		case _ => false
	} 
	
	override def hashCode = toOpt.hashCode
	
	override def toString = toOpt.map(o => s"@Full($o)") getOrElse "@Empty"


}



object Reference {

	import com.adpilot.cortb.clientapi.util.Matching.&&

	implicit def asValueOption[T](association :Reference[T]) :Option[T] = association.toOpt

	def apply[T](value :T) = Transient(value)


	object Full {
		def unapply[T](association :Reference[T]) = association.toOpt
	}



	object Empty {
		def unapply(association :Reference[_]) = association.toOpt match {
			case None => true
			case Some(_) => false
		}
	}



	object Transient {
		def apply[T](x :T) :Reference[T] = new Transient[T] { val value = x }

		def unapply[T](ass :Reference[T]) = ass match {
			case t:Transient[_] => Some(t.value.asInstanceOf[T])
			case _ => None
		}
	}



	object Unknown {
		def apply() : Reference[Nothing] = new Unknown[Nothing] {}

		def unapply(ref :Reference[_]) = ref match {
			case x:Unknown[_] => true
			case _ => false
		}

		def apply(ref :Reference[_]) :Boolean = ref.isInstanceOf[Unknown[_]]
	}




	private sealed trait Empty[+T] extends Reference[T] {
		def toOpt = None
	}


	private sealed trait Transient[+T] extends Reference[T] {
		def value :T
		def toOpt = Some(value)

		override def canEqual(that: Any): Boolean = that.isInstanceOf[Transient[_]]

		override def toString = s"@Transient($value)"
	}


	private sealed trait Unknown[+T] extends Reference[T] {
		def toOpt = None

		override def toString = "@Unknown"

		override def canEqual(that: Any): Boolean = that.isInstanceOf[Reference[_]]

		override def equals(that: Any): Boolean = that.isInstanceOf[Reference[_]]

		override def hashCode = 0
	}




	sealed trait CollectionOf[+T, E] {
		def apply(items :Iterable[E]) :T
	}

	object CollectionOf {
		implicit def ExactlyOne[E] :E CollectionOf E = new CollectionOf[E, E] {
			override def apply(items: Iterable[E]): E = items.size match {
				case 0 => throw new IllegalArgumentException("expected exactly one element, got zero")
				case 1 => items.head
				case n => throw new IllegalArgumentException(s"expected exactly one element, got $n: $items")
			}
		}

		implicit def ZeroOrOne[E] :Option[E] CollectionOf E = new CollectionOf[Option[E], E] {
			override def apply(items :Iterable[E]) =
				if (items.size>1)
					throw new IllegalArgumentException(s"expected zero or one elements, got $items")
				else items.headOption
		}

		implicit def CollectionOf[T, E](implicit cbf :CanBuildFrom[_, E, T]) :T CollectionOf E = new CollectionOf[T, E] {
			override def apply(items: Iterable[E]): T = (cbf() ++= items).result()
		}
	}




	trait One[+E] extends Reference[E] {
		def join :E = throw new UnsupportedOperationException("Reference.join can be called only by functions specifying fetch requirements")
	}

	trait PropertyReference[+T, E, +I] extends Reference[T] {
		def target :ObjectProperty[E, I]
		def key :I

		override def toString = toOpt.map(v => s"@[$target=$key: $v]") getOrElse s"@[$target=$key]"
	}


	type UniquePropertyReference[E, I] = PropertyReference[E, E, I] with One[E]




	object One {
		val Unknown :One[Nothing] = apply()

		def apply[E]() :One[Nothing] = new One[Nothing] with Unknown[Nothing]

		def apply[E](target :E) :One[E] = new One[E] with Transient[E] { val value = target }


		def apply[E, K](property :ObjectProperty[E, K]) :UniquePropertyReferenceFactory[E, K] =
			new UniquePropertyReferenceFactory(property)

		def apply[E :TypeTag, K](property :E=>K) :UniquePropertyReferenceFactory[E, K] =
			new UniquePropertyReferenceFactory(property)

		def apply[E :TypeTag, K](property :E=>K, key :K) :UniquePropertyReference[E, K] =
			UniquePropertyReference(property, key)
//			apply(property).Empty(key)
//			new EmptyKey[E, E, K](PropertyPath(property), key) with One[E]

		def apply[E :TypeTag, K](entity :E, property :E=>K) :UniquePropertyReference[E, K] =
			UniquePropertyReference(entity, property)
//			new FullKey[E, E, K](PropertyPath(property), property(entity), entity) with One[E]
//			apply(property).Full(entity)


		def unapply[E](ass :Reference[E]) = ass match {
			case _:One[_] => ass.toOpt
			case _ => None
		}
	}





	object PropertyReference {

		def apply[T <:Iterable[E], E :TypeTag, I](target :E=>I, key :I):PropertyReference[T, E, I] =
			new EmptyKey[T, E, I](ObjectProperty(target), key)

		def apply[T <:Iterable[E], E :TypeTag, I](value :T, target :E=>I, key :I) :PropertyReference[T, E, I] =
			new FullKey[T, E, I](ObjectProperty(target), key, value)

//		def apply[T <:Iterable[E], E, I](target :E=>I)(implicit ct :TypeTag[T], et :TypeTag[E]) :PropertyReferenceFactory[T, E, I] =
//			new PropertyReferenceFactory[T, E, I](target)
		
//		def apply[E :TypeTag, I](target :E=>I) :UniquePropertyReferenceFactory[E, I] = new UniquePropertyReferenceFactory(target)
		
		def apply[E :TypeTag, I](target :E=>I) :PropertyReferenceFactory[E, I] = new PropertyReferenceFactory(target)

		def apply[E, I](target :ObjectProperty[E, I]) :PropertyReferenceFactory[E, I] = new PropertyReferenceFactory(target)

//		def unapply[T](ref :One[T]) :Option[(AnyProperty[T], Any)] = ref match {
//			case o:PropertyReference[_, _, _] => Some(o.target.asInstanceOf[AnyProperty[T]], o.key)
//			case _ => None
//		}

//		def unapply[T<:Iterable[E], E](ref :ManyOf[T, E]) :Option[(AnyProperty[E], Any)] = ref match {
//			case o:PropertyReference[_, _, _] => Some(o.target.asInstanceOf[AnyProperty[E]], o.key)
//			case _ => None
//		}

		def unapply[T](ref :Reference[T]) :Option[(ObjectProperty[_, _], Any)] = ref match {
			case o:PropertyReference[_, _, _] => Some(o.target, o.key)
			case _ => None
		}
	}

	object PropertyMultiReference {
		def unapply[T](ref :Reference[T]) :Option[(ObjectProperty[_, _], Seq[Any])] = ref match {
			case o:PropertyReference[_, _, _] => Some(o.target, Seq(o.key))
			case o:PropertyMultiReference[_, _, _] => Some(o.target, o.keys)
			case _ => None
		}

	}



	object UniquePropertyReference {
		def apply[E :TypeTag, K](property :E=>K) :UniquePropertyReferenceFactory[E, K] =
			new UniquePropertyReferenceFactory(property)

		def apply[E :TypeTag, K](property :E=>K, key :K) :UniquePropertyReference[E, K] =
			apply(property).Empty(key)
//			new EmptyKey[E, E, K](PropertyPath(property), key) with One[E]

		def apply[E :TypeTag, K](entity :E, property :E=>K) :UniquePropertyReference[E, K] =
//			new FullKey[E, E, K](PropertyPath(property), property(entity), entity) with One[E]
			apply(property).Full(entity)


		def unapply[T](ref :Reference[T]) :Option[(AnyProperty[T], Any)] = ref match {
			case o :PropertyReference[_, _, _] with One[_] =>
				Some(o.target.asInstanceOf[AnyProperty[T]], o.key)
			case _ => None
		}

//		def check[T](ref :Reference[T]) :Option[(AnyProperty[T], Any)] = unapply(ref)

	}

	
	class PropertyReferenceFactory[E, K](val Referencing :ObjectProperty[E, K]) { factory =>
		def this(target :E=>K)(implicit typeTag :TypeTag[E]) = this(ObjectProperty(target))
//		val Referencing = ObjectProperty(target)
		
		def Empty[T](key :K) :PropertyReference[T, E, K] =
			new EmptyKey[T, E, K](Referencing, key)

		def Empty[T](keys :Seq[K]) :Reference[T] = new EmptyKeys(Referencing, keys)

		def Full(value :E) :UniquePropertyReference[E, K] =
			new FullKey[E, E, K](Referencing, Referencing(value), value) with One[E]

		def Full[T](key :K, value :T)(implicit proof :T CollectionOf E) :PropertyReference[T, E, K] =
			new FullKey[T, E, K](Referencing, key, value)

//		def Full[T](key :K, value :T) :PropertyReference[T, E, K] = new FullKey[T, E, K](Referencing, key, value)
		
//		def apply[T](key :K) :PropertyReference[T, E, K] = Empty(key)

//		def apply[T](key :K, value :T)(implicit proof :T ReferableByPropertyOf E) :PropertyReference[T, E, K] =
//			new FullKey[T, E, K](Referencing, key, value)

		
		val one = new UniquePropertyReferenceFactory(Referencing)

		def maybe :TypedPropertyReferenceFactory[Option[E]] = as[Option[E]]

		def as[T] :TypedPropertyReferenceFactory[T] = typed.asInstanceOf[TypedPropertyReferenceFactory[T]]

		def in[T[X]] :TypedPropertyReferenceFactory[T[E]] = as[T[E]]

		trait TypedPropertyReferenceFactory[T] {
			def apply(key :K) :PropertyReference[T, E, K] = Empty[T](key)

			def apply(key :K, values :T)(implicit proof :T CollectionOf E) :PropertyReference[T, E, K] = Full(key, values)

			def apply(keys :Seq[K]) :Reference[T] = Empty[T](keys)
		}
		private val typed = new TypedPropertyReferenceFactory[Nothing] {}
	}




//	object ReferableByPropertyOf {
//		implicit def oneself[E] :E ReferableByPropertyOf E = evidence.asInstanceOf[E ReferableByPropertyOf E]
//		implicit def optional[E] :Option[E] ReferableByPropertyOf E = evidence.asInstanceOf[Option[E] ReferableByPropertyOf E]
//		implicit def iterable[T<:Iterable[E], E] :T ReferableByPropertyOf E = evidence.asInstanceOf[T ReferableByPropertyOf E]
//
//		private val evidence = new ReferableByPropertyOf[Nothing, Nothing]
//	}


/*
	class PropertyIterableReferenceFactory[T<:Iterable[E] :TypeTag, E :TypeTag, K](target :E=>K) {
		val Referencing = ObjectProperty(target)

		def Empty(key :K) = apply(key)

		def Full(key :K, value :T) = apply(key, value)

		def apply(key :K) :PropertyReference[T, E, K] = new EmptyKey[T, E, K](Referencing, key)

		def apply(key :K, value :T) :PropertyReference[T, E, K] = new FullKey(Referencing, key, value)

		def unapply(ref :Reference[T]) = ref match {
			case PropertyReference(Referencing, key) =>
				Some(ref.asInstanceOf[PropertyReference[T, E, K]])
			case Full(x) if contentsMatchKey(x) =>
				Some(Full(target(x.head), x))
			case _ => None
		}

		def convertFrom(ref :Reference[T]) = unapply(ref)

		def forceFrom(ref :Reference[T]) = unapply(ref) getOrElse (
			throw new IllegalArgumentException(s"passed reference $ref cannot be adapted to foreign key referencing $Referencing")
			)

		def forceKeyOutOf(ref :Reference[T]) = forceFrom(ref).key

		private def contentsMatchKey(values :T) =
			values.nonEmpty && values.forall(target(_)==target(values.head))
	}
*/







	class UniquePropertyReferenceFactory[E, K](val Referencing :ObjectProperty[E, K]) {
		def this(target :E=>K)(implicit tag :TypeTag[E]) = this(ObjectProperty(target))


		def Empty(key :K) :UniquePropertyReference[E, K] = //One(target, key)
			new EmptyKey[E, E, K](Referencing, key) with One[E]

		def Full(value :E) :UniquePropertyReference[E, K] = //One(value, target)
			new FullKey[E, E, K](Referencing, Referencing(value), value) with One[E]
		
		def unapply(one :One[E]) = one match {
			case PropertyReference(Referencing, key) => Some(one.asInstanceOf[UniquePropertyReference[E, K]])
			case Full(e) => Some(Full(e))
			case _ => None
		}  
		
		def convertFrom(one :One[E]) = unapply(one)
		
		def forceFrom(one :One[E]) = unapply(one) getOrElse (
			throw new IllegalArgumentException(s"passed reference $one cannot be adapted to foreign key referencing $Referencing")
		)

		def forceKeyOutOf(one :One[E]) = forceFrom(one).key
	}






	private case class FullKey[T, E, +I](target :ObjectProperty[E, I], key :I, value :T) extends PropertyReference[T, E, I] {
		def toOpt = Some(value)
	}

	private case class EmptyKey[T, E, I](target :ObjectProperty[E, I], key :I) extends PropertyReference[T, E, I] {
		def toOpt = None
	}

	private trait PropertyMultiReference[+T, E, +I] extends Reference[T] {
		def target :ObjectProperty[E, I]
		def keys :Seq[I]

		override def toString = toOpt.map(v => s"@[$target in $keys: $v]") getOrElse s"@[$target in $keys]"
	}

	private case class EmptyKeys[T, E, I](target :ObjectProperty[E, I], keys :Seq[I]) extends PropertyMultiReference[T, E, I] {
		def toOpt = None
	}



//	trait Many[+T<:Iterable[_]] extends Reference[T]
	type Many[T<:Iterable[_]] = Reference[T]

	type ManyOf[T<:Iterable[E], E] = Many[T]
//	trait ManyOf[+T<:Iterable[E], E] extends Many[T]

	private trait All[+T<:Iterable[_]] extends Many[T] {
		override def toString = toOpt.map(items => s"@All($items)") getOrElse "@All"
	}

	object Many {
		val Unknown :Many[Nothing] = apply()
		val Everything :Many[Nothing] = All[Nothing]()
		
		def apply[T<:Iterable[_]]() :Many[T] = new Many[T] with Unknown[T]
		
		def apply[T<:Iterable[_]](values :T) :Many[T] = new Many[T] with Transient[T] {
			val value = values
		}
		
//		def apply[E, K](target :E=>K) :Nothing = ???

		object All {
			def apply[T<:Iterable[_]]() :Many[T] = new All[T] with Unknown[T]

			def unapply[T](ref :Reference[T]) = ref.isInstanceOf[All[_]]
		}
	}





}
