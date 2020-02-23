package com.adpilot.cortb.clientapi.prototype.repository.entities

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference._
import com.adpilot.cortb.clientapi.util.ObjectProperty


object Model {
	val IdProperty = ObjectProperty[HasId](_.id)
	case class Id(value :Long)


	trait HasId {
		val id :Option[Id]
	}

	object HasId {
		def unapply(a :Any) = a match {
			case e :HasId => e.id
			case _ => None
		}
	}

	type Ref[E] = Reference.One[E]

	type IdRef[E<:HasId] = Reference.UniquePropertyReference[E, Option[Id]]

	object IdRef {

		def maybe[E <: HasId](key :Id) :Reference[Option[E]] =
			PropertyReference((_:HasId).id).maybe(Some(key)).asInstanceOf[Reference[Option[E]]]

		def maybe[E <:HasId](value :E) :Reference[Option[E]] =
			if (value.id.isDefined)
				PropertyReference((_:HasId).id).maybe(Some(value.id.get)).asInstanceOf[Reference[Option[E]]]
			else throw new IllegalArgumentException(s"Attempted to create an IdRef for an entity without primary key: $value")

		def seq[E <: HasId](key :Id) :Reference[Seq[E]] =
			PropertyReference((_:HasId).id).in[Seq](Some(key)).asInstanceOf[Reference[Seq[E]]]

		def apply[E <: HasId](value :E) :IdRef[E] =
			if (value.id.isDefined)
				One[HasId, Option[Id]](value, (_:HasId).id).asInstanceOf[IdRef[E]]
			else throw new IllegalArgumentException(s"Attempted to create an IdRef for an entity without primary key: $value")

		def apply[E <: HasId](key :Id) :IdRef[E] = One[HasId, Option[Id]]((_:HasId).id, Some(key)).asInstanceOf[IdRef[E]]

		def apply[E <: HasId](one :One[E]) :IdRef[E] = one match {
			case Full(e) if e.id.isDefined => apply(e)
			case UniquePropertyReference(IdProperty, Some(id :Id)) => apply[E](id)
			case _ => throw new IllegalArgumentException(s"Can't create an IdRef from $one")
		}

		def unapply[E](ass :Reference[E]) = ass match {
			case UniquePropertyReference(IdProperty, Some(key :Id)) => Some(key)
			case Full(HasId(id)) => Some(id)
			case _ => None
		}

		def idOf[E <: HasId](ass :Reference[E]) = ass match {
			case IdRef(id) => id
			case _ => throw new IllegalArgumentException(s"expected an id-based foreign key, got $ass")
		}
	}
/*
	type Ref[E] = Association[E]

	type IdRef[E] = Association.One[Id, E]

	object Ref {
		def apply[T <: Iterable[_]](values :T) :Ref[T] = Many(values)
//		def apply[T <: HasId](value :T) :IdRef[T] = IdRef(value)
	}

	object IdRef {
		def apply[E <: HasId](value :E) :IdRef[E] = value.id match {
			case Some(id) => ForeignKey(id, value)
			case None => One(value)
		}
		def apply(id :Id) :IdRef[Nothing] = ForeignKey(id)
	}

	trait Entity[E] {
		def pk(entity :E) :Id

	}

	trait Relation[L, R, T] {
		val name :String
		val target :Entity[R]
		def apply(entity :L) :T
		def values(entity :L) :Iterable[R]


	}

*/


}

//object Domain {
//	object static extends Domain
//}
