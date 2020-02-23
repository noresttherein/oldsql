package com.hcore.clientapi.entities

import com.hcore.ogre.model.Restriction.{Restrictive, PropertyEquals}
import com.hcore.ogre.model.Reference
import com.hcore.ogre.model.Reference._
import com.hcore.ogre.morsels.necromancy.PropertyChain

/** Convenience methods for working with Reference[<:HasId] in general and references pointing out entities by their id in particular */
object Model {
	type Many[X] = Reference[X]
	type One[X] = Reference[X]

	def Many[X]() = Reference.Unknown[X]
//	def One(ref :)


	val IdProperty = PropertyChain[HasId](_.id)
	def IdPropertyOf[E<:HasId] :PropertyChain[E, Option[Id]] = IdProperty

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

	type Ref[E] = One[E]

	type IdRef[E<:HasId] = Reference[E] //Reference.UniquePropertyReference[E, Option[Id]]

	object IdRef {
		def referBy[E<:HasId](key :Id) :ReferenceComposer[E] =
			Reference.Satisfying(IdPropertyOf[E] === Some(key))

		def maybe[E <: HasId](key :Id) :Reference[Option[E]] =
			referBy[E](key)
//			PropertyReference((_:HasId).id).maybe(Some(key)).asInstanceOf[Reference[Option[E]]]

		def maybe[E <:HasId](value :E) :Reference[Option[E]] =
			if (value.id.isDefined)
				referBy[E](value.id.get)
//				PropertyReference((_:HasId).id).maybe(Some(value.id.get)).asInstanceOf[Reference[Option[E]]]
			else throw new IllegalArgumentException(s"Attempted to create an IdRef for an entity without primary key: $value")

		def seq[E <: HasId](key :Id) :Reference[Seq[E]] =
			referBy[E](key)
//			PropertyReference((_:HasId).id).in[Seq](Some(key)).asInstanceOf[Reference[Seq[E]]]

		def apply[E <: HasId](value :E) :IdRef[E] =
			if (value.id.isDefined)
				referBy[E](value.id.get)
			else throw new IllegalArgumentException(s"Attempted to create an IdRef for an entity without primary key: $value")

		def apply[E <: HasId](key :Id) :IdRef[E] = //One[HasId, Option[Id]]((_:HasId).id, Some(key)).asInstanceOf[IdRef[E]]
			referBy[E](key)

		def apply[E <: HasId](one :One[E]) :IdRef[E] = one match {
			case Full(e) if e.id.isDefined => apply(e)
			case Satisfying(PropertyEquals(IdProperty, Some(id :Id))) => apply[E](id)
//			case UniquePropertyReference(IdProperty, Some(id :Id)) => apply[E](id)
			case _ => throw new IllegalArgumentException(s"Can't create an IdRef from $one")
		}

		def unapply[E](ass :Reference[E]) = ass match {
			case Satisfying(PropertyEquals(IdProperty, Some(id :Id))) => Some(id)
//			case UniquePropertyReference(IdProperty, Some(key :Id)) => Some(key)
			case Full(HasId(id)) => Some(id)
			case _ => None
		}

		def idOf[E <: HasId](ass :Reference[E]) = ass match {
			case IdRef(id) => id
			case _ => throw new IllegalArgumentException(s"expected an id-based foreign key, got $ass")
		}
	}



}

