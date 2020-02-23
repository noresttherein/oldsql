package com.adpilot.cortb.clientapi.prototype.repository.plain

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, Id, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Many, One}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.{Reference, Selection}

import scala.collection.generic.CanBuildFrom

trait Repository {
	type Store[T<:HasId] <: EntityStore[T]
	type Session

//	protected def newSession :Session

	def inSession[T](block :Session => T) :T




	def apply[E <:HasId, T](select :Selection[T, E], fetch :(E=>Reference[Any])*)(implicit store :Store[E], session :Session) :T


	def save[E <:HasId](entity :E)(implicit store :Store[E], session :Session) :E

	def save[E <:HasId](entities :Seq[E])(implicit store :Store[E], session :Session) :Seq[E] =
		entities.map(save(_))


	def delete[E <:HasId](entity :E)(implicit store :Store[E], session :Session) :Unit = entity.id match {
		case Some(id) => delete[E](id)
		case None => throw new IllegalArgumentException(s"Can't delete $entity: no id associated with instance.")

	}

	def delete[E <:HasId](id :Id)(implicit store :Store[E], session :Session) :Unit =
		delete(IdRef[E](id))

	def delete[E <:HasId, T](selection :Selection[T, E])(implicit store :Store[E], session :Session) :Unit



	trait EntityStore[Entity<:HasId] {

		protected def id(entity :Entity)(implicit s :Session) = entity.id

		def apply(fetch :(Entity => One[Any])*)(implicit s :Session) :Seq[Entity] = apply(Many.All[Seq[Entity]](), fetch:_*)


		def apply[T](selection :Selection[T, Entity], fetch :(Entity => One[Any])*)(implicit s :Session) :T


		def one(reference :One[Entity], fetch :(Entity => One[Any])*)(implicit s :Session) :Entity =
			apply(reference, fetch:_*)

		def maybe(reference :Reference[Option[Entity]], fetch :(Entity => One[Any])*)(implicit s :Session) :Option[Entity] =
			apply(reference, fetch:_*)

		def all[T<:Iterable[Entity]](reference :Reference[T], fetch :(Entity => One[Any])*)(implicit s :Session, cbf :CanBuildFrom[_, Entity, T]) :T =
			apply(reference, fetch:_*)




		def save(entity :Entity)(implicit s :Session) :Entity

		def save(entities :Seq[Entity])(implicit s :Session) :Seq[Entity] = entities.map(save(_))


		def += (entity :Entity)(implicit s :Session) :Option[Id]

		def ++=(entities :Seq[Entity])(implicit s :Session) :Seq[Option[Id]] = entities.map(this += _)


		def -= (entity :Entity)(implicit s :Session) :Unit = id(entity).foreach(delete)



		def delete(selection :Selection[_, Entity])(implicit s :Session) :Unit

		def delete(id :Id)(implicit s :Session) :Unit = delete(IdRef[Entity](id))



	}


}
