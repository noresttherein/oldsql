package com.hcore.ogre.repository

import com.hcore.clientapi.entities.Model.{HasId, Id, IdRef}
import com.hcore.ogre.model.ComposedOf.{ComposableFrom, DecomposableTo}
import com.hcore.ogre.model.{ComposedOf, Reference}
import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.TypeTag

trait Repository {
	type Store[T<:HasId] <: EntityStore[T]
	type Session

//	protected def newSession :Session

	def inSession[T](block :Session => T) :T




	def apply[E <:HasId, T](select :Reference[T], fetch :(E=>Reference[Any])*)(implicit store :Store[E], composition :T ComposedOf E, session :Session) :T


	def save[E <:HasId](entity :E)(implicit store :Store[E], session :Session) :E

	def save[E <:HasId](entities :Seq[E])(implicit store :Store[E], session :Session) :Seq[E] =
		entities.map(save(_))


	def delete[E <:HasId](entity :E)(implicit store :Store[E], session :Session) :Unit = entity.id match {
		case Some(id) => delete[E](id)
		case None => throw new IllegalArgumentException(s"Can't delete $entity: no id associated with instance.")

	}

	def delete[E <:HasId](id :Id)(implicit store :Store[E], session :Session) :Unit =
		delete(IdRef[E](id))

	def delete[E <:HasId, T](selection :Reference[T])(implicit store :Store[E], composition :T ComposedOf E, session :Session) :Unit



	trait EntityStore[Entity<:HasId] {
		implicitly[DecomposableTo[Seq[Entity], Entity]]
		implicitly[ComposableFrom[Seq[Entity], Entity]]


		protected def id(entity :Entity)(implicit s :Session) = entity.id

		def apply(fetch :(Entity => Reference[Any])*)(implicit s :Session) :Seq[Entity] = apply(Reference.All.as[Seq[Entity]], fetch:_*)


//		def apply[T](reference :CompositeReference[T, Entity], fetch :(Entity => Reference[Any])*)(implicit s :Session) :T =
//			apply(reference :Reference[T], fetch:_*)(reference.composition, s)

		def apply[T](selection :Reference[T], fetch :(Entity => Reference[Any])*)(implicit composition :T ComposedOf Entity, s :Session) :T


		def one(reference :Reference[Entity], fetch :(Entity => Reference[Any])*)(implicit s :Session) :Entity =
			apply(reference, fetch:_*)

		def maybe(reference :Reference[Option[Entity]], fetch :(Entity => Reference[Any])*)(implicit s :Session) :Option[Entity] =
			apply(reference, fetch:_*)

		def all[T<:Iterable[Entity]](reference :Reference[T], fetch :(Entity => Reference[Any])*)(implicit s :Session, cbf :CanBuildFrom[_, Entity, T]) :T =
			apply(reference, fetch:_*)




		def save(entity :Entity)(implicit s :Session) :Entity

		def save(entities :Seq[Entity])(implicit s :Session) :Seq[Entity] = entities.map(save(_))


		def += (entity :Entity)(implicit s :Session) :Option[Id]

		def ++=(entities :Seq[Entity])(implicit s :Session) :Seq[Option[Id]] = entities.map(this += _)


		def -= (entity :Entity)(implicit s :Session) :Unit = id(entity).foreach(delete)



		def delete[T](selection :Reference[T])(implicit composite :T ComposedOf Entity, s :Session) :Unit

		def delete(id :Id)(implicit s :Session) :Unit = delete(IdRef[Entity](id))



	}



	trait Fetch[E <:HasId, T]

	object Fetch {
		implicit def entity[E<:HasId :Store, T <:HasId :Store](property :PropertyChain[E, Reference[T]]) :FetchEntity[E, T] =
			FetchEntity(property)

		implicit def entity[E<:HasId :Store :TypeTag, T <:HasId :Store](property :E=>Reference[T]) :FetchEntity[E, T] =
			entity(PropertyChain(property))



	}

	trait FetchEntity[E<:HasId, T<:HasId] extends Fetch[E, Reference[T]]

	object FetchEntity {
		def apply[E<:HasId :Store, T <:HasId :Store](property :PropertyChain[E, Reference[T]]) :FetchEntity[E, T] = ???

		def apply[E<:HasId :Store :TypeTag, T <:HasId :Store](property :E=>Reference[T]) :FetchEntity[E, T] =
			apply(PropertyChain(property))
	}

	trait FetchOption[E<:HasId, T<:HasId] extends Fetch[E, Reference[Option[T]]]

	trait FetchMany[E<:HasId, T]



}


/*
object Repository {
	trait Fetch[E <:HasId, T]

	trait FetchEntity[E<:HasId, T<:HasId] extends Fetch[E, One[T]]

	object FetchEntity

	trait FetchOption[E<:HasId, T<:HasId] extends Fetch[E, Reference[Option[T]]]

	trait FetchMany[E<:HasId, T]

}*/
