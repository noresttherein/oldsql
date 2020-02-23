package com.hcore.clientapi.rest

import com.hcore.clientapi.entities.Model.HasId
import com.hcore.clientapi.repository.APIRepository
import com.hcore.clientapi.rest.items._
import com.hcore.clientapi.rest.mappings.ItemRepresentation
import com.hcore.ogre.morsels.Time

trait CRUD[I] {
	import ItemMeta._
	
	def get(items :Items[I], relations :Property[I, _]*) :Seq[I]

	def get(ref :Item[I], relations :Property[I, _]*) :Option[I]
	
	def create(items :Seq[I]) :Seq[I]
	
	def update(updates :Seq[ItemUpdate[I]]) :Seq[I]
	
	def delete(ref :Item[I]) :Unit

}

object CRUD {
	def apply[I :CRUD] :CRUD[I] = implicitly[CRUD[I]]
}


class EntityCRUD[I, E <:HasId, R<:APIRepository](converter :ItemRepresentation[I, E], repository :R, store :R#Store[E])
	extends CRUD[I]
{
	import ItemMeta._
	private implicit val myStore = store.asInstanceOf[repository.Store[E]]

	override def get(items: Items[I], relations: Property[I, _]*): Seq[I] = {
		val selection = converter.seq(items)
		val fetch = relations.map(p => converter.referenceProperty(p).getOrElse {
			throw new IllegalArgumentException(s"Can't find a reference to fetch for $p ($store). Probably a bug. Known properties are :${converter.references}")
		})
		val results = repository.inSession { implicit s =>
			Time(s"fetching $items from $store with $relations ")(repository(selection, fetch:_*))
		}
		Time(s"converting $items (${results.size} entities) from $store results to items")(results.map(converter.mapEntity))
	}

	override def get(ref: Item[I], relations: Property[I, _]*): Option[I] = repository.inSession { implicit s =>
		val selection = converter.maybe(ref)
		val fetch = relations.map(converter.referenceProperty(_).get)
		val result = repository(selection, fetch:_*)
		result.map(converter.mapEntity)
	}
	
	override def create(items: Seq[I]): Seq[I] = repository.inSession { implicit s =>
		val entities = items.map(converter.entity)
		val saved = repository.save(entities)
		val returned = saved.map(converter.mapEntity(_))
		returned
	}
//		repository.save[E](items.map(repr.entity)).map(repr.fromEntity)


	override def update(updates: Seq[ItemUpdate[I]]): Seq[I] = ???

	override def delete(ref: Item[I]): Unit =  ???
//		repository.delete[E](repr.reference(ref).asInstanceOf[One[E]])
}


object EntityCRUD {
	def apply[I, E<:HasId](repository :APIRepository)(store :repository.Store[E])(implicit converter :ItemRepresentation[I, E]) :CRUD[I] =
		new EntityCRUD[I, E, repository.type](converter, repository, store)
}