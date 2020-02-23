package com.adpilot.cortb.clientapi.rest

import com.adpilot.cortb.clientapi.prototype.repository.APIRepository
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.HasId
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Selection.{SelectSeq, SelectOpt, SelectMany, SelectAll}
import com.adpilot.cortb.clientapi.rest.items._
import com.adpilot.cortb.clientapi.rest.mappings.ItemRepresentation
import com.adpilot.cortb.clientapi.util.Time

trait CRUD[I] {
	import ItemMeta._
	
	def get(items :Items[I], relations :Property[I, _]*) :Seq[I]

	def get(ref :Item[I], relations :Property[I, _]*) :Option[I]
	
	def create(items :Seq[I]) :Seq[I]
	
	def update(updates :Seq[ItemUpdate[I]]) :Seq[I]
	
	def delete(ref :Items[I]) :Unit

}

object CRUD {
	def apply[I :CRUD] :CRUD[I] = implicitly[CRUD[I]]
}


class EntityCRUD[I, E <:HasId, R<:APIRepository](converter :ItemRepresentation[I, E], repository :R, store :R#Store[E])
	extends CRUD[I]
{
	import repository._
	import ItemMeta._
	private implicit val myStore = store.asInstanceOf[repository.Store[E]]

	override def get(items: Items[I], relations: Property[I, _]*): Seq[I] = {
		val selection = SelectSeq(converter.seq(items))
		val fetch = relations.map(p => converter.referenceProperty(p).getOrElse {
			throw new IllegalArgumentException(s"Can't find a reference to fetch for $p ($store). Probably a bug. Known properties are :${converter.references}")
		})
		val results = repository.inSession { implicit s =>
			Time(s"fetching $items from $store with $relations ")(repository(selection, fetch:_*))
		}
		Time(s"converting $items (${results.size} entities) from $store results to items")(results.map(converter.mapEntity))
	}

	override def get(ref: Item[I], relations: Property[I, _]*): Option[I] = repository.inSession { implicit s =>
		val selection = SelectOpt(converter.maybe(ref))
		val fetch = relations.map(converter.referenceProperty(_).get)
		val result = repository(selection, fetch:_*)
		result.map(converter.mapEntity)
	}
	
	override def create(items: Seq[I]): Seq[I] = repository.inSession { implicit s =>
		val entities = items.map(converter.entity)
		if (entities.exists(_.id.isDefined))
			throw new IllegalArgumentException(s"Can't create $store entity with a user-assigned id")
		val saved = repository.save(entities)
		val returned = saved.map(converter.mapEntity(_))
		returned
	}
//		repository.save[E](items.map(repr.entity)).map(repr.fromEntity)


	override def update(updates: Seq[ItemUpdate[I]]): Seq[I] = repository.inSession { implicit s =>
		val entities = updates.collect { case FullItemUpdate(item) => converter.entity(item) }
		if (entities.size!=updates.size)
			throw new IllegalArgumentException(s"Unsupported update type in $updates.\nThis is a bug!")
		if (entities.exists(_.id.isEmpty))
			throw new IllegalArgumentException(s"Cannot update $store without an id")
		val saved = repository.save(entities)
		val returned = saved.map(converter.mapEntity(_))
		returned

	}

	override def delete(items: Items[I]): Unit =  repository.inSession { implicit s =>
		val selection = SelectSeq(converter.seq(items))
		repository.delete(selection)
	}
//		repository.delete[E](repr.reference(ref).asInstanceOf[One[E]])
}


object EntityCRUD {
	def apply[I, E<:HasId](repository :APIRepository)(store :repository.Store[E])(implicit converter :ItemRepresentation[I, E]) :CRUD[I] =
		new EntityCRUD[I, E, repository.type](converter, repository, store)
}