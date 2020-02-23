package com.hcore.clientapi.rest.mappings

import com.hcore.clientapi.entities.Model.{HasId, IdRef}
import com.hcore.clientapi.rest.items.{IdItem, Item, ValueItem}
import com.hcore.ogre.model.Reference
import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.reflect.runtime.universe.TypeTag

abstract class EntityRepresentation[I, E<:HasId :TypeTag] extends ItemRepresentationSupport[I, E] {
	protected val IdProperty = PropertyChain[E](_.id)

	override def one(ref: Item[I]): Reference[E] = ref match {
		case IdItem(id) => IdRef[E](id)
		case _ => super.one(ref)
	}

	override def maybe(ref: Item[I]): Reference[Option[E]] = ref match {
		case IdItem(id) => IdRef.maybe[E](id)
		case ValueItem(item) => IdRef.maybe[E](entity(item))
		case _ => super.maybe(ref)
	}

}
