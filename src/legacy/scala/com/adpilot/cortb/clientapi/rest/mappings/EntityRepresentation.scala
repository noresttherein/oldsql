package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.rest.items.{PropertyItem, ValueItem, IdItem, Item}
import com.adpilot.cortb.clientapi.util.ObjectProperty

import scala.reflect.runtime.universe.TypeTag

abstract class EntityRepresentation[I, E<:HasId :TypeTag] extends ItemRepresentationSupport[I, E] {
	protected val IdProperty = ObjectProperty[E](_.id)

	override def one(ref: Item[I]): One[E] = ref match {
		case IdItem(id) => IdRef[E](id)
		case _ => super.one(ref)
	}

	override def maybe(ref: Item[I]): Reference[Option[E]] = ref match {
		case IdItem(id) => IdRef.maybe[E](id)
		case ValueItem(item) => IdRef.maybe[E](entity(item))
		case _ => super.maybe(ref)
	}

}
