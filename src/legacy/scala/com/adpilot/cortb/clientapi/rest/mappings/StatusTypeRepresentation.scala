package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.IdRef
import com.adpilot.cortb.clientapi.prototype.repository.entities.{StatusType, TagKey}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.rest.items.{StatusItem, IdItem, TagKeyItem}


object StatusTypeRepresentation extends EntityRepresentation[StatusItem, StatusType] {
	override def entity(item: StatusItem) = StatusType(item.name, item.description)

	override def mapEntity(entity: StatusType) = StatusItem(entity.name, entity.description)

	private implicit def idRef(item :IdItem[Any]) :One[Nothing] = IdRef(item.id)

	private implicit def idItem(ref :One[Nothing]) :IdItem[Any] = ref match {
		case IdRef(id) => IdItem(id)
		case _ => throw new IllegalArgumentException(s"can't create a StatusType without owner id. Got: $ref")
	}

	lazy val unique = Seq(prop(_.name, _.name))

	lazy val references = Seq()

	lazy val referable = unique

	lazy val properties = referable
}
