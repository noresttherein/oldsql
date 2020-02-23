package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.IdRef
import com.adpilot.cortb.clientapi.prototype.repository.entities.{TagKey, Tag}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.rest.items.{TagKeyItem, IdItem, TagItem}


object TagKeyRepresentation extends EntityRepresentation[TagKeyItem, TagKey] {
	override def entity(item: TagKeyItem): TagKey = {
		import item._
		TagKey(name, description)
	}

	override def mapEntity(entity: TagKey): TagKeyItem = {
		import entity._
		TagKeyItem(name, description)
	}

	private implicit def idRef(item :IdItem[Any]) :One[Nothing] = IdRef(item.id)

	private implicit def idItem(ref :One[Nothing]) :IdItem[Any] = ref match {
		case IdRef(id) => IdItem(id)
		case _ => throw new IllegalArgumentException(s"can't create a TagItem without owner id. Got: $ref")
	}

	lazy val unique = Seq(prop(_.name, _.name))

	lazy val references = Seq()

	lazy val referable = unique

	lazy val properties = referable
}
