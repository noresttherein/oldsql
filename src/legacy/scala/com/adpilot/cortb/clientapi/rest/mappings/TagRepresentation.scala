package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.IdRef
import com.adpilot.cortb.clientapi.prototype.repository.entities.Tag
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Full, One}
import com.adpilot.cortb.clientapi.rest.items.{ValueItem, IdItem, Item, TagItem}


object TagRepresentation extends EntityRepresentation[TagItem, Tag] {
	override def entity(item: TagItem): Tag = {
		import item._
		Tag(id, owner, key, value)
	}

	override def mapEntity(entity: Tag): TagItem = {
		import entity._
		TagItem(id, owner, key, value)
	}

	import Implicits._

	private implicit def idRef(item :IdItem[Any]) :One[Nothing] = IdRef(item.id)

	private implicit def idItem(ref :One[Nothing]) :IdItem[Any] = ref match {
		case IdRef(id) => IdItem(id)
		case _ => throw new IllegalArgumentException(s"can't create a TagItem without owner id. Got: $ref")
	}

	lazy val unique = Seq(prop(_.id, _.id))

	lazy val references = Seq()

	lazy val referable = Seq(prop(_.key, _.key), map(_.owner, _.owner, "owner.id")) ++: unique

	lazy val properties = referable
}
