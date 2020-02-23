package com.hcore.clientapi.rest.mappings

import com.hcore.clientapi.entities.Model.IdRef
import com.hcore.clientapi.entities.Tag
import com.hcore.clientapi.rest.items.{IdItem, TagItem}
import com.hcore.ogre.model.Reference


object TagRepresentation extends EntityRepresentation[TagItem, Tag] {
	override def entity(item: TagItem): Tag = {
		import item._
		Tag(id, owner, key, value)
	}

	override def mapEntity(entity: Tag): TagItem = {
		import entity._
		TagItem(id, owner, key, value)
	}

	private implicit def idRef(item :IdItem[Any]) :Reference[Nothing] = IdRef(item.id)

	private implicit def idItem(ref :Reference[Nothing]) :IdItem[Any] = ref match {
		case IdRef(id) => IdItem(id)
		case _ => throw new IllegalArgumentException(s"can't create a TagItem without owner id. Got: $ref")
	}

	lazy val unique = Seq(prop(_.id, _.id))

	lazy val references = Seq()

	lazy val referable = Seq(prop(_.key, _.key), map(_.owner, _.owner, "owner.id")) ++: unique

	lazy val properties = referable
}
