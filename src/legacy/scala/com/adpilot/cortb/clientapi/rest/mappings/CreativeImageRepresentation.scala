package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Creative
import com.adpilot.cortb.clientapi.prototype.repository.entities.Creative.Image
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.rest.items.CreativeImageItem


object CreativeImageRepresentation extends ItemRepresentationSupport[CreativeImageItem, Creative.Image] {



	override def unique = Seq()

	override def references = Seq()

	override def referable = Seq()

	override def properties = Seq()

	override def entity(item: CreativeImageItem): Image =
		Image(item.contents)

	override def mapEntity(entity: Image): CreativeImageItem =
		CreativeImageItem(entity.data)
}
