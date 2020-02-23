package com.adpilot.cortb.clientapi.rest.items

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id
import com.adpilot.cortb.clientapi.rest.items.ItemMeta.Property


case class ProductItem(
	id :Option[Id],
	hash :Option[String],
	name :String,
	description :Option[String],
	status :String,
	productType :String,
	country :Option[String],
	campaigns :Items[CampaignItem],
	client :Item[ClientItem]
)


/*
object ProductItem {

	implicit object Meta extends ItemMeta[ProductItem] {

		val unique = Seq(Property(_.id), Property(_.hash), Property(_.name))

		val referable = Seq(ItemProperty(_.client), Property(_.status)) ++: unique

		val properties = referable

	}
}
*/



