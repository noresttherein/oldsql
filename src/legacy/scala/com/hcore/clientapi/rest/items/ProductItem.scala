package com.hcore.clientapi.rest.items

import com.hcore.clientapi.entities.Model.Id


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



