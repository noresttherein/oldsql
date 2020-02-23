package com.hcore.clientapi.rest.items

import java.util.Date

import com.hcore.clientapi.entities.Model.Id


case class CampaignItem(
	id :Option[Id],
	hash :Option[String],
	name :String,
	product :Item[ProductItem],
	targetingPolicy :Item[TargetingPolicyItem],
	budgetLimit :Option[Double],
	budgetDailyLimit :Option[Double],
	startTime :Option[Date],
	endTime :Option[Date],
	status :String,
	campaignType :String,
	impressionLimit :Option[Long],
	impressionDailyLimit :Option[Long],
	clickLimit :Option[Long],
    clickDailyLimit :Option[Long],
    creatives :Items[CreativeItem],
	orders :Items[OrderItem],
    landings :Items[LandingItem]
)

class CampaignUpdate extends ItemUpdate[CampaignItem]


/*
object CampaignItem extends  {
	import ItemMeta._

	implicit object Meta extends ItemMeta[CampaignItem] {
		val unique = Seq(Property(_.id), Property(_.hash), Property(_.name))

		val referable = Seq(ItemProperty(_.product)) ++: unique

		def properties = referable


	}
}*/
