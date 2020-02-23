package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Campaign
import com.hcore.clientapi.rest.items._


object CampaignRepresentation extends EntityRepresentation[CampaignItem, Campaign] {
	import ItemRepresentation._

	private val DefaultBudgetLimit = 0.0 //todo
	private val DefaultDailyBudgetLimit = 0.0
	private def DefaultStartTime = new Date()
	private def DefaultEndTime = new Date()
	private def AddedAt = new Date()

	override def entity(item: CampaignItem): Campaign =
		Campaign(item.id, item.hash.orNull, item.name, item.product, item.targetingPolicy,
			item.budgetLimit getOrElse DefaultBudgetLimit,
			item.budgetDailyLimit getOrElse DefaultDailyBudgetLimit,
			item.startTime getOrElse DefaultStartTime,
			item.endTime getOrElse DefaultEndTime,
			AddedAt, None, Campaign.Status(item.status), Campaign.Type(item.campaignType),
			item.impressionLimit, item.impressionDailyLimit, item.clickLimit, item.clickDailyLimit,
			item.creatives, item.orders, item.landings
		)

	override def mapEntity(entity: Campaign): CampaignItem =
		CampaignItem(entity.id, Option(entity.hash), entity.name, entity.product, entity.targetingPolicy,
			Some(entity.budgetLimit), Some(entity.budgetDailyLimit), Some(entity.startTime), Some(entity.endTime),
			entity.status.name, entity.ctype.name, entity.impressionLimit, entity.impressionDailyLimit,
			entity.clickLimit, entity.clickDailyLimit, entity.creatives, entity.orders, entity.landings
		)



	import Implicits._
	implicit val status = Campaign.Status.apply _
	implicit val unstatus = (_:Campaign.Status).name
	implicit val ctype = Campaign.Type.apply _
	implicit val untype = (_:Campaign.Type).name


	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.name, _.name))
	lazy val single = Seq(one(_.product, _.product), one(_.targetingPolicy, _.targetingPolicy))
	lazy val referable = single ++: Seq(map(_.status, _.status), map(_.ctype, _.campaignType, "type")) ++: unique
	lazy val references = Seq(set(_.creatives, _.creatives), set(_.landings, _.landings), set(_.orders, _.orders)) ++: single
	lazy val properties = noDups(references ++: referable)
}
