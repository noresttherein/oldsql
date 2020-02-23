package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Components.BiddingStrategy
import com.hcore.clientapi.entities.Order
import com.hcore.clientapi.entities.Order.BiddingConfig
import com.hcore.clientapi.rest.items.{BiddingItem, OrderItem}


object OrderRepresentation extends EntityRepresentation[OrderItem, Order] {
	val DefaultBudgetLimit = 0.0
	val DefaultBudgetDailyLimit = 0.0
	val DefaultBiddingConfig :BiddingConfig = BiddingConfig(None, None, None, None, None, None, None, None, None)

	def StartTime = new Date()
	def EndTime = new Date()
	def AddedAt = new Date()

	def entity(item: OrderItem): Order = {
		import item._
		Order(
			id, hash.orNull, name, campaign, targetingPolicy,
			budgetLimit getOrElse DefaultBudgetLimit, budgetDailyLimit getOrElse DefaultBudgetDailyLimit,
			startTime getOrElse StartTime, endTime getOrElse EndTime,
			AddedAt, None, Order.Status(status), Order.Type(orderType), biddingItem(bidding),
			trackingCPAPixel0, trackingCPAPixel1, trackingCPAPixel2,
			includeInLearning.exists(!_), impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit
		)
	}



	def mapEntity(entity: Order): OrderItem = {
		import entity._
		OrderItem(
			id, Option(hash), name, campaign, targetingPolicy,
			Some(budgetLimit), Some(budgetDailyLimit), Some(startTime), Some(endTime),
			status.name, otype.name, impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit,
			mapBidding(bidding), trackingCPAPixel0, trackingCPAPixel1, trackingCPAPixel2, Some(!dontIncludeInLearning)
		)
	}

	def biddingItem(bidding :Option[BiddingItem]) :BiddingConfig = bidding.map { item =>
		import item._
		BiddingConfig(
			strategy.map(BiddingStrategy), cpmValue, cpcValue, cpaPixel0, cpaPixel1, cpaPixel2,
			cpaValue0, cpaValue1, cpaValue2

		)
	} getOrElse DefaultBiddingConfig

	def mapBidding(bidding :BiddingConfig) :Option[BiddingItem] = Some {
		import bidding._
		BiddingItem(
			strategy.map(_.name), cpmValue, cpcValue, cpaPixel0, cpaPixel1, cpaPixel2, cpaValue0, cpaValue1, cpaValue2
		)
	}


	import Implicits._


	private lazy val campaignProp = one(_.campaign, _.campaign)
	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.name, _.name))

	lazy val referable = campaignProp +: unique
	lazy val references = Seq(campaignProp, one(_.targetingPolicy, _.targetingPolicy))


	lazy val properties = noDups(references ++: referable)
}
