package com.hcore.clientapi.entities

import java.util.Date

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.Model.{HasId, Id, One}


case class Order(id :Option[Id],
	             hash :String,
	             name :String,
	             campaign :One[Campaign],
	             targetingPolicy :One[TargetingPolicy],
	             budgetLimit :Double,
	             budgetDailyLimit :Double,
	             startTime :Date, //with timezone
	             endTime :Date, //with timezone
	             addedAt :Date, //with timezone
	             deletedAt :Option[Date], //with timezone
	             status :Order.Status,
	             otype :Order.Type,
	             bidding :Order.BiddingConfig,
	             trackingCPAPixel0 :Option[One[Pixel]],
	             trackingCPAPixel1 :Option[One[Pixel]],
	             trackingCPAPixel2 :Option[One[Pixel]],
	             dontIncludeInLearning :Boolean,
	             impressionLimit :Option[Long],
	             impressionDailyLimit :Option[Long],
	             clickLimit :Option[Long],
	             clickDailyLimit :Option[Long]) extends HasId


object Order {
	case class BiddingConfig(strategy :Option[BiddingStrategy],
		                     cpmValue :Option[Double],
		                     cpcValue :Option[Double],
		                     cpaPixel0 :Option[One[Pixel]],
		                     cpaPixel1 :Option[One[Pixel]],
		                     cpaPixel2 :Option[One[Pixel]],
		                     cpaValue0 :Option[Double],
		                     cpaValue1 :Option[Double],
		                     cpaValue2 :Option[Double])

	case class Status(name :String)
	case class Type(name :String)

}