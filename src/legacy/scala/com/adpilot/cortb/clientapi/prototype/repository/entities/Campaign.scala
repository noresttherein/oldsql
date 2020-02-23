package com.adpilot.cortb.clientapi.prototype.repository.entities

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Many, One}


case class Campaign(id: Option[Id],
                    hash: String, //?
                    name: String, //*
                    product: One[Product], //*
                    targetingPolicy: One[TargetingPolicy],
                    budgetLimit: Double, //min(_, defaultMax)
                    budgetDailyLimit: Double, //min(_, defaultMax)
                    startTime: Date, //with timezone
                    endTime: Date, //with timezone
                    addedAt: Date, //with timezone
                    deletedAt: Option[Date], //with timezone -
                    status: Campaign.Status,
                    ctype: Campaign.Type,
                    impressionLimit: Option[Long],
                    impressionDailyLimit: Option[Long],
                    clickLimit: Option[Long],
                    clickDailyLimit: Option[Long],
                    creatives: Many[Seq[Creative]] = Many(),
                    orders: Many[Seq[Order]] = Many(),
                    landings: Many[Seq[Landing]] = Many(),
                    thirdPartyImpressionPixelUrl: Option[String] = None
                     ) extends HasId

//+ Creatives + Landing


object Campaign {

  case class Status(name: String)

  case class Type(name: String)

}


