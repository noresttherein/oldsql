package com.adpilot.cortb.clientapi.rest.items

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id


case class LandingItem(
	id :Option[Id],
	hash :Option[String],
	url :String,
	targetURL :String,
	status :String,
	campaign :Item[CampaignItem]
)


/*
case class Landing(id :Option[Id],
                   hash :String,
                   url :URL,
                   campaign :One[Campaign],
                   targetURL :Option[URL],
                   addedAt :Date, //with timezone
                   deletedAt :Option[Date],
                   status :Landing.Status) extends HasId
*/
