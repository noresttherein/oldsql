package com.hcore.clientapi.rest.items

import com.hcore.clientapi.entities.Model.Id


case class LandingItem(
	id :Option[Id],
	hash :Option[String],
	url :String,
	targetURL :Option[String],
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
