package com.adpilot.cortb.clientapi.prototype.repository.entities

import java.util.Date

import Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One

case class Landing(id :Option[Id],
	               hash :String,
	               url :URL,
	               campaign :One[Campaign],
	               targetURL :URL,
	               addedAt :Date, //with timezone
	               deletedAt :Option[Date],
	               status :Landing.Status) extends HasId


object Landing {
	case class Status(name :String)
}
