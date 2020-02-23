package com.hcore.clientapi.entities

import java.util.Date

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.Model.{HasId, Id, One}

case class Landing(id :Option[Id],
	               hash :String,
	               url :URL,
	               campaign :One[Campaign],
	               targetURL :Option[URL],
	               addedAt :Date, //with timezone
	               deletedAt :Option[Date],
	               status :Landing.Status) extends HasId


object Landing {
	case class Status(name :String)
}
