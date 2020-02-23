package com.adpilot.cortb.clientapi.prototype.repository.entities

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}

import Product._
import Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Many, One}

case class Product(id :Option[Id],
	               hash :String,
	               client :One[Client],
	               name :String,
	               description :Option[String],
	               addedAt :Date,
	               deletedAt :Option[Date],
	               status: Status,
                   ptype :Type,
                   country :Option[Country],
                   campaigns :Many[Seq[Campaign]] = Many()
   ) extends HasId

object Product {
	case class Status(name :String)
	case class Type(name :String)
}
