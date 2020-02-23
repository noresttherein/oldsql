package com.hcore.clientapi.entities

import java.util.Date

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.Model.{HasId, Id, Many, One}
import com.hcore.clientapi.entities.Product._


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
                   campaigns :Many[Set[Campaign]] = Many()
   ) extends HasId

object Product {
	case class Status(name :String)
	case class Type(name :String)
}
