package com.hcore.clientapi.entities

import java.util.Date

import com.hcore.clientapi.entities.Client.{Address, InvoiceInfo, Status}
import com.hcore.clientapi.entities.Model.{HasId, Id}



case class Client(
	id :Option[Id],
	hash :String,
	name :String,
	password :String,
    email :Option[String],
	phone :Option[String],
	contactPerson :Option[String],
	invoiceInfo :InvoiceInfo,
	mailingAddress :Address,
	mailingInvoiceEmail :Option[String],
	status :Status,
	addedAt :Date,
	deletedAt :Option[Date]

) extends HasId


object Client {
	case class InvoiceInfo(
		vatNumber :Option[String],
		address :Address,
		defaultCurrency :Option[String],
		defaultDueDays :Option[Int],
		autoInvoicing :Option[Boolean],
        comments :String,
		email :Option[String]
	)

	case class Address(companyName :Option[String],
	                   street1 :Option[String],
	                   street2 :Option[String],
	                   city :Option[String],
	                   postalCode :Option[String],
	                   country :Option[String])


	case class Status(name :String)
}