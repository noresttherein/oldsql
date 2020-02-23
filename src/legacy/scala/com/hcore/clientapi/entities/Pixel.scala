package com.hcore.clientapi.entities

import java.util.Date

import com.hcore.clientapi.entities.Model.{HasId, Id, One}


case class Pixel(
	id :Option[Id],
	hash :String,
	name :String,
	pixelType :Pixel.Type,
	product :One[Product],
	previous :Option[One[Pixel]],
	addedAt :Date,
	deletedAt :Option[Date],
	status :Pixel.Status
) extends HasId

object Pixel {
	case class Type(name :String)
	case class Status(name :String)
}
