package com.adpilot.cortb.clientapi.rest.items

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.Product
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One


case class PixelItem(
	id :Option[Id],
	hash :Option[String],
	name :String,
	product :Item[ProductItem],
	previous :Option[Item[PixelItem]],
	pixelType :String,
	status :String
)


/*
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
*/
