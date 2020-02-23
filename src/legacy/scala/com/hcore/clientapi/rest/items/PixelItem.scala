package com.hcore.clientapi.rest.items

import com.hcore.clientapi.entities.Model.Id


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
