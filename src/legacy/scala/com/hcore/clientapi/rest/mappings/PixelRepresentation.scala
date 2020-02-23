package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Pixel
import com.hcore.clientapi.rest.items.PixelItem


object PixelRepresentation extends EntityRepresentation[PixelItem, Pixel] {
	def AddedAt = new Date()

	override def entity(item: PixelItem): Pixel = {
		import item._
		Pixel(id, hash.orNull, name, Pixel.Type(pixelType), product, previous.map(itemAsOne(_)), AddedAt, None, Pixel.Status(status))
	}


	override def mapEntity(entity: Pixel): PixelItem = {
		import entity._
		PixelItem(id, Option(hash), name, product, previous.map(oneAsItem(_)), pixelType.name, status.name)
	}

	import Implicits._

	private implicit val autoStatus = Pixel.Status.apply _
	private implicit val autoDestatus = (_:Pixel.Status).name
	private implicit val autoType = Pixel.Type.apply _
	private implicit val autoUntype = (_:Pixel.Type).name

	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.name, _.name))

	lazy val references = Seq(one(_.product, _.product))

	lazy val referable = references ++: Seq(map(_.status, _.status), map(_.pixelType, _.pixelType)) ++: unique

	lazy val properties = referable
}
