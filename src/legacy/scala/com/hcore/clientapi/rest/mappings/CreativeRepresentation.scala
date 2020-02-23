package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Creative
import com.hcore.clientapi.rest.items.CreativeItem


object CreativeRepresentation extends EntityRepresentation[CreativeItem, Creative] {
	def AddedAt = new Date()


	override def entity(item: CreativeItem): Creative = {
		import item._
		Creative(
			id, hash.orNull, Creative.Format(format),
			Creative.File(fileName, Creative.FileFormat(fileFormat), fileContents),
			AddedAt, None, Creative.Status(status), Creative.Type(creativeType),
			product, campaigns
		)
	}

	override def mapEntity(entity: Creative): CreativeItem = {
		import entity._
		CreativeItem(
			id, Option(hash), format.format, file.name, file.format.name, file.data,
			status.name, ctype.name, product, campaigns
		)
	}

	import Implicits._
	private implicit val autoStatus = Creative.Status.apply _
	private implicit val autoUnstatus = (_:Creative.Status).name
	private implicit val autoType = Creative.Type.apply _
	private implicit val autoUntype = (_:Creative.Type).name

	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.file.name, _.fileName))
	private lazy val productProp = one(_.product, _.product)
	lazy val references = Seq(productProp, set(_.campaigns, _.campaigns))

	lazy val referable = productProp +: map(_.status, _.status) +: unique

	lazy val properties = Seq(map(_.status, _.status), map(_.ctype, _.creativeType)) ++: references ++: unique
}
