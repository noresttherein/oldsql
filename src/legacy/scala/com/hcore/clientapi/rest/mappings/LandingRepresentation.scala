package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Components.URL
import com.hcore.clientapi.entities.Landing
import com.hcore.clientapi.rest.items.LandingItem


object LandingRepresentation extends EntityRepresentation[LandingItem, Landing] {
	def AddedAt = new Date()


	override def entity(item: LandingItem): Landing = Landing(
		item.id, item.hash.orNull, URL(item.url), item.campaign, item.targetURL.map(URL(_)),
		AddedAt, None, Landing.Status(item.status)
	)

	override def mapEntity(entity: Landing): LandingItem = LandingItem(
		entity.id, Option(entity.hash), Option(entity.url).map(_.name).orNull, entity.targetURL.map(_.name),
		entity.status.name, entity.campaign
	)


	import Implicits._
	import Landing._
	implicit val status = Status.apply _
	implicit val unstatus = (_:Status).name


	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash))

	lazy val references = Seq(one(_.campaign, _.campaign))

	lazy val referable = map(_.status, _.status) +: references ++: unique

	lazy val properties = referable
}
