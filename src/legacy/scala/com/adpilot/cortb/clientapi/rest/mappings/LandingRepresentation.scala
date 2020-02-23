package com.adpilot.cortb.clientapi.rest.mappings

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities.Components.URL
import com.adpilot.cortb.clientapi.prototype.repository.entities.Landing
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.rest.items.LandingItem


object LandingRepresentation extends EntityRepresentation[LandingItem, Landing] {
	def AddedAt = new Date()


	override def entity(item: LandingItem): Landing = Landing(
		item.id, item.hash.orNull, URL(item.url), item.campaign, URL(item.targetURL),
		AddedAt, None, Landing.Status(item.status)
	)

	override def mapEntity(entity: Landing): LandingItem = LandingItem(
		entity.id, Option(entity.hash), Option(entity.url).map(_.name).orNull, entity.targetURL.name,
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
