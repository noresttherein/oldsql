package com.adpilot.cortb.clientapi.prototype.repository

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, Id, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities._
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Selection.{SelectOpt, SelectOne, SelectMany}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.{Selection, Reference}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{Many, One}
import com.adpilot.cortb.clientapi.prototype.repository.plain.Repository

import scala.Product
import scala.collection.generic.CanBuildFrom


trait APIRepository extends Repository {

	implicit val Clients :Store[Client]
	implicit val Products :Store[entities.Product]
	implicit val Campaigns :Store[Campaign]
	implicit val Orders :Store[Order]
	implicit val TargetingPolicies :Store[TargetingPolicy]
	implicit val Creatives :Store[Creative]
	implicit val Landings :Store[Landing]
	implicit val Pixels :Store[Pixel]
	implicit val AppnexusCreativesData : Store[AppnexusCreativeData]
	val ProductTags :Store[Tag]
	val CampaignTags :Store[Tag]
	val CreativeTags :Store[Tag]
	val CreativeTagKeys :Store[TagKey]
	val Statuses :Store[StatusType]

	def orphanedCreatives()(implicit s :Session) :Seq[Creative]

	trait Relationship[L<:HasId, R<:HasId] {
		val left :Store[L]
		val right :Store[R]

	}


}


