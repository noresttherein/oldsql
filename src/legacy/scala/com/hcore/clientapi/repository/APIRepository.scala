package com.hcore.clientapi.repository

import com.hcore.clientapi.entities
import com.hcore.clientapi.entities.Model.HasId
import com.hcore.clientapi.entities._
import com.hcore.ogre.repository.Repository


trait APIRepository extends Repository {

	implicit val Clients :Store[Client]
	implicit val Products :Store[entities.Product]
	implicit val Campaigns :Store[Campaign]
	implicit val Orders :Store[Order]
	implicit val TargetingPolicies :Store[TargetingPolicy]
	implicit val Creatives :Store[Creative]
	implicit val Landings :Store[Landing]
	implicit val Pixels :Store[Pixel]
	val ProductTags :Store[Tag]
	val CampaignTags :Store[Tag]
	val CreativeTags :Store[Tag]



	trait Relationship[L<:HasId, R<:HasId] {
		val left :Store[L]
		val right :Store[R]

	}


}


