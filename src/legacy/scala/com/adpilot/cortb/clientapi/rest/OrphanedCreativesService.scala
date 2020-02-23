package com.adpilot.cortb.clientapi.rest

import akka.actor.ActorRefFactory
import com.adpilot.cortb.clientapi.prototype.repository.APIRepository
import com.adpilot.cortb.clientapi.prototype.repository.entities.Creative
import com.adpilot.cortb.clientapi.rest.items.{CreativeItem, ItemMeta}
import com.adpilot.cortb.clientapi.rest.mappings.ItemRepresentation


abstract class OrphanedCreativesService(repository :APIRepository, val name :String) extends SprayService {
	val meta = implicitly[ItemRepresentation[CreativeItem, Creative]]

	import JsonFormats._

	val route = get {
		path(separateOnSlashes(name)) {
			complete {
				repository.inSession { implicit s =>
					val creatives = repository.orphanedCreatives()
					creatives.map(meta.mapEntity(_))
				}
			}
		}
	}

}

object OrphanedCreativesService {
	def apply(repository :APIRepository, name :String)(implicit akkaContext :ActorRefFactory) :OrphanedCreativesService =
		new OrphanedCreativesService(repository, name) {
			override def actorRefFactory: ActorRefFactory = akkaContext
		}

}