package com.adpilot.cortb.clientapi.rest

import akka.actor.Actor.Receive
import akka.actor.ActorLogging
import com.adpilot.cortb.clientapi.prototype.repository.APIRepository
import com.adpilot.cortb.clientapi.rest.items._
import com.adpilot.cortb.clientapi.rest.mappings.ItemRepresentation
import spray.httpx.PlayJsonSupport
import spray.routing.{HttpServiceActor, RequestContext}


trait APIService extends SprayService with PlayJsonSupport { this :ActorLogging =>
	import JsonFormats._
	import ItemRepresentation._

	val repository :APIRepository
	val name = "clientapi"

	lazy val CRUDS = new CRUDs(repository)
	import CRUDS._


	private val entities = TreeService(name,
		HealthCheckService("healthCheck"),
		EntityService[ClientItem]("clients"),
		EntityService[ProductItem]("products"),
		EntityService[CampaignItem]("campaigns"),
		EntityService[OrderItem]("orders"),
		CreativeImageService(repository, "creatives/image"),
		OrphanedCreativesService(repository, "creatives/orphans"),
		EntityService[CreativeItem]("creatives"),
		EntityService[LandingItem]("landings"),
		EntityService[PixelItem]("pixels"),
		EntityService[TargetingPolicyItem]("targeting"),
		EntityService[TagItem]("products/tags", ProductTags),
		EntityService[TagItem]("campaigns/tags", CampaignTags),
		EntityService[TagItem]("creatives/tags", CreativeTags),
		EntityService[TagKeyItem]("creatives/tags/keys", CreativeTagKeys),
		EntityService[StatusItem]("statuses")
	)

	val route = entities.route

}


class TestActor extends HttpServiceActor with ActorLogging with PlayJsonSupport {
	val route = get { path("clientapi"/"orders") { complete {{
		log.info("handling request")
		System.err.println("sleeping "+Thread.currentThread().getName)
		Thread.sleep(1000)
		System.err.println("woke up "+Thread.currentThread().getName)
		Seq[Int]()
	}}}}

	override def receive: Receive = runRoute(route)
}
