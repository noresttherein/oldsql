package com.hcore.clientapi.rest

import akka.actor.ActorLogging
import com.hcore.clientapi.repository.APIRepository
import com.hcore.clientapi.rest.items._
import com.hcore.clientapi.rest.mappings.ItemRepresentation
import spray.httpx.PlayJsonSupport
import spray.routing.HttpServiceActor


trait APIService extends SprayService with PlayJsonSupport { this :ActorLogging =>
	import ItemRepresentation._
	import JsonFormats._

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
		EntityService[CreativeItem]("creatives"),
		EntityService[LandingItem]("landings"),
		EntityService[PixelItem]("pixels"),
		EntityService[TargetingPolicyItem]("targeting"),
		EntityService[TagItem]("products/tags", ProductTags),
		EntityService[TagItem]("campaigns/tags", CampaignTags),
		EntityService[TagItem]("creatives/tags", CreativeTags)
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
