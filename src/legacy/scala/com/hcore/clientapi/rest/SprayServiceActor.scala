package com.hcore.clientapi.rest

import akka.actor.ActorLogging
import spray.routing.HttpServiceActor


trait SprayServiceActor extends HttpServiceActor with SprayService with ActorLogging {

	override def receive: Receive = runRoute(route)
}

class SprayServiceAdapterActor(service :SprayService) extends SprayServiceActor {
	override val name: String = service.name

	override val route = service.route

}


object SprayServiceActor {
	def apply(service :SprayService) = new SprayServiceAdapterActor(service)
}
