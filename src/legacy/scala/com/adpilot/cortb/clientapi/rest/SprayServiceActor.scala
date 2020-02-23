package com.adpilot.cortb.clientapi.rest
import akka.actor.{ActorLogging, Actor}
import akka.actor.Actor.Receive
import spray.routing.{ExceptionHandler, RequestContext, HttpServiceActor}
import spray.util.LoggingContext

trait SprayServiceActor extends HttpServiceActor with SprayService with ActorLogging {
	override def receive: Receive = runRoute(handleExceptions(myExceptionHandler)(route))

	def myExceptionHandler(implicit log: LoggingContext) =
		ExceptionHandler {
			case e: IllegalArgumentException => ctx => {
				log.debug(s"${e.getMessage}\n${e.getStackTrace}\n${e.getCause}")
				ctx.complete(404, e.getMessage)
			}
			case e: Exception => ctx => {
				log.debug(s"${e.getMessage}\n${e.getStackTrace}\n${e.getCause}")
				ctx.complete(500, e.getMessage)
			}
		}
}

class SprayServiceAdapterActor(service :SprayService) extends SprayServiceActor {
	override val name: String = service.name
	override val route = service.route
}
object SprayServiceActor {
	def apply(service :SprayService) = new SprayServiceAdapterActor(service)
}
