package com.adpilot.cortb.clientapi.rest

import akka.actor.{ActorLogging, ActorRefFactory}
import spray.httpx.PlayJsonSupport
import spray.routing.{PathMatcher, HttpServiceActor, RequestContext, HttpService}


trait SprayService extends HttpService with PlayJsonSupport {
	@deprecated
	val name :String
	val route :RequestContext => Unit
}

object SprayService {
	val NotFound :RequestContext => Unit = _.reject()

}

//abstract class ActorRefFactoryMixin(val actorRefFactory :ActorRefFactory) extends SprayService


abstract class TreeService(root :String, services :SprayService*) extends SprayService {
	{
		val nameConflicts = services.groupBy(_.name).collect {
			case (name, services) if services.length>1 => (name, services)
		}
		if (nameConflicts.nonEmpty)
			throw new IllegalArgumentException(s"Cannot combine routes; path conflicts found: $nameConflicts")
	}
	val name = root


//	val noAlternative :RequestContext => Unit =
//		rq => rq.reject() //todo rejection

	val route =
		if (services.isEmpty)
			SprayService.NotFound
		else
			pathPrefix(separateOnSlashes(name)) {
				services.map(_.route).reduce(_ ~ _)
			}


}

object TreeService {
//	def actor(root :String, services :SprayService*) :TreeService =
//		new TreeService(root, services:_*) with SprayServiceActor

	def apply(root :String, services :SprayService*)(implicit akkaContext :ActorRefFactory) :TreeService =
		apply(akkaContext, root, services:_*)

	def apply(akkaContext :ActorRefFactory, root :String, services :SprayService*) :TreeService =
		new TreeService(root, services:_*) {
			override implicit def actorRefFactory: ActorRefFactory = akkaContext
		}

}


