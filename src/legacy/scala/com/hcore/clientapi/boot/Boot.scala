package com.hcore.clientapi.boot

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import akka.routing.FromConfig
import com.hcore.clientapi.Configuration
import com.hcore.clientapi.repository.PlainAPIRepository
import com.hcore.clientapi.rest.APIServiceActor
import spray.can.Http

object Boot extends App with Configuration {


	System.err.println(s"Starting clientapi service connected to: $databaseConfig")
	val repository = new PlainAPIRepository(databaseConfig, blockedClients.toSeq)

	// create an actor system for application
	implicit val system = ActorSystem("cortb-client-api")

//	val restService = system.actorOf(Props(classOf[APIServiceActor], repository), "rest-clientapi")
//	val restService = system.actorOf(Props(classOf[TestActor]), "rest-clientapi")
	val restService = system.actorOf(FromConfig.props(Props(classOf[APIServiceActor], repository)), "rest-routes")

	// start HTTP server with rest service actor as a handler
	IO(Http) ! Http.Bind(restService, serviceHost, servicePort)
}