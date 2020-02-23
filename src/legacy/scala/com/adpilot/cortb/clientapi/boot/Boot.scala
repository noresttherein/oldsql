package com.adpilot.cortb.clientapi.boot

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import akka.routing.FromConfig
import com.adpilot.cortb.clientapi.Configuration
import com.adpilot.cortb.clientapi.prototype.repository.DBConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id
import com.adpilot.cortb.clientapi.prototype.repository.plain.PlainAPIRepository
import com.adpilot.cortb.clientapi.rest.{TestActor, APIServiceActor}

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