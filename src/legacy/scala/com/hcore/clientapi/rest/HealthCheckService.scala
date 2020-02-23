package com.hcore.clientapi.rest

import akka.actor.ActorRefFactory
import com.hcore.clientapi.rest.HealthCheckService.Whisky
import play.api.libs.json._


abstract class HealthCheckService(val name :String) extends SprayService {
	import Whisky._
	val route =
		post {
			handleWith[Whisky, Whisky] { input =>
				System.err.println(s"got $input")
				Whisky("Mortlach Flora & Fauna", new Age(16))
			}
		}
}


object HealthCheckService {
	def apply(path :String)(implicit akkaContext :ActorRefFactory) :HealthCheckService =
		apply(akkaContext, path)

	def apply(akkaContext :ActorRefFactory, path :String) :HealthCheckService =
		new HealthCheckService(path) {
			override implicit def actorRefFactory: ActorRefFactory = akkaContext
		}


	case class Whisky(name :String, age :Whisky.Age)
	object Whisky {
		implicit val format :Format[Whisky] = Json.format[Whisky]

		class Age(val years :Int)
		object Age {
			implicit val format :Format[Age] = new Format[Age] {
				override def reads(json: JsValue): JsResult[Age] = JsError()

				override def writes(o: Age): JsValue = JsNumber(BigDecimal(o.years))
			}
		}
	}

}
