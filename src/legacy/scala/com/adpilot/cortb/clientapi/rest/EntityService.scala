package com.adpilot.cortb.clientapi.rest

import akka.actor.ActorRefFactory
import com.adpilot.cortb.clientapi.rest.items.{Items, ItemMeta}
import com.adpilot.cortb.clientapi.util.Time
import play.api.libs.json.Format
import spray.http.StatusCodes
import spray.httpx.marshalling.{Marshaller, ToResponseMarshaller, ToResponseMarshallable}
import spray.httpx.unmarshalling.{Unmarshaller, FromRequestUnmarshaller}
import spray.routing.{Route, RequestContext}


abstract class EntityService[E :Format :ItemMeta :CRUD](val name :String) extends SprayService {
	val meta = ItemMeta[E]
	val crud = CRUD[E]

	import EntityService._
	import JsonFormats._

	protected def getRoute :Route =
		parameterMultiMap { params => {
			complete { Time(s"processing GET $name request") {
				System.err.println(Thread.currentThread.getName + s" handling $name")
				val fetchParam = params.getOrElse(FetchParam, Seq()).flatMap(_.split(",").toSeq.map(_.trim))
				val fetchProperties = fetchParam.map(p => meta(p))
				val items: Items[E] = filteredItems(params)
				val result = Time(s"GET $name :$items")(crud.get(items, fetchProperties:_*))
				System.err.println(Thread.currentThread.getName + s" finished $name")
				result
			}}

		}}

	protected def filteredItems(params: Map[String, List[String]]): Items[E] = {
		val filterParams = params.filterNot { case (key, _) => GetParams.contains(key) }
		val items = filterParams.size match {
			case 0 => Items.All[E]
			case 1 =>
				val (name, values) = filterParams.head
				meta.property(name).flatMap(_.items(values)) getOrElse {
					throw new scala.IllegalArgumentException(s"can't filter on property $name=$values")
				}
			case _ => throw new scala.IllegalArgumentException(s"can't filter on multiple properties")
		}
		items
	}

	protected def putRoute :Route =
		handleWith[Seq[E], Seq[E]] { items =>
			val saved = crud.create(items)
			saved
		}

	protected def postRoute :Route =
		handleWith[Seq[E], Seq[E]] { items =>
			val saved = crud.update(items)
			saved
		}

	protected def deleteRoute :Route = parameterMultiMap { params =>
		complete {
			System.err.println(s"accepted DELETE $params")
			val items = filteredItems(params)
			System.err.println(s"deleting $items")
			crud.delete(items)
			StatusCodes.NoContent
		}
	}

	val route = pathPrefix(separateOnSlashes(name)) {
		pathEnd {
			get {
				getRoute
			} ~
			put {
				putRoute
			} ~
			post {
				postRoute
			} ~
			delete {
				deleteRoute
			}
		}
	}

}


object EntityService {
	val FetchParam = "fetch"
	val GetParams = Seq(FetchParam)
//	def actor[E :ToResponseMarshaller](name :String) :EntityService[E] =
//		new EntityService[E](name) with SprayServiceActor

	def apply[E](name :String)(implicit akkaContext :ActorRefFactory, meta :ItemMeta[E], crud :CRUD[E], format :Format[E]) :EntityService[E] =
		new EntityService[E](name) {
			def actorRefFactory: ActorRefFactory = akkaContext
		}

	def apply[E](name :String, crud :CRUD[E])(implicit akkaContext :ActorRefFactory, meta :ItemMeta[E], format :Format[E]) :EntityService[E] =
		apply(name)(akkaContext, meta, crud, format)

}