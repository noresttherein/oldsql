package com.hcore.clientapi.rest

import akka.actor.ActorRefFactory
import com.hcore.clientapi.rest.items.{ItemMeta, Items}
import com.hcore.ogre.morsels.Time
import play.api.libs.json.Format
import spray.routing.Route


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
				val filterParams = params.filterNot{ case (key, _) => GetParams.contains(key) }
				val items = filterParams.size match {
					case 0 => Items.All[E]
					case 1 =>
						val (name, values) = filterParams.head
						meta.property(name).flatMap(_.items(values)) getOrElse {
							throw new IllegalArgumentException(s"can't filter on property $name=$values")
						}
					case _ => throw new IllegalArgumentException(s"can't filter on multiple properties")
				}
				val result = Time(s"GET $name :$items")(crud.get(items, fetchProperties:_*))
				System.err.println(Thread.currentThread.getName + s" finished $name")
				result
			}}

		}}

	protected def putRoute :Route =
		handleWith[Seq[E], Seq[E]] { items =>
			val saved = crud.create(items)
			items
		}

	val route = pathPrefix(separateOnSlashes(name)) {
		pathEnd {
			get {
				getRoute
			} ~
			put {
				putRoute
			}
		}
	}

/*	val route = pathPrefix(name) {
		pathEnd {
			get {
				parameterMap { params =>
					getRoute
				}
			} ~
			post {
				handleWith[E, E] { entity =>
//					complete {
						System.err.println(s"$name: save $entity")
						entity
//					}
				}
			}
		} ~
		path(LongNumber) { id =>
			pathEnd {
				get {
					complete {
						System.err.print(s"$name: get $id")
						s"$name: get $id\n"
					}
				} ~
				delete {
					complete {
						System.err.print(s"$name: delete $id")
						s"$name: delete $id\n"
					}
				}
			}
		}
	}
*/
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