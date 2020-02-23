package com.hcore.clientapi.rest

import akka.actor.ActorRefFactory
import com.hcore.clientapi.entities.Creative
import com.hcore.clientapi.repository.APIRepository
import com.hcore.clientapi.rest.items.CreativeItem
import com.hcore.clientapi.rest.mappings.ItemRepresentation
import spray.http.{ContentType, MediaTypes}
import spray.httpx.marshalling.BasicMarshallers


abstract class CreativeImageService(repository :APIRepository, val name :String) extends SprayService {

	private val meta = implicitly[ItemRepresentation[CreativeItem, Creative]]

	import SprayService._

	val route =
		pathPrefix(separateOnSlashes(name)) {
			val byUniqueProperty = meta.unique.map{ prop =>
				pathPrefix(prop.name / RestPath) { value =>
					System.err.println(s"requested creative ${prop.name}=$value")
					val maybeCreative = prop.item(value.toString).map(meta.maybe).map(ref =>
						repository.inSession { implicit s =>
							repository.Creatives(ref)
						})
					maybeCreative match {
						case Some(Some(creative)) =>
							val mediaType = Option(creative.file.format).map(_.name).flatMap(MediaTypes.forExtension) getOrElse MediaTypes.`application/octet-stream`
							respondWithMediaType(mediaType) {
								_.complete(creative.file.data)(BasicMarshallers.byteArrayMarshaller(ContentType(mediaType)))
							}
						case Some(None) =>
							System.err.println(s"Couldn't find image ${prop.name}=$value")
							NotFound //No such image in the db
						case None =>
							System.err.println(s"No such creative property: $prop")
							NotFound //No such property

					}
				}
			}
			if (byUniqueProperty.isEmpty) {
				System.err.println(s"No unique properties for creatives! other properties are :${meta.properties}")
				NotFound //No unique properties for creative, shouldn't happen
			}else
				byUniqueProperty.reduce(_ ~ _)
		}

}


object CreativeImageService {

	def apply(repository :APIRepository, name :String)(implicit akkaContext :ActorRefFactory) :CreativeImageService =
		new CreativeImageService(repository, name) {
			override def actorRefFactory: ActorRefFactory = akkaContext
		}

	def apply(akkaContext :ActorRefFactory, repository :APIRepository, name :String) :CreativeImageService =
		apply(repository, name)(akkaContext)
}
