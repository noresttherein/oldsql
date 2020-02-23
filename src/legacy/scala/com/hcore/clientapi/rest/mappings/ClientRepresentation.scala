package com.hcore.clientapi.rest.mappings

import com.hcore.clientapi.entities.Client
import com.hcore.clientapi.rest.items.ClientItem


object ClientRepresentation extends EntityRepresentation[ClientItem, Client] {
	override def entity(item: ClientItem): Client =
		throw new UnsupportedOperationException(s"Clients in input are not supported. received $item")

	override def mapEntity(entity: Client): ClientItem = ClientItem(
		entity.id, Option(entity.hash), entity.name, entity.email, entity.phone
	)

	import Implicits._

	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.name, _.name))

	val references = Seq()
	lazy val referable = prop(_.email, _.email) +: prop(_.phone, _.phone) +: unique

	def properties = referable
}
