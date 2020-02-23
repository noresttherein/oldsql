package com.hcore.clientapi.rest.mappings

import java.util.Date

import com.hcore.clientapi.entities.Components.Country
import com.hcore.clientapi.entities.Product
import com.hcore.clientapi.rest.items.ProductItem


object ProductRepresentation extends EntityRepresentation[ProductItem, Product] {
	def StartTime = new Date()
	def EndTime = new Date()
	def AddedAt = new Date()



	override def entity(item: ProductItem): Product = Product(
		item.id, item.hash.orNull, item.client, item.name, item.description,
		AddedAt, None, Product.Status(item.status), Product.Type(item.productType), item.country.map(Country),
		item.campaigns
	)

	override def mapEntity(entity: Product): ProductItem = ProductItem(
		entity.id, Option(entity.hash), entity.name, entity.description,
		entity.status.name, entity.ptype.name, entity.country.map(_.code), entity.campaigns, entity.client
	)

	import Implicits._
	import Product._
	implicit val status = Status.apply _
	implicit val unstatus = (_:Status).name
	implicit val ptype = Type.apply _
	implicit val untype = (_:Type).name

	lazy val unique = Seq(prop(_.id, _.id), map(_.hash, _.hash), prop(_.name, _.name))
	lazy val client = one(_.client, _.client)
	lazy val references = Seq(client, set(_.campaigns, _.campaigns))
	lazy val referable = map(_.status, _.status) +: map(_.ptype, _.productType, "type") +: client +: unique

	lazy val properties = noDups(references ++: referable)
}
