package com.hcore.clientapi.repository

import com.hcore.clientapi.entities.Model.{HasId, Id}
import com.hcore.clientapi.entities._
import com.hcore.ogre.model.Reference
import com.hcore.ogre.repository.MappedSchemaRepository

import scala.collection.generic.CanBuildFrom
import scala.reflect.runtime.universe.TypeTag
import scala.slick.jdbc.{StaticQuery => Q}


class PlainAPIRepository(val schema :Schema, blockedClients :Seq[Id]=Seq()) extends APIRepository with MappedSchemaRepository
{ repository =>
	def this(config :DBConfig, schemaName :Option[String], blockedClients :Seq[Id]) = this(new Schema(config, schemaName), blockedClients)
	def this(config :DBConfig, blockedClients :Seq[Id]) = this(new Schema(config), blockedClients)

	import schema._


	implicit val Clients = new TableStore[Client](schema.Clients, IdProperty[Client]((c, id) => c.copy(id=id)))()

	implicit val Products :Store[Product] = new TableStore[Product](schema.Products, IdProperty[Product]((e, id) => e.copy(id=id)))(
		new ForeignKey[Product, Client](_.client, (o, fk) => o.copy(client=fk))(Products, Clients)
	)


	implicit val Orders :Store[Order] = new TableStore[Order](schema.Orders, IdProperty[Order]((e, id) => e.copy(id=id)),
		new ForeignKeyCascade[Order, TargetingPolicy](_.targetingPolicy, (c, p) => c.copy(targetingPolicy = p))(Orders, TargetingPolicies)
	)(
		new ForeignKey[Order, Campaign](_.campaign, (o, fk) => o.copy(campaign=fk))(Orders, Campaigns)
	)

	implicit val Pixels = new TableStore[Pixel](schema.Pixels, IdProperty[Pixel]((e, id) => e.copy(id=id)))()

	val Landings :Store[Landing] = new TableStore[Landing](schema.Landings, IdProperty[Landing]((e, id)=>e.copy(id=id)))(
		new ForeignKey[Landing, Campaign](_.campaign, (o, fk) => o.copy(campaign=fk))(Landings, Campaigns)
	)

	implicit val TargetingPolicies = new TableStore[TargetingPolicy](schema.TargetingPolicies, IdProperty[TargetingPolicy]((e, id) => e.copy(id=id)))()

	implicit val Creatives :Store[Creative] = new TableStore[Creative](schema.Creatives, IdProperty[Creative]((e, id) => e.copy(id=id)),
		new JoinTableCascadeLeft[Creative, Campaign, Set[Campaign]](_.campaigns, (cr, cmp) => cr.copy(campaigns = cmp), CampaignCreatives)(Creatives, Campaigns)
	)(
		new ForeignKey[Creative, Product](_.product, (o, fk) => o.copy(product=fk))(Creatives, Products)
	)

	implicit val Campaigns :Store[Campaign] = new TableStore[Campaign](schema.Campaigns, IdProperty[Campaign]((e, id) => e.copy(id=id)),
		new ForeignKeyCascade[Campaign, TargetingPolicy](_.targetingPolicy, (c, p) => c.copy(targetingPolicy = p))(Campaigns, TargetingPolicies),

		new ReverseForeignKeyCascade[Campaign, Order, Set[Order]](_.orders, (cmp, o) => cmp.copy(orders=o), _.campaign, (o, c)=>o.copy(campaign=c))(Campaigns, Orders),

		new JoinTableCascadeRight[Campaign, Creative, Set[Creative]](_.creatives, (cmp, cr) => cmp.copy(creatives=cr), CampaignCreatives)(Campaigns, Creatives),

		new ReverseForeignKeyCascade[Campaign, Landing, Set[Landing]](_.landings, (cmp, l) => cmp.copy(landings=l), _.campaign, (l, c) => l.copy(campaign=c))(Campaigns, Landings),

		new ReverseForeignKeyCascade[Campaign, Order, Set[Order]](_.orders, (cmp, o) => cmp.copy(orders=o), _.campaign, (o, c) => o.copy(campaign=c))(Campaigns, Orders)
	)(
		new ForeignKey[Campaign, Product](_.product, (o, fk) => o.copy(product=fk), blockedClients)(Campaigns, Products)
	)

	val ProductTags = new TableStore[Tag](schema.ProductTags, IdProperty[Tag]((e, id) => e.copy(id=id)))()

	val CampaignTags = new TableStore[Tag](schema.CampaignTags, IdProperty[Tag]((e, id) => e.copy(id=id)))()

	val CreativeTags = new TableStore[Tag](schema.CreativeTags, IdProperty[Tag]((t, id) => t.copy(id=id)))()



	protected class JoinTableCascadeRight[O<:HasId, M<:HasId, C<:Iterable[M]](
		get :O=>Reference[C], set :(O, Reference[C])=>O, jt :JoinTable[O, M])(owner : =>Store[O], many : =>Store[M])(
		implicit cbf :CanBuildFrom[_, M, C], TypeTag :TypeTag[O]
	)
		extends AbstractJoinTableCascade[O, M, C, JoinTable[O, M]](get, set, jt)(owner, many)
	{
		override def ownerFK = table.left

		override def manyFK = table.right

		override def insert(owner: Reference[O], many: Reference[M]) = table.insertStmt(new JoinRow(None, owner, many))
	}

	protected class JoinTableCascadeLeft[O<:HasId, M<:HasId, C<:Iterable[M]](
		get :O=>Reference[C], set :(O, Reference[C])=>O, jt :JoinTable[M, O])(owner : =>Store[O], many : =>Store[M])(
		implicit cbf :CanBuildFrom[_, M, C], TypeTag :TypeTag[O]
    ) extends AbstractJoinTableCascade[O, M, C, JoinTable[M, O]](get, set, jt)(owner, many)
	{
		override def ownerFK = table.right

		override def manyFK = table.left

		override def insert(owner: Reference[O], many: Reference[M]) = table.insertStmt(new JoinRow(None, many, owner))
	}


}




