package com.hcore.clientapi.rest

import com.hcore.clientapi.entities
import com.hcore.clientapi.entities._
import com.hcore.clientapi.repository.APIRepository
import com.hcore.clientapi.rest.items._


class CRUDs(repository :APIRepository) {
	implicit val Clients = EntityCRUD[ClientItem, Client](repository)(repository.Clients)//(CampaignRepresentation)
	implicit val Products = EntityCRUD[ProductItem, entities.Product](repository)(repository.Products)
	implicit val Campaigns = EntityCRUD[CampaignItem, Campaign](repository)(repository.Campaigns)
	implicit val Orders = EntityCRUD[OrderItem, Order](repository)(repository.Orders)
	implicit val Landings = EntityCRUD[LandingItem, Landing](repository)(repository.Landings)
	implicit val Creatives = EntityCRUD[CreativeItem, Creative](repository)(repository.Creatives)
	implicit val Pixels = EntityCRUD[PixelItem, Pixel](repository)(repository.Pixels)
	implicit val TargetingPolicies = EntityCRUD[TargetingPolicyItem, TargetingPolicy](repository)(repository.TargetingPolicies)

	val ProductTags = EntityCRUD[TagItem, Tag](repository)(repository.ProductTags)
	val CampaignTags = EntityCRUD[TagItem, Tag](repository)(repository.CampaignTags)
	val CreativeTags = EntityCRUD[TagItem, Tag](repository)(repository.CreativeTags)

}
