package com.adpilot.cortb.clientapi.rest

import com.adpilot.cortb.clientapi.prototype.repository.APIRepository
import com.adpilot.cortb.clientapi.prototype.repository.entities
import com.adpilot.cortb.clientapi.prototype.repository.entities._
import com.adpilot.cortb.clientapi.rest.items._


class CRUDs(repository :APIRepository) {
	implicit val Clients = EntityCRUD[ClientItem, Client](repository :repository.type)(repository.Clients)//(CampaignRepresentation)
	implicit val Products = EntityCRUD[ProductItem, entities.Product](repository :repository.type)(repository.Products)
	implicit val Campaigns = EntityCRUD[CampaignItem, Campaign](repository :repository.type)(repository.Campaigns)
	implicit val Orders = EntityCRUD[OrderItem, Order](repository :repository.type)(repository.Orders)
	implicit val Landings = EntityCRUD[LandingItem, Landing](repository :repository.type)(repository.Landings)
	implicit val Creatives = EntityCRUD[CreativeItem, Creative](repository :repository.type)(repository.Creatives)
	implicit val Pixels = EntityCRUD[PixelItem, Pixel](repository :repository.type)(repository.Pixels)
	implicit val TargetingPolicies = EntityCRUD[TargetingPolicyItem, TargetingPolicy](repository :repository.type)(repository.TargetingPolicies)

	val ProductTags = EntityCRUD[TagItem, Tag](repository :repository.type)(repository.ProductTags)
	val CampaignTags = EntityCRUD[TagItem, Tag](repository :repository.type)(repository.CampaignTags)
	val CreativeTags = EntityCRUD[TagItem, Tag](repository :repository.type)(repository.CreativeTags)
	val CreativeTagKeys = EntityCRUD[TagKeyItem, TagKey](repository :repository.type)(repository.CreativeTagKeys)

	implicit val Statuses = EntityCRUD[StatusItem, StatusType](repository :repository.type)(repository.Statuses)

}
