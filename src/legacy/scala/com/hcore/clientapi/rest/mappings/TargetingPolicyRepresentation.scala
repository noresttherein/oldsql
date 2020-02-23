package com.hcore.clientapi.rest.mappings

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.TargetingPolicy
import com.hcore.clientapi.rest.items.TargetingPolicyItem
import com.hcore.clientapi.rest.items.TargetingPolicyItem.{Geography, Topics}


object TargetingPolicyRepresentation extends EntityRepresentation[TargetingPolicyItem, TargetingPolicy] {

	override def entity(item: TargetingPolicyItem): TargetingPolicy = {
		import item._
		TargetingPolicy(
			id, networks.toSeq.flatten.map(Network), hours.toSeq.flatten.map(Hour),
			geography.toSeq.flatMap(_.geos.map(Geo)), geography.toSeq.flatMap(_.countries.map(Country)),
			geography.toSeq.flatMap(_.languages.map(Language)), geography.flatMap(_.acceptUnknownLanguages),
			capDaily, capLifetime, urlPattern, requiredTagsPattern, forbiddenTagsPattern,
			topics.toSeq.flatMap(_.adx), topics.toSeq.flatMap(_.rmx), topics.toSeq.flatMap(_.openX),
			sections.toSeq.flatten, folds.toSeq.flatten, requiredURLPattern, forbiddenURLPattern,
			whiteSellerRMX.toSeq.flatten, blackSellerRMX.toSeq.flatten,
			whiteIPRanges.toSeq.flatten.map(IPRange), blackIPRanges.toSeq.flatten.map(IPRange),
			bucket.map(Bucket), sspBuckets.toSeq.flatten.map(BucketSSP)
		)
	}

	override def mapEntity(entity: TargetingPolicy): TargetingPolicyItem = {
		import entity._
		TargetingPolicyItem(
			id, Some(networks.map(_.name)), Some(hours.map(_.hour)),
			Some(Geography(geos.map(_.location), countries.map(_.code), languages.map(_.code), acceptUnknownLanguages)),
			capDaily, capLifetime, urlPattern, requiredTagsPattern, forbiddenTagsPattern,
			Some(Topics(topicsADX, topicsRMX, topicsOpenX)), Some(sections), Some(folds), requiredURLPattern, forbiddenURLPattern,
			Some(whiteSellerRMX), Some(blackSellerRMX), Some(whiteIPRanges.map(_.range)), Some(blackIPRanges.map(_.range)),
			bucket.map(_.name), Some(sspBuckets.map(_.buckets))
		)
	}




	lazy val unique = Seq(prop(_.id, _.id))
	lazy val references = Seq()
	lazy val referable = unique
	lazy val properties = referable
}
