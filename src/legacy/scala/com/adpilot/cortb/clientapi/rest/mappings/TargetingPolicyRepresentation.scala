package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities.Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.TargetingPolicy
import com.adpilot.cortb.clientapi.rest.items.TargetingPolicyItem
import com.adpilot.cortb.clientapi.rest.items.TargetingPolicyItem.{Geography, Topics}

//implicits
import com.adpilot.cortb.clientapi.util.OptionOps._

object TargetingPolicyRepresentation extends EntityRepresentation[TargetingPolicyItem, TargetingPolicy] {

	override def entity(item: TargetingPolicyItem): TargetingPolicy = {
		import item._
		TargetingPolicy(
			id, networks.map(_.map(Network)), hours.map(_.map(Hour.fromString(_))),
			geography.flatMap(_.geos.map(_.map(Geo))), geography.flatMap(_.countries.map(_.map(Country))),
			geography.flatMap(_.languages.map(_.map(Language))), geography.flatMap(_.acceptUnknownLanguages),
			capDaily, capLifetime, urlPattern, requiredTagsPattern, forbiddenTagsPattern,
			topics.flatMap(_.adx), topics.flatMap(_.rmx), topics.flatMap(_.openX),
			sections, folds, requiredURLPattern, forbiddenURLPattern,
			whiteSellerRMX, blackSellerRMX,
			whiteIPRanges.map(_.map(IPRange)), blackIPRanges.map(_.map(IPRange)),
			bucket.map(Bucket), sspBuckets.map(_.map(BucketSSP))
		)
	}

	override def mapEntity(entity: TargetingPolicy): TargetingPolicyItem = {
		import entity._
		val geo = Geography(geos.map(_.map(_.location)), countries.map(_.map(_.code)), languages.map(_.map(_.code)), acceptUnknownLanguages)

		TargetingPolicyItem(
			id, networks.map(_.map(_.name)),
			hours.map(_.map(Hour.format(_))),
			geo.providing(geo.geos.nonEmpty || geo.countries.nonEmpty || geo.languages.nonEmpty || geo.acceptUnknownLanguages.nonEmpty),
			capDaily, capLifetime, urlPattern, requiredTagsPattern, forbiddenTagsPattern,
			Topics(topicsADX, topicsRMX, topicsOpenX).providing(t => t.adx.nonEmpty || t.rmx.nonEmpty || t.openX.nonEmpty),
			sections, folds, requiredURLPattern, forbiddenURLPattern,
			whiteSellerRMX, blackSellerRMX, whiteIPRanges.map(_.map(_.range)), blackIPRanges.map(_.map(_.range)),
			bucket.map(_.name), sspBuckets.map(_.map(_.buckets))
		)
	}




	lazy val unique = Seq(prop(_.id, _.id))
	lazy val references = Seq()
	lazy val referable = unique
	lazy val properties = referable
}
