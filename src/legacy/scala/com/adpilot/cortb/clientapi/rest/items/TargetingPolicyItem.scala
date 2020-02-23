package com.adpilot.cortb.clientapi.rest.items

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.Id

case class TargetingPolicyItem(
	id :Option[Id],
	networks :Option[Seq[String]],
	hours :Option[Seq[String]],
	geography :Option[TargetingPolicyItem.Geography],
	capDaily :Option[Int],
	capLifetime :Option[Int],
	urlPattern :Option[String],
	requiredTagsPattern :Option[String],
	forbiddenTagsPattern :Option[String],
	topics :Option[TargetingPolicyItem.Topics],
	sections :Option[Seq[String]],
	folds :Option[Seq[String]],
	requiredURLPattern :Option[String],
	forbiddenURLPattern :Option[String],
	whiteSellerRMX :Option[Seq[String]],
	blackSellerRMX :Option[Seq[String]],
    whiteIPRanges :Option[Seq[String]],
	blackIPRanges :Option[Seq[String]],
	bucket :Option[String],
	sspBuckets :Option[Seq[String]]
)

object TargetingPolicyItem {
	case class Topics(adx :Option[Seq[String]], rmx :Option[Seq[String]], openX :Option[Seq[String]])
	case class Geography(geos :Option[Seq[String]], countries :Option[Seq[String]], languages :Option[Seq[String]], acceptUnknownLanguages :Option[Boolean])
}



/*
case class TargetingPolicy(id :Option[Id],
                           networks :Seq[Network], //Seq[String]
                           hours :Seq[Hour],
                           geos :Seq[Geo],
                           countries :Seq[Country],
                           languages :Seq[Language],
                           acceptUnknownLanguages :Option[Boolean],
                           capDaily :Option[Int],
                           capLifetime :Option[Int],
                           urlPattern :Option[String],
                           requiredTagsPattern :Option[String],
                           forbiddenTagsPattern :Option[String],
                           topicsADX :Seq[String],
                           topicsRMX :Seq[String],
                           topicsOpenX :Seq[String],
                           sections :Seq[String],
                           folds :Seq[String],
                           requiredURLPattern :Option[String],
                           forbiddenURLPattern :Option[String],
                           whiteSellerRMX :Seq[String],
                           blackSellerRMX :Seq[String],
                           whiteIPRanges :Seq[IPRange],
                           blackIPRanges :Seq[IPRange],
                           bucket :Option[Bucket],
                           sspBuckets :Seq[BucketSSP]
	                          ) extends HasId
*/
