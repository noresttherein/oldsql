package com.adpilot.cortb.clientapi.prototype.repository.entities

import Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}

case class TargetingPolicy(id :Option[Id],
                           networks :Option[Seq[Network]], //Seq[String]
                           hours :Option[Seq[Hour]],
                           geos :Option[Seq[Geo]],
                           countries :Option[Seq[Country]],
	                       languages :Option[Seq[Language]],
	                       acceptUnknownLanguages :Option[Boolean],
	                       capDaily :Option[Int],
	                       capLifetime :Option[Int],
	                       urlPattern :Option[String],
	                       requiredTagsPattern :Option[String],
	                       forbiddenTagsPattern :Option[String],
	                       topicsADX :Option[Seq[String]],
	                       topicsRMX :Option[Seq[String]],
	                       topicsOpenX :Option[Seq[String]],
	                       sections :Option[Seq[String]],
	                       folds :Option[Seq[String]],
	                       requiredURLPattern :Option[String],
	                       forbiddenURLPattern :Option[String],
	                       whiteSellerRMX :Option[Seq[String]],
	                       blackSellerRMX :Option[Seq[String]],
	                       whiteIPRanges :Option[Seq[IPRange]],
	                       blackIPRanges :Option[Seq[IPRange]],
	                       bucket :Option[Bucket],
	                       sspBuckets :Option[Seq[BucketSSP]]
  ) extends HasId
