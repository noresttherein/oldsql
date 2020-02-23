package com.hcore.clientapi.entities

import com.hcore.clientapi.entities.Components._
import com.hcore.clientapi.entities.Model.{HasId, Id}

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
