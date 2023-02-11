package com.adpilot.cortb.clientapi.prototype.repository.plain

import java.net.URL
import java.sql.Timestamp
import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities
import com.adpilot.cortb.clientapi.prototype.repository.entities.Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, Id, HasId}
import com.adpilot.cortb.clientapi.prototype.repository.entities.Order.BiddingConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities._
import com.adpilot.cortb.clientapi.prototype.repository.DBConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{PropertyReference, One}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ComponentMapping.EmbeddedMappingComponent
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.EntityMapping.ForeignKeyType
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping.{SetParameters, AbstractMapping}
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.MappingResult.ComponentResult
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping._
import TypedColumn.ColumnOption
import ColumnType.MappedType


import com.adpilot.cortb.clientapi.util.{Time, OptionOps}



import OptionOps._
import ColumnOption._
import TypedColumn._

import scala.reflect.runtime.universe.TypeTag
import scala.slick.jdbc.{GetResult, SetParameter}
import scala.util.Try

class Schema(config :DBConfig, name :Option[String]=Schema.DefaultSchemaName) extends SchemaMapping(config, name) with ColumnTypes {
	schema =>


//	import ForeignKeyType._


	abstract class IdTable[E<:HasId](idColumnName: String, idOptions: Seq[ColumnOption[Option[Id]]])(implicit val typeTag :TypeTag[E]) extends Table[E, Option[Id]] with MappingSupport[E] {

		def this()(implicit typeTag :TypeTag[E]) = this("id", Seq(AutoGen))

		def schemaName = Schema.this.name

		val id = column(idColumnName, _.id, idOptions:_*) //autoins("id", _.id)
//		val id = column(_.id)("id", ColumnFlagType.create(AutoGen))
		val PK = id

//		protected def foreignKey[T](name :String, pick :E=>One[T], options :ColumnOption[One[T]]*)(implicit tpe :ColumnType[One[T]]) :Column[One[T]] =
//			column(name, pick, options:_*)

//		protected def foreignKey[T<:HasId :TypeTag](name :String, pick :E=>One[T], target : =>IdTable[T], options :ColumnOption[One[T]]*) :Column[One[T]] =
//			column(name, pick, options:_*)(ForeignKeyType(target))

		protected def foreignKey[FK, T<:HasId](name :String, pick :E=>FK, options :ColumnOption[FK]*)(table : =>IdTable[T])(implicit builder :ForeignKeyBuilder[FK, T]) :Column[FK] =
			builder(name, pick, ()=>table, options)

//		protected class ForeignKeyBuilder[FK](name :String, pick :E=>FK, options :Seq[ColumnOption[FK]]) {
//			def apply[T](target : =>IdTable[T])(implicit tag :TypeTag[T], ev : (E=>FK)=:=(E=>One[T])) :Column[One[T]] =
//				column(name, pick.asInstanceOf[E=>One[T]], options.asInstanceOf[Seq[ColumnOption[One[T]]]]:_*)(ForeignKeyType(target))
//
//			def apply[T](target : =>IdTable[T])(implicit tag :TypeTag[T], ev : FK=:=Option[One[T]]) :Column[Option[One[T]]] =
//				column(name, pick.asInstanceOf[E=>Option[One[T]]], options.asInstanceOf[Seq[ColumnOption[Option[One[T]]]]]:_*)(
//					NullableOptionType(ForeignKeyType(target))
//				)
//
//
//		}

		protected trait ForeignKeyBuilder[FK, T<:HasId] {
			def apply(name :String, pick :E=>FK, target :()=>IdTable[T], options :Seq[ColumnOption[FK]]) :Column[FK]
		}
		protected implicit def idFK[T<:HasId] :ForeignKeyBuilder[Id, T] = new ForeignKeyBuilder[Id, T] {
			override def apply(name: String, pick: (E) => Id, target: () => IdTable[T], options: Seq[ColumnOption[Id]]): Column[Id] =
				column(name, pick, options:_*)
		}
		protected implicit def oneReferenceFK[T<:HasId :TypeTag] :ForeignKeyBuilder[One[T], T] = new ForeignKeyBuilder[One[T], T] {
			override def apply(name: String, pick: (E) => One[T], target: () => IdTable[T], options: Seq[ColumnOption[One[T]]]): Column[One[T]] =
				column(name, pick, options:_*)(ForeignKeyType(target()))
		}
		protected implicit def optionOneRefFK[T<:HasId :TypeTag]: ForeignKeyBuilder[Option[One[T]], T]  = new ForeignKeyBuilder[Option[One[T]], T] {
			override def apply(name: String, pick: (E) => Option[One[T]], target: () => IdTable[T], options: Seq[ColumnOption[Option[One[T]]]]): Column[Option[One[T]]] = {
//				implicit val fk :MappedType[One[T], Option[Id]] = ForeignKeyType(target())
				implicit val fk :MappedType[One[T], Id] = ForeignKeyType(target(), Some(_:Id),
					(pk:Option[Id]) => pk.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${target()} to a foreign key")))
//				implicit val fk = ForeignKeyType(target(), Some(_:Id), (pk:Option[Id]) => pk.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${target()} to a foreign key")))
				column(name, pick, options: _*)(ColumnType[Option[One[T]]])
			}
		}
	}



	case class JoinRow[L<:HasId, R<:HasId](id :Option[Id], left :One[L], right :One[R]) extends HasId

	class JoinTable[L <:HasId, R<:HasId](val tableName :String, leftKey :String, rightKey :String)(
	        implicit leftTable :Table[L, Option[Id]], rightTable :Table[R, Option[Id]], leftType :TypeTag[L], rightClass :TypeTag[R]
		) extends IdTable[JoinRow[L, R]] with MappingSupport[JoinRow[L, R]]
	{

		implicit val left = column(leftKey, _.left, NoUpdate)
		implicit val right = column(rightKey, _.right, NoUpdate)

		def side[T](implicit col :Column[One[T]]) = col

		override protected def assemble(implicit res: ResultRow): JoinRow[L, R] = JoinRow(id, left, right)
	}


	object common {
		type HasHash = { def hash :String }
		type HasName = { def name :String }
		type HasLifecycle = { def addedAt :Date; def deletedAt :Option[Date] }
		type TypicalEntity = HasId with HasHash with HasName with HasLifecycle
		
		
		val hash = TypedColumn[String]("hash", AutoGen)
		val name = TypedColumn[String]("name")
		val addedAt = TypedColumn[Date]("added_at") //todo :Default option
		val deletedAt = TypedColumn[Option[Date]]("deleted_at")
	}


	
	abstract class EntityTable[E <: common.TypicalEntity :TypeTag](val tableName :String)
		extends IdTable[E]
	{
		val hash = column(common.hash)(_.hash)
		val name = column(common.name)(_.name)
		val addedAt = column(common.addedAt)(_.addedAt)
		val deletedAt = column(common.deletedAt)(_.deletedAt)


	}

	implicit object Clients extends EntityTable[Client]("clients") {

		trait Addresses extends MappingSupport[Client.Address] {
			val companyName = column("company_name", _.companyName, ReadOnly)
//			val companyName = column(_.companyName)
			val street1 = column("street_1", _.street1, ReadOnly)
			val street2 = column("street_2", _.street2, ReadOnly)
			val city = column("city", _.city, ReadOnly)
			val postalCode = column("postal_code", _.postalCode, ReadOnly)
			val country = column("country", _.country, ReadOnly)

			override protected def assemble(implicit res: ResultRow): Client.Address =
				Client.Address(companyName, street1, street2, city, postalCode, country)

		}

		val email = column("email", _.email, ReadOnly)
		val phone = column("phone", _.phone, ReadOnly)
		val contactPerson = column("contact_person", _.contactPerson, ReadOnly)
		val password = column("password", _.password, ReadOnly)


		val invoiceInfo = new ComponentSupport[Client.InvoiceInfo](_.invoiceInfo, "invoice_") {
			val vatNumber = column("vat_number", _.vatNumber)
			val defaultCurrency = column("default_currency", _.defaultCurrency)
			val defaultDueDays = column("default_due_days", _.defaultDueDays)
			val autoInvoicing = column("autoinvoicing", _.autoInvoicing)
			val comments = column("comments", _.comments)
			val email = column("email", _.email)

			val address = new ComponentSupport(_.address) with Addresses

			override protected def assemble(implicit res: ResultRow): Client.InvoiceInfo =
				Client.InvoiceInfo(vatNumber, address, defaultCurrency, defaultDueDays, autoInvoicing, comments, email)

		}

		val mailingAddress = new ComponentSupport(_.mailingAddress, "mailing_") with Addresses
		val mailingInvoiceEmail = column("mailing_email_for_invoices", _.mailingInvoiceEmail)
		val status = column("status", _.status)


		override protected def assemble(implicit res: ResultRow): Client = { //val res2 :ComponentResult[this.type] = ???
			(res \ (_.invoiceInfo) \ (_.address) \ (_.companyName)).get :Option[String]
			Client(id, hash, name, password, email, phone, contactPerson, invoiceInfo, mailingAddress, mailingInvoiceEmail, status, addedAt, deletedAt)
		}
	}


	implicit object Products extends EntityTable[Product]("products") {
		val client = column("client_id", _.client)
		val description = column("description", _.description)
		
		val status = column("status", _.status)
		val ptype = column("type", _.ptype)

		val country = column("country", _.country)

		protected def assemble(implicit res: ResultRow): Product =
			Product(id, hash, client, name, description, addedAt, deletedAt, status, ptype, country)
	}



	type OrderLike = common.TypicalEntity {
		def targetingPolicy :One[TargetingPolicy]
		def budgetLimit :Double
		def budgetDailyLimit :Double
		def startTime :Date
		def endTime :Date
		
		def impressionLimit :Option[Long]
		def impressionDailyLimit :Option[Long]
		def clickLimit :Option[Long]
		def clickDailyLimit :Option[Long]
	}


	implicit val TargetingPolicies = new IdTable[TargetingPolicy] {
		val tableName = "targeting_policy"

		val networks = column("networks", _.networks)
		val hours = column("hours", _.hours)
		val geos = column("geo", _.geos)
		val countries = column("countries", _.countries)
		val languages = column("languages", _.languages)
		val acceptUnknownLanguages = column("accept_unknown_languages", _.acceptUnknownLanguages)
		//todo: default value needed for null values
		val capDaily = column("cap_daily", _.capDaily)
		val capLifetime = column("cap_lifetime", _.capLifetime)
		val urlPattern = column("url_pattern", _.urlPattern)
		val requiredTagsPattern = column("required_tags_pattern", _.requiredTagsPattern)
		val forbiddenTagsPattern = column("forbidden_tags_pattern", _.forbiddenTagsPattern)
		val topicsADX = column("topics_adx", _.topicsADX)
		val topicsRMX = column("topics_rmx", _.topicsRMX)
		val topicsOpenX = column("topics_openx", _.topicsOpenX)
		val sections = column("sections", _.sections)
		val folds = column("folds", _.folds)
		val requiredURLPattern = column("required_url_pattern", _.requiredURLPattern)
		val forbiddenURLPattern = column("forbidden_url_pattern", _.forbiddenURLPattern)
		val blackSellerRMX = column("black_seller_rmx", _.blackSellerRMX)
		val whiteSellerRMX = column("white_seller_rmx", _.whiteSellerRMX)
		val ipRangesWhite = column("ip_ranges_white", _.whiteIPRanges)
		val ipRangesBlack = column("ip_ranges_black", _.blackIPRanges)
		val bucket = column("bucket", _.bucket)
		val sspBuckets = column("bucket_ssp", _.sspBuckets)


		protected def assemble(implicit res: ResultRow): TargetingPolicy = TargetingPolicy(
			id, networks, hours, geos, countries, languages, acceptUnknownLanguages,
			capDaily, capLifetime, urlPattern, requiredTagsPattern, forbiddenTagsPattern,
			topicsADX, topicsRMX, topicsOpenX, sections, folds, requiredURLPattern, forbiddenURLPattern,
			whiteSellerRMX, blackSellerRMX, ipRangesWhite, ipRangesBlack, bucket, sspBuckets
		)

	}




	abstract class OrderLikeTable[E <:OrderLike :TypeTag](tableName :String) extends EntityTable[E](tableName) {
		val targetingPolicy = column("targeting_policy_id", _.targetingPolicy)
		val budgetLimit = column("budget_limit", _.budgetLimit)
		val budgetDailyLimit = column("budget_daily_limit", _.budgetDailyLimit)
		val startTime = column("start_time", _.startTime)
		val endTime = column("end_time", _.endTime)

		val impressionLimit = column("impression_limit", _.impressionLimit)
		val impressionDailyLimit = column("impression_daily_limit", _.impressionDailyLimit)
		val clickLimit = column("click_limit", _.clickLimit)
		val clickDailyLimit = column("click_daily_limit", _.clickDailyLimit)

	}
	
	implicit object Campaigns extends OrderLikeTable[Campaign]("campaigns") {
		val product = column("product_id", _.product)

		val status = column("status", _.status)
		val ctype = column("type", _.ctype)
		val thirdPartyImpressionPixelUrl = column("third_party_impression_pixel_url", _.thirdPartyImpressionPixelUrl)

		
		protected def assemble(implicit res: ResultRow): Campaign =
			Campaign(id, hash, name, product, targetingPolicy, budgetLimit, budgetDailyLimit, startTime, endTime,
			         addedAt, deletedAt, status, ctype, impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit,
          		 thirdPartyImpressionPixelUrl = thirdPartyImpressionPixelUrl
      )
	}
	
	
	implicit object Orders extends OrderLikeTable[Order]("orders") {

		val campaign = column("campaign_id", _.campaign)

		val status = column("status", _.status)
		val otype = column("type", _.otype)
		val bidLimit = column("bid_limit", _.bidLimit)
		val Bidding = new MappingSupport[BiddingConfig] with Component[BiddingConfig] {
			val strategy = column("std_bidding_strategy", _.strategy)
			val cpmValue = column("std_bidding_cpm_val", _.cpmValue)
			val cpcValue = column("std_bidding_cpc_val", _.cpcValue)
			val cpaPixel0 = column("std_bidding_cpa_pixel_id_0", _.cpaPixel0)
			val cpaPixel1 = column("std_bidding_cpa_pixel_id_1", _.cpaPixel1)
			val cpaPixel2 = column("std_bidding_cpa_pixel_id_2", _.cpaPixel2)
			val cpaValue0 = column("std_bidding_cpa_val_0", _.cpaValue0)
			val cpaValue1 = column("std_bidding_cpa_val_1", _.cpaValue1)
			val cpaValue2 = column("std_bidding_cpa_val_2", _.cpaValue2)

			def value(entity: Order): BiddingConfig = entity.bidding

			def assemble(implicit res: ResultRow): BiddingConfig =
				BiddingConfig(strategy, cpmValue, cpcValue, cpaPixel0, cpaPixel1, cpaPixel2,
					cpaValue0, cpaValue1, cpaValue2)

		}

		val trackingCPAPixel0 = column("std_tracking_cpa_pixel_id_0", _.trackingCPAPixel0)
		val trackingCPAPixel1 = column("std_tracking_cpa_pixel_id_1", _.trackingCPAPixel1)
		val trackingCPAPixel2 = column("std_tracking_cpa_pixel_id_2", _.trackingCPAPixel2)

		val dontIncludeInLearning = column("do_not_include_in_learning", _.dontIncludeInLearning)


		protected def assemble(implicit res: ResultRow): Order = Order(
			id, hash, name, campaign, targetingPolicy, budgetLimit, budgetDailyLimit, startTime, endTime,
			addedAt, deletedAt, status, otype, Bidding,
			trackingCPAPixel0, trackingCPAPixel1, trackingCPAPixel2, dontIncludeInLearning,
			impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit, bidLimit
		)


	}

	implicit object Pixels extends EntityTable[Pixel]("pixels") {
		val pixelType = column("type", _.pixelType)
		val status = column("status", _.status)

		val previous = foreignKey("prev_pixel_id", _.previous)(this)
		val product = column("product_id", _.product)

		override protected def assemble(implicit res: ResultRow): Pixel = Time(s"assembling pixel #${res(id)}"){

			Pixel(id, hash, name, pixelType, product, previous, addedAt, deletedAt, status)
		}
	}



	implicit object Landings extends IdTable[Landing] {
		val tableName = "landings"

		val hash = column(common.hash)(_.hash)
		val url = column("url", _.url)
		val campaign = column("campaign_id", _.campaign)
		val targetURL = column("target_url", _.targetURL)
		val status = column("status", _.status)
		val addedAt = column(common.addedAt)(_.addedAt)
		val deletedAt = column(common.deletedAt)(_.deletedAt)

		protected def assemble(implicit res: ResultRow): Landing = Landing(
			id, hash, url, campaign, targetURL, addedAt, deletedAt, status
		)

	}

	class Tags(ownerName :String) extends IdTable[com.adpilot.cortb.clientapi.prototype.repository.entities.Tag] {
		val tableName = ownerName + "_tags"

		val owner = column(ownerName+"_id", _.owner)
		val key = column("key", _.key)
		val value = column("value", _.value)


		protected def assemble(implicit res: ResultRow): Tag =
			Tag(id, owner, key, value)

	}
	val ProductTags = new Tags("product")
	val CampaignTags = new Tags("campaign")
	val CreativeTags = new Tags("creative")

	class TagKeys(ownerName :String) extends Table[TagKey, Option[Id]] with MappingSupport[TagKey] {
		def schemaName = Schema.this.name
		val tableName: String = s"dict_${ownerName}_tag_keys"
		val typeTag = implicitly[TypeTag[TagKey]]

		val name = column("name", _.name, ReadOnly)
		val description = column("description", _.description, ReadOnly)

		val PK = new ComponentSupport[Option[Id]](_.id) {
			override protected def assemble(implicit res: ResultRow): Option[Id] = None
		}

		override protected def assemble(implicit res: ResultRow): TagKey =
			TagKey(name, description)
	}
	val CreativeTagKeys = new TagKeys("creative")


	implicit object Creatives extends IdTable[Creative] {
		val tableName = "creatives"
		val hash = column(common.hash)(_.hash)
		val addedAt = column(common.addedAt)(_.addedAt)
		val deletedAt = column(common.deletedAt)(_.deletedAt)
		val format = column("format", _.format)
		val fileName = column("file_name", _.file.name)
		val fileFormat = column("file_format", _.file.format)
		val fileData = column("file_data", _.file.image, OptionalSelect(One()))(optionalSelect[Creative.Image])
		val status = column("status", _.status)
		val ctype = column("type", _.ctype)
		val product = column("product_id", _.product)


		protected def assemble(implicit res: ResultRow): Creative = Creative(
			id, hash, format, new Creative.File(fileName, fileFormat, fileData),
			addedAt, deletedAt, status, ctype, product
		)

	}

	object AppnexusCreativesData extends IdTable[AppnexusCreativeData]("internal_id", Seq[ColumnOption[Option[Id]]](NoUpdate, NoInsert)) {
		override val tableName = "appnexus_creatives_data"
		val creative = foreignKey("internal_id", _.creative)(Creatives)
		val categories = column("categories", _.categories)

		override protected def assemble(implicit res: ResultRow) = AppnexusCreativeData(id, creative, categories)
	}

	implicit val CampaignCreatives = new JoinTable[Campaign, Creative]("campaigns_creatives", "campaign_id", "creative_id")


	implicit object Statuses extends Table[StatusType, Option[Id]] with MappingSupport[StatusType]{
		override protected implicit val typeTag = implicitly[TypeTag[StatusType]]

		val tableName = "dict_status"
		def schemaName = Schema.this.name

		val name = column("name", _.name)
		val description = column("description", _.description)

		val PK = new ComponentSupport[Option[Id]](_.id) {
			override protected def assemble(implicit res: ResultRow): Option[Id] = None
		}


		override protected def assemble(implicit res: ResultRow): StatusType =
			StatusType(name, description)
	}

}


object Schema {
	val DefaultSchemaName = Some("rtb")
}
