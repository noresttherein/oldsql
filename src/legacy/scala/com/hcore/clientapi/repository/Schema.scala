package com.hcore.clientapi.repository

import java.util.Date

import com.hcore.clientapi.entities.Model.{HasId, Id}
import com.hcore.clientapi.entities.Order.BiddingConfig
import com.hcore.clientapi.entities._
import com.hcore.ogre.mapping.ColumnType.MappedType
import com.hcore.ogre.mapping.EntityMapping.ForeignKeyType
import com.hcore.ogre.mapping.Mapping.MappingExtension
import com.hcore.ogre.mapping.Mapping.MappingExtension._
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.model.Reference
import com.hcore.ogre.morsels.Time

import scala.reflect.runtime.universe.TypeTag

class Schema(config :DBConfig, name :Option[String]=Schema.DefaultSchemaName) extends SchemaMapping(config, name) with ColumnTypes {
	schema =>


//	import ForeignKeyType._


	abstract class IdTable[E<:HasId](implicit val typeTag :TypeTag[E]) extends Table[E, Option[Id]] with StaticMapping[E] {
		def schemaName = Schema.this.name

		val id = column("id", _.id, AutoGen) //autoins("id", _.id)
//		val id = column(_.id)("id", ColumnFlagType.create(AutoGen))
		val PK = id

		override def isDefined(values: Values): Boolean =
			values(id).isDefined

//		protected def foreignKey[T](name :String, pick :E=>Reference[T], options :ColumnOption[Reference[T]]*)(implicit tpe :ColumnType[Reference[T]]) :Column[Reference[T]] =
//			column(name, pick, options:_*)

//		protected def foreignKey[T<:HasId :TypeTag](name :String, pick :E=>Reference[T], target : =>IdTable[T], options :ColumnOption[Reference[T]]*) :Column[Reference[T]] =
//			column(name, pick, options:_*)(ForeignKeyType(target))



		protected def foreignKey[FK, T<:HasId](name :String, pick :E=>FK, options :MappingExtension[FK]*)(table : =>IdTable[T])(implicit builder :ForeignKeyBuilder[FK, T]) :Column[FK] =
			builder(name, pick, ()=>table, options)

//		protected class ForeignKeyBuilder[FK](name :String, pick :E=>FK, options :Seq[ColumnOption[FK]]) {
//			def apply[T](target : =>IdTable[T])(implicit tag :TypeTag[T], ev : (E=>FK)=:=(E=>Reference[T])) :Column[Reference[T]] =
//				column(name, pick.asInstanceOf[E=>Reference[T]], options.asInstanceOf[Seq[ColumnOption[Reference[T]]]]:_*)(ForeignKeyType(target))
//
//			def apply[T](target : =>IdTable[T])(implicit tag :TypeTag[T], ev : FK=:=Option[Reference[T]]) :Column[Option[Reference[T]]] =
//				column(name, pick.asInstanceOf[E=>Option[Reference[T]]], options.asInstanceOf[Seq[ColumnOption[Option[Reference[T]]]]]:_*)(
//					NullableOptionType(ForeignKeyType(target))
//				)
//
//
//		}

		protected trait ForeignKeyBuilder[FK, T<:HasId] {
			def apply(name :String, pick :E=>FK, target :()=>IdTable[T], options :Seq[MappingExtension[FK]]) :Column[FK]
		}
		protected implicit def idFK[T<:HasId] :ForeignKeyBuilder[Id, T] = new ForeignKeyBuilder[Id, T] {
			override def apply(name: String, pick: (E) => Id, target: () => IdTable[T], options: Seq[MappingExtension[Id]]): Column[Id] =
				column(name, pick, options:_*)
		}
		protected implicit def oneReferenceFK[T<:HasId :TypeTag] :ForeignKeyBuilder[Reference[T], T] = new ForeignKeyBuilder[Reference[T], T] {
			override def apply(name: String, pick: (E) => Reference[T], target: () => IdTable[T], options: Seq[MappingExtension[Reference[T]]]): Column[Reference[T]] =
				column(name, pick, options:_*)(ForeignKeyType(target()))
		}
		protected implicit def optionOneRefFK[T<:HasId :TypeTag]: ForeignKeyBuilder[Option[Reference[T]], T]  = new ForeignKeyBuilder[Option[Reference[T]], T] {
			override def apply(name: String, pick: (E) => Option[Reference[T]], target: () => IdTable[T], options: Seq[MappingExtension[Option[Reference[T]]]]): Column[Option[Reference[T]]] = {
//				implicit val fk :MappedType[Reference[T], Option[Id]] = ForeignKeyType(target())
				implicit val fk :MappedType[Reference[T], Id] = ForeignKeyType(target(), Some(_:Id),
					(pk:Option[Id]) => pk.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${target()} to a foreign key")))
//				implicit val fk = ForeignKeyType(target(), Some(_:Id), (pk:Option[Id]) => pk.getOrElse(throw new IllegalArgumentException(s"can't convert empty primary key of ${target()} to a foreign key")))
				column(name, pick, options: _*)(ColumnType[Option[Reference[T]]])
			}
		}
	}



	case class JoinRow[L<:HasId, R<:HasId](id :Option[Id], left :Reference[L], right :Reference[R]) extends HasId

	class JoinTable[L <:HasId, R<:HasId](val tableName :String, leftKey :String, rightKey :String)(
	        implicit leftTable :Table[L, Option[Id]], rightTable :Table[R, Option[Id]], leftType :TypeTag[L], rightClass :TypeTag[R]
		) extends IdTable[JoinRow[L, R]] with StaticMapping[JoinRow[L, R]]
	{

		implicit val left = column(leftKey, _.left, NoUpdate)
		implicit val right = column(rightKey, _.right, NoUpdate)

		def side[T](implicit col :Column[Reference[T]]) = col

		override def construct(implicit res: Values): JoinRow[L, R] = JoinRow(id, left, right)
	}


	object common {
		type HasHash = { def hash :String }
		type HasName = { def name :String }
		type HasLifecycle = { def addedAt :Date; def deletedAt :Option[Date] }
		type TypicalEntity = HasId with HasHash with HasName with HasLifecycle
		
		
		val hash = ColumnMapping[String]("hash", AutoGen)
		val name = ColumnMapping[String]("name")
		val addedAt = ColumnMapping[Date]("added_at") //todo :Default option
		val deletedAt = ColumnMapping[Option[Date]]("deleted_at")
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

		trait Addresses extends StaticMapping[Client.Address] {
			val companyName = column("company_name", _.companyName, ReadOnly)
			val street1 = column("street_1", _.street1, ReadOnly)
			val street2 = column("street_2", _.street2, ReadOnly)
			val city = column("city", _.city, ReadOnly)
			val postalCode = column("postal_code", _.postalCode, ReadOnly)
			val country = column("country", _.country, ReadOnly)

			override def construct(implicit res: Values): Client.Address =
				Client.Address(companyName, street1, street2, city, postalCode, country)

			override def isDefined(values: Values): Boolean = true
		}

		val email = column("email", _.email, ReadOnly)
		val phone = column("phone", _.phone, ReadOnly)
		val contactPerson = column("contact_person", _.contactPerson, ReadOnly)
		val password = column("password", _.password, ReadOnly)


		val invoiceInfo = new StaticComponent[Client.InvoiceInfo](_.invoiceInfo, "invoice_") {
			val vatNumber = column("vat_number", _.vatNumber)
			val defaultCurrency = column("default_currency", _.defaultCurrency)
			val defaultDueDays = column("default_due_days", _.defaultDueDays)
			val autoInvoicing = column("autoinvoicing", _.autoInvoicing)
			val comments = column("comments", _.comments)
			val email = column("email", _.email)

			val address = new StaticComponent(_.address) with Addresses :Component[Client.Address] with Addresses

			override def construct(implicit res: Values): Client.InvoiceInfo =
				Client.InvoiceInfo(vatNumber, address, defaultCurrency, defaultDueDays, autoInvoicing, comments, email)

			override def isDefined(values: Values): Boolean = true
		}

		val mailingAddress = new StaticComponent(_.mailingAddress, "mailing_") with Addresses
		val mailingInvoiceEmail = column("mailing_email_for_invoices", _.mailingInvoiceEmail)
		val status = column("status", _.status)

		override def construct(implicit res: Values): Client = { //val res2 :ComponentResult[this.type] = ???
//			(res \ (_.invoiceInfo) \ (_.address) \ (_.companyName)).get :Option[String]
			val x = invoiceInfo :\ invoiceInfo.address :\ invoiceInfo.address.companyName :Option[String]
			(this \\ invoiceInfo \ (_.address) \ (_.companyName))() :Option[String]
			(this \\ invoiceInfo)(_.address)(_.companyName)() :Option[String]
//			res(invoiceInfo \-> (_.address) \-> (_.companyName)) :Option[String]
//			(res :\ invoiceInfo :\ invoiceInfo.address :\ invoiceInfo.address.companyName).get :Option[String]
			Client(id, hash, name, password, email, phone, contactPerson, invoiceInfo, mailingAddress, mailingInvoiceEmail, status, addedAt, deletedAt)
		}
	}


	implicit object Products extends EntityTable[Product]("products") {
//		Clients(_.mailingAddress)(_.companyName)

		val client = column("client_id", _.client)
		val description = column("description", _.description)
		
		val status = column("status", _.status)
		val ptype = column("type", _.ptype)

		val country = column("country", _.country)

		def construct(implicit res: Values): Product =
			Product(id, hash, client, name, description, addedAt, deletedAt, status, ptype, country)
	}



	type OrderLike = common.TypicalEntity {
		def targetingPolicy :Reference[TargetingPolicy]
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


		def construct(implicit res: Values): TargetingPolicy = TargetingPolicy(
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

		
		def construct(implicit res: Values): Campaign =
			Campaign(id, hash, name, product, targetingPolicy, budgetLimit, budgetDailyLimit, startTime, endTime,
			         addedAt, deletedAt, status, ctype, impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit)
	}
	
	
	implicit object Orders extends OrderLikeTable[Order]("orders") {

		val campaign = column("campaign_id", _.campaign)

		val status = column("status", _.status)
		val otype = column("type", _.otype)

		val Bidding = new StaticComponent[BiddingConfig](_.bidding) {
			val strategy = column("std_bidding_strategy", _.strategy)
			val cpmValue = column("std_bidding_cpm_val", _.cpmValue)
			val cpcValue = column("std_bidding_cpc_val", _.cpcValue)
//			val cpaPixel0 = column("std_bidding_cpa_pixel_id_0", _.cpaPixel0)
//			val cpaPixel1 = column("std_bidding_cpa_pixel_id_1", _.cpaPixel1)
//			val cpaPixel2 = column("std_bidding_cpa_pixel_id_2", _.cpaPixel2)
			val cpaValue0 = column("std_bidding_cpa_val_0", _.cpaValue0)
			val cpaValue1 = column("std_bidding_cpa_val_1", _.cpaValue1)
			val cpaValue2 = column("std_bidding_cpa_val_2", _.cpaValue2)

//			def value(entity: Order): BiddingConfig = entity.bidding

			def construct(implicit res: Values): BiddingConfig =
				BiddingConfig(strategy, cpmValue, cpcValue, None, None, None, //cpaPixel0, cpaPixel1, cpaPixel2,
					cpaValue0, cpaValue1, cpaValue2)

			override def isDefined(values: Values): Boolean = true
		}

//		val trackingCPAPixel0 = column("std_tracking_cpa_pixel_id_0", _.trackingCPAPixel0)
//		val trackingCPAPixel1 = column("std_tracking_cpa_pixel_id_1", _.trackingCPAPixel1)
//		val trackingCPAPixel2 = column("std_tracking_cpa_pixel_id_2", _.trackingCPAPixel2)

		val dontIncludeInLearning = column("do_not_include_in_learning", _.dontIncludeInLearning)


		def construct(implicit res: Values): Order = Order(
			id, hash, name, campaign, targetingPolicy, budgetLimit, budgetDailyLimit, startTime, endTime,
			addedAt, deletedAt, status, otype, Bidding,
			None, None, None, dontIncludeInLearning,
//			trackingCPAPixel0, trackingCPAPixel1, trackingCPAPixel2, dontIncludeInLearning,
			impressionLimit, impressionDailyLimit, clickLimit, { /*(res \ (_.Bidding)).get :BiddingConfig;*/ clickDailyLimit}
		)


	}

	implicit object Pixels extends EntityTable[Pixel]("pixels") {
		val pixelType = column("type", _.pixelType)
		val status = column("status", _.status)

		val previous = foreignKey("prev_pixel_id", _.previous)(this)
		val product = column("product_id", _.product)

		override def construct(implicit res: Values): Pixel = Time(s"assembling pixel #${res(id)}"){

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

		def construct(implicit res: Values): Landing = Landing(
			id, hash, url, campaign, targetURL, addedAt, deletedAt, status
		)

	}

	class Tags(ownerName :String) extends IdTable[com.hcore.clientapi.entities.Tag] {
		val tableName = ownerName + "_tags"

		val owner = column(ownerName+"_id", _.owner)
		val key = column("key", _.key)
		val value = column("value", _.value)


		def construct(implicit res: Values): Tag =
			Tag(id, owner, key, value)

	}
	val ProductTags = new Tags("product")
	val CampaignTags = new Tags("campaign")
	val CreativeTags = new Tags("creative")


	implicit object Creatives extends IdTable[Creative] {
		val tableName = "creatives"
		val hash = column(common.hash)(_.hash)
		val addedAt = column(common.addedAt)(_.addedAt)
		val deletedAt = column(common.deletedAt)(_.deletedAt)
		val format = column("format", _.format)
		val fileName = column("file_name", _.file.name)
		val fileFormat = column("file_format", _.file.format)
		val fileData = column("file_data", _.file.data)
		val status = column("status", _.status)
		val ctype = column("type", _.ctype)
		val product = column("product_id", _.product)


		def construct(implicit res: Values): Creative = Creative(
			id, hash, format, new Creative.File(fileName, fileFormat, fileData),
			addedAt, deletedAt, status, ctype, product
		)

	}


	implicit val CampaignCreatives = new JoinTable[Campaign, Creative]("campaigns_creatives", "campaign_id", "creative_id")


}


object Schema {
	val DefaultSchemaName = Some("rtb")
}