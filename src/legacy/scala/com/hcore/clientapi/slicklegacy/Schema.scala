package com.hcore.clientapi.repository.slicklegacy

import com.hcore.clientapi.repository.DBConfig


class Schema(config :DBConfig) /*extends Repository {
	val driver = config.driver

	import driver.{simple => ql}
	import ql._

//	val db = config.database
	val db = {
		import config._
		Database.forURL(url, user.orNull, password.orNull, driver = driverClass)
	}
//	import db._

	private val schemaName = Some("rtb")
	val CSVSep = ","



	def withSession[T](block :Session => T) = db.withSession(block)



	def Boxed[B :ClassTag, C :BaseColumnType](box :C=>B)(unbox :B=>C) = MappedColumnType.base[B, C](unbox, box)

	def StringSeq[B](box :String=>B)(unbox :B=>String) = MappedColumnType.base[Seq[B], String](
		_.map(unbox).mkString(CSVSep), Option(_).filter(_!="").toSeq.flatMap(_.split(CSVSep)).map(box)
	)

	implicit val csvType = Boxed[Seq[String], String](Option(_).filter(_!="").toSeq.flatMap(_.split(CSVSep)).map(_.trim))(_.mkString(CSVSep))
	implicit val dateType = MappedColumnType.base[Date, Timestamp](d => new Timestamp(d.getTime), d => new Date(d.getTime))
	implicit val urlType = Boxed(new URL(_:String))(_.toString)

	implicit val campaignTypeType = Boxed(Campaign.Type)(_.name)
	implicit val campaignStatusType = Boxed(Campaign.Status)( _.name)
	implicit val productTypeType = Boxed(Product.Type)(_.name)
	implicit val productStatusType = Boxed(Product.Status)(_.name)
	implicit val orderTypeType = Boxed(Order.Type)(_.name)
	implicit val orderStatusType = Boxed(Order.Status)(_.name)
	implicit val landingStatusType = Boxed(Landing.Status)(_.name)
	implicit val creativeTypeType = Boxed(Creative.Type)(_.name)
	implicit val creativeStatusType = Boxed(Creative.Status)(_.name)
	implicit val creativeFormat = Boxed(Creative.Format)(_.format)
	implicit val creativeFileFormat = Boxed(Creative.FileFormat)(_.name)

	implicit val countryType = Boxed(Country)(_.code)
	implicit val biddingStrategyType = Boxed(BiddingStrategy)(_.name)

	implicit val networksType = StringSeq(Network)(_.name)
	implicit val hoursType = StringSeq(new Hour(_:String))(_.toString)
	implicit val geosType = StringSeq(Geo)(_.location)
	implicit val countriesType = StringSeq(Country)(_.code)
	implicit val languagesType = StringSeq(Language)(_.code)
	implicit val ipRangesType = StringSeq(IPRange)(_.range)
	implicit val sspBucketsType = StringSeq(BucketSSP)(_.buckets)
	
	implicit val bucketType = Boxed(Bucket)(_.name)
	

	implicit val idType = MappedColumnType.base[Id, Long](_.value, Id.apply)
//	implicit def foreignKeyType[T :BaseColumnType, E] = MappedColumnType.base[ForeignKey[E, T], T](_.keyOpt.get, ForeignKey())
	implicit def foreignKeyType[E<:HasId] = MappedColumnType.base[One[E], Id](IdRef.idOf(_), IdRef[E](_))





	abstract class IdTable[T](tag :ql.Tag, name :String) extends Table[T](tag, schemaName, name) {
		def id = column[Option[Id]]("id", O.PrimaryKey, O.AutoInc)
	}	

	abstract class EntityTable[T](tag :ql.Tag, name :String) extends IdTable[T](tag, name) {
		def hash = column[String]("hash", O.AutoInc)
	}

	abstract class NamedTable[T](tag :ql.Tag, tableName :String) extends EntityTable[T](tag, tableName) {
		def name = column[String]("name")
		def addedAt = column[Date]("added_at")
		def deletedAt = column[Option[Date]]("deleted_at")
	}


	class Products(tag :ql.Tag) extends NamedTable[Product](tag, "products") {
		def client = column[One[Nothing]]("client_id", O.NotNull)(foreignKeyType[Nothing])
		def description = column[Option[String]]("description")

		def status = column[Product.Status]("status")
		def ptype = column[Product.Type]("type")

		def country = column[Country]("country")

		def * = (id, hash, client, name, description, addedAt, deletedAt, status, ptype, country) <> (
				(Product.apply _).tupled, Product.unapply _
			)
	}
	val Products = TableQuery[Products](new Products(_))

	trait OrderLikeTable[T] { this :Table[T] =>
		def targetingPolicy = column[One[TargetingPolicy]]("targeting_policy_id")
		def budgetLimit = column[Double]("budget_limit")
		def budgetDailyLimit = column[Double]("budget_daily_limit")
		def startTime = column[Date]("start_time")
		def endTime = column[Date]("end_time")

		def impressionLimit = column[Option[Long]]("impression_limit")
		def impressionDailyLimit = column[Option[Long]]("impression_daily_limit")
		def clickLimit = column[Option[Long]]("click_limit")
		def clickDailyLimit = column[Option[Long]]("click_daily_limit")

	}

	class Campaigns(tag :ql.Tag) extends NamedTable[Campaign](tag, "campaigns") with OrderLikeTable[Campaign] {
		def product = column[One[Product]]("product_id", O.NotNull)

		def status = column[Campaign.Status]("status")
		def ctype = column[Campaign.Type]("type")


//		def products = foreignKey("rtb_campaigns_products_fkey", product, Products)(_.id)

		def * = (id, hash, name, product, targetingPolicy, budgetLimit, budgetDailyLimit,
		         startTime, endTime, addedAt, deletedAt, status, ctype, impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit) <> (
				(Campaign.apply _).tupled, Campaign.unapply _
			)
	}
	val Campaigns = TableQuery[Campaigns](new Campaigns(_))



	class Orders(tag :ql.Tag) extends NamedTable[Order](tag, "orders") with OrderLikeTable[Order] {
		def campaign = column[One[Campaign]]("campaign_id")


		def status = column[Order.Status]("status")
		def otype = column[Order.Type]("type")

		def biddingStrategy = column[Option[BiddingStrategy]]("std_bidding_strategy")
		def biddingCPMValue = column[Option[Double]]("std_bidding_cpm_val")
		def biddingCPCValue = column[Option[Double]]("std_bidding_cpc_val")
		def biddingCPAPixel0 = column[Option[One[Pixel]]]("std_bidding_cpa_pixel_id_0")
		def biddingCPAPixel1 = column[Option[One[Pixel]]]("std_bidding_cpa_pixel_id_1")
		def biddingCPAPixel2 = column[Option[One[Pixel]]]("std_bidding_cpa_pixel_id_2")
		def biddingCPAValue0 = column[Option[Double]]("std_bidding_cpa_val_0")
		def biddingCPAValue1 = column[Option[Double]]("std_bidding_cpa_val_1")
		def biddingCPAValue2 = column[Option[Double]]("std_bidding_cpa_val_2")

		def trackingCPAPixel0 = column[Option[One[Pixel]]]("std_tracking_cpa_pixel_id_0")
		def trackingCPAPixel1 = column[Option[One[Pixel]]]("std_tracking_cpa_pixel_id_1")
		def trackingCPAPixel2 = column[Option[One[Pixel]]]("std_tracking_cpa_pixel_id_2")

		def dontIncludeInLearning = column[Boolean]("do_not_include_in_learning")

//		type Packed = (
//			Column[Option[Id]], Column[String], Column[String], Column[One[Campaign]], Column[One[TargetingPolicy]],
//		    Column[Double], Column[Double], Column[Date], Column[Date], Column[Date], Column[Option[Date]],
//			(Column[Option[BiddingStrategy]], Column[Option[Double]], Column[Option[Double]],
//			 Column[Option[One[Pixel]]], Column[Option[One[Pixel]]], Column[Option[One[Pixel]]],
//			 Column[Option[Double]], Column[Option[Double]], Column[Option[Double]]),
//			Column[Option[One[Pixel]]], Column[Option[One[Pixel]]], Column[Option[One[Pixel]]],
//			Column[Boolean], Column[Option[Long]], Column[Option[Long]], Column[Option[Long]], Column[Option[Long]]
//		)
		type Unpacked = (
			Option[Id], String, String, One[Campaign], One[TargetingPolicy],
			Double, Double, Date, Date, Date, Option[Date], Order.Status, Order.Type,
//			Order.BiddingConfig,
			(Option[BiddingStrategy], Option[Double], Option[Double],
			 Option[One[Pixel]], Option[One[Pixel]], Option[One[Pixel]],
			 Option[Double], Option[Double], Option[Double]),
			Option[One[Pixel]], Option[One[Pixel]], Option[One[Pixel]],
			Boolean, Option[Long], Option[Long], Option[Long], Option[Long]
		)


		def * = (id, hash, name, campaign, targetingPolicy,
		         budgetLimit, budgetDailyLimit, startTime, endTime, addedAt, deletedAt, status, otype,
			     (biddingStrategy, biddingCPMValue, biddingCPCValue, biddingCPAPixel0, biddingCPAPixel1, biddingCPAPixel2,
		         biddingCPAValue0, biddingCPAValue1, biddingCPAValue2), trackingCPAPixel0, trackingCPAPixel1, trackingCPAPixel2,
			     dontIncludeInLearning, impressionLimit, impressionDailyLimit, clickLimit, clickDailyLimit
			) <> (
				(cols :Unpacked) => Order(cols._1, cols._2, cols._3, cols._4, cols._5,
				                          cols._6, cols._7, cols._8, cols._9, cols._10, cols._11, cols._12, cols._13,
					                      (Order.BiddingConfig.apply _).tupled(cols._14), cols._15, cols._16, cols._17,
				                          cols._18, cols._19, cols._20, cols._21, cols._22),
				(x :Order) => Order.unapply(x).map(x => (x._1, x._2, x._3, x._4, x._5,
					                                      x._6, x._7, x._8, x._9, x._10, x._11, x._12, x._13,
				                                          Order.BiddingConfig.unapply(x._14).get,
					                                      x._15, x._16, x._17,
					                                      x._18, x._19, x._20, x._21, x._22))
			)


	}
	val Orders = TableQuery(new Orders(_))


	class TargetingPolicies(tag :ql.Tag) extends IdTable[TargetingPolicy](tag, "targeting_policy") {
//		def id = column[Option[Id]]("id", O.PrimaryKey, O.AutoInc)
		def networks = column[Seq[Network]]("networks", O.Nullable)
		def hours = column[Seq[Hour]]("hours", O.Nullable)
		def geos = column[Seq[Geo]]("geo", O.Nullable)
		def countries = column[Seq[Country]]("countries", O.Nullable)
		def languages = column[Seq[Language]]("languages", O.Nullable)
		def acceptUnknownLanguages = column[Option[Boolean]]("accept_unknown_languages") //todo: default value needed for null values
		def capDaily = column[Option[Int]]("cap_daily")
		def capLifetime = column[Option[Int]]("cap_lifetime")
		def urlPattern = column[Option[String]]("url_pattern")
		def requiredTagsPattern = column[Option[String]]("required_tags_pattern")
		def forbiddenTagsPattern = column[Option[String]]("forbidden_tags_pattern")
		def topicsADX = column[Seq[String]]("topics_adx", O.Nullable)
		def topicsRMX = column[Seq[String]]("topics_rmx", O.Nullable)
		def topicsOpenX = column[Seq[String]]("topics_openx", O.Nullable)
		def sections = column[Seq[String]]("sections", O.Nullable)
		def folds = column[Seq[String]]("folds", O.Nullable)
		def requiredURLPattern = column[Option[String]]("required_url_pattern")
		def forbiddenURLPattern = column[Option[String]]("forbidden_url_pattern")
		def blackSellerRMX = column[Seq[String]]("black_seller_rmx", O.Nullable)
		def whiteSellerRMX = column[Seq[String]]("white_seller_rmx", O.Nullable)
		def ipRangesWhite = column[Seq[IPRange]]("ip_ranges_white", O.Nullable)
		def ipRangesBlack = column[Seq[IPRange]]("ip_ranges_black", O.Nullable)
		def bucket = column[Option[Bucket]]("bucket")
		def sspBuckets = column[Seq[BucketSSP]]("bucket_ssp", O.Nullable)
		
		type Unpacked = (
			Option[Id], Seq[Network], Seq[Hour], Seq[Geo], Seq[Country], Seq[Language], Option[Boolean],
			(Option[Int], Option[Int]), Option[String], (Option[String], Option[String]),
			(Seq[String], Seq[String], Seq[String]), Seq[String], Seq[String], (Option[String], Option[String]),
			(Seq[String], Seq[String]), (Seq[IPRange], Seq[IPRange]), Option[Bucket], Seq[BucketSSP]
		)
		
		def * = (id, networks, hours, geos, countries, languages, acceptUnknownLanguages, 
		         (capDaily, capLifetime), urlPattern, (requiredTagsPattern, forbiddenTagsPattern), 
		         (topicsADX, topicsRMX, topicsOpenX), sections, folds, (requiredURLPattern, forbiddenURLPattern),
		         (whiteSellerRMX, blackSellerRMX), (ipRangesWhite, ipRangesBlack), bucket, sspBuckets) <> (
			(cols :Unpacked) => TargetingPolicy(cols._1, cols._2, cols._3, cols._4, cols._5, cols._6, cols._7,
				                                cols._8._1, cols._8._2, cols._9, cols._10._1, cols._10._2,
			                                    cols._11._1, cols._11._2, cols._11._3, cols._12, cols._13, cols._14._1, cols._14._2,
			                                    cols._15._1, cols._15._2, cols._16._1, cols._16._2, cols._17, cols._18),
			(x :TargetingPolicy) => Some(x.id, x.networks, x.hours, x.geos, x.countries, x.languages, x.acceptUnknownLanguages,
			                         (x.capDaily, x.capLifetime), x.urlPattern, (x.requiredTagsPattern, x.forbiddenTagsPattern),
			                         (x.topicsADX, x.topicsRMX, x.topicsOpenX), x.sections, x.folds, (x.requiredURLPattern, x.forbiddenURLPattern),
			                         (x.whiteSellerRMX, x.blackSellerRMX), (x.whiteIPRanges, x.blackIPRanges), x.bucket, x.sspBuckets)
			)


	}
	val TargetingPolicies = TableQuery(new TargetingPolicies(_))


	class Landings(tag :ql.Tag) extends EntityTable[Landing](tag, "landings") {
		def url = column[URL]("url")
		def campaign = column[One[Campaign]]("campaign_id")
		def targetURL = column[Option[URL]]("target_url")
		def addedAt = column[Date]("added_at")
		def deletedAt = column[Option[Date]]("deleted_at")
		def status = column[Landing.Status]("status")

		def * = (id, hash, url, campaign, targetURL, addedAt, deletedAt, status) <> (
			(Landing.apply _).tupled, Landing.unapply
		)
	}
	val Landings = TableQuery(new Landings(_))

	class Tags(val tag :ql.Tag, ownerName :String) extends IdTable[com.hcore.clientapi.entities.Tag](tag, ownerName+"_tags") {
//		def id = column[Option[Id]]("id", O.PrimaryKey, O.AutoInc)
		def owner = column[One[Nothing]](ownerName+"_id")
		def key = column[String]("key")
		def value = column[String]("value")

		def * = (id, owner, key, value) <> ((entities.Tag.apply _).tupled, entities.Tag.unapply)
	}
	val ProductTags = TableQuery(new Tags(_, "product"))
	val CampaignTags = TableQuery(new Tags(_, "campaign"))



	class Creatives(tag :ql.Tag) extends EntityTable[Creative](tag, "creatives") {
		def format = column[Creative.Format]("format")
		def fileName = column[String]("file_name")
		def fileFormat = column[Creative.FileFormat]("file_format")
		def fileData = column[Array[Byte]]("file_data")
		def addedAt = column[Date]("added_at")
		def deletedAt = column[Option[Date]]("deleted_at")
		def status = column[Creative.Status]("status")
		def ctype = column[Creative.Type]("type")
		def product = column[One[Product]]("product_id")

		type Unpacked = (
			Option[Id], String, Creative.Format, (String, Creative.FileFormat, Array[Byte]),
			Date, Option[Date], Creative.Status, Creative.Type, One[Product]
		)

		def * = (id, hash, format, (fileName, fileFormat, fileData), addedAt, deletedAt, status, ctype, product) <> (
			(cols :Unpacked) => Creative(cols._1, cols._2, cols._3, new Creative.File(cols._4._1, cols._4._2, cols._4._3),
			                             cols._5, cols._6, cols._7, cols._8, cols._9),
			(c :Creative) => Some(c.id, c.hash, c.format, (c.file.name, c.file.format, c.file.data),
				              c.addedAt, c.deletedAt, c.status, c.ctype, c.product)
		)
	}
	val Creatives = TableQuery(new Creatives(_))






}
*/


object Schema {


}