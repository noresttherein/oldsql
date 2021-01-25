package com.adpilot.cortb.clientapi.prototype.repository.mapping

import java.util.Date

import com.adpilot.cortb.clientapi.prototype.repository.entities
import com.adpilot.cortb.clientapi.prototype.repository.entities.Components._
import com.adpilot.cortb.clientapi.prototype.repository.entities.Creative.FileFormat
import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{IdRef, Id}
import com.adpilot.cortb.clientapi.prototype.repository.entities.Order.BiddingConfig
import com.adpilot.cortb.clientapi.prototype.repository.entities.{Product => Prod, Tag=>Tagg, _}
import com.adpilot.cortb.clientapi.prototype.repository.DBConfig
import com.adpilot.cortb.clientapi.util.OptionOps
import org.scalatest._


/*
class SchemaSpec extends FlatSpec with Matchers {
	import OptionOps._
	import OptionOps.AnyToOption._
	
	System.err.println(DBConfig.Defaults)
	val schema = new Schema(DBConfig.Defaults)
	import schema._
	import schema.driver.simple._

	"mapping.Schema" should "create, update & delete products" in {
		withSession { implicit s =>
			val prod = new Prod(None, null, IdRef(Id(1L)), "bilbo", Some("hobbit product"), new Date(), Some(new Date()), Prod.Status("active"), Prod.Type("standard"), Country("PL"))
			
			val (id, hash) = (Products returning Products.map(c => (c.id, c.hash))).insert(prod)
			
			id should not be None
			hash should not be None
			
			val expected = prod.copy(id = id, hash = hash)
			val inserted = Products.filter(_.id===id).list
			inserted shouldEqual Seq(expected)
			
			Products.filter(_.id===id).map(_.name).update("frodo")
			val updated = Products.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(name="frodo"))
			
			Products.filter(_.id===id).delete
			val deleted = Products.filter(_.id===id).list
			deleted shouldEqual Seq()
	    }
	}

	"mapping.Schema" should "create, update & delete campaigns" in {

		withSession { implicit s =>
			val camp: Campaign = new Campaign(None, null, "bilbo", IdRef(Id(1L)), IdRef(Id(1L)), 0.1, 0.2, new Date(), new Date(1000), new Date(2000), None, new Campaign.Status("active"), new Campaign.Type("standard"), Some(2), Some(3), Some(4), Some(5))

			val (id, hash) = (Campaigns returning Campaigns.map(c => (c.id, c.hash))).insert(camp)

			id should not be None
			hash should not be None

			val expected = camp.copy(id = id, hash = hash)
			val inserted = Campaigns.filter(_.id===id).list
			inserted shouldEqual Seq(expected)

			Campaigns.filter(_.id===id).map(_.name).update("frodo")
			val updated = Campaigns.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(name="frodo"))

			Campaigns.filter(_.id===id).delete
			val deleted = Campaigns.filter(_.id===id).list
			deleted shouldEqual Seq()
		}
	}

	"mapping.Schema" should "create, update & delete orders" in {
		withSession { implicit s =>
			val order = new Order(None, null, "bilbo", IdRef(Id(1L)), IdRef(Id(2L)), 0.1, 0.2,
				new Date(), new Date(), new Date(0L), new Date(1L), Order.Status("active"), Order.Type("standard"),
				new BiddingConfig(new BiddingStrategy("wtf"), 0.1, 0.2, IdRef[Pixel](Id(42)), IdRef[Pixel](Id(44)), IdRef[Pixel](Id(11)), 0.3, 0.4, 0.5),
				IdRef[Pixel](Id(1)), IdRef[Pixel](Id(2)), IdRef[Pixel](Id(3)), true, 33L, 66L, 69L, 102L)

			val (id, hash) = (Orders returning Orders.map(c => (c.id, c.hash))).insert(order)

			id should not be None
			hash should not be None

			val expected = order.copy(id = id, hash = hash)
			val inserted = Orders.filter(_.id===id).list
			inserted shouldEqual Seq(expected)

			Orders.filter(_.id===id).map(_.name).update("frodo")
			val updated = Orders.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(name="frodo"))

			Orders.filter(_.id===id).delete
			val deleted = Orders.filter(_.id===id).list
			deleted shouldEqual Seq()
		}
	}

	"mapping.Schema" should "create, update & delete targeting policies" in {
		withSession { implicit s =>
			val targeting = new TargetingPolicy(None, Seq(Network("n1"), Network("n2")), Seq(Hour(12), Hour(13), Hour(14)),
				Seq(Geo("moon")), Seq(Country("pl")), Seq(Language("pl")), false, 1, 2, "(.*)", ".*", "", Seq("topic"), Seq("top cat"),
				Seq("openx"), Seq("s1", "s2"), Seq("f1", "f2", "f3"), ".*", "ala", Seq("white"), Seq("black"),
				Seq(IPRange("ip1")), Seq(IPRange("ip")), Bucket("bucket"), Seq(BucketSSP("ssp1"), BucketSSP("ssp2"))
			)

			val id = (TargetingPolicies returning TargetingPolicies.map(_.id)).insert(targeting)

			id should not be None

			val expected = targeting.copy(id = id)
			val inserted = TargetingPolicies.filter(_.id===id).list
			inserted shouldEqual Seq(expected)

			TargetingPolicies.filter(_.id===id).map(t => (t.geos, t.bucket)).update((Seq(), None))
			val updated = TargetingPolicies.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(geos=Seq(), bucket=None))

			TargetingPolicies.filter(_.id===id).delete
			val deleted = TargetingPolicies.filter(_.id===id).list
			deleted shouldEqual Seq()
		}
	}

	"mapping.Schema" should "create, update & delete landings" in {
		withSession { implicit s =>
			val landing = new Landing(None, null, new URL("http://dailywtf.com"), IdRef(Id(1L)), new URL("http://programming-motherfucker.com"), new Date(1L), new Date(), Landing.Status("active"))

			val (id, hash) = (Landings returning Landings.map(c => (c.id, c.hash))).insert(landing)

			id should not be None
			hash should not be None

			val expected = landing.copy(id = id, hash = hash)
			val inserted = Landings.filter(_.id===id).list
			inserted shouldEqual Seq(expected)

			Landings.filter(_.id===id).map(l => (l.campaign, l.targetURL)).update((IdRef(Id(2)), None))
			val updated = Landings.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(campaign=IdRef(Id(2)), targetURL=None))

			Landings.filter(_.id===id).delete
			val deleted = Landings.filter(_.id===id).list
			deleted shouldEqual Seq()
		}
	}

	"mapping.Schema" should "create, update & delete creatives" in {
		withSession { implicit s =>
			val creative = new Creative(None, null, Creative.Format("10x20"),
				Creative.File("whoooaa.gif", FileFormat("gif"), Seq(1.toByte, 2.toByte).toArray),
				new Date(11), new Date(), Creative.Status("active"), Creative.Type("standard"), IdRef(Id(1)))

			val (id, hash) = (Creatives returning Creatives.map(c => (c.id, c.hash))).insert(creative)

			id should not be None
			hash should not be None

			val expected = creative.copy(id = id, hash = hash)
			val inserted = Creatives.filter(_.id===id).list
			inserted shouldEqual Seq(expected)

			Creatives.filter(_.id===id).map(c => (c.fileName, c.fileFormat)).update(("xxxx.jpg"), FileFormat("jpg"))
			val updated = Creatives.filter(_.id===id).list
			updated shouldEqual Seq(expected.copy(file=Creative.File("xxxx.jpg", format=FileFormat("jpg"), data=expected.file.data)))

			Creatives.filter(_.id===id).delete
			val deleted = Creatives.filter(_.id===id).list
			deleted shouldEqual Seq()
		}
	}

	testTags("product tags", ProductTags)

	testTags("campaign tags", CampaignTags)

	def testTags(name :String, tags :TableQuery[Tags]) =
		"mapping.Schema" should s"create, update & delete $name" in {
			withSession { implicit s =>
				val tag = new Tagg(None, IdRef(Id(1)), "adx_pc", "smart")

				val id = (tags returning tags.map(_.id)).insert(tag)

				id should not be None

				val expected = tag.copy(id = id)
				val inserted = tags.filter(_.id===id).list
				inserted shouldEqual Seq(expected)

				tags.filter(_.id===id).map(_.value).update("dumb")
				val updated = tags.filter(_.id===id).list
				updated shouldEqual Seq(expected.copy(value="dumb"))

				tags.filter(_.id===id).delete
				val deleted = tags.filter(_.id===id).list
				deleted shouldEqual Seq()
			}
		}
	
	
}
*/
