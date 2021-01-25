package com.hcore.clientapi.repository

import java.util.Date

import com.hcore.clientapi.entities.Components.{Country, URL}
import com.hcore.clientapi.entities.Model.{HasId, Id, IdRef}
import com.hcore.clientapi.entities._
import com.hcore.ogre.model.Restriction.PropertyEquals
import com.hcore.ogre.model.Reference.{Satisfying, Single, Unknown}
import com.hcore.ogre.model.{CompositeReference, Reference}
import org.scalatest._

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

class RepositorySpec extends FlatSpec with Matchers {

	implicit def AnyAsLeft[T](value :T) = Left(value)
	implicit def AnyAsRight[T](value :T) = Right(value)
//	implicit def AnyAsOption[T](value :T) = Option(value)

	implicit def ref[T<:HasId :ClassTag](either :Either[Id, T]) :Reference[T] = either.fold(IdRef[T](_), e => e.id.map(IdRef[T](_)) getOrElse Reference(e))
	implicit def one[T<:HasId](id :Id) :Reference[T] = IdRef[T](id)
//	def maybe[T<:HasId](id :Id) :Reference[Option[T]] = IdRef.maybe(id)
	def maybe[T<:HasId](id :Id) :CompositeReference[Option[T], T] = ??? //IdRef.maybe(id)
//	def compositeMaybe[T<:HasId](id :Id) :CompositeReference[Option[T], T] = ???

//	implicit def selectOpt[T <: HasId](id :Id) :Reference[Option[T]] = IdRef.maybe(id)
//	implicit def maybeGetById[T <:HasId](id :Id)(implicit) :CompositeReference[Option[T], T]

	val Now = new Date()
	
	val repositories = Seq[(String, APIRepository)]()

//	val repositories = Seq[(String, APIRepository)](//("MockRepository", new MockRepository),
//	                       ("PlainRepository", new PlainAPIRepository(DBConfig.Defaults, Seq()))
//	)

	 
	val clientRef = IdRef[Client](Id(1L))

	def productTemplate(diff :Int, pk :Option[Id]=None) =
		Product(pk, null, clientRef, "get product "+diff, Some("RepositorySpec"), Now, None, Product.Status("active"), Product.Type("standard"), Some(Country("PL")))

	val policyTemplate = TargetingPolicy(None, Seq(), Seq(), Seq(), Seq(), Seq(), None, None, None, None, None, None, Seq(), Seq(), Seq(), Seq(), Seq(), None, None, Seq(), Seq(), Seq(), Seq(), None, Seq())

	def creativeTemplate(path :String, product :Reference[Product]=Unknown(), campaigns :Reference[Set[Campaign]]=Unknown()) =
		Creative(None, null, Creative.Format("10x20"), Creative.File(path, Creative.FileFormat("jpg"), Array[Byte]()),
		         Now, None, Creative.Status("active"), Creative.Type("standard"), product, campaigns)

	def landingTemplate(url :String, campaign :Reference[Campaign]=Unknown()) =
		Landing(None, null, new URL(url), campaign, None, Now, None, Landing.Status("active"))

	def campaignTemplate(product :Either[Id, Product], policy :Either[Id, TargetingPolicy], diff :Int, pk :Option[Id]=None) =
		Campaign(pk, null, "get campaign "+diff, ref(product), ref(policy), 0, 0, Now, Now, Now, None, Campaign.Status("active"), Campaign.Type("standard"), None, None, None, None)

	def orderTemplate(camp :Either[Id, Campaign], policy :Either[Id, TargetingPolicy], diff :Int, pk :Option[Id]=None) =
		Order(pk, null, "get order "+diff, camp, policy, 0, 0, Now, Now, Now, None, Order.Status("active"), Order.Type("standard"),
		      Order.BiddingConfig(None, None, None, None, None, None, None, None, None), None, None, None, false, None, None, None, None)


	repositories foreach { case (repoName, repository) =>
		import repository._

		inSession { implicit s =>

			

			s"$repoName.Clients" should "select all clients" in inSession { implicit s =>
				val clients = Clients()
			}


			def create(diff: Int) = {
				val prod = Products.save(productTemplate(diff))
				prod.id should not be None
				prod.hash shouldNot equal(null)
				prod
//				productTemplate(diff).copy(id = id)
			}

			case class Delete[T <:HasId](id :Id, store :repository.Store[T]) {
				def get(implicit s :Session) = store.maybe(IdRef.maybe(id))
				def delete()(implicit s :Session) = store.delete(id)
			}

			var toDelete = new ListBuffer[Delete[_]]

			val (prod1, prod2, prod3) = (create(1), create(2), create(3))
			val (id1, id2, id3) = (prod1.id.get, prod2.id.get, prod3.id.get)
			val (product1, product2, product3) = (Products.maybe(maybe(id1)), Products.maybe(maybe(id2)), Products.maybe(maybe(id3)))

			toDelete ++= Seq(Delete(id1, Products), Delete(id2, Products), Delete(id3, Products))

			repoName + ".Products" should "add new products" in inSession { implicit s =>
				id1 should not equal id2
				id1 should not equal id3
				id2 should not equal id3

				product1/*.map(_.copy(hash = null)) */should equal(Some(prod1))
				product2/*.map(_.copy(hash = null))*/ should equal(Some(prod2))
				product3/*.map(_.copy(hash = null))*/ should equal(Some(prod3))
			}

			repoName + ".Products" should "update existing product" in inSession { implicit s =>
				//			System.err.println("get update product")
				val updated = product2.get.copy(name = "updated")
				Products += updated
				Products.maybe(maybe(updated.id.get)) should equal(Some(updated))
			}

			val (Some(policy11Id), Some(policy12Id)) = (TargetingPolicies += policyTemplate, TargetingPolicies += policyTemplate)
			val (Some(policy11), Some(policy12)) = (TargetingPolicies.maybe(maybe(policy11Id)), TargetingPolicies.maybe(maybe(policy12Id)))

			val (camp11, camp12) = (campaignTemplate(id1, policy11Id, 1), campaignTemplate(product1.get, policy12, 2))
			val (Some(campaign11Id), Some(campaign12Id)) = (Campaigns += camp11, Campaigns += camp12)

			toDelete ++= Seq(Delete(campaign11Id, Campaigns), Delete(campaign12Id, Campaigns))

			repoName + ".Campaigns" should "fetch requested relations with campaign" in inSession { implicit s =>
				implicit def AnyAsOption[X](value: X) = Option(value)

				val (campaign11, campaign12) = (Campaigns.maybe(maybe(campaign11Id)), Campaigns.maybe(maybe(campaign12Id)))
				val Some(c) = campaign11
				c.copy(product = Single(PropertyEquals((_:Product).hash, "dupa")))

				campaign11.map(_.copy(hash = null)) should equal(Some(campaignTemplate(id1, policy11Id, 1, campaign11Id)))
				campaign12.map(_.copy(hash = null)) should equal(Some(campaignTemplate(id1, policy12Id, 2, campaign12Id)))

				val (withPolicy1, withPolicy2) = (Campaigns.maybe(maybe(campaign11Id), (_:Campaign).targetingPolicy), Campaigns.maybe(maybe(campaign12Id), (_:Campaign).targetingPolicy))

				withPolicy1.map(_.copy(hash = null)) should equal(Some(campaignTemplate(id1, policy11, 1, campaign11Id)))
				withPolicy2.map(_.copy(hash = null)) should equal(Some(campaignTemplate(id1, policy12, 2, campaign12Id)))

				val (withProduct1, withProduct2) = (Campaigns.maybe(maybe(campaign11Id), _.product), Campaigns.maybe(maybe(campaign12Id), _.product))
				withProduct1.map(_.copy(hash = null)) should equal(Some(campaignTemplate(product1.get, policy11Id, 1, campaign11Id)))
				withProduct2.map(_.copy(hash = null)) should equal(Some(campaignTemplate(product1.get, policy12Id, 2, campaign12Id)))

				val (withEverything1, withEverything2) = (Campaigns.maybe(maybe(campaign11Id), _.product, _.targetingPolicy), Campaigns.maybe(maybe(campaign12Id), _.targetingPolicy, _.product))
				withEverything1.map(_.copy(hash = null)) should equal(Some(campaignTemplate(product1.get, policy11, 1, campaign11Id)))
				withEverything2.map(_.copy(hash = null)) should equal(Some(campaignTemplate(product1.get, policy12, 2, campaign12Id)))
			}



			val (ord111, ord112) = (orderTemplate(campaign11Id, policy11Id, 1), orderTemplate(campaign11Id, policy12Id, 2))
			val (Some(order111Id), Some(order112Id)) = (Orders += ord111, Orders += ord112)

			toDelete ++= Seq(Delete(order111Id, Orders), Delete(order112Id, Orders))

			repoName + ".Orders" should "fetch requested transitive relations with an order" in inSession { implicit s =>
				implicit class Assertions(val o: Option[Order]) {
					def hasProduct = {
						//					System.err.println(s"should have campaign.product: $o")
						o.flatMap(_.campaign).flatMap(_.product) should equal(product1)
						this
					}

					def hasCampaignPolicy = {
						o.flatMap(_.campaign).flatMap(_.targetingPolicy) should equal(Some(policy11))
						this
					}

					def hasPolicy = {
						o.flatMap(_.targetingPolicy) should equal(Some(policy11))
					}
				}

//				"GET /orders/13413/"
//				One((_:Campaign).hash, "hash")
				Orders.maybe(maybe(order111Id), _.campaign.join.product).hasProduct
				Orders.maybe(maybe(order111Id), _.campaign, _.campaign.join.product).hasProduct
				Orders.maybe(maybe(order111Id), _.campaign.join.product, _.campaign).hasProduct

				Orders.maybe(maybe(order111Id), _.campaign.join.product, _.campaign.join.targetingPolicy).hasProduct.hasCampaignPolicy
				Orders.maybe(maybe(order111Id), _.campaign.join.targetingPolicy, _.campaign.join.product).hasProduct.hasCampaignPolicy
				Orders.maybe(maybe(order111Id), _.campaign.join.targetingPolicy, _.campaign, _.campaign.join.product).hasProduct.hasCampaignPolicy

				Orders.maybe(maybe(order111Id), _.campaign.join.product, _.campaign.join.targetingPolicy, _.targetingPolicy).hasProduct.hasCampaignPolicy.hasPolicy
				Orders.maybe(maybe(order111Id), _.campaign.join.product, _.targetingPolicy, _.campaign.join.targetingPolicy).hasProduct.hasCampaignPolicy.hasPolicy
			}




			repoName + ".Orders" should "fetch orders by campaign" in inSession { implicit s =>
				val expected = Seq(Orders.maybe(maybe(order111Id)), Orders.maybe(maybe(order112Id))).flatten
				val orders = Orders(Satisfying(PropertyEquals((_: Order).campaign)).in[Seq].empty(campaign11Id))

				orders.size should be(2)
				expected.foreach { o => orders.contains(o) shouldBe true}

			}

			s"$repoName.Orders" should "fetch orders by product" in inSession { implicit s =>
				val expected = Seq(Orders.maybe(maybe(order111Id)).get, Orders.maybe(maybe(order112Id)).get)
				val byProduct = Orders(Satisfying(PropertyEquals((_:Order).campaign.join.product)).in[Seq].empty(IdRef(id1)))

				byProduct.size should be(2)
				expected.foreach { o=> byProduct.contains(o) shouldBe true}

			}


			val creatives = Set(creativeTemplate("file1", id1), creativeTemplate("file2", id2))
			val landings = Set(landingTemplate("http://localhost"), landingTemplate("http://google.com"))
			val campaign = campaignTemplate(id1, policyTemplate, 1).copy(creatives=Reference(creatives), landings=Reference(landings))

			val saved = repository.save(campaign)

			toDelete ++= saved.targetingPolicy.flatMap(_.id).map(Delete(_, TargetingPolicies))
			toDelete ++= saved.creatives.toOpt.toSeq.flatten.flatMap(_.id).map(Delete(_, Creatives))
			toDelete ++= saved.landings.toOpt.toSeq.flatten.flatMap(_.id).map(Delete(_, Landings))




			repoName + ".Campaigns" should "save dependent entities when saving a campaign" in inSession { implicit s =>


				def test(saved :Campaign) = {
					saved.id shouldBe Some(_:Id)
					saved.targetingPolicy.toOpt.flatMap(_.id) shouldNot equal(None)
					TargetingPolicies.maybe(maybe(saved.targetingPolicy.get.id.get)).map(_.copy(id = None)) should equal(Some(policyTemplate))

					saved.creatives.toOpt.toSeq.flatten.map(_.file.name).toSet should equal(creatives.map(_.file.name))
					saved.landings.toOpt.toSeq.flatten.map(_.url).toSet should equal(landings.map(_.url))

//					System.err.println(s"targeting policy: $saved.targetingPolicy")
//					System.err.println(s"landings: ${saved.landings.toOpt.toSeq.flatten.map(_.url)}")
//					System.err.println(s"creatives: ${saved.creatives.toOpt.toSeq.flatten.map(_.file.name)}")
				}

				test(saved)

				val read = repository[Campaign, Option[Campaign]](IdRef.maybe[Campaign](saved.id.get), _.targetingPolicy, _.creatives, _.landings)
				read shouldNot equal(None)
				test(read.get)

			}

			repoName +".Campaigns" should "delete landings with a campaign" in inSession { implicit s =>
				repository.delete(saved)
				val landings = Landings(Satisfying(PropertyEquals((_:Landing).campaign)).in[Seq].empty(IdRef(saved)))
				landings should equal(Seq())
			}

			repoName should "delete created entities by id" in inSession { implicit s =>
				toDelete.reverse.foreach {
					case delete =>
						delete.delete()
						delete.get should equal(None)
				}

			}
		}
	}

}
