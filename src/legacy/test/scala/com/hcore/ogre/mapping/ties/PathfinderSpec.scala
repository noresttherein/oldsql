package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.{MappingPath, Mapping}
import com.hcore.ogre.mapping.Mapping.ReferenceContext
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.mapping.ties.MappingReference.LazyMappingReference
import com.hcore.ogre.model.CompositeReference.BaseCompositeReference
import com.hcore.ogre.model.{ComposedOf, NavigableReference, Reference}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.sql.From
import org.scalatest.{FlatSpec, Matchers}


class PathfinderSpec extends FlatSpec with Matchers {
	
	case class Favourites(hats :Reference[Seq[Hat]])
	
	case class Actress(name :String, lover :Reference[Lover], favourites :Favourites)
	
	case class Hat(colour :String, kind :String)
	
	case class Lover(name :String)
	
	
	import NavigableReference._
	
	class MockRefMapping[T, E](val table :Mapping[E])(implicit composition :T ComposedOf E) extends StaticMapping[Reference[T]]
	{ mapping =>

		//		val path = JoinPath[this.type, last.type](From[this.type](this) join (last :last.type))((_:Reference[T]) => None: Option[E])
		val path = JoinPath[this.type, table.type](From[table.type](table) joinInstance this, (_:Reference[T]) => None: Option[E])
		override protected def construct(implicit res: Values) = ???

		override def scoutValue(ctx :ReferenceContext[this.type]) = Some(
			new BaseCompositeReference[T, E] with CompositeNavigableReference[T, E] with Scout {
				override def path :MappingPath[_, _] = ctx.path++mapping.path
				override def fetch: E = table.scoutValue.get
			}
		)
	}

	object Lovers extends StaticMapping[Lover] {

		val name = column("name", _.name)

//		override def mockValue(ctx: MappingContext[this.type]): Option[Lover] =
//			Some(Lover("JFK"))

		override protected def construct(implicit res: Values): Lover = Lover(name)
		override def sqlName = Some("Lovers")
	}

	object Hats extends StaticMapping[Hat] {
		val colour = column("colour", _.colour)
		val kind = column("kind", _.kind)

//		override def mockValue(ctx: MappingContext[this.type]): Option[Hat] = Some(Hat("black", "wide brim"))

		override protected def construct(implicit res: Values): Hat = Hat(colour, kind)
		override val sqlName = Some("Hats")
	}

	object Actresses extends StaticMapping[Actress] {

		val name = column("name", _.name)
		val lover = embed(_.lover, new MockRefMapping[Lover, Lover](Lovers))
		val favourites = new StaticComponent(_.favourites) {
			val hats = embed(_.hats, new MockRefMapping[Seq[Hat], Hat](Hats))

			override protected def construct(implicit res: Values): Favourites = Favourites(hats)
		}


		override protected def construct(implicit res: Values): Actress =
			Actress(name, lover, favourites)
//		override def mockValue(ctx :MappingContext[this.type]) = Som
		override val sqlName = Some("Actresses")
	}


	val pathfinder = Pathfinder.guild.enlist(Hats).enlist(Lovers).enlist(Actresses)
	"Pathfinder" should "correctly identify component property" in {
		pathfinder.get(PropertyChain[Hat](_.colour)) should equal(Some(Hats \\ Hats.colour))
	}

	"Pathfinder" should "correctly identify foreign key target" in {
		val lovers = pathfinder.get(PropertyChain[Actress](_.lover.fetch))
		lovers.map(_.end) should equal(Some(Lovers))
	}

	"Pathfinder" should "correctly identify to-many reference" in {
		val hats = pathfinder.get(PropertyChain[Actress](_.favourites.hats.fetch[Hat]))
		hats.map(_.end) should equal(Some(Hats))
	}


}
