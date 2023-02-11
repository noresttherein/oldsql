package com.hcore.ogre.deeds

import com.hcore.ogre.mapping.Mapping.ReferenceContext
import com.hcore.ogre.mapping.MappingPath.\~\
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.bits.OptionMapping.DirectOptionMapping
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.mapping.ties.{JoinPath, Scout}
import com.hcore.ogre.model.ComposedOf.Arity
import com.hcore.ogre.model.Reference
import com.hcore.ogre.model.Reference.Unknown
import com.hcore.ogre.sql.From
import org.scalatest.{FlatSpec, Matchers}


class HaulSpec extends FlatSpec with Matchers {
	
	trait Entity {
		val id :Int
	}
	
	abstract class EntityMapping[T<:Entity] extends StaticMapping[T] {
		val id = column("id", _.id) 
	} 
	
	trait IntRef[T<:Entity] extends Reference[T] {
		val key :Int = 0
		def toOpt = None
	}
	
	class FK[T<:Entity](references : =>EntityMapping[T]) extends StaticMapping[IntRef[T]] { comp =>
		lazy val target = references
//		lazy val join = From[this.type](this).join[target.type](target) on (_(_.fk)===_(_.id))
		lazy val join = From[target.type](target).join[this.type](this) on (_(_.id)===_(_.fk))
		lazy val path = JoinPath[this.type, target.type](join, (_:IntRef[T]).toOpt, Arity._1)
		val fk = column("fk", _.key)

		def inversePath[R<:Mapping](owner :ComponentPath[R, this.type]) = {
			val inverseJoin = From[target.type](target).join[R](owner.start) where (t => t[R] \\ owner :\ fk === t[target.type] \ (_.id))
			JoinPath(inverseJoin, (_:Any) => None, Arity._0_n)
		}
		
		override protected def construct(implicit res: Values): IntRef[T] = new IntRef[T]{ override val key = fk :Int }

		override def scoutValue(ctx: ReferenceContext[this.type]): Option[IntRef[T]] =
			Some(new IntRef[T] with Scout {
				override def path :MappingPath[_, _] = ctx.path ++ comp.path
			})
	}


//	class InverseFK[T<:Entity,]

	
	trait HasOne[T<:Entity] extends Entity {
		val ref :Reference[T]
	}
	
	trait HasMany[T<:Entity] extends Entity {
		val ref :Reference[Seq[T]]
	}
	
	
	class Chain(val id :Int, val next :IntRef[Chain]) extends Entity
	
	class Chains[C>:Null <:Chains[_] ](name :String, target : =>C) extends EntityMapping[Chain] {
		val next = new FK[Chain](target) with Component[IntRef[Chain]] {
			val pick = (x :Chain) => Some(x.next)
			val surepick = Some((_:Chain).next)
		}
//		val next = embed(_.next, new FK[Chain](target))

		def join :this.type\~\C = next.scoutValue(new ReferenceContext(this) :\ next) match {
			case Scout(path) => path.asInstanceOf[this.type\~\C]
		}

		override protected def construct(implicit res: Values): Chain = new Chain(id, next)
		
		override def sqlName = Some(name) 
	}

	lazy val chains4 = new Chains[Null]("chains4", null)
	lazy val chains3 = new Chains[chains4.type]("chains3", chains4)
	lazy val chains2 = new Chains[chains3.type]("chains2", chains3)
	lazy val chains1 = new Chains[chains2.type]("chains1", chains2)


//	lazy val chains4 :Chains = new Chains("chains4", chains1)
//	class InverseFK[T<:Entity](references : =>EntityMapping[T], fk)
	
	
	"Haul" should "join several tables in one query" in {

		val expectedJoin = From(chains1) join
			chains2 where (t => t.last(_.id) === t.prev \ (_.next) \ (_.fk)) join
			chains3 where (t => t.last(_.id) === t.prev \ (_.next) \ (_.fk)) join
			chains4 where (t => t.last(_.id) === t.prev \ (_.next) \ (_.fk))

		val haul = Haul[chains1.type, Chain](chains1)

		val oneGo = haul.fetch(chains1.join ++ chains2.join ++ chains3.join ++ (chains4 \\ chains4.id))

		val additive = haul fetch (chains1.join ++ (chains2 \\ chains2.next)) fetch
			(chains1.join ++ chains2.join) fetch
			(chains1.join ++ chains2.join ++ chains3(_.id)) fetch
			(chains1.join ++ chains2.join ++ chains3.join ++ chains4(_.id))

		val random = haul fetch (chains1.join ++ chains2.join) fetch
			(chains1.join ++ chains2(_.id)) fetch
			(chains1.join ++ chains2.join ++ chains3.join ++ chains4(_.next)) fetch
			(chains1.join ++ chains2.join ++ chains3(_.next)) fetch
			(chains1.join ++ chains2.join ++ chains3(_.id))
//		System.err.println(oneGo.mainSource)
//		System.err.println(expectedJoin)
//		(oneGo.mainSource equivalent expectedJoin) should equal(true)
//		(additive.mainSource equivalent expectedJoin) should equal(true)
//		(random.mainSource equivalent expectedJoin) should equal(true)
		oneGo.mainSource should equal(expectedJoin)
		additive.mainSource should equal(expectedJoin)
		random.mainSource should equal(expectedJoin)
	}

	class ChainsRec(name :String, target : =>ChainsRec) extends Chains[ChainsRec](name, target) {
		def rec = join.cast[ChainsRec, ChainsRec]
	}

	lazy val chainrec1 :ChainsRec = new ChainsRec("chainrec1", chainrec2)
	lazy val chainrec2 :ChainsRec = new ChainsRec("chainrec2", chainrec1)

	"Haul" should "join the same last multiple times" in {
		val haul = Haul[ChainsRec, Chain](chainrec1)
		val path = chainrec1.rec ++ chainrec2.rec ++ chainrec1.rec //++ chainrec2.rec
		val three = haul fetch path

		val expected = From(chainrec1) join chainrec2 on ((l, r) => r(_.id)===l(_.next)(_.fk)) join chainrec1 on ((l, r) => r(_.id)===l(_.next)(_.fk)) join chainrec2 on ((l, r) => r(_.id)===l(_.next)(_.fk))
//		System.err.println(s"expected: $expected")
//		System.err.println(s"fetched : ${three.mainSource}")
		three.mainSource should equal(expected)
//		(three.mainSource equivalent expected) should equal(true)
	}



	class RefStream(val id :Int, val next :Option[IntRef[RefStream]]) extends Entity

	object Streams extends EntityMapping[RefStream] {
		val next = new BaseComponent(_.next) with DirectOptionMapping[IntRef[RefStream], FK[RefStream]] {
			val get = new FK[RefStream](Streams)
		}

		lazy val fetch = ((this \\ next :\ next.get) ++ next.get.path).cast[Streams.type, Streams.type]

		override protected def construct(implicit res: Values): RefStream = new RefStream(id, next)
	}

	"Haul" should "perform outer join when necessary" in {
		val haul = Haul[Streams.type, RefStream](Streams)
		val path = Streams.fetch ++ Streams.fetch
		val join = haul.fetch(path).mainSource

		val expected = From(Streams) leftJoin Streams on ((l, r) => r(_.id)===l(_.next)(_.get)(_.fk)) leftJoin Streams on ((l, r) => r(_.id)===l(_.next)(_.get)(_.fk))
		join should equal(expected)
	}



	case class Tree(id :Int, parent :Option[IntRef[Tree]], children :Reference[Seq[Tree]]) extends Entity

	object Trees extends EntityMapping[Tree] {
		val parent = new BaseComponent(_.parent) with DirectOptionMapping[IntRef[Tree], FK[Tree]] {
			val get = new FK[Tree](Trees)
		}

//		val children =

		lazy val toParent = ((this \\ parent :\ parent.get) ++ parent.get.path).cast[Trees.type, Trees.type]
		lazy val toChildren = parent.get.inversePath(this \\ parent :\ parent.get).asInstanceOf[JoinPath[Trees.type, Trees.type]]

		override protected def construct(implicit res: Values): Tree = Tree(id, parent, Unknown())
	}

	"Haul" should "perform subselect joins as separate queries" in {
		val haul = Haul[Trees.type, Tree](Trees)
		val multi = haul fetch Trees.toParent fetch Trees.toChildren fetch (Trees.toChildren ++ Trees.toChildren) fetch (Trees.toParent++Trees.toChildren)

		val mainSource = From(Trees) leftJoin Trees on (_(_.parent)(_.get)(_.fk)===_(_.id))
		val childrenSubselect = mainSource join Trees where (t => t.last(_.parent)(_.get)(_.fk) === t.source.left.left.last(_.id))
		val grandchildrenSubselect = childrenSubselect join Trees where (t => t.source.prev \ (_.id) === t.last(_.parent)(_.get)(_.fk))
		val siblingsSubselect = mainSource join Trees where (t => t.source.prev \ (_.id) === t.last(_.parent)(_.get)(_.fk))

		val expected = Seq(mainSource, childrenSubselect, grandchildrenSubselect, siblingsSubselect)

//		for ((l, r)<- expected zip multi.sources) {
//			System.err.println(r)
//			System.err.println(l)
//			System.err.println(r.all)
//			System.err.println(l.all)
//			System.err.println(r.filter)
//			System.err.println(l.filter)
//			System.err.println
//		}

		multi.sources.size should equal(expected.size)
		expected.foreach { benchmark =>
			multi.sources.exists(s => s.size==benchmark.size && (s.filter equivalent benchmark.filter)) should equal(true)
		}
	}



//	class Streams(val)
}
