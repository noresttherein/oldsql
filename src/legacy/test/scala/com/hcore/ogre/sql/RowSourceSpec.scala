package com.hcore.ogre.sql

import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.sql.RowSource.SubsourceOf
import com.hcore.ogre.sql.SQLFormula.SelectFormula
import org.scalatest.{FlatSpec, Matchers}


class RowSourceSpec extends FlatSpec with Matchers {

	case class Rec(id :Long, fk :Long)

	import com.hcore.ogre.sql.SQLFormula.TermFormulas

	object Recs extends StaticMapping[Rec] {
		override val sqlName=Some("recs")

		val id = column("id", _.id)
		val fk = column("fk", _.fk)

		override protected def construct(implicit res: Values): Rec = Rec(id, fk)
	}

	"RowProduct" should "preserve equality of member tables when joined with other tables" in {
		val j1 = From(Recs)
		val j2 = j1 join Recs
		val j3 = j2 where (t => t.last(_.id)===t.prev \ (_.fk))
		val j4 = j3 join Recs on (_(_.fk)===_(_.id))
		val j5 = j4 where (t => t.last(_.fk) === j1.last \ (_.id))

		val first = Seq(j1.last, j2.prev, j2.left.last, j3.prev, j3.left.last, j4.left.prev, j4.left.left.last, j5.left.prev, j5.left.left.last)
		for (t1<-first; t2<-first) {
			t1 should equal(t2)
		}

		val second = Seq(j2.last, j3.last, j4.prev, j4.left.last, j5.prev, j5.left.last)
		for (t1<-first; t2<-first) {
			t1 should equal(t2)
		}
	}

	"RowProduct" should "treat different aliases for the same last in the join as different" in {
		val join = From(Recs) join Recs join Recs
		val tables = join.all.zipWithIndex
		for ((t1, i1)<-tables; (t2, i2)<-tables) {
			if (i1==i2) t1 should equal(t2)
			else t1 should not equal(t2)
		}
		join.last should not equal(join.prev)
		join.last should not equal(join.left.last)
		join.last should not equal(join.left.prev)
		join.last should not equal(join.left.left.last)

		join.left.last should not equal(join.left.prev)
		join.left.last should not equal(join.left.left.last)
	}


	"RowProduct" should "join two sources" in {
		val s1 = From(Recs) join Recs on (_(_.fk)===_(_.id)) join Recs on (_(_.id)===_(_.fk))

		(s1 joinAll Dual) should equal(s1)
		(s1 joinAny Dual) should equal(s1)

		val s2 = From(Recs) where (_.last(_.id) === 5L)
		val s1_s2 = s1 join Recs where (_.last(_.id)===5L)
		(s1 joinAll s2) should equal(s1_s2)
		(s1 joinAny s2) should equal(s1_s2)

		val s3 = From(Recs) join Recs on (_(_.id)===_(_.id)) where (_.last(_.fk)===10L)
		val s1_s3 = s1 join Recs join Recs on (_(_.id)===_(_.id)) where (_.last(_.fk)===10L)
		(s1 joinAll s3) should equal(s1_s3)
		(s1 joinAny s3) should equal(s1_s3)

	}


	"RowProduct" should "create select statement for one of it's tables" in {
		val s = From(Recs) join Recs on (_(_.fk)===_(_.id)) join Recs on (_(_.id)===_(_.fk))

		val select = s.selectLast

		SQLPrinter(select) should equal("select recs2.id, recs2.fk from recs as recs, recs as recs1, recs as recs2 where (recs.fk=recs1.id and recs1.id=recs2.fk)")
//		System.err.println(s)
//		System.err.println(select.queryString)
	}


	"RowProduct" should "correctly replace a mapping with parameters" in {
		val s = From(Recs) join Recs on (_(_.fk)===_(_.id))

		val first = s.substitute(new Rec(1, 2))
		val expectedFirst = From(Recs) where (_.last(_.fk)===1L.?)

//		(first isomorphic expectedFirst) should equal(true)
		first should equal(expectedFirst)

		val second = s.substitute(s.prev, new Rec(42, 44))
		val expectedSecond = From(Recs) where (t => 44L.? === t.last(_.id))
		second should equal(expectedSecond)
	}


	"RowProduct" should "add and fill synthetic parameter mapping" in {
		val s = From(Recs).withParam[(Long, Long)] where {
			t => t.?[(Long, Long)](_._1) === t[Recs.type](_.id) && t.?[(Long, Long)](_._2) === t[Recs.type](_.fk)
		}

		val parameterized = s.substitute((11, 4))
		val expected = From(Recs) where ( t => 11L.? === t.last(_.id) && 4L.? === t.last(_.fk) )
		parameterized should equal(expected)
//		val query = SQLExpressionPrinter(parameterized)
//		System.err.println(query)
//		System.err.println(parameterized.filter.parameters)
	}




}
