package com.hcore.ogre.mapping.ties

import com.hcore.ogre.deeds.Haul
import com.hcore.ogre.mapping.TypedColumn
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.model.Reference
import com.hcore.ogre.model.Reference.{Satisfying, Unknown, Full}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.sql.{RowSource, SQLPrinter, From}
import org.scalatest.{Matchers, FlatSpec}


class ReferenceMappingSpec extends FlatSpec with Matchers {
	
	case class Liberal(id :Long, name :String, nemesis :Reference[Conservatist], hates :Reference[Seq[Conservatist]])
	
	case class Conservatist(id :Long, name :String, nemesis :Reference[Liberal], hates :Reference[Seq[Liberal]])
	
	object Liberals extends ConnectedMapping[Liberal] {
		val id = column("id", _.id)
		val name = column("name", _.name)
		val nemesis = foreignKey(_.nemesis)("nemesis", Conservatists \\ Conservatists.id)
		
		override protected def construct(implicit res: Values): Liberal = 
			Liberal(id, name, nemesis, ???)

		override def sqlName = Some("liberals")
	}
	
	object Conservatists extends ConnectedMapping[Conservatist] {
		val id = column("id", _.id)
		val name = column("name", _.name)
		val nemesis = inverse(_.nemesis, Liberals \\ Liberals.nemesis)
		override protected def construct(implicit res: Values): Conservatist =
			Conservatist(id, name, nemesis, ???)

		override def sqlName = Some("conservatists")
	}

	type Hates = (Reference[Liberal], Reference[Conservatist])

	object Haters extends ConnectedMapping[Hates] {
		val _1 = foreignKey(_._1)("left", Liberals \\ Liberals.id)
		val _2 = foreignKey(_._2)("right", Conservatists \\ Conservatists.id)

		override protected def construct(implicit res: Values): (Reference[Liberal], Reference[Conservatist]) = 
			(res(_1), res(_2))

		override def sqlName = Some("haters")
	}


//	val pathfinder = Pathfinder.guild.enlist(Liberals).enlist(Conservatists).enlist(Haters)

	"ForeignKeyReferenceMapping" should "include key columns in last mapping's column list" in {
		Liberals.columns.flatMap(_.sqlName).toSet should equal(Set("id", "name", "nemesis"))
	}


	"ReferenceMapping" should "create join path for a foreign key" in {
		val Some(path) = Scout(Liberals \\ Liberals.nemesis)
		path.start should equal(Liberals)
		path.end should equal(Conservatists)

		val selects = Haul[Liberals.type, Liberal](Liberals).fetch(path).sources
		selects.size should equal(1)
		val select = selects.head
		select.mappings.toSet should equal(Set(Liberals, Conservatists))
		val expected = From(Liberals) join Conservatists where (t => t[Conservatists.type] \ (_.id) === t[Liberals.type] \ (_.nemesis) \ (_.key))
		(select.filter equivalent  expected.filter) should equal(true)
	}

	"ReferenceMapping" should "retrieve foreign key value from an entity" in {
		import com.hcore.ogre.model.Restriction.Restrictive
		val ref = Satisfying(((_:Conservatist).id) === 1).single
		val liberal = new Liberal(0, "me", Full(Conservatist(1, "nemesis", Unknown(), Unknown())), Unknown())
		Liberals(_.nemesis)(_.key)(liberal) should equal(Some(1))
		Liberals(_.nemesis)(_.key)(liberal.copy(nemesis = ref)) should equal(Some(1))
	}


	"ReferenceMapping" should "create join path for foreign key inverse mapping" in {
		val Some(path) = Scout(Conservatists \\ Conservatists.nemesis)
		path.start should equal(Conservatists)
		path.end should equal(Liberals)

		val selects = Haul[Conservatists.type, Conservatist](Conservatists).fetch(path).sources
		selects.size should equal(1)
		val select = selects.head
		select.mappings.toSet should equal(Set(Liberals, Conservatists))
		val expected = From(Conservatists) join Liberals where (t => t[Liberals.type] \ (_.nemesis) \ (_.key) === t[Conservatists.type] \ (_.id))
		SQLPrinter(select.subselectAll.asSQL[Nothing]) should equal(SQLPrinter(expected.selectAll))
//		(select.filter equivalent  expected.filter) should equal(true)

	}

	"ReferenceMapping" should "retrieve inverse foreign key (primary key) value from an entity" in {
		import com.hcore.ogre.model.Restriction.Restrictive
		val ref = Satisfying(((_:Conservatist).id) === 1).single
//		val ref = Satisfying(((_:Liberal).nemesis) === Satisfying((_:Conservatist).id===1).single).single
		val conservatist = new Conservatist(1, "me", Full(Liberal(0, "nemesis", ref, Unknown())), Unknown())
		Conservatists.nemesis.columns.size should equal(1)

		def columnValue(c :Conservatist) =
			Conservatists(_.nemesis)(_.columns.head)(c)

		columnValue(conservatist) should equal(Some(1))
		columnValue(conservatist.copy(nemesis = Satisfying(((_:Liberal).nemesis)===ref).single)) should equal(Some(1))
//		Conservatists(_.nemesis)(_.backer.key).apply(conservatist) should equal(Some(1))
//		Conservatists(_.nemesis)(_.backer.key)(conservatist.copy(nemesis = Satisfying(((_:Liberal).nemesis)===ref).single)) should equal(Some(1))
	}

	
}
