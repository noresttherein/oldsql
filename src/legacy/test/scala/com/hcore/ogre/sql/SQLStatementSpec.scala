package com.hcore.ogre.sql

import com.hcore.ogre.mapping.Mapping.MappingExtension.AutoGen
import com.hcore.ogre.mapping.support.StaticMapping
import com.hcore.ogre.sql.RowSource.{WithParam, RowTables}
import com.hcore.ogre.sql.SQLFormula.SQLHNil
import org.scalatest.{Matchers, FlatSpec}


class SQLStatementSpec extends FlatSpec with Matchers {

	import SQLFormula.TermFormulas

	case class Whisky(id :Long, name :String, age :Int, country :String, strength :Double)


	object Whiskies extends StaticMapping[Whisky] {
		val id = column("id", _.id, AutoGen)
		val name = column("name", _.name)
		val age = column("age", _.age)
		val country = column("country", _.country)
		val strength = column("strength", _.strength)

		override protected def construct(implicit res: Values): Whisky =
			Whisky(id, name, age, country, strength)

		override def sqlName = Some("whiskies")
	}

	val Mortlach = Whisky(0, "Mortlach", 16, "Scotland", 42.5)

	"Delete" should "create delete all statement" in {
		val del = Delete all Whiskies
		del.sql should equal("delete from whiskies")
	}

	"Delete" should "create parameterized delete statement" in {
		val del = Delete.by[Long] from Whiskies where (t => t[Whiskies.type] \ (_.id) === t.?[Long])

//		val del2 = Delete from Whiskies on (_(_.age) === (_:)())
		del.sql should equal("delete from whiskies where whiskies.id=?")
		del.input.writtenColumns should equal(1)

//		val del2 = Delete from Whiskies on (_(_.id)===_(_.id))
//		del2.sql should equal(del.sql)
	}




	"Insert" should "create an insert entity statement" in {
		val ins = Insert into Whiskies
		ins.sql should equal("insert into whiskies(name, age, country, strength) values (?, ?, ?, ?)")
		ins.input.writtenColumns should equal(ins.sql.count(_=='?'))
	}

	"Insert" should "create insert statement returning requested columns" in {
		val ins = Insert into Whiskies
		val autoins = ins.returning
		autoins.sql should equal("insert into whiskies(name, age, country, strength) values (?, ?, ?, ?) returning id")
		autoins.input.writtenColumns should equal(autoins.sql.count(_=='?'))
//		autoins.output.writtenColumns should equal(1)

		val returning = ins returning Whiskies.name
		returning.sql should equal("insert into whiskies(name, age, country, strength) values (?, ?, ?, ?) returning name")
		returning.input.writtenColumns should equal(returning.sql.count(_=='?'))
//		returning.output.writtenColumns should equal(1)

	}


	"Insert" should "create an insert statement for selected columns" in {
		val ins = Insert(Whiskies)(_(_.name) := "Mortlach", _(_.age) := 16, _(_.country) := "Scotland", _(_.strength) := 42.5)
		ins.sql should equal("insert into whiskies(name, age, country, strength) values (?, ?, ?, ?)")
		ins.input.writtenColumns should equal(ins.sql.count(_=='?'))
	}

	"Insert" should "create an insert statement with a subselect" in {
		val ins = Insert(Whiskies)(_(_.name) := "Mortlach", _(_.country) := "Scotland", _(_.strength) := 42.5,
			_(_.age) := (From(Whiskies) whereLast (_(_.name)==="Mortlach".?) select(_.last(_.age))).single )

		ins.sql should equal("insert into whiskies(name, country, strength, age) values (?, ?, ?, (select whiskies.age from whiskies as whiskies where whiskies.name=?))")
		ins.input.writtenColumns should equal(ins.sql.count(_=='?'))
	}



	"Update" should "create an update all entities statement" in {
		val upd = (Update table Whiskies).all
		upd.sql should equal("update whiskies whiskies set name=?, age=?, country=?, strength=?")
		upd.input.writtenColumns should equal(upd.sql.count(_=='?'))

	}

	"Update" should "create an update statement returning requested columns" in {
		val returning = (Update table Whiskies).all returning (t => t[Whiskies.type](_.name)::t[Whiskies.type](_.id)::SQLHNil)
		returning.sql should equal("update whiskies whiskies set name=?, age=?, country=?, strength=? returning (name, id)")
		returning.input.writtenColumns should equal(returning.sql.count(_=='?'))
		returning.output.readColumns should equal(2)
	}

	"Update" should "create an update statement for selected columns" in {
		val upd = (Update table Whiskies).all.set(_.strength, _.age)
		upd.sql should equal("update whiskies whiskies set strength=?, age=?")
		upd.input.writtenColumns should equal(upd.sql.count(_=='?'))
	}

	"Update" should "create an update where statement" in {
		val upd = Update table Whiskies where (t => t.prev \ (_.id) === t.?[Whisky](_.id))
		upd.sql should equal("update whiskies whiskies set name=?, age=?, country=?, strength=? where whiskies.id=?")
		upd.input.writtenColumns should equal(upd.sql.count(_=='?'))
	}


	"Update" should "create an update for individual columns" in {
		val upd = Update table Whiskies on (_(_.id)===_(_.id)) setTo
			(t => t.prev \ (_.country) := t.?[Whisky](_.country)) setTo
			(t => t.prev \ (_.age) :=
				t.single(t from Whiskies where (t => t.last(_.id) === t.?[Whisky](_.id)) subselect (_.last \ (_.age))) )
		upd.sql should equal("update whiskies whiskies set age=(select whiskies1.age from whiskies as whiskies1 where whiskies1.id=?), country=? where whiskies.id=?")
		upd.input.writtenColumns should equal(3)
	}


}
