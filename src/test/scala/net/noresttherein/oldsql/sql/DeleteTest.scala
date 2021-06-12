package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.bases.SimpleMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.Relation.{BaseTable, Table}
import net.noresttherein.oldsql.sql.ComponentSetter.:=






/**
  * @author Marcin Mościcki
  */
class DeleteTest {
	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL
	case class Monster(name :String, race :String, level :Int)

	class Monsters[O] extends SimpleMapping[Monster, O] {
		val name = column(_.name)
		val race = column(_.race)
		val level = column(_.level)

		override def construct(implicit pieces :Pieces) =
			Monster(name, race, level)
	}

	val Monsters :BaseTable[Monsters] = Table("monsters", new Monsters[Unit])
	implicit val monsterForm = Monsters.row.selectForm <> Monsters.row.filterForm

	val firkraag = Monster("Firkraag", "Dragon", 23)
	val belhifet = Monster("Belhifet", "Devil", 25)
	val monsters = Seq(firkraag, belhifet)

	Delete from Monsters where (_.race ==? "Dragon" && _.name ==? "Thaxll'sillyia")
	Delete from Monsters where (_.race === _.name)
	Delete from Monsters
	Delete all Monsters

//	implicitly[FromParam[String, RowProduct]]
	Delete(Monsters) where (_.level > 20.?) and (_.name === _(_.name))
	Delete(Monsters).using[String] where (_.race === _) //fixme: in Scala 3 apply should work
	Delete(Monsters).using[Monster] where (_.name === _(_.name))
	Delete(Monsters).using[Monster] where { (t, p) => t.race === p(_.race) }
	Delete(Monsters)
	Delete(Monsters).apply(firkraag)
	Delete(Monsters).apply(firkraag, belhifet)
	Delete(Monsters).apply(monsters)
//	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.implicitLiteral
	Delete(Monsters) * 5 where (_.name === _(_.name)) and (_.race === "Red " ++: _(_.race))
	Delete(Monsters).using[String] * 5 where (_.name === _) or (_.name === "Firkraag")

	Delete(firkraag) from Monsters
	Delete(firkraag, belhifet) from Monsters
	Delete(monsters) from Monsters

	Delete one Monsters
	Delete one Monsters where (_.name === _(_.name))
	Delete many Monsters
	Delete many Monsters where (_.name === _(_.name))


	class InsertValue[X, Y] {
		def set(setter :(X, Y) => Seq[Any]) = ???
	}
	val insert = new InsertValue[Int, Int]

	insert set { (t, p) => Seq(t + p, t - p) }

}
