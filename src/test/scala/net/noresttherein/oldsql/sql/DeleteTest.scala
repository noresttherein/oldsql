package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{BaseTable, Table}
import net.noresttherein.oldsql.schema.bases.SimpleMapping
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.mechanics.RelationCount





/**
  * @author Marcin Mościcki
  */
object DeleteTest extends App {
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
//	val res = Monsters[From[Monsters]].toSQL substitute (_.name := "name")
//	res :Nothing
//	implicitly[RelationCount[From[Monsters], _ <: Numeral]]
//	def count[F <: RowProduct, N <: Numeral](from :F)(implicit count :RelationCount[F, N]) :count.type = count

//	count(From(Monsters))
//	Monsters[From[Monsters]].race.toSQL
//	val ms = Monsters[From[Monsters]]
//	println(scala.reflect.runtime.universe.reify(ms.race.toSQL))
//	Delete from Monsters where (_.race ==? "Dragon" && _.name ==? "Thaxll'sillyia")
//	Delete from Monsters where ((m1, m2) => m1.race.toSQL===(m2.name.toSQL)(SQLTypeUnification.directly[String]))
//	Delete from Monsters where { (m1 :Monsters[From[Monsters]], m2 :Monsters[From[Monsters]]) => m1.race.toSQL; m2.name.toSQL; True }
//	Delete from Monsters where { (m1) => m1.race.toSQL1; True }
//	Delete from Monsters where ((_, _) => True)
/*
	Delete from Monsters where (_.race ==? "Dragon")
	Delete from Monsters
	Delete all Monsters
*/

/*
//	implicitly[UnboundParam[String, RowProduct]]
	Delete(Monsters) where (_.level > 20.?) and (_.name === _(_.name))
	Delete(Monsters).by[String] where (_.race === _) //fixme: in Scala 3 apply should work
	Delete(Monsters).by[Monster] where (_.name === _(_.name))
	Delete(Monsters).by[Monster] where { (t, p) => t.race === p(_.race) }
	Delete(Monsters)
	Delete(Monsters).apply(firkraag)
	Delete(Monsters).apply(firkraag, belhifet)
	Delete(Monsters).apply(monsters)
//	import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.implicitLiteral
	Delete(Monsters) * 5 where (_.name === _(_.name)) and (_.race === "Red " ++: _(_.race))
	Delete(Monsters).by[String] * 5 where (_.name === _) or (_.name === "Firkraag")

	Delete(firkraag) from Monsters
	Delete(firkraag, belhifet) from Monsters
	Delete(monsters) from Monsters

	Delete one Monsters
	Delete one Monsters where (_.name === _(_.name))
	Delete many Monsters
	Delete many Monsters where (_.name === _(_.name))
*/

}
