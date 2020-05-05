import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Record.{#>, |#}
import net.noresttherein.oldsql.morsels.Origin.Rank
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{AbstractSchemaMapping, GenericMapping, Mapping, MappingSchema}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.SchemaMapping.LabeledSchemaComponent
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.MappingPath.SelfPath
import net.noresttherein.oldsql.sql.{FromClause, SQLOrdering}
import net.noresttherein.oldsql.sql.SQLFormula.{BooleanFormula, SQLTypePromotion}



/**
  * @author Marcin Mościcki
  */
object playground extends App {

	case class Gun(make :String, model :String, caliber :Double)
	case class Human(gun :Gun, backup :Gun, secondBackup :Gun)

	def guns[O] = MappingSchema[Gun, O].col(_.make).col(_.model).col(_.caliber).map(Gun.apply _)
	def humans[O] =
		MappingSchema[Human, O].comp(_.gun, guns[O].:@["gun"]).comp(_.backup, guns[O].:@["backup"]).comp(_.secondBackup, guns[O].:@["second"])
//		MappingSchema[O, Human].comp(_.gun, guns[O]).comp(_.backup, guns[O]).comp(_.secondBackup, guns[O])

	class Humans[O] extends AbstractSchemaMapping(humans[O]) {
		val gun = schema / 0
		val backup = schema / 1
		val second = schema / 2
		gun :LabeledMapping["gun", Gun, O]
		backup :LabeledMapping["backup", Gun, O]
		second :LabeledMapping["second", Gun, O]
		override protected def construct(implicit pieces :Pieces) :Human =
			Human(~"gun", ~"backup", ~"second")
	}

	trait Base {
		def m :String

		val x = m
		if (x == null)
			println ("null :(")
	}

	class Sub(val m :String = "hey") extends Base {

	}

	new Sub

//	new Humans[Any].forSelect[@~ ~ "backup"]:Int
//	guns[Any].forSelect[@~ ~ 0 ~ 2 ~ 1] :Int
//	val path = SelfPath(new Humans[Any])
//	val Backup = (new Humans[Any]) \ (_.gun)
//	import net.noresttherein.oldsql.collection.Record._

//	trait Tagged[+T]

//	class TaggedAny(val inner :Tagged[T]) extends Tagged

	val record =   "key1" #> 1 |# "key2" #> 2 |# "key3" #> 3 : @~ |# ("key1", Int) |# ("key2", Int) |# ("key3", Int)

	println(record)

	record match {
//		case @~ &| key1 :-> val1 &| key2 :-> val2 &| key3 :-> val3 =>
		case key1 #> val1 |# key2 #> val2 |# key3 #> val3 =>
			println(Seq(key1 -> val1, key2 -> val2, key3 -> val3))
		case _ =>
			println("???")
	}
//	val list = record.toList
	val seq = record.toSeq[(String with Singleton, Int)]
//	val seq = (record :Record |# ("key2", Int) |# ("key3", Int)).toSeq//[(String with Singleton, Any)]
//	val map = record.toSeq[(String, Int)].toMap
//	import UpperIndexBound._
//	val map = (record :Record |# ("key2", Int) |# ("key3", Int)).toMap//[String with Singleton, Any]//(upperBound(upperBound(NoUpperBound)))//[String, Any]
	val map = record.toMap[String with Singleton, Int]
//	list :List[(String, Int)]
//	seq :Int
//	seq :Seq[(String, Int)]
//	map :Int
//	map :Map[String, Int]

	println(seq)
//	println(map)

	println(record("key2"))
	println(record("key2") = "value2")
	println(record("key5") = 5)
	//	implicit class SomeTable[O](val name :String) extends MappingFrame[O, Int] {
//		override protected def construct(implicit pieces :Pieces) :Int = 1
//	}
//
//	//	val T = Table("table", new SomeTable("table"))
//	val T = Table[SomeTable]("table")
//	val I = Table.of[Int]("table")
//
//	val name :String = I[Long].name
}
