import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Record.{#>, |#}
import net.noresttherein.oldsql.morsels.Origin.{@#, Index}
import net.noresttherein.oldsql.morsels.abacus.INT
import net.noresttherein.oldsql.schema.{AbstractSchemaMapping, GenericMapping, Mapping, MappingSchema}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.SchemaMapping.LabeledSchemaMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.sql.FromClause
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula



/**
  * @author Marcin Mościcki
  */
object playground extends App {
	type I = Index[N] forSome { type N <: INT }
	type M = GenericMapping[_, I] forSome { type I <: INT }
//	implicitly[MappingFrom[_ <: -1] <:< MappingFrom[-1]]
	val m :GenericMapping[_, _ <: T] = ???
	trait T
	m :GenericMapping[_, T]

//	val m :GenericMapping[Int, _ <: -1] = ???
//	m :GenericMapping[Int, Index[-1]]
//	val bool = m <= m
//	bool :BooleanFormula[FromClause]
//	high(new High[Lower] {})
	class J[L, R <: Mapping] {
		def as[A <: String with Singleton, M[O] <: MappingFrom[O]](alias :A)(implicit ev: R <:< M[_]) :J[L, M[A]] = ???
	}
//	abstract class C[O] extends GenericMapping[O, Int]
//	val alias = new J[C[Any], C[Any]] as "ala"
//	alias :J[C[Any], C["ala"]]
	abstract class C extends GenericMapping[Int, Any]
	val alias = new J[C, C] as "ala"
	alias :J[C, GenericMapping[Int, "ala"]]

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
			Human(!"gun", !"backup", !"second")
	}


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
