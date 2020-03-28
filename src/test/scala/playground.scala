import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Record.{|#, #>}
import net.noresttherein.oldsql.collection.Record
import net.noresttherein.oldsql.schema.{RowSchema, SQLForm}
import net.noresttherein.oldsql.schema.RowSource.Table



/**
  * @author Marcin Mościcki
  */
object playground extends App {

//	import net.noresttherein.oldsql.collection.Record._

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
	//	implicit class SomeTable[O](val name :String) extends RowSchema[O, Int] {
//		override protected def construct(implicit pieces :Pieces) :Int = 1
//	}
//
//	//	val T = Table("table", new SomeTable("table"))
//	val T = Table[SomeTable]("table")
//	val I = Table.of[Int]("table")
//
//	val name :String = I[Long].name
}
