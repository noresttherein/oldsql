import net.noresttherein.oldsql.schema.RowSchema
import net.noresttherein.oldsql.schema.RowSource.Table


/**
  * @author Marcin Mościcki
  */
object playground extends App {
	abstract class Base {
		val string :String
		val prefixed = "prefix" + string

		override def toString = prefixed
	}

	class Subclass(val string :String) extends Base

	println(new Subclass("string"))



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
