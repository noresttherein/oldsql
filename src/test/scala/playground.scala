
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
}
