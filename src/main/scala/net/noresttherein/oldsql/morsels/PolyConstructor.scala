package net.noresttherein.oldsql.morsels





/** Constructs an instance of $descO from a $descY. The latter can be specified instead as a constructor function
  * of an arbitrary arity `(`$X`*) => `$Y, where the same instance of $X is given for all arguments of the function.
  * Overloaded `apply` for various function arities allow to specify the constructor using the shortened lambda
  * syntax, referring to the argument $X an arbitrary number of times using the placeholder character `_`.
  * Assume `where :PolyConstructor[Dragon, Boolean, Hamster :Boolean]`:
  * {{{
  *     val f = where(_.level == 10 && _.strength > _.intelligence) //Dragon => Boolean
  * }}}
  * @define descRes `Res`
  * @define Res `Res`
  * @define descY `Y`
  * @define Y `Y`
  * @define X `X`
  * @tparam X   the type of arguments taken by constructor function arguments of all `apply` methods.
  * @tparam Y   the type returned by constructor function arguments of all `apply` methods.
  * @tparam Res the output type, returned by all `apply` methods.
  * @author Marcin Mościcki
  */ //todo: rename to MultiApply
trait PolyConstructor[X, Y, +Res] { self =>
	import self.{arg => x}
	protected def arg :X

	def apply(item :Y) :Res

	@inline final def apply(f :X => Y) :Res = apply(f(x))
	@inline final def apply(f :(X, X) => Y) :Res = apply(f(x, x))
	@inline final def apply(f :(X, X, X) => Y) :Res = apply(f(x, x, x))
	@inline final def apply(f :(X, X, X, X) => Y) :Res = apply(f(x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X) => Y) :Res = apply(f(x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :Res =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))
}



object PolyConstructor {
	def apply[X, Y, Z](arg :X)(f :Y => Z) :PolyConstructor[X, Y, Z] = new Impl(arg)(f)

	private class Impl[X, Y, +Res](override val arg :X)(f :Y => Res) extends PolyConstructor[X, Y, Res] {
		override def apply(item :Y) :Res = f(item)
	}
}



abstract class MultiPolyConstructor[X, Y, +Res](x :X) extends PolyConstructor[X, Y, Res] {
	override def apply(item :Y) :Res = apply(item :: Nil)

	def apply(items :Seq[Y]) :Res

	def apply(first :X => Y, second :X => Y, rest :X => Y*) :Res =
		apply(first(x) +: second(x) +: rest.map(_(x)))

	def apply(first :(X, X) => Y, second :(X, X) => Y, rest :(X, X) => Y*) :Res =
		apply(first(x, x) +: second(x, x) +: rest.map(_(x, x)))
}




//class GenericFunctionApplicator[+X, Y[_], E[_] <: { type Result }](x :X) {
//	def apply[T](f :X => Y[T])(implicit result :E[T]) :result.Result
//}