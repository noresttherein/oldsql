package net.noresttherein.oldsql.morsels





/** Constructs an instance of $descO from a $descY. The latter can be specified instead as a constructor function
  * of an arbitrary arity `(`$X`*) => `$Y, where the same instance of $X is given for all arguments of the function.
  * Overloaded `apply` for various function arities allow to specify the constructor using the shortened lambda
  * syntax, referring to the argument $X an arbitrary number of times using the placeholder character `_`.
  * @define descO `O`
  * @define O `O`
  * @define descY `Y`
  * @define Y `Y`
  * @define X `X`
  * @author Marcin Mościcki
  */
abstract class PolyConstructor[X, Y, +O](private[morsels] val x :X) {
	def apply(y :Y) :O

	@inline final def apply(f :X => Y) :O = apply(f(x))
	@inline final def apply(f :(X, X) => Y) :O = apply(f(x, x))
	@inline final def apply(f :(X, X, X) => Y) :O = apply(f(x, x, x))
	@inline final def apply(f :(X, X, X, X) => Y) :O = apply(f(x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x, x, x, x, x))
	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X) => Y) :O = apply(f(x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))

	@inline final def apply(f :(X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X, X) => Y) :O =
		apply(f(x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x))
}



object PolyConstructor {
	def apply[X, Y, Z](arg :X)(f :Y => Z) :PolyConstructor[X, Y, Z] =
		new PolyConstructor[X, Y, Z](arg) {
			override def apply(y :Y) :Z = f(y)
		}
}




//class GenericFunctionApplicator[+X, Y[_], E[_] <: { type Result }](x :X) {
//	def apply[T](f :X => Y[T])(implicit result :E[T]) :result.Result
//}