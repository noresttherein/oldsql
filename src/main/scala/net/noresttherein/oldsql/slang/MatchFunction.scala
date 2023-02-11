package net.noresttherein.oldsql.slang

import scala.Specializable.{Arg, Return}






/** A multi-purpose extractor for values of `Out` from parameter `In`. Can be used in pattern matching or
  * as a partial function. It is a single abstract method type (''SAM'') and thus the compiler will convert a lambda
  * function `In => Out` where the type `MatchFunction[In, Out]` is expected, eliminating the overhead of wrapping
  * a function while preserving the convenience of the shortened definition.
  * @tparam In the argument type.
  * @tparam Out the type of extracted value.
  */
trait MatchFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	extends PartialFunction[In, Out] with (In => Out) //for specialization
{
	def unapply(in :In) :Option[Out]

	override def isDefinedAt(x: In): Boolean = unapply(x).isDefined

	override def apply(x: In): Out = unapply(x) getOrElse {
		throw new NoSuchElementException(s"$this($x)")
	}

	override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
		unapply(x) getOrElse default(x)

	override def toString :String = this.abbrevClassName
}



/** Companion object for [[net.noresttherein.oldsql.slang.MatchFunction! MatchFunction]] extractors,
  * providing several factory methods.
  */
object MatchFunction {
	/** Turn a given function returning an `Option[Out]` for input values `In` into an extractor
	  * that can be used in pattern matching or as a partial function.
	  * @tparam In  the argument type.
	  * @tparam Out the extracted result type.
	  * @param f function extracting `Out` values from `In` arguments.
	  * @return a partial function extractor wrapping the given function `f`.
	  */
	def unlift[@specialized(Arg) In, @specialized(Return) Out](f :In => Option[Out]) :MatchFunction[In, Out] =
		new OptionFunction(f, s"MatchFunction(${f.innerClassName})")

	/** An equivalent of [[net.noresttherein.oldsql.slang.MatchFunction.unlift unlift]] for those not into neologisms.
	  * Turns a given function returning an `Option[Out]` for input values `In` into an extractor
	  * that can be used in pattern matching or as a partial function.
	  * @tparam In  the argument type.
	  * @tparam Out the extracted result type.
	  * @param f function extracting `Out` values from `In` arguments.
	  * @return a partial function extractor wrapping the given function `f`.
	  */
	@inline def lower[@specialized(Arg) In, @specialized(Return) Out](f :In => Option[Out]) :MatchFunction[In, Out] =
		unlift(f)


	/** Adapts a partial function `X => Y` to a [[net.noresttherein.oldsql.slang.MatchFunction MatchFunction]].
	  * The application is split into two steps in order to provide an explicit argument type parameter first,
	  * with the return type being inferred.
	  */
	@inline def partial[@specialized(Arg) In] :Match.AdaptPartialFunction[In] = Match[In]


	/** Forces a function literal of type `(In, In => Out) => Out` to become an instance
	  * of [[net.noresttherein.oldsql.slang.MatchFunction MatchFunction]] using it
	  * as its [[net.noresttherein.oldsql.slang.MatchFunction!.applyOrElse applyOrElse]] method, to which other methods
	  * delegate. Note that, due to a limitation of implementation, the result type `Out` is still boxed, so the benefit
	  * could be seen only in reference types and simple functions.
	  */
	@inline final def applyOrElse[@specialized(Arg) In, Out](applyOrElse :ApplyOrElse[In, Out]) :MatchFunction[In, Out] =
		applyOrElse

	/** A ''single abstract method'' subtype of [[net.noresttherein.sugar.matching.MatchFunction MatchFunction]]
	  * requiring from subclasses an implementation of [[PartialFunction]]'s
	  * [[net.noresttherein.oldsql.slang.MatchFunction.applyOrElse applyOrElse]] method
	  * (as the abstract [[net.noresttherein.oldsql.slang.MatchFunction.ApplyOrElse.getOrElse getOrElse]]
	  * method). Used primarily in conjunction with
	  * `MatchFunction.`[[net.noresttherein.oldsql.slang.MatchFunction.partial partial]] to create instances
	  * of `MatchFunction` which do not resort to intermediate boxing to `Option` in that method.
	  */
	trait ApplyOrElse[@specialized(Arg) -In, +Out] extends MatchFunction[In, Out] {

		protected def getOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1
		final override def applyOrElse[A1 <: In, B1 >: Out](x :A1, default :A1 => B1) :B1 =
			getOrElse(x, default)

		final override def unapply(a :In) :Option[Out] = {
			val z = getOrElse(a, Fallback.downcastParams[In, Out])
			if (z.asAnyRef eq Fallback) Some(z) else None
		}
	}
	private[this] final val Fallback :Any => Any = _ => Fallback

	private class OptionFunction[@specialized(Specializable.Arg) -In, @specialized(Specializable.Return) +Out]
	                            (f :In => Option[Out], override val toString :String)
		extends MatchFunction[In, Out]
	{
		override def lift :In => Option[Out] = f
		override def unapply(in: In): Option[Out] = f(in)
//		override def pattern :MatchPattern[In, Out] = MatchPattern(f)
	}
}



/** Adapts a partial function `X => Y` to a [[net.noresttherein.oldsql.slang.MatchFunction MatchFunction]].
  * @see [[net.noresttherein.oldsql.slang.Match.apply]]
  */
object Match {
	/** Adapts a partial function `X => Y` to a [[net.noresttherein.oldsql.slang.MatchFunction MatchFunction]].
	  * The application is split into two steps in order to provide an explicit argument type parameter first,
	  * with the return type being inferred.
	  * @return an object with method
	  *         [[net.noresttherein.oldsql.slang.Match.AdaptPartialFunction.apply apply]]`(f :PartialFunction[X, Y])`
	  *         returning a `MatchFunction[X, Y]`.
	  */
	def apply[@specialized(Specializable.Arg) X] :AdaptPartialFunction[X] = new AdaptPartialFunction[X] {}

	sealed trait AdaptPartialFunction[@specialized(Specializable.Arg) X] extends Any {
		@inline def apply[@specialized(Specializable.Return) Y](f :PartialFunction[X, Y]) :MatchFunction[X, Y] =
			new MatchFunction[X, Y] {
				override val lift = f.lift
				override def unapply(a :X) :Option[Y] = lift(a)
				override def apply(a :X) :Y = f(a)
				override def applyOrElse[A1 <: X, B1 >: Y](x :A1, default :A1 => B1) :B1 = f.applyOrElse(x, default)
				override def isDefinedAt(x :X) :Boolean = f.isDefinedAt(x)
//				override def pattern = MatchPattern(lift)
			}
	}
}
