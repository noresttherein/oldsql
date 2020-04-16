package net.noresttherein.oldsql.collection

import net.noresttherein
import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection
import net.noresttherein.oldsql.collection.Chain.{@~, ~, UpperBound}
import net.noresttherein.oldsql.collection.LiteralIndex.{&~, IndexGet, IndexSet}
import net.noresttherein.oldsql.morsels.generic.{Const, GenericFun, Self}
import net.noresttherein.oldsql.morsels.LUB

import scala.annotation.{implicitNotFound, tailrec}


/** A list-like collection of fixed length, with the types of all its elements encoded in its type.
  * This is a very limited variant of ''shapeless'' `HList`, but which is extended from the end, rather than the front,
  * in opposition to traditional immutable lists. This stems from the intent of using it in conjunction with
  * `FromClause`, with `With` being likewise left-associative, which makes the elements of both appear in the exact same
  * order, rather than reversed as an ease-of-life feature.
  *
  * Chain instances are, like lists, typically built by extending the empty chain `@~`:
  * {{{
  *     val values = @~ ~ 1 ~ "next" ~ 4.2
  *     values :@~ ~ Int ~ String ~ Double
  * }}}
  *
  * Aside from the traditional empty and non-empty subclasses, there are also subtypes of `Chain` with specific type
  * bounds, making possible enforcing a desired invariant without resorting to implicit witnesses.
  * @see [[net.noresttherein.oldsql.collection.Chain.@~]]
  * @see [[net.noresttherein.oldsql.collection.Chain.~]]
  * @see [[net.noresttherein.oldsql.collection.LiteralIndex]]
  * @see [[net.noresttherein.oldsql.collection.Record]]
  * @author Marcin Mościcki
  */
sealed trait Chain extends Serializable {
	def isEmpty :Boolean = true
}



object Chain extends ChainFactory {
	override type Type = Chain
	override type Item = Any
	override type NonSingleton = Any
	override type Link[+I <: Chain, +L] = I ~ L

	override def link[I <: Chain, L <: Any](init :I, last :L) :I ~ L = new ~(init, last)



	/** Non-empty `Chain` implementation consisting of the 'tail' `I` (or rather, `init`, considering its 'reversed'
	  * nature) and 'head' `L` (the last element of the chain). There is an implicit conversion available
	  * to `ChainOps` which actually defines the methods for all chain operations, in order for it to work both
	  * for empty and non-empty chains and be able to provide type invariant implementations.
	  * This is also the base class of all non-empty `Chain` subclasses (variants with different type bounds).
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	sealed case class ~[+I <: Chain, +L](init :I, last :L) extends Chain {
		def get :(I, L) = init -> last

		override def isEmpty = false

		override def toString :String = {
			def entry(sb :StringBuilder, e :Any) :StringBuilder = e match {
				case _ : Chain => sb append "(" append e append ")"
				case _ => sb append e
			}
			def rec(chain :Chain) :StringBuilder = chain match {
				case t ~ h => entry(rec(t) append "~", h)
				case _ => new StringBuilder append chain
			}
			rec(this).toString
		}
	}

	/** The type of all empty chains, with a single member being its companion object
	  * [[net.noresttherein.oldsql.collection.Chain.@~$]].
	  */
	sealed class @~ private[Chain] extends Record

	/** An empty `Chain`, which is also an instance of every `Chain` variants defined here. */
	case object @~ extends @~ {
		def unapply(chain :Chain) :Boolean = chain.isInstanceOf[@~]
	}



	@inline implicit class ChainOps[C <: Chain](private val self :C) extends AnyVal {
		/** Adds a new element at the end of the chain. */
		@inline def ~[N](next :N) :C ~ N = new ~(self, next)

		/** Maps this chain using the given generic (polymorphic) function.
		  * @tparam XF type constructor which forms the type of every element in this chain. If this chain contains
		  *            elements without a common type constructor, [[net.noresttherein.oldsql.morsels.generic.Self]]
		  *            can be used which is an identity functor for types.
		  * @tparam YF the type constructor which is being applied to every argument of functor `XF` in this chain.
		  * @tparam Y the type of the transformed chain, which is the result of replacing every top-level application
		  *           of `XF` on every element of this chain with the type constructor `YF`.
		  */
		@inline def map[XF[T], YF[T], Y <: Chain](f :GenericFun[XF, YF])(implicit result :MapChain[XF, C, YF, Y]) :Y =
			result(f)(self)

		/** Applies the given function to every element of this chain for the side effects. */
		def foreach(f :GenericFun[Self, Const[Unit]#T]) :Unit = {
			def rec(chain :Chain) :Unit = chain match {
				case t ~ h => rec(t); f(h)
				case _ => ()
			}
			rec(self)
		}


		/** Appends the given chain to the end of this chain. */
		def ++[S <: Chain, R <: Chain](suffix :S)(implicit concat :ChainConcat[C, S, R]) :R =
			concat(self, suffix)

		/** Given a function `F` accepting the same number of arguments, of the same types and in the same order
		  * as the elements of this chain, apply it to the elements of this chain and return the result.
		  */
		@inline def feedTo[F, Y](f :F)(implicit application :ChainApplication[C, F, Y]) :Y = application(f, self)


		/** Returns the elements of this chain in a `Seq`, which type is the lowest upper bound of all type elements
		  * (unless another `UpperBound` instance is provided explicitly). This simply forwards to `toList`.
		  */
		@inline def toSeq[U](implicit ub :UpperBound[C, U]) :Seq[U] = toList

		/** Returns the elements of this chain in a list, which type is the lowest upper bound of all type elements
		  * (unless the type parameter `U` or another `UpperBound` instance is provided explicitly).
		  * If this chain contains only singleton elements, the singleton part of the type will be removed
		  * (i.e., `String with Singleton` is replaced with `String`), unless the singleton type bound is provided
		  * explicitly. The list contains the elements in the same order as this chain, meaning the top (last) element
		  * of this chain becomes the deepest (first after `Nil`) element of the list.
		  */
		def toList[U](implicit ub :UpperBound[C, U]) :List[U] = {
			@tailrec def rec(chain :Chain, res :List[U] = Nil) :List[U] = chain match {
				case t ~ h => rec(t, h.asInstanceOf[U]::res)
				case _ => res
			}
			rec(self)
		}

		/** If all elements of this chain are `Tuple2` instances, returns them in a `Map` with the key and value types
		  *  being the upper bounds of the first and second elements of the tuples, respectively. Unless the type
		  *  parameter or the `UpperBound` implicit parameter is provided explicitly, these will be the lowest upper
		  *  bounds which are ''not'' singleton types.
		  */
		@inline def toMap[K, V](implicit ub :UpperBound[C, (K, V)]) :Map[K, V] =
			self.toSeq[(K, V)].toMap
	}






	@implicitNotFound("Type ${U} is not an upper bound of elements in chain ${C}.")
	sealed class UpperBound[C <: Chain, +U] private[collection] ()

	implicit final val EmptyChainBound = new UpperBound[@~, Nothing]

	@inline implicit def upperBound[T <: Chain, H, U, X](implicit tail :UpperBound[T, U], lub :LUB[H, U, X]) :UpperBound[T ~ H, X] =
		tail.asInstanceOf[UpperBound[T ~ H, X]]






	@implicitNotFound("Can't perform a natural transformation of the chain ${X} (of applied ${XF}) to a chain of ${YF}" +
	                  "applied to all type arguments of elements of the mapped chain.")
	sealed abstract class MapChain[XF[T], X <: Chain, YF[T], Y <: Chain] {
		def apply(f :GenericFun[XF, YF])(x :X) :Y
	}

	object MapChain {
		@inline implicit def mapEmptyChain[XF[T], YF[T]] :MapChain[XF, @~, YF, @~] = new MapChain[XF, @~, YF, @~] {
			override def apply(f :GenericFun[XF, YF])(x: @~): @~ = x
		}

		@inline implicit def mapChainHead[XF[T], XT <: Chain, YF[T], YT <: Chain, H]
		                         (implicit mapTail :MapChain[XF, XT, YF, YT]) :MapChain[XF, XT~XF[H], YF, YT~YF[H]] =
			new MapChain[XF, XT~XF[H], YF, YT~YF[H]] {
				override def apply(f :GenericFun[XF, YF])(x :XT ~ XF[H]): YT ~ YF[H] =
					mapTail(f)(x.init) ~ f(x.last)
			}
	}






	class ChainApplication[-X <: Chain, -F, +Y] private[Chain](application :(F, X) => Y) extends ((F, X) => Y) {
		override def apply(f :F, x :X) :Y = application(f, x)
	}

	private def ChainApplication[X <: Chain, F, Y](apply :(F, X) => Y) :ChainApplication[X, F, Y] =
		new ChainApplication(apply)


	
	implicit def applyFunction1[X, Y] :ChainApplication[@~ ~ X, X => Y, Y] =
		ChainApplication { (f :X => Y, xs: @~ ~X) => f(xs.last) }

	implicit def applyFunction2[A, B, Y] :ChainApplication[@~ ~ A ~ B, (A, B) => Y, Y] =
		ChainApplication { (f :(A, B) => Y, xs: @~ ~A~B) => f(xs.init.last, xs.last) }

	implicit def applyFunction3[A, B, C, Y] :ChainApplication[@~ ~ A ~ B ~ C, (A, B, C) => Y, Y] =
		ChainApplication {
			(f :(A, B, C) => Y, xs: @~ ~A~B~C) => val xs1 = xs.init; f(xs1.init.last, xs1.last, xs.last)
		}

	implicit def applyFunction4[A, B, C, D, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D, (A, B, C, D) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D) => Y, xs: @~ ~A~B~C~D) =>
				val xs1 = xs.init; val xs2 = xs1.init; f(xs2.init.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction5[A, B, C, D, E, Y] :ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E, (A, B, C, D, E) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E) => Y, xs: @~ ~A~B~C~D~E) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init
				f(xs3.init.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction6[A, B, C, D, E, F, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F, (A, B, C, D, E, F) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F) => Y, xs: @~ ~A~B~C~D~E~F) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init
				f(xs4.init.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction7[A, B, C, D, E, F, G, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G, (A, B, C, D, E, F, G) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G) => Y, xs: @~ ~A~B~C~D~E~F~G) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				f(xs5.init.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction8[A, B, C, D, E, F, G, H, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H, (A, B, C, D, E, F, G, H) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H) => Y, xs: @~ ~A~B~C~D~E~F~G~H) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init
				f(xs6.init.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction9[A, B, C, D, E, F, G, H, I, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I, (A, B, C, D, E, F, G, H, I) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init
				f(xs7.init.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction10[A, B, C, D, E, F, G, H, I, J, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J,
			                  (A, B, C, D, E, F, G, H, I, J) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init
				f(xs8.init.last, xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction11[A, B, C, D, E, F, G, H, I, J, K, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K,
			                  (A, B, C, D, E, F, G, H, I, J, K) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init
				f(xs9.init.last, xs9.last, 
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction12[A, B, C, D, E, F, G, H, I, J, K, L, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L,
			                  (A, B, C, D, E, F, G, H, I, J, K, L) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				f(xs10.init.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction13[A, B, C, D, E, F, G, H, I, J, K, L, M, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M,
			                  (A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init
				f(xs11.init.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init
				f(xs12.init.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O,
			                  (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init
				f(xs13.init.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				f(xs14.init.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y, xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init
			f(xs15.init.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
				xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init
				f(xs16.init.last, xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init
				f(xs17.init.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				f(xs18.init.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init
				f(xs19.init.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}

	implicit def applyFunction22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, Y]
			:ChainApplication[@~ ~ A ~ B ~ C ~ D ~ E ~ F ~ G ~ H ~ I ~ J ~ K ~ L ~ M ~ N ~ O ~ P ~ Q ~ R ~ S ~ T ~ U ~ V,
		                      (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y, Y] =
		ChainApplication {
			(f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y,
			 xs: @~ ~A~B~C~D~E~F~G~H~I~J~K~L~M~N~O~P~Q~R~S~T~U~V) =>
				val xs1 = xs.init; val xs2 = xs1.init; val xs3 = xs2.init; val xs4 = xs3.init; val xs5 = xs4.init
				val xs6 = xs5.init; val xs7 = xs6.init; val xs8 = xs7.init; val xs9 = xs8.init; val xs10 = xs9.init
				val xs11 = xs10.init; val xs12 = xs11.init; val xs13 = xs12.init; val xs14 = xs13.init
				val xs15 = xs14.init; val xs16 = xs15.init; val xs17 = xs16.init; val xs18 = xs17.init
				val xs19 = xs18.init; val xs20 = xs19.init
				f(xs20.init.last, xs20.last, xs19.last, xs18.last, xs17.last,
					xs16.last, xs15.last, xs14.last, xs13.last, xs12.last, xs11.last, xs10.last, xs9.last,
					xs8.last, xs7.last, xs6.last, xs5.last, xs4.last, xs3.last, xs2.last, xs1.last, xs.last)
		}







}






/** An implementation artifact required to enforce required precedence of implicit values.
  * @see [[net.noresttherein.oldsql.collection.ChainFactory]]
   */
sealed trait ChainFactoryBase {
	/** Type of the companion class. */
	type Type >: @~ <: Chain
	/** Upper bound for all items in the chain. */
	type Item
	/** Non-singleton lowest upper bound of `Item`, or `Item` if it is not a singleton type nor does it contain one. */
	type NonSingleton >: Item
	/** The non-empty subtype of `Type`. */
	type Link[+I <: Type, +L <: Item] <: (I ~ L) with Type


	/** Factory method for non-empty chains of type `Type`. */
	def link[I <: Type, L <: Item](init :I, last :L) :I Link L

	private[this] final val noBound = new UpperBound[Type, Item]

	/** This is the lowest priority implicit which can infer any type `U >: Item` as the upper bound of any chain.
	  * It is used when both `C` is abstract (i.e, ends with `Type` rather than `@~`) and the bound is externally
	  * constrained - usually by the caller specifying it explicitly. One design quirk is that it ''can'' infer
	  * the singleton type `Item` (if it is, or contains a singleton type) as the bound, which the higher priority
	  * implicit defined in `ChainFactory` cannot. This comes from the scala's compiler which strongly avoids
	  * inferring a singleton type. Note that the seemingly superfluous `U >: Item` type parameter is required
	  * to make this implicit lower priority than the wider definition declared in the extending `ChainFactory`.
	  *
	  */
	implicit def noUpperBound[C <: Type, U >: Item] :UpperBound[C, U] = noBound.asInstanceOf[UpperBound[C, U]]
}



/** Base trait for `Chain` subclasses companion objects. In this minimal form, it contains only `UpperBound` implicits
  * and type declarations for both the companion class and its type parameter bounds.
  */
trait ChainFactory extends ChainFactoryBase {
	private[this] final val noBound = new UpperBound[Type, NonSingleton]

	/** Fallback `UpperBound` implicit value used when the chain `C` is abstract, that is ends with `Chain`/`Type`
	  * rather than `@~`. As the name implies, the type inferred will actually use the non-singleton version of*/
	implicit def nonSingletonUpperBound[C <: Type] :UpperBound[C, NonSingleton] = noBound.asInstanceOf[UpperBound[C, NonSingleton]]



	abstract class ChainConcat[P <: Type, S <: Type, C <: Type] private[ChainFactory] {
		def apply(prefix :P, suffix :S) :C
	}

	private[this] final val cat_@~ = new ChainConcat[Type, @~, Type] {
		override def apply(prefix :Type, suffix: @~) = prefix
	}

	implicit def emptyConcat[P <: Type] :ChainConcat[P, @~, P] = cat_@~.asInstanceOf[ChainConcat[P, @~, P]]

	implicit def concatLast[P <: Type, S <: Type, C <: Type, L <: Item](implicit init :ChainConcat[P, S, C])
			:ChainConcat[P, S Link L, C Link L] =
		new ChainConcat[P, S Link L, C Link L] {
			override def apply(prefix :P, suffix :Link[S, L]) = link(init(prefix, suffix.init), suffix.last)
		}
}






/** Base class for the companion object of the `LiteralIndex` implementation of `Chain` as well as companion objects
  * of its subclasses. Contains implicit witnesses which form the basis of implementation of most operations shared
  * by all classes in the `LiteralIndex` type hierarchy and which need access to the factory method for the particular
  * implementation, preventing their declaration in a static context. A `LiteralIndex` is a `Chain` consisting
  * of pairs, which first element is always a singleton type (with the intention of it being a literal type),
  * forming a specialized but limited ''shapeless'' `HMap` variant.
  * @see [[net.noresttherein.oldsql.collection.LiteralIndex]]
  */
sealed abstract class LiteralIndexFactory extends ChainFactory {
	type Type >: @~ <: LiteralIndex
	type Link[+T <: Type, +H <: Item] <: (T &~ H) with Type

	/** The upper bound type of the first member of each tuple in the index. */
	type Key <: Singleton

	/** The upper bound for the second member of each tuple in the index */
	type Value
	type Item = (Key, Value)



	@implicitNotFound("Type ${K} is not a key in index ${I} (or is not mapped to type ${V}).")
	sealed abstract class IndexGet[-I <: Type, K <: Key, +V <: Value] extends ((I, K) => V)
	object IndexGet {

		implicit def getLast[K <: Key, V <: Value] :IndexGet[Type Link (K, V), K, V] =
			new IndexGet[Type Link (K, V), K, V] {
				override def apply(index :Type Link (K, V), key :K) =
					if (index.last._1 == key)
						index.last._2
					else
                        throw new IllegalArgumentException(s"Key $key matches the type, but not the value of the first key in index $index")
			}

		@inline implicit def getPrev[I <: Type, K <: Key, V <: Value]
		                    (implicit get :IndexGet[I, K, V]) :IndexGet[I Link Item, K, V] =
			new IndexGet[I Link Item, K, V] {
				override def apply(i :I Link Item, key :K) = get(i.init, key)
			}
	}






	@implicitNotFound("Can't put ${V} under ${K} in index ${I}: either the type is abstract or the result " +
	                  "does not conform to ${O}.")
	sealed abstract class IndexPut[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends ((I, K, V) => O)

	@implicitNotFound("Can't set ${K} to ${V} in index ${I}: either the key is not present, the type is abstract, " +
	                  "or the result does not conform to ${O}.")
	sealed abstract class IndexSet[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]

	@implicitNotFound("Can't add ${V} under ${K} in index ${I}: either the key already exists, the type is abstract, " +
	                  "or the result does not conform to ${O}.")
	sealed abstract class IndexAdd[-I <: Type, K <: Key, -V <: Value, +O <: Type] extends IndexPut[I, K, V, O]



	sealed abstract class AddWhenMissing {
		private[this] val add = new IndexAdd[Type, Key, Value, Type Link (Key, Value)] {
			override def apply(tail :Type, key :Key, v :Value) = link(tail, (key, v))
		}

		@inline implicit def addEntry[I <: Type, K <: Key, V <: Value]
		                     (implicit unique :UniqueKey[I, K]) :IndexAdd[I, K, V, I Link (K, V)] =
			add.asInstanceOf[IndexAdd[I, K, V, I Link (K, V)]]
	}

	object IndexPut extends AddWhenMissing {
		private[this] val last = new IndexSet[Type Link (Key, Value), Key, Value, Type Link (Key, Value)] {
			override def apply(i :Type Link (Key, Value), key :Key, v :Value) =
				if (i.last._1 == key)
					link(i.init, (key, v))
				else
					throw new IllegalArgumentException(s"Key $key matches the type, but not the value of the first entry in index $i")
		}

		@inline implicit def setLast[I <: Type, K <: Key, V <: Value] :IndexSet[I Link (K, Value), K, V, I Link (K, V)] =
			last.asInstanceOf[IndexSet[I Link (K, Value), K, V, I Link (K, V)]]

		@inline implicit def setPrev[I <: Type, K <: Key, V <: Value, T <: Type, E <: Item]
		                    (implicit set :IndexSet[I, K, V, T]) :IndexSet[I Link E, K, V, T Link E] =
			new IndexSet[I Link E, K, V, T Link E] {
				override def apply(r :I Link E, k :K, v :V) = link(set(r.init, k, v), r.last)
			}
	}






	@implicitNotFound("Key type ${K} is present in index ${I} or the index is abstract.")
	final class UniqueKey[-I <: Type, K <: Key] private ()

	object UniqueKey {
		private[this] final val instance = new UniqueKey[@~, Key]

		implicit def uniqueInEmpty[K <: Key] :UniqueKey[@~ , K] = instance.asInstanceOf[UniqueKey[@~, K]]

		@inline implicit def uniqueIn[I <: Type, E <: Item, K <: Key](implicit unique :UniqueKey[I, K]) :UniqueKey[I Link E, K] =
			unique.asInstanceOf[UniqueKey[I Link E, K]]

		implicit def conflictWhenPresent[K <: Key] :UniqueKey[Type Link (K, Value), K] =
			instance.asInstanceOf[UniqueKey[Type Link (K, Value), K]]
	}





	@implicitNotFound("Type (${K}, ${V}) is not an upper bound of elements in chain ${C}")
	/** A specialized `UpperBound` implementation existing to force ''scalac'' to infer singleton types when needed. */
	final class UpperIndexBound[C <: Type, +K, +V] private[LiteralIndexFactory]() extends UpperBound[C, (K, V)]

	implicit final val EmptyIndexBound = new UpperIndexBound[@~, Nothing, Nothing]

	@inline implicit def upperIndexBound[T <: Type, HK <: Key, HV <: Value, K, V]
	                                    (implicit tail :UpperIndexBound[T, K, V], k :HK <:< K, v :HV <:< V)
			:UpperIndexBound[T Link (HK, HV), K, V] =
		tail.asInstanceOf[UpperIndexBound[T Link (HK, HV), K, V]]

}






/** A variant of `Chain` where all elements a pairs `(L, Any)`, with `L` being a singleton type
  * (intended to be a literal type). It exists to decrease the reliance on implicit witnesses for all operations
  * and putting a static upper bound on such chains, simplifying generic operations, especially when the chain
  * is abstract. Note that, like `Chain`, but unlike `List`, it is left-associative, thus being built
  * 'from left to right', with the easy access to the last element rather than the first.
  * An empty `LiteralIndex` is simply the empty chain [[net.noresttherein.oldsql.collection.Chain.@~$]].
  * @see [[net.noresttherein.oldsql.collection.LiteralIndex.&~]]
  */
sealed trait LiteralIndex extends Chain



object LiteralIndex extends LiteralIndexFactory {
	override type Type = LiteralIndex
	override type Link[+T <: Type, +H <: Item] = T &~ H
	override type Key = Singleton
	override type Value = Any
	override type NonSingleton = (Any, Any)

	override def link[T <: LiteralIndex, H <: (Key, Value)](tail :T, head :H) :T &~ H = new &~(tail, head)


	/*** A non-empty `LiteralIndex`, consisting of another (possibly empty) `LiteralIndex` `init`, followed by
	  * the entry `last`.
	  * @tparam I the type of the chain with all elements but the last element of this type.
	  * @tparam L the type of the last element in the chain.
	  */
	class &~[+I <: LiteralIndex, +L <: (Singleton, Any)](index :I, entry :L)
		extends ~[I, L](index, entry) with LiteralIndex
	{

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[&~[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other: &~[_, _] if other canEqual this => other.last == last && other.init == init
			case _ => false
		}

		override def toString :String = {
			def entry(sb :StringBuilder, e :(Singleton, Any)) :StringBuilder =
				if (e._1.isInstanceOf[Chain] || e._2.isInstanceOf[Chain])
					sb append "(" append e._1 append "->" append e._2 append ")"
				else
                    sb append e._1 append "->" append e._2

			def rec(t :LiteralIndex, h: (Singleton, Any)) :StringBuilder = t match {
				case r &~ e => entry(rec(r, e) append " &~ ", h)
				case _ => entry(new StringBuilder(), h)
			}
			rec(init, last).toString
		}

	}



	/** A constructor and extractor of non-empty `LiteralIndex` implementations.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `&~`
	  * appears in the index's definition:
	  * {{{
	  *     index match {
	  *         case @~ &~ key1 -> value1 &~ key2 - value2
	  *     }
	  * }}}
	  */
	object &~ {
		@inline def apply[T <: LiteralIndex, H <: (Singleton, Any)](tail :T, head :H) :T &~ H = new &~(tail, head)

		@inline def unapply[T<: LiteralIndex, H <: (Singleton, Any)](index :T &~ H) :T &~ H = index

		@inline def unapply(index :LiteralIndex) :Option[(LiteralIndex, (Singleton, Any))] = index match {
			case nonEmpty: &~[_, _] => Some(nonEmpty.init -> nonEmpty.last)
			case _ => None
		}
	}



	/** Implicitly extends an index of type `T` with methods requiring its static type. As this method uses the same
	  * name as the non-empty index class [[net.noresttherein.oldsql.collection.LiteralIndex.&~#]], this implicit
	  * conversion is imported automatically automatically alongside it.
	  */
	@inline implicit def &~[T <: LiteralIndex](index :T) :TypeIndexOps[T] = new TypeIndexOps(index)



	/** Operations on the index `I` */
	class TypeIndexOps[I <: LiteralIndex](private val self :I) extends AnyVal {
		/** Extends the index with another entry `N`, becoming the new last entry. */
		@inline def &~[N <: (Singleton, Any)](next :N) :I &~ N = new &~(self, next)

		/** Retrieves the value of associated with the given key. This assumes that the keys in this index are
		  * literal types (or at least, they where in the context in which it was created) and the key comparison
		  * is made based on the types, rather than values. As `LiteralIndex` is covariant regarding both
		  * of its type parameters, it is possible to break the implicit entry resolution mechanism by upcasting
		  * an entry to `Singleton` (or some `I with Singleton`), in which case `K =:= Singleton` (alternatively,
		  * the same `I with Singleton`) would resolve the first of such entries. If the key of the found entry
		  * does not equal the argument, an `IllegalArgumentException` will be thrown. Note that this might happen
		  * even the key is actually present in the index and in the fully instantiated part of this index's type
		  * definition, but following a bogus widened key.
		  * This method can't be called if the key `K` is not a part of the (known) type definition of the index `I`.
		  * @tparam K a singleton type of the key (a literal type in non-abstract contexts).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Singleton, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `(key, value) :(K, V)`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `LiteralIndex`), it returns a new index with the new pair at the end.
		  * Note that, as `LiteralIndex` is covariant regarding both of its type arguments, it is possible to break
		  * this method by upcasting a key type simply to `Singleton` (or `I with Singleton` for some type `I`),
		  * which would then match any other `K =:= I with Singleton` provided here. If the actual key of the entry
		  * returned by the `IndexPut` implicit parameter does not equal the key `key` provided here, an
		  * `IllegalArgumentException` will be thrown.
		  */
		@inline def update[K <: Singleton, V, R <: LiteralIndex](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given chain to the end of this chain. */
		def ++[S <: LiteralIndex, R <: LiteralIndex](suffix :S)(implicit concat :ChainConcat[I, S, R]) :R =
			concat(self, suffix)

//		@inline def toMap[K, V](implicit ub :UpperBound[I, (K, V)]) :Map[K, V] =
//			self.toSeq[(K, V)].toMap

	}



}






/** A `Record` is, simply put, just a `LiteralIndex` where the key types are string literals (singleton types).
  *
  * @see [[net.noresttherein.oldsql.collection.Record.|#]]
  * @see [[net.noresttherein.oldsql.collection.Record.#>]]
  */
sealed trait Record extends LiteralIndex



object Record extends LiteralIndexFactory {
	override type Type = Record
	override type Link[+T <: Record, +H <: Item] = T |# H
	type Key = String with Singleton
	override type Value = Any
	override type NonSingleton = (String, Any)

	override def link[T <: Record, H <: (Key, Any)](tail :T, head :H) :T |# H = new |#(tail, head)






	/** A non-empty record, consisting of the given record as its first elements, followed by the element `next`.
	  * Importing this class also imports implicit conversions which add a `#>` method to any `String`,
	  */
	final class |#[+I <: Record, +E <: (Key, Any)](record :I, next :E) extends &~[I, E](record, next) with Record {

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[|#[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case other: |#[_, _] => (other eq this) || other.last == last && other.init == init
			case _ => false
		}

		override def toString :String = {
			def entry(sb :StringBuilder, e :(Key, Any)) :StringBuilder = e._2 match {
				case _ :Record => sb append e._1 append ": (" append e._2 append ")"
				case _ => sb append e._1 append ": " append e._2
			}
			def rec(t :Record, h: (Key, Any)) :StringBuilder = t match {
				case r |# e => entry(rec(r, e) append " |# ", h)
				case _ => entry(new StringBuilder(), h)
			}
			rec(init, last).toString
		}

	}



	/** A factory and extractor of non-empty `Record` instances.
	  * The extractor part is designed to be used in the infix notation, following the same order as the type `|#`
	  * appears in the index's definition:
	  * {{{
	  *     "silver" #> "monsters" |# "steel" |# "humans" match {
	  *         case key1 #> value1 |# key2 #> value2
	  *     }
	  * }}}
	  * Note that in the above example we also use the feature of the `#>` tuple extractor which matches also
	  * any singleton `Record` (that is, any value of type `@~ |# E`).
	  * Importing this symbol imports also implicit conversion which from any valid entry
	  * (a pair `(String with Singleton, V)`) to a singleton record, and a conversion extending all string singletons
	  * with a method `#>` for creating tuples with a singleton type as the key. Both of these features together
	  * allow omitting of `@~` when creating records and starting with the first element instead, as also shown
	  * in the above example.
	  */
	object |# {

		@inline def apply[T <: Record, K <: Key, V](tail :T, head :(K, V)) :T |# (K, V) =
			new |#(tail, head)

		@inline def unapply[T <: Record, E <: (Key, Any)](record :T |# E) :T |# E = record

		@inline def unapply(record :Record) :Option[(Record, (Key, Any))] = record match {
			case rec: |#[_, _] => Some(rec.init -> rec.last)
			case _ => None
		}
	}



	@inline implicit def |#[K <: Key, V](entry :(K, V)) :RecordOps[@~ |# (K, V)] = @~ |# entry

	@inline implicit def |#[K <: Key](key :K) :method_#>[K] = new method_#>(key)

	class method_#>[K <: Key](private val key :K) extends AnyVal {
		@inline def #>[V](value :V) :(K, V) = (key, value)
	}



	/** A type alias for a tuple where the first element is a string singleton type. */
	type #>[+K <: Key, V] = (K, V)

	/** An extractor for pairs being elements of a `Record`. Aside from introducing an infix format for the tuple,
	  * it declares also an `unapply` method accepting a `Record` itself, matching it ''iff'' it contains
	  * exactly one element. This allows to write extractors without the initial `@~`:
	  * {{{
	  *     val record = "silver" #> "monsters |# "steel" #> humans
	  *     val (sword1 #> victim1 |# sword2# #> victim2) = record
	  * }}}
	  */
	object #> {

		def unapply[K <: Key, V](entry :(K, V)) :Some[(K, V)] = Some(entry)

		def unapply[T <: Record, K <: Key, V](record :T |# (K, V)) :Option[(K, V)] =
			if (record.init eq @~) Some(record.last) else None

		def unapply(record :Record) :Option[(Key, Any)] = record match {
			case @~ |# entry => Some(entry)
			case _ => None
		}

	}





	implicit class RecordOps[I <: Record](private val self :I) extends AnyVal {
		/** Appends a next entry to the end of the record. */
		@inline def |#[E <: (Key, Any)](entry :E) :I |# E = new |#(self, entry)

		/** Retrieves the value of associated with the given key. This assumes that the keys in this index are
		  * literal types (or at least, they where in the context in which it was created) and the key comparison
		  * is made based on the types, rather than values. As `LiteralIndex` is covariant regarding both
		  * of its type parameters, it is possible to break the implicit entry resolution mechanism by upcasting
		  * an entry to `Singleton` (or some `I with Singleton`), in which case `K =:= Singleton` (alternatively,
		  * the same `I with Singleton`) would resolve the first of such entries. If the key of the found entry
		  * does not equal the argument, an `IllegalArgumentException` will be thrown. Note that this might happen
		  * even the key is actually present in the index and in the fully instantiated part of this index's type
		  * definition, but following a bogus widened key.
		  * This method can't be called if the key `K` is not a part of the (known) type definition of the index `I`.
		  * @tparam K a singleton type of the key (a literal type in non-abstract contexts).
		  * @tparam V the type of the value associated with the key `K`.
		  */
		@inline def apply[K <: Key, V](key :K)(implicit get :IndexGet[I, K, V]) :V = get(self, key)

		/** Puts the given `(key, value)` pair in this index. If key `K` is part of this index's type definition as
		  * seen in the caller's context, this will create a new index, where the entry with that key is replaced
		  * with the entry `(key, value) :(K, V)`. If the key is not present, and the index is fully instantiated
		  * (it starts with `@~` rather than `LiteralIndex`), it returns a new index with the new pair at the end.
		  * Note that, as `LiteralIndex` is covariant regarding both of its type arguments, it is possible to break
		  * this method by upcasting a key type simply to `Singleton` (or `I with Singleton` for some type `I`),
		  * which would then match any other `K =:= I with Singleton` provided here. If the actual key of the entry
		  * returned by the `IndexPut` implicit parameter does not equal the key `key` provided here, an
		  * `IllegalArgumentException` will be thrown.
		  */
		@inline def update[K <: Key, V, R <: Record](key :K, value :V)(implicit put :IndexPut[I, K, V, R]) :R =
			put(self, key, value)

		/** Appends the given record to the end of this record. */
		def ++[S <: Record, R <: Record](suffix :S)(implicit concat :ChainConcat[I, S, R]) :R =
			concat(self, suffix)

//		@inline def toMap[K, V](implicit ub :UpperBound[I, (K, V)]) :Map[K, V] =
//			self.toSeq[(K, V)].toMap

	}

}
