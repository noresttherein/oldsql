package net.noresttherein.oldsql.collection



/**
  * @author Marcin Mościcki
  */
sealed trait Chain extends Serializable



object Chain {


	case class ~[+T <: Chain, +H](tail :T, head :H) extends Chain

	sealed class @~ private[Chain] extends Chain

	case object @~ extends @~



	implicit class ChainOps[C <: Chain](private val chain :C) extends AnyVal {
		@inline def ~[H](head :H) :C ~ H = new ~(chain, head)

		@inline def map[XF[T], YF[T], Y <: Chain](f :GenericFun[XF, YF])(implicit result :MapChain[XF, C, YF, Y]) :Y =
			result(f)(chain)

		def foreach(f :GenericFun[Self, Const[Unit]#T]) :Unit = {
			def rec(chain :Chain) :Unit = chain match {
				case t ~ h => rec(t); f(h)
				case _ => ()
			}
			rec(chain)
		}
	}


	type Self[X] = X
	type Const[Y] = { type T[X] = Y }

//	type =%>[-X[A], +Y[A]] = GenericFun[X, Y]

	trait GenericFun[-X[A], +Y[A]] {
		def apply[T](x :X[T]) :Y[T]
	}

	trait BoxFun[+Y[A]] extends GenericFun[Self, Y] {
		override def apply[T](x :T) :Y[T]
	}

	trait UnboxFun[-X[A]] extends GenericFun[X, Self] {
		override def apply[T](x :X[T]) :T
	}


//	sealed abstract class FunctorChain[XF[T], X <: Chain, Y <: Chain] {
//		def box(f :GenericFun[Self, XF])(y :Y) :X
//		def unbox(f :GenericFun[XF, Self])(x :X) :Y
//	}
//
//	object FunctorChain {
//		implicit def emptyFunctorChain[XF[T]] :FunctorChain[XF, @~, @~] = new FunctorChain[XF, @~, @~] {
//			override def box(f :GenericFun[Self, XF])(y: @~): @~ = y
//			override def unbox(f :GenericFun[XF, Self])(x: @~): @~ = x
//		}
//
//		implicit def functorChain[XF[T], XT <: Chain, YT <: Chain, YH]
//		                         (implicit tail :FunctorChain[XF, XT, YT]) :FunctorChain[XF, XT~XF[YH], YT~YH] =
//			new FunctorChain[XF, XT~XF[YH], YT~YH] {
//				override def box(f :GenericFun[Self, XF])(y :YT ~ YH): XT~XF[YH] = tail.box(f)(y.tail) ~ f(y.head)
//				override def unbox(f :GenericFun[XF, Self])(x :XT ~ XF[YH]): YT~YH = tail.unbox(f)(x.tail) ~ f(x.head)
//			}
//	}



	sealed abstract class MapChain[XF[T], X <: Chain, YF[T], Y <: Chain] {
		def apply(f :GenericFun[XF, YF])(x :X) :Y
	}

	object MapChain {
		implicit def mapEmptyChain[XF[T], YF[T]] :MapChain[XF, @~, YF, @~] = new MapChain[XF, @~, YF, @~] {
			override def apply(f :GenericFun[XF, YF])(x: @~): @~ = x
		}

		implicit def mapChainHead[XF[T], XT <: Chain, YF[T], YT <: Chain, H]
		                         (implicit mapTail :MapChain[XF, XT, YF, YT]) :MapChain[XF, XT~XF[H], YF, YT~YF[H]] =
			new MapChain[XF, XT~XF[H], YF, YT~YF[H]] {
				override def apply(f :GenericFun[XF, YF])(x :XT ~ XF[H]): YT ~ YF[H] =
					mapTail(f)(x.tail) ~ f(x.head)
			}
	}



}
