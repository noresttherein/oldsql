package com.adpilot.cortb.clientapi.util

import com.adpilot.cortb.clientapi.util.Matching.Unapply.Sure


object Matching {
	import scala.util.matching.Regex
	import OptionOps._


	object && {
		def unapply(x :Any) = Some(x, x)
	}

	/**
	 * @see http://hootenannylas.blogspot.com.au/2013/02/pattern-matching-with-string.html for an explanation of
	 *      string interpolation in pattern matching
	 */
	implicit class RegexpInterpolation(val sc :StringContext) extends AnyVal {
		def rx = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"):_*)
	}






	implicit class ValuePatternMatching[T](val value :T) extends AnyVal {
		def matches[X](expr :PartialFunction[T, X]) :Boolean = expr.isDefinedAt(value)
//		def matches[X](expr :T=>Any) :Boolean = Try(expr(value)).isSuccess
//		def matches[X](expr :Unapply[T, X]) :Boolean = expr.unapply(value).isDefined
//		def matches[X](expr :T=>Option[X]) :Boolean = expr(value).isDefined
	}
	
	trait Unapply[-X, +Y] {
		def unapply(x :X) :Option[Y]

		def optional = Unapply.Sure(unapply)

		def orElse[T>:Y](default :T) = Unapply.Sure[X, T](x => unapply(x) getOrElse default)

		def || [S<:X, T>:Y](other :Unapply[S, T]) :Unapply[S, T] =
			Unapply((x :S) => unapply(x).orElse(other.unapply(x)))

		def && [S<:X, T](other :Unapply[S, T]) :Unapply[S, (Y, T)] =
			Unapply((x :S) => unapply(x).flatMap(first => other.unapply(x).map((first, _))))
	}


	object Unapply {
//		type DefinesUnapply[X, Y] = { def unapply(x :X) :Option[Y] }

		def apply[X, Y](extractor :X=>Option[Y]) :Unapply[X, Y] =
			new UnapplyAdapter(extractor)

		def apply[X, Y](pf :PartialFunction[X, Y]) :Unapply[X, Y] =
			new UnapplyAdapter[X, Y](x => pf(x).providing(pf.isDefinedAt(x)))

		def Try[X, Y](extractor :X => Y) :Unapply[X, Y] =
//			new UnapplyAdapter[X, Y](x => scala.util.Try{ extractor(x) }.toOption)
			new TryUnapply(extractor)
		
		def Sure[X, Y](extractor :X=>Y) :SureUnapply[X, Y] =
			new SureUnapply[X, Y](extractor)

		def Check[X](condition :X=>Boolean) :Unapply[X, Unit] = apply[X, Unit](condition(_:X).ifTrue(()))


		implicit def toPartialFunction[X, Y](unapply :Unapply[X, Y]) :PartialFunction[X, Y] = {
			case unapply(y) => y
		}

//		implicit def structural[X, Y](obj :{ def unapply(x :X) :Option[Y] }) = Unapply(obj.unapply)

//		implicit class UnapplyCombiner[X, Y](val unapply :Unapply[X, Y]) extends AnyVal {
//			def &&[T](other :Unapply[X, T]) :Unapply[X, (Y, T)] =
//				new UnapplyAdapter[X, (Y, T)](x => unapply.unapply(x).flatMap(first => other.unapply(x).map((first, _))))
//
//		}
//
//
		
		class UnapplyAdapter[-X, +Y](extractor :X=>Option[Y]) extends Unapply[X, Y] {
			def unapply(x :X) = extractor(x)

		}

		class SureUnapply[-X, +Y](extractor :X=>Y) extends Unapply[X, Y] {
			override def unapply(x: X): Some[Y] = Some(extractor(x))
		}

//		object Sure {
//			def apply[X, Y](extractor :X=>Y) = new Sure(extractor)
//		}

		
		class TryUnapply[-X, +Y](extractor :X => Y)
			extends UnapplyAdapter[X, Y](x => scala.util.Try{ extractor(x) }.toOption)
		
		
//		object TryUnapply {
//			def apply[X, Y](extractor :X=>Y) = new TryUnapply(extractor)
//		}


//		class UnapplyOption[-X, +Y](extractor :X=>Option[Y])
//			extends SureUnapply[X, Option[Y]](extractor)

//		object UnapplyOption {
//			def apply[X, Y](extractor :X=>Option[Y]) = new UnapplyOption(extractor)
//
//			def Try[X, Y](extractor :X=>Y) = new UnapplyOption[X, Y](x => util.Try{ extractor(x) }.toOption)
//		}
		

	}


	trait UnapplySeq[-X, +Y] {
		def unapplySeq(x :X) :Option[Seq[Y]]

		def optional = Unapply.Sure(unapplySeq)

		def orElse[T>:Y](default :Seq[T]) = UnapplySeq.Sure[X, T](x => unapplySeq(x) getOrElse default)

		def orEmpty = orElse(Nil) //UnapplySeq.Sure[X, Y](x => unapplySeq(x) getOrElse Seq())

		def || [S<:X, T>:Y](other :UnapplySeq[S, T]) :UnapplySeq[S, T] =
			UnapplySeq((x :S) => unapplySeq(x).orElse(other.unapplySeq(x)))

//		def && [S<:X, T](other :Unapply[S, T]) :Unapply[S, (Y, T)] =
//			UnapplySeq((x :S) => unapply(x).flatMap(first => other.unapply(x).map((first, _))))
	}


	object UnapplySeq {
		//		type DefinesUnapply[X, Y] = { def unapply(x :X) :Option[Y] }

		def apply[X, Y](extractor :X=>Option[Seq[Y]]) :UnapplySeq[X, Y] =
			new UnapplySeqAdapter(extractor)

		def apply[X, Y](pf :PartialFunction[X, Seq[Y]]) :UnapplySeq[X, Y] =
			new UnapplySeqAdapter[X, Y](x => pf(x).providing(pf.isDefinedAt(x)))

		def Try[X, Y](extractor :X => Seq[Y]) :UnapplySeq[X, Y] =
			new TryUnapplySeq(extractor)

		def Sure[X, Y](extractor :X=>Seq[Y]) :SureUnapplySeq[X, Y] =
			new SureUnapplySeq[X, Y](extractor)

		def orEmpty[X, Y](extractor :X=>Option[Seq[Y]]) :UnapplySeq[X, Y] = apply(extractor).orEmpty


		implicit def toPartialFunction[X, Y](unapply :UnapplySeq[X, Y]) :PartialFunction[X, Seq[Y]] = {
			case unapply(elems @_*) => elems
		}


		class UnapplySeqAdapter[-X, +Y](extractor :X=>Option[Seq[Y]]) extends UnapplySeq[X, Y] {
			def unapplySeq(x :X) = extractor(x)

		}

		class SureUnapplySeq[-X, +Y](extractor :X=>Seq[Y]) extends UnapplySeq[X, Y] {
			override def unapplySeq(x: X): Some[Seq[Y]] = Some(extractor(x))
		}


		class TryUnapplySeq[-X, +Y](extractor :X => Seq[Y])
			extends UnapplySeqAdapter[X, Y](x => scala.util.Try{ extractor(x) }.toOption)


	}

	
	
	

}