package com.hcore.ogre.slang.matching



//implicits

import com.hcore.ogre.slang.options.extensions
import extensions._
import com.hcore.ogre.slang.options.extensions


trait UnapplySeq[-X, +Y] {
	def unapplySeq(x :X) :Option[Seq[Y]]

	def optional = Unapply.Sure(unapplySeq)

	def orElse[T>:Y](default :Seq[T]) = UnapplySeq.Sure[X, T](x => unapplySeq(x) getOrElse default)

	def orEmpty = orElse(Nil)

	def || [S<:X, T>:Y](other :UnapplySeq[S, T]) :UnapplySeq[S, T] =
		UnapplySeq((x :S) => unapplySeq(x).orElse(other.unapplySeq(x)))

}


object UnapplySeq {

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


