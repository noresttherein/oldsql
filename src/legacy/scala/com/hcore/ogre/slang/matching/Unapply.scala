package com.hcore.ogre.slang.matching


//implicits

import com.hcore.ogre.slang.options.extensions
import extensions._
import com.hcore.ogre.slang.options.extensions


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

	def apply[X, Y](extractor :X=>Option[Y]) :Unapply[X, Y] =
		new UnapplyAdapter(extractor)

	def apply[X, Y](pf :PartialFunction[X, Y]) :Unapply[X, Y] =
		new UnapplyAdapter[X, Y](x => pf(x).providing(pf.isDefinedAt(x)))

	def Try[X, Y](extractor :X => Y) :Unapply[X, Y] =
		new TryUnapply(extractor)

	def Sure[X, Y](extractor :X=>Y) :SureUnapply[X, Y] =
		new SureUnapply[X, Y](extractor)

	def Check[X](condition :X=>Boolean) :Unapply[X, Unit] = apply[X, Unit](condition(_:X).ifTrue(()))

	object Never extends Unapply[Any, Nothing] {
		override def unapply(x: Any): Option[Nothing] = None
	}


	implicit def toPartialFunction[X, Y](unapply :Unapply[X, Y]) :PartialFunction[X, Y] = {
		case unapply(y) => y
	}



	class UnapplyAdapter[-X, +Y](extractor :X=>Option[Y]) extends Unapply[X, Y] {
		def unapply(x :X) = extractor(x)

	}

	class SureUnapply[-X, +Y](extractor :X=>Y) extends Unapply[X, Y] {
		override def unapply(x: X): Some[Y] = Some(extractor(x))
	}



	class TryUnapply[-X, +Y](extractor :X => Y)
		extends UnapplyAdapter[X, Y](x => scala.util.Try{ extractor(x) }.toOption)


}



