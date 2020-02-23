package com.hcore.ogre.slang.matching


//implicits

import com.hcore.ogre.slang.options.extensions
import extensions._
import com.hcore.ogre.slang.matching.GenUnapply.SureGenUnapply
import com.hcore.ogre.slang.options.extensions


trait GenUnapply[-X[T], +Y[T]] { matcher =>
	def unapply[T](x :X[T]) :Option[Y[T]]

	def existential :Unapply[X[_], Y[_]] = Unapply[X[_], Y[_]](unapply(_:X[_]))

	def forType[T] :Unapply[X[T], Y[T]] = Unapply[X[T], Y[T]](unapply(_:X[T]))

//	def optional :GenUnapply[X, ({ type L[T]=Some[Y[T]] })#L]= new GenUnapply[X, ({ type L[T]=Some[Y[T]] })#L] {
//		override def unapply[T](x: X[T]): Some[Option[Y[T]]] = Some(matcher.unapply(x))
//	}


	def || [W[T]<:X[T], Z[T]>:Y[T]](other :GenUnapply[W, Z]) :GenUnapply[W, Z] =
		GenUnapply[W, Z]((x :W[_]) => unapply(x).asInstanceOf[Option[Y[Any]]].orElse(other.unapply(x).asInstanceOf[Option[Z[Any]]]))

//	def && [W[T]<:X[T], Z[T]](other :Unapply[W, Z]) :Unapply[W, (Y, Z)] =
//		Unapply((x :W) => unapply(x).flatMap(first => other.unapply(x).map((first, _))))

}


object GenUnapply {

	def apply[X[T], Y[T]](extractor :X[_]=>Option[Y[_]]) :GenUnapply[X, Y] =
		new GenUnapplyAdapter[X, Y](extractor)

	def apply[X[T], Y[T]](pf :PartialFunction[X[_], Y[_]]) :GenUnapply[X, Y] =
		new GenUnapplyAdapter[X, Y](x => pf(x).providing(pf.isDefinedAt(x)))

	def Try[X[T], Y[T]](extractor :X[_] => Y[_]) :GenUnapply[X, Y] =
		new TryGenUnapply[X, Y](extractor)

	def Sure[X[T], Y[T]](extractor :X[_]=>Y[_]) :SureGenUnapply[X, Y] =
		new SureGenUnapply[X, Y](extractor)

//	def Check[X[T]](condition :X[_]=>Boolean) :Unapply[X, Unit] = apply[X, Unit](condition(_:X).ifTrue(()))


	implicit def toPartialFunction[X, Y](unapply :Unapply[X, Y]) :PartialFunction[X, Y] = {
		case unapply(y) => y
	}



	class GenUnapplyAdapter[-X[T], +Y[T]](extractor :X[_]=>Option[Y[_]]) extends GenUnapply[X, Y] {
		def unapply[T](x :X[T]) = extractor(x).asInstanceOf[Option[Y[T]]]

	}

	class SureGenUnapply[-X[T], +Y[T]](extractor :X[_]=>Y[_]) extends GenUnapply[X, Y] {
		override def unapply[T](x: X[T]): Some[Y[T]] = Some(extractor(x).asInstanceOf[Y[T]])
	}



	class TryGenUnapply[-X[T], +Y[T]](extractor :X[_] => Y[_])
		extends GenUnapplyAdapter[X, Y](x => scala.util.Try{ extractor(x) }.toOption)


}


