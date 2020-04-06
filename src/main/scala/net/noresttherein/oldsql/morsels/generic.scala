package net.noresttherein.oldsql.morsels

/**
  * @author Marcin Mościcki
  */
object generic {
//	type Subtype[U] = { type T[X <: U] = X }
	type Self[X] = X
	type Const[Y] = { type T[X] = Y }

	type =#>[-X[A], +Y[A]] = GenericFun[X, Y]

	trait GenericFun[-X[A], +Y[A]] {
		def apply[T](x :X[T]) :Y[T]

		def existential: X[_] => Y[_] = apply(_)
	}

	trait BoxFun[+Y[A]] extends GenericFun[Self, Y] {
		override def apply[T](x :T) :Y[T]
	}

	trait UnboxFun[-X[A]] extends GenericFun[X, Self] {
		override def apply[T](x :X[T]) :T
	}



	def ident[X[A]] :GenericFun[X, X] = identity.asInstanceOf[GenericFun[X, X]]

	private[this] final val identity = new GenericFun[Self, Self] {
		override def apply[T](x :T) = x
	}

}