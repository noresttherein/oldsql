package com.hcore.ogre.morsels


object Generic {
	trait GenericFunction[X[T], Y[T]] {
		def apply[T](x :X[T]) :Y[T]
	}
}
