package com.adpilot.cortb.clientapi.util


object Generic {
	trait GenericFunction[X[T], Y[T]] {
		def apply[T](x :X[T]) :Y[T]
	}
}
