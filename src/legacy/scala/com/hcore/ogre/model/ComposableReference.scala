package com.hcore.ogre.model

import com.hcore.ogre.model.ComposedOf.{DecomposableTo, ComposableFrom}


trait ComposableReference[-E, +T] extends Reference[T] {
	def composition :T ComposableFrom E
}


trait DecomposableReference[T, +E] extends Reference[T] {
	def decomposition :T DecomposableTo E
}


object DecomposableReference {
//	def unapply[T](ref :Reference[T]) :Option[Iterable[_]]
}




