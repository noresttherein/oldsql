package com.hcore.ogre.morsels.implicits


final class ImplicitBox[T](val value :T) extends AnyVal


object ImplicitBox {
	@inline
	implicit def box[T](value :T) :ImplicitBox[T] = new ImplicitBox[T](value)
}
