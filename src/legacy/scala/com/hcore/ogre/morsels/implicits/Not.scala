package com.hcore.ogre.morsels.implicits


class Not[T] private[Not] ()

object Not {
	implicit def byDefault[T] :Not[T] = instance.asInstanceOf[Not[T]]
	implicit def doubleDefinition[T](implicit ev :T) :Not[T] = instance.asInstanceOf[Not[T]]

	private[this] val instance = new Not[Any]
}
