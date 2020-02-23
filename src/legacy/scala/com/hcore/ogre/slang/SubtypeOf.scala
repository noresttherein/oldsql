package com.hcore.ogre.slang


sealed trait SubtypeOf[+X] {
	type Type<:X
}

sealed trait SupertypeOf[-X] {
	type Type>:X
}

sealed trait BoundType[-L<:U, +U] extends SubtypeOf[U] with SupertypeOf[L] {
	type Type >:L <:U
}


class TypeBox[T] extends BoundType[T, T] {
	type Type = T
}

object TypeBox {
	def apply[X] :TypeBox[X] = new TypeBox[X]
}

