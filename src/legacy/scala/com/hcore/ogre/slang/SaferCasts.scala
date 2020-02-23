package com.hcore.ogre.slang

import scala.reflect.ClassTag



object SaferCasts {

	implicit class CastingExtension[T](private val value :T) extends AnyVal {
		def downcast[S<:T] :S = value.asInstanceOf[S]
		def upcast[S>:T] :S = value.asInstanceOf[S]


		@inline
		def castTo[S](implicit S :ClassTag[S]) :S =
			castTo[S](new ClassCastException(s"expected class ${S.runtimeClass}; got $value :${value.getClass}"))

		@inline
		def castTo[S](excp : =>Exception)(implicit S :ClassTag[S]) :S = value match {
			case S(s) => s
			case _ => throw excp
		}


		@inline
		def asSubclass[S<:T](implicit S :ClassTag[S]) :Option[S] = S.unapply(value)

		@inline
		def asSubclassOf[S](implicit S:ClassTag[S]) :Option[S] = S.unapply(value)

		@inline
		def ifSubclassOf[S] :CastValueGuard[T, S] = new CastValueGuard[T, S](value)

		@inline
		def ifSubclass[S<:T] :CastValueGuard[T, S] = ifSubclassOf[S]

		@inline
		def explicitCast[F>:T, S] :S = value.asInstanceOf[S]

	}




	class CastValueGuard[T, S](val value :T) extends AnyVal {
//		def apply[X](result : =>X)(implicit S :ClassTag[S]) :Option[X] = value match {
//			case S(s) => Some(result)
//			case _ => None
//		}

		def apply[X](block :S=>X)(implicit S :ClassTag[S]) :Option[X] = value match {
			case S(s) => Some(block(s))
			case _ => None
		}

		def orElse[X](block :S=>X)(alternative : =>X)(implicit S: ClassTag[S]) :X = value match {
			case S(s) => block(s)
			case _ => alternative
		}
	}

	implicit class TypeParameterCastingExtension1[G[X]](private val value: G[_]) extends AnyVal {
//		def uptyped[S>:T] :G[S] = as[G[S]]
//		def downtyped[S<:T] :G[S] = as[G[S]]
		def crosstyped[S] :G[S] = as[G[S]]

		private[this] final def as[X] = value.asInstanceOf[X]
	}

	implicit class TypeParameterCastingExtension2[G[X, Y]](private val value :G[_, _]) extends AnyVal {
		def crosstyped[S, T] :G[S, T] = value.asInstanceOf[G[S, T]]
	}

	implicit class TypeParameterCastingExtension3[G[X, Y, Z]](private val value :G[_, _, _]) extends AnyVal {
		def crosstyped[S, T, U] :G[S, T, U] = value.asInstanceOf[G[S, T, U]]
	}


//
//	implicit class TypeParameterCastingExtension2[G[A, B], X, Y](val value :G[X, Y]) extends AnyVal {
//		def uptyped1[S>:X] :G[S, Y] = as[G[S, Y]]
//		def downtyped1[S<:X] :G[S, Y] = as[G[S, Y]]
//		def crosstyped1[S] :G[S, Y] = as[G[S, Y]]
//
//		def uptyped2[T>:Y] :G[X, T] = as[G[X, T]]
//		def downtyped2[T<:Y] :G[X, T] = as[G[X, T]]
//		def crosstyped2[T] :G[X, T] = as[G[X, T]]
//
//		def uptyped[S>:X, T>:Y] :G[S, T] = as[G[S, T]]
//		def downtyped[S<:X, T<:Y] :G[S, T] = as[G[S, T]]
//		def crosstyped[S, T] :G[S, T] = as[G[S, T]]
//
//		private[this] final def as[A] = value.asInstanceOf[A]
//
//	}
//
//
//	implicit class TypeParameterCastingExtension3[G[A, B, C], X, Y, Z](val value :G[X, Y, Z]) extends AnyVal {
//		def uptyped1[S>:X] :G[S, Y, Z] = as[G[S, Y, Z]]
//		def downtyped1[S<:X] :G[S, Y, Z] = as[G[S, Y, Z]]
//		def crosstyped1[S] :G[S, Y, Z] = as[G[S, Y, Z]]
//
//		def uptyped2[T>:Y] :G[X, T, Z] = as[G[X, T, Z]]
//		def downtyped2[T<:Y] :G[X, T, Z] = as[G[X, T, Z]]
//		def crosstyped2[T] :G[X, T, Z] = as[G[X, T, Z]]
//
//		def uptyped3[U>:Z] :G[X, Y, U] = as[G[X, Y, U]]
//		def downtyped3[U<:Z] :G[X, Y, U] = as[G[X, Y, U]]
//		def crosstyped3[U] :G[X, Y, U] = as[G[X, Y, U]]
//
//
//		def uptyped[S>:X, T>:Y, U>:Z] :G[S, T, U] = as[G[S, T, U]]
//		def downtyped[S<:X, T<:Y, U<:Z] :G[S, T, U] = as[G[S, T, U]]
//		def crosstyped[S, T, U] :G[S, T, U] = as[G[S, T, U]]
//
//		private[this] final def as[A] = value.asInstanceOf[A]
//
//	}

}
