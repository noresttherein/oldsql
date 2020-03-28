package net.noresttherein.oldsql.slang

import scala.reflect.ClassTag


/**
  * @author Marcin Mościcki
  */
private[oldsql] object SaferCasts {

	implicit class CastingExtension[T](private val value :T) extends AnyVal {
		def downcast[S<:T] :S = value.asInstanceOf[S]
//		def upcast[S>:T] :S = value.asInstanceOf[S]


		@inline def castTo[S](implicit S :ClassTag[S]) :S =
			castTo[S](new ClassCastException(s"expected class ${S.runtimeClass}; got $value :${value.getClass}"))

		@inline def castTo[S](excp : =>Exception)(implicit S :ClassTag[S]) :S = value match {
			case S(s) => s
			case _ => throw excp
		}


		@inline def asSubclass[S <: T](implicit S :ClassTag[S]) :Option[S] = S.unapply(value)

		@inline def asSubclassOf[S](implicit S :ClassTag[S]) :Option[S] = S.unapply(value)

		@inline def ifSubclassOf[S] :CastValueGuard[T, S] = new CastValueGuard[T, S](value)

		@inline def ifSubclass[S <: T] :CastValueGuard[T, S] = ifSubclassOf[S]

//		@inline def explicitCast[F>:T, S] :S = value.asInstanceOf[S]

	}




	class CastValueGuard[T, S](val value :T) extends AnyVal {

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
		def crosstyped[S] :G[S] = value.asInstanceOf[G[S]]
	}

	implicit class TypeParameterCastingExtension2[G[X, Y]](private val value :G[_, _]) extends AnyVal {
		def crosstyped[S, T] :G[S, T] = value.asInstanceOf[G[S, T]]
	}

	implicit class TypeParameterCastingExtension3[G[X, Y, Z]](private val value :G[_, _, _]) extends AnyVal {
		def crosstyped[S, T, U] :G[S, T, U] = value.asInstanceOf[G[S, T, U]]
	}


}
