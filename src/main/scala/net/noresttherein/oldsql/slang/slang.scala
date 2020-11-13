package net.noresttherein.oldsql

import scala.reflect.{classTag, ClassTag}
import scala.util.Try






package object slang {

	private[oldsql] object && {
		def unapply[T](value :T) = Some(value, value)
	}



	/** An implicit conversion extending Int with a method 'repeat' which executes a block the given number of times. */
	private[oldsql] implicit class repeat(private val count :Int) extends AnyVal {
		/** Execute the given block the number of times specified by 'this' argument. */
		def times(block : =>Unit): Unit =
			for (_ <- 0 until count) block

	}



	private[oldsql] implicit class UnqualifiedClassName[T](private val value :T) extends AnyVal {
		@inline def unqualifiedClassName :String = slang.unqualifiedClassName(value.getClass)

		@inline def innerClassName :String = slang.innerClassName(value.getClass)
	}

	def unqualifiedClassName(clazz :Class[_]) :String = {
		val qualified = clazz.getName
		val i = qualified.lastIndexOf('.')
		qualified.substring(i + 1, if (qualified.last != '$') qualified.length else qualified.length-1)

	}

	def innerClassName(clazz :Class[_]) :String = {
		val qualified = clazz.getName
		val len = qualified.length
		val unqualified = qualified.lastIndexOf('.') + 1
		val anon = qualified.indexOf("$anon", unqualified)
		val end =
			if (anon >= 0) anon
			else {
				var i = len - 1
				while (i > unqualified && qualified(i) == '$')
					i -= 1
				i
			}
		val innermost = qualified.lastIndexOf('$', end - 1)
		if (innermost < 0) qualified.substring(unqualified, end)
		else qualified.substring(innermost + 1, end)
	}






	/** Implicit conversion to a lazy value of type T which can be lifted to an Option[T] by one of its methods. */
	private[oldsql] implicit class ProvidingAndUnless[T](expr: =>T) {

		/** Returns Some(this) if passed condition is true, None otherwise;
		  * `this` is passed by name and evaluated only if condition is true!
		  */
		def providing(condition :Boolean) :Option[T] =
			if (condition) Some(expr) else None

		/** Returns Some(this) if passed condition is true for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		def providing(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (condition(x)) Some(x) else None
		}


		/** Returns Some(this) if passed condition is false, None otherwise;
		  * `this` is passed by name and evaluated only if condition is false!
		  */
		def unless(condition :Boolean) :Option[T] =
			if (!condition) Some(expr) else None


		/** Returns Some(this) if passed condition is false for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		def unless(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (!condition(x)) Some(x) else None
		}

	}



	private[oldsql] implicit class IfTrueAndIfFalse(private val condition :Boolean) extends AnyVal {
		def ifTrue[T](expr: => T) :Option[T] = if (condition) Some(expr) else None

		def ifFalse[T](expr: => T) :Option[T] = if (!condition) Some(expr) else None

		def thenMaybe[T](expr : => Option[T]) :Option[T] = if (condition) expr else None

		def otherwiseMaybe[T](expr : => Option[T]) :Option[T] = if (!condition) expr else None
	}



	private[oldsql] implicit class OptionGuardExtension[T](opt : => Option[T]) {
		def orNoneIf(expr :Boolean) :Option[T] =
			if (expr) None else opt

		def orNoneUnless(expr :Boolean) :Option[T] =
			if (expr) opt else None

		def mapOrElse[X](expr : T => X, default : => X) :X = opt match {
			case Some(t) => expr(t)
			case none => default
		}
	}





	private[oldsql] implicit class CastingExtension[T](private val value :T) extends AnyVal {
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




	private[oldsql] class CastValueGuard[T, S](val value :T) extends AnyVal {

		def apply[X](block :S=>X)(implicit S :ClassTag[S]) :Option[X] = value match {
			case S(s) => Some(block(s))
			case _ => None
		}

		def orElse[X](block :S=>X)(alternative : =>X)(implicit S: ClassTag[S]) :X = value match {
			case S(s) => block(s)
			case _ => alternative
		}
	}



	private[oldsql] implicit class TypeParameterCastingExtension1[G[_]](private val value: G[_]) extends AnyVal {
		@inline def crosstyped[S] :G[S] = value.asInstanceOf[G[S]]
	}

	private[oldsql] implicit class TypeParameterCastingExtension2[G[_, _]](private val value :G[_, _]) extends AnyVal {
		@inline def crosstyped[S, T] :G[S, T] = value.asInstanceOf[G[S, T]]
	}

	private[oldsql] implicit class TypeParameterCastingExtension3[G[_, _, _]](private val value :G[_, _, _]) extends AnyVal {
		@inline def crosstyped[S, T, U] :G[S, T, U] = value.asInstanceOf[G[S, T, U]]
	}






	final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	final def raise[E <: Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get

}
