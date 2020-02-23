package com.hcore.ogre.slang.options

/** Implicit extensions for working with options, mainly facilitating lifting some expression E to Option[E] based on a boolean guard condition */
object extensions {


	/** Implicit conversion to a lazy value of type T which can be lifted to an Option[T] by one of its methods. */
	implicit class ProvidingAndUnless[T](expr: =>T) {

		/** Returns Some(this) if passed condition is true, None otherwise;
		  * <code>this</code> is passed by name and evaluated only if condition is true!
		  */
		def providing(condition :Boolean) :Option[T] =
			if (condition) Some(expr) else None

		/** Returns Some(this) if passed condition is true for this, None otherwise;
		  * <code>this</code> is evaluated once, before passing its value to the condition!
		  */
		def providing(condition :T=>Boolean) :Option[T] = {
			val x = expr
			if (condition(x)) Some(x) else None
		}


		/** Returns Some(this) if passed condition is false, None otherwise;
		  * <code>this</code> is passed by name and evaluated only if condition is false!
		  */
		def unless(condition :Boolean) :Option[T] =
			if (!condition) Some(expr) else None


		/** Returns Some(this) if passed condition is false for this, None otherwise;
		  * <code>this</code> is evaluated once, before passing its value to the condition!
		  */
		def unless(condition :T=>Boolean) :Option[T] = {
			val x = expr
			if (!condition(x)) Some(x) else None
		}

		/** Returns this if passed condition is true, alternative if not.
		  * 'this' expression is evaluated only if condition is true, alternative is evaluated only if condition is false.
		  * Equivalent to this.providing(condition).orElse(alternative).
		  */
		def providingOrElse[X>:T](condition :Boolean, alternative : =>X) :X =
			if (condition) expr else alternative

		/** Returns this if passed condition is true, alternative if not.
		  * 'this' expression is evaluated once, before passing to the condition function; alternative is evaluated only if condition is false.
		  * Equivalent to this.providing(condition).orElse(alternative).
		  */
		def providingOrElse[X>:T](condition :T=>Boolean, alternative : =>X) :X = {
			val x = expr
			if (condition(x)) x else alternative
		}

		/** Returns this if passed condition is false, alternative if not.
		  * 'this' expression is evaluated only if condition is false, alternative is evaluated only if condition is true.
		  * Equivalent to this.unless(condition).orElse(alternative).
		  */
		def unlessThen[X>:T](condition :Boolean, alternative : =>X) :X =
			if (condition) expr else alternative

		/** Returns this if passed condition is false, alternative if not.
		  * 'this' expression is evaluated once, before passing to the condition function; alternative is evaluated only if condition is true.
		  * Equivalent to this.unless(condition).orElse(alternative).
		  */
		def unlessThen[X>:T](condition :T=>Boolean, alternative : =>X) :X = {
			val x = expr
			if (condition(x)) x else alternative
		}



		def providingOrThrow(condition :Boolean, msg : =>String =null) :T =
			if (condition) expr
			else throw new RuntimeException(if (msg==null) s"failed guard for $expr" else msg)

		def providingOrThrow(condition :T=>Boolean, msg : =>String) :T = {
			val x = expr
			if (condition(x)) x
			else throw new RuntimeException(msg)
		}



		def unlessThenThrow(condition :Boolean, msg : =>String=null) :T =
			if (condition) throw new RuntimeException(if (msg==null) s"failed guard for $expr" else msg)
			else expr

		def unlessThenThrow(condition :T=>Boolean, msg : =>String) :T = {
			val x = expr
			if (condition(x)) throw new RuntimeException(msg)
			else x
		}


		/** Option(expr); helpful for anonymous functions when Option(_) won't compile */
		def toOption = Option(expr)

	}



	implicit class IfTrueAndIfFalse(val condition :Boolean) extends AnyVal {
		def ifTrue[T](expr: =>T) = if (condition) Some(expr) else None

		def ifFalse[T](expr: =>T) = if (!condition) Some(expr) else None

		def thenTry[T](expr : =>Option[T]) = if (condition) expr else None

		def otherwiseTry[T](expr : =>Option[T]) = if (!condition) expr else None
	}


	implicit class OptionGuardExtension[T](opt : =>Option[T]) {
		def orNoneIf(expr :Boolean) =
			if (expr) None else opt

		def orNoneUnless(expr :Boolean) =
			if (expr) opt else None

		def mapOrElse[X](expr : T=>X, default : =>X) = opt match {
			case Some(t) => expr(t)
			case none => default
		}
	}


	/** Extension of Some[T] providing a safe counterpart of get method. While calling get on Some is
	  * safe, changing the static type of expression (due to a change in other place in the code)
	  * may introduce hidden bugs. When this implicit conversion is imported,
	  * some.someValue can be called for some :Some[_]. Chaning the type of some to Option[_]
	  * will result in a compile error.
	  */
	implicit class SomeGuardExtension[T](val some :Some[T]) extends AnyVal {
		/** this.get by implicit conversion available only from Some[T] - will never throw an exception */
		@inline
		final def getValue :T = some.get
	}


	object AnyToOption {
		implicit def option[T](value :T) :Option[T] = Option(value)
	}

	object ValueOfSome {
		implicit def valueOfSome[T](some :Some[T]) :T = some.get
	}


	def NoneOf[T] :Option[T] = None
//	implicit def AnyToOption[T](value :T) :Option[T] = Option(value)

}
