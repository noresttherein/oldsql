package net.noresttherein.oldsql


package object slang {

	private[oldsql] object && {
		def unapply[T](value :T) = Some(value, value)
	}



	/** An implicit conversion extending Int with a method 'repeat' which executes a block the given number of times. */
	private[oldsql] implicit class repeat(private val count :Int) extends AnyVal {
		/** Execute the given block the number of times specified by 'this' argument. */
		def times(block : =>Unit): Unit =
			for (i<- 0 until count) block

	}



	private[oldsql] implicit class UnqualifiedClassName[T](private val value :T) extends AnyVal {
		def unqualifiedClassName :String = {
			val qualified = value.getClass.getName
			val i = qualified.lastIndexOf('.')
			qualified.substring(i + 1, if (qualified.last != '$') qualified.length else qualified.length-1)
		}

		def innerClassName :String = {
			val qualified = value.getClass.getName
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
		def providing(condition :T=>Boolean) :Option[T] = {
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
		def unless(condition :T=>Boolean) :Option[T] = {
			val x = expr
			if (!condition(x)) Some(x) else None
		}



		/** Option(expr); helpful for anonymous functions when Option(_) won't compile */
		def toOption = Option(expr)

	}



	private[oldsql] implicit class IfTrueAndIfFalse(private val condition :Boolean) extends AnyVal {
		def ifTrue[T](expr: =>T) :Option[T] = if (condition) Some(expr) else None

		def ifFalse[T](expr: =>T) :Option[T] = if (!condition) Some(expr) else None

		def thenTry[T](expr : =>Option[T]) :Option[T] = if (condition) expr else None

		def otherwiseTry[T](expr : =>Option[T]) :Option[T] = if (!condition) expr else None
	}



	private[oldsql] implicit class OptionGuardExtension[T](opt : =>Option[T]) {
		def orNoneIf(expr :Boolean) :Option[T] =
			if (expr) None else opt

		def orNoneUnless(expr :Boolean) :Option[T] =
			if (expr) opt else None

		def mapOrElse[X](expr : T=>X, default : =>X) :X = opt match {
			case Some(t) => expr(t)
			case none => default
		}
	}

}