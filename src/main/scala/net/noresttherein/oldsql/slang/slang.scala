package net.noresttherein.oldsql


package object slang {

	private[oldsql] object && {
		def unapply[T](value :T) = Some(value, value)
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



	private[oldsql] implicit class IfTrueAndIfFalse(val condition :Boolean) extends AnyVal {
		def ifTrue[T](expr: =>T) :Option[T] = if (condition) Some(expr) else None

		def ifFalse[T](expr: =>T) :Option[T] = if (!condition) Some(expr) else None

		def thenTry[T](expr : =>Option[T]) :Option[T] = if (condition) expr else None

		def otherwiseTry[T](expr : =>Option[T]) :Option[T] = if (!condition) expr else None
	}
	
}