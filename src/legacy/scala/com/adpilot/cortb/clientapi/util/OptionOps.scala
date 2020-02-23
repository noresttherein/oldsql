package com.adpilot.cortb.clientapi.util


object OptionOps {

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

		/** Option(expr); helpful for anonymous function when Option(_) won't compile */
		def toOption = Option(expr)

	}

	implicit class IfTrueAndIfFalse(val condition :Boolean) extends AnyVal {
		def ifTrue[T](expr: =>T) = if (condition) Some(expr) else None

		def ifFalse[T](expr: =>T) = if (!condition) Some(expr) else None
	}

	object AnyToOption {
		implicit def option[T](value :T) = Option(value)
	}


	def NoneOf[T] :Option[T] = None
//	implicit def AnyToOption[T](value :T) :Option[T] = Option(value)
	
}
