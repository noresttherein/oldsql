package com.hcore.ogre.slang


package object matching {

	import scala.util.matching.Regex


	object && {
		def unapply[T](value :T) = Some(value, value)
	}

	/**
	 * @see http://hootenannylas.blogspot.com.au/2013/02/pattern-matching-with-string.html for an explanation of
	 *      string interpolation in pattern matching
	 */
	implicit class RegexpInterpolation(val sc :StringContext) extends AnyVal {
		def rx = new Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"):_*)
	}






	implicit class ValuePatternMatching[T](val value :T) extends AnyVal {
		def matches[X](expr :PartialFunction[T, X]) :Boolean = expr.isDefinedAt(value)
	}


}
