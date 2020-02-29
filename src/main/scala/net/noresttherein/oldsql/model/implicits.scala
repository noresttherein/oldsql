package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.Restraint.{BooleanRestraint, False, True}
import net.noresttherein.oldsql.model.Restrictive.{Collection, Literal, TranslableTerm}
import net.noresttherein.oldsql.morsels.PropertyPath

import scala.reflect.runtime.universe.TypeTag

/** A trait extended by various model classes in order to include the implicits inside its companion object
  * in the search of implicit conversions. This allows one to both use automatic implicit resolution
  * (when the source/target type is known) as well as explicitly import the conversions.
  */
trait implicits


sealed abstract class StandardFallbackImplicits {
	implicit def arbitraryLiteral[T](value :T) :TranslableTerm[Any, T] = Literal(value)
}

/** Implicit conversions to and from classes used to define `Restraint`s.
  * Classes (and traits) covered by these conversions extend the trait
  * [[net.noresttherein.oldsql.model.implicits implicits]], so, as a companion object of a super type, its declarations
  * are considered when searching for implicit values related to these types. At the same time, it is possible
  * to import everything wholesale explicitly and this will include also conversions from unrelated classes such
  * as functions or [[net.noresttherein.oldsql.morsels.PropertyPath PropertyPath]], which would not be found
  * when searching for a implicit conversion forced by a call to a method not present on the converted object.
  */
object implicits extends StandardFallbackImplicits {
	implicit def booleanRestraint[T](term :Restrictive[T, Boolean]) :Restraint[T] =
		new BooleanRestraint[T](term)

	implicit def booleanRestraint[T](property :PropertyPath[T, Boolean]) :Restraint[T] =
		new BooleanRestraint[T](Restrictive.Property(property))

	implicit def booleanRestraint[T :TypeTag](property :T => Boolean) :Restraint[T] =
		new BooleanRestraint[T](Restrictive.Property(property))

	implicit def booleanRestraint[T](value :Boolean) :Restraint[T] =
		if (value) True else False




	@inline implicit def propertyRestrictive[T, V](property :PropertyPath[T, V]) :Restrictive[T, V] =
		Restrictive.Property(property)

	@inline implicit def functionRestrictive[T :TypeTag, V](property :T=>V) :Restrictive[T, V] =
		Restrictive.Property(property)


	@inline implicit def collectionRestrictive[T, V](collection :Iterable[Restrictive[T, V]]) :Restrictive[T, Iterable[V]] =
		Collection(collection)

	@inline implicit def compositeRestrictive[T, C, V](collection :Iterable[Restrictive[T, V]])
	                                                  (implicit comp :C ComposedOf V) :Restrictive[T, C] =
		Collection(collection)


	@inline implicit def literalRestrictive(byte :Byte) :Restrictive[Any, Byte] = Literal(byte)
	@inline implicit def literalRestrictive(short :Short) :Restrictive[Any, Short] = Literal(short)
	@inline implicit def literalRestrictive(int :Int) :Restrictive[Any, Int] = Literal(int)
	@inline implicit def literalRestrictive(long :Long) :Restrictive[Any, Long] = Literal(long)
	@inline implicit def literalRestrictive(float :Float) :Restrictive[Any, Float] = Literal(float)
	@inline implicit def literalRestrictive(double :Double) :Restrictive[Any, Double] = Literal(double)
	@inline implicit def literalRestrictive(char :Char) :Restrictive[Any, Char] = Literal(char)
	@inline implicit def literalRestrictive(boolean :Boolean) :Restrictive[Any, Boolean] = Literal(boolean)
	
	@inline implicit def literalRestrictive(big :BigInt) :Restrictive[Any, BigInt] = Literal(big)
	@inline implicit def literalRestrictive(big :BigDecimal) :Restrictive[Any, BigDecimal] = Literal(big)
	//todo: ambiguous conversion for adding apply to string
//	@inline implicit def literalRestrictive[T](string :String) :Restrictive[T, String] = Literal(string)
	

}

