package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.Restraint.{BooleanRestraint, False, True}
import net.noresttherein.oldsql.model.Restrictive.{Collection, Literal, TranslatableTerm}
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.model.types.LiteralSupport

/** A trait extended by various model classes in order to include the implicits inside its companion object
  * in the search of implicit conversions. This allows one to both use automatic implicit resolution
  * (when the source/target type is known) as well as explicitly import the conversions. In this way
  * we can link the implicit conversions to the available lists of all concerned types ''and'' at the same time
  * use a wholesale implicit import for situations when the type being converted is not defined here; defining these
  * implicits separately would in this case result in conflicts.
  */ //todo: remove, this is unnecessary: import implicits._ does not conflict with implicits in companion objects
private[model] trait implicits



private[model] sealed abstract class StandardFallbackImplicits {
	implicit def arbitraryLiteral[T](value :T) :TranslatableTerm[Any, T] = Literal(value)
}


/** Implicit conversions to and from classes used to define `Restraint`s.
  * Classes (and traits) covered by these conversions extend the trait
  * [[net.noresttherein.oldsql.model.implicits implicits]], so, as a companion object of a super type, its declarations
  * are considered when searching for implicit values related to these types. At the same time, it is possible
  * to import everything wholesale explicitly and this will include also conversions from unrelated classes such
  * as functions or [[net.noresttherein.oldsql.model.PropertyPath PropertyPath]], which would not be found
  * when searching for a implicit conversion forced by a call to a method not present on the converted object.
  */
object implicits extends StandardFallbackImplicits {
	implicit def booleanRestrictiveRestraint[T](term :Restrictive[T, Boolean]) :Restraint[T] =
		new BooleanRestraint[T](term)

	implicit def booleanPathRestraint[T](property :PropertyPath[T, Boolean]) :Restraint[T] =
		new BooleanRestraint[T](Restrictive.Property(property))

	implicit def booleanPropertyRestraint[T :TypeTag](property :T => Boolean) :Restraint[T] =
		new BooleanRestraint[T](Restrictive.Property(property))

	implicit def booleanRestraint[T](value :Boolean) :Restraint[T] =
		if (value) True else False




	@inline implicit def propertyRestrictive[T, V](property :PropertyPath[T, V]) :Restrictive[T, V] =
		Restrictive.Property(property)

	@inline implicit def functionRestrictive[T :TypeTag, V](property :T => V) :Restrictive[T, V] =
		Restrictive.Property(property)


	@inline implicit def literalTermCollection[V](collection :Iterable[V]) :Iterable[TranslatableTerm[Any, V]] =
		collection.map(Literal.apply)

	@inline implicit def collectionRestrictive[T, V](collection :Iterable[Restrictive[T, V]]) :Restrictive[T, Iterable[V]] =
		Collection(collection)

	@inline implicit def compositeRestrictive[T, C, V](collection :Iterable[Restrictive[T, V]])
	                                                  (implicit comp :C ComposedOf V) :Restrictive[T, C] =
		Collection(collection)


	@inline implicit def literalRestrictive[T :LiteralSupport](literal :T) :Restrictive[Any, T] = Literal(literal)
	@inline implicit def stringRestrictive[T](string :String) :Restrictive[T, String] = Literal(string)
	

}

