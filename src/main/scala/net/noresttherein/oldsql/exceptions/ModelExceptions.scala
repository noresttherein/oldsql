package net.noresttherein.oldsql.exceptions

import net.noresttherein.oldsql.model.Kin






/** Thrown when some composite type `T` should be [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo decomposed]]
  * to a collection of some element type `E` which is incompatible with the internal composition of `T` in that
  * the actual element type does not match the intended element type. This is in particular the case when
  * a [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin is to be decomposed into a
  * [[net.noresttherein.oldsql.model.Kin Kin]] collecting values of a certain property for all its elements,
  * but the [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo decomposition]] and
  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom composition]] are not inverse functions.
  */
class IncompatibleElementTypeException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	override def stackOn(msg :String) :OldSQLException = new IncompatibleElementTypeException(msg, this)
}


trait IllegalReferenceException extends OldSQLException

/** A general base trait for exceptions thrown when a value for a [[net.noresttherein.oldsql.model.Kin Kin]]
  * or a similar reference type cannot be assembled.
  */
trait KinCompositionException extends IllegalReferenceException

/** Thrown by implementations of [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom ComposableFrom]]
  * when the collection of individual entities passed to the factory contains an illegal number of elements
  * (for example, more than one for [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Self self]]-composition).
  */
class IllegalKinArityException(msg :String, cause :Throwable = null)
	extends IllegalResultArityException(msg, cause) with KinCompositionException
{
	override def stackOn(msg :String) :OldSQLException = new IllegalKinArityException(msg, this)
}

/** Thrown after a failure to resolve a [[net.noresttherein.oldsql.model.Kin.One One]] kin, representing a mandatory
  * relationship with a single entity, if the referenced entity does not exist in the underlying database.
  */
class NonexistentEntityException(msg :String, ex :Throwable = null)
	extends NoSuchElementException(msg) with KinCompositionException
{
	def this() = this("Specified entity does not exist.")
	def this(kin :Kin[_]) = this("Entity specified by " + kin + " does not exist.")

	initCause(ex)

	override def stackOn(msg :String) :OldSQLException = new NonexistentEntityException(msg, this)
}


/** Thrown when a value is accessed on a [[net.noresttherein.oldsql.model.Kin Kin]] instance which
  * doesn't have it loaded or which references no existing value. This comes primarily from direct and indirect
  * invocations of [[net.noresttherein.oldsql.model.Kin.get Kin.get]] when
  * [[net.noresttherein.oldsql.model.Kin.isAbsent isAbsent]] is true.
  */
class AbsentKinException(msg :String, ex :Throwable = null)
	extends NoSuchElementException(msg) with OldSQLException
{
	def this(kin :Kin[_]) = this("Absent kin: " + kin + ".")

	initCause(ex)

	override def stackOn(msg :String) :OldSQLException = new AbsentKinException(msg, this)
}
