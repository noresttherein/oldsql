package net.noresttherein.oldsql.exceptions

import net.noresttherein.oldsql.schema.Mapping






/** Thrown primarily by mappings, when a method which accepts only components of the mapping,
  * such as [[net.noresttherein.oldsql.schema.Mapping.export export]], is passed a mapping which neither
  * a direct or indirect component of said mapping, in any version. Other classes,
  * such as [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]
  * and [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]] instances storing
  * [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] of a mapping may also throw this exception
  * when given an unrecognized mapping as an argument.
  */
class NoSuchComponentException(msg :String, cause :Throwable = null)
	extends NoSuchElementException(msg) with OldSQLException
{
	def this(owner :Mapping, component :Mapping) = this("Mapping " + component + " is not a component of " + owner + ".")

	initCause(cause)

	override def addInfo(msg :String) :OldSQLException = new NoSuchComponentException(msg, this)
}



/** Thrown when mapping `m2` should substitute mapping `m1`, but their component structures are incompatible
  * from the point of view of the particular use case. Compatibility is typically expressed through
  * methods [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]],
  * [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]], etc.
  */
class IncompatibleMappingsException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	def this(m1 :Mapping, m2 :Mapping, relation :String, msg :String) =
		this(msg + ": mapping " + m1.debugString + " is not " + relation + " with " + m2.debugString + ".")

	override def addInfo(msg :String) :OldSQLException = new IncompatibleMappingsException(msg, this)
}


/**
  */
class MissingKeyException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	override def addInfo(msg :String) :OldSQLException = new MissingKeyException(msg, this)
}


class MismatchedKeyException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	override def addInfo(msg :String) :OldSQLException = new MismatchedKeyException(msg, this)
}



/** An exception thrown when the [[net.noresttherein.oldsql.schema.Mapping.optMap Mapping.optMap]] operation
  * fails due to presence of a buff which cannot be mapped with the given function, either because
  * the operation is not supported at all, or the function returned `None` for the `Buff`'s value.
  * This exception can be thrown both from the `optMap` method and at some later point, when the buff's value
  * is accessed for buffs which generated values.
  */
class BuffMappingFailureException(msg :String, cause :Throwable = null)
	extends RuntimeException(msg, cause) with OldSQLException
{
	override def addInfo(msg :String) :OldSQLException =
		new BuffMappingFailureException(msg, this)
}



/** An exception thrown by mappings and forms if they were to return a `null` value to the application
  * and the component/column is not nullable. Note that this is a subclass of standard [[NullPointerException]],
  * in order to allow catching both recognized and intentionally reported errors resulting from encountering
  * a `null` value, and those the application was not prepared to handle.
  */
class NullValueException(msg :String, cause :Throwable = null)
	extends NullPointerException(msg) with OldSQLException
{
	if (cause != null) initCause(cause)

	override def addInfo(msg :String) :OldSQLException = new NullValueException(msg, this)
}


