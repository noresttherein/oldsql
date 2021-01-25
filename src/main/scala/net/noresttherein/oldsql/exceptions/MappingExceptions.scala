package net.noresttherein.oldsql.exceptions





class NoSuchComponentException(msg :String, cause :Throwable = null)
	extends NoSuchElementException(msg) with OldSQLException
{
	initCause(cause)
}


/**
  * @author Marcin Mościcki
  */
class MissingKeyException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause)


class MismatchedKeyException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause)



/** An exception thrown when the [[net.noresttherein.oldsql.schema.Mapping.optMap Mapping.optMap]] operation
  * fails due to presence of a buff which cannot be mapped with the given function, either because
  * the operation is not supported at all, or the function returned `None` for the `Buff`'s value.
  * This exception can be thrown both from the `optMap` method and at some later point, when the buff's value
  * is accessed for buffs which generated values.
  */
class BuffMappingFailureException(msg :String, cause :Throwable = null)
	extends RuntimeException(msg, cause) with OldSQLException



