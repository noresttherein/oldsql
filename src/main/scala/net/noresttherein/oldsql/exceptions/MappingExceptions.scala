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



