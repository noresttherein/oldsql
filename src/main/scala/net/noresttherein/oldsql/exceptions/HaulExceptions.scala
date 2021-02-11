package net.noresttherein.oldsql.exceptions



/** Thrown by implementations of [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom ComposableFrom]]
  * when the collection of individual entities passed to the factory contains an illegal number of elements
  * (for example, more than one for [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Self self]]-compoposition).
  */
class IllegalResultArityException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause)
