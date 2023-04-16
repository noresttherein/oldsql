package net.noresttherein.oldsql.exceptions



/** Thrown by implementations of [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom ComposableFrom]]
  * when the collection of individual entities passed to the factory contains an illegal number of elements
  * (for example, more than one for [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Self self]]-composition).
  */
class IllegalResultArityException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	override def addInfo(msg :String) :OldSQLException = new IllegalResultArityException(msg, this)
}

class TooManyResultsException(msg :String, cause :Throwable = null) extends IllegalResultArityException(msg, cause) {
	override def addInfo(msg :String) :OldSQLException = new TooManyResultsException(msg, this)
}

/** Thrown when an implementation attempts to access either the update count or a [[java.sql.ResultSet ResultSet]]
  * on a [[java.sql.PreparedStatement PreparedStatements]] which does not have additional results.
  * Note that it doesn't concern itself with the number of ''rows'' in a returned `ResultSet`, which is the domain
  * of [[net.noresttherein.oldsql.exceptions.IllegalResultArityException IllegalResultArityException]].
  * This is most likely a bug either in the framework or custom extending classes rather than application code.
  */
class MissingStatementResultBug(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) with Bug {
	override def addInfo(msg :String) :OldSQLException = new MissingStatementResultBug(msg, this)
}
