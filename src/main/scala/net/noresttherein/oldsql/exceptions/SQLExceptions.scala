package net.noresttherein.oldsql.exceptions

import net.noresttherein.oldsql.sql.{DMLStatement, Query, SQLExpression}






/** Thrown when an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] or
  * DML [[net.noresttherein.oldsql.sql.DML statement]] cannot be formatted as valid SQL/DML,
  * when a [[String]] argument is not a valid SQL, or when valid SQL cannot be created in other context.
  * Note that this class is a trait, so that it can be mixed in to standard Java exceptions such as
  * [[IllegalArgumentException]]. [[net.noresttherein.oldsql.exceptions.MisspelledSQLException MisspelledSQLException]]
  * is the default, most generic implementation which also can serve as a base class for more specific subclasses.
  * Instances can be created by factory methods of the [[net.noresttherein.oldsql.exceptions.IllegalSQLException$ companion]]
  * object to this class.
  */
trait IllegalSQLException extends OldSQLException

object IllegalSQLException {
	def apply(msg :String, cause :Throwable = null) :IllegalSQLException =
		new MisspelledSQLException(msg, cause)

	def apply(expression :SQLExpression.*) :IllegalSQLException = apply("Invalid SQL expression: " + expression + ".")
	def apply(query :Query.*)              :IllegalSQLException = apply("Invalid SQL query: " + query + ".")
	def apply(statement :DMLStatement.*)   :IllegalSQLException = apply("Invalid DML statement: " + statement + ".")

}


/** The default, most generic implementation of
  * [[net.noresttherein.oldsql.exceptions.IllegalSQLException IllegalSQLException]] which does not specify a concrete
  * cause of the exception.
  */
class MisspelledSQLException(msg :String, cause :Throwable = null)
	extends BaseOldSQLException(msg, cause) with IllegalSQLException
{
	def this(expression :SQLExpression.*) = this("Invalid SQL expression: " + expression + ".")
	def this(query :Query.*) = this("Invalid SQL query: " + query + ".")
	def this(statement :DMLStatement.*) = this("Invalid DML statement: " + statement + ".")

	override def stackOn(msg :String) :OldSQLException = new MisspelledSQLException(msg, this)
}


/** Thrown during rendering of SQL based on an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
  * when a need arises to split a composite expression into SQL for multiple columns, to be interspersed with arbitrary
  * strings, but the expression does not support this functionality. For example, an SQL ''select'' returning multiple
  * columns cannot be used to set values in an SQL ''update'' on a column-by-column basis.
  */
class InseparableExpressionException(msg :String, cause :Throwable = null) extends MisspelledSQLException(msg, cause) {
	def this(expression :SQLExpression.*) = this(
		s"Cannot split expression '$expression' of ${expression.readForm.readColumns} columns into individual columns."
	)

	override def stackOn(msg :String) :OldSQLException = new InseparableExpressionException(msg, this)
}



/** Thrown when an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] or
  * DML [[net.noresttherein.oldsql.sql.DML statement]] cannot be formatted as valid SQL/DML because it contains
  * two subexpressions which should contain the same number (and types) of columns, but cannot be fitted to a common shape.
  * This is the case when two expressions with the same value type are used in a comparison, assignment or ''select''
  * clauses of composites of a compound ''select'' have different implementation and structure.
  */
class MismatchedExpressionsException(msg :String, cause :Throwable = null) extends MisspelledSQLException(msg, cause) {
	override def stackOn(msg :String) :OldSQLException = new MismatchedExpressionsException(msg, this)
}


