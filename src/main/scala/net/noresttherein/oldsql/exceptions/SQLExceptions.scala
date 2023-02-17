package net.noresttherein.oldsql.exceptions

import net.noresttherein.oldsql.schema.{Mapping, Relation}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{DMLStatement, Query, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling







/** Thrown when an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] or
  * DML [[net.noresttherein.oldsql.sql.DML statement]] cannot be formatted as valid SQL/DML,
  * when a [[String]] argument is not a valid SQL, or when valid SQL cannot be created in other context.
  * Note that this type is a trait, so that it can be mixed in to standard Java exceptions such as
  * [[IllegalArgumentException]]. [[net.noresttherein.oldsql.exceptions.MisspelledSQLException MisspelledSQLException]]
  * is the default, most generic implementation which also can serve as a base class for more specific subclasses.
  * Instances can be created by factory methods of the [[net.noresttherein.oldsql.exceptions.InvalidSQLException$ companion]]
  * object to this class.
  */
trait InvalidSQLException extends OldSQLException

object InvalidSQLException {
	def apply(msg :String, cause :Throwable = null) :InvalidSQLException =
		new MisspelledSQLException(msg, cause)

	def apply(expression :SQLExpression.__) :InvalidSQLException = apply("Invalid SQL expression: " + expression + ".")
	def apply(query :Query.__)              :InvalidSQLException = apply("Invalid SQL query: " + query + ".")
	def apply(statement :DMLStatement.__)   :InvalidSQLException = apply("Invalid DML statement: " + statement + ".")
}


/** The default, most generic implementation of
  * [[net.noresttherein.oldsql.exceptions.InvalidSQLException IllegalSQLException]] which does not specify a concrete
  * cause of the exception.
  */
class MisspelledSQLException(msg :String, cause :Throwable = null)
	extends BaseOldSQLException(msg, cause) with InvalidSQLException
{
	def this(expression :SQLExpression.__) = this("Invalid SQL expression: " + expression + ".")
	def this(query :Query.__) = this("Invalid SQL query: " + query + ".")
	def this(statement :DMLStatement.__) = this("Invalid DML statement: " + statement + ".")

	override def addInfo(msg :String) :OldSQLException = new MisspelledSQLException(msg, this)
}


/** Thrown during SQL rendering when a [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
  * is encountered for a component or column whose buffs prevent it from being used in the given context
  * (syntactical scope/clause).
  */
class IllegalComponentException(msg :String, cause :Throwable = null) extends MisspelledSQLException(msg, cause) {
	def this(relation :Relation[MappingAt], component :Mapping)(implicit spelling :SQLSpelling) =
		this({
			val compType = if (component.isInstanceOf[TypedColumn[_, _]]) "Column" else "Component"
			s"$compType $component of $relation is not permitted in " + spelling.scope + "."
		})
}


/** Thrown during rendering of SQL based on an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
  * when a need arises to split a composite expression into SQL for multiple columns, to be interspersed with arbitrary
  * strings, but the expression does not support this functionality. For example, an SQL ''select'' returning multiple
  * columns cannot be used to set values in an SQL ''update'' on a column-by-column basis.
  */
class InseparableExpressionException(val expression :SQLExpression.__, msg :String, cause :Throwable = null)
	extends MisspelledSQLException(msg, cause)
{
	def this(expression :SQLExpression.__)(implicit spelling :SQLSpelling) = this(
		expression, s"Cannot split expression '$expression' into individual columns."
	)

	override def addInfo(msg :String) :OldSQLException = new InseparableExpressionException(expression, msg, this)
}



/** An exception thrown, primarily during the spelling process, when an expression does not have a definite column set
  * and thus can be neither spelled, nor reformed with another expression.
  * A primary example would be a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] with member selects
  * of conflicting shapes.
  */
//Consider: either using it also for LooseComponent, or update docs and use UndefinedShapeException
class MisalignedExpressionException(msg :String, cause :Throwable = null)
	extends IllegalStateException(msg, cause) with InvalidSQLException
{
	def this(e :SQLExpression.__) = this("Expression " + e + " has an internally inconsistent column set.")

	def this(e :SQLExpression.__, msg :String) =
		this("Expression " + e + " has an internally inconsistent column set: " + msg)

	override def addInfo(msg :String) :OldSQLException = new MisalignedExpressionException(msg, this)
}


class UndefinedShapeException(msg :String, cause :Throwable = null)
	extends UnsupportedOperationException(msg, cause) with InvalidSQLException
{
	def this(sql :SQLExpression.__) = this("Expression `" +sql + "`: " + sql.className +" has an undefined column set." )

	override def addInfo(msg :String) :OldSQLException = new UndefinedShapeException(msg, this)
}


/** Thrown when an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] or
  * a DML [[net.noresttherein.oldsql.sql.DML statement]] cannot be formatted as valid SQL/DML because it contains
  * two subexpressions which should contain the same number (and types) of columns, but cannot be molded
  * to a common shape. This is the case when two expressions with the same value type are used in a comparison,
  * assignment or ''select'' clauses of composites of a compound ''select'' have different implementation and structure.
  */
class MismatchedExpressionsException(msg :String, cause :Throwable = null) extends MisspelledSQLException(msg, cause) {
	def this(left :SQLExpression.__, right :SQLExpression.__) = //todo: SQLExpression.dumpString
		this("Cannot align expressions with different column sets: " + left + " vs. " + right +
		     " :(" + left.getClass.getName + ", " + right.getClass.getName + ").")

	def this(left :SQLExpression.__, right :SQLExpression.__, msg :String) =
		this("Cannot align expressions (" + left + ") and (" + right + "); " + msg)

	def this(left :Query.__, right :Query.__) =
		this("Cannot align queries with different column sets: " + left + " vs. " + right + ".")

	def this(left :Query.__, right :Query.__, msg :String) =
		this("Cannot align queries (" + left + ") and (" + right + "); " + msg)

	override def addInfo(msg :String) :OldSQLException = new MismatchedExpressionsException(msg, this)
}


/** Exception thrown when a given name does not uniquely identify of an object, such as a table in a ''from'' clause. */
class AmbiguousAliasException(msg :String, cause :Throwable = null)
	extends BaseOldSQLException(msg, cause) with InvalidSQLException
{
	override def addInfo(msg :String) :OldSQLException = new AmbiguousAliasException(msg, this)
}


/** Thrown when an SQL expression provided as a subexpression for some composite expression is incompatible.
  * This is in particular used when transcribing expressions
  * with an [[net.noresttherein.oldsql.sql.mechanics.SQLScribe SQLScribe]], for example if an expression
  * wrapping a `ComponentSQL` is given an expression of some other type as a replacement.
  */
class IllegalExpressionException(msg :String, cause :Throwable = null) extends BaseOldSQLException(msg, cause) {
	def this(e :SQLExpression.__, msg :String) = this("Unexpected expression type " + e + " :" + e.className + ".")

	override def addInfo(msg :String) :OldSQLException = new IllegalExpressionException(msg, this)
}


/** An exception thrown when the spelling of an SQL expression must be postponed until execution time because
  * of its dependence on SQL statement parameter values.
  * This is the case when a column with a [[net.noresttherein.oldsql.schema.Buff.CustomInsert CustomInsert]] or
  * [[net.noresttherein.oldsql.schema.Buff.CustomUpdate CustomUpdate]] buff is being assigned to.
  */
class ExecutionTimeSQLException(msg :String, cause :Throwable = null)
	extends BaseOldSQLException(msg, cause) with InvalidSQLException
{
	def this(table :Relation.__, column :TypedColumn[_, _]) =
		this(s"Column $column of $table cannot be assigned a value without access to statement parameters. " +
			"SQL can only be generated at execution time.")

	override def addInfo(msg :String) :OldSQLException = new ExecutionTimeSQLException(msg, this)
}



class SpellingWarning(msg :String, cause :Throwable = null) extends MisspelledSQLException(msg, cause)

