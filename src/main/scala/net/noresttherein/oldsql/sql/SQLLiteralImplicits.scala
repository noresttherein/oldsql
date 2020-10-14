package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.sql.SQLLiteralImplicits.nullSQL
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnLiteral, ColumnTerm, CompositeNULL, False, SQLLiteral, SQLParameter, SQLTermFactory, True}





/** A mix in trait extended by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] in order to bring
  * into scope implicit conversions from Scala values to [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral SQLLiteral]]
  * instances declared by the [[net.noresttherein.oldsql.sql.implicitSQLLiterals$ companion object]].
  * @see [[net.noresttherein.oldsql.sql.SQLLiteralImplicits SQLLiteralImplicits]]
  */
trait implicitSQLLiterals



/** Namespace with implicit conversions from Scala literals providing extension methods for creating bound SQL parameter
  * expressions as well as SQL `null` literals.
  */
object implicitSQLLiterals extends SQLLiteralImplicits






sealed trait SQLMultiColumnLiteralImplicits { //todo: exclude Boolean to avoid errors of using == instead of ===
	implicit def implicitLiteral[T :SQLForm](value :T) :SQLTerm[T] = SQLLiteral(value)
}



/** A trait grouping definitions of implicit conversions from Scala literals to
  * [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral SQLLiteral]] expressions as well as extension factory
  * methods for [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]]s
  * ([[net.noresttherein.oldsql.sql.SQLLiteralImplicits.boundParameterSQL.? _.?]]) and
  * [[net.noresttherein.oldsql.sql.SQLTerm.CompositeNULL nulls]]
  * ([[net.noresttherein.oldsql.sql.SQLLiteralImplicits.nullSQL.apply null]]`[T]`).
  *
  * It is extended by the [[net.noresttherein.oldsql.sql.implicitSQLLiterals$ implicitSQLLiterals]] object, which
  * brings the literal conversions into the implicit scope of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * but can also be extended by user's package objects (or other classes/objects) to bring them into the lexical scope
  * of nested types (which will be required for the `null` and parameter factories).
  */
trait SQLLiteralImplicits extends SQLMultiColumnLiteralImplicits {

	implicit def implicitBoolean(value :Boolean) :GlobalBoolean[FromClause] = if (value) True else False

	implicit def implicitColumnLiteral[T :ColumnForm](value :T) :ColumnTerm[T] = ColumnLiteral(value)

	/** Enriches any Scala type for which a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists with a `?` method creating
	  * an [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] expression representing a JDBC parameter
	  * together with its value.
	  */
	implicit class boundParameterSQL[T, P <: SQLParameter[T]](value :T)(implicit factory :SQLTermFactory[T, P]) {
		/** Creates a ''bound'' parameter of an SQL statement represented as an
		  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]. The value of the parameter
		  * is set at this point to `this`, but any SQL ''selects'' using this expression will be translated
		  * to a [[java.sql.PreparedStatement PreparedStatement]] with a parameter placeholder for this expression.
		  * @return an expression of type `P`, being a subtype of
		  *         [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameter SQLParameter]] defined by the available
		  *         implicit [[net.noresttherein.oldsql.sql.SQLTerm.SQLTermFactory SQLTermFactory]] for the type `T`.
		  *         If an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]` is available,
		  *         the result will be a [[net.noresttherein.oldsql.sql.SQLTerm.SQLParameterColumn SQLParameterColumn]].
		  *         Otherwise it will be a `SQLParameter[T]` (using an implicit
		  *         [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]` provided by the factory).
		  */
		@inline def ? :P = factory(value)
	}

	/** Enriches the `null` literal with an `apply[X]` methods which create
	  * [[net.noresttherein.oldsql.sql.SQLTerm SQLTerm]]`[X]` expressions representing the SQL NULL value.
	  */
	implicit def nullSQL(n :Null) :nullSQL = new nullSQL {}
}






object SQLLiteralImplicits {

	@implicitNotFound("Cannot create an SQL NULL expression: no implicit SQLForm[${T}] (or ColumnForm[${T}]).")
	trait SQLNullFactory[T] {
		type NULL <: CompositeNULL[T]
		def apply() :NULL
	}

	implicit def sqlNullFactory[T :SQLForm] :SQLNullFactory[T] { type NULL = CompositeNULL[T] } =
		new SQLNullFactory[T] {
			type NULL = CompositeNULL[T]
			override def apply() = CompositeNULL[T]
		}

	implicit def nullSQLColumnFactory[T :ColumnForm] :SQLNullFactory[T] { type NULL = SQLTerm.NULL[T] } =
		new SQLNullFactory[T] {
			type NULL = SQLTerm.NULL[T]
			override def apply() = SQLTerm.NULL[T]
		}

	/** Extension method for `null` literals which creates SQL NULL expressions. Used in conjunction with the implicit
	  * conversion [[net.noresttherein.oldsql.sql.SQLLiteralImplicits.nullSQL nullSQL]], it allows the syntax of
	  * `null[Int]` to create the expression [[net.noresttherein.oldsql.sql.SQLTerm.NULL NULL]]`[Int]`.
	  */
	trait nullSQL extends Any {
		/** Returns [[net.noresttherein.oldsql.sql.SQLTerm.NULL NULL]]`[X]` or
		  * [[net.noresttherein.oldsql.sql.SQLTerm.CompositeNULL CompositeNULL]]`[X]`, depending on whether
		  * a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]] or
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists for the type T.
		  */
		@inline def apply[T](implicit factory :SQLNullFactory[T]) :factory.NULL = factory()
	}

}

