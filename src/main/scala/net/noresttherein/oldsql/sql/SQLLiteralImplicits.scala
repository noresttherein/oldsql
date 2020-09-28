package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.sql.SQLLiteralImplicits.nullSQL
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnLiteral, ColumnTerm, CompositeNULL, False, SQLLiteral, SQLParameter, SQLTermFactory, True}





/** A mix in trait extended by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] in order to bring
  * into scope implicit conversions from Scala values to [[net.noresttherein.oldsql.sql.SQLTerm.SQLLiteral SQLLiteral]]
  * instances declared by the companion object.
  */
trait implicitSQLLiterals


/** Namespace with implicit conversions from Scala literals providing extension methods for creating bound SQL parameter
  * expressions as well as SQL `null` literals.
  */
object implicitSQLLiterals extends SQLLiteralImplicits



sealed trait SQLMultiColumnLiteralImplicits {
	implicit def implicitLiteral[T :SQLForm](value :T) :SQLTerm[T] = SQLLiteral(value)
}



/**
  * @author Marcin Mościcki
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

