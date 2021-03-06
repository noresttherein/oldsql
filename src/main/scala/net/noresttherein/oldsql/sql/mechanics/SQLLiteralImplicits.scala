package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitAmbiguous

import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.sql.{GlobalBoolean, RowProduct}
import net.noresttherein.oldsql.sql.ast.{SQLNull, SQLParameter, SQLTerm}
import net.noresttherein.oldsql.sql.ast.SQLLiteral.{False, True}
import net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm
import net.noresttherein.oldsql.sql.ast.SQLParameter.ParameterFactory
import net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.nullSQL






/** A mix in trait extended by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] in order to bring
  * into scope implicit conversions from Scala values to [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]]
  * instances declared by the [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals$ companion object]].
  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits SQLLiteralImplicits]]
  */
trait implicitSQLLiterals


/** Namespace with implicit conversions from Scala literals providing extension methods for creating bound SQL parameter
  * expressions as well as SQL `null` literals.
  */
object implicitSQLLiterals extends SQLLiteralImplicits


sealed trait LowPrioritySQLLiteralImplicits {
	implicit def implicitLiteral[T :SQLForm](value :T) :SQLTerm[T] = SQLTerm(value)
}


/** A trait grouping definitions of implicit conversions from Scala literals to
  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] expressions as well as extension factory
  * methods for [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]]s
  * ([[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.boundParameterSQL.? _.?]]) and
  * [[net.noresttherein.oldsql.sql.ast.CompositeNull nulls]]
  * ([[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.nullSQL.apply null]]`[T]`).
  *
  * It is extended by the [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals$ implicitSQLLiterals]] object, which
  * brings the literal conversions into the implicit scope of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * but can also be extended by user's package objects (or other classes/objects) to bring them into the lexical scope
  * of nested types (which will be required for the `null` and parameter factories).
  */
trait SQLLiteralImplicits extends LowPrioritySQLLiteralImplicits {
	//We don't want implicit conversions from Boolean because a common mistake of using == instead of ===
	// would get unnoticed. We don't need it anyhow, as it can be either used  directly in a condition,
	// or is compared with an expression derived from some columns, in which case it will be propagated automatically
	@implicitAmbiguous("Boolean values are not implicitly convertible to SQLBoolean due to a high risk " +
	                   "of inadvertent conversions of expressions, which should produce an SQLExpression " +
	                   "but instead erroneously result in a Boolean, such as using == instead of ===.")
	implicit def noImplicitBoolean1(value :Boolean) :GlobalBoolean[RowProduct] = if (value) True else False
	implicit def noImplicitBoolean2(value :Boolean) :GlobalBoolean[RowProduct] = if (value) True else False

	implicit def implicitBooleanLiteral[B <: Boolean with Singleton](value :B) :GlobalBoolean[RowProduct] =
		if (value) True else False

	implicit def implicitColumnLiteral[T :ColumnForm](value :T) :ColumnTerm[T] =
		SQLTerm(value)



	/** Enriches any Scala type for which a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists with a `?` method creating
	  * an [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]] expression representing a JDBC parameter
	  * together with its value.
	  */ //todo: move it someplace better than mechanics and make it AnyVal
	implicit class boundParameterSQL[T, P <: SQLParameter[T]](value :T)(implicit factory :ParameterFactory[T, P]) {
		/** Creates a ''bound'' parameter of an SQL statement represented as an
		  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]. The value of the parameter
		  * is set at this point to `this`, but any SQL ''selects'' using this expression will be translated
		  * to a [[java.sql.PreparedStatement PreparedStatement]] with a parameter placeholder for this expression.
		  * @return an expression of type `P`, being a subtype of
		  *         [[net.noresttherein.oldsql.sql.ast.SQLParameter SQLParameter]] defined by the available implicit
		  *         [[net.noresttherein.oldsql.sql.ast.SQLParameter.ParameterFactory ParameterFactory]]
		  *         for the type `T`. If an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]`
		  *         is available, the result
		  *         will be a [[net.noresttherein.oldsql.sql.ast.SQLParameterColumn SQLParameterColumn]].
		  *         Otherwise it will be a `SQLParameter[T]` (using an implicit
		  *         [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]` provided by the factory).
		  */
		@inline def ? :P = factory(value)
	}

	/** Enriches the `null` literal with an `apply[X]` methods which create
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]]`[X]` expressions representing the SQL NULL value.
	  */
	implicit def nullSQL(n :Null) :nullSQL = new nullSQL {}

}






object SQLLiteralImplicits {

	/** Extension method for `null` literals which creates SQL NULL expressions. Used in conjunction with the implicit
	  * conversion [[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.nullSQL nullSQL]], it allows the syntax of
	  * `null[Int]` to create the expression [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]`[Int]`.
	  */
	trait nullSQL extends Any {
		/** Returns [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]`[T]` or
		  * [[net.noresttherein.oldsql.sql.ast.CompositeNull CompositeNull]]`[T]`, depending on whether
		  * a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]] or
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists for type `T`.
		  */
		@inline final def apply[T](implicit factory :SQLNull.Factory[T]) :factory.Res = factory(())
	}

}

