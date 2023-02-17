package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitAmbiguous

import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.{RowProduct, SingleBoolean, SQLExpression}
import net.noresttherein.oldsql.sql.SQLBoolean.{False, True}
import net.noresttherein.oldsql.sql.ast.{BoundParam, ColumnTerm, SeqSQL, SQLNull, SQLTerm}
import net.noresttherein.oldsql.sql.ast.BoundParam.BoundParamFactory
import net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.{boundParameterSQL, nullSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.With.CTEName






/** A mix in trait extended by [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] in order to bring
  * into scope implicit conversions from Scala values to [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]]
  * instances declared by the [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals$ companion object]].
  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits SQLLiteralImplicits]]
  */ //todo: these are no longer just literals, rename it/move to implicits/syntax
trait implicitSQLLiterals


/** Namespace with implicit conversions from Scala literals providing extension methods for creating bound SQL parameter
  * expressions as well as SQL `null` literals.
  */
object implicitSQLLiterals extends SQLLiteralImplicits


private[sql] sealed trait Rank1SQLLiteralImplicits {
	implicit def implicitLiteral[T :SQLForm](value :T) :SQLTerm[T] = SQLTerm(value)

	implicit def literalSeq[T:SQLForm](items :Seq[T]) :SeqSQL[RowProduct, Single, T] =
		SeqSQL(items.map(SQLTerm(_)) :_*)
}


/** A trait grouping definitions of implicit conversions from Scala literals to
  * [[net.noresttherein.oldsql.sql.ast.SQLLiteral SQLLiteral]] expressions as well as extension factory
  * methods for [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]s
  * ([[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.boundParameterSQL.? _.?]]) and
  * [[net.noresttherein.oldsql.sql.ast.MultiNull nulls]]
  * ([[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.nullSQL.apply null]]`[T]`).
  *
  * It is extended by the [[net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals$ implicitSQLLiterals]] object, which
  * brings the literal conversions into the implicit scope of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * but can also be extended by user's package objects (or other classes/objects) to bring them into the lexical scope
  * of nested types (which will be required for the `null` and parameter factories).
  */ //fixme: solve the issue of multiple imports/implicits in scope by introducing precedence between uppercase/lowercase and sql
private[sql] trait SQLLiteralImplicits extends Rank1SQLLiteralImplicits {
	//We don't want implicit conversions from Boolean because a common mistake of using == instead of ===
	// would get unnoticed. We don't need it anyhow, as it can be either used  directly in a condition,
	// or is compared with an expression derived from some columns, in which case it will be propagated automatically
	@implicitAmbiguous("Boolean values are not implicitly convertible to SQLBoolean due to a high risk " +
	                   "of inadvertent conversions of expressions which should produce an SQLExpression, " +
	                   "but instead erroneously result in a Boolean, such as using == instead of ===.")
	implicit def noImplicitBoolean1(value :Boolean) :SingleBoolean[RowProduct] = if (value) True else False
	implicit def noImplicitBoolean2(value :Boolean) :SingleBoolean[RowProduct] = if (value) True else False

	implicit def implicitBooleanLiteral[B <: Boolean with Singleton](value :B) :SingleBoolean[RowProduct] =
		if (value) True else False

	implicit def implicitColumnLiteral[T :ColumnForm](value :T) :ColumnTerm[T] =
		SQLTerm(value)

	implicit def boundParameterSQL[T, P <: BoundParam[T]]
	                              (value :T)(implicit factory :BoundParamFactory[T, P]) :boundParameterSQL[T, P] =
		new boundParameterSQL[T, P](value)

	implicit def commonTableExpressionName[A <: Label](name :A) :CTEName[A] = new CTEName[A](name)

	/** Enriches the `null` literal with an `apply[X]` methods which create
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]]`[X]` expressions representing the SQL NULL value.
	  */
	implicit def nullSQL(n :Null) :nullSQL = new nullSQL {}


	implicit def expressionSeq[F <: RowProduct, S >: Grouped <: Single, T]
	                          (items :Seq[SQLExpression[F, S, T]]) :SeqSQL[F, S, T] =
		SeqSQL(items :_*)

}






object SQLLiteralImplicits {

	/** Extension method for `null` literals which creates SQL NULL expressions. Used in conjunction with the implicit
	  * conversion [[net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits.nullSQL nullSQL]], it allows the syntax of
	  * `null[Int]` to create the expression [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]`[Int]`.
	  */
	trait nullSQL extends Any {
		/** Returns [[net.noresttherein.oldsql.sql.ast.SQLNull SQLNull]]`[T]` or
		  * [[net.noresttherein.oldsql.sql.ast.MultiNull CompositeNull]]`[T]`, depending on whether
		  * a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]] or
		  * [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists for type `T`.
		  */
		@inline final def apply[T](implicit factory :SQLNull.Factory[T]) :factory.Res = factory(())
	}

	//todo: make subtypes (probably artificial) to allow it being placed in both sql, uppercase and lowercase
	/** Enriches any Scala type for which a [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]
	  * or an [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] exists with a `?` method creating
	  * an [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] expression representing a JDBC parameter
	  * together with its value.
	  */ //todo: move it someplace better than mechanics and make it AnyVal
	class boundParameterSQL[T, P <: BoundParam[T]](private val value :T) extends AnyVal {
		/** Creates a ''bound'' parameter of an SQL statement represented as an
		  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]. The value of the parameter
		  * is set at this point to `this`, but any SQL ''selects'' using this expression will be translated
		  * to a [[java.sql.PreparedStatement PreparedStatement]] with a parameter placeholder for this expression.
		  * @return an expression of type `P`, being a subtype of
		  *         [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]] defined by the available implicit
		  *         [[net.noresttherein.oldsql.sql.ast.BoundParam.BoundParamFactory ParameterFactory]]
		  *         for the type `T`. If an implicit [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]`[T]`
		  *         is available, the result
		  *         will be a [[net.noresttherein.oldsql.sql.ast.BoundColumnParam BoundParamColumn]].
		  *         Otherwise it will be a `BoundParam[T]` (using an implicit
		  *         [[net.noresttherein.oldsql.schema.SQLForm SQLForm]]`[T]` provided by the factory).
		  */
		@inline def ?(implicit factory :BoundParamFactory[T, P]) :P = factory(value)
	}

}

