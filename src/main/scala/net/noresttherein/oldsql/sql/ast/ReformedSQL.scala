package net.noresttherein.oldsql.sql.ast

import java.sql.JDBCType

import scala.collection.immutable.ArraySeq
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, MisspelledSQLException}
import net.noresttherein.oldsql.pixies.RearrangedIndexing
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.SQLReadForm
import net.noresttherein.oldsql.slang.mappingMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{ArbitraryTransformation, SQLDecoration}






/** An SQL expression wrapper which changes the order of its columns, possibly removing some and adding null columns.
  * As the exact number and order of columns is not in general specified for an `SQLExpression` and may vary
  * depending on the context of use, it should not be considered a general purpose class.
  * Instead, it is used late in the spelling process when aligning column sets of several expressions,
  * in particular ''select'' clauses of member ''selects'' in a ''compound select''.
  *
  * The `n`-th column of `value` will become the `order(n)`-th column of this expression, unless `!order.isMapped(n)`,
  * in which case the column is omitted. If not all indices in `[1..order.underlyingColumnCount]` are assigned
  * new columns, then null columns are inserted at omitted indices. Note that the validation of whether
  * the number of columns in `value` actually matches `order.columnCount` is deferred until spelling, i.e.
  * done by [[net.noresttherein.oldsql.sql.ast.RearrangedSQL.explodedSpelling explodedSpelling]].
  * This means that when this instance is created, the creator must be sure about its intended use
  * and the wrapped expression's column count.
  */
final class RearrangedSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
                         (override val value :SQLExpression[F, S, V], val order :RearrangedIndexing)
	extends UnaryCompositeSQL[F, S, V, V]
{
	if (value.selectForm.columnCount != order.columnCount)
		throw new IllegalArgumentException(
			"Cannot reordered columns of " + value + " with " + order + " (with " + order.columnCount +
			" columns) because the selectForm of the expression " + value.selectForm + " has a different (" +
			value.selectForm.columnCount + ") number of columns."
		)

	override lazy val selectForm :SQLReadForm[V] = value.selectForm.reorder(order)

	override def groundValue :Opt[V] = value.groundValue

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		copy(e)

	private def copy[E <: RowProduct, C >: Grouped <: Single, O]
	                (e :SQLExpression[E, C, O]) :SQLExpression[E, C, O] =
//		if (e.selectForm.columnCount != value.selectForm.columnCount)
//			throw new IllegalExpressionException(
//				"Cannot rearrange columns of expression " + e + " as in " + this + " because its form " + e.selectForm +
//				" has a different number of columns than current " + value.selectForm + "."
//			)
//		else
			new RearrangedSQL(e, order)

	val transformation :SQLTransformation[V, V] =
		new ArbitraryTransformation[V, V] with SQLDecoration[V] {
			override def apply[C <: RowProduct, A >: Grouped <: Single, E[v] <: ConvertibleSQL[C, A, v, E]]
			                  (expr :ConvertibleSQL[C, A, V, E]) =
				copy(expr)
			override def applyString(arg :String) = order + "(" + arg + ")"
		}

//	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, V] =
//		if (order.underlyingColumnCount == permutation.length &&
//			permutation.indices.forall { i => order.isCovered(i) && i == permutation(order.inverse(i)) }
//		)
//			value
//		else
//			new RearrangedSQL(value, order compose RearrangedIndexing.inverse(permutation))
//
//	protected def reform[F1 <: F, S1 >: Grouped <: S,
//	                     F2 <: RowProduct, S2 >: Grouped <: Single, V2, EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//	                    (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                    (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
//	                              spelling :SQLSpelling)
//			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
//		if (passCount.firstTime)
//			other.reform(this)(reform.swap, passCount.++).swap
//		else
//			reformer[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount).apply(other)

//
//	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                              Expr2 <: SQLExpression[F2, S2, V2] with ConvertingTemplate[F2, S2, V2, Expr2], U,
//	                              LeftRes <: SQLExpression[_ <: F, _ >: Grouped <: S, U], RightRes <: SQLExpression[F2, S2, U]]
//	                             (other :ConvertingTemplate[F2, S2, V2, Expr2])(reform :Reform, passCount :PassCount)
//	                             (implicit leftResult  :SpecificTransformation[V, U, SQLExpression[F, S, V], LeftRes],
//	                                       rightResult :SpecificTransformation[V2, U, Expr2, RightRes],
//	                                       spelling :SQLSpelling)
//			:(LeftRes, RightRes) =
//		if (!passCount.secondTime)
//			super.reform(other)(reform, passCount)
//		else
//			reform.prohibitReformLeft(value, other)(
//				leftResult compose transformation.generic, rightResult, spelling
//			)

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] =
		order.reorder(spelling.split(value).toIndexedSeq, SQLNull[Null]())

	protected override def shape(implicit spelling :SQLSpelling) :RowShape =
		spelling.shape(value) match {
			case RowShape(exposed) => RowShape(order.reorder(exposed.toIndexedSeq, JDBCType.NULL))
			case indefinite => indefinite
		}

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = order.underlyingColumnCount

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		( "(" +: spelling.inlineSpelling(this)(from, context, params)) + ")"

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	{   //independent = true because we reorder setters.
		val virtual = spelling.explode(value, true)(from, context, params).toIndexedSeq
		if (virtual.length != order.columnCount)
			throw new MisspelledSQLException(
				"Cannot spell a reordered expression " + this + " with " + spelling + " because its number of columns " +
					virtual.length + " does not match the number of columns of reordering " + order.columnCount + ": " +
					virtual + "."
			)
		val outContext = virtual.last.context
		if (order.isCovered(order.underlyingColumnCount)) { //set context of the SQL for our last column to outContext
			val last = order(order.underlyingColumnCount) - 1
			val sqls = virtual.mapWithIndex { (sql, i) =>
				if (i == last) SpelledSQL(sql.sql, sql.setter, outContext)
				else SpelledSQL(sql.sql, sql.setter, context)
			}
			order.reorder(sqls, SpelledSQL(spelling.NULL, context))
		} else {//our last column is a dummy, so the missing argument to reorder will be used for it - with outContext
			val sqls = virtual.map(sql => SpelledSQL(sql.sql, sql.setter, context))
			order.reorder(sqls, SpelledSQL(spelling.NULL, outContext))
		}
	}

	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] = ???
	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = ???
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.rearranged(this)

	override def toString :String = order.toString + "(" + value + ")"
}



object RearrangedSQL {
	trait SpecificRearrangedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] {
		def rearranged(e :RearrangedSQL[F, S, V]) :Y
	}
	type MatchSpecificRearranged[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificRearrangedVisitor[F, S, V, Y]
	type CaseSpecificRearranged[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificRearrangedVisitor[F, S, V, Y]
//
//	trait RearrangedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def rearranged[S >: Grouped <: Single, V](e :RearrangedSQL[F, S, V]) :Y[S, V, RearrangedSQL[F, S, V]]
//	}
//	type MatchRearranged[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		RearrangedVisitor[F, Y]
//	type CaseRearranged[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		RearrangedVisitor[F, Y]

	trait AnyRearrangedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
		def rearranged[S >: Grouped <: Single, V](e :RearrangedSQL[F, S, V]) :R[S, V]
	}
	type MatchAnyRearranged[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = AnyRearrangedVisitor[F, R]
	type CaseAnyRearranged[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = AnyRearrangedVisitor[F, R]
}






/**
  * @author Marcin Mościcki
  */
trait ReformedSQL[-F <: RowProduct, -S >: Grouped <: Single, V] {

}
