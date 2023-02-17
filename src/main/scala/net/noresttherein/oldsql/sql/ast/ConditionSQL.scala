package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.{Opt, PassedArray}
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm}
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLBoolean, SQLExpression, SQLString}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.BetweenSQL.{AnyBetweenVisitor, CaseAnyBetween, CaseSpecificBetween, SpecificBetweenVisitor}
import net.noresttherein.oldsql.sql.ast.ComparisonSQL.{AnyComparisonVisitor, CaseAnyComparison, CaseSpecificComparison, SpecificComparisonVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.BinaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.CompositeSQL.{BinaryCompositeSQL, GroundingUnaryComposite, UnaryCompositeSQL}
import net.noresttherein.oldsql.sql.ast.ExistsSQL.{AnyExistsVisitor, CaseAnyExists, CaseSpecificExists, SpecificExistsVisitor}
import net.noresttherein.oldsql.sql.ast.InSQL.{AnyInVisitor, CaseAnyIn, CaseSpecificIn, SpecificInVisitor}
import net.noresttherein.oldsql.sql.ast.IsNull.{AnyIsNullVisitor, CaseAnyIsNull, CaseSpecificIsNull, SpecificIsNullVisitor}
import net.noresttherein.oldsql.sql.ast.LikeSQL.{AnyLikeVisitor, CaseAnyLike, CaseSpecificLike, SpecificLikeVisitor}
import net.noresttherein.oldsql.sql.ast.LogicalSQL.{AnyLogicalVisitor, CaseAnyLogical, CaseSpecificLogical, SpecificLogicalVisitor}
import net.noresttherein.oldsql.sql.ast.HasNulls.{AnyHasNullsVisitor, CaseAnyHasNulls, CaseSpecificHasNulls, SpecificHasNullsVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLOrdering, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}





/** Base trait for basic `Boolean` tests, other than Boolean [[net.noresttherein.oldsql.sql.ast.LogicalSQL logic]]
  * expressions.
  */
trait ConditionSQL[-F <: RowProduct, -S >: Grouped <: Single] extends CompositeColumnSQL[F, S, Boolean] {
	override def selectForm :ColumnReadForm[Boolean] = ColumnForm[Boolean]

//	override def visit[Y[-_ >: Grouped <: GlobalScope, _]](matcher :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
//		matcher.condition(this)
}




object ConditionSQL {
	trait SpecificConditionVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificBetweenVisitor[F, S, X, Y] with SpecificComparisonVisitor[F, S, X, Y]
		   with SpecificExistsVisitor[F, S, X, Y] with SpecificHasNullsVisitor[F, S, X, Y]
		   with SpecificInVisitor[F, S, X, Y] with SpecificIsNullVisitor[F, S, X, Y]
		   with SpecificLikeVisitor[F, S, X, Y] with SpecificLogicalVisitor[F, S, X, Y]
	{
		def condition(e :ConditionSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}

	trait MatchSpecificCondition[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConditionVisitor[F, S, X, Y] with CaseSpecificBetween[F, S, X, Y]
		   with CaseSpecificComparison[F, S, X, Y] with CaseSpecificExists[F, S, X, Y]
		   with CaseSpecificHasNulls[F, S, X, Y]  with CaseSpecificIn[F, S, X, Y]
		   with CaseSpecificIsNull[F, S, X, Y] with CaseSpecificLike[F, S, X, Y]
		   with CaseSpecificLogical[F, S, X, Y]

	trait CaseSpecificCondition[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificCondition[F, S, X, Y]
	{
		override def between[V](e :BetweenSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def comparison[V](e :ComparisonSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def exists[V](e :ExistsSQL[F, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def in[V](e :InSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def isNull[V](e :IsNull[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def like(e :LikeSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def hasNulls[V](e :HasNulls[F, S, V])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
		override def logical(e :LogicalSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y = condition(e)
	}
//
//	trait ConditionVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends BetweenVisitor[F, R] with ComparisonVisitor[F, R] with ExistsVisitor[F, R] with InVisitor[F, R]
//		   with IsNullVisitor[F, R] with LikeVisitor[F, R]
//	{
//		def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :R[S, Boolean, ConditionSQL[F, S]]
//	}
//
//	trait MatchCondition[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends ConditionVisitor[F, R] with CaseBetween[F, R] with CaseComparison[F, R] with CaseExists[F, R]
//		   with CaseIn[F, R] with CaseIsNull[F, R] with CaseLike[F, R]
//
//	trait CaseCondition[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]]
//		extends MatchCondition[F, R]
//	{
//		override def in[S >: Grouped <: Single, X]
//		               (e :InSQL[F, S, X]) :R[S, Boolean, InSQL[F, S, X]] = condition(e)
//
//		override def comparison[S >: Grouped <: Single, N]
//		                       (e :ComparisonSQL[F, S, N]) :R[S, Boolean, ComparisonSQL[F, S, N]] = condition(e)
//
//		override def exists[S >: Grouped <: Single, X]
//		                   (e :ExistsSQL[F, X]) :R[S, Boolean, ExistsSQL[F, X]] = condition(e)
//
//		override def between[S >: Grouped <: Single, X]
//		                    (e :BetweenSQL[F, S, X]) :R[S, Boolean, BetweenSQL[F, S, X]] = condition(e)
//
//		override def like[S >: Grouped <: Single](e :LikeSQL[F, S]) :R[S, Boolean, LikeSQL[F, S]] = condition(e)
//
//		override def isNull[S >: Grouped <: Single, X]
//		                   (e :IsNull[F, S, X]) :R[S, Boolean, IsNull[F, S, X]] = condition(e)
//	}


	trait AnyConditionVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyBetweenVisitor[F, Y] with AnyComparisonVisitor[F, Y] with AnyExistsVisitor[F, Y]
		   with AnyHasNullsVisitor[F, Y] with AnyInVisitor[F, Y] with AnyIsNullVisitor[F, Y] with AnyLikeVisitor[F, Y]
		   with AnyLogicalVisitor[F, Y]
	{
		def condition[S >: Grouped <: Single](e :ConditionSQL[F, S]) :Y[S, Boolean]
	}

	trait MatchAnyCondition[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyConditionVisitor[F, Y] with CaseAnyBetween[F, Y] with CaseAnyComparison[F, Y]
		   with CaseAnyExists[F, Y] with CaseAnyHasNulls[F, Y] with CaseAnyIn[F, Y] with CaseAnyIsNull[F, Y]
		   with CaseAnyLike[F, Y] with CaseAnyLogical[F, Y]


	trait CaseAnyCondition[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyCondition[F, Y] {
		override def between[S >: Grouped <: Single, V](e :BetweenSQL[F, S, V]) :Y[S, Boolean] = condition(e)

		override def comparison[S >: Grouped <: Single, X](e :ComparisonSQL[F, S, X]) :Y[S, Boolean] =
			condition(e)

		override def exists[V](e :ExistsSQL[F, V]) :Y[Single, Boolean] = condition(e)
		override def in[S >: Grouped <: Single, V](e :InSQL[F, S, V]) :Y[S, Boolean] = condition(e)
		override def isNull[S >: Grouped <: Single, T](e :IsNull[F, S, T]) :Y[S, Boolean] = condition(e)
		override def like[S >: Grouped <: Single](e :LikeSQL[F, S]) :Y[S, Boolean] = condition(e)
		override def partiallyNull[S >: Grouped <: Single, T](e :HasNulls[F, S, T]) :Y[S, Boolean] = condition(e)
		override def logical[S >: Grouped <: Single](e :LogicalSQL[F, S]) :Y[S, Boolean] = condition(e)
	}

}






private[ast] trait NullCheck[-F <: RowProduct, -S >: Grouped <: Single, T]
	extends UnaryCompositeSQL[F, S, T, Boolean] with ConditionSQL[F, S] //not UnaryCompositeColumn because value is not a column
{
	protected def columnOperator(implicit spelling :SQLSpelling) :String

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] = {
		val spell = spelling.inOperand
		val isNullSQL = " " + spelling.keyword("is") + " " + spelling.NULL
		value match {
			case _ :ColumnSQL[F, S, T] =>
				spell(value)(from, context, params) + isNullSQL
			case _ =>
				val operator = " " + columnOperator + " "
				spell.explode(value, false)(from, context, params) match {
					case Seq() => SpelledSQL(context)
					case Seq(single) => single + isNullSQL
					case columns => columns.view.map { c => ("(" +: c) + (isNullSQL + ")") }.reduce {
						(_1, _2) => _1 + columnOperator + _2
					}
				}
		}
	}
}




case class IsNull[-F <: RowProduct, -S >: Grouped <: Single, T](value :SQLExpression[F, S, T])
	extends NullCheck[F, S, T]
{
	override def groundValue :Opt[Boolean] = value.groundValue.map(_ == null)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (expr :SQLExpression[E, C, T]) :ColumnSQL[E, C, Boolean] =
		new IsNull(expr)

	protected override def columnOperator(implicit spelling :SQLSpelling) :String = spelling.AND

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.isNull(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.isNull(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.isNull(this)

	override def toString :String = "(" + value + " IS NULL)"
}


object IsNull {
	trait SpecificIsNullVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def isNull[V](e :IsNull[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificIsNull[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificIsNullVisitor[F, S, X, Y]
	type CaseSpecificIsNull[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificIsNullVisitor[F, S, X, Y]
//
//	trait IsNullVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def isNull[S >: Grouped <: Single, X](e :IsNull[F, S, X]) :R[S, Boolean, IsNull[F, S, X]]
//	}
//	type MatchIsNull[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		IsNullVisitor[F, R]
//	type CaseIsNull[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		IsNullVisitor[F, R]

	trait AnyIsNullVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def isNull[S >: Grouped <: Single, T](e :IsNull[F, S, T]) :Y[S, Boolean]
	}
	type MatchAnyIsNull[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyIsNullVisitor[F, Y]
	type CaseAnyIsNull[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyIsNullVisitor[F, Y]
}






case class HasNulls[-F <: RowProduct, -S >: Grouped <: Single, T](value :SQLExpression[F, S, T])
	extends NullCheck[F, S, T]
{
	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (expr :SQLExpression[E, C, T]) :ColumnSQL[E, C, Boolean] =
		new HasNulls(expr)

	protected override def columnOperator(implicit spelling :SQLSpelling) :String = spelling.OR

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.partiallyNull(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.hasNulls(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.isNull(this)

	override def toString :String = "(" + value + " IS NULL)"
}


object HasNulls {
	trait SpecificHasNullsVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def hasNulls[V](e :HasNulls[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificHasNulls[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificHasNullsVisitor[F, S, X, Y]
	type CaseSpecificHasNulls[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificHasNullsVisitor[F, S, X, Y]
//
//	trait IsNullVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def isNull[S >: Grouped <: Single, X](e :IsNull[F, S, X]) :R[S, Boolean, IsNull[F, S, X]]
//	}
//	type MatchIsNull[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		IsNullVisitor[F, R]
//	type CaseIsNull[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		IsNullVisitor[F, R]

	trait AnyHasNullsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def partiallyNull[S >: Grouped <: Single, T](e :HasNulls[F, S, T]) :Y[S, Boolean]
	}
	type MatchAnyHasNulls[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyHasNullsVisitor[F, Y]
	type CaseAnyHasNulls[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyHasNullsVisitor[F, Y]
}




case class BetweenSQL[-F <: RowProduct, -S >: Grouped <: Single, V :SQLOrdering]
                     (value :ColumnSQL[F, S, V], low :ColumnSQL[F, S, V], high :ColumnSQL[F, S, V])
	extends ConditionSQL[F, S]
{
	protected override val parts :Seq[SQLExpression[F, S, _]] = value::low::high::Nil

	override def groundValue :Opt[Boolean] =
		for (v <- value.groundValue; l <- low.groundValue; h <- high.groundValue)
			yield SQLOrdering[V].compare(v, l) >= 0 && SQLOrdering[V].compare(v, h) <= 0

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
		BetweenSQL(mapper(value), mapper(low), mapper(high))

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		value.atomicSpelling(from, context, params)(spell) + spelling._BETWEEN_ +
			(spell(low)(from, _, params)) + spelling._AND_ +
			(spell(high)(from, _, params))
	}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] =
		visitor.between(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.between(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.between(this)

	override def toString :String = value.toString + " BETWEEN " + low + " AND " + high
}


object BetweenSQL {
	trait SpecificBetweenVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def between[V](e :BetweenSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificBetween[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificBetweenVisitor[F, S, X, Y]
	type CaseSpecificBetween[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificBetweenVisitor[F, S, X, Y]
//
//	trait BetweenVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def between[S >: Grouped <: Single, X](e :BetweenSQL[F, S, X]) :R[S, Boolean, BetweenSQL[F, S, X]]
//	}
//	type MatchBetween[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		BetweenVisitor[F, R]
//	type CaseBetween[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		BetweenVisitor[F, R]

	trait AnyBetweenVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def between[S >: Grouped <: Single, V](e :BetweenSQL[F, S, V]) :Y[S, Boolean]
	}
	type MatchAnyBetween[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyBetweenVisitor[F, Y]
	type CaseAnyBetween[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyBetweenVisitor[F, Y]
}




case class LikeSQL[-F <: RowProduct, -S >: Grouped <: Single]
                  (left :ColumnSQL[F, S, String], right :ColumnSQL[F, S, String])
	extends BinaryCompositeColumn[F, S, String, Boolean] with ConditionSQL[F, S]
{
	override def groundValue :Opt[Boolean] = Lack //todo:

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                              (left :SQLString[E, C], right :SQLString[E, C]) :SQLBoolean[E, C] =
		LikeSQL(left, right)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		val l = left.atomicSpelling(from, context, params)(spell)
		l + spelling._LIKE_ + right.atomicSpelling(from, l.context, params)(spell)
	}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.like(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.like(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.like(this)
	override def toString = s"$left LIKE $right"
}


object LikeSQL {
	trait SpecificLikeVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def like(e :LikeSQL[F, S])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificLike[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificLikeVisitor[F, S, X, Y]
	type CaseSpecificLike[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificLikeVisitor[F, S, X, Y]
//
//	trait LikeVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def like[S >: Grouped <: Single](e :LikeSQL[F, S]) :R[S, Boolean, LikeSQL[F, S]]
//	}
//	type MatchLike[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		LikeVisitor[F, R]
//	type CaseLike[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		LikeVisitor[F, R]

	trait AnyLikeVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def like[S >: Grouped <: Single](e :LikeSQL[F, S]) :Y[S, Boolean]
	}
	type MatchAnyLike[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLikeVisitor[F, Y]
	type CaseAnyLike[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyLikeVisitor[F, Y]
}




case class ExistsSQL[-F <: RowProduct, V](select :SQLExpression[F, Single, Rows[V]])
	extends ConditionSQL[F, Single]// with UnaryCompositeSQL[F, GlobalScope, Rows[V], Boolean]
{   //it could be UnaryCompositeSQL if we allowed it to take also Grouped (reapply must work for any scope)
//	protected override def value :SQLExpression[F, GlobalScope, Rows[V]] = select
	protected override def parts :Seq[SQLExpression[F, Single, Rows[V]]] = PassedArray.one(select)
	override def isSingleRow    :Boolean = select.isSingleRow
	override def groundValue :Opt[Boolean] = select.groundValue.map(_.nonEmpty)
	override def isGround    :Boolean = select.isGround
	override def isAnchored  = true
	override def isAnchored(from :F) :Boolean = select.isAnchored(from)

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, Single, Boolean] =
		ExistsSQL(mapper(select))

//	protected override def reapply[E <: RowProduct, C >: Grouped <: GlobalScope]
//	                              (e :SQLExpression[E, C, Rows[V]]) :ColumnSQL[E, C, Boolean] =
//		ExistsSQL(e)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		(spelling.function("exists") + "(") +: (spelling.inOperand(select)(from, context, params) + ")")

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, Boolean] = visitor.exists(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, Boolean, Y]) :Y = visitor.exists(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: ColumnSQL[F, Single, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.exists(this)

	override def toString :String = s"EXISTS($select)"
}


object ExistsSQL {
	trait SpecificExistsVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def exists[V](e :ExistsSQL[F, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificExists[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificExistsVisitor[F, S, X, Y]
	type CaseSpecificExists[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificExistsVisitor[F, S, X, Y]
//
//	trait ExistsVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def exists[S >: Grouped <: Single, X](e :ExistsSQL[F, X]) :R[S, Boolean, ExistsSQL[F, X]]
//	}
//	type MatchExists[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ExistsVisitor[F, R]
//	type CaseExists[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		ExistsVisitor[F, R]

	trait AnyExistsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def exists[V](e :ExistsSQL[F, V]) :Y[Single, Boolean]
	}
	type MatchAnyExists[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyExistsVisitor[F, Y]
	type CaseAnyExists[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyExistsVisitor[F, Y]
}






case class InSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
                (left :ColumnSQL[F, S, V], right :SQLExpression[F, S, Seq[V]])
	extends ConditionSQL[F, S]
{   //can't be a BinaryCompositeSQL because left must be a ColumnSQL
	protected override def parts :Seq[SQLExpression[F, S, _]] = PassedArray.two(left, right)
	override def groundValue :Opt[Boolean] =
		for (l <- left.groundValue; r <- right.groundValue) yield r.contains(l)

	override def isSingleRow   :Boolean = left.isSingleRow && right.isSingleRow
	override def isGround   :Boolean = left.isGround && right.isGround
	override def isAnchored :Boolean = left.isAnchored && right.isAnchored
	override def isAnchored(from :F) :Boolean = left.isAnchored(from) && right.isAnchored(from)

	override def anchor(from :F) :SQLBoolean[F, S] =
		(left.anchor(from), right.anchor(from)) match {
			case (l, r) if (l eq left) && (r eq right) => this
			case (l, r) => InSQL(l, r)
		}

	override def rephrase[E <: RowProduct](mapper :SQLScribe[F, E]) :ColumnSQL[E, S, Boolean] =
		InSQL(mapper(left), mapper(right))

//	protected override def reapply[E <: RowProduct, C >: Grouped <: GlobalScope]
//	                              (left :SQLExpression[E, C, V], right :SQLExpression[E, C, Seq[V]]) :SQLExpression[E, C, Boolean] =

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val spell = spelling.inOperand
		val l = left.atomicSpelling(from, context, params)(spell)
		l + (" " + spelling.keyword("in") + " (") + spell(right)(from, l.context, params) + ")"
	}

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[F, Y]) :Y[S, Boolean] = visitor.in(this)

	protected override def visit[Y](visitor :SpecificColumnVisitor[F, S, Boolean, Y]) :Y = visitor.in(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//		visitor.in(this)

	override def toString :String = s"($left IN $right)"
}


object InSQL {
	trait SpecificInVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def in[V](e :InSQL[F, S, V])(implicit isBoolean :X =:= Boolean) :Y
	}
	type MatchSpecificIn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificInVisitor[F, S, X, Y]
	type CaseSpecificIn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificInVisitor[F, S, X, Y]
//
//	trait InVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] {
//		def in[S >: Grouped <: Single, X](e :InSQL[F, S, X]) :R[S, Boolean, InSQL[F, S, X]]
//	}
//	type MatchIn[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InVisitor[F, R]
//	type CaseIn[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -E <: SQLExpression[F, S, V]]] =
//		InVisitor[F, R]

	trait AnyInVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def in[S >: Grouped <: Single, V](e :InSQL[F, S, V]) :Y[S, Boolean]
	}
	type MatchAnyIn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyInVisitor[F, Y]
	type CaseAnyIn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyInVisitor[F, Y]
}



