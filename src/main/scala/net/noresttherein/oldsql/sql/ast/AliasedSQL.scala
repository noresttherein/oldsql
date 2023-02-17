package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.AbstractAdaptedSQL
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL
import net.noresttherein.oldsql.sql.ast.LabeledSQL.LabeledColumnSQL.{AnyLabeledColumnVisitor, SpecificLabeledColumnVisitor}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLAdaptation, SQLConversion, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount




//consider: this probably should be a TransformedSQL, as it changes the output?
/** A column with an alias given for use in the `as` clause. The value given here is treated only as a suggestion:
  * it will be ignored if the column appears in a position where `as` clause is illegal, and the alias
  * can be changed when generating SQL to ensure uniqueness in the pertinent scope.
  */ //not AliasedColumn because it exists ass a subtype of AliasedMapping
class AliasedSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
                (val value :ColumnSQL[F, S, V], val alias :String)
	extends DecoratedColumnSQL[F, S, V] with AbstractAdaptedSQL[F, S, V, V]
{
	override def as(alias :String) :ColumnSQL[F, S, V] = new AliasedSQL(value, alias)

	override def @:[N <: Label](alias :N) :LabeledColumnSQL[F, S, N, V] =
		LabeledColumnSQL(alias, value)

	override def groundValue :Opt[V] = value.groundValue

	override def asSingleRow :Option[AliasedSQL[F, Single, V]] =
		if (value.isSingleRow) Some(this.asInstanceOf[AliasedSQL[F, Single, V]])
		else None

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		e

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
		new AliasedSQL(e, alias)

//	protected override def adapt[X](conversion :SQLAdaptation[V, X]) :ColumnSQL[F, S, X] =
//		new AliasedSQL(conversion(value), alias)

	protected override def convert[Y](conversion :SQLConversion[V, Y]) :ColumnSQL[F, S, Y] =
		new AliasedSQL(conversion(value), alias)

	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult  :SQLTransformation[V, U],
	                                       rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		passReform[F1, S1, F2, S2, V2, EC2, U](other)(reform.prohibitReformLeft, passCount)


//	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
//	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//		spelling(value)(from, context, params)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Grouped,
//                                 E >: ColumnSQL[F_, S_, V] <: SQLExpression[F_, S_, V],
//                                 R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//                                (visitor :ColumnVisitor[F, R]) :R[S_, V, E] =
//		visitor.alias(this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R =
		visitor.alias(this)
	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, V] =
		visitor.alias(this)


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :AliasedSQL[_, _, _] if canEqual(other) && other.canEqual(this) =>
			alias == other.alias && value == other.value
		case _ => false
	}
	override def hashCode :Int = value.hashCode * 31 +  alias.hashCode

	override def toString :String = value.toString + " as " + alias
//		override def typeString :ChunkedString = column.typeString + " as " + alias
}



object AliasedSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (column :ColumnSQL[F, S, V], alias :String) :AliasedSQL[F, S, V] =
		new AliasedSQL[F, S, V](column, alias)

	def unapply[F <: RowProduct, S >: Grouped <: Single, V]
	           (expr :SQLExpression[F, S, V]) :Opt[(ColumnSQL[F, S, V], String)] =
		expr match {
			case alias :AliasedSQL[F @unchecked, S @unchecked, V @unchecked] =>
				Got((alias.value, alias.alias))
			case _ =>
				Lack
		}


	trait SpecificAliasedExpressionVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificLabeledColumnVisitor[F, S, V, Y]
	{
		def alias(e :AliasedSQL[F, S, V]) :Y
	}
	type MatchSpecificAliasedExpression[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificAliasedExpressionVisitor[F, S, X, Y]
	trait CaseSpecificAliasedExpression[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAliasedExpressionVisitor[F, S, X, Y]
	{
		override def labeledColumn[N <: Label](e :LabeledColumnSQL[F, S, N, X]) :Y = alias(e)
	}
//
//	trait AliasedExpressionVisitor[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] {
//		def alias[S >: Grouped <: Single, V](e :AliasedSQL[F, S, V]) :R[S, V, AliasedSQL[F, S, V]]
//	}
//	type MatchAliasedExpression[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//		AliasedExpressionVisitor[F, R]
//	type CaseAliasedExpression[+F <: RowProduct, +R[-s >: Grouped <: Single, v, -e <: SQLExpression[F, s, v]]] =
//		AliasedExpressionVisitor[F, R]

	trait AnyAliasedExpressionVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyLabeledColumnVisitor[F, Y]
	{
		def alias[S >: Grouped <: Single, V](e :AliasedSQL[F, S, V]) :Y[S, V]
	}
	type MatchAnyAliasedExpression[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyAliasedExpressionVisitor[F, Y]
	trait CaseAnyAliasedExpression[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyAliasedExpressionVisitor[F, Y]
	{
		override def labeledColumn[S >: Grouped <: Single, N <: Label, V](e :LabeledColumnSQL[F, S, N, V]) :Y[S, V] =
			alias(e)
	}
}
