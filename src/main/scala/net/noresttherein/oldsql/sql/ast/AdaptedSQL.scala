package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.ColumnAt
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.slang.classNameMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ConvertibleColumn, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingTemplate, GroundingTemplate, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.AdaptedColumnSQL.{AnyAdaptedColumnVisitor, SpecificAdaptedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.AdaptedSQL.{AbstractAdaptedSQL, AnyAdaptedVisitor, CaseAnyAdapted, CaseSpecificAdapted, SpecificAdaptedVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL
import net.noresttherein.oldsql.sql.ast.ColumnMappingSQL.ConvertedColumnMappingSQL.{AnyConvertedColumnMappingVisitor, CaseAnyConvertedColumnMapping, CaseSpecificConvertedColumnMapping, SpecificConvertedColumnMappingVisitor}
import net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn
import net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL
import net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL.{AnyConvertedColumnVisitor, CaseAnyConvertedColumn, CaseSpecificConvertedColumn, SpecificConvertedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.ConvertedSQL.{AnyConvertedVisitor, CaseAnyConverted, CaseSpecificConverted, ConvertedSQLTemplate, SpecificConvertedVisitor}
import net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL.{AnyDecoratedColumnVisitor, CaseAnyDecoratedColumn, CaseSpecificDecoratedColumn, SpecificDecoratedColumnVisitor}
import net.noresttherein.oldsql.sql.ast.DecoratedSQL.{AnyDecoratedVisitor, CaseAnyDecorated, CaseSpecificDecorated, SpecificDecoratedVisitor}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL.{AnyConvertedMappingVisitor, CaseAnyConvertedMapping, CaseSpecificConvertedMapping, SpecificConvertedMappingVisitor}
import net.noresttherein.oldsql.sql.mechanics.{Reform, SpelledSQL, SQLAdaptation, SQLConversion, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.ArbitraryAdaptation
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.SQLDecoration




///** A base trait for expressions adapting another `SQLExpression[F, S, Y]`, with implementation centered around
//  * an [[net.noresttherein.oldsql.sql.mechanics.SQLTransformation SQLTransformation]]`[X, Y]`,
//  * which acts as its constructor. SQL generated from this expression
//  * and `this.`[[net.noresttherein.oldsql.sql.ast.TransformedSQL.value value]] may be different, but must type check
//  * on the database level, i.e. must consist of the same number of columns using compatible types.
//  * All methods implemented here assume that the column sets
//  * of this expression and the adapted [[net.noresttherein.oldsql.sql.ast.TransformedSQL.value value]] are the same,
//  * and spelling methods simply amount to spelling of the latter.
//  *
//  * This covers some edge cases usually not encountered by the application directly - most proper adapters
//  * will be satisfied with its subclass [[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]],
//  * and conversions of only the value of the expression, rather than its SQL form, are implemented
//  * by [[net.noresttherein.oldsql.sql.ast.ConvertedSQL ConvertedSQL]].
//  */ //todo: AdaptedSQL docs say that it may change the SQL, providing the column types match. Do we need this class, then?
//trait TransformedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
//	extends UnaryCompositeSQL[F, S, X, Y] with SelectableSQL[F, S, Y]
//{
//	override def selectForm  :SQLReadForm[Y] = value.selectForm.nullMap(convert) //consider: NullValue?
//	override def groundValue :Opt[Y] = value.groundValue.map(convert)
//	override def value       :SQLExpression[F, S, X]
//
//	def conversion :SQLTransformation[X, Y]
//
//	def convert(x :X) :Y
//
//	override def isShapeComposite :Boolean = true
//	override def isInline         :Boolean = true
//
//	override def reapply[E <: RowProduct, C >: Grouped <: Single] //made public
//	                    (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y]
//
//	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//                                 (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
//                                           spelling :SQLSpelling)
//			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
//		if (passCount.firstTime)
//			super.reform(other)(reform, passCount)
//		else {
//			/* We optimistically reform value with other and verify that the result is ok.
//			 * If it is a preprocessing reform which do not fully reform the results, it's validate must not perform
//			 * actual validation. In that case however the expression will be reformed with another reform later,
//			 * when validate will do its job correctly.
//			 */
//			implicit val composed = conversion andThen leftResult
//			val res = reform(value, other)
//			reform.validate(res._1, res._2) //res._1 has reapplied this adapter to reformed value
//			res
//		}
//
//	protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(value)
//	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = spelling.split(value)
//	protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(value)
//
//	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
//	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//		spelling(value)(from, context, params)
//
//	protected override def explodedSpelling[P](independent :Boolean)
//	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
//	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
//		spelling.explode(value, independent)(from, context, params)
//
//	protected def name :String = this.innerClassName
//	override def toString = s"$name($value)"
//}
//
//
//
//object TransformedSQL {
//
////	private[sql] def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
////	                      (expr :SQLExpression[F, S, X], conversion :SQLTransformation[X, Y]) :TransformedSQL[F, S, X, Y] =
////		new StandardTransformedSQL(expr, conversion)
//
//	def unapply[F <: RowProduct, S >: Grouped <: Single, X]
//	           (e :SQLExpression[F, S, X]) :Opt[SQLExpression[F, S, _]] =
//		e match {
//			case adapter :TransformedSQL[F, S, _, X] => Got(adapter.value)
//			case _ => Lack
//		}
//
//	trait SpecificTransformedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
//		extends SpecificAdaptedVisitor[F, S, V, R]
//	{
//		def transformed[X](e :TransformedSQL[F, S, X, V]) :R
//	}
//	trait MatchSpecificTransformed[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
//		extends SpecificTransformedVisitor[F, S, X, Y] with CaseSpecificAdapted[F, S, X, Y]
//
//	trait CaseSpecificTransformed[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
//		extends MatchSpecificTransformed[F, S, V, Y]
//	{
//		override def adapted[X](e :AdaptedSQL[F, S, X, V]) :Y = transformed(e)
//	}
//
//
//	trait AnyTransformedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
//		extends AnyAdaptedVisitor[F, R]
//	{
//		def transformed[S >: Grouped <: Single, X, Y](e :TransformedSQL[F, S, X, Y]) :R[S, Y]
//	}
//	trait MatchAnyTransformed[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
//		extends AnyTransformedVisitor[F, R] with CaseAnyAdapted[F, R]
//
//	trait CaseAnyTransformed[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyTransformed[F, R] {
//		override def adapted[S >: Grouped <: Single, T, U](e :AdaptedSQL[F, S, T, U]) :R[S, U] =
//			transformed(e)
//	}
//}


/** This class exists primarily as a base class for default `AdaptedSQL`, `AdaptedColumn`, `ConvertedSQL` and
  * `ConvertedColumn` implementation. There is little sense in having a no-op `TransformedSQL` implementation,
  * because it must equal `transformation(value)` anyway, so we might as well call that; an `SQLTransformation`
  * either creates a specific class, or is an `SQLAdaptation`, which will use `AdaptedSQL`.
  */
private[sql] class StandardTransformedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
                   (override val value :SQLExpression[F, S, X], val conversion :SQLTransformation[X, Y])
	extends UnaryCompositeSQL[F, S, X, Y]
{
	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, X]) =
		if (e eq value) this.asInstanceOf[SQLExpression[E, C, Y]] else conversion(e)

//	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.composite(this)
}






/** A [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] wrapper over another
  * SQL [[net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL.value expression]]
  * which has the same SQL column set as the latter, despite possibly having different value types.
  * In the default implementation, the generated SQL does not change by wrapping the expression, although
  * it is not strictly required, providing its [[net.noresttherein.oldsql.sql.ast.AdaptedSQL.adaptation adaptation]]
  * creates the same specialized adapter type. It encompasses transparent
  * type [[net.noresttherein.oldsql.sql.ast.ConvertedSQL conversions]]
  * as well as [[net.noresttherein.oldsql.sql.ast.DecoratedSQL decorators]] which add some additional
  * information about the expression.
  */ //todo :parameterize this with the type of the adapted expression
trait AdaptedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends UnaryCompositeSQL[F, S, X, Y] with SelectableSQL[F, S, Y]
{
	def adaptation :SQLAdaptation[X, Y]
	override def selectForm  :SQLReadForm[Y] = value.selectForm.nullMap(convert) //consider: NullValue?
	override def groundValue :Opt[Y] = value.groundValue.map(convert)
	override def value       :SQLExpression[F, S, X]

	def convert(x :X) :Y

	override def isShapeComposite :Boolean = true
	override def isInline         :Boolean = true

	override def reapply[E <: RowProduct, C >: Grouped <: Single] //made public
	                    (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y]

	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (passCount.firstTime)
			passReform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)(leftResult, rightResult, spelling)
		else
			reform[F1, S1, X, SQLExpression.from[F1]#rows[S1]#E, F2, S2, V2, EC2, U](
				value :SQLExpression[F1, S1, X], other)(adaptation andThen leftResult, rightResult, spelling
			)

	protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(value)
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = spelling.split(value)
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(value)

	protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(value)(from, context, params)

	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		spelling.explode(value, independent)(from, context, params)

	protected def name :String = this.innerClassName
	override def toString = s"$name($value)"

	//consider: moving it down to ConvertedSQL.
	def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y]

//	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, Y] =
//		if (permutation == permutation.indices) this
//		else reapply(value.reorder(permutation))
//	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2, Expr2 <: SQLExpression[F2, S2, V2],
//	                              U, LeftRes <: SQLExpression[F, S, U], RightRes <: SQLExpression[F2, S2, U]]
//	                             (other :Expr2)(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult  :SpecificTransformation[F, S, Y, U, SQLExpression[F, S, Y], LeftRes],
//	                                       rightResult :SpecificTransformation[F2, S2, V2, U, Expr2, RightRes],
//	                                       spelling :SQLSpelling)
//			:(LeftRes, RightRes) =
//		if (passesAllowed >= 3)
//			super.reform(other)(reform, passesAllowed)
//		else
//			reform(value, other)(adaptation[F, S] andThen leftResult, rightResult, spelling)

/*
	protected override def reform[E <: RowProduct, C >: Grouped <: GlobalScope, T, U]
	                             (other :SQLExpression[E, C, T])(reform :Reform, passesAllowed :Int)
	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[T, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], SQLExpression[E, C, U]) =
		if (passesAllowed >= 3) //must be >= 3 to trigger reforming value before AlignedExpression does its thing
			super.reform(other)(reform, passesAllowed)
		else {
			//fixme: ConvertingTemplate with (lift andThen leftResult) will create a ConversionSQL, not this class.
			//  No way around it, as it conflicts with the idea that Lift preserves the type of some classes like ComponentLValueSQL)
			//  The thing is, we don't need to preserve that type after lifting, because we are going to use the result
			//  in our place, and we are just a plain SQLExpression. It seems like Reform.LValue should be replaced
			//  with Lift.Expr
			reform(value, other)(conversion andThen leftResult vs rightResult, spelling)
		}

	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], T, U]
	                             (other :LValueSQL[E, C, T])(reform :Reform, passesAllowed :Int)
	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[T, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
		if (passesAllowed >= 3) //must be >= 3 to trigger reforming value before AlignedExpression does its thing
			super.reform(other)(reform, passesAllowed)
		else
			reform(value, other)(conversion andThen leftResult vs rightResult, spelling)
*/

//	protected override def visit[R](visitor :ExpressionVisitor[F, S, Y, R]) :R = visitor.adapted(this)
	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.adapted(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor: AnyExpressionVisitor[F, R]): R[S, Y] =
		visitor.adapted(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F_, S_, X] <: SQLExpression[F_, S_, X],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[RowProduct, R]) :R[S_, Y, E] =
//		visitor.adapted(this)

	override def equals(that :Any) :Boolean = that match {
		case other :AdaptedSQL[_, _, _, _] if canEqual(other) && other.canEqual(this) => value == other.value
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.getClass == getClass
	override def hashCode :Int = value.hashCode

}




object AdaptedSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :SQLExpression[F, S, X], conversion :SQLAdaptation[X, Y]) :AdaptedSQL[F, S, X, Y] =
		expr match {
			case column :ColumnSQL[F, S, X] => AdaptedColumnSQL(column, conversion)
			case _ => new Impl(expr, conversion)
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, X]
	           (e :SQLExpression[F, S, X]) :Opt[SQLExpression[F, S, _]] =
		e match {
			case adapter :AdaptedSQL[F, S, _, X] => Got(adapter.value)
			case _ => Lack
		}

	trait AbstractAdaptedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y] extends AdaptedSQL[F, S, X, Y] { self =>
		override def adaptation :SQLAdaptation[X, Y] = new DerivedAdaptation

		protected class DerivedAdaptation extends ArbitraryAdaptation[X, Y] {
			override def apply(value :X) :Y = self.convert(value)

			override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
			                  (expr :ConvertingTemplate[F1, S1, X, E]) :SQLExpression[F1, S1, Y] =
				expr match {
					case null => reapply(SQLNull[X]())
					case column :ColumnSQL[F1, S1, X] => reapply(column)
					case _ => reapply(expr.toConvertibleSQL)
				}
//			override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
//			                  (expr :ConvertibleSQL[F1, S1, X, E]) :SQLExpression[F1, S1, Y] =
//				expr match {
//					case column :ColumnSQL[F1, S1, X] => reapply(column)
//					case _ => reapply(expr)
//				}

			override def column[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
			                   (expr :ConvertibleColumn[F1, S1, X, E]) :ColumnSQL[F1, S1, Y] =
				reapply(expr)

			override def applyString(arg :String) :String = arg + "." + name
		}
	}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :SQLExpression[F, S, X], override val adaptation :SQLAdaptation[X, Y])
		extends StandardTransformedSQL[F, S, X, Y](value, adaptation) with AdaptedSQL[F, S, X, Y]
	{
		override def convert(x :X) = adaptation(x)

		override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
			if (e eq value) this.asInstanceOf[ColumnSQL[E, C, Y]] else AdaptedColumnSQL(e, adaptation)
	}


	trait SpecificAdaptedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +R]
		extends SpecificAdaptedColumnVisitor[F, S, V, R]
		   with SpecificConvertedVisitor[F, S, V, R] with SpecificDecoratedVisitor[F, S, V, R]
	{
		def adapted[X](e :AdaptedSQL[F, S, X, V]) :R
	}
	trait MatchSpecificAdapted[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedVisitor[F, S, X, Y]
		   with CaseSpecificConverted[F, S, X, Y] with CaseSpecificDecorated[F, S, X, Y]
	{
		override def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y = adapted(e)
	}
	trait CaseSpecificAdapted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends MatchSpecificAdapted[F, S, V, Y]
	{
		override def converted[X](e :ConvertedSQL[F, S, X, V]) :Y = adapted(e)
		override def decorated(e :DecoratedSQL[F, S, V]) :Y = adapted(e)
	}


/*
	trait AdaptedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends AdaptedColumnVisitor[F, Y] with ConvertedVisitor[F, Y] with DecoratedVisitor[F, Y]
	{
		def adapted[S >: Grouped <: Single, X, V](e :AdaptedSQL[F, S, X, V]) :Y[S, V, AdaptedSQL[F, S, X, V]]
	}
	trait MatchAdapted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends AdaptedVisitor[F, Y] with CaseConverted[F, Y] with CaseDecorated[F, Y]
	{
		override def adaptedColumn[S >: Grouped <: Single, X, V]
		                          (e :AdaptedColumnSQL[F, S, X, V]) :Y[S, V, AdaptedColumnSQL[F, S, X, V]] =
			adapted(e)
	}
	trait CaseAdapted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
		extends MatchAdapted[F, Y]
	{
		override def converted[S >: Grouped <: Single, X, V]
		                      (e :ConvertedSQL[F, S, X, V]) :Y[S, V, ConvertedSQL[F, S, X, V]] =
			adapted(e)

		override def decorated[S >: Grouped <: Single, V]
		                      (e :DecoratedSQL[F, S, V]) :Y[S, V, DecoratedSQL[F, S, V]] =
			adapted(e)
	}
*/


	trait AnyAdaptedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, R] with AnyConvertedVisitor[F, R] with AnyDecoratedVisitor[F, R]
	{
		def adapted[S >: Grouped <: Single, X, Y](e :AdaptedSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyAdapted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedVisitor[F, R] with CaseAnyConverted[F, R] with CaseAnyDecorated[F, R]
	{
		override def adaptedColumn[S >: Grouped <: Single, X, Y](e :AdaptedColumnSQL[F, S, X, Y]) :R[S, Y] =
			adapted(e)
	}
	trait CaseAnyAdapted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyAdapted[F, R] {
		override def converted[S >: Grouped <: Single, T, U](e :ConvertedSQL[F, S, T, U]) :R[S, U] =
			adapted(e)

		override def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :R[S, V] = adapted(e)
	}

}







/** A [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL CompositeColumnSQL]] wrapper over another
  * [[net.noresttherein.oldsql.sql.ast.CompositeColumnSQL.UnaryCompositeColumn.value column expression]]
  * which translates to the same SQL. It encompasses transparent
  * type [[net.noresttherein.oldsql.sql.ast.ConvertedColumnSQL conversions]]
  * as well as [[net.noresttherein.oldsql.sql.ast.DecoratedColumnSQL decorators]] which add some additional
  * information about the expression.
  */
trait AdaptedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends AdaptedSQL[F, S, X, Y] with UnaryCompositeColumn[F, S, X, Y]
{
	override def value      :ColumnSQL[F, S, X]
	override def selectForm :ColumnReadForm[Y] = value.selectForm.nullMap(convert)

	//override clash
	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y]

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.adaptedColumn(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
		visitor.adaptedColumn(this)

//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :Y[S_, Y, E] =
//		visitor.adaptedColumn(this)
}




object AdaptedColumnSQL {
	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :ColumnSQL[F, S, X], conversion :SQLAdaptation[X, Y]) :AdaptedColumnSQL[F, S, X, Y] =
		new Impl(expr, conversion)

	def unapply[F <: RowProduct, S >: Grouped <: Single, X](e :ColumnSQL[F, S, X]) :Opt[ColumnSQL[F, S, _]] =
		(e :SQLExpression[F, S, X]) match {
			case conv :AdaptedColumnSQL[F, S, _, X] => Got(conv.value)
//			case AdaptedSQL(col :ColumnSQL[F, S, _]) => Got(col)
			case _ => Lack
		}
	//We don't need an AbstractAdaptedColumnSQL because AbstractAdaptedSQL already has reapply(e :ColumnSQL)
	// and AdaptedColumnSQL needs to implement reapply(e :SQLExpression) anyway.
//
//	trait AbstractAdaptedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y] extends AdaptedSQL[F, S, X, Y] {
//		override def conversion :SQLAdaptation[X, Y] = new DerivedAdaptation
//
//		protected class DerivedAdaptation extends ArbitraryAdaptation[X, Y] {
//			override def apply(value :X) :Y = convert(value)
//
//			override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
//			                  (expr :ConvertibleSQL[F1, S1, X, E]) :SQLExpression[F1, S1, Y] =
//				reapply(expr)
//
//			override def apply[F1 <: RowProduct, S1 >: Grouped <: Single, E[v] <: ConvertibleSQL[F1, S1, v, E]]
//			                  (expr :ConvertibleColumn[F1, S1, X, E]) :ColumnSQL[F1, S1, Y] =
//				reapply(expr)
//
//			override def applyString(arg :String) :String = arg + "." + name
//		}
//	}

	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :ColumnSQL[F, S, X], override val adaptation :SQLAdaptation[X, Y])
		extends StandardTransformedSQL[F, S, X, Y](value, adaptation) with AdaptedColumnSQL[F, S, X, Y]
	{
		override def convert(x :X) = adaptation(x)

		override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
			if (e eq value) this.asInstanceOf[ColumnSQL[E, C, Y]] else AdaptedColumnSQL(e, adaptation)
	}


	trait SpecificAdaptedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnVisitor[F, S, X, Y] with SpecificDecoratedColumnVisitor[F, S, X, Y]
	{
		def adaptedColumn[V](e :AdaptedColumnSQL[F, S, V, X]) :Y
	}
	trait MatchSpecificAdaptedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificAdaptedColumnVisitor[F, S, X, Y]
		   with CaseSpecificConvertedColumn[F, S, X, Y] with CaseSpecificDecoratedColumn[F, S, X, Y]

	trait CaseSpecificAdaptedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificAdaptedColumn[F, S, X, Y]
	{
		override def convertedColumn[V](e :ConvertedColumnSQL[F, S, V, X]) :Y = adaptedColumn(e)
		override def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y = adaptedColumn(e)
	}
//
//
//	trait AdaptedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with DecoratedColumnVisitor[F, Y]
//	{
//		def adaptedColumn[S >: Grouped <: Single, X, V]
//		                 (e :AdaptedColumnSQL[F, S, X, V]) :Y[S, V, AdaptedColumnSQL[F, S, X, V]]
//	}
//	trait MatchAdaptedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends AdaptedColumnVisitor[F, Y] with CaseConvertedColumn[F, Y] with CaseDecoratedColumn[F, Y]
//
//	trait CaseAdaptedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchAdaptedColumn[F, Y]
//	{
//		override def convertedColumn[S >: Grouped <: Single, X, V]
//		                            (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]] =
//			adaptedColumn(e)
//
//		override def decoratedColumn[S >: Grouped <: Single, V]
//		                            (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]] =
//			adaptedColumn(e)
//
//	}


	trait AnyAdaptedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, R] with AnyDecoratedColumnVisitor[F, R]
	{
		def adaptedColumn[S >: Grouped <: Single, X, Y](e :AdaptedColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyAdaptedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyAdaptedColumnVisitor[F, Y] with CaseAnyConvertedColumn[F, Y] with CaseAnyDecoratedColumn[F, Y]

	trait CaseAnyAdaptedColumn[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends MatchAnyAdaptedColumn[F, R]
	{
		override def convertedColumn[S >: Grouped <: Single, X, Y]
		                            (e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y] = adaptedColumn(e)

		override def decoratedColumn[S >: Grouped <: Single, V]
		                            (e :DecoratedColumnSQL[F, S, V]) :R[S, V] = adaptedColumn(e)
	}
}






/** An adapter of an `SQLExpression[F, S, X]` to a new value type `Y`.
  * The conversion is assumed to be either between two Scala types which map to the same SQL type,
  * or between types which are implicitly converted between each other in SQL.
  * There is no change whatsoever to the generated SQL, nor any additional information or functionality.
  *
  * The difference from its supertype `AdaptedSQL` is that the expression is only an implementation
  * artifact introduced to satisfy type checking and it limits itself ''only'' to changing the Scala value of an expression
  * - is essentially just a function `X => Y` lift to `SQLExpression` type. Various operations on `SQLExpression`
  * may compose several instances into one, change the exact class used, or change the order in which other adapters
  * are applied with regard to this instance, and are generally not limited in any way other than ensuring the result
  * still type checks and the [[net.noresttherein.oldsql.sql.SQLExpression.selectForm selectForm]] of the expression
  * uses method `this.`[[net.noresttherein.oldsql.sql.ast.ConvertedSQL.convert convert]] or
  * `this.`[[net.noresttherein.oldsql.sql.ast.ConvertedSQL.adaptation adaptation]].
  */ //consider: making it extend TransformedSQL instead to reflect that AdaptedSQL cannot be moved up/down or composed
trait ConvertedSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends AdaptedSQL[F, S, X, Y] with ConvertedSQLTemplate[F, S, X, Y, SQLExpression.from[F]#rows[S]#E]
{
	override val value      :SQLExpression[F, S, X] //declaration clash
	override def adaptation :SQLConversion[X, Y]
	override def convert(x :X) :Y = adaptation(x)
	//overridden to facilitate form equality
	override def selectForm :SQLReadForm[Y] = adaptation(value.selectForm)

	protected override def convert[Z](implicit conversion :SQLConversion[Y, Z]) :SQLExpression[F, S, Z] =
		ConvertedSQL(value, this.adaptation andThen conversion)

	override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                    (e :SQLExpression[E, C, X]) :SQLExpression[E, C, Y] =
		if (e eq value) this.asInstanceOf[SQLExpression[E, C, Y]] else adaptation(e)

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
		adaptation(e)


//	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
//	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
//	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
//	                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
//	                                       spelling :SQLSpelling)
//			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
//		if (!passCount.hasPassed)
//			super.reform(other)(reform, passCount)
//		else
//			reform(value, other)(conversion andThen leftResult, rightResult, spelling)

//	protected override def reform[E <: RowProduct, C >: Grouped <: Single, V, U]
//	                             (other :SQLExpression[E, C, V])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[V, U], spelling :SQLSpelling)
//			:(SQLExpression[F, S, U], SQLExpression[E, C, U]) =
//		if (passesAllowed >= 3)
//			super.reform(other)(reform, passesAllowed)
//		else {
//			implicit val liftCompat = conversion andThen leftResult vs rightResult
//			reform(value, other)
//		}
//
//	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], V, U]
//	                             (other :LValueSQL[E, C, V])(reform :Reform, passesAllowed :Int)
//	                             (implicit leftResult :Lift[Y, U], rightResult :Lift[V, U], spelling :SQLSpelling)
//			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
//		if (passesAllowed >= 3)
//			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
//		else {
//			implicit val liftCompat = conversion andThen leftResult vs rightResult
//			reform(value, other)
//		}


	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, R]) :R[S, Y] =
		visitor.converted(this)

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, Y, R]) :R = visitor.converted(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: S,
//	                             E >: SQLExpression[F_, S_, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[RowProduct, R]) :R[S_, Y, E] =
//		visitor.converted(this)

	override def sameAs(that :CompositeSQL.__) :Boolean = that.isInstanceOf[ConvertedSQL[_, _, _, _]]

	override def canEqual(that :Any) :Boolean = that match {
		case promo :ConvertedSQL[_, _, _, _] if promo sameAs this => adaptation == promo.adaptation
		case _ => false
	}
	override def hashCode :Int = value.hashCode * adaptation.hashCode

	override def name :String = adaptation.toString
}



object ConvertedSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :SQLExpression[F, S, X], conversion :SQLConversion[X, Y]) :ConvertedSQL[F, S, X, Y] =
		expr match {
			case column :ColumnSQL[F, S, X] => ConvertedColumnSQL(column, conversion)
			case _ => new Impl(expr, conversion)
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(SQLExpression[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		e match {
			case promo :ConvertedSQL[F, S, x, Y] => Got((promo.value, promo.adaptation))
			case _ => Lack
		}

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :ColumnSQL[F, S, Y])
			:Opt[(ColumnSQL[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		(e :SQLExpression[F, S, Y]) match {
			case conv :ConvertedColumnSQL[F, S, x, Y] => Got((conv.value, conv.adaptation))
			case conv :ConvertedSQL[F, S, x, Y] => conv.value match  {
				case column :ColumnSQL[F, S, x] => Got((column, conv.adaptation))
				case _ => Lack
			}
			case _ => Lack
		}

	def OrNull[F <: RowProduct, S >: Grouped <: Single, T]
	          (expr :SQLExpression[F, S, T]) :SQLExpression[F, S, Option[T]] =
		ConvertedSQL(expr, SQLConversion.toOption[T])

	def OrNull[F <: RowProduct, S >: Grouped <: Single, T]
	          (expr :ColumnSQL[F, S, T]) :ColumnSQL[F, S, Option[T]] =
		ConvertedColumnSQL[F, S, T, Option[T]](expr, SQLConversion.toOption[T])


	trait ConvertedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, X, Y, +EC[v] <: ConvertibleSQL[F, S, v, EC]]
		extends ConvertingTemplate[F, S, Y, EC]
	{ this :EC[Y] with ConvertedSQLTemplate[F, S, X, Y, EC] =>
		def value      :SQLExpression[F, S, X]
		def adaptation :SQLConversion[X, Y]

		protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[Y, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.SQLResult[F1, S1, EC[U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult[F1, S1, EC](this), rightResult(other))
			else if (passCount.firstTime)
				passReform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)
			else
				reform[F1, S1, X, EC, F2, S2, V2, EC2, U](value, other)(adaptation andThen leftResult, rightResult, spelling)
	}


	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :SQLExpression[F, S, X], override val adaptation :SQLConversion[X, Y])
		extends ConvertedSQL[F, S, X, Y]


	trait SpecificConvertedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificConvertedColumnVisitor[F, S, V, Y] with SpecificConvertedMappingVisitor[F, S, V, Y]
	{
		def converted[X](e :ConvertedSQL[F, S, X, V]) :Y
	}
	trait MatchSpecificConverted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificConvertedVisitor[F, S, V, Y] with CaseSpecificConvertedMapping[F, S, V, Y]
	{
		override def convertedColumn[X](e :ConvertedColumnSQL[F, S, X, V]) :Y = converted(e)
	}
	trait CaseSpecificConverted[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] extends MatchSpecificConverted[F, S, V, Y] {

		override def convertedMapping[M[A] <: MappingAt[A], X](e :ConvertedMappingSQL[F, S, M, X, V]) :Y =
			converted(e)
	}
//
//
//	trait ConvertedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with ConvertedMappingVisitor[F, Y]
//	{
//		def converted[S >: Grouped <: Single, X, V]
//		             (e :ConvertedSQL[F, S, X, V]) :Y[S, V, ConvertedSQL[F, S, X, V]]
//	}
//	trait MatchConverted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedVisitor[F, Y] with CaseConvertedMapping[F, Y]
//	{
//		override def convertedColumn[S >: Grouped <: Single, X, V]
//		                            (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]] =
//			converted(e)
//	}
//	trait CaseConverted[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchConverted[F, Y]
//	{
//		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, V]
//		                             (e :ConvertedMappingSQL[F, S, M, X, V]) :Y[S, V, ConvertedMappingSQL[F, S, M, X, V]] =
//			converted(e)
//	}

	trait AnyConvertedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, R] with AnyConvertedMappingVisitor[F, R]
	{
		def converted[S >: Grouped <: Single, X, Y](e :ConvertedSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyConverted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedVisitor[F, R] with CaseAnyConvertedMapping[F, R]
	{
		override def convertedColumn[S >: Grouped <: Single, X, Y]
		                            (e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y] = converted(e)
	}
	trait CaseAnyConverted[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends MatchAnyConverted[F, R] {
		override def convertedMapping[S >: Grouped <: Single, M[A] <: MappingAt[A], X, Y]
		                              (e :ConvertedMappingSQL[F, S, M, X, Y]) :R[S, Y] =
			converted(e)
	}
}






trait ConvertedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	extends ConvertedSQL[F, S, X, Y] with AdaptedColumnSQL[F, S, X, Y]
{
	override def selectForm :ColumnReadForm[Y] = adaptation(value.selectForm)

	protected override def convert[Z](implicit conversion :SQLConversion[Y, Z]) :ColumnSQL[F, S, Z] =
		if (conversion.isIdentity) conversion(this)
		else ConvertedColumnSQL(value, this.adaptation andThen conversion)

	override def reapply[E <: RowProduct, C >: Grouped <: Single]
	                    (e :ColumnSQL[E, C, X]) :ColumnSQL[E, C, Y] =
		if (e eq value) this.asInstanceOf[ColumnSQL[E, C, Y]] else adaptation(e)

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, Y] =
		visitor.convertedColumn[S, X, Y](this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, Y, R]) :R = visitor.convertedColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F_, S_, Y] <: SQLExpression[F_, S_, Y],
//	                             R[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, R]) :R[S_, Y, E] =
//		visitor.convertedColumn(this)
}



object ConvertedColumnSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, X, Y]
	         (expr :ColumnSQL[F, S, X], conversion :SQLConversion[X, Y]) :ConvertedColumnSQL[F, S, X, Y] =
		new Impl(expr, conversion)

	def unapply[F <: RowProduct, S >: Grouped <: Single, Y](e :SQLExpression[F, S, Y])
			:Opt[(ColumnSQL[F, S, X], SQLConversion[X, Y])] forSome { type X } =
		e match {
			case promo :ConvertedColumnSQL[F, S, x, Y] => Got((promo.value, promo.adaptation))
			case _ => Lack
		}

	private class Impl[-F <: RowProduct, -S >: Grouped <: Single, X, Y]
	                  (override val value :ColumnSQL[F, S, X], override val adaptation :SQLConversion[X, Y])
		extends ConvertedColumnSQL[F, S, X, Y]


	trait SpecificConvertedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnMappingVisitor[F, S, X, Y]
	{
		def convertedColumn[V](e :ConvertedColumnSQL[F, S, V, X]) :Y
	}
	trait MatchSpecificConvertedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificConvertedColumnVisitor[F, S, X, Y] with CaseSpecificConvertedColumnMapping[F, S, X, Y]

	trait CaseSpecificConvertedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends MatchSpecificConvertedColumn[F, S, X, Y]
	{
		override def convertedColumnMapping[M[O] <: ColumnAt[O], V](e :ConvertedColumnMappingSQL[F, S, M, V, X]) :Y =
			convertedColumn(e)
	}
//
//
//	trait ConvertedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnMappingVisitor[F, Y]
//	{
//		def convertedColumn[S >: Grouped <: Single, X, V]
//		                   (e :ConvertedColumnSQL[F, S, X, V]) :Y[S, V, ConvertedColumnSQL[F, S, X, V]]
//	}
//	trait MatchConvertedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ConvertedColumnVisitor[F, Y] with CaseConvertedColumnMapping[F, Y]
//
//	trait CaseConvertedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchConvertedColumn[F, Y]
//	{
//		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], X, V]
//		                                   (e :ConvertedColumnMappingSQL[F, S, M, X, V])
//				:Y[S, V, ConvertedColumnMappingSQL[F, S, M, X, V]] =
//			convertedColumn(e)
//	}


	trait AnyConvertedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnMappingVisitor[F, R]
	{
		def convertedColumn[S >: Grouped <: Single, X, Y](e :ConvertedColumnSQL[F, S, X, Y]) :R[S, Y]
	}
	trait MatchAnyConvertedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyConvertedColumnVisitor[F, Y] with CaseAnyConvertedColumnMapping[F, Y]

	trait CaseAnyConvertedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyConvertedColumn[F, Y]
	{
		override def convertedColumnMapping[S >: Grouped <: Single, M[O] <: ColumnAt[O], X, V]
		                                    (e :ConvertedColumnMappingSQL[F, S, M, X, V]) :Y[S, V] =
			convertedColumn(e)
	}
}







trait DecoratedSQL[-F <: RowProduct, -S >: Grouped <: Single, V] extends AbstractAdaptedSQL[F, S, V, V] {
	override def selectForm  :SQLReadForm[V] = value.selectForm
	override def groundValue :Opt[V] = value.groundValue

	override def adaptation :SQLAdaptation[V, V] = new Decoration
	override def convert(x :V) :V = x
//todo: add a template ConvertsTo[Expr]
//		override def to[X](implicit lift :Lift[V, X]) :SQLExpression[F, S, X] =
//			if (!lift.isDerived || lift.isIdentity) lift(this)
//			else reapply(value.to[X])

	protected class Decoration extends DerivedAdaptation with SQLDecoration[V] {
		override lazy val swap = this
	}

	protected override def visit[R](visitor :SpecificExpressionVisitor[F, S, V, R]) :R = visitor.decorated(this)

	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, R]) :R[S, V] = visitor.decorated(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.decorated(this)
}


object DecoratedSQL {

	trait SpecificDecoratedVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificDecoratedColumnVisitor[F, S, X, Y]
	{
		def decorated(e :DecoratedSQL[F, S, X]) :Y
	}
	trait MatchSpecificDecorated[+F <: RowProduct, +S >: Grouped <: Single, X, +Y]
		extends SpecificDecoratedVisitor[F, S, X, Y]
	{
		override def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y = decorated(e)
	}
	type CaseSpecificDecorated[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		MatchSpecificDecorated[F, S, V, Y]
//
//
//	trait DecoratedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends DecoratedColumnVisitor[F, Y]
//	{
//		def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :Y[S, V, DecoratedSQL[F, S, V]]
//	}
//	trait MatchDecorated[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends DecoratedVisitor[F, Y]
//	{
//		override def decoratedColumn[S >: Grouped <: Single, V]
//		                            (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]] = decorated(e)
//	}
//	type CaseDecorated[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		MatchDecorated[F, Y]


	trait AnyDecoratedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyDecoratedColumnVisitor[F, R]
	{
		def decorated[S >: Grouped <: Single, V](e :DecoratedSQL[F, S, V]) :R[S, V]
	}
	trait MatchAnyDecorated[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] extends AnyDecoratedVisitor[F, R] {
		override def decoratedColumn[S >: Grouped <: Single, V](e :DecoratedColumnSQL[F, S, V]) :R[S, V] =
			decorated(e)
	}
	type CaseAnyDecorated[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = MatchAnyDecorated[F, R]

}




trait DecoratedColumnSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
	extends DecoratedSQL[F, S, V] with AdaptedColumnSQL[F, S, V, V]
{
	override def selectForm :ColumnReadForm[V] = value.selectForm

	protected override def visit[R[-_ >: Grouped <: Single, _]](visitor :AnyColumnVisitor[F, R]) :R[S, V] =
		visitor.decoratedColumn(this)

	protected override def visit[R](visitor :SpecificColumnVisitor[F, S, V, R]) :R = visitor.decoratedColumn(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: S,
//	                             E >: ColumnSQL[F, S, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ColumnVisitor[F, Y]) :Y[S_, Boolean, E] =
//		visitor.decoratedColumn(this)
}


object DecoratedColumnSQL {
	trait SpecificDecoratedColumnVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def decoratedColumn(e :DecoratedColumnSQL[F, S, X]) :Y
	}
	type MatchSpecificDecoratedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificDecoratedColumnVisitor[F, S, X, Y]
	type CaseSpecificDecoratedColumn[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] =
		SpecificDecoratedColumnVisitor[F, S, X, Y]
//
//	trait DecoratedColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def decoratedColumn[S >: Grouped <: Single, V]
//		                   (e :DecoratedColumnSQL[F, S, V]) :Y[S, V, DecoratedColumnSQL[F, S, V]]
//	}
//	type MatchDecoratedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		DecoratedColumnVisitor[F, Y]
//	type CaseDecoratedColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		DecoratedColumnVisitor[F, Y]


	trait AnyDecoratedColumnVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] {
		def decoratedColumn[S >: Grouped <: Single, V](e :DecoratedColumnSQL[F, S, V]) :R[S, V]
	}
	type MatchAnyDecoratedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyDecoratedColumnVisitor[F, Y]
	type CaseAnyDecoratedColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyDecoratedColumnVisitor[F, Y]
}

