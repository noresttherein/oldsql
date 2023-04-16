package net.noresttherein.oldsql.sql.ast

import java.sql.JDBCType

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.exceptions.{MisalignedExpressionException, MismatchedExpressionsException, MisspelledSQLException}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, SQLForm, SQLReadForm}
import net.noresttherein.oldsql.slang.mappingMethods
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, ConvertingOps, GroundingOps, Grouped, ReorderingTemplate, Single, SpecificExpressionVisitor, VariantGroundingTemplate}
import net.noresttherein.oldsql.sql.ast.CompositeSQL.UnaryCompositeSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.RearrangedMappingSQL
import net.noresttherein.oldsql.sql.ast.MappingSQL.RearrangedMappingSQL.{AnyRearrangedMappingVisitor, CaseAnyRearrangedMapping, CaseSpecificRearrangedMapping, SpecificRearrangedMappingVisitor}
import net.noresttherein.oldsql.sql.ast.RearrangedSQL.RearrangedSQLTemplate
import net.noresttherein.oldsql.sql.ast.TransformedSQL.{AbstractTransformedSQL, ReshapedSQLTemplate, WrappedSQLTemplate}
import net.noresttherein.oldsql.sql.mechanics.{AlignableColumn, AlignableColumns, Reform, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.NoReform
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLAdaptation.IndependentAdaptation
import net.noresttherein.oldsql.sql.mechanics.SQLTransformation.{ArbitraryTransformation, GenericTransformation, IndependentTransformation}
import net.noresttherein.oldsql.sql.mechanics.SQLValueConversion.SQLValueIdentity






/** An SQL expression wrapper which changes the order of its columns, possibly removing some,
  * including optional ones, or adding null columns.
  * As the exact number and order of columns is not in general specified for an `SQLExpression` and may vary
  * depending on the context of use, it should not be considered a general purpose class.
  * Instead, it is used late in the spelling process when aligning column sets of several expressions,
  * in particular ''select'' clauses of member ''selects'' in a ''compound select''.
  *
  * The ordering is based on
  * `value.`[[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns potentialColumns]]`.`[[net.noresttherein.oldsql.sql.mechanics.AlignableColumns.columns columns]],
  * not `value.`[[net.noresttherein.oldsql.sql.SQLExpression.split split]], with the former columns constituting
  * the 'exposed/public' columns in `Rearrangement` terminology. The number of columns in this expression
  * equals `order.`[[net.noresttherein.oldsql.pixies.Rearrangement.underlyingColumnCount underlyingColumnCount]].
  * The `n`-th column of `value.potentialColumns.columns` will become the `order.inverse(n)`-th column of this expression,
  * unless `!order.isMapped(n)`, in which case the column is omitted (counting starts from `1` for this purpose).
  * If not all indices in `[1..order.columnCount]` are assigned new columns, then null columns
  * are inserted at omitted indices. Note that the number of default columns of `value` can be determined only during
  * spelling, that is in presence of an implicit [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]],
  * which this class takes as a parameter.
  * However, [[net.noresttherein.oldsql.sql.ast.RearrangedSQL.RearrangedSQLTemplate.explodedSpelling explodedSpelling]]
  * (and `defaultSpelling`) will use whatever `SQLSpelling` is passed as an argument.
  */
class RearrangedSQL[-F <: RowProduct, -S >: Grouped <: Single, V]
                   (override val value :SQLExpression[F, S, V], override val order :Rearrangement)
                   (implicit protected override val spelling :SQLSpelling)
	extends TransformedSQL[F, S, V, V] with RearrangedSQLTemplate[F, S, V, SQLExpression]
{

/*

	override def groundValue :Opt[V] = value.groundValue
	override def transformation :SQLTransformation[V, V] = rearranging
	private[this] val rearranging = new RearrangingTransformation

	//It would be great to have order defined over all potentialColumns,
	// but it's currently impossible to implement split and spelling if some non default columns were included.
	def defaultOrder :Rearrangement = order //rearranging.defaultOrder


	private class RearrangingTransformation(implicit spelling :SQLSpelling)
		extends ArbitraryTransformation[V, V] with SQLValueIdentity[V]
	{
//		val defaultOrder :Rearrangement = spellingOrder

		override def apply(form :ColumnReadForm[V]) :ColumnReadForm[V] = {
			if (defaultOrder.columnCount != 1 || !defaultOrder.isMapped(1) || defaultOrder(1) != 1)
				throw new IllegalArgumentException(
					"Cannot reorder a column form with " + defaultOrder + " (adapted from rearranging " + value +
						" with " + order + ")."
				)
				form
//			form >> (defaultOrder(1) - 1)
		}
		override def apply(form :ColumnForm[V]) :ColumnForm[V] = {
			if (defaultOrder.columnCount != 1 || !defaultOrder.isMapped(1) || defaultOrder(1) != 1)
				throw new IllegalArgumentException(
					"Cannot reorder a column form with " + defaultOrder + " (adapted from rearranging " + value +
						" with " + order + ")."
				)
			form
//			form >> (defaultOrder(1) - 1)
		}
		override def apply(form :SQLReadForm[V]) :SQLReadForm[V] = form.reorder(defaultOrder)
		override def apply(form :SQLForm[V]) :SQLForm[V] = form.reorder(defaultOrder)

		override def apply[C <: RowProduct, A >: Grouped <: Single, E[v] <: ConvertibleSQL[C, A, v, E]]
		                  (expr :ConvertingOps[C, A, V, E]) =
			expr match {
				case null =>
					new RearrangedSQL(MultiNull(order.columnCount), order)
				case other :RearrangedSQL[C, A, V] =>
					new RearrangedSQL[C, A, V](other.value, other.order compose order)
				case _ =>
					new RearrangedSQL(expr, order)
			}

		override lazy val swap = this
		override def applyString(arg :String) = arg + ".reorder(" + order + ")"
	}


	private def spellingOrder(implicit spelling :SQLSpelling) :Rearrangement = {
		val allColumns = spelling.potentialColumns(value, NoReform)
		val columnCount = allColumns.columns.length
		if (columnCount != order.columnCount)
			throw new IllegalArgumentException(
				"The number of (potential) columns of `" + value + "` (" + allColumns.columns.length +
					") does not match the number expected by reordering " + order + " (" + order.columnCount + "): " +
					allColumns + "."
			)
		if (allColumns.columns.length == spelling.columnCount(value))
			order
		else {
			//index of n-th default column within alignableColumns
			val defaultIndexing = allColumns.columns.view.zipWithIndex.filter(_._1.isDefault).map(_._2).toIndexedSeq
			val fullIndexing = (0 until columnCount).map { i => order(defaultIndexing(i) + 1) - 1 }
			Rearrangement(fullIndexing)
		}
	}

	//	override def reorder(permutation :IndexedSeq[Int]) :SQLExpression[F, S, V] =
	//		if (order.underlyingColumnCount == permutation.length &&
	//			permutation.indices.forall { i => order.isCovered(i) && i == permutation(order.inverse(i)) }
	//		)
	//			value
	//		else
	//			new RearrangedSQL(value, order compose Rearrangement.inverse(permutation))
	//
	protected override def reform(reordering :Rearrangement)(implicit spelling :SQLSpelling) :SQLExpression[F, S, V] =
		spelling.reorder(value, reordering compose order)

	//overridden to prevent reforming of this expression.
	protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2, EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
	                                       spelling :SQLSpelling)
			:(leftResult.Expression[F, S, SQLExpression[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		if (this eq other)
			(leftResult(this), rightResult(other))
		else if (passCount.firstTime)
			passReform(other)(reform.prohibitReformLeft, passCount)
		else
			reformer[F2, S2, V2, EC2, U](other)(reform.prohibitReformLeft, passCount).apply(other)

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

	protected override def potentialColumns(permissions :Permissions)
	                                       (implicit spelling :SQLSpelling) :AlignableColumns =
	{
		val alignableColumns = spelling.potentialColumns(value, NoReform)
		val includedColumns  = alignableColumns.columns.view.filter(_.isDefault).toIndexedSeq
		val reorderedColumns = order.reorder(includedColumns, AlignableColumn(SQLNull[Null]()))
/*
		val includedColumns = alignableColumns.columns.view.zipWithIndex.filter(_._1.isDefault).toIndexedSeq
		@inline def isSameOrder =
			(0 until includedColumns.length).forall { i =>
				!defaultOrder.isMapped(i) && !order.isMapped(includedColumns(i)._2) ||
					defaultOrder(i) == order(includedColumns(i)._2)
			}
		val effectiveOrder =
			if (includedColumns.length == defaultOrder.columnCount && isSameOrder)
				defaultOrder
			else
				spellingOrder
		//we just drop all non-included columns because we are final anyway.
		val reorderedColumns = effectiveOrder.reorder(includedColumns.map(_._1), AlignableColumn(SQLNull[Null]()))
*/
		new AlignableColumns(this, NoReform, reorderedColumns)

//		//if order is not a surjection, the result must contain Null columns
//		val Null = AlignableColumn(SQLNull[Null]())(_ == _)
//		val publicColumns  = order.reorder(includedColumns, Null)
//		/* A permutation of allColumns which swaps only columns included in publicColumns, so that their relative order
//		 * is the same as in publicColumns. However, if order is not a surjection, we need still to inject null columns
//		 * into the result in such a way that result.filter(_.isDefault) == publicColumns.
//		 */
//		val fullPermutation = superPermutation(allColumns, publicColumns.view.filter(_ ne Null) to Unique)
//		val reorderedColumns =
//			if (order.isSurjection)
//				fullPermutation.map(allColumns(_))
//			else {
//				val publicIndices  = publicColumns.view.zipWithIndex.toMap
//				fullPermutation.flatMap { i => //if
//					val column = allColumns(i)
//					publicIndices.getOrElse(column, -1) match {
//						case -1 =>
//							Some(column) //not included in publicColumns
//						case n =>
//							//a default (included) column; if it is followed by null columns in the reordered sequence
//							// publicColumns, squeeze that many null columns after it.
//							var nullsCount = n + 1
//							while (nullsCount < publicColumns.size && (publicColumns(nullsCount) eq Null))
//								nullsCount += 1
//							nullsCount -= n + 1
//							if (nullsCount == 0)
//								Some(column)
//							else
//								column +: ConstSeq(Null, nullsCount)
//					}
//				}
//			}
	}

	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = {
		val underlying = spelling.split(value).toIndexedSeq
		val effectiveOrder =
			if (underlying.length == defaultOrder.columnCount) defaultOrder
			else spellingOrder
		if (underlying.length != effectiveOrder.columnCount)
			throw new AssertionError(
				"The number of (included) columns of `" + value + "` (" + underlying.length +
					") does not match the number expected by reordering " + effectiveOrder +
					" (" + effectiveOrder.columnCount + "): " + underlying + " (from " + order + ")."
			)
		effectiveOrder.reorder(underlying, SQLNull[Null]())
	}

	protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
		spelling.shape(value) match {
			case RowShape(exposed) =>
				//correct check would need to verify that
//				val effectiveOrder =
//					if (exposed.length == defaultOrder.columnCount) defaultOrder
//					else spellingOrder
				RowShape(defaultOrder.reorder(exposed.toIndexedSeq, JDBCType.NULL))
			case indefinite => indefinite
		}
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
		if (virtual.length != defaultOrder.columnCount)
			throw new MisspelledSQLException(
				"Cannot spell a reordered expression " + this + " with " + spelling + " because its number of columns " +
					virtual.length + " does not match the number of columns of reordering " + defaultOrder.columnCount + ": " +
					virtual + "."
			)
		val outContext = virtual.last.context
		if (defaultOrder.isCovered(defaultOrder.underlyingColumnCount)) { //set context of the SQL for our last column to outContext
			val last = order(order.underlyingColumnCount) - 1
			val sqls = virtual.mapWithIndex { (sql, i) =>
				if (i == last) SpelledSQL(sql.sql, sql.setter, outContext)
				else SpelledSQL(sql.sql, sql.setter, context)
			}
			defaultOrder.reorder(sqls, SpelledSQL(spelling.NULL, context))
		} else {//our last column is a dummy, so the missing argument to reorder will be used for it - with outContext
			val sqls = virtual.map(sql => SpelledSQL(sql.sql, sql.setter, context))
			defaultOrder.reorder(sqls, SpelledSQL(spelling.NULL, outContext))
		}
	}
*/

	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
	                               (e :SQLExpression[E, C, X]) :SQLExpression[E, C, X] =
		spelling.realign(e, order)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, S, V, Y]) :Y = visitor.rearranged(this)
	protected override def visit[Y[-_ >: Grouped <: Single, _]](visitor :AnyExpressionVisitor[F, Y]) :Y[S, V] =
		visitor.rearranged(this)
//
//	protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//	                             E >: SQLExpression[F_, S_, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.rearranged(this)

	override def toString :String = order.toString + "(" + value + ")"
}



object RearrangedSQL {

	def apply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V], order :Rearrangement)
	                                                     (implicit spelling :SQLSpelling) :RearrangedSQL[F, S, V] =
	{
		if (spelling.columnCount(e) != order.columnCount)
			throw new IllegalArgumentException(
				"Cannot reorder a " + spelling.columnCount(e) + "-column expression `" + e + "` according to a " +
					order.columnCount + "-column " + order + "."
			)
		new RearrangedSQL(e, order)
	}

	trait RearrangedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                            +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                  <: ConvertibleSQL[f, s, v, ({ type E[A] = Same[f, s, A] })#E]
		                                    with ReorderingTemplate[f, s, v, Same[f, s, v]]]
		extends WrappedSQLTemplate[F, S, V, Same]
		   with ReshapedSQLTemplate[F, S, V, V, Same]
	{ self :Same[F, S, V] with RearrangedSQLTemplate[F, S, V, Same]
	                      with GroundingOps[F, S, V, ({ type E[-f <: RowProduct] = Same[f, S, V] })#E, Same[F, S, V]] =>

		implicit protected def spelling :SQLSpelling
		override val value :Same[F, S, V]

		val order :Rearrangement
		protected def defaultOrder :Rearrangement = order

		protected def spellingOrder(implicit spelling :SQLSpelling) :Rearrangement = {
			val allColumns = spelling.potentialColumns(value, NoReform)
			val columnCount = allColumns.columns.length
			if (columnCount != order.columnCount)
				throw new IllegalArgumentException(
					"The number of (potential) columns of `" + value + "` (" + allColumns.columns.length +
						") does not match the number expected by reordering " + order + " (" + order.columnCount + "): " +
						allColumns + "."
				)
			if (allColumns.columns.length == spelling.columnCount(value))
				order
			else {
				//index of n-th default column within alignableColumns
				val defaultIndexing = allColumns.columns.view.zipWithIndex.filter(_._1.isDefault).map(_._2).toIndexedSeq
				val fullIndexing = (0 until columnCount).map { i => order(defaultIndexing(i) + 1) - 1 }
				Rearrangement(fullIndexing)
			}
		}
		override def groundValue :Opt[V] = value.groundValue
//		override def selectForm = value.selectForm.reorder(defaultOrder)
//		override def universalForm :Opt[SQLForm[V]] = value.universalForm.
		protected def isUniversal :Boolean = true

		override val transformation :IndependentTransformation.Convertible[V, V, Same] =
			new ReorderingTransformation[Same]

		protected class ReorderingTransformation
		               [Wrap[-f <: RowProduct, -s >: Grouped <: Single, v] >: Same[f, s, v]
		                     <: ConvertibleSQL[f, s, v, ({type E[A] = Wrap[f, s, A]})#E]
		                           with ReorderingTemplate[f, s, v, Wrap[f, s, v]]]
			extends GenericTransformation[V, V, Wrap] with SQLValueIdentity[V]
		{
			/* We might require that the shape of the argument expression is the same as that of the outer expression,
			 * because it is highly unlikely this transformation was intended to reorder other expressions than value
			 * and will yield unpredictable results. Currently TransformedSQL.reform performs compatibility validation,
			 * but that is on top of possibly other transformations. There is an issue with the former approach,
			 * however, and that is used Reform may have different compatibility criterion, including none at all
			 * for partial reforms.
			 */
//			val defaultOrder :Rearrangement = spellingOrder
			override def isUniversal :Boolean = self.isUniversal

			override def apply(form :ColumnReadForm[V]) :ColumnReadForm[V] = {
				if (defaultOrder.columnCount != 1 || !defaultOrder.isMapped(1) || defaultOrder(1) != 1)
					throw new IllegalArgumentException(
						"Cannot reorder a column form with " + defaultOrder + " (adapted from rearranging " + value +
							" with " + order + ")."
					)
					form
//				form >> (defaultOrder(1) - 1)
			}
			override def apply(form :ColumnForm[V]) :ColumnForm[V] = {
				if (defaultOrder.columnCount != 1 || !defaultOrder.isMapped(1) || defaultOrder(1) != 1)
					throw new IllegalArgumentException(
						"Cannot reorder a column form with " + defaultOrder + " (adapted from rearranging " + value +
							" with " + order + ")."
					)
				form
//				form >> (defaultOrder(1) - 1)
			}
			override def apply(form :SQLReadForm[V]) :SQLReadForm[V] = form.reorder(defaultOrder)
			override def apply(form :SQLForm[V]) :SQLForm[V] = form.reorder(defaultOrder)

			override def apply[f <: RowProduct, s >: Grouped <: Single, E[v] <: ConvertibleSQL[f, s, v, E]]
			                  (expr :ConvertingOps[f, s, V, E]) :Wrap[f, s, V] =
				self.decorate(denullify(expr))

			override def applyString(arg :String) :String = arg + ".reorder(" + order + ")"
		}

//		protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
//		                               (e :SQLExpression[E, C, X]) :Same[E, C, X] =

		protected override def realign(reordering :Rearrangement)(implicit spelling :SQLSpelling) :Same[F, S, V] =
			spelling.realign(value, reordering compose order)

		//overridden to prevent reforming of this expression.
		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2, EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                             (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                             (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
		                                       spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			if (this eq other)
				(leftResult(self), rightResult(other))
			else
				super.reform(other)(reform.prohibitReformLeft, passCount)


		protected override def potentialColumns(permissions :Permissions)
		                                       (implicit spelling :SQLSpelling) :AlignableColumns =
		{
			val alignableColumns = spelling.potentialColumns(value, NoReform)
			val includedColumns  = alignableColumns.columns.view.filter(_.isDefault).toIndexedSeq
			val reorderedColumns = order.reorder(includedColumns, AlignableColumn(SQLNull[Null]()))
/*
			val includedColumns = alignableColumns.columns.view.zipWithIndex.filter(_._1.isDefault).toIndexedSeq
			@inline def isSameOrder =
				(0 until includedColumns.length).forall { i =>
					!defaultOrder.isMapped(i) && !order.isMapped(includedColumns(i)._2) ||
						defaultOrder(i) == order(includedColumns(i)._2)
				}
			val effectiveOrder =
				if (includedColumns.length == defaultOrder.columnCount && isSameOrder)
					defaultOrder
				else
					spellingOrder
			//we just drop all non-included columns because we are final anyway.
			val reorderedColumns = effectiveOrder.reorder(includedColumns.map(_._1), AlignableColumn(SQLNull[Null]()))
*/
			new AlignableColumns(this, NoReform, reorderedColumns)
/*
			//if order is not a surjection, the result must contain Null columns
			val Null = AlignableColumn(SQLNull[Null]())(_ == _)
			val publicColumns  = order.reorder(includedColumns, Null)
			/* A permutation of allColumns which swaps only columns included in publicColumns, so that their relative order
			 * is the same as in publicColumns. However, if order is not a surjection, we need still to inject null columns
			 * into the result in such a way that result.filter(_.isDefault) == publicColumns.
			 */
			val fullPermutation = superPermutation(allColumns, publicColumns.view.filter(_ ne Null) to Unique)
			val reorderedColumns =
				if (order.isSurjection)
					fullPermutation.map(allColumns(_))
				else {
					val publicIndices  = publicColumns.view.zipWithIndex.toMap
					fullPermutation.flatMap { i => //if
						val column = allColumns(i)
						publicIndices.getOrElse(column, -1) match {
							case -1 =>
								Some(column) //not included in publicColumns
							case n =>
								//a default (included) column; if it is followed by null columns in the reordered sequence
								// publicColumns, squeeze that many null columns after it.
								var nullsCount = n + 1
								while (nullsCount < publicColumns.size && (publicColumns(nullsCount) eq Null))
									nullsCount += 1
								nullsCount -= n + 1
								if (nullsCount == 0)
									Some(column)
								else
									column +: ConstSeq(Null, nullsCount)
						}
					}
				}
*/
		}

		protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
			order.underlyingColumnCount

		protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, S, _]] = {
			val underlying = spelling.split(value).toIndexedSeq
			val effectiveOrder =
				if (underlying.length == defaultOrder.columnCount) defaultOrder
				else spellingOrder
			if (underlying.length != effectiveOrder.columnCount)
				throw new AssertionError(
					"The number of (included) columns of `" + value + "` (" + underlying.length +
						") does not match the number expected by reordering " + effectiveOrder +
						" (" + effectiveOrder.columnCount + "): " + underlying + " (from " + order + ")."
				)
			effectiveOrder.reorder(underlying, SQLNull[Null]())
		}

		protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
			spelling.shape(value) match {
				case RowShape(exposed) =>
					//correct check would need to verify that
	//				val effectiveOrder =
	//					if (exposed.length == defaultOrder.columnCount) defaultOrder
	//					else spellingOrder
					RowShape(defaultOrder.reorder(exposed.toIndexedSeq, JDBCType.NULL))
				case indefinite => indefinite
			}
		}

		protected override def columnCount(implicit spelling :SQLSpelling) :Int =
			if (this.spelling != spelling || spelling.columnCount(value) != defaultOrder.columnCount)
				throw new MisalignedExpressionException(
					"SQLSpelling " + spelling + " passed to `" + this +
						"` on creation is incompatible with given spelling " + spelling + ".")
			else
				order.underlyingColumnCount

		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			( "(" +: spelling.inlineSpelling(this)(from, context, params)) + ")"

		protected override def explodedSpelling[P](independent :Boolean)
		                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		{   //independent = true because we reorder setters.
			val virtual = spelling.explode(value, true)(from, context, params).toIndexedSeq
			if (virtual.length != defaultOrder.columnCount)
				throw new MisspelledSQLException(
					"Cannot spell a reordered expression " + this + " with " + spelling + " because its number of columns " +
						virtual.length + " does not match the number of columns of reordering " + defaultOrder.columnCount + ": " +
						virtual + "."
				)
			val outContext = virtual.last.context
			if (defaultOrder.isCovered(defaultOrder.underlyingColumnCount)) { //set context of the SQL for our last column to outContext
				val last = order(order.underlyingColumnCount) - 1
				val sqls = virtual.mapWithIndex { (sql, i) =>
					if (i == last) SpelledSQL(sql.sql, sql.setter, outContext)
					else SpelledSQL(sql.sql, sql.setter, context)
				}
				defaultOrder.reorder(sqls, SpelledSQL(spelling.NULL, context))
			} else {//our last column is a dummy, so the missing argument to reorder will be used for it - with outContext
				val sqls = virtual.map(sql => SpelledSQL(sql.sql, sql.setter, context))
				defaultOrder.reorder(sqls, SpelledSQL(spelling.NULL, outContext))
			}
		}

	}



	trait SpecificRearrangedVisitor[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificRearrangedMappingVisitor[F, S, V, Y]
	{
		def rearranged(e :RearrangedSQL[F, S, V]) :Y
	}
	type MatchSpecificRearranged[+F <: RowProduct, +S >: Grouped <: Single, V, +Y] =
		SpecificRearrangedVisitor[F, S, V, Y]

	trait CaseSpecificRearranged[+F <: RowProduct, +S >: Grouped <: Single, V, +Y]
		extends SpecificRearrangedVisitor[F, S, V, Y] with CaseSpecificRearrangedMapping[F, S, V, Y]
	{
		override def rearrangedMapping[M[O] <: MappingAt[O]](e :RearrangedMappingSQL[F, S, M, V]) :Y = rearranged(e)
	}
//
//	trait RearrangedVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def rearranged[S >: Grouped <: Single, V](e :RearrangedSQL[F, S, V]) :Y[S, V, RearrangedSQL[F, S, V]]
//	}
//	type MatchRearranged[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		RearrangedVisitor[F, Y]
//	type CaseRearranged[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		RearrangedVisitor[F, Y]

	trait AnyRearrangedVisitor[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyRearrangedMappingVisitor[F, R]
	{
		def rearranged[S >: Grouped <: Single, V](e :RearrangedSQL[F, S, V]) :R[S, V]
	}
	type MatchAnyRearranged[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]] = AnyRearrangedVisitor[F, R]

	trait CaseAnyRearranged[+F <: RowProduct, +R[-_ >: Grouped <: Single, _]]
		extends AnyRearrangedVisitor[F, R] with CaseAnyRearrangedMapping[F, R]
	{
		override def rearrangedMapping[S >: Grouped <: Single, M[O] <: MappingAt[O], V]
		                              (e :RearrangedMappingSQL[F, S, M, V]) :R[S, V] = rearranged(e)
	}
}





