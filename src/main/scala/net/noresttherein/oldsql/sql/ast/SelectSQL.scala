package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.OperationView.SelectView
import net.noresttherein.oldsql.collection.Chain.{@~, ChainApplication}
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{Bug, MismatchedExpressionsException}
import net.noresttherein.oldsql.schema.{ColumnReadForm, SQLReadForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.{IndexedColumnMapping, IndexedMapping}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.{ColumnSQL, Dual, FromSome, GroupedColumn, GroupedSQL, Query, RowProduct, RowShape, Select, SQLExpression, TypedColumnSQLMapping, TypedListingColumnSQLMapping, TypedListingSQLMapping, TypedSQLMapping, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, ConvertingColumnTemplate, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, GroundRow, NonEmptyRow, PartOf, SubselectOf}
import net.noresttherein.oldsql.sql.Select.{SelectMapping, SelectTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, ConvertibleSQL, GroundSQLTemplate, Grouped, Single, SingleRowSQLTemplate, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnQuery.ColumnSingleQuery
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{LabeledColumnSQL, LabeledValueSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{Rows, SingleQuerySQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.{AnySelectAsVisitor, AnySubselectAsVisitor, AnyTopSelectAsVisitor, BaseSubselectAs, BaseTopSelectAs, CaseAnySubselectAs, CaseAnyTopSelectAs, CaseSpecificSubselectAs, CaseSpecificTopSelectAs, MatchAnySelectAs, MatchSpecificSelectAs, SpecificSelectAsVisitor, SpecificSubselectAsVisitor, SpecificTopSelectAsVisitor, SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumn.{AnySelectColumnVisitor, AnySubselectColumnVisitor, AnyTopSelectColumnVisitor, SpecificSelectColumnVisitor, SpecificSubselectColumnVisitor, SpecificTopSelectColumnVisitor, SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{AnySelectColumnAsVisitor, AnySubselectColumnAsVisitor, AnyTopSelectColumnAsVisitor, BaseSubselectColumnAs, BaseTopSelectColumnAs, SpecificSelectColumnAsVisitor, SpecificSubselectColumnAsVisitor, SpecificTopSelectColumnAsVisitor, SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{ArbitrarySubselectColumn, ArbitraryTopSelectColumn, SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.{QueryReform, Reform, RelationOffset, SpelledSQL, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.Reform.PassCount
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLConversion.ConvertRows
import net.noresttherein.oldsql.sql.Query.QueryReformingTemplate

//here be implicits
import net.noresttherein.oldsql.slang._






/** Representation of an SQL ''select'' as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
  * depending on relations listed by ''from'' clause `F`. If the first type argument is the wildcard `RowProduct`,
  * this will be a `TopSelectSQL` instance - a select independent of any external
  * tables or parameters, in which all expressions (''select'', ''where'' and other clauses) can be evaluated
  * based on the values of the tables in its [[net.noresttherein.oldsql.sql.ast.SelectSQL.From From]] clause.
  * If `F` is not `RowProduct`, but contains tables, then this is a ''dependent select'' (subselect),
  * nested inside a ''select'' with `F` as its ''from'' clause - in the latter's ''select'', ''from'' or ''where''
  * clause. Type `From` of this class is always a direct [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy expansion]]
  * of type `RowProduct`, satisfying `From <:`[[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]`.
  * It is referred to here for simplicity as the ''from'' clause of this select, but can be in fact
  * a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] representing its ''from'', ''where'',
  * ''group by'' and ''having'' clauses. Be warned that in that case
  * (or when `From <: `[[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[_]`), the relation listed by
  * [[net.noresttherein.oldsql.sql.Select.SelectTemplate.relations relations]] and
  * [[net.noresttherein.oldsql.sql.Select.SelectTemplate.from from]]`.`[[net.noresttherein.oldsql.sql.RowProduct.tableStack tableStack]]
  * are synthetic mappings representing the expressions in the ''group by'' clause, rather than tables of the ''from''
  * clause - see method [[net.noresttherein.oldsql.sql.Select.SelectTemplate.tables tables]] for the latter.
  * Regardless of whether this is a top select or a subselect expression, `From` never contains any
  * [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in its ''explicit'' section; type `F` however -
  * included also in the prefix of `From` - can contain parameters. This allows the use of subselect expressions
  * (as it follows from the above that top selects are always parameterless) inside parameterized
  * [[net.noresttherein.oldsql.sql.Select Select]] quasi expressions.
  *
  * Subclasses should extend the trait for one of the above cases:
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]] or
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]], instead of being derived directly
  * from this trait.
  *
  * @tparam F the source of data for the ''enclosing'' select - tables from the ''from'' clause and any unbound parameters.
  * @tparam R the combined type of the whole ''select'' clause, to which every returned row maps.
  */
//Consider: make it selectable by converting to a CommonTableExpression and joining From Join RowMapping.
// Easy (and not very useful) if it is the whole select clause, but if it is embedded deeper in an expression
// we need to stop spelling/creating an SQLMapping and rewrite the whole select clause expression to use the join.
// Alternatively, we could try to rewrite the whole expression using this SelectSQL into an expression using
// a derived select, with the comparison it takes part in (or other expression) incorporated in that select.
// This will require a LateralJoin if the select refers to tables in `From`
sealed trait SelectSQL[-F <: RowProduct, R] //can't be a MappingSQL because of Rows[V] value type
	extends SingleQuerySQL[F, R]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = SelectSQL[f, R] })#Q]
	   with SelectTemplate[R, ({ type Q[X] = SelectSQL[F, X] })#Q]
{
	override def selectForm   :SQLReadForm[Rows[R]] = rowForm.nullMap(Rows(_))
	override def rowForm      :SQLReadForm[R] = selectClause.selectForm

	/** The from clause of this select. */
	override type From <: SubselectOf[F]

	def isSubselect :Boolean = from.isSubselect

	//order by will be super problematic here
	//we can't sadly move it to SelectTemplate, because SelectColumn cannot return SelectColumn
	def selectOther[X](selectClause :SQLExpression[From, Grouped, X]) :SelectSQL[F, X] =
		if (isDistinct)
			(selectClause selectFrom from).distinct
		else
			selectClause selectFrom from

	override def groundValue :Opt[Rows[R]] = selectClause.groundValue.map(Rows.single)
	override def isGround    :Boolean      = selectClause.isGround
	override def isAnchored = true
	override def isAnchored(from :F) :Boolean = this.from.outer == from //todo: define _exact_ semantics of equality method(s)
//	override def anchor(from :F) :SelectSQL[F, V]
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectSQL[E, V]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SelectSQL[E, V]



	protected override def reformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
                                    EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                   (implicit leftResult  :SQLTransformation[Rows[R], U],
                                             rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
			:SpecificExpressionVisitor
			 [F2, S2, V2, (leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]])] =
		new SelectReformer[F1, F2, S2, V2, EC2, U, leftResult.SQLResult, rightResult.SQLResult](other)(reform, passCount)

	protected class SelectReformer[F1 <: F, S1 >: Grouped <: Single, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                           EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U,
		                           LR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U],
		                           RR[-f <: RowProduct, -s >: Grouped <: Single, +e <: SQLExpression[f, s, U]] <: SQLExpression[f, s, U]]
		                          (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                          (implicit leftResult  :SQLTransformation[Rows[R], U]#Into[LR],
		                                    rightResult :SQLTransformation[V2, U]#Into[RR], spelling :SQLSpelling)
		extends BaseReformer[F1, Single, F2, S2, V2, EC2, U, LR, RR](other)(reform, passCount)
	{
		override def select[R2](e :SelectSQL[F2, R2])(implicit isRows :V2 =:= Rows[R2]) = {
			implicit val rightSelectResult = isRows.liftCo[RightResult](rightResult)

			(splitRowsTransformation(leftResult), splitRowsTransformation(rightSelectResult)) match { //reform the select clauses of each
				case (Got((leftItems :SQLConversion[R, Any] @unchecked, leftRes :LeftResult[Any] @unchecked)),
				      Got((rightItems :SQLConversion[R2, Any] @unchecked, rightRes :RightResult[Any] @unchecked)))
				if leftRes == rightRes =>
					/* The unification below type checks only because we cast both leftItems and rightItems
					 * to SQLConversion[R/R2, Any]. This would be dangerous in general, because we might deeper down assign
					 * an incompatible SQLForm to a term (among other problems). We require thus that the lifts
					 * applied to the whole selects are equal (essentially implying their being identities)
					 * as a sort of guarantee that the value types of the first pair are compatible
					 * (because Lift is invariant). This isn't much of a restriction, as there is no reason
					 * for anyone to create an expression with Rows value type which isn't a select
					 * and we don't lift anything to Rows automatically.
					 */
					val (leftClause, rightClause) =
						reform(selectClause, e.selectClause)(leftItems, rightItems, spelling.inSelect)
					val left = leftRes(leftClause selectFrom from)
					val right = rightRes(rightClause selectFrom e.from)
					(left, right)
				case (Got((_, leftRes)), Got((_, rightRes))) =>
					fallbackGuard(
						s"incompatible conversion types ($leftResult, $rightResult) ending with ($leftRes, $rightRes)."
					)
				case (Got((_, _)), _) =>
					fallbackGuard(
						"unsupported type conversion for Rows of the right expression: " + rightResult + "."
					)
				case (_, Got((_, _))) =>
					fallbackGuard(
						"unsupported type conversion for Rows of the left expression: " + leftResult  + "."
					)
				case _ =>
					fallbackGuard(
						s"unsupported type conversions for a SELECT pair: ($leftResult, $rightResult)."
					)
			}
		}

		override def expression(e :SQLExpression[F2, S2, V2]) =
			if (passCount.mayPass)
				passReform(e)(reform, passCount)
			else {
				splitRowsTransformation(leftResult) match {
					case Got((row, rows)) if rows == SQLConversion.selectRow =>
						val (left, right) = reform(selectClause, e)(row.castParam2[U], rightResult, spelling.inSelect)
						val select = SQLConversion.selectRow(left selectFrom from)
						(select, right)
					case _ =>
						fallbackGuard(
							"Non rencilable type conversions (left ends with " + rows + ", not SQLConversion.selectRow: "
							+ leftResult + " vs " + rightResult + "."
						)
				}
			}
	}


//	protected override def reformer[E <: RowProduct, C >: Grouped <: Single, X, U]
//	                               (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
//	                               (implicit leftResult :Lift[Rows[R], U], rightResult :Lift[X, U], spelling :SQLSpelling)
//			:SpecificExpressionVisitor[E, C, X, (SQLExpression[F, Single, U], SQLExpression[E, C, U])] =
//		new SelectReformer[E, C, X, U](other)(reform, passesAllowed)
//
////	protected class SelectReformer[E <: RowProduct, C >: Grouped <: Single, X, U]
////	                              (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
////	                              (implicit leftResult :Lift[Rows[R], U], rightResult :Lift[X, U], spelling :SQLSpelling)
////		extends BaseReformer[E, C, X, U](other)(reform, passesAllowed)
//	{
//		override def expression(e :SQLExpression[E, C, X]) :(SQLExpression[F, Single, U], SQLExpression[E, C, U]) =
//			if (passesAllowed > 0)
//				e.`->reform`(SelectSQL.this)(reform.swap, passesAllowed - 1).swap
//			else
//				(leftResult(SelectSQL.this), rightResult(e)) match {
//					case (l, r) if !leftResult.isDerived || !rightResult.isDerived =>
//						reform(l, r)
//					case (l, r) => splitRowsLift(leftResult) match {
//						case Got((item, rows)) if rows == Lift.selectRow =>
//							val (left, right) = reform(selectClause, e)(
//								item.asInstanceOf[Lift[R, U]] vs rightResult, spelling.inSelect
//							)
//							val select = (left selectFrom from) to Lift.selectRow[U]
//							(select, right)
//						case _ => try {
//							reform.fallback(l, r)
//						} catch {
//							case e :MismatchedExpressionsException =>
//								throw e.addInfo(
//									"Non reconcilable type lifts (left ends with " + rows + ", not Row[X]=>X): " +
//										leftResult + " vs " + rightResult + ".")
//						}
//					}
//				}
//
//		override def select[R](e :SelectSQL[E, R])(implicit isRows :X =:= Rows[R])
//				:(SQLExpression[F, Single, U], SQLExpression[E, C, U]) =
//			if (!leftResult.isDerived || !rightResult.isDerived)
//				this.reform(leftResult(SelectSQL.this), rightResult(other))
//			else {
//				implicit val rightSelectResult = isRows.liftCo[({ type T[A] = Lift[A, U] })#T](rightResult)
//				(splitRowsLift(leftResult), splitRowsLift(rightSelectResult)) match { //reform the select clauses of each
//					case (Got((leftItems, leftRes)), Got((rightItems, rightRes))) if leftRes == rightRes =>
//						/* The unification below type checks only because we cast both lefTItems and rightItems
//						 * to Lift[V/X, Any]. This would be dangerous in general, because we might deeper down assign
//						 * an incompatible SQLForm to a term (among other problems). We require thus that the lifts
//						 * applied to the whole selects are equal (essentially implying their being identities)
//						 * as a sort of guarantee that the value types of the first pair are compatible
//						 * (because Lift is invariant). This isn't much of a restriction, as there is no reason
//						 * for anyone to create an expression with Rows value type which isn't a select
//						 * and we don't lift anything to Rows automatically.
//						 */
//						val (leftClause, rightClause) =
//							reform(selectClause, e.selectClause)(leftItems vs rightItems, spelling.inSelect)
//						val left = (leftClause selectFrom from) to leftRes
//						val right = (rightClause selectFrom e.from) to rightRes
//						fallbackGuard(
//							"Reformed select clauses " + leftClause + " and " + rightClause +
//								" changed column counts after converting with (" + leftRes + ", " +
//								rightRes + "): (" + left + ", " + right + "). This is a bug."
//						)
//					case (Got((_, leftRes)), Got((_, rightRes))) =>
//						fallbackGuard(
//							s"incompatible Lift types ($leftResult, $rightResult) ending with ($leftRes, $rightRes)."
//						)
//					case (Got((_, _)), _) =>
//						fallbackGuard(
//							"unsupported type conversion for Rows of the right expression: " + rightResult + "."
//						)
//					case (_, Got((_, _))) =>
//						fallbackGuard(
//							"unsupported type conversion for Rows of the left expression: " + leftResult  + "."
//						)
//					case _ =>
//						fallbackGuard(
//							s"unsupported type conversions for a SELECT pair: ($leftResult, $rightResult)."
//						)
//				}
//			}
//
//	}


	/** Splits a transformation for a SelectSQL into a conversion for its individual rows
	  * and one applied to the whole reformed select.
	  */
	private def splitRowsTransformation[X, U](conversion :SQLTransformation[Rows[X], U])
			:Opt[(SQLConversion[X, A], SQLTransformation[Rows[A], U]#Into[conversion.SQLResult]) forSome { type A }] =
	{
		type Result[A] = SQLTransformation[A, U]#Into[conversion.SQLResult]
		type Composed[A] = (SQLTransformation[Rows[X], A], Result[A])
		conversion match {
			case ident if ident.isIdentity =>
				Got((SQLConversion.toSelf[X], conversion))
			case rows :ConvertRows[X, y] =>
				Got((rows.item, SQLConversion.toSelf.asInstanceOf[Result[y]]))
			case _ =>
				SQLTransformation.Composition.unapply(conversion) match {
					case composed :Opt[Composed[y]] if composed.isDefined =>
						type SplitFirst[A] = (SQLConversion[X, A], SQLTransformation[Rows[A], y])
						splitRowsTransformation(composed.get._1) match {
							case split_1 :Opt[SplitFirst[a]] if split_1.isDefined && split_1.get._2.isIdentity =>
								type SplitSecond[A] = (SQLConversion[a, A], Result[Rows[A]])
								splitRowsTransformation(composed.get._2.castParam1[Rows[a]]) match {
									case split_2 :Opt[SplitSecond[b]] if split_2.isDefined =>
										Got((split_1.get._1 andThen split_2.get._1, split_2.get._2))
									case _ => Lack
								}
							case split :Opt[SplitFirst[a]] if split.isDefined =>
								Got((split.get._1, split.get._2 andThen composed.get._2))
							case _ => Lack
						}
					case _ => Lack
				}
		}
	}
//		lift match {
//			case seq :RowsLift[_, _] =>
//				Got((seq.item, Lift .self).asInstanceOf[(Lift[X, Any], Lift[Rows[Any], U])])
//			case _ if lift.isIdentity => //possible really only at the start of recursion
//				Got((Lift.self, Lift.self).asInstanceOf[(Lift[X, Any], Lift[Rows[Any], U])])
//			case _ if lift == Lift.selectRow || lift == Lift.selectRows =>
//				Got((Lift.self, lift).asInstanceOf[(Lift[X, Any], Lift[Rows[Any], U])])
//			case composed :ComposedLift[Rows[X], o, U] =>
//				splitRowsLift[X, o](composed.first) match {
//					case Got((item, rows)) if rows.isIdentity => //composed.second <:< Lift[Rows[A], U]
//						splitRowsLift(composed.second.asInstanceOf[Lift[Rows[Any], U]]) match {
//							case Got((secondItem, rows)) =>
//									Got((item andThen secondItem, rows))
//							case _ => Lack
//						}
//					case Got((item, rows)) =>
//						Got((item, rows andThen composed.second))
//					case _ => Lack
//				}
//			case _ => Lack //unknown Lift whose application would create a ConversionSQL wrapping the select
//		}
//
//	private def splitLast[X, U](lift :Lift[X, U]) :(Lift[X, Any], Lift[Any, U]) =
//		lift match {
//			case composed :ComposedLift[X, y, U] =>
//				val (first, second) = splitLast(composed.second)
//				(composed.first andThen first, second).asInstanceOf[(Lift[X, Any], Lift[Any, U])]
//			case _ =>
//				(Lift.self, lift).asInstanceOf[(Lift[X, Any], Lift[Any, U])]
//		}


/*
	protected override def reform[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
	                                     (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		reform(this, second)

	protected override def reform_:[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
	                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
	{   //technically, left reforming should be already prohibited by `first` when making this callback, but better safe than sorry
		val reformRight = reform.prohibitReformLeft
		val reformed = reformRight[Nothing, Grouped, R, From, Grouped, R, R](first.selectClause, selectClause)._2
		if (reformed eq selectClause)
			(first, this)
		else
			(first, selectOther(reformed))
	}

	protected override def reform_:[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
	                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
	{
		val (left, right) = reform(first.selectClause, selectClause)
		val leftSelect = if (left eq first.selectClause) first else first.selectOther(left)
		val rightSelect = if (right eq selectClause) this else selectOther(right)
		(leftSelect, rightSelect)
	}
*/
	//this is where we potentially obtain an id column
	protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :SelectSQL[F, R] =
		reform.default(this)


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.inSelect.sqlParamCount(selectClause) + spelling.sqlParamCount(from)

//	override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//	                    (matcher :AnyExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.select(this)

	override def isomorphic(expression: SQLExpression.__): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.selectClause isomorphic selectClause) && (s.from == from)
		case _ => false
	}

	private[oldsql] override def equivalent(expression: SQLExpression.__): Boolean = expression match {
		case s :SelectSQL[_, _] =>
			(s eq this) || (s canEqual this) && (s.selectClause equivalent selectClause) && (s.from == from)
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

}






object SelectSQL {
	//anchoring of the select clause expression isn't necessary in theory, but its readForm is the rowForm of the result
	//todo: order by
	//todo: limit, offset
	//todo: mapping indexed headers

	//document that some expressions aren't allowed
	//todo: allow any SQLExpression
	//todo: allow any MappingSQL, or at least every standard implementation, including conversions
	def apply[F <: GroundRow { type Complete <: F }, V]
	         (from :F, selectClause :SQLExpression[F, Grouped, V]) :TopSelectSQL[V] =
		new ArbitraryTopSelect[F, V](from, selectClause.anchor(from), false) with RowShapeCache

	def apply[F <: GroundRow { type Complete <: F }, V](from :F, column :ColumnSQL[F, Grouped, V]) :TopSelectColumn[V] =
		new ArbitraryTopSelectColumn(from, column.anchor(from), false)

	def apply[F <: GroundRow { type Complete <: F }, M[A] <: BaseMapping[V, A], V]
	         (from :F, selectClause :ComponentSQL[F, M]) :TopSelectAs[M] =
		new SelectComponent[F, M, V](from, selectClause.anchor(from), false) with RowShapeCache

	def apply[F <: GroundRow { type Complete <: F }, M[A] <: BaseColumn[V, A], V]
	         (from :F, selectClause :GenericColumnComponentSQL[F, M, V]) :TopSelectColumnAs[M, V] =
		new SelectComponentColumn[F, M, V](from, selectClause.anchor(from), false)

	def apply[F <: GroundRow { type Complete <: F }, V <: Listing]
	         (from :F, selectClause :LabeledSQL[F, Grouped, V]) :TopSelectAs[IndexedMapping.of[V]#Mapping] =
		new TopIndexedSelect(from, selectClause.anchor(from), false) with RowShapeCache

	def apply[F <: GroundRow { type Complete <: F }, A <: Label, V]
	         (from :F, selectClause :LabeledColumnSQL[F, Grouped, A, V])
			:TopSelectColumnAs[IndexedMapping.of[V]#Column, V] =
		new TopSelectIndexedColumn(from, selectClause.anchor(from), false)

//	def apply[F <: GroundRow { type Complete <: F }, X, Y]
//	         (from :F, selectClause :ConversionSQL[F, Grouped, X, Y]) :TopSelectSQL[Y] =
//		new ArbitraryTopSelect[F, Y](from, selectClause.anchor(from), false)
//
//	def apply[F <: GroundRow { type Complete <: F }, M[A] <: BaseMapping[S, A], S]
//	         (from :F, selectClause :EditedLValueSQL[F, M, S]) :TopSelectSQL[S] =
//		new ArbitraryTopSelect[F, S](from, selectClause.anchor(from), false)
//
//	def apply[F <: GroundRow { type Complete <: F }, M[A] <: BaseMapping[S, A], S]
//	         (from :F, selectClause :EditedLooseComponent[F, M, S]) :TopSelectSQL[S] =
//		new ArbitraryTopSelect[F, S](from, selectClause.anchor(from), false)
//
//	def apply[F <: GroundRow { type Complete <: F }, M[A] <: BaseMapping[S, A], S]
//	         (from :F, selectClause :EditedComponentSQL[F, M, S]) :TopSelectSQL[S] =
//		new ArbitraryTopSelect[F, S](from, selectClause.anchor(from), false)
//
//	def apply[F <: GroundRow { type Complete <: F }, V](from :F, selectClause :InlineSQL[F, Grouped, V]) :TopSelectSQL[V] =
//		new ArbitraryTopSelect[F, V](from, selectClause.anchor(from), false)


	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], V]
	             (from :S, selectClause :SQLExpression[S, Grouped, V]) :SubselectSQL[F, V] =
		new ArbitrarySubselect[F, S, V](from, selectClause.anchor(from), false)

	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], V]
	             (from :S, column :ColumnSQL[S, Grouped, V]) :SubselectColumn[F, V] =
		new ArbitrarySubselectColumn[F, S, V](from, column.anchor(from), false)

	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], M[A] <: BaseMapping[V, A], V]
	             (from :S, selectClause :ComponentSQL[S, M]) :SubselectAs[F, M] =
		 new SubselectComponent[F, S, M, V](from, selectClause.anchor(from), false)
	//consider: moving column methods to SelectColumn
	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], M[A] <: BaseColumn[V, A], V]
	             (from :S, column :GenericColumnComponentSQL[S, M, V]) :SubselectColumnAs[F, M, V] =
		new SubselectComponentColumn[F, S, M, V](from, column.anchor(from), false)

	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], V <: Listing]
	             (from :S, selectClause :LabeledSQL[S, Grouped, V]) :SubselectAs[F, IndexedMapping.of[V]#Mapping] =
		new IndexedSubselect(from, selectClause.anchor(from), false)

	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], A <: Label, V]
	             (from :S, selectClause :LabeledColumnSQL[S, Grouped, A, V])
			:SubselectColumnAs[F, IndexedMapping.of[V]#Column, V] =
		new SubselectIndexedColumn(from, selectClause.anchor(from), false)

//	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], V]
//	             (from :S, selectClause :InlineSQL[S, Grouped, V]) :SubselectSQL[F, V] =
//		new ArbitrarySubselect[F, S, V](from, selectClause.anchor(from), false)

//	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], X, Y]
//	             (from :S, selectClause :ConversionSQL[S, Grouped, X, Y]) :SubselectSQL[F, Y] =
//		new ArbitrarySubselect[F, S, Y](from, selectClause.anchor(from), false)
//
//	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], M[A] <: BaseMapping[V, A], V]
//	             (from :S, selectClause :EditedLooseComponent[S, M, V]) :SubselectSQL[F, V] =
//		new ArbitrarySubselect[F, S, V](from, selectClause.anchor(from), false)
//
//	def subselect[F <: NonEmptyRow, S <: SubselectOf[F], M[A] <: BaseMapping[V, A], V]
//	             (from :S, selectClause :EditedComponentSQL[S, M, V]) :SubselectSQL[F, V] =
//		new ArbitrarySubselect[F, S, V](from, selectClause.anchor(from), false)


	type __ = SelectSQL[_ <: RowProduct, _]

	type GroundSelectSQL[V] = SelectSQL[RowProduct, V] //todo: rename to OuterSelectSQL
	type GroundSelectColumn[V] = SelectColumn[RowProduct, V]
	type GroundSelectAs[H[A] <: MappingAt[A]] = SelectAs[RowProduct, H]
	type GroundSelectColumnAs[H[A] <: BaseColumn[V, A], V] = SelectColumnAs[RowProduct, H, V]


//	trait SelectSQLTemplate[-F <: RowProduct, R, +Q[f <: RowProduct, v] <: SelectSQL[f, v]]
//		extends SingleSQLTemplate[F, GlobalScope, Rows[R], ({ type E[f <: RowProduct] = Q[f, R] })#E]
//		   with SelectTemplate[F, ({ type E[v] = Q[F, v] })#E]
//	{ this :Q[F, R] =>
//		override def distinct :Q[F, R]
//	}



//OuterSelect
	/** Base trait for SQL select expressions where the ''select'' clause depends solely on the explicit ''from'' clause
	  * of the ''select'', i.e. it is not dependent on any outside rows. Such an expression is also a valid independent
	  * select statement in opposition to subselect expressions.
	  */ //todo: move to sql/sql.ast/sql.Select; rename to GroundSelect
	trait TopSelectSQL[R]
		extends SelectSQL[RowProduct, R] with Select[@~, R]
		   with GroundSQLTemplate[Rows[R], TopSelectSQL[R]] with SelectTemplate[R, TopSelectSQL]
		   with QueryReformingTemplate[R, TopSelectSQL]
	{
//		override def rowForm :SQLReadForm[V] = selectClause.selectForm
//		override type Domain >: From <: RowProduct //not GroundRow, because it would exclude Dual and NoParams
		override type From <: GroundRow { type Complete <: TopSelectSQL.this.From }
		override val selectClause :SQLExpression[From, Grouped, R]
		override def columns      :Seq[TypedColumnSQLMapping[From, Grouped, _, this.type]]
		//conflict between QuerySQL and Query
//		override def parameterization :Parameterization[@~, From] = Parameterization.paramless[From]

		//inherited overloaded operators from QuerySQL and Query lead to a lack of clear choice for another TopSelectSQL
//		def union(other :TopSelectSQL[V])     :TopSelectSQL[V] = Select.Union(this, other)
//		def unionAll(other :TopSelectSQL[V])  :TopSelectSQL[V] = Select.UnionAll(this, other)
//		def minus(other :TopSelectSQL[V])     :TopSelectSQL[V] = Select.Minus(this, other)
//		def intersect(other :TopSelectSQL[V]) :TopSelectSQL[V] = Select.Intersect(this, other)

		override def transform[X](transformation :SQLTransformation[R, X]) :TopSelectSQL[X] =
			transformation(selectClause).topSelectFrom(from)

		override def rowsTo[X](implicit conversion :SQLConversion[R, X]) :TopSelectSQL[X] =
			if (conversion.isIdentity) this.castParam[X]
			else selectOther(conversion(selectClause))//new ArbitraryTopSelect[From, X](from, conversion(selectClause), isDistinct)

//		override def map[X](f :R => X) :TopSelectSQL[X] = selectOther(selectClause.map(f))
//			new ArbitraryTopSelect[From, X](from, selectClause.map(f), isDistinct)
//
//		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C)
//				:TopSelectSQL[X] =
//			map(applyFun(f))

		override def selectOther[X](selectClause :SQLExpression[From, Grouped, X]) :TopSelectSQL[X] =
			if (isDistinct) (selectClause topSelectFrom from).distinct
			else selectClause topSelectFrom from

		override def bind(params: @~) :TopSelectSQL[R] = this

		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :TopSelectSQL[R] =
			reform.default(this)

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			super[SelectSQL].shape

		protected override def columnCount(implicit spelling :SQLSpelling) :Int =
			super[SelectSQL].columnCount

		protected override def defaultSpelling[P](from :RowProduct, context :SQLContext[P],
		                                          params :Parameterization[P, RowProduct])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			def completeParams(from :RowProduct) :Parameterization[from.Params, from.Complete] = from.parameterization
			defaultSpelling(this.from, context.adapted[this.from.Params])(
				completeParams(this.from), this.from.parameterization
			) compose { _ :P => @~ }
		}


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[RowProduct, Y]) :Y[Single, Rows[R]] =
			visitor.topSelect(this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, Rows[R], Y]) :Y =
			visitor.topSelect(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: TopSelectSQL[R] <: SQLExpression[F_, S_, Rows[R]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, Rows[R], E] =
//			visitor.topSelect(this)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TopSelectSQL[_]]
	}




	/** A base trait for all SQL select expressions nested under another SQL select.
	  * @tparam F the ''from'' clause of the outer select, forming a prefix of `S` until the last occurrence
	  *           of a `Subselect` join kind.
	  * @tparam R the type of the scala value selected by this subselect.
	  */ //consider: relations for subselects
	trait SubselectSQL[-F <: RowProduct, R]
		extends SelectSQL[F, R]
		   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = SubselectSQL[f, R] })#Q]
		   with SelectTemplate[R, ({ type Q[r] = SubselectSQL[F, r] })#Q]
	{ //consider: why doesn't it extend SelectTemplate?
//		override def distinct :SubselectSQL[F, R]

		override def transform[X](transformation :SQLTransformation[R, X]) :SubselectSQL[F, X] =
			selectOther(transformation(selectClause))

		override def rowsTo[X](implicit conversion :SQLConversion[R, X]) :SubselectSQL[F, X] =
			if (conversion.isIdentity) this.asInstanceOf[SubselectSQL[F, X]]
			else selectOther(conversion(selectClause))
//			else new ArbitrarySubselect[F, From, X](from, conversion(selectClause), isDistinct)

		override def selectOther[X](selectClause :SQLExpression[From, Grouped, X]) :SubselectSQL[F, X] =
			if (isDistinct) (selectClause subselectFrom from).distinct
			else selectClause subselectFrom from

//		override def map[X](f :R => X) :SubselectSQL[F, X] =
//			new ArbitrarySubselect[F, From, X](from, selectClause.map(f), isDistinct)
//
//		override def map[Fun, C <: Chain, X]
//		                (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C)
//				:SubselectSQL[F, X] =
//			map(applyFun(f))

		//unnecessary with SelectSQL return type, because selectFrom in super will create a subselect.
//		override def copy(selectClause :SQLExpression[From, Grouped, V]) :SelectSQL[F, V] =
//			if (isDistinct)
//				(selectClause subselectFrom[] from).distinct
//			else
//				selectClause subselectFrom from
//			new ArbitrarySubselect[F, From, V](from, selectClause, isDistinct)

//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectSQL[E, R] =
//			expand(base)(ext.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:SubselectSQL[E, R]

		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			if (from.isExplicitParameterized)
				throw Bug(
					"Cannot spell a dependent select with unbound parameters in its (explicit) from clause: " + this + "."
				)
			else if (from == this.from.outer) {
//				val fromParams = params.subselect(this.from.self)
				val fromParams = params.subselect(this.from).asInstanceOf[Parameterization[P, this.from.Self]] //todo: in Scala3 replace with above
				defaultSpelling(this.from, context)(params.subselect(this.from), fromParams)
			} else from match {
				case outer :NonEmptyRow with F @unchecked =>
					val subselect = this.from.asSubselectOf(outer)(ExpandedBy.itself[NonEmptyRow with F]).asInstanceOf[From]
//					val fromParams = params.subselect(subselect.self) //todo: eliminate the casts in Scala 3
					val fromParams = params.subselect(subselect).asInstanceOf[Parameterization[P, subselect.Self]]
					defaultSpelling(subselect, context)(params.subselect(this.from), fromParams)
				case _ =>
					throw Bug(
						"From clause " + from + " given to spell '" + this + "' is not a NonEmptyRow."
					)
			}


		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, Rows[R]] =
			visitor.subselect(this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, Rows[R], Y]) :Y =
			visitor.subselect(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//		                             E >: SubselectSQL[F_, R] <: SQLExpression[F_, S_, Rows[R]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, Rows[R], E] =
//			visitor.subselect(this)
	}







	private abstract class BaseSelectComponent[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
	                                          (override val from :S, override val selectClause :ComponentSQL[S, H])
		extends SelectSQL[F, V] with SelectAs[F, H]
	{
		override type From = S

		override val columns: Seq[SelectedColumn[_]] =
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :TypedColumn[X, selectClause.Origin]) :SelectedColumn[X] =
			TypedColumnSQLMapping(selectClause \ column)

		override def mapping[O] :RowMapping[O] = selectClause.mapping.withOrigin[O]
		override def export[O] :TypedMapping[V, O] = selectClause.anchored.withOrigin[O]

		override val withClause = selectClause.outerWithClause ++ from.withClause
	}


	private class SelectComponent[F <: GroundRow { type Complete <: F }, H[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val selectClause :ComponentSQL[F, H],
	                              override val isDistinct :Boolean)
		extends BaseSelectComponent[RowProduct, F, H, V](from, selectClause) with BaseTopSelectAs[F, H, V]
	{
		override def distinct :BaseTopSelectAs[F, H, V] =
			if (isDistinct) this else new SelectComponent(from, selectClause, true)
	}


	private class SubselectComponent[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
	                                (subselect :S, component :ComponentSQL[S, H], override val isDistinct :Boolean)
		extends BaseSelectComponent[F, S, H, V](subselect, component)
		   with BaseSubselectAs[F, S, H, V]
	{
		override def distinct :BaseSubselectAs[F, S, H, V] =
			if (isDistinct) this else new SubselectComponent(from, selectClause, true)

		override def anchor(from :F) :SelectAs[F, H] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome])(ExpandedBy.itself[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h == selectClause) this else new SubselectComponent[F, S, H, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectAs[E, H] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyRow =>
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the expansion clause G
					type Rel[O <: RowProduct] = JoinedRelation[O, selectClause.Entity] { type FromLast = selectClause.FromLast }
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val offset = selectClause.origin.index
					val replacement =
						if (offset < subselectTables)
							selectClause.asInstanceOf[ComponentSQL[stretched.type, H]]
						else
							selectClause.moveTo(
								RelationOffset.unsafe[stretched.type, selectClause.FromLast, Rel, selectClause.Entity](
									offset + expansion.length
								)
							)
					new SubselectComponent[E, stretched.type, H, V](
						stretched, replacement, isDistinct
					)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					val adaptedHeader = selectClause.asInstanceOf[ComponentSQL[Dual, H]]
					new SubselectComponent[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(s"Cannot rebase clause $from :${from.localClassName} onto $base.")
			}

	}




	private class SelectComponentColumn[F <: GroundRow { type Complete <: F }, H[A] <: BaseColumn[V, A], V]
	                                   (clause :F, override val selectClause :GenericColumnComponentSQL[F, H, V],
	                                    override val isDistinct :Boolean)
		extends SelectComponent[F, H, V](clause, selectClause, isDistinct) with BaseTopSelectColumnAs[F, H, V]
		   with RowShapeCache
	{
		override def export[O] :TypedColumn[V, O] = selectClause.anchored.withOrigin[O]

		override def distinct :BaseTopSelectColumnAs[F, H, V] =
			if (isDistinct) this else new SelectComponentColumn(from, selectClause, true)
	}


	private class SubselectComponentColumn[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseColumn[V, A], V]
	              (override val from :S, override val selectClause :GenericColumnComponentSQL[S, H, V],
	               override val isDistinct :Boolean)
		extends SubselectComponent[F, S, H, V](from, selectClause, isDistinct)
		   with BaseSubselectColumnAs[F, S, H, V]
	{
		override def export[O] :TypedColumn[V, O] = selectClause.anchored.withOrigin[O]

		override def distinct :BaseSubselectColumnAs[F, S, H, V] =
			if (isDistinct) this else new SubselectComponentColumn(from, selectClause, true)

		override def anchor(from :F) :SelectColumnAs[F, H, V] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h == selectClause) this else new SubselectComponentColumn[F, S, H, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectColumnAs[E, H, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyRow => //todo: refactor this together with SubselectComponent; if S <: NonEmptyRow, than the casting could conceivably by omitted
					type Ext = SubselectOf[E] //pretend this is the actual type S after rebasing to the expansion clause G
					type Rel[O <: RowProduct] = JoinedRelation[O, selectClause.Entity] { type FromLast = selectClause.FromLast }
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val subselectTables = stretched.fullSize - base.fullSize
					val offset = selectClause.origin.index
					val replacement =
						if (offset < subselectTables)
							selectClause.asInstanceOf[GenericColumnComponentSQL[stretched.type, H, V]]
						else
							selectClause.moveTo(
								RelationOffset.unsafe[stretched.type, selectClause.FromLast, Rel, selectClause.Entity](
									offset + expansion.length
								)
							)
					new SubselectComponentColumn[E, stretched.type, H, V](stretched, replacement, isDistinct)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					val adaptedHeader = selectClause.asInstanceOf[GenericColumnComponentSQL[Dual, H, V]]
					new SubselectComponentColumn[E, Dual, H, V](empty, adaptedHeader, isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}




	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */ //todo: check if adding/removing public inner classes violates binary compatibility and, if so, move it to ast
	private[ast] abstract class ArbitrarySelect[-F <: RowProduct, S <: SubselectOf[F], V] protected
	                            (override val from :S, protected val result :TypedSQLMapping[S, Grouped, V, Unit])
		extends SelectSQL[F, V]
	{
		def this(from :S, header :SQLExpression[S, Grouped, V]) =
			this(from, TypedSQLMapping[S, Grouped, V, Unit](header, SelectView))

		override type From = S

		override val selectClause :SQLExpression[S, Grouped, V] = result.expr
		override val withClause   = selectClause.outerWithClause ++ from.withClause
		override val columns :Seq[TypedColumnSQLMapping[S, Grouped, _, this.type]] =
			result.columns.toSeq.withOrigin[this.type]
	}


	private[ast] trait ArbitrarySelectTemplate[-F <: RowProduct, S <: SubselectOf[F], M[O] <: BaseMapping[V, O], V]
		extends SelectSQL[F, V]
	{ this :ArbitrarySelect[F, S, V] =>
		override type RowMapping[O] = M[O]

		protected val result :M[Unit]
		override def mapping[O] :M[O] = (this :ArbitrarySelectTemplate[F, S, M, V]).result.withOrigin[O]
		override def export[O] :TypedMapping[V, O] = mapping[O]
//		override def export[O] :M[O] = mapping[O]
	}


	private class ArbitraryTopSelect[F <: GroundRow { type Complete <: F }, V]
	              (override val from :F, override val selectClause :GroupedSQL[F, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[RowProduct, F, V](from, selectClause)
		   with ArbitrarySelectTemplate[RowProduct, F, TypedSQLMapping.c[F]#c[Grouped]#c[V]#project, V]
		   with TopSelectSQL[V]
	{
		override def distinct :TopSelectSQL[V] =
			if (isDistinct) this else new ArbitraryTopSelect(from, selectClause, true)
	}


	private class ArbitrarySubselect[-F <: RowProduct, S <: SubselectOf[F], V]
	              (subclause :S, select :GroupedSQL[S, V], override val isDistinct :Boolean)
		extends ArbitrarySelect[F, S, V](subclause, select)
		   with ArbitrarySelectTemplate[F, S, TypedSQLMapping.c[S]#c[Grouped]#c[V]#project, V]
		   with SubselectSQL[F, V]
	{
		override def distinct :SubselectSQL[F, V] =
			if (isDistinct) this else new ArbitrarySubselect(from, selectClause, true)

		override def anchor(from :F) :SubselectSQL[F, V] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h == selectClause) this else new ArbitrarySubselect[F, S, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectSQL[E, V] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyRow =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[from.type, stretched.type](from, stretched)
					new ArbitrarySubselect[E, stretched.type, V](
						stretched, substitute(selectClause), isDistinct
					)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					new ArbitrarySubselect[E, Dual, V](empty, selectClause.asInstanceOf[GroupedSQL[Dual, V]], isDistinct)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}




	private[ast] abstract
	class ArbitrarySelectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	                           (override val from :S, override val result :TypedColumnSQLMapping[S, Grouped, V, Unit])
		extends ArbitrarySelect[F, S, V](from, result)
		   with ArbitrarySelectTemplate[F, S, TypedColumnSQLMapping.c[S]#c[Grouped]#c[V]#project, V]
		   with SelectColumn[F, V]
	{
		def this(from :S, expression :ColumnSQL[S, Grouped, V]) =
			this(from, TypedColumnSQLMapping[S, Grouped, V, Unit](expression))

		override val selectClause :GroupedColumn[S, V] = result.expr
		override def export[O]    :TypedColumn[V, O] = mapping[O]
	}


	private[ast] class ArbitraryTopSelectColumn[F <: GroundRow { type Complete <: F }, V]
	                   (override val from :F, override val selectClause :GroupedColumn[F, V],
	                    override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[RowProduct, F, V](from, selectClause) with TopSelectColumn[V]
		   with RowShapeCache
	{
		override def distinct :TopSelectColumn[V] =
			if (isDistinct) this else new ArbitraryTopSelectColumn(from, selectClause, true)
	}


	private[ast] class ArbitrarySubselectColumn[-F <: RowProduct, S <: SubselectOf[F], V]
	                   (clause :S, override val selectClause :ColumnSQL[S, Grouped, V],
	                    override val isDistinct :Boolean)
		extends ArbitrarySelectColumn[F, S, V](clause, selectClause) with SubselectColumn[F, V]
	{
		override def distinct :SubselectColumn[F, V] =
			if (isDistinct) this else new ArbitrarySubselectColumn(from, selectClause, true)

		override def anchor(from :F) :SubselectColumn[F, V] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h == selectClause) this else new ArbitrarySubselectColumn[F, S, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectColumn[E, V] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, stretched.type](from, stretched)
					new ArbitrarySubselectColumn[E, stretched.type, V](
						stretched, substitute(selectClause), isDistinct
					)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					new ArbitrarySubselectColumn[E, Dual, V](
						empty, selectClause.asInstanceOf[GroupedColumn[Dual, V]], isDistinct
					)
			}
	}




	private abstract class IndexedSelect[-F <: RowProduct, S <: SubselectOf[F], V]
	                       (from :S, override val result :TypedListingSQLMapping[S, Grouped, V, Unit])
		extends ArbitrarySelect[F, S, V](from, result)
		   with ArbitrarySelectTemplate[F, S, IndexedMapping.of[V]#Mapping, V]
	{
		def this(from :S, expression :LabeledValueSQL[S, Grouped, V]) =
			this(from, expression.mapping[Unit])

		override val selectClause = result.expr
	}


	private class TopIndexedSelect[F <: GroundRow { type Complete <: F }, V]
	              (clause :F, select :LabeledValueSQL[F, Grouped, V], override val isDistinct :Boolean)
		extends IndexedSelect[RowProduct, F, V](clause, select)
		   with TopSelectAs[IndexedMapping.of[V]#Mapping]
	{
		override def distinct :TopSelectAs[IndexedMapping.of[V]#Mapping] =
			if (isDistinct) this else new TopIndexedSelect(from, selectClause, true)
	}


	private class IndexedSubselect[-F <: RowProduct, S <: SubselectOf[F], V]
	              (subclause :S, select :LabeledValueSQL[S, Grouped, V], override val isDistinct :Boolean)
		extends IndexedSelect[F, S, V](subclause, select)
		   with SubselectAs[F, IndexedMapping.of[V]#Mapping]
	{
		override def distinct :SubselectAs[F, IndexedMapping.of[V]#Mapping] =
			if (isDistinct) this else new IndexedSubselect(from, selectClause, true)

		override def anchor(from :F) :SubselectAs[F, IndexedMapping.of[V]#Mapping] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h eq selectClause) this else new IndexedSubselect[F, S, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectAs[E, IndexedMapping.of[V]#Mapping] =
			from match { //would be safer to refactor this out as a RowProduct method
				case some :NonEmptyRow =>
					type Ext = SubselectOf[E] //RowProduct { type Implicit = G }
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, Ext](from, stretched)
					val indexed = substitute(selectClause).asInstanceOf[LabeledValueSQL[Ext, Grouped, V]]
					new IndexedSubselect[E, stretched.type, V](stretched, indexed, isDistinct)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					new IndexedSubselect[E, Dual, V](
						empty, selectClause.asInstanceOf[LabeledValueSQL[Dual, Grouped, V]], isDistinct
					)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}




	private abstract class SelectIndexedColumn[-F <: RowProduct, S <: SubselectOf[F], A <: Label, V]
	                                          (override val from :S,
	                                           override val result :TypedListingColumnSQLMapping[S, Grouped, A, V, Unit])
		extends ArbitrarySelect[F, S, V](from, result)
		   with ArbitrarySelectTemplate[F, S, IndexedMapping.of[V]#Column, V]
		   with SelectColumn[F, V]
	{
		def this(from :S, expression :LabeledColumnSQL[S, Grouped, A, V]) =
			this(from, TypedListingColumnSQLMapping[S, Grouped, A, V, Unit](expression))

		override val selectClause :LabeledColumnSQL[S, Grouped, A, V] = result.expr
		override def export[O]    :TypedColumn[V, O] = mapping[O]
	}


	private class TopSelectIndexedColumn[F <: GroundRow { type Complete <: F }, A <: Label, V]
	              (override val from :F, override val selectClause :LabeledColumnSQL[F, Grouped, A, V],
	               override val isDistinct :Boolean)
		extends SelectIndexedColumn[RowProduct, F, A, V](from, selectClause)
		   with TopSelectColumnAs[IndexedMapping.of[V]#Column, V] with RowShapeCache
	{
		override def distinct :TopSelectColumnAs[IndexedMapping.of[V]#Column, V] =
			if (isDistinct) this else new TopSelectIndexedColumn(from, selectClause, true)
	}


	private class SubselectIndexedColumn[-F <: RowProduct, S <: SubselectOf[F], A <: Label, V]
	                                    (clause :S, override val selectClause :LabeledColumnSQL[S, Grouped, A, V],
	                                     override val isDistinct :Boolean)
		extends SelectIndexedColumn[F, S, A, V](clause, selectClause) with SubselectColumn[F, V]
		   with SubselectColumnAs[F, IndexedMapping.of[V]#Column, V]
	{
		override type RowMapping[O] = IndexedColumnMapping[V, O]

		override def distinct :SubselectColumnAs[F, IndexedMapping.of[V]#Column, V] =
			if (isDistinct) this else new SubselectIndexedColumn(from, selectClause, true)

		override def anchor(from :F) :SubselectColumnAs[F, IndexedMapping.of[V]#Column, V] =
			if (this.from.outer == from)
				this
			else {
				val rebase = this.from.asSubselectOf(from.asInstanceOf[F with FromSome]).asInstanceOf[S]
				val h = selectClause.anchor(rebase)
				if (h == selectClause) this else new SubselectIndexedColumn[F, S, A, V](rebase, h, isDistinct)
			}

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
				:SubselectColumnAs[E, IndexedMapping.of[V]#Column, V] =
			from match {
				case some :FromSome =>
					type Ext = SubselectOf[E]
					implicit val expansion = expansion.asInstanceOf[some.Implicit ExpandedBy base.Generalized]
					val stretched = base.fromSubselect(some).asInstanceOf[Ext]
					val substitute = SQLScribe.shiftBack[S, stretched.type](from, stretched)
					val shift = selectClause.alias @: substitute(selectClause.value) :LabeledColumnSQL[stretched.type, Grouped, A, V]
					new SubselectIndexedColumn[E, stretched.type, A, V](
						stretched, shift, isDistinct
					)

				case empty :Dual => //this shouldn't happen, but lets play it safe
					new SubselectIndexedColumn[E, Dual, A, V](
						empty, selectClause.asInstanceOf[LabeledColumnSQL[Dual, Grouped, A, V]], isDistinct
					)

				case _ =>
					throw new UnsupportedOperationException(
						s"Cannot rebase clause $from :${from.localClassName} onto $base."
					)
			}
	}






	trait SpecificTopSelectVisitor[+F <: RowProduct, X, +Y]
		extends SpecificTopSelectColumnVisitor[F, X, Y] with SpecificTopSelectAsVisitor[F, X, Y]
	{
		def topSelect[R](e :TopSelectSQL[R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificTopSelect[+F <: RowProduct, X, +Y]
		extends SpecificTopSelectVisitor[F, X, Y] with CaseSpecificTopSelectAs[F, X, Y]
	{
		override def topSelectColumn[R](e :TopSelectColumn[R])(implicit isRows :X =:= Rows[R]) :Y = topSelect(e)
	}
	trait CaseSpecificTopSelect[+F <: RowProduct, X, +Y] extends MatchSpecificTopSelect[F, X, Y] {
		override def topSelectAs[M[O] <: MappingAt[O]]
		                        (e :TopSelectAs[M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y = topSelect(e)
	}

	trait SpecificSubselectVisitor[+F <: RowProduct, X, +Y]
		extends SpecificSubselectColumnVisitor[F, X, Y] with SpecificSubselectAsVisitor[F, X, Y]
	{
		def subselect[R](e :SubselectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificSubselect[+F <: RowProduct, X, +Y]
		extends SpecificSubselectVisitor[F, X, Y] with CaseSpecificSubselectAs[F, X, Y]
	{
		override def subselectColumn[R](e :SubselectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y = subselect(e)
	}
	trait CaseSpecificSubselect[+F <: RowProduct, X, +Y] extends MatchSpecificSubselect[F, X, Y] {
		override def subselectAs[M[O] <: MappingAt[O]]
		                        (e :SubselectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y = subselect(e)
	}

	trait SpecificSelectVisitor[+F <: RowProduct, X, +Y] extends SpecificSelectColumnVisitor[F, X, Y]
		with SpecificTopSelectVisitor[F, X, Y] with SpecificSubselectVisitor[F, X, Y] with SpecificSelectAsVisitor[F, X, Y]
	{
		def select[R](e :SelectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificSelect[+F <: RowProduct, X, +Y] extends SpecificSelectVisitor[F, X, Y]
		with CaseSpecificTopSelect[F, X, Y] with CaseSpecificSubselect[F, X, Y] with MatchSpecificSelectAs[F, X, Y]
	{
		override def selectColumn[R](e :SelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y = select(e)
		override def selectAs[H[O] <: MappingAt[O]](e :SelectAs[F, H])(implicit isRows :X =:= Rows[H[Unit]#Subject]) :Y =
			select[H[Unit]#Subject](e)
	}
	trait CaseSpecificSelect[+F <: RowProduct, X, +Y] extends MatchSpecificSelect[F, X, Y] {
		override def subselect[R](e: SubselectSQL[F, R])(implicit isRows :X =:= Rows[R]): Y = select(e)
		override def topSelect[R](e: TopSelectSQL[R])(implicit isRows :X =:= Rows[R]): Y = select(e)
	}
//
//
//
//	trait TopSelectVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends TopSelectColumnVisitor[Y] with TopSelectAsVisitor[Y]
//	{
//		def topSelect[R](e :TopSelectSQL[R]) :Y[Single, Rows[R], TopSelectSQL[R]]
//	}
//	trait MatchTopSelect[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends TopSelectVisitor[Y] with CaseTopSelectAs[Y]
//	{
//		override def topSelectColumn[R](e :TopSelectColumn[R]) :Y[Single, Rows[R], TopSelectColumn[R]] =
//			topSelect(e)
//	}
//	trait CaseTopSelect[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchTopSelect[Y]
//	{
//		override def topSelectAs[M[O] <: MappingAt[O]]
//		                        (e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject], TopSelectAs[M]] =
//			topSelect(e)
//	}
//
//	trait SubselectVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SubselectColumnVisitor[F, Y] with SubselectAsVisitor[F, Y]
//	{
//		def subselect[R](e :SubselectSQL[F, R]) :Y[Single, Rows[R], SubselectSQL[F, R]]
//	}
//	trait MatchSubselect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SubselectVisitor[F, Y] with CaseSubselectAs[F, Y]
//	{
//		override def subselectColumn[R](e :SubselectColumn[F, R]) :Y[Single, Rows[R], SubselectColumn[F, R]] =
//			subselect(e)
//	}
//	trait CaseSubselect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSubselect[F, Y]
//	{
//		override def subselectAs[M[O] <: MappingAt[O]]
//		                        (e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject], SubselectAs[F, M]] =
//			subselect(e)
//	}
//
//	trait SelectVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectColumnVisitor[F, Y] with SelectAsVisitor[F, Y] with TopSelectVisitor[Y] with SubselectVisitor[F, Y]
//	{
//		def select[R](e :SelectSQL[F, R]) :Y[Single, Rows[R], SelectSQL[F, R]]
//	}
//	trait MatchSelect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectVisitor[F, Y] with CaseTopSelect[Y] with CaseSubselect[F, Y] with MatchSelectAs[F, Y]
//	{
//		override def selectColumn[R](e :SelectColumn[F, R]) :Y[Single, Rows[R], SelectColumn[F, R]] =
//			select(e)
//		override def selectAs[M[O] <: MappingAt[O]]
//		                     (e :SelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject], SelectAs[F, M]] =
//			select(e)
//	}
//	trait CaseSelect[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSelect[F, Y]
//	{
//		override def topSelect[R](e :TopSelectSQL[R]) :Y[Single, Rows[R], TopSelectSQL[R]] = select(e)
//		override def subselect[R](e :SubselectSQL[F, R]) :Y[Single, Rows[R], SubselectSQL[F, R]] = select(e)
//	}



	trait AnyTopSelectVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectColumnVisitor[F, Y] with AnyTopSelectAsVisitor[F, Y]
	{
		def topSelect[V](e :TopSelectSQL[V]) :Y[Single, Rows[V]]
	}

	trait MatchAnyTopSelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectVisitor[F, Y] with CaseAnyTopSelectAs[F, Y]
	{
		override def topSelectColumn[V](e :TopSelectColumn[V]) :Y[Single, Rows[V]] = topSelect(e)
	}
	trait CaseAnyTopSelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyTopSelect[F, Y] {
		override def topSelectAs[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject]] =
			topSelect(e)
	}

	trait AnySubselectVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectColumnVisitor[F, Y] with AnySubselectAsVisitor[F, Y]
	{
		def subselect[V](e :SubselectSQL[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnySubselect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectVisitor[F, Y] with CaseAnySubselectAs[F, Y]
	{
		override def subselectColumn[V](e :SubselectColumn[F, V]) :Y[Single, Rows[V]] = subselect(e)
	}
	trait CaseAnySubselect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnySubselect[F, Y] {
		override def subselectAs[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]] =
			subselect(e)
	}

	trait AnySelectVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnySelectColumnVisitor[F, Y]
		with AnyTopSelectVisitor[F, Y] with AnySubselectVisitor[F, Y] with AnySelectAsVisitor[F, Y]
	{
		def select[V](e :SelectSQL[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnySelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnySelectVisitor[F, Y]
		with CaseAnyTopSelect[F, Y] with CaseAnySubselect[F, Y] with MatchAnySelectAs[F, Y]
	{
		override def selectColumn[V](e :SelectColumn[F, V]) :Y[Single, Rows[V]] = select(e)

		override def selectAs[H[O] <: MappingAt[O]](e :SelectAs[F, H]) :Y[Single, Rows[H[Unit]#Subject]] =
			select(e)
	}
	trait CaseAnySelect[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnySelect[F, Y] {
		override def subselect[V](e: SubselectSQL[F, V]): Y[Single, Rows[V]] = select(e)
		override def topSelect[V](e: TopSelectSQL[V]): Y[Single, Rows[V]] = select(e)
	}

}






//This class is problematic: it is a column expression and, as such, selectable and possible to use everywhere.
//At the same time its value type is `Rows[V]` rather than `V`. Is there any need for single column, multi row selects,
//which cannot be satisfied just by `Select`?
trait SelectColumn[-F <: RowProduct, R]
	extends ColumnSingleQuery[F, R] with SelectSQL[F, R]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = SelectColumn[f, R] })#Q]
	   with SelectTemplate[R, ({ type Q[X] = SelectColumn[F, X] })#Q]
{
//	override def export[O]    :TypedColumn[V, O] = mapping[O]
	override def selectForm   :ColumnReadForm[Rows[R]] = selectClause.selectForm.nullMap(Rows.single)
	override def rowForm      :ColumnReadForm[R] = selectClause.selectForm
	override val selectClause :ColumnSQL[From, Grouped, R]
//	override def columns      :Seq[SelectedColumn[_]] = selectClause::Nil
//	override def constituents :Seq[SelectColumn[F, V]] = this::Nil
	override def one          :ColumnSQL[F, Single, R] = to[R]

	def selectOther[X](selectClause :ColumnSQL[From, Grouped, X]) :SelectColumn[F, X] =
		if (isDistinct)
			(selectClause selectFrom from).distinct
		else
			selectClause selectFrom from

//	override def asGlobal :Option[SelectColumn[F, R]] = Some(this)
//	override def anchor(from :F) :SelectColumn[F, R] //= this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :PartOf[U, E]) :SelectColumn[E, R]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SelectColumn[E, R]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]](matcher :AnyColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.select(this)
}






object SelectColumn {

	trait TopSelectColumn[R]
		extends TopSelectSQL[R] with SelectColumn[RowProduct, R]
		   with GroundSQLTemplate[Rows[R], TopSelectColumn[R]]
		   with SelectTemplate[R, TopSelectColumn]
	{
		override def rowForm :ColumnReadForm[R] = selectClause.selectForm //override clash between Select and SelectColumn

//		override def transform[X](conversion :SQLTransformation[R, X]) :TopSelectColumn[X] =
//			selectOther(conversion(selectClause))

		override def rowsTo[X](implicit conversion :SQLConversion[R, X]) :TopSelectColumn[X] =
			if (conversion.isIdentity) this.castParam[X]
			else selectOther(conversion(selectClause)) //new ArbitraryTopSelectColumn[From, X](from, conversion(selectClause), isDistinct)

//		override def map[X](f :R => X) :TopSelectColumn[X] =
//			new ArbitraryTopSelectColumn[From, X](from, selectClause.map(f), isDistinct)
//
//		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C)
//				:TopSelectColumn[X] =
//			map(applyFun(f))

		override def selectOther[X](selectClause :ColumnSQL[From, Grouped, X]) :TopSelectColumn[X] =
			if (isDistinct)
				(selectClause topSelectFrom from).distinct
			else
				selectClause topSelectFrom from

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, Rows[R]] =
			visitor.topSelectColumn(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, Rows[R], Y]) :Y =
			visitor.topSelectColumn(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: TopSelectColumn[R] <: SQLExpression[F_, S_, Rows[R]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, Rows[R], E] =
//			visitor.topSelectColumn(this)
	}

//	type TypedTopSelectColumn[M[O] <: TypedColumn[_, O], V] = TopSelectColumn[V] { type RowMapping[O] <: M[O] }


	trait SubselectColumn[-F <: RowProduct, R]
		extends SubselectSQL[F, R] with SelectColumn[F, R]
		   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = SubselectColumn[f, R] })#Q]
		   with SelectTemplate[F, ({ type Q[v] = SubselectColumn[F, v] })#Q]
	{
//		override def distinct :SubselectColumn[F, V]

		override def rowsTo[X](implicit conversion :SQLConversion[R, X]) :SubselectColumn[F, X] =
			if (conversion.isIdentity) this.asInstanceOf[SubselectColumn[F, X]]
			else selectOther(conversion(selectClause)) //new ArbitrarySubselectColumn[F, From, X](from, conversion(selectClause), isDistinct)

//		override def map[X](f :R => X) :SubselectColumn[F, X] =
//			new ArbitrarySubselectColumn[F, From, X](from, selectClause.map(f), isDistinct)
//
//		override def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C)
//				:SubselectColumn[F, X] =
//			map(applyFun(f))

		override def selectOther[X](e :ColumnSQL[F, Grouped, X]) :SubselectColumn[F, X] =
			if (isDistinct) (e subselectFrom from).distinct
			else e subselectFrom from
//		override def asGlobal :Option[SubselectColumn[F, R]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :SubselectColumn[E, R] =
			expand(base)(expansion.asExpandedBy, implicitly[Single <:< Single])

//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:SubselectColumn[E, R]

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, Rows[R]] =
			visitor.subselectColumn(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, Rows[R], Y]) :Y =
			visitor.subselectColumn(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//		                             E >: SubselectColumn[F_, R] <: SQLExpression[F_, S_, Rows[R]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, Y]) :Y[S_, Rows[R], E] =
//			visitor.subselectColumn(this)
	}

//	type TypedSubselectColumn[-F <: RowProduct, M[O] <: TypedColumn[_, O], V] =
//		SubselectColumn[F, V] { type RowMapping[O] <: M[O] }



	trait SpecificTopSelectColumnVisitor[+F <: RowProduct, X, +Y] extends SpecificTopSelectColumnAsVisitor[F, X, Y] {
		def topSelectColumn[R](e :TopSelectColumn[R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificTopSelectColumn[+F <: RowProduct, X, +Y] = SpecificTopSelectColumnVisitor[F, X, Y]

	trait CaseSpecificTopSelectColumn[+F <: RowProduct, X, +Y] extends MatchSpecificTopSelectColumn[F, X, Y] {
		override def topSelectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :TopSelectColumnAs[H, R])(implicit isRows :X =:= Rows[R]) :Y = topSelectColumn(e)
	}


	trait SpecificSubselectColumnVisitor[+F <: RowProduct, X, +Y] extends SpecificSubselectColumnAsVisitor[F, X, Y] {
		def subselectColumn[R](e :SubselectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificSubselectColumn[+F <: RowProduct, X, +Y] = SpecificSubselectColumnVisitor[F, X, Y]

	trait CaseSpecificSubselectColumn[+F <: RowProduct, X, +Y] extends MatchSpecificSubselectColumn[F, X, Y] {
		override def subselectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :SubselectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			subselectColumn(e)
	}


	trait SpecificSelectColumnVisitor[+F <: RowProduct, X, +Y]
		extends SpecificSubselectColumnVisitor[F, X, Y] with SpecificTopSelectColumnVisitor[F, X, Y]
		   with SpecificSelectColumnAsVisitor[F, X, Y]
	{
		def selectColumn[R](e :SelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificSelectColumn[+F <: RowProduct, X, +Y]
		extends SpecificSelectColumnVisitor[F, X, Y]
		   with CaseSpecificSubselectColumn[F, X, Y] with CaseSpecificTopSelectColumn[F, X, Y]
	{
		override def selectColumnAs[H[O] <: BaseColumn[R, O], R]
		                           (e :SelectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y = selectColumn(e)
	}
	trait CaseSpecificSelectColumn[+F <: RowProduct, X, +Y] extends MatchSpecificSelectColumn[F, X, Y] {
		override def subselectColumn[R](e :SubselectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			selectColumn(e)

		override def topSelectColumn[R](e :TopSelectColumn[R])(implicit isRows :X =:= Rows[R]) :Y =
			selectColumn(e)
	}
//
//
//
//	trait TopSelectColumnVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends TopSelectColumnAsVisitor[Y]
//	{
//		def topSelectColumn[R](e :TopSelectColumn[R]) :Y[Single, Rows[R], TopSelectColumn[R]]
//	}
//	type MatchTopSelectColumn[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		TopSelectColumnVisitor[Y]
//
//	trait CaseTopSelectColumn[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends MatchTopSelectColumn[Y]
//	{
//		override def topSelectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                              (e :TopSelectColumnAs[M, S]) :Y[Single, Rows[S], TopSelectColumnAs[M, S]] =
//			topSelectColumn(e)
//	}
//
//	trait SubselectColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SubselectColumnAsVisitor[F, Y]
//	{
//		def subselectColumn[R](e :SubselectColumn[F, R]) :Y[Single, Rows[R], SubselectColumn[F, R]]
//	}
//	type MatchSubselectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SubselectColumnVisitor[F, Y]
//
//	trait CaseSubselectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSubselectColumn[F, Y]
//	{
//		override def subselectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                              (e :SubselectColumnAs[F, M, S]) :Y[Single, Rows[S], SubselectColumnAs[F, M, S]] =
//			subselectColumn(e)
//	}
//
//	trait SelectColumnVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectColumnAsVisitor[F, Y] with TopSelectColumnVisitor[Y] with SubselectColumnVisitor[F, Y]
//	{
//		def selectColumn[R](e :SelectColumn[F, R]) :Y[Single, Rows[R], SelectColumn[F, R]]
//	}
//	trait MatchSelectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectColumnVisitor[F, Y] with CaseTopSelectColumn[Y] with CaseSubselectColumn[F, Y]
//	{
//		override def selectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                           (e :SelectColumnAs[F, M, S]) :Y[Single, Rows[S], SelectColumnAs[F, M, S]] =
//			selectColumn(e)
//	}
//	trait CaseSelectColumn[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSelectColumn[F, Y]
//	{
//		override def subselectColumn[R](e :SubselectColumn[F, R]) :Y[Single, Rows[R], SubselectColumn[F, R]] =
//			selectColumn(e)
//
//		override def topSelectColumn[R](e :TopSelectColumn[R]) :Y[Single, Rows[R], TopSelectColumn[R]] =
//			selectColumn(e)
//	}



	trait AnyTopSelectColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectColumnAsVisitor[F, Y]
	{
		def topSelectColumn[V](e :TopSelectColumn[V]) :Y[Single, Rows[V]]
	}
	type MatchAnyTopSelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyTopSelectColumnVisitor[F, Y]

	trait CaseAnyTopSelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyTopSelectColumn[F, Y]
	{
		override def topSelectColumnAs[H[O] <: BaseColumn[V, O], V]
		                              (e :TopSelectColumnAs[H, V]) :Y[Single, Rows[V]] =
			topSelectColumn(e)
	}

	trait AnySubselectColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectColumnAsVisitor[F, Y]
	{
		def subselectColumn[V](e :SubselectColumn[F, V]) :Y[Single, Rows[V]]
	}
	type MatchAnySubselectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnySubselectColumnVisitor[F, Y]

	trait CaseAnySubselectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnySubselectColumn[F, Y]
	{
		override def subselectColumnAs[H[O] <: BaseColumn[V, O], V]
		                              (e :SubselectColumnAs[F, H, V]) :Y[Single, Rows[V]] = subselectColumn(e)
	}

	trait AnySelectColumnVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectColumnVisitor[F, Y] with AnyTopSelectColumnVisitor[F, Y] with AnySelectColumnAsVisitor[F, Y]
	{
		def selectColumn[V](e :SelectColumn[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnySelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySelectColumnVisitor[F, Y] with CaseAnySubselectColumn[F, Y] with CaseAnyTopSelectColumn[F, Y]
	{
		override def selectColumnAs[H[O] <: BaseColumn[V, O], V](e :SelectColumnAs[F, H, V]) :Y[Single, Rows[V]] =
			selectColumn(e)
	}
	trait CaseAnySelectColumn[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnySelectColumn[F, Y]
	{
		override def subselectColumn[V](e :SubselectColumn[F, V]) :Y[Single, Rows[V]] = selectColumn(e :SelectColumn[F, V])
		override def topSelectColumn[V](e :TopSelectColumn[V]) :Y[Single, Rows[V]] = selectColumn(e :SelectColumn[F, V])
	}
}






/** A `SelectSQL` interface exposing the mapping type `H` used for the ''select'' clause. */
//todo: rename to MetaSelectSQL
trait SelectAs[-F <: RowProduct, H[A] <: MappingAt[A]] //consider: value type independent of the mapping Subject, for use with LValueSQL
	extends MappingQuerySQL[F, H] with SelectSQL[F, H[Unit]#Subject]
	   with SingleRowSQLTemplate[F, Rows[H[Unit]#Subject], ({ type Q[-f <: RowProduct] = SelectAs[f, H] })#Q]
{
//	override val selectClause :ComponentSQL[From, H]
//	override def constituents :Seq[SelectAs[F, H]] = this::Nil

	override def distinct :SelectAs[F, H]

//	def selectOther(selectClause :MappingSQL[From, Grouped, H, H[Unit]#Subject]) :SelectAs[F, H]
//
//	override def asGlobal :Option[SelectAs[F, H]] = Some(this)
//	override def anchor(from :F) :SelectAs[F, H]
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectAs[E, H]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SelectAs[E, H]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[H[Any]#Subject]] =
//			matcher.selectMapping(this)
}




object SelectAs {
	//todo: implementations for EditedComponentSQL and LValueConversion
	trait TopSelectAs[H[A] <: MappingAt[A]]
	 	extends TopSelectSQL[H[Unit]#Subject] with SelectAs[RowProduct, H] with SelectMapping[@~, H]
	 	   with GroundSQLTemplate[Rows[H[Unit]#Subject], TopSelectAs[H]]
	{
		override def distinct :TopSelectAs[H]
//		override def asGlobal :Option[TopSelectAs[H]] = Some(this)

		override def bind(params: @~) :TopSelectAs[H] = this

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[RowProduct, Y])
				:Y[Single, Rows[H[Unit]#Subject]] =
			visitor.topSelectAs[H](this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[RowProduct, Single, Rows[H[Unit]#Subject], Y])
				:Y =
			visitor.topSelectAs[H](this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: TopSelectAs[H] <: SQLExpression[F_, S_, Rows[H[Unit]#Subject]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[RowProduct, Y]) :Y[S_, Rows[H[Unit]#Subject], E] =
//			visitor.topSelectAs(this)
	}


	trait SubselectAs[-F <: RowProduct, H[A] <: MappingAt[A]]
		extends SelectAs[F, H] with SubselectSQL[F, H[Unit]#Subject]
	{
//		override def distinct :SubselectAs[F, H]
//		override def asGlobal :Option[SubselectAs[F, H]] = Some(this)
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectAs[E, H] =
//			expand(base)(ext.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:SubselectAs[E, H]
//
		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, Rows[H[Unit]#Subject]] =
			visitor.subselectAs[H](this)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, Rows[H[Unit]#Subject], Y]) :Y =
			visitor.subselectAs[H](this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: SubselectAs[F_, H] <: SQLExpression[F_, S_, Rows[H[Unit]#Subject]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, Rows[H[Unit]#Subject], E] =
//			visitor.subselectAs(this)
	}


	private[sql] trait BaseTopSelectAs[F <: GroundRow { type Complete <: F }, H[A] <: BaseMapping[V, A], V]
		extends TopSelectAs[H]
	{
		override type From = F
//		override def distinct :BaseTopSelectAs[F, H, V]
	}

	private[sql] trait BaseSubselectAs[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseMapping[V, A], V]
		extends SubselectAs[F, H]
	{
		override type From = S
//		override def distinct :BaseSubselectAs[F, S, H, V]
//		override def asGlobal :Option[BaseSubselectAs[F, S, H, V]] = Some(this)
	}






	trait SpecificTopSelectAsVisitor[+F <: RowProduct, X, +Y] extends SpecificTopSelectColumnAsVisitor[F, X, Y] {
		def topSelectAs[M[O] <: MappingAt[O]](e :TopSelectAs[M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y
	}
	trait MatchSpecificTopSelectAs[+F <: RowProduct, X, +Y] extends SpecificTopSelectAsVisitor[F, X, Y] {
		override def topSelectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :TopSelectColumnAs[H, R])(implicit isRows :X =:= Rows[R]) :Y =
			{ val res = topSelectAs(e); res }
	}
	type CaseSpecificTopSelectAs[+F <: RowProduct, X, +Y] = MatchSpecificTopSelectAs[F, X, Y]

	trait SpecificSubselectAsVisitor[+F <: RowProduct, X, +Y] extends SpecificSubselectColumnAsVisitor[F, X, Y] {
		def subselectAs[M[O] <: MappingAt[O]](e :SubselectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y
	}
	trait MatchSpecificSubselectAs[+F <: RowProduct, X, +Y] extends SpecificSubselectAsVisitor[F, X, Y] {
		override def subselectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :SubselectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			{ val  res = subselectAs(e); res }
	}
	type CaseSpecificSubselectAs[+F <: RowProduct, X, +Y] = MatchSpecificSubselectAs[F, X, Y]

	trait SpecificSelectAsVisitor[+F <: RowProduct, X, +Y]
		extends SpecificSelectColumnAsVisitor[F, X, Y] with SpecificTopSelectAsVisitor[F, X, Y] with SpecificSubselectAsVisitor[F, X, Y]
	{
		def selectAs[M[O] <: MappingAt[O]](e :SelectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y
	}
	trait MatchSpecificSelectAs[+F <: RowProduct, X, +Y]
		extends SpecificSelectAsVisitor[F, X, Y] with CaseSpecificTopSelectAs[F, X, Y] with CaseSpecificSubselectAs[F, X, Y]
	{
		override def selectColumnAs[H[O] <: BaseColumn[R, O], R]
		                           (e :SelectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			{ val res = selectAs(e); res }
	}
	trait CaseSpecificSelectAs[+F <: RowProduct, X, +Y] extends MatchSpecificSelectAs[F, X, Y] {
		override def subselectAs[M[O] <: MappingAt[O]]
		                        (e :SubselectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y = selectAs(e)

		override def topSelectAs[M[O] <: MappingAt[O]]
		                        (e :TopSelectAs[M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y = selectAs(e)
	}
//
//
//	trait TopSelectAsVisitor[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends TopSelectColumnAsVisitor[Y]
//	{
//		def topSelectAs[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject], TopSelectAs[M]]
//	}
//	trait MatchTopSelectAs[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]]
//		extends TopSelectAsVisitor[Y]
//	{
//		override def topSelectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                              (e :TopSelectColumnAs[M, S]) :Y[Single, Rows[S], TopSelectColumnAs[M, S]] =
//			topSelectAs(e)
//	}
//	type CaseTopSelectAs[+Y[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		MatchTopSelectAs[Y]
//
//	trait SubselectAsVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SubselectColumnAsVisitor[F, Y]
//	{
//		def subselectAs[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject], SubselectAs[F, M]]
//	}
//	trait MatchSubselectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SubselectAsVisitor[F, Y]
//	{
//		override def subselectColumnAs[M[O] <: BaseColumn[S, O], S](e :SubselectColumnAs[F, M, S])
//				:Y[Single, Rows[S], SubselectColumnAs[F, M, S]] =
//			subselectAs(e)
//	}
//	type CaseSubselectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		MatchSubselectAs[F, Y]
//
//	trait SelectAsVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectColumnAsVisitor[F, Y] with TopSelectAsVisitor[Y] with SubselectAsVisitor[F, Y]
//	{
//		def selectAs[M[O] <: MappingAt[O]](e :SelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject], SelectAs[F, M]]
//	}
//	trait MatchSelectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends SelectAsVisitor[F, Y]
//	{
//		override def selectColumnAs[M[O] <: BaseMapping[S, O], S]
//		                           (e :SelectColumnAs[F, M, S]) :Y[Single, Rows[S], SelectColumnAs[F, M, S]] =
//			selectAs(e)
//	}
//	trait CaseSelectAs[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSelectAs[F, Y] with CaseTopSelectAs[Y] with CaseSubselectAs[F, Y]
//	{
//		override def topSelectAs[M[O] <: MappingAt[O]]
//		                        (e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject], TopSelectAs[M]] =
//			selectAs(e)
//
//		override def subselectAs[M[O] <: MappingAt[O]]
//		                        (e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject, SubselectAs[F, M]]] =
//			selectAs(e)
//	}


	trait AnyTopSelectAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectColumnAsVisitor[F, Y]
	{
		def topSelectAs[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject]]
	}
	trait MatchAnyTopSelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectAsVisitor[F, Y]
	{
		override def topSelectColumnAs[H[O] <: BaseColumn[V, O], V](e :TopSelectColumnAs[H, V]) :Y[Single, Rows[V]] =
			{ val res = topSelectAs(e); res }
	}
	type CaseAnyTopSelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = MatchAnyTopSelectAs[F, Y]

	trait AnySubselectAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectColumnAsVisitor[F, Y]
	{
		def subselectAs[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]]
	}
	trait MatchAnySubselectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySubselectAsVisitor[F, Y]
	{
		override def subselectColumnAs[H[O] <: BaseColumn[V, O], V](e :SubselectColumnAs[F, H, V]) :Y[Single, Rows[V]] =
			{ val res = subselectAs(e); res }
	}
	type CaseAnySubselectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = MatchAnySubselectAs[F, Y]

	trait AnySelectAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySelectColumnAsVisitor[F, Y] with AnyTopSelectAsVisitor[F, Y] with AnySubselectAsVisitor[F, Y]
	{
		def selectAs[H[O] <: MappingAt[O]](e :SelectAs[F, H]) :Y[Single, Rows[H[Unit]#Subject]]
	}
	trait MatchAnySelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySelectAsVisitor[F, Y] with CaseAnyTopSelectAs[F, Y] with CaseAnySubselectAs[F, Y]
	{
		override def selectColumnAs[H[O] <: BaseColumn[V, O], V](e :SelectColumnAs[F, H, V]) :Y[Single, Rows[V]] =
			{ val res = selectAs(e); res }
	}
	trait CaseAnySelectAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnySelectAs[F, Y]
	{
		override def subselectAs[M[O] <: MappingAt[O]](e :SubselectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]] =
			selectAs(e)

		override def topSelectAs[M[O] <: MappingAt[O]](e :TopSelectAs[M]) :Y[Single, Rows[M[Unit]#Subject]] =
			selectAs(e)
	}

}





/** Selects a single column and exposes its mapping type `H`.
  * There is little practical use in this type that cannot be satisfied
  * by [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]] as columns are not expected to be subclassed,
  * but it serves as a conjunction type of essential [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]
  * and [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]].
  * A [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]],
  * even for base [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]], is a subtype
  * of both [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] - which is selectable as `SelectAs` -
  * and `ColumnSQL` - which is selectable as `SelectColumn`, thus the requirement for this trait.
  */ //consider: removing the subject type like in SelectAs
trait SelectColumnAs[-F <: RowProduct, H[A] <: BaseColumn[V, A], V]
	extends ColumnMappingQuery[F, H, V] with SelectAs[F, H] with SelectColumn[F, V]
	   with SingleRowSQLTemplate[F, Rows[V], ({ type Q[-f <: RowProduct] = SelectColumnAs[f, H, V] })#Q]
{
//	override val selectClause :ColumnMappingSQL[From, H, V]
//	override def constituents :Seq[SelectColumnAs[F, H, V]] = this::Nil

	override def distinct :SelectColumnAs[F, H, V]

//	override def asGlobal :Option[SelectColumnAs[F, H, V]] = Some(this)

//	override def anchor(from :F) :SelectColumnAs[F, H, V] //= this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SelectColumnAs[E, H, V]
//
//	override def expand[U <: F, E <: RowProduct]
//	             (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :SelectColumnAs[E, H, V]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.selectMapping(this)
}




object SelectColumnAs {

	trait TopSelectColumnAs[H[A] <: BaseColumn[R, A], R]
		extends TopSelectColumn[R] with TopSelectAs[H] with SelectColumnAs[RowProduct, H, R]
		   with GroundSQLTemplate[Rows[R], TopSelectColumnAs[H, R]]
	{
		override def distinct :TopSelectColumnAs[H, R]
//		override def asGlobal :Option[TopSelectColumnAs[H, V]] = Some(this)
//
//		override def basedOn[U <: RowProduct, E <: RowProduct](base :E)(implicit ext :U PartOf E) :TopSelectColumnAs[H, V] =
//			this
//
//		override def expand[U <: RowProduct, S <: RowProduct]
//		                   (base :S)(implicit ev :U ExpandedBy S, global :GlobalScope <:< GlobalScope)
//				:TopSelectColumnAs[H, V] =
//			this

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[RowProduct, Y]) :Y[Single, Rows[R]] =
			visitor.topSelectColumnAs(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[RowProduct, Single, Rows[R], Y]) :Y =
			visitor.topSelectColumnAs(this)
//
//		protected override def visit[F_ <: RowProduct, S_ >: Grouped <: Single,
//		                             E >: TopSelectColumnAs[H] <: SQLExpression[F_, S_, Rows[R]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[RowProduct, Y]) :Y[S_, Rows[R], E] =
//			visitor.topSelectColumnAs(this)
	}


	trait SubselectColumnAs[-F <: RowProduct, H[A] <: BaseColumn[V, A], V]
		extends SubselectColumn[F, V] with SubselectAs[F, H] with SelectColumnAs[F, H, V]
		   with SingleRowSQLTemplate[F, Rows[V], ({ type Q[-f <: RowProduct] = SubselectColumnAs[f, H, V] })#Q]
	{
		override def distinct :SubselectColumnAs[F, H, V]
//		override def asGlobal :Option[SubselectColumnAs[F, H, V]] = Some(this)
//
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :SubselectColumnAs[E, H, V] =
//			expand(base)(ext.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:SubselectColumnAs[E, H, V]

		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyColumnVisitor[F, Y]) :Y[Single, Rows[V]] =
			visitor.subselectColumnAs(this)

		protected override def visit[Y](visitor :SpecificColumnVisitor[F, Single, Rows[V], Y]) :Y =
			visitor.subselectColumnAs(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//		                             E >: SubselectColumnAs[H] <: SQLExpression[F_, S_, Rows[H[Unit]#Subject]],
//		                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F_, Y]) :Y[S_, Rows[H[Unit]#Subject], E] =
//			visitor.subselectColumnAs(this)
	}


	private[sql] trait BaseTopSelectColumnAs[F <: GroundRow { type Complete <: F }, H[A] <: BaseColumn[V, A], V]
		extends TopSelectColumnAs[H, V] with BaseTopSelectAs[F, H, V]
	{
		override def distinct :BaseTopSelectColumnAs[F, H, V]
//		override def asGlobal :Option[BaseTopSelectColumnAs[F, H, V]] = Some(this)
	}

	private[sql] trait BaseSubselectColumnAs[-F <: RowProduct, S <: SubselectOf[F], H[A] <: BaseColumn[V, A], V]
		extends SubselectColumnAs[F, H, V] with BaseSubselectAs[F, S, H, V]
	{
		override def distinct :BaseSubselectColumnAs[F, S, H, V]
//		override def asGlobal :Option[SubselectColumnMapping[F, S, H, V]] = Some(this)
//
//		protected override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                            (visitor :AnyColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			visitor.subselectColumnAs(this)
//
//		protected override def visit[Y](visitor :SpecificColumnVisitor[F, GlobalScope, Rows[V], Y]) :Y =
//			visitor.subselectColumnAs(this)
//
//		protected override def visit[F_ <: F, S_ >: Grouped <: S,
//		                             E >: SelectColumnSQL[F, S, Boolean] <: SQLExpression[F_, S_, Boolean],
//		                             R[-s >: Grouped <: GlobalScope, v, -e <: SQLExpression[F_, s, v]]]
//		                            (visitor :ColumnVisitor[F, R]) :R[S_, Boolean, E] =
//			visitor.subselectColumnAs(this)
	}




	trait SpecificTopSelectColumnAsVisitor[+F <: RowProduct, X, +Y] {
		def topSelectColumnAs[H[O] <: BaseColumn[R, O], R]
		                     (e :TopSelectColumnAs[H, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificTopSelectColumnAs[+F <: RowProduct, X, +Y] = SpecificTopSelectColumnAsVisitor[F, X, Y]
	type CaseSpecificTopSelectColumnAs[+F <: RowProduct, X, +Y] = SpecificTopSelectColumnAsVisitor[F, X, Y]


	trait SpecificSubselectColumnAsVisitor[+F <: RowProduct, X, +Y] {
		def subselectColumnAs[H[O] <: BaseColumn[R, O], R]
		                     (e :SubselectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificSubselectColumnAs[+F <: RowProduct, X, +Y] = SpecificSubselectColumnAsVisitor[F, X, Y]
	type CaseSpecificSubselectColumnAs[+F <: RowProduct, X, +Y] = SpecificSubselectColumnAsVisitor[F, X, Y]


	trait SpecificSelectColumnAsVisitor[+F <: RowProduct, X, +Y]
		extends SpecificTopSelectColumnAsVisitor[F, X, Y] with SpecificSubselectColumnAsVisitor[F, X, Y]
	{
		def selectColumnAs[H[O] <: BaseColumn[R, O], R](e :SelectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificSelectColumnAs[+F <: RowProduct, X, +Y] = SpecificSelectColumnAsVisitor[F, X, Y]

	trait CaseSpecificSelectColumnAs[+F <: RowProduct, X, +Y] extends SpecificSelectColumnAsVisitor[F, X, Y] {
		override def topSelectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :TopSelectColumnAs[H, R])(implicit isRows :X =:= Rows[R]) :Y =
			selectColumnAs(e)

		override def subselectColumnAs[H[O] <: BaseColumn[R, O], R]
		                              (e :SubselectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			selectColumnAs(e)
	}
//
//
//	trait TopSelectColumnAsVisitor[+R[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] {
//		def topSelectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                     (e :TopSelectColumnAs[M, S]) :R[Single, Rows[S], TopSelectColumnAs[M, S]]
//	}
//	type MatchTopSelectColumnAs[+R[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		TopSelectColumnAsVisitor[R]
//	type CaseTopSelectColumnAs[+R[-S >: Grouped <: Single, V, -e <: SQLExpression[RowProduct, S, V]]] =
//		TopSelectColumnAsVisitor[R]
//
//	trait SubselectColumnAsVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def subselectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                     (e :SubselectColumnAs[F, M, S]) :R[Single, Rows[S], SubselectColumnAs[F, M, S]]
//	}
//	type MatchSubselectColumnAs[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SubselectColumnAsVisitor[F, R]
//	type CaseSubselectColumnAs[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SubselectColumnAsVisitor[F, R]
//
//	trait SelectColumnAsVisitor[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends TopSelectColumnAsVisitor[R] with SubselectColumnAsVisitor[F, R]
//	{
//		def selectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                  (e :SelectColumnAs[F, M, S]) :R[Single, Rows[S], SelectColumnAs[F, M, S]]
//	}
//	type MatchSelectColumnAs[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		SelectColumnAsVisitor[F, R]
//
//	trait CaseSelectColumnAs[+F <: RowProduct, +R[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchSelectColumnAs[F, R] with CaseTopSelectColumnAs[R] with CaseSubselectColumnAs[F, R]
//	{
//		override def topSelectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                              (e :TopSelectColumnAs[M, S]) :R[Single, Rows[S], TopSelectColumnAs[M, S]] =
//			selectColumnAs(e)
//
//		override def subselectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                              (e :SubselectColumnAs[F, M, S]) :R[Single, Rows[S], SubselectColumnAs[F, M, S]] =
//			selectColumnAs(e)
//	}


	trait AnyTopSelectColumnAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def topSelectColumnAs[H[O] <: BaseColumn[V, O], V](e :TopSelectColumnAs[H, V]) :Y[Single, Rows[V]]
	}
	type MatchAnyTopSelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyTopSelectColumnAsVisitor[F, Y]
	type CaseAnyTopSelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyTopSelectColumnAsVisitor[F, Y]


	trait AnySubselectColumnAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def subselectColumnAs[H[O] <: BaseColumn[V, O], V](e :SubselectColumnAs[F, H, V]) :Y[Single, Rows[V]]
	}
	type MatchAnySubselectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnySubselectColumnAsVisitor[F, Y]
	type CaseAnySubselectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnySubselectColumnAsVisitor[F, Y]


	trait AnySelectColumnAsVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTopSelectColumnAsVisitor[F, Y] with AnySubselectColumnAsVisitor[F, Y]
	{
		def selectColumnAs[H[O] <: BaseColumn[V, O], V](e :SelectColumnAs[F, H, V]) :Y[Single, Rows[V]]
	}
	type MatchAnySelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnySelectColumnAsVisitor[F, Y]

	trait CaseAnySelectColumnAs[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnySelectColumnAsVisitor[F, Y]
	{
		override def topSelectColumnAs[H[O] <: BaseColumn[V, O], V](e :TopSelectColumnAs[H, V]) :Y[Single, Rows[V]] =
			selectColumnAs(e)

		override def subselectColumnAs[H[O] <: BaseColumn[V, O], V](e :SubselectColumnAs[F, H, V]) :Y[Single, Rows[V]] =
			selectColumnAs(e)
	}

}

