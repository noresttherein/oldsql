package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.{CompoundSelect, Query, RowProduct, Select, SQLExpression}
import net.noresttherein.oldsql.sql.Query.SingleQuery
import net.noresttherein.oldsql.sql.Select.SelectOperator
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingTemplate, Grouped, Single, SQLShape}
import net.noresttherein.oldsql.sql.ast.{AdaptedSQL, ChainSQL, ComponentSQL, CompoundSelectSQL, InlineSQL, LabeledSQL, LValueSQL, QuerySQL, SelectIdSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL




/*
private[sql] trait LinearReform extends AbstractExpressionReform {
	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
	                                      (implicit spelling :SQLSpelling) =


	override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])(implicit spelling :SQLSpelling) = super.apply(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])(implicit spelling :SQLSpelling) = super.apply(left, right)
}
*/
/** A base trait for `Reform` implementations which do not perform a full unification
  * of the argument expressions/queries, but instead replace leaf (non-composite) expressions with some placeholders
  * containing information for a later stage of the unification.
  * Overrides all two-argument `apply` methods, directing them to the three basic `fallback` methods:
  * for `SQLExpression`, `QuerySQL` and `Query` pairs. Main `apply` for `SQLExpression`, as an exception,
  * defers to `super` if both arguments are a `CompositeSQL` - i.e., it lets the standard reform process
  * of composite expressions to descend down the tree and rebuild the expression pair with a shared structure.
  * All `fallback` methods for query arguments simply return the query pair without any reforming.
  * This short circuits  the standard `Query`/`QuerySQL` reforming algorithm, turning it into a recursion
  * linear in computational complexity, as `Query.`[[net.noresttherein.oldsql.sql.Query.reform reform]] methods
  * are never called: reforming of a ''compound select'' is limited to recursively reforming its sides,
  * separately of each other.
  *
  * Subclasses should as a minimum override `fallback(SQLExpression, SQLExpression)`, as the default implementation
  * inherited from `Reform` performs only shape validation and throws an `MismatchedExpressionsException`
  * if different.
  *
  * Taken together, when called for a `query :QuerySQL` or `query :Query` instance, it will:
  *   1. return `default(query)` if `query :SelectSQL` or `query :Select`, as normal (which, unless overridden,
  *      returns the argument unchanged)
  *   1. return `query` if `query :SingleQuerySQL` or `query :SingleQuery`, again, as normal;
  *   1. return `default(apply(query.left), query.operator, apply(query.right))` if `query :CompoundSelectSQL`
  *      or `query :CompoundSelect`, which applies the same algorithm recursively.
  *
  */ //todo: after splitting into SelectReform and Reform, change the SelectReform name to LinearSelectReform
private[sql] trait PreprocessorReform extends Reform {
	override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
	                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
	                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                            spelling :SQLSpelling)
			:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
		//Far from beautiful, but surprisingly it correctly covers the whole standard class hierarchy.
		// Even in case of custom extensions, it's better to err on the side of bundling the expressions together
		// than risk their attempts at actual column-level reforming.
		//fixme: AlignedExpression can have another AlignedExpression as a subexpression as difference
		// from other expression can occur at different levels
		//
		//todo: QuerySQL (but non column queries cannot appear in a select clause, so maybe unnecessary?
		(left, right) match {
//			case (_ :ColumnSQL[_, _, _], _ :ColumnSQL[_, _, _]) => fallback(left, right)
			//if we recurse when only one of the expressions is a CompositeSQL, we risk aligning it with a ComponentSQL
//			case (_ :CompositeSQL[_, _, _], _ :CompositeSQL[_, _, _]) => super.apply(left, right)
//			case (_ :ComponentLValueSQL[_, _, _], _) => ???
//			case (_, _ :ComponentLValueSQL[_, _, _]) => ???
			case (_ :LabeledSQL[_, _, _], _ :LabeledSQL[_, _, _]) =>
				super.apply[LF, LS, LV, LE, RF, RS, RV, RE, U](left, right)
			case (_ :InlineSQL[_, _, _] | _ :ChainSQL[_, _, _, _] | _ :SelectIdSQL[_, _, _],
			      _ :InlineSQL[_, _, _] | _ :ChainSQL[_, _, _, _] | _ :SelectIdSQL[_, _, _]) =>
				super.apply(left, right)
			case (_ :AdaptedSQL[_, _, _, _], _) => super.apply[LF, LS, LV, LE, RF, RS, RV, RE, U](left, right)
			case (_, _ :AdaptedSQL[_, _, _, _]) => super.apply[LF, LS, LV, LE, RF, RS, RV, RE, U](left, right)
			case _ => fallback(left, right) //ColumnSQL, CompositeSQL, MappingSQL, SQLTerm, QuerySQL
		}

/*
	override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
	                  (left :LValueSQL[L, A, X], right :SQLExpression[R, B, Y])
	                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
			:(ExpressionLValue[L, A, Z], SQLExpression[R, B, Z]) =
		apply(left :SQLExpression[L, Single, X], right :SQLExpression[R, B, Y])

	override def apply[L <: RowProduct, A >: Grouped <: Single, R <: RowProduct, X, B[O] <: MappingAt[O], Y, Z]
	                  (left :SQLExpression[L, A, X], right :LValueSQL[R, B, Y])
	                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
			:(SQLExpression[L, A, Z], ExpressionLValue[R, B, Z]) =
		apply(left :SQLExpression[L, A, X], right :SQLExpression[R, Single, Y])

	override def apply[L <: RowProduct, A[O] <: MappingAt[O], X, R <: RowProduct, B[O] <: MappingAt[O], Y, Z]
	                  (left :LValueSQL[L, A, X], right :LValueSQL[R, B, Y])
	                  (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
			:(ExpressionLValue[L, A, Z], ExpressionLValue[R, B, Z]) =
		apply(left :SQLExpression[L, Single, X], right :SQLExpression[R, Single, Y])
*/

//	override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//	                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//	                     (implicit leftResult :SQLTransformation[left.Subject, Z],
//	                               rightResult :SQLTransformation[right.Subject, Z], spelling :SQLSpelling)
//			:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z], rightResult.SQLResult[R, Single, LValue[R, B, Z]]) =
//		apply(left :SQLExpression[L, Single, A[Unit]#Subject], right :SQLExpression[R, Single, B[Unit]#Subject])


/*
	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		fallback(left, right)

	override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		fallback(left, right)

	override def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		fallback(left, right)

	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		fallback(left, right)

	override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		fallback(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling)
			:(Query[X, V], Query[Y, V]) =
		fallback(left, right)

	override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling)
			:(Query[X, V], Query[Y, V]) =
		fallback(left, right)

	override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling)
			:(Query[X, V], Query[Y, V]) =
		fallback(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])(implicit spelling :SQLSpelling)
				:(Query[X, V], Query[Y, V]) =
		fallback(left, right)

	override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])(implicit spelling :SQLSpelling)
			:(Query[X, V], Query[Y, V]) =
		fallback(left, right)
*/


//	override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
//	                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
//	                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
//	                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
//	                               spelling :SQLSpelling)
//			:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =

	override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
	                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
	                    (implicit leftResult :SQLTransformation[left.Subject, U],
	                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
			:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
		fallback(left, right)
/*

	override def fallback[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
	                                         (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
		(left, right)

	override def fallback[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling)
			:(Query[X, V], Query[Y, V]) =
		(left, right)

	override def reformed[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V])
	                                         (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
	{
		val (leftSelectClause, rightSelectClause) = fallback[Nothing, Grouped, V, Nothing, Grouped, V, V](
			left.selectClause, right.selectClause
		)(SQLTypeUnification.directly, spelling.inSelect)
		//todo:
		val selectClause = compoundSelectClause(leftSelectClause, rightSelectClause)
		CompoundSelectSQL.reformed(left, operator, right, selectClause)(self)
	}

	override def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
	                           (implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
	{
		val (leftSelectClause, rightSelectClause) = fallback[Nothing, Grouped, V, Nothing, Grouped, V, V](
			left.selectClause, right.selectClause
		)(SQLTypeUnification.directly, spelling.inSelect)
		//todo:
		val selectClause = compoundSelectClause(leftSelectClause, rightSelectClause)
		CompoundSelect.reformed(left, operator, right, selectClause)(self)
	}
*/

	protected def compoundSelectClause[V](left :SQLShape[V], right :SQLShape[V]) :SQLShape[V] = left
/*
		(left, right) match {
			case (l :ComponentLValueSQL.Cast[Nothing, V] @unchecked, _) => l
			case (_, r :ComponentLValueSQL.Cast[Nothing, V] @unchecked) => r
			case (l :MappingSQL.Cast[Nothing, V] @unchecked, _)         => l
			case (_, r :MappingSQL.Cast[Nothing, V] @unchecked)         => r
			case (l :ColumnSQL[Nothing, _, V] @unchecked, _)            => l
			case (_, r :ColumnSQL[Nothing, _, V] @unchecked)            => r
			case (l :AlignedExpression[Nothing, _, V] @unchecked,
			      r :AlignedExpression[Nothing, _, V] @unchecked)       =>
				new AlignedExpression[Nothing, Grouped, V](compoundSelectClause(l.value, r.value), l.alignment)
			case (l :AlignedExpression[Nothing, _, V] @unchecked, _)    =>
				new AlignedExpression[Nothing, Grouped, V](compoundSelectClause(l.value, right), l.alignment)
			case (_, r :AlignedExpression[Nothing, _, V] @unchecked)    =>
				new AlignedExpression[Nothing, Grouped, V](compoundSelectClause(left, r.value), r.alignment)
			case (l :CompositeSQL[Nothing, _, V] @unchecked, _)         => l
			case (_, r :CompositeSQL[Nothing, _, V] @unchecked)         => r
			case _                                                      => left
		}
*/


//	override def validate[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) :Unit = ()
//
/*
	override def compatible[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) :Boolean =
		true

	override def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	                       (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                       (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                                 spelling :SQLSpelling) :Boolean =
		true

//	protected def layout[X, Y, Z](left :SQLLayout[X], right :SQLLayout[Y])
//	                             (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling) :SQLLayout[Z] =
//		(left, right) match {
//			case (_ :ComponentLValueSQL.*, _) => left.to(compat.left)
//			case (_, _ :ComponentLValueSQL.*) => right.to(compat.right)
//			case (_ :MappingSQL.*, _) => left.to(compat.left)
//			case (_, _ :MappingSQL.*) => right.to(compat.right)
//			case (_ :ColumnTerm[_], _ :ColumnSQL[_, _, _]) => right.to(compat.right)
//			case (_ :ColumnSQL[_, _, _], _ :ColumnSQL[_, _, _]) => left.to(compat.left)
//			case (QuerySQL(query, equiv), _) =>
//				def lift[A](equiv :X =:= Rows[A]) = {
//					val ident = equiv.substituteContra[({ type L[V] = Lift[Rows[A], V] })#L](Lift.self)
//					Lift.toRow[A] andThen ident andThen compat.left vs compat.right
//				}
//				layout(query.selectClause, right)(lift(equiv), spelling)
//			case (_, _ :QuerySQL[_, _]) => layout(right, left)(compat.swap, spelling)
////			case (l :CompositeSQL[_, _, X], r :CompositeSQL[_, _, Y]) =>
////				???
//			case (_ :CompositeSQL[_, _, _], _) => left.to(compat.left)
//			case (_, _ :CompositeSQL[_, _, _]) => right.to(compat.right)
//			case (_ :SQLTerm[_], _) => right.to(compat.right)
//			case (_, _ :SQLTerm[_]) => left.to(compat.left)
//		}


	override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :Reform =
		query.operator.prevent(self)

	override def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :Reform =
		query.operator.prevent(self)
*/

//	override def left(implicit spelling :SQLSpelling)  :Reform = self //:ReformTo[LValue] = this
//	override def right(implicit spelling :SQLSpelling) :Reform = self //:ReformTo[LValue] = this
//	override def left
}


