package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Decorable.BaseDecorable
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.Query.{AbstractQuery, QueryTemplate}
import net.noresttherein.oldsql.sql.{ColumnSQL, CompoundSelect, Query, RowProduct, SQLExpression, Select}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.DecoratedSQL.FloatingDecoratedSQLTemplate
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, CompoundSelectColumn, CompoundSelectSQL, DecoratedSQL, QuerySQL, SelectSQL}
import net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform.{QueryAligner, TermQueryReform, TermReform}
import net.noresttherein.oldsql.sql.mechanics.QueryReform.{OptimisticQueryReform, TopDownQueryReform}
import net.noresttherein.oldsql.sql.mechanics.Reform.{AbstractReform, OptimisticReform, PassCount, TunedReform}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReform, NoReform}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{DetailedReformPermissions, ForbiddenReform, Permissions}
import net.noresttherein.oldsql.sql.mechanics.UnalignedSQL.UnalignedSQLTemplate






/** Reforms a [[net.noresttherein.oldsql.sql.Query Query]] or a [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]
  * in order to attain consistent [[net.noresttherein.oldsql.sql.ast.HasRowShape.shape shape]] by considering
  * all member ''selects'' at once, instead of recursively doing it in pairs.
  *   1. In the first step, an [[net.noresttherein.oldsql.sql.mechanics.Alignment Alignment]] is built for the query,
  *      which defines the final column composition and order. Each query contributes to the alignment `ColumnSQL`
  *      expressions which it expects may match columns of ''select'' clauses of other queries through
  *      [[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns potentialColumns]] method. `Alignment` is then
  *      responsible to match like columns with like based on criteria implemented in particular
  *      [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn AlignableColumn]] returned from `potentialColumns`,
  *      and determines the best order and the final shape of the whole query.
  *   1. Then, all ''select'' clauses are [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.realign reordered]]
  *      to match the alignment. These [[net.noresttherein.oldsql.pixies.Rearrangement reorderings]]
  *      define the transformation which needs to be applied to the columns of every ''select'' clause, and, recursively,
  *      its subexpressions. This may involve all reforming options: including optional columns,
  *      excluding optional columns, adding null columns, and reordering them. This is expected to resolve the issue
  *      for most expressions. However, some expressions, most notably
  *      a majority of [[net.noresttherein.oldsql.sql.ast.SQLTerm SQLTerm]]s, cannot be aligned in the manner described
  *      above, as it is impossible to create `AlignedColumn`s representing their columns, in a way which could match
  *      columns of expressions of other types. Instead, those expressions wrap themselves in a
  *      [[net.noresttherein.oldsql.sql.mechanics.UnalignedSQL UnalignedSQL]] returned from their `realign` method.
  *   1. Finally, the default query [[net.noresttherein.oldsql.sql.Query.reform reforming]] algorithm is used
  *      to reform the ''select'' clauses in pairs with all other ''select'' clauses. The actual
  *      expression [[net.noresttherein.oldsql.sql.SQLExpression.ConvertingOps.reform reforming]] is done with
  *      a [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] whose permissions do not allow any kind of reforming.
  *      Those restrictions are lifted only for `UnalignedSQL` classes, which in term allow their wrapped expressions
  *      to take the form of whatever other expression they are aligned with.
  */
class HolisticQueryReform(override val permissions :Permissions)(wrap :QueryReform => QueryReform)
	extends DetailedReformPermissions[QueryReform](permissions)(wrap, new HolisticQueryReform(_)(_))
	   with QueryReform
{
	override def apply[F <: RowProduct, R](left :QuerySQL[F, R], right :QuerySQL[F, R])
	                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, R], QuerySQL[F, R]) =
		(left, right)

	override def apply[X, Y, R](left :Query[X, R], right :Query[Y, R])
	                           (implicit spelling :SQLSpelling) :(Query[X, R], Query[Y, R]) =
		(left, right)

	//this is cut&paste from QueryReform, in case that implementation is changed.
	override def default[F <: RowProduct, V](query: CompoundSelectColumn[F, V])
	                                        (implicit spelling: SQLSpelling): CompoundSelectColumn[F, V] =
	{
		validate(query.left, query.right)
		query
	}

	override def default[F <: RowProduct, V](query: CompoundSelectSQL[F, V])
	                                        (implicit spelling: SQLSpelling): CompoundSelectSQL[F, V] =
	{   //todo: fast paths
		val alignment = spelling.alignment(query, permissions)
		val aligner   = new QueryAligner(alignment)(0, query.left.selectCount, query.selectCount)(identity)
		val reordered = aligner(query)
		TermQueryReform(reordered)
	}

	override def default[P, V](query: CompoundSelect[P, V])
	                          (implicit spelling: SQLSpelling): CompoundSelect[P, V] =
	{   //todo: fast paths
		val alignment = spelling.alignment(query, permissions)
		val aligner   = new QueryAligner(alignment)(0, query.left.selectCount, query.selectCount)(identity)
		val reordered = aligner(query)
		//consider: how will this work if all select clauses are terms?
		TermQueryReform(reordered)
		//Consider: currently, TermReform does not perform any validation, so its possible, even if unlikely,
		// that the result is invalid. Should we run a compatibility check on everything once we're done?
	}


	//permissions are compared in equals proper
	override def undecoratedEquals(that: QueryReform): Boolean = that.isInstanceOf[HolisticQueryReform]
}



object HolisticQueryReform {

	/** Given an alignment containing all ''select'' clauses of the reformed query, reform each by using
	  * a proper [[net.noresttherein.oldsql.pixies.Rearrangement Rearrangement]].
	  * This is not a complete reform, but a single step
	  * of [[net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform HolisticQueryReform]].
	  * Any terms, as well as possibly other expressions which return empty
	  * [[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns potentialColumns]], will remain unchanged
	  * and need to be reformed by other means. The proper reordering in `alignment` is identified by a narrowing
	  * range `from..until` of indices of member ''selects'' of the top level query.
	  */
	private class QueryAligner(val alignment :Alignment)(val from :Int, val middle :Int, val until :Int)
	                          (wrap :QueryReform => QueryReform)
	//	extends DetailedReformPermissions(permissions)(wrap, new QueryAligner(alignment)(_)(_))
		extends BaseDecorable[QueryReform](wrap, new QueryAligner(alignment)(from, middle, until)(_))
		   with ForbiddenReform[QueryReform]
		   with QueryReform
	{
		if (until < from)
			throw new UnsupportedOperationException(
				"Parameterless left/right called not on a result of compound when aligning " + alignment + "."
			)
		override def apply[F <: RowProduct, R](left :QuerySQL[F, R], right :QuerySQL[F, R])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, R], QuerySQL[F, R]) =
			(left, right)

		override def apply[X, Y, R](left :Query[X, R], right :Query[Y, R])
		                           (implicit spelling :SQLSpelling) :(Query[X, R], Query[Y, R]) =
			(left, right)

		private def validateRange[R](query :AbstractQuery[R], expected :String) :Unit =
			if (query.selectCount != until - from)
				throw new IllegalArgumentException(
					"Given `" + query + "` to reform when expected a " + expected + " of size " + until + " - " + from + "."
				)

		override def default[F <: RowProduct, V](query: SelectSQL[F, V])
		                                        (implicit spelling: SQLSpelling) :SelectSQL[F, V] =
		{
			validateRange(query, "CompoundSelectSQL")
			spelling.realign(query, alignment.reorderings(from))
		}

		override def default[V](query: TopSelectSQL[V])(implicit spelling: SQLSpelling) :TopSelectSQL[V] = {
			validateRange(query, "CompoundSelectSQL/CompoundSelect")
			spelling.realign(query, alignment.reorderings(from))
		}

		override def default[P, V](query: Select[P, V])(implicit spelling: SQLSpelling) :Select[P, V] = {
			validateRange(query, "CompoundSelect")
			//Currently Query does not extend ReorderingTemplate
//			spelling.realign(query, alignment.reorderings(from))
			val realigned = spelling.realign(query.selectClause, alignment.reorderings(from))
			if (realigned eq query.selectClause) query
			else query.selectOther(realigned)
		}
/*

		override def default[F <: RowProduct, V](query: CompoundSelectColumn[F, V])
		                                        (implicit spelling: SQLSpelling) :CompoundSelectColumn[F, V] =

		override def default[F <: RowProduct, V](query: CompoundSelectSQL[F, V])
		                                        (implicit spelling: SQLSpelling) :CompoundSelectSQL[F, V] =
		{
			val reformedLeft  = apply(query.left)
			val reformedRight = apply(query.right)
			if ((reformedLeft eq query.left) && (reformedRight eq query.right)) {
	//			query.reformedBy = (this, spelling)
				query
			} else
				reformed(reformedLeft, query.operator, reformedRight)
		}

		override def default[P, V](query: CompoundSelect[P, V])(implicit spelling: SQLSpelling) :CompoundSelect[P, V] = {
			val reformedLeft = apply(query.left)
			val reformedRight = apply(query.right)
			if ((reformedLeft eq query.left) && (reformedRight eq query.right)) {
	//			query.reformedBy = (this, spelling)
				query
			} else
				reformed(reformedLeft, query.operator, reformedRight)
		}
*/

		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                     (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		{
//			validateRange(query, if (until - from == 1) "Select/SelectSQL" else "CompoundSelect/CompoundSelectSQL")
			new QueryAligner(alignment)(from, from + query.left.selectCount, until)(wrap)
		}
		override def left(implicit spelling :SQLSpelling) :QueryReform =
			new QueryAligner(alignment)(from, -1, middle)(wrap)

		override def right(implicit spelling :SQLSpelling) :QueryReform =
			new QueryAligner(alignment)(middle, -1, until)(wrap)

		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		{
			validateRange(query, if (until - from == 1) "Select/SelectSQL" else "CompoundSelect/CompoundSelectSQL")
			new QueryAligner(alignment)(from, -1, from + query.left.selectCount)(wrap)
		}

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		{
			validateRange(query, if (until - from == 1) "Select/SelectSQL" else "CompoundSelect/CompoundSelectSQL")
			new QueryAligner(alignment)(until - query.right.selectCount, -1, until)(wrap)
		}

		override def undecoratedEquals(that :QueryReform) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :QueryAligner =>
				from == other.from && middle == other.middle && until == other.until && alignment == other.alignment
			case _ => false
		}
		override def undecoratedHashCode :Int =
			((from.hashCode * 31 + middle.hashCode) * 31 + until.hashCode) * 31 + alignment.hashCode

		override def toString :String = "QueryAligner(" + alignment + ")"
	}



	private class TermQueryReform(override val permissions :Permissions)(wrap :QueryReform => QueryReform)
		extends DetailedReformPermissions[QueryReform](permissions)(wrap, new TermQueryReform(_)(_))
		   with TopDownQueryReform
		   with OptimisticQueryReform
	{
		override def reform(implicit spelling :SQLSpelling) :Reform = new TermReform(permissions)(identity)
		override def typeName :String = "TermQueryReform"
	}

	private val TermQueryReform :QueryReform = new TermQueryReform(MayReform)(identity)

//	private class TermQueryReform(permissions :Permissions)(wrap :QueryReform => QueryReform)
//		extends DetailedReformPermissions[QueryReform](permissions)(wrap, new TermQueryReform(_)(_))
//		   with TopDownQueryReform
//	{
//		override def reform(implicit spelling :SQLSpelling) :Reform = new TermReform(permissions)(identity)
//		override def typeName :String = "TermQueryReform"
//	}
//	private class TermQueryReform(wrap :QueryReform => QueryReform)
//		extends BaseDecorable[QueryReform](wrap, new TermQueryReform(_))
//		   with ForbiddenReform[QueryReform]
//		   with QueryReform
//	{
//		override def reform(implicit spelling :SQLSpelling) :Reform = TermReform
//		override def typeName :String = "TermQueryReform"
//	}
//
//	private val TermQueryReform :QueryReform = new TermQueryReform(identity)


	/** A special `Reform`, which is normally created without any permissions. However, in addition to
	  * [[net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform.TermReform.permissions permissions]] property,
	  * there are also
	  * [[net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform.TermReform.potentialPermissions potentialPermissions]],
	  * which are initialized in a way that `permissions` normally would. All `prohibit` methods work on both permission
	  * sets, but `allow` methods have effect only on `permissions`, and grants a permission only if it is also
	  * allowed by `potentialPermissions`. In particular, this means that `allowAll` will reinstate
	  * the permission set which would exist if `permissions` were original set to the same values
	  * as `potentialPermissions`. This, in turn, allows to surgically limit all reforming only to expressions
	  * wrapped in a [[net.noresttherein.oldsql.sql.mechanics.UnalignedSQL UnalignedSQL]].
	  */
	//todo: make this a decorator, so that HolisticQueryReform can be used with any Reform.
	private class TermReform(val potentialPermissions :Permissions, override val permissions :Permissions = NoReform)
	                        (wrap :Reform => Reform)
		extends TunedReform(permissions)(wrap, new TermReform(potentialPermissions, _)(_))
           with OptimisticReform
	{
		//We might be forced to reform an SQLTerm with another SQLTerm, which would get us nowhere, so cancel validation.
		override def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
		                       (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
		                       (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                                 spelling :SQLSpelling) :Boolean =
			true

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult :SQLTransformation[left.Subject, U],
		                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling) =
			fallback(left, right)

		override def prohibit(permissions :Permissions) :Reform =
			if (permissions(this.permissions) && permissions(potentialPermissions))
				self
			else
				new TermReform(potentialPermissions & permissions, this.permissions & permissions)(wrap)

		override def allow(permissions :Permissions) :Reform =
			if (this.permissions(permissions & potentialPermissions))
				self
			else
				new TermReform(this.permissions | potentialPermissions & permissions, potentialPermissions)(wrap)

		override def permit(permissions :Permissions) :Reform =
			if (permissions == this.permissions && permissions == potentialPermissions)
				self
			else
				new TermReform(permissions, permissions)(wrap)


		override def typeName = "TermReform"
	}

	object TermReform {
		def apply() :Reform = prototype
		def apply(permissions :Permissions) :Reform = new TermReform(permissions, NoReform)(identity)

		def unapply(reform :Reform) :Opt[Permissions] = reform.underlying match {
			case tr :TermReform => Got(tr.potentialPermissions)
			case _ => Lack
		}
		private[this] val prototype = new TermReform(MayReform, NoReform)(identity)
	}

//	private val TermReform = new TermReform(NoReform)(identity)
}





/** A wrapper class over an expression which cannot be reformed using
  * an [[net.noresttherein.oldsql.sql.mechanics.Alignment Alignment]]. It is returned from
  * [[net.noresttherein.oldsql.sql.SQLExpression.ReorderingTemplate.realign realign]] method of
  * `SQLExpression` implementations which do not report all - or any - columns for a global alignment
  * in their [[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns potentialColumns]] method.
  * As the final step of [[net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform HolisticQueryReform]],
  * [[net.noresttherein.oldsql.sql.mechanics.HolisticQueryReform.TermReform TermReform]] is used to reform
  * these expressions by assigning them forms of other expressions they are aligned with. As we do not want
  * to modify expressions included in `Alignment`, `TermReform` has no reforming permissions. The role of this class
  * is to grant those permissions, and delegate to the wrapped class normally, so that reforming can be applied
  * surgically only to selected expressions.
  */
class UnalignedSQL[-F <: RowProduct, -S >: Grouped <: Single, V](override val value :SQLExpression[F, S, V])
	extends DecoratedSQL[F, S, V] with UnalignedSQLTemplate[F, S, V, SQLExpression]
{
	protected override def decorate[E <: RowProduct, C >: Grouped <: Single, X]
	                               (e :SQLExpression[E, C, X]) :SQLExpression[E, C, X] =
		e match {
			case unaligned :UnalignedSQL[E, C, X] => unaligned
			case _ => new UnalignedSQL(e)
		}
}


object UnalignedSQL {
	trait UnalignedSQLTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                           +Same[-f <: RowProduct, -s >: Grouped <: Single, v]
		                             <: ConvertibleSQL[f, s, v, ({type E[X] = Same[f, s, X]})#E]]
		extends FloatingDecoratedSQLTemplate[F, S, V, Same]
	{ this :Same[F, S, V] =>
		protected override def reapply[E <: RowProduct, C >: Grouped <: Single]
		                              (e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
			e

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                              (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                              (implicit leftResult  :SQLTransformation[V, U],
		                                        rightResult :SQLTransformation[V2, U], spelling :SQLSpelling)
				:(leftResult.Expression[F, S, Same[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		//todo: adjust the moment when we reform so that if other is an adapter it unwraps itself,
		// but nobody does any real reforming. This might need increasing the number o passes by another round.
		//We want other adapters to unwrap themselves, but avoid forms reforming against us
		if (!passCount.gaveTwoChances)
			passReform(other)(reform, passCount)
		else
			reform match {
//				case TermReform(_) => other match {
//					case _ :UnalignedSQL[F2, S2, V2] =>
//						//No sense in attempting to reform, as other does not have the definite shape.
//						/* Consider: we might go on with reforming as in the following case.
//						 *  This would allow us to have a proper compatibility test in TermReform, rather than agreeing
//						 *  for anything. On the second hand, it may cause unnecessary recreation of the whole query.
//						 */
//						super.reform(other)(reform, passCount)
//					case _ =>
//						/* Consider: we are permitting here only operations allowed by select operators of the
//						 *  compound select(s) containing the select with this expression as a subexpression.
//						 *  However, maybe we should treat terms (other than MappingTerm) specially,
//						 *  on an all-or-nothing basis, and always take the form of the other expression.
//						 */
//						/* Consider: ideally, we would like to reform only once, and remove ourselves
//						 *  from the transformation stack and the new expression/query in the process.
//						 *  This, aside from avoiding rebuilding of the whole query multiple times,
//						 *  would have the benefit of reforming against the first eligible expression,
//						 *  so taking on the form from the select clause of the closest select.
//						 *  However, it is possible that other is a RecordSQL, which still has an UnalignedSQL
//						 *  as one of its elements. I am not sure what's best: should we perform validation here,
//						 *  and remove ourselves only if it passes?
//						 */
////						reform.allowReformLeft(value, other)
//						super.reform(other)(reform.allowReformLeft, passCount)
//				}
				case TermReform(_) =>
					/* Note: we are allowing reforming here even if other is an UnalignedSQL.
					 * While this is counterproductive, as this shape is unlikely to be the definite one determined
					 * by QueryAligner, there is a possibility that we are aligned *only* with UnalignedSQL instances,
					 * and thus cannot take shape from anything proper. In that case we should settle for just
					 * sharing whichever form we can among all aligned expressions.
					 * Also, see musings in the commented out code above on whether we can replace ourselves
					 * with a reformed result completely.
					 */
					super.reform(other)(reform.allowReformLeft, passCount)
				case _ => super.reform(other)(reform, passCount)
			}

		override def toString :String = "Unaligned("  + value + ")"
	}
}
