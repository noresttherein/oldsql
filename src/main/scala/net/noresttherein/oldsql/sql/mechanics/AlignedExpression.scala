package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.exceptions.{IllegalExpressionException, MismatchedExpressionsException}
import net.noresttherein.oldsql.morsels.Decorable.BaseDecorable
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.slang.saferCasting
import net.noresttherein.oldsql.sql.{ColumnSQL, CompoundSelect, Query, RowProduct, SQLExpression, Select}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.Query.{AbstractQuery, QueryTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ArithmeticSQL, ChainSQL, ComponentSQL, CompoundSelectColumn, CompoundSelectSQL, ConditionSQL, DecoratedColumnSQL, DecoratedSQL, FunctionSQL, LValueSQL, QuerySQL, SQLTerm, SelectSQL, TransformedSQL}
import net.noresttherein.oldsql.sql.ast.DecoratedSQL.FloatingDecoratedSQLTemplate
import net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledValueSQL
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.ast.TransformedSQL.TransformedSQLConvertingTemplate
import net.noresttherein.oldsql.sql.mechanics.AlignedExpression.AlignedExpressionTemplate
import net.noresttherein.oldsql.sql.mechanics.Reform.{OptimisticReform, PassCount, TunedReform}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{DetailedReformPermissions, ForbiddenReform, Permissions}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.NoReform






private object TerminalSQL {
	def unapply(e :SQLExpression.__) :Boolean = e match {
		case _ :LValueSQL.__ | _ :SQLTerm[_] | _ :LabeledValueSQL.__ | _ :ChainSQL.__ |
		     _ :FunctionSQL.__ | _ :ConditionSQL.__ | _ :ArithmeticSQL.__
		=>
			true
		case _ =>
			false
	}
}




/** A `Reform` which doesn't perform any actual reforming -
  * its [[net.noresttherein.oldsql.sql.mechanics.SQLAligner.default default]] method always throws
  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]],
  * and [[net.noresttherein.oldsql.sql.mechanics.SQLAligner.compatible compatible]] returns `true` -
  * but instead descends down both expressions, possibly branching in various
  * [[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]] composite expressions, in an attempt to find the deepest
  * expressions paired with each other, and wraps them
  * in an [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression AlignedExpression]].
  * Not always it is the deepest/non decomposable expression that is wrapped: for example,
  * it is impossible to wrap a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
  * under a [[net.noresttherein.oldsql.sql.ast.LValueSQL.ConvertedLValueSQL ConvertedLValueSQL]],
  * in which case the deepest expression which can be safely substituted with any `SQLExpression` is wrapped.
  * If the reform encounters an existing `AlignedExpression`, it will align it with a corresponding other subexpression,
  * but also recursively attempt to decompose the expression wrapped by `AlignedExpression` regardless
  * (because, for example, it may be a [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]] which was previously
  * aligned with a `ComponentSQL`, but now faces another `IndexedSQL`).
  *
  * This pseudo reform is used as a preliminary step in aligning of multiple expressions at the same time,
  * in order to possess full knowledge about columns of other expressions before committing to a particular
  * choice of modifications designed to unify the shapes of all aligned expressions.
  */
class SQLAligner(permissions :Permissions)(wrap :Reform => Reform)
	extends TunedReform(permissions)(wrap, new SQLAligner(_)(_))
{
	private def align[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
	                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
	                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                            spelling :SQLSpelling)
			:(leftResult.Expression[LF, LS, SQLExpression[LF, LS, U]], rightResult.Expression[RF, RS, SQLExpression[RF, RS, U]]) =
	{
		val alignment = new Alignment(left, right)(leftPermissions, rightPermissions)
		val l = AlignedExpression(left, alignment)
		val r = AlignedExpression(right, alignment)
		(leftResult(l), rightResult(r))
	}

	/** Descends down both expressions, attempting to substitute subexpressions for `AlignedExpression`
	  * as deep within each expression as possible, so that other `Reform` traversing the expressions will always
	  * encounter a pair of `AlignedExpression`s before hitting the end of recursion.
	  */
	override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
	                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
	                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
	                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                            spelling :SQLSpelling)
			:(leftResult.Expression[LF, LS, LE[U]], rightResult.Expression[RF, RS, RE[U]]) =
	{
		//Can we safely substitute e for e.transformation(AlignedExpression(e.value, _)) ?
		def isAlignable(e :TransformedSQL.__) :Boolean = {
			val transformation = e.transformation
			(transformation.isUniversal || transformation.isColumnOnly) &&
				(!transformation.isTypeDependent || (e.value match {
					case column :ColumnSQL.__ => !column.isColumnConvertingSpecific
					case value => !value.isConvertingSpecific
				})
			)
		}
		//A much weaker variant of isAlignable, used when otherwise we would have to throw an exception anyway
		def isSpecific(e :SQLExpression.__) :Boolean = e match {
			case column :ColumnSQL.__ => column.isSpecificColumn
			case _ => e.isSpecific
		}
		def isRecoverable(e :Exception) = e match {
			case _ :MismatchedExpressionsException | _ :IllegalExpressionException | _ :ClassCastException => true
			case _ => false
		}

		val reformed = (left, right) match {
			//AlignedExpression will try to deconstruct the opponent as much as possible and take care of reforming itself
			case (_, _ :AlignedExpression[RF, RS, RV]) => super.apply(left, right)
			case (_ :AlignedExpression[LF, LS, LV], _) => super.apply(left, right)

			case (l :TransformedSQL[LF, LS, x, LV], r :TransformedSQL[RF, RS, y, RV])
				if isAlignable(l) && isAlignable(r)
			=>
				try {
					super.apply(left, right) //first, try to inject AlignedExpression lower for both sides
				} catch {
					case e :Exception if isRecoverable(e)  =>
						try {
							val aligned = new AlignedExpression(l.value, leftPermissions)
							implicit val lResult = l.transformation andThen leftResult
							super.apply(aligned, right) //try to align left with an underlying expression of right
						} catch {
							case e :Exception if isRecoverable(e) =>
								try {
									val aligned = new AlignedExpression(r.value, rightPermissions)
									implicit val rResult = r.transformation andThen rightResult
									super.apply(left, aligned) //try to align right with an underlying expression of left
								} catch {
									case e :Exception if isRecoverable(e) =>
										align(left, right) //all things failed, so align on this level
								}
						}
				}
			case (l :TransformedSQL[LF, LS, x, LV], _) if isAlignable(l) =>
				try
					super.apply(left, right)
				catch {
					case e :Exception if isRecoverable(e) =>
						implicit val lResult = l.transformation andThen leftResult
						val aligned = new AlignedExpression(l.value, leftPermissions)
						super.apply(aligned, right)
				}
			case (_, r :TransformedSQL[RF, RS, x, RV]) if isAlignable(r) =>
				try
					super.apply(left, right)
				catch {
					case e :Exception if isRecoverable(e) =>
						implicit val rResult = r.transformation andThen rightResult
						val aligned = new AlignedExpression(r.value, rightPermissions)
						super.apply(left, aligned)
				}
			//break the recursion to avoid throwing a MismatchedExpressionsException which must happen
			case (TerminalSQL(), TerminalSQL()) =>
				align(left, right)
			case _ =>
				//Assuming both left and right are stacks of TransformedSQL, this code won't cover all pairs
				// on the stack not specifically convertible, but it will cover the lowest in each, which is enough.
				try
					super.apply(left, right)
				catch {
					//isSpecific proxies (unreliably) for 'its type matters'
					case e :Exception if isRecoverable(e) && !isSpecific(left) && !isSpecific(right) =>
						align(left, right)
				}
		}
		reformed.castFrom[
			(leftResult.Expression[LF, LS, SQLExpression[LF, LS, U]], rightResult.Expression[RF, RS, SQLExpression[RF, RS, U]]),
			(leftResult.Expression[LF, LS, LE[U]], rightResult.Expression[RF, RS, RE[U]]),
		]
	}

	override def compatible[LF <: RowProduct, LS >: Grouped <: Single, LV, RF <: RowProduct, RS >: Grouped <: Single, RV, U]
	                       (left :SQLExpression[LF, LS, LV], right :SQLExpression[RF, RS, RV])
	                       (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
	                                 spelling :SQLSpelling) :Boolean =
		true

	override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
	                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
	                    (implicit leftResult  :SQLTransformation[left.Subject, U],
	                              rightResult :SQLTransformation[right.Subject, U], spelling :SQLSpelling)
			:(leftResult.Expression[LF, Single, LValueSQL[LF, LM, U]], rightResult.Expression[RF, Single, LValueSQL[RF, RM, U]]) =
		throw new MismatchedExpressionsException( //todo: make the message lazy
			"Cannot wrap ComponentSQL expressions `" + left + "` and `" + right + "` in a AlignedExpression"
		)
}






/** A special purpose pseudo [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] which is not a real
  * SQL expression (and must not be used with other expressions to form SQL statements), but which instead
  * is a collection of expressions sharing the same, or similar, structure. It is an implementation artefact,
  * not featuring among public SQL expression types and does not even has its own case in
  * [[net.noresttherein.oldsql.sql.SQLExpression.SpecificExpressionVisitor ExpressionVisitor]]/[[net.noresttherein.oldsql.sql.SQLExpression.AnyExpressionVisitor AnyExpressionVisitor]].
  *
  * It is used in the first step of unifying the column lists of multiple expressions.
  * It depends on the reforming process flow to preliminarily validate structural compatibility of reformed expressions
  * by iteratively reforming the current unified result with the next expression, and collecting all corresponding
  * subexpressions in a `AlignedExpression`. This allows for a more global unification implementation than
  * attempting to fully reform expressions pairwise in a purely recursive manner.
  *
  * @see [[net.noresttherein.oldsql.sql.mechanics.SQLAligner]]
  */ //remember that the order of expressions is not the same as in the whole query because reforming can swap sides
///** A transparent expression wrapper attaching a list of other expressions of a compatible value type which the wrapped
//  * expression should be unified with.
//  */
private[sql] class AlignedExpression[-F <: RowProduct, -S >: Grouped <: Single, V]
                                    (override val value :SQLExpression[F, S, V], val alignment :Alignment)
	extends DecoratedSQL[F, S, V] with AlignedExpressionTemplate[F, S, V, SQLExpression]
{
	def this(value :SQLExpression[F, S, V], permission :Permissions)(implicit spelling :SQLSpelling) =
		this(value, new Alignment(value, permission))

	def aligned     :IndexedSeq[SQLExpression[Nothing, Grouped, _]] = alignment.aligned
	def permissions :Seq[Permissions] = alignment.permissions

//	protected override def adapt[X](conversion :SQLAdaptation[V, X]) :SQLExpression[F, S, X] =
//		decorate(conversion(value), alignment)

//	protected override def convert[X](conversion :SQLConversion[V, X]) :SQLExpression[F, S, X] =
//		adapt(conversion)
//		if (!conversion.isDerived || conversion.isIdentity) conversion(this)
//		else copy(conversion(value), alignment)


	protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		decorate(e, alignment)

	protected override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
		new AlignedColumn(e, alignment)


	protected override def align(alignment :Alignment) :SQLExpression[F, S, V] =
		AlignedExpression(value, alignment)

	override def name = "Aligned"
	override def toString :String =
		aligned.view.map("`" + _ + "`").mkString("`" + value + "` aligned { ", ", ", "}")
}


private[sql] class AlignedColumn[-F <: RowProduct, -S >: Grouped <: Single, V]
                   (override val value :ColumnSQL[F, S, V], override val alignment :Alignment)
	extends AlignedExpression[F, S, V](value, alignment)
	   with DecoratedColumnSQL[F, S, V]
	   with AlignedExpressionTemplate[F, S, V, ColumnSQL]
{
	protected override def align(alignment :Alignment) :ColumnSQL[F, S, V] =
		new AlignedColumn(value, alignment)
}




private[sql] object AlignedExpression {
	def apply[F <: RowProduct, S >: Grouped <: Single, V](e :SQLExpression[F, S, V], alignment :Alignment)
			:AlignedExpression[F, S, V] =
		e match {
			case column :ColumnSQL[F, S, V] => new AlignedColumn(column, alignment)
			case _ => new AlignedExpression(e, alignment)
		}

	trait AlignedExpressionTemplate[-F <: RowProduct, -S >: Grouped <: Single, V,
	                                +EC[-f <: RowProduct, -s >: Grouped <: Single, v]
	                                    <: ConvertibleSQL[f, s, v, ({ type E[X] = EC[f, s, X] })#E]]
//		extends AdaptedSQL[F, S, V, V]
		extends TransformedSQLConvertingTemplate[F, S, V, V, EC]
	{ self :EC[F, S, V] with AlignedExpressionTemplate[F, S, V, EC] =>
		protected def alignment :Alignment

		protected abstract override def reformTransformation[U](leftResult :SQLTransformation[V, U]) =
//				:SQLTransformation.Bound[V, U, leftResult.Expression] = //make method available to other instances
			super.reformTransformation(leftResult)

		protected def decorate[E <: RowProduct, C >: Grouped <: Single, U]
		                      (value :SQLExpression[E, C, U], alignment :Alignment) :AlignedExpression[E, C, U] =
			AlignedExpression(value, alignment)

		protected def align(alignment :Alignment) :EC[F, S, V]

		protected def align[F2 <: RowProduct, S2 >: Grouped <: Single, V2, EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
		                   (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
		                   (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
	                                 spelling :SQLSpelling)
				:(leftResult.Expression[F, S, EC[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
		{
			val alignment = this.alignment.updated(other, reform.rightPermissions)
			val left = align(alignment)
			val right = decorate(other, alignment).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
			(leftResult(left), rightResult(right))
		}

		protected override def reform[F2 <: RowProduct, S2 >: Grouped <: Single, V2,
		                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
	                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
	                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
	                                           spelling :SQLSpelling)
				:(leftResult.Expression[F, S, EC[F, S, U]], rightResult.Expression[F2, S2, EC2[U]]) =
			//Give other all chances to reform as long as we can default to our implementation, because we want it
			// to go as deep as possible before we create a new AlignedExpression
			if (!passCount.lastChance) {
				try
					self.passReform(other)(reform, passCount)(leftResult, rightResult, spelling)
				catch {
					case _ :MismatchedExpressionsException | _ :IllegalExpressionException | _ :ClassCastException
					if (rightResult.isUniversal || rightResult.isColumnOnly) &&
						!other.isSpecific || !rightResult.isSpecific && !rightResult.isTypeDependent ||
							other.isInstanceOf[ColumnSQL.__] && !other.asInstanceOf[ColumnSQL.__].isSpecificColumn ||
							!rightResult.isSpecificColumn
					=>
						align(other)(reform, passCount)
				}
			} else
				other match {
					/* The fact that we are aligned with other, doesn't mean that both this and other
					 * can be deconstructed and aligned on a lower level, too, for example if both are record
					 * expressions previously aligned with the same ComponentSQL.
					 */
					case aligned :AlignedExpression[F2, S2, V2] =>
						val alignment = this.alignment :++ aligned.alignment
						val left  = align(alignment)
						val right = decorate(aligned.value, alignment)
						try
							reform(value, aligned.value)(
								reformTransformation(leftResult), right.reformTransformation(rightResult), spelling
							).castFrom[
								(leftResult.Expression[F, S, SQLExpression[F, S, U]],
									rightResult.Expression[F2, S2, SQLExpression[F2, S2, U]]),
								(leftResult.Expression[F, S, EC[F, S, U]], rightResult.Expression[F2, S2, EC2[U]])
							]
						catch {
							case _ :MismatchedExpressionsException | _ :IllegalExpressionException | _ :ClassCastException =>
								val cast = right.castFrom[SQLExpression[F2, S2, V2], ConvertibleSQL[F2, S2, V2, EC2]]
								(leftResult(left), rightResult(cast))
						}

					case _ if (rightResult.isUniversal || rightResult.isColumnOnly) && !other.isSpecific ||
					           !rightResult.isSpecific && !rightResult.isTypeDependent ||
					           other.isInstanceOf[ColumnSQL.__] && !other.asInstanceOf[ColumnSQL.__].isSpecificColumn ||
					            !rightResult.isSpecificColumn
					=>
						align(other)(reform, passCount)
					case _ => try
						align(other)(reform, passCount)
					catch {
						case _ :IllegalExpressionException | _ :ClassCastException => reform.fallback(self, other)
					}
				}

	}
}




//private[sql] object AlignedExpression {
//	val BottomUp :Reform = new Aligner
//	val TopDown  :Reform = new TopDownAligner
//	def Realigner[V](selectClause :SQLShape[V]) :Reform = new Realigner(selectClause)
//
//
//	/** A preprocessing reform used in the process of unifying the shapes of more than two expressions.
//	  * It relies on the existing reforming implementations in [[net.noresttherein.oldsql.sql.ast.CompositeSQL composite]]
//	  * SQL expressions, which compare the structure of two expressions, recursively reform each corresponding
//	  * subexpression pairs, and recreate themselves while substituting the reformed results for paired subexpressions.
//	  * Other expression types, leaves in the expression tree,
//	  * in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]]
//	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]], are handled differently:
//	  * instead of performing any reforming, the `Reform` substitutes each
//	  * with an [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression AlignedExpression]] wrapping the original
//	  * expression, but also listing subexpressions of all seen 'root' expressions aligned with the given expression.
//	  *
//	  * When reforming a [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]
//	  * or a [[net.noresttherein.oldsql.sql.Query Query]] pair, it bypasses the normal process of reforming member
//	  * queries relying on `query.`[[net.noresttherein.oldsql.sql.Query.reformed reform]], instead recursively processing
//	  * left and right subqueries of
//	  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]/[[net.noresttherein.oldsql.sql.Query.CompoundSelect CompoundSelect]]
//	  * and unifying `left.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]]
//	  * with `right.selectClause` afterwards. As the result, `selectClause` of the reformed query,
//	  * as well as all its member ''selects'' and ''compound selects'' subexpressions, is an `SQLExpression`
//	  * reflecting the shared structure of the ''select'' clauses of its subexpressions. More specifically,
//	  * it is the outer (composite) part of the expression tree shared by all those ''select'' clauses (if any),
//	  * with differing subexpressions replaced with an `AlignedExpression` instance, carrying all aligned subexpressions
//	  * of the covered ''select'' clauses, as well as an expression resulting from substituting the differing
//	  * part in the reformed expressions with the most informative between the aligned subexpressions.
//	  * The precedence is: [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]],
//	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]], [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]],
//	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] followed by others.
//	  *
//	  * The [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.aligned aligned]] ''select'' clauses
//	  * are accompanied with [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.permissions permissions]] flags,
//	  * reflecting the permissions given to a `Reform` for reforming the corresponding expression
//	  * on the `aligned` list, depending on [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]s
//	  * used to combine the queries. They are not necessarily in the same order as ''selects'' from which they came
//	  * on the `query.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.constituents constituents]], but rather
//	  * in the order determined by the reforming algorithm, which at any level may swap the left and right expressions.
//	  *
//	  * Note that after reforming a query, only its ''select'' clause contains `AlignedExpression`s listing ''all''
//	  * member ''selects'' of the query, with all its subqueries, in particular member ''selects'' themselves,
//	  * being aligned only with a subset of other ''selects''.
//	  */ //todo: this should be also a QueryReform
//	private class Aligner(override val permissions :Permissions)
//	                     (wrap :Reform => Reform,
//	                      constructor :(Permissions, Reform => Reform) => Reform =
//	                        new Aligner(_)(_))
//		extends TunedReform(permissions)(wrap, constructor)
//		   with PreprocessorReform with Reform
//	{
//		def this() = this(MayReform)(identity)
//
//		override lazy val swap =
//			if ((this eq self) && isSymmetrical)
//				self
//			else
//				new Aligner(permissions.swap)(wrap, constructor) {
//		            override lazy val swap = Aligner.this.self
//	            }.self
//
//
///*
//		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
//		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
//		                   F <: RowProduct, B >: Grouped <: Single, Y,
//		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
//		                  (left :L[X], right :R[Y])
//		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
//				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
//*/
//
//		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
//		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
//		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
//		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
//		                            spelling :SQLSpelling)
//				:(leftResult.Expression[LF, LS, LE[U]], rightResult.Expression[RF, RS, RE[U]]) =
//			(left, right) match {
//				case (l :AlignedExpression[LF, LS, LV], r :AlignedExpression[RF, RS, RV]) =>
//					val all = l.aligned ++ r.aligned
//					val ps  = l.permissions ++ r.permissions
//					val alignment   = new Alignment(all, ps, spelling.scope)
//					val leftRes  = leftResult(new AlignedExpression(l.value, alignment))
//					val rightRes = rightResult(new AlignedExpression(r.value, alignment))
//					(leftRes, rightRes)
//				//AdapterSQL will implement its reform before AlignedExpression gets to
//				case (l :AlignedExpression[LF, LS, LV], r :AdaptedSQL[RF, RS, x, RV]) =>
//					super[Reform].apply[LF, LS, LV, LE, RF, RS, RV, RE, U](l, r)(leftResult, rightResult, spelling)
//				case (l :AlignedExpression[LF, LS, LV], _) =>
//					val all = l.aligned :+ right
//					val ps  = l.permissions :+ rightPermissions
//					val alignment = new Alignment(all, ps, spelling.scope)
//					val leftRes   = leftResult(new AlignedExpression(l.value, alignment))
//					val rightRes  = rightResult(new AlignedExpression(right, alignment))
//					(leftRes, rightRes)
//
//				case (l :AdaptedSQL[LF, LS, x, LV], r :AlignedExpression[RF, RS, RV]) =>
//					super[Reform].apply[LF, LS, LV, LE, RF, RS, RV, RE, U](l, r)(leftResult, rightResult, spelling)
//				case (_, r :AlignedExpression[RF, RS, RV]) =>
//					val all = left +: r.aligned
//					val ps  = leftPermissions +: r.permissions
//					val alignment = new Alignment(all, ps, spelling.scope)
//					val leftRes   = leftResult(new AlignedExpression(left, alignment))
//					val rightRes  = rightResult(new AlignedExpression(r.value, alignment))
//					(leftRes, rightRes)
//				case _ => super.apply(left, right)
//			}
//
//		//This seems impossible to implement, because we have no idea what type the enclosing expressions expect:
//		// if it's anything but an SQLExpression (or ColumnSQL, I guess), we can neither wrap it before, nor after
//		// applying the conversion. We must try to instead wrap the whole root expressions in AlignedExpression
//		// and somehow push them down.
///*
//		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
//		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
//		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
//		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
//		                               spelling :SQLSpelling)
//				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
//		{
////			val unifiedLeft  = left.to(compat.left)
////			val unifiedRight = right.to(compat.right)
//			val expressions = PassedArray.two(left, right)
//			val permissions = PassedArray.two(leftPermissions, rightPermissions)
//			val alignment = new Alignment(expressions, permissions, spelling.scope)
//			val l = new AlignedExpression(left, alignment)
//			val r = new AlignedExpression(right, alignment)
//			(l, r)
//		}
//*/
//
//		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
//		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//		                    (implicit leftResult  :SQLTransformation[LM[Unit]#Subject, U],
//		                              rightResult :SQLTransformation[RM[Unit]#Subject, U], spelling :SQLSpelling)
//				:(leftResult.Expression[LF, Single, LValueSQL[LF, LM, U]], rightResult.Expression[RF, Single, LValueSQL[RF, RM, U]]) =
//			fallback(left, right)(leftResult, rightResult, spelling)
//
//	}
//
//
//
//	private class TopDownAligner(override val permissions :Permissions)
//	                            (wrap :Reform => Reform)
//		extends Aligner(permissions)(wrap, new TopDownAligner(_)(_)) //with TopDownReform
//	{
//		def this() = this(MayReform)(identity)
//
//		override lazy val swap =
//			if ((this eq self) && isSymmetrical)
//				self
//			else
//				new TopDownAligner(permissions.swap)(wrap) {
//					override lazy val swap = TopDownAligner.this.self
//				}.self
//	}
//
//
//	/** A pseudo `Reform` hijacking the query and expression traversing mechanism in order to align
//	  * the ''select'' clauses of all member ''selects'' in a query with the ''select'' clause of the whole query.
//	  * It is used immediately after [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.Aligner Aligner]],
//	  * in which recursive reforming of individual selects created a layout expression
//	  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
//	  * for the whole query, which now contains in the positions of aligned leaf expressions an instance
//	  * of [[net.noresttherein.oldsql.sql.mechanics.Alignment Alignment]] possessing information
//	  * about aligned subexpressions in every ''select''.
//	  *
//	  * `Realigner` is then used to unify the whole ''compound select'', and when reaching individual ''selects'',
//	  * unifies their [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]]
//	  * with the layout [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
//	  * of the whole ''compound select'', which
//	  *
//	  * the `selectClause` of each member ''select'' (as the `left` argument)
//	  * and the layout `selectClause` of the whole ''compound select'', listing the aligned subexpression information
//	  * present in [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
//	  * of the whole ''compound select'' (on the right), and reforms the left expression to conform to the column set
//	  * defined by `Alignment`.
//	  *
//	  * instances present in all [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression AlignedExpression]]
//	  * subexpressions of the given `selectClause` to matched `AlignedExpression`s within ''select'' clauses
//	  * of each ''select'' in the query. This is needed because after non-reforming by `Aligner`, ''select'' clauses
//	  * of all subqueries in the 'reformed' query have only partial information, not including all ''selects''
//	  * from the whole query. Additionally, thanks to this the needed preprocessing data is calculated only once,
//	  * in the universally shared `Alignment`.
//	  */
//	private class Realigner[S](selectClause :SQLExpression[Nothing, Grouped, S],
//	                           wrap :Reform => Reform = identity[Reform])
//		extends AbstractReform(wrap, new Realigner(selectClause, _))
//		   with AbstractValidatorReform with PreprocessorReform with Reform
//	{
////		override val swap = super[AbstractReform].swap
//
///*
//		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
//		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
//		                   F <: RowProduct, B >: Grouped <: Single, Y,
//		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
//		                  (left :L[X], right :R[Y])
//		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
//				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
//*/
//		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
//		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
//		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
//		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
//		                            spelling :SQLSpelling)
//				:(leftResult.Expression[LF, LS, LE[U]], rightResult.Expression[RF, RS, RE[U]]) =
//			???
////			(left, right) match {
////				case (l :AlignedExpression[LF, LS, LV], r :AlignedExpression[RF, RS, RV]) => (
////					leftResult(new AlignedExpression(l.value, r.alignment)),
////					rightResult(right)
////				)
////				//we should use reform on anything which will recurse down, as the AlignedExpression may be lower
////				case (l :AdaptedSQL[LF, LS, x, LV], r :AlignedExpression[RF, RS, RV]) => super[Reform].apply(left, right)
////				case (_, r :AlignedExpression[RF, RS, RV]) => (
////					leftResult(new AlignedExpression(left, r.alignment)),
////					rightResult(right)
////				)
////				case (l :AlignedExpression[LF, LS, LV], r :AdaptedSQL[RF, RS, x, RV]) => super.apply(l, r)
////				case (l :AlignedExpression[LF, LS, LV], _) => //this shouldn't happen, but whatever
////					(leftResult(left), rightResult(right))
////				case _ =>
////					super.apply(left, right)
////			}
//
//		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
//		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
//		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
//		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
//		                               spelling :SQLSpelling)
//				:(leftResult.Expression[LF, LS, LE[U]], rightResult.Expression[RF, RS, RE[U]]) =
//			???
//
//		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
//		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//		                    (implicit leftResult  :SQLTransformation[LM[Unit]#Subject, U],
//		                              rightResult :SQLTransformation[RM[Unit]#Subject, U], spelling :SQLSpelling)
//				:(leftResult.Expression[LF, Single, LValueSQL[LF, LM, U]], rightResult.Expression[RF, Single, LValueSQL[RF, RM, U]]) =
//			???
//
//		//we leave the default fallback simply returning the two expressions
///*
//
//		override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) :Select[P, V] =
//			query.selectOther(
//				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
//					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
//				)._1
//			)
//
//		override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) :SelectSQL[F, V] =
//			query.selectOther(
//				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
//					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
//				)._1
//			)
//
//		override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) :TopSelectSQL[V] =
//			query.selectOther(
//				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
//					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
//				)._1
//			)
//
//		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] = {
//			val inOperand = spelling.inOperand
//			val l = left.apply(query.left)(inOperand)
//			val r = right.apply(query.right)(inOperand)
//			CompoundSelect.reformed(l, query.operator, r, query.selectClause)(this)
//		}
//
//		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
//		{
//			val inOperand = spelling.inOperand
//			val l = left.apply(query.left)(inOperand)
//			val r = right.apply(query.right)(inOperand)
//			CompoundSelectSQL.reformed(l, query.operator, r, query.selectClause)(this)
//		}
//*/
//	}
//
//	/* Things to do after running Realigner on a query:
//	 *   1. All cases of ComponentLValueSQL (and, optionally, EditedLValueSQL and any DecoratorSQL over them)
//	 *      directly under an AlignedExpression should be replaced with an expression matching the column order
//	 *      in the Alignment. This should involve first:
//	 *      1. Including/excluding columns present on the list;
//	 *      1. Verifying if the current alignment.scope.defaultColumns is a (sparse) subsequence of alignment.columnOrder;
//	 *      1. If not, either using ComponentSQL.reorder to create a custom instance, or including the changes in the next point;
//	 *      1. Wrapping the whole thing in an expression wrapper squeezing null columns between the component's columns
//	 *         and being able to add null columns when reforming.
//	 *      For reordering, we have RearrangedSQL. It can also be used to add initial null columns,
//	 *      but cannot do so when reforming, as it cannot match columns of its value (i.e, the component)
//	 *      to another expression, because it knows nothing about its type. We would either have to create
//	 *      a special wrapper subclass for every expression type which can be spliced with null columns,
//	 *      or incorporate Reform.mayAddNull handing to the types itself. The latter could be attempted
//	 *      with using as export Mapping a special implementation with mock columns, although it would
//	 *      make it not isomorphic with the nominal mapping, and thus incompatible with current implementations
//	 *      of all methods which compare if component expressions are compatible
//	 *      based on mapping isomorphism/homomorphism. The former is potentially quite a lot of work,
//	 *      duplicating existing functionality to a degree, such as more simple reforming. Hmm.
//	 *
//	 *   1. Using the normal (exponential) query reforming algorithm to reform all select clauses, without altering
//	 *      already reformed component expressions columns in other ways than adding null columns.
//	 *      This cannot be a global Reform permission setting, as we should allow of reforming
//	 *      all other expression types (permissions permitting). For this we need either
//	 *      a special Reform implementation/decorator, or an expression wrapper, such as one mentioned
//	 *      in the previous point, which will set the permissions as required.
//	 *
//	 *   1. Do something for IndexedSQL reforming, as we certainly could add null columns and reorder,
//	 *      but this has the same problems as ComponentSQL.
//	 */
//
//
///*
//	class PreliminaryAligner(override val mayExcludeLeft :Boolean, override val mayIncludeLeft :Boolean,
//	                         override val mayReorderLeft :Boolean, override val mayAddNullLeft :Boolean)
//	                        (override val mayExcludeRight :Boolean, override val mayIncludeRight :Boolean,
//	                         override val mayReorderRight :Boolean, override val mayAddNullRight :Boolean)
//	                        (wrap :ExpressionReform => ExpressionReform)
//		extends TunedReform[ExpressionLValue](mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft)(
//		                                        mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)(
//		                                        wrap, new Aligner(_, _, _, _)(_, _, _, _)(_))
//		   with PreprocessorReform with TopDownReform with Reform
//	{
//		def this() = this(true, true, true, true)(true, true, true, true)(identity)
//
//		override def fallback[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
//		                     (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
//		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
//				:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
//			(left, right) match {
//				case (group :GroupedExpressions[X], _) =>
////					val l = left.to(compat.left)
//					val r = new AlignedExpression(right.to(compat.right), group.expressions.map(_.to(compat.left)), ???, spelling.scope)
//					(left.to(compat.left), r)
//				case (_, group :GroupedExpressions[Y]) =>
//					val l = new AlignedExpression(left.to(compat.left), group.expressions.map(_.to(compat.right)), ???, spelling.scope)
//					(l, right.to(compat.right))
//				case (_ :CompositeSQL[L, A, X], _) =>
//					super[Reform].apply(left, right)
//				case (_, _ :CompositeSQL[R, B, Y]) =>
//					super[Reform].apply(left, right)
//				case _ => //this should not happen
//					super[Reform].apply(left, right)
////					val aligned = new GroupedExpressions(PassedArray :+ left.to(compat.left) :+ right.to(compat.right))
////					(aligned, aligned)
//		}
//
//	}
//*/
//
//}






/*
private[sql] final class ExpressionAlignment[T](val expressions :Seq[SQLExpression[Nothing, Grouped, T]],
                                                val permissions :Array[Int], scope :SpellingScope = SelectScope)
	extends ErrorSQL[T]
{
	val (components, mayReorder) = {
		val (_1, _2) = expressions.view.zip(permissions).collect {
			case (c :ComponentLValueSQL[_, MappingAt @unchecked, _], permission) =>
				(c.component, (permission & MayReorder) == MayReorder)
		}.unzip
		(_1 to PassedArray, _2 to PassedArray)
	}

	/** Maps all columns from any mapping which are aligned with another column into an identifier of
	 * the class of abstraction to which they belong in the relation of 'corresponds to in terms of Mapping.intersect'. */
	val alignmentIds :Map[TypedColumn[_, _], Integer] = {
		var i = components.length
		var res = Map.empty[TypedColumn[_, _], Integer]
		var nextToken = 0
		while (i > 0) {
			i -= 1
			var j = i
			while (j > 0) {
				j -= 1
				components(j).mapping intersect components(i).mapping foreach { case (extract1, extract2) =>
					val id1 = res.getOrElse(extract1.export, null)
					val id2 = res.getOrElse(extract2.export, null)
					if ((id1 ne null) && (id2 ne null)) {
						if (id1 ne id2)
							throw new MismatchedExpressionsException(
								expressions.mkString("Cannot unify expressions ", ", ", ": column ") + extract1 + " of "
								+ components(j).mapping + " and column " + extract2 + " of " + components(i).mapping
								+ components.map(_.mapping).mkString(
									" correspond to different columns in one of the mappings ", ", ", "."
								)
							)
					} else if (id1 ne null)
						res += (extract2.export, id1) +=
							(components(i).origin.anchored.export(extract2.export), id1)
					else if (id2 ne null)
						res += (extract1.export, id2) +=
							(components(j).origin.anchored.export(extract1.export), id2)
					else {
						val id = java.lang.Integer.valueOf(nextToken)
						nextToken += 1
						res += (extract1.export, id) += (extract2.export, id) +=
							(components(j).origin.anchored.export(extract1.export), id) +=
							(components(i).origin.anchored.export(extract2.export), id)
					}
				}
			}
		}
		res
	}

	/** The best column order for components, i.e. the one with a highest likelihood of a successful unification.
	  * Columns are represented here by identifiers linking aligned columns of all mappings:
	  * if `alignmentIds(column) == id`, then the column should come after all columns which in `columnOrder`
	  * come before `columnOrder.indexOf(id)` and before all columns which come in this sequence after that index.
	  * The order is defined primarily by the order the columns in
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.allColumns allColumns]] and
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]
	  * for component expressions whose columns cannot be reordered. Aside from the latter, a preference is given
	  * to the order in more specific components
	  * (i.e., if `m1 `[[net.noresttherein.oldsql.schema.Mapping.submappingOf submappingOf]]` m2`,
	  * then we start defining the order with `m1`.
	  */
	val columnOrder :IndexedSeq[Integer] = {
		//Determine the best ordering of columns in all component expressions. Start with those which cannot be reordered.
		val alignedColumnsCount = alignmentIds.size
		//precedes(i * alignmentIds.size + j) ==
		// == 1 if column with id i comes before column with id j in one of the components which cannot be reordered.
		// == 2 as above, but both columns are default
		val precedence = {
			val res = new Array[Int](alignedColumnsCount * alignedColumnsCount)
			var i = components.length
			while (i > 0) {
				i -= 1
				if (!mayReorder(i)) {
					val comp = components(i)
					def setPrecedence(columns :Unique[TypedColumn[_, comp.Origin]], value :Int) = {
						val ordered = columns.toIndexedSeq.flatMap(alignmentIds)
						var b = ordered.length
						while (b > 0) {
							b -= 1
							var a = b
							while (a > 0) {
								a -= 1
								res(ordered(a).intValue * alignedColumnsCount + ordered(b).intValue) = value
							}
						}
					}
					setPrecedence(scope.allColumns(comp.origin.anchored, comp.mapping), 1)
					setPrecedence(scope.defaultColumns(comp.origin.anchored, comp.mapping), 2)
				}
			}
			res
		}
		@inline def precedes(i :Int, j :Int) = precedence(i * alignedColumnsCount + j) >= 1
		@inline def stronglyPrecedes(i :Int, j :Int) = precedence(i * alignedColumnsCount + j) == 2

		/* The order of column ids in which the columns should appear, enforced by the order in which they appear
		 * in those components which cannot be reordered. Comes from topologically sorting the graph defined by
		 * matrix precedes: if precedes(i * alignmentIds.size + j) then forcedOrder.indexOf(i) <= forcedOrder.indexOf(j)
		 * It can still be changed, provided it does not invalidate the invariant (so, in practice, in sections
		 * in which none of the elements precede another). We don't fail with an exception here
		 * if a cycle is encountered, as we don't have the full or definite information yet - it is simply a heuristic.
		 */
		val ids = (0 until alignedColumnsCount) map Integer.valueOf
		val topologicalOrder = topologicalSort(ids, false) {
			(a, b) => precedes(a.intValue, b.intValue)
		}.toIndexedSeq

		//Now, try to refine the order starting with most specialized components (in terms of submappingOf).
		val componentsInExtendingOrder =
			topologicalSort(components, false) { _.mapping submappingOf _.mapping }.toIndexedSeq

		def merge(existingIds :IndexedSeq[Integer], component :ComponentSQL.*) = {
			val nextIds =
				scope.allColumns(component.origin.anchored, component.mapping).flatMap(alignmentIds.get).toIndexedSeq
			val existingIndices = existingIds.view.zipWithIndex to Map
			val nextIndices = nextIds.view.zipWithIndex to Map
			def rec(existingIdx :Int, nextIdx :Int, res :Builder[Integer, IndexedSeq[Integer]])
					:IndexedSeq[Integer] =
				if (existingIdx == existingIds.length) { //append all remaining columns from the current order
					var i = nextIdx; val end = nextIds.length
					while (i < end) {
						res += nextIds(i)
						i += 1
					}
					res.result()
				} else if (nextIdx == nextIds.length) { //append all remaining columns from the next component
					var i = existingIdx; val end = existingIds.length
					while (i < end) {
						res += existingIds(i)
						i += 1
					}
					res.result()
				} else if (existingIds(existingIdx) == nextIds(nextIdx))
					rec(existingIdx + 1, nextIdx + 1, res += existingIds(existingIdx))
				else {
					val existingId = existingIds(existingIdx)
					val nextId = nextIds(nextIdx)
					val existingInNext = nextIndices.getOrElse(existingId, -1)
					val nextInExisting = existingIndices.getOrElse(nextId, -1)
					if (nextInExisting < 0)      //adding a previously unseen column
						rec(existingIdx, nextIdx + 1, res += nextId)
					else if (existingInNext < 0) //current column does not occur in the next component
						rec(existingIdx + 1, nextIdx, res += existingId)
					else if (nextInExisting < existingIdx) //we have already added nextId, skip it
						rec(existingIdx, nextIdx + 1, res)
					else if (existingInNext < nextId)      //we have already added existingId, skip it
						rec(existingIdx + 1, nextIdx, res)
					else if ((nextIdx until existingInNext).forall(i => !precedes(nextIds(i), existingId)))
						//existingId in next can be moved before nextId
						rec(existingIdx + 1, nextIdx, res += existingId)
					else if ((existingIdx until nextInExisting).forall(i => !precedes(existingIds(i), nextId)))
						//nextId in existing can be moved before existingId
						rec(existingIdx, nextIdx + 1, res += nextId)
					else if ((nextIdx until existingInNext).forall(i => !stronglyPrecedes(nextIds(i), existingId)))
						//same as the second last case above, but a weaker precedence check
						rec(existingIdx + 1, nextIdx, res += existingId)
					else if ((existingIdx until nextInExisting).forall(i => !stronglyPrecedes(existingIds(i), nextId)))
						//same as the second last case above, but a weaker precedence check
						rec(existingIdx, nextIdx + 1, res += nextId)
					else if (precedes(existingId, nextId) && !precedes(nextId, existingId))
						//existingId comes before nextId, conflict involves only later columns
						rec(existingIdx + 1, nextIdx, res += existingId)
					else if (precedes(nextId, existingId) && !precedes(existingId, nextId))
						//nextId comes before existingId, conflict involves only later columns
						rec(existingIdx, nextIdx + 1, res += nextId)
					else if (stronglyPrecedes(existingId, nextId) && !stronglyPrecedes(nextId, existingId))
						//existingId comes strictly before nextId (both are default for some mapping)
						rec(existingIdx + 1, nextIdx, res += existingId)
					else if (stronglyPrecedes(nextId, existingId) && !stronglyPrecedes(existingId, nextId))
						//nextId comes strictly before existingId (both are default for some mapping)
						rec(existingIdx, nextIdx + 1, res += nextId)
					else  //conflict! pick any order, we'll report an error later, as this is just a heuristic
						rec(existingIdx + 1, nextIdx + 1, res += existingId += nextId)
				}
			rec(0, 0, PassedArray.newBuilder[Integer])
		}
		(topologicalOrder /: componentsInExtendingOrder)(merge)
	}

}
*/


