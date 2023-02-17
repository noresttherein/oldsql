package net.noresttherein.oldsql.sql.mechanics

import java.sql.JDBCType

import scala.annotation.tailrec
import scala.collection.immutable.Set

import net.noresttherein.oldsql.collection.{PassedArray, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.MismatchedExpressionsException
import net.noresttherein.oldsql.morsels.weightedTopologicalSort
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.slang.{foldingMethods, mappingMethods, saferCasting}
import net.noresttherein.oldsql.sql.{ColumnSQL, CompoundSelect, RowProduct, RowShape, Select, SQLExpression}
import net.noresttherein.oldsql.sql.SQLDialect.{SpellingScope, SQLSpelling}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope
import net.noresttherein.oldsql.sql.SQLExpression.{ConvertibleSQL, ConvertingTemplate, Grouped, Single, SQLShape}
import net.noresttherein.oldsql.sql.ast.{AdaptedSQL, ColumnMappingSQL, ComponentSQL, CompoundSelectSQL, DecoratedColumnSQL, DecoratedSQL, EditedComponentSQL, LValueSQL, MappingSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.ConvertedMappingSQL
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.Reform.{AbstractReform, AbstractValidatorReform, PassCount, TunedReform}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{MayReform, Permissions}






/** Information about aligned/corresponding, differing subexpressions of a list of a aligned expressions
  * (like individual ''select'' clauses in a ''compound'' select), with a special focus on
  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] instances.
  * Generally, the column order in components is clear: it is defined by
  * [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]].[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.applicableColumns allColumns]]
  * for a component, and overridden in case one of the aligned components is explicitly marked as non-reorderable
  * or non-reformable.
  *
  * When unifying the column sets and column order between components for subclasses of a common base class,
  * there is a need to establish the order between column sets unique to each subclass,
  * as none needs to be a supertype of all the others (include the columns of all the others).
  * Because we do not want the user to really fiddle with column order within a component,
  * we must try to align them ourselves, by inserting null columns in the right places between component columns,
  * potentially reordering the latter.
  */
class Alignment(
		/** Expressions aligned with `value`, including `value` itself. If this expression
		  * is a [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]]
		  * of a ''compound select'', then the list contains ''select'' clauses of all its
		  * [[net.noresttherein.oldsql.sql.Query.QueryTemplate.constituents constituent]]
		  * ''selects'', not necessarily in the same order. */
		val aligned :Seq[SQLExpression[Nothing, Grouped, _]],
		/** Reforming [[net.noresttherein.oldsql.sql.mechanics.Reform.permissions permissions]]
		  * for each of the expressions in `aligned`, in the same order. */
		val permissions :Seq[Permissions],
		val scope :SpellingScope = SelectScope
	)(implicit spelling :SQLSpelling)
{
//		def to[U](implicit lift :Lift[V, U]) = new Alignment(aligned.map(_.to[U]), permissions, scope)

	private lazy val (components, mayReorder) = {
		val (_1, _2) = aligned.view.zipFlatMap(permissions) { (expr, permission) =>
			//todo: we need to get rid of these classes and either perform deeper, recursive search for supported
			//  expressions, or guarantee that the reforming process already strips them.
			//  The latter would require pushing AlignedExpression down, i.e. allowing recursion on all such
			//  wrappers. We currently do it if either side is a DecoratorSQL, which should be enough
			//  for the standard expressions, but is certainly a brittle solution.
			@tailrec def collect(e :SQLShape[_]) :Option[(MappingSQL.__, Boolean)] = e match {
				case c :LValueSQL.__ @unchecked =>
					Some((c.component :ComponentSQL.__, permission.mayReorder && !c.component.`->isFinal`))
				case c :EditedComponentSQL[_, MappingOf[Any]#TypedProjection, Any] @unchecked =>
					Some((c.component :ComponentSQL.__, permission.mayReorder && !c.component.`->isFinal`))
				case c :ConvertedMappingSQL[_, _, MappingAt, _, _] @unchecked =>
					collect(c.value)
				case c :MappingSQL.__ =>
					Some((c, permission.mayReorder))
				case _ => None
			}
			collect(expr)
		}.unzip
		(_1 to PassedArray, _2 to PassedArray)
	}

//	lazy val defaultSplits :IndexedSeq[Seq[TypedColumn[_, _]]] = components.map(spelling.split(_))

	lazy val (
		/** Maps all columns from any mapping which are aligned with another column into an identifier of
		  * the class of abstraction to which they belong in the relation of 'corresponds to in terms of Mapping.intersect'.
		  * The keys are export columns of nominal mappings of expressions in `components` and their anchored versions.
		  */
		alignmentIds,
		/** Number of aligned columns - `alignmentIds.values.toSet.size`. */
		columnCount
	) = {
		var i = components.length
		var res = Map.empty[TypedColumn[_, _], Int]
		var nextToken = 0
		while (i > 0) {
			i -= 1
			var j = i
			while (j > 0) {
				j -= 1
				val comp1 :MappingSQL.__ = components(j)
				val comp2 :MappingSQL.__ = components(i)
				comp1.mapping intersect comp2.mapping foreach { case (extract1, extract2) =>
					val col1 = (comp1 \ (extract1.export :TypedColumn[_, comp1.Origin]) :ColumnMappingSQL.__).anchored :TypedColumn[_, _]
					val col2 = (comp2 \ (extract2.export :TypedColumn[_, comp2.Origin])).anchored :TypedColumn[_, _]
					//we don't really need anchored columns in alignmentIds, but need them to see what is default etc.
					(res.get(extract1.export), res.get(extract2.export)) match {
						case (Some(id1), Some(id2)) if id1 != id2 =>
							throw new MismatchedExpressionsException(
								aligned.mkString("Cannot unify expressions ", ", ", ": column ") + extract1 + " of " +
									components(j).mapping + " and column " + extract2 + " of " + components(i).mapping +
									components.map(_.mapping).mkString(
										" correspond to different columns in one of the mappings ", ", ", "."
									)
							)
						case (Some(id1), _) =>
							res = res.updated(extract2.export, id1).updated(col2, id1)
//							res += ((extract2.export, id1))
//							res += ((comp2.origin.anchored.export(extract2.export), id1))
						case (_, Some(id2)) =>
							res = res.updated(extract1.export, id2).updated(col1, id2)
//							res += ((extract1.export, id2))
//							res += ((comp1.origin.anchored.export(extract1.export), id2))
						case _ =>
							val id = nextToken
							nextToken += 1
							res = res.updated(extract2.export, id).updated(col2, id)
							         .updated(extract1.export, id).updated(col1, id)
//							res += ((extract1.export, id))
//							res += ((extract2.export, id))
//							res += ((comp1.origin.anchored.export(extract1.export), id))
//							res += ((comp2.origin.anchored.export(extract2.export), id))
					}
				}
			}
		}
		(res, nextToken)
	}

	/** The best column order for components, i.e. the one with a highest likelihood of a successful unification.
	  * Columns are represented here by identifiers linking aligned (corresponding) columns of all mappings:
	  * if `alignmentIds(column) == id`, then the column should come after all columns which in `columnOrder`
	  * come before `columnOrder.indexOf(id)` and before all columns which come in this sequence after that index.
	  * The order is defined by the order the columns in
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.applicableColumns allColumns]] and
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]]
	  * in all components, with a strict preference towards expressions which cannot be reordered
	  * (are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isFinal final]])
	  * or should not be reordered
	  * (are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isCustom custom]])
	  */
	lazy val columnOrder :IndexedSeq[Int] = { //perhaps it would be better as a Unique
		//Determine the best ordering of columns in all component expressions. Start with those which cannot be reordered.
		val alignedColumnsCount = columnCount
		//if column with id i comes before column with id j in one of the components which cannot be reordered, then
		//  precedence(i * alignmentIds.size + j) > 3.
		//if column with id i comes before column with id j in one of custom components, then
		//  precedence(i * alignmentIds.size + j) > 0
		//  The value increases with the likelihood of conflict coming from both being mandatory or default.
		val precedenceGraph = {
			val res = new Array[Int](alignedColumnsCount * alignedColumnsCount)
			var i = components.length
			while (i > 0) {
				i -= 1
				val comp :MappingSQL.__ = components(i)
				def setPrecedence(columns :Unique[TypedColumn[_, comp.Origin]], value :Int) :Unit = {
					val ordered = columns.toIndexedSeq.flatMap(alignmentIds.get)
					var b = ordered.length
					while (b > 0) {
						b -= 1
						var a = b
						while (a > 0) {
							a -= 1
							res(ordered(a) * alignedColumnsCount + ordered(b)) = value
						}
					}
				}
				//fixme: take the columns from split
/*
				setPrecedence(scope.applicableColumns(comp.origin.anchored, comp.mapping), 1)
				if (!mayReorder(i)) {
					setPrecedence(scope.defaultColumns(comp.origin.anchored, comp.mapping), 5)
				} else if (comp.isCustom) {
					setPrecedence(scope.defaultColumns(comp.origin.anchored, comp.mapping), 4)
				} else {
					val defaults = scope.defaultColumns(comp.origin.anchored, comp.mapping)
					setPrecedence(defaults, 2)
					setPrecedence(defaults.filter(scope.isMandatory), 3)
				}
*/
			}
			res
		}
		@inline def precedence(i :Int, j :Int) = precedenceGraph(i * alignedColumnsCount + j)

		/* The order of column ids in which the columns should appear, enforced by the order in which they appear
		 * in those components which cannot be reordered. Comes from topologically sorting the graph defined by
		 * matrix precedenceGraph. If no cycles exist and if precedence(i, j) >= 0 then
		 * topologicalOrder.indexOf(i) <= topologicalOrder.indexOf(j). The conflicts caused by cycles are broken
		 * in favour of the higher `precedence` value.
		 * It can still be changed, provided it does not invalidate the invariant (so, in practice, in sections
		 * in which none of the elements precede another). We don't fail with an exception here
		 * if a cycle is encountered, as we don't have the full or definite information yet - it is simply a heuristic.
		 */
		val ids = 0 until alignedColumnsCount
		weightedTopologicalSort(ids)(precedence).toIndexedSeq
/*
		//Now, try to refine the order starting with most specialized components (in terms of submappingOf).
		val componentsInExtendingOrder =
			topologicalSort(components, false) { _.mapping submappingOf _.mapping }.toIndexedSeq

		def merge(existingIds :IndexedSeq[Int], component :ComponentSQL.*) = {
			val nextIds =
				scope.allColumns(component.origin.anchored, component.mapping).toIndexedSeq.flatMap(alignmentIds.get)
			val existingIndices = existingIds.view.zipWithIndex to Map
			val nextIndices = nextIds.view.zipWithIndex to Map
			def rec(existingIdx :Int, nextIdx :Int, res :Builder[Int, IndexedSeq[Int]]) :IndexedSeq[Int] =
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
			rec(0, 0, PassedArray.newBuilder[Int])
		}
		(topologicalOrder /: componentsInExtendingOrder)(merge)
*/
	}

	/** Defines the order in which columns of aligned components should appear: for every `MappingSQL` in `aligned`,
	  * each of its default columns (for a given spelling scope) can be found in one of the listed collections.
	  */
	lazy val alignedColumns :IndexedSeq[Set[TypedColumn[_, _]]] = {
		val inverse = alignmentIds.view.groupMap(_._2)(_._1).view.mapValues(_.toSet).toMap
		columnOrder.map(inverse)
	}

	lazy val shape = {
		val types = alignedColumns.foldSome(PassedArray.ofCapacity[JDBCType](alignedColumns.size)) {
			(types, columns) => columns.map(_.form.sqlType) match {
				case single if single.sizeIs == 1 => Some(types :+ single.head)
				case _ => None
			}
		}
		if (types.length == alignedColumns.length) RowShape(types)
		else RowShape.Indefinite
	}

	override def toString :String =
		aligned.zipMap(permissions) { (e :SQLShape[_], ps :Permissions) =>
			"(" + e + " :" + ps.leftRightString + ")" }.mkString("Alignment(", ", ", ")")
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
  * @see [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.Aligner]]
  */ //remember that the order of expressions is not the same as in the whole query because reforming can swap sides
/** A transparent expression wrapper attaching a list of other expressions of the same value type which the wrapped
  * expression should be unified with.
  */
private[sql] class AlignedExpression[-F <: RowProduct, -S >: Grouped <: Single, V]
                                    (override val value :SQLExpression[F, S, V], val alignment :Alignment)
	extends DecoratedSQL[F, S, V] with SQLExpression[F, S, V]
{
	def this(value :SQLExpression[F, S, V], permission :Permissions)(implicit spelling :SQLSpelling) =
		this(value, new Alignment(PassedArray.single(value), PassedArray.single(permission), spelling.scope))

	def aligned     :Seq[SQLExpression[Nothing, Grouped, _]] = alignment.aligned
	def permissions :Seq[Permissions] = alignment.permissions
	def scope       :SpellingScope = alignment.scope

	if (aligned.length != permissions.length)
		throw new IllegalArgumentException(
			"Cannot create " + this + ": permission list " + permissions +
				" is of a different length than the aligned expression list."
		)

	protected override def adapt[X](conversion :SQLAdaptation[V, X]) :SQLExpression[F, S, X] =
		decorate(conversion(value), alignment)

	protected override def convert[X](conversion :SQLConversion[V, X]) :SQLExpression[F, S, X] =
		adapt(conversion)
//		if (!conversion.isDerived || conversion.isIdentity) conversion(this)
//		else copy(conversion(value), alignment)


	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :SQLExpression[E, C, V]) :SQLExpression[E, C, V] =
		decorate(e, alignment)

	override def reapply[E <: RowProduct, C >: Grouped <: Single](e :ColumnSQL[E, C, V]) :ColumnSQL[E, C, V] =
		new AlignedColumn(e, alignment)

	protected def decorate[E <: RowProduct, C >: Grouped <: Single, U]
	                      (value :SQLExpression[E, C, U], alignment :Alignment) :SQLExpression[E, C, U] =
		new AlignedExpression(value, alignment)

	protected override def reform[F1 <: F, S1 >: Grouped <: S, F2 <: RowProduct, S2 >: Grouped <: Single, V2,
	                              EC2[v] <: ConvertibleSQL[F2, S2, v, EC2], U]
                                 (other :ConvertibleSQL[F2, S2, V2, EC2])(reform :Reform, passCount :PassCount)
                                 (implicit leftResult :SQLTransformation[V, U], rightResult :SQLTransformation[V2, U],
                                           spelling :SQLSpelling)
			:(leftResult.SQLResult[F1, S1, SQLExpression[F1, S1, U]], rightResult.SQLResult[F2, S2, EC2[U]]) =
		if (!passCount.lastChance) //give other all chances to reform as long as we can default to our implementation
			passReform[F1, S1, F2, S2, V2, EC2, U](other)(reform, passCount)
		else
			other match {
				case aligned :AlignedExpression[F2, S2, V2] =>
					val all = this.aligned ++ aligned.aligned
					val ps = permissions ++ aligned.permissions
					val alignment = new Alignment(all, ps, spelling.scope)
//					val left = copy(value.to[U], alignment)
//					val right = copy(aligned.value.to[U], alignment)
					val left  = decorate(value, alignment)
					val right = decorate(aligned.value, alignment).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
					(leftResult(left), rightResult(right))
//				case adapter :AdaptedSQL[F2, S2, x, V2] =>
//				case lvalue :LValueSQL.from[F2]#of[V2] @unchecked =>
////					val that = rightResult(lvalue)
////					val all = aligned.map(_.to[U]) :+ that
//					val all = aligned :+ that
//					val ps = permissions :+ reform.rightPermissions
//					val alignment = new Alignment(all, ps, spelling.scope)
////					val left = copy(value.to[U], alignment)
//					val left  = decorate(value, alignment)
//					val right = decorate[F2, S2, U](that, alignment).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
//					(leftResult(left), rightResult(right))

				case _ =>
					val all = aligned :+ other
					val ps = permissions :+ reform.rightPermissions
					val alignment = new Alignment(all, ps, spelling.scope)
					val left  = decorate(value, alignment)
//					val right = copy(that, alignment)
					val right = decorate(other, alignment).asInstanceOf[ConvertibleSQL[F2, S2, V2, EC2]]
					(left, right)
			}

/*
	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                             (other :LValueSQL[E, C, X])
	                             (reform :Reform, passesAllowed :Int)
	                             (implicit leftResult :Lift[V, U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, S, U], reform.LValue[E, C, U]) =
	{
		val that = other.to[U]
//		val all = aligned.map(_.to[U]) :+ that
		val all = aligned :+ that
		val ps = permissions :+ reform.rightPermissions
		val alignment = new Alignment(all, ps, spelling.scope)
//		val left = copy(value.to[U], alignment)
		val left  = copy(value, alignment).to[U]
		val right = reform.lift[E, C, U, U](copy[E, Single, U](that, alignment)) match {
			case Got(lvalue) => lvalue
			case _ => throw new IllegalArgumentException(
				"Cannot unify " + this + " with " + other +
				": reform " + reform + " does not use SQLExpression as its LValue type."
			)
		}
		(left, right)
	}
*/


	override def name = "Aligned"
	override def toString :String =
		aligned.view.map("`" + _ + "`").mkString("`" + value + "` aligned { ", ", ", "}")
}


private[sql] class AlignedColumn[-F <: RowProduct, -S >: Grouped <: Single, V]
                   (override val value :ColumnSQL[F, S, V], override val alignment :Alignment)
	extends AlignedExpression[F, S, V](value, alignment) with DecoratedColumnSQL[F, S, V]




private[sql] object AlignedExpression {
	val BottomUp :Reform = new Aligner
	val TopDown  :Reform = new TopDownAligner
	def Realigner[V](selectClause :SQLShape[V]) :Reform = new Realigner(selectClause)


	/** A preprocessing reform used in the process of unifying the shapes of more than two expressions.
	  * It relies on the existing reforming implementations in [[net.noresttherein.oldsql.sql.ast.CompositeSQL composite]]
	  * SQL expressions, which compare the structure of two expressions, recursively reform each corresponding
	  * subexpression pairs, and recreate themselves while substituting the reformed results for paired subexpressions.
	  * Other expression types, leaves in the expression tree,
	  * in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm terms]]
	  * and [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]], are handled differently:
	  * instead of performing any reforming, the `Reform` substitutes each
	  * with an [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression AlignedExpression]] wrapping the original
	  * expression, but also listing subexpressions of all seen 'root' expressions aligned with the given expression.
	  *
	  * When reforming a [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]
	  * or a [[net.noresttherein.oldsql.sql.Query Query]] pair, it bypasses the normal process of reforming member
	  * queries relying on `query.`[[net.noresttherein.oldsql.sql.Query.reform reform]], instead recursively processing
	  * left and right subqueries of
	  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]/[[net.noresttherein.oldsql.sql.Query.CompoundSelect CompoundSelect]]
	  * and unifying `left.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]]
	  * with `right.selectClause` afterwards. As the result, `selectClause` of the reformed query,
	  * as well as all its member ''selects'' and ''compound selects'' subexpressions, is an `SQLExpression`
	  * reflecting the shared structure of the ''select'' clauses of its subexpressions. More specifically,
	  * it is the outer (composite) part of the expression tree shared by all those ''select'' clauses (if any),
	  * with differing subexpressions replaced with an `AlignedExpression` instance, carrying all aligned subexpressions
	  * of the covered ''select'' clauses, as well as an expression resulting from substituting the differing
	  * part in the reformed expressions with the most informative between the aligned subexpressions.
	  * The precedence is: [[net.noresttherein.oldsql.sql.ast.LValueSQL ComponentLValueSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL MappingSQL]], [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]],
	  * [[net.noresttherein.oldsql.sql.ast.CompositeSQL CompositeSQL]] followed by others.
	  *
	  * The [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.aligned aligned]] ''select'' clauses
	  * are accompanied with [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.permissions permissions]] flags,
	  * reflecting the permissions given to a `Reform` for reforming the corresponding expression
	  * on the `aligned` list, depending on [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]s
	  * used to combine the queries. They are not necessarily in the same order as ''selects'' from which they came
	  * on the `query.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.constituents constituents]], but rather
	  * in the order determined by the reforming algorithm, which at any level may swap the left and right expressions.
	  *
	  * Note that after reforming a query, only its ''select'' clause contains `AlignedExpression`s listing ''all''
	  * member ''selects'' of the query, with all its subqueries, in particular member ''selects'' themselves,
	  * being aligned only with a subset of other ''selects''.
	  */ //todo: this should be also a QueryReform
	private class Aligner(override val permissions :Permissions)
	                     (wrap :Reform => Reform,
	                      constructor :(Permissions, Reform => Reform) => Reform =
	                        new Aligner(_)(_))
		extends TunedReform(permissions)(wrap, constructor)
		   with PreprocessorReform with Reform
	{
		def this() = this(MayReform)(identity)

		override lazy val swap =
			if ((this eq self) && isSymmetrical)
				self
			else
				new Aligner(permissions.swap)(wrap, constructor) {
		            override lazy val swap = Aligner.this.self
	            }.self


/*
		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
		                   F <: RowProduct, B >: Grouped <: Single, Y,
		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
		                  (left :L[X], right :R[Y])
		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
*/

		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			(left, right) match {
				case (l :AlignedExpression[LF, LS, LV], r :AlignedExpression[RF, RS, RV]) =>
					val all = l.aligned ++ r.aligned
					val ps  = l.permissions ++ r.permissions
					val alignment   = new Alignment(all, ps, spelling.scope)
					val leftRes  = leftResult(new AlignedExpression(l.value, alignment))
					val rightRes = rightResult(new AlignedExpression(r.value, alignment))
					(leftRes, rightRes)
				//AdapterSQL will implement its reform before AlignedExpression gets to
				case (l :AlignedExpression[LF, LS, LV], r :AdaptedSQL[RF, RS, x, RV]) =>
					super[Reform].apply[LF, LS, LV, LE, RF, RS, RV, RE, U](l, r)(leftResult, rightResult, spelling)
				case (l :AlignedExpression[LF, LS, LV], _) =>
					val all = l.aligned :+ right
					val ps  = l.permissions :+ rightPermissions
					val alignment = new Alignment(all, ps, spelling.scope)
					val leftRes   = leftResult(new AlignedExpression(l.value, alignment))
					val rightRes  = rightResult(new AlignedExpression(right, alignment))
					(leftRes, rightRes)

				case (l :AdaptedSQL[LF, LS, x, LV], r :AlignedExpression[RF, RS, RV]) =>
					super[Reform].apply[LF, LS, LV, LE, RF, RS, RV, RE, U](l, r)(leftResult, rightResult, spelling)
				case (_, r :AlignedExpression[RF, RS, RV]) =>
					val all = left +: r.aligned
					val ps  = leftPermissions +: r.permissions
					val alignment = new Alignment(all, ps, spelling.scope)
					val leftRes   = leftResult(new AlignedExpression(left, alignment))
					val rightRes  = rightResult(new AlignedExpression(r.value, alignment))
					(leftRes, rightRes)
				case _ => super.apply(left, right)
			}

		//This seems impossible to implement, because we have no idea what type the enclosing expressions expect:
		// if it's anything but an SQLExpression (or ColumnSQL, I guess), we can neither wrap it before, nor after
		// applying the conversion. We must try to instead wrap the whole root expressions in AlignedExpression
		// and somehow push them down.
/*
		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
		{
//			val unifiedLeft  = left.to(compat.left)
//			val unifiedRight = right.to(compat.right)
			val expressions = PassedArray.two(left, right)
			val permissions = PassedArray.two(leftPermissions, rightPermissions)
			val alignment = new Alignment(expressions, permissions, spelling.scope)
			val l = new AlignedExpression(left, alignment)
			val r = new AlignedExpression(right, alignment)
			(l, r)
		}
*/

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[LM[Unit]#Subject, U],
		                              rightResult :SQLTransformation[RM[Unit]#Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			fallback(left, right)(leftResult, rightResult, spelling)

	}



	private class TopDownAligner(override val permissions :Permissions)
	                            (wrap :Reform => Reform)
		extends Aligner(permissions)(wrap, new TopDownAligner(_)(_)) //with TopDownReform
	{
		def this() = this(MayReform)(identity)

		override lazy val swap =
			if ((this eq self) && isSymmetrical)
				self
			else
				new TopDownAligner(permissions.swap)(wrap) {
					override lazy val swap = TopDownAligner.this.self
				}.self
	}


	/** A pseudo `Reform` hijacking the query and expression traversing mechanism in order to align
	  * the ''select'' clauses of all member ''selects'' in a query with the ''select'' clause of the whole query.
	  * It is used immediately after [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression.Aligner Aligner]],
	  * in which recursive reforming of individual selects created a layout expression
	  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
	  * for the whole query, which now contains in the positions of aligned leaf expressions an instance
	  * of [[net.noresttherein.oldsql.sql.mechanics.Alignment Alignment]] possessing information
	  * about aligned subexpressions in every ''select''.
	  *
	  * `Realigner` is then used to unify the whole ''compound select'', and when reaching individual ''selects'',
	  * unifies their [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]]
	  * with the layout [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
	  * of the whole ''compound select'', which
	  *
	  * the `selectClause` of each member ''select'' (as the `left` argument)
	  * and the layout `selectClause` of the whole ''compound select'', listing the aligned subexpression information
	  * present in [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.selectClause selectClause]]
	  * of the whole ''compound select'' (on the right), and reforms the left expression to conform to the column set
	  * defined by `Alignment`.
	  *
	  * instances present in all [[net.noresttherein.oldsql.sql.mechanics.AlignedExpression AlignedExpression]]
	  * subexpressions of the given `selectClause` to matched `AlignedExpression`s within ''select'' clauses
	  * of each ''select'' in the query. This is needed because after non-reforming by `Aligner`, ''select'' clauses
	  * of all subqueries in the 'reformed' query have only partial information, not including all ''selects''
	  * from the whole query. Additionally, thanks to this the needed preprocessing data is calculated only once,
	  * in the universally shared `Alignment`.
	  */
	private class Realigner[S](selectClause :SQLExpression[Nothing, Grouped, S],
	                           wrap :Reform => Reform = identity[Reform])
		extends AbstractReform(wrap, new Realigner(selectClause, _))
		   with AbstractValidatorReform with PreprocessorReform with Reform
	{
//		override val swap = super[AbstractReform].swap

/*
		override def apply[E <: RowProduct, A >: Grouped <: Single, X,
		                   L[v] <: SQLExpression[E, A, v] with ConvertingTemplate[E, A, v, L],
		                   F <: RowProduct, B >: Grouped <: Single, Y,
		                   R[v] <: SQLExpression[F, B, v] with ConvertingTemplate[F, B, v, R], Z]
		                  (left :L[X], right :R[Y])
		                  (implicit leftResult :SQLTransformation[X, Z], rightResult :SQLTransformation[Y, Z], spelling :SQLSpelling)
				:(leftResult.SQLResult[E, A, L[Z]], rightResult.SQLResult[F, B, R[Z]]) =
*/
		override def apply[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                   RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                  (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                  (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                            spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			???
//			(left, right) match {
//				case (l :AlignedExpression[LF, LS, LV], r :AlignedExpression[RF, RS, RV]) => (
//					leftResult(new AlignedExpression(l.value, r.alignment)),
//					rightResult(right)
//				)
//				//we should use reform on anything which will recurse down, as the AlignedExpression may be lower
//				case (l :AdaptedSQL[LF, LS, x, LV], r :AlignedExpression[RF, RS, RV]) => super[Reform].apply(left, right)
//				case (_, r :AlignedExpression[RF, RS, RV]) => (
//					leftResult(new AlignedExpression(left, r.alignment)),
//					rightResult(right)
//				)
//				case (l :AlignedExpression[LF, LS, LV], r :AdaptedSQL[RF, RS, x, RV]) => super.apply(l, r)
//				case (l :AlignedExpression[LF, LS, LV], _) => //this shouldn't happen, but whatever
//					(leftResult(left), rightResult(right))
//				case _ =>
//					super.apply(left, right)
//			}

		override def fallback[LF <: RowProduct, LS >: Grouped <: Single, LV, LE[v] <: ConvertibleSQL[LF, LS, v, LE],
		                      RF <: RowProduct, RS >: Grouped <: Single, RV, RE[v] <: ConvertibleSQL[RF, RS, v, RE], U]
		                     (left :ConvertibleSQL[LF, LS, LV, LE], right :ConvertibleSQL[RF, RS, RV, RE])
		                     (implicit leftResult :SQLTransformation[LV, U], rightResult :SQLTransformation[RV, U],
		                               spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, LS, LE[U]], rightResult.SQLResult[RF, RS, RE[U]]) =
			???

		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
		                    (implicit leftResult  :SQLTransformation[RM[Unit]#Subject, U],
		                              rightResult :SQLTransformation[LM[Unit]#Subject, U], spelling :SQLSpelling)
				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
			???

		//we leave the default fallback simply returning the two expressions
/*

		override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) :Select[P, V] =
			query.selectOther(
				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
				)._1
			)

		override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) :SelectSQL[F, V] =
			query.selectOther(
				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
				)._1
			)

		override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) :TopSelectSQL[V] =
			query.selectOther(
				apply[query.From, Grouped, V, Nothing, Grouped, V, V](
					query.selectClause, selectClause.castFrom[SQLShape[S], SQLShape[V]]
				)._1
			)

		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] = {
			val inOperand = spelling.inOperand
			val l = left.apply(query.left)(inOperand)
			val r = right.apply(query.right)(inOperand)
			CompoundSelect.reformed(l, query.operator, r, query.selectClause)(this)
		}

		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
		{
			val inOperand = spelling.inOperand
			val l = left.apply(query.left)(inOperand)
			val r = right.apply(query.right)(inOperand)
			CompoundSelectSQL.reformed(l, query.operator, r, query.selectClause)(this)
		}
*/
	}

	/* Things to do after running Realigner on a query:
	 *   1. All cases of ComponentLValueSQL (and, optionally, EditedLValueSQL and any DecoratorSQL over them)
	 *      directly under an AlignedExpression should be replaced with an expression matching the column order
	 *      in the Alignment. This should involve first:
	 *      1. Including/excluding columns present on the list;
	 *      1. Verifying if the current alignment.scope.defaultColumns is a (sparse) subsequence of alignment.columnOrder;
	 *      1. If not, either using ComponentSQL.reorder to create a custom instance, or including the changes in the next point;
	 *      1. Wrapping the whole thing in an expression wrapper squeezing null columns between the component's columns
	 *         and being able to add null columns when reforming.
	 *      For reordering, we have RearrangedSQL. It can also be used to add initial null columns,
	 *      but cannot do so when reforming, as it cannot match columns of its value (i.e, the component)
	 *      to another expression, because it knows nothing about its type. We would either have to create
	 *      a special wrapper subclass for every expression type which can be spliced with null columns,
	 *      or incorporate Reform.mayAddNull handing to the types itself. The latter could be attempted
	 *      with using as export Mapping a special implementation with mock columns, although it would
	 *      make it not isomorphic with the nominal mapping, and thus incompatible with current implementations
	 *      of all methods which compare if component expressions are compatible
	 *      based on mapping isomorphism/homomorphism. The former is potentially quite a lot of work,
	 *      duplicating existing functionality to a degree, such as more simple reforming. Hmm.
	 *
	 *   1. Using the normal (exponential) query reforming algorithm to reform all select clauses, without altering
	 *      already reformed component expressions columns in other ways than adding null columns.
	 *      This cannot be a global Reform permission setting, as we should allow of reforming
	 *      all other expression types (permissions permitting). For this we need either
	 *      a special Reform implementation/decorator, or an expression wrapper, such as one mentioned
	 *      in the previous point, which will set the permissions as required.
	 *
	 *   1. Do something for LabeledSQL reforming, as we certainly could add null columns and reorder,
	 *      but this has the same problems as ComponentSQL.
	 */


/*
	class PreliminaryAligner(override val mayExcludeLeft :Boolean, override val mayIncludeLeft :Boolean,
	                         override val mayReorderLeft :Boolean, override val mayAddNullLeft :Boolean)
	                        (override val mayExcludeRight :Boolean, override val mayIncludeRight :Boolean,
	                         override val mayReorderRight :Boolean, override val mayAddNullRight :Boolean)
	                        (wrap :ExpressionReform => ExpressionReform)
		extends TunedReform[ExpressionLValue](mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft)(
		                                        mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)(
		                                        wrap, new Aligner(_, _, _, _)(_, _, _, _)(_))
		   with PreprocessorReform with TopDownReform with Reform
	{
		def this() = this(true, true, true, true)(true, true, true, true)(identity)

		override def fallback[L <: RowProduct, A >: Grouped <: Single, X, R <: RowProduct, B >: Grouped <: Single, Y, Z]
		                     (left :SQLExpression[L, A, X], right :SQLExpression[R, B, Y])
		                     (implicit compat :SQLTypeUnification[X, Y, Z], spelling :SQLSpelling)
				:(SQLExpression[L, A, Z], SQLExpression[R, B, Z]) =
			(left, right) match {
				case (group :GroupedExpressions[X], _) =>
//					val l = left.to(compat.left)
					val r = new AlignedExpression(right.to(compat.right), group.expressions.map(_.to(compat.left)), ???, spelling.scope)
					(left.to(compat.left), r)
				case (_, group :GroupedExpressions[Y]) =>
					val l = new AlignedExpression(left.to(compat.left), group.expressions.map(_.to(compat.right)), ???, spelling.scope)
					(l, right.to(compat.right))
				case (_ :CompositeSQL[L, A, X], _) =>
					super[Reform].apply(left, right)
				case (_, _ :CompositeSQL[R, B, Y]) =>
					super[Reform].apply(left, right)
				case _ => //this should not happen
					super[Reform].apply(left, right)
//					val aligned = new GroupedExpressions(PassedArray :+ left.to(compat.left) :+ right.to(compat.right))
//					(aligned, aligned)
		}

	}
*/

}






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


