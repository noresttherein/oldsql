package net.noresttherein.oldsql.sql.mechanics

import java.sql.JDBCType

import scala.annotation.tailrec
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.{Listing, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.morsels.{EqRef, weightedTopologicalSort}
import net.noresttherein.oldsql.pixies.Rearrangement
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.schema.bits.LabelPath
import net.noresttherein.oldsql.schema.bits.LabelPath.{Label, ~/}
import net.noresttherein.oldsql.slang.{classNameMethods, mappingMethods, saferCasting}
import net.noresttherein.oldsql.sql.RowShape.JDBCTypeExtensions
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, SQLShape, Single}
import net.noresttherein.oldsql.sql.ast.{AliasedSQL, ColumnMappingSQL, ComponentSQL, GenericColumnComponentSQL, IndexedSQL, InlineSQL, MappingSQL, RecordSQL}
import net.noresttherein.oldsql.sql.mechanics.AlignableColumn.{componentPrecedence, labeledPrecedence, strictPrecedence, weakPrecedence}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayExclude, MayReform, MayReorder}






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
		val aligned :IndexedSeq[SQLExpression[Nothing, Grouped, _]],
		/** Reforming [[net.noresttherein.oldsql.sql.mechanics.Reform.permissions permissions]]
		  * for each of the expressions in `aligned`, in the same order. */
		val permissions :IndexedSeq[Permissions]
	)(implicit private val spelling :SQLSpelling)
{
	def this(left :SQLExpression[Nothing, Grouped, _], right :SQLExpression[Nothing, Grouped, _])
	        (leftPermissions :Permissions, rightPermissions :Permissions)
	        (implicit spelling :SQLSpelling) =
		this(PassedArray.two(left, right), PassedArray.two(leftPermissions, rightPermissions))

	def this(single :SQLExpression[Nothing, Grouped, _], permissions :Permissions)
	        (implicit spelling :SQLSpelling) =
		this(PassedArray.single(single), PassedArray.single(permissions))

	if (aligned.size != permissions.length)
		throw new IllegalArgumentException(
			"Cannot create " + this + ": permission list " + permissions +
				" is of a different length than the aligned expression list " + aligned + "."
		)

	/** The index of the given expression in `this.`[[net.noresttherein.oldsql.sql.mechanics.Alignment.aligned aligned]],
	  * identifying also corresponding values in
	  * [[net.noresttherein.oldsql.sql.mechanics.Alignment.reorderings reorderings]] and other related properties.
	  * Expressions are compared for reference identity, that is `e eq expr`, rather than `equals`.
	  * Note that it is however still possible that the same expression instance occurs more than once in the alignment,
	  * although in that case [[net.noresttherein.oldsql.pixies.Rearrangement Rearrangement]] will be the same.
	  * @return the last index of the given expression in this alignment, or `-1` if not present.
	  */
	def indexOf(expr :SQLExpression[Nothing, Grouped, _]) :Int = {
		var i = aligned.length
		while (i > 0 && { aligned(i) ne expr })
			i -= 1
		i
	}

	def append(expr :SQLExpression[Nothing, Grouped, _], permissions :Permissions) :Alignment =
		new Alignment(aligned :+ expr, this.permissions :+ permissions)

	def prepend(expr :SQLExpression[Nothing, Grouped, _], permissions :Permissions) :Alignment =
		new Alignment(expr +: aligned, permissions +: this.permissions)

	def :+(expr :(SQLExpression[Nothing, Grouped, _], Permissions)) :Alignment =
		append(expr._1, expr._2)

	def +:(expr :(SQLExpression[Nothing, Grouped, _], Permissions)) :Alignment =
		prepend(expr._1, expr._2)

	def :++(other :Alignment) :Alignment = new Alignment(aligned :++ other.aligned, permissions :++ other.permissions)

	//Fixme: equal expressions in different member selects may have different permissions!
	// They should have separate entries in both tables.
/*
	def ++(other :Alignment) :Alignment = {
		val permissions = this.permissions.mapWithIndex { (perm, idx) =>
			other.aligned.indexOf(aligned(idx)) match {
				case -1 => perm
				case otherIdx => other.permissions(otherIdx) & perm
			}
		}
		val allAligned = aligned ++ other.aligned.view.filterNot(aligned.contains)
		val allPermissions = permissions ++ (0 until aligned.size).collect {
			case i if aligned.contains(other.aligned(i)) => other.permissions(i)
		}
		new Alignment(allAligned, allPermissions)
	}
*/

	@deprecated("use :+ or +:", "now")
	def updated(e :SQLExpression[Nothing, Grouped, _], permissions :Permissions) :Alignment =
		aligned.indexOf(e) match {
			case -1 => new Alignment(aligned :+ e, this.permissions :+ permissions)
			case n  => new Alignment(aligned, this.permissions.updated(n, this.permissions(n) & permissions))
		}

	private lazy val alignableColumns :IndexedSeq[AlignableColumns] =
		aligned.view.zipMap(permissions)(spelling.potentialColumns(_, _)).toIndexedSeq

	private lazy val owners :Map[AlignableColumn.__, AlignableColumns] =
		alignableColumns.flatMap { owner => owner.columns.map((_, owner)) }.toMap

	private lazy val (
		/** Maps all column expressions from any of aligned expression into an identifier of the class of abstraction
		  * to which they belong in the relation of
		  * `AlignedColumn.`[[net.noresttherein.oldsql.sql.mechanics.AlignedColumn.corresponds corresponds]].
		  * The keys are the members of [[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns potentialColumns]]:
		  * {{{
		  *     aligned.zipMap(permissions)(_ potentialColumns _).flatMap(_.columns).toSet
		  * }}}
		  */
		columnGroupIds, //:Map[AlignableColumn.__, Int]
		/** Groups together components of the graph in which the vertices are all alignable columns of all `aligned`
		  * expressions, and an edge between two columns exists 'iff' `v1 corresponds v2`. Note that a component
		  * is a connected graph, but not necessarily complete. The values are all columns within a particular component,
		  * and the keys are ids of those components. This enumeration is consistent with `columnGroupIds`:
		  * {{{
		  *     columnGroups.forall { case (id, columns) => columns.forall(id == columnGroupIds(_)) }
		  * }}}
		  * holds.
		  */
		columnGroups   //:Map[Int, Set[AlignableColumn.__]]
	) = {
		val owners = this.owners //avoid @volatile access
		var ids = Map.empty[AlignableColumn.__, Int]          //will become columnGroupIds
		var groups = Map.empty[Int, Set[AlignableColumn.__]]  //will become columnGroups
		var nextId = 0                                        //first Int not used as a group id (greater than all used)
		//returns the id for the column from ids if it contains column, or assigns it a new one, updating ids.
		def columnGroupId(column :AlignableColumn.__) :Int =
			ids.getOrElse(column, -1) match {
				case -1 =>
					ids = ids.updated(column, nextId)
					groups = groups.updated(nextId, Set.empty[AlignableColumn.__] + column)
					nextId += 1
					nextId - 1
				case n => n
			}
		var e1Idx = alignableColumns.length
		while (e1Idx > 0) {
			e1Idx -= 1
			val columns1 = alignableColumns(e1Idx).columns
			var e2Idx = e1Idx
			while (e2Idx > 0) {
				e2Idx -= 1
				val owner2 = alignableColumns(e2Idx)
				val columns2 = owner2.columns
				var i = columns1.length
				while (i > 0) {
					i -= 1
					val col1 = columns1(i)
					val id1 = columnGroupId(col1)
//					var j = if (e2Idx == e1Idx) i else columns2.length
					var j = columns2.length
					while (j > 0) {
						j -= 1
						val col2 = columns2(j)
						if (col1 corresponds col2) {
							ids.getOrElse(col2, -1) match {
								case -1 =>
									ids = ids.updated(col2, id1)
									val group = groups(id1)
									group.find(owners(_) eq owner2) foreach { c1 =>
										throw new AssertionError(
											"Aligning column " + col2 + " with another column " + c1 +
												" of the same expression`" + owner2.owner + "` within " + group + "."
										)
									}
									groups = groups.updated(id1, group + col2)
								case n if n != id1 =>
									val group1 = groups(id1)
									val group2 = groups(n)
									for {
										c1 <- group1
										c2 <- group2
										if owners(c1) eq owners(c2)
									}
										throw new AssertionError(
											"Aligning columns " + c1 + " and " + c2 + " of the same expression `" +
												owners(c1) + "`: " + group1 + " ++ " + group2 + "."
										)
									groups -= n
									groups = groups.updated(id1, group2 ++ groups(id1))
								case _ =>
							}
						} else
							columnGroupId(col2) //add it to maps if it is not already there
					}
				}
			}
		}
//		(ids, groupIds, nextId)
		//We'll be running an O(n^2) algorithm over (0 until columnGroups.size), so lets remove all gaps among used ids
		// caused by merging.
		val shiftDown = (0 until nextId).scanLeft(0) { //shiftDown(i) == i - groups.keySet.filter(_ <= i).size
			(unused, id) => if (groups.contains(id)) unused else unused + 1
		}.tail
		val reducedIds = ids.view.mapValues(id => id - shiftDown(id)).toMap
		val reducedGroups = groups.map { case (id, columns) => (id - shiftDown(id), columns) }
		//We should validate that no group contains two columns from the same expression, because it means conflicts.
		(reducedIds, reducedGroups)
	}

	/** The best column order for components, i.e. the one with a highest likelihood of a successful unification.
	  * Columns are represented here by identifiers linking aligned (corresponding) columns of all mappings:
	  * if `columnGroupIds(column) == id`, then the column should come after all columns which in `columnOrder`
	  * come before `columnOrder.indexOf(id)` and before all columns which come in this sequence after that index.
	  * The order is defined by the order the columns are listed in `AlignableColumns` for each aligned expression,
	  * by whether a particular column can or is expected to be included in the final alignment, and whether
	  * columns in a particular expressions can be reordered.
	  * For component expressions, it is defined by the column order in
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.applicableColumns applicableColumns]] and
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]],
	  * with a strict preference towards expressions which cannot be reordered
	  * (are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isFinal final]]
	  * or `!permissions.mayReorder`), or should not be reordered
	  * (are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isCustom custom]]).
	  */
	private lazy val groupOrder :IndexedSeq[Int] = {
		/* Determine the best ordering of columns in all aligned expressions, starting with those which
		 * cannot be reordered. We create a weighted directed graph with all columns as vertices, and an edge between
		 * columns i and j exists if i comes before j in some ordering, with the value increasing with the likelihood
		 * of this fact causing a conflict.
		 * If a column with id i comes before a column with id j in one of the expressions which cannot be reordered,
		 * then precedence(i * columnGroups.size + j) >= 10.
		 * If a column with id i comes before column with id j in one of components,
		 * then precedence(i * columnsGroups.size + j) > 0, with ordering between mandatory columns having precedence
		 * over ordering between default columns, and that one over ordering of all available columns of the expression.
		 */
		val groupCount = columnGroups.size //don't use the lazy val all the time
		val precedenceGraph = {
			val res = new Array[Int](groupCount * groupCount)
			var i = alignableColumns.length
			while (i > 0) {
				i -= 1
				val expr = alignableColumns(i)
				val columns = expr.columns
				val groupIds = columns.map(columnGroupIds)
				var y = groupIds.length
				while (y > 0) {
					y -= 1
					var x = y
					while (x > 0) {
						x -= 1
						val weight = -expr.precedence(x, y)
						val xId = groupIds(x)
						val yId = groupIds(y)
						if (res(xId * groupCount + yId) < weight)
							res(xId * groupCount + yId) = weight
					}
				}
/*
				//sets or increases the precedence between all column pairs in columns to
				def setPrecedence(columns :Seq[ColumnSQL.__], value :Int) :Unit = {
					val groupIds = columns.toIndexedSeq.flatMap(columnGroupIds.get)
					var y = groupIds.length
					while (y > 0) {
						y -= 1
						var x = y
						while (x > 0) {
							x -= 1
							val xId = groupIds(x)
							val yId = groupIds(y)
							if (res(xId * groupCount + yId) < value)
								res(xId * groupCount + yId) = value
						}
					}
				}
				val expr = alignableColumns(i)
				val columns = expr.columns.map(_.column)
				val defaultColumns = expr.columns.collect { case col if col.isDefault => col.column }
				val mandatoryColumns = expr.columns.collect { case col if !col.isOptional => col.column }

				expr.owner match {
					case _ if !expr.permissions.mayReorder => //Can't reorder, or exclude/include
						setPrecedence(mandatoryColumns, 13)
						if (expr.permissions.mayExclude && mandatoryColumns.length != defaultColumns.length)
							setPrecedence(defaultColumns, 12)
						if (expr.permissions.mayInclude && columns.length != defaultColumns.length)
							setPrecedence(columns, 11)
					//Currently, comp.isCustom implies !expr.permissions.mayReorder, so this case is dead code,
					// but better to have it in case we don't take into account component's state in permissions later.
					case comp :ComponentSQL.__ if comp.isCustom =>
						setPrecedence(defaultColumns, 9)
					case comp :ComponentSQL.__ if !comp.isDefault =>
						setPrecedence(defaultColumns, 5)
					case _ :IndexedSQL.__ =>
						setPrecedence(defaultColumns, 4)
					case _ =>
						setPrecedence(mandatoryColumns, 3)
						if (expr.permissions.mayExclude && mandatoryColumns.size != defaultColumns.size)
							setPrecedence(defaultColumns, 2)
						if (expr.permissions.mayInclude && columns.length != defaultColumns.length)
							setPrecedence(columns, 1)
				}
*/
			}
			res
		}
		@inline def precedence(i :Int, j :Int) = precedenceGraph(i * groupCount + j)

		/* The order of column ids in which the columns should appear, enforced by the order in which they appear
		 * in those components which cannot be reordered. Comes from topologically sorting the graph defined by
		 * adjacency matrix precedenceGraph. If no cycles exist and if precedence(i, j) >= 0 then
		 * topologicalOrder.indexOf(i) <= topologicalOrder.indexOf(j). The conflicts caused by cycles are broken
		 * in favour of the higher `precedence` value.
		 * It can still be changed, provided it does not invalidate the invariant (so, in practice, in sections
		 * in which none of the elements precede another). We don't fail with an exception here
		 * if a cycle is encountered, as we don't have the full or definite information yet - it is simply a heuristic.
		 */
		val ids = 0 until groupCount
		weightedTopologicalSort(ids)(precedence).toIndexedSeq
	}

	/** [[net.noresttherein.oldsql.sql.mechanics.Alignment.groupOrder groupOrder]] filtered to exclude columns which:
	  *   - are non default for all expressions, or
	  *   - are are either optional or not reorderable within each expression, or
	  *   - are all optional, but against the column order in a non reorderable expression.
	  */
	private lazy val included :Unique[Int] = {
		val owners = this.owners
		(for {
			(id, i)  <- groupOrder.zipWithIndex
			group    = columnGroups(id)
			if group.exists(_.isDefault) //exclude potential columns nobody wanted
			included = group.map(c => new EqRef(owners(c)))
			optional = group.forall(_.isOptional)
			//exclude columns such that an expression absent from the aligned group may not add null columns
			if !optional || !alignableColumns.forall {
				owner => owner.permissions.mayAddNull || included(new EqRef(owner))
			}
			//exclude columns which break the ordering
			if !optional || !(0 until i).exists { j =>
				group.exists { column =>
					val owner = owners(column)
					columnGroups(groupOrder(j)).exists { earlierColumn =>
						(owner eq owners(earlierColumn)) && owner.precedence(i, j) < 0
					}
				}
			}
		} yield id) to Unique
	}

	/** The recommended alignment of all columns in each of the aligned expressions.
	  * All columns with [[net.noresttherein.oldsql.sql.mechanics.AlignableColumns AlignableColumns]]
	  * for all expressions are first grouped into sets such that for every two columns in a set, `a` and `b`,
	  * there are columns `c0, c1, ..., cn` in the same set such that
	  * `a `[[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.corresponds corresponds]]` c0`, `c0 corresponds c1`,
	  * `cn corresponds b`. Only those sets within which at least one column
	  * is [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.isDefault default]] are included.
	  * The order is affected by the order in `AlignableColumns.columns` of each aligned expression
	  * (which, in turn, for component expressions is defined by the order in
	  * `scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.applicableColumns applicableColumns]]),
	  * with the order in expressions which cannot be reordered (`!permissions.mayReorder`,
	  * or are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.isFinal final]]), or should best
	  * not be reordered (are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.custom custom]].
	  * If no cycles due to conflicting ordering within different expressions exist, then if column `a` comes before
	  * column `b` in some `AlignableColumns.columns`, then it is included in a set at a lower index in `alignment`
	  * than `b`. All conflicts are ignored, but the heuristic ordering attempts to favour ordering within
	  * non [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.isOptional optional]] column over
	  * [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.isDefault default]] columns, and those over all others.
	  */
	lazy val alignment   :IndexedSeq[Set[AlignableColumn.__]] = included.view.map(columnGroups).toIndexedSeq

	/** Assigns each column appearing in at least one
	  * [[net.noresttherein.oldsql.sql.mechanics.AlignableColumns AlignableColumns]] returned by aligned expressions,
	  * which is included in this alignment,
	  * an index `0 <= i < `[[net.noresttherein.oldsql.sql.mechanics.Alignment.alignment alignment]]`.length` such that,
	  * if a column `a` appears before column `b` in some `AlignableColumns` and there are no cycles,
	  * then `columnIndex(a) < columnIndex(b)`.
	  */
	lazy val columnIndex :Map[AlignableColumn.__, Int] = {
		val included = this.included
		columnGroupIds.collect {
			case (col, groupId) if included.contains(groupId) => (col, included.indexOf(groupId))
		}
	}

	/** For every [[net.noresttherein.oldsql.sql.mechanics.Alignment.aligned aligned]] expression, provides
	  * a mapping from indices of columns listed in expression's
	  * [[net.noresttherein.oldsql.sql.SQLExpression.potentialColumns alignableColumns]] to indices in accordance
	  * with `this.`[[net.noresttherein.oldsql.sql.mechanics.Alignment.alignment alignment]].
	  */
	lazy val reorderings :IndexedSeq[Rearrangement] =
		alignableColumns.map { expr => Rearrangement(expr.columns.map(columnIndex), included.size) }

	/*
		lazy val alignedColumns = alignableColumns.view.map { exprCols =>
	//		val exprColumns = exprCols.columns.view.map(_.column).toSet
			val alignableColumns = exprCols.columns.view.map { c => c.column -> c }.toMap
			val exprColumns = alignableColumns.keySet
			exprCols.owner -> alignment.map { alignedColumns =>
				val columns = alignedColumns & exprColumns
				columns.size match {
					case 0 => SQLNull[Null](alignedColumns.head.selectForm)
				}
			}
		}
	*/

	lazy val shape :RowShape =
		try {
			val types = alignment.foldLeft(PassedArray.ofCapacity[JDBCType](alignment.size)) {
				(types, columns) => types :+ columns.view.map(_.column.selectForm.sqlType).reduce(_ | _)
			}
			RowShape(types)
		} catch {
			case _ :IllegalArgumentException => RowShape.Indefinite(alignment.length)
		}


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case other :Alignment if other canEqual this =>
			permissions == other.permissions && aligned == other.aligned && spelling == other.spelling
		case _ => false
	}
	def canEqual(that :Any) :Boolean = that.getClass == getClass
	override def hashCode :Int = (permissions.hashCode * 31 + aligned.hashCode) * 31 + spelling.hashCode

	override def toString :String =
		aligned.view.zipMap(permissions) { (e :SQLShape[_], ps :Permissions) =>
			"(" + e + ": " + ps.leftRightString + ")" }.mkString("Alignment(", ", ", ")")
}







class AlignableColumns(val owner   :SQLExpression.__, val permissions :Permissions,
                       val columns :IndexedSeq[AlignableColumn.__])
	extends Serializable
{
	def permit(permissions :Permissions) = new AlignableColumns(owner, this.permissions | permissions, columns)
	def prohibit(permissions :Permissions) = new AlignableColumns(owner, this.permissions & permissions, columns)

	def +(column :AlignableColumn[_ <: RowProduct, _ >: Grouped <: Single, _]) :AlignableColumns =
		new AlignableColumns(owner, permissions, columns :+ column)

	def +:(column :AlignableColumn[_ <: RowProduct, _ >: Grouped <: Single, _]) :AlignableColumns =
		new AlignableColumns(owner, permissions, column +: columns)

	/** Compares the strength of the ordering between columns at indices `column1` and `column2` in this expression.
	  * `precedence(column1, column2).sign` always equals `column1 compare column2`, but the absolute value
	  * increases with a rough likelihood that both columns appear in the final alignment in that order.
	  * If `!permissions.mayReorder`, then the results absolute value will equal
	  * `AlignableColumns.`[[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.strictPrecedence strictPrecedence]].
	  * Otherwise, for most non-component expressions, it will equal
	  * `AlignableColumns.`[[net.noresttherein.oldsql.sql.mechanics.AlignableColumn.weakPrecedence weakPrecedence]].
	  * Component expressions (and possibly some special expressions, depending on
	  * [[net.noresttherein.oldsql.sql.mechanics.AlignableColumn AlignableColumn]] type) have values greater or equal
	  * than the later and lesser or equal than the former.
	  */
	def precedence(column1 :Int, column2 :Int)(implicit spelling :SQLSpelling) :Int =
		if (column1 == column2)
			0
		else {
			val c1 = columns(column1)
			val c2 = columns(column2)
			if (!permissions.mayReorder)
				(column1 compare column2) * strictPrecedence(c1.isOptional, c2.isOptional, c1.isDefault, c2.isDefault)
			else
				columns(column1) precedence columns(column2) getOrElse (
					(column1 compare column2) * weakPrecedence(c1.isOptional, c2.isOptional, c1.isDefault, c2.isDefault)
				)
		}

	def precedence(column1 :AlignableColumn.__, column2 :AlignableColumn.__)(implicit spelling :SQLSpelling) :Int = {
		val i = columns.indexOf(column1)
		val j = columns.indexOf(column2)
		if (i < 0)
			throw new IllegalArgumentException(
				column1.toString + " does not belong to potential columns of `" + owner + "`."
			)
		if (j < 0)
			throw new IllegalArgumentException(
				column2.toString + " does not belong to potential columns of `" + owner + "`."
			)
		precedence(i, j)
	}

	override def toString :String = "AlignableColumns(" + owner + ", " + permissions + ", " + columns + ")"
}


object AlignableColumns {
	def none(expression :SQLExpression.__, permissions :Permissions = MayReform) :AlignableColumns =
		new AlignableColumns(expression, permissions, IndexedSeq.empty)

	def apply[F <: RowProduct, S >: Grouped <: Single]
	         (expression :SQLExpression[F, S, _], permissions :Permissions, columns :IndexedSeq[AlignableColumn[F, S, _]])
			:AlignableColumns =
		new AlignableColumns(expression, permissions, columns)

	def apply[F <: RowProduct, S >: Grouped <: Single]
	         (expression :SQLExpression[F, S, _], columns :Seq[ColumnSQL[F, S, _]])(implicit spelling :SQLSpelling)
			:AlignableColumns =
	{
		val alignableColumns = columns.view.map { c => AlignableColumn(c) }.toIndexedSeq
		new AlignableColumns(expression, Permissions.MayAddNull, alignableColumns)
	}

	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (column :AlignableColumn[F, S, V], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
		AlignableColumns(column.column, permissions, IndexedSeq.empty :+ column)

	def apply[F <: RowProduct, S >: Grouped <: Single]
	         (expression :ColumnSQL[F, S, _], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
		AlignableColumns(expression, permissions, IndexedSeq.empty :+ new AlignableNestedColumn(expression))

	def apply[F <: RowProduct, S >: Grouped <: Single]
	         (expression :ColumnSQL[F, S, _])(implicit spelling :SQLSpelling) :AlignableColumns =
		apply(expression, Permissions.NoReform)

	def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: ColumnAt[A]]
	         (expression :ColumnMappingSQL[F, S, M, _], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
		new AlignableColumns(expression, permissions - MayExclude, IndexedSeq.empty :+ AlignableColumn(expression))

	def apply[F <: RowProduct, M[A] <: BaseColumn[V, A], V]
	         (expression :GenericColumnComponentSQL[F, M, V], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
		new AlignableColumns(expression, permissions - MayExclude, IndexedSeq.empty :+ AlignableColumn(expression))

	def apply[F <: RowProduct, M[O] <: MappingAt[O]]
	         (expression :ComponentSQL[F, M], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
	{
		val scope = spelling.scope
		val origin = expression.origin.anchored
		val alignableColumns = expression.mapping.columns.toIndexedSeq.collect {
			case column :TypedColumn[v, expression.Origin] @unchecked if !scope.isProhibited(origin.export(column)) =>
				AlignableColumn[F, M, v, expression.Origin](expression, permissions, column)
		}
		new AlignableColumns(expression, permissions, alignableColumns)
	}

	def apply[F <: RowProduct, S >: Grouped <: Single, M[O] <: MappingAt[O], V]
	         (expression :MappingSQL[F, S, M, V], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
	{
		val scope  = spelling.scope
		val export = expression.export
		val alignableColumns = expression.mapping.columns.toIndexedSeq.collect {
			case column :TypedColumn[v, expression.Origin] @unchecked if !scope.isProhibited(export.export(column)) =>
				AlignableColumn[F, S, M, V, v, expression.Origin](expression, permissions, column)
		}
		new AlignableColumns(expression, permissions, alignableColumns)
	}


	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (expression :InlineSQL[F, S, V], permissions :Permissions)(implicit spelling :SQLSpelling)
	       :AlignableColumns =
	{
		val columns = expression.items.flatMap { item =>
			spelling.potentialColumns(item.value, permissions).columns.map(
				new AlignableInlineColumn(expression, item.index, _)
			)
		}
		new AlignableColumns(expression, permissions - MayReorder, columns)
	}

	//todo: allow reordering of RecordSQL columns; allow reordering within particular levels/subexpressions
	def apply[F <: RowProduct, S >: Grouped <: Single, V <: Listing]
	         (expression :RecordSQL[F, S, V], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
	{
/*
		def collect(owner :RecordSQL[F, S, _ <: Listing], path :LabelPathPrefix, e :SQLExpression[F, S, _],
		            res :PassedArray[AlignableColumn.__])
				:PassedArray[AlignableColumn.__] =
			e match {
				case record :RecordSQL[F, S, _] =>
					(res /: record.items) { (acc, item) => collect(owner, path / item.key, item.value, acc) }
				case column :ColumnSQL[F, S, _] => //only on the first call path is empty, and then e :RecordSQL[_, _, _]
					res :+ AlignableColumn(owner, path.castFrom[LabelPathPrefix, LabelPath[_]], column)
				case _ => res :++ spelling.potentialColumns(e, permissions).columns
			}
		val alignableColumns = collect(expression, ~/, expression, PassedArray.empty)
*/
		val alignableColumns = expression.items.flatMap { item =>
			spelling.potentialColumns(item.value, permissions).columns.map(
				new AlignableLabeledColumn(expression, item.index, item.key, _)
			)
		}
		new AlignableColumns(expression, permissions - MayReorder, alignableColumns)
	}

	def apply[F <: RowProduct, S >: Grouped <: Single, V <: Listing]
	         (expression :IndexedSQL[F, S, V], permissions :Permissions)(implicit spelling :SQLSpelling)
			:AlignableColumns =
	{
		val alignableColumns = expression.paths.view.map(AlignableColumn(expression, _)).toIndexedSeq
		new AlignableColumns(expression, permissions, alignableColumns)
	}
}




trait AlignableColumn[-F <: RowProduct, -S >: Grouped <: Single, V] extends Serializable {
	val column     :ColumnSQL[F, S, V]
	def isDefault  :Boolean
	def isOptional :Boolean
	def isDirect   :Boolean

	def corresponds[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	               (other :AlignableColumn[F2, S2, V2])(implicit spelling :SQLSpelling) :Boolean =
		//We use spelling.reform, rather than accept an external reform, because we need a 'real' one, not an SQLAligner
		(this eq other) || matches(other)(spelling.reform, spelling) ||
			other.matches(this)(spelling.reform, spelling)

	def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	           (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) :Boolean

	//todo: renmae precedence and compare to something that more clearly describes their relationship
	def precedence[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	              (other :AlignableColumn[F2, S2, V2])(implicit spelling :SQLSpelling) :Opt[Int] =
		compare(other)(spelling.reform, spelling) match {
			case Got(value) => Got(value)
			case _ => other.compare(this)(spelling.reform, spelling) match {
				case Got(value) => Got(-value)
				case _ => Lack
			}
		}

	def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	           (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling): Opt[Int]


	protected def compatible[F1 <: RowProduct, S1 >: Grouped <: Single, V1]
	                        (other :AlignableColumn[F1, S1, V1])(implicit reform :Reform, spelling :SQLSpelling)
			:Boolean =
	{
		implicit def tag[X]: ClassTag[X] = ClassTag(classOf[Any])
		reform.compatible(column, other.column)(
			SQLConversion.supertype[V, Any], SQLConversion.supertype[V1, Any], spelling
		)
	}

	def groupKey(reform :Reform)(implicit spelling :SQLSpelling) :AlignableColumn.GroupKey =
		new AlignableColumn.GroupKey(this, reform)

	def canEqual(that :Any) :Boolean = that.isInstanceOf[AlignedColumn[_, _, _]]
}


object AlignableColumn {
	type __ = AlignableColumn[_ <: RowProduct, _ >: Grouped <: Single, _]

	def apply[F <: RowProduct, S >: Grouped <: Single, V](expr :ColumnSQL[F, S, V]) :AlignableColumn[F, S, V] =
		new AlignableColumn[F, S, V] {
			override val column = expr
			override def isDefault = true
			override def isOptional = false
			override def isDirect = true

			override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
			                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) =
				if (this == other) Got(0) else Lack

			override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
			                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) =
				this == other

			override def equals(that :Any) :Boolean = that match {
				case other :AlignableColumn[_, _, _] if other.canEqual(this) =>
					(this eq other) || (column eq other.column)
				case _ => false
			}
			override def canEqual(that :Any) :Boolean = that.getClass == getClass
			override def hashCode = column.hashCode
			override def toString :String = "|" + column + "|"
		}
/*

	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (expr :ColumnSQL[F, S, V], cmp :(AlignableColumn[_, _, _], AlignableColumn[_, _, _]) => Opt[Int])
			:AlignableColumn[F, S, V] =
		new AlignableColumn[F, S, V] {
			override val column = expr
			override def isDefault  = true
			override def isOptional = false
			override def isDirect = true

			override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
			                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) =
				Got(cmp(this, other))
//			override def groupHashCode = groupId
			override def toString :String = "|" + column + "|"
		}
*/

	def apply[F <: RowProduct, S >: Grouped <: Single, R <: Listing, P]
	         (record :IndexedSQL[F, S, R], path :LabelPath[P]) :AlignableColumn[F, S, _] =
		new AlignableRecordColumn(record, path, record.columns(path), false)

	def apply[F <: RowProduct, S >: Grouped <: Single, R <: Listing, V, P]
	         (record :RecordSQL[F, S, R], path :LabelPath[P], column :ColumnSQL[F, S, V]) :AlignableColumn[F, S, V] =
		new AlignableRecordColumn(record, path, column, false)

//	def apply[F <: RowProduct, S >: Grouped <: Single, N <: Label, V]
//	         (column :LabeledColumnSQL[F, S, N, V]) :AlignableColumn[F, S, V] =
//		AlignableAliasedColumn(column)
//
	def apply[F <:  RowProduct, S >: Grouped <: Single, V](column :AliasedSQL[F, S, V]) :AlignableColumn[F, S, V] =
		new AlignableAliasedColumn(column)


	def apply[F <: RowProduct, M[A] <: MappingAt[A], V, O >: F <: RowProduct]
	         (component :ComponentSQL[F, M] { type Origin = O }, permissions :Permissions, column :TypedColumn[V, O])
	         (implicit spelling :SQLSpelling) :AlignableColumn[F, Single, V] =
	{
		val export = component.origin.anchored.export(column)
		val scope = spelling.scope
		val default = scope.isDefault(export)
		val optional = !default || !scope.isMandatory(export) && permissions.mayExclude
		new AlignableMappingColumn(component, component \ column, default, optional)
	}

	def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: MappingAt[A], X, V, O]
	         (component :MappingSQL[F, S, M, X] { type Origin = O }, permissions :Permissions, column :TypedColumn[V, O])
	         (implicit spelling :SQLSpelling) :AlignableColumn[F, S, V] =
	{
		val export  = component.export.export(column)
		val scope   = spelling.scope
		val default = scope.isDefault(export)
		val optional = !default || !scope.isMandatory(export) && permissions.mayExclude
		new AlignableMappingColumn[F, S, M, X, MappingOf[V]#TypedColumnProjection, V, O](
			component, component \ column, default, optional
		)
	}

	def apply[F <: RowProduct, M[A] <: BaseColumn[V, A], V]
	         (column :GenericColumnComponentSQL[F, M, V]) :AlignableColumn[F, Single, V] =
		new AlignableMappingColumn[F, Single, M, V, M, V, column.Origin](column, column, true, false)

	def apply[F <: RowProduct, S >: Grouped <: Single, M[A] <: ColumnAt[A], V]
	         (column :ColumnMappingSQL[F, S, M, V]) :AlignableColumn[F, S, V] =
		new AlignableMappingColumn[F, S, M, V, M, V, column.Origin](column, column, true, false)


	def selectId[F <: RowProduct, V](name :String, column :ColumnSQL[F, Single, V]) :AlignableColumn[F, Single, V] =
		new AlignableSelectIdColumn(name, column)


	def unapply[F <: RowProduct, S >: Grouped <: Single, V](column :AlignableColumn[F, S, V]) :Opt[ColumnSQL[F, S, V]] =
		Got(column.column)

	final class GroupKey(val column :AlignableColumn[_ <: RowProduct, _ >: Grouped <: Single, _], val reform :Reform)
	                    (implicit val spelling :SQLSpelling)
		extends Serializable
	{
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :GroupKey => column corresponds other.column
			case _ => false
		}
		override def hashCode :Int = spelling.shape(column.column).hashCode //column.groupHashCode
		override def toString :String = column.toString
	}

	def strictPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) StrictPrecedence
		else if (default1 && default2) StrictDefaultPrecedence
		else StrictOptionalPrecedence

	def weakPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) WeakPrecedence
		else if (default1 && default2) WeakDefaultPrecedence
		else WeakOptionalPrecedence

	def labeledPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) RecordPrecedence
		else if (default1 && default2) RecordDefaultPrecedence
		else RecordOptionalPrecedence

	def customComponentPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) CustomComponentPrecedence
		else if (default1 && default2) CustomComponentDefaultPrecedence
		else CustomComponentOptionalPrecedence

	def nonDefaultComponentPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) NonDefaultComponentPrecedence
		else if (default1 && default2) NonDefaultComponentDefaultPrecedence
		else NonDefaultComponentOptionalPrecedence

	def componentPrecedence(optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (!optional1 && !optional2) ComponentPrecedence
		else if (default1 && default2) ComponentDefaultPrecedence
		else ComponentOptionalPrecedence

	def componentPrecedence(final_ :Boolean, custom :Boolean, default :Boolean,
	                        optional1 :Boolean, optional2 :Boolean, default1 :Boolean, default2 :Boolean) :Int =
		if (final_) strictPrecedence(optional1, optional2, default1, default2)
		else if (custom) customComponentPrecedence(optional1, optional2, default1, default2)
		else if (!default) nonDefaultComponentPrecedence(optional1, optional2, default1, default2)
		else componentPrecedence(optional1, optional2, default1, default2)

	//todo: modify the precedence of cases !optional vs default, etc.
	final val StrictPrecedence = 13
	final val StrictDefaultPrecedence = 12
	final val StrictOptionalPrecedence = 11
	final val WeakPrecedence = 3
	final val WeakDefaultPrecedence = 2
	final val WeakOptionalPrecedence = 1
	final val RecordPrecedence = 3
	final val RecordDefaultPrecedence = 2
	final val RecordOptionalPrecedence = 1
	final val CustomComponentPrecedence = 9
	final val CustomComponentDefaultPrecedence = 8
	final val CustomComponentOptionalPrecedence = 7
	final val NonDefaultComponentPrecedence = 6
	final val NonDefaultComponentDefaultPrecedence = 5
	final val NonDefaultComponentOptionalPrecedence = 4
	final val ComponentPrecedence      = 3
	final val ComponentDefaultPrecedence = 2
	final val ComponentOptionalPrecedence = 1
	final val IdentityPrecedence = 0
}


/** An `AlignableColumn` simply wrapping any `ColumnSQL`
  * and [[net.noresttherein.oldsql.sql.mechanics.AliganbleColumn.match matching]] any other `AlignableNestedColumn`
  * whose `column` is [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]]
  * according to the argument reform. This class is used as the terminator for chaining classes
  * [[net.noresttherein.oldsql.sql.mechanics.AlignableInlineColumn AlignableInlineColumn]] and
  * [[net.noresttherein.oldsql.sql.mechanics.AlignableNestedColumn AlignableNestedColumn]].
  */
class AlignableNestedColumn[-F <: RowProduct, -S >: Grouped <: Single, V]
                           (override val column :ColumnSQL[F, S, V],
                            override val isOptional :Boolean = false, override val isDefault :Boolean = true)
	extends AlignableColumn[F, S, V]
{
	override def isDirect: Boolean = true

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Boolean =
		other match {
			case _ :AlignableNestedColumn[_, _, _] => compatible(other)
			case _ => false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		other match {
			case _ :AlignableNestedColumn[_, _, _] => if (column eq other.column) Got(0) else Lack
			case _ => Lack
		}

	override def toString :String = "|*" + column + "|"
}


class AlignableInlineColumn[-F <: RowProduct, -S >: Grouped <: Single, V]
                           (val owner :SQLExpression[F, S, _], val order :Int, val nested :AlignableColumn[F, S, V])
	extends AlignableColumn[F, S, V]
{
	override val column     :ColumnSQL[F, S, V] = nested.column
	override def isDefault  :Boolean = nested.isDefault
	override def isOptional :Boolean = nested.isOptional
	override def isDirect   :Boolean = false

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Boolean =
		other match {
			case _ if this eq other => true
			case inline :AlignableInlineColumn[F2, S2, V2] =>
				order == inline.order && (nested matches inline.nested)
			case _ =>
				false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		other match {
			case _ if this eq other =>
				Got(0)
			case inline :AlignableInlineColumn[F2, S2, V2] if owner eq inline.owner =>
				order compare inline.order match {
					case 0 => nested.compare(inline.nested)
					case 1 => Got(strictPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
					case _ => Got(-strictPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
				}
			case _ =>
				Lack
		}

	override def toString :String = {
		@tailrec def append(e :AlignableColumn[F, S, V], res :StringBuilder) :String = e match {
			case labeled :AlignableLabeledColumn[F, S, V] =>
				append(labeled.nested, res ++= labeled.order.toString += ':' ++= labeled.label += '/')

			case inline :AlignableInlineColumn[F, S, V] =>
				append(inline.nested, res ++= order.toString += '.')
			case _ => (res ++= e.toString += '|').result()
		}
		append(this, new StringBuilder += '|' ++= owner.toString)
	}
}


class AlignableLabeledColumn[-F <: RowProduct, -S >: Grouped <: Single, V]
                            (override val owner :SQLExpression[F, S, _], override val order :Int, val label :Label,
                             override val nested :AlignableColumn[F, S, V], override val isOptional :Boolean = false)
	extends AlignableInlineColumn[F, S, V](owner, order, nested)
{
	def path :Opt[LabelPath[_]] = {
		@tailrec def append(e :AlignableColumn[_, _, _], init :LabelPath[_]) :Opt[LabelPath[_]] = e match {
			case labeled :AlignableLabeledColumn[_, _, _] =>
				append(labeled.nested, init / labeled.label)
			case record  :AlignableRecordColumn[_, _, _, _, _] =>
				Got(LabelPath.fromSeq(init.toList ++: record.path.toList))
			case alias   :AlignableAliasedColumn[_, _, _] => Got(init / (alias.column.alias :alias.column.alias.type))
			case _       :AlignableInlineColumn[_, _, _] => Got(init)
			case _ => Lack
		}
		append(nested, ~/[Label](label))
	}

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling): Boolean =
		other match {
			case _ if this eq other => true
			case labeled :AlignableLabeledColumn[_, _, _] =>
				(owner eq labeled.owner) && label == labeled.label && (nested matches labeled.nested)
			case inline :AlignableInlineColumn[_, _, _] =>
				order == inline.order && (nested matches inline.nested)
			case alias :AlignableAliasedColumn[_, _, _] if nested.isDirect =>
				path.contains(~/[alias.column.alias.type](alias.column.alias)) && compatible(other)
			case record :AlignableRecordColumn[_, _, _, _, _] =>
				this.path.contains(record.path) && compatible(other)
			case comp :AlignableMappingColumn[_, _, MappingAt, _, ColumnAt, _, _] @unchecked => path match {
				case Got(p) =>
					val name = comp.column.anchored.name
					p == ~/[name.type](name) && compatible(other)
				case _ => false
			}
			case _ =>
				false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		other match {
			case _ if this eq other =>
				Got(0)
			case labeled :AlignableLabeledColumn[_, _, _] if owner eq labeled.owner =>
				order compare labeled.order match {
					case 0 => nested.compare(labeled.nested)
					case 1 => Got(labeledPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
					case _ => Got(-labeledPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
				}
			case _ => super.compare(other)
		}

}

/*
object AlignableLabeledColumn {
	def apply[F <: RowProduct, S >: Grouped <: Single, V]
	         (owner: SQLExpression[F, S, _], order: Int, label: Label, nested: AlignableColumn[F, S, V])
			:AlignableLabeledColumn[F, S, V] =
		new AlignableLabeledColumn(owner, order, label, nested)

	def unapply[F <: RowProduct, S >: Grouped <: Single, V](column :AlignableColumn[F, S, V])
			:Option[(SQLExpression[F, S, _], Int, Label, AlignableColumn[F, S, V])] =
		column match {
			case nested :AlignableLabeledColumn[F, S, V] @unchecked =>
				Some((nested.owner, nested.order, nested.label, nested.nested))
			case _ =>
				None
		}
	def unapply(that :Any)
			:Option[(SQLExpression[F, S, _], Int, Label, AlignableColumn[F, S, _]) forSome {
				type F <: RowProduct; type S >: Grouped <: Single
			}] =
		that match {
			case nested :AlignableLabeledColumn[f, s, _] @unchecked =>
				Some((nested.owner, nested.order, nested.label, nested.nested))
			case _ =>
				None
		}
}
*/


class AlignableMappingColumn[-F <: RowProduct, -S >: Grouped <: Single, T[A] <: MappingAt[A], X, M[A] <: ColumnAt[A], V, O]
                            (val owner :MappingSQL[F, S, T, X] { type Origin = O },
                             override val column :ColumnMappingSQL[F, S, M, V] { type Origin = O },
                             override val isDefault :Boolean, override val isOptional :Boolean)
	extends AlignableColumn[F, S, V]
{
	override def isDirect = true

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) :Boolean =
		other match {
			case same if this eq same => true
			case comp :AlignableMappingColumn[F2, S2, MappingAt, _, ColumnAt, V, _] @unchecked =>
				val mapping = owner.mapping
				val otherMapping = comp.owner.mapping
				val columnMapping = column.mapping.refine
				val otherColumnMapping = comp.column.mapping.refine
				otherMapping.counterpartOpt(mapping, columnMapping).contains(otherColumnMapping) ||
					mapping.counterpartOpt(otherMapping, otherColumnMapping).contains(columnMapping) ||
					otherMapping.withOrigin[O].contains(columnMapping) &&
						otherMapping.withOrigin[O].export(columnMapping) == otherMapping.export(otherColumnMapping) ||
					mapping.contains(otherColumnMapping.withOrigin[O]) &&
						mapping.export(otherColumnMapping.withOrigin[O]) == mapping.export(columnMapping)

			case record :AlignableRecordColumn[F2, S2, _, V2, _] =>
				//todo: better (deeper) name comparison, using various concatenation strategies for path elements
				record.path == ~/(record.path.last) && column.anchored.name == record.path.last && compatible(other)
					//we should ask reform instead of doing this manually, but it requires conversions to the same type
//					(this.column.selectForm comparable column.selectForm)
//			case AlignableLabeledColumn(column) =>
//				//todo: better (deeper) name comparison, using various concatenation strategies for path elements
//				this.column.anchored.name == column.alias &&
//					//we should ask reform instead of doing this manually, but it requires conversions to the same type
////					(this.column.selectForm comparable column.selectForm)
//					reform.compatible(this.column, column)(
//						SQLConversion.supertype[V, Any], SQLConversion.supertype[V2, Any], spelling
//					)
			case alias :AlignableAliasedColumn[F2, S2, V2] =>
				column.anchored.name == alias.column.alias && compatible(other)
			case _ =>
				false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		other match {
			case self if this eq self =>
				Got(0)
			case comp :AlignableMappingColumn[F2, S2, MappingAt, _, ColumnAt, V2, _] @unchecked if owner eq comp.owner =>
				val index = owner.anchored.columns.sureIndexOf(column.anchored)
				val otherIndex = owner.anchored.columns.sureIndexOf(comp.column.anchored.withOrigin[O])
				//todo: the following throws StackOverflowError on compilation. Report it
//				(owner :MappingSQL.__) match {
//					case comp :ComponentSQL.__ =>
//				}
				val (isFinal, isCustom) = (owner :SQLExpression.__) match {
					case comp :ComponentSQL.__ => (comp.`->isFinal`, comp.isCustom)
					case _ => (false, false)
				}
				index compare otherIndex match {
					case 0 => Got(0)
					case 1 => Got(componentPrecedence(
						isFinal, isCustom, owner.isDefault, isOptional, other.isOptional, isDefault, other.isDefault
					))
					case _ => Got(-componentPrecedence(
						isFinal, isCustom, owner.isDefault, isOptional, other.isOptional, isDefault, other.isDefault
					))
				}
			case _ =>
				Lack
		}

	override def toString :String = "|" + owner.mapping.mappingName + "." + column + "|"
}

/*
case class AlignableComponentColumn[-F <: RowProduct, T[A] <: MappingAt[A], M[A] <: BaseColumn[V, A], V, O >: F <: RowProduct]
                                   (owner :ComponentSQL[F, T] { type Origin = O },
                                    override val column :GenericColumnComponentSQL[F, M, V] { type Origin = O },
                                    override val isDefault :Boolean, override val isOptional :Boolean)
	extends AlignableColumn[F, Single, V]
{
	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(reform :Reform)(implicit spelling :SQLSpelling) :Boolean =
		other match {
			case AlignableComponentColumn(otherOwner, otherColumn, _, _) =>
					otherOwner.mapping.counterpartOpt(owner.mapping, column.mapping).contains(otherColumn.mapping) ||
					owner.mapping.counterpartOpt(otherOwner.mapping, otherColumn.mapping).contains(column.mapping)

			case AlignableRecordColumn(_, path, column, _) =>
				//todo: better (deeper) name comparison, using various concatenation strategies for path elements
				path == LabelPath(column.alias) && this.column.anchored.name == column.alias &&
					//we should ask reform instead of doing this manually, but it requires conversions to the same type
					(this.column.form comparable column.selectForm)
//					reform.compatible(column, this.column)
			case _ =>
				false
		}
	override def toString :String = owner.mapping.mappingName + "." + column
}
*/


class AlignableRecordColumn[-F <: RowProduct, -S >: Grouped <: Single, R <: Listing, V, P]
                           (val record :RecordSQL[F, S, R], val path :LabelPath[P],
                            override val column :ColumnSQL[F, S, V], override val isOptional :Boolean)
	extends AlignableColumn[F, S, V]
{
	override def isDefault   = true
	override def isDirect    = true

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) :Boolean =
		other match {
			case _ if this eq other => true
			case record :AlignableRecordColumn[F2, S2, _, V2, _] => path == record.path && compatible(other)
			case alias  :AlignableAliasedColumn[F2, S2, V2] => path.last == alias.column.alias && compatible(other)
			case _ => false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		other match {
			case _ if this eq other =>
				Got(0)
			case recordCol :AlignableRecordColumn[F2, S2, _, V2, _] if record eq recordCol.record =>
				record match {
					case index :IndexedSQL[F, S, _] =>
						index.paths.indexOf(path) compare index.paths.indexOf(recordCol.path) match {
							case 0 => Got(0)
							case 1 => Got(labeledPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
							case _ => Got(-labeledPrecedence(isOptional, other.isOptional, isDefault, other.isDefault))
						}
					case _ =>
						@tailrec def compare(e1 :SQLExpression[F, S, _], path1 :List[Label],
						                     e2 :SQLExpression[F2, S2, _], path2 :List[Label]) :Int =
							(path1, path2) match {
								case (h1::t1, h2::t2) =>
									assert(e1.isInstanceOf[RecordSQL[_, _, _]],
										"Record `" + record + "` does not contain path " + path + ": element `" +
											e1 + "`: " + e1.className + " at " +
											path.toList.dropRight(path1.length).mkString("", "/", "") +
											" is not a RecordSQL."
									)
									assert(e1.isInstanceOf[RecordSQL[_, _, _]],
										"Record `" + record + "` does not contain path " + recordCol.path
											+ ": element `" + e2 + "`: " + e2.className + " at " +
											recordCol.path.toList.dropRight(path1.length).mkString("", "/", "") +
											" is not a RecordSQL."
									)
									val record1 = e1.castFrom[SQLExpression[F, S, _], RecordSQL[F, S, _]]
									val record2 = e2.castFrom[SQLExpression[F2, S2, _], RecordSQL[F2, S2, _]]
									val i1 = record1.items.indexWhere(_.key == h1)
									val i2 = record2.items.indexWhere(_.key == h2)
									assert(i1 >= 0, "Record `" + record + "` does not contain path " + path + ".")
									assert(i2 >= 0, "Record `" + record + "` des not contain path " + recordCol.path + ".")

									if (i1 < i2) -1
									else if (i1 > i2) 1
									else compare(record1.items(i1).value, t1, record2.items(i2).value, t2)

								case (Nil, Nil) => 0
								case (Nil, _) => -1 //these two can't really happen
								case _ => 1
							}
						Got(compare(record, path.toList, recordCol.record, recordCol.path.toList))
				}
			case _ =>
				Lack
		}

	override def toString :String = "|" + path.toString + "/" + column + "|"
}
/*


case class AlignableLabeledColumn[-F <: RowProduct, -S >: Grouped <: Single, L <: Label, V]
                                 (override val column :LabeledColumnSQL[F, S, L, V])
	extends AlignableColumn[F, S, V]
{
	override def isDefault  = true
	override def isOptional = false

	implicit private def tag[X] :ClassTag[X] = ClassTag(classOf[Any])

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(reform :Reform)(implicit spelling :SQLSpelling) :Boolean =
		other match {
			case AlignableLabeledColumn(labeled) =>
				column.alias == labeled.alias && reform.compatible(column, labeled)(
					SQLConversion.supertype[V, Any], SQLConversion.supertype[V2, Any], spelling
				)
			case AlignableRecordColumn(_, _, labeled, _) =>
				column.alias == labeled && reform.compatible(column, labeled)(
					SQLConversion.supertype[V, Any], SQLConversion.supertype[V2, Any], spelling
				)
			case AlignableAliasedColumn(aliased) =>
				column.alias == aliased.alias && reform.compatible(column, aliased)(
					SQLConversion.supertype[V, Any], SQLConversion.supertype[V2, Any], spelling
				)
			case _ => false
		}

	override def toString :String = "|" + column.toString + "|"
}
*/


//Consider: adding an owner expression.
// Its lack is not a biggie, though, because if it's a RecordSQL, then AlignableRecordColumn is used instead of this.
class AlignableAliasedColumn[-F <: RowProduct, -S >: Grouped <: Single, V](override val column :AliasedSQL[F, S, V])
	extends AlignableColumn[F, S, V]
{
	override def isDefault  = true
	override def isOptional = false
	override def isDirect   = true

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) :Boolean =
		other match {
			case alias :AlignableAliasedColumn[F2, S2, V2] =>
				column.alias == alias.column.alias && compatible(other)
//			case AlignableLabeledColumn(labeled) =>
//				column.alias == labeled.alias && reform.compatible(column, labeled)(
//					SQLConversion.supertype[V, Any], SQLConversion.supertype[V2, Any], spelling
//				)
			case record :AlignableRecordColumn[F2, S2, _, V2, _] =>
				column.alias == record.path.last && compatible(other)
			case _ => false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		if (column eq other.column) Got(0) else Lack

	override def toString :String = "|" + column + "|"
}


class AlignableSelectIdColumn[-F <: RowProduct, V](val name :String, override val column :ColumnSQL[F, Single, V])
	extends AlignableColumn[F, Single, V]
{
	override def isDefault  = true
	override def isOptional = false
	override def isDirect = true

	override def matches[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other :AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling :SQLSpelling) :Boolean =
		other match {
			case select :AlignableSelectIdColumn[F2, V2] => name == select.name
			case _ => false
		}

	override def compare[F2 <: RowProduct, S2 >: Grouped <: Single, V2]
	                    (other: AlignableColumn[F2, S2, V2])(implicit reform :Reform, spelling: SQLSpelling) :Opt[Int] =
		if (other eq this) Got(0) else Lack

	override def toString :String = "|" + name + "=" + column + "|"
}

