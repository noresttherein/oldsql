package net.noresttherein.oldsql.sql.mechanics

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.{~, ChainGet}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.SQLWriteForm
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, By, DecoratedFrom, Expanded, FromSome, GroupBy, GroupByClause, NonParam, ParamClause, RowProduct}
import net.noresttherein.oldsql.sql.ParamClause.{ParamAt, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{AggregateOf, AggregateOfGeneralized, NonEmptyFrom, ParameterizedFrom, ParamlessFrom, SubselectOf}
import net.noresttherein.oldsql.sql.ast.{BoundParam, JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{ver, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.{JoinParameterization, MappedParameterization, UngroupedParameterization}






/** A temporary artefact of translating an SQL AST [[net.noresttherein.oldsql.sql.SQLExpression expression]]
  * to an [[net.noresttherein.oldsql.sql.Incantation Incantation]]. It represents a fragment of an SQL or DML
  * expression - not necessarily corresponding to a syntactically complete subexpression - together with
  * context information necessary for ensuring its validity, in particular used table aliases
  * and column names, and a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting the statement's parameters.
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext]]
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization]]
  * @author Marcin Mościcki
  */
@SerialVersionUID(ver)
class SpelledSQL[-P](val sql :ChunkedString, val context :SQLContext[P], val setter :SQLWriteForm[P])
	extends Serializable
{
	def isEmpty  :Boolean = sql.isEmpty
	def nonEmpty :Boolean = sql.nonEmpty

	def +[X <: P](form :SQLWriteForm[X]) :SpelledSQL[X] = new SpelledSQL(sql, context, this.setter + form)
	def +(char :Char) :SpelledSQL[P] = new SpelledSQL(sql + char, context, setter)
	def +(string :String) :SpelledSQL[P] = new SpelledSQL(sql + string, context, setter)
	def +(string :ChunkedString) :SpelledSQL[P] = new SpelledSQL(sql + string, context, setter)
	def +[X <: P, E <: RowProduct](string :SQLContext[P] => SpelledSQL[X]) :SpelledSQL[X] = {
		val suffix = string(context)
		new SpelledSQL(sql + suffix.sql, suffix.context, setter + suffix.setter)
	}
	def +[X <: P](sql :SpelledSQL[X]) :SpelledSQL[X] =
		new SpelledSQL(this.sql + sql.sql, sql.context, setter + sql.setter)

	def +(param :BoundParam[_]) :SpelledSQL[P] = {
		val f = param.writeForm.unmap { _ :Any => () }
		new SpelledSQL(this.sql + f.param, context, setter + param.form)
	}
	//todo: verify all 'reduce' operations to see if we are not losing forms!!!
	def +:[X <: P](form :SQLWriteForm[X]) :SpelledSQL[X] = new SpelledSQL(sql, context, form + this.setter)
	def +:(char :Char) :SpelledSQL[P] = new SpelledSQL(char.toString +: sql, context, setter)
	def +:(string :String) :SpelledSQL[P] = new SpelledSQL(string +: sql, context, setter)
	def +:(string :ChunkedString) :SpelledSQL[P] = new SpelledSQL(string + sql, context, setter)
	def +:[X <: P](sql :SpelledSQL[X]) :SpelledSQL[X] = new SpelledSQL(sql.sql + this.sql, context, sql.setter + setter)

	def +:(param :BoundParam[_]) :SpelledSQL[P] = {
		val f = param.writeForm.unmap { _ :Any => () }
		new SpelledSQL(f.param +: this.sql, context, param.form + setter)
	}

	/** Updates the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] of this SQL fragment
	  * by applying the given function to include an additional condition for the ''where'' clause. The SQL text of
	  * the result is taken to be a boolean condition which should be combined in a logical conjunction with the
	  * rest of the ''where'' clause, and its context is the update of this instance's context with said condition
	  * already included. The process is similar to monadic ''flat map'', with the context being passed through
	  * all chained operations.
	  */
	def &&[X <: P](condition :SQLContext[P] => SpelledSQL[X]) :SpelledSQL[X] = {
		val suffix = condition(context)
		new SpelledSQL(sql, context && suffix, setter + suffix.setter)
	}

	def adapt[X](f :X => P) :SpelledSQL[X] = new SpelledSQL(sql, context adapt f, setter unmap f)

	def compose[X](f :X =?> P) :SpelledSQL[X] = new SpelledSQL(sql, context compose f, setter compose f)


	override def toString :String = {
		val prefix = "SQL('" +: (sql + "' " + setter.toString + "; FROM ")
		val terminator = if (context.groupings > 0) " GROUP BY #" + context.groupings + ")" else ")"
		val where =
			if (context.whereReversed.isEmpty) terminator
			else context.whereReversed.reverseIterator.mkString(" WHERE ", " && ", terminator)
		context.tablesReversed.reverseIterator.mkString(prefix.toString, ", ", where)
	}
}






object SpelledSQL {
	private[sql] final val ver = 1L

	private[this] val empty =
		new SpelledSQL[Any](ChunkedString.empty, SQLContext(), SQLWriteForm.empty)

	def apply() :SpelledSQL[Any] = empty

	def apply(sql :String) :SpelledSQL[Any] =
		new SpelledSQL(ChunkedString(sql), SQLContext(), SQLWriteForm.empty)

	def apply(sql :ChunkedString) :SpelledSQL[Any] =
		new SpelledSQL(sql, SQLContext(), SQLWriteForm.empty)

	def apply[P](context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(ChunkedString.empty, context, SQLWriteForm.empty)

	def apply[P](sql :String, context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(ChunkedString(sql), context, SQLWriteForm.empty)

	def apply[P](sql :ChunkedString, context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(sql, context, SQLWriteForm.empty)

	def apply[P](sql :String, context :SQLContext[P], form :SQLWriteForm[P]) :SpelledSQL[P] =
		new SpelledSQL(ChunkedString(sql), context, form)

	def apply[P](sql :ChunkedString, context :SQLContext[P], form :SQLWriteForm[P]) :SpelledSQL[P] =
		new SpelledSQL(sql, context, form)


	@inline def unapply[P](sql :SpelledSQL[P]) :Opt[(ChunkedString, SQLContext[P], SQLWriteForm[P])] =
		Got((sql.sql, sql.context, sql.setter))



	/** Non-local context related to an SQL fragment
	  * (of some [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]).
	  * It contains primarily the aliases of all the tables in the ''from'' clause of the ''select'' using the fragment,
	  * as well as those of any outer ''selects'' in which the current ''select'' is embedded.
	  * @param tablesReversed an indexed table containing aliases of all relations, with indexing congruent with that
	  *                       of [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  *                       expressions used in the SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
	  *                       being translated. This means that the first alias in the sequence is that for the ''last''
	  *                       relation in the ''from'' clause, and so on. The exception are expressions based on
	  *                       ''group by'' clauses, where the grouping relations receive no aliases and indexing stays
	  *                       the same as for the grouped ''from'' clause (with the aliases of the aggregated relations
	  *                       remaining available, unlike the relations rows themselves for the SQL expression).
	  *                       This list includes (unused) aliases for all
	  *                       occurrences of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] type in the ''from''
	  *                       clause, as well as placeholder entries for grouping relations ''from outer selects only''
	  *                       in order to retain consistency with numbering used by `RowProduct` for its joined relations.
	  *                       An empty string signifies that the relation does not have an alias ''and'' its columns
	  *                       should be rendered using their unqualified names, without the table name. This situation
	  *                       is however an exception and should be used only if the alternative would result in
	  *                       invalid SQL. Normally, if the table does not feature an alias in the ''from'' clause,
	  *                       its context entry should be the unqualified table name itself, and its columns
	  *                       should be qualified with the table name instead.
	  * @param whereReversed  SQL fragments coming from individual join conditions (and other `RowProduct` subcomponents),
	  *                       which were not included in a ''join ... on'' clause and should become part of a logical
	  *                       conjunction forming the ''where'' clause. This in particular includes conditions
	  *                       present on `From` and `JoinParam`. The conditions are in the reverse order of their
	  *                       appearance in the ''from'' clause.
	  * @param outerOffset    the size of the ''from'' clause of the most deeply nested select of the associated SQL
	  *                       expression/the offset in the alias table of the first table from the outer select,
	  *                       or `-1` for top clauses (not subselects).
	  * @param groupings      combined number of [[net.noresttherein.oldsql.sql.GroupBy GroupBy]],
	  *                       [[net.noresttherein.oldsql.sql.By By]] and [[net.noresttherein.oldsql.sql.GroupParam]]
	  *                       (including their type aliases) elements of the ''group by'' clause of the SQL fragment.
	  *                       It is the number of grouping relations which are ''not'' included in the alias index.
	  *                       Zero signifies an aggregate clause, while `-1` is used for `RowProduct` types not
	  *                       conforming to `GroupByClause`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.spellingContext]]
	  */ //consider: parameterizing it with the RowProduct
	@SerialVersionUID(ver)
	class SQLContext[-P](/** See [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]. */
	                     val tablesReversed :IndexedSeq[String]  = IndexedSeq.empty[String],
	                     /** See [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]. */
	                     val whereReversed  :List[SpelledSQL[P]] = Nil,
	                     /** See [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]. */
	                     val outerOffset :Int = -1,
	                     /** See [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]. */
	                     val groupings   :Int = -1)
		extends Serializable
	{
		/** A copy constructor used by all methods adding to the context so subclasses need override only this method. */
		def reset[X](tablesReversed :IndexedSeq[String] = tablesReversed,
		             whereReversed :List[SpelledSQL[X]] = Nil,
		             outerOffset :Int = outerOffset, groupings :Int = groupings) :SQLContext[X] =
			new SQLContext(tablesReversed, whereReversed, outerOffset, groupings)

		/** A copy constructor used by all methods adding to the context, shielding users from its manual creations
		  * by providing all member values. It is just a delegate
		  * to [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.reset reset]], providing a default value
		  * for `whereReversed`.
		  */
		def copy[X <: P](tablesReversed :IndexedSeq[String] = tablesReversed,
		                 whereReversed :List[SpelledSQL[X]] = whereReversed,
		                 outerOffset :Int = outerOffset, groupings :Int = groupings) :SQLContext[X] =
			reset(tablesReversed, whereReversed, outerOffset, groupings)

		/** Adapts this context to a new parameter type. Currently this only adapts
		  * all [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereReversed whereReversed]] chunks.
		  */
		def adapt[X](f :X => P) :SQLContext[X] = reset(whereReversed = whereReversed.map(_.adapt(f)))

		/** Adapts this context to a new parameter type. Currently this only composes
		  * all [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereReversed whereReversed]] chunks.
		  */
		def compose[X](f :X =?> P) :SQLContext[X] = reset(whereReversed = whereReversed.map(_.compose(f)))

		/** A virtual constructor of a default context for an empty SQL fragment used instead of the global
		  * factory to propagate custom implementations for the use by the whole 'spelling' process.
		  */
		def fresh[X] = new SQLContext[X]()

		private[SpelledSQL] def isLegal(alias :String) :Boolean =
			alias != null && alias.length > 0 && !alias.startsWith("?") && !alias.startsWith("<")

		/** SQL fragments coming from individual join conditions (and other `RowProduct` subcomponents),
		  * which were not included in a ''join ... on'' clause and should become part of a logical conjunction
		  * forming the ''where'' clause. This in particular includes conditions present on `From` and `JoinParam`.
		  */
		def where :List[SpelledSQL[P]] = whereReversed.reverse

		/**	Aliases of all real tables in the ''from'' clause of the SQL fragment, not including aliases of tables from
		  * outer ''selects'' or synthetic relations corresponding to unbound parameters or grouping expressions,
		  * in the order in which they appear in the linearization of the ''from'' clause.
		  */
		def fromClause :Iterator[String] =
			if (outerOffset < 0) tablesReversed.reverseIterator.filter(isLegal)
			else tablesReversed.view.take(outerOffset).reverseIterator.filter(isLegal)

		/** Aliases of all tables in the ''from'' clause of the translated SQL fragment, including aliases of tables
		  * from the outer ''selects'', but not including any synthetic relations (for unbound parameters and grouping
		  * expressions).
		  */
		def tables :Set[String] = tablesReversed.view.filter(isLegal).toSet

		/** Checks if the context contains the given table alias. This takes into account only usable aliases:
		  * Those of the aggregated tables from outer ''selects'' are not included.
		  */
		def contains(table :String) :Boolean = tablesReversed.contains(table)

		/** Returns a unique alias (different from all tables in the scope) based on the given name.
		  * @return `proposed` if the alias is not currently used, or first unique alias `"proposed$i"`
		  *         for `i = 1, 2, ...`
		  */
		def newAlias(proposed :String) :String =
			if (proposed.length == 0)
				newAlias("table")
			else if (!tablesReversed.contains(proposed))
				proposed
			else {
				var i = 1
				var indexed = proposed + i
				while (tablesReversed.contains(indexed)) {
					i += 1
					indexed = proposed + i
				}
				indexed
			}

		/** The alias for the `idx-th` last table (that is, counting from the rightmost table in the ''from'' clause).
		  * This is the alias corresponding to a
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] with that offset.
		  * An empty string indicates that the table does not feature an alias and its columns should be rendered
		  * using their unqualified names (without prefixing them with the name of the table).
		  */
		@throws[IllegalArgumentException]("if the relation at the given index is synthetic and does not correspond " +
		                                  "to an actual table in the FROM clause.")
		@throws[IndexOutOfBoundsException]("if the index is less than zero or greater than the number of all relations" +
		                                   "relations in the visible scope, including synthetic relations.")
		def apply(idx :Int) :String = {
			val alias = tablesReversed(idx)
			if (!isLegal(alias))
				throw new IllegalArgumentException(
					idx.toString + "-th relation (counting from the right) is not a valid table. " +
						"Unbound parameters and grouping expressions cannot be aliased: '" + alias + "'"
				)
			alias
		}

		/** The alias used for the given relation expression by the associated SQL. */
		@throws[IllegalArgumentException]("if the relation at the given index is synthetic and does not correspond " +
		                                  "to an actual table in the FROM clause.")
		@throws[IndexOutOfBoundsException]("if the index is less than zero or greater than the number of all relations" +
		                                   "relations in the visible scope, including synthetic relations.")
		def apply(table :JoinedRelation.*) :String = this(table.offset)

		/** Adds a new table alias at index `0`, shifting back existing table aliases. */
		@throws[IllegalStateException]("if the context corresponds to an aggregate select (with a group by clause).")
		@throws[IllegalArgumentException]("if the alias is already in use.")
		def join(table :String) :SQLContext[P] =
			if (groupings >= 0)
				throw new IllegalStateException(s"Cannot join '$table' with an aggregate clause: $this.")
			else if (table.length > 0 && tablesReversed.contains(table))
				throw new IllegalArgumentException(s"$this already contains table named '$table' in scope.")
			else if (outerOffset < 0)
				copy(tablesReversed = table +: tablesReversed)
			else
				copy(tablesReversed = table +: tablesReversed, outerOffset = outerOffset + 1)

		/** Adds a new table alias as the first (index `0`) element of the ''from'' clause of a nested ''select''.
		  * @return `this.subselect.join(table)`.
		  */
		@throws[IllegalArgumentException]("if the alias is already in use.")
		def subselect(table :String) :SQLContext[P] =
			if (table.length > 0 && tablesReversed.contains(table))
				throw new IllegalArgumentException(s"$this already contains table named '$table' in scope.")
			else if (groupings >= 0) {
				val aliases = table +: Iterator.tabulate(groupings)("<grouping" + _ + ">") ++:
					(if (outerOffset > 0) tablesReversed.drop(outerOffset) else ArraySeq.empty[String])
				copy(tablesReversed = aliases, groupings = -1, outerOffset = 1)
			} else
				copy(tablesReversed = table +: tablesReversed, groupings = -1, outerOffset = 1)

		/** Marks the beginning of a new nested ''select''. This does not change current alias indexing unless
		  * the current ''from'' clause is aggregated, in which case synthetic placeholders in the number of
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]] elements
		  * are prepended to the index. It resets however the `outerOffset` to zero, corresponding to an empty
		  * ''from'' clause.
		  */
		def subselect :SQLContext[P] =
			if (groupings >= 0) { //groupings == 0 means an aggregate (not group by) clause
				val aliases = Iterator.tabulate(groupings)("<grouping" + _ + ">") ++:
					(if (outerOffset > 0) tablesReversed.drop(outerOffset) else ArraySeq.empty[String])
				copy(tablesReversed = aliases, groupings = -1, outerOffset = 0)
			} else
				copy(groupings = -1, outerOffset = 0)

		/** Pushes a placeholder alias for a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instance
		  * at the index `0` of the table aliases in scope, shifting back existing aliases.
		  * Use `grouped` method instead to represent a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]].
		  * The actual alias will have "?" prepended to it and, if it is not unique, an additional suffix.
		  */
		def param(alias :String) :SQLContext[P] = join(newAlias("?" + alias))

		/** Adds another ''group by'' clause expression corresponding to either
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], [[net.noresttherein.oldsql.sql.By By]] or
		  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] element.
		  */
		@throws[IllegalStateException]("if the associated clause is an instance of Aggregated[_].")
		def grouped :SQLContext[P] =
			if (groupings == 0)
				throw new IllegalStateException("Cannot group an aggregated clause " + this + ".")
			else copy(
				groupings = if (groupings > 0) groupings + 1 else 1,
				outerOffset = if (outerOffset >= 0) outerOffset + 1 else -1
			)

		/** Marks the ''select'' as ''aggregated'', i.e. containing aggregate functions in its ''select'' clause.
		  * This corresponds to an artificial [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] wrapper of
		  * a [[net.noresttherein.oldsql.sql.FromClause FromClause]]. The effect is the same that a ''group by''
		  * clause with zero grouping expressions would have.
		  */
		@throws[IllegalStateException]("if the context corresponds to an aggregate select (with a group by clause).")
		def aggregate :SQLContext[P] =
			if (groupings >= 0)
				throw new IllegalStateException("Cannot aggregate an already aggregated clause: " + this + ".")
			else copy(groupings = 0)

		/** Adds the SQL for a boolean expression which should be included in the ''where'' clause of a larger SQL
		  * fragment.
		  */
		def &&[X <: P](condition :SpelledSQL[X]) :SQLContext[X] = copy(whereReversed = condition.sql::this.whereReversed)

		def shrink(relations :Int = 1) :SQLContext[P] =
			if (relations > 0)
				if (relations <= groupings)
					copy(groupings = groupings - relations)
				else if (groupings < 0)
					if (outerOffset >= 0)
						if (relations <= outerOffset)
							copy(tablesReversed.drop(relations), outerOffset = outerOffset - relations)
						else
							throw new IllegalArgumentException(
								s"Cannot shrink $this into the outer select past $outerOffset relations ($relations)."
							)
					else
						if (tablesReversed.sizeIs >= relations)
							copy(tablesReversed.drop(relations))
						else
							throw new IllegalArgumentException(
								s"Cannot shrink $this of ${tablesReversed.size} tables by $relations relations."
							)
				else if (outerOffset >= 0)
					if (relations - groupings <= outerOffset)
						copy(
							tablesReversed = tablesReversed.drop(relations - groupings),
							outerOffset = outerOffset - (relations - groupings),
							groupings = -1
						)
					else
						throw new IllegalArgumentException(
							s"Cannot shrink $this into the outer select past $outerOffset relations ($relations)."
						)
				else if (tablesReversed.sizeIs >= relations - groupings)
					copy(tablesReversed = tablesReversed.drop(relations), groupings = -1)
				else
					throw new IllegalArgumentException(
						s"Cannot shrink $this by more than the existing number of relations ($relations)."
					)
			else if (relations == 0)
				this
			else
				throw new IllegalArgumentException("Cannot shrink " + this + " by " + relations + " relations.")


		override def toString :String = {
			val terminator = if (groupings > 0) " GROUP BY #" + groupings + ")" else ")"
			val where =
				if (whereReversed.isEmpty) terminator
				else whereReversed.reverseIterator.mkString(" WHERE ", " && ", terminator)
			tablesReversed.reverseIterator.mkString("SQLContext(FROM ", ", ", where)
		}
	}



	object SQLContext {
		private[this] val empty = new SQLContext[Any](ArraySeq.empty[String])

		/** An empty context. */
		def apply() :SQLContext[Any] = empty

		def apply(aliases :IndexedSeq[String], fromClauseSize :Int) :SQLContext[Any] =
			new SQLContext(aliases.reverse, outerOffset = fromClauseSize)
	}




	/** Parameter context of an SQL `PreparedStatement` as a function of a single parameter `Ps` representing
	  * all [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of ''from'' clause `F`.
	  * This class serves two purposes:
	  *   1. it contains a list of [[net.noresttherein.oldsql.schema.SQLWriteForm forms]] setting the values
	  *      of all parameters for a particular statement,
	  *   1. and is a factory of accessors for `Ps` which return values of individual
	  *      [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters based on
	  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] instances from `F` for these parameters.
	  *      This class is used in conjunction with [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]],
	  *      carrying information about that statement fragment. As the latter is expanded, so too is this object:
	  *      primarily by adding additional forms with `:+`, `+:` or swapping the whole list with
	  *      [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.reset reset]] method. These forms
	  *      are either constant forms (with a `Unit` argument) of bound
	  *      [[net.noresttherein.oldsql.sql.ast.BoundParam BoundParam]]  expressions, or the mapping forms
	  *      taken from [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mappings of unbound parameters.
	  *      Adapting of the former to the required argument type `Ps` can happen by a trivial constant function
	  *      returning `()`, while the composition of the latter consists of the accessor function `Ps => P` returned by
	  *      [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]]
	  *      for a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]`[_ >: F, P]`, being the
	  *      synthetic relation taken from an `ParamClause` pseudo join, and - if the value required is not that of the
	  *      parameter `P` itself, but a derived one `X` represented by a component of `UnboundParam` - an optional selector
	  *      `P => X` for the particular parameter component taken from `UnboundParam`, as with all mappings.
	  *      Additionally, the polymorphic function returning the parameter accessors is updated recursively by following
	  *      the structure of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] of the statement, in order
	  *      to reflect updated indexing and, in case of parameter relations, expanding also the parameter type `Ps`.
	  *      This happens by factory methods defined by this class and corresponding to particular `Adjoin` subtypes.
	  *
	  * An instance of `Parameterization[Ps, F]` does not declare that it can provide a getter from `Ps`
	  * for any parameter of any instance of `F`; rather, it states that it can provide the getters
	  * for all parameters listed by type `F` itself (that is, in its non-wildcard suffix).
	  * For this reason, the origin (domain) type of any relation resolvable to a parameter getter must
	  * be a supertype of `F`, as it is declared and returned by any instance of `F`.
	  *
	  * The values of this class can be created by factory methods in the companion object, either based on
	  * an existing `RowProduct` instance of `F`, or by expanding
	  * a [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.paramless paramless]] instance
	  * in the manner described above.
	  *
	  * @tparam Ps a composite type combining the values of all individual unbound parameters in `F`, typically
	  *            `F#Params` [[net.noresttherein.oldsql.collection.Chain Chain]] (for concrete `F`).
	  * @tparam F  a (potentially) parameterized ''from'' clause defining the positions of all unbound parameters.
	  * @param settersReversed A list of write forms setting the statement parameters ''in the reverse order''.
	  *                        The 'reverse order' here means that after reversing the lists and applying the forms
	  *                        sequentially, all individual columns will be set according to specification. Note that,
	  *                        due to multi-column forms, applying the forms in the order of this list needs not
	  *                        result in the written columns being written in the exact reverse of the former.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.parameterization]]
	  */
	@SerialVersionUID(ver) //todo: this should be applied to all anonymous classes
	abstract class Parameterization[-Ps, +F <: RowProduct](val settersReversed :List[SQLWriteForm[Ps]])
		extends Serializable
	{ outer =>
		/** Forms setting all bound and unbound parameters off an associated SQL fragment
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] accepting the whole parameter set `Ps`
		  * of the statement (or a particular statement fragment).
		  */
		def setters :Seq[SQLWriteForm[Ps]] = settersReversed.reverse

		/** A write form combining all forms on `setters` list into a single instance. */
		def setter :SQLWriteForm[Ps] = SQLWriteForm.join(setters :_*)

		/** Returns an accessor function returning the value of a [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  * parameter of `F` represented by the synthetic relation expression carried by the parameter pseudo join.
		  *///fixme: should return an Extractor
		@throws[IllegalArgumentException]("if the given relation does not represent an unbound parameter of `F`.")
		def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct](param :JoinedRelation[O, T]) :Ps => S

		/** Replaces all write forms for bound and unbound parameters with the given list. This method is the delegation
		  * target for default implementations of all other methods modifying the form list.
		  * @param settersReversed a list of write forms accepting the complete parameter set `Ps`. The list must be
		  *                        in the ''reversed'' order, with the first form setting the rightmost parameter(s)
		  *                        in the associated SQL.
		  */
		def reset[Xs <: Ps](settersReversed :List[SQLWriteForm[Xs]] = Nil) :Parameterization[Xs, F] =
			new Parameterization[Xs, F](settersReversed) {
				override def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
				                  (param :JoinedRelation[O, T]) :Xs => S =
					outer[T, S, O](param)

				override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) =
					outer.ungroup(grouped)

				override def compose[Ys](f :Ys => Xs) :Parameterization[Ys, F] = outer.compose(f)

				override def name = outer.name
				override def aliasedString(aliases :List[String]) :Option[ChunkedString] = outer.aliasedString(aliases)
			}

		/** Adds a write form for the given bound parameter following the existing parameters in the associated SQL. */
		def :+[X](param :BoundParam[X]) :Parameterization[Ps, F] = this :+ param.writeForm.unmap { _ :Ps => () }

		/** Adds a write form for the given bound parameter preceding the existing parameters in the associated SQL. */
		def +:[X](param :BoundParam[X]) :Parameterization[Ps, F] = param.writeForm.unmap { _ :Ps => () } +: this

		/** Appends the given form for a bound or unbound parameter to this instance. */
		def :+[Xs <: Ps](setter :SQLWriteForm[Xs]) :Parameterization[Xs, F] = reset(setter::settersReversed)

		/** Appends to this instance the given forms for bound and unbound parameters, following the existing parameters
		  * in the associated SQL. The forms should be in the exact same order in which they appear in SQL (not reversed).
		  */
		def :++[Xs <: Ps](setters :Seq[SQLWriteForm[Xs]]) :Parameterization[Xs, F] =
			reset(setters.toList reverse_::: settersReversed)

		/** ''Prepends'' the forms from this instance to the form list of the argument, representing the concatenation
		  * of corresponding SQL fragments. Note that the instance returned is based on (created by) the argument,
		  * not this instance - this method is exactly equivalent to `this ++: succeeding`.
		  */
		def :++[Xs <: Ps, E >: F <: RowProduct](succeeding :Parameterization[Xs, E]) :Parameterization[Xs, E] =
			succeeding.reset(succeeding.settersReversed ::: settersReversed)

		/** Prepends the given form to this instance, representing an SQL bound or unbound parameter preceding
		  * the SQL fragment associated with this instance. This method is less effective than `:+`.
		  */
		def +:[Xs <: Ps](setter :SQLWriteForm[Xs]) :Parameterization[Xs, F] = reset(settersReversed:::setter::Nil)

		/** Prepends the given forms to the list of forms of this instance. The forms must occur in the exact same
		  * order as the parameters they set in the associated SQL (not reversed).
		  */
		def ++:[Xs <: Ps](setters :Seq[SQLWriteForm[Xs]]) :Parameterization[Xs, F] =
			reset(settersReversed ::: setters.view.reverse.toList)

		/** Prepends all forms from the argument to the form in this instance, representing the concatenation
		  * of associated SQL fragments.
		  */
		def ++:[Xs <: Ps, E >: F <: RowProduct](preceding :Parameterization[Xs, E]) :Parameterization[Xs, E] =
			reset(preceding.settersReversed ::: settersReversed)



		/** Adapts this instance to a join clause combining clause `F` of this instance with an extra relation `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def join[E <: L NonParam R, L <: RowProduct, R[O] <: MappingAt[O]]
		        (implicit left :F <:< L) :Parameterization[Ps, E] =
			new JoinParameterization[Ps, F, E](this, "join")

		/** Adapts this instance to a join clause combining clause `F` of this instance with an extra relation `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def join[L <: RowProduct, J[+A <: L, B[O] <: R[O]] <: A NonParam B, R[O] <: MappingAt[O]]
		        (join :L J R)(implicit left :F <:< L) :Parameterization[Ps, L J R] =
			this.join[L J R, L, R]


		def subselect[E <: SubselectOf[F]](from :E) :Parameterization[Ps, E] =
			new Parameterization[Ps, E](Nil) {
				override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct](param :JoinedRelation[O, T]) =
					if (param.offset < from.size)
						throw new IllegalArgumentException(
							"Requested a parameter for relation " + param + " inside a subselect from clause " + from + "."
						)
					else
						outer[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset - from.size))

				override def ungroup[C <: FromSome](implicit grouped :E <:< C#GeneralizedAggregate) = ???

				override def fromClause[C <: FromSome](implicit grouped :E <:< AggregateOfGeneralized[C]) =
					from match {
						case aggr :AggregateClause =>
							val inner = aggr.fromClause.asInstanceOf[SubselectOf[F]]
							outer.subselect(inner).asInstanceOf[Parameterization[Ps, C]]
						case _ =>
							throw new UnsupportedOperationException("Cannot ungroup a non-aggregate clause " + from + ".")
					}

				override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :E <:< (L Expanded R)) =
					if (expanded(from).left eq expanded(from).outer)
						outer.asInstanceOf[Parameterization[Ps, L]]
					else
						outer.subselect(expanded(from).left.asInstanceOf[SubselectOf[F]]).asInstanceOf[Parameterization[Ps, L]]
			}


		/** Adapts this instance to a ''from'' expanded with a ''group by'' clause into type `F GroupBy R`.
		  * This has the effect of ignoring the relation for the grouping expression (with index `0`) and shifting
		  * to lower indices all relations from the parent (`outer`) ''select'', removing the relations
		  * from the actual ''from'' clause of the subselect of `F` from indexing.
		  * @param groupBy an instance of the associated ''from''/''group by'' clause, needed to determine the number
		  *                of skipped relations.
		  */
		def group[G <: L GroupBy R, L <: FromSome, R[O] <: MappingAt[O]]
		         (groupBy :G)(implicit left :F <:< L) :Parameterization[Ps, G] =
			new JoinParameterization[Ps, F, G](this, "group by") {
				private[this] val expansion = groupBy.left.size - 1

				override def apply[T[A] <: BaseMapping[S, A], S, O >: G <: RowProduct]
				                  (param :JoinedRelation[O, T]) :Ps => S =
					if (param.offset == 0)
						throw new IllegalArgumentException(
							s"Grouping mapping of $groupBy is not (should not be) an unbound parameter: $param."
						)
					else
						original[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset + expansion))

				override def ungroup[E <: FromSome](implicit grouped :G <:< E#GeneralizedAggregate) =
					new Parameterization[Ps, E](Nil) with UngroupedParameterization[Ps, E] {
						override type Params = Ps
						override def params = this
						override def ungroupParams = { ps :Ps => ps }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Ps => S =
							original[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset))
					}

				override def aliasedString(aliases :List[String]) :Option[ChunkedString] =
					original.aliasedString(aliases).map(_ + " group by _")
			}

		/** Adapts this instance to a ''group by'' clause with an additional grouping expression represented by
		  * mapping `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def group[G <: L By R, L <: GroupByClause, R[O] <: MappingAt[O]](implicit left :F <:< L) :Parameterization[Ps, G] =
			new JoinParameterization[Ps, F, G](this, "by") {
				override def ungroup[E <: FromSome](implicit grouped :G <:< E#GeneralizedAggregate) =
					this.left.ungroup[E](grouped.asInstanceOf[F <:< E#GeneralizedAggregate])

				override def aliasedString(aliases :List[String]) :Option[ChunkedString] =
					left.aliasedString(aliases).map(_ + " by _")
			}

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */ //fixme: E can be completely unrelated to F due to upcasting and downcasting
		def param[E <: L ParamClause R, L <: NonEmptyFrom, R[O] <: UnboundParam[X, O], Xs <: Chain with Ps, X]
		         (implicit ev :F <:< L) :Parameterization[Xs ~ X, E] =
			new Parameterization[Xs ~ X, E](settersReversed.map(_.unmap { xs :(Xs ~ X) => xs.init })) {

				override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct]
				                  (param :JoinedRelation[O, T]) :(Xs ~ X) => S =
					if (param.offset == 0)
						(xs :(Xs ~ X)) => xs.last.asInstanceOf[S]
					else {
						val prev :Xs => S = outer[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset - 1))
						(xs :(Xs ~ X)) => prev(xs.init)
					}

				override def ungroup[G <: FromSome](implicit grouped :E <:< G#GeneralizedAggregate) = {
					val ungrouped = outer.ungroup[G](grouped.asInstanceOf[F <:< G#GeneralizedAggregate])
					new Parameterization[ungrouped.Params, G](Nil) with UngroupedParameterization[Xs ~ X, G] {
						override type Params = ungrouped.Params
						override def params = this
						override val ungroupParams = { xs :(Xs ~ X) => ungrouped.ungroupParams(xs.init) }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: G <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Params => S =
							ungrouped.params.apply[T, S, G](RelationSQL[G, T, S, G](param.relation, param.offset))

						override def left[C <: RowProduct, M[O] <: MappingAt[O]](implicit expanded :G <:< (C Expanded M)) =
							??? //outerb
//							outer.asInstanceOf[Parameterization[Ps, C]]
					}
				}

				override def name = outer.name + " param _"

				override def aliasedString(aliases :List[String]) :Option[ChunkedString] =
					outer.aliasedString(aliases).map { _ + " param _" }
			}

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */
		def param[L <: NonEmptyFrom, J[+A <: L, B[O] <: R[O]] <: A ParamClause B,
		          R[O] <: UnboundParam[X, O], Xs <: Chain with Ps, X]
		         (join :L J R)(implicit ev :F <:< L) :Parameterization[Xs ~ X, L J R] =
			param[L J R, L, R, Xs, X]

		/** Lifts this instance to one representing an aggregated select based on `F`, using the special type
		  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[F]`. This removes all relations in the deepest
		  * select of `F` (since last `Subselect` 'join' or `From`/`Dual`) from indexing, shifting down the indices of
		  * the relations from the outer clauses.
		  */
		def aggregate[E <: FromSome](from :Aggregated[E])(implicit ev :F <:< E) :Parameterization[Ps, Aggregated[E]] =
			new Parameterization[Ps, Aggregated[E]](settersReversed) {
				private[this] val grouped = from.clause.size - from.outer.size

				override def apply[T[A] <: BaseMapping[S, A], S, O >: Aggregated[E] <: RowProduct]
				                  (param :JoinedRelation[O, T]) :Ps => S =
					outer[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset + grouped))

				override def ungroup[U <: FromSome](implicit grouped :Aggregated[E] <:< U#GeneralizedAggregate) =
					new Parameterization[Ps, U](Nil) with UngroupedParameterization[Ps, U] {
						override type Params = Ps
						override def params = this
						override val ungroupParams = { ps :Ps => ps }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: U <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Ps => S =
							outer[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset))

						override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :U <:< Expanded[L, R]) =
							??? //outer.left
					}

				override def left[L <: RowProduct, R[O] <: MappingAt[O]]
				                 (implicit expanded :Aggregated[E] <:< (L Expanded R)) =
					throw new UnsupportedOperationException("Cannot get the left side of an Aggregated clause.")

				override def name = "Aggregate(" + outer.name + ")"
				override def aliasedString(aliases :List[String]) :Option[ChunkedString] =
					outer.aliasedString(aliases).map(as => "Aggregate(" +: (as + ")"))
			}

		/** Adapts this instance to one based on the decorated clause `E`, without any actual changes in behaviour. */
		def decorate[G >: F <: RowProduct, E <: DecoratedFrom[G]] :Parameterization[Ps, E] =
			this.asInstanceOf[Parameterization[Ps, E]]


		def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) :Parameterization[Ps, L]

		/** Adapts this instance to the 'ungrouped' ''from'' clause under this ''group by'' clause.
		  * This is the reverse operation to
		  * [[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.group[L<:FromSome,R[O]<:MappingAt[O]]* group]]]:
		  * it replaces in the index all pseudo relations for grouping expressions at the lowest positions
		  * with the relations following the last `Subselect`/`Dual` in the ungrouped clause `F`.
		  * Note that returned parameterization will contain ''no setter forms from this instance'' and should be
		  * used only as a factory for unbound parameter accessors.
		  * This method is possible to call only if `F` is
		  * an [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  * and will throw an `UnsupportedOperationException` otherwise.
		  */
		def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) :UngroupedParameterization[Ps, E]
		//fixme: replace ungroup with this method
		def fromClause[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) :Parameterization[Ps, E] = ???

		/** Presents itself as a parameterization of the [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type
		  * of the current domain type `F`. This is an identity cast derived from the fact that
		  * `RowProduct.`[[net.noresttherein.oldsql.sql.RowProduct.self self]] is always an identity function.
		  */
		def self[S <: RowProduct](implicit self :F <:< RowProduct { type Self <: S }) :Parameterization[Ps, S] =
			this.asInstanceOf[Parameterization[Ps, S]]

		/** Presents itself as a parameterization of the [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type
		  * of the current domain type `F`. This is an identity cast derived from the fact that
		  * `RowProduct.`[[net.noresttherein.oldsql.sql.RowProduct.self self]] is always an identity function.
		  */
//		def self[U >: F <: RowProduct, S <: RowProduct](from :U)(implicit self :U <:< RowProduct { type Self = S })
//				:Parameterization[Ps, S] =
//			this.asInstanceOf[Parameterization[Ps, S]]

		//todo: we need compose[Xs](f :Xs =?> Ps)
		/** Adapts this instance to one using a different parameter set type `Xs`, from which the parameters `Ps`
		  * of this instance can be derived. Note that function `f` will be called by every accessor returned
		  * by [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]], resulting in
		  * potentially costly re-creation of the whole parameter set `Ps` with setting of every parameter.
		  */
		def compose[Xs](f :Xs => Ps) :Parameterization[Xs, F] = new MappedParameterization[Xs, Ps, F](this, f)


		private[sql] def name :String = "-"

		/** Merges the information about the underlying ''from''/''group by'' clause, if available, with the list
		  * of aliases given to all non-grouping relations, with the first alias on the list corresponding
		  * to the last ''non-grouping'' relation in `F`; this includes tables under a ''group by'' clause,
		  * which are inaccessible to expressions based on `F`.
		  *
		  * Implementing this method is optional and it is used only to provide nicer `toString` implementation
		  * in [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]], by combining this parameterization
		  * with an associated [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]].
		  */
		def aliasedString(aliases :List[String]) :Option[ChunkedString] = None

		override def toString :String = setters.mkString("Parameterization(" + name + " :", "|", ")")
	}



	object Parameterization {

		/** A parameterization of an SQL or DML fragment with no bound or unbound parameters, and hence no setter forms.
		  * It is parameterless in the context of ''unbound'' parameters - all calls to its
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] method throw
		  * an [[IllegalArgumentException]]. Forms for bound parameters can be added to this instance
		  * by the standard API without changing this paramless nature, and, due to contravariance in the type parameter
		  * of the parameter set, 'joining' it with unbound parameters via method
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.param param]], expands it with
		  * an unbound parameter.
		  */
		def paramless[F <: ParamlessFrom] :Parameterization[Any, F] = instance.asInstanceOf[Parameterization[Any, F]]

		/** A parameterization for the given ''from'' clause, providing accessors for the values
		  * of all unbound parameters. The setter form list of the returned instance is ''empty'' - it doesn't include
		  * entries for bound and unbound parameters appearing in the associated ''where'' clause (and join conditions).
		  */
		def apply[Ps <: Chain, F <: ParameterizedFrom[Ps]](from :F) :Parameterization[Ps, F] =
			new ParamChain(from, Nil)

		private[this] val instance = new Paramless[Any, ParamlessFrom](Nil)



		/** A wrapper for a [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
		  * with an abstract parameter set type `Params`, together with a function for obtaining said parameter set
		  * from the values of type parameter `Xs` (of some other parameterization instance).
		  */
		trait UngroupedParameterization[-Xs, +F <: FromSome] extends Serializable { this :Parameterization[Nothing, F] =>
			/** The type of the parameter set of `params` :`Parameterization[Params, F]` property. */
			type Params
			/** Accessor obtaining the parameter set `Params` of the `params` parameterization property from a
			  * larger parameter set `Xs` of some source `Parameterization[Xs, F]`. In the main use case,
			  * where the parameter set type is the `Params` chain defined by relations `F`, and the use of this trait
			  * in adapting a parameterization of a ''group by'' clause to that of the `FromClause` underneath it,
			  * this boils down to dropping the number of first elements of the chain corresponding to the number
			  * of `GroupParam` pseudo joins in the ''group by'' clause.
			  */
			def ungroupParams :Xs => Params

			/** A parameterization adapter to `Parameterization[Xs, F]` using an altered parameter set type `Params`. */
			def params :Parameterization[Params, F]

			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) =
				throw new UnsupportedOperationException(
					"Not grouped FROM clause cannot be ungrouped. This method should not have been possible to call."
				)
		}



		@SerialVersionUID(ver)
		private class Paramless[-Ps, +F <: RowProduct](setters :List[SQLWriteForm[Ps]])
			extends Parameterization[Ps, F](setters)
		{
			override def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
			                  (param :JoinedRelation[O, T]) :Ps => S =
				throw new IllegalArgumentException(
					"Parameter requested for relation " + param + " of a supposedly parameterless clause."
				)

			override def reset[Xs <: Ps](settersReversed :List[SQLWriteForm[Xs]]) :Parameterization[Xs, F] =
				new Paramless(settersReversed)

			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate)
					:UngroupedParameterization[Ps, E] =
				new Paramless[Ps, E](Nil) with UngroupedParameterization[Ps, E] {
					override type Params = Ps
					override def params = this
					override def ungroupParams = { ps :Ps => ps }
				}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				this.asInstanceOf[Parameterization[Ps, L]]

			override def name = "_"

			override def aliasedString(aliases :List[String]) :Option[ChunkedString] =
				if (aliases.isEmpty) Some(ChunkedString("Dual"))
				else None
		}



		/** A `Parameterization` implementation based on a full `RowProduct` instance `from`, implementing all
		  * accessor methods globally rather than in an incremental fashion.
		  */
		@SerialVersionUID(ver)
		private class ParamChain[-Ps <: Chain, +F <: RowProduct](protected val from :F, setters :List[SQLWriteForm[Ps]])
			extends Parameterization[Ps, F](setters)
		{ self =>
			private[this] val paramIndex = from.fullTableStack.scanLeft(-1) {
				case (paramCount, relation) if relation.relation.row.isInstanceOf[ParamAt[_]] => paramCount + 1
				case (paramCount, _) => paramCount
			}.tail.toArray

			override def apply[T[A] <: RefinedMapping[P, A], P, O >: F <: RowProduct]
			                  (relation :JoinedRelation[O, T]) :Ps => P =
			{
				val offset = relation.offset
				val param = paramIndex(offset)
				if (offset > 0 && paramIndex(offset - 1) == param)
					throw new IllegalArgumentException(
						s"Relation's $relation offset #$param doesn't match an unbound parameter in $from."
					)
				else if (!relation.relation.row.isInstanceOf[ParamAt[_]])
					throw new IllegalArgumentException(
						s"Relation $relation is not an unbound parameter of $from."
					)
				val get = ChainGet(param)
				params => params(param)(get).asInstanceOf[P]
			}

			override def reset[Xs <: Ps](settersReversed :List[SQLWriteForm[Xs]]) :Parameterization[Xs, F] =
				new ParamChain(from, settersReversed)

			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) :UngroupedParameterization[Ps, E] =
				new ParamChain[Chain, E](grouped(from).fromClause.asInstanceOf[E], Nil)
					with UngroupedParameterization[Ps, E]
				{
					override type Params = Chain
					override def params = this
					val groupParams = self.from.paramCount - from.paramCount

					override val ungroupParams =
						if (groupParams == 0)
							{ ps :Chain => ps }
						else
							{ ps :Chain =>
								var res = ps; var i = groupParams
								while(i > 0) { i -= 1; res = ps.asInstanceOf[Chain~Any].init }
								res
							}
				}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				new ParamChain[Ps, L](expanded(from).left, Nil)

			override def name = from.toString

			override def aliasedString(aliases :List[String]) :Option[ChunkedString] = None
		}



		@SerialVersionUID(ver)
		private class JoinParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                  (protected val original :Parameterization[Ps, L], val join :String)
			extends Parameterization[Ps, F](original.settersReversed)
		{
			override def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
			                  (param :JoinedRelation[O, T]) :Ps => S =
				if (param.offset == 0)
					throw new IllegalArgumentException(
						"The last relation of the associated FROM clause is not an unbound parameter: " + param + "."
					)
				else
					original[T, S, L](RelationSQL[L, T, S, L](param.relation, param.offset - 1))


			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate)
					:UngroupedParameterization[Ps, E] =
				throw new UnsupportedOperationException(
					"Not grouped FROM clause cannot be ungrouped. This method should not have been possible to call."
				)

			override def left[C <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (C Expanded R)) =
				original.asInstanceOf[Parameterization[Ps, C]]

			override def name = original.name + " " + join + " _"

			override def aliasedString(aliases :List[String]) :Option[ChunkedString] = aliases match {
				case hd::tail => original.aliasedString(tail).map(_ + (" " + join + " ") + hd)
				case _ => None
			}
		}



		@SerialVersionUID(ver)
		private class MappedParameterization[-Xs, Ys, +F <: RowProduct](original :Parameterization[Ys, F], f :Xs => Ys)
			extends Parameterization[Xs, F](original.settersReversed.map(_.unmap(f)))
		{
			override def apply[T[A] <: BaseMapping[S, A], S, O >: F <:RowProduct]
			                  (param :JoinedRelation[O, T]) :Xs => S =
				f andThen original[T, S, O](param)

			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) :UngroupedParameterization[Xs, E] = {
				val ungrouped = original.ungroup[E]
				new Parameterization[ungrouped.Params, E](Nil) with UngroupedParameterization[Xs, E] {
					override type Params = ungrouped.Params
					override def params = this
					override val ungroupParams = f andThen ungrouped.ungroupParams

					override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct]
					                  (param :JoinedRelation[O, T]) :ungrouped.Params => S =
						ungrouped.params[T, S, O](param)

					override def compose[Zs](f :Zs => ungrouped.Params) :Parameterization[Zs, E] =
						ungrouped.params.compose(f)
				}
			}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				new MappedParameterization[Xs, Ys, L](original.left, f)

			override def compose[Zs](g :Zs => Xs) :Parameterization[Zs, F] = original.compose(g andThen f)


			override def name = original.name

			override def aliasedString(aliases :List[String]) :Option[ChunkedString] = original.aliasedString(aliases)

			override def toString = "MappedParameterization(" + name + ")"
		}
	}


}

