package net.noresttherein.oldsql.sql.mechanics

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{~, ChainGet}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.schema.SQLWriteForm
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{Aggregated, By, DecoratedFrom, FromSome, GroupBy, GroupByClause, JoinedRelation, NonParam, RowProduct, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{NonEmptyFrom, ParameterizedFrom, ParamlessFrom}
import net.noresttherein.oldsql.sql.UnboundParam.{FromParam, ParamAt}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.{JoinParameterization, MappedParameterization, UngroupedParameterization}






/** A temporary artefact of translating an SQL AST [[net.noresttherein.oldsql.sql.SQLExpression expression]]
  * to an [[net.noresttherein.oldsql.sql.SQLStatement SQLStatement]]. It represents a fragment of an SQL or DML
  * expression - not necessarily corresponding to a syntactically complete subexpression - together with
  * context information necessary for ensuring its validity, in particular used table aliases
  * and column names, and a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting the statement's parameters.
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext]]
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization]]
  * @author Marcin Mościcki
  */
class SpelledSQL[-P, +F <: RowProduct]
                (val sql :ChunkedString, val context :SQLContext, val params :Parameterization[P, F])
	extends Serializable
{
	def +(char :Char) :SpelledSQL[P, F] = new SpelledSQL(sql + char, context, params)
	def +(string :String) :SpelledSQL[P, F] = new SpelledSQL(sql + string, context, params)
	def +(string :ChunkedString) :SpelledSQL[P, F] = new SpelledSQL(sql + string, context, params)
	def +[X, E <: RowProduct](string :(SQLContext, Parameterization[P, F]) => SpelledSQL[X, E]) :SpelledSQL[X, E] = {
		val suffix = string(context, params)
		new SpelledSQL(sql + suffix.sql, suffix.context, suffix.params)
	}

	def +(param :SQLParameter[_]) :SpelledSQL[P, F] = param.writeForm.unmap { _ :Any => () } match {
		case form if form.writtenColumns == 1 =>
			new SpelledSQL(this.sql + "?", context, params :+ form)
		case form =>
			val tuple = Iterator.fill(form.writtenColumns)("?").mkString("(", ", ", ")")
			new SpelledSQL(this.sql + tuple, context, params :+ form)
	}

	def +:(char :Char) :SpelledSQL[P, F] = new SpelledSQL(char.toString +: sql, context, params)
	def +:(string :String) :SpelledSQL[P, F] = new SpelledSQL(string +: sql, context, params)
	def +:(string :ChunkedString) :SpelledSQL[P, F] = new SpelledSQL(string + sql, context, params)

	def +:(param :SQLParameter[_]) :SpelledSQL[P, F] = param.writeForm.unmap { _ :Any => () } match {
		case form if form.writtenColumns == 1 =>
			new SpelledSQL(this.sql + "?", context, form +: params)
		case form =>
			val tuple = Iterator.fill(form.writtenColumns)("?").mkString("(", ", ", ")")
			new SpelledSQL(this.sql + tuple, context, form +: params)
	}

	/** Updates the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] of this SQL fragment
	  * by applying the given function to include an additional condition for the ''where'' clause. The SQL text of
	  * the result is taken to be a boolean condition which should be combined in a logical conjunction with the
	  * rest of the ''where'' clause, and its context is the update of this instance's context with said condition
	  * already included. The process is similar to monadic ''flat map'', with the context being passed through
	  * all chained operations.
	  */
	def &&[X <: P, E <: RowProduct]
	      (condition :(SQLContext, Parameterization[P, F]) => SpelledSQL[X, E]) :SpelledSQL[X, E] =
	{
		val suffix = condition(context, params)
		new SpelledSQL(sql, context && suffix, suffix.params)
	}

	def newAlias(name :String) :String = context.newAlias(name)
	def newAliases(names :String*) :Seq[String] = names.map(newAlias)

	def column(name :String) :SpelledSQL[P, F] = new SpelledSQL(sql, context.column(name), params)
	def columns(names :String*) :SpelledSQL[P, F] = new SpelledSQL(sql, (context /: names)(_ column _), params)


	override def toString :String = {
		val namespace = context.namespace.mkString("; columns: {", ", ", "})")
		val from = params.aliasedString(context.tablesReversed.view.filter(context.isLegal).toList) getOrElse {
			context.tablesReversed.reverseIterator.mkString(params.name + " as ", ", ", "")
		}
		val where =
			if (context.where.isEmpty) from + namespace
			else context.where.mkString(from + " WHERE ", " && ", namespace)

		"SQL('" + sql + "' " + params.setter + "; " + where
	}
}






object SpelledSQL {
	private[this] val empty =
		new SpelledSQL[Any, Nothing](ChunkedString.empty, SQLContext(), Parameterization.paramless)

	def apply() :SpelledSQL[Any, Nothing] = empty
//fixme: what type params these should be?
	def apply(sql :String) :SpelledSQL[Any, Nothing] =
		new SpelledSQL(ChunkedString(sql), SQLContext(), Parameterization.paramless)

	def apply(sql :ChunkedString) :SpelledSQL[Any, Nothing] =
		new SpelledSQL(sql, SQLContext(), Parameterization.paramless)

	def apply(context :SQLContext) :SpelledSQL[Any, Nothing] =
		new SpelledSQL(ChunkedString.empty, context, Parameterization.paramless)

	def apply[P, F <: RowProduct](context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
		new SpelledSQL(ChunkedString.empty, context, params)

	def apply[P, F <: RowProduct](sql :String, context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
		new SpelledSQL(ChunkedString(sql), context, params)

	def apply[P, F <: RowProduct](sql :ChunkedString, context :SQLContext, params :Parameterization[P, F])
			:SpelledSQL[P, F] =
		new SpelledSQL(sql, context, params)


	@inline def unapply[P, F <: RowProduct](sql :SpelledSQL[P, F]) :(ChunkedString, SQLContext, Parameterization[P, F]) =
		(sql.sql, sql.context, sql.params)



	/** Non-local context related to an SQL fragment
	  * (of some [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]).
	  * It contains primarily the aliases of all the tables in the ''from'' clause of the ''select'' using the fragment,
	  * as well as those of any outer ''selects'' in which the current ''select'' is embedded.
	  * @param tablesReversed an indexed table containing aliases of all relations, with indexing congruent with that
	  *                       of [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]]
	  *                       expressions used in the SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
	  *                       being translated. This means that the first alias in the sequence is that for the ''last''
	  *                       relation in the ''from'' clause, and so on. The exception are expressions based on
	  *                       ''group by'' clauses, where the grouping relations receive no aliases and indexing stays
	  *                       the same as for the grouped ''from'' clause (with the aggregated relations being available).
	  *                       This list includes (unused) aliases for all
	  *                       occurrences of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] type in the ''from''
	  *                       clause, as well as placeholder entries for grouping relations ''from outer selects only''
	  *                       in order to retain consistency with numbering used by `RowProduct` for its joined relations.
	  * @param whereReversed  SQL fragments coming from individual join conditions (and other `RowProduct` subcomponents),
	  *                       which were not included in a ''join ... on'' clause and should become part of a logical
	  *                       conjunction forming the ''where'' clause. This in particular includes conditions
	  *                       present on `From` and `JoinParam`. The conditions are in the reverse order of their
	  *                       appearance in the ''from'' clause.
	  * @param namespace      all visible column aliases.
	  * @param outerOffset    the size of the ''from'' clause of the most deeply nested select of the associated SQL
	  *                       expression/the offset in the alias table of the first table from the outer select,
	  *                       or `-1` for top clauses (not subselects).
	  * @param groupings      combined number of [[net.noresttherein.oldsql.sql.GroupBy GroupBy]],
	  *                       [[net.noresttherein.oldsql.sql.By By]] and [[net.noresttherein.oldsql.sql.GroupParam]]
	  *                       (including their type aliases) elements of the ''group by'' clause of the SQL fragment.
	  *                       It is the number of grouping relations which are ''not'' included in the alias index.
	  */
	class SQLContext(val tablesReversed :IndexedSeq[String] = IndexedSeq.empty[String],
	                 val namespace :Set[String] = Set(""),
	                 val whereReversed :List[ChunkedString] = Nil,
	                 val outerOffset :Int = -1, val groupings :Int = -1)
		extends Serializable
	{ //todo: higher-level, use-case centric 'copy' mutators for all properties in order to allow some room, for implementation changes.
		/** A copy constructor used by all methods adding to the context so subclasses need override only this method. */
		def copy(tablesReversed :IndexedSeq[String] = tablesReversed, namespace :Set[String] = namespace,
		         whereReversed :List[ChunkedString] = whereReversed,
		         outerOffset :Int = outerOffset, groupings :Int = groupings)
				:SQLContext =
			new SQLContext(tablesReversed, namespace)

		private[SpelledSQL] def isLegal(alias :String) :Boolean =
			alias != null && alias.length > 0 && !alias.startsWith("?") && !alias.startsWith("<")

		/** A virtual constructor of a default context for an empty SQL fragment used instead of the global
		  * factory to propagate custom implementations for the use by the whole 'spelling' process.
		  */
		def fresh = new SQLContext()

		/** SQL fragments coming from individual join conditions (and other `RowProduct` subcomponents),
		  * which were not included in a ''join ... on'' clause and should become part of a logical conjunction
		  * forming the ''where'' clause. This in particular includes conditions present on `From` and `JoinParam`.
		  */
		def where :List[ChunkedString] = whereReversed.reverse

		/**	Aliases of all tables in the ''from'' clause of the SQL fragment, not including aliases of tables from
		  * outer ''selects'' or synthetic relations corresponding to unbound parameters or grouping expressions,
		  * in the order in which they appear in the linearization of the ''from'' clause.
		  */
		def tables :Iterator[String] =
			if (outerOffset < 0) tablesReversed.reverseIterator.filter(isLegal)
			else tablesReversed.view.take(outerOffset).reverseIterator.filter(isLegal)

		/** Aliases of all tables in the ''from'' clause of the translated SQL fragment, including aliases of tables
		  * from the outer ''selects'', but not including any synthetic relations (for unbound parameters and grouping
		  * expressions).
		  */
		def tableAliases :Set[String] = tablesReversed.view.filter(isLegal).toSet

		/** Checks if the context contains the given table alias. This takes into account only usable aliases:
		  * Those of the aggregated tables from outer ''selects'' are not included.
		  */
		def contains(table :String) :Boolean = tablesReversed.contains(table)

		/** The alias for the `idx-th` last table (that is, counting from the rightmost table in the ''from'' clause).
		  * This is the alias corresponding to a
		  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]] with that offset.
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
		def join(table :String) :SQLContext =
			if (groupings >= 0)
				throw new IllegalStateException(s"Cannot join '$table' with an aggregate clause $this.")
			else if (tablesReversed.contains(table))
				throw new IllegalArgumentException(s"$this already contains table named '$table' in scope.")
			else
				copy(tablesReversed = table +: tablesReversed, outerOffset = outerOffset + 1)

		/** Adds a new table alias as the first (index `0`) element of the ''from'' clause of a nested ''select''.
		  * @return `this.subselect.join(table)`.
		  */
		@throws[IllegalArgumentException]("if the alias is already in use.")
		def subselect(table :String) :SQLContext =
			if (tablesReversed.contains(table))
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
		def subselect :SQLContext =
			if (groupings >= 0) {
				val aliases = Iterator.tabulate(groupings)("<grouping" + _ + ">") ++:
					(if (outerOffset > 0) tablesReversed.drop(outerOffset) else ArraySeq.empty[String])
				copy(tablesReversed = aliases, groupings = -1, outerOffset = 0)
			} else
				copy(groupings = -1, outerOffset = 0)

		/** Pushes a placeholder alias for a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instance
		  * at the index `0` of the table aliases in scope, shifting back existing aliases.
		  * Use `grouped` method instead to represent a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]].
		  */
		def param(alias :String) :SQLContext = join("?" + alias)

		/** Adds another ''group by'' clause expression corresponding to either
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], [[net.noresttherein.oldsql.sql.By By]] or
		  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] element.
		  */
		def grouped :SQLContext = copy(outerOffset = outerOffset + 1, groupings = groupings + 1)

		/** Marks the ''select'' as ''aggregated'', i.e. containing aggregate functions in its ''select'' clause.
		  * This corresponds to an artificial [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] wrapper of
		  * a [[net.noresttherein.oldsql.sql.FromClause FromClause]]. The effect is the same that a ''group by''
		  * clause with zero grouping expressions would have.
		  */
		def aggregate :SQLContext =
			if (groupings >= 0)
				throw new IllegalStateException("Cannot aggregate an already aggregated clause: "+ this +".")
			else copy(groupings = 0)

		/** A unique column alias using the given name as the starting prefix. */
		def newAlias(name :String) :String = {
			var res = name
			var i = 0
			while (namespace.contains(res)) {
				i += 1
				res = name + i
			}
			res
		}

		/** Adds the given column name/alias to the namespace. */
		def column(name :String) :SQLContext =
			if (namespace contains name)
				throw new IllegalArgumentException(s"$this already contains column '$name' in scope.")
			else
				copy(namespace = namespace + name)

		/** Adds the SQL for a boolean expression which should be included in the ''where'' clause of a larger SQL
		  * fragment.
		  */
		def &&(condition :SpelledSQL[_, _]) :SQLContext = copy(whereReversed = condition.sql::this.whereReversed)


		override def toString :String = {
			val suffix = namespace.mkString("; columns: {", ", ", "})")
			val where = whereReversed.reverseIterator.mkString(" WHERE ", " && ", suffix)
			tablesReversed.reverseIterator.mkString("SQLContext(FROM ", ", ", where)
		}
	}



	object SQLContext {
		private[this] val empty = new SQLContext(ArraySeq.empty[String])

		/** An empty context. */
		def apply() :SQLContext = empty

		def apply(aliases :IndexedSeq[String], fromClauseSize :Int) :SQLContext =
			new SQLContext(aliases.reverse, outerOffset = fromClauseSize)
	}




	/** Parameter context of an SQL `PreparedStatement` as a function of a single parameter `Ps` representing
	  * all [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters of ''from'' clause `F`.
	  * This class serves two purposes:
	  *   1. it contains a list of [[net.noresttherein.oldsql.schema.SQLWriteForm forms]] setting the values
	  *      of all parameters for a particular statement,
	  *   1. and is a factory of accessors for `Ps` which return values of individual
	  *      [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters based on
	  *      [[net.noresttherein.oldsql.sql.JoinedRelation JoinedRelation]] instances from `F` for these parameters.
	  * This class is used in conjunction with [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]],
	  * carrying information about that statement fragment. As the latter is expanded, so too is this object:
	  * primarily by adding additional forms with `:+`, `+:` or swapping the whole list with
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.reset reset]] method. These forms
	  * are either constant forms (with a `Unit` argument) of bound
	  * [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter SQLParameter]]  expressions, or the mapping forms
	  * taken from [[net.noresttherein.oldsql.sql.UnboundParam.FromParam FromParam]] mappings of unbound parameters.
	  * Adapting of the former to the required argument type `Ps` can happen by a trivial constant function
	  * returning `()`, while the composition of the latter consists of the accessor function `Ps => P` returned by
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]]
	  * for a [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation JoinedRelation]]`[_ >: F, P]`, being the
	  * synthetic relation taken from an `UnboundParam` pseudo join, and - if the value required is not that of the
	  * parameter `P` itself, but a derived one `X` represented by a component of `FromParam` - an optional selector
	  * `P => X` for the particular parameter component taken from `FromParam`, as with all mappings.
	  * Additionally, the polymorphic function returning the parameter accessors is updated recursively by following
	  * the structure of the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] of the statement, in order
	  * to reflect updated indexing and, in case of parameter relations, expanding also the parameter type `Ps`.
	  * This happens by factory methods defined by this class and corresponding to particular `Adjoin` subtypes.
	  *
	  * The values of this class can be created by factory methods in the companion object, either based on
	  * an existing `RowProduct` instance of `F`, or by expanding
	  * a [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.paramless paramless]] instance
	  * in the manner described above.
	  *
	  * @tparam Ps a composite type combining the values of all individual unbound parameters in `F`, typically
	  *            `F#Params` [[net.noresttherein.oldsql.collection.Chain Chain]].
	  * @tparam F  a (potentially) parameterized ''from'' clause defining the positions of all unbound parameters.
	  * @param settersReversed A list of write forms setting the statement parameters ''in the reverse order''.
	  *                        The 'reverse order' here means that after reversing the lists and applying the forms
	  *                        sequentially, all individual columns will be set according to specification. Note that,
	  *                        due to multi-column forms, applying the forms in the order of this list needs not
	  *                        result in the written columns being written in the exact reverse of the former.
	  */ //todo: this would be probably better with -F
	abstract class Parameterization[-Ps, +F <: RowProduct](val settersReversed :List[SQLWriteForm[Ps]])
		extends Serializable
	{ left =>
		/** Forms setting all bound and unbound parameters off an associated SQL fragment
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] accepting the whole parameter set `Ps`
		  * of the statement (or a particular statement fragment).
		  */
		def setters :Seq[SQLWriteForm[Ps]] = settersReversed.reverse

		/** A write form combining all forms on `setters` list into a single instance. */
		def setter :SQLWriteForm[Ps] = SQLWriteForm.combine(setters :_*)

		/** Returns an accessor function returning the value of an [[net.noresttherein.oldsql.sql.UnboundParam unbound]]
		  * parameter of `F` represented by the synthetic relation expression carried by the parameter pseudo join.
		  */
		@throws[IllegalArgumentException]("if the given relation does not represent an unbound parameter of `F`.")
		def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		         (param :JoinedRelation[O, T]) :Ps => S

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
					left[T, S, O](param)

				override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate) =
					left.ungroup(grouped)

				override def compose[Ys](f :Ys => Xs) :Parameterization[Ys, F] = left.compose(f)

				override def name = left.name
				override def aliasedString(aliases :List[String]) :Option[String] = left.aliasedString(aliases)
			}

		/** Adds a write form for the given bound parameter following the existing parameters in the associated SQL. */
		def :+[X](param :SQLParameter[X]) :Parameterization[Ps, F] = this :+ param.writeForm.unmap { _ :Ps => () }

		/** Adds a write form for the given bound parameter preceding the existing parameters in the associated SQL. */
		def +:[X](param :SQLParameter[X]) :Parameterization[Ps, F] = param.writeForm.unmap { _ :Ps => () } +: this

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
						left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset + expansion))

				override def ungroup[E <: FromSome](implicit grouped :G <:< E#GeneralizedAggregate) =
					new Parameterization[Ps, E](Nil) with UngroupedParameterization[Ps, E] {
						override type Params = Ps
						override def params = this
						override def ungroupParams = { ps :Ps => ps }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Ps => S =
							left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset))
					}

				override def aliasedString(aliases :List[String]) :Option[String] =
					left.aliasedString(aliases).map(_ + " group by _")
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

				override def aliasedString(aliases :List[String]) :Option[String] =
					left.aliasedString(aliases).map(_ + " by _")
			}

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */
		def param[E <: L UnboundParam R, L <: NonEmptyFrom, R[O] <: FromParam[X, O], Xs <: Chain with Ps, X]
		         (implicit ev :F <:< L) :Parameterization[Xs ~ X, E] =
			new Parameterization[Xs ~ X, E](settersReversed.map(_.unmap { xs :(Xs ~ X) => xs.init })) {

				override def apply[T[A] <: BaseMapping[S, A], S, O >: E <: RowProduct]
				                  (param :JoinedRelation[O, T]) :(Xs ~ X) => S =
					if (param.offset == 0)
						(xs :(Xs ~ X)) => xs.last.asInstanceOf[S]
					else {
						val prev :Xs => S = left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset - 1))
						(xs :(Xs ~ X)) => prev(xs.init)
					}

				override def ungroup[G <: FromSome](implicit grouped :E <:< G#GeneralizedAggregate) =
					new Parameterization[Xs, G](Nil) with UngroupedParameterization[Xs ~ X, G] {
						override type Params = Xs
						override def params = this
						override val ungroupParams = { xs :(Xs ~ X) => xs.init }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: G <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Xs => S =
							left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset))
					}

				override def name = left.name + " param _"

				override def aliasedString(aliases :List[String]) :Option[String] =
					left.aliasedString(aliases).map { _ + " param _" }
			}

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */
		def param[L <: NonEmptyFrom, J[+A <: L, B[O] <: R[O]] <: A UnboundParam B,
		          R[O] <: FromParam[X, O], Xs <: Chain with Ps, X]
		         (join :L J R)(implicit ev :F <:< L) :Parameterization[Xs ~ X, L J R] =
			param[L J R, L, R, Xs, X]

//		def subselect()

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
					left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset + grouped))

				override def ungroup[U <: FromSome](implicit grouped :Aggregated[E] <:< U#GeneralizedAggregate) =
					new Parameterization[Ps, U](Nil) with UngroupedParameterization[Ps, U] {
						override type Params = Ps
						override def params = this
						override val ungroupParams = { ps :Ps => ps }

						override def apply[T[A] <: BaseMapping[S, A], S, O >: U <: RowProduct]
						                  (param :JoinedRelation[O, T]) :Ps => S =
							left[T, S, F](RelationSQL[F, T, S, F](param.relation, param.offset))
					}

				override def name = "Aggregate(" + left.name + ")"
				override def aliasedString(aliases :List[String]) :Option[String] =
					left.aliasedString(aliases).map("Aggregate(" + _ + ")")
			}

		/** Adapts this instance to one based on the decorated clause `E`, without any actual changes in behaviour. */
		def decorate[G >: F <: RowProduct, E <: DecoratedFrom[G]] :Parameterization[Ps, E] =
			this.asInstanceOf[Parameterization[Ps, E]]


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
		  * which are unavailable to access.
		  *
		  * Implementing this method is optional and it is used only to provide nicer `toString` implementation
		  * in [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]], by combining this parameterization
		  * with an asssociated [[net.noresttherein.oldsql.sql.mechanics.SpelledSQl.SQLContext SQLContext]].
		  */
		def aliasedString(aliases :List[String]) :Option[String] = None

		override def toString :String = setters.mkString("Parameterization(" + name + " :", "|", ")")
	}



	object Parameterization {
		//todo: this bound is useless in light of covariance of Parameterization in F
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

		/** A parameterization for the given ''from'' clause, providing accessors for the values of all unbound parameters.
		  * The setter form list of the returned instance is ''empty'' - it doesn't include entries for
		  * bound and unbound parameters appearing in the associated ''where'' clause (and join conditions).
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

			override def name = "_"

			override def aliasedString(aliases :List[String]) :Option[String] =
				if (aliases.isEmpty) Some("Dual")
				else None
		}



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


			override def name = from.toString

			override def aliasedString(aliases :List[String]) :Option[String] = None
		}



		private class JoinParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                  (protected val left :Parameterization[Ps, L], val join :String)
			extends Parameterization[Ps, F](left.settersReversed)
		{
			override def apply[T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
			                  (param :JoinedRelation[O, T]) :Ps => S =
				if (param.offset == 0)
					throw new IllegalArgumentException(
						"The last relation of the associated FROM clause is not an unbound parameter: " + param + "."
					)
				else
					left[T, S, L](RelationSQL[L, T, S, L](param.relation, param.offset - 1))


			override def ungroup[E <: FromSome](implicit grouped :F <:< E#GeneralizedAggregate)
					:UngroupedParameterization[Ps, E] =
				throw new UnsupportedOperationException(
					"Not grouped FROM clause cannot be ungrouped. This method should not have been possible to call."
				)

			override def name = left.name + " " + join + " _"

			override def aliasedString(aliases :List[String]) :Option[String] = aliases match {
				case hd::tail => left.aliasedString(tail).map(_ + " " + join + " " + hd)
				case _ => None
			}
		}



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

			override def compose[Zs](g :Zs => Xs) :Parameterization[Zs, F] = original.compose(g andThen f)


			override def name = original.name

			override def aliasedString(aliases :List[String]) :Option[String] = original.aliasedString(aliases)

			override def toString = "MappedParameterization(" + name + ")"
		}
	}

}

