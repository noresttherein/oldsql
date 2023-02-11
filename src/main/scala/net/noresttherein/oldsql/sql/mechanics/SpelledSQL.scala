package net.noresttherein.oldsql.sql.mechanics

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{Chain, Opt, PassedArray, Unique}
import net.noresttherein.oldsql.collection.Chain.{~, ChainGet}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{ExecutionTimeSQLException, SpellingWarning}
import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Optional, Requisite}
import net.noresttherein.oldsql.schema.{Relation, RelVar, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.Table.Aliased
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.support.AliasedColumn
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, By, Expanded, FromSome, GroupBy, GroupByClause, Join, NonParam, ParamClause, RowProduct, Subselect}
import net.noresttherein.oldsql.sql.DecoratedRow.ExpandingDecorator
import net.noresttherein.oldsql.sql.ParamClause.UnboundParam
import net.noresttherein.oldsql.sql.RowProduct.{AggregateOfGeneralized, As, NonEmptyRow, ParamlessRow, ParamsRow, RowComposition, SubselectOf}
import net.noresttherein.oldsql.sql.ast.{BoundParam, JoinedParam, JoinedRelation, JoinedTable}
import net.noresttherein.oldsql.sql.ast.JoinedRelation.__
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{ver, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.{AdaptedParameterization, AggregatedParameterization, ByParameterization, ComposedParameterization, GroupByParameterization, JoinParameterization, ParamParameterization, SubselectParameterization}






/** A fragment of an SQL or DML expression - not necessarily corresponding to a syntactically complete subexpression -
  * together with context information about the spelled SQL fragment, in particular table aliases introduced
  * by a spelled ''from'' clause, and a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
  * the statement's parameters included in this fragment. As an artefact produced during spelling
  * of an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] or some other object
  * representing an SQL/DML/DDL syntax element, it consists of three parts:
  *   1. `SpelledSQL.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] which contains the text
  *      of the spelled fragment, specific to the targeted DBMS and in a form accepted by the JDBC
  *      [[java.sql.PreparedStatement PreparedStatement]];
  *   1. `SpelledSQL.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] -
  *      a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] used to set all JDBC parameters included
  *      in the fragment (present in `this.sql` as '?' placeholders), in their order of appearance;
  *   1. `SpelledSQL.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
  *      which contains both any additional information about the fragment necessary for its use as a part
  *      of the complete statement, not included by the other two properties, as well as to spell other SQL fragments,
  *      depending on the spelled syntax element. This in particular includes the aliases for all relation included
  *      in the spelled `RowProduct` instance serving as the domain in which the spelled fragment is grounded.
  *      Unlike the other two, it is an ''in/out'' parameter of the spelling process and can contain information
  *      not limited to the spelled syntax element, but coming from other fragments of the spelled statement
  *      of which the spelled element is a fragment. A spelling method will accept
  *      an [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]] as an input parameter,
  *      enrich it with any information coming from the spelled SQL fragment, and include the modified context.
  *      Another difference is that contexts are not combined when two `SpelledSQL` instances are concatenated,
  *      but the one of the suffix is used, with the one coming from the prefix being discarded. Instead,
  *      the spelling process ensures the correct sequential order (at least where it is relevant).
  *
  * It is an intermediate step of translating an SQL AST [[net.noresttherein.oldsql.sql.Query Query]]
  * or [[net.noresttherein.oldsql.sql.DML DML]] to an [[net.noresttherein.oldsql.sql.Incantation Incantation]],
  * created by methods [[net.noresttherein.oldsql.sql.Query.spell Query.spell]],
  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.spell QuerySQL.spell]]
  * and various methods of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy used
  * for this purpose. As such, it is not typically used by the client code, but only by extensions of the SQL syntax
  * and SQL dialects. The individual fragments corresponding to subexpressions are then combined to form
  * the full statement SQL, a process which involves concatenation
  * of the associated [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] texts
  * and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] forms.
  * @tparam P Parameters of the translated statement.
  *           It is typically the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type of the ''from'' clause
  *           of the translated [[net.noresttherein.oldsql.sql.Query Query]], listing
  *           all its [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters.
  *           A `SpelledSQL[P]` for a whole statement is converted
  *           to an [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[P, _]`.
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext]]
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization]]
  * @author Marcin Mościcki
  */ //consider: adding a read form, possibly optional for selectClause
@SerialVersionUID(ver)
final class SpelledSQL[-P] protected ( //discourage explicit creation, but allow it in case someone wants to extend it.
	    /** The text of this SQL fragment as recursively concatenated chunks, optimised for `O(1)` append/prepend,
	      * but with slow random access.
	      */
		val sql :ChunkedString,

	    /** A form used to set all statement parameters present in the `sql` property. */
	    val setter :SQLWriteForm[P],

	    /** Additional information about the spelled SQL fragment returned by the spelling process,
	      * needed to combine it with other fragments, or to spell subexpressions depending on the spelled SQL object.
	      * It is ''not'' necessarily limited to the scope of the spelled expression, but can include relevant
	      * information about fragments spelled previously: for example, when spelling a ''table expression''
	      * (i.e., a ''select'' in a ''from'' clause, the context would list also preceding tables in the same ''from''
	      * clause. In particular, spelling of a dependent ''select'' requires the context of the enclosed
	      * scope, specified as its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]]
	      * property. All spelling methods, be it of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]
	      * itself or various defaults in [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
	      * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] and others, accept an `SQLContext` parameter
	      * providing information needed to spell the given syntax object. This context is created by spelling
	      * the `RowProduct` instance in which the spelled fragment is grounded. For queries and their subexpressions,
	      * it is the context produced by spelling the ''from'' clause of the encompassing SQL ''select''.
	      */
	    val context :SQLContext[P],

	    /** A flag marking the spelled fragment as a complete and indivisible subexpressions, which can be safely
	      * combined with various operators (in particular logical functions) without a risk of incorrect splitting
	      * of the fragment based on associativity and precedence rules. Defaults to `false`
	      * and typically only references to table columns are marked as atomic.
	      */
		val isAtomic :Boolean = false)
	extends Serializable
{
	/** The list of warnings produced by the spelling process up to this point, in the order of their occurrence. */
	def warnings :Seq[SpellingWarning] = context.warnings //warningStack.reverse

	/** True for an empty `String`. */
	def isEmpty  :Boolean = sql.isEmpty

	/** True if [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] text is not empty. */
	def nonEmpty :Boolean = sql.nonEmpty

	/** Surrounds this SQL with a pair of `'('` and `')'`, unless it is not needed because the expression
	  * cannot possibly be split differently when used as a subexpression of any SQL operation.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.isAtomic]]
	  */
	def inParens :SpelledSQL[P] =
		if (isAtomic) this
		else new SpelledSQL(("(" +: sql) + ")", setter, context, true)

	/** Appends a single character to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `char` appended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +(char :Char) :SpelledSQL[P] = new SpelledSQL(sql + char, setter, context)

	/** Appends the given `String` to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `string` appended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +(string :String) :SpelledSQL[P] = new SpelledSQL(sql + string, setter, context)

	/** Appends the given text to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `string` appended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +(string :ChunkedString) :SpelledSQL[P] = new SpelledSQL(sql + string, setter, context)

	/** Concatenates this SQL fragment with the argument, appending
	  * its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] values to those in this object.
	  * The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context contexts]] are ''not'' combined in any way,
	  * but rather the context from the argument is returned with the result.
	  */
	def +[X <: P](sql :SpelledSQL[X]) :SpelledSQL[X] =
		new SpelledSQL(this.sql + sql.sql, setter + sql.setter, sql.context)

	/** Executes the given spelling function,
	  * passing `this.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] as the argument,
	  * and appends the value it returns to this instance.
	  * This is equivalent to `this + string(this.context)`, useful for chaining spelling methods.
	  * @return a `SpelledSQL` with its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] properties
	  *         being concatenations of the texts and forms from this and the created fragments,
	  *         and the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] from the fragment
	  *         returned by `string`,
	  */
	def +[X <: P, E <: RowProduct](string :SQLContext[P] => SpelledSQL[X]) :SpelledSQL[X] = {
		val suffix = string(context)
		new SpelledSQL(sql + suffix.sql, setter + suffix.setter, suffix.context)
	}

	/** Appends a new warning to the list of [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.warnings warnings]]
	  * associated with this SQL fragment.
	  */
	def +(warning :SpellingWarning) :SpelledSQL[P] = new SpelledSQL(sql, setter, context.warn(warning), isAtomic)
//		new SpelledSQL(sql, setter, context, warning +: warningStack, isAtomic)


	//todo: verify all 'reduce' operations to see if we are not losing forms!!!
	/** Prepends a single character to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `char` prepended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +:(char :Char) :SpelledSQL[P] = new SpelledSQL(char.toString +: sql, setter, context)

	/** Prepends the given `String` to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `string` prepended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +:(string :String) :SpelledSQL[P] = new SpelledSQL(string +: sql, setter, context)

	/** Prepends the given text to this SQL fragment.
	  * @return a `SpelledSQL` with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]]
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]],
	  *         but with `string` prepended to its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] property.
	  */
	def +:(string :ChunkedString) :SpelledSQL[P] = new SpelledSQL(string + sql, setter, context)

	/** Concatenates this SQL fragment with the argument, prepending
	  * its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] values to those in this object.
	  * The contexts are ''not'' combined in any way, but rather
	  * `this.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] is returned with the result.
	  * The result is exactly the same as with `string + this`, but this expression can be freely mixed
	  * with calls of other right-associative methods of `SpelledSQL` and the underlying
	  * [[net.noresttherein.oldsql.morsels.ChunkedString ChunkedString]] slightly better supports left-to-right traversal.
	  */
	def +:[X <: P](sql :SpelledSQL[X]) :SpelledSQL[X] = new SpelledSQL(sql.sql + this.sql, sql.setter + setter, context)


	/** Updates the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] of this SQL fragment
	  * by applying the given function to include an additional condition for the ''where'' clause. The SQL text of
	  * the result is taken to be a boolean condition which should be combined in a logical conjunction with the
	  * rest of the ''where'' clause, and its context is the update of this instance's context with said condition
	  * already included. The process is similar to monadic ''flat map'', with the context being passed through
	  * all chained operations.
	  * @return A `SpelledSQL` instance with the same [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.sql sql]],
	  *         but with the concatenation of [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setters]]
	  *         from `this` and `condition(this.context)`,
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] equal to
	  *         `this.context && condition(this.context`.
	  */
	def &&[X <: P](condition :SQLContext[P] => SpelledSQL[X]) :SpelledSQL[X] = {
		val suffix = condition(context)
		new SpelledSQL(sql, setter + suffix.setter, context && suffix)
	}


	/** Appends a new warning to the list of [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.warnings warnings]]
	  * associated with this SQL fragment.
	  */
	def warn(warning :String) :SpelledSQL[P] = this + new SpellingWarning(warning)


	/** Composes the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] of this instance with the given function.
	  */
	def compose[X](f :X => P) :SpelledSQL[X] = new SpelledSQL(sql, setter unmap f, context compose f)

	/** Composes the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] of this instance with the given extractor.
	  */
	def adapt[X](f :X =?> P) :SpelledSQL[X] = new SpelledSQL(sql, setter compose f, context adapt f)


	/** Returns `this.context.outer`, validating that it is compatible with the context given as the argument.
	  * If `this.context.isSubselect` (which is assumed), than a check is made if `original matches this.context.outer`,
	  * failing which results in throwing an `IllegalStateException`.
	  * If `!this.context.isSubselect` (there is no outer context), then argument itself is returned.
	  *
	  * This method is used when a context associated with a spelled expression (typically a dependent select)
	  * is to be used for spelling other expressions on the same level, which would otherwise use `original`,
	  * in order to pass back 'out' information such as spelling warnings to the caller.
	  */
	private[sql] def outContext[X](original :SQLContext[X]) :SQLContext[X] =
		if (!context.isSubselect)
			original
		else if (original matches context.outer)
			context.outer.adapted
		else
			throw new IllegalStateException(
				"Context " + context.outer + " returned after spelling of " + this +
				" does not match the initial one: " + context + "."
			)


	override def toString :String = {
		val res = new StringBuilder
		res ++= "SQL('"
		sql.appendTo(res)
		res ++= "' " ++= setter.toString ++= "; "
		context.appendTo(res)
		res += ')'
		res.toString
	}
}






/** A factory of SQL fragments.
  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL!]]
  */
object SpelledSQL {
	private[sql] final val ver = 1L

	final val EmptySQL = new SpelledSQL[Any](ChunkedString.empty, SQLWriteForm.empty, SQLContext(), true)

	def empty[P] :SpelledSQL[P] = EmptySQL

	/** An empty SQL fragment with a default (empty) context. */
	def apply() :SpelledSQL[Any] = EmptySQL

	/** An empty SQL fragment with the given context. */
	def apply[P](context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(ChunkedString.empty, SQLWriteForm.empty, context, true)

	/** SQL for a bound parameter with the textual representation of "?" repeated
	  * `param.`[[net.noresttherein.oldsql.sql.ast.BoundParam.writeForm writeForm]]`.`[[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]]
	  * times and separated with ", ".
	  */
	def apply(param :BoundParam[_]) :SpelledSQL[Any] = SpelledSQL(param, SQLContext())

	/** SQL for a bound parameter with the textual representation of "?" repeated
	  * `param.`[[net.noresttherein.oldsql.sql.ast.BoundParam.writeForm writeForm]]`.`[[net.noresttherein.oldsql.schema.SQLWriteForm.columnCount columnCount]]
	  * times and separated with ", ".
	  * @see [[net.noresttherein.oldsql.schema.SQLWriteForm.inlineParam]]
	  */
	def apply[P](param :BoundParam[_], context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(ChunkedString(param.writeForm.inlineParam), param.writeForm.unmap { _ :Any => () }, context, true)

	/** SQL for an unbound parameter with the textual representation of
	  * `form.`[[net.noresttherein.oldsql.schema.SQLWriteForm.inlineParam inlineParam]].
	  */
	def apply[P](form :SQLWriteForm[P]) :SpelledSQL[P] = SpelledSQL(form, SQLContext())

	/** SQL for an unbound parameter with the textual representation of
	  * `form.`[[net.noresttherein.oldsql.schema.SQLWriteForm.inlineParam inlineParam]].
	  */
	def apply[P](form :SQLWriteForm[P], context :SQLContext[P]) :SpelledSQL[P] =
		SpelledSQL(form.inlineParam, form, context)

	def apply[P](sql :String) :SpelledSQL[P] =
		SpelledSQL(ChunkedString(sql))

	def apply[P](sql :String, context :SQLContext[P]) :SpelledSQL[P] =
		SpelledSQL(ChunkedString(sql), context)

	def apply[P](sql :String, form :SQLWriteForm[P], context :SQLContext[P]) :SpelledSQL[P] =
		SpelledSQL(ChunkedString(sql), form, context)

	def apply(sql :ChunkedString) :SpelledSQL[Any] =
		SpelledSQL(sql, SQLWriteForm.empty)

	def apply[P](sql :ChunkedString, form :SQLWriteForm[P]) :SpelledSQL[P] =
		SpelledSQL(sql, form, SQLContext())

	def apply[P](sql :ChunkedString, context :SQLContext[P]) :SpelledSQL[P] =
		SpelledSQL(sql, SQLWriteForm.empty, context)

	def apply[P](sql :ChunkedString, form :SQLWriteForm[P], context :SQLContext[P]) :SpelledSQL[P] =
		new SpelledSQL(sql, form, context, sql.isEmpty)


	@inline def unapply[P](sql :SpelledSQL[P]) :Opt[(ChunkedString, SQLWriteForm[P], SQLContext[P])] =
		Got((sql.sql, sql.setter, sql.context))



	/** Non-local context related to an SQL fragment
	  * (of some [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]).
	  * It contains primarily the aliases of all the tables in the ''from'' clause of the ''select'' using the fragment,
	  * as well as those of any outer ''selects'' in which the current ''select'' is embedded.
	  * While it is not reflected in its type signature, a context always corresponds to a specific
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type, at a level of abstraction similar
	  * to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] ''from'' clauses.
	  *
	  * It is, together with [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]],
	  * one of the parameters shared by almost all spelling methods such as
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
	  * As the `SpelledSQL` instance returned by those methods also contains a context, custom implementations
	  * can be used to pass arbitrary information both to subexpressions (as a parameter) and the enclosing expression
	  * (as a member of `SpelledSQL`).
	  *
	  * Aside from storing the above information, it serves at the same time as the default naming strategy
	  * for both column and table aliases. New instances should be obtained from method
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.newContext newContext]]
	  * of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]], with custom implementations
	  * of the latter allowed to return specialized implementations of the former. For this reason,
	  * in order to avoid losing any extra information, incremental expanding of a context should happen by invoking
	  * various factory methods of the context, in particular specific to `RowProduct` subclasses, or lower level
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.copy copy]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.reset reset]].
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.spellingContext]]
	  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.newContext]]
	  */ //consider: parameterizing it with the RowProduct; renaming to SpellingContext; moving to top level or SQLSpelling
	@SerialVersionUID(ver) //todo: list of all specifically included columns for every relation
	class SQLContext[-P] protected ( //todo: rename to SpellingContext; move outside.
			/** An indexed table containing aliases of all relations, with indexing congruent with that
			  * of [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
			  * expressions used in the SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
			  * being translated. This means that the first alias in the sequence is that for the ''last''
			  * relation in the ''from'' clause, and so on. The exception are expressions based on
			  * ''group by'' clauses, where the grouping relations receive no aliases and indexing stays
			  * the same as for the grouped ''from'' clause (with the aliases of the aggregated relations
			  * remaining available, unlike the relations rows themselves for the SQL expression).
			  * This list includes (unused) aliases for all
			  * occurrences of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] type in the ''from'' clause
			  * as well as placeholder entries for grouping relations ''from outer selects only''
			  * in order to retain consistency with numbering used by `RowProduct` for its joined relations.
			  * An empty string signifies that the relation does not have an alias ''and'' its columns
			  * should be rendered using their unqualified names, without the table name. This situation
			  * is however an exception and should be used only if the alternative would result in
			  * invalid SQL. Normally, if the table does not feature an alias in the ''from'' clause,
			  * its context entry should be the unqualified table name itself, and its columns
			  * should be qualified with the table name instead.
			  */
			val aliasStack :IndexedSeq[String] = PassedArray.empty[String],

			/** The size of the ''from'' clause of the most deeply nested select of the associated SQL
			  * expression/the offset in the alias table of the first table from the outer select.
			  * If the represented clause is aggregated
			  * ([[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]] is non negative),
			  * then this number reflects the size of the ungrouped ''from'' clause
			  * (meaning it's always equal to the size of
			  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]`.`[[net.noresttherein.oldsql.sql.RowProduct.fromClause fromClause]])
			  */
			val tables :Int = 0,

			/** The combined number of [[net.noresttherein.oldsql.sql.GroupBy GroupBy]],
			  * [[net.noresttherein.oldsql.sql.By By]] and [[net.noresttherein.oldsql.sql.GroupParam]]
			  * (including their type aliases) elements of the ''group by'' clause of the SQL fragment.
			  * It is the number of grouping relations which are ''not'' included in the alias index.
			  * Zero signifies an [[net.noresttherein.oldsql.sql.Aggregated aggregate]] clause,
			  * while `-1` is used for `RowProduct` types not conforming to
			  * [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]].
			  */
			val groupings   :Int = -1,

			/** SQL fragments coming from individual join conditions (and other `RowProduct` subcomponents),
			  * which were not included in a ''join ... on'' clause and should become part of a logical
			  * conjunction forming the ''where'' clause. This in particular includes conditions
			  * present on `From` and `JoinParam`.
			  */
			val whereClause       :Seq[SpelledSQL[P]] = PassedArray.empty[SpelledSQL[P]],

			/** Provides names for a chosen subset of tables used by the spelled expression.
			  * The primary use case are unnamed
			  * [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]]s,
			  * but it can be also used to provide overrides, or default aliases
			  * for [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]]s.
			  * Note that [[scala.collection.immutable.Map.withDefault withDefault]] method
			  * of [[scala.collection.immutable.Map Map]] allows in fact to provide here an arbitrary function,
			  * for example wrapping the table name in backticks, or changing capitalization.
			  * This property does not need to contain all, or any, tables used by the formatted expression.
			  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.nameOf]]
			  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.name]]
			  */
			val tableNames :Map[Table[MappingAt], String] = Map.empty[Table[MappingAt], String],

			/** A list of messages about potential problems encountered during spelling so far, in the reverse order
			  * of their occurrence. New warning can be added with
			  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.warn warn]] methods.
			  */ //todo: actually print it
			//consider: moving it to SpelledSQL (and making local to it). The problem are all places
			// which create a new SpelledSQL with factory methods based on existing instances,
			// which would require explicit passing of warnings. Or just scratch that optimisations
			// and rely solely on concatenation methods of this class wherever possible.
			val warningStack :List[SpellingWarning] = Nil,

			//todo: errorStack. We need something like it to list all selects inside a select clause

			/** The context for the ''from'' clause of an outer ''select'' if this context is associated
			  * with a dependent ''select''. Instead of using this property, it is recommended instead to
			  * use [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]] (after an optional
			  * check of [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.isAggregated isAggregated]]),
			  * as it allows this instance to potentially pass information back, adding information to the parent context.
			  */
			val parent :Opt[SQLContext[P]] = Lack
		) extends Serializable
	{
		/** A copy constructor used by all methods adding to the context so subclasses need override only this method.
		  * The difference from `this.copy` is that `whereStack` defaults to `Nil` and `parent` to `Lack`.
		  * This allows 'mapping'/'composing' of the context in regard to the query parameter types
		  * (typically to an empty parameter set) in places where `whereStack` must be empty anyway.
		  * Note that if `parent` context
		  *
		  * This is a low-level method which may potentially create contexts for impossible ''from'' clauses -
		  * use with care. It is recommended to use named arguments, overriding only those properties
		  * which are necessary for increased future compatibility.
		  */
		def reset[X](aliasStack :IndexedSeq[String] = aliasStack, tables :Int = tables, groupings :Int = groupings,
		             whereClause :Seq[SpelledSQL[X]] = PassedArray.empty[SpelledSQL[X]],
		             tableNames :Map[Table[MappingAt], String] = tableNames,
		             warningStack :List[SpellingWarning] = warningStack, parent :Opt[SQLContext[X]] = Lack)
				:SQLContext[X] =
			new SQLContext(aliasStack, tables, groupings, whereClause, tableNames, warningStack, parent)

		/** A copy constructor used by all methods adding to the context, shielding users from its manual creations
		  * by providing all member values. It is just a delegate
		  * to [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.reset reset]], providing a default value
		  * for `whereStack`.
		  *
		  * This is a low-level method which may potentially create contexts for impossible ''from'' clauses -
		  * use with care. It is recommended to use named arguments, overriding only those properties
		  * which are necessary for increased future compatibility.
		  */
		def copy[X <: P](aliasStack :IndexedSeq[String] = aliasStack, tables :Int = tables, groupings :Int = groupings,
		                 whereClause :Seq[SpelledSQL[X]] = whereClause,
		                 tableNames :Map[Table[MappingAt], String] = tableNames,
		                 warningStack :List[SpellingWarning] = warningStack, parent :Opt[SQLContext[X]] = parent)
				:SQLContext[X] =
			reset(aliasStack, tables, groupings, whereClause, tableNames, warningStack, parent)

		/** Adapts this context to a new parameter type. Currently this only adapts
		  * all [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]] chunks.
		  */
		def compose[X](f :X => P) :SQLContext[X] =
			reset(whereClause = whereClause.map(_.compose(f)), parent = parent.map(_.compose(f)))

		/** Adapts this context to a new parameter type. Currently this only composes
		  * all [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]] chunks.
		  */
		def adapt[X](f :X =?> P) :SQLContext[X] =
			reset(whereClause = whereClause.map(_.adapt(f)), parent = parent.map(_.adapt(f)))

		/** Adapts this context to a new parameter type, clearing
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]] in the process.
		  * This has the same effect as [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.reset reset]]`()`,
		  * but the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]] context, if present,
		  * is recursively adapted rather than cleared.
		  *
		  * As [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]] with setter forms
		  * cannot be converted to an arbitrary argument type, calling this method on an instance with a non-empty
		  * `whereStack` will throw an [[UnsupportedOperationException]]. This property is however non-empty
		  * only when spelling a [[net.noresttherein.oldsql.sql.FromClause FromClause]], and cleared before
		  * passing the context to any SQL expressions based on that clause for spelling.
		  */
		@throws[UnsupportedOperationException]("if whereStack is non empty.")
		def adapted[X] :SQLContext[X] =
			if (whereClause.nonEmpty)
				throw new UnsupportedOperationException(
					"Cannot adapt " + this + " to a new argument type because whereStack is non-empty."
				)
			else
				reset(parent = parent.map(_.adapted))

		/** A virtual constructor of a default context for an empty SQL fragment used instead of the global
		  * factory to propagate custom implementations for the use by the whole 'spelling' process.
		  */
		def fresh[X] :SQLContext[X] = reset(aliasStack = ArraySeq.empty[String], tables = 0, groupings = -1)

		/** Checks if the proposed table alias is a legal identifier. This method isn't currently sensibly implemented
		  * and limits itself to verifying that it doesn't fall in the naming schema used for synthetic aliases
		  * used by grouping expressions and unbound parameters.
		  */
		def isLegal(alias :String) :Boolean = isTable(alias)

		private[SpelledSQL] def isTable(alias :String) :Boolean =
			alias != null && alias.length > 0 && !alias.startsWith("?") && !alias.startsWith("<")

		/** Returns the parent context representing the ''from'' clause of an outer select to this context's clause.
		  * Typically it be the instance on which
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.subselect subselect]] was called in order
		  * to create this object (or its ancestor), but subclasses can substitute it in order to pass back
		  * information.
		  */
		@throws[UnsupportedOperationException]("If no outer context is present")
		def outer :SQLContext[P] = parent match {
			case Got(ctx) =>
				if (warningStack eq ctx.warningStack) ctx
				else ctx.copy(warningStack = warningStack)
			case _ =>
				throw new UnsupportedOperationException("No outer context for " + this + ".")
		}

		/** Returns the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]] context
		  * of this instance, or the argument if none exists.
		  * @return `if(`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.isSubselect isSubselect]]`) `[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]]` else other`.
		  */
		def outerOrElse[Q <: P](other :SQLContext[Q]) :SQLContext[Q] =
			if (isSubselect) outer else other

		/** If this context is an adapter/decorator of another instance, return the adapted context.
		  * This can be useful in case an adapter
		  * ('[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.SpellingRedactor redactor]]') is applied
		  * to a spelling strategy which constructs contexts adapting contexts created by the adapted
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]. It allows the adapted spelling
		  * to get hold of the original context it created and potentially access any extra features provided
		  * by a customized implementation.
		  */
		def unwrap :Opt[SQLContext[P]] = Lack

		/** Does this context represent a ''group by'' clause (or an aggregated ''from'' clause in the sense of
		  * [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]])?
		  */
		def isAggregated :Boolean = groupings >= 0

		/** Does this context represent a dependent select, that is a ''from'' clause
		  * with a [[net.noresttherein.oldsql.sql.Subselect Subselect]] join?
		  * If so, than `this.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]]`.isDefined`
		  * is also true and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.outer outer]]
		  * will not throw an exception.
		  */
		def isSubselect  :Boolean = parent.isDefined

		/** The size of the represented ''from'' clause, equal
		  * to [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]`.`[[net.noresttherein.oldsql.sql.RowProduct.size size]].
		  * @return `if (`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]]` >= 0) groupings else `[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.tables tables]].
		  */
		def size :Int =
			if (groupings >= 0) groupings else tables

		/** The total number of relations available to SQL expressions in the represented ''from'' clause.
		  * It is equal to [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]`.`[[net.noresttherein.oldsql.sql.RowProduct.fullSize fullSize]].
		  * @return [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.size size]]` + `[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]]`.mapOrElse(_.fullSize, 0)`.
		  */
		def fullSize :Int =
			(if (groupings >= 0) groupings else tables) + parent.mapOrElse(_.fullSize, 0)


		/**	Aliases of all real tables in the ''from'' clause of the SQL fragment, not including aliases of tables from
		  * outer ''selects'' or synthetic relations corresponding to unbound parameters or grouping expressions,
		  * in the order in which they appear in the linearization of the ''from'' clause.
		  */
		def fromClause :Iterator[String] =
			if (tables == aliasStack.length) aliasStack.reverseIterator.filter(isTable)
			else aliasStack.view.take(tables).reverseIterator.filter(isTable)

		/** Aliases of all tables in the ''from'' clause of the translated SQL fragment, including aliases of tables
		  * from the outer ''selects'', but not including any synthetic relations (for unbound parameters and grouping
		  * expressions).
		  */
		def aliases :Set[String] = aliasStack.view.filter(isTable).toSet

		/** Checks if the context contains the given table alias. This takes into account only usable aliases:
		  * Those of the aggregated tables from outer ''selects'' are not included.
		  */
		def contains(table :String) :Boolean = aliasStack.contains(table)

		/** The name which should be used for the given table in the ''from'' clause of the spelled SQL ''select''.
		  * This is not the alias given to a particular reference to a table in a ''from'' clause, but rather how
		  * the reference should be spelled at all.
		  * @return [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.tableNames tableNames]]`.get(table)`.
		  */
		def nameOf(table :Table[MappingAt]) :Option[String] =
			try { Some(tableNames(table)) } catch { //not tableNames.get to use default, if available
				case _ :NoSuchElementException => None
			}

		/** Adds an override name for the given table
		  * to [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.tableNames tableNames]].
		  * Introducing an override means that all references to this table (outside its introduction)
		  * must use the alias returned by method
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.nameOf nameOf]].
		  * This method is used to resolve any ambiguities between provided aliases
		  * of [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]].
		  */
		@throws[IllegalArgumentException]("if the name is already in use.")
		def name(table :Table[MappingAt], name :String) :SQLContext[P] =
			tableNames.find(_._2 == name) match {
				case Some((table, _)) =>
					throw new IllegalArgumentException(
						"Cannot name " + table + " " + name + ": the name is already in use by table " + table + "."
					)
				case _ =>
					copy(tableNames = tableNames.updated(table, name))
			}

		/** Checks if there are no tables in the formatted ''from'' clause.
		  * @return [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]]`.isEmpty`.
		  */
		def isEmpty :Boolean = aliasStack.isEmpty

		protected def defaultAliasRoot = "query"

		/** Creates a new, unique table alias according to the default naming scheme. */
		def newAlias :String = newAlias("")

		/** Returns a unique alias (different from all tables in the scope) based on the given name.
		  * The method checks first if `proposed` is already in use and, if not, returns it as the recommended alias.
		  * Otherwise, it finds the first unique alias `"proposed_$i"` for `i = 1, 2, ...`. This algorithm
		  * can be however overridden by custom `SQLContext` classes.
		  * @param  proposed A suggested alias to use. An empty string results in using a default root.
		  * @return a `String` which does not occur among the aliases on
		  *         the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]] list.
		  */
		def newAlias(proposed :String) :String = {
			val name = if (proposed.length == 0) defaultAliasRoot else proposed
			if (!aliasStack.contains(name))
				name
			else {
				var i = 1
				var indexed = name + '_' + i
				while (aliasStack.contains(indexed)) {
					i += 1
					indexed = name + '_' + i
				}
				indexed
			}
		}

		/** Returns a unique alias (different from all tables in the scope) based on the given table.
		  * If the given relation has a name associated with it (either table/view name, or an explicitly given alias)
		  * it will be used, as long as it doesn't already exist in this context, in which case a unique suffix
		  * will be appended. Otherwise (the case of a dependent ''select'' in a ''from'' clause), a unique alias
		  * is generated.
		  *
		  * This method delegates to [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.newAlias(proposed:String) newAlias]].
		  * @param  table a table occurring in the ''from'' clause outside of the scope of this context.
		  * @return a `String` which does not occur among the aliases on
		  *         the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]] list.
		  */
		def newAlias(table :Relation[MappingAt]) :String = table match {
			case Aliased(_ :Table.__, alias) => newAlias(alias)
			case t :RelVar[MappingAt] => newAlias(t.name)
			case _ => newAlias(defaultAliasRoot) //we could try to check if the select returns a single entity to provide a more informative alias
		}

		protected def defaultColumnRoot :String = ""

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
			val alias = aliasStack(idx)
			if (!isTable(alias))
				throw new IllegalArgumentException(
					idx.toString + "-th relation '" + alias + "' (counting from the right) in " + this +
						" is not a valid table. Unbound parameters and grouping expressions cannot be aliased."
				)
			alias
		}

		/** The alias used for the given relation expression by the associated SQL.
		  * Note that the relation must be an expression based on the whole ''from'' clause represented by this context
		  * in order to list a valid index: [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
		  * instances carried by [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] clauses always list their index as `0`,
		  * unless [[net.noresttherein.oldsql.sql.ast.JoinedRelation.expand expanded]] through one of its methods.
		  */
		@throws[IllegalArgumentException]("if the relation at the given index is synthetic and does not correspond " +
		                                  "to an actual table in the FROM clause.")
		@throws[IndexOutOfBoundsException]("if the index is less than zero or greater than the number of all relations" +
		                                   "relations in the visible scope, including synthetic relations.")
		def apply(table :JoinedRelation.__) :String = this(table.index)

		/** Adds a new table alias at index `0`, shifting back existing table aliases. */
		@throws[IllegalStateException]("if the context corresponds to an aggregate select (with a group by clause).")
		@throws[IllegalArgumentException]("if the alias is already in use.")
		def join(table :String) :SQLContext[P] =
			if (groupings >= 0)
				throw new IllegalStateException(s"Cannot join '$table' with an aggregate clause: $this.")
			else if (table.length > 0 && aliasStack.contains(table))
				throw new IllegalArgumentException(s"$this already contains a table named '$table' in scope.")
			else
				copy(aliasStack = table +: aliasStack, tables = tables + 1)

		/** Adds a new table alias as the first (index `0`) element of the ''from'' clause of a nested ''select''.
		  * The returned context will contain this instance
		  * as its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]] property.
		  * Additionally, if the represented ''from'' clause has a ''group by'' clause
		  * ([[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]] is non negative),
		  * then all aliases from the current ''from'' clause
		  * (top [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.size size]] elements of
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]]) will be replaced
		  * with `groupings` synthetic aliases for each [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]
		  * relation in the represented clause. These should not be used and are added solely to retain consistent
		  * indexing of the stack with relations in
		  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]`.`[[net.noresttherein.oldsql.sql.RowProduct.fullTableStack fullTableStack]].
		  * @return `this.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.subselect subselect]]`.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.join join]]`(table)`.
		  */
		@throws[IllegalArgumentException]("if the alias is already in use.")
		def subselect(table :String) :SQLContext[P] =
			if (table.length > 0 && contains(table))
				throw new IllegalArgumentException(s"$this already contains table named '$table' in scope.")
			else if (groupings >= 0) {
				val aliases = ArraySeq.newBuilder[String] ++= Iterator.tabulate(groupings)("<grouping" + _ + ">")
				if (tables < aliasStack.length)
					aliases ++= aliasStack.iterator.drop(tables)
				reset(aliasStack = aliases.result(), tables = 1, groupings = -1, parent = Got(this))
			} else
				reset(aliasStack = table +: aliasStack, tables = 1, groupings = -1, parent = Got(this))

		/** Marks the beginning of a new nested ''select''. This does not change current alias indexing unless
		  * the current ''from'' clause is aggregated, in which case synthetic placeholders in the number of
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]] elements
		  * replace the top [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.tables tables]] entries in
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]], representing
		  * the unavailability of the ungrouped proper ''from'' clause .
		  * The `tables` property is set to zero, corresponding to an empty ''from'' clause, and this context
		  * becomes the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]] context
		  * of the result..
		  */
		def subselect :SQLContext[P] =
			if (groupings >= 0) { //groupings == 0 means an aggregate (not group by) clause
				val aliases = Iterator.tabulate(groupings)("<grouping" + _ + ">") ++:
					(if (tables == aliasStack.length) ArraySeq.empty[String] else aliasStack.drop(tables))
				reset(aliasStack = aliases, tables = 0, groupings = -1, parent = Got(this))
			} else
				reset(tables = 0, parent = Got(this))

		/** Pushes a placeholder alias for a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instance
		  * at the index `0` of the table aliases in scope, shifting back existing aliases.
		  * Use `grouped` method instead to represent a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]].
		  * The actual alias will have "?" prepended to it and, if it is not unique, an additional suffix.
		  * @param alias a suggested parameter name (for debugging).
		  */
		@throws[IllegalStateException]("if the represented FROM clause is aggregated (groupings >= 0).")
		def param(alias :String) :SQLContext[P] = {
			val name = newAlias("?" + alias)
			if (groupings >= 0)
				throw new IllegalStateException(s"Cannot join '$alias' with an aggregate clause: $this.")
			else
				copy(aliasStack = name +: aliasStack, tables = tables + 1)
		}

		/** Pushes a placeholder alias for a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] instance
		  * at the index `0` of the table aliases in scope, shifting back existing aliases.
		  * Use `grouped` method instead to represent a [[net.noresttherein.oldsql.sql.GroupParam GroupParam]].
		  * The actual alias will have "?" prepended to it and, if it is not unique, an additional suffix.
		  * This method variant is equivalent to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.param param]]`(alias).`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.adapt adapt]]`(params)`.
		  * @param alias  suggested parameter name (for debugging).
		  * @param params getter returning parameters of this context from an instance of parameters of the
		  *               returned context.
		  */
		@throws[IllegalStateException]("if the represented FROM clause is aggregated (groupings >= 0).")
		def param[X](alias :String, params :X =?> P) :SQLContext[X] = {
			val name = newAlias("?" + alias)
			if (groupings >= 0)
				throw new IllegalStateException(s"Cannot join '$alias' with an aggregate clause: $this.")
			else
				reset(
					aliasStack = name +: aliasStack, tables = tables + 1,
					whereClause = whereClause.map(_.adapt(params)), parent = parent.map(_.adapt(params))
				)
		}

		/** Adds another ''group by'' clause expression corresponding to either
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], [[net.noresttherein.oldsql.sql.By By]] or
		  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] element.
		  */
		@throws[IllegalStateException]("if the associated clause is an instance of Aggregated[_].")
		def grouped :SQLContext[P] =
			if (groupings == 0)
				throw new IllegalStateException("Cannot group an aggregated clause " + this + ".")
			else
				copy(groupings = if (groupings > 0) groupings + 1 else 1)

		/** Adds the given number of ''group by'' expressions ([[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
		  * and [[net.noresttherein.oldsql.sql.By By]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]])
		  * to this context. This has the effect of setting the
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]] property
		  * to the given number if it is currently negative (representing a non-aggregated ''select''), or
		  * increasing its current value by the same amount otherwise (if greater than zero).
		  * @param expressions The number of additional relations for the aggregate clause. Must be non-negative;
		  *                    if zero, the context will represents
		  *                    an [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] ''select''
		  *                    without a ''group by'' clause.
		  */
		@throws[IllegalArgumentException]("if the argument is negative.")
		@throws[IllegalStateException]("if the associated clause is an instance of Aggregated[_].")
		def group(expressions :Int = 1) :SQLContext[P] =
			if (expressions < 0)
				throw new IllegalArgumentException("Cannot add " + expressions + " grouping expressions to " + this + ".")
			else if (groupings == 0)
				throw new IllegalStateException("Cannot group an aggregated clause " + this + ".")
			else
				copy(groupings = if (groupings > 0) groupings + expressions else expressions)

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

		/** Removes all information about grouping expression, essentially resetting the context to the one
		  * before first of the last (contiguous) calls
		  * to [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.group group]].
		  * @return this context with [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]]
		  *          set to `-1`.
		  */
		def ungroup :SQLContext[P] = copy(groupings = -1)

		/** Adds the SQL for a boolean expression which should be included in the ''where'' clause of a larger SQL
		  * fragment.
		  */
		def &&[X <: P](condition :SpelledSQL[X]) :SQLContext[X] = copy(whereClause = whereClause :+ condition)

		/** Creates a context for a subclause of the associated clause by forgetting `relations` last relations.
		  * This count is ''not'' congruent with relation indexing, but instead counts all relations included
		  * in the clause, including those under grouping. If the associated ''from'' clause has a ''group by'' clause
		  * (`this.`[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.groupings groupings]]` > 0`),
		  * then first the counter of grouping expressions is reduced, and only if `relations > groupings`,
		  * aliases for last `relations - groupings` tables are dropped (leading elements
		  * of [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.aliasStack aliasStack]]),
		  * of  the ungrouped ''from'' clause (which are omitted in relation indexing for ''group by'' clauses).
		  * If `relations` exceeds the combined number `max(groupings, 0) + tables)`, shrinking proceeds
		  * to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]] context.
		  * If `relations` is zero and `groupings == 0` (`isAggregated` is true), then a context for the
		  * ungrouped clause is returned (resetting `groupings` to `-1`).
		  */
		def shrink(relations :Int = 1) :SQLContext[P] = {
			def shrinkUngrouped(relations :Int) =           //shrink the actual 'from' clause, removing 'group by' completely
				if (relations < tables)                     //shrink within the same select clause
					copy(aliasStack = aliasStack.drop(relations), tables = tables - relations, groupings = -1)
				else if (parent.isDefined)                   //shrink into the outer clause
					parent.get.shrink(relations - tables)
				else if (aliasStack.sizeIs == relations)    //shrink to an empty context
					reset(aliasStack = ArraySeq.empty, tables = 0, groupings = -1)
				else if (aliasStack.sizeIs < relations)     //blame user, the number is too high
					throw new IllegalArgumentException(
						s"Cannot shrink $this by $relations relations: that is more than the number of aliases in the FROM clause."
					)
				else                                        //we lost the outer context, likely a bug.
					throw new IllegalStateException(
						s"Cannot shrink $this by $relations relations: no outer context available."
					)

			if (relations == 0)
				if (groupings == 0) copy(groupings = -1)        //just unpack the Aggregated[] clause
				else this
			else if (relations > 0)
				if (relations <= groupings)                     //reduce only the number of grouping expressions
					copy(groupings = groupings - relations)
				else if (groupings < 0)                         //if no group by clause then:
					shrinkUngrouped(relations)
				else
					shrinkUngrouped(relations - groupings)
			else
				throw new IllegalArgumentException("Cannot shrink " + this + " by " + relations + " relations.")
		}


		/** Pushes a new warning about a potential issue in the spelled SQL fragment
		  * on the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.warningStack warningStack]].
		  */
		def warn(warning :SpellingWarning) :SQLContext[P] = copy(warningStack = warning::warningStack)

		/** Pushes a new warning about a potential issue in the spelled SQL fragment
		  * on the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.warningStack warningStack]].
		  */
		def warn(warning :String) :SQLContext[P] = warn(new SpellingWarning(warning))

		/** The list of warnings produced by the spelling process up to this point, in the order of their occurrence.
		  * @return [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.warningStack warningStack]]`.reverse`.
		  */
		def warnings :Seq[SpellingWarning] = warningStack.reverse


		/** Checks if the ''from'' clause structure of the associated SQL ''selects'' (including their outer ''selects'')
		  * for the two contexts is equal. This is the case if the numbers of aliases and grouping expressions are equal
		  * and the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.parent parent]] contexts match.
		  */
		def matches(that :SQLContext[_]) :Boolean =
			(that eq this) ||
				tables == that.tables && groupings == that.groupings && aliasStack.length == that.aliasStack.length &&
					(parent.isEmpty || !that.parent.isEmpty && outer.matches(that.outer))


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :SQLContext[_] if other canEqual this =>
				tables == other.tables && groupings == other.groupings && aliasStack == other.aliasStack &&
					whereClause == other.whereClause && parent == other.parent
			case _ => false
		}
		def canEqual(that :Any) :Boolean = that.isInstanceOf[SQLContext[_]]

		override def hashCode :Int = ((
				((tables.hashCode) * 31 + groupings.hashCode) * 31 + aliasStack.hashCode) * 31 + whereClause.hashCode
			) * 31 + parent.hashCode


		def appendTo(builder :StringBuilder) :Unit = {
			def from(ctx :SQLContext[_], res :StringBuilder) :Unit = {
				if (ctx.parent.isDefined)
					from(ctx.parent.get, res)
				res ++= "FROM "
				if (ctx.tables > 0) { //top ctx.tables in ctx.aliasStack in the reverse order
					ctx.aliasStack.reverseIterator.drop(ctx.aliasStack.length - ctx.tables) foreach { res ++= _ ++= ", " }
					res.delete(res.length - 2, res.length)
				}
				if (ctx.groupings > 0)
					res ++= " GROUP BY #" + groupings
				else if (ctx.groupings == 0)
					res ++= " (aggregated)"
			}
			from(this, builder)
			if (whereClause.nonEmpty) {
				builder ++= " WHERE "
				whereClause foreach { builder ++= _.toString ++= " && " }
				builder.delete(builder.length - 4, builder.length)
			}
		}

		override def toString :String = {
			val res = new StringBuilder
			res ++= "SQLContext("
			appendTo(res)
			res += ')'
			res.toString
		}


	}



	object SQLContext {
		private[this] val empty = new SQLContext[Any](ArraySeq.empty[String])

		/** An empty context. */
		def apply() :SQLContext[Any] = empty

		/** A context for a single, [[net.noresttherein.oldsql.sql.FromClause ungrouped]]
		  * [[net.noresttherein.oldsql.sql.RowProduct.TopRow top]] ''from'' clause.
		  * @param aliases aliases for all relations in the represented type, including
		  *                unbounded [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]] parameters.
		  *                The order is the same as they would appear in the final SQL, so inverse
		  *                to the right-to-left indexing of relations within a ''from'' clause.
		  */
		def apply(aliases :IndexedSeq[String]) :SQLContext[Any] =
			new SQLContext(aliases.reverse, aliases.length)


		/** A base class for `SQLContext` decorators. Delegates all methods to the underlying context,
		  * and wraps any returned `SQLContext` instances in another decorator, as returned
		  * by [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.SQLContextProxy.copy copy]].
		  */
		abstract class SQLContextProxy[P](val context :SQLContext[P])
			extends SQLContext[P](context.aliasStack, context.tables, context.groupings, context.whereClause,
			                      context.tableNames, context.warningStack, context.parent)
		{
			protected def copy[X](context :SQLContext[X]) :SQLContext[X]

			override def copy[X <: P](aliasStack :IndexedSeq[String], tables :Int, groupings :Int,
			                          whereClause :Seq[SpelledSQL[X]], tableNames :Map[Table[MappingAt], String],
			                          warningStack :List[SpellingWarning], parent :Opt[SQLContext[X]]) :SQLContext[X] =
				copy(context.copy(aliasStack, tables, groupings, whereClause, tableNames, warningStack, parent))

			override def reset[X](aliasStack :IndexedSeq[String], tables :Int, groupings :Int,
			                      whereClause :Seq[SpelledSQL[X]], tableNames :Map[Table[MappingAt], String],
			                      warningStack :List[SpellingWarning], parent :Opt[SQLContext[X]]) :SQLContext[X] =
				copy(context.reset(aliasStack, tables, groupings, whereClause, tableNames, warningStack, parent))

			override def compose[X](f :X => P) :SQLContext[X] = copy(context.compose(f))
			override def adapt[X](f :X =?> P) :SQLContext[X] = copy(context.adapt(f))
			override def adapted[X] :SQLContext[X] = copy(context.adapted)
			override def fresh[X] :SQLContext[X] = copy(context.fresh)

			override def outer :SQLContext[P] = copy(context.outer)
			override def outerOrElse[Q <: P](other :SQLContext[Q]) :SQLContext[Q] = copy(context.outerOrElse(other))
			override def unwrap :Opt[SQLContext[P]] = Got(context)

			override def join(table :String) :SQLContext[P] = copy(context.join(table))
			override def subselect(table :String) :SQLContext[P] = copy(context.subselect(table))
			override def subselect :SQLContext[P] = copy(context.subselect)
			override def param(alias :String) :SQLContext[P] = copy(context.param(alias))
			override def param[X](alias :String, params :X =?> P) :SQLContext[X] = copy(context.param(alias, params))
			override def grouped :SQLContext[P] = copy(context.grouped)
			override def group(expressions :Int) :SQLContext[P] = copy(context.group(expressions))
			override def aggregate :SQLContext[P] = copy(context.aggregate)
			override def ungroup :SQLContext[P] = copy(context.ungroup)
			override def &&[X <: P](condition :SpelledSQL[X]) :SQLContext[X] = copy(context && condition)
			override def shrink(relations :Int) :SQLContext[P] = copy(context.shrink(relations))

			override def warn(warning :SpellingWarning) :SQLContext[P] = copy(context.warn(warning))
			override def warn(warning :String) :SQLContext[P] = copy(context.warn(warning))

			override def isLegal(alias :String) :Boolean = context.isLegal(alias)
			private[SpelledSQL] override def isTable(alias :String) = context.isTable(alias)
			override def isAggregated :Boolean = context.isAggregated
			override def isSubselect :Boolean = context.isSubselect

			override def size :Int = context.size
			override def fullSize :Int = context.fullSize
			override def fromClause :Iterator[String] = context.fromClause
			override def aliases :Set[String] = context.aliases

			override def contains(table :String) :Boolean = context.contains(table)
			override def nameOf(table :Table[MappingAt]) :Option[String] = context.nameOf(table)
			override def name(table :Table[MappingAt], name :String) :SQLContext[P] = context.name(table, name)
			override def isEmpty :Boolean = context.isEmpty
//			protected override def defaultAliasRoot :String = context.defaultAliasRoot
			override def newAlias :String = context.newAlias
			override def newAlias(proposed :String) :String = context.newAlias(proposed)
			override def newAlias(table :Relation[MappingAt]) :String = context.newAlias(table)
			override protected def defaultColumnRoot :String = context.defaultColumnRoot

			override def apply(idx :Int) :String = context(idx)
			override def apply(table : JoinedRelation.__) :String = context(table)

			override def warnings :Seq[SpellingWarning] = context.warnings

			override def matches(that :SQLContext[_]) :Boolean = context.matches(that)

			override def equals(that :Any) :Boolean = that match {
				case other :SQLContextProxy[_] if other canEqual this => context == other.context
				case _ => false
			}
			override def canEqual(that :Any) :Boolean = this.isInstanceOf[SQLContextProxy[_]]
			override def hashCode :Int = context.hashCode

			override def appendTo(builder :StringBuilder) :Unit = context.appendTo(builder)
		}
	}




	/** Representation of [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam unbound]] parameters
	  * of a ''from'' clause as a parameter type `Ps`. It is used during the SQL spelling process,
	  * with the responsibility of providing for any [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
	  * from that clause a getter function returning the value for the unbound parameter represented
	  * by the given relation from the complete parameter set. These functions allow the creation
	  * of [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] included
	  * in the built [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]], responsible for setting
	  * the JDBC parameters of the [[java.sql.PreparedStatement PreparedStatement]] created for the spelled expression.
	  *
	  * An instance for a given ''from'' clause type `F` can be expanded to larger clauses (such as joins between `F`
	  * and additional tables) by factory methods of this trait specific to particular clause types.
	  * This process also expands the parameter set `Ps`
	  * (assuming it is a [[net.noresttherein.oldsql.collection.Chain Chain]]) and build the parameterization
	  * of a ''from'' clause recursively. Similarly, an instance can be reduced to subclauses of `F`, this time
	  * while retraining the complete parameter set `Ps`. This reversed process is used to recursively spell
	  * the individual expressions included by subclauses, such as filter conditions for the ''where'' clause,
	  * in the context of their declarations, without expanding them all to expressions based on the complete
	  * ''from'' clause of a formatted query.
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
	  * in the manner described above. Additionally, every [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
	  * has a [[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]] property for
	  * the whole parameter chain [[net.noresttherein.oldsql.sql.RowProduct.Params Params]].
	  *
	  * @tparam Ps a composite type combining the values of all individual unbound parameters in `F`, typically
	  *            `F#Params` [[net.noresttherein.oldsql.collection.Chain Chain]] (for concrete `F`).
	  * @tparam F  a (potentially) parameterized ''from'' clause defining the positions of all unbound parameters.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.parameterization]]
	  */ //todo: move this outside or to SQLSpelling
	abstract class Parameterization[-Ps, +F <: RowProduct] extends Serializable { parent =>

		/** Returns an accessor function returning the value of a [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  * parameter of `F` represented by the synthetic relation expression carried by the parameter pseudo join.
		  */
		@throws[IllegalArgumentException]("if the given relation does not represent an unbound parameter of `F`.")
		def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) :Ps =?> S

		/** A special purpose method returning a value for the specified column of a modified table, obtained
		  * from arguments obtained at execution of an [[net.noresttherein.oldsql.sql.Incantation Incantation]].
		  * This method is supported only by few, dedicated `Parameterization` implementations,
		  * used when assigning to a column an SQL expression dependent on an execution time statement parameter value.
		  * This is the case if the column contains an [[net.noresttherein.oldsql.schema.Buff.CustomInsert CustomInsert]]
		  * or a [[net.noresttherein.oldsql.schema.Buff.CustomUpdate CustomUpdate]] buff.
		  */
		@throws[ExecutionTimeSQLException]("if no value for the column has been provided at the creation of this instance.")
		def apply[T[A] <: TypedMapping[S, A], S, V, O >: F <: RowProduct]
		         (table :JoinedTable[O, T], column :TypedColumn[V, O]) :Opt[V] =
			throw new ExecutionTimeSQLException(
				"No predefined value for " + table + "." + column + ". This feature is only supported " +
				"in special circumstances, if the column is assigned a new value in a parameterized insert or update " +
				"statements (such as ParamEntityInsert/ParamEntityUpdate)."
			)

		/** Adapts this instance to a join clause combining clause `F` of this instance with an extra relation `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */ //while E <: L NonParam R, subtyping is only in the type constructor, as it decomposes exactly to L J R
		def join[L <: FromSome, R[O] <: MappingAt[O], E <: L Join R]
		        (implicit left :F <:< L, compose :RowComposition[E, L, _]) :Parameterization[Ps, E] =
			new JoinParameterization[Ps, F, E](this, "join")

		/** Adapts this instance to a join clause combining clause `F` of this instance with an extra relation `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  * This method works for all standard [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] subclasses other
		  * than [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] and
		  * [[net.noresttherein.oldsql.sql.Subselect Subselect]].
		  * @see [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.subselect]]
		  */
		def join[L >: F <: RowProduct, J[A <: L, B[O] <: R[O]] <: A NonParam B, R[O] <: MappingAt[O]](join :L J R)
				:Parameterization[Ps, L J R] =
			if (join.isInstanceOf[Subselect.__ @unchecked])
				new SubselectParameterization[Ps, F, L J R](this, join.name)
			else
				new JoinParameterization[Ps, F, L J R](this, join.name)


		/** Adapts this instance to a subselect clause of clause `F` with a single table `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def subselect[L <: NonEmptyRow, R[O] <: MappingAt[O], E <: L Subselect R]
		             (implicit left :F <:< L, compose :RowComposition[E, L, _]) :Parameterization[Ps, E] =
			new SubselectParameterization[Ps, F, E](this, "subselect")

		/** Adapts this instance to a subselect clause `E` of clause `F`, expanding it with an arbitrary
		  * number of tables. This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by `from.size`.
		  */
		def subselect[E <: SubselectOf[F]](from :E) :Parameterization[Ps, E] =
			new Parameterization[Ps, E] {
				override def apply[T[A] <: TypedMapping[S, A], S, O >: E <: RowProduct](param :JoinedParam[O, T]) =
					if (param.index < from.size) //theoretically prevented by being SubselectOf
						throw new IllegalArgumentException(
							"Requested a parameter for relation " + param + " inside a subselect from clause " + from + "."
						)
					else
						parent[T, S, F](param.moveTo(
							RelationOffset.unsafe[F, param.FromLast, param.position.Rel, T](param.index - from.size)
						))

				override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :E <:< (L Expanded R)) =
					if (expanded(from).left eq expanded(from).outer)
						parent.asInstanceOf[Parameterization[Ps, L]]
					else
						parent.subselect(expanded(from).left.asInstanceOf[SubselectOf[F]]).asInstanceOf[Parameterization[Ps, L]]

				override def ungroup[C <: RowProduct](implicit grouped :E <:< AggregateOfGeneralized[C]) =
					from match {
						case aggr :AggregateClause =>
							val inner = aggr.fromClause.asInstanceOf[SubselectOf[F]]
							parent.subselect(inner).asInstanceOf[Parameterization[Ps, C]]
						case _ =>
							throw new UnsupportedOperationException("Cannot ungroup a non-aggregate clause " + from + ".")
					}

				override def outer[P <: RowProduct](implicit outer :E <:< RowProduct { type Outer <: P }) =
					parent.asInstanceOf[Parameterization[Ps, P]]

				override def typeString :String = from.typeString
			}

		/** Adapts this instance to a ''from'' expanded with a ''group by'' clause into type `F GroupBy R`.
		  * This has the effect of ignoring the relation for the grouping expression (with index `0`) and shifting
		  * to lower indices all relations from the parent (`outer`) ''select'', removing the relations
		  * from the actual ''from'' clause of the subselect of `F` from indexing.
		  */
		def group[L <: FromSome, R[O] <: MappingAt[O], G[A <: L] <: A GroupBy R]
		         (implicit left :F <:< L, size :SubselectSize.Of[L]) :Parameterization[Ps, G[L]] =
			new GroupByParameterization[Ps, F, G[L]](this, size.size - 1)

		/** Adapts this instance to a ''from'' expanded with a ''group by'' clause into type `F GroupBy R`.
		  * This has the effect of ignoring the relation for the grouping expression (with index `0`) and shifting
		  * to lower indices all relations from the parent (`outer`) ''select'', removing the relations
		  * from the actual ''from'' clause of the subselect of `F` from indexing.
		  * @param groupBy an instance of the associated ''from''/''group by'' clause, needed to determine the number
		  *                of skipped relations.
		  */
		def group[L <: FromSome, G[A <: L, B[O] <: R[O]] <: A GroupBy B, R[O] <: MappingAt[O]]
		         (groupBy :L G R)(implicit left :F <:< L) :Parameterization[Ps, L G R] =
			group[L, R, ({ type E[A <: L] = A G R })#E](left, SubselectSize(groupBy.left))

		/** Adapts this instance to a ''group by'' clause with an additional grouping expression represented by
		  * mapping `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def by[L <: GroupByClause, R[O] <: MappingAt[O], G <: L By R]
		      (implicit left :F <:< L, compose :RowComposition[G, L, _]) :Parameterization[Ps, G] =
			new ByParameterization[Ps, F, G](this)

		/** Adapts this instance to a ''group by'' clause with an additional grouping expression represented by
		  * mapping `R`.
		  * This has the effect of shifting indices of all relations - in particular arguments to
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] - by one.
		  */
		def by[L <: GroupByClause, G[A <: L, B[O] <: R[O]] <: A By B, R[O] <: MappingAt[O]]
		      (groupBy :L G R)(implicit left :F <:< L) :Parameterization[Ps, L G R] =
			new ByParameterization[Ps, F, L G R](this)

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */
		def param[Xs <: Chain with Ps, X, L <: NonEmptyRow, R[O] <: UnboundParam[X, O], E <: L ParamClause R]
		         (implicit ev :F <:< L, compose :RowComposition[E, L, _]) :Parameterization[Xs ~ X, E] =
			new ParamParameterization[Xs, X, F, E](this)

		/** Expands both the parameter chain `Ps` and the ''from'' clause `F` associated with this instance
		  * to encompass an additional unbound parameter from the relation added by the pseudo join `E`.
		  * This has the effect of shifting all relation indices by one, and of associating the last (now at index `0`)
		  * relation with the new parameter `X`.
		  */
		def param[Xs <: Chain with Ps, X,
		          L <: NonEmptyRow, J[+A <: L, B[O] <: R[O]] <: A ParamClause B, R[O] <: UnboundParam[X, O]]
		         (join :L J R)(implicit ev :F <:< L) :Parameterization[Xs ~ X, L J R] =
			new ParamParameterization[Xs, X, F, L J R](this)

		/** Lifts this instance to one representing an aggregated select based on `F`, using the special type
		  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[F]`. This removes all relations in the deepest
		  * select of `F` (since last `Subselect` 'join' or `From`/`Dual`) from indexing, shifting down the indices of
		  * the relations from the outer clauses.
		  */
		def aggregate[E <: FromSome](implicit ev :F <:< E, size :SubselectSize.Of[F]) :Parameterization[Ps, Aggregated[E]] =
			new AggregatedParameterization[Ps, E](this.asInstanceOf[Parameterization[Ps, E]], size.size)

		/** Lifts this instance to one representing an aggregated select based on `F`, using the special type
		  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[F]`. This removes all relations in the deepest
		  * select of `F` (since last `Subselect` 'join' or `From`/`Dual`) from indexing, shifting down the indices of
		  * the relations from the outer clauses.
		  */
		def aggregate[E <: FromSome](from :Aggregated[E])(implicit ev :F <:< E) :Parameterization[Ps, Aggregated[E]] =
			new AggregatedParameterization[Ps, E](this.asInstanceOf[Parameterization[Ps, E]], from.clause.size)

		/** Adapts this instance to one based on the decorated clause `E`, without any actual changes in behaviour. */
		def decorate[P >: F <: RowProduct, E <: ExpandingDecorator[P]](implicit compose :RowComposition[E, P, _])
				:Parameterization[Ps, E] =
			this.asInstanceOf[Parameterization[Ps, E]]

		/** Adapts this instance to one based on the decorated clause `E`, without any actual changes in behaviour. */
		def decorate[P >: F <: RowProduct, E[A <: P] <: ExpandingDecorator[A]](from :E[P]) :Parameterization[Ps, E[P]] =
			this.asInstanceOf[Parameterization[Ps, E[P]]]

		/** 'Shrinks' this parameter set by adapting it to the left side of the join it is dedicated to.
		  * The parameters remain the same, regardless if `F` is a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]]
		  * or not - only indexing changes.
		  */
		def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) :Parameterization[Ps, L]

		/** Adapts this parameter set to the ''from'' clause of the outer ''select'' of `F`. */
		//may throw UnsupportedOperationException if there are parameters in the explicit portion of F
		def outer[P <: RowProduct](implicit outer :F <:< (RowProduct { type Outer <: P })) :Parameterization[Ps, P]

		/** Adapts this instance to the 'ungrouped' ''from'' clause under this ''group by'' clause.
		  * This is the reverse operation to
		  * [[[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.group[L<:FromSome,R[O]<:MappingAt[O]]* group]]]:
		  * it replaces in the index all pseudo relations for grouping expressions at the lowest positions
		  * with the relations following the last `Subselect`/`Dual` in the ungrouped clause `F`.
		  * This method is possible to call only if `F` is
		  * an [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
		  * and will throw an `UnsupportedOperationException` otherwise.
		  */
		def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) :Parameterization[Ps, E]

		/** Adapts this instance to the inner clause of the decorated clause `F` of this instance. */
		def undecorate[P <: RowProduct](implicit ev :F <:< ExpandingDecorator[P]) :Parameterization[Ps, P] =
			this.asInstanceOf[Parameterization[Ps, P]]

		/** Presents itself as a parameterization of the [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type
		  * of the current domain type `F`. This is an identity cast derived from the fact that
		  * `RowProduct.`[[net.noresttherein.oldsql.sql.RowProduct.self self]] is always an identity function.
		  */
		def self[S <: RowProduct](implicit self :F <:< (RowProduct { type Self = S })) :Parameterization[Ps, S] =
			this.asInstanceOf[Parameterization[Ps, S]]

		/** Presents itself as a parameterization of the [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type
		  * of the current domain type `F`. This is an identity cast derived from the fact that
		  * `RowProduct.`[[net.noresttherein.oldsql.sql.RowProduct.self self]] is always an identity function.
		  */
		def self[U >: F <: RowProduct, S <: RowProduct](from :U)(implicit self :U <:< RowProduct { type Self = S })
				:Parameterization[Ps, S] =
			this.asInstanceOf[Parameterization[Ps, S]]

		/** This parameterization as a parameterization of its aliased clause. */
		def as[U <: NonEmptyRow, N <: Label](implicit ev :F <:< U) :Parameterization[Ps, U As N] =
			this.asInstanceOf[Parameterization[Ps, U As N]]

		/** This parameterization as a parameterization of its aliased clause. */
		def as[U <: NonEmptyRow, N <: Label](label :N)(implicit ev :F <:< U) :Parameterization[Ps, U As N] = as[U, N]

		/** This parameterization as a parameterization of its aliased clause. */
		def as[U <: NonEmptyRow, N <: Label](from :U As N)(implicit ev :F <:< U) :Parameterization[Ps, U As N] = as[U, N]


		/** Adapts this instance to one using a different parameter set type `Xs`, from which the parameters `Ps`
		  * of this instance can be derived. Note that function `f` will be called by every accessor returned
		  * by [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]], resulting in
		  * potentially costly re-creation of the whole parameter set `Ps` with setting of every parameter.
		  */
		def compose[Xs](f :Xs => Ps) :Parameterization[Xs, F] = new ComposedParameterization[Xs, Ps, F](this, f)

		/** Adapts this instance to one using a different parameter set type `Xs`, from which the parameters `Ps`
		  * of this instance can be derived. Note that extractor `f` will be called by every accessor returned
		  * by [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]], resulting in
		  * potentially costly re-creation of the whole parameter set `Ps` with setting of every parameter.
		  */
		def adapt[Xs](f :Xs =?> Ps) :Parameterization[Xs, F] = new AdaptedParameterization[Xs, Ps, F](this, f)


		private[sql] def typeString :String = "-"
		private[sql] def paramString :String = "???"

		override def toString :String = "Parameterization(" + paramString +" => "+ typeString + ")"
	}



	object Parameterization {

		/** A parameterization of an SQL or DML fragment with no bound or unbound parameters, and hence no setter forms.
		  * It is parameterless in the context of ''unbound'' parameters - all calls to its
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.apply apply]] method throw
		  * an [[IllegalArgumentException]]. Joining it with unbound parameters via method
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization.param param]], expands it with
		  * an unbound parameter.
		  */
		def paramless[F <: ParamlessRow] :Parameterization[Any, F] = instance

		/** A parameterization for the given ''from'' clause, providing accessors for the values
		  * of all unbound parameters. The setter form list of the returned instance is ''empty'' - it doesn't include
		  * entries for bound and unbound parameters appearing in the associated ''where'' clause (and join conditions).
		  */
		def apply[Ps <: Chain, F <: ParamsRow[Ps]](from :F) :Parameterization[Ps, F] =
			new DefaultParameterization(from)

		private[this] val instance = new Paramless[Nothing]



		/** An 'empty' parameterization - one which always throws an [[IllegalArgumentException]] when asked
		  * for a getter. This is safe, because `ParamlessRow` (or any its supertype) does not include any
		  * unbound parameters.
		  * @tparam F - Either a subtype of `ParamlessRow`, or any supertype of such a subtype. Any of these properties
		  *             guarantee that type `F` does not list any unbound parameters.
		  */
		@SerialVersionUID(ver)
		private class Paramless[+F <: RowProduct] extends Parameterization[Any, F] {
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) =
				throw new IllegalArgumentException(
					"Parameter requested for relation " + param + " of a supposedly parameterless clause."
				)

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				this.asInstanceOf[Parameterization[Any, L]]

			override def outer[P <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: P }) =
				this.asInstanceOf[Parameterization[Any, P]]

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				this.asInstanceOf[Parameterization[Any, E]]

			override def typeString = "*"
			override def paramString = "()"
		}



		/** A `Parameterization` implementation based on a full `RowProduct` instance `from`, implementing all
		  * accessor methods globally rather than in an incremental fashion.
		  */
		@SerialVersionUID(ver)
		private class DefaultParameterization[-Ps <: Chain, +F <: RowProduct](protected val from :F)
			extends Parameterization[Ps, F]
		{ parent =>
			private[this] val paramIndex = from.fullTableStack.scanLeft(-1) {
				case (paramCount, relation) if relation.isInstanceOf[JoinedParam.__] => paramCount + 1
				case (paramCount, _) => paramCount
			}.tail.toArray

			override def apply[T[A] <: TypedMapping[P, A], P, O >: F <: RowProduct](param :JoinedParam[O, T]) = {
				val offset = param.index
				val index = paramIndex(offset)
				if (offset > 0 && paramIndex(offset - 1) == index)
					throw new IllegalArgumentException(
						s"Relation's $param offset #$index doesn't match an unbound parameter in $from."
					)
//				else if (!relation.relation.row.isInstanceOf[ParamAt[_]])
//					throw new IllegalArgumentException(
//						s"Relation $relation is not an unbound parameter of $from."
//					)
				val get = ChainGet(index)
				Requisite { params : Ps => params(index)(get).asInstanceOf[P] }
			}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) = {
				val prev = expanded(from).left
				if (from.lastParamOffset != 0)
					new DefaultParameterization[Ps, L](prev)
				else
					new InitParameterization[prev.Params, from.LastMapping[Unit]#Subject, L](
						new DefaultParameterization[prev.Params, L](prev)
					).asInstanceOf[Parameterization[Ps, L]]
			}

			override def outer[P <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: P }) = {
				val outerFrom = outer(from).outer
				if (from.paramCount == outerFrom.paramCount)
					new DefaultParameterization[Ps, P](outerFrom)
				else
					new PrefixParameterization[outerFrom.Params, P](
						new DefaultParameterization[outerFrom.Params, P](outerFrom),
						from.paramCount - outerFrom.paramCount
					)
			}

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) = {
				val ungrouped = grouped(from).generalizedFromClause
				if (from.paramCount == ungrouped.paramCount)
					new DefaultParameterization[Ps, E](ungrouped)
				else
					new PrefixParameterization[ungrouped.Params, E](
						new DefaultParameterization[ungrouped.Params, E](ungrouped),
						from.paramCount - ungrouped.paramCount
					)
			}

			override def typeString  = from.typeString

			override def paramString = {
				val res = new StringBuilder("@~")
				from.fullTableStack.reverseIterator foreach { table =>
					if (table.isInstanceOf[JoinedParam.__ @unchecked])
						res += '~' ++= table.mapping.mappingName
				}
				res.toString
			}
		}



		/** A parameterization adapting another instance for ''from'' clause `L` to its some extension
		  * in the form of `L Extended _`. This class assumes `F` is a non-subselect, non-param join type,
		  * but subclasses modify its behaviour in order to adapt it to other `Adjoin` subtypes.
		  */
		@SerialVersionUID(ver)
		private class JoinParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                  (protected val original :Parameterization[Ps, L], val join :String)
			extends Parameterization[Ps, F]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) =
				if (param.index == 0)
					throw new IllegalArgumentException(
						"The last relation of the associated FROM clause is not an unbound parameter: " + param + "."
					)
				else
					original[T, S, L](param.moveTo(
						RelationOffset.unsafe[L, param.FromLast, param.position.Rel, T](param.index - 1)
					))


			override def left[C <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (C Expanded R)) =
				original.asInstanceOf[Parameterization[Ps, C]]

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E])
					:Parameterization[Ps, E] =
				throw new UnsupportedOperationException(
					"Not grouped FROM clause cannot be ungrouped. This method should not have been possible to call."
				)

			override def outer[P <: RowProduct](implicit outer :F <:< (RowProduct { type Outer <: P })) =
				original.outer[P](outer.asInstanceOf[L <:< (RowProduct { type Outer <: P })])

			override def typeString = original.typeString + " " + join + " _"
			override def paramString = original.paramString
		}


		@SerialVersionUID(ver)
		private class SubselectParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                       (protected override val original :Parameterization[Ps, L], name :String)
			extends JoinParameterization[Ps, L, F](original, name)
		{
			override def outer[P <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: P }) =
				original.asInstanceOf[Parameterization[Ps, P]]
		}


		@SerialVersionUID(ver)
		private class GroupByParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                     (protected override val original :Parameterization[Ps, L], expansion :Int)
			extends JoinParameterization[Ps, L, F](original, "group by")
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) =
				if (param.index == 0)
					throw new IllegalArgumentException(
						s"The mapping of a GroupBy clause is not (should not be) an unbound parameter: $param."
					)
				else
					original[T, S, L](param.moveTo(
						RelationOffset.unsafe[L, param.FromLast, param.position.Rel, T](param.index + expansion)
					))

			override def left[C <: RowProduct, T[O] <: MappingAt[O]](implicit expanded :F <:< (C Expanded T)) =
				throw new UnsupportedOperationException(
					s"GroupBy clause does not expand its left side. This call should have been impossible."
				)

			override def outer[P <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: P }) =
				original.outer[P](outer.asInstanceOf[L <:< (RowProduct { type Outer <: P })])

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				original.asInstanceOf[Parameterization[Ps, E]]
		}


		@SerialVersionUID(ver)
		private class ByParameterization[-Ps, L <: RowProduct, +F <: RowProduct]
		                                (protected override val original :Parameterization[Ps, L])
			extends JoinParameterization[Ps, L, F](original, "by")
		{
			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				original.ungroup[E](grouped.asInstanceOf[L <:< AggregateOfGeneralized[E]])
		}


		@SerialVersionUID(ver) //size = F.size - number of tables under grouping
		private class AggregatedParameterization[-Ps, +F <: FromSome](ungrouped :Parameterization[Ps, F], size :Int)
			extends Parameterization[Ps, Aggregated[F]]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: Aggregated[F] <: RowProduct]
			                  (param :JoinedParam[O, T]) =
				ungrouped[T, S, F](param.moveTo(
					RelationOffset.unsafe[F, param.FromLast, param.position.Rel, T](param.index + size)
				))

			override def left[L <: RowProduct, R[O] <: MappingAt[O]]
			                 (implicit expanded :Aggregated[F] <:< (L Expanded R)) =
				throw new UnsupportedOperationException("Cannot get the left side of an Aggregated clause.")

			override def outer[P <: RowProduct](implicit outer :Aggregated[F] <:< (RowProduct { type Outer <: P })) =
				ungrouped.outer[P](outer.asInstanceOf[F <:< RowProduct { type Outer <: P }])

			override def ungroup[E <: RowProduct](implicit grouped :Aggregated[F] <:< AggregateOfGeneralized[E]) =
				ungrouped.asInstanceOf[Parameterization[Ps, E]]

			override def typeString = "Aggregate(" + ungrouped.typeString + ")"
			override def paramString = ungrouped.paramString
		}


		/** A parameterization for a clause adding an unbound parameter through a `JoinParam` or `GroupParam`
		  * to the ''from'' clause of the adapted instance.
		  */
		@SerialVersionUID(ver)
		private class ParamParameterization[-Ps <: Chain, -P, L <: RowProduct, +F <: RowProduct]
		                                   (original :Parameterization[Ps, L])
			extends Parameterization[Ps ~ P, F]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) =
				if (param.index == 0)
					Requisite { (xs :(Ps ~ P)) => xs.last.asInstanceOf[S] }
				else {
					val offset = RelationOffset.unsafe[L, param.FromLast, param.position.Rel, T](param.index - 1)
					val prev = original[T, S, L](param.moveTo(offset))
					Optional { (xs :(Ps ~ P)) => prev.?(xs.init) }
				}

			override def left[C <: RowProduct, M[O] <: MappingAt[O]](implicit expanded :F <:< (C Expanded M)) =
				new InitParameterization[Ps, P, C](original.asInstanceOf[Parameterization[Ps, C]])

			override def outer[C <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: C }) =
				new InitParameterization[Ps, P, C](
					original.outer(outer.asInstanceOf[L <:< RowProduct { type Outer = C }])
				)

			override def ungroup[C <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[C]) =
				new InitParameterization[Ps, P, C](
					original.ungroup(grouped.asInstanceOf[L <:< AggregateOfGeneralized[C]])
				)

			override def typeString = original.typeString + " param _"
			override def paramString = original.paramString + "~?"
		}


		/** An implementation representing a parameter chain with an additional element in respect to the
		  * adapted parameterization.
		  */
		@SerialVersionUID(ver)
		private class InitParameterization[-Ps <: Chain, -P, +F <: RowProduct](original :Parameterization[Ps, F])
			extends Parameterization[Ps ~ P, F]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) = {
				val initGet = original[T, S, O](param)
				Optional { (ps :(Ps ~ P)) => initGet.?(ps.init) }
			}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				new InitParameterization[Ps, P, L](original.left)

			override def outer[C <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: C }) =
				new InitParameterization[Ps, P, C](original.outer)

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				new InitParameterization[Ps, P, E](original.ungroup)

			override def typeString = original.typeString + " ~ _"
			override def paramString = original.paramString + "~_"
		}


		/** Adapts another parameterization to a longer parameter chain `Ps`, returning getters dropping
		  * last `extraParams` elements from any input before forwarding to the accessor returned for the relation
		  * by `original`.
		  */
		@SerialVersionUID(ver)
		private class PrefixParameterization[-Ps <: Chain, +F <: RowProduct]
		                                    (original :Parameterization[Ps, F], extraParams :Int)
			extends Parameterization[Chain, F]
		{
			if (extraParams < 0)
				throw new IllegalArgumentException(
					"Cannot add a negative number of parameters " + extraParams + " to " + original + "."
				)

			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <: RowProduct](param :JoinedParam[O, T]) = {
				val get = original[T, S, O](param)
				Optional { ps :Chain =>
					var i = extraParams
					var prefix = ps
					while (i > 0) {
						prefix = prefix.asInstanceOf[Chain ~ Any].init
						i -= 1
					}
					get.?(prefix.asInstanceOf[Ps])
				}
			}

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				new PrefixParameterization[Ps, L](original.left, extraParams)

			override def outer[P <: RowProduct](implicit outer :F <:< RowProduct { type Outer <: P }) =
				new PrefixParameterization[Ps, P](original.outer, extraParams)

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				new PrefixParameterization[Ps, E](original.ungroup, extraParams)

			override def typeString :String = original.typeString + " ~ _*" + extraParams
			override def paramString :String = original.paramString + "~?*" + extraParams
		}



		@SerialVersionUID(ver)
		private class ComposedParameterization[-Xs, Ys, +F <: RowProduct](original :Parameterization[Ys, F], f :Xs => Ys)
			extends Parameterization[Xs, F]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <:RowProduct](param :JoinedParam[O, T]) =
				original[T, S, O](param) compose f

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				original.left.compose(f)

			override def outer[P <: RowProduct](implicit outer :F <:< (RowProduct { type Outer <: P })) =
				original.outer[P].compose(f)

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				original.ungroup.compose(f)

			override def compose[Zs](g :Zs => Xs) :Parameterization[Zs, F] = original.compose(f compose g)

			override def typeString = original.typeString
			override def paramString = original.paramString

			override def toString = "ComposedParameterization(" + typeString + ")"
		}



		@SerialVersionUID(ver)
		private class AdaptedParameterization[-Xs, Ys, +F <: RowProduct]
		                                       (original :Parameterization[Ys, F], f :Xs =?> Ys)
			extends Parameterization[Xs, F]
		{
			override def apply[T[A] <: TypedMapping[S, A], S, O >: F <:RowProduct](param :JoinedParam[O, T]) =
				f andThen original[T, S, O](param)

			override def left[L <: RowProduct, R[O] <: MappingAt[O]](implicit expanded :F <:< (L Expanded R)) =
				original.left.adapt(f)

			override def outer[P <: RowProduct](implicit outer :F <:< (RowProduct { type Outer <: P })) =
				original.outer[P].adapt(f)

			override def ungroup[E <: RowProduct](implicit grouped :F <:< AggregateOfGeneralized[E]) =
				original.ungroup.adapt(f)

			override def compose[Zs](g :Zs => Xs) :Parameterization[Zs, F] = original.adapt(f compose g)
			override def adapt[Zs](g :Zs =?> Xs) :Parameterization[Zs, F] = original.adapt(f compose g)


			override def typeString = original.typeString
			override def paramString = original.paramString

			override def toString = "ExtractedParameterization(" + typeString + ")"
		}

	}

}

