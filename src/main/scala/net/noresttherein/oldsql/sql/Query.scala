package net.noresttherein.oldsql.sql

import scala.collection.{EvidenceIterableFactory, IterableFactory}
import net.noresttherein.oldsql.OperationView.SelectView
import net.noresttherein.oldsql.collection.{Chain, Listing, Opt}
import net.noresttherein.oldsql.collection.Chain.ChainApplication
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, InvalidSQLException, MisalignedExpressionException, MismatchedExpressionsException, UndefinedShapeException}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{Relation, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.IndexedMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.MappingQuery.{ComposedMappingQuery, SingleMappingQuery}
import net.noresttherein.oldsql.sql.Query.{AbstractQuery, ComposedQuery, ComposedSingleQuery, QueryReformingTemplate, QueryTemplate, SingleQuery, SingleQueryTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{Complete, GroundRow, TopRow}
import net.noresttherein.oldsql.sql.Select.{ArbitrarySelect, SelectOperator, SelectTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, SQLShape}
import net.noresttherein.oldsql.sql.ast.{ColumnMappingQuery, ColumnQuery, ComponentSQL, CompoundSelectAs, CompoundSelectColumn, CompoundSelectColumnAs, CompoundSelectSQL, HasRowShape, IndexedSQL, MappingQuerySQL, MappingSQL, QuerySQL, RelationSQL, RowShapeCache}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.{LabeledColumnSQL, LabeledValueSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectAs
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.{Alignment, AlignableColumns, QueryReform, Reform, ReformPermissions, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.CompoundSelect.{CompoundSelectTemplate, ReformedCompoundSelectTemplate}
import net.noresttherein.oldsql.sql.ast.MappingSQL.MappingSQLShape
import net.noresttherein.oldsql.sql.ast.QuerySQL.Rows
import net.noresttherein.oldsql.sql.mechanics.Reform.ArityValidator
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.NoReform

//here be implicits
import net.noresttherein.oldsql.slang._





/*  We need both a query type with a public mapping type and without one, for when it's a SQLMapping, or a converted query.
 *  There are three options:
 *  1. Separate traits.
 *     Pros: narrowed down selectClause, clear implementation distinction allowing additional methods,
 *     good type inference, simple and clean type names.
 *     Cons: twice as much code, including any handling code, we loose information and functionality by upcasting.
 *     There is a need both for a ShapedQuery[P, M, V] (conversions) and MappingQuery[P, M] (only valid one for Relations)
 *  1. Make a ShapedQuery[P, M[O] <: MappingAt[O], V] trait and a type alias QuerySQL[P, V] = ShapedQuery[P, MappingAt, V]
 *     Pros: single type - less coding, we don't loose functionality as we always deal with this type.
 *     Much cleaner interface than with type refinement, especially in QueryTemplate.
 *     Cons: we always have the mapping type visible, even very long SQLMapping types - unless we upcast to MappingAt,
 *     but still not clean. Likely better type inference than the other way round.
 *  1. Query[P, V] is the interface trait, and ShapeQuery[P, M[O], V] = Query[P, V] { type RowMapping[O] <: M[P] }
 *     Pros: a clean root type for the majority of cases when we want only to query, not to make a TableExpression.
 *     Easier to go back to variant 1) if we need to do so than with variant 2).
 *     Member types are more flexible as can refer to this.
 *     Cons: awful types, refinements everywhere, the longest compiler errors of all. Likely problems
 *     with type inference, conformance.
 */

/** An SQL statement returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.Select Select]] type hierarchy and
  * set operations on them (such as `UNION`): [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]].
  * All instances are top-level queries: they may be a part
  * of [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]], but cannot exist as dependent ''selects''
  * (subselects of other queries). The latter function is performed by
  * [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]].
  * @tparam P The type of this query parameters, typically a subtype of [[net.noresttherein.oldsql.collection.Chain Chain]].
  *           It will also be the parameter type of [[net.noresttherein.oldsql.sql.Incantation Incantation]]s built
  *           from this query.
  * @tparam R The value type of the [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause select clause]]
  *           of this query.
  * @define Thing `Query`
  * @define thing query
  */ //consider: we might potentially get rid of this class completely and replace it with QuerySQL using a pure param clause
trait Query[P, R]
	extends QueryTemplate[R, ({ type Q[X] = Query[P, X] })#Q]
	   with QueryReformingTemplate[R, Query[P, R]]
	   with Serializable
{
	//todo: Batching; common supertype with DMLStatement or DML at least
	// problems:
	//   DML is co/contravariant and DMLAPI requires it to be so. Param cannot be contravariant because it's From#Params;
	//   DML#bind returns DML, Query#bind returns QuerySQL - and is overriden in several subclasses to narrow it down;
	//   DML#bind switches the Param type to Unit, Query#bind to @~
	//   DML doesn't have a visitor, only DMLStatement
	//   DMLStatement has many options for changing the SelectResult, including updateCount, which makes no sense;
	//   DMLStatement has a (protected) method switching to an arbitrary SelectResult;
	//   DMLStatement#returns and related return a DMLStatement, but Query#returnAs returns an incantation
	//overrides to grant access to classes located in the companion object
//	protected override def component[O] :RowMapping[O]
//	protected override def export[O] :TypedMapping[RowMapping[O]#Subject, O] //= component[O]
	/** Member ''selects'' (or cursors) which constitute this query in order, joined with
	  * [[net.noresttherein.oldsql.sql.Select.SelectOperator operators]] like 'union'.
	  * Any [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]],
	  * in particular [[net.noresttherein.oldsql.sql.Select Select]], returns itself.
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] returns a concatenation
	  * of constituents of its left and right sides.
	  */
	override def constituents :Seq[SingleQuery[P, R]]

	//semantics of unions and the rest:
	// - if the column set is the same, normal union
	// - if not, but the mapping is the homomorphic, than missing columns are added with null values to each operand
	// - if both are mapping-based, with mappings from the same hierarchy, use union column set with a discriminator column
	// - otherwise the column set becomes two separate sets with a discriminator
	def union(other :Query[P, R])     :Query[P, R] = Select.Union(this, other)
	def unionAll(other :Query[P, R])  :Query[P, R] = Select.UnionAll(this, other)
	def minus(other :Query[P, R])     :Query[P, R] = Select.Minus(this, other)
	def intersect(other :Query[P, R]) :Query[P, R] = Select.Intersect(this, other)

	def transform[X](transformation :SQLTransformation[R, X]) :Query[P, X]

	/** Represents this query as one with a different arguments type.
	  * This method is particularly useful when combining several queries in set operations.
	  * @param params a getter deriving a value of this instance's arguments type from the new parameters.
	  */ //todo: extract these two to a template trait
	def compose[X](params :X => P) :Query[X, R] = new ComposedQuery[X, P, R](params, this)

	/** Replaces all [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in this instance with
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters of values taken from the argument,
	  * turning this parameterized query statement
	  * into a ground [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
	  */
	def bind(params :P) :QuerySQL[RowProduct, R]


	//todo: outdated docs
	/** Reforms the ''select'' clauses of all constituent ''selects'' in this query and `second`
	  * to achieve unified column types. The exact details of what operations are allowed depend on `reform`
	  * implementation. However, this is generally limited to some of:
	  *   - including additional columns in a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  *     (sub)expression;
	  *   - excluding additional columns from a `ComponentSQL` (sub)expression;
	  *   - adding columns for 'null' literals to match columns without counterparts in aligned subexpressions;
	  *   - reordering columns.
	  * This is a protected method with a default implementation to which `reform` strategy serves as a facade.
	  *   1. A single ''select'' (or any other query producing a single/already unified row type) immediately delegates
	  *      to `reform(this, second)`, a bridge for method [[net.noresttherein.oldsql.sql.Query.reform_: reform_:]]
	  *      of the argument.
	  *   1. A [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] splits the algorithm into three parts:
	  *     1. reforms itself using `reform.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]];
	  *     1. unifies the `left` subquery with `second`;
	  *     1. unifies the `right` subquery with previously reformed `second`;
	  *     1. unifies the first query again with fully reformed `second`, leaving the latter unchanged.
	  *
	  * If the above scheme fails to produce a `CompoundSelect` with unified member ''selects'',
	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * is thrown. `Reform` implementation is required to be additive in the sense that modifications done
	  * to a query `a` when reforming it query `b` cannot be reverted by reforming `a` with another query `c`.
	  *
	  * Both `this` and `second` are assumed to be internally unified (be a result of calling
	  * [[net.noresttherein.oldsql.sql.Query.reformed reformed]] on some `Query`).
	  * @param second another query of the same value type, combined with this query in a larger `CompoundSelect`.
	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
	  */
	protected def reform[X](second :Query[X, R])(reform :QueryReform)
	                       (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R])

	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
	  * from `this.`[[net.noresttherein.oldsql.sql.Query.reform reform]] by
	  * [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]] instances other than
	  * [[net.noresttherein.oldsql.sql.Select Select]]. It should not be called directly,
	  * but rather through the `Reform` facade method of the same signature, which by default delegates here,
	  * but can be also overriden by specialized reforming strategies.
	  *
	  * The general algorithm is the same as for `reform_:[X](first :Select[X, V])`,
	  * except the argument is not normally changed in the process:
	  * `reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.prohibitReformRight prohibitReformRight]] is used
	  * to unify `first.`[[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]] with the select clause(s)
	  * in this query, and the first query in the returned pair is always `first`, unless this case is explicitly
	  * overridden by a particular `Reform` implementation.
	  * Both `this` and `first` are assumed to be internally unified (be a result of calling
	  * [[net.noresttherein.oldsql.sql.Query.reformed reformed]] on some `Query`).
	  * @param first  a non-compound SQL query combined with this query into some larger `CompoundSelect`
	  *               (as the first query of the pair).
	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
	  */
	protected def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
	                         (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R])

	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
	  * from `this.`[[net.noresttherein.oldsql.sql.Query.reform reform]] by
	  * [[net.noresttherein.oldsql.sql.Select Select]] queries. It should not be called directly, but rather
	  * through the `Reform` facade method of the same signature, which by default delegates here, but can be also
	  * overridden by specialized reforming strategies.
	  *   1. If this query is also a `Select` or is otherwise already unified, its ''select'' clause
	  *      is unified with the ''select'' clause of the argument using the standard reforming process
	  *      of [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] and
	  *      [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.realign reform]].
	  *   1. If this query is a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]],
	  *      the argument is unified first with `this.`[[net.noresttherein.oldsql.sql.CompoundSelect.left left]],
	  *      then the result is unified with `this.`[[net.noresttherein.oldsql.sql.CompoundSelect.right right]],
	  *      and finally reformed `left` and `right` queries are unified with each other and combined in a returned
	  *      `CompoundSelect`.
	  *
	  * For this scheme to work, `reform` must be additive in the sense that a query `a`, unified with a query `b`,
	  * cannot have its changes reverted when unifying it with another query `c`.
	  * @param first  an SQL ''select'' combined with this query into some larger `CompoundSelect` (as the first query
	  *               of the pair).
	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
	  */
	protected def reform_:[X](first :Select[X, R])(reform :QueryReform)
	                         (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		((first :SingleQuery[X, R]) reform_: this)(reform)


	private[sql] def `->reform`[X](other :Query[X, R])(reform :QueryReform)
	                              (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
		this.reform(other)(reform)

	private[sql] def `->reform_:`[X](first :SingleQuery[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		reform_:(first)(reform)

	private[sql] def `->reform_:`[X](first :Select[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		reform_:(first)(reform)


	/** Converts this query into its textual representation for the DBMS specified implicitly by the `spelling` argument,
	  * with additional meta data about query parameters. This is a simple caching forwarder to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.spell[P,V](query:Query[P,V])* spell]] method
	  * of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] and should not be generally overriden
	  * by subclasses unless to enrich the result with additional information without changing the complete SQL.
	  * Implement [[net.noresttherein.oldsql.sql.Query.defaultSpelling(context:SQLContext)(implicit spelling:SQLSpelling)]]
	  * in order to define how this AST node should be formatted as SQL instead.
	  * It is a lower level method and, in most cases, the top-level method
	  * [[net.noresttherein.oldsql.sql.Query.chant chant]] should be used should be used instead, unless specifically
	  * in order to create an [[net.noresttherein.oldsql.sql.Incantation Incantation]].
	  */
	@throws[InseparableExpressionException]("if a subexpression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	@throws[InvalidSQLException]("if the expression is dedicated to a particular DMBS and requires a more specific " +
		                         "SQLSpelling implementation than the provided implicit spelling argument.")
	def spell(implicit spelling :SQLSpelling = StandardSQL.spelling) :SpelledSQL[P] =
		cachedSpelling match {
			case null =>
				val sql = spelling.spell(this)
				cachedSQL = sql
				cachedSpelling = spelling
				sql
			case s if s == spelling => cachedSQL
			case _ => spelling.spell(this)
		}

	@volatile private var cachedSQL      :SpelledSQL[P] = _
	@volatile private var cachedSpelling :SQLSpelling = _


	/** Generates the SQL `String` for this query as a parameterized expression. This is a fallback method
	  * used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] if no non-standard representation
	  * is required for the used DBMS. Subclasses should return standard SQL unless the class itself is dedicated
	  * to a particular DBMS.
	  */
	@throws[InseparableExpressionException]("if a subexpression cannot be separated into individual column strings, " +
	                                        "for example a multi-column SQL select.")
	@throws[InvalidSQLException]("if the expression is dedicated to a particular DBMS and requires a more specific " +
	                             "SQLSpelling implementation than the provided implicit spelling argument.")
	protected def defaultSpelling(context :SQLContext[P])(implicit spelling :SQLSpelling) :SpelledSQL[P]

	private[oldsql] final def defaultSpelling(spelling :SQLSpelling)(context :SQLContext[P]) :SpelledSQL[P] =
		defaultSpelling(context)(spelling)


	/** Converts this query SQL AST into an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
	  * proper for the DBMS using the implicit [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
	  * The implementation is delegated to the dialect object.
	  */
	@throws[InvalidSQLException]("if the expression cannot be rendered as SQL for the DBMS particular to the given dialect.")
	def chant[Res](implicit composition :StatementResult[R, Res], dialect :SQLDialect = StandardSQL) :Incantation[P, Res] =
		dialect(this)

	/** Converts this query SQL AST into an executable
	  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[P, C[V]]` proper to the implicit
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
	  * a collection `C[V]` with the given factory.
	  */
	def returnAs[C[_]](collection :IterableFactory[C])
	                  (implicit dialect :Maybe[SQLDialect]) :Incantation[P, C[R]] =
		chant(StatementResult(collection)(rowForm), dialect getOrElse StandardSQL)

	/** Converts this query SQL AST into an executable
	  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[P, C[V]]` proper to the implicit
	  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
	  * a collection `C[V]` with the given factory.
	  */
	def returnAs[C[_], E[_]](collection :EvidenceIterableFactory[C, E])
	                  (implicit ev :E[R], dialect :Maybe[SQLDialect]) :Incantation[P, C[R]] =
		chant(StatementResult(collection)(ev, rowForm), dialect getOrElse StandardSQL)

	def homomorphic(that :Query.__) :Boolean
	def isomorphic(that :Query.__) :Boolean
	def equivalent(that :Query.__) :Boolean
	def identical(that :Query.__) :Boolean

}


/**
 * @define This `Query`
 * @define this query
 */
object Query {
	type __ = Query[_, _]

	trait AbstractQuery[R] extends HasRowShape {
		/** The nominal type of this query, that is the type
		  * of `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.mapping mapping]] representing the selected
		  * columns and defining the assembly process of final values of `V` from their values.
		  */ //consider: renaming to SelectClause
		type RowMapping[O] <: MappingAt[O]

		/** The 'nominal' mapping of the [[java.sql.ResultSet ResultSet]] of this query. For queries selecting
		  * a single component (in particular, rows from a single table), this will be the nominal
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
		  * of the corresponding [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression used
		  * as the [[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause select]] clause. For queries
		  * returning arbitrary column sets, in particular joined queries returning entities from multiple tables,
		  * this will be a [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] instance wrapping the expression used
		  * as the ''select'' clause.
		  * @see [[net.noresttherein.oldsql.sql.Query.QueryTemplate.export]]
		  */
		def mapping[O] :RowMapping[O]

		/** The definitive version of the nominal [[net.noresttherein.oldsql.sql.Query.QueryTemplate.mapping mapping]]
		  * for the result set.
		  * For a [[net.noresttherein.oldsql.sql.Select Select]]/[[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]],
		  * its default column set for the [[net.noresttherein.oldsql.OperationView.SelectView SelectView]] defines
		  * the actually selected columns. The column names in this mapping may be changed with regard to the original.
		  */
		def export[O]  :TypedMapping[RowMapping[O]#Subject, O]

		/** The form for reading and assembling the whole row of this query.
		  * Unlike [[net.noresttherein.oldsql.sql.SQLExpression.selectForm selectForm]], the assembled value
		  * is of the true entity type `V`, rather than the query
		  * value type [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[V]`.
		  */
		def rowForm    :SQLReadForm[R]

		/** Number of SQL ''selects'' within this query. [[net.noresttherein.oldsql.sql.Select Select]] and
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] report simply `1`, while
		  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
		  * sum the numbers from their left and right side.
		  */
		def selectCount :Int

		/** A template/mock expression defining the type of the ''select'' clause(s) of all constituent
		  * SQL ''select''s (and
		  * [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]]/[[net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL SingleQuerySQL]]).
		  * For a single ''select''s it is simply the expression used as its ''select'' clause, while
		  * other non-compound queries use an arbitrary expression best representing the columns in the cursor
		  * (typically either a [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentShape ComponentLayout]]
		  * or an expression with `null` placeholders for all columns). For a ''compound select'',
		  * it is an expression representing the unified `selectClause` properties of its left and right side.
		  * If a ''compound select'' has been [[net.noresttherein.oldsql.sql.Query.reformed reformed]],
		  * then this expression has the same structure as `this.left` and `this.right`
		  * (as defined by the used [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]]). This typically
		  * means that both expressions are equal except for actual column values and
		  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] subexpressions which may use different,
		  * although for related [[net.noresttherein.oldsql.schema.Mapping mappings]]. This is not a hard rule
		  * and expressions with larger differences can still be sometimes unified (for example
		  * two [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]] expressions with a different order
		  * of keys). If this ''compound select'' has not been reformed, but was created with arbitrary queries
		  * with the same value type, the method will throw an exception
		  * if the [[net.noresttherein.oldsql.sql.SQLExpression.shape row shapes]] of the ''select'' clauses differ.
		  */
		@throws[MismatchedExpressionsException]("if not all selects in this query are of the same shape.")
		def selectClause :SQLShape[R]

		/** The mappings for the selected columns. */
		def columns    :Seq[TypedColumn[_, this.type]]

		/** The ''with'' clause of this query. By default, a query gets a single ''with'' clause preceding the whole query,
		  * which collects all [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]] instances
		  * used by all its subexpressions. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] instances
		  * of dependent ''selects'' do not, generally, receive a ''with'' clause in the final SQL, but instead
		  * the referenced named table expressions will be included in this main clause. This can be changed by
		  * adding a ''common table expression'' to the [[net.noresttherein.oldsql.sql.WithClause.local local]]
		  * part of this clause in any query, which requests their placement in a ''with'' clause directly preceding
		  * the query, rather than the default [[net.noresttherein.oldsql.sql.WithClause.outer outer]] table subset.
		  * For top level queries, tables from both parts are always included. Whether this distinction will be honored,
		  * however, depends on a [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy used
		  * to format this instance into SQL. The `withClause` of a
		  * [[net.noresttherein.oldsql.sql.Select Select]]/[[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]
		  * is the sum of [[net.noresttherein.oldsql.sql.SQLExpression.outerWithClause outerWithClause]]
		  * of its [[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause select]] clause
		  * and [[net.noresttherein.oldsql.sql.RowProduct.withClause withClause]]
		  * of its [[net.noresttherein.oldsql.sql.Select.SelectTemplate.from from]] clause which, together,
		  * list all table expressions referenced anywhere within that ''select''. Similarly, a `withClause` of a
		  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
		  * is the sum of `outer` with clauses of its member queries. Unlike in other
		  * [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]], `outerWithClause` of any
		  * [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] is defined simply as `withClause.outer`.
		  *
		  * The requested placement of a ''common table expression'' in the formatted query can be modified
		  * to the directly encompassing ''select'' on the level of any expression using its
		  * [[net.noresttherein.oldsql.sql.SQLExpression.withLocal withLocal]] method.
		  * @see [[net.noresttherein.oldsql.sql.ast.SelectSQL.outerWithClause outerWithClause]]
		  * @see [[net.noresttherein.oldsql.sql.Query.QueryTemplate.localWithClause localWithClause]]
		  */
		def withClause :WithClause

		/** A collection of ''common table expressions'' explicitly marked for including in a ''with'' clause
		  * of this query.
		  * @return [[net.noresttherein.oldsql.sql.Query.QueryTemplate.withClause withClause]]`.`[[net.noresttherein.oldsql.sql.WithClause.local local]].
		  */
		def localWithClause :WithClause = withClause.local

		/** Says if the query's internal structure will lead, to the best of existing knowledge, to valid SQL.
		  * This check is done without [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]
		  * or [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]], and is not binding.
		  * Said strategies may affect the final form of the query even if straightforward translation would result
		  * in a valid query without it. It is however considerably more efficient than performing a full reform,
		  * and is a good indicator of the likelihood of validity of the query. This level of confidence is also enough
		  * for all of the property methods for forms, shape, etc., to not throw an
		  * [[net.noresttherein.oldsql.exceptions.InvalidSQLException InvalidSQLException]].
		  * [[net.noresttherein.oldsql.sql.Query.QueryTemplate.reformed Reformed]] queries are always roughly consistent.
		  *
		  * This method is pertinent mostly to ''compound selects'', which are in danger of being constructed
		  * from misaligned subqueries.
		  */
		def isRoughlyConsistent :Boolean

		/** Check if the $this was properly
		  * [[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.reformed reformed]]
		  * by some [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] instance.
		  * If so, the expression was validated and is in its final form. For ''compound selects'', this means
		  * that their left and right subqueries were deemed compatible and may be treated as such. The exact level
		  * of integrity depends on the [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] used,
		  * but by default this means that both row [[net.noresttherein.oldsql.sql.Query.QueryTemplate.shape shapes]]
		  * and expression structure
		  * (the generalized [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause selectClause]] expression)
		  * are compatible - the columns have types which are considered equivalent from the point of view of a query,
		  * as defined by [[net.noresttherein.oldsql.sql.RowShape.<:> <:>]], and expressions are either of the same
		  * class (including subexpressions), or otherwise the column type match is considered not be an effect
		  * of chance (for example, [[net.noresttherein.oldsql.sql.ast.IndexedSQL IndexedSQL]]'s keys match exactly
		  * the column names of a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] it is aligned with.
		  * Again, the exact meaning depends on spelling and reform used.
		  *
		  * Being reformed automatically implies being
		  * [[net.noresttherein.oldsql.sql.Query.QueryTemplate.isRoughlyConsistent roughly consistent]].
		  * @see [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.reformed]]
		  * @see [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.isRoughlyConsistent]]
		  */
		def isReformed :Boolean

		/** Specifies the restrictions to reforming that must be observed when reforming the argument query.
		  * These permissions are than combined in logical
		  * [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.& conjunction]] (i.e., the common
		  * subset) with those of the [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] used.
		  */
		protected def allowedReforms(implicit spelling :SQLSpelling) :Permissions

		private[sql] def `->allowedReforms`(implicit spelling :SQLSpelling) :Permissions = allowedReforms

//		/** The default strategy for determining the final shape of rows returned by this query.
//		  * This is only really important for ''compound selects'', which need to reform the select clauses
//		  * of all member ''selects''. Default implementation returns simply
//		  * `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]],
//		  * but [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]
//		  * and [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] delegate to
//		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]].
//		  * or `operator.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]],
//		  * if the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] of argument `spelling`
//		  * is [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope TopScope]].
//		  *
//		  * This method is used as a fallback; there is no guarantee that the returned reform will be really used,
//		  * or it may only be used to change detailed permissions for possible reforming operations
//		  * on another `QueryReform`.
//		  */
//		def defaultReform(implicit spelling :SQLSpelling) :QueryReform = spelling.queryReform
//
//		/** The default reforming strategy for unifying the combined select clause of this query, used
//		  * used for top level queries, that is when this query is the whole spelled expression, and not a subquery
//		  * of another ''compound select''. The default implementation delegates to
//		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]].
//		  */
//		def topReform(implicit spelling :SQLSpelling)     :QueryReform = QueryReform.bottomUp
	}


	/** Methods for unifying shapes of compound queries (reforming). Separate from
	  * [[net.noresttherein.oldsql.sql.Query.QueryTemplate QueryTemplate]] because for
	  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate CompoundSelectTemplate]] `Reformed <: Q[R]`,
	  * while for [[net.noresttherein.oldsql.sql.Query.SingleQueryTemplate SingleQueryTemplate]] `Q[R] <: Reformed`,
	  * where `Q[_]` is the type argument given to `QueryTemplate`.
	  */ //todo: merge with QueryTemplate; rename to QueryOps
	trait QueryReformingTemplate[R, +Reformed] extends AbstractQuery[R] {
		/** Unifies the ''select'' clauses of all constituent subqueries by aligning their corresponding columns.
		  * Individual [[net.noresttherein.oldsql.sql.ast.SelectSQL select expressions]] normally return themselves,
		  * while [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] recursively unify
		  * their left and right sides.
		  * @return `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]][[net.noresttherein.oldsql.sql.mechanics.QueryReform.apply[P,V](query:Query[P,V]) (this)]].
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and cannot be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		def reformed(implicit spelling :SQLSpelling) :Reformed = spelling.queryReform(this)

		//todo: update docs
		/** Default algorithm for unifying the column sets of all subqueries (if this query is
		  * a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]).
		  * Individual [[net.noresttherein.oldsql.sql.ast.SelectSQL select expressions]] normally return themselves,
		  * while [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] recursively unify
		  * their left and right sides.
		  *
		  * The argument `reform` is ''not'' used by `CompoundSelectSQL` to unify its member ''select''s;
		  * rather, it is used as a factory to obtain a proper instance for this `CompoundSelectSQL`: `reform(this)`.
		  * This allows to algorithms which work globally, passing information also top to bottom instead
		  * of just building the result 'from the ground up'.
		  * @param reform a parent reforming strategy used as a ''factory'' for a proper `Reform` or to modify
		  *               this query.
		  */
		@throws[MisalignedExpressionException]("if the expression is internally inconsistent and cannot be reformed " +
		                                       "to attain a unified shape.")
		@throws[UndefinedShapeException]("if this expression or its subexpression has no definite shape " +
		                                 "(for example, if it includes a LooseComponent).")
		protected def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Reformed

		private[sql] def `->reformed`(reform :QueryReform)(implicit spelling :SQLSpelling) :Reformed =
			this.reformed(reform)
	}


	/** A template for classes representing generalized ''selects'' - both standard ''select'' queries and
	  * compound ''selects'' combining several individual ''selects'' with set operators.
	  * It is inherited by both [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] and
	  * [[net.noresttherein.oldsql.sql.Query Query]].
	  * @tparam R $R
	  * @tparam Q $Q
	  * @define this query
	  * @define Same `Query`
	  * @define R    the value type of the [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause select clause]]
	  *              expression, to which each row is mapped..
	  * @define Q    the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *              with its value type `R`.	  */
	trait QueryTemplate[R, +Q[_]] extends AbstractQuery[R] { this :Q[R] =>
//		@deprecated("this is here temporarily because RowMapping does not preserve the subject type at this time.", "Scala 3")
		def supertyped :Q[R] { type RowMapping[O] <: BaseMapping[R, O] } =
			this.asInstanceOf[Q[R] { type RowMapping[O] <: BaseMapping[R, O] }]

		/** Member ''selects'' (or cursors) which constitute this query in order, joined with
		  * [[net.noresttherein.oldsql.sql.Select.SelectOperator operators]] like 'union'.
		  * Any [[net.noresttherein.oldsql.sql.Query.SingleQueryTemplate single query]],
		  * in particular [[net.noresttherein.oldsql.sql.Select.SelectTemplate select]], returns itself.
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate Compound selects]] return concatenation
		  * of constituents of its left and right sides.
		  */
		def constituents :Seq[Q[R]]


		/** Converts this query to an instance with `X` as its ''select'' clause's the value type, without changing
		  * the SQL representation.
		  * Implicit `Lift` type classes exist for various standard types with values largely interchangeable
		  * from the point of view of the database. This method allows unifying the expressions used as
		  * ''select'' clauses to the same Scala type.
		  * Similar to [[net.noresttherein.oldsql.sql.SQLExpression.to]], but accepted conversion works
		  * on the row value type `V`, rather than this expression's value type
		  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.Rows Rows]]`[V]`.
		  * @return a query of the same kind, with the ''select'' clauses of constituting selects being converted
		  *         with `selectClause.to[X]`.
		  */
		def rowsTo[X](implicit conversion :SQLConversion[R, X]) :Q[X] = adaptRows(conversion)

		def adaptRows[X](adaptation :SQLAdaptation[R, X]) :Q[X]

		//Transform is not here because SQLTransformation doesn't always translate a column to a column,
		// so SelectColumn cannot return a SelectColumn
//		def transform[X](conversion :SQLTransformation[R, X]) :Q[X]

		//Todo: rename either these or SQLExpression.map as, even if they won't have conflicting erasures due to
		// different return types, will break inference of type arguments. Same for overload with the other Query.map
		def mapRows[X](f :R => X) :Q[X] = rowsTo(SQLConversion(".mapRows", f))// { type RowMapping[O] <: QueryTemplate.this.RowMapping[O] }

		def mapRows[Fun, C <: Chain, X]
		           (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C) :Q[X] = // { type RowMapping[O] <: QueryTemplate.this.RowMapping[O] } =
			mapRows(applyFun(f))

		protected def applyFun[Fun, C <: Chain, X]
		                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C) :R => X =
			{ v => application(f, isChain(v)) }


		/** A collection of all ''select'' clauses of member ''selects'' in `query`, in the order of their appearance,
		  * paired with `Permissions` describing allowed reforms for that ''select'' when unifying the shape
		  * of the query. This method descends recursively in ''compound selects'' until it reaches
		  * a [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]]/[[net.noresttherein.oldsql.sql.ast.SelectSQL.SingleQuerySQL SingleQuerySQL]],
		  * combining argument `permissions` with
		  * `query.`[[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.allowedReforms allowedReforms]];
		  * for a left side of a ''compound select'',
		  * `permissions.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.asInLeft asInLeft]]
		  * are used for the whole query, and vice versa for the right side of a ''compound select''.
		  */ //todo: topDown vs bottomUp
		protected def alignment(permissions :Permissions)(implicit spelling :SQLSpelling) :Alignment

		private[sql] def `->alignment`(permissions :Permissions)(spelling :SQLSpelling) :Alignment =
			alignment(permissions)(spelling)

		def canEqual(that :Any) :Boolean = that.getClass == getClass
	}


	type ReformableQuery[P, R, +Reformed] = Query[P, R] with QueryReformingTemplate[R, Reformed]



	/** Common declarations of queries which are not ''compound selects''. */
	trait SingleQueryTemplate[R, +Q[_], +Reformed]
		extends QueryTemplate[R, Q] with QueryReformingTemplate[R, Reformed]
	{ this :Q[R] with Reformed =>
		override def constituents :Seq[Q[R]] = this::Nil
		override def selectCount = 1
		override def isRoughlyConsistent :Boolean = true

		//todo: actually make it possible for spelling to do something with these queries
		override def isReformed :Boolean = true

//		override def reformed(implicit spelling :SQLSpelling) :Reformed = spelling.queryReform(this)
		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Reformed = this

		override protected def allowedReforms(implicit spelling: SQLSpelling): Permissions = NoReform

		protected override def alignment(permissions: Permissions)(implicit spelling: SQLSpelling): Alignment =
			new Alignment(selectClause, permissions)
	}



	/** A `Query` which is not a [[net.noresttherein.oldsql.sql.CompoundSelect Compound Select]], that is
	  * cannot be divided into component queries. Most importantly, this includes the basic
	  * [[net.noresttherein.oldsql.sql.Select Select]], but can represent also cursors coming from different sources,
	  * such as returned by stored procedures. Aside from `Select`, these queries' ''select'' clause
	  * typically cannot be reformed.
	  */
	trait SingleQuery[P, R]
		extends Query[P, R]
		   with SingleQueryTemplate[R, ({ type Q[X] = SingleQuery[P, X] })#Q, SingleQuery[P, R]]
	{

		override def transform[X](transformation :SQLTransformation[R, X]) :SingleQuery[P, X]

		override def compose[X](params :X => P) :SingleQuery[X, R] =
			new ComposedSingleQuery(params, this)


//		override def bind(params :P) :SingleQuerySQL[RowProduct, V]

		protected override def reform[X](second :Query[X, R])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
			(this reform_: second)(reform)

		protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		{
			val validator = reform.reform.prohibitAll
			validator[Nothing, Grouped, R, SQLShape, Nothing, Grouped, R, SQLShape, R](
				first.selectClause, selectClause)(SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
			)
			(first, this)
		}

		protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		{
			val selectClauseReform = reform.reform.prohibitReformRight
			val selectClause =
				selectClauseReform[
					first.From, Grouped, R, SQLExpression.from[first.From]#rows[Grouped]#E,
					Nothing, Grouped, R, SQLShape, R
				](
					first.selectClause, this.selectClause)(
					SQLConversion.toSelf, SQLConversion.toSelf, spelling.inSelect
				)._1
			val left =
				if (selectClause eq first.selectClause) first
				else first selectOther selectClause
			(left, this)
		}
	}

	object SingleQuery {
		type __ = SingleQuery[_, _]
	}




	class ComposedQuery[Args, Y, V](val argmap :Args => Y, val query :Query[Y, V])
		extends Query[Args, V]
	{
		override type RowMapping[O] = query.RowMapping[O]

		override def mapping[O] :RowMapping[O] = query.mapping
		override def export[O] :TypedMapping[RowMapping[O]#Subject, O] = query.export

		override def columns :Seq[TypedColumn[_, this.type]] = query.columns.withOrigin[this.type]
		override def selectCount :Int = query.selectCount

		private lazy val _constituents :Seq[SingleQuery[Args, V]] = query.constituents.map(_.compose(argmap))
		override def constituents :Seq[SingleQuery[Args, V]] = _constituents

		override def selectClause :SQLShape[V] = query.selectClause
		override def withClause   :WithClause = query.withClause

		override def isRoughlyConsistent :Boolean = query.isRoughlyConsistent
		override def isReformed :Boolean = query.isReformed

		override def transform[X](transformation :SQLTransformation[V, X]) :Query[Args, X] =
			query.transform(transformation).compose(argmap)

		override def adaptRows[X](conversion :SQLAdaptation[V, X]) :Query[Args, X] =
			query.adaptRows(conversion).compose(argmap)

//		override def map[X](f :V => X) :Query[Args, X] = query.map(f).compose(argmap)

		override def compose[X](params :X => Args) :Query[X, V] =
			new ComposedQuery(params andThen argmap, query)

		override def bind(params :Args) :QuerySQL[RowProduct, V] = query.bind(argmap(params))

		override def rowForm :SQLReadForm[V] = query.rowForm

		protected override def reform[X](second :Query[X, V])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[Args, V], Query[X, V]) =
		{
			val (left, right) = (query reform second)(reform)
			(left compose argmap, right)
		}

		protected override def reform_:[X](first :SingleQuery[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
		{
			val (left, right) = (first `->reform` query)(reform)
			(left, right compose argmap)
		}

		protected override def allowedReforms(implicit spelling :SQLSpelling) :Permissions =
			spelling.allowedReforms(query)

		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Query[Args, V] =
			query.reformed compose argmap

		protected override def alignment(permissions :Permissions)(implicit spelling :SQLSpelling) :Alignment =
			spelling.alignment(query, permissions)

		protected override def potentialColumns(permissions: ReformPermissions.Permissions)
		                                       (implicit spelling: SQLSpelling): AlignableColumns =
			spelling.potentialColumns(query, permissions)

		protected override def potentialColumnsCount(implicit spelling :SQLSpelling) :Int =
			spelling.potentialColumnsCount(query)


		@throws[UndefinedShapeException]("if the query is not anchored.")
		@throws[MisalignedExpressionException]("if the individual select clauses cannot be unified to a common shape.")
		protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(query)

		@throws[UndefinedShapeException]("if the query is not anchored.")
		@throws[MisalignedExpressionException]("if the individual select clauses cannot be unified to a common shape.")
		protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(query)

		protected override def defaultSpelling(context :SQLContext[Args])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[Args] =
			spelling(query)(context.adapted).compose(argmap)

		override def homomorphic(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :ComposedQuery[_, _, _] => query homomorphic other.query
			case _ => false
		}
		override def isomorphic(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :ComposedQuery[_, _, _] => query isomorphic other.query
			case _ => false
		}
		override def equivalent(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :ComposedQuery[_, _, _] if other canEqual this => query equivalent other.query
			case _ => false
		}
		override def identical(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :ComposedQuery[_, _, _] if other canEqual this => query identical other.query
			case _ => false
		}

		override def toString :String = "ComposedQuery(" + query + ")"
	}


	object ComposedQuery {
		type __ = ComposedQuery[_, _, _]
	}


	class ComposedSingleQuery[Args, Y, V](override val argmap :Args => Y, override val query :SingleQuery[Y, V])
		extends ComposedQuery[Args, Y, V](argmap, query) with SingleQuery[Args, V]
	{
		override def transform[X](transformation :SQLTransformation[V, X]) :SingleQuery[Args, X] =
			query.transform(transformation).compose(argmap)

		override def adaptRows[X](conversion :SQLAdaptation[V, X]) :SingleQuery[Args, X] =
			query.adaptRows(conversion).compose(argmap)
//		override def map[X](f :V => X) :SingleQuery[Args, X] = query.map(f).compose(argmap)

		override def compose[X](params :X => Args) :SingleQuery[X, V] =
			new ComposedSingleQuery(params andThen argmap, query)

		protected override def reform[X](second :Query[X, V])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[Args, V], Query[X, V]) =
			super[ComposedQuery].reform(second)(reform)

		protected override def reform_:[X](first :SingleQuery[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
			super[ComposedQuery].reform_:(first)(reform)

		protected override def reform_:[X](first :Select[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
			((first :SingleQuery[X, V]) reform_: this)(reform)

	}


	object ComposedSingleQuery {
		type __ = ComposedSingleQuery[_, _, _]
	}


	private[sql] trait DecoratorQueryTemplate[R, +Q[X] <: QueryTemplate[X, Q]] extends QueryTemplate[R, Q] {
		this :Q[R] =>

		val query :Q[R]

		override type RowMapping[O] = query.RowMapping[O]

		override def mapping[O] :RowMapping[O] = query.mapping
		override def export[O]  :TypedMapping[RowMapping[O]#Subject, O] = query.export

		override def selectCount  :Int = query.selectCount
		override def constituents :Seq[Q[R]] = query.constituents
		override def columns      :Seq[TypedColumn[_, this.type]] = query.columns.withOrigin[this.type]
		override def selectClause :SQLShape[R] = query.selectClause
		override def withClause   :WithClause = query.withClause
		override def rowForm      :SQLReadForm[R] = query.rowForm

		override def isRoughlyConsistent :Boolean = query.isRoughlyConsistent
		override def isReformed :Boolean = query.isReformed

		protected override def allowedReforms(implicit spelling :SQLSpelling) :Permissions =
			spelling.allowedReforms(query)

		protected override def alignment(permissions :Permissions)(implicit spelling :SQLSpelling) :Alignment =
			spelling.alignment(query, permissions)
	}


	trait DecoratorQuery[Args, R]
		extends Query[Args, R] with DecoratorQueryTemplate[R, ({ type Q[X] = Query[Args, X] })#Q]
	{
		override def constituents :Seq[SingleQuery[Args, R]] = query.constituents

		override def transform[X](transformation :SQLTransformation[R, X]) :Query[Args, X] =
			copy(query.transform(transformation))

		override def adaptRows[X](conversion: SQLAdaptation[R, X]): Query[Args, X] =
			copy(query.adaptRows(conversion))

		protected def copy[X](query :Query[Args, X]) :Query[Args, X]

		override def bind(params :Args) :QuerySQL[RowProduct, R] = query.bind(params)

		protected override def reform[X](second :Query[X, R])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[Args, R], Query[X, R]) =
		{
			val (left, right) = (query reform second)(reform)
			(copy(left), right)
		}
		protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[Args, R]) =
		{
			val (left, right) = (first reform_: query)(reform)
			(left, copy(right))
		}

		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Query[Args, R] =
			copy(query.reformed)

		//todo: move it to the template trait once we generalize the code in spelling.
		protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(query)
		protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(query)

		protected override def defaultSpelling(context :SQLContext[Args])
		                                      (implicit spelling :SQLSpelling) :SpelledSQL[Args] =
			spelling(query)(context)

		override def homomorphic(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :DecoratorQuery[_, _] => query homomorphic other.query
			case _ => false
		}
		override def isomorphic(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :DecoratorQuery[_, _] => query isomorphic other.query
			case _ => false
		}
		override def equivalent(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :DecoratorQuery[_, _] if other canEqual this => query equivalent other.query
			case _ => false
		}
		override def identical(that :Query.__) :Boolean = that match {
			case _ if this eq that => true
			case other :DecoratorQuery[_, _] if other canEqual this => query identical other.query
			case _ => false
		}

		override def toString :String = this.innerClassName + "(" + query + ")"
	}


	trait DecoratorSingleQuery[Args, V] extends DecoratorQuery[Args, V] with SingleQuery[Args, V] {
		override val query :SingleQuery[Args, V]

		override def transform[X](transformation :SQLTransformation[V, X]) :SingleQuery[Args, X] =
			copy(query.transform(transformation))

		override def adaptRows[X](conversion :SQLAdaptation[V, X]) :SingleQuery[Args, X] =
			copy(query.adaptRows(conversion))

		protected def copy[X](query :SingleQuery[Args, X]) :SingleQuery[Args, X]
//		protected def copy[X](query :SingleQuery[Args, X]) :SingleQuery[Args, X]
//		override def rowsTo[X](implicit lift :Lift[V, X]) :SingleQuery[Args, X] = copy(query.rowsTo[X])
//		override def map[X](f :V => X) :SingleQuery[Args, X] = copy(query.map(f))
	}


	trait DecoratorMappingQuery[Args, M[O] <: MappingAt[O]]
		extends DecoratorQuery[Args, M[Unit]#Subject] with MappingQuery[Args, M]
	{
		override val query :MappingQuery[Args, M]
		override def bind(params :Args) :MappingQuerySQL[RowProduct, M] = query.bind(params)
	}
}






//todo: a special class or type alias for SQLMapping and, especially, ListingSQLMapping
//todo: is there any benefit at all to having a mapping in a query *statement*?
/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectAs select]] or
  * a [[net.noresttherein.oldsql.sql.ast.CompoundSelectAs set operation]] on them,
  * which provides a [[net.noresttherein.oldsql.schema.Mapping Mapping]] for the returned rows.
  */ //consider: moving it to package sql top scope, just like SelectAs is in ast
trait MappingQuery[P, M[O] <: MappingAt[O]] extends Query[P, M[Unit]#Subject] {
	override type RowMapping[O] = M[O]
	//we don't narrow down the expression type because of SelectIdSQL and possible future features
//		override def selectClause :ComponentLayout[M]

	def union(other :MappingQuery[P, M])     :MappingQuery[P, M] = Select.Union(this, other)
	def unionAll(other :MappingQuery[P, M])  :MappingQuery[P, M] = Select.UnionAll(this, other)
	def minus(other :MappingQuery[P, M])     :MappingQuery[P, M] = Select.Minus(this, other)
	def intersect(other :MappingQuery[P, M]) :MappingQuery[P, M] = Select.Intersect(this, other)

	override def compose[X](params :X => P) :MappingQuery[X, M] = new ComposedMappingQuery(params, this)
	override def bind(params :P) :MappingQuerySQL[RowProduct, M]
}


object MappingQuery {
	type __ = MappingQuery[_, M] forSome { type M[A] <: MappingAt[A] }


	//todo: move it to sql.MappingQuery
	trait SingleMappingQuery[P, M[O] <: MappingAt[O]] extends SingleQuery[P, M[Unit]#Subject] with MappingQuery[P, M] {
		override def compose[X](params :X => P) :SingleMappingQuery[X, M] =
			new ComposedSingleMappingQuery(params, this)
//		override def bind(params :P) :SingleMappingQuerySQL[RowProduct, M] =
	}

	object SingleMappingQuery {
		type __ = SingleMappingQuery[_, M] forSome { type M[A] <: MappingAt[A] }
	}


	class ComposedMappingQuery[Args, Y, M[O] <: MappingAt[O]]
	                          (override val argmap :Args => Y, override val query :MappingQuery[Y, M])
		extends ComposedQuery[Args, Y, M[Unit]#Subject](argmap, query) with MappingQuery[Args, M]
	{
//		override def selectClause :ComponentLayout[M] = query.selectClause
		override def compose[X](params :X => Args) :MappingQuery[X, M] =
			new ComposedMappingQuery(params andThen argmap, query)

		override def bind(params :Args) :MappingQuerySQL[RowProduct, M] = query.bind(argmap(params))
	}


	object ComposedMappingQuery {
		type __ = ComposedMappingQuery[_, _, M] forSome { type M[A] <: MappingAt[A] }
	}


	class ComposedSingleMappingQuery[Args, Y, M[O] <: MappingAt[O]]
	                                (override val argmap :Args => Y, override val query :SingleMappingQuery[Y, M])
		extends ComposedSingleQuery[Args, Y, M[Unit]#Subject](argmap, query) with SingleMappingQuery[Args, M]
	{
		override def compose[X](params :X => Args) :SingleMappingQuery[X, M] =
			new ComposedSingleMappingQuery(params andThen argmap, query)

		override def bind(params :Args) :MappingQuerySQL[RowProduct, M] = query.bind(argmap(params))
	}


	object ComposedSingleMappingQuery {
		type __ = ComposedSingleMappingQuery[_, _, M] forSome { type M[A] <: MappingAt[A] }
	}
}

