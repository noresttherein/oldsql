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
import net.noresttherein.oldsql.sql.Query.{ComposedQuery, ComposedSingleQuery, QueryReformingTemplate, QueryTemplate, SingleQuery, SingleQueryTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{Complete, GroundRow, TopRow}
import net.noresttherein.oldsql.sql.Select.{ArbitrarySelect, SelectOperator, SelectTemplate}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, SQLShape}
import net.noresttherein.oldsql.sql.ast.{ColumnMappingQuery, ColumnQuery, ComponentSQL, CompoundSelectAs, CompoundSelectColumn, CompoundSelectColumnAs, CompoundSelectSQL, HasRowShape, LabeledSQL, MappingQuerySQL, MappingSQL, QuerySQL, RelationSQL, RowShapeCache}
import net.noresttherein.oldsql.sql.ast.LabeledSQL.{LabeledColumnSQL, LabeledValueSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectAs
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.{QueryReform, Reform, SpelledSQL, SQLAdaptation, SQLConversion, SQLScribe, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.mechanics.Reform.ArityValidator

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
trait Query[P, R] extends QueryTemplate[R, ({ type Q[X] = Query[P, X] })#Q] with Serializable {
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

	//semantics of unions and rest:
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
	  */
	def compose[X](params :X => P) :Query[X, R] = new ComposedQuery[X, P, R](params, this)

	/** Replaces all [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in this instance with
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters of values taken from the argument,
	  * turning this parameterized query statement
	  * into a ground [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]].
	  */
	def bind(params :P) :QuerySQL[RowProduct, R]


//	/** Reforms the ''select'' clauses of all constituent ''selects'' in this query and `second`
//	  * to achieve unified column types. The exact details of what operations are allowed depend on `reform`
//	  * implementation. However, this is generally limited to some of:
//	  *   - including additional columns in a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
//	  *     (sub)expression;
//	  *   - excluding additional columns from a `ComponentSQL` (sub)expression;
//	  *   - adding columns for 'null' literals to match columns without counterparts in aligned subexpressions;
//	  *   - reordering columns.
//	  * This is a protected method with a default implementation to which `reform` strategy serves as a facade.
//	  *   1. A single ''select'' (or any other query producing a single/already unified row type) immediately delegates
//	  *      to `reform(this, second)`, a bridge for method [[net.noresttherein.oldsql.sql.Query.reform_: reform_:]]
//	  *      of the argument.
//	  *   1. A [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] splits the algorithm into three parts:
//	  *     1. reforms itself using `reform.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.subreform subreform]];
//	  *     1. unifies the `left` subquery with `second`;
//	  *     1. unifies the `right` subquery with previously reformed `second`;
//	  *     1. unifies the first query again with fully reformed `second`, leaving the latter unchanged.
//	  *
//	  * If the above scheme fails to produce a `CompoundSelect` with unified member ''selects'',
//	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
//	  * is thrown. `Reform` implementation is required to be additive in the sense that modifications done
//	  * to a query `a` when reforming it query `b` cannot be reverted by reforming `a` with another query `c`.
//	  *
//	  * Both `this` and `second` are assumed to be internally unified (be a result of calling
//	  * [[net.noresttherein.oldsql.sql.Query.reformed reformed]] on some `Query`).
//	  * @param second another query of the same value type, combined with this query in a larger `CompoundSelect`.
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform[X](second :Query[X, R])(reform :QueryReform)
//	                       (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R])
//
//	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
//	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
//	  * from `this.`[[net.noresttherein.oldsql.sql.Query.reform reform]] by
//	  * [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]] instances other than
//	  * [[net.noresttherein.oldsql.sql.Select Select]]. It should not be called directly,
//	  * but rather through the `Reform` facade method of the same signature, which by default delegates here,
//	  * but can be also overriden by specialized reforming strategies.
//	  *
//	  * The general algorithm is the same as for `reform_:[X](first :Select[X, V])`,
//	  * except the argument is not normally changed in the process:
//	  * `reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.prohibitReformRight prohibitReformRight]] is used
//	  * to unify `first.`[[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]] with the select clause(s)
//	  * in this query, and the first query in the returned pair is always `first`, unless this case is explicitly
//	  * overridden by a particular `Reform` implementation.
//	  * Both `this` and `first` are assumed to be internally unified (be a result of calling
//	  * [[net.noresttherein.oldsql.sql.Query.reformed reformed]] on some `Query`).
//	  * @param first  a non-compound SQL query combined with this query into some larger `CompoundSelect`
//	  *               (as the first query of the pair).
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
//	                         (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R])
//
//	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
//	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
//	  * from `this.`[[net.noresttherein.oldsql.sql.Query.reform reform]] by
//	  * [[net.noresttherein.oldsql.sql.Select Select]] queries. It should not be called directly, but rather
//	  * through the `Reform` facade method of the same signature, which by default delegates here, but can be also
//	  * overriden by specialized reforming strategies.
//	  *   1. If this query is also a `Select` or is otherwise already unified, its ''select'' clause
//	  *      is unified with the ''select'' clause of the argument using the standard reforming process
//	  *      of [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] and
//	  *      [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]].
//	  *   1. If this query is a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]],
//	  *      the argument is unified first with `this.`[[net.noresttherein.oldsql.sql.CompoundSelect.left left]],
//	  *      then the result is unified with `this.`[[net.noresttherein.oldsql.sql.CompoundSelect.right right]],
//	  *      and finally reformed `left` and `right` queries are unified with each other and combined in a returned
//	  *      `CompoundSelect`.
//	  *
//	  * For this scheme to work, `reform` must be additive in the sense that a query `a`, unified with a query `b`,
//	  * cannot have its changes reverted when unifying it with another query `c`.
//	  * @param first  an SQL ''select'' combined with this query into some larger `CompoundSelect` (as the first query
//	  *               of the pair).
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform_:[X](first :Select[X, R])(reform :QueryReform)
//	                         (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R])
//
//
//	private[sql] def `->reform`[X](other :Query[X, R])(reform :QueryReform)
//	                              (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
//		this.reform(other)(reform)
//
//	private[sql] def `->reform_:`[X](first :SingleQuery[X, R])(reform :QueryReform)
//	                                (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
//		reform_:(first)(reform)
//
//	private[sql] def `->reform_:`[X](first :Select[X, R])(reform :QueryReform)
//	                                (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
//		reform_:(first)(reform)

	protected override def reformed(implicit spelling :SQLSpelling) :Query[P, R] = spelling.queryReform(this)

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






object Query {
	type __ = Query[_, _]

	//todo: if nothing else will come here it's better to remove this trait and just override both reformed and ->reformed
	private[sql] trait QueryReformingTemplate[R, +Q[_]] {
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
		def reformed(implicit spelling :SQLSpelling) :Q[R]

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
		protected def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Q[R]

		private[sql] def `->reformed`(reform :QueryReform)(implicit spelling :SQLSpelling) :Q[R] =
			reformed(reform)
	}


	/** A template for classes representing generalized ''selects'' - both standard ''select'' queries and
	  * compound ''selects'' combining several individual ''selects'' with set operators.
	  * It is inherited by both [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] and
	  * [[net.noresttherein.oldsql.sql.Query Query]].
	  * @tparam R value type representing the whole ''select'' clause, used as its return type and
	  *           [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] value type.
	  * @tparam Q the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *           with its value type `V`.
	  */
	trait QueryTemplate[R, +Q[_]] extends QueryReformingTemplate[R, Q] with HasRowShape {
		/** The nominal type of this query, that is the type
		  * of `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.mapping mapping]] representing the selected
		  * columns and defining the assembly process of final values of `V` from their values.
		  */ //consider: renaming to SelectClause
		type RowMapping[O] <: MappingAt[O]

		@deprecated("this is here temporarily because RowMapping does not preserve the subject type at this time.", "Scala 3")
		def supertyped :Q[R] { type RowMapping[O] <: BaseMapping[R, O] } =
			this.asInstanceOf[Q[R] { type RowMapping[O] <: BaseMapping[R, O] }]

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

		/** Member ''selects'' (or cursors) which constitute this query in order, joined with
		  * [[net.noresttherein.oldsql.sql.Select.SelectOperator operators]] like 'union'.
		  * Any [[net.noresttherein.oldsql.sql.Query.SingleQueryTemplate single query]],
		  * in particular [[net.noresttherein.oldsql.sql.Select.SelectTemplate select]], returns itself.
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate Compound selects]] return concatenation
		  * of constituents of its left and right sides.
		  */
		def constituents :Seq[Q[R]]

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
		  * two [[net.noresttherein.oldsql.sql.ast.LabeledSQL LabeledSQL]] expressions with a different order
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
		def rowsTo[X](implicit conversion :SQLAdaptation[R, X]) :Q[X]// { type RowMapping[O] <: QueryTemplate.this.RowMapping[O] }

		//Transform is not here because SQLTransformation doesn't always translate a column to a column,
		// so SelectColumn cannot return a SelectColumn
//		def transform[X](conversion :SQLTransformation[R, X]) :Q[X]

		//Todo: rename either these or SQLExpression.map as, even if they won't have conflicting erasures due to
		// different return types, will break inference of type arguments. Same for overload with the other Query.map
		def map[X](f :R => X) :Q[X] = rowsTo(SQLAdaptation(".map", f))// { type RowMapping[O] <: QueryTemplate.this.RowMapping[O] }

		def map[Fun, C <: Chain, X](f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C) :Q[X] = // { type RowMapping[O] <: QueryTemplate.this.RowMapping[O] } =
			map(applyFun(f))

		protected def applyFun[Fun, C <: Chain, X]
		                      (f :Fun)(implicit application :ChainApplication[C, Fun, X], isChain :R <:< C) :R => X =
			{ v => application(f, isChain(v)) }


		/** The default strategy for determining the final shape of rows returned by this query.
		  * This is only really important for ''compound selects'', which need to reform the select clauses
		  * of all member ''selects''. Default implementation returns simply
		  * `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]],
		  * but [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]
		  * and [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] delegate to
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]].
		  * or `operator.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]],
		  * if the [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] of argument `spelling`
		  * is [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.TopScope TopScope]].
		  *
		  * This method is used as a fallback; there is no guarantee that the returned reform will be really used,
		  * or it may only be used to change detailed permissions for possible reforming operations
		  * on another `QueryReform`.
		  */
		def defaultReform(implicit spelling :SQLSpelling) :QueryReform = spelling.queryReform
//
//		/** The default reforming strategy for unifying the combined select clause of this query, used
//		  * used for top level queries, that is when this query is the whole spelled expression, and not a subquery
//		  * of another ''compound select''. The default implementation delegates to
//		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]].
//		  */
//		def topReform(implicit spelling :SQLSpelling)     :QueryReform = QueryReform.bottomUp

		//def reformed(reform :Reform)(implicit spelling :SQLSpelling) :Q[V]

		def canEqual(that :Any) :Boolean = that.getClass == getClass
	}



	/** Common declarations of queries which are not ''compound selects''. */
	trait SingleQueryTemplate[R, +Q[_]] extends QueryTemplate[R, Q] { this :Q[R] =>
		override def constituents :Seq[Q[R]] = this::Nil
		override def selectCount = 1
	}



	/** A `Query` which is not a [[net.noresttherein.oldsql.sql.CompoundSelect Compound Select]], that is
	  * cannot be divided into component queries. Most importantly, this includes the basic
	  * [[net.noresttherein.oldsql.sql.Select Select]], but can represent also cursors coming from different sources,
	  * such as returned by stored procedures. Aside from `Select`, these queries' ''select'' clause
	  * typically cannot be reformed.
	  */
	trait SingleQuery[P, R] extends Query[P, R] with SingleQueryTemplate[R, ({ type Q[X] = SingleQuery[P, X] })#Q] {
		override def transform[X](transformation :SQLTransformation[R, X]) :SingleQuery[P, X]

		override def compose[X](params :X => P) :SingleQuery[X, R] =
			new ComposedSingleQuery(params, this)

//		override def bind(params :P) :SingleQuerySQL[RowProduct, V]

/*
		protected override def reform[X](second :Query[X, R])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
			reform.prohibitReformLeft(this, second) //calls this reform_: second

		protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		{
			val validator = reform.prohibitAll //prohibitReformLeft was called, if needed by first.reform above
			validator.fallback(first.selectClause, selectClause)
			(first, this)
		}

		protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		{
			val reformFirst = reform.prohibitReformRight
			val reformed = reformFirst(first.selectClause :SQLExpression[first.From, Grouped, R], selectClause :SQLExpression[_, _, R])._1
			if (reformed eq first.selectClause)
				(first, this)
			else
				(first.selectOther(reformed), this)
		}
*/
		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :SingleQuery[P, R] = this
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

		override def transform[X](transformation :SQLTransformation[V, X]) :Query[Args, X] =
			query.transform(transformation).compose(argmap)

		override def rowsTo[X](implicit conversion :SQLAdaptation[V, X]) :Query[Args, X] =
			query.rowsTo[X].compose(argmap)

//		override def map[X](f :V => X) :Query[Args, X] = query.map(f).compose(argmap)

		override def compose[X](params :X => Args) :Query[X, V] =
			new ComposedQuery(params andThen argmap, query)

		override def bind(params :Args) :QuerySQL[RowProduct, V] = query.bind(argmap(params))

		override def rowForm :SQLReadForm[V] = query.rowForm

/*
		protected override def reform[X](second :Query[X, V])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[Args, V], Query[X, V]) =
		{
			val (left, right) = reform(query, second)
			(left compose argmap, right)
		}

		protected override def reform_:[X](first :SingleQuery[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
		{
			val (left, right) = reform(first, query)
			(left, right compose argmap)
		}

		protected override def reform_:[X](first :Select[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
		{
			val (left, right) = reform(first, query)
			(left, right compose argmap)
		}
*/

		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Query[Args, V] =
			reform(query) compose argmap


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
		override def rowsTo[X](implicit conversion :SQLAdaptation[V, X]) :SingleQuery[Args, X] =
			query.rowsTo[X].compose(argmap)
//		override def map[X](f :V => X) :SingleQuery[Args, X] = query.map(f).compose(argmap)

		override def compose[X](params :X => Args) :SingleQuery[X, V] =
			new ComposedSingleQuery(params andThen argmap, query)
	}


	object ComposedSingleQuery {
		type __ = ComposedSingleQuery[_, _, _]
	}



	trait DecoratorQuery[Args, V] extends Query[Args, V] {
		val query :Query[Args, V]

		override type RowMapping[O] = query.RowMapping[O]

		override def mapping[O] :RowMapping[O] = query.mapping
		override def export[O] :TypedMapping[RowMapping[O]#Subject, O] = query.export

		override def selectCount  :Int = query.selectCount
		override def constituents :Seq[SingleQuery[Args, V]] = query.constituents
		override def columns      :Seq[TypedColumn[_, this.type]] = query.columns.withOrigin[this.type]
		override def selectClause :SQLShape[V] = query.selectClause
		override def withClause   :WithClause = query.withClause
		override def rowForm      :SQLReadForm[V] = query.rowForm

		def copy(query :Query[Args, V]) :Query[Args, V]

		override def bind(params :Args) :QuerySQL[RowProduct, V] = query.bind(params)

/*
		protected override def reform[X](second :Query[X, V])(reform :QueryReform)
		                                (implicit spelling :SQLSpelling) :(Query[Args, V], Query[X, V]) =
		{
			val (left, right) = reform(query, second)
			(copy(left), right)
		}
		protected override def reform_:[X](first :SingleQuery[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
		{
			val (left, right) = reform(first, query)
			(left, copy(right))
		}
		protected override def reform_:[X](first :Select[X, V])(reform :QueryReform)
		                                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Args, V]) =
		{
			val (left, right) = reform(first, query)
			(left, copy(right))
		}
*/
		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Query[Args, V] =
			copy(reform(query))


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






/**
  * @author Marcin Mościcki
  */
trait Select[P, R] extends SingleQuery[P, R] with SelectTemplate[R, ({ type Q[X] = Select[P, X] })#Q] {

	/** The from clause of this select. It is in its [[net.noresttherein.oldsql.sql.RowProduct!.Complete Complete]] form:
	  * all clause type constructors are in their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]
	  * form, and all relations are listed. This assures that
	  * [[net.noresttherein.oldsql.sql.Select.SelectTemplate.from from]]`.Complete <:< Form` and `Form <:< form.Complete`.
	  * As Scala does not allow recursive type aliases, `Complete` is instead bound from both sides by `From`.
	  */
	override type From <: TopRow { type Complete <: Select.this.From; type Params = P }

	override def rowForm :SQLReadForm[R] = selectClause.selectForm

//	override def constituents :Seq[Select[P, V]] = ReversedList :+ this
	override def transform[X](transformation :SQLTransformation[R, X]) :Select[P, X] =
		selectOther(transformation(selectClause))

	override def rowsTo[X](implicit conversion :SQLAdaptation[R, X]) :Select[P, X] =
		if (conversion.isIdentity) this.castParam2[X]
		else selectOther(selectClause.to[X])
		//ArbitrarySelect[P, From, X](from, selectClause.to[X], isDistinct)

//	override def map[X](f :R => X) :Select[P, X] =
//		selectOther(selectClause.map(f))
//		ArbitrarySelect[P, From, X](from, selectClause.map(f), isDistinct)

	//order by will be problematic here
	def selectOther[X](selectClause :SQLExpression[From, Grouped, X]) :Select[P, X] =
		if (isDistinct)
			selectClause.paramSelectFrom[P, From](from).distinct
		else
			selectClause paramSelectFrom from

	override def bind(params :P) :TopSelectSQL[R]

	//todo: orderBy - should it be a function of the selectClause, or From?

	//todo: take/drop/top/limit


/*
	protected override def reform[X](second :Query[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
		reform(this, second)

	protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
		val reformRight = reform.prohibitReformLeft
		val reformed = reformRight(first.selectClause, selectClause)._2
		if (reformed eq selectClause)
			(first, this)
		else
			(first, selectOther(reformed))
	}

	protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
		val (left, right) = reform(first.selectClause, selectClause)
		val leftSelect =
			if (left eq first.selectClause) first
			else first.selectOther(left)
		val rightSelect =
			if (right eq selectClause) this
			else selectOther(right
		(leftSelect, rightSelect)
	}
*/

	protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :Select[P, R] =
		reform.default(this)


	protected override def defaultSpelling(context :SQLContext[P])(implicit spelling :SQLSpelling) :SpelledSQL[P] = {
		def completeParams(from :RowProduct) :Parameterization[from.Params, from.Complete] = from.parameterization
		defaultSpelling(from, context)(completeParams(from), from.parameterization)
	}


	override def homomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause homomorphic s.selectClause) && (from homomorphic s.from)
		case _ => false
	}
	override def isomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause isomorphic s.selectClause) && (from isomorphic s.from)
		case _ => false
	}
	override def equivalent(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) => //or from equivalent s.from?
			isDistinct == s.isDistinct && (selectClause equivalent s.selectClause) && (from isomorphic s.from)
		case _ => false
	}
	override def identical(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case s :Select[_, _] if this.canEqual(that) && s.canEqual(this) =>
			isDistinct == s.isDistinct && (selectClause identical s.selectClause) && (from identical s.from)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case s :AnyRef if s eq this => true
		case s :Select[_, _] if s canEqual this=>
			isDistinct == s.isDistinct && s.selectClause == selectClause && s.from == from
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Select[_, _]] //that.getClass == getClass
	override def hashCode :Int = (selectClause.hashCode * 31 + from.hashCode) * 31 + isDistinct.hashCode

}




/** A lower level factory of SQL ''select'' statements and expressions. It is used mostly internally,
  * by the [[net.noresttherein.oldsql.sql.SQLExpression.selectFrom selectFrom]] family of methods
  * of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  * and by the [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class which governs
  * what type of SQL ''select'' can be created based on its [[net.noresttherein.oldsql.sql.RowProduct from]]
  * and ''select'' clause.
  *
  * Applications wishing to create a query should first have a look at [[net.noresttherein.oldsql.sql.From From]],
  * a factory of single table [[net.noresttherein.oldsql.sql.FromClause from clauses]], which serves as the starting
  * point of the SQL DSL. Through it, an SQL ''select'' is built in a more natural, reversed fashion,
  * in which its ''from'' clause (including a ''where'' clause, and, optionally
  * a [[net.noresttherein.oldsql.sql.GroupByClause group by]] clause) is created first, and the ''select'' clause,
  * that is an `SQLExpression` based on that ''from'' clause, is provided last,
  * to one of the [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]]
  * extension methods of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]:
  * {{{
  *     From(AnimalCompanions) join Rangers on (_.id) === (_.familiarId) where {
  *         row => row.prev.species ==? "Hamster" && row.prev.name ==? "Boo"
  *     } select _.last.name
  * }}}
  *
  * This object can be however useful in more generic code, where an `SQLExpression` for the ''select'' clause
  * and its domain `RowProduct` are built separately:
  * {{{
  *     Select(selectClause) from fromClause
  *     Select(Dragons.joined) from From(Dragons)
  * }}}
  *
  * Two separate kinds of abstract SQL ''select'' representations exist:
  *   1. [[net.noresttherein.oldsql.sql.Select Select]], the companion class to this object,
  *      is an (optionally) parameterized statement, which,
  *      through [[net.noresttherein.oldsql.sql.Select.spell spelling]],
  *      can be [[net.noresttherein.oldsql.sql.Select.chant converted]]
  *      to an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]].
  *      These serve as stand-alone queries, or parts of a bigger
  *      [[net.noresttherein.oldsql.sql.Query.CompoundSelect CompoundSelect]] forming the final query.
  *   1. [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]
  *      is an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]], which can be freely used
  *      within larger SQL expressions, for example as an argument to ''exists''. They are distinguished
  *      from the former in that they can represent ''dependent selects''
  *      ([[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL subselects]]) and capture the namespace of their
  *      outer ''select'' in their ''from'' clause, allowing them to use tables and columns from the outer ''select''
  *      in addition to their own. They can be parameterized by [[net.noresttherein.oldsql.sql.ParamClause unbound]]
  *      parameters only in the outer `RowProduct` - their actual ''from'' clause type becomes lost.
  *
  * The gap between the two is bridged by [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]],
  * which extends both types and represent a ground ''select'' - one without any free variables, that is
  * depending on neither unbound parameters nor any external tables.
  */
object Select {
	//todo: mapping indexed headers

/*
	@inline def apply[E <: SQLExpression[F, Grouped, _], F <: RowProduct]
	                 (expr :E)(implicit result :CanSelect[F, E]) :SelectFactory[E, F] =
		new SelectFactory(expr)

	class SelectFactory[E <: SQLExpression[F, Grouped, _], F <: RowProduct] private[Select]
	                  (private val selectClause :E)
		extends AnyVal
	{
		@inline final def from(clause :F)(implicit result :CanSelect[F, E]) :result.Select =
			result(clause, selectClause)
	}

	@inline def apply(all: *) :SelectAllFactory = new SelectAllFactory {}

	sealed trait SelectAllFactory extends Any {
		@inline final def from[F <: RowProduct]
		                  (clause :F)
		                  (implicit result :CanSelect[F, ChainTuple[clause.Generalized, GlobalScope, clause.Row]])
				:result.Select =
			result(clause, clause.row)
	}
*/

	@inline def apply(from :TopRow) :SelectCurriedFactory[from.Params, from.Complete] =
		new SelectCurriedFactory[from.Params, from.Complete](from.self)

	class SelectCurriedFactory[P, F <: TopRow { type Complete <: F ; type Params = P }] private[Select] (private val from :F)
		extends AnyVal
	{
		//todo: document that not all expressions are accepted!
		def apply[V](selectClause :SQLExpression[F, Grouped, V]) :Select[P, V] =
			ArbitrarySelect[P, F, V](from, selectClause.anchor(from))

//		def apply[V](selectClause :ColumnSQL[from.Self, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//		def apply[V <: Chain](selectClause :SQLExpression[from.Generalized, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		def apply[V](selectClause :InlineSQL[from.Generalized, Grouped, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		//an unused type parameter due to an overloading resolution bug in scala 2
//		def apply[X, Y, *](selectClause :ConversionSQL[from.Generalized, Grouped, X, Y]) :Select[from.Params, Y] =
//			new ArbitrarySelect[from.Params, from.Self, Y](from.self, selectClause.anchor(from.self))
//
//		def apply[M[A] <: BaseMapping[V, A], V]
//		         (selectClause :EditedLooseComponent[from.Generalized, M, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)
//
//		def apply[M[A] <: BaseMapping[V, A], V]
//		         (selectClause :EditedComponentSQL[from.Generalized, M, V]) :Select[from.Params, V] =
//			new ArbitrarySelect[from.Params, from.Self, from.Generalized, V](
//				from.self, selectClause.anchor(from.self), from.parameterization
//			)

		def apply[M[O] <: BaseMapping[V, O], V](selectClause :ComponentSQL[F, M]) :SelectMapping[P, M] =
			new SelectComponent[P, F, M, V](from, selectClause.anchor(from))

		def apply[V <: Listing](selectClause :LabeledSQL[F, Grouped, V])
				:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
			SelectListing[P, F, V](from, selectClause.anchor(from))

		def apply[A <: Label, V](selectClause :LabeledColumnSQL[F, Grouped, A, V])
				:SelectMapping[P, IndexedMapping.of[V]#Column] =
			SelectListingColumn[P, F, A, V](from, selectClause.anchor(from))
	}




	/** An SQL binary operator which can be used to create a query out of two SQL ''selects'', by combining their
	  * result sets. The most notable instance is [[net.noresttherein.oldsql.sql.Select.Union Union]], but other
	  * predefined operators provided by some database engines have also their definition in the enclosing
	  * [[net.noresttherein.oldsql.sql.Select Select]] object.
	  */
	abstract class SelectOperator(val name :String) {
		val NAME :String = name.toUpperCase

		def apply[P, V](left :Query[P, V], right :Query[P, V]) :CompoundSelect[P, V] =
			CompoundSelect(left, this, right)

		def apply[P, M[O] <: MappingAt[O]]
		         (left :MappingQuery[P, M], right :MappingQuery[P, M]) :CompoundSelectMapping[P, M] =
			CompoundSelectMapping(left, this, right)


		def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V]) :CompoundSelectSQL[F, V] =
			CompoundSelectSQL(left, this, right)

		def apply[F <: RowProduct, V](left :ColumnQuery[F, V], right :ColumnQuery[F, V]) :CompoundSelectColumn[F, V] =
			CompoundSelectColumn(left, this, right)

		def apply[F <: RowProduct, M[O] <: MappingAt[O]]
		         (left :MappingQuerySQL[F, M], right :MappingQuerySQL[F, M]) :CompoundSelectAs[F, M] =
			CompoundSelectAs(left, this, right)

		def apply[F <: RowProduct, M[O] <: BaseColumn[V, O], V]
		         (left :ColumnMappingQuery[F, M, V], right :ColumnMappingQuery[F, M, V]) :CompoundSelectColumnAs[F, M, V] =
			CompoundSelectColumnAs(left, this, right)

		/** Default reform strategy used to unify ''select'' clauses of the left and right side
		  * of a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
		  * joined with this operator. This strategy is used if the reformed ''compound select'' is actually
		  * a subterm of a larger ''compound select''. This reform is used when the strategy for the whole query
		  * is implemented in a 'bottom-up' manner: the left and right sides of a ''compound select''
		  * are reformed independently before being unified with each other. 'Top-down' reforming strategies
		  * do not rely on this property, instead returning an instance of their own type.
		  *
		  * It is the default implementation of
		  * `CompoundSelect.`[[net.noresttherein.oldsql.sql.CompoundSelect.defaultReform defaultReform]]
		  * and `CompoundSelectSQL.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.defaultReform defaultReform]],
		  * and those methods should be used instead.
		  * @see [[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform]]
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform]]
		  */ //consider: adding arguments like in topReform. Inconvenient because it is overriden by vals
		def defaultReform :QueryReform

		/** Default reform strategy used to unify ''select'' clauses of two queries joined with this operator
		  * in a ''compound select'' representing the whole formatted statement/expression.
		  * While the default implementation simply returns
		  * [[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]],
		  * this distinction is created with ''union all'' ''compound selects'', which add a synthetic discriminator
		  * column specifying from which of the member ''selects'' any given row comes from, which should not
		  * be duplicated. Additionally, reforming of a top-level
		  * [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]] is guaranteed to always return a single
		  * column query; this might not be the case if such a query is unified with ''selects'' for multiple columns.
		  *
		  * It is the default implementation of
		  * `CompoundSelect.`[[net.noresttherein.oldsql.sql.CompoundSelect.topReform topReform]]
		  * and `CompoundSelectSQL.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.topReform topReform]],
		  * and those methods should be used instead.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform]]
		  */
		def topReform :QueryReform = defaultReform

		/** Alters the argument `reform` by [[net.noresttherein.oldsql.sql.mechanics.Reform.prohibit prohibiting]]
		  * certain reforming actions. This varies between operators, following the principle of least surprise.
		  */
		def prevent(reform :QueryReform) :QueryReform

		override def toString :String = name
	}


	//consider: We need to know from which select each row comes in order to use a proper form for reading.
	//  However, we can't use discriminators as in UnionAll, because it obviously will no longer be a union.
	//  An exception to the rule would be if we added null columns, as a row with null column will equal no other row.
	//  Of course, we might opt to use an 'application null' in place of nulls in order to enforce union semantics.
	//  Otherwise, the only option we have is to do a UnionAll and then group by the row key
	//  (all columns unless we can use determine that each relation in its from clause has a primary key),
	//  and use min(discriminator) (or whatever) to select a single discriminator for equal rows.
	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). It follows the semantics of set union,
	  * with no duplicate rows in the returned result set.
	  */ //consider: we could add a discriminator, do union all, group by (not supported by clobs/blobs)
	object Union extends SelectOperator("union") { //can we just go with the assembler of the first query?
		//fixme: discriminator? equal null values?
		override val defaultReform :QueryReform = QueryReform.bottomUp(
			true, true, true, false)(
			true, true, true, false
		)

		override def prevent(reform :QueryReform) :QueryReform =
			reform.prohibit(mayAddNullLeft = false, mayAddNullRight = false)
	}
//	final val Union = new SelectOperator("union")

	//  1. mapping vs mapping:
	//      1. nominal mappings are identical (and effective mappings are homomorphic) - use the first mapping for all rows
	//          1. if a component is default, add missing columns, retaining those excluded from the other;
	//          1. otherwise add a null column for every extra column in the other mapping
	//      1. mappings are homomorphic
	//          1. a mapping is default, add missing columns, but retain those excluded from the other
	//          1. otherwise, add null columns
	//      1. mappings share a column extract subset
	//          - add a discriminator column, if not already present
	//          - for every missing column on either side, add the local counterpart,
	//            if it exists and the mapping is default; otherwise add a null column
	//      1.
	/** A union operator combining two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). This is a multiset variant, with every row
	  * from either of the ''selects'' mapping to a single row in the returned result set; if a row is present
	  * in both ''selects'' (or it has duplicates within either of them), each occurrence will be represented by
	  * a separate row in the result.
	  */ //with ... as select1, ... as select2 select
	object UnionAll extends SelectOperator("union all") {
		override val defaultReform :QueryReform = QueryReform.bottomUp

		override val topReform :QueryReform = UnionAllReform.bottomUp

		override def prevent(reform :QueryReform) :QueryReform = reform
	}

	//always use the header of the first one; standard ReformDefaults.reform(_, _)(false, true) (for all selects if the second operand is a compound select)
	/** An operator implementing set difference between two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return every row
	  * from the first (left) argument which is not present in the second (right) argument.
	  */
	object Minus extends SelectOperator("minus") {
		override val defaultReform :QueryReform = QueryReform.bottomUp(false, true)

		override def prevent(reform :QueryReform) :QueryReform =
			reform.prohibitReformLeft
	}

	//Same reform as in SQLExpression
	/** An operator implementing set intersection of two SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]]
	  * (or other [[net.noresttherein.oldsql.sql.ast.QuerySQL queries]]). The query created will return all rows
	  * which are present in both of its arguments, with every row occurring exactly once, regardless of the number
	  * of duplicates in the input ''selects''.
	  */
	object Intersect extends SelectOperator("intersect") {
		override val defaultReform :QueryReform = QueryReform.bottomUp(
			mayExcludeLeft = true, mayIncludeLeft = true, mayReorderLeft = true, mayAddNullLeft = false
		)(
			mayExcludeRight = true, mayIncludeRight = true, mayReorderRight = true, mayAddNullRight = false
		)
		override def prevent(reform :QueryReform) :QueryReform =
			reform.prohibit(mayAddNullLeft = false, mayAddNullRight = false)
	}




	/** A template for classes representing SQL ''selects'' - both standard SQL
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL expressions]] and
	  * [[net.noresttherein.oldsql.sql.Select parameterized]] ''selects'', including their compound forms.
	  * @tparam V value type representing the whole ''select'' clause, used as its return type and
	  *           [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] value type.
	  * @tparam Q the self type of this interface, that is the whole public type of the ''select'' parameterized
	  *           with its value type `V`.
	  */
	trait SelectTemplate[V, +Q[_]] extends SingleQueryTemplate[V, Q] { this :Q[V] =>
//		type Domain >: From <: RowProduct
		/** The from clause of this select.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct]]
		  */
		type From <: RowProduct //{ type Generalized = Domain }

		/** A synthetic mapping type for [[net.noresttherein.oldsql.sql.Select.SelectTemplate.columns columns]]
		  * of this ''select''. These columns are ''not'' necessarily the columns of
		  * `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.mapping mapping]]
		  * or `this.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.export export]],
		  * but rather wrappers over [[net.noresttherein.oldsql.sql.ColumnSQL expressions]] for individual
		  * columns in `this.`[[net.noresttherein.oldsql.sql.Select.SelectTemplate.selectClause selectClause]],
		  * as would be returned by `selectClause.`[[net.noresttherein.oldsql.sql.SQLExpression.split split]].
		  */
		type SelectedColumn[X] = TypedColumnSQLMapping[From, Grouped, X, this.type]

		/** The ''from'' and ''where'' clause of this SQL ''select''
		  * (optionally including also ''group by'' and ''having'' clauses).
		  */
		val from :From

		/** An expression serving as the ''select'' clause of this SQL ''select''.
		  * If it is not a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]], all its individual columns are inlined
		  * when formatting the final SQL statement. The expressions for those individual columns are made available
		  * as [[net.noresttherein.oldsql.sql.TypedColumnSQLMapping.expr expr]] property of the columns listed by
		  * `this.`[[net.noresttherein.oldsql.sql.Select.SelectTemplate.columns columns]].
		  */
		override val selectClause :SQLExpression[From, Grouped, V]
		override def columns :Seq[SelectedColumn[_]]

		//consider: allowing select clauses with other value types
//		@deprecated("this method cannot be implemented properly in SelectColumn", "0.0")
//		def selectOther(selectClause :SQLExpression[From, Grouped, V]) :Q[V]

//		override def withClause :WithClause = selectClause.outerWithClause ++ from.withClause
		//caution: in group by queries this returns the elements of the group by clause, not the actual from clause
		def relations :Seq[RelationSQL.from[from.Generalized]#__] = from.tableStack.reverse //todo: remove the explicit type in Scala 3
		def tables    :Seq[Relation.__] =
			(from.fromClause.tableStack :Iterable[RelationSQL.__]).mapReverse(_.relation :Relation[MappingAt]).toSeq

		def isDistinct :Boolean //consider: making this and others return the same From type
		def distinct   :Q[V]// { type RowMapping[O] <: SelectTemplate.this.RowMapping[O] }
		//todo: order by, top, limit

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.inSelect.shape(selectClause)

		protected override def columnCount(implicit spelling :SQLSpelling) :Int =
			spelling.inSelect.columnCount(selectClause)

		/** A lower-level variant of standard [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]
		  * method of [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
		  * and [[net.noresttherein.oldsql.sql.Query Query]] expressed in terms of
		  * the ''from'' clause of this select, rather than the base clause of this expression (an outer clause of `from`),
		  * with separate parameter getters for the ''from''/''where'' and ''select'' clauses of this instance.
		  * It is used in implementation of the former by
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]],
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]],
		  * and [[net.noresttherein.oldsql.sql.Select Select]].
		  */
		protected def defaultSpelling[P](from :From, context :SQLContext[P])
		                                (selectParams :Parameterization[P, From], fromParams :Parameterization[P, from.Self])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val withClause = if (spelling.scope == TopScope) this.withClause else localWithClause
			val withSQL =
				if (withClause.isEmpty) SpelledSQL(context)
				else spelling.inWith(withClause)(context, Parameterization.paramless) + " "
			val fromSQL = from.spell(withSQL.context, fromParams)
			val selectSpelling = spelling.inSelect
			val selectSetter =
				if (selectSpelling.sqlParamCount(selectClause) == 0) SQLWriteForm.empty[P]
				else selectSpelling(selectClause)(from, fromSQL.context, selectParams).setter
			val selectSQL = this.columns.scanLeft(SpelledSQL(fromSQL.context)) {
				(prev, sqlColumn) => selectSpelling.select(sqlColumn)(from, prev.context, selectParams)
			}.tail.reduce(_ + ", " + _)
			val setter = selectSetter + fromSQL.setter
			val resultContext = selectSQL.outContext(context) //allow selectSQL.context to pass back information
			if (fromSQL.sql.isEmpty)
				withSQL + SpelledSQL(spelling.SELECT_ + selectSQL.sql, setter, resultContext)
			else
				withSQL + SpelledSQL(spelling.SELECT_ + selectSQL.sql + " " + fromSQL.sql, setter, resultContext)
		}

		override def toString :String =
			if (isDistinct) s"SELECT DISTINCT $selectClause FROM $from"
			else  s"SELECT $selectClause FROM $from"
	}



	type __ = Select[_, _]

//	type TypedSelect[P, M[O] <: MappingAt[O], V] = Select[P, V] { type RowMapping[O] <: M[O] }
//	type SelectMapping[P, M[O] <: MappingAt[O]] = Select[P, M[Unit]#Subject] { type RowMapping[O] <: M[O] }

	//consider: moving it out, as is SelectAs
	/** A parameterized ''select'' interface exposing the mapping type `H` used for the ''select'' clause. */
	trait SelectMapping[P, H[A] <: MappingAt[A]] extends Select[P, H[Unit]#Subject] with SingleMappingQuery[P, H] {
		override type RowMapping[O] = H[O]

		override def distinct :SelectMapping[P, H]

		override def bind(params :P) :TopSelectAs[H]

//		def selectOther(selectClause :MappingSQL[From, Grouped, H, H[Unit]#Subject])
	}


	object SelectMapping {
		type __ = SelectMapping[_, M] forSome { type M[A] <: MappingAt[A] }
	}




	private class SelectComponent[P, F <: TopRow { type Complete <: F ; type Params = P }, M[A] <: BaseMapping[V, A], V]
	                             (override val from :F, override val selectClause :ComponentSQL[F, M],
	                              override val isDistinct :Boolean = false)
		extends SelectMapping[P, M]
	{
		override type From = F
//		override type RowMapping[O] = M[O]

		override def mapping[O] :RowMapping[O] = selectClause.mapping.withOrigin[O]
		override def export[O] :TypedMapping[V, O] = selectClause.anchored.withOrigin[O]

		override val withClause = from.withClause ++ selectClause.outerWithClause

		override val columns: Seq[TypedColumnSQLMapping[F, Grouped, _, this.type]] =
			selectClause.export.selectedByDefault.toSeq.map(include(_))

		private def include[X](column :TypedColumn[X, selectClause.Origin]) :SelectedColumn[X] =
			TypedColumnSQLMapping(selectClause \ column)

		override def distinct :SelectMapping[P, M] =
			if (isDistinct) this
			else new SelectComponent[P, F, M, V](from, selectClause, true)

		override def bind(params :P) :TopSelectAs[M] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header.asInstanceOf[ComponentSQL[Complete[GroundRow], M]] topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}




	/** A select expression based on the given row source and selecting an arbitrary expression `header` in its ''select''
	  * clause. This header will be translated by recursively flat mapping the header expression to obtain a flat sequence
	  * of columns.
	  */
	private[sql] abstract
	class BaseArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V] protected
	                         (override val from :F, protected val result :TypedSQLMapping[F, Grouped, V, Unit])
		extends Select[P, V]
	{
		def this(from :F, header :SQLExpression[F, Grouped, V]) =
			this(from, TypedSQLMapping[F, Grouped, V, Unit](header, SelectView))

//		override type Domain = G
		override type From = F

		override val selectClause = result.expr
		override val withClause   = from.withClause ++ selectClause.outerWithClause
		override val columns :Seq[TypedColumnSQLMapping[F, Grouped, _, this.type]] =
			result.columns.toSeq.withOrigin[this.type]

		override def bind(params :P) :TopSelectSQL[V] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] trait ArbitrarySelectTemplate[P, F <: TopRow { type Complete <: F ; type Params = P },
	                                           M[O] <: BaseMapping[V, O], V]
		extends Select[P, V]
	{ this :BaseArbitrarySelect[P, F, V] =>
		override type RowMapping[O] = M[O]

		protected val result :M[Unit]
		override def mapping[O] :M[O] = (this :ArbitrarySelectTemplate[P, F, M, V]).result.withOrigin[O]
		override def export[O]  :M[O] = mapping[O]
//		override def export[O] :M[O] = mapping[O]
	}




	private[sql] def ArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V]
	                                (from :F, selectClause :SQLExpression[F, Grouped, V], isDistinct :Boolean = false)
			:Select[P, V] =
		new ArbitrarySelect[P, F, V](from, TypedSQLMapping[F, Grouped, V, Unit](selectClause, SelectView), isDistinct)

	private[sql] class ArbitrarySelect[P, F <: TopRow { type Complete <: F ; type Params = P }, V]
	                   (override val from :F, protected override val result :TypedSQLMapping[F, Grouped, V, Unit],
	                    override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, TypedSQLMapping.c[F]#c[Grouped]#c[V]#project, V]
	{
		override def distinct :Select[P, V] =
			if (isDistinct) this else new ArbitrarySelect[P, F, V](from, result, true)
	}


	private[sql] def SelectListing[P, F <: TopRow { type Complete <: F ; type Params = P }, V <: Listing]
	                              (from :F, selectClause :LabeledValueSQL[F, Grouped, V], isDistinct :Boolean = false)
			:SelectMapping[P, IndexedMapping.of[V]#Mapping] =
		new SelectListing[P, F, V](from, selectClause.mapping[Unit], isDistinct)

	private[sql] class SelectListing[P, F <: TopRow { type Complete <: F ; type Params = P }, V <: Listing]
	                  (override val from :F, override val result :TypedListingSQLMapping[F, Grouped, V, Unit],
	                   override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
			with ArbitrarySelectTemplate[P, F, IndexedMapping.of[V]#Mapping, V]
			with SelectMapping[P, IndexedMapping.of[V]#Mapping]
	{
		override val selectClause = result.expr

		override def distinct :SelectMapping[P, IndexedMapping.of[V]#Mapping] =
			if (isDistinct) this else new SelectListing[P, F, V](from, result, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.of[V]#Mapping] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[LabeledSQL[Complete[GroundRow], Grouped, V]]
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}


	private[sql] def SelectListingColumn[P, F <: TopRow { type Complete <: F ; type Params = P }, A <: Label, V]
	                 (from :F, selectClause :LabeledColumnSQL[F, Grouped, A, V], isDistinct :Boolean = false)
			:SelectMapping[P, IndexedMapping.of[V]#Column] =
		new SelectListingColumn[P, F, A, V](from, selectClause.mapping, isDistinct)

	private[sql] class SelectListingColumn[P, F <: TopRow { type Complete <: F ; type Params = P }, A <: Label, V]
	                   (override val from :F, override val result :TypedListingColumnSQLMapping[F, Grouped, A, V, Unit],
	                    override val isDistinct :Boolean)
		extends BaseArbitrarySelect[P, F, V](from, result)
		   with ArbitrarySelectTemplate[P, F, IndexedMapping.of[V]#Column, V]
		   with SelectMapping[P, IndexedMapping.of[V]#Column]
	{
		override val selectClause = result.expr :ColumnSQL[result.Domain, result.Scope, V]

		override def distinct :SelectMapping[P, IndexedMapping.of[V]#Column] =
			if (isDistinct) this
			else new SelectListingColumn[P, F, A, V](from, result, true)

		override def bind(params :P) :TopSelectAs[IndexedMapping.of[V]#Column] = {
			val paramless = from.bind(params).asInstanceOf[Complete[GroundRow]]
			val header = SQLScribe.applyParams(from, paramless)(params)(selectClause)
				.asInstanceOf[LabeledColumnSQL[Complete[GroundRow], Grouped, A, V]]
			val select = header topSelectFrom paramless
			if (isDistinct) select.distinct else select
		}
	}

}









/** Implements a set operation combining the result sets of two
  * [[net.noresttherein.oldsql.sql.Select parameterized selects]]
  * (or other [[net.noresttherein.oldsql.sql.Query queries]]). The kind of operation is defined by
  * the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.CompoundSelect.operator operator]] member property.
  * The row schemas of both arguments must match or an exception will be thrown when this expression
  * is converted into an executable SQL statement. If the schema of any of the member ''selects'' is flexible
  * (it is defined by a mapping with [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Optional]] columns),
  * the schema of the first member is used for both of the arguments.
  */
trait CompoundSelect[P, R]
	extends Query[P, R]
	   with QueryReformingTemplate[R, ({ type Q[X] = CompoundSelect[P, X] })#Q]
	   with CompoundSelectTemplate[R, ({ type Q[X] = Query[P, X] })#Q]
{
	override def constituents :Seq[SingleQuery[P, R]] = left.constituents ++: right.constituents

	override def selectClause :SQLShape[R] =
		QueryReform(ArityValidator)(this)(StandardSQL.spelling).selectClause
//
//	protected def construct[X](left :Query[P, X], operator :SelectOperator, right :Query[P, X]) :CompoundSelect[P, X] =
//		CompoundSelect(left, operator, right)

	override def rowsTo[X](implicit conversion :SQLAdaptation[R, X]) :CompoundSelect[P, X] =
		if (conversion.isIdentity) this.castFrom[CompoundSelect[P, R], CompoundSelect[P, X]]
		else CompoundSelect(left.rowsTo[X], operator, right.rowsTo[X])

	override def transform[X](transformation :SQLTransformation[R, X]) :CompoundSelect[P, X] =
		operator(left.transform(transformation), right.transform(transformation))

/*
	protected override def reform[X](second :Query[X, R])(reform :QueryReform)
	                                (implicit spelling :SQLSpelling) :(Query[P, R], Query[X, R]) =
	{
		val (leftWithSecond, secondWithLeft)   = reform(left, second)
		val (rightWithSecond, secondWithRight) = reform(right, secondWithLeft)
		val finalLeft = reform.prohibitReformRight(leftWithSecond, secondWithRight)._1
		val reformedThis =
			if ((finalLeft eq left) & (rightWithSecond eq right)) this
			else reform.reformed(finalLeft, operator, rightWithSecond)
		(reformedThis, secondWithRight)
	}

	protected override def reform_:[X](first :SingleQuery[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
	{
		val (firstWithLeft, leftWithFirst)   = reform(first :Query[X, R], left)
		val (firstWithRight, rightWithFirst) = reform(firstWithLeft :Query[X, R], right)
		//this is better than reformedThis = leftWithFirst.reform(rightWithFirst)(operator),
		// as it guarantees that, in case of sql terms, regardless of the direction in which the forms are passed
		// in all reform methods, they all share the same form as firstWithRight
		val finalLeft = reform.prohibitReformLeft(firstWithRight, leftWithFirst)._2
		val reformedThis =
			if ((finalLeft eq left) & (rightWithFirst eq right)) this
			else reform.reformed(finalLeft, operator, rightWithFirst)
		(firstWithRight, reformedThis)
	}

	protected override def reform_:[X](first :Select[X, R])(reform :QueryReform)
	                                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[P, R]) =
		reform_:(first :SingleQuery[X, R])(reform)
*/

	@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
	override def reformed(implicit spelling :SQLSpelling) :CompoundSelect[P, R] =
		try {
			spelling.queryReform(this)
		} catch {
			case e :InvalidSQLException =>
				throw new MisalignedExpressionException(
					s"Irreconcilable shapes of member selects in $this: $left vs $right.", e
				)
		}

	protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelect[P, R] = {
		val ctx = (reform, spelling) //consider: should we save reform or reform.subreform(this)?
		var res = if (_reformedContext == ctx) _reformed else null
		if (res == null) {
			res = reform.default(this)
			synchronized {
				if (_reformedContext == null) {
					_reformed = res
					_reformedContext = ctx
				}
			}
		}
		res
	}


	/** A cached reformed version of this instance created using `Reform` and `SQLSpelling` from `_reformedContext`. */
	@volatile @transient private[this] var _reformed        :CompoundSelect[P, R] = _
	/** `Reform` and `SQLSpelling` instances used to create `_reformed` instance, if not `null`.  */
	@volatile @transient private[this] var _reformedContext :(QueryReform, SQLSpelling) = _

	private[sql] final def wasReformedBy(params :(QueryReform, SQLSpelling)) :Unit = synchronized {
		_reformed = this
		_reformedContext = params
	}

//		private[sql] override def `->reformed`(reform :Reform)(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//			reformed(reform)

/*
	protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
		var res = _shape
		if (_shapeSpelling == spelling)
			res
		else {
			val reformed = try { spelling.queryReform(this) } catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException("Non unifiable select clauses in query " + this + ".", e)
			}
			spelling.shape(selectClause)
//			val l = spelling.inLeft(this).shape(reformed.left)
//			val r = spelling.inRight(this).shape(reformed.right)
			try { res = l | r } catch {
				case _ :IllegalArgumentException =>
					throw new MisalignedExpressionException(
						if ((reformed.left eq left) && (reformed.right eq right))
							s"Irreconcilable shapes of member selects in $this: $l vs $r."
						else
							s"Irreconcilable shapes of member selects in $this (unified as $reformed): $l vs $r."
					)
			}
			if (_shapeSpelling == null) synchronized {
				if (_shapeSpelling == null) {
					_shapeSpelling = spelling
					_shape = res
				}
			}
			res
		}
	}

	@volatile @transient private var _shape :RowShape = _
	@volatile @transient private var _shapeSpelling :SQLSpelling = _

	protected override def columnCount(implicit spelling :SQLSpelling) :Int =
		if (_shapeSpelling == spelling)
			_shape.size
		else try {
			val reformed = spelling.queryReform(this)
			val leftCount  = spelling.inLeft(this).columnCount(reformed.left)
			val rightCount = spelling.inRight(this).columnCount(reformed.right)
			if (leftCount != rightCount)
				throw new IllegalStateException(
					if ((reformed.left eq left) && (reformed.right eq right))
						s"Differing column counts of member selects of $this: $leftCount vs $rightCount."
					else
						s"Differing column counts of member selects of $this (unified as $reformed): $leftCount vs $rightCount."
				)
			leftCount
		} catch {
			case e :InvalidSQLException =>
				throw new IllegalStateException("Non unifiable select clauses in query " + this + ".", e)
		}
*/
	protected override def defaultSpelling(context :SQLContext[P])(implicit spelling :SQLSpelling) :SpelledSQL[P] = {
		val withClause = if (spelling.scope == TopScope) this.withClause else localWithClause
		val withSQL =
			if (withClause.isEmpty) SpelledSQL(context)
			else spelling.inWith(withClause)(context, Parameterization.paramless) + " "
		val CompoundSelect(left, _, right) = spelling.queryReform(this)
		val inLeft = spelling.inLeft(this)
		val l = left match {
			case CompoundSelect(_, op, _) if op != operator || left.withClause.local.nonEmpty =>
				"(" +: (inLeft(left)(withSQL.context) + ")")
			case _ =>
				inLeft(left)(withSQL.context)
		}
		val inRight = spelling.inRight(this)
		val r = right match { //use operator name comparison as different implementations than Minus are possible
			case CompoundSelect(_, op, _) if op != operator || operator.NAME == "MINUS" ||
			                                 right.localWithClause.nonEmpty =>
				"(" +: (inRight(right)(l.context) + ")")
			case _ =>
				inRight(right)(l.context)
		}
		val sql = withSQL.sql + l.sql + (" " + spelling(operator) + " ") + r.sql
		SpelledSQL(sql, withSQL.setter + l.setter + r.setter, r.context)
	}


	override def homomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _]  =>
			operator == other.operator && (left homomorphic other.left) && (right homomorphic other.right)
		case _ => false
	}
	override def isomorphic(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] =>
			operator == other.operator && (left isomorphic other.left) && (right isomorphic other.right)
		case _ => false
	}
	override def equivalent(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] =>
			operator == other.operator && (left equivalent other.left) && (right equivalent other.right)
		case _ => false
	}
	override def identical(that :Query.__) :Boolean = that match {
		case _ if this eq that => true
		case other :CompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && (left identical other.left) && (right identical other.right)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :CompoundSelect[_, _] if canEqual(other) && other.canEqual(this) =>
			operator == other.operator && left == other.left && right == other.right
		case _ => false
	}
	override def hashCode :Int = (operator.hashCode * 31 + left.hashCode) * 31 + right.hashCode

	override def toString :String = s"($left) $operator ($right)"
}



object CompoundSelect {
	def apply[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V]) :CompoundSelect[P, V] =
		new Impl(left, operator, right) with RowShapeCache {
			override lazy val selectClause = super.selectClause
		}

	def apply[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V], selectClause :SQLShape[V])
			:CompoundSelect[P, V] =
	{
		val res = new ReformedCompoundSelect(left, operator, right, selectClause)
		validate(res)
		res
	}

	//we don't know if they have a SelectId - should we check all selects for it?
//		private[sql] def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
//		                               (reform :Reform)(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//			reformed[P, V](left, operator, right, CompoundSelectIdSQL.selectClause(_))(reform)

	private[sql] def reformed[P, V]
	                 (left :Query[P, V], operator :SelectOperator, right :Query[P, V], selectClause :SQLShape[V])
	                 (reform :QueryReform)(implicit spelling :SQLSpelling)
			:CompoundSelect[P, V] =
	{
		val res = new ReformedCompoundSelect(left, operator, right, selectClause)
		res.wasReformedBy((reform, spelling))
		res
	}

	private[sql] def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V],
	                                selectClause :CompoundSelect[P, V] => SQLShape[V])
	                               (reform :QueryReform)(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
	{
		val init = selectClause
		val res = new Impl[P, V](left, operator, right) with RowShapeCache {
			override lazy val selectClause = init(this)
			shape
		}
		res.wasReformedBy((reform, spelling))
		res
	}


	@inline def unapply[P, V](query :Query[P, V]) :Opt[(Query[P, V], SelectOperator, Query[P, V])] =
		query match {
			case op :CompoundSelect[P @unchecked, V @unchecked] => Got((op.left, op.operator, op.right))
			case _ => Lack
		}

	private[sql] def validate[V, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[V, Q]) :Unit = {
		if (!(query.left.rowForm comparable query.right.rowForm))
			throw new IllegalArgumentException(
				"Mismatched select clauses of member selects (" + query.left.rowForm + " vs " + query.right.rowForm
				+ ") in " + query + "."
			)
		if (!(query.left.rowForm comparable query.selectClause.selectForm))
			throw new IllegalArgumentException(
				"Given select clause " + query.selectClause + " is not comparable with form " + query.left.rowForm +
					" of the left side in " + query + "."
			)
		if (!(query.right.rowForm comparable query.selectClause.selectForm))
			throw new IllegalArgumentException(
				"Given select clause " + query.selectClause + " not comparable with form " + query.right.rowForm +
					" of the right side in " + query + "."
			)
	}

	private[sql] def validate[V, Q[_]](left :QueryTemplate[V, Q], operator :SelectOperator, right :QueryTemplate[V, Q],
	                                   rowForm :SQLReadForm[V]) :Unit =
	{
		if (!(left.rowForm comparable right.rowForm))
			throw new IllegalArgumentException(
				"Mismatched select clauses of member selects (" + left.rowForm + " vs " + right.rowForm +
				") in (" + left + ") " + operator + " (" + right + ")."
			)
		if (!(left.rowForm comparable rowForm))
			throw new IllegalArgumentException(
				"Given rowForm " + rowForm + " not comparable with form " + left.rowForm + " of the left side in (" +
				left + ") " + operator + " (" + right + ")."
			)
		if (!(right.rowForm comparable rowForm))
			throw new IllegalArgumentException(
				"Given rowForm " + rowForm + " not comparable with form " + right.rowForm + " of the right side in (" +
				left + ") " + operator + " (" + right + ")."
			)
	}


	type __ = CompoundSelect[_, _]

	type TypedCompoundSelect[P, +M[O] <: MappingAt[O], V] = CompoundSelect[P, V] { type RowMapping[O] <: M[O] }



	/** Common declarations shared by ''compound select'' implementations:
	  * statement [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]
	  * and expression [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]].
	  */
	trait CompoundSelectTemplate[V, +Q[X] <: QueryTemplate[X, Q]] extends QueryTemplate[V, Q] { this :Q[V] =>
		val left  :Q[V] //must be primary constructor parameters!
		val right :Q[V]
		val operator :SelectOperator

		override def rowForm :SQLReadForm[V] = left.rowForm

		override def columns :Seq[TypedColumn[_, this.type]] = left.columns.withOrigin[this.type]

		//fixme: selectClause at least of reformed instances should best represent the expression:
		//  we should not return a term when paired with a ComponentSQL, and reforming composite expressions
		//  should be recursive, always picking the expression type most likely to be usable in future reforming
		override def selectClause :SQLShape[V] = {
			val spelling = StandardSQL.spelling.inSelect
			val leftShape = spelling.shape(left.selectClause)
			val rightShape = spelling.shape(right.selectClause)
			if (!(leftShape <:> rightShape))
				throw new MismatchedExpressionsException(
					"Cannot determine select clause layout for " + this + " because the shapes of left and right side " +
					"are unrelated: " + leftShape + " vs. " + rightShape + "."
				)
			left.selectClause
		}
		override val selectCount  :Int = left.selectCount + right.selectCount
//		override def selectClause :SQLLayout[V] =
		//consider: this means that the local with clause of any compound select must be empty; are we fine with it?
		private lazy val ctes     :WithClause = left.withClause.outer ++ right.withClause.outer
		override def withClause   :WithClause = ctes

//		override def rowsTo[X](implicit conversion :SQLConversion[V, X]) :Q[X] =
//			if (conversion.isIdentity) conversion(this)
//			else construct(left.rowsTo[X], operator, right.rowsTo[X])
//
//		override def map[X](f :V => X) :Q[X] = construct(left.map(f), operator, right.map(f))
//
//		protected[this] def construct[X](left :Q[X], operator :SelectOperator, right :Q[X]) :Q[X]
		@inline final private[sql] def asSuperQuery :Q[V] = this

		/** The default strategy for unifying the ''select'' clauses of `left` and `right` subqueries
		  * of this ''compound select''. The default implementation delegates to
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.defaultReform defaultReform]].
		  */
		override def defaultReform(implicit spelling :SQLSpelling) :QueryReform =
			if (spelling.scope == TopScope) operator.topReform else operator.defaultReform

		/** The default reforming strategy for unifying select clauses of SQL ''selects'' within a ''compound select'',
		  * used for top level queries, that is when this query is the whole spelled expression, and not a subquery
		  * of another ''compound select''. The default implementation delegates to
		  * [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.operator operator]]`.`[[net.noresttherein.oldsql.sql.Select.SelectOperator.topReform topReform]].
		  */
//		override def topReform(implicit spelling :SQLSpelling)     :QueryReform = operator.topReform(this)

		//it's tempting to extract the identical reform/reform_:/reformed methods from CompoundSelect and CompoundSelectSQL,
		//  but unfortunately TopSelectSQL inherits both, which would lead to a conflict.

		@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
		protected override def shape(implicit spelling :SQLSpelling) :RowShape = {
			val reformed = this.reformed
			try {
				spelling.inSelect.shape(reformed.selectClause)
			} catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException(
						if (reformed eq this)
							"Irreconcilable shapes of member selects in " + this + "."
						else
							"Irreconcilable shapes of member selects in " + this + " (unified as " + reformed + ").",
						e
					)
			}
		}
		@throws[MisalignedExpressionException]("if select clauses of member selects cannot be unified to a common shape.")
		protected override def columnCount(implicit spelling :SQLSpelling) :Int = {
			val reformed = this.reformed
			try {
				spelling.inSelect.columnCount(reformed.selectClause)
			} catch {
				case e :InvalidSQLException =>
					throw new MisalignedExpressionException(
						if (reformed eq this)
							"Irreconcilable shapes of member selects in " + this + "."
						else
							"Irreconcilable shapes of member selects in " + this + " (unified as " + reformed + ").",
						e
					)
			}
		}
	}


	private[sql] trait ReformedCompoundSelectTemplate[V, +Q[X] <: QueryTemplate[X, Q]]
		extends CompoundSelectTemplate[V, Q]
	{ this :Q[V] =>
		override lazy val rowForm = selectClause.selectForm

		protected override def shape(implicit spelling :SQLSpelling) :RowShape =
			spelling.inSelect.shape(selectClause)
	}

	private class Impl[P, V](override val left :Query[P, V], override val operator :SelectOperator,
	                         override val right :Query[P, V])
		extends CompoundSelect[P, V]
	{
		override type RowMapping[O] = left.RowMapping[O]
		override def mapping[O] = left.mapping[O]
		override def export[O] = left.export[O] //todo: this should involve some reconciliation

		override lazy val constituents = super.constituents

		override def bind(params :P) :QuerySQL[RowProduct, V] =
			operator(left.bind(params), right.bind(params))
	}

	private class ReformedCompoundSelect[P, V](l :Query[P, V], op :SelectOperator, r :Query[P, V],
	                                           override val selectClause :SQLShape[V])
		extends Impl(l, op, r)
		   with ReformedCompoundSelectTemplate[V, ({ type Q[X] = Query[P, X] })#Q] with RowShapeCache
}




/** Implements a set operation combining the result sets of two parameterized
  * [[net.noresttherein.oldsql.sql.Select.SelectMapping selects]]
  * (or other [[net.noresttherein.oldsql.sql.MappingQuery queries]]), sharing the same row schema,
  * as defined by the shared [[net.noresttherein.oldsql.schema.Mapping mapping]] type `M`. The kind of operation
  * is defined by the [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]]
  * of the [[net.noresttherein.oldsql.sql.CompoundSelect.operator operator]] member property.
  */ //todo: move it to sql or sql.CompoundSelect
trait CompoundSelectMapping[P, M[O] <: MappingAt[O]]
	extends CompoundSelect[P, M[Unit]#Subject] with MappingQuery[P, M]
{
	override val left :MappingQuery[P, M]
	override val right :MappingQuery[P, M]

	override def mapping[O] = left.mapping
	override def export[O]  = left.export //todo: this should involve some reconciliation

	override def bind(params :P) :MappingQuerySQL[RowProduct, M] = operator(left.bind(params), right.bind(params))
}


object CompoundSelectMapping {
	def apply[P, M[O] <: MappingAt[O]]
	         (left :MappingQuery[P, M], operator :SelectOperator, right :MappingQuery[P, M])
			:CompoundSelectMapping[P, M] =
		new Impl(left, operator, right, left.rowForm) with RowShapeCache

	def reformed[P, M[O] <: MappingAt[O]](left :MappingQuery[P, M], operator :SelectOperator, right :MappingQuery[P, M],
	                                      rowForm :SQLReadForm[M[Unit]#Subject]) :CompoundSelectMapping[P, M] =
	{
		CompoundSelect.validate(left, operator, right, rowForm)
		new Impl(left, operator, right, rowForm)
	}

	@inline def unapply[P, V](query :Query[P, V])
			:Opt[(MappingQuery[P, M], SelectOperator, MappingQuery[P, M]) forSome { type M[O] <: MappingAt[O] }] =
		query match {
			case op :CompoundSelectMapping[P @unchecked, MappingAt @unchecked] =>
				Got((op.left, op.operator, op.right))
			case _ => Lack
		}


	type __ = CompoundSelectMapping[_, M] forSome { type M[A] <: MappingAt[A] }

	private class Impl[P, M[O] <: MappingAt[O]]
	                  (override val left :MappingQuery[P, M], override val operator :SelectOperator,
	                   override val right :MappingQuery[P, M], override val rowForm :SQLReadForm[M[Unit]#Subject])
		extends CompoundSelectMapping[P, M]
}



