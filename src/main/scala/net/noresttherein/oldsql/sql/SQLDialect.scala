package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.morsels.TextCase
import net.noresttherein.oldsql.morsels.TextCase.LowerCase
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.Relation.{Aliased, RelVar, Table}
import net.noresttherein.oldsql.slang.OptionGuardExtension
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.Incantation.Cantrip
import net.noresttherein.oldsql.sql.Select.{Intersect, Minus, SetOperator, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLDialect.{DefaultSpelling, SQLSpelling}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.{FromScope, GroupByScope, HavingScope, InsertScope, SelectScope, UpdateScope, WhereScope}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMapper, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.QuerySQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A high level bridge between the SQL/DML DSL of this package and the JDBC API. It is responsible for translating
  * and formatting various SQL statements and queries into SQL strings understandable by the DBMS in use.
  * Return objects wrap [[java.sql.PreparedStatement PreparedStatement]] instances, handling setting of parameters,
  * reading and mapping the result as well as all related functions such as batching of same statement types.
  *
  * The default implementation is object [[net.noresttherein.oldsql.sql.StandardSQL$ StandardSQL]],
  * extending [[net.noresttherein.oldsql.sql.StandardSQL StandardSQL]] class, which implements a subset of
  * the SQL standard understood by most databases. Most situations can be served by this dialect, especially
  * that it uses double dispatch to delegate the actual SQL generation to the classes being its abstract representation
  * in a polymorphic, object-orienting manner, so language extensions can be performed by custom implementations
  * of the appropriate expression class. Some DBMS or requirements for special handling, such as setting particular
  * statement properties, handling of more advanced or proprietary JDBC features cannot be however served
  * by the existing API and can be introduced globally by providing an `SQLDialect` tuned to the application's
  * or DBMS needs. In particular, any changes to the SQL itself, such as formatting, comments, query hints or
  * rendering of existing expression classes will be most convenient to implement in this place.
  *
  * This is a high-level class with a narrow API serving as a facade to the complex task of SQL rendering.
  * It concerns itself mainly with the creation of executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
  * instances, including controlling [[java.sql.PreparedStatement PreparedStatement]] parameters and properties,
  * and setting up the handling of the execution results. The task of actual SQL formatting is,
  * by the [[net.noresttherein.oldsql.sql.StandardSQL! default]] implementation, delegated
  * to [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] which translates any class in the package
  * representing some database, SQL, DML or DDL element, to an intermediate
  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]]. Any changes required in that scope will be
  * easier to implement on that level, by providing a custom spelling for an otherwise unmodified `StandardSQL`.
  * @see [[net.noresttherein.oldsql.sql.StandardSQL]]
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling]]
  * @author Marcin Mościcki
  */ //consider: maybe this should go into the mechanics package?
trait SQLDialect {
	def apply[X, R, Y](query :Query[X, R])(implicit result :StatementResult[R, Y]) :Incantation[X, Y]
	def apply[R, Y](query :QuerySQL[RowProduct, R])(implicit result :StatementResult[R, Y]) :Cantrip[Y]
//	def apply[X <: Chain, Y](call :FunctionSQL[RowProduct, GlobalScope, X, Y]) :Cantrip[Y]
	//todo: double dispatch to handle Call, InsertReturning
	def apply[X, Y](statement :DMLStatement[X, Y]) :Incantation[X, Y]
	def apply[X, Y](statement :Call[X, Y]) :Incantation[X, Y]
//	def apply[Y](statement :GroundDMLStatement[Y]) :Cantrip[Y] = apply(statement :DMLStatement[(), Y])
//	def apply[X](call :CallProcedure[X]) :Incantation[X, ()]
//	def apply[X, Y](call :CallFunction[X, Y]) :Incantation[X, Y]
}




object SQLDialect {

	/** A syntactical fragment of an SQL expression, representing logical scope of a clause such as
	  * ''from'' or ''where'' clauses of an SQL ''select''. Used in the process of spelling SQL expressions,
	  * affecting mainly the subset of table columns used when inlining component-level expressions.
	  * This is done by default by delegating to one of the standard column lists dedicated to individual
	  * database operations (''select'', ''insert'', ''update'' and ''filter'' - for the ''where'' clause).
	  */
	trait SpellingScope {
		/** All columns, direct or indirect, of the given mapping which are applicable to this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationType.Prohibited Prohibited]] buff.
		  * @see [[net.noresttherein.oldsql.OperationType.defaultColumns]]
		  */
		def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

		/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationType.NonDefault NonDefault]] buff.
		  * @see [[net.noresttherein.oldsql.OperationType.columns]]
		  */
		def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]
	}


	object SpellingScope {

		private class OperationTypeScope(op :OperationType, override val toString :String) extends SpellingScope {
			override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = op.columns(mapping)

			override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] =
				op.defaultColumns(mapping)
		}

		val SelectScope  :SpellingScope = new OperationTypeScope(SELECT, "SELECT")
		val FromScope    :SpellingScope = new OperationTypeScope(SELECT, "FROM")
		val WhereScope   :SpellingScope = new OperationTypeScope(FILTER, "WHERE")
		val GroupByScope :SpellingScope = new OperationTypeScope(SELECT, "GROUP BY")
		val HavingScope  :SpellingScope = new OperationTypeScope(FILTER, "HAVING")
		val InsertScope  :SpellingScope = new OperationTypeScope(INSERT, "INSERT")
		val UpdateScope  :SpellingScope = new OperationTypeScope(UPDATE, "UPDATE")
	}




	/** A strategy class used to render all classes representing some SQL/DML/DDL syntax features or database objects
	  * as SQL (or DML/DDL, appropriately). It provides high-level `spell` methods for all classes which represent
	  * whole statements of various types, serving as entry points for converting the arguments to their
	  * intermediate [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] representation.
	  * `SpelledSQL` associates SQL fragments in a [[net.noresttherein.oldsql.morsels.ChunkedString 'chunked']] form
	  * with meta information shared with other fragments of the same statements (such as table aliases)
	  * and everything necessary to eventually create and execute a [[java.sql.PreparedStatement PreparedStatement]]
	  * with the final statement text. The implementations of these methods in this class universally delegate back
	  * to the rendered object's appropriate method such as
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]], which are responsible
	  * for assembling their SQL representation from SQL fragments for their components created using this instance.
	  * The callbacks for these [[net.noresttherein.oldsql.sql.SQLExpression subexpressions]] and other constructs
	  * which do not exist by themselves use overloaded `apply` methods (where the scope is evident) or methods named
	  * after the element(s) they print and have signatures accepting a preexisting context of the larger statement.
	  *
	  * The formatting happens recursively, by dividing any composite expressions/syntactic constructs into
	  * subexpressions, formatting them in a mostly sequential, left-to-right manner, and concatenating the results.
	  * The devil is in the context passing and combining of the meta data of the concatenated `SpelledSQL` fragments:
	  * expressions refer to table [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL components]]
	  * and [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters, which must be properly resolved into,
	  * respectively, their actual aliases in the SQL and getter functions of the parameter(s) of the complete,
	  * final query/statement. The scope with visible symbols changes neither in a top-down, nor left-to-right fashion,
	  * requiring a look-ahead and look-aside (in particular ''select'' clauses depend on the tables
	  * in their ''from'' clauses), so the process requires performing some things out of order and passing
	  * of information in both directions.
	  *
	  * This trait itself takes a hands-off approach, implementing only methods which:
	  *   1. delegate back to the formatted object's appropriate `defaultSpelling` equivalent and satisfying themselves
	  *      with the default format defined by the element,
	  *   1. are under tight constraints coming from the contracts of collaborating classes and leave little freedom
	  *      for implementation,
	  *   1. handle very small syntactical elements such as keywords with obvious implementations.
	  * As the result, they would rarely need overriding for functional reasons.
	  *
	  * The default implementation is [[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]],
	  * which expands on the above with some more arbitrary implementations and - in some cases - overrides them
	  * for efficiency.
	  *
	  * Custom implementations, regardless if derived from `DefaultSpelling` or this trait directly, are likely
	  * to benefit from implementing or including components for the ''visitor'' traits for separate type hierarchies
	  * if they desire to override the behaviour on a more detailed scale, down to case-by-case for each single class
	  * provided by the framework. They come with mix-in traits implementing cases of any branch by delegating them
	  * step-by-step to their common super type, allowing overriding on arbitrary levels. Consult their documentation
	  * to learn more about composing tailored visitor implementations.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.ExpressionVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.DMLStatement.StatementVisitor]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.RowProductVisitor]]
	  */
	trait SQLSpelling {
		/** Renders as a full textual DML an abstract representation of any supported database operation.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted DML string with an [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the statement.
		  */
		def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P, RowProduct] =
			defaultSpelling(statement)

		/** Renders as a stand alone SQL query a parameterized query. Defaults to `this(query)(newContext)`
		  * (which defaults to `query.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]]).
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the query.
		  */
		def spell[P, V](query :Query[P, V]) :SpelledSQL[P, RowProduct] =
			apply(query)(newContext)

		/** Renders as a stand alone SQL query a ''select'' top-level, parameterless ''select'' expression. Defaults to
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.paramless paramless]]`(query)(`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.newContext newContext]]`)`.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the query.
		  */
		def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~, RowProduct] =
			paramless(query)(newContext)


		/** Renders as SQL a parameterless, [[net.noresttherein.oldsql.sql.ast.QuerySQL.GroundQuery ground]] ''select''
		  * expression for use as a part of a larger SQL/DML expression, either as a dependent select
		  * or in a compound ''select''.
		  * Defaults to `query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.paramlessSpelling paramlessSpelling]].
		  * This is currently completely equivalent to
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,LocalScope,V])* apply(query)]]
		  * (the general path for any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]) and exists
		  * as a separate method both for historical reasons and to open a path for future modifications
		  * which might require it due to some technical complications. Prefer using `this(query)`.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @param query   the formatted stand-alone expression.
		  * @param context the list of tables in the ''select'' under which the query is to be nested,
		  *                or an empty context for top-level queries. As queries independent of any tables
		  *                or parameters do not require any standard functionality of `SQLContext`,
		  *                and the extended context used internally with ''select'', ''where'' and other clauses
		  *                of the expression is discarded, this can essentially be any value for the purpose
		  *                of this method; it will however be included unchanged in the returned `SpelledSQL`.
		  *                The parameter is declared for consistency with other methods and to facilitate uniform
		  *                passing of the context when formatting subsequent fragments of a larger query.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *                all potential [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]] parameters
		  *                present in the query.
		  */ //consider: removing context parameter, as we do not require Parameterization any way
		//this cannot be apply, as then Query[F, V] doesn't overload to the method for generic SQLExpression
		def paramless[V](query :QuerySQL[RowProduct, V])(context :SQLContext) :SpelledSQL[@~, RowProduct] =
			query.paramlessSpelling(this, context)

		/** Renders as SQL a parameterized query expression for use as a part of a larger SQL/DML expression -
		  * either as a dependent (nested) ''select'' or in a compound ''select''. This is analogous to main
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,LocalScope,V])* apply]]
		  * method for SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and the control flow follows
		  * the same pattern; the method delegates to
		  * `query.`[[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]] by means of bridge method
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.defaultSpelling[P](e:Query[P,_])* defaultSpelling]]`(query)(context)`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam V      the nominal value type of the formatted expression.
		  * @param query   the formatted query as an abstract object.
		  * @param context the list of tables in the outer ''select'' for the use of `query` as a dependent ''select'',
		  *                or an empty context if the argument is to be used as a stand-alone query.
		  *                `Query` instances do not depend on any external tables and all tables used by them
		  *                are invisible outside their scope, so it is included unchanged in the returned `SpelledSQL`.
		  *                The parameter is declared for consistency with other methods and to facilitate uniform
		  *                passing of the context when formatting subsequent fragments of a larger query.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]]
		  *                setting all bound and unbound parameters present in the query.
		  */ //consider: removing context parameter
		def apply[P, V](query :Query[P, V])(context :SQLContext) :SpelledSQL[P, RowProduct] =
			defaultSpelling(query)(context)

		/** Renders as SQL an abstract representation of any SQL expression, for use within a larger SQL/DML expression.
		  * Defaults to `e.`[[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]`(this)(context, params)`
		  * (using bridge method
		  * `this.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.defaultSpelling[P,F<:RowProduct](e:SQLExpression[F,LocalScope,_])* defaultSpelling]]`(this)(context, params)`).
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` on which `e` is based, listing all tables in the ''from'' clause
		  *                of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam V      the nominal value type of the formatted expression.
		  * @param e       the formatted SQL (sub)expression as an abstract syntax tree.
		  * @param context the list of tables in scope of the formatted expression; this includes all tables
		  *                in the ''from'' clause of the encompassing expression (even if under ''group by'' clause)
		  *                as well as any tables in the outer ''select''s and unbound parameters, in the order
		  *                consistent with their appearance in `F`. It is purely an 'in' parameter, included unchanged
		  *                in the returned `SpelledSQL`, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  *                (including synthetic unbound parameters) can introduce new table aliases to scope.
		  * @param params  ''in/out'' parameter carrying necessary information about all (relevant)
		  *                [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]] and
		  *                [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters of the formatted SQL.
		  *                In its main role, it is a list of
		  *                [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] setting the parameters
		  *                on a [[java.sql.PreparedStatement PreparedStatement]]: it is assumed to contain forms
		  *                for the whole prefix preceding the fragment represented by `e` in the complete,
		  *                yet-to-be-formatted SQL expression. While this list is unusable by this method,
		  *                forms for all parameters in `e` should be appended to it and returned as the parameterization
		  *                included in `SpelledSQL`. As its second function, it provides getter functions `P => X`
		  *                for all ''unbound'' parameters `X` in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedTable JoinedTable]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause, creating non-linear concatenation
		  *                (possible because ''from'' clauses are completely independent of the ''select'' clauses).
		  *                Callers may 'cheat' and omit information about parameters proven to be unused by `e`
		  *                and combine them later with `SpelledSQL` returned by this method.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *                all bound and unbound parameters present in the query.
		  */
		def apply[P, F <: RowProduct, V]
                 (e :SQLExpression[F, LocalScope, V])
                 (context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F] =
			defaultSpelling(e)(context, params)

		/** Recursively splits an expression into its constituting columns, to be rendered as a part
		  * of a comma-separated list, for example when inlining a complex Scala type to individual parameters
		  * for a procedure call or when formatting a ''select'' clause. Not all expressions can be split this way,
		  * with the most common case being ''select'' expressions returning multiple columns - in that case
		  * an exception is thrown, although this should not occur in practice when the method is called in the process
		  * of formatting a valid top-level query/statement.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing `e`
		  *                as its subexpression; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` on which `e` is based, listing all tables in the ''from'' clause
		  *                of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @param e       the formatted SQL (sub)expression as an abstract syntax tree.
		  * @param context the list of tables in scope of the split expression; this includes all tables
		  *                in the ''from'' clause of the encompassing expression (even if under ''group by'' clause)
		  *                as well as any tables in the outer ''select''s and unbound parameters, in the order
		  *                consistent with their appearance in `F`. It is purely an 'in' parameter, included unchanged
		  *                in the returned `SpelledSQL`, as only [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  *                (including synthetic unbound parameters) can introduce new table aliases to scope.
		  * @param params  ''in/out'' parameter carrying necessary information about all (relevant)
		  *                [[net.noresttherein.oldsql.sql.ast.SQLTerm.SQLParameter bound]] and
		  *                [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters of the formatted SQL.
		  *                In its main role, it is a list of
		  *                [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] setting the parameters
		  *                on a [[java.sql.PreparedStatement PreparedStatement]]: it is assumed to contain forms
		  *                for the whole prefix preceding the fragment represented by `e` in the complete,
		  *                yet-to-be-formatted SQL expression. While this list is unusable by this method,
		  *                forms for all parameters in `e` should be appended to it and returned as the parameterization
		  *                included in `SpelledSQL`. As its second function, it provides getter functions `P => X`
		  *                for all ''unbound'' parameters `X` in the scope of the formatted expression, given
		  *                the [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedTable JoinedTable]]
		  *                of the [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component]] expression
		  *                representing them. These getters are in turn necessary to adapt the forms of individual
		  *                expression to the type `P` of the parameter(s) of the final SQL.
		  *                Note that this means that rendering of a ''select'' clause requires the parameterization
		  *                created by the rendering of its ''from'' clause, creating non-linear concatenation
		  *                (possible because ''from'' clauses are completely independent of the ''select'' clauses).
		  *                Callers may 'cheat' and omit information about parameters proven to be unused by `e`
		  *                and combine them later with `SpelledSQL` returned by this method.
		  * @return        a list of formatted SQL strings for every column in this expression in the exact same order.
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] and
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]]
		  *                included in the last `SpelledSQL` of the list represent the complete state,
		  *                taking into account all previous columns and is the same as if the expression was formatted
		  *                using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,LocalScope,V])* apply]]
		  *                method. The instances in chunks for all preceding columns are undefined
		  *                and should not be used.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual column strings, " +
		                                        "for example a multi-column SQL select.")
		def explode[P, F <: RowProduct, V]
		           (e :SQLExpression[F, LocalScope, V])
		           (context :SQLContext, params :Parameterization[P, F]) :Seq[SpelledSQL[P, F]] =
			e.inlineSpelling(this)(context, params)

		/** Renders as SQL the 'outermost' component of the passed relation product - generally corresponding to
		  * the right-most join or a join-like class, which recursively formats any preceding components by
		  * delegating back to this method, until [[net.noresttherein.oldsql.sql.Dual Dual]] or
		  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] are reached, marking the begin of the rendered
		  * ''from'' clause. If `from` conforms to [[net.noresttherein.oldsql.sql.FromClause FromClause]]
		  * (is not aggregated), this will print the full ''from'' clause, including any join conditions if
		  * rendered using the 'join ... on ...' syntax, but without a ''where'' clause, instead collecting remaining
		  * filter condition in the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]]
		  * returned with `SpelledSQL`. If `from` is a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]],
		  * returned SQL will contain full ''from'', ''where'' and ''group by'' clauses, but without a ''having''
		  * clause.
		  *
		  * This method is not the entry point to the recursion, which starts with either
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere fromWhere]] or
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving groupByHaving]], depending on the type
		  * of the relation product used; the proper one between the above is picked by the polymorphic method
		  * [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]. The latter should always be used
		  * when formatting 'whole' objects (i.e., those referenced by [[net.noresttherein.oldsql.sql.Select Select]]
		  * expressions.
		  *
		  * Defaults to `from.`[[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]].
		  * @return a formatted SQL string in a chunked form with rendered and concatenated all of the ''from'',
		  *         ''where'', ''group by'', ''having'' clauses which are present in this instance - possibly none
		  *         and an empty string for [[net.noresttherein.oldsql.sql.Dual Dual]].
		  *         The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context spelling context]] included
		  *         appends to `context` argument the aliases for all tables present in the ''from'' clause,
		  *         as well as empty placeholders for [[net.noresttherein.oldsql.sql.JoinParam unbound]] parameters.
		  *         The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]] included
		  *         Reflects the full namespace made available to the ''select'' and ''order by'' clauses and contains
		  *         [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] for all formatted expressions
		  *         (but not necessarily for all ''join/where/having'' conditions present in the instance).
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere]]
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.spell]]
		  */
		def apply(from :RowProduct)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
			from.defaultSpelling(this)(context)

		/** Renders as SQL a call to an aggregate function (including any modifying clauses such as ''distinct'').
		  * This method is called from
		  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.defaultSpelling AggregateSQL.defaultSpelling]] and
		  * immediately dispatches to [[net.noresttherein.oldsql.sql.AggregateFunction.defaultSpelling AggregateFunction.defaultSpelling]].
		  * @tparam P       the type of the parameter or parameters of the complete query expression containing
		  *                 the enclosing expression; typically, it will be
		  *                 `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                 do not exist.
		  * @tparam F       the 'ungrouped' `FromClause` representing the namespace available
		  *                 to the aggregated expression (rather than the one on which the containing
		  *                 [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] is based).
		  * @param function the aggregate function invoked for a group of rows.
		  * @param arg      the expression in the scope of a single row passed as the argument to the function.
		  * @param context  the spelling context with table aliases indexed consistently with `F`.
		  * @param params   the parameterization context of [[net.noresttherein.oldsql.sql.UnboundParam unbound]]
		  *                 parameters of `F`, but ''not'' including any write forms.
		  * @return         the formatted SQL for `function(arg)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]]
		  *                 containing write forms for all parameters used by `arg` (and being otherwise unchanged
		  *                 from `params`).
		  */
		def apply[P, F <: RowProduct](function :AggregateFunction, distinct :Boolean)
		                             (arg :ColumnSQL[F, LocalScope, _])
		                             (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			function.defaultSpelling(this)(arg, distinct)(context, params)

		/** Renders as SQL a call to a standard SQL function (or a custom stored function). This method is called from
		  * [[net.noresttherein.oldsql.sql.ast.FunctionSQL.defaultSpelling FunctionSQL.defaultSpelling]] and
		  * immediately dispatches to [[net.noresttherein.oldsql.sql.StoredFunction.defaultSpelling StoredFunction.defaultSpelling]].
		  * @tparam P       the type of the parameter or parameters of the complete query expression containing the
		  *                 enclosing expression; typically, it will be
		  *                 `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                 do not exist.
		  * @tparam F       the `RowProduct` on which `args` is based, listing all tables in the ''from'' clause
		  *                 of the containing expression as well as columns in its ''group by'' clause (if present).
		  * @tparam X       a chain listing the types of all formal parameters of `function`.
		  * @tparam Y       the return type of the function and the value type of the whole formatted SQL expression.
		  * @param function the called function.
		  * @param args     a tuple expression containing expressions for all arguments of `function` as elements.
		  * @param context  the spelling context with table aliases indexed consistently with `F`.
		  * @param params   the parameterization context of [[net.noresttherein.oldsql.sql.UnboundParam unbound]]
		  *                 parameters of `F`, but ''not'' including any write forms.
		  * @return         the formatted SQL for `function(args)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]]
		  *                 appending write forms for all parameters used by `args` to `params`.
		  */
		def apply[P, F <: RowProduct, X, Y](function :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
		                                   (context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			function.defaultSpelling(this)(args)(context, params)


		def groupByHaving(from :GroupByClause)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] = {
			val groupBySQL = inGroupBy(from)(context)
			if (from.filter == True) groupBySQL
			else groupBySQL + (" " + HAVING + " ") + (inHaving(from.filter)(_, _))
		}

		/** Renders as an SQL fragment full ''from'' and ''where'' clauses of a ''select''. The process recursively
		  * calls [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply(from:RowProduct)* apply(from)(context)]]
		  * (which by default will delegate to
		  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct.defaultSpelling]]) and simultaneously
		  * appends ''join'' clauses (or simply lists the tables) and builds `SQLContext`
		  * and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]] for the rest
		  * of the SQL ''select'' to which `from` belongs. Recursion stops and backtracks when either
		  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.Subselect Subselect]]
		  * is encountered, marking the start of the ''from'' clause of the most nested ''select''. From that point,
		  * table aliases and join conditions are added to `context`.
		  * This method is invoked from [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]
		  * and the latter should be preferred as the entry point.
		  * @param from    the ''from'' clause to format
		  * @param context the namespace containing the aliases of all tables in scope of `from` due to belonging
		  *                to an outer ''select'', or a fresh instance if the formatted ''select'' is the top-level
		  *                expression is a top select.
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  */
		def fromWhere(from :FromClause)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] = {
			val fromSQL = inFrom(from)(context)
			val resultContext = fromSQL.context.copy(whereReversed = Nil)
			if (fromSQL.context.whereReversed.isEmpty)
				SpelledSQL(fromSQL.sql, resultContext, fromSQL.params)
			else {
				val whereSQL = fromSQL.context.whereReversed.reduceLeft((_1, _2) => _2 + ", " + _1)
				fromSQL + (" " + WHERE + " ") + whereSQL
			}
		}

		/** SQL fragment for an empty ''from'' clause. Defaults to an empty string, leading to rendering a ''select''
		  * without a ''from'' clause.
		  */
		def emptyFrom(context :SQLContext) :SpelledSQL[@~, Dual] = SpelledSQL(context)

		/** Creates an SQL fragment adding a single table to a non-empty ''from'' clause.
		  * This can happen both using a ''join'' clause (and optionally an accompanying ''on'' clause),
		  * and as simple comma-separated concatenation.
		  * If [[net.noresttherein.oldsql.sql.Join.condition join.condition]] is not used in the the returned SQL,
		  * it is added to the pending
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereReversed where]] clause
		  * in the `SQLContext` included in the returned SQL.
		  */
		def join[L <: FromSome, R[O] <: MappingAt[O]]
		        (join :L Join R, clause :String)(context :SQLContext) :SpelledSQL[join.Params, join.Generalized]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is called when no alias for the table
		  * has been explicitly specified by the application and one may need to be picked in order to ensure uniqueness
		  * of aliases for all tables listed in `context`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` listing all preceding tables in the ''from'' clause as well as tables
		  *                inherited from enclosing ''select''(s) and remaining in the scope of the formatted ''select''.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param context spelling context containing all preceding elements of the formatted ''from'' clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  parameterization providing access to the individual parameters in `P`
		  *                of the whole formatted query which can be used by the expressions.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.Relation.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]] remains
		  *                the same as `params`.
		  */
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M])(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is called when an alias for the table
		  * has been explicitly specified by the application. If the alias is already used by another table in `context`,
		  * it may be necessary to assign a unique one - the handling of this case is given up to subclasses.
		  * The exception is an empty string, which has the special meaning that no alias should be assigned
		  * (and references to columns of this table should not be qualified with the table name).
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` listing all preceding tables in the ''from'' clause as well as tables
		  *                inherited from enclosing ''select''(s) and remaining in the scope of the formatted ''select''.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param alias   proposed alias for the table.
		  * @param context spelling context containing all preceding elements of the formatted ''from'' clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  parameterization providing access to the individual parameters in `P`
		  *                of the whole formatted query which can be used by the expressions.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.Relation.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]] remains
		  *                the same as `params`.
		  */
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :String)(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is equivalent
		  * to `this.table(table, alias.get)(context, params)` if `alias` is defined,
		  * and to `this.table(table)(context, params)` otherwise. Each way, the `SQLContext` included
		  * in the returned `SpelledSQL` will contain an additional entry with a unique ''non-empty alias'' used
		  * by the expressions to refer to this particular occurrence of `table`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` listing all preceding tables in the ''from'' clause as well as tables
		  *                inherited from enclosing ''select''(s) and remaining in the scope of the formatted ''select''.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param alias   proposed alias for the table.
		  * @param context spelling context containing all preceding elements of the formatted ''from'' clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  parameterization providing access to the individual parameters in `P`
		  *                of the whole formatted query which can be used by the expressions.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.Relation.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.params parameterization]] remains
		  *                the same as `params`.
		  */
		def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :Option[String])(context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))

		/** Renders as SQL a reference to column `column` of the relation `table` in the ''from'' clause
		  * of the formatted expression. This method terminates the recursion and simply returns the column name
		  * qualified with the alias for the given `JoinedTable`, as specified by `context` - unless the alias
		  * is an empty string, in which case the column name will not be qualified. The `table` argument
		  * must refer to a [[net.noresttherein.oldsql.schema.Relation.Table Table]] relation (in official SQL
		  * terminology this includes ''stored views'' and ''select'' expressions as well as ''base tables'') -
		  * if it is some kind of synthetic pseudo relation existing only in the framework, an exception will be thrown.
		  *///context must correspond to RowProduct the table is based on (same number of tables)
		def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		          (table :JoinedRelation[O, T], column :ColumnMapping[_, O])
		          (context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F] =
		{
			val tableAlias = context(table)
			val columnAlias = if (tableAlias.length == 0) column.name else context(table) + "." + column.name
			SpelledSQL(columnAlias, context, params)
		}


		def apply(operator :SetOperator) :String = operator match {
			case Union => UNION
			case UnionAll => UNION_ALL
			case Intersect => INTERSECT
			case Minus => MINUS
			case _ => this.operator(operator.name)
		}

		/** Formats the given SQL literal. `SQLSpelling` returns the argument `sql` here,
		  * but subclasses can potentially change capitalization. This method is used by
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.TRUE TRUE]],
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.FALSE FALSE]],
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.NULL NULL]] and similar.
		  */
		def literal(sql :String) :String = sql

		/** Formats the given SQL operator (this involves arithmetic operators, but also
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.BETWEEN BETWEEN]] and
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.LIKE LIKE]], and subclasses can change letter case)
		  */
		def operator(sql :String) :String = sql

		/** Formats the given SQL function name. It is used both for standard SQL functions and custom stored functions.
		  * By default this method returns the argument as-is, but subclasses can change letter case, quote
		  * or even qualify the identifier.
		  */
		def function(sql :String) :String = sql

		/** Formats the given SQL keyword. This method is used for all constants such as ''insert'', ''update'',
		  * ''from'', and all such uppercase properties of this trait are initialized with it.
		  */
		def keyword(sql :String) :String = sql

		/** The spelling scope specifying what kind of statement is being currently formatted (''insert'', ''update'',
		  * ''select'', etc) and what clause. This affects primarily which columns will be included by default
		  * for [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component]] expressions, but can have
		  * also other effects. The scope is changed with [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.in in]]
		  * method or dedicated properties as the formatted expression/statement is recursively inspected.
		  */
		def scope :SpellingScope

		/** Switches spelling into an inline mode, in which multi-column expressions are never rendered as tuples,
		  * but as comma-separated columns (distinct expressions) without any delimiters. It is used when formatting
		  * tuple expressions of ''select'' clauses and function call arguments, but possibly also others.
		  * An ''inline'' speller should delegate to
		  * [[net.noresttherein.oldsql.sql.SQLExpression.inlineSpelling inlineSpelling]] rather than
		  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] methods and be equivalent
		  * to using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode this.explode]].
		  */
		def inline    :SQLSpelling

		def inSelect  :SQLSpelling //= in(SelectScope)
		def inFrom    :SQLSpelling //= in(FromScope)
		def inWhere   :SQLSpelling //= in(WhereScope)
		def inGroupBy :SQLSpelling //= in(GroupByScope)
		def inHaving  :SQLSpelling //= in(HavingScope)
		def inInsert  :SQLSpelling //= in(InsertScope)
		def inUpdate  :SQLSpelling //= in(UpdateScope)

		def in(scope :SpellingScope) :SQLSpelling = scope match {
			case SelectScope  => inSelect
			case FromScope    => inFrom
			case WhereScope   => inWhere
			case GroupByScope => inGroupBy
			case HavingScope  => inHaving
			case InsertScope  => inInsert
			case UpdateScope  => inUpdate
			case _ =>
				throw new IllegalArgumentException(
					"Unknown SQL expression scope: " + scope + ": " + scope.getClass.getName
				)
		}

		def NULL       :String = literal("null")
		def TRUE       :String = literal("true")
		def FALSE      :String = literal("false")
		def CONCAT     :String = operator("+")
		def LIKE       :String = operator("like")
		def BETWEEN    :String = operator("between")
		def NOT        :String = operator("not")
		def AND        :String = operator("and")
		def OR         :String = operator("or")
		def UNION      :String = operator("union")
		def UNION_ALL  :String = operator("union all")
		def INTERSECT  :String = operator("intersect")
		def MINUS      :String = operator("minus")
		def SELECT     :String = keyword("select")
		def FROM       :String = keyword("from")
		def WHERE      :String = keyword("where")
		def GROUP_BY   :String = keyword("group by")
		def HAVING     :String = keyword("having")
		def AS         :String = keyword("as")
		def INNER_JOIN :String = keyword("join")
		def OUTER_JOIN :String = keyword("outer join")
		def LEFT_JOIN  :String = keyword("left join")
		def RIGHT_JOIN :String = keyword("right join")
		def ON         :String = keyword("on")
		def INSERT     :String = keyword("insert")
		def INTO       :String = keyword("into")
		def VALUES     :String = keyword("values")
		def UPDATE     :String = keyword("update")
		def SET        :String = keyword("set")
		def MERGE      :String = keyword("merge")
		def DELETE     :String = keyword("delete")

		/** A hot spot allowing subclasses to inject specialized `SQLContext` implementations. */
		def newContext :SQLContext = SQLContext()


		//forwarding methods.

		protected final def defaultSpelling[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P, RowProduct] =
			statement.defaultSpellingForwarder(this)

		protected final def defaultSpelling[P](e :Query[P, _])(implicit context :SQLContext) :SpelledSQL[P, RowProduct] =
			e.defaultSpelling(this, context)

		protected final def defaultSpelling[P, F <: RowProduct, X, Y]
		                                   (f :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
		                                   (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			f.defaultSpelling(this)(args)

		protected final def paramSpelling[X, Y](f :SQLExecutable[X, Y]) :SpelledSQL[X, RowProduct] =
			f.paramSpellingForwarder(this)

		protected final def defaultSpelling[P, F <: RowProduct]
		                    (f :AggregateFunction)(arg :ColumnSQL[F, LocalScope, _], distinct :Boolean = false)
		                    (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			f.defaultSpelling(this)(arg, distinct)

		protected final def defaultSpelling[P, F <: RowProduct]
		                                   (e :SQLExpression[F, LocalScope, _])
		                                   (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			e.defaultSpelling(this)

		protected final def inlineSpelling[P, F <: RowProduct]
		                                  (e :SQLExpression[F, LocalScope, _])
		                                  (implicit context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			e.inlineSpelling(this) match {
				case Seq() => SpelledSQL("", context, params)
				case columns => columns.reduce(_.sql +: ", " +: _)
			}

		protected final def defaultSpelling(from :RowProduct)(context :SQLContext)
				:SpelledSQL[from.Params, from.Generalized] =
			from.defaultSpelling(this)(context)
	}



	object SQLSpelling {
		private val default = new DefaultSpelling(SelectScope)

		def apply() :SQLSpelling = default

		def apply(scope :SpellingScope) :SQLSpelling = new DefaultSpelling(scope)

//		def apply[P](exprSpelling :SQLContext[P] => ExpressionSpelling,
//		             fromSpelling :SQLContext[P] => RowProductVisitor[SpelledSQL[P]]) :SQLSpelling =
//			new TweakedDefaultSpelling(SelectScope, false)(exprSpelling, fromSpelling)


		/** A straightforward proxy to another `SQLSpelling` instance forwarding all method calls to the 'author'
		  * intended as a base class for implementations modifying the spelling of only few SQL clauses with regard
		  * to the proxy target.
		  */
		class SpellingRedactor(val author :SQLSpelling) extends SQLSpelling {
			override def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P, RowProduct] = author.spell(statement)
			override def spell[P, V](query :Query[P, V]) :SpelledSQL[P, RowProduct] = author.spell(query)
			override def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~, RowProduct] = author.spell(query)

			override def paramless[V](query :QuerySQL[RowProduct, V])(context :SQLContext) :SpelledSQL[@~, RowProduct] =
				author.paramless(query)(context)

//			override def apply[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P, RowProduct] = author(statement)

			override def apply[P, V](query :Query[P, V])(context :SQLContext) :SpelledSQL[P, RowProduct] =
				author.apply(query)(context)

			override def apply[P, F <: RowProduct, V]
			                  (e :SQLExpression[F, LocalScope, V])
			                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.apply(e)(context, params)

			override def explode[P, F <: RowProduct, V]
			                    (e :SQLExpression[F, LocalScope, V])
			                    (context :SQLContext, params :Parameterization[P, F]) :Seq[SpelledSQL[P, F]] =
				author.explode(e)(context, params)

			override def apply(from :RowProduct)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
				author.apply(from)(context)

			override def apply[P, F <: RowProduct]
			                  (function :AggregateFunction, distinct :Boolean)(arg :ColumnSQL[F, LocalScope, _])
			                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.apply(function, distinct)(arg)(context, params)

			override def apply[P, F <: RowProduct, X, Y]
			                  (function :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
			                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.apply(function)(args)(context, params)


			override def table[P, F <: RowProduct, M[A] <: MappingAt[A]]
			                  (table :Table[M])(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.table(table)(context, params)

			override def table[P, F <: RowProduct, M[A] <: MappingAt[A]]
			                  (table :Table[M], alias :String)
			                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.table(table, alias)(context, params)

			override def table[P, F <: RowProduct, M[A] <: MappingAt[A]]
			                  (table :Table[M], alias :Option[String])
			                  (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.table(table, alias)(context, params)

			override def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
			                   (table :JoinedRelation[O, T], column :ColumnMapping[_, O])
			                   (context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
				author.column(table, column)(context, params)

			override def fromWhere(from :FromClause)(context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
				author.fromWhere(from)(context)

			override def emptyFrom(context :SQLContext) :SpelledSQL[@~, Dual] = author.emptyFrom(context)

			override def join[L <: FromSome, R[A] <: MappingAt[A]](join :Join[L, R], clause :String)(context :SQLContext)
					:SpelledSQL[join.Params, join.Generalized] =
				author.join(join, clause)(context)

			override def groupByHaving(from :GroupByClause)
			                          (context :SQLContext) :SpelledSQL[from.Params, from.Generalized] =
				author.groupByHaving(from)(context)

			override def apply(operator :SetOperator) :String = author.apply(operator)

			override def literal(sql :String) :String  = author.literal(sql)
			override def operator(sql :String) :String = author.operator(sql)
			override def function(sql :String) :String = author.function(sql)
			override def keyword(sql :String) :String  = author.keyword(sql)

			override def scope :SpellingScope   = author.scope
			override def inline :SQLSpelling    = author.inline
			override def inSelect :SQLSpelling  = author.inSelect
			override def inFrom :SQLSpelling    = author.inFrom
			override def inWhere :SQLSpelling   = author.inWhere
			override def inGroupBy :SQLSpelling = author.inGroupBy
			override def inHaving :SQLSpelling  = author.inHaving
			override def inInsert :SQLSpelling  = author.inInsert
			override def inUpdate :SQLSpelling  = author.inUpdate
			override def in(scope :SpellingScope) :SQLSpelling = author.in(scope)

			override def NULL :String       = author.NULL
			override def TRUE :String       = author.TRUE
			override def FALSE :String      = author.FALSE
			override def CONCAT :String     = author.CONCAT
			override def LIKE :String       = author.LIKE
			override def BETWEEN :String    = author.BETWEEN
			override def NOT :String        = author.NOT
			override def AND :String        = author.AND
			override def OR :String         = author.OR
			override def UNION :String      = author.UNION
			override def UNION_ALL :String  = author.UNION_ALL
			override def INTERSECT :String  = author.INTERSECT
			override def MINUS :String      = author.MINUS
			override def SELECT :String     = author.SELECT
			override def FROM :String       = author.FROM
			override def WHERE :String      = author.WHERE
			override def GROUP_BY :String   = author.GROUP_BY
			override def HAVING :String     = author.HAVING
			override def AS :String         = author.AS
			override def INNER_JOIN :String = author.INNER_JOIN
			override def OUTER_JOIN :String = author.OUTER_JOIN
			override def LEFT_JOIN :String  = author.LEFT_JOIN
			override def RIGHT_JOIN :String = author.RIGHT_JOIN
			override def ON :String         = author.ON
			override def INSERT :String     = author.INSERT
			override def INTO :String       = author.INTO
			override def VALUES :String     = author.VALUES
			override def UPDATE :String     = author.UPDATE
			override def SET :String        = author.SET
			override def MERGE :String      = author.MERGE
			override def DELETE :String     = author.DELETE

			override def newContext :SQLContext = author.newContext

			override def toString :String = "Redactor(" + author + ")"
		}




		type ExpressionSpelling[P, F <: RowProduct] =
			ExpressionMapper[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P, F] })#T]

		class ExpressionSpellingBase[P, F <: RowProduct](spelling :SQLSpelling, inline :Boolean = false)
		                                                (context :SQLContext)
		                                                (implicit params :Parameterization[P, F])
			extends CaseExpression[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P, F] })#T]
		{
			override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SpelledSQL[P, F] =
				if (inline) inlineSpelling(e)(context) else defaultSpelling(e)(context)

			protected def defaultSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext) :SpelledSQL[P, F] =
				e.defaultSpelling(spelling)(context, params)

			protected def inlineSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext) :SpelledSQL[P, F] =
				inline(e)(context) match {
					case Seq() => SpelledSQL("", context, params)
					case columns => columns.reduce(_.sql +: ", " +: _)
				}

			protected def inline(e :SQLExpression[F, LocalScope, _])(context :SQLContext) :Seq[SpelledSQL[P, F]] =
				e.inlineSpelling(spelling)(context, params)
		}

	}




	/** An SQL renderer which delegates to `defaultSpelling` methods of
	  * [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement]],
	  * [[net.noresttherein.oldsql.sql.Query.defaultSpelling Query]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLException]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct]],
	  * [[net.noresttherein.oldsql.schema.Relation.Table.defaultSpelling Table]] and others, doing little by itself
	  * that is not required by the contract of [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
	  * One added feature is ensuring table aliases are unique, by treating `alias` parameters to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.table table]] methods as proposed, rather than required
	  * values. All tables are always aliased, using a generated name if none is given, unless an empty string
	  * is explicitly passed as the alias. An additional cosmetic function is applying preferred capitalization
	  * to various syntactical elements.
	  * @param literals  The capitalization to use for literal expressions (defaults to `LowerCase`).
	  * @param operators The capitalization to use for operators (infix functions such as 'and', defaults to `LowerCase`).
	  * @param functions The capitalization to use for function and procedure names (defaults to `LowerCase`).
	  * @param keywords  The capitalization to use for keywords (defaults to `LowerCase`).
	  * @param aliases   The capitalization to use for aliases given to tables and columns (defaults to `LowerCase`).
	  * @param scope     The syntactic context of expression arguments corresponding to an SQL clause
	  *                  (''select'' clause, ''where'' clause, etc.).
	  * @param isInline  Should multi-column expressions be rendered as individual comma-separated columns rather
	  *                  than tuples or stand-alone expressions? This is used to flatten the tree structure of
	  *                  SQL expressions and format `SQLExpression` instances representing SQL fragments larger
	  *                  than a single expression, for example a whole or fragment of ''select'' or ''group by'' clause.
	  */
	class DefaultSpelling(literals :TextCase = LowerCase, operators :TextCase = LowerCase,
	                      functions :TextCase = LowerCase, keywords :TextCase = LowerCase, aliases :TextCase = LowerCase)
	                     (override val scope :SpellingScope, protected val isInline :Boolean = false)
		extends SQLSpelling
	{
		def this(scope :SpellingScope, textCase :TextCase, isInline :Boolean) =
			this(textCase, textCase, textCase, textCase, textCase)(scope, isInline)

		def this(scope :SpellingScope, isInline :Boolean) = this(scope, LowerCase, isInline)
		def this(scope :SpellingScope) = this(scope, LowerCase, false)

		protected def copy(scope :SpellingScope = this.scope, isInline :Boolean = this.isInline) :SQLSpelling =
			new DefaultSpelling(literals, operators, functions, keywords, aliases)(scope, isInline)

		override def inline    :SQLSpelling = if (isInline) this else copy(scope, true)
		override def inSelect  :SQLSpelling = if (scope == SelectScope) this else copy(SelectScope, true)
		override def inFrom    :SQLSpelling = if (scope == FromScope) this else copy(FromScope, false)
		override def inWhere   :SQLSpelling = if (scope == WhereScope) this else copy(WhereScope, false)
		override def inGroupBy :SQLSpelling = if (scope == GroupByScope) this else copy(GroupByScope, true)
		override def inHaving  :SQLSpelling = if (scope == HavingScope) this else copy(HavingScope, false)
		override def inInsert  :SQLSpelling = if (scope == InsertScope) this else copy(InsertScope, true)
		override def inUpdate  :SQLSpelling = if (scope == UpdateScope) this else copy(UpdateScope, false)

		override def in(scope :SpellingScope) :SQLSpelling = scope match {
			case SelectScope  => inSelect
			case FromScope    => inFrom
			case WhereScope   => inWhere
			case GroupByScope => inGroupBy
			case HavingScope  => inHaving
			case InsertScope  => inInsert
			case UpdateScope  => inUpdate
			case _ => new DefaultSpelling(scope, false)
		}

		override def apply[P, F <: RowProduct, V]
                          (e :SQLExpression[F, LocalScope, V])
                          (context :SQLContext, params: Parameterization[P, F]) :SpelledSQL[P, F] =
			if (isInline) inlineSpelling(e)(context, params) else defaultSpelling(e)(context, params)


		protected val tableAliasRoot :String = aliases("table")
		protected val selectAliasRoot :String = aliases("select")

		protected def alias(root :String)(context :SQLContext) :String = {
			var i = 1; var alias = root + i
			while (context.tablesReversed.contains(alias)) {
				i += 1; alias = root + i
			}
			alias
		}

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M])(context :SQLContext, params :Parameterization[P, F]) :SpelledSQL[P, F] =
			table match {
				case Aliased(t :Table.*, alias) => this.table(t, alias)(context, params)
				case t :RelVar[M] => this.table(t, t.name)(context, params)
				case _ =>
					//we could try to check if the select returns a single entity to provide a more informative alias
					val default = tableAliasRoot + context.tablesReversed.length
					if (!context.contains(default)) this.table(table, default)(context, params)
					else this.table(table, selectAliasRoot)(context, params)
			}

		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :String)(context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
		{
			val sql = table.defaultSpelling(this)(context, params)
			if (alias.length == 0)
				SpelledSQL(sql.sql, context.join(""), sql.params)
			else {
				val unique =
					if (!context.tablesReversed.contains(alias)) alias
					else this.alias(alias)(context)
				SpelledSQL(sql.sql + (" " + AS + " " + unique), context.join(unique), sql.params)
			}
		}
		//overriden due to Scala's stupid overloading rules
		override def table[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :Option[String])
		                  (context :SQLContext, params :Parameterization[P, F])
				:SpelledSQL[P, F] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))


		override def join[L <: FromSome, R[O] <: MappingAt[O]]
		                 (join :L Join R, clause :String)(context :SQLContext) :SpelledSQL[join.Params, join.Generalized] =
		{
			val left = apply(join.left :join.left.type)(context)
			val right = table(join.right, join.aliasOpt)(left.context, left.params)
			val sql = left.sql + (" " + clause + " ") + right.sql
			val joined = SpelledSQL(sql, right.context, right.params.join[join.Generalized, join.left.Generalized, R])
			if (join.condition == True)
				joined
			else if (useJoinOnClause)
				joined + (" " + ON + " ") + (inWhere(join.condition)(_, _))
			else
				joined && (inWhere(join.condition)(_, _))
		}

		protected def useJoinOnClause :Boolean = true


		override def literal(sql :String) :String = literals(sql)
		override def operator(sql :String) :String = operators(sql)
		override def function(sql :String) :String = functions(sql)
		override def keyword(sql :String) :String = keyword(sql)

		override val NULL       :String = literals("null")
		override val TRUE       :String = literals("true")
		override val FALSE      :String = literals("false")
		override val CONCAT     :String = operators("+")
		override val LIKE       :String = operators("like")
		override val BETWEEN    :String = operators("between")
		override val NOT        :String = operators("not")
		override val AND        :String = operators("and")
		override val OR         :String = operators("or")
		override val UNION      :String = operators("union")
		override val UNION_ALL  :String = operators("union all")
		override val INTERSECT  :String = operators("intersect")
		override val MINUS      :String = operators("minus")
		override val SELECT     :String = keywords("select")
		override val FROM       :String = keywords("from")
		override val WHERE      :String = keywords("where")
		override val GROUP_BY   :String = keywords("group by")
		override val HAVING     :String = keywords("having")
		override val AS         :String = keywords("as")
		override val INNER_JOIN :String = keywords("join")
		override val OUTER_JOIN :String = keywords("outer join")
		override val LEFT_JOIN  :String = keywords("left join")
		override val RIGHT_JOIN :String = keywords("right join")
		override val ON         :String = keywords("on")
		override val INSERT     :String = keywords("insert")
		override val INTO       :String = keywords("into")
		override val VALUES     :String = keywords("values")
		override val UPDATE     :String = keywords("update")
		override val SET        :String = keywords("set")
		override val MERGE      :String = keywords("merge")
		override val DELETE     :String = keywords("delete")
	}

}






/** Implementation of the SQL standard formatting all SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
  * and DML [[net.noresttherein.oldsql.sql.DMLStatement statements]] using
  * a [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy of its
  * [[net.noresttherein.oldsql.sql.StandardSQL!.spelling spelling]] property. Unless overriden, all formatting methods
  * delegate the generation of SQL to the formatted objects themselves in double dispatch, most notably to
  * methods [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLExpression.defaultSpelling]]
  * and [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement.defaultSpelling]].
  * The [[net.noresttherein.oldsql.sql.SQLDialect$ companion]] object to this class extends it, serving
  * as the default dialect used by all methods unless an implicit instance is present.
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]]
  */
class StandardSQL extends SQLDialect {
	private val defaultSpelling :SQLSpelling = new DefaultSpelling(SelectScope)

	/** The effective spelling strategy used by this dialect. All other methods are implemented by delegating
	  * to the appropriate method of this strategy. Standard implementation returns a cached
	  * `new `[[net.noresttherein.oldsql.sql.SQLDialect.DefaultSpelling DefaultSpelling]]([[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.SelectScope SelectScope]])
	  * but subclasses are not restricted to always returning the same instance when overriding this method.
	  */
	def spelling :SQLSpelling = defaultSpelling

	override def apply[R, Y](query :QuerySQL[RowProduct, R])(implicit result :StatementResult[R, Y]) :Cantrip[Y] =
		chant(spelling.spell(query).compose { _ :Any => @~ }, result)

	override def apply[X, R, Y](query :Query[X, R])(implicit result :StatementResult[R, Y]) :Incantation[X, Y] =
		chant(spelling.spell(query), result)
//
//	override def apply[X <: Chain, Y](call :FunctionSQL[RowProduct, GlobalScope, X, Y]) :Cantrip[Y] =
//		chant(spelling.spell(call).compose { _ :Any => @~ }, StatementResult.SingleResult(call.readForm))

	override def apply[X, Y](statement :DMLStatement[X, Y]) :Incantation[X, Y] =  //todo: CallableStatement
		chant(spelling.spell(statement), statement.result)

	override def apply[X, Y](statement :Call[X, Y]) :Incantation[X, Y] = {
		val sql = spelling.spell(statement)
		Incantation.call(sql.sql.toString)(sql.params.setter, statement.result)
	}

	/** The default factory method creating incantations from complete SQL/DML of the translated statement
	  * and a strategy for reading the required values from the [[java.sql.PreparedStatement PreparedStatement]]
	  * result(s). It is the delegation target of all `apply` methods in this dialect except for
	  * [[net.noresttherein.oldsql.sql.Call Call]] statements which require a specific incantation implementation
	  * creating a [[java.sql.CallableStatement CallableStatement]] instead of regular `PreparedStatement`
	  * in order to read (optional) ''OUT'' parameters.
	  */
	protected def chant[X, Y](sql :SpelledSQL[X, RowProduct], result :StatementResult[Nothing, Y]) :Incantation[X, Y] =
		Incantation(sql.sql.toString)(sql.params.setter, result)

//	private[this] val action = new StatementVisitor[Incantation] with CaseStatement[Incantation] {
//		override def statement[X, Y](stmt :DMLStatement[X, Y]) = StandardSQL.this(stmt)
//	}
}



/** The default formatter of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and
  * DML [[net.noresttherein.oldsql.sql.DMLStatement statements]], converting them
  * to [[net.noresttherein.oldsql.sql.Incantation incantations]] executing
  * [[java.sql.PreparedStatement PreparedStatement]]s for their generated textual representations.
  * Generated SQL conforms to the standard for maximum portability and, on principle, defers the translation of
  * each expression and syntactic element to the appropriate class representing it in the abstract syntax tree,
  * most notably methods [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLExpression.defaultSpelling]]
  * and [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement.defaultSpelling]], so minor
  * specific modifications can be performed by extending individual classes in the framework.
  *
  * This is ''not'' an implicit value; instead methods accepting - typically implicitly - a dialect provide
  * it as a default value used if no implicit instance is available.
  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]
  * @see [[net.noresttherein.oldsql.morsels.witness.Maybe Maybe]]
  */
case object StandardSQL extends StandardSQL

