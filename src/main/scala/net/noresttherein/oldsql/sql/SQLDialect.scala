package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.exceptions.InseparableExpressionException
import net.noresttherein.oldsql.morsels.TextCase
import net.noresttherein.oldsql.morsels.TextCase.LowerCase
import net.noresttherein.oldsql.schema.{ColumnMapping, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.NamedRelation
import net.noresttherein.oldsql.schema.Table.Aliased
import net.noresttherein.oldsql.slang.OptionGuardExtension
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.Incantation.Cantrip
import net.noresttherein.oldsql.sql.Select.{Intersect, Minus, SetOperator, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.{DefaultSpelling, SQLSpelling}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.{FromScope, GroupByScope, HavingScope, InsertScope, SelectScope, UpdateScope, WhereScope, WithScope}
import net.noresttherein.oldsql.sql.SQLExpression.{CaseExpression, ExpressionMapper, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.{JoinedRelation, JoinedTable, QuerySQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.{GroupingSpellingContext, SpellingRedactor}






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

		/** Export, from the point of view of `mapping`, columns of `component`, which are applicable to this
		  *  operation type. These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]]
		  *  list ''without'' the [[net.noresttherein.oldsql.OperationType.Prohibited Prohibited]] buff after exporting.
		  * @see [[net.noresttherein.oldsql.OperationType.defaultColumns]]
		  */
		def columns[O](mapping :MappingAt[O], component :MappingAt[O]) :Unique[ColumnMapping[_, O]]

		/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
		  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
		  * the [[net.noresttherein.oldsql.OperationType.NonDefault NonDefault]] buff.
		  * @see [[net.noresttherein.oldsql.OperationType.columns]]
		  */
		def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

		/** Export, from the point of view of `mapping`, columns of `component`, which are used by default in this
		  * operation type. These are all columns from `component.`[[net.noresttherein.oldsql.schema.Mapping.columns columns]]
		  * list ''without'' the [[net.noresttherein.oldsql.OperationType.NonDefault NonDefault]] buff after exporting.
		  * @see [[net.noresttherein.oldsql.OperationType.columns]]
		  */
		def defaultColumns[O](mapping :MappingAt[O], component :MappingAt[O]) :Unique[ColumnMapping[_, O]]

		def writeForm[S, O](mapping :RefinedMapping[S, O]) :SQLWriteForm[S]
		def writeForm[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S]
		def writeForm[S, O](mapping :MappingAt[O], component :RefinedMapping[S, O]) :SQLWriteForm[S]
	}


	object SpellingScope {

		private class OperationTypeScope(op :OperationType, override val toString :String) extends SpellingScope {
			override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = op.columns(mapping)
			override def columns[O](mapping :MappingAt[O], component :MappingAt[O]) :Unique[ColumnMapping[_, O]] =
				op.columns(mapping, component)

			override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] =
				op.defaultColumns(mapping)

			override def defaultColumns[O](mapping :MappingAt[O], component :MappingAt[O]) :Unique[ColumnMapping[_, O]] =
				op.defaultColumns(mapping, component)

			override def writeForm[S, O](mapping :RefinedMapping[S, O]) :SQLWriteForm[S] = SQLWriteForm.empty

			override def writeForm[S, O](mapping :MappingAt[O], component :RefinedMapping[S, O]) :SQLWriteForm[S] =
				SQLWriteForm.empty

			override def writeForm[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]])
					:SQLWriteForm[S] =
				SQLWriteForm.empty
		}

		private class WriteOperationScope(op :WriteOperationType, override val toString :String)
			extends OperationTypeScope(op, toString)
		{
			override def writeForm[S, O](mapping :RefinedMapping[S, O]) = op.form(mapping)

			override def writeForm[S, O](mapping :MappingAt[O], component :RefinedMapping[S, O]) =
				op.form(mapping, component)

			override def writeForm[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) =
				op.form(mapping, components)
		}

		val WithScope    :SpellingScope = new OperationTypeScope(SELECT, "WITH")
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
	  * expressions refer to table [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]]
	  * and [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters, which must be properly resolved into,
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
		def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] =
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
		def spell[P, V](query :Query[P, V]) :SpelledSQL[P] = {
			val prefix = apply[@~](query.withClause)(newContext, Parameterization.paramless)
			if (prefix.isEmpty)
				apply(query)(prefix.context.reset())
			else {
				val ctx = prefix.context.reset[P]()
				val adapted = SpelledSQL(prefix.sql, ctx, prefix.setter compose { _ :P => @~ })
				adapted +: " " +: apply(query)(ctx)
			}
		}

		/** Renders as a stand alone SQL query a top-level, parameterless ''select'' expression. Defaults to
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.paramless paramless]]`(query)(`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.newContext newContext]]`)`.
		  * @tparam V      the nominal value type of the whole ''select'' clause of the query (or unified ''select''
		  *                clauses for compound ''selects'').
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all bound and unbound parameters present in the query.
		  */
		def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~] = {
			val prefix = apply[@~](query.withClause)(newContext, Parameterization.paramless)
			if (prefix.isEmpty)
				paramless(query)(prefix.context)
			else
				prefix +: " " +: paramless(query)(prefix.context)
		}

		/** Renders as SQL an optional ''with'' clause preceding a query or a statement. If the passed clause
		  * does not contain any [[net.noresttherein.oldsql.sql.CommonTableExpression common table expressions]],
		  * an empty string will be returned.
		  */
		def apply[P](ctes :WithClause)(context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
			if (ctes.isEmpty)
				SpelledSQL(context)
			else {
				val ctx = context.reset[@~]() //todo: WithParam (new, not the JoinParam)
				val res = WITH_ +: ctes.map { cte =>
					(cte.name + _AS_ + "(") +: (paramless(cte.query)(ctx) + ")")
				}.reduce(_ + ", " + _)
				SpelledSQL(res.sql, context, res.setter compose { _:P => @~ })
			}

		/** Renders as SQL a parameterless, [[net.noresttherein.oldsql.sql.ast.QuerySQL.GroundQuery ground]] ''select''
		  * expression for use as a part of a larger SQL/DML expression, either as a dependent select
		  * or in a compound ''select''.
		  * Defaults to `query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.topSpelling topSpelling]].
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
		  * @return a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *         all potential [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameters
		  *         present in the query.
		  */ //consider: removing context parameter, as we do not require Parameterization anyway
		//this cannot be apply, as then Query[F, V] doesn't overload to the method for generic SQLExpression
		def paramless[V](query :QuerySQL[RowProduct, V])(context :SQLContext[@~]) :SpelledSQL[@~] =
			apply(query)(Dual, context, Parameterization.paramless)
//			query.topSpelling(this, context)

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
		def apply[P, V](query :Query[P, V])(context :SQLContext[P]) :SpelledSQL[P] =
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
		  * @param params ''in/out'' parameter carrying necessary information about all (relevant)
		  *               [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] and
		  *               [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of the formatted SQL.
		  *               In its main role, it is a list of
		  *               [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] setting the parameters
		  *               on a [[java.sql.PreparedStatement PreparedStatement]]: it is assumed to contain forms
		  *               for the whole prefix preceding the fragment represented by `e` in the complete,
		  *               yet-to-be-formatted SQL expression. While this list is unusable by this method,
		  *               forms for all parameters in `e` should be appended to it and returned as the parameterization
		  *               included in `SpelledSQL`. As its second function, it provides getter functions `P => X`
		  *               for all ''unbound'' parameters `X` in the scope of the formatted expression, given
		  *               the [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]
		  *               of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *               representing them. These getters are in turn necessary to adapt the forms of individual
		  *               expression to the type `P` of the parameter(s) of the final SQL.
		  *               Note that this means that rendering of a ''select'' clause requires the parameterization
		  *               created by the rendering of its ''from'' clause, creating non-linear concatenation
		  *               (possible because ''from'' clauses are completely independent of the ''select'' clauses).
		  *               Callers may 'cheat' and omit information about parameters proven to be unused by `e`
		  *               and combine them later with `SpelledSQL` returned by this method.
		  * @return        a formatted SQL string with a [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting
		  *                all bound and unbound parameters present in the query.
		  */
		def apply[P, F <: RowProduct, V]
                 (e :SQLExpression[F, LocalScope, V])
                 (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			defaultSpelling(e)(from, context, params)

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
		  * @param params ''in/out'' parameter carrying necessary information about all (relevant)
		  *               [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] and
		  *               [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters of the formatted SQL.
		  *               In its main role, it is a list of
		  *               [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] setting the parameters
		  *               on a [[java.sql.PreparedStatement PreparedStatement]]: it is assumed to contain forms
		  *               for the whole prefix preceding the fragment represented by `e` in the complete,
		  *               yet-to-be-formatted SQL expression. While this list is unusable by this method,
		  *               forms for all parameters in `e` should be appended to it and returned as the parameterization
		  *               included in `SpelledSQL`. As its second function, it provides getter functions `P => X`
		  *               for all ''unbound'' parameters `X` in the scope of the formatted expression, given
		  *               the [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]
		  *               of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression
		  *               representing them. These getters are in turn necessary to adapt the forms of individual
		  *               expression to the type `P` of the parameter(s) of the final SQL.
		  *               Note that this means that rendering of a ''select'' clause requires the parameterization
		  *               created by the rendering of its ''from'' clause, creating non-linear concatenation
		  *               (possible because ''from'' clauses are completely independent of the ''select'' clauses).
		  *               Callers may 'cheat' and omit information about parameters proven to be unused by `e`
		  *               and combine them later with `SpelledSQL` returned by this method.
		  * @return        a list of formatted SQL strings for every column in this expression in the exact same order.
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                included in the last `SpelledSQL` of the list represent the complete state,
		  *                taking into account all previous columns and is the same as if the expression was formatted
		  *                using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply[P,F<:RowProduct,V](e:SQLExpression[F,LocalScope,V])* apply]]
		  *                method.
		  */
		@throws[InseparableExpressionException]("if the expression cannot be separated into individual column strings, " +
		                                        "for example a multi-column SQL select.")
		def explode[P, F <: RowProduct, V]
		           (e :SQLExpression[F, LocalScope, V])
		           (from :F, context :SQLContext[P], params :Parameterization[P, F]) :Seq[SpelledSQL[P]] =
			explodedSpelling(e)(from, context, params)

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
		  * @param params   the parameterization context of [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters of `F`, but ''not'' including any write forms.
		  * @return         the formatted SQL for `function(arg)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]]
		  *                 for all parameters used by `arg`.
		  */ //consider: removing it and short wiring from AggregateSQL to AggregateFunction - seems like an exception made for no reason
		def apply[P, F <: RowProduct](function :AggregateFunction, distinct :Boolean)
		                             (arg :ColumnSQL[F, LocalScope, _])
		                             (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			function.defaultSpelling(this)(arg, distinct)(from, context, params)

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
		  * @param params   the parameterization context of [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                 parameters of `F`, but ''not'' including any write forms.
		  * @return         the formatted SQL for `function(args)` (pseudocode), with the same
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]]
		  *                 (or at least an instance with unmodified relation indexing) and
		  *                 [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]]
		  *                 setting all parameters used by `args` by the use of `params`.
		  */
		def apply[P, F <: RowProduct, X, Y](function :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
		                                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			function.defaultSpelling(this)(args)(context, params)


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
		  *         The [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] included contains
		  *         a [[net.noresttherein.oldsql.schema.SQLWriteForm write form]] for all formatted expressions
		  *         (but not necessarily for all ''join/where/having'' conditions present in the instance).
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere]]
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.spell]]
		  */
		def apply[P](from :RowProduct)
		            (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
			defaultSpelling(from)(context, params)

		/** Renders as an SQL fragment full ''from'' and ''where'' clauses of a ''select''. The process recursively calls
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply(from:RowProduct)* apply(from)(context, params)]]
		  * (which by default will delegate to
		  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct.defaultSpelling]]) and simultaneously
		  * appends ''join'' clauses (or simply lists the tables) and builds
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]] for the rest
		  * of the SQL ''select'' to which `from` belongs. Recursion stops and backtracks when either
		  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.Subselect Subselect]]
		  * is encountered, marking the start of the ''from'' clause of the most nested ''select''. From that point,
		  * table aliases and join conditions are added to `context`.
		  * This method is invoked from [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]
		  * and the latter should be preferred as the entry point.
		  * @param from    the ''from'' clause to format.
		  * @param context the namespace containing the aliases of all tables in scope of `from` due to belonging
		  *                to an outer ''select'', or a fresh instance if the formatted ''select'' is the top-level
		  *                expression is a top select.
		  * @param params  a facade to the type of parameters of the whole statement/query, returning an accessor
		  *                `P => X` for any [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]
		  *                of value type `X`, allowing the expressions for unbound parameters to be translated
		  *                into [[net.noresttherein.oldsql.schema.SQLWriteForm forms]].
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.groupByHaving]]
		  */
		def fromWhere[P](from :FromClause)
		                (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
		{
			val spelledFrom = inFrom(from)(context, params)
			if (spelledFrom.context.whereReversed.isEmpty)
				spelledFrom
			else {//the forms for the where clause are already included by `from`
				val whereSQL = spelledFrom.context.whereReversed.reduceLeft((_1, _2) => _2 +: _AND_ +: _1)
				val sql = spelledFrom.sql + _WHERE_ + whereSQL.sql
				SpelledSQL(sql, spelledFrom.context.reset(), spelledFrom.setter + whereSQL.setter)
			}
		}

		/** Renders as an SQL fragment full ''from'', ''where'', ''group by'' and ''having'' clauses of a ''select''.
		  * This method is invoked from [[net.noresttherein.oldsql.sql.RowProduct.spell RowProduct.spell]]
		  * and the latter should be preferred as the entry point. The process recursively calls
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.apply(from:RowProduct)* apply(from)(context, params)]]
		  * (which by default will delegate to
		  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct.defaultSpelling]]) and simultaneously
		  * appends ''join'' clauses (or simply lists the tables) and builds `SQLContext` for the rest
		  * of the SQL ''select'' to which `from` belongs. When the recursion encounters the
		  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] instance, its `defaultSpelling` calls
		  * [[net.noresttherein.oldsql.sql.FromClause.spell spell]] again on the grouped clause, which forwards the call
		  * to [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere fromWhere]] method of this instance.
		  * @param from    the ''from'' clause with a ''group by'' clause to format.
		  * @param context the namespace containing the aliases of all tables in scope of `from` due to belonging
		  *                to an outer ''select'', or a fresh instance if the formatted ''select'' is the top-level
		  *                expression is a top select.
		  * @param params  a facade to the type of parameters of the whole statement/query, returning an accessor
		  *                `P => X` for any [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]
		  *                of value type `X`, allowing the expressions for unbound parameters to be translated
		  *                into [[net.noresttherein.oldsql.schema.SQLWriteForm forms]].
		  * @see [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.fromWhere]]
		  */
		def groupByHaving[P](from :GroupByClause)
		                    (context :SQLContext[P], params :Parameterization[P, from.Generalized]) :SpelledSQL[P] =
		{
			val spelledGroupBy = inGroupBy(from)(context, params)
			if (from.filter == True) spelledGroupBy
			else spelledGroupBy + _HAVING_ + inHaving(from.filter)(from.generalized, spelledGroupBy.context, params)
		}

		/** SQL fragment for an empty ''from'' clause. Defaults to an empty string, leading to rendering a ''select''
		  * without a ''from'' clause.
		  */
		def emptyFrom[P](context :SQLContext[P]) :SpelledSQL[P] = SpelledSQL(context)

		/** Creates an SQL fragment adding a single table to a non-empty ''from'' clause.
		  * This can happen both using a ''join'' clause (and optionally an accompanying ''on'' clause),
		  * and as simple comma-separated concatenation.
		  * If [[net.noresttherein.oldsql.sql.Join.condition join.condition]] is not used in the the returned SQL,
		  * it is added to the pending
		  * [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereReversed where]] clause
		  * in the `SQLContext` included in the returned SQL.
		  */
		def join[P, L <: FromSome, R[O] <: MappingAt[O]]
		        (join :L Join R, clause :String)(context :SQLContext[P], params :Parameterization[P, join.Generalized])
					:SpelledSQL[P]

		/** Formats a single table (including ''derived tables'', that is ''select'' expressions) included in a ''from''
		  * clause of a ''select''. This overloaded method variant is called when no alias for the table
		  * has been explicitly specified by the application and one may need to be picked in order to ensure uniqueness
		  * of aliases for all tables listed in `context`.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam M      the type constructor of the [[net.noresttherein.oldsql.schema.Mapping Mapping]]
		  *                used to map individual rows of `table`.
		  * @param table   a relation permissible in the ''from'' clause.
		  * @param context spelling context containing all preceding elements of the formatted ''from'' clause,
		  *                as well as aliases of tables from enclosing ''selects'' which remain in scope
		  *                for dependent ''selects''.
		  * @param params  parameterization providing access to the individual parameters in `P`
		  *                of the whole formatted query which can be used by the expressions.
		  * @return        in the most common cases of actual tables and stored views
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		def table[P, M[O] <: MappingAt[O]]
		         (table :Table[M])(context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P]

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
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		def table[P, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :String)
		         (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P]

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
		  *                ([[net.noresttherein.oldsql.schema.RelVar RelVar]] instances), a string containing
		  *                the table/view name together with an optional alias. Included
		  *                [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.context context]] contains
		  *                an additional entry in respect to `context`, while
		  *                the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter write form]] sets any
		  *                bound and unbound parameters used by the table if it is
		  *                a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
		  *                and is empty for base tables.
		  */
		def table[P, M[O] <: MappingAt[O]]
		         (table :Table[M], alias :Option[String])
		         (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))

		/** Spells a grouping expression from a ''group by'' clause exposed as a pseudo relation.
		  * @tparam P      the type of the parameter or parameters of the complete query expression containing
		  *                the formatted ''from'' clause; typically, it will be
		  *                `F#`[[net.noresttherein.oldsql.sql.RowProduct.Params]], but on the top level such constraints
		  *                do not exist.
		  * @tparam F      the `RowProduct` listing all preceding tables in the ''from'' clause as well as tables
		  *                inherited from enclosing ''select''(s) and remaining in the scope of the formatted ''select''.
		  */
		def grouping[P, F <: RowProduct, M[O] <: MappingAt[O]]
		            (grouping :GroupingRelation[F, M])
		            (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
			grouping.defaultSpelling(this)(from, context, params)


		/** Renders as SQL a reference to column `column` of the relation `table` in the ''from'' clause
		  * of the formatted expression. This method terminates the recursion and simply returns the column name
		  * qualified with the alias for the given `JoinedTable`, as specified by `context` - unless the alias
		  * is an empty string, in which case the column name will not be qualified. The `table` argument
		  * must refer to a [[net.noresttherein.oldsql.schema.Table Table]] relation (in official SQL
		  * terminology this includes ''stored views'' and ''select'' expressions as well as ''base tables'') -
		  * if it is some kind of synthetic pseudo relation existing only in the framework, an exception will be thrown.
		  *///context must correspond to RowProduct the table is based on (same number of tables)
		@throws[IllegalArgumentException]("if the relation at the given index is synthetic and does not correspond " +
		                                  "to an actual table in the FROM clause.")
		@throws[IndexOutOfBoundsException]("if the context doesn't correspond to from clause F.")
		def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
		          (table :JoinedTable[O, T], column :ColumnMapping[_, O])
		          (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			table.table.spell(this)(table, table.export.export(column))(from, context, params)


		/** Dedicated names for operators combining two queries
		  * into a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL compound select]].
		  */
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
		  * for [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expressions, but can have
		  * also other effects. The scope is changed with [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.in in]]
		  * method or dedicated properties as the formatted expression/statement is recursively inspected.
		  */
		def scope :SpellingScope

		/** Switches spelling into an inline mode, in which multi-column expressions are never rendered as tuples,
		  * but as comma-separated columns (distinct expressions) without any delimiters. It is used when formatting
		  * tuple expressions of ''select'' clauses and function call arguments, but possibly also others.
		  * An ''inline'' speller should delegate to
		  * [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]] rather than
		  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]] methods and be equivalent
		  * to using [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.explode this.explode]].
		  */
		def inline    :SQLSpelling

		/** Spelling instance to use for rendering ''with'' clauses. */
		def inWith    :SQLSpelling
		/** Spelling instance to use for rendering ''select'' clauses. */
		def inSelect  :SQLSpelling //= in(SelectScope)
		/** Spelling instance to use for rendering (proper) ''from'' clauses. */
		def inFrom    :SQLSpelling //= in(FromScope)
		/** Spelling instance to use for rendering ''where'' clauses. */
		def inWhere   :SQLSpelling //= in(WhereScope)
		/** Spelling instance to use for rendering ''group by'' clauses. */
		def inGroupBy :SQLSpelling //= in(GroupByScope)
		/** Spelling instance to use for rendering ''having'' clauses. */
		def inHaving  :SQLSpelling //= in(HavingScope)
		/** Spelling instance to use for rendering ''values'', ''set'' and other dedicated clauses of ''insert'' statements. */
		def inInsert  :SQLSpelling //= in(InsertScope)
		/** Spelling instance to use for rendering ''set'' clauses of SQL ''update'' statements.*/
		def inUpdate  :SQLSpelling //= in(UpdateScope)

		def in(scope :SpellingScope) :SQLSpelling = scope match {
			case WithScope    => inWith
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

		def NULL         :String = literal("null")
		def TRUE         :String = literal("true")
		def FALSE        :String = literal("false")
		def CONCAT       :String = operator("+")
		def LIKE         :String = operator("like")
		def BETWEEN      :String = operator("between")
		def NOT          :String = operator("not")
		def AND          :String = operator("and")
		def OR           :String = operator("or")
		def UNION        :String = operator("union")
		def UNION_ALL    :String = operator("union all")
		def INTERSECT    :String = operator("intersect")
		def MINUS        :String = operator("minus")
		def WITH         :String = keyword("with")
		def SELECT       :String = keyword("select")
		def DISTINCT     :String = keyword("distinct")
		def FROM         :String = keyword("from")
		def WHERE        :String = keyword("where")
		def GROUP_BY     :String = keyword("group by")
		def HAVING       :String = keyword("having")
		def AS           :String = keyword("as")
		def INNER_JOIN   :String = keyword("join")
		def OUTER_JOIN   :String = keyword("outer join")
		def LEFT_JOIN    :String = keyword("left join")
		def RIGHT_JOIN   :String = keyword("right join")
		def ON           :String = keyword("on")
		def INSERT       :String = keyword("insert")
		def INTO         :String = keyword("into")
		def VALUES       :String = keyword("values")
		def UPDATE       :String = keyword("update")
		def SET          :String = keyword("set")
		def MERGE        :String = keyword("merge")
		def DELETE       :String = keyword("delete")

		def _NULL_       :String = " " + NULL + " "
		def _TRUE_       :String = " " + TRUE + " "
		def _FALSE_      :String = " " + FALSE + " "
		def _CONCAT_     :String = " " + CONCAT + " "
		def _LIKE_       :String = " " + LIKE + " "
		def _BETWEEN_    :String = " " + BETWEEN + " "
		def _NOT_        :String = " " + NOT + " "
		def _AND_        :String = " " + AND + " "
		def _OR_         :String = " " + OR + " "
		def _UNION_      :String = " " + UNION + " "
		def _UNION_ALL_  :String = " " + UNION_ALL + " "
		def _INTERSECT_  :String = " " + INTERSECT + " "
		def _MINUS_      :String = " " + MINUS + " "
		def _SELECT_     :String = " " + SELECT + " "
		def _FROM_       :String = " " + FROM + " "
		def _WHERE_      :String = " " + WHERE + " "
		def _GROUP_BY_   :String = " " + GROUP_BY + " "
		def _HAVING_     :String = " " + HAVING + " "
		def _AS_         :String = " " + AS + " "
		def _INNER_JOIN_ :String = " " + INNER_JOIN + " "
		def _OUTER_JOIN_ :String = " " + OUTER_JOIN + " "
		def _LEFT_JOIN_  :String = " " + LEFT_JOIN + " "
		def _RIGHT_JOIN_ :String = " " + RIGHT_JOIN + " "
		def _ON_         :String = " " + ON + " "
		def _INTO_       :String = " " + INTO + " "
		def _VALUES_     :String = " " + VALUES + " "
		def _SET_        :String = " " + SET + " "
		def WITH_        :String = WITH + " "
		def SELECT_      :String = SELECT + " "
		def INSERT_      :String = INSERT + " "
		def UPDATE_      :String = UPDATE + " "
		def MERGE_       :String = MERGE + " "
		def DELETE_      :String = DELETE + " "

		/** A hot spot allowing subclasses to inject specialized `SQLContext` implementations. */
		def newContext :SQLContext[Any] = SQLContext()


		//forwarding methods.

		/** A forwarder method to
		  * [[net.noresttherein.oldsql.sql.RowProduct.groupingSpellingContext RowProduct.groupingSpellingContext]].
		  */
		@inline protected final def groupingSpellingContext[P]
		                            (from :RowProduct)
		                            (position :Int, context :SQLContext[P], params :Parameterization[P, from.Generalized])
				:GroupingSpellingContext[P] =
			from.groupingSpellingContextForwarder(position, context, params)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] =
			statement.defaultSpellingForwarder(this)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.Query.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P](e :Query[P, _])(context :SQLContext[P]) :SpelledSQL[P] =
			e.defaultSpelling(this)(context)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExecutable.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct, X, Y]
		                                           (f :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
		                                           (implicit context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			f.defaultSpelling(this)(args)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExecutable.paramSpelling paramSpelling]]. */
		@inline protected final def paramSpelling[X, Y](f :SQLExecutable[X, Y]) :SpelledSQL[X] =
			f.paramSpellingForwarder(this)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.AggregateFunction.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct]
		                            (f :AggregateFunction)(arg :ColumnSQL[F, LocalScope, _], distinct :Boolean = false)
		                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			f.defaultSpelling(this)(arg, distinct)(from, context, params)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P, F <: RowProduct]
		                                           (e :SQLExpression[F, LocalScope, _])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			e.defaultSpelling(this)(from, context, params)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.SQLExpression.explodedSpelling explodedSpelling]]. */
		protected final def inlineSpelling[P, F <: RowProduct]
		                                  (e :SQLExpression[F, LocalScope, _])
		                                  (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			e.explodedSpelling(this)(from, context, params) match {
				case Seq() => SpelledSQL(context)
				case columns => columns.reduce(_ + ", " + _)
			}

		@inline protected final def explodedSpelling[P, F <: RowProduct]
		                                            (e :SQLExpression[F, LocalScope, _])
		                                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] =
			e.explodedSpelling(this)(from, context, params)

		/** A forwarder method to [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]]. */
		@inline protected final def defaultSpelling[P](from :RowProduct)
		                                              (context :SQLContext[P],
		                                               params :Parameterization[P, from.Generalized])
				:SpelledSQL[P] =
			from.defaultSpelling(this)(context, params)

		@inline protected final def defaultSpelling[P, M[O] <: MappingAt[O]]
		                                           (table :Table[M])
		                                           (context :SQLContext[P], params :Parameterization[P, RowProduct])
				:SpelledSQL[P] =
			table.defaultSpelling(this)(context, params)

		@inline protected final def defaultSpelling[P, F <: RowProduct, M[O] <: MappingAt[O]]
		                                           (grouping :GroupingRelation[F, M])
		                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			grouping.defaultSpelling(this)(from, context, params)
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
			override def spell[P, V](statement :DMLStatement[P, V]) :SpelledSQL[P] = author.spell(statement)
			override def spell[P, V](query :Query[P, V]) :SpelledSQL[P] = author.spell(query)
			override def spell[V](query :QuerySQL[RowProduct, V]) :SpelledSQL[@~] = author.spell(query)

			override def paramless[V](query :QuerySQL[RowProduct, V])(context :SQLContext[@~]) :SpelledSQL[@~] =
				author.paramless(query)(context)


			override def apply[P](ctes :WithClause)
			                     (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
				author.apply(ctes)(context, params)

			override def apply[P, V](query :Query[P, V])(context :SQLContext[P]) :SpelledSQL[P] =
				author.apply(query)(context)

			override def apply[P, F <: RowProduct, V]
			                  (e :SQLExpression[F, LocalScope, V])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.apply(e)(from, context, params)

			override def explode[P, F <: RowProduct, V]
			                    (e :SQLExpression[F, LocalScope, V])
			                    (from :F, context :SQLContext[P], params :Parameterization[P, F]) :Seq[SpelledSQL[P]] =
				author.explode(e)(from, context, params)

			override def apply[P](from :RowProduct)(context :SQLContext[P], params :Parameterization[P, from.Generalized])
					:SpelledSQL[P] =
				author.apply(from)(context, params)

			override def apply[P, F <: RowProduct]
			                  (function :AggregateFunction, distinct :Boolean)(arg :ColumnSQL[F, LocalScope, _])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.apply(function, distinct)(arg)(from, context, params)

			override def apply[P, F <: RowProduct, X, Y]
			                  (function :SQLExecutable[X, Y])(args :SQLExpression[F, LocalScope, X])
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.apply(function)(args)(from, context, params)


			override def table[P, M[A] <: MappingAt[A]]
			                  (table :Table[M])
			                  (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
				author.table(table)(context, params)

			override def table[P, M[A] <: MappingAt[A]]
			                  (table :Table[M], alias :String)
			                  (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
				author.table(table, alias)(context, params)

			override def table[P, M[A] <: MappingAt[A]]
			                  (table :Table[M], alias :Option[String])
			                  (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
				author.table(table, alias)(context, params)

			override def grouping[P, F <: RowProduct, M[O] <: MappingAt[O]]
			                     (grouping :GroupingRelation[F, M])
			                     (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.grouping(grouping)(from, context, params)

			override def column[P, O <: RowProduct, F <: O, T[A] <: MappingAt[A]]
			                   (table :JoinedTable[O, T], column :ColumnMapping[_, O])
			                   (from :F, context :SQLContext[P], params :Parameterization[P, F]) :SpelledSQL[P] =
				author.column(table, column)(from, context, params)

			override def fromWhere[P](from :FromClause)
			                         (context :SQLContext[P], params :Parameterization[P, from.Generalized])
					:SpelledSQL[P] =
				author.fromWhere(from)(context, params)

			override def groupByHaving[P](from :GroupByClause)
			                             (context :SQLContext[P], params :Parameterization[P, from.Generalized])
					:SpelledSQL[P] =
				author.groupByHaving(from)(context, params)

			override def emptyFrom[P](context :SQLContext[P]) :SpelledSQL[P] = author.emptyFrom(context)

			override def join[P, L <: FromSome, R[A] <: MappingAt[A]]
			                 (join :L Join R, clause :String)
			                 (context :SQLContext[P], params :Parameterization[P, join.Generalized]) :SpelledSQL[P] =
				author.join(join, clause)(context, params)


			override def apply(operator :SetOperator) :String = author.apply(operator)

			override def literal(sql :String) :String  = author.literal(sql)
			override def operator(sql :String) :String = author.operator(sql)
			override def function(sql :String) :String = author.function(sql)
			override def keyword(sql :String) :String  = author.keyword(sql)

			override def scope     :SpellingScope = author.scope
			override def inline    :SQLSpelling   = author.inline
			override def inWith    :SQLSpelling   = author.inWith
			override def inSelect  :SQLSpelling   = author.inSelect
			override def inFrom    :SQLSpelling   = author.inFrom
			override def inWhere   :SQLSpelling   = author.inWhere
			override def inGroupBy :SQLSpelling   = author.inGroupBy
			override def inHaving  :SQLSpelling   = author.inHaving
			override def inInsert  :SQLSpelling   = author.inInsert
			override def inUpdate  :SQLSpelling   = author.inUpdate
			override def in(scope :SpellingScope) :SQLSpelling = author.in(scope)

			override def NULL        :String = author.NULL
			override def TRUE        :String = author.TRUE
			override def FALSE       :String = author.FALSE
			override def CONCAT      :String = author.CONCAT
			override def LIKE        :String = author.LIKE
			override def BETWEEN     :String = author.BETWEEN
			override def NOT         :String = author.NOT
			override def AND         :String = author.AND
			override def OR          :String = author.OR
			override def UNION       :String = author.UNION
			override def UNION_ALL   :String = author.UNION_ALL
			override def INTERSECT   :String = author.INTERSECT
			override def MINUS       :String = author.MINUS
			override def WITH        :String = author.WITH
			override def SELECT      :String = author.SELECT
			override def DISTINCT    :String = author.DISTINCT
			override def FROM        :String = author.FROM
			override def WHERE       :String = author.WHERE
			override def GROUP_BY    :String = author.GROUP_BY
			override def HAVING      :String = author.HAVING
			override def AS          :String = author.AS
			override def INNER_JOIN  :String = author.INNER_JOIN
			override def OUTER_JOIN  :String = author.OUTER_JOIN
			override def LEFT_JOIN   :String = author.LEFT_JOIN
			override def RIGHT_JOIN  :String = author.RIGHT_JOIN
			override def ON          :String = author.ON
			override def INSERT      :String = author.INSERT
			override def INTO        :String = author.INTO
			override def VALUES      :String = author.VALUES
			override def UPDATE      :String = author.UPDATE
			override def SET         :String = author.SET
			override def MERGE       :String = author.MERGE
			override def DELETE      :String = author.DELETE

			override def _NULL_       :String = author._NULL_
			override def _TRUE_       :String = author._TRUE_
			override def _FALSE_      :String = author._FALSE_
			override def _CONCAT_     :String = author._CONCAT_
			override def _LIKE_       :String = author._LIKE_
			override def _BETWEEN_    :String = author._BETWEEN_
			override def _NOT_        :String = author._NOT_
			override def _AND_        :String = author._AND_
			override def _OR_         :String = author._OR_
			override def _UNION_      :String = author._UNION_
			override def _UNION_ALL_  :String = author._UNION_ALL_
			override def _INTERSECT_  :String = author._INTERSECT_
			override def _MINUS_      :String = author._MINUS_
			override def _SELECT_     :String = author._SELECT_
			override def _FROM_       :String = author._FROM_
			override def _WHERE_      :String = author._WHERE_
			override def _GROUP_BY_   :String = author._GROUP_BY_
			override def _HAVING_     :String = author._HAVING_
			override def _AS_         :String = author._AS_
			override def _INNER_JOIN_ :String = author._INNER_JOIN_
			override def _OUTER_JOIN_ :String = author._OUTER_JOIN_
			override def _LEFT_JOIN_  :String = author._LEFT_JOIN_
			override def _RIGHT_JOIN_ :String = author._RIGHT_JOIN_
			override def _ON_         :String = author._ON_
			override def _INTO_       :String = author._INTO_
			override def _VALUES_     :String = author._VALUES_
			override def _SET_        :String = author._SET_
			override def WITH_        :String = author.WITH_
			override def SELECT_      :String = author.SELECT_
			override def INSERT_      :String = author.INSERT_
			override def UPDATE_      :String = author.UPDATE_
			override def MERGE_       :String = author.MERGE_
			override def DELETE_      :String = author.DELETE_

			override def newContext :SQLContext[Any] = author.newContext

			override def toString :String = "Redactor(" + author + ")"
		}




		type ExpressionSpelling[P, F <: RowProduct] =
			ExpressionMapper[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P] })#T]

		class ExpressionSpellingBase[P, F <: RowProduct](spelling :SQLSpelling, inline :Boolean = false)
		                                                (from :F, context :SQLContext[P])
		                                                (implicit params :Parameterization[P, F])
			extends CaseExpression[F, ({type T[-S >: LocalScope <: GlobalScope, V] = SpelledSQL[P] })#T]
		{
			override def expression[S >: LocalScope <: GlobalScope, X](e :SQLExpression[F, S, X]) :SpelledSQL[P] =
				if (inline) inlineSpelling(e)(context) else defaultSpelling(e)(context)

			protected def defaultSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext[P]) :SpelledSQL[P] =
				e.defaultSpelling(spelling)(from, context, params)

			protected def inlineSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext[P]) :SpelledSQL[P] =
				explodedSpelling(e)(context) match {
					case Seq() => SpelledSQL("", context)
					case columns => columns.reduce(_ +: ", " +: _)
				}

			protected def explodedSpelling(e :SQLExpression[F, LocalScope, _])(context :SQLContext[P]) :Seq[SpelledSQL[P]] =
				e.explodedSpelling(spelling)(from, context, params)
		}



		trait GroupingSpellingContext[P] {
			type Ungrouped <: FromSome
			type Grouping[O] <: MappingAt[O]

			val grouping :GroupingRelation[Ungrouped, Grouping]
			val from     :Ungrouped
			val context  :SQLContext[P]
			val params   :Parameterization[P, Ungrouped]
		}

		object GroupingSpellingContext {
			def apply[P, F <: FromSome, M[O] <: MappingAt[O]]
			         (grouping :GroupingRelation[F, M])(from :F, context :SQLContext[P], params :Parameterization[P, F])
					:GroupingSpellingContext[P] =
				new Impl(grouping)(from, context, params)

			private class Impl[P, F <: FromSome, M[O] <: MappingAt[O]]
			                  (val grouping :GroupingRelation[F, M])
			                  (val from :F, val context :SQLContext[P], val params :Parameterization[P, F])
				extends GroupingSpellingContext[P]
			{
				type Ungrouped = F
				type Grouping[O] = M[O]
			}
		}
	}




	/** An SQL renderer which delegates to `defaultSpelling` methods of
	  * [[net.noresttherein.oldsql.sql.DMLStatement.defaultSpelling DMLStatement]],
	  * [[net.noresttherein.oldsql.sql.Query.defaultSpelling Query]],
	  * [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling SQLException]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling RowProduct]],
	  * [[net.noresttherein.oldsql.schema.Table.defaultSpelling Table]] and others, doing little by itself
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
		override def inWith    :SQLSpelling = if (scope == WithScope) this else copy(WithScope, false)
		override def inSelect  :SQLSpelling = if (scope == SelectScope) this else copy(SelectScope, true)
		override def inFrom    :SQLSpelling = if (scope == FromScope) this else copy(FromScope, false)
		override def inWhere   :SQLSpelling = if (scope == WhereScope) this else copy(WhereScope, false)
		override def inGroupBy :SQLSpelling = if (scope == GroupByScope) this else copy(GroupByScope, true)
		override def inHaving  :SQLSpelling = if (scope == HavingScope) this else copy(HavingScope, false)
		override def inInsert  :SQLSpelling = if (scope == InsertScope) this else copy(InsertScope, true)
		override def inUpdate  :SQLSpelling = if (scope == UpdateScope) this else copy(UpdateScope, false)

		override def in(scope :SpellingScope) :SQLSpelling = scope match {
			case WithScope    => inWith
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
                          (from :F, context :SQLContext[P], params: Parameterization[P, F]) :SpelledSQL[P] =
			if (isInline) inlineSpelling(e)(from, context, params) else defaultSpelling(e)(from, context, params)


		protected val tableAliasRoot :String = aliases("table")
		protected val selectAliasRoot :String = aliases("select")

		protected def alias(root :String)(context :SQLContext[_]) :String = {
			var i = 1; var alias = root + i
			while (context.tablesReversed.contains(alias)) {
				i += 1; alias = root + i
			}
			alias
		}

		override def table[P, M[O] <: MappingAt[O]]
		                  (table :Table[M])(context :SQLContext[P], params :Parameterization[P, RowProduct])
				:SpelledSQL[P] =
			table match {
				case Aliased(t :Table.*, alias) => this.table(t, alias)(context, params)
				case t :NamedRelation[M] => this.table(table, t.name)(context, params)
				case _ =>
					//we could try to check if the select returns a single entity to provide a more informative alias
					val default = tableAliasRoot + context.tablesReversed.length
					if (!context.contains(default)) this.table(table, default)(context, params)
					else this.table(table, selectAliasRoot)(context, params)
			}

		override def table[P, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :String)
		                  (context :SQLContext[P], params :Parameterization[P, RowProduct]) :SpelledSQL[P] =
		{
			val sql = table.defaultSpelling(this)(context, params)
			if (alias.length == 0)
				SpelledSQL(sql.sql, context.join(""), sql.setter)
			else {
				val unique =
					if (!context.tablesReversed.contains(alias)) alias
					else this.alias(alias)(context)
				SpelledSQL(sql.sql + (_AS_ + unique), context.join(unique), sql.setter)
			}
		}
		//overriden due to Scala's stupid overloading rules
		override def table[P, M[O] <: MappingAt[O]]
		                  (table :Table[M], alias :Option[String])
		                  (context :SQLContext[P], params :Parameterization[P, RowProduct])
				:SpelledSQL[P] =
			alias.mapOrElse(this.table(table, _)(context, params), this.table(table)(context, params))


		override def join[P, L <: FromSome, R[O] <: MappingAt[O]]
		                 (join :L Join R, clause :String)
		                 (context :SQLContext[P], params :Parameterization[P, join.Generalized]) :SpelledSQL[P] =
		{
			val left = apply(join.left :join.left.type)(context, params.left)
			val right = join.last.spell(join, left.context, params)(this)// table(join.right, join.aliasOpt)(left.context, params)
			val sql = left.sql + (" " + clause + " ") + right.sql
			val joined = SpelledSQL(sql, right.context, left.setter + right.setter)
			if (join.condition == True)
				joined
			else if (useJoinOnClause)
				joined + _ON_ + inWhere(join.condition :GlobalBoolean[join.Self])(joined.context, params)
			else {
				val context = joined && inWhere(join.condition)(join.self, joined.context, params)
				SpelledSQL(joined.sql, context, joined.setter)
			}
		}

		protected def useJoinOnClause :Boolean = true


		override def literal(sql :String) :String = literals(sql)
		override def operator(sql :String) :String = operators(sql)
		override def function(sql :String) :String = functions(sql)
		override def keyword(sql :String) :String = keyword(sql)

		override val NULL         :String = literals("null")
		override val TRUE         :String = literals("true")
		override val FALSE        :String = literals("false")
		override val CONCAT       :String = operators("+")
		override val LIKE         :String = operators("like")
		override val BETWEEN      :String = operators("between")
		override val NOT          :String = operators("not")
		override val AND          :String = operators("and")
		override val OR           :String = operators("or")
		override val UNION        :String = operators("union")
		override val UNION_ALL    :String = operators("union all")
		override val INTERSECT    :String = operators("intersect")
		override val MINUS        :String = operators("minus")
		override val WITH         :String = keywords("with")
		override val SELECT       :String = keywords("select")
		override val DISTINCT     :String = keywords("distinct")
		override val FROM         :String = keywords("from")
		override val WHERE        :String = keywords("where")
		override val GROUP_BY     :String = keywords("group by")
		override val HAVING       :String = keywords("having")
		override val AS           :String = keywords("as")
		override val INNER_JOIN   :String = keywords("join")
		override val OUTER_JOIN   :String = keywords("outer join")
		override val LEFT_JOIN    :String = keywords("left join")
		override val RIGHT_JOIN   :String = keywords("right join")
		override val ON           :String = keywords("on")
		override val INSERT       :String = keywords("insert")
		override val INTO         :String = keywords("into")
		override val VALUES       :String = keywords("values")
		override val UPDATE       :String = keywords("update")
		override val SET          :String = keywords("set")
		override val MERGE        :String = keywords("merge")
		override val DELETE       :String = keywords("delete")


		override val _NULL_       :String = " " + NULL + " "
		override val _TRUE_       :String = " " + TRUE + " "
		override val _FALSE_      :String = " " + FALSE + " "
		override val _CONCAT_     :String = " " + CONCAT + " "
		override val _LIKE_       :String = " " + LIKE + " "
		override val _BETWEEN_    :String = " " + BETWEEN + " "
		override val _NOT_        :String = " " + NOT + " "
		override val _AND_        :String = " " + AND + " "
		override val _OR_         :String = " " + OR + " "
		override val _UNION_      :String = " " + UNION + " "
		override val _UNION_ALL_  :String = " " + UNION_ALL + " "
		override val _INTERSECT_  :String = " " + INTERSECT + " "
		override val _MINUS_      :String = " " + MINUS + " "
		override val _SELECT_     :String = " " + SELECT + " "
		override val _FROM_       :String = " " + FROM + " "
		override val _WHERE_      :String = " " + WHERE + " "
		override val _GROUP_BY_   :String = " " + GROUP_BY + " "
		override val _HAVING_     :String = " " + HAVING + " "
		override val _AS_         :String = " " + AS + " "
		override val _INNER_JOIN_ :String = " " + INNER_JOIN + " "
		override val _OUTER_JOIN_ :String = " " + OUTER_JOIN + " "
		override val _LEFT_JOIN_  :String = " " + LEFT_JOIN + " "
		override val _RIGHT_JOIN_ :String = " " + RIGHT_JOIN + " "
		override val _ON_         :String = " " + ON + " "
		override val _INTO_       :String = " " + INTO + " "
		override val _VALUES_     :String = " " + VALUES + " "
		override val _SET_        :String = " " + SET + " "
		override val WITH_        :String = WITH + " "
		override val SELECT_      :String = SELECT + " "
		override val INSERT_      :String = INSERT + " "
		override val UPDATE_      :String = UPDATE + " "
		override val MERGE_       :String = MERGE + " "
		override val DELETE_      :String = DELETE + " "
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
		chant(spelling.spell(query).adapt { _ :Any => @~ }, result)

	override def apply[X, R, Y](query :Query[X, R])(implicit result :StatementResult[R, Y]) :Incantation[X, Y] =
		chant(spelling.spell(query), result)
//
//	override def apply[X <: Chain, Y](call :FunctionSQL[RowProduct, GlobalScope, X, Y]) :Cantrip[Y] =
//		chant(spelling.spell(call).compose { _ :Any => @~ }, StatementResult.SingleResult(call.readForm))

	override def apply[X, Y](statement :DMLStatement[X, Y]) :Incantation[X, Y] =  //todo: CallableStatement
		chant(spelling.spell(statement), statement.result)

	override def apply[X, Y](statement :Call[X, Y]) :Incantation[X, Y] = {
		val sql = spelling.spell(statement)
		Incantation.call(sql.sql.toString)(sql.setter, statement.result)
	}

	/** The default factory method creating incantations from complete SQL/DML of the translated statement
	  * and a strategy for reading the required values from the [[java.sql.PreparedStatement PreparedStatement]]
	  * result(s). It is the delegation target of all `apply` methods in this dialect except for
	  * [[net.noresttherein.oldsql.sql.Call Call]] statements which require a specific incantation implementation
	  * creating a [[java.sql.CallableStatement CallableStatement]] instead of regular `PreparedStatement`
	  * in order to read (optional) ''OUT'' parameters.
	  */
	protected def chant[X, Y](sql :SpelledSQL[X], result :StatementResult[Nothing, Y]) :Incantation[X, Y] =
		Incantation(sql.sql.toString)(sql.setter, result)

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

