package net.noresttherein.oldsql.sql.ast

import scala.collection.{EvidenceIterableFactory, IterableFactory}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{InseparableExpressionException, InvalidSQLException}
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{ColumnReadForm, ColumnWriteForm, SQLReadForm, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.Table.TableExpression
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.slang.cast2TypeParams
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, RowShape, SQLDialect, SQLExpression, StandardSQL, WithClause}
import net.noresttherein.oldsql.sql.CommonTableExpression.CommonTableAlias
import net.noresttherein.oldsql.sql.DMLStatement.StatementResult
import net.noresttherein.oldsql.sql.Incantation.Cantrip
import net.noresttherein.oldsql.sql.Query.{QueryTemplate, SingleQueryTemplate}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.Select.{Intersect, Minus, Union, UnionAll}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, Grouped, Single, SingleRowSQLTemplate, SpecificExpressionVisitor, SQLShape}
import net.noresttherein.oldsql.sql.ast.ColumnMappingQuery.{AnyColumnMappingQueryVisitor, CaseAnyColumnMappingQuery, CaseSpecificColumnMappingQuery, SpecificColumnMappingQueryVisitor}
import net.noresttherein.oldsql.sql.ast.ColumnQuery.{AnyColumnQueryVisitor, ColumnSingleQuery, SpecificColumnQueryVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumnAs.{AnyCompoundSelectColumnAsVisitor, SpecificCompoundSelectColumnAsVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectAs.{AnyCompoundSelectAsVisitor, CaseAnyCompoundSelectAs, CaseSpecificCompoundSelectAs, SpecificCompoundSelectAsVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectColumn.{AnyCompoundSelectColumnVisitor, CaseAnyCompoundSelectColumn, CaseSpecificCompoundSelectColumn, SpecificCompoundSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.{AnyCompoundSelectVisitor, CaseAnyCompoundSelect, CaseSpecificCompoundSelect, SpecificCompoundSelectVisitor}
import net.noresttherein.oldsql.sql.ast.MappingQuerySQL.{AnyMappingQueryVisitor, MatchAnyMappingQuery, MatchSpecificMappingQuery, SpecificMappingQueryVisitor}
import net.noresttherein.oldsql.sql.ast.QuerySQL.{Rows, SingleQuerySQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.{AnySelectAsVisitor, CaseAnySelectAs, CaseSpecificSelectAs, SpecificSelectAsVisitor}
import net.noresttherein.oldsql.sql.ast.SelectColumn.{AnySelectColumnVisitor, CaseAnySelectColumn, CaseSpecificSelectColumn, SpecificSelectColumnVisitor}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{AnySelectColumnAsVisitor, SpecificSelectColumnAsVisitor}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{AnySelectVisitor, CaseAnySelect, CaseSpecificSelect, SpecificSelectVisitor}
import net.noresttherein.oldsql.sql.mechanics.{=~=, Reform, QueryReform, SpelledSQL, SQLTransformation}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL expression returning a row cursor. It is the common base type for
  * the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] type hierarchy and
  * compound selects on them (such as `UNION`): [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]].
  * It also forms its own hierarchy parallel to that of `SelectSQL` and `CompoundSelectSQL`, with subtypes
  * for queries returning a single [[net.noresttherein.oldsql.sql.ast.ColumnQuery column]]
  * and a [[net.noresttherein.oldsql.sql.ast.MappingQuerySQL mapping]].
  *
  * Query expressions (with the exception  of [[net.noresttherein.oldsql.sql.ast.ColumnQuery ColumnQuery]])
  * are not valid values in all places which accept
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope, V]`,
  * in particular as ''select'' clauses.
  * This is because they cannot be [[net.noresttherein.oldsql.sql.SQLExpression.split split]] into individual columns.
  * They are generally used only as subexpressions
  * of [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]s and, through an implicit conversion,
  * as [[net.noresttherein.oldsql.schema.Table.TableExpression table expressions]] usable in
  * [[net.noresttherein.oldsql.sql.FromClause from clauses]]. If an instance is encountered during
  * SQL [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.spell spelling]] in a location requiring
  * access to individual columns, an [[net.noresttherein.oldsql.exceptions.InvalidSQLException IllegalSQLException]].
  * Some lax spelling [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling strategies]] may however opt
  * to perform a more global refactor, moving the query to the ''from'' clause of the encompassing expression
  * and replacing it with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] of that table expression.
  * @tparam F $F
  * @tparam R The value type of the [[net.noresttherein.oldsql.sql.Query.QueryTemplate.selectClause select clause]]
  *           of this query.
  */
trait QuerySQL[-F <: RowProduct, R]
	extends SQLExpression[F, Single, Rows[R]]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = QuerySQL[f, R] })#Q]
	   with QueryTemplate[R, ({ type Q[r] = QuerySQL[F, r] })#Q]
{
	/** Member ''selects'' (or cursors) which constitute this query in order, joined with
	  * [[net.noresttherein.oldsql.sql.Select.SelectOperator operators]] like 'union'.
	  * Any [[net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL SingleQuerySQL]],
	  * in particular [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]], returns itself.
	  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] returns a concatenation
	  * of constituents of its left and right sides.
	  */
	override def constituents :Seq[SingleQuerySQL[F, R]]

	//semantics of unions and the rest:
	// - if the column set is the same, normal union
	// - if not, but the nominal mapping is the same, than missing columns are added with null values to each operand
	// - if both are mapping-based, with mappings from the same hierarchy, use union column set with a discriminator column
	// - otherwise the column set becomes two separate sets with a discriminator
	def union[E <: F, X](other :QuerySQL[E, X])(implicit equiv :R =~= X) :QuerySQL[E, equiv.Unified] =
		Union(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def unionAll[E <: F, X](other :QuerySQL[E, X])(implicit equiv :R =~= X) :QuerySQL[E, equiv.Unified] =
		UnionAll(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def minus[E <: F, X](other :QuerySQL[E, X])(implicit equiv :R =~= X) :QuerySQL[E, equiv.Unified] =
		Minus(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def intersect[E <: F, X](other :QuerySQL[E, X])(implicit equiv :R =~= X) :QuerySQL[E, equiv.Unified] =
		Intersect(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	/** Wraps this ''select'' in an SQL `EXISTS(...)` function. */
	def exists :ColumnSQL[F, Single, Boolean] = ExistsSQL(this)

	/** Wraps this ''select'' in SQL `NOT EXISTS(...)`. */
	def notExists :ColumnSQL[F, Single, Boolean] = !ExistsSQL(this)

	/** Converts this ''select'' to an expression for its returned value, representing its usage in SQL assuming
	  * that exactly one row is returned.
	  */
	def one :SQLExpression[F, Single, R] = to[R]

	/** Converts this ''select'' to an expression for a result set which can be used in conjunction with
	  * [[net.noresttherein.oldsql.sql.ast.ExistsSQL ExistsSQL]] or
	  * [[net.noresttherein.oldsql.sql.ast.InSQL InSQL]] and the like.
	  */
	def rows :SQLExpression[F, Single, Seq[R]] = to[Seq[R]]

	def transform[X](transformation :SQLTransformation[R, X]) :QuerySQL[F, X]
	override def groundValue :Opt[Rows[R]] = Lack
	override def isGround :Boolean = false

	override def isAnchored = true
//	override def anchor(from :F) :QuerySQL[F, V]// = this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :QuerySQL[E, V]
//
//	override def expand[U <: F, E <: RowProduct]
//
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :QuerySQL[E, V]
	/** Returns [[net.noresttherein.oldsql.sql.Select.SelectTemplate.withClause withClause]]`.`[[net.noresttherein.oldsql.sql.WithClause.outer outer]]. */
	override def outerWithClause :WithClause = withClause.outer
/*
	protected override def reform[E <: RowProduct, C >: Grouped <: GlobalScope, X, U]
	                             (other :SQLExpression[E, C, X])(reform :Reform, passesAllowed :Int)
	                             (implicit leftResult :Lift[Rows[V], U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, GlobalScope, U], SQLExpression[E, C, U]) =
		if (passesAllowed > 3)
			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
		else
			reformed(reform.left).reformer[E, C, X, U](reform.prohibitReformLeft, passesAllowed).apply(other)

	protected override def reform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                             (other :ComponentLValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
	                             (implicit leftResult :Lift[Rows[V], U], rightResult :Lift[X, U], spelling :SQLSpelling)
			:(SQLExpression[F, GlobalScope, U], reform.LValue[E, C, U]) =
		if (passesAllowed > 3)
			other.`->reform`(this)(reform.swap, passesAllowed - 1).swap
		else
			reformed(reform.left).defaultReform(other)(reform.prohibitReformLeft, passesAllowed)

	private def defaultReform[E <: RowProduct, C[O] <: MappingAt[O], X, U]
	                         (other :ComponentLValueSQL[E, C, X])(reform :Reform, passesAllowed :Int)
	                         (implicit leftResult :Lift[Rows[V], U], rightResult :Lift[X, U], spelling :SQLSpelling) =
		super.reform(other)(reform, passesAllowed)
*/

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
//	  *      to `reform(this, second)`, a bridge for method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reform_: reform_:]]
//	  *      of the argument.
//	  *   1. A [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] splits the algorithm
//	  *      into three parts:
//	  *        1. reforms itself using `reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.subreform subreform]];
//	  *        1. unifies the first of the two queries with `second`;
//	  *        1. unifies the second of the two queries with previously reformed `second`;
//	  *        1. unifies the first query again with fully reformed `second`, leaving the latter unchanged.
//	  *
//	  * If the above scheme fails to produce a `CompoundSelectSQL` with unified member ''selects'',
//	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
//	  * is thrown. `Reform` implementation is required to be additive in the sense that modifications done
//	  * to a query `a` when reforming it query `b` cannot be reverted by reforming `a` with another query `c`.
//	  *
//	  * Both `this` and `second` are assumed to be internally unified (be results of calling
//	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] on some other queries).
//	  * @param second another query of the same value type, combined with this query in a larger `CompoundSelectSQL`.
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
//	                            (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R])
//
//	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
//	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
//	  * from `this.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.reform reform]] by
//	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL SingleQuerySQL]] instances other than
//	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]. It should not be called directly,
//	  * but rather through the `Reform` facade method of the same signature, which by default delegates here,
//	  * but can be also overridden by specialized reforming strategies.
//	  *
//	  * The general algorithm is the same as for `reform_:[E <: F](first :SelectSQL[E, V])`,
//	  * except the argument is not normally changed in the process:
//	  * `reform.`[[net.noresttherein.oldsql.sql.mechanics.Reform.prohibitReformRight prohibitReformRight]] is used
//	  * to unify `first.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL SingleQuerySQL]]
//	  * with the select clause(s) in this query, and the first query in the returned pair is always `first`,
//	  * unless this case is explicitly overridden by a particular `Reform` implementation.
//	  *
//	  * Both `this` and `first` are assumed to be internally unified (be results of calling
//	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] on some other queries).
//	  * @param first  a non-compound SQL query combined with this query into some larger `CompoundSelectSQL`
//	  *               (as the first query of the pair).
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform_:[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
//	                              (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R])
//
//	/** Reforms this query (as the second/right one in a pair) with the given argument (the first/left query)
//	  * to a consistent ''select'' clause between member ''selects''. This is a double dispatch method called
//	  * from `this.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.reform reform]] by
//	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] queries. It should not be called directly, but rather
//	  * through the `Reform` facade method of the same signature, which by default delegates here, but can be also
//	  * overridden by specialized reforming strategies.
//	  *   1. If this query is also a `SelectSQL` or is otherwise already unified, its ''select'' clause
//	  *      is unified with the ''select'' clause of the argument using the standard reforming process
//	  *      of [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] and
//	  *      [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`.`[[net.noresttherein.oldsql.sql.SQLExpression.reform reform]].
//	  *   1. If this query is a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]],
//	  *      the argument is unified first with `this.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.left left]],
//	  *      then the result is unified with `this.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.right right]],
//	  *      and finally reformed `left` and `right` queries are unified with each other and combined in a returned
//	  *      `CompoundSelectSQL`.
//	  *
//	  * For this scheme to work, `reform` must be additive in the sense that a query `a`, unified with a query `b`,
//	  * cannot have its changes reverted when unifying it with another query `c`.
//	  * @param first  an SQL ''select'' combined with this query into some larger `CompoundSelectSQL`
//	  *               (as the first query of the pair).
//	  * @param reform a strategy used to unify expression pairs to the same shape (column set).
//	  */
//	protected def reform_:[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
//	                              (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R])
//
//	private[sql] final def `->reform`[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
//	                                         (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
//		this.reform(second)(reform)
//
//	private[sql] final def `->reform_:`[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
//	                                           (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
//		reform_:(first)(reform)
//
//	private[sql] final def `->reform_:`[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
//	                                           (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
//		reform_:(first)(reform)

	override def reformed(implicit spelling :SQLSpelling) :QuerySQL[F, R] = spelling.queryReform(this)


	@throws[InseparableExpressionException]("If this query is a not a ColumnQuerySQL.")
	protected override def split(implicit spelling :SQLSpelling) :Seq[ColumnSQL[F, Single, _]] =
		throw new InseparableExpressionException(this, "Cannot split a non-column query " + this + " into columns.")

	/** Target of [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.spell spell]] extension method.
	  * Forwards to
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.spell[V](query:QuerySQL[RowProduct,V])* SQLSpelling.spell]]
	  * and caches the result. All future calls using the same `spelling` argument will return the cached SQL.
	  * This method should not be generally overridden, unless to additionally enrich the result without changing
	  * the actual SQL.
	  */
	protected def spellParamless(implicit spelling :SQLSpelling, top :QuerySQL[F, R] <:< QuerySQL[RowProduct, R])
			:SpelledSQL[@~] =
	{
		@inline def shazam = if (cachedSpelling == spelling) cachedSQL else spelling.spell(top(this))
		if (cachedSQL == null) synchronized {
			if (cachedSQL == null) {
				cachedSpelling = spelling
				cachedSQL = spelling.spell(top(this))
				cachedSQL
			} else shazam
		} else {
			if (cachedSpelling == null) synchronized { shazam }
			else shazam
		}
	}
	@volatile private var cachedSQL      :SpelledSQL[@~] = _
	@volatile private var cachedSpelling :SQLSpelling = _


	/** Formats this query as an SQL `String` expression for use inside a tuple with other expressions,
	  * or where tuples cannot be used. As ''selects'' cannot be inlined, this method throws an `IllegalStateException`
	  * if its [[net.noresttherein.oldsql.sql.SQLExpression.selectForm read form]]'s column count is greater than `1`.
	  * Single column instances, even when not conforming to [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]],
	  * delegate to [[net.noresttherein.oldsql.sql.SQLExpression.defaultSpelling defaultSpelling]].
	  */
	protected override def explodedSpelling[P](independent :Boolean)
	                                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
	                                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		selectForm.columnCount match {
			case 1 => defaultSpelling(from, context, params)::Nil
			case n =>
				throw new InseparableExpressionException(
					this, s"Cannot inline a compound select of multiple ($n :$selectForm) columns: $this."
				)
		}


	/** Target of [[net.noresttherein.oldsql.sql.ast.QuerySQL.TopQuerySQLExtension.chant chant]] extension method.
	  * Simply forwards to [[net.noresttherein.oldsql.sql.SQLDialect.apply[R,Y](query:QuerySQL[RowProduct,V])* apply]]
	  * method of the implicit [[net.noresttherein.oldsql.sql.SQLDialect dialect]].
	  */
	protected def chantParamless[O](implicit composition :StatementResult[R, O], dialect :SQLDialect,
	                                top :QuerySQL[F, R] <:< QuerySQL[RowProduct, R]) :Cantrip[O] =
		dialect(top(this))


//	override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//	                    (matcher :AnyExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//		matcher.query(this)

	override def canEqual(that :Any) :Boolean = that.getClass == getClass
}






private[ast] sealed abstract class Rank1QuerySQLImplicits {
	//we can't have a single implicit into Table[query.RowMapping] and we need these casts as, above all,
	//  we need to preserve the value type of the query as the subject type, and bound MappingAt does not specify it.
	//todo: replace typed projection with projection once we get rid of references to BaseMapping in API
	implicit def arbitraryQueryRelation[V](query :QuerySQL[RowProduct, V]) :Table[MappingOf[V]#TypedProjection] =
		TableExpression[query.RowMapping, V](query).asInstanceOf[Table[MappingOf[V]#TypedProjection]]
//		TableExpression(query.supertyped)

	implicit def singleColumnRelation[V](query :ColumnQuery[RowProduct, V]) :Table[MappingOf[V]#ColumnProjection] =
		TableExpression[query.RowMapping, V](query)
}



object QuerySQL extends Rank1QuerySQLImplicits {

	//todo: verify the implicit conversion works
	implicit class TopQuerySQLExtension[M[O] <: MappingAt[O], V]
	               (private val self :QuerySQL[RowProduct, V] { type RowMapping[O] = M[O] })
		extends AnyVal
	{
		def toRelation :Table[M] = TableExpression[M, V](self)
		def toTable    :Table[M] = TableExpression[M, V](self)

//		def as[A <: Label](alias :A) :M Aliased A = self.toTable as alias
		def as[A <: Label](alias :A) :CommonTableAlias[A, M] = CommonTableAlias[A, M, V](alias, self)

//		def as[A <: Label](alias :A) :With[M] As A = With(alias, this) //todo: With for MappingQuerySQL
		//extracted because of conflicts in TopSelect with Query
		/** Translates the abstract syntax tree of this SQL expression into textual SQL.
		  * The result is adapted to the DBMS at hand using implicitly available
		  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] strategy. Aside from the `String`
		  * representation, returned [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] contains
		  * write form setting all bound parameters of the query.
		  */ //an extension method as TopSelect would inherit conflicting signatures from Select and SelectSQL
		@throws[InvalidSQLException]("if the expression cannot be rendered as SQL for the DBMS particular to the given dialect.")
		def spell(implicit spelling :SQLSpelling = StandardSQL.spelling) :SpelledSQL[@~] =
			self.spellParamless


		/** Converts this query SQL expression into an executable [[net.noresttherein.oldsql.sql.Incantation Incantation]]
		  * proper to the DBMS using the implicit [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]].
		  * The implementation is delegated to the dialect object.
		  */
		@throws[InvalidSQLException]("if the expression cannot be rendered as SQL for the DBMS particular to the given dialect.")
		def chant[R](implicit composition :StatementResult[V, R], dialect :SQLDialect = StandardSQL) :Cantrip[R] =
			self.chantParamless

		/** Converts this query SQL expression into an executable
		  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[(), C[V]]` proper to the implicit
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
		  * a collection `C[V]` with the given factory.
		  */
		def returnAs[C[_]](collection :IterableFactory[C])(implicit dialect :Maybe[SQLDialect]) :Cantrip[C[V]] =
			chant[C[V]](StatementResult(collection)(self.rowForm), dialect getOrElse StandardSQL)

		/** Converts this query SQL expression into an executable
		  * [[net.noresttherein.oldsql.sql.Incantation Incantation]]`[(), C[V]]` proper to the implicit
		  * [[net.noresttherein.oldsql.sql.SQLDialect SQLDialect]]. The results of the query will be returned as
		  * a collection `C[V]` with the given factory.
		  */
		def returnAs[C[_], E[_]](collection :EvidenceIterableFactory[C, E])
		                        (implicit ev :E[V], dialect :Maybe[SQLDialect]) :Cantrip[C[V]] =
			chant[C[V]](StatementResult(collection)(ev, self.rowForm), dialect getOrElse StandardSQL)
	}


	implicit def derivedTable[M[O] <: MappingAt[O]](query :MappingQuerySQL[RowProduct, M]) :Table[M] =
		TableExpression[M, M[Unit]#Subject](query)




	type __ = QuerySQL[_ <: RowProduct, _]

	type GroundQuery[V] = QuerySQL[RowProduct, V]
	type GroundColumnQuery[V] = ColumnQuery[RowProduct, V]
	type GroundMappingQuery[H[A] <: MappingAt[A]] = MappingQuerySQL[RowProduct, H]
	type GroundColumnMappingQuery[H[A] <: BaseColumn[V, A], V] = ColumnMappingQuery[RowProduct, H, V]


	//todo: same patterns for all QuerySQL subtypes
//	def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, V])
//			:Opt[(QuerySQL[F, X], V =:= Rows[X]) forSome { type X }] =
//		e match {
//			case q :QuerySQL[F, x] => Got((q, implicitly[V =:= V].castParam2[Rows[x]]))
//			case _ => Lack
//		}



	/** A mixin trait for [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] interfaces which defines
	  * its 'self type' as a functor of its domain clause. Narrows the result type of several methods
	  * to return a query of the same type. This trait extends `QueryTemplate`, but only with a most generic
	  * query type parameter of `QuerySQL[F, _]`. Subclasses must mix that trait independently.
	  */
/*
	private[sql] trait QuerySQLSelfType[-F <: RowProduct, V, +S[-E <: RowProduct] <: QuerySQL[E, V]]
		extends SQLExpression[F, GlobalScope, Rows[V]] with QueryTemplate[V, ({ type Q[X] = QuerySQL[F, X] })#Q]
	{ this :S[F] =>
//		override def distinct :S[F]
		override def isGlobal :Boolean = true
		override def asGlobal :Option[S[F]] = Some(this)

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :S[E] =
			(this :QuerySQLSelfType[F, V, S]).expand[U, E](base)(ext.asExpandedBy, implicitly[GlobalScope <:< GlobalScope])

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope) :S[E]

		override def canEqual(that :Any) = super[QueryTemplate].canEqual(that)
	}
*/


	/** A `QuerySQL` expression which cannot be divided into subqueries. This trait is extended by
	  * implementations which are not a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] -
	  * most importantly, [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]], but also for expressions
	  * producing row cursors coming from other sources, such as a stored function.
	  * Aside from `Select`, these queries' ''select'' clause typically cannot be reformed.
	  */
	trait SingleQuerySQL[-F <: RowProduct, R]
		extends QuerySQL[F, R]
		   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = SingleQuerySQL[f, R] })#Q]
		   with SingleQueryTemplate[R, ({ type Q[X] = SingleQuerySQL[F, X] })#Q]
	{
		override def transform[X](transformation :SQLTransformation[R, X]) :SingleQuerySQL[F, X]

/*
		protected override def reform[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
		                                     (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
			reform.prohibitReformLeft(this, second) //calls this reform_: second

		protected override def reform_:[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
		                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		{
			val validator = reform.prohibitReformLeft.prohibitReformRight
			validator[Nothing, Grouped, R, Nothing, Grouped, R, R](first.selectClause, selectClause)
			(first, this)
		}

		protected override def reform_:[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
		                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		{
			val reformFirst = reform.prohibitReformRight
			val reformed = reformFirst[first.From, Grouped, R, Nothing, Grouped, R, R](
				first.selectClause, selectClause
			)._1
			if (reformed eq first.selectClause)
				(first, this)
			else
				(first.selectOther(reformed), this)
		}
*/

		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :SingleQuerySQL[F, R] =
			this
	}


	trait DecoratorQuerySQL[-F <: RowProduct, R] extends QuerySQL[F, R] {
		val query :QuerySQL[F, R]
		override def selectForm :SQLReadForm[Rows[R]] = query.selectForm
		override def rowForm :SQLReadForm[R] = query.rowForm

		override type isSelectable  = query.isSelectable
		override type RowMapping[O] = query.RowMapping[O]

		override def mapping[O] :RowMapping[O] = query.mapping
		override def export[O] :TypedMapping[RowMapping[O]#Subject, O] = query.export

		override def constituents :Seq[SingleQuerySQL[F, R]] = query.constituents
		override def selectCount  :Int = query.selectCount
		override def columns      :Seq[TypedColumn[_, this.type]] = query.columns.withOrigin[this.type]
		override def selectClause :SQLShape[R] = query.selectClause
		override def withClause   :WithClause = query.withClause

		def copy[E <: F](query :QuerySQL[E, R]) :QuerySQL[E, R]
		override def isAnchored(from :F) :Boolean = query.isAnchored(from)
		override def anchor(from :F) :QuerySQL[F, R] = copy(query.anchor(from))

		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :QuerySQL[E, R] =
			expand(base)

		override def expand[U <: F, E <: RowProduct]
		                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single) :QuerySQL[E, R]

/*
		protected override def reform[E <: F](second :QuerySQL[E, R])(reform :QueryReform)
		                                     (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		{
			val (left, right) = reform(query, second)
			(copy(left), right)
		}
		protected override def reform_:[E <: F](first :SingleQuerySQL[E, R])(reform :QueryReform)
		                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		{
			val (left, right) = reform(first, query)
			(left, copy(right))
		}
		protected override def reform_:[E <: F](first :SelectSQL[E, R])(reform :QueryReform)
		                                       (implicit spelling :SQLSpelling) :(QuerySQL[E, R], QuerySQL[E, R]) =
		{
			val (left, right) = reform(first, query)
			(left, copy(right))
		}
*/
		protected override def reformed(reform :QueryReform)(implicit spelling :SQLSpelling) :QuerySQL[F, R] =
			copy(reform(query))

		protected override def shape(implicit spelling :SQLSpelling) :RowShape = spelling.shape(query)
		protected override def columnCount(implicit spelling :SQLSpelling) :Int = spelling.columnCount(query)
		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(query)

		protected override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling(query)(from, context, params)

		protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, Rows[R], Y]) :Y = visitor.query(this)
		protected override def visit[Y[-_ >: Grouped <: Single, _]]
		                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, Rows[R]] =
			visitor.query(this)

		override def isomorphic(that :SQLExpression.__) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :DecoratorQuerySQL[_, _] if getClass == other.getClass => query isomorphic other.query
			case _ => false
		}
	}





	/** The value type of [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] instances
	  * with ''select'' clause of type `V`. This indirection allows the use of an SQL ''select'' expression
	  * both as a sequence (for example, inside `exists`) and as a single value (or rather, single row).
	  * Implicit conversions exist from `SQLExpression[F, S, Rows[V]]` to both `SQLExpression[F, S, V]`
	  * and `SQLExpression[F, S, Seq[V]]`. It is used mostly on the type level, with no values being normally
	  * created as a part of an execution of a query.
	  */ //todo: rename to Selected/Cursor or something and move out
	trait Rows[+V] { //consider: rename in conformance to GroupScope/GroupingRelation/Grouping - Group? Multi?
		def size :Int = seq.size
		def isEmpty :Boolean = seq.isEmpty
		def nonEmpty :Boolean = seq.nonEmpty

		def map[X](f :V => X) :Rows[X]

		def seq  :Seq[V]
		def one  :V
		def head :V
		def headOption :Option[V]
	}


	private[QuerySQL] sealed abstract class Rank1RowsImplicits {
		implicit def readForm[T :SQLReadForm] :SQLReadForm[Rows[T]] =
			SQLReadForm.map("Rows")((t :T) => Rows(t))

		implicit def writeForm[T :SQLWriteForm] :SQLWriteForm[Rows[T]] =
			SQLWriteForm.map("Rows") { row :Rows[T] => row.one }
	}

	object Rows extends Rank1RowsImplicits {
		def apply[E](items :E*) :Rows[E] =
			if (items.isEmpty || items.sizeIs > 1) MultipleRows(items)
			else new SingleRow(items.head)

		def single[E](item :E) :Rows[E] = new SingleRow(item)

		implicit val empty :Rows[Nothing] = MultipleRows(Nil)

		implicit def readForm[T :ColumnReadForm] :ColumnReadForm[Rows[T]] =
			ColumnReadForm.map("Rows")((t :T) => Rows(t))

		implicit def writeForm[T :ColumnWriteForm] :ColumnWriteForm[Rows[T]] =
			ColumnWriteForm.map("Rows") { row :Rows[T] => row.one }

//			SQLWriteForm.unsupported(0, "<Rows[_]")("")
//			SQLWriteForm.nullValue[T]("Rows")(SQLWriteForm[T], NullValue.Unsupported("Rows is a phantom type and should not be used as a parameter."))

		private case class MultipleRows[+E](override val seq :Seq[E]) extends Rows[E] {
			override def one :E = seq match {
				case Seq(res) => res
				case _ => throw new IllegalStateException("Expected a single result from a Rows instance, got " + seq.size)
			}
			override def head :E = seq.head
			override def headOption :Option[E] = seq.headOption

			override def map[X](f :E => X) :Rows[X] = new MultipleRows(seq.map(f))
		}

		private class SingleRow[E](override val one :E) extends Rows[E] {
			override def head = one
			override def headOption = Some(one)
			override def seq :Seq[E] = one::Nil

			override def map[X](f :E => X) :Rows[X] = new SingleRow(f(one))
		}
	}


	trait SpecificQueryVisitor[+F <: RowProduct, X, +Y]
		extends SpecificColumnQueryVisitor[F, X, Y] with SpecificMappingQueryVisitor[F, X, Y]
		   with SpecificCompoundSelectVisitor[F, X, Y] with SpecificSelectVisitor[F, X, Y]
	{
		def query[R](e :QuerySQL[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificQuery[+F <: RowProduct, X, +Y] extends SpecificQueryVisitor[F, X, Y]
		with CaseSpecificCompoundSelect[F, X, Y] with CaseSpecificSelect[F, X, Y] with MatchSpecificMappingQuery[F, X, Y]
	{   //not CaseMappingQuery because the following traits override those methods
		override def columnQuery[R](e :ColumnQuery[F, R])(implicit isRows :X =:= Rows[R]) :Y = query(e :QuerySQL[F, R])
		override def mappingQuery[M[O] <: MappingAt[O]]
		                         (e :MappingQuerySQL[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y = query(e)
	}
	trait CaseSpecificQuery[+F <: RowProduct, X, +Y] extends MatchSpecificQuery[F, X, Y] {
		override def select[R](e :SelectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y = query(e)
		override def compoundSelect[R](e :CompoundSelectSQL[F, R])(implicit isRows :X =:= Rows[R]) :Y = query(e)
	}
//
//
//	trait QueryVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnQueryVisitor[F, Y] with MappingQueryVisitor[F, Y]
//		   with CompoundSelectVisitor[F, Y] with SelectVisitor[F, Y]
//	{
//		def query[R](e :QuerySQL[F, R]) :Y[Single, Rows[R], QuerySQL[F, R]]
//	}
//	trait MatchQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends QueryVisitor[F, Y] with MatchMappingQuery[F, Y]
//		   with CaseCompoundSelect[F, Y] with CaseSelect[F, Y]
//	{
//		override def columnQuery[R](e :ColumnQuery[F, R]) :Y[Single, Rows[R], ColumnQuery[F, R]] = query(e)
//		override def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M])
//				:Y[Single, Rows[M[Unit]#Subject], MappingQuerySQL[F, M]] =
//			query(e)
//	}
//	trait CaseQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchQuery[F, Y]
//	{
//		override def select[R](e :SelectSQL[F, R]) :Y[Single, Rows[R], SelectSQL[F, R]] = query(e)
//		override def compoundSelect[R](e :CompoundSelectSQL[F, R]) :Y[Single, Rows[R], CompoundSelectSQL[F, R]] =
//			query(e)
//	}


	trait AnyQueryVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnQueryVisitor[F, Y] with AnyMappingQueryVisitor[F, Y]
		   with AnyCompoundSelectVisitor[F, Y] with AnySelectVisitor[F, Y]
	{
		def query[V](e :QuerySQL[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnyQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyQueryVisitor[F, Y]
		with CaseAnyCompoundSelect[F, Y] with CaseAnySelect[F, Y] with MatchAnyMappingQuery[F, Y]
	{ //not CaseAnyMappingQuery because the following traits override those methods
		override def columnQuery[V](e :ColumnQuery[F, V]) :Y[Single, Rows[V]] = query(e :QuerySQL[F, V])

		override def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M]) :Y[Single, Rows[M[Unit]#Subject]] =
			query(e)
	}
	trait CaseAnyQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends MatchAnyQuery[F, Y] {
		override def select[V](e :SelectSQL[F, V]) :Y[Single, Rows[V]] = query(e)
		override def compoundSelect[V](e :CompoundSelectSQL[F, V]) :Y[Single, Rows[V]] = query(e)
	}

}






/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectColumn select]] or
  * a [[net.noresttherein.oldsql.sql.ast.CompoundSelectColumn compound select]], which returns
  * a single column.
  */
trait ColumnQuery[-F <: RowProduct, R]
	extends QuerySQL[F, R] with ColumnSQL[F, Single, Rows[R]]
	   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = ColumnQuery[f, R] })#Q]
	   with QueryTemplate[R, ({ type Q[V] = ColumnQuery[F, V] })#Q]
{
	override type RowMapping[O] <: TypedColumn[R, O]

	override def export[O] :TypedColumn[R, O]

	override def rowForm :ColumnReadForm[R]
	override def constituents :Seq[ColumnSingleQuery[F, R]]
	//todo: update these after the query refactor
	def union[E <: F, X](other :ColumnQuery[E, X])(implicit equiv :R =~= X) :ColumnQuery[E, equiv.Unified] =
		Union(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def unionAll[E <: F, X](other :ColumnQuery[E, X])(implicit equiv :R =~ X) :ColumnQuery[E, equiv.Unified] =
		UnionAll(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def minus[E <: F, X](other :ColumnQuery[E, X])(implicit equiv :R =~= X) :ColumnQuery[E, equiv.Unified] =
		Minus(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	def intersect[E <: F, X](other :ColumnQuery[E, X])(implicit equiv :R =~= X) :ColumnQuery[E, equiv.Unified] =
		Intersect(this.rowsTo(equiv.left), other.rowsTo(equiv.right))

	override def one  :ColumnSQL[F, Single, R] = to[R]
	override def rows :ColumnSQL[F, Single, Seq[R]] = to[Seq[R]]

//	override def anchor(from :F) :ColumnQuery[F, V] //= this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnQuery[E, V]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:ColumnQuery[E, V]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.query(this)
}



object ColumnQuery {
	def unapply[F <: RowProduct, V](e :SQLExpression[F, Grouped, V])
			:Opt[(ColumnQuery[F, X], V =:= Rows[X]) forSome { type X }] =
		e match {
			case q :ColumnQuery[F, x] => Got((q, implicitly[V =:= V].castParam2[Rows[x]]))
			case _ => Lack
		}


	trait ColumnSingleQuery[-F <: RowProduct, R]
		extends ColumnQuery[F, R] with SingleQuerySQL[F, R]
		   with SingleRowSQLTemplate[F, Rows[R], ({ type Q[-f <: RowProduct] = ColumnSingleQuery[f, R] })#Q]
		   with SingleQueryTemplate[R, ({ type Q[X] = ColumnSingleQuery[F, X] })#Q]


	trait SpecificColumnQueryVisitor[+F <: RowProduct, X, +Y] extends SpecificColumnMappingQueryVisitor[F, X, Y]
		with SpecificCompoundSelectColumnVisitor[F, X, Y] with SpecificSelectColumnVisitor[F, X, Y]
	{
		def columnQuery[R](e :ColumnQuery[F, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	trait MatchSpecificColumnQuery[+F <: RowProduct, X, +Y]
		extends SpecificColumnQueryVisitor[F, X, Y] with CaseSpecificColumnMappingQuery[F, X, Y]
		   with CaseSpecificCompoundSelectColumn[F, X, Y] with CaseSpecificSelectColumn[F, X, Y]
	{
	   override def columnMappingQuery[M[O] <: BaseColumn[R, O], R]
	                                  (e :ColumnMappingQuery[F, M, R])(implicit isRows :X =:= Rows[R]) :Y =
		   columnQuery(e)
	}
	trait CaseSpecificColumnQuery[+F <: RowProduct, X, +Y] extends MatchSpecificColumnQuery[F, X, Y] {
		override def selectColumn[R](e :SelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y = columnQuery(e)
		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])(implicit isRows :X =:= Rows[R]) :Y =
			columnQuery(e)
	}
//
//
//	trait ColumnQueryVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectColumnVisitor[F, Y] with SelectColumnVisitor[F, Y] with ColumnMappingQueryVisitor[F, Y]
//	{
//		def columnQuery[R](e :ColumnQuery[F, R]) :Y[Single, Rows[R], ColumnQuery[F, R]]
//	}
//	trait MatchColumnQueryVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnQueryVisitor[F, Y] with CaseCompoundSelectColumn[F, Y] with CaseSelectColumn[F, Y]
//		   with CaseColumnMappingQuery[F, Y]
//	{
//		override def columnMappingQuery[M[O] <: BaseColumn[S, O], S](e :ColumnMappingQuery[F, M, S])
//				:Y[Single, Rows[S], ColumnMappingQuery[F, M, S]] =
//			columnQuery(e)
//	}
//	trait CaseColumnQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchColumnQueryVisitor[F, Y]
//	{
//		override def selectColumn[R](e :SelectColumn[F, R]) :Y[Single, Rows[R], SelectColumn[F, R]] =
//			columnQuery(e)
//
//		override def compoundSelectColumn[R](e :CompoundSelectColumn[F, R])
//				:Y[Single, Rows[R], CompoundSelectColumn[F, R]] =
//			columnQuery(e)
//	}


	trait AnyColumnQueryVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingQueryVisitor[F, Y]
		   with AnyCompoundSelectColumnVisitor[F, Y] with AnySelectColumnVisitor[F, Y]
	{
		def columnQuery[V](e :ColumnQuery[F, V]) :Y[Single, Rows[V]]
	}
	trait MatchAnyColumnQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnQueryVisitor[F, Y] with CaseAnyColumnMappingQuery[F, Y]
		   with CaseAnyCompoundSelectColumn[F, Y] with CaseAnySelectColumn[F, Y]
	{
		override def columnMappingQuery[M[O] <: BaseColumn[V, O], V]
		                               (e :ColumnMappingQuery[F, M, V]) :Y[Single, Rows[V]] =
			columnQuery(e)
	}
	trait CaseAnyColumnQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyColumnQuery[F, Y]
	{
		override def selectColumn[V](e :SelectColumn[F, V]) :Y[Single, Rows[V]] = columnQuery(e)
		override def compoundSelectColumn[V](e :CompoundSelectColumn[F, V]) :Y[Single, Rows[V]] = columnQuery(e)
	}

}






//todo: a special class or type alias for SQLMapping and, especially, ListingSQLMapping
/** An SQL query, that is an SQL [[net.noresttherein.oldsql.sql.ast.SelectAs select]] or
  * a [[net.noresttherein.oldsql.sql.ast.CompoundSelectAs compound select]] on them,
  * which provides [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` for the returned rows.
  */ //todo: either move it to package ast, just like SelectAs, or move SelectAs to SelectSQL object
trait MappingQuerySQL[-F <: RowProduct, M[O] <: MappingAt[O]]
	extends QuerySQL[F, M[Unit]#Subject] //not a subtype of QuerySQLTemplate for now, although we could make it so
	   with SingleRowSQLTemplate[F, Rows[M[Unit]#Subject], ({ type Q[-f <: RowProduct] = MappingQuerySQL[f, M] })#Q]
{
	override type RowMapping[O] = M[O]

//		override def constituents :Seq[SelectAs[F, M]]

	def union[E <: F](other :MappingQuerySQL[E, M])     :MappingQuerySQL[E, M] = Union(this, other)
	def unionAll[E <: F](other :MappingQuerySQL[E, M])  :MappingQuerySQL[E, M] = UnionAll(this, other)
	def minus[E <: F](other :MappingQuerySQL[E, M])     :MappingQuerySQL[E, M] = Minus(this, other)
	def intersect[E <: F](other :MappingQuerySQL[E, M]) :MappingQuerySQL[E, M] = Intersect(this, other)

//	override def anchor(from :F) :MappingQuerySQL[F, M] //= this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :MappingQuerySQL[E, M]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:MappingQuerySQL[E, M]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyExpressionVisitor[F, Y]) :Y[GlobalScope, Rows[M[()]#Subject]] =
//			matcher.mappingQuery(this)
}


object MappingQuerySQL {
	trait SpecificMappingQueryVisitor[+F <: RowProduct, X, +Y] extends SpecificColumnMappingQueryVisitor[F, X, Y]
		with SpecificCompoundSelectAsVisitor[F, X, Y] with SpecificSelectAsVisitor[F, X, Y]
	{
		def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y
	}
	trait MatchSpecificMappingQuery[+F <: RowProduct, X, +Y]
		extends SpecificMappingQueryVisitor[F, X, Y] with CaseSpecificSelectAs[F, X, Y] with CaseSpecificCompoundSelectAs[F, X, Y]
	{
		override def columnMappingQuery[M[O] <: BaseColumn[R, O], R]
		                               (e :ColumnMappingQuery[F, M, R])(implicit isRows :X =:= Rows[R]) :Y =
			{ val res = mappingQuery(e); res }
	}
	trait CaseSpecificMappingQuery[+F <: RowProduct, X, +Y] extends MatchSpecificMappingQuery[F, X, Y] {
		override def selectAs[M[O] <: MappingAt[O]](e :SelectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y =
			mappingQuery(e)

		override def compoundSelectAs[M[O] <: MappingAt[O]]
		                             (operation :CompoundSelectAs[F, M])(implicit isRows :X =:= Rows[M[Unit]#Subject]) :Y =
			mappingQuery(operation)
	}
//
//
//	trait MappingQueryVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends ColumnMappingQueryVisitor[F, Y] with CompoundSelectAsVisitor[F, Y] with SelectAsVisitor[F, Y]
//	{
//		def mappingQuery[M[O] <: MappingAt[O]]
//		                (e :MappingQuerySQL[F, M]) :Y[Single, Rows[M[Unit]#Subject], MappingQuerySQL[F, M]]
//	}
//	trait MatchMappingQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MappingQueryVisitor[F, Y] with CaseCompoundSelectAs[F, Y] with CaseSelectAs[F, Y]
//	{
//		override def columnMappingQuery[M[O] <: BaseColumn[S, O], S](e :ColumnMappingQuery[F, M, S])
//				:Y[Single, Rows[S], ColumnMappingQuery[F, M, S]] =
//			mappingQuery(e)
//	}
//	trait CaseMappingQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchMappingQuery[F, Y]
//	{
//		override def selectAs[M[O] <: MappingAt[O]](e :SelectAs[F, M])
//				:Y[Single, Rows[M[Unit]#Subject], SelectAs[F, M]] =
//			mappingQuery(e)
//
//		override def compoundSelectAs[M[O] <: MappingAt[O]](e :CompoundSelectAs[F, M])
//				:Y[Single, Rows[M[Unit]#Subject], CompoundSelectAs[F, M]] =
//			mappingQuery(e)
//	}


	trait AnyMappingQueryVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyColumnMappingQueryVisitor[F, Y] with AnyCompoundSelectAsVisitor[F, Y] with AnySelectAsVisitor[F, Y]
	{
		def mappingQuery[M[O] <: MappingAt[O]](e :MappingQuerySQL[F, M]) :Y[Single, Rows[M[Unit]#Subject]]
	}
	trait MatchAnyMappingQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyMappingQueryVisitor[F, Y] with CaseAnySelectAs[F, Y] with CaseAnyCompoundSelectAs[F, Y]
	{
		override def columnMappingQuery[M[O] <: BaseColumn[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[Single, Rows[V]] =
			{ val res = mappingQuery(e); res }
	}
	trait CaseAnyMappingQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyMappingQuery[F, Y]
	{
		override def selectAs[H[O] <: MappingAt[O]]
		                          (e :SelectAs[F, H]) :Y[Single, Rows[H[Unit]#Subject]] =
			mappingQuery(e)

		override def compoundSelectAs[M[O] <: MappingAt[O]]
		                             (operation :CompoundSelectAs[F, M]) :Y[Single, Rows[M[Unit]#Subject]] =
			mappingQuery(operation)
	}
}






/** An SQL query returning a single column of some relation.
  * @see [[net.noresttherein.oldsql.sql.ast.SelectColumnAs]]
  * @see [[net.noresttherein.oldsql.sql.ast.CompoundSelectColumnAs]]
  */
trait ColumnMappingQuery[-F <: RowProduct, M[O] <: BaseColumn[V, O], V] //todo: replace TypedColumn with ColumnAt
	extends ColumnQuery[F, V] with MappingQuerySQL[F, M]
	   with SingleRowSQLTemplate[F, Rows[V], ({ type Q[-f <: RowProduct] = ColumnMappingQuery[f, M, V] })#Q]
{
//		override def constituents :Seq[SelectColumnAs[F, M, V]]
	def union[E <: F](other :ColumnMappingQuery[E, M, V])     :ColumnMappingQuery[E, M, V] = Union(this, other)
	def unionAll[E <: F](other :ColumnMappingQuery[E, M, V])  :ColumnMappingQuery[E, M, V] = UnionAll(this, other)
	def minus[E <: F](other :ColumnMappingQuery[E, M, V])     :ColumnMappingQuery[E, M, V] = Minus(this, other)
	def intersect[E <: F](other :ColumnMappingQuery[E, M, V]) :ColumnMappingQuery[E, M, V] = Intersect(this, other)

//	override def anchor(from :F) :ColumnMappingQuery[F, M, V] //= this
//
//	override def basedOn[U <: F, E <: RowProduct](base :E)(implicit ext :U PartOf E) :ColumnMappingQuery[E, M, V]
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:ColumnMappingQuery[E, M, V]

//		override def visit[Y[-_ >: Grouped <: GlobalScope, _]]
//		                    (matcher :AnyColumnVisitor[F, Y]) :Y[GlobalScope, Rows[V]] =
//			matcher.mappingQuery(this)
}


object ColumnMappingQuery {
	trait SpecificColumnMappingQueryVisitor[+F <: RowProduct, X, +Y]
		extends SpecificCompoundSelectColumnAsVisitor[F, X, Y] with SpecificSelectColumnAsVisitor[F, X, Y]
	{
		def columnMappingQuery[M[O] <: BaseColumn[R, O], R]
		                      (e :ColumnMappingQuery[F, M, R])(implicit isRows :X =:= Rows[R]) :Y
	}
	type MatchSpecificColumnMappingQuery[+F <: RowProduct, X, +Y] = SpecificColumnMappingQueryVisitor[F, X, Y]

	trait CaseSpecificColumnMappingQuery[+F <: RowProduct, X, +Y] extends MatchSpecificColumnMappingQuery[F, X, Y] {
		override def selectColumnAs[H[O] <: BaseColumn[R, O], R]
		                           (e :SelectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			columnMappingQuery(e)

		override def compoundSelectColumnAs[H[O] <: BaseColumn[R, O], R]
		                                   (e :CompoundSelectColumnAs[F, H, R])(implicit isRows :X =:= Rows[R]) :Y =
			columnMappingQuery(e)
		}
//
//
//	trait ColumnMappingQueryVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends CompoundSelectColumnAsVisitor[F, Y] with SelectColumnAsVisitor[F, Y]
//	{
//		def columnMappingQuery[M[O] <: BaseColumn[S, O], S]
//		                      (e :ColumnMappingQuery[F, M, S]) :Y[Single, Rows[S], ColumnMappingQuery[F, M, S]]
//	}
//	type MatchColumnMappingQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		ColumnMappingQueryVisitor[F, Y]
//
//	trait CaseColumnMappingQuery[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchColumnMappingQuery[F, Y]
//	{
//		override def selectColumnAs[M[O] <: BaseColumn[S, O], S]
//		                           (e :SelectColumnAs[F, M, S]) :Y[Single, Rows[S], SelectColumnAs[F, M, S]] =
//			columnMappingQuery(e)
//
//		override def compoundSelectColumnAs[M[O] <: BaseColumn[S, O], S](e :CompoundSelectColumnAs[F, M, S])
//				:Y[Single, Rows[S], CompoundSelectColumnAs[F, M, S]] =
//			columnMappingQuery(e)
//	}


	trait AnyColumnMappingQueryVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyCompoundSelectColumnAsVisitor[F, Y] with AnySelectColumnAsVisitor[F, Y]
	{
		def columnMappingQuery[M[O] <: BaseColumn[V, O], V](e :ColumnMappingQuery[F, M, V]) :Y[Single, Rows[V]]
	}
	type MatchAnyColumnMappingQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] =
		AnyColumnMappingQueryVisitor[F, Y]

	trait CaseAnyColumnMappingQuery[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends MatchAnyColumnMappingQuery[F, Y]
	{
		override def selectColumnAs[H[O] <: BaseColumn[V, O], V](e :SelectColumnAs[F, H, V]) :Y[Single, Rows[V]] =
			columnMappingQuery(e)

		override def compoundSelectColumnAs[M[O] <: BaseColumn[V, O], V]
		                                   (e :CompoundSelectColumnAs[F, M, V]) :Y[Single, Rows[V]] =
			columnMappingQuery(e)
	}
}
