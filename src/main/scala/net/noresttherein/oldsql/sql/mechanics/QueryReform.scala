package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.tailrec

import net.noresttherein.oldsql.exceptions.MismatchedExpressionsException
import net.noresttherein.oldsql.slang.mappingMethods
import net.noresttherein.oldsql.sql.{CompoundSelect, Query, RowProduct, Select}
import net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate
import net.noresttherein.oldsql.sql.Query.{QueryReformingTemplate, QueryTemplate, SingleQuery}
import net.noresttherein.oldsql.sql.Select.{SelectOperator, UnionAll}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.ast.{ColumnQuery, CompoundSelectColumn, CompoundSelectIdSQL, CompoundSelectSQL, QuerySQL, SelectColumn, SelectIdSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.QuerySQL.ReformableQuerySQL
import net.noresttherein.oldsql.sql.ast.SelectColumn.TopSelectColumn
import net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL
import net.noresttherein.oldsql.sql.mechanics.QueryReform.{BottomUpQueryReformDecorator, OptimisticQueryReformDecorator, ReformOverrideDecorator, TopDownQueryReformDecorator}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.{DetailedReformPermissions, Permissions, ReformPermissionsDecorator}
import net.noresttherein.oldsql.sql.mechanics.ReformPermissions.Permissions.{MayReform, MayReformLeft, MayReformRight}




/**
  * @define Self `QueryReform`
  * @define self query reform
  */
trait QueryReform extends ReformPermissions[QueryReform] {
	//This class is separate from Reform because it is impossible to implement swap for reforming two, or even more,
	// queries, and it would be confusing anyway. Additionally, the functionality is largely independent,
	// and having them as separate types avoids type inference issues between the multitude of apply methods.

	/** Reforms the ''select'' clauses of all member ''selects'' of `left` and `right` queries in order to achieve
	  * unified column sets between them. The method assumes that both queries are internally unified,
	  * that is, that ''compound selects'' include only queries with matching column sets.
	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
	  * which delegates to method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] of the query.
	  *
	  * Default implementation simply delegates the responsibility to the left query:
	  * `left.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.realign reform]]`(right)(this)`.
	  * The above method proceeds recursively down both queries, delegating to their more specific variants
	  * accepting [[net.noresttherein.oldsql.sql.ast.QuerySQL.SingleQuerySQL SingleQuerySQL]]
	  * and [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]], which eventually results in reforming
	  * of the ''select'' clauses between ''select'' pairs using the most generic `apply` method.
	  * In the process, this instance may be substituted by another `Reform`, either due to permission modifications,
	  * or through [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]/[[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]].
	  *
	  * This method is typically overridden only by implementations dedicated specifically to reforming two queries,
	  * as the algorithm and scope differs from the standard reforming of two arbitrary expressions.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform]]
	  */
	def apply[F <: RowProduct, R](left :QuerySQL[F, R], right :QuerySQL[F, R])
	                             (implicit spelling :SQLSpelling) :(QuerySQL[F, R], QuerySQL[F, R]) =
		left.`->reform`(right)(self)

//	/** A forwarder method to
//	  * `(left `[[net.noresttherein.oldsql.sql.ast.QuerySQL.reform_: reform_:]]` right)(this)`. It is called
//	  * in a dispatch chain started by the overloaded method with two `QuerySQL` arguments.
//	  *
//	  * The method assumes that both queries are internally unified,
//	  * that is, that ''compound selects'' include only queries with matching column sets.
//	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
//	  * which delegates to method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] of the query.
//	  */
//	def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
//	                             (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
//		(left `->reform_:` right)(this)
//
//	/** A forwarder method to
//	  * `(left `[[net.noresttherein.oldsql.sql.ast.QuerySQL.reform_: reform_:]]` right)(this)`. It is called
//	  * in a dispatch chain started by the overloaded method with two `QuerySQL` arguments.
//	  *
//	  *  The method assumes that both queries are internally unified,
//	  * that is, that ''compound selects'' include only queries with matching column sets.
//	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
//	  * which delegates to method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] of the query.	  *
//	  */
//	def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])
//	                             (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
//		(left `->reform_:` right)(this)
//
////	//these two methods are needed only so that swap also swaps arguments to query related methods, but this is not a feature we need
////	/** A swapped variant of `apply(SingleQuerySQL QuerySQL)` existing to allow a proper implementation
////	  * of `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]].
////	  * Equivalent to `this.swap(right, left).swap`.
////	  */
////	def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
////	                             (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
////		(right `->reform_:` left)(swap).swap
////
////	/** A swapped variant of `apply(SelectSQL QuerySQL)` existing to allow a proper implementation
////	  * of `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]].
////	  * Equivalent to `this.swap(right, left).swap`.
////	  */
////	def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
////	                             (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
////		(right `->reform_:` left)(swap).swap
//
//
//
	//It can't return a CompoundSelect because left and right may have different parameters because of ComposedQuery.
	//As it is, we must resign to calling default(left, operator, right) afterwards.
	/** Reforms the ''select'' clauses of all member ''selects'' of `left` and `right` queries in order to achieve
	  * unified column sets between them. The method assumes that both queries are internally unified,
	  * that is, that ''compound selects'' include only queries with matching column sets.
	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
	  * which delegates to method [[net.noresttherein.oldsql.sql.Query.reformed reformed]] of the query.
	  *
	  * Default implementation simply delegates the responsibility to the left query:
	  * `left.`[[net.noresttherein.oldsql.sql.Query.reform reform]]`(right)(this)`.
	  * The above methods proceeds recursively down both queries, delegating to more specific variants of this method,
	  * accepting [[net.noresttherein.oldsql.sql.Query.SingleQuery SingleQuery]]
	  * and [[net.noresttherein.oldsql.sql.Select Select]], which eventually results in reforming
	  * of the ''select'' clauses between ''select'' pairs using the most generic `apply` method.
	  * In the process, this instance may be substituted by another `Reform`, either due to permission modifications,
	  * or through [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]/[[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]].
	  *
	  * This method is typically overridden only by implementations dedicated specifically to reforming two queries,
	  * as the algorithm and scope differs from the standard reforming of two arbitrary expressions.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform]]
	  */
	def apply[X, Y, R](left :Query[X, R], right :Query[Y, R])
	                  (implicit spelling :SQLSpelling) :(Query[X, R], Query[Y, R]) =
		left.`->reform`(right)(self)
//
//	/** A forwarder method to
//	  * `(left `[[net.noresttherein.oldsql.sql.Query.reform_: reform_:]]` right)(this)`. It is called
//	  * in a dispatch chain started by the overloaded method with two `QuerySQL` arguments.
//	  *
//	  * The method assumes that both queries are internally unified,
//	  * that is, that ''compound selects'' include only queries with matching column sets.
//	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
//	  * which delegates to method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] of the query.
//	  */
//	def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])
//	                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//		(left `->reform_:` right)(this)
//
//	/** A forwarder method to
//	  * `(left `[[net.noresttherein.oldsql.sql.Query.reform_: reform_:]]` right)(operator, this)`. It is called
//	  * in a dispatch chain started by the overloaded method with two `QuerySQL` arguments.
//	  *
//	  * The method assumes that both queries are internally unified,
//	  * that is, that ''compound selects'' include only queries with matching column sets.
//	  * This can be achieved by invoking the single argument `apply` method of this instance for both queries,
//	  * which delegates to method [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]] of the query.
//	  */
//	def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])
//	                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//		(left `->reform_:` right)(this)

//	//these two methods are needed only so that swap also swaps arguments to query related methods, but this is not a feature we need
//	/** A swapped variant of `apply(SingleQuery Query)` existing to allow a proper implementation
//	  * of `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]].
//	  * Equivalent to `this.swap(right, left).swap`.
//	  */
//	def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])
//	                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//		(right `->reform_:` left)(swap).swap
//
//	/** A swapped variant of `apply(Select Query)` existing to allow a proper implementation
//	  * of `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.swap swap]].
//	  * Equivalent to `this.swap(right, left).swap`.
//	  */
//	def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])
//	                  (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//		(right `->reform_:` left)(swap).swap

	/** Reform the ''select'' clauses of all member ''selects'' in the given query in order to achieve
	  * a unified ''select'' clause shape. The default implementation
	  * delegates to `query.`[[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.reformed reformed]].
	  * That query will normally call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default default]]
	  * passing itself as an argument in a double dispatch.
	  *
	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
	  * and/or modify the query's form.
	  */
	def apply[R, Res](query :QueryReformingTemplate[R, Res])(implicit spelling :SQLSpelling) :Res =
		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given
//	  * [[net.noresttherein.oldsql.sql.Query Query]] in order to achieve a unified ''select'' clause shape. The default
//	  * implementation delegates to `query.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.reformed reformed]].
//	  * That query will normally call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default default]]
//	  * passing itself as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[P, R, Res]
//	         (query :ReformableQuery[P, R, Res])(implicit spelling :SQLSpelling) :Res =
//		(query :QueryReformingTemplate[R, Res]).`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given
//	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]] in order to achieve a unified ''select'' clause shape.
//	  * The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.Query.QueryTemplate.reformed reformed]].
//	  * That query will normally call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default default]]
//	  * passing itself as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[F <: RowProduct, R, Res](query :ReformableQuerySQL[F, R, Res])(implicit spelling :SQLSpelling) :Res =
//		(query :QueryReformingTemplate[R, Res]).`->reformed`(self)

//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given query
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.Query.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given query
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.Query.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[P, V](query :SingleQuery[P, V])(implicit spelling :SQLSpelling) :SingleQuery[P, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' of the given ''compound select''
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is exactly equivalent to the overloaded variant
//	  * accepting a [[net.noresttherein.oldsql.sql.Query Query]], except for a more specific return type.
//	  * It is ''not'' a double dispatch callback. It is overridden only by specialized implementations,
//	  * such as those adding synthetic columns and/or modify the query's form.
//	  */
//	def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given query
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' in the given query
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is overridden only by specialized implementations, such as those adding synthetic columns
//	  * and/or modify the query's form.
//	  */
//	def apply[F <: RowProduct, V](query :ColumnQuery[F, V])(implicit spelling :SQLSpelling) :ColumnQuery[F, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' of the given ''compound select''
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is exactly equivalent to the overloaded variant
//	  * accepting a [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]], except for a more specific return type.
//	  * It is ''not'' a double dispatch callback. It is overridden only by specialized implementations,
//	  * such as those adding synthetic columns and/or modify the query's form.
//	  */
//	def apply[F <: RowProduct, V]
//	         (query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
//		query.`->reformed`(self)
//
//	/** Reform the ''select'' clauses of all member ''selects'' of the given ''compound select''
//	  * in order to achieve a unified ''select'' clause shape. The default implementation delegates
//	  * to `query.`[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reformed]]. That query will normally
//	  * call back `this.`[[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] passing itself
//	  * as an argument in a double dispatch.
//	  *
//	  * This method is exactly equivalent to the overloaded variant
//	  * accepting a [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]], except for a more specific return type.
//	  * It is ''not'' a double dispatch callback. It is overridden only by specialized implementations,
//	  * such as those adding synthetic columns and/or modify the query's form.
//	  */
//	def apply[F <: RowProduct, V]
//	         (query :CompoundSelectColumn[F, V])(implicit spelling :SQLSpelling) :CompoundSelectColumn[F, V] =
//		query.`->reformed`(self)


//
//	/** A fallback method called by a `QuerySQL` instance when it gives up on reforming its select clause(s)
//	  * (for ''compound selects'') with that/those of another query, with which it is intended to be joined
//	  * with a [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]].
//	  * Implementations will likely do very little here aside from validating if the expressions are already aligned
//	  * with each other and throwing an exception if it is not the case.
//	  */
//	@throws[MismatchedExpressionsException]("if the expressions' shapes do not match.")
//	@throws[UnsupportedOperationException]("if any select clause in either of queries contains a LooseComponent.")
//	def fallback[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
//	                                (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
//	{
//		validate(left, right)
//		(left, right)
//	}
//
//	/** A fallback method called by a `Query` instance when it gives up on reforming its select clause(s)
//	  * (for ''compound selects'') with that/those of another query, with which it is intended to be joined
//	  * with a [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]].
//	  * Implementations will likely do very little here aside from validating if the expressions are already aligned
//	  * with each other and throwing an exception if it is not the case.
//	  */
//	@throws[MismatchedExpressionsException]("if the expressions' shapes do not match.")
//	@throws[UnsupportedOperationException]("if any select clause in either of queries contains a LooseComponent.")
//	def fallback[X, Y, V](left :Query[X, V], right :Query[Y, V])
//                         (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//	{
//		validate(left, right)
//		(left, right)
//	}
//

	//todo: this really needs to be generalized
	//todo: extract docs into single $defs
	/** Reforms the ''select'' clause of the given query so that it conforms to the constraints enforced by this
	  * strategy. The default implementation is an identity function; this method is currently overridden only
	  * by specialized implementations which require adding of synthetic id/key/discriminator columns to
	  * all member ''selects'' of a ''compound select''.
	  */ //or we could accept only the select clause expression to distinguish it from methods above, as it goes in the other direction
	def default[F <: RowProduct, R](query :SelectSQL[F, R])(implicit spelling :SQLSpelling) :SelectSQL[F, R] = query
//
//	/** Reforms the ''select'' clause of the given query so that it conforms to the constraints enforced by this
//	  * strategy. The default implementation is an identity function; this method is currently overridden only
//	  * by specialized implementations which require adding of synthetic id/key/discriminator columns to
//	  * all member ''selects'' of a ''compound select''.
//	  */ //or we could accept only the select clause expression to distinguish it from methods above, as it goes in the other direction
//	def default[F <: RowProduct, V](query :SelectColumn[F, V])(implicit spelling :SQLSpelling) :SelectColumn[F, V] =
//		query

	/** Reforms the ''select'' clause of the given query so that it conforms to the constraints enforced by this
	  * strategy. The default implementation is an identity function; this method is currently overridden only
	  * by specialized implementations which require adding of synthetic id/key/discriminator columns to
	  * all member ''selects'' of a ''compound select''.
	  */
	def default[R](query :TopSelectSQL[R])(implicit spelling :SQLSpelling) :TopSelectSQL[R] = query
//
//	/** Reforms the ''select'' clause of the given query so that it conforms to the constraints enforced by this
//	  * strategy. The default implementation is an identity function; this method is currently overridden only
//	  * by specialized implementations which require adding of synthetic id/key/discriminator columns to
//	  * all member ''selects'' of a ''compound select''.
//	  */
//	def default[V](query :TopSelectColumn[V])(implicit spelling :SQLSpelling) :TopSelectColumn[V] = query

	/** Reforms the ''select'' clause of the given query so that it conforms to the constraints enforced by this
	  * strategy. The default implementation is an identity function; this method is currently overridden only
	  * by specialized implementations which require adding of synthetic id/key/discriminator columns to
	  * all member ''selects'' of a ''compound select''.
	  */
	def default[P, R](query :Select[P, R])(implicit spelling :SQLSpelling) :Select[P, R] = query

	/** Method called by default from method [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reformed]]
	  * of [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]],
	  * giving this `Reform` the permission to proceed with reforming all ''select'' clauses of its
	  * (recursively included) member ''selects'' to a common [[net.noresttherein.oldsql.sql.SQLExpression.shape shape]].
	  * It recursively proceeds to reform both sides of the argument
	  * (using its [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * and [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]] sub reforms) and combines the results
	  * with [[net.noresttherein.oldsql.sql.mechanics.QueryReform.reformed reformed]].
	  */
	def default[F <: RowProduct, R](query :CompoundSelectSQL[F, R])
	                               (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
	{
		val l = left(query).apply(query.left)
		val r = right(query).apply(query.right)
		val (reformedLeft, reformedRight) = prohibit(spelling.allowedReforms(query))(l, r)
		if ((reformedLeft eq query.left) & (reformedRight eq query.right))
			query
		else
			reformed(reformedLeft, query.operator, reformedRight)
	}

	/** Method called by default from method [[net.noresttherein.oldsql.sql.ast.CompoundSelectColumn.reformed reformed]]
	  * of [[net.noresttherein.oldsql.sql.ast.CompoundSelectColumn CompoundSelectColumn]],
	  * giving this `Reform` the permission to proceed with reforming all ''select'' clauses of its
	  * (recursively included) member ''selects'' to a common [[net.noresttherein.oldsql.sql.SQLExpression.shape shape]].
	  * It recursively proceeds to reform both sides of the argument
	  * (using its [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * and [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]] sub reforms) and combines the results
	  * with [[net.noresttherein.oldsql.sql.mechanics.QueryReform.reformed reformed]].
	  */
	def default[F <: RowProduct, R](query :CompoundSelectColumn[F, R])
	                               (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
	{ //this should work differently than the rest, because it doesn't care about SelectId - may ignore query.defaultReform
		//todo: far from perfect, but we don't have reform/reform_: methods in CompoundSelectColumn
		validate(query.left, query.right)
		query
	}

	/** Method called by default from method [[net.noresttherein.oldsql.sql.CompoundSelect.reformed reformed]]
	  * of [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]],
	  * giving this `Reform` the permission to proceed with reforming all ''select'' clauses of its
	  * (recursively included) member ''selects'' to a common [[net.noresttherein.oldsql.sql.SQLExpression.shape shape]].
	  * It recursively proceeds to reform both sides of the argument
	  * (using its [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * and [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]] sub reforms) and combines the results
	  * with [[net.noresttherein.oldsql.sql.mechanics.QueryReform.reformed reformed]].
	  */
	def default[P, R](query :CompoundSelect[P, R])(implicit spelling :SQLSpelling) :CompoundSelect[P, R] = {
//		val inLeft = spelling.inLeft(query)
//		val inRight = spelling.inRight(query)
//		val l = inLeft.queryReform(query.left)(inLeft)
//		val r = inRight.queryReform(query.right)(inRight)
		val l = left(query).apply(query.left)
		val r = right(query).apply(query.right)
		val (reformedLeft, reformedRight) = prohibit(spelling.allowedReforms(query))(l, r)
		if ((reformedLeft eq query.left) & (reformedRight eq query.right))
			query
		else
			reformed(reformedLeft, query.operator, reformedRight)
	}


	/** Checks if the argument queries already have a unified form.
	  * The default implementation checks if their [[net.noresttherein.oldsql.sql.RowShape shapes]]
	  * are [[net.noresttherein.oldsql.sql.RowShape.<:> compatible]], but this can be overridden.
	  * This method is used directly by [[net.noresttherein.oldsql.sql.mechanics.Reform.validate validate]]
	  * and, indirectly, by all [[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] methods.
	  */
	def compatible[F <: RowProduct, R]
	              (left :QuerySQL[F, R], right :QuerySQL[F, R])(implicit spelling :SQLSpelling) :Boolean =
		try pairWiseCompatible(left, right) catch {
			case _ :MismatchedExpressionsException => false
		}

	/** Checks if the argument queries already have a unified form.
	  * The default implementation checks if their [[net.noresttherein.oldsql.sql.RowShape shapes]]
	  * are [[net.noresttherein.oldsql.sql.RowShape.<:> compatible]], but this can be overridden.
	  * This method is used directly by [[net.noresttherein.oldsql.sql.mechanics.Reform.validate validate]]
	  * and, indirectly, by all [[net.noresttherein.oldsql.sql.mechanics.Reform.default fallback]] methods.
	  */
	def compatible[X, Y, R](left :Query[X, R], right :Query[Y, R])(implicit spelling :SQLSpelling) :Boolean =
		try pairWiseCompatible(left, right) catch {
			case _ :MismatchedExpressionsException => false
		}

	private def pairWiseCompatible[R, Q[A] <: QueryTemplate[A, Q]]
	                              (left: QueryTemplate[R, Q], right: QueryTemplate[R, Q])
	                              (implicit spelling :SQLSpelling) =
	{
		val reform = this.reform
		val leftSelects = left.constituents
		val rightSelects = right.constituents
		leftSelects.zipForall(leftSelects.tail) {
			(select1, select2) => reform.compatible(select1.selectClause, select2.selectClause)
		} && reform.compatible(leftSelects.last.selectClause, rightSelects.head.selectClause) && {
			rightSelects.zipForall(rightSelects.tail) {
				(select1, select2) => reform.compatible(select1.selectClause, select2.selectClause)
			}
		}
	}

	/** A convenience method throwing
	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * if check [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]]`(left, right)` fails.
	  */
	protected def validate[F <: RowProduct, R]
	                      (left :QuerySQL[F, R], right :QuerySQL[F, R])(implicit spelling :SQLSpelling) :Unit =
		if (!pairWiseCompatible(left, right))
			throw new MismatchedExpressionsException(left, right)

	/** A convenience method throwing
	  * a [[net.noresttherein.oldsql.exceptions.MismatchedExpressionsException MismatchedExpressionsException]]
	  * if check [[net.noresttherein.oldsql.sql.mechanics.Reform.compatible compatible]]`(left, right)` fails.
	  */
	protected def validate[X, Y, R](left :Query[X, R], right :Query[Y, R])(implicit spelling :SQLSpelling) :Unit =
		if (!pairWiseCompatible(left, right))
			throw new MismatchedExpressionsException(left, right)

	//I really don't like having these two methods public, as they absolutely require
	// that CompoundSelectSQL(left, operator, right) is a perfectly reformed instance, because it caches itself
	// as the result of `reformed`. We however need it for the implementation of QuerySQL.reform.

	/** Given two unified queries, create a ''compound select'' combining them into a single query.
	  * Both arguments must be results of reforming two sides of a `CompoundSelectSQL` reformed with this instance.
	  * This method should be called as a final step of reforming a `CompoundSelectSQL`
	  * which contained the original, non unified versions of `left` and `right`.
	  */
	protected def reformed[F <: RowProduct, R](left :QuerySQL[F, R], operator :SelectOperator, right :QuerySQL[F, R])
	                                          (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
	{   /* We could just call CompoundSelectSQL factory method directly and pass ourselves to create a 'reformed'
		 * instance, but that would not allow custom operators to create special implementations of CompoundSelectSQL.
		 * It is not clear though if the latter is actually something we'd want to support, because SelectOperator
		 *  seems to have enough power in itself. */
		val res = operator(left, right)
		res.reformedBy = (this, spelling)
		res
	}

	/** Given two unified queries, create a ''compound select'' combining them into a single query.
	  * Both arguments must be results of reforming two sides of a `CompoundSelectSQL` reformed with this instance.
	  * This method should be called as a final step of reforming a `CompoundSelectSQL`
	  * which contained the original, non unified versions of `left` and `right`.
	  */
	protected def reformed[F <: RowProduct, R](left :ColumnQuery[F, R], operator :SelectOperator, right :ColumnQuery[F, R])
	                                          (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
	{   /* We could just call CompoundSelectSQL factory method directly and pass ourselves to create a 'reformed'
		 * instance, but that would not allow custom operators to create special implementations of CompoundSelectSQL.
		 * It is not clear though if the latter is actually something we'd want to support, because SelectOperator
		 *  seems to have enough power in itself. */
		val res = operator(left, right)
		res.reformedBy = (this, spelling)
		res
	}

	/** Given two unified queries, create a ''compound select'' combining them into a single query.
	  * Both arguments must be results of reforming two sides of a `CompoundSelect` reformed with this instance.
	  * This method should be called as a final step of reforming a `CompoundSelect`
	  * which contained the original, non unified versions of `left` and `right`.
	  *
	  * Note that [[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.isReformed isReformed]] property
	  * of the returned compound select will be `true`, so implementations which do not complete the job are responsible
	  * for ensuring that that instance does not leak to the application.
	  */
	protected def reformed[P, R](left :Query[P, R], operator :SelectOperator, right :Query[P, R])
	                            (implicit spelling :SQLSpelling) :CompoundSelect[P, R] =
	{
		val res = operator(left, right)
		res.reformedBy = (this, spelling)
		res
	}



	/** A `Reform` to use for unifying the left and right sides of a
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]].
	  * When a ''compound select'' is to be spelled, its both sides must be unified first.
	  * If it is not a part of a larger ''compound select'', it uses for this purpose a fresh
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]] obtained from
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]. However, it's left and right side,
	  * if any of them are also a ''compound select'', will - in method
	  * [[net.noresttherein.oldsql.sql.Query.reformed reformed]] invoked by the enclosing ''compound select'' -
	  * use this method instead to obtain a `QueryReform` which should be used.
	  *
	  * The default implementation simply returns
	  * `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]`(query)`,
	  * making reforming a ''compound select'' independent of any other ''compound select'' it might
	  * be a part of (i.e., reforming of `select1 union select2` in `select1 union select2 minus select3`
	  * ignores the reform used for reforming the result with `minus select3`.
	  * Implementations may however override this method to introduce such dependence.
	  *
	  * The permissions of the returned reform are undefined unless this instance was created
	  * by [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * or [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]].
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.TopDownQueryReform]]
	  */ //consider: renaming to compound
//	def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :QueryReform =
//		if (spelling.scope == TopScope) query.topReform else query.defaultReform

	/** A `QueryReform` to use for unifying the left and right sides of a
	  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]].
	  * The default implementation simply returns [[net.noresttherein.oldsql.sql.mechanics.QueryReform.self self]],
	  * but some implementations return
	  * `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]`(query)`,
	  * making reforming a ''compound select'' independent on any other ''compound select'' it might
	  * be a part of (i.e., reforming of `select1 union select2` in `select1 union select2 minus select3`
	  * ignores the reform used for reforming the result with `minus select3`.
	  *
	  * The permissions of the returned reform are undefined unless this instance was created
	  * by [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * or [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]].
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.TopDownQueryReform]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.BottomUpQueryReform]]
	  */
//	def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :QueryReform =
//		if (spelling.scope == TopScope) query.topReform else query.defaultReform
	def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
                (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		//consider: if we shouldn't return query.defaultReform as the default implementation
		prohibit(spelling.allowedReforms(query))
//		self

//	  * That method allows to provide an implementation dedicated to a certain operator type. The default implementation
//	  * returns `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.left left]]`.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.defaultReform defaultReform]]`.`[[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibit prohibit]]`(`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.leftSubqueryPermissions leftSubqueryPermissions]]`)`.
	/** A `Reform` to use for initial reforming the first of two queries combined into a ''compound select''.
	  * This method may be called only on a `QueryReform` instance returned by method
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound compound]] of another query reform.
	  *
	  * When [[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.reformed reformed]] method of a
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * is called, it invokes [[net.noresttherein.oldsql.sql.mechanics.QueryReform.default default]] method of
	  * passed `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]].
	  * While implementations of that method vary, they generally need to either unify the left and right
	  * sides of the query independently, or at least otherwise prepare it for aligning with each other.
	  * This method is then called to recurse down the left branch in the reformed compound select.
	  *
	  * The permissions of the returned `Reform` depend on implementation. Two standard schemes exist:
	  *   1. [[net.noresttherein.oldsql.sql.mechanics.QueryReform.isBottomUp 'Bottom up']]:
	  *      using the default permissions as defined by `spelling`, interpreting the permissions of this instance
	  *      as applying to unification of already internally consistent expressions. In this case the reforming
	  *      happens from the bottom to the top.
	  *   1. [[net.noresttherein.oldsql.sql.mechanics.QueryReform.isTopDown 'Top down']]:
	  *      propagating left side permissions of this instance to apply also to the right side.
	  *      This forces the internal unification of the query in the previous example to comply with constraints
	  *      defined for the overall unification. In this case the reforming happens from the top to the bottom.
	  *
	  * @return [[net.noresttherein.oldsql.sql.mechanics.QueryReform.prohibit prohibit]]`(this.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.leftSubqueryPermissions, leftSubqueryPermissions]]`)`.
	  */
	def left(implicit spelling :SQLSpelling) :QueryReform = prohibit(leftSubqueryPermissions)

	/** A `Reform` to use for initial reforming the second of two queries combined into a ''compound select''.
	  * This method may be called only on a `QueryReform` instance returned by method
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound compound]] of another query reform.
	  *
	  * When [[net.noresttherein.oldsql.sql.Query.QueryReformingTemplate.reformed reformed]] method of a
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * is called, it invokes [[net.noresttherein.oldsql.sql.mechanics.QueryReform.default default]] method of
	  * passed `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]].
	  * While implementations of that method vary, they generally need to either unify the left and right
	  * sides of the query independently, or at least otherwise prepare it for aligning with each other.
	  * This method is then called to recurse down the right branch in the reformed compound select.
	  *
	  * The permissions of the returned `Reform` depend on implementation. Two standard schemes exist:
	  *   1. [[net.noresttherein.oldsql.sql.mechanics.QueryReform.isBottomUp 'Bottom up']]:
	  *      using the default permissions as defined by `spelling`, interpreting the permissions of this instance
	  *      as applying to unification of already internally consistent expressions. In this case the reforming
	  *      happens from the bottom to the top.
	  *   1. [[net.noresttherein.oldsql.sql.mechanics.QueryReform.isTopDown 'Top down']]:
	  *      propagating left side permissions of this instance to apply also to the right side.
	  *      This forces the internal unification of the query in the previous example to comply with constraints
	  *      defined for the overall unification. In this case the reforming happens from the top to the bottom.
	  *
	  * @return [[net.noresttherein.oldsql.sql.mechanics.QueryReform.prohibit prohibit]]`(this.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.rightSubqueryPermissions, rightSubqueryPermissions]]`)`.
	  */
	def right(implicit spelling :SQLSpelling) :QueryReform = prohibit(rightSubqueryPermissions)

	/** A `Reform` to use for initial reforming
	  * the [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.left left]] of the two subqueries
	  * of `query`.
	  * @return [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound compound]]`(query).`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]].
	  */
	def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		    (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		compound(query).left
//		prohibit(spelling.allowedReforms(query) & leftSubqueryPermissions)

	/** A `Reform` to use for initial reforming
	  * the [[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.right right]] of the two subqueries
	  * of `query`.
	  * @return [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound compound]]`(query).`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]].
	  */
	def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	         (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
		compound(query).right
//		prohibit(spelling.allowedReforms(query) & rightSubqueryPermissions)

	//I'd rather these were protected, but it poses problems with decorators.
	/** Additional restrictions applied to the left subquery when this reform is used to align a ''compound select''.
	  * Passed as an argument to [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibit prohibit]]
	  * or a constructor arguments to a new instance.
	  */
	def leftSubqueryPermissions  :Permissions = MayReform

	/** Additional restrictions applied to the left subquery when this reform is used to align a ''compound select''.
	  * Passed as an argument to [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibit prohibit]]
	  * or a constructor arguments to a new instance.
	  */
	def rightSubqueryPermissions :Permissions = MayReform

	/** Creates an optimistic variant of this reform, which will first always test
	  * if the argument(s) are already aligned,
	  * using method [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compatible compatible]].
	  * Only if the check fails, the call is delegated to this reform.
	  * If this reform already is optimistic, the call has no effect.
	  */
	def optimistic :QueryReform = decorate(OptimisticQueryReformDecorator)

	/** A ''bottom-up'' $Self applies its permissions only to reforming the ''compound select''/query pair
	  * it was tasked with, and leaves recursive internal reforming to independent instances.
	  * The process of shape unification is recursive, in which left and right subqueries
	  * of a ''compound select'' must either be reformed internally, or otherwise prepared before they are aligned
	  * with each other, A ''bottom-up'' reform will start with reforming of each expression pair with first,
	  * independently, using [[net.noresttherein.oldsql.sql.Select.SelectOperator.allowedReforms default permissions]]
	  * of the reformed ''compound select''
	  * Only when both subqueries are internally consistent and have definite shapes, reform's permissions
	  * are taken in account when aligning them with each other.
	  * In contrast, a ''top-down'' $Self propagates its permissions to the process of internally reforming
	  * of each subquery: for a left query in a ''compound select'',
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.leftSubqueryPermissions left permissions]] of this reform
	  * are used as both left and right permissions (and vice versa). This is on top of whatever restrictions
	  * are introduced by the combining [[net.noresttherein.oldsql.sql.Select.SelectOperator operator]].
	  *
	  * This distinction is not a dichotomy: a reform can be neither of these two, or simply the information
	  * on how the process is carried out is too distributed and cannot be provided by a $Self.
	  *
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.BottomUpQueryReform]]
	  */
	def isBottomUp :Boolean = !isTopDown

	/** A ''top-down'' $Self applies its permissions to the whole process of reforming a query for which it was created,
	  * including internal reforming of its subqueries.
	  * The process of shape unification is recursive, in which left and right subqueries
	  * of a ''compound select'' must be reformed internally before they are reformed with each other.
	  * In case of a ''top-down'' query reform, when recursion descends to the left subquery,
	  * right permissions of this reform are initialized to its left permissions, and vice versa.
	  * These restrictions are applied on top of whatever restrictions are introduced by a particular
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * (or rather, in practice, their [[net.noresttherein.oldsql.sql.Select.SelectOperator operator]]).
	  * This contrasts with ''bottom-up'' reforms, which consider reforming of subexpressions of an expression
	  * as a separate process, which is carried out with an independent $Self instance. Only when the two,
	  * internally reformed expressions need to be aligned with each other, reform's permissions are taken into account.
	  *
	  * This distinction is not a dichotomy: a reform can be neither of these two, or simply the information
	  * on how the process is carried out is too distributed and cannot be provided by a $Self.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.TopDownQueryReform]]
	  */
	def isTopDown :Boolean = false

	/** A ''bottom-up'' $Self applies its permissions only to reforming the ''compound select''/query pair
	  * it was tasked with, and leaves recursive internal reforming to independent instances.
	  * The process of shape unification is recursive, in which left and right subqueries
	  * of a ''compound select'' must either be reformed internally, or otherwise prepared before they are aligned
	  * with each other, A ''bottom-up'' reform will start with reforming of each expression pair with first,
	  * independently, using [[net.noresttherein.oldsql.sql.Select.SelectOperator.allowedReforms default permissions]]
	  * of the reformed ''compound select''
	  * Only when both subqueries are internally consistent and have definite shapes, reform's permissions
	  * are taken in account when aligning them with each other.
	  * In contrast, a ''top-down'' $Self propagates its permissions to the process of internally reforming
	  * of each subquery: for a left query in a ''compound select'',
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.leftSubqueryPermissions left permissions]] of this reform
	  * are used as both left and right permissions (and vice versa). This is on top of whatever restrictions
	  * are introduced by the combining [[net.noresttherein.oldsql.sql.Select.SelectOperator operator]].
	  *
	  * This distinction is not a dichotomy: a reform can be neither of these two, or simply the information
	  * on how the process is carried out is too distributed and cannot be provided by a $Self.
	  *
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]]
	  */ //consider: taking SQLSpelling as a parameter so we can return spelling.queryReform.bottomUp, if we want
	def bottomUp :QueryReform = if (isBottomUp) self else decorate(BottomUpQueryReformDecorator)

	/** A ''top-down'' $Self applies its permissions to the whole process of reforming a query for which it was created,
	  * including internal reforming of its subqueries.
	  * The process of shape unification is recursive, in which left and right subqueries
	  * of a ''compound select'' must be reformed internally before they are reformed with each other.
	  * In case of a ''top-down'' query reform, when recursion descends to the left subquery,
	  * right permissions of this reform are initialized to its left permissions, and vice versa.
	  * These restrictions are applied on top of whatever restrictions are introduced by a particular
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]]/[[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * (or rather, in practice, their [[net.noresttherein.oldsql.sql.Select.SelectOperator operator]]).
	  * This contrasts with ''bottom-up'' reforms, which consider reforming of subexpressions of an expression
	  * as a separate process, which is carried out with an independent $Self instance. Only when the two,
	  * internally reformed expressions need to be aligned with each other, reform's permissions are taken into account.
	  *
	  * This distinction is not a dichotomy: a reform can be neither of these two, or simply the information
	  * on how the process is carried out is too distributed and cannot be provided by a $Self.
	  *
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]]
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp]]
	  */
	def topDown :QueryReform = if (isTopDown) self else decorate(TopDownQueryReformDecorator)

	/** A regular reform strategy for aligning two arbitrary expressions, used for aligning the ''select'' clauses
	  * of member ''selects'' in the reformed query.
	  */
	def reform(implicit spelling :SQLSpelling) :Reform = spelling.reform.prohibit(self.permissions)

	/** A reform equivalent to this one, but using the specified `Reform` for reforming ''select'' clause expressions
	  * of all SQL ''selects'' in this query. The reform is treated as a template, and permissions
	  * of the `QueryReform` tasked with reforming a particular SQL ''select'' are propagated to it
	  * using [[net.noresttherein.oldsql.sql.mechanics.ReformPermissions.prohibit prohibit]] method.
	  */
	def impose(reform :Reform) :QueryReform = decorate(new ReformOverrideDecorator(reform)(_))

}






/** A factory of standard [[net.noresttherein.oldsql.sql.mechanics.Reform Reform]] strategies used to unify
  * ''select'' clauses of all member ''selects'' in a ''compound select''.
  */
object QueryReform {

	def apply(reform :Reform) :QueryReform = default.impose(reform)

	/** A special `QueryReform` implementations which swaps itself
	  * for `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.defaultReform defaultReform]]
	  * when it is used to reform a ''compound select''.
	  */
	val default :QueryReform = default(true, true)

	def default(mayReformLeft :Boolean, mayReformRight :Boolean) :QueryReform =
		default(MayReformLeft & mayReformLeft | MayReformRight & mayReformRight)

	def default(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean)
	           (mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
			:QueryReform =
		default(Permissions(
			mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft, mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
		))

	def default(permissions :Permissions) :QueryReform =
		new DefaultReform(permissions)(identity)
//		new BasicQueryReform(permissions, identity)

	/** The default [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] implementation used to unify
	  * the ''select'' clauses of all member SQL ''selects'' within a top-level query.
	  * It doesn't implement any strategy of its own, but uses implicit
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] argument to obtain a proper,
	  * 'top level' reforming strategy. It depends on the fact that
	  * [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] and
	  * [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]] always use
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]] of the reform argument
	  * given to [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL.reformed reformed]],
	  * rather than the argument itself. This allows to implement unification algorithms working globally
	  * on the whole query; for example, a ''union'' query by default will add an 'id' column to every
	  * ''select'' clause to track from which SQL ''select'' any given row comes, and thus which read form
	  * should be used for it.
	  */
//	val TopReform :ExpressionReform =
//		new TopReform(true, true, true, true)(true, true, true, true)(identity)

	/** Equivalent to [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp(mayReformLeft* bottomUp]]`(true, true)`.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown topDown]]
	  */
	val bottomUp :QueryReform = bottomUp(true, true)

	/** Returns [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp(mayExcludeLeft* bottomUp]]`(mayReformLeft, mayReformLeft, mayReformLeft, mayReformLeft)(mayReformRight, mayReformRight, mayReformRight, mayReformRight)`.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown(mayReformLeft* topDown]]
	  */
	def bottomUp(mayReformLeft :Boolean, mayReformRight :Boolean) :QueryReform =
		bottomUp(MayReformLeft & mayReformLeft | MayReformRight & mayReformRight)

	/** The default implementation of [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] used to unify
	  * the shapes (column sets) of ''select'' clauses within a ''compound select''.
	  * It recursively and independently reforms both sides
	  * of a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * or a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]] using the reform returned by
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]`.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform queryReform]]`(operator)`,
	  * and only then starts to unify queries reformed in such a way. It follows that the permissions specified
	  * for this instance are ignored in favour of default permissions (and in fact, a default `Reform` implementation)
	  * for a particular [[net.noresttherein.oldsql.sql.Select.SelectOperator SelectOperator]] when reforming each side
	  * individually. They are taken into account only when unifying the results of reformed left and right sides.
	  *
	  * The implementation depends on the reforming algorithm implemented
	  * by [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]
	  * and [[net.noresttherein.oldsql.sql.Query Query]] themselves: for details see
	  * [[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed[E<:F](query* reform]] and
	  * [[net.noresttherein.oldsql.sql.Query.reformed reform]].
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown(mayExcludeLeft* topDown]]
	  */
	def bottomUp(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean)
	            (mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
			:QueryReform =
		bottomUp(Permissions(
			mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft, mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
		))

	def bottomUp(permissions :Permissions) :QueryReform = new BasicBottomUpQueryReform(permissions)(identity)

	/** Equivalent to [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown(mayReformLeft* topDown]]`(true, true)`.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp bottomUp]]
	  */
	val topDown :QueryReform = topDown(true, true)

	/** Returns [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown(mayExcludeLeft* topDown]]`(mayReformLeft, mayReformLeft, mayReformLeft, mayReformLeft)(mayReformRight, mayReformRight, mayReformRight, mayReformRight)`.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp(mayReformLeft* bottomUp]]
	  */
	def topDown(mayReformLeft :Boolean, mayReformRight :Boolean) :QueryReform =
		topDown(MayReformLeft & mayReformLeft | MayReformRight & mayReformRight)

	/** The default implementation of [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]] used to unify
	  * the shapes (column sets) of ''select'' clauses within a ''compound select''.
	  * It starts with recursively reforming both sides
	  * of a [[net.noresttherein.oldsql.sql.ast.CompoundSelectSQL CompoundSelectSQL]]
	  * or a [[net.noresttherein.oldsql.sql.CompoundSelect CompoundSelect]], using reform instances
	  * of the same type, but in which permissions for both sides are equal (left permissions are propagated
	  * to right permissions when reforming the left side, and vice versa for the right side).
	  *
	  * Reforming of the two results is performed using the algorithm implemented
	  * by [[net.noresttherein.oldsql.sql.ast.QuerySQL QuerySQL]]`.`[[net.noresttherein.oldsql.sql.ast.QuerySQL.reformed[E<:F](query* reform]]
	  * and [[net.noresttherein.oldsql.sql.Query Query]]`.`[[net.noresttherein.oldsql.sql.Query.reformed reform]].
	  * See those methods for more details.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown(mayExcludeLeft* topDown]]
	  */
	def topDown(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean)
	            (mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
			:QueryReform =
		topDown(Permissions(
			mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft, mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
		))

	def topDown(permissions :Permissions) :QueryReform = new BasicTopDownQueryReform(permissions)(identity)


	/** A factory of specialized, top level ''union all'' query [[net.noresttherein.oldsql.sql.mechanics.Reform reform]]
	  * strategies, which include an extra column in the ''select'' clauses to track from which member ''select''
	  * any given row of the result came from.
	  */
	object UnionAllReform {
		val DefaultSelectIdColumn :String = "_select_id"

		def apply(base :QueryReform) :QueryReform = base decorate (new TopSelectIdDecorator(DefaultSelectIdColumn, _))

		//Todo: should these methods give permissions without considering query.defaultReform.permissions?
		// A non obvious factor is that these methods may potentially be called from query.defaultReform,
		// leading to an infinite recursion
		/** Equivalent to `bottomUp(query, true, true)`.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.topDown topDown]]
		  */
		val bottomUp :QueryReform = bottomUp(MayReform)

		/** Returns `bottomUp(query, mayReformLeft, mayReformLeft, mayReformLeft, mayReformLeft, mayReformRight, mayReformRight, mayReformRight, mayReformRight)`.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.topDown]]
		  */
		def bottomUp(mayReformLeft :Boolean, mayReformRight :Boolean) :QueryReform =
			bottomUp(MayReformLeft & mayReformLeft | MayReformRight & mayReformRight)

		/** The default implementation of [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]]
		  * used to unify the shapes (column sets) of ''select'' clauses within a top level ''union all select''.
		  * It works very similarly to
		  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]]`.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.bottomUp bottomUp]],
		  * but all member selects of the the query receive an identifier allowing to track which select any given
		  * result set row came from. For queries composed solely using a ''union all'' operator, these identifiers are
		  * all unique. However, if a query term under the given query is a ''compound select'' using another
		  * operator type, all its member ''selects'' receive the same id in order not to interfere with
		  * semantics of intersection or subtraction.
		  *
		  * Despite being a 'bottom-up' reform, it controls the full unification process of all subqueries,
		  * ensuring that the id column is used consistently in all member ''selects''.
		  * The [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]/[[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]]
		  * sub-reforms are instances derived from this class, and the reform provided
		  * by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] is not used.
		  * The instance created here is proper for returning
		  * from `Reform.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]] and `subreform`
		  * should ''not'' be called on it directly (rather than a result of `this.left`/`this.right`).
		  *
		  * As with `QueryReform.bottomUp`, if either of the sides of the argument query or some its subterm query
		  * is also a ''compound select'', it is first reformed independently of the outer query using
		  * a default reform strategy provided by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]
		  * for that operator type. This part of the process ignores the permissions given here in favour
		  * of the permissions of said `reform` instance. Only after that reforming is complete,
		  * this instance starts reforming it with with the other side of the enclosing ''compound select'',
		  * in line with the permissions given here.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.topDown]]
		  */
		def bottomUp(
				mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
				mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
				:QueryReform =
			bottomUp(Permissions(
				mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
				mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
			))

		def bottomUp(permissions :Permissions) :QueryReform =
			new UnionAllTopReform(DefaultSelectIdColumn, "UnionAllReform.bottomUp")(permissions)(
				identity, new BottomUpUnionAllReform(_, _, _)(_)(_)
			)



		/** Equivalent to `topDown(query, true, true)`.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.bottomUp]]
		  */
		val topDown :QueryReform = topDown(MayReform)

		/** Returns `topDown(query, mayReformLeft, mayReformLeft, mayReformLeft, mayReformLeft, mayReformRight, mayReformRight, mayReformRight, mayReformRight)`.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.bottomUp]]
		  */
		def topDown(mayReformLeft :Boolean, mayReformRight :Boolean) :QueryReform =
			topDown(MayReformLeft & mayReformLeft | MayReformRight & mayReformRight)

		/** An alternative implementation of [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]]
		  * used to unify the shapes (column sets) of ''select'' clauses within a top level ''union all select''.
		  * It works very similarly to
		  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform QueryReform]]`.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.topDown topDown]],
		  * but all member selects of the the query receive an identifier allowing to track which select any given
		  * result set row came from. For queries composed solely using a ''union all'' operator, these identifiers are
		  * all unique. However, if a query term under the given query is a ''compound select'' using another
		  * operator type, all its member ''selects'' receive the same id in order not to interfere with
		  * semantics of intersection or subtraction.
		  *
		  * In addition to normal cascading of permission constraints to reforms used for the internal unification
		  * of left and right sides of a ''compound select'', it returns related instances from methods
		  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]]/[[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]],
		  * ensuring that the id column is used consistently in all member ''selects''; the reform provided
		  * by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] is not used.
		  * The instance created here is proper for returning
		  * from `Reform.`[[net.noresttherein.oldsql.sql.mechanics.QueryReform.compound subreform]] and `subreform`
		  * should ''not'' be called on it directly (rather than a result of `this.left`/`this.right`).
		  *
		  * As with `QueryReform.topDown`, while recursively reforming any ''compound selects'' being subterms
		  * of the given query, the permissions for reforming a particular side are enforced during the whole
		  * process of shape unification. This differs from 'bottomUp' reforms in that the process of initial
		  * internal unification of ''select'' clauses within a subterm ''compound select'' in the latter proceeds
		  * independently of reforming it with other member ''selects'' of the enclosing query, ignoring the permissions
		  * given to the original `Reform` instance. On the other hand, when reforming `(A union all B) union all C`,
		  * this implementation will use for unifying `A union all B` a reform instance with 'left' permissions
		  * equal to 'right' permissions, equal to 'left' permissions of this instance.
		  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.UnionAllReform.bottomUp]]
		  */
		def topDown(
				mayExcludeLeft :Boolean, mayIncludeLeft :Boolean, mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
				mayExcludeRight :Boolean, mayIncludeRight :Boolean, mayReorderRight :Boolean, mayAddNullRight :Boolean)
				:QueryReform =
			topDown(Permissions(
				mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
				mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight
			))

		def topDown(permissions :Permissions) :QueryReform =
			new UnionAllTopReform(DefaultSelectIdColumn, "UnionAllReform.topDown")(permissions)(
				identity, new TopDownUnionAllReform(_, _, _)(_)(_)
			) with TopDownQueryReform
	}




//
//	trait QueryCrossReform extends QueryReform {
//		override def default[F <: RowProduct, R](query :CompoundSelectSQL[F, R])
//		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
//		{
//			val l = left(query).apply(query.left)
//			val r = right(query).apply(query.right)
//			val (reformedLeft, reformedRight) = prohibit(spelling.allowedReforms(query))(l, r)
//			if ((reformedLeft eq query.left) & (reformedRight eq query.right))
//				query
//			else
//				reformed(reformedLeft, query.operator, reformedRight)
//		}
//
//		override def default[F <: RowProduct, R](query :CompoundSelectColumn[F, R])(implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] = super
//			.default(query)
//
//		override def default[P, R](query :CompoundSelect[P, R])(implicit spelling :SQLSpelling) :CompoundSelect[P, R] = super
//			.default(query)
//	}


	abstract class QueryReformDecorator(private[QueryReform] val copyConstructor :QueryReform => QueryReform)
		extends ReformPermissionsDecorator[QueryReform](copyConstructor) with QueryReform
	{
		override val decorated :QueryReform //public

		override def apply[R, Res](query :QueryReformingTemplate[R, Res])(implicit spelling :SQLSpelling) :Res =
			decorated(query)

//		override def apply[P, R, Res]
//		                  (query :ReformableQuery[P, R, Res])(implicit spelling :SQLSpelling) :Res =
//			decorated(query)
//
//		override def apply[F <: RowProduct, R, Res]
//		                  (query :ReformableQuerySQL[F, R, Res])(implicit spelling :SQLSpelling) :Res =
//			decorated(query)

//		override def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] = decorated(query)
//		override def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] =
//			decorated(query)
//
//		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
//			decorated(query)
//
//		override def apply[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                      (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
//			decorated(query)
//
//		override def apply[F <: RowProduct, V](query :CompoundSelectColumn[F, V])
//		                                      (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, V] =
//			decorated(query)

//		override def fallback[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
//		                                         (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
//			decorated.fallback(left, right)
//
//		override def fallback[X, Y, V](left :Query[X, V], right :Query[Y, V])
//		                              (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
//			decorated.fallback(left, right)


		override def compatible[F <: RowProduct, V]
		                       (left :QuerySQL[F, V], right :QuerySQL[F, V])(implicit spelling :SQLSpelling) :Boolean =
			decorated.compatible(left, right)

		override def compatible[X, Y, V](left :Query[X, V], right :Query[Y, V])(implicit spelling :SQLSpelling) :Boolean =
			decorated.compatible(left, right)


		override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) :SelectSQL[F, V] =
			decorated.default(query)

		override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) :Select[P, V] =
			decorated.default(query)

		override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) :TopSelectSQL[V] =
			decorated.default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			decorated.default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectColumn[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, V] =
			decorated.default(query)

		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			decorated.default(query)


		override def reformed[F <: RowProduct, V](left :QuerySQL[F, V], operator :SelectOperator, right :QuerySQL[F, V])
		                                         (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			decorated.reformed(left, operator, right)

		override def reformed[P, V](left :Query[P, V], operator :SelectOperator, right :Query[P, V])
		                           (implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			decorated.reformed(left, operator, right)


//		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :QueryReform =
//			decorated.subreform(query)
//
//		override def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
//		                                          (implicit spelling :SQLSpelling) :QueryReform =
//			decorated.subreform(query)
		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                     (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			decorated.compound(query)

		override def left(implicit spelling :SQLSpelling) :QueryReform = decorated.left
		override def right(implicit spelling :SQLSpelling) :QueryReform = decorated.right

		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			decorated.left(query)

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			decorated.right(query)

		override def optimistic :QueryReform = decorated.optimistic
		override def bottomUp   :QueryReform = decorated.bottomUp
		override def topDown    :QueryReform = decorated.topDown

		override def leftSubqueryPermissions  :Permissions = decorated.leftSubqueryPermissions
		override def rightSubqueryPermissions :Permissions = decorated.rightSubqueryPermissions
		override def isBottomUp :Boolean = decorated.isBottomUp
		override def isTopDown  :Boolean = decorated.isTopDown

		override def impose(reform :Reform) :QueryReform = decorated.impose(reform)
		override def reform(implicit spelling :SQLSpelling) :Reform = decorated.reform
	}



	/** A `QueryReform` mixin which, before starting to reform a ''compound select'', first checks
	  * if its sides are already [[net.noresttherein.oldsql.sql.mechanics.QueryReform.compatible compatible]],
	  * simply returning the argument if it is so (otherwise it delegates to `super`).
	  */
	trait OptimisticQueryReform extends QueryReform {
/*
		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[F <: RowProduct, V](left :SingleQuerySQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[F <: RowProduct, V](left :SelectSQL[F, V], right :QuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SingleQuerySQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[F <: RowProduct, V](left :QuerySQL[F, V], right :SelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, V], QuerySQL[F, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[X, Y, V](left :SingleQuery[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[X, Y, V](left :Select[X, V], right :Query[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :SingleQuery[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)

		override def apply[X, Y, V](left :Query[X, V], right :Select[Y, V])
		                           (implicit spelling :SQLSpelling) :(Query[X, V], Query[Y, V]) =
			if (compatible(left, right))
				(left, right)
			else
				super.apply(left, right)
*/

		//consider: overriding left/right
		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                     (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.compound(query).optimistic

		override def apply[F <: RowProduct, R](left :QuerySQL[F, R], right :QuerySQL[F, R])
		                                      (implicit spelling :SQLSpelling) :(QuerySQL[F, R], QuerySQL[F, R]) =
			if (compatible(left, right)) (left, right)
			else super.apply(left, right)

		override def apply[X, Y, R](left :Query[X, R], right :Query[Y, R])
		                           (implicit spelling :SQLSpelling) :(Query[X, R], Query[Y, R]) =
			if (compatible(left, right)) (left, right)
			else super.apply(left, right)

		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] =
			if (compatible(query.left, query.right)) query
			else super.default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			if (compatible(query.left, query.right)) query
			else super.default(query)

		override def default[F <: RowProduct, V](query :CompoundSelectColumn[F, V])
		                                        (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, V] =
			if (compatible(query.left, query.right)) query
			else super.default(query)

		override def optimistic :QueryReform = self
	}


	trait BottomUpQueryReform extends QueryReform {
		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                      (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			//we cannot simply use query.defaultReform, because some reforms absolutely must create instances
			// of the same class
			super.compound(query).bottomUp
//			query.defaultReform.bottomUp

		override def left(implicit spelling :SQLSpelling) :QueryReform = super.left.bottomUp
		override def right(implicit spelling :SQLSpelling) :QueryReform = super.right.bottomUp

		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.left(query).bottomUp

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.right(query).bottomUp

		override def optimistic :QueryReform = super.optimistic.bottomUp
		override def isBottomUp = true
		override def isTopDown = false
	}
	//in order to create a BottomUpQueryReformDecorator we need 'allow' method accepting permissions



	//todo: mention that it doesn't necessarily propagate the actual reform implementation, anly permissions
	/** A mixin trait for `Reform` implementations with strict reforming permissions semantics.
	  * Overrides [[net.noresttherein.oldsql.sql.mechanics.QueryReform.left left]] and
	  * [[net.noresttherein.oldsql.sql.mechanics.QueryReform.right right]] to return an instance of the same type,
	  * but with equal permissions for left and right sides: in the case of the former, right permissions become
	  * the same as left permissions, and vice versa in the latter.
	  *
	  * The effect is that internal reforming of any expression in the pair being unified is treated as a part
	  * of the unification of the whole pair. This differs from the default semantics, in which internal reforming
	  * performed in order to achieve consistency and validity (for example of individual SQL ''selects''
	  * inside a ''compound select'') is treated as a separate process, and permissions of this reform apply
	  * only to the unification of the internally reformed expression.
	  */ //remember that super.left/super.right must return a TopDownReform, or otherwise the behaviour will be lost
	trait TopDownQueryReform extends QueryReform {
		/** Carries over the prohibitions of this reform to the argument. */
		//		protected def prohibit[R <: ReformPermissions[R]](reform :R) :R = reform match {
		//			case reform if !reform.isBottomUp => reform.prohibit(permissions)
		//			case reform => prohibit(reform.permissions)
		//		}
		//			reform.permit(
		//				mayExcludeLeft & reform.mayExcludeLeft, mayIncludeLeft & reform.mayIncludeLeft,
		//				mayReorderLeft & reform.mayReorderLeft, mayAddNullLeft & reform.mayAddNullLeft,
		//				mayExcludeRight & reform.mayExcludeRight, mayIncludeRight & reform.mayIncludeRight,
		//				mayReorderRight & reform.mayReorderRight, mayAddNullRight & reform.mayAddNullRight,
		//			)

		//		override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :QueryReform =
		//			super.subreform(query) match {
		//				case reform if reform.isTopDown => reform.prohibit(permissions)
		//				case reform => prohibit(reform.permissions)
		//			}
		//			prohibit(super.subreform(query).permissions)
		//			prohibit(super.subreform(query))

		//		override def subreform[F <: RowProduct, V]
		//		                      (query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) :QueryReform =
		//			super.subreform(query) match {
		//				case reform if reform.isTopDown => reform.prohibit(permissions)
		//				case reform => prohibit(reform.permissions)
		//			}
		//			super.subreform(query).prohibit(permissions)
		//			prohibit(super.subreform(query).permissions)
		//			prohibit(super.subreform(query))

		//		override def left[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q])
		//		                                                 (implicit spelling :SQLSpelling) :QueryReform =
		//			subreform(query).left
		//
		//		override def right[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q])
		//		                                                  (implicit spelling :SQLSpelling) :QueryReform =
		//			subreform(query).right

		//		override def left(implicit spelling :SQLSpelling)  :QueryReform = super.left.prohibit(permissions.asInLeft)//left(this)
		//		override def right(implicit spelling :SQLSpelling) :QueryReform = super.right.prohibit(permissions.asInRight)//right(this)

		//		protected def left(reform :QueryReform)(implicit spelling :SQLSpelling) :QueryReform =
		//			prohibit(reform.permissions.asInLeft)
		//			reform.permit(
		//				mayExcludeRight = reform.mayExcludeRight & mayExcludeLeft,
		//				mayIncludeRight = reform.mayIncludeRight & mayIncludeLeft,
		//				mayReorderRight = reform.mayReorderRight & mayReorderLeft,
		//				mayAddNullRight = reform.mayAddNullRight & mayAddNullLeft
		//			)
		//		protected def right(reform :QueryReform)(implicit spelling :SQLSpelling) :QueryReform =
		//			prohibit(reform.permissions.asInRight)
		//			reform.permit(
		//				mayExcludeLeft = reform.mayExcludeLeft & mayExcludeRight,
		//				mayIncludeLeft = reform.mayIncludeLeft & mayIncludeRight,
		//				mayReorderLeft = reform.mayReorderLeft & mayReorderRight,
		//				mayAddNullLeft = reform.mayAddNullLeft & mayAddNullRight
		//			)

		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                      (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.compound(query).topDown

		override def left(implicit spelling :SQLSpelling) :QueryReform = super.left.topDown
		override def right(implicit spelling :SQLSpelling) :QueryReform = super.right.topDown

		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.left(query).topDown

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			super.right(query).topDown

		override def optimistic :QueryReform = super.optimistic.topDown
		override def leftSubqueryPermissions  :Permissions = permissions.asInLeft
		override def rightSubqueryPermissions :Permissions = permissions.asInRight
//		override def isBottomUp = false
		override def isTopDown  = true
	}



	private def OptimisticQueryReformDecorator(reform :QueryReform) :QueryReform = {
		@tailrec def redecorate(reform :QueryReform, decorators :QueryReform => QueryReform) :QueryReform =
			reform match {
				case other :OptimisticQueryReform => other.self
				case other :QueryReformDecorator =>
					redecorate(other.decorated, other.copyConstructor andThen decorators)
				case _ => //we have hit the bottom.
					reform.redecorate(decorators andThen (new OptimisticQueryReformDecorator(_)))
			}
		redecorate(reform, identity)
	}

	private class OptimisticQueryReformDecorator(override val decorated :QueryReform)
		extends QueryReformDecorator(OptimisticQueryReformDecorator) with OptimisticQueryReform
	{
		override def typeName = "Optimistic"
		override def toString :String = "Optimistic(" + decorated + ")"
	}


	private def BottomUpQueryReformDecorator(reform :QueryReform) :QueryReform = {
		@tailrec def redecorate(reform :QueryReform, decorators :QueryReform => QueryReform) :QueryReform =
			reform match {
				case other :BottomUpQueryReform => other.self
				case topDown :TopDownQueryReformDecorator =>
					redecorate(topDown.decorated, decorators)
				case other :QueryReformDecorator =>
					redecorate(other.decorated, other.copyConstructor andThen decorators)
				case _ => //we have hit the bottom.
					reform.redecorate(decorators andThen (new BottomUpQueryReformDecorator(_)))
			}
		redecorate(reform, identity)
	}

	private class BottomUpQueryReformDecorator(override val decorated :QueryReform)
		extends QueryReformDecorator(BottomUpQueryReformDecorator) with BottomUpQueryReform
	{
		override def typeName = "BottomUp"
		override def toString :String = "BottomUp(" + decorated + ")"
	}

	private def TopDownQueryReformDecorator(reform :QueryReform) :QueryReform = {
		@tailrec def redecorate(reform :QueryReform, decorators :QueryReform => QueryReform) :QueryReform =
			reform match {
				case other :TopDownQueryReform => other.self
				case bottomUp :BottomUpQueryReformDecorator =>
					redecorate(bottomUp.decorated, decorators)
				case other :QueryReformDecorator =>
					redecorate(other.decorated, other.copyConstructor andThen decorators)
				case _ => //we have hit the bottom.
					reform.redecorate(decorators andThen (new TopDownQueryReformDecorator(_)))
			}
		redecorate(reform, identity)
	}

	private class TopDownQueryReformDecorator(override val decorated :QueryReform)
		extends QueryReformDecorator(TopDownQueryReformDecorator) with TopDownQueryReform
	{
		override def typeName = "TopDown"
		override def toString :String = "TopDown(" + decorated + ")"
	}



	private class ReformOverrideDecorator(defaultReform :Reform)(override val decorated :QueryReform)
		extends QueryReformDecorator(new ReformOverrideDecorator(defaultReform)(_))
	{
		override def reform(implicit spelling :SQLSpelling) :Reform = defaultReform.prohibit(self.permissions)
	}


//	trait BaseQueryReform extends QueryReform {
//		override def undecoratedEquals(that :QueryReform) :Boolean = that match {
//			case _ if that eq this => true
//			case other :QueryReformDecorator => other undecoratedEquals this
//			case _ => undecoratedCanEqual(that) && that.undecoratedCanEqual(this)
//		}
//		override def undecoratedCanEqual(that :QueryReform) :Boolean = getClass == that.getClass
//		override def undecoratedHashCode :Int = getClass.hashCode
//	}


/*
	private abstract
	class AbstractQueryReform(override val mayExcludeLeft :Boolean, override val mayIncludeLeft :Boolean,
	                          override val mayReorderLeft :Boolean, override val mayAddNullLeft :Boolean)
	                         (override val mayExcludeRight :Boolean, override val mayIncludeRight :Boolean,
	                           override val mayReorderRight :Boolean, override val mayAddNullRight :Boolean)
	                         (wrap :QueryReform => QueryReform,
	                          constructor :(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean, Boolean,
	                                        QueryReform => QueryReform) => QueryReform)
		extends DetailedReformPermissions[QueryReform](
		            mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft)(
		            mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight)(
		            wrap,
		            constructor(mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
		                        mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight, _)
		)  with QueryReform
	{
		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :QueryReform =
			if (mayExcludeLeft == this.mayExcludeLeft && mayIncludeLeft == this.mayIncludeLeft &&
			    mayReorderLeft == this.mayReorderLeft && mayAddNullRight == this.mayAddNullRight &&
			    mayExcludeRight == this.mayExcludeRight && mayIncludeRight == this.mayIncludeRight &&
			    mayReorderRight == this.mayReorderRight && mayAddNullRight == this.mayAddNullRight
			)
				self
			else
				constructor(mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft,
				            mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight, wrap).self
//
//		override lazy val swap :Reform =
//			if (self.isSymmetrical)
//				self
//			else if (self eq this) //this will be wrong for UnionAllReform
//				constructor(mayExcludeRight, mayIncludeRight, mayReorderRight, mayAddNullRight,
//				            mayExcludeLeft, mayIncludeLeft, mayReorderLeft, mayAddNullLeft, wrap).self
//			else
//				SwappedReform(self)
//
//		override def swapEquals(that :Reform) :Boolean = undecoratedEquals(that)
	}
*/



	private class BasicQueryReform(permissions :Permissions)
	                              (wrap :QueryReform => QueryReform,
	                               constructor :(Permissions, QueryReform => QueryReform) => QueryReform)
		extends DetailedReformPermissions[QueryReform](permissions)(wrap, constructor)
		   with QueryReform
	{
		def this(permissions :Permissions, wrap :QueryReform => QueryReform) =
			this(permissions)(wrap, new BasicQueryReform(_, _))

		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                     (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			query.defaultReform.prohibit(permissions).decorate(wrap)

		protected override def typeName = "QueryReform"
	}

	private class BasicBottomUpQueryReform(permissions :Permissions)(wrap :QueryReform => QueryReform)
		extends BasicQueryReform(permissions)(wrap, new BasicBottomUpQueryReform(_)(_)) with BottomUpQueryReform
	{
		protected override def typeName = "QueryReform.bottomUp"
	}

	private class BasicTopDownQueryReform(permissions :Permissions)(wrap :QueryReform => QueryReform)
		extends BasicQueryReform(permissions)(wrap, new BasicTopDownQueryReform(_)(_))
		   with TopDownQueryReform
	{
		protected override def typeName = "QueryReform.topDown"
	}


	/** A special `QueryReform` implementations which swaps itself
	  * for `query.`[[net.noresttherein.oldsql.sql.CompoundSelect.CompoundSelectTemplate.defaultReform defaultReform]]
	  * when it is used to reform a ''compound select''.
	  */
	private class DefaultReform(permissions :Permissions)(wrap :QueryReform => QueryReform)
		extends BasicQueryReform(permissions :Permissions)(wrap, new DefaultReform(_)(_))
	{
		override def compound[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                      (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			query.defaultReform.prohibit(permissions).decorate(wrap)

		override def default[F <: RowProduct, V](query: CompoundSelectSQL[F, V])(implicit spelling: SQLSpelling) =
			if (self eq this) query.defaultReform.prohibit(permissions)(query)
			else query.defaultReform.prohibit(permissions).decorate(wrap)(query)

		override def default[F <: RowProduct, V](query: CompoundSelectColumn[F, V])(implicit spelling: SQLSpelling) =
			if (self eq this) query.defaultReform.prohibit(permissions)(query)
			else query.defaultReform.prohibit(permissions).decorate(wrap)(query)

		override def default[P, V](query: CompoundSelect[P, V])(implicit spelling: SQLSpelling) =
			if (self eq this) query.defaultReform.prohibit(permissions)(query)
			else query.defaultReform.prohibit(permissions).decorate(wrap)(query)

		override def undecoratedEquals(that: QueryReform): Boolean = that.isInstanceOf[DefaultReform]
	}





	/** A `QueryReform` mixin which wraps the ''select'' clause of any SQL ''select''
	  * in a [[net.noresttherein.oldsql.sql.ast.SelectIdSQL SelectIdSQL]], prepending an extra column
	  * which can be then used to associate each result row with the particular member of a ''compound select''
	  * it came from, so the proper read form can be used for each row.
	  * @see [[net.noresttherein.oldsql.sql.mechanics.QueryReform.ConstSelectIdDecorator]]
	  */
	private trait SelectIdReform extends QueryReform {
		protected def id :String
		protected def columnName :String

	    override def default[F <: RowProduct, V](query :SelectSQL[F, V])(implicit spelling :SQLSpelling) :SelectSQL[F, V] =
		    query.selectClause match {
			    case SelectIdSQL(id, name, selectClause) =>
					if (id == this.id && name == columnName) query
					else query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
			    case selectClause =>
				    query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
		    }

	    override def default[P, V](query :Select[P, V])(implicit spelling :SQLSpelling) :Select[P, V] =
		    query.selectClause match {
			    case SelectIdSQL(id, name, selectClause) =>
					if (id == this.id && name == columnName) query
					else query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
			    case selectClause =>
				    query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
		    }

	    override def default[V](query :TopSelectSQL[V])(implicit spelling :SQLSpelling) :TopSelectSQL[V] =
		    query.selectClause match {
			    case SelectIdSQL(id, name, selectClause) =>
					if (id == this.id && name == columnName) query
					else query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
			    case selectClause =>
				    query.selectOther(SelectIdSQL(this.id, columnName, selectClause))
		    }


		//consider: what type of exceptions should be thrown here, and in QueryReform in general?
		override def default[F <: RowProduct, R](query :CompoundSelectColumn[F, R])(implicit spelling :SQLSpelling) =
			throw new IllegalArgumentException(
				"Cannot add a " + columnName + " column `" + id + "` to a ColumnQuery: `" + query + "`."
			)

		override def reformed[F <: RowProduct, R]
		                     (left :ColumnQuery[F, R], operator :SelectOperator, right :ColumnQuery[F, R])
		                     (implicit spelling :SQLSpelling) :CompoundSelectColumn[F, R] =
			throw new UnsupportedOperationException(
				"Cannot add a " + columnName + " column `" + id + "` to a ColumnQuery: `(" +
				left + " " + operator + " " + right + ")`."
			)

		override def reformed[F <: RowProduct, R](left :QuerySQL[F, R], operator :SelectOperator, right :QuerySQL[F, R])
		                                         (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, R] =
			CompoundSelectSQL.reformed(left, operator, right)(CompoundSelectIdSQL(_ :CompoundSelectSQL[F, R]))(this)

		override def reformed[P, R](left :Query[P, R], operator :SelectOperator, right :Query[P, R])
		                           (implicit spelling :SQLSpelling) :CompoundSelect[P, R] =
			CompoundSelect.reformed(left, operator, right)(CompoundSelectIdSQL(_ :CompoundSelect[P, R]))(this)
	}


	/** Ensures that there is a ConstSelectIdDecorator somewhere in the decorator stack for `reform`.
	  * We do not satisfy ourselves with any other `SelectIdReform` type because it is important
	  * that `id` is constant (and currently only other implementation of the private trait assigns different ids).
	  */
	private def ConstSelectIdDecorator(columnName :String, id :String)(decorated :QueryReform) :QueryReform = {
		@tailrec def redecorate(reform :QueryReform, decorators :QueryReform => QueryReform) :QueryReform =
			reform match {
				//we do not accept other SelectIdReform types because it is important that id is constant
				case other :ConstSelectIdDecorator if other.id == id && other.columnName == columnName =>
					other.self
				case other :ConstSelectIdDecorator =>
					other.decorated.undecorate((new ConstSelectIdDecorator(columnName, id, _)) andThen decorators)
				case const :SelectIdDecorator =>
					redecorate(const.decorated, decorators)
				case outer :TopSelectIdDecorator =>
					redecorate(outer.decorated, decorators)
				case other :QueryReformDecorator =>
					redecorate(other.decorated, other.copyConstructor andThen decorators)
				case _ => //we have hit the bottom.
					reform.redecorate(decorators andThen (new ConstSelectIdDecorator(columnName, id, _)))
			}
		redecorate(decorated, identity)
	}

	/** Wraps the select clause of ''every'' reformed SQL ''select'' in a `SelectIdSQL` with the ''same'' `id`.
	  * Used when a subquery in ''union all'' uses another operator and all its member selects must
	  * share the same id in order not to mess up the operator's semantics.
	  */
	private class ConstSelectIdDecorator(override val columnName :String, override val id :String,
	                                     override val decorated :QueryReform)
		extends QueryReformDecorator(new ConstSelectIdDecorator(columnName, id, _)) with SelectIdReform
	{
		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			decorated.left(query) decorate ConstSelectIdDecorator(columnName, id)

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			decorated.right(query) decorate ConstSelectIdDecorator(columnName, id)

		override def undecoratedEquals(that :QueryReform) :Boolean = that match {
			case _ if this eq that => true
			case other :ConstSelectIdDecorator =>
				columnName == other.columnName && id == other.id && (decorated undecoratedEquals other.decorated)
			case _ => false
		}
		override def undecoratedHashCode :Int = (columnName.hashCode * 31 + id.hashCode) * 31 + decorated.hashCode
		override def toString :String = decorated.toString + "(" + id + " as " + columnName + ")"
	}


	/** Ensures that there is a SelectIdDecorator somewhere in the decorator stack for `reform`.
	  * We do not satisfy ourselves with any other `SelectIdReform` type because it is important that `id` is unique.
	  */
	private def SelectIdDecorator(columnName :String, from :Int, until :Int)(decorated :QueryReform) :QueryReform = {
		@tailrec def redecorate(reform :QueryReform, decorators :QueryReform => QueryReform) :QueryReform =
			reform match {
				//we do not accept other SelectIdReform types because it is important that id is constant
				case other :SelectIdDecorator
					if from == other.firstId && until == other.idLimit && other.columnName == columnName
				=>
					other.self
				case other :SelectIdDecorator =>
					other.decorated.undecorate((new SelectIdDecorator(columnName, from, until, _)) andThen decorators)
				case const :ConstSelectIdDecorator =>
					redecorate(const.decorated, decorators)
				case outer :TopSelectIdDecorator =>
					redecorate(outer.decorated, decorators)
				case other :QueryReformDecorator =>
					redecorate(other.decorated, other.copyConstructor andThen decorators)
				case _ => //we have hit the bottom.
					reform.redecorate(decorators andThen (new SelectIdDecorator(columnName, from, until, _)))
			}
		redecorate(decorated, identity)
	}

	/** Wraps the select clause of ''every'' reformed SQL ''select'' in a `SelectIdSQL` with a unique id
	  * from range `[firstId, idLimit)`. Used to reform top ''union all'' ''compount selects''.
	  */
	private class SelectIdDecorator(override val columnName :String, val firstId :Int, val idLimit :Int,
	                                override val decorated :QueryReform)
		extends QueryReformDecorator(new SelectIdDecorator(columnName, firstId, idLimit, _)) with SelectIdReform
	{
		override def id :String = String.valueOf(firstId)
	    private def expectedQuerySize = idLimit - firstId

		private def assertQuerySize[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	                               (query :CompoundSelectTemplate[R, Q, Self]) :Unit =
            assert(expectedQuerySize == query.selectCount,
		        "cannot reform " + query + " with " + this + " because the number of combined selects " +
			        query.selectCount + " does not match expected " + expectedQuerySize +
			        " (from id range " + firstId + ".." + idLimit + ")."
		    )

	    override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
	    {
		    assertQuerySize(query)
		    val recommendedReform = decorated.left(query)
		    query.operator match {
			    case UnionAll =>
				    recommendedReform.decorate(SelectIdDecorator(columnName, firstId, firstId + query.left.selectCount))
			    case _ =>
				    recommendedReform.decorate(ConstSelectIdDecorator(columnName, firstId.toString))
		    }
	    }

	    override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
	    {
		    assertQuerySize(query)
		    val recommendedReform = decorated.right(query)
		    query.operator match {
			    case UnionAll =>
				    recommendedReform.decorate(SelectIdDecorator(columnName, idLimit - query.right.selectCount, idLimit))
			    case _ =>
				    recommendedReform.decorate(ConstSelectIdDecorator(columnName, firstId.toString))
		    }
	    }

		override def undecoratedEquals(that :QueryReform) :Boolean = that match {
			case _ if this eq that => true
			case other :SelectIdDecorator =>
				columnName == other.columnName && firstId == other.firstId && idLimit == other.idLimit &&
					(decorated undecoratedEquals other.decorated)
			case _ => false
		}
		override def undecoratedHashCode :Int =
			((columnName.hashCode * 31 + firstId.hashCode) * 31 + idLimit.hashCode) * 31 + decorated.hashCode

		override def toString :String = decorated.toString + "(" + firstId + ".." + idLimit + " as " + columnName + ")"
	}


	private class TopSelectIdDecorator(override val columnName :String, override val decorated :QueryReform)
		extends QueryReformDecorator(new TopSelectIdDecorator(columnName, _)) with SelectIdReform
	{
		protected override def id :String = "0"

		private def assertUnionAll[R, Q[X] <: QueryTemplate[X, Q], Same <: Q[R]]
		                          (query :CompoundSelectTemplate[R, Q, Same])(implicit spelling :SQLSpelling) :Unit =
			if (query.operator != UnionAll)
				throw new IllegalArgumentException(
					"QueryReform " + this + " cannot be used to reform compound select `" + query +
						"` because it is not a union all query."
				)
		override def default[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) = {
			assertUnionAll(query)
			val reform = decorated decorate SelectIdDecorator(columnName, 0, query.selectCount)
			reform.default(query)
		}
		override def default[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) = {
			assertUnionAll(query)
			val reform = decorated decorate SelectIdDecorator(columnName, 0, query.selectCount)
			reform.default(query)
		}
		//these probably would better throw an exception
		override def left[R, Q[X] <: QueryTemplate[X, Q], Same <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Same])(implicit spelling :SQLSpelling) =
		{
			assertUnionAll(query)
			decorated.left(query) decorate SelectIdDecorator(columnName, 0, query.left.selectCount)
		}
		override def right[R, Q[X] <: QueryTemplate[X, Q], Same <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Same])(implicit spelling :SQLSpelling) =
		{
			assertUnionAll(query)
			decorated.right(query) decorate SelectIdDecorator(columnName, query.left.selectCount, query.selectCount)
		}
	}

//	def UnionAllDecorator
//	private class UnionAllDecorator(val columnName :String, val firstLeftId :Int, val leftIdLimit :Int,
//	                                val firstRightId :Int, val rightIdLimit :Int)
//	                               (override val decorated :QueryReform)
//		extends QueryReformDecorator(new UnionAllDecorator)
	/** The default `Reform` for top-level ''union all'' selects.
	  * Aside from the usual permission and decorator/constructor arguments, it takes two id ranges,
	  * which should be used to assign to member selects in order. An instance can be in two states, depending
	  * on the knowledge about the reformed query. If no information about the query type is given,
	  * then both id ranges must be equal. Such `Reform`s are returned by `left`/`right` methods for
	  * subqueries of a ''compound select''. If the reformed query is (single) ''select'', then the appropriate callback
	  * method of this class will reform its ''select'' clause to include an id column, with a value equal
	  * to the first id of the range (which is expected to be of length 1). If the reformed query
	  * is a ''compound select'', it will ask for a dedicated `subreform`. Both ranges must then be equal
	  * and of length equal to `query.selectCount`. The current range is then split so that the left range
	  * of the new instance receives the first `query.left.selectCount` elements of the current range,
	  * while the right range receives the last `query.right.selectCount` elements of the current range.
	  *
	  * In turn, methods `left` and `right` should be called only on instances with disjoint ranges
	  * (returned by `subreform` methods, as described above). They return a `Reform` with both ranges
	  * equal to, correspondingly, the `left` or the `right` range of a subreform, which completes the cycle.
	  *
	  * If a subquery passed to `subreform` is not a ''union all'', then it will be reformed
	  * with default `spelling.`[[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.queryReform]]`(query)`
	  * decorated in a [[net.noresttherein.oldsql.sql.mechanics.QueryReform.ConstSelectIdDecorator ConstSelectIdDecorator]]
	  * which will add to all member selects an id column with the same value, equal to the first number
	  * in the left range.
	  */ //this should be a decorator, really.
/*
	private class UnionAllReform(permissions :Permissions)
	                            (val columnName :String, val firstLeftId :Int, val leftIdLimit :Int,
	                             val firstRightId :Int, val rightIdLimit :Int)
	                            (wrap :QueryReform => QueryReform,
	                             constructor :(Permissions, String, Int, Int, Int, Int, QueryReform => QueryReform)
	                                           => QueryReform)
		extends BasicQueryReform(
		            permissions)(
		            wrap, constructor(_, columnName, firstLeftId, leftIdLimit, firstRightId, rightIdLimit, _))
		   with SelectIdReform
    {
	    protected override def id = String.valueOf(firstLeftId)

	    override def isSymmetrical = false

	    protected def expectedQuerySize :Int = leftIdLimit - firstLeftId

//	    override def subreform[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) =
//		    queryReform(query)
//
//	    override def subreform[F <: RowProduct, V](query :CompoundSelectSQL[F, V])(implicit spelling :SQLSpelling) =
//		    queryReform(query)

	    protected def queryReform[V, Q[X] <: QueryTemplate[X, Q]]
	                             (query :CompoundSelectTemplate[V, Q])(implicit spelling :SQLSpelling) :QueryReform =
		    query.operator match {
			    case _ if firstRightId != firstLeftId | rightIdLimit != leftIdLimit =>
				    throw new IllegalStateException(
					    "Requested a subreform for " + query + " from " + this +
						    ". The reform has an indefinite expected query size. A call to subreform should happen " +
					        "only on instances returned by reform.left/reform.right."
				    )
			    case _ if query.selectCount != expectedQuerySize =>
				    throw new IllegalArgumentException(
					    toString + ": expected a query of " + expectedQuerySize + " member selects, got " + query +
						    " (with " + query.selectCount + " members). This seems like a bug."
				    )
			    case Select.UnionAll => //Another union: divide the firstLeftId..limitId as defined by the query
				    //consider: what with other unions?
				    unionAllReform(query)
			    case _ => //not a union: all selects within this query should use the same id = firstLeftId.
				    default(query).decorate(ConstSelectIdDecorator(columnName, firstLeftId.toString))
		    }
		protected def permissions(permissions :Permissions) :Permissions = permissions

	    protected def unionAllReform[V, Q[X] <: QueryTemplate[X, Q]]
		                            (query :CompoundSelectTemplate[V, Q])(implicit spelling :SQLSpelling) =
		{
			val idDivider = firstLeftId + query.left.selectCount
			constructor(
				permissions(default(query).permissions), columnName, firstLeftId, idDivider, idDivider, leftIdLimit, wrap
			).self
		}
	    protected def default[V, Q[X] <: QueryTemplate[X, Q]]
	                         (query :CompoundSelectTemplate[V, Q])(implicit spelling :SQLSpelling) :QueryReform =
		    if (spelling.scope == TopScope) query.topReform else query.defaultReform

//	    override def left(implicit spelling :SQLSpelling) :QueryReform =
//			constructor(permissions, columnName, firstLeftId, leftIdLimit, firstLeftId, leftIdLimit, wrap).self
//
//	    override def right(implicit spelling :SQLSpelling) :QueryReform =
//			constructor(permissions, columnName, firstRightId, rightIdLimit, firstRightId, rightIdLimit, wrap).self

	    override def left[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q])
	                                                     (implicit spelling :SQLSpelling) :QueryReform =
	    {
		    val recommendedReform = super.left(query)
		    val idDivider = firstLeftId + query.left.selectCount
		    val newPermissions = permissions(recommendedReform.permissions)
		    constructor(newPermissions, columnName, firstLeftId, )
//		    constructor(newPermissions, columnName, firstLeftId, idDivider, idDivider, leftIdLimit, wrap).self
	    }

	    override def right[R, Q[X] <: QueryTemplate[X, Q]](query :CompoundSelectTemplate[R, Q])
	                                                      (implicit spelling :SQLSpelling) :QueryReform =
	    {
		    val recommendedReform = super.left(query)
		    val idDivider = firstLeftId + query.left.selectCount
		    val newPermissions = permissions(recommendedReform.permissions)
		    constructor(newPermissions, columnName, firstLeftId, idDivider, idDivider, leftIdLimit, wrap).self
	    }

	    override def undecoratedEquals(that :QueryReform) :Boolean = that match {
		    case _ if this eq that => true
		    case other :UnionAllReform =>
			    undecoratedCanEqual(other) && other.undecoratedCanEqual(this) &&
				    columnName == other.columnName &&
				    firstLeftId == other.firstLeftId && firstRightId == other.firstRightId &&
				    leftIdLimit == other.leftIdLimit && rightIdLimit == other.rightIdLimit
		    case _ => super.undecoratedEquals(that)
	    }
	    override def undecoratedHashCode :Int =
		    (((columnName.hashCode * 31 +
			    firstLeftId.hashCode) * 31 +
			    firstRightId.hashCode) * 31 +
			    leftIdLimit.hashCode) * 31 +
			    rightIdLimit.hashCode

	    protected override def typeName = "UnionAllReform.bottomUp"

	    override def toString =
		    typeName + "(" + firstLeftId + ".." + leftIdLimit + "/" + firstRightId + ".." + rightIdLimit + ")(" +
			    permissionsString + ")"
    }
*/


	private class UnionAllReform(val columnName :String, val firstId :Int, val idLimit :Int)(permissions :Permissions)
	                            (wrap :QueryReform => QueryReform,
	                             constructor :(String, Int, Int, Permissions, QueryReform => QueryReform) => QueryReform)
		extends BasicQueryReform(permissions)(wrap, constructor(columnName, firstId, idLimit, _, _))
		   with SelectIdReform
    {
	    protected override def id = String.valueOf(firstId)

	    protected def expectedQuerySize :Int = idLimit - firstId

	    private def assertQuerySize[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
	                               (query :CompoundSelectTemplate[R, Q, Self]) :Unit =
            assert(expectedQuerySize == query.selectCount,
		        "cannot reform " + query + " with " + this + " because the number of combined selects " +
			        query.selectCount + " does not match expected " + expectedQuerySize +
			        " (from id range " + firstId + ".." + idLimit + ")."
		    )

	    override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
	    {
		    assertQuerySize(query)
		    val recommendedReform = super.left(query)
		    query.operator match {
			    case UnionAll =>
	                val newPermissions = recommendedReform.permissions & leftSubqueryPermissions
				    constructor(columnName, firstId, firstId + query.left.selectCount, newPermissions, wrap).self
			    case _ =>
				    recommendedReform.prohibit(leftSubqueryPermissions)
				                     .decorate(ConstSelectIdDecorator(columnName, firstId.toString))
		    }
	    }

	    override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
	    {
		    assertQuerySize(query)
		    val recommendedReform = super.right(query)
		    query.operator match {
			    case UnionAll =>
		            val newPermissions = recommendedReform.permissions & rightSubqueryPermissions
				    constructor(columnName, firstId + query.left.selectCount, idLimit, newPermissions, wrap).self
			    case _ =>
				    recommendedReform.prohibit(rightSubqueryPermissions)
				                     .decorate(ConstSelectIdDecorator(columnName, (firstId + query.left.selectCount).toString))
		    }
	    }

	    override def undecoratedEquals(that :QueryReform) :Boolean = that match {
		    case _ if this eq that => true
		    case other :UnionAllReform =>
			    undecoratedCanEqual(other) && other.undecoratedCanEqual(this) &&
				    columnName == other.columnName && firstId == other.firstId && idLimit == other.idLimit
		    case _ => super.undecoratedEquals(that)
	    }
	    override def undecoratedHashCode :Int =
		    (columnName.hashCode * 31 + firstId.hashCode) * 31 + idLimit.hashCode

	    protected override def typeName = "UnionAllReform.bottomUp"
	    override def toString = typeName + "(" + firstId + ".." + idLimit + ")(" + permissionsString + ")"
    }


	private class BottomUpUnionAllReform(columnName :String, firstId :Int, idLimit :Int)(permissions :Permissions)
	                                    (wrap :QueryReform => QueryReform)
		extends UnionAllReform(columnName, firstId, idLimit)(permissions)(
			wrap, new BottomUpUnionAllReform(_, _, _)(_)(_)
		)
	{
		protected override def typeName = "UnionAllReform.bottomUp"
	}


	/** A variant of `UnionAllReform` which differs only in that it propagates its permissions to
	  * `Reform`s returned by `left`, `right` and `subreform` by calling `permit` on the instance which would be
	  * returned by `UnionAllReform`.
	  */
	private class TopDownUnionAllReform(columnName :String, firstId :Int, idLimit :Int)
	                                   (permissions :Permissions)
	                                   (wrap :QueryReform => QueryReform)
		extends UnionAllReform(columnName, firstId, idLimit)(permissions)(wrap, new TopDownUnionAllReform(_, _, _)(_)(_))
		   with TopDownQueryReform
    {
//	    override def permissions(permissions :Permissions) :Permissions = permissions & this.permissions

	    //technically, only typeName is important - all other methods simply save the creation of one or more
	    // intermediate objects.
//	    override def left(implicit spelling :SQLSpelling) :Reform =
//		    if (self eq this)
//		        new TopDownUnionAllReform(permissions.asInLeft)(
//			        columnName, firstLeftId, leftIdLimit, firstLeftId, leftIdLimit)(wrap
//		        ).self
//		    else //call the implementation in TopDownReform which recreates the whole Reform stack with `permit` (in case our decorator changes permissions)
//			    super.left
//
//	    override def right(implicit spelling :SQLSpelling) :Reform =
//		    if (self eq this)
//			    new TopDownUnionAllReform(permissions.asInRight)(
//		                                  columnName, firstRightId, rightIdLimit, firstRightId, rightIdLimit)(wrap
//		        ).self
//		    else //call the implementation in TopDownReform which recreates the whole Reform stack with `permit` (in case our decorator changes permissions)
//			    super.right

	    override def typeName = "UnionAllReform.topDown"
	}


	private class UnionAllTopReform(val columnName :String, override val typeName :String)(permissions :Permissions)
	                               (wrap :QueryReform => QueryReform,
	                                subreform :(String, Int, Int, Permissions, QueryReform => QueryReform) => QueryReform)
		extends BasicQueryReform(permissions)(wrap, new UnionAllTopReform(columnName, typeName)(_)(_, subreform))
	{
		protected def left(defaults :Permissions)  :Permissions = defaults
		protected def right(defaults :Permissions) :Permissions = defaults

		override def left[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                 (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			subreform(columnName, 0, query.left.selectCount, left(self.permissions), wrap).self

		override def right[R, Q[X] <: QueryTemplate[X, Q], Self <: Q[R]]
		                  (query :CompoundSelectTemplate[R, Q, Self])(implicit spelling :SQLSpelling) :QueryReform =
			subreform(columnName, query.left.selectCount, query.selectCount, right(self.permissions), wrap).self
	}





/*
	class HolisticReform(wrap :Reform => Reform)
		extends BaseDecorable[QueryReform](wrap, new HolisticReform(_)) with QueryReform
	{
		def this() = this(identity)


//		override def default[LF <: RowProduct, LM[O] <: MappingAt[O], RF <: RowProduct, RM[O] <: MappingAt[O], U]
//		                    (left :ComponentSQL[LF, LM], right :ComponentSQL[RF, RM])
//		                    (implicit leftResult  :SQLTransformation[RM[Unit]#Subject, U],
//		                              rightResult :SQLTransformation[LM[Unit]#Subject, U], spelling :SQLSpelling)
//				:(leftResult.SQLResult[LF, Single, LValueSQL[LF, LM, U]], rightResult.SQLResult[RF, Single, LValueSQL[RF, RM, U]]) =
//			???

		override def apply[F <: RowProduct, V](query :QuerySQL[F, V])(implicit spelling :SQLSpelling) :QuerySQL[F, V] = {
//			val banded = query.constituents.map(_.selectClause).reduce(
//				PreliminaryAligner[Nothing, Grouped, V, Nothing, Grouped, V, V](_, _)._1
//			)
			???
		}

		override def apply[P, V](query :Query[P, V])(implicit spelling :SQLSpelling) :Query[P, V] = super.apply(query)

		override def apply[F <: RowProduct, V](query :CompoundSelectSQL[F, V])
		                                      (implicit spelling :SQLSpelling) :CompoundSelectSQL[F, V] =
			super.apply(query)

		override def apply[P, V](query :CompoundSelect[P, V])(implicit spelling :SQLSpelling) :CompoundSelect[P, V] = super.apply(query)



//		override def fallback[L <: RowProduct, A[O] <: MappingAt[O], R <: RowProduct, B[O] <: MappingAt[O], Z]
//		                     (left :ComponentSQL[L, A], right :ComponentSQL[R, B])
//		                     (implicit leftResult :SQLTransformation[left.Subject, Z], rightResult :SQLTransformation[right.Subject, Z],
//		                               spelling :SQLSpelling)
//				:(leftResult.SQLResult[L, Single, LValueSQL[L, A, Z]],
//				  rightResult.SQLResult[R, Single, LValueSQL[R, B, Z]]) =

		override def mayExcludeLeft  :Boolean = true
		override def mayIncludeLeft  :Boolean = true
		override def mayReorderLeft  :Boolean = true
		override def mayAddNullLeft  :Boolean = true
		override def mayExcludeRight :Boolean = true
		override def mayIncludeRight :Boolean = true
		override def mayReorderRight :Boolean = true
		override def mayAddNullRight :Boolean = true

		override def permit(mayExcludeLeft :Boolean, mayIncludeLeft :Boolean,
		                    mayReorderLeft :Boolean, mayAddNullLeft :Boolean,
		                    mayExcludeRight :Boolean, mayIncludeRight :Boolean,
		                    mayReorderRight :Boolean, mayAddNullRight :Boolean) :Reform =
			this

		override def swap :Reform = this
	}
*/


}
