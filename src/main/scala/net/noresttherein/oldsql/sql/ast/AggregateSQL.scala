package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.{AggregateClause, AggregateFunction, ColumnSQL, FromSome, RowProduct, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.{AnyColumnVisitor, SpecificColumnVisitor}
import net.noresttherein.oldsql.sql.RowProduct.{AggregateOfGeneralized, ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL






/** An SQL expression representing the application of an aggregate function such as `sum` to an expression using
  * columns from multiple rows. Used in conjunction with queries featuring a ''group by'' clause,
  * or in a ''select'' clause when all rows are being aggregated.
  * @tparam F the ungrouped ''from'' clause whose [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''Explicit'']]
  *           section is aggregated, that is contains relations whose columns are not available individually
  *           (unless featured in
  *           [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]).
  *           All instances of this class are always created with `F <: FromSome`, but the most abstract upper bound
  *           is required for proper implementation of all operations.
  * @tparam G the [[net.noresttherein.oldsql.sql.AggregateClause ''aggregated'']] clause for `F`. It is the
  *           `RowProduct` on which this expression is based; all instances of `AggregateSQL` are created with bounds
  *           `F >: G#Discrete <: FromSome, G <: AggregateClause`. This is not enforced on this level in order to
  *           preserve `G` when downcasting from [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  *           as well as contravariance in `G`.
  * @tparam X the value type of the expression which is the argument to the aggregate function.
  * @tparam V the return type of the aggregate function used and the value type of this expression.
  */ //consider: renaming to AggregateFunctionSQL
trait AggregateSQL[-F <: RowProduct, -G <: RowProduct, X, V] extends ColumnSQL[G, Grouped, V] {

	/** The aggregate function which is applied to the aggregated expression
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.arg arg]].
	  */
	def function :AggregateFunction

	/** The argument expression passed to the aggregate [[net.noresttherein.oldsql.sql.ast.AggregateSQL.function function]].
	  * The expression is based on the ''from'' clause `F` under grouping introduced by `G`, thus giving access
	  * to relations which columns are not available individually to expressions based on `G`.
	  */
	def arg :ColumnSQL[F, Grouped, X]

	/** If `true`, the argument will be prefixed with the `DISTINCT` clause and only non-null values of
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.arg arg]] will be taken into account.
	  */
	def isDistinct :Boolean

	/** Add a `DISTINCT` clause to this aggregate expression. */
	def distinct :AggregateSQL[F, G, X, V] //=
//		if (isDistinct) this
//		else new Impl(function, arg, true)(readForm)

	override def isSingleRow = false
	override def asSingleRow :Option[Nothing] = None
	override def groundValue :Opt[V] = Lack
	override def isGround :Boolean = arg.isGround
	override def isAnchored :Boolean = arg.isAnchored

	override def isAnchored(from :G) :Boolean = from match {
		case aggregate :AggregateClause =>
			val arg = this.arg.asInstanceOf[ColumnSQL[aggregate.Discrete, Grouped, X]]
			arg.isAnchored(aggregate.fromClause)
		case _ =>
			throw new IllegalArgumentException(s"Cannot anchor an AggregateSQL $this in a non AggregateClause $from.")
	}

	override def anchor(from :G) :ColumnSQL[G, Grouped, V]  = from match {
		case aggregate :AggregateClause =>
			val arg = this.arg.asInstanceOf[ColumnSQL[aggregate.Discrete, Grouped, X]]
			arg.anchor(aggregate.fromClause) match {
				case same if same eq arg => this
				case grounded =>
					AggregateSQL(function, grounded, isDistinct)(selectForm).asInstanceOf[ColumnSQL[G, Grouped, V]]
			}
		case _ =>
			throw new IllegalArgumentException(s"Cannot anchor an AggregateSQL $this in a non AggregateClause $from.")
	}

	override def basedOn[U <: G, E <: RowProduct](base :E)(implicit expansion :U PartOf E) :AggregateSQL[F, E, X, V] =
		this.asInstanceOf[AggregateSQL[F, E, X, V]]
//		new Impl(function, arg, isDistinct)(readForm) //we could just cast ourselves and it would be fine

	override def expand[U <: G, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle: Single <:< Grouped) :Nothing =
		throw new UnsupportedOperationException(
			s"AggregateSQL expression cannot be used in a subselect $base of its original FROM clause."
		)


	override def outerWithClause :WithClause = arg.outerWithClause

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(arg)


	protected override def visit[R[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyColumnVisitor[G, R]) :R[Grouped, V] = visitor.aggregate(this)
//
//	protected override def visit[G_ <: G, S_ >: Grouped <: Grouped,
//                                 E >: ColumnSQL[G_, S_, V] <: SQLExpression[G_, S_, V],
//                                 R[-s >: Grouped <: Single, v, -e <: SQLExpression[G_, s, v]]]
//                                (visitor :ColumnVisitor[G, R]) :R[S_, V, E] =
//		visitor.aggregate(this)

	protected override def visit[R](visitor :SpecificColumnVisitor[G, Grouped, V, R]) :R = visitor.aggregate(this)

	override def isomorphic(that: SQLExpression.__) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL.__ if aggr canEqual this =>
			function == aggr.function && isDistinct == aggr.isDistinct && arg.isomorphic(aggr.arg)
		case _ => false
	}
//
//	override def compareWith(that :SQLExpression.*)(cmp :(JoinedRelation.*, JoinedRelation.*) => Boolean) :Boolean =
//		that match {
//			case self :AnyRef if self eq this => true
//			case aggr :AggregateSQL.* if aggr canEqual this =>
//				function == aggr.function && isDistinct == aggr.isDistinct && arg.compareWith(aggr.arg)(cmp)
//			case _ => false
//		}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateSQL.__]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL.__ if aggr canEqual this =>
			function == aggr.function && isDistinct == aggr.isDistinct && arg == aggr.arg
		case _ => false
	}

	override def hashCode :Int = (function.hashCode * 31 + distinct.hashCode) * 31 + arg.hashCode


	override def toString :String =
		if (isDistinct) s"${function.name}(distinct $arg)" else s"${function.name}($arg)"

}






object AggregateSQL {

	/** Creates an aggregate expression applying the function `fun` to the argument expression `arg`.
	  * This is a low-level function targeted at implementors, applications should in almost all cases prefer
	  * the factory methods from the appropriate
	  * [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]] implementation.
	  * @param fun an SQL aggregate function used.
	  * @param arg an SQL expression based on `F` which is used as the argument for `fun`.
	  * @param distinct a flag specifying if the argument should be preceded with a 'DISTINCT' clause.
	  * @return an `SQLExpression` based on the ''from'' clause aggregating the rows from the clause `F`.
	  */ //fixme: abstract type projection
	def apply[F <: FromSome, X, V :ColumnReadForm]
	         (fun :AggregateFunction, arg :ColumnSQL[F, Grouped, X], distinct :Boolean)
			:AggregateSQL[F, F#GeneralizedAggregate, X, V] =
		new Impl(fun, arg, distinct)


	def unapply[F <: FromSome, G <: RowProduct, X, Y]
	           (e :AggregateSQL[F, G, X, Y]) :Opt[(AggregateFunction, Boolean, ColumnSQL[F, Grouped, X])] =
		Got((e.function, e.isDistinct, e.arg))

	def unapply[G <: RowProduct, T](e :SQLExpression[G, Grouped, T])
			:Opt[(AggregateFunction, Boolean, ColumnSQL[_ <: FromSome, Grouped, _])] =
		e match {
			case aggr :AggregateSQL.__ =>
				Got((aggr.function, aggr.isDistinct, aggr.arg.asInstanceOf[ColumnSQL[_ <: FromSome, Grouped, _]]))
			case _ => Lack
		}


	//Loose bounds here are required only by distinct, basedOn and Count.*
	private class Impl[F <: FromSome, X, Y]
	                  (override val function :AggregateFunction, override val arg :ColumnSQL[F, Grouped, X],
	                   override val isDistinct :Boolean)
	                  (implicit override val selectForm :ColumnReadForm[Y])
		extends AggregateSQL[F, F#GeneralizedAggregate, X, Y]
	{
		override def distinct =
			if (isDistinct) this else new Impl(function, arg, true)

		protected override def defaultSpelling[P](from :F#GeneralizedAggregate, context :SQLContext[P],
		                                          params :Parameterization[P, F#GeneralizedAggregate])
		                                         (implicit spelling :SQLSpelling) =
		{
			//fixme: if we switch from F#GeneralizedAggregate to AggregateOfGeneralized[F] this cast can go away
			val sql = spelling(function, isDistinct)(arg)(
				from.fromClause.asInstanceOf[F], context.ungroup,
				params.asInstanceOf[Parameterization[P, AggregateOfGeneralized[F]]].ungroup
			)
			SpelledSQL(sql.sql, sql.setter, sql.context.group(context.groupings))
		}
	}


	type __ = AggregateSQL[_ <: RowProduct, _ <: RowProduct, _, _]

	type from[-F <: RowProduct] = {
		type __ = AggregateSQL[F, _ <: RowProduct, _, _]
		type group[-G <: RowProduct] = {
			type __ = AggregateSQL[F, G, _, _]
			type of[X] = {
				type __ = AggregateSQL[F, G, X, _]
				type E[V] = AggregateSQL[F, G, X, _]
				type C[V] = AggregateSQL[F, G, X, _]
			}
		}
	}

	trait SpecificAggregateVisitor[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] {
		def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
		     (implicit conforms :ColumnSQL[RowProduct, Grouped, Nothing] <:< ColumnSQL[RowProduct, S, X]) :Y
		def aggregate[D <: FromSome, E >: F <: RowProduct, V]
		             (e :AggregateSQL[D, E, V, X])(implicit conforms :AggregateSQL[D, E, V, X] <:< ColumnSQL[E, S, X]) :Y
	}
	type MatchSpecificAggregate[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificAggregateVisitor[F, S, X, Y]
	type CaseSpecificAggregate[+F <: RowProduct, +S >: Grouped <: Single, X, +Y] = SpecificAggregateVisitor[F, S, X, Y]
//
//	trait AggregateVisitor[+G <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[G, S, v], -E <: EC[V]]] {
//		/** A special pseudo expression of `*` used inside `count(*)`. The actual type is not exposed. */
//		def *(e :ColumnSQL[RowProduct, Grouped, Nothing])
//				:Y[Grouped, Nothing, SQLExpression.c[RowProduct]#c[Grouped]#C, ColumnSQL[RowProduct, Grouped, Nothing]]
//
//		def aggregate[F <: FromSome, X, V](e :AggregateSQL[F, G, X, V])
//				:Y[Grouped, V, SQLExpression.c[G]#c[Grouped]#C, AggregateSQL[F, G, X, V]]
//	}
//	type MatchAggregate[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]] =
//		AggregateVisitor[F, Y]
//	type CaseAggregate[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -EC[v] <: SQLExpression[F, S, v], -E <: EC[V]]] =
//		AggregateVisitor[F, Y]

	//the problem with the visitor pattern is that it prevents us from narrowing F to GroupByClause
	trait AnyAggregateVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		/** A special pseudo expression of `*` used inside `count(*)`. The actual type is not exposed. */
		def *(e :ColumnSQL[RowProduct, Grouped, Nothing]) :Y[Grouped, Nothing]
		def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[Grouped, V]
	}
	type MatchAnyAggregate[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyAggregateVisitor[F, Y]
	type CaseAnyAggregate[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyAggregateVisitor[F, Y]

}



