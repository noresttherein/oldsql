package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.{AggregateClause, AggregateFunction, ColumnSQL, FromSome, RowProduct, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{*, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






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
  *           `F >: G#FromClause <: FromSome, G <: AggregateClause`. This is not enforced on this level in order to
  *           preserve `G` when downcasting from [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  *           as well as contravariance in `G`.
  * @tparam X the type of the expression which is the argument to the aggregate function.
  * @tparam Y the return type of the aggregate function used and the value type of this expression.
  */ //consider: renaming to AggregateFunctionSQL
trait AggregateSQL[-F <: RowProduct, -G <: RowProduct, X, Y] extends ColumnSQL[G, LocalScope, Y] {
	/** The aggregate function which is applied to the aggregated expression
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.arg arg]].
	  */
	def function :AggregateFunction

	/** The argument expression passed to the aggregate [[net.noresttherein.oldsql.sql.ast.AggregateSQL.function function]].
	  * The expression is based on the ''from'' clause `F` under grouping introduced by `G`, thus giving access
	  * to relations which columns are not available individually to expressions based on `G`.
	  */
	def arg :ColumnSQL[F, LocalScope, X]

	/** If `true`, the argument will be prefixed with the `DISTINCT` clause and only non-null values of
	  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL.arg arg]] will be taken into account.
	  */
	def isDistinct :Boolean

	/** Add a `DISTINCT` clause to this aggregate expression. */
	def distinct :AggregateSQL[F, G, X, Y] //=
//		if (isDistinct) this
//		else new DefaultAggregateSQL(function, arg, true)(readForm)

	override def groundValue :Opt[Y] = Lack
	override def isGlobal = false
	override def asGlobal :Option[Nothing] = None
	override def isAnchored :Boolean = arg.isAnchored

	override def anchor(from :G) :ColumnSQL[G, LocalScope, Y]  = from match {
		case aggregate :AggregateClause =>
			val arg = this.arg.asInstanceOf[ColumnSQL[aggregate.Discrete, LocalScope, X]]
			arg.anchor(aggregate.fromClause) match {
				case same if same eq arg => this
				case grounded =>
					AggregateSQL(function, grounded, isDistinct)(readForm).asInstanceOf[ColumnSQL[G, LocalScope, Y]]
			}
		case _ =>
			throw new IllegalArgumentException(s"Cannot anchor an AggregateSQL $this in a non AggregateClause $from.")
	}


	override def basedOn[U <: G, E <: RowProduct](base :E)(implicit ext :U PartOf E) :AggregateSQL[F, E, X, Y] =
		this.asInstanceOf[AggregateSQL[F, E, X, Y]]
//		new DefaultAggregateSQL(function, arg, isDistinct)(readForm) //we could just cast ourselves and it would be fine

	override def expand[U <: G, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, global: GlobalScope <:< LocalScope) :Nothing =
		throw new UnsupportedOperationException(
			s"AggregateSQL expression cannot be used in a subselect $base of its original FROM clause."
		)


	protected override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](visitor :ColumnVisitor[G, R]) :R[LocalScope, Y] =
		visitor.aggregate(this)


	override def withClause :WithClause = arg.withClause

	protected override def reverseCollect[Z](fun :PartialFunction[*, Z], acc :List[Z]) :List[Z] =
		arg.reverseCollectForwarder(fun, super.reverseCollect(fun, acc))

	override def isomorphic(that: SQLExpression.*) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL.* if aggr canEqual this =>
			function == aggr.function && isDistinct == aggr.isDistinct && arg.isomorphic(aggr.arg)
		case _ => false
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateSQL.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL.* if aggr canEqual this =>
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
	  * @param distinct a flag specifying if the argument should be preceded with the 'DISTINCT' clause.
	  * @return an `SQLExpression` based on the ''from'' clause aggregating the rows from the clause `F`.
	  */ //fixme: abstract type projection
	def apply[F <: FromSome, X, V :ColumnReadForm]
	         (fun :AggregateFunction, arg :ColumnSQL[F, LocalScope, X], distinct :Boolean)
			:AggregateSQL[F, F#GeneralizedAggregate, X, V] =
		new DefaultAggregateSQL(fun, arg, distinct)


	def unapply[F <: FromSome, G <: RowProduct, X, Y]
	           (e :AggregateSQL[F, G, X, Y]) :Opt[(AggregateFunction, Boolean, ColumnSQL[F, LocalScope, X])] =
		Got((e.function, e.isDistinct, e.arg))

	def unapply[G <: RowProduct, T](e :SQLExpression[G, LocalScope, T])
			:Opt[(AggregateFunction, Boolean, ColumnSQL[_ <: FromSome, LocalScope, _])] =
		e match {
			case aggr :AggregateSQL.* =>
				Got((aggr.function, aggr.isDistinct, aggr.arg.asInstanceOf[ColumnSQL[_ <: FromSome, LocalScope, _]]))
			case _ => Lack
		}



	type * = AggregateSQL[_ <: RowProduct, _ <: RowProduct, _, _]

	//Loose bounds here are required only by distinct, basedOn and Count.*
	private[sql] class DefaultAggregateSQL[F <: FromSome, X, Y]
	                                      (override val function :AggregateFunction,
	                                       override val arg :ColumnSQL[F, LocalScope, X],
	                                       override val isDistinct :Boolean)
	                                      (implicit override val readForm :ColumnReadForm[Y])
		extends AggregateSQL[F, F#GeneralizedAggregate, X, Y]
	{
		override def distinct =
			if (isDistinct) this else new DefaultAggregateSQL(function, arg, true)

		protected override def defaultSpelling[P, E <: F#GeneralizedAggregate]
		                                      (context :SQLContext, params :Parameterization[P, E])
		                                      (implicit spelling :SQLSpelling) =
		{
			val ungroupedParams = params.ungroup[F]
			val res = spelling(function, isDistinct)(arg)(context, ungroupedParams.params)
			val grouped = res.params.settersReversed.map(_.unmap(ungroupedParams.ungroupParams))
			SpelledSQL(res.sql, res.context, params.reset(grouped ::: params.settersReversed))
		}
	}



	//the problem with the visitor pattern is that it prevents us from narrowing F to GroupByClause
	trait AggregateVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		/** A special pseudo expression of `*` used inside `count(*)`. The actual type is not exposed. */
		def *(e :ColumnSQL[RowProduct, LocalScope, Nothing]) :Y[LocalScope, Nothing]

		def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V]
	}

	type MatchAggregate[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AggregateVisitor[F, Y]

	type CaseAggregate[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = AggregateVisitor[F, Y]

}



