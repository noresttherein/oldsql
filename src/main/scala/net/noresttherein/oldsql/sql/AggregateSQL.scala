package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql.AggregateSQL.{AggregateFunction, DefaultAggregateSQL}
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{AggregateOf, ExtendedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLNumber.SQLFraction






/** An SQL expression representing the application of an aggregate function such as `sum` to an expression using
  * columns from multiple rows. Used in conjunction with queries featuring a ''group by'' clause,
  * or in the ''select'' clause when all rows are being aggregated.
  * @tparam F the ungrouped ''from'' clause whose [[net.noresttherein.oldsql.sql.FromClause.Explicit ''explicit'']]
  *           section is aggregated, that is contains relations whose columns are not available individually
  *           (unless featured in
  *           [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]]/[[net.noresttherein.oldsql.sql.ByAll ByAll]]).
  *           All instances of this class are always created with `F <: FromSome`, but the most abstract upper bound
  *           is required for proper implementation of all operations.
  * @tparam G the [[net.noresttherein.oldsql.sql.AggregateClause ''aggregated'']] clause for `F`. It is the
  *           `FromClause` on which this expression is based; all instances of `AggregateSQL` are created with bounds
  *           `F >: G#DiscreteFrom <: FromSome, G <: AggregateClause`. This is not enforced on this level in order to
  *           preserve `G` when downcasting from [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]],
  *           as well as contravariance in `G`.
  * @tparam X the type of the expression which is the argument to the aggregate function.
  * @tparam Y the return type of the aggregate function used and the value type of this expression.
  */
trait AggregateSQL[-F <: FromClause, -G <: FromClause, X, Y] extends ColumnSQL[G, LocalScope, Y] {
	/** The aggregate function which is applied to the aggregated expression
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.arg arg]].
	  */
	def function :AggregateFunction

	/** The argument expression passed to the aggregate [[net.noresttherein.oldsql.sql.AggregateSQL.function function]].
	  * The expression is based on the ''from'' clause `F` under grouping introduced by `G`, thus giving access
	  * to relations which columns are not available individually to expressions based on `G`.
	  */
	def arg :ColumnSQL[F, LocalScope, X]

	/** If `true`, the argument will be prefixed with the `DISTINCT` clause and only non-null values of
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.arg arg]] will be taken into account.
	  */
	def isDistinct :Boolean

	/** Add a `DISTINCT` clause to this aggregate expression. */
	def distinct :AggregateSQL[F, G, X, Y] =
		if (isDistinct) this
		else new DefaultAggregateSQL(function, arg, true)(readForm)


	override def isGlobal = false
	override def asGlobal :Option[Nothing] = None


	override def basedOn[U <: G, E <: FromClause](base :E)(implicit ext :U PartOf E) :AggregateSQL[F, E, X, Y] =
		new DefaultAggregateSQL(function, arg, isDistinct)(readForm) //we could just cast ourselves and it would be fine

	override def extend[U <: G, E <: FromClause]
	                   (base :E)(implicit extension :U ExtendedBy E, global: GlobalScope <:< LocalScope) :Nothing =
		throw new UnsupportedOperationException(
			s"AggregateSQL expression cannot be extended over to a subselect clause $base."
		)



	override def applyTo[R[-_ >: LocalScope <: GlobalScope, _]](matcher :ColumnMatcher[G, R]) :R[LocalScope, Y] =
		matcher.aggregate(this)



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
			function == aggr.function && distinct == aggr.distinct && arg == aggr.arg
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
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction AggregateFunction]] implementation.
	  * @param fun an SQL aggregate function used.
	  * @param arg an SQL expression based on `F` which is used as the argument for `fun`.
	  * @param distinct a flag specifying if the argument should be preceded with the 'DISTINCT' clause.
	  * @return an `SQLExpression` based on the ''from'' clause aggregating the rows from the clause `F`.
	  */ //fixme: abstract type projection
	def apply[F <: FromSome, X, V :ColumnReadForm]
	         (fun :AggregateFunction, arg :ColumnSQL[F, LocalScope, X], distinct :Boolean)
			:AggregateSQL[F, F#GeneralizedAggregate, X, V] =
		new DefaultAggregateSQL(fun, arg, distinct)


	def unapply[F <: FromSome, G <: FromClause, X, Y]
	           (e :AggregateSQL[F, G, X, Y]) :Some[(AggregateFunction, Boolean, ColumnSQL[F, LocalScope, X])] =
		Some((e.function, e.isDistinct, e.arg))

	def unapply[G <: FromClause, T](e :SQLExpression[G, LocalScope, T])
			:Option[(AggregateFunction, Boolean, ColumnSQL[_ <: FromSome, LocalScope, _])] =
		e match {
			case aggr :AggregateSQL.* =>
				Some((aggr.function, aggr.isDistinct, aggr.arg.asInstanceOf[ColumnSQL[_ <: FromSome, LocalScope, _]]))
			case _ => None
		}



	type * = AggregateSQL[_ <: FromClause, _ <: FromClause, _, _]

	private class DefaultAggregateSQL[-F <: FromClause, -G <: FromClause, X, Y]
	                                 (override val function :AggregateFunction,
	                                  override val arg :ColumnSQL[F, LocalScope, X],
	                                  override val isDistinct :Boolean)
	                                 (implicit override val readForm :ColumnReadForm[Y])
		extends AggregateSQL[F, G, X, Y]



	/** An SQL aggregate function such as `count`, `max` or `stddev`. As these functions often work differently,
	  * this interface is almost empty, serving only to group them all into a single type hierarchy.
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Count]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.HomomorphicAggregateFunction]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.FractionalAggregateFunction]]
	  */
	trait AggregateFunction {
		val name :String

		def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateFunction]

		override def equals(that :Any) :Boolean = that match {
			case fun :AggregateFunction => (fun eq this) || (fun canEqual this) && name == fun.name
			case _ => false
		}

		override def hashCode :Int = name.hashCode

		override def toString :String = name
	}



	/** Implementations of the SQL `COUNT` function. It accepts expressions of any type as the argument.
	  * When counting all the rows returned by an SQL ''select'', prefer the factory method
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome.count count]] and its overloaded variants,
	  * available on not grouping, non-empty ''from'' clauses.
	  */
	case object Count extends AggregateFunction {
		override val name = "count"

		//todo: support for other integer return types, in particular Long

		/** Represents the `COUNT(*)` SQL expression. */
		def apply() :AggregateSQL[FromSome, AggregateClause, Nothing, Int] = *
//			AggregateSQL[FromSome, Nothing, Int](this, *, false)

		/** Represents the `COUNT(arg)` SQL expression. */
		def apply[F <: FromSome, V](arg :ColumnSQL[F, LocalScope, V]) :AggregateSQL[F, F#GeneralizedAggregate, V, Int] =
			AggregateSQL(this, arg, false)

		/** Represents the `COUNT(DISTINCT arg)` SQL expression. */
		def distinct[F <: FromSome, V](arg :ColumnSQL[F, LocalScope, V]) :AggregateSQL[F, F#GeneralizedAggregate, V, Int] =
			AggregateSQL(this, arg, true)


		/** Represents the `COUNT(*)` SQL expression. Note that this stands for the whole expression, not only
		  * the '*' within it.
		  */
		final val * :AggregateSQL[FromSome, AggregateClause, Nothing, Int] =
			new DefaultAggregateSQL[FromSome, AggregateOf[FromSome], Nothing, Int](this, AllColumns, false)


		private case object AllColumns extends ColumnSQL[FromClause, LocalScope, Nothing] {
			override def readForm :ColumnReadForm[Nothing] =
				ColumnReadForm.unsupported("count(*).expr.readForm")


			override def asGlobal :Option[ColumnSQL[FromClause, GlobalScope, Nothing]] = None

			override def basedOn[U <: FromClause, E <: FromClause]
			                    (base :E)(implicit ext :U PartOf E) :ColumnSQL[E, LocalScope, Nothing] = this

			override def extend[U <: FromClause, E <: FromClause]
			                   (base :E)(implicit ev :U ExtendedBy E, global :GlobalScope <:< LocalScope) :Nothing =
				throw new UnsupportedOperationException("Count(*) cannot be extended over to subselect clauses.")

			override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                    (matcher :ColumnMatcher[FromClause, Y]) :Y[LocalScope, Nothing] =
				matcher.*(this)


			override def isomorphic(expression: SQLExpression.*) :Boolean = this == expression

			override def toString = "*"
		}

	}



	/** An SQL aggregate function whose result type is the same as the type of its argument.
	  * It is applicable only to [[net.noresttherein.oldsql.sql.SQLNumber numeric]] SQL types.
	  * Note that the preferred way for including an aggregate function in the ''select'' clause of an SQL ''select''
	  * ''without'' a ''group by'' clause is to use one of the factory methods included in
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]], which create the appropriate
	  * '`select `''fun''`(`''arg''`) from `''F'' ' directly.
	  * @param name the name of the function
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Max]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Min]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Sum]]
	  */
	class HomomorphicAggregateFunction(override val name :String) extends AggregateFunction {

		/** Applies this aggregate function to the given numeric expression.
		  * @return an SQL expression representing `<this function>(`''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def apply[F <: FromSome, X :SQLNumber]
		         (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, X] =
			AggregateSQL(this, arg, false)(arg.readForm)

		/** Applies this aggregate function to the given numeric expression preceded with the `DISTINCT` clause.
		  * @return an SQL expression representing `<this function>(DISTINCT `''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def distinct[F <: FromSome, X :SQLNumber]
		            (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, X] =
			AggregateSQL(this, arg, true)(arg.readForm)
	}


	/** An SQL aggregate function applicable to any [[net.noresttherein.oldsql.sql.SQLNumber numeric]] type and
	  * which always returns a [[net.noresttherein.oldsql.sql.SQLNumber.SQLFraction fractional]] value.
	  * Note that the preferred way for including an aggregate function in the ''select'' clause of an SQL ''select''
	  * ''without'' a ''group by'' clause is to use one of the factory methods included in
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]], which create the appropriate
	  * '`select `''fun''`(`''arg''`) from `''F'' ' directly.
	  * @param name the name of the function.
	  * @tparam V return type of the function. Must have implicit
	  *           [[net.noresttherein.oldsql.sql.SQLNumber.SQLFraction fractional]] type class and a
	  *           [[net.noresttherein.oldsql.schema.ColumnReadForm]].
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Avg]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.Var]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateSQL.StdDev]]
	  */
	class FractionalAggregateFunction[V :SQLFraction :ColumnReadForm](override val name :String)
		extends AggregateFunction
	{
		/** Applies this aggregate function to the given numeric expression.
		  * @return an SQL expression representing `<this function>(`''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def apply[F <: FromSome, X :SQLNumber]
		         (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, V] =
			AggregateSQL(this, arg, false)

		/** Applies this aggregate function to the given numeric expression, preceded by the `DISTINCT` clause.
		  * @return an SQL expression representing `<this function>(DISTINCT `''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def distinct[F <: FromSome, X :SQLNumber]
		            (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, V] =
			AggregateSQL(this, arg, true)
	}



	final val Min = new HomomorphicAggregateFunction("min")
	final val Max = new HomomorphicAggregateFunction("max")
	final val Sum = new HomomorphicAggregateFunction("sum")
	//todo: conversions to other fractional types
	final val Avg = new FractionalAggregateFunction[BigDecimal]("avg")
	final val Var = new FractionalAggregateFunction[BigDecimal]("var")
	final val StdDev = new FractionalAggregateFunction[BigDecimal]("stddev")



	//the problem with the visitor pattern is that it prevents us from narrowing F to GroupByClause
	trait AggregateMatcher[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		/** A special pseudo expression of `*` used inside `count(*)`. The actual type is not exposed. */
		def *(e :ColumnSQL[FromClause, LocalScope, Nothing]) :Y[LocalScope, Nothing]

		def aggregate[D <: FromSome, X, V](e :AggregateSQL[D, F, X, V]) :Y[LocalScope, V]
	}

	type MatchAggregate[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AggregateMatcher[F, Y]

	type CaseAggregate[+F <: FromClause, +Y[-_ >: LocalScope <: GlobalScope, _]] = AggregateMatcher[F, Y]

}



