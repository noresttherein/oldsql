package net.noresttherein.oldsql.sql

import java.sql.JDBCType

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.schema.ColumnReadForm
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnVisitor
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, PartOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.AggregateSQL
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, SQLNumber, SQLOrdering}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLNumber.SQLFraction






/** An SQL aggregate function such as `count`, `max` or `stddev`. As these functions often work differently,
  * this interface is almost empty, serving only to group them all into a single type hierarchy.
  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.HomomorphicAggregateFunction]]
  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.RepresentativeAggregateFunction]]
  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.FractionalAggregateFunction]]
  */ //todo: window functions
trait AggregateFunction extends Serializable {
	val name :String

	protected def defaultSpelling[P, F <: RowProduct](arg :ColumnSQL[F, LocalScope, _], distinct :Boolean = false)
	                                                 (context :SQLContext, params :Parameterization[P, F])
	                                                 (implicit spelling :SQLSpelling)
			:SpelledSQL[P, F] =
	{
		val prefix = SpelledSQL(spelling.function(name) + (if (distinct) "(DISTINCT" else "("), context, params)
		prefix + (spelling(arg)(_, _)) + ")"
	}

	private[sql] final def defaultSpelling[P, F <: RowProduct](spelling :SQLSpelling)
	                                                          (arg :ColumnSQL[F, LocalScope, _], distinct :Boolean)
	                                                          (implicit context :SQLContext, params :Parameterization[P, F])
			:SpelledSQL[P, F] =
		defaultSpelling(arg, distinct)(context, params)(spelling)

	def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateFunction]

	override def equals(that :Any) :Boolean = that match {
		case fun :AggregateFunction => (fun eq this) || (fun canEqual this) && name == fun.name
		case _ => false
	}

	override def hashCode :Int = name.hashCode

	override def toString :String = name
}






/** Definitions of standard SQL aggregate functions. */
object AggregateFunction {

	/** Implementations of the SQL `COUNT` function. It accepts expressions of any type as the argument.
	  * When counting all the rows returned by an SQL ''select'', prefer the factory method
	  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.count count]] and its overloaded variants,
	  * available on not grouping, non-empty ''from'' clauses.
	  */
	case object Count extends AggregateFunction {
		override val name = "count"

		//todo: support for other integer return types, in particular Long

		/** Represents the `COUNT(*)` SQL expression. */
		def apply() :AggregateSQL[FromSome, AggregateClause, Nothing, Int] = *

		/** Represents the `COUNT(*)` SQL expression. */
		def apply(all :sql.*) :AggregateSQL[FromSome, AggregateClause, Nothing, Int] = *

		/** Represents the `COUNT(arg)` SQL expression. */
		def apply[F <: FromSome, V](arg :ColumnSQL[F, LocalScope, V]) :AggregateSQL[F, F#GeneralizedAggregate, V, Int] =
			AggregateSQL(this, arg, false)

		/** Represents the `COUNT(DISTINCT arg)` SQL expression. */
		def distinct[F <: FromSome, V](arg :ColumnSQL[F, LocalScope, V]) :AggregateSQL[F, F#GeneralizedAggregate, V, Int] =
			AggregateSQL(this, arg, true)


		/** Represents the `COUNT(*)` SQL expression. Note that this stands for the whole expression, not only
		  * the '*' within it.
		  */
//		final val * :AggregateSQL[FromSome, AggregateClause, Nothing, Int] =
//			new DefaultAggregateSQL[FromSome, AggregateOf[FromSome], Nothing, Int](this, AllColumns, false)
		val * :AggregateSQL[FromSome, AggregateClause, Nothing, Int] =
			new AggregateSQL[FromSome, AggregateClause, Nothing, Int] {
				override def readForm :ColumnReadForm[Int] = ColumnReadForm[Int]
				override def function :AggregateFunction = Count.this
				override def arg :ColumnSQL[FromSome, LocalScope, Nothing] = AllColumns
				override def isDistinct :Boolean = false

				override val distinct = new AggregateSQL[FromSome, AggregateClause, Nothing, Int] {
					override def readForm = ColumnReadForm[Int]
					override def function = Count.this
					override def arg = AllColumns
					override def isDistinct = true
					override def distinct = this

					protected override def defaultSpelling[P, E <: AggregateClause]
					                       (context :SQLContext, params :Parameterization[P, E])
					                       (implicit spelling :SQLSpelling) =
					{
						val sql = spelling.function("count") + "(" + spelling.keyword("distinct") + ")"
						SpelledSQL(sql, context, params)
					}

				}

				protected override def defaultSpelling[P, E <: AggregateClause]
				                       (context :SQLContext, params :Parameterization[P, E])
				                       (implicit spelling :SQLSpelling) =
					SpelledSQL(spelling.function("count(*)"), context, params)
			}

		/** The SQL expression used as the argument in `count(*)`. */
		private case object AllColumns extends ColumnSQL[RowProduct, LocalScope, Nothing] {
			override def readForm :ColumnReadForm[Nothing] =
				ColumnReadForm.unsupported(JDBCType.INTEGER, "count(*)")(
					"count(*).expr.readForm"
				)
			override def groundValue :Opt[Nothing] = Lack

			override def basedOn[U <: RowProduct, E <: RowProduct]
			                    (base :E)(implicit ext :U PartOf E) :ColumnSQL[E, LocalScope, Nothing] = this

			override def expand[U <: RowProduct, E <: RowProduct]
			                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< LocalScope) :Nothing =
				throw new UnsupportedOperationException("Count(*) cannot be expanded over to subselect clauses.")

			override def asGlobal :Option[ColumnSQL[RowProduct, GlobalScope, Nothing]] = None
			override def isAnchored = true
			override def anchor(from :RowProduct) = this

			protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
			                    (visitor :ColumnVisitor[RowProduct, Y]) :Y[LocalScope, Nothing] =
				visitor.*(this)

			protected override def defaultSpelling[P, E <: RowProduct]
			                       (context :SQLContext, params :Parameterization[P, E])
			                       (implicit spelling :SQLSpelling) =
				SpelledSQL("*", context, params)

			override def isomorphic(expression: SQLExpression.*) :Boolean = this == expression

			override def toString = "*"
		}

	}




	/** An SQL aggregate function returning one of the elements from the row set.
	  * It is applicable only to [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering ordered]] SQL types.
	  * Note that the preferred way for including an aggregate function in the ''select'' clause of an SQL ''select''
	  * ''without'' a ''group by'' clause is to use one of the factory methods included in
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]], which create the appropriate
	  * '`select `''fun''`(`''arg''`) from `''F'' ' directly.
	  * @param name the name of the function
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.HomomorphicAggregateFunction]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
	  */
	class RepresentativeAggregateFunction(override val name :String) extends AggregateFunction {

		/** Applies this aggregate function to the given column or expression.
		  * @return an SQL expression representing `<this function>(`''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def apply[F <: FromSome, X :SQLOrdering]
		         (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, X] =
			AggregateSQL(this, arg, false)(arg.readForm)

		/** Applies this aggregate function to the given column or expression, preceded with the `DISTINCT` clause.
		  * @return an SQL expression representing `<this function>(DISTINCT `''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def distinct[F <: FromSome, X :SQLOrdering]
		            (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, X] =
			AggregateSQL(this, arg, true)(arg.readForm)


		/** Presents this aggregate function as a `HomomorphicAggregateFunction` for polymorphism with
		  * [[net.noresttherein.oldsql.sql.AggregateFunction.Sum sum]].
		  */
		def homomorphic :HomomorphicAggregateFunction = new HomomorphicAggregateFunction(name)
	}



	/** An SQL aggregate function whose result type is the same as the type of its argument.
	  * It is applicable only to [[net.noresttherein.oldsql.sql.mechanics.SQLNumber numeric]] SQL types.
	  * The only standard instance is [[net.noresttherein.oldsql.sql.AggregateFunction.Sum Sum]].
	  * Note that the preferred way for including an aggregate function in the ''select'' clause of an SQL ''select''
	  * ''without'' a ''group by'' clause is to use one of the factory methods included in
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]], which create the appropriate
	  * '`select `''fun''`(`''arg''`) from `''F'' ' directly.
	  * @param name the name of the function
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.RepresentativeAggregateFunction]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
	  */
	class HomomorphicAggregateFunction(override val name :String) extends AggregateFunction {

		/** Applies this aggregate function to the given column or expression.
		  * @return an SQL expression representing `<this function>(`''arg''`)`. It can be used in the context
		  *         of any [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] whose
		  *         [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] ''from'' clause
		  *         (i.e., the relations under the grouping) is a subtype of `F`, guaranteeing that all relations
		  *         used by the argument are available to it.
		  */
		def apply[F <: FromSome, X :SQLNumber]
		         (arg :ColumnSQL[F, LocalScope, X]) :AggregateSQL[F, F#GeneralizedAggregate, X, X] =
			AggregateSQL(this, arg, false)(arg.readForm)

		/** Applies this aggregate function to the given column or expression, preceded with the `DISTINCT` clause.
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


	/** An SQL aggregate function applicable to any [[net.noresttherein.oldsql.sql.mechanics.SQLNumber numeric]] type and
	  * which always returns a [[net.noresttherein.oldsql.sql.mechanics.SQLNumber.SQLFraction fractional]] value.
	  * Note that the preferred way for including an aggregate function in the ''select'' clause of an SQL ''select''
	  * ''without'' a ''group by'' clause is to use one of the factory methods included in
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]], which create the appropriate
	  * '`select `''fun''`(`''arg''`) from `''F'' ' directly.
	  * @param name the name of the function.
	  * @tparam V return type of the function. Must have implicit
	  *           [[net.noresttherein.oldsql.sql.mechanics.SQLNumber.SQLFraction fractional]] type class and a
	  *           [[net.noresttherein.oldsql.schema.ColumnReadForm]].
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Avg]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
	  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
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



	final val Min = new RepresentativeAggregateFunction("min")
	final val Max = new RepresentativeAggregateFunction("max")
	final val Sum = new HomomorphicAggregateFunction("sum")
	//todo: conversions to other fractional types
	final val Avg = new FractionalAggregateFunction[BigDecimal]("avg")
	final val Var = new FractionalAggregateFunction[BigDecimal]("var")
	final val StdDev = new FractionalAggregateFunction[BigDecimal]("stddev")

}
