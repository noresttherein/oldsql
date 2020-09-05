package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.{ColumnReadForm, Mapping, SQLReadForm}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction
import net.noresttherein.oldsql.sql.GroupedExpression.{FlatMapGroup, MapGroup}
import net.noresttherein.oldsql.sql.ArithmeticSQL.SQLArithmetic
import net.noresttherein.oldsql.sql.ColumnSQL.ColumnMatcher
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy






class AggregateSQL[-F <: FromClause, S, T]
                  (val function :AggregateFunction, val expr :GroupedExpression[F, S], val isDistinct :Boolean)
                  (implicit override val readForm :ColumnReadForm[T])
	extends ColumnSQL[F, T]
{
	def distinct :AggregateSQL[F, S, T] =
		if (isDistinct) this
		else new AggregateSQL[F, S, T](function, expr, true)

	override def applyTo[Y[_]](matcher :ColumnMatcher[F, Y]) :Y[T] = ??? //matcher.aggregate(this)

	override def stretch[U <: F, E <: FromClause](base :E)(implicit ev :U ExtendedBy E) :AggregateSQL[E, S, T] = ???
//		new AggregateSQL[E, S, T](function, expr.stretch(base), isDistinct)

	override def isomorphic(that: SQLExpression.*) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL[_, _, _] if aggr canEqual this =>
			function == aggr.function && isDistinct == aggr.isDistinct && expr.isomorphic(aggr.expr)
		case _ => false
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateSQL[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case aggr :AggregateSQL[_, _, _] if aggr canEqual this =>
			function == aggr.function && distinct == aggr.distinct && expr == aggr.expr
		case _ => false
	}

	override def hashCode :Int = (function.hashCode * 31 + distinct.hashCode) * 31 + expr.hashCode

	override def toString :String =
		if (isDistinct) s"${function.name}(distinct $expr)" else s"${function.name}($expr)"

}






object AggregateSQL {


	def unapply[F <: FromClause, S, T]
	           (e :AggregateSQL[F, S, T]) :Some[(AggregateFunction, GroupedExpression[F, S], Boolean)] =
		Some((e.function, e.expr, e.isDistinct))

	def unapply[F <: FromClause, T](e :SQLExpression[F, T]) :Option[(AggregateFunction, GroupedExpression[F, _], Boolean)] =
		e match {
			case aggr :AggregateSQL[F @unchecked, _, _] =>Some((aggr.function, aggr.expr, aggr.isDistinct))
			case _ => None
		}



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



	case object Count extends AggregateFunction {
		override val name = "count"

		object * extends GroupedExpression[FromClause, Nothing] {

			override protected case object expr extends ColumnSQL[FromClause, Nothing] {
				override def readForm :ColumnReadForm[Nothing] =
					ColumnReadForm.unsupported("count(*).readForm")

				override def stretch[U <: FromClause, E <: FromClause]
				                    (base :E)(implicit ev :U ExtendedBy E) :ColumnSQL[E, Nothing] = this

				override def applyTo[Y[_]](matcher :ColumnMatcher[FromClause, Y]) :Y[Nothing] = ??? //matcher.*(this)

				override def isomorphic(expression: SQLExpression.*) :Boolean = this == expression

				override def toString = "*"
			}

//			override def stretch[S <: FromClause, E <: GroupByClause]
//			                    (base :E)(implicit extension :S ExtendedBy E) :GroupedExpression[E, Nothing] = this

			override def toString = "*"
		}

		def apply() :AggregateSQL[FromClause, Nothing, Int] =
			new AggregateSQL[FromClause, Nothing, Int](this, *, false)

		def apply[F <: FromClause, V](expr :GroupedExpression[F, V]) :AggregateSQL[F, V, Int] =
			new AggregateSQL(this, expr, false)

		def distinct[F <: FromClause, V](expr :GroupedExpression[F, V]) :AggregateSQL[F, V, Int] =
			new AggregateSQL(this, expr, true)
	}


	class HomomorphicAggregateFunction(override val name :String) extends AggregateFunction {

		def apply[F <: FromClause, X :SQLArithmetic :ColumnReadForm]
		         (expr :GroupedExpression[F, X]) :AggregateSQL[F, X, X] =
			new AggregateSQL[F, X, X](this, expr, false)

		def distinct[F <: FromClause, X :SQLArithmetic :ColumnReadForm]
		            (expr :GroupedExpression[F, X]) :AggregateSQL[F, X, X] =
			new AggregateSQL[F, X, X](this, expr, true)
	}

	class FractionalAggregateFunction[T :SQLArithmetic :ColumnReadForm](override val name :String)
		extends AggregateFunction
	{
		def apply[F <: FromClause, S :SQLArithmetic](expr :GroupedExpression[F, S]) :AggregateSQL[F, S, T] =
			new AggregateSQL(this, expr, false)

		def distinct[F <: FromClause, S :SQLArithmetic](expr :GroupedExpression[F, S]) :AggregateSQL[F, S, T] =
			new AggregateSQL(this, expr, true)
	}



	final val Max = new HomomorphicAggregateFunction("max")
	final val Min = new HomomorphicAggregateFunction("min")
	final val Sum = new HomomorphicAggregateFunction("sum")

	final val Avg = new FractionalAggregateFunction[BigDecimal]("avg")
	final val Var = new FractionalAggregateFunction[BigDecimal]("var")
	final val StdDev = new FractionalAggregateFunction[BigDecimal]("stddev")



	trait AggregateMatcher[+F <: FromClause, +Y[_]] {
		def aggregate[S, T](e :AggregateSQL[F, S, T]) :Y[T]

		/** A special pseudo expression used inside `count(*)`. The type itself is not exposed and this method
		  * will never be called under normal circumstances. */
		def *(e :ColumnSQL[FromClause, Nothing]) :Y[Nothing]
	}

	type MatchAggregate[+F <: FromClause, +Y[_]] = AggregateMatcher[F, Y]

	type CaseAggregate[+F <: FromClause, +Y[_]] = AggregateMatcher[F, Y]

}






/** A projections of rows from a single ''group by'' group to a column or columns evaluating to an `SQLExpression[F, V]`.
  * A group expression can be used as an argument for SQL aggregate functions.
  * It is a monad lifting that expression type, as evaluated for a single row, to multiple rows.
  * @tparam F an ungrouped ''from'' clause, that is the left side (i.e. the first type argument)
  *           of the `GroupBy`/`GroupByAll` clause owning this group.
  * @tparam V the value type of the lifted SQL expression
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  */
trait GroupedExpression[-F <: FromClause, V] {
	protected val expr :SQLExpression[F, V]

	def map[I, O, G <: GroupedExpression[_, _]](f :I => O)(implicit doMap :MapGroup[this.type, I, O, G]) :G = doMap(this, f)

	def flatMap[I, G <: GroupedExpression[_, _]](f :I => G)(implicit doMap :FlatMapGroup[this.type, I, G]) :G = doMap(this, f)

//	def stretch[S <: F, E <: GroupByClause](base :E)(implicit extension :S ExtendedBy E) :GroupedExpression[E, V] =

	def isomorphic(that :GroupedExpression[_ <: FromClause, _]) :Boolean = expr isomorphic that.expr

	def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupedExpression[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case group :GroupedExpression[_, _] => (group eq this) || group.canEqual(this) && group.expr == expr
		case _ => false
	}

	override def hashCode :Int = expr.hashCode

	override def toString :String = "{" + expr + "*}"

}






object GroupedExpression {

	private class BaseGroupedExpression[F <: FromClause, V](protected override val expr :SQLExpression[F, V])
		extends GroupedExpression[F, V]



	//consider: parameterize with M[O] <: MappingAt to avoid long origin types; can't be contravariant in that case
	trait MappingGroupedExpression[-F <: FromClause, M <: Mapping] extends GroupedExpression[F, M#Subject] {
		protected override val expr :MappingSQL[F, M] //fixme: this must be a ComponetSQL, ugh

		def mapping :M = expr.mapping

//		override def stretch[S <: F, E <: GroupByClause]
//		                    (base :E)(implicit extension :ExtendedBy[S, E]) :GroupedExpression[E, M#Subject] = ???

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingGroupedExpression[_, _]]

		override def toString :String = "MappingGroupedExpression(" + expr.mapping + ")"
	}

	private class BaseMappingGroupedExpression[-F <: FromClause, M <: Mapping](protected override val expr :MappingSQL[F, M])
		extends MappingGroupedExpression[F, M]



	object MappingGroupedExpression {

		implicit def mapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:MapGroup[MappingGroupedExpression[F, X[O]], X[O], Y[O], MappingGroupedExpression[F, Y[O]]] =
			(group :MappingGroupedExpression[F, X[O]], f :X[O] => Y[O]) => ??? //new BaseMappingGroupedExpression(f(group.mapping))

		implicit def mapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:MapGroup[MappingGroupedExpression[F, X], X, SQLExpression[F, Y], GroupedExpression[F, Y]] =
			(group :MappingGroupedExpression[F, X], f :X => SQLExpression[F, Y]) => ???


		implicit def flatMapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:FlatMapGroup[MappingGroupedExpression[F, X[O]], X[O], MappingGroupedExpression[F, Y[O]]] =
			(group :MappingGroupedExpression[F, X[O]], f :X[O] => MappingGroupedExpression[F, Y[O]]) => f(group.mapping)

		implicit def flatMapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:FlatMapGroup[MappingGroupedExpression[F, X], X, GroupedExpression[F, Y]] =
			(group :MappingGroupedExpression[F, X], f :X => GroupedExpression[F, Y]) => f(group.mapping)
	}


//	trait GroupColumn[-F <: FromClause, V] extends GroupedExpression[F, V] {
//	}


	@implicitNotFound("Cannot map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be an SQLExpression of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingGroupedExpression[F, M], the argument may be the mapping M " +
	                  "and the result type may be any of its components.\n" +
		              "Missing implicit MapGroup[${G}, ${I}, ${O}, ${G}].")
	abstract class MapGroup[-G <: GroupedExpression[_, _], I, O, +R <: GroupedExpression[_, _]] {
		def apply(group :G, f :I => O) :R
	}

	implicit def defaultMapGroup[F <: FromClause, X, Y]
			:MapGroup[GroupedExpression[F, X], SQLExpression[F, X], SQLExpression[F, Y], GroupedExpression[F, Y]] =
		(group :GroupedExpression[F, X], f :SQLExpression[F, X] => SQLExpression[F, Y]) => new BaseGroupedExpression(f(group.expr))



	@implicitNotFound("Cannot flat map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be a GroupedExpression of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingGroupedExpression[F, M], the argument may be the mapping M " +
	                  "and the result type may be a MappingGroupedExpression for any of its components.\n" +
	                  "Missing implicit FlatMapGroup[${G}, ${I}, ${O}].")
	abstract class FlatMapGroup[-G <: GroupedExpression[_, _], I, O <: GroupedExpression[_, _]] {
		def apply(group :G, f :I => O) :O
	}

	implicit def defaultFlatMapGroup[F <: FromClause, X, Y]
			:FlatMapGroup[GroupedExpression[F, X], SQLExpression[F, X], GroupedExpression[F, Y]] =
		(group :GroupedExpression[F, X], f :SQLExpression[F, X] => GroupedExpression[F, Y]) => f(group.expr)



}
