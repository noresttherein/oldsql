package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.AggregateSQL.{FlatMapGroup, MapGroup}






/** A projections of rows from a single ''group by'' group to a column or columns evaluating to an `SQLExpression[F, V]`.
  * A group expression can be used as an argument for SQL aggregate functions.
  * It is a monad lifting that expression type, as evaluated for a single row, to multiple rows.
  * @tparam F an ungrouped ''from'' clause, that is the left side (i.e. the first type argument)
  *           of the `GroupBy`/`GroupByAll` clause owning this group.
  * @tparam V the value type of the lifted SQL expression
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  */
trait AggregateSQL[-F <: FromClause, V] {
	protected val expr :SQLExpression[F, V]

	def map[I, O, G <: AggregateSQL[_, _]](f :I => O)(implicit doMap :MapGroup[this.type, I, O, G]) :G = doMap(this, f)

	def flatMap[I, G <: AggregateSQL[_, _]](f :I => G)(implicit doMap :FlatMapGroup[this.type, I, G]) :G = doMap(this, f)

//	def stretch[E <: GroupedFrom]()


	def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateSQL[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case group :AggregateSQL[_, _] => (group eq this) || group.canEqual(this) && group.expr == expr
		case _ => false
	}

	override def hashCode :Int = expr.hashCode

	override def toString :String = "AggregateSQL(" + expr + ")"
}






object AggregateSQL {

	private class BaseAggregateSQL[F <: FromClause, V](protected override val expr :SQLExpression[F, V]) extends AggregateSQL[F, V]



	//consider: parameterize with M[O] <: MappingAt to avoid long origin types; can't be contravariant in that case
	trait MappingAggregateSQL[-F <: FromClause, M <: Mapping] extends AggregateSQL[F, M#Subject] {
		protected override val expr :MappingSQL[F, M] //fixme: this must be a ComponetSQL, ugh

		def mapping :M = expr.mapping

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingAggregateSQL[_, _]]

		override def toString :String = "MappingAggregateSQL(" + expr.mapping + ")"
	}

	private class BaseMappingAggregateSQL[-F <: FromClause, M <: Mapping](protected override val expr :MappingSQL[F, M])
		extends MappingAggregateSQL[F, M]



	object MappingAggregateSQL {

		implicit def mapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:MapGroup[MappingAggregateSQL[F, X[O]], X[O], Y[O], MappingAggregateSQL[F, Y[O]]] =
			(group :MappingAggregateSQL[F, X[O]], f :X[O] => Y[O]) => ??? //new BaseMappingAggregateSQL(f(group.mapping))

		implicit def mapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:MapGroup[MappingAggregateSQL[F, X], X, SQLExpression[F, Y], AggregateSQL[F, Y]] =
			(group :MappingAggregateSQL[F, X], f :X => SQLExpression[F, Y]) => ???


		implicit def flatMapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:FlatMapGroup[MappingAggregateSQL[F, X[O]], X[O], MappingAggregateSQL[F, Y[O]]] =
			(group :MappingAggregateSQL[F, X[O]], f :X[O] => MappingAggregateSQL[F, Y[O]]) => f(group.mapping)

		implicit def flatMapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:FlatMapGroup[MappingAggregateSQL[F, X], X, AggregateSQL[F, Y]] =
			(group :MappingAggregateSQL[F, X], f :X => AggregateSQL[F, Y]) => f(group.mapping)
	}


//	trait GroupColumn[-F <: FromClause, V] extends AggregateSQL[F, V] {
//	}


	@implicitNotFound("Cannot map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be an SQLExpression of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingAggregateSQL[F, M], the argument may be the mapping M " +
	                  "and the result type may be any of its components.\n" +
		              "Missing implicit MapGroup[${G}, ${I}, ${O}, ${G}].")
	abstract class MapGroup[-G <: AggregateSQL[_, _], I, O, +R <: AggregateSQL[_, _]] {
		def apply(group :G, f :I => O) :R
	}

	implicit def defaultMapGroup[F <: FromClause, X, Y]
			:MapGroup[AggregateSQL[F, X], SQLExpression[F, X], SQLExpression[F, Y], AggregateSQL[F, Y]] =
		(group :AggregateSQL[F, X], f :SQLExpression[F, X] => SQLExpression[F, Y]) => new BaseAggregateSQL(f(group.expr))



	@implicitNotFound("Cannot flat map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be a AggregateSQL of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingAggregateSQL[F, M], the argument may be the mapping M " +
	                  "and the result type may be a MappingAggregateSQL for any of its components.\n" +
	                  "Missing implicit FlatMapGroup[${G}, ${I}, ${O}].")
	abstract class FlatMapGroup[-G <: AggregateSQL[_, _], I, O <: AggregateSQL[_, _]] {
		def apply(group :G, f :I => O) :O
	}

	implicit def defaultFlatMapGroup[F <: FromClause, X, Y]
			:FlatMapGroup[AggregateSQL[F, X], SQLExpression[F, X], AggregateSQL[F, Y]] =
		(group :AggregateSQL[F, X], f :SQLExpression[F, X] => AggregateSQL[F, Y]) => f(group.expr)

}
