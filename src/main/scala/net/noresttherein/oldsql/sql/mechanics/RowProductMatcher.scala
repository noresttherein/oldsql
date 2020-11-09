package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.BaseMapping
import net.noresttherein.oldsql.sql._
import net.noresttherein.oldsql.sql.DecoratedFrom.{ExtendingDecorator, FromSomeDecorator}
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom
import net.noresttherein.oldsql.sql.UnboundParam.FromParam






/**
  * @author Marcin Mościcki
  */
class RowProductMatcher[Y] {
	def apply(from :RowProduct) :Option[Y] = from.bridgeMatchWith(this)

	def rowProduct(from :RowProduct) :Option[Y] = None
	def fromClause(from :FromClause) :Option[Y] = rowProduct(from)
	def dual(dual :Dual) :Option[Y] = fromClause(dual)
	def andFrom[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L AndFrom R) :Option[Y] = fromClause(from)
	def from[T[O] <: BaseMapping[S, O], S](from :From[T]) :Option[Y] = andFrom[Dual, T, S](from)
	def joinLike[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S](join :L JoinLike R) :Option[Y] = andFrom[L, R, S](join)
	def subselect[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S](subselect :L Subselect R) :Option[Y] =
		joinLike[L, R, S](subselect)
	def join[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L Join R) :Option[Y] = joinLike[L, R, S](join)
	def innerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L InnerJoin R) :Option[Y] = this.join[L, R, S](join)
	def outerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L OuterJoin R) :Option[Y] = this.join[L, R, S](join)
	def leftJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L LeftJoin R) :Option[Y] = this.join[L, R, S](join)
	def rightJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L RightJoin R) :Option[Y] = this.join[L, R, S](join)
	def joinParam[L <: FromSome, P[O] <: FromParam[S, O], S](param :L JoinParam P) :Option[Y] = andFrom[L, P, S](param)


	def aggregateClause(from :AggregateClause) :Option[Y] = rowProduct(from)
	def aggregated[F <: FromSome](aggregate :Aggregated[F]) :Option[Y] = aggregateClause(aggregate)


	def groupByClause(from :GroupByClause) :Option[Y] = aggregateClause(from)
	def groupBy[L <: FromSome, R[O] <: BaseMapping[S, O], S](group :L GroupBy R) :Option[Y] = groupByClause(group)
	def andBy[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L AndBy R) :Option[Y] = groupByClause(group)
	def by[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L By R) :Option[Y] = andBy[L, R, S](group)
	def groupParam[L <: GroupByClause, P[O] <: FromParam[S, O], S](param :L GroupParam P) :Option[Y] =
		andBy[L, P, S](param)




	def compound[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L Compound R) :Option[Y] = rowProduct(from)
	def extended[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L Extended R) :Option[Y] = compound[L, R, S](from)
	def param[L <: NonEmptyFrom, P[O] <: FromParam[S, O], S](param :L UnboundParam P) :Option[Y] = extended[L, P, S](param)


	def decorator[F <: RowProduct](from :DecoratedFrom[F]) :Option[Y] = rowProduct(from)
	def extendingDecorator[F <: RowProduct](from :ExtendingDecorator[F]) :Option[Y] = decorator(from)
	def fromSomeDecorator[F <: FromSome](from :FromSomeDecorator[F]) :Option[Y] = extendingDecorator(from)

//	def aliased[F <: FromSome, N <: Label](from :F Aliased N) :Option[Y] = fromSomeDecorator(from)
}
