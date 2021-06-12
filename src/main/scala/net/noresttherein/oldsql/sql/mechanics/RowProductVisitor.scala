package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{Adjoin, AggregateClause, Aggregated, AndFrom, By, DecoratedFrom, Dual, Expanded, From, FromClause, FromSome, GroupBy, GroupByClause, GroupParam, InnerJoin, Join, JoinLike, JoinParam, LeftJoin, OuterJoin, RightJoin, RowProduct, Subselect, UnboundParam}
import net.noresttherein.oldsql.sql.DecoratedFrom.{ExpandingDecorator, FromSomeDecorator}
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom
import net.noresttherein.oldsql.sql.UnboundParam.FromParam






/** A visitor interface working for the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] type hierarchy. */
//todo: this should probably be moved to RowProduct for consistency with ExpressionVisitor and StatementVisitor
//todo: rename to visitor
trait RowProductVisitor[Y] {
	def apply(from :RowProduct) :Y = from.applyToForwarder(this)

	def rowProduct(from :RowProduct) :Y
	def fromClause(from :FromClause) :Y = rowProduct(from)
	def dual(dual :Dual) :Y = fromClause(dual)
	def andFrom[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L AndFrom R) :Y = fromClause(from)
	def from[T[O] <: BaseMapping[S, O], S](from :From[T]) :Y = andFrom[Dual, T, S](from)
	def joinLike[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S](join :L JoinLike R) :Y = andFrom[L, R, S](join)
	def subselect[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S](subselect :L Subselect R) :Y =
		joinLike[L, R, S](subselect)
	def join[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L Join R) :Y = joinLike[L, R, S](join)
	def innerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L InnerJoin R) :Y = this.join[L, R, S](join)
	def outerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L OuterJoin R) :Y = this.join[L, R, S](join)
	def leftJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L LeftJoin R) :Y = this.join[L, R, S](join)
	def rightJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L RightJoin R) :Y = this.join[L, R, S](join)
	def joinParam[L <: FromSome, P[O] <: FromParam[S, O], S](param :L JoinParam P) :Y = andFrom[L, P, S](param)


	def aggregateClause(from :AggregateClause) :Y = rowProduct(from)
	def aggregated[F <: FromSome](aggregate :Aggregated[F]) :Y = aggregateClause(aggregate)

	def groupByClause(from :GroupByClause) :Y = aggregateClause(from)
	def groupBy[L <: FromSome, R[O] <: BaseMapping[S, O], S](group :L GroupBy R) :Y = groupByClause(group)
	def andBy[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L AndBy R) :Y = groupByClause(group)
	def by[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L By R) :Y = andBy[L, R, S](group)
	def groupParam[L <: GroupByClause, P[O] <: FromParam[S, O], S](param :L GroupParam P) :Y =
		andBy[L, P, S](param)


	def adjoin[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L Adjoin R) :Y = rowProduct(from)
	def expanded[L <: RowProduct, R[O] <: BaseMapping[S, O], S](from :L Expanded R) :Y = adjoin[L, R, S](from)
	def param[L <: NonEmptyFrom, P[O] <: FromParam[S, O], S](param :L UnboundParam P) :Y = expanded[L, P, S](param)


	def decorator[F <: RowProduct](from :DecoratedFrom[F]) :Y = rowProduct(from)
	def expandingDecorator[F <: RowProduct](from :ExpandingDecorator[F]) :Y = decorator(from)
	def fromSomeDecorator[F <: FromSome](from :FromSomeDecorator[F]) :Y = expandingDecorator(from)

//	def aliased[F <: FromSome, N <: Label](from :F Aliased N) :Option[Y] = fromSomeDecorator(from)
}
