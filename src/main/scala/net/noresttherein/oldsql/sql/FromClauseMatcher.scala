package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.GroupByAll.{AndByAll, ByAll}
import net.noresttherein.oldsql.sql.UnboundParam.FromParam



/**
  * @author Marcin Mościcki
  */
class FromClauseMatcher[Y] {
	def apply(from :FromClause) :Option[Y] = None //todo:

	def fromClause(from :FromClause) :Option[Y] = None
	def discreteFrom(from :DiscreteFrom) :Option[Y] = fromClause(from)
	def dual(dual :Dual) :Option[Y] = discreteFrom(dual)
	def andFrom[L <: FromClause, R[O] <: BaseMapping[S, O], S](from :L AndFrom R) :Option[Y] = discreteFrom(from)
	def from[T[O] <: BaseMapping[S, O], S](from :From[T]) :Option[Y] = andFrom[Dual, T, S](from)
	def joinLike[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L JoinLike R) :Option[Y] = andFrom[L, R, S](join)
	def subselect[L <: FromSome, R[O] <: BaseMapping[S, O], S](subselect :L Subselect R) :Option[Y] =
		joinLike[L, R, S](subselect)
	def join[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L Join R) :Option[Y] = joinLike[L, R, S](join)
	def innerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L InnerJoin R) :Option[Y] = this.join[L, R, S](join)
	def outerJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L OuterJoin R) :Option[Y] = this.join[L, R, S](join)
	def leftJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L LeftJoin R) :Option[Y] = this.join[L, R, S](join)
	def rightJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S](join :L RightJoin R) :Option[Y] = this.join[L, R, S](join)
	def joinParam[L <: FromSome, P[O] <: FromParam[S, O], S](param :L JoinParam P) :Option[Y] = andFrom[L, P, S](param)


	def groupByClause(from :GroupByClause) :Option[Y] = fromClause(from)
	def groupBy[L <: FromSome, R[O] <: BaseMapping[S, O], S](group :L GroupByAll R) :Option[Y] = groupByClause(group)
	def andBy[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L AndByAll R) :Option[Y] = groupByClause(group)
	def by[L <: GroupByClause, R[O] <: BaseMapping[S, O], S](group :L ByAll R) :Option[Y] = andBy[L, R, S](group)
	def groupParam[L <: GroupByClause, P[O] <: FromParam[S, O], S](param :L GroupParam P) :Option[Y] =
		andBy[L, P, S](param)

	def using[L <: FromClause, R[O] <: BaseMapping[S, O], S](from :L Using R) :Option[Y] = fromClause(from)
	def extended[L <: FromClause, R[O] <: BaseMapping[S, O], S](from :L Extended R) :Option[Y] = using[L, R, S](from)
}
