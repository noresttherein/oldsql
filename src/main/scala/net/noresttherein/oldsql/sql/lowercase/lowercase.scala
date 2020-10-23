package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.NonEmptyFrom
import net.noresttherein.oldsql.sql.SQLTerm.SQLParameter






/**
  * @author Marcin Mościcki
  */
package object lowercase extends SQLLiteralImplicits {

	def param[T](value :T)(implicit factory :SQLParameter.Factory[T]) :factory.Res =
		factory(value)

	implicit def param_?[T](value :T)(implicit factory :SQLParameter.Factory[T]) :boundParameterSQL[T, factory.Res] =
		boundParameterSQL[T, factory.Res](value)(factory :SQLParameter.Factory[T] { type Res = factory.Res })


	type dual = Dual
	val dual = Dual

	type from[T[O] <: MappingAt[O]] = From[T]
	val from = From

	type join[+L <: FromSome, R[O] <: MappingAt[O]] = L Join R
	type innerJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L InnerJoin R
	type outerJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L OuterJoin R
	type leftJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L LeftJoin R
	type rightJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L RightJoin R
	type subselect[+L <: NonEmptyFrom, R[O] <: MappingAt[O]] = L Subselect R
	type groupByAll[+L <: FromSome, R[O] <: MappingAt[O]] = L GroupByAll R
	type byAll[+L <: GroupByClause, R[O] <: MappingAt[O]] = L ByAll R


//	type not[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.NOT[F, S]

	val not = LogicalSQL.NOT

//	type exists[-F <: FromClause, S >: LocalScope <: GlobalScope, V] = ConditionSQL.ExistsSQL[F, S, V]

	val exists = ConditionSQL.ExistsSQL

	val like = ConditionSQL.LikeSQL




	val count = AggregateSQL.Count
	val min = AggregateSQL.Min
	val max = AggregateSQL.Max
	val sum = AggregateSQL.Sum
	val avg = AggregateSQL.Avg
	val variance = AggregateSQL.Var
	val stddev = AggregateSQL.StdDev



}
