package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom
import net.noresttherein.oldsql.sql.ast.{ConditionSQL, LogicalSQL}
import net.noresttherein.oldsql.sql.ast.SQLParameter
import net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits






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
	type groupByAll[+L <: FromSome, R[O] <: MappingAt[O]] = L GroupBy R
	type byAll[+L <: GroupByClause, R[O] <: MappingAt[O]] = L By R


//	type not[-F <: RowProduct, S >: LocalScope <: GlobalScope] = LogicalSQL.NotSQL[F, S]

	val not = LogicalSQL.NotSQL

//	type exists[-F <: RowProduct, S >: LocalScope <: GlobalScope, V] = ConditionSQL.ExistsSQL[F, S, V]

	val exists = ConditionSQL.ExistsSQL

	val like = ConditionSQL.LikeSQL




	val count = AggregateFunction.Count
	val min = AggregateFunction.Min
	val max = AggregateFunction.Max
	val sum = AggregateFunction.Sum
	val avg = AggregateFunction.Avg
	val variance = AggregateFunction.Var
	val stddev = AggregateFunction.StdDev



}
