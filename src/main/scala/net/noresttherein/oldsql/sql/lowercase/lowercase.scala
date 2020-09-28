package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.NonEmptyFrom
import net.noresttherein.oldsql.sql.GroupByAll.ByAll
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.{SQLParameter, SQLTermFactory}

/**
  * @author Marcin Mościcki
  */
package object lowercase extends SQLLiteralImplicits {

	def param[T, P <: SQLParameter[T]](value :T)(implicit factory :SQLTermFactory[T, P]) :P =
		factory(value)

//	implicit def param_?[T, P <: SQLParameter[T]]
//	                    (value :T)(implicit factory :SQLTermFactory[T, P]) :boundParameterSQL[T, P] =
//		boundParameterSQL(value)



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


	type not[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.NOT[F, S]

	val not = LogicalSQL.NOT

//	type and[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.AND[F, S]
//
//	val and = LogicalSQL.AND
//
//	type or[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.OR[F, S]
//
//	val or = LogicalSQL.OR



	type exists[-F <: FromClause, S >: LocalScope <: GlobalScope, V] = ConditionSQL.ExistsSQL[F, S, V]

	val exists = ConditionSQL.ExistsSQL




	val count = AggregateSQL.Count
	val min = AggregateSQL.Min
	val max = AggregateSQL.Max
	val sum = AggregateSQL.Sum
	val avg = AggregateSQL.Avg
	val variance = AggregateSQL.Var
	val stddev = AggregateSQL.StdDev



}
