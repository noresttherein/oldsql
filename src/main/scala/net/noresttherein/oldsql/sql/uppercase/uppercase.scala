package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.mechanics.implicitSQLLiterals.boundParameterSQL
import net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom
import net.noresttherein.oldsql.sql.ast.{ColumnLiteral, ConditionSQL, LogicalSQL, SQLLiteral, SQLNull, SQLParameter, SQLTerm}
import net.noresttherein.oldsql.sql.mechanics.SQLLiteralImplicits






/**
  * @author Marcin Mościcki
  */
package object uppercase extends SQLLiteralImplicits {

	type NULL[V] = SQLNull[V]

	val NULL = SQLNull
	val TRUE :ColumnLiteral[Boolean] = SQLLiteral.True
	val FALSE :ColumnLiteral[Boolean] = SQLLiteral.False

	def PARAM[T](value :T)(implicit factory :SQLParameter.Factory[T]) :factory.Res =
		factory(value)

	implicit def PARAM_?[T](value :T)(implicit factory :SQLParameter.Factory[T]) :boundParameterSQL[T, factory.Res] =
		boundParameterSQL[T, factory.Res](value)(factory :SQLParameter.Factory[T] { type Res = factory.Res })



	type DUAL = Dual
	val DUAL = Dual

	type FROM[T[O] <: MappingAt[O]] = From[T]
	val FROM = From

	type JOIN[+L <: FromSome, R[O] <: MappingAt[O]] = L Join R
	type INNER_JOIN[+L <: FromSome, R[O] <: MappingAt[O]] = L InnerJoin R
	type OUTER_JOIN[+L <: FromSome, R[O] <: MappingAt[O]] = L OuterJoin R
	type LEFT_JOIN[+L <: FromSome, R[O] <: MappingAt[O]] = L LeftJoin R
	type RIGHT_JOIN[+L <: FromSome, R[O] <: MappingAt[O]] = L RightJoin R
	type SUBSELECT[+L <: NonEmptyFrom, R[O] <: MappingAt[O]] = L Subselect R
	type GROUP_BY_ALL[+L <: FromSome, R[O] <: MappingAt[O]] = L GroupBy R
	type BY_ALL[+L <: GroupByClause, R[O] <: MappingAt[O]] = L By R

	//todo: capitalized FromSomeExtension; best to wait until we are sure is close to final


	val NOT = LogicalSQL.NotSQL

	val EXISTS = ConditionSQL.ExistsSQL



	val COUNT = AggregateFunction.Count
	val MIN = AggregateFunction.Min
	val MAX = AggregateFunction.Max
	val SUM = AggregateFunction.Sum
	val AVG = AggregateFunction.Avg
	val VAR = AggregateFunction.Var
	val STDDEV = AggregateFunction.StdDev

}
