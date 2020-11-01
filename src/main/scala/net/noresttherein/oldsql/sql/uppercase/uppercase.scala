package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.implicitSQLLiterals.boundParameterSQL
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.NonEmptyFrom
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnLiteral, SQLParameter}






/**
  * @author Marcin Mościcki
  */
package object uppercase {

	type NULL[V] = SQLTerm.SQLNull[V]

	val NULL = SQLTerm.SQLNull
	val TRUE :ColumnLiteral[Boolean] = SQLTerm.True
	val FALSE :ColumnLiteral[Boolean] = SQLTerm.False

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
	type GROUP_BY_ALL[+L <: FromSome, R[O] <: MappingAt[O]] = L GroupByAll R
	type BY_ALL[+L <: GroupByClause, R[O] <: MappingAt[O]] = L ByAll R

	//todo: capitalized FromSomeExtension; best to wait until we are sure is close to final


	val NOT = LogicalSQL.NOT

	val EXISTS = ConditionSQL.ExistsSQL



	val COUNT = AggregateSQL.Count
	val MIN = AggregateSQL.Min
	val MAX = AggregateSQL.Max
	val SUM = AggregateSQL.Sum
	val AVG = AggregateSQL.Avg
	val VAR = AggregateSQL.Var
	val STDDEV = AggregateSQL.StdDev

}
