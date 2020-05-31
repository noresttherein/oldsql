package net.noresttherein.oldsql.sql

/**
  * @author Marcin Mościcki
  */
package object lowercase {

	type not[-F <: FromClause] = LogicalSQL.NOT[F]

	val not = LogicalSQL.NOT

	type and[-F <: FromClause] = LogicalSQL.AND[F]

	val and = LogicalSQL.AND

	type or[-F <: FromClause] = LogicalSQL.OR[F]

	val or = LogicalSQL.OR



	type exists[-F <: FromClause, V, O] = ConditionSQL.ExistsSQL[F, V, O]

	val exists = ConditionSQL.ExistsSQL




}
