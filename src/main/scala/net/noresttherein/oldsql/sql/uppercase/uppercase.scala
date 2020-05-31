package net.noresttherein.oldsql.sql

/**
  * @author Marcin Mościcki
  */
package object uppercase {

	type NOT[-F <: FromClause] = LogicalSQL.NOT[F]

	val NOT = LogicalSQL.NOT

	type AND[-F <: FromClause] = LogicalSQL.AND[F]

	val AND = LogicalSQL.AND

	type OR[-F <: FromClause] = LogicalSQL.OR[F]

	val OR = LogicalSQL.OR



	type EXISTS[-F <: FromClause, V, O] = ConditionSQL.ExistsSQL[F, V, O]

	val EXISTS = ConditionSQL.ExistsSQL



	type NULL[V] = SQLTerm.NULL[V]

	val NULL = SQLTerm.NULL

	val TRUE = SQLTerm.True

	val FALSE = SQLTerm.False
}
