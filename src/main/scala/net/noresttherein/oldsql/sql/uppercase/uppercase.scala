package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.ColumnForm
import net.noresttherein.oldsql.sql.SQLExpression.{boundParameterSQL, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.{ColumnLiteral, SQLParameter, SQLTermFactory}

/**
  * @author Marcin Mościcki
  */
package object uppercase {

	type NOT[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.NOT[F, S]

	val NOT = LogicalSQL.NOT

//	type AND[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.AND[F, S]
//
//	val AND = LogicalSQL.AND
//
//	type OR[-F <: FromClause, S >: LocalScope <: GlobalScope] = LogicalSQL.OR[F, S]
//
//	val OR = LogicalSQL.OR



	type EXISTS[-F <: FromClause, S >: LocalScope <: GlobalScope, V] = ConditionSQL.ExistsSQL[F, S, V]

	val EXISTS = ConditionSQL.ExistsSQL



	type NULL[V] = SQLTerm.NULL[V]

	def NULL[T :ColumnForm]() :SQLTerm[T] = SQLTerm.NULL[T]

	val NULL = SQLTerm.NULL

	val TRUE :ColumnLiteral[Boolean] = SQLTerm.True

	val FALSE :ColumnLiteral[Boolean] = SQLTerm.False

	def PARAM[T, P <: SQLParameter[T]](value :T)(implicit factory :SQLTermFactory[T, P]) :P =
		factory(value)

	implicit def PARAM_?[T, P <: SQLParameter[T]]
	                    (value :T)(implicit factory :SQLTermFactory[T, P]) :boundParameterSQL[T, P] =
		SQLExpression.boundParameterSQL(value)

}
