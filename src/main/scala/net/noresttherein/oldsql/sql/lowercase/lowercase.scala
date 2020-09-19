package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{boundParameterSQL, GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.{SQLParameter, SQLTermFactory}

/**
  * @author Marcin Mościcki
  */
package object lowercase {

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


	def param[T, P <: SQLParameter[T]](value :T)(implicit factory :SQLTermFactory[T, P]) :P =
		factory(value)

	implicit def param_?[T, P <: SQLParameter[T]]
	                    (value :T)(implicit factory :SQLTermFactory[T, P]) :boundParameterSQL[T, P] =
		SQLExpression.boundParameterSQL(value)

}
