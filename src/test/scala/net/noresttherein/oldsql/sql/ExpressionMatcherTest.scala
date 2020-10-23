package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, LocalScope, SelectiveMatcher}




/** Assert that SelectiveMatcher implements all methods in ExpressionMatcher, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
object ExpressionMatcherTest
	extends ExpressionMatcher[FromClause, ({ type T[-S >: LocalScope <: GlobalScope, V] = () })#T]
	   with SelectiveMatcher[FromClause, ({ type T[-S >: LocalScope <: GlobalScope, V] = () })#T]