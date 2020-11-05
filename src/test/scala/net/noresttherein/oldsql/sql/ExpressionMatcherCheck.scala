package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, GlobalScope, LocalScope, SelectiveMatcher}




/** Assert that SelectiveMatcher implements all methods in ExpressionMatcher, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
class ExpressionMatcherCheck
	extends ExpressionMatcher[RowProduct, ({ type T[-S >: LocalScope <: GlobalScope, V] = () })#T]
	   with SelectiveMatcher[RowProduct, ({ type T[-S >: LocalScope <: GlobalScope, V] = () })#T]