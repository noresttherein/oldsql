package net.noresttherein.oldsql.sql

//import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionMatcher, SelectiveMatcher}




/** Assert that SelectiveMatcher implements all methods in ExpressionMatcher, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
//object MatchSuperExhaustsFormulaMatcher extends ExpressionMatcher[FromClause, Option] with SelectiveMatcher[FromClause, Option]