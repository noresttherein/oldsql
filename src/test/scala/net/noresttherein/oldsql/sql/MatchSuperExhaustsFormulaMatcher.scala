package net.noresttherein.oldsql.sql

//import net.noresttherein.oldsql.sql.SQLFormula.{FormulaMatcher, MatchSuper}




/** Assert that MatchSuper implements all methods in FormulaMatcher, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
//object MatchSuperExhaustsFormulaMatcher extends FormulaMatcher[FromClause, Option] with MatchSuper[FromClause, Option]