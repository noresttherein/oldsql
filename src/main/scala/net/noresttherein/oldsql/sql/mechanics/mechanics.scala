package net.noresttherein.oldsql.sql






/** A package grouping helper classes which are a necessary part of the public API, but which are not expected
  * to be referred often or at all by the application code. This includes in particular many implicit witnesses
  * providing necessary functions in a mostly transparent manner, but also lower level classes such as
  * the intermediate SQL structure [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] built during
  * rendering of all SQL objects and features, and [[net.noresttherein.oldsql.sql.mechanics.SQLScribe SQLScribe]]
  * visitor providing a global framework for converting SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  *///consider: renaming the package to familiars or magic
package object mechanics {
	type sql_=>[X, Y] = SQLConversion[X, Y]
}
