package net.noresttherein.oldsql.sql




/** A package grouping helper classes which are a necessary part of the public API, but which are not expected
  * to be referred often or at all by the application code. This includes in particular many implicit witnesses
  * providing necessary functions in a mostly transparent manner, but also lower level classes such as
  * the intermediate SQL structure [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] built during
  * rendering of all SQL objects and features, and [[net.noresttherein.oldsql.sql.mechanics.SQLScribe SQLScribe]]
  * visitor providing a global framework for converting SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]].
  *///consider: renaming the package to familiars
package object mechanics {
//	/** A type used by [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] class hierarchy (and potentially others)
//	  * to signify that a given member type is illegal/does not exist/has no values. Any methods returning this type
//	  * will always throw an exception, methods accepting it, or other and any type figuring it in its signature is almost certainly also i*/
//	type Invalid
}