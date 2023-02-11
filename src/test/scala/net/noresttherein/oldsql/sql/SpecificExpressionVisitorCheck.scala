package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, BaseAnyExpressionVisitor, BaseSpecificExpressionVisitor, SpecificExpressionVisitor, Single, Grouped}




class SpecificExpressionVisitorCheck
	extends SpecificExpressionVisitor[RowProduct, Single, Boolean, Unit]
	   with BaseSpecificExpressionVisitor[RowProduct, Single, Boolean, Unit]

/** Assert that AnyBaseExpressionVisitor implements all methods in AnyExpressionVisitor, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
class AnyExpressionVisitorCheck
	extends AnyExpressionVisitor[RowProduct, ({ type T[-S >: Grouped <: Single, V] = Unit })#T]
	   with BaseAnyExpressionVisitor[RowProduct, ({ type T[-S >: Grouped <: Single, V] = Unit })#T]

