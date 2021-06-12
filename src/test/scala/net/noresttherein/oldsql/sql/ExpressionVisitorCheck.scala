package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope, BaseExpressionVisitor}




/** Assert that BaseExpressionVisitor implements all methods in ExpressionVisitor, and thus all CaseXxx cases are covered by redirecting
  * to the case for their superclass.
  */
class ExpressionVisitorCheck
	extends ExpressionVisitor[RowProduct, ({ type T[-S >: LocalScope <: GlobalScope, V] = Unit })#T]
	   with BaseExpressionVisitor[RowProduct, ({ type T[-S >: LocalScope <: GlobalScope, V] = Unit })#T]