package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.{CompositeNull, SQLNull}






//todo: All/Any expressions: 1 = All (select ...)
package object ast {

	def denullify[F <: RowProduct, S >: LocalScope <: GlobalScope, V](e :SQLExpression[F, S, V])
			:SQLExpression[F, S, V] =
		if (e == null) CompositeNull[V](1) else e

	def denullify[F <: RowProduct, S >: LocalScope <: GlobalScope, V](e :ColumnSQL[F, S, V])
			:ColumnSQL[F, S, V] =
		if (e == null) SQLNull[V]() else e

}
