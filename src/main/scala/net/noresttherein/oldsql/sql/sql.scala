package net.noresttherein.oldsql

import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}



package object sql {

	type SQLBoolean[-F <: FromClause, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, Boolean]
	type LocalBoolean[-F <: FromClause] = ColumnSQL[F, LocalScope, Boolean]
	type GlobalBoolean[-F <: FromClause] = ColumnSQL[F, GlobalScope, Boolean]

}