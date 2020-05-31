package net.noresttherein.oldsql



package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

}