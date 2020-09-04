package net.noresttherein.oldsql




package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

//	type GroupBy[+F <: FromSome, T] = GroupByAll[F, MappingOf[T]#TypedProjection]
}