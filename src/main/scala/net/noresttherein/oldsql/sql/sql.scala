package net.noresttherein.oldsql




package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

//	type GroupBy[+F <: DiscreteFrom, T] = GroupByAll[F, MappingOf[T]#TypedProjection]
}