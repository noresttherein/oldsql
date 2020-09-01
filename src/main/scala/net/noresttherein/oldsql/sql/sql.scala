package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.sql.FromClause.DiscreteFrom



package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

//	type GroupBy[+F <: DiscreteFrom, T] = GroupByAll[F, MappingOf[T]#TypedProjection]
}