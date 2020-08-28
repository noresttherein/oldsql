package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.sql.FromClause.UngroupedFrom



package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

//	type GroupBy[+F <: UngroupedFrom, T] = GroupByAll[F, MappingOf[T]#TypedProjection]
}