package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.MappingFrom


package object sql {
	type OuterJoin[+L <: FromClause, R <: Mapping] = LeftJoin[L, R]

	final val OuterJoin = LeftJoin
}