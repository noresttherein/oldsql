package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.AnyComponent


package object sql {
	type OuterJoin[+L <: FromClause, R[O] <: AnyComponent[O]] = LeftJoin[L, R]

	final val OuterJoin = LeftJoin
}