package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome



package object sql {

	type SQLBoolean[-F <: FromClause] = ColumnSQL[F, Boolean]

//	type GroupBy[+F <: FromSome, T] = GroupByAll[F, LabeledMapping.[T]#TypedProjection]
}