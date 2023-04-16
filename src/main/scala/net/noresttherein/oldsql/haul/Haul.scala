package net.noresttherein.oldsql.haul

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.Relation






/**
  * @author Marcin Mościcki
  */ //todo: rename to Quest
trait Haul[T] {
	def apply[M[O] <: MappingAt[O], C[O] <: TypedMapping[S, O], S]
	         (table :Relation[M])(component :M[this.type] => C[this.type]) :Option[S]

	def apply[C <: MappingOf[S], S](table :Relation[MappingAt], component :C) :Option[S]
}
