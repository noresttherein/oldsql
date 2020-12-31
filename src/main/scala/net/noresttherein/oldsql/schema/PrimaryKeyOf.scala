package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}






/**
  * @author Marcin Mościcki
  */
trait PrimaryKeyOf[M[O] <: MappingAt[O]] {
	type Key
	type PKMapping[O] <: RefinedMapping[Key, O]
	def apply[O](entity :M[O]) :PKMapping[O]
}



object PrimaryKeyOf {
	type PrimaryKeyColumnOf[M[O] <: MappingAt[O]] = PrimaryKeyOf[M] { type PKMapping[O] <: ColumnMapping[Key, O] }
}

