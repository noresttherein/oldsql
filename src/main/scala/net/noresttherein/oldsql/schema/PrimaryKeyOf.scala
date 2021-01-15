package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}






/** A type class for [[net.noresttherein.oldsql.schema.Mapping Mapping]] types providing their primary key.
  * This is not a definition of the key per se - the component must be defined in the mapping (and, optionally,
  * declared as a foreign key) - it is only a selector function which returns the ke component from mapping `M`.
  */
trait PrimaryKeyOf[-M[O] <: MappingAt[O]] {
	type Key
	type PKMapping[O] <: RefinedMapping[Key, O]
	def apply[O](entity :M[O]) :PKMapping[O]
}



object PrimaryKeyOf {
	type PrimaryKeyColumnOf[M[O] <: MappingAt[O]] = PrimaryKeyOf[M] { type PKMapping[O] <: ColumnMapping[Key, O] }
}

