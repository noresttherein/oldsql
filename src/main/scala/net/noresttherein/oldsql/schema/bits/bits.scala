package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.bits.ComponentProperty.SpecificComponentProperty




/** Concrete implementations of [[net.noresttherein.oldsql.schema.Mapping Mapping]] and some related classes,
  * ready for use by applications.
  */
package object bits {

	/** A `MappingExtract` carrying the reflected form of its function as a `PropertyPath`.
	  * @see [[net.noresttherein.oldsql.schema.bits.ColumnProperty]]
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.model.PropertyPath]]
	  * @author Marcin Mościcki
	  */
	type ComponentProperty[-S, T, O] = SpecificComponentProperty[TypedMapping[T, O], S, T, O]

	type ColumnProperty[-S, T, O] = SpecificComponentProperty[TypedColumn[T, O], S, T, O]

//	type Export[M <: Mapping]
}