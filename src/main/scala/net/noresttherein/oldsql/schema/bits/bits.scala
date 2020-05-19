package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.bits.ComponentProperty.GenericComponentProperty
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping


package object bits {

	/** A `MappingExtract` carrying the reflected form of its function as a `PropertyPath`.
	  * @see [[net.noresttherein.oldsql.schema.bits.ColumnProperty]]
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.model.PropertyPath]]
	  * @author Marcin Mościcki
	  */
	type ComponentProperty[-S, T, O] = GenericComponentProperty[RefinedMapping[T, O], S, T, O]

	type ColumnProperty[-S, T, O] = GenericComponentProperty[ColumnMapping[T, O], S, T, O]
}