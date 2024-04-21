package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.{Mapping, SQLForm}




/** A mapping with `Nothing` as its [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
  * A useful base classes for reusable mappings which are simply column/subcomponent groups for a larger object,
  * which do not map to a single property. Instead, the owning mapping uses this mapping's components directly.
  * @author Marcin Mościcki
  *
  */ //todo: NothingFrame and SimpleNothingMapping, requires extracting most of the methods out.`
trait NothingMapping[O] extends Mapping {
	override type Subject = Nothing
	override type Origin  = O

	override def assemble(pieces :Pieces) :Opt[Nothing] = Lack

	override def apply(pieces :Pieces) :Nothing =
		throw new UnsupportedOperationException("Mapping " + this + " does not correspond to a value.")

	override def optionally(pieces :Pieces) :Opt[Nothing] = Lack

	override def nullValue :NullValue[Nothing] = NullValue.Unsupported
}
