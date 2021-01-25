package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.EmptyProxy






/** An empty mapping proxy. This class does not declare any components or extracts, but assumes that `backer`
  * and its subcomponents are declared elsewhere as components of the same larger mapping (i.e., they come from
  * the same table mapping). It is used to include a component as a virtual component of another component.
  *
  * @author Marcin Mościcki
  */
class SymlinkMapping[M <: RefinedMapping[S, O], S, O](protected override val backer :M)
	extends EmptyProxy[S, O]



object SymlinkMapping {
	def apply[M <: RefinedMapping[S, O], S, O](component :M) :Adapted[M] =
		new SymlinkMapping[M, S, O](component) with DelegateAdapter[M, S, O]

	def adapter[M <: RefinedMapping[S, O], S, O](adapter :MappingAdapter[M, S, O]) :Adapted[M] =
		new SymlinkMapping[MappingAdapter[M, S, O], S, O](adapter) with ComposedAdapter[M, S, S, O]
}
