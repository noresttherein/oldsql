package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.DelegateMapping



class PrefixedMapping[M <: RefinedMapping[S, O], S, O](val prefix :String, protected override val backer :M)
	extends EagerDeepProxy[S, O](backer) with DelegateMapping[M, S, O] //Adapted[M]
{
	protected override def adapt[T](component :Component[T]) :Component[T] = component.prefixed(prefix)

	protected override def adapt[T](column :Column[T]) :Column[T] = column.prefixed(prefix)

	override def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, S, O](prefix + "." + this.prefix, backer)

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, S, O](prefix + this.prefix, backer)



	override def toString :String = prefix + backer
}



object PrefixedMapping {

	def apply[M <: RefinedMapping[S, O], S, O](prefix :String, component :M) :Adapted[M] =
		new PrefixedMapping[M, S, O](prefix, component) with DelegateAdapter[M, S, O] {

			override def qualified(prefix :String) :Adapted[M] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + "." + this.prefix, backer)

			override def prefixed(prefix :String) :Adapted[M] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, backer)

		}


	def apply[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		new PrefixedMapping[MappingAdapter[M, S, O], S, O](prefix, adapter) with ComposedAdapter[M, S, S, O] {

			override def qualified(prefix :String) :MappingAdapter[M, S, O] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + "." + this.prefix, backer)

			override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, backer)

		}



	def qualified[M <: RefinedMapping[S, O], S, O](prefix :String, component :M) :Adapted[M] =
		if (prefix.length == 0) PrefixedMapping[M, S, O]("", component)
		else PrefixedMapping[M, S, O](prefix + ".", component)

	def qualified[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		if (prefix.length == 0) adapter
		else PrefixedMapping(prefix + ".", adapter)


}
