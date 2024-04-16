package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AbstractDelegateAdapter, Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy






private[oldsql] class RenamedMapping[S, O](mapping :TypedMapping[S, O], rename :String => String)
	extends DeepProxy[S, O](mapping)
{
	protected override def adapt[T](component :backer.Component[T]) :Component[T] = component.renamed(rename)
	protected override def adapt[T](column :backer.Column[T]) :Column[T] = column.renamed(rename)

	//Renaming a renamed mapping creates a proxy to the original mapping, not to this one.
	// This is somewhat problematic because the returned mapping will not recognize components of this mapping.
	override def renamed(naming :String => String) :Component[S] =
		new RenamedMapping[S, O](mapping, rename andThen naming)

	override def toString :String = mapping.toString + ".renamed"
}



object RenamedMapping {

	def apply[M <: TypedMapping[S, O], S, O](mapping :M, rename :String => String) :Adapted[M] =
		new RenamedMapping[S, O](mapping, rename)
			with MappingDecorator[M, S, O] with AbstractDelegateAdapter[M, S, O]
		{
			override val body = mapping
			override def renamed(naming :String => String) :Adapted[M] =
				RenamedMapping[M, S, O](body, rename andThen naming)
		}

	def apply[M <: MappingAt[O], S, O](adapter :MappingAdapter[M, S, O], rename :String => String)
			:MappingAdapter[M, S, O] =
		new RenamedMapping[S, O](adapter, rename) with AbstractDelegateAdapter[M, S, O] {
			override val body = adapter.body
			override def renamed(naming :String => String) :MappingAdapter[M, S, O] =
				RenamedMapping[M, S, O](adapter, rename andThen naming)
		}
}






private[oldsql] class PrefixedMapping[S, O](protected val prefix :String, mapping :TypedMapping[S, O])
	extends DeepProxy[S, O](mapping)
{
	protected override def adapt[T](component :Component[T]) :Component[T] = component.prefixed(prefix)
	protected override def adapt[T](column :Column[T]) :Column[T] = column.prefixed(prefix)

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[S, O](prefix + this.prefix, mapping)

	override def renamed(naming :String => String) :Component[S] =
		new RenamedMapping[S, O](mapping, name => naming(prefix + name))

	override def toString :String = prefix + mapping
}



object PrefixedMapping {

	def apply[M <: TypedMapping[S, O], S, O](prefix :String, mapping :M) :Adapted[M] =
		new PrefixedMapping[S, O](prefix, mapping)
			with MappingDecorator[M, S, O] with AbstractDelegateAdapter[M, S, O]
		{
			override val body = mapping
			//the problem with these methods is that the results do not recognize the components of this adapter
			//might be better to just leave it at defaults, which short-circuit assemble calls
			override def prefixed(prefix :String) :Adapted[M] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, body)

			override def renamed(naming :String => String) :Adapted[M] =
				RenamedMapping[M, S, O](body, name => naming(this.prefix + name))
		}


	def apply[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		new PrefixedMapping[S, O](prefix, adapter) with AbstractDelegateAdapter[M, S, O] {
			override val body = adapter.body

			override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, adapter)

			override def renamed(naming :String => String) :MappingAdapter[M, S, O] =
				RenamedMapping[M, S, O](adapter, name => naming(this.prefix + name))
		}



	def qualified[M <: TypedMapping[S, O], S, O](prefix :String, component :M) :Adapted[M] =
		if (prefix.length == 0) PrefixedMapping[M, S, O]("", component)
		else PrefixedMapping[M, S, O](prefix + ".", component)

	def qualified[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		if (prefix.length == 0) adapter
		else PrefixedMapping(prefix + ".", adapter)
}
