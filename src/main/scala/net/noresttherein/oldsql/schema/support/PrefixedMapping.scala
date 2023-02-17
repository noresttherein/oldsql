package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy






private[oldsql] class RenamedMapping[+M <: TypedMapping[S, O], S, O]
                                    (protected override val backer :M, rename :String => String)
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	protected override def adapt[T](component :backer.Component[T]) :Component[T] = component.renamed(rename)
	protected override def adapt[T](column :backer.Column[T]) :Column[T] = column.renamed(rename)

	override def renamed(naming :String => String) :Component[S] =
		new RenamedMapping[M, S, O](backer, rename andThen naming)

	override def toString :String = backer.toString + ".renamed"
}



object RenamedMapping {

	def apply[M <: TypedMapping[S, O], S, O](component :M, rename :String => String) :Adapted[M] =
		new RenamedMapping[M, S, O](component, rename) with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O] {
			override def renamed(naming :String => String) :Adapted[M] =
				RenamedMapping[M, S, O](backer, rename andThen naming)
		}

	def apply[M <: MappingAt[O], S, O](adapter :MappingAdapter[M, S, O], rename :String => String)
			:MappingAdapter[M, S, O] =
		new RenamedMapping[MappingAdapter[M, S, O], S, O](adapter, rename) with ComposedAdapter[M, S, S, O] {
			override def renamed(naming :String => String) :MappingAdapter[M, S, O] =
				RenamedMapping[M, S, O](backer, rename andThen naming)
		}
}






private[oldsql] class PrefixedMapping[+M <: TypedMapping[S, O], S, O]
                                     (protected val prefix :String, protected override val backer :M)
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	protected override def adapt[T](component :Component[T]) :Component[T] = component.prefixed(prefix)
	protected override def adapt[T](column :Column[T]) :Column[T] = column.prefixed(prefix)

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, S, O](prefix + this.prefix, backer)

	override def renamed(naming :String => String) :Component[S] =
		new RenamedMapping[M, S, O](backer, name => naming(prefix + name))


	override def toString :String = prefix + backer
}



object PrefixedMapping {

	def apply[M <: TypedMapping[S, O], S, O](prefix :String, component :M) :Adapted[M] =
		new PrefixedMapping[M, S, O](prefix, component) with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O] {
			//the problem with these methods is that the results do not recognize the components of this adapter
			//might be better to just leave it at defaults, which short-circuit assemble calls
			override def prefixed(prefix :String) :Adapted[M] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, backer)

			override def renamed(naming :String => String) :Adapted[M] =
				RenamedMapping[M, S, O](backer, name => naming(prefix + name))
		}


	def apply[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		new PrefixedMapping[MappingAdapter[M, S, O], S, O](prefix, adapter) with ComposedAdapter[M, S, S, O] {
			override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
				if (prefix.length == 0) this
				else PrefixedMapping[M, S, O](prefix + this.prefix, backer)

			override def renamed(naming :String => String) :MappingAdapter[M, S, O] =
				RenamedMapping[M, S, O](backer, name => naming(prefix + name))
		}



	def qualified[M <: TypedMapping[S, O], S, O](prefix :String, component :M) :Adapted[M] =
		if (prefix.length == 0) PrefixedMapping[M, S, O]("", component)
		else PrefixedMapping[M, S, O](prefix + ".", component)

	def qualified[M <: MappingAt[O], S, O](prefix :String, adapter :MappingAdapter[M, S, O]) :MappingAdapter[M, S, O] =
		if (prefix.length == 0) adapter
		else PrefixedMapping(prefix + ".", adapter)


}
