package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import MappingAdapter.{Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.{EagerDeepProxy, ShallowProxy}






/**
  * @author Marcin Mościcki
  */
class BuffedMapping[+M <: RefinedMapping[S, O], S, O](protected override val backer :M, override val buffs :Seq[Buff[S]])
	extends EagerDeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	override protected def adapt[T](component :backer.Component[T]) :Component[T] =
		new BuffedMapping[Component[T], T, O](component, schema.cascadeBuffs(this)(backer(component)))

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		column.withBuffs(schema.cascadeBuffs(this)(backer(column)))

}



object BuffedMapping {

	def cascading[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buff[S]*) :Adapted[M] =
		new BuffedMapping[M, S, O](mapping, buffs) with DelegateAdapter[M, S, O]

	def cascading[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buff[S]*) :MappingAdapter[M, S, O] =
		new BuffedMapping[MappingAdapter[M, S, O], S, O](mapping, buffs) with ComposedAdapter[M, S, S, O]


	def nonCascading[M <: RefinedMapping[S, O], S, O](mapping :M, buffs :Buff[S]*) :Adapted[M] =
		new NonCascadingBuffedMapping[M, S, O](mapping, buffs) with DelegateAdapter[M, S, O]

	def nonCascading[M <: MappingAt[O], S, O](mapping :MappingAdapter[M, S, O], buffs :Buff[S]*) :MappingAdapter[M, S, O] =
		new NonCascadingBuffedMapping[mapping.type, S, O](mapping, buffs) with ComposedAdapter[M, S, S, O]



	private class NonCascadingBuffedMapping[+M <: RefinedMapping[S, O], S, O]
	                                       (protected override val backer :M, override val buffs :Seq[Buff[S]])
		extends ShallowProxy[S, O] with DelegateMapping[M, S, O]

}
