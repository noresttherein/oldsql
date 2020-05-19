package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms


/**
  * @author Marcin Mościcki
  */
class BuffedMapping[+M <: RefinedMapping[S, O], S, O](override val egg :M, override val buffs :Seq[Buff[S]])
	extends EagerDeepProxy[M, S, O](egg) with MappingAdapter[M, S, O]
{
	override protected def adapt[T](component :egg.Component[T]) :Component[T] =
		new BuffedMapping(component, schema.cascadeBuffs(this)(egg(component)))

	protected override def adapt[T](column :egg.Column[T]) :Column[T] =
		column.withBuffs(schema.cascadeBuffs(this)(egg(column)))

//	override def toString :String = buffs.mkString(mapping.toString + "(", ",", ")")
}



object BuffedMapping {

	def apply[X <: Mapping, M <: RefinedMapping[S, O], S, O](mapping :X, buffs :Buff[S]*)
	                                                        (implicit infer :Conforms[X, M, RefinedMapping[S, O]])
			:MappingAdapter[M, S, O] =
		new BuffedMapping[M, S, O](mapping, buffs)

}
