package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth


/**
  * @author Marcin Mościcki
  */
class BuffedMapping[+M <: Component[O, S], O, S](override val egg :M, override val buffs :Seq[Buff[S]])
	extends EagerDeepProxy[M, O, S](egg) with MappingAdapter[M, O, S]
{
	override protected def adapt[T](component :egg.Component[T]) :Component[T] =
		new BuffedMapping(component, schema.mapBuffs(this)(egg(component)))

	protected override def adaptColumn[T](component :egg.Component[T]) :Component[T] = component match {
		case column :ColumnMapping[O, T] =>
			column.withBuffs(schema.mapBuffs(this)(egg(component)))
		case _ =>
			adapt(component)
	}

//	override def toString :String = buffs.mkString(mapping.toString + "(", ",", ")")
}



object BuffedMapping {
	//todo: withBuffs method on Mapping
	def apply[X <: Mapping, M <: Component[O, S], O, S](mapping :X, buffs :Buff[S]*)(implicit infer :IsBoth[X, M, Component[O, S]]) :MappingAdapter[M, O, S] =
		new BuffedMapping[M, O, S](mapping, buffs)
}
