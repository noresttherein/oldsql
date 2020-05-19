package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms



class PrefixedMapping[M <: RefinedMapping[S, O], S, O](val prefix :String, override val egg :M)
	extends EagerDeepProxy[M, S, O](egg) with MappingAdapter[M, S, O] //Adapted[M]
{
	protected override def adapt[T](component :Component[T]) :Component[T] = component.prefixed(prefix)

	protected override def adapt[T](column :Column[T]) :Column[T] = column.prefixed(prefix)

	override def qualified(prefix :String) :Adapted[M] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, S, O](prefix + "." + this.prefix, egg)

	override def prefixed(prefix :String) :Adapted[M] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, S, O](prefix + this.prefix, egg)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[PrefixedMapping[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case prefixed :PrefixedMapping[_, _, _] =>
			(prefixed eq this) || prefixed.canEqual(this) && egg == prefixed.egg && prefix == prefixed.prefix
		case _ => false
	}

	override def hashCode :Int = egg.hashCode * 31 + prefix.hashCode


	override def toString :String = prefix + egg
}



object PrefixedMapping {
	def apply[S, O](prefix :String, component :RefinedMapping[S, O]) :Adapted[component.type] =
		new PrefixedMapping[component.type, S, O](prefix, component)

	def qualified[S, O](prefix :String, component :RefinedMapping[S, O]) :Adapted[component.type] =
		if (prefix.length == 0)
			new PrefixedMapping[component.type, S, O]("", component)
		else
            new PrefixedMapping[component.type, S, O](prefix + ".", component)



	def generic[X <: Mapping, M <: RefinedMapping[S, O], S, O]
	           (prefix :String, component :X)(implicit hint :Conforms[X, M, RefinedMapping[S, O]]) :Adapted[M] =
		new PrefixedMapping[M, S, O](prefix, component)

}
