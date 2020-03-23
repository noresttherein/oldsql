package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.{Component, SingletonComponent}
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy



class PrefixedMapping[M <: SingletonComponent[O, S], O, S](val prefix :String, mapping :M)
	extends EagerDeepProxy[M, O, S](mapping)
{
	override protected def adapt[T](component :Component[T]) :Component[T] = component.prefixed(prefix)


	override def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, O, S](prefix + "." + this.prefix, egg)

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else new PrefixedMapping[M, O, S](prefix + this.prefix, egg)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[PrefixedMapping[_, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case prefixed :PrefixedMapping[_, _, _] =>
			(prefixed eq this) || prefixed.canEqual(this) && egg == prefixed.egg && prefix == prefixed.prefix
		case _ => false
	}

	override def hashCode :Int = mapping.hashCode * 31 + prefix.hashCode


	override def toString :String = prefix + egg
}



object PrefixedMapping {
	def apply[O, S](prefix :String, component :Component[O, S]) :Component[O, S] =
		if (prefix.length == 0) component
		else new PrefixedMapping[component.type, O, S](prefix, component)

	def qualified[O, S](prefix :String, component :Component[O, S]) :Component[O, S] =
		if (prefix.length == 0) component
		else new PrefixedMapping[component.type, O, S](prefix + ".", component)
}
