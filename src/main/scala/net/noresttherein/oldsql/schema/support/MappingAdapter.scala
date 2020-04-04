package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.{Component, ConcreteMapping}
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.AdaptedAs
import net.noresttherein.oldsql.schema.support.MappingNest.OpenNest



/** Skeletal base trait for mappings enclosing another mapping `egg`. It is the root of the hierarchy of various
  * proxies, adapters and mapped mappings.
  */
trait MappingNest[+M <: Mapping] extends Mapping { this :ConcreteMapping =>
	protected val egg :M

	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case proxy :MappingNest[_] => canEqual(proxy) && proxy.canEqual(this) && egg == proxy.egg
		case _ => false
	}

	override def hashCode :Int = egg.hashCode


	override def sqlName :Option[String] = egg.sqlName

	override def toString :String = egg.toString
}






object MappingNest {

	trait OpenNest[+M <: ConcreteMapping] extends MappingNest[M] { this :ConcreteMapping =>
		override val egg :M
	}

}






trait MappingAdapter[+M <: ConcreteMapping, O, S] extends GenericMapping[O, S] with OpenNest[M] {
	override val egg :M

//	override def map[X](there :S => X, back :X => S) :egg.type AdaptedAs X =
//		MappedMapping[egg.type, O, S, X](egg, map andThen there, back andThen unmap)
//
//	override def flatMap[X](there :S => Option[X], back :X => Option[S]) :egg.type AdaptedAs X =
//		MappedMapping.opt[egg.type, O, S, X](egg, map andThen there, back(_) map unmap)

}





object MappingAdapter {

	type Adapted[M <: ConcreteMapping] = MappingAdapter[M, M#Owner, M#Subject]
	type AdaptedAs[M <: ConcreteMapping, T] = MappingAdapter[M, M#Owner, T]
	type AdaptedFor[M <: ConcreteMapping, O] = MappingAdapter[M, O, M#Subject]


	/** Base trait for mappings which adapt another proxy from the same source `O`. Declares a single component,
	  * the embedded `egg` mapping, with all its components and subcomponents (and columns in particular)
	  * becoming directly the subcomponents of this mapping. As the adapted mapping `M` may have a different subject
	  * type to this mapping's subject, it serves in particular as a root for all mappings which map the subject of
	  * the nested mapping, but also simple decorators providing additional functionality or information, but leaving
	  * all mapping details of the `egg` intact.
	  * @tparam M the type of the adapted mapping, exposed to subclasses by the `egg` `val`.
	  * @tparam O a discriminator tag marking components from the same source.
	  * @tparam S the subject type of the adapted mapping `M`.
	  * @tparam T the subject type of this mapping.
	  */
	trait ShallowAdapter[+M <: Mapping.Component[O, S], O, S, T] extends GenericMapping[O, T] with MappingNest[M] {

		override def components :Unique[Component[_]] = Unique(egg)
		override def subcomponents :Unique[Component[_]] = egg.subcomponents //fixme: does not include egg

		override def columns :Unique[Component[_]] = egg.columns
		override def selectable :Unique[Component[_]] = egg.selectable
		override def queryable :Unique[Component[_]] = egg.queryable
		override def updatable :Unique[Component[_]] = egg.updatable
		override def autoUpdated :Unique[Component[_]] = egg.autoUpdated
		override def insertable :Unique[Component[_]] = egg.insertable
		override def autoInserted :Unique[Component[_]] = egg.autoInserted


		/** Refers to the adapted mapping `egg` to lift the passed component to its final representation, unless `component`
		  * is the `egg` itself, in which it is returned as-is.
		  */
		override def lift[X](component :Component[X]) :Component[X] =
			if (component eq egg) component
			else egg.lift(component)

	}

}







