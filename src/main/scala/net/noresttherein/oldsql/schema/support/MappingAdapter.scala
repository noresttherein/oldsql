package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.{Mapping, AbstractMapping}


trait MappingNest[+M <: Mapping] extends Mapping {
	protected val egg :M

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingNest[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case proxy :MappingNest[_] => canEqual(proxy) && proxy.canEqual(this) && egg == proxy.egg
		case _ => false
	}

	override def hashCode :Int = egg.hashCode


	override def sqlName :Option[String] = egg.sqlName

	override def toString :String = egg.toString
}



/**
  */
trait MappingAdapter[+M <: Mapping.Component[O, S], O, S, T] extends AbstractMapping[O, T] with MappingNest[M] {

	override def components :Unique[Component[_]] = Unique(egg)
	override def subcomponents :Unique[Component[_]] = egg.subcomponents

	override def columns :Unique[Component[_]] = egg.columns
	override def selectable :Unique[Component[_]] = egg.selectable
	override def queryable :Unique[Component[_]] = egg.queryable
	override def updatable :Unique[Component[_]] = egg.updatable
	override def autoUpdated :Unique[Component[_]] = egg.autoUpdated
	override def insertable :Unique[Component[_]] = egg.insertable
	override def autoInserted :Unique[Component[_]] = egg.autoInserted



	override def lift[X](component :Component[X]) :Component[X] =
		if (component eq egg) component
		else egg.lift(component)


}



