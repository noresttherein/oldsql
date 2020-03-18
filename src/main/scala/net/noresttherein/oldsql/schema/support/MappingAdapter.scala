package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.{Mapping, AbstractMapping}


/**
  * @author Marcin Mościcki
  */
trait MappingAdapter[M <: Mapping.Component[O, S], O, S, T] extends AbstractMapping[O, T] {
	protected val adaptee :M


	override def components :Unique[Component[_]] = Unique(adaptee)
	override def subcomponents :Unique[Component[_]] = adaptee.subcomponents

	override def columns :Unique[Component[_]] = adaptee.columns
	override def selectable :Unique[Component[_]] = adaptee.selectable
	override def queryable :Unique[Component[_]] = adaptee.queryable
	override def updatable :Unique[Component[_]] = adaptee.updatable
	override def autoUpdated :Unique[Component[_]] = adaptee.autoUpdated
	override def insertable :Unique[Component[_]] = adaptee.insertable
	override def autoInserted :Unique[Component[_]] = adaptee.autoInserted



	override def lift[X](component :Component[X]) :Component[X] =
		if (component eq adaptee) component
		else adaptee.lift(component)


	override def sqlName :Option[String] = adaptee.sqlName

	override def introString :String = adaptee.introString

	override def toString :String = adaptee.toString

}



object MappingAdapter {
	trait ShallowAdapter[M <: Mapping.Component[O, S], O, S, T] extends MappingAdapter[M, O, S, T] {}
}