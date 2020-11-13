package net.noresttherein.oldsql.schema.bases

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.schema.bits.{ColumnProperty, ComponentProperty}






/**
  * @author Marcin Mościcki
  */
trait ReflectedMapping[S, O] extends MappingFrame[S, O] { composite =>
	protected implicit val subjectType :TypeTag[S]


	override protected def extractFor[T](component :FrameComponent[T]) :ComponentProperty[S, T, O] =
		ComponentProperty(component)(component.extractor)

	protected override def extractFor[T](column :FrameColumn[T]) :ColumnProperty[S, T, O] =
		ComponentProperty(column)(column.extractor)




	override def apply[T](component :Component[T]) :ComponentProperty[S, T, O] =
		super.apply(component).asInstanceOf[ComponentProperty[S, T, O]]

	override def apply[T](column :Column[T]) :ColumnProperty[S, T, O] =
		super.apply(column).asInstanceOf[ColumnProperty[S, T, O]]
	

}
