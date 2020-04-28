package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.PropertyReflectionException
import net.noresttherein.oldsql.morsels.Extractor.{ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ComponentExtractor
import net.noresttherein.oldsql.schema.ComponentExtractor.{ColumnExtractor, ComponentExtractorTemplate, ConstantColumn, ConstantComponent, EmptyColumn, EmptyComponent, IdentityColumn, IdentityComponent, OptionalColumn, RequisiteColumn, RequisiteComponent, RequisiteComponentTemplate}
import net.noresttherein.oldsql.schema.bits.ComponentProperty
import net.noresttherein.oldsql.schema.bits.ComponentProperty.ColumnProperty

import scala.reflect.runtime.universe.TypeTag






/**
  * @author Marcin Mościcki
  */
trait ReflectedMapping[S, O] extends MappingFrame[S, O] { composite =>
	protected implicit val subjectType :TypeTag[S]


	override protected def selectorFor[T](component :FrameComponent[T]) :ComponentProperty[S, T, O] =
		ComponentProperty(component)(component.extractor)

	protected override def selectorFor[T](column :FrameColumn[T]) :ColumnProperty[S, T, O] =
		ComponentProperty(column)(column.extractor)




	override def apply[T](component :Component[T]) :ComponentProperty[S, T, O] =
		super.apply(component).asInstanceOf[ComponentProperty[S, T, O]]

	override def apply[T](column :Column[T]) :ColumnProperty[S, T, O] =
		super.apply(column).asInstanceOf[ColumnProperty[S, T, O]]
	

}
