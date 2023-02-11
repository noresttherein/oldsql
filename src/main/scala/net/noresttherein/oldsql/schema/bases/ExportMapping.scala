package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnMappingTemplate, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.bases.ExportMapping.ExportMappingTemplate






/** Mixin trait for mappings which include ''only'' export components. Implements all `export` methods
  * by simply returning the argument. This is rarely appropriate unless the mapping controls the full
  * component tree.
  */
trait ExportMapping extends Mapping with ExportMappingTemplate[TypedMapping, TypedColumn] {
/*
	override def exportOrNot[T](component :Component[T]) :Component[T] = component
	override def exportOrNot[T](column :Column[T]) :Column[T] = column

	override def columns[T](component :Component[T])      :Unique[Column[_]] = component.columns
	override def selectable[T](component :Component[T])   :Unique[Column[_]] = component.selectable
	override def filterable[T](component :Component[T])   :Unique[Column[_]] = component.filterable
	override def insertable[T](component :Component[T])   :Unique[Column[_]] = component.insertable
	override def updatable[T](component :Component[T])    :Unique[Column[_]] = component.updatable
	override def autoInserted[T](component :Component[T]) :Unique[Column[_]] = component.autoInserted
	override def autoUpdated[T](component :Component[T])  :Unique[Column[_]] = component.autoUpdated

	override def selectedByDefault[T](component :Component[T]) :Unique[Column[_]] = component.selectedByDefault
	override def filteredByDefault[T](component :Component[T]) :Unique[Column[_]] = component.filteredByDefault
	override def insertedByDefault[T](component :Component[T]) :Unique[Column[_]] = component.insertedByDefault
	override def updatedByDefault[T](component :Component[T])  :Unique[Column[_]] = component.updatedByDefault

	override def columns[T](op :OperationView, component :Component[T]) :Unique[Column[_]] = component.columns(op)
	override def defaultColumns[T](op :OperationView, component :Component[T]) :Unique[Column[_]] =
		component.defaultColumns(op)

	override def columnsWith(buff :BuffType, component :Component[_]) :Unique[Column[_]] = component.columnsWith(buff)

	override def columnsWithout(buff :BuffType, component :Component[_]) :Unique[Column[_]] =
		component.columnsWithout(buff)
*/

	override def export[T](component :Component[T]) :Component[T] = component
	override def export[T](column :Column[T]) :Column[T] = column
}



object ExportMapping {
	trait ExportMappingTemplate[+Comp[S, O] <: TypedMapping[S, O] with MappingTemplate[Comp, Col],
	                            +Col[S, O] <: Comp[S, O] with TypedColumn[S, O] with ColumnMappingTemplate[Col]]
		extends Mapping with MappingTemplate[Comp, Col]
	{
		override def selectForm[T](component :Component[T]) :SQLReadForm[T]  = component.selectForm
		override def filterForm[T](component :Component[T]) :SQLWriteForm[T] = component.filterForm
		override def insertForm[T](component :Component[T]) :SQLWriteForm[T] = component.insertForm
		override def updateForm[T](component :Component[T]) :SQLWriteForm[T] = component.updateForm

		override def selectForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLReadForm[T] =
			component.selectForm(subcomponents)

		override def filterForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			component.filterForm(subcomponents)

		override def insertForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			component.insertForm(subcomponents)

		override def updateForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			component.updateForm

		protected override def newWriteForm[T](op :WriteOperationView, component :Component[T]) :SQLWriteForm[T] =
			component.writeForm(op)

		protected override def newWriteForm[T](op :WriteOperationView, component :Component[T],
		                                       subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			component.writeForm(op, subcomponents)

		override def columns[T](component :Component[T])      :Unique[Col[_, Origin]] = export(component).columns
		override def selectable[T](component :Component[T])   :Unique[Col[_, Origin]] = export(component).selectable
		override def filterable[T](component :Component[T])   :Unique[Col[_, Origin]] = export(component).filterable
		override def insertable[T](component :Component[T])   :Unique[Col[_, Origin]] = export(component).insertable
		override def updatable[T](component :Component[T])    :Unique[Col[_, Origin]] = export(component).updatable
		override def autoInserted[T](component :Component[T]) :Unique[Col[_, Origin]] = export(component).autoInserted
		override def autoUpdated[T](component :Component[T])  :Unique[Col[_, Origin]] = export(component).autoUpdated

		override def selectedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			export(component).selectedByDefault

		override def filteredByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			export(component).filteredByDefault

		override def insertedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			export(component).insertedByDefault

		override def updatedByDefault[T](component :Component[T])  :Unique[Col[_, Origin]] =
			export(component).updatedByDefault

		override def exportOrNot[T](component :Component[T]) :Component[T] = component
		override def exportOrNot[T](column :Column[T]) :Column[T] = column
	}
}
