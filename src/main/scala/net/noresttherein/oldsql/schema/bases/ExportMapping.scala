package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping






/** Mixin trait for mappings which include ''only'' export components. Implements all `export` methods
  * by simply returning the argument. This is rarely appropriate unless the mapping controls the full
  * component tree.
  */
trait ExportMapping extends Mapping {
	override def export[T](component :Component[T]) :Component[T] = component
	override def export[T](column :Column[T]) :Column[T] = column
	override def exportOrNot[T](component :Component[T]) :Component[T] = component
	override def exportOrNot[T](column :Column[T]) :Column[T] = column

	override def selectable(component :Component[_]) :Unique[Column[_]] = component.selectable
	override def filterable(component :Component[_]) :Unique[Column[_]] = component.filterable
	override def insertable(component :Component[_]) :Unique[Column[_]] = component.insertable
	override def updatable(component :Component[_]) :Unique[Column[_]] = component.updatable
	override def autoInserted(component :Component[_]) :Unique[Column[_]] = component.autoInserted
	override def autoUpdated(component :Component[_]) :Unique[Column[_]] = component.autoUpdated

	override def selectedByDefault(component :Component[_]) :Unique[Column[_]] = component.selectedByDefault
	override def filteredByDefault(component :Component[_]) :Unique[Column[_]] = component.filteredByDefault
	override def insertedByDefault(component :Component[_]) :Unique[Column[_]] = component.insertedByDefault
	override def updatedByDefault(component :Component[_]) :Unique[Column[_]] = component.updatedByDefault

	//it follows form the default implementation due to export being identity
//	override def selectForm[T](component :Component[T]) :SQLReadForm[T] = component.selectForm
//	override def filterForm[T](component :Component[T]) :SQLWriteForm[T] = component.filterForm
//	override def insertForm[T](component :Component[T]) :SQLWriteForm[T] = component.insertForm
//	override def updateForm[T](component :Component[T]) :SQLWriteForm[T] = component.updateForm
}
