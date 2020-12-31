package net.noresttherein.oldsql.schema.bases

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
}
