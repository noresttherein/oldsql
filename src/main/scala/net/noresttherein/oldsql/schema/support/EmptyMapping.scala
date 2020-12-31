package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.{SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.bases.BaseMapping






/** A `Mapping` with no columns or other components. It follows that its write and read forms are empty
  * implementations, reading and writing nothing. It can still produce a value from the `assemble` method
  * if one has been preset for it in the `ComponentValues`.
  */
trait EmptyMapping[S, O] extends BaseMapping[S, O] {

	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit = ()

	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] = ComponentValues.empty

	override def filterValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty
	override def updateValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty
	override def insertValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty

	override def assemble(pieces :Pieces) :Option[S] = None
	override def optionally(pieces :Pieces) :Option[S] = pieces.preset(this)



	override def export[T](component :Component[T]) :Component[T] =
		if (component eq this) component
		else throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def export[T](column :Column[T]) :Column[T] =
		if (column eq this) column
		else throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping :$this.")

	override def exportOrNot[T](component :Component[T]) :Component[T] = component
	override def exportOrNot[T](column :Column[T]) :Column[T] = column

	override def apply[T](component :Component[T]) :Extract[T] =
		throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def apply[T](column :Column[T]) :ColumnExtract[T] =
		throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping: $this.")
	

	override def extracts :NaturalMap[Component, Extract] = NaturalMap.empty
	override def columnExtracts :NaturalMap[Column, ColumnExtract] = NaturalMap.empty

	override def components :Unique[Component[_]] = Unique.empty
	override def subcomponents :Unique[Component[_]] = Unique.empty

	override def columns :Unique[Column[_]] = Unique.empty
	override def selectable :Unique[Column[_]] = Unique.empty
	override def filterable :Unique[Column[_]] = Unique.empty
	override def updatable :Unique[Column[_]] = Unique.empty
	override def autoUpdated :Unique[Column[_]] = Unique.empty
	override def insertable :Unique[Column[_]] = Unique.empty
	override def autoInserted :Unique[Column[_]] = Unique.empty
	override def selectedByDefault :Unique[Column[_]] = Unique.empty
	override def filteredByDefault :Unique[Column[_]] = Unique.empty
	override def updatedByDefault :Unique[Column[_]] = Unique.empty
	override def insertedByDefault :Unique[Column[_]] = Unique.empty


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = selectForm
//	override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
//	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
//	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		SQLWriteForm.empty
	
	override def selectForm :SQLReadForm[S] = SQLReadForm.empty
//	override def filterForm :SQLWriteForm[S] = SQLWriteForm.empty
//	override def updateForm :SQLWriteForm[S] = SQLWriteForm.empty
//	override def insertForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = SQLWriteForm.empty

}

