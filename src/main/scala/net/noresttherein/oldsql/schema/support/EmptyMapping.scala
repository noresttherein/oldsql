package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.schema.{ColumnMappingExtract, SQLForm, SQLReadForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OfFreeOrigin


/**
  * @author Marcin Mościcki
  */
trait EmptyMapping[S, O] extends TypedMapping[S, O] {

	override def assemble(pieces :Pieces) :Option[S] = pieces.preset(this)


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
	override def queryable :Unique[Column[_]] = Unique.empty
	override def updatable :Unique[Column[_]] = Unique.empty
	override def autoUpdated :Unique[Column[_]] = Unique.empty
	override def insertable :Unique[Column[_]] = Unique.empty
	override def autoInserted :Unique[Column[_]] = Unique.empty


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = SQLForm[Nothing]
	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	
	override def selectForm :SQLReadForm[S] = SQLForm[Nothing]
	override def queryForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def updateForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def insertForm :SQLWriteForm[S] = SQLWriteForm.empty


}






class ConstantMapping[S, O](subject :S) extends EmptyMapping[S, O] with OfFreeOrigin[O] {
	private[this] val result = Some(subject)

	override def assemble(values :Pieces) :Option[S] = result

	override def optionally(values :Pieces) :Option[S] = result

	override def apply(values :Pieces) :S = subject

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		SQLReadForm.const(subject, (0 /: components) { _ + _.columns.size })

	override val selectForm :SQLReadForm[S] = SQLReadForm.const(subject)

	override def toString :String = "Const(" + subject + ")"
}






class FormMapping[S, O](implicit val form :SQLForm[S]) extends EmptyMapping[S, O] {

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.isEmpty) SQLReadForm.nulls(form.nulls)
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] = insertForm(components)
	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = insertForm(components)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.isEmpty) SQLWriteForm.empty
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)



	override def selectForm :SQLReadForm[S] = form
	override def insertForm :SQLWriteForm[S] = form
	override def queryForm :SQLWriteForm[S] = form
	override def updateForm :SQLWriteForm[S] = form

}

