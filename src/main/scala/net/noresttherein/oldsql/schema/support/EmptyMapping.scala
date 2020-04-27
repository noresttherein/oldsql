package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{GenericMapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ComponentExtractor
import net.noresttherein.oldsql.schema.ComponentExtractor.ColumnExtractor


/**
  * @author Marcin Mościcki
  */
trait EmptyMapping[S, O] extends GenericMapping[S, O] {

	override def apply[T](component :Component[T]) :Selector[T] =
		throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def apply[T](column :Column[T]) :ColumnExtractor[S, T, O] =
		throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping: $this.")
	
	
	override def components :Unique[Component[_]] = Unique.empty
	override def subcomponents :Unique[Component[_]] = Unique.empty

	override def columns :Unique[Column[_]] = Unique.empty
	override def selectable :Unique[Column[_]] = Unique.empty
	override def queryable :Unique[Column[_]] = Unique.empty
	override def updatable :Unique[Column[_]] = Unique.empty
	override def autoUpdated :Unique[Column[_]] = Unique.empty
	override def insertable :Unique[Column[_]] = Unique.empty
	override def autoInserted :Unique[Column[_]] = Unique.empty


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = SQLForm.NothingForm
	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = SQLWriteForm.empty
	
	override def selectForm :SQLReadForm[S] = SQLForm.NothingForm
	override def queryForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def updateForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def insertForm :SQLWriteForm[S] = SQLWriteForm.empty


	override def assemble(values :Pieces) :Option[S] = None
}






class ConstantMapping[S, O](subject :S) extends EmptyMapping[S, O] {
	private[this] val result = Some(subject)

	override def assemble(values :Pieces) :Option[S] = result

	override def optionally(values :Pieces) :Option[S] = result

	override def apply(values :Pieces) :S = subject

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



	override def selectForm :SQLForm[S] = form
	override def insertForm :SQLForm[S] = form
	override def queryForm :SQLForm[S] = form
	override def updateForm :SQLForm[S] = form

}

