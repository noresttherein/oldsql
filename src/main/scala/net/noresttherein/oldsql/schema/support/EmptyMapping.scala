package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.ComponentExtractor


/**
  * @author Marcin Mościcki
  */
trait EmptyMapping[O, S] extends GenericMapping[O, S] {

	override def apply[T](component :Component[T]) :Selector[T] =
		throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def components :Unique[Component[_]] = Unique.empty
	override def subcomponents :Unique[Component[_]] = Unique.empty

	override def columns :Unique[Component[_]] = Unique.empty
	override def selectable :Unique[Component[_]] = Unique.empty
	override def queryable :Unique[Component[_]] = Unique.empty
	override def updatable :Unique[Component[_]] = Unique.empty
	override def autoUpdated :Unique[Component[_]] = Unique.empty
	override def insertable :Unique[Component[_]] = Unique.empty
	override def autoInserted :Unique[Component[_]] = Unique.empty


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = SQLForm.NothingForm
	override def selectForm :SQLReadForm[S] = SQLForm.NothingForm

	override def queryForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def updateForm :SQLWriteForm[S] = SQLWriteForm.empty
	override def insertForm :SQLWriteForm[S] = SQLWriteForm.empty


	override def assemble(values :Pieces) :Option[S] = None
}



class ConstantMapping[O, S](subject :S) extends EmptyMapping[O, S] {
	private[this] val result = Some(subject)

	override def assemble(values :Pieces) :Option[S] = result

	override def optionally(values :Pieces) :Option[S] = result

	override def apply(values :Pieces) :S = subject

	override def toString :String = "Const(" + subject + ")"
}

