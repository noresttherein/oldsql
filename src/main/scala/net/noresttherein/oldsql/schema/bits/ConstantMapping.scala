package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.support.EmptyMapping
import net.noresttherein.oldsql.schema.{SQLForm, SQLReadForm, SQLWriteForm}



/** A `Mapping` implementation with no components or columns, always returning the predefined value.
  * Any value present in `ComponentValues` is always ignored.
  * @author Marcin Mościcki
  */
class ConstantMapping[S, O](subject :S) extends EmptyMapping[S, O] {
	private[this] val result = Some(subject)

	override def assemble(values :Pieces) :Option[S] = result

	override def optionally(values :Pieces) :Option[S] = result

	override def apply(values :Pieces) :S = subject

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		SQLReadForm.const(subject, (0 /: components) { _ + _.selectable.size })

	override val selectForm :SQLReadForm[S] = SQLReadForm.const(subject)

	override def toString :String = "Const(" + subject + ")"
}






class GeneratorMapping[S, O](generator: => S) extends EmptyMapping[S, O] {

	override def assemble(values :Pieces) :Option[S] = Some(generator)

	override def optionally(values :Pieces) :Option[S] = Some(generator)

	override def apply(values :Pieces) :S = generator

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		SQLReadForm.eval(generator, (0 /: components) { _ + _.selectable.size })

	override val selectForm :SQLReadForm[S] = SQLReadForm.eval(generator)

	override def toString :String = "Generator#" + System.identityHashCode(this)
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

