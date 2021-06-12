package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.{Buffs, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.EmptyMapping






/** A synthetic `EmptyMapping` implementation backed by an `SQLForm`. As it does not have any components
  * but is not a column, either, it breaks the `Mapping` contract and should not be used in the client code.
  * It serves the niche need of representing `SQL` parameters as mappings in the SQL DSL.
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
class FormMapping[S, O](implicit val form :SQLForm[S]) extends EmptyMapping[S, O] {

	final override def buffs :Buffs[S] = Buffs.empty

	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		collector.add(this, subject)

	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] =
		ComponentValues(this, subject)

	override def filterValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.isEmpty) SQLReadForm.defaults(form.nulls)
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.isEmpty) SQLWriteForm.empty
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)

//	override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] = writeForm(FILTER, components)
//	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = writeForm(UPDATE, components)
//	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = writeForm(INSERT, components)

	override def selectForm :SQLReadForm[S] = form
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = form
//	override def insertForm :SQLWriteForm[S] = form
//	override def filterForm :SQLWriteForm[S] = form
//	override def updateForm :SQLWriteForm[S] = form

}
