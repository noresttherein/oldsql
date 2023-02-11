package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.{Buffs, Mapping, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.EmptyMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue






/** A synthetic `EmptyMapping` implementation backed by an `SQLForm`. As it does not have any components
  * but is not a column, either, it breaks the `Mapping` contract and should not be used in the client code.
  * It serves the niche need of representing `SQL` parameters as mappings in the SQL DSL.
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
class FormMapping[S, O](implicit val form :SQLForm[S]) extends EmptyMapping[S, O] {

	override def nullValue :NullValue[S] = form.nulls

	final override def buffs :Buffs[S] = Buffs.empty

	override def writtenValues[T](op :WriteOperationView, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		collector.add(this, subject)

	override def writtenValues[T](op :WriteOperationView, subject :S) :ComponentValues[S, O] =
		ComponentValues(this, subject)

	override def filterValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = ComponentValues(this, subject)

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components.isEmpty) SQLReadForm.defaults(form.columnCount)(form.nulls)
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)

	protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components.isEmpty) SQLWriteForm.empty
		else if (components.size == 1 && components.head == this) form
		else throw new IllegalArgumentException("Mappings " + components + " are not components of " + this)

	override def selectForm :SQLReadForm[S] = form
	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = form


	override def uniIsomorphic(that :Mapping) :Boolean = that match {
		case _ if this eq that => true
		case other :FormMapping[_, _] => form == other.form
		case _ => false
	}
	override def equivalent(that :Mapping) :Boolean = identical(that)
	override def identical(that :Mapping) :Boolean = that match {
		case _ if this eq that => true
		case other :FormMapping[_, _] if canEqual(that) && that.canEqual(this) => form == other.form
		case _ => false
	}

	override def toString :String = mappingName + "[" + form + "]"
}
