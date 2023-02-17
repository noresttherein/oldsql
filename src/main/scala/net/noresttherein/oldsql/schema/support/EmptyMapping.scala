package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.{ColumnExtract, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.BuffType
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.bases.BaseMapping






/** A base trait for mappings which may declare components and columns, but they are used only in the assembly
  * process, and are never written or selected through this mapping. All forms returned by this instance
  * are empty, and all [[net.noresttherein.oldsql.schema.support.EmptyMapping.writtenValues writtenValues]] methods
  * likewise return empty [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]].
  * All column and component lists in this mapping are empty, but it can have non-empty
  * [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] map in order to support aliasing of `ComponentValues`
  * and be fully compliant with the latter's requirements.
  *
  * This trait is useful for adapters of other components of the same enclosing mappings, which would otherwise
  * need to export every used component/column as an adapter with
  * [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
  */ //fixme: this is wrong! Selecting a component will select no columns, updating a component will update no columns...
trait EffectivelyEmptyMapping[S, O] extends BaseMapping[S, O] {

	override def writtenValues[T](op :WriteOperationView, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit = ()
	override def writtenValues[T](op :WriteOperationView, subject :S) :ComponentValues[S, O] = ComponentValues.empty

	override def filterValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty
	override def insertValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty
	override def updateValues(subject :S) :ComponentValues[S, O] = ComponentValues.empty

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = selectForm
	protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[S] =
		SQLWriteForm.empty

	override def selectForm :SQLReadForm[S] = SQLReadForm.empty
	protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[S] = SQLWriteForm.empty

	override def extracts :NaturalMap[Component, Extract] = NaturalMap.empty
	override def columnExtracts :NaturalMap[Column, ColumnExtract] = NaturalMap.empty

	override def contains[T](component :Component[T]) :Boolean = component == this

	override def components :Unique[Component[_]] = Unique.empty
	override def subcomponents :Unique[Component[_]] = Unique.empty

	override def columns :Unique[Column[_]] = Unique.empty
	override def columnsWith(buff :BuffType) :Unique[TypedColumn[_, O]] = Unique.empty
	override def columnsWithout(buff :BuffType) :Unique[TypedColumn[_, O]] = Unique.empty
	override def columnsWith(component :Component[_], buff :BuffType) :Unique[TypedColumn[_, O]] = Unique.empty
	override def columnsWithout(component :Component[_], buff :BuffType) :Unique[TypedColumn[_, O]] = Unique.empty
	override def includedColumns(excludedBuff :BuffType, component :Component[_]) :Unique[TypedColumn[_, O]] =
		Unique.empty


	override def exportOrNot[T](component :Component[T]) :Component[T] = component
	override def exportOrNot[T](column :Column[T]) :Column[T] = column


	override def uniIsomorphic(that :Mapping) :Boolean = that.columns.isEmpty
	override def identical(that :Mapping) :Boolean = canEqual(that) && that.canEqual(this) && that.columns.isEmpty
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[EmptyMapping[_, _]]
}






/** A `Mapping` with no columns or other components. It follows that its write and read forms are empty
  * implementations, reading and writing nothing. It can still produce a value from the `assemble` method
  * if one has been preset for it in the `ComponentValues`.
  */
trait EmptyMapping[S, O] extends EffectivelyEmptyMapping[S, O] {

	override def assemble(pieces :Pieces) :Opt[S] = Lack
	override def optionally(pieces :Pieces) :Opt[S] = pieces.preset(this)

	override def columnNamed(name :String) :Column[_] =
		throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")

	override def apply[T](component :Component[T]) :Extract[T] =
		if (component eq this) MappingExtract.ident(this).asInstanceOf[Extract[T]]
		else throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def apply[T](column :Column[T]) :ColumnExtract[T] =
		if (column eq this) ColumnExtract.ident(column).asInstanceOf[ColumnExtract[T]]
		else throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping: $this.")

	override def export[T](component :Component[T]) :Component[T] =
		if (component eq this) component
		else throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping: $this.")

	override def export[T](column :Column[T]) :Column[T] =
		if (column eq this) column
		else throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping :$this.")

	override def uniHomomorphic(that :Mapping) :Boolean = uniIsomorphic(that)
	override def uniIsomorphic(that :Mapping) :Boolean =
		that.subcomponents.isEmpty && that.columns.isEmpty && nullValue == that.nullValue

	override def equivalent(that :Mapping) :Boolean = //isomorphic(that)
		that.selectedByDefault.isEmpty && that.filteredByDefault.isEmpty &&
			that.insertedByDefault.isEmpty && that.updatedByDefault.isEmpty && nullValue == that.nullValue

	override def identical(that :Mapping) :Boolean = that match {
		case _ if that eq this => true
		case other :EmptyMapping[_, _] if canEqual(that) && that.canEqual(this) => nullValue == other.nullValue
		case _ => false
	}

}

