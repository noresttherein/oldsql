package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, FilterAudit, InsertAudit, InsertDefault, NoFilter, NoInsert, NoUpdate, OptionalSelect, SelectAudit, SelectDefault, UpdateAudit, UpdateDefault}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping






/** A `Mapping` mixin trait which caches buff information in member fields and overrides the methods for assembly
  * and disassembly taking advantage of these flags.
  */ //todo: buffs?
trait OptimizedMappingAssembly extends Mapping {

	override def optionally(pieces :Pieces) :Opt[Subject] = pieces.assemble(refine) match {
		case res if buffs.isEmpty => res
		case res if res.isDefined => selectAudit.get match {
			case Got(audit) => Got(audit(res.get))
			case _ => res
		}
		case _ => selectDefault.get
	}

	private val selectAudit = Lazy { if (SelectAudit.active(refine)) Got(SelectAudit.fold(refine)) else Lack }
	private val selectDefault = Lazy { SelectDefault.Value(refine) orElse ExtraSelect.Value(refine) }


	override def writtenValues[T](op :WriteOperationType, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		op.writtenValues(refine, subject, collector)

	override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isFilterable) {
			val audited = filterAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.filterValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isFilterable = Lazy(NoFilter.inactive(this))
	private val isInsertable = Lazy(NoInsert.inactive(this))
	private val isUpdatable = Lazy(NoUpdate.inactive(this))
	private val filterAudit = Lazy(FilterAudit.fold(refine))
	private val insertAudit = Lazy(InsertAudit.fold(refine))
	private val updateAudit = Lazy(UpdateAudit.fold(refine))
	private val insertDefault = Lazy(InsertDefault.Value(refine))
	private val updateDefault = Lazy(UpdateDefault.Value(refine))

}






/** A convenience base trait for simple mappings which overrides a selection of `Mapping` methods for efficiency,
  * storing often used values in member fields. The fields are declared as
  * [[net.noresttherein.oldsql.morsels.Lazy Lazy]] values, which are evaluated at most once, when first used,
  * (because they depend on several abstract methods to be implemented by subclasses) and are thread safe, but incur
  * no synchronization penalties once initialized.
  *
  * The implemented methods include forms, column, component and extract lists (except for `components`, `extracts`
  * by filtering the result of the abstract method `columns` based on their applied buffs, but also `optionally`
  * and the `writtenValues` family of methods to benefit from stored buff information.
  * @see [[net.noresttherein.oldsql.schema.bases.StableMapping]]
  */
trait LazyMapping[S, O] extends BaseMapping[S, O] with OptimizedMappingAssembly {

	override def apply[T](component :Component[T]) :Extract[T] =
		if (component eq this) lazySelfExtract.get.asInstanceOf[Extract[T]]
		else extracts(component)

	protected[schema] val lazySelfExtract :Lazy[MappingExtract[S, S, O]] = Lazy(MappingExtract.ident(this))
	protected[schema] val lazyColumnExtracts :Lazy[ColumnExtractMap] =
		Lazy(schema.filterColumnExtracts(this)(extracts))

	override def columnExtracts :NaturalMap[Column, ColumnExtract] = lazyColumnExtracts

	protected[schema] val lazySubcomponents :Unique[Component[_]] =
		Lazy(components.flatMap { c => export(c) +: c.components.map(export(_)) })

	//columns of a column is the column itself, so works for direct columns, too
	protected[schema] val lazyColumns :Lazy[Unique[Column[_]]] = Lazy(components.flatMap(_.columns).map(export(_)))
	protected[schema] val lazySelectable :Lazy[Unique[Column[_]]] = Lazy(super.selectable)
	protected[schema] val lazyFilterable :Lazy[Unique[Column[_]]] = Lazy(super.filterable)
	protected[schema] val lazyInsertable :Lazy[Unique[Column[_]]] = Lazy(super.insertable)
	protected[schema] val lazyUpdatable :Lazy[Unique[Column[_]]] = Lazy(super.updatable)
	protected[schema] val lazyAutoInserted :Lazy[Unique[Column[_]]] = Lazy(super.autoInserted)
	protected[schema] val lazyAutoUpdated :Lazy[Unique[Column[_]]] = Lazy(super.autoUpdated)
	protected[schema] val lazySelectedByDefault :Lazy[Unique[Column[_]]] = Lazy(super.selectedByDefault)
	protected[schema] val lazyFilteredByDefault :Lazy[Unique[Column[_]]] = Lazy(super.filteredByDefault)
	protected[schema] val lazyInsertedByDefault :Lazy[Unique[Column[_]]] = Lazy(super.insertedByDefault)
	protected[schema] val lazyUpdatedByDefault :Lazy[Unique[Column[_]]] = Lazy(super.updatedByDefault)

	override def subcomponents :Unique[Component[_]] = lazySubcomponents
	override def columns :Unique[Column[_]] = lazyColumns
	override def selectable :Unique[Column[_]] = lazySelectable
	override def filterable :Unique[Column[_]] = lazyFilterable
	override def insertable :Unique[Column[_]] = lazyInsertable
	override def updatable :Unique[Column[_]] = lazyUpdatable
	override def autoInserted :Unique[Column[_]] = lazyAutoInserted
	override def autoUpdated :Unique[Column[_]] = lazyAutoUpdated
	override def selectedByDefault :Unique[Column[_]] = lazySelectedByDefault
	override def filteredByDefault :Unique[Column[_]] = lazyFilteredByDefault
	override def insertedByDefault :Unique[Column[_]] = lazyInsertedByDefault
	override def updatedByDefault :Unique[Column[_]] = lazyUpdatedByDefault

	protected[schema] val lazyColumnMap :Lazy[Map[String, Column[_]]] =
		Lazy(columns.view.map { c => (c.name, c) }.toMap)

	override def columnNamed(name :String) :Column[_] = lazyColumnMap.getOrElse(name, null) match {
		case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
		case res => res
	}

	protected[schema] val lazySelectForm :Lazy[SQLReadForm[S]] = Lazy(super.selectForm)
	protected[schema] val lazyFilterForm :Lazy[SQLWriteForm[S]] = Lazy(super.filterForm)
	protected[schema] val lazyInsertForm :Lazy[SQLWriteForm[S]] = Lazy(super.insertForm)
	protected[schema] val lazyUpdateForm :Lazy[SQLWriteForm[S]] = Lazy(super.updateForm)

	override def selectForm: SQLReadForm[S] = lazySelectForm
	override def filterForm: SQLWriteForm[S] = lazyFilterForm
	override def insertForm: SQLWriteForm[S] = lazyInsertForm
	override def updateForm: SQLWriteForm[S] = lazyUpdateForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components == selectedByDefault) selectForm else super.selectForm(components)

	override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components == filteredByDefault) filterForm else super.filterForm(components)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components == insertedByDefault) insertForm else super.insertForm(components)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (components == updatedByDefault) updateForm else super.updateForm(components)

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		op.form(this)
}






/** A stackable mixin trait which defines all column and component lists as `val`s initialized by the call to super.
  * It caches also extracts and some buff information required by `optionally` to provide a faster implementation.
  * It must come in linearization order after the final definitions of all standard `Mapping` properties,
  * in particular buffs, all column, component and extract lists, which are in most cases overriden here
  * as `abstract override` methods. The implementations are very similar to those in
  * [[net.noresttherein.oldsql.schema.bases.LazyMapping LazyMapping]], the difference between the two being
  * that this trait is not really considered a base trait for extension by custom `Mapping` classes,
  * as the precomputed values are not lazy and would be referenced before the initialization of the extending class.
  * @see [[net.noresttherein.oldsql.schema.bases.LazyMapping]]
  */
trait StableMapping extends Mapping {
	//todo: this should extend BaseMapping but currently it is extended by some traits with conflicting (narrowed)
	// declarations of methods implemented in BaseMapping

	override def optionally(pieces :Pieces) :Opt[Subject] = pieces.assemble(refine) match {
		case res if noBuffs => res
		case res if res.isDefined => if (noSelectAudit) res else Got(selectAudit(res.get))
		case _ => selectDefault
	}


	override def writtenValues[T](op :WriteOperationType, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		op.writtenValues(refine, subject, collector)

	override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isFilterable) {
			val audited = filterAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.filterValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isFilterable = NoFilter.inactive(this)
	private val isInsertable = NoInsert.inactive(this)
	private val isUpdatable = NoUpdate.inactive(this)
	private val filterAudit = FilterAudit.fold(refine)
	private val insertAudit = InsertAudit.fold(refine)
	private val updateAudit = UpdateAudit.fold(refine)


	private val noBuffs = buffs.isEmpty
	private val noSelectAudit = SelectAudit.inactive(this)
	private val selectAudit = SelectAudit.fold(refine)
	private val selectDefault = SelectDefault.Value(refine)


//	override def apply[T](component :Component[T]) :Extract[T] =
//		if (component eq this) selfExtract.asInstanceOf[Extract[T]]
//		else extracts(component)
//
//	override def export[T](component :Component[T]) :Component[T] = apply(component).export
//
	abstract override val extracts :NaturalMap[Component, Extract] = super.extracts
	abstract override val columnExtracts :NaturalMap[Column, ColumnExtract] = super.columnExtracts
//	private val selfExtract = extracts.getOrElse(refine, MappingExtract.ident(refine))

	abstract override val subcomponents :Unique[Component[_]] = super.subcomponents
	abstract override val components :Unique[Component[_]] = super.components

	abstract override val columns :Unique[Column[_]] = super.columns

	override val selectable :Unique[Column[_]] = super.selectable
	override val filterable :Unique[Column[_]] = super.filterable
	override val insertable :Unique[Column[_]] = super.insertable
	override val updatable :Unique[Column[_]] = super.updatable
	override val autoInserted :Unique[Column[_]] = super.autoInserted
	override val autoUpdated :Unique[Column[_]] = super.autoUpdated
	override val selectedByDefault :Unique[Column[_]] = super.selectedByDefault
	override val filteredByDefault :Unique[Column[_]] = super.filteredByDefault
	override val insertedByDefault :Unique[Column[_]] = super.insertedByDefault
	override val updatedByDefault :Unique[Column[_]] = super.updatedByDefault

	private val columnMap = columns.view.map { c => (c.name, c) }.toMap

	override def columnNamed(name :String) :Column[_] = columnMap.getOrElse(name, null) match {
		case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
		case res => res
	}

	abstract override val selectForm: SQLReadForm[Subject] = super.selectForm
	abstract override val filterForm :SQLWriteForm[Subject] = super.filterForm
	abstract override val insertForm :SQLWriteForm[Subject] = super.insertForm
	abstract override val updateForm :SQLWriteForm[Subject] = super.updateForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[Subject] = op.form(refine)


	abstract override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject] =
		if (components == selectedByDefault) selectForm else super.selectForm(components)

	abstract override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
		if (components == filteredByDefault) filterForm else super.filterForm(components)

	abstract override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
		if (components == insertedByDefault) insertForm else super.insertForm(components)

	abstract override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
		if (components == updatedByDefault) updateForm else super.updateForm(components)

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[Subject] =
		op.form(refine)

}





