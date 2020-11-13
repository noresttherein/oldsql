package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, FilterAudit, InsertAudit, NoFilter, NoInsert, NoUpdate, OptionalSelect, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.OperationType.WriteOperationType






/** A `Mapping` mixin trait which caches buff information in member fields and overrides the methods for assembly
  * and disassembly taking advantage of these flags.
  */
trait OptimizedMappingAssembly extends Mapping {

	override def writtenValues[T](op :WriteOperationType, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		op.writtenValues(refine, subject, collector)

	override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isFilterable) {
			val audited = filterAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.filterValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isFilterable = Lazy(NoFilter.disabled(this))
	private val isUpdatable = Lazy(NoUpdate.disabled(this))
	private val isInsertable = Lazy(NoInsert.disabled(this))
	private val filterAudit = Lazy(FilterAudit.fold(refine))
	private val updateAudit = Lazy(UpdateAudit.fold(refine))
	private val insertAudit = Lazy(InsertAudit.fold(refine))



	override def optionally(pieces :Pieces) :Option[Subject] = pieces.assemble(refine) match {
		case res if buffs.isEmpty => res
		case res :Some[Subject] =>
			if (isAuditable) Some(selectAudit(res.get)) else res
		case _ =>
			val res = default.get
			if (res.isDefined) res else explicit
	}

	private val isAuditable = Lazy(SelectAudit.enabled(refine))
	private val selectAudit = Lazy(SelectAudit.fold(refine))
	private val default = Lazy(OptionalSelect.Value(refine))
	private val explicit = Lazy(ExtraSelect.Value(refine))

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

	private val lazyColumnExtracts = Lazy(schema.filterColumnExtracts(this)(extracts))

	override def columnExtracts :NaturalMap[Column, ColumnExtract] = lazyColumnExtracts

	private val lazySubcomponents :Unique[Component[_]] = Lazy(components.flatMap { c => c +: c.components })
	private val lazyColumns = Lazy(components.flatMap(_.columns)) //columns of a column is the column itself, so works for direct columns, too
	private val lazySelectable = Lazy(super.selectable)
	private val lazyFilterable = Lazy(super.filterable)
	private val lazyUpdatable = Lazy(super.updatable)
	private val lazyAutoUpdated = Lazy(super.autoUpdated)
	private val lazyInsertable = Lazy(super.insertable)
	private val lazyAutoInserted = Lazy(super.autoInserted)

	override def subcomponents :Unique[Component[_]] = lazySubcomponents
	override def columns :Unique[Column[_]] = lazyColumns
	override def selectable :Unique[Column[_]] = lazySelectable
	override def filterable :Unique[Column[_]] = lazyFilterable
	override def updatable :Unique[Column[_]] = lazyUpdatable
	override def autoUpdated :Unique[Column[_]] = lazyAutoUpdated
	override def insertable :Unique[Column[_]] = lazyInsertable
	override def autoInserted :Unique[Column[_]] = lazyAutoInserted

	private val lazySelectForm = Lazy(super.selectForm)
	private val lazyFilterForm = Lazy(super.filterForm)
	private val lazyUpdateForm = Lazy(super.updateForm)
	private val lazyInsertForm = Lazy(super.insertForm)

	override def selectForm: SQLReadForm[S] = lazySelectForm
	override def filterForm: SQLWriteForm[S] = lazyFilterForm
	override def updateForm: SQLWriteForm[S] = lazyUpdateForm
	override def insertForm: SQLWriteForm[S] = lazyInsertForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

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

	override def writtenValues[T](op :WriteOperationType, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		op.writtenValues(refine, subject, collector)

	override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isFilterable) {
			val audited = filterAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.filterValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isFilterable = NoFilter.disabled(this)
	private val isUpdatable = NoUpdate.disabled(this)
	private val isInsertable = NoInsert.disabled(this)
	private val filterAudit = FilterAudit.fold(refine)
	private val updateAudit = UpdateAudit.fold(refine)
	private val insertAudit = InsertAudit.fold(refine)



	override def optionally(pieces :Pieces) :Option[Subject] = pieces.assemble(refine) match {
		case res if noBuffs => res
		case res :Some[Subject] =>
			if (noSelectAudit) res else Some(selectAudit(res.get))
		case _ =>
			if (default.isDefined) default else explicit
	}

	private val noBuffs = buffs.isEmpty
	private val noSelectAudit = SelectAudit.disabled(this)
	private val selectAudit = SelectAudit.fold(refine)
	private val default = OptionalSelect.Value(refine)
	private val explicit = ExtraSelect.Value(refine)


	abstract override val extracts :NaturalMap[Component, Extract] = super.extracts
	abstract override val columnExtracts :NaturalMap[Column, ColumnExtract] = super.columnExtracts

	abstract override val subcomponents :Unique[Component[_]] = super.subcomponents
	abstract override val components :Unique[Component[_]] = super.components

	abstract override val columns :Unique[Column[_]] = super.columns

	override val selectable :Unique[Column[_]] = super.selectable
	override val filterable :Unique[Column[_]] = super.filterable
	override val updatable :Unique[Column[_]] = super.updatable
	override val autoUpdated :Unique[Column[_]] = super.autoUpdated
	override val insertable :Unique[Column[_]] = super.insertable
	override val autoInserted :Unique[Column[_]] = super.autoInserted

	abstract override val selectForm: SQLReadForm[Subject] = super.selectForm
	abstract override val filterForm :SQLWriteForm[Subject] = super.filterForm
	abstract override val updateForm :SQLWriteForm[Subject] = super.updateForm
	abstract override val insertForm :SQLWriteForm[Subject] = super.insertForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[Subject] = op.form(refine)

}





