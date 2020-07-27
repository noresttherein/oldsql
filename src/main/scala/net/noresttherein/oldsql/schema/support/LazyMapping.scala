package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{Mapping, SQLReadForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, InsertAudit, NoInsert, NoQuery, NoUpdate, OptionalSelect, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.OperationType.WriteOperationType


/** A convenience base trait for simple mappings which initialize all column, component and extract lists
  * by filtering the result of the abstract method `columns` based on their applied buffs. The fields are initialized
  * lazily to avoid calls to `columns` before the class defining it is properly initialized. Additionally,
  * `optionally` implementation is optimized by similarly storing lazily precomputed required buff information.
  * They all use [[net.noresttherein.oldsql.collection.Unique.delayed]] or [[net.noresttherein.oldsql.morsels.Lazy Lazy]],
  * which are thread safe, invoke the initializer at most once, and don't incur any computational penalty once initialized.
  * @see [[net.noresttherein.oldsql.schema.support.StableMapping]]
  */
trait LazyMapping[S, O] extends TypedMapping[S, O] {



	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		op.writtenValues(this, subject, collector)

	override def queryValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isQueryable) {
			val audited = queryAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.queryValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isQueryable = Lazy(NoQuery.disabled(this))
	private val isUpdatable = Lazy(NoUpdate.disabled(this))
	private val isInsertable = Lazy(NoInsert.disabled(this))
	private val queryAudit = Lazy(QueryAudit.fold(this))
	private val updateAudit = Lazy(UpdateAudit.fold(this))
	private val insertAudit = Lazy(InsertAudit.fold(this))



	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res
		case res :Some[S] =>
			if (noSelectAudit) res else Some(selectAudit(res.get))
		case _ =>
			val res = default.get
			if (res.isDefined) res else explicit.get
	}

	private val selectAudit = Lazy(SelectAudit.fold(this))
	private val noSelectAudit = Lazy(SelectAudit.enabled(this))
	private val default = Lazy(OptionalSelect.Value(this))
	private val explicit = Lazy(ExtraSelect.Value(this))



	override val columnExtracts :NaturalMap[Column, ColumnExtract] =
		NaturalMap.Lazy(schema.selectColumnExtracts(this)(extracts))

	override val subcomponents :Unique[Component[_]] = Unique.delayed(components.flatMap { c => c +: c.components })

	override val columns :Unique[Column[_]] = Unique.delayed(components.flatMap(_.columns))
	override val selectable :Unique[Column[_]] = Unique.delayed(super.selectable)
	override val queryable :Unique[Column[_]] = Unique.delayed(super.queryable)
	override val updatable :Unique[Column[_]] = Unique.delayed(super.updatable)
	override val autoUpdated :Unique[Column[_]] = Unique.delayed(super.autoUpdated)
	override val insertable :Unique[Column[_]] = Unique.delayed(super.insertable)
	override val autoInserted :Unique[Column[_]] = Unique.delayed(super.autoInserted)

	override val selectForm: SQLReadForm[S] = SQLReadForm.Lazy(super.selectForm)
	override val queryForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.queryForm)
	override val updateForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.updateForm)
	override val insertForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.insertForm)
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)


}





/** A mixin trait which defines all column and component lists as `val`s initialized by the call to super.
  * It caches also extracts and some buff information required by `optionally` to provide a faster implementation.
  * It must come in linearization order after the final definitions of all standard `Mapping` properties, which are
  * in most cases overriden here as `abstract override` methods.
  * @see [[net.noresttherein.oldsql.schema.support.LazyMapping]]
  */
trait StableMapping[S, O] extends Mapping { this :TypedMapping[S, O] =>
	//todo: this should extend TypedMapping but currently it is extended by some traits with confliciting (narrowed)
	// declarations of methods implemented in TypedMapping

	override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		op.writtenValues(this, subject, collector)

	override def queryValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isQueryable) {
			val audited = queryAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.queryValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
				case Some(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private val isQueryable = NoQuery.disabled(this)
	private val isUpdatable = NoUpdate.disabled(this)
	private val isInsertable = NoInsert.disabled(this)
	private val queryAudit = QueryAudit.fold(this)
	private val updateAudit = UpdateAudit.fold(this)
	private val insertAudit = InsertAudit.fold(this)



	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res if noBuffs => res
		case res :Some[S] =>
			if (noSelectAudit) res else Some(selectAudit(res.get))
		case _ =>
			if (default.isDefined) default else explicit
	}

	private val noBuffs = buffs.isEmpty
	private val noSelectAudit = SelectAudit.disabled(this)
	private val selectAudit = SelectAudit.fold(this)
	private val default = OptionalSelect.Value(this)
	private val explicit = ExtraSelect.Value(this)


	abstract override val extracts :NaturalMap[Component, Extract] = super.extracts
	abstract override val columnExtracts :NaturalMap[Column, ColumnExtract] = super.columnExtracts

	abstract override val subcomponents :Unique[Component[_]] = super.subcomponents
	abstract override val components :Unique[Component[_]] = super.components

	abstract override val columns :Unique[Column[_]] = super.columns

	override val selectable :Unique[Column[_]] = super.selectable
	override val queryable :Unique[Column[_]] = super.queryable
	override val updatable :Unique[Column[_]] = super.updatable
	override val autoUpdated :Unique[Column[_]] = super.autoUpdated
	override val insertable :Unique[Column[_]] = super.insertable
	override val autoInserted :Unique[Column[_]] = super.autoInserted

	abstract override val selectForm: SQLReadForm[S] = super.selectForm
	abstract override val queryForm :SQLWriteForm[S] = super.queryForm
	abstract override val updateForm :SQLWriteForm[S] = super.updateForm
	abstract override val insertForm :SQLWriteForm[S] = super.insertForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

}





