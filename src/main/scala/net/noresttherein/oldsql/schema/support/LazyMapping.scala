package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{Mapping, SQLReadForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, OptionalSelect, SelectAudit}


/** A convenience base trait for simple mappings which initialize all column, component and extract lists
  * by filtering the result of the abstract method `columns` based on their applied buffs. The fields are initialized
  * lazily to avoid calls to `columns` before the class defining it is properly initialized. Additionally,
  * `optionally` implementation is optimized by similarly storing lazily precomputed required buff information.
  * They all use [[net.noresttherein.oldsql.collection.Unique.delay]] or [[net.noresttherein.oldsql.morsels.Lazy Lazy]],
  * which are thread safe, invoke the initializer at most once, and don't incur any computational penalty once initialized.
  * @see [[net.noresttherein.oldsql.schema.support.StableMapping]]
  */
trait LazyMapping[S, O] extends TypedMapping[S, O] {


	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res
		case res :Some[S] =>
			if (audits.isEmpty) res else Some((res.get /: audits) { (acc, f) => f(acc) })
		case _ =>
			val res = default.get
			if (res.isDefined) res else explicit.get
	}

	private val audits = Lazy(SelectAudit.Audit(this))
	private val default = Lazy(OptionalSelect.Value(this))
	private val explicit = Lazy(ExtraSelect.Value(this))



	override val columnExtracts :NaturalMap[Column, ColumnExtract] =
		NaturalMap.Lazy(schema.selectColumnExtracts(this)(extracts))

	override val subcomponents :Unique[Component[_]] = Unique.delay(components.flatMap { c => c +: c.components })

	override val columns :Unique[Column[_]] = Unique.delay(components.flatMap(_.columns))
	override val selectable :Unique[Column[_]] = Unique.delay(super.selectable)
	override val queryable :Unique[Column[_]] = Unique.delay(super.queryable)
	override val updatable :Unique[Column[_]] = Unique.delay(super.updatable)
	override val autoUpdated :Unique[Column[_]] = Unique.delay(super.autoUpdated)
	override val insertable :Unique[Column[_]] = Unique.delay(super.insertable)
	override val autoInserted :Unique[Column[_]] = Unique.delay(super.autoInserted)

	override val selectForm: SQLReadForm[S] = SQLReadForm.Lazy(super.selectForm)
	override val queryForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.queryForm)
	override val updateForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.updateForm)
	override val insertForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.insertForm)


}





/** A mixin trait which defines all column and component lists as `val`s initialized by the call to super.
  * It caches also extracts and some buff information required by `optionally` to provide a faster implementation.
  * It must come in linearization order after the final definitions of all standard `Mapping` properties, which are
  * in most cases overriden here as `abstract override` methods.
  * @see [[net.noresttherein.oldsql.schema.support.LazyMapping]]
  */
trait StableMapping[S, O] extends Mapping { this :TypedMapping[S, O] =>

	override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
		case res if noBuffs => res
		case res :Some[S] =>
			if (audits.isEmpty) res else Some((res.get /: audits) { (acc, f) => f(acc) })
		case _ =>
			val res = default
			if (res.isDefined) res else explicit
	}

	private val noBuffs = buffs.isEmpty
	private val audits = SelectAudit.Audit(this)
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

}





