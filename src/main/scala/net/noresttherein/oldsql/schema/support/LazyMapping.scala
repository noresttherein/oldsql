package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{ColumnMapping, GenericMapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.MappingExtract.ColumnMappingExtract


/** A convenience base trait for simple mappings which initialize all column lists by filtering the result
  * of the abstract method `columns` based on their applied buffs. The fields are initialized lazily to avoid
  * calls to `columns` before the class defining it is properly initialized. They all use
  * [[net.noresttherein.oldsql.collection.Unique.delay]], which is thread safe, invokes the initializer at most once,
  * and doesn't incur any computational penalty once initialized.
  */
trait LazyMapping[S, O] extends GenericMapping[S, O] {

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






trait StableMapping[S, O] extends GenericMapping[S, O] {

	abstract override val extracts :NaturalMap[Component, Extract] = super.extracts
	abstract override val columnExtracts :NaturalMap[Column, ColumnExtract] = super.columnExtracts

	abstract override val subcomponents :Unique[Component[_]] = super.subcomponents
	abstract override val components :Unique[Component[_]] = super.components

	abstract override val columns :Unique[Column[_]] = super.columns //components.flatMap(_.columns)

	override val selectable :Unique[Column[_]] = super.selectable
	override val queryable :Unique[Column[_]] = super.queryable
	override val updatable :Unique[Column[_]] = super.updatable
	override val autoUpdated :Unique[Column[_]] = super.autoUpdated
	override val insertable :Unique[Column[_]] = super.insertable
	override val autoInserted :Unique[Column[_]] = super.autoInserted

	override val selectForm: SQLReadForm[S] = super.selectForm
	override val queryForm :SQLWriteForm[S] = super.queryForm
	override val updateForm :SQLWriteForm[S] = super.updateForm
	override val insertForm :SQLWriteForm[S] = super.insertForm

}





