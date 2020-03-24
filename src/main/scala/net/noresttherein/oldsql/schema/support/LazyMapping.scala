package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping, SQLReadForm, SQLWriteForm}


/** A Convenience base trait for simple mappings which initializes all column lists by filtering the result
  * of the abstract method `columns` based on their applied buffs. The fields are initialized lazily to avoid
  * calls to `columns` before the class defining it is properly initialized. They all use
  * [[net.noresttherein.oldsql.collection.Unique.Lazy]], which is thread safe, invokes the initializer at most once,
  * and doesn't incur any computational penalty once initialized.
  */
trait LazyMapping[O, S] extends GenericMapping[O, S] {

//	override val subcomponents :Unique[Component[_]] = Unique.Lazy(super.subcomponents)

	override val selectable :Unique[Component[_]] = Unique.Lazy(super.selectable)
	override val queryable :Unique[Component[_]] = Unique.Lazy(super.queryable)
	override val updatable :Unique[Component[_]] = Unique.Lazy(super.updatable)
	override val autoUpdated :Unique[Component[_]] = Unique.Lazy(super.autoUpdated)
	override val insertable :Unique[Component[_]] = Unique.Lazy(super.insertable)
	override val autoInserted :Unique[Component[_]] = Unique.Lazy(super.autoInserted)

	override val selectForm: SQLReadForm[S] = SQLReadForm.Lazy(super.selectForm)
	override val queryForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.queryForm)
	override val updateForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.updateForm)
	override val insertForm: SQLWriteForm[S] = SQLWriteForm.Lazy(super.insertForm)


}
