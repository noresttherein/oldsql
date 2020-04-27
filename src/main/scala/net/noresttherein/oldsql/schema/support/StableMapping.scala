package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{GenericMapping, SQLReadForm, SQLWriteForm}


/** A convenience base trait for simple mappings which initialize all column lists by filtering the result
  * of the abstract method `columns` based on their applied buffs. The fields are initialized lazily to avoid
  * calls to `columns` before the class defining it is properly initialized. They all use
  * [[net.noresttherein.oldsql.collection.Unique.delay]], which is thread safe, invokes the initializer at most once,
  * and doesn't incur any computational penalty once initialized.
  */
trait StableMapping[S, O] extends GenericMapping[S, O] {

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





trait SelectorCache[S, O] extends GenericMapping[S, O] {


	private[this] val selectors = Lazy {
		def entry[T](component :Component[T]) =
			new NaturalMap.Entry[Component, Selector, T](component, super.apply(component))
		val builder = NaturalMap.newBuilder[Component, Selector]
		for (c <- subcomponents)
			builder += entry(c)
		builder.result()
	}

	private[this] val columnSelectors = Lazy {
		def entry[T](column :Column[T]) =
			new NaturalMap.Entry[Column, ColumnSelector, T](column, super.apply(column))

		val builder = NaturalMap.newBuilder[Column, ColumnSelector]
		for (c <- columns)
			builder += entry(c)
		builder.result()
	}



	abstract override def apply[T](component :Component[T]) :Selector[T] = selectors.get(component)

	abstract override def apply[T](column :Column[T]) :ColumnSelector[T] = columnSelectors.get(column)

}
