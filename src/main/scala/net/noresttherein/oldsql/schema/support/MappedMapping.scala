package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{ColumnExtract, ColumnForm, ColumnMapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.schema.support.MappingAdapter.{ColumnAdapter, ComposedAdapter, DelegateAdapter, MappedTo}
import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}






trait MappedMapping[T, S, O] extends ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[T, O], S, O] {

	/** The subject value `S` that `null` values from the database are mapped to.
	  * This is different from `this.nullValue` in that it is designed to hold the implicit method parameter
	  * of `Mapping.map` (producing this mapping) which defaults to `null`. For this reason `nullValue`
	  * will use it if not-null, but default to mapping the `nullVaulue` of the mapped component if `null`.
	  * Note that this '''must''' be implemented as either a method or a constructor parameter `val` as it is used in
	  * this trait's constructor body.
	  */
	protected def nulls :NullValue[S]

	/** Function mapping the subject type `T` of the adapted mapping to the target subject type `S`, used by read forms
	  * and assembly method. It is used in the constructor body of this trait and as such '''must''' be overriden
	  * with either a method or a constructor parameter `val` or `NullPointerException` will be thrown from
	  * the constructor.
	  */
	protected def map :T =?> S //todo: make sure these are stable in subclasses

	/** Function mapping the subject type `S`` of this mapping to the subject type `T` of the` adapted mapping,
	  * used by write forms. It is used in the constructor body of this trait and as such '''must''' be overriden
	  * with either a method or a constructor parameter `val` or `NullPointerException` will be thrown from
	  * the constructor.
	  */
	protected def unmap :S =?> T

	if (map == null || unmap == null)
		throw new IllegalStateException(s"MappedMapping map ($map) or unmap ($unmap) extract is null; overrides " +
			                             "must happen with a def or a constructor parameter val.")

	private[this] val mapFun = map.requisite.orNull
	private[this] val flatMapFun = map.optional


	override def optionally(values :Pieces) :Opt[S] = values.assemble(this)

	override def assemble(values :Pieces) :Opt[S] = values.get(backerExtract) match {
		case Got(t) => map.opt(t)
		case _ => Lack
	}

	/** The subject value `S` that database `null`s should map to, used by the read forms. It is initialized
	  * with the `nulls` parameter of the `Mapping.optMap` method creating `MappedMapping` instances. If the latter
	  * is `null` (the field, not the wrapped value) it defaults to mapping the `nullValue` of the adapted mapping
	  * with `map`.
	  */
	implicit override val nullValue :NullValue[S] =
		if (nulls != null) nulls
		else if (mapFun == null) backer.nullValue.optMap(flatMapFun)
		else backer.nullValue.map(mapFun)


	override def apply[X](component :Component[X]) :Extract[X] =
		if (component eq backer) backerExtract.asInstanceOf[Extract[X]]
		else if (component eq this) selfExtract.asInstanceOf[Extract[X]]
		else extracts(component)

	override def apply[X](column :Column[X]) :ColumnExtract[X] =
		if (column eq backer) backerExtract.asInstanceOf[ColumnExtract[X]]
		else columnExtracts(column)


	private[this] val backerExtract :Extract[T] = MappingExtract(backer)(unmap)
	private[this] val selfExtract :Extract[S] = MappingExtract.ident(this)

	private val composedExtracts :NaturalMap[Component, Extract] =
		schema.composeExtracts(backer, backerExtract).updated(backer, backerExtract)

	private val composedColumnExtracts :NaturalMap[Column, ColumnExtract] = //we could shortcut it if backer is a column,
		schema.composeColumnExtracts(backer, backerExtract)          //but for the ColumnLabel which has *two* components

	override def extracts :NaturalMap[Component, Extract] = composedExtracts
	override def columnExtracts :NaturalMap[Column, ColumnExtract] = composedColumnExtracts

	private val singletonComponent = Unique.single(backer)
	private val backerComponents = backer +: backer.subcomponents
	override def components :Unique[Component[_]] = singletonComponent
	override def subcomponents :Unique[Component[_]] = backerComponents


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		if (components == selectedByDefault)
			selectForm
		else {
			val comps =
				if (components.contains(backer)) backer.selectedByDefault ++ components.view.filter(_ != backer)
				else components
			val form = backer.selectForm(comps)
			if (nulls == null) form.nullTo(map) else form.to(map)
		}

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] =
		if (op.defaultColumns(backer) == components)
			op.form(this)
		else {
			val form = backer.writeForm(op, if (components.contains(backer)) op.defaultColumns(backer) else components)
			form.from(unmap)
		}

	private val defaultSelectForm :SQLReadForm[S] =
		if (nulls == null) backer.selectForm.nullTo(map)
		else backer.selectForm.to(map)

	private val defaultFilterForm :SQLWriteForm[S] = backer.filterForm.from(unmap)
	private val defaultInsertForm :SQLWriteForm[S] = backer.insertForm.from(unmap)
	private val defaultUpdateForm :SQLWriteForm[S] = backer.updateForm.from(unmap)

	override def selectForm :SQLReadForm[S] = defaultSelectForm
	override def filterForm :SQLWriteForm[S] = defaultFilterForm
	override def insertForm :SQLWriteForm[S] = defaultInsertForm
	override def updateForm :SQLWriteForm[S] = defaultUpdateForm
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :Component[X] =
		MappedMapping[RefinedMapping[T, O], T, X, O](backer, map andThen there, back andThen unmap)(
		                                             nullValue extract there)


	override def toString :String = "Mapped(" + backer  + ")"

}






object MappedMapping {

	def apply[M <: RefinedMapping[T, O], T, S, O](mapping :M, mapped :T =?> S, unmapped :S =?> T)
	                                             (implicit nulls :NullValue[S] = null) :M MappedTo S =
		new MappedMappingAdapter[M, T, S, O](mapping, mapped, unmapped)

	def map[M <: RefinedMapping[T, O], T, S, O](mapping :M, mapped :T => S, unmapped :S => T)
	                                           (implicit nulls :NullValue[S] = null) :M MappedTo S =
		apply[M, T, S, O](mapping, Extractor.req(mapped), Extractor.req(unmapped))


	def opt[M <: RefinedMapping[T, O], T, S, O](mapping :M, mapped :T => Option[S], unmapped :S => Option[T])
	                                           (implicit nulls :NullValue[S] = null) :M MappedTo S =
		apply[M, T, S, O](mapping, Extractor.opt(mapped), Extractor.opt(unmapped))



	def adapter[M <: MappingAt[O], T, S, O](mapping :MappingAdapter[M, T, O], mapped: T =?> S, unmapped :S =?> T)
	                                       (implicit nulls :NullValue[S] = null) :M MappedTo S =
		new MappedAdapterAdapter[M, T, S, O](mapping, mapped, unmapped)

	def mapAdapter[M <: MappingAt[O], T, S, O](mapping :MappingAdapter[M, T, O], mapped :T => S, unmapped :S => T)
	                                          (implicit nulls :NullValue[S] = null) :M MappedTo S =
		adapter(mapping, mapped, unmapped)

	def optAdapter[M <: MappingAt[O], T, S, O]
	              (mapping :MappingAdapter[M, T, O], mapped :T => Option[S], unmapped :S => Option[T])
	              (implicit nulls :NullValue[S] = null) :M MappedTo S =
		adapter(mapping, mapped, unmapped)



	def column[M <: ColumnMapping[T, O], T, S, O](column :M, mapped :T =?> S, unmapped :S =?> T)
	                                             (implicit nulls :NullValue[S] = null) :ColumnAdapter[M, T, S, O] =
		new MappedColumnMapping[M, T, S, O](column, mapped, unmapped)
			with DelegateAdapter[M, S, O] with ColumnAdapter[M, T, S, O]
		{ outer =>
			override def columnExtracts = super[MappedColumnMapping].columnExtracts
			override def extracts = super[MappedColumnMapping].extracts

			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, T, X, O] =
				MappedMapping.column[M, T, X, O](column, mapped andThen there, back andThen unmapped)
		}

	def columnAdapter[M <: ColumnMapping[T, O], T, Q, S, O]
	                 (column :ColumnAdapter[M, T, Q, O], mapped :Q =?> S, unmapped :S =?> Q)
	                 (implicit nulls :NullValue[S] = null) :ColumnAdapter[M, T, S, O] =
		new MappedColumnMapping[ColumnAdapter[M, T, Q, O], Q, S, O](column, mapped, unmapped)
			with ComposedAdapter[M, Q, S, O] with ColumnAdapter[M, T, S, O]
		{ outer =>
			override def columnExtracts = super[MappedColumnMapping].columnExtracts
			override def extracts = super[MappedColumnMapping].extracts

			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, T, X, O] =
				MappedMapping.columnAdapter[M, T, Q, X, O](backer, mapped andThen there, back andThen unmapped)
		}



	//consider: some better name distinction between the two, we might need them to be public
	private class MappedMappingAdapter[+M <: RefinedMapping[T, O], T, S, O]
	                                  (protected override val backer :M,
	                                   protected override val map :T =?> S, protected override val unmap :S =?> T)
	                                  (implicit protected override val nulls :NullValue[S] = null)
		extends MappedMapping[T, S, O] with DelegateAdapter[M, S, O]
	{
		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping[M, T, X, O](backer, map andThen there, back andThen unmap)(nullValue extract there)
	}


	private class MappedAdapterAdapter[+M <: MappingAt[O], T, S, O]
	                                  (protected override val backer :MappingAdapter[M, T, O],
	                                   protected override val map :T =?> S, protected override val unmap :S =?> T)
	                                  (implicit protected override val nulls :NullValue[S] = null)
		extends MappedMapping[T, S, O] with ComposedAdapter[M, T, S, O]
	{
		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(backer, map andThen there, back andThen unmap)(nullValue extract there)
	}


	abstract class MappedColumnMapping[+M <: ColumnMapping[T, O], T, S, O]
	                                  (protected override val backer :M,
	                                   protected val map :T =?> S, protected val unmap :S =?> T)
	                                  (implicit nulls :NullValue[S] = null)
		extends ShallowDelegate[S, O]
	{ this :ColumnMapping[S, O] =>
		override val name :String = backer.name
		override val form :ColumnForm[S] = backer.form.as(map)(unmap)

		private[this] val mappedExtracts :ColumnExtractMap =
			schema.composeColumnExtracts(backer, unmap).updated[ColumnExtract, T](backer, ColumnExtract(backer)(unmap))
				.updated[ColumnExtract, S](this, ColumnExtract.ident(this))

		override def columnExtracts :ColumnExtractMap = mappedExtracts
		//this cast can possibly fail with custom NaturalMap implementations!
		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]
	}


}

