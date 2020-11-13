package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.schema.support.MappingAdapter.{ComposedAdapter, DelegateAdapter, MappedTo}
import net.noresttherein.oldsql.OperationType.WriteOperationType






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
	protected def map :T =?> S

	/** Function mapping the subject type `S`` of this mapping to the subject type `T` of the` adapted mapping,
	  * used by write forms. It is used in the constructor body of this trait and as such '''must''' be overriden
	  * with either a method or a constructor parameter `val` or `NullPointerException` will be thrown from
	  * the constructor.
	  */
	protected def unmap :S =?> T

	if (map == null || unmap == null)
		throw new IllegalStateException(s"MappedMapping map ($map) or unmap ($unmap) extract is null; overrides " +
			                             "must happen with a def or a constructor parameter val.")


	override def apply[X](component :Component[X]) :Extract[X] =
		if (component eq backer) backerExtract.asInstanceOf[Extract[X]]
		else extracts(component)

	override def apply[X](column :Column[X]) :ColumnExtract[X] =
		if (column eq backer) backerExtract.asInstanceOf[ColumnExtract[X]]
		else columnExtracts(column)


	private[this] val mapFun = map.requisite.orNull
	private[this] val flatMapFun = map.optional
	private[this] val backerExtract :Extract[T] = MappingExtract(backer)(unmap)

	override val extracts :NaturalMap[Component, Extract] =
		schema.composeExtracts(backer, backerExtract).updated(backer, backerExtract)

	override val columnExtracts :NaturalMap[Column, ColumnExtract] = //we could shortcut it if backer is a column,
		schema.composeColumnExtracts(backer, backerExtract)          //but for the ColumnLabel which has *two* components


	override val components :Unique[Component[_]] = Unique.single(backer)//backer.components
	override val subcomponents :Unique[Component[_]] = backer +: backer.subcomponents


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		//fixme: not selectable, default select form
		val form = backer.selectForm(if (components.contains(backer)) backer.selectable else components)
		if (nulls == null) form.nullTo(map) else form.to(map)
	}

//	override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
//		val form = backer.filterForm(if (components.contains(backer)) backer.filterable else components)
//		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
//	}
//
//	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
//		val form = backer.updateForm(if (components.contains(backer)) backer.updatable else components)
//		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
//	}
//
//	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
//		val form = backer.insertForm(if (components.contains(backer)) backer.insertable else components)
//		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
//	}

	override def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = backer.writeForm(op, if (components.contains(backer)) backer.insertable else components)
		form.from(unmap)
	}


	override val selectForm :SQLReadForm[S] =
		if (nulls == null) backer.selectForm.nullTo(map)
		else backer.selectForm.to(map)

	override val filterForm :SQLWriteForm[S] = backer.filterForm.from(unmap)
	override val updateForm :SQLWriteForm[S] = backer.updateForm.from(unmap)
	override val insertForm :SQLWriteForm[S] = backer.insertForm.from(unmap)
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)



	override def assemble(values :Pieces) :Option[S] =
		if (mapFun == null) values.get(backerExtract).flatMap(flatMapFun)
		else values.get(backerExtract).map(mapFun)

	override def optionally(values :Pieces) :Option[S] = values.assemble(this)



	/** The subject value `S` that database `null`s should map to, used by the read forms. It is initialized
	  * with the `nulls` parameter of the `Mapping.optMap` method creating `MappedMapping` instances. If the latter
	  * is `null` (the field, not the wrapped value) it defaults to mapping the `nullValue` of the adapted mapping
	  * with `map`.
	  */
	implicit override val nullValue :NullValue[S] =
		if (nulls != null) nulls
		else if (mapFun == null) backer.nullValue.flatMap(map.optional)
		else backer.nullValue.map(mapFun)

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

}

