package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{BaseAdapter, ComposedAdapter, DelegateAdapter, MappedTo}
import net.noresttherein.oldsql.schema.support.DelegateMapping





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



/*
	override def apply[X](component :Component[X]) :Extract[X] =
		if (component eq backer)
			eggExtract.asInstanceOf[Extract[X]]
		else
			MappingExtract(component)(backer(component) compose unmap)

	override def apply[X](column :Column[X]) :ColumnExtract[S, X, O] =
		if (column eq backer)
			eggExtract.asInstanceOf[ColumnExtract[S, X, O]]
		else
			MappingExtract(column)(backer(column) compose unmap)
*/

	if (map == null || unmap == null)
		throw new IllegalStateException(s"MappedMapping map ($map) or unmap ($unmap) extract is null; overrides " +
			                             "must happen with a def or a constructor parameter val.")

	private[this] val mapFun = map.requisite.orNull
	private[this] val unmapFun = unmap.requisite.orNull

	private[this] val eggExtract :Extract[T] = MappingExtract(backer)(unmap)

	override val extracts :NaturalMap[Component, Extract] =
		schema.composeExtracts(backer, eggExtract).updated(backer, eggExtract)

	override val columnExtracts :NaturalMap[Column, ColumnExtract] = backer match {
		case column :ColumnMapping[T @unchecked, O @unchecked] =>
			NaturalMap.single[Column, ColumnExtract, T](column, eggExtract.asInstanceOf[ColumnExtract[T]])
		case _ =>
			schema.composeColumnExtracts(backer, eggExtract)
	}



	override val components :Unique[Component[_]] = Unique.single(backer)//backer.components



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val form = backer.selectForm(if (components.contains(backer)) backer.selectable else components)
		if (nulls == null)
			if (mapFun == null) form.flatMapNull(map.optional) else form.mapNull(mapFun)
		else
			if (mapFun == null) form.flatMap(map.optional) else form.map(mapFun)
	}

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = backer.queryForm(if (components.contains(backer)) backer.queryable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = backer.updateForm(if (components.contains(backer)) backer.updatable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = backer.insertForm(if (components.contains(backer)) backer.insertable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}


	override val selectForm :SQLReadForm[S] =
		if (nulls == null)
			if (mapFun == null) backer.selectForm.flatMapNull(map.optional)
			else backer.selectForm.mapNull(mapFun)
		else
	        if (mapFun == null) backer.selectForm.flatMap(map.optional)
			else backer.selectForm.map(mapFun)

	override val queryForm :SQLWriteForm[S] =
		if (unmapFun == null) backer.queryForm.flatUnmap(unmap.optional) else backer.queryForm.unmap(unmapFun)

	override val updateForm :SQLWriteForm[S] =
		if (unmapFun == null) backer.updateForm.flatUnmap(unmap.optional) else backer.updateForm.unmap(unmapFun)

	override val insertForm :SQLWriteForm[S] =
		if (unmapFun == null) backer.insertForm.flatUnmap(unmap.optional) else backer.insertForm.unmap(unmapFun)



	override val buffs :Seq[Buff[S]] = schema.mapBuffs(backer)(map, unmap) //consider: should buffs use the NullValue?



	override def assemble(values :Pieces) :Option[S] =
		if (mapFun == null) values.get(eggExtract).flatMap(map.optional)
		else values.get(eggExtract).map(mapFun)



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
		MappedMapping[RefinedMapping[T, O], T, X, O](backer, map andThen there, back andThen unmap)(mapNulls(there))




	protected def mapNulls[X](there :S =?> X)(implicit nulls :NullValue[X]) :NullValue[X] = there.requisite match {
		case Some(req) => mapNulls(req)
		case _ => flatMapNulls(there.optional)
	}


	protected def mapNulls[X](there :S => X)(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else nullValue.map(there)

	protected def flatMapNulls[X](there :S => Option[X])(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else nullValue.flatMap(there)



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
		new MappedAdapter[M, T, S, O](mapping, mapped, unmapped)

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
			MappedMapping[M, T, X, O](backer, map andThen there, back andThen unmap)(mapNulls(there))
	}



	private class MappedAdapter[+M <: MappingAt[O], T, S, O]
	                           (protected override val backer :MappingAdapter[M, T, O],
	                            protected override val map :T =?> S, protected override val unmap :S =?> T)
	                           (implicit protected override val nulls :NullValue[S] = null)
		extends MappedMapping[T, S, O] with ComposedAdapter[M, T, S, O]
	{
		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(backer, map andThen there, back andThen unmap)(mapNulls(there))
	}

}

