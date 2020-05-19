package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AdaptedAs, ShallowAdapter}
import net.noresttherein.oldsql.schema.support.MappingAdapter

import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>





trait MappedMapping[+M <: Mapping.RefinedMapping[T, O], T, S, O] extends ShallowAdapter[M, T, S, O] {

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
		if (component eq egg)
			eggExtract.asInstanceOf[Extract[X]]
		else
			MappingExtract(component)(egg(component) compose unmap)

	override def apply[X](column :Column[X]) :ColumnExtract[S, X, O] =
		if (column eq egg)
			eggExtract.asInstanceOf[ColumnExtract[S, X, O]]
		else
			MappingExtract(column)(egg(column) compose unmap)
*/

	if (map == null || unmap == null)
		throw new IllegalStateException(s"MappedMapping map ($map) or unmap ($unmap) extract is null; overrides " +
			                             "must happen with a def or a constructor parameter val.")

	private[this] val mapFun = map.requisite.orNull
	private[this] val unmapFun = unmap.requisite.orNull

	private[this] val eggExtract :Extract[T] = MappingExtract(egg)(unmap)

	override val extracts :NaturalMap[Component, Extract] =
		schema.composeExtracts(egg, eggExtract).updated(egg, eggExtract)

	override val columnExtracts :NaturalMap[Column, ColumnExtract] = egg match {
		case column :ColumnMapping[T @unchecked, O @unchecked] =>
			NaturalMap.single[Column, ColumnExtract, T](column, eggExtract.asInstanceOf[ColumnExtract[T]])
		case _ =>
			schema.composeColumnExtracts(egg, eggExtract)
	}



	override val components :Unique[Component[_]] = Unique(egg)//egg.components



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val form = egg.selectForm(if (components.contains(egg)) egg.selectable else components)
		if (nulls == null)
			if (mapFun == null) form.flatMapNull(map.optional) else form.mapNull(mapFun)
		else
			if (mapFun == null) form.flatMap(map.optional) else form.map(mapFun)
	}

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = egg.queryForm(if (components.contains(egg)) egg.queryable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = egg.updateForm(if (components.contains(egg)) egg.updatable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] = {
		val form = egg.insertForm(if (components.contains(egg)) egg.insertable else components)
		if (unmapFun == null) form.flatUnmap(unmap.optional) else form.unmap(unmapFun)
	}


	override val selectForm :SQLReadForm[S] =
		if (nulls == null)
			if (mapFun == null) egg.selectForm.flatMapNull(map.optional)
			else egg.selectForm.mapNull(mapFun)
		else
	        if (mapFun == null) egg.selectForm.flatMap(map.optional)
			else egg.selectForm.map(mapFun)

	override val queryForm :SQLWriteForm[S] =
		if (unmapFun == null) egg.queryForm.flatUnmap(unmap.optional) else egg.queryForm.unmap(unmapFun)

	override val updateForm :SQLWriteForm[S] =
		if (unmapFun == null) egg.updateForm.flatUnmap(unmap.optional) else egg.updateForm.unmap(unmapFun)

	override val insertForm :SQLWriteForm[S] =
		if (unmapFun == null) egg.insertForm.flatUnmap(unmap.optional) else egg.insertForm.unmap(unmapFun)



	override val buffs :Seq[Buff[S]] = schema.mapBuffs(egg)(map, unmap) //consider: should buffs use the NullValue?



	override def assemble(values :Pieces) :Option[S] =
		if (mapFun == null) values.get(eggExtract).flatMap(map.optional)
		else values.get(eggExtract).map(mapFun)



	/** The subject value `S` that database `null`s should map to, used by the read forms. It is initialized
	  * with the `nulls` parameter of the `Mapping.map` method creating `MappedMapping` instances. If the latter
	  * is `null` (the field, not the wrapped value) it defaults to mapping the `nullValue` of the adapted mapping
	  * with `map`.
	  */
	implicit override val nullValue :NullValue[S] =
		if (nulls != null) nulls
		else if (mapFun == null) egg.nullValue.flatMap(map.optional)
		else egg.nullValue.map(mapFun)



	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping[M, T, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))

	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping[M, T, X, O](egg, map andThen there, unmap compose back)(mapNulls(there))

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping[M, T, X, O](egg, map andThen Extractor(there), unmap composeOpt back)(flatMapNulls(there))



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



	override def toString :String = "Mapped(" + egg  + ")"

}






object MappedMapping {

	def apply[M <: RefinedMapping[S, O], S, T, O](mapping :M, mapped :S =?> T, unmapped :T =?> S)
	                                             (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new MappedMappingAdapter[M, S, T, O](mapping, mapped, unmapped)

	def sure[M <: RefinedMapping[S, O], S, T, O](mapping :M, mapped :S => T, unmapped :T => S)
	                                            (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		apply[M, S, T, O](mapping, Extractor.req(mapped), Extractor.req(unmapped))


	def opt[M <: RefinedMapping[S, O], S, T, O](mapping :M, mapped :S => Option[T], unmapped :T => Option[S])
	                                           (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		apply[M, S, T, O](mapping, Extractor.opt(mapped), Extractor.opt(unmapped))






	class MappedMappingAdapter[M <: RefinedMapping[T, O], T, S, O]
	                          (override val egg :M, override val map :T =?> S, override val unmap :S =?> T)
	                          (implicit override val nulls :NullValue[S] = null)
		extends MappedMapping[M, T, S, O] with MappingAdapter[M, S, O]


}

