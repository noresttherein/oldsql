package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{Buff, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AdaptedAs, ShallowAdapter}
import net.noresttherein.oldsql.schema.Buff.BuffMappingFailureException

import scala.util.Try






trait MappedMapping[+M <: Mapping.Component[S, O], S, T, O] extends ShallowAdapter[M, S, T, O] {
	implicit protected def nulls :NullValue[T]
	protected def map :S => T
	protected def unmap :T => S

	override def apply[X](component :Component[X]) :Selector[X] =
		if (component eq egg)
			eggSelector.asInstanceOf[Selector[X]]
		else
			ComponentExtractor(component)(egg(component) compose unmap)

	private[this] val eggSelector = ComponentExtractor.req(egg)(unmap) :Selector[S]



	override val components :Unique[Component[_]] = Unique(egg)//egg.components

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] = {
		val form = egg.selectForm(if (components.contains(egg)) egg.selectable else components)
		if (nulls == null) form.mapNull(map) else form.map(map)
	}

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
		egg.queryForm(if (components.contains(egg)) egg.queryable else components).unmap(unmap)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
		egg.updateForm(if (components.contains(egg)) egg.updatable else components).unmap(unmap)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
		egg.insertForm(if (components.contains(egg)) egg.insertable else components).unmap(unmap)



	override val selectForm :SQLReadForm[T] =
		if (nulls == null) egg.selectForm.mapNull(map)
		else egg.selectForm.map(map)

	override val queryForm :SQLWriteForm[T] = egg.queryForm.unmap(unmap)
	override val updateForm :SQLWriteForm[T] = egg.updateForm.unmap(unmap)
	override val insertForm :SQLWriteForm[T] = egg.insertForm.unmap(unmap)


	override val buffs :Seq[Buff[T]] = egg.buffs.map(_.bimap(map, unmap)) //todo: should buffs use the NullValue?



	override def assemble(values :Pieces) :Option[T] = values.get(eggSelector).map(map)

	override def nullValue :Option[T] =
		if (nulls != null) nulls.toOption
		else Try { egg.nullValue.map(map) }.toOption.flatten

	override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping[M, S, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))

	override def flatMap[X](there :T => Option[X], back :X => Option[T])
	                       (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		MappedMapping.opt[M, S, X, O](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))



	protected def mapNulls[X](there :T => X)(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else if (this.nulls != null) this.nulls.map(there)
		else null

	protected def flatMapNulls[X](there :T => Option[X])(implicit nulls :NullValue[X]) :NullValue[X] =
		if (nulls != null) nulls
		else if (this.nulls != null) this.nulls.flatMap(there)
		else null


	override def toString :String = "Mapped(" + egg  + ")"
}






object MappedMapping {

	def apply[M <: Component[S, O], S, T, O](mapping :M, mapped :S => T, unmapped :T => S)
	                                        (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new MappingBijection[M, S, T, O](mapping, mapped, unmapped)


	def opt[M <: Component[S, O], S, T, O](mapping :M, mapped :S => Option[T], unmapped :T => Option[S])
	                                      (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new FlatMappedMapping[M, S, T, O](mapping, mapped, unmapped, nulls)






	private class MappingBijection[M <: Mapping.Component[S, O], S, T, O]
	                              (override val egg :M, override val map :S => T, override val unmap :T => S)
	                              (implicit override val nulls :NullValue[T] = null)
		extends MappedMapping[M, S, T, O] with MappingAdapter[M, T, O] //AdaptedAs[M, T]



	class FlatMappedMapping[+M <: Mapping.Component[S, O], S, T, O]
	                       (override val egg :M,
	                        protected final val map :S => Option[T],
	                        protected final val unmap :T => Option[S],
	                        onNone :NullValue[T] = null)
		extends ShallowAdapter[M, S, T, O] with MappingAdapter[M, T, O] //AdaptedAs[M, T]
	{
		implicit protected val nulls :NullValue[T] =
			if (onNone != null)
				onNone
			else
	            Try { egg.nullValue.flatMap(map) }.toOption.flatten.map(NullValue.apply[T] _) getOrElse
					NullValue.eval[T] {
						throw new IllegalArgumentException(
							s"No NullValue provided for $this and None after mapping ${egg.nullValue}."
						)
					}

		override def nullValue :Option[T] = nulls.toOption

		override val components :Unique[Component[_]] = Unique(egg)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] =
			if (components.contains(egg)) egg.selectForm(egg.selectable).flatMap(map)
			else egg.selectForm(components).flatMap(map)

		override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
			if (components.contains(egg)) egg.queryForm(egg.queryable).flatUnmap(unmap)
			else egg.queryForm(components).flatUnmap(unmap)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
			if (components.contains(egg)) egg.updateForm(egg.updatable).flatUnmap(unmap)
			else egg.updateForm(components).flatUnmap(unmap)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[T] =
			if (components.contains(egg)) egg.insertForm(egg.insertable).flatUnmap(unmap)
			else egg.insertForm(components).flatUnmap(unmap)

		

		override val selectForm :SQLReadForm[T] = egg.selectForm.flatMap(map)
		override val queryForm :SQLWriteForm[T] = egg.queryForm.flatUnmap(unmap)
		override val updateForm :SQLWriteForm[T] = egg.updateForm.flatUnmap(unmap)
		override val insertForm :SQLWriteForm[T] = egg.insertForm.flatUnmap(unmap)


		override val buffs :Seq[Buff[T]] = egg.buffs.map(buff => buff.bimap(
			s => map(s).getOrElse { throw new BuffMappingFailureException(
					s"Failed mapping of $egg: could not derive the value for the buff $buff from $s."
				)},
			(t :T) => unmap(t) getOrElse { throw new BuffMappingFailureException(
					s"Failed mapping of $egg: could not unmap value $t as part of the buff $buff."
				)}
		))



		override def apply[X](component :Component[X]) :Selector[X] =
			if (component eq egg)
				eggSelector.asInstanceOf[Selector[X]]
			else
				ComponentExtractor(component)(egg(component) compose Extractor(unmap))

		private[this] val eggSelector = ComponentExtractor.opt(egg)(unmap) :Selector[S]


		override def assemble(values :Pieces) :Option[T] = values.get(eggSelector).flatMap(map)



		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			new FlatMappedMapping[M, S, X, O](egg, map(_) map there, back andThen unmap, mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
			new FlatMappedMapping[M, S, X, O](egg, map(_) flatMap there, back(_) flatMap unmap, flatMapNulls(there))


		protected def mapNulls[X](there :T => X)(implicit nulls :NullValue[X]) :NullValue[X] =
			if (nulls != null) nulls else this.nulls.map(there)

		protected def flatMapNulls[X](there :T => Option[X])(implicit nulls :NullValue[X]) :NullValue[X] =
			if (nulls != null) nulls else this.nulls.flatMap(there)



		override def toString :String = "Mapped(" + egg  + ")"

	}






/*
	class MappedColumnMapping[M <: ColumnMapping[O, S], O, S, T]
	                         (override val egg :M, override val map :S => T, override val unmap :T => S)
	                         (implicit override val nulls :NullValue[T] = null)
		extends MappedMapping[M, O, S, T] with MappingAdapter[M, O, T] with ColumnMapping[O, T]
	{
		override def name :String = egg.name

		override val form :ColumnForm[T] =
			if (nulls != null) egg.form.bimap(map)(unmap)
			else egg.form.bimapNull(map)(unmap)

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedColumnMapping[M, O, S, X] =
			new MappedColumnMapping[M, O, S, X](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	class FlatMappedColumnMapping[M <: ColumnMapping[O, S], O, S, T]
	                             (col :M, there :S => Option[T], back :T => Option[S])
	                             (implicit nullValue :NullValue[T] = null)
		extends FlatMappedMapping[M, O, S, T](col, there, back) with ColumnMapping[O, T]
	{
		override def name :String = egg.name

		override val form :ColumnForm[T] =
			if (nulls != null) egg.form.biflatMap(map)(unmap)
			else egg.form.biflatMapNull(map)(unmap)

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedColumnMapping[M, O, S, X] =
			new FlatMappedColumnMapping[M, O, S, X](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))

	}
*/



}

