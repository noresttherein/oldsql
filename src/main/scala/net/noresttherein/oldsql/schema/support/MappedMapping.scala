package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{Buff, GenericMapping, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AdaptedAs, ShallowAdapter}

import scala.util.Try






trait MappedMapping[M <: Mapping.Component[O, S], O, S, T]
	extends ShallowAdapter[M, O, S, T]
{
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

	override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :M AdaptedAs X = {
		val newNull =
			if (nulls != null) nulls
			else if (this.nulls != null) this.nulls.map(there)
			else null
		MappedMapping[M, O, S, X](egg, map andThen there, back andThen unmap)(newNull)
	}

	override def flatMap[X](there :T => Option[X], back :X => Option[T])
	                       (implicit nulls :NullValue[X] = null) :M AdaptedAs X =
	{
		val newNull =
			if (nulls != null) nulls
			else if (this.nulls != null) this.nulls.flatMap(there)
			else null
		MappedMapping.opt[M, O, S, X](egg, map andThen there, back(_) map unmap)(newNull)
	}



	override def toString :String = "Mapped(" + egg  + ")"
}






object MappedMapping {

	def apply[M <: Component[O, S], O, S, T](mapping :M, mapped :S => T, unmapped :T => S)
	                                        (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new MappingBijection[M, O, S, T](mapping, mapped, unmapped)


	def opt[M <: Component[O, S], O, S, T](mapping :M, mapped :S => Option[T], unmapped :T => Option[S])
	                                      (implicit nulls :NullValue[T] = null) :M AdaptedAs T =
		new FlatMappedMapping[M, O, S, T](mapping, mapped, unmapped, nulls)






	private class MappingBijection[M <: Mapping.Component[O, S], O, S, T]
	                              (override val egg :M, override val map :S => T, override val unmap :T => S)
	                              (implicit override val nulls :NullValue[T] = null)
		extends MappedMapping[M, O, S, T] with MappingAdapter[M, O, T] //AdaptedAs[M, T]



	class FlatMappedMapping[M <: Mapping.Component[O, S], O, S, T]
	                       (override val egg :M,
	                        protected final val map :S => Option[T],
	                        protected final val unmap :T => Option[S],
	                        onNone :NullValue[T] = null)
		extends ShallowAdapter[M, O, S, T] with MappingAdapter[M, O, T] //AdaptedAs[M, T]
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
			if (components.contains(egg)) egg.selectForm(selectable).flatMap(map)
			else egg.selectForm(components).flatMap(map)

		override val selectForm :SQLReadForm[T] = egg.selectForm.flatMap(map)
		override val queryForm :SQLWriteForm[T] = egg.queryForm.flatUnmap(unmap)
		override val updateForm :SQLWriteForm[T] = egg.updateForm.flatUnmap(unmap)
		override val insertForm :SQLWriteForm[T] = egg.insertForm.flatUnmap(unmap)


		override val buffs :Seq[Buff[T]] = egg.buffs.map(buff => buff.bimap(
			s => map(s).getOrElse { throw new IllegalStateException(
				s"Failed mapping of $egg: could not derive the value for the buff $buff from $s."
				)},
			(t :T) => unmap(t) getOrElse { throw new IllegalStateException(
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



		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :M AdaptedAs X = {
			val newNull =
				if (nulls != null) nulls
				else this.nulls.map(there)
			new FlatMappedMapping[M, O, S, X](egg, map(_) map there, back andThen unmap, newNull)
		}

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :M AdaptedAs X =
		{
			val newNull =
				if (nulls != null) nulls
				else this.nulls.flatMap(there)
			new FlatMappedMapping[M, O, S, X](egg, map(_) flatMap there, back(_) flatMap unmap, newNull)
		}


		override def toString :String = "Mapped(" + egg  + ")"

	}



}

