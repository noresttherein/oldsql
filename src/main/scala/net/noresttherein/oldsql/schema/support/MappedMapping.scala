package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{GenericMapping, Buff, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue



trait MappedAs[M <: Mapping, T] extends GenericMapping[M#Owner, T] with MappingNest[M] {
	override val egg :M
}






class MappedMapping[M <: Mapping.Component[O, S], O, S, T](override val egg :M, map :S => T, unmap :T => S)
	extends MappingAdapter[M, O, S, T] with MappedAs[M, T]
{

	override def apply[X](component :Component[X]) :Selector[X] =
		if (component eq egg)
			eggSelector.asInstanceOf[Selector[X]]
		else
			ComponentExtractor(component)(egg(component) compose unmap)

	private[this] val eggSelector = ComponentExtractor.req(egg)(unmap) :Selector[S]



	override val components :Unique[Component[_]] = Unique(egg)//egg.components

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] =
		if (components.contains(egg)) egg.selectForm(selectable).mapNull(map)
		else egg.selectForm(components).mapNull(map)

	override val selectForm :SQLReadForm[T] = egg.selectForm.mapNull(map)
	override val queryForm :SQLWriteForm[T] = egg.queryForm.unmap(unmap)
	override val updateForm :SQLWriteForm[T] = egg.updateForm.unmap(unmap)
	override val insertForm :SQLWriteForm[T] = egg.insertForm.unmap(unmap)


	override val buffs :Seq[Buff[T]] = egg.buffs.map(_.bimap(map, unmap))



	override def assemble(values :Pieces) :Option[T] = values.get(eggSelector).map(map)

	override def nullValue :Option[T] = egg.nullValue.map(map)



	override def toString :String = "Mapped(" + egg  + ")"
}






object MappedMapping {

	def apply[M <: Component[O, S], O, S, T](mapping :M, mapped :S => T, unmapped :T => S) :M MappedAs T =
		new MappedMapping[M, O, S, T](mapping, mapped, unmapped)

	def nullable[M <: Component[O, S], O, S, T :NullValue](mapping :M, mapped :S => T, unmapped :T => S) :M MappedAs T =
		new NullMappedMapping[M, O, S, T](mapping, mapped, unmapped)

	def opt[M <: Component[O, S], O, S, T](mapping :M, mapped :S => Option[T], unmapped :T => Option[S]) :M MappedAs T =
		new OptionalMapping[M, O, S, T](mapping, mapped, unmapped)(
			NullValue[T](mapping.nullValue.flatMap(mapped) getOrElse {
				throw new IllegalArgumentException(
					s"Can't map mapping $mapping: no null value after mapping ${mapping.nullValue}."
				)
			})
		)

	def nullableOpt[M <: Component[O, S], O, S, T :NullValue](mapping :M, mapped :S => Option[T], unmapped :T => Option[S]) :M MappedAs T =
		new OptionalMapping[M, O, S, T](mapping, mapped, unmapped)





	private class NullMappedMapping[M <: Mapping.Component[O, S], O, S, T](mapping :M, map :S => T, unmap :T => S)
	                                                                      (implicit nulls :NullValue[T])
		extends MappedMapping[M, O, S, T](mapping, map, unmap)
	{
		override def nullValue :Option[T] = Some(nulls.value)

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] =
			if (components.contains(egg)) egg.selectForm(selectable).map(map)
			else egg.selectForm(components).map(map)

		override val selectForm :SQLReadForm[T] = egg.selectForm.map(this.map)
	}



	private class OptionalMapping[M <: Mapping.Component[O, S], O, S, T]
	                             (val egg :M, map :S => Option[T], unmap :T => Option[S])(implicit nulls :NullValue[T])
		extends MappingAdapter[M, O, S, T] with GenericMapping[O, T] with MappedAs[M, T]
	{




		override def nullValue :Option[T] = Some(nulls.value)

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



		override def toString :String = "Mapped(" + egg  + ")"

	}



}

