package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{Mapping, AbstractMapping, Buff, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentSelector, ComponentFor}



trait MappedAs[M <: Mapping, T] extends AbstractMapping[M#Owner, T] {
	val adaptee :M
}






class MappedMapping[M <: Mapping.Component[O, S], O, S, T](override val adaptee :M, map :S => T, unmap :T => S)
	extends MappingAdapter[M, O, S, T] with MappedAs[M, T]
{

	override def apply[X](component :Component[X]) :Selector[X] =
		if (component eq adaptee)
			adapteeSelector.asInstanceOf[Selector[X]]
		else
			ComponentSelector(this, component)(adaptee(component).extractor compose unmap)

	private[this] val adapteeSelector = ComponentSelector.req(this, adaptee)(unmap) :Selector[S]



	override val components :Unique[Component[_]] = Unique(adaptee)//adaptee.components

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] =
		if (components.contains(adaptee)) adaptee.selectForm(selectable).mapNull(map)
		else adaptee.selectForm(components).mapNull(map)

	override val selectForm :SQLReadForm[T] = adaptee.selectForm.mapNull(map)
	override val queryForm :SQLWriteForm[T] = adaptee.queryForm.unmap(unmap)
	override val updateForm :SQLWriteForm[T] = adaptee.updateForm.unmap(unmap)
	override val insertForm :SQLWriteForm[T] = adaptee.insertForm.unmap(unmap)


	override val buffs :Seq[Buff[T]] = adaptee.buffs.map(_.bimap(map, unmap))



	override def assemble(values :Pieces) :Option[T] = values.get(adapteeSelector).map(map)

	override def nullValue :Option[T] = adaptee.nullValue.map(map)



	override def toString :String = "Mapped(" + adaptee  + ")"
}






object MappedMapping {

	def apply[M <: Component[O, S], O, S, T](mapping :M, mapped :S => T, unmapped :T => S) :M MappedAs T =
		new MappedMapping[M, O, S, T](mapping, mapped, unmapped)

	def opt[M <: Component[O, S], O, S, T](mapping :M, mapped :S => Option[T], unmapped :T => Option[S]) :M MappedAs T =
		new OptionalMapping[M, O, S, T](mapping, mapped, unmapped)







	private class OptionalMapping[M <: Mapping.Component[O, S], O, S, T]
	                             (val adaptee :M, map :S => Option[T], unmap :T => Option[S])
		extends MappingAdapter[M, O, S, T] with AbstractMapping[O, T] with MappedAs[M, T]
	{




		override val nullValue :Option[T] = adaptee.nullValue.flatMap(map)

		if (nullValue == null)
			throw new IllegalArgumentException(s"Can't map mapping $adaptee: no null value after mapping ${adaptee.nullValue}.")

		override val components :Unique[Component[_]] = Unique(adaptee)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[T] =
			if (components.contains(adaptee)) adaptee.selectForm(selectable).flatMap(map, nullValue.get)
			else adaptee.selectForm(components).flatMap(map, nullValue.get)

		override val selectForm :SQLReadForm[T] = adaptee.selectForm.flatMap(map, nullValue.get)
		override val queryForm :SQLWriteForm[T] = adaptee.queryForm.flatUnmap(unmap)
		override val updateForm :SQLWriteForm[T] = adaptee.updateForm.flatUnmap(unmap)
		override val insertForm :SQLWriteForm[T] = adaptee.insertForm.flatUnmap(unmap)


		override val buffs :Seq[Buff[T]] = adaptee.buffs.map(buff => buff.bimap(
			s => map(s).getOrElse { throw new IllegalStateException(
				s"Failed mapping of $adaptee: could not derive the value for the buff $buff from $s."
				)},
			(t :T) => unmap(t) getOrElse { throw new IllegalStateException(
				s"Failed mapping of $adaptee: could not unmap value $t as part of the buff $buff."
				)}
		))



		override def apply[X](component :Component[X]) :Selector[X] =
			if (component eq adaptee)
				adapteeSelector.asInstanceOf[Selector[X]]
			else
				ComponentSelector(this, component)(adaptee(component).extractor compose Extractor(unmap))

		private[this] val adapteeSelector = ComponentSelector.opt(this, adaptee)(unmap) :Selector[S]


		override def assemble(values :Pieces) :Option[T] = values.get(adapteeSelector).flatMap(map)



		override def toString :String = "Mapped(" + adaptee  + ")"

	}



}

