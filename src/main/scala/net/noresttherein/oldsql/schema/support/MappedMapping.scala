package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.{AnyMapping, Buff, Mapping, SQLReadForm, SQLWriteForm, SubMapping}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentSelector, TypedMapping}



trait MappedAs[M <: AnyMapping, T] extends Mapping[T] {
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
		if (components.contains(adaptee)) adaptee.selectForm(selectable).map(map)
		else adaptee.selectForm(components).map(map)

	override val selectForm :SQLReadForm[T] = adaptee.selectForm.map(map)
	override val queryForm :SQLWriteForm[T] = adaptee.queryForm.unmap(unmap)
	override val updateForm :SQLWriteForm[T] = adaptee.updateForm.unmap(unmap)
	override val insertForm :SQLWriteForm[T] = adaptee.insertForm.unmap(unmap)


	override val buffs :Seq[Buff[T]] = adaptee.buffs.map(_.bimap(map, unmap))



	override def assemble(values :Values) :Option[T] = values.get(adapteeSelector).map(map)

	override def nullValue :Option[T] = adaptee.nullValue.map(map)



//	def map[X](there :T => X, back :X => T) :MappedAs[M, X] =
//		adaptee.map(map andThen there, back andThen unmap)


	override def toString :String = "Mapped(" + adaptee  + ")"
}






object MappedMapping {

	def apply[M <: Component[O, S], O, S, T](mapping :M, mapped :S => T, unmapped :T => S) :M MappedAs T =
		new MappedMapping[M, O, S, T](mapping, mapped, unmapped)

	def opt[M <: Component[O, S], O, S, T](mapping :M, mapped :S => Option[T], unmapped :T => Option[S]) :M MappedAs T =
		new OptionalMapping[M, O, S, T](mapping, mapped, unmapped)







	private class OptionalMapping[M <: Mapping.Component[O, S], O, S, T]
	                             (val adaptee :M, map :S => Option[T], unmap :T => Option[S])
		extends MappingAdapter[M, O, S, T] with SubMapping[O, T] with MappedAs[M, T]
	{

		override def apply[X](component :Component[X]) :Selector[X] =
			if (component eq adaptee)
				adapteeSelector.asInstanceOf[Selector[X]]
			else
				ComponentSelector(this, component)(adaptee(component).extractor compose Extractor(unmap))

		private[this] val adapteeSelector = ComponentSelector.opt(this, adaptee)(unmap) :Selector[S]



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



		override def assemble(values :Values) :Option[T] = values.get(adapteeSelector).flatMap(map)

		override val nullValue :Option[T] = adaptee.nullValue.flatMap(map)

		if (nullValue == null)
			throw new IllegalArgumentException(s"Can't map mapping $adaptee: no null value after mapping ${adaptee.nullValue}.")


//		override def map[X](there :T => X, back :X => T) :MappedAs[M, X] =
//			MappedMapping.opt[M, O, S, T](adaptee, map(_:S).map(there), back andThen unmap)
//
//		override def optmap[X](there :T => Option[X], back :X => Option[T]) :MappedAs[, X] =

		override def toString :String = "Mapped(" + adaptee  + ")"

	}



}

