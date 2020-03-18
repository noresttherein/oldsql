package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Buff.ExplicitSelect
import net.noresttherein.oldsql.schema.{AbstractMapping, Buff, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentSelector}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter
import net.noresttherein.oldsql.slang._



trait OptionMapping[M <: Component[O, S], O, S] extends AbstractMapping[O, Option[S]] {
	val get :M
}



object OptionMapping {

	def singleton[O, S](mapping :Component[O, S]) :OptionMapping[mapping.type, O, S] =
		apply(mapping)


	def apply[M <: Component[O, S], O, S](mapping :M) :OptionMapping[M, O, S] =
		new DirectOptionMapping[M, O, S](mapping)



	class DirectOptionMapping[M <: Component[O, S], O, S](val adaptee :M)
		extends OptionMapping[M, O, S] with MappingAdapter[M, O, S, Option[S]]
	{ box =>
		val get :M = adaptee

		override val components :Unique[Component[_]] = Unique(get) //todo: unify the Component types to use AnyMaping
		override val subcomponents :Unique[Component[_]] = get +: get.subcomponents

		override def selectForm :SQLReadForm[Option[S]] = get.selectForm.asOpt
		override def queryForm :SQLWriteForm[Option[S]] = get.queryForm.asOpt
		override def updateForm :SQLWriteForm[Option[S]] = get.updateForm.asOpt
		override def insertForm :SQLWriteForm[Option[S]] = get.insertForm.asOpt



		override lazy val buffs :Seq[Buff[Option[S]]] =
			get.buffs.map(_.map(Option(_))) ++ (ExplicitSelect.enabled(get) ifTrue ExplicitSelect[Option[S]](None))


		private def getSelector :Selector[S] = ComponentSelector(this, get)(Extractor.fromOpt)

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq get)
				getSelector.asInstanceOf[Selector[T]]
			else {
				val selector = get(component)
				ComponentSelector(this, selector.lifted)(Extractor.fromOpt[S] andThen selector.extractor)
			}

		override def assemble(values: Pieces): Option[Option[S]] = Some(values.get(getSelector))

		override val nullValue = Some(None)


		override def sqlName :Option[String] = get.sqlName

		override def toString :String = "Option[" + get + "]"
	}

}


