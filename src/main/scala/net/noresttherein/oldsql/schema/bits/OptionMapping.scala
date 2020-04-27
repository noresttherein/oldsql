package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Buff.ExplicitSelect
import net.noresttherein.oldsql.schema.{Buff, ComponentExtractor, GenericMapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.slang._



trait OptionMapping[M <: TypedMapping[S, O], S, O] extends GenericMapping[Option[S], O] {
	val get :M
}



object OptionMapping {

	def singleton[S, O](mapping :TypedMapping[S, O]) :OptionMapping[mapping.type, S, O] =
		apply(mapping)


	def apply[M <: TypedMapping[S, O], S, O](mapping :M) :OptionMapping[M, S, O] =
		new DirectOptionMapping[M, S, O](mapping)



	class DirectOptionMapping[M <: TypedMapping[S, O], S, O](val egg :M)
		extends OptionMapping[M, S, O] with ShallowAdapter[M, S, Option[S], O]
	{ box =>
		val get :M = egg

		override val components :Unique[Component[_]] = Unique(get)
		override val subcomponents :Unique[Component[_]] = get +: get.subcomponents


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Option[S]] =
			get.selectForm(if (components.contains(get)) get.selectable else components).asOpt

		override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.queryForm(if (components.contains(get)) get.queryable else components).asOpt

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.updateForm(if (components.contains(get)) get.updatable else components).asOpt

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.insertForm(if (components.contains(get)) get.insertable else components).asOpt

		override val selectForm :SQLReadForm[Option[S]] = get.selectForm.asOpt
		override val queryForm :SQLWriteForm[Option[S]] = get.queryForm.asOpt
		override val updateForm :SQLWriteForm[Option[S]] = get.updateForm.asOpt
		override val insertForm :SQLWriteForm[Option[S]] = get.insertForm.asOpt



		override lazy val buffs :Seq[Buff[Option[S]]] =
			get.buffs.map(_.bimap(Option(_), (_:Option[S]).get)) ++
				(ExplicitSelect.enabled(get) ifTrue ExplicitSelect[Option[S]](None))


		private val eggExtractor :Selector[S] = ComponentExtractor(get)(Extractor.fromOpt)

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq get)
				eggExtractor.asInstanceOf[Selector[T]]
			else {
				val selector = get(component)
				ComponentExtractor(selector.export)(Extractor.fromOpt[S] andThen selector)
			}

		override def apply[T](column :Column[T]) :ColumnSelector[T] =
			if (column eq get)
				eggExtractor.asInstanceOf[ColumnSelector[T]]
			else {
				val selector = get(column)
				ComponentExtractor(selector.export)(Extractor.fromOpt[S] andThen selector)
			}



		override def assemble(values: Pieces): Option[Option[S]] = Some(values.get(eggExtractor))

		override val nullValue = Some(None)


		override def sqlName :Option[String] = get.sqlName

		override def toString :String = "Option[" + get + "]"
	}

}


