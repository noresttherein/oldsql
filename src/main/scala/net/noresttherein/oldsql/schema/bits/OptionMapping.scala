package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{InverseIndexSeq, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Buff.ExplicitSelect
import net.noresttherein.oldsql.schema.{AnyMapping, Buff, Mapping, SQLReadForm, SQLWriteForm, SubMapping}
import net.noresttherein.oldsql.schema.Mapping.{Component, NarrowedMapping, ComponentSelector}
import net.noresttherein.oldsql.slang._



trait OptionMapping[M <: NarrowedMapping[O, S], O, S] extends SubMapping[O, Option[S]] {
	val get :M
}



object OptionMapping {

	def singleton[O, S](mapping :Component[O, S]) :OptionMapping[mapping.type, O, S] =
		apply(mapping)

//	def of[M <: AnyMapping](mapping :M) :OptionMapping[M, M#Owner, M#Subject] =
//		new DirectOptionMapping[M, M#Owner, M#Subject] {
//			override val get = mapping
//		}
//	def of[M <: AnyMapping] = new Factory[M] {}
	def apply[M <: NarrowedMapping[O, S], O, S](mapping :M) :OptionMapping[M, O, S] =
		new DirectOptionMapping[M, O, S] {
			override val get = mapping
		}



	trait DirectOptionMapping[M <: NarrowedMapping[O, S], O, S] extends OptionMapping[M, O, S] { box =>

		val get :M


		override def components :Unique[Component[_]] = Unique(get) //todo: unify the Component types to use AnyMaping
		override def subcomponents :Unique[Component[_]] = get +: get.subcomponents

		override def columns :Unique[Component[_]] = get.columns
		override def selectable :Unique[Component[_]] = get.selectable
		override def queryable :Unique[Component[_]] = get.queryable
		override def updatable :Unique[Component[_]] = get.updatable
		override def autoUpdated :Unique[Component[_]] = get.autoUpdated
		override def insertable :Unique[Component[_]] = get.insertable
		override def autoInserted :Unique[Component[_]] = get.autoInserted


		override def selectForm :SQLReadForm[Option[S]] = get.selectForm.asOpt
		override def queryForm :SQLWriteForm[Option[S]] = get.queryForm.asOpt
		override def updateForm :SQLWriteForm[Option[S]] = get.updateForm.asOpt
		override def insertForm :SQLWriteForm[Option[S]] = get.insertForm.asOpt



		override lazy val buffs :Seq[Buff[Option[S]]] =
			get.buffs.map(_.map(Option(_))) ++ (ExplicitSelect.enabled(get) ifTrue ExplicitSelect[Option[S]](None))


		private def getSelector :Selector[S] =
			ComponentSelector(this, get)(Extractor.fromOpt)

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq get)
				getSelector.asInstanceOf[Selector[T]]
//				ComponentSelector(this, component)(Extractor.fromOpt.asInstanceOf[Extractor[Option[S], T]])
			else {
				val selector = get(component)
				ComponentSelector(this, selector.lifted)(Extractor.fromOpt[S] andThen selector.extractor)
			}

		override def assemble(values: Values): Option[Option[S]] = Some(values.get(getSelector))

		override def nullValue = Some(None)


		override def sqlName :Option[String] = get.sqlName

	}

}


