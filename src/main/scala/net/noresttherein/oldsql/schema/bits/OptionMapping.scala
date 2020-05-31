package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Buff.ExplicitSelect
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, TypedMapping, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.slang._



trait OptionMapping[M <: RefinedMapping[S, O], S, O] extends TypedMapping[Option[S], O] {
	val get :M
}



object OptionMapping {

	def singleton[S, O](mapping :RefinedMapping[S, O]) :OptionMapping[mapping.type, S, O] =
		apply(mapping)


	def apply[M <: RefinedMapping[S, O], S, O](mapping :M) :OptionMapping[M, S, O] =
		new DirectOptionMapping[M, S, O](mapping)



	class DirectOptionMapping[M <: RefinedMapping[S, O], S, O](val egg :M)
		extends OptionMapping[M, S, O] with ShallowAdapter[M, S, Option[S], O]
	{ box =>
		val get :M = egg



		override def assemble(values: Pieces): Option[Option[S]] = Some(values.get(eggExtract))

		override def nullValue :NullValue[Option[S]] = NullValue.None



		private val eggExtract :Extract[S] = MappingExtract(get)(Extractor.fromOpt)

/*
		override def apply[T](component :Component[T]) :Extract[T] =
			if (component eq get)
				eggExtract.asInstanceOf[Extract[T]]
			else {
				val selector = get(component)
				MappingExtract(selector.export)(Extractor.fromOpt[S] andThen selector)
			}

		override def apply[T](column :Column[T]) :ColumnExtract[T] =
			if (column eq get)
				eggExtract.asInstanceOf[ColumnExtract[T]]
			else {
				val selector = get(column)
				MappingExtract(selector.export)(Extractor.fromOpt[S] andThen selector)
			}
*/
		override val extracts :NaturalMap[Component, Extract] =
			egg.extracts.map(schema.composeExtractAssoc(eggExtract)(_)).updated(egg, eggExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] = egg match {
			case column :ColumnMapping[S @unchecked, O @unchecked] =>
				NaturalMap.single[Column, ColumnExtract, S](column, eggExtract.asInstanceOf[ColumnExtract[S]])
			case _ =>
				egg.columnExtracts.map(schema.composeColumnExtractAssoc(eggExtract)(_))
		}



		override val components :Unique[Component[_]] = Unique(get)
		override val subcomponents :Unique[Component[_]] = get +: get.subcomponents


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Option[S]] =
			get.selectForm(if (components.contains(get)) get.selectable else components).toOpt

		override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.queryForm(if (components.contains(get)) get.queryable else components).toOpt

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.updateForm(if (components.contains(get)) get.updatable else components).toOpt

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Option[S]] =
			get.insertForm(if (components.contains(get)) get.insertable else components).toOpt

		override val selectForm :SQLReadForm[Option[S]] = get.selectForm.toOpt
		override val queryForm :SQLWriteForm[Option[S]] = get.queryForm.toOpt
		override val updateForm :SQLWriteForm[Option[S]] = get.updateForm.toOpt
		override val insertForm :SQLWriteForm[Option[S]] = get.insertForm.toOpt



		override lazy val buffs :Seq[Buff[Option[S]]] =
			get.buffs.map(_.bimap(Option(_), (_:Option[S]).get)) ++
				(ExplicitSelect.enabled(get) ifTrue ExplicitSelect[Option[S]](None))


		override def sqlName :Option[String] = get.sqlName

		override def toString :String = "Option[" + get + "]"
	}

}


