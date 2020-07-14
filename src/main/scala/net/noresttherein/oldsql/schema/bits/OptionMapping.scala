package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.schema.Buff.ExplicitSelect
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, Mapping, MappingExtract, SQLReadForm, SQLWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.RefinedProjection
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms

//implicits
import net.noresttherein.oldsql.slang._


//todo: uncheckedVariance
sealed trait OptionMapping[+M <: Mapping, S, O] extends TypedMapping[Option[S], O] {
	val get :M
}



object OptionMapping {

	type Optional[M <: Mapping] = OptionMapping[M, M#Subject, M#Origin]

	type OptionAt[M[A] <: MappingAt[A], O] = OptionMapping[M[O], M[O]#Subject, O]


	def apply[X <: Mapping, M <: RefinedMapping[S, O], S, O]
	         (mapping :X)(implicit types :Conforms[X, M, RefinedMapping[S, O]]) :Optional[M] =
		new DirectOptionMapping[M, S, O](mapping)

	def singleton[S, O](mapping :RefinedMapping[S, O]) :Optional[mapping.type] =
		apply[mapping.type, mapping.type, S, O](mapping)


//todo: OriginProjection
//	implicit def optionMappingProjection[M <: Mapping](implicit body :OriginProjection[M])
//			:OriginProjection[Optional[M]] { type WithOrigin[A] = Optional[body.WithOrigin[A]] } =
//		OriginProjection.projectAs[Optional[M], ({ type P[A] = Optional[body.WithOrigin[A]] })#P]
//	implicit def optionMappingProjection[M <: RefinedMapping[S, O], S, O](implicit body :RefinedProjection[M, S])
//			:OriginProjection[OptionMapping[M, S, O]] { type WithOrigin[A] = OptionMapping[body.WithOrigin[A], S, A] } =
//		OriginProjection.projectAs[OptionMapping[M, S, O], ({ type P[A] = OptionMapping[body.WithOrigin[A], S, A] })#P]



	private class DirectOptionMapping[+M <: RefinedMapping[S, O], S, O](protected override val backer :M)
		extends OptionMapping[M, S, O] with ShallowDelegate[Option[S], O]
	{ box =>
		override val get :M = backer



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
			backer.extracts.map(schema.composeExtractAssoc(eggExtract)(_)).updated(backer, eggExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] = backer match {
			case column :ColumnMapping[S @unchecked, O @unchecked] =>
				NaturalMap.single[Column, ColumnExtract, S](column, eggExtract.asInstanceOf[ColumnExtract[S]])
			case _ =>
				backer.columnExtracts.map(schema.composeColumnExtractAssoc(eggExtract)(_))
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


