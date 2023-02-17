package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.{Extractor, InferTypeParams}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.support.{DelegateMapping, MappedMapping}
import net.noresttherein.oldsql.schema.support.MappedMapping.MappedColumnMapping






sealed trait OptionMapping[+M <: Mapping, S, O] extends BaseMapping[Option[S], O] {
	val get :M
}



object OptionMapping {

	type Optional[M <: Mapping] = OptionMapping[M, M#Subject, M#Origin]

	type OptionAt[M[A] <: MappingAt[A], O] = OptionMapping[M[O], M[O]#Subject, O]


	def apply[X <: Mapping, M <: TypedMapping[S, O], S, O]
	         (mapping :X)(implicit types :InferTypeParams[X, M, TypedMapping[S, O]]) :Optional[M] =
		new MappedOptionMapping[M, S, O](mapping)

	def singleton[S, O](mapping :TypedMapping[S, O]) :Optional[mapping.type] =
		apply[mapping.type, mapping.type, S, O](mapping)

	def column[X <: TypedColumn[_, _], M <: TypedColumn[S, O], S, O]
	          (column :M)(implicit types :InferTypeParams[X, M, TypedColumn[S, O]]) :OptionColumn[M, S, O] =
		new MappedOptionColumn[M, S, O](column)

	implicit def optionMappingProjection[M <: TypedMapping[S, O], S, O](implicit body :ExactProjection[M])
			:ProjectionDef[OptionMapping[M, S, O], ({ type P[X] = OptionMapping[body.WithOrigin[X], S, X] })#P, Option[S]] =
		body.map[({ type P[+X <: Mapping, Q] = OptionMapping[X, S, Q] })#P, Option[S]]


	trait OptionColumn[+M <: Mapping, S, O] extends BaseColumn[Option[S], O] with OptionMapping[M, S, O]


	private[oldsql] trait OptionComponent[+M <: TypedMapping[S, O], S, O]
		extends OptionMapping[M, S, O] with DelegateMapping[M, Option[S], O]
	{
		override val get = backer

		override def assemble(values: Pieces): Opt[Option[S]] = Got(values.get(get).toOption)

//		override def nullValue :NullValue[Option[Nothing]] = NullValue.None

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Option[S]] =
			if (selectedByDefault == components)
				selectForm
			else get.selectForm(
				if (components.contains(get)) get.selectedByDefault ++ components - get
				else components
			).toOpt

		protected override def newWriteForm(op :WriteOperationView, components :Unique[Component[_]])
				:SQLWriteForm[Option[S]] =
		{
			val exports = components.map(export(_))
			if (get.defaultColumns(op) == exports)
				op.form(this)
			else get.writeForm(op,
				if (exports.contains(get)) get.defaultColumns(op) ++ exports - get
				else exports
			).toOpt
		}

		override val selectForm :SQLReadForm[Option[S]] = get.selectForm.toOpt
		override val filterForm :SQLWriteForm[Option[S]] = get.filterForm.toOpt
		override val insertForm :SQLWriteForm[Option[S]] = get.insertForm.toOpt
		override val updateForm :SQLWriteForm[Option[S]] = get.updateForm.toOpt
		protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[Option[S]] = op.form(this)

		override def mappingName = "Option"
		override def toString :String = "Option[" + get + "]"
	}



	private[oldsql] class MappedOptionMapping[+M <: TypedMapping[S, O], S, O] private
	                                         (protected override val backer :M,
	                                          protected override val map :S =?> Option[S],
	                                          protected override val unmap :Option[S] =?> S)
		extends MappedMapping[S, Option[S], O] with OptionComponent[M, S, O]// with ShallowDelegate[Option[S], O]
	{
		def this(get :M) = this(get, Extractor.Requisite(Some(_:S)), Extractor.fromOption[S])

		private[this] val backerExtract = apply(get)

		override def assemble(values: Pieces): Opt[Option[S]] = Got(values.get(backerExtract).toOption)

		protected override def nulls :NullValue[Option[Nothing]] = NullValue.None
	}



	private[oldsql] class MappedOptionColumn[+M <: TypedColumn[S, O], S, O] private
	                                        (backer :M, map :S =?> Option[S], unmap :Option[S] =?> S)
		extends MappedColumnMapping[M, S, Option[S], O](backer, map, unmap)(NullValue.None)
		   with OptionColumn[M, S, O] with OptionComponent[M, S, O]
	{
		def this(get :M) = this(get, Extractor.Requisite(Some(_:S)), Extractor.fromOption[S])

		private[this] val backerExtract = apply(get)

		override def assemble(values: Pieces): Opt[Option[S]] = Got(values.get(backerExtract).toOption)
	}

}


