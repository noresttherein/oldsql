package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.model.RelatedEntityFactory
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Optional, Requisite}
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.{MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{ColumnAdapter, DelegateAdapter}






object OptionalMapping {
	def apply[M <: RefinedMapping[Option[S], O], S, O]
	         (body :M)(implicit nulls :NullValue[S] = NullValue.NotNull) :MappingAdapter[M, S, O] =
		MappedMapping[M, Option[S], S, O](body, Extractor.fromOption, Some(_:S))

	def adapter[M <: RefinedMapping[T, O], T, S, O]
	           (body :MappingAdapter[M, Option[S], O])(implicit nulls :NullValue[S] = NullValue.NotNull)
			:MappingAdapter[M, S, O] =
		MappedMapping.adapter[M, Option[S], S, O](body, Extractor.fromOption, Some(_:S))

	def column[M <: ColumnMapping[Option[S], O], S, O]
	          (column :M)(implicit nulls :NullValue[S] = NullValue.NotNull) :ColumnAdapter[M, Option[S], S, O] =
		MappedMapping.column[M, Option[S], S, O](column, Extractor.fromOption, Some(_:S))

	def columnAdapter[M <: ColumnMapping[T, O], T, S, O]
	                 (column :ColumnAdapter[M, T, Option[S], O])(implicit nulls :NullValue[S] = NullValue.NotNull)
			:ColumnAdapter[M, T, S, O] =
		MappedMapping.columnAdapter[M, T, Option[S], S, O](column, Extractor.fromOption, Some(_:S))



	class OptionalKinMapping[+M <: RefinedMapping[S, O], K, S, R, O]
	                        (protected override val backer :M, factory :RelatedEntityFactory[K, S, S, R])
		extends MappedMapping[S, R, O] with DelegateAdapter[M, R, O]
	{
		protected override def nulls :NullValue[R] =
			if (factory.isRequired) NullValue.eval(factory.nonexistent) else NullValue(factory.nonexistent)

		protected override def map :S =?> R = Requisite(factory.present _)
		protected override def unmap :R =?> S = Optional(factory.valueOf(_:R))
	}

}
