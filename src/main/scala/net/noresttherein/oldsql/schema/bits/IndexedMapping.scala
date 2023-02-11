package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Listing
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnMappingTemplate, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.MappingTemplate
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.IndexedMapping.GetListingComponent
import net.noresttherein.oldsql.schema.bits.LabelPath.{/, Label}






/**
  * @author Marcin Mościcki
  */
trait IndexedMapping[S, O] extends BaseMapping[S, O] with MappingTemplate[IndexedMapping, IndexedColumnMapping] {
	def apply[N <: Label](label :N)(implicit get :GetListingComponent[S, N]) :IndexedMapping[get.Value, O]

	def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[S, P]) :IndexedMapping[get.Value, O]
}



object IndexedMapping {
	type of[S] = { //consider: renaming projections to simply P and C
		type M[O] = IndexedMapping[S, O]
		type C[O] = IndexedColumnMapping[S, O]
		type Mapping[O] = IndexedMapping[S, O]
		type Column[O] = IndexedColumnMapping[S, O]
	}

	//todo: differentiate between columns and other components. Possible with type ColOrComp[Comp, Col] = ...
	class GetListingComponent[X, N] private[IndexedMapping] {
		type Value
	}

	object GetListingComponent {
		def last[I <: Listing, K, V, N <: Label] :GetListingComponent[I |~ (K :~ V), N] { type Value = V } =
			instance.asInstanceOf[GetListingComponent[I |~ (K :~ V), N] { type Value = V }]

		def previous[I <: Listing, K, V, P](implicit prefix :GetListingComponent[I, P])
				:GetListingComponent[I |~ (K :~ P), P] { type Value = prefix.Value } =
			instance.asInstanceOf[GetListingComponent[I |~ (K :~ P), P] { type Value = prefix.Value }]

		def nested[I <: Listing, T, P, N <: Label]
		          (implicit prefix :GetListingComponent[I, P] { type Value = T }, last :GetListingComponent[T, N])
				:GetListingComponent[I, P / N] { type Value = last.Value } =
			instance.asInstanceOf[GetListingComponent[I, P / N] { type Value = last.Value }]

		private[this] val instance = new GetListingComponent[Any, Any] {}
	}

}



trait IndexedColumnMapping[S, O]
	extends IndexedMapping[S, O] with BaseColumn[S, O] with ColumnMappingTemplate[IndexedColumnMapping]
{
	override def apply[N <: Label](label :N)(implicit get :GetListingComponent[S, N]) :IndexedMapping[get.Value, O] =
		this.asInstanceOf[IndexedColumnMapping[get.Value, O]]

	override def apply[P](path :LabelPath[P])(implicit get :GetListingComponent[S, P]) :IndexedMapping[get.Value, O] =
		this.asInstanceOf[IndexedColumnMapping[get.Value, O]]
}
