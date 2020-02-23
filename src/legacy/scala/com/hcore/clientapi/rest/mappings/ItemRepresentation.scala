package com.hcore.clientapi.rest.mappings

import com.hcore.clientapi.rest.items.ItemMeta.ItemProperty
import com.hcore.clientapi.rest.items._
import com.hcore.ogre.model.Reference
import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.collection.generic.CanBuildFrom




trait ItemRepresentation[I, E] extends ItemMeta[I] {

	def entity(item :I) :E
	def mapEntity(entity :E) :I

	def property(property :Item.Property[I, _]) :PropertyChain[E, _]
	def foreignProperty(property :ItemProperty[I, _]) :E=>Reference[Any]
	def referenceProperty(property :Item.Property[I, _]) :Option[E=>Reference[Any]]
	def mapProperty(property :PropertyChain[E, _]) :ItemMeta.Property[I, _]

	def one(ref :Item[I]) :Reference[E]
	def maybe(ref :Item[I]) :Reference[Option[E]]
	def seq(ref :Items[I]) :Reference[Seq[E]]
	def many[T<:Iterable[E]](ref :Items[I])(implicit cbf :CanBuildFrom[_, E, T]) :Reference[T]

	def item(ref :Reference[E]) :Item[I]
	def items[T<:Iterable[E]](ref :Reference[T]) :Items[I]

	def target[T](property :ItemProperty[I, T]) :ItemRepresentation[T, _]

}






object ItemRepresentation {
	implicit val ClientMeta = ClientRepresentation
	implicit val ProductMeta = ProductRepresentation
	implicit val CampaignMeta = CampaignRepresentation
	implicit val OrderMeta = OrderRepresentation
	implicit val CreativeMeta = CreativeRepresentation
	implicit val LandingMeta = LandingRepresentation
	implicit val TargetingPolicyMeta = TargetingPolicyRepresentation
	implicit val PixelMeta = PixelRepresentation
	implicit val TagMeta = TagRepresentation
}





