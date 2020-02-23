package com.adpilot.cortb.clientapi.rest.mappings

import com.adpilot.cortb.clientapi.prototype.repository.entities
import com.adpilot.cortb.clientapi.prototype.repository.entities._
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference._
import com.adpilot.cortb.clientapi.rest.items.ItemMeta.ItemProperty
import com.adpilot.cortb.clientapi.rest.items._
import com.adpilot.cortb.clientapi.util.ObjectProperty

import scala.collection.generic.CanBuildFrom


import scala.reflect.runtime.universe._




trait ItemRepresentation[I, E] extends ItemMeta[I] {

	def entity(item :I) :E
	def mapEntity(entity :E) :I

	def property(property :Item.Property[I, _]) :ObjectProperty[E, _]
	def foreignProperty(property :ItemProperty[I, _]) :E=>One[Any]
	def referenceProperty(property :Item.Property[I, _]) :Option[E=>Reference[Any]]
	def mapProperty(property :ObjectProperty[E, _]) :ItemMeta.Property[I, _]

	def one(ref :Item[I]) :One[E]
	def maybe(ref :Item[I]) :Reference[Option[E]]
	def seq(ref :Items[I]) :Reference[Seq[E]]
	def many[T<:Iterable[E]](ref :Items[I])(implicit cbf :CanBuildFrom[_, E, T]) :Many[T]

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
	implicit val CreativeImageMeta = CreativeImageRepresentation
	implicit val LandingMeta = LandingRepresentation
	implicit val TargetingPolicyMeta = TargetingPolicyRepresentation
	implicit val PixelMeta = PixelRepresentation
	implicit val TagMeta = TagRepresentation
	implicit val TagKeyMeta = TagKeyRepresentation
	implicit val StatusMeta = StatusTypeRepresentation
}





