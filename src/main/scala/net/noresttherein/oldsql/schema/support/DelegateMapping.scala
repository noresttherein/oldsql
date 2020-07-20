package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}






/** Skeletal base trait for mappings enclosing another mapping `backer`, not intended to be exposed to the outside.
  * It is the root of the hierarchy of various types of proxies and adapters which modify some aspect of the original
  * mapping. These are implementation interfaces, introducing or updating no new declarations, but providing defaults
  * and frameworks for implementing the existing methods. It is a separate hierarchy to
  * [[net.noresttherein.oldsql.schema.bits.MappingAdapter MappingAdapter]], which is a 'public' interface, exposing
  * the the adapted mapping through a property. This allows a class to implement both, each with another mapping
  * type as the adapted mapping - a feature used when the adapted mapping itself is an adapter to expose the
  * original mapping, rather than the adapter.
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappedMapping]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.DelegateAdapter]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.ComposedAdapter]]
  */
trait DelegateMapping[+M <: Mapping, S, O] extends TypedMapping[S, O] {
	protected val backer :M

	override def sqlName :Option[String] = backer.sqlName
}





object DelegateMapping {

	type Delegate[S, O] = DelegateMapping[MappingAt[O], S, O]
	type Proxy[S, O] = DelegateMapping[RefinedMapping[S, O], S, O]
	type MappedDelegate[T, S, O] = DelegateMapping[RefinedMapping[T, O], S, O]



	//commented out to avoid conflicts for classes extending also other projectable mappings.
//	implicit def delegateProjection[M <: Mapping, S, A](implicit body :OriginProjection[M])
//			:OriginProjection[DelegateMapping[M, S, A]] { type WithOrigin[O] = DelegateMapping[body.WithOrigin[O], S, O] } =
//		body.lift[({ type T[+X <: Mapping, O] = DelegateMapping[X, S, O] })#T, M]



	/** Base trait for mappings which adapt another proxy from the same source `O`. Declares a single component,
	  * the embedded `backer` mapping, with all its components and subcomponents (and columns in particular)
	  * becoming directly the subcomponents of this mapping. As the adapted mapping `M` may have a different subject
	  * type to this mapping's subject, it serves in particular as a root for all mappings which map the subject of
	  * the nested mapping, but also simple decorators providing additional functionality or information, but leaving
	  * all mapping details of the `backer` intact.
	  * @tparam O a discriminator tag marking components from the same source.
	  * @tparam S the subject type of this mapping.
	  */
	trait ShallowDelegate[S, O] extends DelegateMapping[MappingAt[O], S, O] {

		override def components :Unique[Component[_]] = backer.components //Unique(backer)
		override def subcomponents :Unique[Component[_]] = backer.subcomponents

		override def columns :Unique[Column[_]] = backer.columns
		override def selectable :Unique[Column[_]] = backer.selectable
		override def queryable :Unique[Column[_]] = backer.queryable
		override def updatable :Unique[Column[_]] = backer.updatable
		override def autoUpdated :Unique[Column[_]] = backer.autoUpdated
		override def insertable :Unique[Column[_]] = backer.insertable
		override def autoInserted :Unique[Column[_]] = backer.autoInserted


		/** Refers to the adapted mapping `backer` to export the passed component to its final representation,
		  * unless `component` is the `backer` itself, in which it is returned as-is.
		  */
		override def export[X](component :Component[X]) :Component[X] =
			if (component eq backer) component
			else backer.export(component)

		/** Refers to the adapted mapping `backer` to export the passed component to its final representation,
		  * unless `column` is the `backer` itself, in which it is returned as-is.
		  */
		override def export[X](column :Column[X]) :Column[X] =
			if (column eq backer) column
			else backer.export(column)

	}



}







