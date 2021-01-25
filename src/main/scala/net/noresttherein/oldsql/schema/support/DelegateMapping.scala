package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.schema.bases.BaseMapping






/** Skeletal base trait for mappings enclosing another mapping `backer`, not intended to be exposed to the outside.
  * It is the root of the hierarchy of various types of proxies and adapters which modify some aspect of the original
  * mapping. These are implementation interfaces, introducing or updating no new declarations, but providing defaults
  * and frameworks for implementing the existing methods. It is a separate hierarchy to
  * [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]], which is a 'public' interface, exposing
  * the the adapted mapping through a property. This allows a class to implement both, each with another mapping
  * type as the adapted mapping - a feature used when the adapted mapping itself is an adapter to expose the
  * original mapping, rather than the adapter.
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy]]
  * @see [[net.noresttherein.oldsql.schema.support.MappedMapping]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter]]
  */
trait DelegateMapping[+M <: Mapping, S, O] extends BaseMapping[S, O] {
	protected val backer :M
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
	  * becoming directly the subcomponents of this mapping. The backing mapping is considered a 'hidden' component
	  * of this mapping: it is not included (mainly for efficiency) in any component/column lists,
	  * but like all its components, is considered the ''export'' version of itself (from the point of view of this
	  * mapping). Both of the above properties can be changed in subclasses, though. This trait serves as a base type
	  * for simple decorators which provide additional features, leaving all mapping details of `backer` intact,
	  * but also, as the adapted mapping `M` may have a different subject type to this mapping's subject,
	  * for all mappings which map the subject of the nested mapping.
	  * @tparam O the mapping's [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type:
	  *           a discriminator tag marking components from the same source.
	  * @tparam S the subject type of this mapping.
	  */
	trait ShallowDelegate[S, O] extends DelegateMapping[MappingAt[O], S, O] {

		override def components :Unique[Component[_]] = backer.components //Unique(backer)
		override def subcomponents :Unique[Component[_]] = backer.subcomponents

		override def columns :Unique[Column[_]] = backer.columns
		override def selectable :Unique[Column[_]] = backer.selectable
		override def filterable :Unique[Column[_]] = backer.filterable
		override def insertable :Unique[Column[_]] = backer.insertable
		override def updatable :Unique[Column[_]] = backer.updatable
		override def autoInserted :Unique[Column[_]] = backer.autoInserted
		override def autoUpdated :Unique[Column[_]] = backer.autoUpdated
		override def selectedByDefault :Unique[Column[_]] = backer.selectedByDefault
		override def filteredByDefault :Unique[Column[_]] = backer.filteredByDefault
		override def insertedByDefault :Unique[Column[_]] = backer.insertedByDefault
		override def updatedByDefault :Unique[Column[_]] = backer.updatedByDefault

		override def columnNamed(name :String) :Column[_] = backer.columnNamed(name)

		/** Refers to the adapted mapping `backer` to export the passed component to its final representation,
		  * unless `component` is the `this` itself, in which it is returned as-is.
		  */
		override def export[T](component :Component[T]) :Component[T] =
			if (component eq this) component else backer.export(component)

		/** Refers to the adapted mapping `backer` to export the passed component to its final representation. */
		override def export[T](column :Column[T]) :Column[T] = backer.export(column)

		override def exportOrNot[T](component :Component[T]) :Component[T] = backer.exportOrNot(component)

		override def exportOrNot[T](column :Column[T]) :Column[T] = backer.exportOrNot(column)

		override def contains[T](component :Component[T]) :Boolean =
			(component eq backer) || backer.contains(component)
	}



	/** A mapping having a single direct component, its `backer` and treating its subcomponents as subcomponents
	  * of this instance. The `subcomponents` list consists of all `backer.subcomponents` and `backer` itself,
	  * but in all other ways this trait acts the same way as `ShallowDelegate`.
	  * @tparam O the mapping's [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type:
	  *           a discriminator tag marking components from the same source.
	  * @tparam S the subject type of this mapping.
	  */
	trait WrapperDelegate[S, O] extends ShallowDelegate[S, O] with DelegateMapping[RefinedMapping[_, O], S, O] {
		override def components :Unique[Component[_]] = Unique.single(backer)
		override def subcomponents :Unique[Component[_]] = backer +: backer.subcomponents
	}

}







