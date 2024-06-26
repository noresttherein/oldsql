package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.bases.BaseMapping






/** Skeletal base trait for mappings enclosing another mapping `backer`, not intended to be exposed to the outside.
  * It is the root of the hierarchy of various types of proxies and adapters which modify some aspect of the original
  * mapping. These are implementation interfaces, introducing or updating no new declarations, but providing defaults
  * and frameworks for implementing the existing methods. It is a separate hierarchy to
  * [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]], which is a 'public' interface, exposing
  * the the adapted mapping through a property. This allows a class to implement both, each with another mapping
  * type as the adapted mapping - a feature used when the adapted mapping itself is an adapter in order to expose
  * the original mapping, rather than the adapter.
  *
  * Unless explicitly noted (or evident by type signature), extending classes and traits assume they are ''not''
  * columns, even if `backer` is a column.
  *
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingProxy]]
  * @see [[net.noresttherein.oldsql.schema.support.MappedMapping]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter]]
  */
trait DelegateMapping[+M <: Mapping, S, O] extends BaseMapping[S, O] {
	protected val backer :M //todo: rename to underlying

	/** Converts a component of this mapping to a corresponding component of the backing mapping. If the argument
	  * already is a component of `backer`, it is returned itself; otherwise a component of `backer` which served
	  * as the base for the argument is returned, if it exists. In standard implementations, where only ''export''
	  * components of `backer` are exported to components uniquely of this instance - and non-export backer's components
	  * are exported as the same component as their export version under `backer` - the mapping returned is
	  * an ''export'' component. A complete inverse of the function applied to all components of `backer` when exporting
	  * them as components of this instance might not exist - for example the delegate itself might
	  * not be an ''export'' version of the backing mapping - in which case
	  * a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]] should be thrown.
	  * If this delegate mapping is
	  * a [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate ShallowDelegate]] -
	  * which uses the components of `backer` as-is, the function is an identity on all of its domain (which might not
	  * be necessarily the complete component set of `backer`), with a possible exception of this delegate itself.
	  * The method has protected access here as `backer` might be completely shielded fromm the outside,
	  * but it exists in a public version also in [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]],
	  * and extending both these classes by mixing in
	  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]], as is typical,
	  * makes it available to the clients of this mapping.
	  */
	protected def unexport[X](component :Component[X]) :backer.Component[X]

	/** Converts a column of this mapping to a corresponding column of the backing mapping. If the argument
	  * already is a column of `backer`, it is returned itself; otherwise a column of `backer` which served
	  * as the base for the argument is returned, if it exists. In standard implementations, where only ''export''
	  * columns of `backer` are exported to columns uniquely of this instance - and non-export backer's columns
	  * are exported as the same columns as their export versions under `backer` - the column returned is
	  * an ''export'' column. A complete inverse of the function applied to `backer`'s columns when exporting
	  * them by this instance might not exist, in which case
	  * a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]] should be thrown.
	  * If this delegate mapping is
	  * a [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate ShallowDelegate]] -
	  * which uses the components of `backer` as-is, the function is an identity on all of its domain (which might not
	  * be necessarily the complete component set of `backer`).
	  * The method has protected access here as `backer` might be completely shielded fromm the outside,
	  * but it exists in a public version also in [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]],
	  * and extending both these classes by mixing in
	  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]], as is typical,
	  * makes it available to the clients of this mapping.
	  */
	protected def unexport[X](column :Column[X]) :backer.Column[X]
}





object DelegateMapping {

	type Delegate[S, O] = DelegateMapping[MappingAt[O], S, O]
	type Proxy[S, O] = DelegateMapping[TypedMapping[S, O], S, O]
	type MappedDelegate[T, S, O] = DelegateMapping[TypedMapping[T, O], S, O]


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
	  * @tparam S $SubjectParamInfo
	  * @tparam O $OriginParamInfo
	  */ //fixme: outdated docs closer to WrapperDelegateTemplate semantics
	trait ShallowDelegateTemplate[+M <: MappingAt[O] with MappingTemplate[Comp, Col],
	                              +Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
		extends DelegateMapping[M, S, O] with MappingTemplate[Comp, Col]
	{ this :Comp[S, O] =>
//		protected override val backer :MappingAt[O] with MappingTemplate[Comp, Col]

		override def components    :Unique[Comp[_, O]] = backer.components
		override def subcomponents :Unique[Comp[_, O]] = backer.subcomponents

		override def columns           :Unique[Col[_, O]] = backer.columns
		override def selectable        :Unique[Col[_, O]] = backer.selectable
		override def filterable        :Unique[Col[_, O]] = backer.filterable
		override def insertable        :Unique[Col[_, O]] = backer.insertable
		override def updatable         :Unique[Col[_, O]] = backer.updatable
		override def autoInserted      :Unique[Col[_, O]] = backer.autoInserted
		override def autoUpdated       :Unique[Col[_, O]] = backer.autoUpdated
		override def selectedByDefault :Unique[Col[_, O]] = backer.selectedByDefault
		override def filteredByDefault :Unique[Col[_, O]] = backer.filteredByDefault
		override def insertedByDefault :Unique[Col[_, O]] = backer.insertedByDefault
		override def updatedByDefault  :Unique[Col[_, O]] = backer.updatedByDefault
		override def mandatorySelect   :Unique[Col[_, O]] = backer.mandatorySelect
		override def mandatoryFilter   :Unique[Col[_, O]] = backer.mandatoryFilter
		override def mandatoryInsert   :Unique[Col[_, O]] = backer.mandatoryInsert
		override def mandatoryUpdate   :Unique[Col[_, O]] = backer.mandatoryUpdate

		override def columns(op :OperationView) :Unique[Col[_, O]] = backer.columns(op)
		override def defaultColumns(op :OperationView) :Unique[Col[_, O]] = backer.defaultColumns(op)

		override def columns[T](op :OperationView, component :Component[T]) :Unique[Col[_, O]] =
			if (component == this || component == backer) columns(op)
			else backer.columns(op, component)

		override def defaultColumns[T](op :OperationView, component :Component[T]) :Unique[Col[_, O]] =
			if (component == this || component == backer) defaultColumns(op)
			else backer.defaultColumns(op, component)

		override def columnNamed(name :String) :Col[_, O] = backer.columnNamed(name)

		/** Refers to the adapted mapping `backer` to export the passed component to its final representation,
		  * unless `component` is the `this` itself, in which it is returned as-is.
		  */
		override def export[T](component :Component[T]) :Comp[T, O] =
			if (component eq this) this.asInstanceOf[Comp[T, O]] else backer.export(component)

		/** Refers to the adapted mapping `backer` to export the passed component to its final representation. */
		override def export[T](column :Column[T]) :Col[T, O] = backer.export(column)

		override def exportOrNot[T](component :Component[T]) :Component[T] = backer.exportOrNot(component)

		override def exportOrNot[T](column :Column[T]) :Column[T] = backer.exportOrNot(column)

		override def unexport[X](component :Component[X]) :Component[X] =
			if (component eq this)
				throw new IllegalArgumentException("Cannot un-export the adapter mapping itself: " + component)
			else
				component

		override def unexport[X](column :Column[X]) :Column[X] = column

		override def contains[T](component :Component[T]) :Boolean =
			(component eq backer) || backer.contains(component)
	}

	type ShallowDelegate[S, O] = ShallowDelegateTemplate[MappingAt[O], TypedMapping, TypedColumn, S, O]
//	trait ShallowDelegate[S, O] extends ShallowDelegateTemplate[TypedMapping, TypedColumn, S, O]
/*
	trait ShallowDelegate[S, O] extends DelegateMapping[MappingAt[O], S, O] {
		override def components :Unique[Component[_]] = backer.components
		override def subcomponents :Unique[Component[_]] = backer.subcomponents

		override def columns           :Unique[Column[_]] = backer.columns
		override def selectable        :Unique[Column[_]] = backer.selectable
		override def filterable        :Unique[Column[_]] = backer.filterable
		override def insertable        :Unique[Column[_]] = backer.insertable
		override def updatable         :Unique[Column[_]] = backer.updatable
		override def autoInserted      :Unique[Column[_]] = backer.autoInserted
		override def autoUpdated       :Unique[Column[_]] = backer.autoUpdated
		override def selectedByDefault :Unique[Column[_]] = backer.selectedByDefault
		override def filteredByDefault :Unique[Column[_]] = backer.filteredByDefault
		override def insertedByDefault :Unique[Column[_]] = backer.insertedByDefault
		override def updatedByDefault  :Unique[Column[_]] = backer.updatedByDefault
		override def mandatorySelect   :Unique[Column[_]] = backer.mandatorySelect
		override def mandatoryFilter   :Unique[Column[_]] = backer.mandatoryFilter
		override def mandatoryInsert   :Unique[Column[_]] = backer.mandatoryInsert
		override def mandatoryUpdate   :Unique[Column[_]] = backer.mandatoryUpdate

		override def columns(op :OperationView) :Unique[Column[_]] = backer.columns(op)
		override def defaultColumns(op :OperationView) :Unique[Column[_]] = backer.defaultColumns(op)

		override def columns[T](op :OperationView, component :Component[T]) :Unique[Column[_]] =
			if (component == this || component == backer) columns(op)
			else backer.columns(op, component)

		override def defaultColumns[T](op :OperationView, component :Component[T]) :Unique[Column[_]] =
			if (component == this || component == backer) defaultColumns(op)
			else backer.defaultColumns(op, component)

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

		override def unexport[X](component :Component[X]) :Component[X] =
			if (component eq this)
				throw new IllegalArgumentException("Cannot un-export the adapter mapping itself: " + component)
			else
				component

		override def unexport[X](column :Column[X]) :Column[X] = column

		override def contains[T](component :Component[T]) :Boolean =
			(component eq backer) || backer.contains(component)
	}
*/


	/** A mapping having a single direct component, its `backer` and treating its subcomponents as subcomponents
	  * of this instance. The `subcomponents` list consists of all `backer.subcomponents` and `backer` itself,
	  * but in all other ways this trait acts the same way as `ShallowDelegate`.
	  * @tparam O the mapping's [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type:
	  *           a discriminator tag marking components from the same source.
	  * @tparam S the subject type of this mapping.
	  */
	trait WrapperDelegateTemplate[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
		extends ShallowDelegateTemplate[Comp[_, O] with MappingTemplate[Comp, Col], Comp, Col, S, O]
	{ this :Comp[S, O] =>
		override def components    :Unique[Comp[_, O]] = Unique.single(backer)
		override def subcomponents :Unique[Comp[_, O]] = backer +: backer.subcomponents
	}
	type WrapperDelegate[S, O] = WrapperDelegateTemplate[TypedMapping, TypedColumn, S, O]
/*
	trait WrapperDelegate[S, O] extends DelegateMapping[TypedMapping[_, O], S, O] with ShallowDelegate[S, O] {
		override def components :Unique[Component[_]] = Unique.single(backer)
		override def subcomponents :Unique[Component[_]] = backer +: backer.subcomponents
	}
*/


	/** A convenience base class for anonymous implementations of `DelegateMapping`, in particular
	  * those which extend also [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]],
	  * as it initializes early `backer` field to a mapping of an arbitrary type.
	  */
	private[oldsql] abstract class BaseDelegateMapping[+M <: Mapping, S, O](protected override val backer :M)
		extends DelegateMapping[M, S, O]
}







