package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingNest, OpenNest}





trait MappingAdapter[+M <: Mapping, S, O] extends GenericMapping[S, O] with OpenNest[M] {
	override val egg :M

//	override def map[X](there :S => X, back :X => S) :egg.type AdaptedAs X =
//		MappedMapping[egg.type, O, S, X](egg, map andThen there, back andThen unmap)
//
//	override def flatMap[X](there :S => Option[X], back :X => Option[S]) :egg.type AdaptedAs X =
//		MappedMapping.opt[egg.type, O, S, X](egg, map andThen there, back(_) map unmap)

}





object MappingAdapter {

	type Adapted[M <: Mapping] = MappingAdapter[M, M#Subject, M#Origin]
	type AdaptedAs[M <: Mapping, T] = MappingAdapter[M, T, M#Origin]
	type AdaptedFor[M <: Mapping, O] = MappingAdapter[M, M#Subject, O]



//	implicit def MappingAdapterAlias[X <: Mapping, A, Y <: Mapping, S, B]
//	                                (implicit alias :OriginProjection[X, A, Y, B])
//			:OriginProjection[MappingAdapter[X, S, A], A, MappingAdapter[X, S, B], B] =

	/** Base trait for mappings which adapt another proxy from the same source `O`. Declares a single component,
	  * the embedded `egg` mapping, with all its components and subcomponents (and columns in particular)
	  * becoming directly the subcomponents of this mapping. As the adapted mapping `M` may have a different subject
	  * type to this mapping's subject, it serves in particular as a root for all mappings which map the subject of
	  * the nested mapping, but also simple decorators providing additional functionality or information, but leaving
	  * all mapping details of the `egg` intact.
	  * @tparam M the type of the adapted mapping, exposed to subclasses by the `egg` `val`.
	  * @tparam O a discriminator tag marking components from the same source.
	  * @tparam T the subject type of the adapted mapping `M`.
	  * @tparam S the subject type of this mapping.
	  */
	trait ShallowAdapter[+M <: Mapping.TypedMapping[T, O], T, S, O] extends GenericMapping[S, O] with MappingNest[M] {

		override def components :Unique[Component[_]] = egg.components //Unique(egg)
		override def subcomponents :Unique[Component[_]] = egg.subcomponents

		override def columns :Unique[Column[_]] = egg.columns
		override def selectable :Unique[Column[_]] = egg.selectable
		override def queryable :Unique[Column[_]] = egg.queryable
		override def updatable :Unique[Column[_]] = egg.updatable
		override def autoUpdated :Unique[Column[_]] = egg.autoUpdated
		override def insertable :Unique[Column[_]] = egg.insertable
		override def autoInserted :Unique[Column[_]] = egg.autoInserted


		/** Refers to the adapted mapping `egg` to export the passed component to its final representation,
		  * unless `component` is the `egg` itself, in which it is returned as-is.
		  */
		override def export[X](component :Component[X]) :Component[X] =
			if (component eq egg) component
			else egg.export(component)

		/** Refers to the adapted mapping `egg` to export the passed component to its final representation,
		  * unless `column` is the `egg` itself, in which it is returned as-is.
		  */
		override def export[X](column :Column[X]) :Column[X] =
			if (column eq egg) column
			else egg.export(column)
	}

}







