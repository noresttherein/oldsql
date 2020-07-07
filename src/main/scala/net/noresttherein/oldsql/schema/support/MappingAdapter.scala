package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingSeal, OriginProjection, RefinedMapping}






/** Skeletal base trait for mappings enclosing another mapping `egg`. It is the root of the hierarchy of various
  * proxies, adapters and mapped mappings.
  */
trait MappingNest[+M <: Mapping] extends Mapping { this :MappingSeal =>
	protected val egg :M

	override def sqlName :Option[String] = egg.sqlName

	override def toString :String = egg.toString
}



trait OpenNest[+M <: Mapping] extends MappingNest[M] { this :MappingSeal =>
	override type Origin = egg.Origin
	override val egg :M
}






trait MappingAdapter[+M <: Mapping, S, O] extends MappingNest[M] with TypedMapping[S, O] {

	override val egg :M { type Origin = O }//consider: renaming to body or content
//	override type Origin = egg.Origin

//	override def map[X](there :S => X, back :X => S) :egg.type AdaptedTo X =
//		MappedMapping[egg.type, O, S, X](egg, map andThen there, back andThen unmap)
//
//	override def flatMap[X](there :S => Option[X], back :X => Option[S]) :egg.type AdaptedTo X =
//		MappedMapping.opt[egg.type, O, S, X](egg, map andThen there, back(_) map unmap)

}





object MappingAdapter {

	type Adapted[M <: Mapping] = MappingAdapter[M, M#Subject, M#Origin]
	type AdaptedTo[M <: Mapping, S] = MappingAdapter[M, S, M#Origin]
	type AdaptedAt[M[A] <: MappingAt[A], O] = MappingAdapter[M[O], M[O]#Subject, O]

//	trait Adapted[M <: Mapping] extends OpenNest[M] { this :MappingSeal =>
//		override type Subject = egg.Subject
//	}
//	type Adapted[M[O] <: Mapping] = AdaptedTo[M, M#Subject]






	implicit def adapterProjection[M <: Mapping, S, A](implicit body :OriginProjection[M])
			:OriginProjection[MappingAdapter[M, S, A]] { type WithOrigin[O] = MappingAdapter[body.WithOrigin[O], S, O] } =
		body.lift[({ type T[X <: Mapping, O] = MappingAdapter[X, S, O] })#T, M]



	/** Base trait for mappings which adapt another proxy from the same source `O`. Declares a single component,
	  * the embedded `egg` mapping, with all its components and subcomponents (and columns in particular)
	  * becoming directly the subcomponents of this mapping. As the adapted mapping `M` may have a different subject
	  * type to this mapping's subject, it serves in particular as a root for all mappings which map the subject of
	  * the nested mapping, but also simple decorators providing additional functionality or information, but leaving
	  * all mapping details of the `egg` intact.
	  * @tparam M the type of the adapted mapping, exposed to subclasses by the `egg` `val`.
	  * @tparam O a discriminator tag marking components from the same source.
	  * @tparam S the subject type of this mapping.
	  */
	trait ShallowAdapter[+M <: MappingAt[O], S, O] extends TypedMapping[S, O] with MappingNest[M] {

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

		override def sqlName :Option[String] = egg.sqlName

	}










}







