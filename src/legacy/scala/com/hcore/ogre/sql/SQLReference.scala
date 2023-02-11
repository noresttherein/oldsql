package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{AnyMapping, Mapping}
import com.hcore.ogre.model.Reference.GenericReferenceFactory
import com.hcore.ogre.model.{Reference, ComposedOf}
import com.hcore.ogre.model.CompositeReference.{BaseCompositeReference, AbstractCompositeReference}
import com.hcore.ogre.slang.SaferCasts

//implicits
import SaferCasts._

/*

trait SQLReference[T, E] extends AbstractCompositeReference[T, E] {
	type TargetMapping <: Mapping[E]
	type Source <:RowSource
	def select :Select[Source, TargetMapping]

	override def toString = toOpt.map(v => s"Full[$select]($v)") getOrElse s"Empty[$select]"
}



object SQLReference {
	def apply[T, C](select :Select[RowSource, Mapping[T]])(implicit ev :C ComposedOf T) :SQLReference[C, T] =
		new TypedSQLReference[RowSource, Mapping[T], C, T](select)

	def unapply[T](reference :Reference[T]) :Option[(Select[RowSource, Mapping[E]], T ComposedOf E) forSome { type E }] =
		reference.ifSubclass[SQLReference[T, Any]] { r => (r.select, r.items) }


	class TypedSQLReference[S<:RowSource, M<:Mapping[E], T, E](val select :Select[S, M])(implicit val items :T ComposedOf E)
		extends SQLReference[T, E]
	{
		type TargetMapping = M
		type Source = S
		def toOpt = None
	}



	trait GenericSQLReferenceFactory[S<:Select[RowSource, Mapping[E]], E, T, R<:Reference[T]] extends GenericReferenceFactory[S, E, T, R]

}
*/
