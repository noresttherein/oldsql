package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.Mapping.ReferenceContext
import com.hcore.ogre.mapping.MappingMorphism.ValueMorphism
import com.hcore.ogre.mapping.support.MappingAdapter.DirectMappingAdapter
import com.hcore.ogre.mapping.ties.MappingReference.GenericMappingReferenceFactory
import com.hcore.ogre.mapping.{ComponentPath, AnyMapping, Mapping}
import com.hcore.ogre.model.ComposedOf.DecomposableTo
import com.hcore.ogre.model.CompositeReference.FlattenedReference
import com.hcore.ogre.model.{ComposedOf, Reference}
import com.hcore.ogre.model.Reference.{Full, ReferenceFactorySupport, ReferenceFactory, GenericReferenceFactory}
import com.hcore.ogre.sql.SQLReadForm


trait GenericReferenceFlatMapping[RK<:Reference[JES], RKM<:Mapping[RK], JES, JE, JEM<:Mapping[JE], JRM<:AnyMapping, E, EM<:Mapping[E], ES, R<:Reference[ES]]
	extends DirectMappingAdapter[R, RK, RKM] with ReferenceMapping[RK, E, ES, R]
{

	val referenceFactory :GenericMappingReferenceFactory[RK, E, ES, R, EM]
	val ref :RKM
	protected def joinedReference :ComponentPath[JEM, JRM]
	

	override def scoutValue(ctx: ReferenceContext[this.type]): Option[R] =
		for {
			head <- Scout(ref)
			join <- head concat joinedReference
			tail <- Scout(join)
			if tail.end==referenceFactory.target
			targetCtx = (ctx \\ (ComponentPath.direct[this.type, RKM](this, ref) ++ tail)).asInstanceOf[ReferenceContext[EM]]
		} yield referenceFactory.scout(targetCtx)


	object morphism extends DirectMorphism {
		val value = ValueMorphism[R, RK](referenceFactory.keyOf(_))
	}

	override def selectForm: SQLReadForm[R] = ref.selectForm.map(referenceFactory.empty)

	override protected def referenceKey(values: Values): Option[RK] = values.get(ref)
}





//
//
//class ReferenceFlatMapping[RK<:Reference[JE], RKM<:Mapping[RK], JE, JEM<:Mapping[JE], JRM<:Mapping, E, EM<:Mapping[E], X, R<:Reference[X]]
//		(val ref :RKM, joinReference : =>ComponentPath[JEM, JRM], val referenceFactory :GenericMappingReferenceFactory[RK, E, X, R, EM])
//	extends GenericReferenceFlatMapping[RK, RKM, JE, JEM, JRM, E, EM, X, R]
//{
//	val adaptee = ref
//	def joinedReference = joinReference
//
//}
//


object ReferenceFlatMapping {

	class FlatMappingReferenceFactory[HK, RK<:Reference[JES], JES, JE, TK, E, TX, ES](
			head :GenericReferenceFactory[HK, JE, JES, RK],
			tail :ReferenceFactory[TK, E, TX],
			tailRef :JE=>Reference[TX]//,
//			headRef :
			)(implicit val items :ES ComposedOf E)
		extends ReferenceFactorySupport[RK, E, ES]
	{
		private val headDecomposition = DecomposableTo(head.items.decomposition(_: JES).map(tailRef))

		override def empty(key: RK): Reference[ES] =
			FlattenedReference(key, headDecomposition, tail.items.decomposition)

		override def keyOf[F >: Reference[ES] <: Reference[ES]](ref: F): Option[RK] = ref match {
			case FlattenedReference(head(key, value), _, _) =>
				Some(head(key, value))
			//			case FlattenedReference(Full(refs), headItems, tailItems) if tailItems==tail.items.decomposition =>
			//				headItems(refs)
			//			case Full(items(values)) =>
			//				head.empty()
			case _ => None
		}


		override def keyFor(item: E): Option[RK] = None

		//			tail.keyFor(item).map(tail.empty)

		override def as[Y](implicit composition: ComposedOf[Y, E]): ReferenceFactory[RK, E, Y] =
			new FlatMappingReferenceFactory[HK, RK, JES, JE, TK, E, TX, Y](head, tail, tailRef)

		override def equivalencyToken: Any = (head.equivalencyToken, tail.equivalencyToken)
	}
}