package com.hcore.ogre.mapping.ties

import com.hcore.ogre.mapping.{Mapping, MappingPath, AnyMapping}
import com.hcore.ogre.mapping.ComponentPath.{SelfPath, TypedComponentPath}
import com.hcore.ogre.mapping.Mapping.{TypedMapping, ReferenceContext}
import com.hcore.ogre.model.ComposedOf.Arity
import com.hcore.ogre.model.CompositeReference.AbstractCompositeReference
import com.hcore.ogre.model.Reference.Lazy.LazyReferenceFactory
import com.hcore.ogre.model.Restriction.Opressor
import com.hcore.ogre.model.{NavigableReference, CompositeReference, ComposedOf, Reference}
import com.hcore.ogre.model.Reference.{ReferenceFactory, RestrictedReferenceFactory, ReferenceFactoryProxy, GenericReferenceFactory}
import NavigableReference.NavigableReferenceFactory
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.sql.RowSource


//implicits
import SaferCasts._






object MappingReference {

	def unapply(any :Any) :Option[(MappingPath[_<:AnyMapping, _<:AnyMapping], ComposedOf[_, _])] =
		any.ifSubclass[MappingReference[_, _]] { r => (r.target, r.items) }

	def unapply[T](ref :Reference[T]) :Option[(MappingPath[_<:AnyMapping, _<:AnyMapping], (_<:T) ComposedOf _)] =
		ref.ifSubclass[MappingReference[T, _]] { r => (r.target, r.items )}


	private trait MappingReference[T, E] extends AbstractCompositeReference[T, E] with Scout {
		type ValueType = T
		type ElementType = E
		type TargetMapping <: Mapping[ElementType]

		val target :MappingPath[_<:AnyMapping, TargetMapping]
		override def path :MappingPath[_<:AnyMapping, _<:AnyMapping] = target

		override def canEqual(that :Any) = that.isInstanceOf[MappingReference[_, _]]

		override def equals(that :Any) = that match {
			case m:MappingReference[_, _] => (m eq this) || m.canEqual(this) && target==m.target
			case _ => false
		}

	}




	private class LazyMappingReference[T, E, /*FK<:Mapping, */M<:Mapping[E]](
			val target :MappingPath[_<:AnyMapping, M], value : =>Option[T])(implicit val items :T ComposedOf E)
		extends MappingReference[T, E]
	{
		def this(target :MappingPath[_<:AnyMapping, M])(implicit composedOf :T ComposedOf E) = this(target, None)

		def this(target :MappingPath[_<:AnyMapping, M], value :T)(implicit composedOf :T ComposedOf E) = this(target, Some(value))

		type TargetMapping = M

		lazy val toOpt = value
	}






	type MappingReferenceFactory[K, E, X, M<:Mapping[E]] = GenericMappingReferenceFactory[K, E, X, Reference[X], M]

	trait GenericMappingReferenceFactory[K, E, X, R<:Reference[X], M<:TypedMapping[E]] extends GenericReferenceFactory[K, E, X, R] {

		def target :M

		override def as[Y](implicit composition: ComposedOf[Y, E]): GenericMappingReferenceFactory[K, E, Y, Reference[Y], M]

		def scout(ctx :ReferenceContext[M]) :R

		override def canEqual(that :Any) = that.getClass==getClass 

		override def equals(that :Any) = that match {
			case m:GenericMappingReferenceFactory[_, _, _, _, _] =>
				(m eq this) || m.canEqual(this) && m.target==target
			case _ => false
		}

		override def toString = s"Reference[$target]"

	}


	object MappingReferenceFactory {
		def apply[K, E, X, M<:Mapping[E]](target :M, opressor :Opressor[K, E])(implicit composition :X ComposedOf E) :MappingReferenceFactory[K, E, X, M] =
			new RestrictedMappingReferenceFactory[K, E, X, M](target, opressor)

		def unapply[K, E, X, R<:Reference[X]](factory :GenericReferenceFactory[K, E, X, R]) :Option[GenericMappingReferenceFactory[K, E, X, R, _<:Mapping[E]]] =
			factory match {
				case m :GenericMappingReferenceFactory[_, _, _, _, _] =>
					Some(m.asInstanceOf[GenericMappingReferenceFactory[K, E, X, R, Mapping[E]]])
				case  _ => None
			}



//		def Lazy[K, E, X, R<:Reference[X], M<:Mapping[E]](target : =>M, factory : =>GenericReferenceFactory[K, E, X, R]) :GenericMappingReferenceFactory[K, E, X, R, M] =
//			new LazyMappingReferenceFactory(target, factory)

		def Lazy[K, E, X, R<:Reference[X], M<:Mapping[E]](factory : =>GenericMappingReferenceFactory[K, E, X, R, M]) :GenericMappingReferenceFactory[K, E, X, R, M] =
			new LazyMappingReferenceFactory(factory)


		class RestrictedMappingReferenceFactory[K, E, X, M<:Mapping[E]](
				val target :M, opressor :Opressor[K, E])(implicit composition :X ComposedOf E)
			extends RestrictedReferenceFactory[K, E, X](opressor) with MappingReferenceFactory[K, E, X, M]
		{ factory =>
			type TargetMapping = M

			override def scout(ctx: ReferenceContext[M]): Reference[X] =
				new LazyMappingReference[X, E, M](ctx.path,
					target.scoutValue(ctx.asInstanceOf[ReferenceContext[target.type]]).map(e => composition(Seq(e)))
				)
				

			override def as[Y](implicit composition: ComposedOf[Y, E]) =
				new RestrictedMappingReferenceFactory(target, opressor)(composition)


			override def equivalencyToken = (target, opressor)

			override def canEqual(that :Any) = that.isInstanceOf[RestrictedMappingReferenceFactory[_, _, _, _]]

			override def equals(that :Any) =
				super[GenericMappingReferenceFactory].equals(that) && super[RestrictedReferenceFactory].equals(that)

			override def toString = s"$composition($opressor[$target])"
		}


		class LazyMappingReferenceFactory[K, E, X, R<:Reference[X], M<:Mapping[E]](factory : => GenericMappingReferenceFactory[K, E, X, R, M])
			extends LazyReferenceFactory[K, E, X, R](()=>factory) with GenericMappingReferenceFactory[K, E, X, R, M]
		{
			override protected def backing = super.backing.asInstanceOf[GenericMappingReferenceFactory[K, E, X, R, M]]

			def target = backing.target

			override def scout(ctx: ReferenceContext[M]): R =
				backing.scout(ctx)

			override def as[Y](implicit composition: ComposedOf[Y, E]): MappingReferenceFactory[K, E, Y, M] =
				new LazyMappingReferenceFactory[K, E, Y, Reference[Y], M](this.backing.as[Y])

		}
//
//		class MappingReferenceFactoryProxy[K, E, T, R<:Reference[T], FK<:Mapping, M<:Mapping[E]](
//				val target :MappingPath[FK, M], factory :GenericReferenceFactory[K, E, T, R])
//			extends ReferenceFactoryProxy[K, E, T, R](factory) with GenericMappingReferenceFactory[K, E, T, R]
//		{
//			type ReferenceComponent = FK
//			type TargetMapping = M
//
//			override def as[X](implicit composition: ComposedOf[X, E]): GenericMappingReferenceFactory[K, E, X, Reference[X]] =
//				new MappingReferenceFactoryProxy(target, references.as[X])
//
//			override def equivalencyToken = (target, references.equivalencyToken)
//
//			override def equals(that :Any) =
//				super[GenericMappingReferenceFactory].equals(that) && super[ReferenceFactoryProxy].equals(that)
//
//			override def toString = s"$references[$target]"
//		}

	}




}
