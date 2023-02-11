package com.hcore.ogre.model

import com.hcore.ogre.model.CompositeReference.AbstractCompositeReference
import com.hcore.ogre.model.MappedReference.ReferenceMapper
import com.hcore.ogre.model.Reference.{Full, Lazy, GenericReferenceFactory, ReferenceFactory}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.SaferCasts


//implicits
import SaferCasts._



trait MappedReference[S, +T] extends Reference[T] {
	def source :Reference[S]
	def fun :ReferenceMapper[S, T]

	lazy val toOpt = source.toOpt.map(fun)
	
	override def toString = s"$source.map($fun)"

}



object MappedReference {
	trait ReferenceMapper[-X, +Y] extends (X=>Y) {
		def apply(x :X) :Y
		def apply(ref :Reference[X]) :Reference[Y] = ref.map(this)
	}

	def apply[S, T](source :Reference[S], map :ReferenceMapper[S, T]) :MappedReference[S, T] =
		new BaseMappedReference(source, map)

	def unapply[T](ref :Reference[T]) :Option[(Reference[S], ReferenceMapper[S, T]) forSome { type S }] =
		ref.ifSubclass[MappedReference[Any, T]]{ r => (r.source, r.fun) }




	private case class BaseMappedReference[S, +T](source :Reference[S], fun :ReferenceMapper[S, T]) extends MappedReference[S, T]
//	trait AbstractCompositeMappedReference[SS, S, TT, T] extends AbstractCompositeReference[TT, T] with MappedReference[TT, T]


	case class PropertyMapper[-X, +Y](property :PropertyChain[X, Y]) extends ReferenceMapper[X, Y] {
		def apply(x :X) = property(x)
		override def toString = "_."+property
	}

	case class CompositeMapper[XS, X, Y, YS](element :ReferenceMapper[X, Y])(implicit source :ComposedOf[XS, X], target :YS ComposedOf Y)
		extends ReferenceMapper[XS, YS]
	{
		def apply(xs :XS) = target.composition(source.decomposition(xs).map(element))

		override def toString = s"_.map(_.$element)"
	}


	case class ComposingMapper[X, Y](implicit compose :Y ComposedOf X) extends ReferenceMapper[X, Y] {
		def apply(x :X) = compose(Seq(x))

		override def apply(ref: Reference[X]): Reference[Y] =
			if (compose==ComposedOf.itself[X]) ref.asInstanceOf[Reference[Y]]
			else ref.map(this)

		override def toString = s"$compose(_)"
	} 
	

//	class MappedReferenceFactory[K, E, ES, F, FS](
//			mapper :ReferenceMapper[ES, F], source :ReferenceFactory[K, E, ES])(implicit val items :FS ComposedOf F)
//		extends GenericReferenceFactory[K, F, FS, Reference[FS]]
//	{
//		val composer = new ComposingMapper
//		override def delayed(key: K, value: => FS): Reference[FS] = Lazy(value)
//
//		override def full(value: FS): Reference[FS] = Full(value)
//
//		override def empty(key: K): Reference[FS] = composer(source.empty(key).map(mapper))
//
//		override def keyFor(item: F): Option[K] = None
//
//		override def keyOf[R >: Reference[FS] <: Reference[FS]](ref: R): Option[K] =
//
//
//
//		def wrap(ref :Reference[E]) = ref.map(mapper)
//
//		override def as[Y](implicit composition: ComposedOf[Y, F]): ReferenceFactory[K, F, Y] = ???
//
//		override def equivalencyToken: Any = ???
//	}

}