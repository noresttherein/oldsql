package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.MappedKin.KinMapper
import net.noresttherein.oldsql.morsels.PropertyChain
import net.noresttherein.oldsql.slang.SaferCasts._



trait MappedKin[S, +T] extends Kin[T] {
	def source :Kin[S]
	def fun :KinMapper[S, T]

	override def isEmpty :Boolean = source.isEmpty

	override lazy val toOpt :Option[T] = source.toOpt.map(fun)

	override def toString = s"$source.map($fun)"
}



object MappedKin {
	sealed trait KinMapper[-X, +Y] extends (X=>Y) {
		def apply(x :X) :Y
		def apply(kin :Kin[X]) :Kin[Y] = kin.map(this)
	}

	def apply[S, T](source :Kin[S], map :KinMapper[S, T]) :MappedKin[S, T] =
		new BaseMappedKin(source, map)

	def unapply[T](ref :Kin[T]) :Option[(Kin[S], KinMapper[S, T]) forSome { type S }] =
		ref.ifSubclass[MappedKin[Any, T]]{ r => (r.source, r.fun) }



	private case class BaseMappedKin[S, +T](source :Kin[S], fun :KinMapper[S, T]) extends MappedKin[S, T]



	case class PropertyMapper[-X, +Y](property :PropertyChain[X, Y]) extends KinMapper[X, Y] {
		def apply(x :X) :Y = property(x)
		override def toString :String = "_."+property
	}


	case class CompositeMapper[XS, X, Y, YS](element :KinMapper[X, Y])(implicit source :XS ComposedOf X, target :YS ComposedOf Y)
		extends KinMapper[XS, YS]
	{
		def apply(xs :XS) :YS = target.composition(source.decomposition(xs).map(element))

		override def toString = s"_.map(_.$element)"
	}


	case class ComposingMapper[X, Y](implicit compose :Y ComposedOf X) extends KinMapper[X, Y] {
		def apply(x :X) :Y = compose(Seq(x))

		override def apply(ref: Kin[X]): Kin[Y] =
			if (compose == ComposedOf.itself[X]) ref.asInstanceOf[Kin[Y]]
			else ref.map(this)

		override def toString :String = compose + "(_)"
	}


	//	class MappedKinFactory[K, E, ES, F, FS](
	//			mapper :KinMapper[ES, F], source :KinFactory[K, E, ES])(implicit val items :FS ComposedOf F)
	//		extends GenericKinFactory[K, F, FS, Kin[FS]]
	//	{
	//		val composer = new ComposingMapper
	//		override def delayed(key: K, value: => FS): Kin[FS] = Lazy(value)
	//
	//		override def full(value: FS): Kin[FS] = Full(value)
	//
	//		override def empty(key: K): Kin[FS] = composer(source.empty(key).map(mapper))
	//
	//		override def keyFor(item: F): Option[K] = None
	//
	//		override def keyOf[R >: Kin[FS] <: Kin[FS]](ref: R): Option[K] =
	//
	//
	//
	//		def wrap(ref :Kin[E]) = ref.map(mapper)
	//
	//		override def as[Y](implicit composition: ComposedOf[Y, F]): KinFactory[K, F, Y] = ???
	//
	//		override def equivalencyToken: Any = ???
	//	}

}