package net.noresttherein.oldsql.morsels.witness

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}






/** An optional evidence provider for type `T`. An implicit `Maybe[T]` is always present, but, if an implicit value
  * for `T` can be found, it is exposed as [[net.noresttherein.oldsql.collection.Opt.Got Got]]`[T]`
  * through this instance's [[net.noresttherein.oldsql.morsels.witness.Maybe.opt opt]] property.
  */
class Maybe[+T] private[witness] (val opt :Opt[T]) {
	@inline final def getOrElse[U >: T](alternative: => U) :U =
		if (opt.isEmpty) alternative else opt.get //don't use opt.getOrElse as it won't be inlined
}



sealed abstract class MaybeNoImplicit {
	private[this] val no :Maybe[Nothing] = new Maybe(Lack)

	implicit def maybeNo[T] :Maybe[T] = no
}


/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
object Maybe extends MaybeNoImplicit {
	type TypeClass[F[_]] = { type T[X] = Maybe[F[X]] }

	def apply[T](implicit maybe :Maybe[T]) :Opt[T] = maybe.opt

	def unapply[T](maybe :Maybe[T]) :Opt[T] = maybe.opt

	implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(Opt(e))

	def some[T](implicit e :T) :Maybe[T] = new Maybe(Got(e))

	val none :Maybe[Nothing] = new Maybe[Nothing](Lack)

	implicit def explicit[T](value :T) :Maybe[T] = new Maybe(Opt(value))

}






sealed trait WithDefault[+T, +D] //todo: get :T|D


object WithDefault {
	implicit def provided[T](implicit evidence :T) :Provided[T] = Provided(evidence)
	implicit def default[D](implicit evidence :D) :Default[D] = Default(evidence)

	final case class Provided[+T](get :T) extends WithDefault[T, Nothing]
	final case class Default[+D](get :D) extends WithDefault[Nothing, D]
}
