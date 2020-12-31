package net.noresttherein.oldsql.morsels.witness






/** A type for which an implicit value is always present, however, if an implicit value for `T` can be found,
  * it is exposed as `Some[T]` through this instances [[net.noresttherein.oldsql.morsels.witness.Maybe.opt opt]] property.
  */
class Maybe[+T] private[witness] (val opt :Option[T])



sealed abstract class MaybeNoImplicit {
	private[this] val no :Maybe[Nothing] = new Maybe(None)

	implicit def maybeNo[T] :Maybe[T] = no
}


/** Provides optional implicit values if they are available wrapped as `Maybe[T]`. */
object Maybe extends MaybeNoImplicit {
	type TypeClass[F[_]] = { type T[X] = Maybe[F[X]] }

	def apply[T](implicit maybe :Maybe[T]) :Option[T] = maybe.opt

	def unapply[T](maybe :Maybe[T]) :Option[T] = maybe.opt

	implicit def maybeYes[T](implicit e :T) :Maybe[T] = new Maybe(Option(e))

	def some[T](implicit e :T) :Maybe[T] = new Maybe(Some(e))

	val none :Maybe[Nothing] = new Maybe[Nothing](None)

	implicit def explicit[T](value :T) :Maybe[T] = new Maybe(Option(value))

}






sealed trait WithDefault[+T, +D] //todo: get :T|D


object WithDefault {
	implicit def provided[T](implicit evidence :T) :Provided[T] = Provided(evidence)
	implicit def default[D](implicit evidence :D) :Default[D] = Default(evidence)

	final case class Provided[+T](get :T) extends WithDefault[T, Nothing]
	final case class Default[+D](get :D) extends WithDefault[Nothing, D]
}
