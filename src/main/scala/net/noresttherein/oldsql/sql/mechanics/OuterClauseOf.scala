package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.{Aggregated, Dual, From, FromSome, GroupBy, RowProduct, Subselect}
import net.noresttherein.oldsql.sql.DecoratedFrom.{DecoratorDecomposition, ExtendingDecorator}
import net.noresttherein.oldsql.sql.Extended.{ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.RowProduct.{As, NonEmptyFrom}


/** Implicit witness that `O` is the outer clause of `F`. If `F` is a concrete clause, this is exactly `F#Outer`,
  * and the member type should generally be preferred over this implicit value. There two main, closely related
  * differences between the type `O` provided by this implicit value and the member type `F#Outer`:
  *   1. The static type `O` is defined exactly how it appears as a prefix of the static type `F`:
  *      when viewed as a syntax tree, `O` is literally a node under `F`, without any subtyping involved in any
  *      direction, which is important in use cases which depend on preserving the exact types of joins used.
  *   1. It follows from the above, that if `F` has a wildcard prefix in the form of
  *      [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], [[net.noresttherein.oldsql.sql.FromSome]],
  *      [[net.noresttherein.oldsql.sql.GroupByClause]], or any other, it will be preserved as the init
  *      of the clause `O`. An incomplete type used as an upper bound will still be the upper bound for
  *      the corresponding outer prefixes of all clauses conforming to `F`. Compare it with `F#Outer`,
  *      which - as a `Self` type may involve upcasting of `F` if it involves a type extending from a type
  *      with a concrete definition of `Self` as its supertype. On the other hand, if `F` includes abstract
  *      join types - for example, it is in its [[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]]
  *      form - `F#Outer` is an undefined, but still concrete type in the sense that only clauses with the same
  *      concrete joins types conform to it. In other words, upcasting `F` to its super type does not involve
  *      a corresponding upcasting of `F#Outer` and, what's more, `F <:< F#Self` does not hold.
  */
@implicitNotFound("Cannot determine the outer clause for ${F}: missing implicit OuterClauseOf[${F}, ${O}].")
abstract class OuterClauseOf[O <: RowProduct, F <: RowProduct] {
	def apply(subselect :F) :O
}


object OuterClauseOf {
	@inline def apply[O <: RowProduct, F <: RowProduct](from :F)(implicit outer :O OuterClauseOf F) :O =
		outer(from)


	private[this] val instance :OuterClauseOf[RowProduct, RowProduct] = _.outer

	implicit val dual :OuterClauseOf[Dual, Dual] = instance.asInstanceOf[Dual OuterClauseOf Dual]

	implicit def from[T[O] <: MappingAt[O]] :Dual OuterClauseOf From[T] =
		dual.asInstanceOf[OuterClauseOf[Dual, From[T]]]

	implicit def subselect[L <: FromSome, R[A] <: MappingAt[A]] :L OuterClauseOf (L Subselect R) =
		dual.asInstanceOf[OuterClauseOf[L, L Subselect R]]

	implicit def extended[O <: RowProduct, F <: L J R, L <: U, R[A] <: MappingAt[A],
	                      J[+C <: U, T[A] <: R[A]] <: NonSubselect[C, T], U <: RowProduct]
	                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U], outer :O OuterClauseOf L)
			:O OuterClauseOf F =
		outer.asInstanceOf[OuterClauseOf[O, F]]

	implicit def adapted[O <: RowProduct, F <: D[C], D[+B <: U] <: ExtendingDecorator[B], C <: U, U <: RowProduct]
	                    (implicit decompose :DecoratorDecomposition[F, C, D, U], outer :O OuterClauseOf C)
			:OuterClauseOf[O, F] =
		outer.asInstanceOf[OuterClauseOf[O, F]]

	implicit def grouped[O <: RowProduct, L <: FromSome, R[A] <: MappingAt[A]]
	                    (implicit outer :O OuterClauseOf L) :O OuterClauseOf (L GroupBy R) =
		outer.asInstanceOf[OuterClauseOf[O, L GroupBy R]]

	implicit def aggregated[O <: RowProduct, F <: FromSome]
	                       (implicit outer :O OuterClauseOf F) :O OuterClauseOf Aggregated[F] =
		outer.asInstanceOf[OuterClauseOf[O, Aggregated[F]]]

	implicit def aliased[O <: RowProduct, F <: NonEmptyFrom, A <: Label](implicit outer :O OuterClauseOf F)
			:O OuterClauseOf (F As A) =
		outer.asInstanceOf[O OuterClauseOf (F As A)]

}