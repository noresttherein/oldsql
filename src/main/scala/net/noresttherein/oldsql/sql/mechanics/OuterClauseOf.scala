package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.{Aggregated, Dual, From, FromSome, GroupBy, RowProduct, Subselect}
import net.noresttherein.oldsql.sql.DecoratedRow.{DecoratorDecomposition, ExpandingDecorator}
import net.noresttherein.oldsql.sql.Expanded.{ExpandedDecomposition, NonSubselect}






/** Implicit witness that `O` is the outer clause of `F`. If `F` is a concrete clause, this is exactly `F#Outer`,
  * and the member type should generally be preferred over this implicit value. There three main, closely related
  * differences between the type `O` provided by this implicit value and the member type `F#Outer`, stemming
  * from the fact that it is reconstructed based on the static type `F`, rather than its
  * [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type:
  *   1. Only concrete subtypes of [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] link clauses are used
  *      in its definition, never abstract types. Compare this with `Self`, which, for non-aliased clauses
  *      is always an abstract type with an upper bound such as `type Self <: L InnerJoin R`. This makes this implicit
  *      preferable when using `O` as an invariant type argument, especially in implicit values.
  *   1. Any `Adjoin` and `DecoratedRow` subtype is permitted in `F` and preserved in `O`,
  *      as long as the definition section of `F` following `O` is at least as specific as its
  *      [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] type. This contrasts with `F#Outer`,
  *      as it is always the `Self` type of `O`.
  *   1. If `F` has a wildcard prefix in the form of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]],
  *      [[net.noresttherein.oldsql.sql.FromSome]], [[net.noresttherein.oldsql.sql.GroupByClause]], or any other,
  *      it will be preserved as the init of the clause `O`. An incomplete type used as an upper bound will still be
  *      the upper bound for the corresponding outer prefixes of all clauses conforming to `F`. Compare it with
  *      `F#Outer`, which - as a `Self` type may involve upcasting of `F` if it involves a type extending from a type
  *      with a concrete definition of `Self` as its supertype. On the other hand, if `F` includes abstract
  *      join types - for example, it is in its [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]
  *      form - `F#Outer` is an undefined, but still concrete type in the sense that only clauses with the same
  *      concrete joins types conform to it. In other words, upcasting `F` to its super type does not involve
  *      a corresponding upcasting of `F#Outer` and, what's more, `F <:< F#Self` does not hold.
  */
@implicitNotFound("Cannot determine the outer clause for ${F}: missing implicit OuterClauseOf[${F}, ${O}].")
abstract class OuterClauseOf[+O <: RowProduct, -F <: RowProduct] {
	def apply(subselect :F) :O
}


object OuterClauseOf {
	@inline def apply[O <: RowProduct, F <: RowProduct](from :F)(implicit outer :O OuterClauseOf F) :O =
		outer(from)

	def apply(from :RowProduct) :from.Outer OuterClauseOf from.Self =
		instance.asInstanceOf[from.Outer OuterClauseOf from.Self]

	def generalized(from :RowProduct) :from.Implicit OuterClauseOf from.Generalized =
		instance.asInstanceOf[from.Implicit OuterClauseOf from.Generalized]


	private[this] val instance :OuterClauseOf[RowProduct, RowProduct] = _.outer

	implicit val dual :OuterClauseOf[Dual, Dual] = instance.asInstanceOf[Dual OuterClauseOf Dual]

	implicit def from[T[O] <: MappingAt[O]] :Dual OuterClauseOf From[T] =
		dual.asInstanceOf[OuterClauseOf[Dual, From[T]]]

	implicit def subselect[L <: FromSome, R[A] <: MappingAt[A]] :L OuterClauseOf (L Subselect R) =
		dual.asInstanceOf[OuterClauseOf[L, L Subselect R]]

	implicit def expanded[O <: RowProduct, F <: L J R, L <: U, R[A] <: MappingAt[A],
	                      J[+P <: U, T[A] <: R[A]] <: P NonSubselect T, U <: RowProduct]
	                     (implicit decompose :ExpandedDecomposition[F, L, R, J, U], outer :O OuterClauseOf L)
			:O OuterClauseOf F =
		outer.asInstanceOf[OuterClauseOf[O, F]]

	implicit def decorated[O <: RowProduct, F <: D[P], D[+C <: U] <: ExpandingDecorator[C], P <: U, U <: RowProduct]
	                      (implicit decompose :DecoratorDecomposition[F, P, D, U], outer :O OuterClauseOf P)
			:O OuterClauseOf F =
		outer.asInstanceOf[OuterClauseOf[O, F]]

	implicit def grouped[O <: RowProduct, L <: FromSome, R[A] <: MappingAt[A]]
	                    (implicit outer :O OuterClauseOf L) :O OuterClauseOf (L GroupBy R) =
		outer.asInstanceOf[OuterClauseOf[O, L GroupBy R]]

	implicit def aggregated[O <: RowProduct, F <: FromSome]
	                       (implicit outer :O OuterClauseOf F) :O OuterClauseOf Aggregated[F] =
		outer.asInstanceOf[OuterClauseOf[O, Aggregated[F]]]

}
