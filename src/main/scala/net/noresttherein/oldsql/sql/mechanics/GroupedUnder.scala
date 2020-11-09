package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, FromSome, GroupBy, GroupByClause, RowProduct}
import net.noresttherein.oldsql.sql.DecoratedFrom.{DecoratorDecomposition, ExtendingDecorator}
import net.noresttherein.oldsql.sql.Extended.ExtendedDecomposition
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.RowProduct.As







/** Implicit witness that `F` is the actual ''from'' clause grouped by in `G`, that is either `G <: Aggregated[F]`,
  * or `F GroupBy M` is the last occurrence of [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] in `G`.
  * If `G` is a concrete type consisting of concrete joins,
  * then `F =:= G#`[[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] (assuming that `G =:= G#Self`,
  * rather than `G <:< G#Self`), and the member type should generally be preferred to this implicit. However,
  * in situations where `G` is used as upper bound, the expectation is typically to use `F` in the same capacity.
  * This implicit preserves type `F` exactly as it appears as a part of type `G`, in particular preserving
  * any wildcard type as its prefix, but also the join types. In contrast, `G#Discrete` will remain a concrete,
  * but unspecified type `g.Discrete forSome { val g :G }`, matching only the concrete type of `G` (and, in reality,
  * only g.type due to inability to compare this unspecified type with any other). Thus, in particular,
  * `G#Generalized#Discrete` is not a generalized type, but its some subtype (see `G#GeneralizedDiscrete` for that).
  */
@implicitNotFound("Cannot determine the FROM clause of ${G}: missing implicit GroupedUnder[${F}, ${G}].")
abstract class GroupedUnder[+F <: RowProduct, -G <: RowProduct] {
	def apply(grouping :G) :F
}


object GroupedUnder {
	@inline def apply[F <: FromSome, G <: RowProduct](from :G)(implicit grouping :F GroupedUnder G) :F =
		grouping(from)


	implicit def andBy[F <: RowProduct, G <: L J R, L <: U, R[O] <: MappingAt[O],
	                   J[+C <: U, T[A] <: R[A]] <: C AndBy T, U <: GroupByClause]
	                  (implicit decompose :ExtendedDecomposition[G, L, R, J, U], from :F GroupedUnder L)
			:F GroupedUnder G =
		instance.asInstanceOf[F GroupedUnder G]

	implicit def decorated[F <: RowProduct, G <: D[C], D[+B <: U] <: ExtendingDecorator[B], C <: U, U <: RowProduct]
	                      (implicit decompose :DecoratorDecomposition[G, C, D, U], from :F GroupedUnder C)
			:F GroupedUnder G =
		instance.asInstanceOf[F GroupedUnder G]

	implicit def groupedBy[F <: FromSome, M[O] <: MappingAt[O]] :F GroupedUnder (F GroupBy M) =
		instance.asInstanceOf[F GroupedUnder (F GroupBy M)]

	implicit def aggregated[F <: FromSome] :F GroupedUnder Aggregated[F] =
		instance.asInstanceOf[F GroupedUnder Aggregated[F]]

	private[this] val instance :GroupedUnder[FromSome, AggregateClause] = _.fromClause
}
