package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.{AggregateClause, Aggregated, AndFrom, By, Dual, Extended, From, FromClause, FromSome, GroupBy, GroupByClause, GroupParam, Join, JoinParam, RowProduct, Subselect}
import net.noresttherein.oldsql.sql.DecoratedFrom.{DecoratorDecomposition, ExtendingDecorator}
import net.noresttherein.oldsql.sql.Extended.{ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.RowProduct.{As, NonEmptyFrom}
import net.noresttherein.oldsql.sql.UnboundParam.ParamAt






/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the clause `F`.
  * It is the number of relations available to any `SQLExpression[F, _, _]` and excludes any relations
  * under the `GroupBy` clause (that is, all relations since the `Subselect`/`Dual` preceding it, representing
  * the ''from'' clause of a subselect with a ''group by'' clause), but including any grouping columns/mappings
  * (the right side of the `GroupBy` clause until the next `Subselect` or the end of the clause).
  * This calculation happens completely on the type level and requires that the clause `F` starts with `Dual`
  * (or `From`).
  */
@implicitNotFound("Can't calculate the size of the FROM clause ${F}.\nEither the clause is incomplete " +
                  "or the expected number ${N} is incorrect. Missing implicit: RowProductSize[${F}, ${N}].")
class RowProductSize[-F <: RowProduct, N <: Numeral] private (private val n :Int) extends AnyVal {
	/** The number of tables in the clause `F`. */
	@inline def size :N = n.asInstanceOf[N] //val size :N crashes scalac
}



object RowProductSize {
	implicit val DualCount :RowProductSize[Dual, 0] = new RowProductSize[Dual, 0](0)

	implicit def extended[L <: RowProduct, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
	                     (implicit count :RowProductSize[L, M], plus :Inc[M, N]) :RowProductSize[L Extended R, N] =
		new RowProductSize[L Extended R, N](plus.n)

	implicit def decorated[F <: RowProduct, N <: Numeral](implicit count :RowProductSize[F, N])
			:RowProductSize[ExtendingDecorator[F], N] =
		new RowProductSize(count.size)

	implicit def grouped[F <: FromSome, G[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
	                    (implicit count :RowProductSize[F#Outer, M], plus :Inc[M, N])
			:RowProductSize[F GroupBy G, N] =
		new RowProductSize(plus.n)

	implicit def aggregated[F <: FromSome, N <: Numeral](implicit count :RowProductSize[F#Outer, N])
			:RowProductSize[Aggregated[F], N] =
		new RowProductSize(count.size)

}






/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the explicit portion
  * of the clause `F`, i.e., the relations which are members of the ''from'' clause of the most nested
  * select/subselect based on `F`. This is the number of relations joined since the rightmost `Subselect` 'join'
  * or `Dual`/`From`, if the clause is not a subselect clause. If the clause is a `GroupByClause`, only the relations
  * to the right of the last `GroupBy` are counted in order to reflect that the relations being grouped are
  * not available to SQL expressions based on the clause. The calculation happens completely on the type level
  * and requires that the kinds of all joins starting with the last `Subselect`/`Dual` are known at least to
  * their `Generalized` form to correctly determine the beginning of the explicit suffix of the clause.
  */
@implicitNotFound("Can't count the number of relations in the most nested Subselect in ${F}.\n"+
                  "Most likely the type's Generalized form is not known. " +
                  "Missing implicit: SubselectClauseSize[${F}, ${N}].")
class SubselectClauseSize[-F <: RowProduct, N <: Numeral] private (private val n :Int) extends AnyVal {
	@inline def size :N = n.asInstanceOf[N]
}



object SubselectClauseSize {
	implicit val DualCount :SubselectClauseSize[Dual, 0] = new SubselectClauseSize[Dual, 0](0)

	implicit def from[T[O] <: MappingAt[O]] :SubselectClauseSize[From[T], 1] = new SubselectClauseSize[From[T], 1](1)

	implicit def subselect[T[O] <: MappingAt[O]] :SubselectClauseSize[NonEmptyFrom Subselect T, 1] =
		new SubselectClauseSize[NonEmptyFrom Subselect T, 1](1)

	implicit def grouped[L <: FromSome, T[O] <: MappingAt[O]] :SubselectClauseSize[L GroupBy T, 1] =
		new SubselectClauseSize[L GroupBy T, 1](1)

	implicit def aggregated[F <: FromSome] :SubselectClauseSize[Aggregated[F], 0] =
		new SubselectClauseSize[Aggregated[F], 0](0)

	implicit def extended[F <: L J R, L <: U, R[O] <: MappingAt[O],
	                      J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: RowProduct, M <: Numeral, N <: Numeral]
	                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U],
	                      prev :SubselectClauseSize[L, M], plus :Inc[M, N]) :SubselectClauseSize[F, N] =
		new SubselectClauseSize[F, N](plus.n)

	implicit def decorated[D <: ExtendingDecorator[F], F <: RowProduct, N <: Numeral]
	                      (implicit decompose :Conforms[D, D, ExtendingDecorator[F]], prev :SubselectClauseSize[F, N])
			:SubselectClauseSize[D, N] =
		new SubselectClauseSize[D, N](prev.n)

}






/** Implicit witness to the fact that the `Int` literal `N` is the number of ''known'' relations present
  * in the clause `F`. It is the number of relations available to any `SQLExpression` based on the type `F`.
  * This counts all relations joined since the leftmost terminator clause - which may be a wildcard: `RowProduct`,
  * `FromSome`, `FromClause`, `GroupByClause` - ignoring any relations which are grouped by a `GroupBy` 'join'
  * or the `Aggregated` decorator. This calculation happens completely on the type level and, for it to work,
  * it requires that the join kinds are known at least to the level allowing distinguish between `GroupBy`
  * and others and, if a `GroupBy` clause is present somewhere in the type, that all joins preceding it
  * since the most recent `Subselect` (or `Dual`) are known at least to their `Generalized` form in order
  * to properly exclude the relations under grouping from the count.
  */
@implicitNotFound("Failed to count the tables in ${F}. Is ${N} the number of mappings listed in its definition?\n" +
                  "Note that witness TableCount[F, N] is invariant in type F, but requires that it starts " +
                  "with one of RowProduct, GroupByClause, FromClause, FromSome, or Dual/From[_].")
class TableCount[F <: RowProduct, N <: Numeral] private[mechanics] (private val n :Int) extends AnyVal {
	/** The number of relations listed in the type `F` (not including any relations hidden by a wildcard/abstract prefix). */
	@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac

	/** The zero-based index of the first relation in the clause when counting from the right.
	  * @return `tables - 1`.
	  */
	@inline def offset :Int = n - 1
}



object TableCount {
	implicit final val RowProductHasZero :TableCount[RowProduct, 0] = new TableCount[RowProduct, 0](0)
	implicit final val FromClauseHasZero :TableCount[FromClause, 0] = new TableCount[FromClause, 0](0)
	implicit final val FromSomeHasZero :TableCount[FromSome, 0] = new TableCount[FromSome, 0](0)
	implicit final val DualHasZero :TableCount[Dual, 0] = new TableCount[Dual, 0](0)
	implicit final val GroupByClauseHasZero :TableCount[GroupByClause, 0] = new TableCount[GroupByClause, 0](0)
	implicit final val AggregateClauseHasZero :TableCount[AggregateClause, 0] = new TableCount[AggregateClause, 0](0)

	implicit def extended[F <: L J R, L <: U, R[O] <: MappingAt[O],
	                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: RowProduct, M <: Numeral, N <: Numeral]
	                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U],
	                      count :TableCount[L, M], inc :Inc[M, N])
			:TableCount[F, N] =
		new TableCount[F, N](inc.n)

	implicit def decorated[E <: D[F], F <: U, D[+C <: U] <: ExtendingDecorator[C], U <: RowProduct, N <: Numeral]
	                      (implicit decompose :DecoratorDecomposition[E, F, D, U], count :TableCount[F, N])
			:TableCount[E, N] =
		new TableCount[E, N](count.tables)

	implicit def grouped[O <: RowProduct, F <: FromSome, T[A] <: MappingAt[A], M <: Numeral, N <: Numeral]
	                    (implicit outer :O OuterClauseOf F, ungrouped :TableCount[F, M], plus :Inc[M, N])
			:TableCount[F GroupBy T, N] =
		new TableCount[F GroupBy T, N](plus.n)

	implicit def aggregated[O <: RowProduct, F <: FromSome, N <: Numeral]
	                       (implicit outer :O OuterClauseOf F, ungrouped :TableCount[F, N])
			:TableCount[Aggregated[F], N] =
		new TableCount[Aggregated[F], N](ungrouped.tables)

	implicit def aliased[F <: NonEmptyFrom, A <: Label, N <: Numeral]
	                    (implicit count :TableCount[F, N]) :TableCount[F As A, N] =
		new TableCount[F As A, N](count.tables)
}






/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the ''from'' clause
  * of the most nested subselect of `F`. This is the number of relations to the right since the most recent
  * `Subselect` or `Dual`/`From` if `F <: FromClause`, and the number of relations to the right of
  * the most recent `GroupBy` clause if `F <: GroupByClause`. This calculation happens solely
  * on the type level and requires that the `Generalized` form of the pertinent clause fragment is known
  * in order to properly recognize the cut off point.
  */
@implicitNotFound("Failed to count joined relations since last Subselect in ${F}.\n" +
                  "Most likely the type contains joins with unknown Generalized form or it starts with" +
                  "an undefined prefix other than RowProduct, GroupByClause, FromClause, FromSome, Dual, From. " +
                  "Missing implicit SubselectTableCount[${F}, ${N}]")
class SubselectTableCount[F <: RowProduct, N <: Numeral] private (private val n :Int) extends AnyVal {
	@inline def tables :N = n.asInstanceOf[N]
}



object SubselectTableCount {
	implicit def zero[F <: RowProduct](implicit count :TableCount[F, 0]) :SubselectTableCount[F, 0] =
		new SubselectTableCount[F, 0](0)

	implicit def from[T[A] <: MappingAt[A]] :SubselectTableCount[From[T], 1] =
		new SubselectTableCount[From[T], 1](1)

	implicit def subselect[F <: NonEmptyFrom, T[A] <: MappingAt[A]] :SubselectTableCount[F Subselect T, 1] =
		new SubselectTableCount[F Subselect T, 1](1)

	implicit def groupBy[F <: FromSome, T[A] <: MappingAt[A]] :SubselectTableCount[F GroupBy T, 1] =
		new SubselectTableCount[F GroupBy T, 1](1)

	implicit def aggregated[F <: FromSome] :SubselectTableCount[Aggregated[F], 0] =
		new SubselectTableCount[Aggregated[F], 0](0)

	implicit def extended[E <: L J R, L <: U, R[O] <: MappingAt[O],
	                      J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: RowProduct, M <: Numeral, N <: Numeral]
	             (implicit decompose :ExtendedDecomposition[E, L, R, J, U],
	              prev :SubselectTableCount[E, M], inc :Inc[M, N])
			:SubselectTableCount[E, N] =
		new SubselectTableCount[E, N](inc.n)

	implicit def decorated[E <: D[C], C <: U, D[+B <: U] <: ExtendingDecorator[B], U <: RowProduct, N <: Numeral]
	                      (implicit decompose :DecoratorDecomposition[E, C, D, U], count :SubselectTableCount[C, N])
			:SubselectTableCount[E, N] =
		new SubselectTableCount[E, N](count.tables)

	implicit def aliased[F <: NonEmptyFrom, A <: Label, N <: Numeral]
	                    (implicit count :SubselectTableCount[F, N]) :SubselectTableCount[F As A, N] =
		new SubselectTableCount[F As A, N](count.tables)
}






/** Implicit witness that `M` is the mapping for the first known relation in the clause `F`
  * and `N` is the number of relations to its right. Any relations under the scope of a `GroupBy`
  * (between the last `Subselect` or the first relation and `GroupBy`) are excluded from the count
  * and `M` must not be such a relation. The clause `F` must be in the form
  * `L E M G1 T1 ... Gn Tn`, where `L` is the upper bound of the left side of the extension `E` and all `Gi`
  * are extension with a definite `Generalized` form. `From[M]` can replace `L E M` in the above definition.
  * This evidence is invariant in `F` in order to maintain the invariant of `M` being the first `Mapping` type
  * listed and preserve the correct relation count `N`. When `F` is used as the `Origin` type for the mapping `M`,
  * this allows to track back any component of `M` back to the relation it originated from.
  */
@implicitNotFound("Relation mapping ${M} is not the first known mapping of the FROM clause ${F}: "+
                  "no implicit TableShift[${F}, ${M}, ${N}].")
class TableShift[F <: RowProduct, M[O] <: MappingAt[O], N <: Numeral](private val n :Int) extends AnyVal {
	@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
}



object TableShift {
	//this is the only one really needed by JoinedRelations + GetTable, which use FromLast
	implicit def firstAndFrom[T[A] <: MappingAt[A]] :TableShift[RowProduct AndFrom T, T, 0] =
		new TableShift[RowProduct AndFrom T, T, 0](0)

	//these firstXxx are here mostly to allow other uses of TableShift
	implicit def firstFrom[T[A] <: MappingAt[A]] :TableShift[From[T], T, 0] =
		new TableShift[From[T], T, 0](0)

	implicit def firstJoin[J[+L <: FromSome, R[A] <: MappingAt[A]] <: L Join R, T[A] <: MappingAt[A]]
			:TableShift[FromSome J T, T, 0] =
		new TableShift[FromSome J T, T, 0](0)

	implicit def firstSubselect[T[A] <: MappingAt[A]] :TableShift[NonEmptyFrom Subselect T, T, 0] =
		new TableShift[NonEmptyFrom Subselect T, T, 0](0)

	implicit def firstParam[P[A] <: ParamAt[A]] :TableShift[FromSome JoinParam P, P, 0] =
		new TableShift[FromSome JoinParam P, P, 0](0)

	implicit def firstGroupParam[P[A] <: ParamAt[A]] :TableShift[GroupByClause GroupParam P, P, 0] =
		new TableShift[GroupByClause GroupParam P, P, 0](0)

	implicit def firstBy[G[A] <: MappingAt[A]] :TableShift[GroupByClause By G, G, 0] =
		new TableShift[GroupByClause By G, G, 0](0)

	implicit def firstGroupBy[G[A] <: MappingAt[A]] :TableShift[FromSome GroupBy G, G, 0] =
		new TableShift[FromSome GroupBy G, G, 0](0)



	implicit def extended[E <: L J R, L <: U, R[O] <: MappingAt[O],
	                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: RowProduct,
	                      T[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
	                     (implicit decompose :ExtendedDecomposition[E, L, R, J, U],
	                      prev :TableShift[L, T, M], inc :Inc[M, N])
			:TableShift[E, T, N] =
		new TableShift[E, T, N](inc.n)

	implicit def decorated[E <: D[C], C <: U, D[+B <: U] <: ExtendingDecorator[B], U <: RowProduct,
	                       T[O] <: MappingAt[O], N <: Numeral]
	                      (implicit decompose :DecoratorDecomposition[E, C, D, U], body :TableShift[C, T, N])
			:TableShift[E, T, N] =
		new TableShift[E, T, N](body.tables)

	implicit def grouped[O <: RowProduct, F <: FromSome, T[A] <: MappingAt[A], M <: Numeral, N <: Numeral]
	                    (implicit outer :O OuterClauseOf F, ungrouped :TableShift[O, T, M], plus :Inc[M, N])
			:TableShift[F GroupBy T, T, N] =
		new TableShift[F GroupBy T, T, N](plus.n)

	implicit def aggregated[O <: RowProduct, F <: FromSome, T[A] <: MappingAt[A], N <: Numeral]
	                       (implicit outer :O OuterClauseOf F, ungrouped :TableShift[O, T, N])
			:TableShift[Aggregated[F], T, N] =
		new TableShift[Aggregated[F], T, N](ungrouped.tables)

	implicit def aliased[F <: NonEmptyFrom, A <: Label, T[O] <: MappingAt[O], N <: Numeral]
	                    (implicit shift :TableShift[F, T, N]) :TableShift[F As A, T, N] =
		new TableShift[F As A, T, N](shift.tables)
}

