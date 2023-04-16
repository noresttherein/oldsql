package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral, PositiveInc}
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.{Adjoin, AggregateClause, Aggregated, AndBy, AndFrom, By, Dual, Expanded, From, FromClause, FromSome, GroupBy, GroupByClause, GroupParam, Join, JoinParam, NonParam, RowProduct, SelectFrom, Subselect}
import net.noresttherein.oldsql.sql.DecoratedRow.{DecoratorComposition, DecoratorDecomposition, ExpandingDecorator}
import net.noresttherein.oldsql.sql.Expanded.{ExpandedComposition, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.ParamClause.ParamAt
import net.noresttherein.oldsql.sql.RowProduct.{As, EmptyRow, ExpandedBy, NonEmptyRow, PrefixOf, WildcardRow}
import net.noresttherein.oldsql.sql.ast.{JoinedGrouping, JoinedRelation, JoinedTable}

//implicits
import net.noresttherein.oldsql.slang._






/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the clause `F`.
  * It is the number of relations available to any `SQLExpression[F, _, _]` and excludes any relations
  * under the `GroupBy` clause (that is, all relations since the `Subselect`/`Dual` preceding it, representing
  * the ''from'' clause of a subselect with a ''group by'' clause), but including any grouping columns/mappings
  * (the right side of the `GroupBy` clause until the next `Subselect` or the end of the clause).
  * This calculation happens completely on the type level and requires that the clause `F` starts with `Dual`
  * (or `From`), i.e. that it does not include a wildcard prefix.
  */ //todo: make type param N a member type
@implicitNotFound("Can't calculate the size of the FROM clause ${F}.\nEither the clause is incomplete " +
                  "or the expected number ${N} is incorrect. Missing implicit: RowProductSize[${F}, ${N}].")
class RowProductSize[-F <: RowProduct, N <: Numeral] private (private val n :Int) extends AnyVal {
	/** The number of tables in the clause `F`. */
	@inline def size :N = n.asInstanceOf[N] //val size :N crashes scalac
}



object RowProductSize {
	type Of[-F <: RowProduct] = RowProductSize[F, _ <: Numeral]

	def apply[F <: RowProduct](from :F) :RowProductSize.Of[F] = new RowProductSize[F, Numeral](from.fullSize)


	implicit val DualIsEmpty :RowProductSize[Dual, 0] = new RowProductSize[Dual, 0](0)

	implicit def expanded[L <: RowProduct, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
	                     (implicit count :RowProductSize[L, M], plus :Inc[M, N]) :RowProductSize[L Expanded R, N] =
		new RowProductSize[L Expanded R, N](plus.n)

	implicit def decorated[F <: RowProduct, N <: Numeral](implicit count :RowProductSize[F, N])
			:RowProductSize[ExpandingDecorator[F], N] =
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
  * their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] form to correctly determine the beginning
  * of the explicit suffix of the clause.
  *
  * The difference from [[net.noresttherein.oldsql.sql.mechanics.SubselectRelationCount SubselectTableCount]] is that
  * this evidence requires that the generalized types of all joins (or pseudo joins) in the
  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] portion of `F` to be known (in case of grouped clauses,
  * at least since the last [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause, not necessarily the ungrouped
  * ''from'' clause preceding it) and, as the result, specifies the complete size of the clause as it would be returned
  * by [[net.noresttherein.oldsql.sql.RowProduct.size F#size]]. The full knowledge allows it to be also covariant in `F`.
  * On the other hand, `SubselectTableCount` witnesses the number of mappings listed in type `F`, exactly as stated.
  * Implicit values will exist for any generalized clause type `F`, regardless of any of its prefix 'hidden'
  * by a wildcard `RowProduct` type, but requires in turn invariance in `F`.
  */ //todo: make type param N a member type
@implicitNotFound("Can't count the number of relations in the most nested Subselect in ${F}.\n"+
                  "Most likely the type's Generalized form is not known. " +
                  "Missing implicit: ExplicitSize[${F}, ${N}].")
class SubselectSize[-F <: RowProduct, N <: Numeral] private (private val n :Int) extends AnyVal {
	@inline def size :N = n.asInstanceOf[N]
}



object SubselectSize {
	type Of[-F <: RowProduct] = SubselectSize[F, _ <: Numeral] //todo: rename to of

	def apply[F <: RowProduct](from :F) :SubselectSize.Of[F] = new SubselectSize[F, Numeral](from.size)


	implicit val DualIsEmpty :SubselectSize[Dual, 0] = new SubselectSize[Dual, 0](0)

	implicit def from[T[O] <: MappingAt[O]] :SubselectSize[RowProduct SelectFrom T, 1] =
		new SubselectSize[RowProduct SelectFrom T, 1](1)

	implicit def grouped[L <: FromSome, T[O] <: MappingAt[O]] :SubselectSize[L GroupBy T, 1] =
		new SubselectSize[L GroupBy T, 1](1)

	implicit def aggregated[F <: FromSome] :SubselectSize[Aggregated[F], 0] =
		new SubselectSize[Aggregated[F], 0](0)

	implicit def expanded[F <: L J R, L <: U, R[O] <: MappingAt[O],
	                      J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: RowProduct, M <: Numeral, N <: Numeral]
	                     (implicit decompose :ExpandedDecomposition[F, L, R, J, U],
	                      prev :SubselectSize[L, M], plus :Inc[M, N]) :SubselectSize[F, N] =
		new SubselectSize[F, N](plus.n)

	implicit def decorated[D <: ExpandingDecorator[F], F <: RowProduct, N <: Numeral]
	             (implicit decompose :InferTypeParams[D, D, ExpandingDecorator[F]], prev :SubselectSize[F, N])
			:SubselectSize[D, N] =
		new SubselectSize[D, N](prev.n)

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
  *
  * Note the difference from [[net.noresttherein.oldsql.sql.mechanics.RowProductSize RowProductSize]], which witnesses
  * to the complete number of relations in any instance of `F`, as it would be returned by
  * [[net.noresttherein.oldsql.sql.RowProduct.fullSize F#fullSize]]
  * (and [[net.noresttherein.oldsql.sql.RowProduct.fullTableStack F#fullTableStack.size]]), while this evidence counts
  * only the statically listed relations (in other words, the lower bound of the full size of any instance of `F`),
  * which requires it to be invariant in `F`, where the former is contravariant (but requires that `F` does not start
  * with a wildcard prefix type).
  */
//todo: make type param N a member type
//todo: make it a type alias for RelationOffset or vice versa
//todo: rename to KnownRelationCount or ListedRelationCount
//todo: make N a member type
@implicitNotFound("Failed to count the relations in ${F}. Is ${N} the number of mappings listed in its definition?\n" +
                  "Note that witness RelationCount[F, N] is invariant in type F, but requires that it starts " +
                  "with one of RowProduct, GroupByClause, FromClause, FromSome, or Dual/From[_].")
class RelationCount[F <: RowProduct, N <: Numeral] private[sql] (private val n :Int) extends AnyVal {
	/** The number of relations listed in the type `F` (not including any relations hidden by a wildcard/abstract prefix). */
	@inline def knownSize :N = n.asInstanceOf[N] //val tables :N crashes scalac

	/** The zero-based index of the first known relation in the clause when counting from the right.
	  * @return `knownSize - 1`.
	  */
	@inline def offset :Int = n - 1
}



private[mechanics] sealed abstract class Rank1RelationCountImplicits {
//todo: uncomment when Scala fixes the bug which hangs the compiler on DeleteTest

//	implicit def first[L >: WildcardRow <: RowProduct, J[A >: L <: L, B[O] >: M[O] <: M[O]] <: A Adjoin B, M[O] <: MappingAt[O]]
//			:RelationCount[L J M, 1] =
//		new RelationCount[L J M, 1](1)
}
//todo: common base trait with SubselectCount
object RelationCount extends Rank1RelationCountImplicits {
	type In[F <: RowProduct] = RelationCount[F, _ <: Numeral]

	implicit def emptyHasZero[F <: EmptyRow] :RelationCount[F, 0] = new RelationCount[F, 0](0)
	implicit val RowProductHasZero = new RelationCount[RowProduct, 0](0)
	implicit val NonEmptyRowHasZero = new RelationCount[NonEmptyRow, 0](0)
	implicit val FromClauseHasZero = new RelationCount[FromClause, 0](0)
	implicit val FromSomeHasZero = new RelationCount[FromSome, 0](0)
	implicit val GroupByClauseHasZero = new RelationCount[GroupByClause, 0](0)
	//todo: this one should replace the above when Scala fixes a bug which hangs the compiler.
//	implicit def wildcardHasZero[F >: WildcardRow <: RowProduct] :RelationCount[F, 0] = new RelationCount[F, 0](0)

	implicit def from[T[A] <: MappingAt[A]] :RelationCount[From[T], 1] = new RelationCount[From[T], 1](1)


	implicit def expanded[L <: RowProduct, J[+A <: L, B[O] <: R[O]] <: A Expanded B, R[O] <: MappingAt[O],
	                      M <: Numeral, N <: Numeral]
	                     (implicit count :RelationCount[L, M], inc :Inc[M, N])
			:RelationCount[L J R, N] =
		new RelationCount[L J R, N](inc.n)

	implicit def decorated[F <: RowProduct, D[+C <: F] <: ExpandingDecorator[C], N <: Numeral]
	                      (implicit count :RelationCount[F, N])
			:RelationCount[D[F], N] =
		new RelationCount[D[F], N](count.knownSize)

	implicit def grouped[O <: RowProduct, L <: FromSome, R[A] <: MappingAt[A], M <: Numeral, N <: Numeral]
	                    (implicit outer :O OuterClauseOf L, ungrouped :RelationCount[L, M], plus :Inc[M, N])
			:RelationCount[L GroupBy R, N] =
		new RelationCount[L GroupBy R, N](plus.n)

	implicit def aggregated[O <: RowProduct, F <: FromSome, N <: Numeral]
	                       (implicit outer :O OuterClauseOf F, ungrouped :RelationCount[F, N])
			:RelationCount[Aggregated[F], N] =
		new RelationCount[Aggregated[F], N](ungrouped.knownSize)

	implicit def aliased[F <: NonEmptyRow, A <: Label, N <: Numeral]
	                    (implicit count :RelationCount[F, N]) :RelationCount[F As A, N] =
		new RelationCount[F As A, N](count.knownSize)
}






/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the ''from'' clause
  * of the most nested subselect of `F`. This is the number of relations to the right since the most recent
  * `Subselect` or `Dual`/`From` if `F <: FromClause`, and the number of relations to the right of
  * the most recent `GroupBy` clause if `F <: GroupByClause`. This calculation happens solely
  * on the type level and requires that the `Generalized` form of the pertinent clause fragment is known
  * in order to properly recognize the cut off point.
  *
  * The difference from [[net.noresttherein.oldsql.sql.mechanics.SubselectSize SubselectSize]] is that
  * the former evidence requires that the generalized types of all joins (or pseudo joins) in the
  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] portion of `F` to be known (in case of grouped clauses,
  * at least since the last [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause, not necessarily the ungrouped
  * ''from'' clause preceding it) and, as the result, specifies the complete size of the clause as it would be returned
  * by [[net.noresttherein.oldsql.sql.RowProduct.size F#size]]. The full knowledge allows it to be also covariant in `F`.
  * On the other hand, this class witnesses the number of mappings listed in type `F`, exactly as stated.
  * Implicit values exist for any generalized clause type `F`, regardless of any of its prefix 'hidden' by a wildcard
  * `RowProduct` type, but requires in turn invariance in `F`.
  */
//todo: make type param N a member type
//todo: rename to KnownSubselectRelationCount/KnownInnerRelationCount
@implicitNotFound("Failed to count joined relations since last Subselect in ${F}.\n" +
                  "Most likely the type contains joins with unknown Generalized form or it starts with" +
                  "an undefined prefix other than RowProduct, GroupByClause, FromClause, FromSome, Dual, From. " +
                  "Missing implicit SubselectTableCount[${F}, ${N}]")
class SubselectRelationCount[F <: RowProduct, N <: Numeral] private[sql] (private val n :Int) extends AnyVal {
	@inline def knownSize :N = n.asInstanceOf[N]
}



private[mechanics] sealed abstract class Rank1SubselectRelationCountImplicits {
	implicit def first[L >: WildcardRow <: RowProduct, J[A >: L <: L, B[O] >: M[O] <: M[O]] <: A Adjoin B, M[O] <: MappingAt[O]]
			:SubselectRelationCount[L J M, 1] =
		new SubselectRelationCount[L J M, 1](1)
}

object SubselectRelationCount extends Rank1SubselectRelationCountImplicits {
	type In[F <: RowProduct] = SubselectRelationCount[F, _ <: Numeral]

	implicit def emptyHasZero[F <: EmptyRow] :SubselectRelationCount[F, 0] =
		new SubselectRelationCount[F, 0](0)

	implicit def wildcardHasZero[F >: WildcardRow <: RowProduct] :SubselectRelationCount[F, 0] =
		new SubselectRelationCount[F, 0](0)

	implicit def zero[F <: RowProduct](implicit count :RelationCount[F, 0]) :SubselectRelationCount[F, 0] =
		new SubselectRelationCount[F, 0](0)

	implicit def from[T[A] <: MappingAt[A]] :SubselectRelationCount[From[T], 1] =
		new SubselectRelationCount[From[T], 1](1)

	implicit def selectFrom[L <: RowProduct, T[A] <: MappingAt[A]] :SubselectRelationCount[L SelectFrom T, 1] =
		new SubselectRelationCount[L SelectFrom T, 1](1)

	implicit def subselect[L <: NonEmptyRow, T[A] <: MappingAt[A]] :SubselectRelationCount[L Subselect T, 1] =
		new SubselectRelationCount[L Subselect T, 1](1)

	implicit def groupBy[L <: FromSome, T[A] <: MappingAt[A]] :SubselectRelationCount[L GroupBy T, 1] =
		new SubselectRelationCount[L GroupBy T, 1](1)

	implicit def aggregated[F <: FromSome] :SubselectRelationCount[Aggregated[F], 0] =
		new SubselectRelationCount[Aggregated[F], 0](0)

	implicit def expanded[L <: RowProduct, J[+A <: L, B[O] <: R[O]] <: A NonSubselect B, R[O] <: MappingAt[O],
	                      M <: Numeral, N <: Numeral]
	                     (implicit prev :SubselectRelationCount[L, M], inc :Inc[M, N])
			:SubselectRelationCount[L J R, N] =
		new SubselectRelationCount[L J R, N](inc.n)

	implicit def decorated[C <: RowProduct, D[+B <: C] <: ExpandingDecorator[B], N <: Numeral]
	                      (implicit count :SubselectRelationCount[C, N])
			:SubselectRelationCount[D[C], N] =
		new SubselectRelationCount[D[C], N](count.knownSize)

	implicit def aliased[F <: NonEmptyRow, A <: Label, N <: Numeral]
	                    (implicit count :SubselectRelationCount[F, N]) :SubselectRelationCount[F As A, N] =
		new SubselectRelationCount[F As A, N](count.knownSize)
}





//todo: rename to FirstRelation, make M a member type, introduce RelationPosition
/** Implicit witness that `M` is the mapping for the first known relation in the clause `F`
  * and `N` is the number of relations to its right. Any relations under the scope of a `GroupBy`
  * (between the last `Subselect` or the first relation and `GroupBy`) are excluded from the count
  * and `M` must not be such a relation. The clause `F` must be in the form
  * `L E M G1 T1 ... Gn Tn`, where `L` is the upper bound of the left side of the expansion `E` and all `Gi`
  * are expansion with a definite `Generalized` form. `From[M]` can replace `L E M` in the above definition.
  * This evidence is invariant in `F` in order to maintain the invariant of `M` being the first `Mapping` type
  * listed and preserve the correct relation count `N`. When `F` is used as the `Origin` type for the mapping `M`,
  * this allows to track back any component of `M` back to the relation it originated from.
  */ //todo: this could be a refined RelationCount. Or forget it and make it work not only for the first relation.
@implicitNotFound("${M} is not the mapping of the first known relation in the FROM clause ${F}: "+
                  "no implicit RelationOffset[${F}, ${M}].")
class RelationOffset[F <: RowProduct, M[O] <: MappingAt[O]] private (private val n :Int) extends AnyVal { self =>
	/** An `Int` literal type specifying the index of the relation `M` in `F`
	  * (the number of relations following it in `F`).
	  */
	type N <: Numeral

	/** The specific subtype of [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] used by the first
	  * known join after a wildcard clause (or in [[net.noresttherein.oldsql.sql.From From]]`[M]`)
	  * which uses mapping `M`.
	  */
	type Rel[O <: RowProduct] <: JoinedRelation[O, M] { type FromLast = First }

	/** The [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type of the first known join in `F`
	  * (which introduces a relation with mapping `M`).
	  */
	type First <: RowProduct

	/** The index of the mapping `M` in clause `F`, that is the number of relations in `F` following `M`. */
	def index :N = n.asInstanceOf[N] //val tables :N crashes scalac


	/** Converts this evidence to one for a clause `E` extending `F` by adding the length of the expansion to
	  * its [[net.noresttherein.oldsql.sql.mechanics.RelationOffset.index index]].
	  */
	def +[E <: RowProduct](suffix :F PrefixOf E)
			:RelationOffset[E, M] { type First = self.First; type Rel[O <: RowProduct] = self.Rel[O] } =
		RelationOffset.unsafe(n + suffix.lengthDiff)

	/** Converts this evidence to one for a clause `E` extending `F` by adding the length of the expansion to
	  * its [[net.noresttherein.oldsql.sql.mechanics.RelationOffset.index index]].
	  */
	def asIn[E <: RowProduct](implicit suffix :F PrefixOf E)
			:RelationOffset[E, M] { type First = self.First; type Rel[O <: RowProduct] = self.Rel[O] } =
		RelationOffset.unsafe(n + suffix.lengthDiff)

}



object RelationOffset { //todo: API for explicit creation of instances in case of ambiguity
//	type In[F <: RowProduct] = RelationOffset[F, Mapping.AnyAt]

	@inline def apply[F <: RowProduct, M[O] <: MappingAt[O]](implicit offset :RelationOffset[F, M]) :offset.type =
		offset

	@inline private[sql] def counted[F <: RowProduct, L <: RowProduct,
	                                 R[O <: RowProduct] <: JoinedRelation[O, M] { type FromLast = L },
	                                 M[A] <: MappingAt[A], I <: Numeral]
	                                (n :I)
			:RelationOffset[F, M] { type N = I; type First = L; type Rel[O <: RowProduct] = R[O] } =
		new RelationOffset[F, M](n).asInstanceOf[
			RelationOffset[F, M] { type N = I; type First= L; type Rel[O <: RowProduct] = R[O] }
		]

	@inline private[sql] def unsafe[F <: RowProduct, L <: RowProduct,
	                                R[O <: RowProduct] <: JoinedRelation[O, M] { type FromLast = L },
	                                M[O] <: MappingAt[O]]
	                               (n :Int)
			:RelationOffset[F, M] { type First = L; type Rel[O <: RowProduct] = R[O] } =
		counted[F, L, R, M, n.type](n)

	@inline private[sql] def ofRelation[F <: RowProduct, L <: RowProduct, M[O] <: MappingAt[O]](n :Int)
			:RelationOffset[F, M] { type First = L; type Rel[O <: RowProduct] = JoinedRelation[O, M] { type FromLast = L } } =
		counted[F, L, ({ type T[O <: RowProduct] = JoinedRelation[O, M] { type FromLast = L } })#T, M, n.type](n)

	@inline private[sql] def ofTable[F <: RowProduct, M[O] <: MappingAt[O]](n :Int)
			:RelationOffset[F, M] { type First = RowProduct AndFrom M; type Rel[O <: RowProduct] = JoinedTable[O, M] } =
		counted[F, RowProduct AndFrom M, JoinedTable.Of[M]#T, M, n.type](n)

	@inline private[sql] def ofGrouping[D <: RowProduct, F <: RowProduct, M[O] <: MappingAt[O]](n :Int)
			:RelationOffset[F, M] { type First = RowProduct AndBy M; type Rel[O <: RowProduct] = JoinedGrouping[D, O, M] } =
		counted[F, RowProduct AndBy M, JoinedGrouping.Of[D, M]#T, M, n.type](n)


	//FromLast types for JoinedRelations + GetTable
	implicit def firstFrom[T[A] <: MappingAt[A]]
			:RelationOffset[From[T], T] {
				type N = 0; type First = RowProduct AndFrom T; type Rel[O <: RowProduct] = JoinedTable[O, T]
			} =
		counted[From[T], From.Last[T], JoinedTable.Of[T]#T, T, 0](0)

	implicit def firstAndFrom[L >: WildcardRow <: RowProduct, J[A <: L, B[O] <: R[O]] <: A AndFrom B, R[O] <: MappingAt[O]]
			:RelationOffset[L J R, R] {
				type N = 0; type First = RowProduct AndFrom R
				type Rel[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = First }
			} =
		RelationOffset.counted[
			L J R, From.Last[R],
			({ type Rel[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = From.Last[R] } })#Rel,
			R, 0
		](0)

	implicit def firstAndBy[L >: WildcardRow <: RowProduct, J[A <: L, B[O] <: R[O]] <: A AndBy B, R[O] <: MappingAt[O]]
			:RelationOffset[L J R, R] {
				type N = 0; type First = RowProduct AndBy R
				type Rel[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = First }
			} =
		RelationOffset.counted[
			L J R, GroupBy.Last[R],
			({ type Rel[O <: RowProduct] = JoinedRelation[O, R] { type FromLast = GroupBy.Last[R] } })#Rel,
			R, 0
		](0)



	implicit def expanded[L <: RowProduct, R[O] <: MappingAt[O], J[+A <: L, B[O] <: R[O]] <: A Expanded B,
	                      T[A] <: MappingAt[A], X <: Numeral, Y <: Numeral]
	                     (implicit prev :RelationOffset[L, T] { type N = X }, inc :Inc[X, Y])
			:RelationOffset[L J R, T] { type N = Y; type First = prev.First; type Rel[O <: RowProduct] = prev.Rel[O] } =
		counted[L J R, prev.First, prev.Rel, T, Y](inc.n)

	implicit def decorated[F <: RowProduct, D[+B <: F] <: ExpandingDecorator[B], T[O] <: MappingAt[O]]
	                      (implicit body :RelationOffset[F, T])
			:RelationOffset[D[F], T] { type N = body.N; type First = body.First; type Rel[O <: RowProduct] = body.Rel[O] } =
		counted[D[F], body.First, body.Rel, T, body.N](body.index)

	implicit def grouped[O <: RowProduct, F <: FromSome, T[A] <: MappingAt[A], X <: Numeral, Y <: Numeral]
	                    (implicit outer :O OuterClauseOf F, ungrouped :RelationOffset[O, T] { type N = X }, plus :Inc[X, Y])
			:RelationOffset[F GroupBy T, T] {
				type N = Y; type First = ungrouped.First; type Rel[E <: RowProduct] = ungrouped.Rel[E]
			} =
		counted[F GroupBy T, ungrouped.First, ungrouped.Rel, T, Y](plus.n)

	implicit def aggregated[O <: RowProduct, F <: FromSome, T[A] <: MappingAt[A]]
	                       (implicit outer :O OuterClauseOf F, ungrouped :RelationOffset[O, T])
			:RelationOffset[Aggregated[F], T] {
				type N = ungrouped.N; type First = ungrouped.First; type Rel[E <: RowProduct] = ungrouped.Rel[E]
			} =
		counted[Aggregated[F], ungrouped.First, ungrouped.Rel, T, ungrouped.N](ungrouped.index)

	implicit def aliased[F <: NonEmptyRow, A <: Label, T[O] <: MappingAt[O]]
	                    (implicit offset :RelationOffset[F, T])
			:RelationOffset[F As A, T] {
				type N = offset.N; type First = offset.First; type Rel[O <: RowProduct] = offset.Rel[O]
			} =
		counted[F As A, offset.First, offset.Rel, T, offset.N](offset.index)

}

