package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.{Aggregated, FromSome, RowProduct, Select}
import net.noresttherein.oldsql.sql.RowProduct.{GroundRow, NonEmptyRow, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.ast.{SelectAs, SelectColumn, SelectColumnAs, SelectSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumn.SubselectColumn
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.mechanics.CanSelect.{CanSelectDef, CanSelectDirect}






/** A factory of [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] subtype appropriate for the
  * ''select'' clause expression of type `E`. Serves as a type class marking
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtype (or some other type) as selectable
  * from the point of view the `select` method of `RowProduct`.
  * Implicit instances exist in the companion object for:
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]]`[G, M, T]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn TypedColumn]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]]`[B, M, T]`
  *          for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.BaseTopSelectColumnAs TopSelectColumnMapping]]`[M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.BaseSubselectColumnAs SubselectColumnMapping]]`[B, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]];
  *   1. `E <: `[[net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn TypedColumn]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]]`[B, M, T]`
  *          for any `F <: RowProduct`, `O >: F#Generalized <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.BaseTopSelectColumnAs TopSelectColumnMapping]]`[M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.BaseSubselectColumnAs SubselectColumnMapping]]`[B, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]]
  *          (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]]`[O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]`[B, M]` for any `F <: RowProduct`,
  *          `O >: F#Generalized <: RowProduct`
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectAs TopSelectAs]]`[M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.SubselectAs SubselectAs]]`[B, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]]
  *          (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[G, M]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]`[B, M]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectAs TopSelectAs]]`[M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.SubselectAs SubselectAs]]`[B, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]];
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.EditedLValueSQL EditedLValueSQL]]`[G, M, T]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, T]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]];
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[G, Grouped, T]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]]`[B, T]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn.TopSelectColumn TopSelectColumn]]`[T]` for `F <: GroundRow` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn.SubselectColumn SubselectColumn]]`[B, T]` for `F <: SubselectRow`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.InlineSQL InlineSQL]]`[G, Grouped, T]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, T]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[T]` for `F <: GroundRow` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, T]` for `F <: SubselectRow`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.AdaptedSQL AdaptedSQL]]`[G, Grouped, X, Y]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, Y]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[Y]` for `F <: GroundRow` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, Y]` for `F <: SubselectRow`;
  *   1. `E =:= sql.*` pseudo expression:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, F#Row]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[F#Row]` for `F <: GroundRow` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, F#Row]` for `F <: SubselectRow`;
  *   1.  Any `E` such that an implicit `select :CanSelect[Aggregated[F], E]` exists: `select.Select`;
  *
  * where `G =:= F#`[[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] and
  * `B =:= F#`[[net.noresttherein.oldsql.sql.RowProduct.Base Base]].
  * @tparam F the [[net.noresttherein.oldsql.sql.RowProduct.Self self]] type of the ''from'' clause selected from.
  * @tparam E type of the selected expression; generally either a `SQLExpression[F#Generalized, _]` subtype or
  *           a `BaseMapping[_, O]` for some type `O >: F#Generalized`.
  * @see [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select]]
  */ //consider: stop using subselect
@implicitNotFound("Cannot select expression ${E} from clause ${F}.")
trait CanSelect[-F <: RowProduct, -E] {
	type Select
	def apply(from :F, expr :E) :Select
}



//No need to duplicate SQLExpression.selectAggregate for Mapping and * because Aggregated[F]#Row is empty and no ComponentSQL can exist
private[mechanics] sealed abstract class Rank3CanSelectImplicits {
		implicit def selectMapping[F <: RowProduct, B <: RowProduct, E <: Mapping, V, O <: RowProduct]
	                              (implicit mappingType :OriginProjection[E, V], originType :E <:< MappingAt[O],
	                                        offset :RelationCount[O, _ <: Numeral],
	                                        fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectAs[B, mappingType.WithOrigin]] =
		new CanSelectDef[F, E, SelectAs[B, mappingType.WithOrigin]] {
			override def apply(from :F, expr :E) = expr selectFrom from
		}

	implicit def paramSelectMapping[F <: TopRow { type Complete = C; type Params = P },
	                                C <: TopRow { type Complete = C; type Params = P },
	                                P, E <: Mapping, V, O >: C <: RowProduct]
	                               (implicit originType :E <:< MappingAt[O], mappingType :OriginProjection[E, V],
	                                         offset :RelationCount[O, _ <: Numeral],
	                                         fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
			:CanSelectDirect[F, E, SelectMapping[P, mappingType.WithOrigin]] =
		new CanSelectDef[F, E, SelectMapping[P, mappingType.WithOrigin]] {
			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
		}
}



private[mechanics] sealed abstract class Rank2CanSelectImplicits extends Rank3CanSelectImplicits {
//	type SelectClauseTemplate[-F <: RowProduct, V, S[-_ <: RowProduct]] = ExpressionSelectMethods[F, V] {
//		type isSelectable = true
//		def selectFrom(from :F) :S[from.Base]
//	}
//	implicit def select[F <: RowProduct, B <: RowProduct,
//	                    E <: SelectClauseTemplate[O, V, S], V, O <: RowProduct, S[-_ <: RowProduct]]
//	                   (implicit selectClauseType :E <:< SQLExpression[O, Grouped, V] {
//                                    type isSelectable = true; def selectFrom(from :O) :S[from.Base]
//	                             }, outerClause :F <:< O { type Base = B }
//	                   ) :CanSelectDirect[F, E, S[B]] =
//		new CanSelectDirect[F, E, S[B]] {
//			override def apply(from :F, expr :E) = expr selectFrom from
//		}

	implicit def selectColumn[F <: RowProduct, B <: RowProduct, E <: TypedColumn[V, O], V, O <: RowProduct]
	                         (implicit mappingType :OriginProjection[E, V] { type WithOrigin[A] <: BaseColumn[V, A] },
	                                   originType :E <:< TypedColumn[V, O], offset :RelationCount[O, _ <: Numeral],
	                                   fromClauseType :F <:< O { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:CanSelectDirect[F, E, SelectColumnAs[B, mappingType.WithOrigin, V]] =
		new CanSelectDef[F, E, SelectColumnAs[B, mappingType.WithOrigin, V]] {
			override def apply(from :F, expr :E) = expr selectFrom from
		}

	implicit def selectAll[F <: RowProduct { type Row = R; type Base = B }, B <: RowProduct, R]
	                      (implicit fromClauseType :
	                                    F <:< RowProduct {
	                                        type Row = R; type Base = B; type DefineBase[+I <: RowProduct] = I
	                                    }
	                      )
			:CanSelectDirect[F, sql.*, SelectSQL[B, R]] =
		new CanSelectDef[F, sql.*, SelectSQL[B, R]] {
			override def apply(from :F, expr :sql.*) = from.row selectFrom from.self
		}
//
//	implicit def paramSelectColumn[F <: TopRow { type Complete = C; type Params = P },
//	                               C <: TopRow { type Complete = C; type Params = P },
//	                               P, E, V, O >: C <: RowProduct]
//	                              (implicit mappingType :OriginProjection[E, V] { type WithOrigin[A] <: TypedColumn[V, A] },
//	                                        originType :E <:< TypedColumn[O, V], offset :RelationCount[O, _ <: Numeral],
//	                                        fromClauseType :F <:< TopRow { type Complete = C; type Params = P })
//			:CanSelectDirect[F, E, SelectMapping[P, mappingType.WithOrigin]] =
//		new CanSelectDef[F, E, SelectMapping[P, mappingType.WithOrigin]] {
//			override def apply(from :F, expr :E) = expr paramSelectFrom from.complete
//		}
	implicit def paramSelectAll[F <: TopRow { type Complete = C; type Row = R; type Params = P },
	                            C <: TopRow { type Complete = C; type Row = R; type Params = P }, P, R, O >: C <: RowProduct]
	                           (implicit fromClauseType :F <:< TopRow { type Row = R; type Complete = C; type Params = P })
			:CanSelectDirect[F, sql.*, Select[P, R]] =
		new CanSelectDef[F, sql.*, Select[P, R]] {
			override def apply(from :F, expr :sql.*) = from.row.paramSelectFrom[P, C](from.complete)
		}
}



private[mechanics] sealed abstract class Rank1CanSelectImplicits extends Rank2CanSelectImplicits {

	implicit def topSelectMapping[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                              E <: Mapping, V, O >: C <: RowProduct]
	                             (implicit mappingType :OriginProjection[E, V], originType :E <:< MappingAt[O],
	                                       offset :RelationCount[O, _ <: Numeral],
	                                       completeType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectAs[mappingType.WithOrigin]] =
		new CanSelectDef[F, E, TopSelectAs[mappingType.WithOrigin]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def subselectMapping[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                              B <: NonEmptyRow, E <: Mapping, V, O <: RowProduct]
	                             (implicit mappingType :OriginProjection[E, V], originType :E <:< MappingAt[O],
	                                       offset :RelationCount[O, _ <: Numeral],
	                                       fromClauseType :F <:< O {
	                                           type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                       })
			:CanSelectDirect[F, E, SubselectAs[B, mappingType.WithOrigin]] =
		new CanSelectDef[F, E, SubselectAs[B, mappingType.WithOrigin]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}
}



/** Implicit [[net.noresttherein.oldsql.sql.mechanics.CanSelect.CanSelectDirect CanSelectDirect]] values for
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] subtypes specific to supported ''select'' clause
  * expression types.
  *
  * Methods defined directly here (and not inherited) provide values for all individual column expression types
  * and tuples.
  */
object CanSelect extends Rank1CanSelectImplicits {

	implicit def topSelectColumn[F <: GroundRow { type Complete = C }, C <: GroundRow { type Complete = C },
	                             E <: TypedColumn[V, O], V, O >: C <: RowProduct]
	                            (implicit mappingType :OriginProjection[E, V] { type WithOrigin[A] <: BaseColumn[V, A] },
	                                      originType :E <:< TypedColumn[V, O], offset :RelationCount[O, _ <: Numeral],
	                                      completeType :F <:< GroundRow { type Complete = C })
			:CanSelectDirect[F, E, TopSelectColumnAs[mappingType.WithOrigin, V]] =
		new CanSelectDef[F, E, TopSelectColumnAs[mappingType.WithOrigin, V]] {
			override def apply(from :F, expr :E) = expr topSelectFrom from.complete
		}

	implicit def topSelectAll[F <: GroundRow { type Row = R; type Complete = C }, C <: GroundRow { type Complete = C }, R]
	                         (implicit completeType :F <:< GroundRow { type Complete = C; type Row = R })
			:CanSelectDirect[F, sql.*, TopSelectSQL[R]] =
		new CanSelectDef[F, sql.*, TopSelectSQL[R]] {
			override def apply(from :F, expr :sql.*) = from.row topSelectFrom from.complete
		}



//	type SubselectClauseTemplate[F <: RowProduct, V, S[-_ <: NonEmptyRow]] = ExpressionSelectMethods[F, V] {
//		type isSelectable = true
//		def subselect_:[B <: NonEmptyRow](from :F ProperSubselectOf B) :S[B]
//	}
//
//	implicit def subselect[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
//	                       B <: NonEmptyRow, E, V, O <: RowProduct, S[_ <: NonEmptyRow]]
//	                      (implicit selectClauseType :
//		                                E <:< (ExpressionSelectMethods[O, V] {
//		                                    type isSelectable = true
//			                                def subselectFrom[A <: NonEmptyRow](from :O ProperSubselectOf A) :S[A]
//		                                }),
//	                                fromClauseType :F <:< O {
//		                                type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
//	                                }
//	                      )
//			:CanSelectDirect[F, E] { type Select = S[B] } =
//		new CanSelectDirect[F, E] {
//			override type Select = S[B]
//			override def apply(from :F, expr :E) = expr subselectFrom from
//		}

	implicit def subselectColumn[F <: RowProduct { type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                             B <: NonEmptyRow, E <: TypedColumn[V, O], V, O <: RowProduct]
	                            (implicit mappingType :OriginProjection[E, V] { type WithOrigin[A] <: BaseColumn[V, A] },
	                                      originType :E <:< TypedColumn[V, O], offset :RelationCount[O, _ <: Numeral],
	                                      fromClauseType :F <:< O {
	                                          type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I
	                                      }
	                            )
			:CanSelectDirect[F, E, SubselectColumnAs[B, mappingType.WithOrigin, V]] =
		new CanSelectDef[F, E, SubselectColumnAs[B, mappingType.WithOrigin, V]] {
			override def apply(from :F, expr :E) = expr subselectFrom from
		}

	implicit def subselectAll[F <: RowProduct { type Row = R; type Base = B; type Implicit = B; type DefineBase[+I <: RowProduct] = I },
	                          B <: NonEmptyRow, R]
	                         (implicit fromClauseType :F <:< RowProduct { type Row = R; type Base = B; type Implicit = B })
			:CanSelectDirect[F, sql.*, SubselectSQL[B, R]] =
		new CanSelectDef[F, sql.*, SubselectSQL[B, R]] {
			override def apply(from :F, expr :sql.*) :SubselectSQL[B, R] = from.row subselectFrom from.self
		}


	@inline def apply[F <: RowProduct, E](implicit factory :CanSelect[F, E]) :factory.type = factory



	//todo: try to get rid of it in Scala 3
	/** A declaration-level `CanSelect` subtype. Implicit values of this type include only expression types
	  * (and mappings with `Origin` type) based directly on the clause `F`. This specifically excludes
	  * selecting of an aggregated expression based on [[net.noresttherein.oldsql.sql.RowProduct.AggregateOf AggregateOf]]`[F]`
	  * from a non aggregate clause `F`, which is handled by a separate implicit value
	  * depending on `CanSelect[Aggregated[F], E]`. It additionally provides a covariant upper bound on
	  * return type [[net.noresttherein.oldsql.sql.mechanics.CanSelect.Select Select]] for conciseness.
	  */
	trait CanSelectDirect[-F <: RowProduct, -E, +S] extends CanSelect[F, E] {
		override type Select <: S
	}
	private[sql] trait CanSelectDef[F <: RowProduct, E, S] extends CanSelectDirect[F, E, S] {
		override type Select = S
	}


	type AnyType[-A] = Any
	type NoType[-A] = Nothing
	type default[V] = { //todo: rename to something better
		type from[-F <: RowProduct]   = SelectSQL[F, V]
		type nested[-F <: RowProduct] = SubselectSQL[F, V]
		type params[X] = Select[X, V]
	}
	type as[M[A] <: MappingAt[A]] = {
		type from[-F <: RowProduct]   = SelectAs[F, M]
		type nested[-F <: RowProduct] = SubselectAs[F, M]
		type params[X] = SelectMapping[X, M]
	}
	type column[V] = {
		type from[-F <: RowProduct] = SelectColumn[F, V]
		type nested[-F <: RowProduct] = SubselectColumn[F, V]
		type as[M[A] <: BaseColumn[V, A]] = {
			type from[-F <: RowProduct] = SelectColumnAs[F, M, V]
			type nested[-F <: RowProduct] = SubselectColumnAs[F, M, V]
		}
	}
}
