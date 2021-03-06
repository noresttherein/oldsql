package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.{Chain, Listing}
import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.{Aggregated, ColumnSQL, FromSome, IndexedMapping, RowProduct}
import net.noresttherein.oldsql.sql.RowProduct.{GroundFrom, NonEmptyFrom, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.{ChainSQL, ColumnComponentSQL, ComponentSQL, ConversionSQL, SelectAs, SelectColumn, SelectColumnAs, SelectSQL, TupleSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumn.{SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.ListingColumn






/** A factory of [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] subtype appropriate for the
  * ''select'' clause expression of type `E`. Serves as a type class marking
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtype (or some other type) as selectable
  * from the point of view the `select` method of `RowProduct`.
  * Implicit instances exist in the companion object for:
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]]`[G, M, T]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]]`[B, M, T]`
  *          for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.TopSelectColumnMapping TopSelectColumnMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.SubselectColumnMapping SubselectColumnMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom SubselectFrom]];
  *   1. `E <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs SelectColumnAs]]`[B, M, T]`
  *          for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.TopSelectColumnMapping TopSelectColumnMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumnAs.SubselectColumnMapping SubselectColumnMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom SubselectFrom]]
  *          (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[G, M]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]`[B, M]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectMapping TopSelectMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.SubselectMapping SubselectMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom SubselectFrom]];
  *   1. `E <: `[[net.noresttherein.oldsql.schema.Mapping Mapping]]:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs SelectAs]]`[B, M]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.TopSelectMapping TopSelectMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundFrom GroundFrom]] and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectAs.SubselectMapping SubselectMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom SubselectFrom]]
  *          (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`G, LocalScope, T]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn SelectColumn]]`[B, T]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn.TopSelectColumn TopSelectColumn]]`[T]` for `F <: GroundFrom` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectColumn.SubselectColumn SubselectColumn]]`[B, T]` for `F <: SubselectFrom`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.TupleSQL TupleSQL]]`[G, LocalScope, T]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, T]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[T]` for `F <: GroundFrom` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, T]` for `F <: SubselectFrom`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ast.ConversionSQL ConversionSQL]]`[G, LocalScope, X, Y]`:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, Y]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[Y]` for `F <: GroundFrom` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, Y]` for `F <: SubselectFrom`;
  *   1. `E =:= sql.*` pseudo expression:
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[B, F#Row]` for any `F <: RowProduct`,
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]]`[F#Row]` for `F <: GroundFrom` and
  *        - [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]]`[B, F#Row]` for `F <: SubselectFrom`;
  *   1.  Any `E` such that an implicit `select :SelectFactory[Aggregated[F], E]` exists: `select.Select`;
  *
  * where `G =:= F#`[[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]] and
  * `B =:= F#`[[net.noresttherein.oldsql.sql.RowProduct.Base Base]].
  * @tparam F the [[net.noresttherein.oldsql.sql.RowProduct.Self self]] type of the ''from'' clause selected from.
  * @tparam E type of the selected expression; generally either a `SQLExpression[F#Generalized, _]` subtype or
  *           a `BaseMapping[_, O]` for some type `O >: F#Generalized`.
  * @see [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select]]
  */
trait SelectFactory[F <: RowProduct, E] {
	type Select

	def apply(from :F, expr :E) :Select
}




sealed abstract class AggregateSelectFactories {
	implicit def selectAggregate[F <: G { type Generalized = G }, G <: FromSome, E]
	                            (implicit typer :F <:< RowProduct { type Generalized = G },
	                             select :SelectFactory[Aggregated[F], E])
			:SelectFactory[F, E] { type Select = select.Select } =
		new SelectFactory[F, E] {
			override type Select = select.Select

			override def apply(from :F, expr :E) = select(from.aggregate, expr)
		}
}




sealed abstract class DefaultSelectFactories extends AggregateSelectFactories {

	implicit def selectColumn
	             [F <: G { type Generalized <: G; type Base = B },
	              G <: RowProduct, B <: RowProduct, E <: ColumnSQL[G, LocalScope, X], X]
	             (implicit base :F <:< RowProduct { type Base = B }, typer :E <:< ColumnSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SelectColumn[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectColumn[B, X]
			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	implicit def selectMapping
	             [F <: G { type Generalized <: G; type Base = B },
		          G <: RowProduct, B <: RowProduct, E <: Mapping, X]
	             (implicit base :F <:< RowProduct { type Base = B }, mapping :E <:< BaseMapping[X, G],
	              offset :TableCount[G, _ <: Numeral], project :OriginProjection[E, X])
			:SelectFactory[F, E] { type Select = SelectAs[B, project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = SelectAs[B, project.WithOrigin]
			override def apply(from :F, expr :E) = expr.anchor(from).selectFrom(from)
		}

	implicit def selectComponent[F <: G { type Generalized <: G; type Base = B },
	                             G  <: RowProduct, B <: RowProduct, E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	                            (implicit base :F <:< RowProduct { type Base = B }, typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = SelectAs[B, M] } =
		new SelectFactory[F, E] {
			override type Select = SelectAs[B, M]
			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def selectConversion[F <: G { type Generalized <: G; type Base = B },
	                              G  <: RowProduct, B <: RowProduct, S >: LocalScope <: GlobalScope, X, Y]
	                             (implicit base :F <:< RowProduct { type Base = B })
			:SelectFactory[F, ConversionSQL[G, S, X, Y]] { type Select = SelectSQL[B, Y] } =
		new SelectFactory[F, ConversionSQL[G, S, X, Y]] {
			override type Select = SelectSQL[B, Y]
			override def apply(from :F, expr :ConversionSQL[G, S, X, Y]) = expr.selectFrom(from)
		}

	implicit def selectChain[F <: G { type Generalized <: G; type Base = B },
	                         G <: RowProduct, B <: RowProduct, E <: ChainSQL[G, LocalScope, I, L], I <: Chain, L]
	                        (implicit base :F <:< RowProduct { type Base = B },
	                         typer :E <:< ChainSQL[G, LocalScope, I, L])
			:SelectFactory[F, E] { type Select = SelectSQL[B, I ~ L] } =
		new SelectFactory[F, E] {
			override type Select = SelectSQL[B, I ~ L]
			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	implicit def selectTuple[F <: G { type Generalized <: G; type Base = B },
	                         G <: RowProduct, B <: RowProduct, E <: TupleSQL[G, LocalScope, X], X]
	                        (implicit base :F <:< RowProduct { type Base = B },
	                         typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SelectSQL[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectSQL[B, X]
			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	implicit def selectAll[F <: G { type Generalized = G; type Base = B; type Row = R },
	                       G <: RowProduct, B <: RowProduct, R <: Chain]
	                      (implicit typer :F <:< RowProduct { type Generalized = G; type Base = B; type Row = R })
            :SelectFactory[F, sql.*] { type Select = SelectSQL[B, R] } =
		new SelectFactory[F, sql.*] {
			override type Select = SelectSQL[B, R]
			override def apply(from :F, expr: sql.*) = from.row.selectFrom(from)
		}



	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def parameterizedSelectConversion[F <: TopFrom { type Params = P; type Generalized <: G },
	                                           G >: F <: RowProduct, P <: Chain, S >: LocalScope <: GlobalScope, X, T]
	                                          (implicit params :F <:< RowProduct { type Params = P })
			:SelectFactory[F, ConversionSQL[G, S, X, T]] { type Select = sql.Select[P, T] } =
		new SelectFactory[F, ConversionSQL[G, S, X, T]] {
			override type Select = sql.Select[P, T]
			override def apply(from :F, expr :ConversionSQL[G, S, X, T]) =
				expr.paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectChain[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                      G <: RowProduct, P <: Chain, E <: ChainSQL[G, LocalScope, I, L], I <: Chain, L]
	                                     (implicit params :F <:< RowProduct { type Params = P },
	                                               typer :E <:< ChainSQL[G, LocalScope, I, L])
			:SelectFactory[F, E] { type Select = sql.Select[P, I ~ L] } =
		new SelectFactory[F, E] {
			override type Select = sql.Select[P, I ~ L]
			override def apply(from :F, expr :E) = expr.paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectTuple[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                      G <: RowProduct, P <: Chain, E <: TupleSQL[G, LocalScope, T], T]
	                                     (implicit params :F <:< RowProduct { type Params = P },
	                                               typer :E <:< TupleSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = sql.Select[P, T] } =
		new SelectFactory[F, E] {
			override type Select = sql.Select[P, T]
			override def apply(from :F, expr :E) = expr.paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectAll[F <: G with TopFrom { type Params = P; type Generalized = G; type Row = R },
	                                    G <: RowProduct, P <: Chain, R <: Chain]
	                                   (implicit typer :F <:< TopFrom { type Params = P; type Generalized = G; type Row = R })
			:SelectFactory[F, sql.*] { type Select = sql.Select[P, R] } =
		new SelectFactory[F, sql.*] {
			override type Select = sql.Select[P, R]
			override def apply(from :F, expr: sql.*) = from.row.paramSelectFrom[P, G](from)
		}

}




sealed abstract class ForthChoiceSelectFactories extends DefaultSelectFactories {
	implicit def parameterizedSelectColumn
	             [F <: G with TopFrom { type Params = P; type Generalized <: G },
	              G <: RowProduct, P <: Chain, E <: ColumnSQL[G, LocalScope, X], X]
	             (implicit params :F <:< RowProduct { type Params = P }, typer :E <:< ColumnSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = sql.Select[P, X] } =
		new SelectFactory[F, E] {
			override type Select = sql.Select[P, X]
			override def apply(from :F, expr :E) = expr.paramSelectFrom[P, G](from)
		}
}




/** Default implicit [[net.noresttherein.oldsql.sql.mechanics.SelectFactory SelectFactory]] for individual supported
  * ''select'' clause column expression types and `ListingSQL`, based on ''from'' clauses with arbitrary
  * [[net.noresttherein.oldsql.sql.RowProduct.Base Base]] types.
  * This trait contains also a factory for parameterized [[net.noresttherein.oldsql.sql.Select selects]]
  * of single columns.
  */
sealed abstract class ThirdChoiceSelectFactories extends ForthChoiceSelectFactories {

	implicit def selectColumnMapping
	             [F <: G { type Generalized <: G; type Base = B },
		          G <: RowProduct, B <: RowProduct, E <: ColumnMapping[_, _], X]
	             (implicit base :F <:< RowProduct { type Base = B },
	              mapping :E <:< ColumnMapping[X, G], offset :TableCount[G, _ <: Numeral],
	              project :OriginProjection[E, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:SelectFactory[F, E] { type Select = SelectColumnAs[B, project.WithOrigin, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectColumnAs[B, project.WithOrigin, X]
			override def apply(from :F, expr :E) = expr.anchor(from).selectFrom(from)
		}

	implicit def selectColumnComponent[F <: G { type Generalized <: G; type Base = B },
	                                   G <: RowProduct, B <: RowProduct, M[A] <: ColumnMapping[V, A], V]
	                                  (implicit base :F <:< RowProduct { type Base = B })
			:SelectFactory[F, ColumnComponentSQL[G, M, V]] { type Select = SelectColumnAs[B, M, V] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, V]] {
			override type Select = SelectColumnAs[B, M, V]
			override def apply(from :F, expr :ColumnComponentSQL[G, M, V]) =
				expr.selectFrom(from)
		}

	implicit def selectIndexedColumn[F <: G { type Generalized <: G; type Base = B }, G <: RowProduct, B <: RowProduct,
	                                 S >: LocalScope <: GlobalScope, A <: Label, V]
	                                (implicit base :F <:< RowProduct { type Base = B })
			:SelectFactory[F, ListingColumn[G, S, A, V]]
				{ type Select = SelectColumnAs[B, IndexedMapping.Of[V]#Column, V] } =
		new SelectFactory[F, ListingColumn[G, S, A, V]] {
			override type Select = SelectColumnAs[B, IndexedMapping.Of[V]#Column, V]
			override def apply(from :F, expr :ListingColumn[G, S, A, V]) =
				expr.selectFrom(from)
		}

	implicit def selectIndexedTuple[F <: G { type Generalized <: G; type Base = B },
	                                G <: RowProduct, B <: RowProduct,
	                                E <: ListingSQL[G, LocalScope, X], X <: Listing]
	                               (implicit base :F <:< RowProduct { type Base = B },
	                                         typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SelectAs[B, IndexedMapping.Of[X]#Projection] } =
		new SelectFactory[F, E] {
			override type Select = SelectAs[B, IndexedMapping.Of[X]#Projection]
			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}


	implicit def parameterizedSelectMapping[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                        G <: RowProduct, P <: Chain, E <: Mapping, S]
	                                       (implicit params :F <:< RowProduct { type Params = P },
	                                                 origin :E <:< BaseMapping[S, G], offset :TableCount[G, _ <: Numeral],
	                                                 project :OriginProjection[E, S])
			:SelectFactory[F, E] { type Select = SelectMapping[P, project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = SelectMapping[P, project.WithOrigin]
			override def apply(from :F, expr :E) =
				(expr.anchor(from) :ComponentSQL[G, project.WithOrigin]).paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectComponent[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                          G <: RowProduct, P <: Chain, E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	                                         (implicit params :F <:< RowProduct { type Params = P },
	                                                   typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = SelectMapping[P, M] } =
		new SelectFactory[F, E] {
			override type Select = SelectMapping[P, M]
			override def apply(from :F, expr :E) = expr.paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectIndexedColumn[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                              G <: RowProduct, P <: Chain, S >: LocalScope <: GlobalScope, A <: Label, V]
	                                             (implicit params :F <:< RowProduct { type Params = P })
			:SelectFactory[F, ListingColumn[G, S, A, V]]
				{ type Select = SelectMapping[P, IndexedMapping.Of[V]#Column] } =
		new SelectFactory[F, ListingColumn[G, S, A, V]] {
			override type Select = SelectMapping[P, IndexedMapping.Of[V]#Column]
			override def apply(from :F, expr :ListingColumn[G, S, A, V]) =
				expr.paramSelectFrom[P, G](from)
		}

	implicit def parameterizedSelectIndexedTuple[F <: G with TopFrom { type Params = P; type Generalized <: G },
	                                             G <: RowProduct, P <: Chain,
	                                             E <: ListingSQL[G, LocalScope, T], T <: Listing]
	                                            (implicit params :F <:< RowProduct { type Params = P },
	                                                      typer :E <:< TupleSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = SelectMapping[P, IndexedMapping.Of[T]#Projection] } =
		new SelectFactory[F, E] {
			override type Select = SelectMapping[P, IndexedMapping.Of[T]#Projection]
			override def apply(from :F, expr :E) = expr.paramSelectFrom[P, G](from)
		}
}




/** Implicit [[net.noresttherein.oldsql.sql.mechanics.SelectFactory SelectFactory]] for both
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]] and
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL.SubselectSQL SubselectSQL]] subclasses and individual supported
  * non-column expression types.
  */
sealed abstract class SecondChoiceSelectFactories extends ThirdChoiceSelectFactories {

	implicit def topSelectColumn[F <: G with GroundFrom { type Generalized <: G }, G  <: RowProduct,
	                             E <: ColumnSQL[G, LocalScope, T], T]
	                            (implicit types :E <:< ColumnSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = TopSelectColumn[T] } =
		new SelectFactory[F, E] {
			type Select = TopSelectColumn[T]
			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}

	implicit def topSelectMapping[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                              E <: Mapping, T]
	                             (implicit origin :E <:< BaseMapping[T, G], offset :TableCount[G, _ <: Numeral],
	                              project :OriginProjection[E, T])
			:SelectFactory[F, E] { type Select = TopSelectAs[project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectAs[project.WithOrigin]
			override def apply(from :F, expr :E) = expr.topSelectFrom(from)
		}

	implicit def topSelectComponent[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                                E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	                               (implicit typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = TopSelectAs[M] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectAs[M]
			override def apply(from :F, expr :E) = expr.topSelectFrom(from)
		}

	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def topSelectConversion[F <: GroundFrom { type Generalized <: G }, G >: F <: RowProduct,
	                                 S >: LocalScope <: GlobalScope, X, T]
			:SelectFactory[F, ConversionSQL[G, S, X, T]] { type Select = TopSelectSQL[T] } =
		new SelectFactory[F, ConversionSQL[G, S, X, T]] {
			override type Select = TopSelectSQL[T]
			override def apply(from :F, expr :ConversionSQL[G, S, X, T]) = SelectSQL(from, expr)
		}

	implicit def topSelectChain[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                            E <: ChainSQL[G, LocalScope, I, L], I <: Chain, L]
	                           (implicit typer :E <:< ChainSQL[G, LocalScope, I, L])
			:SelectFactory[F, E] { type Select = TopSelectSQL[I ~ L] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectSQL[I ~ L]
			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}

	implicit def topSelectTuple[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                            E <: TupleSQL[G, LocalScope, T], T]
	                           (implicit typer :E <:< TupleSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = TopSelectSQL[T] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectSQL[T]
			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}

	implicit def topSelectAll[F <: G with GroundFrom { type Generalized = G; type Row = R }, G <: RowProduct, R <: Chain]
	                         (implicit typer :F <:< GroundFrom { type Generalized = G; type Row = R })
			:SelectFactory[F, sql.*] { type Select = TopSelectSQL[R] } =
		new SelectFactory[F, sql.*] {
			override type Select = TopSelectSQL[R]
			override def apply(from :F, expr: sql.*) = from.row.topSelectFrom(from)
		}



	implicit def subselectColumn
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, E <: ColumnSQL[G, LocalScope, X], X]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              typer :E <:< ColumnSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SubselectColumn[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectColumn[B, X]
			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectMapping
	             [F <: G { type Generalized <: G; type Implicit = B; type Base = B; type DefineBase[+I <: RowProduct] = I },
		          G <: RowProduct, B <: NonEmptyFrom, E <: Mapping, X]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              mapping :E <:< BaseMapping[X, G], offset :TableCount[G, _ <: Numeral],
	              project :OriginProjection[E, X])
			:SelectFactory[F, E] { type Select = SubselectAs[B, project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = SubselectAs[B, project.WithOrigin]
			override def apply(from :F, expr :E) =
				expr.anchor(from).subselectFrom(from)
		}

	implicit def subselectComponent
	             [F <: G { type Generalized <: G; type Implicit = B; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = SubselectAs[B, M] } =
		new SelectFactory[F, E] {
			override type Select = SubselectAs[B, M]
			override def apply(from :F, expr :E) = expr.subselectFrom(from)
		}

	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def subselectConversion
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, S >: LocalScope <: GlobalScope, B <: NonEmptyFrom, X, Y]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:SelectFactory[F, ConversionSQL[G, S, X, Y]] { type Select = SubselectSQL[B, Y] } =
		new SelectFactory[F, ConversionSQL[G, S, X, Y]] {
			override type Select = SubselectSQL[B, Y]
			override def apply(from :F, expr :ConversionSQL[G, S, X, Y]) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectChain
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+O <: RowProduct] = O },
	              G <: RowProduct, B <: NonEmptyFrom, E <: ChainSQL[G, LocalScope, I, L], I <: Chain, L]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+O <: RowProduct] = O },
	              typer :E <:< ChainSQL[G, LocalScope, I, L])
			:SelectFactory[F, E] { type Select = SubselectSQL[B, I ~ L] } =
		new SelectFactory[F, E] {
			override type Select = SubselectSQL[B, I ~ L]
			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectTuple
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, E <: TupleSQL[G, LocalScope, X], X]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SubselectSQL[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectSQL[B, X]
			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectAll
	             [F <: G { type Generalized = G; type Implicit = B; type Base = B; type DefineBase[+I <: RowProduct] = I; type Row = R },
	              G <: RowProduct, B <: NonEmptyFrom, R <: Chain]
	             (implicit typer :F <:< RowProduct { type Generalized = G; type Implicit = B; type Base = B;
	                                                 type DefineBase[+I <: RowProduct] = I; type Row = R })
            :SelectFactory[F, sql.*] { type Select = SubselectSQL[B, R] } =
		new SelectFactory[F, sql.*] {
			override type Select = SubselectSQL[B, R]
			override def apply(from :F, expr: sql.*) =
				(from.row :TupleSQL[F, LocalScope, from.Row]).subselectFrom(from)
		}
}




/** Implicit [[net.noresttherein.oldsql.sql.mechanics.SelectFactory SelectFactory]] values for
  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] subtypes specific to supported ''select'' clause
  * expression types.
  *
  * Methods defined directly here (and not inherited) provide values for all individual column expression types
  * and tuples.
  */
object SelectFactory extends SecondChoiceSelectFactories {
	//SelectColumnAs

	implicit def topSelectColumnMapping[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                                    E <: ColumnMapping[_, G], T]
	                                   (implicit origin :E <:< ColumnMapping[T, G],
	                                    offset :TableCount[G, _ <: Numeral],
	                                    project :OriginProjection[E, T] { type WithOrigin[A] <: ColumnMapping[T, A] })
			:SelectFactory[F, E] { type Select = TopSelectColumnAs[project.WithOrigin, T] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectColumnAs[project.WithOrigin, T]
			override def apply(from :F, expr :E) = expr.topSelectFrom(from)
		}

	implicit def topSelectColumnComponent[F <: GroundFrom { type Generalized <: G }, G >: F <: RowProduct,
	                                      M[A] <: ColumnMapping[T, A], T]
	                                     (implicit subject :M[G] <:< ColumnMapping[T, G])
			:SelectFactory[F, ColumnComponentSQL[G, M, T]] { type Select = TopSelectColumnAs[M, T] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, T]] {
			override type Select = TopSelectColumnAs[M, T]
			override def apply(from :F, expr :ColumnComponentSQL[G, M, T]) = SelectSQL(from, expr)
		}

	implicit def topSelectIndexedColumn[F <: GroundFrom { type Generalized <: G }, G >: F <: RowProduct,
	                                    S >: LocalScope <: GlobalScope, A <: Label, V]
			:SelectFactory[F, ListingColumn[G, S, A, V]]
				{ type Select = TopSelectColumnAs[IndexedMapping.Of[V]#Column, V] } =
		new SelectFactory[F, ListingColumn[G, S, A, V]] {
			override type Select = TopSelectColumnAs[IndexedMapping.Of[V]#Column, V]
			override def apply(from :F, expr :ListingColumn[G, S, A, V]) = SelectSQL(from, expr)
		}



	implicit def topSelectIndexedTuple[F <: G with GroundFrom { type Generalized <: G }, G <: RowProduct,
	                                   E <: ListingSQL[G, LocalScope, T], T <: Listing]
	                                  (implicit typer :E <:< TupleSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = TopSelectAs[IndexedMapping.Of[T]#Projection] } =
		new SelectFactory[F, E] {
			override type Select = TopSelectAs[IndexedMapping.Of[T]#Projection]
			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}



	implicit def subselectColumnMapping
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
		          G <: RowProduct, B <: NonEmptyFrom, E <: ColumnMapping[_, _], X]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              mapping :E <:< ColumnMapping[X, G], offset :TableCount[G, _ <: Numeral],
	              project :OriginProjection[E, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:SelectFactory[F, E] { type Select = SubselectColumnAs[B, project.WithOrigin, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectColumnAs[B, project.WithOrigin, X]
			override def apply(from :F, expr :E) =
				SelectSQL.subselect(from, expr.anchor(from))
		}

	implicit def subselectColumnComponent
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, M[A] <: ColumnMapping[V, A], V]
	             (implicit typer :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:SelectFactory[F, ColumnComponentSQL[G, M, V]] { type Select = SubselectColumnAs[B, M, V] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, V]] {
			override type Select = SubselectColumnAs[B, M, V]
			override def apply(from :F, expr :ColumnComponentSQL[G, M, V]) =
				SelectSQL.subselect(from, expr)
		}

	implicit def subselectIndexedColumn
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, S >: LocalScope <: GlobalScope, A <: Label, V]
	             (implicit typer :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I })
			:SelectFactory[F, ListingColumn[G, S, A, V]]
				{ type Select = SubselectColumnAs[B, IndexedMapping.Of[V]#Column, V] } =
		new SelectFactory[F, ListingColumn[G, S, A, V]] {
			override type Select = SubselectColumnAs[B, IndexedMapping.Of[V]#Column, V]
			override def apply(from :F, expr :ListingColumn[G, S, A, V]) =
				SelectSQL.subselect(from, expr)
		}


	implicit def subselectIndexedTuple
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: RowProduct] = I },
	              G <: RowProduct, B <: NonEmptyFrom, E <: ListingSQL[G, LocalScope, X], X <: Listing]
	             (implicit base :F <:< RowProduct { type Base = B; type DefineBase[+I <: RowProduct] = I },
	              typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SubselectAs[B, IndexedMapping.Of[X]#Projection] } =
		new SelectFactory[F, E] {
			override type Select = SubselectAs[B, IndexedMapping.Of[X]#Projection]
			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}



	def apply[F <: RowProduct, E](implicit factory :SelectFactory[F, E]) :factory.type = factory

}
