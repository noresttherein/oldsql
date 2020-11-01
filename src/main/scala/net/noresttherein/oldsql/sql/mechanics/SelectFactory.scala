package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, OriginProjection}
import net.noresttherein.oldsql.sql.{*, Aggregated, ColumnSQL, ConversionSQL, FromClause, SelectSQL, TupleSQL}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{FreeFrom, NonEmptyFrom, TableCount}
import net.noresttherein.oldsql.sql.MappingSQL.{ColumnComponentSQL, ComponentSQL}
import net.noresttherein.oldsql.sql.SelectSQL.{FreeSelectAs, FreeSelectColumn, FreeSelectColumnAs, FreeSelectSQL, SelectAs, SelectColumn, SelectColumnAs, SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumn, SubselectColumnAs, SubselectColumnMapping, SubselectMapping, SubselectSQL}
import net.noresttherein.oldsql.sql.SQLExpression.LocalScope






/** A factory of [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]] subtype appropriate for the
  * ''select'' clause expression of type `E`. Serves as a type class marking
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] subtype (or some other type) as selectable
  * from the point of view the `select` method of `FromClause`.
  * Implicit instances exist in the companion object for:
  *   1. `E <: `[[net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL ColumnComponentSQL]]`[G, M, T]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumnAs SelectColumnAs]]`[B, M, T]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumnMapping SelectColumnMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]] and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectColumnMapping SubselectColumnMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]];
  *   1. `E <: `[[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumnAs SelectColumnAs]]`[B, M, T]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumnMapping SelectColumnMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]] and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectColumnMapping SubselectColumnMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
  *      (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]`[G, M]`,
  *      `M[O] <: `[[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[T, O]`:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectAs SelectAs]]`[B, M]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectMapping SelectMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]] and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectMapping SubselectMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]];
  *   1. `E <: `[[net.noresttherein.oldsql.schema.Mapping Mapping]]:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectAs SelectAs]]`[B, M]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectMapping SelectMapping]]`[S, M, T]` for
  *          `F <: `[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]] and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectMapping SubselectMapping]]`[B, S, M, T]`
  *          for `F <: `[[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
  *        (where `M[O]` is the origin [[net.noresttherein.oldsql.schema.Mapping.OriginProjection projection]] of `E`);
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`G, LocalScope, T]`:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SelectColumn SelectColumn]]`[B, T]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.FreeSelectColumn FreeSelectColumn]]`[T]` for `F <: FreeFrom` and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectColumn SubselectColumn]]`[B, T]` for `F <: SubselectFrom`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.TupleSQL TupleSQL]]`[G, LocalScope, T]`:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]]`[B, T]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.FreeSelectSQL FreeSeelctSQL]]`[T]` for `F <: FreeFrom` and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]]`[B, T]` for `F <: SubselectFrom`;
  *   1. `E <: `[[net.noresttherein.oldsql.sql.ConversionSQL ConversionSQL]]`[G, LocalScope, X, Y]`:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]]`[B, Y]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.FreeSelectSQL FreeSeelctSQL]]`[Y]` for `F <: FreeFrom` and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]]`[B, Y]` for `F <: SubselectFrom`;
  *   1. `E =:= sql.*` pseudo expression:
  *        - [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]]`[B, F#InnerRow]` for any `F <: FromClause`,
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.FreeSelectSQL FreeSelectSQL]]`[F#InnerRow]` for `F <: FreeFrom` and
  *        - [[net.noresttherein.oldsql.sql.SelectSQL.SubselectSQL SubselectSQL]]`[B, F#InnerRow]` for `F <: SubselectFrom`;
  *   1.  Any `E` such that an implicit `select :SelectFactory[Aggregated[F], E]` exists: `select.Select`;
  *
  * where `G =:= F#`[[net.noresttherein.oldsql.sql.FromClause.Generalized Generalized]] and
  * `B =:= F#`[[net.noresttherein.oldsql.sql.FromClause.Base Base]].
  * @tparam F the [[net.noresttherein.oldsql.sql.FromClause.Self self]] type of the ''from'' clause selected from.
  * @tparam E type of the selected expression; generally either a `SQLExpression[F#Generalized, _]` subtype or
  *           a `BaseMapping[_, O]` for some type `O >: F#Generalized`.
  * @see [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension.select]]
  */
trait SelectFactory[F <: FromClause, E] {
	type Select <: SelectSQL[_ <: FromClause, _]

	def apply(from :F, expr :E) :Select
}




sealed abstract class AggregateSelectFactories {

	implicit def selectAggregate[F <: G { type Generalized = G }, G <: FromSome, E]
	                            (implicit typer :F <:< FromClause { type Generalized = G },
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
	              G <: FromClause, B <: FromClause, E <: ColumnSQL[G, LocalScope, X], X]
	             (implicit base :F <:< FromClause { type Base = B }, typer :E <:< ColumnSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SelectColumn[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectColumn[B, X]

			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	implicit def selectMapping
	             [F <: G { type Generalized <: G; type Base = B },
		          G <: FromClause, B <: FromClause, E <: Mapping, X]
	             (implicit base :F <:< FromClause { type Base = B }, mapping :E <:< BaseMapping[X, G],
	                       offset :TableCount[G, _ <: Numeral], project :OriginProjection[E, X])
			:SelectFactory[F, E] { type Select = SelectAs[B, project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = SelectAs[B, project.WithOrigin]

			override def apply(from :F, expr :E) = expr.anchor(from).selectFrom(from)
		}

	implicit def selectComponent[F <: G { type Generalized <: G; type Base = B },
	                             G  <: FromClause, B <: FromClause, E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	                            (implicit base :F <:< FromClause { type Base = B }, typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = SelectAs[B, M] } =
		new SelectFactory[F, E] {
			override type Select = SelectAs[B, M]

			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def selectConversion[F <: G { type Generalized <: G; type Base = B },
	                              G  <: FromClause, B <: FromClause, X, Y]
	                             (implicit base :F <:< FromClause { type Base = B })
			:SelectFactory[F, ConversionSQL[G, LocalScope, X, Y]] { type Select = SelectSQL[B, Y] } =
		new SelectFactory[F, ConversionSQL[G, LocalScope, X, Y]] {
			override type Select = SelectSQL[B, Y]

			override def apply(from :F, expr :ConversionSQL[G, LocalScope, X, Y]) = expr.selectFrom(from)
		}

	implicit def selectTuple[F <: G { type Generalized <: G; type Base = B },
	                         G <: FromClause, B <: FromClause, E <: TupleSQL[G, LocalScope, X], X]
	                        (implicit base :F <:< FromClause { type Base = B },
	                                  typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SelectSQL[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectSQL[B, X]

			override def apply(from :F, expr :E) = expr.selectFrom(from)
		}

	implicit def selectAll[F <: G { type Generalized = G; type Base = B; type InnerRow = R },
	                       G <: FromClause, B <: FromClause, R <: Chain]
	                      (implicit typer :F <:< FromClause { type Generalized = G; type Base = B; type InnerRow = R })
            :SelectFactory[F, *] { type Select = SelectSQL[B, R] } =
		new SelectFactory[F, *] {
			override type Select = SelectSQL[B, R]

			override def apply(from :F, expr: *) = from.innerRow.selectFrom(from)
		}

}




sealed abstract class SingleColumnSelectFactories extends DefaultSelectFactories {

	implicit def selectColumnMapping
	             [F <: G { type Generalized <: G; type Base = B },
		          G <: FromClause, B <: FromClause, E <: ColumnMapping[_, _], X]
	             (implicit base :F <:< FromClause { type Base = B },
	                       mapping :E <:< ColumnMapping[X, G], offset :TableCount[G, _ <: Numeral],
	                       project :OriginProjection[E, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:SelectFactory[F, E] { type Select = SelectColumnAs[B, project.WithOrigin, X] } =
		new SelectFactory[F, E] {
			override type Select = SelectColumnAs[B, project.WithOrigin, X]

			override def apply(from :F, expr :E) = expr.anchor(from).selectFrom(from)
		}

	implicit def selectColumnComponent[F <: G { type Generalized <: G; type Base = B },
	                                   G <: FromClause, B <: FromClause, M[A] <: ColumnMapping[V, A], V]
	                                  (implicit base :F <:< FromClause { type Base = B })
			:SelectFactory[F, ColumnComponentSQL[G, M, V]] { type Select = SelectColumnAs[B, M, V] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, V]] {
			override type Select = SelectColumnAs[B, M, V]

			override def apply(from :F, expr :ColumnComponentSQL[G, M, V]) = expr.selectFrom(from)
		}

}




sealed abstract class SecondChoiceSelectFactories extends SingleColumnSelectFactories {

	implicit def freeSelectColumn[F <: G with FreeFrom { type Generalized <: G }, G  <: FromClause,
	                              E <: ColumnSQL[G, LocalScope, T], T]
	                             (implicit types :E <:< ColumnSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = FreeSelectColumn[T] } =
		new SelectFactory[F, E] {
			type Select = FreeSelectColumn[T]

			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}

	implicit def freeSelectMapping[F <: G with FreeFrom { type Generalized <: G }, G <: FromClause,
	                               E <: Mapping, T]
	                              (implicit origin :E <:< BaseMapping[T, G], offset :TableCount[G, _ <: Numeral],
	                               project :OriginProjection[E, T])
			:SelectFactory[F, E] { type Select = FreeSelectAs[project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = FreeSelectAs[project.WithOrigin]

			override def apply(from :F, expr :E) = expr.freeSelectFrom(from)
		}

	implicit def freeSelectComponent[F <: G with FreeFrom { type Generalized <: G }, G <: FromClause,
	                                 E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	                                (implicit typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = FreeSelectAs[M] } =
		new SelectFactory[F, E] { //consider: FreeSelectAs result type: not needed BaseMapping supertype
			override type Select = FreeSelectAs[M]

			override def apply(from :F, expr :E) = expr.freeSelectFrom(from)
		}

	//no ambiguity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def freeSelectConversion[F <: FreeFrom { type Generalized <: G }, G >: F <: FromClause, X, T]
			:SelectFactory[F, ConversionSQL[G, LocalScope, X, T]] { type Select = FreeSelectSQL[T] } =
		new SelectFactory[F, ConversionSQL[G, LocalScope, X, T]] {
			override type Select = FreeSelectSQL[T]

			override def apply(from :F, expr :ConversionSQL[G, LocalScope, X, T]) = SelectSQL(from, expr)
		}

	implicit def freeSelectTuple[F <: G with FreeFrom { type Generalized <: G }, G <: FromClause,
	                             E <: TupleSQL[G, LocalScope, T], T]
	                            (implicit typer :E <:< TupleSQL[G, LocalScope, T])
			:SelectFactory[F, E] { type Select = FreeSelectSQL[T] } =
		new SelectFactory[F, E] {
			override type Select = FreeSelectSQL[T]

			override def apply(from :F, expr :E) = SelectSQL(from, expr)
		}

	implicit def freeSelectAll[F <: G with FreeFrom { type Generalized = G; type InnerRow = R }, G <: FromClause, R <: Chain]
	                          (implicit typer :F <:< FreeFrom { type Generalized = G; type InnerRow = R })
			:SelectFactory[F, *] { type Select = FreeSelectSQL[R] } =
		new SelectFactory[F, *] {
			override type Select = FreeSelectSQL[R]

			override def apply(from :F, expr: *) = from.innerRow.freeSelectFrom(from)
		}



	implicit def subselectColumn
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: FromClause] = I },
	              G <: FromClause, B <: NonEmptyFrom, E <: ColumnSQL[G, LocalScope, X], X]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I },
	                       typer :E <:< ColumnSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SubselectColumn[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectColumn[B, X]

			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectMapping
	             [F <: G { type Generalized <: G; type Implicit = B; type Base = B; type DefineBase[+I <: FromClause] = I },
		          G <: FromClause, B <: NonEmptyFrom, E <: Mapping, X]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I },
	                       mapping :E <:< BaseMapping[X, G], offset :TableCount[G, _ <: Numeral],
	                       project :OriginProjection[E, X])
			:SelectFactory[F, E] { type Select = SubselectAs[B, project.WithOrigin] } =
		new SelectFactory[F, E] {
			override type Select = SubselectAs[B, project.WithOrigin]

			override def apply(from :F, expr :E) =
				expr.anchor(from).subselectFrom(from)
		}

	implicit def subselectComponent
	             [F <: G { type Generalized <: G; type Implicit = B; type Base = B; type DefineBase[+I <: FromClause] = I },
	              G <: FromClause, B <: NonEmptyFrom, E <: ComponentSQL[G, M], M[A] <: MappingAt[A]]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I },
	                       typer :E <:< ComponentSQL[G, M])
			:SelectFactory[F, E] { type Select = SubselectAs[B, M] } =
		new SelectFactory[F, E] { //consider: SubselectAs result type: not needed BaseMapping supertype
			override type Select = SubselectAs[B, M]

			override def apply(from :F, expr :E) = expr.subselectFrom(from)
		}

	//no ambigouity with column conversion as SelectFactory is not contravariant in the expression type
	implicit def subselectConversion
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: FromClause] = I },
	              G <: FromClause, B <: NonEmptyFrom, X, Y]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I })
			:SelectFactory[F, ConversionSQL[G, LocalScope, X, Y]] { type Select = SubselectSQL[B, Y] } =
		new SelectFactory[F, ConversionSQL[G, LocalScope, X, Y]] {
			override type Select = SubselectSQL[B, Y]

			override def apply(from :F, expr :ConversionSQL[G, LocalScope, X, Y]) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectTuple
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: FromClause] = I },
	              G <: FromClause, B <: NonEmptyFrom, E <: TupleSQL[G, LocalScope, X], X]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I },
	                       typer :E <:< TupleSQL[G, LocalScope, X])
			:SelectFactory[F, E] { type Select = SubselectSQL[B, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectSQL[B, X]

			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr)
		}

	implicit def subselectAll
	             [F <: G { type Generalized = G; type Implicit = B; type Base = B; type DefineBase[+I <: FromClause] = I; type InnerRow = R },
	              G <: FromClause, B <: NonEmptyFrom, R <: Chain]
	             (implicit typer :F <:< FromClause { type Generalized = G; type Implicit = B; type Base = B;
	                                                 type DefineBase[+I <: FromClause] = I; type InnerRow = R })
            :SelectFactory[F, *] { type Select = SubselectSQL[B, R] } =
		new SelectFactory[F, *] {
			override type Select = SubselectSQL[B, R]

			override def apply(from :F, expr: *) =
				(from.innerRow :TupleSQL[F, LocalScope, from.InnerRow]).subselectFrom(from)
		}
}




object SelectFactory extends SecondChoiceSelectFactories {

	implicit def freeSelectColumnMapping[F <: G with FreeFrom { type Generalized <: G }, G <: FromClause,
	                                     E <: ColumnMapping[_, G], T]
	                                    (implicit origin :E <:< ColumnMapping[T, G],
	                                     offset :TableCount[G, _ <: Numeral],
	                                     project :OriginProjection[E, T] { type WithOrigin[A] <: ColumnMapping[T, A] })
			:SelectFactory[F, E] { type Select = FreeSelectColumnAs[project.WithOrigin, T] } =
		new SelectFactory[F, E] {
			override type Select = FreeSelectColumnAs[project.WithOrigin, T]

			override def apply(from :F, expr :E) = expr.freeSelectFrom(from)
		}

	implicit def freeSelectColumnComponent[F <: FreeFrom { type Generalized <: G }, G >: F <: FromClause,
	                                       M[A] <: ColumnMapping[T, A], T]
	                                      (implicit subject :M[G] <:< ColumnMapping[T, G])
			:SelectFactory[F, ColumnComponentSQL[G, M, T]] { type Select = FreeSelectColumnAs[M, T] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, T]] {
			override type Select = FreeSelectColumnAs[M, T]

			override def apply(from :F, expr :ColumnComponentSQL[G, M, T]) = SelectSQL(from, expr)
		}



	implicit def subselectColumnMapping
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: FromClause] = I },
		          G <: FromClause, B <: NonEmptyFrom, E <: ColumnMapping[_, _], X]
	             (implicit base :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I },
	                       mapping :E <:< ColumnMapping[X, G], offset :TableCount[G, _ <: Numeral],
	                       project :OriginProjection[E, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:SelectFactory[F, E] { type Select = SubselectColumnAs[B, project.WithOrigin, X] } =
		new SelectFactory[F, E] {
			override type Select = SubselectColumnAs[B, project.WithOrigin, X]

			override def apply(from :F, expr :E) = SelectSQL.subselect(from, expr.anchor(from))
		}

	implicit def subselectColumnComponent
	             [F <: G { type Generalized <: G; type Base = B; type DefineBase[+I <: FromClause] = I },
	              G <: FromClause, B <: NonEmptyFrom, M[A] <: ColumnMapping[V, A], V]
	             (implicit typer :F <:< FromClause { type Base = B; type DefineBase[+I <: FromClause] = I })
			:SelectFactory[F, ColumnComponentSQL[G, M, V]] { type Select = SubselectColumnAs[B, M, V] } =
		new SelectFactory[F, ColumnComponentSQL[G, M, V]] {
			override type Select = SubselectColumnAs[B, M, V]

			override def apply(from :F, expr :ColumnComponentSQL[G, M, V]) = SelectSQL.subselect(from, expr)
		}



	def apply[F <: FromClause, E](implicit factory :SelectFactory[F, E]) :factory.type = factory

}
