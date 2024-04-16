package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.{implicitNotFound, nowarn}

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.IndexedMapping
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.{Aggregated, AndFrom, ColumnSQL, From, InnerJoin, Join, JoinParam, LeftJoin, NonParam, OuterJoin, RightJoin, RowProduct, Select, SelectFrom, SQLBoolean, Subselect, WithParam}
import net.noresttherein.oldsql.sql.ParamClause.UnboundParam
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.ast.{AdaptedSQL, ChainSQL, ChainTuple, ComponentSQL, EditedComponentSQL, EditedLooseComponent, EmptySQL, GenericColumnComponentSQL, IndexedSQL, LooseColumn, LooseComponent, SelectAs, SelectColumn, SelectColumnAs, SelectSQL}
import net.noresttherein.oldsql.sql.ast.IndexedSQL.LabeledColumnSQL
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.SelectColumn.{SubselectColumn, TopSelectColumn}
import net.noresttherein.oldsql.sql.ast.SelectColumnAs.{SubselectColumnAs, TopSelectColumnAs}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SubselectSQL, TopSelectSQL}
import net.noresttherein.oldsql.sql.RowProduct.{GroundRow, NonEmptyRow, TopRow}
import net.noresttherein.oldsql.sql.mechanics.CanSelect.CanSelectDirect






class CanSelectCheck {{

	trait A[O] extends BaseMapping[Int, O]; trait B[O] extends BaseMapping[Long, O]
	trait C[O] extends BaseMapping[Float, O]; trait D[O] extends BaseColumn[Double, O]
	type F = From[A] LeftJoin B OuterJoin C RightJoin D
	type I = From[A] Join B Join C Join D
	type G = RowProduct AndFrom A Join B Join C Join D
	type H = RowProduct AndFrom B Join C Join D

	type S = From[A] RightJoin B Subselect C InnerJoin D
	type T = RowProduct AndFrom A Join B Subselect C Join D
	type U = RowProduct AndFrom B Subselect C Join D
	type O = RowProduct NonParam A Join B

	type X = RowProduct SelectFrom A LeftJoin B RightJoin C InnerJoin D
	type Y = RowProduct AndFrom A Join B Join C Join D
	type Z = RowProduct AndFrom C Join D

	type P = From[A] WithParam Int OuterJoin B WithParam Long
	type Q = RowProduct AndFrom A WithParam Int Join B WithParam Long
	type R = RowProduct AndFrom UnboundParam.of[Int]#M Join B WithParam Long
	type Params = @~ ~ Int ~ Long

	val f :F = ???
	val s :S = ???
	val t :T = ???
	val x :X = ???
	val p :P = ???

	type ColumnProjection[O] = BaseColumn[Int, O]
	type BaseProjection[O] = BaseMapping[Int, O]

	implicit def CanSelectResultValidator[F <: RowProduct, E](factory :CanSelect[F, E]) =
		new CanSelectResultValidator[factory.Select] {}

	trait CanSelectResultValidator[S] extends Any {
		implicit def returns[R](implicit ev :S <:< R) :Unit = {}
	}

	def summon[X](implicit x :X) :x.type = x

	CanSelect[F, TypedColumn[Int, G]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[f.Self, TypedColumn[Int, H]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[S, TypedColumn[Int, T]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[s.Self, TypedColumn[Int, T]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[x.Self, TypedColumn[Int, Y]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[x.Self, TypedColumn[Int, Z]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[P, TypedColumn[Int, Q]].returns[SelectMapping[Params, ColumnProjection]]
	CanSelect[p.Self, TypedColumn[Int, R]].returns[SelectMapping[Params, ColumnProjection]]

	CanSelect[F, BaseMapping[Int, G]].returns[TopSelectAs[BaseProjection]]
	CanSelect[f.Self, BaseMapping[Int, H]].returns[TopSelectAs[BaseProjection]]
	CanSelect[S, BaseMapping[Int, T]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[s.Self, BaseMapping[Int, U]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[x.Self, BaseMapping[Int, Y]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[x.Self, BaseMapping[Int, Z]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[P, BaseMapping[Int, Q]].returns[SelectMapping[Params, BaseProjection]]
	CanSelect[p.Self, BaseMapping[Int, R]].returns[SelectMapping[Params, BaseProjection]]

	CanSelect[F, LooseColumn[G, ColumnProjection, Int]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[f.Self, LooseColumn[H, ColumnProjection, Int]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[S, LooseColumn[T, ColumnProjection, Int]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[s.Self, LooseColumn[U, ColumnProjection, Int]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[x.Self, LooseColumn[Y, ColumnProjection, Int]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[x.Self, LooseColumn[Z, ColumnProjection, Int]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[P, LooseColumn[Q, ColumnProjection, Int]].returns[SelectMapping[Params, ColumnProjection]]
	CanSelect[p.Self, LooseColumn[R, ColumnProjection, Int]].returns[SelectMapping[Params, ColumnProjection]]

	CanSelect[F, LooseComponent[G, BaseProjection, Int]].returns[TopSelectAs[BaseProjection]]
	CanSelect[f.Self, LooseComponent[H, BaseProjection, Int]].returns[TopSelectAs[BaseProjection]]
	CanSelect[S, LooseComponent[T, BaseProjection, Int]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[s.Self, LooseComponent[U, BaseProjection, Int]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[x.Self, LooseComponent[Y, BaseProjection, Int]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[x.Self, LooseComponent[Z, BaseProjection, Int]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[P, LooseComponent[Q, BaseProjection, Int]].returns[SelectMapping[Params, BaseProjection]]
	CanSelect[p.Self, LooseComponent[R, BaseProjection, Int]].returns[SelectMapping[Params, BaseProjection]]

	CanSelect[F, GenericColumnComponentSQL[G, ColumnProjection, Int]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[f.Self, GenericColumnComponentSQL[H, ColumnProjection, Int]].returns[TopSelectColumnAs[ColumnProjection, Int]]
	CanSelect[S, GenericColumnComponentSQL[T, ColumnProjection, Int]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[s.Self, GenericColumnComponentSQL[U, ColumnProjection, Int]].returns[SubselectColumnAs[O, ColumnProjection, Int]]
	CanSelect[x.Self, GenericColumnComponentSQL[Y, ColumnProjection, Int]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[x.Self, GenericColumnComponentSQL[Z, ColumnProjection, Int]].returns[SelectColumnAs[x.Base, ColumnProjection, Int]]
	CanSelect[P, GenericColumnComponentSQL[Q, ColumnProjection, Int]].returns[SelectMapping[Params, ColumnProjection]]
	CanSelect[p.Self, GenericColumnComponentSQL[R, ColumnProjection, Int]].returns[SelectMapping[Params, ColumnProjection]]

	CanSelect[F, ComponentSQL[G, BaseProjection]].returns[TopSelectAs[BaseProjection]]
	CanSelect[f.Self, ComponentSQL[H, BaseProjection]].returns[TopSelectAs[BaseProjection]]
	CanSelect[S, ComponentSQL[T, BaseProjection]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[s.Self, ComponentSQL[U, BaseProjection]].returns[SubselectAs[O, BaseProjection]]
	CanSelect[x.Self, ComponentSQL[Y, BaseProjection]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[x.Self, ComponentSQL[Z, BaseProjection]].returns[SelectAs[x.Base, BaseProjection]]
	CanSelect[P, ComponentSQL[Q, BaseProjection]].returns[SelectMapping[Params, BaseProjection]]
	CanSelect[p.Self, ComponentSQL[R, BaseProjection]].returns[SelectMapping[Params, BaseProjection]]

	CanSelect[F, EditedLooseComponent[G, BaseProjection, Int]].returns[TopSelectSQL[Int]]
	CanSelect[f.Self, EditedLooseComponent[H, BaseProjection, Int]].returns[TopSelectSQL[Int]]
	CanSelect[S, EditedLooseComponent[T, BaseProjection, Int]].returns[SubselectSQL[O, Int]]
	CanSelect[s.Self, EditedLooseComponent[U, BaseProjection, Int]].returns[SubselectSQL[O, Int]]
	CanSelect[x.Self, EditedLooseComponent[Y, BaseProjection, Int]].returns[SelectSQL[x.Base, Int]]
	CanSelect[x.Self, EditedLooseComponent[Z, BaseProjection, Int]].returns[SelectSQL[x.Base, Int]]
	CanSelect[P, EditedLooseComponent[Q, BaseProjection, Int]].returns[Select[Params, Int]]
	CanSelect[p.Self, EditedLooseComponent[R, BaseProjection, Int]].returns[Select[Params, Int]]

	CanSelect[F, EditedComponentSQL[G, BaseProjection, Int]].returns[TopSelectSQL[Int]]
	CanSelect[f.Self, EditedComponentSQL[H, BaseProjection, Int]].returns[TopSelectSQL[Int]]
	CanSelect[S, EditedComponentSQL[T, BaseProjection, Int]].returns[SubselectSQL[O, Int]]
	CanSelect[s.Self, EditedComponentSQL[U, BaseProjection, Int]].returns[SubselectSQL[O, Int]]
	CanSelect[x.Self, EditedComponentSQL[Y, BaseProjection, Int]].returns[SelectSQL[x.Base, Int]]
	CanSelect[x.Self, EditedComponentSQL[Z, BaseProjection, Int]].returns[SelectSQL[x.Base, Int]]
	CanSelect[P, EditedComponentSQL[Q, BaseProjection, Int]].returns[Select[Params, Int]]
	CanSelect[p.Self, EditedComponentSQL[R, BaseProjection, Int]].returns[Select[Params, Int]]

	CanSelect[F, LabeledColumnSQL[G, Grouped, "boo", Int]].returns[TopSelectColumnAs[IndexedMapping.of[Int]#Column, Int]]
	CanSelect[f.Self, LabeledColumnSQL[H, Single, "boo", Int]].returns[TopSelectColumnAs[IndexedMapping.of[Int]#Column, Int]]
	CanSelect[S, LabeledColumnSQL[T, Grouped, "boo", Int]].returns[SubselectColumnAs[O, IndexedMapping.of[Int]#Column, Int]]
	CanSelect[s.Self, LabeledColumnSQL[U, Single, "boo", Int]].returns[SubselectColumnAs[O, IndexedMapping.of[Int]#Column, Int]]
	CanSelect[x.Self, LabeledColumnSQL[Y, Grouped, "boo", Int]].returns[SelectColumnAs[x.Base, IndexedMapping.of[Int]#Column, Int]]
	CanSelect[x.Self, LabeledColumnSQL[Z, Single, "boo", Int]].returns[SelectColumnAs[x.Base, IndexedMapping.of[Int]#Column, Int]]
	CanSelect[P, LabeledColumnSQL[Q, Grouped, "boo", Int]].returns[SelectMapping[Params, IndexedMapping.of[Int]#Column]]
	CanSelect[p.Self, LabeledColumnSQL[R, Single, "boo", Int]].returns[SelectMapping[Params, IndexedMapping.of[Int]#Column]]

	type IT = @~ |~ ("1" :~ Int) |~ ("2" :~ String)
	CanSelect[F, IndexedSQL[H, Grouped, IT]].returns[TopSelectAs[IndexedMapping.of[IT]#Mapping]]
//	CanSelect[f.Self, IndexedSQL.EmptyIndex.type].returns[TopSelectAs[IndexedMapping.of[@~]#Mapping]]
	CanSelect[f.Self, EmptySQL].returns[TopSelectAs[IndexedMapping.of[@~]#Mapping]]
	CanSelect[S, IndexedSQL[U, Grouped, IT]].returns[SubselectAs[O, IndexedMapping.of[IT]#Mapping]]
	CanSelect[s.Self, EmptySQL].returns[SubselectAs[O, IndexedMapping.of[@~]#Mapping]]
	CanSelect[x.Self, IndexedSQL[Y, Grouped, IT]].returns[SelectAs[x.Base, IndexedMapping.of[IT]#Mapping]]
	CanSelect[x.Self, IndexedSQL[Y, Single, IT]].returns[SelectAs[x.Base, IndexedMapping.of[IT]#Mapping]]
	CanSelect[P, IndexedSQL[Q, Grouped, IT]].returns[SelectMapping[Params, IndexedMapping.of[IT]#Mapping]]
	CanSelect[p.Self, EmptySQL].returns[SelectMapping[Params, IndexedMapping.of[@~]#Mapping]]

	CanSelect[x.Self, ColumnSQL[Aggregated[Y], Grouped, Int]].returns[SelectColumn[x.Base, Int]]
	CanSelect[x.Self, ColumnSQL[Aggregated[Z], Single, Int]].returns[SelectColumn[x.Base, Int]]
	CanSelect[P, ColumnSQL[Q, Grouped, Int]].returns[Select[Params, Int]]
	CanSelect[p.Self, ColumnSQL[R, Single, Int]].returns[Select[Params, Int]]

	CanSelect[F, SQLBoolean[G, Grouped]].returns[TopSelectColumn[Boolean]]
	CanSelect[f.Self, SQLBoolean[H, Single]].returns[TopSelectColumn[Boolean]]
	CanSelect[S, SQLBoolean[T, Grouped]].returns[SubselectColumn[O, Boolean]]
	CanSelect[s.Self, SQLBoolean[U, Single]].returns[SubselectColumn[O, Boolean]]
	CanSelect[x.Self, SQLBoolean[Y, Grouped]].returns[SelectColumn[x.Base, Boolean]]
	CanSelect[x.Self, SQLBoolean[Z, Single]].returns[SelectColumn[x.Base, Boolean]]
	CanSelect[P, SQLBoolean[Q, Grouped]].returns[Select[Params, Boolean]]
	CanSelect[p.Self, SQLBoolean[R, Single]].returns[Select[Params, Boolean]]

	CanSelect[F, AdaptedSQL[G, Grouped, Int, Long]].returns[TopSelectSQL[Long]]
	CanSelect[f.Self, AdaptedSQL[H, Single, Int, Long]].returns[TopSelectSQL[Long]]
	CanSelect[S, AdaptedSQL[T, Grouped, Int, Long]].returns[SubselectSQL[O, Long]]
	CanSelect[s.Self, AdaptedSQL[U, Single, Int, Long]].returns[SubselectSQL[O, Long]]
	CanSelect[x.Self, AdaptedSQL[Y, Grouped, Int, Long]].returns[SelectSQL[x.Base, Long]]
	CanSelect[x.Self, AdaptedSQL[Z, Single, Int, Long]].returns[SelectSQL[x.Base, Long]]
	CanSelect[P, AdaptedSQL[Q, Grouped, Int, Long]].returns[Select[Params, Long]]
	CanSelect[p.Self, AdaptedSQL[R, Single, Int, Long]].returns[Select[Params, Long]]

	CanSelect[F, ChainSQL[H, Grouped, @~ ~ Int, String]].returns[TopSelectSQL[@~ ~ Int ~ String]]
	CanSelect[f.Self, ChainSQL[H, Single, @~ ~ Int, String]].returns[TopSelectSQL[@~ ~ Int ~ String]]
	CanSelect[S, ChainSQL[U, Grouped, @~ ~ Int, String]].returns[SubselectSQL[O, @~ ~ Int ~ String]]
	CanSelect[s.Self, ChainSQL[U, Single, @~ ~ Int, String]].returns[SubselectSQL[O, @~ ~ Int ~ String]]
	CanSelect[x.Self, ChainSQL[Y, Grouped, @~ ~ Int, String]].returns[SelectSQL[x.Base, @~ ~ Int ~ String]]
	CanSelect[x.Self, ChainSQL[Z, Single, @~ ~ Int, String]].returns[SelectSQL[x.Base, @~ ~ Int ~ String]]
	CanSelect[P, ChainSQL[Q, Grouped, @~ ~ Int, String]].returns[Select[Params, @~ ~ Int ~ String]]
	CanSelect[p.Self, ChainSQL[R, Single, @~ ~ Int, String]].returns[Select[Params, @~ ~ Int ~ String]]

	CanSelect[F, ChainTuple[H, Grouped, @~ ~ Int ~ String]].returns[TopSelectSQL[@~ ~ Int ~ String]]
	CanSelect[f.Self, ChainTuple[H, Single, @~ ~ Int ~ String]].returns[TopSelectSQL[@~ ~ Int ~ String]]
	CanSelect[S, ChainTuple[U, Grouped, @~ ~ Int ~ String]].returns[SubselectSQL[O, @~ ~ Int ~ String]]
	CanSelect[s.Self, ChainTuple[U, Single, @~ ~ Int ~ String]].returns[SubselectSQL[O, @~ ~ Int ~ String]]
	CanSelect[x.Self, ChainTuple[Y, Grouped, @~ ~ Int ~ String]].returns[SelectSQL[x.Base, @~ ~ Int ~ String]]
	CanSelect[x.Self, ChainTuple[Z, Single, @~ ~ Int ~ String]].returns[SelectSQL[x.Base, @~ ~ Int ~ String]]
	CanSelect[P, ChainTuple[Q, Grouped, @~ ~ Int ~ String]].returns[Select[Params, @~ ~ Int ~ String]]
	CanSelect[p.Self, ChainTuple[R, Single, @~ ~ Int ~ String]].returns[Select[Params, @~ ~ Int ~ String]]

	CanSelect[F, sql.*].returns[TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	CanSelect[f.Self, sql.*].returns[TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	CanSelect[S, sql.*].returns[SubselectSQL[O, @~ ~ Float ~ Double]]
	CanSelect[s.Self, sql.*].returns[SubselectSQL[O, @~ ~ Float ~ Double]]
	CanSelect[x.Self, sql.*].returns[SelectSQL[x.Base, x.Row]]
	CanSelect[P, sql.*].returns[Select[Params, @~ ~ Int ~ Long]]
	CanSelect[p.Self, sql.*].returns[Select[Params, @~ ~ Int ~ Long]]

	CanSelect[F, ColumnSQL[Aggregated[G], Grouped, Int]].returns[TopSelectColumn[Int]]
	CanSelect[f.Self, ColumnSQL[Aggregated[H], Single, Int]].returns[TopSelectColumn[Int]]
	CanSelect[S, ColumnSQL[Aggregated[T], Grouped, Int]].returns[SubselectColumn[O, Int]]
	CanSelect[s.Self, ColumnSQL[Aggregated[U], Single, Int]].returns[SubselectColumn[O, Int]]
	CanSelect[x.Self, ColumnSQL[Aggregated[Y], Grouped, Int]].returns[SelectColumn[x.Base, Int]]
	CanSelect[x.Self, ColumnSQL[Aggregated[Z], Grouped, Int]].returns[SelectColumn[x.Base, Int]]

}}

