package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.SQLExpression.LocalScope
import net.noresttherein.oldsql.sql.{Aggregated, AndFrom, ColumnSQL, From, InnerJoin, Join, LeftJoin, OuterJoin, RightJoin, RowProduct, SQLBoolean, Subselect}
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ColumnComponentSQL, ComponentSQL}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectAs, SelectColumn, SelectColumnAs, SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumn, SubselectColumnAs, SubselectColumnMapping, SubselectMapping, SubselectSQL, TopSelectAs, TopSelectColumn, TopSelectColumnAs, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple






class SelectFactoryCheck {{

	trait A[O] extends BaseMapping[Int, O]; trait B[O] extends BaseMapping[Long, O]
	trait C[O] extends BaseMapping[Float, O]; trait D[O] extends ColumnMapping[Double, O]
	type F = From[A] LeftJoin B OuterJoin C RightJoin D
	type G = RowProduct AndFrom A Join B Join C Join D
	type H = RowProduct AndFrom B Join C Join D

	type S = From[A] RightJoin B Subselect C InnerJoin D
	type T = RowProduct AndFrom A Join B Subselect C Join D
	type U = RowProduct AndFrom B Subselect C Join D
	type R = RowProduct AndFrom A Join B

	type V = RowProduct AndFrom A LeftJoin B RightJoin C InnerJoin D
	type W = RowProduct AndFrom A Join B Join C Join D
	type Z = RowProduct AndFrom C Join D

	val f :F = ???
	val s :S = ???
	val v :V = ???

	type ColumnProjection[O] = ColumnMapping[Int, O]
	type BaseProjection[O] = BaseMapping[Int, O]

	@implicitNotFound("Result of selecting ${E} from ${F} is not ${S} (if it is selectable at all).")
	class ExpectSelect[F <: RowProduct, E, S]

	implicit def ExpectSelect[F <: RowProduct, E, R, S](implicit factory :SelectFactory[F, E] { type Select = R },
	                                                    conforms :R =:= S) :ExpectSelect[F, E, S] =
		new ExpectSelect

	def expectSelect[F <: RowProduct, E, S](implicit result :ExpectSelect[F, E, S]) :result.type = result

	implicitly[SelectFactory[F, ColumnMapping[Int, G]]]
	expectSelect[F, ColumnMapping[Int, G], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[f.Self, ColumnMapping[Int, H], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[S, ColumnMapping[Int, T], SubselectColumnAs[R, ColumnProjection, Int]]
	expectSelect[s.Self, ColumnMapping[Int, U], SubselectColumnAs[R, ColumnProjection, Int]]

	expectSelect[F, ColumnComponentSQL[G, ColumnProjection, Int], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[f.Self, ColumnComponentSQL[H, ColumnProjection, Int], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[S, ColumnComponentSQL[T, ColumnProjection, Int], SubselectColumnAs[R, ColumnProjection, Int]]
	expectSelect[s.Self, ColumnComponentSQL[U, ColumnProjection, Int], SubselectColumnAs[R, ColumnProjection, Int]]

	expectSelect[F, SQLBoolean[G, LocalScope], TopSelectColumn[Boolean]]
	expectSelect[f.Self, SQLBoolean[H, LocalScope], TopSelectColumn[Boolean]]
	expectSelect[S, SQLBoolean[T, LocalScope], SubselectColumn[R, Boolean]]
	expectSelect[s.Self, SQLBoolean[U, LocalScope], SubselectColumn[R, Boolean]]

	expectSelect[F, BaseMapping[Int, G], TopSelectAs[BaseProjection]]
	expectSelect[f.Self, BaseMapping[Int, H], TopSelectAs[BaseProjection]]
	expectSelect[S, BaseMapping[Int, T], SubselectAs[R, BaseProjection]]
	expectSelect[s.Self, BaseMapping[Int, U], SubselectAs[R, BaseProjection]]

	expectSelect[F, ComponentSQL[G, BaseProjection], TopSelectAs[BaseProjection]]
	expectSelect[f.Self, ComponentSQL[H, BaseProjection], TopSelectAs[BaseProjection]]
	expectSelect[S, ComponentSQL[T, BaseProjection], SubselectAs[R, BaseProjection]]
	expectSelect[s.Self, ComponentSQL[U, BaseProjection], SubselectAs[R, BaseProjection]]

	expectSelect[F, ConversionSQL[G, LocalScope, Int, Long], TopSelectSQL[Long]]
	expectSelect[f.Self, ConversionSQL[H, LocalScope, Int, Long], TopSelectSQL[Long]]
	expectSelect[S, ConversionSQL[T, LocalScope, Int, Long], SubselectSQL[R, Long]]
	expectSelect[s.Self, ConversionSQL[U, LocalScope, Int, Long], SubselectSQL[R, Long]]

	expectSelect[F, ChainTuple[H, LocalScope, @~ ~ Int ~ String], TopSelectSQL[@~ ~ Int ~ String]]
	expectSelect[f.Self, ChainTuple.EmptyChain.type, TopSelectSQL[@~]]
	expectSelect[S, ChainTuple[U, LocalScope, @~ ~ Int ~ String], SubselectSQL[R, @~ ~ Int ~ String]]
	expectSelect[s.Self, ChainTuple.EmptyChain.type, SubselectSQL[R, @~]]

	expectSelect[F, sql.*, TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	expectSelect[f.Self, sql.*, TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	expectSelect[S, sql.*, SubselectSQL[R, @~ ~ Float ~ Double]]
	expectSelect[s.Self, sql.*, SubselectSQL[R, @~ ~ Float ~ Double]]

	expectSelect[F, ColumnSQL[Aggregated[G], LocalScope, Int], TopSelectColumn[Int]]
	expectSelect[f.Self, ColumnSQL[Aggregated[H], LocalScope, Int], TopSelectColumn[Int]]
	expectSelect[S, ColumnSQL[Aggregated[T], LocalScope, Int], SubselectColumn[R, Int]]
	expectSelect[s.Self, ColumnSQL[Aggregated[U], LocalScope, Int], SubselectColumn[R, Int]]

	expectSelect[v.Self, ColumnMapping[Int, W], SelectColumnAs[v.Base, ColumnProjection, Int]]
	expectSelect[v.Self, ColumnMapping[Int, Z], SelectColumnAs[v.Base, ColumnProjection, Int]]

	expectSelect[v.Self, ColumnComponentSQL[W, ColumnProjection, Int], SelectColumnAs[v.Base, ColumnProjection, Int]]
	expectSelect[v.Self, ColumnComponentSQL[Z, ColumnProjection, Int], SelectColumnAs[v.Base, ColumnProjection, Int]]

	expectSelect[v.Self, SQLBoolean[W, LocalScope], SelectColumn[v.Base, Boolean]]
	expectSelect[v.Self, SQLBoolean[Z, LocalScope], SelectColumn[v.Base, Boolean]]

	expectSelect[v.Self, BaseMapping[Int, W], SelectAs[v.Base, BaseProjection]]
	expectSelect[v.Self, BaseMapping[Int, Z], SelectAs[v.Base, BaseProjection]]

	expectSelect[v.Self, ComponentSQL[W, BaseProjection], SelectAs[v.Base, BaseProjection]]
	expectSelect[v.Self, ComponentSQL[Z, BaseProjection], SelectAs[v.Base, BaseProjection]]

	expectSelect[v.Self, ConversionSQL[W, LocalScope, Int, Long], SelectSQL[v.Base, Long]]
	expectSelect[v.Self, ConversionSQL[Z, LocalScope, Int, Long], SelectSQL[v.Base, Long]]

	expectSelect[v.Self, ChainTuple[W, LocalScope, @~ ~ Int ~ String], SelectSQL[v.Base, @~ ~ Int ~ String]]
	expectSelect[v.Self, ChainTuple[Z, LocalScope, @~ ~ Int ~ String], SelectSQL[v.Base, @~ ~ Int ~ String]]

	expectSelect[v.Self, sql.*, SelectSQL[v.Base, v.Row]]

	expectSelect[v.Self, ColumnSQL[Aggregated[W], LocalScope, Int], SelectColumn[v.Base, Int]]
	expectSelect[v.Self, ColumnSQL[Aggregated[Z], LocalScope, Int], SelectColumn[v.Base, Int]]

}}

