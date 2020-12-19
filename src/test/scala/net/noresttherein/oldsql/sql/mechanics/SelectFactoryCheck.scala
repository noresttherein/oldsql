package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~}
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.{Aggregated, AndFrom, ColumnSQL, From, IndexedMapping, InnerJoin, Join, JoinParam, LeftJoin, NonParam, OuterJoin, ParamSelect, RightJoin, RowProduct, SQLBoolean, Subselect}
import net.noresttherein.oldsql.sql.ast.{ConversionSQL, SelectSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ColumnComponentSQL, ComponentSQL}
import net.noresttherein.oldsql.sql.ast.SelectSQL.{SelectAs, SelectColumn, SelectColumnAs, SelectColumnMapping, SelectMapping, SubselectAs, SubselectColumn, SubselectColumnAs, SubselectColumnMapping, SubselectMapping, SubselectSQL, TopSelectAs, TopSelectColumn, TopSelectColumnAs, TopSelectSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.{ChainTuple, ListingSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ListingSQL.ListingColumn
import net.noresttherein.oldsql.sql.JoinParam.WithParam
import net.noresttherein.oldsql.sql.ParamSelect.ParamSelectAs
import net.noresttherein.oldsql.sql.UnboundParam.FromParam






class SelectFactoryCheck {{

	trait A[O] extends BaseMapping[Int, O]; trait B[O] extends BaseMapping[Long, O]
	trait C[O] extends BaseMapping[Float, O]; trait D[O] extends ColumnMapping[Double, O]
	type F = From[A] LeftJoin B OuterJoin C RightJoin D
	type G = RowProduct AndFrom A Join B Join C Join D
	type H = RowProduct AndFrom B Join C Join D

	type S = From[A] RightJoin B Subselect C InnerJoin D
	type T = RowProduct AndFrom A Join B Subselect C Join D
	type U = RowProduct AndFrom B Subselect C Join D
	type V = RowProduct NonParam A Join B

	type X = RowProduct AndFrom A LeftJoin B RightJoin C InnerJoin D
	type Y = RowProduct AndFrom A Join B Join C Join D
	type Z = RowProduct AndFrom C Join D

	type P = From[A] WithParam Int OuterJoin B WithParam Long 
	type Q = RowProduct AndFrom A WithParam Int Join B WithParam Long
	type R = RowProduct AndFrom FromParam.Of[Int]#P Join B WithParam Long  
	type Params = @~ ~ Int ~ Long
	
	val f :F = ???
	val s :S = ???
	val v :X = ???
	val p :P = ???

	type ColumnProjection[O] = ColumnMapping[Int, O]
	type BaseProjection[O] = BaseMapping[Int, O]

	@implicitNotFound("Result of selecting ${E} from ${F} is not ${S} (if it is selectable at all).")
	class ExpectSelect[F <: RowProduct, E, S]

	implicit def ExpectSelect[F <: RowProduct, E, R, S](implicit factory :SelectFactory[F, E] { type Select = R },
	                                                    conforms :R =:= S) :ExpectSelect[F, E, S] =
		new ExpectSelect

	def expectSelect[F <: RowProduct, E, S](implicit result :ExpectSelect[F, E, S]) :result.type = result

	
	expectSelect[F, ColumnMapping[Int, G], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[f.Self, ColumnMapping[Int, H], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[S, ColumnMapping[Int, T], SubselectColumnAs[V, ColumnProjection, Int]]
	expectSelect[s.Self, ColumnMapping[Int, U], SubselectColumnAs[V, ColumnProjection, Int]]

	expectSelect[F, ColumnComponentSQL[G, ColumnProjection, Int], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[f.Self, ColumnComponentSQL[H, ColumnProjection, Int], TopSelectColumnAs[ColumnProjection, Int]]
	expectSelect[S, ColumnComponentSQL[T, ColumnProjection, Int], SubselectColumnAs[V, ColumnProjection, Int]]
	expectSelect[s.Self, ColumnComponentSQL[U, ColumnProjection, Int], SubselectColumnAs[V, ColumnProjection, Int]]

	expectSelect[F, ListingColumn[G, LocalScope, "boo", Int], TopSelectColumnAs[IndexedMapping.Of[Int]#Column, Int]]
	expectSelect[f.Self, ListingColumn[H, GlobalScope, "boo", Int], TopSelectColumnAs[IndexedMapping.Of[Int]#Column, Int]]
	expectSelect[S, ListingColumn[T, LocalScope, "boo", Int], SubselectColumnAs[V, IndexedMapping.Of[Int]#Column, Int]]
	expectSelect[s.Self, ListingColumn[U, GlobalScope, "boo", Int], SubselectColumnAs[V, IndexedMapping.Of[Int]#Column, Int]]

	type IT = @~ |~ ("1" :~ Int) |~ ("2" :~ String)
	expectSelect[F, ListingSQL[H, LocalScope, IT], TopSelectAs[IndexedMapping.Of[IT]#Projection]]
	expectSelect[f.Self, ListingSQL.EmptyListing.type, TopSelectAs[IndexedMapping.Of[@~]#Projection]]
	expectSelect[S, ListingSQL[U, LocalScope, IT], SubselectAs[V, IndexedMapping.Of[IT]#Projection]]
	expectSelect[s.Self, ListingSQL.EmptyListing.type, SubselectAs[V, IndexedMapping.Of[@~]#Projection]]

	expectSelect[F, SQLBoolean[G, LocalScope], TopSelectColumn[Boolean]]
	expectSelect[f.Self, SQLBoolean[H, GlobalScope], TopSelectColumn[Boolean]]
	expectSelect[S, SQLBoolean[T, LocalScope], SubselectColumn[V, Boolean]]
	expectSelect[s.Self, SQLBoolean[U, GlobalScope], SubselectColumn[V, Boolean]]

	expectSelect[F, BaseMapping[Int, G], TopSelectAs[BaseProjection]]
	expectSelect[f.Self, BaseMapping[Int, H], TopSelectAs[BaseProjection]]
	expectSelect[S, BaseMapping[Int, T], SubselectAs[V, BaseProjection]]
	expectSelect[s.Self, BaseMapping[Int, U], SubselectAs[V, BaseProjection]]

	expectSelect[F, ComponentSQL[G, BaseProjection], TopSelectAs[BaseProjection]]
	expectSelect[f.Self, ComponentSQL[H, BaseProjection], TopSelectAs[BaseProjection]]
	expectSelect[S, ComponentSQL[T, BaseProjection], SubselectAs[V, BaseProjection]]
	expectSelect[s.Self, ComponentSQL[U, BaseProjection], SubselectAs[V, BaseProjection]]

	expectSelect[F, ConversionSQL[G, LocalScope, Int, Long], TopSelectSQL[Long]]
	expectSelect[f.Self, ConversionSQL[H, GlobalScope, Int, Long], TopSelectSQL[Long]]
	expectSelect[S, ConversionSQL[T, LocalScope, Int, Long], SubselectSQL[V, Long]]
	expectSelect[s.Self, ConversionSQL[U, GlobalScope, Int, Long], SubselectSQL[V, Long]]

	expectSelect[F, ChainTuple[H, LocalScope, @~ ~ Int ~ String], TopSelectSQL[@~ ~ Int ~ String]]
	expectSelect[f.Self, ChainTuple[H, GlobalScope, @~ ~ Int ~ String], TopSelectSQL[@~ ~ Int ~ String]]
	expectSelect[S, ChainTuple[U, LocalScope, @~ ~ Int ~ String], SubselectSQL[V, @~ ~ Int ~ String]]
	expectSelect[s.Self, ChainTuple[U, GlobalScope, @~ ~ Int ~ String], SubselectSQL[V, @~ ~ Int ~ String]]

	expectSelect[F, sql.*, TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	expectSelect[f.Self, sql.*, TopSelectSQL[@~ ~ Int ~ Long ~ Float ~ Double]]
	expectSelect[S, sql.*, SubselectSQL[V, @~ ~ Float ~ Double]]
	expectSelect[s.Self, sql.*, SubselectSQL[V, @~ ~ Float ~ Double]]

	expectSelect[F, ColumnSQL[Aggregated[G], LocalScope, Int], TopSelectColumn[Int]]
	expectSelect[f.Self, ColumnSQL[Aggregated[H], GlobalScope, Int], TopSelectColumn[Int]]
	expectSelect[S, ColumnSQL[Aggregated[T], LocalScope, Int], SubselectColumn[V, Int]]
	expectSelect[s.Self, ColumnSQL[Aggregated[U], GlobalScope, Int], SubselectColumn[V, Int]]

	expectSelect[v.Self, ColumnMapping[Int, Y], SelectColumnAs[v.Base, ColumnProjection, Int]]
	expectSelect[v.Self, ColumnMapping[Int, Z], SelectColumnAs[v.Base, ColumnProjection, Int]]

	expectSelect[v.Self, ColumnComponentSQL[Y, ColumnProjection, Int], SelectColumnAs[v.Base, ColumnProjection, Int]]
	expectSelect[v.Self, ColumnComponentSQL[Z, ColumnProjection, Int], SelectColumnAs[v.Base, ColumnProjection, Int]]

	expectSelect[v.Self, ListingColumn[Y, LocalScope, "boo", Int], SelectColumnAs[v.Base, IndexedMapping.Of[Int]#Column, Int]]
	expectSelect[v.Self, ListingColumn[Z, GlobalScope, "boo", Int], SelectColumnAs[v.Base, IndexedMapping.Of[Int]#Column, Int]]

	expectSelect[v.Self, ListingSQL[Y, LocalScope, IT], SelectAs[v.Base, IndexedMapping.Of[IT]#Projection]]
	expectSelect[v.Self, ListingSQL[Y, GlobalScope, IT], SelectAs[v.Base, IndexedMapping.Of[IT]#Projection]]

	expectSelect[v.Self, SQLBoolean[Y, LocalScope], SelectColumn[v.Base, Boolean]]
	expectSelect[v.Self, SQLBoolean[Z, GlobalScope], SelectColumn[v.Base, Boolean]]

	expectSelect[v.Self, BaseMapping[Int, Y], SelectAs[v.Base, BaseProjection]]
	expectSelect[v.Self, BaseMapping[Int, Z], SelectAs[v.Base, BaseProjection]]

	expectSelect[v.Self, ComponentSQL[Y, BaseProjection], SelectAs[v.Base, BaseProjection]]
	expectSelect[v.Self, ComponentSQL[Z, BaseProjection], SelectAs[v.Base, BaseProjection]]

	expectSelect[v.Self, ConversionSQL[Y, LocalScope, Int, Long], SelectSQL[v.Base, Long]]
	expectSelect[v.Self, ConversionSQL[Z, GlobalScope, Int, Long], SelectSQL[v.Base, Long]]

	expectSelect[v.Self, ChainTuple[Y, LocalScope, @~ ~ Int ~ String], SelectSQL[v.Base, @~ ~ Int ~ String]]
	expectSelect[v.Self, ChainTuple[Z, GlobalScope, @~ ~ Int ~ String], SelectSQL[v.Base, @~ ~ Int ~ String]]

	expectSelect[v.Self, sql.*, SelectSQL[v.Base, v.Row]]

	expectSelect[v.Self, ColumnSQL[Aggregated[Y], LocalScope, Int], SelectColumn[v.Base, Int]]
	expectSelect[v.Self, ColumnSQL[Aggregated[Z], GlobalScope, Int], SelectColumn[v.Base, Int]]


	
	expectSelect[P, ColumnMapping[Int, Q], ParamSelectAs[Params, ColumnProjection]]
	expectSelect[p.Self, ColumnMapping[Int, R], ParamSelectAs[Params, ColumnProjection]]

	expectSelect[P, ColumnComponentSQL[Q, ColumnProjection, Int], ParamSelectAs[Params, ColumnProjection]]
	expectSelect[p.Self, ColumnComponentSQL[R, ColumnProjection, Int], ParamSelectAs[Params, ColumnProjection]]

	expectSelect[P, ListingColumn[Q, LocalScope, "boo", Int], ParamSelectAs[Params, IndexedMapping.Of[Int]#Column]]
	expectSelect[p.Self, ListingColumn[R, GlobalScope, "boo", Int], ParamSelectAs[Params, IndexedMapping.Of[Int]#Column]]

	expectSelect[P, ListingSQL[Q, LocalScope, IT], ParamSelectAs[Params, IndexedMapping.Of[IT]#Projection]]
	expectSelect[p.Self, ListingSQL.EmptyListing.type, ParamSelectAs[Params, IndexedMapping.Of[@~]#Projection]]

	expectSelect[P, SQLBoolean[Q, LocalScope], ParamSelect[Params, Boolean]]
	expectSelect[p.Self, SQLBoolean[R, GlobalScope], ParamSelect[Params, Boolean]]

	expectSelect[P, BaseMapping[Int, Q], ParamSelectAs[Params, BaseProjection]]
	expectSelect[p.Self, BaseMapping[Int, R], ParamSelectAs[Params, BaseProjection]]

	expectSelect[P, ComponentSQL[Q, BaseProjection], ParamSelectAs[Params, BaseProjection]]
	expectSelect[p.Self, ComponentSQL[R, BaseProjection], ParamSelectAs[Params, BaseProjection]]

	expectSelect[P, ConversionSQL[Q, LocalScope, Int, Long], ParamSelect[Params, Long]]
	expectSelect[p.Self, ConversionSQL[R, GlobalScope, Int, Long], ParamSelect[Params, Long]]

	expectSelect[P, ChainTuple[Q, LocalScope, @~ ~ Int ~ String], ParamSelect[Params, @~ ~ Int ~ String]]
	expectSelect[p.Self, ChainTuple[R, GlobalScope, @~ ~ Int ~ String], ParamSelect[Params, @~ ~ Int ~ String]]

	expectSelect[P, sql.*, ParamSelect[Params, @~ ~ Int ~ Long]]
	expectSelect[p.Self, sql.*, ParamSelect[Params, @~ ~ Int ~ Long]]


}}

