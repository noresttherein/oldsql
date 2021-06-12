package net.noresttherein.oldsql.sql.mechanics

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.ComponentSQL
import net.noresttherein.oldsql.sql.{AndFrom, By, ByOne, ByVal, ColumnSQL, From, GroupBy, GroupByOne, GroupByVal, InnerJoin, Join, LeftJoin, NonParam, RowProduct, SQLExpression, Subselect}



class GroupingExpressionCheck {{
	trait A[O] extends BaseMapping[Int, O]; trait B[O] extends BaseMapping[Int, O]
	trait C[O] extends BaseMapping[Int, O];trait D[O] extends BaseMapping[Int, O]
	trait E[O] extends BaseMapping[Int, O]; trait F[O] extends BaseMapping[Int, O]

	type S = From[A] InnerJoin B Subselect C LeftJoin D
	type T = RowProduct NonParam A Join B Subselect C Join D
	type G = S GroupBy E By F
	type U = T GroupBy E By F


	def groupingExpression[F <: RowProduct, G <: RowProduct, E]
	                      (implicit group :GroupingExpression[F, G, E]) :group.type = group

	@implicitNotFound("${R} is not the grouping of ${F} under ${G} by ${E} (or grouping is impossible).")
	class ExpectGrouping[F <: RowProduct, G <: RowProduct, E, R]

	implicit def ExpectGrouping[F <: RowProduct, G <: RowProduct, E, Y, R]
	                           (implicit group :GroupingExpression[F, G, E] { type Result = Y }, same :Y =:= R)
			:ExpectGrouping[F, G, E, R] = new ExpectGrouping

	def expectGrouping[F <: RowProduct, G <: RowProduct, E, R](implicit group :ExpectGrouping[F, G, E, R]) :group.type =
		group

	expectGrouping[T, S, E[T], S GroupBy E]
	expectGrouping[T, S, ComponentSQL[T, E], S GroupBy E]
	expectGrouping[T, S, ColumnSQL[T, GlobalScope, Int], S GroupByOne Int]
	expectGrouping[T, S, SQLExpression[T, GlobalScope, Int], S GroupByVal Int]

	expectGrouping[T, G, E[T], G By E]
	expectGrouping[T, G, ComponentSQL[T, E], G By E]
	expectGrouping[T, G, ColumnSQL[T, GlobalScope, Int], G ByOne Int]
	expectGrouping[T, G, SQLExpression[T, GlobalScope, Int], G ByVal Int]
}}