package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.BaseMapping
import net.noresttherein.oldsql.sql.GroupByClause.GroupingExpression
import net.noresttherein.oldsql.sql.MappingSQL.BaseComponentSQL
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope


class GroupByClauseTest {

	class GroupingExpressionImplicitCheck {{
		trait A[O] extends BaseMapping[Int, O]; trait B[O] extends BaseMapping[Int, O]
		trait C[O] extends BaseMapping[Int, O];trait D[O] extends BaseMapping[Int, O]
		trait E[O] extends BaseMapping[Int, O]; trait F[O] extends BaseMapping[Int, O]

		type S = From[A] InnerJoin B Subselect C LeftJoin D
		type T = FromClause AndFrom A Join B Subselect C Join D
		type G = S GroupByAll E ByAll F
		type U = T GroupByAll E ByAll F


		def groupingExpression[F <: FromClause, G <: FromClause, E]
		                      (implicit group :GroupingExpression[F, G, E]) :group.type = group

		@implicitNotFound("${R} is not the grouping of ${F} under ${G} by ${E} (or grouping is impossible).")
		class ExpectGrouping[F <: FromClause, G <: FromClause, E, R]

		implicit def ExpectGrouping[F <: FromClause, G <: FromClause, E, Y, R]
		                           (implicit group :GroupingExpression[F, G, E] { type Result = Y }, same :Y =:= R)
				:ExpectGrouping[F, G, E, R] = new ExpectGrouping

		def expectGrouping[F <: FromClause, G <: FromClause, E, R](implicit group :ExpectGrouping[F, G, E, R]) :group.type =
			group

		expectGrouping[T, S, E[T], S GroupByAll E]
		expectGrouping[T, S, BaseComponentSQL[T, E, T], S GroupByAll E]
		expectGrouping[T, S, ColumnSQL[T, GlobalScope, Int], S GroupByOne Int]
		expectGrouping[T, S, SQLExpression[T, GlobalScope, Int], S GroupByVal Int]

		expectGrouping[T, G, E[T], G ByAll E]
		expectGrouping[T, G, BaseComponentSQL[T, E, T], G ByAll E]
		expectGrouping[T, G, ColumnSQL[T, GlobalScope, Int], G ByOne Int]
		expectGrouping[T, G, SQLExpression[T, GlobalScope, Int], G ByVal Int]
	}}

}
