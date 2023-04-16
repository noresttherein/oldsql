package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.bases.BaseColumn
import net.noresttherein.oldsql.sql.{ColumnSQL, FromSome, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}




/** A wrapper over an arbitrary grouping expression grounded in a proper ''from'' clause `F`.
  * It offers an alternate, more concise way for creating SQL ''selects'' with a ''group by'' clause:
  * instead of adding the grouping expression [[net.noresttherein.oldsql.sql.ast.GroupedSQL.expr expr]]
  * to the ''from'' clause using [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]]
  * or [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] methods
  * of [[net.noresttherein.oldsql.sql.FromSome FromSome]] and [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]],
  * and then referencing it in the ''select'' clause as a relation expression
  * [[net.noresttherein.oldsql.sql.ast.GroupingSQL GroupingSQL]] (or its component), it allows to add the expression
  * directly to the ''select'' clause expression. When the latter is used to create
  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] or [[net.noresttherein.oldsql.sql.Select Select]],
  * it is searched for all subexpressions of this type, at which point select's
  * [[net.noresttherein.oldsql.sql.Select.SelectTemplate.from from]] clause is expanded by adding
  * a [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]]
  * `this.`[[net.noresttherein.oldsql.sql.ast.GroupedSQL.relation relation]]. At this point, this expression
  * is also substituted in the ''select'' clause for a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
  * for that relation.
  * @author Marcin Mościcki
  */ //todo: GroupedSQL
trait GroupedSQL[F <: FromSome, V]
	extends SQLExpression[F#GeneralizedAggregate, Grouped, V]
{
	type GroupingMapping[O] <: TypedMapping[V, O]
	val expr :SQLExpression[F, Single, V]
	val relation :GroupingRelation[F, GroupingMapping]


}


object GroupedSQL {

}


trait GroupedColumn[F <: FromSome, V] extends GroupedSQL[F, V] with ColumnSQL[F#GeneralizedAggregate, Grouped, V] {
	override type GroupingMapping[O] <: TypedColumn[V, O]
	override val expr :ColumnSQL[F, Single, V]
}

object GroupedColumn {

}

trait GroupedMappingSQL[F <: FromSome, M[A] <: TypedMapping[V, A], V]
	extends GroupedSQL[F, V] with MappingSQL[F#GeneralizedAggregate, Grouped, M, V]
{
	override type GroupingMapping[O] = M[O]
	override val expr :MappingSQL[F, Single, M, V]
}

object GroupedMappingSQL {

}


trait GroupedColumnMappingSQL[F <: FromSome, M[A] <: BaseColumn[V, A], V]
	extends GroupedMappingSQL[F, M, V] with ColumnMappingSQL[F#GeneralizedAggregate, Single, M, V]
{
	override val expr :ColumnMappingSQL[F, Single, M, V]
}


object GroupedColumnMappingSQL {

}
