package net.noresttherein.oldsql.sql.ast

import net.noresttherein.oldsql.collection.NaturalMap
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.{ColumnSQL, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}




/** An expression using column schema of an existing [[net.noresttherein.oldsql.schema.Mapping Mapping]],
  * but providing arbitrary expressions as values for all of its (used/default) columns.
  * It is similar to [[net.noresttherein.oldsql.sql.ast.EditedLValueSQL EditedLValueSQL]], but unlike in the latter,
  * there is no underlying mapping here: values for all columns must be provided explicitly.
  * As there is no generic way of enforcing on type level that all mandatory columns for
  * the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]] appropriate to the use case
  * are indeed included, this expression bypasses type safety guaranteed by standard
  * `MappingSQL` implementations such as [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
  * and [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]. As such, it should be used
  * as a last resort.
  * @author Marcin Mościcki
  */ //todo:
trait ImpostorMappingSQL[-F <: RowProduct, -S >: Grouped <: Single, M[O] <: MappingAt[O]]
	extends MappingSQL[F, S, M, M[Unit]#Subject]
{
	type Origin >: F <: RowProduct
	def values[C >: Grouped <: S] :NaturalMap[MappingAt[Origin]#Column, ColumnSQL.from[Origin]#rows[C]#apply]
}



object ImpostorMappingSQL {

}
