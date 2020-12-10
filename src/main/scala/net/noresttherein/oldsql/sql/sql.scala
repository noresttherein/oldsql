package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}



package object sql {

	/** An occurrence of an SQL [[net.noresttherein.oldsql.schema.Relation relation]] in a ''from'' clause
	  * of an SQL ''select''. It declares the [[net.noresttherein.oldsql.schema.Mapping mapping]] for that relation
	  * and is a valid SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] based on the ''from'' clause
	  * of its second type parameter. The relation object (both this value and wrapped `Relation`) may
	  * be a relation only in the abstract sense, and not a valid ''derived table''; chief example here
	  * are grouping relations which represent a subset of attributes from a ''group by'' clause.
	  * @see [[net.noresttherein.oldsql.sql.JoinedTable]]
	  */
	type JoinedRelation[F <: RowProduct, M[O] <: MappingAt[O]] = ast.MappingSQL.JoinedRelation[F, M]

	val JoinedRelation = ast.MappingSQL.JoinedRelation

	/** An occurrence of an SQL [[net.noresttherein.oldsql.schema.Relation.Table Table]] in a ''from'' clause
	  * of an SQL ''select''. It declares the [[net.noresttherein.oldsql.schema.Mapping mapping]] for that table
	  * and is a valid SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] based on the ''from'' clause
	  * of its second type parameter.
	  * @see [[net.noresttherein.oldsql.sql.JoinedRelation]]
	  */
	type JoinedTable[F <: RowProduct, M[O] <: MappingAt[O]] = ast.MappingSQL.JoinedTable[F, M]

	val JoinedTable = ast.MappingSQL.JoinedTable


	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as the argument to
	  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]].
	  */
	final class * private[sql] ()

	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as an argument to
	  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension.select select]].
	  */
	final val * = new *


	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] `S`.
	  */
	type SQLBoolean[-F <: RowProduct, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.GlobalBoolean GlobalBoolean]]`[F] <: LocalBoolean[F]`.
	  */
	type LocalBoolean[-F <: RowProduct] = ColumnSQL[F, LocalScope, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its enclosing selects (for subselect clauses).
	  * Such expressions can also be extended over to subselects of the clause `F` using the
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] method. This is in particular the expression type
	  * used in all ''where'' clauses.
	  * Note that `GlobalBoolean[F] <: `[[net.noresttherein.oldsql.sql.LocalBoolean LocalBoolean]]`F`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GlobalBoolean[-F <: RowProduct] = ColumnSQL[F, GlobalScope, Boolean]


	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] `S`.
	  */
	type SQLString[-F <: RowProduct, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.GlobalString GlobalString]]`[F] <: LocalString[F]`.
	  */
	type LocalString[-F <: RowProduct] = ColumnSQL[F, LocalScope, String]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of `String` type which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its enclosing selects (for subselect clauses).
	  * Such expressions can also be extended over to subselects of the clause `F` using the
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] method. This is in particular the expression type
	  * used in all ''where'' clauses.
	  * Note that `GlobalString[F] <: `[[net.noresttherein.oldsql.sql.LocalString LocalString]]`F`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GlobalString[-F <: RowProduct] = ColumnSQL[F, GlobalScope, String]



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[X, _]`.
	  */ //consider: renaming to GroupBy. Question only if this or GroupBy
	type GroupByVal[+F <: FromSome, X] = F GroupBy Group[X]#T

	object GroupByVal {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, X] //fixme: add filter once factory methods for all joins are refactored
		         (from :F, group :SQLExpression[U, GlobalScope, X])//, filter :LocalBoolean[F#Generalized GroupByVal X] = True)
				:F GroupByVal X =
			GroupBy(from, GroupingRelation(group))//, filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#TypedProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[X, _]`.
	  */
	type ByVal[+F <: GroupByClause, X] = F By Group[X]#T

	object ByVal {

		def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], X] //fixme: add filter once joins are refactored
		         (from :G, group :SQLExpression[F, GlobalScope, X])//, filter :LocalBoolean[G#Generalized ByVal X] = True)
				:G ByVal X =
			By(from, GroupingRelation(group))//, filter)

		type * = By.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L By MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L By MappingOf[X]#TypedProjection }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupBy group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type GroupByOne[+F <: FromSome, X] = F GroupBy Group[X]#C

	object GroupByOne {

		def apply[U <: RowProduct, F <: FromSome { type Generalized <: U }, X]//fixme: add filter once join factory methods are refactored
		         (from :F, group :ColumnSQL[U, GlobalScope, X])//, filter :LocalBoolean[F#Generalized GroupByOne X] = True)
				:F GroupByOne X =
			GroupBy(from, GroupingRelation(group))//, filter)

		type * = GroupBy.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupBy MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupBy MappingOf[X]#ColumnProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.By following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type ByOne[+F <: GroupByClause, X] = F By Group[X]#C

	object ByOne {

		def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], X]//fixme: add filter once joins are refactored
		         (from :G, group :ColumnSQL[F, GlobalScope, X])//, filter :LocalBoolean[G#Generalized ByOne X] = True)
				:G ByOne X =
			By(from, GroupingRelation(group))//, filter)

		type * = By.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L By MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L By MappingOf[X]#ColumnProjection }
	}

}

