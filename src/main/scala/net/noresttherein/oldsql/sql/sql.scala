package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{GroupingOfGeneralized, GroupingOf}
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.True



package object sql {

	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as the argument to
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension.select select]].
	  */
	final class * private[sql] ()

	/** Represents the multipurpose 'everything' wildcard `*` in SQL. Can be used as the argument to
	  * [[net.noresttherein.oldsql.sql.AggregateSQL.Count Count]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension.select select]].
	  */
	final val * = new *


	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value and
	  * with [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope scope]] `S`.
	  */
	type SQLBoolean[-F <: FromClause, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can be used solely
	  * within ''select'' and ''having'' clauses of the most deeply nested select represented by the ''from'' clause `F`.
	  * It is typically a derivative expression, containing an [[net.noresttherein.oldsql.sql.AggregateSQL aggregate]]
	  * expression as its subexpression.
	  * Note that [[net.noresttherein.oldsql.sql.GlobalBoolean GlobalBoolean]]`[F] <: LocalBoolean[F]`.
	  */
	type LocalBoolean[-F <: FromClause] = ColumnSQL[F, LocalScope, Boolean]

	/** An SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] of Boolean value which can occur in any place
	  * of the SQL select represented by the ''from'' clause `F` or its enclosing selects (for subselect clauses).
	  * Such expressions can also be extended over to subselects of the clause `F` using the
	  * [[net.noresttherein.oldsql.sql.SQLExpression.basedOn basedOn]] method. This is in particular the expression type
	  * used in all ''where'' clauses.
	  * Note that `GlobalBoolean[F] <: `[[net.noresttherein.oldsql.sql.LocalBoolean LocalBoolean]]`F`.
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope]]
	  */
	type GlobalBoolean[-F <: FromClause] = ColumnSQL[F, GlobalScope, Boolean]



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupByAll group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[X, _]`.
	  */ //consider: renaming to GroupBy. Question only if this or GroupByAll
	type GroupByVal[+F <: FromSome, X] = F GroupByAll Group[X]#T

	object GroupByVal {

		def apply[U <: FromClause, F <: FromSome { type Generalized <: U }, X] //fixme: add filter once factory methods for all joins are refactored
		         (from :F, group :SQLExpression[U, GlobalScope, X])//, filter :LocalBoolean[F#Generalized GroupByVal X] = True)
				:F GroupByVal X =
			GroupByAll(from, GroupingRelation(group))//, filter)

		type * = GroupByAll.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupByAll MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupByAll MappingOf[X]#TypedProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.ByAll following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[X, _]`.
	  */
	type ByVal[+F <: GroupByClause, X] = F ByAll Group[X]#T

	object ByVal {

		def apply[F <: FromClause, G <: GroupingOfGeneralized[F], X] //fixme: add filter once joins are refactored
		         (from :G, group :SQLExpression[F, GlobalScope, X])//, filter :LocalBoolean[G#Generalized ByVal X] = True)
				:G ByVal X =
			ByAll(from, GroupingRelation(group))//, filter)

		type * = ByAll.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L ByAll MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L ByAll MappingOf[X]#TypedProjection }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupByAll group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type GroupByOne[+F <: FromSome, X] = F GroupByAll Group[X]#C

	object GroupByOne {

		def apply[U <: FromClause, F <: FromSome { type Generalized <: U }, X]//fixme: add filter once join factory methods are refactored
		         (from :F, group :ColumnSQL[U, GlobalScope, X])//, filter :LocalBoolean[F#Generalized GroupByOne X] = True)
				:F GroupByOne X =
			GroupByAll(from, GroupingRelation(group))//, filter)

		type * = GroupByAll.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupByAll MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupByAll MappingOf[X]#ColumnProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.ByAll following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type ByOne[+F <: GroupByClause, X] = F ByAll Group[X]#C

	object ByOne {

		def apply[F <: FromClause, G <: GroupingOfGeneralized[F], X]//fixme: add filter once joins are refactored
		         (from :G, group :ColumnSQL[F, GlobalScope, X])//, filter :LocalBoolean[G#Generalized ByOne X] = True)
				:G ByOne X =
			ByAll(from, GroupingRelation(group))//, filter)

		type * = ByAll.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L ByAll MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L ByAll MappingOf[X]#ColumnProjection }
	}

}

