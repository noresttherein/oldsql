package net.noresttherein.oldsql

import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.SQLTerm.True



package object sql {

	type SQLBoolean[-F <: FromClause, -S >: LocalScope <: GlobalScope] = ColumnSQL[F, S, Boolean]
	type LocalBoolean[-F <: FromClause] = ColumnSQL[F, LocalScope, Boolean]
	type GlobalBoolean[-F <: FromClause] = ColumnSQL[F, GlobalScope, Boolean]



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupByAll group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[X, _]`.
	  */
	type GroupByVal[+F <: FromSome, X] = F GroupByAll Group[X]#T

	object GroupByVal {

		def apply[F <: FromSome, X]
		         (from :F, group :SQLExpression[F, GlobalScope, X], filter :LocalBoolean[F#Generalized GroupByVal X] = True)
				:F GroupByVal X =
			GroupByAll(from, GroupingRelation(group), filter)

		type * = GroupByAll.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupByAll MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupByAll MappingOf[X]#TypedProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.ByAll following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[X, _]`.
	  */
	type ByVal[+F <: GroupByClause, X] = F ByAll Group[X]#T

	object ByVal {

		def apply[F <: FromSome, G <: GroupByClause { type GeneralizedDiscrete <: F }, X]
		         (from :G, group :SQLExpression[F, GlobalScope, X], filter :LocalBoolean[G#Generalized ByVal X] = True)
				:G ByVal X =
			ByAll(from, GroupingRelation(group), filter)

		type * = ByAll.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L ByAll MappingOf[X]#TypedProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L ByAll MappingOf[X]#TypedProjection }
	}



	/** The first grouping expression of a [[net.noresttherein.oldsql.sql.GroupByAll group by]] clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type GroupByOne[+F <: FromSome, X] = F GroupByAll Group[X]#C

	object GroupByOne {

		def apply[F <: FromSome, X]
		         (from :F, group :ColumnSQL[F, GlobalScope, X], filter :LocalBoolean[F#Generalized GroupByOne X] = True)
				:F GroupByOne X =
			GroupByAll(from, GroupingRelation(group), filter)

		type * = GroupByAll.*

		type WithLeft[L <: FromSome] = { type F[X] = L GroupByAll MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: FromSome] = L GroupByAll MappingOf[X]#ColumnProjection }
	}



	/** A [[net.noresttherein.oldsql.sql.ByAll following]] grouping expression of a ''group by'' clause,
	  * represented by a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]`[X, _]`.
	  */
	type ByOne[+F <: GroupByClause, X] = F ByAll Group[X]#C

	object ByOne {

		def apply[F <: FromSome, G <: GroupByClause { type GeneralizedDiscrete <: F }, X]
		         (from :G, group :ColumnSQL[F, GlobalScope, X], filter :LocalBoolean[G#Generalized ByOne X] = True)
				:G ByOne X =
			ByAll(from, GroupingRelation(group), filter)

		type * = ByAll.*

		type WithLeft[L <: GroupByClause] = { type F[X] = L ByAll MappingOf[X]#ColumnProjection }

		type WithRight[X] = { type F[L <: GroupByClause] = L ByAll MappingOf[X]#ColumnProjection }
	}

}

