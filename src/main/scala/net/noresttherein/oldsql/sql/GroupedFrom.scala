package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{JoinedEntities, NonEmptyFrom}






/** A base type for all clauses which contain a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupedFrom extends NonEmptyFrom { thisClause =>
	override type FromLast <: GroupedFrom
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing
	override type This <: GroupedFrom


	override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	protected override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		throw new UnsupportedOperationException(s"GroupedFrom.filterNext (on $this)")


	/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.FromClause#where where]], introduced
	  * in order to clarify that applied filter condition will become the ''having'' clause of the generated select,
	  * rather than the ''where'' clause. The condition is combined using `&&` with `this.condition`
	  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] instance
	  *                  functioning as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	def having(condition :JoinedEntities[Generalized] => SQLBoolean[Generalized]) :This = where(condition)

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedUngrouped`.
	  */
	type GeneralizedUngrouped <: DiscreteFrom

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Ungrouped`.
	  */
	type Ungrouped <: GeneralizedUngrouped

//		type Grouped = GroupedFrom { type Ungrouped = thisClause.Ungrouped }

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */
	val from :Ungrouped

	override val outer :Outer



	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupedFrom :Nothing =
		throw new UnsupportedOperationException

}






object GroupedFrom {

}