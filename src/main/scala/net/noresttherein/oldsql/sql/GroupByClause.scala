package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.{BaseMapping, SQLForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedMappings, NonEmptyFrom, OuterGroupedFrom}
import net.noresttherein.oldsql.sql.GroupByAll.ByAll
import net.noresttherein.oldsql.sql.GroupParam.ByParam
import net.noresttherein.oldsql.sql.MappingSQL.{FreeComponent, JoinedRelation}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalSQL, LocalSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.UnboundParam.{?:, ParamRelation}






/** A base type for all clauses which contain a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupByClause extends NonEmptyFrom { thisClause =>
	override type FromLast <: GroupByClause
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing

	override type This <: GroupByClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
		type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
		type FromSubselect[+F <: NonEmptyFrom] = thisClause.FromSubselect[F]
	}



	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#filter]]
	  */
	def condition :LocalBoolean[Generalized]

	private[this] val cachedFilter = Lazy { filter(generalized) }

	override def filter :LocalBoolean[Generalized] = cachedFilter.get


	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''where'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `where` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	def withCondition(filter :LocalBoolean[Generalized]) :This


	/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause#having having]].
	  * This means that the applied filter condition will become the ''having'' clause of the generated select,
	  * rather than the ''where'' clause. The condition is combined using `&&` with `this.condition`
	  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
	  *                  functioning as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	override def where(condition :GlobalBoolean[Generalized]) :This = having(condition)

	/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause#having having]].
	  * This means that the applied filter condition will become the ''having'' clause of the generated select,
	  * rather than the ''where'' clause. The condition is combined using `&&` with `this.condition`
	  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
	  *                  functioning as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	override def where(condition :JoinedMappings[Generalized] => GlobalBoolean[Generalized]) :This =
		having(condition)

	def having(condition :LocalBoolean[Generalized]) :This = withCondition(condition && filter)

	def having(condition :JoinedMappings[Generalized] => LocalBoolean[Generalized]) :This = {
		val grounded = SQLScribe.groundFreeComponents(generalized)(condition(new JoinedMappings(generalized)))
		having(grounded)
	}

	/** Apply a filter condition to the last grouping expression in this clause. The condition is combined using `&&`
	  * with `this.condition` and becomes a part of `this.filter` representing the ''having'' clause of the SQL statement.
	  * It is equivalent to `this.having(mappings => condition(mappings.last))`.
	  * @param condition a function accepting the expression for the last relation in this clause and creating
	  *                  an additional SQL expression for the ''having'' clause.
	  * @return an `Extended` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of the preexisting `this.condition`
	  *         and the `LocalBoolean` returned by the passed filter function.
	  */
	def havingLast(condition :JoinedRelation[FromLast, LastMapping] => LocalBoolean[FromLast]) :This =
		having(SQLScribe.groundFreeComponents(generalized)(condition(last)))


	override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	protected override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		throw new UnsupportedOperationException(s"GroupByClause.filterNext (on $this)")



	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */
	type GeneralizedDiscrete <: DiscreteFrom

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */
	type Discrete <: GeneralizedDiscrete

//		type Grouped = GroupByClause { type Discrete = thisClause.Discrete }

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */
	val from :Discrete


	type GeneralizedGrouped = from.Explicit

	type Grouped = from.Inner

	override val outer :Outer



	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupedFrom :Nothing =
		throw new UnsupportedOperationException

}






object GroupByClause {

	def unapply(f :FromClause) :Option[DiscreteFrom] = f match {
		case group :GroupByClause => Some(group.from)
		case _ => None
	}


	implicit class GroupByClauseExtension[F <: GroupByClause](private val clause :F) extends AnyVal {
		def byAll[E <: GlobalSQL[F, V], V, G <: GroupByClause]
		         (expr :JoinedMappings[F] => E)(implicit grouping :GroupingExpression[F, E, V, G]) :G =
			grouping(clause, expr(clause.mappings))

	}



	implicit class OuterGroupedFromExtension[F <: OuterGroupedFrom](private val clause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm] :F ByParam X = GroupParam(clause, ParamRelation[X]())

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm](name :String) :F ByParam X = GroupParam(clause, ParamRelation[X](name))

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F GroupParam (N ?: X)#T =
			GroupParam(clause, form ?: (name.value :N))

	}



	trait GroupingExpression[F <: GroupByClause, -S <: GlobalSQL[F, V], V, G <: GroupByClause] {
		def apply(clause :F, expr :S) :G
	}


	object GroupingExpression {
		implicit def groupByComponent[F <: GroupByClause, M[O] <: BaseMapping[S, O], S]
				:GroupingExpression[F, FreeComponent[F, M, S], S, F ByAll M] =
			(clause :F, expr :FreeComponent[F, M, S]) => ByAll[F, M, S](clause, ???)(True)

	}


}