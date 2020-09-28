package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{AggregateOf, ExtendedBy, FreeFrom, JoinedMappings, NonEmptyFrom, OuterGroupedFrom, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.GroupByAll.ByAll
import net.noresttherein.oldsql.sql.GroupParam.ByParam
import net.noresttherein.oldsql.sql.MappingSQL.{FreeComponent, JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.UnboundParam.{?:, ParamRelation}






/** A base type for all ''from'' clauses of SQL ''selects'' which make use of aggregate functions.
  * This encompasses both queries with a ''group by'' clause
  * - [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] - and a special
  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] implementation used for ''selects'' which feature
  * an aggregate function in their ''select'' clause, aggregating all rows into a single result.
  */
sealed trait AggregateClause extends FromClause { thisClause =>
//	override type FromLast >: Generalized <: AggregateClause
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing

	override type Generalized >: Self <: AggregateClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = FromClause
		type Base <: thisClause.Base
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: AggregateClause {
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

	override type This <: AggregateClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type LastTable[F <: FromClause] = thisClause.LastTable[F]
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


	override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	protected override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		throw new UnsupportedOperationException(s"GroupByClause.filterNext (on $this)")



	/** Type constructor for this clause. It replaces
	  * the [[net.noresttherein.oldsql.sql.AggregateClause.Grouped Grouped]] portion of this type
	  * (that is, all relations which are aggregated and unavailable individually to SQL expressions based on
	  * this clause) with `U`.
	  */
	type Grouping[+U <: FromSome] <: AggregateClause

	/** The ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.FromClause.Explicit ''explicit'']] portion.
	  * This is the ''generalized'' supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete this.Discrete]].
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Generalized`;
	  * for [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */
	type GeneralizedDiscrete >: Discrete <: FromSome

	/** The self typ of the ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.FromClause.Explicit ''explicit'']] portion.
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Self`;
	  * for [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */
	type Discrete <: FromSome

	/** The [[net.noresttherein.oldsql.sql.FromClause FromClause]] forming the prefix of this clause which contains
	  * all the aggregated relations in its [[net.noresttherein.oldsql.sql.FromClause.Explicit ''explicit'']] portion.
	  * These relations are ''not'' available to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on this clause.
	  */
	val from :Discrete

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.FromClause.Implicit ''implicit'']] prefix with
	  * [[net.noresttherein.oldsql.sql.FromClause FromClause]]. Equals `from.Explicit`.
	  */
	type GeneralizedGrouped = from.Explicit

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.FromClause.Outer outer]] prefix with
	  * the upper bound for the left side of the [[net.noresttherein.oldsql.sql.AndFrom join]] between the
	  * outer and inner sections. Equals `from.Inner`.
	  */
	type Grouped = from.Inner


	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.aggregateClause(this)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateClause]

}







/** A special [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] adapter used when selecting
  * an aggregate [[net.noresttherein.oldsql.sql.AggregateSQL! expression]], coalescing all rows from the adapted
  * clause into a single result. Its existence is needed as such aggregate functions are - aside from ''selects''
  * with a ''group by'' clause - allowed only in the ''select'' clause, and not the ''where'' clause.
  * It is considered a part of the implementors' interface, exposed for applications
  * which add their own aggregate [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction functions]] or
  * SQL dialects (requiring access to the whole [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] hierarchy).
  * Applications which satisfy themselves with the standard aggregate functions defined in
  * [[net.noresttherein.oldsql.sql.AggregateSQL$ AggregateSQL]] should have no need for this class, as appropriate
  * 'shortcut' methods are defined in [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]]
  * implicit extension class for [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome non-empty, ungrouped]] clauses.
  * Be warned that several standard methods (in particular `where`) are unsupported, as this type should be used only
  * as an intermediate value created directly before calling one of its `select` methods.
  * @tparam F a non-empty, ungrouped ''from'' clause which rows are being aggregated for the purpose of selecting
  *           a result of an aggregate function. As with [[net.noresttherein.oldsql.sql.GroupByClause]], all relations
  *           following the last [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  *           (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) are unavailable
  *           to [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] based on this clause other than
  *           as arguments to aggregate functions.
  * @author Marcin Mościcki
  */
sealed trait Aggregated[+F <: FromSome] extends DecoratedFrom[F] with AggregateClause { thisClause =>
	override type LastMapping[O] = Nothing
	override type LastTable[E <: FromClause] = Nothing
	override type FromLast = FromClause

	/** Throws an `NoSuchElementException`. */
	override def last :Nothing = throw new NoSuchElementException(s"($this).last")

	/** Throws an `NoSuchElementException`. */
	override def lastAsIn[E <: FromSome](implicit extension :FromLast PrefixOf E) :Nothing = last

	override type Bound = FromSome
	override type Generalized = Aggregated[clause.Generalized]
	override type Self = Aggregated[clause.Self]
	override type This = Aggregated[clause.This]


	override def withClause[C <: FromSome](from :C) :Aggregated[C] = Aggregated(from)


	/** Always returns [[net.noresttherein.oldsql.sql.SQLTerm.True True]]. For the actual, ungrouped ''where'' clause
	  * use `this.clause.filter`.
	  */
	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] = True

	/** Throws an `UnsupportedOperationException`. */
	override def where(condition :GlobalBoolean[Generalized]) :Nothing =
		throw new UnsupportedOperationException(s"($this).where($condition)")



	override def isEmpty :Boolean = outer.isEmpty
	override def fullSize :Int = outer.fullSize
	override def innerSize :Int = 0

	override type FullRow = clause.OuterRow

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		clause.outerRow(target)(explicitSpan extend extension)

	override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:LazyList[RelationSQL.AnyIn[E]] =
		outer.fullTableStack(target)(explicitSpan.extend(extension))


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
		Aggregated[clause.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withClause(clause.joinedWith(prefix, firstJoin))

	override type JoinedWithSubselect[+P <:  NonEmptyFrom] = Aggregated[clause.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withClause(clause.joinedWithSubselect(prefix))

	override def appendedTo[P <: DiscreteFrom](prefix :P) :Aggregated[clause.JoinedWith[P, AndFrom]] =
		withClause(clause.appendedTo(prefix))



	override type Explicit = Aggregated[clause.Explicit]
	override type Inner = Aggregated[clause.Inner]
	override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: FromClause] = clause.DefineBase[I]

	override def base :Base = clause.base

	override type InnerRow = @~

	/** Returns an empty chain expression. */
	override def innerRow[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:ChainTuple[E, GlobalScope, @~] =
		EmptyChain

	/** Returns an empty list. */
	override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty

	override type OuterRow = clause.OuterRow

	override def outerRow[E <: FromClause](target :E)(implicit extension :Implicit ExtendedBy E)
			:ChainTuple[E, GlobalScope, OuterRow] =
		clause.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = Aggregated[clause.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:Aggregated[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		withClause(clause.asSubselectOf(newOuter))



	override type Grouping[+D <: FromSome] = Aggregated[D]
	override type GeneralizedDiscrete = clause.Generalized
	override type Discrete = clause.Self

	//these could probably be made to work, but I'm lazy and this class shouldn't be used for much else than a select call
	override type FromRelation[T[O] <: MappingAt[O]] = Nothing

	/** Throws `UnsupportedOperationException`. */
	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Relation[M])
	//               (implicit cast :InferSubject[this.type, Subselect, M, T, S])
	                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:Nothing =
		throw new UnsupportedOperationException(s"($this).from($first)")

	override type FromSubselect[+S <: NonEmptyFrom] = Nothing

	/** Throws `UnsupportedOperationException`. */
	override def from[S <: NonEmptyFrom with FreeFrom](subselect :S) :Nothing =
		throw new UnsupportedOperationException(s"($this).from($subselect)")

	/** Throws `UnsupportedOperationException`. */
	override def fromSubselect[E <: NonEmptyFrom]
	                          (subselect :E)(implicit extension :subselect.Implicit ExtendedBy Generalized) :Nothing =
		throw new UnsupportedOperationException(s"($this).fromSubselect($subselect)")



	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.aggregated(this)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Aggregated.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :Aggregated.* if other canEqual this => other.clause == clause
		case _ => false
	}

	override def hashCode :Int = clause.hashCode



	private[sql] override def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupByClause =
		throw new UnsupportedOperationException


	override def toString :String= clause.toString + " aggregated"
}






object Aggregated {

	def apply[F <: FromSome](discrete :F) :Aggregated[F] =
		new Aggregated[discrete.type] {
			override val clause :discrete.type = discrete
			override val from = discrete.self
			override val outer = from.outer
		}


	def unapply[F <: FromSome](from :Aggregated[F]) :Some[F] = Some(from.clause)

	def unapply[F <: FromClause](from :DecoratedFrom[F]) :Option[F] = from match {
		case _ :Aggregated[_] => Some(from.clause)
		case _ => None
	}

	def unapply(from :FromClause) :Option[FromSome] = from match {
		case aggro :Aggregated.* => Some(aggro.clause)
		case _ => None
	}



	type * = Aggregated[_ <: FromSome]
}







/** A base type for all clauses which contain a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupByClause extends NonEmptyFrom with AggregateClause { thisClause =>
	override type FromLast >: Generalized <: GroupByClause
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing

	override type Generalized >: Self <: GroupByClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = FromClause
		type Base <: thisClause.Base
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: GroupByClause {
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

	override type This >: this.type <: GroupByClause {
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
	  * SQL standard for true joins, or the ''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this 'join'.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#filter]]
	  */
	def condition :LocalBoolean[Generalized]

	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''having'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `having` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	def withCondition(filter :LocalBoolean[Generalized]) :This


	/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.having having]].
	  * This means that the applied filter condition will become the ''having'' clause of the generated select,
	  * rather than the ''where'' clause.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
	  *                  functioning as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	override def where(condition :GlobalBoolean[Generalized]) :This = having(condition)

	/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.having having]].
	  * This means that the applied filter condition will become the ''having'' clause of the generated select,
	  * rather than the ''where'' clause.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
	  *                  functioning as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	override def where(condition :JoinedMappings[Generalized] => GlobalBoolean[Generalized]) :This =
		having(condition)

	/** Creates a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  * instance's filter and the given `SQLBoolean`. The filter becomes the part of the ''having'' clause
	  * of the SQL ''select'' based on this clause or some its extension. This works analogously to the
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method with the same signature on ungrouped clauses.
	  * @see [[net.noresttherein.oldsql.sql.GroupByClause.having]]
	  */
	def having(condition :LocalBoolean[Generalized]) :This = withCondition(condition && filter)

	/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
	  * and becomes a part of `this.filter` representing the ''having'' clause of the SQL statement.
	  * This works analogously to the [[net.noresttherein.oldsql.sql.FromClause.where where]] method
	  * with the same signature on ungrouped clauses.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance functioning
	  *                  as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  *                  The provided `JoinedRelations` do ''not'' include the relations from the actual ''from''
	  *                  clause (i.e. listed by the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]]),
	  *                  as the values for their individual rows are unavailable. They can however be accessed
	  *                  by the implicitly injected to it
	  *                  [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.grouped grouped]] method.
	  *                  The expressions for these relations will be based however on the `GeneralizedDiscrete` type
	  *                  rather than this clause; they can be adapted for the required `this.Generalized` by
	  *                  passing them to an
	  *                  [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction aggregate function]],
	  *                  creating an [[net.noresttherein.oldsql.sql.AggregateSQL AggregateSQL]] in the result.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
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



	/** Type constructor for the whole ''group by'' clause - that is all 'joins' appearing after the ungrouped,
	  * 'true' ''from'' clause `U`, starting with the [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] compound.
	  */
	override type Grouping[+U <: FromSome] <: GroupByClause

//	type GeneralizedGrouped >: GeneralizedDiscrete <: FromSome
//
//	type Grouped >: Discrete <: GeneralizedGrouped

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */ //overriden for docs only
	override type GeneralizedDiscrete >: Discrete <: FromSome

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */ //overriden for docs only
	override type Discrete <: FromSome

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */ //todo: rename to grouped, conflicts with the from method for creating subselects
	override val from :Discrete



	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.groupByClause(this)


	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException

}






object GroupByClause {

	def unapply(f :FromClause) :Option[DiscreteFrom] = f match {
		case group :GroupByClause => Some(group.from)
		case _ => None
	}



	implicit class GroupByClauseExtension[F <: GroupByClause](val thisClause :F) extends AnyVal {
		def grouped :JoinedMappings[thisClause.Discrete] = new JoinedMappings(thisClause.from)

		def byAll[E <: GlobalSQL[F, V], V, G <: GroupByClause]
		         (expr :JoinedMappings[F] => E)(implicit grouping :GroupingExpression[F, E, V, G]) :G =
			grouping(thisClause, expr(thisClause.mappings))
	}

	implicit def JoinedMappingsExtension[F <: GroupByClause](mappings :JoinedMappings[F]) :GroupByClauseExtension[F] =
		new GroupByClauseExtension[F](mappings.thisClause)



	implicit class OuterGroupedFromExtension[F <: OuterGroupedFrom](private val thisClause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm] :F ByParam X = GroupParam(thisClause, ParamRelation[X]())

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
		@inline def param[X :SQLForm](name :String) :F ByParam X = GroupParam(thisClause, ParamRelation[X](name))

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
			GroupParam(thisClause, form ?: (name.value :N))

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