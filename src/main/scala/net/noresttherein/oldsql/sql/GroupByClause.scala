package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Mapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFrom, FromClauseMatrix, GeneralizedGroupingOf, JoinedMappings, NonEmptyFrom, NonEmptyFromMatrix, OuterGroupingFrom, PartOf, PrefixOf, TableCount}
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix
import net.noresttherein.oldsql.sql.GroupParam.ByParam
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, ComponentSQL, JoinedRelation, LooseComponent, RelationSQL}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, LocalScope}
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
sealed trait AggregateClause extends FromClause with FromClauseMatrix[AggregateClause] { thisClause =>

	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing

	override type Generalized >: Dealiased <: AggregateClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = FromClause
		type Base <: thisClause.Base
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: AggregateClause {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: AggregateClause {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}



	override type FilterNext[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	protected override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
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
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */
	type GeneralizedDiscrete >: Discrete <: FromSome

	/** The self typ of the ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.FromClause.Explicit ''explicit'']] portion.
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Self`;
	  * for [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]]
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
sealed trait Aggregated[+F <: FromSome] extends DecoratedFrom[F] with AggregateClause {
	thisClause =>

	override type LastMapping[O] = Nothing
	override type LastTable[E <: FromClause] = Nothing
	override type FromLast = FromClause

	/** Throws an `NoSuchElementException`. */
	override def last :Nothing = throw new NoSuchElementException(s"($this).last")

	/** Throws an `NoSuchElementException`. */
	override def lastAsIn[E <: FromSome](implicit extension :FromLast PrefixOf E) :Nothing = last

	override type Bound = FromSome
	override type Generalized = Aggregated[clause.Generalized]
	override type Dealiased = Aggregated[clause.Dealiased]
	override type Self = Aggregated[clause.Self]
	override type Copy = Aggregated[clause.Copy]


	override def withClause[C <: FromSome](from :C) :Aggregated[C] = Aggregated(from)


	/** Always returns [[net.noresttherein.oldsql.sql.SQLTerm.True True]]. For the actual, ungrouped ''where'' clause
	  * use `this.clause.filter`.
	  */
	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] = True

	/** Throws an `UnsupportedOperationException`. */
	override def filtered[S >: GlobalScope <: GlobalScope](condition :SQLBoolean[Generalized, S]) :Nothing =
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
			override val outer = clause.outer
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
  * @see [[net.noresttherein.oldsql.sql.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupByClause extends NonEmptyFrom with AggregateClause with GroupByClauseMatrix[GroupByClause, GroupByClause] {
	thisClause =>

	override type FromLast >: Generalized <: GroupByClause
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing

	override type Generalized >: Dealiased <: GroupByClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = FromClause
		type Base <: thisClause.Base
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: GroupByClause {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: GroupByClause {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
	}


	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this 'join'.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#filter]]
	  */
	def condition :LocalBoolean[Generalized]


	/** Type constructor for the whole ''group by'' clause - that is all 'joins' appearing after the ungrouped,
	  * 'true' ''from'' clause `U`, starting with the [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] compound.
	  */
	override type Grouping[+U <: FromSome] <: GroupByClause

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */ //overriden for docs only
	override type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */ //overriden for docs only
	override type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}


	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */ //todo: rename, conflicts with the from method for creating subselects. 'grouped' is an extension method returning JoinedMappings[Discrete]
	override val from :Discrete



	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.groupByClause(this)


	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException

}






object GroupByClause {

	type Group[S] = { type T[O] = BaseMapping[S, O]; type C[O] = ColumnMapping[S, O] }


	def unapply(f :FromClause) :Option[DiscreteFrom] = f match {
		case group :GroupByClause => Some(group.from)
		case _ => None
	 }



	trait GroupByClauseMatrix[+U <: GroupByClause, +F <: U]
		extends NonEmptyFromMatrix[U, F]
	{ thisClause :GroupByClause with GroupByClauseMatrix[U, F] =>

		override type Copy <: F {
			type LastMapping[O] = thisClause.LastMapping[O]
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
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
		}


		/** A copy of this clause with the `condition` being replaced with the given `filter`.
		  * This does not replace the whole ''having'' filter, as the conditions (if present) of the left clause remain
		  * unchanged. It is the target of the `having` and other filtering methods (which add to the condition, rather
		  * then completely replacing it).
		  */
		def withCondition(filter :LocalBoolean[Generalized]) :Copy

		override def filtered[S >: LocalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :Copy =
			withCondition(condition && filter)


		override type JoinFilter = Nothing

		override def filtered(condition :Nothing) :Nothing =
			throw new UnsupportedOperationException(s"$this.on")

		/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]].
		  * This means that the applied filter condition will become the ''having'' clause of the generated select,
		  * rather than the ''where'' clause.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def where(condition :GlobalBoolean[Generalized]) :F = having(condition)

		/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having having]].
		  * This means that the applied filter condition will become the ''having'' clause of the generated select,
		  * rather than the ''where'' clause.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def where(condition :JoinedMappings[Generalized] => GlobalBoolean[Generalized]) :F =
			having(condition)

		/** Creates a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
		  * instance's filter and the given `SQLBoolean`. The filter becomes the part of the ''having'' clause
		  * of the SQL ''select'' based on this clause or some its extension. This works analogously to the
		  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method with the same signature on ungrouped clauses.
		  * @see [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix.having]]
		  */
		def having(condition :LocalBoolean[Generalized]) :F =
			withCondition(condition && filter)

		/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
		  * and becomes a part of `this.filter` representing the ''having'' clause of the SQL statement.
		  * This works analogously to the [[net.noresttherein.oldsql.sql.FromClause.where where]] method
		  * with the same signature on ungrouped clauses.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  *                  The provided `JoinedRelations` do ''not'' include the relations from the actual ''from''
		  *                  clause (i.e. listed by the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]]),
		  *                  as the values for their individual rows are unavailable. They can however be accessed
		  *                  by [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings.grouped grouped]] method.
		  *                  The expressions for these relations will be based however on the `GeneralizedDiscrete`
		  *                  type rather than this clause; they can be adapted for the required `this.Generalized` by
		  *                  passing them to an
		  *                  [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction aggregate function]],
		  *                  creating an [[net.noresttherein.oldsql.sql.AggregateSQL AggregateSQL]] in the result.
		  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		def having(condition :JoinedMappings[Generalized] => LocalBoolean[Generalized]) :F = {
			val grounded = SQLScribe.anchorLooseComponents(generalized)(condition(new JoinedMappings(generalized)))
			having(grounded)
		}

		/** Apply a filter condition to the last grouping expression in this clause. The condition is combined
		  * using `&&` with `this.condition` and becomes a part of `this.filter` representing the ''having'' clause
		  * of the SQL statement. It is equivalent to `this.having(mappings => condition(mappings.last))`.
		  * @param condition a function accepting the expression for the last relation in this clause and creating
		  *                  an additional SQL expression for the ''having'' clause.
		  * @return an `Extended` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of the preexisting `this.condition`
		  *         and the `LocalBoolean` returned by the passed filter function.
		  */
		def havingLast(condition :JoinedRelation[FromLast, LastMapping] => LocalBoolean[FromLast]) :F =
			having(SQLScribe.anchorLooseComponents(generalized)(condition(last)))

	}






	implicit def GeneralizedGroupByExtension
	             [C <: GroupByClause, F <: FromSome, G <: GroupByClause { type GeneralizedDiscrete = F }]
	             (self :C)(implicit cast :Conforms[C, G, GroupByClause { type GeneralizedDiscrete = F }])
			:GroupByClauseExtension[F, G] =
		new GroupByClauseExtension[F, G](self)


	/** Extension methods for any ''group by'' clause `G` which require its static type for the appropriate return type.
	  * @tparam F the actual ''from'' clause under grouping. All grouping expressions being the elements of
	  *           the ''group by'' clause `G` are based on this type.
	  * @tparam G this ''group by'' clause.
	  */
	class GroupByClauseExtension[F <: FromSome, G <: GroupByClause { type GeneralizedDiscrete = F }]
	                            (val thisClause :G)
		extends AnyVal
	{
		/** Adds another expression (column or columns) to the [[net.noresttherein.oldsql.sql.GroupByAll group by]]
		  * clause to this ''from'' clause.
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.GroupByClause.GroupingExpression GroupingExpression]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.GroupByClause.GroupingExpression$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - not anchored adapters of component mappings of any relation mapping present in the provided
		  *               [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]]:
		  *               [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent LooseComponent]]`[O, _, _]`
		  *               (and [[net.noresttherein.oldsql.sql.MappingSQL.LooseColumnComponent LooseColumnComponent]]`[O, _, _]`),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]`[F, _, _, _, _, O]`
		  *               and [[net.noresttherein.oldsql.sql.MappingSQL.ColumnComponentSQL ColumnComponentSQL]]`[F, _, _, _, _, O]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F =:= this.GeneralizedDiscrete` is the 'true', ''from'' clause grouped by this
		  *           ''group by'' clause, and `O` is its some supertype, with the origin relation of the component
		  *           expression being the first relation following an abstract type (typically `FromSome`).
		  * @param expr a function accepting the facade to this clause
		  *             [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]], which provides
		  *             accessors to the mappings for all relations in this clause, and which returns
		  *             either a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]] with a supertype
		  *             of this clause as its `Origin` type argument,
		  *             or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]] based on this clause which
		  *             will be used as the grouping expression. The expression may be
		  *             a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *             in which case all columns of the expression will be inlined in the ''group by'' clause
		  *             in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *             If the returned value is a a mapping `M[O] <: MappingAt[O]` or a component expression
		  *             for such a mapping - either a ready
		  *             [[net.noresttherein.oldsql.sql.MappingSQL.BaseComponentSQL BaseComponentSQL]] or unanchored
		  *             [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent! LooseComponent]] (implicitly
		  *             convertible from any component mapping) - then the return type of the method will be
		  *             `G `[[net.noresttherein.oldsql.sql.ByAll ByAll]]` M`, allowing selecting of any
		  *             of its components/columns, just as with components of tables joined using
		  *             the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *             mechanisms).
		  *             Note that the argument, `JoinedMappings[F]`, is parameterised not with this ''group by''
		  *             clause, but the discrete ''from'' clause underneath it.
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.ByAll ByAll]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.MappingSQL.BaseComponentSQL BaseComponentSQL]] or
		  *         [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent LooseComponent]] (including their column
		  *         subtypes), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.ByVal ByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.ByOne ByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def by[E](expr :JoinedMappings[F] => E)(implicit grouping :GroupingExpression[F, G, E]) :grouping.Result =
			grouping(thisClause)(expr(thisClause.from.generalized.mappings))

		/** Expands this ''group by'' clause with all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]]
		  * columns of the given component. The component becomes available to the ''having'' and ''select'' clauses
		  * as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] in the same way as the mappings
		  * of the joined tables. It can be accessed in the same way as those relations, through
		  * the [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]] facade to this clause.
		  * @param component a mapping for a component of one of the relations listed by this clause.
		  *                  It must be a [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`[S, O]`, where
		  *                  the `Origin` type `O` is a supertype of the `GeneralizedDiscrete` type of this clause,
		  *                  that is the `Generalized` supertype of the ''from'' clause to the left of the most recent
		  *                  [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]]. It must also start with
		  *                  `FromClause AndFrom T`, where `T` is the mapping type for the relation this component
		  *                  comes from.
		  * @param typeParams used to instantiate the necessary `Subject` and `Origin` types `S`, `O` of the argument mapping.
		  * @param shift implicit evidence with the number of relations listed in the `Origin` type.
		  * @param projection a casting type class for `M` which provides its necessary type constructor accepting
		  *                   an `Origin` type.
		  */ //todo: this currently is not picked over the overload with BaseComponentSQL for some reason
		def by[M <: Mapping, S, O <: FromClause]
		      (component :M)
		      (implicit typeParams :M <:< BaseMapping[S, O], belongs :F <:< O,
		                shift :TableCount[O, _ <: Numeral], projection :OriginProjection[M, S])
				:G ByAll projection.WithOrigin =
		{
			val relation = thisClause.fullTableStack(shift.tables).asRelationSQL
				.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			val expr = ComponentSQL(relation, projection[F](component))(projection.isomorphism)
			ByAll[G, projection.WithOrigin, projection.WithOrigin, S](thisClause, expr.groupingRelation)
		}

		/** Expands this ''group by'' clause with all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]]
		  * columns of the given component expression based on the
		  * [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type of this clause.
		  * The component becomes available to the ''having'' and ''select'' clauses
		  * as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] and can be accessed in the same way
		  * as the mappings of the joined tables.
		  */
		def by[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		      (component :BaseComponentSQL[F, M, _ >: F <: FromClause])
		      (implicit cast :InferSubject[G, ByAll, M, T, S]) :G ByAll M =
			ByAll(thisClause, component)

		/** Expands this ''group by'' clause with the given single column expression. */
		def by[V](column :GlobalColumn[F, V]) :G ByOne V =
			ByOne(thisClause, column)

		/** Expands this ''group by'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.SQLTerm.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.MappingSQL.BaseComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[net.noresttherein.oldsql.sql.ConversionSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.TupleSQL.ChainTuple tuples]] and
		  *     [[net.noresttherein.oldsql.sql.TupleSQL.IndexedChainTuple indexed tuples]].
		  */
		def by[V](expr :GlobalSQL[F, V]) :G ByVal V =
			ByVal(thisClause, expr)


		//the subselect methods are exact copy&paste from FromSomeExtension
		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.FromClause.NonEmptyFromMatrix.on on()]],
		  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseMatrix.where where()]]
		  * and [[net.noresttherein.oldsql.sql.FromClause.NonEmptyFromMatrix.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[G, Subselect, R, T, S]) :G Subselect R =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `FromClause`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.FromClause.NonEmptyFromMatrix.on on()]],
		  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseMatrix.where where()]]
		  * and [[net.noresttherein.oldsql.sql.FromClause.NonEmptyFromMatrix.whereLast whereLast()]] methods.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order). The clause cannot be empty to enforce that the `Subselect` join
		  *              is actually applied and that any relations joined later will be part of the new subselect
		  *              rather than the currently most deeply nested select.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: NonEmptyFrom](other :R) :other.JoinedWithSubselect[G] =
			other.joinedWithSubselect(thisClause)

	}



	/** Extension methods for any ''group by'' clause `F` which is not a subselect of another select. */
	implicit class OuterGroupingFromExtension[F <: OuterGroupingFrom](private val thisClause :F) extends AnyVal {

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
		  */ //the order of implicits is important to avoid a double definition
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F GroupParam (N ?: X)#T =
			GroupParam(thisClause, form ?: (name.value :N))

	}






	/** A pseudo relation used by the ''group by'' clauses [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] and
	  * [[net.noresttherein.oldsql.sql.ByAll ByAll]].
	  */
	class GroupingRelation[M[O] <: BaseMapping[S, O], S, A] private
	                      (val expr :SQLExpression[_, GlobalScope, S], template :M[A])
	                      (implicit projection :IsomorphicProjection[M, S, A])
		extends Relation[M]
	{
		override def apply[O] :M[O] = projection(template)

		override def sql :String = ??? //fixme: proper default SQL
	}



	object GroupingRelation {

		def apply[F <: FromClause, M[A] <: BaseMapping[S, A], S, O >: F <: FromClause]
		         (component :BaseComponentSQL[F, M, O])(implicit project :IsomorphicProjection[M, S, O])
				:GroupingRelation[M, S, O] =
			new GroupingRelation[M, S, O](component, component.mapping)

		def apply[F <: FromClause, S](expression :SQLExpression[F, GlobalScope, S]) :GroupingRelation[Group[S]#T, S, F] =
			new GroupingRelation[Group[S]#T, S, F](expression, ExpressionMapping(expression))(
				OriginProjection[Group[S]#T[Any], S].isomorphism[F]
			)

		def apply[F <: FromClause, S](expression :ColumnSQL[F, GlobalScope, S]) :GroupingRelation[Group[S]#C, S, F] =
			new GroupingRelation[Group[S]#C, S, F](expression, ExpressionColumnMapping(expression))(
				OriginProjection[Group[S]#C[Any], S].isomorphism[F]
			)
	}






	/** A type class for type `E`, allowing it to be used when creating ''group by'' clause elements
	  * using the [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.groupBy groupBy]] and
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] methods.
	  * @tparam F the actual ''from'' clause `<: `[[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]]
	  *           in the [[net.noresttherein.oldsql.sql.FromClause.Generalized generalized]] form,
	  *           which the grouping expression created by this type class is based on. It must be a supertype of
	  *           `G#GeneralizedDiscrete`.
	  * @tparam G the clause which is being expanded with a new grouping expression. It will become the left side
	  *           of the returned `this.Result`. It may be either the type `F` when adding a first grouping expression
	  *           using [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]], or
	  *           a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], in which case it will be expanded
	  *           with a [[net.noresttherein.oldsql.sql.ByAll ByAll]] class.
	  * @tparam E the type of the expression which can be turned into a grouping extension. Predefined implicit values
	  *           of this type exist in the companion
	  *           [[net.noresttherein.oldsql.sql.GroupByClause.GroupingExpression$ object]] for:
	  *             - [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]]`V, O]` subtypes,
	  *               where `O >: F <: FromClause`,
	  *             - [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent LooseComponent]]
	  *               (and [[net.noresttherein.oldsql.sql.MappingSQL.LooseColumnComponent LooseColumnComponent]]),
	  *             - [[net.noresttherein.oldsql.sql.MappingSQL.BaseComponentSQL BaseComponentSQL]] and
	  *               [[net.noresttherein.oldsql.sql.MappingSQL.BaseColumnComponentSQL BaseColumnComponentSQL]],
	  *             - [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, `[[net.noresttherein.oldsql.sql.SQLExpression.GlobalScope GlobalScope]]`, V]`
	  *               and [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, GlobalScope, V]`.
	  */
	@implicitNotFound("Type ${E} cannot be used as a grouping expression in ${G}:\n" +
	                  "Missing implicit GroupingExpression[${F}, ${G}, ${E}]")
	trait GroupingExpression[-F <: FromClause, -G <: FromClause, -E] {
		type Result <: GroupByClause
		def apply(clause :G)(expr :E) :Result
	}


	sealed abstract class ArbitraryGroupingExpressionImplicits {

		implicit def groupByVal[F <: FromSome, V]
				:GroupingExpression[F, F, GlobalSQL[F, V]] { type Result = F GroupByVal V } =
			new GroupingExpression[F, F, GlobalSQL[F, V]] {
				override type Result = F GroupByVal V

				override def apply(clause :F)(expr :GlobalSQL[F, V]) = GroupByVal(clause, expr)
			}

		implicit def byVal[F <: FromSome, G <: GeneralizedGroupingOf[F], V]
				:GroupingExpression[F, G, GlobalSQL[F, V]] { type Result = G ByVal V } =
			new GroupingExpression[F, G, GlobalSQL[F, V]] {
				override type Result = G ByVal V

				override def apply(clause :G)(expr :GlobalSQL[F, V]) = ByVal(clause, expr)
			}
	}


	sealed abstract class GroupingColumnExpressionImplicits extends ArbitraryGroupingExpressionImplicits {

		implicit def groupByOne[F <: FromSome, V]
				:GroupingExpression[F, F, ColumnSQL[F, GlobalScope, V]] { type Result = F GroupByOne V } =
			new GroupingExpression[F, F, ColumnSQL[F, GlobalScope, V]] {
				override type Result = F GroupByOne V

				override def apply(clause :F)(expr :ColumnSQL[F, GlobalScope, V]) =
					GroupByOne(clause, expr)
			}

		implicit def byOne[F <: FromSome, G <: GeneralizedGroupingOf[F], V]
				:GroupingExpression[F, G, ColumnSQL[F, GlobalScope, V]] { type Result = G ByOne V } =
			new GroupingExpression[F, G, ColumnSQL[F, GlobalScope, V]] {
				override type Result = G ByOne V

				override def apply(clause :G)(expr :ColumnSQL[F, GlobalScope, V]) = ByOne(clause, expr)
			}

	}


	object GroupingExpression extends GroupingColumnExpressionImplicits {

		implicit def groupByMapping[F <: FromSome, C <: Mapping, S, O <: FromClause]
		                           (implicit subject :C <:< BaseMapping[S, O], origin :F <:< O,
		                                     shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
				:GroupingExpression[F, F, C] { type Result = F GroupByAll projection.WithOrigin } =
			new GroupingExpression[F, F, C] {
				override type Result = F GroupByAll projection.WithOrigin

				override def apply(clause :F)(expr :C) = { //clause groupBy expr <- requires Generalized <: O
					val relation = clause.fullTableStack(shift.tables).asRelationSQL
					                     .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
					val component = ComponentSQL(relation, projection[F](expr))(projection.isomorphism)
					GroupByAll[F, projection.WithOrigin, projection.WithOrigin, S](clause, component.groupingRelation)
				}
			}

		implicit def groupByFree[F <: FromSome, U >: F <: FromClause, M[O] <: BaseMapping[S, O], S]
				:GroupingExpression[F, F, LooseComponent[U, M, S]] { type Result = F GroupByAll M } =
			new GroupingExpression[F, F, LooseComponent[U, M, S]] {
				override type Result = F GroupByAll M

				override def apply(clause :F)(expr :LooseComponent[U, M, S]) =
					GroupByAll[F, M, S](clause, expr)
			}

		implicit def groupByComponent[F <: FromSome, M[O] <: BaseMapping[S, O], S]
				:GroupingExpression[F, F, BaseComponentSQL[F, M, _ >: F <: FromClause]] { type Result = F GroupByAll M } =
			new GroupingExpression[F, F, BaseComponentSQL[F, M, _ >: F <: FromClause]] {
				override type Result = F GroupByAll M

				override def apply(clause :F)(expr :BaseComponentSQL[F, M, _ >: F <: FromClause]) =
					GroupByAll(clause, expr)
			}



		implicit def byMapping[F <: FromSome, G <: GroupByClause { type GeneralizedDiscrete = F },
		                       C <: Mapping, S, O <: FromClause]
		                      (implicit typeParams :C <:< BaseMapping[S, O], fromDiscrete :F <:< O,
		                                shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
				:GroupingExpression[F, G, C] { type Result = G ByAll projection.WithOrigin } =
			new GroupingExpression[F, G, C] {
				override type Result = G ByAll projection.WithOrigin

				override def apply(clause :G)(expr :C) = clause by[C, S, O] expr
			}

		implicit def byFree[F <: FromSome, G <: GeneralizedGroupingOf[F], M[O] <: BaseMapping[S, O], S]
				:GroupingExpression[F, G, LooseComponent[F, M, S]] { type Result = G ByAll M } =
			new GroupingExpression[F, G, LooseComponent[F, M, S]] {
				override type Result = G ByAll M

				override def apply(clause :G)(expr :LooseComponent[F, M, S]) =
					ByAll[F, G, M, S](clause, expr)
			}

		implicit def byComponent[F <: FromSome, G <: GeneralizedGroupingOf[F], M[O] <: BaseMapping[S, O], S]
				:GroupingExpression[F, G, BaseComponentSQL[F, M, _ >: F <: FromClause]] { type Result = G ByAll M } =
			new GroupingExpression[F, G, BaseComponentSQL[F, M, _ >: F <: FromClause]] {
				override type Result = G ByAll M

				override def apply(clause :G)(expr :BaseComponentSQL[F, M, _ >: F <: FromClause]) =
					ByAll(clause, expr)
			}

	}

}
