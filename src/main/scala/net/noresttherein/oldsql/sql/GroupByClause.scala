package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.{PseudoRelation, Table}
import net.noresttherein.oldsql.schema.Relation.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject.{InferAliasedSubject, InferSubject}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroundFrom, JoinedMappings, NonEmptyFrom, NonEmptyFromTemplate, PartOf, PrefixOf, RowProductTemplate}
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate
import net.noresttherein.oldsql.sql.GroupParam.ByParam
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.UnboundParam.{NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, RelationSQL, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.mechanics.{GroupingExpression, RowProductMatcher, TableCount}






/** A base type for all ''from'' clauses of SQL ''selects'' which make use of aggregate functions.
  * This encompasses both queries with a ''group by'' clause
  * - [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] - and a special
  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] implementation used for ''selects'' which feature
  * an aggregate function in their ''select'' clause, aggregating all rows into a single result.
  */
sealed trait AggregateClause extends RowProduct with RowProductTemplate[AggregateClause] { thisClause =>

	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override type Generalized >: Dealiased <: AggregateClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: AggregateClause {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: AggregateClause {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}



	override type FilterNext[E[+L <: FromSome] <: L Expanded N, S <: RowProduct Expanded N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	protected override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		throw new UnsupportedOperationException(s"GroupByClause.filterNext (on $this)")



	/** Type constructor for this clause. It replaces
	  * the [[net.noresttherein.oldsql.sql.AggregateClause.Grouped Grouped]] portion of this type
	  * (that is, all relations which are aggregated and unavailable individually to SQL expressions based on
	  * this clause) with `U`.
	  */
	type Grouping[+U <: FromSome] <: AggregateClause

	/** The ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * This is the ''generalized'' supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete this.Discrete]].
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Generalized`;
	  * for [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */
	type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The self typ of the ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Self`;
	  * for [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */
	type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}

	/** The [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] forming the prefix of this clause which contains
	  * all the aggregated relations in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * These relations are ''not'' available to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on this clause.
	  */
	override val fromClause :Discrete

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.RowProduct.Implicit ''implicit'']] prefix with
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]. Equals `fromClause.Explicit`.
	  */
	type GeneralizedGrouped = fromClause.Explicit

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]] prefix with
	  * the upper bound for the left side of the [[net.noresttherein.oldsql.sql.AndFrom join]] between the
	  * outer and inner sections. Equals `fromClause.Inner`.
	  */
	type Grouped = fromClause.Inner




	protected override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.aggregateClause(this)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateClause]

}







/** A special [[net.noresttherein.oldsql.sql.FromSome FromSome]] adapter used when selecting
  * an aggregate [[net.noresttherein.oldsql.sql.ast.AggregateSQL! expression]], coalescing all rows from the adapted
  * clause into a single result. Its existence is needed as such aggregate functions are - aside from ''selects''
  * with a ''group by'' clause - allowed only in the ''select'' clause, and not the ''where'' clause.
  * It is considered a part of the implementors' interface, exposed for applications
  * which add their own aggregate [[net.noresttherein.oldsql.sql.AggregateFunction functions]] or
  * SQL dialects (requiring access to the whole [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] hierarchy).
  * Applications which satisfy themselves with the standard aggregate functions defined in
  * [[net.noresttherein.oldsql.sql.ast.AggregateSQL$ AggregateSQL]] should have no need for this class, as appropriate
  * 'shortcut' methods are defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]
  * implicit extension class for [[net.noresttherein.oldsql.sql.FromSome non-empty, ungrouped]] clauses.
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
	override type Last[E <: RowProduct] = Nothing
	override type FromLast = RowProduct

	/** Throws an `NoSuchElementException`. */
	override def last :Nothing = throw new NoSuchElementException(s"($this).last")

	/** Throws an `NoSuchElementException`. */
	override def lastAsIn[E <: FromSome](implicit expansion :FromLast PrefixOf E) :Nothing = last

	override type Bound = FromSome
	override type Generalized = Aggregated[clause.Generalized]
	override type Dealiased = Aggregated[clause.Dealiased]
	override type Self = Aggregated[clause.Self]
	override type Copy = Aggregated[clause.Copy]


	override def withClause[C <: FromSome](from :C) :Aggregated[C] = Aggregated(from)


	/** Always returns [[net.noresttherein.oldsql.sql.ast.SQLTerm.True True]]. For the actual, ungrouped ''where'' clause
	  * use `this.clause.filter`.
	  */
	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :LocalBoolean[E] = True

	/** Throws an `UnsupportedOperationException`. */
	override def filtered[S >: GlobalScope <: GlobalScope](condition :SQLBoolean[Generalized, S]) :Nothing =
		throw new UnsupportedOperationException(s"($this).where($condition)")


	override type AppliedParam = Aggregated[clause.AppliedParam]
	override type Paramless = Aggregated[clause.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :LastParam) :AppliedParam = withClause(clause.bind(param))
	override def bind(params :Params) :Paramless = withClause(clause.bind(params))

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))



	override def isEmpty :Boolean = outer.isEmpty
	override def fullSize :Int = outer.fullSize
	override def size :Int = 0


	override type FullRow = clause.OuterRow

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		clause.outerRow(target)(explicitSpan expand expansion)

	override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:LazyList[RelationSQL.AnyIn[E]] =
		outer.fullTableStack(target)(explicitSpan.expand(expansion))


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		Aggregated[clause.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withClause(clause.joinedWith(prefix, firstJoin))

	override type JoinedWithSubselect[+P <:  NonEmptyFrom] = Aggregated[clause.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withClause(clause.joinedWithSubselect(prefix))

	override def appendedTo[P <: FromClause](prefix :P) :Aggregated[clause.JoinedWith[P, NonParam]] =
		withClause(clause.appendedTo(prefix))



	override type Explicit = Aggregated[clause.Explicit]
	override type Inner = Aggregated[clause.Inner]
	override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = clause.DefineBase[I]

	override def base :Base = clause.base

	override type Row = @~

	/** Returns an empty chain expression. */
	override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:ChainTuple[E, GlobalScope, @~] =
		EmptyChain

	/** Returns an empty list. */
	override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty

	override type OuterRow = clause.OuterRow

	override def outerRow[E <: RowProduct](target :E)(implicit expansion :Implicit ExpandedBy E)
			:ChainTuple[E, GlobalScope, OuterRow] =
		clause.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = Aggregated[clause.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
			:Aggregated[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		withClause(clause.asSubselectOf(newOuter))



	override type Grouping[+D <: FromSome] = Aggregated[D]
	override type GeneralizedDiscrete = clause.Generalized
	override type Discrete = clause.Self

	//these could probably be made to work, but I'm lazy and this class shouldn't be used for much else than a select call
	override type FromRelation[T[O] <: MappingAt[O]] = Nothing

	/** Throws `UnsupportedOperationException`. */
	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Table[M])
	                 (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:Nothing =
		throw new UnsupportedOperationException(s"($this).from($first)")

	/** Throws `UnsupportedOperationException`. */
	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
	                 (first :StaticTable[A, M])
	                 (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
			:Nothing =
		throw new UnsupportedOperationException(s"($this).from($first)")

	override type FromSubselect[+S <: NonEmptyFrom] = Nothing

	/** Throws `UnsupportedOperationException`. */
	override def from[S <: NonEmptyFrom with GroundFrom](subselect :S) :Nothing =
		throw new UnsupportedOperationException(s"($this).from($subselect)")

	/** Throws `UnsupportedOperationException`. */
	override def fromSubselect[E <: NonEmptyFrom]
	                          (subselect :E)(implicit expansion :subselect.Implicit ExpandedBy Generalized) :Nothing =
		throw new UnsupportedOperationException(s"($this).fromSubselect($subselect)")



	protected override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.aggregated(this)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Aggregated.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :Aggregated.* if other canEqual this => other.clause == clause
		case _ => false
	}

	override def hashCode :Int = clause.hashCode



	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause =
		throw new UnsupportedOperationException


	override def toString :String= clause.toString + " aggregated"

}






object Aggregated {

	def apply[F <: FromSome](discrete :F) :Aggregated[F] =
		new Aggregated[discrete.type] {
			override val clause :discrete.type = discrete
			override val fromClause = discrete.self
			override val outer = clause.outer
		}


	def unapply[F <: FromSome](from :Aggregated[F]) :Some[F] = Some(from.clause)

	def unapply[F <: RowProduct](from :DecoratedFrom[F]) :Option[F] = from match {
		case _ :Aggregated[_] => Some(from.clause)
		case _ => None
	}

	def unapply(from :RowProduct) :Option[FromSome] = from match {
		case aggro :Aggregated.* => Some(aggro.clause)
		case _ => None
	}



	type * = Aggregated[_ <: FromSome]

}







/** A base type for all ''from'' clauses with a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupBy]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupByClause extends NonEmptyFrom with AggregateClause with GroupByClauseTemplate[GroupByClause, GroupByClause] {
	thisClause =>

	override type Last[O <: RowProduct] = JoinedRelation[O, LastMapping]
	override type FromLast >: Generalized <: GroupByClause
	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override type Generalized >: Dealiased <: GroupByClause {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: GroupByClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: GroupByClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type AppliedParam <: GroupByClause
	override type Paramless <: BoundParamless //because GroupParam requires a GroupByClause on the left.
	override type BoundParamless = GroupByClause { type Params = @~ }

	override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] =
		last.asIn[E]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this 'join'.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct#filter]]
	  */
	def condition :LocalBoolean[Generalized]


	/** Type constructor for the whole ''group by'' clause - that is all 'joins' appearing after the ungrouped,
	  * 'true' ''from'' clause `U`, starting with the [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] compound.
	  */
	override type Grouping[+U <: FromSome] <: GroupByClause

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */ //overriden for docs only
	override type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */ //overriden for docs only
	override type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}


	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */ //'from' conflicts with the 'from' method for creating subselects. 'grouped' is an extension method returning JoinedMappings[Discrete]
	override val fromClause :Discrete



	protected override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.groupByClause(this)


	private[sql] def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException

}






object GroupByClause {

	type Group[S] = { type T[O] = BaseMapping[S, O]; type C[O] = ColumnMapping[S, O] }


	def unapply(f :RowProduct) :Option[FromClause] = f match {
		case group :GroupByClause => Some(group.fromClause)
		case _ => None
	 }



	trait GroupByClauseTemplate[+U <: GroupByClause, +F <: U]
		extends NonEmptyFromTemplate[U, F]
	{ thisClause :F with GroupByClauseTemplate[U, F] =>

		override type Copy <: F {
			type LastMapping[O] = thisClause.LastMapping[O]
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
			type Dealiased = thisClause.Dealiased
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row = thisClause.Row
			type OuterRow = thisClause.OuterRow
		}


		override def filtered[S >: LocalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :Copy //now accepts LocalSQL


		override type JoinFilter = Nothing

		override def filtered(condition :Nothing) :Nothing =
			throw new UnsupportedOperationException(s"$this.on")

		/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]].
		  * This means that the applied filter condition will become the ''having'' clause of the generated select,
		  * rather than the ''where'' clause.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def where(condition :GlobalBoolean[Generalized]) :F = having(condition)

		/** A straightforward delegate to `this.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]].
		  * This means that the applied filter condition will become the ''having'' clause of the generated select,
		  * rather than the ''where'' clause.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def where(condition :JoinedMappings[F] => GlobalBoolean[Generalized]) :F =
			having(condition)

		/** Creates a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  * instance's filter and the given `SQLBoolean`. The filter becomes the part of the ''having'' clause
		  * of the SQL ''select'' based on this clause or some its expansion. This works analogously to the
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method with the same signature on ungrouped clauses.
		  * @see [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having]]
		  */
		override def having(condition :LocalBoolean[Generalized]) :F =
			filtered(condition.anchor(generalized))

		/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
		  * and becomes a part of `this.filter` representing the ''having'' clause of the SQL statement.
		  * This works analogously to the [[net.noresttherein.oldsql.sql.RowProduct.where where]] method
		  * with the same signature on ungrouped clauses.
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  *                  The provided `JoinedRelations` do ''not'' include the relations from the actual ''from''
		  *                  clause (i.e. listed by the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]]),
		  *                  as the values for their individual rows are unavailable. They can however be accessed
		  *                  by [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.grouped grouped]] method.
		  *                  The expressions for these relations will be based however on the `GeneralizedDiscrete`
		  *                  type rather than this clause; they can be adapted for the required `this.Generalized` by
		  *                  passing them to an
		  *                  [[net.noresttherein.oldsql.sql.AggregateFunction aggregate function]],
		  *                  creating an [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] in the result.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def having(condition :JoinedMappings[F] => LocalBoolean[Generalized]) :F =
			having(condition(this.mappings))

		/** Apply a filter condition to the last grouping expression in this clause. The condition is combined
		  * using `&&` with `this.condition` and becomes a part of `this.filter` representing the ''having'' clause
		  * of the SQL statement. It is equivalent to `this.having(mappings => condition(mappings.last))`.
		  * @param condition a function accepting the expression for the last relation in this clause and creating
		  *                  an additional SQL expression for the ''having'' clause.
		  * @return an `Expanded` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of the preexisting `this.condition`
		  *         and the `LocalBoolean` returned by the passed filter function.
		  */
		override def havingLast(condition :JoinedRelation[FromLast, LastMapping] => LocalBoolean[FromLast]) :F =
			having(condition(last))

	}






	/** Extension methods for any ''group by'' clause `G` which require its static type for the appropriate return type.
	  * @tparam G this ''group by'' clause.
	  */  //Can't be AnyVal for now as it uses PDTs
	implicit class GroupByClauseExtension[G <: GroupByClause](val thisClause :G) extends AnyVal {
		//this needs to be an extension rather than a NonEmptyFromTemplate subtype because of As:
		// GroupByClause As A would inherit the generic template trait, not parameterized with F As A
		import thisClause.fromClause.{FromLast, LastMapping}
		import thisClause.{Discrete => F, GeneralizedDiscrete => U}

		/** Adds another expression (column or columns) to the [[net.noresttherein.oldsql.sql.GroupBy group by]]
		  * clause to this ''from'' clause.
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression GroupingExpression]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]`[F, _]`
		  *               and [[net.noresttherein.oldsql.sql.ast.MappingSQL.ColumnComponentSQL ColumnComponentSQL]]`[F, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F =:= this.GeneralizedDiscrete` is the 'true', ''from'' clause grouped by this
		  *           ''group by'' clause, and `O` is its some supertype, with the origin relation of the component
		  *           expression being the first relation following an abstract type (typically `FromSome`).
		  * @param expr     a function accepting the facade to the grouped clause (the actual ''from'' clause without
		  *                 the ''group by'' expansions)
		  *                 [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]], which provides
		  *                 accessors to the mappings for all relations in the scope of the group by clause
		  *                 (that is all or all since the last [[net.noresttherein.oldsql.sql.Subselect Subselect]],
		  *                 and which returns either a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *                 with a supertype of this clause as its `Origin` type argument,
		  *                 or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]] based on this clause which
		  *                 will be used as the grouping expression. The expression may be
		  *                 a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *                 in which case all columns of the expression will be inlined in the ''group by'' clause
		  *                 in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *                 If the returned value is a a mapping `M[O] <: MappingAt[O]` or
		  *                 a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component expression]]
		  *                 for such a mapping, then the return type of the method will be
		  *                 `G `[[net.noresttherein.oldsql.sql.By By]]` M`, allowing selecting of any
		  *                 of its components/columns, just as with components of tables joined using
		  *                 the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *                 mechanisms).
		  *                 Note that the argument, `JoinedMappings[F]`, is parameterised not with this ''group by''
		  *                 clause, but the discrete ''from'' clause underneath it.
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.By By]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.ByVal ByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.ByOne ByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def by[E](expr :JoinedMappings[F] => E)(implicit grouping :GroupingExpression[U, G, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.fromClause.mappings))

		/** Adds another expression (column or columns) to the [[net.noresttherein.oldsql.sql.GroupBy group by]]
		  * clause to this ''from'' clause. The expression is based on the last
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression GroupingExpression]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.GroupingExpression$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.MappingSQL.TypedComponentSQL TypedComponentSQL]]`[F, _, _, _, _, O]`
		  *               and [[net.noresttherein.oldsql.sql.ast.MappingSQL.TypedColumnComponentSQL TypedColumnComponentSQL]]`[F, _, _, _, _, O]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F =:= this.GeneralizedDiscrete` is the 'true', ''from'' clause grouped by this
		  *           ''group by'' clause, and `O` is its some supertype, with the origin relation of the component
		  *           expression being the first relation following an abstract type (typically `FromSome`).
		  * @param expr a function accepting the last [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation relation]]
		  *             of the grouped ''from clause'' clause (that is, the one directly preceding
		  *             [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]), and which returns either
		  *             a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype of this clause
		  *             as its `Origin` type argument, or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]]
		  *             based on this clause which will be used as the grouping expression. The expression may be
		  *             a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *             in which case all columns of the expression will be inlined in the ''group by'' clause
		  *             in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *             If the returned value is a a mapping `M[O] <: MappingAt[O]` or a
		  *             [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL component expression]]
		  *             for such a mapping - then the return type of the method will be
		  *             `F `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` M`, allowing selecting of any
		  *             of its components/columns, just as with components of tables joined using
		  *             the [[net.noresttherein.oldsql.sql.Join Join]] classes (and through the same
		  *             mechanisms).
		  * @param grouping a type class responsible for creating the returned ''group by'' clause, which defines
		  *                 the return type based on the type of the expression returned by the function passed
		  *                 as the first argument. See the `returns` section for a listing.
		  * @return a [[net.noresttherein.oldsql.sql.By By]] instance using this clause as its left side.
		  *         The mapping type on the right side will be the mapping for the expression `E` returned
		  *         by the passed function: if it is a
		  *         [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], it is used directly after anchoring
		  *         to the relation based on its `Origin` type. In case of
		  *         [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]] (including its column
		  *         subtypes), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.ByVal ByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.ByOne ByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def byLast[E](expr :JoinedRelation[FromLast, LastMapping] => E)
		             (implicit grouping :GroupingExpression[U, G, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.fromClause.last))

		/** Expands this ''group by'' clause with all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]]
		  * columns of the given component. The component becomes available to the ''having'' and ''select'' clauses
		  * as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] in the same way as the mappings
		  * of the joined tables. It can be accessed in the same way as those relations, through
		  * the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] facade to this clause.
		  * @param component a mapping for a component of one of the relations listed by this clause.
		  *                  It must be a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[S, O]`, where
		  *                  the `Origin` type `O` is a supertype of the `GeneralizedDiscrete` type of this clause,
		  *                  that is the `Generalized` supertype of the ''from'' clause to the left of the most recent
		  *                  [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]. It must also start with
		  *                  `RowProduct AndFrom T`, where `T` is the mapping type for the relation this component
		  *                  comes from.
		  * @param origin used to instantiate the necessary `Origin` type `O` of the argument mapping.
		  * @param shift implicit evidence with the number of relations listed in the `Origin` type.
		  * @param projection a casting type class for `M` which provides its necessary type constructor accepting
		  *                   an `Origin` type.
		  */ //todo: this currently is not picked over the overload with ComponentSQL for some reason
		def by[C <: Mapping, S, O <: RowProduct]
		      (component :C)
		      (implicit origin :C <:< MappingAt[O], belongs :U <:< O,
		                shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
				:G By projection.WithOrigin =
		{
			val relation = thisClause.fromClause.fullTableStack(shift.offset).toRelationSQL
				.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
			val expr = TypedComponentSQL(relation, projection[F](component))(projection.isomorphism)
			By[G, projection.WithOrigin, projection.WithOrigin, S](thisClause, expr.groupingRelation)
		}

		/** Expands this ''group by'' clause with all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]]
		  * columns of the given component expression based on the
		  * [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type of this clause.
		  * The component becomes available to the ''having'' and ''select'' clauses
		  * as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] and can be accessed in the same way
		  * as the mappings of the joined tables.
		  */
		def by[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		      (component :ComponentSQL[U, M])(implicit cast :InferSubject[G, By, M, T, S]) :G By M =
			By(thisClause, component.groupingRelation)

		/** Expands this ''group by'' clause with the given single column expression. */
		def by[V](column :GlobalColumn[U, V]) :G ByOne V =
			ByOne[U, thisClause.type, V](thisClause, column)

		/** Expands this ''group by'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[net.noresttherein.oldsql.sql.ast.ConversionSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.SQLExpression.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuples]] and
		  *     [[ast.TupleSQL.ListingSQL indexed tuples]].
		  */
		def by[V](expr :GlobalSQL[U, V]) :G ByVal V =
			ByVal[U, thisClause.type, V](thisClause, expr)


		//the subselect methods are exact copy&paste from FromSomeExtension
		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the table given as a `Table` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Table[R])
		                     (implicit cast :InferSubject[G, Subselect, R, T, S]) :G Subselect R =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The `String` literal type with name of the table, taken from the argument's type, is used for
		  * the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause added to the joined table.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Table` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :StaticTable[N, R])
		                     (implicit cast :InferAliasedSubject[G, Subselect, R, T, S, N]) :G Subselect R As N =
			Subselect(thisClause, table)(cast)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
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
	implicit class TopGroupByClauseExtension[F <: TopGroupByClause](private val thisClause :F) extends AnyVal {

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm] :F ByParam X = GroupParam(thisClause, ParamRelation[X]())

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The artificial pseudo relation [[net.noresttherein.oldsql.sql.UnboundParam.ParamRelation ParamRelation]]
		  * is best obtained using the [[net.noresttherein.oldsql.sql.?: ?:]] factory method from package `sql`:
		  * {{{
		  *     From(Critters) groupBy (_.last.species) as "species" param ?:[String] on (_.name === _) having {
		  *         t => t("species") === t(-1)
		  *     }
		  * }}}
		  * @param relation a pseudo relation dedicated to `UnboundParam` joins, representing a future parameter
		  *                 of type `X`, which can be later accessed as any other mappings.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X](relation :ParamRelation[X]) :F ByParam X = GroupParam(thisClause, relation)

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[X :SQLForm](name :String) :F ByParam X = GroupParam(thisClause, ParamRelation[X](name))

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */ //the order of implicits is important to avoid a double definition
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F ByParam X As N =
			GroupParam(thisClause, form ?: (name.value :N))

		/** Creates a parameterized `RowProduct` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  *
		  * The recommended ways of creating these relations are the
		  * [[net.noresttherein.oldsql.sql.?: ?:]] factory method from package `sql` and an extension method
		  * for `String` literals with the same name: [[net.noresttherein.oldsql.sql.method_?:.?: ?:]].
		  * {{{
		  *     def parameterize1[N <: Label :ValueOf, X :SQLForm, F <: TopGroupByClause](from :F) :F WithParam X As N =
		  *         from param ?:[N, X]
		  *
		  *     From(Characters) groupBy (_.last.characterClass) as "class" param "selectedClass".?:[String] having {
		  *         t => t("class") === (t ? "selectedClass")
		  *     }
		  * }}}
		  * Importing the `?:` symbol imports at the same time the factory methods for named and unnamed parameter
		  * relations, the implicit conversion enriching `String` literals, and the container type
		  * with the type constructor for the type of `Mapping` used by `JoinParam`.
		  * @param relation a pseudo relation dedicated to `UnboundParam` joins, representing a future parameter
		  *                 of type `X`, with its similarly synthetic `Mapping` being labeled with `N`,
		  *                 used at the same time for the suggested parameter name.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[N <: Label, X](relation :NamedParamRelation[N, X]) :F ByParam X As N =
			GroupParam(thisClause, relation)

	}






	/** A `RowProduct` of a top level, independent ''select'' with a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (is not a ''from'' clause of a subselect
	  * of some other select). In order to conform naturally (rather than by refinement) to `OuterDiscreteForm`,
	  * the clause must be ''complete'', that is its static type must start with either
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] - rather than
	  * a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to the level of
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is a 'grouping' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopFrom TopFrom]].
	  * See [[net.noresttherein.oldsql.sql.FromClause.TopFromClause TopFromClause]] for a non grouping variant
	  * with a [[net.noresttherein.oldsql.sql.FromClause FromClause]] upper bound. An `TopGroupByClause`
	  * can still contain (unbound) [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] parameters;
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroundGroupByClause GroundGroupByClause]] is the specialization
	  * of this type without any `GroupParam` 'joins'.
	  * @see [[net.noresttherein.oldsql.sql.FromClause FromClause]]
	  */
	type TopGroupByClause = GroupByClause {
		type Implicit = RowProduct
	}

	/** A `RowProduct` with a ''group by'' clause but without any [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * or [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * synthetic joins. Representing a single ''from'' clause (and not one of a nested subselect), only such clauses
	  * can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct.GeneralizedFrom GeneralizedFrom]], but there is no actual subtype
	  * relationship between the two in the type system. A `GroundGroupByClause` will however always by a subtype of
	  * [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClause TopGroupByClause]], which groups all outer clauses,
	  * including those with unbound parameters,
	  * and [[net.noresttherein.oldsql.sql.RowProduct.ParamlessFrom ParamlessFrom]].
	  * For convenience, this type has two sibling types with narrowed upper bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]],
	  * [[net.noresttherein.oldsql.sql.FromSome.GroundFromSome GroundFromSome]] and
	  */
	type GroundGroupByClause = GroupByClause {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}





	/** A pseudo relation used by the ''group by'' clauses [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and
	  * [[net.noresttherein.oldsql.sql.By By]].
	  * '''Warning:''' this class essentially adapts any SQL expression, regardless of dependencies, to an interface
	  * which can be used by `From` and normal `Join`s without causing compile a error.
	  */ //todo: this shouldn't be public. Problem: it's exposed by GroupBy/By and TypedComponentSQL anyway :(
	class GroupingRelation[M[O] <: BaseMapping[S, O], S, A] private
	                      (val expr :SQLExpression[_, GlobalScope, S], template :M[A])
	                      (implicit projection :IsomorphicProjection[M, S, A])
		extends PseudoRelation[M]
	{
		override def apply[O] :M[O] = projection(template)
		override def export[O] :M[O] = projection(template)

		override def sql :String = template.selectedByDefault.view.map(_.name).mkString(", ")
	}



	object GroupingRelation {

		def apply[F <: RowProduct, M[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
		         (component :ComponentSQL[F, M] { type Origin = O })(implicit project :IsomorphicProjection[M, S, O])
				:GroupingRelation[M, S, O] =
			new GroupingRelation[M, S, O](component, component.mapping)

		def apply[F <: RowProduct, S](expression :SQLExpression[F, GlobalScope, S]) :GroupingRelation[Group[S]#T, S, F] =
			new GroupingRelation[Group[S]#T, S, F](expression, SQLMapping(expression))(
				OriginProjection[Group[S]#T[Any], S].isomorphism[F]
			)

		def apply[F <: RowProduct, S](expression :ColumnSQL[F, GlobalScope, S]) :GroupingRelation[Group[S]#C, S, F] =
			new GroupingRelation[Group[S]#C, S, F](expression, ColumnSQLMapping(expression))(
				OriginProjection[Group[S]#C[Any], S].isomorphism[F]
			)
	}

}
