package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, GroupingOfGeneralized, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.MappingSQL.{ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.GroupByAll.AndByAll
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseMatrix
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}






/** A [[net.noresttherein.oldsql.sql.FromClause FromClause]] extension introducing a ''group by'' clause.
  * The appended [[net.noresttherein.oldsql.schema.Relation relation]] is not a database table, view, or select as with
  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F#Generalized, GlobalScope S]`, where type `S`
  * is the subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` of the right type parameter.
  * The latter may either directly come from a component of any of the relations from the ''from'' clause `F`
  * of the left side - ranging from a whole entity to a single column - or be another synthetic adapter,
  * this type for an arbitrary SQL expression. In either case, all
  * [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns of the mapping are used in their order;
  * for non-components, the expression is recursively reduced to individual
  * [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions which comprise the column list.
  *
  * As grouping by composite SQL expressions is a common use case, two type aliases exist to support this feature:
  * [[net.noresttherein.oldsql.sql.GroupByVal GroupByVal]]`[F, V]`
  * and [[net.noresttherein.oldsql.sql.GroupByOne GroupByOne]]`[F, V]` which take only the subject type of the mapping
  * as the `V` parameter, using a generic [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]] (for the former)
  * or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] (for the latter) as the adapter used
  * in the grouping relation of this instance.
  *
  * This 'join' works differently than other [[net.noresttherein.oldsql.sql.Compound Compound]] subtypes in that
  * it doesn't [[net.noresttherein.oldsql.sql.Extended extend]] the clause on its left side: all relations
  * from the ''from'' clause `F` since the last occurrence of [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) become 'shadowed'
  * and unavailable through the [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateSQL.Count Count]] or [[net.noresttherein.oldsql.sql.AggregateSQL.Avg Avg]]).
  * Relations preceding the last `Subselect` are the relations of outer selects, not direct members of this select's
  * ''from'' clause, and are available individually as normal. Adding additional grouping expressions to this clause
  * in the form of the [[net.noresttherein.oldsql.sql.ByAll ByAll]] sister type is done in a similar fashion to creating
  * this clause: by an extension method [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]],
  * which accepts a function from `JoinedMappings[F#Generalized]` (obtained from `this.mappings.grouped`),
  * where `F` is the left type parameter of ''this'' trait, to an `SQLExpression[F#Generalized, GlobalScope, V]`.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnSQLAggregateMethods]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction AggregateFunction]].
  * In both cases, the `FromClause` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.GroupByAll$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.GroupByVal$ GroupByVal]] and [[net.noresttherein.oldsql.sql.GroupByOne$ GroupByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.groupBy groupBy]] added to any
  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome non-empty, discrete]] clause.
  * @author Marcin Mościcki
  */
trait GroupByAll[+F <: FromSome, M[A] <: MappingAt[A]]
	extends Compound[F, M] with GroupByClause with GroupByClauseMatrix[F GroupByAll M, F GroupByAll M]
{ thisClause =>

	override type FromLast = FromSome GroupByAll M
	override type Generalized = left.Generalized GroupByAll M
	override type Dealiased = left.Self GroupByAll M
	override type Self <: left.Self GroupByAll M

	type DealiasedLeft[+L <: FromSome] = L GroupByAll M
	type WithLeft[+L <: FromSome] <: L GroupByAll M
	type LeftBound = FromSome

	private[sql] def unsafeLeftSwap[L <: FromSome]
	                               (left :L)(condition :LocalBoolean[left.Generalized GroupByAll M]) :WithLeft[L] =
		withLeft(left)(condition)

	protected def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupByAll M]) :WithLeft[L]

	protected override def narrow :WithLeft[left.type]

	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''having'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `having` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	def withCondition(filter :LocalBoolean[Generalized]) :Copy

	override def filtered[S >: LocalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :Copy =
		withCondition(condition && filter)

	override def filter :LocalBoolean[Generalized] = condition

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] =
		condition.basedOn(target)


	override type Params = left.Params
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = unsafeLeftSwap[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		unsafeLeftSwap[l.type](l)(substitute(condition))
	}

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))

	override def isSubselectParameterized :Boolean = left.isSubselectParameterized


	override def fullSize :Int = outer.fullSize + 1

	override type FullRow = OuterRow ~ last.Subject

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.outerRow(target)(explicitSpan.extend(extension)) ~ last.extend(target)


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withLeft(left.joinedWithSubselect(prefix))(condition)

	override def appendedTo[P <: DiscreteFrom](prefix :P) :JoinedWith[P, AndFrom] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Grouping[+U <: FromSome] = U GroupByAll M
	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupByAll M
	override type Inner = left.Inner GroupByAll M
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Base = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
	override type DefineBase[+I <: FromClause] = left.DefineBase[I]

	override def base :Base = left.base


	override type InnerRow = @~ ~ last.Subject

	override def innerRow[E <: FromClause]
	                     (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, InnerRow] =
		EmptyChain ~ last.extend(target)

	override type OuterRow = left.OuterRow

	override def outerRow[E <: FromClause]
	                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, GlobalScope, OuterRow] =
		left.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized GroupByAll M](
			generalized, unfiltered, extension.length, innerSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupByAll.*]

	override def name = "group by"

}






object GroupByAll {


//	/** Introduces a ''group by'' clause to a 'pure' ''from'' clause `F`, grouping it by the columns
//	  * of the given component.
//	  * @param from a pure ''from'' clause containing true relations for the ''from'' clause of the built SQL select,
//	  *             as well as possibly the relations from the ''from'' clauses of enclosing selects for dependent select
//	  *             statements.
//	  * @param group a shill expression resulting from an implicit conversion of a `Mapping` with
//	  *              a supertype of `F` as its `Origin` type.
//	  */
//	def apply[F <: FromSome, O <: FromClause, M[A] <: BaseMapping[S, A], S]
//	         (from :F, group :LooseComponent[O, M, S])(implicit origin :F <:< FromSome { type Generalized <: O })
//			:F GroupByAll M =
//	{
//		val relation = from.fullTableStack(group.shift).toRelationSQL
//		                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
//		val component = TypedComponentSQL(relation, group.mapping)(group.projection.isomorphism)
//		GroupByAll(from, component.groupingRelation)
//	}

	/** Introduces a ''group by'' clause to a 'pure' ''from'' clause `F`, grouping it by the columns
	  * of the given component.
	  * @param from a pure ''from'' clause containing true relations for the ''from'' clause of the built SQL select,
	  *             as well as possibly the relations from the ''from'' clauses of enclosing selects for dependent select
	  *             statements.
	  * @param group a component expression from the mapping of one of the relations from the clause `F`.
	  */
	def apply[F <: FromSome { type Generalized <: U }, U <: FromClause, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :ComponentSQL[U, M])(implicit cast :InferSubject[F, GroupByAll, M, T, S]) :F GroupByAll M =
		GroupByAll(from, group.groupingRelation)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `M[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `G`
	  *             and its narrowed down form of `M` with the required upper bound of `BaseMapping`.
	  * @return an `F GroupByAll G`.
	  */
	private[sql] def apply[F <: FromSome, G[O] <: MappingAt[O], M[O] <: BaseMapping[S, O], S]
	                      (from :F, group :Relation[G], filter :LocalBoolean[F#Generalized GroupByAll G] = True)
	                      (implicit cast :InferSubject[F, GroupByAll, G, M, S]) :F GroupByAll G =
	{
		val last = RelationSQL[FromSome GroupByAll M, M, S, FromSome GroupByAll M](cast(group), 0)
		GroupByAll[F, M, S, Nothing](from, last, None)(cast.cast(filter))
	}


	private[sql] def apply[F <: FromSome, M[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :F, group :RelationSQL[FromSome GroupByAll M, M, S, FromSome GroupByAll M],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized GroupByAll M])
			:F GroupByAll M As A =
		new GroupByAll[clause.type, M] with GroupByClauseMatrix[clause.type GroupByAll M, clause.type GroupByAll M] {
			override val left = clause
			override val last = group
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause = left.self
			override val outer = left.outer
			override val fullSize = outer.fullSize + 1

			override type Alias = A
			override type WithLeft[+L <: FromSome] = L GroupByAll M As A
			override type Self = left.Self GroupByAll M As A
			override type DealiasedCopy = left.type GroupByAll M
			override type Copy = left.type GroupByAll M As A

			override def narrow :left.type GroupByAll M As A = this.asInstanceOf[left.type GroupByAll M As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupByAll[left.type, M, S, A](left, last, aliasOpt)(filter)

			override def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupByAll M]) =
				GroupByAll[L, M, S, A](left, last, aliasOpt)(condition)

			override def aliased[N <: Label](alias :N) =
				GroupByAll[left.type, M, S, N](left, last, Some(alias))(condition)

			override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) =
				last.extend(target) #:: outer.fullTableStack(target)(explicitSpan.extend(extension))


			override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
					:LazyList[RelationSQL.AnyIn[E]] =
				last.extend(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.groupBy[F, M, S](this)

		}.asInstanceOf[F GroupByAll M As A]



	/** Matches all `GroupByAll` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
		case _ :GroupByAll[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `GroupByAll` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case group :GroupByAll.* => Some((group.left, group.right))
		case _ => None
	}






	/** Type alias for `GroupByAll` with erased type parameters, covering all instances of `GroupByAll`/`GroupBy`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupByAll[_ <: FromSome, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `GroupByAll` instances, accepting the left `DiscreteFrom` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L GroupByAll R }

	/** A curried type constructor for `GroupByAll` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `DiscreteFrom` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L GroupByAll R }






	trait AndByAll[+F <: GroupByClause, M[A] <: MappingAt[A]]
		extends NonSubselect[F, M] with GroupByClause with GroupByClauseMatrix[F AndByAll M, F AndByAll M]
	{ thisClause =>

		override type FromLast = GroupByClause AndByAll M

		override type Generalized >: Dealiased <: (left.Generalized AndByAll M) {
			type Generalized <: thisClause.Generalized
			type Explicit <: thisClause.Explicit
			type Implicit <: thisClause.Implicit
			type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
		}

		type Dealiased >: Self <: (left.Self AndByAll M) {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Implicit = thisClause.Implicit
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
		}

		override type Self <: (left.Self AndByAll M) {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
		}


		protected override def narrow :WithLeft[left.type]


		type GeneralizedLeft[+L <: GroupByClause] <: (L AndByAll M)
		type DealiasedLeft[+L <: GroupByClause] <: GeneralizedLeft[L]
		type WithLeft[+L <: GroupByClause] <: DealiasedLeft[L]
		type LeftBound = GroupByClause

		private[sql] def unsafeLeftSwap[L <: GroupByClause]
		                               (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L] =
			withLeft(left)(filter)

		protected def withLeft[L <: GroupByClause]
		                      (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]


		/** A copy of this clause with the `condition` being replaced with the given `filter`.
		  * This does not replace the whole ''having'' filter, as the conditions (if present) of the left clause remain
		  * unchanged. It is the target of the `having` and other filtering methods (which add to the condition, rather
		  * then completely replacing it).
		  */
		def withCondition(filter :LocalBoolean[Generalized]) :Copy

		override def filtered[S >: LocalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :Copy =
			withCondition(condition && filter)

		private[this] val cachedFilter = Lazy { filter(generalized) }

		override def filter :LocalBoolean[Generalized] = cachedFilter.get

		override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] =
			left.filter(target)(extension.extendFront[left.Generalized, M]) && condition.basedOn(target)



		override type Grouping[+U <: FromSome] = WithLeft[left.Grouping[U]]
		override type GeneralizedDiscrete = left.GeneralizedDiscrete
		override type Discrete = left.Discrete


		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[AndByAll[_, T] forSome { type T[O] <: MappingAt[O] }]

	}

}







/** An extension of a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause providing an additional
  * grouping expression. This type in any clause must be preceded by a similar
  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] type, which adapts
  * a [[net.noresttherein.oldsql.sql.DiscreteFrom discrete]] ''from'' producing individual rows into groups of rows.
  * The appended [[net.noresttherein.oldsql.schema.Relation relation]] is not a database table, view, or select as with
  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope S]`, where type `S` is the
  * subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` of the right type parameter
  * and `F =:= this.GeneralizedDiscrete` is the generalized form of the left type parameter of the preceding `GroupByAll`.
  * The mapping may either directly come from a component of any of the relations from the ''from'' clause `F`
  * - ranging from a whole entity to a single column - or be another synthetic adapter, this type for an arbitrary
  * SQL expression. In either case, all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns
  * of the mapping are used in their order; for non-components, the expression is recursively reduced to individual
  * [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions which comprise the column list.
  *
  * As grouping by composite SQL expressions is a common use case, two type aliases exist to support this feature:
  * [[net.noresttherein.oldsql.sql.ByVal ByVal]]`[F, V]`
  * and [[net.noresttherein.oldsql.sql.ByOne ByOne]]`[F, V]` which take only the subject type of the mapping
  * as the `V` parameter, using a generic [[net.noresttherein.oldsql.schema.BaseMapping BaseMapping]] (for the former)
  * or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] (for the latter) as the adapter used
  * in the grouping relation of this instance.
  *
  * This 'join' is created and provides an interface very similar to `GroupByAll`, but unlike it,
  * and like all [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] subtypes it is a true extension of the
  * ''group by'' clause of its left side, as both the mapping `M` from it and all preceding mappings of
  * grouping expressions since the last `GroupByAll` are normally available to the ''select'' and ''having'' clauses.
  * All relations listed in the clause between the last [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) and `GroupByAll` become
  * 'shadowed' and unavailable through the [[net.noresttherein.oldsql.sql.FromClause.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateSQL.Count Count]] or [[net.noresttherein.oldsql.sql.AggregateSQL.Avg Avg]]).
  * Relations preceding the last `Subselect` are the relations of outer selects, not direct members of this select's
  * ''from'' clause, and are available individually as normal. Additional `ByAll` extensions may follow this type,
  * adding new grouping expressions to this ''group by'' clause.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnSQLAggregateMethods]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateSQL.AggregateFunction AggregateFunction]].
  * In both cases, the `FromClause` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.ByAll$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.MappingSQL.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.MappingSQL.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.ByVal$ ByVal]] and [[net.noresttherein.oldsql.sql.ByOne$ ByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] added to any `GroupByClause`.
  * @author Marcin Mościcki
  */
trait ByAll[+F <: GroupByClause, M[A] <: MappingAt[A]]
	extends AndByAll[F, M] with GroupByClauseMatrix[F ByAll M, F ByAll M]
{ thisClause =>

	override type Generalized = left.Generalized ByAll M
	override type Dealiased = left.Self ByAll M
	override type Self <: left.Self ByAll M

	override type GeneralizedLeft[+L <: GroupByClause] = L ByAll M
	override type DealiasedLeft[+L <: GroupByClause] = L ByAll M
	override type WithLeft[+L <: GroupByClause] <: L ByAll M



	override type Params = left.Params
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = unsafeLeftSwap[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		unsafeLeftSwap[l.type](l)(substitute(condition))
	}

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))



	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withLeft(left.joinedWithSubselect(prefix))(condition)

	override def appendedTo[P <: DiscreteFrom](prefix :P) :WithLeft[left.JoinedWith[P, AndFrom]] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit ByAll M
	override type Inner = left.Inner ByAll M
	override type Base = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: FromClause] = left.DefineBase[I]
	override def base :Base = left.base


	override type AsSubselectOf[+P <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized ByAll M](
			generalized, unfiltered, extension.length, innerSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ByAll.*]

	override def name = "by"

}





object ByAll {

//	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
//	  * of the given component.
//	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
//	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
//	  *             the discrete, 'true' ''from'' clause grouped under this clause.
//	  * @param group a shill expression resulting from an implicit conversion of a `Mapping` with
//	  *              a supertype of `F =:= from.GeneralizedDiscrete` as its `Origin` type.
//	  */
//	def apply[F <: FromSome, O <: FromClause, G <: GroupByClause, M[A] <: BaseMapping[S, A], S]
//	         (from :G, group :LooseComponent[O, M, S])
//	         (implicit origin :F <:< FromSome { type Generalized <: O }, grouping :G <:< GroupingOf[F]) :G ByAll M =
//	{
//		val relation = from.from.fullTableStack(group.shift).toRelationSQL
//		                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
//		val component = TypedComponentSQL(relation, group.mapping)(group.projection)
//		ByAll[G, M, M, S](from, component.groupingRelation)
//	}

	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
	  * of the given component.
	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
	  *             the discrete, 'true' ''from'' clause grouped under this clause.
	  * @param group a component expression from the mapping of one of the relations from the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  */
	def apply[F <: FromClause, G <: GroupingOfGeneralized[F], M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (from :G, group :ComponentSQL[F, M])
	         (implicit  cast :InferSubject[G, ByAll, M, T, S])
			:G ByAll M =
		ByAll(from, group.groupingRelation)


	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `M`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `G GroupByAll M`.
	  */
	private[sql] def apply[G <: GroupByClause, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                      (from :G, group :Relation[M], filter :LocalBoolean[G#Generalized ByAll M] = True)
	                      (implicit cast :InferSubject[G, ByAll, M, T, S]) :G ByAll M =
		ByAll[G, T, S, Nothing](from, RelationSQL(cast(group), 0), None)(cast.cast(filter))



	private[sql] def apply[G <: GroupByClause, T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :G, group :RelationSQL[GroupByClause AndByAll T, T, S, GroupByClause AndByAll T],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized ByAll T])
			:G ByAll T As A =
		new ByAll[clause.type, T] with AbstractExtended[clause.type, T, S] {
			override val left = clause
			override val last = group
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause = left.fromClause
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type Alias = A
			override type WithLeft[+L <: GroupByClause] = L ByAll T As A
			override type Self = left.Self ByAll T As A
			override type DealiasedCopy = left.type ByAll T
			override type Copy = left.type ByAll T As A
			//todo: this cast completely invalidates the purpose of this being an anonymous class
			override def narrow :left.type ByAll T As A = this.asInstanceOf[left.type ByAll T As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				ByAll[left.type, T, S, A](left, last, aliasOpt)(filter)

			override def withLeft[L <: GroupByClause](left :L)(condition :LocalBoolean[left.Generalized ByAll T]) =
				ByAll[L, T, S, A](left, last, aliasOpt)(condition)

			override def aliased[N <: Label](alias :N) =
				ByAll[left.type, T, S, N](left, last, Some(alias))(condition)

			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.by[G, T, S](this)

		}.asInstanceOf[G ByAll T As A]



	/** Matches all `ByAll` instances, splitting them into their left (preceding column groupings)
	  * and right (the last column grouping) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
		case _ :ByAll[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `ByAll` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(GroupByClause, Relation.*)] = from match {
		case group :ByAll.* => Some((group.left, group.right))
		case _ => None
	}


	//todo: can it incorporate aliased clauses the same way join does?
	implicit def byAllDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L ByAll R, L, R, ByAll, GroupByClause] =
		composition.asInstanceOf[ExtendedDecomposition[L ByAll R, L, R, ByAll, GroupByClause]]

	private[this] val composition =
		new ExtendedDecomposition[GroupByClause ByAll MappingAt, GroupByClause, MappingAt, ByAll, GroupByClause]






	/** Type alias for `ByAll` with erased type parameters, covering all instances of `ByAll`/`By`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = ByAll[_ <: GroupByClause, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `ByAll` instances, accepting the left `GroupByClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: GroupByClause] = { type F[R[O] <: MappingAt[O]] = L ByAll R }

	/** A curried type constructor for `ByAll` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `GroupByClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L ByAll R }

}

