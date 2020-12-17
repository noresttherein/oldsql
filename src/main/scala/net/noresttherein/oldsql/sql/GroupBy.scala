package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroupingOfGeneralized, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.GroupBy.AndBy
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.{AggregateSQL, MappingSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{ComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.mechanics.{RowProductMatcher, SQLScribe}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] extension introducing a ''group by'' clause.
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
  * as the `V` parameter, using a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] (for the former)
  * or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] (for the latter) as the adapter used
  * in the grouping relation of this instance.
  *
  * This 'join' works differently than other [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] subtypes in that
  * it doesn't [[net.noresttherein.oldsql.sql.Expanded expand]] the clause on its left side: all relations
  * from the ''from'' clause `F` since the last occurrence of [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) become 'shadowed'
  * and unavailable through the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] or
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Avg Avg]]). Relations preceding the last `Subselect`
  * are the relations of outer selects, not direct members of this select's ''from'' clause, and are available
  * individually as normal. Adding additional grouping expressions to this clause in the form of
  * the [[net.noresttherein.oldsql.sql.By By]] sister type is done in a similar fashion to creating
  * this clause: by an extension method [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]],
  * which accepts a function from `JoinedMappings[F#Generalized]` (obtained from `this.mappings.grouped`),
  * where `F` is the left type parameter of ''this'' trait, to an `SQLExpression[F#Generalized, GlobalScope, V]`.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnSQLAggregateMethods]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]].
  * In both cases, the `RowProduct` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.GroupBy$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.GroupByVal$ GroupByVal]] and [[net.noresttherein.oldsql.sql.GroupByOne$ GroupByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]] added to any
  * [[net.noresttherein.oldsql.sql.FromSome non-empty, discrete]] clause.
  * @author Marcin Mościcki
  */
trait GroupBy[+F <: FromSome, M[A] <: MappingAt[A]]
	extends Adjoin[F, M] with GroupByClause with GroupByClauseTemplate[F GroupBy M, F GroupBy M]
{ thisClause =>

	override type FromLast = FromSome GroupBy M
	override type Generalized = left.Generalized GroupBy M
	override type Dealiased = left.Self GroupBy M
	override type Self <: left.Self GroupBy M

	type DealiasedLeft[+L <: FromSome] = L GroupBy M
	type WithLeft[+L <: FromSome] <: L GroupBy M
	type LeftBound = FromSome

	private[sql] def unsafeLeftSwap[L <: FromSome]
	                               (left :L)(condition :LocalBoolean[left.Generalized GroupBy M]) :WithLeft[L] =
		withLeft(left)(condition)

	protected def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupBy M]) :WithLeft[L]

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

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :LocalBoolean[E] =
		condition.basedOn(target)


	override type LastParam = left.LastParam
	override type Params = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		val unfiltered = unsafeLeftSwap[l.type](l)(True)
		val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param, lastParamOffset)
		unsafeLeftSwap[l.type](l)(substitute(condition))
	}

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

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.outerRow(target)(explicitSpan.expand(expansion)) ~ last.expand(target)


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withLeft(left.joinedWithSubselect(prefix))(condition)

	override def appendedTo[P <: FromClause](prefix :P) :JoinedWith[P, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Grouping[+U <: FromSome] = U GroupBy M
	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupBy M
	override type Inner = left.Inner GroupBy M
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Base = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]

	override def base :Base = left.base


	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, Row] =
		EmptyChain ~ last.expand(target)

	override type OuterRow = left.OuterRow

	override def outerRow[E <: RowProduct]
	                     (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, GlobalScope, OuterRow] =
		left.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized GroupBy M](
			generalized, unfiltered, expansion.length, size + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupBy.*]

	override def name = "group by"

}






object GroupBy {

	/** Introduces a ''group by'' clause to a 'pure' ''from'' clause `F`, grouping it by the columns
	  * of the given component.
	  * @param from a pure ''from'' clause containing true relations for the ''from'' clause of the built SQL select,
	  *             as well as possibly the relations from the ''from'' clauses of enclosing selects for dependent select
	  *             statements.
	  * @param group a component expression from the mapping of one of the relations from the clause `F`.
	  */
	def apply[F <: FromSome { type Generalized <: U }, U <: RowProduct, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :ComponentSQL[U, M])(implicit cast :InferSubject[F, GroupBy, M, T, S]) :F GroupBy M =
		GroupBy(from, group.groupingRelation)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `M[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `G`
	  *             and its narrowed down form of `M` with the required upper bound of `BaseMapping`.
	  * @return an `F GroupBy G`.
	  */
	private[sql] def apply[F <: FromSome, G[O] <: MappingAt[O], M[O] <: BaseMapping[S, O], S]
	                      (from :F, group :Relation[G], filter :LocalBoolean[F#Generalized GroupBy G] = True)
	                      (implicit cast :InferSubject[F, GroupBy, G, M, S]) :F GroupBy G =
	{
		val last = RelationSQL[FromSome GroupBy M, M, S, FromSome GroupBy M](cast(group), 0)
		GroupBy[F, M, S, Nothing](from, last, None)(cast.cast(filter))
	}


	private[sql] def apply[F <: FromSome, M[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :F, group :RelationSQL[FromSome GroupBy M, M, S, FromSome GroupBy M],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized GroupBy M])
			:F GroupBy M As A =
		new GroupBy[clause.type, M] with GroupByClauseTemplate[clause.type GroupBy M, clause.type GroupBy M] {
			override val left = clause
			override val last = group
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause = left.self
			override val outer = left.outer
			override val fullSize = outer.fullSize + 1

			override type Alias = A
			override type WithLeft[+L <: FromSome] = L GroupBy M As A
			override type Self = left.Self GroupBy M As A
			override type DealiasedCopy = left.type GroupBy M
			override type Copy = left.type GroupBy M As A

			override def narrow :left.type GroupBy M As A = this.asInstanceOf[left.type GroupBy M As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupBy[left.type, M, S, A](left, last, aliasOpt)(filter)

			override def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupBy M]) =
				GroupBy[L, M, S, A](left, last, aliasOpt)(condition)

			override def aliased[N <: Label](alias :N) =
				GroupBy[left.type, M, S, N](left, last, Some(alias))(condition)

			override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
				last.expand(target) #:: outer.fullTableStack(target)(explicitSpan.expand(expansion))


			override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
					:LazyList[RelationSQL.AnyIn[E]] =
				last.expand(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.groupBy[F, M, S](this)

		}.asInstanceOf[F GroupBy M As A]



	/** Matches all `GroupBy` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Option[(L, Relation[R])] = from match {
		case _ :GroupBy[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `GroupBy` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Relation.*)] = from match {
		case group :GroupBy.* => Some((group.left, group.right))
		case _ => None
	}






	/** Type alias for `GroupBy` with erased type parameters, covering all instances of `GroupBy`/`GroupBy`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupBy[_ <: FromSome, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `GroupBy` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L GroupBy R }

	/** A curried type constructor for `GroupBy` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L GroupBy R }






	trait AndBy[+F <: GroupByClause, M[A] <: MappingAt[A]]
		extends NonSubselect[F, M] with GroupByClause with GroupByClauseTemplate[F AndBy M, F AndBy M]
	{ thisClause =>

		override type FromLast = GroupByClause AndBy M

		override type Generalized >: Dealiased <: (left.Generalized AndBy M) {
			type Generalized <: thisClause.Generalized
			type Explicit <: thisClause.Explicit
			type Implicit <: thisClause.Implicit
			type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
		}

		type Dealiased >: Self <: (left.Self AndBy M) {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Implicit = thisClause.Implicit
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row = thisClause.Row
			type OuterRow = thisClause.OuterRow
		}

		override type Self <: (left.Self AndBy M) {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row = thisClause.Row
			type OuterRow = thisClause.OuterRow
		}


		protected override def narrow :WithLeft[left.type]


		type GeneralizedLeft[+L <: GroupByClause] <: (L AndBy M)
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

		override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :LocalBoolean[E] =
			left.filter(target)(expansion.expandFront[left.Generalized, M]) && condition.basedOn(target)



		override type Grouping[+U <: FromSome] = WithLeft[left.Grouping[U]]
		override type GeneralizedDiscrete = left.GeneralizedDiscrete
		override type Discrete = left.Discrete


		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[AndBy[_, T] forSome { type T[O] <: MappingAt[O] }]

	}

}







/** An extension of a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause providing an additional
  * grouping expression. This type in any clause must be preceded by a similar
  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] type, which adapts
  * a [[net.noresttherein.oldsql.sql.FromClause discrete]] ''from'' producing individual rows into groups of rows.
  * The appended [[net.noresttherein.oldsql.schema.Relation relation]] is not a database table, view, or select as with
  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope S]`, where type `S` is the
  * subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `M` of the right type parameter
  * and `F =:= this.GeneralizedDiscrete` is the generalized form of the left type parameter of the preceding `GroupBy`.
  * The mapping may either directly come from a component of any of the relations from the ''from'' clause `F`
  * - ranging from a whole entity to a single column - or be another synthetic adapter, this type for an arbitrary
  * SQL expression. In either case, all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]] columns
  * of the mapping are used in their order; for non-components, the expression is recursively reduced to individual
  * [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions which comprise the column list.
  *
  * As grouping by composite SQL expressions is a common use case, two type aliases exist to support this feature:
  * [[net.noresttherein.oldsql.sql.ByVal ByVal]]`[F, V]`
  * and [[net.noresttherein.oldsql.sql.ByOne ByOne]]`[F, V]` which take only the subject type of the mapping
  * as the `V` parameter, using a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] (for the former)
  * or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] (for the latter) as the adapter used
  * in the grouping relation of this instance.
  *
  * This 'join' is created and provides an interface very similar to `GroupBy`, but unlike it,
  * and like all [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] subtypes it is a true expansion of the
  * ''group by'' clause of its left side, as both the mapping `M` from it and all preceding mappings of
  * grouping expressions since the last `GroupBy` are normally available to the ''select'' and ''having'' clauses.
  * All relations listed in the clause between the last [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) and `GroupBy` become
  * 'shadowed' and unavailable through the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] or
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Avg Avg]]). Relations preceding the last `Subselect`
  * are the relations of outer selects, not direct members of this select's ''from'' clause, and are available
  * individually as normal. Additional `By` expansions may follow this type, adding new grouping expressions
  * to this ''group by'' clause.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLAggregateMethods ColumnSQLAggregateMethods]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]].
  * In both cases, the `RowProduct` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.By$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.ast.MappingSQL.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.ast.MappingSQL.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.ByVal$ ByVal]] and [[net.noresttherein.oldsql.sql.ByOne$ ByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] added to any `GroupByClause`.
  * @author Marcin Mościcki
  */
trait By[+F <: GroupByClause, M[A] <: MappingAt[A]]
	extends AndBy[F, M] with GroupByClauseTemplate[F By M, F By M]
{ thisClause =>

	override type Generalized = left.Generalized By M
	override type Dealiased = left.Self By M
	override type Self <: left.Self By M

	override type GeneralizedLeft[+L <: GroupByClause] = L By M
	override type DealiasedLeft[+L <: GroupByClause] = L By M
	override type WithLeft[+L <: GroupByClause] <: L By M

	override type LastParam = left.LastParam
	override type Params = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		val unfiltered = unsafeLeftSwap[l.type](l)(True)
		val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param, lastParamOffset)
		unsafeLeftSwap[l.type](l)(substitute(condition))
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = unsafeLeftSwap[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		unsafeLeftSwap[l.type](l)(substitute(condition))
	}

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))



	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, M]) ~ last.expand(target)



	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
		withLeft(left.joinedWithSubselect(prefix))(condition)

	override def appendedTo[P <: FromClause](prefix :P) :WithLeft[left.JoinedWith[P, NonParam]] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit By M
	override type Inner = left.Inner By M
	override type Base = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]
	override def base :Base = left.base


	override type Row = left.Row ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, M]) ~ last.expand(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized By M](
			generalized, unfiltered, expansion.length, size + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[By.*]

	override def name = "by"

}





object By {

//	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
//	  * of the given component.
//	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
//	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
//	  *             the discrete, 'true' ''from'' clause grouped under this clause.
//	  * @param group a shill expression resulting from an implicit conversion of a `Mapping` with
//	  *              a supertype of `F =:= from.GeneralizedDiscrete` as its `Origin` type.
//	  */
//	def apply[F <: FromSome, O <: RowProduct, G <: GroupByClause, M[A] <: BaseMapping[S, A], S]
//	         (from :G, group :LooseComponent[O, M, S])
//	         (implicit origin :F <:< FromSome { type Generalized <: O }, grouping :G <:< GroupingOf[F]) :G By M =
//	{
//		val relation = from.from.fullTableStack(group.offset).toRelationSQL
//		                   .asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, F]]
//		val component = TypedComponentSQL(relation, group.mapping)(group.projection)
//		By[G, M, M, S](from, component.groupingRelation)
//	}

	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
	  * of the given component.
	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
	  *             the discrete, 'true' ''from'' clause grouped under this clause.
	  * @param group a component expression from the mapping of one of the relations from the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  */
	def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (from :G, group :ComponentSQL[F, M])
	         (implicit  cast :InferSubject[G, By, M, T, S])
			:G By M =
		By(from, group.groupingRelation)


	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `M`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `G GroupBy M`.
	  */
	private[sql] def apply[G <: GroupByClause, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                      (from :G, group :Relation[M], filter :LocalBoolean[G#Generalized By M] = True)
	                      (implicit cast :InferSubject[G, By, M, T, S]) :G By M =
		By[G, T, S, Nothing](from, RelationSQL(cast(group), 0), None)(cast.cast(filter))



	private[sql] def apply[G <: GroupByClause, T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :G, group :RelationSQL[GroupByClause AndBy T, T, S, GroupByClause AndBy T],
	                       asOpt :Option[A])
	                      (cond :LocalBoolean[clause.Generalized By T])
			:G By T As A =
		new By[clause.type, T] with AbstractExpanded[clause.type, T, S] {
			override val left = clause
			override val last = group
			override val aliasOpt = asOpt
			override val condition = cond
			override val fromClause = left.fromClause
			override val outer = left.outer
			override val fullSize = left.fullSize + 1
			override def lastRelation = last

			override type Alias = A
			override type WithLeft[+L <: GroupByClause] = L By T As A
			override type Self = left.Self By T As A
			override type DealiasedCopy = left.type By T
			override type Copy = left.type By T As A
			//todo: this cast completely invalidates the purpose of this being an anonymous class
			override def narrow :left.type By T As A = this.asInstanceOf[left.type By T As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				By[left.type, T, S, A](left, last, aliasOpt)(filter)

			override def withLeft[L <: GroupByClause](left :L)(condition :LocalBoolean[left.Generalized By T]) =
				By[L, T, S, A](left, last, aliasOpt)(condition)

			override def aliased[N <: Label](alias :N) =
				By[left.type, T, S, N](left, last, Some(alias))(condition)

			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.by[G, T, S](this)

		}.asInstanceOf[G By T As A]



	/** Matches all `By` instances, splitting them into their left (preceding column groupings)
	  * and right (the last column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Option[(L, Relation[R])] = from match {
		case _ :By[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `By` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(GroupByClause, Relation.*)] = from match {
		case group :By.* => Some((group.left, group.right))
		case _ => None
	}


	//todo: can it incorporate aliased clauses the same way join does?
	implicit def byDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L By R, L, R, By, GroupByClause] =
		composition.asInstanceOf[ExpandedDecomposition[L By R, L, R, By, GroupByClause]]

	private[this] val composition =
		new ExpandedDecomposition[GroupByClause By MappingAt, GroupByClause, MappingAt, By, GroupByClause]






	/** Type alias for `By` with erased type parameters, covering all instances of `By`/`By`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = By[_ <: GroupByClause, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `By` instances, accepting the left `GroupByClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: GroupByClause] = { type F[R[O] <: MappingAt[O]] = L By R }

	/** A curried type constructor for `By` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `GroupByClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L By R }

}

