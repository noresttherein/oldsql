package net.noresttherein.oldsql.sql

import scala.annotation.showAsInfix

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.GroupByClause.{GroupByClauseTemplate, GroupingRelation, NonParamGrouping}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroupingOfGeneralized, NonEmptyRow, PartOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.GroupByScope
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ChainTuple, ComponentSQL, EmptySQL, GroupingSQL, JoinedGrouping, RelationSQL}
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] extension introducing a ''group by'' clause.
  * The appended [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation relation]] is not a database table,
  * view, or select as with [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F#Generalized, GlobalScope S]`, where type `S`
  * is the subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `R` of the right type parameter.
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
  * as the `V` parameter, using a synthetic [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] (for the former)
  * or [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]] (for the latter) as the adapter used
  * in the grouping relation of this instance.
  *
  * This 'join' works differently than other [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] subtypes in that
  * it doesn't [[net.noresttherein.oldsql.sql.Expanded expand]] the clause on its left side: all relations
  * from the ''from'' clause `L` since the last occurrence of [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) become 'shadowed'
  * and unavailable through the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] or
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Avg Avg]]). Relations preceding the last `Subselect`
  * are the relations of outer selects, not direct members of this select's ''from'' clause, and are available
  * individually as normal.
  *
  * Note that, as a side effect, any [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] unbound parameters
  * included in `F` are also unavailable for expressions based on this clause (and extending clauses).
  * This can be addressed in two ways; one is by repeating the parameter declaration using
  * [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] join, created in the same way as the former,
  * in this way doubling the occurrence of the parameter in the unbound parameter list
  * [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] of the clause. As an alternative, the expression
  * for the shadowed parameter (or values derived from the parameter) can be, like any other
  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression, be used as the grouping expression
  * used by either this trait or [[net.noresttherein.oldsql.sql.By By]]. In that case the value of the parameter
  * is made available as if it was any other expression and is not considered an unbound parameter by other classes.
  * It doesn't however expand the generated ''group by'' clause in any way - no '?' JDBC parameter placeholders
  * are used in the ''group by'' clause, but rather at the point of reference, i.e. where the value of the
  * added grouping relation is used within another SQL expression.
  *
  * Adding additional grouping expressions to this clause in the form of
  * the [[net.noresttherein.oldsql.sql.By By]] sister type is done in a similar fashion to creating
  * this clause: by an extension method [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]],
  * which accepts a function from `JoinedMappings[F#Generalized]` (obtained from `this.mappings.grouped`),
  * where `F` is the left type parameter of ''this'' trait, to an `SQLExpression[F#Generalized, GlobalScope, V]`.
  *
  * Note that the information about the grouping expression's dependency on the
  * [[net.noresttherein.oldsql.sql.AndBy.Discrete ungrouped]] ''from'' clause is not preserved in expressions
  * referencing this grouping expression, as [[net.noresttherein.oldsql.sql.GroupBy.FromLast FromLast]] domain type
  * used by [[net.noresttherein.oldsql.sql.By.last last]] grouping expression is `FromSome AndFrom G`,
  * which poses an apparent type safety violation. This is not the case however,
  * as any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions - in particular
  * [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]] relation expression itself - are semantically
  * deemed ''placeholders'', to be evaluated in the context of the `RowProduct` instance used within
  * an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]]. While this is of little practical importance
  * in case of [[net.noresttherein.oldsql.sql.Join Join]] subclasses, where the only difference will typically amount
  * to adding or removing optional columns from the selected column set, in case of `GroupBy` and `By`, this means
  * that the grouping expression for an arbitrary type `S` can evaluate to an unrelated expression
  * of the same type, with the generated SQL for the same instance being potentially completely different, depending
  * on the ''select'' within which it is used. In particular, component expressions referencing subsets of the columns
  * of the grouping expression in this instance, will be translated column-by-column to the corresponding columns
  * of the expression provided by the effective ''from'' clause. This is consistent with all other component expressions,
  * and a way to remember it is that the 'real' grouping relation is not a part of the grouping expression instance,
  * but of the ''from'' clause with the `GroupBy` clause used. This discrepancy can however only arise in generic code
  * which uses prebuilt expression instances, as in static contexts the expressions are created and used in tandem
  * with `RowProduct` instances carrying them.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLExtension ColumnSQLExtension]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]].
  * In both cases, the `RowProduct` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.GroupBy$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.GroupByVal$ GroupByVal]] and [[net.noresttherein.oldsql.sql.GroupByOne$ GroupByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]] added to any
  * [[net.noresttherein.oldsql.sql.FromSome non-empty, discrete]] clause.
  * @author Marcin Mościcki
  */ //todo: rename the type parameters for consistency (everywhere)
@showAsInfix
trait GroupBy[+L <: FromSome, R[A] <: MappingAt[A]]
	extends NonParamGrouping[L, R] with GroupByClauseTemplate[L GroupBy R, L GroupBy R]
{ thisClause =>

	override val expr             :SQLExpression[left.Generalized, Single, last.Subject]
	override val grouping         :GroupingRelation[left.Generalized, R]// { type From <: left.Self }
//	override def right            :GroupingRelation[left.Generalized, M] = grouping
	override val last             :JoinedGrouping[left.Generalized, FromLast, R]
	override def generalizedClass :Class[left.Generalized GroupBy R] = classOf[left.Generalized GroupBy R]
	override def selfClass        :Class[Self] = classOf[left.Self GroupBy R].asInstanceOf[Class[Self]]

	override type Generalized = left.Generalized GroupBy R
	override type Complete    = left.Complete GroupBy R
	override type NoAlias     = left.Self GroupBy R
	override type Self       <: left.Self GroupBy R

	type NoAliasLeft[+F <: FromSome] = F GroupBy R
	type WithLeft[+F <: FromSome]   <: F GroupBy R //consider: rename to GroupLeft/SwapLeft/Left
	type LeftBound = FromSome

	/** Substitutes the left (ungrouped) side of this clause with another instance of a compatible type.
	  * It is primarily used when SQL expressions being parts of this clause need rewriting
	  * (such as [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchoring]]).
	  * The [[net.noresttherein.oldsql.sql.GroupBy.grouping grouping]] expression is not rewritten, meaning
	  * it may become unanchored in the process. The upper bound, not present in the analogous method
	  * [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.withLeft AndFrom.withLeft]], is needed because
	  * the grouping relation must remain type compatible with the new clause. In order to change the left side
	  * to an arbitrary `FromSome` subtype, use [[net.noresttherein.oldsql.sql.GroupBy.rewriteLeft rewriteLeft]],
	  * which will rewrite both the grouping and filter expressions accordingly.
	  *
	  * This is a lower level 'virtual constructor' method, and application code should generally prefer the API
	  * provided by [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate GroupByClauseTemplate]]
	  * and its supertypes.
	  * @param left   a clause which will become [[net.noresttherein.oldsql.sql.Adjoin.left left]] property of the result.
	  * @param filter a Boolean expression which will become
	  *               [[net.noresttherein.oldsql.sql.GroupByClause.condition condition]] property of the result.
	  *               It will not be altered in any way by this method.
	  */
	def withLeft[F <: FromSome { type Generalized <: thisClause.left.Generalized }]
	            (left :F)(filter :GroupedBoolean[left.Generalized GroupBy R]) :WithLeft[F]

	/** Substitutes the left (ungrouped) side of this clause with another instance of `FromSome`.
	  * Because the [[net.noresttherein.oldsql.sql.GroupBy.expr grouping expression]] of this clause is based
	  * on its left side, it needs to be adapted to the new clause, a task handled by the passed `groupingScribe`
	  * transformation, which imposes practical constrains on type `L`.
	  * @param left           a clause which will become [[net.noresttherein.oldsql.sql.Adjoin.left left]] property
	  *                       of the result.
	  * @param groupingScribe an SQL expression rewriter used to transform the expression of
	  *                       the [[net.noresttherein.oldsql.sql.GroupBy.grouping grouping]] relation from this instance.
	  * @param filterScribe   a constructor function for an SQL expression rewriter used to transform
	  *                       the accompanying ''having'' clause filter from this instance.
	  * @see [[net.noresttherein.oldsql.sql.GroupBy.withLeft]]
	  */
	def rewriteLeft[F <: FromSome]
	               (left :F)
	               (groupingScribe :SQLScribe[GeneralizedDiscrete, left.Generalized],
	                filterScribe :(left.Generalized GroupBy R) => SQLScribe[Generalized, left.Generalized GroupBy R])
			:WithLeft[F]

	override def narrow :WithLeft[left.type]

	/** A copy of this clause with [[net.noresttherein.oldsql.sql.GroupByClause.condition condition]] property
	  * replaced with the given filter expression. The ''where'' clause defined by the left side remains unchanged.
	  * It is the target of [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]]
	  * and other filtering methods (which add to the condition, rather then completely replacing it).
	  * @param filter a Boolean expression to include as-is in the returned instance.
	  */
	def withCondition(filter :GroupedBoolean[Generalized]) :Copy

	override def columnCount :Int = GroupByScope.defaultColumns(right.export[Unit]).size


	override def filtered[S >: Grouped <: Single](filter :SQLBoolean[Generalized, S]) :Copy =
		if (filter == True) selfAsCopy
		else withCondition(condition && filter)

	override def filter :GroupedBoolean[Generalized] = condition

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GroupedBoolean[E] =
		condition.basedOn(target)

//	override type LastParam = left.LastParam
//	override type Params = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type GeneralizedParamless = left.GeneralizedParamless GroupBy R
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))

	override def isExplicitParameterized :Boolean = left.isExplicitParameterized
	override def lastParamOffset :Int = left.lastParamOffset match {

		case n if n < 0 => n
		case n if n < left.size => -1
		case n => n - left.size + 1
	}


	override def size :Int = 1
	override def fullSize :Int = outer.fullSize + 1

	override type FullRow = OuterRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow] =
		left.outerRow(target)(explicitSpan.expand(expansion)) ~ last.expand(target)


	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type SelectedFrom[+P <: NonEmptyRow] = WithLeft[left.SelectedFrom[P]]

	override def selectedFrom[F <: NonEmptyRow](prefix :F) :SelectedFrom[F] =
		withLeft(left.selectedFrom(prefix))(condition)

	override def appendedTo[F <: FromClause](prefix :F) :JoinedWith[F, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Grouping[+U <: FromSome] = U GroupBy R
	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupBy R
	override type Inner    = left.Inner GroupBy R
	override type Implicit = left.Implicit
	override type Outer    = left.Outer
	override type Base     = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]

	override def base :Base = left.base


	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, Row] =
		EmptySQL ~ last.expand(target)



	override type AsSubselectOf[+P <: NonEmptyRow] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		rewriteLeft[newLeft.type](newLeft)(
			SQLScribe.shiftBack[GeneralizedDiscrete, newLeft.Generalized](left.generalized, newLeft.generalized),
			SQLScribe.shiftBack[Generalized, newLeft.Generalized GroupBy R](generalized, _)
		)
	}


	override def withClause :WithClause = left.withClause ++ right.withClause ++ condition.outerWithClause

	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.inWhere.sqlParamCount(left) + spelling.inGroupBy.sqlParamCount(right) +
			spelling.inHaving.sqlParamCount(condition)

	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val fromParams = params.ungroup[fromClause.Generalized]
		val fromSQL = left.spell(context, fromParams)(spelling.inFrom)
		if (columnCount == 0) {
			SpelledSQL(fromSQL.sql, fromSQL.setter, fromSQL.context.grouped)
		} else { //exprSQL non empty because columnCount > 0 (and ParamMapping has zero columns)
			val exprSQL = spelling.grouping(grouping)(left.self, fromSQL.context, fromParams)
			val sql = fromSQL.sql + spelling._GROUP_BY_ + exprSQL.sql
			SpelledSQL(sql, fromSQL.setter + exprSQL.setter, exprSQL.context.grouped)
		}
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupBy.__]

	override def name = "group by"
}






object GroupBy {

	/** Introduces a ''group by'' clause to an ungrouped ''from'' clause `F`, grouping it by the columns
	  * of the given component.
	  * @param from a pure ''from'' clause containing true relations for the ''from'' clause of the built SQL select,
	  *             as well as possibly the relations from the ''from'' clauses of enclosing selects for dependent select
	  *             statements.
	  * @param group a component expression from the mapping of one of the relations from the clause `F`.
	  */
	def apply[F <: FromSome { type Generalized <: G }, G <: FromSome, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :ComponentSQL[G, M])(implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
		GroupBy[F, G, M, T, S](from, group, True)

	/** Introduces a ''group by'' clause to an ungrouped ''from'' clause `F`, grouping it by the columns
	  * of the given component.
	  * @param from a pure ''from'' clause containing true relations for the ''from'' clause of the built SQL select,
	  *             as well as possibly the relations from the ''from'' clauses of enclosing selects for dependent select
	  *             statements.
	  * @param group a component expression from the mapping of one of the relations from the clause `F`.
	  */ //consider: should we anchor the filter here?
	def apply[F <: FromSome { type Generalized <: G }, G <: FromSome, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :ComponentSQL[G, M], filter :GroupedBoolean[G GroupBy M])
	         (implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
	{
		val relation = cast(group).anchor(from.generalized).asGrouping //note that this might be a param expression
		val last = GroupingSQL[G, RowProduct AndBy T, T, S](relation, 0)
		cast.back.join(GroupBy[F, T, S, Nothing](from, None)(last, cast.column(filter)))
	}

	/** Create a ''group by'' clause to an ungrouped ''from'' clause `F`, grouping it by the expression wrapped
	  * in the given dedicated synthetic relation.,
	  * It is a lower level method; it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.groupBy groupBy]] ''expression''
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] ''filter'' DSL instead.
	  * @param from   a ''from'' clause containing the list of grouped tables.
	  * @param group  the relation for the first (and only) grouping expression of the created ''from'' clause,
	  *               using the `M[O] <: BaseMapping[S, O]` `Mapping` type.
	  * @param filter an optional condition filtering the result.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `G`
	  *               and its narrowed down form of `M` with the required upper bound of `BaseMapping`.
	  * @return an `F GroupBy G`.
	  */ //consider: should we anchor the filter here?
	def apply[F <: FromSome, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F)(group :GroupingRelation[from.Generalized, M], filter :GroupedBoolean[from.Generalized GroupBy M] = True)
	         (implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
	{
		val relation = cast(group).anchor(from.generalized)
		val last = GroupingSQL[from.Generalized, RowProduct AndBy T, T, S](relation, 0)
		cast.back.join(GroupBy[F, T, S, Nothing](from, None)(last, cast.column(filter)))
	}


	private[sql] def apply[F <: FromSome, M[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :F, asOpt :Option[A])
	                      (group :GroupingSQL[clause.Generalized, RowProduct AndBy M, M, S],
	                       cond :GroupedBoolean[clause.Generalized GroupBy M])
			:F GroupBy M As A =
		new GroupBy[clause.type, M] with GroupByClauseTemplate[clause.type GroupBy M, clause.type GroupBy M] {
			override val left       = clause
			override val last       = group
			override val grouping   = group.grouping
			override val expr       = grouping.expr
			override val aliasOpt   = asOpt
			override val condition  = cond
			override val fromClause = left.self
			override val outer      = left.outer
			override val fullSize   = outer.fullSize + 1
			override val parameterization :Parameterization[Params, Self] = left.parameterization.group(self).as(self)
			override lazy val tableStack     = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias       = A
			override type WithLeft[+L <: FromSome] = L GroupBy M As A
			override type Self        = left.Self GroupBy M As A
			override type NoAliasCopy = left.type GroupBy M
			override type Copy        = left.type GroupBy M As A

			override def narrow :left.type GroupBy M As A = this.asInstanceOf[left.type GroupBy M As A]

			override def withCondition(filter :GroupedBoolean[Generalized]) =
				GroupBy[left.type, M, S, A](left, aliasOpt)(last, filter)

			override def withLeft[L <: FromSome { type Generalized <: clause.Generalized }]
			                     (left :L)(filter :GroupedBoolean[left.Generalized GroupBy M]) =
				GroupBy[L, M, S, A](left, aliasOpt)(last, filter)

			override def rewriteLeft[L <: FromSome]
			                        (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.Generalized],
			                                  filterScribe :(left.Generalized GroupBy M)
				                                 => SQLScribe[Generalized, left.Generalized GroupBy M]) =
			{
				val last = GroupingSQL[left.Generalized, RowProduct AndBy M, M, S](
					grouping.copy(groupingScribe(expr).anchor(left.generalized)), 0
				)
				val unfiltered = GroupBy[left.type, M, S, A](left, aliasOpt)(last, True)
				if (condition == True)
					unfiltered
				else
					GroupBy[left.type, M, S, A](left, aliasOpt)(last, filterScribe(unfiltered.generalized)(condition))
			}

			override def aliased[N <: Label](alias :N) =
				GroupBy[left.type, M, S, N](left, Some(alias))(last, condition)


			override def bind(param :LastParam) :AppliedParam = {
				val l = left.bind(param)
				if (left.lastParamOffset < 0)
					GroupBy[l.type, M, S, A](l, aliasOpt)(
						this.last.asInstanceOf[GroupingSQL[l.Generalized, RowProduct AndBy M, M, S]],
						condition.asInstanceOf[SingleBoolean[l.Generalized GroupBy M]]
					)
				else {
					val last = {
						val groupingScribe = SQLScribe.applyParam(left.self, l.generalized, param)
						GroupingSQL[l.Generalized, RowProduct AndBy M, M, S](
							grouping.copy(groupingScribe(expr)), 0
						)
					}
					val unfiltered = GroupBy[l.type, M, S, A](l, aliasOpt)(last, True)
					if (condition == True)
						unfiltered
					else { //we need to rewrite even if left.lastParamOffset < left.size because of possible aggregate expressions
						val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param)
						GroupBy[l.type, M, S, A](l, aliasOpt)(last, substitute(condition))
					}
				}
			}

			override def bind(params :Params) :Paramless = {
				val l = left.bind(params)
				val groupingScribe = SQLScribe.applyParams(left.self, l.generalized)(params)
				rewriteLeft[l.type](l)(groupingScribe, SQLScribe.applyParams(self, _)(params))
			}

			override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
				LazyList.cons(
					last.expand[Generalized, E](target).upcast, outer.fullTableStack(target)(explicitSpan.expand(expansion))
				) //crashes the compiler:
//				last.expand(target) #:: outer.fullTableStack(target)(explicitSpan.expand(expansion))


			override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
				LazyList.cons(last.expand[Generalized, E](target).upcast, LazyList.empty[RelationSQL.from[E]#__])
//				last.expand(target) #:: LazyList.empty[RelationSQL.from[E]#__]


			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.groupBy[F, M, S](this)

		}.asInstanceOf[F GroupBy M As A]



	/** Matches all `GroupBy` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Relation[R])] = from match {
		case _ :GroupBy[_, _] => Got((from.left, from.right))
		case _ => Lack
	}

	/** Matches all `GroupBy` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Relation.__)] = from match {
		case group :GroupBy.__ => Got((group.left, group.right))
		case _ => Lack
	}




	/** Type alias for `GroupBy` with erased type parameters, covering all instances of `GroupBy`/`GroupBy`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = GroupBy[_ <: FromSome, T] forSome { type T[O] <: MappingAt[O] }

	/** An upper bound on all [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clauses with a single grouping
	  * expression of mapping `R`.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome GroupBy R

	/** Type alias for the upper bound of all [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] subclasses
	  * which join their left side with `M` as the mapping type of the last grouping expression.
	  * It is the [[net.noresttherein.oldsql.sql.AndBy.FromLast FromLast]] type of
	  * [[net.noresttherein.oldsql.sql.AndBy AndBy]] (all grouping clauses,
	  * including[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) and, as such,
	  * it it opens every [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] form
	  * of all aggregated clauses. In particular, it is also
	  * the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type used for the mapping of a last
	  * table in a ''group by'' clause.
	  */
	type Last[M[O] <: MappingAt[O]] = RowProduct AndBy M

	/** A curried type constructor for `GroupBy` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L GroupBy R }

	/** A curried type constructor for `GroupBy` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L GroupBy R }

}






/** The supertype of all clauses [[net.noresttherein.oldsql.sql.Expanded expanding]] ''group by'' clause `L`
  * with a relation using mapping `R`. As such, it is the supertype of
  * all ''group by'' clauses with more than one relation (in their proper, inner span) and ending with relation `R`.
  * It has two standard implementations: [[net.noresttherein.oldsql.sql.By By]] which uses an expression
  * based on the ungrouped ''from'' clause in `F` as the provider of the new relation,
  * and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] which introduces an unbound query parameter
  * in the same way as [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] in non-grouped ''from'' clauses.
  */
@showAsInfix
trait GroupJoin[+L <: GroupByClause, R[A] <: MappingAt[A]]
	extends AndBy[L, R] with NonSubselect[L, R] with GroupByClauseTemplate[L GroupJoin R, L GroupJoin R]
{ thisClause =>
	override type Generalized >: Complete <: (left.Generalized GroupJoin R) {
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete GroupJoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type NoAlias >: Self <: (left.Self GroupJoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self GroupJoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}


	override def narrow :WithLeft[left.type]


	type GeneralizedLeft[+F <: GroupByClause] <: (F GroupJoin R)
	type NoAliasLeft[+F <: GroupByClause] <: GeneralizedLeft[F]
	type WithLeft[+F <: GroupByClause] <: NoAliasLeft[F] //consider: rename GroupLeft/SwapLeft/Left
	type LeftBound = GroupByClause

	/** Expands the given ''group by'' clause with the grouping expression carried by this instance.
	  * This method is primarily used when SQL expressions being parts of this clause need rewriting
	  * (such as [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchoring]]).
	  * The [[net.noresttherein.oldsql.sql.GroupBy.grouping grouping]] expression is not rewritten, meaning
	  * it may become unanchored in the process. The upper bound, not present in the analogous method
	  * [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.withLeft AndFrom.withLeft]], is needed because
	  * the grouping relation must remain type compatible with the new clause. In order to change the left side
	  * to an arbitrary `GroupByClause` subtype, use [[net.noresttherein.oldsql.sql.By.rewriteLeft rewriteLeft]],
	  * which will rewrite both the grouping and filter expressions accordingly.
	  *
	  * This is a lower level 'virtual constructor' method, and application code should generally prefer the API
	  * provided by [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate GroupByClauseTemplate]]
	  * and its supertypes.
	  * @param left   a clause which will become [[net.noresttherein.oldsql.sql.Adjoin.left left]] property of the result.
	  * @param filter a Boolean expression which will become
	  *               [[net.noresttherein.oldsql.sql.GroupByClause.condition condition]] property of the result.
	  *               It will not be altered in any way by this method.
	  */
	def withLeft[F <: GroupByClause { type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete }]
	            (left :F)(filter :GroupedBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]


	/** A copy of this clause with [[net.noresttherein.oldsql.sql.GroupByClause.condition condition]] property
	  * replaced with the given filter expression. This does not replace the whole ''having'' clause, as the conditions
	  * of the left clause (if present) remain unchanged. It is the target of
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]] and other filtering methods
	  * (which add to the condition, rather then completely replacing it).
	  * @param filter a Boolean expression to include as-is in the returned instance.
	  */
	def withCondition(filter :GroupedBoolean[Generalized]) :Copy

	override def filtered[S >: Grouped <: Single](filter :SQLBoolean[Generalized, S]) :Copy =
		if (filter == True) selfAsCopy
		else withCondition(condition && filter)

	private[this] val cachedFilter = Lazy { filter(generalized) }

	override def filter :GroupedBoolean[Generalized] = cachedFilter.get

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GroupedBoolean[E] =
		left.filter(target)(expansion.expandFront[left.Generalized, R]) && condition.basedOn(target)



	override type Grouping[+U <: FromSome] = WithLeft[left.Grouping[U]]
	override type GeneralizedDiscrete = left.GeneralizedDiscrete
	override type Discrete = left.Discrete


	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P] =
		if (position < size)  //in this group by clause
			groupingSpellingContext(left)(position - 1, context.shrink(), params.left)
		else
			super.groupingSpellingContext(position, context, params)


	override def canEqual(that :Any) :Boolean =
		that.isInstanceOf[AndBy[_, T] forSome { type T[O] <: MappingAt[O] }]

}





object GroupJoin {

	/** Splits any `GroupJoin` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](join :L Adjoin R) :Opt[(L, Relation[R])] = join match {
		case join :GroupJoin[L @unchecked, R @unchecked] => Got((join.left, join.right))
		case _ => Lack
	}

	/** Matches all `GroupJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(RowProduct, Relation.__)] = from match {
		case join :GroupJoin.__ => Got((join.left, join.right))
		case _ => Lack
	}


	implicit def groupJoinDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L GroupJoin R, L, R, GroupJoin, GroupByClause] =
		decomposition.asInstanceOf[ExpandedDecomposition[L GroupJoin R, L, R, GroupJoin, GroupByClause]]

	private[this] val decomposition =
		new ExpandedDecomposition[GroupByClause GroupJoin MappingAt, GroupByClause, MappingAt, GroupJoin, GroupByClause]


	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = GroupJoin[_ <: GroupByClause, M] forSome { type M[O] <: MappingAt[O] }

	/** The upper bound of all [[net.noresttherein.oldsql.sql.GroupJoin GroupJoin]] instances grouping any clause
	  * by an expression represented by mapping `M`.
	  */
	type LUB[M[O] <: MappingAt[O]] = GroupByClause GroupJoin M

	/** A curried type constructor for `GroupJoin` instances, accepting the left `GroupByClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: GroupByClause] = { type F[R[O] <: MappingAt[O]] = L GroupJoin R }

	/** A curried type constructor for `GroupJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L GroupJoin R }

}






/** An extension of a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause providing an additional
  * grouping expression. This type in any clause must be preceded by a similar
  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] type, which adapts
  * a [[net.noresttherein.oldsql.sql.FromClause discrete]] ''from'' producing individual rows into groups of rows.
  * The appended [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation relation]] is not a database table,
  * view, or select as with [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope S]`, where type `S` is the
  * subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `R` of the right type parameter
  * and `F =:= this.GeneralizedDiscrete` is the generalized form of the left type parameter of the preceding `GroupBy`.
  * The mapping may either directly come from a component of any of the relations from the ''from'' clause `F`
  * - ranging from a whole entity to a single column - or be another synthetic adapter, this type for an arbitrary
  * SQL expression. In either case, all [[net.noresttherein.oldsql.schema.Mapping.filterable filterable]] columns
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
  * ''group by'' clause of its left side, as both the mapping `G` from it and all preceding mappings of
  * grouping expressions since the last `GroupBy` are normally available to the ''select'' and ''having'' clauses.
  * All relations listed in the clause between the last [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) and `GroupBy` become
  * 'shadowed' and unavailable through the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] facades and can't be used directly
  * in SQL expressions based on this clause, as the rows of these relations - forming the ''from'' clause of
  * the most deeply nested select represented by this clause - are unavailable individually and can only be
  * grouped or used in [[net.noresttherein.oldsql.sql.ast.AggregateSQL aggregate]] expressions (such as
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Count Count]] or
  * [[net.noresttherein.oldsql.sql.AggregateFunction.Avg Avg]]). This includes any pseudo relations introduced
  * with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] clause; they can however be exported and adapted
  * for use in SQL expressions based on this type by using them as the grouping expressions (expressions based on the
  * shadowed ''from'' clause provided to [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]]
  * extension methods) in the same way as any other expression. In that case the created `By` clause remains purely
  * virtual, having no effect on the rendered SQL: the JDBC parameter placeholders '?' are used only in places
  * where a component expression for the relation adapting the parameter is actually used. Relations preceding
  * the last `Subselect` are the relations of outer selects, not direct members of this select's ''from'' clause,
  * and are available individually as normal. Additional `By` expansions may follow this type,
  * adding new grouping expressions to this ''group by'' clause.
  *
  * Note that the information about the grouping expression's dependency on the
  * [[net.noresttherein.oldsql.sql.GroupJoin.Discrete ungrouped]] ''from'' clause is not preserved in expressions
  * referencing this grouping expression, as [[net.noresttherein.oldsql.sql.AndFrom.FromLast FromLast]] domain type
  * used by [[net.noresttherein.oldsql.sql.GroupByClause.NonParamGrouping.last last]] grouping expression
  * is `GroupByClause AndFrom R`, which poses an apparent type safety violation. This is not the case however,
  * as any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions - in particular
  * [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]] relation expression itself - are semantically
  * deemed ''placeholders'', to be evaluated in the context of the `RowProduct` instance used within
  * an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]]. While this is of little practical importance
  * in case of [[net.noresttherein.oldsql.sql.Join Join]] subclasses, where the only difference will typically amount
  * to adding or removing optional columns from the selected column set, in case of `GroupBy` and `By`, this means
  * that the grouping expression for an arbitrary type `S` can evaluate to an unrelated expression
  * of the same type, with the generated SQL for the same instance being potentially completely different, depending
  * on the ''select'' within which it is used. In particular, component expressions referencing subsets of the columns
  * of the grouping expression in this instance, will be translated column-by-column to the corresponding columns
  * of the expression provided by the effective ''from'' clause. This is consistent with all other component expressions,
  * and a way to remember it is that the 'real' grouping relation is not a part of the grouping expression instance,
  * but the ''from'' clause with the `By` clause used. This discrepancy can however only arise in generic code
  * which uses prebuilt expression instances, as in static contexts the expressions are created and used in tandem
  * with `RowProduct` instances carrying them.
  *
  * Universal SQL aggregate expressions for use in the ''having'' clause of this clause can be created through
  * extension methods of `ColumnSQL` from
  * [[net.noresttherein.oldsql.sql.ColumnSQL.ColumnSQLExtension ColumnSQLExtension]], or
  * directly using an appropriate [[net.noresttherein.oldsql.sql.AggregateFunction AggregateFunction]].
  * In both cases, the `RowProduct` type parameter of the aggregated expression must be `this.GeneralizedDiscrete`
  * (that is, `F#Generalized`), not this clause - as with expressions used for the grouping expression.
  *
  * Factory methods are available in the [[net.noresttherein.oldsql.sql.By$ companion]] object, accepting
  * either a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]] or
  * a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]. Similar methods
  * accepting a [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] and
  * [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]] exist in the 'companion' objects to the type aliases:
  * [[net.noresttherein.oldsql.sql.ByVal$ ByVal]] and [[net.noresttherein.oldsql.sql.ByOne$ ByOne]].
  * The typical and recommended practice is however to use the extension method
  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseExtension.by by]] added to any `GroupByClause`.
  * @author Marcin Mościcki
  */
@showAsInfix
trait By[+L <: GroupByClause, R[A] <: MappingAt[A]]
	extends GroupJoin[L, R] with NonParamGrouping[L, R] with GroupByClauseTemplate[L By R, L By R]
{ thisClause =>

	override val expr             :SQLExpression[GeneralizedDiscrete, Single, last.Subject]
	override val grouping         :GroupingRelation[GeneralizedDiscrete, R]
//	override def right            :GroupingRelation[GeneralizedDiscrete, M] = grouping
	override val last             :JoinedGrouping[GeneralizedDiscrete, FromLast, R]
	override def generalizedClass :Class[left.Generalized By R] = classOf[left.Generalized By R]
	override def selfClass        :Class[Self] = classOf[By[L, R]].asInstanceOf[Class[Self]]

	override type Generalized = left.Generalized By R
	override type Complete    = left.Complete By R
	override type NoAlias     = left.Self By R
	override type Self       <: left.Self By R

	override type GeneralizedLeft[+F <: GroupByClause] = F By R
	override type NoAliasLeft[+F <: GroupByClause]     = F By R
	override type WithLeft[+F <: GroupByClause]       <: F By R

	/** Substitutes the left side of this clause with another instance of `GroupByClause`.
	  * Because the [[net.noresttherein.oldsql.sql.By.expr grouping expression]] of this clause is based
	  * on its left side, it needs to be adapted to the new clause, a task handled by the passed `groupingScribe`
	  * transformation, which imposes practical constrains on type `L`.
	  * @param left           a clause which will become [[net.noresttherein.oldsql.sql.Adjoin.left left]] property
	  *                       of the result.
	  * @param groupingScribe an SQL expression rewriter used to transform the expression of
	  *                       the [[net.noresttherein.oldsql.sql.By.grouping grouping]] relation from this instance.
	  * @param filterScribe   a constructor function for an SQL expression rewriter used to transform
	  *                       the accompanying ''having'' clause filter from this instance.
	  * @see [[net.noresttherein.oldsql.sql.GroupJoin.withLeft]]
	  */
	def rewriteLeft[F <: GroupByClause]
	               (left :F)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.GeneralizedDiscrete],
	                         filterScribe :(left.Generalized By R) => SQLScribe[Generalized, left.Generalized By R])
			:WithLeft[F]

	override def columnCount :Int = left.columnCount + GroupByScope.defaultColumns(right.export[Unit]).size

	override type AppliedParam = WithLeft[left.AppliedParam]
	override type GeneralizedParamless = left.GeneralizedParamless By R
	override type Paramless    = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))



	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)



	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type SelectedFrom[+P <: NonEmptyRow] = WithLeft[left.SelectedFrom[P]]

	override def selectedFrom[F <: NonEmptyRow](prefix :F) :SelectedFrom[F] =
		withLeft(left.selectedFrom(prefix))(condition)

	override def appendedTo[F <: FromClause](prefix :F) :WithLeft[left.JoinedWith[F, NonParam]] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit By R
	override type Inner    = left.Inner By R
	override type Base     = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]
	override def base :Base = left.base


	override type Row = left.Row ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)


	override type AsSubselectOf[+P <: NonEmptyRow] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		rewriteLeft[newLeft.type](newLeft)(
			SQLScribe.shiftBack[GeneralizedDiscrete, newLeft.GeneralizedDiscrete](
				fromClause.generalized, newLeft.fromClause
			),
			SQLScribe.shiftBack[Generalized, newLeft.Generalized By R](generalized, _)
		)
	}



	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val preceding = spelling(left)(context, params.left)
		if (expr == True || GroupByScope.defaultColumns(right.export[Unit]).isEmpty) //zero columns in expr
			SpelledSQL(preceding.sql, preceding.setter, preceding.context.grouped)
		else {
			val ungroupedParams = params.ungroup[fromClause.Generalized]
			//we take here advantage of the fact that indexing of aliases in SQLContext is *not* changed by grouping it
			val group = spelling.grouping(grouping)(fromClause, preceding.context.grouped, ungroupedParams)
			val form = preceding.setter + group.setter
			if (group.isEmpty) //no columns - possible for component expressions for unbound parameters
				SpelledSQL(preceding.sql, form, group.context)
			else if (left.columnCount == 0) //no group by clause added
				SpelledSQL(preceding.sql + spelling._GROUP_BY_ + group.sql, form, group.context)
			else //append all columns of this grouping expression
				SpelledSQL(preceding.sql + ", " + group.sql, form, group.context)
		}
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[By.__]

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
	  * @param group a component expression from the mapping of one of the tables in the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  */
	def apply[F <: FromSome, L <: GroupingOfGeneralized[F], M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (from :L, group :ComponentSQL[F, M])(implicit cast :BaseMappingSubject[M, T, S]) :L By M =
		By[F, L, GroupByClause, M, T, S](from, group, True)

	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
	  * of the given component.
	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
	  *             the discrete, 'true' ''from'' clause grouped under this clause.
	  * @param group a component expression from the mapping of one of the tables in the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  */
	def apply[F <: FromSome, L <: GroupingOfGeneralized[F] { type Generalized <: G }, G <: GroupByClause,
	          M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (from :L, group :ComponentSQL[F, M], filter :GroupedBoolean[G By M])
	         (implicit cast :BaseMappingSubject[M, T, S]) :L By M =
	{
		val last = GroupingSQL[F, T, S](cast(group).anchor(from.fromClause))
		cast.back.join[from.type, By](By[from.type, T, S, Nothing](from, None)(last, cast.column(filter)))
	}
//		By[L, M, T, S](from)(group.anchor(from.fromClause :F).asGrouping, True)


	/** Adds a new grouping expression to the ''group by'' clause `G`, grouping it by the columns
	  * of the given component.
	  * @param from a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] with `F` as its
	  *             [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type -
	  *             the discrete, 'true' ''from'' clause grouped under this clause.
	  * @param group a component expression from the mapping of one of the relations from the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  * @param filter an optional condition for the ''having'' clause of the created ''select''.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `M`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `G By M`.
	  */
	def apply[L <: GroupByClause, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :L)
	         (group :GroupingRelation[from.GeneralizedDiscrete, M], filter :GroupedBoolean[from.Generalized By M] = True)
	         (implicit cast :BaseMappingSubject[M, T, S]) :L By M =
	{
		type FromLast = RowProduct AndBy T
		val relation = cast(group).anchor(from.fromClause)
		val last = GroupingSQL[from.GeneralizedDiscrete, FromLast, T, S](relation, 0)
		cast.back.join[from.type, By](By[from.type, T, S, Nothing](from, None)(last, cast.column(filter)))
	}



	private[sql] def apply[G <: GroupByClause, T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :G, asOpt :Option[A])
	                      (group :GroupingSQL[clause.GeneralizedDiscrete, RowProduct AndBy T, T, S],
	                       cond :GroupedBoolean[clause.Generalized By T])
			:G By T As A =
		new By[clause.type, T] with AbstractExpanded[clause.type, T, S] {
			override val left             = clause
			override val last             = group
			override val grouping         = last.grouping
			override val expr             = grouping.expr
			override val aliasOpt         = asOpt
			override val condition        = cond
			override val fromClause       = left.fromClause
			override val outer            = left.outer
			override val fullSize         = left.fullSize + 1
			override val parameterization :Parameterization[Params, Self] = left.parameterization.by(self).as(self)
			override def lastRelation     = last
			override lazy val tableStack     = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias = A
			override type WithLeft[+L <: GroupByClause] = L By T As A
			override type Self = left.Self By T As A
			override type NoAliasCopy = left.type By T
			override type Copy = left.type By T As A
			//todo: this cast completely invalidates the purpose of this being an anonymous class
			override def narrow :left.type By T As A = this.asInstanceOf[left.type By T As A]

			override def withCondition(filter :GroupedBoolean[Generalized]) =
				By[left.type, T, S, A](left, aliasOpt)(last, filter)

			override def withLeft[L <: GroupByClause { type GeneralizedDiscrete <: clause.GeneralizedDiscrete }]
			                     (left :L)(condition :GroupedBoolean[left.Generalized By T]) =
				By[L, T, S, A](left, aliasOpt)(last, condition)

			override def rewriteLeft[L <: GroupByClause]
			             (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.GeneralizedDiscrete],
			                       filterScribe :(left.Generalized By T) => SQLScribe[Generalized, left.Generalized By T]) =
			{
				val last = GroupingSQL[left.GeneralizedDiscrete, FromLast, T, S](
					grouping.copy(groupingScribe(expr)), 0
				)
				val unfiltered = By[left.type, T, S, A](left, aliasOpt)(last, True)
				if (condition == True)
					unfiltered
				else
					By[left.type, T, S, A](left, aliasOpt)(last, filterScribe(unfiltered.generalized)(condition))
			}

			override def aliased[N <: Label](alias :N) =
				By[left.type, T, S, N](left, Some(alias))(last, condition)

			override def bind(param :LastParam) :AppliedParam = {
				val l = left.bind(param)
				if (fromClause.lastParamOffset < 0)
					By[l.type, T, S, A](l, aliasOpt)(
						last.asInstanceOf[GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S]],
						condition.asInstanceOf[GroupedBoolean[l.Generalized By T]]
					)
				else {
					val expression =
						if (lastParamOffset < size) //the grouping expression cannot depend on the parameter as it comes after it
							last.asInstanceOf[GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S]]
						else {
							val groupingScribe = SQLScribe.applyParam(fromClause, l.fromClause :l.GeneralizedDiscrete, param)
							GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S](
								grouping.copy(groupingScribe(expr)), 0
							)
						}
					val unfiltered = By[l.type, T, S, A](l, aliasOpt)(expression, True)
					if (condition == True)
						unfiltered
					else {
						val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param)
						By[l.type, T, S, A](l, aliasOpt)(expression, substitute(condition))
					}
				}
			}

			override def bind(params :Params) :Paramless = {
				val l = left.bind(params)
				val substitute = {
					var count = paramCount - fromClause.paramCount
					var ungroupedParams :Chain = params
					while (count > 0) {
						ungroupedParams = ungroupedParams.asInstanceOf[Chain ~ Any].init
						count -= 1
					}
					SQLScribe.applyParams[GeneralizedDiscrete, l.GeneralizedDiscrete](fromClause, l.fromClause)(
						ungroupedParams.asInstanceOf[fromClause.Params]
					)
				}
				rewriteLeft[l.type](l)(substitute, SQLScribe.applyParams(self, _)(params))
			}

//			override def aliased[N <: Label](alias :N) = {
//				val table = last as alias
//				val swapped = SQLScribe.replaceRelation[T, S, T, S, Generalized, Generalized](
//					generalized, generalized, last, table
//				)(condition)
//				By[left.type, T, S, N](left, table, Some(alias))(swapped)
//			}

			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.by[G, T, S](this)

		}.asInstanceOf[G By T As A]



	/** Matches all `By` instances, splitting them into their left (preceding column groupings)
	  * and right (the last column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Relation[R])] = from match {
		case _ :By[_, _] => Got((from.left, from.right))
		case _ => Lack
	}

	/** Matches all `By` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(GroupByClause, Relation.__)] = from match {
		case group :By.__ => Got((group.left, group.right))
		case _ => Lack
	}


	//todo: can it incorporate aliased clauses the same way join does?
	implicit def byDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L By R, L, R, By, GroupByClause] =
		composition.asInstanceOf[ExpandedDecomposition[L By R, L, R, By, GroupByClause]]

	private[this] val composition =
		new ExpandedDecomposition[GroupByClause By MappingAt, GroupByClause, MappingAt, By, GroupByClause]






	/** Type alias for `By` with erased type parameters, covering all instances of `By`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = By[_ <: GroupByClause, T] forSome { type T[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.By By]] clauses introducing a grouping expression
	  * represented by mapping `R`.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome RightJoin R

	/** A curried type constructor for `By` instances, accepting the left `GroupByClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: GroupByClause] = { type F[R[O] <: MappingAt[O]] = L By R }

	/** A curried type constructor for `By` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `GroupByClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L By R }

}

