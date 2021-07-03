package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.GroupByClause.{GroupByClauseTemplate, GroupingRelation}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroupingOfGeneralized, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.GroupByScope
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, GroupingSQL, JoinedGrouping, RelationSQL}
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] extension introducing a ''group by'' clause.
  * The appended [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation relation]] is not a database table,
  * view, or select as with [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F#Generalized, GlobalScope S]`, where type `S`
  * is the subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `G` of the right type parameter.
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
  */
trait GroupBy[+F <: FromSome, G[A] <: MappingAt[A]]
	extends Adjoin[F, G] with GroupByClause with GroupByClauseTemplate[F GroupBy G, F GroupBy G]
{ thisClause =>

	val expr           :SQLExpression[left.Generalized, GlobalScope, last.Subject]
	val grouping       :GroupingRelation[left.Generalized, G]// { type From <: left.Self }
	override def right :GroupingRelation[left.Generalized, G] = grouping
	override val last  :JoinedGrouping[left.Generalized, FromLast, G]

	override type FromLast    = FromSome GroupBy G
	override type Generalized = left.Generalized GroupBy G
	override type Dealiased   = left.Self GroupBy G
	override type Self       <: left.Self GroupBy G

	type DealiasedLeft[+L <: FromSome] = L GroupBy G
	type WithLeft[+L <: FromSome] <: L GroupBy G
	type LeftBound = FromSome

	def withLeft[L <: FromSome { type Generalized <: thisClause.left.Generalized }]
	            (left :L)(condition :LocalBoolean[left.Generalized GroupBy G]) :WithLeft[L]

	def rewriteLeft[L <: FromSome]
	    (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.Generalized],
	              filterScribe :(left.Generalized GroupBy G) => SQLScribe[Generalized, left.Generalized GroupBy G])
			:WithLeft[L]

	override def narrow :WithLeft[left.type]

	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''having'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `having` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	def withCondition(filter :LocalBoolean[Generalized]) :Copy

	override def columnCount :Int = GroupByScope.defaultColumns(right.export[Unit]).size


	override def filtered[S >: LocalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :Copy =
		withCondition(condition && filter)

	override def filter :LocalBoolean[Generalized] = condition

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :LocalBoolean[E] =
		condition.basedOn(target)


	override type LastParam = left.LastParam
	override type Params = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type GeneralizedParamless = left.GeneralizedParamless GroupBy G
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))

	override def isSubselectParameterized :Boolean = left.isSubselectParameterized

	override def lastParamOffset :Int = left.lastParamOffset match {
		case n if n < 0 => n
		case n if n < left.size => -1
		case n => n - left.size + 1
	}


	override def size :Int = 1
	override def fullSize :Int = outer.fullSize + 1

	override type FullRow = OuterRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.outerRow(target)(explicitSpan.expand(expansion)) ~ last.expand(target)


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type SelectedFrom[+P <: NonEmptyFrom] = WithLeft[left.SelectedFrom[P]]

	override def selectedFrom[P <: NonEmptyFrom](prefix :P) :SelectedFrom[P] =
		withLeft(left.selectedFrom(prefix))(condition)

	override def appendedTo[P <: FromClause](prefix :P) :JoinedWith[P, NonParam] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Grouping[+U <: FromSome] = U GroupBy G
	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupBy G
	override type Inner    = left.Inner GroupBy G
	override type Implicit = left.Implicit
	override type Outer    = left.Outer
	override type Base     = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
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
		rewriteLeft[newLeft.type](newLeft)(
			SQLScribe.shiftBack[GeneralizedDiscrete, newLeft.Generalized](left.generalized, newLeft.generalized),
			SQLScribe.shiftBack[Generalized, newLeft.Generalized GroupBy G](generalized, _)
		)
	}


	override def withClause :WithClause = left.withClause ++ right.withClause ++ condition.withClause


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val fromParams = params.fromClause[fromClause.Generalized]
		val fromSQL = left.spell(context, fromParams)(spelling.inFrom)
		if (columnCount == 0) {
			SpelledSQL(fromSQL.sql, fromSQL.context.grouped, fromSQL.setter)
		} else { //exprSQL non empty because columnCount > 0 (and ParamMapping has zero columns)
			val exprSQL = spelling.grouping(grouping)(left.self, fromSQL.context, fromParams)
			val sql = fromSQL.sql + spelling._GROUP_BY_ + exprSQL.sql
			SpelledSQL(sql, exprSQL.context.grouped, fromSQL.setter + exprSQL.setter)
		}
	}

	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
				:GroupingSpellingContext[P] =
		if (position == 0)
			GroupingSpellingContext(grouping)(fromClause, context.shrink(), params.fromClause)
		else
			groupingSpellingContext(left)(position + left.size - 1, context.shrink(), params.fromClause)



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
	         (from :F, group :ComponentSQL[U, M], filter :LocalBoolean[U GroupBy M] = True)
	         (implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
		GroupBy[F, M, T, S](from)(group.asGrouping, filter)

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
	private[sql] def apply[F <: FromSome, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                      (from :F)(group :GroupingRelation[from.Generalized, M],
	                       filter :LocalBoolean[from.Generalized GroupBy M])
	                      (implicit cast :BaseMappingSubject[M, T, S]) :F GroupBy M =
	{
		val last = GroupingSQL[from.Generalized, FromSome GroupBy T, T, S, FromSome GroupBy T](cast(group), 0)
		cast.back.join(GroupBy[F, T, S, Nothing](from, None)(last, cast.column(filter)))
	}


	private[sql] def apply[F <: FromSome, M[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :F, asOpt :Option[A])
	                      (group :GroupingSQL[clause.Generalized, FromSome GroupBy M, M, S, FromSome GroupBy M],
	                       cond :LocalBoolean[clause.Generalized GroupBy M])
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
			override val parameterization    = left.parameterization.group[Self, left.Self, M](self)
			override lazy val tableStack     = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias = A
			override type WithLeft[+L <: FromSome] = L GroupBy M As A
			override type Self = left.Self GroupBy M As A
			override type DealiasedCopy = left.type GroupBy M
			override type Copy = left.type GroupBy M As A

			override def narrow :left.type GroupBy M As A = this.asInstanceOf[left.type GroupBy M As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupBy[left.type, M, S, A](left, aliasOpt)(last, filter)

			override def withLeft[L <: FromSome { type Generalized <: clause.Generalized }]
			                     (left :L)(condition :LocalBoolean[left.Generalized GroupBy M]) =
				GroupBy[L, M, S, A](left, aliasOpt)(last, condition)

			override def rewriteLeft[L <: FromSome]
			                        (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.Generalized],
			                                  filterScribe :(left.Generalized GroupBy M)
				                                 => SQLScribe[Generalized, left.Generalized GroupBy M]) =
			{
				val last = GroupingSQL[left.Generalized, FromSome GroupBy M, M, S, FromSome GroupBy M](
					grouping.copy(groupingScribe(expr)), 0
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
						this.last.asInstanceOf[GroupingSQL[l.Generalized, FromSome GroupBy M, M, S, FromSome GroupBy M]],
						condition.asInstanceOf[GlobalBoolean[l.Generalized GroupBy M]]
					)
				else {
					val last = {
						val groupingScribe = SQLScribe.applyParam(left.self, l.generalized, param)
						GroupingSQL[l.Generalized, FromSome GroupBy M, M, S, FromSome GroupBy M](
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
				last.expand(target) #:: outer.fullTableStack(target)(explicitSpan.expand(expansion))


			override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
				last.expand(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def applyTo[Y](matcher :RowProductVisitor[Y]) = matcher.groupBy[F, M, S](this)

		}.asInstanceOf[F GroupBy M As A]



	/** Matches all `GroupBy` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Relation[R])] = from match {
		case _ :GroupBy[_, _] => Got((from.left, from.right))
		case _ => Lack
	}

	/** Matches all `GroupBy` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Relation.*)] = from match {
		case group :GroupBy.* => Got((group.left, group.right))
		case _ => Lack
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

}






/** The supertype of all clauses [[net.noresttherein.oldsql.sql.Expanded expanding]] ''group by'' clause `F`
  * with a relation using mapping `M`. As such, it is used in its own
  * [[net.noresttherein.oldsql.sql.AndFrom.FromLast FromLast]] type `GroupByClause AndBy M` as the supertype of
  * all ''group by'' clauses of more than one relation, ending with relation `M`. It has two standard implementations:
  * [[net.noresttherein.oldsql.sql.By By]] which uses an expression based on the ungrouped ''from'' clause
  * in `F` as the provider of the new relation, and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] which
  * introduces an unbound query parameter in the same way as [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
  * in non-grouped ''from'' clauses.
  *
  * This type has a type aliases [[net.noresttherein.oldsql.sql.AndByVal AndByVal]]
  * and [[net.noresttherein.oldsql.sql.AndByOne AndByOne]] which both take an arbitrary type `X`
  * as their second type parameter and parameterize this trait with a synthetic
  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]/[[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]
  * (respectively) of that subject type.
  */
trait AndBy[+F <: GroupByClause, M[A] <: MappingAt[A]]
	extends NonSubselect[F, M] with GroupByClause with GroupByClauseTemplate[F AndBy M, F AndBy M]
{ thisClause =>

	override type FromLast = GroupByClause AndBy M

	override type Generalized >: Dealiased <: (left.Generalized AndBy M) {
		type Generalized <: thisClause.Generalized
//		type Dealiased   >: Self <: Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self AndBy M) {
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type ParamsOnly  = thisClause.ParamsOnly
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self AndBy M) {
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type ParamsOnly  = thisClause.ParamsOnly
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}


	override def narrow :WithLeft[left.type]


	type GeneralizedLeft[+L <: GroupByClause] <: (L AndBy M)
	type DealiasedLeft[+L <: GroupByClause] <: GeneralizedLeft[L]
	type WithLeft[+L <: GroupByClause] <: DealiasedLeft[L]
	type LeftBound = GroupByClause

	/** Expands the given ''group by'' clause with the grouping expression carried by this instance.
	  * This is a trusted method which '''is not type safe''' and should not be made public.
	  */
	def withLeft[L <: GroupByClause { type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete }]
	            (left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]

//	def withLeft[L <: GroupByClause](left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]


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






/** An extension of a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause providing an additional
  * grouping expression. This type in any clause must be preceded by a similar
  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] type, which adapts
  * a [[net.noresttherein.oldsql.sql.FromClause discrete]] ''from'' producing individual rows into groups of rows.
  * The appended [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation relation]] is not a database table,
  * view, or select as with [[net.noresttherein.oldsql.sql.AndFrom AndFrom]], but a synthetic adapter for
  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, GlobalScope S]`, where type `S` is the
  * subject type of the [[net.noresttherein.oldsql.schema.Mapping mapping]] `G` of the right type parameter
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
  * [[net.noresttherein.oldsql.sql.AndBy.Discrete ungrouped]] ''from'' clause is not preserved in expressions
  * referencing this grouping expression, as [[net.noresttherein.oldsql.sql.AndFrom.FromLast FromLast]] domain type
  * used by [[net.noresttherein.oldsql.sql.By.last last]] grouping expression is `GroupByClause AndFrom G`,
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
trait By[+F <: GroupByClause, G[A] <: MappingAt[A]] extends AndBy[F, G] with GroupByClauseTemplate[F By G, F By G] {
	thisClause =>

	val expr           :SQLExpression[GeneralizedDiscrete, GlobalScope, last.Subject]
	val grouping       :GroupingRelation[GeneralizedDiscrete, G]
	override def right :GroupingRelation[GeneralizedDiscrete, G] = grouping
	override val last  :JoinedGrouping[GeneralizedDiscrete, FromLast, G]

	override type Generalized = left.Generalized By G
	override type Dealiased = left.Self By G
	override type Self <: left.Self By G

	override type GeneralizedLeft[+L <: GroupByClause] = L By G
	override type DealiasedLeft[+L <: GroupByClause]   = L By G
	override type WithLeft[+L <: GroupByClause]       <: L By G

	def rewriteLeft[L <: GroupByClause]
	    (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.GeneralizedDiscrete],
	              filterScribe :(left.Generalized By G) => SQLScribe[Generalized, left.Generalized By G]) :WithLeft[L]

	override def columnCount :Int = left.columnCount + GroupByScope.defaultColumns(right.export[Unit]).size

	override type ParamsOnly   = false
	override type LastParam    = left.LastParam
	override type Params       = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type GeneralizedParamless = left.GeneralizedParamless By G
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))



	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, G]) ~ last.expand(target)



	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type SelectedFrom[+P <: NonEmptyFrom] = WithLeft[left.SelectedFrom[P]]

	override def selectedFrom[P <: NonEmptyFrom](prefix :P) :SelectedFrom[P] =
		withLeft(left.selectedFrom(prefix))(condition)

	override def appendedTo[P <: FromClause](prefix :P) :WithLeft[left.JoinedWith[P, NonParam]] =
		withLeft(left.appendedTo(prefix))(condition)


	override type Explicit = left.Explicit By G
	override type Inner    = left.Inner By G
	override type Base     = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]
	override def base :Base = left.base


	override type Row = left.Row ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, G]) ~ last.expand(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		rewriteLeft[newLeft.type](newLeft)(
			SQLScribe.shiftBack[GeneralizedDiscrete, newLeft.GeneralizedDiscrete](
				fromClause.generalized, newLeft.fromClause
			),
			SQLScribe.shiftBack[Generalized, newLeft.Generalized By G](generalized, _)
		)
	}



	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val preceding = spelling(left)(context, params.left)
		if (expr == True || GroupByScope.defaultColumns(right.export[Unit]).isEmpty) //zero columns in expr
			SpelledSQL(preceding.sql, preceding.context.grouped, preceding.setter)
		else {
			val ungroupedParams = params.fromClause[fromClause.Generalized]
			//we take here advantage of the fact that indexing of aliases in SQLContext is *not* changed by grouping it
			val group = spelling.grouping(grouping)(fromClause, preceding.context.grouped, ungroupedParams)
			val form = preceding.setter + group.setter
			if (group.isEmpty) //no columns - possible for component expressions for unbound parameters
				SpelledSQL(preceding.sql, group.context, form)
			else if (left.columnCount == 0) //no group by clause added
				SpelledSQL(preceding.sql + spelling._GROUP_BY_ + group.sql, group.context, form)
			else //append all columns of this grouping expression
				SpelledSQL(preceding.sql + ", " + group.sql, group.context, form)
		}
	}


	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P] =
		if (position == 0)
			GroupingSpellingContext(grouping)(fromClause, context.shrink(size), params.fromClause)
		else if (position < size) //in this group by clause
			groupingSpellingContext(left)(position - 1, context.shrink(), params.left)
		else
			groupingSpellingContext(fromClause)(
				position - size + fromClause.size, context.shrink(size), params.fromClause
			)


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
	  * @param group a component expression from the mapping of one of the tables in the clause
	  *              `F =:= from.GeneralizedDiscrete`.
	  */ //todo: filter
	def apply[F <: RowProduct, G <: GroupingOfGeneralized[F], M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (from :G, group :ComponentSQL[F, M])(implicit cast :BaseMappingSubject[M, T, S]) :G By M =
		By[G, M, T, S](from)(group.asGrouping, True)


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
	private[sql] def apply[G <: GroupByClause, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                      (from :G)(group :GroupingRelation[from.GeneralizedDiscrete, M],
	                                filter :LocalBoolean[from.Generalized By M])
	                      (implicit cast :BaseMappingSubject[M, T, S]) :G By M =
	{
		type FromLast = GroupByClause AndBy T
		val relation = GroupingSQL[from.GeneralizedDiscrete, FromLast, T, S, FromLast](cast(group), 0)
		cast.back.join[from.type, By](By[from.type, T, S, Nothing](from, None)(relation, cast.column(filter)))
	}



	private[sql] def apply[G <: GroupByClause, T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :G, asOpt :Option[A])
	                      (group :GroupingSQL[clause.GeneralizedDiscrete, GroupByClause AndBy T, T, S, GroupByClause AndBy T],
	                       cond :LocalBoolean[clause.Generalized By T])
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
			override val parameterization = left.parameterization.group[Self, left.Self, T]
			override def lastRelation     = last
			override lazy val tableStack     = super.tableStack
			override lazy val fullTableStack = super.fullTableStack

			override type Alias = A
			override type WithLeft[+L <: GroupByClause] = L By T As A
			override type Self = left.Self By T As A
			override type DealiasedCopy = left.type By T
			override type Copy = left.type By T As A
			//todo: this cast completely invalidates the purpose of this being an anonymous class
			override def narrow :left.type By T As A = this.asInstanceOf[left.type By T As A]

			override def withCondition(filter :LocalBoolean[Generalized]) =
				By[left.type, T, S, A](left, aliasOpt)(last, filter)

			override def withLeft[L <: GroupByClause { type GeneralizedDiscrete <: clause.GeneralizedDiscrete }]
			                     (left :L)(condition :LocalBoolean[left.Generalized By T]) =
				By[L, T, S, A](left, aliasOpt)(last, condition)

			override def rewriteLeft[L <: GroupByClause]
			             (left :L)(groupingScribe :SQLScribe[GeneralizedDiscrete, left.GeneralizedDiscrete],
			                       filterScribe :(left.Generalized By T) => SQLScribe[Generalized, left.Generalized By T]) =
			{
				val last = GroupingSQL[left.GeneralizedDiscrete, FromLast, T, S, FromLast](
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
						last.asInstanceOf[GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S, FromLast]],
						condition.asInstanceOf[LocalBoolean[l.Generalized By T]]
					)
				else {
					val expression =
						if (lastParamOffset < size) //the grouping expression cannot depend on the parameter as it comes after it
							last.asInstanceOf[GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S, FromLast]]
						else {
							val substitute = SQLScribe.applyParam(fromClause, l.fromClause :l.GeneralizedDiscrete, param)
							GroupingSQL[l.GeneralizedDiscrete, FromLast, T, S, FromLast](
								grouping.copy(substitute(expr)), 0
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

			override def applyTo[Y](matcher :RowProductVisitor[Y]) = matcher.by[G, T, S](this)

		}.asInstanceOf[G By T As A]



	/** Matches all `By` instances, splitting them into their left (preceding column groupings)
	  * and right (the last column grouping) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Relation[R])] = from match {
		case _ :By[_, _] => Got((from.left, from.right))
		case _ => Lack
	}

	/** Matches all `By` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(GroupByClause, Relation.*)] = from match {
		case group :By.* => Got((group.left, group.right))
		case _ => Lack
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

