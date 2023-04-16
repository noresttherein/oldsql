package net.noresttherein.oldsql.sql

import scala.annotation.showAsInfix
import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.OperationView.GroupByView
import net.noresttherein.oldsql.collection.{Listing, Opt, PassedArray}
import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{Mapping, MappingExtract, Relation, SQLForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.PseudoRelation
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.IndexedMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.support.PatchedMapping
import net.noresttherein.oldsql.sql.AggregateFunction.{Avg, Count, Max, Min, StdDev, Sum, Var}
import net.noresttherein.oldsql.sql.Expanded.ExpandedDecomposition
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, JoinedMappings, NonEmptyRow, NonEmptyRowTemplate, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.{Single, Grouped}
import net.noresttherein.oldsql.sql.TypedColumnSQLMapping.GlobalTypedSQLColumn
import net.noresttherein.oldsql.sql.TypedSQLMapping.GlobalTypedSQLMapping
import net.noresttherein.oldsql.sql.ast.{ChainTuple, ComponentSQL, GroupingSQL, JoinedGrouping, JoinedParam, JoinedRelation, IndexedSQL, ParamSQL, RelationSQL, SelectColumn}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.mechanics.{CanGroup, CanSelect, RelationCount, RowProductVisitor, SpelledSQL, SQLNumber, SQLOrdering}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.mechanics.SQLNumber.SQLFraction

//implicits
import net.noresttherein.oldsql.slang._





/** A base type for all ''from'' clauses with a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupBy]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
trait GroupByClause extends NonEmptyRow with AggregateClause with GroupByClauseTemplate[GroupByClause, GroupByClause] {
	thisClause =>

	/** A conjunction of filter [[net.noresttherein.oldsql.sql.GroupByClause.condition conditions]]
	  * of all grouping joins since the rightmost [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] occurrence
	  * in this type.
	  * @return [[net.noresttherein.oldsql.sql.GroupByClause.filter filter]]
	  */
	def havingClause :GroupedBoolean[Generalized] = filter

//	override type Last[O <: RowProduct] = JoinedRelation[O, LastMapping]
	override type FromLast >: Generalized <: GroupByClause
	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override type Generalized >: Complete <: GroupByClause {
		type LastMapping[O]        = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast              = thisClause.FromLast
		type Generalized          <: thisClause.Generalized
		type GeneralizedDiscrete  <: thisClause.GeneralizedDiscrete
		type Explicit             <: thisClause.Explicit
		type Implicit             <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base                 <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: GroupByClause {
		type LastMapping[O]        = thisClause.LastMapping[O]
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast              = thisClause.FromLast
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type Implicit              = thisClause.Implicit
		type Base                  = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
		type OuterRow              = thisClause.OuterRow
	}

	override type NoAlias >: Self <: GroupByClause {
		type LastMapping[O]        = thisClause.LastMapping[O]
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast              = thisClause.FromLast
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type Implicit              = thisClause.Implicit
		type Base                  = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
		type OuterRow              = thisClause.OuterRow
	}

	override type Self <: GroupByClause {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type LastMapping[O]        = thisClause.LastMapping[O]
		type FromLast              = thisClause.FromLast
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type Inner                 = thisClause.Inner
		type Implicit              = thisClause.Implicit
		type Base                  = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
		type OuterRow              = thisClause.OuterRow
	}

	override type AppliedParam <: GroupByClause
	override type GeneralizedParamless >: Paramless <: GroupByClause
	override type Paramless <: BoundParamless //because GroupParam requires a GroupByClause on the left.
	override type BoundParamless = GroupByClause { type Params = @~ }

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this 'join'.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct#filter]]
	  */
	def condition :GroupedBoolean[Generalized]

	/** The number of columns in the whole ''group by'' clause. This assumes all component expressions use
	  * [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]] column set.
	  */

	def columnCount :Int

	/** Type constructor for the whole ''group by'' clause - that is all 'joins' appearing after the ungrouped,
	  * 'true' ''from'' clause `U`, starting with the [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] compound.
	  */ //consider: renaming to GroupOther/Group/Regroup. Currently unused, may not be that useful.
	override type Grouping[+U <: FromSome] <: GroupByClause

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */ //overridden for docs only //consider: renaming to GeneralizedUngrouped? Note that AggregateSQL.GeneralizedFrom = fromClause.Explicit
	override type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */ //overridden for docs only //consider: renaming to Ungrouped? Grouped? Groups? From?
	override type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}


	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */ //'from' conflicts with the 'from' method for creating subselects. 'ungrouped' is an extension method returning JoinedMappings[Discrete]
	override val fromClause :Discrete


	override def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.groupByHaving(this)(context, params)

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] = {
		val ctx = fromClause.spellingContext//.group(size).adapt()
		ctx.reset(groupings = size, parent = ctx.parent.map(_.adapted))
	}

	protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.groupByClause(this)


	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause(seal :Seal) :Unit = ()
}






object GroupByClause {

	/** Curried type constructors for mappings [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]
	  * and [[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]], representing
	  * SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] for the purpose in using in grouping
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation relations]]. It accepts the expression's
	  * value type (becoming the mapping's [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type),
	  * and provides two pairs of member types:
	  *   1. `Mapping[O] = SQLMapping[GlobalScope, S, O]` and its duplicate `M[O]` as a shorthand, and
	  *   2. `Column[O] = ColumnSQLMapping[GlobalScope, S, O]` and its equal shorthand `C[O]`.
	  */
	type Group[S] = {
		type M[O] = SQLMapping[S, O]
		type C[O] = ColumnSQLMapping[S, O]
		type Mapping[O] = SQLMapping[S, O]
		type Column[O]  = ColumnSQLMapping[S, O]
	}

	def unapply(f :RowProduct) :Opt[FromClause] = f match {
		case group :GroupByClause => Got(group.fromClause)
		case _ => Lack
	 }



	/** Extension methods for any ''group by'' clause `G` which require its static type for the appropriate return type.
	  * @tparam G this ''group by'' clause.
	  */  //Can't be AnyVal for now as it uses PDTs
	implicit class GroupByClauseExtension[G <: GroupByClause](val thisClause :G) /*extends AnyVal*/ {
		//this needs to be an extension rather than a NonEmptyRowTemplate subtype because of As:
		// GroupByClause As A would inherit the generic template trait, not parameterized with F As A
		import thisClause.fromClause.{FromLast, LastMapping}
		import thisClause.{Base, Complete, Discrete, Generalized, GeneralizedDiscrete, Self}

		/** Adds another expression (column or columns) to the [[net.noresttherein.oldsql.sql.GroupBy group by]]
		  * clause to this ''from'' clause. The expression is based on the last
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.CanGroup CanGroup]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.CanGroup$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]`[F, _, _, _, _]`
		  *               and [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]]`[F, _, _, _, _]`,
		  *             - any single column expressions [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]`[F, _]`,
		  *             - base [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F, _]`,
		  *           where type `F =:= this.GeneralizedDiscrete` is the 'true', ''from'' clause grouped by this
		  *           ''group by'' clause, and `O` is its some supertype, with the origin relation of the component
		  *           expression being the first relation following an abstract type (typically `FromSome`).
		  * @param expr a function accepting the last [[net.noresttherein.oldsql.sql.ast.JoinedRelation relation]]
		  *             of the grouped ''from clause'' clause (that is, the one directly preceding
		  *             [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]), and which returns either
		  *             a [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with a supertype of this clause
		  *             as its `Origin` type argument, or an [[net.noresttherein.oldsql.sql.SQLExpression SQL expression]]
		  *             based on this clause which will be used as the grouping expression. The expression may be
		  *             a [[net.noresttherein.oldsql.sql.ColumnSQL single column]], but it doesn't have to,
		  *             in which case all columns of the expression will be inlined in the ''group by'' clause
		  *             in the order defined by its [[net.noresttherein.oldsql.schema.SQLReadForm form]].
		  *             If the returned value is a a mapping `M[O] <: MappingAt[O]` or a
		  *             [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expression]]
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
		  *         [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] (including its column
		  *         subtypes), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.ByVal ByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.ByOne ByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def byLast[E](expr :JoinedRelation[FromLast, LastMapping] => E)
		             (implicit grouping :CanGroup[GeneralizedDiscrete, G, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.fromClause.last))

		/** Adds another expression (column or columns) to the [[net.noresttherein.oldsql.sql.GroupBy group by]]
		  * clause to this ''from'' clause.
		  * @tparam E an expression used for grouping, for which
		  *           a [[net.noresttherein.oldsql.sql.mechanics.CanGroup CanGroup]]
		  *           type class exist. The [[net.noresttherein.oldsql.sql.mechanics.CanGroup$ companion]]
		  *           object contains definitions for:
		  *             - `M <: `[[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]`[_, O]`, having an implicit
		  *               [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] (which exists
		  *               for all subtypes of `BaseMapping` taking the `Origin` type as its last type parameter),
		  *             - components of relations:
		  *               [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[F, _]`
		  *               and [[net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL ColumnMappingSQL]]`[F, _]`,
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
		  *                 a [[net.noresttherein.oldsql.sql.ast.ComponentSQL component expression]]
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
		  *         [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] (including its column
		  *         subtype), the mapping is the mapping type parameter of the component expression.
		  *         Otherwise a generic [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]]
		  *         (or [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] if `E` is
		  *         a [[net.noresttherein.oldsql.sql.ColumnSQL ColumnSQL]]) is used. In the latter case, the return type
		  *         is abbreviated as `F `[[net.noresttherein.oldsql.sql.ByVal ByVal]]` V`
		  *         (or `F `[[net.noresttherein.oldsql.sql.ByOne ByOne]]` V`), where `V` is the value type
		  *         of the expression `E`.
		  */
		def by[E](expr :JoinedMappings[Discrete] => E)
		         (implicit grouping :CanGroup[GeneralizedDiscrete, G, E]) :grouping.Result =
			grouping(thisClause, expr(thisClause.fromClause.mappings))

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
		      (implicit origin :C <:< MappingAt[O], belongs :GeneralizedDiscrete <:< O,
		       shift :RelationCount.In[O], projection :OriginProjection[C, S])
				:G By projection.WithOrigin =
		{ //todo: this can be perhaps cleaned up by replacing RelationCount with RelationOffset and using \
			val relation = thisClause.fromClause.fullTableStack(shift.offset).toRelationSQL.asInstanceOf[
				RelationSQL[GeneralizedDiscrete, MappingOf[Any]#TypedProjection, Any, From.Last[MappingOf[Any]#TypedProjection]]
			]
			val expr = TypedComponentSQL(relation, projection[GeneralizedDiscrete](component))(projection.isomorphism)
			By[thisClause.type, projection.WithOrigin, projection.WithOrigin, S](thisClause)(expr.asGrouping, True)
		}

		/** Expands this ''group by'' clause with all [[net.noresttherein.oldsql.schema.Mapping.selectable selectable]]
		  * columns of the given component expression based on the
		  * [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type of this clause.
		  * The component becomes available to the ''having'' and ''select'' clauses
		  * as an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] and can be accessed in the same way
		  * as the mappings of the joined tables.
		  */
		def by[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
		      (component :ComponentSQL[GeneralizedDiscrete, M])
		      (implicit cast :BaseMappingSubject[M, T, S]) :G By M =
			By[GeneralizedDiscrete, thisClause.type, M, T, S](thisClause, component)
//			By[thisClause.type, M, T, S](thisClause)(component.asGrouping, True)

		/** Expands this ''group by'' clause with the given single column expression. */
		def by[V](column :SingleColumn[GeneralizedDiscrete, V]) :G ByOne V =
			ByOne[GeneralizedDiscrete, thisClause.type, V](thisClause, column)

		/** Expands this ''group by'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.ast.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[net.noresttherein.oldsql.sql.ast.AdapterSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.ast.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.ast.ChainTuple tuples]] and
		  *     [[net.noresttherein.oldsql.sql.ast.IndexedSQL indexed tuples]].
		  */
		def by[V](expr :SingleSQL[GeneralizedDiscrete, V]) :G ByVal V =
			ByVal[GeneralizedDiscrete, thisClause.type, V](thisClause, expr)

		/** Expands this ''group by'' clause with all member columns of the given expression. */
		def by[V <: Listing](expr :IndexedSQL[GeneralizedDiscrete, Single, V]) :G By IndexedMapping.of[V]#Mapping =
			By[thisClause.type, IndexedMapping.of[V]#M, IndexedMapping.of[V]#M, V](thisClause)(
				GroupingRelation[GeneralizedDiscrete, IndexedMapping.of[V]#M, V](expr)
			)


		//consider: a NonEmptyRowExtension: the subselect methods are exact copy&paste from FromSomeExtension
		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the table given as a `Table` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :G Subselect R =
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
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :StaticTable[N, R])
		                     (implicit cast :BaseMappingSubject[R, T, S]) :G Subselect R As N =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based on this clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast()]] methods.
		  * @param other a `RowProduct` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order). The clause cannot be empty to enforce that the `Subselect` join
		  *              is actually applied and that any relations joined later will be part of the new subselect
		  *              rather than the currently most deeply nested select.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: NonEmptyRow](other :R) :other.SelectedFrom[G] =
			other.selectedFrom(thisClause)



		/** Creates a single column SQL `select count(*) from` with this instance as the from clause.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Int]]) :result.Select =
//			thisClause select Count()
			thisClause select (Count() :ColumnSQL[Generalized, Grouped, Int])

		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column
		  * for all groups in this ''group by'' clause.
		  * This translates to `select count(column) from this`.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]].
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :ColumnSQL[GeneralizedDiscrete, Grouped, _])
		         (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Int]]) :result.Select =
//			thisClause select Count(column)
			thisClause select (Count(column) :ColumnSQL[Generalized, Grouped, Int])

		/** Creates a single column SQL ''select'' counting all rows with non-null values in the given column
		  * for all groups in this ''group by'' clause.
		  * This translates to `select count(column) from this`.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]].
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Count]]
		  */
		def count(column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, _])
		         (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Int]]) :result.Select =
//			thisClause select Count(column(new JoinedMappings(thisClause.fromClause)))
			thisClause select (Count(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, Int])


		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               and the actual ordering is not influenced by the type class.
		  * @return an SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLOrdering[X])
				:result.Select =
		{
//			val min = Min(column); thisClause select min //todo: investigate if it persists in Scala 3
			val min = Min(column) :ColumnSQL[Generalized, Grouped, X]; thisClause select min
		}

		/** Creates a single column SQL ''select'' returning the smallest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general
		  *               and the actual ordering is not influenced by the type class.
		  * @return SQL expression representing `select min(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Min]]
		  */
		def min[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLOrdering[X])
				:result.Select =
		{
//			val min = Min(column(new JoinedMappings(thisClause.fromClause)))
			val min = Min(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, X]
			thisClause select min
		}


		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLOrdering[X])
				:result.Select =
		{
//			val max = Max(column); thisClause select max
			val max = Max(column) :ColumnSQL[Generalized, Grouped, X]; thisClause select max
		}

		/** Creates a single column SQL ''select'' returning the largest value of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a type with [[net.noresttherein.oldsql.sql.mechanics.SQLOrdering SQLOrdering]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  * @return an SQL expression representing `select max(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Max]]
		  */
		def max[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLOrdering[X])
				:result.Select =
		{
//			val max = Max(column(new JoinedMappings(thisClause.fromClause)))
			val max = Max(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, X]
			thisClause select max
		}


		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLNumber[X])
				:result.Select =
		{
//			val sum = Sum(column); thisClause select sum
			val sum = Sum(column) :ColumnSQL[Generalized, Grouped, X]; thisClause select sum
		}

		/** Creates a single column SQL ''select'' returning the sum of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select sum(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Sum]]
		  */
		def sum[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, X]], number :SQLNumber[X])
				:result.Select =
		{
//			val sum = Sum(column(new JoinedMappings(thisClause.fromClause)))
			val sum = Sum(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, X]
			thisClause select sum
		}


		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Avg]]
		  */
		def avg[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Avg.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val avg = Avg(column); thisClause select avg
			val avg = Avg(column) :ColumnSQL[Generalized, Grouped, Avg.Result]; thisClause select avg
		}

		/** Creates a single column SQL ''select'' returning the average of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select avg(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def avg[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
		          (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Avg.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val avg = Avg(column(new JoinedMappings(thisClause.fromClause)))
			val avg = Avg(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, Avg.Result]
			thisClause select avg
		}


		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.Var]]
		  */
		def variance[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
		            (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Var.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val variance = Var(column); thisClause select variance
			val variance = Var(column) :ColumnSQL[Generalized, Grouped, Var.Result]; thisClause select variance
		}

		/** Creates a single column SQL ''select'' returning the variance of values of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return a SQL expression representing `select var(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def variance[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
		            (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, Var.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val variance = Var(column(new JoinedMappings(thisClause.fromClause)))
			val variance = Var(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, Var.Result]
			thisClause select variance
		}


		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a column of any of the joined relations, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X](column :ColumnSQL[GeneralizedDiscrete, Grouped, X])
				     (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, StdDev.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val stddev = StdDev(column); thisClause select stddev
			val stddev = StdDev(column) :ColumnSQL[Generalized, Grouped, StdDev.Result]; thisClause select stddev
		}

		/** Creates a single column SQL ''select'' returning the standard deviation of the gaussian approximation
		  * of the value distribution of some column or a single column
		  * SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] for all row groups in this ''group by'' clause.
		  * Null values are ignored.
		  * @param column a function from a facade to the ungrouped ''from'' clause, providing access to the mappings
		  *               of its relations, returning any of their columns, or a single column
		  *               SQL [[net.noresttherein.oldsql.sql.ColumnSQL expression]] based on
		  *               `this.`[[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]]. The column
		  *               must be of a numeric type with [[net.noresttherein.oldsql.sql.mechanics.SQLNumber SQLNumber]]
		  *               type class. Note that the type class serves here solely to artificially restrict
		  *               allowed column types based on their scala counterparts and is not used in implementation.
		  *               In particular, it can guarantee neither complete correctness nor exhaustiveness in general.
		  *               It cannot be used to provide custom arithmetic.
		  * @return an SQL expression representing `select stddev(column) from this`.
		  * @see [[net.noresttherein.oldsql.sql.AggregateFunction.StdDev]]
		  */
		def stddev[X](column :JoinedMappings[GeneralizedDiscrete] => ColumnSQL[GeneralizedDiscrete, Grouped, X])
				     (implicit result :CanSelect[Complete, ColumnSQL[Generalized, Grouped, StdDev.Result]], number :SQLNumber[X])
				:result.Select =
		{
//			val stddev = StdDev(column(new JoinedMappings(thisClause.fromClause)))
			val stddev = StdDev(column(new JoinedMappings(thisClause.fromClause))) :ColumnSQL[Generalized, Grouped, StdDev.Result]
			thisClause select stddev
		}
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
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
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
		  * The artificial pseudo relation [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]]
		  * is best obtained using the [[net.noresttherein.oldsql.sql.?: ?:]] factory method from package `sql`:
		  * {{{
		  *     From(Critters) groupBy (_.last.species) as "species" param ?:[String] on (_.name === _) having {
		  *         t => t("species") === t(-1)
		  *     }
		  * }}}
		  * @param relation a pseudo relation dedicated to `ParamClause` joins, representing a future parameter
		  *                 of type `X`, which can be later accessed as any other mappings.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
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
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
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
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */ //the order of implicits is important to avoid a double definition
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F ByParam X As N =
			GroupParam(thisClause, (name.value :N) ?: form)

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
		  * @param relation a pseudo relation dedicated to `ParamClause` joins, representing a future parameter
		  *                 of type `X`, with its similarly synthetic `Mapping` being labeled with `N`,
		  *                 used at the same time for the suggested parameter name.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
		  * @see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.?]]
		  */
		@inline def param[N <: Label, X](relation :NamedParamRelation[N, X]) :F ByParam X As N =
			GroupParam(thisClause, relation)

	}




	/** Mixin trait for [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] implementations using `F`
	  * as their upper bound. It overrides standard implementations of copying methods from
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate RowProductTemplate]] - see their documentation
	  * for the reasons of their existence - and adds `having` methods which work the same way as their `where`
	  * counterparts, but accept a [[net.noresttherein.oldsql.sql.GroupedBoolean LocalBoolean]] rather than
	  * a [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]].
	  * @tparam U the non aliased supertype of this clause (typically equivalent to `this.Self`).
	  * @tparam F the self type of this clause produced by various copy methods. It is always either `U`, or `U As A`.
	  */
	trait GroupByClauseTemplate[+U <: GroupByClause, +F <: U] extends NonEmptyRowTemplate[U, F] {
		thisClause :F with GroupByClauseTemplate[U, F] =>

		override type Copy <: F {
			type LastMapping[O]        = thisClause.LastMapping[O]
			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast              = thisClause.FromLast
			type Generalized           = thisClause.Generalized
			type Complete              = thisClause.Complete
			type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
			type NoAlias             = thisClause.NoAlias
			type Params                = thisClause.Params
			type FullRow               = thisClause.FullRow
			type Explicit              = thisClause.Explicit
			type Inner                 = thisClause.Inner
			type Implicit              = thisClause.Implicit
			type Outer                 = thisClause.Outer
			type Base                  = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row                   = thisClause.Row
			type OuterRow              = thisClause.OuterRow
		}

		override def filtered[S >: Grouped <: Single](filter :SQLBoolean[Generalized, S]) :Copy //now accepts LocalSQL

		override type JoinFilter = Nothing

		override def filtered(condition :Nothing) :Nothing =
			throw new UnsupportedOperationException(s"$this.on")

		/** Creates a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  * instance's filter and the given `SQLBoolean`. The filter becomes the part of the ''having'' clause
		  * of the SQL ''select'' based on this clause or some its expansion. This works analogously to the
		  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method with the same signature on ungrouped clauses.
		  * @see [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having]]
		  */
		override def having(condition :GroupedBoolean[Generalized]) :F = filtered(condition)

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
		  *                  by [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.ungrouped ungrouped]] method.
		  *                  The expressions for these relations will be based however on the `GeneralizedDiscrete`
		  *                  type rather than this clause; they can be adapted for the required `this.Generalized` by
		  *                  passing them to an
		  *                  [[net.noresttherein.oldsql.sql.AggregateFunction aggregate function]],
		  *                  creating an [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] as the result.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */
		override def having(condition :JoinedMappings[F] => GroupedBoolean[Generalized]) :F =
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
		override def havingLast(condition :Last[FromLast] => GroupedBoolean[FromLast]) :F =
			having(condition(last))

	}



	/** A `RowProduct` of a top level, independent ''select'' with a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (is not a ''from'' clause of a dependent select
	  * of some other select). In order to conform naturally (rather than by refinement) to `OuterDiscreteForm`,
	  * the clause must be ''complete'', that is its static type must start with either
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] - rather than
	  * a wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to the level of
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  *
	  * This is a 'grouping' subtype of the more generic [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]].
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
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], but there is no actual subtype
	  * relationship between the two in the type system. A `GroundGroupByClause` will however always by a subtype of
	  * [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClause TopGroupByClause]], which groups all outer clauses,
	  * including those with unbound parameters, and [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]].
	  */
	type GroundGroupByClause = GroupByClause {
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I
		type Params = @~
	}


	
	/** A supertype of both [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * and [[net.noresttherein.oldsql.sql.By By]] (but not [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * - subtypes of `GroupByClause` which actually
	  * add a new [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]] to the clause.
	  * It is a lower-level interface existing to allow some additional generalization among these implementations,
	  * but is unlikely to be of use in application code. It is a counterpart
	  * of [[net.noresttherein.oldsql.sql.NonParam NonParam]] for `GroupByClause` subtypes.
	  *
	  * Note that a subtype of this type can in fact still represent an unbound parameter,
	  * but not in the form of a declaration of a new one, but rather a 'reimport' of one from
	  * the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]] clause, making it available
	  * to expressions based on this clause (and any extending it).
	  */
	@showAsInfix //consider: renaming to ByNonParam
	trait NonParamGrouping[+L <: RowProduct, R[O] <: MappingAt[O]]
		extends AndBy[L, R] with GroupByClauseTemplate[L NonParamGrouping R, L NonParamGrouping R]
	{ thisClause =>
		override type Last[O <: RowProduct] = JoinedGrouping[GeneralizedDiscrete, O, R]

		override type Generalized >: Complete <: (left.Generalized NonParamGrouping R) {
			type FromLast             = thisClause.FromLast
			type Generalized         <: thisClause.Generalized
			type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
			type Explicit            <: thisClause.Explicit
			type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
		}

		override type Complete >: NoAlias <: (left.Complete NonParamGrouping R) {
			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast              = thisClause.FromLast
			type Generalized           = thisClause.Generalized
			type Complete              = thisClause.Complete
			type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
			type Params                = thisClause.Params
			type FullRow               = thisClause.FullRow
			type Explicit              = thisClause.Explicit
			type Row                   = thisClause.Row
		}

		override type NoAlias >: Self <: (left.Self NonParamGrouping R) {
			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast              = thisClause.FromLast
			type Generalized           = thisClause.Generalized
			type Complete              = thisClause.Complete
			type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
			type Params                = thisClause.Params
			type FullRow               = thisClause.FullRow
			type Explicit              = thisClause.Explicit
			type Row                   = thisClause.Row
		}

		override type Self <: (left.Self NonParamGrouping R) {
			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast              = thisClause.FromLast
			type Generalized           = thisClause.Generalized
			type Complete              = thisClause.Complete
			type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
			type Params                = thisClause.Params
			type FullRow               = thisClause.FullRow
			type Explicit              = thisClause.Explicit
			type Inner                 = thisClause.Inner
			type Row                   = thisClause.Row
		}

		/** An expression appended to the ''group by'' clause of the SQL ''select'' generated from (or using)
		  * this instance. It can be of any type is not limited to a single
		  * [[net.noresttherein.oldsql.sql.ColumnSQL column]]. It cannot be however assumed that it translates
		  * to non-empty column list, as 'grouping' a [[net.noresttherein.oldsql.sql.FromSome from clause]]
		  * by its [[net.noresttherein.oldsql.sql.ast.JoinedParam unbound parameter]] (or an expression for a component
		  * of such a parameter) has only the effect of bringing it to the scope of SQL expressions based on this
		  * and expanding clauses with no additional effect.
		  *
		  * It is always initialized with
		  * `this.`[[net.noresttherein.oldsql.sql.GroupByClause.NonParamGrouping.grouping grouping]]`.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.expr expr]].
		  */
		val expr :SQLExpression[GeneralizedDiscrete, Single, last.Subject]

		/** A pseudo [[net.noresttherein.oldsql.schema.Relation relation]] wrapping an expression added
		  * to the ''group by'' clause by the implementing classes.
		  * It is the '[[net.noresttherein.oldsql.sql.Adjoin.right right]]' side of this type.
		  */
		val grouping :GroupingRelation[GeneralizedDiscrete, R]

		override def right :GroupingRelation[GeneralizedDiscrete, R] = grouping

		/** An SQL expression for a suffix of the ''group by'' clause represented by this type.
		  * This is ''not'' the actual [[net.noresttherein.oldsql.sql.GroupByClause.NonParamGrouping.expr expression]]
		  * for two reasons:
		  *   1. It must be adapted to [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] interface
		  *      in order to be used in SQL expressions based on this clause the same way
		  *      as [[net.noresttherein.oldsql.sql.ast.JoinedTable tables]]
		  *      from a [[net.noresttherein.oldsql.sql.Join join]];
		  *   1. The grouping expression, as created, is based
		  *      on the [[net.noresttherein.oldsql.sql.AggregateClause.fromClause ungrouped]] ''from'' clause of
		  *      this type, rather than this type, meaning its type signature of
		  *      [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[`[[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]`, `[[net.noresttherein.oldsql.sql.SQLExpression.Grouped Grouped]]`, _]`
		  *      is incompatible with the type of SQL expressions allowed in this clause:
		  *      `SQLExpression[`[[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]`, Grouped, _]`.
		  *
		  * True to its name, it is treated always
		  * as the last entry on the list and the expression `join.left.last` is an instance representing only
		  * the last relation in the prefix clause `join.left`. In particular, it is not a valid reference to a relation
		  * from the point of view of `join`. Its type parameter makes it incompatible for direct use
		  * in SQL expressions based on this clause and casting it will result in generating invalid SQL.
		  * You can however use the method [[net.noresttherein.oldsql.sql.Adjoin.lastAsIn lastAsIn]] in the following way:
		  * {{{
		  *     def secondLast[T1[O] <: MappingAt[O], T2 <: MappingAt[O]](from :RowProduct AndFrom T1 AndFrom T2) =
		  *         from.left.lastAsIn[RowProduct AndFrom T1 AndFrom T2]
		  * }}}
		  * The relation can be used in this way in expressions based on clauses containing this instance as a prefix
		  * (which where created by joining/adding additional [[net.noresttherein.oldsql.sql.Expanded Expanded]] links to it).
		  * Moreover, any two clauses with types sharing a suffix, can freely exchange relations from that suffix and,
		  * by transitivity, any SQL expressions based on the clause type representing that suffix,
		  * with the differing prefixes replaced with a wildcard or abstract type.
		  *
		  */
		override val last  :JoinedGrouping[GeneralizedDiscrete, FromLast, R]

		override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] =
			last.asIn[E]

		override type Params    = left.Params
		override type LastParam = left.LastParam

		override type DefineBase[+I <: RowProduct] = left.DefineBase[I]
		override def base :Base = left.base


		protected override def groupingSpellingContext[P](context :SQLContext[P], params :Parameterization[P, Generalized])
				:GroupingSpellingContext[P] =
			GroupingSpellingContext(grouping)(fromClause, context, params.ungroup)

	}

	object NonParamGrouping {
		type __ = NonParamGrouping[_ <: RowProduct, R] forSome { type R[A] <: MappingAt[A] }
	}






	/** A pseudo relation used by the ''group by'' clauses [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and
	  * [[net.noresttherein.oldsql.sql.By By]]. It wraps an SQL expression based on type `F` which, in reality,
	  * is always given as a subtype of [[net.noresttherein.oldsql.sql.FromSome FromSome]], but its bound is relaxed
	  * for improved interoperability in SQL expressions. From the point of view of SQL expressions, it works the same
	  * way as a [[net.noresttherein.oldsql.schema.Table Table]] in a ''from'' clause, introducing a new mapping `G`
	  * to the domain, but is treated differently when generating SQL: at the declaration site, the wrapped expression
	  * is printed in an inlined format, i.e. all columns of `G` are separated with a ',' and not surrounded by any text.
	  * In the use site (that is, in a ''select'' clause or a ''having'' clause), instead of referring to columns
	  * by name (an undefined property for columns of this relation's mapping), the expression is formatted as in
	  * the ''group by'' clause. As with other [[net.noresttherein.oldsql.schema.Relation relations]], it can be
	  * referred to as a whole expression with a [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]
	  * expression, or to a subexpression with a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
	  * based on a `JoinedGrouping` wrapping this instance.
	  *
	  * If [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.expr expr]]
	  * is a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] itself, then its mapping type is used
	  * as the row mapping type of this relation. Otherwise, some synthetic mapping implementation such
	  * as [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] is used to expose the individual columns
	  * of the expression together as a `Mapping`. In the former case, components of `G` can be used exactly
	  * as if it was introduced by a `Table` relation, and the generated SQL is the same as if it was used within
	  * an ungrouped ''from'' clause `F`, that is a list of column names (usually qualified with the originating table
	  * alias). For relations wrapping more generic expression types, columns may represent arbitrary SQL expressions
	  * and are always rendered in their full form wherever used.
	  *
	  * As a `GroupingRelation` encapsulates an expression based on clause `F`, each instance is tied to that clause.
	  * Instances provided by [[net.noresttherein.oldsql.sql.AndBy AndBy]] ''group by'' clauses cannot be freely
	  * reused in a context of another clause. Whenever a component of this relation is however used with a different
	  * ''from'' clause instance, it is [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.anchor anchored]],
	  * recreating an equivalent relation based on its expression anchored in that clause. When a component expression
	  * for this relation is being anchored in another clause, it is possible that the expression
	  * in the `GroupingRelation` at the same position is unrelated to this instance. In that case an attempt is made
	  * to anchor the component expression in that clause by transforming it on a column by column basis.
	  * @tparam F the type of the ''ungrouped'' ''from'' clause being the domain of the wrapped expression.
	  * @tparam G the mapping type of this relation - either a component created by the application for use
	  *           in its table schema, or some synthetic mapping: `SQLMapping` for composite expressions
	  *           or [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] when `expr`
	  *           is a `ComponentSQL` for a [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]].
	  *
	  */ //todo: move it to sql; todo: add a value type V and allow any MappingSQL[F, Single, M, V] to be a basis for the expression.
	sealed trait GroupingRelation[-F <: RowProduct, G[O] <: MappingAt[O]] extends PseudoRelation[G] {
		//todo: try to make it covariant in Scala 3 refactor - if successful, asGrouping could become a polymorphic method of SQLExpression
		type Subject = G[Unit]#Subject
		val expr :SQLExpression[F, Single, G[Unit]#Subject]

		def joined :JoinedGrouping[F, RowProduct AndBy G, G]

		private[sql] def upcast[M[O] <: TypedMapping[G[Unit]#Subject, O]] :GroupingRelation[F, M] =
			this.asInstanceOf[GroupingRelation[F, M]] //todo: remove this once we are covariant (and SQLExpression has member value type)

		/** A copy of this relation for an expression being an adaptation of
		  * `this.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.expr expr]] to a different domain
		  * ''from'' clause `R`. The argument expression must be of the same type (barring the `RowProduct`
		  * type parameter change) as the one in this instance (as given to the factory method).
		  */ //todo: replace this with SQLExpression.asGrouping
		@throws[IllegalArgumentException]("if the expression type is incompatible with this specific class.")
		def copy[R <: RowProduct](expr :SQLExpression[R, Single, G[Unit]#Subject]) :GroupingRelation[R, G]

		def anchor(from :F) :GroupingRelation[F, G] = expr.anchor(from) match {
			case same if same eq expr => this
			case anchored => copy(anchored)
		}

		override def withClause :WithClause = expr.outerWithClause

		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
			spelling.inGroupBy.sqlParamCount(expr)

		/** Formats the expression for use in a ''group by'' clause.
		  * This is the default spelling used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
		  */
		protected def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling.inGroupBy(expr)(from, context, params)

		private[oldsql] def defaultSpelling[P](spelling :SQLSpelling)
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			defaultSpelling(from, context, params)(spelling)


		/** Formats a component of this relation, that is an expression covering a subset of columns introduced
		  * by this relation to any ''group by'' clause. If the component's relation is not this instance,
		  * an attempt is made to verify if they are type-compatible. If successful, the expression is translated
		  * in terms of this relation on a column-by-column basis; otherwise an exception will be thrown.
		  * @param component a component with [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]
		  *                  of the same type as this instance, but not necessarily equal.
		  * @param from      the clause used for spelling, which must contain this instance at position
		  *                  `component.origin.position`.
		  * @param context   The list of relations in scope of the spelled component expression.
		  * @param params    Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                  parameters of the formatted SQL.
		  */ //consider: Table dispatches here to SQLSpelling, and has also a lower level defaultSpellingOf
		protected def spell[P, M[O] <: MappingAt[O]]
		                   (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] })
		                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val columns = spellExploded(component, false)(from, context, params).filter(!_.isEmpty)
			if (columns.isEmpty)
				SpelledSQL(context)
			else if (columns.sizeIs <= 1)
				columns.head
			else
				columns.reduce(_ +: ", " +: _)
		}

		private[oldsql] def spell[P, M[O] <: MappingAt[O]]
		                         (spelling :SQLSpelling)
		                         (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] })
		                         (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			spell(component)(from, context, params)(spelling)


		/** Formats all columns of a component of this relation, that is an expression covering a subset
		  * of columns introduced by this relation to any ''group by'' clause. If the component's relation
		  * is not this instance, an attempt is made to verify if they are type-compatible.
		  * If successful, the expression is translated in terms of this relation on a column-by-column basis;
		  * otherwise an exception will be thrown.
		  * @param component a component with [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]
		  *                  of the same type as this instance, but not necessarily equal.
		  * @param from      the clause used for spelling, which must contain this instance at position
		  *                  `component.origin.position`.
		  * @param context   The list of relations in scope of the spelled component expression.
		  * @param params    Necessary information about all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
		  *                  parameters of the formatted SQL.
		  */
		protected def spellExploded[P, M[O] <: MappingAt[O]]
		                           (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] },
		                            independent :Boolean)
		                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                           (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]]

		private[oldsql] def spellExploded[P, M[O] <: MappingAt[O]]
		                                 (spelling :SQLSpelling)
		                                 (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] },
		                                  independent :Boolean)
		                                 (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] =
			spellExploded(component, independent)(from, context, params)(spelling)


		//fixme: this should be a different method, or at least in line with SQLExpression - as is, the mappings are incompatible
//		override def sameAs(that :Relation.*) :Boolean = that match {
//			case grouping :GroupingRelation[_, G @unchecked] => expr isomorphic grouping.expr
//			case _ => false
//		}

		override def equivalent(that :Relation[MappingAt]) :Boolean = that match {
			case _ if this eq that => true
			case other :GroupingRelation[_, MappingAt] => expr equivalent other.expr
			case _ => false
		}
		override def identical(that :Relation[MappingAt]) :Boolean = that match {
			case _ if this eq that => true
			case other :GroupingRelation[_, G @unchecked] if other canEqual this => expr identical other.expr
			case _ => false
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :GroupingRelation[_, G @unchecked] if other canEqual this => expr == other.expr
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.getClass == getClass
		override def hashCode :Int = expr.hashCode

		override def refString :String = "group"
		override def toString :String = "|" + expr + "|"
	}



	object GroupingRelation {
		//todo: verify all calls that all grouping expressions are anchored in the from clause the relation is used in.
		def apply[F <: RowProduct, G[A] <: BaseMapping[S, A], S]
		         (component :ComponentSQL[F, G])(implicit project :IsomorphicProjection[G, S, F])
				:GroupingRelation[F, G] =
			if (component.origin.asTableSQL.isDefined)
				new GroupingComponentRelation[F, G, S](component)
			else if (component.origin.asParamSQL.isDefined)
				new GroupingParamRelation[F, G, S](component)
			else if (component.origin.asGroupingSQL.isDefined)
				new GroupingGroupingRelation[F, G, S](component)
			else
				throw new IllegalArgumentException(
					"Cannot create a grouping relation for a component " + component + " of unknown relation type " +
					component.origin + " :" + component.origin.getClass.getName + "."
				)

		def apply[F <: RowProduct, U[O] >: SQLMapping[S, O] <: BaseMapping[S, O], S]
		         (expression :SQLExpression[F, Single, S]) :GroupingRelation[F, U] =
			new GroupingExpressionRelation[F, U, S](expression)

		def apply[F <: RowProduct, U[O] >: ListingSQLMapping[S, O] <: BaseMapping[S, O], S <: Listing]
		         (expression :IndexedSQL[F, Single, S]) :GroupingRelation[F, U] =
			new GroupingListingRelation[F, U, S](expression)

		def apply[F <: RowProduct, U[O] >: ColumnSQLMapping[S, O] <: BaseColumn[S, O], S]
		         (expression :ColumnSQL[F, Single, S]) :GroupingRelation[F, U] =
			new GroupingColumnRelation[F, U, S](expression)

		def param[F <: RowProduct, G[A] <: BaseMapping[S, A], S]
		         (param :ComponentSQL[F, G])(implicit project :IsomorphicProjection[G, S, F]) :GroupingRelation[F, G] =
			new GroupingParamRelation[F, G, S](param)


		type __ = GroupingRelation[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }


		//todo: finalize semantics of isomorphic, homomorphic, equivalent, identical
		private abstract class AbstractGroupingRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                       (override val expr :SQLExpression[F, Single, S], template :G[F])
		                       (implicit projection :IsomorphicProjection[G, S, F])
			extends GroupingRelation[F, G]
		{
			override type Subject = S
			override def apply[O] :G[O] = projection(template)
			override def export[O] :TypedMapping[S, O] = projection(template)
			override lazy val joined = GroupingSQL[F, RowProduct AndBy G, G, S](this, 0)

			override lazy val toString :String = super.toString
		}


		/** A relation being a grouping expression for a component of a `JoinedRelation`.
		  * Default implementation is correct only for `JoinedTable` components.
		  * Note that while a [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]]
		  * cannot be [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter altered]]
		  * (or, rather, the result is no more a `GroupingRelation` and all but unusable),
		  * a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression used can,
		  * resulting in effectively altering the column set used both in the ''group by'' clause and
		  * by any `ComponentSQL` of this relation (that is, a 'reference' to a subset of columns represented
		  * by this grouping relation).
		  */
		private class GroupingComponentRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                                       (override val expr :ComponentSQL[F, G])
		                                       (implicit projection :IsomorphicProjection[G, S, F])
			extends AbstractGroupingRelation[F, G, S](expr, expr.mapping.withOrigin[F])
		{
			override def isDefault :Boolean = expr.isDefault
			override def export[O] :TypedMapping[S, O] = altered.anchored.withOrigin[O]
			private lazy val altered :ComponentSQL[F, G] = {
				//Eliminate the possibility of including non-default columns so that selected components
				// of this relation do not add columns which are not included in this relation (excluding is permitted).
				// we finalize the expression as the relation is always grounded in a from clause:
				// when another clause is used, the relation is recreated anchored in that clause.
				val origin = expr.origin.custom(PatchedMapping.prohibitIncluding(_, _)).`->makeFinal`
				expr.graft(origin) //this expression will not have its origin substituted when spelled
			}

			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) = expr match {
				//todo: allow isomorphic mappings (at least those which guarantee the same mapping type)
				case comp :ComponentSQL[R @unchecked, G @unchecked] if comp.mapping == this.expr.mapping =>
					new GroupingComponentRelation[R, G, S](comp)(projection.isomorphism[R])
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of GroupingRelation(" + this + ") for expression " + expr +
						": not a ComponentSQL instance for mapping " + this.expr.mapping + "."
					)
			}

			protected def anchored[P, M[O] <: MappingAt[O]]
			                      (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] })
					:SQLExpression[F, Single, _] =
			{
				val composed = altered.alterLike(component.origin)
				composed \ composed.mapping.counterpart(component.origin.mapping, component.mapping.refine)
			}

			override def spell[P, M[O] <: MappingAt[O]]
			                  (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] })
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				spelling(anchored(component))(from, context, params)

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = G[O] },
			                           independent :Boolean)
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) =
				spelling.explode(anchored(component), independent)(from, context, params)

			override def refString :String = expr.export.mappingName
		}


		/** A relation being a grouping expression for a component of a `JoinedParam`.
		  * This is not a declaration of a new unbound parameter; instead, it works as a `GroupingComponentRelation`
		  * for a parameter relation component, with the exception that it translates to empty SQL
		  * in the ''group by'' clause.
		  */
		private class GroupingParamRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                                   (override val expr :ComponentSQL[F, G])
		                                   (implicit projection :IsomorphicProjection[G, S, F])
			extends GroupingComponentRelation[F, G, S](expr)
		{
			type Start = expr.Subject
			override def export[O] :TypedMapping[S, O] = expr.anchored.withOrigin[O]

			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) = expr match {
//				case ComponentSQL( //todo: better reconciliation than relation equality
//					ParamSQL(this.expr.origin.relation, _),
//					MappingExtract(_, _, this.expr.extract.export)
//				) =>
//					new GroupingParamRelation[R, G, S](expr.asInstanceOf[ComponentSQL[R, G]])(projection.isomorphism)
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of GroupingRelation(" + this + ") for expression " + expr +
						" because it is not a ComponentSQL for the same (sub)parameter " + this.expr.mapping + "."
					)
			}

			override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
			                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				SpelledSQL(context)
			type End = expr.Subject
		}


		/** A relation being a grouping expression for a component of a `JoinedGrouping`.
		  * It covers a very niche case of using certain grouping expression columns inside
		  * a ''group by'' clause of a subselect of the ''select'' using `expr.origin.relation`.
		  */
		private class GroupingGroupingRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                                      (override val expr :ComponentSQL[F, G])
		                                      (implicit projection :IsomorphicProjection[G, S, F])
			extends GroupingComponentRelation[F, G, S](expr)
		{
			override def export[O] :TypedMapping[S, O] = expr.anchored.withOrigin[O]

			try {
				expr.origin.asInstanceOf[JoinedGrouping[F, expr.Origin, MappingOf[Any]#Projection]]
			} catch { case _ :ClassCastException =>
				throw new IllegalArgumentException(
					"Cannot export component expression " + expr + " to a group by clause as a grouping expression " +
						"because its origin is not a GroupingSQL."
				)
			}

			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) :GroupingComponentRelation[R, G, S] =
				expr match {
					case comp :ComponentSQL[R @unchecked, G @unchecked]
							if comp.origin.asGroupingSQL.isDefined && this.expr.mapping.getClass == comp.mapping.getClass
								&& (this.expr.mapping isomorphic comp.origin.mapping) =>
							new GroupingGroupingRelation[R, G, S](comp)(projection.isomorphism)
					case _ =>
						throw new IllegalArgumentException(
							"Cannot create a substitute copy of GroupingRelation(" + this + ") for expression " + expr +
								": not a ComponentSQL instance for mapping " + this.expr.mapping +
								" :" + this.expr.getClass.getName + "."
						)
				}
		}


		private abstract class AbstractGroupingExpressionRelation
		                       [F <: RowProduct, G[O] <: GlobalTypedSQLMapping[F, S, O],
		                        U[O] >: G[O] <: BaseMapping[S, O], S]
		                       (override val expr :SQLExpression[F, Single, S], template :G[F])
			extends GroupingRelation[F, U]
		{
			override type Subject = S
			override def row[O] :G[O] = projection(template)
			override def apply[O] :G[O] = projection(template)
			override def export[O] :TypedMapping[S, O] = projection(template)
			override lazy val joined = GroupingSQL[F, RowProduct AndBy U, U, S](this, 0)

			private[this] val projection = OriginProjection[G[F]].isomorphism[F]

			override def spell[P, M[O] <: MappingAt[O]]
			                  (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = U[O] })
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				component.origin match {
					case origin if origin.relation == this => //
						spelling(row[component.Origin].export(component.mapping.refine).expr)(from, context, params)
					case grouping :JoinedGrouping[_, component.Origin, component.Entity]
							if isDefault && component.isDefault && (grouping.relation.expr isomorphic expr) =>
						spelling(expr)(from, context, params)

					case _ => super.spell(component)(from, context, params) //resort to column-by-column spelling
				}

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = U[O] },
			                           independent :Boolean)
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
				component.mapping match {
					case column :TypedColumn[_, component.Origin] @unchecked =>
						if (export[component.Origin] contains column)
							PassedArray :+ spell(column)(from, context, params)
						else if (row isomorphic component.origin.mapping) {
							val index = row[component.Origin].columns.sureIndexOf(component.origin.mapping.export(column))
							val substitute = component.origin.mapping.columns(index)
							PassedArray :+ spell(substitute)(from, context, params)
						} else
							throw new IllegalArgumentException(
								"Cannot spell grouping column expression " + component + " of " + this +
								" as in " + from + " because the columns of the argument's origin mapping" +
								" are incompatible with this relation's columns: " + component.origin.mapping.columns +
								" (in the argument) and " + row.columns + " (in this)."
							)
					case mapping =>
						val scope = spelling.scope
						val grouping = component.origin.mapping
						val actualColumns =
							if (mapping == grouping && component.isDefault)  //the most common case
								scope.defaultColumns(export[component.Origin]).view //just use the full column set of this grouping
							else if (row == grouping)                        //typical, the expression is anchored in this relation
								if (component.isDefault)
									scope.defaultColumns(grouping, mapping).view
								else {
									val altered = component.origin.altered.withOrigin[component.Origin]
									val defaults = scope.defaultColumns(altered, mapping)
									grouping.columns.view.filter(c => defaults.indexOf(altered.export(c)) >= 0)
								}
							else { //remember that this.row == this.export
								val columns = export[component.Origin].columns
								if (!(columns isomorphic grouping.columns))
									throw new IllegalArgumentException(
										"Cannot spell grouping component expression " + component + " of " + this +
										" as in " + from + " because the columns of the argument's grouping expression" +
										" are incompatible with this relation's columns: " + grouping.columns +
										" (in the argument) and " + columns + " (in this)."
									)
								if (component.isDefault) //with no includes and excludes it is a 1-to-1 translation
									mapping.columns.view.map {
										c => columns(grouping.columns.sureIndexOf(grouping.export(c)))
									}.filter(scope.isDefault)
								else if (!component.isCustom && component.origin.includes.forall(_.isColumn) &&
								         component.origin.excludes.forall(_.isColumn))
									mapping.columns.view.map { defaultCol =>
										val compCol = grouping.export(defaultCol)
										(compCol, columns(grouping.columns.sureIndexOf(compCol)))
									}.collect {
										case (compCol, actualCol) //export 'nominal' column of component and its counterpart in this relation
										if scope.isDefault(actualCol) && !component.origin.excludes.contains(compCol)
										   || !scope.isProhibited(actualCol) && component.origin.includes.contains(compCol)
										=>
											actualCol
									}
								else { //fixme: this should pass the counterpart of mapping in row as an argument, but we don't know how to get it.
									val anchored = component.origin.anchor(this).anchored.withOrigin[Unit]
									val counterpart = anchored.subcomponents(grouping.subcomponents.sureIndexOf(mapping)) //todo: better substitution check
									scope.defaultColumns(anchored, counterpart).view.map {
										c => columns(anchored.columns.sureIndexOf(anchored.export(c)))
									}
								}
							}
						//we could optimise this if independent is true
						actualColumns.scanLeft(SpelledSQL(context)) {
							(sql, col) => spell(col)(from, sql.context, params)
						}.tail.to(ArraySeq)
				}

			protected def spell[P, X, O](column :TypedColumn[X, O])
			                            (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				spelling(row[O].export(column).expr)(from, context, params)

			override lazy val toString :String = super.toString
		}


		/** A relation for an arbitrary grouping expression. */
		private class GroupingExpressionRelation[F <: RowProduct, G[O] >: SQLMapping[S, O] <: BaseMapping[S, O], S]
		                                        (override val expr :SQLExpression[F, Single, S])
			extends AbstractGroupingExpressionRelation[F, TypedSQLMapping.c[F]#c[Single]#c[S]#project, G, S](
				expr, TypedSQLMapping[F, Single, S, F](expr, GroupByView)
			)
		{
			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) =
				new GroupingExpressionRelation[R, G, S](expr)
		}


		/** A relation for an arbitrary grouping expression. */
		private class GroupingListingRelation[F <: RowProduct,
		                                      G[O] >: ListingSQLMapping[S, O] <: BaseMapping[S, O], S <: Listing]
		                                     (override val expr :IndexedSQL[F, Single, S])
			extends AbstractGroupingExpressionRelation[F, TypedListingSQLMapping.c[F]#c[Single]#c[S]#apply, G, S](
				expr, TypedListingSQLMapping[F, Single, S, F](expr)
			)
		{
			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) = expr match {
				case listing :IndexedSQL[R, Single, S] =>
					new GroupingListingRelation[R, G, S](listing)
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a copy of grouping relation " + this + " for expression " + expr +
						" :" + expr.getClass.getName + " because it is not a IndexedSQL."
					)
			}
		}


		/** A relation for a single column grouping expression. */
		private class GroupingColumnRelation[F <: RowProduct, U[O] >: ColumnSQLMapping[S, O] <: BaseMapping[S, O], S]
		                                    (override val expr :ColumnSQL[F, Single, S])
			extends GroupingRelation[F, U]
		{
			override type Subject = S
			private[this] lazy val template = TypedColumnSQLMapping[F, Single, S, this.type](expr)
			private[this] val projection = OriginProjection[GlobalTypedSQLColumn[F, S, this.type]].isomorphism[this.type]

			override def apply[O] :GlobalTypedSQLColumn[F, S, O] = projection(template)
			override def row[O] :GlobalTypedSQLColumn[F, S, O] = projection(template)
			override def export[O] :TypedMapping[S, O] = projection(template)
			override lazy val joined = GroupingSQL[F, RowProduct AndBy U, U, S](this, 0)

			override def copy[R <: RowProduct](expr :SQLExpression[R, Single, S]) = expr match {
				case column :ColumnSQL[R, Single, S] =>
					new GroupingColumnRelation[R, U, S](column)
				case _ =>
					//don't need to check if mapping is TypedColumn because the public factory method of ComponentSQL
					// will always create a ColumnSQL in that case
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of a single column GroupingRelation(" + this +
						") for expression " + expr + " as it is not a ColumnSQL instance."
					)
			}

			override def spell[P, M[O] <: MappingAt[O]]
			                  (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = U[O] })
			                  (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                  (implicit spelling :SQLSpelling) =
				if (component.export.columns.size != 1) //or component.mapping, doesn't matter really
					throw new IllegalArgumentException(
						"Cannot spell grouping component expression " + component + " of " + this + " as in " + from +
						" because the grouping expression of the argument does not consist of one column: " +
						component.export.columns + "."
					)
				else
					spelling(expr)(from, context, params)

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M] { type Entity[O] = U[O] },
			                           independent :Boolean)
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
				PassedArray :+ spell(component)(from, context, params)

			override lazy val toString :String = super.toString
		}

	}

}






/** A common interface for all ''group by'' clause pseudo joins, that is all
  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] types which extend also
  * [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]. This clause adds a new relation
  * (be it a [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]] or other type)
  * to another `RowProduct` used as its 'left' side. It is featured
  * as their [[net.noresttherein.oldsql.sql.AndBy.FromLast FromLast]] type.
  *
  * This type has a type aliases [[net.noresttherein.oldsql.sql.AndByVal AndByVal]]
  * and [[net.noresttherein.oldsql.sql.AndByOne AndByOne]] which both take an arbitrary type `X`
  * as their second type parameter and parameterize this trait with a synthetic
  * [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]/[[net.noresttherein.oldsql.sql.ColumnSQLMapping ColumnSQLMapping]]
  * (respectively) of that subject type.
  * @see [[net.noresttherein.oldsql.sql.GroupBy]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.GroupParam]]
  */
@showAsInfix
trait AndBy[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Adjoin[L, R] with GroupByClause with GroupByClauseTemplate[L AndBy R, L AndBy R]
{ thisClause =>
	override type Last[O <: RowProduct] <: JoinedRelation[O, R] { type FromLast = RowProduct AndBy R }
	override type FromLast = RowProduct AndBy R

	override type Generalized >: Complete <: (left.Generalized AndBy R) {
		type Generalized         <: thisClause.Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
		type Explicit            <: thisClause.Explicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete AndBy R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
	}

	override type NoAlias >: Self <: (left.Self AndBy R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
	}

	override type Self <: (left.Self AndBy R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized           = thisClause.Generalized
		type Complete              = thisClause.Complete
		type GeneralizedDiscrete   = thisClause.GeneralizedDiscrete
		type LastParam             = thisClause.LastParam
		type Params                = thisClause.Params
		type FullRow               = thisClause.FullRow
		type Explicit              = thisClause.Explicit
		type Inner                 = thisClause.Inner
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                   = thisClause.Row
	}

	override type Implicit = left.Implicit
	override type Outer = left.Outer

	override type OuterRow = left.OuterRow

	override def outerRow[E <: RowProduct]
	             (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, Single, OuterRow] =
		left.outerRow(target)


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.sqlParamCount(left) + spelling.inGroupBy.sqlParamCount(right) +
			spelling.inHaving.sqlParamCount(condition)

	/** The context of the last (`this.last`) grouping expression for spelling purposes. Called from within
	  * [[net.noresttherein.oldsql.sql.RowProduct.groupingSpellingContext groupingSpellingContext]] if `position == 0`.
	  */
	protected def groupingSpellingContext[P](context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P]

	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P] =
		if (context.groupings != size)
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the " + position + "-th last relation in " +
				(typeString :String) + " because context " + context + " does not match this type (parameterization: " +
				params + ")."
			)
		else if (position == 0)
			groupingSpellingContext(context, params)
		else if (context.isSubselect) //leap to outer
			groupingSpellingContext(outer)(position - size, context.outer, params.outer[Implicit])
		else
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the " + position + "-th last relation in " +
				(typeString :String) + " because no context for the outer select is present in " + context +
				" (parameterization: " + params + ")."
			)

}


/** A matching pattern and a scope with standard type constructors
  * for [[net.noresttherein.oldsql.sql.AndBy! AndBy]] trait.
  * There are similar objects with contents mirroring the standard contents
  * of [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] companion objects, but accepting only
  * the subject type of the mapping as their right side:
  * [[net.noresttherein.oldsql.sql.AndByVal$ AndByVal]] and [[net.noresttherein.oldsql.sql.AndByOne$ AndByOne$]].
  */
object AndBy {

	/** Splits any `AndBy` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](join :L Adjoin R) :Opt[(L, Relation[R])] = join match {
		case join :AndBy[L @unchecked, R @unchecked] => Got((join.left, join.right))
		case _ => Lack
	}

	/** Matches all `AndBy` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(RowProduct, Relation.__)] = from match {
		case join :AndBy.__ => Got((join.left, join.right))
		case _ => Lack
	}


	/** An existential upper bound of all `AndBy` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = AndBy[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.AndBy AndBy]] instances grouping any clause
	  * by an expression represented by mapping `M`.
	  * It is the [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type of all grouping clauses
	  * adding `M` to the left clause.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct AndBy M

	/** [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type
	  * of all [[net.noresttherein.oldsql.sql.AndBy AndBy]] with mapping `M` as their right side.
	  */
	type AndByLast[M[O] <: MappingAt[O]] = RowProduct AndBy M

	/** A curried type constructor for `AndBy` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L AndBy R }

	/** A curried type constructor for `AndBy` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L AndBy R }

	type Last[R[O] <: MappingAt[O]] = RowProduct AndBy R
}
