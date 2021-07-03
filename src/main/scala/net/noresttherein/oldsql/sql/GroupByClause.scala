package net.noresttherein.oldsql.sql

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.{Opt, ReversedList, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping, MappingExtract, Relation, SQLForm, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.Relation.PseudoRelation
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.ColumnSQL.GlobalColumn
import net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate
import net.noresttherein.oldsql.sql.ParamClause.{NamedParamRelation, ParamRelation}
import net.noresttherein.oldsql.sql.RowProduct.{As, JoinedMappings, NonEmptyFrom, NonEmptyFromTemplate, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, GlobalSQL, LocalScope}
import net.noresttherein.oldsql.sql.ast.{ComponentSQL, JoinedGrouping, JoinedRelation, ParamSQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.mechanics.{CanGroup, RowProductVisitor, SpelledSQL, TableCount}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//implicits
import net.noresttherein.oldsql.slang._





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
		type FromLast             = thisClause.FromLast
		type Generalized         <: thisClause.Generalized
//		type Dealiased   >: Self <: Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
		type Explicit            <: thisClause.Explicit
		type Implicit            <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base                <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: GroupByClause {
		type LastMapping[O]      = thisClause.LastMapping[O]
		type FromLast            = thisClause.FromLast
		type Generalized         = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type ParamsOnly          = thisClause.ParamsOnly
		type LastParam           = thisClause.LastParam
		type Params              = thisClause.Params
		type FullRow             = thisClause.FullRow
		type Explicit            = thisClause.Explicit
		type Implicit            = thisClause.Implicit
		type Base                = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                 = thisClause.Row
		type OuterRow            = thisClause.OuterRow
	}

	override type Self <: GroupByClause {
		type LastMapping[O]      = thisClause.LastMapping[O]
		type FromLast            = thisClause.FromLast
		type Generalized         = thisClause.Generalized
//		type Dealiased  >: Self <: Generalized
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type ParamsOnly          = thisClause.ParamsOnly
		type LastParam           = thisClause.LastParam
		type Params              = thisClause.Params
		type FullRow             = thisClause.FullRow
		type Explicit            = thisClause.Explicit
		type Inner               = thisClause.Inner
		type Implicit            = thisClause.Implicit
		type Base                = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                 = thisClause.Row
		type OuterRow            = thisClause.OuterRow
	}

	override type AppliedParam <: GroupByClause
	override type GeneralizedParamless >: Paramless <: GroupByClause
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

	/** The number of columns in the whole ''group by'' clause. This assumes all component expressions use
	  * [[net.noresttherein.oldsql.schema.Mapping.selectedByDefault selectedByDefault]] column set.
	  */ //consider: or should it be filteredByDefault? Depends on how comparing BLOBs works probably - must be consistent with SpellingScope.GroupByScope.
	def columnCount :Int


	/** Type constructor for the whole ''group by'' clause - that is all 'joins' appearing after the ungrouped,
	  * 'true' ''from'' clause `U`, starting with the [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] compound.
	  */
	override type Grouping[+U <: FromSome] <: GroupByClause

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */ //overriden for docs only //consider: renaming to GeneralizedUngrouped?
	override type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */ //overriden for docs only //consider: renaming to Ungrouped? Grouped? Groups? From?
	override type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}


	/** The ''from'' clause under grouping of this ''group by'' clause.
	  * For [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is its left side;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.from`.
	  */ //'from' conflicts with the 'from' method for creating subselects. 'ungrouped' is an extension method returning JoinedMappings[Discrete]
	override val fromClause :Discrete


	override def collect[X](fun :PartialFunction[SQLExpression.*, X]) :Seq[X] =
		fromClause.collect(fun) ++: row.collect(fun) ++: filter.collect(fun)



	override def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.groupByHaving(this)(context, params)

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Any] =
		fromClause.spellingContext.copy(groupings = size)

	protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Y = matcher.groupByClause(this)


	private[sql] def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException
}






object GroupByClause {

	type Group[S] = { //fixme: By/GroupBy are invariant in the mapping type, meaning this catches only expressions really declared with a Nothing type.
//		type E[O] = SQLMapping[F, GlobalScope, S, O]
//		type C[O] = ColumnSQLMapping[F, GlobalScope, S, O]
		type E[O] = BaseMapping[S, O]
		type C[O] = ColumnMapping[S, O]
	}

	def unapply(f :RowProduct) :Opt[FromClause] = f match {
		case group :GroupByClause => Got(group.fromClause)
		case _ => Lack
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
		  *                  by [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.ungrouped ungrouped]] method.
		  *                  The expressions for these relations will be based however on the `GeneralizedDiscrete`
		  *                  type rather than this clause; they can be adapted for the required `this.Generalized` by
		  *                  passing them to an
		  *                  [[net.noresttherein.oldsql.sql.AggregateFunction aggregate function]],
		  *                  creating an [[net.noresttherein.oldsql.sql.ast.AggregateSQL AggregateSQL]] as the result.
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
		import thisClause.{Discrete, Generalized, GeneralizedDiscrete}

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
		  *               [[net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL TypedComponentSQL]]`[F, _, _, _, _, O]`
		  *               and [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL TypedColumnComponentSQL]]`[F, _, _, _, _, O]`,
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
		  *               and [[net.noresttherein.oldsql.sql.ast.ColumnComponentSQL ColumnComponentSQL]]`[F, _]`,
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
		                shift :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
				:G By projection.WithOrigin =
		{
			val relation = thisClause.fromClause.fullTableStack(shift.offset).toRelationSQL
				.asInstanceOf[RelationSQL[GeneralizedDiscrete, MappingOf[Any]#TypedProjection, Any, GeneralizedDiscrete]]
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
			By[thisClause.type, M, T, S](thisClause)(component.asGrouping, True)

		/** Expands this ''group by'' clause with the given single column expression. */
		def by[V](column :GlobalColumn[GeneralizedDiscrete, V]) :G ByOne V =
			ByOne[GeneralizedDiscrete, thisClause.type, Generalized, V](thisClause, column)

		/** Expands this ''group by'' clause with all member columns of the given expression.
		  * The expression is traversed structurally until a [[net.noresttherein.oldsql.sql.ColumnSQL column expression]]
		  * is encountered, which is then added to the ''group by'' clause of the generated SQL.
		  * Not all possible expressions are supported; the expression may consist of
		  *   - any single [[net.noresttherein.oldsql.sql.ColumnSQL column expressions]] (atomic SQL values),
		  *     in particular [[net.noresttherein.oldsql.sql.ast.SQLTerm.ColumnTerm terms]],
		  *   - [[net.noresttherein.oldsql.sql.ast.ComponentSQL components]] (ranging from whole entities
		  *     to single columns),
		  *   - [[net.noresttherein.oldsql.sql.ast.ConversionSQL conversion]] nodes,
		  *   - any [[net.noresttherein.oldsql.sql.ast.CompositeSQL composites]] combining the above, in particular:
		  *   - [[net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple tuples]] and
		  *     [[ast.TupleSQL.ListingSQL indexed tuples]].
		  */
		def by[V](expr :GlobalSQL[GeneralizedDiscrete, V]) :G ByVal V =
			ByVal[GeneralizedDiscrete, thisClause.type, Generalized, V](thisClause, expr)


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
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on()]],
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]]
		  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast()]] methods.
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
		@inline def subselect[R <: NonEmptyFrom](other :R) :other.SelectedFrom[G] =
			other.selectedFrom(thisClause)

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
	  */ //todo: try to make it covariant in Scala 3 refactor - if successful, asGrouping could become a polymorphic method on SQLExpression
	sealed trait GroupingRelation[-F <: RowProduct, G[O] <: MappingAt[O]]
		extends PseudoRelation[G] //with RelationTemplate[G, ({ type R[+M[O] <: MappingAt[O]] = GroupingRelation[F, M] })#R]
	{
		type Subject = G[Unit]#Subject
		val expr :SQLExpression[F, GlobalScope, G[Unit]#Subject]

		def copy[R <: RowProduct](expr :SQLExpression[R, GlobalScope, G[Unit]#Subject]) :GroupingRelation[R, G]

		override def withClause :WithClause = expr.withClause

		/** Formats the expression for use in a ''group by'' clause.
		  * This is the default spelling used by [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]].
		  */
		protected def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
		                                (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			spelling.inline(expr)(from, context, params)

		private[oldsql] def defaultSpelling[P](spelling :SQLSpelling)
		                                      (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			defaultSpelling(from, context, params)(spelling)


		protected def spell[P, M[O] <: MappingAt[O]]
		                   (component :ComponentSQL[_ <: RowProduct, M])
		                   (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		{
			val columns = spellExploded(component)(from, context, params).filter(!_.isEmpty)
			if (columns.isEmpty)
				SpelledSQL(context)
			else if (columns.sizeIs <= 1)
				columns.head
			else
				columns.reduce(_ +: ", " +: _)
		}

		private[oldsql] def spell[P, M[O] <: MappingAt[O]]
		                         (spelling :SQLSpelling)(component :ComponentSQL[_ <: RowProduct, M])
		                         (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:SpelledSQL[P] =
			spell(component)(from, context, params)(spelling)


		protected def spellExploded[P, M[O] <: MappingAt[O]]
		                           (component :ComponentSQL[_ <: RowProduct, M])
		                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
		                           (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]]

		private[oldsql] def spellExploded[P, M[O] <: MappingAt[O]]
		                                 (spelling :SQLSpelling)(component :ComponentSQL[_ <: RowProduct, M])
		                                 (from :F, context :SQLContext[P], params :Parameterization[P, F])
				:Seq[SpelledSQL[P]] =
			spellExploded(component)(from, context, params)(spelling)


		override def sameAs(that :Relation.*) :Boolean = that match {
			case grouping :GroupingRelation[_, G @unchecked] => grouping.expr isomorphic expr
			case _ => false
		}
		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :GroupingRelation[_, G @unchecked] if other canEqual this => other.expr == expr
			case _ => false
		}
		override def hashCode :Int = expr.hashCode

		override def refString :String = ""
		override def toString :String = "(" + expr + ")"
	}



	object GroupingRelation {

		def apply[F <: RowProduct, G[A] <: BaseMapping[S, A], S]
		         (component :ComponentSQL[F, G])(implicit project :IsomorphicProjection[G, S, F])
				:GroupingRelation[F, G] =
			if (component.origin.asTableSQL.isEmpty)
				throw new IllegalArgumentException(
					"Cannot create a grouping relation for a component " + component + " of non-table relation " +
					component.origin + " :" + component.origin.getClass.getName + "."
				)
			else
				new GroupingComponentRelation[F, G, S](component)

		def apply[F <: RowProduct, U[O] >: SQLMapping[F, GlobalScope, S, O] <: BaseMapping[S, O], S]
		         (expression :SQLExpression[F, GlobalScope, S]) :GroupingRelation[F, U] =
			new GroupingExpressionRelation[F, U, S](expression)

		def apply[F <: RowProduct, U[O] >: ColumnSQLMapping[F, GlobalScope, S, O] <: BaseMapping[S, O], S]
		         (expression :ColumnSQL[F, GlobalScope, S]) :GroupingRelation[F, U] =
			new GroupingColumnRelation[F, U, S](expression)

		def param[F <: RowProduct, G[A] <: BaseMapping[S, A], S]
		         (param :ComponentSQL[F, G])(implicit project :IsomorphicProjection[G, S, F]) :GroupingRelation[F, G] =
			new GroupingParamRelation[F, G, S](param)


		type * = GroupingRelation[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }


		private abstract class AbstractGroupingRelation[F <: RowProduct, M[O] <: BaseMapping[S, O], S]
		                       (override val expr :SQLExpression[F, GlobalScope, S], template :M[F])
		                       (implicit projection :IsomorphicProjection[M, S, F])
			extends GroupingRelation[F, M]
		{
			override type Subject = S
			override def apply[O] :M[O] = projection(template)
			override def export[O] :MappingAt[O] = projection(template)

			override lazy val toString :String = "(" + expr + ")"
		}


		/** A relation for a grouping expression for a component of a `JoinedRelation`.
		  * Default implementation is correct only for `JoinedTable` components.
		  */
		private class GroupingComponentRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                                       (override val expr :ComponentSQL[F, G])
		                                       (implicit projection :IsomorphicProjection[G, S, F])
			extends AbstractGroupingRelation[F, G, S](expr, expr.mapping.withOrigin[F])
		{
			override def export[O] :MappingAt[O] = expr.export.withOrigin[O]

			override def copy[R <: RowProduct](expr :SQLExpression[R, GlobalScope, S]) = expr match {
				case comp :ComponentSQL[R, G @unchecked] if comp.mapping == this.expr.mapping =>
					new GroupingComponentRelation[R, G, S](comp)(projection.isomorphism[R])
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of GroupingRelation(" + this + ") for expression " + expr +
						": not a ComponentSQL instance."
					)
			}

			override def spell[P, M[O] <: MappingAt[O]](component :ComponentSQL[_ <: RowProduct, M])
			                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                                           (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			{
				val origin = expr.origin.alter(
					component.origin.includes.asInstanceOf[Unique[RefinedMapping[_, expr.Origin]]],
					component.origin.excludes.asInstanceOf[Unique[RefinedMapping[_, expr.Origin]]]
				)
				val actual = origin \ component.mapping.refine.withOrigin[expr.Origin] :SQLExpression[F, GlobalScope, _]
				spelling(actual)(from, context, params)
			}

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M])
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) =
			{
				val origin = expr.origin.alter(
					component.origin.includes.asInstanceOf[Unique[RefinedMapping[_, expr.Origin]]],
					component.origin.excludes.asInstanceOf[Unique[RefinedMapping[_, expr.Origin]]]
				)
				val actual = origin \ component.mapping.refine.withOrigin[expr.Origin] :SQLExpression[F, GlobalScope, _]
				spelling.explode(actual)(from, context, params)
			}

			override def refString :String = expr.export.mappingName
		}


		/** A relation for a grouping expression of a component of an `JoinedParam`.
		  * This is not a declaration of a new unbound parameter; instead, it works as `GroupingComponentRelation`
		  * for components of tables, with the exception that it translates to an empty SQL in the ''group by'' clause.
		  */
		private class GroupingParamRelation[F <: RowProduct, G[O] <: BaseMapping[S, O], S]
		                                   (override val expr :ComponentSQL[F, G])
		                                   (implicit projection :IsomorphicProjection[G, S, F])
			extends GroupingComponentRelation[F, G, S](expr)
		{
			if (!expr.origin.isInstanceOf[ParamSQL.*])
				throw new IllegalArgumentException(
					"Cannot export component expression " + expr + " to a group by clause as a parameter " +
						"because its origin is not a ParamSQL."
				)

			override def copy[R <: RowProduct](expr :SQLExpression[R, GlobalScope, S]) = expr match {
				case ComponentSQL(
					ParamSQL(this.expr.origin.relation, _),
					MappingExtract(_, _, this.expr.extract.export)
				) =>
					new GroupingParamRelation[F, G, S](expr.asInstanceOf[ComponentSQL[R, G]])
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of GroupingRelation(" + this + ") for expression " + expr +
						" because it is not a ComponentSQL for the same (sub)parameter " + this.expr.mapping + "."
					)
			}

			override def defaultSpelling[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
			                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				SpelledSQL()
		}


		/** A relation for an arbitrary grouping expression. */
		private class GroupingExpressionRelation[F <: RowProduct,
		                                         U[O] >: SQLMapping[F, GlobalScope, S, O] <: BaseMapping[S, O], S]
		                                        (override val expr :SQLExpression[F, GlobalScope, S])
			extends AbstractGroupingRelation[F, U, S](expr, SQLMapping(expr))(
				OriginProjection[U[F], S].isomorphism[F]
			)
		{
			override def row[O] :SQLMapping[F, GlobalScope, S, O] =
				super.row[O].castFrom[U[O], SQLMapping[F, GlobalScope, S, O]]

			override def copy[R <: RowProduct](expr :SQLExpression[R, GlobalScope, S]) =
				new GroupingExpressionRelation[R, U, S](expr)

			override def spell[P, M[O] <: MappingAt[O]](component :ComponentSQL[_ <: RowProduct, M])
			                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                                           (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				 //SQLMapping doesn't have any optional columns, so we ignore includes/excludes
				component.origin match {
					case origin if origin.relation.row == row =>
						spelling(row[component.Origin].export(component.mapping.refine).expr)(from, context, params)

					case grouping :JoinedGrouping[_, _, U @unchecked] //todo: this should be comparison ignoring adjustments
							if grouping.relation.expr == expr && grouping.includes.isEmpty && grouping.excludes.isEmpty =>
						spelling(expr)(from, context, params)

					case _ => super.spell(component)(from, context, params)
				}

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M])
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
			{
				val row = component.origin.export //todo: would be good to have some compatibility test as well
				//fixme: this will fail every time a ComponentSQL with an altered column set is a part of the tuple
				if (row.columns.size != this.row.columns.size)
					throw new IllegalArgumentException(
						"Cannot spell grouping component expression " + component + " of " + this + " as in " + from +
						" because the grouping expression of the argument has a different number of columns than " +
						"this grouping relation: " + this.row.columns + " (in this) and " + row.columns + " (in the argument)."
					)
				//note that a GroupingRelation has always export == mapping
				//fixme: remember about single column expressions, which currently will return empty lists here
				val exprColumns = spelling.scope.defaultColumns(row, component.export)
				val actualColumns = exprColumns.map(col => this.row[Unit].columns(row.columns.indexOf(col)))
				actualColumns.view.scanLeft(SpelledSQL(context)) {
					(sql, col) => spell(col)(from, sql.context, params)
				}.tail.to(ArraySeq)
			}

			def spell[P](column :ColumnMapping[_, Unit])(from :F, context :SQLContext[P], params :Parameterization[P, F])
			            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
				spelling(row[Unit].export(column).expr)(from, context, params)
		}


		/** A relation for a single column grouping expression. */
		private class GroupingColumnRelation[F <: RowProduct,
		                                     U[O] >: ColumnSQLMapping[F, GlobalScope, S, O] <: BaseMapping[S, O], S]
		                                    (override val expr :ColumnSQL[F, GlobalScope, S])
			extends AbstractGroupingRelation[F, U, S](expr, ColumnSQLMapping(expr))(
					OriginProjection[U[F], S].isomorphism[F]
			)
		{
			override def copy[R <: RowProduct](expr :SQLExpression[R, GlobalScope, S]) = expr match {
				case column :ColumnSQL[R, GlobalScope, S] =>
					new GroupingColumnRelation[R, U, S](column)
				case _ =>
					throw new IllegalArgumentException(
						"Cannot create a substitute copy of a single column GroupingRelation(" + this +
						") for expression " + expr + " as it is not a ColumnSQL instance."
					)
			}

			override def spell[P, M[O] <: MappingAt[O]](component :ComponentSQL[_ <: RowProduct, M])
			                                           (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                                           (implicit spelling :SQLSpelling) =
				if (component.export.columns.size != 1)
					throw new IllegalArgumentException(
						"Cannot spell grouping component expression " + component + " of " + this + " as in " + from +
						" because the grouping expression of the argument consists of more than one column: " +
						component.export.columns + "."
					)
				else
					spelling(expr)(from, context, params)

			override def spellExploded[P, M[O] <: MappingAt[O]]
			                          (component :ComponentSQL[_ <: RowProduct, M])
			                          (from :F, context :SQLContext[P], params :Parameterization[P, F])
			                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
				ReversedList :+ spell(component)(from, context, params)
		}

	}

}
