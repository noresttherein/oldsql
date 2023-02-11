package net.noresttherein.oldsql.sql.ast


import net.noresttherein.oldsql.collection.{Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{MappingExtract, Relation, SQLForm, Table}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, MappingAt, MappingOf, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.support.{AliasedMapping, PatchedMapping, ReorderedMapping}
import net.noresttherein.oldsql.schema.support.AlteredMapping.annulled
import net.noresttherein.oldsql.sql.{AndBy, AndByOne, AndByVal, AndFrom, ByParam, ColumnSetter, ColumnSQL, ComponentSetter, Expanded, FromSome, GroupBy, GroupByClause, RowProduct, Seal, Select, SQLExpression, WithClause, WithParam}
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.ParamClause.{ParamRelation, UnboundParam}
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, GroundRow, NonEmptyRow, PrefixOf, ProperSubselectOf, TopRow}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{AnyExpressionVisitor, Grouped, Single, SpecificExpressionVisitor}
import net.noresttherein.oldsql.sql.ast.ComponentSQL.{ComponentSQLTemplate, GenericComponentSQLTemplate, TypedComponentSQL}
import net.noresttherein.oldsql.sql.ast.GenericColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.GroupingSQL.{AnyGroupingVisitor, CustomGroupingSQL, SpecificGroupingVisitor}
import net.noresttherein.oldsql.sql.ast.JoinedRelation.{CustomRelationMixin, JoinedRelationTemplate}
import net.noresttherein.oldsql.sql.ast.LValueSQL.LValueTemplate
import net.noresttherein.oldsql.sql.ast.ParamSQL.{AnyParamVisitor, SpecificParamVisitor}
import net.noresttherein.oldsql.sql.ast.RelationSQL.{CustomRelationSQL, FinalRelationSQL}
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectAs, TopSelectAs}
import net.noresttherein.oldsql.sql.ast.TableSQL.{AnyTableVisitor, CustomTableSQL, SpecificTableVisitor}
import net.noresttherein.oldsql.sql.mechanics.{=~=, RelationCount, RelationOffset, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}
import net.noresttherein.oldsql.sql.AndFrom.AndFromLast

//here be implicits:
import net.noresttherein.oldsql.slang._






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] for a single instance/occurrence of a relation
  * used by a larger expression - most notably an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]],
  * in its ''from'' and ''select'' clauses), representing the whole [[net.noresttherein.oldsql.schema.Relation relation]]
  * as a column set. The relation may be a [[net.noresttherein.oldsql.sql.ast.JoinedTable table]],
  * but it can also be used in a more abstract capacity, for arbitrary lists of columns introduced together
  * bu a [[net.noresttherein.oldsql.sql.RowProduct clause]] (see [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]).
  * An example of this is a multi column [[net.noresttherein.oldsql.sql.ast.JoinedGrouping  expression]] occurring
  * in a [[net.noresttherein.oldsql.sql.GroupByClause group by]] clause (and possibly a ''select'' clause).
  * It performs five functions:
  *   1. it is a [[net.noresttherein.oldsql.sql.ast.ComponentSQL component]] expression for q complete entity type,
  *      translating to the full column set of a table [[net.noresttherein.oldsql.schema.Mapping mapping]]
  *      applicable in the context of use of this expression, as specified
  *      by their [[net.noresttherein.oldsql.schema.Buff buffs]];
  *   1. it is a factory of component expressions for components (and columns) of the mapping
  *      for the represented relation, represented as a subset of columns of this relation
  *      (see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.\ \]]);
  *   1. it constitutes a fragment of the domain `F`, that is a set of basic
  *      [[net.noresttherein.oldsql.sql.ColumnSQL column]] expressions which can be used by any SQL expression
  *      based on `F` - table column names and expressions from a ''group by'' clause;
  *   1. it identifies the origin of mapping `T` and its components - most notably a specific occurrence
  *      of a [[net.noresttherein.oldsql.schema.Table table]] in a ''from'' clause, but also any subset of columns
  *      coming from clause `F`;
  *   1. it defines as `F` the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the mapping
  *      for this relation and all its components, making it possible to identify on the type level
  *      the origin of any component and column, linking them to this particular `JoinedRelation` instance
  *      (because each `JoinedRelation` in a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] uses
  *      a different supertype `F` of that particular clause and, unlike other SQL expressions, is invariant in this type).
  *
  * The latter case allows a direct use of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] `T`
  * of this instance and all its components within SQL expressions.
  * is invariant in its domain type, which always starts with mapping `T` joined
  * with a [[net.noresttherein.oldsql.sql.RowProduct.WildcardRow wildcard]] clause, followed
  * by any number of relation mappings 'joined' with [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]]
  * [[net.noresttherein.oldsql.sql.Adjoin join types]].
  * When using a [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]]`[G]` to access the relations
  * included in clause `G`, or a [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[G]`
  * to access their mappings directly, is made available with a different domain/`Origin` type, specifically the
  * suffix of `G` starting with the mapping for that relation.
  * Thus, the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.index offset]]
  * of this relation/mapping in clause `G` equals the number of mappings listed in `F` minus one and can be counted
  * on the type level. This makes it possible to implement an implicit conversion
  * from any mapping `M <: `[[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[S, F]`
  * to a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]]`[G, M, S]` which, when an SQL expression
  * using it is [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in a particular instance of `G`,
  * can be replaced with a proper [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]].
  *
  *
  * This interface has a minimized signature and is primarily used by
  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] subclasses to represent each relation available to
  * expressions using the particular ''from'' clause as their domain. The relation object (both this value
  * and wrapped `Relation`) may be a relation only in the abstract sense, and not a valid table;
  * chief example here are grouping relations which represent a subset of attributes from a ''group by'' clause.
  * Instances exposed to the application always belong to one of its subclasses:
  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]] -
  *      for [[net.noresttherein.oldsql.schema.Table tables]] (including
  *      [[net.noresttherein.oldsql.schema.Table.DerivedTable derived tables]] such as SQL ''selects'')
  *      occurring within a ''from'' clause of an SQL ''select'' (that is, being a part of a
  *      [[net.noresttherein.oldsql.sql.FromClause FromClause]] instance);
  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]] -
  *      for grouping expressions, occurring in a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause,
  *      which can represent many individual grouping columns (i.e., proper SQL expressions used in the ''group by''
  *      clause) together, and can be used by SQL expressions used in the ''having'' and ''select'' clauses
  *      in the same way as `JoinedTable` in ungrouped SQL ''selects'';
  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]] - synthetic entities representing query/statement
  *      parameters of unknown values, which do not exist in the final SQL other than at reference locations
  *      in the form of '?' JDBC parameter placeholders.
  *
  * Note that, unlike other SQL expressions,  this type is invariant in its `RowProduct` domain `F`: this is because
  * it is used as the `Origin` type of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] `T[F]` for this relation.
  * As the `Origin` type of a mapping is shared by all its components, it allows matching any subcomponent
  * or column of this relation - or another instance, sharing `F` type parameter, and thus for the same relation `T`
  * occurring at the same position in the ''from''/''group by'' clause - back to the originating relation.
  * This is leveraged when a mapping instance is implicitly converted
  * to a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]], but the invariance is inconvenient
  * when using this instance as an actual expression. For this reason - and in order to use here the wider upper bound
  * of `Mapping`, rather than [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] actually
  * required to create instances of this trait by the implementation - every instance of `JoinedRelation`
  * is created for its subclass [[net.noresttherein.oldsql.sql.ast.RelationSQL RelationSQL]].
  * @tparam F $F
  * @tparam T $M
  * @define F The domain of of this expression, that is a ''from'' clause type (including any implicitly inherited
  *           ''from'' clauses of outer ''select'' expressions and an optional ''group by'' clause).
  *           It always starts with a wildcard type, such as `RowProduct` itself, 'joined' with the mapping `T`
  *           as the first (going left to right) relation. This will typically have the form of
  *           [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]` `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` T`
  *           if `T` represents an actual table listed by the ''from'' clause, or in general
  *           [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] member type of the particular
  *           [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] instance which introduced the relation
  *           ([[net.noresttherein.oldsql.sql.FromSome FromSome]]` `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` T`
  *           or [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]]` `[[net.noresttherein.oldsql.sql.AndBy AndBy]]` T`
  *           if `T` is an [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] for a grouping expression).
  * @define M A type constructor for the mapping for the whole relation, accepting its `Origin` type.
  */ //consider: I think we should move all non-spelling, non-comparison methods to JoinedRelationTemplate for consistency
trait JoinedRelation[F <: RowProduct, T[A] <: MappingAt[A]]
	extends ComponentSQL[F, T] //ComponentGroundingTemplate is extended directly only by JoinedRelation, not JoinedTable, etc.
	   with JoinedRelationTemplate[F, T, ({ type E[f <: RowProduct] = JoinedRelation[f, T] })#E]
{ //consider: a marking type to mark a relation as 'default'
	override val relation :Relation[T] //redeclared as val

	/** A view on the row mapping of this relation with any alterations defined by this instance, in particular
	  * any included/excluded optional components. This is a modification of
	  * `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]] (equal to
	  * `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.row row]]),
	  * and does not take into account that the relation may have been altered itself -
	  * its [[net.noresttherein.oldsql.schema.Relation.export export]] mapping is not used and will not be recognized
	  * as a component of this mapping (unless it is equal to the relation's nominal mapping).
	  *
	  * The mapping does not use this instance's [[net.noresttherein.oldsql.sql.ast.MappingSQL.Origin Origin]] type
	  * as its components should be used neither to create new component expressions, nor as arguments
	  * for `include`/`exclude` method family. It doesn't represent the definitive version of the row mapping
	  * of the underlying relation either, and thus doesn't allow determining of the specific column set to use
	  * in rendered SQL. For these reasons, it is unlikely to be of any use in any client code - it exists
	  * as an optimisation in the spelling process, with a purpose of avoiding repeated re-creation of the altered
	  * mapping for all spelled columns.
	  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation.mapping mapping]]
	  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]]
	  */
	override val altered  :TypedMapping[T[Unit]#Subject, F] //redeclared for docs

	/** The definitive mapping used for the whole relation, including any includes and excludes on this instance.
	  * This assumes that this instance is [[net.noresttherein.oldsql.sql.ast.ComponentSQL.isAnchored(from:F)* anchored]]
	  * in the [[net.noresttherein.oldsql.schema.Relation Relation]] actually included in the ''from'' clause `F`
	  * in which this instance is used; otherwise it will be replaced with the `RelationSQL` at the same position
	  * as this one when the expression is formatted as SQL.
	  *
	  * The mapping does not use this instance's [[net.noresttherein.oldsql.sql.ast.MappingSQL.Origin Origin]] type
	  * as its components should be used neither to create new component expressions, nor as arguments
	  * for `include`/`exclude` method family.
	  */
	override val anchored :TypedMapping[T[Unit]#Subject, F] //redeclared for docs

	override def isDefault :Boolean = includes.isEmpty && excludes.isEmpty

	/** An altered version of the argument relation expression representing a view of its
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] which applies the changes to the view
	  * defined by this instance (in particular, [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes included]]
	  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excluded]] optional components of `mapping`
	  * on top of those defined by the argument
	  * and its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.relation relation]]
	  * (the [[net.noresttherein.oldsql.schema.Relation.export export]] view of the relation).
	  * Only alterations defined directly by this `JoinedRelation` are taken into account,
	  * ignoring any potential difference between
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.row row]]
	  * and `relation.`[[net.noresttherein.oldsql.schema.Relation.export export]].
	  * The returned [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] will use the same
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] instance as `rel`,
	  * and its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] property
	  * will be a view of the mapping `rel.relation.row`, being compositions of alterations defined by
	  * `rel.relation` (i.e., `rel.relation.export`), `rel` and `this` (in that order),
	  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.altered altered]] property will be the composition
	  * of views defined by `rel` and `this`.
	  *
	  * Exact semantics of stacking are undefined: in particular, returned relation may not recognize components of
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mappings and
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]`.`[[net.noresttherein.oldsql.schema.Relation.export export]]
	  * of `this` and `rel`, with guarantees only for the components of the nominal mappings.
	  * This effect does not have to be restricted to the mapping of this component, but may involve global changes
	  * to the whole relation.
	  *
	  * This method is delegated to from method
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.alterLike alterLike]]
	  * of `ComponentSQL`, used primarily when an altered component expression must be evaluated in a context of
	  * an already altered corresponding relation in a spelled ''from'' clause, for example
	  * when spelling of a subcomponent of a component expression used in a ''group by'' clause.
	  * @param rel       A relation expression to which alterations should be applied.
	  * @param component A component expression for the counterpart of this relation's mapping within `rel.mapping`;
	  *                  [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] must equal
	  *                  the `rel` argument.
	  */ //todo: check if we can remove the refinement on FromLast, it doesn't seem to be doing anything
	def alterOther[C <: E, E <: RowProduct, M[A] <: MappingAt[A],
	               R[f <: RowProduct] <: JoinedRelation[f, M] with JoinedRelationTemplate[f, M, R]]
	              (rel :R[E], component :ComponentSQL[C, T] { type Origin = E; type Entity[A] = M[A] }) :R[E]

	override def isAnchored(clause :F) :Boolean =
		clause.fullTableStack(index).relation == relation

	//Note that the following grounding/anchoring methods return a JoinedRelation, not Rel:
	// this is because the same mapping type may be used in different clauses for different purposes,
	// especially a parameter with an entity type may be interchangeable with a table for that entity.
	/** Returns the relation in the same position in `clause` as this instance
	  * (based on its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.index index]] property).
	  */ //fixme: JoinedParam on type level it is interchangeable with JoinedTable because FromLast is the same
	override def anchor(clause :F) :JoinedRelation[F, T] = clause.relations(position)

	//todo: make a method in Relation and delegate to it. Probably use abstract JoinedRelation return type
//	override def anchor(relation :Relation[T]) :JoinedRelation[F, T] //can't be Rel because GroupingSQL

	/** Simply returns the given relation. */
	override def graft[E <: RowProduct](rel :JoinedRelation[E, T]) :JoinedRelation[E, T] = rel

//	override def basedOn[U <: F, E <: RowProduct]
//	                    (base :E)(implicit expansion :U PartOf E) :JoinedRelation[_ >: E <: RowProduct, T] =
//		expand(base)(expansion.asExpandedBy, implicitly[Single <:< Single])
//

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:JoinedRelation[_ >: E <: RowProduct, T] =
		base.relations(position + expansion.superPrefix[F])

	override def expand[E <: RowProduct]
	                   (implicit expansion :F ExpandedBy E) :JoinedRelation[_ >: E <: RowProduct, T] =
		asIn(expansion.superPrefix)

	def upcast :JoinedRelation[F, MappingOf[Any]#TypedProjection] =
		this.asInstanceOf[JoinedRelation[F, MappingOf[Any]#TypedProjection]]

	override def outerWithClause :WithClause = relation.withClause


	/** Returns the number of JDBC parameters in the given component expression.
	  * This is the implementation target of [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.sqlParamCount sqlParamCount]].
	  * @param component a component with this instance as its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]].
	  */
	protected def sqlParamCountOf[C <: F, M[O] <: MappingAt[O]]
                                 (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
                                 (implicit spelling :SQLSpelling) :Int =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot count JDBC parameters of component expression " + component +
				" because its origin is not this relation: " + this + "."
			)
		else 0

	private[sql] def sqlParamCountOf[C <: F, M[O] <: MappingAt[O]]
	                                (spelling :SQLSpelling)
                                    (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }) :Int =
		sqlParamCountOf(component)(spelling)

	/** Creates an SQL fragment referencing the given component of the mapping for this relation.
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of `alias.columnName`, while other components print the above form for all their columns picked by
	  *      `spelling.scope.`[[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.defaultColumns defaultColumns]];
	  *      whether these columns are surrounded by a parenthesis in order to form a tuple, or inlined for use
	  *      in a larger expression, is decided by `inline` flag.
	  *   1. [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' (or multiple, as with regular components) and return their form as
	  *      [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.setter setter]] property of the returned SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  *
	  * This method is used when formatting clauses using the relation, such as ''where'' and ''having'',
	  * but never declaration clauses such as ''from'' or ''group by''.
	  *
	  * Note that, since a `JoinedRelation` represents semantically a reference to the
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] declared at the given position
	  * in any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance conforming
	  * to its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.Origin Origin]] type, when rendering an SQL expression,
	  * it is always the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]] property
	  * of the counterpart of this relation in the `RowProduct` used as the ''from'' clause/domain
	  * that is called, rather than the instance included in the SQL expression being rendered.
	  * This can affect the selected column set (if a [[net.noresttherein.oldsql.schema.Relation relation]]
	  * has been altered using one if its method (such as `Loot(_.owner.-)`), or even the table used
	  * if more than one uses the same mapping. In particular, this means
	  * that [[net.noresttherein.oldsql.sql.ast.JoinedGrouping grouping]] relations will always use the expression(s)
	  * listed in the passed `from` argument, rather than the one carried by this instance (unless they are the same).
	  *
	  * All relation classes exposed to applications delegate this call to method
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.defaultSpellingOf defaultSpellingOf]] of the relation
	  * at `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]] in `from`.
	  * By default, this will eventually execute
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.explodedSpellingOf explodedSpellingOf]] and concatenate
	  * the results for individual columns, but subclasses are free to override this behavior.
	  *
	  * @tparam P the type of the parameter(s) of the whole formatted statement/query.
	  * @tparam C the domain [[net.noresttherein.oldsql.sql.RowProduct]] of the spelled expression,
	  *           a supertype of `from` and a subtype of this relation's origin type `F`.
	  * @tparam M the mapping type constructor for the formatted component, accepting its `Origin` type.
	  * @param component a component expression using `this` relation
	  *                  as its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] whose
	  *                  column set is to be spelled, according to the `SpellingScope` and buffs on
	  *                  its anchored counterpart.
	  * @param inline    a flag specifying if the component should be formatted as an SQL tuple
	  *                  (with listed columns surrounded by '(' and ')'), or exploded for use in a longer column list,
	  *                  such as a ''select'' or ''group by'' clause.
	  * @param from      the full ''from'' clause (that is, including an optional ''group by'' clause and any tables
	  *                  inherited from outer ''select'' expressions) used by the SQL ''select'' of which
	  *                  the component is a fragment.
	  * @param context   additional spelling information required for formatting the whole SQL select, in particular
	  *                  the list of aliases used by all listed tables.
	  * @param params    a factory of `P => X` accessor functions, where `P` is the intended parameter(s) type
	  *                  of the query being built.
	  * @param spelling  a formatting strategy used for all fragments of the returned SQL, responsible in particular
	  *                  for implementing any deviations from the SQL standard demanded by the DBMS being used.
	  */ //todo: add information about always(?) including the component/column, ignoring any `ExplicitXxx` buffs declared or inherited by it.
	@throws[IllegalArgumentException]("if component.origin is not this relation.")
	@deprecatedInheritance("the library assumes this method works as implemented and overriding it is likely to lead " +
	                       "to erroneous behaviour.", "0.0.1")
	protected def spell[P, C <: F, M[O] <: MappingAt[O]]
	                   (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }, inline :Boolean)
	                   (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                   (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a relation different than " + this + "."
			)
		else
			from.relations(position).defaultSpellingOf(component, inline)(from, context, params)

	/** Creates SQL fragments referencing the columns of the given component, returning them as a sequence.
	  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedTable Tables]] will format each column as ''alias.columnName'',
	  *      where ''alias'' is taken from the passed `context`
	  *      at `this`.[[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.position position]],
	  *      and ''columnName'' is the [[net.noresttherein.oldsql.schema.ColumnMapping.name name]] of the
	  *      ([[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]] column.
	  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]] will return a sequence
	  *      of "?" JDBC placeholders, one for each column/JDBC type set. If `independenent` is `true`,
	  *      then each of these SQL fragments must have an associated
	  *      write [[net.noresttherein.oldsql.schema.SQLWriteForm form]] setting that parameter ''alone'';
	  *      if it is impossible - which will be generally the case for a large group of arbitrary composite
	  *      values given by single a form - it must throw an [[UnsupportedOperationException]]. If `independent`
	  *      is `false` (which happens when the SQL fragments for each column are concatenated together,
	  *      for example joined with ", " alone, and not separated by any other JDBC parameter - this parameter
	  *      expression is allowed to return fragments with [[net.noresttherein.oldsql.schema.SQLWriteForm.empty empty]]
	  *      forms, with the exception for the last one, which would in that case carry the complete form for
	  *      the whole parameter.
	  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]] do not reference the expressions from
	  *      the ''group by'' clause ''per se'', but repeat it literally. Often, `component` will be the mapping
	  *      for the whole `JoinedGrouping`, rather than a proper component (for some grouping expressions
	  *      this is the only possibility), in which case the result will be the same
	  *      as the [[net.noresttherein.oldsql.sql.ast.JoinedGrouping.defaultSpelling spelling]] of the relation
	  *      in the ''group by'' clause. In other cases, subexpressions of
	  *      `this.`[[net.noresttherein.oldsql.sql.ast.JoinedGrouping.grouping grouping]]`.`[[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation.expr expr]]
	  *      for the individual columns will be formatted: either values at the given positions,
	  *      if the expression is a [[net.noresttherein.oldsql.sql.ast.InlineSQL.ChainTuple tuple]], or,
	  *      when grouping by a component/column of a table directly, the columns of the subcomponent of `this.mapping`
	  *      given as the `component` argument.
	  *
	  * Note that all `ComponentSQL` expressions
	  * are [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchor anchored]] in the
	  * formatted ''from'' clause `from` before spelling: the result will not reflect the state of mappings
	  * carried by this instance and `component`, but instead by the `JoinedRelation` (and thus, also `Relation`)
	  * in `from` at this relation's [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]],
	  * and the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.counterpart counterpart]]
	  * of `component.mapping` in
	  * `from.`[[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.relations relations]]`(this.position)`:
	  * it simply delegates to that relation's
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.explodedSpellingOf explodedSpellingOf]] method,
	  * which contains the actual implementation, specific to that relation's type. Most concrete `JoinedRelation`
	  * classes require the substitute relation to be of the same type.
	  *
	  * The actual column set is thus defined
	  * by the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]] version
	  * of `component.anchored(from)` (even if that component expression isn't created explicitly)
	  * and the implicitly passed spelling [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope scope]].
	  * It will reflect any changes applied by the `Relation` and `JoinedRelation` from `from` as well as
	  * this relation, stacking the included/excluded components as three separate operations.
	  *
	  * @tparam P the type of the parameter(s) of the whole formatted statement/query.
	  * @tparam C the domain [[net.noresttherein.oldsql.sql.RowProduct]] of the spelled expression,
	  *           a supertype of `from` and a subtype of this relation's origin type `F`.
	  * @tparam M the mapping type constructor for the formatted component, accepting its `Origin` type.
	  * @param component   a component expression using `this` relation
	  *                    as its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] whose
	  *                    column set is to be spelled, according to the `SpellingScope` and buffs on
	  *                    its anchored counterpart.
	  * @param independent specifies if the column SQL fragments will be used independently, or they will become all
	  *                    concatenated in the exact same order, separated by SQL fragments
	  *                    containing no JDBC parameters. If false, it allows the implementation to 'cheat' and
	  *                    include a combined [[net.noresttherein.oldsql.schema.SQLWriteForm setter form]] for all
	  *                    columns as part of the last returned fragment.
	  *                    For [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]], this can be a mandatory
	  *                    condition if the associated parameter form
	  *                    cannot be [[net.noresttherein.oldsql.schema.SQLWriteForm.split split]]; in other cases,
	  *                    it offers a possibility for optimisations, as individual forms can perform worse than
	  *                    a combined one for all columns of the component.
	  * @param from        the full ''from'' clause (that is, including an optional ''group by'' clause and any tables
	  *                    inherited from outer ''select'' expressions) used by the SQL ''select'' of which
	  *                    the component is a fragment. It is the counterpart of this relation instance in this
	  *                    clause that determines the exact spelling, rather than this instance.
	  * @param context     additional spelling information required for formatting the whole SQL select, in particular
	  *                    the list of aliases used by all listed tables.
	  * @param params      a factory of `P => X` accessor functions, where `P` is the intended parameter(s) type
	  *                    of the query being built.
	  * @param spelling    a formatting strategy used for all fragments of the returned SQL, responsible in particular
	  *                    for implementing any deviations from the SQL standard demanded by the DBMS being used.
	  */
	@throws[IllegalArgumentException]("if component.origin is not this relation.")
	@deprecatedInheritance("the library assumes this method works as implemented and overriding it is likely to lead " +
	                       "to erroneous behaviour.", "0.0.1")
	protected def spellExploded[P, C <: F, M[O] <: MappingAt[O]]
	                           (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                            independent :Boolean)
	                           (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                           (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a relation different than " + this + "."
			)
		else
			from.relations(position).explodedSpellingOf(component, independent)(from, context, params)


	/** Creates an SQL fragment referencing the given component of the mapping for this relation.
	  * This method is used when formatting clauses using the relation, such as ''where'' and ''having'',
	  * but never declaration clauses such as ''from'' or ''group by''.
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of ''alias.columnName'', where ''alias'' is taken from `context`
	  *      at `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]], and ''columnName''
	  *      is the name of the particular column's export version in
	  *      `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]].
	  *   1. [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' and append a setter form for the parameter
	  *      to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
	  *      returned with the SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  * In contrast to the 'entry' method of [[net.noresttherein.oldsql.sql.ast.JoinedRelation.spell spell]],
	  * this method actually formats the given component instance, without substituting it first for a corresponding one
	  * in `from`. See that method's documentation for more detailed information.
	  *
	  * The default implementation invokes
	  * `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.explodedSpellingOf explodedSpellingOf]]
	  * and concatenates the result, but can be overriden by subclasses.
	  */
	protected def defaultSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                               (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                inline :Boolean)
	                               (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                               (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val columns = explodedSpellingOf(component, false)(from, context, params).filter(_.nonEmpty)
		if (columns.isEmpty)
			SpelledSQL(context)
		else if (columns.sizeIs <= 1)
			columns.head
		else if (inline)
			columns.reduce(_ +: ", " +: _)
		else
			("(" +: columns.reduce(_ +: ", " +: _)) + ")"
	}

	/** Creates SQL fragments referencing all appropriate columns of the mapping of the given component
	  * as separate SQL fragments. It
	  * This method is used when formatting clauses using the relation, such as ''where'' and ''having'',
	  * but never declaration clauses such as ''from'' or ''group by''.
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of ''alias.columnName'', where ''alias'' is taken from `context`
	  *      at `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]], and ''columnName''
	  *      is the name of the particular column's export version in
	  *      `this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]].
	  *   1. [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' and append a setter form for the parameter
	  *      to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
	  *      returned with the SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  * In contrast to the 'entry' method of [[net.noresttherein.oldsql.sql.ast.JoinedRelation.spellExploded spellExploded]],
	  * this method actually formats the given component instance, without substituting it first for a corresponding one
	  * in `from`. See that method's documentation for more detailed information.
	  */
	protected def explodedSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                                (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                 independent :Boolean)
	                                (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                                (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]]

	//protected forwarders
	@inline private[sql] final def spell[P, C <: F, M[O] <: MappingAt[O]]
	                                    (spelling :SQLSpelling)
	                                    (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                     inline :Boolean = false)
	                                    (from :C, context :SQLContext[P], params :Parameterization[P, C]) :SpelledSQL[P] =
		spell(component, inline)(from, context, params)(spelling)

	@inline private[sql] final def spellExploded[P, C <: F, M[O] <: MappingAt[O]]
	                                            (spelling :SQLSpelling, independent :Boolean)
	                                            (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                                            (from :C, context :SQLContext[P], params :Parameterization[P, C])
			:Seq[SpelledSQL[P]] =
		spellExploded(component, independent)(from, context, params)(spelling)

	private[sql] final def defaultSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                                        (spelling :SQLSpelling)
	                                        (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                         inline :Boolean)
	                                        (from :C, context :SQLContext[P], params :Parameterization[P, C])
			:SpelledSQL[P] =
		defaultSpellingOf(component, inline)(from, context, params)(spelling)

	private[sql] final def explodedSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                                         (spelling :SQLSpelling, independent :Boolean)
	                                         (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                                         (from :C, context :SQLContext[P], params :Parameterization[P, C])
			:Seq[SpelledSQL[P]] =
		explodedSpellingOf(component, independent)(from, context, params)(spelling)



	/** Checks if this instance and the argument use the same [[net.noresttherein.oldsql.schema.Relation Relation]]
	  * and have the same offset. When comparing relations from the same
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], this attests that the two relations refer to
	  * the same alias in the ''from'' clause (or a grouping expression in the ''group by'' clause),
	  * ignoring `includes` and `excludes` lists which alter the column set for all components of this relation.
	  */
//	def same(that :JoinedRelation.*) :Boolean = relation == that.relation && index == that.index

	override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
		case self if self eq this => true //fixme: proper isomorphism
		case other :JoinedRelation.__ if canEqual(other) && other.canEqual(this) =>
			def sameComponents(left :Unique[TypedMapping[_, _]], right :Unique[TypedMapping[_, _]]) =
				left.size == right.size &&
					left.view.map(relation.row.subcomponents.sureIndexOf) ==
						right.view.map(other.relation.row.subcomponents.sureIndexOf)
			(index == other.index) && {
				relation == other.relation && includes == other.includes && excludes == other.excludes ||
					(relation isomorphic other.relation) && isDefault == other.isDefault && isCustom == other.isCustom &&
						sameComponents(includes, other.includes) && sameComponents(excludes, other.excludes)
			}
		case _ => false
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.__]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case relation :JoinedRelation.__ if (this canEqual relation) && (relation canEqual this) =>
			relation.index == index && relation.mapping == mapping &&
				relation.includes.toSet == includes.toSet && relation.excludes.toSet == excludes.toSet
		case _ => false
	}
	override def hashCode :Int = index * 31 + mapping.hashCode

	/** A textual representation of this relation longer than `toString`, which includes the complete formatted relation. */
	def fullString :String =
		if (isDefault)
			relation.toString + "@" + index
		else
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
				relation.toString + "@" + index + "(", ",", ")"
			)

	override lazy val toString :String =
		if (includes.isEmpty && excludes.isEmpty)
			relation.refString + "@" + index
		else
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
				relation.refString + "@" + index + "(", ",", ")"
			)


	private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL(seal :Seal) :Unit
}




object JoinedRelation {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :JoinedRelation.Typed[F, X] @unchecked => Got(from.relation -> from.index)
			case _ => Lack
		}


	@inline implicit def JoinedRelationExtension[F <: RowProduct, M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                                            (self :JoinedRelation[F, M])
	                                            (implicit subject :BaseMappingSubject[M, T, S])
			:JoinedRelationExtension[F, self.FromLast, T, S] =
		new JoinedRelationExtension[F, self.FromLast, T, S](subject(self))

	class JoinedRelationExtension[F <: RowProduct, L <: RowProduct, T[O] <: BaseMapping[S, O], S] private[ast]
	                             (private val self :JoinedRelation[F, T])
		extends AnyVal
	{
		def typed :RelationSQL[F, T, S, L]=
			self.asInstanceOf[RelationSQL[F, T, S, L]]
	}


	type __ = JoinedRelation[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type FromLast[M[O] <: MappingAt[O]] = JoinedRelation[RowProduct AndFrom M, M]
	type ByLast[M[O] <: MappingAt[O]] = JoinedRelation[RowProduct AndBy M, M]

	type AnyIn[F <: RowProduct] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedRelation[F, T] forSome { type T[O] <: TypedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = {
		type T[O <: RowProduct] = JoinedRelation[O, M]
		type InFrom[O <: RowProduct] = JoinedRelation[O, M] { type FromLast = RowProduct AndFrom M }
		type InGroupBy[O <: RowProduct] = JoinedRelation[O, M] { type FromLast = RowProduct AndBy M }
	}


	/** Interface extended by various [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] subtypes `Rel`,
	  * containing methods with signatures specific to `Rel`.
	  * @define this relation
	  * @define Cons `JoinedRelation`
	  * @define link [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  */
	trait JoinedRelationTemplate[F <: RowProduct, T[O] <: MappingAt[O],
	                             +Rel[f <: RowProduct] <: JoinedRelation[f, T] with JoinedRelationTemplate[f, T, Rel]]
		//note that the return type of the grounding methods is always JoinedRelation
		extends LValueTemplate[F, T, T[Unit]#Subject,
		                       ({ type R[f <: RowProduct] = JoinedRelation[_ >: f <: RowProduct, T] })#R, JoinedRelation[F, T]]
		   with GenericComponentSQLTemplate[F, T, ({ type R[f <: RowProduct] = JoinedRelation[f, T] })#R, Rel[F]]
	{ self :Rel[F] with JoinedRelationTemplate[F, T, Rel] =>
		override type Origin = F
		override type Entity[O] = T[O]
		override type FromLast <: RowProduct
		override val origin :Rel[F] = this

		/** `this.index` as an evidence class vouching that relation with row mapping `T` indeed occurs as the first one
		  * in `F`. This type is used as implicit evidence by many methods, in particular factories of `JoinedRelation`
		  * and `ComponentSQL` subclasses.
		  */
		val position :RelationOffset[F, T] { type First = FromLast }

		/** Index of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
		def index :Int = position.index

		/** A list of components of mapping `T`, in their [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * versions, which should be included in the rendered SQL for the full row of this table/relation,
		  * as long as they are allowed for use in that context by the buffs on their definitive versions
		  * (that is their export versions from the point of view of
		  * the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.export export]] version of the row mapping
		  * from the underlying relation).
		  *
		  * The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping of this expression
		  * is the `export` mapping of its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]
		  * additionally adjusted by passing [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] of this instance
		  * to its [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable[Component[_],exclude:Iterable[Component[_]) apply]]`(includes, excludes)`.
		  *
		  * The truly definitive ([[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]])
		  * version of the mapping for any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression
		  * using this relation as its origin is an ''export'' component of `this.anchored`, so the alteration
		  * of the column set is propagated to it.
		  *
		  * Note that `excludes` property of a `JoinedRelation` instance being
		  * an [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] of a
		  * component expression `ComponentSQL[F, M]` may contain any components of the row mapping,
		  * not only subcomponents of the component mapping `M`.
		  */
		def includes :Unique[TypedMapping[_, F]]

		/** A list of components of mapping `T`, in their [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * versions, which should be excluded from the rendered SQL for the full row of this table/relation,
		  * as long as they are optional in that context by the buffs of their definitive versions
		  * (that is their export versions from the point of view of
		  * the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.export export]] version of the row mapping
		  * from the underlying relation).
		  *
		  * The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping of this expression
		  * is the `export` mapping of its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]
		  * additionally adjusted by passing [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] of this instance
		  * to its [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable[Component[_],exclude:Iterable[Component[_]) apply]]`(includes, excludes)`.
		  *
		  * The truly definitive ([[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.anchored anchored]])
		  * version of the mapping for any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expression
		  * using this relation as its origin is an ''export'' component of `this.anchored`, so the alteration
		  * of the column set is propagated to it.
		  *
		  * Note that `excludes` property of a `JoinedRelation` instance being
		  * an [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.origin origin]] of a
		  * component expression `ComponentSQL[F, M]` may contain any components of the row mapping,
		  * not only subcomponents of the component mapping `M`.
		  */
		def excludes :Unique[TypedMapping[_, F]]


		/** Checks if this instance describes a database table (including table expressions),
		  * that is it is an instance of [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]].
		  */
		def isTable :Boolean = false //asTableSQL.isDefined

		/** Checks if this instance describes an unbound statement parameter,
		  * that is it is an instance, of [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]].
		  */
		def isParam :Boolean = false //asParamSQL.isDefined

		/** Checks if this instance describes an expression used within a ''group by'' clause,
		  * that is it is an instance, of [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]].
		  */
		def isGrouping :Boolean = false //asGroupingSQL.isDefined

		/** Casts down this relation to its implementation interface; this is a guaranteed safe cast. */
		def toRelationSQL :RelationSQL[F, M, T[F]#Subject, FromLast] //todo: ugly, we'd need FromLast[M] rather than FromLast for this
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

		/** Casts down this instance to a more strongly typed `RelationSQL`. The result type is an `Option`
		  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
		  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
		  * unable to abstract over existential higher type `T`).
		  */
		def asRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, FromLast]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]

		/** Casts this relation down to more strongly typed `TableSQL` if it is a table (persistent or derived). */
		def asTableSQL :Option[TableSQL[F, M, T[F]#Subject]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None

		/** Casts down this instance to more strongly typed `ParamSQL` if it is a pseudo relation for an unbound parameter. */
		def asParamSQL :Option[ParamSQL[F, T[F]#Subject, _ <: RowProduct]] =
			None

		/** Casts down this instance to more strongly typed `ParamSQL` if it is a pseudo relation representing an expression
		  * in a ''group by'' clause.
		  */
		def asGroupingSQL :Option[GroupingSQL[_ <: FromSome, F, M, T[F]#Subject]
				forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None

		override def default :Rel[F] =
			if (includes.isEmpty && excludes.isEmpty) this //origin
			else result[F](index, Unique.empty, Unique.empty)

		override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]]) :Rel[F] =
			result(index, includes.map(mapping.export(_)), excludes.map(mapping.export(_)))

		override def include(components :Iterable[TypedMapping[_, F]]) :Rel[F] =
			if (components.isEmpty)
				this //origin
			else {
				val newIncludes = components.view.map(mapping.export(_)).to(Unique)
				result(includes ++ newIncludes, excludes.filter(!annulled(newIncludes)(_)))
			}

		override def exclude(components :Iterable[TypedMapping[_, F]]) :Rel[F] =
			if (components.isEmpty)
				this //origin
			else {
				val newExcludes = components.view.map(mapping.export(_)).to(Unique)
				result(includes.filter(!annulled(newExcludes)(_)), excludes ++ newExcludes)
			}

		override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]]) :Rel[F] =
			if (includes.isEmpty && excludes.isEmpty)
				this //origin
			else {
				val newIncludes = includes.view.map(mapping.export(_)).to(Unique)
				val newExcludes = excludes.view.map(mapping.export(_)).to(Unique)
				if (isDefault)
					result(newIncludes.filter(!annulled(newExcludes)(_)), newExcludes)
				else
					result(
						(this.includes.view ++ newIncludes).filter(!annulled(newExcludes)(_)).to(Unique),
						(this.excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
					)
			}

		//We could implement in directly like reorder, but for now the implementation from GenericComponentSQLTemplate
		// should be enough.
//		override def alter(columns :Unique[TypedColumn[_, F]]) :Rel[F]



		override def alterLike[E <: RowProduct](template :JoinedRelation[E, T]) :Rel[F] = {
			type R[f <: RowProduct] = Rel[f] { type FromLast = self.FromLast }
			template.typed.alterOther[F, F, T, R](self, self)
		}


		override def aliased(aliases :Map[TypedColumn[_, F], String]) :Rel[F] =
			custom { (row, counterpart) =>
				val refined = row.withOrigin[F]
				if (row == mapping || row == altered || row == anchored ||
					counterpart == mapping || counterpart == altered || counterpart == anchored ||
					aliases.forall(entry => refined.contains(entry._1))
				)
					AliasedMapping(refined, aliases)
				else if (counterpart isomorphic mapping) {
					val counterparts = aliases.map { entry => (counterpart.counterpart(mapping, entry._1), entry._2) }
					AliasedMapping(row, counterparts)
				} else
					throw new IllegalArgumentException(
						"Cannot adapt " + refined + " by adding column aliases " + aliases +
						" because the given counterpart " + counterpart + " of " + mapping + ", the mapping of " +
						origin.fullString + ", is not isomorphic with it."
					)
			}

		//todo: in Scala3, extract the bulk of these two methods into a single implementation (requires generic functions)
		override def reorder(permutation :IndexedSeq[Int]) :Rel[F] =
			if (!isCustom && permutation == permutation.indices)
				this
			else {
				ReorderedMapping.validatePermutation(mapping, permutation)
				custom { (relation, counterpart) =>
					if (relation == counterpart) //this will also cover relation == mapping
						relation.reorder(permutation)
//					ReorderedMapping[TypedMapping[Any, Unit], Any, Unit](relation, permutation)
					else {
						val nominal = //originCounterpart == mapping also checks for relation == mapping, which is the important case here
							if (counterpart == mapping || relation.contains(mapping.withOrigin[Unit]))
								mapping.refine.withOrigin[Unit]
							else
								counterpart.original
						val export = relation.export(nominal)
//						val reordered = ReorderedMapping[TypedMapping[S, Unit], S, Unit](export, permutation)
						val reordered = export.reorder(permutation)
						val substitution = PatchedMapping.Overrides(export, reordered)
						PatchedMapping[TypedMapping[Any, Unit], Any, Unit](relation, substitution)
					}
				}
			}

		/** A `JoinedRelation` is ''custom'' if its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]]
		  * mapping is created in a non standard way. This flag opens the way for implementing expressions
		  * which represent views of its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]
		  * not limited to specifying lists of optional components
		  * to [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes include]]
		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes exclude]] to one of the standard altering
		  * methods of [[net.noresttherein.oldsql.schema.Relation Relation]].
		  * Returning `true` implies `!this.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.isDefault isDefault]].
		  * @return `false` for all standard implementations.
		  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.custom]]
		  */
		override def isCustom :Boolean = false

		/** Alters this relation in a non-standard way (that is, non limited to including/excluding components).
		  * This may change any buffs on the mapping's components, but cannot change column names.
		  * @param alter A function applying changes to a mapping containing a component homomorphic
		  *              with this relation's mapping. The first argument is
		  *              the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping
		  *              for a whole row of an altered table, while the second is the counterpart export component
		  *              of this relation's mapping in the former. Neither needs to feature this relation's
		  *              [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.mapping mapping]]
		  *              as its component. The returned mapping must be some kind of a proxy
		  *              to the first argument, recognizing all of its subcomponents.
		  *              When constructing the relation returned by this method, both arguments are specified
		  *              as `this.anchored`. However, in order to implement
		  *              [[net.noresttherein.oldsql.sql.ast.JoinedRelation.alterOther alterOther]] correctly,
		  *              this function must take this more generic form.
		  * @return a relation expression of the same type as this one, but with
		  *         its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping being
		  *         the result of applying the given function to `this.anchored`.
		  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.isCustom]]
		  */
		def custom(alter :(TypedMapping[Any, Unit], TypedMapping[Subject, Unit]) => MappingOf[Any]) :Rel[F]

		/** Alters this relation in a non-standard way (that is, non limited to including/excluding components).
		  * This may change any buffs on the mapping's components, but cannot change column names.
		  * @param alter A function applying the changes to a mapping of another relation, containing a component
		  *              homomorphic with this one.
		  *              The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping
		  *              for the whole row of an altered table is given as its argument, and the returned mapping
		  *              must be some kind of a proxy to it, recognizing all of its subcomponents.
		  *              Note that the argument is ''not'' necessarily homomorphic with this relation's mapping.
		  * @return a relation expression of the same type as this one, but with
		  *         its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored anchored]] mapping being
		  *         the result of applying the given function to `this.anchored`.
		  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate.isCustom]]
		  */
		def custom(alter :MappingOf[Any] => MappingOf[Any]) :Rel[F] = custom((root, _) => alter(root))


		/** A new `JoinedRelation` instance representing the same relation, but in ''from'' clause `E` at `offset`.
		  * The class of the created instance and all its fields except for `position` are the same as in this instance.
		  * Note that some specific implementations, such as [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]
		  * may in result carry information not representative of clause `E` (in this case, a grouping expression referring
		  * to tables not existing in `E`. This discrepancy is resolved by stating that the value (exact SQL produced from)
		  * of a `JoinedRelation[F, T]` is unspecified unless
		  * it is [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchored(from:F)* anchored]] in the same instance of `F`
		  * that is included in the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] instance of which the relation
		  * expression is a part. In other words, evaluation of this expression is deferred until the time is rendered
		  * as a part of an SQL ''select'', and all instances of the same subclass of this trait are exactly equivalent
		  * as long as they [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes include]]
		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes exclude]] the same optional components.
		  */
		//Fixme: this is unsafe, as the relation type might change between JoinedTable, JoinedParam and JoinedGrouping.
		// Unless we can essentially treat all unanchored relations the same way and convert them on anchoring,
		// it might be better to just expose copy to the whole package.
		override def moveTo[E <: RowProduct](offset :RelationOffset[E, T] { type First = FromLast }) :JoinedRelation[E, T] =
			copy(offset, includes.withOrigin[E], excludes.withOrigin[E])

		//these three methods
		override def asIn[E[+L <: F] <: L Expanded M forSome {type M[O] <: MappingAt[O]}] :Rel[E[F]] =
			asIn[E[F]](PrefixOf.expand)

		/** This method is equivalent to
		  * `this.`[[net.noresttherein.oldsql.sql.ast.ComponentSQL.GenericComponentSQLTemplate.expand expand]]`[E]`,
		  * but relies on invariant `PrefixOf` rather than variant
		  * [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] of the latter, so the result
		  * has an exact clause type parameter `E`, rather than some `_ >: E <: RowProduct`.
		  */
		override def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :Rel[E] =
			moveTo(position + expansion)

		override def asLast :Rel[FromLast] = result(0)

		/** Forwarder to `defaultWith` introduced because self type `R` of this class makes Scala pick
		  * the concrete `defaultWith` from `RelationSQL` rather than the generic, templated signature of this trait.
		  */
		@inline private def result(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]]) :Rel[F] =
			if (includes == this.includes && excludes == this.excludes) this //origin
			else defaultWith(includes, excludes)

		@inline private def result[E <: RowProduct]
		                          (index :Int, includes :Unique[TypedMapping[_, E]], excludes :Unique[TypedMapping[_, E]])
				:Rel[E] =
			copy(RelationOffset.unsafe[E, FromLast, origin.position.Rel, T](index), includes, excludes)

		@inline private def result[E <: RowProduct](index :Int) :Rel[E] =
			if (index == this.index)
				origin.asInstanceOf[Rel[E]]
			else
				copy(
					RelationOffset.unsafe[E, FromLast, origin.position.Rel, T](index),
					includes.withOrigin[E], excludes.withOrigin[E]
				)

		/** A ''trusted'' copy constructor for the dynamic type of this instance, using the same `relation`.
		  * In wrong hands, may result in invalid instances (in particular for `GroupingSQL`).
		  */
		protected def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
		                                    includes :Unique[TypedMapping[_, E]],
		                                    excludes :Unique[TypedMapping[_, E]]) :Rel[E]
	}


	private[sql] trait CustomRelationMixin[F <: RowProduct, T[A] <: MappingAt[A],
	                                       +Rel[f <: RowProduct] <: JoinedRelation[f, T] with JoinedRelationTemplate[f, T, Rel]]
		extends JoinedRelationTemplate[F, T, Rel] with RowShapeCache
	{ this :Rel[F] =>
		override def isDefault = false
		override def isCustom = true
		override def makeCustom :Rel[F] = this
	}

}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] referencing a single occurrence
  * of a [[net.noresttherein.oldsql.schema.Table table]] with mapping type `M` in ''from'' clause `F`.
  * Instances of this class are carried as the right sides of all [[net.noresttherein.oldsql.sql.Join Join]] types
  * (and [[net.noresttherein.oldsql.sql.Subselect Subselect]]), representing particular table aliases,
  * with the whole table list being available through
  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] facade to any `RowProduct` type.
  * It can be used in three ways:
  *   1. as a component expression for the whole table row, with the full entity type as its value, to be used
  *      as a single value, for example within a ''select'' clause of an SQL ''select'';
  *   1. as a factory of component expressions for components (and columns) of the mapping for the represented table
  *      (see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.ComponentSQLTemplate.\ ComponentSQL.\]]);
  *   1. as an origin of mapping instances for components of table mapping `T`, sharing the `RowProduct` subtype `F`
  *      - the domain of this expression - as their [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type;
  *      this allows their identification and matching with the originating relation. Because of this function
  *      it is often called the ''origin table'', or simply ''origin'', of a component mapping or
  *      a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] instance.
  *
  * When used within larger SQL expressions, it is either rendered as an SQL tuple/sequence of all
  * [[net.noresttherein.oldsql.schema.Mapping.selectedByDefault selectable by default]] columns of the table
  * or, when used inside a comparison expression (such as [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]),
  * it can be split and result in a conjunction of column vs column comparisons between the both sides.
  *
  * @tparam F $F
  * @tparam T $M
  * @define F    the domain of of this expression, that is a ''from'' clause type (including any implicitly inherited
  *              ''from'' clauses of outer ''select'' expressions). While `RowProduct` itself is used here
  *              as the upper bound, all instances are created for
  *              `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]]; this relaxation increases the flexibility
  *              of its use, in particular allows matching any `JoinedRelation[F, T]` with `JoinedTable[F, T]` type.
  *              It always starts with `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` T`,
  *              which is the upper bound of all [[net.noresttherein.oldsql.sql.Join joins]] and ''from'' clauses
  *              ending with the same tables following `T`. The type parameter is invariant and used
  *              as [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the exported table mapping
  *              and all its components.
  *              Thus, any [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[T, F]` is presumed
  *              to be a component coming from mapping `T` and used to reference that particular occurrence of `T` in `F`.
  * @define this table
  * @define Cons `JoinedTable`
  * @define link [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]
  */
trait JoinedTable[F <: RowProduct, T[A] <: MappingAt[A]]
	extends JoinedRelation[F, T] with JoinedRelationTemplate[F, T, ({ type E[f <: RowProduct] = JoinedTable[f, T] })#E]
{
	override type FromLast = RowProduct AndFrom T

	override val relation :Table[T]

	/** The underlying table - same as [[net.noresttherein.oldsql.sql.ast.JoinedTable.relation relation]]. */
	def table :Table[T] = relation

	override def isTable = true

	/** Casts down this table to its implementation interface; this is a guaranteed safe cast. */
	def toTableSQL :TableSQL[F, M, T[F]#Subject]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

//	override def anchor(relation :Relation[T]) :JoinedTable[F, T]
//
//	override def moveTo[E <: RowProduct](offset :RelationOffset[E, T] { type First = FromLast }) :JoinedTable[E, T]
//
//	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
//	override def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedTable[E[F], T]
//
//	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
//	  * and returns a `JoinedTable`. The `expand` method cannot be overriden here to return a `JoinedTable`
//	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
//	  */
//	override def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedTable[G, T]
//
//	override def asLast :JoinedTable[FromLast, T]

	/** Renders the declaration of this relation, that is the form in which it should appear in the ''from'' clause `F`.
	  * This will return either the [[net.noresttherein.oldsql.schema.BaseTable.name name]] of
	  * the [[net.noresttherein.oldsql.sql.ast.JoinedTable.table table]], or a completely formatted SQL ''select''
	  * if `this.table` is a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
	  * possibly with an ''as'' clause (using either the
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.alias alias]] listed by `F`,
	  * or an automatically generated new unique identifier) and add a new alias to `context` using method
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.table table]] of `spelling`. The latter will typically
	  * later delegate to [[net.noresttherein.oldsql.schema.Table.defaultSpelling defaultSpelling]] method
	  * of the associated [[net.noresttherein.oldsql.schema.Table Table]].
	  *
	  * As an instance of this class semantically represents a placeholder reference to a relation in
	  * any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance conforming
	  * to its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.Origin Origin]] type `F`,
	  * returned SQL fragment reflects ''the corresponding table in ''`from`'', rather than this one''.
	  * In most cases it doesn't matter, as expressions are typically included in the same ''select''
	  * as the ''from'' clause instance which was used as the source of referenced relations. Moreover,
	  * even if an expression has been created separately, in most cases it will be equal to the one included in `from`:
	  * mapping types typically uniquely identify source tables in the schema. However, it is possible that another
	  * relation is used than the one included in this instance, either due to several tables with the same mapping,
	  * or because a table has been altered by
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.include including]]
	  * or [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.exclude excluding]] certain columns.
	  *
	  * @param from     a ''from'' clause prefix of the formatted SQL ''select'' which ends with this table.
	  * @param context  additional spelling information required for formatting the whole SQL select, in particular
	  *                 the list of aliases used by all listed tables.
	  * @param params   a factory of `P => X` accessor functions, where `P` is the intended parameter(s) type
	  *                 of the query being built.
	  * @param spelling a formatting strategy used for all fragments of the returned SQL, responsible in particular
	  *                 for implementing any deviations from the SQL standard demanded by the DBMS being used.
	  */
	@throws[IllegalArgumentException]("if from is not an NonEmptyRow, or its last relation does not equal this.")
	protected def spell[P, C <: F with FromSome](from :C, context :SQLContext[P], params :Parameterization[P, C])
	                                            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if(from.last != this)
			throw new IllegalArgumentException("Cannot spell non last table " + this + " of " + from + ".")
		else
			spelling.table(table, from.aliasOpt)(from, context, params)

	private[sql] def spell[P, C <: F with FromSome]
	                      (spelling :SQLSpelling)(from :C, context :SQLContext[P], params :Parameterization[P, C])
			:SpelledSQL[P] =
		spell(from, context, params)(spelling)


	protected override def defaultSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                                        (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                         inline :Boolean)
	                                        (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                                        (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		component.mapping match {
			case column :TypedColumn[_, F] @unchecked => component.origin match {
				case table :JoinedTable[F, T] =>
					relation.spell(spelling)(table, column)(from, context, params)
				case _ => //if anchored isomorphic other.mapping =>
					spelling(component.graft(this))(from, context, params)
			}
			case _ =>
				super.defaultSpellingOf[P, C, M](component, inline)(from, context, params)
		}

	protected override def explodedSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                                         (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                                          independent :Boolean)
	                                         (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                                         (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		component.origin match {
			case table :JoinedTable[F, T] =>
				relation.spellExploded(spelling)(table, component.mapping, independent)(from, context, params)
			case _ =>
				spelling.explode(component.graft(this))(from, context, params)
		}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedTable.__]

	private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL(seal :Seal) :Unit
}




object JoinedTable {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :JoinedTable.Typed[F, X] @unchecked =>
				Got(from.relation -> from.index)
			case _ => Lack
		}

	@inline implicit def JoinedTableExtension[F <: RowProduct, T[O] <: BaseMapping[S, O], S]
	                                         (self :JoinedTable[F, T])(implicit subject :T[Unit] <:< BaseMapping[S, Unit])
			:JoinedTableExtension[F, T, S] =
		new JoinedTableExtension[F, T, S](self)

	class JoinedTableExtension[F <: RowProduct, T[O] <: BaseMapping[S, O], S] private[ast]
	                          (private val self :JoinedTable[F, T])
		extends AnyVal
	{
		def typed :TableSQL[F, T, S] = self.asInstanceOf[TableSQL[F, T, S]]
	}


	type __ = JoinedTable[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[M[O] <: MappingAt[O]] = JoinedTable[RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedTable[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedTable[F, T] forSome { type T[O] <: TypedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedTable[O, M] }
}






/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[U, GlobalScope, T[Unit]#Subject]` used
  * in the context of a ''group by'' clause
  * `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized GroupingOfGeneralized]]`[U]`,
  * adapted to [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] interface shared with tables
  * and all other mappings exposed as [[net.noresttherein.oldsql.sql.Adjoin.right right]] sides of various
  * generalized 'join' [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] implementations.
  *
  * This trait is used by [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]]
  * clauses (but not [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) and works in the same way as
  * other relation expressions: it can be used within larger SQL expressions (in particular, the ''select'' clause),
  * as well as serve as the origin for subexpressions of the adapted grouping expression. These are represented
  * by standard [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] expressions.
  *
  * @tparam U $U
  * @tparam F $F
  * @tparam T $M
  * @define U    The type of the actual ''from'' clause grouped by `F`, used as the domain of the grouping expression.
  * @define F    The domain type of this relation expression (not the grouping expression inside the relation).
  *              It is always a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] subtype, an upper bound
  *              not enforced explicitly in the type to increase its usability in generic contexts, but at construction -
  *              either statically instantiated to a `GroupBy`/`By`, or copied over from another `JoinedGrouping`
  *              instance (or some type which similarly is always created only for `GroupByClause` subtypes).
  * @define M    The mapping type constructor accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *              adapting and exposing the carried expression. In most cases, it is
  *              some [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] subtype, but component expressions
  *              [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[U, T]` used as grouping expressions
  *              have it declared simply as their mapping type `T`. Additionally, it is possible to expose
  *              an [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping for a statement
  *              parameter declared in the grouped ''from'' clause `U` by using it as any other expression to expand
  *              a ''group by'' clause. Such a parameter mapping translates to empty SQL just as
  *              [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] clause, but is not considered an unbound parameter
  *              itself (it doesn't feature again in the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type
  *              of `F`) - instead it simply allows the use of the parameter represented by the normally unavailable
  *              (outside of aggregate functions) [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]],
  *              including the creation of derived parameter (components of `UnboundParam`).
  * @define this grouping
  * @define Cons `JoinedGrouping`
  * @define link [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]
  * @see [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation]]
  */
trait JoinedGrouping[-U <: RowProduct, F <: RowProduct, T[A] <: MappingAt[A]]
	extends JoinedRelation[F, T]
	   with JoinedRelationTemplate[F, T, ({ type E[f <: RowProduct] = JoinedGrouping[U, f, T] })#E]
{
	override type FromLast = RowProduct AndBy T

	override val relation :GroupingRelation[U, T]

	/** The pseudo relation for the represented grouping expression -
	  * same as [[net.noresttherein.oldsql.sql.ast.JoinedGrouping.relation relation]].
	  */
	def grouping :GroupingRelation[U, T] = relation

	override def isGrouping = true

	/** Casts down this table to its implementation interface; this is a guaranteed safe cast. */
	def toGroupingSQL :GroupingSQL[U, F, M, T[F]#Subject]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }


//	override def anchor(relation :Relation[T]) :JoinedGrouping[_ <: RowProduct, F, T]
//
//	override def moveTo[E <: RowProduct](offset :RelationOffset[E, T] { type First = FromLast }) :JoinedGrouping[U, E, T]
//
//	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
//	override def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedGrouping[U, E[F], T]
//
//	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
//	  * and returns a `JoinedGrouping`. The `expand` method cannot be overridden here to return a `JoinedGrouping`
//	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
//	  */
//	override def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedGrouping[U, G, T]
//
//	override def asLast :JoinedGrouping[U, FromLast, T]


	protected override def sqlParamCountOf[C <: F, M[A] <: MappingAt[A]]
	                                      (component :ComponentSQL[C, M] { type Origin = F; type Entity[A] = T[A] })
	                                      (implicit spelling :SQLSpelling) :Int =
		spelling.sqlParamCount(relation)

	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	protected override def defaultSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                  (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }, inline :Boolean)
	                  (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else {
			//todo: it would be great if we could enforce that ctx.Grouping[O] = T[O];
			// not impossible if we implement shrinking in RelationOffset as a proof
			val ctx = from.`->groupingSpellingContext`(
				index, context, params.asInstanceOf[Parameterization[P, from.Self]]
			)
			ctx.grouping.asInstanceOf[GroupingRelation[ctx.Ungrouped, T]]
				.spell(spelling)(component)(ctx.from, ctx.context, ctx.params)
		}

	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	protected override def explodedSpellingOf[P, C <: F, M[O] <: MappingAt[O]]
	                       (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                        independent :Boolean)
	                       (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else {
			val ctx = from.`->groupingSpellingContext`(
				index, context, params.asInstanceOf[Parameterization[P, from.Self]]
			)
			ctx.grouping.asInstanceOf[GroupingRelation[ctx.Ungrouped, T]]
				.spellExploded(spelling)(component, independent)(ctx.from, ctx.context, ctx.params)
		}


	override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
		case self if this eq self => true
		case other :JoinedGrouping.__ if canEqual(other) && other.canEqual(this) =>
			index == other.index && (relation isomorphic other.relation) //todo: type equality; todo
		case _ =>
			false
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedGrouping.__]

	private[sql] def concrete_JoinedGrouping_subclass_must_extend_GroupingSQL(seal :Seal) :Unit
}




object JoinedGrouping {

//	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(GroupingRelation[MappingAt], Int)] =
//		e match {`
//			case from :JoinedGrouping.Typed[F, X] @unchecked =>
//				Got(from.relation -> from.offset)
//			case _ => Lack
//		}
	@inline implicit def JoinedGroupingExtension[D <: RowProduct, F <: RowProduct, T[O] <: BaseMapping[S, O], S]
	                                            (self :JoinedGrouping[D, F, T])
	                                            (implicit subject :T[Unit] <:< BaseMapping[S, Unit])
			:JoinedGroupingExtension[D, F, T, S] =
		new JoinedGroupingExtension[D, F, T, S](self)

	class JoinedGroupingExtension[D <: RowProduct, F <: RowProduct, T[O] <: BaseMapping[S, O], S]
	                             (private val self :JoinedGrouping[D, F, T])
		extends AnyVal
	{
		def typed :GroupingSQL[D, F, T, S] = self.asInstanceOf[GroupingSQL[D, F, T, S]]
	}



	type __ = JoinedGrouping[_ <: RowProduct, _ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[D <: RowProduct, M[O] <: MappingAt[O]] = JoinedGrouping[D, RowProduct AndBy M, M]

	type AnyIn[F <: RowProduct] = JoinedGrouping[Nothing, F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[D <: RowProduct, F <: RowProduct, V] =
		JoinedGrouping[D, F, T] forSome { type T[O] <: TypedMapping[V, O] }
	//todo: rename all Of types to of
	type Of[D <: RowProduct, M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedGrouping[D, O, M] }
}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] representing a single occurrence
  * of a (whole) [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter of ''from'' clause `F`.
  * Instances of this class are carried as the right sides of all [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
  * and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] clauses.
  * It can be used in three ways:
  *   1. as a component expression for the whole parameter, with the mapping's
  *      [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type as its value, just as any other
  *      [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]].
  *   1. as a factory of unbound parameter expressions with values derived from the subject of mapping `T`
  *      (see [[net.noresttherein.oldsql.sql.ast.ComponentSQL.\ \]]).
  *   1. as an origin of mapping instances for components of the represented parameter,
  *      sharing the `RowProduct` subtype `F` - the domain of this expression -
  *      as their [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type;
  *      this allows their identification as derived from this parameter
  *      (see [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]]).
  *
  * When used within larger SQL expressions, it is either rendered as an SQL tuple/sequence of JDBC parameter
  * placeholders "?" in a number defined by a [[net.noresttherein.oldsql.schema.SQLForm SQLForm]] provided
  * for the parameter type. When used inside a comparison expression
  * (such as [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]) or
  * [[net.noresttherein.oldsql.sql.ComponentSetter ComponentSetter]] assignments, it can be split into individual
  * JDBC parameters, interlaced in the final statement with other SQL fragments.
  *
  * @tparam F $F
  * @tparam T $M
  * @define F    the domain of of this expression, that is a ''from'' clause type (including any implicitly inherited
  *              ''from'' clauses of outer ''select'' expressions). While `RowProduct` itself is used here
  *              as the upper bound, all instances are created for `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]];
  *              this relaxation increases the flexibility of its use, in particular allows matching any
  *              `JoinedRelation[F, T]` with `JoinedTable[F, T]` type. It always starts
  *              with `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` T`, which is the upper bound
  *              of all [[net.noresttherein.oldsql.sql.Join joins]] and ''from'' clauses ending with the same tables
  *              following `T`. The type parameter is invariant and used
  *              as [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the exported table mapping
  *              and all its components.
  *              Thus, any [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[T, F]` is presumed
  *              to be a component coming from mapping `T` and used to reference that particular occurrence of `T` in `F`.
  * @define M    a type constructor of the mapping for the parameter, accepting its `Origin` type.
  *              In case of arbitrary parameters, this will be
  *              [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]], but this pseudo relation can
  *              also use mappings defined for other relations, when the parameter is their `Subject` type;
  *              this allows easier access to parameter properties as usual component mappings/expressions.
  * @define this parameter
  * @define Cons `JoinedParam`
  * @define link [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]
  */
trait JoinedParam[F <: RowProduct, T[O] <: MappingAt[O]]
	extends JoinedRelation[F, T] with JoinedRelationTemplate[F, T, ({ type E[f <: RowProduct] = JoinedParam[f, T] })#E]
{
	override def isParam = true

	/** Casts down this table to its implementation interface; this is a guaranteed safe cast. */
	def toParamSQL :ParamSQL[F, Subject, _ <: RowProduct]

//	override def anchor(relation :Relation[T]) :JoinedParam[F, T] //can't be Rel because GroupingSQL and its ungrouped clause which can change
//
//	override def moveTo[E <: RowProduct](offset :RelationOffset[E, T] { type First = FromLast }) :JoinedParam[E, T]
//
//	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
//	override def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedParam[E[F], T]
//
//	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
//	  * and returns a `JoinedParam`. The `expand` method cannot be overriden here to return a `JoinedParam`
//	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
//	  */
//	override def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedParam[G, T]
//
//	override def asLast :JoinedParam[FromLast, T]


	protected override def sqlParamCountOf[C <: F, M[O] <: MappingAt[O]]
                                          (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
                                          (implicit spelling :SQLSpelling) :Int =
		component.selectForm.columnCount


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedParam.__]

	private[sql] def concrete_JoinedParam_subclass_must_extend_ParamSQL(seal :Seal) :Unit
}




object JoinedParam {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(ParamRelation[X], Int)] =
		e match {
			case from :ParamSQL[_, X, _] => Got(from.relation -> from.index)
			case _ => Lack
		}


	@inline implicit def JoinedParamExtension[F <: RowProduct, S]
	                                         (self :JoinedParam[F, UnboundParam.Of[S]#M])
			:JoinedParamExtension[F, self.FromLast, S] =
		new JoinedParamExtension[F, self.FromLast, S](self)

	class JoinedParamExtension[F <: RowProduct, L <: RowProduct, S] private[JoinedParam]
	                          (private val self :JoinedParam[F, UnboundParam.Of[S]#M])
		extends AnyVal
	{
		def typed :ParamSQL[F, S, L] = self.asInstanceOf[ParamSQL[F, S, L]]
	}



	type __ = JoinedParam[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

//	type Last[M[O] <: MappingAt[O]] = JoinedParam[RowProduct AndFrom M, M]
//	type FromLast[X] = JoinedParam[RowProduct AndFrom ParamRelation[X]#Param, ParamRelation[X]#Param]
//	type ByLast[X] = JoinedParam[RowProduct AndBy ParamRelation[X]#Param, ParamRelation[X]#Param]

	type AnyIn[F <: RowProduct] = JoinedParam[F, T] forSome { type T[O] <: MappingAt[O] }

//	type Typed[F <: RowProduct, V] = JoinedParam[F, ] forSome { type T[O] <: TypedMapping[V, O] }

//	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedParam[O, M] }
	type Of[X] = { type T[O <: RowProduct] = JoinedParam[O, ParamRelation[X]#Param] }
}






/** The implementation interface extended by all concrete [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
  * subclasses. It takes as an additional type parameter
  * the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]] type `S` of the relation mapping `T`,
  * which is necessary to implement many methods not possible in `JoinedRelation`, because the latter is used
  * in contexts where the former is unavailable, namely as right sides of joins. This is an abstract base class
  * for all implementations of different relation types, and every `JoinedRelation` 'interface' subtype
  * ([[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]],
  * [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]],
  * [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]) has a corresponding subtype of `RelationSQL`
  * ([[net.noresttherein.oldsql.sql.ast.TableSQL TableSQL]], [[net.noresttherein.oldsql.sql.ast.GroupingSQL GroupingSQL]],
  * [[net.noresttherein.oldsql.sql.ast.ParamSQL ParamSQL]]) extending the former.
  * @tparam F $F
  * @tparam T $M
  * @tparam V $V
  * @tparam L $L
  *
  * @define M    a type constructor for the `Mapping` type of the underlying
  *              [[net.noresttherein.oldsql.schema.Relation Relation]]. Note that this is the interface/nominal type
  *              of the relation, but the ''export'' mapping of the origin relation might be different,
  *              and so can also this expression define [[net.noresttherein.oldsql.sql.ast.RelationSQL.altered alterations]]
  *              to the column set used. This has the effect of changing the actual mapping used, which might be any proxy
  *              to this mapping (preserving its subject type).
  *              In the basic use case, it would be a mapping for the whole table row.
  * @define V    the type to which the complete row of the underlying relation maps; it is the `Subject` type
  *              of the mapping `T` of the `origin` of this expression.
  * @define L    [[net.noresttherein.oldsql.sql.ast.LValueSQL.FromLast FromLast]] type of this relation:
  *              a `RowProduct` subtype joining mapping `T` using the join type proper to this relation type
  *              with the wildcard upper bound on the join's left side.
  * @define Cons `RelationSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.RelationSQL RelationSQL]]
  */ //todo: L >: F <: RowProduct
abstract class RelationSQL[F <: RowProduct, T[A] <: BaseMapping[V, A], V, L <: RowProduct]
                          (override val relation :Relation[T],
                           override val position :RelationOffset[F, T] { type First = L })
	extends TypedComponentSQL[F, T, V, T, V, L]
	   with JoinedRelation[F, T]
	   with LValueTemplate[F, T, V, ({ type E[f <: RowProduct] = RelationSQL[f, T, V, L]})#E, RelationSQL[F, T, V, L]]
	   with JoinedRelationTemplate[F, T, ({ type R[f <: RowProduct] = RelationSQL[f, T, V, L] })#R]
{ self =>
	override type FromLast = L
	override def projection :IsomorphicProjection[T, V, F] = OriginProjection.isomorphism

	override val mapping       :T[Origin] = relation[Origin]
	override val export        :BaseMapping[V, Origin] = mapping
	override val extract       :MappingExtract[V, V, Origin] = MappingExtract.ident(export)

	override lazy val altered  :TypedMapping[V, Origin] =
		if (isDefault) mapping else mapping(includes, excludes)

	override lazy val anchored :TypedMapping[V, Origin] = {
		if (isDefault) relation.export[Origin]
		//will not recognize components of relation.export, but combines all includes/excludes and applies them together.
		else relation.alter(includes, excludes).export[Origin]
	}.asInstanceOf[TypedMapping[V, Origin]]

	//these need to be overridden due to JoinedRelation's having wider bounds than the inherited implementations
	override def \[K <: MappingAt[Origin], X](component :K)(implicit project :OriginProjection[K, X])
			:TypedComponentSQL[F, T, V, project.WithOrigin, X, L] =
		TypedComponentSQL(origin, component)

	override def \[K <: ColumnAt[Origin], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: BaseColumn[X, A] })
			:TypedColumnComponentSQL[F, T, V, project.WithOrigin, X, L] =
		TypedColumnComponentSQL(origin, column)

	override def \[K <: MappingAt[Origin]]
	              (component :T[Origin] => K)(implicit factory :ComponentSQL.Factory[K])
			:factory.TypedResult[F, T, V, L] =
		factory(origin, component(mapping))

	@inline override def upcast :RelationSQL[F, MappingOf[Any]#TypedProjection, Any, L] =
		this.asInstanceOf[RelationSQL[F, MappingOf[Any]#TypedProjection, Any, L]]

	@inline final override def toRelationSQL :RelationSQL[F, T, V, L] = this

	override def asRelationSQL :Some[RelationSQL[F, T, V, L]] = Some(this)


	override def alterOther[C <: E, E <: RowProduct, M[A] <: MappingAt[A],
	                        R[f <: RowProduct] <: JoinedRelation[f, M] with JoinedRelationTemplate[f, M, R]]
	                       (rel :R[E], component :ComponentSQL[C, T] { type Origin = E; type Entity[A] = M[A] }) :R[E] =
		if (component.origin != rel)
			throw new IllegalArgumentException(
				"Cannot alter relation " + rel.fullString + " because it is not the origin of " + component + "."
			)
		else if (isDefault)
			rel
		else if (isCustom)
			throw Bug(
				"Cannot alter component " + component + " of " + rel.fullString + " as in " + fullString +
				" because the latter is custom, but does not override method alterOther."
			)
		else if (rel.mapping contains mapping.withOrigin[E]) //components of this are subcomponents of origin
			rel.alter(includes.withOrigin[E], excludes.withOrigin[E])
		else if (mapping isomorphic component.mapping) {
			rel.alter(
				includes.withOrigin[E].view.map(rel.mapping.counterpart(component.mapping, _)),
				excludes.withOrigin[E].view.map(rel.mapping.counterpart(component.mapping, _))
			)
		} else
			throw new IllegalArgumentException(
				"Cannot alter component " + component + " of " + rel.fullString + " as in " + fullString +
				" because the latter's mapping " + mapping + " does not match the component's mapping " +
				component.mapping + "."
			)

//	private def superSubstitute[E <: F](columns :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, T, V] =
//		super.substitute(columns)
//
//	override def substitute[E <: F](substitutes :Iterable[ColumnSetter[FromLast, E]]) :MappingSQL[E, Single, T, V] =
//		super.substitute(substitutes)
//		origin.superSubstitute(substitutes)

	protected override def isFinal :Boolean = false

	//Preservation of FromLast is needed for anchoring EditedComponentSQL. It is a bit fishy, as it's technically
	// possible for the counterpart in `clause` to be of a different type, same Origin type *should* be sufficient
	// to preserve FromLast even if the JoinedRelation itself is of a different type, because Origin
	// cannot start with something more generic than FromLast
	override def anchor(clause :F) :RelationSQL[F, T, V, L] = {
		val actual = clause.relations(position)
		if (actual.relation == relation) origin
		else if (isDefault) actual.typed
		else if (relation sameAs actual.relation)
			actual.typed.alterLike(origin)
		else
			throw new IllegalArgumentException(
				"Cannot anchor relation expression " + this + " in FROM clause " + clause + " because " +
				"relation " + actual.relation + " of its corresponding expression " + actual + " is not the same " +
				"as this relation: " + relation + "."
			)
	}

	//This may change the RelationSQL type, based on the type of the given relation.
	// If it is not acceptable, we need to uncomment overrides in subclasses
	override def anchor(relation :Relation[T]) :RelationSQL[F, T, V, _ <: RowProduct] =//can't be Rel because GroupingSQL
		RelationSQL(relation, position) //not ideal, as it will not support non standard relations

	/** Simply returns the given relation. */
	override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :RelationSQL[P, T, V, relation.FromLast] =
		relation.typed

	override def expand[U <: F, E <: RowProduct]
	                   (base :E)(implicit expansion :U ExpandedBy E, isSingle :Single <:< Single)
			:RelationSQL[_ >: E <: RowProduct, T, V, L] =
		base.relations(position + expansion.superPrefix[F])

	override def expand[E <: RowProduct]
	                   (implicit expansion :F ExpandedBy E) :RelationSQL[_ >: E <: RowProduct, T, V, L] =
		asIn(expansion.superPrefix)


	override def asGrouping :GroupingRelation[F, T] = asGrouping(this)

	def asGrouping[M[A] <: BaseMapping[S, A], S]
	              (component :TypedComponentSQL[F, T, V, M, S, L]) :GroupingRelation[F, M]

/*
	override def :=[P <: RowProduct, Y](rvalue :SQLExpression[P, Single, Y])
	                                   (implicit compat :V =~= Y) :ComponentSetter[F, P] =
		???
//		ComponentSetter[F, T, V, P, Y, compat.Unified](this, rvalue)(compat)

	override def :=[C <: MappingOf[V], E <: RowProduct, P <: RowProduct]
	               (component :C)(implicit cast :C <:< TypedMapping[V, P],
	                              subtype :SQLExpression[P, Single, V] <:< SQLExpression[E, Single, V],
	                              project :OriginProjection[C, V], offset :RelationCount[P, _ <: Numeral]) :ComponentSetter[F, E] =
		???
//		this := subtype(LooseComponent(component))

	override def :=[X](that :X)(implicit compat :V =~= X, form :SQLForm[X]) :ComponentSetter[F, RowProduct] =
		???
//		this := SQLTerm(that)

	override def :=?[X](rvalue :X)(implicit compat :V =~= X, form :SQLForm[X]) :ComponentSetter[F, RowProduct] =
		???
//		this := BoundParam(rvalue)
*/


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
		visitor.relation(toRelationSQL)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.relation(toRelationSQL)


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case rel :RelationSQL.__ @unchecked if rel canEqual this =>
			relation == rel.relation && includes.toSet == rel.includes.toSet && excludes.toSet == rel.excludes.toSet
		case _ => false
	}
	override def hashCode :Int = relation.hashCode

	private[sql] override def concrete_JoinedRelation_subclass_must_extend_RelationSQL(seal :Seal) :Unit = ()
}




object RelationSQL {

	def apply[T[O] <: BaseMapping[V, O], V](relation :Relation[T])
			:RelationSQL[F, T, V, F] forSome { type F <: RowProduct } =
		relation match {
			case table :Table[T] =>
				TableSQL[AndFrom.LUB[T], T, V](table, 0)
			case grouping :GroupingRelation[f, T] =>
				GroupingSQL[f, AndBy.Last, T, V](grouping, 0)
			case param :ParamRelation[v] =>
				ParamSQL[WithParam.Last[v], v, WithParam.Last[v]](param, 0)
					.asInstanceOf[RelationSQL[RowProduct, T, V, RowProduct]]
			case _ =>
				throw new IllegalArgumentException(
					"Cannot create a RelationSQL for an unknown relation " + relation + " :" + relation.className + "."
				)
		}

	def apply[F, T[O] <: BaseMapping[V, O], V](relation :Relation[T], offset :RelationOffset[F, T])
			:RelationSQL[F, T, V, offset.First] =
		(relation match {
			case table :Table[T] => TableSQL[F, T, V](table, offset.index)
			case grouping :GroupingRelation[f, T] => GroupingSQL[f, RowProduct, T, V](grouping, offset.index)
			case param :ParamRelation[v] => ParamSQL(param, offset.index)
			case _ =>
				throw new IllegalArgumentException(
					"Cannot create a RelationSQL for an unknown relation " + relation + " :" + relation.className + "."
				)
		}).asInstanceOf[RelationSQL[F, T, V, offset.First]]

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :RelationSQL.from[F]#of[X]#__ @unchecked =>
				Got(from.relation.asInstanceOf[Relation[MappingOf[X]#TypedProjection]] -> from.index)
			case _ => Lack
		}


	type __ = RelationSQL[F, T, X, L] forSome {
		type T[A] <: BaseMapping[X, A]; type X
		type F <: RowProduct; type L <: RowProduct
	}
	//type curry not possible to implement because we can't separate T[O] <: BaseMapping[V, O] and V
	//this deviates from the practice of using 'from' as type constructors taking (invariant) Origin; is it counterintuitive?
	//instead of RelationSQL[_ >: F <: RowProduct, T, V, L] use _ <: RelationSQL[F, T, V, L]
	type from[-F <: RowProduct] = {
		type __ = RelationSQL[O, T, V, L] forSome {
			type O >: F <: RowProduct; type T[A] <: BaseMapping[V, A]; type V; type L <: RowProduct
		}
		type of[V] = {
			type __ = RelationSQL[_ >: F <: RowProduct, T, V, _ <: RowProduct] forSome { type T[O] <: BaseMapping[V, O] }
			type apply[L <: RowProduct] =
				RelationSQL[_ >: F <: RowProduct, T, V, L] forSome { type T[O] <: BaseMapping[V, O] }
		}
	}

	type LastRelation[T[A] <: BaseMapping[S, A], S] =
		RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

	type AnyIn[-F <: RowProduct] = RelationSQL[O, T, V, L] forSome {
		type O >: F <: RowProduct; type T[A] <: BaseMapping[V, A]; type V; type L <: RowProduct
	}

	/** Implementation interface extended by various [[net.noresttherein.oldsql.sql.ast.RelationSQL RelationSQL]]
	  * subtypes `Rel`, containing methods with signatures specific to `Rel`, in particular those altering
	  * and rebasing this relation.
	  * @tparam F   A ''from'' clause serving as the ''domain'' of this expression -
	  *             a list of relations/tables which provide columns used in this expression.
	  *             As this type is invariant in `F`, it serves also as the `Origin` type of all versions of
	  *             the relation mapping (and their components).
	  * @tparam T   A type constructor for the `Mapping` type of the underlying
	  *             [[net.noresttherein.oldsql.schema.Relation Relation]]. Note that this is the interface/nominal type
	  *             of the relation, but the ''export'' mapping of the origin relation might be different, and so can
	  *             also this expression define [[net.noresttherein.oldsql.sql.ast.RelationSQL.altered alterations]]
	  *             to the column set used. This has the effect of changing the actual mapping used, which might be
	  *             any proxy to this mapping (preserving its subject type).
	  *             In the basic use case, it would be a mapping for the whole table row.
	  * @tparam S   The type to which the complete row of the underlying relation maps; it is the `Subject` type
	  *             of the mapping `T` of the `origin` of this expression.
	  * @tparam L   The [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type of the clause resulting
	  *             from dropping all joins and tables following `T` in `F`.
	  * @tparam Rel A type constructor for the extending `RelationSQL` subtype, accepting different `F` and `O`
	  *             clause parameters (as per this type), but preserving the mapping (and its subject type).
	  */
/*
	trait RelationSQLTemplate[F <: RowProduct, T[A] <: BaseMapping[S, A], S, L <: RowProduct,
	                          +Rel[f <: RowProduct] <: RelationSQL[f, T, S, L]]// with RelationSQLTemplate[f, T, S, L, Rel]]
		extends JoinedRelationTemplate[F, T, Rel]
	{ self :Rel[F] with RelationSQLTemplate[F, T, S, L, Rel] => //todo: try self :Rel[F] =>
		override type FromLast = L

//		protected[sql] override def upcast :Rel[F] = origin

//		override val origin :Rel[F] = this.asInstanceOf[Rel[F]]
//
//		override def default :Rel[F] =
//			if (includes.isEmpty && excludes.isEmpty) origin
//			else result[F](index, Unique.empty, Unique.empty)
//
//		override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]]) :Rel[F] =
//			result(index, includes.map(mapping.export(_)), excludes.map(mapping.export(_)))
//
//		override def alterLike[E <: RowProduct](template :JoinedRelation[E, T]) :Rel[F] =
//			template.typed.alterOther[F, F, T, L, Rel[F]](origin, origin)
//
//		//todo: in Scala3, extract the bulk of these two methods into a single implementation (requires generic functions)
//		override def reorder(permutation :IndexedSeq[Int]) :Rel[F] = {
//			ReorderedMapping.validatePermutation(mapping, permutation)
//			custom { (relation, counterpart) =>
//				if (relation == counterpart) //this will also cover relation == mapping
//					relation.reorder(permutation)
////					ReorderedMapping[TypedMapping[Any, Unit], Any, Unit](relation, permutation)
//				else {
//					val nominal = //originCounterpart == mapping also checks for relation == mapping, which is the important case here
//						if (counterpart == mapping || relation.contains(mapping.withOrigin[Unit]))
//							mapping.refine.withOrigin[Unit]
//						else
//							counterpart.original
//					val export = relation.export(nominal)
////					val reordered = ReorderedMapping[TypedMapping[S, Unit], S, Unit](export, permutation)
//					val reordered = export.reorder(permutation)
//					val substitution = PatchedMapping.Overrides(export, reordered)
//					PatchedMapping[TypedMapping[Any, Unit], Any, Unit](relation, substitution)
//				}
//			}
//		}
//
//
//		override def isAnchored(clause :F) :Boolean =
//			clause.fullTableStack(index).relation == relation
//
//		//Preservation of FromLast is needed for anchoring EditedComponentSQL. It is a bit fishy, as it's technically
//		// possible for the counterpart in `clause` to be of a different type, same Origin type *should* be sufficient
//		// to preserve FromLast even if the JoinedRelation itself is of a different type, because Origin
//		// cannot start with something more generic than FromLast
//		override def anchor(clause :F) :RelationSQL[F, T, S, L] = {
//			val actual = clause.relations(position)
//			if (actual.relation == relation) origin
//			else if (isDefault) actual.typed
//			else if (relation sameAs actual.relation)
//				actual.typed.alterLike(origin)
//			else
//				throw new IllegalArgumentException(
//					"Cannot anchor relation expression " + this + " in FROM clause " + clause + " because " +
//					"relation " + actual.relation + " of its corresponding expression " + actual + " is not the same " +
//					"as this relation: " + relation + "."
//				)
//		}
//
//		override def anchor(relation :Relation[T]) :RelationSQL[F, T, S, _ <: RowProduct] //can't be Rel because GroupingSQL
//
//		/** Simply returns the given relation. */
//		override def graft[P <: RowProduct](rel :JoinedRelation[P, T]) :RelationSQL[P, T, S, rel.FromLast] =
//			rel.typed

//		/** A new `RelationSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
//		  * The class of the created instance and all its fields except for `position` are the same as in this instance.
//		  * Note that some specific implementations,
//		  * such as [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]] may in result carry information
//		  * not representative of clause `E` (in this case, a grouping expression referring to tables not existing in `E`.
//		  * This discrepancy is resolved by stating that the value (exact SQL produced from) of a `JoinedRelation[F, T]`
//		  * is unspecified unless it is [[net.noresttherein.oldsql.sql.ast.JoinedRelation.anchor anchored]] in the same
//		  * instance of `F` that is included in the [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] instance
//		  * of which the relation expression is a part. In other words, evaluation of this expression is deferred
//		  * until the time is rendered as a part of an SQL ''select'', and all instances of the same subclass
//		  * of this trait are exactly equivalent
//		  * as long as they [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes include]]
//		  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes exclude]] the same optional components.
//		  */
//		override def moveTo[P <: RowProduct](position :RelationOffset[P, T] { type First = FromLast }) :Rel[P] =
//			if (position == this.position) origin.asInstanceOf[Rel[P]]
//			else copy(position, includes.withOrigin[P], excludes.withOrigin[P])
//
//		override def asIn[J[+A <: F] <: A Expanded M forSome { type M[B] <: MappingAt[B] }] :Rel[J[F]] =
//			result(index + 1)
//
//		override def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :Rel[E] =
//			result(index + expansion.lengthDiff)
//
//		//fixme: FromLast of both JoinParam and all JoinLike is the same, so we cannot preserve the exact relation type
//		//  (just as anchor() does not) unless we do not claim the result is anchored in base, which would be a shame.
//		override def basedOn[U <: F, E <: RowProduct](base :E)(implicit expansion :U PartOf E)
//				:Rel[_ >: E <: RowProduct] =
//			result(index + expansion.lengthDiff)//E is incorrect, but we lose this information anyway
//
//		override def expand[U <: F, E <: RowProduct]
//		                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//				:Rel[_ >: E <: RowProduct] =
//			result(index + expansion.length) //E is incorrect, but we lose this information anyway
//
//		override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :Rel[_ >: E <: RowProduct] =
//			result(index + expansion.length)
//
//		override def asLast :Rel[L] =
//			moveTo(RelationOffset.unsafe[L, L, origin.position.Rel, T](0))
//
//
//		@inline private def result[E <: RowProduct](index :Int, includes :Unique[TypedMapping[_, E]],
//		                                            excludes :Unique[TypedMapping[_, E]]) :Rel[E] =
//			copy(RelationOffset.unsafe[E, FromLast, origin.position.Rel, T](index), includes, excludes)
//
//		@inline private def result[E <: RowProduct](index :Int) :Rel[E] =
//			if (index == this.index)
//				origin.asInstanceOf[Rel[E]]
//			else
//				copy(
//					RelationOffset.unsafe[E, FromLast, origin.position.Rel, T](index),
//					includes.withOrigin[E], excludes.withOrigin[E]
//				)
//
//		/** A ''trusted'' copy constructor for the dynamic type of this instance, using the same `relation`.
//		  * In wrong hands, may result in invalid instances (in particular for `GroupingSQL`).
//		  */
//		protected def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
//		                                    includes :Unique[TypedMapping[_, E]],
//		                                    excludes :Unique[TypedMapping[_, E]])
//				:Rel[E]
	}
*/



	/** A mixin to `RelationSQL` which hard wires component spelling to methods of this instance
	  * (and does not delegate spelling to an actual `RelationSQL` in the ''from'' clause `F` at its `position`).
	  */
	private[sql] trait FinalRelationSQL[F <: RowProduct, T[A] <: BaseMapping[S, A], S, L <: RowProduct]
		extends RelationSQL[F, T, S, L] with RowShapeCache
	{
		protected override def isFinal = true
		protected override def makeFinal :this.type = this

		override def spell[P, C <: F, M[A] <: MappingAt[A]]
		                  (component :ComponentSQL[C, M] { type Origin = F; type Entity[A] = T[A] }, inline :Boolean)
		                  (from :C, context :SQLContext[P], params :Parameterization[P, C])
		                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
			defaultSpellingOf(component, inline)(from, context, params)

		override def spellExploded[P, C <: F, M[A] <: MappingAt[A]]
		                          (component :ComponentSQL[C, M] { type Origin = F; type Entity[A] = T[A] },
		                           independent :Boolean)
		                          (from :C, context :SQLContext[P], params :Parameterization[P, C])
		                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
			explodedSpellingOf(component, independent)(from, context, params)
	}


	private[sql] trait CustomRelationSQL[F <: RowProduct, T[A] <: BaseMapping[S, A], S, L <: RowProduct]
		extends RelationSQL[F, T, S, L]
	{
		protected val underlying :RelationSQL[F, T, S, L]
		protected val customize  :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any]

		override lazy val altered  :TypedMapping[S, Origin] = adapt(underlying.altered)
		override lazy val anchored :TypedMapping[S, Origin] = adapt(underlying.anchored)

		override def isDefault = false
		override def isCustom = true

		protected def composed(alter :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any])
				:(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any] =
			(root, component) => {
				val intermediate = customize(root, component).withOrigin[Unit]
				val counterpart = intermediate.counterpart(root, component)
				alter(intermediate, counterpart)
			}

		private def adapt(row :TypedMapping[S, Origin]) :TypedMapping[S, Origin] =
			customize(row.asInstanceOf[TypedMapping[Any, Unit]], row.withOrigin[Unit]).asInstanceOf[TypedMapping[S, F]]

		private def stackOn(row :TypedMapping[Any, Unit], component :TypedMapping[S, Unit]) :TypedMapping[Any, Unit] =
			if (row == mapping)
				altered.asInstanceOf[TypedMapping[Any, Unit]]
			else if (row == relation.export)
				anchored.asInstanceOf[TypedMapping[Any, Unit]]
			else {
				val customized = customize(row, component).withOrigin[Unit]
				if (includes.isEmpty && excludes.isEmpty)
					customized
				else if (customized.contains(mapping.withOrigin[Unit])
				         || includes.withOrigin[Unit].forall(customized.contains(_))
				         && excludes.withOrigin[Unit].forall(customized.contains(_)))
					customized(includes.withOrigin[Unit], excludes.withOrigin[Unit])
				else if (customized isomorphic mapping) {
					val rowIncludes = customized.counterparts[S, F](mapping, includes) :Unique[TypedMapping[_, Unit]]
					val rowExcludes = customized.counterparts(mapping, excludes) :Unique[TypedMapping[_, Unit]]
					customized(rowIncludes, rowExcludes)
				} else if ((customized isomorphic row) && (component isomorphic mapping)) {
					val counterpart = customized.counterpart(row, component)
					//may fail, but we would have to fail anyway
					val counterpartIncludes = counterpart.counterparts(mapping, includes)
					val counterpartExcludes = counterpart.counterparts(mapping, excludes)
					customized(counterpartIncludes, counterpartExcludes) //it will export them itself
				} else
					throw new IllegalArgumentException(
						"Cannot alter relation mapping " + row + " like " + fullString + " because " + component +
						" is not isomorphic with this relation's mapping " + mapping + "."
					)
			}
		override def alterOther[C <: E, E <: RowProduct, N[A] <: MappingAt[A],
		                        R[f <: RowProduct] <: JoinedRelation[f, N] with JoinedRelationTemplate[f, N, R]]
		                       (rel :R[E], component :ComponentSQL[C, T] { type Origin = E; type Entity[A] = N[A] }) :R[E] =
			if (component.origin != rel)
				throw new IllegalArgumentException(
					"Cannot alter relation " + rel.fullString + " because it is not the origin of " + component + "."
				)
			else
				rel.custom { (root, counterpart) =>
					val underlyingCounterpart = counterpart.counterpart(rel.mapping, component.mapping)
					stackOn(root, underlyingCounterpart)
				}

		override def anchor(relation :Relation[T]) :RelationSQL[F, T, S, _ <: RowProduct] =
			if (relation eq this.relation) this
			else underlying.anchor(relation).custom(customize)

		override def isomorphic(expression :SQLExpression.__) :Boolean = expression match {
			case self if this eq self => true
			case other :CustomRelationSQL[_, T @unchecked, S @unchecked, _] =>
				canEqual(other) && other.canEqual(this) &&
					(customize == other.customize || (altered identical other.altered)) && super.isomorphic(other)
			case _ => false
		}

		override lazy val toString = underlying.toString + "(custom)"
	}



	trait SpecificRelationVisitor[+F <: RowProduct, X, +Y]
		extends SpecificTableVisitor[F, X, Y] with SpecificParamVisitor[F, X, Y] with SpecificGroupingVisitor[F, X, Y]
	{
		def relation[O >: F <: RowProduct, T[A] <: BaseMapping[X, A], L <: RowProduct]
		            (e :RelationSQL[O, T, X, L]) :Y
	}
	type MatchSpecificRelation[+F <: RowProduct, X, +Y] = SpecificRelationVisitor[F, X, Y]

	trait CaseSpecificRelation[+F <: RowProduct, X, +Y] extends MatchSpecificRelation[F, X, Y] {
		override def param[O >: F <: RowProduct, L <: RowProduct]
		                  (e :ParamSQL[O, X, L]) :Y = relation(e)
		override def table[O >: F <: RowProduct, T[A] <: BaseMapping[X, A]](e :TableSQL[O, T, X]) :Y = relation(e)
		override def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[X, A]]
		                     (e :GroupingSQL[D, O, T, X]) :Y = relation(e)
	}
//
//
//	trait RelationVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends TableVisitor[F, Y] with ParamVisitor[F, Y] with GroupingVisitor[F, Y]
//	{
//		def relation[O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V, L <: RowProduct]
//		            (e :RelationSQL[O, T, V, L]) :Y[Single, V, RelationSQL[O, T, V, L]]
//	}
//	type MatchRelation[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		RelationVisitor[F, Y]
//
//	trait CaseRelation[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]]
//		extends MatchRelation[F, Y]
//	{
//		override def param[O >: F <: RowProduct, V, L <: RowProduct]
//		                  (e :ParamSQL[O, V, L]) :Y[Single, V, ParamSQL[O, V, L]] = relation(e)
//
//		override def table[O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V]
//		                  (e :TableSQL[O, T, V]) :Y[Single, V, TableSQL[O, T, V]] = relation(e)
//
//		override def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V]
//		                     (e :GroupingSQL[D, O, T, V]) :Y[Single, V, GroupingSQL[D, O, T, V]] = relation(e)
//	}


	trait AnyRelationVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]]
		extends AnyTableVisitor[F, Y] with AnyParamVisitor[F, Y] with AnyGroupingVisitor[F, Y]
	{
		def relation[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R, L <: RowProduct]
		            (e :RelationSQL[O, T, R, L]) :Y[Single, R]
	}
	type MatchAnyRelation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyRelationVisitor[F, Y]

	trait CaseAnyRelation[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] extends AnyRelationVisitor[F, Y] {
		override def table[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R]
		                  (e :TableSQL[O, T, R]) :Y[Single, R] =
			relation(e)

		override def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R]
		                     (e :GroupingSQL[D, O, T, R]) :Y[Single, R] =
			relation(e)

		override def param[O >: F <: RowProduct, X, L <: RowProduct](e :ParamSQL[O, X, L]) :Y[Single, X] =
			relation(e)
	}
}






/** The sole implementation of [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]. Extracted primarily
  * due to the necessity of a separate mapping `Subject` type `V` in Scala 2.
  * @param relation
  * @param position
  * @param includes
  * @param excludes
  * @tparam F $F
  * @tparam T $M
  * @tparam V $V
  * @define Cons `TableSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.TableSQL TableSQL]]
  */
class TableSQL[-F <: RowProduct, T[A] <: BaseMapping[V, A], V] protected
              (override val relation :Table[T],
               override val position :RelationOffset[F, T] { type First = RowProduct AndFrom T },
               override val includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
               override val excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
	extends RelationSQL[F, T, V, RowProduct AndFrom T](relation, position)
	   with JoinedTable[F, T]
	   with JoinedRelationTemplate[F, T, ({ type R[f <: RowProduct] = TableSQL[f, T, V] })#R]
{
	@inline final override def toTableSQL :TableSQL[F, T, V] = this

	override def asTableSQL :Some[TableSQL[F, T, V]] = Some(this)

	override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[V, Unit]) => MappingOf[Any]) :TableSQL[F, T, V] =
		new CustomTableSQL[F, T, V](this, alter)

	override def makeCustom :TableSQL[F, T, V] =
		if (isCustom)
			toTableSQL
		else
			new TableSQL[F, T, V](relation, position, includes, excludes)
				with CustomRelationMixin[F, T, ({ type R[f <: RowProduct] = TableSQL[f, T, V] })#R]

	protected override def makeFinal :TableSQL[F, T, V] =
		if (isFinal)
			this
		else
			new TableSQL[F, T, V](relation, position, includes, excludes)
				with FinalRelationSQL[F, T, V, FromLast] with RowShapeCache

	override def anchor(relation :Relation[T]) :TableSQL[F, T, V] = relation match {
		case this.relation => this
		case table :Table[T] => TableSQL(
			table, position.index,
			includes.map(relation.row[F].counterpart(mapping, _)),
			excludes.map(relation.row[F].counterpart(mapping, _))
		)
		case _ =>
			throw new IllegalArgumentException(
				"Cannot recreate a TableSQL " + fullString + " for relation " + relation + " :" + relation.getClass.getName + "."
			)
	}

	protected override def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
	                                             includes :Unique[TypedMapping[_, E]],
	                                             excludes :Unique[TypedMapping[_, E]]) :TableSQL[E, T, V] =
		if (!(isDefault && relation.isDefault) && includes == this.includes && excludes == this.excludes)
			new TableSQL[E, T, V](relation, position, includes, excludes) with RowShapeCache { //reuse expensive objects from this instance
				override lazy val altered  = TableSQL.this.altered.withOrigin[E]
				override lazy val anchored = TableSQL.this.anchored.withOrigin[E]
			}
		else
			TableSQL[E, T, V](relation, position.index, includes, excludes)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, T, V, M, V, FromLast]) :GroupingRelation[F, M] =
		GroupingRelation[F, M, V](component)(component.projection.isomorphism)

	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] = visitor.table(toTableSQL)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y = visitor.table(toTableSQL)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: TableSQL[F_, T, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.grouping(this)

	private[sql] override def concrete_JoinedTable_subclass_must_extend_TableSQL(seal :Seal) :Unit = ()
}




object TableSQL {

	def apply[F <: RowProduct] :TableSQLFactory[F] = new TableSQLFactory[F] {}

	sealed trait TableSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		               (table :Table[M])
		               (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]],
		                         offset :RelationOffset[F, T] { type First = RowProduct AndFrom T }) :TableSQL[F, T, S] =
			TableSQL[F, T, S](cast(table), offset)
	}

	def apply[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	         (table :Table[T])(implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:TableSQL[RowProduct AndFrom T, T, S] =
		TableSQL[AndFromLast[T], T, S](table, 0)

	def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	         (table :Table[T], index :RelationOffset[F, T] { type First = RowProduct AndFrom T }) :TableSQL[F, T, S] =
		TableSQL[F, T, S](table, index.index, Unique.empty, Unique.empty)

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	                      (table :Table[T], index :Int,
	                       includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                       excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
			:TableSQL[F, T, S] =
	{
		val position = RelationOffset.unsafe[F, RowProduct AndFrom T, JoinedTable.Of[T]#T, T](index)
		new TableSQL[F, T, S](table, position, includes, excludes) with RowShapeCache
	}


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :TableSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[Table[MappingOf[X]#TypedProjection]] -> from.index)
			case _ => Lack
		}



	type __ = TableSQL[F, T, X] forSome {
		type F <: RowProduct;
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = TableSQL[O, T, R] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = TableSQL[O, T, R] forSome {
		type T[A] <: BaseMapping[R, A]; type O >: F <: RowProduct
	}

	type LastTable[T[A] <: BaseMapping[S, A], S] = TableSQL[RowProduct AndFrom T, T, S]

	def LastTable[T[A] <: BaseMapping[S, A], S](from :Table[T]) :LastTable[T, S] =
		TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](from, 0, Unique.empty, Unique.empty)


	private[sql] class CustomTableSQL[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	                   (protected override val underlying :TableSQL[F, T, S],
	                    protected override val customize  :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any],
	                    override val includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                    override val excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
		extends TableSQL[F, T, S](underlying.relation, underlying.position, includes, excludes)
		   with CustomRelationSQL[F, T, S, RowProduct AndFrom T]
	{
		override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any])
				:TableSQL[F, T, S] =
			if (includes.isEmpty && excludes.isEmpty)
				new CustomTableSQL[F, T, S](underlying, composed(alter))
			else
				new CustomTableSQL[F, T, S](this, alter)

		override def anchor(relation :Relation[T]) :TableSQL[F, T, S] =
			if (relation == table) this
			else underlying.anchor(relation).custom(customize)

		protected override def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
		                                             includes :Unique[TypedMapping[_, E]],
		                                             excludes :Unique[TypedMapping[_, E]]) :TableSQL[E, T, S] =
			if (includes.nonEmpty || excludes.nonEmpty)
				new CustomTableSQL(underlying.moveTo(position), customize, includes, excludes)
			else
				new CustomTableSQL[E, T, S](underlying.moveTo(position), customize) {
					override lazy val altered  = CustomTableSQL.this.altered.withOrigin[E]
					override lazy val anchored = CustomTableSQL.this.anchored.withOrigin[E]
				}
	}



	private[sql] class FinalTableSQL[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	                                (table :Table[T], definitive :TypedMapping[S, F],
	                                 offset :RelationOffset[F, T] { type First = RowProduct AndFrom T })
		extends TableSQL[F, T, S](table, offset, Unique.empty, Unique.empty)
		   with FinalRelationSQL[F, T, S, RowProduct AndFrom T]
	{
		override lazy val altered  :TypedMapping[S, F] = definitive
		override lazy val anchored :TypedMapping[S, F] = definitive

		override def isDefault :Boolean = anchored == relation.row || anchored == relation.export
		override def isCustom  :Boolean = !isDefault

		override def alterOther[C <: E, E <: RowProduct, M[A] <: MappingAt[A],
		                        R[f <: RowProduct] <: JoinedRelation[f, M] with JoinedRelationTemplate[f, M, R]]
		                       (rel :R[E], component :ComponentSQL[C, T] { type Origin = E; type Entity[A] = M[A] }) :R[E] =
			if (isDefault)
				rel
			else
				throw new UnsupportedOperationException(
					"Cannot alter " + rel.fullString + " like " + fullString + " because the latter is custom and final."
				)

		override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any])
				:TableSQL[F, T, S] =
			throw new UnsupportedOperationException(
				"Cannot alter a definitive version of table " + this + ": " + relation + "."
			)

		override def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
		                                   includes :Unique[TypedMapping[_, E]], excludes :Unique[TypedMapping[_, E]])
				:TableSQL[E, T, S] =
			if (includes.nonEmpty || excludes.nonEmpty)
				throw new UnsupportedOperationException(
					"Cannot alter a definitive version of table " + this + ": " + relation + "."
				)
			else
				new FinalTableSQL[E, T, S](relation, anchored.withOrigin[E], position)

		override lazy val toString = relation.refString + "@" + index + "(=" + anchored + ")"
	}



	trait SpecificTableVisitor[+F <: RowProduct, X, +Y] {
		def table[O >: F <: RowProduct, T[A] <: BaseMapping[X, A]](e :TableSQL[O, T, X]) :Y
	}
	type MatchSpecificTable[+F <: RowProduct, X, +Y] = SpecificTableVisitor[F, X, Y]
	type CaseSpecificTable[+F <: RowProduct, X, +Y] = SpecificTableVisitor[F, X, Y]
//
//	trait TableVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def table[O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V]
//		         (e :TableSQL[O, T, V]) :Y[Single, V, TableSQL[O, T, V]]
//	}
//	type MatchTable[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		TableVisitor[F, Y]
//	type CaseTable[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		TableVisitor[F, Y]

	trait AnyTableVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def table[O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R](e :TableSQL[O, T, R]) :Y[Single, R]
	}
	type MatchAnyTable[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyTableVisitor[F, Y]
	type CaseAnyTable[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyTableVisitor[F, Y]
}






/** The sole implementation of [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]. Extracted primarily
  * due to the necessity of a separate mapping `Subject` type `V` in Scala 2.
  *
  * @param relation
  * @param position
  * @param includes
  * @param excludes
  * @tparam U $U
  * @tparam F $F
  * @tparam T $M
  * @tparam V $V
  * @define Cons `GroupingSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.GroupingSQL GroupingSQL]]
  */
class GroupingSQL[-U <: RowProduct, F <: RowProduct, T[A] <: BaseMapping[V, A], V] protected
                 (override val relation :GroupingRelation[U, T],
                  override val position :RelationOffset[F, T] { type First = RowProduct AndBy T },
                  override val includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
                  override val excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
	extends RelationSQL[F, T, V, RowProduct AndBy T](relation, position)
	   with JoinedGrouping[U, F, T]
	   with JoinedRelationTemplate[F, T, ({ type R[f <: RowProduct] = GroupingSQL[U, f, T, V] })#R]
{
	@inline final override def toGroupingSQL :GroupingSQL[U, F, T, V] = origin

	override def asGroupingSQL :Some[GroupingSQL[U, F, T, V]] = Some(toGroupingSQL)

	override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[V, Unit]) => MappingOf[Any])
			:GroupingSQL[U, F, T, V] =
		new CustomGroupingSQL(this, alter)

	override def makeCustom :GroupingSQL[U, F, T, V] =
		if (isCustom)
			toGroupingSQL
		else
			new GroupingSQL[U, F, T, V](relation, position, includes, excludes)
				with CustomRelationMixin[F, T, GroupingSQL[U, F, T, V]]

	protected override def makeFinal :GroupingSQL[U, F, T, V] =
		if (isFinal)
			this
		else
			new GroupingSQL[U, F, T, V](relation, position, includes, excludes)
				with FinalRelationSQL[F, T, V, RowProduct AndBy T]

	override def anchor(relation :Relation[T]) :GroupingSQL[_ <: RowProduct, F, T, V] = relation match {
		case this.relation => this
		case grouping :GroupingRelation[d, T] =>
			GroupingSQL[d, F, T, V](
				grouping, index,
				includes.map(relation.row[F].counterpart(mapping, _)),
				excludes.map(relation.row[F].counterpart(mapping, _))
			)
		case _ =>
			throw new IllegalArgumentException(
				"Cannot recreate a GroupingSQL " + fullString + " for relation " + relation + " :" +
				relation.getClass.getName + "."
			)
	}

	protected override def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
	                                             includes :Unique[TypedMapping[_, E]],
	                                             excludes :Unique[TypedMapping[_, E]]) :GroupingSQL[U, E, T, V] =
		if (!(isDefault && relation.isDefault) && includes == this.includes && excludes == this.excludes)
			new GroupingSQL[U, E, T, V](relation, position, includes, excludes) with RowShapeCache {
				override lazy val altered  = GroupingSQL.this.altered.withOrigin[E]
				override lazy val anchored = GroupingSQL.this.anchored.withOrigin[E]
			}
		else
			GroupingSQL(relation, position.index, includes, excludes)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, T, V, M, V, FromLast]) :GroupingRelation[F, M] =
		//not too difficult to implement, but it would essentially be a constant within a grouped subselect
		throw new UnsupportedOperationException(
			"Cannot use a grouping relation component " + component + " as a new grouping expression."
		)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
		visitor.grouping(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.grouping(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: GroupingSQL[U, F_, T, V] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.grouping(this)


	private[sql] override def concrete_JoinedGrouping_subclass_must_extend_GroupingSQL(seal :Seal) :Unit = ()
}




object GroupingSQL {

	private[sql] def apply[D <: RowProduct, F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	                      (grouping :GroupingRelation[D, T], index :Int,
	                       includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                       excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
			:GroupingSQL[D, F, T, S] =
		{
			val position = RelationOffset.unsafe[F, GroupBy.Last[T], JoinedGrouping.Of[D, T]#T, T](index)
			new GroupingSQL[D, F, T, S](grouping, position, includes, excludes) with RowShapeCache
		}

	def apply[F <: FromSome, T[A] <: BaseMapping[S, A], S](relation :GroupingRelation[F, T])
			:GroupingSQL[F, RowProduct AndBy T, T, S] =
		GroupingSQL[F, RowProduct AndBy T, T, S](relation, 0)

	def apply[F <: FromSome, T[A] <: BaseMapping[S, A], S](component :ComponentSQL[F, T])
			:GroupingSQL[F, RowProduct AndBy T, T, S] =
		GroupingSQL[F, RowProduct AndBy T, T, S](component.asGrouping, 0)

	def apply[F <: FromSome, X](e :SQLExpression[F, Single, X])
			:GroupingSQL[F, RowProduct AndByVal X, Group[X]#M, X] =
		GroupingSQL[F, RowProduct AndBy Group[X]#M, Group[X]#M, X](GroupingRelation(e), 0)

	def apply[F <: FromSome, X](e :ColumnSQL[F, Single, X])
			:GroupingSQL[F, RowProduct AndByOne X, Group[X]#C, X] =
		GroupingSQL[F, RowProduct AndBy Group[X]#C, Group[X]#C, X](GroupingRelation(e), 0)



	type __ = GroupingSQL[_ <: RowProduct, F, T, X] forSome {
		type F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = GroupingSQL[Nothing, O, T, R] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = GroupingSQL[Nothing, O, T, R] forSome {
		type T[A] <: BaseMapping[R, A]; type O >: F <: RowProduct
	}

	type LastGrouping[F <: FromSome, T[A] <: BaseMapping[S, A], S] = GroupingSQL[F, GroupByClause AndBy T, T, S]



	private[sql] class CustomGroupingSQL
	                   [-D <: RowProduct, F <: RowProduct, T[A] <: BaseMapping[S, A], S]
	                   (protected override val underlying :GroupingSQL[D, F, T, S],
	                    protected override val customize :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any],
	                    override val includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                    override val excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
		extends GroupingSQL[D, F, T, S](underlying.relation, underlying.position, includes, excludes)
		   with CustomRelationSQL[F, T, S, RowProduct AndBy T]
	{
		override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[S, Unit]) => MappingOf[Any])
				:GroupingSQL[D, F, T, S] =
			if (includes.isEmpty && excludes.isEmpty)
				new CustomGroupingSQL[D, F, T, S](underlying, composed(alter))
			else
				new CustomGroupingSQL[D, F, T, S](this, alter)

		override def anchor(relation :Relation[T]) :GroupingSQL[_ <: RowProduct, F, T, S] =
			if (relation == grouping) origin
			else underlying.anchor(relation).custom(customize).origin

		protected override def copy[E <: RowProduct](position :RelationOffset[E, T] { type First = FromLast },
		                                             includes :Unique[TypedMapping[_, E]],
		                                             excludes :Unique[TypedMapping[_, E]]) :GroupingSQL[D, E, T, S] =
			if (includes.nonEmpty || excludes.nonEmpty)
				new CustomGroupingSQL[D, E, T, S](underlying.moveTo(position), customize, includes, excludes)
			else
				new CustomGroupingSQL[D, E, T, S](underlying.moveTo(position), customize) {
					override lazy val altered  = CustomGroupingSQL.this.altered.withOrigin[E]
					override lazy val anchored = CustomGroupingSQL.this.anchored.withOrigin[E]
				}
	}



	trait SpecificGroupingVisitor[+F <: RowProduct, X, +Y] {
		def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[X, A]](e :GroupingSQL[D, O, T, X]) :Y
	}
	type MatchSpecificGrouping[+F <: RowProduct, X, +Y] = SpecificGroupingVisitor[F, X, Y]
	type CaseSpecificGrouping[+F <: RowProduct, X, +Y] = SpecificGroupingVisitor[F, X, Y]
//
//	trait GroupingVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[V, A], V]
//		            (e :GroupingSQL[D, O, T, V]) :Y[Single, V, GroupingSQL[D, O, T, V]]
//	}
//	type MatchGrouping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		GroupingVisitor[F, Y]
//	type CaseGrouping[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		GroupingVisitor[F, Y]

	trait AnyGroupingVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def grouping[D <: FromSome, O >: F <: RowProduct, T[A] <: BaseMapping[R, A], R]
		            (e :GroupingSQL[D, O, T, R]) :Y[Single, R]
	}
	type MatchAnyGrouping[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyGroupingVisitor[F, Y]
	type CaseAnyGrouping[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyGroupingVisitor[F, Y]
}






/** The sole implementation of [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]]. Extracted primarily
  * due to the necessity of a separate mapping `Subject` type `V` in Scala 2. Unlike the former,
  * it always uses [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[V, O]` as its mapping type.
  *
  * @param relation
  * @param position
  * @tparam F $F
  * @tparam V The value of the parameter.
  * @tparam L $L
  * @define Cons `ParamSQL`
  * @define link [[net.noresttherein.oldsql.sql.ast.ParamSQL ParamSQL]].
  */
class ParamSQL[F <: RowProduct, V, L <: RowProduct] protected
              (override val relation :ParamRelation[V],
               override val position :RelationOffset[F, ParamRelation[V]#Param] { type First = L })
	extends RelationSQL[F, ParamRelation[V]#Param, V, L](relation, position)
	   with JoinedParam[F, ParamRelation[V]#Param]
	   with JoinedRelationTemplate[F, ParamRelation[V]#Param, ({ type R[f <: RowProduct] = ParamSQL[f, V, L] })#R]
{ self =>
//	protected[sql] override def upcast :ParamSQL[F, X, L] = origin

	def param :ParamRelation[V] = relation
//	override val origin :ParamSQL[F, X, L] = this
	override def includes :Unique[TypedMapping[_, F]] = Unique.empty
	override def excludes :Unique[TypedMapping[_, F]] = Unique.empty

	@inline final override def toParamSQL :ParamSQL[F, V, L] = this

	override def asParamSQL :Some[ParamSQL[F, V, L]] = Some(this)

	override def default :ParamSQL[F, V, L] = this

	override def defaultWith(includes :Unique[TypedMapping[_, F]], excludes :Unique[TypedMapping[_, F]])
			:ParamSQL[F, V, L] =
		this

	override def include(components :Iterable[TypedMapping[_, F]]) :ParamSQL[F, V, L] = this
	override def exclude(components :Iterable[TypedMapping[_, F]]) :ParamSQL[F, V, L] = this
	override def include(components :UnboundParam[V, F] => TypedMapping[_, F]*) :ParamSQL[F, V, L] = this
	override def exclude(components :UnboundParam[V, F] => TypedMapping[_, F]*) :ParamSQL[F, V, L] = this

	override def alter(includes :Iterable[TypedMapping[_, F]], excludes :Iterable[TypedMapping[_, F]])
			:ParamSQL[F, V, L] =
		this

	override def alter(components :UnboundParam[V, F] => ComponentSelection[_, F]*) :ParamSQL[F, V, L] = this


	override def +-(components :Iterable[UnboundParam[V, F] => ComponentSelection[_, F]]) :ParamSQL[F, V, L] = this
	override def +(component :UnboundParam[V, F] => TypedMapping[_, F]) :ParamSQL[F, V, L] = this
	override def -(component :UnboundParam[V, F] => TypedMapping[_, F]) :ParamSQL[F, V, L] = this
//
//	override def alterLike[E <: RowProduct](template :JoinedRelation[E, ParamRelation[V]#Param]) :ParamSQL[F, V, L] =
//		template.typed.alterOther[F, F, ParamRelation[V]#Param, L, ParamSQL[F, V, L]](this, this)
//
//	override def reorder(permutation :IndexedSeq[Int]) :ParamSQL[F, V, L] =
//		ParamSQL[F, V, L](ParamRelation(param.name)(param.form.reorder(permutation)), position)

	override def reorder(precedes :(TypedColumn[_, F], TypedColumn[_, F]) => Boolean) :ParamSQL[F, V, L] =
		throw new UnsupportedOperationException("Columns of a parameter relation " + this + " cannot be reordered.")

	override def custom(alter :(TypedMapping[Any, Unit], TypedMapping[V, Unit]) => MappingOf[Any])
			:ParamSQL[F, V, L] =
		throw new UnsupportedOperationException("Cannot customize the view of parameter relation " + this + ".")

	override def makeCustom :ParamSQL[F, V, L] =
		???
//		if (isCustom)
//			this
//		else
//			new ParamSQL[F, V, L](param, position)
//				with CustomRelationMixin[F, ParamRelation[V]#Param, ({ type R[f <: RowProduct] = ParamSQL[f, V, L] })#R]

	protected override def makeFinal :ParamSQL[F, V, L] =
		???
//		if (isFinal)
//			this
//		else
//			new ParamSQL[F, V, L](relation, position)
//				with FinalRelationSQL[F, ParamRelation[V]#Param, V, L]

	override def anchor(relation :Relation[ParamRelation[V]#Param]) :ParamSQL[F, V, L] =
		???
//		relation match {
//			case this.relation => this
//			case param :ParamRelation[V @unchecked] => ParamSQL(param, position.index)
//			case _ =>
//				throw new IllegalArgumentException(
//					"Cannot recreate a ParamSQL " + this.fullString + " for a relation which is not a ParamRelation: " +
//					relation + " :" + relation.getClass.getName + "."
//				)
//		}

//	/** A new `ParamSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
//	  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
//	  */ //todo: if we refactor ParamSQL and JoinParam to accept any mapping, a JoinedTable may be translated to a JoinParam and vice versa
//	override def moveTo[E <: RowProduct](offset :RelationOffset[E, ParamRelation[X]#Param] { type First = FromLast })
//			:ParamSQL[E, X, L] =
//		ParamSQL[E, X, L](relation, offset.index)
//
//	//overrides necessary because ParamSQL doesn't extend RelationSQL 'for ParamSQL'; can go away if we remove the UnboundParam upper bound
//	override def basedOn[U <: F, E <: RowProduct]
//	                    (base :E)(implicit expansion :U PartOf E) :ParamSQL[_ >: E <: RowProduct, X, L] =
//		ParamSQL[E, X, L, E](relation, index + expansion.lengthDiff)
//
//	override def expand[U <: F, E <: RowProduct]
//	                   (base :E)(implicit expansion :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
//			:ParamSQL[_ >: E <: RowProduct, X, L] =
//		ParamSQL[E, X, L](relation, index + expansion.length)
//
//	override def expand[E <: RowProduct](implicit expansion :F ExpandedBy E) :ParamSQL[_ >: E <: RowProduct, X, L] =
//		ParamSQL[E, X, L](relation, index + expansion.length)
//
//	override def asIn[J[+A <: F] <: A Expanded T forSome { type T[Z] <: MappingAt[Z] }] :ParamSQL[J[F], X, L] =
//		ParamSQL[J[F], X, L, J[F]](relation, index + 1)
//
//	override def asIn[E <: RowProduct](implicit expansion :F PrefixOf E) :ParamSQL[E, X, L] =
//		ParamSQL(relation, index + expansion.lengthDiff)

	override def asLast :ParamSQL[L, V, L] = ??? //ParamSQL[L, V, L](relation, 0)


	protected override def copy[E <: RowProduct](position :RelationOffset[E, ParamRelation[V]#Param] { type First = FromLast },
	                                             includes :Unique[TypedMapping[_, E]],
	                                             excludes :Unique[TypedMapping[_, E]]) :ParamSQL[E, V, L] =
		???
//		ParamSQL[E, V, L](relation, position.index)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, ParamRelation[V]#Param, V, M, V, L])
			:GroupingRelation[F, M] =
		???
//		GroupingRelation.param[F, M, V](component)(OriginProjection.isomorphism)


	protected override def defaultSpellingOf[P, C <: F, M[A] <: MappingAt[A]]
	                       (component :ComponentSQL[C, M] { type Origin = F; type Entity[A] = UnboundParam[V, A] },
	                        inline :Boolean)
	                       (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		???
//		relation.spell(spelling)(this, component.mapping, inline)(from, context, params)

	protected override def explodedSpellingOf[P, C <: F, M[A] <: MappingAt[A]]
	                       (component :ComponentSQL[C, M] { type Origin = F; type Entity[A] = UnboundParam[V, A] },
	                        independent :Boolean)
	                       (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		???
//		relation.spellExploded(spelling)(this, component.mapping, independent)(from, context, params)


	protected override def visit[Y[-_ >: Grouped <: Single, _]]
	                            (visitor :AnyExpressionVisitor[F, Y]) :Y[Single, V] =
		visitor.param(this)

	protected override def visit[Y](visitor :SpecificExpressionVisitor[F, Single, V, Y]) :Y =
		visitor.param(this)
//
//	protected override def visit[F_ <: F, S_ >: Grouped <: Single,
//	                             E >: ParamSQL[F_, V, L] <: SQLExpression[F_, S_, V],
//	                             Y[-s >: Grouped <: Single, v, -e <: SQLExpression[F_, s, v]]]
//	                            (visitor :ExpressionVisitor[F_, Y]) :Y[S_, V, E] =
//		visitor.param(this)

	private[sql] override def concrete_JoinedParam_subclass_must_extend_ParamSQL(seal :Seal) :Unit = ()
}




object ParamSQL {

	def apply[F <: RowProduct] :ParamSQLCurriedConstructor[F] = new ParamSQLCurriedConstructor[F] {}

	sealed trait ParamSQLCurriedConstructor[F <: RowProduct] extends Any {
		final def apply[X](param :ParamRelation[X])
		                  (implicit offset :RelationOffset[F, ParamRelation[X]#Param]) :ParamSQL[F, X, offset.First] =
			???
//			ParamSQL[F, X](param, offset)
	}

	def join[F <: RowProduct, X]
	        (param :ParamRelation[X],
	         offset :RelationOffset[F, ParamRelation[X]#Param] { type First = RowProduct AndFrom ParamRelation[X]#Param })
			:ParamSQL[F, X, RowProduct AndFrom ParamRelation[X]#Param] =
		???
//		ParamSQL(param, offset.index)

	def group[F <: RowProduct, X]
	         (param :ParamRelation[X],
	          offset :RelationOffset[F, ParamRelation[X]#Param] { type First = RowProduct AndBy ParamRelation[X]#Param })
			:ParamSQL[F, X, RowProduct AndBy ParamRelation[X]#Param] =
		???
//		ParamSQL(param, offset.index)

	def apply[F <: RowProduct, X](param :ParamRelation[X], offset :RelationOffset[F, ParamRelation[X]#Param])
			:ParamSQL[F, X, offset.First] =
		???
//		new ParamSQL[F, X, offset.First](param, offset) with RowShapeCache

	private[sql] def apply[F <: RowProduct, X, L <: RowProduct]
	                      (param :ParamRelation[X], index :Int,
	                       includes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]],
	                       excludes :Unique[TypedMapping[_, F]] = Unique.empty[TypedMapping[_, F]])
			:ParamSQL[F, X, L] =
		???
//		ParamSQL[F, X, L](param, index, Unique.empty, Unique.empty)

	def join[X](param :ParamRelation[X]) :LastParam[X] = ??? //ParamSQL(param, 0)

	def group[X](param :ParamRelation[X]) :LastGroupParam[X] = ??? //ParamSQL(param, 0)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(ParamRelation[X], Int)] =
		e match {
			case from :ParamSQL[_, X, _] => Got(from.relation -> from.index)
			case _ => Lack
		}


	type __ = ParamSQL[F, X, L] forSome {
		type F <: RowProduct; type L <: RowProduct; type X
	}

	type AnyIn[-F <: RowProduct] = ParamSQL[O, X, L] forSome {
		type X; type L <: RowProduct; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, L <: RowProduct, X] = ParamSQL[_ >: F <: RowProduct, X, L]
	//consider: renaming to LastParamSQL (and LastTable, LastGrouping, etc.)
	type LastParam[X] = ParamSQL[WithParam.Last[X], X, WithParam.Last[X]]

	def LastParam[X](from :ParamRelation[X]) :LastParam[X] = ParamSQL[WithParam.Last[X], X, WithParam.Last[X]](from, 0)

	type LastGroupParam[X] = ParamSQL[ByParam.Last[X], X, ByParam.Last[X]]

	def LastGroupParam[X](from :ParamRelation[X]) :LastGroupParam[X] = ParamSQL[ByParam.Last[X], X, ByParam.Last[X]](from, 0)


	trait SpecificParamVisitor[+F <: RowProduct, X, +Y] {
		def param[O >: F <: RowProduct, L <: RowProduct](e :ParamSQL[O, X, L]) :Y
	}
	type MatchSpecificParam[+F <: RowProduct, X, +Y] = SpecificParamVisitor[F, X, Y]
	type CaseSpecificParam[+F <: RowProduct, X, +Y] = SpecificParamVisitor[F, X, Y]
//
//	trait ParamVisitor[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] {
//		def param[O >: F <: RowProduct, V, L <: RowProduct](e :ParamSQL[O, V, L]) :Y[Single, V, ParamSQL[O, V, L]]
//	}
//	type MatchParam[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		ParamVisitor[F, Y]
//	type CaseParam[+F <: RowProduct, +Y[-S >: Grouped <: Single, V, -e <: SQLExpression[F, S, V]]] =
//		ParamVisitor[F, Y]

	trait AnyParamVisitor[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] {
		def param[O >: F <: RowProduct, X, L <: RowProduct](e :ParamSQL[O, X, L]) :Y[Single, X]
	}
	type MatchAnyParam[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyParamVisitor[F, Y]
	type CaseAnyParam[+F <: RowProduct, +Y[-_ >: Grouped <: Single, _]] = AnyParamVisitor[F, Y]
}

