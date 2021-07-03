package net.noresttherein.oldsql.sql.ast

import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.{Chain, Opt, ReversedList, Unique}
import net.noresttherein.oldsql.exceptions.Bug
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{ColumnMapping, MappingExtract, Relation, SQLForm, SQLWriteForm, Table}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.IsomorphicProjection
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.{AndBy, AndByOne, AndByVal, AndFrom, By, ByOne, ByVal, ColumnSQL, ComponentSetter, Expanded, FromSome, GroupBy, GroupByClause, GroupByOne, GroupByVal, RowProduct, Select, SQLExpression, WithClause}
import net.noresttherein.oldsql.sql.GroupByClause.{Group, GroupingRelation}
import net.noresttherein.oldsql.sql.ParamClause.{UnboundParam, ParamRelation}
import net.noresttherein.oldsql.sql.RowProduct.{ExactSubselectOf, ExpandedBy, GroundFrom, GroupingOfGeneralized, NonEmptyFrom, PartOf, PrefixOf, TopFrom}
import net.noresttherein.oldsql.sql.Select.SelectMapping
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.{ExpressionVisitor, GlobalScope, LocalScope, SQLTypeUnification}
import net.noresttherein.oldsql.sql.ast.ColumnComponentSQL.TypedColumnComponentSQL
import net.noresttherein.oldsql.sql.ast.ComponentSQL.TypedComponentSQL
import net.noresttherein.oldsql.sql.ast.GroupingSQL.GroupingVisitor
import net.noresttherein.oldsql.sql.ast.JoinedRelation.JoinedRelationTemplate
import net.noresttherein.oldsql.sql.ast.ParamSQL.ParamVisitor
import net.noresttherein.oldsql.sql.ast.RelationSQL.RelationSQLTemplate
import net.noresttherein.oldsql.sql.ast.SelectAs.{SubselectMapping, TopSelectMapping}
import net.noresttherein.oldsql.sql.ast.TableSQL.TableVisitor
import net.noresttherein.oldsql.sql.mechanics.{SpelledSQL, TableCount, TableOffset}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] for a single instance/occurrence of a relation
  * used by a larger SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] expression (most notably within its
  * ''from'' clause), representing the whole [[net.noresttherein.oldsql.schema.Relation relation]] as a column set.
  * It serves three functions:
  *   1. as a component expression for the whole row, with the full entity type as its value, to be used
  *      as a single value, for example within a ''select'' clause of an SQL ''select'';
  *   1. as a factory of component expressions for components (and columns) of the mapping for the represented relation;
  *   1. as the origin of mapping instances for components of relation mapping `T`, sharing the `RowProduct` subtype
  *      `F`, being the domain of this expression, as their [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
  *      type, allowing their identification and matching with the originating relation. Because of this function
  *      it is often called the ''origin relation'', or simply ''origin'' of a component mapping or
  *      a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] instance.
  *
  * This interface has a minimized signature and is primarily used by
  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] subclasses to represent each relation available to
  * expressions using the particular ''from'' clause as their domain. The relation object (both this value
  * and wrapped `Relation`) may be a relation only in the abstract sense, and not a valid ''derived table'';
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
  *      in the same way as table expressions in ungrouped SQL ''selects'';
  *   1. [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]] - synthetic entities representing query/statement
  *      parameters of unknown values, which do not exist in the final SQL other than at reference locations
  *      in the form of '?' JDBC parameter placeholders.
  *
  * Note that this type is invariant in its `RowProduct` domain `F`: this is because it is used as the `Origin` type
  * of the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]] `T[F]` for this relation.
  * As the `Origin` type of a mapping is shared by all its components, it allows matching any subcomponent
  * or column of this relation - or another instance, sharing `F` type parameter, and thus for the same relation `T`
  * occurring at the same position in the ''from''/''group by'' clause - back to the originating relation.
  * This is leveraged when a mapping instance is implicitly converted
  * to a [[net.noresttherein.oldsql.sql.ast.LooseComponent LooseComponent]], but the invariance is inconvenient
  * when using this instance as an actual expression. For this reason - and in order to use here the wider upper bound
  * of `Mapping`, rather than [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] actually
  * required to create instances of this trait by the implementation - every instance of `JoinedRelation`
  * is created for its subclass [[net.noresttherein.oldsql.sql.ast.RelationSQL RelationSQL]].
  * @tparam F the domain of of this expression, that is a ''from'' clause type (including any implicitly inherited
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
  * @tparam T a type constructor for the mapping for the whole relation, accepting its `Origin` type.
  */ //consider: renaming to OriginRelation
trait JoinedRelation[F <: RowProduct, T[A] <: MappingAt[A]]
	extends ComponentSQL[F, T] with JoinedRelationTemplate[F, T, JoinedRelation[F, T]]
{
	override type Origin = F
	override type Entity[A] = T[A]

	type Self = RelationSQL[F, M, T[F]#Subject, F] forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

	/** Offset of this relation in the clause `F`, counting ''from right to left'' and starting with 0. */
	def offset :Int
	override def position :TableOffset[F, T] = new TableOffset(offset)
	override val relation :Relation[T]
	override val mapping  :T[F]

	/** List of components of mapping `T` which should be included in the rendered SQL for this expression,
	  * as long as they are allowed for use in that context by the buffs carried by their export versions.
	  * The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.export export]] mapping of this expression
	  * is the [[net.noresttherein.oldsql.schema.Relation.export export]] mapping of its
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]] additionally adjusted
	  * by passing [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] of this instance
	  * to its [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable[Component[_],exclude:Iterable[Component[_]) apply]]`(includes, excludes)`
	  *
	  * Any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] using this relation as its origin
	  * uses `this.export` as the root mapping of its component, meaning the adjustment of the column set
	  * is propagated to it.
	  */
	def includes :Unique[RefinedMapping[_, F]]

	/** List of components of mapping `T` which should be excluded in the rendered SQL for this expression,
	  * as long as they are allowed for use in that context by the buffs carried by their export versions.
	  * The [[net.noresttherein.oldsql.sql.ast.JoinedRelation.export export]] mapping of this expression
	  * is the [[net.noresttherein.oldsql.schema.Relation.export export]] mapping of its
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]] additionally adjusted
	  * by passing [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] of this instance
	  * to its [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable[Component[_],exclude:Iterable[Component[_]) apply]]`(includes, excludes)`
	  *
	  * Any [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] using this relation as its origin
	  * uses `this.export` as the root mapping of its component, meaning the adjustment of the column set
	  * is propagated to it.
	  */
	def excludes :Unique[RefinedMapping[_, F]]

	/** Casts down this relation to its implementation interface; this is a guaranteed safe cast. */
	def toRelationSQL :RelationSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }

	/** Casts down this instance to more strongly typed `RelationSQL`. The result type is an `Option`
	  * not because the cast may fail, but because the correct existential subtype cannot exist as a value
	  * (returning simply the existentially refined `RelationSQL` produces a compiler error about being
	  * unable to abstract over existential higher type `T`).
	  */
	def asRelationSQL :Option[RelationSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }]

	/** Casts this relation down to more strongly typed `TableSQL` if it is a table (persistent or derived). */
	def asTableSQL :Option[TableSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None

	/** Casts down this instance to more strongly typed `ParamSQL` if it is a pseudo relation for an unbound parameter. */
	def asParamSQL :Option[ParamSQL[F, T[F]#Subject, F]] = None

	/** Casts down this instance to more strongly typed `ParamSQL` if it is a pseudo relation representing an expression
	  * in a ''group by'' clause.
	  */
	def asGroupingSQL :Option[GroupingSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }] = None

	/** A new `JoinedRelation` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedRelation[E, T]

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedRelation[E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedRelation`. The `expand` method cannot be overriden here to return a `JoinedRelation`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedRelation[G, T]

	override def withClause :WithClause = relation.withClause


	/** Renders the declaration of this relation, that is the form in which it should appear in the ''from''
	  * (or ''group by'') clause `C`. [[net.noresttherein.oldsql.sql.ast.JoinedTable Tables]] will return their name,
	  * possibly with an ''as'' clause, and add a new alias to `context` using method
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.table table]] of `spelling`.
	  * [[net.noresttherein.oldsql.sql.ast.JoinedParam Parameters]] return an empty string, adding only an entry
	  * to `context`, while [[net.noresttherein.oldsql.sql.ast.JoinedGrouping grouping expressions]] will print
	  * all constituent column expressions separated with ','. This method should be called only for the last
	  * relation in a clause, i.e. `this` must equal `from.last`.
	  *
	  * As an instance of this class semantically represents a placeholder reference to a relation in
	  * any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance conforming
	  * to its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.Origin Origin]] type `F`,
	  * returned SQL fragment reflects ''the corresponding relation in ''`from`'', rather than this one''.
	  * In most cases it doesn't matter, as expressions are typically included in the same ''select''
	  * as the ''from'' clause instance which was used as the source of referenced relations. Moreover,
	  * even if an expression has been created separately, in most cases it will be equal to the one included in `from`:
	  * mapping type typically uniquely identify source tables in the schema. However, it is possible that another
	  * relation is used than the one included in this instance, either due to several tables with the same mapping,
	  * or because a table has been altered by
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.include including]]
	  * or [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.exclude excluding]] certain columns.
	  *
	  * This has a particularly striking implications
	  * for [[net.noresttherein.oldsql.sql.ast.JoinedGrouping JoinedGrouping]]: the expression defined by the relation
	  * used in any SQL expression will be ignored and replaced with the one of the instance at the same position
	  * in `from`.
	  */
//	def spell[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
//	            (implicit spelling :SQLSpelling) :SpelledSQL[P]


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
	  * Default implementation delegates to [[net.noresttherein.oldsql.sql.ast.JoinedRelation.spellExploded spellExploded]]
	  * and simply combines the results for individual columns into a single fragment. Extending classes may resort
	  * to other schemes.
	  * @param component a component expression using `this` relation
	  *                  as its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] which,
	  *                  together with the `export` mapping for the whole relation and
	  *                  [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] of `spelling`,
	  *                  define the column set which should be included in the rendered SQL.
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
	  */
	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	def spell[P, C <: F, M[O] <: MappingAt[O]]
	         (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }, inline :Boolean = false)
	         (from :C, context :SQLContext[P], params :Parameterization[P, C])
	         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//		from.relations(position).defaultSpelling(component, inline)(from, context, params)
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a relation different than " + this + "."
			)
		else {
			val columns = spellExploded(component)(from, context, params).filter(_.nonEmpty)
			if (columns.isEmpty)
				SpelledSQL(context)
			else if (columns.sizeIs <= 1)
				columns.head
			else if (inline)
				columns.reduce(_ +: ", " +: _)
			else
				("(" +: columns.reduce(_ +: ", " +: _)) + ")"
		}

	/** Creates an SQL fragment referencing the given column of the mapping for this relation inside
	  * an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]].
	  *   1. For a table column (including ''derived'' tables, i.e. ''select'' expressions), this translates to
	  *      the form of `alias.columnName`.
	  *   1. [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation Unbound]] parameters insert the JDBC statement
	  *      parameter placeholder '?' and append a setter form for the parameter
	  *      to the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.Parameterization Parameterization]]
	  *      returned with the SQL.
	  *   1. [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation Grouping]] relations always copy the whole
	  *      expression as appearing in the ''group by'' clause, together with repetitions of any bound or unbound
	  *      parameters included in the expression.
	  * This method is used when formatting clauses using the relation, such as ''where'' and ''having'',
	  * but never declaration clauses such as ''from'' or ''group by''.
	  *
	  * Note that, since a `JoinedRelation` represents semantically a reference to the relation declared at the given
	  * position in any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] instance conforming
	  * to its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.Origin Origin]] type, when rendering an SQL expression,
	  * it is always the counterpart of this relation in the `RowProduct` used as the ''from'' clause/domain
	  * that is called, rather than the instance included in the SQL expression being rendered. However, unlike
	  * in the case of the overloaded method accepting a component mapping, the column will ''always'' be printed,
	  * even if its export version provided by the relation is not included by default in the operation type/scope
	  * defined by `spelling`, as defined by the [[net.noresttherein.oldsql.schema.Buffs Buffs]] carried by the column.
	  * This means that calling this method can have a different result than calling `spell` after upcasting `column`
	  * to `RefinedMapping[V, F]`.
	  * @param column an ''export'' column of the [[net.noresttherein.oldsql.sql.ast.RelationSQL.export export]] mapping
	  *               of this relation.
	  */
//	def spell[P, C <: F](column :ColumnMapping[_, F])(from :C, context :SQLContext[P], params :Parameterization[P, C])
//	                    (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//		from.relations(position).defaultSpelling(column)(from, context, params)

	/** Spells each column of `component` individually, returning them as a sequence.
	  * The actual column set is defined by the [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]] version
	  * of the component (which takes into account [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]]
	  * and [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] lists of columns/subcomponents
	  * of this relation) and the [[net.noresttherein.oldsql.sql.SQLDialect.SpellingScope SpellingScope]] of
	  * passed `spelling` implicit argument.
	  *
	  * This method is used as implementation target of method
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.spell spell]] and in circumstances
	  * where a custom concatenation strategy is required (for example, as part of an SQL ''update's'' ''set'' clause).
	  * @param component a component expression using `this` relation
	  *                  as its [[net.noresttherein.oldsql.sql.ast.ComponentSQL.origin origin]] which,
	  *                  together with the `export` mapping for the whole relation and
	  *                  [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.scope scope]] of `spelling`,
	  *                  define the column set which should be included in the rendered SQL.
	  * @param from      the full ''from'' clause (that is, including an optional ''group by'' clause and any tables
	  *                  inherited from outer ''select'' expressions) used by the SQL ''select'' of which
	  *                  the component is a fragment. It is the counterpart of this relation instance in this
	  *                  clause that determines the exact spelling, rather than this instance.
	  * @param context   additional spelling information required for formatting the whole SQL select, in particular
	  *                  the list of aliases used by all listed tables.
	  * @param params    a factory of `P => X` accessor functions, where `P` is the intended parameter(s) type
	  *                  of the query being built.
	  * @param spelling  a formatting strategy used for all fragments of the returned SQL, responsible in particular
	  *                  for implementing any deviations from the SQL standard demanded by the DBMS being used.
	  */
	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	def spellExploded[P, C <: F, M[O] <: MappingAt[O]]
	                 (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                 (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                 (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]]
//		from.relations(position).explodedSpelling(component)(from, context, params)



	/** Checks if this instance and the argument use the same [[net.noresttherein.oldsql.schema.Relation Relation]]
	  * and have the same offset. When comparing relations from the same
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], this attests that the two relations refer to
	  * the same alias in the ''from'' clause (or a grouping expression in the ''group by'' clause),
	  * ignoring `includes` and `excludes` lists which alter the column set for all components of this relation.
	  */
	def same(that :JoinedRelation.*) :Boolean = relation == that.relation && offset == that.offset

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedRelation.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case relation :JoinedRelation.* if (this canEqual relation) && (relation canEqual this) =>
			relation.offset == offset && relation.mapping == mapping &&
				relation.includes.toSet == includes.toSet && relation.excludes.toSet == excludes.toSet
		case _ => false
	}
	override def hashCode :Int = offset * 31 + mapping.hashCode

	override lazy val toString :String =
		if (includes.isEmpty && excludes.isEmpty)
			relation.refString + "#" + offset
		else
			(includes.view.map("+" + _) ++ excludes.view.map("-" + _)).mkString(
				relation.refString + "#" + offset + "(", ",", ")"
			)


	private[sql] def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing
}




object JoinedRelation {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :JoinedRelation.Typed[F, X] @unchecked => Got(from.relation -> from.offset)
			case _ => Lack
		}


	type * = JoinedRelation[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[M[O] <: MappingAt[O]] = JoinedRelation[RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedRelation[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedRelation[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedRelation[O, M] }


	trait JoinedRelationTemplate[F <: RowProduct, T[O] <: MappingAt[O], +R <: JoinedRelation[F, T] with JoinedRelationTemplate[F, T, R]]
		extends ComponentSQL[F, T]
	{ this :R =>
		override type Origin = F
		override val origin  :R = this
		override def default :R

		/** `this.offset` as an evidence class vouching that table `T` indeed occurs in `F` at this position.
		  * This type is used as implicit evidence by many methods, in particular factories of `JoinedRelation`
		  * and `ComponentSQL` subclasses. Note that the exact subtype of the returned `JoinedRelation` is consistent
		  * with this instance.
		  */
		def position :TableOffset[F, T] { type Rel <: R } =
			(new TableOffset[F, T](offset)).asInstanceOf[TableOffset[F, T] { type Rel <: R }]

		/** Returns the relation in the same position in `clause` as this instance
		  * (based on its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.offset offset]] property).
		  */
		override def anchor(clause :F) :R = clause.relations((this :JoinedRelationTemplate[F, T, R]).position)

		@inline private final def result(includes :Unique[RefinedMapping[_, F]], excludes :Unique[RefinedMapping[_, F]]) :R =
			(this :JoinedRelationTemplate[F, T, R]).alter(includes, excludes)

		//fixme: the name is the same, but includes and excludes *replace* those on this instacnce.
		def alter(includes : Unique[RefinedMapping[_, F]], excludes :Unique[RefinedMapping[_, F]]) :R

		override def alter(components :T[F] => ComponentSelection[_, F]*) :R = this +- components

		override def +-(components :Iterable[T[F] => ComponentSelection[_, F]]) :R = {
			val newExcludes = components.view.map(_(mapping)).collect {
				case ExcludedComponent(c) => c
			}.to(Unique)
			val newIncludes = components.view.map(_(mapping)).collect {
				case IncludedComponent(c) => c
			}.to(Unique)
			result(
				(includes.view ++ newIncludes).filterNot(newExcludes.toSet).to(Unique),
				(excludes.view.filterNot(newIncludes.toSet) ++ newExcludes).to(Unique)
			)
		}

		override def include(components :Iterable[RefinedMapping[_, F]]) :R =
			if (components.isEmpty) this
			else {
				val newIncludes = components.view.map(mapping.export).to(Unique)
				result(includes ++ newIncludes, excludes.filterNot(newIncludes.toSet))
			}

		override def exclude(components :Iterable[RefinedMapping[_, F]]) :R =
			if (components.isEmpty) this
			else {
				val newExcludes = components.view.map(export.export(_)).to(Unique)
				result(includes.filterNot(newExcludes.toSet), excludes ++ newExcludes)
			}

		override def include(components :T[F] => RefinedMapping[_, F]*) :R =
			include(components.view.map(_(mapping)))

		override def exclude(components :T[F] => RefinedMapping[_, F]*) :R =
			exclude(components.view.map(_(mapping)))

		override def +(component :T[F] => RefinedMapping[_, F]) :R =
			include(Unique.single(component(mapping)))

		override def -(component :T[F] => RefinedMapping[_, F]) :R =
			exclude(Unique.single(component(mapping)))
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
  *   1. as a factory of component expressions for components (and columns) of the mapping for the represented relation;
  *   1. as the origin of mapping instances for components of table mapping `T`, sharing the `RowProduct` subtype `F`,
  *      being the domain of this expression, as their [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type,
  *      allowing their identification and matching with the originating relation. Because of this function
  *      it is often called the ''origin table'', or simply ''origin'', of a component mapping or
  *      a [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]] instance.
  *
  * When used within larger SQL expressions, it is either rendered as an SQL tuple/sequence of all
  * [[net.noresttherein.oldsql.schema.Mapping.selectedByDefault selectable by default]] columns of the table
  * or, when used inside a comparison expression (such as [[net.noresttherein.oldsql.sql.SQLExpression.=== ===]]),
  * it can be split and result in a conjunction of column vs column comparisons between the both sides.
  *
  * @tparam F the domain of of this expression, that is a ''from'' clause type (including any implicitly inherited
  *           ''from'' clauses of outer ''select'' expressions). While `RowProduct` itself is used here
  *           as the upper bound, all instances are created for `F <: `[[net.noresttherein.oldsql.sql.FromSome FromSome]];
  *           this relaxation increases the flexibility of its use, in particular allows matching any
  *           `JoinedRelation[F, T]` with `JoinedTable[F, T]` type. It always starts
  *           with `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]` T`, which is the upper bound
  *           of all [[net.noresttherein.oldsql.sql.Join joins]] and ''from'' clauses ending with the same tables
  *           following `T`. The type parameter is invariant and used
  *           as [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the exported table mapping
  *           and all its components.
  *           Thus, any [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]]`[T, F]` is presumed
  *           to be a component coming from mapping `T` and used to reference that particular occurrence of `T` in `F`.
  * @tparam T a type constructor for the mapping for the whole relation, accepting its `Origin` type.
  */
trait JoinedTable[F <: RowProduct, T[A] <: MappingAt[A]]
	extends JoinedRelation[F, T] with JoinedRelationTemplate[F, T, JoinedTable[F, T]]
{
	override val relation :Table[T]
	def table :Table[T] = relation

	override def position :TableOffset[F, T] { type Rel = JoinedTable[F, T] } =
		new TableOffset(offset).asInstanceOf[TableOffset[F, T] { type Rel = JoinedTable[F, T] }]


	/** Casts down this table to its implementation interface; this is a guaranteed safe cast. */
	def toTableSQL :TableSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }


	/** A new `JoinedTable` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedTable[E, T]

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	override def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedTable[E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedTable`. The `expand` method cannot be overriden here to return a `JoinedTable`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	override def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedTable[G, T]


	/** Renders the declaration of this relation, that is the form in which it should appear in the ''from'' clause `F`.
	  * This will return either the [[net.noresttherein.oldsql.schema.BaseTable.name name]] of
	  * the [[net.noresttherein.oldsql.sql.ast.JoinedTable.table table]], or completely formatted SQL ''select''
	  * if `this.table` is a [[net.noresttherein.oldsql.schema.Table.TableExpression TableExpression]],
	  * possibly with an ''as'' clause (using either the
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFrom.alias alias]] listed by `F`,
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
	  * mapping type typically uniquely identify source tables in the schema. However, it is possible that another
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
	@throws[IllegalArgumentException]("if from is not an NonEmptyFrom, or its last relation does not equal this.")
	def spell[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	            (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		from match {
			case join :NonEmptyFrom if join.last == this =>
				spelling.table(table, join.aliasOpt)(context, params)
			case _ :NonEmptyFrom =>
				throw new IllegalArgumentException("Cannot spell non last table " + this + " of " + from + ".")
			case _ =>
				throw new IllegalArgumentException(
					"Cannot spell the last table " + this + " of " + from + " as it is not a NonEmptyFrom." +
					"This situation should be impossible."
				)
		}

	@throws[IllegalArgumentException]("if component.origin is not this table.")
	override def spell[P, C <: F, M[O] <: MappingAt[O]]
	                  (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }, inline :Boolean)
	                  (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else
			from.relations(position).defaultSpelling(component, inline)(from, context, params)

	@throws[IllegalArgumentException]("if component.origin is not this table.")
	override def spellExploded[P, C <: F, M[O] <: MappingAt[O]]
	                          (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                          (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else
			from.relations(position).explodedSpelling(component)(from, context, params)


	protected def defaultSpelling[P, C <: F, M[O] <: MappingAt[O]]
	                             (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] },
	                              inline :Boolean = false)
	                             (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		component.mapping match {
			case column :ColumnMapping[_, F] =>
				defaultSpelling(column)(from, context, params)
			case _ =>
				val columns = explodedSpelling(component)(from, context, params).filter(!_.isEmpty)
				if (columns.isEmpty)
					SpelledSQL(context)
				else if (columns.sizeIs <= 1)
					columns.head
				else if (inline)
					columns.reduce(_ +: ", " +: _)
				else
					("(" +: columns.reduce(_ +: ", " +: _)) + ")"
		}

	protected def defaultSpelling[P, C <: F](column :ColumnMapping[_, F])
		                                    (from :C, context :SQLContext[P], params :Parameterization[P, C])
		                                    (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.column(this, export.export(column))(from, context, params)

	protected def explodedSpelling[P, C <: F, M[O] <: MappingAt[O]]
	                              (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                              (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                              (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
	{
		val actual =
			if (component.origin.includes.isEmpty && component.origin.excludes.isEmpty) this
			else alter(component.origin.includes, component.origin.excludes)
		val columns = spelling.scope.defaultColumns(actual.export, component.mapping)
		columns.view.scanLeft(SpelledSQL(context)) {
			(sql, col) => defaultSpelling(col)(from, sql.context, params)
		}.tail.to(ArraySeq)
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedTable.*]

	private[sql] def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing
}




object JoinedTable {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :JoinedTable.Typed[F, X] @unchecked =>
				Got(from.relation -> from.offset)
			case _ => Lack
		}


	type * = JoinedTable[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[M[O] <: MappingAt[O]] = JoinedTable[RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedTable[F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[F <: RowProduct, V] = JoinedTable[F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedTable[O, M] }
}






trait JoinedParam[F <: RowProduct, T[O] <: MappingAt[O]]
	extends JoinedRelation[F, T] with JoinedRelationTemplate[F, T, JoinedParam[F, T]]
{
//	override def relation :ParamRelation[X] = toParamSQL.param //overriden in ParamSQL, as this will cause a StackOverflowException
//	def param :ParamRelation[X] = relation

	override def position :TableOffset[F, T] { type Rel = JoinedParam[F, T] } =
		new TableOffset(offset).asInstanceOf[TableOffset[F, T] { type Rel = JoinedParam[F, T] }]


	def toParamSQL :ParamSQL[F, T[Unit]#Subject, F]


	/** A new `JoinedParam` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedParam[E, T]

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedParam[E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedParam`. The `expand` method cannot be overriden here to return a `JoinedParam`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedParam[G, T]

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedParam.*]

	private[sql] def concrete_JoinedParam_subclass_must_extend_ParamSQL :Nothing
}




object JoinedParam {

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(ParamRelation[X], Int)] =
		e match {
			case from :ParamSQL[_, X, _] => Got(from.relation -> from.offset)
			case _ => Lack
		}

	type * = JoinedParam[_ <: RowProduct, _]

//	type Last[M[O] <: MappingAt[O]] = JoinedParam[RowProduct AndFrom M, M]
	type Last[X] = JoinedParam[RowProduct AndFrom ParamRelation[X]#Param, ParamRelation[X]#Param]

	type AnyIn[F <: RowProduct] = JoinedParam[F, _]

//	type Typed[F <: RowProduct, V] = JoinedParam[F, ] forSome { type T[O] <: RefinedMapping[V, O] }

//	type Of[M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedParam[O, M] }
	type Of[X] = { type T[O <: RowProduct] = JoinedParam[O, ParamRelation[X]#Param] }
}






/** An [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[D, GlobalScope, T[Unit]#Subject]` used
  * in the context of a ''group by'' clause
  * `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroupingOfGeneralized GroupingOfGeneralized]]`[D]`,
  * adapted to [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] interface shared with tables
  * and all other mappings exposed as [[net.noresttherein.oldsql.sql.Adjoin.right right]] sides of various
  * generalized 'join' [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] implementations.
  *
  * This trait is used by [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]]
  * clauses (but not [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) and works in the same way as
  * other relation expressions: it can be used within larger SQL expressions (in particular, the ''select'' clause),
  * as well as serve as the origin for subexpressions of the adapted grouping expression. These are represented
  * by standard [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]
  * @tparam D The type of the actual ''from'' clause grouped by `F`, used as the domain of the grouping expression.
  * @tparam F The domain type of this relation expression (not the grouping expression inside the relation).
  *           It is always a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] subtype, an upper bound
  *           not enforced explicitly in the type to increase its usability in generic contexts, but at construction -
  *           either statically instantiated to a `GroupBy`/`By`, or copied over from another `JoinedGrouping`
  *           instance (or some type which similarly is always created only for `GroupByClause` subtypes).
  * @tparam T The mapping type constructor accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *           adapting and exposing the carried expression. In most cases, it is
  *           some [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] subtype, but component expressions
  *           [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[D, T]` used as grouping expressions
  *           have it declared simply as their mapping type `T`. Additionally, it is possible to expose
  *           an [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping for a statement
  *           parameter declared in the grouped ''from'' clause `D` by using it as any other expression to expand
  *           a ''group by'' clause. Such a parameter mapping translates to empty SQL just as
  *           [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] clause, but is not considered an unbound parameter
  *           itself (it doesn't feature again in the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type
  *           of `F`) - instead it simply allows the use of the parameter represented by the normally unavailable
  *           (outside of aggregate functions) [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]],
  *           including the creation of derived parameter (components of `UnboundParam`).
  */
trait JoinedGrouping[-D <: FromSome, F <: RowProduct, T[A] <: MappingAt[A]]
	extends JoinedRelation[F, T] with JoinedRelationTemplate[F, T, JoinedGrouping[D, F, T]]
{
	override val relation :GroupingRelation[D, T]
	def grouping :GroupingRelation[D, T] = relation

	override def position :TableOffset[F, T] { type Rel = JoinedGrouping[D, F, T] } =
		new TableOffset(offset).asInstanceOf[TableOffset[F, T] { type Rel = JoinedGrouping[D, F, T] }]

	def toGroupingSQL :GroupingSQL[F, M, T[F]#Subject, F]
			forSome { type M[A] <: BaseMapping[T[F]#Subject, A] with T[A] }


	/** A new `JoinedGrouping` identical to this one, but in ''from'' clause `E` at offset `offset`. */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :JoinedGrouping[D, E, T]

	/** Converts this relation to an expression based on the clause `E[F]`, which expands `F` by a single relation. */
	def asIn[E[+L <: F] <: L Expanded T forSome { type T[O] <: MappingAt[O] }] :JoinedGrouping[D, E[F], T]

	/** This method is equivalent to `this.expand()`, but doesn't require the `G` clause as the parameter
	  * and returns a `JoinedGrouping`. The `expand` method cannot be overriden here to return a `JoinedGrouping`
	  * as the compiler doesn't know if `T[O]#Subject =:= T[G]#Subject`.
	  */
	def asIn[G <: RowProduct](implicit expansion :F PrefixOf G) :JoinedGrouping[D, G, T]


	override def spell[P](from :F, context :SQLContext[P], params :Parameterization[P, F])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val ctx = from.groupingSpellingContextForwarder(
			offset, context, params.asInstanceOf[Parameterization[P, from.Self]]
		)
		spelling.grouping(ctx.grouping)(ctx.from, ctx.context, ctx.params)
	}

	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	override def spell[P, C <: F, M[O] <: MappingAt[O]]
	                  (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] }, inline :Boolean)
	                  (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                  (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else {
			val ctx = from.groupingSpellingContextForwarder(
				offset, context, params.asInstanceOf[Parameterization[P, from.Self]]
			)
			ctx.grouping.spell(spelling)(component)(ctx.from, ctx.context, ctx.params)
		}

	@throws[IllegalArgumentException]("if component is not a component of this relation's mapping.")
	override def spellExploded[P, C <: F, M[O] <: MappingAt[O]]
	                          (component :ComponentSQL[C, M] { type Origin = F; type Entity[O] = T[O] })
	                          (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else {
			val ctx = from.groupingSpellingContextForwarder(
				offset, context, params.asInstanceOf[Parameterization[P, from.Self]]
			)
			ctx.grouping.spellExploded(spelling)(component)(ctx.from, ctx.context, ctx.params)
		}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[JoinedGrouping.*]

	private[sql] def concrete_JoinedGrouping_subclass_must_extend_GroupingSQL :Nothing
}




object JoinedGrouping {

//	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(GroupingRelation[MappingAt], Int)] =
//		e match {
//			case from :JoinedGrouping.Typed[F, X] @unchecked =>
//				Got(from.relation -> from.offset)
//			case _ => Lack
//		}

	type * = JoinedGrouping[_ <: FromSome, _ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	type Last[D <: FromSome, M[O] <: MappingAt[O]] = JoinedGrouping[D, RowProduct AndFrom M, M]

	type AnyIn[F <: RowProduct] = JoinedGrouping[Nothing, F, T] forSome { type T[O] <: MappingAt[O] }

	type Typed[D <: FromSome, F <: RowProduct, V] = JoinedGrouping[D, F, T] forSome { type T[O] <: RefinedMapping[V, O] }

	type Of[D <: FromSome, M[O] <: MappingAt[O]] = { type T[O <: RowProduct] = JoinedGrouping[D, O, M] }
}






//consider: why do we have includes and excludes here? Some problem with relation equality?
//The relation actually used is the one which takes precedence over this.relation, including includes/excludes.
// So we have them also on RelationSQL itself, where they are considered 'component scope' rather than 'query scope'.
class RelationSQL[-F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct] protected
                 (override val relation :Relation[T], override val offset :Int,
                  override val includes :Unique[RefinedMapping[_, O]],
                  override val excludes :Unique[RefinedMapping[_, O]])
	extends TypedComponentSQL[F, T, S, T, S, O] with JoinedRelation[O, T]
	   with RelationSQLTemplate[F, T, S, O,
	                            ({ type R[-E <: RowProduct, U >: E <: RowProduct] = RelationSQL[E, T, S, U] })#R]
{
	override val mapping :T[O] = relation[O]

	/** The export mapping used for the whole relation; note that it might not be `relation.export` due
	  * to includes and excludes on the particular instance of a component.
	  */
	override val export :RefinedMapping[S, O] = {
		if (includes.isEmpty && excludes.isEmpty) relation.export[O]
		else relation.export[O].apply(includes, excludes)
	}.asInstanceOf[RefinedMapping[S, O]]

	override val extract :MappingExtract[S, S, O] = MappingExtract.ident(export)

	override def projection :IsomorphicProjection[T, S, O] = OriginProjection.isomorphism

	def asGrouping[M[A] <: BaseMapping[V, A], V]
	              (component :TypedComponentSQL[F, T, S, M, V, O]) :GroupingRelation[F, M]
//		GroupingRelation[F, M, V, O](compo)(projection)


	//	override def groupingRelation :Relation[T] = relation
//
//	override def group[C <: FromSome { type Generalized <: O }] :GroupingSQL[C GroupBy T, T, S, C GroupBy T] = ???
//
//	override def anotherGroup[C <: GroupByClause { type GeneralizedDiscrete <: O }] :GroupingSQL[C By T, T, S, C By T] = ???

//	def group[U <: F, M[A] <: BaseMapping[V, A], V](component :TypedComponentSQL[U, T, S, M, V, O])
//			:GroupingSQL[FromSome GroupBy M, M, V, FromSome GroupBy M] = ???
////		GroupingSQL.groupBy(component)
//
//	def anotherGroup[U <: F, M[A] <: BaseMapping[V, A], V](component :TypedComponentSQL[U, T, S, M, V, O])
//			:GroupingSQL[GroupByClause By M, M, V, GroupByClause By M] = ???
	def group[C <: F with FromSome, M[A] <: BaseMapping[V, A], V]
             (from :C, component :TypedComponentSQL[C, T, S, M, V, O]) :GroupingRelation[M] { type From = C } =
		GroupingRelation(from, component)

//	def subgroup[C <: F with FromSome, M[A] <: BaseMapping[V, A], V]
//	            (from :C, component :TypedComponentSQL[C, T, S, M, V, O]) :GroupingRelation[M] { type From = C } =
//		GroupingRelation(from, component)

	@inline final override def toRelationSQL :RelationSQL[O, T, S, O] = this.asInstanceOf[RelationSQL[O, T, S, O]]

	override def asRelationSQL :Some[RelationSQL[O, T, S, O]] = Some(toRelationSQL)

	override def upcast :ComponentSQL[O, T] = this

	protected override def recreate[E <: RowProduct](offset :Int, includes :Unique[RefinedMapping[_, E]],
	                                                 excludes :Unique[RefinedMapping[_, E]]) :RelationSQL[E, T, S, E] =
		new RelationSQL[E, T, S, E](relation, offset, includes, excludes)

	//these need to be overriden due to JoinedRelation's having wider bounds than the inherited implementations
	override def \[K <: MappingAt[O], X](component :K)(implicit project :OriginProjection[K, X])
			:TypedComponentSQL[O, T, S, project.WithOrigin, X, O] =
		TypedComponentSQL(toRelationSQL, component)

	override def \[K <: MappingAt[O]]
	              (component :T[O] => K)(implicit factory :ComponentSQL.Factory[K]) :factory.TypedResult[O, T, S, O] =
		factory(toRelationSQL, component(mapping))

	override def \[K <: ColumnMapping[_, O], X]
	              (column :K)(implicit project :OriginProjection[K, X] { type WithOrigin[A] <: ColumnMapping[X, A] })
			:TypedColumnComponentSQL[O, T, S, project.WithOrigin, X, O] =
		TypedColumnComponentSQL(toRelationSQL, column)

	/** Simply returns the given relation. */
	override def graft[P <: RowProduct](relation :JoinedRelation[P, T]) :RelationSQL[P, T, S, P] =
		relation.toRelationSQL.asInstanceOf[RelationSQL[P, T, S, P]]


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, S] =
		visitor.relation(toRelationSQL)


	//the following methods are overriden due to a winder bound O inherited from invariant JoinedTable

	override def topSelectFrom[E <: O with GroundFrom](from :E) :TopSelectMapping[E, T, S] =
		SelectSQL(from, toRelationSQL)

	override def subselectFrom[B <: NonEmptyFrom](from :ExactSubselectOf[F, B]) :SubselectMapping[B, from.type, T, S] =
		SelectSQL.subselect(from, toRelationSQL)

	override def paramSelectFrom[P <: Chain, G <: O](from :TopFrom { type Generalized <: G; type Params = P })
			:SelectMapping[P, T] =
		Select(from)[T, S](toRelationSQL)


	override def :=[P <: RowProduct, Y, U](rvalue :SQLExpression[P, GlobalScope, Y])
	                                      (implicit promote :SQLTypeUnification[S, Y, U])
			:ComponentSetter[O, P, U] =
		ComponentSetter[O, T, S, P, Y, U](this, rvalue)(promote)

	override def :=[C <: MappingOf[S], E <: RowProduct, P <: RowProduct]
	               (component :C)(implicit cast :C <:< RefinedMapping[S, P],
	                              subtype :SQLExpression[P, GlobalScope, S] <:< SQLExpression[E, GlobalScope, S],
	                              project :OriginProjection[C, S], offset :TableCount[P, _ <: Numeral]) :ComponentSetter[O, E, S] =
		this := subtype(LooseComponent(component))

	override def :=[X, U](that :X)(implicit promote :SQLTypeUnification[S, X, U], form :SQLForm[X])
			:ComponentSetter[O, RowProduct, U] =
		this := SQLTerm(that)

	override def :=?[Y, U](rvalue :Y)(implicit promote :SQLTypeUnification[S, Y, U], form :SQLForm[Y])
			:ComponentSetter[O, RowProduct, U] =
		this := BoundParam(rvalue)


	//this can fail - will always fail if `this.relation` is not a `Table`, but subclasses override it
	// and in some circumstances we may need to create a RelationSQL for any relation, without exposing it
	// to the application or using this method.
//	override def spell[P, C <: O](column :ColumnMapping[_, O])
//	                             (from :C, context :SQLContext[P], params :Parameterization[P, C])
//	                             (implicit spelling :SQLSpelling) :SpelledSQL[P] =
////		relation.spell(this, column)(context, params)
//		spelling.column(this, export.export(column))(from.self, context, params)

	override def spell[P](from :O, context :SQLContext[P], params :Parameterization[P, O])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else
			from.relations(position).spell(component, inline)(from, context, params)


//	override protected def defaultSpelling[P, C <: O](column :ColumnMapping[_, O])(from :C, context :SQLContext[P], params :Parameterization[P, C])(implicit spelling :SQLSpelling) :SpelledSQL[P] = ???

	override def spellExploded[P, C <: O, M[A] <: MappingAt[A]]
	                          (component :ComponentSQL[C, M] { type Origin = O; type Entity[O] = T[O] })
	                          (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                          (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		if (component.origin != this)
			throw new IllegalArgumentException(
				"Cannot spell component " + component + " of a different relation than " + this + "."
			)
		else
			from.relations(position).spellExploded(component)(from, context, params)


	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case rel :RelationSQL.* @unchecked if rel canEqual this =>
			relation == rel.relation && includes.toSet == rel.includes.toSet && excludes.toSet == rel.excludes.toSet
		case _ => false
	}
	override def hashCode :Int = relation.hashCode

	private[sql] override def concrete_JoinedRelation_subclass_must_extend_RelationSQL :Nothing =
		throw new UnsupportedOperationException
}




object RelationSQL {

//		def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S]
//		         (table :Relation[T])(implicit offset :TableOffset[F, T, _ <: Numeral]) :RelationSQL[F, T, S, F] =
//			new RelationSQL[F, T, S, F](table, table[F], offset.tables)

	def apply[F <: RowProduct] :RelationSQLFactory[F] = new RelationSQLFactory[F] {}

	sealed trait RelationSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		          (relation :Relation[M])
		          (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]],
		                    shift :TableOffset[F, T]) :RelationSQL[F, T, S, F] =
			RelationSQL[F, T, S, F](cast(relation), shift.tables, Unique.empty, Unique.empty)
	}

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (relation :Relation[T], index :Int) :RelationSQL[F, T, S, O] =
		RelationSQL[F, T, S, O](relation, index, Unique.empty, Unique.empty)

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (relation :Relation[T], index :Int,
	                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
			:RelationSQL[F, T, S, O] =
		relation match {
			case t :Table[T] => TableSQL[F, T, S, O](t, index, includes, excludes)
			case p :ParamRelation[x] => ParamSQL[F, x, O](p, index).asInstanceOf[RelationSQL[F, T, S, O]]
			case g :GroupingRelation[T] => GroupingSQL[F, T, S, O](g, index, includes, excludes)
			case _ => new RelationSQL[F, T, S, O](relation, index, includes, excludes)
		}


	def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	        (table :Relation[M])
	        (implicit cast :InferTypeParams[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:LastRelation[T, S] =
		new RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			table, 0, Unique.empty, Unique.empty
		)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Relation[MappingAt], Int)] =
		e match {
			case from :RelationSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[Relation[MappingOf[X]#TypedProjection]] -> from.offset)
			case _ => Lack
		}


	type * = RelationSQL[F, T, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = RelationSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = RelationSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
	}

	type LastRelation[T[A] <: BaseMapping[S, A], S] = RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

	def LastRelation[T[A] <: BaseMapping[S, A], S](from :Relation[T]) :LastRelation[T, S] =
		RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			from, 0, Unique.empty, Unique.empty
		)



	trait RelationSQLTemplate[-F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct,
	                          +R[-E <: RowProduct, U >: E <: RowProduct] <:
		                           RelationSQL[E, T, S, U] with RelationSQLTemplate[E, T, S, U, R]]
		extends JoinedRelationTemplate[O, T, R[F, O]]// with TypedComponentSQL[F, T, S, T, S, O]
	{ this :R[F, O] =>
		protected def includes :Unique[RefinedMapping[_, O]]
		protected def excludes :Unique[RefinedMapping[_, O]]
		protected def offset :Int

		override def isAnchored(clause :O) :Boolean =
			clause.fullTableStack(offset).relation == relation

		override def anchor(clause :O) :R[F, O] = { //this is risky; would be better to return just RelationSQL always
			val actual = clause.fullTableStack(offset).asInstanceOf[R[F, O]]
			if (actual.relation == relation) this
			else if (includes.isEmpty && excludes.isEmpty) actual
			else actual.alter(includes, excludes) //fixme: unlikely that actual.export will recognize includes/excludes
		}

		private def result[E <: RowProduct](offset :Int, includes :Unique[RefinedMapping[_, E]],
		                                    excludes :Unique[RefinedMapping[_, E]]) :R[E, E] =
			(this :RelationSQLTemplate[F, T, S, O, R]).recreate(offset, includes, excludes)

		private def result[E <: RowProduct](offset :Int) :R[E, E] =
			(this :RelationSQLTemplate[F, T, S, O, R]).recreate(offset)

		protected def recreate[E <: RowProduct](offset :Int, includes :Unique[RefinedMapping[_, E]],
		                                        excludes :Unique[RefinedMapping[_, E]]) :R[E, E]

		protected def recreate[E <: RowProduct](offset :Int) :R[E, E] =
			result(offset,
				includes.asInstanceOf[Unique[RefinedMapping[_, E]]], excludes.asInstanceOf[Unique[RefinedMapping[_, E]]]
			)

		override def alter(includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]]) :R[F, O] =
			result(offset, includes, excludes)

		override def default :R[O, O] =
			if (includes.isEmpty && excludes.isEmpty) this.asInstanceOf[R[O, O]]
			else result[O](offset, Unique.empty, Unique.empty)

		/** A new `RelationSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
		  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
		  */ //fixme: this doesn't work for GroupingRelation!!!
		override def moveTo[E <: RowProduct](offset :TableOffset[E, T]) :R[E, E] = result(offset.tables)

		override def basedOn[U <: O, E <: RowProduct]
		                    (base :E)(implicit expansion :U PartOf E) :R[E, _ >: E <: RowProduct] =
			result(offset + expansion.lengthDiff)//E is incorrect, but we lose this information anyway

		override def expand[U <: O, E <: RowProduct]
		                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
				:R[E, _ >: E <: RowProduct] =
			result(offset + ev.length) //E is incorrect, but we lose this information anyway

		override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E) :R[E, _ >: E <: RowProduct] =
			result(offset + expansion.length)

		override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :R[J[F], J[O]] =
			result(offset + 1)

		override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :R[E, E] =
			result(offset + expansion.lengthDiff)
	}



	trait RelationVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]]
		extends TableVisitor[F, Y] with ParamVisitor[F, Y] with GroupingVisitor[F, Y]
	{
		def relation[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		            (e :RelationSQL[F, T, R, O]) :Y[GlobalScope, R]
	}

	type MatchRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = RelationVisitor[F, Y]

	trait CaseRelation[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] extends RelationVisitor[F, Y] {
		override def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		                  (e :TableSQL[F, T, R, O]) :Y[GlobalScope, R] =
			relation(e)

		override def grouping[D <: FromSome, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		                     (e :GroupingSQL[D, F, T, R, O]) :Y[GlobalScope, R] =
			relation(e)

		override def param[X, O >: F <: RowProduct](e :ParamSQL[F, X, O]) :Y[GlobalScope, X] = relation(e)
	}
}






class TableSQL[-F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct] protected
              (override val relation :Table[T], override val offset :Int,
               override val includes :Unique[RefinedMapping[_, O]],
               override val excludes :Unique[RefinedMapping[_, O]])
	extends RelationSQL[F, T, S, O](relation, offset, includes, excludes)
	   with JoinedTable[O, T]
	   with RelationSQLTemplate[F, T, S, O, ({ type R[-E <: RowProduct, U >: E <: RowProduct] = TableSQL[E, T, S, U] })#R]
{
	@inline final override def toTableSQL :TableSQL[O, T, S, O] = this.asInstanceOf[TableSQL[O, T, S, O]]

	override def asTableSQL :Some[TableSQL[O, T, S, O]] = Some(toTableSQL)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, T, S, M, V, O]) :GroupingRelation[F, M] =
		GroupingRelation[F, M, V](component)(component.projection.isomorphism[F])


	protected override def recreate[E <: RowProduct](offset :Int, includes :Unique[RefinedMapping[_, E]],
	                                                 excludes :Unique[RefinedMapping[_, E]]) :TableSQL[E, T, S, E] =
		new TableSQL[E, T, S, E](relation, offset, includes, excludes)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, S] =
		visitor.table(toTableSQL)


	private[sql] override def concrete_JoinedTable_subclass_must_extend_TableSQL :Nothing =
		throw new UnsupportedOperationException
}




object TableSQL {

	def apply[F <: RowProduct] :TableSQLFactory[F] = new TableSQLFactory[F] {}

	sealed trait TableSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		               (table :Table[M])
		               (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]],
		                         offset :TableOffset[F, T]) :TableSQL[F, T, S, F] =
			new TableSQL[F, T, S, F](cast(table), offset.tables, Unique.empty, Unique.empty)
	}

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Table[T], index :Int) :TableSQL[F, T, S, O] =
		new TableSQL[F, T, S, O](table, index, Unique.empty, Unique.empty)

	private[sql] def apply[F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (table :Table[T], index :Int,
	                       includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
			:TableSQL[F, T, S, O] =
		new TableSQL[F, T, S, O](table, index, includes, excludes)


	def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	        (table :Table[M])
	        (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]]) :LastTable[T, S] =
		new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			table, 0, Unique.empty, Unique.empty
		)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(Table[MappingAt], Int)] =
		e match {
			case from :TableSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[Table[MappingOf[X]#TypedProjection]] -> from.offset)
			case _ => Lack
		}



	type * = TableSQL[F, T, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = TableSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = TableSQL[F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
	}

	type LastTable[T[A] <: BaseMapping[S, A], S] = TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]

	def LastTable[T[A] <: BaseMapping[S, A], S](from :Table[T]) :LastTable[T, S] =
		new TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			from, 0, Unique.empty, Unique.empty
		)



	trait TableVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def table[T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct](e :TableSQL[F, T, R, O]) :Y[GlobalScope, R]
	}

	type MatchTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]

	type CaseTable[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = TableVisitor[F, Y]
}






class ParamSQL[-F <: RowProduct, X, O >: F <: RowProduct] protected
              (override val relation :ParamRelation[X], override val offset :Int)
	extends RelationSQL[F, ParamRelation[X]#Param, X, O](relation, offset, Unique.empty, Unique.empty)
	   with JoinedParam[O, ParamRelation[X]#Param]
	   with JoinedRelationTemplate[O, ParamRelation[X]#Param, ParamSQL[F, X, O]]
{
	private[this] val Form = mapping.formExtractor

	def param :ParamRelation[X] = relation

	@inline final override def toParamSQL :ParamSQL[O, X, O] = this.asInstanceOf[ParamSQL[O, X, O]]

	override def asParamSQL :Some[ParamSQL[O, X, O]] = Some(toParamSQL)


	override def asGrouping :GroupingRelation[O, ParamRelation[X]#Param] =
		GroupingRelation.param[O, ParamRelation[X]#Param, X](toParamSQL)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, ParamRelation[X]#Param, X, M, V, O]) :GroupingRelation[F, M] =
		GroupingRelation.param[F, M, V](component)

	override def alter(includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]]) :ParamSQL[F, X, O] =
		this

	override def alter(components :UnboundParam[X, O] => ComponentSelection[_, O]*) :ParamSQL[F, X, O] = this
	override def +-(components :Iterable[UnboundParam[X, O] => ComponentSelection[_, O]]) :ParamSQL[F, X, O] = this

	override def include(components :Iterable[RefinedMapping[_, O]]) :ParamSQL[O, X, O] = toParamSQL
	override def exclude(components :Iterable[RefinedMapping[_, O]]) :ParamSQL[O, X, O] = toParamSQL
	override def include(components :UnboundParam[X, O] => RefinedMapping[_, O]*) :ParamSQL[O, X, O] = toParamSQL
	override def exclude(components :UnboundParam[X, O] => RefinedMapping[_, O]*) :ParamSQL[O, X, O] = toParamSQL
	override def +(component :UnboundParam[X, O] => RefinedMapping[_, O]) :ParamSQL[O, X, O] = toParamSQL
	override def -(component :UnboundParam[X, O] => RefinedMapping[_, O]) :ParamSQL[O, X, O] = toParamSQL

	override def default :ParamSQL[O, X, O] = toParamSQL

	override def anchor(clause :O) :ParamSQL[F, X, O] = clause.fullTableStack(offset).asInstanceOf[ParamSQL[F, X, O]]

	/** A new `ParamSQL` instance representing the same relation, but in ''from'' clause `E` at `offset`.
	  * The class of the created instance and all its fields except for `offset` are the same as in this instance.
	  */
	override def moveTo[E <: RowProduct](offset :TableOffset[E, ParamRelation[X]#Param]) :ParamSQL[E, X, E] =
		new ParamSQL[E, X, E](relation, offset.tables)

	override def basedOn[U <: O, E <: RowProduct]
	                    (base :E)(implicit expansion :U PartOf E) :ParamSQL[E, X, _ >: E <: RowProduct] =
		new ParamSQL[E, X, E](relation, offset + expansion.lengthDiff) //E is incorrect, but we lose this information anyway

	override def expand[U <: O, E <: RowProduct]
	                   (base :E)(implicit ev :U ExpandedBy E, global :GlobalScope <:< GlobalScope)
			:ParamSQL[E, X, _ >: E <: RowProduct] =
		new ParamSQL[E, X, E](relation, offset + ev.length) //E is incorrect, but we lose this information anyway

	override def expand[E <: RowProduct](implicit expansion :O ExpandedBy E) :ParamSQL[E, X, _ >: E <: RowProduct] =
		new ParamSQL[E, X, E](relation, offset + expansion.length)

	override def asIn[J[+L <: O] <: L Expanded T forSome { type T[A] <: MappingAt[A] }] :ParamSQL[J[F], X, J[O]] =
		new ParamSQL[J[F], X, J[O]](relation, offset + 1)

	override def asIn[E <: RowProduct](implicit expansion :O PrefixOf E) :ParamSQL[E, X, E] =
		new ParamSQL(relation, offset + expansion.lengthDiff)


	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, X] =
		visitor.param(toParamSQL)


//	override def spell[P](from :O, context :SQLContext[P], params :Parameterization[P, O])
//	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
//		SpelledSQL(context.param(relation.name)) //this method will never be called normally, but better be prepared

	protected override def defaultSpelling[P, C <: O, M[A] <: MappingAt[A]]
	                       (component :ComponentSQL[C, M] { type Origin = O; type Entity[A] = UnboundParam[X, A] },
	                        inline :Boolean)
	                       (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                       (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		component.mapping match { //the usual problem of equality between ParamRelations and their mappings...
			case Form(paramForm) =>
				val getter = params[ParamRelation[X]#Param, X, O](this)
				val writeForm = paramForm.unmap(getter)
				SpelledSQL(writeForm.param(inline), context, writeForm)
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $component passed as a component of unbound param $this is not a ParamMapping derived from $relation"
				)
		}

	protected override def defaultSpelling[P, C <: O](column :ColumnMapping[_, O])
	                                                 (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                                                 (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		defaultSpelling(this \ column)(from, context, params) //never called normally because we override explodedSpelling

	protected override def explodedSpelling[P, C <: O, M[A] <: MappingAt[A]]
	                       (component :ComponentSQL[C, M] { type Origin = O; type Entity[A] = UnboundParam[X, A] })
	                       (from :C, context :SQLContext[P], params :Parameterization[P, C])
	                       (implicit spelling :SQLSpelling) :Seq[SpelledSQL[P]] =
		component match {
			case Form(paramForm) =>
				val getter = params[ParamRelation[X]#Param, X, O](this)
				val writeForm = paramForm.unmap(getter)
				relation.form.writtenColumns match {
					case 0 => Seq.empty
					case 1 => SpelledSQL("?", context, writeForm)::Nil
					case n => try {
						writeForm.split.map { SpelledSQL("?", context, _) }
					} catch { //fixme: this exception should be somehow propagated as it means we can't intertwine expressions with params
						case _ :UnsupportedOperationException =>
							ReversedList.fill(n - 1)(SpelledSQL("?", context, SQLWriteForm.empty)) :+
								SpelledSQL("?", context, writeForm)
					}
				}
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $component passed as a component of unbound param $this is not a ParamMapping derived from $param."
				)

		}

	private[sql] override def concrete_JoinedParam_subclass_must_extend_ParamSQL :Nothing =
		throw new UnsupportedOperationException
}




object ParamSQL {

	def apply[F <: RowProduct] :ParamSQLFactory[F] = new ParamSQLFactory[F] {}

	sealed trait ParamSQLFactory[F <: RowProduct] extends Any {
		final def apply[X](param :ParamRelation[X])
		                  (implicit offset :TableOffset[F, ParamRelation[X]#Param]) :ParamSQL[F, X, F] =
			new ParamSQL[F, X, F](param, offset.tables)
	}

	private[sql] def apply[F <: RowProduct, X, O >: F <: RowProduct]
	                      (param :ParamRelation[X], index :Int) :ParamSQL[F, X, O] =
		new ParamSQL[F, X, O](param, index)

	def last[X](param :ParamRelation[X]) :LastParam[X] =
		new ParamSQL[RowProduct AndFrom ParamRelation[X]#Param, X, RowProduct AndFrom ParamRelation[X]#Param](
			param, 0
		)

	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(ParamRelation[X], Int)] =
		e match {
			case from :ParamSQL[_, X, _] => Got(from.relation -> from.offset)
			case _ => Lack
		}


	type * = ParamSQL[F, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct; type X
	}

	type AnyIn[-F <: RowProduct] = ParamSQL[F, X, O] forSome {
		type X; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, X] = ParamSQL[F, X, _ >: F <: RowProduct]

	type LastParam[X] = ParamSQL[RowProduct AndFrom ParamRelation[X]#Param, X, RowProduct AndFrom ParamRelation[X]#Param]

	def LastParam[X](from :ParamRelation[X]) :LastParam[X] =
		new ParamSQL[RowProduct AndFrom ParamRelation[X]#Param, X, RowProduct AndFrom ParamRelation[X]#Param](
			from, 0
		)


	trait ParamVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def param[X, O >: F <: RowProduct](e :ParamSQL[F, X, O]) :Y[GlobalScope, X]
	}

	type MatchParam[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParamVisitor[F, Y]

	type CaseParam[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = ParamVisitor[F, Y]
}


/**
  *
  * @tparam D The type of the actual ''from'' clause grouped by `F`, used as the domain of the grouping expression.
  * @tparam F The domain type of this relation expression (not the grouping expression inside the relation).
  *           It is always a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] subtype, an upper bound
  *           not enforced explicitly in the type to increase its usability in generic contexts, but at construction -
  *           either statically instantiated to a `GroupBy`/`By`, or copied over from another `JoinedGrouping`
  *           instance (or some type which similarly is always created only for `GroupByClause` subtypes).
  * @tparam T The mapping type constructor accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *           adapting and exposing the carried expression. In most cases, it is
  *           some [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]] subtype, but component expressions
  *           [[net.noresttherein.oldsql.sql.ast.ComponentSQL ComponentSQL]]`[D, T]` used as grouping expressions
  *           have it declared simply as their mapping type `T`. Additionally, it is possible to expose
  *           an [[net.noresttherein.oldsql.sql.ParamClause.UnboundParam UnboundParam]] mapping for a statement
  *           parameter declared in the grouped ''from'' clause `D` by using it as any other expression to expand
  *           a ''group by'' clause. Such a parameter mapping translates to empty SQL just as
  *           [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] clause, but is not considered an unbound parameter
  *           itself (it doesn't feature again in the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] type
  *           of `F`) - instead it simply allows the use of the parameter represented by the normally unavailable
  *           (outside of aggregate functions) [[net.noresttherein.oldsql.sql.ParamClause.ParamRelation ParamRelation]],
  *           including the creation of derived parameter (components of `UnboundParam`).
  * @tparam S
  * @tparam O The [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this instance and the mapping `T`
  *           exposed by properties [[net.noresttherein.oldsql.sql.ast.ComponentSQL.mapping mapping]]
  *           and [[net.noresttherein.oldsql.sql.ast.ComponentSQL.export export]].
  * @param relation
  * @param offset
  * @param includes
  * @param excludes
  */
class GroupingSQL[-D <: FromSome, -F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct] protected
                 (override val relation :GroupingRelation[D, T], override val offset :Int = 0,
                  override val includes :Unique[RefinedMapping[_, O]] = Unique.empty[RefinedMapping[_, O]],
                  override val excludes :Unique[RefinedMapping[_, O]] = Unique.empty[RefinedMapping[_, O]])
	extends RelationSQL[F, T, S, O](relation, offset, includes, excludes)
	   with JoinedGrouping[D, O, T]
	   with RelationSQLTemplate[F, T, S, O,
	                            ({ type R[-E <: RowProduct, U >: E <: RowProduct] = GroupingSQL[D, E, T, S, U] })#R]
{
	@inline final override def toGroupingSQL :GroupingSQL[D, O, T, S, O] = this.asInstanceOf[GroupingSQL[D, O, T, S, O]]

	override def asGroupingSQL :Some[GroupingSQL[D, O, T, S, O]] = Some(toGroupingSQL)

	override def asGrouping :GroupingRelation[O, T] =
		throw new UnsupportedOperationException(
			"A grouping relation " + this + " cannot be used as a grouping expression."
		)

	override def asGrouping[M[A] <: BaseMapping[V, A], V]
	                       (component :TypedComponentSQL[F, T, S, M, V, O]) :GroupingRelation[F, M] =
		throw new UnsupportedOperationException(
			"Cannot use a grouping relation component " + component + " as a new grouping expression."
		)


	protected override def recreate[E <: RowProduct](offset :Int, includes :Unique[RefinedMapping[_, E]],
	                                                 excludes :Unique[RefinedMapping[_, E]]) :GroupingSQL[D, E, T, S, E] =
		new GroupingSQL(relation, offset, includes, excludes)
//		GroupingSQL[E, T, S, E](relation, offset, includes, excludes)

	protected override def applyTo[Y[-_ >: LocalScope <: GlobalScope, _]]
	                              (visitor :ExpressionVisitor[O, Y]) :Y[GlobalScope, S] =
		visitor.grouping(toGroupingSQL)


	private[sql] override def concrete_JoinedGrouping_subclass_must_extend_GroupingSQL :Nothing =
		throw new UnsupportedOperationException
}




object GroupingSQL {

/*
	def apply[F <: RowProduct] :GroupingSQLFactory[F] = new GroupingSQLFactory[F] {}

	sealed trait GroupingSQLFactory[F <: RowProduct] extends Any {
		final def apply[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		               (grouping :GroupingRelation[M])
		               (implicit cast :InferTypeParams[GroupingRelation[M], GroupingRelation[T],
		                                               GroupingRelation[MappingOf[S]#TypedProjection]],
		                         offset :TableOffset[F, T]) :GroupingSQL[F, T, S, F] =
			new GroupingSQL[F, T, S, F](cast(grouping), offset.tables, Unique.empty, Unique.empty)
	}


	def last[M[A] <: MappingAt[A], T[A] <: BaseMapping[S, A], S]
	        (grouping :GroupingRelation[M])
	        (implicit cast :InferTypeParams[GroupingRelation[M], GroupingRelation[T],
	                                        GroupingRelation[MappingOf[S]#TypedProjection]]) :LastGrouping[T, S] =
		new GroupingSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T](
			grouping, 0, Unique.empty, Unique.empty
		)


	def unapply[F <: RowProduct, X](e :SQLExpression[F, _, X]) :Opt[(GroupingRelation[MappingAt], Int)] =
		e match {
			case from :GroupingSQL.Typed[F, X] @unchecked =>
				Got(from.relation.asInstanceOf[GroupingRelation[MappingOf[X]#TypedProjection]] -> from.offset)
			case _ => Lack
		}
*/
	private[sql] def apply[D <: FromSome, F <: RowProduct, T[A] <: BaseMapping[S, A], S, O >: F <: RowProduct]
	                      (grouping :GroupingRelation[D, T], index :Int,
	                       includes :Unique[RefinedMapping[_, O]] = Unique.empty[RefinedMapping[_, O]],
	                       excludes :Unique[RefinedMapping[_, O]] = Unique.empty[RefinedMapping[_, O]])
			:GroupingSQL[D, F, T, S, O] =
		new GroupingSQL[D, F, T, S, O](grouping, index, includes, excludes)


	def groupBy[F <: FromSome, T[A] <: BaseMapping[S, A], S]
	           (relation :GroupingRelation[F, T]) :GroupingSQL[F, F GroupBy T, T, S, FromSome GroupBy T] =
		new GroupingSQL[F, F GroupBy T, T, S, FromSome GroupBy T](relation, 0)

	def groupBy[F <: FromSome { type Generalized <: O }, T[A] <: BaseMapping[S, A], S, O <: RowProduct]
	           (component :ComponentSQL[F, T] { type Origin = O }) :GroupingSQL[F, F GroupBy T, T, S, FromSome GroupBy T] =
		GroupingSQL[F, F GroupBy T, T, S, FromSome GroupBy T](component.asGrouping, 0)

	def groupBy[F <: FromSome, X](e :SQLExpression[F, GlobalScope, X])
			:GroupingSQL[F, F GroupByVal X, Group[X]#E, X, FromSome GroupByVal X] =
		GroupingSQL[F, F GroupBy Group[X]#E, Group[X]#E, X, FromSome GroupByVal X](GroupingRelation(e), 0)

	def groupBy[F <: FromSome, X](e :ColumnSQL[F, GlobalScope, X])
			:GroupingSQL[F, F GroupByOne X, Group[X]#C, X, FromSome GroupByOne X] =
		GroupingSQL[F, F GroupBy Group[X]#C, Group[X]#C, X, FromSome GroupByOne X](GroupingRelation(e), 0)


	def by[F <: FromSome, G <: GroupByClause, T[O] <: BaseMapping[S, O], S]
	      (relation :GroupingRelation[F, T]) :GroupingSQL[F, G By T, T, S, GroupByClause AndBy T] =
		GroupingSQL[F, G By T, T, S, GroupByClause AndBy T](relation, 0)

	def by[F <: FromSome, G <: GroupingOfGeneralized[F], T[O] <: BaseMapping[S, O], S]
	      (component :ComponentSQL[F, T]) :GroupingSQL[F, G By T, T, S, GroupByClause AndBy T] =
		GroupingSQL[F, G By T, T, S, GroupByClause AndBy T](component.asGrouping, 0)

	def by[F <: FromSome, G <: GroupingOfGeneralized[F], X]
	      (e :SQLExpression[F, GlobalScope, X]) :GroupingSQL[F, G ByVal X, Group[X]#E, X, GroupByClause AndByVal X] =
		GroupingSQL[F, G By Group[X]#E, Group[X]#E, X, GroupByClause AndByVal X](GroupingRelation(e), 0)

	def by[F <: FromSome, G <: GroupingOfGeneralized[F], X]
	      (e :ColumnSQL[F, GlobalScope, X]) :GroupingSQL[F, G ByOne X, Group[X]#C, X, GroupByClause AndByOne X] =
		GroupingSQL[F, G By Group[X]#C, Group[X]#C, X, GroupByClause AndByOne X](GroupingRelation(e), 0)



	type * = GroupingSQL[F, T, X, O] forSome {
		type F <: RowProduct; type O >: F <: RowProduct
		type T[A] <: BaseMapping[X, A]; type X
	}

	type AnyIn[-F <: RowProduct] = GroupingSQL[Nothing, F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, A]; type R; type O >: F <: RowProduct
	}

	type Typed[-F <: RowProduct, R] = GroupingSQL[Nothing, F, T, R, O] forSome {
		type T[A] <: BaseMapping[R, O]; type O >: F <: RowProduct
	}

	type LastGrouping[F <: FromSome, T[A] <: BaseMapping[S, A], S] =
		GroupingSQL[F, GroupByClause AndBy T, T, S, GroupByClause AndBy T]

//	def LastGrouping[T[A] <: BaseMapping[S, A], S](from :GroupingRelation[T]) :LastGrouping[T, S] =
//		new GroupingSQL[GroupByClause AndBy T, T, S, GroupByClause AndBy T](
//			from, 0, Unique.empty, Unique.empty
//		)



	trait GroupingVisitor[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] {
		def grouping[D <: FromSome, T[A] <: BaseMapping[R, A], R, O >: F <: RowProduct]
		            (e :GroupingSQL[D, F, T, R, O]) :Y[GlobalScope, R]
	}

	type MatchGrouping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = GroupingVisitor[F, Y]

	type CaseGrouping[+F <: RowProduct, +Y[-_ >: LocalScope <: GlobalScope, _]] = GroupingVisitor[F, Y]
}

