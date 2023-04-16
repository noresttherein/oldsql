package net.noresttherein.oldsql.sql

import scala.annotation.{implicitNotFound, showAsInfix}

import net.noresttherein.oldsql.collection.{Chain, Opt}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.{ChunkedString, InferTypeParams}
import net.noresttherein.oldsql.schema.{Mapping, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.DecoratedRow.ExpandingDecorator
import net.noresttherein.oldsql.sql.Expanded.{ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromSome.GroundFromSome
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroundRow, NonEmptyRow, ParamlessRow, PartOf, PrefixOf, RowProductTemplate}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.{Grouped, Single}
import net.noresttherein.oldsql.sql.ast.{ChainTuple, ColumnMappingSQL, ComponentSQL, GenericColumnComponentSQL, JoinedParam, JoinedRelation, RelationSQL, SelectAs, SelectColumn, SelectColumnAs, SelectSQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{CanSelect, GroupedUnder, LastTableOf, OuterClauseOf, RelationCount, RelationOffset, RowProductVisitor, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.GetTable.{ByAlias, ByIndex, ByLabel, ByParamIndex, ByParamName, ByParamType, BySubject, ByType}
import net.noresttherein.oldsql.sql.mechanics.LastTableOf.LastBound
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}

//implicits
import net.noresttherein.oldsql.slang._





/*  An alternative design could be:
 *  trait ClauseType[+T <: ClauseType[T]] {
 *      type Self <: T { type Self = ClauseType.this.Self }
 *  }
 *  type Clause = ClauseType[_]
 *
 */
/** A filtered cartesian join of a number of [[net.noresttherein.oldsql.schema.Relation relations]] represented
  * by [[net.noresttherein.oldsql.schema.Mapping mappings]] encoded in this concrete type. In its most basic use,
  * a `RowProduct` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in a query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] class hierarchy)
  * are defined, providing all non-constant values available to them. It consists of a list of
  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation relations]], with associated
  * [[net.noresttherein.oldsql.schema.Mapping Mapping]]s, together with an optional filter working on those relations,
  * especially any required join conditions. While the individual elements of a clause are referred to often as tables
  * for simplicity, they can not only be arbitrary relations such as other ''selects'', but also synthetic artifacts
  * such as query parameters. It is even theoretically possible to use arbitrary mapping components, to be replaced
  * at a later time with references to concrete tables, parameters or constants. As such, it doesn't necessarily
  * represent a final fragment of a select from the application's schema. In that case it's best thought as a signature
  * containing declarations of terms (mappings and parameters) over which SQL expressions can be defined;
  * hence `SQLExpression` is parameterized with a `RowProduct`.
  * {{{}}}
  * Instances and subclasses of this type are typically referenced in the documentation as ''from'' clauses
  * for simplicity. It is, however, also used as a supertype of [[net.noresttherein.oldsql.sql.GroupByClause group by]]
  * clauses, with their associated ''from'' clauses. Furthermore, a `RowProduct` of a nested ''select''
  * includes also the `RowProduct` for its outer ''select''
  * as its [[net.noresttherein.oldsql.sql.RowProduct!.Implicit implicit]] prefix. 'Pure' ''from'' clauses
  * (without a ''group by'' clause and disallowing use of their tables in aggregate expressions) are subtypes of
  * [[net.noresttherein.oldsql.sql.FromClause FromClause]] and are called 'ungrouped' ''from'' clauses,
  * to distinguish between '[[net.noresttherein.oldsql.sql.AggregateClause aggregate]]' or 'grouped' clauses.
  * Similarly, when a need arises to specifically mention an actual ''from'' clause of an SQL ''select'' expression,
  * it is referred to as an '[[net.noresttherein.oldsql.sql.RowProduct!.Explicit explicit]]' or 'actual' ''from'' clause,
  * and its full type as a 'full' or 'extended' clause.
  * {{{}}}
  * The mappings taking part in the query are encoded in the static type of the standard implementations, which
  * build both the instance and its type by recursively applying various
  * [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] subtypes to a shorter clause, expanding it by another relation.
  * In that aspect it is quite similar to a general purpose polymorphic tuple or a heterogeneous list such as
  * the ''shapeless'' `HList` and [[net.noresttherein.oldsql.collection.Chain Chain]], replacing its wider functionality
  * with an application specific to this task. Just as a `*:` - and all other two-argument type constructors -
  * it can be and is better written using the infix notation: `Dual Join Mages Join Familiars Join SpellBooks`.
  * Note however that the whole `Adjoin` class hierarchy is left-associative for a more natural left to right reading
  * and writing. Specialized classes for all join kinds exist, as well as special variants for ''from'' clauses
  * of subselects of some outer clause and unbound query parameters. ''Group by'' clauses are also represented under
  * this type, with all subtypes having to extend either [[net.noresttherein.oldsql.sql.FromClause FromClause]] or
  * [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]] (typically through
  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause), the latter tree containing in particular
  * the [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] type. Other implementations, not derived from `Adjoin`
  * are also possible, in particular decorators extending [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]]
  * which introduce no new relations, but can add other features. As subtypes are also universally covariant
  * regarding their `RowProduct` type parameters, any prefix can be always substituted with `RowProduct` base trait
  * (or whatever upper bound on the prefix clause is imposed by a particular type), balancing static type
  * checking with a degree of freedom and flexibility promoting code reuse. Such generic base classes are called
  * [[net.noresttherein.oldsql.sql.RowProduct.WildcardRow 'wildcard']] types/clauses, to distinguish them
  * from abstract types in the Scala sense (not fully instantiated).
  * Apart from `RowProduct` and mentioned `GroupByClause`, some common such types
  * are [[net.noresttherein.oldsql.sql.FromSome FromSome]]
  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow NonEmptyRow]].
  * {{{}}}
  * This type is thus more general than simply an implementation of an actual ''from'' clause of a select:
  * not only does it include the ''where'' clause and the potential ''group by'' clause is incorporated into
  * the class hierarchy, but it also allows represent dependent selects, by combining a ''from'' clause
  * of a subselect with the relations brought to scope by enclosing selects. This is done
  * by [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join, which linearizes the `RowProduct`
  * of the outer select and the ''from'' clause of the subselect by joining them together. Hence, from the point of view
  * of an SQL select expression derived from such a clause, only the relations to the right of the last
  * `Subselect` join are a part of the ''from'' clause. Each subtype is thus divided into
  * an [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] (or inner) section, which contains all relations
  * which will be listed in the actual ''from'' clause of an SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL select]]
  * based on it, and an [[net.noresttherein.oldsql.sql.RowProduct.Implicit implicit]] (or outer) section, which contains
  * all relations from the enclosing ''selects''(s), available to SQL expressions based on this clause
  * (assuming the database engine supports ''dependent'' selects). The divide is the rightmost `Subselect`
  * pseudo join, with the explicit section spanning the type in its entirety if none such is present.
  * The implicit section is thus the type of the ''from'' clause of the SQL select directly enclosing
  * the actual ''from'' clause. On the other hand, a `GroupByClause` clause 'hides' all preceding relations
  * down to the most recent `Subselect` or the start of the clause, which are under grouping and thus unavailable
  * as individual rows. Subtypes with a `Subselect` pseudo join are collectively referenced as 'dependent' clauses,
  * to distinguish them from `Subselect` type itself. Dependent clauses including a ''group by'' clause
  * in their implicit section (and not the explicit section) are not considered 'grouped' clauses.
  * {{{}}}
  * A `RowProduct` subtype is said to be ''complete'' if its definition starts with
  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]], rather than
  * a wildcard type, such as the root `RowProduct`, and the number of all mappings participating in the join/clause
  * is known. Note that the mapping types may still be abstract and the joins may be of some generic base type like
  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]. The opposite is an ''incomplete'' clause, the definition of which
  * starts with the base `RowProduct` or one of its wildcard subtypes which do not expose their prefix clause type,
  * or an abstract type `F <: RowProduct`: `F Join Deaths Join Instruments`. In all these cases,
  * the wildcard/abstract type stands for any sequence of relations, and the type from the example describes any
  * ''from'' clause ending with tables `Deaths` and `Instruments`. A ''complete'' clause is called ''concrete'' if
  * all join types in its signature are concrete clauses (concrete here means any public trait having a dedicated
  * factory in its companion object). Additionally, each `RowProduct` subtype defines a `Generalized` member type
  * as its supertype which still allows external functions to correctly discern its role in the whole clause.
  * A ''complete'' clause is then called ''generalized'' if its `Generalized` type is well defined,
  * that is the static type of the former is a subtype of the latter.
  * {{{}}}
  * This trait is a bare bones interface as well as a wildcard in type definitions meaning
  * 'an arbitrary number of other preceding relations not relevant to the current problem'. Most functionality
  * is defined in the `Expanded` and `AndFrom` subclasses, representing actual non-empty clauses. Some additional
  * generic methods however are made available through implicit conversions in order to benefit from a static self type:
  *   - from any `F &lt;: RowProduct` to
  *     [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]`[`F`]`,
  *     which define accessor methods returning [[net.noresttherein.oldsql.schema.Mapping mappings]] for the relations
  *     in the clause, which are in turn implicitly convertible to
  *     SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] usable in the ''select'' and ''where''
  *     clauses;
  *   - from any `F &lt;: RowProduct`
  *     to [[net.noresttherein.oldsql.sql.RowProduct.RowProductExtension RowProductExtension]], which declares
  *     some accessors, in particular
  *     `relations` to [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] and
  *     `mappings` to [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]],
  *     and `select` methods creating SQL [[net.noresttherein.oldsql.sql.ast.SelectSQL selects]] based on this ''from'' clause;
  *   - from non-empty discrete clauses `F &lt;: FromSome` (those without a ''group by'' clause)
  *     to [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]], which define
  *     factory methods for other join types;
  *   - from clauses without a `Subselect` join `F &lt;: TopFromSome`
  *     to [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension TopFromSomeExtension]] for adding unbound
  *     query parameters in the form of the [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] synthetic 'joins'.
  *
  *     Aside from the above extensions, associated implicits and other helper classes, the companion object
  *     defines some base types serving as common upper bounds and several useful type aliases which enforce
  *     certain features on `RowProduct` instances, such as [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]],
  *     [[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]],
  *     [[net.noresttherein.oldsql.sql.RowProduct.As As]] and others.
  *
  * @see [[net.noresttherein.oldsql.sql.FromSome]] the base trait for all non-empty clauses
  *      (without a ''group by'' clause).
  * @see [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] the root of the type hierarchy of classes which add another
  *      relation to some other clause.
  * @see [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] the 'join' type expanding a preceding discrete clause
  *      by another relation and the base trait for all SQL join kinds.
  * @see [[net.noresttherein.oldsql.sql.Dual Dual]] the standard empty clause used for ''select'' statements
  *      without a ''from'' clause (or 'select ... from Dual' in Oracle) as well as the terminator of non-empty
  *      relation lists.
  * @see [[net.noresttherein.oldsql.sql.From From]] The factory for clauses with a single relation which
  *      is the most common starting point for building larger clauses.
  * @see [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] the supertype of all ''from'' clauses
  *      with a ''group by'' clause.
  * @see [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] a ''group by'' clause under the interface
  *      of a ''from'' clause, introducing the first grouping expression.
  *
  * @author Marcin Mościcki
  */ //consider: renaming simply to Relations or Clause
trait RowProduct extends RowProductTemplate[RowProduct] with Serializable { thisClause =>
	private[oldsql] type ThisType = this.type

	/** Type of the last `Mapping` in this join (the rightmost one) if not empty. `Dual` defines it as `Nothing`. */
	type LastMapping[O] <: MappingAt[O]

	/** Type alias for the last relation in this list as seen from some `RowProduct` type `F` containing this instance
	  * in it1s 'tail'. In other words, this projects the type of the last element of this clause to one based
	  * on an expanding clause. `JoinedRelation[F, LastMapping]` provides access to the `Mapping` for the relation
	  * together with all its components and is an instance of `SQLExpression[F, GlobalScope, LastMapping[_]#Subject]`,
	  * meaning it can be used as part of larger SQL expressions. Notably, `Dual`, containing no relations,
	  * defines this type as `Nothing`.
	  */
	type Last[F <: RowProduct] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `RowProduct` using
	  * the most abstract link ('join') kind which can still take the place of this type as a parameter in
	  * larger clauses. For non-empty, ''discrete'' clauses it is
	  * `RowProduct `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
	  * [[net.noresttherein.oldsql.sql.RowProduct.LastMapping LastMapping]] - the upper bound on all discrete clauses
	  * containing `LastMapping`. If this clause is not a `AndFrom` subclass but a decorator,
	  * the ''generalized'' supertype of the decorator is used as the wrapper over `RowProduct AndFrom LastMapping`.
	  * Similarly, ''group by'' clauses define it as
	  * `RowProduct `[[net.noresttherein.oldsql.sql.AndBy AndBy]]` LastMapping`, with the exception of decorators,
	  * which again use former decorated in the generalized supertype of the decorator type.
	  * `Dual` defines this type as the most generic `RowProduct` to indicate that any expression based on
	  * an empty clause can be used as a part of expressions for any other clause.
	  * The [[net.noresttherein.oldsql.sql.RowProduct.last last]] joined relation of this instance uses this type
	  * as its domain type parameter.
	  */
	type FromLast >: Generalized <: RowProduct

	/** Given a type constructor for a ''from'' clause accepting a `FromSome` type, apply this type to it
	  * if it conforms to `FromSome`. This is a helper type allowing the declaration of
	  * [[net.noresttherein.oldsql.sql.RowProduct.filterNext filterNext]], used in the implementation of
	  * [[net.noresttherein.oldsql.sql.AndFrom.on AndFrom.on]].
	  */
	type FromNext[E[+L <: FromSome] <: RowProduct] <: RowProduct

	/** The last relation in this clause when treated as a list, if any.
	  * @return a [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] instance of type specific to this
	  *         clause type. The expression is always [[net.noresttherein.oldsql.sql.ast.JoinedRelation.isDefault default]],
	  *         meaning it does not alter its [[net.noresttherein.oldsql.sql.ast.JoinedRelation.relation relation]]
	  *         in any way (`last.`[[net.noresttherein.oldsql.sql.ast.JoinedRelation.altered altered]]` == last.relation.`[[net.noresttherein.oldsql.schema.Relation.export export]])
	  */
	@throws[NoSuchElementException]("if this clause is empty.")
	def last :Last[FromLast]

	/** The last relation in this clause as an expression based on some expanding clause `E`.
	  * For this particular purpose, if `E` contains this type as its prefix, but also
	  * a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause following this relation,
	  * and they are not separated by a [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join,
	  * it is considered unusable and this method is impossible to call due to the lack of the required implicit argument.
	  *
	  * This is equivalent to `last.asIn[E]`, but returns the result as the narrowed `Last[E]` rather than
	  * `JoinedRelation[E, LastMapping]`. Thus it preserves the `Nothing` return type for empty clauses and allows
	  * its use in the `on` filtering method.
	  * @tparam E a `RowProduct` type which results from application of various `RowProduct` type constructors,
	  *           in particular `Adjoin` implementations, to this type.
	  * @param expansion an implicit witness to the fact that clause of type `E` can by created by adding additional
	  *                  links (`Adjoin` instances) to this clause and that this relation is available
	  *                  to SQL expressions based on that clause.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf]]
	  */
	def lastAsIn[E <: FromSome](implicit expansion :FromLast PrefixOf E) :Last[E]


	//consider: renaming to Domain or SuperEgo; Erasing all invisible relations from the type
	/* Todo: we need two types: one which lists param joins explicitly in order to have a definite definition of Params,
	 *  and one which reduces all joins, param joins and what not to the same supertype, so that we can easily substitute
	 *  a table for a parameter. Note that any type which starts with a wildcard clause has undefined Params member type,
	 *  so having Generalized distinguish between them doesn't have a real use. The issue is less clear with GroupBy
	 *  and Subselect, as they affect which relations are actually available. If we were to erase that difference as well,
	 *  we would probably need to use a type like this.Outer PseudoJoin T and use that as the domain of SQL expressions.
	 *  Unfortunately, satisfying this.type <:< this.Outer PseudoJoin T, or even this.Self <:< this.Outer PseudoJoin T
	 *  seems next to impossible, and it is a prerequisite for using it as expression domain type. Almost certainly
	 *  PseudoJoin would need to be a type alias. Perhaps
	 *  {{{
	 *      type PseudoJoin[+L, R[A]] = (L Expanded T) | GroupByClause ((RowProduct GroupBy T) { type.Outer <: L })`
	 *  }}}
	 *  Of course, this precludes any operations on parameters and any use of aggregate functions, so it is not clear
	 *  it would not prevent valid use cases where we start with an SQL expression's domain type.
	 *  Certainly worth a try, though. */
	/** A super type of this instance used as the basis for its ''where'' clause
	  * [[net.noresttherein.oldsql.sql.RowProduct.filter filter]] expression (and other expressions used by this clause).
	  * It is the supertype of [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] created by replacing all join kinds
	  * (and in general, all `RowProduct` types and type constructors) with their generalized supertypes. For example,
	  * the generalized form of [[net.noresttherein.oldsql.sql.Dual Dual]] is `RowProduct` itself, while for
	  * [[net.noresttherein.oldsql.sql.InnerJoin InnerJoin]], [[net.noresttherein.oldsql.sql.LeftJoin LeftJoin]], etc.,
	  * it is [[net.noresttherein.oldsql.sql.Join Join]]. Every concrete subclass of `RowProduct` declares
	  * its most abstract superclass which still provides distinction necessary to fulfill its intended role
	  * in an SQL select. Its `Generalized` type is then formed recursively, and every subtype
	  * `J[+F <: RowProduct] <: RowProduct`, having a member `val `''f''` :F`, is substituted by `G[f.Generalized]`,
	  * where `G[+F <: RowProduct] >: J[F]` is the generalized version of type constructor `J`. As a special case,
	  * initial `From[T]` is replaced with `RowProduct `[[net.noresttherein.oldsql.sql.NonParam NonParam]]` T`.
	  * In this way, the generalized supertype of any clause is also a supertype of clauses which prepend relations
	  * to the former at the front by replacing `RowProduct` wildcard with a more concrete type.
	  *
	  * In general, it is the most abstract supertype of this clause
	  * which retains full information required to determine exactly which relations are available
	  * to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] based on it
	  * (because not all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] subtypes actually include the relations listed
	  * by their left side - see [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]), as well as its role (for example,
	  * a ''group by'' expression or a statement parameter, to mention something else than regular tables).
	  * It abstracts over the exact joins used and focuses on preserving the compatibility
	  * between all `SQLExpression[Generalized, S, V]`, regardless of the exact `RowProduct` instance
	  * used as their domain. The subtype relation of clause classes is preserved by their generalized types:
	  * if `F &lt;: S` are clause types with fully defined `Generalized` types and `f :F, s :S`,
	  * then `f.Generalized &lt;: s.Generalized`. This is in particular exploited when `S` is a 'suffix clause'
	  * of `F`, i.e. `S` starts with a wildcard type and `F` can be formed by replacing that wildcard with
	  * a more concrete type. In that case `SQLExpression[s.Generalized, A, V] &lt;: SQLExpression[f.Generalized, A, V]`,
	  * and expressions used as ''filter'', ''group by'', ''having'' and ''select'' clauses for `S` can also be used
	  * as such for `F`.
	  *
	  * This type is of central importance, because the filter for the associated ''where''
	  * clause and all joined relations accessed through
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] conform to
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[Generalized, GlobalScope, _]`.
	  * Clauses with the same `Generalized` type are considered interchangeable in most cases, although,
	  * depending on the exact implementations of [[net.noresttherein.oldsql.schema.Mapping mappings]] of included
	  * relations, using a different instance of the same relation type
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]`[M]`/[[net.noresttherein.oldsql.schema.Table Table]]`[M]`
	  * may lead toclauses including them not being [[net.noresttherein.oldsql.sql.RowProduct.homomorphic homomorphic]],
	  * and, as a result, SQL expressions built based on those clauses and relations being incompatible
	  * despite type checking. The implementation goes as far as possible to minimise those occurrences, though.
	  */ //it would be really tempting to declare it Generalized <: FromLast (saves a lot of overrides), but the result is volatile
	type Generalized >: Complete <: RowProduct {
		//todo: try to add InnerParams = thisClause.InnerParams
		//caution: there is a serious issue in that this.generalized.Self <:< this.generalized.Generalized does not hold.
		//  This is a direct consequence of using Self as a lower bound, rather than declaring Self <: Generalized.
		//  The alternative however would make Self volatile, so no member types of self could be used.
		type FromLast    <: thisClause.FromLast
		type Generalized <: thisClause.Generalized //these can't be =:= because Dual.Generalized =:= RowProduct
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type Base        <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	/** This clause upcast to its generalized supertype in which all join kinds are replaced with their least upper
	  * bounds which still retain the information about their function.
	  * @return `this.self`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Generalized]]
	  */ //this makes sense for type inference, but also poor Scala 2 intersection types, where Self may not be a subtype of Generalized
	@inline final val generalized :Generalized = this.asInstanceOf[Generalized] //a val for member types

	/** The `Class` object for the class/trait of this instance's
	  * [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] type.
	  * In most cases, `getClass != generalizedClass`, but always `this.generalizedClass.isInstance(this)`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.selfClass]]
	  */
	def generalizedClass :Class[Generalized]

	/** A supertype of clause's dynamic type whose number of relation's is known and whose all (pseudo) join kinds'
	  * [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] supertypes are known.
	  * The requirement that the relation list is complete implies that the clause starts
	  * with a [[net.noresttherein.oldsql.sql.From From]] clause (or [[net.noresttherein.oldsql.sql.Dual Dual]]),
	  * rather than some wildcard type. This type is formed recursively, with every clause type whose type constructor
	  * accepts a preceding clause type `F` and is at the same time its generalized form, replaces `F`
	  * with `this.<prefix>.Complete`, where `<prefix> :F` is the corresponding property.
	  * For example, given a `join :L `[[net.noresttherein.oldsql.sql.Join Join]]`R`, the type will be be defined
	  * as `join.Complete =:= join.GeneralizedJoin[join.left.Complete, R]`. The recursion terminates with `From`:
	  * `From[T]#Complete = From[T]` (or, optionally, [[net.noresttherein.oldsql.sql.Dual Dual]]: `Dual#Complete = Dual`.
	  *
	  * This type is very closely related to [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]:
	  * by replacing the initial `From[T]` of any clause's `Complete` type with `RowProduct NonParam T` one obtains
	  * its `Generalized` type. The other difference is more subtle: `Complete` preserves exactly most of the important
	  * member types of `RowProduct`, while `Generalized`' offers information about only a handful of types,
	  * and does so only through upper bounds. Perhaps most importantly, for any `from :RowProduct`,
	  * `from.Complete =:= from.complete.Complete` and `from.Params =:= from.complete.Params`.
	  *
	  * `Generalized` is used as the type parameter of SQL expressions used as ''where'', ''having'',
	  * ''group by'' and ''select'' clauses, and to join two clauses together. On the other hand, `Complete`
	  * is needed for [[net.noresttherein.oldsql.sql.Select Select]]`#`[[net.noresttherein.oldsql.sql.Select.From From]],
	  * because it is the lowest supertype of any clause for which the full parameter list is known.
	  *
	  * For any `clause :RowProduct`, the three self types always form a linear order:
	  * `clause.Self <:< clause.Complete` and `clause.Complete <:< clause.Generalized`.
	  */
	type Complete >: NoAlias <: RowProduct {
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O] //lets leave some flexibility for unforeseen implementations
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	/** This clause as its [[net.noresttherein.oldsql.sql.RowProduct!.Complete Complete]] type.
	  * @return `this.asInstanceOf[Complete]`.
	  */
	@inline final val complete :Complete = this.asInstanceOf[Complete]

	/** The [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type upcast to its super type which doesn't
	  * feature an alias for its relation. This has the effect of transforming
	  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` into `F`, where `F =:= f.Self`.
	  * Note that this type strips only the ''last'' alias, with all previous relations in `F` retaining their aliases
	  * (if any) in the form equal to `Self`. For this reason, while all 'bottom' implementations such as
	  * [[net.noresttherein.oldsql.sql.InnerJoin InnerJoin]], etc. provide a concrete definition of `NoAlias`,
	  * it is still not a concrete type as it is defined in terms of a bound type `left.Self`.
	  * This is an implementation type artefact required for the definition of the `Self` type and of little or no use
	  * in client code, unless it involves the manipulation of the `As` clause. Its direct usage should be avoided
	  * in favour of global types, as it may disappear with changes to implementation.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Generalized]]
	  */ //todo: in many subtypes these copy&paste definitions can be replaced with Self <: NoAlias and NoAlias <: Complete
	type NoAlias >: Self <: RowProduct {
		type LastMapping[O] = thisClause.LastMapping[O]
		//		type Last[O <: RowProduct] = thisClause.Last[O] //lets leave some flexibility for unforeseen implementations
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	/** A recursive reconstruction of this clause's concrete type. It is defined by replacing every type parameter
	  * `F <: RowProduct`, corresponding to a member `val f :F`, with `f.Self`. A concrete clause `C`
	  * without type parameters ([[net.noresttherein.oldsql.sql.Dual Dual]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]]) defines it simply as `type Self = C`,
	  * terminating the recursion. The result is the lowest upper bound of a clause `F` without wildcard `RowProduct`
	  * ''subclasses'' in its definition. If `F` is an abstract type, all abstract type parameters in its signature
	  * remain abstract types in `f.Self`.
	  *
	  * This procedure circumvents possible covariance in the type parameter `F`, which would prevent defining an alias
	  * for the self type using `F` directly in its definition. For a concrete class with fully instantiated
	  * type parameters, it is its concrete supertype. Any additions into the class hierarchy must provide
	  * a definition of `Self` to which every instance of the class can be safely cast,
	  * as the [[net.noresttherein.oldsql.sql.RowProduct.self self]] property is final and implemented
	  * as `this.asInstanceOf[Self]`.
	  *
	  * Most implementations - all subtypes of [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow NonEmptyRow]]
	  * define their `Self` type only through an upper bound. While on the surface this opens a possibility
	  * for a concrete class `S <: T, T <: RowProduct` to narrow down the definition of `Self` inherited from `T`
	  * with a lower upper bound, this is in fact only to accommodate adding
	  * an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause to to clause `S` or `T`, which narrows down
	  * the aliased clause with a definition `type Self = NoAlias As A`, where `A` is a `String` literal used
	  * as an alias for the last table. For this reason, this type should not be more specialized further
	  * if a class inherits a concrete definition of [[net.noresttherein.oldsql.sql.RowProduct.NoAlias NoAlias]].
	  *
	  * For the above reason, `f.type <:< f.Self` will hold only if `f.Self` is fully known,
	  * that is if ''all'' [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] occurrences in its definition
	  * (including [[net.noresttherein.oldsql.sql.From From]]
	  * and all [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]] are followed
	  * by an `As` clause. The unfortunate implication of this fact is that `s.Self <:< s.self.Self` will likewise
	  * rarely hold. While a requirement of `F <: RowProduct { type Self <: F }`
	  * or `F <: RowProduct { type Self = S }, S <: RowProduct { type Self <: S }` would simplify most generic code,
	  * it should be avoided as it will rarely hold. Try to use `this.self` instead of `this` or migrate
	  * to the `Generalized` type, as function `S -> S#Generalized` is defined as monotonic
	  * (in reality it is a fixed point transformation).
	  *
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.NoAlias]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Generalized]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Copy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ConcreteRow]]
	  */ //we can't preserve Self and Outer without adding refinements to all join types (because of aliases)
	type Self <: RowProduct { //consider: renaming to Ego
		type LastMapping[O] = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O] //lets leave some flexibility for unforeseen implementations
		type FromLast    = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type Base        = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	/** This clause as its [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type.
	  * @return `this.asInstanceOf[Self]`.
	  */
	@inline final val self :Self = this.asInstanceOf[Self]

	/** The class object of this instance's [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type.
	  * Type `Self` for most implementations is defined only by its upper bound, in order to accommodate
	  * adding an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause, but it is always the same object
	  * as `Class[this.`[[net.noresttherein.oldsql.sql.RowProduct.NoAlias NoAlias]]`]`.
	  * It is unlikely to be the same object as `this.getClass`, but `selfClass.isInstance(this)` always holds.
	  */
	def selfClass :Class[Self]



	/** The filter condition applied to all rows represented by this product relation.
	  * It combines [[net.noresttherein.oldsql.sql.Adjoin.condition conditions]] of all pseudo joins
	  * included in the [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] section of this clause.
	  * For instances representing a true (ungrouped) ''from'' clause, it is a conjunction of all join conditions
	  * added with [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.on on]] and
	  * and [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] methods.
	  * Note that the final SQL doesn't have to use all, or any of these conditions, as the ''where'' clause,
	  * but instead opt to include them in ''on'' clauses with every join clause.
	  * For [[net.noresttherein.oldsql.sql.GroupByClause grouped]] clauses, this is a conjunction of all conditions
	  * added with [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having]] and is used
	  * for the ''having'' clause of the final SQL.
	  *
	  * This condition does ''not'' include filters any preceding the rightmost
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join in this clause,
	  * or preceding the last [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause, whichever comes later.
	  */
	def filter :GroupedBoolean[Generalized] = filter(generalized)

	/** The combined join conditions of all joins since the last `Subselect` or `GroupBy` join as a expression based
	  * on an expanding clause. Used by zero-argument `filter` to request the individual join conditions
	  * as expressions based on the clause it was called for.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GroupedBoolean[E]


	/** A boolean function of two relations - the `last` relation of this clause and another one for mapping `N` -.
	  * Used by the [[net.noresttherein.oldsql.sql.AndFrom.on on]] method to provide a join condition between this
	  * clause and another relation. It is always parameterized with the same set of type members of the filtered
	  * relation: `next.GeneralizedLeft`, `next.FromLast`, `next.Generalized` and `next.LastMapping`.
	  * For non-empty clauses and an expanding join `next :_ AndFrom N`, this type essentially resolves to:
	  * {{{
	  *     (JoinedRelation[FromLast next.GeneralizedJoin N, LastMapping], JoinedRelation[next.FromLast, N])
	  *         => SQLBoolean[next.Generalized]
	  * }}}
	  * The clauses used as type parameters of both arguments are supertypes of `next.Generalized`,
	  * so any boolean expression based on them will automatically conform to `SQLBoolean[next.Generalized]` due
	  * to `SQLExpressions`'s contravariance.
	  * `Dual` however defines this type as `Nothing`, and not having a lower bound means that for clauses
	  * not conforming to `FromSome` it is also equivalent to `Nothing`, meaning no argument can be provided
	  * and the `next.on` method cannot be called.
	  *
	  * The type parameters are:
	  *   - `E[+L]` a type constructor for the left (this) clause, accepting the left side of the join (`GeneralizedLeft`);
	  *   - `S` the `FromLast` type of the expanding clause (adding the following relation);
	  *   - `G` the `Generalized` type of the expanding clause;
	  *   - `T` the mapping type of the following relation.
	  */
	type FilterNext[E[+L <: FromSome] <: RowProduct Expanded T, S <: RowProduct Expanded T, G <: S, T[O] <: MappingAt[O]] <:
		           (JoinedRelation[FromNext[E], LastMapping], JoinedRelation[S, T]) => SingleBoolean[G]

	/** The implementation method to which [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] delegates. */
	protected def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	              (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :next.Copy

	private[sql] final def `->filterNext`[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N])
			:next.Copy =
		filterNext[F, N](next)(filter)



	/** The type of the last [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameter among these relations.
	  * It is the last element of the type of chain [[net.noresttherein.oldsql.sql.RowProduct.Params Params]].
	  */
	type LastParam

	/** A [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters of this clause joined with
	  * the [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param]] method.
	  * In particular, a `RowProduct` subtype with no `JoinParam` joins in its full type has this type equal to `@~`.
	  * This is can be used to statically enforce that a clause is parameterless by refining on this type:
	  * `from :RowProduct { type Params = @~ }`, or `from :ParamlessFrom` as a less bug-prone alternative.
	  */
	type Params <: Chain //consider: type InnerParams (and maybe OuterParams)

	/** The type of this clause with the last [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo join
	  * removed. It is the result type of method [[net.noresttherein.oldsql.sql.RowProduct.bind(param:LastParam) bind]]
	  * used to provide the value for the last parameter.
	  */
	type AppliedParam <: RowProduct

	/** The [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form of this instance's
	  * [[net.noresttherein.oldsql.sql.RowProduct.Paramless Paramless]] type - the type of the `RowProduct` resulting
	  * from removing all [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins from this `RowProduct`.
	  */
	type GeneralizedParamless >: Paramless <: RowProduct

	/** The type of this clause with all [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] expansions
	  * removed. It is the result type of method
	  * [[net.noresttherein.oldsql.sql.RowProduct.bind(params:Params) bind]]`(`[[net.noresttherein.oldsql.sql.RowProduct.Params Params]]`)`,
	  * which replaces all references to unbound parameters with [[net.noresttherein.oldsql.sql.ast.BoundParam bound]]
	  * parameter expressions.
	  */ //todo: Paramless cannot be preserved by Complete, but if we introduced GeneralizedParamless, we might.
	type Paramless <: ParamlessRow

	/** The upper bound for the [[net.noresttherein.oldsql.sql.RowProduct.Paramless Paramless]] version of this clause,
	  * which allows its usage as part of larger clauses. It is bound by
	  * [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessFrom]]; wildcard classes
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]] and
	  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] define it as parameterless refinements
	  * of themselves, while their superclasses declare it as bound from above by their parameterless refinements.
	  */
	type BoundParamless >: Paramless <: ParamlessRow
	//todo: DecoratedGeneralizedParamless
	/** A helper type used in the definitions of [[net.noresttherein.oldsql.sql.RowProduct.Paramless Paramless]]:
	  * [[net.noresttherein.oldsql.sql.ParamClause Unbound]] parameters define it as their `Paramless`
	  * (ignoring the argument), while other types define it as the argument type `D`. This gives the option
	  * to `RowProduct` implementations which are closely tied to the preceding relation/join
	  * (in particular [[net.noresttherein.oldsql.sql.DecoratedRow decorators]]) to remove themselves
	  * from their `Paramless` type together with annotated preceding clause.
	  */
	type DecoratedParamless[D <: BoundParamless] <: BoundParamless

	//consider: both bind methods should probably only bind parameters from the most nested subselect
	/** Applies this clause to its last parameter, removing the last
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] and substituting all references to its parameter
	  * components with [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions
	  * with values derived from `param`. Parameters in aggregated sections (for example, under
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] are treated the same way as all others
	  * @param param the value for the last parameter pseudo relation joined with `UnboundParam`
	  * @return a `RowProduct` instance obtained by removing the rightmost `UnboundParam` pseudo join
	  *         from this clause's type.
	  */
	def bind(param :LastParam) :AppliedParam

	/** Applies this clause to its parameters: removes all [[net.noresttherein.oldsql.sql.ParamClause unbound]]
	  * parameters from this clause and substitutes all references to parameter components with
	  * [[net.noresttherein.oldsql.sql.ast.BoundParam bound]] parameter expressions with values derived
	  * from the corresponding chain elements. This method will include ''all'' parameters in their order of appearance
	  * in `F`, including those in the grouped, 'discrete' section, which aren't available for use in SQL expressions.
	  * @param params a chain consisting of subject types of all parameter mappings of all `UnboundParam` joins
	  *               in this clause in order of their appearance. Note that there is an implicit conversion
	  *               from a tuple of matching arity and element types.
	  * @return a `RowProduct` instance obtained by removing all `UnboundParam` instances from this clause's type.
	  */ //todo: if we make this method bind only params in the explicit segment, than it would preserve Implicit and a TopRow would become a GroundRow - neat!
	def bind(params :Params) :Paramless

	/** Retrieves the value of the given unbound parameter from the given set of of this clause's parameters. */
	def eval[F >: Generalized <: RowProduct, M[O] <: TypedMapping[S, O], S]
	        (param :JoinedParam[F, M])(params :Params)(implicit subject :M[Unit] <:< MappingOf[S]) :S =
		parameterization.apply[M, S, F](param)(params) //very ineffective, we'd do well to override it with specific implementations.

	/** Forwards to the `decoratedBind` method of `prefix`.
	  */
	protected final def decoratedBind[U <: ParamlessRow, D <: U]
	                                 (prefix :RowProduct { type BoundParamless = U })
	                                 (params :prefix.Params)(decorate :prefix.Paramless => D)
	        :prefix.DecoratedParamless[D] =
		prefix.decoratedBind[D](params)(decorate)

	/** Implementation target of [[net.noresttherein.oldsql.sql.RowProduct.bind bind]] for
	  * [[net.noresttherein.oldsql.sql.DecoratedRow decorator]] clauses which should be removed from the
	  * [[net.noresttherein.oldsql.sql.RowProduct.Paramless Paramless]] result together with a decorated
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]]. `UnboundParam` returns simply their `Paramless`
	  * form, while other implementations apply the `decorate` function to their `Paramless` form, reapplying
	  * the decorator in the process.
	  */
	protected def decoratedBind[D <: BoundParamless]
	                           (params :Params)(decorate :Paramless => D) :DecoratedParamless[D]

	/** Does this clause contain any [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] 'joins'
	  * (or its subtypes)?
	  * @return `false` ''iff'' `this.paramCount =:= 0`.
	  */
	def isParameterized :Boolean = paramCount == 0

	/** Does this clause contain any [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] 'joins'
	  * (or its subtypes) in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] section?
	  * Such clauses cannot be used to create SQL ''select'' statements.
	  * @return true ''iff'' `this.Base =:= Nothing`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Base]]
	  */
	def isExplicitParameterized :Boolean

	/** The total number of [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in this clause,
	  * i.e. the length of the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] chain. This includes
	  * parameters under a ''group by'' clause (for which relations are not listed
	  * in [[net.noresttherein.oldsql.sql.RowProduct.fullTableStack fullTableStack]]).
	  */
	def paramCount :Int

	/** The index of the rightmost ''visible'' [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] in this clause.
	  * Counting goes from right to left and starts with zero;
	  * [[net.noresttherein.oldsql.sql.RowProduct.fullTableStack fullTableStack]]`(lastParamOffset)` will return
	  * the [[net.noresttherein.oldsql.sql.ast.JoinedParam JoinedParam]] representing the parameter.
	  * If the last parameter relation is not present in `fullTableStack` because it is
	  * in an [[net.noresttherein.oldsql.sql.AggregateClause.Discrete ungrouped]] section
	  * of an aggregate/''group by'' clause, `-1` is returned.
	  * @throws UnsupportedOperationException if `!this.isParameterized`.
	  */
	def lastParamOffset :Int


	/** Does this clause contain no relations, being a base for a select without a ''from'' clause? */
	def isEmpty :Boolean

	/** Does this clause contain any relations? */
	@inline final def nonEmpty :Boolean = !isEmpty

	/** Number of relations available in this join (including
	  * [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters
	  * and [[net.noresttherein.oldsql.sql.GroupByClause.AbstractGrouping grouping]] expressions), in both the implicit
	  * and explicit parts (counting all their occurrences separately). Note that this doesn't always equal the number
	  * of mappings listed in the type, as the relations between any `Subselect`-`GroupBy` pair are not counted.
	  * This makes `fullSize` consistent with the length of
	  * [[net.noresttherein.oldsql.sql.RowProduct.fullTableStack fullTableStack]]; however,
	  * [[net.noresttherein.oldsql.sql.RowProduct.fullRow fullRow]] may contain fewer elements, as it does not include
	  * parameter expressions.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.size]]
	  */
	def fullSize :Int

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'RowProduct' is the prefix clause of its `Generalized` type, with all its relations.
	  */
	@inline final def generalizedSpan :RowProduct ExpandedBy Generalized = new ExpandedBy(fullSize)

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'Dual' is the prefix clause of its `Self` type, with all its relations.
	  */
	@inline final def fullSpan :Dual ExpandedBy Self = new ExpandedBy(fullSize)


	/** The number of all relations available in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'RowProduct' is the prefix clause of its `Generalized` type, with all its relations.
	  */
	@inline final def generalizedSuffix :RowProduct PrefixOf Generalized = new PrefixOf(fullSize)

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'Dual' is the prefix clause of its `Self` type, with all its relations.
	  */
	@inline final def fullSuffix :Dual PrefixOf Self = new PrefixOf(fullSize)


	/** Subject types of the mappings of all available relations in this clause, excluding unbound parameters
	  * (synthetic relations joined using [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins),
	  * concatenated into a heterogeneous list. Aside from unbound parameters, this chain excludes any relations
	  * between a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]
	  * (or [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] clause and the last preceding
	  * [[net.noresttherein.oldsql.sql.From From]]/[[net.noresttherein.oldsql.sql.Subselect Subselect]] clause,
	  * as they are not available as individual rows.
	  *
	  * The [[net.noresttherein.oldsql.collection.Chain chain]] contains the mapped types
	  * in the same order as their mappings appear in this type's definition and is,
	  * like [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] (and all [[net.noresttherein.oldsql.sql.Join Join]]
	  * kinds), but unlike `::`, left associative.
	  *
	  * Note that a `FromClause` having `FullRow <:< @~`
	  * and [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]]` <:< RowProduct` implies that it consists
	  * solely of unbound parameters.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.fullRow]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow]]
	  */
	type FullRow <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all relations available
	  * in this clause in their order of appearance, excluding unbound parameters
	  * (synthetic relations joined using [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins).
	  * All mappings occurring between a [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) until the next
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], if present, are considered unavailable. The omitted
	  * segment constitutes the ''from'' clause of a single select expression with a ''group by'' clause, meaning
	  * the values of individual rows cannot be accessed.
	  */ //todo: subclass with only RelationSQL elements - replace tableStack/fullTableStack
	def fullRow :ChainTuple[Generalized, Single, FullRow] = fullRow(generalized)

	/** Create an SQL tuple expression, based on some expanding clause `E`, containing `JoinedRelation` expressions
	  * for all joined elements in their order of appearance. It will contain entries for all mappings in this clause,
	  * including parameter mappings and mappings listed in this clause's `Implicit` prefix (if this clause
	  * is a subselect clause). This overloaded variant is used by the zero-argument `fullRow` to obtain
	  * the chain prefix containing the relation formulas based on the final clause type from a prefix clause of a join,
	  * without the need to adapt each relation once for every intermediate join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def fullRow[E <: RowProduct]
	           (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow]

	/** All relations in this clause as abstract, untyped mappings, in the reverse order of their appearance.
	  * Any relations in the scope of a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause are excluded.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.fullRow]]
	  */
	def fullTableStack :Seq[RelationSQL.from[Generalized]#__] = fullTableStack[Generalized](generalized)

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * The `JoinedRelation`s are based on some expanding clause `E`, so that the stack for a prefix clause
	  * can be used as-is, with no need to map over it, by expanding clause's zero argument `fullTableStack`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def fullTableStack[E <: RowProduct]
	                  (target :E)(implicit expansion :Generalized ExpandedBy E) :LazyList[RelationSQL.from[E]#__]

	/* Todo: we have a lot of related types to rename here:
	 *    1. this.JoinedWith -> JoinTo, JoinWith, AppendedTo, Expand? a good name, because it applies both to the join kind and the joined clause, but great for PartOf
	 *    2. this.SelectedFrom -> GroundedIn, GroundIn
	 *    3. this.DirectSubselect -> SubselectClause, GroundedClause. Or we could just have a common name for it and RowProduct.SubselectRow, not such a bad idea, really.
	 *    4. this.SubselectSome -> SubselectFromClause? (SubselectFrom is bad as it suggests a type argument)
	 *    5. this.FromTable -> ok
	 *    6. this.FromSubselect (= that.AsSubselectOf) -> ok
	 *    7. RowProduct.SubselectRow -> DependentClause
	 *    8. FromClause.Expand -> remove
	 *    9. FromClause.JoinWith -> MultiJoin
	 *   10. DependantOf -> (Dependant may be a weird choice, but 'Of' suffix creates a parallel to SubselectOf)
	 *   11. PartOf -> JoinedWith (or JoinedBy/JoinedIn?) (doesn't really suggest the direction of the relation, but at least its purpose is clear)
	 *   12. ExpandedBy -> ExpandedBy
	 */
	/** A join of relations from the clause `P` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `P`, that is appending all mappings from this clause
	  * in order to `P`, using the same join kinds. The join type `J` is used as the join between the last relation
	  * in `P` and the first relation in `this`, unless this clause starts with a `JoinParam` join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.joinedWith]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.SelectedFrom]]
	  */ //todo: I think we can safely drop the NonParam bound for now
	type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] <: Generalized

	/** A join of relations from the clause `F` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `F`, that is appending all mappings from this clause
	  * in order to `F`, using the same join kinds. The `From` 'join' between `Dual` and the first relation
	  * in this clause is replaced with `firstJoin.LikeJoin`. As the call to this method is written
	  * in the reversed order to the one in which both clauses participate in the join and accepts
	  * an additional template argument, it is recommended to use one of regular `join`, `outerJoin`, etc. methods
	  * introduced by an implicit conversion: `this rightJoin that` is equivalent
	  * to `that.joinedWith(this, RightJoin.template)`.
	  * @param prefix a ''from'' clause containing first relations of the produced cross join.
	  * @param firstJoin a template join instance defining the kind of the join between the last relation in `prefix`
	  *                  and the first relation in `this`. This template will be ignored if the first join of this
	  *                  clause is the `JoinParam` type, in which case it is used instead, with the clause `F`
	  *                  replacing `Dual` as its left side.
	  * @see [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join]]
	  */
	def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin]

	/** A specialized variant of the [[net.noresttherein.oldsql.sql.RowProduct.JoinedWith JoinedWith]], it is the type
	  * of a [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join' between the relations from the clause `P`
	  * and those from this clause. It is the result of substituting the initial
	  * [[net.noresttherein.oldsql.sql.From From]]`[T]` clause in this type's definition with `P Subselect T`.
	  * The join kinds of both clauses are preserved. The difference from related type
	  * `this.`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]] is that this type joins
	  * all the table listed by this clause, including those in the [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]]
	  * section, while `AsSubselectOf` joins only those in the [[net.noresttherein.oldsql.sql.RowProduct.Inner inner]]
	  * section, i.e., the proper ''from'' clause, completely replacing the type preceding the last `Subselect`
	  * pseudo join in this clause with the type argument.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.selectedFrom]]
	  */ //consider: renaming to NestedIn, DependentOn, GroundedIn
	type SelectedFrom[+P <: NonEmptyRow] <: JoinedWith[P, Subselect] {
		type Row = thisClause.Row
		type Explicit = thisClause.Explicit
		type Generalized <: thisClause.Generalized
	}

	/** Represents this ''from'' clause as a subselect of the clause `prefix` by joining them with the
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] artificial join. The resulting clause
	  * is the result of appending the first relation from `this` to the `prefix` clause using the `Subselect` join,
	  * and then all the following relations using the same joins as in this clause. All join conditions are copied,
	  * but any additional condition narrowing the resulting cross join must be added manually. Note that the result
	  * is not necessarily a subselect of this clause (in terms of
	  * [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]), as there may be other intermediate
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins present in `prefix`. This method
	  * is the delegate target of the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
	  * method available through an implicit conversion, which should be used instead by the client code.
	  * @throws UnsupportedOperationException if this clause is empty or the first join in this clause is a `JoinParam`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.asSubselectOf]]
	  */
	def selectedFrom[F <: NonEmptyRow](prefix :F) :SelectedFrom[F]

	/** Joins the clause given as the parameter with this clause. The type of the resulting clause is the result
	  * of replacing the empty clause `Dual` in this clause's type with `F` and upcasting the join between `Dual`
	  * and the first relation `T` to `F NonParam T`. The clauses are joined using an inner join,
	  * unless any of the clauses are empty, in which case the other is returned.
	  * The difference from [[net.noresttherein.oldsql.sql.FromClause.joinWith joinWith]]
	  * is that it accepts empty clauses as arguments, but the return type is upcast to `NonParam`.
	  *
	  * This is a low-level method and client code should generally prefer the implicit extension method
	  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension.andFrom andFrom]] which uses the more natural
	  * prefix - suffix order rather than the inversion as in this method.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.joinedWith]]
	  */ //todo: rename to joinedWith (or whatever we rename joinedWith to).
	//We coud just as well use F <: RowProduct if From to create a SelectedFrom
	def appendedTo[F <: FromClause](prefix :F) :JoinedWith[F, NonParam]



	/** Does this ''from'' clause have a [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join' somewhere
	  * in its dynamic type?
	  */
	def isSubselect :Boolean = outer.nonEmpty

	/** Is this ''from'' clause a legal subselect ''from'' clause, that is does it have a
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join' somewhere in its complete (dynamic) type
	  * and no [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in the most nested subselect clause?
	  * @return `isSubselect && !isSubselectParameterized`
	  */
	def isValidSubselect :Boolean


	/** Super type of all `RowProduct` subtypes representing a valid ''from'' clause of a subselect directly nested
	  * under this clause's ''select''. This type matches only 'true' subselect clauses, that is types
	  * `S <: Generalized Subselect T0 J1 T1 ... JN TN`, where `J1...JN` are `Join` subtypes or
	  * `GroupBy`/`By` (with no `UnboundParam` 'joins'). This means that non subselect, ground clauses
	  * do not conform to this type despite free select expressions being valid subselects of any select expression.
	  * As a member type, it is able to define the [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.Base Base]] types of this refinement as `this.Generalized`, allowing
	  * it to work for abstract types: `f :f.outer.DirectSubselect`
	  * for any `f :`[[net.noresttherein.oldsql.sql.RowProduct.SubselectRow SubselectRow]]. Similarly,
	  * `s :f.DirectSubselect` for any clause `s = f subselect t ...` (where '...' denote any sequence of join / group by
	  * methods other than [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param]] and `subselect`
	  * (or any other method creating a [[net.noresttherein.oldsql.sql.Subselect Subselect]] join).
	  *
	  * This type differs from [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]] in several aspects:
	  *   - it always works on full, concrete types - if either clause `S` or `F` has a wildcard clause as
	  *     a prefix, `S` won't conform to `F#DirectSubselect` even if the whole explicit portion is fully instantiated;
	  *   - the types must match exactly in their entirety, meaning that `F` is an exact prefix of `S` (in their
	  *     generalized forms) and thus `S` contains all relations from `F`, where `S <: SubselectOf[F]` only
	  *     checks if `S#Base >: F`, which allows `S#Base` to only match a suffix of `F`.
	  *   - it matches only true subselect clauses, that is those where `F` is immediately followed by `Subselect`
	  *     in `S`, where the `S <: SubselectOf[F]` is also true for any `F` if
	  *     `S <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]].
	  *     This makes `f.DirectSubselect` a strict subtype of `SubselectOf[f.Self]`.
	  *
	  * It is worth noting that the only difference from [[net.noresttherein.oldsql.sql.Dual Dual]]`.DirectSubselect`
	  * and `GroundRow` is the upper bound on the refinement: the latter uses `RowProduct`,
	  * while the former `NonEmptyRow`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Base]]
	  */ //consider: renaming to DependentClause/Dependent/NestedClause/Nested
	type DirectSubselect = NonEmptyRow {
		type Implicit = thisClause.Generalized
		type Base     = thisClause.Generalized
		type Params   = thisClause.Params
		type DefineBase[+I <: RowProduct] = I
	}

	/** Super type of all not aggregated/grouped ''from'' clauses representing a subselect directly nested
	  * under this clause's ''select''. It is a simple narrowing of
	  * [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]]
	  * to the [[net.noresttherein.oldsql.sql.FromSome FromSome]] upper bound, introduced for convenience.
	  */ //consider: renaming to DependentSelect/NestedFromSome/NestedFrom
	type SubselectSome = FromSome {
		type Implicit = thisClause.Generalized
		type Base     = thisClause.Generalized
		type Params   = thisClause.Params
		type DefineBase[+I <: RowProduct] = I
	}


	/** The incomplete suffix clause containing all relations since the last `Subselect` join or `Dual`, with all
	  * join kinds replaced with their ''generalized'' form, while the `Subselect` in question with `AndFrom`.
	  * It lists all relations present in the actual ''from'' clause of the associated SQL select in a canonical form,
	  * abstracting over join kinds. It can be formed by substituting in the complete `Generalized` type of this clause
	  * the `Implicit` prefix, containing relations present 'implicitly' in the scope, with `RowProduct`. All `AndFrom`
	  * subclasses define it as `left.Explicit AndFrom R`, with the exception of `Subselect` join, which defines it
	  * as `RowProduct AndFrom R` instead, while `Dual` defines it as `RowProduct` - consistently with `Generalized`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Inner]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Generalized]]
	  */ //other name idea: Actual
	type Explicit >: Generalized <: FromLast// { type Explicit <: thisClause.Explicit } //it's either this or AsSubselectOf <: Inner

	/** The suffix clause containing all relations from the explicit portion of this subselect clause.
	  * It represents the actual ''from'' clause of an associated SQL subselect. For subselect clauses, it results from
	  * replacing the left side of the last `Subselect` in this type's `Self` member with `FromSome`; for non-subselect
	  * clauses (with no `Subselect` joins in its complete type), it is the full type of this clause with `Dual`
	  * replaced with `RowProduct` (or `From[T]` with `Dual AndFrom T`).
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Outer]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Self]]
	  */ //todo: currently Inner loses all aliases
	type Inner >: Self <: Explicit// { type Inner <: thisClause.Inner } //it's either this or AsSubselectOf <: Inner

	/** A prefix ''from'' clause of this clause consisting of all relations available to SQL expressions implicitly,
	  * that is not included in the actual ''from'' clause of this subselect clause.
	  * It is the generalized form of the outer clause if this clause represents a subselect clause, created by
	  * `Implicit.subselect()`. All `Join` subclasses and `From` have this type equal to the `Implicit` type
	  * of their left side, but `Subselect` defines `Implicit` as the generalized type of its left side. Additionally,
	  * all 'true' joins conform to `AsSubselectOf[L#Implicit]` and `L Subselect R` conforms to `AsSubselectOf[L]`.
	  * Therefore, `Implicit` is equal to the `Generalized` type of the left side of the last `Subselect`,
	  * or `RowProduct` for non subselect clauses. This means that for any generalized type `S <: RowProduct`
	  * with fully instantiated parameters (the clause is ''complete'' and the `Generalized` type is well defined) value
	  * `(s :S) subselect t1 join t2 ... join t3` conforms to `SubselectOf[S]` and `s.DirectSubselect`.
	  * This way one can statically express a dependency relationship between ''from'' clauses without resorting
	  * to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Outer]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Generalized]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf]]
	  */
	type Implicit <: RowProduct { type Generalized <: thisClause.Implicit }

	/** A prefix ''from'' clause of this clause consisting of all joins and relations preceding the last (rightmost)
	  * `Subselect` join. If there are no `Subselect` joins in the complete type definition of this clause,
	  * it is equal to `Dual`. It represents the clause of the outer select for this subselect clause, assuming
	  * this clause is a subselect clause created by `outer.subselect()` (and possibly expanded further).
	  * All `Join` subclasses and `From` have this type equal to the `Outer` type of their left side, but `Subselect`
	  * defines `Outer` as the `Self` type of its left side. This means that, for any type `S <: RowProduct`
	  * with fully instantiated parameters (the clause is ''concrete'', that is the concrete types of all joins in it
	  * are known), value `(s :S) subselect t1 join t2 ... join t3` conforms to `s.DirectSubselect`
	  * and `RowProduct { type Outer = s.Self }` if none of the joins following the subselect element are
	  * an [[net.noresttherein.oldsql.sql.ParamClause ParamClause]]. This way one can statically express a dependency
	  * relationship between ''from'' clauses without resorting to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.outer]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Inner]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Self]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf]]
	  */
	type Outer <: Implicit {
		type Generalized = thisClause.Implicit
		type FullRow     = thisClause.OuterRow
	}

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselect()`. When viewed as a list of relations, `outer` constitutes
	  * the result of dropping all relations joined in this instance up until and including a relation joined
	  * by the `.subselect()` call, going from right to left. If there's no `Subselect` in the dynamic type definition
	  * of this clause, meaning this is not a subselect clause (the actual ''from'' clause of resulting selects
	  * will include all members of this clause), `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or `left.Self` if this instance is a `Subselect`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Outer]]
	  */
	val outer :Outer

	/** The `RowProduct` type used as the type parameter of [[net.noresttherein.oldsql.sql.ast.SelectSQL select]] SQL
	  * expressions created from this instance. It is equal to `Implicit`, unless
	  * a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] is present in the explicit portion of
	  * this clause, in which case it equals `Nothing` (making any expression grounded in this clause unusable).
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  */ //todo: not defined as DefineBase[Implicit] here because of a clash with SubselectOf refinement caused by a scala bug
	type Base <: DefineBase[Implicit] //consider: renaming to Grounding /Closure, Dependencies, Ground or Root

	/** A helper type used to define the type `Base`. Always parameterized with `Implicit`, all `Expanded` subclasses
	  * define it as equal to the argument `I`, except of `ParamClause`, which defines it as `Nothing`.
	  * This is better than defining `Base` directly, as it allows to propagate behavior of the explicit portion
	  * (which determines whether `Base` will equal `Implicit` or `Nothing`) when rebasing it onto another
	  * clause (and hence changing the actual `Implicit` type).
	  */ //consider: using Implicit is upper and lower bound on I
	type DefineBase[+I <: RowProduct] <: I

	/** The implicit prefix of this clause, that is the clause ending before the last
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] join, or [[net.noresttherein.oldsql.sql.Dual Dual]]
	  * for non subselect clauses.
	  * @return `this.outer`
	  * @throws UnsupportedOperationException if there is a [[net.noresttherein.oldsql.sql.ParamClause ParamClause]]
	  *                                       join after the last `Subselect` in this clause.
	  */
	def base :Base



	/** Number of relations contained in the explicit portion of this clause, excluding any relations under grouping
	  * of a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause. This is equal to the number of mappings
	  * to the right of the rightmost [[net.noresttherein.oldsql.sql.Subselect Subselect]] join,
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] (including its presence in [[net.noresttherein.oldsql.sql.From From]]),
	  * or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] (if present), that is
	  * [[net.noresttherein.oldsql.sql.RowProduct.fullSize]]` - `[[net.noresttherein.oldsql.sql.RowProduct.outer outer]]`.fullSize`.
	  * It is the number of elements in `this.`[[net.noresttherein.oldsql.sql.RowProduct.tableStack tableStack]].
	  */
	def size :Int = fullSize - outer.fullSize

	/** The number of relations contained in the explicit portion of this clause. It is the same as
	  * `this.`[[net.noresttherein.oldsql.sql.RowProduct.size size]], but stresses the fact that the number does not
	  * include the relations from the enclosing selects.
	  * @return `this.`[[net.noresttherein.oldsql.sql.RowProduct.size size]].
	  */
	final def innerSize :Int = size



	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that its 'Implicit' type is the generalization of the prefix clause
	  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
	  * it represents the full `Generalized` type, with all its relations.
	  * This is the same value and relation as in [[net.noresttherein.oldsql.sql.RowProduct.innerSpan innerSpan]],
	  * but expressed in terms of the generalized forms of this clause and its outer clause.
	  */
	@inline final def explicitSpan :Implicit ExpandedBy Generalized = new ExpandedBy(size)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that its 'Outer' type is the generalization of the prefix clause
	  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
	  * it represents the full type `Self`, with all its relations.
	  * This is the same value and relation as in [[net.noresttherein.oldsql.sql.RowProduct.explicitSpan explicitSpan]],
	  * but expressed in terms of the self types of this clause and its outer clause.
	  */
	@inline final def innerSpan :Outer ExpandedBy Self = new ExpandedBy(size)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that [[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf this.AsSubselectOf]]`[F]`
	  * joins all these relations to the clause `F` with a `Subselect` 'join'.
	  * This is the same value and relation as [[net.noresttherein.oldsql.sql.RowProduct.explicitSpan explicitSpan]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.innerSpan innerSpan]], but expressed in the form of the
	  * `AsSubselectOf` type constructor.
	  */
	@inline final def subselectSpan[F <: NonEmptyRow] :F ExpandedBy AsSubselectOf[F] = new ExpandedBy(size)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that its 'Implicit' base clause, constituting of all relations listed by
	  * enclosing selects, is a prefix of the `Generalized` type of this clause. For non-subselect clauses,
	  * it represents the full `Generalized` type size (that is, all relations in the clause).
	  * This is the same value and relation as in [[net.noresttherein.oldsql.sql.RowProduct.innerSuffix innerSuffix]],
	  * but expressed in terms of the generalized forms of this clause and its outer clause.
	  */
	@inline final def explicitSuffix :Implicit PrefixOf Generalized = new PrefixOf(size)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that its 'Outer' portion is a prefix clause of its `Self` type (that is,
	  * its complete type with all join kinds known). For non-subselect clauses, it is the number of all relations.
	  * This is the same value and relation as in [[net.noresttherein.oldsql.sql.RowProduct.explicitSuffix explicitSuffix]],
	  * but expressed in terms of the self types of this clause and its outer clause.
	  */
	@inline final def innerSuffix :Outer PrefixOf Self = new PrefixOf(size)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.size`,
	  * as a witness to the fact that [[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf this.AsSubselectOf]]`[F]`
	  * joins all these relations to the clause `F` with a `Subselect` 'join'.
	  * This is the same value and relation as [[net.noresttherein.oldsql.sql.RowProduct.explicitSuffix explicitSuffix]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.innerSuffix innerSuffix]], but expressed in the form of the
	  * `AsSubselectOf` type constructor.
	  */
	@inline final def subselectSuffix[F <: NonEmptyRow] :F PrefixOf AsSubselectOf[F] = new PrefixOf(size)



	/** Subject types of all non-parameter mappings in this clause, following
	  * the [[net.noresttherein.oldsql.sql.RowProduct.outer implicit]] prefix and excluding unbound parameters
	  * (synthetic relations joined using [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins),
	  * concatenated into a [[net.noresttherein.oldsql.collection.Chain Chain]].
	  * For a [[net.noresttherein.oldsql.sql.FromClause FromClause]], this amounts to the list of mapping subjects
	  * from all [[net.noresttherein.oldsql.sql.ast.JoinedTable JoinedTable]]s from the ''from'' clause
	  * of the most deeply nested subselect, that is to the right of [[net.noresttherein.oldsql.sql.From From]]
	  * or the last subselect [[net.noresttherein.oldsql.sql.Subselect Subselect]].
	  * For an [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]], these are subjects of all mappings
	  * since the last [[net.noresttherein.oldsql.sql.GroupByClause]]
	  * (and empty for [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]).
	  * This way the chain contains all mappings which are available to the ''select'' clause created from this instance,
	  * with the exception of unbound parameters.
	  *
	  * If this clause doesn't represent a subselect, but a top-level query, it is the same as
	  * [[net.noresttherein.oldsql.sql.RowProduct.FullRow FullRow]].
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition and is,
	  * like [[net.noresttherein.oldsql.sql.Expanded Expanded]] (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.row]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.FullRow]]
	  */
	type Row <: Chain

	/** Create an SQL tuple expression containing [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * expressions for all joined relations in the most deeply nested subselect, excluding unbound parameters
	  * (synthetic relations joined using [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins),
	  * in their order of appearance. This includes all relations in this clause following the most recent
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] or
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]] 'join',
	  * marking the first not grouped relation following the [[net.noresttherein.oldsql.sql.RowProduct.outer implicit]]
	  * clause prefix. It represents the complete data set available to the ''select'' clause with the exception
	  * of potential grouped relations available only to aggregate functions.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.fullRow]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  */
	def row :ChainTuple[Generalized, Single, Row] = row(generalized)

	/** Create an SQL tuple expression containing [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]
	  * expressions for all joined relations in the most deeply nested subselect, excluding unbound parameters
	  * (synthetic relations joined using [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] pseudo joins),
	  * in their order of appearance. This includes all relations in this clause following the most recent
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] or
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]] 'join',
	  * marking the first not grouped relation following the [[net.noresttherein.oldsql.sql.RowProduct.outer implicit]]
	  * clause prefix. It represents the complete data set available to the ''select'' clause with the exception
	  * of potential grouped relations available only to aggregate functions. The expressions are based on
	  * some expanding clause `E`, so they can be used by the zero-argument `row` as the chain prefix of its result.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def row[E <: RowProduct]
	       (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, Row]

	/** All relations in the ''explicit'' section of this instance as generic, untyped mappings, in the reverse order
	  * of their appearance, ending with the first not grouped relation following
	  * the [[net.noresttherein.oldsql.sql.RowProduct.outer implicit]] prefix. It is the one joined
	  * with the last [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join', unless it is followed
	  * (possibly indirectly) by a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause (before any other `Subselect`),
	  * in which case the last relation is the one for the component following
	  * the last [[net.noresttherein.oldsql.sql.By By]] grouping expression following `GroupBy`.
	  * If this is not a subselect clause (no `Subselect` 'joins' are present in this clause
	  * and `Implicit =:= RowProduct`), all not grouped relations are included.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.row]]
	  */
	def tableStack :Seq[RelationSQL.from[Generalized]#__] = tableStack[Generalized](generalized)

	/** All relations in the ''explicit'' section of this instance as generic, untyped mappings, in the reverse order
	  * of their appearance, ending with the first not grouped relation following
	  * the [[net.noresttherein.oldsql.sql.RowProduct.outer implicit]] prefix. It is the one joined
	  * with the last [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join', unless it is followed
	  * (possibly indirectly) by a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause (before any other `Subselect`),
	  * in which case the last relation is the one for the component following
	  * the last [[net.noresttherein.oldsql.sql.By By]] grouping expression following `GroupBy`.
	  * If this is not a subselect clause (no `Subselect` 'joins' are present in this clause
	  * and `Implicit =:= RowProduct`), all not grouped relations are included. The returned relation SQL expressions
	  * are based on some expanding clause `E`. Used by the zero-argument `tableStack`
	  * to request the tail of the stack with expressions of the correct type from the prefix clause.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def tableStack[E <: RowProduct]
	              (target :E)(implicit expansion :Generalized ExpandedBy E) :LazyList[RelationSQL.from[E]#__]




	/** A [[net.noresttherein.oldsql.collection.Chain Chain]] with `Subject` types of mappings for all relations
	  * available to this clause implicitly, i.e., by virtue of being a part of a ''from'' clause of one of
	  * enclosing ''selects''. It is equal to `outer.`[[net.noresttherein.oldsql.sql.RowProduct.FullRow FullRow]].
	  */ //this type is needed to implement FullRow in GroupBy so that Self preserves it, as it does not outer.type
	type OuterRow <: Chain

	/** Create an SQL tuple expression containing all non-parameter `JoinedRelation` expressions from the outer,
	  * ''implicit'' section of this clause, that is all elements until the rightmost `Subselect` join, in their order
	  * of appearance. If this is not a subselect clause, the chain will be empty.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  */
	def outerRow :ChainTuple[Generalized, Single, OuterRow] = outerRow(generalized)(explicitSpan)

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all elements joined until
	  * the rightmost `Subselect` join, in their order of appearance. If this is not a subselect clause,
	  * the chain will be empty. The expressions are based on some expanding clause `E`, so they can be used
	  * by the zero-argument `outerRow` as the chain prefix of its result.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def outerRow[E <: RowProduct]
	            (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, Single, OuterRow]




	/** A type constructor appending all relations joined since the last (rightmost) `Subselect` 'join' (or `Dual`)
	  * to the given clause `F`. This is the result of replacing everything to the left of the last `Subselect`
	  * in this type's signature with `F`. For any concrete subselect clause `J <: FromSome` without
	  * any parameters in the explicit/inner section, `J =:= J#AsSubselectOf[J#Outer]`
	  * and `J#AsSubselectOf[J#Implicit] <:< J#Generalized`. For outer clauses, it replaces
	  * the first `From[T]` 'join' with `F Subselect T`, but `Dual`, despite already being a subselect clause
	  * of any clause, defines it as `Nothing` as an exception, because joining the result with additional relations
	  * would otherwise ''not'' become a subselect clause of `F`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.Implicit]]
	  */ //consider: renaming to InnerSelectedFrom analogous type for any join;
	type AsSubselectOf[+F <: NonEmptyRow] <: Inner {
		type Explicit = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
	}

	/** For subselect clauses - that is subtypes with a `Subselect` join kind occurring somewhere in their definition,
	  * not necessarily `Subselect` instances - it represents them as a subselect of a clause `F`, being
	  * an expansion of their outer clause (the left side of the right-most `Subselect` join). In syntactic terms,
	  * it replaces the `Outer` type in this type's definition with type `F`. Procedurally, it joins in order
	  * all relations since the last occurrence of a `Subselect` join, forming the explicit ''from'' clause of
	  * a modified subselect, with the new outer clause `F`, preserving all join conditions. If this clause is not a
	  * subselect clause, this method will use `Subselect` 'join' to join the `newOuter` with the first relation of this
	  * clause. All join conditions are preserved. As the argument clause is an expansion of the outer clause of
	  * this instance, this has the effect of inserting the relations appended to the outer clause between it and
	  * the inner clause. For outer clauses, this should produce the same result as
	  * [[net.noresttherein.oldsql.sql.RowProduct.selectedFrom selectedFrom]], though the exact types
	  * may be different when called for abstract types.
	  * @return a ''from'' clause which, assuming this instance doesn't contain any `UnboundParam` parameters
	  *         in its outer section, conforms to `newOuter.DirectSubselect` and, if this type is instantiated at least to
	  *         the last `Subselect` (or `Dual`/`From`) and its generalized form is known,
	  *         to [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]`.
	  * @throws `UnsupportedOperationException` if this clause is empty or there is an `UnboundParam` 'join'
	  *                                         in its explicit portion.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */
	def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self }




	/** A ''from'' clause for a subselect of this clause with a single relation `T`, conforming to `SubselectOf[Self]`. */
	type FromTable[T[O] <: MappingAt[O]] <: Self SelectFrom T

	protected[sql] type SingletonFromTable[T[O] <: MappingAt[O]] <: this.type SelectFrom T
	protected[sql] type GeneralizedFromTable[T[O] <: MappingAt[O]] <: Generalized NonParam T

	protected[sql] def from[T[O] <: BaseMapping[S, O], S, A <: Label]
	                       (right :LastTable[T, S], alias :Option[A],
	                        filter :SingleBoolean[GeneralizedFromTable[T]]) :SingletonFromTable[T] As A

	/** Creates a ''from'' clause for a subselect of a select with this clause. It will have the given relation
	  * as the only member of the ''explicit'' portion (i.e. the one actually appearing in the ''from'' clause
	  * of the generated SQL) and this clause as the ''implicit'' prefix,
	  * joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. All relations included
	  * in this clause are available to any `SQLExpression` parameterized with the type of the returned clause.
	  * The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns `From(subselect)`. All non subselect clauses conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf[RowProduct] ]] so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way, as long as its `Generalized` type is known.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.from from]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] expansion method instead,
	  * which has a friendlier return type, but is available only for non-empty clauses.
	  * @param first the sole relation of the ''from'' clause of the new subselect clause.
	  * @param infer implicit witness guiding the compiler to properly infer the subject type of mapping `M` (and `T`).
	  * @return `Self Subselect T`, or `From[T]` if this clause is empty.
	  *        The clause will conform to `this.DirectSubselect` and, as long as it is complete and its generalized form is known,
	  *        to `SubselectOf[Generalized]`.
	  */ //consider: renaming to selectFrom
	def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (first :Table[M])(implicit infer :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:FromTable[T] //uses InferTypeParams instead of JoinedRelationSubject because the result type might be a Subselect or From

	/** Creates a ''from'' clause for a subselect of a select with this clause. It will have the given relation
	  * as the only member of the ''explicit'' portion (i.e. the one actually appearing in the ''from'' clause
	  * of the generated SQL) and this clause as the ''implicit'' prefix,
	  * joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. All relations included
	  * in this clause are available to any `SQLExpression` parameterized with the type of the returned clause.
	  * The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns `From(subselect)`. All non subselect clauses conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf[RowProduct] ]] so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way, as long as its `Generalized` type is known.
	  *
	  * This overloaded method variant accepts a table tagged with a `String` literal type with its name, which
	  * is used for the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause of the returned object.
	  * You may prefer to use [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] extension
	  * method instead, which has a friendlier return type, but is available only for non-empty clauses.
	  * @param first the sole relation of the ''from'' clause of the new subselect clause.
	  * @param infer implicit witness guiding the compiler to properly infer the subject type of mapping `M` (and `T`).
	  * @return An object equal to `from(first :Table[M]) as first.name`, but of correct narrowed type.
	  */
	def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
	        (first :StaticTable[A, M])
	        (implicit infer :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
			:FromTable[T] As A

	/** Type resulting from replacing the `Outer`/`Implicit` part of `F` with `Self` type of this clause.
	  * It is defined in terms of `F#`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]],
	  * with the exception of empty clauses, which define it simply as the right side.
	  */
	type FromSubselect[+F <: NonEmptyRow] <: NonEmptyRow {
		type Explicit <: F#Explicit
		type Implicit = thisClause.Generalized
		type Row <: F#Row
	}

	/** Creates a ''from'' clause for a subselect of a select with this clause. It uses the given clause `subselect`
	  * as its ''explicit'' suffix (i.e. the relations from the actual ''from'' clause of the associated subselect),
	  * and this clause as the ''implicit'' prefix, joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * pseudo join. The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns the `subselect` argument unchanged. All non-subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf[RowProduct] ]] (either statically,
	  * or by the wildcard [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]] refinement,
	  * so the returned clause is a valid subselect clause of `Generalized` (and `Self`) either way.
	  *
	  * Note that a clause `F <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]] is considered
	  * a subselect clause of any other clause `E`,
	  * that is `GroundRow <: `[[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[E]`.
	  * It means this method is not necessary to create a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]
	  * expression valid for use with this clause, and it's useful only if the result set of the dependent select
	  * should be further narrowed by a filter expression depending on the rows of this clause.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.from from]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] extension method instead,
	  * which accepts any non-empty clause, not only
	  * [[net.noresttherein.oldsql.sql.FromSome.GroundFromSome GroundFromSome]] subtypes.
	  * @param subselect the actual, explicit ''from'' clause of a target subselect - without any `Subselect`
	  *                  or `UnboundParam` joins of its own.
	  * @return a ''from'' clause adapted from the `subselect` argument by prepending this clause to it.
	  *         It will conform to `this.DirectSubselect` and, as long as this clause is ''generalized'',
	  *         to `SubselectOf[Generalized]`.
	  */ //does not use GroundFromSome as an upper bound because F might be an aggregated clause.
	def from[F <: NonEmptyRow with GroundRow](subselect :F) :FromSubselect[F] { type DefineBase[+I <: RowProduct] = I }

	/** Creates a ''from'' clause for a subselect of a select with this clause. It uses the ''explicit''/''inner''
	  * portion of the given `subselect` clause as its ''explicit'' suffix (i.e. all the relations
	  * from the actual ''from'' clause of the associated subselect, be they statically known or not), substituting
	  * its ''implicit'' prefix with this clause, joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * pseudo join. It can only be applied to clauses whose an implicit, outer portion is expanded by this clause,
	  * that is if the latter contains all the relations available implicitly to the `subselect` clause as its prefix
	  * in the exact order and, presumably, was created by expanding the very `subselect.outer` instance.
	  * Compare this with method [[net.noresttherein.oldsql.sql.RowProduct.from from]], which joins
	  * a whole independent clause as the ''explicit/inner'' portion of a the resulting subselect clause.
	  *
	  * On non-empty clauses, it delegates
	  * to `subselect.`[[net.noresttherein.oldsql.sql.RowProduct.asSubselectOf asSubselectOf]].
	  * On empty clauses it works differently: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns the `subselect` argument unchanged. All non-subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf[Dual]]], so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way, as long as this clause is ''complete''.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.fromSubselect fromSubselect]] method of
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]].
	  * @param subselect the actual, explicit ''from'' clause of a target subselect.
	  * @return a ''from'' clause adapted from the `subselect` argument by replacing its `Outer` part with this clause,
	  *         conforming to `outer.DirectSubselect` and `SubselectOf[Generalized]` (if this clause's `Generalized` type
	  *         is well defined).
	  */
	//Todo: allow any clause and use From instead of Subselect for an EmptyRow and SelectFrom if F <: Aggregated
	//Lack of such polymorphism hurts SubselectSQL implementations.
	def fromSubselect[F <: NonEmptyRow](subselect :F)(implicit expansion :subselect.Implicit ExpandedBy Generalized)
			:FromSubselect[F] { type DefineBase[+I <: RowProduct] = subselect.DefineBase[I] }



	/** The collection of all [[net.noresttherein.oldsql.sql.CommonTableExpression CommonTableExpression]]
	  * named derived tables used by any dependent ''select'' expressions used by any SQL ''select''
	  * based on this clause - in ''where''/''having'' (i.e, in [[net.noresttherein.oldsql.sql.RowProduct.filter filter]]
	  * and `this.fromClause.filter`), or in ''from''/''group by'' (i.e, by any relations
	  * in [[net.noresttherein.oldsql.sql.RowProduct.tableStack tableStack]] or
	  * `this.`[[net.noresttherein.oldsql.sql.RowProduct.fromClause fromClause]]`.tableStack`) clauses of this instance.
	  * These named ''selects'' are collected together to form a single ''with'' clause of the generated
	  * complete SQL query using this instance, with each derived table receiving a single, unique declaration,
	  * rather than being repeated with every subselect of this instance using them. Note that this only covers
	  * the [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]] span of this instance, ignoring any
	  * expressions in the [[net.noresttherein.oldsql.sql.RowProduct.Implicit implicit]] tables inherited from
	  * outer clauses.
	  */
	def withClause :WithClause


	/** Number of JDBC parameters in the SQL for this clause. This counts all individual columns
	  * of both embedded bound and unbound parameters in all SQL clauses corresponding to this instance:
	  * ''from'' (in case of ''table expressions'', that is using an inlined ''select''), ''group by''
	  * ([[net.noresttherein.oldsql.sql.SQLExpression.sqlParamCount sqlParamCount]] of the grouping expression),
	  * and ''where''/''having'' ([[net.noresttherein.oldsql.sql.Adjoin.condition condition]] properties of all joins
	  * and join-like classes). It should correspond to the number of '?' JDBC placeholders in the formatted SQL
	  * returned by [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]].
	  */
	protected def sqlParamCount(implicit spelling :SQLSpelling) :Int
	private[oldsql] final def `->sqlParamCount`(spelling :SQLSpelling) :Int = sqlParamCount(spelling)


	/** Creates SQL for the full ''from'' and ''where'' clauses of this instance (as well as ''group by''
	  * and ''having'', if present). The returned SQL fragment can be than used to create the SQL for a full ''select''
	  * expression based on this clause. It is implemented on a high level by abstract `RowProduct` subtypes
	  * and concrete implementations will typically define only
	  * [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling defaultSpelling]].
	  */
	def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	            (implicit spelling :SQLSpelling) :SpelledSQL[P]

	/** A fallback method creating full, standard SQL for this instance invoked by
	  * [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]] (and the higher-level
	  * [[net.noresttherein.oldsql.sql.RowProduct.spell spell]] method) if the given DBMS type does not require
	  * non-standard representation. It concerns itself only with the actual ''from'' clause (including any join
	  * conditions in ''on'' clauses), without implicit tables from outer clauses, and optional ''group by'' clause
	  * with all its expressions, but not ''where'' and ''having'' clauses. This is because it is a recursion step
	  * assuming that ''where'' and ''having'' clauses will be appended when recursion
	  * for the whole instance/its ''from'' clause ends. Clients of this class, if they indeed require specifically
	  * the whole SQL fragment for this instance, should use [[net.noresttherein.oldsql.sql.RowProduct.spell spell]].
 	  * @param context  the namespace of the greater ''select'' using this ''from'' clause, including aliases for all
	  *                 tables inherited from [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] ''selects''.
	  *                 The indexing of the aliases is consistent with the indexing of relations in
	  *                 [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] clause. In particular, expressions
	  *                 from the [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]]s
	  *                 in the ''group by'' clause of the outer ''select'' will be included in the indexing
	  *                 (although without any aliases), but the tables grouped by a ''group by'' clause or
	  *                 an [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] decorator will be not.
	  * @param spelling a strategy combining the knowledge of proper SQL dialect, SQL formatting preferences and used
	  *                 table aliases.
	  * @return an SQL string with [[net.noresttherein.oldsql.sql.RowProduct.parameterization parameterization]],
	  *         including setters for all bound and unbound parameters of SQL subexpressions present in this instance
	  *         and [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext SQLContext]] with aliases of
	  *         all tables in this instance, indexed consistently with the indexing of
	  *         [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] declared by it.
	  *         The `SQLContext` of the returned SQL will reflect this instance: subselect clauses will expand
	  *         the context given as the argument here, while ''top'' clauses (without
	  *         a [[net.noresttherein.oldsql.sql.Subselect Subselect]] subcomponent) will build up on a fresh instance.
	  */
	protected def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                (implicit spelling :SQLSpelling) :SpelledSQL[P]

	private[sql] final def defaultSpelling[P](spelling :SQLSpelling)
	                                         (context :SQLContext[P], params :Parameterization[P, Generalized])
			:SpelledSQL[P] =
		defaultSpelling(context, params)(spelling)

	/** The spelling context with the aliases of all referencable relations in this instance with the same table aliases
	  * as in the context included in the [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL SpelledSQL]] returned
	  * by method [[net.noresttherein.oldsql.sql.RowProduct.defaultSpelling(context:SQLContext) defaultSpelling]]
	  * (when given an empty initial context). The difference is that this instance doesn't include conditions
	  * for the ''where'' clause
	  * (its [[net.noresttherein.oldsql.sql.mechanics.SpelledSQL.SQLContext.whereClause whereClause]] property
	  * is empty). The two methods should remain otherwise consistent, but the latter typically
	  * builds its context concurrently with the SQL fragment, not relying on this method. In fact, ensuring
	  * that the returned contexts are the same may require this method to be implemented in terms of `defaultSpelling`,
	  * which can incur unexpected computational cost. For this reason the other method should be preferred for
	  * spelling SQL ''select'' statements, and currently this method is used only in spelling DML and call statements.
	  *
	  * Note that [[net.noresttherein.oldsql.sql.AggregateClause aggregated]]/[[net.noresttherein.oldsql.sql.GroupByClause grouped]]
	  * instances do not include aliases for the grouping expressions and the aliases from the actual ''from'' clause
	  * are available as if no ''group by'' clause was present, with only the total number of grouping 'relations'
	  * introduced by [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] and [[net.noresttherein.oldsql.sql.By By]]
	  * is provided.
	  */
	def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params]

	/** A provider of accessor functions returning the values of the appropriate parameter in
	  * [[net.noresttherein.oldsql.sql.RowProduct.Params this.Params]] chain based on
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] expressions of the associated
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] clause. It is used to provide write
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm forms]] setting the values of the parameters of SQL
	  * [[net.noresttherein.oldsql.sql.Incantation commands]] based on this ''from'' clause.
	  * It is used in the process of [[net.noresttherein.oldsql.sql.RowProduct.spell spelling]] the SQL
	  * for this instance as well as any ''selects'' based on it. The returned instance is empty - it does not contain
	  * forms for setting any parameters present in ''where''/''having'' clauses in this instance and serves
	  * only as the facade to present unbound parameters.
	  *
	  * Implementation may not depend on `this.`[[net.noresttherein.oldsql.sql.RowProduct.spellingContext spellingContext]]
	  * or infinite recursion will occur.
	  */
	def parameterization :Parameterization[Params, Self]

	/** Drops the last `position` relations (counting as with their indices) from this instance and
	  * returns the prefix clause - which must be a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] or
	  * [[net.noresttherein.oldsql.sql.By By]] clause - together with its last
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupingRelation GroupingRelation]], and
	  * `context` and `params` adjusted by similarly dropping the suffix of this clause.
	  * The result thus combines type-compatible argument set to pass to one of grouping relation's spelling methods.
	  */
	protected def groupingSpellingContext[P]
	              (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P]

	/** A forwarder to `from.groupingSpellingContext(position, context, params)`. */
	protected final def groupingSpellingContext[P](from :RowProduct)
	                                              (position :Int, context :SQLContext[P],
	                                               params :Parameterization[P, from.Generalized])
			:GroupingSpellingContext[P] =
		if (position < 0)
			throw new IndexOutOfBoundsException(
				"Cannot return GroupingSpellingContext for relation #" + position + " in " + from + "."
			)
		else
			from.groupingSpellingContext(position, context, params)

	private[sql] final def `->groupingSpellingContext`[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
	        :GroupingSpellingContext[P] =
		groupingSpellingContext(this)(position, context, params)



	protected def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.rowProduct(this)

	private[sql] final def `->visit`[Y](visitor :RowProductVisitor[Y]) :Y = visit(visitor)


	/** Extension of the [[net.noresttherein.oldsql.sql.SQLExpression.homomorphic homomorphism]]
	  * for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
	  * (and [[net.noresttherein.oldsql.schema.Mapping Mapping]]) to `RowProduct` type hierarchy.
	  * Intuitively, two clauses being homomorphic imply that any SQL expressions built by the same, non branching code,
	  * based on them are homomorphic. In particular, it implies that all expressions used as any part
	  * of the two clauses are homomorphic, too.
	  *
	  * It is defined as having the same `Generalized` form
	  * (using [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedClass generalizedClass]] as a proxy),
	  * and any of their subclauses ([[net.noresttherein.oldsql.sql.Adjoin.left left]]
	  * for [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] and [[net.noresttherein.oldsql.sql.DecoratedRow.clause clause]]
	  * for [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]]) and all their subexpressions
	  * (in particular, the [[net.noresttherein.oldsql.sql.RowProduct.last last]] relation
	  * and [[net.noresttherein.oldsql.sql.RowProduct.filter filter]]) being homomorphic.
	  *
	  * While rarely used on its own, it takes part in determining whether an `SQLExpression`
	  * can be [[net.noresttherein.oldsql.sql.SQLExpression.anchor anchored]] in another clause,
	  * which is paramount to their reuse. If both clauses consist of the same join kinds and the same tables,
	  * they will always be homomorphic, even if the tables have been
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter altered]] by
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.include including]] or
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.exclude excluding]] certain columns.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.homomorphic]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.homomorphic]]
	  */
	def homomorphic(that :RowProduct) :Boolean //todo: an ambitious implementation would ignore join order, but then SQLExpression homomorphism (or smth) would have to use index mapping.

	/** Extension of the [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic isomorphism]]
	  * for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
	  * (and [[net.noresttherein.oldsql.schema.Mapping Mapping]]) to `RowProduct` type hierarchy.
	  * Intuitively, two clauses being isomorphic imply that any SQL expressions built by the same, non branching code,
	  * based on them are isomorphic. In particular, it implies that all expressions used as any part
	  * of the two clauses are isomorphic, too.
	  *
	  * It is defined as having the same `Generalized` form
	  * (using [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedClass generalizedClass]] as a proxy),
	  * and any of their subclauses ([[net.noresttherein.oldsql.sql.Adjoin.left left]]
	  * for [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] and [[net.noresttherein.oldsql.sql.DecoratedRow.clause clause]]
	  * for [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]]) and all their subexpressions
	  * (in particular, the [[net.noresttherein.oldsql.sql.RowProduct.last last]] relation
	  * and [[net.noresttherein.oldsql.sql.RowProduct.filter filter]]) being isomorphic.
	  *
	  * While rarely used on its own, it takes part in determining the extent to which two `SQLExpression` instances
	  * built based on different clauses/[[net.noresttherein.oldsql.schema.Relation relations]] are interchangeable
	  * or compatible, which is required in various situations encountered
	  * when [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling spelling]], for example.
	  * If both clauses consist of the same join kinds and the same tables,
	  * they will always be isomorphic, even if the tables have been
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.alter altered]] by
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.include including]] or
	  * [[net.noresttherein.oldsql.schema.Relation.RelationTemplate.exclude excluding]] certain columns.
	  *
	  * Isomorphism implies [[net.noresttherein.oldsql.sql.RowProduct.homomorphic homomorphism]].
	  * @see [[net.noresttherein.oldsql.schema.Mapping.isomorphic]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic]]
	  */
	def isomorphic(that :RowProduct) :Boolean



	/** Implements the check for structural equality ('deep equality') between the two clauses.
	  * Standard `equals` implementation is defined in terms of equality of included
	  * [[net.noresttherein.oldsql.schema.Relation relations]], which, in turn, are based on ''referential'' equality
	  * of their row [[net.noresttherein.oldsql.schema.Mapping mappings]]. It is the stringiest check available,
	  * ensuring full compatibility (as interchangeability of parts) between the two relations
	  * and any SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]. However, it seriously limits reusability,
	  * as the actual interoperability of `SQLExpression` implementations goes farther; for example, a deserialized
	  * `RowProduct` will - bar singletons such as [[net.noresttherein.oldsql.sql.Dual Dual]] - not equal itself
	  * before serialization, but the ''where'' condition of one will, in most cases, be possible to use with another.
	  *
	  * It is recursively defined as any of subclauses of the two clauses
	  * ([[net.noresttherein.oldsql.sql.Adjoin.left left]] for [[net.noresttherein.oldsql.sql.Adjoin Adjoin]]
	  * and [[net.noresttherein.oldsql.sql.DecoratedRow.clause clause]]
	  * for [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]]), and all their subexpressions
	  * (in particular, the [[net.noresttherein.oldsql.sql.RowProduct.last last]] relation
	  * and [[net.noresttherein.oldsql.sql.RowProduct.filter filter]]), being identical, as per their respective methods.
	  * In addition, in order to remain, as a mathematical relation, a strict superset of the equality relation,
	  * all subclauses in the recursion require mutual [[net.noresttherein.oldsql.sql.RowProduct.canEqual canEqual]]
	  * checks to pass.
	  *
	  * It is an extension of the [[net.noresttherein.oldsql.sql.SQLExpression.isomorphic isomorphism]]
	  * for [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
	  * (and [[net.noresttherein.oldsql.schema.Mapping Mapping]]) to `RowProduct` type hierarchy.
	  * Intuitively, two clauses being identical imply that any code not performing any referential equality checks
	  * will always yield the same results for both.
	  *
	  * Being equal implies being identical, and being identical
	  * implies being  [[net.noresttherein.oldsql.sql.RowProduct.isomorphic isomorphic]].
	  * @see [[net.noresttherein.oldsql.schema.Mapping.isomorphic]]
	  * @see [[net.noresttherein.oldsql.sql.SQLExpression.identical]]
	  */
	def identical(that :RowProduct) :Boolean

	def canEqual(that :Any) :Boolean = that.isInstanceOf[RowProduct]

	//todo: toInnerString
	//todo: expand all tables to the complete clause so they have proper indices
	/** The implementation for `toString` avoiding O(n^2^) `String` concatenation cost by using a list-like buffer. */
	def chunkedString :ChunkedString

	/** A `String` representation which omits ''where'' and ''having'' clauses, as well as
	  * the exact grouping expressions in ''group by'' clauses. It lists only the mappings and 'join' types.
	  */
	def typeString :String = typeString(new StringBuilder).toString

	def typeString(res :StringBuilder) :StringBuilder

	override def toString :String = chunkedString.toString

	private[sql] def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause(seal :Seal) :Unit
}






/** A companion object for the [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] trait representing
  * somewhat generalized ''from'' clauses of SQL select statements. It serves primarily as a namespace
  * for related classes and implicit values.
  */
object RowProduct {
	//todo: search and replace all references in the project to `&lt;` inside `` with `<:`
	/** A factory interface mixed into [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] classes,
	  * providing a covariant upper bound of `F` on produced `RowProduct` instances. Each trait or class
	  * `F &lt;: RowProduct` will typically extend also `RowProductTemplate[F]` (or its subtype).
	  * It works in the vein of `IterableOps` from the standard Scala library, except only few `RowProduct` subclasses
	  * have a corresponding template trait extending this trait, as adding
	  * an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause to a type `F` will downtype only
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]] to `F As A`.
	  * There is a certain dualism in `RowProduct` methods, with some being defined in terms of this upper bound `F`,
	  * while others working on (and returning) values of member types
	  * (such as [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]]). It is necessary because
	  * more complex operations on abstract `RowProduct` types can only be defined in terms of member types,
	  * which retain the relationships between them, but abstract types can easily fool the type inferer and client code
	  * in most use cases would rather have the result types inferred as the most specific concrete upper bound
	  * of the former (concrete here means that it is not a bound/existential type, rather than non-abstract class, i.e.
	  * it can contain wildcard ''from'' clause as a prefix). For instantiated types, it should always be preferred
	  * over member types such as `Copy`, which exist to allow declaring abstract types simply as `F <: RowProduct`
	  * (or `FromSome`, `GroupByClause`, etc.), instead of `F <: RowProduct with RowProductTemplate[F]`,
	  * making for much cleaner and shorter signatures.
	  */ //todo: rename to ClauseTemplate
	trait RowProductTemplate[+F <: RowProduct] { thisClause :F with RowProductTemplate[F] =>
		//todo: in Scala 3, use F as an additional upper bound on Self; then filtered could return Self and Copy wouldn't be needed.
		/** The supertype of this clause used by some copy constructors/factory methods declared here and in
		  * the subclasses. It is defined as the most specialized supertype type of `f :F` which is not its
		  * singleton type. The difference from the other self type [[net.noresttherein.oldsql.sql.RowProduct.Self Self]]
		  * is that this type is ''not'' formed recursively, with the narrowing in subclasses being restricted only
		  * to the outer type constructor, retaining the same type parameters. For this reason, this type generally
		  * doesn't receive a concrete definition in public classes, as covariance in type parameters restricts it
		  * to remain upper bound instead; it rarely receives an override in derived public classes at all.
		  * Thus, for `join :InnerJoin[L, R]`: `join.Copy &lt;: L InnerJoin R` and
		  * `join.Self =:= join.left.Self InnerJoin R`. The primary advantage over `Self` is that
		  * (`F `''SomeJoin''` T)#Copy &lt;: F `''SomeJoin''` T`. This type should generally be used only in generic
		  * contexts, that is for `f :F` if `F` is an wildcard type such as `RowProduct`, `FromSome`, `GroupByClause`, etc..
		  * Methods declared here which return values of this type are considered of lower level, and
		  * when at least the last join of `F` is known (and thus `F &lt;: RowProductTemplate[U]` for some known type `U`),
		  * the more aptly named methods such as [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]],
		  * should always be preferred, as they will return a concrete type `U`, rather than some `_ <: U`
		  * (because only the upper bound on `Copy` is known). This reduces interface dependency, improves readability
		  * and, above all, type inference - in particular for recursive implicits over `U`.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct!.Self]]
		  */ //consider: getting rid of it and instead implement filtered in terms of Self
		type Copy <: F { //todo: Copy <: UpperBound; rename to Clone
			type LastMapping[O] = thisClause.LastMapping[O]
			type Last[C <: RowProduct] = thisClause.Last[C]
			type FromLast    = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Complete    = thisClause.Complete
			type NoAlias     = thisClause.NoAlias
			type Params      = thisClause.Params
			type FullRow     = thisClause.FullRow
			type Explicit    = thisClause.Explicit
			type Inner       = thisClause.Inner
			type Implicit    = thisClause.Implicit
			type Outer       = thisClause.Outer
			type Base        = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row         = thisClause.Row
			type OuterRow    = thisClause.OuterRow
		}
		@inline private[oldsql] final def selfAsCopy :Copy = this.asInstanceOf[Copy]

		//todo: add 'or'
		/** Adds a new filter expression for the `where`/`having` clause, joining it in logical conjunction
		  * with any preexisting filters. This is the lower level method than
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] methods, differing in two aspects:
		  *   - the `filter`'s argument scope type argument is the type parameter of this method, allowing it to
		  *     be loosened to [[net.noresttherein.oldsql.sql.SQLExpression.Grouped Grouped]] in
		  *     [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]];
		  *   - the return type is narrower, specified by the `Copy` member type. `Copy` allows the most precise
		  *     return type, the same as this clause, even if this clause is an abstract type at the point of invocation.
		  *
		  * While in most cases (especially where the whole type is statically known) `where`/`having`/`and` methods
		  * are both sufficient and more convenient, this one provides stronger typing. Compare:
		  * {{{
		  *     def where(clause :RowProduct) = clause.and(True)
		  *     def filtered(clause :RowProduct) = clause.filtered(True)
		  *     val abstractResult = where(From(Hamsters) join Rangers)     //:RowProduct
		  *     val specificResult = filtered(From(Hamsters) join Rangers)  //:From[Hamsters] InnerJoin Rangers
		  * }}}
		  * The downside however is that bound path dependent types fool implicit proofs, so methods relying on them
		  * may not work correctly for the returned value, unless it is specifically upcast to a non-PDT.
		  * All `where` methods delegate to this method.
		  */ //this method is extracted because GetTable implicits have problems with recognizing #Copy
		def filtered[S >: Single <: Single](filter :SQLBoolean[this.Generalized, S]) :Copy

		/** Creates a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  * instance's filter and the given `SQLBoolean`. The filter becomes the ''where'' clause of the SQL ''select''
		  * created from the returned instance. However, if this clause
		  * is a [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], the condition will be used
		  * in its ''having'' clause instead.
		  *
		  * This method exists to allow adding filters for any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]];
		  * methods [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.where where]]
		  * and [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]], defined
		  * for non aggregated and grouped clauses respectively, delegate to this method and are likely more readable.
		  * As an additional functionality, it is possible to follow a call to `where` or `having` with this method:
		  * {{{
		  *     val hamsterFilter  :SQLBoolean[From[Hamsters], GlobalScope] = ???
		  *     val squirrelFilter :SQLBoolean[RowProduct AndFrom Squirrels] = ???
		  *     val join = From(Hamsters) join Squirrels where hamsterFilter.expand(join) and squirrelFilter
		  * }}}
		  * @return [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.filtered filtered]]`(filter)`.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.and and]]
		  */
		def and(filter :SingleBoolean[this.Generalized]) :F = filtered(filter)

		/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
		  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement
		  * (if this instance conforms to [[net.noresttherein.oldsql.sql.FromClause FromClause]])
		  * or the ''having'' clause for instances of [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]].
		  * The function's argument provides access to the mapping of this instance's relations.
		  * Each is parameterized with an [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
		  * type `O >: Generalized <: RowProduct` and implicitly convertible to
		  * an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[O, GlobalScope, M[_]#Subject]`
		  * and thus usable in any `SQLExpression[Generalized, _, _]`, which will make any Boolean expression built
		  * from them conform to the function's required return type, providing no external expression dependent
		  * on other relations are used. It suffices to call any `SQLExpression` method such as one of the comparison
		  * methods like [[net.noresttherein.oldsql.sql.SQLExpression.==? ==?]].
		  *
		  * This method is the implementation of
		  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeTemplate.where where]] and
		  * [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate.having having]],
		  * defined for [[net.noresttherein.oldsql.sql.FromSome FromSome]] and
		  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], respectively,
		  * but can be called for any [[net.noresttherein.oldsql.sql.RowProduct RowProduct]].
		  * Additionally, it allows syntax like:
		  * {{{
		  *     From(Hamsters) join Squirrels where (!_[Squirrel].isFlying) and (t => t[Hamster].name === t[Squirrel].name)
		  * }}}
		  * @param condition a function which accepts a
		  *                  [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] instance
		  *                  functioning as a facade to this clause, providing easy access to all its relations,
		  *                  and which returns an SQL expression for the new join/filter condition.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */ //parameterized with F, not Self/Complete, as Self is a bound type unless all relations are aliased and Complete does not have aliases.
		def and(condition :JoinedMappings[F] => SingleBoolean[this.Generalized]) :F =
			and(condition(this.mappings))

		//these are here so that in FromSomeTemplate[U, U] with RowProductTemplate[U, F] they are both public
		// and returning F. This scenario occurs in U As A, where F =:= U As A
		//todo: overload and taking JoinedMappings[F] multiple times
		/** Creates a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  * instance's filter and the given `SQLBoolean`. This method is defined here as protected
		  * in order to be able to narrow down its return type to a clause `T <: F` by casting `F`
		  * to `F with RowProductTemplate[FromSome, T]`. It is overridden
		  * in [[net.noresttherein.oldsql.sql.FromSome.FromSomeTemplate FromSomeTemplate]] to be public.
		  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` narrows its left side by casting it to
		  * `F with `[[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]`[F, F As A]`.
		  * As a result, in `F As A`, this method is both public and has a return type of `F As A`, as expected.
		  * @return [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.and and]]`(filter)`.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where]]
		  */
		protected def where(filter :SingleBoolean[this.Generalized]) :F = and(filter)

		/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
		  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
		  * This method is defined here as protected in order to be able to narrow down its return type to a clause
		  * `T <: F` by casting `F` to `F with RowProductTemplate[FromSome, T]`. It is overridden
		  * in [[net.noresttherein.oldsql.sql.FromSome.FromSomeTemplate FromSomeTemplate]] to be public.
		  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` narrows its left side by casting it to
		  * `F with `[[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]`[F, F As A]`.
		  * As a result, in `F As A`, this method is both public and has a return type of `F As A`, as expected.
		  * @return a `RowProduct` of the same type as this one, but with its `filter` being the conjunction of this
		  *         instance's filter and the `SQLBoolean` returned by the function.
		  */ //parameterized with F, not Self, as Self is a bound type unless all relations are aliased.
		protected def where(condition :JoinedMappings[F] => SingleBoolean[this.Generalized]) :F =
			where(condition(this.mappings))

		//these are here so that in GroupByClauseTemplate[U, U] with RowProductTemplate[U, F] they are both public
		// and returning F. This scenario occurs in U As A, where F =:= U As A
		/** Throws `UnsupportedOperationException`. This method is defined here as protected
		  * in order to be able to narrow down its return type to a clause `T <: F` by casting `F`
		  * to `F with RowProductTemplate[FromSome, T]`. It is overridden
		  * in [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate GroupByClauseTemplate]] to be public.
		  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` narrows its left side by casting it to
		  * `F with `[[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]`[F, F As A]`.
		  * As a result, in `F As A`, this method is both public and has a return type of `F As A`, as expected.
		  */
		protected def having(condition :GroupedBoolean[this.Generalized]) :F =
			throw new UnsupportedOperationException(s"($this :${this.localClassName}).having")

		/** Throws `UnsupportedOperationException`. This method is defined here as protected
		  * in order to be able to narrow down its return type to a clause `T <: F` by casting `F`
		  * to `F with RowProductTemplate[FromSome, T]`. It is overridden
		  * in [[net.noresttherein.oldsql.sql.GroupByClause.GroupByClauseTemplate GroupByClauseTemplate]] to be public.
		  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` narrows its left side by casting it to
		  * `F with `[[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]`[F, F As A]`.
		  * As a result, in `F As A`, this method is both public and has a return type of `F As A`, as expected.
		  */
		protected def having(condition :JoinedMappings[F] => GroupedBoolean[this.Generalized]) :F =
			having(condition(this.mappings))


		def define(withClause :WithClause) :Copy = ???//todo: implement it. Remember that all where methods etc will need to copy it.

//		def withLocal(cte :CommonTableExpression.*) :F = withLocal(cte.withClause)
//		def withLocal(withClause :WithClause) :F = define(withClause)
//		def `with`(withClause :WithClause) :F = define(withClause)
//
//		def `with`(cte :CommonTableExpression[MappingAt]) :F = `with`(cte.withClause)
//
//		def `with`(cte :CommonTableExpressions[WithClauses] => CommonTableExpression[MappingAt])

		/** The actual ''from'' and ''where'' clauses in this instance. It is an instance in which
		  * [[net.noresttherein.oldsql.sql.RowProduct.row row]] and
		  * [[net.noresttherein.oldsql.sql.RowProduct.tableStack tableStack]] contain the complete list of relations
		  * which should occur in the ''from'' clause of any derived SQL ''select'', and
		  * the [[net.noresttherein.oldsql.sql.RowProduct.filter filter]] is the condition for the ''where'' clause.
		  * [[net.noresttherein.oldsql.sql.FromClause FromClause]] subtypes return simply themselves.
		  * [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]] subtypes return
		  * the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete prefix]] clause which is the left side
		  * of the last [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] occurrence in this type.
		  */
		def fromClause :FromClause
	}



	/** Marker trait for `RowProduct` implementations which contain a clause `F` as their part.
	  * All relations available from `F` are also available from this clause. Most implementations
	  * derive from this trait, with the exception of the [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]]
	  * and its subtypes. Every concrete class having this trait as its supertype must extend either
	  * [[net.noresttherein.oldsql.sql.Expanded Expanded]] or
	  * [[net.noresttherein.oldsql.sql.DecoratedRow.ExpandingDecorator]].
	  */
	trait ExpandingClause[+F <: RowProduct] extends RowProduct {
		private[sql] def concrete_ExpandingClause_subclass_must_extend_Expanded_or_ExpandedDecorator(seal :Seal) :Unit
	}
	//we can't have RowDecomposition at this level because we don't know the length of the expansion.
	object ExpandingClause {
		implicit def genericExpandedDecomposition[F <: RowProduct, J[+L <: F, R[O] <: M[O]] <: L Expanded R, M[O] <: MappingAt[O]]
				:RowDecomposition[F J M, F, F] { type E[+A <: F] = A J M; type S[+A >: F <: F] = A J M } =
			decomposition.asInstanceOf[
				RowDecomposition[F J M, F, F] { type E[+A <: F] = A J M; type S[+A >: F <: F] = A J M }
			]

		private[this] val decomposition =
			new ExpandedDecomposition[RowProduct Expanded MappingAt, RowProduct, MappingAt, Expanded, RowProduct]
	}



	/** Skeleton implementation of empty clauses. Currently extended only by [[net.noresttherein.oldsql.sql.Dual Dual]].
	  * It sets the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] type and several others
	  * to [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], which enforces its equivalence with `Dual`
	  * from the point of view of expressions based on this clause. It may however introduce additional information
	  * to the whole clause - an implementation could, for example, set some JDBC parameters (assuming pairing
	  * with a custom [[net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling SQLSpelling]]) or DBMS specific query hints.
	  * It cannot however add any information on type level.
	  */ //todo: currently it is not really used, we can make it more generic, so it can include param-only clauses and With.
	trait EmptyRow extends RowProduct with RowProductTemplate[EmptyRow] { thisClause =>
		override type LastMapping[O] = Nothing
		override type Last[-F <: RowProduct] = Nothing
		override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing
		override type FromLast    = RowProduct
		override type Generalized = RowProduct
//		override type Complete   >: NoAlias <: EmptyRow
//		override type NoAlias    <: EmptyRow
		override type Complete   <: EmptyRow {
			type Complete    = thisClause.Complete
			type LastParam   = thisClause.LastParam
			type Params      = thisClause.Params
		}
		override type NoAlias    <: Complete
		override type Self       <: NoAlias {
			type Inner = thisClause.Inner
		}
		override type Explicit    = RowProduct
		override type Implicit    = RowProduct
		override type Base        = RowProduct
		override type DefineBase[+I <: RowProduct] = I

		override def last :Nothing = throw new NoSuchElementException(s"$typeString.last")
		override def lastAsIn[E <: RowProduct](implicit expansion :RowProduct PrefixOf E) :Nothing = last
		override def generalizedClass :Class[RowProduct] = classOf[RowProduct]

		override type FilterNext[E[+L <: FromSome] <: RowProduct Expanded N,
		                         S <: RowProduct Expanded N, G <: S, N[O] <: MappingAt[O]] = Nothing

		override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
		              (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
			throw new UnsupportedOperationException(s"$typeString.filterNext($next)")

		override def size = 0

		override type FullRow = @~

		override def fullRow :ChainTuple[RowProduct, Single, @~] = ChainTuple()

		override def fullRow[E <: RowProduct]
		                    (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, Single, @~] =
			ChainTuple()

		override def fullTableStack :Seq[RelationSQL.from[RowProduct]#__] = Nil

		override def fullTableStack[E <: RowProduct]
		                           (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.from[E]#__] =
			LazyList.empty[RelationSQL.from[E]#__]

		override type Row = @~

		override def row :ChainTuple[RowProduct, Single, @~] = ChainTuple()

		override def row[E <: RowProduct]
		                (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, Single, @~] =
			ChainTuple()

		override def tableStack :Seq[RelationSQL.from[RowProduct]#__] = Nil

		override def tableStack[E <: RowProduct]
		             (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.from[E]#__] =
			LazyList.empty[RelationSQL.from[E]#__]


		override type OuterRow = @~

		override def outerRow :ChainTuple[RowProduct, Single, @~] = ChainTuple()

		override def outerRow[E <: RowProduct]
		                     (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[RowProduct, Single, @~] =
			ChainTuple()

		protected[sql] override type SingletonFromTable[T[O] <: MappingAt[O]] = this.type SelectFrom T
		protected[sql] override type GeneralizedFromTable[T[O] <: MappingAt[O]] = RowProduct NonParam T
	}



	/** A factory interface for classes extending [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow NonEmptyRow]],
	  * with a covariant upper bound on produced objects. It expands on the base `RowProductTemplate` by providing
	  * additional filtering methods [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]]
	  * working on the last relation only (rather than [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]])
	  * and [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]], which accepts a function taking
	  * the last two relations in this clause (assuming they both exist).
	  *
	  * Additionally, it splits the 'self' upper bound into two types: non-aliased `U` (such as `L InnerJoin R)`) and,
	  * for `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A`, its aliased form (such as `L InnerJoin R As A`).
	  * For ''from'' clauses without an alias for their last relation/grouping expression, both these types are equal.
	  * @tparam U the non aliased supertype of this clause (typically equivalent to `this.Self`).
	  * @tparam F the self type of this clause produced by various copy methods. It is always either `U`, or `U As A`.
	  */ //todo: rename to NonEmptyClauseTemplate
	trait NonEmptyRowTemplate[+U <: NonEmptyRow, +F <: U] extends RowProductTemplate[F] {
		thisClause :F with NonEmptyRowTemplate[U, F] =>
		/* Any extending 'template' traits (with the same type parameters) must not add any new methods or types
		 * referring to F. As refinement only narrows this trait with F = U As A.
		 * If such a need is necessary, there are two choices: use Self/Copy as the result type rather than F,
		 * or introduce the method here as protected.
		 */

		/** The supertype of `this.`[[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.Copy Copy]] which strips
		  * the alias type of the last (and only last) relation in this clause. Their relation is the same as
		  * between the type parameters `F` and `U`. Unlike [[net.noresttherein.oldsql.sql.RowProduct.NoAlias NoAlias]],
		  * this type typically does not need overriding.
		  */
		type NoAliasCopy >: Copy <: U { //todo: Copy <: UpperBound
			type LastMapping[O] = thisClause.LastMapping[O]
			type Last[C <: RowProduct] = thisClause.Last[C]
			type FromLast    = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Complete    = thisClause.Complete
			type NoAlias     = thisClause.NoAlias
			type Params      = thisClause.Params
			type FullRow     = thisClause.FullRow
			type Explicit    = thisClause.Explicit
			type Inner       = thisClause.Inner
			type Implicit    = thisClause.Implicit
			type Outer       = thisClause.Outer
			type Base        = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row         = thisClause.Row
			type OuterRow    = thisClause.OuterRow
		}

		/** A function type creating an [[net.noresttherein.oldsql.sql.SQLBoolean SQLBoolean]] based on the last
		  * two relations in this clause. If this type conforms to `F AndFrom T1 AndFrom T2`, than it takes the form of:
		  * {{{
		  *     (JoinedRelation[GeneralizedLeft[this.left.FromLast], this.left.LastMapping],
		  *      JoinedRelation[this.FromLast, this.LastMapping])
		  *     => SQLBoolean[this.Generalized]
		  * }}}
		  * If the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form of the last two joins
		  * is known, than both [[net.noresttherein.oldsql.sql.AndFrom.GeneralizedLeft GeneralizedLeft]]`[left.FromLast]`
		  * and [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] are supertypes of `Generalized`, hence
		  * any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] created basing on any of the two relations
		  * conforms to the required return type of `SQLExpression[Generalized]`.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]]
		  */
		type JoinFilter

		/** A variant of [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] with the return type
		  * using the member type `Copy <: F` used in its implementation. It should be used only for values of
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow NonEmptyRow]] (without a proper upper bound),
		  * if the definition of `JoinFilter` is supplied from the context where this type is known. In almost all cases,
		  * `on` should be preferred, as it provides better type inference and works better with implicits.
		  */
		def filtered(condition :JoinFilter) :Copy

		/** Apply a join condition to the last two relations in this clause. The condition is combined using `&&` with
		  * the preexising `where` part of this clause
		  * ([[net.noresttherein.oldsql.sql.Adjoin.condition Adjoin.condition]]) and becomes a part of `this.filter`.
		  * This works exactly like 'where', but instead of a single argument representing all joined relations,
		  * the filter function should take as its arguments the last two relations, i.e, the last relation defined
		  * by the left side of this join, if any, and the right side of this join.
		  * This method is only possible to call if this type is a subtype of `_ AndFrom T1 AndFrom T2`, that is
		  * it is an non-grouped ''from'' clause with at least two relations. Its signature then takes the form of:
		  * {{{
		  *     def on(condition :(JoinedRelation[GeneralizedLeft[left.FromLast], left.LastMapping],
		  *                        JoinedRelation[FromLast, R]) => SQLBoolean[Generalized]) :F
		  * }}}
		  * If the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form of the last two joins
		  * is known, then both [[net.noresttherein.oldsql.sql.AndFrom.GeneralizedLeft GeneralizedLeft]]`[left.FromLast]`
		  * and [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] are supertypes of `Generalized`, hence
		  * any [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] created basing on any of the two relations
		  * conform to the required return type of `SQLExpression[Generalized]`.
		  * For example, for type `From[L] InnerJoin R` it takes the form of
		  * {{{
		  *     def on(condition :(JoinedRelation[RowProduct AndFrom L Join R, L],
		  *                        JoinedRelation[RowProduct AndFrom R, R])
		  *                       => SQLBoolean[RowProduct AndFrom L Join R]) :From[L] InnerJoin R
		  * }}}
		  * If the left type is too abstract, the type of `condition` argument will likewise be abstract, preventing
		  * the caller from providing a value; on the other hand, `Dual` defines it simply as `Nothing`,
		  * also eliminating the possibility to call this method.
		  * @param condition a function accepting the expressions for the last two relations in this clause and creating
		  *                  an SQL expression for the join condition.
		  * @return a `AndFrom` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
		  *         returned by the passed filter function.
		  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
		  * @see [[net.noresttherein.oldsql.sql.AndFrom.GeneralizedLeft]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.FromLast]]
		  * @see [[net.noresttherein.oldsql.sql.ast.JoinedRelation]]
		  */
		protected def on(condition :JoinFilter) :F = filtered(condition)

		/** Apply a filter condition to the last relation in this clause. The condition is combined using `&&` with
		  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
		  * It is equivalent to `this.and(mappings => condition(mappings.last))`.
		  * @param condition a function accepting the expression for the last relation in this clause and creating
		  *                  an additional SQL expression for the join condition.
		  * @return an `Expanded` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
		  *         returned by the passed filter function.
		  */ //consider: simply JoinedRelation as the function argument - may result in better type inference
		def andLast(condition :Last[FromLast] => SingleBoolean[FromLast]) :F =
			where(condition(last))

		protected def whereLast(condition :Last[FromLast] => SingleBoolean[FromLast]) :F = where(condition(last))

		protected def havingLast(condition :Last[FromLast] => GroupedBoolean[FromLast]) :F = having(condition(last))


		/** Specify an alias for the last relation in this clause to use in the ''as'' clause following
		  * the relation name in the ''from'' clause. This is not necessary and may be overriden
		  * in case of conflicts, but can be used as the default value and/or help with debugging.
		  * If this relation is already aliased, the alias will be replaced with the given `alias :A`.
		  * In addition to the effect on the generated SQL, relations can be accessed based on this alias
		  * using the [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.apply JoinedMappings]]`(alias)`
		  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations.apply JoinedRelations]]`(alias)`.
		  * @param alias a `String` literal identifying this relation.
		  * @return a new clause isomorphic with this instance, but with a new last relation (not equal to this.last).
		  */
		def as[A <: Label](alias :A) :U As A = aliased(alias)
		//todo: as for non-literals - or does it already work using the singleton type?

		/** A variant of [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as as]] with the return type
		  * expressed in terms of the member type `NoAliasCopy`. It provides the implementation for the latter and
		  * can be used for `f :NonEmptyRow`, when `as` would lose the precise type information by being upcast
		  * to [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow NonEmptyRow]]. In all other cases,
		  * `as` should be preferred as it provides better type inference.
		  */
		def aliased[A <: Label](alias :A) :NoAliasCopy As A

	}



	/** A marker trait for all non empty clauses, i.e. those which contain at least one, non-param relation.
	  * It is defined as a clause with non-empty [[net.noresttherein.oldsql.sql.RowProduct.Row Row]].
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]] is its more prominent subtype, used for
	  * ''from'' clauses without a ''group by'' clause (conforming to
	  * [[net.noresttherein.oldsql.sql.FromClause FromClause]].
	  * All [[net.noresttherein.oldsql.sql.GroupByClause grouping]] clauses are automatically subtypes
	  * of this type, too.
	  */ //todo: rename to NonEmptyClause
	trait NonEmptyRow extends RowProduct with NonEmptyRowTemplate[NonEmptyRow, NonEmptyRow] { thisClause =>
		override type Last[F <: RowProduct] <: JoinedRelation[F, LastMapping]
		override type FromLast >: Generalized <: NonEmptyRow

		/** Implicit evidence witnessing that `this.FromLast` lists exactly one mapping. */
		implicit def lastTableOffset :RelationCount[FromLast, 1] = new RelationCount[FromLast, 1](1)

		/** The string literal `A` used as the alias for this relation if it conforms to
		  * `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A` and an undefined type for non-aliased clauses.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.aliasOpt]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.alias]]
		  */
		type Alias <: Label

		/** The name for the alias in the `as` clause following the last relation in this ''from'' clause.
		  * This option has value only if it was provided by the call to
		  * `f `[[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as as]]` alias` (and this clause is
		  * a `F `[[net.noresttherein.oldsql.sql.RowProduct.As As]]` A`). It's type is a `String` singleton type
		  * and can be used to access this relation from the
		  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]] facade to this instance.
		  */
		def aliasOpt :Option[Alias]

		/** The name for the alias in the `as` clause following the last relation in this ''from'' clause or an empty
		  * `String` if [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.aliasOpt aliasOpt]] is empty.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as as]]
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.As As]]
		  */
		def alias :String = aliasOpt getOrElse ""

		override type Generalized >: Complete <: NonEmptyRow {
			//todo: try to add Params = thisClause.Params
			type LastMapping[O] = thisClause.LastMapping[O]
			type FromLast       = thisClause.FromLast
			type Generalized   <: thisClause.Generalized //all these below must be <: because of From
			type Explicit      <: thisClause.Explicit
			type Implicit      <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
			type Base          <: thisClause.Base
			type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
		}

		override type Complete >: NoAlias <: NonEmptyRow {
			type LastMapping[O] = thisClause.LastMapping[O]
//			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast    = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Complete    = thisClause.Complete
			type LastParam   = thisClause.LastParam
			type Params      = thisClause.Params
			type FullRow     = thisClause.FullRow
			type Explicit    = thisClause.Explicit
			type Implicit    = thisClause.Implicit
			type Base        = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row         = thisClause.Row
			type OuterRow    = thisClause.OuterRow
		}

		override type NoAlias >: Self <: NonEmptyRow {
			type LastMapping[O] = thisClause.LastMapping[O]
//			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast    = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Complete    = thisClause.Complete
			type LastParam   = thisClause.LastParam
			type Params      = thisClause.Params
			type FullRow     = thisClause.FullRow
			type Explicit    = thisClause.Explicit
			type Implicit    = thisClause.Implicit
			type Base        = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row         = thisClause.Row
			type OuterRow    = thisClause.OuterRow
		}

		override type Self <: NonEmptyRow {
			type LastMapping[O] = thisClause.LastMapping[O]
//			type Last[O <: RowProduct] = thisClause.Last[O]
			type FromLast    = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Complete    = thisClause.Complete
			type LastParam   = thisClause.LastParam
			type Params      = thisClause.Params
			type FullRow     = thisClause.FullRow
			type Explicit    = thisClause.Explicit
			type Inner       = thisClause.Inner
			type Implicit    = thisClause.Implicit
			type Base        = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row         = thisClause.Row
			type OuterRow    = thisClause.OuterRow
		}

		//these remain NonEmptyRow so Subselect never becomes Dual Subselect
		override type AppliedParam <: NonEmptyRow
		override type GeneralizedParamless >: Paramless <: NonEmptyRow
		override type Paramless <: NonEmptyRow { type Params = @~ }
		override type BoundParamless >: Paramless <: NonEmptyRow { type Params = @~ }


		override def isEmpty = false

		protected[sql] override type SingletonFromTable[T[O] <: MappingAt[O]] = this.type Subselect T
		protected[sql] override type GeneralizedFromTable[T[O] <: MappingAt[O]] = Generalized Subselect T

		protected[sql] override def from[T[O] <: BaseMapping[S, O], S, A <: Label]
		                                (right :LastTable[T, S], alias :Option[A],
		                                 filter :SingleBoolean[GeneralizedFromTable[T]]) :this.type Subselect T As A =
			Subselect[this.type, T, S, A](this, right, alias)(filter)

		override type FromTable[T[O] <: MappingAt[O]] = Self Subselect T

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                 (first :Table[M])
		                 (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
				:Self Subselect T =
			Subselect(self, TableSQL.LastTable[T, S](cast(first)), None)(True)

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
		                 (first :StaticTable[A, M])
		                 (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
				:Self Subselect T As A =
			Subselect(self, TableSQL.LastTable[T, S](cast(first)), None)(True)


		override type FromSubselect[+F <: NonEmptyRow] = F#AsSubselectOf[Self] {
			type Implicit = thisClause.Generalized
			type Row <: F#Row
		}

		override def from[F <: NonEmptyRow with GroundRow](subselect :F)
				:subselect.AsSubselectOf[Self] { type Implicit = thisClause.Generalized } =
			subselect.asSubselectOf(self)

		override def fromSubselect[F <: NonEmptyRow]
		                          (subselect :F)(implicit expansion :subselect.Implicit ExpandedBy Generalized)
				:subselect.AsSubselectOf[Self] { type Implicit = thisClause.Generalized } =
			subselect.asSubselectOf(self)

	}






	/** A ''from'' clause which modifies clause `F` by providing an alias `A` for its last relation.
	  * This alias is used in the ''as'' clause following the relation name/SQL select: `from Rangers, Hamsters as heroes`,
	  * but can be automatically overriden during SQL generation in case of conflicts. At the same time, it serves as
	  * an indexing key for this relation, allowing it to be retrieved from
	  * [[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings JoinedMappings]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations JoinedRelations]] by providing a string literal
	  * of type `A`: `(From(Rangers) join Hamsters as "heroes").mappings(`[[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.apply[N<:Label] "heroes"]]`)`.
	  *
	  * The clause can be introduced to any [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow non-empty]]
	  * ''from'' clause, including grouping expressions and unbound parameters, via
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as as]] method accepting a `String` literal.
	  * The alias for the last relation in a non-empty clause can be retrieved programmatically through
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.alias NonEmptyRow.alias]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.aliasOpt NonEmptyRow.aliasOpt]].
	  * Note that the indexing function is often not required for access, as the relations can be retrieved also
	  * by their mapping and/or subject type.
	  *
	  * This is a type alias which structurally refines the type `F` itself, by demanding that `type Alias = A`
	  * and several 'copy' types preserve the aliasing. It is done this way for two reasons:
	  *   1. So that `(F As A) <: A`, and in particular that they share the
	  *      [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] type, allowing all expressions
	  *      created for not aliased clauses to be used regardless of what aliases are added and for which relations.
	  *      Similarly, the [[net.noresttherein.oldsql.sql.RowProduct.filter filter]] condition for the ''where'' clause,
	  *      and any other expressions exported or created by `F As A` are based on `F#Generalized`.
	  *   1. To allow it to appear both after actual joined relations in a
	  *      true, [[net.noresttherein.oldsql.sql.FromSome ungrouped]] ''from'' clause, and following
	  *      a grouping expression in a [[net.noresttherein.oldsql.sql.GroupByClause ''group by'']] clause:
	  *      {{{
	  *          From(Parties) as "parties" join PartyMembers join Characters as "heroes" groupBy
	  *             (_("heroes").level) as "level" by (_("heroes").name) as "name"
	  *      }}}
	  *      This would be impossible with a trait or class, as it would either have to extend one of
	  *      `FromSome`/`GroupByClause` types, preventing the usage as part of the other, or both, voiding type safety.
	  */
	//todo: this can be made to work double duty in a With clause, too. We don't really need NonEmptyRowTemplate,
	// just any Template class to narrow down the self type, and all where & co. methods turn into extensions
	// or protected methods as currently having.
	type As[+F <: NonEmptyRow, A <: Label] <: F with NonEmptyRowTemplate[F, F As A] {
		type Alias = A
		type NoAlias <: NonEmptyRow //this reverses the direction of bounds from NoAlias >: Self <: NonEmptyRow :(
		type Self = NoAlias As A
		type Copy <: F As A
		//consider: maybe simply define Aliased[F <: NonEmptyFrom] <: F type and narrow it? It would replace narrowing
		// on the below types. Currently we are losing As in numerous places and this could also help with preserving
		// the Self type. A potential issues is that we don't really want Aliased[Self] inferred, only Self
		type LeftBound <: RowProduct
		type NoAliasLeft[+L <: LeftBound] <: NonEmptyRow
		type WithLeft[+L <: LeftBound]    <: NoAliasLeft[L] As A //why not = NoAliasLEft[L] As A
	}

	/** A matching pattern for ''from''/''group by'' clauses which feature an alias for their last relation/grouping
	  * expression. Note that, as [[net.noresttherein.oldsql.sql.RowProduct.As As]] is a type alias and not a trait
	  * or class, it can't be matched by type. Instead, this check simply verifies if
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.aliasOpt aliasOpt]] is defined.
	  */
	object As {
		/** Matches an aliased clause, providing that `as.aliasOpt.isDefined`. A non-aliased clause `F` can potentially
		  * be constructed appear as `F As F#Alias`, with appropriate member type definitions, so this match can fail.
		  */
		def unapply[F <: NonEmptyRow, A <: Label](as :F As A) :Opt[(F, A)] = as.aliasOpt match {
			case Some(alias) => Got((as, alias))
			case _ => Lack
		}

		/** Matches a clause if it features an alias for its last relation/grouping expression, that is its
		  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRow.aliasOpt aliasOpt]] property is defined.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.As]]
		  */
		def unapply(from :RowProduct) :Opt[(NonEmptyRow, Label)] = from match {
			case as :NonEmptyRow if as.aliasOpt.isDefined => Got((as, as.aliasOpt.get))
			case _ => Lack
		}
	}



	/** A supertype of all clause types in their generalized form.
	  * it refines `F` to ensure its `Generalized` type is a subtype of `F` itself. As all concrete `Generalized` types
	  * start with `RowProduct` and `RowProduct#Generalized` is a proper subtype of `RowProduct`, this transformation
	  * always creates a proper subtype of its argument type and no fixed point exists.
	  * This essentially requires `F` to already be a `Generalized` type of a non empty clause.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct$.Complete]]
	  */ //Currently nothing conforms to this type, but we can cast things to it in order to force their conformance
	type Generalized[F <: RowProduct] <: F { type Generalized <: RowProduct.Generalized[F] }

	/** A supertype of all clauses type with a known complete form of `F`. It ensures that either `F` is a fixed point
	  * of type functor `Complete[F] = F#`[[net.noresttherein.oldsql.sql.RowProduct!.Complete Complete]],
	  * or type `Complete[F]` is contradictory (has no values). Conforming types list all relations present
	  * in any instance in their type: `F` must start with [[net.noresttherein.oldsql.sql.From From]]
	  * or [[net.noresttherein.oldsql.sql.Dual Dual]], not a wildcard type. Additionally, all listed
	  * clause type constructors must be of their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]]
	  * kind, and they do not include an [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause.
	  *
	  * This type is used primarily as ''from'' clauses of SQL ''select''
	  * [[net.noresttherein.oldsql.sql.Select statements]] and [[net.noresttherein.oldsql.sql.ast.SelectSQL expressions]].
	  * Note that this type simply ensures that `F =:= F#Complete`, and its type parameter must already be in
	  * its complete type: this type does not change that. In particular, `Complete[from.Complete] =:= from.Complete`
	  * for any clause `from :RowProduct`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct$.Generalized]]
	  */ //in Scala 3 we can define it simply as Complete[F <: RowProduct] = F { type Complete = F }. Currently no proper conjunction with bounds on F#Complete
	type Complete[F <: RowProduct]
//		>: F { type Complete >: RowProduct.Complete[F] <: RowProduct.Complete[F] }
		<: F { type Complete <: RowProduct.Complete[F] }

	/** Supertype of all clause types in their 'self' form. It refines `F` to ensure its `Self` type is equal
	  * to `F` itself. Only fully aliased clauses (including empty ones) have well defined `Self` types,
	  * so instances cannot be typically expected to conform to this type naturally.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct$.Complete]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct$.Generalized]]
	  */
	type Self[F <: RowProduct]
//		>: F { type Self >: RowProduct.Self[F] <: RowProduct.Self[F] }
		<: F { type Self <: RowProduct.Self[F] }


	//consider: deleting those two
	/** A `RowProduct` refinement which is an upper bound of all ''from'' clauses with a statically known `Generalized`
	  * type. Such clauses are referred to as ''generalized'' for short. If `F <: GeneralizedFrom`, then
	  *   a) `F` is ''complete'' - the number of joined relations is statically known, and
	  *   b) `F <: F#Generalized` - all 'join' kinds are narrowed down at least to their `Generalized` form
	  *     (although this can't be expressed in the type system).
	  * Clauses conforming to this type generally offer full functionality, in particular regarding access to
	  * their mappings. The above statement holds only for subtypes which conform to this type naturally,
	  * rather than through refinement. In particular, `GeneralizedFrom` is a generic type for which
	  * no functions depending on the statically known structure (i.e., based on implicit evidence), in particular
	  * access to mappings, will be available.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ConcreteRow]]
	  */ //todo: rename to GeneralizedClause
	type GeneralizedRow = RowProduct {
		type FromLast    >: this.type <: RowProduct
		type Generalized >: this.type <: FromLast
	}

	//fixme: only Dual and fully aliased clauses conform to this, as all joins use bound types for Self to support aliasing
	/** A `RowProduct` refinement which is the upper bound of all ''from'' clauses with a statically known
	  * `Self` type; such clauses are referred to as ''concrete''. If `F <: ConcreteFrom`, then
	  *   a) `F` is ''complete'' - the number of joined relations is statically known,
	  *   b) `F <: GeneralizedRow` and `F <: F#Generalized` (although the latter cannot be expressed
	  *      in the type system),
	  *   c) `F <: F#Self` - there are no abstract join kinds in the type signature (though this cannot be expressed
	  *      in the type system either).
	  *   d) result types of all methods returning other clauses are likewise `ConcreteFrom` instances, although
	  *      again this cannot be expressed directly.
	  * In essence, conforming to the this type signifies that a type is concrete with respect to the join aspect,
	  * although the mapping type parameters may still be abstract types. Conforming ''naturally'' to this type implies
	  * that all functions, in particular those depending on implicit evidence, are available and their result types
	  * are statically known. This does not hold if a clause conforms only through refinement, in particular by
	  * being declared as `ConcreteFrom`, as this type alias carries no information in itself.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow]]
	  */ //todo: rename to ConcreteClause
	type ConcreteRow = RowProduct {
		type FromLast    >: this.type <: RowProduct
		type Generalized >: this.type <: FromLast
		type Self        >: this.type <: Generalized
	}



	/** A refinement of `RowProduct` subtype `F` imposing an upper bound of `F` itself on its
	  * [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] member types.
	  */
	type SuperGeneralized[+F <: RowProduct] = F { type Generalized >: Self <: F; type Self <: F }

	/** A refinement of `RowProduct` subtype `F` imposing an upper bound of `F` itself on its
	  * [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] member type.
	  */
	type SuperSelf[+F <: RowProduct] = F { type Self <: F }

	/** A `RowProduct` of a top level, independent ''select'' - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (is not a ''from'' clause of a subselect
	  * of some other select). In order to conform naturally (rather than by refinement) to `TopRow`, the clause
	  * must be ''complete'', that is its static type must start with either
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] - rather than a
	  * wildcard or abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * (or [[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]]).
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * An outer clause is the only place where unbound join parameters are allowed, but is not in itself a valid
	  * base for a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] in that case. For that, see its subtype
	  * [[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]]. For convenience, this type has sibling
	  * definitions with lower, often occurring, type bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.TopFromClause TopFromClause]],
	  * [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]] and
	  * [[net.noresttherein.oldsql.sql.GroupByClause.TopGroupByClause TopGroupByClause]]; they all conform to this type.
	  */ //we opt for a minimalistic definition so that more abstract method results conform to it, rather than have more info
	type TopRow = RowProduct { //todo: rename to TopClause
		type Implicit = RowProduct  //everything down to Dual is explicit
	}

	/** A `RowProduct` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * synthetic joins. Representing a single ''from'' clause (and not one of a nested subselect). Only such clauses
	  * can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.RowProduct!.GeneralizedRow GeneralizedRow]], but there is no actual subtype
	  * relationship between the two in the type system. A `GroundRow` will however always by a subtype of
	  * [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]], which groups all outer clauses, including
	  * those with unbound parameters, and [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessRow]].
	  * For convenience, this type has two sibling types with narrowed upper bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.GroundFromClause GroundFromClause]],
	  * [[net.noresttherein.oldsql.sql.FromSome.GroundFromSome GroundFromSome]] and
	  * [[net.noresttherein.oldsql.sql.GroupByClause.GroundGroupByClause GroundGroupByClause]].
	  */ //todo: rename to GroundClause
	type GroundRow = RowProduct { //Dual.DirectSubselect is a subtype of this, differing only in the bound of NonEmptyRow
		type Implicit = RowProduct
		type Base = RowProduct
		type DefineBase[+I <: RowProduct] = I //No unbound parameters; crucial for use in SelectSQL
		type Params = @~ //implied from the other two, but nice to be ParamlessFrom
	}



	/** A ''from'' clause is ''dependent'' on another clause `F` if its ''implicit'' portion is a supertype of `F`,
	  * meaning it is a potential candidate for creating a (direct) subselect expression nested under that clause.
	  * Whether or not it will be actually possible will depend on whether there are any
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * 'joins' in its explicit portion. It is a supertype of all valid subselect clauses defined as
	  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]`, introduced as a result type
	  * for functions creating subselects which can't statically guarantee the lack of unbound parameters.
	  */ //GroundedIn?
	type DependantOf[-F <: RowProduct] = RowProduct {
		type Implicit >: F <: RowProduct //F may still have a longer prefix hidden under RowProduct at start of Base
	}

	/** An upper bound for `RowProduct` subtypes representing valid ''from'' clauses of subselects of a select
	  * with the ''from'' clause `F`. `S <: SubselectOf[F]` if and only if `S` either:
	  *   - has the form of `F Subselect M1 J2 M2 ... JN MN` for some mappings `M1...MN`
	  *     and join types `J2...JN`, with no `Subselect` or `JoinParam` joins among them.
	  *   - is a complete, non subselect clause: `S <: `[[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]]
	  *     with no `JoinParam` joins.
	  *
	  * Clauses conforming to [[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]]`[F]` can use
	  * all the mappings/entities which are a part of `F`, but they are not a part of any select expressions
	  * created from that clause. This allows the use of nested select queries which depend on values
	  * from the ''from'' clause of the outer select.
	  *
	  * `S <: SubselectOf[F]` does ''not'' imply that any clause `s :S` is a proper subselect of any clause  `f :F`,
	  * in the sense that `s` contains all of the relations from `f` in its implicit portion. Instead, it witnesses
	  * that `F` satisfies all the implicit dependencies of `S`:
	  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] created from `S` is an `SQLExpression[F, GlobalScope, _]`.
	  * If `F <: GeneralizedRow`, then such a subselect expression can be used
	  * as a part of both ''select'' and ''where'' clauses of a select from `F`. On the other hand,
	  * an `SQLExpression[F, GlobalScope, T]` is convertible to `SQLExpression[S, GlobalScope, T]` by a call
	  * [[net.noresttherein.oldsql.sql.SQLExpression.expand expand]]`(s)(s.explicitSpan, GlobalScope)`.
	  * In other words, when concerning oneself only with the explicit portion of `S`, that is all relations
	  * and join/filter conditions in its deepest subselect, any instance of `S` can be safely evaluated with `F`
	  * as the enclosing select, without any modifications to its ''where'' clause filter.
	  *
	  * Perhaps counterintuitively, this type is contravariant rather than covariant. There are two reasons behind it:
	  *   1. one, preventing any type from becoming a subselect clause of a clause with a more abstract prefix
	  *      (with fewer relations specified), ensuring that the full implicit mapping list of the subselect
	  *      clause is accounted for, and,
	  *   1. two, treating all join kinds as equivalent for this purpose.
	  *
	  * Note that subselects may be nested to an arbitrary depth and only directly nested dependent seleccts of `F`
	  * conform to this type.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.DependantOf]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectRow]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  */ //consider: making it <: NonEmptyRow. The problem is Aggregated which is not a NonEmptyRow, as well as any others we create.
	type SubselectOf[-F <: RowProduct] = RowProduct {
		type Implicit = Base
		type Base >: F <: RowProduct          //F may still have an abstract prefix
		type DefineBase[+I <: RowProduct] = I //this assures that Base=Implicit, i.e. there are no UnboundParam joins in Explicit
	}

	/** A supertype of all valid subselect clauses with their [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]]
	  * and [[net.noresttherein.oldsql.sql.RowProduct.Base Base]] types equal to `F`. It is very similar to
	  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]`, but invariant in `F`
	  * and the types mentioned are equal, rather than supertypes of `F`.
	  * `ExactSubselectOf[F] <:< SubselectOf[F]` for any `F <: RowProduct`,
	  */
	type ExactSubselectOf[F <: RowProduct] = RowProduct {
		type Implicit = F
		type Base = F
		type DefineBase[+I <: RowProduct] = I
	}
//
//	/** A version of [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]], which is a refinement of `F`
//	  * and which uses `B` as the definition for [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]] and
//	  * [[net.noresttherein.oldsql.sql.RowProduct.Base Base]] types of the refined clause `F`, rather than their
//	  * upper bound.
//	  */ //this exists only as a volatile type workaround, in Scala 3 should be got rid of
//	type ProperSubselectOf[+F <: RowProduct, B <: RowProduct] = F {
//		type Implicit = B
//		type Base = B
//		type DefineBase[+I <: RowProduct] = I
//	}

	/** An upper bound for all ''from'' clauses of subselect clauses, that is `RowProduct` subtypes with a
	  * `Subselect` join in the type definition: `S <: F Subselect T1 ... TN . ` for some type `F <: RowProduct`
	  * and mapping type constructors `T1 ... TN`. For a type to conform to `SubselectRow`, the join kinds in
	  * the explicit part of the clause (after the last subselect) must be statically known to be `Join` subtypes
	  * or ''group by'' elements.
	  * Note that ''outer'' clauses (without any `Subselect` joins) are considered subselect clauses of any other
	  * clause in terms of [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]], but do not conform
	  * to this type.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf]]
	  */ //todo: rename to SubselectClause/DependentClause
	type SubselectRow = RowProduct {
		type Implicit <: NonEmptyRow //this holds only if there is a Subselect involved (meaning clear way to the Subselect)
		type Base = Implicit //this excludes clauses with a ParamClause in the explicit portion
		type DefineBase[+I <: RowProduct] = I //only to preserve the validity on rebasing and to conform to DirectSubselect
	}



	/** An upper bound for all ''from'' clauses which do not contain any
	  * [[net.noresttherein.oldsql.sql.ParamClause ParamClause]] 'joins' in their concrete type.
	  * In order to prove this conformity the clause type must be complete and cannot contain
	  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] joins (all joins in its static type must be at least
	  * as specific as their [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form).
	  * It is possible however to propagate this constraint to incomplete clauses simply by declaring them as
	  * [[net.noresttherein.oldsql.sql.FromSome.ParamlessFromSome ParamlessFromSome]]` Join X ...` instead of
	  * `FromSome Join X ...`. Note that
	  * `ParamlessFrom =:= `[[net.noresttherein.oldsql.sql.RowProduct.ParameterizedRow ParameterizedRow]]`[@~]`.
	  * Analogical narrowing type aliases are defined also for most common subclasses of `RowProduct` in their
	  * companion objects, such as `FromClause` and `FromSome`.
	  */ //consider: renaming to ParamlessClause/ParamlessProduct/ParamlessRelations
	type ParamlessRow = RowProduct { //consider: can we make a type 'has no params in the instantiated portion (that we know of)'?
		type Params = @~
//		type Paramless = Self
	}

	/** An upper bound for all ''from'' clauses which are known to contain at least one
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * 'join' in their static type. Note that a `RowProduct` not conforming to this type does not mean that
	  * it indeed contains no parameters, as the information may have been lost by type abstraction.
	  */ //consider: renaming to ParameterizedClause or Parameterized
	type ParameterizedRow[P] = RowProduct {
		type Params = P
	}

	/** A type alias marking a `RowProduct` as containing no joins
	  * other than [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], used for stored procedure call expressions
	  * and DML. It conforms to [[net.noresttherein.oldsql.sql.FromSome.ParameterizedFromSome ParameterizedFromSome]]
	  * (and its counterparts for super types of `FromSome`, including
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]), as well as
	  * [[net.noresttherein.oldsql.sql.FromSome.TopFromSome TopFromSome]].
	  */ //consider: renaming to ParamsClause and ParamClause to AndParam.
	type ParamsRow[P] = FromSome { //FromSome to use it on the left side of JoinParam
		type Params = P
		type Row = @~
		type Implicit = RowProduct
	}

	private[RowProduct] sealed trait ParamsRowCompanion {
		/** A type alias for a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] representing an empty parameter list
		  * to a stored procedure/function call. It is used as the starting point for all other (concrete) subtypes
		  * of [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]], which can be built by expanding it
		  * with [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo join and its type alias
		  * [[net.noresttherein.oldsql.sql.WithParam WithParam]]. Note that that this type is contradictory,
		  * breaking the contract: it declares no join [[net.noresttherein.oldsql.sql.ParamClause parameters]]
		  * and no non-parameter relations, which implies it being an empty clause, but at the same time
		  * it is a (non-empty) [[net.noresttherein.oldsql.sql.FromSome]] clause. This is an unfortunate consequence
		  * of the current limitation of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which requires
		  * a `FromSome` clause as its left side. The [[net.noresttherein.oldsql.sql.RowProduct.row row]] expression
		  * (and other related values and types) are not actually empty, but contain an unspecified dummy relation.
		  * @see [[net.noresttherein.oldsql.sql.Call Call]]
		  */
		type Empty <: ParamsRow[@~] { type Generalized >: Empty <: ParamsRow[@~]; type Self <: Empty }

		/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] representing an empty parameter list
		  * to a stored procedure/function call. It is used as the starting point for all other instances
		  * of [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]], which can be built by joining
		  * it with the standard [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param]]
		  * extension methods. Note that that this type is contradictory, breaking the contract:
		  * it declares no join [[net.noresttherein.oldsql.sql.ParamClause parameters]] and no non-parameter relations,
		  * which implies it being an empty clause, but at the same time
		  * it is a (non-empty) [[net.noresttherein.oldsql.sql.FromSome]] clause. This is an unfortunate consequence
		  * of the current limitation of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which requires
		  * a `FromSome` clause as its left side. The [[net.noresttherein.oldsql.sql.RowProduct.row row]] expression
		  * (and other related values and types) are not actually empty, but contain an unspecified dummy relation.
		  * @see [[net.noresttherein.oldsql.sql.Call Call]]
		  */
		val empty :Empty
	}
	val ParamsRow :ParamsRowCompanion = new ParamsRowCompanion {
		override type Empty = FromClause.EmptyFromSome
		override val empty = FromClause.EmptyFromSome
	}

	/** A type alias for a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] representing an empty parameter list
	  * to a stored procedure/function call. It is used as the starting point for all other (concrete) subtypes
	  * of [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]], which can be built by expanding it
	  * with [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] pseudo join and its type alias
	  * [[net.noresttherein.oldsql.sql.WithParam WithParam]]. Note that that this type is contradictory,
	  * breaking the contract: it declares no join [[net.noresttherein.oldsql.sql.ParamClause parameters]]
	  * and no non-parameter relations, which implies it being an empty clause, but at the same time
	  * it is a (non-empty) [[net.noresttherein.oldsql.sql.FromSome]] clause. This is an unfortunate consequence
	  * of the current limitation of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which requires
	  * a `FromSome` clause as its left side. The [[net.noresttherein.oldsql.sql.RowProduct.row row]] expression
	  * (and other related values and types) are not actually empty, but contain an unspecified dummy relation.
	  * @see [[net.noresttherein.oldsql.sql.Call Call]]
	  */
	type NoParams = ParamsRow.Empty

	/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] representing an empty parameter list
	  * to a stored procedure/function call. It is used as the starting point for all other instances
	  * of [[net.noresttherein.oldsql.sql.RowProduct.ParamsRow ParamsRow]], which can be built by joining
	  * it with the standard [[net.noresttherein.oldsql.sql.FromSome.TopFromSomeExtension.param param]]
	  * extension methods. Note that that this type is contradictory, breaking the contract:
	  * it declares no join [[net.noresttherein.oldsql.sql.ParamClause parameters]] and no non-parameter relations,
	  * which implies it being an empty clause, but at the same time
	  * it is a (non-empty) [[net.noresttherein.oldsql.sql.FromSome]] clause. This is an unfortunate consequence
	  * of the current limitation of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]], which requires
	  * a `FromSome` clause as its left side. The [[net.noresttherein.oldsql.sql.RowProduct.row row]] expression
	  * (and other related values and types) are not actually empty, but contain an unspecified dummy relation.
	  * @see [[net.noresttherein.oldsql.sql.Call Call]]
	  */
	val NoParams :NoParams = ParamsRow.empty

	/** A narrowing of a ''from'' clause type `F` enforcing that `P` is the type of
	  * its [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] chain both of `F`
	  * and `F#`[[net.noresttherein.oldsql.sql.RowProduct.Self Self]].
	  * This type conforms to [[net.noresttherein.oldsql.sql.RowProduct.ParameterizedRow ParameterizedRow]]`[P]`.
	  */
	type ParameterizedWith[+F <: RowProduct, P <: Chain] = F { type Self <: F { type Params = P }; type Params = P }



	/** An upper bound of all aggregated `RowProduct` subtypes
	  * with [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] type of their 'real' ''from'' clause
	  * conforming to `F`. By the 'real' ''from'' clause we mean their prefix clause stripped of the longest
	  * suffix of [[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]] links.
	  * This encompasses both queries with a ''group by'' clause:
	  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] as well as a special clause for ungrouped
	  * ''selects'' which contain an SQL aggregate function in their ''select'' clause (such as `select count(*) from T`):
	  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]. Their common properties are
	  *   1. Relations from the explicit portion of `F` (i.e. those belonging to the most nested subselect,
	  *      but not those from enclosing ''selects''), are unavailable
	  *      to [[net.noresttherein.oldsql.sql.SQLExpression expressions]] based on this clause.
	  *   1. A [[net.noresttherein.oldsql.sql.GroupedSQL LocalSQL]]`[F, X]` is a valid subexpression
	  *      for use in the ''group by'' clause and
	  *   1. It can potentially be used as an argument to some
	  *      [[net.noresttherein.oldsql.sql.AggregateFunction aggregate]] function, creating
	  *      an expression valid for use in the ''select'' and ''having'' clauses of this type.
	  *
	  * This type is defined as a refinement
	  * of the [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]] type,
	  * which is defined as `this.from.`[[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]],
	  * that is the type of the longest prefix conforming to [[net.noresttherein.oldsql.sql.FromSome FromSome]].
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.GroupingOf]]
	  */ //this type is covariant as it used by covariant Parameterization to ungroup itself to its GeneralizedDiscrete
	type AggregateOfGeneralized[+F <: RowProduct] = AggregateClause { //consider: moving to AggregateClause
		type GeneralizedDiscrete <: F
	}

	/** An upper bound on all clauses containing all the relations listed in `F`, joined with the same
	  * - or more specific - joins, and the rows of which are aggregated, rather than available individually
	  * in the created SQL select. This encompasses both queries with a ''group by'' clause:
	  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], as well as the special clause
	  * for ungrouped ''selects'' which contain an SQL aggregate function in their ''select'' clause
	  * (such as `select count(*) from T`): [[net.noresttherein.oldsql.sql.Aggregated Aggregated]].
	  * Their common property is that relations from the explicit portion of `F` (i.e. those belonging
	  * to the most nested subselect, but not those from enclosing ''selects''), are not available
	  * to [[net.noresttherein.oldsql.sql.SQLExpression expressions]] based on this clause.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.GroupingOf]]
	  */
	type AggregateOf[+F <: RowProduct] = AggregateClause {
		type Discrete <: F
	}

	/** The upper bound for all ''group by'' clauses such that the generalized form of their grouped segment
	  * (the actual ''from'' clause) conforms to `F`, guaranteeing in particular that
	  * [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]`[F]` can be used as part of a grouping expression
	  * in their ''group by'' clauses, as well as aggregated
	  * with an [[net.noresttherein.oldsql.sql.AggregateFunction aggregate]] function for use in
	  * their ''select'' and ''having'' clauses. This works by refinement
	  * of the [[net.noresttherein.oldsql.sql.GroupByClause.GeneralizedDiscrete GeneralizedDiscrete]] type,
	  * which is defined as `this.from.`[[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]],
	  * that is the type of the longest prefix conforming to [[net.noresttherein.oldsql.sql.FromSome FromSome]].
	  */ //consider: moving to GroupByClause
	type GroupingOfGeneralized[+F <: RowProduct] = GroupByClause {
		type GeneralizedDiscrete >: Discrete <: F
		type Discrete <: F
	}

	/** An upper bound of all ''group by'' clauses featuring all relations (and their respective joins)
	  * listed in `F` in their actual ''from'' clause. This works by refinement of
	  * the [[net.noresttherein.oldsql.sql.GroupByClause.Discrete Discrete]] type, which is defined as
	  * the [[net.noresttherein.oldsql.sql.RowProduct.Self Self]] type of the longest prefix conforming to
	  * [[net.noresttherein.oldsql.sql.FromSome FromSome]].
	  */ //fixme: doesn't really work, because g.generalized.Discrete <:< g.generalized.GeneralizedDiscrete doesn't hold.
	type GroupingOf[+F <: RowProduct] = GroupByClause {
		type Discrete <: F
	}


	/** A lower bound of wildcard clauses, that is abstract subclasses
	  * of [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] which are not composites adding new relations
	  * or other information to another clause type. Wildcard clauses are commonly used as upper bounds
	  * of the left sides of various [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] subclasses. As these are normally
	  * covariant in their clause type parameter, an argument of an appropriate wildcard clause yields a supertype
	  * of all such 'joins', regardless of their left sides.
	  *
	  * No values can exist, as many member types of [[net.noresttherein.oldsql.sql.FromSome FromSome]] and
	  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] have conflicting definitions.
	  * It is useful as a lower bound on type parameters when one wants to express the property of some mapping `M`
	  * being the first listed mapping by a particular clause, for example:
	  * {{{
	  *     //A witness that M is the first listed relation in type C
	  *     class FirstRelation[C <: RowProduct, M[O] <: MappingAt[O]]
	  *
	  *     implicit def first[L >: WildcardRow <: RowProduct, J[A <: L, B[O] <: R[O]] <: A Adjoin B, R[O] <: MappingAt[O]]
	  *                       (fromLast :L J R) :FirstRelation[L J R, R] = new FirstRelation
	  *
	  *     implicit def previous[L <: RowProduct, J[A <: L, B[O] <: R[O]] <: A Adjoin B, R[O] <: MappingAt[O], M[O] <: MappingAt[O]]
	  *                          (implicit first :FirstRelation[L, M]) :FirstRelation[L J R, M] = new FirstRelation
	  * }}}
	  */
	type WildcardRow = FromSome with GroupByClause






	/* There is a certain competition between the xxxExtension and xxxTemplate classes, as their primary purpose
	 * is to provide a 'clean' return type (not an abstract member type with an upper bound, which is bad for inference).
	 * The possibilities are however different and, unfortunately, both families are needed.
	 * RowProductTemplate:
	 *   1. Polymorphic methods, possible to override in subclasses and, in general, provide differing implementations.
	 *   1. F#Copy <: F, so can call any factory methods like where, etc.
	 * RowProductExtension:
	 *   1. Invariant F - passable as the type argument to invariant higher types like JoinedMappings.
	 *   1. Works for any F <: RowProduct, FromSome, etc - no need to declare it as RowProduct with RowProductTemplate[F].
	 *   1. Can be defined for refinements only, like TopFromSomeExtension
	 *   1. Can be defined for any RowProduct subtype, while in F As A, templates other than NonEmptyRowTemplate
	 *      will be parameterized only with F, not with F As A. This however can be hacked with introducing new methods
	 *      from subclasses as protected methods in NonEmptyRowTemplate -
	 *      NonEmptyRowTemplate[U, F] with GroupByClauseTemplate[F, F] seems to be just as good as GroupByClauseTemplate[U, F].
	 */

	implicit def RowProductExtension[F <: RowProduct](clause :F) :RowProductExtension[F, clause.type] =
		new RowProductExtension[F, clause.type](clause)

	/** Extension methods for `RowProduct` classes which benefit from having a static, invariant self type.
	  * It contains the default `select` methods, which rely on an implicit
	  * [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class.
	  * @tparam F    the inferred type of `this` clause.
	  * @tparam This the singleton type of `this` clause; preserves member types such as `Self`, where `F` could not.
	  */ //todo: rename to ClauseExtension
	class RowProductExtension[F <: RowProduct, This <: F with Singleton](val thisClause :This) {
		import thisClause.{FromLast, Last, LastMapping, Generalized, Params, Base, Row}
		import thisClause.{isParameterized, generalized, self, row}

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.RowProduct.SelectedFrom SelectedFrom]]
		  * (meaning it is a valid ''from'' clause for a subselect of its `Implicit` clause) and, if so, casts `this`
		  * to `outer.`[[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]] and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a subselect clause, or `None` otherwise.
		  */
		@inline def ifSubselect[T](map :F with thisClause.outer.DirectSubselect => T) :Option[T] =
			if (thisClause.isValidSubselect)
				Some(map(thisClause.asInstanceOf[F with thisClause.outer.DirectSubselect]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.RowProduct.TopRow TopRow]]
		  * (meaning it is a top-level clause with an empty ''implicit'' portion) and, if so,
		  * casts `this` to `F with TopRow` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is an ''outer'' clause, or `None` otherwise.
		  */
		@inline def ifTop[T](map :F { type Implicit = RowProduct } => T) :Option[T] =
			if (!thisClause.isSubselect)
				Some(map(thisClause.asInstanceOf[F { type Implicit = RowProduct }]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.RowProduct.GroundRow GroundRow]]
		  * (meaning it is a valid ''from'' clause for a ''free'' select - its ''implicit'' portion is empty and
		  * it contains no [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters) and, if so,
		  * casts `this` to `F with GroundRow` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a ''free'' clause, or `None` otherwise.
		  */
		@inline def ifGround[T](map :F with GroundRow => T) :Option[T] =
			if (!thisClause.isSubselect && !isParameterized)
				Some(map(thisClause.asInstanceOf[F with GroundRow]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessFrom]]
		  * (meaning it has no [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters in its definition) and,
		  * if so, casts `this` to `F with ParamlessFrom` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a parameterless clause, or `None` otherwise.
		  */
		@inline def ifParameterless[T](map :F { type Params = @~ } => T) :Option[T] =
			if (thisClause.isSubselect && isParameterized)
				Some(map(thisClause.asInstanceOf[F { type Params = @~ }]))
			else None

		//fixme: all docs below are not up to date
		//it would be good to have also variants not relying on CanSelect, but we'll see how it fares
		/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses
		  * and the expression returned by the passed function as its ''select'' clause.
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
		  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
		  * the `Outer` clause).
		  * @param selectClause a function returning any kind of SQL expression (including, in particular, tuples,
		  *                     which will be inlined in the ''select'' clause), constructable from the expressions
		  *                     for the joined relations of this clause.
		  * @return a `SelectSQL` based on this instance's `Implicit` type (that is, embeddable only as an expression
		  *         in instances of this `this.Implicit`), using the given arbitrary expression as the ''select'' clause.
		  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins
		  *                                       in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]]
		  *                                       section).
		  * @throws IllegalArgumentException if the return expression contains a subexpression of a not supported type.
		  */
		def select[E](selectClause :JoinedMappings[F] => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
			result(thisClause.self, selectClause(thisClause.mappings))

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F]) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F]
		                             ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                             ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables, tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			result(thisClause.self, selectClause(tables, tables, tables, tables, tables, tables, tables, tables, tables))
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(tables, tables, tables, tables, tables, tables, tables, tables, tables, tables)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F],
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		def select[E](selectClause :(JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F], JoinedMappings[F], JoinedMappings[F],
		                             JoinedMappings[F], JoinedMappings[F]
		                            ) => E)
		             (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
		{
			val tables = thisClause.mappings
			val e = selectClause(
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables, tables,
				tables, tables, tables, tables, tables, tables, tables, tables, tables, tables
			)
			result(thisClause.self, e)
		}

		/** Creates an SQL ''select'' with the given expression as its ''select'' clause.
		  * The type of the returned value depends on the type of the argument, whether this instance is parameterized,
		  * and whether its a [[net.noresttherein.oldsql.sql.RowProduct.SubselectRow subselect]] or
		  * [[net.noresttherein.oldsql.sql.RowProduct.TopRow top]] ''select''.
		  * If this instance represents a dependent select, the returned type is always
		  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] subtype specific to the argument expression type,
		  * with `this.`[[net.noresttherein.oldsql.sql.RowProduct.Base Base]] as its domain (first type parameter).
		  * Note the use of `Base` rather than [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]],
		  * with the former being defined as `Nothing` by clauses which contain parameters in their explicit
		  * portions (i.e., in the proper ''from'' clause).
		  * Otherwise, a [[net.noresttherein.oldsql.sql.Select Select]] instance is returned instead. However,
		  * if this instance does not contain any [[net.noresttherein.oldsql.sql.ParamClause unbound]] parameters
		  * (`this.`[[net.noresttherein.oldsql.sql.RowProduct.Params Params]]` = @~`), then the result will
		  * be a [[net.noresttherein.oldsql.sql.ast.SelectSQL.TopSelectSQL TopSelectSQL]], which is both
		  * a `SelectSQL` (an SQL expression) and `Select` (a parameterized SQL statement).
		  *
		  * See [[net.noresttherein.oldsql.sql.mechanics.CanSelect CanSelect]] type class documentation
		  * for more details on exact supported expression types and their corresponding results.
		  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins
		  *                                       in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]]
		  *                                       section).
		  * @throws IllegalArgumentException if the return expression contains a subexpression of a not supported type.
		  */
//		def select[V](selectClause :SQLExpression[_, Grouped, V])
//		             (implicit result :CanSelect[thisClause.Complete, selectClause.type]) :result.Select =
//			result(thisClause.self, selectClause)

		def select[E](selectClause :E)(implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
			result(thisClause.self, selectClause)
		//we don't want singleton types in the signature of Select
//		def select[S, O](component :TypedMapping[S, O])(implicit factory :CanSelect[thisClause.Complete, component.type]) :factory.Select

		/** Creates an SQL ''select'' expression selecting all columns from all relations of the explicit (subselect)
		  * portion of this clause as a `Chain` of relation subjects.
		  * It is equivalent to `select(this.`[[net.noresttherein.oldsql.sql.RowProduct.row row]]`)`.
		  * If this clause is a parameterized [[net.noresttherein.oldsql.sql.RowProduct.TopRow top]] clause,
		  * the result will be a
		  * [[net.noresttherein.oldsql.sql.Select Select]]`[this.`[[net.noresttherein.oldsqsl.sql.RowProduct.Params Params]]`, this.`[[net.noresttherein.oldsql.sql.RowProduct.Row Row]]`]`;
		  * otherwise it will be a
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[this.`[[net.noresttherein.oldsql.sql.RowProduct.Base Base]]`, this.Row]`
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing`
		  * (rather than [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]]), making it statically impossible
		  * to use as a part of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
		  * (and, in particular, inside the `Outer` clause).
		  * @param all the SQL wildcard pseudo expression [[net.noresttherein.oldsql.sql.*$ *]]
		  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.Row Row]]
		  */ //consider: a special mapping for Row
		def select(all: *)(implicit result :CanSelect[thisClause.Complete, sql.*]) :result.Select =
			result(thisClause.self, all)

		/** Creates an SQL ''select'' expression selecting all columns from all relations of the explicit (subselect)
		  * portion of this clause as a `Chain` of relation subjects.
		  * It is equivalent to `select(this.`[[net.noresttherein.oldsql.sql.RowProduct.row row]]`)`.
		  * If this clause is a parameterized [[net.noresttherein.oldsql.sql.RowProduct.TopRow top]] clause,
		  * the result will be a
		  * [[net.noresttherein.oldsql.sql.Select Select]]`[this.`[[net.noresttherein.oldsqsl.sql.RowProduct.Params Params]]`, this.`[[net.noresttherein.oldsql.sql.RowProduct.Row Row]]`]`;
		  * otherwise it will be a
		  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]]`[this.`[[net.noresttherein.oldsql.sql.RowProduct.Base Base]]`, this.Row]`
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing`
		  * (rather than [[net.noresttherein.oldsql.sql.RowProduct.Implicit Implicit]]), making it statically impossible
		  * to use as a part of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]
		  * (and, in particular, inside the `Outer` clause).
		  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.Row Row]]
		  */
		def select()(implicit result :CanSelect[thisClause.Complete, sql.*]) :result.Select =
			result(thisClause.self, sql.*)


		/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses,
		  * and the ''select'' clause consisting of all selectable columns the last relation in this clause.
		  */ //consider: moving to RowProductExtension for consistency
		def selectLast(implicit result :CanSelect[thisClause.Complete, Last[FromLast]]) :result.Select =
			result(self, thisClause.last)

		/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses,
		  * and the ''select'' clause consisting of all selectable columns of the mapping returned by the passed function.
		  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
		  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
		  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
		  * the `Outer` clause).
		  * @param selectClause a function returning any component (including columns and the whole relation mappings)
		  *                     of any of the last relations of this clause.
		  * @return a `SelectSQL` with the specified component as the ''select'' clause and `this.Base` as its
		  *         'outer' type (the ''from'' clause serving as the basis for the expression).
		  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins
		  *                                       in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit explicit]]
		  *                                       section).
		  * @throws IllegalArgumentException if the return expression contains a subexpression of a not supported type.
		  */
		def selectLast[E](selectClause :Last[FromLast] => E)
		                 (implicit result :CanSelect[thisClause.Complete, E]) :result.Select =
			result(self, selectClause(thisClause.last))
	}




	/** A facade to a ''from'' clause of type `F`, providing access to mappings for all relations listed in its type.
	  * The `Origin` type of every returned `M[O] <: MappingAt[O]` instance (and, by transition, its components)
	  * is the generalized super type of `F` formed by replacing all mappings preceding `M` in its type definition
	  * with the wildcard clause (`RowProduct` or `GroupByClause`), joined with `M` using `AndFrom`.
	  * All following joins are generalized, abstracting over aliases, actual join used and preserving only
	  * the distinction between joins, subselects and params. As the result, the mapping `M` becomes the first mapping
	  * in the origin clause, and the number of all mappings defines the index of the mapping in this clause
	  * (when counting from the right). There is an implicit conversion available from each mapping `M`
	  * with the origin type `O` of that form into an `SQLExpression[O, GlobalScope, M#Subject]`,
	  * so the components of the root mapping can be used directly inside SQL expressions. The greatest lower bound
	  * of all such `Origin` types is `this.Generalized`, which makes all SQL expressions based on them be upper bound
	  * by `SQLExpression[Generalized, GlobalScope, _]`.
	  */ //todo: implicitNotFound on all implicit parameters will be very helpful
	implicit class JoinedMappings[+F <: RowProduct](
	                             /** The `RowProduct` with the accessed relations. Note that inside factory methods
	                               * for `F` it might not be fully initialized - in that case only the `left`
	                               * and `right`/`last` properties are initialized and accessing any of its methods
	                               * yields undefined results.
	                               */
	                             val thisClause :F
		) extends AnyVal
	{
		/** A facade to this `RowProduct` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def mappings :JoinedMappings[F] = this

		/** A facade to this `RowProduct` providing access to all relations it consists of. The relations are
		  * returned as `JoinedRelation` SQL expressions parameterized with any of the mapping types listed
		  * by this clause and a supertype of this clause as the base for the expression.
		  */
		@inline def relations :JoinedRelations[F] = new JoinedRelations(thisClause)

		/** A facade to the ungrouped portion of `F`, that is the ''actual'' ''from'' clause of the query.
		  * This assumes that `F <: `[[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]], that is
		  * it is either in the form of
		  * `D `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` G1 `[[net.noresttherein.oldsql.sql.By By]]` G2 ... By G3`
		  * (possibly interspersed by [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] parameters, it is
		  * a directly aggregated clause [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[D]` for selecting
		  * an aggregate expression over all its rows.
		  */ //todo: the implicit param prevents calls to apply on the result
		@inline def ungrouped[D <: FromSome](implicit grouped :D GroupedUnder F) :JoinedMappings[D] =
			new JoinedMappings[D](grouped(thisClause))


		/** Returns the `Mapping` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * @param label a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def labeled[A <: Label](label :A)(implicit get :ByLabel[F, thisClause.Generalized, A]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * Same as `this `[[net.noresttherein.oldsql.sql.RowProduct.JoinedMappings.labeled labeled]]` label`.
		  * @param label a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def :@[A <: Label](label :A)(implicit get :ByLabel[F, thisClause.Generalized, A]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last relation with type `E` as the mapping subject. */
		def apply[E](implicit get :BySubject[F, thisClause.Generalized, E]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByType[F, thisClause.Generalized, M]) :M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last relation given an alias `A`.
		  * These are relations the join for which has been followed by the call to
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as NonEmptyRow.as]] method:
		  * `From(Rangers) join Hamsters as "heroes" :From[Rangers] InnerJoin Hamsters As "heroes"`
		  * @param alias a `String` literal used as the alias of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.As]]
		  */
		def apply[A <: Label](alias :A)(implicit get :ByAlias[F, thisClause.Generalized, A]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :ByIndex[F, thisClause.Generalized, N]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the relation at the specified position in this clause.
		  * The index is a zero-based integer given as an (implicit) evidence class attesting to the existence
		  * of a table/relation with the stated mapping type at described position in `F`.
		  * An instance of `TableOffset` can be summoned by providing both the correct mapping type `M`
		  * and super type `O` of this clause ([[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type
		  * of the join with the specific relation, used as its `Origin` type). Alternatively,
		  * each relation carries a `TableOffset` describing its position as its
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]] property. This makes this method
		  * the most versatile table accessor, as it doesn't require any actual information about `F` and
		  * is the easiest to use from generic code.
		  */
		def apply[O >: F <: RowProduct, M[A] <: MappingAt[A]](position :RelationOffset[O, M]) :M[O] =
			thisClause.fullTableStack(position.index).mapping.asInstanceOf[M[O]]

		/** Returns the `Mapping` instance for the last relation in this clause. */
		def apply()(implicit get :ByIndex[F, thisClause.Generalized, -1]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last relation in this clause. */
		def last(implicit get :ByIndex[F, thisClause.Generalized, -1]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the second-last relation in this clause. */
		def prev(implicit get :ByIndex[F, thisClause.Generalized, -2]) :get.M[get.O] =
			get(thisClause.generalized).mapping


		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByParamType[F, thisClause.Generalized, X]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `UnboundParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, thisClause.Generalized, N]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		/** Returns the `Mapping` instance for the `n`-th unbound parameter.
		  * The returned parameter is the same as the one at the `n`-th position in `F#FullRow`. In particular,
		  * any `JoinParam` joins present under the grouped section of a `GroupBy` clause, which correspond
		  * to unavailable relations, are not excluded from indexing: any such relations will remain unavailable,
		  * creating a gap between available indices. Indexing is based on the length of
		  * the [[net.noresttherein.oldsql.sql.RowProduct.Params Params]] chain: the benefit is that the clause doesn't
		  * have to be concrete if it starts with a [[net.noresttherein.oldsql.sql.RowProduct.ParamlessRow ParamlessFrom]],
		  * as long as the accessed parameter is in the instantiated portion of the clause.
		  * @param n An `Int` literal specifying the position of the requested parameter with regard to other parameters.
		  *          If non-negative, counting goes from left to right, starting with zero;
		  *          otherwise it goes right to left, starting with `-1`.
		  */
		def ?[N <: Numeral](n :N)(implicit get :ByParamIndex[F, thisClause.Generalized, N]) :get.M[get.O] =
			get(thisClause.generalized).mapping

		//methods below are copy&pasted without changes to JoinedRelations

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression using this object as its ''from''
		  * clause. The explicit list of relations in the clause is initialized with the table given as a `Table`
		  * object and can be further expanded by joining with additional tables. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause instead of `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param table a database table (including views and adapted ''selects'') exposing the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Table[M])
		                (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
				:thisClause.FromTable[T] =
			{ val res = thisClause.from(table); res }  //decouple type inference from the result type

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression using this object as its ''from''
		  * clause. The explicit list of relations in the clause is initialized with the table given as a `StaticTable`
		  * object and can be further expanded by joining with additional tables. The name of the table, given
		  * as the string literal type parameter of the argument, is automatically used for its
		  * [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause instead of a `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param table a database table (including views and adapted ''selects''), exposing the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
		                (table :StaticTable[A, M])
		                (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
				:thisClause.FromTable[T] As A =
			{ val res = thisClause.from(table); res }

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * for empty 'outer' clauses, creating another 'outer' clause, forgoing the `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param other a non subselect `RowProduct` listing relations which should be appended to this clause
		  *              (i.e. joined, preserving the order).
		  * @return `clause.`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *         if `F` is not empty and `R` otherwise. The result conforms to
		  *         `clause.`[[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]]
		  *         (for non empty clauses) and
		  *         [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[R <: NonEmptyRow with GroundRow](other :R) :thisClause.FromSubselect[R] =
			thisClause.from(other)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The ''explicit'' list of relations in the clause is initialized with the explicit portion
		  * of the `RowProduct` given as the argument and can be further expanded by joining with additional relations.
		  * Its ''implicit'' portion becomes this relation. The created clause is represented
		  * as a linearization of the two parts: this clause, followed by a `Subselect` pseudo join linking the
		  * first relation following the last `Subselect` (or `Dual`, if `F =:= Dual`) of `R`, with the rest
		  * of the relations following suit. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * @param other a subselect clause of some clause expanded by this clause.
		  * @return `clause.`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *         if `F` is not empty and `R` otherwise. The result conforms to
		  *         `clause.`[[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]] and
		  *         [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def fromSubselect[R <: NonEmptyRow]
		                         (other :R)(implicit expansion :other.Implicit ExpandedBy thisClause.Generalized)
				:thisClause.FromSubselect[R] =
			thisClause.fromSubselect(other)


		override def toString :String = thisClause.toString
	}



	/** A facade to a ''from'' clause of type `F`, providing access to all relations listed in its type as
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] SQL expressions, which can be used,
	  * either directly or through their components, as part of other SQL expressions - in particular `SQLBoolean`
	  * filters based on the `clause.Generalized`. The `RowProduct` type parameter of every returned relation
	  * is the generalized super type of `F` formed by replacing all mappings preceding `M` in its type definition
	  * with the wildcard `RowProduct`, joined with `M` using `AndFrom`. All following joins are generalized,
	  * abstracting over actual join used and preserving only the distinction between joins, subselects and params.
	  * As the result, the mapping `M` becomes the first mapping in the origin clause, and the number of all mappings
	  * defines the index of the mapping in this clause (when counting from the right). Thus, the least upper bound
	  * of all returned relations is `JoinedRelation[this.Generalized, _]` and all expressions built using them
	  * will also be based on `this.Generalized`
	  */
	class JoinedRelations[+F <: RowProduct]
	                      /** The `RowProduct` with the accessed relations. Note that inside factory methods for `F`
	                        * it might not be fully initialized - in that case only the `left` and `right`/`last`
	                        * properties are initialized and accessing any of its methods yields undefined results.
	                        */
	                     (val thisClause :F)
        extends AnyVal
	{
		/** A facade to this `RowProduct` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def entity :JoinedMappings[F] = new JoinedMappings[F](thisClause)

		/** A facade to the ungrouped portion of `F`, that is the ''actual'' ''from'' clause of the query.
		  * This assumes that `F <: `[[net.noresttherein.oldsql.sql.AggregateClause AggregateClause]], that is
		  * it is either in the form of
		  * `D `[[net.noresttherein.oldsql.sql.GroupBy GroupBy]]` G1 `[[net.noresttherein.oldsql.sql.By By]]` G2 ... By G3`
		  * (possibly interspersed by [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] parameters, it is
		  * a directly aggregated clause [[net.noresttherein.oldsql.sql.Aggregated Aggregated]]`[D]` for selecting
		  * an aggregate expression over all its rows.
		  */
		@inline def ungrouped[D <: FromSome](implicit grouped :D GroupedUnder F) :JoinedRelations[D] =
			new JoinedRelations[D](grouped(thisClause))


		/** Returns the `JoinedRelation` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * @param label a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def labeled[A <: Label](label :A)(implicit get :ByLabel[F, thisClause.Generalized, A]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * Same as `this `[[net.noresttherein.oldsql.sql.RowProduct.JoinedRelations.labeled labeled]]` label`.
		  * @param label a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def :@[A <: Label](label :A)(implicit get :ByLabel[F, thisClause.Generalized, A]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last relation with type `E` as the mapping subject. */
		def apply[E](implicit get :BySubject[F, thisClause.Generalized, E]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByType[F, thisClause.Generalized, M]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last relation given an alias `A`.
		  * These are relations the join for which has been followed by the call to
		  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as NonEmptyRow.as]] method:
		  * `From(Rangers) join Hamsters as "heroes" :From[Rangers] InnerJoin Hamsters As "heroes"`
		  * @param alias a `String` literal used as the alias of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.sql.RowProduct.As]]
		  */
		def apply[A <: Label](alias :A)(implicit get :ByAlias[F, thisClause.Generalized, A]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :ByIndex[F, thisClause.Generalized, N]) :get.T[get.O] =
			get(thisClause.generalized)


		/** Returns the `JoinedRelation` instance for the relation at the specified position in this clause.
		  * The index is a zero-based integer given as an (implicit) evidence class attesting to the existence
		  * of a table/relation with the stated mapping type at the described position in `F`.
		  * An instance of `TableOffset` can be summoned by providing both the correct mapping type `M`
		  * and super type `O` of this clause ([[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type
		  * of the join with the specific relation, used as its `Origin` type). Alternatively,
		  * each relation carries a `TableOffset` describing its position as its
		  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.position position]] property. This makes this method
		  * the most versatile table accessor, as it doesn't require any actual information about `F` and
		  * is the easiest to use from generic code.
		  */
		def apply[O >: F <: RowProduct, M[A] <: MappingAt[A]](position :RelationOffset[O, M]) :position.Rel[O] =
			thisClause.fullTableStack(position.index).asInstanceOf[position.Rel[O]]


		/** Returns the `JoinedRelation` instance for the last relation in this clause. */
		def apply()(implicit get :ByIndex[F, thisClause.Generalized, -1]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last relation in this clause. */
		def last(implicit get :ByIndex[F, thisClause.Generalized, -1]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the second-last relation in this clause. */
		def prev(implicit get :ByIndex[F, thisClause.Generalized, -2]) :get.T[get.O] =
			get(thisClause.generalized)


		/** Returns the `JoinedRelation` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByParamType[F, thisClause.Generalized, X]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `UnboundParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, thisClause.Generalized, N]) :get.T[get.O] =
			get(thisClause.generalized)

		/** Returns the `JoinedRelation` instance for the `n`-th unbound parameter.
		  * The returned parameter is the same as the one at the `n`-th position in `F#FullRow`. In particular,
		  * any `JoinParam` joins present under the grouped section of a `GroupBy` clause, which correspond
		  * to unavailable relations, are not excluded from indexing: any such relations will remain unavailable,
		  * creating a gap between available indices.
		  */
		def ?[N <: Numeral](n :N)(implicit get :ByParamIndex[F, thisClause.Generalized, N]) :get.T[get.O] =
			get(thisClause.generalized)

		//straight copy&paste from JoinedMappings for convenience

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression using this object as its ''from''
		  * clause. The explicit list of relations in the clause is initialized with the table given as a `Table`
		  * object and can be further expanded by joining with additional tables. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause instead of `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param table a database table (including views and adapted ''selects'') exposing the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Table[M])
		                (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
				:thisClause.FromTable[T] =
			{ val res = thisClause.from(table); res } //decouple type inference from the result type

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression using this object as its ''from''
		  * clause. The explicit list of relations in the clause is initialized with the table given as a `StaticTable`
		  * object and can be further expanded by joining with additional tables. The name of the table, given
		  * as the string literal type parameter of the argument, is automatically used for its
		  * [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause instead of a `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param table a database table (including views and adapted ''selects''), exposing the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
		                (table :StaticTable[A, M])
		                (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
				:thisClause.FromTable[T] As A =
			{ val res = thisClause.from(table); res }

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `RowProduct`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]]
		  * extension method available for any `RowProduct`; the difference is that this method works also
		  * for empty 'outer' clauses, creating another 'outer' clause, forgoing the `Subselect` join in the result.
		  * As a trade off, it has a more complex return type, expressed through the `Self` type of this clause rather
		  * than simply the static type of this clause.
		  * @param other a non subselect `RowProduct` listing relations which should be appended to this clause
		  *              (i.e. joined, preserving the order).
		  * @return `clause.`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *         if `F` is not empty and `R` otherwise. The result conforms to
		  *         `clause.`[[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]] and
		  *         [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[R <: GroundFromSome](other :R) :thisClause.FromSubselect[R] = thisClause.from(other)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The ''explicit'' list of relations in the clause is initialized with the explicit portion
		  * of the `RowProduct` given as the argument and can be further expanded by joining with additional relations.
		  * Its ''implicit'' portion becomes this relation. The created clause is represented
		  * as a linearization of the two parts: this clause, followed by a `Subselect` pseudo join linking the
		  * first relation following the last `Subselect` (or `Dual`, if `F =:= Dual`) of `R`, with the rest
		  * of the relations following suit. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' and ''select'' clauses. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where()]] method.
		  *
		  * @param other a subselect clause of some clause expanded by this clause.
		  * @return `clause.`[[net.noresttherein.oldsql.sql.RowProduct.AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *         if `F` is not empty and `R` otherwise. The result conforms to
		  *         `clause.`[[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect DirectSubselect]] and
		  *         [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def fromSubselect[R <: FromSome]
		                         (other :R)(implicit expansion :other.Implicit ExpandedBy thisClause.Generalized)
				:thisClause.FromSubselect[R] =
			thisClause.fromSubselect(other)


		override def toString :String = thisClause.toString
	}



	implicit def lastRelationOf[U <: NonEmptyRow, M[O] <: MappingAt[O]]
			:LastTableOf[LastBound[JoinedRelation.Of[M]#T, U, M]]
				{ type FromLast = U; type LastMapping[O] = M[O]; type Last[O <: RowProduct] = JoinedRelation[O, M] } =
		LastBound.asInstanceOf[LastTableOf[LastBound[JoinedRelation.Of[M]#T, U, M]] {
			type FromLast = U; type LastMapping[O] = M[O]; type Last[O <: RowProduct] = JoinedRelation[O, M]
		}]

	/** Implicit evidence that the clause `F` is a ''direct'' expansion of `P`, that is type `F` results
	  * from applying some type constructor to `P`. This covers both the [[net.noresttherein.oldsql.sql.Expanded Expanded]]
	  * class hierarchy which actually add a relation to the clause `P` and
	  * the [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]] hierarchy of clauses which only modify
	  * the adapted clause without adding new relations. Hence, the `expansion` property always represents
	  * the length of `0` or `1`. Additionally, it provides type constructor(s) allowing substituting `P` with other
	  * clauses. This class is used by various other implicits in order to recursively deconstruct
	  * the type `F`, for example for the relation accessor methods, and is introduced to allow their usage with
	  * any future or custom `RowProduct` implementations which provide an implicit value of this class in their
	  * companion object.
	  * @tparam F the 'whole' clause expanding (in the sense of
	  *           `P` [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf PrefixOf]] `F`) the clause `P`.
	  * @tparam P the largest subclause of `F` which doesn't equal `F` - typically the `RowProduct` type argument
	  *           provided for `F`.
	  * @tparam U the upper bound on the clauses accepted by `F`'s type constructor in place of `P`.
	  *           If `F =:= C[P]` for some `C[X <: U]`, then `F <: C[U]` and it is the highest upper bound on `F`
	  *           which preserves the type constructor `C` intact.
	  */ //todo: move to mechanics; rename to ClauseDecomposition; try to make it invariant in F
	@implicitNotFound("I don't know how to extract a prefix clause ${P} (with an upper bound ${U}) from ${F}.\n" +
	                  "Missing implicit RowDecomposition[${F}, ${P}, ${U}].")
	trait RowDecomposition[-F <: RowProduct, P <: U, U <: RowProduct] {

		/** The type constructor of `F`, accepting a prefix clause. This is the generic version of `S[A]`,
		  * accepting any type conforming to `U`, but without the lower bound of `F` in the result.
		  */
		type E[+A <: U] <: RowProduct

		/** The type constructor which, when applied to `P`, produces the type `F`.
		  * In reality, it is the same type as `E[A]`, but the lower bound on the type parameter allows it
		  * the lower bound of `F`.
		  */ //can't be <: E[A] because not F <: E[A]
		type S[+A >: P <: U] >: F <: RowProduct

		/** a `PrefixOf` instance representing the application of the type constructor
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowDecomposition.S S]] to some supertype of the decomposed
		  * prefix `P` of the clause `F`, which this instance is the type class of. It has the length of `1` for
		  * [[net.noresttherein.oldsql.sql.Expanded Expanded]] subtypes and `0` for
		  * [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]] subtypes.
		  */
		def prefix[A >: P <: U] :A PrefixOf S[A]

		/** a `PrefixOf` instance representing the application of the type constructor
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowDecomposition.S S]] to some arbitrary ''from'' clause `A`.
		  * It has the length of `1` for [[net.noresttherein.oldsql.sql.Expanded Expanded]] subtypes and `0` for
		  * [[net.noresttherein.oldsql.sql.DecoratedRow DecoratedRow]] subtypes.
		  */
		def expansion[A <: U] :A PrefixOf E[A]

		/** Extracts the prefix clause `P` expanded by the associated ''from'' clause `F`. */
		def unapply(clause :F) :P

		/** A type class for a supertype of the represented ''from'' clause, resulting from the application
		  * of the type constructor specific to this clause to some supertype `A` of `P`.
		  */
		def upcast[A >: P <: U] :RowDecomposition[S[A], A, U]

		/** A type class for a ''from'' clause sharing the top type constructor
		  *  [[net.noresttherein.oldsql.sql.RowProduct.RowDecomposition.E E]] with this clause `F`.
		  */
		def cast[A <: U] :RowDecomposition[E[A], A, U]

	}



	//todo: remove it. It's only used by Parameterization, which can just as well use RowDecomposition
    @implicitNotFound("I don't know how to extract a prefix ${P} (with an upper bound ${U}) from FROM clause ${F}.\n" +
	                  "Missing implicit RowComposition[${F}, ${P}, ${U}].")
	trait RowComposition[F <: RowProduct, P <: U, U <: RowProduct] extends RowDecomposition[F, P, U] {
		/** Create a new ''from'' clause by replacing the 'prefix' `P` in `template` with `F`.
		  * The created instance will contain no filter condition over what already exists in the clause `C`.
		  */
		def apply[C <: U](template :F, clause :C) :E[C]
	}





	/** A proof that the ''from'' clause `F` is a part of the ''from'' clause `E` and represents the 'same'
	  * SQL ''select''. This holds if `F ExpandedBy E` and there is no `Subselect` join present
	  * in the expansion. The implication is that `E` contains all the relations listed in `F`, with a possible addition
	  * of extra relations appended (by joining) or 'prepended' to it (by substituting a wildcard prefix type
	  * of `F` with its subtype), while at the same time not being a subselect expression of `F`.
	  * Note that subtypes of [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] are not considered here
	  * 'the same select' as their [[net.noresttherein.oldsql.sql.GroupByClause.fromClause fromClause]],
	  * because the tables from the discrete ''select'' are not available for expressions
	  * based on the ''group by'' clause.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf]]
	  */ //consider: renaming to SubClauseOf //SameSelectAs//ExpandedBy; moving the bunch to mechanics
	@showAsInfix //todo: make it a refined subtype of ExpandedBy; we could use a single bit in length for 'isPartOf'
	@implicitNotFound("The FROM clause ${F} is not a part of the same SELECT as the clause ${E}.")
	class PartOf[+F <: RowProduct, -E <: RowProduct] private[RowProduct] (val lengthDiff :Int) extends AnyVal {
		//todo: casting ourselves to types such as (F with NonEmptyRow) ExpandedBy (E with NonEmptyRow)
		/** An `F` [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] `E` instance representing the
		  * same expansion of the clause `F`. */
		implicit def asExpandedBy :F ExpandedBy E = new ExpandedBy(lengthDiff)


		/** A transitive proof that a clause expanding `E` with a single relation (mapping) also extends `F`. */
		@inline def expand[R[O] <: MappingAt[O]] :F PartOf (E NonSubselect R) = new PartOf(lengthDiff + 1)

		/** A transitive proof that a clause expanded by `F` with a single relation (mapping) is also expanded by `E`. */
		@inline def expandFront[L <: RowProduct, R[O] <: MappingAt[O]]
		                       (implicit front :F <:< (L NonSubselect R)) :L PartOf E =
			new PartOf(lengthDiff + 1)

		/** A transitive proof that if `E PartOf C`, then also `F PartOf C` (appending to this witness). */
		@inline def expand[C <: RowProduct](implicit next :E PartOf C) :F PartOf C =
			new PartOf[F, C](lengthDiff + next.lengthDiff)

		/** A transitive proof that if `C PartOf F`, then also `C PartOf E` (prepending to this witness). */
		@inline def expandFront[C <: RowProduct](implicit front :C PartOf F) :C PartOf E =
			new PartOf(front.lengthDiff + lengthDiff)

		/** A transitive proof that if `E PartOf C`, then also `F PartOf C` (appending to this witness). */
		@inline def +[C <: RowProduct](next :E PartOf C) :F PartOf C =
			new PartOf[F, C](lengthDiff + next.lengthDiff)

		/** A transitive proof that a decorated clause `E` is still an expansion of `F`. */
		@inline def wrap :F PartOf ExpandingDecorator[E] = new PartOf[F, ExpandingDecorator[E]](lengthDiff)

		/** A transitive proof that if `F <: ExpandingDecorator[C]`, then `C PartOf E` without any length change. */
		@inline def unwrapFront[C <: FromSome](implicit front :F <:< ExpandingDecorator[C]) :C PartOf E =
			new PartOf(lengthDiff)


		override def toString :String = lengthDiff.toString + " tables"
	}



	object PartOf {
		@inline def apply[F <: RowProduct, E <: RowProduct](implicit ev :F PartOf E) :F PartOf E = ev

		implicit def itself[F <: RowProduct] :F PartOf F = new (F PartOf F)(0)

		implicit def expand[F <: RowProduct, L <: RowProduct, R[A] <: MappingAt[A]]
		                   (implicit ev :F PartOf L) :F PartOf (L NonSubselect R) =
			new PartOf(ev.lengthDiff + 1)

		implicit def wrap[F <: RowProduct, E <: RowProduct](implicit ev :F PartOf E) :F PartOf ExpandingDecorator[E] =
			new PartOf[F, ExpandingDecorator[E]](ev.lengthDiff)

		implicit def group[F <: RowProduct, O <: RowProduct, E <: FromSome, R[A] <: MappingAt[A]]
		                  (implicit outer :O OuterClauseOf E, part :F PartOf O) :F PartOf (E GroupBy R) =
			new PartOf(part.lengthDiff + 1)

		implicit def aggregate[F <: RowProduct, O <: RowProduct, E <: FromSome]
		                      (implicit outer :O OuterClauseOf E, part :F PartOf O) :F PartOf Aggregated[E] =
			new PartOf(part.lengthDiff)
	}






	/** A proof that the ''from'' clause `E` is an ''expansion'' of the clause `F`, meaning `E` contains all relations
	  * listed by type `F` as a consecutive, ordered sequence. If `F` is a ''complete'' clause
	  * (starting with `Dual`/`From`), then `F` is a supertype of some prefix of `E`. More formally,
	  * `F ExpandedBy E` if one of the following conditions hold:
	  *   - `E <: F`,
	  *   - `E <: ExpandingClause[S]` for some `S <: RowProduct` such that `F ExpandedBy S`,
	  *   - `E <: F#Aggregate` (and `F <: FromSome`),
	  *   - `F =:= Dual` and `E <: F#Aggregate` for some `F <: TopFromSome`.
	  *
	  * It means that a value of type `F` can be extracted from `E` by a sequence of operations:
	  *   - taking the inner clause of an [[net.noresttherein.oldsql.sql.DecoratedRow.ExpandingDecorator ExpandingDecorator]];
	  *   - taking the left side of an [[net.noresttherein.oldsql.sql.Expanded Expanded]] clause;
	  *   - taking the outer clause of a [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] clause;
	  *   - taking the outer clause of an [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] clause.
	  *
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has a wildcard prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual expansion,
	  * rather than a proof of some clause `e :E` containing all the relations of `F` unless `F` is complete.
	  * It does however specify this when only the mapping lists in the static types are compared and can
	  * be understood as a stronger version of the statement that any dependency on the relations from `F`
	  * can be satisfied with the clause `E`. The primary use of this class is in conversions
	  * from `SQLExpression[F, S, T]` to `SQLExpression[E, S, T]`.
	  *
	  * A non-obvious implication of being contravariant in the expanding type `E` is that if `F` is incomplete,
	  * this instance is also a witness that subtypes of `E` which replace the initial `RowProduct` with a join list
	  * are likewise expansions of `F`, in this way covering both expansions from the back (right side) and front
	  * (left side). The purpose of this class is to allow `SQLExpression` instances based on one clause `F`
	  * to be converted into isomorphic expressions based on a second clause `E`, as long as all mappings
	  * in `F`'s static type form a continuous subsequence of mappings listed in `E`. Note that `SQLExpression`
	  * is contravariant in its clause type parameter, meaning that a wildcard `RowProduct` occurring
	  * at the start of the ''from'' clause type parameter doesn't 'hide' any mappings used by that expression,
	  * but to the contrary, serves to signify that it doesn't rely in any form on that portion of the clause
	  * it is based on. As a result, this proof is indeed a valid representation that such a conversion from `F` to `E`
	  * is possible for any `SQLExpression`.
	  *
	  * Due to this contravariance in `E`, this isn't any form of a generalized subtyping relation and should be used
	  * when comparing the mapping lists of the two clauses, particularly when both are 'input' (type) parameters.
	  * When preserving of the exact type of `F` within `E` is important,
	  * [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf PrefixOf]] should be used instead, which serves a similar
	  * function, but is invariant in its type parameters.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PrefixOf]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	//todo: rename to IncludedIn/UsedBy; problem: Expanded clause and various expand methods. Rename that to Include?
	//  Could also be DependentOn, DependantOf SubClauseOf, UsedBy, AvailableIn, ProvidedBy unless we want to reserve this name for evidence
	//todo: make it a supertype of PartOf (in Scala 3, most likely, though it could be made by casting member types)
	@showAsInfix
	@implicitNotFound("The FROM clause ${F} is not expanded by the clause ${E} (ignoring join kinds).")
	class ExpandedBy[+F <: RowProduct, -E <: RowProduct] private[RowProduct](val length :Int) extends AnyVal {
		type Self[+A <: RowProduct, -B <: RowProduct] <: A ExpandedBy B
		//todo: we really need a way to say that expansion of a non empty clause is a non empty clause at the very least.
		//todo: doesn't compile as of now, but a fix is reportedly coming
//		type Bound <: RowProduct
//		type Extension[+T <: Bound] <: RowProduct
//
//		def expansion[T <: Bound] :T ExpandedBy Extension[T] = new ExpandedBy[T, Extension[T]](length)
//		def superPrefix[] :F PrefixOf (_ <: E) = new PrefixOf(length)
		def asPrefixOf[C <: E] :(_ <: F) PrefixOf C = new PrefixOf(length)
		def superPrefix[C >: F <: RowProduct] :C PrefixOf (_ >: E <: RowProduct) = new PrefixOf(length)

		/** A transitive proof that a clause expanding `E` with a single relation (mapping) also extends `F`. */
		@inline def expand[R[O] <: MappingAt[O]] :F ExpandedBy (E Expanded R) = new ExpandedBy(length + 1)

		/** A transitive proof that a clause expanded by `F` with a single relation (mapping) is also expanded by `E`. */
		@inline def expandFront[L <: RowProduct, R[O] <: MappingAt[O]]
		                        (implicit front :F <:< (L Expanded R)) :L ExpandedBy E =
			new ExpandedBy(length + 1)

		/** A transitive proof that if `E ExpandedBy C`, then also `F ExpandedBy C` (appending to this witness). */
		@inline def expand[C <: RowProduct](implicit next :E ExpandedBy C) :F ExpandedBy C =
			new ExpandedBy[F, C](length + next.length)

		/** A transitive proof that if `C ExpandedBy F`, then also `C ExpandedBy E` (prepending to this witness). */
		@inline def expandFront[C <: RowProduct](implicit front :C ExpandedBy F) :C ExpandedBy E =
			new ExpandedBy(front.length + length)

		/** A transitive proof that if `E ExpandedBy C`, then also `F ExpandedBy C` (appending to this witness). */
		@inline def +[C <: RowProduct](next :E ExpandedBy C) :F ExpandedBy C =
			new ExpandedBy[F, C](length + next.length)

		/** A transitive proof that a decorated clause `E` is still an expansion of `F`. */
		@inline def wrap :F ExpandedBy ExpandingDecorator[E] = new ExpandedBy[F, ExpandingDecorator[E]](length)

		/** A transitive proof that if `F <: ExpandingDecorator[C]`, then `C ExpandedBy E` without any length change. */
		@inline def unwrapFront[C <: FromSome](implicit front :F <:< ExpandingDecorator[C]) :C ExpandedBy E =
			new ExpandedBy(length)
		//todo: aggregates

		override def toString :String = length.toString + " tables"
	}



	sealed abstract class ExpandedByFromPartOf {
		implicit def part[F <: RowProduct, E <: RowProduct](implicit part :F PartOf E) :F ExpandedBy E =
			new ExpandedBy(part.lengthDiff)
	}

	object ExpandedBy extends ExpandedByFromPartOf {
		@inline def apply[F <: RowProduct, E <: RowProduct](implicit ev :F ExpandedBy E) :F ExpandedBy E = ev

		def generalized(from :RowProduct) :from.Implicit ExpandedBy from.Generalized = new ExpandedBy(from.size)

		def self(from :RowProduct) :from.Outer ExpandedBy from.Self = new ExpandedBy(from.size)


		implicit def itself[F <: RowProduct] :F ExpandedBy F = new (F ExpandedBy F)(0)

		implicit def expand[F <: RowProduct, L <: RowProduct, R[A] <: MappingAt[A]]
		                   (implicit ev :F ExpandedBy L) :F ExpandedBy (L Expanded R) =
			new ExpandedBy(ev.length + 1)

		implicit def wrap[F <: RowProduct, E <: RowProduct]
		                 (implicit ev :F ExpandedBy E) :F ExpandedBy ExpandingDecorator[E] =
			new ExpandedBy[F, ExpandingDecorator[E]](ev.length)

		implicit def group[F <: RowProduct, O <: RowProduct, E <: FromSome, R[A] <: MappingAt[A]]
		                  (implicit outer :O OuterClauseOf E, prefix :F ExpandedBy O) :F ExpandedBy (E GroupBy R) =
			new ExpandedBy(prefix.length + 1)

		implicit def aggregate[F <: RowProduct, O <: RowProduct, E <: FromSome]
		                      (implicit outer :O OuterClauseOf E, prefix :F ExpandedBy O) :F ExpandedBy Aggregated[E] =
			new ExpandedBy(prefix.length)

	}






	/** A proof that the ''from'' clause  `F` is a prefix of the clause of `E`.
	  * A clause `F` is a prefix of `E` if one of the conditions hold:
	  *   - `E =:= F`,
	  *   - `E =:= D[G]` for some `D[C] <: ExpandingDecorator[C], G <: RowProduct` such that `F PrefixOf G`,
	  *   - `E =:= G J _` for some `J[+L, R[O]] <: Expanded[L, R], G <: RowProduct` such that `F PrefixOf G`,
	  *   - `E =:= G GroupBy _` for some `G <: F#DirectSubselect`,
	  *   - `E =:= Aggregated[G]` for some `G <: F#DirectSubselect`,
	  *   - `F =:= Dual` and `E <: O GroupBy _` or `E <: Aggregated[O]` for some `O <: TopFromSome`,
	  *
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has a wildcard prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual expansion,
	  * rather than a proof of `E` containing all the relations of `F` unless `F` is complete.
	  *
	  * The difference from the similar witness [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]]
	  * is that this type is invariant in both its parameters and hence attests that `F` occurs exactly in this form
	  * as a part of type `E`. It is primarily used in conjunction with invariant
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]], to make sure the type parameters
	  * are preserved exactly for the purpose of indexing, or in operations where one of its type parameters is a part
	  * of the output. In contrast, `F ExpandedBy E` is covariant/contravariant and used primarily
	  * when there is a need to confirm that `E` contains all the relations listed in `F`.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.PartOf]]
	  */
	@showAsInfix
	@implicitNotFound("The FROM clause ${F} is not a prefix of the clause ${E}.")
	class PrefixOf[F <: RowProduct, E <: RowProduct] private[sql](val lengthDiff :Int) extends AnyVal {

		/** An `F` [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] `E` instance representing the
		  * same expansion of the clause `F`. `ExpandedBy`, unlike `PrefixOf`, is covariant/contravariant in its
		  * first/second parameter.
		  */
		@inline def expansion :F ExpandedBy E = new ExpandedBy(lengthDiff)

		/** A transitive proof that if `E PrefixOf C`, then also `F PrefixOf C` (concatenation of the two witnesses). */
		@inline def +[C <: RowProduct](next :E PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](lengthDiff + next.lengthDiff)

		/** A transitive proof that if `E PrefixOf C`, then also `F PrefixOf C` (concatenation of the two witnesses). */
		@inline def expand[C <: RowProduct](implicit next :E PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](lengthDiff + next.lengthDiff)

		/** A transitive proof that `F PrefixOf (E J T)` for any `Expanded` subtype `J` and mapping type constructor `T`. */
		@inline def expand[J[+L <: E, R[O] <: T[O]] <: L Expanded R, T[O] <: MappingAt[O]] :F PrefixOf (E J T) =
			new PrefixOf[F, E J T](lengthDiff + 1)

		/** A transitive proof that `F` is a prefix of its any direct subselect with a ''group by'' clause. */
		@inline def group[G <: FromSome, R[O] <: MappingAt[O]]
		                 (implicit outer :E OuterClauseOf G) :F PrefixOf (G GroupBy R) =
			new PrefixOf(lengthDiff + 1)

		/** A transitive proof that `F` is a prefix of its any direct aggregated subselect. */
		@inline def aggregate[G <: FromSome](implicit outer :E OuterClauseOf G) :F PrefixOf Aggregated[G] =
			new PrefixOf(lengthDiff)

		/** A transitive proof that `F PrefixOf D[E]` for any ''from'' clause decorator (with no change in length). */
		@inline def wrap[D[B >: E <: E] <: ExpandingDecorator[B]] :F PrefixOf D[E] = new PrefixOf[F, D[E]](lengthDiff)

		override def toString :String = lengthDiff.toString + " tables"
	}



	object PrefixOf {
		@inline def apply[F <: RowProduct, E <: RowProduct](implicit ev :F PrefixOf E) :F PrefixOf E = ev

		def generalized(from :RowProduct) :from.Implicit PrefixOf from.Generalized = new PrefixOf(from.size)

		def self(from :RowProduct) :from.Outer PrefixOf from.Self = new PrefixOf(from.size)


		@inline implicit def PrefixOfNonEmpty[F <: RowProduct, E <: NonEmptyRow](prefix :F PrefixOf E)
				:F PrefixOfNonEmpty E =
			new PrefixOfNonEmpty[F, E](prefix.lengthDiff)

		class PrefixOfNonEmpty[F <: RowProduct, E <: NonEmptyRow] private[PrefixOf](private val diff :Int)
			extends AnyVal
		{
			/** A transitive proof that `F PrefixOf (E As A)` for any alias `A <: Label`. */
			@inline def as[A <: Label] :F PrefixOf (E As A) = new PrefixOf[F, E As A](diff)
		}


		implicit def itself[F <: RowProduct] :F PrefixOf F = new PrefixOf(0)

		//fixme: should either use RowComposition or make RowDecomposition invariant.
		implicit def expand[F <: RowProduct, E <: RowProduct, L <: U, U <: RowProduct]
		                   (implicit decompose :RowDecomposition[E, L, U], prefix :F PrefixOf L) :F PrefixOf E =
			new PrefixOf[F, E](prefix.lengthDiff + decompose.expansion.lengthDiff)

		implicit def group[F <: RowProduct, O <: RowProduct, E <: FromSome, R[A] <: MappingAt[A]]
		                  (implicit outer :O OuterClauseOf E, part :F PrefixOf O) :F PrefixOf (E GroupBy R) =
			new PrefixOf(part.lengthDiff + 1)

		implicit def aggregate[F <: RowProduct, O <: RowProduct, E <: FromSome]
		                      (implicit outer :O OuterClauseOf E, part :F PrefixOf O) :F PrefixOf Aggregated[E] =
			new PrefixOf(part.lengthDiff)

		implicit def alias[F <: RowProduct, E <: NonEmptyRow, A <: Label]
		                  (implicit prefix :F PrefixOf E) :F PrefixOf (E As A) =
			new PrefixOf(prefix.lengthDiff)
	}


	//todo: zipper
}


