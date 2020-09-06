package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainLength}
import net.noresttherein.oldsql.morsels.abacus.{Inc, Negative, Numeral, Positive, Sum}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.GetTable.{ByIndex, ByLabel, ByParamIndex, ByParamName, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFrom, FreeFromSome, JoinedEntities, NonEmptyFrom, PrefixOf, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.{ComponentSQL, JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.UnboundParam.{?:, FromParam, LabeledFromParam, ParamAt, ParamRelation}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.DecoratedFrom.{Alias, DecoratorDecomposition, GenericDecorator}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{ExtendedComposition, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.GroupByAll.ByAll
import net.noresttherein.oldsql.sql.JoinParam.WithParam
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SelectSQL.{SelectAs, SelectColumn}
import net.noresttherein.oldsql.sql.SQLScribe.RemoveParams
import net.noresttherein.oldsql.sql.Using.JoinedRelationSubject






/** In its basic use, a `FromClause` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in a query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] class hierarchy)
  * are defined, providing all non-constant values available to them. It consists of a list of
  * [[net.noresttherein.oldsql.schema.Relation relations]] with associated
  * [[net.noresttherein.oldsql.schema.Mapping Mappings]], together with an optional filter working on those relations,
  * especially any required join conditions. While the individual elements of a clause are referred to often as tables
  * for simplicity, they can not only be arbitrary relations such as other ''selects'', but also synthetic artifacts
  * such as query parameters. It is even possible to use arbitrary mapping components, to be replaced at a later time
  * with references to concrete tables, parameters or constants. As such, it doesn't necessarily represent
  * a valid fragment of a select from the application's schema. In that case it's best thought as a signature containing
  * declarations of terms (mappings and parameters) over which SQL expressions can be defined;
  * hence `SQLExpression` is parameterized with a `FromClause`.
  * {{{}}}
  * The mappings taking part in the query are encoded in the static type of the standard implementation, which
  * builds both the instance and its type by recursively applying various
  * [[net.noresttherein.oldsql.sql.Using Using]] subtypes to a shorter clause, extending it by another relation.
  * In that aspect it is quite similar to general purpose polymorphic tuples or heterogeneous lists such as
  * the ''shapeless'' `HList` and [[net.noresttherein.oldsql.collection.Chain Chain]], replacing its wider functionality
  * with specific application to this task. Just as a `HList` - and all other two-argument type constructors -
  * it can be and is better written using the infix notation: `Dual Join Rangers Join Familiars Join Monsters`.
  * Note that the whole `JoinLike` class hierarchy is left-associative for a more natural left to right reading and writing.
  * Specialized classes for all join kinds exist, as well as special variants for ''from'' clauses of subselects
  * of some outer clause and unbound query parameters. A ''group by'' clause is also represented under this type,
  * with all subtypes having to extend either [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]] or
  * [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]], the latter tree containing in particular
  * the [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] type. Other implementations, not derived from `Using`
  * are also possible, in particular decorators extending [[net.noresttherein.oldsql.sql.DecoratedFrom DecoratedFrom]]
  * which introduce no new relations, but can add other features. As subtypes are also universally covariant
  * regarding their `FromClause` type parameters, any prefix can be always substituted with the abstract `FromClause`
  * supertype, balancing static type checking with a degree of freedom and flexibility promoting code reuse.
  * {{{}}}
  * This type is thus more general than simply an implementation of the actual ''from'' clause of a select:
  * not only does it include the ''where'' clause and the potential ''group by'' clause is incorporated into
  * the class hierarchy, but it also allows represent subselects, by combining the ''from'' clause of the subselect
  * with the relations brought to scope by enclosing selects. This is done by
  * the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join, which linearizes the `FromClause`
  * of the outer select and the ''from'' clause of the subselect by joining them together. Hence, from the point of view
  * of an SQL select expression created from such a clause, only the relations to the right of the last
  * `Subselect` join are a part of the ''from'' clause. Similarly, a presence of
  * the [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] clause 'hides' all preceding relations down to
  * the most recent `Subselect` or the start of clause, which are under grouping and thus unavailable as individual
  * rows.
  * {{{}}}
  * A `FromClause` subtype from the above category is said to be ''complete'' if its definition starts with
  * the [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] type,
  * that is the number of all mappings participating in the join/clause is known (although their types may be abstract).
  * Note that the mapping types may still be abstract and the joins may be some generic base type like
  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]. The opposite is an ''incomplete'' clause, the definition of which
  * starts with the base `FromClause`, one of its generic subclasses which do not expose their prefix clause type,
  * or an abstract type `F &lt;: FromClause`: `FromClause Join Deaths Join Instruments`, which specifies any sequence
  * of relations ending with the `Deaths` and `Instruments`. A ''complete'' clause is called ''concrete'' if
  * all join types in its signature are concrete classes (concrete here meaning any public trait having a dedicated
  * factory in its companion object). Additionally, each `FromClause` subtype defines a `Generalized` member type
  * as its supertype which still allows external functions to correctly discern its role in the whole clause.
  * A ''complete'' clause is then called ''generalized'' if its `Generalized` type is known, that is its static type
  * is a subtype of its `Generalized` type.
  * {{{}}}
  * This trait is a bare bones common upper type, serving at the same time as a wildcard in type definitions meaning
  * 'an arbitrary number of other preceding relations not relevant in the circumstances at hand'. Most functionality
  * is defined in the `Extended` and `AndFrom` subclasses, representing actual non-empty clauses. Some additional
  * generic methods however are made available through implicit conversions in order to benefit from the static self type:
  *   - from any `F &lt;: FromClause`
  *     to [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension FromClauseExtension]], which declares
  *     some accessors, in particular
  *     `relations` to [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] and
  *     `entities` to [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]].
  *   - from non-empty discrete clauses `F &lt;: FromSome` (those without a ''group by'' clause)
  *     to [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]], which define
  *     factory methods for other join types;
  *   - from clauses without a `Subselect` join `F &lt;: OuterFromSome`
  *     to [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension OuterFromExtension]] for adding unbound
  *     query parameters in the form of the [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] synthetic 'joins'.
  * {{{}}}
  * Aside from the above extensions, associated implicits and other helper classes, the companion object
  * defines some base types serving as common upper bounds and several useful type aliases which enforce
  * certain features on `FromClause` instances, such as [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]],
  * [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]] and others.
  *
  * @see [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome]] the base trait for all non-empty clauses
  *      (without a ''group by'' clause).
  * @see [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] the 'link' type extending a preceding discrete clause
  *      by another relation and the base trait for all join kinds.
  * @see [[net.noresttherein.oldsql.sql.Dual Dual]] the standard empty clause used for ''select'' statements
  *      without a ''from'' clause (or 'select ... from Dual' in Oracle) as well as the terminator of non-empty
  *      relation lists.
  * @see [[net.noresttherein.oldsql.sql.From From]] The factory for clauses with a single relation which
  *      is the most common starting point for building larger clauses.
  * @see [[net.noresttherein.oldsql.sql.JoinLike JoinLike]] The supertype for all join kinds as well as subselect clauses.
  * @see [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] a ''group by'' clause under the interface
  *      of a ''from'' clause
  *
  * @author Marcin Mościcki
  */ //consider: renaming to FromWhere
trait FromClause { thisClause =>

	/** Type of the last `Mapping` in this join (the rightmost one) if not empty. `Dual` defines it as `Nothing`. */
	type LastMapping[O] <: MappingAt[O]

	/** Type alias for the last relation in this list as seen from some `FromClause` type `F` containing this instance
	  * in it1s 'tail'. In other words, this projects the type of the last element of this clause to one based
	  * on an extending clause. `JoinedRelation[F, LastMapping]` provides access to the `Mapping` for the relation
	  * together with all its components and is an instance of `SQLExpression[F, LastMapping[_]#Subject]`, meaning
	  * it can be used as part of larger SQL expressions. Notably, `Dual`, containing no relations,
	  * defines this type as `Nothing`.
	  */
	type LastTable[F <: FromClause] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `FromClause` using
	  * the most abstract extension ('join') kind which can still take the place of this type as a parameter in
	  * larger clauses. For non-empty, ''discrete'' clauses it is
	  * `FromClause `[[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
	  * [[net.noresttherein.oldsql.sql.FromClause#LastMapping LastMapping]] - the upper bound on all discrete clauses
	  * containing `LastMapping`. If this clause is not a `AndFrom` subclass but a decorator,
	  * the ''generalized'' supertype of the decorator is used as the wrapper over `FromClause AndFrom LastMapping`.
	  * It is the type this instance's [[net.noresttherein.oldsql.sql.FromClause#last last]] joined relation is based on.
	  * `Dual` defines this type as the most generic `FromClause` to indicate that any expression based on
	  * an empty clause can be used as part of expressions for any other clause.
	  */
	//type FromLast >: this.type <: FromClause //this.type lower bound doesn't hold for decorators
	type FromLast <: FromClause

	/** Given a type constructor for a ''from'' clause accepting a `FromSome` type, apply this type to it
	  * if it conforms to `FromSome`. This is a helper type allowing the declaration of
	  * [[net.noresttherein.oldsql.sql.FromClause#filterNext filterNext]], used in the implementation of
	  * [[net.noresttherein.oldsql.sql.AndFrom#on AndFrom.on]].
	  */
	type FromNext[E[+L <: FromSome] <: FromClause] <: FromClause

	/** The last relation in this clause when treated as a list, if any.
	  * @throws NoSuchElementException if this clause is empty.
	  */
	def last :LastTable[FromLast]

	/** The last relation in this clause as an expression based on some extending clause `E`.
	  * This is equivalent to `last.extend[E]`, but returns the result as the narrowed `LastTable[E]` rather than
	  * `JoinedRelation[E, LastMapping]`. Thus it preserves the `Nothing` return type for empty clauses and allows
	  * its use in the `on` filtering method.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.PrefixOf]]
	  */
	def lastAsIn[E <: FromSome](implicit extension :FromLast PrefixOf E) :LastTable[E]



	/** The super type of this instance used as the basis for its ''where'' clause filter expression (and others).
	  * It is the [[net.noresttherein.oldsql.sql.FromClause#Self Self]] type with all join kinds replaced with their
	  * generalized forms. It abstracts over the exact joins used and focuses only on the relations available to
	  * SQL expressions based on this clause. Every concrete subclass of `FromClause` declares its most abstract
	  * superclasses which still provides distinction necessary to fulfill its intended role in the SQL select.
	  * Its `Generalized` type is then formed recursively, with every `FromClause` subtype which accepts another
	  * `F &lt;: FromClause` type parameter corresponding to some member `val f :F` substituting `f.Generalized` for `F`.
	  * The generalized form of `Dual` is simply `FromClause`, so the `Generalized` type of any clause will match also
	  * clauses which prepend relations to it at the front. The filter for the associated ''where'' clause and
	  * all joined relations accessed through [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]]
	  * conform to `SQLExpression[Generalized, _]`.
	  * Clauses with the same `Generalized` type are considered interchangeable in most cases.
	  */
	type Generalized >: Self <: FromLast {
		type FromLast <: thisClause.FromLast
		type Generalized <: thisClause.Generalized //these can't be =:= because Dual.Generalized =:= FromClause
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = FromClause
		type Base <: thisClause.Base
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	/** This clause upcast to its generalized supertype in which all join kinds are replaced with their least upper
	  * bounds which still retain the information about their function.
	  * @return `this.self`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  */
	def generalized :Generalized = self

	/** A recursive reconstruction of this clause's concrete type. It is defined by replacing every type parameter
	  * `F &lt;: FromClause`, corresponding to a member `val f :F`, with `f.Self`. A concrete clause `C` without
	  * type parameters (only `Dual`) defines it simply as `type Self = C`, terminating the recursion. The result is
	  * the lowest upper bound of a clause `F` without abstract `FromClause` ''subclasses'' in its definition.
	  * If `F` is an abstract type, all abstract type parameters in its signature remain abstract types in `f.Self`.
	  * This procedure circumvents possible covariance in the type parameter `F`, which would prevent defining an alias
	  * for the self type using `F` directly in its definition. For a concrete class with fully instantiated
	  * type parameters, it is its concrete supertype. While impossible to enforce in the type system, for any
	  * `F &lt;: FromClause`, `F &lt;: F#Self` ''iff'' `F` is a concrete clause - one without any abstract subtypes
	  * and abstract subclasses of `FromClause` in its signature. Any additions into the class hierarchy must
	  * provide a definition of `Self` to which every instance of the class can be safely cast, as
	  * the [[net.noresttherein.oldsql.sql.FromClause#self self]] method is final and implemented as
	  * `this.asInstanceOf[Self]`. Note that it is possible for a concrete class `S` to extend another
	  * concrete `FromClause` subclass `B`, in which case `b.Self =:= s.Self`, for some `val b :B[]; val s :S[]`,
	  * where `B[]` and `S[]` are concrete types resulting from applying `B` and `S` to their type parameters.
	  * All `Self` types are subtypes of the `Generalized` type for the given instance by definition.
	  * Most member types defined here are preserved, so in most use cases `f.self` can be substituted for `f`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#This]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ConcreteFrom]]
	  */
	type Self <: FromLast {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
		//type AsSubselectOf[+F <: FromSome] <: thisClause.AsSubselectOf[F]    //not for JoinLike, not worth it making it work
		//type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T] //not for Dual
		//type FromSubselect[+F <: FromSome] = thisClause.FromSubselect[F]     //not for Dual
	}

	/** This clause as its [[net.noresttherein.oldsql.sql.FromClause#Self Self]] type.
	  * @return `this.asInstanceOf[Self]`.
	  */
	final def self :Self = this.asInstanceOf[Self]

	/** The supertype of this clause used by some copy constructors/factory methods declared here and in the subclasses.
	  * For concrete classes, `This >: this.type` always holds, but it is not enforced on this level.
	  * The difference from the other self type [[net.noresttherein.oldsql.sql.FromClause#Self Self]] is that this type
	  * if ''not'' formed recursively, with the narrowing in subclasses being restricted only to the outer class,
	  * retaining the same type parameters. For this reason, this type generally doesn't receive a concrete definition
	  * in public classes, as the covariance in type parameters restricts it to remain an upper bound instead.
	  * Thus, for `join :InnerJoin[L, R]`: `join.This &lt;: L InnerJoin R` and `join.Self =:= join.left.Self InnerJoin R`.
	  * The advantages over `Self` is that in most implementation classes `This >: this.type`, allowing interoperability
	  * between an original clause and its copy as well as cleaner method return types, with the result of  `l join r`
	  * for some types `val l :L; val r :Relation[R]` being of type `L InnerJoin R` rather than `l.Self InnerJoin R`.
	  * The drawback is that this type doesn't conform to `Generalized` from the point of view of this clause.
	  * However, if for any concrete clause `F &lt;: ConcreteFrom` and `from :F`, `from.This &lt;: from.Self`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Self]]
	  */
	//type This >: this.type <: FromLast //doesn't hold for decorators
	type This <: FromClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type LastTable[F <: FromClause] = thisClause.LastTable[F]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}



	/** Creates a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  * instance's filter and the given `SQLBoolean`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#where]]
	  */
	def where(filter :SQLBoolean[Generalized]) :This

	/** Apply a filter condition to this clause. The condition is combined using `&&` with `this.condition`
	  * and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * @param condition a function which accepts a
	  *                  [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] instance functioning
	  *                  as a facade to this clause, providing easy access to all its relations,
	  *                  and which returns an SQL expression for the new join/filter condition.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	def where(condition :JoinedEntities[Generalized] => SQLBoolean[Generalized]) :This = {
		val cond = condition(new JoinedEntities[Generalized](generalized))
		where(SQLScribe.groundFreeComponents(generalized, cond))
	}



	/** A boolean function of two relations - the `last` relation of this clause and another one for mapping `N` -.
	  * Used by the [[net.noresttherein.oldsql.sql.AndFrom#on on]] method to provide a join condition between this
	  * clause and another relation. It is always parameterized with the same set of type members of the filtered
	  * relation: `next.GeneralizedLeft`, `next.FromLast`, `next.Generalized` and `next.LastMapping`.
	  * For non-empty clauses and an extending join `next :_ AndFrom N`, this type essentially resolves to:
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
	  */
	type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] <:
		           (JoinedRelation[FromNext[E], LastMapping], JoinedRelation[S, N]) => SQLBoolean[G]

	/** The implementation method to which [[net.noresttherein.oldsql.sql.AndFrom#on on]] delegates. */
	protected def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	              (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :next.This

	private[sql] final def bridgeFilterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N])
			:next.This =
		filterNext[F, N](next)(filter)



	/** Does this clause contain no relations, being a base for a select without a ''from'' clause? */
	def isEmpty :Boolean

	/** Does this clause contain any relations? */
	@inline final def nonEmpty :Boolean = !isEmpty

	/** Number of relations available in this join, in both the implicit and explicit part
	  * (counting all their occurrences separately). Note that this doesn't always equal the number of mappings
	  * listed in the type, as the relations between any `Subselect`-`GroupByAll` pair are not counted.
	  * This makes `fullSize` consistent with the length of [[net.noresttherein.oldsql.sql.FromClause#fullRow fullRow]]
	  * and [[net.noresttherein.oldsql.sql.FromClause#fullTableStack fullTableStack]].
	  * @see [[net.noresttherein.oldsql.sql.FromClause#innerSize]]
	  */
	def fullSize :Int

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'FromClause' is the prefix clause of its `Generalized` type, with all its relations.
	  */
	@inline final def generalizedSpan :FromClause ExtendedBy Generalized = new ExtendedBy(fullSize)

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'Dual' is the prefix clause of its `Self` type, with all its relations.
	  */
	@inline final def fullSpan :Dual ExtendedBy Self = new ExtendedBy(fullSize)


	/** The number of all relations available in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'FromClause' is the prefix clause of its `Generalized` type, with all its relations.
	  */
	@inline final def generalizedSuffix :FromClause PrefixOf Generalized = new PrefixOf(fullSize)

	/** The number of all available relations in this clause, implicitly or explicitly, that is `this.fullSize`,
	  * as a witness to the fact that 'Dual' is the prefix clause of its `Self` type, with all its relations.
	  */
	@inline final def fullSuffix :Dual PrefixOf Self = new PrefixOf(fullSize)



	/** A [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters of this clause joined with
	  * the [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension.param param]] method.
	  * In particular, a `FromClause` subtype with no `JoinParam` joins in its full type has this type equal to `@~`.
	  * This is can be used to statically enforce that a clause is parameterless by refining on this type:
	  * `from :FromClause { type Params = @~ }`, or `from :ParameterlessFrom` as a less bug-prone alternative.
	  */
	type Params <: Chain

	/** Does this clause contain any [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] 'joins'
	  * (or its subtypes)?
	  * @return `false` ''iff'' `this.Params =:= @~`.
	  */
	def isParameterized :Boolean

	/** Subject types of the mappings of all relations in this clause, concatenated into a heterogeneous list.
	  * The [[net.noresttherein.oldsql.collection.Chain chain]] contains the mapped types in the same order
	  * as their mappings appear in this type's definition and is, like [[net.noresttherein.oldsql.sql.Using Using]]
	  * (and all [[net.noresttherein.oldsql.sql.Join Join]] kinds), but unlike `::`, left associative.
	  * Clauses with a [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] component are treated specially:
	  * any mappings preceding the `GroupByAll` extension class since the last
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] or the start of the clause are omitted from the chain
	  * as being under the ''group by'' clause.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#fullRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type FullRow <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all relations available
	  * in this clause in their order of appearance. All mappings occurring between a
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) until the next
	  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]], if present, are considered unavailable. The omitted
	  * segment constitutes the ''from'' clause of a single select expression with a ''group by'' clause, meaning
	  * the values of individual rows cannot be accessed.
	  */
	def fullRow :ChainTuple[Generalized, FullRow]

	/** Create an SQL tuple expression, based on some extending clause `E`, containing `JoinedRelation` expressions
	  * for all joined elements in their order of appearance. It will contain entries for all mappings in this clause,
	  * including parameter mappings and mappings listed in this clause's `Implicit` prefix (if this clause
	  * is a subselect clause). This overloaded variant is used by the zero-argument `fullRow` to obtain
	  * the chain prefix containing the relation formulas based on the final clause type from a prefix clause of a join,
	  * without the need to adapt each relation once for every intermediate join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def fullRow[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, FullRow]

	/** All relations in this clause as abstract, untyped mappings, in the reverse order of their appearance.
	  * Any relations in the scope of a [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]] clause are excluded.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#fullRow]]
	  */
	def fullTableStack :LazyList[RelationSQL.AnyIn[Generalized]]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * The `JoinedRelation`s are based on some extending clause `E`, so that the stack for a prefix clause
	  * can be used as-is, with no need to map over it, by extending clause's zero argument `fullTableStack`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def fullTableStack[E <: FromClause]
	                  (target :E)(implicit extension :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]]



	/** A join of relations from the clause `P` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `P`, that is appending all mappings from this clause
	  * in order to `P`, using the same join kinds. The join type `J` is used as the join between the last relation
	  * in `P` and the first relation in `this`, unless this clause starts with a `JoinParam` join.
	  * It is a narrower variant of [[net.noresttherein.oldsql.sql.FromClause#AppendedTo AppendedTo]],
	  * with the join type `J`'s left side being restricted to non-empty clauses.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#joinedWith]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#JoinedWithSubselect]]
	  */
	type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] <: Generalized

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
	  * @see [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join]]
	  */
	def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :JoinedWith[F, firstJoin.LikeJoin]

	/** A specialized variant of the [[net.noresttherein.oldsql.sql.FromClause#JoinedWith JoinedWith]], it is the type
	  * of a [[net.noresttherein.oldsql.sql.Subselect Subselect]] 'join' between the relations from the clause `P`
	  * and those from this clause. It is the result of substituting the initial
	  * [[net.noresttherein.oldsql.sql.From From]]`[T]` clause in this type's definition with `P Subselect T`.
	  * The join kinds of both clauses are preserved.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#joinedWithSubselect]]
	  */
	type JoinedWithSubselect[+P <: NonEmptyFrom] <: JoinedWith[P, Subselect] {
		type InnerRow = thisClause.InnerRow
		type Explicit = thisClause.Explicit //JoinLike loses equality
		//type Inner <: thisClause.Inner //Doesn't work in JoinLike, too complex
	}

	/** Represents this ''from'' clause as a subselect of the clause `prefix` by joining them with the
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] artificial join. The resulting clause
	  * is the result of appending the first relation from `this` to the `prefix` clause using the `Subselect` join,
	  * and then all the following relations using the same joins as in this clause. All join conditions are copied,
	  * but any additional condition narrowing the resulting cross join must be added manually. Note that the result
	  * is not necessarily a subselect of this clause (in terms of `Nested` and `SubselectOf`), as there may be other
	  * intermediate `Subselect` joins present in `prefix`. This method is the delegate target
	  * of the [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#subselect subselect]]
	  * method available through an implicit conversion, which should be used instead by the client code.
	  * @throws UnsupportedOperationException if this clause is empty or the first join in this clause is a `JoinParam`.
	  * @see [[[net.noresttherein.oldsql.sql.FromClause#asSubselectOf]]
	  */
	def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :JoinedWithSubselect[F]

	/** Joins the clause given as the parameter with this clause. The type of the resulting clause is the result
	  * of replacing the empty clause `Dual` in this clause's type with `P` and upcasting the join between `Dual`
	  * and the first relation `T` to `P AndFrom T`. The clauses are joined using an inner join,
	  * unless `prefix` any of the clauses are empty, in which case the other is returned.
	  * The difference from [[net.noresttherein.oldsql.sql.FromClause#joinWith joinWith]] is that it accepts
	  * empty clauses as arguments, but the return type is upcast to `AndFrom`.
	  *
	  * This is a low-level method and client code should generally prefer the implicit extension method
	  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension#andFrom andFrom]] which uses the more natural
	  * prefix - suffix order rather than the inversion as in this method.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#joinedWith]]
	  */
	def appendedTo[P <: DiscreteFrom](prefix :P) :JoinedWith[P, AndFrom]



	/** A property specifying if this ''from'' clause is has a `Subselect` join somewhere in its dynamic type. */
	def isSubselect :Boolean = outer.nonEmpty

	/** A property specifying if this ''from'' clause is a legal subselect ''from'' clause, that is it has a `Subselect`
	  * join somewhere in its complete (dynamic) type and no unbound parameters in the most nested subselect clause.
	  */
	def isValidSubselect :Boolean



	/** Super type of all `FromClause` subtypes representing a valid ''from'' clause of a subselect directly nested
	  * under this clause's select. This type matches only 'true' subselect clauses, that is types
	  * `S &lt;: Generalized Subselect T0 J1 T1 ... JN TN`, where `J1...JN` are `Join` subtypes or
	  * `GroupByAll`/`ByAll` (with no `UnboundParam` 'joins'). This means that non subselect, 'outer' clauses
	  * do not conform to this type despite free select expressions being valid subselects of any select expression.
	  * As a member type, it is able to define the [[net.noresttherein.oldsql.sql.FromClause#Implicit Implicit]] and
	  * [[net.noresttherein.oldsql.sql.FromClause#Base Base]] types of this refinement as `this.Generalized`, allowing
	  * it to work for abstract clauses: `f :f.outer.Nested`
	  * for any `f :`[[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]. Similarly,
	  * `s :f.Nested` for any clause `s = f subselect t ...` (where '...' denote any sequence of join / group by
	  * methods other than [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension#param param]] and `subselect`
	  * (or any other method creating a [[net.noresttherein.oldsql.sql.Subselect Subselect]] join).
	  * This type differs from [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]] in several aspects:
	  *   - it always works on the full, concrete types - if either clause `S` or `F` has an abstract clause as
	  *     a prefix, `S` won't conform to `F#Nested` even if the whole explicit portion is fully instantiated;
	  *   - the types must match exactly in their entirety, meaning that `F` is an exact prefix of `S` (in their
	  *     generalized forms) and thus `S` contains all relations from `F`, where `S &lt;: SubselectOf[F]` only
	  *     checks if `S#Base >: F`, which allows `S#Base` to only match a suffix of `F`.
	  *   - it matches only true subselect clauses, that is those where `F` is immediately followed by `Subselect`
	  *     in `S`, where the `S &lt;: SubselectOf[F]` for any `F` if
	  *     `S &lt;: `[[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]].
	  * This makes `f.Nested` a strict subtype of `SubselectOf[f.Self]`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Base]]
	  */
	type Nested = NonEmptyFrom {
		type Implicit = thisClause.Generalized
		type Base = thisClause.Generalized
		type DefineBase[+I <: FromClause] = I
	}


	/** The incomplete suffix clause containing all relations since the last `Subselect` join or `Dual`, with all
	  * join kinds replaced with their ''generalized'' form, while the `Subselect` in question with `AndFrom`.
	  * It lists all relations present in the actual ''from'' clause of the associated SQL select in a canonical form,
	  * abstracting over join kinds. It can be formed by substituting in the complete `Generalized` type of this clause
	  * the `Implicit` prefix, containing relations present 'implicitly' in the scope, with `FromClause`. All `AndFrom`
	  * subclasses define it as `left.Explicit AndFrom R`, with the exception of `Subselect` join, which defines it
	  * as `FromClause AndFrom R` instead, while `Dual` defines it as `FromClause` - consistently with `Generalized`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Inner]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  */
	type Explicit >: Generalized <: FromLast// { type Explicit <: thisClause.Explicit } //it's either this or AsSubselectOf <: Inner

	/** The suffix clause containing all relations from the explicit portion of this subselect clause.
	  * It represents the actual ''from'' clause of an associated SQL subselect. For subselect clauses, it results from
	  * replacing the left side of the last `Subselect` in this type's `Self` member with `FromSome`; for non-subselect
	  * clauses (with no `Subselect` joins in its complete type), it is the full type of this clause with `Dual`
	  * replaced with `FromClause` (or `From[T]` with `Dual AndFrom T`).
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Self]]
	  */
	type Inner >: Self <: Explicit// { type Inner <: thisClause.Inner } //it's either this or AsSubselectOf <: Inner

	/** A prefix ''from'' clause of this clause consisting of all relations available to SQL expressions implicitly,
	  * that is not included in the actual ''from'' clause of this subselect clause.
	  * It is the generalized form of the outer clause if this clause represents a subselect clause, created by
	  * `Implicit.subselect()`. All `Join` subclasses and `From` have this type equal to the `Implicit` type
	  * of their left side, but `Subselect` defines `Implicit` as the generalized type of its left side. Additionally,
	  * all 'true' joins conform to `AsSubselectOf[L#Implicit]` and `L Subselect R` conforms to `AsSubselectOf[L]`.
	  * Therefore, `Implicit` is equal to the `Generalized` type of the left side of the last `Subselect`,
	  * or `FromClause` for non subselect clauses. This means that for any generalized type `S &lt;: FromClause`
	  * with fully instantiated parameters (the clause is ''complete'' and the `Generalized` type is well defined) value
	  * `(s :S) subselect t1 join t2 ... join t3` conforms to `SubselectOf[S]` and `s.Nested`.
	  * This way one can statically express a dependency relationship between ''from'' clauses without resorting
	  * to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Nested]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type Implicit <: FromClause { type Generalized <: thisClause.Implicit }

	/** A prefix ''from'' clause of this clause consisting of all joins and relations preceding the last (rightmost)
	  * `Subselect` join. If there are no `Subselect` joins in the complete type definition of this clause,
	  * it is equal to `Dual`. It represents the clause of the outer select for this subselect clause, assuming
	  * this clause is a subselect clause created by `outer.subselect()` (and possibly expanded further).
	  * All `Join` subclasses and `From` have this type equal to the `Outer` type of their left side, but `Subselect`
	  * defines `Outer` as the `Self` type of its left side. This means that, for any type `S &lt;: FromClause`
	  * with fully instantiated parameters (the clause is ''concrete'', that is the concrete types of all joins in it
	  * are known), value `(s :S) subselect t1 join t2 ... join t3` conforms to `s.Nested`
	  * and `FromClause { type Outer = s.Self }` if none of the joins following the subselect element are
	  * an [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]]. This way one can statically express a dependency
	  * relationship between ''from'' clauses without resorting to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Inner]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Self]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type Outer <: Implicit {
		type Generalized = thisClause.Implicit
		type Self = thisClause.Outer
	}

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselect()`. When viewed as a list of relations, `outer` constitutes
	  * the result of dropping all relations joined in this instance up until and including a relation joined
	  * by the `.subselect()` call, going from right to left. If there's no `Subselect` in the dynamic type definition
	  * of this clause, meaning this is not a subselect clause (the actual ''from'' clause of resulting selects
	  * will include all members of this clause), `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or `left.Self` if this instance is a `Subselect`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	val outer :Outer

	/** The `FromClause` type used as the type parameter of [[net.noresttherein.oldsql.sql.SelectSQL select]] SQL
	  * expressions created from this instance. It is equal to `Implicit`, unless
	  * an [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] is present in the explicit portion of
	  * this clause, in which case it equals `Nothing` (hence invalidating the expression).
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */ //not defined as DefineBase[Implicit] here because of a clash with SubselectOf refinement caused by a scala bug
	type Base <: DefineBase[Implicit]

	/** A helper type used to define the type `Base`. Always parameterized with `Implicit`, all `Extended` subclasses
	  * define it as equals to the argument `I`, except of `UnboundParam`, which defines it as `Nothing`.
	  * This is better than defining `Base` directly, as it allows to propagate behavior of the explicit portion
	  * (which determines whether `Base` will equal `Implicit` or `Nothing`) when rebasing it onto another
	  * clause (and hence changing the actual `Implicit` type).
	  */
	type DefineBase[+I <: FromClause] <: I

	/** The implicit prefix of this clause, that is the clause ending before the last
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] join, or [[net.noresttherein.oldsql.sql.Dual Dual]]
	  * for non subselect clauses.
	  * @return `this.outer`
	  * @throws UnsupportedOperationException if there is an [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]]
	  *                                       join after the last `Subselect` in this clause.
	  */
	def base :Base


	/** The ''where'' clause of this subselect clause representing the explicit filter condition as an SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause since the last `Subselect` or `GroupByAll`
	  * join. For non subselect clauses, it is equal to [[net.noresttherein.oldsql.sql.FromClause#fullFilter fullFilter]].
	  */
	def filter :SQLBoolean[Generalized]

	/** The combined join conditions of all joins since the last `Subselect` or `GroupByAll` join as a expression based
	  * on an extending clause. Used by zero-argument `fullFilter` to request the individual join conditions
	  * as expressions based on the clause it was called for.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E]



	/** Number of relations contained in the explicit portion of this subselect join. This is equal to
	  * the number of mappings to the right of the rightmost `Subselect` join or `Dual`,
	  * that is `fullSize - outer.fullSize`.
	  *///consider: renaming it to size
	def innerSize :Int = fullSize - outer.fullSize

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.innerSize`,
	  * as a witness to the fact that its 'Implicit' type is the generalization of the prefix clause
	  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
	  * it represents the full `Generalized` type, with all its relations.
	  */
	@inline final def explicitSpan :Implicit ExtendedBy Generalized = new ExtendedBy(innerSize)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.innerSize`,
	  * as a witness to the fact that its 'Outer' type is the generalization of the prefix clause
	  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
	  * it represents the full type `Self`, with all its relations.
	  */
	@inline final def innerSpan :Outer ExtendedBy Self = new ExtendedBy(innerSize)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.innerSize`,
	  * as a witness to the fact that its 'Implicit' base clause, constituting of all relations listed by
	  * enclosing selects, is a prefix of the `Generalized` type of this clause. For non-subselect clauses,
	  * it represents the full `Generalized` type size (that is, all relations in the clause).
	  */
	@inline final def explicitSuffix :Implicit PrefixOf Generalized = new PrefixOf(innerSize)

	/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.innerSize`,
	  * as a witness to the fact that its 'Outer' portion is a prefix clause of its `Self` type (that is,
	  * its complete type with all join kinds known). For non-subselect clauses, it is the number of all relations.
	  */
	@inline final def innerSuffix :Outer PrefixOf Self = new PrefixOf(innerSize)



	/** Subject types of all mappings in this clause following the `Implicit` prefix, concatenated into
	  * a [[net.noresttherein.oldsql.collection.Chain Chain]]. This amounts to the list of entities from
	  * all the relations from the ''from'' clause of the most deeply nested subselect or, if this is a ''group by''
	  * clause, all mappings to the right of the last `GroupByAll` appended by it or `ByAll`. This way the chain
	  * contains all mappings which are available to the ''select'' clause created from this instance.
	  * If this clause doesn't represent a subselect, but a top-level query, it is the same as `FullRow`.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition and is,
	  * like `AndFrom` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#innerRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#FullRow]]
	  */ //consider: renaming to Row and drop the 'inner' prefix from the following methods
	type InnerRow <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all joined relations
	  * in the most deeply nested subselect, in their order of appearance. This includes all relations in this clause
	  * following the most recent `Subselect` or `GroupByAll` 'join', marking the first not grouped relation following
	  * the `Implicit` clause prefix. It represents the complete data set available to the ''select'' clause with
	  * the exception of potential grouped relations available only to aggregate functions.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	def innerRow :ChainTuple[Generalized, InnerRow]

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all joined elements
	  * of the most deeply nested subselect clause, in their order of appearance. This includes all relations
	  * in this clause following the most recent `Subselect` or `GroupByAll` 'join', marking the first not grouped
	  * relation following the `Implicit` clause prefix. It represents the complete data set available to the ''select''
	  * clause with the exception of potential grouped relations available only to aggregate functions. The expressions
	  * are based on some extending clause `E`, so they can be used by the zero-argument `innerRow` as the chain prefix
	  * of its result.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def innerRow[E <: FromClause]
	            (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, InnerRow]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first not grouped relation following the `Implicit` prefix. This is the one joined with the last
	  * `Subselect` 'join', unless it is followed (possibly indirectly) by a `GroupByAll` clause (before any other
	  * `Subselect`), in which case the last relation is the one for the component following `GroupByAll`.
	  * If this is not a subselect clause (no `Subselect` 'joins' are present in this clause
	  * and `Implicit =:= FromClause`), all not grouped relations are included.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#innerRow]]
	  */
	def innerTableStack :LazyList[RelationSQL.AnyIn[Generalized]]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first not grouped relation following the `Implicit` prefix. This is the one joined with the last
	  * `Subselect` 'join', unless it is followed (possibly indirectly) by a `GroupByAll` clause (before any other
	  * `Subselect`), in which case the last relation is the one for the component following `GroupByAll`.
	  * If this is not a subselect clause (no `Subselect` 'joins' are present in this clause
	  * and `Implicit =:= FromClause`), all not grouped relations are included. The returned relation SQL expressions
	  * are based on some extending clause `E`. Used by the zero-argument `innerTableStack`
	  * to request the tail of the stack with expressions of the correct type from the prefix clause.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def innerTableStack[E <: FromClause]
	                   (target :E)(implicit extension :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]]


	/** A [[net.noresttherein.oldsql.collection.Chain Chain]] with `Subject` types of mappings for all relations
	  * available to this clause implicitly, i.e., by virtue of being a part of a ''from'' clause of one of
	  * enclosing ''selects''. It is equal to `outer.`[[net.noresttherein.oldsql.sql.FromClause#FullRow FullRow]].
	  */ //this type is needed to implement FullRow in GroupByAll so that Self preserves it, as it does not outer.type
	type OuterRow <: Chain

	/** Create an SQL tuple expression containing all `JoinedRelation` expressions from the outer, ''implicit''
	  * section of this clause, that is all elements until the rightmost `Subselect` join, in their order of appearance.
	  * If this is not a subselect clause, the chain will be empty.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	def outerRow :ChainTuple[Generalized, OuterRow]

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all elements joined until
	  * the rightmost `Subselect` join, in their order of appearance. If this is not a subselect clause,
	  * the chain will be empty. The expressions are based on some extending clause `E`, so they can be used
	  * by the zero-argument `outerRow` as the chain prefix of its result.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def outerRow[E <: FromClause](target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, OuterRow]



	/** A type constructor appending all relations joined since the last (rightmost) `Subselect` 'join' (or `Dual`)
	  * to the given clause `F`. This is the result of replacing everything to the left of the last `Subselect`
	  * in this type's signature with `F`. For any concrete subselect clause `J &lt;: FromSome` without
	  * any parameters in the explicit/inner section, `J =:= J#AsSubselectOf[J#Outer]`
	  * and `J#AsSubselectOf[J#Implicit] &lt;:&lt; J#Generalized`. For outer clauses, it replaces
	  * the first `From[T]` 'join' with `F Subselect T`, but `Dual`, despite already being a subselect clause
	  * of any clause, defines it as `Nothing` as an exception, because joining the result with additional relations
	  * would otherwise ''not'' become a subselect clause of `F`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */ //consider: parameterizing it instead (or also) with a singleton type, so we can have Implicit = F#Generalized, etc.
	type AsSubselectOf[+F <: NonEmptyFrom] <: Inner {
		type Explicit = thisClause.Explicit
		//type Inner <: thisClause.Inner //Doesn't hold for From[T] and makes asSubselectOf result volatile
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type AsSubselectOf[+E <: NonEmptyFrom] = thisClause.AsSubselectOf[E]
	}

	/** For subselect clauses - that is subtypes with a `Subselect` join kind occurring somewhere in their definition,
	  * not necessarily `Subselect` instances - it represents them as a subselect of a clause `F`, being
	  * an extension of their outer clause (the left side of the right-most `Subselect` join). In syntactic terms,
	  * it replaces the `Outer` type in this type's definition with type `F`. Procedurally, it joins in order
	  * all relations since the last occurrence of a `Subselect` join, forming the explicit ''from'' clause of
	  * a modified subselect, with the new outer clause `F`, preserving all join conditions. If this clause is not a
	  * subselect clause, this method will use `Subselect` 'join' to join the `newOuter` with the first relation of this
	  * clause. All join conditions are preserved. As the argument clause is an extension of the outer clause of
	  * this instance, this has the effect of inserting the relations appended to the outer clause between it and
	  * the inner clause. For outer clauses, this should produce the same result as
	  * [[net.noresttherein.oldsql.sql.FromClause#joinedWithSubselect joinedWithSubselect]], though the exact types
	  * may be different when called for abstract types.
	  * @return a ''from'' clause which, assuming this instance doesn't contain any `UnboundParam` parameters
	  *         in its outer section, conforms to `newOuter.Nested` and, if this type is instantiated at least to
	  *         the last `Subselect` (or `Dual`/`From`) and its generalized form is known,
	  *         to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[F]`.
	  * @throws `UnsupportedOperationException` if this clause is empty or there is an `UnboundParam` 'join'
	  *                                         in its explicit portion.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self }



	/** A ''from'' clause for a subselect of this clause with the single relation `T`, conforming to `SubselectOf[Self]`. */
	type FromRelation[T[O] <: MappingAt[O]] <: (Self AndFrom T) {
		type Params = thisClause.Params
		type FullRow <: thisClause.FullRow ~ T[FromClause AndFrom T]#Subject
		type Explicit = FromClause AndFrom T
		type Implicit = thisClause.Generalized
		type Outer = thisClause.Self
		type Base = thisClause.Generalized
		type DefineBase[+I <: FromClause] = I
		type InnerRow <: @~ ~ T[FromClause AndFrom T]#Subject
		type OuterRow = thisClause.FullRow
	}

	/** Creates a ''from'' clause for a subselect of a select with this clause. It will have the given relation
	  * as the only member of the ''explicit'' portion (i.e. the one actually appearing in the ''from'' clause
	  * of the generated SQL) and this clause as the ''implicit'' prefix,
	  * joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. All relations included
	  * in this clause are available to any `SQLExpression` parameterized with the type of the returned clause.
	  * The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns `From(subselect)`. All non subselect clauses conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[FromClause] ]] so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way, as long as its `Generalized` type is known.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities#from from]] method of
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#subselect subselect]] extension method instead,
	  * which has a friendlier return type.
	  * @param first the sole relation of the ''from'' clause of the new subselect clause.
	  * @param infer implicit witness guiding the compiler to properly infer the subject type of mapping `M` (and `T`).
	  * @return `Self Subselect T`, or `From[T]` if this clause is empty.
	  *        The clause will conform to `this.Nested` and, as long as it is complete and its generalized form is known,
	  *        to `SubselectOf[Generalized]`.
	  */
	def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (first :Relation[M]) //uses Conforms instead of JoinedRelationSubject because the result type might be a Subselect or From
	        //(implicit cast :InferSubject[this.type, Subselect, M, T, S]) //subselect would have to accept FromClause (including empty)
	        (implicit infer :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:FromRelation[T]

	/** Type resulting from replacing the `Outer`/`Implicit` part of `F` with `Self` type of this clause.*/
	type FromSubselect[+F <: NonEmptyFrom] <: NonEmptyFrom { //consider: parameterizing with a singleton type
		type Explicit <: F#Explicit
		type Implicit = thisClause.Generalized
		//type Outer = thisClause.Self //too much refining by casting in Dual
		type InnerRow <: F#InnerRow
	}

	/** Creates a ''from'' clause for a subselect of a select with this clause. It uses the given clause `subselect`
	  * as its ''explicit'' suffix (i.e. the relations from the actual ''from'' clause of the associated subselect),
	  * and this clause as the ''implicit'' prefix, joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * pseudo join. The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns the `subselect` argument unchanged. All non-subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[FromClause] ]] (either statically,
	  * or by the abstract [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]], so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities#from from]] method of
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#subselect subselect]] extension method instead,
	  * which accepts any non-empty clause, not only
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeFromSome FreeFromSome]] subtypes.
	  * @param subselect the actual, explicit ''from'' clause of a target subselect - without any `Subselect`
	  *                  or `UnboundParam` joins of its own.
	  * @return a ''from'' clause adapted from the `subselect` argument by prepending this clause to it.
	  *         It will conform to `this.Nested` and, as long as this clause is ''generalized'',
	  *         to `SubselectOf[Generalized]`.
	  */
	def from[F <: NonEmptyFrom with FreeFrom](subselect :F) :FromSubselect[F] { type DefineBase[+I <: FromClause] = I }

	/** Creates a ''from'' clause for a subselect of a select with this clause. It uses the ''explicit''/''inner''
	  * portion of the given `subselect` clause as its ''explicit'' suffix (i.e. all the explicit relations
	  * from the ''from'' clause of the associated subselect, be they statically known or not), substituting
	  * its ''implicit'' prefix with this clause, joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * pseudo join. It can only be applied to clauses of with an implicit, outer portion extended by this clause,
	  * that is if the latter contains all the relations available implicitly to the `subselect` clause as its prefix
	  * in the exact order and, presumably, was created by extending the very `subselect.outer` instance.
	  * Compare this with the [[net.noresttherein.oldsql.sql.FromClause#from from]] method, which joins
	  * a whole independent clause as the ''explicit/inner'' portion of a the resulting subselect clause.
	  *
	  * The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns the `subselect` argument unchanged. All non-subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[Dual] ]], so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way, as long as `this.Terminator =:= Dual`
	  * (this clause is complete).
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities#fromSubselect fromSubselect]] method of
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]].
	  * @param subselect the actual, explicit ''from'' clause of a target subselect.
	  * @return a ''from'' clause adapted from the `subselect` argument by replacing its `Outer` part with this clause,
	  *         conforming to `outer.Nested` and `SubselectOf[Generalized]` (if this clause's `Generalized` type
	  *         is well defined).
	  */
	def fromSubselect[F <: NonEmptyFrom](subselect :F)(implicit extension :subselect.Implicit ExtendedBy Generalized)
			:FromSubselect[F] { type DefineBase[+I <: FromClause] = subselect.DefineBase[I] }



	/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses
	  * and the ''select'' clause consisting of all selectable columns of the mapping returned by the passed function.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param component a function returning any component (including columns and the whole relation mappings)
	  *         of any of the relations available in this clause.
	  * @return a `SelectSQL` with the specified component as the ''select'' clause and `this.Base` as its
	  *         'outer' type (the ''from'' clause serving as the basis for the expression).
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	def selectAs[C[A] <: MappingAt[A], M[A] <: BaseMapping[S, A], S, O >: Generalized <: FromClause]
	            (component :JoinedEntities[Generalized] => C[O])
	            (implicit cast :Conforms[C[O], M[O], BaseMapping[S, O]], shift :TableShift[O, M, _ <: Numeral])
			:Base SelectAs M[Any] =
		{
			type Mock = RelationSQL[Generalized, MappingOf[Any]#TypedProjection, Any, O]
			val table = fullTableStack(shift.tables).asInstanceOf[Mock]
			val comp = table \ component(generalized.entities)
			selectAs(comp)
		}


	/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses
	  * and the ''select'' clause consisting of all selectable columns of the passed mapping.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param component any component (including columns and whole relation mappings) of any of the relations
	  *        available in this clause.
	  * @return a `SelectSQL` with the specified component as the ''select'' clause and `this.Base` as its
	  *         'outer' type (the ''from'' clause serving as the basis for the expression).
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	def selectAs[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[S, A], S, O >: Generalized <: FromClause]
	            (component :ComponentSQL[Generalized, T, E, M, S, O]) :Base SelectAs M[Any] =
		if (isParameterized)
			throw new UnsupportedOperationException(
				s"Cannot use a parameterized clause as a basis for select $component: $this."
			)
		else if (isSubselect) {
			component.subselectFrom[Self, Any](self)
		} else
			component.asInstanceOf[ComponentSQL[FromClause, T, E, M, S, FromClause]].selectFrom(this.asInstanceOf[FreeFrom])


	/** Creates an SQL ''select'' expression with this instance used for the ''from'' and ''where'' clauses
	  * and the expression returned by the passed function as its ''select'' clause.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param header a function returning any kind of SQL expression (including, in particular, tuples, which will be
	  *               inlined in the ''select'' clause), constructable from the expressions for the joined relations
	  *               of this clause.
	  * @return a `SelectSQL` based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), using the given arbitrary expression as the ''select'' clause.
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	@inline def select[V](header :JoinedEntities[Generalized] => SQLExpression[Generalized, V])
			:SelectSQL[Base, V, _] =
		select(header(generalized.entities))


	/** Creates an SQL ''select'' expression selecting a single column, as defined by the result of the passed function,
	  * and this instance as the source of its ''from'' and ''where'' clauses.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param header a function returning an expression for a single column, constructable from the expressions
	  *               for the joined relations of this clause.
	  * @return a single column select, based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), which is a valid `ColumnExpression` (that is, an atomic SQL value).
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	@inline def selectColumn[V](header :JoinedEntities[Generalized] => ColumnSQL[Generalized, V])
			:SelectColumn[Base, V, _] =
		select(header(generalized.entities))


	/** Creates an SQL ''select'' expression selecting an arbitrary `SQLExpression`. The ''from'' and ''where'' clauses
	  * are defined by this instance, while the ''select'' clause consists of all column sub-expressions constituting
	  * the `header` subexpressions.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param header an arbitrary type of SQL expression (including, in particular, tuples, which will be
	  *               inlined in the ''select'' clause), constructable from the expressions for the joined relations
	  *               of this clause.
	  * @return a `SelectSQL` based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), using the given arbitrary expression as the ''select'' clause.
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	def select[V](header :SQLExpression[Generalized, V]) :SelectSQL[Base, V, _] =
		if (isParameterized)
			throw new UnsupportedOperationException(
				s"Cannot use a parameterized clause as a basis for select $header: $this."
			)
		else if (isSubselect) {
			header.subselectFrom(self)
		} else
			header.asInstanceOf[SQLExpression[FromClause, V]].selectFrom(this.asInstanceOf[FreeFrom])


	/** Creates an SQL ''select'' expression selecting a single column, as defined by the passed `header` `ColumnSQL`,
	  * and this instance as the source of its ''from'' and ''where'' clauses.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @param header a single column SQL expression, constructable from the expressions for the joined relations
	  *               of this clause.
	  * @return a single column select, based on this instance's `Implicit` type (that is, embeddable only as an expression
	  *         in instances of this `this.Implicit`), which is a valid `ColumnExpression` (that is, an atomic SQL value).
	  * @throws UnsupportedOperationException if this clause is parameterized (contains any `UnboundParam` joins).
	  */
	def select[V](header :ColumnSQL[Generalized, V]) :SelectColumn[Base, V, _] =
		if (isParameterized)
			throw new UnsupportedOperationException(
				s"Cannot use a parameterized clause as a basis for select $header: $this."
			)
		else if (thisClause.isSubselect) {
			header.subselectFrom(self)
		} else
			header.asInstanceOf[ColumnSQL[FromClause, V]].selectFrom(this.asInstanceOf[FreeFrom])



	/** Creates an SQL ''select'' expression selecting all columns from all relations of the explicit (subselect)
	  * portion of this clause as a `Chain` of relation subjects. This is equivalent to `select(this.fullRow)`.
	  * If there are any unbound parameters inside the explicit portion of this clause, the resulting ''select''
	  * will be based on the type `Nothing` (rather than `Implicit`), making it statically impossible to use as a part
	  * of any other [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] (and, in particular, inside
	  * the `Outer` clause).
	  * @see [[net.noresttherein.oldsql.sql.FromClause#InnerRow InnerRow]]
	  */
	def select_* :SelectSQL[Base, InnerRow, _] = select(innerRow)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]



	private[sql] def concrete_FromClause_subclass_must_extend_DiscreteFrom_or_GroupedFrom :Nothing
}






/** A companion object for the [[net.noresttherein.oldsql.sql.FromClause FromClause]] trait representing
  * somewhat generalized ''from'' clauses of SQL select statements. It serves primarily as a namespace
  * for related classes and implicit values.
  */
object FromClause {

//	trait EmptyFrom extends DiscreteFrom { thisClause =>
//
//		override type Params = @~
//		override type FullRow = @~
//
//		protected[sql] override def extend[T[O] <: BaseMapping[S, O], S]
//		                                  (right :LastRelation[T, S], filter :SQLBoolean[Generalized AndFrom T])
//				:this.type EmptyJoin T =
//			EmptyJoin.narrow(this, right)(filter)
//
//
//		override type InnerParams = @~
//		override type InnerRow = @~
//
//		/** Helper type providing the definition of `EmptyJoin.AsSubselectOf[F]`. */
//		type BaseSubselectOf[+F <: FromSome, T[O] <: MappingAt[O]] <: FromClause AndFrom T {
//			type Explicit = thisClause.Explicit AndFrom T
////			type AsSubselectOf[+O] = thisClause.BaseSubselectOf[O, T]
//		}
//
//		@inline private[sql] final def bridgeAsSubselectOf[F <: FromSome, T[O] <: BaseMapping[S, O], S]
//		                               (newOuter :F, next :LastRelation[T, S], filter :SQLBoolean[Generalized AndFrom T])
//				:BaseSubselectOf[F, T] {
//					type Implicit = newOuter.Generalized
//					type Outer = newOuter.Self
//					type Explicit = thisClause.Explicit AndFrom T
////					type AsSubselectOf[+O] = thisClause.BaseSubselectOf[O, T] {
////						type Explicit = thisClause.Explicit AndFrom T
////						type AsSubselectOf[O] = thisClause.BaseSubselectOf[O, T]
////					}
//				} =
//			baseSubselectOf[F, T, S](newOuter, next, filter)
//
//		protected def baseSubselectOf[F <: FromSome, T[O] <: BaseMapping[S, O], S]
//		                             (newOuter :F, next :LastRelation[T, S], filter :SQLBoolean[Generalized AndFrom T])
//				:BaseSubselectOf[F, T] {
//					type Implicit = newOuter.Generalized
//					type Outer = newOuter.Self
//					type Explicit = thisClause.Explicit AndFrom T
////					type AsSubselectOf[+O] = thisClause.BaseSubselectOf[O, T]
//				}
//	}



	/** A marker trait for all non empty clauses, i.e. those which contain at least one relation.
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] is its more prominent subtype, used for
	  * ''from'' clauses without a ''group by'' clause (conforming to
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]].
	  * All [[net.noresttherein.oldsql.sql.GroupByClause grouped]] clauses are automatically subtypes
	  * of this type, too.
	  */
	trait NonEmptyFrom extends FromClause { thisClause =>
		override type LastTable[F <: FromClause] = JoinedRelation[F, LastMapping]
		override type FromLast <: NonEmptyFrom

		type Self <: FromLast {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Self = thisClause.Self
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
			type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+F <: NonEmptyFrom] = thisClause.FromSubselect[F]
		}

		override type This <: NonEmptyFrom {
			type LastMapping[O] = thisClause.LastMapping[O]
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Self = thisClause.Self
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
			type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+F <: NonEmptyFrom] = thisClause.FromSubselect[F]
		}

		override def lastAsIn[E <: FromClause](implicit extension :FromLast PrefixOf E) :JoinedRelation[E, LastMapping] =
			last.extend[E]

		override def isEmpty = false

		override def fullRow :ChainTuple[Generalized, FullRow] = fullRow(generalized)
		override def fullTableStack :LazyList[RelationSQL.AnyIn[Generalized]] = fullTableStack(generalized)

		override def filter :SQLBoolean[Generalized] = filter(generalized)
		override def innerRow :ChainTuple[Generalized, InnerRow] = innerRow(generalized)
		override def innerTableStack :LazyList[RelationSQL.AnyIn[Generalized]] = innerTableStack(generalized)
		override def outerRow :ChainTuple[Generalized, OuterRow] = outerRow(generalized)(explicitSpan)


		override type FromRelation[T[O] <: MappingAt[O]] = Self Subselect T

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                 (first :Relation[M])
		//               (implicit cast :InferSubject[this.type, Subselect, M, T, S])
		                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:FromRelation[T] =
			Subselect[Self, T, S](self, LastRelation(cast(first)))(True)


		override type FromSubselect[+F <: NonEmptyFrom] = F#AsSubselectOf[Self] {
			//type Self <: AsSubselectOf[Outer]
			type Implicit = thisClause.Generalized
			type Outer = thisClause.Self
			type InnerRow <: F#InnerRow
		}

		override def from[F <: NonEmptyFrom with FreeFrom](subselect :F)
				:subselect.AsSubselectOf[Self] { type Implicit = thisClause.Generalized; type Outer = thisClause.Self } =
			subselect.asSubselectOf(self)

		override def fromSubselect[F <: NonEmptyFrom]
		                          (subselect :F)(implicit extension :subselect.Implicit ExtendedBy Generalized)
				:subselect.AsSubselectOf[Self] { type Implicit = thisClause.Generalized; type Outer = thisClause.Self } =
			subselect.asSubselectOf(self)

	}



	/** A `FromClause` refinement which is the upper bound of all ''from'' clauses with a statically known `Generalized`
	  * type. Such clauses are referred to as ''generalized'' for shirt. If `F &lt;: GeneralizedFrom`, then
	  *   a) `F` is ''complete'' - the number of joined relations is statically known, and
	  *   b) `F &lt;: F#Generalized` - all 'join' kinds are narrowed down at least to their `Generalized` form
	  *     (although this can't be expressed in the type system).
	  * Clauses conforming to this type generally offer full functionality, in particular regarding access to
	  * their mappings. The above statement holds only for subtypes which conform to this type naturally,
	  * rather than through refinement. In particular, a `from :GeneralizedFrom` is an abstract type for which
	  * no functions depending on the statically known structure (i.e., based on implicit evidence), in particular
	  * access to mappings, will be available
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ConcreteFrom]]
	  */
	type GeneralizedFrom = FromClause {
		type FromLast >: this.type <: FromClause
		type Generalized >: this.type <: FromLast
	}

	/** A `FromClause` refinement which is the upper bound of all ''from'' clauses with a statically known
	  * `Self` type; such clauses are referred to as ''concrete''. If `F &lt;: ConcreteFrom`, then
	  *   a) `F` is ''complete'' - the number of joined relations is statically known,
	  *   b) `F &lt;: GeneralizedFrom` and `F &lt;: F#Generalized` (although the latter cannot be expressed
	  *      in the type system),
	  *   c) `F &lt;: F#Self` - there are no abstract join kinds in the type signature (though this cannot be expressed
	  *      in the type system either).
	  *   d) result types of all methods returning other clauses are likewise `ConcreteFrom` instances, although
	  *      again this cannot be expressed directly.
	  * In essence, conforming to the this type signifies that a type is concrete with respect to the join aspect,
	  * although the mapping type parameters may still be abstract types. Conforming ''naturally'' to this type implies
	  * that all functions, in particular those depending on implicit evidence, are available and their result types
	  * are statically known. This does not hold if a clause conforms only through refinement, in particular by
	  * being declared as `ConcreteFrom`, as this type alias carries no information in itself.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom]]
	  */
	type ConcreteFrom = FromClause {
		type FromLast >: this.type <: FromClause
		type Generalized >: this.type <: FromLast
		type Self >: this.type <: Generalized
	}



	/** A `FromClause` of a top level, independent ''select'' - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (is not a ''from'' clause of a subselect
	  * of some other select). In order to conform naturally (rather than by refinement) to `OuterForm`, the clause
	  * must be ''complete'', that is its static type must start with either
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] - rather than an
	  * abstract type hiding an unknown prefix - and its `Generalized` supertype must be known, meaning all join kinds
	  * should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
	  * (or [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]]/[[net.noresttherein.oldsql.sql.GroupByAll.ByAll ByAll]]).
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * An outer clause is the only place where unbound join parameters are allowed, but is not in itself a valid
	  * base for a [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]] in that case. For that, see its subtype
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]]. For convenience, this type has sibling
	  * definitions with lower, often occurring, type bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterDiscreteFrom OuterDiscreteFrom]],
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterFromSome OuterFromSome]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterGroupedFrom OuterGroupedFrom]]; they all conform to this type.
	  */ //we opt for a minimalistic definition so that more abstract method results conform to it, rather than have more info
	type OuterFrom = FromClause {
		//type FromLast >: this.type <: FromClause //these properties would hold for static types, but are not preserved
		//type Generalized >: this.type <: FromLast// on generalization
		type Implicit = FromClause  //everything down to Dual is explicit
	}

	/** A `FromClause` of a top level, independent ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `OuterDiscreteForm`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than an abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]].
	  * See [[net.noresttherein.oldsql.sql.FromClause.OuterFromSome OuterFromSome]] for a variant with a
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] upper bound. An `OuterDiscreteFrom` can still
	  * contain (unbound) parameters; [[net.noresttherein.oldsql.sql.FromClause.FreeDiscreteFrom FreeDiscreteFrom]]
	  * is the specialization of this type without any `JoinParam` 'joins'.
	  * @see [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]]
	  */
	type OuterDiscreteFrom = DiscreteFrom {
		type Implicit = FromClause
	}

	/** A refinement of the [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] type, representing a top level,
	  * independent and non-empty ''from'' clause of a ''select'' without a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]]
	  * joins (is not a ''from'' clause of a subselect of some other select). In order to conform naturally
	  * (rather than by refinement) to `OuterFromSome`, the clause must be ''complete'', that is its static type
	  * must start with either [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]]
	  * - rather than an abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is an 'ungrouped' subtype of the more generic [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]].
	  *  An `OuterDiscreteFrom` can still contain (unbound) parameters;
	  *  [[net.noresttherein.oldsql.sql.FromClause.FreeDiscreteFrom FreeDiscreteFrom]] is the specialization of this type
	  *  without any `JoinParam` 'joins'.
	  */
	type OuterFromSome = FromSome {
		type Implicit = FromClause
	}

	/** A `FromClause` of a top level, independent ''select'' with a ''group by'' clause - one which doesn't contain
	  * any [[net.noresttherein.oldsql.sql.Subselect Subselect]] joins (is not a ''from'' clause of a subselect
	  * of some other select). In order to conform naturally (rather than by refinement) to `OuterDiscreteForm`,
	  * the clause must be ''complete'', that is its static type must start with either
	  * [[net.noresttherein.oldsql.sql.Dual Dual]] or [[net.noresttherein.oldsql.sql.From From]] - rather than
	  * an abstract type hiding an unknown prefix - and its `Generalized` supertype must be known,
	  * meaning all join kinds should be narrowed down at least to the level of
	  * [[net.noresttherein.oldsql.sql.Join Join]]/[[net.noresttherein.oldsql.sql.JoinParam JoinParam]].
	  * Despite the requirement for completeness, this type does not conform in itself to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], maintaining a minimalistic
	  * definition to preserve the property of being an outer clause by as many transformation functions as possible.
	  * This is a 'grouped' subtype of the more generic [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]].
	  * See [[net.noresttherein.oldsql.sql.FromClause.OuterDiscreteFrom OuterDiscreteFrom]] for a non grouped variant
	  * with a [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]] upper bound. An `OuterGroupedFrom`
	  * can still contain (unbound) [[net.noresttherein.oldsql.sql.GroupParam GroupParam]] parameters;
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeGroupedFrom FreeGroupedFrom]] is the specialization of this type
	  * without any `GroupParam` 'joins'.
	  * @see [[net.noresttherein.oldsql.sql.DiscreteFrom DiscreteFrom]]
	  */
	type OuterGroupedFrom = GroupByClause {
		type Implicit = FromClause
	}

	/** A `FromClause` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]] or
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * synthetic joins. Representing a single ''from'' clause (and not one of a nested subselect), only such clauses
	  * can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], but there is no actual subtype
	  * relationship between the two in the type system. A `FreeFrom` will however always by a subtype of
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]], which groups all outer clauses, including
	  * those with unbound parameters, and [[net.noresttherein.oldsql.sql.FromClause.ParameterlessFrom ParameterlessFrom]].
	  * For convenience, this type has two sibling types with narrowed upper bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeDiscreteFrom FreeDiscreteFrom]],
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeFromSome FreeFromSome]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeGroupedFrom FreeGroupedFrom]].
	  */
	type FreeFrom = FromClause {
		type Implicit = FromClause
		type Base = FromClause
		type DefineBase[+I <: FromClause] = I //No unbound parameters; crucial for use in SelectSQL
		type Params = @~ //implied from the other two, but nice to be ParameterlessFrom
	}

	/** A `FromClause` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupByAll, GroupByAll]]/[[net.noresttherein.oldsql.sql.GroupByAll.ByAll By All]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], but there is no actual subtype
	  * relationship between the two in the type system. A `FreeDiscreteFrom` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterDiscreteFrom OuterDiscreteFrom]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.FromClause.ParameterlessFrom ParameterlessFrom]].
	  */
	type FreeDiscreteFrom = DiscreteFrom {
		type Implicit = FromClause
		type Base = FromClause
		type DefineBase[+I <: FromClause] = I
		type Params = @~
	}

	/** A non empty `FromClause` without any [[net.noresttherein.oldsql.sql.Subselect Subselect]],
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]],
	  * or [[net.noresttherein.oldsql.sql.GroupByAll, GroupByAll]]/[[net.noresttherein.oldsql.sql.GroupByAll.ByAll By All]]
	  * joins. Representing a single ''from'' clause (and not one of a nested subselect), without a ''group by'' clause,
	  * it can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], but there is no subtype
	  * relationship between the two in the type system. A `FreeFromSome` will however always by a subtype of
	  * its more generic variants, [[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]],
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeDiscreteFrom FreeDiscreteFrom]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterFromSome OuterFromSome]] (which groups all outer clauses,
	  * including those with unbound parameters), as well as
	  * [[net.noresttherein.oldsql.sql.FromClause.ParameterlessFrom ParameterlessFrom]].
	  */
	type FreeFromSome = FromSome {
		type Implicit = FromClause
		type Base = FromClause
		type DefineBase[+I <: FromClause] = I
		type Params = @~
	}

	/** A `FromClause` with a ''group by'' clause but without any [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * or [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * synthetic joins. Representing a single ''from'' clause (and not one of a nested subselect), only such clauses
	  * can be used as a basis of (top level) SQL ''selects''. In order to conform naturally (rather than by refinement),
	  * a type must be ''complete'', and the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form
	  * of every component clause must be known; such types will automatically also conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]], but there is no actual subtype
	  * relationship between the two in the type system. A `FreeGroupedFrom` will however always by a subtype of
	  * [[net.noresttherein.oldsql.sql.FromClause.OuterGroupedFrom OuterGroupedFrom]], which groups all outer clauses,
	  * including those with unbound parameters,
	  * and [[net.noresttherein.oldsql.sql.FromClause.ParameterlessFrom ParameterlessFrom]].
	  * For convenience, this type has two sibling types with narrowed upper bounds:
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeDiscreteFrom FreeDiscreteFrom]],
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeFromSome FreeFromSome]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.FreeGroupedFrom FreeGroupedFrom]].
	  */
	type FreeGroupedFrom = GroupByClause {
		type Implicit = FromClause
		type Base = FromClause
		type DefineBase[+I <: FromClause] = I
		type Params = @~
	}



	/** A ''from'' clause is a ''subclause'' of another clause `F` if its ''implicit'' portion is a supertype of `F`,
	  * meaning it is a potential candidate for creating a subselect expression nested under that clause. Whether or not
	  * it will be actually possible will depend on whether there are any
	  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]]
	  * 'joins' in its explicit portion. It is a supertype of all valid subselect clauses defined as
	  * [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[F] ]], introduced as a result type
	  * for functions creating subselects which can't statically guarantee the lack of unbound parameters.
	  */
	type SubclauseOf[-F <: FromClause] = FromClause {
		type Implicit >: F <: FromClause //F may still have a longer prefix hidden under FromClause at start of Base
	}

	/** An upper bound for `FromClause` subtypes representing valid ''from'' clauses of subselects of a select
	  * with the ''from'' clause `F`. `S &lt;: AsSubselectOf[F]` if and only if `S` either:
	  *   - has the form of `F Subselect M1 J2 M2 ... JN MN` for some mappings `M1...MN`
	  *     and join types `J2...JN`, with no `Subselect` or `JoinParam` joins among them.
	  *   - is a complete, non subselect clause: `S &lt;: `[[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]]
	  *     with no `JoinParam` joins.
	  * Clauses conforming to `AsSubselectOf[F]` can use all the mappings/entities
	  * which are a part of `F`, but they are not a part of any select expressions created from that clause. This allows
	  * the use of nested select queries which depend on values from the ''from'' clause of the outer select.
	  *
	  * `S &lt;: SubselectOf[F]` does not imply that any clause `s :S` is a proper subselect of any clause  `f :F`,
	  * in the sense that `s` contains all of the relations from `f`. Instead, it witnesses that `F` satisfies
	  * all the implicit dependencies of `S`: a [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]] created from `s`
	  * is an `SQLExpression[F, _]`. If `F &lt;: GeneralizedFrom`, then such a subselect expression can be used
	  * as a part of both ''select'' and ''where'' clauses of a select from `F`. On the other hand,
	  * an `SQLExpression[F, T]` is convertible to `SQLExpression[S, T]` by a call
	  * [[net.noresttherein.oldsql.sql.SQLExpression#stretch stretch]]`(s)(s.explicitSpan)`.
	  *
	  * Perhaps counterintuitively, this type is contravariant rather than covariant. There are two reasons behind it:
	  * one, preventing any type from becoming a subselect clause of a clause with a more abstract prefix (with fewer
	  * relations specified), ensuring that the full implicit mapping list of the subselect clause is accounted for and,
	  * two, treating all join kinds as equivalent for this purpose. Note that subselects may be nested to an arbitrary
	  * depth and only directly nested subselects of `F` conform to this type.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubclauseOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Nested]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	type SubselectOf[-F <: FromClause] = FromClause {
		type Base >: F <: FromClause //F may still have an abstract prefix
	}

	/** An upper bound for all ''from'' clauses of subselect clauses, that is `FromClause` subtypes with a
	  * `Subselect` join in the type definition: `S &lt;: F Subselect T1 ... TN . ` for some type `F &lt;: FromClause`
	  * and mapping type constructors `T1 ... TN`. For a type to conform to `SubselectFrom`, the join kinds in
	  * the explicit part of the clause (after the last subselect) must be statically known to be `Join` subtypes
	  * or ''group by'' elements.
	  * Note that ''outer'' clauses (without any `Subselect` joins) are considered subselect clauses of any other
	  * clause in terms of [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]], but do not conform
	  * to this type.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type SubselectFrom = FromClause {
		type Implicit <: NonEmptyFrom //this holds only if there is a Subselect involved (meaning clear way to the Subselect)
		type Base = Implicit //this excludes clauses with an UnboundParam in the explicit portion
		type DefineBase[+I <: FromClause] = I //only to preserve the validity on rebasing and to conform to Nested
	}



	/** An upper bound for all ''from'' clauses which do not contain any `JoinParam` 'joins' in their concrete type.
	  * In order to prove this conformity the clause type must be complete and cannot contain `AndFrom` joins
	  * (all joins in its static type must be of some proper `AndFrom` subclass). It is possible however to propagate
	  * this constraint to incomplete clauses simply by declaring them as `ParameterlessFrom Join X ...` instead of
	  * `FromClause Join X ...`.
	  */
	type ParameterlessFrom = FromClause {
		type Params = @~
	}

	/** An upper bound for all ''from'' clauses which are known to contain at least one `JoinParam` 'join' in their
	  * static type. Note that a `FromClause` not conforming to this type does not mean that it indeed contains
	  * no parameters, as the information may have been lost by type abstraction.
	  */
	type ParameterizedFrom = FromSome {
		type Params <: Chain ~ _
	}






	/** Extension methods for `FromClause` classes which benefit from having a static, invariant self type. */
	implicit class FromClauseExtension[F <: FromClause](val clause :F) extends AnyVal {

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def entities :JoinedEntities[F] = new JoinedEntities[F](clause)

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `JoinedRelation` SQL expressions parameterized with any of the mapping types listed
		  * by this clause and a supertype of this clause as the base for the expression.
		  */
		@inline def relations :JoinedRelations[F] = new JoinedRelations[F](clause)


		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
		  * (meaning it is a valid ''from'' clause for a subselect of its `Implicit` clause) and, if so, casts `this`
		  * to `outer.`[[net.noresttherein.oldsql.sql.FromClause#Nested Nested]] and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a subselect clause, or `None` otherwise.
		  */
		@inline def ifSubselect[T](map :F with clause.outer.Nested => T) :Option[T] =
			if (clause.isValidSubselect)
				Some(map(clause.asInstanceOf[F with clause.outer.Nested]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]]
		  * (meaning it is a top-level clause with an empty ''implicit'' portion) and, if so,
		  * casts `this` to `F with OuterFrom` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is an ''outer'' clause, or `None` otherwise.
		  */
		@inline def ifOuter[T](map :F { type Implicit = FromClause } => T) :Option[T] =
			if (!clause.isSubselect)
				Some(map(clause.asInstanceOf[F { type Implicit = FromClause }]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.FromClause.FreeFrom FreeFrom]]
		  * (meaning it is a valid ''from'' clause for a ''free'' select - its ''implicit'' portion is empty and
		  * it contains no [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters) and, if so,
		  * casts `this` to `F with FreeFrom` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a ''free'' clause, or `None` otherwise.
		  */
		@inline def ifFree[T](map :F with FreeFrom => T) :Option[T] =
			if (!clause.isSubselect && !clause.isParameterized)
				Some(map(clause.asInstanceOf[F with FreeFrom]))
			else None

		/** Checks if this clause conforms to [[net.noresttherein.oldsql.sql.FromClause.ParameterlessFrom ParameterlessFrom]]
		  * (meaning it has no [[net.noresttherein.oldsql.sql.UnboundParam unbound]] parameters in its definition) and,
		  * if so, casts `this` to `F with ParameterlessFrom` and passes it to the given function.
		  * @return result of executing the given function in `Some` if this clause is a paramterless clause, or `None` otherwise.
		  */
		@inline def ifParameterless[T](map :F { type Params = @~ } => T) :Option[T] =
			if (clause.isSubselect && clause.isParameterized)
				Some(map(clause.asInstanceOf[F { type Params = @~ }]))
			else None

	}






	/** Extension methods for `OuterFrom` objects (''from'' clauses without any `Subselect`s which can serve
	  * as the basis for independent selects). It provides methods for introducing unbound parameters
	  * to the clause in the form of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins',
	  * which can be substituted with
	  */
	implicit class OuterFromExtension[F <: OuterFromSome](private val clause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm] :F WithParam X = JoinParam(clause, ParamRelation[X]())

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @param name the suggested name for the parameter in the generated SQL, as specified by JDBC.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(clause, ParamRelation[X](name))

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @tparam N a string literal used as the label for the mapping and suggested parameter name.
		  * @tparam X parameter type.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.UnboundParam.FromParam]]
		  */
		@inline def param[N <: Label, X](implicit form :SQLForm[X], name :ValueOf[N]) :F JoinParam (N ?: X)#T =
			JoinParam(clause, form ?: (name.value :N))

	}







	/** A facade to a ''from'' clause of type `F`, providing access to mappings for all relations listed in its type.
	  * The `Origin` type of every returned `M[O] &lt;: MappingAt[O]` instance (and, by transition, its components)
	  * is the generalized super type of `F` formed by replacing all mappings preceding `M` in its type definition
	  * with the abstract `FromClause`, joined with `M` using `AndFrom`. All following joins are generalized,
	  * abstracting over actual join used and preserving only the distinction between joins, subselects and params.
	  * As the result, the mapping `M` becomes the first mapping in the origin clause, and the number of all mappings
	  * defines the index of the mapping in this clause (when counting from the right). There is an implicit conversion
	  * available from each mapping `M` with the origin type `O` of that form into a `SQLExpression[O, M#Subject]`,
	  * so the components of the root mapping can be used directly inside SQL expressions. The least upper bound
	  * of all such `Origin` types is `this.Generalized` and all expressions built using them will also be based
	  * on `this.Generalized`.
	  */
	implicit class JoinedEntities[F <: FromClause]
	                             /** The `FromClause` with the accessed relations. Note that inside factory methods
	                               * for `F` it might not be fully initialized - in that case only the `left`
	                               * and `right`/`last` properties are initialized and accessing any of its methods
	                               * yields undefined results.
	                               */
	                             (val clause :F)
        extends AnyVal
	{
		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `JoinedRelation` SQL expressions parameterized with any of the mapping types listed
		  * by this clause and a supertype of this clause as the base for the expression.
		  */
		@inline def relation :JoinedRelations[F] = new JoinedRelations(clause)
		//todo: think of a better name, either the method or this class
		/** Returns the `Mapping` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject[F, E]) :get.T[get.G] = get(clause).mapping

		/** Returns the `Mapping` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.JoinLike.as JoinLike.as]]
		  * method.
		  * @param alias a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def apply[A <: Label](alias :A)(implicit get :ByLabel[F, A]) :get.T[get.G] =
			get(clause).mapping

		/** Returns the `Mapping` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByTypeConstructor[F, M]) :M[get.G] =
			get(clause).mapping


		/** Returns the `Mapping` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :ByIndex[F, N]) :get.T[get.G] = get(clause).mapping


		/** Returns the `Mapping` instance for the last relation in this clause (counting from the right). */
		def last(implicit get :ByIndex[F, -1]) :get.T[get.G] = get(clause).mapping

		/** Returns the `Mapping` instance for the second-last relation in this clause (counting from the right). */
		def prev(implicit get :ByIndex[F, -2]) :get.T[get.G] = get(clause).mapping



		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor[F, ParamRelation[X]#Param]) :get.T[get.G] =
			get(clause).mapping

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `UnboundParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, N]) :get.T[get.G] =
			get(clause).mapping

		/** Returns the `Mapping` instance for the `n`-th unbound parameter with name `name` as its label.
		  * The returned parameter is the same as the one at the `n`-th position in `F#FullRow`. In particular,
		  * any `JoinParam` joins present under the grouped section of a `GroupByAll` clause, which correspond
		  * to unavailable relations, are not excluded from indexing: any such relations will remain unavailable,
		  * creating a gap between available indices.
		  */
		def ?[N <: Numeral](n :N)(implicit get :ByParamIndex[F, N]) :get.T[get.G] =
			get(clause).mapping


		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension#subselect subselect]]
		  * extension method available for any `FromClause`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause, forgoing the `Subselect` join in the result.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[M])
		                (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
		//              (implicit cast :InferSubject[clause.type, Subselect, M, T, S]) :F#Extend[Subselect, M] =
				:clause.FromRelation[T] =
			{ val res = clause.from(table); res }  //decouple type inference from the result type

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `FromClause`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension#subselect subselect]]
		  * extension method available for any `FromClause`; the difference is that this method works also
		  * for empty 'outer' clauses, creating another 'outer' clause, forgoing the `Subselect` join in the result.
		  * @param other a non subselect `FromClause` listing relations which should be appended to this clause
		  *              (i.e. joined, preserving the order).
		  * @return `clause.`[[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *        if `F` is not empty and `R` otherwise. The result conforms to
		  *        `clause.`[[net.noresttherein.oldsql.sql.FromClause#Nested Nested]] and
		  *        [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[R <: FreeFromSome](other :R) :clause.FromSubselect[R] = clause.from(other)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The ''explicit'' list of relations in the clause is initialized with the explicit portion
		  * of the `FromClause` given as the argument and can be further expanded by joining with additional relations.
		  * Its ''implicit'' portion becomes this relation. The created clause is represented
		  * as a linearization of the two parts: this clause, followed by a `Subselect` pseudo join linking the
		  * first relation following the last `Subselect` (or `Dual`, if `F =:= Dual`) of `R`, with the rest
		  * of the relations following suit. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * @param other a subselect clause of some clause extended by this clause.
		  * @return `clause.`[[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *        if `F` is not empty and `R` otherwise. The result conforms to
		  *        `clause.`[[net.noresttherein.oldsql.sql.FromClause#Nested Nested]] and
		  *        [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def fromSubselect[R <: FromSome]
		                         (other :R)(implicit extension :other.Implicit ExtendedBy clause.Generalized)
				:clause.FromSubselect[R] =
			clause.fromSubselect(other)
	}



	/** A facade to a ''from'' clause of type `F`, providing access to all relations listed in its type as
	  * [[net.noresttherein.oldsql.sql.MappingSQL.JoinedRelation JoinedRelation]] SQL expressions, which can be used,
	  * either directly or through their components, as part of other SQL expressions - in particular `SQLBoolean`
	  * filters based on the `clause.Generalized`. The `FromClause` type parameter of every returned relation
	  * is the generalized super type of `F` formed by replacing all mappings preceding `M` in its type definition
	  * with the abstract `FromClause`, joined with `M` using `AndFrom`. All following joins are generalized,
	  * abstracting over actual join used and preserving only the distinction between joins, subselects and params.
	  * As the result, the mapping `M` becomes the first mapping in the origin clause, and the number of all mappings
	  * defines the index of the mapping in this clause (when counting from the right). Thus, the least upper bound
	  * of all returned relations is `JoinedRelation[this.Generalized, _]` and all expressions built using them
	  * will also be based on `this.Generalized`
	  */
	class JoinedRelations[F <: FromClause]
	                      /** The `FromClause` with the accessed relations. Note that inside factory methods for `F`
	                        * it might not be fully initialized - in that case only the `left` and `right`/`last`
	                        * properties are initialized and accessing any of its methods yields undefined results.
	                        */
	                     (val clause :F)
        extends AnyVal
	{
		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def entity :JoinedEntities[F] = new JoinedEntities[F](clause)

		/** Returns the `JoinedRelation` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject[F, E]) :JoinedRelation[get.G, get.T] = get(clause)

		/** Returns the `JoinedRelation` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByTypeConstructor[F, M]) :JoinedRelation[get.G, M] =
			get(clause)

		/** Returns the `JoinedRelation` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.JoinLike.as JoinLike.as]]
		  * method.
		  * @param alias a string literal which is used as the label of the accessed mapping
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def apply[A <: String with Singleton]
		         (alias :A)(implicit get :ByLabel[F, A]) :JoinedRelation[get.G, get.T] =
			get(clause)


		/** Returns the `JoinedRelation` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :ByIndex[F, N]) :JoinedRelation[get.G, get.T] =
			get(clause)

		/** Returns the `JoinedRelation` instance for the last relation in this clause (counting from the right). */
		def last(implicit get :ByIndex[F, -1]) :JoinedRelation[get.G, get.T] = get(clause)

		/** Returns the `JoinedRelation` instance for the second-last relation in this clause (counting from the right). */
		def prev(implicit get :ByIndex[F, -2]) :JoinedRelation[get.G, get.T] = get(clause)



		/** Returns the `JoinedRelation` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor[F, ParamRelation[X]#Param]) :JoinedRelation[get.G, get.T] =
			get(clause)

		/** Returns the `JoinedRelation` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `UnboundParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, N]) :JoinedRelation[get.G, get.T] =
			get(clause)

		/** Returns the `JoinedRelation` instance for the `n`-th unbound parameter with name `name` as its label.
		  * The returned parameter is the same as the one at the `n`-th position in `F#FullRow`. In particular,
		  * any `JoinParam` joins present under the grouped section of a `GroupByAll` clause, which correspond
		  * to unavailable relations, are not excluded from indexing: any such relations will remain unavailable,
		  * creating a gap between available indices.
		  */
		def ?[N <: Numeral](n :N)(implicit get :ByParamIndex[F, N]) :JoinedRelation[get.G, get.T] =
			get(clause)



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension#subselect subselect]]
		  * extension method available for any `FromClause`; the difference is that this method works also
		  * on empty 'outer' clauses, creating a `From[T]` clause, forgoing the `Subselect` join in the result.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[M])
		                (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
		//              (implicit cast :InferSubject[clause.type, Subselect, M, T, S]) :F#Extend[Subselect, M] =
				:clause.FromRelation[T] =
			{ val res = clause.from(table); res } //decouple type inference from the result type

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `FromClause`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * This is similar the [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension#subselect subselect]]
		  * extension method available for any `FromClause`; the difference is that this method works also
		  * for empty 'outer' clauses, creating another 'outer' clause, forgoing the `Subselect` join in the result.
		  * @param other a non subselect `FromClause` listing relations which should be appended to this clause
		  *              (i.e. joined, preserving the order).
		  * @return `clause.`[[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *        if `F` is not empty and `R` otherwise. The result conforms to
		  *        `clause.`[[net.noresttherein.oldsql.sql.FromClause#Nested Nested]] and
		  *        [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[R <: FreeFromSome](other :R) :clause.FromSubselect[R] = clause.from(other)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The ''explicit'' list of relations in the clause is initialized with the explicit portion
		  * of the `FromClause` given as the argument and can be further expanded by joining with additional relations.
		  * Its ''implicit'' portion becomes this relation. The created clause is represented
		  * as a linearization of the two parts: this clause, followed by a `Subselect` pseudo join linking the
		  * first relation following the last `Subselect` (or `Dual`, if `F =:= Dual`) of `R`, with the rest
		  * of the relations following suit. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  *
		  * @param other a subselect clause of some clause extended by this clause.
		  * @return `clause.`[[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf AsSubselectOf]]`[F, Subselect]`
		  *        if `F` is not empty and `R` otherwise. The result conforms to
		  *        `clause.`[[net.noresttherein.oldsql.sql.FromClause#Nested Nested]] and
		  *        [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]`[clause.Generalized]`.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def fromSubselect[R <: FromSome]
		                         (other :R)(implicit extension :other.Implicit ExtendedBy clause.Generalized)
				:clause.FromSubselect[R] =
			clause.fromSubselect(other)
	}






	/** Implicit evidence that the clause `F` is a ''direct'' extension of `P`, that is type `F` results
	  * from applying some type constructor to `P`. This covers both the [[net.noresttherein.oldsql.sql.Extended Extended]]
	  * class hierarchy which actually add a relation to the clause `P` and
	  * the [[net.noresttherein.oldsql.sql.DecoratedFrom DecoratedFrom]] hierarchy of clauses which only modify
	  * the wrapped clause without adding new relations. Hence, the `extension` property always represents
	  * the length of `0` or `1`. This class is used by various other implicits in order to recursively deconstruct
	  * the type `F`, for example for the relation accessor methods, and is introduced to allow their usage with
	  * any future or custom `FromClause` implementations which provide an implicit value of this class in their
	  * companion object.
	  * @tparam F the 'whole' clause extending (in the sense of
	  *           `P` [[net.noresttherein.oldsql.sql.FromClause.PrefixOf PrefixOf]] `F`) the clause `P`.
	  * @tparam P the largest subclause of `F` which doesn't equal `F` - typically the `FromClause` type argument
	  *           provided for `F`.
	  * @tparam U the upper bound on the clauses accepted by `F`'s type constructor in place of `P`.
	  *           If `F =:= C[P]` for some `C[X &lt;: U]`, then `F &lt;: C[U]` and it is the highest upper bound on `F`
	  *           which preserves the type constructor `C` intact.
	  */
	@implicitNotFound("I don't know how to extract a prefix clause ${P} (with an upper bound ${U}) from ${F}.\n" +
	                  "Missing implicit ClauseDecomposition[${F}, ${P}, ${U}.\n" +
	                  "You may wish to define an implicit Using.ExtendedDecomposition " +
	                  "or DecoratedFrom.DecoratorDecomposition for F if it is a custom implementation.")
	trait ClauseDecomposition[F <: FromClause, P <: U, U <: FromClause] {
		/** The type constructor of `F`, accepting a prefix clause. This is the generic version of `S[A]`,
		  * accepting any type conforming to `U`, but without the lower bound of `F` in the result.
		  */
		type E[+A <: U] <: FromClause

		/** The type constructor which, when applied to `P`, produces the type `F`.
		  * In reality, it is the same type as `E[A]`, but the lower bound on the type parameter allows it
		  * the lower bound of `F`.
		  */
		type S[+A >: P <: U] >: F <: FromClause

		def prefix[A >: P <: U] :A PrefixOf S[A]
		def extension[A <: U] :A PrefixOf E[A]

		def strip(clause :F) :P

		def upcast[A >: P <: U] :ClauseDecomposition[S[A], A, U]
		def cast[A <: U] :ClauseDecomposition[E[A], A, U]
	}


	@implicitNotFound("I don't know how to extract a prefix clause ${P} (with an upper bound ${U}) from ${F}.\n" +
	                  "Missing implicit ClauseComposition[${F}, ${P}, ${U}.\n" +
	                  "You may wish to define an implicit Extended.ExtendedDecomposition " +
	                  "or DecoratedFrom.DecoratorDecomposition for F if it is a custom implementation.")
	trait ClauseComposition[F <: FromClause, P <: U, U <: FromClause] extends ClauseDecomposition[F, P, U] {
		def apply[C <: U](template :F, clause :C) :E[C]
	}



	type LastTableBound[U <: NonEmptyFrom, T[O] <: MappingAt[O]] = U {
		type FromLast = U
		type LastMapping[O] = T[O]
	}

	/** Implicit evidence providing the `LastMapping` and `FromLast` types of the mapping `F`. An implicit value
	  * of this class exists for every type `F` which defines the eponymous types. While in many contexts
	  * this information could be obtained by simply refining the type of the accepted ''from'' clause,
	  * if the `FromLast` type parameter is not a free variable, it will be likely instantiated
	  * based on its bounds and failing the typing. An implicit type which defines these as member types instead
	  * eliminates the possibility of them being instantiated prematurely.
	  */
	@implicitNotFound("I cannot determine the last relation of ${F}. This typically means the type is too abstract " +
	                  "and doesn't define types LastMapping and FromLast or that F =:= Dual.")
	final class LastTableOf[-F <: NonEmptyFrom] private[FromClause]() { evidence =>

		/** The [[net.noresttherein.oldsql.sql.FromClause#LastMapping LastMapping]] type of the clause `F`. */
		type LastMapping[O] <: MappingAt[O]

		/** The [[net.noresttherein.oldsql.sql.FromClause#FromLast FromLast]] type of the clause `F`. */
		type FromLast >: F <: NonEmptyFrom

		@inline def apply(from :F) :JoinedRelation[FromLast, LastMapping] =
			from.last.asInstanceOf[JoinedRelation[FromLast, LastMapping]]
	}

	implicit def lastTableOf[U <: NonEmptyFrom, M[O] <: MappingAt[O]]
			:LastTableOf[LastTableBound[U, M]] { type FromLast = U; type LastMapping[O] = M[O] } =
		lastTableBound.asInstanceOf[LastTableOf[LastTableBound[U, M]] { type FromLast = U; type LastMapping[O] = M[O] }]

	private[this] val lastTableBound = new LastTableOf[NonEmptyFrom]




	/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the clause `F`.
	  * It is the number of relations available to any `SQLExpression[F, _]` and excludes any relations
	  * under the `GroupByAll` clause (that is, all relations since the `Subselect`/`Dual` preceding it, representing
	  * the ''from'' clause of a subselect with a ''group by'' clause), but including any grouping columns/mappings
	  * (the right side of the `GroupByAll` clause until the next `Subselect` or the end of the clause).
	  * This calculation happens completely on the type level and requires that the clause `F` starts with `Dual`
	  * (or `From`).
	  */
	@implicitNotFound("Can't calculate the size of the FROM clause ${F}.\nEither the clause is incomplete " +
		              "or the expected number ${N} is incorrect. Missing implicit: FromClauseSize[${F}, ${N}].")
	class FromClauseSize[-F <: FromClause, N <: Numeral] private (private val n :Int) extends AnyVal {
		/** The number of tables in the clause `F`. */
		@inline def size :N = n.asInstanceOf[N] //val size :N crashes scalac
	}

	object FromClauseSize {
		implicit val DualCount :FromClauseSize[Dual, 0] = new FromClauseSize[Dual, 0](0)

		implicit def joined[L <: FromClause, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                   (implicit count :FromClauseSize[L, M], plus :Inc[M, N]) :FromClauseSize[L Extended R, N] =
			new FromClauseSize[L Extended R, N](plus.n)

		implicit def grouped[F <: FromSome, G[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                    (implicit count :FromClauseSize[F#Outer, M], plus :Inc[M, N])
				:FromClauseSize[F GroupByAll G, N] =
			new FromClauseSize(plus.n)

		implicit def decorated[F <: FromClause, N <: Numeral](implicit count :FromClauseSize[F, N])
				:FromClauseSize[DecoratedFrom[F], N] =
			new FromClauseSize(count.size)
	}



	/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the explicit portion
	  * of the clause `F`, i.e., the relations which are members of the ''from'' clause of the most nested
	  * select/subselect based on `F`. This is the number of relations joined since the rightmost `Subselect` 'join'
	  * or `Dual`/`From`, if the clause is not a subselect clause. If the clause is a `GroupByClause`, only the relations
	  * to the right of the last `GroupByAll` are counted in order to reflect that the relations being grouped are
	  * not available to SQL expressions based on the clause. The calculation happens completely on the type level
	  * and requires that the kinds of all joins starting with the last `Subselect`/`Dual` are known at least to
	  * their `Generalized` form to correctly determine the beginning of the explicit suffix of the clause.
	  */
	@implicitNotFound("Can't count the number of relations in the most nested Subselect in ${F}.\n"+
	                  "Most likely the type's Generalized form is not known. " +
	                  "Missing implicit: SubselectClauseSize[${F}, ${N}].")
	class SubselectClauseSize[-F <: FromClause, N <: Numeral] private (private val n :Int) extends AnyVal {
		@inline def size :N = n.asInstanceOf[N]
	}


	object SubselectClauseSize {
		implicit val DualCount :SubselectClauseSize[Dual, 0] = new SubselectClauseSize[Dual, 0](0)

		implicit def from[T[O] <: MappingAt[O]] :SubselectClauseSize[From[T], 1] = new SubselectClauseSize[From[T], 1](1)

		implicit def subselect[T[O] <: MappingAt[O]] :SubselectClauseSize[NonEmptyFrom Subselect T, 1] =
			new SubselectClauseSize[NonEmptyFrom Subselect T, 1](1)

		implicit def grouped[L <: FromSome, T[O] <: MappingAt[O]] :SubselectClauseSize[L GroupByAll T, 1] =
			new SubselectClauseSize[L GroupByAll T, 1](1)

		implicit def extended[F <: L J R, L <: U, R[O] <: MappingAt[O],
		                      J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: FromClause, M <: Numeral, N <: Numeral]
		                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U],
		                      prev :SubselectClauseSize[L, M], plus :Inc[M, N]) :SubselectClauseSize[F, N] =
			new SubselectClauseSize[F, N](plus.n)

		implicit def decorated[D <: DecoratedFrom[F], F <: FromClause, N <: Numeral]
		                      (implicit decompose :Conforms[D, D, DecoratedFrom[F]], prev :SubselectClauseSize[F, N])
				:SubselectClauseSize[D, N] =
			new SubselectClauseSize[D, N](prev.n)
	}



	/** Implicit witness to the fact that the `Int` literal `N` is the number of ''known'' relations present
	  * in the clause `F`. It is the number of relations available to any `SQLExpression` based on the type `F`.
	  * This counts all relations joined since the leftmost clause, which may be abstract - a `FromClause`, `FromSome`,
	  * `DiscreteFrom`, `GroupByClause`, ignoring any relations which are grouped by a `GroupByAll` 'join'.
	  * This calculation happens completely on the type level and, for it to work, it requires that the join kinds
	  * are known at least to the level allowing distinguish between `GroupByAll` and others and,
	  * if a `GroupByAll` clause is present somewhere in the type, that all joins preceding it since the most recent
	  * `Subselect` (or `Dual`) are known at least to their `Generalized` form in order to properly exclude
	  * the relations under grouping from the count.
	  */
	@implicitNotFound("Failed to count the tables in ${F}. Is ${N} the number of mappings listed in its definition?\n" +
	                  "Note that witness TableCount[F, N] is invariant in type F, but requires that it starts " +
	                  "with one of FromClause, DiscreteFrom, FromSome, GroupByClause or Dual (including From[_]).")
	class TableCount[F <: FromClause, N <: Numeral] private[FromClause] (private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}


	object TableCount {
		implicit final val FromClauseHasZero :TableCount[FromClause, 0] = new TableCount[FromClause, 0](0)
		implicit final val DiscreteFromHasZero :TableCount[DiscreteFrom, 0] = new TableCount[DiscreteFrom, 0](0)
		implicit final val FromSomeHasZero :TableCount[FromSome, 0] = new TableCount[FromSome, 0](0)
		implicit final val DualHasZero :TableCount[Dual, 0] = new TableCount[Dual, 0](0)
		implicit final val GroupByClauseHasZero :TableCount[GroupByClause, 0] = new TableCount[GroupByClause, 0](0)

		implicit def extended[F <: L J R, L <: U, R[O] <: MappingAt[O],
		                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause, M <: Numeral, N <: Numeral]
		                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U],
		                      count :TableCount[L, M], inc :Inc[M, N])
				:TableCount[F, N] =
			new TableCount[F, N](inc.n)

		implicit def grouped[F <: FromSome, T[A] <: MappingAt[A], U <: Numeral, S <: Numeral, O <: Numeral, N <: Numeral]
		                    (implicit ungrouped :TableCount[F, U], subselect :SubselectTableCount[F, S],
		                              minus :Sum[O, S, U], plus :Inc[O, N])
				:TableCount[F GroupByAll T, N] =
			new TableCount[F GroupByAll T, N](plus.n)

		implicit def decorated[E <: D[F], F <: U, D[+C <: U] <: DecoratedFrom[C], U <: FromClause, N <: Numeral]
		                      (implicit decompose :DecoratorDecomposition[E, F, D, U], count :TableCount[F, N])
				:TableCount[E, N] =
			new TableCount[E, N](count.tables)

	}



	/** Implicit witness to the fact that the `Int` literal `N` is the number of relations in the ''from'' clause
	  * of the most nested subselect of `F`. This is the number of relations to the right since the most recent
	  * `Subselect` or `Dual`/`From` if `F &lt;: DiscreteFrom`, and the number of relations to the right of
	  * the most recent `GroupByAll` clause if `F &lt;: GroupByClause`. This calculation happens solely
	  * on the type level and requires that the `Generalized` form of the pertinent clause fragment is known
	  * in order to properly recognize the cut off point.
	  */
	@implicitNotFound("Failed to count joined relations since last Subselect in ${F}.\n" +
	                  "Most likely the type contains joins with unknown Generalized form or it starts with" +
	                  "an undefined prefix other than FromClause, DiscreteFrom, FromSome, Dual, From, GroupByClause. " +
	                  "Missing implicit SubselectTableCount[${F}, ${N}]")
	class SubselectTableCount[F <: FromClause, N <: Numeral] private (private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N]
	}


	object SubselectTableCount {
		implicit def zero[F <: FromClause](implicit count :TableCount[F, 0]) :SubselectTableCount[F, 0] =
			new SubselectTableCount[F, 0](0)

		implicit def from[T[A] <: MappingAt[A]] :SubselectTableCount[From[T], 1] =
			new SubselectTableCount[From[T], 1](1)

		implicit def subselect[F <: NonEmptyFrom, T[A] <: MappingAt[A]] :SubselectTableCount[F Subselect T, 1] =
			new SubselectTableCount[F Subselect T, 1](1)

		implicit def groupBy[F <: FromSome, T[A] <: MappingAt[A]] :SubselectTableCount[F GroupByAll T, 1] =
			new SubselectTableCount[F GroupByAll T, 1](1)

		implicit def extended[E <: L J R, L <: U, R[O] <: MappingAt[O],
		                      J[+A <: U, B[O] <: R[O]] <: A NonSubselect B, U <: FromClause, M <: Numeral, N <: Numeral]
		             (implicit decompose :ExtendedDecomposition[E, L, R, J, U],
		              prev :SubselectTableCount[E, M], inc :Inc[M, N])
				:SubselectTableCount[E, N] =
			new SubselectTableCount[E, N](inc.n)

		implicit def decorated[E <: D[C], C <: U, D[+B <: U] <: DecoratedFrom[B], U <: FromClause, N <: Numeral]
		                      (implicit decompose :DecoratorDecomposition[E, C, D, U], count :SubselectTableCount[C, N])
				:SubselectTableCount[E, N] =
			new SubselectTableCount[E, N](count.tables)

	}



	/** Implicit witness that `M` is the mapping for the first known relation in the clause `F`
	  * and `N` is the number of relations to its right. Any relations under the scope of a `GroupedByAll`
	  * (between the last `Subselect` or the first relation and `GroupedByAll`) are excluded from the count
	  * and `M` must not be such a relation. The clause `F` must be in the form
	  * `L E M G1 T1 ... Gn Tn`, where `L` is the upper bound of the left side of the extension `E` and all `Gi`
	  * are extension with a definite `Generalized` form. `From[M]` can replace `L E M` in the above definition.
	  * This evidence is invariant in `F` in order to maintain the invariant of `M` being the first `Mapping` type
	  * listed and preserve the correct relation count `N`. When `F` is used as the `Origin` type for the mapping `M`,
	  * this allows to track back any component of `M` back to the relation it originated from.
	  */
	@implicitNotFound("Relation mapping ${M} is not the first known mapping of the FROM clause ${F}: "+
	                  "no implicit TableShift[${F}, ${M}, ${N}].")
	class TableShift[F <: FromClause, M[O] <: MappingAt[O], N <: Numeral](private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}


	object TableShift {
		//this is the only one really needed by JoinedRelations + GetTable, which use FromLast
		implicit def firstAndFrom[T[A] <: MappingAt[A]] :TableShift[FromClause AndFrom T, T, 0] =
			new TableShift[FromClause AndFrom T, T, 0](0)
		//these firstXxx are here mostly to allow other uses of TableShift
		implicit def firstFrom[T[A] <: MappingAt[A]] :TableShift[From[T], T, 0] =
			new TableShift[From[T], T, 0](0)

		implicit def firstJoin[J[+L <: FromSome, R[A] <: MappingAt[A]] <: L Join R, T[A] <: MappingAt[A]]
				:TableShift[FromSome J T, T, 0] =
			new TableShift[FromSome J T, T, 0](0)

		implicit def firstSubselect[T[A] <: MappingAt[A]] :TableShift[NonEmptyFrom Subselect T, T, 0] =
			new TableShift[NonEmptyFrom Subselect T, T, 0](0)

		implicit def firstParam[P[A] <: ParamAt[A]] :TableShift[FromSome JoinParam P, P, 0] =
			new TableShift[FromSome JoinParam P, P, 0](0)

		implicit def firstGroupParam[P[A] <: ParamAt[A]] :TableShift[GroupByClause GroupParam P, P, 0] =
			new TableShift[GroupByClause GroupParam P, P, 0](0)

		implicit def firstBy[G[A] <: MappingAt[A]] :TableShift[GroupByClause ByAll G, G, 0] =
			new TableShift[GroupByClause ByAll G, G, 0](0)

		implicit def firstGroupBy[G[A] <: MappingAt[A]] :TableShift[FromSome GroupByAll G, G, 0] =
			new TableShift[FromSome GroupByAll G, G, 0](0)


		implicit def extended[E <: L J R, L <: U, R[O] <: MappingAt[O],
		                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause,
		                      T[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                     (implicit decompose :ExtendedDecomposition[E, L, R, J, U],
		                      prev :TableShift[L, T, M], inc :Inc[M, N])
				:TableShift[E, T, N] =
			new TableShift[E, T, N](inc.n)

		implicit def grouped[F <: FromSome, T[A] <: MappingAt[A],
		                     U <: Numeral, S <: Numeral, O <: Numeral, N <: Numeral]
		                    (implicit ungrouped :TableShift[F, T, U], subselect :SubselectClauseSize[F, S],
		                              minus :Sum[O, S, U], plus :Inc[U, N]) :TableShift[F GroupByAll T, T, N] =
			new TableShift[F GroupByAll T, T, N](plus.n)

		implicit def decorated[E <: D[C], C <: U, D[+B <: U] <: DecoratedFrom[B], U <: FromClause,
		                       T[O] <: MappingAt[O], N <: Numeral]
		                      (implicit decompose :DecoratorDecomposition[E, C, D, U], body :TableShift[C, T, N])
				:TableShift[E, T, N] =
			new TableShift[E, T, N](body.tables)

	}






	/** Namespace containing implicit witnesses for the presence of a certain relation in a given `FromClause`.
	  * They are used to access joined relations by subject, index, label, etc.
	  */
	object GetTable {

		/** Base trait for various implicit evidence used to find a particular relation based on some key type `X`.
		  * It carries the mapping type `T` associated with the found relation, `S` - the `FromLast` type
		  * of the found relation and `G`- a suffix clause of `F` which starts with `S` followed by all
		  * joins and relations just as they appear in the type `F`. The adaptation of the accessed relation
		  * from type `S` to `G` is done based on the [[net.noresttherein.oldsql.sql.FromClause.PrefixOf PrefixOf]]
		  * instance representing the extension.
		  */
		trait RelationEvidence[-F <: FromClause, X] {
			/** The accessed `Mapping` type, matching the key `X`. */
			type T[O] <: MappingAt[O]

			/** The supertype of the ''from'' clause `F`, in which the search takes place, resulting from replacing
			  * the join having the found relation as its right side with `S` - the `FromLast` type of that join.
			  */
			type G >: F <: FromClause

			/** The clause type that the original `JoinedRelation` at index `N` is  parameterized, i.e.
			  * the `FromLast` type of the accessed join.
			  */
			type S <: FromClause

			/** The ''negative'' index of the accessed relation, starting with `-1` for the rightmost relation in `F`.*/
			type I <: Numeral

			/** Extension of the initial clause `S` with the found relation as the last one to the final clause `G`. */
			def stretch :S PrefixOf G

			/** Getter for the matching relation. */
			def apply(from :F) :JoinedRelation[G, T] = table(from).extend(stretch)

			/** The returned relation based on its containig extension clause `S`, before extending it over `F`. */
			private[FromClause] def table(from :F) :JoinedRelation[S, T]
		}



		protected[GetTable] abstract class EvidenceTemplate[-E <: FromClause, X, O >: E <: FromClause,
		                                                    F <: FromClause, M[A] <: MappingAt[A], N <: Numeral]
		                                                   (override val stretch :F PrefixOf O)
			extends RelationEvidence[E, X]
		{
			override type T[A] = M[A]
			override type G = O
			override type S = F
			override type I = N
		}



		/** An implicit result of a search for a relation matching the key type `X` in the ''from'' clause `F`.
		  * The meaning of `X` and the matching rules are specified by the subclasses of the enclosing
		  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableByPredicate GetTableByPredicate]].
		  * This is an implementation class which acts solely as a wrapper over another evidence instance -
		  * typically a `Found[F, X, N]` - and is used as a base class for the final evidence class dedicated
		  * to the particular `GetTableByPredicate` subclass, such as `ByIndex`, `ByLabel`, etc. Its role
		  * is simply to lift the need to define delegates for all member types and methods with each
		  * such 'public' implicit definition.
		  * @tparam F the input `FromClause` from which the relation is taken.
		  * @tparam X the 'key' type used to match the `Mapping` types of all relations in search for an implicit
		  *           `Predicate[F, M[Any], X]`.
		  * @tparam O the `Origin` type for the mapping, which is a supertype of `F` resulting from replacing
		  *           the prefix clause which contains the accessed relation with its `FromLast` type.
		  * @tparam M the mapping of the accessed relation.
		  * @tparam N the negative index of the accessed relation in the clause `F` - it starts with -1
		  *           for the rightmost relation and decreases with each relation going to the left.
		  */
		private[GetTable]
		abstract class Delegate[-F <: FromClause, X, O >: F <: FromClause, M[A] <: MappingAt[A], N <: Numeral]
		                       (val found :RelationEvidence[F, X] { type T[A] = M[A]; type G = O; type I = N })
			extends RelationEvidence[F, X]
		{
			override type T[A] = M[A]
			override type G = O
			override type S = found.S
			override type I = N

			override def stretch :PrefixOf[S, G] = found.stretch

			override def table(from :F) :JoinedRelation[S, T] = found.table(from)
		}



		abstract class GetTableTemplate { this :GetTableByPredicate =>

			/** The upper type of the key type against which the relations are matched. */
			type Key

			/** The companion evidence class to this object. It is unnecessary for the subclasses
			  * to provide a definition, but doing so introduces an implicit conversion `Get => Found`
			  * of the lowest precedence, which allows adapting an existing implicit evidence `Get[F X]`
			  * for an abstract clause `F` into `Get[E, X]`, where `F ExtendedBy E`.
			  */
			type Get[-F <: FromClause, X <: Key] <: RelationEvidence[F, X]



			/** Implicit resolution of search for a mapping `M` in the `FromClause` `F` satisfying a `Predicate[F, M, X]`
			  * (that is, an `M` for which such an implicit value exists. The type of the mapping of the found relation
			  * is returned as the member type `T[O]`. In other words, an implicit value `found :Found[F, X] { type I = N }`
			  * witnesses that `found.T` is the mapping of the last relation (rightmost) in the clause `F` for which
			  * an implicit `Predicate[F, T, X]` exists, with `N` being the ''negative'' index of the mapping
			  * (starting with `-1` for the last mapping and decreasing). It is not the actual companion evidence class
			  * reported by this object for two reasons: first, various implementations introduce additional refinements
			  * over the standard [[net.noresttherein.oldsql.sql.FromClause.GetTable.RelationEvidence RelationEvidence]]
			  * interface and, second, because after erasure all GetTableByPredicate#Found` classes are equal
			  * and methods of the same signature accepting evidence from different `GetTableByEvidence` instances
			  * would clash with each other. The typical procedure is thus to implement the evidence resolution in
			  * means of `Found` and convert the final `Found` evidence into a `Get`. Leaving things at that would
			  * however not allow to perform the search based on an existing implicit `Get` (for example, to convert
			  * `Get[F, X] { type I = -2 }` into `Get[F Join T, X] { type I = -3 }`. For this reason, `GetTableByPredicate`
			  * subclasses typically introduce also a fallback conversion in the other direction - from `Get` to `Found`.
			  * As this would lead to inifinite loops when the evidence cannot be found (and reporting a 'diverging
			  * implicit expansion' error instead of 'implicit not found' with the customized message),
			  * `Found` instances obtained through scanning of the ''from'' clause `F` rather than from an implicit `Get`
			  * are always returned as its subclass
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableTemplate#Return Return]], and only values
			  * of that class are legible for conversion into the final `Get` evidence.
			  */
			@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}: missing implicit Found[F, X].")
			sealed trait Found[-F <: FromClause, X] extends RelationEvidence[F, X] {
				def how :GetTableByPredicate = GetTableTemplate.this
			}

			/** The working subtype of `Found` returned by all recursive implicit methods, but not the one
			  * converting an implicit `Get` into a `Found`. Only values of this type can be adapted as the final,
			  * 'public' implicit value `Get[F, X]`. This distinction is introduced to break the implicit resolution
			  * cycle `Found => Get => Found` which would result in a 'diverging implicit expansion' error.
			  */
			@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}: missing implicit Return[F, X]")
			sealed trait Return[-F <: FromClause, X] extends Found[F, X]


			implicit def found[F <: FromClause, X <: Key](implicit get :Get[F, X])
					:Found[F, X] { type T[O] = get.T[O]; type G = get.G; type I = get.I } =
				get match {
					case found :Found[_, _] =>
						found.asInstanceOf[Found[F, X] { type T[O] = get.T[O]; type G = get.G; type I = get.I }]
					case _ =>
						new Delegate[F, X, get.G, get.T, get.I](get) with Found[F, X]
				}

		}



		/** Implicit resolution of retrieving a relation from a ''from'' clause where the `Mapping` type satisfies
		  * some predicate. The search starts with the last (rightmost) relation in the clause and goes backward,
		  * looking for the first relation with an implicit `Predicate[M, X]`.
		  */
		abstract class GetTableByPredicate extends GetTableTemplate {

			/** Witnesses that mapping `M[Any]` (where `Any` is the `Origin` type) satisfies the search predicate
			  * and should be returned. Note that the predicate is contravariant with the regards to the mapping type,
			  * so all subtypes of a mapping `M[_]` satisfying the predicate satisfy it, too.
			  * @tparam M the type constructor of the mapping of the relation, accepting an arbitrary `Origin` type parameter.
			  * @tparam X the input key of the search: the type provided by the accessor, such as a label for a `LabeledMapping`.
			  */
			@implicitNotFound("The clause ${F} does not match the key type ${X} with mapping ${M}.")
			final class Predicate[-F <: FromClause, M[O] <: MappingAt[O], X]

			private[this] final val predicate = new Predicate[FromClause, MappingAt, Any]

			/** Subclasses can use this method as the implicit */
			protected def report[F <: FromClause, M[O] <: MappingAt[O], X] :Predicate[F, M, X] =
				predicate.asInstanceOf[Predicate[F, M, X]]


			/** A carrier of actual evidence `Found` over a subselect fragment of relations in the grouped portion
			  * of a `GroupByAll` clause, that is mappings that are not available for SQL expressions
			  * and can't be returned as the result of this search.
			  */
			@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}.\n" +
			                  "Missing implicit GroupedTunnel[${F}, ${U}, ${X}].")
			sealed trait GroupedTunnel[F <: FromClause, X] extends RelationEvidence[F, X] {
				/** The prefix of `G` ending with the last `Subselect` of `F`, that is the outer part of `G` */
				type O <: FromClause

				def prefix :S PrefixOf O
				def outerPart :O OuterClauseOf G
			}



			implicit def last[F <: NonEmptyFrom, M[O] <: MappingAt[O], X]
			                 (implicit last :LastTableOf[F] { type LastMapping[O] = M[O] }, pred :Predicate[F, M, X])
					:Return[F, X] { type T[O] = M[O]; type G = last.FromLast; type I = -1 } =
				new EvidenceTemplate[F, X, last.FromLast, last.FromLast, M, -1](PrefixOf.itself) with Return[F, X] {
					override def table(from :F) = last(from)
				}


			implicit def extended[F <: L J R, L <: U, R[O] <: MappingAt[O],
			                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause,
			                      X, M <: Numeral, N <: Numeral]
			                     (implicit extend :ExtendedDecomposition[F, L, R, J, U],
			                      get :Found[L, X] { type G >: L <: U; type I = N }, dec :Inc[M, N])
					:Return[F, X] { type T[O] = get.T[O]; type G = J[get.G, R]; type I = M } =
				new EvidenceTemplate[F, X, J[get.G, R], get.S, get.T, M](get.stretch.extend(extend.extension[get.G]))
					with Return[F, X]
				{
					override def table(from :F) = get.table(from.left)
				}


			implicit def grouped[L <: FromSome, R[O] <: MappingAt[O], X, M <: Numeral, N <: Numeral]
			             (implicit get :GroupedTunnel[L, X] { type G >: L <: FromSome; type I = N }, dec :Inc[M, N])
					:Return[L GroupByAll R, X] { type T[O] = get.T[O]; type G = get.G GroupByAll R; type I = M } =
				new EvidenceTemplate[L GroupByAll R, X, get.G GroupByAll R, get.S, get.T, M](
				                     PrefixOf.group(get.outerPart, get.prefix))
					with Return[L GroupByAll R, X]
				{
					override def table(from :L GroupByAll R) = get.table(from.left)
				}


			implicit def decorated[F <: D[C], C <: U, D[+B <: U] <: DecoratedFrom[B], U <: FromClause, X]
			                      (implicit extend :DecoratorDecomposition[F, C, D, U],
			                       get :Found[C, X] { type G >: C <: U })
					:Return[F, X] { type T[O] = get.T[O]; type G = D[get.G]; type I = get.I } =
				new EvidenceTemplate[F, X, D[get.G], get.S, get.T, get.I](
				                     get.stretch.extend(extend.extension[get.G]))
					with Return[F, X]
				{
					override def table(from :F) = get.table(from.clause)
				}



			implicit def outer[L <: FromSome, R[O] <: MappingAt[O], X]
			                  (implicit get :Found[L, X] { type G >: L <: FromSome })
					:GroupedTunnel[L Subselect R, X]
						{ type T[O] = get.T[O]; type G = get.G Subselect R; type O = get.G; type I = get.I } =
				new EvidenceTemplate[L Subselect R, X, get.G Subselect R, get.S, get.T, get.I](
				                     get.stretch.extend[get.G Subselect R])
					with GroupedTunnel[L Subselect R, X]
				{
					override type O = get.G
					override def outerPart :get.G OuterClauseOf G = OuterClauseOf.subselect[get.G, R]
					override def prefix = get.stretch
					override def table(from :L Subselect R) = get.table(from.left)
				}


			implicit def tunnelJoin[F <: L J R, L <: U, R[O] <: MappingAt[O],
			                        J[+A <: U, B[O] <: R[O]] <: NonSubselect[A, B], U <: FromClause, X]
			                       (implicit extend :ExtendedDecomposition[F, L, R, J, U],
			                        get :GroupedTunnel[L, X] { type G >: L <: U })
					:GroupedTunnel[F, X]
						{ type T[O] = get.T[O]; type G = J[get.G, R]; type O = get.O; type I = get.I } =
				new EvidenceTemplate[F, X, J[get.G, R], get.S, get.T, get.I](get.stretch.extend[J, R])
					with GroupedTunnel[F, X]
				{
					override type O = get.O
					override val prefix = get.prefix
					override val outerPart = OuterClauseOf.extended(extend.upcast[get.G], get.outerPart)
					override def table(from :F) = get.table(from.left)
				}


			implicit def tunnelDecorator[F <: D[C], C <: U, D[+B <: U] <: DecoratedFrom[B], U <: FromClause, X]
			                            (implicit decorate :DecoratorDecomposition[F, C, D, U],
			                             get :GroupedTunnel[C, X] { type G >: C <: U })
					:GroupedTunnel[F, X]
						{ type T[O] = get.T[O]; type G = D[get.G]; type O = get.O; type I = get.I } =
				new EvidenceTemplate[F, X, D[get.G], get.S, get.T, get.I](
					                 get.stretch.extend(decorate.extension[get.G]))
					with GroupedTunnel[F, X]
				{
					override type O = get.O
					override val prefix = get.prefix
					override val outerPart = OuterClauseOf.wrapped(decorate.upcast[get.G], get.outerPart)
					override def table(from :F) = get.table(from.clause)
				}

		}






		/** Implicit resolution of the `N`-th relation in the ''from'' clause `F`. This works both for positive numbers,
		  * indexed from zero and going from left to right (in which case `F` must be complete), and negative -
		  * indexed from `-1` and going from right to left (which is available always, but which index changes with
		  * joining new tables).
		  * @tparam F the input `FromClause`.
		  * @tparam N index of the desired relation as a literal `Int` type.
		  */
		@implicitNotFound("Cannot get ${N}-th relation of the FROM clause ${F}.\n" +
		                  "Either ${N} >= size (where size is the number of relations in the whole FROM clause),\n" +
		                  "or -(${N}) is greater the number of relations in the instantiated suffix of the FROM clause,\n" +
		                  "or ${N} >= 0 and the size is not known (the clause starts with an abstract type).")
		sealed trait ByIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N]


		object ByIndex {

			implicit def byPositiveIndex[F <: FromClause, N <: Numeral, M <: Numeral]
			                            (implicit positive :Positive[N], found :ByPositiveIndex[F, N])
					:ByIndex[F, N] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				found.byIndex

			implicit def byNegativeIndex[F <: FromClause, N <: Numeral]
			                            (implicit negative :Negative[N], found :ByNegativeIndex[F, N])
					:ByIndex[F, N] { type T[O] = found.T[O]; type G = found.G; type I = N } =
				found.byIndex


			/** Performs for the `ByIndex` evidence the same function as
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableTemplate#Return Return]] for
			  * `GetTableByPredicate` instances, that is marks a
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.ByIndex.ByPositiveIndex.Found ByPositiveIndex.Found]]
			  * evidence as being a search result rather than a conversion from an existing `ByIndex` to avoid infinite
			  * implicit resolution loops.
			  */
			@implicitNotFound("Cannot get ${N}-th relation of the FROM clause ${F}.\n" +
			                  "Either ${N} >= size (where size is the number of relations in the whole FROM clause,\n" +
			                  "or the size is not known (the clause starts with an abstract type).\n"+
			                  "Missing implicit ByPositiveIndex[${F}, ${N}]")
			sealed trait ByPositiveIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N] { pos =>
				protected[ByIndex] def byIndex :ByIndex[F, N] { type T[O] = pos.T[O]; type G = pos.G; type I = pos.I }
			}


			object ByPositiveIndex extends GetTableByPredicate {

				//this has actually precedence over real found instances, which introduces extra steps,
				// but is the only way to resolve implicit conflicts.
				/** Adapts an existing implicit value of `ByIndex[F, N]` into a `ByPositiveIndex.Found[F, N]`
				  * to use for further search, especially when `F` is incomplete. */
				implicit def byIndex[F <: FromClause, N <: Numeral](implicit positive :Positive[N], get :ByIndex[F, N])
						:ByPositiveIndex.Found[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I } =
					get match {
						case pos :GetTableByPredicate#Found[_, _] if pos.how == ByPositiveIndex =>
							pos.asInstanceOf[Found[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I }]
						case _ =>
							new Delegate[F, N, get.G, get.T, get.I](get) with Found[F, N]
					}

				implicit def byPositiveIndex[F <: FromClause, N <: Numeral]
				                            (implicit found :Return[F, N])
						:ByPositiveIndex[F, N] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
					new Delegate[F, N, found.G, found.T, found.I](found)
						with ByPositiveIndex[F, N] with ByIndex[F, N] with Found[F, N]
				{
					override def byIndex = this
				}

				implicit def satisfies[L <: FromClause, R[O] <: MappingAt[O], N <: Numeral]
				                      (implicit size :FromClauseSize[L, N]) :Predicate[L Extended R, R, N] =
					report

				implicit def outerSatisfies[O <: FromClause, L <: FromSome, R[O] <: MappingAt[O], N <: Numeral]
				                           (implicit outer :O OuterClauseOf L, size :FromClauseSize[O, N])
						:Predicate[L GroupByAll R, R, N] =
					report
			}



			/** Performs for the `ByIndex` evidence the same function as
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableTemplate#Return Return]] for
			  * `GetTableByPredicate` instances, that is marks a
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.ByIndex.ByNegativeIndex.Found ByNegativeIndex.Found]]
			  * evidence as being a search result rather than a conversion from an existing `ByIndex` to avoid infinite
			  * implicit resolution loops.
			  *
			  * Note that, unlike `ByNegativeIndex.Found`, the 'key' type `N` and the member type `N` are equal
			  * and thus only values of this class constitute valid evidence for the position of the relation `this.T`
			  * in the ''from'' clause `F`. This is because, in order to leverage
			  * the [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableByPredicate GetTableByPredicate]]
			  * framework (which does not have provisions for changing the key type `N` during search),
			  * `ByNegativeIndex.Found[F, X]` is reported for every relation regardless of its position and
			  * the value of the `Int` literal `X`: the matching of the desired index with the relation's position
			  * happens as the last step, by comparing `f.I` with `N` for `f :Found[F, N]`, instead of when reporting
			  * the initial `Found` based on
			  * the [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableByPredicate#Predicate Predicate]] in
			  * the [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableByPredicate#last last]] method.
			  */
			@implicitNotFound("Cannot get ${N}-th relation of the FROM clause ${F}.\n" +
			                  "Either ${N} is not negative, or it is less than -size - 1, where size is the number " +
			                  "of known relations in the clause.\nMissing implicit ByNegativeIndex[${F}, ${N}].")
			sealed trait ByNegativeIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N] { neg =>
				override type I = N
				protected[ByIndex] def byIndex :ByIndex[F, N] { type T[O] = neg.T[O]; type G = neg.G; type I = N }
			}


			object ByNegativeIndex extends GetTableByPredicate {
				//this has actually precedence over real found instances, which introduces extra steps,
				// but is the only way to resolve implicit conflicts.
				/** Adapts an existing implicit value of `ByIndex[F, N]` into a `ByNegativeIndex.Found[F, X] { type I = N }`
				  * to use, especially when `F` is incomplete. This process allows
				  */
				implicit def byIndex[F <: FromClause, X <: Numeral, N <: Numeral]
				                    (implicit negative :Negative[N], get :ByIndex[F, N])
						:ByNegativeIndex.Found[F, X] { type T[O] = get.T[O]; type G = get.G; type I = N  } =
					get match {
						case pos :GetTableByPredicate#Found[_, _] if pos.how == ByNegativeIndex =>
							pos.asInstanceOf[ByNegativeIndex.Found[F, X] { type T[O] = get.T[O]; type G = get.G; type I = N }]
						case _ =>
							new EvidenceTemplate[F, X, get.G, get.S, get.T, N](get.stretch) with Found[F, X] {
								override def table(from :F) = get.table(from)
							}
					}

				implicit def byNegativeIndex[F <: FromClause, N <: Numeral]
				                            (implicit found :ByNegativeIndex.Return[F, N] { type I = N })
						:ByNegativeIndex[F, N] { type T[O] = found.T[O]; type G = found.G } =
					new Delegate[F, N, found.G, found.T, N](found)
						with ByNegativeIndex[F, N] with ByIndex[F, N] with Found[F, N]
					{
						override def byIndex = this
					}

				//this predicate is satisfied for every relation and every index, however ByIndex.byNegativeIndex
				//requires a Found[F, N] { type I = N }, that is that the computed negative index is the same as the input parameter
				implicit def satisfies[M[O] <: MappingAt[O], N <: Numeral] :Predicate[FromClause Using M, M, N] =
					report

				/* Different parameter order makes the implicit parameter resolution start with determining
				 * the correct negative index `M` of essentially any relation in the original clause.
				 * there will be Found[F, X, _] instances for every relation in F, but only one Found[F, X, X] -
				 * which is required by ByIndex[F, X].
				 */

				implicit def previous[F <: L J R, L <: U, R[O] <: MappingAt[O],
				                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause,
				                      X <: Numeral, M <: Numeral, N <: Numeral]
				                     (implicit extend :ExtendedDecomposition[F, L, R, J, U],
				                      dec :Inc[M, N], get :Found[L, X] { type G >: L <: U; type I = N })
						:Return[F, X] { type T[O] = get.T[O]; type G = J[get.G, R]; type I = M } =
					extended(extend, get, dec)

				implicit def skip[L <: FromSome, R[O] <: MappingAt[O], X <: Numeral, M <: Numeral, N <: Numeral]
				                 (implicit dec :Inc[M, N],
				                           get :GroupedTunnel[L, X] { type G >: L <: FromSome; type I = N })
						:Return[L GroupByAll R, X] { type T[O] = get.T[O]; type G = get.G GroupByAll R; type I = M } =
					grouped[L, R, X, M, N](get, dec)
			}

		}




		/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledMapping[N, _, _]`. */
		@implicitNotFound("No relation with alias type ${N} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByLabel[${F}, ${N}].")
		sealed trait ByLabel[-F <: FromClause, N <: Label] extends ByLabel.Found[F, N] {
			override type T[O] <: LabeledMapping[N, _, O]
		}


		/** Accesses relations in a `FromClause` based on their `Label` (that is, the `Label` type parameter
		  * of the `LabeledMapping` used for the relation. Implicit ByLabel.Get[F, L] returns the last relation
		  * with label `L` in `F`. */
		object ByLabel extends GetTableByPredicate {
			override type Key = Label
			override type Get[-F <: FromClause, A <: Label] = ByLabel[F, A]

			implicit def byLabel[F <: FromClause, A <: Label]
			                    (implicit found :Return[F, A] { type T[O] <: LabeledMapping[A, _, O] })
					:ByLabel[F, A] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				new Delegate[F, A, found.G, found.T, found.I](found) with ByLabel[F, A]

			implicit def satisfies[M[A] <: LabeledMapping[L, _, A], L <: Label] :Predicate[FromClause Using M, M, L] =
				report[FromClause Using M, M, L]
		}




 		/** Implicit witness accessing the last relation in the ''from'' clause `F` with alias `A`.
		  * It is defined as the last relation of the clause `L`, such that
		  * `L` [[net.noresttherein.oldsql.sql.DecoratedFrom.Alias Alias]] `A` appears as a part of type `F` and
		  * is the right-most such occurrence.
		  */
		@implicitNotFound("No relation with alias ${A} appears in the clause ${F}.")
		sealed trait ByAlias[-F <: FromClause, A <: Label] extends ByAlias.Found[F, A]


		object ByAlias extends GetTableByPredicate {
			override type Key = Label
			override type Get[-F <: FromClause, X <: Label] = ByAlias[F, X]

			implicit def byAlias[F <: FromClause, A <: Label](implicit found :Return[F, A])
					:ByAlias[F, A] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				new Delegate[F, A, found.G, found.T, found.I](found) with ByAlias[F, A]

			implicit def satisfies[R[O] <: MappingAt[O], A <: Label] :Predicate[DiscreteFrom AndFrom R Alias A, R, A] =
				report
		}




		/** An implicit accessor object for the last relation in `F` with `Subject` type `S`.
		  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
		@implicitNotFound("No relation with Subject type ${X} in the FROM clause ${F}:\n" +
		                  "no implicit value for BySubject[${F}, ${X}].")
		sealed trait BySubject[-F <: FromClause, X] extends BySubject.Found[F, X] {
			override type T[O] <: RefinedMapping[X, O]
		}


		/** Accesses relations in a `FromClause` based on their `Subject` member type. */
		object BySubject extends GetTableByPredicate {
			override type Key = Any
			override type Get[-F <: FromClause, S] = BySubject[F, S]

			implicit def bySubject[F <: FromClause, S, M[O] <: RefinedMapping[S, O]]
			                      (implicit found :Return[F, S] { type T[O] = M[O] })
					:BySubject[F, S] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				new Delegate[F, S, found.G, M, found.I](found) with BySubject[F, S]

			implicit def satisfies[M[A] <: RefinedMapping[S, A], S] :Predicate[FromClause Using M, M, S] =
				report[FromClause Using M, M, S]

		}




		/** An implicit accessor object for the last relation in `F` with `Mapping` type `M[_]`.
		  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
		@implicitNotFound("No relation with type constructor ${M} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByTypeConstructor[${F}, ${M}].")
		sealed trait ByTypeConstructor[-F <: FromClause, M[O] <: MappingAt[O]]
			extends ByTypeConstructor.Found[F, M["Boo"]]
		{
			override type T[O] = M[O]
		}


		/** Accesses relations in a `FromClause` based on the type constructor for the mapping accepting
		  * the `Origin` type. The provided type constructor may be for a supertype of the actual mapping.
		  * While the input to match accepts only a single parameter, it is possible to match mappings with multiple
		  * parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
		  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`. */
		object ByTypeConstructor extends GetTableByPredicate {

			implicit def byTypeConstructor[F <: FromClause, M[O] <: MappingAt[O]]
			                              (implicit found :Return[F, M["Boo"]] { type T[O] = M[O] })
					:ByTypeConstructor[F, M] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				new Delegate[F, M["Boo"], found.G, found.T, found.I](found) with ByTypeConstructor[F, M]

			implicit def satisfies[M[A] <: MappingAt[A]] :Predicate[FromClause Using M, M, M["Boo"]] = report

		}




		/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledFromParam[A, _, _]`. */
		@implicitNotFound("No parameter with name type ${A} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByParamName[${F}, ${A}].")
		sealed trait ByParamName[-F <: FromClause, A <: Label] extends ByParamName.Found[F, A] {
			type T[O] <: LabeledFromParam[A, _, O]
		}


		/** Accesses parameters of a `FromClause` based on the given parameter name used as their label.
		  * Implicit ByParamName.Get[F, N] returns the last relation for the synthetic parameter mapping
		  * `LabeledFromParam[N, X, O]` in `F`.
		  */
		object ByParamName extends GetTableByPredicate {
			type Key = Label
			override type Get[-F <: FromClause, N <: Label] = ByParamName[F, N]

			implicit def byParamName[F <: FromClause, A <: Label]
			                        (implicit found :Return[F, A] { type T[O] <: LabeledFromParam[A, _, O] })
					:ByParamName[F, A] { type T[O] = found.T[O]; type G = found.G; type I = found.I } =
				new Delegate[F, A, found.G, found.T, found.I](found) with ByParamName[F, A]

			implicit def satisfies[M[A] <: LabeledFromParam[N, _, A], N <: Label]
					:Predicate[FromClause UnboundParam M, M, N] =
				report[FromClause UnboundParam M, M, N]

		}




		/** Implicit resolution of the `N`-th unbound parameter in the ''from'' clause `F`. This works both
		  * for positive numbers, indexed from zero and going from left to right (in which case `F` must be complete),
		  * and negative - indexed from `-1` and going from right to left (which is available always,
		  * but whose index changes with joining new tables).
		  * @tparam F the input `FromClause`.
		  * @tparam N index of the desired parameter as a literal `Int` type.
		  */
		@implicitNotFound("Cannot get the ${N}-th unbound parameter of the FROM clause ${F}.\n" +
		                  "Either ${N} is greater or equal than the total number of parameters in the FROM clause,\n" +
			              "or ${N} is less than the number of parameters in the instantiated suffix of the FROM clause,\n" +
			              "or ${N} >= 0 and the clause is incomplete (starts with an abstract type).")
		sealed trait ByParamIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N]


		object ByParamIndex {

			implicit def byPositiveParamIndex[F <: FromClause, N <: Numeral]
			                                 (implicit positive :Positive[N], get :ByPositiveParamIndex[F, N])
					:ByParamIndex[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I } =
				get.byParamIndex


			/** Performs for the `ByParamIndex` evidence the same function as
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableTemplate#Return Return]] for
			  * `GetTableByPredicate` instances, that is marks a
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.ByParamIndex.ByPositiveParamIndex.Found ByPositiveParamIndex.Found]]
			  * evidence as being a search result rather than a conversion from an existing `ByParamIndex`
			  * to avoid infinite implicit resolution loops.
			  */
			@implicitNotFound("Cannot get the ${N}-th unbound parameter of the FROM clause ${F}.\n" +
			                  "Either ${N} is greater or equal than the total number of parameters in the FROM clause," +
			                  "or it is incomplete (starts with an abstract type).\n" +
			                  "Missing implicit ByParamIndex[${F}, ${N}]")
			sealed trait ByPositiveParamIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N] { self =>
				protected[ByParamIndex] def byParamIndex
					:ByParamIndex[F, N] { type T[O] = self.T[O]; type G = self.G; type I = self.I }
			}

			object ByPositiveParamIndex extends GetTableByPredicate {

				implicit def byPositiveParamIndex[F <: FromClause, N <: Numeral](implicit get :Return[F, N])
						:ByPositiveParamIndex[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I } =
					new Delegate[F, N, get.G, get.T, get.I](get)
						with Found[F, N] with ByPositiveParamIndex[F, N] with ByParamIndex[F, N]
					{
						override def byParamIndex = this
					}

				implicit def byParamIndex[F <: FromClause, N <: Numeral](implicit positive :Positive[N], get :ByParamIndex[F, N])
						:Found[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I } =
					get match {
						case pos :GetTableByPredicate#Found[_, _] if pos.how == ByPositiveParamIndex =>
							pos.asInstanceOf[Found[F, N] { type T[O] = get.T[O]; type G = get.G; type I = get.I }]
						case _ =>
							new Delegate[F, N, get.G, get.T, get.I](get) with Found[F, N]
					}

				implicit def satisfies[F <: FromClause, P[O] <: ParamAt[O], N <: Numeral]
				                      (implicit preceding :ChainLength[F#Params, N]) =
					report[F UnboundParam P, P, N]
			}



			/** Performs for the `ByParamIndex` evidence the same function as
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.GetTableTemplate#Return Return]] for
			  * `GetTableByPredicate` instances, that is marks a
			  * [[net.noresttherein.oldsql.sql.FromClause.GetTable.ByParamIndex.ByPositiveParamIndex.Found ByPositiveParamIndex.Found]]
			  * evidence as being a search result rather than a conversion from an existing `ByParamIndex`
			  * to avoid infinite implicit resolution loops.
			  */
/*
			sealed trait ByNegativeParamIndex[-F <: FromClause, N <: Numeral] extends RelationEvidence[F, N] { self =>
				protected[ByParamIndex] def byParamIndex
						:ByParamIndex[F, N] { type T[O] = self.T[O]; type G = self.G; type I = self.I }
			}


			object ByNegativeParamIndex extends GetTableByPredicate {
				object KnownParamIndex extends GetTableByPredicate {
					implicit def satisfies[P[O] <: ParamAt[O]] :Predicate[FromClause UnboundParam]
				}

				implicit def anyIsGood[P[O] <: ParamAt[O], N <: Numeral] :Predicate[FromClause UnboundParam P, P, N]

				/** Optimistically assumes every parameter occurrence is the first known one in `F`.
				  * If it is not, `notFirstKnown` will introduce a conflicting implicit value. */
				implicit def firstKnown[P[O] <: ParamAt[O]] :Predicate[FromClause UnboundParam P, P, -1] =
					report

				/** Falsifies the `firstKnown` implicit */
				implicit def notFirstKnown[L <: FromClause, P[O] <: ParamAt[O]](implicit prev :Found[L, -1])
						:Predicate[L UnboundParam P, P, -1] =
					report

//				implicit def next[F <: FromClause, L <: U, P[O] <: ParamAt[O],
//				                  J[+A <: U, B[O] <: ParamAt[O]] <: A UnboundParam B, U <: FromClause,
//				                  M <: Numeral, N <: Numeral]
//				                 (implicit decompose :ExtendedDecomposition[F, L, P, J, U],
//				                           dec :Inc[M, N], prev :Found[L, M])
//						:Return[F, ]
				implicit def satisfies[L <: FromClause, P[O] <: ParamAt[O], M <: Numeral, N <: Numeral]
				                      (implicit dec :Inc[M, N], prev :Found[L, M]) :Predicate[L UnboundParam P, P, N] =
					report[L UnboundParam P, P, N]
			}
*/
		}
	}






	/** Implicit witness that the ''from'' clause `U` is the result of removing all `JoinParam` 'joins'
	  * from the clause `F` and `F#Params >: P`. It implements the application of the input clause `F` to the
	  * argument chain `P` with values for all `JoinParam` joins in `F` by substituting every reference to
	  * a `ParamMapping` in the filter with a `BoundParameterSQL` wrapping the corresponding value from `P`.
	  */
	@implicitNotFound("Cannot apply the FROM clause ${F}\nto parameters ${P}. I cannot prove that the parameter chain P " +
	                  "is a subtype of F#Params - most likely F is incomplete or its Generalized type is unknown.\n"+
	                  "Missing implicit value ApplyJoinParams[${F}, ${P}, ${U}].")
	sealed abstract class ApplyJoinParams[-F <: FromClause, -P <: Chain, +U] {
		type lastWasParam <: Boolean with Singleton
		def apply(from :F, params :P) :U
	}


	object ApplyJoinParams {
		private[this] val none = new ApplyJoinParams[ParameterlessFrom, @~, ParameterlessFrom] {
			override def apply(from :ParameterlessFrom, params: @~) = from
		}

		implicit val applyToDual :ApplyJoinParams[Dual, @~, Dual] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[Dual, @~, Dual] { type lastWasParam = false }]

		implicit def unparameterized[F <: FromSome with ParameterlessFrom]
				:ApplyJoinParams[F, @~, F] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[F, @~, F] { type lastWasParam = false }]

		implicit def nonParamFrom[T[A] <: MappingAt[A]]
				:ApplyJoinParams[From[T], @~, From[T]] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[From[T], @~, From[T]] { type lastWasParam = false }]

		implicit def nonParamEmptyJoin[J[A <: FromSome, B[O] <: MappingAt[O]] <: A JoinLike B,
		                               L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, P <: _ ~ _]
		                              (implicit prev :ApplyJoinParams[L, P, Dual],
		                                        cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
				:ApplyJoinParams[L J R, P, From[R]] { type lastWasParam = false } =
			new ApplyJoinParams[L J R, P, From[R]] {
				override type lastWasParam = false

				override def apply(from :L J R, params :P) = {
					val dual = prev(from.left, params) //unlikely, but in theory may be filtered
					val unfiltered = From(from.right)
					val generalized = from.generalized
					val ps = params.asInstanceOf[generalized.Params]
					val substitute = new RemoveParams(generalized, unfiltered.generalized)(ps)
					unfiltered where dual.filter && substitute(from.condition)
				}
			}

		implicit def nonParamJoin[J[+A <: U, B[O] <: M[O]] <: A JoinLike B, U <: FromClause, M[A] <: MappingAt[A],
		                          L <: U, R[A] <: M[A], P <: _ ~ _, Q <: U]
		                         (implicit composition :ExtendedComposition[L J R, L, R, J, U, M],
		                                   prev :ApplyJoinParams[L, P, Q])
				:ApplyJoinParams[L J R, P, (Q J R)#This] { type lastWasParam = false } =
			new ApplyJoinParams[L J R, P, (Q J R)#This] {
				override type lastWasParam = false

				override def apply(from :L J R, params :P) = {
					val left = prev(from.left, params)
					val unfiltered = composition[left.type](from, left)// from.withLeft[left.type](left)(True)
					val generalized = from.generalized
					val ps = params.asInstanceOf[generalized.Params]
					val substitute = new RemoveParams(generalized, unfiltered.generalized)(ps)
					unfiltered where substitute(from.condition)
				}
			}

		implicit def groupBy[L <: FromSome, R[A] <: MappingAt[A], P <: _ ~ _, Q <: FromSome with ParameterlessFrom]
		                    (implicit prev :ApplyJoinParams[L, P, Q])
				:ApplyJoinParams[L GroupByAll R, P, Q GroupByAll R] { type lastWasParam = false } =
			new ApplyJoinParams[L GroupByAll R, P, Q GroupByAll R] {
				override type lastWasParam = false

				override def apply(from :L GroupByAll R, params :P) = {
					val left = prev(from.left, params)
					val unfiltered = from.withLeft[left.type](left)(True)
					val generalized = from.generalized
					val ps = params.asInstanceOf[generalized.Params]
					val substitute = new RemoveParams(generalized, unfiltered.generalized)(ps)
					from.withLeft(left)(substitute(from.condition))
				}

			}

		implicit def by[L <: GroupByClause, R[A] <: MappingAt[A], P <: _ ~ _, Q <: GroupByClause with ParameterlessFrom]
		               (implicit prev :ApplyJoinParams[L, P, Q])
				:ApplyJoinParams[L ByAll R, P, Q ByAll R] { type lastWasParam = false } =
			new ApplyJoinParams[L ByAll R, P, Q ByAll R] {
				override type lastWasParam = false

				override def apply(from :L ByAll R, params :P) = {
					val left = prev(from.left, params)
					val unfiltered = from.withLeft[left.type](left)(True)
					val generalized = from.generalized
					val ps = params.asInstanceOf[generalized.Params]
					val substitute = new RemoveParams(generalized, unfiltered.generalized)(ps)
					from.withLeft(left)(substitute(from.condition))
				}

			}

		implicit def nonParamDecorator[D <: GenericDecorator[F], F <: FromSome,
		                               P <: _ ~ _, Q <: FromSome with ParameterlessFrom]
		                              (implicit deconstruct :Conforms[D, D, GenericDecorator[F]],
		                                        prev :ApplyJoinParams[F, P, Q] { type lastWasParam = false })
				:ApplyJoinParams[D, P, D#WithClause[Q]] { type lastWasParam = false } =
			new ApplyJoinParams[D, P, D#WithClause[Q]] {
				override type lastWasParam = false

				override def apply(from :D, params :P) :D#WithClause[Q] =
					from.withClause(prev(from.clause, params))
			}


		implicit def applyParam[L <: FromSome, R[A] <: FromParam[X, A], X, I <: Chain, Q <: ParameterlessFrom]
		                       (implicit prev :ApplyJoinParams[L, I, Q])
				:ApplyJoinParams[L JoinParam R, I ~ X, Q] { type lastWasParam = true } =
			new ApplyJoinParams[L JoinParam R, I ~ X, Q] {
				override type lastWasParam = true

				override def apply(from :JoinParam[L, R], params :I ~ X) = {
					val res = prev(from.left, params.init)
					val generalized = from.generalized
					val substitute = new RemoveParams(generalized, res.generalized)(params.asInstanceOf[generalized.Params])
					res.where(substitute(from.condition)).asInstanceOf[Q]
				}
			}

		implicit def applyGroupParam[L <: GroupByClause, R[A] <: FromParam[X, A], X, I <: Chain, Q <: ParameterlessFrom]
		                            (implicit prev :ApplyJoinParams[L, I, Q])
				:ApplyJoinParams[L GroupParam R, I ~ X, Q] { type lastWasParam = true } =
			new ApplyJoinParams[L GroupParam R, I ~ X, Q] {
				override type lastWasParam = true

				override def apply(from :GroupParam[L, R], params :I ~ X) = {
					val res = prev(from.left, params.init)
					val generalized = from.generalized
					val substitute = new RemoveParams(generalized, res.generalized)(params.asInstanceOf[generalized.Params])
					res.where(substitute(from.condition)).asInstanceOf[Q]
				}
			}

		implicit def paramDecorator[F <: FromSome, P <: _ ~ _, Q <: ParameterlessFrom]
		                           (implicit prev :ApplyJoinParams[F, P, Q] { type lastWasParam = true })
				:ApplyJoinParams[DecoratedFrom[F], P, Q] { type lastWasParam = true } =
			new ApplyJoinParams[DecoratedFrom[F], P, Q] {
				override type lastWasParam = true

				override def apply(from :DecoratedFrom[F], params :P) = prev(from.clause, params)
			}

	}






	/** Implicit witness that `O` is the outer clause of `F`. If `F` is a concrete clause, at least in the most
	  * nested subselect extension, this is exactly `F#Outer`, and the member type should generally be preferred
	  * over this implicit value. It has its use however when there is a requirement for maintaining the type `O`
	  * exactly as it appears as a prefix in `F`: `F#Outer` can introduce abstract types and is not a lossless process
	  */
	@implicitNotFound("Cannot determine the outer clause for ${F}: missing implicit OuterFrom[${F}, ${O}].")
	class OuterClauseOf[O <: FromClause, F <: FromClause] private()


	object OuterClauseOf {
		implicit val dual = new OuterClauseOf[Dual, Dual]

		implicit def from[T[O] <: MappingAt[O]] :OuterClauseOf[Dual, From[T]] =
			dual.asInstanceOf[OuterClauseOf[Dual, From[T]]]

		implicit def subselect[L <: FromSome, R[A] <: MappingAt[A]] :OuterClauseOf[L, L Subselect R] =
			dual.asInstanceOf[OuterClauseOf[L, L Subselect R]]

		implicit def extended[O <: FromClause, F <: L J R, L <: U, R[A] <: MappingAt[A],
		                      J[+C <: U, T[A] <: R[A]] <: NonSubselect[C, T], U <: FromClause]
		                     (implicit decompose :ExtendedDecomposition[F, L, R, J, U], outer :OuterClauseOf[O, L])
				:OuterClauseOf[O, F] =
			outer.asInstanceOf[OuterClauseOf[O, F]]

		implicit def wrapped[O <: FromClause, F <: D[C], D[+B <: U] <: DecoratedFrom[B], C <: U, U <: FromClause]
		                    (implicit decompose :DecoratorDecomposition[F, C, D, U], outer :OuterClauseOf[O, C])
				:OuterClauseOf[O, F] =
			outer.asInstanceOf[OuterClauseOf[O, F]]

		implicit def grouped[O <: FromClause, L <: FromSome, R[A] <: MappingAt[A]]
		                    (implicit outer :OuterClauseOf[O, L]) :OuterClauseOf[O, L GroupByAll R] =
			outer.asInstanceOf[OuterClauseOf[O, L GroupByAll R]]

	}






	/** Proof that the ''from'' clause `E` is an ''extension'' of the clause `F`, meaning `E` contains all relations
	  * listed by type `F` as a consecutive, ordered sequence. If `F` is a ''complete'' clause
	  * (starting with `Dual`/`From`), then `F` is a supertype of some prefix of `E`. More formally,
	  * `F ExtendedBy E` if one of the following conditions hold:
	  *   - `E &lt;: F`,
	  *   - `E &lt;: DecoratedFrom[F]`,
	  *   - `E &lt;: S Extended _` for some `S` such that `F ExtendedBy S`,
	  *   - `E &lt;: S GroupByAll _` for some `S &lt;: F#Nested`,
	  *   - `F =:= Dual` and `E &lt;: S GroupByAll _` for some `S &lt;: OuterFrom`.
	  *
	  * It means that a value of type `F` can be extracted from `E` by a sequence of operations:
	  *   - taking the inner clause of a `DecoratedFrom`;
	  *   - taking the left side of an `Extended` clause;
	  *   - taking the outer clause of a `GroupByAll` clause.
	  *
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `E` containing all the relations of `F` unless `F` is complete. The primary use
	  * of this class is in conversions from `SQLExpression[F, T]` to `SQLExpression[E, T]`.
	  *
	  * A non-obvious implication of being contravariant in the extending type `E` is that if `F` is incomplete,
	  * this instance is also a witness that subtypes of `E` which replace the initial `FromClause` with a join list
	  * are likewise extensions of `F`, in this way covering both extensions from the back (right side) and front
	  * (left side). The purpose of this class is to allow `SQLExpression` instances based on one clause `F`
	  * to be converted into isomorphic expressions based on a second clause `E`, as long as all mappings
	  * in `F`'s static type form a continuous subsequence of mappings listed in `E`. Note that `SQLExpression`
	  * is contravariant in its clause type parameter, meaning that an abstract `FromClause` occurring
	  * at the start of the from clause type parameter doesn't 'hide' any mappings used by that expression,
	  * but to the contrary, serves to signify that it doesn't rely in any form on that portion of the clause
	  * it is based on. As a result, this proof is indeed a valid representation that such a conversion from `F` to `E`
	  * is possible for any `SQLExpression`. Due to this contravariance in `E`, this isn't any form
	  * of a generalized subtyping relation and should be relied upon only in the context of the actual extension.
	  */
	@implicitNotFound("The FROM clause ${F} is not a prefix of the clause ${E} (ignoring join kinds).")
	class ExtendedBy[+F <: FromClause, -E <: FromClause] private[FromClause] (val length :Int) extends AnyVal {

		/** A transitive proof that a clause extending `E` with a single relation (mapping) also extends `F`. */
		@inline def extend[R[O] <: MappingAt[O]] :F ExtendedBy (E Extended R) = new ExtendedBy(length + 1)

		/** A transitive proof that a clause extended by `F` with a single relation (mapping) is also extended by `E`. */
		@inline def extendFront[L <: FromClause, R[O] <: MappingAt[O]]
		                        (implicit front :F <:< (L Extended R)) :L ExtendedBy E =
			new ExtendedBy(length + 1)

		/** A transitive proof that if `E ExtendedBy C`, then also `F ExtendedBy C` (appending to this witness). */
		@inline def extend[C <: FromClause](implicit next :E ExtendedBy C) :F ExtendedBy C =
			new ExtendedBy[F, C](length + next.length)

		/** A transitive proof that if `C ExtendedBy F`, then also `C ExtendedBy E` (prepending to this witness). */
		@inline def extendFront[C <: FromClause](implicit front :C ExtendedBy F) :C ExtendedBy E =
			new ExtendedBy(front.length + length)

		/** A transitive proof that if `E ExtendedBy C`, then also `F ExtendedBy C` (appending to this witness). */
		@inline def +[C <: FromClause](next :E ExtendedBy C) :F ExtendedBy C =
			new ExtendedBy[F, C](length + next.length)

		/** A transitive proof that a decorated clause `E` is still an extension of `F`. */
		@inline def wrap :F ExtendedBy DecoratedFrom[E] = new ExtendedBy[F, DecoratedFrom[E]](length)

		/** A transitive proof that if `F &lt;: DecoratedFrom[C]`, then `C ExtendedBy E` without any length change. */
		@inline def unwrapFront[C <: FromSome](implicit front :F <:< DecoratedFrom[C]) :C ExtendedBy E =
			new ExtendedBy(length)
	}



	object ExtendedBy {
		@inline def apply[F <: FromClause, E <: FromClause](implicit ev :F ExtendedBy E) :F ExtendedBy E = ev

		implicit def itself[F <: FromClause] :F ExtendedBy F = new (F ExtendedBy F)(0)

		implicit def extend[F <: FromClause, L <: FromClause, R[A] <: MappingAt[A]]
		                   (implicit ev :F ExtendedBy L) :F ExtendedBy (L Extended R) =
			new ExtendedBy(ev.length + 1)

		implicit def wrap[F <: FromClause, E <: FromClause](implicit ev :F ExtendedBy E) :F ExtendedBy DecoratedFrom[E] =
			new ExtendedBy[F, DecoratedFrom[E]](ev.length)

		implicit def group[F <: FromClause, O <: FromClause, E <: FromSome, R[A] <: MappingAt[A]]
		                  (implicit outer :O OuterClauseOf E, ev :F ExtendedBy O) :F ExtendedBy (E GroupByAll R) =
			new ExtendedBy(ev.length + 1)
	}






	/** Proof that the ''from'' clause `E` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `E`. A clause `F` is a prefix of `E` if one of the conditions hold:
	  *   - `E =:= F`,
	  *   - `E =:= D[F]` for some `D[C] &lt;: DecoratorFrom[C]`,
	  *   - `E &lt;: G Extended _` for some `G &lt;: FromClause` such that `F PrefixOf G`,
	  *   - `E &lt;: S GroupByAll _` for some `S &lt;: E#Nested`,
	  *   - `F =:= Dual` and `E &lt;: O GroupByAll _` for some `O &lt;: OuterFrom`,
	  *
	  * It means that `E =:= F E1 T1 ... En Tn`,
	  * or `E =:= F E1 O1 ... En On Subselect T0 J1 T1 ... Jm Om GroupByAll G1 By ... By Gk`,
	  * or `E =:= F E1 O1 ... En On Subselect T0 J1 T1 ... Jm Om GroupByAll G1 By ... By Gk Subselect S0 U1 S1 ... Sl`
	  * or `F =:= Dual` and `E =:= T0 J1 T1 ... Jn Tn GroupByAll G1 By ... By Gk`, for some join types
	  * `Ji[L, R] &lt;: L Join R`, `Ei[L, R] &lt;: L Extended R`, `Ui[L, R] &lt;: L Using R` and mapping types
	  * `Ti[O], Oi[O], Gi[O], Si[O] &lt;: MappingAt[O]`. Any decorators in the above examples were ommitted.
	  *
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `E` containing all the relations of `F` unless `F` is complete.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	@implicitNotFound("The FROM clause ${F} is not a prefix of the clause ${E}.")
	class PrefixOf[F <: FromClause, E <: FromClause] private[FromClause] (val diff :Int) extends AnyVal {

		/** An `F` [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy ExtendedBy]] `E` instance representing the
		  * same extension of the clause `F`. `ExtendedBy`, unlike `PrefixOf`, is covariant/contravariant in its
		  * first/second parameter.
		  */
		@inline def extension :F ExtendedBy E = new ExtendedBy(diff)

		/** A transitive proof that if `E PrefixOf C`, then also `F PrefixOf C` (concatenation of the two witnesses). */
		@inline def +[C <: FromClause](next :E PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](diff + next.diff)

		/** A transitive proof that if `E PrefixOf C`, then also `F PrefixOf C` (concatenation of the two witnesses). */
		@inline def extend[C <: FromClause](implicit next :E PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](diff + next.diff)

		/** A transitive proof that `F PrefixOf (E J T)` for any `Extended` subtype `J` and mapping type constructor `T`. */
		@inline def extend[J[+L <: E, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] :F PrefixOf (E J T) =
			new PrefixOf[F, E J T](diff + 1)

		/** A transitive proof that `F PrefixOf D[E]` for any ''from'' clause decorator (with no change in length). */
		@inline def wrap[D[B <: E] <: DecoratedFrom[B]] :F PrefixOf D[E] = new PrefixOf[F, D[E]](diff)
	}



	object PrefixOf {
		@inline def apply[F <: FromClause, E <: FromClause](implicit ev :F PrefixOf E) :F PrefixOf E = ev

		implicit def itself[F <: FromClause] :F PrefixOf F = new PrefixOf(0)

		implicit def extend[F <: FromClause, E <: FromClause, L <: U, U <: FromClause]
		                    (implicit decompose :ClauseDecomposition[E, L, U], prefix :F PrefixOf L) :F PrefixOf E =
			new PrefixOf[F, E](prefix.diff + decompose.extension.diff)

		implicit def group[F <: FromClause, O <: FromClause, E <: FromSome, R[A] <: MappingAt[A]]
		                  (implicit outer :O OuterClauseOf E, ev :F PrefixOf O) :F PrefixOf (E GroupByAll R) =
			new PrefixOf(ev.diff + 1)
	}

}


