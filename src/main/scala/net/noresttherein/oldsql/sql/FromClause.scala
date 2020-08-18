package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Relation, SQLForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.Relation.NamedRelation
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.GetTable.{ByIndex, ByLabel, ByParamName, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.GetTable.ByIndex.ByNegativeIndex
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, JoinedEntities, OuterFrom, PrefixOf, SubselectFrom, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.{ComponentSQL, FreeColumn, JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.{?:, FromParam, LabeledFromParam, ParamRelation, WithParam}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.DecoratedFrom.{Alias, DecoratorUpcasting, FromSomeDecorator, GenericDecorator}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SelectSQL.{SelectAs, SelectColumn}
import net.noresttherein.oldsql.sql.SQLScribe.RemoveParams





/** In its basic use, a `FromClause` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in a query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] class hierarchy)
  * are defined, providing all non-constant values available to them. It consists of a list of `Mapping`s,
  * together with an optional filter working on those mappings, especially any required join conditions.
  * While the individual elements of a clause are referred to often as tables for simplicity,
  * they can not only be arbitrary relations such as other ''selects'', but also synthetic artifacts
  * such as query parameters. It is even possible to use arbitrary mapping components, to be replaced at a later time
  * with references to concrete tables, parameters or constants. As such, it doesn't necessarily represent
  * a valid fragment of a select from the application's schema. In that case it's best thought as a signature containing
  * declarations of terms (mappings and parameters) over which SQL formulas can be defined;
  * hence `SQLExpression` is parameterized with a `FromClause`.
  *
  * The mappings taking part in the query are encoded in the static type of the standard implementation, which
  * builds both the instance and its type by recursively applying the [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
  * type and its subtypes to a shorter clause, extending it by another relation. In that aspect it is quite similar
  * to general purpose heterogeneous lists such as the ''shapeless'' `HList` and
  * [[net.noresttherein.oldsql.collection.Chain Chain]], replacing its wide functionality with specific application
  * to this task. Just as a `HList` - and all other two-argument type constructors - it can be and is better written
  * using the infix notation: `Dual Join Rangers Join Familiars Join Monsters`. Note that the whole `Join`
  * class hierarchy is left-associative for more natural left to right reading and writing. Specialized classes for
  * all join kinds exist, as well as special variants for ''from'' clauses of subselects of some outer clause as well
  * as unbound query parameters. Other implementations, not derived from `AndFrom` are also possible, in particular
  * decorators extending [[net.noresttherein.oldsql.sql.DecoratedFrom DecoratedFrom]] which introduce no new relations,
  * but can add other features. As subtypes are also universally covariant regarding their `FromClause` type parameters,
  * any prefix can be always substituted with the abstract `FromClause` supertype, balancing static type checking
  * with a degree of freedom and flexibility promoting code reuse. A `FromClause` subtype from the above category
  * is said to be ''complete'' if its definition starts with the [[net.noresttherein.oldsql.sql.Dual Dual]]
  * or [[net.noresttherein.oldsql.sql.From From]] type, that is the number of all mappings participating
  * in the join/clause is known (although their types may be abstract). Note that the mapping types may
  * still be abstract and the joins may be some generic base type like `AndFrom`. The opposite is an ''incomplete''
  * clause, the definition of which starts with the base `FromClause` type or an abstract type `F &lt;: FromClause`:
  * `FromClause Join Deaths Join Instruments`, which specifies any sequence of relations ending with the `Deaths`
  * and `Instruments`.
  *
  * This trait is a bare bones common upper type, serving at the same time as a wildcard in type definitions meaning
  * 'an arbitrary number of other preceding relations not relevant in the circumstances at hand'. Most functionality
  * is defined in the `AndFrom` subclass representing actual non-empty clauses. Some additional generic methods however
  * are made available through implicit conversions in order to benefit from the static self type:
  *   - from any `F &lt;: FromClause`
  *     to [[net.noresttherein.oldsql.sql.FromClause.FromClauseExtension FromClauseExtension]], which declares
  *     some accessors, in particular
  *     `relations` to [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] and
  *     `entities` to [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]], as well as
  *     `join` methods for appending new relations;
  *   - from non-empty clauses `F &lt;: FromSome`
  *     to [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]], which define factory methods
  *     for other join types;
  *   - from clauses without a `Subselect` join `F &lt;: OuterFrom`
  *     to [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension OuterFromExtension]] for adding unbound
  *     query parameters in the form of the [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] synthetic 'joins'.
  *
  * Aside from the above extensions, associated implicits and other helper classes, the companion object
  * defines some base types serving as common upper bounds and several useful type aliases which enforce
  * certain features on `FromClause` instances, such as [[net.noresttherein.oldsql.sql.FromClause.OuterFrom OuterFrom]],
  * [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]] and others.
  *
  * @see [[net.noresttherein.oldsql.sql.FromClause.FromSome]] the base trait for all non-empty clauses.
  * @see [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] the 'link' type extending a preceding clause
  *     by another relation and the base trait for all join kinds.
  * @see [[net.noresttherein.oldsql.sql.Dual Dual]] the standard empty clause used for ''select'' statements
  *     without a ''from'' clause (or 'select ... from Dual' in Oracle) as well as the terminator of non-empty
  *     relation lists.
  * @see [[net.noresttherein.oldsql.sql.From From]] The factory for clauses with a single relation which
  *     is the most common starting point for building larger clauses.
  * @see [[net.noresttherein.oldsql.sql.Join Join]] The supertype for all join kinds as well as subselect clauses.
  * @author Marcin Mościcki
  */
trait FromClause { thisClause =>

	/** Type of the last `Mapping` in this join (the rightmost one) if not empty. `Dual` defines it as `Nothing`. */
	type LastMapping[O] <: MappingAt[O]

	/** Type alias for the last relation in this list as seen from some `FromClause` type `F` containing this instance
	  * in its 'tail'. In other words, this projects the type of the last element of this clause to an extending clause.
	  * `JoinedRelation[F, LastMapping]` provides access to the `Mapping` for the relation together
	  * with all its components and is an instance of `SQLExpression[F, LastMapping[_]#Subject]`, meaning it can be used
	  * as part of larger SQL expressions. Note that `Dual`, containing no relations, defines this type as `Nothing`.
	  */
	type LastTable[F <: FromClause] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `FromClause` using
	  * the most abstract 'join' kind `AndFrom`. If this clause is not a `AndFrom` subclass but a decorator,
	  * the ''generalized'' supertype of the decorator is used as the wrapper over `FromClause AndFrom LastMapping`.
	  * It is the type this instance's `last` joined relation is based on. `Dual` defines this type as the most generic
	  * `FromClause` to indicate that any expression based on an empty clause can be used as part of expressions
	  * for any other clause.
	  */
//	type FromLast >: this.type <: FromClause
	type FromLast <: FromClause

	/** The last relation in this clause when treated as a list, if any.
	  * @throws NoSuchElementException if this clause is empty.
	  */
	def last :LastTable[FromLast]

	/** The last relation in this clause as an expression based on some extending clause `E`.
	  * This is equivalent to `last.extend[E]`, but returns the result as the narrowed `LastTable[E]` rather than
	  * `JoinedRelation[E, LastMapping]`. This preserves the `Nothing` return type for empty clauses and allows
	  * its use in the `on` filtering method.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.PrefixOf]]
	  */
	def lastAsIn[E <: FromSome](implicit extension :FromLast PrefixOf E) :LastTable[E]



	/** Similar to `Implicit`, this type is always defined as `Dual`, with all join kinds defining it as `left.Init`.
	  * This way, if the compiler can prove that `F#Init =:= Dual`, then the static type `F` is a complete clause -
	  * with all relations and joins known. It is of little use by itself in the client code.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.CompleteFrom]]
	  */
	type Init <: FromClause



	/** The super type of this instance used as the basis for its ''where'' clause filter expression (and others).
	  * It abstracts over the join kinds used and focuses only on the relations available to SQL expressions
	  * based on this clause. It is formed by replacing all join kinds (and decorators) present in the complete signature
	  * of this type with their most generic versions which still provide distinctions necessary for all external types
	  * working with `FromClause` instances to provide their full functionality. This process works recursively,
	  * with every `FromClause` subtype accepting another `F &lt;: FromClause` type parameter which corresponds
	  * to some member `val f :F` substituting `f.Generalized` for `F`.
	  * The filter for the associated ''where'' clause and all joined relations accessed through
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]]
	  * conform to `SQLExpression[Generalized, _]`.
	  * with all join kinds replaced with the root type `AndFrom` and initial `Dual`
	  * with `FromClause`. Clauses with the same `Generalized` type are considered equivalent for most purposes.
	  */
	type Generalized >: Self <: FromLast { type Generalized <: thisClause.Generalized } //todo: extra refinement

	/** This clause upcast to its generalized supertype in which all join kinds are replaced with their least upper
	  * bounds which still retain the information about their function.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  */
	def generalized :Generalized = self

	/** A recursive reconstruction of this clause's concrete type. It is defined by replacing every type parameter
	  * `F &lt;: FromClause` and corresponding to a member `val f :F` with `f.Self`. Concrete clause `C` without
	  * type parameters (`Dual`) define it simply as `type Self = C`, terminating the recursion. The result is
	  * the lowest upper bound of a clause `F` without abstract `FromClause` ''subclasses'' in its definition.
	  * If `F` is an abstract type, all abstract type parameters in its signature remain abstract types in `f.Self`.
	  * This procedure circumvents possible covariance in the type parameter `F`, which would prevent defining an alias
	  * for the self type using `F` directly in its definition. For a concrete class with fully instantiated
	  * type parameters, it is its concrete supertype. While impossible to enforce in the type system, for any
	  * `F &lt;: FromClause`, `F &lt;: F#Self` ''iff'' `F` is a concrete clause - one without any abstract subtypes
	  * and abstract subclasses of `FromClause` in its signature. Note that it is possible for a concrete class `S`
	  * to extend another concrete `FromClause` subclass `B`, in which case `b.Self =:= s.Self`,
	  * for some `val b :B[]; val s :S[]`, where `B[]` and `S[]` are concrete types resulting from applying `B` and `S`
	  * to their type parameters.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#This]]
	  */
	type Self <: FromLast { type Self = thisClause.Self; type Generalized = thisClause.Generalized }

	/** This clause as its [[net.noresttherein.oldsql.sql.FromClause#Self Self]] type. */
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
//	type This >: this.type <: FromLast
	type This <: FromClause// <: FromLast //>: this.type <: FromLast



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




	/** The ''WHERE'' part of this clause representing the filter condition as SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause.
	  */
	def filter :SQLBoolean[Generalized]

	/** The combined join conditions of all joins in this clause as an expression based on an extending clause.
	  * Used by zero-argument `filter` to request the individual join conditions as expressions based on the clause
	  * it was called for, without the need for adapting every member condition for every intermediate join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def filter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E]



	/** Does this clause contain no relations, being a base for a select without a ''from'' clause? */
	def isEmpty :Boolean

	/** Does this clause contain any relations? */
	@inline final def nonEmpty :Boolean = !isEmpty

	/** Number of relations contained in this join (counting all their occurrences separately). */
	def size :Int


	/** Subject types of all mappings in this clause, concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `AndFrom` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type Row <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all joined elements in their order
	  * of appearance. It will contain entries for all mappings in this clause, including parameter mappings
	  * and mappings listed in this clause's `Implicit` prefix (if this clause is a subselect clause).
	  */
	def row :ChainTuple[Generalized, Row]

	/** Create an SQL tuple expression, based on some extending clause `E`, containing `JoinedRelation` expressions
	  * for all joined elements in their order of appearance. It will contain entries for all mappings in this clause,
	  * including parameter mappings and mappings listed in this clause's `Implicit` prefix (if this clause
	  * is a subselect clause. This overloaded variant is used by the zero-argument `row` to obtain the chain prefix
	  * containing the relation formulas based on the final clause type from a prefix clause of a join,
	  * without the need to adapt each relation once for every intermediate join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def row[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, Row]

	/** All relations in this clause as abstract, untyped mappings, in the reverse order of their appearance.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def tableStack :LazyList[RelationSQL.AnyIn[Generalized]]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * The `JoinedFormula`s are based on some extending clause `E`, so that the stack for a prefix clause
	  * can be used as-is, with no need to map over it, by extending clause's zero argument `tableStack`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def tableStack[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]]



	/** A join between this clause and the relation for mapping `T`. For non empty clauses, this is simply `J[Self, T]`,
	  * but `Dual` defines it as `From[T]` instead.
	  */ //consider: using This instead of Self, we might be able to get rid of the protected extend variant
	type Extend[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, T[O] <: MappingAt[O]] <: Self AndFrom T

	/** Joins this clause with another relation `next` for mapping `T`.
	  * @param next the relation to add to the clause.
	  * @param filter the additional filter condition for the ''where'' clause of the created clause; defaults to `True`.
	  * @param join a template `Join` instance used as a factory for the returned clause; defaults to `InnerJoin`.
	  * @return `From(next)` if this clause is empty and `join.likeJoin(self, next)` for non empty clauses.
	  */
	def extend[T[O] <: BaseMapping[S, O], S]
	          (next :Relation[T], filter :SQLBoolean[Generalized AndFrom T] = True, join :Join.* = InnerJoin.template)
			:Extend[join.LikeJoin, T]

	/** Used to add any relation to any clause, creates the clause of a type depending on this clause:
	  * empty clauses return `From[T]`, while non empty clauses create `this.type InnerJoin T`.
	  */
	protected[sql] def extend[T[O] <: BaseMapping[S, O], S]
	                         (right :LastRelation[T, S], filter :SQLBoolean[Generalized AndFrom T]) :this.type AndFrom T

	/** Joins the given parameter clause with this clause. The type of the resulting clause is the result
	  * of replacing the empty clause `Dual` in this clause's type with `P` and replacing `From[X]` with `P AndFrom X`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#JoinedWith]]
	  */
	type AppendedTo[+P <: FromClause] <: Generalized

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
	  *///todo: both appendedTo and join with will require revision once grouped clauses are introduced.
	def appendedTo[P <: FromClause](prefix :P) :AppendedTo[P]


	/** A join between this clause and the clause `F`. This type is the result of replacing `Dual` in `F` with `Self`.
	  * Non-empty clauses define it as `F#JoinedWith[Self, J]]`, while `Dual` defines it as `F` - the indirection
	  * enforced by the join type `J` (and `Join` subclasses) having `FromSome` as the upper bound of their left side.
	  */
	type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, F <: FromClause] <: FromClause

	/** Joins the clause given as the parameter with this clause. If any of the clauses is empty, the other is
	  * returned. Otherwise the created clause contains this clause as its prefix, followed by all relations
	  * from `suffix` joined with the same join kinds. The first `From` pseudo join in suffix is replaced with
	  * the join type specified by the template parameter (`join.LikeJoin`); if the first relation in `suffix` is
	  * however joined with `Dual` using another join type (such as `JoinParam`), it is preserved. This is a dispatch
	  * call to [[net.noresttherein.oldsql.sql.FromClause#joinedWith suffix.joinedWith(self, join)]]. This extra level
	  * of indirection is enforced by the upper bound of `FromSome` on the left type parameters in `Join` classes,
	  * while this method can be called also if this clause is empty. Additionally, the join kind to use between
	  * the last relation in this clause and the first relation in `suffix` can be specified as `Subselect`,
	  * while `joinedWith` allows only `TrueJoin` subclasses.
	  *
	  * It is a low level method and client code should prefer the eponymously named extension methods
	  * for individual join kinds defined in
	  * [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]]: `join`, `outerJoin`,
	  * `innerJoin`, `leftJoin`, `rightJoin`, `subselect`, as they have more friendly return types.
	  * @param suffix the clause with relations which should be added to this clause
	  * @param join a template instance to use as the factory for the join between the last relation in this clause
	  *             and the first relation in `suffix`.
	  */
	def joinWith[F <: FromSome](suffix :F, join :Join.* = InnerJoin.template) :JoinWith[join.LikeJoin, F]

	/** A join of relations from the clause `P` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `P`, that is appending all mappings from this clause
	  * in order to `P`, using the same join kinds. The join type `J` is used as the join between the last relation
	  * in `P` and the first relation in `this`, unless this clause starts with a `JoinParam` join.
	  * It is a narrower variant of [[net.noresttherein.oldsql.sql.FromClause#AppendedTo AppendedTo]],
	  * with the join type `J`'s left side being restricted to non-empty clauses.
	  */
	type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] <: Generalized

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
	  * @see [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join]]
	  */
	def joinedWith[F <: FromSome](prefix :F, firstJoin :TrueJoin.*) :JoinedWith[F, firstJoin.LikeJoin]

	/** Represents this ''from'' clause as a subselect of the clause `prefix` by joining them with the
	  * [[net.noresttherein.oldsql.sql.Subselect Subselect]] artificial join. The resulting clause
	  * is the result of appending the first relation from `this` to the `prefix` clause using the `Subselect` join,
	  * and then all the following relations using the same joins as in this clause. All join conditions are copied,
	  * but any additional condition narrowing the resulting cross join must be added manually.
	  * This method is the delegate target
	  * of the [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#subselect subselect]]
	  * method available through an implicit conversion, which should be used instead by the client code.
	  * @throws UnsupportedOperationException if this clause is empty or the first join in this clause is a `JoinParam`.
	  */
	def joinedAsSubselect[F <: FromSome](prefix :F) :JoinedWith[F, Subselect]


	/** Creates a ''from'' clause for a subselect of a select with this clause. It uses the given clause `subselect`
	  * as its ''explicit'' suffix (i.e. the relations from the actual ''from'' clause of the associated subselect),
	  * and this clause as the ''implicit'' prefix, joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]]
	  * pseudo join. The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns the `subselect` argument unchanged. All non-subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[Dual]]], so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities#from from]] method of
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#subselect subselect]] extension method instead,
	  * which accepts any non-empty clause, not only `OuterFrom` subtypes.
	  * @param subselect the actual, explicit ''from'' clause of a target subselect.
	  * @return a ''from'' clause conforming to `SubselectOf[Generalized]`, adapted from the `subselect` argument
	  *         by prepending this clause to it.
	  */
	def from[F <: FromSome with OuterFrom](subselect :F) :JoinWith[Subselect, F]

	/** Creates a ''from'' clause for a subselect of a select with this clause. It will have the given relation
	  * as the only member of the ''explicit'' portion (i.e. the one actually appearing in the ''from'' clause
	  * of the generated SQL) and this clause as the ''implicit'' prefix,
	  * joined with the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. All relations included
	  * in this clause are available to any `SQLExpression` parameterized with the type of the returned clause.
	  * The method works differently for empty clauses: as an empty clause cannot appear on the left side
	  * of `Subselect`, it simply returns `From(subselect)`. All non subselect clauses conform
	  * to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[Dual]], so the returned clause
	  * is a valid subselect clause of `Generalized` (and `Self`) either way.
	  *
	  * This method is the implementation target of the
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities#from from]] method of
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedEntities JoinedEntities]] and
	  * [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]]; client code may prefer to use
	  * the [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#subselect subselect]] extension method instead,
	  * which has a friendlier return type.
	  * @param first the sole relation of the ''from'' clause of the new subselect clause.
	  * @param infer implicit witness guiding the compiler to properly infer the subject type of mapping `M` (and `T`).
	  * @return `Self Subselect T`, or `From[T]` if this clause is empty.
	  */
	def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	        (first :Relation[M])
//	        (implicit cast :InferSubject[this.type, Subselect, M, T, S])
	        (implicit infer :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
		:Extend[Subselect, T]



	/** A property specifying if this ''from'' clause is a subselect clause, that is it has a `Subselect` join
	  * somewhere in its complete (dynamic) type.
	  */
	def isSubselect :Boolean = false

	//todo: reconcile the upper bound of FromSome on Implicit
	def asSubselect :Option[SubselectFrom { type Implicit = thisClause.Implicit with FromSome }] =
		if (isSubselect) Some(this.asInstanceOf[SubselectFrom { type Implicit = thisClause.Implicit with FromSome }])
		else None



	/** Super type of all `FromClause` subtypes representing a subselect directly nested under this clause's select.
	  * As a member type, it is able to define `Implicit` type of this refinement as `this.Generalized`,
	  * `S &lt;: f.Nested` being thus a more specific way of establishing the relationship than `S &lt;: SubselectOf[F]`.
	  * This type matches only 'true' subselect clauses, that is types `S &lt;: Generalized Subselect T0 J1 T1 ... JN TN`,
	  * where `J1...JN` are `TrueJoin` subtypes. This means that non-subselect, 'outer' clauses do not conform
	  * to this type despite free select expressions being valid subselects of any select expression.
	  * This stands in contrast to [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]],
	  * which includes outer clauses.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type Nested = SubselectFrom { type Implicit = thisClause.Generalized }


	/** The incomplete suffix clause containing all relations since the last `Subselect` join or `Dual`, with all
	  * join kinds replaced with their ''generalized'' form, while the `Subselect` in question with `AndFrom`.
	  * It lists all relations present in the actual ''from'' clause of the associated SQL select in a canonical form,
	  * abstracting over join kinds. It can be formed by substituting in the complete `Generalized` type of this clause
	  * the `Implicit` prefix, containing relations present 'implicitly' in the scope, with `FromClause`. All `AndFrom`
	  * subclasses define it as `left.Explicit AndFrom R`, with the exception of `Subselect` join, which defines it
	  * as `left.Generalized` instead, while `Dual` defines it as `FromClause` - consistently with `Generalized`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Inner]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Generalized]]
	  */
	type Explicit >: Generalized <: FromLast

	/** The suffix clause containing all relations from the explicit portion of this subselect clause.
	  * It represents the actual ''from'' clause of an associated SQL subselect. For subselect clauses, it results from
	  * replacing the left side of the last `Subselect` in this type's `Self` member with `FromSome`; for non-subselect
	  * clauses (with no `Subselect` joins in its complete type), it is the full type of this clause with `Dual`
	  * replaced with `FromClause` (or `From[T]` with `Dual AndFrom T`).
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Self]]
	  */
	type Inner >: Self <: Explicit

	/** A prefix ''from'' clause of this clause consisting of all relations available to SQL expressions implicitly,
	  * that is not included in the actual ''from'' clause of this subselect clause.
	  * It is the generalized form of the outer clause if this clause represents a subselect clause, created by
	  * `Implicit.subselect()`. All `TrueJoin` subclasses and `From` have this type equal to the `Implicit` type
	  * of their left side, but `Subselect` defines `Implicit` as the generalized type of its left side. Additionally,
	  * all 'proper' joins conform to `AsSubselectOf[L#Implicit]` and `L Subselect R` conforms to `AsSubselectOf[L]`.
	  * As a special case, `JoinParam` defines it as `Nothing`, in order to eliminate the possibility of using it
	  * as a part of an 'inner' subselect clause, which would hide its existence from the select based on the outer
	  * clause. Therefore, `Implicit` is equal to the `Generalized` type of the left side of the last `Subselect`,
	  * or `FromClause` for non subselect clauses. This means that for any generalized type `S &lt;: FromClause`
	  * with fully instantiated parameters (the clause is complete and the `Generalized` type is well defined) value
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
	type Implicit <: FromClause

	/** A prefix ''from'' clause of this clause consisting of all joins and relations preceding the last (rightmost)
	  * `Subselect` join. If there are no `Subselect` joins in the complete type definition of this clause,
	  * it is equal to `Dual`. It represents the clause of the outer select for this subselect clause, assuming
	  * this clause is a subselect clause created by `outer.subselect()` (and possibly expanded further).
	  * All `TrueJoin` subclasses and `From` have this type equal to the `Outer` type of their left side, but `Subselect`
	  * defines `Outer` as the `Self` type of its left side. As a special case, `JoinParam` defines it as `Nothing`
	  * to prevent any clause containing it on the right side of a `Subselect` from actually conforming to
	  * `SubselectOf[left.Outer]` This means that for any type `S &lt;: FromClause` with fully instantiated parameters
	  * (the clause is complete, that is the concrete types of all joins in it are known) value
	  * `(s :S) subselect t1 join t2 ... join t3` conforms to `s.Nested` and `FromClause { type Outer = s.Self }`.
	  * This way one can statically express a dependency relationship between ''from'' clauses without resorting
	  * to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Inner]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Self]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type Outer <: Implicit

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselect()`. When viewed as a list of relations, `outer` constitutes
	  * the result of dropping all relations joined in this instance up until and including a relation joined
	  * by a `.subselectFrom()` call, going from right to left. If there's no `Subselect` in the dynamic type definition
	  * of this clause, meaning this is not a subselect clause (the actual ''from'' clause of resulting selects
	  * will include all members of this clause), `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or just the left side if this instance is a `Subselect`.
	  * @throws UnsupportedOperationException if there is a `JoinParam` 'join' to the right of the last `Subselect`
	  *                                       (or `Dual`, if this is not a subselect clause).
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	def outer :Outer



	/** The ''WHERE'' clause of this subselect clause representing the explicit filter condition as an SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause since the last `Subselect` join.
	  * For non subselect clauses, it is equal to [[net.noresttherein.oldsql.sql.FromClause#filter filter]]
	  */
	def subselectFilter :SQLBoolean[Generalized]

	/** The combined join conditions of all joins since the last `Subselect` join as a expression based on an extending
	  * clause. Used by zero-argument `filter` to request the individual join conditions as formulas based on the clause
	  * it was called for.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def subselectFilter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E]



	/** Number of relations contained in the explicit portion of this subselect join. This is equal to
	  * the number of mappings to the right of the rightmost `Subselect` join or `Dual`, that is `size - outer.size`.
	  */
	def subselectSize :Int = size - outer.size

	/** Subject types of all mappings in this clause following the `Implicit` prefix, concatenated into
	  * a [[net.noresttherein.oldsql.collection.Chain Chain]]. This amounts to the list of entities from
	  * all the relations from the ''from'' clause of the most deeply nested subselect. If this clause doesn't represent
	  * a subselect, but a top-level query, it is the same as `Row`. The chain contains the mapped types
	  * in the same order as their mappings appear in this type's definition and is, like `AndFrom` (but unlike `::`),
	  * left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#subselectRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Row]]
	  */
	type SubselectRow <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` expressions for all joined relations
	  * in the most deeply nested subselect, in their order of appearance. This includes all relations in this clause
	  * following the most recent `Subselect` 'join', marking the first relation following the `Implicit` clause prefix.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	def subselectRow :ChainTuple[Generalized, SubselectRow]

	/** Create an SQL tuple expression containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. The formulas are based on some extending clause `E`,
	  * so they can be used by the zero-argument `subselectRow` as the chain prefix of its result.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def subselectRow[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first relation following the `Implicit` prefix (the one joined with the last `Subselect` 'join').
	  * If this is not a subselect clause (no `Subselect` 'joins' are present in this clause
	  * and `Implicit =:= FromClause`), all relations are included.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#subselectRow]]
	  */
	def subselectTableStack :LazyList[RelationSQL.AnyIn[Generalized]]

	/** All relations in this clause in the reverse order, ending with the last (right-most) appearance of a `Subselect`
	  * 'join' or `Dual`. The elements are returned as `JoinedRelation`s for generic, untyped mappings,
	  * based on some extending clause `E`. Used by the zero-argument `subselectTableStack`
	  * to request the tail of the stack with expressions of the correct type from the prefix clause.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def subselectTableStack[E <: FromSome]
	                       (target :E)(implicit extension :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]]



	/** A type constructor appending all relations joined since the last (rightmost) `Subselect` 'join' (or `Dual`)
	  * to the given clause `F`. This is the result of replacing everything to the left of the last `Subselect`
	  * in this type's signature with `F`. For any concrete subselect clause `J &lt;: FromSome`,
	  * `J =:= J#AsSubselectOf[J#Outer]` and `J#AsSubselectOf[J#Implicit] &lt;:&lt; J#Generalized`.
	  * For outer clauses, it replaces the first `From[T]` 'join' with `F Subselect T`, but `Dual`, despite already
	  * being a subselect clause of any clause, defines it as `Nothing` as an exception, because joining the result
	  * with additional relations would otherwise ''not'' become a subselect clause of `F`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type AsSubselectOf[+F <: FromSome] <: FromLast {
		type Explicit = thisClause.Explicit
	}

	/** For subselect clauses - that is subtypes with a `Subselect` join kind occurring somewhere in their definition,
	  * not necessarily `Subselect` instances - it represents them as a subselect of a clause `F`, being
	  * an extension of their outer clause (the left side of the right-most `Subselect` join). In syntactic terms,
	  * it replaces the `Outer` type in this type's definition with type `F`. Procedurally, it joins in order
	  * all relations since the last occurrence of a `Subselect` join, forming the explicit ''from'' clause of
	  * a modified subselect, with the new outer clause `F`, preserving all join conditions. If this clause is not a
	  * subselect clause, this method will use `Subselect` 'join' to join the `newOuter` with the first relation of this
	  * clause. All join conditions are preserved.
	  * @return a ''from'' clause conforming to `newOuter.Nested` and `SubselectOf[F]`.
	  * @throws `UnsupportedOperationException` if this clause is empty or there is a `JoinParam` 'join'
	  *                                         in its explicit portion.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
	  */
	def asSubselectOf[F <: FromSome](newOuter :F)(implicit extension :Implicit ExtendedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self }



	/** A [[net.noresttherein.oldsql.collection.Chain Chain]] listing all parameters of this clause joined with
	  * the [[net.noresttherein.oldsql.sql.FromClause.OuterFromExtension.param param]] method.
	  * In particular, a `FromClause` subtype with no `JoinParam` joins in its full type has this type equal to `@~`.
	  * This is can be used to statically enforce that a clause is unparameterized by refining on this type:
	  * `from :FromClause { type Params = @~ }`, or `from :ParameterlessFrom` as a less bug-prone alternative.
	  */
	type Params <: Chain



	/** Creates an SQL ''select'' expression with this clause used for the ''from'' and ''where'' clauses
	  * and the ''select'' clause consisting of all selectable columns of the mapping returned by the passed function.
	  */
	def selectAs[C[A] <: MappingAt[A], M[A] <: BaseMapping[S, A], S, O >: Generalized <: FromClause]
	            (component :JoinedEntities[Generalized] => C[O])
	            (implicit typer :Conforms[C[O], M[O], BaseMapping[S, O]], shift :TableShift[O, M, _ <: Numeral])
			:Implicit SelectAs M[Any] =
		{
			type Mock = RelationSQL[Generalized, MappingOf[Any]#TypedProjection, Any, O]
			val table = tableStack(shift.tables).asInstanceOf[Mock]
			val comp = table \ component(generalized.entities)
			if (isSubselect) {
				type AsSubselect = Generalized with SubselectFrom { type Implicit = thisClause.Implicit }
				comp.subselectFrom(this.asInstanceOf[AsSubselect])
			} else
				comp.selectFrom(this.asInstanceOf[Generalized with OuterFrom])
		}


	/** Creates an SQL ''select'' expression with this clause used for the ''from'' and ''where'' clauses
	  * and the ''select'' clause consisting of all selectable columns of the passed mapping.
	  */
	def selectAs[T[A] <: BaseMapping[E, A], E, M[A] <: BaseMapping[S, A], S, O >: Generalized <: FromClause]
	            (component :ComponentSQL[Generalized, T, E, M, S, O]) :Implicit SelectAs M[Any] =
		if (isSubselect) {
			type AsSubselect = Generalized with SubselectFrom { type Implicit = thisClause.Implicit }
			component.subselectFrom(this.asInstanceOf[AsSubselect])
		} else
			component.selectFrom(this.asInstanceOf[Generalized with OuterFrom])


	/** Creates an SQL ''select'' expression with this clause used for the ''from'' and ''where'' clauses
	  * and the expression returned by the passed function as its ''select'' clause.
	  */
	@inline def select[V](header :JoinedEntities[Generalized] => SQLExpression[Generalized, V])
			:SelectSQL[Implicit, V, _] =
		select(header(generalized.entities))


	/** Creates an SQL ''select'' expression selecting a single column, as defined by the result of the passed function,
	  * and this instance as the source of its ''from'' and ''where'' clauses.
	  */
	@inline def selectColumn[V](header :JoinedEntities[Generalized] => ColumnSQL[Generalized, V])
			:SelectColumn[Implicit, V, _] =
		select(header(generalized.entities))


	/** Creates an SQL ''select'' expression selecting an arbitrary `SQLExpression`. The ''from'' and ''where'' clauses
	  * are defined by this instance, while the ''select'' clause consists of all column sub-expressions constituting
	  * the `header` subexpressions.
	  */
	def select[V](header :SQLExpression[Generalized, V]) :SelectSQL[Implicit, V, _] =
		if (thisClause.isSubselect) {
			type AsSubselect = Generalized with SubselectFrom { type Implicit = thisClause.Implicit }
			header.subselectFrom(this.asInstanceOf[AsSubselect])
		} else
			header.selectFrom(this.asInstanceOf[Generalized with OuterFrom])


	/** Creates an SQL ''select'' expression selecting a single column, as defined by the passed `header` `ColumnSQL`,
	  * and this instance as the source of its ''from'' and ''where'' clauses.
	  */
	def select[V](header :ColumnSQL[Generalized, V]) :SelectColumn[Implicit, V, _] =
		if (thisClause.isSubselect) {
			type AsSubselect = Generalized with SubselectFrom { type Implicit = thisClause.Implicit }
			header.subselectFrom(this.asInstanceOf[AsSubselect])
		} else
			header.selectFrom(this.asInstanceOf[Generalized with OuterFrom])



	/** Creates an SQL ''select'' expression selecting all columns from all relations of the explicit (subselect)
	  * portion of this clause as a `Chain` of relation subjects. This is equivalent to `select(this.row)`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#SubselectRow SubselectRow]]
	  */
	def select_* :SelectSQL[Implicit, SubselectRow, _] = select(subselectRow)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]

}






/** A companion object for the [[net.noresttherein.oldsql.sql.FromClause FromClause]] trait representing
  * somewhat generalized ''from'' clauses of SQL select statements. It serves primarily as a namespace
  * for related classes and implicit values.
  */
object FromClause {

	/** Common upper bound for all ''from'' clauses containing at least one relation. Extended by every
	  * `FromClause` implementation other than `Dual`.
	  */
	trait FromSome extends FromClause { thisClause =>
		override type LastTable[F <: FromClause] = JoinedRelation[F, LastMapping]
//		override type FromLast >: this.type <: FromSome
		override type FromLast <: FromSome
		override type This <: FromSome

		override def lastAsIn[E <: FromSome](implicit extension :FromLast PrefixOf E) :LastTable[E] =
			last.extend[E]

		override def isEmpty :Boolean = false

		override def filter :SQLBoolean[Generalized] = filter(generalized)
		override def row :ChainTuple[Generalized, Row] = row(generalized)
		override def tableStack :LazyList[RelationSQL.AnyIn[Generalized]] = tableStack(generalized)



		override type Extend[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, T[O] <: MappingAt[O]] = Self J T

		override def extend[T[O] <: BaseMapping[S, O], S]
		             (next :Relation[T], filter :SQLBoolean[Generalized AndFrom T], join :Join.*) :join.LikeJoin[Self, T] =
			join.likeJoin[Self, T, S](self, next)(filter)

		protected[sql] def extend[T[O] <: BaseMapping[S, O], S]
		                         (right :LastRelation[T, S], filter :SQLBoolean[Generalized AndFrom T])
				:this.type AndFrom T =
			InnerJoin[this.type, T, S](this, right)(filter)


		override type JoinWith[+J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, F <: FromClause] =
			F#JoinedWith[Self, J]

		override def joinWith[F <: FromSome](suffix :F, join :Join.*) :suffix.JoinedWith[Self, join.LikeJoin] =
			join.likeJoin(self, suffix)

		override def from[F <: FromSome with OuterFrom](subselect :F) :subselect.JoinedWith[Self, Subselect] =
			subselect.joinedAsSubselect(self)

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                 (first :Relation[M])
//		                 (implicit cast :InferSubject[this.type, Subselect, M, T, S])
		                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:Extend[Subselect, T] =
			Subselect(self, cast(first))



		override def subselectFilter :SQLBoolean[Generalized] = subselectFilter(generalized)
		override def subselectRow :ChainTuple[Generalized, SubselectRow] = subselectRow(generalized)
		override def subselectTableStack :LazyList[RelationSQL.AnyIn[Generalized]] = subselectTableStack(generalized)

	}



	/** A type to which all complete ''from'' clauses conform. If `F &lt;: CompleteFrom`, then the number of joined
	  * relations in the clause is known. Note that the mappings can still be abstract types and the join kinds may be
	  * some supertypes of actual joins used. It relies on the `Init` member type of `FromClause` introduced
	  * specifically for this purpose: all `AndFrom` subclasses define it simply as `left.Init`, with `Dual` defining
	  * it as `Dual`. Being able to prove that `Init =:= Dual` means that one can 'see all the way back' from the last join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ConcreteFrom]]
	  */
	type CompleteFrom = FromClause {
		type Init = Dual
	}

	/** A `FromClause` refinement which is the upper bound of all ''from'' clauses with a statically known
	  * `Generalized` type. If `F &lt;: GeneralizedFrom`, then
	  *   a) `F &lt;: CompleteFrom` - the number of joined relations is statically known, and
	  *   b) `F &lt;: F#Generalized` - all 'join' kinds are narrowed down at least to their `Generalized` form
	  *     (although this can't be expressed in the type system).
	  * Clauses conforming to this type generally offer full functionality, in particular regarding access to
	  * their mappings. The above statement holds only for subtypes which conform to this type naturally,
	  * rather than through refinement. In particular, a `from :GeneralizedFrom` is an abstract type for which
	  * no functions depending on the statically known structure (i.e., based on implicit evidence), in particular
	  * access to mappings, will be available
	  * @see [[net.noresttherein.oldsql.sql.FromClause.CompleteFrom]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.ConcreteFrom]]
	  */
	type GeneralizedFrom = FromClause {
		type Init = Dual //just to conform to CompleteFrom, as this condition follows the one on Generalized
		type Generalized >: this.type //<: FromLast
	}

	/** A `FromClause` refinement which is the upper bound of all ''from'' clauses with a statically known
	  * `Self` type. If `F &lt;: ConcreteFrom`, then
	  *   a) `F &lt;: CompleteFrom` - the number of joined relations is statically known,
	  *   b) `F &lt;: GeneralizedFrom` and `F &lt;: F#Generalized` (although the latter cannot be expressed
	  *      in the type system),
	  *   c) `F &lt;: F#Self` - there are no abstract join kinds in the type signature (though this cannot be expressed
	  *      in the type system either).
	  *   d) result types of all methods returning other clauses are likewise `ConcreteFrom` instances, although
	  *      again this cannot be expressed directly.
	  * In essence, conforming to the this type signifies that a type is concrete with respect to the join aspect,
	  * although the mapping parameters may still be abstract types. Conforming ''naturally'' to this type implies
	  * that all functions, in particular those depending on implicit evidence, are available and their result types
	  * are statically known. This does not hold if a clause conforms only through refinement, in particular by
	  * being declared as `ConcreteFrom`, as this type alias carries no information in itself.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom]]
	  */
	type ConcreteFrom = FromClause {
		type Init = Dual //just to conform to CompleteFrom, as this condition follows the one on Generalized
		type Generalized >: this.type //<: FromLast
		type Self >: this.type <: Generalized
	}



	/** A complete `FromClause`, which doesn't contain any parameters and does not represent a subselect
	  * of another clause. `S &lt;: OuterFrom` if and only if it is a complete clause (with no abstract joins
	  * or `FromClause` occurring in its definition) and doesn't contain any `JoinParam` or `Subselect`.
	  * All types conforming to `OuterForm` also conform to
	  * [[net.noresttherein.oldsql.sql.FromClause.GeneralizedFrom GeneralizedFrom]]. Only such clauses can be used
	  * to create independent, most outer select statements, hence the name.
	  */
	type OuterFrom = FromClause { //todo: refinement on additional member types: Explicit, Inner, Outer, Self
		type Init = Dual //just to conform to CompleteFrom, as this condition follows the one on Generalized
		type Implicit = FromClause
		type FromLast >: this.type <: FromClause
		type Generalized >: this.type <: FromLast
	}



	/** An upper bound for `FromClause` subtypes representing ''from'' clauses of subselects of a select
	  * with the ''from'' clause `F`. `S &lt;: AsSubselectOf[F]` if and only if `S` either:
	  *   - has the form of `F Subselect M1 J2 M2 ... JN MN` for some mappings `M1...MN`
	  *     and non-subselect join types `J2...JN`, and both types are complete clauses.
	  *   - is a complete, non subselect clause: `S &lt;: OuterFrom` with no `JoinParam` joins.
	  * Clauses conforming to `AsSubselectOf[F]` can use all the mappings/entities
	  * which are a part of `F`, but they are not a part of any select formulas created from that source. This allows
	  * the use of nested select queries which depend on values from the ''from'' clause of the outer select.
	  * Somewhat counterintuitively, this type is contravariant rather than covariant. There are two reasons behind it:
	  * one, preventing any type from being a subselect clause of a clause with an abstract prefix, ensuring that
	  * full mapping lists are compared, and two, treating all join kinds as equivalent for this purpose.
	  * Note that subselects may be nested to an arbitrary depth and only directly nested subselects of `F`
	  * conform to this type.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Nested]]
	  */
	type SubselectOf[-F <: FromClause] = FromClause {
		type Implicit >: F <: FromClause
	}

	/** An upper bound for all ''from'' clauses of subselect expressions, that is `FromClause` subtypes with a
	  * `Subselect` join in the type definition: `S &lt;: F Subselect T1 ... TN . ` for some type `F &lt;: FromClause`
	  * and mapping type constructors `T1 ... TN`. For a type to conform to `SubselectFrom`, the join kinds in
	  * the explicit part of the clause (after the last subselect) must be statically known to be `TrueJoin` subtypes.
	  * Note that ''outer'' clauses (without any `Subselect` joins) are considered subselect clauses of any other
	  * clause, but do not conform to this type.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type SubselectFrom = FromSome {
		type Implicit <: FromSome //this holds only if there is a Subselect or JoinParam involved (Nothing)
		type Self <: AsSubselectOf[Outer] //for JoinParam this translates to Self <: Nothing
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






	/** A wrapper type adapting the labeled mapping type `L @: M` to a form with a single-argument type constructor
	  * accepting the `Origin` type for use in `AndFrom` subclasses and other types accepting such a type constructor:
	  * `Dual Join (Humans As "humans")#T` (where `Humans[O] &lt;: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */ //consider: As could be the Relation itself; instead of T one would use Relation.Row
	type As[M[O] <: MappingAt[O], A <: Label] = { type T[O] = A @: M[O] }


	class AliasedRelation[T[O] <: MappingAt[O], A <: Label](source :Relation[T], val alias :A)
		extends NamedRelation[A, (T As A)#T]
	{
		override def name :A = alias

		override def apply[O] :A @: T[O] =
			(alias @: source[O].asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[A @: T[O]]
	}

	def AliasedRelation[T[O] <: MappingAt[O], A <: Label](source :Relation[T], alias :A) :NamedRelation[A, (T As A)#T] =
		new AliasedRelation(source, alias)






	/** Extension methods for `FromClause` classes which benefit from having a static, invariant self type. */
	implicit class FromClauseExtension[F <: FromClause](val clause :F) extends AnyVal {

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def entities :JoinedEntities[F] = new JoinedEntities[F](clause)

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `JoinedTable` SQL expressions parameterized with any of the mapping types listed by this clause
		  * and a supertype of this clause as the base for the expression.
		  */
		@inline def relations :JoinedRelations[F] = new JoinedRelations[F](clause)


		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'implicit' portion is a prefix clause of its `Self` type (that is,
		  * its complete type with all join kinds known). For non-subselect clauses, it is the number of all relations.
		  */
		@inline def subselectSuffix :clause.Outer PrefixOf clause.Self =
			new PrefixOf[clause.Outer, clause.Self](clause.subselectSize)

		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'implicit' base clause, constituting of all relations listed by
		  * enclosing selects, is a prefix of the `Generalized` type of this clause. For non-subselect clauses,
		  * it represents the full `Generalized` type size (that is, all relations in the clause).
		  */
		@inline def generalizedSubselectSuffix :clause.Implicit PrefixOf clause.Generalized =
			new PrefixOf[clause.Implicit, clause.Generalized](clause.subselectSize)

		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'Implicit' type is the generalization of the prefix clause
		  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
		  * it represents the full type `F`, with all its relations.
		  */
		@inline def subselectSpan :clause.Implicit ExtendedBy F =
			new ExtendedBy(clause.subselectSize)



		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side. The real type of the result depends on the type of this clause:
		  * for `Dual`, the result is `From[R]`, for non-empty clauses the result is `F InnerJoin R`.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom#where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @return `From[R]` if this clause is empty or `F InnerJoin R` otherwise.
		  */
		@inline def andFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		            (table :Relation[R])
		            (implicit cast :JoinedRelationSubject[AndFrom.WithLeft[F]#F, R, T, MappingOf[S]#TypedProjection])
				:F AndFrom R =
			AndFrom(clause, table)

		/** Performs a join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. The join type between the clauses will be an `InnerJoin` if the dynamic
		  * type of the first join of the given clause is `From[_]`, otherwise the join type is preserved.
		  * If either of the clauses is empty, the other is returned unchanged.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def andFrom[R <: FromClause](other :R) :other.AppendedTo[F] = other.appendedTo(clause)

	}



	/** Extension methods for `FromSome` classes (non-empty ''from'' clauses) which benefit from having a static,
	  * invariant self type. Most notably, this includes methods for joining it with other relations.
	  */
	implicit class FromSomeExtension[F <: FromSome](val clause :F) extends AnyVal {

		/** Performs an inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom#where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[R])(implicit cast :InferSubject[F, InnerJoin, R, T, S]) :F InnerJoin R =
			InnerJoin(clause, table)

		/** Performs an inner join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def join[R <: FromSome](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(clause, InnerJoin.template)



		/** Performs a symmetric outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom#where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, OuterJoin, R, T, S]) :F OuterJoin R =
			OuterJoin(clause, table)

		/** Performs a symmetric outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def outerJoin[R <: FromSome](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(clause, OuterJoin.template)



		/** Performs a left outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom#where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                    (table :Relation[R])
		                    (implicit cast :InferSubject[F, LeftJoin, R, T, S]) :F LeftJoin R =
			LeftJoin(clause, table)

		/** Performs a left outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def leftJoin[R <: FromSome](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(clause, LeftJoin.template)



		/** Performs a right outer join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on()]],
		  * [[net.noresttherein.oldsql.sql.AndFrom#where where()]] or
		  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, RightJoin, R, T, S]) :F RightJoin R =
			RightJoin(clause, table)

		/** Performs a right outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side, unless the first join in `other` is a `JoinParam` (or any other type different
		  * than `From`), in which case that join type is preserved, with this clause replacing `Dual` in `other`.
		  * Both the dynamic and static types of the joins from the parameter clause are preserved,
		  * as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def rightJoin[R <: FromSome](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(clause, RightJoin.template)



		/** Performs a natural inner join between this clause on the left side, and the relation given as a `Relation`
		  * object on the right side. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                       (table :Relation[R])
		                       (implicit cast :InferSubject[clause.type, InnerJoin, R, T, S],
		                        last :ByNegativeIndex[clause.Generalized, -1]) :F InnerJoin R =
			cast(InnerJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural symmetric outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalOuterJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Relation[R])
		                            (implicit cast :InferSubject[clause.type, OuterJoin, R, T, S],
		                             last :ByNegativeIndex[clause.Generalized, -1]) :F OuterJoin R =
			cast(OuterJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural left outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalLeftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                           (table :Relation[R])
		                           (implicit cast :InferSubject[clause.type, LeftJoin, R, T, S],
		                            last :ByNegativeIndex[clause.Generalized, -1]) :F LeftJoin R =
			cast(LeftJoin[clause.type, T, T, S](clause, cast(table)) where naturalFilter[T] _)


		/** Performs a natural right outer join between this clause on the left side, and the relation given
		  * as a `Relation` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalRightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :Relation[R])
		                            (implicit cast :InferSubject[clause.type, RightJoin, R, T, S],
		                             last :ByNegativeIndex[clause.Generalized, -1]) :F RightJoin R =
			cast(RightJoin[clause.type, T, T, S](clause, cast(table))(cast.self) where naturalFilter[T] _)


		private def naturalFilter[T[O] <: BaseMapping[_, O]]
		                         (tables :JoinedEntities[clause.Generalized TrueJoin T])
		                         (implicit prev :ByNegativeIndex[clause.Generalized TrueJoin T, -2])
				:SQLBoolean[clause.Generalized TrueJoin T] =
		{
			val firstTable = tables.prev
			val secondTable = tables.last

			val firstColumns = firstTable.columns.map(c => c.name -> c).toMap //todo: below - nondeterministic compilation
			val secondColumns = secondTable.columns.map(c => c.name -> (c :ColumnMapping[_, FromClause AndFrom T])).toMap
			val common = firstColumns.keySet & secondColumns.keySet

			val joins = common map { name =>

				val first = firstColumns(name).asInstanceOf[ColumnMapping[Any, prev.G]]
				val second = secondColumns(name).asInstanceOf[ColumnMapping[Any, FromClause AndFrom T]]
				if (first.form != second.form)
					throw new IllegalArgumentException(s"Can't perform a natural join of $firstTable and $secondTable: " +
							s"columns $first and $second have different types (forms): ${first.form} and ${second.form}.")

				FreeColumn(first, 1) === FreeColumn(second, 0)
			}
			(True[clause.Generalized TrueJoin T]() /: joins)(_ && _)
		}



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Relation` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.AndFrom#on on()]], [[net.noresttherein.oldsql.sql.AndFrom#where where()]]
		  * and [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] methods.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselect[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :Relation[R])
		                     (implicit cast :InferSubject[F, Subselect, R, T, S]) :F Subselect R =
			Subselect(clause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using on clause.
		  * The explicit list of relations in the clause is initialized with the relations given as a `FromClause`
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relations - following the `Subselect`
		  * pseudo join and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition and the ''where'' clause filter can be subsequently specified using one of
		  * the [[net.noresttherein.oldsql.sql.AndFrom#on on()]], [[net.noresttherein.oldsql.sql.AndFrom#where where()]]
		  * and [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast()]] methods.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @throws UnsupportedOperationException if `other` is empty or its first join is a `JoinParam`.
		  */
		@inline def subselect[R <: FromSome](other :R) :other.JoinedWith[F, Subselect] =
			other.joinedAsSubselect(clause)



		/** Applies this clause to its parameters: removes all `JoinParam` joins and substitutes all references
		  * to parameter components with SQL literals extracted from parameter values.
		  * @param params a chain consisting of subject types of all parameter mappings of all `JoinParam` joins
		  *               in this clause in order of their appearance.
		  * @tparam U `FromClause` subtype obtained by removing all `JoinParam` instances from this clause's type.
		  */
		def apply[U <: ParameterlessFrom](params :clause.Params)
		                                 (implicit apply :ApplyJoinParams[F, clause.Params, U]) :U =
			apply(clause, params)

	}






	/** Extension methods for `OuterFrom` objects (''from'' clauses without any `Subselect`s which can serve
	  * as the basis for independent selects). It provides methods for introducing unbound parameters
	  * to the clause in the form of [[net.noresttherein.oldsql.sql.JoinParam JoinParam]] 'joins',
	  * which can be substituted with
	  */
	implicit class OuterFromExtension[F <: OuterFrom](private val clause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
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
		  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
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
		  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
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
	  */ //it is important that clause remains private as it may be not fully initialized
	implicit class JoinedEntities[F <: FromClause](private val clause :F) extends AnyVal {

		/** Returns the `Mapping` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject[F, E]) :get.T[get.G] = get(clause).mapping

		/** Returns the `Mapping` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.Join.as Join.as]]
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



		//todo: ByParamIndex
		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor[F, ParamRelation[X]#Param]) :get.T[get.G] =
			get(clause).mapping

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `JoinParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, N]) :get.T[get.G] =
			get.apply(clause).mapping



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
		  * for empty 'outer' clauses, creating a `From[T]` clause, forgoing the `Subselect` join in the result.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[M])
		                (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
//		                (implicit cast :InferSubject[clause.type, Subselect, M, T, S]) :F#Extend[Subselect, M] =
				:F#Extend[Subselect, T] =
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
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @return `other.JoinedWith[F, Subselect]` if `F` is not empty and `R` otherwise.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  */
		@inline def from[R <: FromSome with OuterFrom](other :R) :F#JoinWith[Subselect, R] = clause.from(other)

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
	  */ //it is important that clause remains private as it may be not fully initialized
	class JoinedRelations[F <: FromClause](private val clause :F) extends AnyVal {

		@inline def entities :JoinedEntities[F] = new JoinedEntities[F](clause)

		/** Returns the `JoinedRelation` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject[F, E]) :JoinedRelation[get.G, get.T] = get(clause)

		/** Returns the `JoinedRelation` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByTypeConstructor[F, M]) :JoinedRelation[get.G, M] =
			get(clause)

		/** Returns the `JoinedRelation` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.Join.as Join.as]]
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



		//todo: ByParamIndex
		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor[F, ParamRelation[X]#Param]) :JoinedRelation[get.G, get.T] =
			get(clause)

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `JoinParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamRelation[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName[F, N]) :JoinedRelation[get.G, get.T] =
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
		  * for empty 'outer' clauses, creating a `From[T]` clause, forgoing the `Subselect` join in the result.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `M`.
		  * @return `F Subselect T` if `F` is not empty and `From[T]` otherwise.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :Relation[M])
		                (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
//		                (implicit cast :InferSubject[clause.type, Subselect, M, T, S]) :F#Extend[Subselect, M] =
				:F#Extend[Subselect, T] =
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
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  * @return `other.JoinedWith[F, Subselect]` if `F` is not empty and `R` otherwise.
		  * @throws UnsupportedOperationException if the first join in `other` is a `JoinParam`.
		  */
		@inline def from[R <: FromSome with OuterFrom](other :R) :F#JoinWith[Subselect, R] = clause.from(other)

	}






	@implicitNotFound("Can't calculate the size of the clause ${F}. Either the FROM clause is incomplete " +
		              "or the expected number ${N} is incorrect. Missing implicit: FromClauseSize[${F}, ${N}].")
	class FromClauseSize[-F <: FromClause, N <: Numeral] private(private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object FromClauseSize {
		implicit val DualCount :FromClauseSize[Dual, 0] = new FromClauseSize[Dual, 0](0)

		implicit def joined[L <: FromClause, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                   (implicit count :FromClauseSize[L, M], plus :Inc[M, N]) :FromClauseSize[L AndFrom R, N] =
			new FromClauseSize[L AndFrom R, N](plus.n)
		
		implicit def decorated[F <: FromClause, N <: Numeral](implicit count :FromClauseSize[F, N])
				:FromClauseSize[DecoratedFrom[F], N] =
			new FromClauseSize(count.tables)
	}



	@implicitNotFound("Failed to count the tables in the clause ${F}. Is ${N} the number of mappings listed " +
	                  "in its definition? Note that witness TableCount[F, N] is invariant in type F, " +
	                  "but requires that it starts with either FromClause or Dual (including From[_]).")
	class TableCount[F <: FromClause, N <: Numeral] private[FromClause] (private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object TableCount {
		implicit final val fromClauseHasZero :TableCount[FromClause, 0] = new TableCount[FromClause, 0](0)
		implicit final val dualHasZero :TableCount[Dual, 0] = new TableCount[Dual, 0](0)

		implicit def joined[F <: FromClause, L <: FromClause, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                   (implicit split :Conforms[F, F, L AndFrom R], count :TableCount[L, M], inc :Inc[M, N])
				:TableCount[F, N] =
			new TableCount[F, N](inc.n)
		
		implicit def decorated[D <: DecoratedFrom[F], F <: FromClause, N <: Numeral]
		                      (implicit deconstruct :Conforms[D, D, DecoratedFrom[F]], count :TableCount[F, N])
				:TableCount[D, N] =
			new TableCount(count.tables)
	}



	@implicitNotFound("Relation mapping ${M} is not the first known mapping of the FROM clause ${F}: "+
	                  "no implicit TableShift[${F}, ${M}, ${N}].")
	class TableShift[F <: FromClause, M[O] <: MappingAt[O], N <: Numeral](private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object TableShift {
		implicit def firstRelation[F <: FromClause, R[A] <: MappingAt[A]]
		                          (implicit split :Conforms[F, F, FromClause AndFrom R]) :TableShift[F, R, 0] =
			new TableShift[F, R, 0](0)

		implicit def joinedEarlier[F <: FromClause, L <: FromClause, R[A] <: MappingAt[A],
		                           T[A] <: MappingAt[A], M <: Numeral, N <: Numeral]
		                          (implicit split :Conforms[F, F, L AndFrom R], prev :TableShift[L, T, M], inc :Inc[M, N])
				:TableShift[F, T, N] =
			new TableShift[F, T, N](inc.n)
		
		implicit def decorated[D <: DecoratedFrom[F], F <: FromClause, T[O] <: MappingAt[O], N <: Numeral]
		                      (implicit deconstruct :Conforms[D, D, DecoratedFrom[F]], prev :TableShift[F, T, N])
				:TableShift[D, T, N] =
			new TableShift(prev.tables)
	}






	/** Namespace containing implicit witnesses for the presence of a certain relation in a given `FromClause`.
	  * They are used to access joined relations by subject, index, label, etc.
	  */
	object GetTable {

		/** Implicit resolution of the `N`-th relation in the ''from'' clause `F`. This works both for positive numbers,
		  * indexed from zero and going from left to right (in which case `F` must be complete), and negative -
		  * indexed from `-1` and going from right to left (which is available always, but which index changes with
		  * joining new tables).
		  * @tparam F the input `FromClause`.
		  * @tparam N index of the desired relation as a literal `Int` type.
		  */
		@implicitNotFound("Cannot get ${N}-th relation of the FROM clause ${F}. \n" +
			              "Either ${N} >= size (where size is the number of relations in the FROM clause),\n" +
			              "or -(${N}) is greater the number of relations in the instantiated suffix of the FROM clause,\n" +
			              "or ${N} >= 0 and the size is not known (the clause type starts with FromClause and not Dual/From.")
		sealed abstract class ByIndex[-F <: FromClause, N <: Numeral] {
			/** The ''negative'' index of the found relation: that's `-1` for the last relation in the clause
			  * and decreases going left. */
			type I <: Numeral

			/** The supertype of the ''from'' clause `F`, in which the search takes place, resulting from replacing the
			  * join having the relation at the given index as its right side with `FromClause AndFrom T`. */
			type G >: F <: FromClause

			/** The mapping type of the relation at index `N`. */
			type T[O] <: MappingAt[O]

			val stretch :FromClause AndFrom T PrefixOf G

			implicit def count :TableCount[G, _ <: Numeral] = new TableCount[G, stretch.suffix.type](stretch.suffix)


			/** Getter for the relation from the input `FromClause`. */
			def apply(from :F) :JoinedRelation[G, T] = table(from).extend(stretch)

			def table(from :F) :JoinedRelation[FromClause AndFrom T, T]
		}



		object ByIndex {

			@implicitNotFound("Can't get the relation at the (negative) index ${N} in ${F}. " +
				              "Either ${N} >= 0 or -(${N}) is greater than the number of known tables in the FROM clause.")
			sealed abstract class ByNegativeIndex[-F <: FromSome, N <: Numeral] extends ByIndex[F, N] {
				override type I = N
				override type G >: F <: FromSome
			}


			implicit def lastNegative[M[O] <: MappingAt[O]]
					:ByNegativeIndex[FromClause AndFrom M, -1] { type T[O] = M[O]; type G = FromClause AndFrom M } =
				new ByNegativeIndex[FromClause AndFrom M, -1] {
					override type T[O] = M[O]
					override type G = FromClause AndFrom M

					override val stretch = implicitly[G PrefixOf G]

					override def table(from :FromClause AndFrom M) =
						from.last
				}

			implicit def previousNegative[J[+F <: FromSome, T[O] <: MappingAt[O]] <: F AndFrom T,
			                              L <: FromSome, R[O] <: MappingAt[O], N <: Numeral, M <: Numeral]
			                             (implicit minus :Inc[N, M], get :ByNegativeIndex[L, M])
					:ByNegativeIndex[L J R, N] { type T[O] = get.T[O]; type G = get.G J R } =
				new ByNegativeIndex[L J R, N] {
					override type T[O] = get.T[O]
					override type G = get.G J R

					override val stretch = get.stretch.stretch[G]

					override def table(from :L J R) =
						get.table(from.left)
				}

			implicit def decoratedNegative[F <: D[C], D[+B <: FromSome] <: FromSomeDecorator[B], C <: FromSome, 
			                               N <: Numeral]
			                              (implicit upcast :DecoratorUpcasting[F, D, C], get :ByNegativeIndex[C, N])
					:ByNegativeIndex[F, N] { type T[O] = get.T[O]; type G = D[get.G] } =
				new ByNegativeIndex[F, N] {
					override type T[O] = get.T[O]
					override type G = D[get.G]

					override val stretch = get.stretch.wrap[D]

					override def table(from :F) =
						get.table(from.clause)
				}



			@implicitNotFound("Can't get relation at the (non-negative) index ${N} in ${F}. Either ${N} < 0, " +
			                  "or ${N} is greater or equal than the number of relations in the FROM clause, " +
			                  "or the exact number of relations in the FROM clause is not known.")
			sealed abstract class ByPositiveIndex[-F <: FromSome, N <: Numeral, M <: Numeral]
				extends ByIndex[F, N]
			{
				type I = M
				override type G >: F <: FromSome
			}


			implicit def lastPositive[L <: FromClause, R[O] <: MappingAt[O], N <: Numeral]
			                         (implicit count :FromClauseSize[L, N])
					:ByPositiveIndex[L AndFrom R, N, -1] { type T[O] = R[O]; type G = FromClause AndFrom R } =
				new ByPositiveIndex[L AndFrom R, N, -1] {
					override type T[O] = R[O]
					override type G = FromClause AndFrom R

					override val stretch = implicitly[G PrefixOf G]

					override def table(from :L AndFrom R) = from.last
				}

			implicit def previousPositive[J[+F <: FromSome, T[O] <: MappingAt[O]] <: F AndFrom T,
			                              L <: FromSome, R[O] <: MappingAt[O], N <: Numeral, X <: Numeral, Y <: Numeral]
			                             (implicit get :ByPositiveIndex[L, N, Y], minus :Inc[X, Y])
					:ByPositiveIndex[L J R, N, X] { type T[O] = get.T[O]; type G = get.G J R } =
				new ByPositiveIndex[L J R, N, X] {
					override type T[O] = get.T[O]
					override type G = get.G J R

					override val stretch = get.stretch.stretch[G]

					override def table(from :L J R) =
						get.table(from.left)
				}

			implicit def decoratedPositive[F <: D[C], D[+B <: FromSome] <: FromSomeDecorator[B], C <: FromSome, 
			                               N <: Numeral, I <: Numeral]
			                              (implicit upcast :DecoratorUpcasting[F, D, C], get :ByPositiveIndex[C, N, I])
					:ByPositiveIndex[F, N, I] { type T[O] = get.T[O]; type G = D[get.G] } =
				new ByPositiveIndex[F, N, I] {
					override type T[O] = get.T[O]
					override type G = D[get.G]

					override val stretch = get.stretch.wrap[D]

					override def table(from :F) =
						get.table(from.clause)
				}

		}






		/** Implicit witness accessing the last relation in the ''from'' clause `F` with alias `A`.
		  * It is defined as the last relation of the clause `L`, such that
		  * `L` [[net.noresttherein.oldsql.sql.DecoratedFrom.Alias Alias]] `A` appears as a part of type `F` and
		  * is the right-most such occurrence.
		  */
		@implicitNotFound("No relation with alias ${A} appears in the clause ${F}.")
		sealed abstract class ByAlias[-F <: FromClause, A <: Label] {
			type T[O] <: MappingAt[O]
			type G >: F <: FromClause

			def stretch :FromClause AndFrom T PrefixOf G

			@inline def apply(from :F) :JoinedRelation[G, T] = table(from).extend(stretch)

			def table(from :F) :JoinedRelation[FromClause AndFrom T, T]
		}

		
		
		object ByAlias {

			sealed abstract class Get[-F <: FromSome, A <: Label] extends ByAlias[F, A] {
				override type G >: F <: FromSome
			}

			implicit def last[M[O] <: MappingAt[O], A <: Label] :Get[FromClause AndFrom M Alias A, A]
					{ type T[O] = M[O]; type G = FromClause AndFrom M Alias A } =
				new Get[FromClause AndFrom M Alias A, A] {
					override type T[O] = M[O]
					override type G = FromClause AndFrom M Alias A

					override val stretch = PrefixOf[FromClause AndFrom M, FromClause AndFrom M Alias A]

					override def table(from :FromClause AndFrom M Alias A) =
						from.clause.last
				}

			implicit def previous[J[+F <: FromSome, T[O] <: MappingAt[O]] <: F AndFrom T,
			                      L <: FromSome, R[O] <: MappingAt[O], A <: Label]
			                     (implicit get :Get[L, A]) :Get[L J R, A] { type T[O] = get.T[O]; type G = get.G J R } =
				new Get[L J R, A] {
					override type T[O] = get.T[O]
					override type G = get.G J R

					override val stretch = get.stretch.stretch[G]

					override def table(from :L J R) =
						get.table(from.left)
				}

			implicit def decorated[F <: D[C], D[+B <: FromSome] <: FromSomeDecorator[B], C <: FromSome, A <: Label]
			                      (implicit deconstruct :DecoratorUpcasting[F, D, C], get :Get[C, A])
					:Get[F, A] { type T[O] = get.T[O]; type G = D[get.G] } =
				new Get[F, A] {
					override type T[O] = get.T[O]
					override type G = D[get.G]

					override val stretch = get.stretch.stretch[G]

					override def table(from :F) =
						get.table(from.clause)
				}

		}






		/** Implicit resolution of retrieving a relation from a ''from'' clause where the `Mapping` type satisfies
		  * some predicate. The search starts with the last (rightmost) relation in the clause and goes backward,
		  * looking for the first relation with an implicit `Predicate[M, X]`.
		  */
		class GetTableByPredicate {

			/** Witnesses that mapping `M[Any]` (where `Any` is the `Origin` type) satisfies the search predicate
			  * and should be returned. Note that the predicate is contravariant with the regards to the mapping type,
			  * so all subtypes of a mapping `M[_]` satisfying the predicate satisfy it, too.
			  * @tparam M the type constructor of the mapping of the relation, accepting an arbitrary `Origin` type parameter.
			  * @tparam X the input key of the search: the type provided by the accessor, such as a label for a `LabeledMapping`.
			  */
			@implicitNotFound("Mapping ${M} does not match the key type ${X}.")
			final class Predicate[-M[A] <: MappingAt[A], X]

			private[this] final val predicate = new Predicate[MappingAt, Any]

			/** Subclasses can use this method as the implicit */
			protected def report[M[A] <: MappingAt[A], X] :Predicate[M, X] =
				predicate.asInstanceOf[Predicate[M, X]]



			/** Implicit resolution of search for a mapping `M` in the `FromClause` `F` satisfying a `Predicate[M, X]`
			  * (that is, an `M` for which such an implicit value exists. The type of the mapping of the found relation
			  * is returned as the member type `T[O]`. In other words, an implicit value `found :Found[F, X, I]`
			  * witnesses that `found.T` is the mapping of the last relation (rightmost) in the clause `F` for which
			  * an implicit `Predicate[T, X]` exists, with `I` being the ''negative'' index of the mapping
			  * (starting with `-1` for the last mapping and decreasing).
			  */
			@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}.")
			sealed abstract class Found[-F <: FromSome, X, I <: Numeral] {
				/** The accessed `Mapping` type. */
				type T[O] <: MappingAt[O]

				/** The supertype of the ''from'' clause `F`, in which the search takes place, resulting from replacing
				  * the join having the found relation as its right side with `FromClause AndFrom T`. */
				type G >: F <: FromSome

				/** The negative index of the relation. */
				def shift :I

				val stretch :FromClause AndFrom T PrefixOf G

				/** Getter for the matching relation. */
				def apply(from :F) :JoinedRelation[G, T] = table(from).extend(stretch)

				def table(from :F) :JoinedRelation[FromClause AndFrom T, T]
			}

			implicit def last[M[O] <: MappingAt[O], X](implicit pred :Predicate[M, X])
					:Found[FromClause AndFrom M, X, -1] { type T[O] = M[O]; type G = FromClause AndFrom M } =
				new Found[FromClause AndFrom M, X, -1] {
					override type T[O] = M[O]
					override type G = FromClause AndFrom M

					override val stretch = implicitly[G PrefixOf G]
					override def shift = -1 : -1

					override def table(from :FromClause AndFrom M) =
						from.last
				}

			implicit def previous[J[+F <: FromSome, T[O] <: MappingAt[O]] <: F AndFrom T,
			                      L <: FromSome, R[O] <: MappingAt[O], K, X <: Numeral, Y <: Numeral]
			                     (implicit get :Found[L, K, Y], minus :Inc[X, Y])
					:Found[L J R, K, X] { type T[O] = get.T[O]; type G = get.G J R } =
				new Found[L J R, K, X] {
					override type T[O] = get.T[O]
					override type G = get.G J R

					override val stretch = get.stretch.stretch[G]
					override val shift = minus.m

					override def table(from :L J R) =
						get.table(from.left)
				}

			implicit def decorated[F <: D[C], D[+B <: FromSome] <: FromSomeDecorator[B], C <: FromSome, K, X <: Numeral]
			                      (implicit upcast :DecoratorUpcasting[F, D, C], get :Found[C, K, X])
					:Found[F, K, X] { type T[O] = get.T[O]; type G = D[get.G] } =
				new Found[F, K, X] {
					override type T[O] = get.T[O]
					override type G = D[get.G]

					override val stretch = get.stretch.wrap[D]
					override def shift = get.shift

					override def table(from :F) =
						get.table(from.clause)
				}



			/** An implicit result of the resolution of search for a relation matching the key type `X`
			  * in the ''from'' clause `F`.
			  * @tparam F the input `FromClause` from which the relation is taken.
			  * @tparam X the 'key' type used to match the `Mapping` types of all relations in search for an implicit
			  *           `Predicate[M[Any], X]`.
			  */
			abstract class Get[-F <: FromClause, X] {
				/* Derived from some implicit `Found[F, X, I]`. This duplication
			     * is required because the index `I` needs to be a type parameter for the type-level arithmetic to work
			     * and the accessor methods accepting `Get` do not declare 'output' types. Additionally, different
			     * accessors with the same signature but for different `GetTableByPredicate` instance will lead to
			     * conflicts after erasure, so subclasses introduce their own `Get` subclass to prevent it.
				 */

				/** The ''negative'' index of the accessed relation, starting with `-1` for the rightmost relation in `F`.*/
				type I <: Numeral

				/** The last mapping type matching `X` in `F`. */
				type T[O] <: MappingAt[O]

				/** A supertype of the ''from'' clause resulting from upcasting the `_ J T` (for some join type `J`)
				  * fragment to `FromClause AndFrom T`, where `T` is the returned table. Anything to the right of the
				  * returened table in the type signature is left unchanged.
				  */
				type G >: F <: FromClause

				/** The negative index of the found relation, starting with `-1` and decreasing from right to left. */
				def shift :I

				/** Returns the found relation from the input `FromClause`. */
				def apply(from :F) :JoinedRelation[G, T]
			}

		}






		/** An implicit accessor object for the last relation in `F` with `Subject` type `S`.
		  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
		@implicitNotFound("No relation with Subject type ${S} in the FROM clause ${F}:\n" +
		                  "no implicit value for BySubject[${F}, ${S}].")
		sealed abstract class BySubject[-F <: FromClause, S] extends BySubject.Get[F, S]

		/** Accesses relations in a `FromClause` based on their `Subject` member type. */
		object BySubject extends GetTableByPredicate {
			implicit def satisfies[M[A] <: RefinedMapping[S, A], S] :Predicate[M, S] = report[M, S]

			implicit def Get[F <: FromSome, S, N <: Numeral]
			                (implicit found :Found[F, S, N] { type T[O] <: RefinedMapping[S, O] })
					:BySubject[F, S] { type T[O] = found.T[O]; type G = found.G; type I = N } =
				new BySubject[F, S] {
					override type I = N
					override type G = found.G
					override type T[O] = found.T[O]

					override def shift = found.shift
					override def apply(from :F) = found(from)
				}

		}



		/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledMapping[N, _, _]`. */
		@implicitNotFound("No relation with alias type ${N} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByLabel[${F}, ${N}].")
		sealed abstract class ByLabel[-F <: FromClause, N <: Label] extends ByLabel.Get[F, N] {
			override type T[O] <: LabeledMapping[N, _, O]
		}

		/** Accesses relations in a `FromClause` based on their `Label` (that is, the `Label` type parameter
		  * of the `LabeledMapping` used for the relation. Implicit ByLabel.Get[F, L] returns the last relation
		  * with label `L` in `F`. */
		object ByLabel extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledMapping[L, _, A], L <: Label] :Predicate[M, L] = report[M, L]

			implicit def Get[F <: FromSome, A <: Label, N <: Numeral]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledMapping[A, _, O] })
					:ByLabel[F, A] { type T[O] = found.T[O]; type G = found.G; type I = N } =
				new ByLabel[F, A] {
					override type I = N
					override type T[O] = found.T[O]
					override type G = found.G

					override def shift = found.shift
					override def apply(from :F) = found(from)
				}
		}






		/** An implicit accessor object for the last relation in `F` with `Mapping` type `M[_]`.
		  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
		@implicitNotFound("No relation with type constructor ${M} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByTypeConstructor[${F}, ${M}].")
		sealed abstract class ByTypeConstructor[-F <: FromClause, M[O] <: MappingAt[O]] {
			type I <: Numeral
			/** A unique origin type with the negative index of the relation encoded in it. */
			type T[O] = M[O]
			type G >: F <: FromClause

			def apply(from :F) :JoinedRelation[G, M]
			def shift :I
		}

		/** Accesses relations in a `FromClause` based on the type constructor for the mapping accepting
		  * the `Origin` type. The provided type constructor may be for a supertype of the actual mapping.
		  * While the input to match accepts only a single parameter, it is possible to match mappings with multiple
		  * parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
		  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`. */
		object ByTypeConstructor extends GetTableByPredicate {
			implicit def satisfies[M[A] <: MappingAt[A]] :Predicate[M, M[Any]] = report[M, M[Any]]

			implicit def Get[F <: FromSome, M[O] <: MappingAt[O], N <: Numeral]
			                (implicit found :Found[F, M[Any], N] { type T[O] = M[O] })
					:ByTypeConstructor[F, M] { type G = found.G; type I = N } =
				new ByTypeConstructor[F, M] {
					override type I = N
					override type G = found.G
					override def shift = found.shift
					override def apply(from :F) = found(from)
				}

		}






		/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledFromParam[A, _, _]`. */
		@implicitNotFound("No parameter with name type ${A} in the FROM clause ${F}:\n" +
		                  "no implicit value for ByParamName[${F}, ${A}].")
		sealed abstract class ByParamName[-F <: FromClause, A <: Label] extends ByParamName.Get[F, A] {
			type T[O] <: LabeledFromParam[A, _, O]
		}

		/** Accesses parameters of a `FromClause` based on the given parameter name used as their label.
		  * Implicit ByParamName.Get[F, N] returns the last relation for the synthetic parameter mapping
		  * `LabeledFromParam[N, X, O]` in `F`.
		  */
		object ByParamName extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledFromParam[N, _, A], N <: Label] :Predicate[M, N] = report[M, N]

			implicit def Get[F <: FromSome, A <: Label, N <: Numeral]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledFromParam[A, _, O] })
					:ByParamName[F, A] { type T[O] = found.T[O]; type G = found.G; type I = N } =
				new ByParamName[F, A] {
					override type I = N
					override type T[O] = found.T[O]
					override type G = found.G

					override def shift = found.shift
					override def apply(from :F) = found(from)
				}

		}

	}






	@implicitNotFound("Cannot apply the FROM clause ${F}\nto parameters ${P}. I cannot prove that the parameter chain P " +
	                  "is a subtype of F#Params - most likely F is incomplete or contains AndFrom joins.\n"+
	                  "Missing implicit value ApplyJoinParams[${F}, ${P}, ${U}].")
	sealed abstract class ApplyJoinParams[-F <: FromClause, -P <: Chain, +U <: ParameterlessFrom] {
		type lastWasParam <: Boolean with Singleton
		def apply(from :F, params :P) :U
	}



	object ApplyJoinParams {
		private[this] val none = new ApplyJoinParams[ParameterlessFrom, @~, ParameterlessFrom] {
			override def apply(from :ParameterlessFrom, params: @~) = from
		}

		implicit val applyToDual :ApplyJoinParams[Dual, @~, Dual] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[Dual, @~, Dual] { type lastWasParam = false }]

		implicit def unparameterized[F <: FromSome with ParameterlessFrom] :ApplyJoinParams[F, @~, F] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[F, @~, F] { type lastWasParam = false }]

		implicit def nonParamFrom[T[A] <: MappingAt[A]] :ApplyJoinParams[From[T], @~, From[T]] { type lastWasParam = false } =
			none.asInstanceOf[ApplyJoinParams[From[T], @~, From[T]] { type lastWasParam = false }]

		implicit def nonParamEmptyJoin[J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B,
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

		implicit def nonParamJoin[J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B,
		                          L <: FromSome, R[A] <: MappingAt[A], P <: _ ~ _, U <: FromSome with ParameterlessFrom]
		                         (implicit prev :ApplyJoinParams[L, P, U])
				:ApplyJoinParams[L J R, P, J[L, R]#WithLeft[U]] { type lastWasParam = false } =
			new ApplyJoinParams[L J R, P, (L J R)#WithLeft[U]] {
				override type lastWasParam = false

				override def apply(from :J[L, R], params :P) = {
					val left = prev(from.left, params)
					val unfiltered = from.withLeft[left.type](left)(True)
					val generalized = from.generalized
					val ps = params.asInstanceOf[generalized.Params]
					val substitute = new RemoveParams(generalized, unfiltered.generalized)(ps)
					from.withLeft(left)(substitute(from.condition))
				}
			}

		implicit def nonParamDecorator[D <: GenericDecorator[F], F <: FromSome,
		                               P <: _ ~ _, U <: FromSome with ParameterlessFrom]
		                              (implicit deconstruct :Conforms[D, D, GenericDecorator[F]],
		                                        prev :ApplyJoinParams[F, P, U] { type lastWasParam = false })
				:ApplyJoinParams[D, P, D#WithClause[U]] { type lastWasParam = false } =
			new ApplyJoinParams[D, P, D#WithClause[U]] {
				override type lastWasParam = false

				override def apply(from :D, params :P) :D#WithClause[U] =
					from.withClause(prev(from.clause, params))
			}


		implicit def applyParam[L <: FromClause, R[A] <: FromParam[X, A], X, I <: Chain, U <: ParameterlessFrom]
		                       (implicit prev :ApplyJoinParams[L, I, U])
				:ApplyJoinParams[L JoinParam R, I ~ X, U] { type lastWasParam = true } =
			new ApplyJoinParams[L JoinParam R, I ~ X, U] {
				override type lastWasParam = true

				override def apply(from :JoinParam[L, R], params :I ~ X) = {
					val res = prev(from.left, params.init)
					val generalized = from.generalized
					val substitute = new RemoveParams(generalized, res.generalized)(params.asInstanceOf[generalized.Params])
					res.where(substitute(from.condition)).asInstanceOf[U]
				}
			}

		implicit def paramDecorator[F <: FromSome, P <: _ ~ _, U <: ParameterlessFrom]
		                           (implicit prev :ApplyJoinParams[F, P, U] { type lastWasParam = true })
				:ApplyJoinParams[DecoratedFrom[F], P, U] { type lastWasParam = true } =
			new ApplyJoinParams[DecoratedFrom[F], P, U] {
				override type lastWasParam = true

				override def apply(from :DecoratedFrom[F], params :P) = prev(from.clause, params)
			}

	}






	/** Proof that the ''from'' clause `E` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `E`. It means that `E &lt;: F AndFrom T1 ... AndFrom TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `E` containing all the relations of `F` unless `F` is complete.
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
		@inline def stretch[R[O] <: MappingAt[O]] :F ExtendedBy (E AndFrom R) = new ExtendedBy(length + 1)

		/** A transitive proof that a clause extended by `F` with a single relation(mapping) is also extended by `E`. */
		@inline def stretchFront[L <: FromClause, R[O] <: MappingAt[O]]
		                        (implicit front :F <:< (L AndFrom R)) :L ExtendedBy E =
			new ExtendedBy(length + 1)

		@inline def stretchFront[C <: FromClause](implicit front :C ExtendedBy F) :C ExtendedBy E =
			new ExtendedBy(front.length + length)

		@inline def stretch[C <: FromClause](implicit next :E ExtendedBy C) :F ExtendedBy C =
			new ExtendedBy[F, C](length + next.length)

		@inline def shrink[U >: F <: FromClause, C <: FromClause, T <: E]
		                  (implicit prefix :U ExtendedBy T, suffix :C ExtendedBy T) :F ExtendedBy C =
			new ExtendedBy[F, C](length - suffix.length)

		@inline def shrinkFront[U >: F <: FromClause, C <: FromClause, T <: E]
		                       (implicit prefix :U ExtendedBy C, suffix :C ExtendedBy T) :C ExtendedBy T =
			new ExtendedBy(length - prefix.length)

		@inline def unwrapFront[C <: FromSome](implicit front :F <:< DecoratedFrom[C]) :C ExtendedBy E =
			new ExtendedBy(length)
	}



	object ExtendedBy {
		@inline def apply[F <: FromClause, E <: FromClause](implicit ev :F ExtendedBy E) :F ExtendedBy E = ev

		private[this] val instance = new ExtendedBy[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def extend[F <: FromClause, L <: FromClause, R[A] <: MappingAt[A]]
		                   (implicit ev :F ExtendedBy L) :F ExtendedBy (L AndFrom R) =
			new ExtendedBy(ev.length + 1)

		implicit def wrap[F <: FromClause, E <: FromClause](implicit ev :F ExtendedBy E) :F ExtendedBy DecoratedFrom[E] =
			new ExtendedBy[F, DecoratedFrom[E]](ev.length)
	}






	/** Proof that the ''from'' clause `E` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `E`. It means that `E =:= F AndFrom T1 ... AndFrom TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `E` containing all the relations of `F` unless `F` is complete.
	  */
	@implicitNotFound("The FROM clause ${F} is not a prefix of the clause ${E}.")
	class PrefixOf[F <: FromClause, E <: FromClause] private[FromClause] (val suffix :Int) extends AnyVal {

		@inline def extension :F ExtendedBy E = new ExtendedBy(suffix)

		@inline def stretch[C <: FromClause](implicit next :E PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](suffix + next.suffix)

		@inline def shrink[C <: FromClause](implicit prefix :F PrefixOf E, suffix :C PrefixOf E) :F PrefixOf C =
			new PrefixOf[F, C](this.suffix - suffix.suffix)

		@inline def shrinkFront[C <: FromClause](implicit prefix :F PrefixOf C, suffix :C PrefixOf E) :C PrefixOf E =
			new PrefixOf(this.suffix - prefix.suffix)

		@inline def wrap[D[B <: E] <: DecoratedFrom[B]] :F PrefixOf D[E] = new PrefixOf[F, D[E]](suffix)
	}



	object PrefixOf {
		@inline def apply[F <: FromClause, E <: FromClause](implicit ev :F PrefixOf E) :F PrefixOf E = ev

		private[this] val instance = new PrefixOf[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :PrefixOf[F, F] = instance.asInstanceOf[F PrefixOf F]

		implicit def extend[F <: FromClause, L <: FromClause, J <: L AndFrom R, R[O] <: MappingAt[O]]
		                   (implicit infer :Conforms[J, J, L AndFrom R], ev :F PrefixOf L) :F PrefixOf J =
			new PrefixOf[F, J](ev.suffix + 1)

		implicit def wrap[F <: FromClause, E <: FromClause, D <: DecoratedFrom[E]]
		                 (implicit types :Conforms[D, D, DecoratedFrom[E]], prefix :F PrefixOf E) :F PrefixOf D =
			new PrefixOf[F, D](prefix.suffix)
	}

}


