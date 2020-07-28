package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema.{ColumnMapping, RowSource, SQLForm, BaseMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.RowSource.NamedSource
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.GetTableByIndex.GetTableByNegativeIndex
import net.noresttherein.oldsql.sql.FromClause.GetTableByPredicate.{ByLabel, ByParamName, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables, OuterFrom, SubselectFrom, TableShift}
import net.noresttherein.oldsql.sql.MappingSQL.{ComponentSQL, FreeColumn, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.{?:, FromParam, LabeledFromParam, ParamSource, WithParam}
import net.noresttherein.oldsql.sql.SQLTerm.True
import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.SelectSQL.{SelectAs, SelectColumn}
import net.noresttherein.oldsql.sql.SQLScribe.RemoveParams





/** In its basic use, a `FromClause` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in a query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] class hierarchy) are defined,
  * providing all non-constant values available to them. It consists of a a list of `Mapping`s, together with an optional
  * filter working on those mappings, especially any required join conditions. While the individual elements of a clause
  * are referred to often as tables for simplicity, they can not only be arbitrary relations such as other ''selects'',
  * but also synthetic artifacts such as query parameters. It is even possible to use arbitrary mapping components,
  * to be replaced at a later time with references to concrete tables, parameters or constants. As such, it
  * doesn't necessarily represent a valid fragment of a select from the application's schema.
  * In that case it's best thought as a signature containing declarations of terms (mappings and parameters)
  * over which SQL formulas can be defined; hence `SQLExpression` is parameterized with a `FromClause`.
  *
  * The mappings taking part in the query are encoded in the static type of the standard implementation, which
  * builds both the instance and its type by recursively applying the [[net.noresttherein.oldsql.sql.With With]]
  * class to a shorter clause, extending it by another relation. In that aspect it is quite similar to general
  * purpose heterogeneous lists such as the ''shapeless'' `HList`, replacing its wide functionality with specific
  * application to this task. Just as a `HList` - and all other two-argument type constructors - it can be and is better
  * written using the infix notation: `Dual Join Rangers Join Familiars Join Monsters`. Specialized classes for
  * all join kinds exist. Note however that the whole `Join` class hierarchy is left-associative for more natural
  * left to right reading and writing. As they are also almost universally covariant regarding its left clause
  * type parameter, any prefix can be always substituted with the abstract `FromClause` supertype, balancing
  * static type checking with a degree of freedom and flexibility promoting code reuse. A `FromClause` subtype
  * from the above category is said to be ''complete'' if its definition starts
  * with the [[net.noresttherein.oldsql.sql.Dual Dual]] (or [[net.noresttherein.oldsql.sql.From From]]) type,
  * that is all mapping types participating in the join/clause are known. Note that the mapping types may
  * still be abstract and the joins may be some generic base type like `With`. The opposite is an ''incomplete'' clause,
  * the definition of which starts with the base `FromClause` type or an * abstract type `F &lt;: FromClause`:
  * `FromClause Join Deaths Join Instruments`, which specifies any sequence of relations ending with the `Deaths`
  * and `Instruments`.
  *
  * This trait is a bare bones common upper type, serving at the same time as a wildcard in type definitions meaning
  * 'an arbitrary number of other relations not relevant in the circumstances at hand'. Most functionality
  * is defined in the `With` subclass representing actual non-empty clauses. Additionally, an implicit conversion
  * exists from any `F &lt;: FromClause` to
  * [[net.noresttherein.oldsql.sql.FromClause.FromClauseMethods FromClauseMethods[F] ]] which declares methods
  * for joining it with other relations and others which benefit from having a static, invariant self-type.
  * Individual relations can be most easily accessed through the
  * [[net.noresttherein.oldsql.sql.FromClause.JoinedTables JoinedTables]]
  * and [[net.noresttherein.oldsql.sql.FromClause.JoinedRelations JoinedRelations]] classes exposed by the
  * `tables` and `formulas` methods defined by the implicit conversion.
  *
  * @see [[net.noresttherein.oldsql.sql.With With]]
  * @see [[net.noresttherein.oldsql.sql.Dual Dual]]
  * @see [[net.noresttherein.oldsql.sql.From From]]
  * @see [[net.noresttherein.oldsql.sql.Join Join]]
  * @author Marcin Mościcki
  */
trait FromClause { thisClause =>

	/** Type of the last mapping in this join (the rightmost one) if not empty. `Dual` defines it as `Nothing`. */
	type LastMapping[O] <: MappingAt[O]

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projects the type of the last element
	  * of this clause to an extending row source. Note that `Dual`, containing no relations, defines this type
	  * as `Nothing`.
	  */
	type LastTable[F <: FromClause] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `FromClause`. */
	type FromLast >: this.type <: FromClause

	/** Last relation in this clause when treated as a list, if any.
	  * @throws NoSuchElementException if this clause is `Dual`.
	  */
	def last :LastTable[FromLast]



	/** Number of relations contained in this join (counting all their occurrences separately). */
	def size :Int



	/** Self type of this clause used by some copy constructors present in the subclasses. */
	type This >: this.type <: FromLast

	/** Similar to `Implicit`, this type is always defined as `Dual`, with all join kinds defining it as `left.Init`.
	  * This way, if the compiler can prove that `F#Init =:= Dual`, then the static type `F` is a complete clause -
	  * with all relations and joins known. It is of little use by itself in the client code.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.CompleteFrom]]
	  */
	type Init <: FromClause



	/** A recursive reconstruction of this clause type. For all `With` sublasses, it is defined by replacing the
	  * covariant left side `L` constituting the prefix clause with `left.Self` in their type definition. `Dual`
	  * serves as the recursion terminator and defines `Self = Dual`. While impossible to enforce in the type system,
	  * for any concrete clause type `F &lt;: FromClause` and any `f :F`, `F &lt;: f.Self`. It allows subclasses to
	  * define copying methods and circumvents the constraint of the 'left' type parameter of all join classes being
	  * covariant (and thus illegal for use on the right side of a type alias definition).
	  */
	type Self <: Generalized { type Self = thisClause.Self }

	/** This clause as its `Self` type. */
	def self :Self



	/** The type of this clause with all join kinds replaced with the root type `With` and initial `Dual`
	  * with `FromClause`. It forms the class of abstraction of clauses with the same relations (and in the same order),
	  * which are considered equivalent for many purposes, in particular as the basis for SQL expressions `SQLExpression`.
	  * Most `SQLExpression` instances returned by this clause are parameterized with this type.
	  */
	type Generalized <: FromLast { type Generalized <: thisClause.Generalized } //todo: extra refinement

	/** This clause upcast to the generalized form in which all join kinds are replaced with `With`. */
	def generalized :Generalized = self



	/** The ''WHERE'' part of this clause representing the filter condition as SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause.
	  */
	def filter :SQLBoolean[Generalized] = filter(generalized)

	/** The combined join conditions of all joins in this clause as a expression based on an extending clause.
	  * Used by zero-argument `filter` to request the individual join conditions as formulas based on the clause
	  * it was called for.
	  */
	def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E]



	/** Subject types of all mappings in this clause, concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type Row <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` formulas for all joined elements in their order
	  * of appearance. It will contain entries for all mappings in this clause, including parameter mappings
	  * and mappings listed in this clause's `Implicit` prefix (if this clause is a subselect clause).
	  */
	def row :ChainTuple[Generalized, Row] = row(generalized)

	/** Create an SQL tuple expression, based on some extending clause `E`, containing `JoinedRelation` formulas
	  * for all joined elements in their order of appearance. It will contain entries for all mappings in this clause,
	  * including parameter mappings and mappings listed in this clause's `Implicit` prefix (if this clause
	  * is a subselect clause. This overloaded variant is used by the zero-argument `row` to obtain the chain prefix
	  * containing the relation formulas based on the final clause type from a prefix clause of a join.
	  */
	def row[E <: FromClause](target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, Row]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def tableStack :LazyList[SQLRelation.AnyIn[Generalized]] = tableStack(generalized)

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * The `JoinedFormula`s are based on some extending clause `E`, so that the stack for a prefix clause
	  * can be used as-is by extending clause's zero argument `tableStack`.
	  */
	def tableStack[E <: FromClause]
	              (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]]



	/** A join of relations from the clause `P` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `P`, that is appending all mappings from this clause
	  * in order to `P`, using the same join kinds. The join type `J` is used as the join between the last relation
	  * in `P` and the first relation in `this`.
	  */
	type JoinedWith[+P <: FromClause, +J[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R] <: Generalized

	/** An auxiliary type used in definitions of `JoinedWith` in join classes. Similar to `JoinedWith`,
	  * it is a cross join between a prefix clause `F`, this clause, and mapping `T`. The last join with `T` becomes
	  * either the join `J`, if this clause contains no mappings itself (i.e. the result becomes `J[F, T]`),
	  * or `N` if this clause is non empty, resulting in `N[,JoinedWith[F, J], T]`.
	  */
	type ExtendJoinedWith[+F <: FromClause, +J[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R,
	                      +N[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R, T[O] <: MappingAt[O]] <:
		Generalized With T

	/** A join of relations from the clause `F` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `F`, that is appending all mappings from this clause
	  * in order to `F`, using the same join kinds. As the call to this method is written in the reversed order
	  * to the one in which both clauses participate in the join and accepts an additional template argument,
	  * it is recommended to use one of regular `join`, `outerJoin`, etc. methods introduced by an implicit conversion:
	  * `this rightJoin that` is equivalent to `that.joinedWith(this, RightJoin.template)`.
	  * @param prefix a ''from'' clause containing first relations of the produced cross join.
	  * @param firstJoin a template join instance defining the kind of the join between the last relation in `prefix`
	  *                  and the first relation in `this`.
	  */
	def joinedWith[F <: FromClause](prefix :F, firstJoin :Join.*) :JoinedWith[F, firstJoin.LikeJoin]

	/** Helper method used by `Join` classes to implement `joinedWith`. It produces a join of relations from the clause
	  * `prefix` with the relations from this clause, followed by the relation `T` from `nextJoin`. The last relation
	  * is joined using the same kind of join as in `nextJoin`, unless this clause is `Dual`, in which case the kind
	  * of join of `firstJoin` is used instead.
	  */
	def extendJoinedWith[F <: FromClause, T[O] <: BaseMapping[X, O], X]
	                    (prefix :F, firstJoin :Join.*, nextJoin :this.type Join T)
			:ExtendJoinedWith[F, firstJoin.LikeJoin, nextJoin.LikeJoin, T]






	/** A property specifying if this ''from'' clause is a subselect clause, that is it has a `Subselect` join
	  * somewhere in its complete (dynamic) type.
	  */
	def isSubselect :Boolean = false

	def asSubselect :Option[SubselectFrom { type Implicit = thisClause.Implicit }] =
		if (isSubselect) Some(this.asInstanceOf[SubselectFrom { type Implicit = thisClause.Implicit }])
		else None



	/** Super type of all `FromClause` subtypes representing a subselect directly nested under this clause's select.
	  * As a member type, it is able to define `Implicit` type of this refinement as `this.Generalized`,
	  * `S &lt;: f.Nested` being thus a more specific way of establishing the relationship than `S &lt;: SubselectOf[F]`.
	  */
	type Nested = SubselectFrom { type Implicit = thisClause.Generalized }

	/** A prefix ''from'' clause of this clause consisting of all relations available to SQL expressions explicitly,
	  * that is not included in the actual ''from'' clause of this subselect clause.
	  * It is the generalized type of the outer clause if this clause represents a subselect clause created by
	  * `Implicit.subselectFrom()`. All `ProperJoin` subclasses have this type equal to the `Implicit` type
	  * of their left side, but `Subselect` defines `Implicit` as the generalized type of its left side. Additionally,
	  * all 'proper' joins conform to `AsSubselectOf[L#Implicit]` and `L Subselect R` conforms to `AsSubselectOf[L]`.
	  * This means that for any type `S &lt;: FromClause` with fully instantiated parameters (the clause is complete,
	  * that is all relations in `S` and kinds of joins in it are known) value
	  * `(s :S) subselectFrom t1 join t2 ... join t3` conforms to `SubselectOf[S]` and `s.Nested`.
	  * This way one can statically express a dependency relationship between ''from'' clauses without resorting
	  * to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Nested]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type Implicit <: FromClause

	/** A prefix ''from'' clause of this clause consisting of all joins and relations preceding the last (rightmost)
	  * `Subselect` join. If there are no `Subselect` joins in the complete type definition of this clause,
	  * it is equal to `Dual`. It represents the clause of the outer select for this subselect clause, assuming
	  * this clause is a subselect clause created by `outer.subselectFrom()` (and possibly expanded further).
	  * All `ProperJoin` subclasses have this type equal to the `Outer` type of their left side, but `Subselect`
	  * defines `Outer` as the `Selef` type of its left side.
	  * This means that for any type `S &lt;: FromClause` with fully instantiated parameters (the clause is complete,
	  * that is all relations in `S` and kinds of joins in it are known) value
	  * `(s :S) subselectFrom t1 join t2 ... join t3` conforms to `FromClause { type Outer = s.Self }`.
	  * This way one can statically express a dependency relationship between ''from'' clauses without resorting
	  * to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicituter]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type Outer <: Implicit

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselectFrom()`. When viewed as a list of relations, `outer` constitutes
	  * the result of dropping all relations joined in this instance up until and including a relation joined
	  * by a `.subselectFrom()` call, going from right to left. If there's no `Subselect` in the dynamic type definition
	  * of this clause, meaning this is not a subselect clause (the actual ''from'' clause of resulting selects
	  * will include all members of this clause), `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or just the left side if this instance is a `Subselect`.
	  */
	def outer :Outer

	/** The incomplete suffix clause containing all relations since the last `Subselect` join or `Dual`, with all
	  * join kinds replaced with the base `With`. It lists all relations present in the actual ''from'' clause of
	  * the associated SQL select in a canonical form, abstracting from join kinds. It can be formed by substituting
	  * in the complete type of this clause the `Outer` prefix containing relations present 'implicitly' in the scope
	  * with `FromClause` and upcasting all join kinds to the base `With` type. All `With` subclasses define
	  * it as `left.Explicit With R`, with the exception of `Subselect` join, which defines it as `left.Generalized`
	  * instead.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Inner]]
	  */
	type Explicit >: Generalized <: FromLast

	/** The incomplete suffix clause containing all relations from the explicit portion of this subselect clause.
	  * It represents the actual ''from'' clause of an associated SQL subselect. For subselect clauses, it results from
	  * replacing the left side of the last `Subselect` in this type's definition with `FromClause`; for non-subselect
	  * clauses (with no `Subselect` joins in its complete type), it is the full type of this clause with `Dual`
	  * replaced with `FromClause`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Explicit]]
	  */
	type Inner <: Explicit



	/** Number of relations contained in the explicit portion of this subselect join. This is equal to
	  * the number of mappings to the right of the rightmost `Subselect` join or `Dual`, that is `size - outer.size`.
	  */
	def subselectSize :Int = size - outer.size



	/** The ''WHERE'' part of this subselect clause representing the explicit filter condition as SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause since the last `Subselect` join.
	  */
	def subselectFilter :SQLBoolean[Generalized] = subselectFilter(generalized)

	/** The combined join conditions of all joins since the last `Subselect` join as a expression based on an extending
	  * clause. Used by zero-argument `filter` to request the individual join conditions as formulas based on the clause
	  * it was called for.
	  */
	def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E]



	/** Subject types of all mappings in this clause following the `Implicit` prefix, concatenated into a heterogeneous list.
	  * This amounts to the list of values mapped by the relations from the ''from'' clause of the most deeply nested
	  * subselect. If this clause doesn't represent a subselect, but a top-level query, it is the same as `Row`.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#subselectRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Row]]
	  */
	type SubselectRow <: Chain

	/** Create an SQL tuple expression containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. This includes all relations in this clause following
	  * the most recent `Subselect` 'join', marking the first relation following the `Implicit` clause prefix.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	def subselectRow :ChainTuple[Generalized, SubselectRow] = subselectRow(generalized)

	/** Create an SQL tuple expression containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. The formulas are based on some extending clause `E`,
	  * so they can be used by the zero-argument `subselectRow` as the chain prefix of its result.
	  */
	def subselectRow[E <: FromClause](target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first relation following the `Implicit` prefix. If this is not a subselect clause (no `Subselect` 'joins'
	  * are present in this clause and `Implicit =:= FromClause`), all relations are included.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def subselectTableStack :LazyList[SQLRelation.AnyIn[Generalized]] = subselectTableStack(generalized)

	/** All relations in this clause in the reverse order, ending with the last (right-most) appearance of a `Subselect`
	  * 'join' or `Dual`. The elements are returned as `JoinedRelation`s for generic, untyped mappings,
	  * based on some extending clause `E`. Used by the zero-argument `subselectTableStack`
	  * to request the tail of the stack with formulas of the correct type from the prefix clause.
	  */
	def subselectTableStack[E <: FromClause]
	                       (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]]



	/** A type constructor appending all relations joined since the last (rightmost) `Subselect` join kind to the
	  * given clause `F`. For any complete clause `J &lt;: FromClause`, `J#AsSubselectOf[J#Implicit]` consists of all
	  * relations from `J` in order joined with the abstract `FromClause` using the same join kinds. Additionally,
	  * `J &lt;:&lt; J#AsSubselectOf[J#Implicit]` and `J#AsSubselectOf[J#Implicit] &lt;:&lt; J#Generalized`.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Implicit]]
	  */
	type AsSubselectOf[F <: FromClause] <: FromLast {
		type Explicit = thisClause.Explicit
	}

	/** An auxiliary type used to define `AsSubselectOf[F]` for a clause extending this clause with mapping `T`.
	  * It uses the `J#LikeJoin` to join `this.AsSubselectOf[F]` with `R`, unless this instance is `Dual`, in which
	  * case a `Subselect` join is used.
	  */
	type ExtendAsSubselectOf[F <: FromClause, J[+L <: FromClause, R[A] <: MappingAt[A]] <: L ProperJoin R,
	                         T[A] <: MappingAt[A]] <:
		FromClause With T

	/** For subselect clauses - that is subtypes with a `Subselect` join kind occurring somewhere in their definition,
	  * not necessarily `Subselect` instances - it represents them as a subselect of a clause `F`, being
	  * an extension of their outer clause (the left side of the right-most `Subselect` join). In syntactic terms,
	  * it replaces the `Implicit` type in this type's definition with type `F`. Procedurally, it joins in order
	  * all relations since the last occurrence of a `Subselect` join, forming the explicit ''from'' clause of
	  * a modified subselect, with the new outer clause `F`, preserving all join conditions. If this clause is not a
	  * subselect clause, this method will use `Subselect` join to join the `newOuter` with the first relation of this
	  * clause. All join conditions are preserved.
	  * @return a ''from'' clause conforming to `SubselectOf[F]`.
	  */
	def asSubselectOf[F <: FromClause](newOuter :F)(implicit extension :Implicit ExtendedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self }

	/** An auxiliary method used to implement `asSubselectOf` by inversion of control: instead of each join instance
	  * copying its join and relation itself, it passes itself to its left side, which invokes its `withLeft` method
	  * in a double dispatch. This allows the `Dual` class to override this behavior and use a `Subselect` to join
	  * the following relation, when `asSubselectOf` is called for a non-subselect clause.
	  */
	def extendAsSubselectOf[F <: FromClause, T[A] <: BaseMapping[X, A], X]
	                       (newOuter :F, next :this.type ProperJoin T)(implicit extension :Implicit ExtendedBy F)
			:ExtendAsSubselectOf[F, next.LikeJoin, T] {
				type Implicit = newOuter.Generalized
				type Outer = newOuter.Self
				type Explicit = next.Explicit
			}





	/** Creates an SQL ''select'' expression with this clause used for the ''from'' and ''where'' clauses
	  * and the ''select'' clause consisting of all selectable columns of the mapping returned by the passed function.
	  */
	def selectAs[C[A] <: MappingAt[A], M[A] <: BaseMapping[S, A], S, O >: Generalized <: FromClause]
	            (component :JoinedTables[Generalized] => C[O])
	            (implicit typer :Conforms[C[O], M[O], BaseMapping[S, O]], shift :TableShift[O, M, _ <: Numeral])
			:Implicit SelectAs M[Any] =
		{
			type Mock = SQLRelation[Generalized, MappingOf[Any]#TypedProjection, Any, O]
			val table = tableStack(shift.tables).asInstanceOf[Mock]
			val comp = table \ component(generalized.tables)
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
	@inline def select[V](header :JoinedTables[Generalized] => SQLExpression[Generalized, V])
			:SelectSQL[Implicit, V, _] =
		select(header(generalized.tables))


	/** Creates an SQL ''select'' expression selecting a single column, as defined by the result of the passed function,
	  * and this instance as the source of its ''from'' and ''where'' clauses.
	  */
	@inline def selectColumn[V](header :JoinedTables[Generalized] => ColumnSQL[Generalized, V])
			:SelectColumn[Implicit, V, _] =
		select(header(generalized.tables))


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



	/** Creates an SQL ''select'' expression selecting all columns from all relations as a `Chain` of relation subjects.
	  * This is equivalent to `select(this.row)`.
	  */
	def select_* :SelectSQL[Implicit, Row, _] = select(row)






	/** Function type returning a join condition between the last relation in this clause and a following mapping `T`.
	  * Used as the parameter for [[net.noresttherein.oldsql.sql.With#on With.on]], it is declared here
	  * (on the 'left side' of a new join) to ensure that `on` won't be possible to call if the left side of
	  * a `With` clause is abstract or empty, i.e. that the clause contains at least two relations and their
	  * types are known.
	  */
	type JoinFilter[T[O] <: MappingAt[O]] <:
		(JoinedRelation[FromLast With T, LastMapping], JoinedRelation[FromClause With T, T])
			=> SQLBoolean[FromLast With T]



	/** Creates a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  * instance's filter and the given `SQLBoolean`.
	  */
	protected def withCondition(filter :SQLBoolean[Generalized]) :This

	/** Apply an additional filter to this clause.
	  * @param condition a function accepting a facade to this clause, which provides access to all its relations,
	  *                  and returning a new filter condition based on these relations.
	  * @return a `FromClause` of the same type as this one, but with its `filter` being the conjunction of this
	  *         instance's filter and the `SQLBoolean` returned by the function.
	  */
	def where(condition :JoinedTables[Generalized] => SQLBoolean[Generalized]) :This



	/** A `Chain` listing all parameters of this clause joined with
	  * the [[net.noresttherein.oldsql.sql.FromClause.OuterFromMethods.param param]] method.
	  * In particular, a `FromClause` subtype with no `JoinParam` joins in its full type has this type equal to `@~`.
	  * This is can be used to statically enforce that a clause is unparameterized by refining on this type:
	  * `from :FromClause { type Params = @~ }`, or `from :ParameterlessFrom` as a less bug-prone alternative.
	  */
	type Params <: Chain






	/** Creates an `InnerJoin` of `this` and `right`. Polymorphic as a join with `Dual` results in `From`, extracted
	  * to a method to eliminate casting in pattern matching.
	  */
	protected[sql] def selfInnerJoin[T[A] <: BaseMapping[X, A], X]
	                                (right :LastRelation[T, X])(filter :SQLBoolean[Generalized With T])
			:this.type InnerJoin T =
		InnerJoin.newJoin[T, X](this, right)(filter)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]


}






object FromClause {

	/** A type to which all complete ''from'' clauses conform. If `F &lt;: CompleteFrom`, then the number of joins
	  * in the clause is known. Note that the mappings can still be abstract types and the join kinds may be some
	  * supertypes of actual joins used. However, if `F &lt;: CompleteFrom`, than also `F#Generalized` is statically
	  * known and `F &lt;:&lt; F#Generalized` can be proven, even if the type system doesn't allow to express it here.
	  * It relies on the `Init` member type of `FromClause` introduced specifically for this purpose: all `With`
	  * subclasses define it simply as `left.Init`, with `Dual` defining it as `Dual`. Being able to prove that
	  * `Init =:= Dual` means that one can 'see all the way back' from the last join.
	  */
	type CompleteFrom = FromClause {
		type Init = Dual
		type Generalized >: this.type <: FromLast
	}



	/** A complete `FromClause`, which doesn't contain any parameters and does not represent a subselect
	  * of another clause. `S &lt;: OuterFrom` if and only if it is a complete clause (with no abstract joins
	  * or `FromClause` occurring in its definition) and doesn't contain any `JoinParam` or `Subselect`.
	  * The name stems from the fact that only such sources can be used to create independent,
	  * most outer select statements.
	  */
	type OuterFrom = FromClause { //todo: refinement on additional member types: Explicit, Inner, Outer, Self
		type Implicit = FromClause
		type Generalized >: this.type <: FromLast
	}



	/** An upper bound for `FromClause` subtypes representing ''from'' clauses of subselects of a select
	  * with the ''from'' clause `F`. `S &lt;: AsSubselectOf[F]` if and only if `S` has the form of
	  * `F Subselect M1 J2 M2 ... JN MN` for some mappings `M1...MN` and non-subselect join types `J2...JN`,
	  * and both types are complete clauses. Clauses conforming to `AsSubselectOf[F]` can use all the mappings/tables
	  * which are a part of `F`, but they are not a part of any select formulas created from that source. This allows
	  * the use of nested select queries which depend on values from the ''from'' clause of the outer select.
	  * Somewhat counterintuitively, this type is contravariant rather than covariant. There are two reasons behind it:
	  * one, preventing any type from being a subselect clause of a clause with an abstract prefix, ensuring that
	  * full mapping lists are compared, and two, treating all join kinds as equivalent for this purpose.
	  * Note that subselects may be nested to an arbitrary depth and only directly nested subselects of `F`
	  * conform to this type.
	  */
	type SubselectOf[-F <: FromClause] = FromClause {
		type Implicit >: F <: FromClause
	}

	/** An upper bound for all ''from'' clauses of subselect expressions, that is `FromClause` subtypes with a
	  * `Subselect` join in the type definition: `S &lt;: F Subselect T1 ... TN . ` for some type `F &lt;: FromClause`
	  * and mapping type constructors `T1 ... TN`. For a type to conform to `SubselectFrom`, the join kinds in
	  * the explicit part of the clause (after the last subselect) must be statically known to be `ProperJoin` subtypes.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
	  */
	type SubselectFrom = FromClause {
		type Outer <: FromClause //to shut up IntelliJ
		type Self >: AsSubselectOf[Outer] //works because for non-subselects Dual Join X becomes Dual Subselect X
	}



	/** An upper bound for all ''from'' clauses which do not contain any `JoinParam` 'joins' in their complete type.
	  * In order to prove this conformity the clause type must be complete and cannot contain `With` joins
	  * (all joins in its static type must be of some proper `With` subclass). It is possible however to propagate
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
	type ParameterizedFrom = FromClause {
		type Params <: Chain ~ _
	}






	/** A wrapper type adapting the labeled mapping type `L @: M` to a form with a single-argument type constructor
	  * accepting the `Origin` type for use in `With` classes and other types accepting such a type constructor:
	  * `Dual With (Humans As "humans")#T` (where `Humans[O] &lt;: MappingAt[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */
	type As[M[O] <: MappingAt[O], A <: Label] = { type T[O] = A @: M[O] }

	class AliasedSource[T[O] <: MappingAt[O], A <: Label](source :RowSource[T], val alias :A)
		extends NamedSource[A, (T As A)#T]
	{
		override def name :A = alias

		override def apply[O] :A @: T[O] =
			(alias @: source[O].asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[A @: T[O]]
	}

	def AliasedSource[T[O] <: MappingAt[O], A <: Label](source :RowSource[T], alias :A) :NamedSource[A, (T As A)#T] =
		new AliasedSource(source, alias)






	/** Extension methods for `FromClause` classes which benefit from having a static, invariant self type.
	  * Most notably, this includes methods for joining it with other relations.
	  */
	implicit class FromClauseMethods[F <: FromClause](val thisClause :F) extends AnyVal {

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `Mapping` instances of one of the types listed in this clause, using their `Origin`
		  * type to encode their position on the list.
		  */
		@inline def tables :JoinedTables[F] = new JoinedTables[F](thisClause)

		/** A facade to this `FromClause` providing access to all relations it consists of. The relations are
		  * returned as `JoinedTable` SQL expressions parameterized with any of the mapping types listed by this clause
		  * and a supertype of this clause as the base for the expression.
		  */
		@inline def relations :JoinedRelations[F] = new JoinedRelations[F](thisClause)



		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'implicit' portion is a prefix clause of its `Self` type (that is,
		  * its complete type with all join kinds known). For non-subselect clauses, it is the number of all relations.
		  */
		@inline def subselectSuffix :thisClause.Outer PrefixOf thisClause.Self =
			new PrefixOf[thisClause.Outer, thisClause.Self](thisClause.subselectSize)

		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'implicit' base clause, constituting of all relations listed by
		  * enclosing selects, is a prefix of the `Generalized` type of this clause. For non-subselect clauses,
		  * it represents the full `Generalized` type size (that is, all relations in the clause).
		  */
		@inline def generalizedSubselectSuffix :thisClause.Implicit PrefixOf thisClause.Generalized =
			new PrefixOf[thisClause.Implicit, thisClause.Generalized](thisClause.subselectSize)

		/** The number of relations in the explicit ''from'' clause of this subselect, that is `this.subselectSize`,
		  * as a witness to the fact that its 'Implicit' type is the generalization of the prefix clause
		  * consisting of all relations preceding the last `Subselect` join. For non-subselect clauses,
		  * it represents the full type `F`, with all its relations.
		  */
		@inline def subselectSpan :thisClause.Implicit ExtendedBy F =
			new ExtendedBy(thisClause.subselectSize)



		/** Performs an inner join between this clause on the left side, and the relation given as a `RowSource`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def join[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                (table :RowSource[R])
		                (implicit cast :InferSubject[thisClause.type, InnerJoin, R, T, S]) :F InnerJoin R =
		InnerJoin(thisClause, table)

		/** Performs an inner join between this clause on the left side, and the relation given as a `Mapping`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  */
//		@inline def join[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                (table :R[A])
//		                (implicit cast :InferSubject[thisClause.type, InnerJoin, R, T, S],
//		                 alias :OriginProjection[R[A], A, R[Any], Any]) :F InnerJoin R =
//			join(RowSource(table))

		/** Performs an inner join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. Both the dynamic and static types of the joins from the parameter clause
		  * are preserved, as are all join conditions between them. The join condition between the clauses can be
		  * subsequently specified using the [[net.noresttherein.oldsql.sql.With#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def join[R <: FromClause](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(thisClause, InnerJoin.template)



		/** Performs a symmetric outer join between this clause on the left side, and the relation given as a `RowSource`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def outerJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                    (table :RowSource[R])
		                    (implicit cast :InferSubject[thisClause.type, OuterJoin, R, T, S]) :F OuterJoin R =
			OuterJoin(thisClause, table)

		/** Performs a symmetric outer join between this clause on the left side, and the relation given as a `Mapping`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  */
//		@inline def outerJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                    (table :R[A])
//		                    (implicit cast :InferSubject[thisClause.type, OuterJoin, R, T, S],
//		                     alias :OriginProjection[R[A], A, R[Any], Any]) :F OuterJoin R =
//			outerJoin(RowSource(table))

		/** Performs a symmetric outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. Both the dynamic and static types of the joins from the parameter clause
		  * are preserved, as are all join conditions between them. The join condition between the clauses can
		  * be subsequently specified using the [[net.noresttherein.oldsql.sql.With#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def outerJoin[R <: FromClause](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(thisClause, OuterJoin.template)



		/** Performs a left outer join between this clause on the left side, and the relation given as a `RowSource`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def leftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                    (table :RowSource[R])
		                    (implicit cast :InferSubject[thisClause.type, LeftJoin, R, T, S]) :F LeftJoin R =
			LeftJoin(thisClause, table)

		/** Performs a left outer join between this clause on the left side, and the relation given as a `Mapping`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  */
//		@inline def leftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                    (table :R[A])
//		                    (implicit cast :InferSubject[thisClause.type, LeftJoin, R, T, S],
//		                     alias :OriginProjection[R[A], A, R[Any], Any]) :F LeftJoin R =
//			leftJoin(RowSource(table))

		/** Performs a left outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. Both the dynamic and static types of the joins from the parameter clause
		  * are preserved, as are all join conditions between them. The join condition between the clauses can
		  * be subsequently specified using the [[net.noresttherein.oldsql.sql.With#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def leftJoin[R <:FromClause ](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(thisClause, LeftJoin.template)



		/** Performs a right outer join between this clause on the left side, and the relation given as a `RowSource`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  */
		@inline def rightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                     (table :RowSource[R])
		                     (implicit cast :InferSubject[thisClause.type, RightJoin, R, T, S]) :F RightJoin R =
			RightJoin(thisClause, table)

		/** Performs a right outer join between this clause on the left side, and the relation given as a `Mapping`
		  * object on the right side.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  */
//		@inline def rightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                     (table :R[A])
//		                     (implicit cast :InferSubject[thisClause.type, RightJoin, R, T, S],
//		                      alias :OriginProjection[R[A], A, R[Any], Any]) :F RightJoin R =
//			rightJoin(RowSource(table))

		/** Performs a right outer join between this clause on the left side, and all relations listed by the `other`
		  * clause on the right side. Both the dynamic and static types of the joins from the parameter clause
		  * are preserved, as are all join conditions between them. The join condition between the clauses can
		  * be subsequently specified using the [[net.noresttherein.oldsql.sql.With#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  */
		@inline def rightJoin[R <: FromClause](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(thisClause, RightJoin.template)



		/** Performs a natural inner join between this clause on the left side, and the relation given as a `RowSource`
		  * object on the right side. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                       (table :RowSource[R])
		                       (implicit cast :InferSubject[thisClause.type, InnerJoin, R, T, S],
		                        last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F InnerJoin R =
			cast(InnerJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T])

		/** Performs a natural inner join between this clause on the left side, and the relation given as a `Mapping`
		  * object on the right side. The join condition of the created instance will compare all columns of the last
		  * relation in this clause with columns with matching names from the given relation. If the column types
		  * (associated `ColumnForm` objects) of any of these column pairs differ, an `IllegalArgumentException`
		  * is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
//		@inline def naturalJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                       (table :R[A])
//		                       (implicit cast :InferSubject[thisClause.type, InnerJoin, R, T, S],
//		                        alias :OriginProjection[R[A], A, R[Any], Any],
//		                        last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F InnerJoin R =
//			naturalJoin(RowSource(table))



		/** Performs a natural symmetric outer join between this clause on the left side, and the relation given
		  * as a `RowSource` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalOuterJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :RowSource[R])
		                            (implicit cast :InferSubject[thisClause.type, OuterJoin, R, T, S],
		                             last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F OuterJoin R =
			cast(OuterJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T])

		/** Performs a natural symmetric outer join between this clause on the left side, and the relation given
		  * as a `Mapping` object on the right side. The join condition of the created instance will compare all columns
		  * of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
//		@inline def naturalOuterJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                            (table :R[A])
//		                            (implicit cast :InferSubject[thisClause.type, OuterJoin, R, T, S],
//		                             alias :OriginProjection[R[A], A, R[Any], Any],
//		                             last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F OuterJoin R =
//			naturalOuterJoin(RowSource(table))



		/** Performs a natural left outer join between this clause on the left side, and the relation given
		  * as a `RowSource` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalLeftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                           (table :RowSource[R])
		                           (implicit cast :InferSubject[thisClause.type, LeftJoin, R, T, S],
		                            last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F LeftJoin R =
			cast(LeftJoin[thisClause.type, T, T, S](thisClause, cast(table)) where naturalFilter[T])

		/** Performs a natural left outer join between this clause on the left side, and the relation given
		  * as a `Mapping` object on the right side. The join condition of the created instance will compare all columns
		  * of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
//		@inline def naturalLeftJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                           (table :R[A])
//		                           (implicit cast :InferSubject[thisClause.type, LeftJoin, R, T, S],
//		                            alias :OriginProjection[R[A], A, R[Any], Any],
//		                            last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F LeftJoin R =
//			naturalLeftJoin(RowSource(table))



		/** Performs a natural right outer join between this clause on the left side, and the relation given
		  * as a `RowSource` object on the right side. The join condition of the created instance will compare
		  * all columns of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
		@inline def naturalRightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                            (table :RowSource[R])
		                            (implicit cast :InferSubject[thisClause.type, RightJoin, R, T, S],
		                             last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F RightJoin R =
			cast(RightJoin[thisClause.type, T, T, S](thisClause, cast(table))(cast.self) where naturalFilter[T])

		/** Performs a natural right outer join between this clause on the left side, and the relation given
		  * as a `Mapping` object on the right side. The join condition of the created instance will compare all columns
		  * of the last relation in this clause with columns with matching names from the given relation.
		  * If the column types (associated `ColumnForm` objects) of any of these column pairs differ,
		  * an `IllegalArgumentException` is thrown.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  * @param last an implicit accessor to the last table of this clause (left side).
		  */
//		@inline def naturalRightJoin[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                            (table :R[A])
//		                            (implicit cast :InferSubject[thisClause.type, RightJoin, R, T, S],
//		                             alias :OriginProjection[R[A], A, R[Any], Any],
//		                             last :GetTableByNegativeIndex[thisClause.Generalized, -1]) :F RightJoin R =
//			naturalRightJoin(RowSource(table))



		private def naturalFilter[T[O] <: BaseMapping[_, O]]
		                         (tables :JoinedTables[thisClause.Generalized With T])
		                         (implicit prev :GetTableByNegativeIndex[thisClause.Generalized With T, -2])
				:SQLBoolean[thisClause.Generalized With T] =
		{
			val firstTable = tables.prev
			val secondTable = tables.last

			val firstColumns = firstTable.columns.map(c => c.name -> c).toMap //todo: below - nondeterministic compilation
			val secondColumns = secondTable.columns.map(c => c.name -> (c :ColumnMapping[_, FromClause With T])).toMap
			val common = firstColumns.keySet & secondColumns.keySet

			val joins = common map { name =>

				val first = firstColumns(name).asInstanceOf[ColumnMapping[Any, prev.J]]
				val second = secondColumns(name).asInstanceOf[ColumnMapping[Any, FromClause With T]]
				if (first.form != second.form)
					throw new IllegalArgumentException(s"Can't perform a natural join of $firstTable and $secondTable: " +
							s"columns $first and $second have different types (forms): ${first.form} and ${second.form}.")

				FreeColumn(first, 1) === FreeColumn(second, 0)
			}
			(True[thisClause.Generalized With T]() /: joins)(_ && _)
		}



		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `RowSource` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselectFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                         (table :RowSource[R])
		                         (implicit cast :InferSubject[thisClause.type, Subselect, R, T, S]) :F Subselect R =
			Subselect(thisClause, table)

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `Mapping` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers.
		  * The join condition can be subsequently specified using the [[net.noresttherein.oldsql.sql.With#on on()]],
		  * [[net.noresttherein.oldsql.sql.With#where where()]] or
		  * [[net.noresttherein.oldsql.sql.With#whereLast whereLast()]] method.
		  * @param table a producer of the mapping for the relation.
		  * @param cast an implicit witness helping with type inference of the subject type of the mapping type `R`.
		  * @param alias an implicit witness allowing the mapping type to be projected to another `Origin` member type.
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
//		@inline def subselectFrom[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A]
//		                         (table :R[A])
//		                         (implicit cast :InferSubject[thisClause.type, Subselect, R, T, S],
//		                          alias :OriginProjection[R[A], A, R[Any], Any]) :F Subselect R =
//			subselectFrom(RowSource(table))

		/** Creates a ''from'' clause of a subselect of an SQL ''select'' expression based using this clause.
		  * The explicit list of relations in the clause is initialized with the relation given as a `RowSource` object
		  * and can be further expanded by joining with additional relations. The created clause is represented
		  * as a linearization of the explicit portion - the given relation and all others, following the `Subselect`
		  * pseudo join - and the implicit portion, constituting of this clause. This grants access to the
		  * mappings for all relations in this clause to any expression based on the created instance, in particular
		  * ''where'' clause filters and `select` clause headers. The join condition can be subsequently specified using
		  * the [[net.noresttherein.oldsql.sql.With#where where()]] method.
		  * @param other a `FromClause` listing relations which should be appended to this clause (i.e. joined,
		  *              preserving the order).
		  * @see [[net.noresttherein.oldsql.sql.Subselect]]
		  */
		@inline def subselectFrom[R <: FromClause](other :R) :other.JoinedWith[F, Subselect] =
			other.joinedWith(thisClause, Subselect.template)



		/** Applies this clause to its parameters: removes all `JoinParam` joins and substitutes all references
		  * to parameter components with SQL literals extracted from parameter values.
		  * @param params a chain consisting of subject types of all parameter mappings of all `JoinParam` joins
		  *               in this clause in order of their appearance.
		  * @tparam U `FromClause` subtype obtained by removing all `JoinParam` instances from this clause's type.
		  */
		def apply[U <: ParameterlessFrom](params :thisClause.Params)
		                                 (implicit apply :ApplyJoinParams[F, thisClause.Params, U]) :U =
			apply(thisClause, params)

	}






	implicit class OuterFromMethods[F <: OuterFrom](private val thisClause :F) extends AnyVal {

		/** Creates a parameterized `FromClause` instance allowing the use of a statement parameter `X` in the SQL
		  * expressions based on the created object. The parameter is represented as a synthetic `Mapping` type,
		  * the subject of which can be used as the subject of any other joined relation. Additionally, it
		  * allows the creation of components for arbitrary functions of `X`, which can be used in SQL expressions
		  * the same way as other mappings' components. The value for the parameter will be provided at a later time
		  * as a parameter of SQL statements created using the returned instance.
		  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
		  * @see [[net.noresttherein.oldsql.sql.JoinParam.FromParam]]
		  */
		@inline def param[X :SQLForm] :F WithParam X = JoinParam(thisClause, ParamSource[X]())

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
		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(thisClause, ParamSource[X](name))

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
			JoinParam(thisClause, form ?: (name.value :N))

	}






	/** A facade to a ''from'' clause of type `F`, providing access to mappings for all relations listed in its type.
	  * The `Origin` type of every returned `M[O] &lt;: MappingAt[O]` instance (and, by transition, its components)
	  * is the generalized super type of `F` formed by replacing all mappings preceding `M` in its type definition
	  * with the abstract `FromClause`, and all join kinds with the base `With` type. As the result, the mapping `M`
	  * becomes the first mapping in the origin clause, and the number of all mappings defines the index of the mapping
	  * in this clause (when counting from the right).
	  */
	implicit class JoinedTables[F <: FromClause](private val thisClause :F) extends AnyVal {

		/** Returns the `Mapping` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject.Get[F, E]) :get.T[get.J] = get(thisClause).mapping

		/** Returns the `Mapping` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.Join.as Join.as]]
		  * method.
		  * @param alias a `String` literal used as the label of the accessed mapping.
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def apply[A <: Label](alias :A)(implicit get :ByLabel.Get[F, A]) :get.T[get.J] =
			get(thisClause).mapping

		/** Returns the `Mapping` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByTypeConstructor.Get[F, M]) :M[get.J] =
			get(thisClause).mapping


		/** Returns the `Mapping` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :GetTableByIndex[F, N]) :get.T[get.J] = get(thisClause).mapping


		/** Returns the `Mapping` instance for the last relation in this clause (counting from the right). */
		def last(implicit get :GetTableByNegativeIndex[F, -1]) :get.T[get.J] = get(thisClause).mapping

		/** Returns the `Mapping` instance for the second-last relation in this clause (counting from the right). */
		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :get.T[get.J] = get(thisClause).mapping



		//todo: ByParamIndex
		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor.Get[F, ParamSource[X]#Row]) :get.T[get.J] =
			get(thisClause).mapping

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `JoinParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamSource[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName.Get[F, N]) :get.T[get.J] =
			get.apply(thisClause).mapping

	}



	/** A facade to a `FromClause` of type `F`, providing access to all joined relations as
	  * [[net.noresttherein.oldsql.sql.MappingSQL.JoinedRelation JoinedRelation]] SQL expressions
	  * which can be directly, or through its components/columns, used in any SQL expressions based on this clause.
	  */
	class JoinedRelations[F <: FromClause](private val thisClause :F) extends AnyVal {

		/** Returns the `JoinedRelation` instance for the last relation with type `E` as the mapping subject. */
		def of[E](implicit get :BySubject.Get[F, E]) :JoinedRelation[get.J, get.T] = get(thisClause)

		/** Returns the `JoinedRelation` instance for the last relation using the provided mapping type.
		  * @tparam M a `Mapping` type constructor accepting the `Origin` type.
		  */
		def apply[M[O] <: MappingAt[O]](implicit get :ByTypeConstructor.Get[F, M]) :JoinedRelation[get.J, M] =
			get(thisClause)

		/** Returns the `JoinedRelation` instance for the last relation using a `LabeledMapping` with label type `A`.
		  * This in particular includes relations aliased using the [[net.noresttherein.oldsql.sql.Join.as Join.as]]
		  * method.
		  * @param alias a string literal which is used as the label of the accessed mapping
		  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping]]
		  */
		def apply[A <: String with Singleton]
		         (alias :A)(implicit get :ByLabel.Get[F, A]) :JoinedRelation[get.J, get.T] =
			get(thisClause)


		/** Returns the `JoinedRelation` instance for the relation at the specified index in this clause.
		  * If the index is non-negative, it is the `n-1`-th relation when counting from the left;
		  * if the index is negative, it is the `-n`-th relation when counting from the right.
		  * In case of non-negative indexes, the clause type `F` must be complete, i.e. the full number of its relations
		  * must be known. If the index is negative, it suffices that at least `-n` last joins are known.
		  * The relations are indexed on the type level, thus the index must be an `Int` literal, not just any
		  * `Int` singleton type.
		  * @param n an `Int` literal to use as the index.
		  */
		def apply[N <: Numeral](n :N)(implicit get :GetTableByIndex[F, N]) :JoinedRelation[get.J, get.T] =
			get(thisClause)

		/** Returns the `JoinedRelation` instance for the last relation in this clause (counting from the right). */
		def last(implicit get :GetTableByNegativeIndex[F, -1]) :JoinedRelation[get.J, get.T] = get(thisClause)

		/** Returns the `JoinedRelation` instance for the second-last relation in this clause (counting from the right). */
		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :JoinedRelation[get.J, get.T] = get(thisClause)



		//todo: ByParamIndex
		/** Returns the `Mapping` instance for the last clause parameter of type `X`. */
		def ?[X](implicit get :ByTypeConstructor.Get[F, ParamSource[X]#Row]) :JoinedRelation[get.J, get.T] =
			get(thisClause)

		/** Returns the `Mapping` instance for the last clause parameter with name `name` as its label.
		  * This takes into account only `JoinParam (N ?: _)#T` joins, that is with the name listed as
		  *  a mapping label in its type, not the actual parameter names which might have been given to
		  *  standard `ParamSource[X]` instances.
		  */
		def ?[N <: Label](name :N)(implicit get :ByParamName.Get[F, N]) :JoinedRelation[get.J, get.T] =
			get(thisClause)

	}






	@implicitNotFound("Can't calculate the size of the clause ${F}. Either the FROM clause is incomplete " +
		              "or the expected number ${N} is incorrect. Missing implicit: FromClauseSize[${F}, ${N}].")
	class FromClauseSize[-F <: FromClause, N <: Numeral] private(private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object FromClauseSize {
		implicit val DualCount :FromClauseSize[Dual, 0] = new FromClauseSize[Dual, 0](0)

		implicit def moreTables[L <: FromClause, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                       (implicit count :FromClauseSize[L, M], plus :Inc[M, N]) :FromClauseSize[L With R, N] =
			new FromClauseSize[L With R, N](plus.n)
	}



	@implicitNotFound("Failed to count the tables in the clause ${F}. Is ${N} the number of mappings listed " +
	                  "in its definition? Note that witness TableCount[F, N] is invariant in type F, " +
	                  "but requires that it starts with either FromClause or Dual (including From[_]).")
	class TableCount[F <: FromClause, N <: Numeral] private[FromClause] (private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object TableCount {
		implicit val fromClauseHasZero :TableCount[FromClause, 0] = new TableCount[FromClause, 0](0)
		implicit val dualHasZero :TableCount[Dual, 0] = new TableCount[Dual, 0](0)

		implicit def forOneTableMore[F <: FromClause, L <: FromClause, R[O] <: MappingAt[O], M <: Numeral, N <: Numeral]
		                            (implicit split :Conforms[F, F, L With R], count :TableCount[L, M], inc :Inc[M, N])
				:TableCount[F, N] =
			new TableCount[F, N](inc.n)
	}



	@implicitNotFound("Relation mapping ${M} is not the first known mapping of the FROM clause ${F}. " +
	                  "An implicit TableShift[${F}, ${M}, ${N}] will only exist if the clause F starts with either" +
	                  "FromClause or Dual joined with the mapping M. Note that TableShift is invariant both " +
	                  "in its F <: FromClause and M[O] <: MappingAt[O] type parameters.")
	class TableShift[F <: FromClause, M[O] <: MappingAt[O], N <: Numeral](private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object TableShift {
		implicit def joinedWithDual[F <: FromClause, R[A] <: MappingAt[A]]
		                           (implicit split :Conforms[F, F, Dual With R]) :TableShift[F, R, 0] =
			new TableShift[F, R, 0](0)

		implicit def joinedWithFromClause[F <: FromClause, R[A] <: MappingAt[A]]
		                                 (implicit split :Conforms[F, F, FromClause With R]) :TableShift[F, R, 0] =
			new TableShift[F, R, 0](0)

		implicit def joinedEarlier[F <: FromClause, L <: FromClause, R[A] <: MappingAt[A],
		                           T[A] <: MappingAt[A], M <: Numeral, N <: Numeral]
		                          (implicit split :Conforms[F, F, L With R], prev :TableShift[L, T, M], inc :Inc[M, N])
				:TableShift[F, T, N] =
			new TableShift[F, T, N](inc.n)
	}






	/** Implicit resolution of the `N`-th relation in the ''from'' clause `F`. This works both for positive numbers,
	  * indexed from zero and going from left to right (in which case `F` must be complete), and negative -
	  * indexed from `-1` and going from right to left (which is available always, but which index changes with
	  * joining new tables).
	  * @tparam F the input `FromClause`.
	  * @tparam N index of the desired relation as a literal `Int` type.
	  */
	@implicitNotFound("Cannot get ${N}-th relation of ${F}. \n" +
		              "Either ${N} >= size (where size is the number of relations in the FROM clause),\n" +
		              "or -(${N}) is greater the number of relations in the instantiated suffix of the FROM clause,\n" +
		              "or ${N} >= 0 and the size is not known (the clause type starts with FromClause and not Dual/From.")
	sealed abstract class GetTableByIndex[-F <: FromClause, N <: Numeral] {
		/** The ''negative'' index of the found relation: that's `-1` for the last relation in the clause
		  * and decreases going left. */
		type I <: Numeral

		type J >: F <: FromClause

		/** The mapping type of the relation at index `N`. */
		type T[O] <: MappingAt[O]

		val stretch :FromClause With T PrefixOf J

		implicit def count :TableCount[J, _ <: Numeral] = new TableCount[J, stretch.suffix.type](stretch.suffix)


		/** Getter for the relation from the input `FromClause`. */
		def apply(from :F) :JoinedRelation[J, T] = table(from).extend(stretch)

		def table(from :F) :JoinedRelation[FromClause With T, T]
	}



	object GetTableByIndex {

		@implicitNotFound("Can't get the relation at the (negative) index ${N} in ${F}. " +
			              "Either ${N} >= 0 or -(${N}) is greater than the number of known tables in the FROM clause.")
		sealed abstract class GetTableByNegativeIndex[-F <: FromClause, N <: Numeral] extends GetTableByIndex[F, N] {
			override type I = N
		}


		implicit def lastNegative[M[O] <: MappingAt[O]]
				:GetTableByNegativeIndex[FromClause With M, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new GetTableByNegativeIndex[FromClause With M, -1] {
				override type T[O] = M[O]
				type J = FromClause With M

				override val stretch = implicitly[J PrefixOf J]

				override def table(from :FromClause With M) :JoinedRelation[FromClause With M, M] = from.last
			}


		implicit def previousNegative[L <: FromClause, R[O] <: MappingAt[O], N <: Numeral, M <: Numeral]
		                             (implicit minus :Inc[N, M], get :GetTableByNegativeIndex[L, M])
				:GetTableByNegativeIndex[L With R, N] { type T[O] = get.T[O]; type J = get.J With R } =
			new GetTableByNegativeIndex[L With R, N] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override def table(from :L With R) :JoinedRelation[FromClause With T, T] = get.table(from.left)
			}



		@implicitNotFound("Can't get relation at the (non-negative) index ${N} in ${F}. Either ${N} < 0, " +
		                  "or ${N} is greater or equal than the number of relations in the FROM clause, " +
		                  "or the exact number of relations in the FROM clause is not known.")
		sealed abstract class GetTableByPositiveIndex[-F <: FromClause, N <: Numeral, M <: Numeral] extends GetTableByIndex[F, N] {
			type I = M
		}

		implicit def lastPositive[L <: FromClause, R[O] <: MappingAt[O], N <: Numeral]
		                         (implicit count :FromClauseSize[L, N])
				:GetTableByPositiveIndex[L With R, N, -1] { type T[O] = R[O]; type J = FromClause With R } =
			new GetTableByPositiveIndex[L With R, N, -1] {
				override type T[O] = R[O]
				override type J = FromClause With R

				override val stretch = implicitly[J PrefixOf J]

				override def table(from :L With R) = from.last
			}

		implicit def previousPositive[L <: FromClause, R[O] <: MappingAt[O], N <: Numeral, I <: Numeral, J <: Numeral]
		                             (implicit get :GetTableByPositiveIndex[L, N, J], minus :Inc[I, J])
				:GetTableByPositiveIndex[L With R, N, I] { type T[O] = get.T[O]; type J = get.J With R } =
			new GetTableByPositiveIndex[L With R, N, I] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override def table(from :L With R) = get.table(from.left)
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
		  * is returned as the member type `T[O]`. In other words, an implicit value `found :Found[F, X, I]` witnesses
		  * that `found.T` is the mapping of the last relation (rightmost) in the clause `F` for which an implicit
		  * `Predicate[T, X]` exists, with `I` being the ''negative'' index of the mapping (starting with `-1`
		  * for the last mapping and decreasing).
		  */
		@implicitNotFound("Cannot find a mapping for key type ${X} in the clause ${F}.")
		sealed abstract class Found[-F <: FromClause, X, I <: Numeral] {
			/** The accessed `Mapping` type. */
			type T[O] <: MappingAt[O]

			type J >: F <: FromClause

			/** The negative index of the relation. */
			def shift :I

			val stretch :FromClause With T PrefixOf J

			/** Getter for the matching relation. */
			def apply(from :F) :JoinedRelation[J, T] = table(from).extend(stretch)

			def table(from :F) :JoinedRelation[FromClause With T, T]
		}

		implicit def last[M[O] <: MappingAt[O], X](implicit pred :Predicate[M, X])
				:Found[FromClause With M, X, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new Found[FromClause With M, X, -1] {
				override type T[O] = M[O]
				override type J = FromClause With M

				override val stretch = implicitly[J PrefixOf J]

				override def shift = -1 : -1

				override def table(from :FromClause With M) = from.last
			}

		implicit def previous[L <: FromClause, R[O] <: MappingAt[O], X, I <: Numeral, J <: Numeral]
		                     (implicit get :Found[L, X, J], minus :Inc[I, J])
				:Found[L With R, X, I] { type T[O] = get.T[O]; type J = get.J With R } =
			new Found[L With R, X, I] {
				override type T[O] = get.T[O]
				override type J = get.J With R

				override val stretch = get.stretch.stretch[R]

				override val shift = minus.m

				override def table(from :L With R) = get.table(from.left)
			}



		/** An implicit result of the resolution of search for a relation matching the key type `X`
		  * in the FROM clause `F`.
		  * @tparam F the input `FromClause` from which the relation is taken.
		  * @tparam X the 'key' type used to match the `Mapping` types of all relations in search for an implicit
		  *           `Predicate[M[Any], X]`.
		  */
		abstract class BaseGet[-F <: FromClause, X] {
			/* Derived from some implicit `Found[F, X, I]`. This duplication
		     * is required because the index `I` needs to be a type parameter for the type-level arithmetic to work
		     * and the accessor methods accepting `Get` do not declare 'output' types. Additionally, different
		     * accessors with the same signature but for different `GetTableByPredicate` instance will lead to conflicts
		     * after erasure, so subclasses introduce their own `Get` subclass to prevent it.
			 */

			/** The ''negative'' index of the accessed relation, starting with `-1` for the rightmost relation in `F`.*/
			type I <: Numeral

			/** The last mapping type matching `X` in `F`. */
			type T[O] <: MappingAt[O]

			/** A supertype of the ''from'' clause owning the table starting with `FromClause With T` and upcasting
			  * all following joins to `With`. */
			type J >: F <: FromClause

			/** The negative index of the found relation, starting with `-1` and decreasing from right to left. */
			def shift :I

			/** Returns the found relation from the input `FromClause`. */
			def apply(from :F) :JoinedRelation[J, T]
		}

	}



	object GetTableByPredicate {

		/** Accesses relations in a `FromClause` based on their `Subject` member type.
		  * Implicit `BySubject.Get[F, S]` returns the rightmost relation with the subject type `S`
		  */
		object BySubject extends GetTableByPredicate {
			implicit def satisfies[M[A] <: RefinedMapping[S, A], S] :Predicate[M, S] = report[M, S]

			/** An implicit accessor object for the last relation in `F` with `Subject` type `S`.
			  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
			@implicitNotFound("No relation with Subject type ${S} in the from clause ${F}:\n" +
			                  "no implicit value for BySubject.Get[${F}, ${S}].")
			sealed abstract class Get[-F <: FromClause, S] extends BaseGet[F, S]

			implicit def Get[F <: FromClause, S, N <: Numeral]
			                (implicit found :Found[F, S, N] { type T[O] <: RefinedMapping[S, O] })
					:Get[F, S] { type T[O] = found.T[O]; type J = found.J; type I = N } =
				new Get[F, S] {
					override type I = N
					override type J = found.J
					override type T[O] = found.T[O]

					override def shift = found.shift
					override def apply(from :F) :JoinedRelation[J, T] = found(from)
				}

		}



		/** Accesses relations in a `FromClause` based on their `Label` (that is, the `Label` type parameter
		  * of the `LabeledMapping` used for the relation. Implicit ByLabel.Get[F, L] returns the last relation
		  * with label `L` in `F`. */
		object ByLabel extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledMapping[L, _, A], L <: Label] :Predicate[M, L] = report[M, L]

			/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledMapping[N, _, _]`. */
			@implicitNotFound("No relation with alias type ${N} in the from clause ${F}:\n" +
			                  "no implicit value for ByLabel.Get[${F}, ${N}].")
			sealed abstract class Get[-F <: FromClause, N <: Label] extends BaseGet[F, N] {
				override type T[O] <: LabeledMapping[N, _, O]
			}

			implicit def Get[F <: FromClause, A <: Label, N <: Numeral]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledMapping[A, _, O] })
					:Get[F, A] { type T[O] = found.T[O]; type J = found.J; type I = N } =
				new Get[F, A] {
					override type I = N
					override type T[O] = found.T[O]
					override type J = found.J

					override def shift = found.shift
					override def apply(from :F) :JoinedRelation[J, T] = found(from)
				}
		}



		/** Accesses relations in a `FromClause` based on the type constructor for the mapping accepting
		  * the `Origin` type. The provided type constructor may be for a supertype of the actual mapping.
		  * While the input to match accepts only a single parameter, it is possible to match mappings with multiple
		  * parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
		  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`. */
		object ByTypeConstructor extends GetTableByPredicate {
			implicit def satisfies[M[A] <: MappingAt[A]] :Predicate[M, M[Any]] = report[M, M[Any]]

			/** An implicit accessor object for the last relation in `F` with `Mapping` type `M[_]`.
			  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
			@implicitNotFound("No relation with type constructor ${M} in the from clause ${F}:\n" +
			                  "no implicit value for ByTypeConstructor.Get[${F}, ${M}].")
			sealed abstract class Get[-F <: FromClause, M[O] <: MappingAt[O]] {
				type I <: Numeral
				/** A unique origin type with the negative index of the relation encoded in it. */
				type T[O] = M[O]
				type J >: F <: FromClause

				def apply(from :F) :JoinedRelation[J, M]
				def shift :I
			}

			implicit def Get[F <: FromClause, M[O] <: MappingAt[O], N <: Numeral]
			                (implicit found :Found[F, M[Any], N] { type T[O] = M[O] })
					:Get[F, M] { type J = found.J; type I = N } =
				new Get[F, M] {
					override type I = N
					override type J = found.J
					override def shift = found.shift
					override def apply(from :F) = found(from)
				}

		}



		/** Accesses parameters of a `FromClause` based on the given parameter name used as their label.
		  * Implicit ByParamName.Get[F, N] returns the last relation for the synthetic parameter mapping
		  * `LabeledFromParam[N, X, O]` in `F`.
		  */
		object ByParamName extends GetTableByPredicate {
			implicit def satisfies[M[A] <: LabeledFromParam[N, _, A], N <: Label] :Predicate[M, N] = report[M, N]

			/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledFromParam[A, _, _]`. */
			@implicitNotFound("No parameter with name type ${A} in the from clause ${F}:\n" +
                              "no implicit value for ByParamName.Get[${F}, ${A}].")
			sealed abstract class Get[-F <: FromClause, A <: Label] extends BaseGet[F, A] {
				type T[O] <: LabeledFromParam[A, _, O]
			}

			implicit def Get[F <: FromClause, A <: Label, N <: Numeral]
			                (implicit found :Found[F, A, N] { type T[O] <: LabeledFromParam[A, _, O] })
					:Get[F, A] { type T[O] = found.T[O]; type J = found.J; type I = N } =
				new Get[F, A] {
					override type I = N
					override type T[O] = found.T[O]
					override type J = found.J

					override def shift = found.shift
					override def apply(from :F) :JoinedRelation[J, T] = found(from)
				}

		}

	}






	@implicitNotFound("Cannot apply FROM clause ${F} to parameters ${P}. I cannot prove that the parameter chain P " +
	                  "is a subtype of F#Params - most likely F is incomplete or contains With joins.\n"+
	                  "Missing implicit value ApplyJoinParams[${F}, ${P}, ${U}].")
	sealed abstract class ApplyJoinParams[-F <: FromClause, -P <: Chain, +U <: ParameterlessFrom] {
		def apply(from :F, params :P) :U
	}



	object ApplyJoinParams {
		private[this] val none = new ApplyJoinParams[ParameterlessFrom, @~, ParameterlessFrom] {
			override def apply(from :ParameterlessFrom, params: @~) = from
		}


		implicit def unparameterized[F <: ParameterlessFrom] :ApplyJoinParams[F, @~, F] =
			none.asInstanceOf[ApplyJoinParams[F, @~, F]]


		implicit def skipNonParam[L <: FromClause, J[A <: FromClause, B[O] <: MappingAt[O]] <: A Join B,
		                          R[A] <: MappingAt[A], P <: Chain, U <: ParameterlessFrom]
		                         (implicit prev :ApplyJoinParams[L, P, U])
				:ApplyJoinParams[L J R, P, J[L, R]#WithLeft[U]] =
			new ApplyJoinParams[L J R, P, (L J R)#WithLeft[U]] {
				override def apply(from :J[L, R], params :P) =
					if (params.isEmpty)
						from.asInstanceOf[J[L, R]#WithLeft[U]]
					else {
						val left = prev(from.left, params)
						val unfiltered = from.withLeft[left.type](left)(True)
						val generalized = from.generalized
						val substitute = new RemoveParams(
							generalized, unfiltered.generalized)(params.asInstanceOf[generalized.Params]
                        )
						from.withLeft(left)(substitute(from.condition))
					}
			}


		implicit def applyParam[L <: FromClause, R[A] <: FromParam[X, A], X, I <: Chain, U <: ParameterlessFrom]
		                       (implicit prev :ApplyJoinParams[L, I, U]) :ApplyJoinParams[L JoinParam R, I ~ X, U] =
			new ApplyJoinParams[L JoinParam R, I ~ X, U] {
				override def apply(from :JoinParam[L, R], params :I ~ X) = {
					val res = prev(from.left, params.init)
					val generalized = from.generalized
					val substitute = new RemoveParams(generalized, res.generalized)(params.asInstanceOf[generalized.Params])
					(res withCondition substitute(from.condition)).asInstanceOf[U]
				}
			}

	}






	/** Proof that the ''from'' clause `S` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `S`. It means that `S &lt;: F With T1 ... With TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `S` containing all the relations of `F` unless `F` is complete.
	  * A non-obvious implication of being contravariant in the extending type `S` is that if `F` is incomplete,
	  * this instance is also a witness that subtypes of `S` which replace the initial `FromClause` with a join list
	  * are likewise extensions of `F`, in this way covering both extensions from the back (right side) and front
	  * (left side). The purpose of this class is to allow `SQLExpression` instances based on one clause `F` be converted
	  * into isomorphic expressions based on a second clause `S`, as long as all mappings in `F`'s static type
	  * form a continuous subsequence of mappings listed in `S`. Note that `SQLExpression` is contravariant in
	  * its clause type parameter, meaning that an abstract `FromClause` occurring at the start of the from clause
	  * type parameter doesn't 'hide' any mappings used by that expression, but to the contrary, serves to signify that
	  * it doesn't rely in any form on that portion of the clause it is based on. As a result, this proof
	  * is indeed a valid representation that such a conversion from `F` to `S` is possible for any `SQLExpression`.
	  * Due to this contravariance in `S`, this isn't any form of a generalized subtyping relation
	  * and should be relied upon only in the context of the actual extension.
	  */
	@implicitNotFound("FromClause ${F} is not a prefix of the clause ${S} (ignoring join kinds).")
	class ExtendedBy[+F <: FromClause, -S <: FromClause] private[FromClause] (val length :Int) extends AnyVal {
		/** A transitive proof that a clause extending `S` with a single relation (mapping) also extends `F`. */
		@inline
		def stretch[R[O] <: MappingAt[O]] :F ExtendedBy (S With R) = new ExtendedBy(length + 1)

		/** A transitive proof that a clause extended by `F` with a single relation(mapping) is also extended by `S`. */
		@inline
		def stretchFront[L <: FromClause, R[O] <: MappingAt[O]](implicit front :F <:< (L With R)) :L ExtendedBy S =
			new ExtendedBy(length + 1)

		@inline
		def stretchFront[C <: FromClause](implicit front :C ExtendedBy F) :C ExtendedBy S =
			new ExtendedBy(front.length + length)

		@inline
		def stretch[C <: FromClause](implicit next :S ExtendedBy C) :F ExtendedBy C =
			new ExtendedBy[F, C](length + next.length)

		@inline
		def shrink[E >: F <: FromClause, C <: FromClause, T <: S]
		          (implicit prefix :E ExtendedBy T, suffix :C ExtendedBy T) :F ExtendedBy C =
			new ExtendedBy[F, C](length - suffix.length)

		@inline
		def shrinkFront[E >: F <: FromClause, C <: FromClause, T <: S]
		               (implicit prefix :E ExtendedBy C, suffix :C ExtendedBy T) :C ExtendedBy S =
			new ExtendedBy(length - prefix.length)
	}



	object ExtendedBy {
		private[this] val instance = new ExtendedBy[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def extend[S <: FromClause, L <: FromClause, R[A] <: MappingAt[A]]
		                   (implicit ev :S ExtendedBy L) :S ExtendedBy (L With R) =
			new ExtendedBy(ev.length + 1)
	}






	/** Proof that the ''from'' clause `S` is an extension of the clause `F` / the clause `F` is a prefix
	  * of the clause of `S`. It means that `S =:= F With T1 ... With TN forSome { type T1 ... TN }`.
	  * This takes into account only the static type of both clauses and the actual mapping lists on both can
	  * differ and be of different lengths if `F` is not a complete clause and has an abstract prefix.
	  * For this reason this class should be in general relied upon only in the context of the actual extension,
	  * rather than a proof of `S` containing all the relations of `F` unless `F` is complete.
	  */
	@implicitNotFound("FromClause ${F} is not a prefix of the clause ${S}.")
	class PrefixOf[F <: FromClause, S <: FromClause] private[FromClause] (val suffix :Int) extends AnyVal {

		@inline def extension :F ExtendedBy S = new ExtendedBy(suffix)

		/** A transitive proof that a clause extending `S` with a single relation (mapping) also extends `F`. */
		@inline
		def stretch[R[O] <: MappingAt[O]] :F PrefixOf (S With R) = new PrefixOf(suffix + 1)

		@inline
		def stretch[C <: FromClause](implicit next :S PrefixOf C) :F PrefixOf C =
			new PrefixOf[F, C](suffix + next.suffix)

		@inline
		def shrink[C <: FromClause](implicit prefix :F PrefixOf S, suffix :C PrefixOf S) :F PrefixOf C =
			new PrefixOf[F, C](this.suffix - suffix.suffix)

		@inline
		def shrinkFront[C <: FromClause](implicit prefix :F PrefixOf C, suffix :C PrefixOf S) :C PrefixOf S =
			new PrefixOf(this.suffix - prefix.suffix)
	}



	object PrefixOf {
		private[this] val instance = new PrefixOf[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :PrefixOf[F, F] = instance.asInstanceOf[F PrefixOf F]

		implicit def extend[F <: FromClause, L <: FromClause,
		                    J[A <: FromClause, B[O] <: MappingAt[O]] <: A With B, R[A] <: MappingAt[A]]
		                   (implicit ev :F PrefixOf L) :F PrefixOf (L J R) =
			new PrefixOf[F, L J R](ev.suffix + 1)
	}

}


