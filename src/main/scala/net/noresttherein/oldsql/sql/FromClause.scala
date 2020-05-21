package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema.{ColumnMapping, RowSource, SQLForm, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.RowSource.NamedSource
import net.noresttherein.oldsql.schema.bits.{ChainMapping, LabeledMapping}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.GetTableByIndex.GetTableByNegativeIndex
import net.noresttherein.oldsql.sql.FromClause.GetTableByPredicate.{ByLabel, BySubject, ByTypeConstructor}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, SubselectOf}
import net.noresttherein.oldsql.sql.MappingFormula.{FreeColumn, JoinedRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple
import net.noresttherein.oldsql.sql.JoinParam.{ParamSource, WithParam}
import net.noresttherein.oldsql.sql.SQLTerm.True
import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingFormula.TypedJoinedRelation.LastRelation





/** In its basic use, a `FromClause` is a representation of the ''FROM'' and ''WHERE'' clauses in an SQL ''SELECT''
  * statement, declaring the relations taking part in a query. More generally, it is the domain over which SQL
  * expressions (instances of the [[net.noresttherein.oldsql.sql.SQLFormula SQLFormula]] class hierarchy) are defined,
  * providing all non-constant values available to them. It consists of a a list of `Mapping`s, together with an optional
  * filter working on those mappings, especially any required join conditions. While the individual elements of a clause
  * are referred to often as tables for simplicity, they can not only be arbitrary relations such as other ''selects'',
  * but also synthetic artifacts such as query parameters. It is even possible to use arbitrary mapping components,
  * to be replaced at a later time with references to concrete tables, parameters or constants. As such, it
  * doesn't necessarily represent a valid fragment of a select from the application's schema.
  * In that case it's best thought as a signature containing declarations of terms (mappings and parameters)
  * over which SQL formulas can be defined; hence `SQLFormula` is parameterized with a `FromClause`.
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
  * that is all mapping types participating in the join/clause are known. The opposite is an ''incomplete'' clause,
  * the definition of which starts with the base `FromClause` type: `FromClause Join Deaths Join Instruments`,
  * which specifies any sequence of relations ending with the `Deaths` and `Instruments`. Note that this is not the same
  * as concrete/fully instantiated types, as a clause can contain abstract types as relation mappings
  * and still be complete.
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

	/** Type of the last mapping in this join (the rightmost one) if not empty. */
	type LastMapping[O] <: MappingFrom[O]

	/** Table alias proxy for the last table (or table-like expression) in this list as seen from some `FromClause`
	  * type `F` containing this instance in its 'tail'. In other words, this projects the type of the last element
	  * of this clause to an extending row source.
	  */
	type LastTable[F <: FromClause] <: JoinedRelation[F, LastMapping]

	/** The supertype of this instance containing only the last relation mapping joined with `FromClause` or `Dual`. */
	type FromLast >: this.type <: FromClause

	/** Last relation in this clause when treated as a list, if any. */
	def last :LastTable[FromLast]



	/** Number of relations contained in this join (counting all their occurrences separately). */
	def size :Int

	/** Number of relations contained in the explicit portion of this subselect join. This is equal to
	  * the number of mappings to the right of the rightmost `Subselect` join or `Dual`, that is `size - outer.size`.
	  */
	def subselectSize :Int = size - outer.size



	/** Self type of this clause used by some copy constructors present in the subclasses. */
	type This >: this.type <: FromLast



	/** The type of this clause with all join kinds replaced with the root type `With` and initial `Dual`
	  * with `FromClause`. It forms the class of abstraction of clauses with the same relations (and in the same order),
	  * which are considered equivalent for many purposes, in particular as the basis for SQL expressions `SQLFormula`.
	  * Most `SQLFormula` instances returned by this clause are parameterized with this type.
	  */
	type Generalized <: FromLast { type Generalized <: thisClause.Generalized }

	/** This clause upcast to the generalized form in which all join kinds are replaced with `With`. */
	def generalized :Generalized


	/** A join of relations from the clause `P` with relations of this clause. This is the type resulting
	  * from substituting `Dual` in this type's signature with `P`, that is appending all mappings from this clause
	  * in order to `P`, using the same join kinds. The join type `J` is used as the join between the last relation
	  * in `P` and the first relation in `this`.
	  */ //todo: try to make it conform to Generalized
	type JoinedWith[+P <: FromClause, +J[+L <: FromClause, R[O] <: MappingFrom[O]] <: L Join R] <: Generalized

	/** An intermediate used in definition of `JoinedWith` by join classes. Similar to `JoinedWith`, it is a cross join
	  * between a prefix clause `F`, this clause, and mapping `T`. The last join with `T` becomes either the join `J`,
	  * if this clause contains no mappings itself (i.e. the result becomes `J[F, T]`), or `N` if this clause is non
	  * empty, resulting in `N[,JoinedWith[F, J], T]`.
	  */
	type ExtendJoinedWith[+F <: FromClause, +J[+L <: FromClause, R[O] <: MappingFrom[O]] <: L Join R,
	                      +N[+L <: FromClause, R[O] <: MappingFrom[O]] <: L Join R, T[O] <: MappingFrom[O]] <:
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
	def extendJoinedWith[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                    (prefix :F, firstJoin :Join.*, nextJoin :this.type Join T)
			:ExtendJoinedWith[F, firstJoin.LikeJoin, nextJoin.LikeJoin, T]


	/** Type of the outer source if this source represents a subselect source created by `Outer.subselect()`.
	  * All 'proper' `With` subclasses have this type equal to the `Outer` of their left side, but `Subselect`
	  * defines `Outer` as the generalized type of its left side. Additionally, all 'proper' joins conform
	  * to `AsSubselectOf[L#Outer]` and `L Subselect R` conforms to `AsSubselectOf[L]`. This means that
	  * for any concrete class `S &lt;: FromClause` with fully instantiated parameters (the clause is complete,
	  * that is all relations in `S` and kinds of joins in it are known) value `(s :S) from t1 join t2 ... join t3`
	  * conforms to `AsSubselectOf[S]`. This way we can statically express a dependency relationship between
	  * ''from'' clauses without resorting to implicit evidence.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#AsSubselectOf]]
	  */
	type Outer <: FromClause

	/** Return the outer clause of this instance if it (or, recursively, any clause in the left side of the join)
	  * was created by calling `outer.subselect()`. When viewed as a list of relations, `outer` constitutes the result
	  * of dropping all relations joined in this instance up until and including a relation joined
	  * by a `.subselect()` call. If there's no `Subselect` in the dynamic type definition of this source, meaning
	  * this is not a subselect clause (''from'' clause of resulting selects will include all members of this clause),
	  * `Dual` instance used as the relation list terminator is returned.
	  * @return `outer` of the left side or just the left side if this instance is a `Subselect`.
	  */
	def outer :Outer



	/** A type constructor appending all relations joined since the last (rightmost) `Subselect` join kind to the
	  * given clause `F`. For any complete clause `J &lt;: FromClause`, `J#AsSubselectOf[J#Outer]` consists of all
	  * relations from `J` in order joined with the abstract `FromClause` using the same join kinds. Additionally,
	  * `J &lt;:&lt; J#AsSubselectOf[J#Outer]` and `J#AsSubselectOf[J#Outer] &lt;:&lt; J#Generalized`.
	  * @see [[[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	type AsSubselectOf[F <: FromClause] <: FromLast

	/** For subselect clauses - that is subtypes with a `Subselect` join kind occurring somewhere in their definition,
	  * not necessarily `Subselect` instances - it represents them as a subselect of a clause `F`, being
	  * an extension of their outer clause (the left side of the right-most `Subselect` join). In syntactic terms,
	  * it replaces the `Outer` type in this type's definition with type `F`. Procedurally, it joins in order
	  * all relations since the last occurrence of a `Subselect` join, forming the explicit ''from'' clause of
	  * the subselect, with the new outer clause `F`, preserving all join conditions. If this clause is not a
	  * subselect clause, this method has simply the effect of performing a cartesian join of the relations
	  * listed in `this` with the relations listed in `outer` (preserving join conditions within both, but without
	  * any additional filtering).
	  */
	def asSubselectOf[F <: FromClause](outer :F)(implicit extension :Outer ExtendedBy F) :AsSubselectOf[F]



	/** Subject types of all mappings in this clause, concatenated into a heterogeneous list.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	type Row <: Chain

	/** Create an SQL tuple formula containing `JoinedRelation` formulas for all joined elements in their order
	  * of appearance. It will contain entries for all mappings in this clause, including parameter mappings
	  * and mappings listed in this clause's `Outer` prefix (if this clause is a subselect clause).
	  */
	def row :ChainTuple[Generalized, Row] = row(generalized)

	/** Create an SQL tuple formula, based on some extending clause `E`, containing `JoinedRelation` formulas 
	  * for all joined elements in their order of appearance. It will contain entries for all mappings in this clause,
	  * including parameter mappings and mappings listed in this clause's `Outer` prefix (if this clause 
	  * is a subselect clause. This overloaded variant is used by the zero-argument `row` to obtain the chain prefix
	  * containing the relation formulas based on the final clause type from a prefix clause of a join.
	  */
	def row[E <: FromClause](target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, Row]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def tableStack :LazyList[JoinedRelation.AnyIn[Generalized]] = tableStack(generalized)

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance.
	  * The `JoinedFormula`s are based on some extending clause `E`, so that the stack for a prefix clause
	  * can be used as-is by extending clause's zero argument `tableStack`.
	  */
	def tableStack[E <: FromClause]
	              (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]]


	/** Subject types of all mappings in this clause following the `Outer` prefix, concatenated into a heterogeneous list.
	  * This amounts to the list of values mapped by the relations from the ''from'' clause of the most deeply nested
	  * subselect. If this clause doesn't represent a subselect, but a top-level query, it is the same as `Row`.
	  * The chain contains the mapped types in the same order as their mappings appear in this type's definition
	  * and is, like `With` (but unlike `::`), left associative.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#subselectRow]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Row]]
	  */
	type SubselectRow <: Chain

	/** Create an SQL tuple formula containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. This includes all relations in this clause following
	  * the most recent `subselect` 'join', marking the first relation following the `Outer` clause prefix.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#Outer]]
	  */
	def subselectRow :ChainTuple[Generalized, SubselectRow] = subselectRow(generalized)

	/** Create an SQL tuple formula containing `JoinedRelation` formulas for all joined elements of the most deeply
	  * nested subselect clause, in their order of appearance. The formulas are based on some extending clause `E`,
	  * so they can be used by the zero-argument `subselectRow` as the chain prefix of its result. 
	  */
	def subselectRow[E <: FromClause](target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow]

	/** All relations in this clause as generic, untyped mappings, in the reverse order of their appearance, ending
	  * with the first relation following the `Outer` prefix. If this is not a subselect clause (no `Subselect` 'joins'
	  * are present in this clause and `Outer =:= FromClause`), all relations are included.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#row]]
	  */
	def subselectTableStack :LazyList[JoinedRelation.AnyIn[Generalized]] = subselectTableStack(generalized)

	/** All relations in this clause in the reverse order, ending with the last (right-most) appearance of a `Subselect`
	  * 'join' or `Dual`. The elements are returned as `JoinedRelation`s for generic, untyped mappings, 
	  * based on some extending clause `E`. Used by the zero-argument `subselectTableStack`
	  * to request the tail of the stack with formulas of the correct type from the prefix clause.
	  */
	def subselectTableStack[E <: FromClause]
	                       (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]]






	/** The ''WHERE'' part of this clause representing the filter condition as SQL AST.
	  * It is the conjunction of join conditions for all joins in this clause.
	  */
	def filter :BooleanFormula[Generalized] = filter(generalized)

	/** The combined join conditions of all joins in this clause as a formula based on an extending clause.
	  * Used by zero-argument `filter` to request the individual join conditions as formulas based on the clause
	  * it was called for.
	  */
	def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :BooleanFormula[E]

	def subselectFilter :BooleanFormula[Generalized] = subselectFilter(generalized)

	def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :BooleanFormula[E]


	/** Function type returning a join condition between the last relation in this clause and a following mapping `T`.
	  * Used as the parameter for [[net.noresttherein.oldsql.sql.With#on With.on]], it is declared here
	  * (on the 'left side' of a new join) to ensure that `on` won't be possible to call if the left side of
	  * a `With` clause is abstract or empty, i.e. that the clause contains at least two relations and their
	  * types are known.
	  */
	type JoinFilter[T[O] <: MappingFrom[O]] <:
		(JoinedRelation[FromLast With T, LastMapping], JoinedRelation[FromClause With T, T])
			=> BooleanFormula[FromLast With T]






	/** Creates an `InnerJoin` of `this` and `right`. Polymorphic as a join with `Dual` results in `From`, extracted
	  * to a method to eliminate casting in pattern matching.
	  */
	protected[sql] def selfInnerJoin[T[A] <: TypedMapping[X, A], X]
	                                (right :LastRelation[T, X])(filter :BooleanFormula[Generalized With T])
			:this.type InnerJoin T =
		InnerJoin.newJoin[T, X](this, right)(filter)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[FromClause]


}






object FromClause {
	
	/** An upper bound for `FromClause` subtypes representing ''from'' clauses of subselects of a select
	  * with the ''from'' clause `F`. `S &lt;: AsSubselectOf[F]` if and only if `S` has the form of
	  * `F Subselect M1 J2 M2 ... JN MN` for some mappings `M1...MN` and non-subselect join types `J2...JN`,
	  * and both types are complete clauses. Clauses conforming to `AsSubselectOf[F]` can use all the mappings/tables
	  * which are a part of `F`, but they are not a part of any select formulas created from that source. This allows
	  * the use of nested select queries which depend on values from the ''from'' clause of the outer select.
	  * Rather counterintuitively, this type is contravariant rather than covariant. There are two reasons behind it:
	  * one, preventing any clause from being a subselect clause of a clause with an abstract prefix, ensuring that
	  * full mapping lists are compared, and two, treating all join kinds as equivalent for this purpose.
	  * Note that subselects may be nested to an arbitrary depth and only directly nested subselects of `F`
	  * conform to this type.
	  */
	type SubselectOf[-F <: FromClause] = FromClause {
		type Outer >: F <: FromClause
	}

	

	/** A complete `FromClause`, which doesn't contain any parameters and does not represent a subselect
	  * of another clause. `S &lt;: OuterFrom` if and only if it is a complete clause (with no abstract joins
	  * or `FromClause` occurring in its definition) and doesn't contain any `JoinParam` or `Subselect`.
	  * The name stems from the fact that only such sources can be used to create independent,
	  * most outer select statements.
	  */
	type OuterFrom = FromClause {
		type Outer = FromClause
	}






	/** A wrapper type adapting the labeled mapping type `L @: M` to a form with a single-argument type constructor
	  * accepting the `Origin` type for use in `With` classes and other types accepting such a type constructor:
	  * `Dual With (Humans As "humans")#T` (where `Humans[O] &lt;: MappingFrom[O]`).
	  * @see [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@:]]
	  */
	type As[M[O] <: MappingFrom[O], L <: Label] = { type T[O] = L @: M[O] }

	class AliasedSource[T[O] <: MappingFrom[O], A <: Label](source :RowSource[T], val alias :A)
		extends NamedSource[A, (T As A)#T]
	{
		override def name :A = alias

		override def apply[O] :A @: T[O] =
			(alias @: source[O].asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[A @: T[O]]
	}

	def AliasedSource[T[O] <: MappingFrom[O], A <: Label](source :RowSource[T], alias :A) :NamedSource[A, (T As A)#T] =
		new AliasedSource(source, alias)






	/** Extension methods for `FromClause` classes which benefit from having a static, invariant self type.
	  * Most notably, this includes methods for joining it with other relations.
	  */
	implicit class FromClauseMethods[F <: FromClause](private val self :F) extends AnyVal {

		@inline def tables :JoinedTables[F] = new JoinedTables[F](self)

		@inline def relations :JoinedRelations[F] = new JoinedRelations[F](self)



		@inline def join[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                (table :RowSource[R])
		                (implicit cast :InferSubject[self.type, InnerJoin, R, T, S]) :F InnerJoin R =
		InnerJoin(self, table)

		@inline def join[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                (table :R[A])
		                (implicit cast :InferSubject[self.type, InnerJoin, R, T, S],
		                 alias :OriginProjection[R[A], A, R[Any], Any]) :F InnerJoin R =
			join(RowSource(table))

		@inline def join[R <: FromClause](other :R) :other.JoinedWith[F, InnerJoin] =
			other.joinedWith(self, InnerJoin.template)



		@inline def outerJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                    (table :RowSource[R])
		                    (implicit cast :InferSubject[self.type, OuterJoin, R, T, S]) :F OuterJoin R =
			OuterJoin(self, table)

		@inline def outerJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                    (table :R[A])
		                    (implicit cast :InferSubject[self.type, OuterJoin, R, T, S],
		                     alias :OriginProjection[R[A], A, R[Any], Any]) :F OuterJoin R =
			outerJoin(RowSource(table))

		@inline def outerJoin[R <: FromClause](other :R) :other.JoinedWith[F, OuterJoin] =
			other.joinedWith(self, OuterJoin.template)



		@inline def leftJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                    (table :RowSource[R])
		                    (implicit cast :InferSubject[self.type, LeftJoin, R, T, S]) :F LeftJoin R =
			LeftJoin(self, table)

		@inline def leftJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                    (table :R[A])
		                    (implicit cast :InferSubject[self.type, LeftJoin, R, T, S],
		                     alias :OriginProjection[R[A], A, R[Any], Any]) :F LeftJoin R =
			leftJoin(RowSource(table))

		@inline def leftJoin[R <:FromClause ](other :R) :other.JoinedWith[F, LeftJoin] =
			other.joinedWith(self, LeftJoin.template)



		@inline def rightJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                     (table :RowSource[R])
		                     (implicit cast :InferSubject[self.type, RightJoin, R, T, S]) :F RightJoin R =
			RightJoin(self, table)

		@inline def rightJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                     (table :R[A])
		                     (implicit cast :InferSubject[self.type, RightJoin, R, T, S],
		                      alias :OriginProjection[R[A], A, R[Any], Any]) :F RightJoin R =
			rightJoin(RowSource(table))

		@inline def rightJoin[R <: FromClause](other :R) :other.JoinedWith[F, RightJoin] =
			other.joinedWith(self, RightJoin.template)



		@inline def naturalJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                       (table :RowSource[R])
		                       (implicit cast :InferSubject[self.type, InnerJoin, R, T, S],
		                        last :GetTableByNegativeIndex[self.Generalized, -1]) :F InnerJoin R =
			cast(InnerJoin[self.type, T, T, S](self, cast(table)) where naturalFilter[T])

		@inline def naturalJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                       (table :R[A])
		                       (implicit cast :InferSubject[self.type, InnerJoin, R, T, S],
		                        alias :OriginProjection[R[A], A, R[Any], Any],
		                        last :GetTableByNegativeIndex[self.Generalized, -1]) :F InnerJoin R =
			naturalJoin(RowSource(table))

		
		

		
		
		@inline def naturalOuterJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                            (table :RowSource[R])
		                            (implicit cast :InferSubject[self.type, OuterJoin, R, T, S],
		                             last :GetTableByNegativeIndex[self.Generalized, -1]) :F OuterJoin R =
			cast(OuterJoin[self.type, T, T, S](self, cast(table)) where naturalFilter[T])

		@inline def naturalOuterJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                            (table :R[A])
		                            (implicit cast :InferSubject[self.type, OuterJoin, R, T, S],
		                             alias :OriginProjection[R[A], A, R[Any], Any],
		                             last :GetTableByNegativeIndex[self.Generalized, -1]) :F OuterJoin R =
			naturalOuterJoin(RowSource(table))



		@inline def naturalLeftJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                           (table :RowSource[R])
		                           (implicit cast :InferSubject[self.type, LeftJoin, R, T, S],
		                            last :GetTableByNegativeIndex[self.Generalized, -1]) :F LeftJoin R =
			cast(LeftJoin[self.type, T, T, S](self, cast(table)) where naturalFilter[T])

		@inline def naturalLeftJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                           (table :R[A])
		                           (implicit cast :InferSubject[self.type, LeftJoin, R, T, S],
		                            alias :OriginProjection[R[A], A, R[Any], Any],
		                            last :GetTableByNegativeIndex[self.Generalized, -1]) :F LeftJoin R =
			naturalLeftJoin(RowSource(table))

		
		
		@inline def naturalRightJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                            (table :RowSource[R])
		                            (implicit cast :InferSubject[self.type, RightJoin, R, T, S],
		                             last :GetTableByNegativeIndex[self.Generalized, -1]) :F RightJoin R =
			cast(RightJoin[self.type, T, T, S](self, cast(table))(cast.self) where naturalFilter[T])

		@inline def naturalRightJoin[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                            (table :R[A])
		                            (implicit cast :InferSubject[self.type, RightJoin, R, T, S],
		                             alias :OriginProjection[R[A], A, R[Any], Any],
		                             last :GetTableByNegativeIndex[self.Generalized, -1]) :F RightJoin R =
			naturalRightJoin(RowSource(table))



		private def naturalFilter[T[O] <: TypedMapping[_, O]]
		                         (tables :JoinedTables[self.Generalized With T])
		                         (implicit prev :GetTableByNegativeIndex[self.Generalized With T, -2])
				:BooleanFormula[self.Generalized With T] =
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
			(True[self.Generalized With T]() /: joins)(_ && _)
		}



		@inline def subselectFrom[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
		                         (table :RowSource[R])
		                         (implicit cast :InferSubject[self.type, Subselect, R, T, S]) :F Subselect R =
			Subselect(self, table)

		@inline def subselectFrom[R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S, A]
		                         (table :R[A])
		                         (implicit cast :InferSubject[self.type, Subselect, R, T, S],
		                          alias :OriginProjection[R[A], A, R[Any], Any]) :F Subselect R =
			subselectFrom(RowSource(table))

		@inline def subselectFrom[R <: FromClause, J <: FromClause]
		                         (other :R)(implicit joined :JoinAll.FirstSubselect.Joined[F, R, J]) :J =
			joined(self, other)



		@inline def param[X :SQLForm] :F WithParam X = JoinParam(self, ParamSource[X]())

		@inline def param[X :SQLForm](name :String) :F WithParam X = JoinParam(self, ParamSource[X](name))
		//todo: select & subselect

	}






	implicit class JoinedTables[F <: FromClause](private val fromClause :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :get.T[get.J] = get(fromClause).mapping

		def apply[A <: String with Singleton](alias :A)(implicit get :ByLabel.Get[F, A]) :get.T[get.J] =
			get(fromClause).mapping

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :M[get.J] =
			get(fromClause).mapping


		def apply[N <: Numeral](n :N)(implicit get :GetTableByIndex[F, N]) :get.T[get.J] = get(fromClause).mapping


		def last(implicit get :GetTableByNegativeIndex[F, -1]) :get.T[get.J] = get(fromClause).mapping

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :get.T[get.J] = get(fromClause).mapping

	}



	class JoinedRelations[F <: FromClause](private val self :F) extends AnyVal {

		def of[E](implicit get :BySubject.Get[F, E]) :JoinedRelation[get.J, get.T] = get(self)

		def apply[M[O] <: MappingFrom[O]](implicit get :ByTypeConstructor.Get[F, M]) :JoinedRelation[get.J, M] =
			get(self)

		def apply[A <: String with Singleton]
		         (alias :A)(implicit get :ByLabel.Get[F, A]) :JoinedRelation[get.J, get.T] =
			get(self)


		def apply[N <: Numeral](n :N)(implicit get :GetTableByIndex[F, N]) :JoinedRelation[get.J, get.T] =
			get(self)

		def last(implicit get :GetTableByNegativeIndex[F, -1]) :JoinedRelation[get.J, get.T] = get(self)

		def prev(implicit get :GetTableByNegativeIndex[F, -2]) :JoinedRelation[get.J, get.T] = get(self)

	}






	@implicitNotFound("Can't calculate the size of the clause ${F}. Either the FROM clause is incomplete " +
		              "or the expected number ${N} is incorrect. Missing implicit: FromClauseSize[${F}, ${N}].")
	class FromClauseSize[-F <: FromClause, N <: Numeral] private(private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object FromClauseSize {
		implicit val DualCount :FromClauseSize[Dual, 0] = new FromClauseSize[Dual, 0](0)

		implicit def moreTables[L <: FromClause, R[O] <: MappingFrom[O], M <: Numeral, N <: Numeral]
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

		implicit def forOneTableMore[F <: FromClause, L <: FromClause, R[O] <: MappingFrom[O], M <: Numeral, N <: Numeral]
		                            (implicit split :Conforms[F, F, L With R], count :TableCount[L, M], inc :Inc[M, N])
				:TableCount[F, N] =
			new TableCount[F, N](inc.n)
	}



	@implicitNotFound("Relation mapping ${M} is not the first mapping of the FROM clause ${F}. " +
	                  "An implicit TableShift[${F}, ${M}, ${N}] will only exist if the clause F starts with either" +
	                  "FromClause or Dual joined with the mapping M. Note that TableShift is invariant both " +
	                  "in its F <: FromClause and M[O] <: MappingFrom[O] type parameters.")
	class TableShift[F <: FromClause, M[O] <: MappingFrom[O], N <: Numeral](private val n :Int) extends AnyVal {
		@inline def tables :N = n.asInstanceOf[N] //val tables :N crashes scalac
	}

	object TableShift {
		implicit def joinedWithDual[F <: FromClause, R[A] <: MappingFrom[A]]
		                           (implicit split :Conforms[F, F, Dual With R]) :TableShift[F, R, 0] =
			new TableShift[F, R, 0](0)

		implicit def joinedWithFromClause[F <: FromClause, R[A] <: MappingFrom[A]]
		                                 (implicit split :Conforms[F, F, FromClause With R]) :TableShift[F, R, 0] =
			new TableShift[F, R, 0](0)

		implicit def joinedEarlier[F <: FromClause, L <: FromClause, R[A] <: MappingFrom[A],
		                           T[A] <: MappingFrom[A], M <: Numeral, N <: Numeral]
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
//		/** A unique origin type with the negative index of the relation encoded in it. */
//		type O = ##[I]
		type J >: F <: FromClause

		/** The mapping type of the relation at index `N`. */
		type T[O] <: MappingFrom[O]

		val stretch :FromClause With T ExtendedBy J

		implicit def count :TableCount[J, _ <: Numeral] = new TableCount[J, stretch.length.type](stretch.length)


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


		implicit def lastNegative[M[O] <: MappingFrom[O]]
				:GetTableByNegativeIndex[FromClause With M, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new GetTableByNegativeIndex[FromClause With M, -1] {
				override type T[O] = M[O]
				type J = FromClause With M

				override val stretch = implicitly[J ExtendedBy J]

				override def table(from :FromClause With M) :JoinedRelation[FromClause With M, M] = from.last
			}


		implicit def previousNegative[L <: FromClause, R[O] <: MappingFrom[O], N <: Numeral, M <: Numeral]
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

		implicit def lastPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: Numeral]
		                         (implicit count :FromClauseSize[L, N])
				:GetTableByPositiveIndex[L With R, N, -1] { type T[O] = R[O]; type J = FromClause With R } =
			new GetTableByPositiveIndex[L With R, N, -1] {
				override type T[O] = R[O]
				override type J = FromClause With R

				override val stretch = implicitly[J ExtendedBy J]

				override def table(from :L With R) = from.last
			}

		implicit def previousPositive[L <: FromClause, R[O] <: MappingFrom[O], N <: Numeral, I <: Numeral, J <: Numeral]
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

		/** Witnesses that mapping `M[Any]` (where `Any` is the `Origin` type) satisfies the sarch predicate
		  * and should be returned. Note that the predicate is invariant with the regards to a mapping, so
		  * it will not match subclasses of a matching mapping, but this can be controlled by the variance of the
		  * implicit value declaration to achieve that effect.
		  * @tparam M the type constructor of the mapping of the relation, accepting an arbitrary `Origin` type parameter.
		  * @tparam X the input key of the search: the type provided by the accessor, such as a label for a `LabeledMapping`.
		  */
		@implicitNotFound("Mapping ${M} does not match the key type ${X}.")
		final class Predicate[-M[A] <: MappingFrom[A], X]

		private[this] final val predicate = new Predicate[MappingFrom, Any]

		/** Subclasses can use this method as the implicit */
		protected def report[M[A] <: MappingFrom[A], X] :Predicate[M, X] =
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
			type T[O] <: MappingFrom[O]

			type J >: F <: FromClause

			/** The negative index of the relation. */
			def shift :I

			val stretch :FromClause With T ExtendedBy J

			/** Getter for the matching relation. */
			def apply(from :F) :JoinedRelation[J, T] = table(from).extend(stretch)

			def table(from :F) :JoinedRelation[FromClause With T, T]
		}

		implicit def last[M[O] <: MappingFrom[O], X](implicit pred :Predicate[M, X])
				:Found[FromClause With M, X, -1] { type T[O] = M[O]; type J = FromClause With M } =
			new Found[FromClause With M, X, -1] {
				override type T[O] = M[O]
				override type J = FromClause With M

				override val stretch = implicitly[J ExtendedBy J]

				override def shift = -1 : -1

				override def table(from :FromClause With M) = from.last
			}

		implicit def previous[L <: FromClause, R[O] <: MappingFrom[O], X, I <: Numeral, J <: Numeral]
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
			type T[O] <: MappingFrom[O]

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
			                  "no implicit value for BySubject.Found[${F}, ${S}].")
			sealed abstract class Get[-F <: FromClause, S] extends super.BaseGet[F, S]

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

			/** Accessor for the right-most relation in `F` with mapping conforming to `LabeledMapping[A, _, _]`. */
			@implicitNotFound("No relation with Alias type ${A} in the from clause ${F}:\n" +
			                  "no implicit value for ByLabel.Found[${F}, ${A}].")
			sealed abstract class Get[-F <: FromClause, A <: Label] extends super.BaseGet[F, A] {
				type T[O] <: LabeledMapping[A, _, O]
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
		  * the `Origin` type. The provided type constructor may be for a super type of the actual mapping.
		  * While the input to match accepts only a single parameter, it is possible to match mappings with multiple
		  * parameters as long as all of them are fixed by using a ''type lambda'' as the argument:
		  * `({ type M[O] = SomeMapping[X1, X2, ..., O] })#M`. */
		object ByTypeConstructor extends GetTableByPredicate {
			implicit def satisfies[M[A] <: MappingFrom[A]] :Predicate[M, M[Any]] = report[M, M[Any]]

			/** An implicit accessor object for the last relation in `F` with `Mapping` type `M[_]`.
			  * The type and index of the relation are returned as members `T[O]` and `I`/ `shift :I`. */
			@implicitNotFound("No relation with type constructor ${M} in the from clause ${F}:\n" +
			                  "no implicit value for ByTypeConstructor.Found[${F}, ${M}].")
			sealed abstract class Get[-F <: FromClause, M[O] <: MappingFrom[O]] {
				type I <: Numeral
				/** A unique origin type with the negative index of the relation encoded in it. */
				type T[O] = M[O]
				type J >: F <: FromClause

				def apply(from :F) :JoinedRelation[J, M]
				def shift :I
			}

			implicit def Get[F <: FromClause, M[O] <: MappingFrom[O], N <: Numeral]
			                (implicit found :Found[F, M[Any], N] { type T[O] = M[O] })
					:Get[F, M] { type J = found.J; type I = N } =
				new Get[F, M] {
					override type I = N
					override type J = found.J
					override def shift = found.shift
					override def apply(from :F) = found(from)
				}
		}

	}






	/** Implicit `Joined[F, Dual, F]` witness performing an identity 'join' of a `FromClause` subtype `F`
	  * with `Dual`, signifying here an empty relation list, resulting in the type `F` itself.
	  * It is of lower precedence as we prefer to end the recursion at last join type `Dual With T`,
	  * as if the type `F` is also `Dual`, we need a polymorphic factory method to also create a `From` instance.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.JoinAll]]
	  */
	sealed abstract class JoinWithDual {
		type Joined[F <: FromClause, S <: FromClause, R <: FromClause]

		@inline implicit def joinDual[F <: FromClause] :Joined[F, Dual, F] = dual[F]

		protected def dual[F <: FromClause] :Joined[F, Dual, F]
	}



	/** Definitions of implicit values joining two `FromClause` instances by appending all relations (mappings)
	  * from the second one to the end of the first one, in the same order and preserving join kinds. Individual
	  * instances of this clause differ only in the kind of join used between the last relation of the first clause
	  * and the first relation of the second clause.
	  * @param start A prototype `Join` instance which copy constructors serve as a factory of joins of the same kind.
	  *              Both the left and right side of the join are irrelevant to this class.
	  * @tparam W A `Join` subtype (including `Join` itself) used to join the two clauses.
	  *           If `F =:= From[Mages] LeftJoin Spellbooks Join Spells` is the first clause,
	  *           `S =:= From[Thieves] LeftJoin Daggers` the second, and `W[L, R] =:= OuterJoin[L, R]`, then
	  *           there will be a single implicit value `Joined[F, S, R]` where
	  *           `R =:= From[Mages] LeftJoin Spellbooks Join Spells OuterJoin Thieves LeftJoin Daggers`.
	  */
	@deprecated("joins between two from clauses now do not require implicit evidence.", "Imoen")
	class JoinAll[W[+L <: FromClause, R[O] <: MappingFrom[O]] <: Join[L, R]] private
	             (start :Dual W MappingOf[_]#TypedProjection)
		extends JoinWithDual
	{
//		import JoinAll.JoinResult
		/** Implicit evidence witnessing that the `FromClause` subtype `J` is the result of joining
		  * the clauses `F` and `R` with the join type `W`.
		  */
		@implicitNotFound("Can't join FROM clauses ${F} and ${R}. The second clause type must not be abstract. " +
			              "Missing implicit JoinAll[${F}, ${R}, ${J}].")
		abstract class Joined[F <: FromClause, R <: FromClause, J <: FromClause] {
			private[sql] def apply(first :F, second :R) :J
//			private[sql] def apply[S <: R](first :F, second :S) :JoinResult[S, J]
		}



		protected override def dual[F <: FromClause] :Joined[F, Dual, F] =
			(first :F, _ :Dual) => first

		implicit def joinFirst[L <: FromClause, R[O] <: MappingFrom[O]]
				:Joined[L, Dual With R, W[Dual, MappingOf[_]#TypedProjection]#LikeJoin[L, R]] =
			(first :L, second :Dual With R) => second.withLeftLike(start)(first)

		implicit def joinNext[F <: FromClause, S <: FromClause, J <: L With R, L <: FromClause, R[O] <: MappingFrom[O],
		                      E <: FromClause] //todo: verify that this implicit induction even works
		                     (implicit conforms :Conforms[S, J, L With R], joinLeft :Joined[F, L, E])
				:Joined[F, S, J#WithLeft[E]] =
//			new Joined[F, S, J#WithLeft[E]] {
//				override private[sql] def apply[C <: S](first :F, second :C) = {
//					val join = conforms(second)
//					val left = joinLeft[join.left.type](first, join.left)
//					new JoinResult[S, J#WithLeft[E]]{
//						override def apply(e :BooleanFormula[S#Generalized]) = {
//
//						}
//					}
//				}
//			}
			(first :F, second :S) => {//we can simply cast the condition as it works on any clause with J as its suffix
				val left = joinLeft(first, second.left) //todo: get rid of this cast
				second.withLeft(left)(second.condition.asInstanceOf[BooleanFormula[left.Generalized With R]])
			}
	}



	object JoinAll {
		private[sql] abstract class JoinResult[S <: FromClause, J <: FromClause]
		                            (val result :J)
		{
			implicit val extension :result.Generalized <:< S#Generalized
			def apply(e :BooleanFormula[S#Generalized]) :BooleanFormula[result.Generalized]
		}

		implicitly[OriginProjection[ChainMapping[@~, @~, Any], Any, ChainMapping[@~, @~, Any], Any]]
		implicitly[OriginProjection[TypedMapping[@~, Any], Any, TypedMapping[@~, Any], Any]]
		private[this] val dummy = RowSource[MappingOf[@~]#TypedProjection, Any](ChainMapping[Any])

		val FirstOuter = new JoinAll[OuterJoin](OuterJoin(Dual, dummy))
		val FirstLeft = new JoinAll[LeftJoin](LeftJoin(Dual, dummy))
		val FirstRight = new JoinAll[RightJoin](RightJoin(Dual, dummy))
		val FirstInner = new JoinAll[InnerJoin](InnerJoin(Dual, dummy))
		val FirstSubselect = new JoinAll[Subselect](Subselect(Dual, dummy))
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
	  * (left side). The purpose of this class is to allow `SQLFormula` instances based on one clause `F` be converted
	  * into isomorphic expressions based on a second clause `S`, as long as all mappings in `F`'s static type
	  * form a continuous subsequence of mappings listed in `S`. Note that `SQLFormula` is contravariant in
	  * its clause type parameter, meaning that an abstract `FromClause` occurring at the start of the from clause
	  * type parameter doesn't 'hide' any mappings used by that formula, but to the contrary, serves to signify that
	  * it doesn't rely in any form on that portion of the clause it is based on. As a result, this proof
	  * is indeed a valid representation that such a conversion from `F` to `S` is possible for any `SQLFormula`.
	  * Due to this contravariance in `S`, this isn't any form of a generalized subtyping relation
	  * and should be relied upon only in the context of the actual extension.
	  */ //todo: analyze if we really can't make it covariant in F
	@implicitNotFound("FromClause ${F} is not a prefix of the clause ${S} (ignoring join kinds).")
	class ExtendedBy[F <: FromClause, -S <: FromClause] private (val length :Int) extends AnyVal {
		/** A transitive proof that a clause extending `S` with a single relation (mapping) also extends `F`. */
		def stretch[R[O] <: MappingFrom[O]] :F ExtendedBy (S With R) = new ExtendedBy(length + 1)

		/** A transitive proof that a clause extended by `F` with a single relation(mapping) is also extended by `S`. */
		def shrink[L <: FromClause, R[O] <: MappingFrom[O]](implicit front :F <:< (L With R)) :L ExtendedBy S =
			new ExtendedBy(length + 1)

	}



	object ExtendedBy {
		private[this] val instance = new ExtendedBy[FromClause, FromClause](0)

		implicit def itself[F <: FromClause] :ExtendedBy[F, F] = instance.asInstanceOf[F ExtendedBy F]

		implicit def extend[S <: FromClause, L <: FromClause, R[A] <: MappingFrom[A]]
		                   (implicit ev :S ExtendedBy L) :S ExtendedBy (L With R) =
			new ExtendedBy(ev.length + 1)
	}




//	import Zipper._
////	todo: zipper
//	trait Zipper[P <: FromClause, S <: ZipperSuffix]
//
//	object Zipper {
//		trait >>[O] extends TypedMapping[Any, O]
//
//		trait ZipperSuffix[S] {
//			type Next[O] <: MappingFrom[O]
//			val next :JoinedRelation[Joined[FromClause With Next], Next]
//
//			type Joined[+F <: FromClause] <: FromClause
//		}
//
//		class LastMapping[M[O] <: MappingFrom[O]](val next :LastRelation[M]) extends ZipperSuffix[M] {
//			override type Next[O] = M[O]
//			override type Joined[+F <: FromClause] = F
//		}
//
//		trait >>:[S] {
//			type LastMapping[O] <: MappingFrom[O]
////			def apply[F <: FromClause](prefix :F) :J[F]
//		}
//	}

}


