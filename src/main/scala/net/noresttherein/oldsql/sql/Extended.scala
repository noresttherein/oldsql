package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.RowProduct.{ClauseComposition, ClauseDecomposition, ClauseGeneralization, ExtendedBy, ExtendingClause, NonEmptyFrom, NonEmptyFromTemplate, PrefixOf}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] implementation combining another ''from'' clause `L`
  * and an additional relation with mapping `R`. The relationship between the constituent clause and this clause
  * is undefined; in particular, the relations from the clause `L` may not be a part of this clause.
  * It is a root of a class hierarchy of various SQL joins, subselect clauses and other implementations sharing the same
  * basic recursive interface. It is too generic to be of much use in the client code, as most operations
  * are not available at this level of abstraction and, as such, is considered more implementation oriented,
  * providing definitions for some additional abstract methods.
  *
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point.
  *
  * @tparam L the left side of this join: a `RowProduct` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause, accepting
  *           the `Origin` type as the unspecified parameter.
  * @see [[net.noresttherein.oldsql.sql.Extended]]
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.JoinLike]]
  * @see [[net.noresttherein.oldsql.sql.GroupBy]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */ //alternative name ideas: fuse/attach
trait Compound[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonEmptyFrom with NonEmptyFromTemplate[L Compound R, L Compound R]
{
	thisClause =>

	override type LastMapping[O] = R[O]
	override type LastTable[F <: RowProduct] = JoinedRelation[F, R]
	override type FromLast >: Generalized <: RowProduct Compound R


	/** A `RowProduct` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	//consider: making it public only in JoinLike. The problem is that it would need to be hidden in RelationSQL, too.
	// Even with that, the Mapping is always available from JoinedMappings, so it could be in theory used to create
	// a new relation. Some application of common sense in judgement will be needed.
	/** The last SQL relation in this clause. This is the relation understood in the global sense, as a unique
	  * database object, rather than an entry in the list of relations in the ''from'' clause -
	  * for that have a look at [[net.noresttherein.oldsql.sql.RowProduct.last last]]. Several specialized
	  * `Compound` subclasses use dedicated implementations which are ''not'' cross-compatible and can't be
	  * used as arguments for any 'join' methods, as it will cause an error at the time this clause is processed.
	  * In these cases, a 'relation' can represent a query parameter
	  * ([[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]]) or a grouping expression
	  * ([[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]])
	  * and is of no use to client code. Additionally, it can possibly serve as a placeholder value,
	  * with a `RowProduct` depending on a relation with `Mapping` for a component not representing any single
	  * database object, but which can be replaced at a later date by 'rooting' it to a table containing
	  * this type of component. For this reason care should be taken when accessing this property directly,
	  * that this instance represents an actual join and this method returns a usable value.
	  */
	def right :Relation[R] = last.relation

	/** The right side of the join - representation of a table/relation alias containing the mapping of its schema.
	  * It identifies the SQL relation (table, view or ''select'') which is being added to the clause and its index,
	  * to distinguish between possible multiple occurrences of the same relation.
	  *
	  * It is an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] for the subject of the relation's mapping,
	  * so it can be used directly as part of larger expressions. True to its name, it is treated always
	  * as the last entry on the list and the expression `join.left.last` is an instance representing only
	  * the last relation in the prefix clause `join.left`. In particular, it is not a valid reference to a relation
	  * from the point of view of this clause. Its type parameter makes it incompatible for direct use
	  * in SQL expressions based on this clause and casting it will result in generating invalid SQL.
	  * You can however use the method [[net.noresttherein.oldsql.sql.Compound.lastAsIn lastAsIn]] in the following way:
	  * {{{
	  *     def secondLast[T1[O] <: MappingAt[O], T2 <: MappingAt[O]](from :RowProduct AndFrom T1 AndFrom T2) =
	  *         from.left.lastAsIn[RowProduct AndFrom T1 AndFrom T2]
	  * }}}
	  * The relation is usable in this way in expressions based on clauses containing this instance as a prefix
	  * (which where created by joining/adding additional `Compound` links to it). Moreover, any two clauses
	  * with types sharing a suffix, can freely exchange relations from that suffix and, by transitivity,
	  * any SQL expressions based on the clause type representing that suffix, with the differing prefixes
	  * replaced with a wildcard or abstract type.

	  * Note however that specialized `Compound` implementations can use dedicated implementations
	  * of [[net.noresttherein.oldsql.schema.Relation Relation]] or its [[net.noresttherein.oldsql.schema.Mapping Mapping]],
	  * tied not only to this particular implementation, but also to this instance: for example,
	  * the relation of an [[net.noresttherein.oldsql.sql.UnboundParam UnboundParam]] cannot be reused in the same
	  * clause, and cannot be joined the same way as relations for tables.
	  *
	  * All instances created internally (rather than provided by the application as an argument to a factory method)
	  * have empty [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation.includes includes]] and
	  * [[net.noresttherein.oldsql.sql.ast.MappingSQL.JoinedRelation.excludes excludes]] list, meaning the operative
	  * column set for the relation was not altered from the default provided by the wrapped
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]. It is a recommended practice, although not
	  * validated. Deviating from this principle will not result in an error, but can introduce unexpected alterations
	  * to the final SQL.
	  */
	override val last :JoinedRelation[FromLast, R]


	override def lastAsIn[E <: RowProduct](implicit extension :FromLast PrefixOf E) :JoinedRelation[E, R] =
		last.asIn[E]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.filter]]
	  */ //declared here to have a single equals/hashCode implementation
	protected def condition :LocalBoolean[Generalized]


	override type Generalized >: Dealiased <: (left.Generalized Compound R) {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self Compound R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self Compound R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}


	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `RowProduct`, as they become proper path types instead of projections.
	  */
	protected def narrow :left.type Compound R


	override def fullSize :Int = left.fullSize + 1

	override def isParameterized :Boolean = left.isParameterized
	override def isValidSubselect :Boolean = left.isValidSubselect

	override type Base = DefineBase[Implicit]



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Compound.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :Extended[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left && join.condition == condition

		case _ => false
	}

	override def hashCode :Int = (left.hashCode * 31 + last.hashCode) * 31 + condition.hashCode


	/** Name of the join for use by the `toString` method. */
	def name :String

	override def toString :String =
		left.toString + " " + name + " " + right + (if (condition == True) "" else " on " + condition)

}






object Compound {

	/** An existential upper bound of all `Compound` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Compound[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Compound` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Compound R }

	/** A curried type constructor for `Compound` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Compound R }






	/** An implicit witness serving as type inference helper for the `Mapping` type of a joined
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]. It separates the inference of the mapping type itself
	  * from its `Subject` type, thus allowing for both to function properly in methods with the type parameters
	  * in the form of `[R <: BaseMapping[S, _], S]` (and similar). It is used in particular when constructing new
	  * `Join` instances to remove explicit type parameters from the call. Apart from this purely independent role,
	  * it provides identity casting methods, similar to those of `=:=`, for various relevant types
	  * (`SQLExpression`, `Relation`, `F`) between those parameterized with `R` and `T` to eliminate the need
	  * for casting inside methods using this support.
	  * @tparam F a type constructor for a `AndFrom` 'join' extending some ''from'' clause with the provided mapping type.
	  * @tparam R a type constructor for a `Mapping` accepting its `Origin` type; it is the 'input' type used by the
	  *           arguments of the method accepting this instance, declared only with the `MappingAt[O]` upper bound.
	  * @tparam T the type `R`, but with the upper bound of `U`. It generally should not be used in the method signature
	  *           (in particular, it cannot appear before this argument), but can be used in the method's body.
	  *           As the type argument `U` is generally a concrete type, the type `R` is typically a type parameter
	  *           with an explicit upper bound consistent with `U`, such as `R[O] <: BaseMapping[S, O]`.
	  * @tparam U an upper bound for the mapping type `R` (and `T`) required for the implementation.
	  *           It is a concrete type with only its type parameters being abstract, typically given as
	  *           `BaseMapping[S, O]`, where `S` is a free type parameter of the method. It allows the inference
	  *           of its (and, by extension, `R`'s) type parameters (`S`).
	  */
	@implicitNotFound("Failed to infer the Subject type of mapping ${R}: cannot prove that " +
	                  "${R}[O] <: ${T}[O] with ${U}. This may be caused by the inferred type ${T} or its subject type S " +
	                  "occurring before the implicit parameter JoinedRelationSubject[${F}, ${R}, ${T}, ${U}] " +
	                  "(alias InferSubject[?, ?, ${R}, ${T}, ?]) or in the method's result type.")
	sealed abstract class JoinedRelationSubject[F[M[O] <: MappingAt[O]] <: RowProduct,
	                                            R[O] <: MappingAt[O], T[O] <: U[O], +U[O] <: BaseMapping[_, O]]
		extends (F[T] => F[R])
	{
		def apply(rows :Relation[R]) :Relation[T]

		def apply(join :F[T]) :F[R]

		def cast[E[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
		        (e :SQLExpression[E[R], S, X]) :SQLExpression[E[T], S, X]

		def cast[E[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
		        (e :ColumnSQL[E[R], S, X]) :ColumnSQL[E[T], S, X]

		def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Compound B, S >: LocalScope <: GlobalScope, X]
                 (e :ColumnSQL[L J R, S, X]) :ColumnSQL[L J T, S, X]

		def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Compound B, S >: LocalScope <: GlobalScope, X]
		         (e :SQLExpression[L J R, S, X]) :SQLExpression[L J T, S, X]

		def self :JoinedRelationSubject[F, T, T, U]
	}



	object JoinedRelationSubject {
		import net.noresttherein.oldsql.schema.bases.BaseMapping.AnyAt

		private[this] val instance =
			new JoinedRelationSubject[Compound.WithLeft[RowProduct]#F, AnyAt, AnyAt, AnyAt] {
				override def apply(rows :Relation[AnyAt]) = rows

				override def apply(join :RowProduct Compound AnyAt) = join

				override def cast[F[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
				                 (e :ColumnSQL[F[AnyAt], S, X]) = e

				override def cast[F[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
				                 (e :SQLExpression[F[AnyAt], S, X]) = e

				override def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Compound B,
				                   S >: LocalScope <: GlobalScope, X]
				                  (e :ColumnSQL[J[L, AnyAt], S, X]) = e

				override def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Compound B,
				                   S >: LocalScope <: GlobalScope, X]
				                  (e :SQLExpression[J[L, AnyAt], S, X]) = e

				override def self = this
			}

		implicit def identity[J[M[O] <: MappingAt[O]] <: _ Compound M, R[O] <: BaseMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]


		/** A simplified form of the implicit witness
		  * [[net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject JoinedRelationSubject]], it guides the compiler
		  * into proper inference of both the type of a mapping, and its `Subject` type as defined by `BaseMapping`.
		  * It is used when constructing a new `Extended` subtype to avoid the need for explicit specification
		  * of the joined mapping type and its subject.
		  * It accepts five type parameters: first three are input parameters - the left side of the join `L`, the join
		  * type `J`, given as a two-argument type constructor, and the (origin-accepting) type constructor
		  * for the mapping `R` on the right side of the join. These should be specified as the types of parameters
		  * passed to the constructor function. The last two type parameters are the output: the type `T[O] =:= R[O]`
		  * (in reality, but not provable at the spot), but with the `BaseMapping` trait as its upper bound,
		  * and the type parameter `S` given by `R` to `BaseMapping` for the subject type. The object itself
		  * contains methods for converting various types related to the use case between those parameterized with `T`,
		  * and those parameterized with `R`. As the result, the parameters with more generic types, but which can be
		  * inferred automatically by the compiler, can be converted to the more specific variants,
		  * used in the construction on the join, and then converted back to a type expressed in terms of the input
		  * parameters.
		  */
		type InferSubject[L <: RowProduct, J[+F <: L, M[O] <: MappingAt[O]] <: F Compound M,
		                  R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S] =
			JoinedRelationSubject[({ type F[M[O] <: MappingAt[O]] = L J M })#F, R, T, MappingOf[S]#TypedProjection]

	}

}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] implementation which extends an existing ''from'' clause `L`
  * with a new relation (an SQL table, view, select, or some synthetic placeholder) `R`. All relations
  * from the left side are considered a part of this clause, witnessed by an implicit value of
  * of `L` [[net.noresttherein.oldsql.sql.RowProduct.ExtendedBy ExtendedBy]] `R`.
  * Thus, all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s based on the left side (parameterized
  * with `L`) are convertible to expressions based on this clause.
  *
  * It is an implementation oriented trait, grouping most common definitions of methods declared in `RowProduct`,
  * useful only when operating on the most abstract level, as most domain specific functions are available only
  * for more specialized types.
  *
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExtendedBy]]
  * @author Marcin Mościcki
  */ //other words :Tack, Include, With
trait Extended[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Compound[L, R] with ExtendingClause[L] with NonEmptyFromTemplate[L Extended R, L Extended R]
{ thisClause =>

	override type FromLast >: Generalized <: RowProduct Extended R

	override type Generalized >: Dealiased <: (left.Generalized Extended R) {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self Extended R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self Extended R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}


	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `RowProduct`, as they become proper path types instead of projections.
	  */
	protected def narrow :left.type Extended R


	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(extension.extendFront[left.Generalized, R]) ~ last.extend(target)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Extended.*]


	private[sql] override def concrete_ExtendingClause_subclass_must_extend_Extended_or_ExtendingDecorator :Nothing =
		throw new UnsupportedOperationException

}






object Extended {


	/** An existential upper bound of all `Extended` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Extended[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Extended` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Extended R }

	/** A curried type constructor for `Extended` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Extended R }



	/** A base trait for implementations of [[net.noresttherein.oldsql.sql.Extended Extended]] which narrows
	  * down the mapping type to the actually required `BaseMapping` (due to some scala bug preventing
	  * the use of `RefinedMapping` in the `TypedComponentSQL` instead).
	  */
	trait AbstractExtended[+L <: RowProduct, R[O] <: BaseMapping[S, O], S] extends Extended[L, R] { thisClause =>

		override val last :RelationSQL[FromLast, R, S, FromLast]

		override def fullTableStack[E <: RowProduct](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			last.extend(target) #:: left.fullTableStack(target)(extension.extendFront[left.Generalized, R])


		override def tableStack[E <: RowProduct](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			last.extend(target) #:: left.tableStack(target)(extension.extendFront[left.Generalized, R])
	}






	/** A marker trait for [[net.noresttherein.oldsql.sql.Extended Extended]] implementations other than
	  * the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. While not mandatory, it is recommended
	  * that all such clauses extend this type, as several implicit parameters responsible for traversing
	  * composite ''from'' clauses, in particular accessors for the joined relations, depend on any `Extended` type
	  * to conform to either it, or the `Subselect`, with no provision for other cases. Note that this holds only
	  * for `Extended` subtypes, and there are other clauses, in particular
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], which are neither.
	  */
	trait NonSubselect[+L <: RowProduct, R[O] <: MappingAt[O]]
		extends Extended[L, R] with NonEmptyFromTemplate[L NonSubselect R, L NonSubselect R]
	{ thisClause =>
		override def isSubselectParameterized :Boolean = left.isSubselectParameterized

		override type Implicit = left.Implicit
		override type Outer = left.Outer

		override type Row = left.Row ~ last.Subject

		override def row[E <: RowProduct]
		             (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, Row] =
			left.row(target)(extension.extendFront[left.Generalized, R]) ~ last.extend(target)

		override type OuterRow = left.OuterRow

		override def outerRow[E <: RowProduct]
		             (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, GlobalScope, OuterRow] =
			left.outerRow(target)
	}






	@implicitNotFound("I do not know how to decompose ${F} into an Extended subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExtendedDecomposition[${F}, ${L}, ${R}, ${J}, ${U}, ${M}].")
	class ExtendedDecomposition[-F <: L J R, L <: U, R[O] <: MappingAt[O],
	                            J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: RowProduct]
		extends ClauseDecomposition[F, L, U]
	{
		override type E[+A <: U] = A J R
		override type S[+A >: L <: U] = A J R

		@inline final override def prefix[A >: L <: U] :A PrefixOf (A J R) = PrefixOf.itself[A].extend[J, R]
		@inline final override def extension[A <: U] :A PrefixOf (A J R) = PrefixOf.itself[A].extend[J, R]

		@inline final override def unapply(join :F) :L = join.left

		override def upcast[A >: L <: U] :ExtendedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedDecomposition[A J R, A, R, J, U]]

		override def cast[A <: U] :ExtendedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedDecomposition[A J R, A, R, J, U]]
	}



	@implicitNotFound("I do not know the generalized Extended type constructor of ${F}.\n" +
	                  "Missing implicit ExtendedGeneralization[${F}, ${L}, ${R}, ${J}, ${U}].")
	abstract class ExtendedGeneralization[F <: L J R, L <: U, R[O] <: MappingAt[O],
	                                      J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: RowProduct]
		extends ExtendedDecomposition[F, L, R, J, U] with ClauseGeneralization[F, L, U]
	{ self =>
		override type G[+A >: L <: U] >: A J R <: A Extended R

		override def upcast[A >: L <: U] :ExtendedGeneralization[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedGeneralization[A J R, A, R, J, U]]

		override def cast[A <: U] :ExtendedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedGeneralization[A J R, A, R, J, U]]
	}



	@implicitNotFound("I do not know how to decompose ${F} into an Extended subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExtendedComposition[${F}, ${L}, ${R}, ${J}, ${U}, ${M}].")
	abstract class ExtendedComposition[F <: L J R, L <: U, R[O] <: M[O],
	                                   J[+A <: U, B[O] <: M[O]] <: A Extended B, U <: RowProduct, M[O] <: MappingAt[O]]
		extends ExtendedGeneralization[F, L, R, J, U] with ClauseComposition[F, L, U]
	{ self =>
		override type G[+A >: L <: U] = A Generalized R
		type Generalized[+A <: U, B[O] <: M[O]] >: A J B <: A Extended B


		override def generalized[P <: U] :ExtendedComposition[P Generalized R, P, R, Generalized, U, M] {
				type Generalized[+A <: U, B[O] <: M[O]] = self.Generalized[A, B]
			} =
			this.asInstanceOf[ExtendedComposition[P Generalized R, P, R, Generalized, U, M] {
				type Generalized[+A <: U, B[O] <: M[O]] = self.Generalized[A, B]
			}]

		override def upcast[A >: L <: U] :ExtendedComposition[A J R, A, R, J, U, M] =
			this.asInstanceOf[ExtendedComposition[A J R, A, R, J, U, M]]

		override def cast[A <: U] :ExtendedComposition[A J R, A, R, J, U, M] =
			this.asInstanceOf[ExtendedComposition[A J R, A, R, J, U, M]]

	}


//	implicit def extendedDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
//			:ExtendedDecomposition[L Extended R, L, R, Extended, RowProduct] =
//		decomposition.asInstanceOf[ExtendedDecomposition[L Extended R, L, R, Extended, RowProduct]]

	private[this] val decomposition =
		new ExtendedDecomposition[RowProduct Extended MappingAt, RowProduct, MappingAt, Extended, RowProduct]

}
