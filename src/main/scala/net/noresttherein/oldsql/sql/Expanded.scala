package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, ExpandingClause, NonEmptyFrom, NonEmptyFromTemplate, PrefixOf, RowComposition, RowDecomposition}
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}
import net.noresttherein.oldsql.sql.ast.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject.InferSubject






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
  * @see [[net.noresttherein.oldsql.sql.Expanded]]
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.JoinLike]]
  * @see [[net.noresttherein.oldsql.sql.GroupBy]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */ //alternative name ideas: fuse/attach/annex/augment
trait Adjoin[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonEmptyFrom with NonEmptyFromTemplate[L Adjoin R, L Adjoin R]
{ thisClause =>

	override type LastMapping[O] = R[O]
	override type Last[F <: RowProduct] <: JoinedRelation[F, R]
	override type FromLast >: Generalized <: RowProduct Adjoin R


	/** A `RowProduct` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	//consider: making it public only in JoinLike. The problem is that it would need to be hidden in RelationSQL, too.
	// Even with that, the Mapping is always available from JoinedMappings, so it could be in theory used to create
	// a new relation. Some application of common sense in judgement will be needed.
	/** The last SQL relation in this clause. This is the relation understood in the global sense, as a unique
	  * database object, rather than an entry in the list of relations in the ''from'' clause -
	  * for that have a look at [[net.noresttherein.oldsql.sql.RowProduct.last last]]. Several specialized
	  * `Adjoin` subclasses use dedicated implementations which are ''not'' cross-compatible and can't be
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
	  * You can however use the method [[net.noresttherein.oldsql.sql.Adjoin.lastAsIn lastAsIn]] in the following way:
	  * {{{
	  *     def secondLast[T1[O] <: MappingAt[O], T2 <: MappingAt[O]](from :RowProduct AndFrom T1 AndFrom T2) =
	  *         from.left.lastAsIn[RowProduct AndFrom T1 AndFrom T2]
	  * }}}
	  * The relation is usable in this way in expressions based on clauses containing this instance as a prefix
	  * (which where created by joining/adding additional `Adjoin` links to it). Moreover, any two clauses
	  * with types sharing a suffix, can freely exchange relations from that suffix and, by transitivity,
	  * any SQL expressions based on the clause type representing that suffix, with the differing prefixes
	  * replaced with a wildcard or abstract type.
	  *
	  * Note however that specialized `Adjoin` implementations can use dedicated implementations
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
	override val last :Last[FromLast]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.filter]]
	  */ //declared here to have a single equals/hashCode implementation
	protected def condition :LocalBoolean[Generalized]


	override type Generalized >: Dealiased <: (left.Generalized Adjoin R) {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self Adjoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
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

	override type Self <: (left.Self Adjoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
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
	def narrow :left.type Adjoin R


	override def fullSize :Int = left.fullSize + 1

	override def lastParamOffset :Int = left.lastParamOffset + 1
	override def isParameterized :Boolean = left.isParameterized
	override def isValidSubselect :Boolean = left.isValidSubselect

	override type Base = DefineBase[Implicit]



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Adjoin.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :Adjoin[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left && join.condition == condition

		case _ => false
	}

	override def hashCode :Int = (left.hashCode * 31 + last.hashCode) * 31 + condition.hashCode


	/** Name of the join for use by the `toString` method. */
	def name :String

	override def toString :String =
		left.toString + " " + name + " " + right +
			(if (aliasOpt.isEmpty) "" else " as " + alias) + (if (condition == True) "" else " on " + condition)

}






/** Base trait for companion objects of [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] types, serving as a factory
  * and match pattern.
  *
  * @tparam J the type constructor of the `Adjoin` type, taking a [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]
  *           of  a specific type for its 'left' side and a [[net.noresttherein.oldsql.schema.Mapping Mapping]]
  *           type constructor accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type
  *           as the right side.
  * @tparam LU  the upper bound on the left side of this join type.
  * @tparam RU  the upper bound on the right side of this join type.
  * @tparam Rel the type of [[net.noresttherein.oldsql.schema.Relation Relation]] which is acceptable as the right
  *             side of this join type.
  * @define JoinClass
  * @define joinType
  * @define relType
  * @define LeftBound
  */
trait AdjoinFactory[J[L <: LU, R[O] <: RU[O]] <: L Adjoin R, LU <: RowProduct, RU[O] <: MappingAt[O], Rel[M[O] <: RU[O]]] {

	/** An existential upper bound of all $JoinClass instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = J[_ <: LU, T] forSome { type T[O] <: RU[O] }

	/** A curried type constructor for $JoinClass type instances, accepting the left $LeftBound type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: LU] = { type F[R[O] <: RU[O]] = L J R }

	/** A curried type constructor for $JoinClass type instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left $LeftBound type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: RU[O]] = { type F[L <: LU] = L J R }



	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] InnerJoin R` clause joining the two relations.
	  */
//	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
//	          R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
//	         (left :Table[L], right :Table[R])
//	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
//	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
//			:From[L] InnerJoin R =
//		InnerJoin(left, right)

	/** Create a $joinType between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left  a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
//	def apply[L <: LU, R[O] <: RU[O], T[O] <: BaseMapping[S, O], S]
//	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized J R] = True)
//	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L J R


	/** Matches all `Join` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])]

	/** Matches all `Join` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(LU, Relation.*)]

}



object Adjoin {

	/** An existential upper bound of all `Adjoin` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Adjoin[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Adjoin` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Adjoin R }

	/** A curried type constructor for `Adjoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Adjoin R }






	/** An implicit witness serving as type inference helper for the `Mapping` type of a joined
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]. It separates the inference of the mapping type itself
	  * from its `Subject` type, thus allowing for both to function properly in methods with the type parameters
	  * in the form of `[R <: BaseMapping[S, _], S]` (and similar). It is used in particular when constructing new
	  * `Join` instances to remove explicit type parameters from the call. Apart from this purely independent role,
	  * it provides identity casting methods, similar to those of `=:=`, for various relevant types
	  * (`SQLExpression`, `Relation`, `F`) between those parameterized with `R` and `T` to eliminate the need
	  * for casting inside methods using this support.
	  * @tparam F a type constructor for a `AndFrom` 'join' expanding some ''from'' clause with the provided mapping type.
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

		def apply(rows :Table[R]) :Table[T]

		def apply(join :F[T]) :F[R]

		def cast[E[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
		        (e :SQLExpression[E[R], S, X]) :SQLExpression[E[T], S, X]

		def cast[E[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
		        (e :ColumnSQL[E[R], S, X]) :ColumnSQL[E[T], S, X]

		def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B, S >: LocalScope <: GlobalScope, X]
                 (e :ColumnSQL[L J R, S, X]) :ColumnSQL[L J T, S, X]

		def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B, S >: LocalScope <: GlobalScope, X]
		         (e :SQLExpression[L J R, S, X]) :SQLExpression[L J T, S, X]

		def self :JoinedRelationSubject[F, T, T, U]
	}



	object JoinedRelationSubject {
		import net.noresttherein.oldsql.schema.bases.BaseMapping.AnyAt

		private[this] val instance =
			new JoinedRelationSubject[Adjoin.WithLeft[RowProduct]#F, AnyAt, AnyAt, AnyAt] {
				override def apply(rows :Relation[AnyAt]) = rows
				override def apply(rows :Table[AnyAt]) = rows

				override def apply(join :RowProduct Adjoin AnyAt) = join

				override def cast[F[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
				                 (e :ColumnSQL[F[AnyAt], S, X]) = e

				override def cast[F[M[O] <: MappingAt[O]] <: RowProduct, S >: LocalScope <: GlobalScope, X]
				                 (e :SQLExpression[F[AnyAt], S, X]) = e

				override def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
				                   S >: LocalScope <: GlobalScope, X]
				                  (e :ColumnSQL[J[L, AnyAt], S, X]) = e

				override def apply[L <: RowProduct, J[A <: L, B[O] <: MappingAt[O]] <: A Adjoin B,
				                   S >: LocalScope <: GlobalScope, X]
				                  (e :SQLExpression[J[L, AnyAt], S, X]) = e

				override def self = this
			}

		implicit def identity[J[M[O] <: MappingAt[O]] <: _ Adjoin M, R[O] <: BaseMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]


		/** A simplified form of the implicit witness
		  * [[net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject JoinedRelationSubject]], it guides the compiler
		  * into proper inference of both the type of a mapping, and its `Subject` type as defined by `BaseMapping`.
		  * It is used when constructing a new `Expanded` subtype to avoid the need for explicit specification
		  * of the joined mapping type and its subject.
		  * It accepts five type parameters: first three are input parameters - the left side of the join `L`, the join
		  * type `J`, given as a two-argument type constructor, and the (origin-accepting) type constructor
		  * for the mapping `R` on the right side of the join. These should be specified as the types of parameters
		  * passed to the constructor function. The last two type parameters are the output: the type `T[O] =:= R[O]`
		  * (in reality, but not provable on the spot), but with the `BaseMapping` trait as its upper bound,
		  * and the type parameter `S` given by `R` to `BaseMapping` for the subject type. The object itself
		  * contains methods for converting various types related to the use case between those parameterized with `T`,
		  * and those parameterized with `R`. As the result, the parameters with more generic types, but which can be
		  * inferred automatically by the compiler, can be converted to the more specific variants,
		  * used in the construction on the join, and then converted back to a type expressed in terms of the input
		  * parameters.
		  */
		type InferSubject[L <: RowProduct, J[+F <: L, M[O] <: MappingAt[O]] <: F Adjoin M,
		                  R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S] =
			JoinedRelationSubject[({ type F[M[O] <: MappingAt[O]] = L J M })#F, R, T, MappingOf[S]#TypedProjection]


		/** A simplified form of the implicit witness
		  * [[net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject JoinedRelationSubject]], it guides the compiler
		  * into proper inference of both the type of a mapping, and its `Subject` type as defined by `BaseMapping`.
		  * It is used when constructing a new `Expanded` subtype to avoid the need for explicit specification
		  * of the joined mapping type and its subject. It is a variant
		  * of [[net.noresttherein.oldsql.sql.Adjoin.JoinedRelationSubject.InferSubject InferSubject]] which adds
		  * an [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause to the joined table.
		  * It accepts six type parameters: first three are input parameters - the left side of the join `L`, the join
		  * type `J`, given as a two-argument type constructor, and the (origin-accepting) type constructor
		  * for the mapping `R` on the right side of the join. These should be specified as the types of parameters
		  * passed to the constructor function. The next two type parameters are the output: the type `T[O] =:= R[O]`
		  * (in reality, but not provable on the spot), but with the `BaseMapping` trait as its upper bound,
		  * and the type parameter `S` given by `R` to `BaseMapping` for the subject type. The object itself
		  * contains methods for converting various types related to the use case between those parameterized with `T`,
		  * and those parameterized with `R`. As the result, the parameters with more generic types, but which can be
		  * inferred automatically by the compiler, can be converted to the more specific variants,
		  * used in the construction on the join, and then converted back to a type expressed in terms of the input
		  * parameters. Finally, the last type parameter `N` is a string literal which is used as the alias for `R`.
		  */
		type InferAliasedSubject[L <: RowProduct, J[+F <: L, M[O] <: MappingAt[O]] <: F Adjoin M,
		                         R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label] =
			JoinedRelationSubject[({ type F[M[O] <: MappingAt[O]] = L J M As A })#F, R, T, MappingOf[S]#TypedProjection]
	}

}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] implementation which expands an existing ''from'' clause `L`
  * with a new relation (an SQL table, view, select, or some synthetic placeholder) `R`. All relations
  * from the left side are considered a part of this clause, witnessed by an implicit value of
  * of `L` [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] `R`.
  * Thus, all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s based on the left side (parameterized
  * with `L`) are convertible to expressions based on this clause.
  *
  * It is an implementation oriented trait, grouping most common definitions of methods declared in `RowProduct`,
  * useful only when operating on the most abstract level, as most domain specific functions are available only
  * for more specialized types.
  *
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
  * @author Marcin Mościcki
  */ //other words :Expand Tack, Include
trait Expanded[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Adjoin[L, R] with ExpandingClause[L] with NonEmptyFromTemplate[L Expanded R, L Expanded R]
{ thisClause =>

	override type FromLast >: Generalized <: RowProduct Expanded R

	override type Generalized >: Dealiased <: (left.Generalized Expanded R) {
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self Expanded R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
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

	override type Self <: (left.Self Expanded R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
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
	def narrow :left.type Expanded R


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Expanded.*]


	private[sql] override def concrete_ExpandingClause_subclass_must_extend_Expanded_or_ExpandedDecorator :Nothing =
		throw new UnsupportedOperationException

}






object Expanded {


	/** An existential upper bound of all `Expanded` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Expanded[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Expanded` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Expanded R }

	/** A curried type constructor for `Expanded` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Expanded R }



	/** A base trait for implementations of [[net.noresttherein.oldsql.sql.Expanded Expanded]] which narrows
	  * down the mapping type to the actually required `BaseMapping` (due to some scala bug preventing
	  * the use of `RefinedMapping` in the `TypedComponentSQL` instead).
	  */
	trait AbstractExpanded[+L <: RowProduct, R[O] <: BaseMapping[S, O], S] extends Expanded[L, R] { thisClause =>

		protected def lastRelation :RelationSQL[FromLast, R, S, FromLast]

		override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			lastRelation.expand(target) #:: left.fullTableStack(target)(expansion.expandFront[left.Generalized, R])


		override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			lastRelation.expand(target) #:: left.tableStack(target)(expansion.expandFront[left.Generalized, R])
	}






	/** A marker trait for [[net.noresttherein.oldsql.sql.Expanded Expanded]] implementations other than
	  * the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. While not mandatory, it is recommended
	  * that all such clauses expand this type, as several implicit parameters responsible for traversing
	  * composite ''from'' clauses, in particular accessors for the joined relations, depend on any `Expanded` type
	  * to conform to either it, or the `Subselect`, with no provision for other cases. Note that this holds only
	  * for `Expanded` subtypes, and there are other clauses, in particular
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], which are neither.
	  */ //todo: update docs with usage once With is fully incorporated
	trait NonSubselect[+L <: RowProduct, R[O] <: MappingAt[O]]
		extends Expanded[L, R] with NonEmptyFromTemplate[L NonSubselect R, L NonSubselect R]
	{ thisClause =>
		override def isSubselectParameterized :Boolean = left.isSubselectParameterized

		override type Implicit = left.Implicit
		override type Outer = left.Outer

		override type OuterRow = left.OuterRow

		override def outerRow[E <: RowProduct]
		             (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, GlobalScope, OuterRow] =
			left.outerRow(target)
	}






	@implicitNotFound("I do not know how to decompose ${F} into an Expanded subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExpandedDecomposition[${F}, ${L}, ${R}, ${J}, ${U}, ${M}].")
	class ExpandedDecomposition[-F <: L J R, L <: U, R[O] <: MappingAt[O],
	                            J[+A <: U, B[O] <: R[O]] <: A Expanded B, U <: RowProduct]
		extends RowDecomposition[F, L, U]
	{
		override type E[+A <: U] = A J R
		override type S[+A >: L <: U] = A J R

		@inline final override def prefix[A >: L <: U] :A PrefixOf (A J R) = new PrefixOf[A, A J R](1)
		@inline final override def expansion[A <: U] :A PrefixOf (A J R) = new PrefixOf[A, A J R](1)

		@inline final override def unapply(join :F) :L = join.left

		override def upcast[A >: L <: U] :ExpandedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExpandedDecomposition[A J R, A, R, J, U]]

		override def cast[A <: U] :ExpandedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExpandedDecomposition[A J R, A, R, J, U]]
	}



	@implicitNotFound("I do not know how to decompose ${F} into an Expanded subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExpandedComposition[${F}, ${L}, ${R}, ${J}, ${U}, ${M}].")
	abstract class ExpandedComposition[F <: L J R, L <: U, R[O] <: M[O],
	                                   J[+A <: U, B[O] <: M[O]] <: A Expanded B, U <: RowProduct, M[O] <: MappingAt[O]]
		extends ExpandedDecomposition[F, L, R, J, U] with RowComposition[F, L, U]
	{ self =>
		override def upcast[A >: L <: U] :ExpandedComposition[A J R, A, R, J, U, M] =
			this.asInstanceOf[ExpandedComposition[A J R, A, R, J, U, M]]

		override def cast[A <: U] :ExpandedComposition[A J R, A, R, J, U, M] =
			this.asInstanceOf[ExpandedComposition[A J R, A, R, J, U, M]]
	}


//	implicit def expandedDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
//			:ExpandedDecomposition[L Expanded R, L, R, Expanded, RowProduct] =
//		decomposition.asInstanceOf[ExpandedDecomposition[L Expanded R, L, R, Expanded, RowProduct]]

	private[this] val decomposition =
		new ExpandedDecomposition[RowProduct Expanded MappingAt, RowProduct, MappingAt, Expanded, RowProduct]

}
