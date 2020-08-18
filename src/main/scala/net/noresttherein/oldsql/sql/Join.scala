package net.noresttherein.oldsql.sql


import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, FromSome}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.{BaseJoin, JoinedRelationSubject}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.TrueJoin.BaseTrueJoin
import net.noresttherein.oldsql.sql.SQLScribe.ReplaceRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.AndFrom.BaseAndFrom
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain






/** Common upper type for `Join` subclasses which accept arbitrary `Mapping` subtypes as joined relations.
  * It is the root of a tree with two branches: [[net.noresttherein.oldsql.sql.TrueJoin TrueJoin]] subtypes
  * which represent various actual join kinds (inner, outer, etc) and [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * which is a synthetic linearization of the ''from'' clause of a subselect joined with the ''from'' clause
  * of its outer select.
  */ //todo: seal it after moving Subselect in
trait Join[+L <: FromSome, R[O] <: MappingAt[O]] extends AndFrom[L, R] { thisClause =>

	/** The generalized version of this join kind: it is `TrueJoin` for all real joins and `Subselect` for `Subselect`. */
	type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F Join T) {
		type GeneralizedJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
	}

	/** This join type, fully parameterized with arbitrary prefix clause and relation mapping. Used by copy constructors
	  * of this class.
	  * @see [[net.noresttherein.oldsql.sql.Join.likeJoin]]
	  */
	type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F GeneralizedJoin T) {
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
		type GeneralizedJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
	}

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join. It is the lower
	  * level variant of the other `likeJoin` method which accepts a `Relation[T]`
	  * @param left the ''from'' clause containing all relations preceding `right` in the new clause.
	  * @param right a `RelationSQL[FromClause AndFrom T, T, S, FromClause AndFrom T]` representing the last joined relation.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :LastRelation[T, X])(filter: SQLBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :Relation[T])(filter: SQLBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		likeJoin(left, LastRelation[T, X](right))(filter)

	/** Joins the two clauses using this join kind. If the first join of the second, `right` clause
	  * is not the `From` pseudo join, the existing join is used instead of this join.
	  * @throws UnsupportedOperationException if this clause is a `Subselect` and the right clause contains
	  *                                       a `JoinParam` synthetic join.
	  */
	def likeJoin[P <: FromSome, S <: FromSome](left :P, right :S) :right.JoinedWith[P, LikeJoin]


	override type GeneralizedLeft[+F <: FromSome] = F GeneralizedJoin R
	override type WithLeft[+F <: FromSome] = F LikeJoin R

	/** Creates a `FromClause` of the same class as this one between the current `left` side of this clause
	  * and the new `right` relation, using the provided `filter` as the `condition` stored in the created join.
	  * @see [[net.noresttherein.oldsql.sql.Join.likeJoin likeJoin]]
	  */
	def withRight[T[O] <: BaseMapping[X, O], X]
	             (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized GeneralizedJoin T]) :L LikeJoin T =
		likeJoin(left, right)(filter)

	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `FromClause`, as they become proper path types instead of projections.
	  */
	protected def narrow :WithLeft[left.type]

	override type Generalized = left.Generalized GeneralizedJoin R
	override type Self = left.Self LikeJoin R
	override type This >: this.type <: L Join R



	/** Apply a join condition to the last two relations in this clause. The condition is combined using `&&` with
	  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * This works exactly like 'where', but instead of a single argument representing all joined relations,
	  * the filter function should take as its arguments the last two relations, i.e, the last relation defined
	  * by the left side of this join, if any, and the right side of this join.
	  * @param condition a function accepting the expressions for the last two relations in this clause and creating
	  *                  an SQL expression for the join condition.
	  * @return a `Join` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  */
	def on(condition :(left.LastTable[left.FromLast GeneralizedJoin R], LastTable[FromLast])
		=> SQLBoolean[left.FromLast GeneralizedJoin R]) :This =
	{
		val joinFilter = condition(left.last.extend[GeneralizedLeft], last)
		val grounded = SQLScribe.groundFreeComponents(generalized, joinFilter)
		where(grounded)
	}



	override type AppendedTo[+P <: FromClause] = left.AppendedTo[P] LikeJoin R

	override type JoinedWith[+P <: FromSome, +J[+K <: FromSome, T[O] <: MappingAt[O]] <: K Join T] =
		left.JoinedWith[P, J] LikeJoin R


	override type Params = left.Params



	/** Specify an alias for the last relation in the join. This is not necessary and may be overriden
	  * in case of conflicts, but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the relation as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last relation (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :L LikeJoin (R As A)#T
	//todo: as for non-literals

//todo: def by(fk :R[FromLast] => ForeignKey[FromLast]) :L LikeJoin R
}






/** Factory and matching pattern for [[net.noresttherein.oldsql.sql.Join Join]] classes. */
object Join {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `Join` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :Join[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `Join` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)]  = from match {
		case join :Join.* => Some((join.left, join.right))
		case _ => None
	}






	/** An implicit witness serving as type inference helper for the `Mapping` type of a joined
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]. It separates the inference of the mapping type itself
	  * from its `Subject` type, thus allowing for both to function properly in methods with the type parameters
	  * in the form of `[R &lt;: BaseMapping[S, _], S]` (and similar). It is used in particular when constructing new
	  * `Join` instances to remove explicit type parameters from the call. Apart from this purely independent role,
	  * it provides identity casting methods, similar to those of `=:=`, for various relevant types
	  * (`SQLExpression`, `Relation`, `J`) between those parameterized with `R` and `T` to eliminate the need
	  * for casting inside methods using this support.
	  * @tparam J a type constructor for a `AndFrom` 'join' extending some ''from'' clause with the provided mapping type.
	  * @tparam R a type constructor for a `Mapping` accepting its `Origin` type; it is the 'input' type used by the
	  *           arguments of the method accepting this instance, declared only with the `MappingAt[O]` upper bound.
	  * @tparam T the type `R`, but with the upper bound of `U`. It generally should not be used in the method signature
	  *           (in particular, it cannot appear before this argument), but can be used in the method's body.
	  *           As the type argument `U` is generally a concrete type, the type `R` is typically a type parameter
	  *           with an explicit upper bound consistent with `U`, such as `R[O] &lt;: BaseMapping[S, O]`.
	  * @tparam U an upper bound for the mapping type `R` (and `T`) required for the implementation.
	  *           It is a concrete type with only its type parameters being abstract, typically given as
	  *           `BaseMapping[S, O]`, where `S` is a free type parameter of the method. It allows the inference
	  *           of its (and, by extension, `R`'s) type parameters (`S`).
	  */
	@implicitNotFound("Failed to infer the Subject type of mapping ${R}: cannot prove that " +
		              "${R}[O] <: ${T}[O] with ${U}. This may be caused by the inferred type ${T} or its subject type S " +
	                  "occurring before the implicit parameter JoinedRelationSubject[${J}, ${R}, ${T}, ${U}] " +
	                  "(alias InferSubject[?, ?, ${R}, ${T}, ?]) or in the method's result type.")
	sealed abstract class JoinedRelationSubject[J[M[O] <: MappingAt[O]] <: _ AndFrom M,
	                                            R[O] <: MappingAt[O], T[O] <: U[O], +U[O] <: BaseMapping[_, O]]
		extends (J[T] => J[R])
	{
		def apply(rows :Relation[R]) :Relation[T]

		def apply(join :J[T]) :J[R]

		def downtype[F[M[O] <: MappingAt[O]] <: _ AndFrom M, X](e :SQLExpression[F[R], X]) :SQLExpression[F[T], X]

		def downtype[F[M[O] <: MappingAt[O]] <: _ AndFrom M, X](e :ColumnSQL[F[R], X]) :ColumnSQL[F[T], X]

		def apply[F <: FromSome, L[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
                 (e :ColumnSQL[F L R, X]) :ColumnSQL[F L T, X]

		def apply[F <: FromSome, L[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
		         (e :SQLExpression[F L R, X]) :SQLExpression[F L T, X]

		def self :JoinedRelationSubject[J, T, T, U]
	}



	object JoinedRelationSubject {
		import BaseMapping.AnyAt
		private[this] val instance =
			new JoinedRelationSubject[({ type F[M[O] <: MappingAt[O]] = FromClause AndFrom M })#F, //JoinWith[FromSome, Join]#F,
	                                  BaseMapping.AnyAt, BaseMapping.AnyAt, BaseMapping.AnyAt]
			{
				override def apply(rows :Relation[BaseMapping.AnyAt]) = rows

				override def apply(join :FromClause AndFrom BaseMapping.AnyAt) = join

				override def downtype[F[M[O] <: MappingAt[O]] <: _ AndFrom M, X](e :ColumnSQL[F[AnyAt], X]) = e
				override def downtype[F[M[O] <: MappingAt[O]] <: _ AndFrom M, X](e :SQLExpression[F[AnyAt], X]) = e

				override def apply[F <: FromSome, L[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
				                  (e :ColumnSQL[L[F, AnyAt], X]) = e

				override def apply[F <: FromSome, L[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
				                  (e :SQLExpression[L[F, AnyAt], X]) = e

				override def self = this
			}

		implicit def identity[J[M[O] <: MappingAt[O]] <: _ AndFrom M, R[O] <: BaseMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]


		/** A simplified form of the implicit witness
		  * [[net.noresttherein.oldsql.sql.Join.JoinedRelationSubject JoinedRelationSubject]], it guides the compiler
		  * into proper inference of both the type of a mapping, and its `Subject` type as defined by `BaseMapping`.
		  * It is used when constructing a new `Join` subtype to avoid the need for explicit specification
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
		type InferSubject[L <: FromSome, J[+F <: FromSome, M[O] <: MappingAt[O]] <: F Join M,
		                  R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S] =
			JoinedRelationSubject[JoinWith[L, J]#F, R, T, MappingOf[S]#TypedProjection]

		type JoinWith[L <: FromSome, J[F <: FromSome, R[O] <: MappingAt[O]] <: F Join R] = {
			type F[R[O] <: MappingAt[O]] = L J R
		}
	}






	/** Type alias for `Join` with erased type parameters, covering all instances of `Join`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Join[_ <: FromSome, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `Join` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L Join R }

	/** A curried type constructor for `Join` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L Join R }



	/** A mixin trait for concrete `Join` implementations which mandates that the mapping class `R` of the right side
	  * is a subclass of `BaseMapping`. It is introduced to circumvent a bug in the scala compiler
	  * [[https://github.com/scala/bug/issues/11996]] which prevents the use of `RefinedMapping` instead.
	  */
	private[sql] trait BaseJoin[+L <: FromSome, R[O] <: BaseMapping[S, O], S]
		extends BaseAndFrom[L, R, S] with Join[L, R]
	{
		override def withLeft[F <: FromSome](newLeft :F)(filter :SQLBoolean[newLeft.Generalized GeneralizedJoin R])
				:F LikeJoin R =
			likeJoin[F, R, S](newLeft, right)(filter)

		override def appendedTo[P <: FromClause](prefix :P) :AppendedTo[P] =
			withLeft(left.appendedTo(prefix))(condition :SQLBoolean[left.Generalized GeneralizedJoin R])

		override def joinedWith[F <: FromSome](prefix :F, firstJoin :TrueJoin.*)
				:left.JoinedWith[F, firstJoin.LikeJoin] LikeJoin R =
			withLeft(left.joinedWith(prefix, firstJoin))(condition :SQLBoolean[left.Generalized GeneralizedJoin R])

		override def joinedAsSubselect[F <: FromSome](prefix :F) :left.JoinedWith[F, Subselect] LikeJoin R =
			withLeft(left.joinedAsSubselect(prefix))(condition :SQLBoolean[left.Generalized GeneralizedJoin R])

		override def as[A <: Label](alias :A) :L LikeJoin (R As A)#T = {
			val source = FromClause.AliasedRelation[R, A](last.relation, alias)
			val aliased = RelationSQL.last[(R As A)#T, (R As A)#T, S](source)
			type Res = left.Generalized AndFrom (R As A)#T //todo: condition from a function
			val unfiltered = likeJoin[left.Generalized, (R As A)#T, S](left.generalized, source)(True)
			val replacement = aliased \ (unfiltered.last.mapping.body :R[FromClause AndFrom (R As A)#T])
			val substitute = new ReplaceRelation[R, S, (R As A)#T, S, Generalized, Res](
				generalized, unfiltered)(last, replacement)
			withRight[(R As A)#T, S](aliased)(substitute(condition))
		}
	}

}






/** Base trait for join implementations representing a real SQL join between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression. Subclasses represent various join kinds
  * (inner, outer, left outer, right outer).
  * It is the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form of all extending classes, meaning
  * that all join conditions for subclasses use this type instead the proper concrete subclass in the `FromClause`
  * parameter of the `SQLBoolean`.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  */
sealed trait TrueJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F TrueJoin T

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F TrueJoin T) {
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	override type This >: this.type <: L TrueJoin R

	override def likeJoin[P <: FromSome, S <: FromClause](left :P, right :S) :right.JoinedWith[P, LikeJoin] =
		right.joinedWith(left, this)

	override def subselectSize :Int = left.subselectSize + 1

	override type Explicit = left.Explicit TrueJoin R
	override type Inner = left.Inner LikeJoin R
	override type Implicit = left.Implicit
	override type Outer = left.Outer

	override def outer :Outer = left.outer



	override type SubselectRow = left.SubselectRow ~ last.Subject

	override def subselectRow[E <: FromSome]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(target)(extension.stretchFront[left.Generalized, R]) ~ last.stretch(target)(extension)

	override type AsSubselectOf[+F <: FromSome] =
		(left.AsSubselectOf[F] LikeJoin R) { type Explicit = thisClause.Explicit }


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[TrueJoin.*]

}






object TrueJoin {


	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `TrueJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :TrueJoin[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `TrueJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)]  = from match {
		case join :TrueJoin.* => Some((join.left, join.right))
		case _ => None
	}



	/** Type alias for `TrueJoin` with erased type parameters, covering all instances of `TrueJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = TrueJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `TrueJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L TrueJoin R }

	/** A curried type constructor for `TrueJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L TrueJoin R }


	private[sql] trait BaseTrueJoin[+L <: FromSome, R[O] <: BaseMapping[S, O], S]
		extends BaseJoin[L, R, S] with TrueJoin[L, R]
	{ thisJoin =>

		override def subselectTableStack[E <: FromSome]
		             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
			last.stretch[Generalized, E](target) #::
				left.subselectTableStack(target)(stretch.stretchFront[left.Generalized, R])

		override def asSubselectOf[F <: FromSome](newOuter :F)(implicit extension :Implicit ExtendedBy F)
				:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		{
			val newLeft = left.asSubselectOf(newOuter)
			//todo: refactor joins so they take functions creating conditions and move this to the constructor
			//  this would however introduce problem with Join.as: one of the relation becoming unavailable
			val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
			val substitute = AndFrom.shiftBack[Generalized, newLeft.Generalized TrueJoin R](
				generalized, unfiltered, extension.length, subselectSize + 1
			)
			withLeft[newLeft.type](newLeft)(substitute(condition))
		}

	}


}






/** A standard join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, filtered by the conjunction of this join's `condition` with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends TrueJoin[L, R] {

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F InnerJoin T
	override type This >: this.type <: L InnerJoin R

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized TrueJoin T]) :F InnerJoin T =
		InnerJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin.*]

	override def joinName :String = "join"

}






object InnerJoin {

	/** A template `InnerJoin` instance with a dummy mapping, for use as a polymorphic factory of `InnerJoin` joins. */
	final val template :InnerJoin.* = InnerJoin(Relation.Dummy, Relation.Dummy)

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		castR(InnerJoin(From(left), LastRelation[RB, B](castR(right)))(True))

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized TrueJoin R]) :L InnerJoin R =
		new InnerJoin[prefix.type, R] with BaseTrueJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type InnerJoin R

			protected override def narrow :left.type InnerJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :SQLBoolean[Generalized]) =
				InnerJoin[left.type, R, S](left, last)(filter)

		}



	/** Matches all `InnerJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :InnerJoin[_, _] => Some(from.left -> from.right)
		case _ => None
	}

	/** Matches all `InnerJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case join :InnerJoin.* => Some((join.left, join.right))
		case _ => None
	}



	/** Type alias for `InnerJoin` with erased type parameters, covering all instances of `InnerJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = InnerJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `InnerJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L InnerJoin R }

	/** A curried type constructor for `InnerJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L InnerJoin R }

}






/** A symmetrical outer join of two (or more) SQL relations, that is one where every row in either of them occurs
  * at least once in the result, matched with an artificial row consisting of only null values from the other table
  * if no true match exist. The left side, which may consist of any number of relations greater than zero, is treated
  * as a single relation for this purpose. The join is, filtered by the conjunction of this join's `condition`
  * with the combined filter expression of the left side.
  */
sealed trait OuterJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends TrueJoin[L, R] {

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F OuterJoin T
	override type This >: this.type <: L OuterJoin R

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized TrueJoin T]) :F OuterJoin T =
		OuterJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[OuterJoin.*]

	override def joinName :String = "outer join"

}






object OuterJoin {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :OuterJoin.* = OuterJoin(Relation.Dummy, Relation.Dummy)

	/** Create an outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#outerJoin outerJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	  * @return an unfiltered `From[L] OuterJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], OuterJoin, R, RB, B])
			:From[L] OuterJoin R =
		castR(apply(From(left), LastRelation[RB, B](castR(right)))(True))

	/** Create an outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#outerJoin outerJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, OuterJoin, R, T, S]) :L OuterJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized TrueJoin R]) :L OuterJoin R =
		new OuterJoin[prefix.type, R] with BaseTrueJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type OuterJoin R

			protected override def narrow :left.type OuterJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :SQLBoolean[Generalized]) =
				OuterJoin[left.type, R, S](left, last)(filter)

		}



	/** Matches all `OuterJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :OuterJoin[_, _] => Some(from.left -> from.right)
		case _ => None
	}

	/** Matches all `OuterJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case join :OuterJoin.* => Some((join.left, join.right))
		case _ => None
	}



	/** Type alias for `OuterJoin` with erased type parameters, covering all instances of `OuterJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = OuterJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `OuterJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L OuterJoin R }

	/** A curried type constructor for `OuterJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L OuterJoin R }

}






sealed trait LeftJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends TrueJoin[L, R] {

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F LeftJoin T
	override type This >: this.type <: L LeftJoin R

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized TrueJoin T]) :F LeftJoin T =
		LeftJoin(left, right)(filter)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin.*]

	override def joinName = "left join"

}






object LeftJoin {

	/** A template `LeftJoin` instance with a dummy mapping, for use as a polymorphic factory of `LeftJoin` joins. */
	final val template :LeftJoin.* = LeftJoin(Relation.Dummy, Relation.Dummy)

	/** Create a left outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#leftJoin leftJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	  * @return an unfiltered `From[L] LeftJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], LeftJoin, R, RB, B])
			:From[L] LeftJoin R =
		castR(apply(From(left), LastRelation[RB, B](castR(right)))(True))

	/** Create a left outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#leftJoin leftJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L LeftJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, LeftJoin, R, T, S]) :L LeftJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[A] <: BaseMapping[S, A], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized TrueJoin R]) :L LeftJoin R =
		new LeftJoin[prefix.type, R] with BaseTrueJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type LeftJoin R

			protected override def narrow :left.type LeftJoin R = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				LeftJoin[left.type, R, S](left, last)(filter)

		}



	/** Matches all `LeftJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :LeftJoin[_, _] => Some(from.left -> from.right)
		case _ => None
	}

	/** Matches all `LeftJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case join :LeftJoin.* => Some((join.left, join.right))
		case _ => None
	}



	/** Type alias for `LeftJoin` with erased type parameters, covering all instances of `LeftJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = LeftJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `LeftJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L LeftJoin R }

	/** A curried type constructor for `LeftJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L LeftJoin R }

}






sealed trait RightJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends TrueJoin[L, R] {

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F RightJoin T
	override type This >: this.type <: L RightJoin R


	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized TrueJoin T]) :F RightJoin T =
		RightJoin(left, right)(filter)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin.*]

	override def joinName = "right join"

}






object RightJoin {

	/** A template `RightJoin` instance with a dummy mapping, used as a polymorphic factory of `RightJoin` joins.  */
	final val template : RightJoin.* = RightJoin(Relation.Dummy, Relation.Dummy)


	/** Create a right outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#rightJoin rightJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] &lt;: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] &lt;: BaseMapping[B, O]` `Mapping` type.
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
	  * @return an unfiltered `From[L] RightJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], RightJoin, R, RB, B])
			:From[L] RightJoin R =
		castR(RightJoin(From(left), LastRelation[RB, B](castR(right)))(True))

	/** Create a right outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#rightJoin rightJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L RightJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized TrueJoin R] = True)
	         (implicit cast :InferSubject[L, RightJoin, R, T, S]) :L RightJoin R =
		cast(RightJoin[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized TrueJoin R]) :L RightJoin R =
		new RightJoin[prefix.type, R] with BaseTrueJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type RightJoin R

			protected override def narrow = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				RightJoin[left.type, R, S](left, last)(filter)

		}



	/** Matches all `RightJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L AndFrom R) :Option[(L, Relation[R])] = from match {
		case _ :RightJoin[_, _] => Some(from.left -> from.right)
		case _ => None
	}

	/** Matches all `RightJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case join :RightJoin.* => Some((join.left, join.right))
		case _ => None
	}



	/** Type alias for `RightJoin` with erased type parameters, covering all instances of `RightJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = RightJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `RightJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L RightJoin R }

	/** A curried type constructor for `RightJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L RightJoin R }

}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is a specialized subclass of `AndFrom[Dual, T]`, so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`. For all intents and purposes it is treated
  * as `Dual InnerJoin T` (which would be illegal due to the restriction of the left side of joins to non empty clauses).
  * All non empty ''from'' clauses start with this instance unless the first 'relation' is an unbound join parameter
  * represented by a `JoinParam`.
  */ //todo: move it to AndFrom;
sealed trait From[T[O] <: MappingAt[O]] extends AndFrom[Dual, T] {
	override val left :Dual = Dual

	def table :Relation[T] = last.relation

	override def filter :SQLBoolean[FromClause AndFrom T] =
		if (left.filter eq True) condition else left.filter && condition

	override type GeneralizedLeft[+L <: FromClause] = L AndFrom T
	override type WithLeft[+L <: FromClause] = L AndFrom T
	override type Generalized = FromClause AndFrom T
	override type Self = From[T]
	override type This >: this.type <: From[T]

	override type AppendedTo[+P <: FromClause] = P AndFrom T

	override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] = P J T


	override type Implicit = FromClause
	override type Outer = Dual
	override type Explicit = FromClause AndFrom T
	override type Inner = FromClause AndFrom T

	override def outer :Dual = left
	override def subselectSize = 1

	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromSome]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		EmptyChain ~ last.stretch(target)(extension)


	override type AsSubselectOf[+F <: FromSome] = F Subselect T


	override type Params = @~


	/** Specify an alias for this relation. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the relation as in 'from users as u'.
	  * @return a new `From` instance, with a new relation (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :From[(T As A)#T]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.*]

	override def joinName :String = "from"

	override def toString :String =
		if (filter == True()) "from " + last.relation
		else "from " + last.relation + " where " + filter

}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `AndFrom` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) whereLast (_.name === "Jack")
  *     val guests = From(Hotels) join GuestsBook on (_.id === _.hotelId) join People on (_.guestId === _.id)
  * }}}
  */
object From {

	/** A template `From` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
	final val template :From.* = From(Relation.Dummy)

	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. It can be later filtered using
	  * the [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast]] method, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`,
	  *                 represented here by types `R =:= T`, split in order to separate the inference of the mapping
	  *                 type and its subject type and remove the need to specify the type parameter for the method
	  *                 explicitly.
	  * @param cast an implicit witness providing the type inference of the subject type of the relation's mapping
	  *             as well as casting functions between associated classes parameterized with `T` and `R`.
	  * @return an unfiltered `From[R]`.
	  */
	def apply[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (relation :Relation[R])(implicit cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
			:From[R] =
		cast(From(LastRelation[T, S](cast(relation)), True))


	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. This is a lower level factory method accepting an optional, additional filter
	  * for the ''where'' clause, but providing no type inference support and thus generally requiring the type
	  * parameters to be specified explicitly. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`.
	  * @param filter an optional boolean condition for the associated ''where'' clause.
	  * @return a `From[R]`.
	  */
	def apply[T[O] <: BaseMapping[S, O], S]
	         (relation :Relation[T], filter: SQLBoolean[FromClause AndFrom T] = True) :From[T] =
		From(LastRelation[T, S](relation), filter)


	private[sql] def apply[T[O] <: BaseMapping[S, O], S]
	                      (relation :LastRelation[T, S], filter :SQLBoolean[FromClause AndFrom T]) :From[T] =
		narrow(Dual, relation, filter)

	private[sql] def narrow[T[O] <: BaseMapping[S, O], S]
	                       (dual :Dual, relation :LastRelation[T, S], filter :SQLBoolean[FromClause AndFrom T])
			:From[T] with (dual.type AndFrom T) =
		new From[T] with BaseAndFrom[dual.type, T, S] {
			override val left :dual.type = dual
			override val last = relation
			override val condition = filter
			override def size = 1

			override def narrow :AndFrom[dual.type, T] = this
			override type This = From[T] with (dual.type AndFrom T)


			override def withCondition(filter :SQLBoolean[FromClause AndFrom T]) =
				From.narrow(left, last, filter)

			override def withLeft[F <: FromClause](newLeft :F)(filter :SQLBoolean[newLeft.Generalized AndFrom T])
					:F AndFrom T =
				newLeft.extend(last, filter)


			override def appendedTo[P <: FromClause](prefix :P) :P AndFrom T = prefix.extend(last, filter)

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :TrueJoin.*) :firstJoin.LikeJoin[F, T] =
				firstJoin.likeJoin[F, T, S](prefix, right)(condition)

			override def joinedAsSubselect[F <: FromSome](prefix :F) :Subselect[F, T] =
				Subselect[F, T, S](prefix, last)(condition)


			override def subselectTableStack[E <: FromSome]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def asSubselectOf[F <: FromSome](newOuter :F)(implicit extension :Implicit ExtendedBy F)
					:newOuter.type Subselect T =
				Subselect[newOuter.type, T, S](newOuter, last)(condition)


			override def as[A <: Label](alias :A) :From[(T As A)#T] = {
				val source = FromClause.AliasedRelation[T, A](last.relation, alias)
				val aliased = RelationSQL.last[(T As A)#T, (T As A)#T, S](source)
				type Res = FromClause AndFrom (T As A)#T //todo: condition from a function
				val unfiltered = From[(T As A)#T, S](aliased, True)
				val replacement = aliased \ (unfiltered.last.mapping.body :T[FromClause AndFrom (T As A)#T])
				val substitute = new ReplaceRelation[T, S, (T As A)#T, S, Generalized, Res](
					generalized, unfiltered)(last, replacement
                )
				From[(T As A)#T, S](aliased, substitute(condition))
			}

		}



	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply[M[O] <: MappingAt[O]](from :FromClause AndFrom M) :Option[Relation[M]] = from match {
		case _ :From.* => Some(from.right)
		case _ => None
	}

	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply(from :FromClause) :Option[Relation.*] = from match {
		case f :From.* => Some(f.table)
		case _ => None
	}

	/** Type alias for `From` with the erased type parameter, covering all instances of `From`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = From[M] forSome { type M[O] <: MappingAt[O] }

}

