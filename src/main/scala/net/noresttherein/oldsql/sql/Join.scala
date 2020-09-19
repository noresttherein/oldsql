package net.noresttherein.oldsql.sql


import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Relation.As
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, NonEmptyFrom, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.ReplaceRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedComposition, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.Join.AbstractJoin
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope






/** Common upper type for `AndFrom` subclasses which accept arbitrary `Mapping` subtypes as joined relations.
  * It is the root of a tree with two branches: [[net.noresttherein.oldsql.sql.Join Join]] subtypes
  * which represent various actual join kinds (inner, outer, etc) and [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * which is a synthetic linearization of the ''from'' clause of a subselect joined with the ''from'' clause
  * of its outer select.
  */ //this class is almost unused; it seems to serve only the purpose of 'not join param'
sealed trait JoinLike[+L <: FromClause, R[O] <: MappingAt[O]] extends AndFrom[L, R] { thisClause =>

	override type Generalized >: Self <: (left.Generalized JoinLike R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: (left.Self JoinLike R) {
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+S <: P, T[O] <: MappingAt[O]] <: S AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	override type This >: this.type <: (L JoinLike R) {
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+S <: P, T[O] <: MappingAt[O]] <: S AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	protected override def narrow :left.type JoinLike R

	override type GeneralizedLeft[+F <: FromSome] = F GeneralizedJoin R
	override type WithLeft[+F <: FromSome] = F LikeJoin R
	type WithRight[T[O] <: MappingAt[O]] <: L JoinLike T


	/** The generalized version of this join kind: it is `Join` for all real joins and `Subselect` for `Subselect`. */
	type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F JoinLike T) {
		type GeneralizedJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
	}

	/** This join type, fully parameterized with arbitrary prefix clause and relation mapping. Used by copy constructors
	  * of this class.
	  * @see [[net.noresttherein.oldsql.sql.JoinLike#likeJoin]]
	  */
	type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F GeneralizedJoin T) {
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join. It is the lower
	  * level variant of the other `likeJoin` method which accepts a `Relation[T]`
	  * @param left the ''from'' clause containing all relations preceding `right` in the new clause.
	  * @param right a `RelationSQL[FromClause AndFrom T, T, S, FromClause AndFrom T]` representing the last joined relation.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :LastRelation[T, X])(filter: GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :Relation[T])(filter: GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		likeJoin(left, LastRelation[T, X](right))(filter)

	/** Joins the two clauses using this join kind. If the first join of the second, `right` clause
	  * is not the `From` pseudo join, the existing join is used instead of this join.
	  * @throws UnsupportedOperationException if this clause is a `Subselect` and the right clause contains
	  *                                       a `JoinParam` synthetic join.
	  */
	def likeJoin[P <: FromSome, S <: FromSome](left :P, right :S) :right.JoinedWith[P, LikeJoin]


	override def generalizedExtension[P <: FromSome] :P PrefixOf (P GeneralizedJoin R) =
		PrefixOf.itself[P].extend[GeneralizedJoin, R]

	override def extension[P <: FromSome] :P PrefixOf (P LikeJoin R) = PrefixOf.itself[P].extend[LikeJoin, R]


	override type Params = left.Params


	override type DefineBase[+I <: FromClause] = I
	override def base :Base = outer



	/** Specify an alias for the last relation in the join. This is not necessary and may be overriden
	  * in case of conflicts, but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the relation as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last relation (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :WithRight[(R As A)#T]
	//todo: as for non-literals

//todo: def by(fk :R[FromLast] => ForeignKey[FromLast]) :L LikeJoin R

}






/** Factory and matching pattern for [[net.noresttherein.oldsql.sql.JoinLike JoinLike]] classes. */
object JoinLike {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `JoinLike` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
		case _ :JoinLike[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `JoinLike` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromClause, Relation.*)]  = from match {
		case join :JoinLike.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def joinLikeDecomposition[L <: FromClause, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L JoinLike R, L, R, JoinLike, FromClause] =
		decomposition.asInstanceOf[ExtendedDecomposition[L JoinLike R, L, R, JoinLike, FromClause]]

	private[this] val decomposition =
		new ExtendedDecomposition[FromClause JoinLike MappingAt, FromClause, MappingAt, JoinLike, FromClause]



	/** Type alias for `JoinLike` with erased type parameters, covering all instances of `JoinLike`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = JoinLike[_ <: FromClause, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `JoinLike` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: MappingAt[O]] = L JoinLike R }

	/** A curried type constructor for `JoinLike` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromClause] = L JoinLike R }

	type JoinWith[J[F <: FromSome, R[O] <: MappingAt[O]] <: F JoinLike R] = {
		type Left[L <: FromSome] = { type Right[R[O] <: MappingAt[O]] = L J R }
	}

}






/** Base trait for 'true' join implementations representing a real SQL join between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression. Subclasses represent various join kinds
  * (inner, outer, left outer, right outer).
  * It is the [[net.noresttherein.oldsql.sql.FromClause#Generalized generalized]] form of all extending classes, meaning
  * that all join conditions for subclasses use this type instead the proper concrete subclass in the `FromClause`
  * parameter of the `GlobalBoolean`.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  */
sealed trait Join[+L <: FromSome, R[O] <: MappingAt[O]] extends JoinLike[L, R] with NonSubselect[L, R] { thisClause =>

	override type Generalized = left.Generalized Join R
	override type Self = left.Self LikeJoin R

	override type This >: this.type <: (L Join R) {
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
		type JoinedWith[+P <: FromClause, +J[+F <: P, T[O] <: MappingAt[O]] <: F AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}


	protected override def narrow :WithLeft[left.type]

	override type WithRight[T[O] <: MappingAt[O]] <: L LikeJoin T
	override type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F Join T

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: (F Join T) {
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	override def likeJoin[P <: FromSome, S <: FromClause](left :P, right :S) :right.JoinedWith[P, LikeJoin] =
		right.joinedWith(left, this)

	
	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :GlobalBoolean[E] =
		left.filter(target)(extension.extendFront[left.Generalized, R]) && condition.stretch(target)

	
	
	override type JoinedWith[+P <: FromClause, +J[+K <: P, T[O] <: MappingAt[O]] <: K AndFrom T] =
		left.JoinedWith[P, J] LikeJoin R

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = left.JoinedWithSubselect[P] LikeJoin R


	override type Explicit = left.Explicit Join R
	override type Inner = left.Inner LikeJoin R
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type InnerRow = left.InnerRow ~ last.Subject
	override type OuterRow = left.OuterRow


	override type AsSubselectOf[+F <: NonEmptyFrom] = left.AsSubselectOf[F] LikeJoin R

	override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized Join R](
			generalized, unfiltered, extension.length, innerSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Join.*]

}






object Join {


	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `Join` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
		case _ :Join[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `Join` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)]  = from match {
		case join :Join.* => Some((join.left, join.right))
		case _ => None
	}



	@implicitNotFound("I do not know how to decompose ${F} into a join ${L} ${J} ${R}.\n" +
	                  "Missing implicit JoinComposition[${F}, ${L}, ${R}, ${J}].")
	class JoinComposition[L <: FromSome, R[O] <: MappingAt[O],
	                      J[+A <: FromSome, B[O] <: MappingAt[O]] <:
	                        (A JoinLike B) { type LikeJoin[+X <: FromSome, Y[O] <: MappingAt[O]] <: X J Y }]
		extends ExtendedComposition[L J R, L, R, J, FromSome, MappingAt]
	{
		override def apply[C <: FromSome](template :L J R, left :C) :C J R = template.withLeft(left)(True)

		def apply[A <: FromSome, B[O] <: BaseMapping[S, O], S]
		         (template :L J R, left :A, right :Relation[B])
		         (condition :GlobalBoolean[template.GeneralizedJoin[left.Generalized, B]]) :A J B =
			template.likeJoin[A, B, S](left, right)(condition)
	}


	implicit def joinComposition[L <: FromSome, R[O] <: MappingAt[O],
	                             J[+A <: FromSome, B[O] <: MappingAt[O]] <:
	                                (A JoinLike B) { type LikeJoin[+X <: FromSome, Y[O] <: MappingAt[O]] <: X J Y }]
			:JoinComposition[L, R, J] =
		composition.asInstanceOf[JoinComposition[L, R, J]]

	private[this] val composition = new JoinComposition[FromSome, MappingAt, JoinLike]



	/** Type alias for `Join` with erased type parameters, covering all instances of `Join`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Join[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

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



	private[sql] trait AbstractJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S]
		extends AbstractExtended[L, R, S] with Join[L, R]
	{ thisClause =>
		override type WithRight[T[O] <: MappingAt[O]] = L LikeJoin T

		override def withLeft[F <: FromSome](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized GeneralizedJoin R])
				:F LikeJoin R =
			likeJoin[F, R, S](newLeft, last)(filter)


		override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*)
				:left.JoinedWith[F, firstJoin.LikeJoin] LikeJoin R =
			withLeft(left.joinedWith(prefix, firstJoin))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])

		override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :left.JoinedWithSubselect[F] LikeJoin R =
			withLeft(left.joinedWithSubselect(prefix))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])

		override def appendedTo[P <: DiscreteFrom](prefix :P) :JoinedWith[P, AndFrom] =
			withLeft(left.appendedTo(prefix))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])


		override def as[A <: Label](alias :A) :L LikeJoin (R As A)#T = {
			val source = last.relation as[A] alias
			val aliased = RelationSQL.last[(R As A)#T, (R As A)#T, S](source)
			type Res = left.Generalized AndFrom (R As A)#T //todo: condition from a function
			val unfiltered = likeJoin[left.Generalized, (R As A)#T, S](left.generalized, source)(True)
			val replacement = aliased \ (unfiltered.last.mapping.body :R[FromClause AndFrom (R As A)#T])
			val substitute = new ReplaceRelation[R, S, (R As A)#T, S, Generalized, Res](
				generalized, unfiltered)(last, replacement)
			likeJoin[L, (R As A)#T, S](left, aliased)(substitute(condition))
		}
	}

}






/** A standard join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, filtered by the conjunction of this join's `condition` with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override type This >: this.type <: (L InnerJoin R) {
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
		type JoinedWith[+P <: FromClause, +J[+F <: P, T[O] <: MappingAt[O]] <: F AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F InnerJoin T

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :GlobalBoolean[left.Generalized Join T]) :F InnerJoin T =
		InnerJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin.*]

	override def name :String = "join"

}






object InnerJoin {

	/** A template `InnerJoin` instance with a dummy mapping, for use as a polymorphic factory of `InnerJoin` joins. */
	final val template :InnerJoin.* = InnerJoin(Relation.Dummy, Relation.Dummy)

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L InnerJoin R =
		new InnerJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type This = left.type InnerJoin R

			protected override def narrow :left.type InnerJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :GlobalBoolean[Generalized]) =
				InnerJoin[left.type, R, S](left, last)(filter)
		}



	/** Matches all `InnerJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
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
sealed trait OuterJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override type This >: this.type <: (L OuterJoin R) {
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
		type JoinedWith[+P <: FromClause, +J[+F <: P, T[O] <: MappingAt[O]] <: F AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F OuterJoin T

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :GlobalBoolean[left.Generalized Join T]) :F OuterJoin T =
		OuterJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[OuterJoin.*]

	override def name :String = "outer join"

}






object OuterJoin {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :OuterJoin.* = OuterJoin(Relation.Dummy, Relation.Dummy)

	/** Create an outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#outerJoin outerJoin]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#outerJoin outerJoin]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, OuterJoin, R, T, S]) :L OuterJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L OuterJoin R =
		new OuterJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type This = left.type OuterJoin R

			protected override def narrow :left.type OuterJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :GlobalBoolean[Generalized]) =
				OuterJoin[left.type, R, S](left, last)(filter)
		}



	/** Matches all `OuterJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
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






sealed trait LeftJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override type This >: this.type <: (L LeftJoin R) {
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
		type JoinedWith[+P <: FromClause, +J[+F <: P, T[O] <: MappingAt[O]] <: F AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F LeftJoin T

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :GlobalBoolean[left.Generalized Join T]) :F LeftJoin T =
		LeftJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin.*]

	override def name = "left join"

}






object LeftJoin {

	/** A template `LeftJoin` instance with a dummy mapping, for use as a polymorphic factory of `LeftJoin` joins. */
	final val template :LeftJoin.* = LeftJoin(Relation.Dummy, Relation.Dummy)

	/** Create a left outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#leftJoin leftJoin]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#leftJoin leftJoin]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, LeftJoin, R, T, S]) :L LeftJoin R =
		cast(apply[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[A] <: BaseMapping[S, A], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L LeftJoin R =
		new LeftJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type This = left.type LeftJoin R

			protected override def narrow :left.type LeftJoin R = this

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				LeftJoin[left.type, R, S](left, last)(filter)
		}



	/** Matches all `LeftJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
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






sealed trait RightJoin[+L <: FromSome, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override type This >: this.type <: (L RightJoin R) {
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
		type JoinedWith[+P <: FromClause, +J[+F <: P, T[O] <: MappingAt[O]] <: F AndFrom T] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F RightJoin T

	override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	             (left :F, right :LastRelation[T, X])(filter :GlobalBoolean[left.Generalized Join T]) :F RightJoin T =
		RightJoin(left, right)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin.*]

	override def name = "right join"

}






object RightJoin {

	/** A template `RightJoin` instance with a dummy mapping, used as a polymorphic factory of `RightJoin` joins.  */
	final val template : RightJoin.* = RightJoin(Relation.Dummy, Relation.Dummy)


	/** Create a right outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#rightJoin rightJoin]] `right`,
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#rightJoin rightJoin]] `right`
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
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, RightJoin, R, T, S]) :L RightJoin R =
		cast(RightJoin[L, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L RightJoin R =
		new RightJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type This = left.type RightJoin R

			protected override def narrow = this

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				RightJoin[left.type, R, S](left, last)(filter)
		}



	/** Matches all `RightJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
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






/** A special artificial join type serving as a ''from'' clause for subselects (selects occurring inside another select,
  * either in the ''select'' or ''where'' clause). It has direct access to all tables from the outer clause,
  * while providing its own additional relations in its (explicit) ''from'' clause. It is represented as a join between
  * the outer clause (the left side), forming the 'implicit' portion of this clause, and the first member
  * of the subselect's ''from'' list (the right side), starting the 'explicit' portion of this clause.
  * Joining additional relations has the effect of adding them to that 'explicit' portion, available only to the
  * subselect (and, potentially, its own subselects). This allows any SQL expressions based on the outer select's clause
  * to be used as expressions based on this select, as it doesn't differ from any other `FromClause` join-like extension.
  * Note that it is possible to recursively nest subselects to an arbitrary depth, modelled by a repeated use
  * of this join type. In that case all relations/mappings to the left of the first occurrence of `Subselect`
  * in the type definition are the ''from'' clause of the most outer, independent select, while relations/mappings
  * in between subsequent `Subselect`s form the ''from'' clauses of subsequent nested selects,
  * with any relations/mappings following its last occurrence in the type being exclusive to the deepest subselect
  * in the chain.
  *
  * This clause type is referred to in the documentation almost exclusively as a 'subselect join', or `Subselect` join,
  * while `FromClause` subtypes which contain a `Subselect` anywhere in their (dynamic) type definition are called
  * ''subselect'' clauses, regardless of whether `Subselect` is the last 'join' or occurs before. The latter term
  * is also sometimes used for the list of relations from between two subselect boundaries, in particular
  * for all relations to the right of the last `Subselect` join. This does not take into account
  * ''selects'' occurring inside a ''from'' clause as a relation, as they are independent expressions, with none
  * having access to the ''from'' clause relations of the other. On the other hand, clauses without a `Subselect`
  * in their concrete definition are most often called ''outer'' clauses, after the independent,
  * 'outer' select expressions which can be based on them. All subselect clauses conform to
  * [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]], providing the type is instantiated at least
  * to the point of the last `Subselect` and no
  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]] join
  * is present to its right. Additionally, direct subselect clauses of some outer clause `F` (that is,
  * those without any `Subselect` to the right of `F` in their type definition) conform to
  * [[net.noresttherein.oldsql.sql.FromClause#Nested F#Nested]] if both clauses are at least in their generalized form.
  * Outer clauses are also valid ''from'' clauses for a subselect of any other select, but do not conform to any
  * of the above, thus this relationship is typically established instead
  * by the [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[F] ]] type alias, which covers
  * both independent clauses and actual subselects (but not those of an indirect subselect,
  * that is with another occurrence of `Subselect` to the right of `F`). Note that while it is possible to construct
  * a `FromClause` instance with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
  * (or [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) to the right of a subselect join, such a clause
  * will be invalid: it will conform to neither of the types listed above, representing the relation of being
  * a subselect clause, which will make it impossible to construct
  * a [[net.noresttherein.oldsql.sql.SelectSQL SelectSQL]] based on it.
  *
  * @tparam F the type of outer select's ''from'' clause.
  * @tparam T the right side of this join - the first relation of the ''from'' clause of the represented subselect.
  *
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom]]
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf]]
  * @see [[net.noresttherein.oldsql.sql.FromClause.OuterFrom]]
  */
sealed trait Subselect[+F <: NonEmptyFrom, T[O] <: MappingAt[O]] extends JoinLike[F, T] { thisClause =>

	override type Generalized = left.Generalized Subselect T
	override type Self = left.Self Subselect T

	override type This >: this.type <: (F Subselect T) {
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


	override type WithRight[R[O] <: MappingAt[O]] <: F Subselect R
	override type GeneralizedJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R
	override type LikeJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R

	override def withLeft[L <: NonEmptyFrom](left :L)(filter :GlobalBoolean[left.Generalized Subselect T]) :L Subselect T //wider bound

	override def likeJoin[L <: NonEmptyFrom, R[O] <: BaseMapping[X, O], X]
	             (left :L, right :LastRelation[R, X])(filter :GlobalBoolean[left.Generalized Subselect R]) :L Subselect R =
		Subselect[L, R, X](left, right)(filter)

	override def likeJoin[L <: NonEmptyFrom, R[O] <: BaseMapping[X, O], X] //wider bound
	                     (left :L, right :Relation[R])(filter :GlobalBoolean[left.Generalized Subselect R]) :L Subselect R =
		Subselect[L, R, X](left, LastRelation[R, X](right))(filter)

	override def likeJoin[P <: NonEmptyFrom, S <: FromSome](left :P, right :S) :right.JoinedWithSubselect[P] =
		right.joinedWithSubselect(left)



	override def isSubselect = true //if it will accept Dual as the left side the standard definition in FromClause must be changed.
	override def isValidSubselect = true

	override type Explicit = FromClause AndFrom T
	override type Inner = NonEmptyFrom Subselect T
	override type Implicit = left.Generalized
	override type Outer = left.Self



	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :GlobalBoolean[E] =
		condition.stretch(target)


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
		left.JoinedWith[P, J] Subselect T

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = left.JoinedWithSubselect[P] Subselect T



	override type InnerRow = @~ ~ last.Subject

	override def innerRow[E <: FromClause]
	             (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.stretch(target)(extension)


	override type OuterRow = left.FullRow

	override def outerRow[E <: FromClause]
	             (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, GlobalScope, left.FullRow] =
		left.fullRow(target)



	override type AsSubselectOf[+O <: NonEmptyFrom] = O Subselect T

	override def asSubselectOf[O <: NonEmptyFrom](newOuter :O)(implicit extension :Implicit ExtendedBy O)
			:newOuter.type Subselect T =
	{
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		val unfiltered = withLeft[newOuter.type](newOuter)(True)
		val substitute = SQLScribe.shiftBack(generalized, unfiltered.generalized, extension.length, 1)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Subselect.*]

	override def name = "subselect"

}






object Subselect {

	/** A template `Subselect` instance with a dummy mapping, for use as a polymorphic factory of `Subselect` joins. */
	final val template :Subselect.* = Subselect(From(Relation.Dummy), Relation.Dummy)


	/** Create a ''from'' clause of a subselect of a select with `outer` as its ''from'' clause, using the given
	  * relation `first` as the sole member of its (actual) ''from'' clause. The result is a special kind of
	  * an artificial 'join' between the ''implicit'' portion (the ''from'' clause of the outer select, providing
	  * access to all its relations, without them appearing in the ''from'' clause of the generated select SQL)
	  * and the ''explicit'' portion (the relations which actually appear in the ''from'' clause of the generated
	  * subselect SQL), consisting of the relation `first` and any other, subsequently joined with the result.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `outer` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#subseleect subselect]] `first`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `fullFilter` DSL instead.
	  * @param outer a ''from'' clause containing the list of relations preceding `first`.
	  * @param first the first (and currently only) relation of the actual ''from'' clause of the created subselect,
	  *              using the `T[O] &lt;: BaseMapping[S, O]` `Mapping` type.
	  * @param filter an optional join condition linking the subselect with the outer clause.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L Subselect R`.
	  */
	def apply[L <: NonEmptyFrom, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (outer :L, first :Relation[R], filter :GlobalBoolean[L#Generalized Subselect R] = True)
	         (implicit cast :InferSubject[L, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[L, T, S](outer, LastRelation[T, S](cast(first)))(cast(filter)))



	private[sql] def apply[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (cond :GlobalBoolean[prefix.Generalized Subselect R]) :L Subselect R =
		new Subselect[prefix.type, R] with AbstractExtended[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = cond
			override val outer = left.self
			override val fullSize = left.fullSize + 1

			override type This = left.type Subselect R

			override def narrow :left.type Subselect R = this

			override type WithRight[T[O] <: MappingAt[O]] = prefix.type Subselect T

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				Subselect[left.type, R, S](left, last)(filter)

			override def withLeft[F <: NonEmptyFrom](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized Subselect R]) =
				Subselect[F, R, S](newLeft, last)(filter)


			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) =
				withLeft(left.joinedWith(prefix, firstJoin))(condition :GlobalBoolean[left.Generalized Subselect R])

			override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) =
				withLeft(left.joinedWithSubselect(prefix))(condition :GlobalBoolean[left.Generalized Subselect R])

			override def appendedTo[P <: DiscreteFrom](prefix :P) =
				withLeft(left.appendedTo(prefix))(condition :GlobalBoolean[left.Generalized Subselect R])


			override def innerTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def as[A <: Label](alias :A) :left.type Subselect (R As A)#T = {
				val source = last.relation as[A] alias
				val aliased = RelationSQL.last[(R As A)#T, (R As A)#T, S](source)
				type Res = left.Generalized AndFrom (R As A)#T //todo: condition from a function
				val unfiltered = likeJoin[left.Generalized, (R As A)#T, S](left.generalized, source)(True)
				val replacement = aliased \ (unfiltered.last.mapping.body :R[FromClause AndFrom (R As A)#T])
				val substitute = new ReplaceRelation[R, S, (R As A)#T, S, Generalized, Res](
					generalized, unfiltered)(last, replacement)
				Subselect[left.type, (R As A)#T, S](left, aliased)(substitute(condition))
			}

		}



	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L Compound R) :Option[(L, Relation[R])] = join match {
		case _ :Subselect[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply(from :FromClause) :Option[(NonEmptyFrom, Relation.*)] = from match {
		case join :Subselect.* => Some(join.left -> join.right)
		case _ => None
	}



	implicit def subselectComposition[L <: NonEmptyFrom, R[O] <: MappingAt[O]]
			:ExtendedComposition[L Subselect R, L, R, Subselect, NonEmptyFrom, MappingAt] =
		composition.asInstanceOf[ExtendedComposition[L Subselect R, L, R, Subselect, NonEmptyFrom, MappingAt]]

	private[this] val composition =
		new ExtendedComposition[NonEmptyFrom Subselect MappingAt, NonEmptyFrom, MappingAt, Subselect, NonEmptyFrom, MappingAt] {
			override def apply[C <: NonEmptyFrom](template :Subselect[NonEmptyFrom, MappingAt], clause :C) =
				template.withLeft(clause)(True)
		}



	/** Type alias for `Subselect` with erased type parameters, covering all instances of `Subselect`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Subselect[_ <: NonEmptyFrom, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Subselect` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: NonEmptyFrom] = { type F[R[O] <: MappingAt[O]] = L Subselect R }

	/** A curried type constructor for `Subselect` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: NonEmptyFrom] = L Subselect R }

}

