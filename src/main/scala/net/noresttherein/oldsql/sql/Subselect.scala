package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.BaseJoin
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple






/** A special artificial join type serving as a ''from'' clause for subselects (selects occurring inside another select,
  * either in the ''select'' or ''where'' clause). It has direct access to all tables from the outer clause,
  * while providing its own additional relations in its (explicit) ''from'' clause. It is represented as a join between
  * the outer clause (the left side), forming the 'implicit' portion of this clause, and the first member
  * of the subselect's ''from'' list (the right side), starting the 'explicit' portion of this clause.
  * Joining additional relations has the effect of adding them to that 'explicit' portion, available only to the
  * subselect (and, potentially, its own subselects). This allows any SQL expressions based on the outer select's clause
  * to be used as expressions based on this select, as it doesn't differ from any other `FromClause` join-like extension.
  * Note that it is possible to recursively nest subselects to an arbitrary depth and it is modelled by a repeated use
  * of this join type. In that case all relations/mappings to the left of the first occurrence of `Subselect`
  * in the type definition are the ''from'' clause of the most outer, independent select, while relations/mappings
  * in between subsequent `Subselect`s form the ''from'' clauses of subsequent nested selects,
  * with any relations/mappings following its last occurrence in the type being exclusive to the deepest subselect
  * in the chain.
  *
  * `FromClause` subtypes which contain a `Subselect` in their (dynamic) type definition are called ''subselect''
  * clauses, regardless of whether `Subselect` is the last 'join' or occurs before. On the other hand, clauses
  * without a `Subselect` in their concrete definition are called ''outer'' clauses, after the independent, 'outer'
  * select expressions which can be based on them. All subselect clauses conform to
  * [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]], providing the type is instantiated at least
  * to the point of the last `Subselect`. Additionally, direct subselect clauses of some outer clause `F` (that is,
  * those without any `Subselect` to the right of `F` in their type definition) conform to
  * [[net.noresttherein.oldsql.sql.FromClause#Nested F#Nested]]. Outer clauses are also valid ''from'' clauses
  * for a subselect of any other select, thus this relationship is typically established by
  * the [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf[F]]] type alias, which covers both independent
  * clauses and actual subselects (but not those of an indirect subselect, that is with another occurrence of `Subselect`
  * to the right of `F`).
  *
  * @tparam F the type of outer select's ''from'' clause.
  * @tparam T the right side of this join - the first table of the ''from'' clause of the represented subselect.
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]
  */ //todo: move to Join
trait Subselect[+F <: FromSome, T[O] <: MappingAt[O]] extends Join[F, T] { thisClause =>

	override type GeneralizedJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R
	override type LikeJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R
	override type This >: this.type <: F Subselect T

	override def likeJoin[L <: FromSome, R[O] <: BaseMapping[X, O], X]
	             (left :L, right :LastRelation[R, X])(filter :SQLBoolean[left.Generalized Subselect R]) :L Subselect R =
		Subselect(left, right)(filter)

	override def likeJoin[P <: FromSome, S <: FromSome](left :P, right :S) :right.JoinedWith[P, Subselect] =
		right.joinedAsSubselect(left)


	override def isSubselect = true

	override def subselectSize = 1

	override type Explicit = FromClause AndFrom T
	override type Inner = FromSome Subselect T
	override type Implicit = left.Generalized
	override type Outer = left.Self

	override def outer :Outer = left.self



	override def subselectFilter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		condition.stretch(target)


	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromSome]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.stretch(target)(extension)


	override type AsSubselectOf[+O <: FromSome] = O Subselect T

	override def asSubselectOf[O <: FromSome](newOuter :O)(implicit extension :Implicit ExtendedBy O)
			:(O Subselect T) { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		val unfiltered = withLeft[newOuter.type](newOuter)(True)
		val substitute = AndFrom.shiftBack(generalized, unfiltered.generalized, extension.length, 1)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}


	override def joinName = "subselect"

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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `outer` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#subseleect subselect]] `first`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param outer a ''from'' clause containing the list of relations preceding `first`.
	  * @param first the first (and currently only) relation of the actual ''from'' clause of the created subselect,
	  *              using the `T[O] &lt;: BaseMapping[S, O]` `Mapping` type.
	  * @param filter an optional join condition linking the subselect with the outer clause.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L Subselect R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (outer :L, first :Relation[R], filter :SQLBoolean[L#Generalized Subselect R] = True)
	         (implicit cast :InferSubject[L, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[L, T, S](outer, LastRelation[T, S](cast(first)))(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized Subselect R]) :L Subselect R =
		new Subselect[prefix.type, R] with BaseJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type Subselect R

			override def narrow :left.type Subselect R = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				Subselect[left.type, R, S](left, last)(filter)

			override def subselectTableStack[E <: FromSome]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]

		}



	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L AndFrom R) :Option[(L, Relation[R])] = join match {
		case _ :Subselect[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case join :Subselect.* => Some(join.left -> join.right)
		case _ => None
	}



	/** Type alias for `Subselect` with erased type parameters, covering all instances of `Subselect`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Subselect[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Subselect` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L Subselect R }

	/** A curried type constructor for `Subselect` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L Subselect R }

}

