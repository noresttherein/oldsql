package net.noresttherein.oldsql.sql


import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.schema.{RowSource, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.TypedMapping.AnyFrom
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.{JoinedRelationSubject, TypedJoin}
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.ProperJoin.TypedProperJoin
import net.noresttherein.oldsql.sql.SQLScribe.ReplaceRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.With.TypedWith






/** Common upper type for `Join` subclasses which accept arbitrary `Mapping` subtypes as joined relations.
  * This represents all joins of real relations, encompassing all proper joins and subselect 'joins', but
  * not the synthetic `JoinParam`/`WithParam` class which restricts itself to the `FromParam` mapping class.
  */
trait Join[+L <: FromClause, R[O] <: MappingAt[O]] extends With[L, R] { join =>

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join.
	  */
	def likeJoin[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	            (left :F, right :RowSource[T])(filter: SQLBoolean[left.Generalized With T]) :F LikeJoin T

	/** Creates a `FromClause` of the same class as this one between the current `left` side of this clause
	  * and the new `right` relation, using the provided `filter` as the `condition` stored in the created join.
	  * It is not the same as `likeJoin(this.left, right)(filter)`, as it preserves the special status of the `From`
	  * class, which `likeJoin` would replace into an `Dual InnerJoin R`.
	  * @see [[net.noresttherein.oldsql.sql.Join.likeJoin likeJoin]]
	  */
	def withRight[T[O] <: TypedMapping[X, O], X]
	             (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T]) :WithRight[T]


	/** This join type, fully parameterized with arbitrary prefix clause and relation mapping. Used by copy constructors
	  * of this class.
	  * @see [[net.noresttherein.oldsql.sql.Join.likeJoin]]
	  */
	type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] <: (F Join T) {
		type LikeJoin[+S <: FromClause, M[O] <: MappingAt[O]] = join.LikeJoin[S, M]
	}

	override type WithLeft[+F <: FromClause] = F LikeJoin R

	/** The type constructor for this `FromClause` type accepting an arbitrary mapping for the last relation.
	  * @see [[net.noresttherein.oldsql.sql.Join.withRight]]
	  */
	type WithRight[T[O] <: MappingAt[O]] <: L LikeJoin T

	override type Self = left.Self LikeJoin R

	override type This >: this.type <: L Join R

	override type JoinedWith[+P <: FromClause, +J[+K <: FromClause, T[O] <: MappingAt[O]] <: K Join T] =
		left.ExtendJoinedWith[P, J, LikeJoin, R]



	type Params = left.Params



	/** Specify an alias for the last last in the join. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the last as in 'from users as u'
	  * @return a new join isomorphic with this instance, but with a new last last (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :WithRight[(R As A)#T]
//todo: def by(fk :R[FromLast] => ForeignKey[FromLast]) :L LikeJoin R
}






object Join {

	/** Create a cross join between the left side, given as a (possibly empty) clause/list of tables,
	  * and the the mapping on the right side representing a table, some other relation or some temporary table surrogate.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L Join R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)]  =
		from match {
			case join :Join.* => Some((join.left :FromClause, join.right))
			case _ => None
		}






	@implicitNotFound("Failed to infer the Subject type of mapping ${R}: cannot prove that " +
		              "${R}[O] <: ${T}[O] with ${U}. This may be caused by the inferred type ${T} or its subject type S " +
	                  "occurring before the implicit parameter JoinedRelationSubject[${J}, ${R}, ${T}, ${U}] " +
	                  "(alias InferSubject[${J}, ${R}, ${T}, S?]) or in the method's result type.")
	sealed abstract class JoinedRelationSubject[J[M[O] <: MappingAt[O]] <: _ Join M,
	                                            R[O] <: MappingAt[O], T[O] <: U[O], +U[O] <: TypedMapping[_, O]]
		extends (J[T] => J[R])
	{
		def apply(rows :RowSource[R]) :RowSource[T]

		def apply(join :J[T]) :J[R]

		def apply[F <: FromClause, X](e :ColumnSQL[F With R, X]) :ColumnSQL[F With T, X]

		def apply[F <: FromClause, X](e :SQLExpression[F With R, X]) :SQLExpression[F With T, X]

		def self :JoinedRelationSubject[J, T, T, U]
	}



	class DerivedJoinedRelationSubjects {
		@inline implicit def derived[J[M[O] <: MappingAt[O]] <: _ Join M,
		                            R[O] <: MappingAt[O], T[O] <: U[O], U[O] <: TypedMapping[_, O]]
		                           (implicit subject :JoinedRelationSubject[J, R, T, U])
				:JoinedRelationSubject[J, T, T, U] =
			subject.self
	}



	object JoinedRelationSubject extends DerivedJoinedRelationSubjects {
		private[this] val instance =
			new JoinedRelationSubject[JoinWith[FromClause, Join]#F,
	                                  TypedMapping.AnyFrom, TypedMapping.AnyFrom, TypedMapping.AnyFrom]
			{
				override def apply(rows :RowSource[TypedMapping.AnyFrom]) = rows

				override def apply(join :FromClause Join TypedMapping.AnyFrom) = join

				override def apply[F <: FromClause, X](e :ColumnSQL[F With AnyFrom, X]) = e

				override def apply[F <: FromClause, X](e :SQLExpression[F With AnyFrom, X]) = e

				override def self = this
			}

		implicit def identity[J[M[O] <: MappingAt[O]] <: _ Join M, R[O] <: TypedMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]

		type InferSubject[L <: FromClause, J[+F <: FromClause, M[O] <: MappingAt[O]] <: F Join M,
		                  R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S] =
			JoinedRelationSubject[JoinWith[L, J]#F, R, T, MappingOf[S]#TypedProjection]

		type JoinWith[L <: FromClause, J[F <: FromClause, R[O] <: MappingAt[O]] <: F Join R] = {
			type F[R[O] <: MappingAt[O]] = L J R
		}
	}






	/** An existential upper bound of all `Join` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Join[_ <: FromClause, T] forSome { type T[O] <: MappingAt[O] }



	/** A mixin trait for concrete `Join` implementations which mandates that the mapping class `R` of the right side
	  * is a subclass of `TypedMapping`. It is introduced to circumvent a bug in the scala compiler
	  * [[https://github.com/scala/bug/issues/11996]] which prevents the use of `RefinedMapping` instead.
	  */
	trait TypedJoin[+L <: FromClause, R[O] <: TypedMapping[S, O], S]
		extends TypedWith[L, R, S] with Join[L, R]
	{
		override def withLeft[F <: FromClause](newLeft :F)(filter :SQLBoolean[newLeft.Generalized With R])
				:F LikeJoin R =
			likeJoin[F, R, S](newLeft, right)(filter)

		override def joinedWith[F <: FromClause]
		                       (prefix :F, first :Join.*) :JoinedWith[F, first.LikeJoin] =
			left.extendJoinedWith[F, R, S](prefix, first, narrow)

		override def as[A <: Label](alias :A) :WithRight[(R As A)#T] = {
			val source = FromClause.AliasedSource[R, A](last.source, alias)
			val aliased = SQLRelation.last[(R As A)#T, (R As A)#T, S](source)
			type Res = left.Generalized With (R As A)#T
			val unfiltered = likeJoin[left.Generalized, (R As A)#T, S](left.generalized, source)(True)
			val replacement = aliased \ (unfiltered.last.mapping.body :R[FromClause With (R As A)#T])
			val substitute = new ReplaceRelation[R, S, (R As A)#T, S, Generalized, Res](
				generalized, unfiltered)(last, replacement
			)
			withRight[(R As A)#T, S](aliased)(substitute(condition))
		}


	}

}






/** Base trait for join implementations representing real SQL joins between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression.
  */
sealed trait ProperJoin[+L <: FromClause, R[O] <: MappingAt[O]] extends Join[L, R] { thisClause =>

	override def subselectSize :Int = left.subselectSize + 1



	override type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] <: (F ProperJoin T) {
		type LikeJoin[+S <: FromClause, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	override type This >: this.type <: L ProperJoin R



	override type Implicit = left.Implicit

	override type Outer = left.Outer

	override def outer :Outer = left.outer

	override type Explicit = left.Explicit With R

	override type Inner = left.Inner LikeJoin R



	override type SubselectRow = left.SubselectRow ~ last.Subject //R[last.Origin]#Subject

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		left.subselectRow(target)(stretch.stretchFront[left.Generalized, R]) ~ last.stretch(target)(stretch)

	override type AsSubselectOf[F <: FromClause] =
		left.ExtendAsSubselectOf[F, LikeJoin, R] { type Explicit = thisClause.Explicit }


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ProperJoin.*]

}






object ProperJoin {


	/** Create a cross join between the left side, given as a (possibly empty) clause/list of relations,
	  * and the the mapping on the right side representing a table, some other relation or a surrogate mapping.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L ProperJoin R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)]  =
		from match {
			case join :ProperJoin.* => Some((join.left :FromClause, join.right))
			case _ => None
		}



	/** An existential upper bound of all `ProperJoin` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = ProperJoin[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	trait TypedProperJoin[+L <: FromClause, R[O] <: TypedMapping[S, O], S]
		extends TypedJoin[L, R, S] with ProperJoin[L, R]
	{ thisJoin =>

		override def subselectTableStack[E <: FromClause]
		             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
			last.stretch[Generalized, E](target) #::
				left.subselectTableStack(target)(stretch.stretchFront[left.Generalized, R])

		override def asSubselectOf[F <: FromClause](newOuter :F)(implicit extension :Implicit ExtendedBy F)
				:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
			left.extendAsSubselectOf[F, R, S](newOuter, narrow)


	}

}






/** A standard join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, narrowed down by the conjunction of this join's condition with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromClause, R[O] <: MappingAt[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] = F InnerJoin T
	override type This >: this.type <: L InnerJoin R


	override def likeJoin[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                     (left :F, right :RowSource[T])(filter :SQLBoolean[left.Generalized With T]) :F InnerJoin T =
		InnerJoin[F, T, X](left, LastRelation(right))(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin.*]

	protected override def joinType :String = "join"

}






object InnerJoin {

	/** A template `InnerJoin` instance with a dummy mapping, for use as a polymorphic factory of `InnerJoin` joins. */
	final val template :InnerJoin.* = InnerJoin(Dual, RowSource.Dummy)

	/** Create a cross join between the left side, given as a (possibly empty) clause/list of relations,
	  * and the the mapping on the right side representing a table, some other relation or a surrogate mapping.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		castR(apply(From(left), LastRelation[RB, B](castR(right)))(True))

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, InnerJoin, R, T, S]) :L InnerJoin R =
		cast(apply[left.type, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized With R]) :L InnerJoin R =
		prefix.selfInnerJoin[R, S](next)(filter)



	private[sql] def newJoin[R[O] <: TypedMapping[S, O], S](prefix :FromClause, next :LastRelation[R, S])
	                                                       (filter :SQLBoolean[prefix.Generalized With R])
			:InnerJoin[prefix.type, R] =
		new CrossJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type InnerJoin R
			override type WithRight[T[O] <: MappingAt[O]] = left.type InnerJoin T

			protected override def narrow :left.type InnerJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :SQLBoolean[left.Generalized With R]) =
				newJoin[R, S](left, last)(condition && filter)


			override def withRight[T[O] <: TypedMapping[X, O], X] //needs to be private because WithRight is invariant in L
			                      (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T]) =
				newJoin[T, X](left, right)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :CrossJoin[_, _, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :AnyCrossJoin => Some(join.left -> join.right)
			case _ => None
		}



	/** An existential upper bound of all `InnerJoin` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = InnerJoin[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	private type AnyCrossJoin = CrossJoin[_ <: FromClause, M, _] forSome { type M[O] <: TypedMapping[_, O] }

	/** An `InnerJoin` not extended by `From`, allowing to differentiate 'true' inner joins from the latter. */
	private trait CrossJoin[+L <: FromClause, R[O] <: TypedMapping[S, O], S]
		extends InnerJoin[L, R] with TypedProperJoin[L, R, S]

}






/** A symmetrical outer join of two SQL relations, that is one where every row in either of them occurs at least
  * once in the result, matched with an artificial row consisting of only null values from the other table if no
  * true matches exist. Represents a cross join of all relations from the left side with the relation
  * on the right, narrowed down by the conjunction of this join's condition with the combined filter expression
  * of the left side.
  */
sealed trait OuterJoin[+L <: FromClause, R[O] <: MappingAt[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] = F OuterJoin T
	override type This >: this.type <: L OuterJoin R

	override def likeJoin[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                     (left :F, right :RowSource[T])(filter :SQLBoolean[left.Generalized With T]) :F OuterJoin T =
		OuterJoin[F, T, X](left, LastRelation(right))(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[OuterJoin.*]

	protected override def joinType :String = "outer join"

}






object OuterJoin {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :OuterJoin.* = OuterJoin(Dual, RowSource.Dummy)

	/** Create an outer join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some last proxy.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], OuterJoin, R, RB, B])
			:From[L] OuterJoin R =
		castR(apply(From(left), LastRelation[RB, B](castR(right)))(True))

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, OuterJoin, R, T, S]) :L OuterJoin R =
		cast(apply[left.type, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized With R]) :L OuterJoin R =
		new OuterJoin[prefix.type, R] with TypedProperJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type This = left.type OuterJoin R
			override type WithRight[T[O] <: MappingAt[O]] = left.type OuterJoin T

			protected override def narrow :left.type OuterJoin R = this

			//needs to be private because the result is This
			override def withCondition(filter :SQLBoolean[left.Generalized With R]) =
				OuterJoin[left.type, R, S](left, last)(condition && filter)


			override def withRight[T[O] <: TypedMapping[X, O], X] //needs to be private because WithRight is invariant in L
			                      (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T]) =
				OuterJoin[left.type, T, X](left, right)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :OuterJoin.* => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :OuterJoin.* => Some(join.left -> join.right)
			case _ => None
		}



	/** An existential upper bound of all `OuterJoin` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = OuterJoin[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

}






sealed trait LeftJoin[+L <: FromClause, R[O] <: MappingAt[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] = F LeftJoin T
	override type This >: this.type <: L LeftJoin R

	override def likeJoin[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                     (left :F, right :RowSource[T])(filter :SQLBoolean[left.Generalized With T]) :F LeftJoin T =
		LeftJoin[F, T, X](left, LastRelation(right))(filter)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin.*]

	protected override def joinType = "left join"

}






object LeftJoin {

	/** A template `LeftJoin` instance with a dummy mapping, for use as a polymorphic factory of `LeftJoin` joins. */
	final val template :LeftJoin.* = LeftJoin(Dual, RowSource.Dummy)

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a last or some last proxy.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], LeftJoin, R, RB, B])
			:From[L] LeftJoin R =
		castR(apply(From(left), LastRelation[RB, B](castR(right)))(True))

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, LeftJoin, R, T, S]) :L LeftJoin R =
		cast(apply[left.type, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[A] <: TypedMapping[S, A], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized With R]) :L LeftJoin R =
		new LeftJoin[prefix.type, R] with TypedProperJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type WithRight[T[A] <: MappingAt[A]] = left.type LeftJoin T
			override type This = left.type LeftJoin R

			override protected def narrow :left.type LeftJoin R = this

			override protected def withCondition(filter :SQLBoolean[left.Generalized With R]) :This =
				LeftJoin[left.type, R, S](left :left.type, last)(condition && filter)

			override def withRight[T[A] <: TypedMapping[X, A], X]
			                      (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T]) :WithRight[T] =
				LeftJoin[left.type, T, X](left, right)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :LeftJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :LeftJoin.* => Some(join.left -> join.right)
			case _ => None
		}



	type * = LeftJoin[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

}






sealed trait RightJoin[+L <: FromClause, R[O] <: MappingAt[O]] extends ProperJoin[L, R] {

	override type LikeJoin[+F <: FromClause, T[O] <: MappingAt[O]] = F RightJoin T
	override type This >: this.type <: L RightJoin R


	override def likeJoin[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                     (left :F, right :RowSource[T])(filter :SQLBoolean[left.Generalized With T]) :F RightJoin T =
		RightJoin[F, T, X](left, LastRelation(right))(filter)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin.*]

	protected override def joinType = "right join"

}






object RightJoin {

	/** A template `RightJoin` instance with a dummy mapping, used as a polymorphic factory of `RightJoin` joins.  */
	final val template : RightJoin.* = RightJoin(Dual, RowSource.Dummy)


	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a last or some last proxy.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	          castR :InferSubject[From[L], RightJoin, R, RB, B])
			:From[L] RightJoin R =
		castR(RightJoin(From(left), LastRelation[RB, B](castR(right)))(True))

	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, RightJoin, R, T, S]) :L RightJoin R =
		cast(RightJoin[left.type, T, S](left, LastRelation(cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized With R]) :L RightJoin R =
		new RightJoin[prefix.type, R] with TypedProperJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type WithRight[T[O] <: MappingAt[O]] = left.type RightJoin T
			override type This = left.type RightJoin R

			override protected def narrow = this

			protected override def withCondition(filter :SQLBoolean[left.Generalized With R]) :This =
				RightJoin[left.type, R, S](left, last)(condition && filter)

			override def withRight[T[O] <: TypedMapping[X, O], X]
			                      (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T]) :WithRight[T] =
				RightJoin[left.type, T, X](left, right)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :RightJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :RightJoin.* => Some(join.left -> join.right)
			case _ => None
		}



	type * = RightJoin[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

}






/** A `FromClause` constituting of exactly one last or SQL relation.
  * This is just a bit of sugar for Join[Dual, T], so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  */ //todo: maybe this should simply be a type alias?
sealed trait From[T[O] <: MappingAt[O]] extends InnerJoin[Dual, T] {
	override val left :Dual = Dual

	override type WithRight[R[O] <: MappingAt[O]] <: From[R]
	override type This >: this.type <: From[T]
	override type Generalized = FromClause With T


	def source :RowSource[T] = last.source


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.*]

	protected override def joinType :String = "from"

	override def toString :String =
		if (condition == True()) "from " + last.source
		else "from " + last.source + " where " + condition

}






/** A `FromClause` factory for both single last queries, and starting points for arbitrary join lists.
  * Example (see the `With` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) whereLast (_.name === "Jack")
  *     val guests = From(Hotels) join GuestsBook on (_.id === _.hotelId) join People on (_.guestId === _.id)
  * }}}
  */
object From {

	/** A template `From` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
	final val template :From.* = From(RowSource.Dummy)

	def apply[R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (source :RowSource[R])(implicit cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
			:From[R] =
		cast(newJoin(LastRelation[T, S](cast(source)))(True))

	private[sql] def apply[T[O] <: TypedMapping[S, O], S]
	                      (source :RowSource[T], filter: SQLBoolean[FromClause With T] = True) :From[T] =
		newJoin(LastRelation[T, S](source))(filter)

	private[sql] def apply[T[O] <: TypedMapping[S, O], S]
	                      (table :LastRelation[T, S], filter :SQLBoolean[FromClause With T]) :From[T] =
		newJoin(table)(filter)



	private[sql] def newJoin[M[O] <: TypedMapping[S, O], S]
	                        (relation :LastRelation[M, S])(filter :SQLBoolean[FromClause With M])
			:From[M] with (Dual.type InnerJoin M) =
		new From[M] with (Dual.type InnerJoin M) with TypedProperJoin[Dual.type, M, S] {
			override val left = Dual
			override val last = relation
			override val condition = filter
			override def size = 1

			override def narrow :left.type InnerJoin M = this
			override type WithRight[R[O] <: MappingAt[O]] = From[R] with (Dual.type InnerJoin R)
			override type This = From[M] with (left.type InnerJoin M)



			protected override def withCondition(filter :SQLBoolean[FromClause With M]) =
				newJoin[M, S](last)(condition && filter)

			override def withRight[T[O] <: TypedMapping[X, O], X]
			                      (right :LastRelation[T, X])(filter :SQLBoolean[FromClause With T]) =
				newJoin[T, X](right)(filter)

		}



	def unapply[M[O] <: MappingAt[O]](from :FromClause With M) :Option[RowSource[M]] = from match {
		case _ :From.* => Some(from.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[RowSource.*] = from match {
		case f :From.* => Some(f.source)
		case _ => None
	}

	type * = From[M] forSome { type M[O] <: MappingAt[O] }

}





