package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{RowSource, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.TypedJoin
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** A special join type serving as a source for subselects (selects occurring inside of another select, either
  * in the ''select'' or ''where'' clause). It has direct access to all tables in the outer source, while providing
  * its own additional tables in its ''from'' clause. It is represented as an explicit synthetic join between
  * the outer source (left side), and the first table of the subselect from list.
  * This allows any expressions grounded in the outer select to be used as expressions grounded in this select,
  * as it doesn't differ from any other FromClause extension by joins. Note that it is possible to recursively nest
  * subselects to an arbitrary depth and it is modelled by a repeated use of this join type. In that case all
  * tables/mappings to the left of the first occurrence of `Subselect` in the type definition of a nested subselect,
  * while tables/mappings in between subsequent `Subselect`s form the ''from'' clauses of subsequent nested
  * selects, with any tables/mappings following its last occurrence in the type are exclusive to the deepest
  * subselect in the chain.
  *
  * @tparam F static type of outer select's source.
  * @tparam T Right side of this join - the first table of the ''from'' clause of the represented subselect.
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectOf SubselectOf]]
  */
trait Subselect[+F <: FromClause, T[O] <: MappingAt[O]] extends Join[F, T] { thisClause =>

	override type LikeJoin[+L <: FromClause, R[O] <: MappingAt[O]] = L Subselect R

	override type This >: this.type <: F Subselect T

	override def likeJoin[L <: FromClause, R[O] <: TypedMapping[X, O], X]
	                     (left :L, right :RowSource[R])(filter :SQLBoolean[left.Generalized With R]) :L Subselect R =
		Subselect[L, R, X](left, LastRelation[R, X](right))(filter)


	override def isSubselect = true

	override def subselectSize = 1

	override type Implicit = left.Generalized

	override type Outer = left.Self

	override def outer :Outer = left.self

	override type Inner = FromClause Subselect T

	override type Explicit = FromClause With T




	override def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:SQLBoolean[E] =
		condition.stretch(target)




	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit stretch :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.stretch(target)(stretch)

	override type AsSubselectOf[O <: FromClause] = O Subselect T

	override def asSubselectOf[O <: FromClause](newOuter :O)(implicit extension :Implicit ExtendedBy O)
			:(O Subselect T) { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		val unfiltered = withLeft[newOuter.type](newOuter)(True)
		val substitute = With.shiftBack(generalized, unfiltered.generalized, extension.length, 1)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}


	protected override def joinType = "subselect"

}






object Subselect {

	/** A template `Subselect` instance with a dummy mapping, for use as a polymorphic factory of `Subselect` joins. */
	final val template :Subselect.* = Subselect(Dual, RowSource.Dummy)


	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[left.type, T, S](left, LastRelation[T, S](cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :LastRelation[R, S])
	                      (filter :SQLBoolean[prefix.Generalized With R]) :L Subselect R =
		newJoin[R, S](prefix, next)(filter)



	private[sql] def newJoin[R[O] <: TypedMapping[S, O], S]
	                        (prefix :FromClause, next :LastRelation[R, S])
	                        (filter :SQLBoolean[prefix.Generalized With R]) :prefix.type Subselect R =
		new Subselect[prefix.type, R] with TypedJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter
			override val size = left.size + 1

			override type WithRight[T[A] <: MappingAt[A]] = left.type Subselect T
			override type This = left.type Subselect R

			override def narrow :left.type Subselect R = this

			override def withCondition(filter :SQLBoolean[left.Generalized With R]) :This =
				Subselect[left.type, R, S](left, last)(condition && filter)

			override def withRight[T[O] <: TypedMapping[X, O], X]
			                      (right :LastRelation[T, X])(filter :SQLBoolean[left.Generalized With T])
					:left.type Subselect T =
				Subselect[left.type, T, X](left, right)(filter)

			override def withLeft[C <: FromClause]
			                     (left :C)(filter :SQLBoolean[left.Generalized With R]) :WithLeft[C] =
				Subselect[C, R, S](left, last)(filter)



			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
				last.stretch(target) #:: LazyList.empty[SQLRelation.AnyIn[E]]

		}



	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L With R) :Option[(L, JoinedRelation[FromClause With R, R])] =
		join match {
			case _ :Subselect.* => Some(join.left -> join.last)
			case _ => None
		}

	def unapply(from :FromClause) :Option[(FromClause, JoinedRelation.*)] =
		from match {
			case join :Subselect.* => Some(join.left -> join.last)
			case _ => None
		}



	type * = Subselect[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }


}

