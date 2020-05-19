package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{TypedMapping, RowSource}
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Join.TypedJoin
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.TypedJoinedRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple



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
trait Subselect[+F <: FromClause, T[O] <: MappingFrom[O]] extends Join[F, T] {

	override def copy[L <: FromClause, R[O] <: TypedMapping[X, O], X]
	                 (left :L, right :RowSource[R])(filter :BooleanFormula[left.Generalized With R]) :L Subselect R =
		Subselect[L, R, X](left, LastRelation[R, X](right))(filter)


	override type LikeJoin[+L <: FromClause, R[O] <: MappingFrom[O]] = L Subselect R

	override type This >: this.type <: F Subselect T

	override type Outer = left.Generalized

	override def outer :Outer = left.generalized

	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.stretch(stretch)

	override def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]] =
		last.extend(stretch) #:: LazyList.empty



	protected override def joinType = "subselect"

}






object Subselect {

	def apply[L <: FromClause, R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :BooleanFormula[L#Generalized With R] = True)
	         (implicit cast :InferSubject[left.type, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[left.type, T, S](left, LastRelation[T, S](cast(right)))(cast(filter)))



	private[sql] def apply[L <: FromClause, R[O] <: TypedMapping[S, O], S]
	                      (prefix :L, next :JoinedRelation[FromClause With R, R])
	                      (filter :BooleanFormula[prefix.Generalized With R]) :L Subselect R =
		new Subselect[prefix.type, R] with TypedJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val condition = filter

			override type WithRight[T[A] <: MappingFrom[A]] = left.type Subselect T
			override type This = left.type Subselect R

			override def self :left.type Subselect R = this

			override def copy(filter :BooleanFormula[left.Generalized With R]) :This =
				Subselect[left.type, R, S](left, last)(filter)


			override def copy[T[O] <: TypedMapping[X, O], X]
			                 (right :LastRelation[T, X])(filter :BooleanFormula[left.Generalized With T])
					:left.type Subselect T =
				Subselect[left.type, T, X](left, right)(filter)

			override def copy[C <: FromClause](left :C)(filter :BooleanFormula[left.Generalized With R]) :WithLeft[C] =
				Subselect[C, R, S](left, last)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :Subselect.* => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :Subselect.* => Some(join.left -> join.right)
			case _ => None
		}



	type * = Subselect[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }


}

