package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.RowSource
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
import net.noresttherein.oldsql.sql.FromClause.ExtendedBy
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
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
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
  */
trait Subselect[+F <: FromClause, T[O] <: MappingFrom[O]] extends Join[F, T] {

	override def copy[L <: FromClause, R[O] <: MappingFrom[O]]
	                 (left :L, right :RowSource[R])(filter :BooleanFormula[left.Generalized With R]) :L Subselect R =
		Subselect(left, LastRelation(right))(filter)

	override def copy[C <: FromClause](left :C)(filter :BooleanFormula[left.Generalized With T]) :JoinRight[C] =
		Subselect(left, table)(filter)


	override type LikeJoin[+L <: FromClause, R[O] <: MappingFrom[O]] = L Subselect R

	override type This >: this.type <: F Subselect T

	override type Outer = left.Generalized

	override def outer :Outer = left.asInstanceOf[Outer]

	override type SubselectRow = @~ ~ table.Subject

	override def subselectRow[E <: FromClause](stretch :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ table.Subject] =
		ChainTuple.EmptyChain ~ table.upcast.stretch(stretch)

	override def subselectTableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		table.extend(stretch) #:: LazyList.empty



	protected override def joinType = "subselect"

}






object Subselect {
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] Subselect R =
		Subselect(From(left), LastRelation(right))(True)

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L Subselect R =
		Subselect(left, LastRelation(right))(True)


	private[sql] def apply[L <: FromClause, R[O] <: MappingFrom[O]]
	                      (prefix :L, next :JoinedRelation[FromClause With R, R])
	                      (filter :BooleanFormula[prefix.Generalized With R]) :L Subselect R =
		new Subselect[prefix.type, R] {
			override val left = prefix
			override val table = next
			override val condition = filter

			override type JoinLeft[T[A] <: MappingFrom[A]] = left.type Subselect T
			override type This = left.type Subselect R

			override def self :left.type Subselect R = this

			override def copy(filter :BooleanFormula[left.Generalized With R]) :This =
				Subselect[left.type, R](left, table)(filter)


			override def copy[T[O] <: MappingFrom[O]](right :LastRelation[T])
			                                         (filter :BooleanFormula[left.Generalized With T])
					:left.type Subselect T =
				Subselect[left.type, T](left, right)(filter)

		}



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] = join match {
		case _ :AnySubselect => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)] =
		from match {
			case join :AnySubselect => Some(join.left -> join.right)
			case _ => None
		}



	type AnySubselect = Subselect[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }


}

