package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.RowSource
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
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
  * @param outer the tables of the outer select's ''from'' clause (possibly recursively inlined nesting).
  * @param from first table in the from list of this subselect
  * @param cond join condition joining this subselect (or rather, its first table) with outer select; may be expanded upon
  *             when additional tables are added to this subselect join (i.e. this source is expanded).
  * @tparam F static type of outer select's source.
  * @tparam T Right side of this join - the first table of the ''from'' clause of the represented subselect.
  * @see [[net.noresttherein.oldsql.sql.FromClause.SubselectFrom SubselectFrom]]
  */
sealed abstract class Subselect[+F <: FromClause, T[O] <: MappingFrom[O]] protected
                               (l :F, r :JoinedRelation[FromClause With T, T, _], cond :BooleanFormula[F With T] = True())
	extends With[F, T](l, r, cond) with Join[F, T] //with SubselectFrom[F]
{ subsource =>

	override def copy[L <: FromClause, R[O] <: MappingFrom[O]]
	                 (left :L, right :RowSource[R], filter :BooleanFormula[L With R]) :L Subselect R =
		Subselect(left, right)

	override type LikeJoin[+L <: FromClause, R[O] <: MappingFrom[O]] = L Subselect R

	override type This >: this.type <: F Subselect T

	override type Outer = left.Generalized

	override def outer :Outer = left.asInstanceOf[Outer]

	override type SubselectRow = @~ ~ table.Subject

	override def subselectRow :ChainTuple[this.type, SubselectRow] =
		ChainTuple.EmptyChain ~ table.upcast

	override def subselectTableStack :LazyList[AnyRelationIn[this.type]] =
		table #:: LazyList.empty



	protected override def joinType = "subselect"

}






object Subselect {
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] Subselect R =
		new SubselectClause(From(left), LastRelation(right, 1))

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L Subselect R =
		new SubselectClause(left, LastRelation(right, left.size))

//	private[sql] def apply[L <: FromClause, R <: Mapping](left :L, right :R) :L Subselect R =
//		new SubselectClause[L, R](left, LastRelation(right, left.size))

//	private[sql] def apply[L <: FromClause, R <: Mapping](left :L, right :JoinedRelation[FromClause With R, R]) :L Subselect R =
//		new SubselectClause[L, R](left, right)



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



	private class SubselectClause[F <: FromClause, T[O] <: MappingFrom[O]]
			(l :F, r :JoinedRelation[FromClause With T, T, _], cond :BooleanFormula[F With T] = True())
		extends Subselect[F, T](l, r, cond)
	{
		override def copy(filter :BooleanFormula[F With T]) :F Subselect T = new SubselectClause(left, table, filter)

		override def copy[L <: FromClause](left :L, filter :BooleanFormula[L With T]) :L Subselect T =
			new SubselectClause(left, table, filter)

		override def copy[R[O] <: MappingFrom[O]]
		                 (right :LastRelation[R, _], filter :BooleanFormula[F With R]) :F Subselect R =
			new SubselectClause(left, right, filter)

		override type JoinLeft[R[O] <: MappingFrom[O]] = F Subselect R
		override type This = F Subselect T
	}

}

