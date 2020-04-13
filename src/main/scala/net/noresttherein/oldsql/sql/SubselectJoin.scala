package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.schema.{Mapping, RowSource}
import net.noresttherein.oldsql.schema.Mapping.AnyComponent
import net.noresttherein.oldsql.sql.FromClause.SubselectFrom
import net.noresttherein.oldsql.sql.MappingFormula.{FromFormula, FromLast}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True


/** A special join type serving as a source for subselects (selects occurring inside of another select, either
  * in the ''select'' or ''where'' clause). It has direct access to all tables in the outer source, while providing
  * its own additional tables in its ''from'' clause. It is represented as an explicit synthetic join between
  * the outer source (left side), and the first table of the subselect from list.
  * This allows any expressions grounded in the outer select to be used as expressions grounded in this select,
  * as it doesn't differ from any other FromClause extension by joins. Note that it is possible to recursively nest
  * subselects to an arbitrary depth and it is modelled by a repeated use of this join type. In that case all
  * tables/mappings to the left of the first occurrence of `SubselectJoin` in the type definition of a nested subselect,
  * while tables/mappings in between subsequent `SubselectJoin`s form the ''from'' clauses of subsequent nested
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
sealed abstract class SubselectJoin[+F <: FromClause, T <: Mapping] protected
                   (l :F, r :FromFormula[FromClause Join T, T], cond :BooleanFormula[F Join T] = True())
	extends Join[F, T](l, r, cond) //with SubselectFrom[F]
{ subsource =>

//	override def copy(filter :BooleanFormula[F Join T]) :F SubselectJoin T = new SubselectJoin(left, table, filter)
//
//	override def copy[L <: FromClause](left :L) :L SubselectJoin T = new SubselectJoin(left, table)

	override type This <: F SubselectJoin T

//	override def self :F SubselectJoin T = this

//	override type Outer = F
	override type Outer = left.Generalized

	override def outer :Outer = left.asInstanceOf[Outer]

/*
	override def filteredBy: BooleanFormula[this.type] = condition

	override def subselectTableStream: Seq[FromFormula[this.type, _ <: Mapping]] = Seq(lastTable)

	override def subselectTables: Seq[FromFormula[this.type, _ <: Mapping]] = Seq(lastTable)


	override def transplant[O <: FromClause](target: O, rewriter: SQLScribe[Outer, O]): SubselectFrom[O] = {
		val transplanted = target from right
		transplanted filterBy SQLScribe.subselect[Outer, this.type, O, SubselectFrom[O], Boolean](joinCondition, this, transplanted, rewriter)
	}

	//	override def copyJoin[L <: FromClause, M <: Mapping](left: L, right: M): L SubselectJoin M =
	//		new SubselectJoin[L, M](left, right)
	//
	//	override protected def copyJoin(replacement: FromFormula[F Join T, T], condition: BooleanFormula[F Join T]=True()): F SubselectJoin T =
	//		new SubselectJoin[F, T](source, replacement, condition)

	override def copyJoin[L <: FromClause, R <: Mapping](left: L, right: R): L Join R =
		new SubselectJoin[L, R](left, right)

	override protected def copyJoin(replacement: FromFormula[F Join T, T], condition: BooleanFormula[F Join T]=True()): F Join T =
		new SubselectJoin[F, T](source, replacement, condition)


	override def on(condition: (F#LastTable[this.type], FromFormula[this.type, T]) => BooleanFormula[this.type]): F SubselectJoin T =
		super.on(condition).asInstanceOf[F SubselectJoin T]
*/


//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SubselectJoin[_, _]]

	protected override def joinType = "subselect"

}






object SubselectJoin {
	def apply[L[O] <: AnyComponent[O], A, R[O] <: AnyComponent[O], B]
	         (left :RowSource[L], right :RowSource[R]) :From[L[A]] SubselectJoin R[B] =
		new SubselectClause[From[L[A]], R[B]](From(left), FromLast(right, 1))

	def apply[L <: FromClause, R[O] <: AnyComponent[O], A](left :L, right :RowSource[R]) :L SubselectJoin R[A] =
		new SubselectClause[L, R[A]](left, FromLast(right, left.size))

	private[sql] def apply[L <: FromClause, R <: Mapping](left :L, right :R) :L SubselectJoin R =
		new SubselectClause[L, R](left, FromLast(right, left.size))

	private[sql] def apply[L <: FromClause, R <: Mapping](left :L, right :FromFormula[FromClause Join R, R]) :L SubselectJoin R =
		new SubselectClause[L, R](left, right)



	def unapply[L <: FromClause, R <: Mapping](join :L Join R) :Option[(L, R)] = join match {
		case _ :SubselectJoin[_, _] => Some(join.left -> join.right)
		case _ => None
	}

	def unapply(from :FromClause) :Option[(FromClause, Mapping)] =
		from match {
			case join :SubselectJoin[_, _] => Some(join.left -> join.right)
			case _ => None
		}



	private class SubselectClause[F <: FromClause, T <: Mapping]
			(l :F, r :FromFormula[FromClause Join T, T], cond :BooleanFormula[F Join T] = True())
		extends SubselectJoin[F, T](l, r, cond)
	{
		override def copy(filter :BooleanFormula[F Join T]) :F SubselectJoin T = new SubselectClause(left, table, filter)

		override def copy[L <: FromClause](left :L) :L SubselectJoin T = new SubselectClause(left, table)

		type This = F SubselectJoin T
	}
}

