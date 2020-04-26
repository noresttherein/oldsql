package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.morsels.Origin.##
import net.noresttherein.oldsql.schema.Mapping.MappingFrom
import net.noresttherein.oldsql.schema.RowSource
import net.noresttherein.oldsql.schema.RowSource.AnyRowSource
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables}
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation.{AnyRelationIn, LastRelation}
import net.noresttherein.oldsql.sql.SQLFormula.BooleanFormula
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.SQLTuple.ChainTuple



/** A join between an existing `FromClause`, representing a relation or a joined list of relations, and another mapping.
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point. This is the root of the class hierarchy of non-empty
  * ''from'' clauses: this includes both [[net.noresttherein.oldsql.sql.ProperJoin proper]] joins
  * (inner, left outer, right outer), synthetic combined ''from'' clauses of a
  * [[net.noresttherein.oldsql.sql.Subselect subselect]] and its outer select, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.WithParam parameters]].
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: With`.
  *
  * @param left a `FromClause` constituting a pre-existing joined list of relations - may be empty (`Dual`).
  * @param table the right side of the join - representation of a table alias containing the joined mapping.
  * @param joinCondition the join condition joining the right side to the left side. It is not the complete filter
  *                      condition, as it doesn't include any join conditions defined in the left side of this join.
  * @tparam L the left side of this join.
  * @tparam R the right side of this join.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait With[+L <: FromClause, R[O] <: MappingFrom[O]] extends FromClause { join =>
	val left :L
	val table :LastRelation[R]
	protected[this] val joinCondition :BooleanFormula[L With R]

	protected def copy(filter :BooleanFormula[L With R]) :This

	def copy[F <: FromClause](left :F, filter :BooleanFormula[F With R] = True()) :JoinRight[F]


	type JoinRight[+F <: FromClause] <: F With R

	protected def self :JoinRight[left.type]

	override type This >: this.type <: L With R

	override type Generalized = left.Generalized With R

	override def generalized :Generalized = this.asInstanceOf[Generalized]


	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]

	override type FromLast = FromClause With R
	override def lastTable :JoinedRelation[FromClause With R, R] = table

	def right :RowSource[R] = lastTable.source


	def condition :BooleanFormula[This] = joinCondition


	override val size :Int = left.size + 1



	override type Row = left.Row ~ table.Subject

	override def row[E <: FromClause](extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
		left.row(extension.stretchFront[left.Generalized, R]) ~ table.upcast.stretch(extension)

	override def tableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[AnyRelationIn[E]] =
		table.extend(stretch) #:: left.tableStack(stretch.stretchFront[left.Generalized, R])



	/** A function accepting the last relation of this clause, as a formula for a join between this clause
	  * and a following mapping `T`, and the formula for the mapping `T`, being the last relation in the join,
	  * and returning the join condition for the two relations as a `BooleanFormula` for the join clause. */
	override type JoinFilter[T[O] <: MappingFrom[O]] =
		(JoinedRelation[FromClause With R With T, R], JoinedRelation[FromClause With T, T])
			=> BooleanFormula[FromClause With R With T]


	/** Apply a join condition to the last two relations in this clause. This works exactly like 'where', but
	  * instead of a single argument representing all joined relations, the filter function should take as its arguments
	  * the last two relations, i.e, the last relation defined by the left side of this join, if any, and the right side
	  * of this join. Static type checking enforces that this method can't be called on 'joins' where the left side
	  * is empty (single table sources).
	  * @param condition a function accepting the formulas for the last two relations in this clause and creating a
	  *                  an SQL expression for the join condition.
	  * @return a `With` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `BooleanFormula`
	  *         returned by the passed filter function.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#JoinFilter]]
	  */
	def on(condition :left.JoinFilter[R]) :JoinRight[left.This] = {
		val joinFilter = condition(left.lastTable.extend[R], table)
		val grounded = SQLScribe.groundFreeComponents[left.type With R, Boolean](self)(joinFilter)
		copy[left.type](left, joinCondition && grounded)
	}


	def whereLast(condition :JoinedRelation[FromClause With R, R] => BooleanFormula[FromClause With R]) :This =
		copy(joinCondition && SQLScribe.groundFreeComponents(this)(condition(table)))


	def where[F >: L <: FromClause](condition :JoinedTables[F With R] => BooleanFormula[F With R]) :This = {
		val cond = condition(new JoinedTables[F With R](this))
		copy(joinCondition && SQLScribe.groundFreeComponents(this)(cond))
	}


	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case join :With[_, _] =>
			(join eq this) || (join canEqual this) && join.left == left && join.lastTable == lastTable
		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + table.hashCode



	protected def joinType :String

	override def toString :String =
		left.toString + " " + joinType + " " + right + (if (condition == True()) "" else " on " + condition)

}






/** A factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables joined together. */
object With {

	/** Create a cross join between the left side, given as a (possibly empty) source/list of  tables,
	  * and the the mapping on the right side representing a table or some table proxy.
	  */
	def apply[L[O] <: MappingFrom[O], R[O] <: MappingFrom[O]]
	         (left :RowSource[L], right :RowSource[R]) :From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingFrom[O]](left :L, right :RowSource[R]) :L InnerJoin R =
		InnerJoin(left, right)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, AnyRowSource)]  =
		from match {
			case join :AnyWith => Some((join.left :FromClause, join.right))
			case _ => None
		}



	type AnyWith = With[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }


}
