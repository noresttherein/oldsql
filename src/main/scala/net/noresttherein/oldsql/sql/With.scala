package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.{RowSource, TypedMapping}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingFormula.JoinedRelation
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
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: With`.
  *
  * @tparam L the left side of this join: a `FromClause` listing all preceding tables.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait With[+L <: FromClause, R[O] <: MappingFrom[O]] extends FromClause { join =>
	/** A `FromClause` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	/** The right side of the join - representation of a table alias containing the joined mapping. */
	override val last :JoinedRelation[FromClause With R, R]

	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]

	override type FromLast = FromClause With R

	def right :RowSource[R] = last.source

	/** the join condition joining the right side to the left side. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined in the left side of this join. */
	val condition :BooleanFormula[Generalized]


	//consider :maybe we should declare it as simply L With R and override in every subclass?
	protected def copy(filter :BooleanFormula[left.Generalized With R]) :This

	def copy[F <: FromClause](left :F)(filter :BooleanFormula[left.Generalized With R]) :WithLeft[F]

	protected[sql] def copyLike[F <: FromClause]
	                           (template :Join.*)(left :F)(implicit single :(L With R) <:< (Dual With R))
			:template.LikeJoin[F, R]



	override type This >: this.type <: L With R

	/** This type with the `FromClause` of the left side substituted for `F`. */
	type WithLeft[+F <: FromClause] <: F With R

	@deprecated
	protected def self :WithLeft[left.type]

	override type Generalized = left.Generalized With R

	override def generalized :Generalized = this.asInstanceOf[Generalized]


//	def condition :BooleanFormula[Generalized] = joinCondition


	override val size :Int = left.size + 1



	override type Row = left.Row ~ last.Subject

	override def row[E <: FromClause](extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
		left.row(extension.shrink[left.Generalized, R]) ~ last.stretch(extension)

	override def tableStack[E <: FromClause](stretch :Generalized ExtendedBy E) :LazyList[JoinedRelation.AnyIn[E]] =
		last.extend(stretch) #:: left.tableStack(stretch.shrink[left.Generalized, R])



	override def filteredBy[E <: FromClause](extension :Generalized ExtendedBy E) :BooleanFormula[E] =
		left.filteredBy(extension.shrink[left.Generalized, R]) && condition.stretch(extension)

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
	def on(condition :left.JoinFilter[R]) :WithLeft[L] = { //todo: left.This is really left.type, so we don't need This
		val joinFilter = condition(left.last.extend[R], last)
		val grounded = SQLScribe.groundFreeComponents(generalized)(joinFilter)
		copy[L](left)(this.condition && grounded)
	}


	def whereLast(condition :JoinedRelation[FromClause With R, R] => BooleanFormula[FromClause With R]) :This =
		copy(this.condition && SQLScribe.groundFreeComponents(generalized)(condition(last)))


	def where[F >: L <: FromClause](condition :JoinedTables[Generalized] => BooleanFormula[Generalized]) :This = {
		val cond = condition(new JoinedTables[Generalized](generalized))
		copy(this.condition && SQLScribe.groundFreeComponents(generalized)(cond))
	}


//	override protected def joinAll_:(preceding :FromClause) :FromClause = ???
//	override protected def joinSubselect_:(preceding :FromClause) :FromClause = ???



	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :With[_, _] if canEqual(join) && join.canEqual(this) =>
			join.left == left && join.last == last

		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + last.hashCode



	protected def joinType :String

	override def toString :String =
		left.toString + " " + joinType + " " + right + (if (condition == True) "" else " on " + condition)

}






/** A factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables joined together. */
object With {


	/** Create a cross join between the left side, given as a (possibly empty) clause/list of relations,
	  * and the the mapping on the right side representing a table, some other relation or a surrogate mapping.
	  */
	def apply[L[O] <: MappingFrom[O], LG[O] <: TypedMapping[A, O], A,
		      R[O] <: MappingFrom[O], RG[O] <: TypedMapping[B, O], B]
	         (left :RowSource[L], right :RowSource[R])
	         (implicit castL :JoinedRelationSubject[From, L, LG, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RG, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	def apply[L <: FromClause, R[O] <: MappingFrom[O], T[O] <: TypedMapping[S, O], S]
	         (left :L, right :RowSource[R], filter :BooleanFormula[L#Generalized With R])
	         (implicit cast :InferSubject[left.type, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, RowSource[R])] =
		Some(join.left -> join.right)

	def unapply(from :FromClause) :Option[(FromClause, RowSource.*)] =
		from match {
			case join :With.* => Some((join.left :FromClause, join.right))
			case _ => None
		}



	/** An existential upper bound of all `With` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = With[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }



	trait TypedWith[+L <: FromClause, R[O] <: TypedMapping[S, O], S] extends With[L, R] {


		protected[sql] override def copyLike[F <: FromClause]
		                                    (template :Join.*)(left :F)
		                                    (implicit single :(L With R) <:< (Dual With R))
				:template.LikeJoin[F, R] =
			template.copy[F, R, S](left, right)(single(this).condition)
	}

}

