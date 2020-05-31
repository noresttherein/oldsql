package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingOf}
import net.noresttherein.oldsql.schema.{RowSource, TypedMapping}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, JoinedRelation, SQLRelation}
import net.noresttherein.oldsql.sql.SQLScribe.SubstituteComponents
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



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
trait With[+L <: FromClause, R[O] <: MappingFrom[O]] extends FromClause { thisClause =>

	/** A `FromClause` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	/** The right side of the join - representation of a table alias containing the joined mapping. */
	override val last :JoinedRelation[FromClause With R, R]

	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]

	override type FromLast = FromClause With R

	def right :RowSource[R] = last.source

	/** the join condition joining the right side to the left side. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.filter]]
	  */
	val condition :SQLBoolean[Generalized]



	override type This >: this.type <: L With R

	/** This type with the `FromClause` of the left side substituted for `F`. */
	type WithLeft[+F <: FromClause] <: F With R

	protected def narrow :WithLeft[left.type]



	protected def withCondition(filter :SQLBoolean[Generalized]) :This

	def withLeft[F <: FromClause](left :F)(filter :SQLBoolean[left.Generalized With R]) :WithLeft[F]

//	def withLeft[F <: FromClause](left :F)(filter :WithLeft[left.type] => SQLBoolean[left.Generalized With R]) :WithLeft[F] = ???



	override type Init = left.Init


	override type Self <: WithLeft[left.Self] { type Self = thisClause.Self }

	override def self :Self = this.asInstanceOf[Self]

	override type Generalized = left.Generalized With R



	override def isSubselect = left.isSubselect


	@volatile private var initializedFilter :SQLBoolean[Generalized] = _
	private var cachedFilter  :SQLBoolean[Generalized] = _

	override def filter :SQLBoolean[Generalized] = {
		if (cachedFilter == null) {
			var filt = initializedFilter
			if (filt == null) {
				filt = filter(generalized)
				initializedFilter = filt
			}
			cachedFilter = filt
		}
		cachedFilter
	}

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		left.filter(target)(extension.stretchFront[left.Generalized, R]) && condition.stretch(target)



	override type Row = left.Row ~ last.Subject

	override def row[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
		left.row(target)(extension.stretchFront[left.Generalized, R]) ~ last.stretch(target)



	override type ExtendJoinedWith[+F <: FromClause, +J[+K <: FromClause, M[O] <: MappingFrom[O]] <: K Join M,
	                               +N[+K <: FromClause, M[O] <: MappingFrom[O]] <: K Join M, T[O] <: MappingFrom[O]] =
		N[JoinedWith[F, J], T]

	override def extendJoinedWith[F <: FromClause, T[O] <: TypedMapping[X, O], X]
	                             (prefix :F, firstJoin :Join.*, nextJoin :Join[this.type, T])
			:nextJoin.LikeJoin[JoinedWith[F, firstJoin.LikeJoin], T] =
		nextJoin.withLeft(joinedWith(prefix, firstJoin))(nextJoin.condition :SQLBoolean[Generalized With T])



	override def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
			:SQLBoolean[E] =
		left.subselectFilter(target)(extension.stretchFront[left.Generalized, R]) && condition.stretch(target)



	override type ExtendAsSubselectOf[O <: FromClause, J[+F <: FromClause, T[A] <: MappingFrom[A]] <: F Join T,
	                                  M[A] <: MappingFrom[A]] =
		J[AsSubselectOf[O], M]

	override def extendAsSubselectOf[O <: FromClause, T[A] <: TypedMapping[X, A], X]
	                                (newOuter :O, next :this.type ProperJoin T)(implicit extension :Implicit ExtendedBy O)
			:ExtendAsSubselectOf[O, next.LikeJoin, T] {
				type Implicit = newOuter.Generalized
				type Outer = newOuter.Self
				type Explicit = next.Explicit
			} =
	{
		val subselectR = asSubselectOf(newOuter)
//		implicit val extendNext = extension.asInstanceOf[next.Generalized ExtendedBy (subselectR.Generalized With T)]
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with Join.as: one of the relation becoming unavailable
		val unfiltered = next.withLeft[subselectR.Generalized](subselectR.generalized)(True)
		val substitute = With.shiftBack[next.Generalized, subselectR.Generalized With T](
			next.generalized, unfiltered, extension.length, subselectSize + 1)
		next.withLeft[subselectR.type](subselectR)(substitute(next.condition))
	}



	/** A function accepting the last relation of this clause, as a expression over a join between this clause
	  * and a following mapping `T`, and the expression for the mapping `T`, being the last relation in the join,
	  * and returning the join condition for the two relations as a `SQLBoolean` for the join clause. */
	override type JoinFilter[T[O] <: MappingFrom[O]] =
		(JoinedRelation[FromClause With R With T, R], JoinedRelation[FromClause With T, T])
			=> SQLBoolean[FromClause With R With T]


	/** Apply a join condition to the last two relations in this clause. This works exactly like 'where', but
	  * instead of a single argument representing all joined relations, the filter function should take as its arguments
	  * the last two relations, i.e, the last relation defined by the left side of this join, if any, and the right side
	  * of this join. Static type checking enforces that this method can't be called on 'joins' where the left side
	  * is empty (single table sources).
	  * @param condition a function accepting the formulas for the last two relations in this clause and creating a
	  *                  an SQL expression for the join condition.
	  * @return a `With` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#JoinFilter]]
	  */
	def on(condition :left.JoinFilter[R]) :WithLeft[L] = {
		val joinFilter = condition(left.last.extend[R], last)
		val grounded = SQLScribe.groundFreeComponents(generalized)(joinFilter)
		withLeft[L](left)(this.condition && grounded)
	}


	def whereLast(condition :JoinedRelation[FromClause With R, R] => SQLBoolean[FromClause With R]) :This =
		withCondition(this.condition && SQLScribe.groundFreeComponents(generalized)(condition(last)))


	def where[F >: L <: FromClause](condition :JoinedTables[Generalized] => SQLBoolean[Generalized]) :This = {
		val cond = condition(new JoinedTables[Generalized](generalized))
		withCondition(this.condition && SQLScribe.groundFreeComponents(generalized)(cond))
	}






	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :With[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left

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
	         (left :L, right :RowSource[R], filter :SQLBoolean[L#Generalized With R])
	         (implicit cast :InferSubject[left.type, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)



	def unapply[L <: FromClause, R[O] <: MappingFrom[O]](join :L With R) :Option[(L, JoinedRelation[FromClause With R, R])] =
		Some(join.left -> join.last)

	def unapply(from :FromClause) :Option[(FromClause, SQLRelation.*)] =
		from match {
			case join :With.* => Some((join.left :FromClause, join.last.toSQLRelation.get))
			case _ => None
		}



	/** An SQL expression rewriter shifting back references to all relations before the last `Subselect` join
	  * by `extension` positions. Used when a subselect clause is 'transplanted' onto another clause,
	  * extending the `Implicit` clause of the subselect.
	  * @param old a subselect clause serving as SQL expression base.
	  * @param extending a new subselect clause with some additional relations inserted between `F#Implicit`
	  *                  and the mapping joined in with a `Subselect` join.
	  * @param extension the difference in relations number between `F` and `G`.
	  * @param threshold number of relations in the explicit ''from'' clause of subselects `F` and `G` (`subselectSize`).
	  */
	private[sql] def shiftBack[F <: FromClause, G <: FromClause]
	                          (old :F, extending :G, extension :Int, threshold :Int) :SQLScribe[F, G] =
		new SubstituteComponents[F, G] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = extending

			override def relation[T[A] <: TypedMapping[E, A], E, O >: F <: FromClause](e :SQLRelation[F, T, E, O])
					:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingFrom[A] } =
				(if (e.shift < threshold) e //todo: we must ensure we are reusing the mapping instance
				 else SQLRelation[G, T, E](e.source, e.shift + extension)).asInstanceOf[SQLRelation[G, T, E, G]]


			protected[this] override def extended[S <: FromClause, N <: FromClause]
			                             (subselect :S, replacement :N)
			                             (implicit oldExt :oldClause.Generalized ExtendedBy S,
			                                       newExt :newClause.Generalized ExtendedBy N) =
				shiftBack[S, N](subselect, replacement, extension, threshold + oldExt.length)
		}






	/** An existential upper bound of all `With` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = With[_ <: FromClause, M] forSome { type M[O] <: MappingFrom[O] }



	trait TypedWith[+L <: FromClause, R[O] <: TypedMapping[S, O], S] extends With[L, R] {
		override val last :SQLRelation[FromLast, R, S, FromLast]


		override def tableStack[E <: FromClause](target :E)(implicit stretch :Generalized ExtendedBy E)
				:LazyList[SQLRelation.AnyIn[E]] =
			last.stretch(target) #:: left.tableStack(target)(stretch.stretchFront[left.Generalized, R])

	}

}

