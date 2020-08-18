package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, JoinedEntities}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.SubstituteComponents
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple






/** A join between an existing `FromClause`, representing a relation or a joined list of relations, and another mapping.
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point. This is the root of the class hierarchy of non-empty
  * ''from'' clauses: this includes both [[net.noresttherein.oldsql.sql.TrueJoin true joins]]
  * (inner, outer, left outer, right outer), synthetic combined ''from'' clauses of a
  * [[net.noresttherein.oldsql.sql.Subselect subselect]] and its outer select, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: AndFrom`.
  *
  * @tparam L the left side of this join: a `FromClause` listing all preceding tables.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait AndFrom[+L <: FromClause, R[O] <: MappingAt[O]] extends FromSome { thisClause =>

	override type Init = left.Init

	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]
	override type FromLast = FromClause AndFrom R

	/** A `FromClause` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	/** The last SQL relation in this clause. */
	def right :Relation[R] = last.relation

	/** The right side of the join - representation of a table alias containing the joined mapping.
	  * It identifies the SQL relation (table, view or ''select'') which is being added to the clause and its index,
	  * to distinguish between possible multiple occurences of the same relation. It is a `SQLExpression` for the
	  * subject of the relation's mapping, so it can be used directly as part of larger expressions.
	  */
	override val last :JoinedRelation[FromClause AndFrom R, R]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.filter]]
	  */
	val condition :SQLBoolean[Generalized]



	/** This type with the `FromClause` of the left side substituted for `F`. */
	type WithLeft[+F <: FromSome] <: GeneralizedLeft[F] {
		type WithLeft[+S <: FromSome] <: thisClause.WithLeft[S]
	}

	/** The `Generalized` type with the `FromClause` of the left side substituted for `F`. */
	type GeneralizedLeft[+F <: FromSome] <: (F AndFrom R) {
		type GeneralizedLeft[+S <: FromSome] <: thisClause.GeneralizedLeft[S]
	}


	/** A join of the same kind as this clause, but with the left clause substituted for `left`. */
	def withLeft[F <: FromSome](left :F)(filter :SQLBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]

	override type Generalized >: Self <: (left.Generalized AndFrom R) { type Generalized <: thisClause.Generalized }

	override type Self <: (left.Self AndFrom R) {
		type Self = thisClause.Self; type Generalized = thisClause.Generalized
	}

	override type This >: this.type <: L AndFrom R

	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `FromClause`, as they become proper path types instead of projections.
	  */
	protected def narrow :left.type AndFrom R



	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''where'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `where` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	protected def withCondition(filter :SQLBoolean[Generalized]) :This

	override def where(filter :SQLBoolean[Generalized]) :This =
		if (filter == True) this else withCondition(condition && filter)

	/** A function accepting the last relation of this clause, as a expression over a join between this clause
	  * and a following mapping `T`, and the expression for the mapping `T`, being the last relation in the join,
	  * and returning the join condition for the two relations as a `SQLBoolean` for the join clause. */
//	override type JoinFilter[T[O] <: MappingAt[O]] =
//		(JoinedRelation[FromClause AndFrom R AndFrom T, R], JoinedRelation[FromClause AndFrom T, T])
//			=> SQLBoolean[FromClause AndFrom R AndFrom T]

	/** Apply a filter condition to the last relation in this clause. The condition is combined using `&&` with
	  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * It is equivalent to `this.where(entities => condition(entities.last))`.
	  * @param condition a function accepting the expression for the last relation in this clause and creating
	  *                  an additional SQL expression for the join condition.
	  * @return a `AndFrom` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  */
	def whereLast(condition :JoinedRelation[FromClause AndFrom R, R] => SQLBoolean[FromClause AndFrom R]) :This =
		where(SQLScribe.groundFreeComponents(generalized, condition(last)))



	private[this] val lzyFilter = Lazy { filter(generalized) }

	override def filter :SQLBoolean[Generalized] = lzyFilter

	override def filter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		left.filter(target)(extension.stretchFront[left.Generalized, R]) && condition.stretch(target)



	override type Row = left.Row ~ last.Subject

	override def row[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
		left.row(target)(extension.stretchFront[left.Generalized, R]) ~ last.stretch(target)



	override def isSubselect = left.isSubselect

	override def subselectFilter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		left.subselectFilter(target)(extension.stretchFront[left.Generalized, R]) && condition.stretch(target)



	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :AndFrom[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left

		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + last.hashCode



	/** Name of the join for use by the `toString` method. */
	def joinName :String

	override def toString :String =
		left.toString + " " + joinName + " " + right + (if (condition == True) "" else " on " + condition)

}






/** A matching pattern and factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables
  * joined together.
  */
object AndFrom {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method.
	  * @param left the first relation of the ''from'' clause.
	  * @param right the second relation of the ''from'' clause.
	  */
	def apply[L[O] <: MappingAt[O], LG[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RG[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LG, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RG, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)


	/** Create a ''from'' clause extending the `left` clause with the relation `right` for mapping `R`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method.
	  * This method will create a [[net.noresttherein.oldsql.sql.From From[R]]] instance if `left` is empty (`Dual`),
	  * or an [[net.noresttherein.oldsql.sql.InnerJoin L InnerJoin R]] otherwise. This method's use is somewhat limited
	  * as the result type of `L AndFrom R` is too abstract (its `Generalized` form is undefined) for many purposes.
	  * Prefer using the factory methods of `FromClause` and, in particular, its extension methods from
	  * [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]], such as `join`, `leftJoin`, etc.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast implicit witness providing type inference of the subject type of the mapping `R` (and `T`).
	  */
	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized AndFrom R] = True)
	         (implicit cast :JoinedRelationSubject[WithLeft[L]#F, R, T, MappingOf[S]#TypedProjection]) :L AndFrom R =
		cast(left.extend[T, S](LastRelation[T, S](cast(right)), cast.downtype[WithLeft[L#Generalized]#F, Boolean](filter)))




	/** Splits any `AndFrom` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L AndFrom R) :Option[(L, Relation[R])] =
		Some(join.left -> join.right)

	/** Matches all `AndFrom` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromClause, Relation.*)] = from match {
		case join :AndFrom.* => Some((join.left :FromClause, join.right))
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

			override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause](e :RelationSQL[F, T, E, O])
					:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingAt[A] } =
				(if (e.shift < threshold) e //todo: we must ensure we are reusing the mapping instance
				 else RelationSQL[G, T, E, G](e.relation, e.shift + extension)).asInstanceOf[RelationSQL[G, T, E, G]]


			protected override def extended[S <: FromClause, N <: FromClause]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
			                                         newExt :newClause.Generalized ExtendedBy N) =
				shiftBack[S, N](subselect, replacement, extension, threshold + oldExt.length)
		}






	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = AndFrom[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `AndFrom` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: MappingAt[O]] = L AndFrom R }

	/** A curried type constructor for `AndFrom` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromClause] = L AndFrom R }



	trait BaseAndFrom[+L <: FromClause, R[O] <: BaseMapping[S, O], S] extends AndFrom[L, R] {
		override val last :RelationSQL[FromLast, R, S, FromLast]

		override def tableStack[E <: FromSome](target :E)(implicit stretch :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			last.stretch(target) #:: left.tableStack(target)(stretch.stretchFront[left.Generalized, R])
	}

}

