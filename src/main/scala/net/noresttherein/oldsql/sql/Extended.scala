package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ClauseComposition, ExtendedBy, NonEmptyFrom, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple






/** A [[net.noresttherein.oldsql.sql.FromClause FromClause]] implementation combining another ''from'' clause `L`
  * and an additional relation with mapping `R`. The relationship between the constituent clause and this clause
  * is undefined; in particular, the relations from the clause `L` may not be a part of this clause.
  * It is a root of a class hierarchy of various SQL joins, subselect clauses and other implementations sharing the same
  * basic recursive interface. It is too generic to be of much use in the client code, as most operations
  * are not available at this level of abstraction and, as such, is considered more implementation oriented,
  * providing definitions for some additional abstract methods.
  *
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point.
  *
  * @tparam L the left side of this join: a `FromClause` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause, accepting
  *           the `Origin` type as the unspecified parameter.
  * @see [[net.noresttherein.oldsql.sql.Extended]]
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.Join]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait Using[+L <: FromClause, R[O] <: MappingAt[O]] extends NonEmptyFrom { thisClause =>

	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]
	override type FromLast >: Generalized <: FromClause Using R


	/** A `FromClause` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	/** The last SQL relation in this clause. This is the relation understood in the global sense, as a unique
	  * database object, rather than an entry in the list of relations in the ''from'' clause -
	  * for that have a look at [[net.noresttherein.oldsql.sql.FromClause#last last]].
	  */
	def right :Relation[R] = last.relation

	/** The right side of the join - representation of a table/relation alias containing represented by joined mapping.
	  * It identifies the SQL relation (table, view or ''select'') which is being added to the clause and its index,
	  * to distinguish between possible multiple occurrences of the same relation. It is a `SQLExpression` for the
	  * subject of the relation's mapping, so it can be used directly as part of larger expressions.
	  * Note that, true to its name, it is treated always as the last entry on the list and the expression
	  * `join.left.last` is an instance representing only the last relation in the prefix clause `join.last` of `join`
	  * and not a valid reference to the relation from the point of view of this clause. Its type parameter
	  * makes it incompatible for direct use in SQL expressions based on this clause and casting it will result
	  * in generating invalid SQL. You can however use the method
	  * [[net.noresttherein.oldsql.sql.Using#lastAsIn lastAsIn]] in the following way:
	  * {{{
	  *     def secondLast[T1[O] <: MappingAt[O], T2 <: MappingAt[O]](from :FromClause AndFrom T1 AndFrom T2) =
	  *         from.left.lastAsIn[FromClause AndFrom T1 AndFrom T2]
	  * }}}
	  */
	override val last :JoinedRelation[FromLast, R]


	override def lastAsIn[E <: FromClause](implicit extension :FromLast PrefixOf E) :JoinedRelation[E, R] =
		last.extend[E]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause#filter]]
	  */
	def condition :SQLBoolean[Generalized]



	override type Generalized >: Self <: (left.Generalized Using R) {
		type FromLast <: thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: (left.Self Using R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromSome, +J[+S <: FromSome, T[O] <: MappingAt[O]] <: S Join T] = thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: FromSome] = thisClause.JoinedWithSubselect[P]
		type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
		type FromSubselect[+F <: FromSome] = thisClause.FromSubselect[F]
	}

	override type This >: this.type <: L Using R

	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `FromClause`, as they become proper path types instead of projections.
	  */
	protected def narrow :left.type Using R



	/** A copy of this clause with the `condition` being replaced with the given `filter`.
	  * This does not replace the whole ''where'' filter, as the conditions (if present) of the left clause remain
	  * unchanged. It is the target of the `where` and other filtering methods (which add to the condition, rather
	  * then completely replacing it).
	  */
	protected def withCondition(filter :SQLBoolean[Generalized]) :This

	override def where(filter :SQLBoolean[Generalized]) :This =
		if (filter == True) this else withCondition(condition && filter)

	/** Apply a filter condition to the last relation in this clause. The condition is combined using `&&` with
	  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * It is equivalent to `this.where(entities => condition(entities.last))`.
	  * @param condition a function accepting the expression for the last relation in this clause and creating
	  *                  an additional SQL expression for the join condition.
	  * @return an `Extended` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  */
	def whereLast(condition :JoinedRelation[FromLast, R] => SQLBoolean[FromLast]) :This =
		where(SQLScribe.groundFreeComponents(generalized, condition(last)))


	override def fullSize :Int = left.fullSize + 1

	override def isParameterized :Boolean = left.isParameterized

	override def isSubselect :Boolean = left.isSubselect
	override def isValidSubselect :Boolean = left.isValidSubselect

	override type Base = DefineBase[Implicit]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Using.*]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :Extended[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left && join.condition == condition

		case _ => false
	}

	override def hashCode :Int = (left.hashCode * 31 + last.hashCode) * 31 + condition.hashCode


	/** Name of the join for use by the `toString` method. */
	def name :String

	override def toString :String =
		left.toString + " " + name + " " + right + (if (condition == True) "" else " on " + condition)

}






object Using {

	/** An existential upper bound of all `Using` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Using[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Using` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: MappingAt[O]] = L Using R }

	/** A curried type constructor for `Using` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromClause] = L Using R }

}






/** A [[net.noresttherein.oldsql.sql.FromClause FromClause]] implementation which extends an existing ''from'' clause `L`
  * with a new relation (an SQL table, view, select, or some synthetic placeholder) `R`. All relations
  * from the left side are considered a part of this clause, witnessed by an implicit value of
  * of `L` [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy ExtendedBy]] `R`.
  * Thus, all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s based on the left side (parameterized
  * with `L`) are convertible to expressions based on this clause.
  *
  * It is an implementation oriented trait, grouping most common definitions of methods declared in `FromClause`,
  * useful only when operating on the most abstract level, as most domain specific functions are available only
  * for more specialized types.
  *
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.GroupByAll.ByAll]]
  * @see [[net.noresttherein.oldsql.sql.FromClause.ExtendedBy]]
  * @author Marcin Mościcki
  */
trait Extended[+L <: FromClause, R[O] <: MappingAt[O]] extends Using[L, R] { thisClause =>
	override type FromLast >: Generalized <: FromClause Extended R

	//WithLeft/GeneralizedLeft

	override type Generalized >: Self <: (left.Generalized Extended R) {
		type FromLast <: thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: (left.Self Extended R) {
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromSome, +J[+S <: FromSome, T[O] <: MappingAt[O]] <: S Join T] = thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: FromSome] = thisClause.JoinedWithSubselect[P]
		type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
		type FromSubselect[+F <: FromSome] = thisClause.FromSubselect[F]
	}

	override type This >: this.type <: L Extended R

	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `FromClause`, as they become proper path types instead of projections.
	  */
	protected def narrow :left.type Extended R


	override def fullFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		left.fullFilter(target)(extension.extendFront[left.Generalized, R]) && condition.stretch(target)

	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, FullRow] =
		left.fullRow(target)(extension.extendFront[left.Generalized, R]) ~ last.stretch(target)



	private[this] val lzyFilter = Lazy { filter(generalized) }

	override def filter :SQLBoolean[Generalized] = lzyFilter.get

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		left.filter(target)(extension.extendFront[left.Generalized, R]) && condition.stretch(target)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Extended.*]

}






object Extended {


	/** An existential upper bound of all `Extended` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = Extended[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Extended` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: MappingAt[O]] = L Extended R }

	/** A curried type constructor for `Extended` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromClause] = L Extended R }



	trait AbstractExtended[+L <: FromClause, R[O] <: BaseMapping[S, O], S] extends Extended[L, R] { thisClause =>

		override val last :RelationSQL[FromLast, R, S, FromLast]

		override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			last.stretch(target) #:: left.fullTableStack(target)(extension.extendFront[left.Generalized, R])


		override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			last.stretch(target) #:: left.innerTableStack(target)(extension.extendFront[left.Generalized, R])
	}






	/** A marker trait for [[net.noresttherein.oldsql.sql.Extended Extended]] implementations other than
	  * the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. While not mandatory, it is recommended
	  * that all such clauses extend this type, as several implicit parameters responsible for traversing
	  * composite ''from'' clauses, in particular acessors for the joined relations, depend on any `Extended` type
	  * to conform to either it, or the `Subselect`, with no provision for other cases. Note that this holds only
	  * for `Extended` subtypes, and there are other clauses, in particular
	  * [[net.noresttherein.oldsql.sql.GroupByAll GroupByAll]], which are neither.
	  */
	trait NonSubselect[+L <: FromClause, R[O] <: MappingAt[O]] extends Extended[L, R] { thisClause =>

		override type Implicit = left.Implicit
		override type Outer = left.Outer

//		override def outer :Outer = left.outer //can't be implemented here because it is an abstract val in GroupedFrom

		override type InnerRow = left.InnerRow ~ last.Subject

		override def innerRow[E <: FromClause]
		                     (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, InnerRow] =
			left.innerRow(target)(extension.extendFront[left.Generalized, R]) ~ last.stretch(target)

		override type OuterRow = left.OuterRow

		override def outerRow[E <: FromClause]
		                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, OuterRow] =
			left.outerRow(target)
	}



	@implicitNotFound("I do not know how to decompose ${F} into an Extended subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExtendedUpcasting[${F}, ${L}, ${R}, ${J}, ${U}].")
	sealed class ExtendedUpcasting[-F <: L J R, +L <: U, R[O] <: MappingAt[O],
	                               +J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause]

	@implicitNotFound("I do not know how to decompose ${F} into an Extended subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit UsingDecomposition[${F}, ${L}, ${R}, ${J}, ${U}].")
	final class ExtendedComposition[F <: L J R, L <: U, R[O] <: MappingAt[O],
	                                  J[+A <: U, B[O] <: R[O]] <: A Extended B, U <: FromClause]
		extends ExtendedUpcasting[F, L, R, J, U] with ClauseComposition[F, L, U]
	{
		override type E[+A <: U] = A J R
		override type S[+A >: L <: U] = A J R

		@inline override def prefix[A >: L <: U] :A PrefixOf (A J R) = PrefixOf.itself[A].extend[J, R]
		@inline override def extension[A <: U] :A PrefixOf (A J R) = PrefixOf.itself[A].extend[J, R]

		@inline override def apply(join :F) :L = join.left

		override def upcast[A >: L <: U] :ExtendedComposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedComposition[A J R, A, R, J, U]]

		override def cast[A <: U] :ExtendedComposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExtendedComposition[A J R, A, R, J, U]]
	}


	implicit def extendedDecomposition[L <: FromClause, R[O] <: MappingAt[O]]
			:ExtendedComposition[L Extended R, L, R, Extended, FromClause] =
		decomposition.asInstanceOf[ExtendedComposition[L Extended R, L, R, Extended, FromClause]]

	private[this] val decomposition =
		new ExtendedComposition[FromClause Extended MappingAt, FromClause, MappingAt, Extended, FromClause]

}
