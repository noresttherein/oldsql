package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, NonEmptyFrom, PartOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope






/**
  * @author Marcin Mościcki
  */
trait GroupByAll[+F <: FromSome, M[A] <: MappingAt[A]] extends GroupByClause with Compound[F, M] { thisClause =>

	override type FromLast = FromSome GroupByAll M
	override type Generalized = left.Generalized GroupByAll M
	override type Self = left.Self GroupByAll M

	override type This >: this.type <: (F GroupByAll M) {
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
		type Base = thisClause.Base
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
			thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}

	protected override def narrow :left.type GroupByAll M

	def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupByAll M]) :L GroupByAll M


	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.filter]]
	  */ //overriden due to a conflict between Compound and GroupByClause
	override def condition :LocalBoolean[Generalized]

	override def filter :LocalBoolean[Generalized] = condition

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] =
		condition.basedOn(target)

	override def having(filter :LocalBoolean[Generalized]) :This =
		if (filter == True) this else withCondition(condition && filter)



	override def fullSize :Int = outer.fullSize + 1

	override type Params = left.Params
	override type FullRow = OuterRow ~ last.Subject

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.outerRow(target)(explicitSpan.extend(extension)) ~ last.extend(target)


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
		left.JoinedWith[P, J] GroupByAll M

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*)
			:left.JoinedWith[P, firstJoin.LikeJoin] GroupByAll M =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = left.JoinedWithSubselect[P] GroupByAll M

	override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :left.JoinedWithSubselect[P] GroupByAll M =
		withLeft(left.joinedWithSubselect(prefix))(condition)

	override def appendedTo[P <: DiscreteFrom](prefix :P) :left.JoinedWith[P, AndFrom] GroupByAll M =
		withLeft(left.appendedTo(prefix))(condition)


	override type Grouping[+U <: FromSome] = U GroupByAll M
//	override type GeneralizedGrouped = left.Explicit
//	override type Grouped = left.Inner
	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupByAll M
	override type Inner = left.Inner GroupByAll M
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Base = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
	override type DefineBase[+I <: FromClause] = left.DefineBase[I]

	override def base :Base = left.base


	override type InnerRow = @~ ~ last.Subject

	override def innerRow[E <: FromClause]
	                     (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, InnerRow] =
		EmptyChain ~ last.extend(target)

	override type OuterRow = left.OuterRow

	override def outerRow[E <: FromClause]
	                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, GlobalScope, OuterRow] =
		left.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyFrom] = left.AsSubselectOf[P] GroupByAll M

	override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized GroupByAll M](
			generalized, unfiltered, extension.length, innerSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupByAll.*]

	override def name = "group by"

}






object GroupByAll {

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `G`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `F GroupByAll G`.
	  */
	def apply[F <: FromSome, G[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :Relation[G], filter :LocalBoolean[F#Generalized GroupByAll G] = True)
	         (implicit cast :InferSubject[F, GroupByAll, G, T, S]) :F GroupByAll G =
	{
		val last = RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T](cast(group), 0)
		GroupByAll[F, T, S](from, last)(cast.cast(filter))
	}


	private[sql] def apply[F <: FromSome, T[O] <: BaseMapping[S, O], S]
	                      (clause :F, group :RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T])
	                      (cond :LocalBoolean[clause.Generalized GroupByAll T])
			:F GroupByAll T =
		new GroupByAll[clause.type, T] {
			override val left = clause
			override val last = group
			override val condition = cond
			override val from = left.self
			override val outer = left.outer
			override val fullSize = outer.fullSize + 1

			override type This = left.type GroupByAll T

			override def narrow :left.type GroupByAll T = this

			override def withCondition(filter :LocalBoolean[Generalized]) =
				GroupByAll[left.type, T, S](left, last)(filter)

			override def withLeft[L <: FromSome](left :L)(condition :LocalBoolean[left.Generalized GroupByAll T]) =
				GroupByAll[L, T, S](left, last)(condition)


			override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) =
				last.extend(target) #:: outer.fullTableStack(target)(explicitSpan.extend(extension))


			override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
					:LazyList[RelationSQL.AnyIn[E]] =
				last.extend(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.groupBy[F, T, S](this)

		}



	/** Matches all `GroupByAll` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  *///consider: shouldn't it return also condition?
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
		case _ :GroupByAll[_, _] => Some((from.left, from.right))
		case _ => None
	}

	/** Matches all `GroupByAll` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromSome, Relation.*)] = from match {
		case group :GroupByAll.* => Some((group.left, group.right))
		case _ => None
	}






	/** Type alias for `GroupByAll` with erased type parameters, covering all instances of `GroupByAll`/`GroupBy`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = GroupByAll[_ <: FromSome, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `GroupByAll` instances, accepting the left `DiscreteFrom` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L GroupByAll R }

	/** A curried type constructor for `GroupByAll` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `DiscreteFrom` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L GroupByAll R }






	trait AndByAll[+F <: GroupByClause, G[A] <: MappingAt[A]] extends GroupByClause with NonSubselect[F, G] {
		thisClause =>

		override type FromLast = GroupByClause AndByAll G

		override type Generalized >: Self <: (left.Generalized AndByAll G) {
			type Generalized <: thisClause.Generalized
			type Explicit <: thisClause.Explicit
			type Implicit <: thisClause.Implicit
			type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
		}

		override type Self <: (left.Self AndByAll G) {
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
			type JoinedWith[+P <: FromClause, +J[+S <: P, T[O] <: MappingAt[O]] <: S AndFrom T] =
				thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+P <: NonEmptyFrom] = thisClause.FromSubselect[P]
		}

		override type This >: this.type <: (F AndByAll G) {
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
			type JoinedWith[+P <: FromClause, +J[+S <: P, T[O] <: MappingAt[O]] <: S AndFrom T] =
				thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+P <: NonEmptyFrom] = thisClause.FromSubselect[P]
		}

		protected override def narrow :WithLeft[left.type]



		type GeneralizedLeft[+L <: GroupByClause] <: (L AndByAll G) {
			type GeneralizedLeft[+C <: GroupByClause] <: thisClause.GeneralizedLeft[C]
		}

		type WithLeft[+L <: GroupByClause] <: GeneralizedLeft[L] {
			type GeneralizedLeft[+C <: GroupByClause] = thisClause.GeneralizedLeft[C]
			type WithLeft[+C <: GroupByClause] = thisClause.WithLeft[C]
		}

		def withLeft[L <: GroupByClause](left :L)(filter :LocalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]


		/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
		  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
		  * condition, as it doesn't include any join conditions defined on the left side of this join.
		  * @see [[net.noresttherein.oldsql.sql.FromClause.filter]]
		  */ //overriden due to a conflict between Compound and GroupByClause
		override def condition :LocalBoolean[Generalized]

		private[this] val cachedFilter = Lazy { filter(generalized) }

		override def filter :LocalBoolean[Generalized] = cachedFilter.get

		override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :LocalBoolean[E] =
			left.filter(target)(extension.extendFront[left.Generalized, G]) && condition.basedOn(target)

		override def having(filter :LocalBoolean[Generalized]) :This =
			if (filter == True) this else withCondition(condition && filter)


		override type Grouping[+U <: FromSome] = WithLeft[left.Grouping[U]]
//		override type GeneralizedGrouped = left.GeneralizedGrouped
//		override type Grouped = left.Grouped
		override type GeneralizedDiscrete = left.GeneralizedDiscrete
		override type Discrete = left.Discrete


		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[AndByAll[_, T] forSome { type T[O] <: MappingAt[O] }]

	}



	trait ByAll[+F <: GroupByClause, G[A] <: MappingAt[A]] extends AndByAll[F, G] { thisClause =>

		override type Generalized = left.Generalized ByAll G
		override type Self = left.Self ByAll G

		override type This >: this.type <: (F ByAll G) {
			type Generalized = thisClause.Generalized
			type Self = thisClause.Self
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
			type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
				thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
		}


		override type GeneralizedLeft[+L <: GroupByClause] = L ByAll G
		override type WithLeft[+L <: GroupByClause] = L ByAll G


		override type Params = left.Params


		override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
			left.JoinedWith[P, J] ByAll G

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :left.JoinedWith[P, firstJoin.LikeJoin] ByAll G =
			withLeft(left.joinedWith(prefix, firstJoin))(condition)

		override type JoinedWithSubselect[+P <: NonEmptyFrom] = left.JoinedWithSubselect[P] ByAll G

		override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :left.JoinedWithSubselect[P] ByAll G =
			withLeft(left.joinedWithSubselect(prefix))(condition)

		override def appendedTo[P <: DiscreteFrom](prefix :P) :left.JoinedWith[P, AndFrom] ByAll G =
			withLeft(left.appendedTo(prefix))(condition)


		override type Explicit = left.Explicit ByAll G
		override type Inner = left.Inner ByAll G
		override type Base = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
		override type DefineBase[+I <: FromClause] = left.DefineBase[I]
		override def base :Base = left.base


		override type AsSubselectOf[+P <: NonEmptyFrom] = left.AsSubselectOf[P] ByAll G

		override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
				:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		{
			val newLeft = left.asSubselectOf(newOuter)
			//todo: refactor joins so they take functions creating conditions and move this to the constructor
			//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
			val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
			val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized ByAll G](
				generalized, unfiltered, extension.length, innerSize + 1
			)
			withLeft[newLeft.type](newLeft)(substitute(condition))
		}


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ByAll.*]

		override def name = "by"

	}





	object ByAll {

		/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
		  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
		  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]],
		  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
		  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method. It is a lower level method;
		  * it is generally recommended to use
		  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension.join join]] `right`
		  * [[net.noresttherein.oldsql.sql.FromClause.where where]] `filter` DSL instead.
		  * @param from a ''from'' clause containing the list of relations preceding `right`.
		  * @param group the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
		  *              `Mapping` type.
		  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
		  *               clause in the generated SQL.
		  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
		  *             and conversions of associated classes between instances parameterized with the more generic `G`
		  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
		  * @return an `F GroupByAll G`.
		  */
		def apply[F <: GroupByClause, G[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		         (from :F, group :Relation[G], filter :LocalBoolean[F#Generalized ByAll G] = True)
		         (implicit cast :InferSubject[F, ByAll, G, T, S]) :F ByAll G =
			ByAll[F, T, S](from, RelationSQL(cast(group), 0))(cast.cast(filter))



		private[sql] def apply[F <: GroupByClause, T[O] <: BaseMapping[S, O], S]
		                      (clause :F, group :RelationSQL[GroupByClause AndByAll T, T, S, GroupByClause AndByAll T])
		                      (cond :LocalBoolean[clause.Generalized ByAll T])
				:F ByAll T =
			new ByAll[clause.type, T] with AbstractExtended[clause.type, T, S] {
				override val left = clause
				override val last = group
				override val condition = cond
				override val from = left.from
				override val outer = left.outer
				override val fullSize = left.fullSize + 1

				override type This = left.type ByAll T

				override def narrow :left.type ByAll T = this

				override def withCondition(filter :LocalBoolean[Generalized]) =
					ByAll[left.type, T, S](left, last)(filter)

				override def withLeft[L <: GroupByClause]
				                     (left :L)(condition :LocalBoolean[left.Generalized ByAll T]) :L ByAll T =
					ByAll[L, T, S](left, last)(condition)


				override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.by[F, T, S](this)

			}



		/** Matches all `ByAll` instances, splitting them into their left (preceding column groupings)
		  * and right (the last column grouping) sides.
		  */
		def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Relation[R])] = from match {
			case _ :ByAll[_, _] => Some((from.left, from.right))
			case _ => None
		}

		/** Matches all `ByAll` subclasses, extracting their `left` and `right` sides in the process. */
		def unapply(from :FromClause) :Option[(GroupByClause, Relation.*)] = from match {
			case group :ByAll.* => Some((group.left, group.right))
			case _ => None
		}



		implicit def byAllDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
				:ExtendedDecomposition[L ByAll R, L, R, ByAll, GroupByClause] =
			composition.asInstanceOf[ExtendedDecomposition[L ByAll R, L, R, ByAll, GroupByClause]]

		private[this] val composition =
			new ExtendedDecomposition[GroupByClause ByAll MappingAt, GroupByClause, MappingAt, ByAll, GroupByClause]






		/** Type alias for `ByAll` with erased type parameters, covering all instances of `ByAll`/`By`.
		  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
		  * be matched directly with the wildcard '_'.
		  */
		type * = ByAll[_ <: GroupByClause, T] forSome { type T[O] <: MappingAt[O] }

		/** A curried type constructor for `ByAll` instances, accepting the left `GroupByClause` type parameter
		  * and returning a type with a member type `F` accepting the type constructor for the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
		  */
		type WithLeft[L <: GroupByClause] = { type F[R[O] <: MappingAt[O]] = L ByAll R }

		/** A curried type constructor for `ByAll` instances, accepting the right mapping type parameter
		  * and returning a type with a member type `F` accepting the left `GroupByClause` type.
		  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
		  */
		type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupByClause] = L ByAll R }

	}



}

