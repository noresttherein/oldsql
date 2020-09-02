package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FreeFromSome, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain
import net.noresttherein.oldsql.sql.Using.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Using.JoinedRelationSubject.InferSubject






/**
  * @author Marcin Mościcki
  */
trait GroupByAll[+F <: FromSome, M[A] <: MappingAt[A]] extends GroupByClause with Using[F, M] { thisClause =>

	override type FromLast = FromSome GroupByAll M
	override type Generalized = left.Generalized GroupByAll M
	override type Self = left.Self GroupByAll M
	override type This >: this.type <: F GroupByAll M

	protected override def narrow :left.type GroupByAll M

	def withLeft[L <: FromSome](left :L)(condition :SQLBoolean[left.Generalized GroupByAll M]) :L GroupByAll M

	override def fullFilter :SQLBoolean[Generalized] = condition

	override def fullFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		condition.stretch(target)


	override def fullSize :Int = outer.fullSize + 1

	override type Params = left.Params
	override type FullRow = OuterRow ~ last.Subject

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, FullRow] =
		left.outerRow(target)(explicitSpan.extend(extension)) ~ last.stretch(target)


	override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L JoinLike R] =
		left.JoinedWith[P, J] GroupByAll M

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*)
			:left.JoinedWith[P, firstJoin.LikeJoin] GroupByAll M =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)

	override type JoinedWithSubselect[+P <: FromSome] = left.JoinedWithSubselect[P] GroupByAll M

	override def joinedWithSubselect[P <: FromSome](prefix :P) :left.JoinedWithSubselect[P] GroupByAll M =
		withLeft(left.joinedWithSubselect(prefix))(condition)



	override type GeneralizedDiscrete = left.Generalized
	override type Discrete = left.Self

	override type Explicit = left.Explicit GroupByAll M
	override type Inner = left.Inner GroupByAll M
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Base = left.DefineBase[left.Implicit] //a supertype of left.Base (in theory, equal in practice)
	override type DefineBase[+I <: FromClause] = left.DefineBase[I]

	override def base :Base = left.base


	override def filter :SQLBoolean[Generalized] = condition

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		condition.stretch(target)

	override def innerSize = 1

	override type InnerRow = @~ ~ last.Subject

	override def innerRow[E <: FromClause]
	                     (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, InnerRow] =
		EmptyChain ~ last.stretch(target)

	override type OuterRow = left.OuterRow

	override def outerRow[E <: FromClause]
	                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, OuterRow] =
		left.outerRow(target)


	override type AsSubselectOf[+P <: FromSome] = left.AsSubselectOf[P] GroupByAll M

	override def asSubselectOf[P <: FromSome](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = AndFrom.shiftBack[Generalized, newLeft.Generalized GroupByAll M](
			generalized, unfiltered, extension.length, innerSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override type FromSubselect[+E <: FromSome] = Nothing //todo:

	override type FromRelation[T[O] <: MappingAt[O]] = Nothing //todo:

	override def from[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Relation[R])
	                 (implicit infer :Conforms[Relation[R], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:FromRelation[T] =
		???

	override def from[S <: FreeFromSome](subselect :S) :FromSubselect[S] = ???

	override def fromSubselect[S <: FromSome](subselect :S)(implicit extension :subselect.Implicit ExtendedBy Generalized)
			:FromSubselect[S] = ???



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GroupByAll.*]

	override def name = "group by"

}






object GroupByAll {
//	type By[+F <: GroupByClause, T] = ByAll[F, MappingOf[T]#TypedProjection]


	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
	  * @param from a ''from'' clause containing the list of relations preceding `right`.
	  * @param group the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `G`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `F GroupByAll G`.
	  */
	def apply[F <: FromSome, G[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (from :F, group :Relation[G], filter :SQLBoolean[F#Generalized GroupByAll G] = True)
	         (implicit cast :InferSubject[F, GroupByAll, G, T, S]) :F GroupByAll G =
	{
		val last = RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T](cast(group), 0)
		GroupByAll[F, T, S](from, last)(cast.cast(filter))
	}


	private[sql] def apply[F <: FromSome, T[O] <: BaseMapping[S, O], S]
	                      (clause :F, group :RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T])
	                      (filter :SQLBoolean[clause.Generalized GroupByAll T])
			:F GroupByAll T =
		new GroupByAll[clause.type, T] {
			override val left = clause
			override val last = group
			override val condition = fullFilter
			override val from = left.self
			override val outer = left.outer
			override val fullSize = outer.fullSize + 1

			override type This = left.type GroupByAll T

			override def narrow :left.type GroupByAll T = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				GroupByAll[left.type, T, S](left, last)(filter)

			override def withLeft[L <: FromSome](left :L)(condition :SQLBoolean[left.Generalized GroupByAll T]) =
				GroupByAll[L, T, S](left, last)(condition)


			override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) =
				last.stretch(target) #:: outer.fullTableStack(target)(explicitSpan.extend(extension))


			override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
					:LazyList[RelationSQL.AnyIn[E]] =
				last.stretch(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]

		}



	/** Matches all `GroupByAll` instances, splitting them into their left (ungrouped ''from'' clause)
	  * and right (the first column grouping) sides.
	  */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Using R) :Option[(L, Relation[R])] = from match {
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






	trait AndByAll[+F <: GroupByClause, G[A] <: MappingAt[A]] extends GroupByClause with NonSubselect[F, G] { thisClause =>
		override type FromLast = GroupByClause AndByAll G

//		override type Generalized = GeneralizedLeft[left.Generalized]
//		override type Self = WithLeft[left.Self]
		override type Generalized >: Self <: (left.Generalized AndByAll G) {
			type FromLast <: thisClause.FromLast
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
			type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L JoinLike R] =
				thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: FromSome] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+P <: FromSome] = thisClause.FromSubselect[P]
		}

		override type This >: this.type <: F AndByAll G

		protected override def narrow :WithLeft[left.type]



		type GeneralizedLeft[+L <: GroupByClause] <: (L AndByAll G) {
			type GeneralizedLeft[+C <: GroupByClause] <: thisClause.GeneralizedLeft[C]
		}

		type WithLeft[+L <: GroupByClause] <: GeneralizedLeft[L] {
			type GeneralizedLeft[+C <: GroupByClause] = thisClause.GeneralizedLeft[C]
			type WithLeft[+C <: GroupByClause] = thisClause.WithLeft[C]
		}

		def withLeft[L <: GroupByClause](left :L)(filter :SQLBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[L]


		/** A proof that the generalized form of this type extends its left side.
		  * Used as evidence required by some implicits.
		  */
		def generalizedExtension[P <: GroupByClause] :P PrefixOf GeneralizedLeft[P]

		/** A proof that this type extends its left side. Used as evidence required by some implicits. */
		def extension[P <: GroupByClause] :P PrefixOf WithLeft[P]


		override type GeneralizedDiscrete = left.GeneralizedDiscrete
		override type Discrete = left.Discrete
//		override type Explicit = GeneralizedLeft[left.Generalized]
//		override type Inner = WithLeft[left.Inner]


		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[AndByAll[_, T] forSome { type T[O] <: MappingAt[O] }]

	}



	trait ByAll[+F <: GroupByClause, G[A] <: MappingAt[A]] extends AndByAll[F, G] { thisClause =>
//		override type FromLast = GroupByClause ByAll G

		override type Generalized = left.Generalized ByAll G
		override type Self = left.Self ByAll G
		override type This >: this.type <: F ByAll G

		override type GeneralizedLeft[+L <: GroupByClause] = L ByAll G
		override type WithLeft[+L <: GroupByClause] = L ByAll G

		override def generalizedExtension[P <: GroupByClause] :P PrefixOf (P ByAll G) =
			PrefixOf.itself[P].extend[ByAll, G]

		override def extension[P <: GroupByClause] :P PrefixOf (P ByAll G) =
			PrefixOf.itself[P].extend[ByAll, G]


		override type Params = left.Params


		override type JoinedWith[+P <: FromSome, J[+L <: FromSome, R[O] <: MappingAt[O]] <: L JoinLike R] =
			left.JoinedWith[P, J] ByAll G

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :left.JoinedWith[P, firstJoin.LikeJoin] ByAll G =
			withLeft(left.joinedWith(prefix, firstJoin))(condition)

		override type JoinedWithSubselect[+P <: FromSome] = left.JoinedWithSubselect[P] ByAll G

		override def joinedWithSubselect[P <: FromSome](prefix :P) :left.JoinedWithSubselect[P] ByAll G =
			withLeft(left.joinedWithSubselect(prefix))(condition)



		override type Explicit = left.Explicit ByAll G
		override type Inner = left.Inner ByAll G
		override type Base = left.DefineBase[left.Implicit] //a supertype of clause.Base (in theory, equal in practice)
		override type DefineBase[+I <: FromClause] = left.DefineBase[I]
		override def base :Base = left.base


		override type AsSubselectOf[+P <: FromSome] = left.AsSubselectOf[P] ByAll G

		override def asSubselectOf[P <: FromSome](newOuter :P)(implicit extension :Implicit ExtendedBy P)
				:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		{
			val newLeft = left.asSubselectOf(newOuter)
			//todo: refactor joins so they take functions creating conditions and move this to the constructor
			//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
			val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
			val substitute = AndFrom.shiftBack[Generalized, newLeft.Generalized ByAll G](
				generalized, unfiltered, extension.length, innerSize + 1
			)
			withLeft[newLeft.type](newLeft)(substitute(condition))
		}



		//todo:
		override type FromSubselect[+S <: FromSome] = Nothing
		//todo:
		override type FromRelation[T[O] <: MappingAt[O]] = Nothing

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                 (first :Relation[M])
		                 (implicit infer :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:FromRelation[T] =
			???

		override def from[S <: FreeFromSome](subselect :S) :FromSubselect[S] = ???

		override def fromSubselect[S <: FromSome]
		                          (subselect :S)(implicit extension :subselect.Implicit ExtendedBy Generalized)
				:FromSubselect[S] = ???


		override def name = "by"

	}





	object ByAll {

		/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
		  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
		  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom#on on]],
		  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
		  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
		  * it is generally recommended to use
		  * `left` [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension#join join]] `right`
		  * [[net.noresttherein.oldsql.sql.FromClause#where where]] `filter` DSL instead.
		  * @param from a ''from'' clause containing the list of relations preceding `right`.
		  * @param group the last relation of the created ''from'' clause, using the `T[O] &lt;: BaseMapping[S, O]`
		  *              `Mapping` type.
		  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
		  *               clause in the generated SQL.
		  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
		  *             and conversions of associated classes between instances parameterized with the more generic `G`
		  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
		  * @return an `F GroupByAll G`.
		  */
		def apply[F <: GroupByClause, G[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		         (from :F, group :Relation[G], filter :SQLBoolean[F#Generalized ByAll G] = True)
		         (implicit cast :InferSubject[F, ByAll, G, T, S]) :F ByAll G =
			ByAll[F, T, S](from, RelationSQL(cast(group), 0))(cast.cast(filter))



		private[sql] def apply[F <: GroupByClause, T[O] <: BaseMapping[S, O], S]
		                      (clause :F, group :RelationSQL[GroupByClause AndByAll T, T, S, GroupByClause AndByAll T])
		                      (filter :SQLBoolean[clause.Generalized ByAll T])
				:F ByAll T =
			new ByAll[clause.type, T] with AbstractExtended[clause.type, T, S] {
				override val left = clause
				override val last = group
				override val condition = fullFilter
				override val from = left.from
				override val outer = left.outer
				override val fullSize = left.fullSize + 1

				override type This = left.type ByAll T

				override def narrow :left.type ByAll T = this

				override def withCondition(filter :SQLBoolean[Generalized]) =
					ByAll[left.type, T, S](left, last)(filter)

				override def withLeft[L <: GroupByClause](left :L)(condition :SQLBoolean[left.Generalized ByAll T]) :L ByAll T =
					ByAll[L, T, S](left, last)(condition)

			}



		/** Matches all `ByAll` instances, splitting them into their left (preceding column groupings)
		  * and right (the last column grouping) sides.
		  */
		def unapply[L <: FromClause, R[O] <: MappingAt[O]](from :L Using R) :Option[(L, Relation[R])] = from match {
			case _ :ByAll[_, _] => Some((from.left, from.right))
			case _ => None
		}

		/** Matches all `ByAll` subclasses, extracting their `left` and `right` sides in the process. */
		def unapply(from :FromClause) :Option[(GroupByClause, Relation.*)] = from match {
			case group :ByAll.* => Some((group.left, group.right))
			case _ => None
		}



		implicit def andFromDecomposition[L <: GroupByClause, R[O] <: MappingAt[O]]
				:ExtendedComposition[L ByAll R, L, R, ByAll, GroupByClause] =
			decomposition.asInstanceOf[ExtendedComposition[L ByAll R, L, R, ByAll, GroupByClause]]

		private[this] val decomposition =
			new ExtendedComposition[GroupByClause ByAll MappingAt, GroupByClause, MappingAt, ByAll, GroupByClause]






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








	trait Groupable[E[F <: FromClause] <: SQLExpression[F, _]] {
		type G[O] <: MappingAt[O]
		type F
	}

//	implicit class GroupingMethods[F <: FromSome](val thisClause :F) extends AnyVal {
//		def groupBy[E[S <: FromClause] <: SQLExpression[S, _]]
//		           (component :JoinedEntities[thisClause.Generalized] => E[thisClause.Generalized])
//		           (implicit groupType :Groupable[E]) :F GroupByAll groupType.G
//
//
//	}

}

