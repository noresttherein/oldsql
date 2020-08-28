package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.{BaseMapping, Mapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.AndFrom.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Extended.{BaseExtended, ExtendedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, JoinedEntities, NonEmptyClause, OuterClause, OuterFromSome, UngroupedFrom}
import net.noresttherein.oldsql.sql.Group.{FlatMapGroup, MapGroup}
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain




/** A projections of rows from a single ''group by'' group to a column or columns evaluating to an `SQLExpression[F, V]`.
  * A group expression can be used as an argument for SQL aggregate functions.
  * It is a monad lifting that expression type, as evaluated for a single row, to multiple rows.
  * @tparam F an ungrouped ''from'' clause, that is the left side (i.e. the first type argument)
  *           of the `GroupBy`/`GroupByAll` clause owning this group.
  * @tparam V the value type of the lifted SQL expression
  * @see [[net.noresttherein.oldsql.sql.GroupByAll]]
  */
trait Group[-F <: FromClause, V] {
	protected val expr :SQLExpression[F, V]

	def map[I, O, G <: Group[_, _]](f :I => O)(implicit doMap :MapGroup[this.type, I, O, G]) :G = doMap(this, f)

	def flatMap[I, G <: Group[_, _]](f :I => G)(implicit doMap :FlatMapGroup[this.type, I, G]) :G = doMap(this, f)



	def canEqual(that :Any) :Boolean = that.isInstanceOf[Group[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case group :Group[_, _] => (group eq this) || group.canEqual(this) && group.expr == expr
		case _ => false
	}

	override def hashCode :Int = expr.hashCode

	override def toString :String = "Group(" + expr + ")"
}



object Group {

	private class BaseGroup[-F <: FromClause, V](protected override val expr :SQLExpression[F, V]) extends Group[F, V]



	//consider: parameterize with M[O] <: MappingAt to avoid long origin types; can't be contravariant in that case
	trait MappingGroup[-F <: FromClause, M <: Mapping] extends Group[F, M#Subject] {
		protected override val expr :MappingSQL[F, M] //fixme: this must be a ComponetSQL, ugh

		def mapping :M = expr.mapping

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[MappingGroup[_, _]]

		override def toString :String = "MappingGroup(" + expr.mapping + ")"
	}

	private class BaseMappingGroup[-F <: FromClause, M <: Mapping](protected override val expr :MappingSQL[F, M])
		extends MappingGroup[F, M]



	object MappingGroup {

		implicit def mapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:MapGroup[MappingGroup[F, X[O]], X[O], Y[O], MappingGroup[F, Y[O]]] =
			(group :MappingGroup[F, X[O]], f :X[O] => Y[O]) => ??? //new BaseMappingGroup(f(group.mapping))

		implicit def mapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:MapGroup[MappingGroup[F, X], X, SQLExpression[F, Y], Group[F, Y]] =
			(group :MappingGroup[F, X], f :X => SQLExpression[F, Y]) => ???


		implicit def flatMapMappingToMapping[F <: FromClause, X[A] <: MappingAt[A], Y[A] <: MappingAt[A], O]
				:FlatMapGroup[MappingGroup[F, X[O]], X[O], MappingGroup[F, Y[O]]] =
			(group :MappingGroup[F, X[O]], f :X[O] => MappingGroup[F, Y[O]]) => f(group.mapping)

		implicit def flatMapMappingToExpression[F <: FromClause, X <: Mapping, Y]
				:FlatMapGroup[MappingGroup[F, X], X, Group[F, Y]] =
			(group :MappingGroup[F, X], f :X => Group[F, Y]) => f(group.mapping)
	}


//	trait GroupColumn[-F <: FromClause, V] extends Group[F, V] {
//	}


	@implicitNotFound("Cannot map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be an SQLExpression of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingGroup[F, M], the argument may be the mapping M " +
	                  "and the result type may be any of its components.\n" +
		              "Missing implicit MapGroup[${G}, ${I}, ${O}, ${G}].")
	abstract class MapGroup[-G <: Group[_, _], I, O, +R <: Group[_, _]] {
		def apply(group :G, f :I => O) :R
	}

	implicit def defaultMapGroup[F <: FromClause, X, Y]
			:MapGroup[Group[F, X], SQLExpression[F, X], SQLExpression[F, Y], Group[F, Y]] =
		(group :Group[F, X], f :SQLExpression[F, X] => SQLExpression[F, Y]) => new BaseGroup(f(group.expr))



	@implicitNotFound("Cannot flat map GroupBy group ${G}\nwith function ${I} => ${O}.\n" +
	                  "The argument must be an SQLExpression with the same type parameters as the mapped group and " +
	                  "the result type must be a Group of any type based on the same from clause. " +
	                  "Alternatively, if the mapped group is a MappingGroup[F, M], the argument may be the mapping M " +
	                  "and the result type may be a MappingGroup for any of its components.\n" +
	                  "Missing implicit FlatMapGroup[${G}, ${I}, ${O}].")
	abstract class FlatMapGroup[-G <: Group[_, _], I, O <: Group[_, _]] {
		def apply(group :G, f :I => O) :O
	}

	implicit def defaultFlatMapGroup[F <: FromClause, X, Y]
			:FlatMapGroup[Group[F, X], SQLExpression[F, X], Group[F, Y]] =
		(group :Group[F, X], f :SQLExpression[F, X] => Group[F, Y]) => f(group.expr)

}






trait GroupedFrom extends NonEmptyClause { //GroupByClause?
	override type FromLast <: GroupedFrom
	override type FromNext[E[+L <: FromSome] <: FromClause] = Nothing
	override type This <: GroupedFrom


	override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	             (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
		throw new UnsupportedOperationException(s"GroupedFrom.filterNext (on $this)")


	//fixme: JoinParam throwing an exception here
	override val outer :Outer

	type Ungrouped <: FromClause
	val from :Ungrouped

	def having(condition :JoinedEntities[Generalized] => SQLBoolean[Generalized]) :This = where(condition)

}






/**
  * @author Marcin Mościcki
  */
trait GroupByAll[+F <: FromSome, M[A] <: MappingAt[A]] extends GroupedFrom with Using[F, M] { thisClause =>

	override type FromLast = FromSome GroupByAll M
	override type Generalized = left.Generalized GroupByAll M
	override type Self = left.Self GroupByAll M
	override type This >: this.type <: F GroupByAll M

	protected override def narrow :left.type GroupByAll M

	def withLeft[L <: FromSome](left :L)(condition :SQLBoolean[left.Generalized GroupByAll M]) :L GroupByAll M

	override def filter :SQLBoolean[Generalized] = condition

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		condition.stretch(target)


	override def size :Int = outer.size + 1

	override type Row = outer.Row ~ last.Subject

	override def row[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
		outer.row(target)(explicitSpan.extend(extension)) ~ last.stretch(target)


	override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
		left.JoinedWith[P, J] GroupByAll M

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :TrueJoin.*)
			:left.JoinedWith[P, firstJoin.LikeJoin] GroupByAll M =
		withLeft(left.joinedWith(prefix, firstJoin))(condition)
//		throw new UnsupportedOperationException(s"GroupBy.joinedWith: $this joinedWith[${firstJoin.name} $prefix.")

	override def joinedAsSubselect[P <: FromSome](prefix :P) :left.JoinedWith[P, Subselect] GroupByAll M =
		withLeft(left.joinedAsSubselect(prefix))(condition)



	override type Explicit = left.Explicit GroupByAll M
	override type Inner = left.Inner GroupByAll M
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Ungrouped = left.Self

	override def subselectFilter :SQLBoolean[Generalized] = condition

	override def subselectFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
		condition.stretch(target)

	override def subselectSize = 1

	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, SubselectRow] =
		EmptyChain ~ last.stretch(target)


	override type AsSubselectOf[+P <: FromSome] = (left.AsSubselectOf[P] GroupByAll M) {
//		type Explicit = thisClause.Explicit
//		type Implicit <: P#Generalized
//		type Outer <: P#Outer
//		type AsSubselectOf[+E <: FromSome] = thisClause.AsSubselectOf[E]
	}

	override def asSubselectOf[P <: FromSome](newOuter :P)(implicit extension :Implicit ExtendedBy P)
			:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with Join.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = AndFrom.shiftBack[Generalized, newLeft.Generalized GroupByAll M](
			generalized, unfiltered, extension.length, subselectSize + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}


	override type FromSubselect[+E <: FromClause] = Nothing //todo:

	override type SubselectTable[T[O] <: MappingAt[O]] = Nothing //todo:

	override def from[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Relation[R])
	                 (implicit infer :Conforms[Relation[R], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:SubselectTable[T] =
		???

	override def from[S <: OuterFromSome](subselect :S) :FromSubselect[S] = ???

	override def fromSubselect[S <: FromSome](subselect :S)(implicit extension :subselect.Implicit ExtendedBy Generalized)
			:FromSubselect[S] = ???



	type Params = left.Params


	override def name = "group by"

}






object GroupByAll {
//	type By[+F <: GroupedFrom, T] = ByAll[F, MappingOf[T]#TypedProjection]


	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`
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
	         (implicit cast :JoinedRelationSubject[WithLeft[F]#F, G, T, MappingOf[S]#TypedProjection]) :F GroupByAll G =
	{
		val last = RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T](cast(group), 0)
		GroupByAll[F, T, S](from, last)(cast.downtype(filter))
	}


	private[sql] def apply[F <: FromSome, T[O] <: BaseMapping[S, O], S]
	                      (clause :F, group :RelationSQL[FromSome GroupByAll T, T, S, FromSome GroupByAll T])
	                      (filter :SQLBoolean[clause.Generalized GroupByAll T])
			:F GroupByAll T =
		new GroupByAll[clause.type, T] {
			override val left = clause
			override val last = group
			override val condition = filter
			override val from = left.self
			override val outer = left.outer
			override val size = outer.size + 1

			override type This = left.type GroupByAll T

			override def narrow :left.type GroupByAll T = this

			override def withCondition(filter :SQLBoolean[Generalized]) =
				GroupByAll[left.type, T, S](left, last)(filter)

			override def withLeft[L <: FromSome](left :L)(condition :SQLBoolean[left.Generalized GroupByAll T]) =
				GroupByAll[L, T, S](left, last)(condition)


			override def tableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) =
				last.stretch(target) #:: outer.tableStack(target)(explicitSpan.extend(extension))


			override def subselectTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
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

	/** A curried type constructor for `GroupByAll` instances, accepting the left `UngroupedFrom` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L GroupByAll R }

	/** A curried type constructor for `GroupByAll` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `UngroupedFrom` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L GroupByAll R }







	trait ByAll[+F <: GroupedFrom, G[A] <: MappingAt[A]] extends GroupedFrom with NonSubselect[F, G] { thisClause =>
		override type Ungrouped = left.Ungrouped
		override type FromLast = GroupedFrom ByAll G
		override type Generalized = left.Generalized ByAll G
		override type Self = left.Self ByAll G
		override type This >: this.type <: F ByAll G

		protected override def narrow :left.type ByAll G

		def withLeft[L <: GroupedFrom](left :L)(condition :SQLBoolean[left.Generalized ByAll G]) :L ByAll G



		override type JoinedWith[+P <: FromSome, J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
			left.JoinedWith[P, J] ByAll G

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :TrueJoin.*) :left.JoinedWith[P, firstJoin.LikeJoin] ByAll G =
			withLeft(left.joinedWith(prefix, firstJoin))(condition)

		override def joinedAsSubselect[P <: FromSome](prefix :P) :left.JoinedWith[P, Subselect] ByAll G =
			withLeft(left.joinedAsSubselect(prefix))(condition)


		override type Explicit = left.Explicit ByAll G
		override type Inner = left.Inner ByAll G
		override type Implicit = left.Implicit
		override type Outer = left.Outer


		override type AsSubselectOf[+P <: FromSome] = (left.AsSubselectOf[P] ByAll G) {
//			type Explicit = thisClause.Explicit
//			type Implicit <: P#Generalized
//			type Outer <: P#Self
		}

		override def asSubselectOf[P <: FromSome](newOuter :P)(implicit extension :Implicit ExtendedBy P)
				:AsSubselectOf[P] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		{
			val newLeft = left.asSubselectOf(newOuter)
			//todo: refactor joins so they take functions creating conditions and move this to the constructor
			//  this would however introduce problem with Join.as: one of the relation becoming unavailable
			val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
			val substitute = AndFrom.shiftBack[Generalized, newLeft.Generalized ByAll G](
				generalized, unfiltered, extension.length, subselectSize + 1
				)
			withLeft[newLeft.type](newLeft)(substitute(condition))
		}



		//todo:
		override type FromSubselect[+S <: FromClause] = Nothing
		//todo:
		override type SubselectTable[T[O] <: MappingAt[O]] = Nothing

		override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		                 (first :Relation[M])
		                 (implicit infer :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
				:SubselectTable[T] =
			???

		override def from[S <: OuterFromSome](subselect :S) :FromSubselect[S] = ???

		override def fromSubselect[S <: FromSome]
		                          (subselect :S)(implicit extension :subselect.Implicit ExtendedBy Generalized)
				:FromSubselect[S] = ???



		type Params = left.Params


		override def name = "by"

	}





	object ByAll {

		/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
		  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
		  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]],
		  * [[net.noresttherein.oldsql.sql.FromClause#where where]] or
		  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method. It is a lower level method;
		  * it is generally recommended to use
		  * `left` [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension#join join]] `right`
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
		def apply[F <: GroupedFrom, G[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
		         (from :F, group :Relation[G], filter :SQLBoolean[F#Generalized ByAll G] = True)
		         (implicit cast :JoinedRelationSubject[WithLeft[F]#F, G, T, MappingOf[S]#TypedProjection]) :F ByAll G =
		{
			val last = RelationSQL[GroupedFrom ByAll T, T, S, GroupedFrom ByAll T](cast(group), 0)
			ByAll[F, T, S](from, last)(cast.downtype(filter))
		}


		private[sql] def apply[F <: GroupedFrom, T[O] <: BaseMapping[S, O], S]
		                      (clause :F, group :RelationSQL[GroupedFrom ByAll T, T, S, GroupedFrom ByAll T])
		                      (filter :SQLBoolean[clause.Generalized ByAll T])
				:F ByAll T =
			new ByAll[clause.type, T] with BaseExtended[clause.type, T, S] {
				override val left = clause
				override val last = group
				override val condition = filter
				override val from = left.from
				override val outer = left.outer
				override val size = left.size + 1

				override type This = left.type ByAll T

				override def narrow :left.type ByAll T = this

				override def withCondition(filter :SQLBoolean[Generalized]) =
					ByAll[left.type, T, S](left, last)(filter)

				override def withLeft[L <: GroupedFrom](left :L)(condition :SQLBoolean[left.Generalized ByAll T]) =
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
		def unapply(from :FromClause) :Option[(GroupedFrom, Relation.*)] = from match {
			case group :ByAll.* => Some((group.left, group.right))
			case _ => None
		}



		implicit def andFromDecomposition[L <: GroupedFrom, R[O] <: MappingAt[O]]
				:ExtendedComposition[L ByAll R, L, R, ByAll, GroupedFrom] =
			decomposition.asInstanceOf[ExtendedComposition[L ByAll R, L, R, ByAll, GroupedFrom]]

		private[this] val decomposition =
			new ExtendedComposition[GroupedFrom ByAll MappingAt, GroupedFrom, MappingAt, ByAll, GroupedFrom]






		/** Type alias for `ByAll` with erased type parameters, covering all instances of `ByAll`/`By`.
		  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
		  * be matched directly with the wildcard '_'.
		  */
		type * = ByAll[_ <: GroupedFrom, T] forSome { type T[O] <: MappingAt[O] }

		/** A curried type constructor for `ByAll` instances, accepting the left `GroupedFrom` type parameter
		  * and returning a type with a member type `F` accepting the type constructor for the right relation.
		  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
		  */
		type WithLeft[L <: GroupedFrom] = { type F[R[O] <: MappingAt[O]] = L ByAll R }

		/** A curried type constructor for `ByAll` instances, accepting the right mapping type parameter
		  * and returning a type with a member type `F` accepting the left `GroupedFrom` type.
		  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
		  */
		type WithRight[R[O] <: MappingAt[O]] = { type F[L <: GroupedFrom] = L ByAll R }

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

