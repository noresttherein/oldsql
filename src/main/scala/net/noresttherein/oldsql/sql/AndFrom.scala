package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Relation.As
import net.noresttherein.oldsql.sql.AndFrom.AndFromLike
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{ClauseComposition, ClauseDecomposition, ExtendedBy, FromClauseLike, NonEmptyFrom, NonEmptyFromLike, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.Compound.{CompoundLike, JoinedRelationSubject}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.SQLExpression.{GlobalScope, LocalScope}






/** A join between an existing [[net.noresttherein.oldsql.sql.FromClause FromClause]], representing a relation
  * or a joined list of relations, and another mapping. It is the root of a hierarchy of classes representing
  * ungrouped ''from'' clauses (i.e., without a ''group by'' clause). Subclasses exist for various SQL join kinds
  * (inner, outer, left, right), clauses for subselects nested under other clauses, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type. Note however that for most functions
  * to work properly, more specific type than `AndFrom` may be needed, typically
  * the [[net.noresttherein.oldsql.sql.FromClause.Generalized Generalized]] form of this clause.
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN <: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN <: AndFrom`.
  *
  * @tparam L the left side of this join: a `FromClause` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause.
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Join]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait AndFrom[+L <: FromClause, R[O] <: MappingAt[O]]
	extends Extended[L, R] with FromSome with AndFromLike[L, R, L AndFrom R]
{ thisClause =>

	override val left :L

	override type FromLast = FromClause AndFrom R

	override type Generalized >: Self <: (left.Generalized AndFrom R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: FromClause] <: thisClause.DefineBase[I]
	}

	override type Self <: (left.Self AndFrom R) {
		type Generalized = thisClause.Generalized
//		type Self <: thisClause.Self
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
//		type Outer = thisClause.Outer
		type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
		type InnerRow = thisClause.InnerRow
		type OuterRow = thisClause.OuterRow
		type JoinedWith[+P <: FromClause, +J[+S <: P, T[O] <: MappingAt[O]] <: S AndFrom T] = thisClause.JoinedWith[P, J]
		type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
	}



	protected override def narrow :left.type AndFrom R



	/** A proof that the generalized form of this type extends its left side.
	  * Used as evidence required by some implicits.
	  */
	def generalizedExtension[P <: FromSome] :P PrefixOf GeneralizedLeft[P]

	/** A proof that this type extends its left side. Used as evidence required by some implicits. */
	def extension[P <: FromSome] :P PrefixOf WithLeft[P]



	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.FromClause.filter]]
	  */
	override def condition :GlobalBoolean[Generalized]

	private[this] val cachedFilter = Lazy { filter(generalized) }

	override def filter :GlobalBoolean[Generalized] = cachedFilter.get



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AndFrom.*]

}






/** A matching pattern and factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables
  * joined together.
  */
object AndFrom {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method.
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
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.AndFrom.on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method.
	  * This method will create a [[net.noresttherein.oldsql.sql.From From]]`[R]` instance if `left` is empty (`Dual`),
	  * or an [[net.noresttherein.oldsql.sql.InnerJoin L InnerJoin R]] otherwise. This method's use is somewhat limited
	  * as the result type of `L AndFrom R` is too abstract (its `Generalized` form is undefined) for many purposes.
	  * Prefer using the factory methods of `FromClause` and, in particular, its extension methods from
	  * [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]], such as `join`, `leftJoin`, etc.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast implicit witness providing type inference of the subject type of the mapping `R` (and `T`).
	  */
	def apply[L <: DiscreteFrom, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :GlobalBoolean[L#Generalized AndFrom R] = True)
	         (implicit cast :InferSubject[L, AndFrom, R, T, S]) :L AndFrom R =
	{
		val condition = cast.cast[WithLeft[L#Generalized]#F, GlobalScope, Boolean](filter)
		cast(left.extend[T, S](LastRelation[T, S](cast(right)), condition))
	}


	//consider: shouldn't it return also condition?
	/** Splits any `AndFrom` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L Compound R) :Option[(L, Relation[R])] = join match {
		case _ :AndFrom[_, _] => Some((join.left, join.right))
		case _ => None
	}

	/** Matches all `AndFrom` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromClause, Relation.*)] = from match {
		case join :AndFrom.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def andFromDecomposition[L <: FromClause, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L AndFrom R, L, R, AndFrom, FromClause] =
		decomposition.asInstanceOf[ExtendedDecomposition[L AndFrom R, L, R, AndFrom, FromClause]]

	private[this] val decomposition =
		new ExtendedDecomposition[FromClause AndFrom MappingAt, FromClause, MappingAt, AndFrom, FromClause]



	trait AndFromLike[+L <: FromClause, R[O] <: MappingAt[O], +S <: (L AndFrom R) with AndFromLike[L, R, S]]
		extends CompoundLike[S]
	{ thisClause :S =>

/*
		override type This >: this.type <: S {
			type LastMapping[O] = thisClause.LastMapping[O]
			type LastTable[C <: FromClause] = thisClause.LastTable[C]
			type FromLast = thisClause.FromLast
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
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+C <: NonEmptyFrom] = thisClause.FromSubselect[C]
		}
*/


		val left :L
		def condition :GlobalBoolean[Generalized]

		/** The `Generalized` type with the left side substituted for `F`. */
		type GeneralizedLeft[+F <: FromSome] <: (F AndFrom R) {
			type GeneralizedLeft[+C <: FromSome] <: thisClause.GeneralizedLeft[C]
		}

		/** This `Self` type with the left side substituted for `F`. */
		type WithLeft[+F <: FromSome] <: GeneralizedLeft[F] {
			type WithLeft[+C <: FromSome] <: thisClause.WithLeft[C] //can't be =:= because From[T].WithLeft[L]= L AndFrom T
		}

		/** A join of the same kind as this clause, but with the left clause substituted for `left`. */
		def withLeft[F <: FromSome](left :F)(filter :GlobalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]


		/** A copy of this clause with the `condition` being replaced with the given `filter`.
		  * This does not replace the whole ''where'' filter, as the conditions (if present) of the left clause remain
		  * unchanged. It is the target of the `where` and other filtering methods (which add to the condition, rather
		  * then completely replacing it).
		  */
		def withCondition(filter :GlobalBoolean[Generalized]) :This

		override def filtered[S >: GlobalScope <: GlobalScope](filter :SQLBoolean[Generalized, S]) :This =
			if (filter == True) this else withCondition(condition && filter)



		/** Apply a join condition to the last two relations in this clause. The condition is combined using `&&` with
		  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
		  * This works exactly like 'where', but instead of a single argument representing all joined relations,
		  * the filter function should take as its arguments the last two relations, i.e, the last relation defined
		  * by the left side of this join, if any, and the right side of this join.
		  * This method is only possible to call if the left side of this join can be statically determined
		  * to be a [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSome FromSome]] subtype, i.e. a non-empty from clause.
		  * Its signature then takes the form of:
		  * {{{
		  *     def on(condition :(JoinedRelation[GeneralizedLeft[left.FromLast], left.LastMapping],
		  *                        JoinedRelation[FromLast, R]) => SQLBoolean[Generalized]) :This
		  * }}}
		  * For example, for the type `From[L] InnerJoin R` it takes the form of
		  * {{{
		  *     def on(condition :(JoinedRelation[FromClause AndFrom L Join R, L],
		  *                        JoinedRelation[FromClause AndFrom R, R])
		  *                       => SQLBoolean[FromClause AndFrom L Join R]) :This
		  * }}}
		  * If the left type is too abstract, the type of `condition` argument will likewise be abstract, preventing
		  * the caller from providing a value; in turn, `Dual` defines it simply as `Nothing`.
		  * @param condition a function accepting the expressions for the last two relations in this clause and creating
		  *                  an SQL expression for the join condition.
		  * @return a `AndFrom` instance of the same kind as this one, with the same left and right sides,
		  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
		  *         returned by the passed filter function.
		  */
		def on(condition :left.JoinFilter[GeneralizedLeft, FromLast, Generalized, LastMapping]) :S =
			left.bridgeFilterNext[this.type, LastMapping](this)(condition)

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

}






///** Base trait for non subselect, non parameter extension clauses with a generic empty clause as the left side.
//  * It provides the types (and their companion methods) needed for all external functions, in particular those
//  * relying on the full definition of some recursive member types declared by `FromClause`, to work as intended,
//  * including generalized form (of `left.Generalized AndFrom T`). In virtually all cases, use
//  * [[net.noresttherein.oldsql.sql.From From]] instead, which is a concrete, full implementation of a ''from'' clause
//  * containing a single relation. This trait exists to future proof the type hierarchy by accepting types
//  * other than `Dual` as its left side, for example potential custom decorators. Note however that many features
//  * of the library assume `Dual` as the terminator clause of any `FromClause` and will not work properly
//  * with a completely different implementation.
//  * @see [[net.noresttherein.oldsql.sql.FromClause.EmptyFrom]]
//  * @see [[net.noresttherein.oldsql.sql.From]]
//  */
//sealed trait EmptyJoin[+L <: EmptyFrom, T[O] <: MappingAt[O]] extends AndFrom[L, T] with NonSubselect[L, T] {
//	thisClause =>
//
//	def table :Relation[T] = last.relation
//
//	override type GeneralizedLeft[+F <: FromClause] = F AndFrom T
//	override type WithLeft[+F <: FromClause] = F AndFrom T
//	override type Generalized = left.Generalized AndFrom T
//
//	override type Self <: Generalized with Inner {
//		type Generalized = thisClause.Generalized
//		type Self = thisClause.Self
//		type Params = thisClause.Params
//		type FullRow = thisClause.FullRow
//		type Explicit = thisClause.Explicit
//		type Inner = thisClause.Inner
//		type Implicit = thisClause.Implicit
//		type Outer = thisClause.Outer
//		type InnerParams = thisClause.InnerParams
//		type InnerRow = thisClause.InnerRow
//		type Terminator = thisClause.Terminator
//	}
//
//	override type This >: this.type <: L EmptyJoin T
//
//	override def withLeft[F <: DiscreteFrom](newLeft :F)(filter :SQLBoolean[newLeft.Generalized AndFrom T]) :F AndFrom T
//
//	override def generalizedExtension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].asIn[AndFrom, T]
//
//	override def extension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].asIn[AndFrom, T]
//
//	override def narrow :left.type EmptyJoin T
//
//	override type Params = @~
//
//	override type AppendedTo[+P <: DiscreteFrom] = left.AppendedTo[P] AndFrom T
//
//	override type JoinedWith[+P <: FromSome, +J[+F <: FromSome, M[O] <: MappingAt[O]] <: F JoinLike M] =
//		left.JoinedWith[P, J] J T
//
//
//
//	override type Explicit = left.Explicit AndFrom T
//	override type Inner = left.Inner AndFrom T
//	override type Implicit = left.Implicit
//	override type Outer = left.Outer
//
//	override def outer :Outer = left.outer
//
//	override type InnerParams = @~
//
//	override type AsSubselectOf[+F <: FromSome] = left.BaseSubselectOf[F, T] {
//		type Explicit = thisClause.Explicit
////		type AsSubselectOf[+O <: FromSome] = thisClause.left.BaseSubselectOf[O, T]
//	}
//
//
//	override def canEqual(that :Any) :Boolean = that.isInstanceOf[EmptyJoin.*]
//}
//
//
//
//
//
//
///** A factory and matching pattern of singleton ''from'' clauses. Prefer using [[net.noresttherein.oldsql.sql.From From]]
//  * whenever possible.
//  */
//object EmptyJoin {
//
//	/** A template `EmptyJoin` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
//	final val template :EmptyJoin.* = EmptyJoin(Dual, Relation.Dummy)
//
//
//
//	/** Create a cross join between the given empty clause as the `left` side, and the the `right` relation representing
//	  * the first joined table, relation or some temporary surrogate mapping.
//	  * The ''where'' clause can be subsequently specified using the
//	  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast]],
//	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] or
//	  * [[net.noresttherein.oldsql.sql.FromClause.where where]] method.
//	  * It is a lower level method returning a non-standard singleton class, created to allow different empty clause
//	  * implementations than `Dual` (though likely based on it) and some functions may not be available for this type.
//	  * Always prefer using the standard [[net.noresttherein.oldsql.sql.From From]] class unless a custom
//	  * empty clause is involved.
//	  * @param left an arbitrary empty ''from'' clause.
//	  * @param right the first relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
//	  *              `Mapping` type.
//	  * @param filter an optional condition filtering the relation to use as the ''where'' clause in the generated SQL.
//	  * @param cast an implicit witness providing proper type inference for the mapping of the first relation
//	  *             and conversions of associated classes between instances parameterized with the more generic `R`
//	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
//	  * @return an `L EmptyJoin R`.
//	  */
//	def apply[L <: EmptyFrom, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
//	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized AndFrom R] = True)
//	         (implicit cast :JoinedRelationSubject[WithLeft[L]#F, R, T, MappingOf[S]#TypedProjection]) :L EmptyJoin R =
//		cast(narrow[L, T, S](left, LastRelation(cast(right)))(cast.cast(filter)))
//
//	private[sql] def apply[L <: EmptyFrom, T[O] <: BaseMapping[S, O], S]
//	                      (left :L, relation :LastRelation[T, S])(filter :SQLBoolean[left.Generalized AndFrom T])
//			:L EmptyJoin T =
//		narrow(left, relation)(filter)
//
//	private[sql] def narrow[L <: EmptyFrom, T[O] <: BaseMapping[S, O], S]
//	                       (dual :L, relation :LastRelation[T, S])(cond :SQLBoolean[dual.Generalized AndFrom T])
//			:dual.type EmptyJoin T =
////		new BaseExtended[dual.type, T, S, FromClause AndFrom T, dual.Generalized AndFrom T](dual, relation, cond)
//		new AbstractExtended[L, T, S]
//			with EmptyJoin[dual.type, T]
//		{
//			override val left :dual.type = dual
//			override val last = relation
//			override val condition = cond
//			override def fullSize = 1
//
//			override def narrow :left.type EmptyJoin T = this
//			override type This = left.type EmptyJoin T
//
//
//			override def withCondition(filter :SQLBoolean[Generalized]) =
//				EmptyJoin.narrow(left, last)(filter)
//
//			override def withLeft[F <: DiscreteFrom](newLeft :F)(filter :SQLBoolean[newLeft.Generalized AndFrom T])
//					:F AndFrom T =
//				newLeft.asIn(last, filter)
//
//
//			override def appendedTo[P <: DiscreteFrom](prefix :P) :AppendedTo[P] =
//				withLeft(left.appendedTo(prefix))(condition :SQLBoolean[Generalized])
//
//			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) = ??? //todo:
////				firstJoin.likeJoin[F, T, S](prefix, right)(condition)
//
//			override def joinedWithSubselect[F <: FromSome](prefix :F) = ??? //todo:
////				Subselect[F, T, S](prefix, last)(condition)
//
//
//			override def innerTableStack[E <: FromClause]
//			             (target :E)(implicit extend :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
//				last.extend[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]
//
//			override def asSubselectOf[F <: FromSome](newOuter :F)(implicit extension :Implicit ExtendedBy F)
//					:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
//			{
//				val res = left.bridgeAsSubselectOf(newOuter, last, condition)
////				implicitly[res.AsSubselectOf[FromSome] =:= AsSubselectOf[FromSome]]
////				implicitly[res.AsSubselectOf[FromSome] =:= left.BaseSubselectOf[FromSome, T]]
//				implicitly[res.Explicit =:= (left.Explicit AndFrom T)]
//				implicitly[res.Implicit =:= newOuter.Generalized]
//				implicitly[res.Outer =:= newOuter.Self]
//				res
//			}
//
//
////			override def as[A <: Label](alias :A) :EmptyJoin[L, (T As A)#T] = {
////				val source = FromClause.AliasedRelation[T, A](last.relation, alias)
////				val aliased = RelationSQL.last[(T As A)#T, (T As A)#T, S](source)
////				type Res = FromClause AndFrom (T As A)#T //todo: condition from a function
////				val unfiltered = EmptyJoin[L, (T As A)#T, S](left, aliased)(True)
////				val replacement = aliased \ (unfiltered.last.mapping.body :T[FromClause AndFrom (T As A)#T])
////				val substitute = new ReplaceRelation[T, S, (T As A)#T, S, Generalized, Res](
////					generalized, unfiltered)(last, replacement
////                )
////				EmptyJoin[L, (T As A)#T, S](left, aliased)(substitute(condition))
////			}
//
//		}
//
//
//
//	/** Matches all `EmptyJoin` instances, extracting their relation and the preceding clause in the process. */
//	def unapply[F <: FromClause, M[O] <: MappingAt[O]](from :F Compound M) :Option[(F, Relation[M])] = from match {
//		case _ :EmptyJoin[_, _] => Some((from.left, from.right))
//		case _ => None
//	}
//
//	/** Matches all `EmptyJoin` instances, extracting their relation and the preceding clause in the process. */
//	def unapply(from :FromClause) :Option[(EmptyFrom, Relation.*)] = from match {
//		case f :EmptyJoin.* => Some((f.left, f.right))
//		case _ => None
//	}
//
//
//
//	implicit def emptyJoinDecomposition[L <: EmptyFrom, R[O] <: MappingAt[O]]
//			:ExtendedDecomposition[L EmptyJoin R, L, R, EmptyJoin, EmptyFrom] =
//		decomposition.asInstanceOf[ExtendedDecomposition[L EmptyJoin R, L, R, EmptyJoin, EmptyFrom]]
//
//	private[this] val decomposition =
//		new ExtendedDecomposition[EmptyFrom EmptyJoin MappingAt, EmptyFrom, MappingAt, EmptyJoin, EmptyFrom]
//
//
//
//	/** Type alias for `EmptyJoin` with the erased type parameter, covering all instances of `EmptyJoin`.
//	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
//	  * be matched directly with the wildcard '_'.
//	  */
//	type * = EmptyJoin[_ <: EmptyFrom, M] forSome { type M[O] <: MappingAt[O] }
//
//	/** A curried type constructor for `EmptyJoin` instances, accepting the left `EmptyFrom` type parameter
//	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
//	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
//	  */
//	type WithLeft[L <: EmptyFrom] = { type F[R[O] <: MappingAt[O]] = L EmptyJoin R }
//
//	/** A curried type constructor for `EmptyJoin` instances, accepting the right mapping type parameter
//	  * and returning a type with a member type `F` accepting the left `EmptyJoin` type.
//	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
//	  */
//	type WithRight[R[O] <: MappingAt[O]] = { type F[+L <: EmptyFrom] = L EmptyJoin R }
//}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is a specialized subclass of `AndFrom[Dual, T]`, so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`. For all intents and purposes it is treated
  * as `Dual InnerJoin T` (which would be illegal due to the restriction of the left side of joins to non empty clauses).
  * All non empty ''from'' clauses start with this instance unless the first 'relation' is an unbound join parameter
  * represented by a `JoinParam`.
  *///consider: making it a subclass of Subselect. The relation of being a subselect of a clause and grafting would be easier
sealed trait From[T[O] <: MappingAt[O]]
	extends AndFrom[Dual, T] with NonSubselect[Dual, T] with AndFromLike[Dual, T, From[T]]
{ thisClause =>
	def table :Relation[T] = last.relation

	override type Generalized = FromClause AndFrom T
	override type Self <: From[T]

	override type GeneralizedLeft[+L <: FromClause] = L AndFrom T
	override type WithLeft[+L <: FromClause] = L AndFrom T

	override def withLeft[F <: DiscreteFrom]
	                     (newLeft :F)(filter :GlobalBoolean[newLeft.Generalized AndFrom T]) :F AndFrom T


	override def filter :GlobalBoolean[FromClause AndFrom T] =
		if (left.filter eq True) condition else left.filter && condition

	override def filter[E <: FromClause](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
		filter.basedOn(target)

	override def fullSize = 1

	override def generalizedExtension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].extend[AndFrom, T]

	override def extension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].extend[AndFrom, T]


	override type Params = @~

//	overriden for clarity
	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = P J T
	override type JoinedWithSubselect[+P <: NonEmptyFrom] = P Subselect T

	override type Explicit = FromClause AndFrom T
	override type Inner = FromClause AndFrom T
	override type Implicit = FromClause
	override type Outer = Dual
	override type Base = FromClause
	override type DefineBase[+I <: FromClause] = I

//	override def outer :Dual = left
	override def base :Dual = left


	override type AsSubselectOf[+F <: NonEmptyFrom] = F Subselect T


	/** Specify an alias for this relation. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the relation as in 'from users as u'.
	  * @return a new `From` instance, with a new relation (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :From[(T As A)#T]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.*]

	override def name :String = "from"

	override def toString :String =
		if (filter == True()) "from " + last.relation
		else "from " + last.relation + " where " + filter

}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `AndFrom` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) whereLast (_.name === "Jack")
  *     val guests = From(Hotels) join GuestsBook on (_.id === _.hotelId) join People on (_.guestId === _.id)
  * }}}
  */
object From {

	/** A template `From` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
	final val template :From.* = From(Relation.Dummy)

	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. It can be later filtered using
	  * the [[net.noresttherein.oldsql.sql.FromClause.where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast]] method, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`,
	  *                 represented here by types `R =:= T`, split in order to separate the inference of the mapping
	  *                 type and its subject type and remove the need to specify the type parameter for the method
	  *                 explicitly.
	  * @param cast an implicit witness providing the type inference of the subject type of the relation's mapping
	  *             as well as casting functions between associated classes parameterized with `T` and `R`.
	  * @return an unfiltered `From[R]`.
	  */
	def apply[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (relation :Relation[R])(implicit cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
			:From[R] =
		cast(From(LastRelation[T, S](cast(relation)), True))


	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. This is a lower level factory method accepting an optional, additional filter
	  * for the ''where'' clause, but providing no type inference support and thus generally requiring the type
	  * parameters to be specified explicitly. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.DiscreteFrom.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`.
	  * @param filter an optional boolean condition for the associated ''where'' clause.
	  * @return a `From[R]`.
	  */
	def apply[T[O] <: BaseMapping[S, O], S]
	         (relation :Relation[T], filter: GlobalBoolean[FromClause AndFrom T] = True) :From[T] =
		From(LastRelation[T, S](relation), filter)


	private[sql] def apply[T[O] <: BaseMapping[S, O], S]
	                      (relation :LastRelation[T, S], filter :GlobalBoolean[FromClause AndFrom T]) :From[T] =
		narrow(Dual, relation, filter)

	private[sql] def narrow[T[O] <: BaseMapping[S, O], S]
	                       (dual :Dual, relation :LastRelation[T, S], cond :GlobalBoolean[FromClause AndFrom T])
			:CustomFrom[dual.type, T] =
		new CustomFrom[dual.type, T]  with AbstractExtended[dual.type, T, S] {
			override val left :dual.type = dual
			override val last = relation
			override val condition = cond
			override val outer = left.outer

			override def narrow :dual.type AndFrom T = this
			override type This = CustomFrom[left.type, T]


			override def withCondition(filter :GlobalBoolean[FromClause AndFrom T]) =
				From.narrow(left, last, filter)

			override def withLeft[F <: DiscreteFrom](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized AndFrom T])
					:F AndFrom T =
				newLeft.extend(last, filter)


			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :firstJoin.LikeJoin[F, T] =
				firstJoin.likeJoin[F, T, S](prefix, right)(filter)

			override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :F Subselect T =
				Subselect[F, T, S](prefix, last)(filter)

			override def appendedTo[P <: DiscreteFrom](prefix :P) :P AndFrom T = prefix.extend(last, filter)


			override def innerTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.extend[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F) =
				Subselect[newOuter.type, T, S](newOuter, last)(filter)


			override def as[A <: Label](alias :A) :From[(T As A)#T] = {
				val source = last.relation as[A] alias
				val aliased = RelationSQL.last[(T As A)#T, (T As A)#T, S](source)
				type Res = FromClause AndFrom (T As A)#T //todo: condition from a function
				val unfiltered = From[(T As A)#T, S](aliased, True)
				val replacement = aliased \ (unfiltered.last.mapping.body :T[FromClause AndFrom (T As A)#T])
				val substitute = SQLScribe.replaceRelation(generalized, unfiltered :Res, last, replacement)
				From[(T As A)#T, S](aliased, substitute(condition))
			}


			override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.from[T, S](this)
		}



	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply[M[O] <: MappingAt[O]](from :FromClause Compound M) :Option[Relation[M]] = from match {
		case _ :From.* => Some(from.right)
		case _ => None
	}

	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply(from :FromClause) :Option[Relation.*] = from match {
		case f :From.* => Some(f.table)
		case _ => None
	}

	
	
	implicit def fromComposition[T[O] <: MappingAt[O]] :FromComposition[T] =
		composition.asInstanceOf[FromComposition[T]]
	
	private[this] val composition = new FromComposition[MappingAt]


	class FromComposition[M[O] <: MappingAt[O]] extends ClauseComposition[From[M], Dual, Dual] {
		override type E[+D <: Dual] = From[M]
		override type S[+D >: Dual <: Dual] = From[M]

		override def prefix[A >: Dual <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def extension[A <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def strip(from :From[M]) :Dual = from.left

		override def apply[D <: Dual](template :From[M], dual :D) :From[M] =
			if (template.left.filter == dual.filter) template //this assumes Dual is sealed and thus it doesn't matter
			else template.withCondition(dual.filter && template.condition) //which instance is used if the filter is right

		override def upcast[A >: Dual <: Dual] :ClauseDecomposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseDecomposition[From[M], A, Dual]]

		override def cast[A <: Dual] :ClauseDecomposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseDecomposition[From[M], A, Dual]]
	}
	
	
	
	/** Type alias for `From` with the erased type parameter, covering all instances of `From`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = From[M] forSome { type M[O] <: MappingAt[O] }



	private[sql] trait CustomFrom[+L <: Dual, T[O] <: MappingAt[O]]
		extends From[T] with AndFrom[L, T] with AndFromLike[L, T, CustomFrom[L, T]]

}


