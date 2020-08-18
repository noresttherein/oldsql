package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.{BaseMapping, ColumnMapping, Mapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, JoinedEntities}
import net.noresttherein.oldsql.sql.Group.{FlatMapGroup, MapGroup}
import net.noresttherein.oldsql.sql.Join.*
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.AnyIn
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple




/** A projections of rows from a single ''group by'' group to a column or columns evaluating to an `SQLExpression[F, V]`.
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








//todo: move to FromClause
trait Extended[+L <: FromClause, R[O] <: MappingAt[O]] extends FromSome { thisClause =>

	override type Init = left.Init
	override type LastMapping[O] = R[O]
	override type LastTable[F <: FromClause] = JoinedRelation[F, R]
	override type FromLast = FromClause Extended R

	val left :L
	def right :Relation[R]
	def last :JoinedRelation[FromLast, R]
	def condition :SQLBoolean[Generalized]

	//WithLeft/GeneralizedLeft



	override type Generalized >: Self <: (left.Generalized Extended R) { type Generalized <: thisClause.Generalized }

	override type Self <: (left.Self Extended R) {
		type Self = thisClause.Self; type Generalized = thisClause.Generalized
	}

	override type This >: this.type <: L Extended R

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

	/** Apply a filter condition to the last relation in this clause. The condition is combined using `&&` with
	  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * It is equivalent to `this.where(entities => condition(entities.last))`.
	  * @param condition a function accepting the expression for the last relation in this clause and creating
	  *                  an additional SQL expression for the join condition.
	  * @return a `AndFrom` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  */
	def whereLast(condition :JoinedRelation[FromClause Extended R, R] => SQLBoolean[FromClause Extended R]) :This =
		where(SQLScribe.groundFreeComponents(generalized, condition(last)))



	private[this] val lzyFilter = Lazy { filter(generalized) }

	override def filter :SQLBoolean[Generalized] = lzyFilter

//	override def size = left.size + 1


	override def canEqual(that :Any) :Boolean = that.getClass == getClass

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true

		case join :Extended[_, _] if canEqual(join) && join.canEqual(this) =>
			join.last == join.last && join.left == left

		case _ => false
	}

	override def hashCode :Int = left.hashCode * 31 + last.hashCode

}



//todo: all join methods should be available only for UngroupedFrom and left side of all joins should be UngroupedFrom.
//  Also, move this to the FromClause object
trait UngroupedFrom extends FromClause



trait GroupedFrom extends FromSome { //GroupByClause?
	type Grouped <: FromClause

	def having(condition :JoinedEntities[Generalized] => SQLBoolean[Generalized]) :This = where(condition)
}


//todo: rename AndFrom => AndFrom, make L <: UngroupedFrom
//      introduce Extend[+L <: FromClause, M[A] <: MappingAt[A]] as the new base trait
//      extend ByAll from Extend



/**
  * @author Marcin Mościcki
  */
trait GroupByAll[+F <: FromClause, M[A] <: MappingAt[A]] extends Extended[F, M] with GroupedFrom {

	override type Grouped = left.Self

	override type Generalized = left.Generalized GroupByAll M
	override type Self = left.Self GroupByAll M
	override type This >: this.type <: F GroupByAll M

//	override def size = left.size + 1
//	override type Row =


	//fixme: Generalized can't upcast Subselect to AndFrom as the former delimits the grouped subselect, the relations
	//  of which aren't available as individual mappings. This causes (currently) a conflict with asSubselectOf:
	//  the latter relies on filter being SQLBoolean[Generalized], more specifically that f.condition where f :From[M]
	//  can be used as a condition for FromClause Subselect M.
	//  This can be fixed by having Generalized upcast to JoinParam/Subselect/TrueJoin (or getting rid of it entirely,
	//  replacing it with Self), but requiring that for f :From[M], f.condition :SQLBoolean[FromClause AndFrom M]
	//  as an exception. Should be doable, but we need to foolproof that Dual InnerJoin M has always self type of From[M].

	override type JoinedWith[+P <: FromSome, +J[+K <: FromSome, T[O] <: MappingAt[O]] <: K Join T] =
		Nothing



	override def joinedWith[L <: FromSome](prefix :L, firstJoin :TrueJoin.*) :Nothing =
		throw new UnsupportedOperationException("GroupBy.joinedWith")


	override type Explicit = left.Explicit GroupByAll M
	override type Inner = left.Inner GroupByAll M
	override type Implicit = left.Implicit
	override type Outer = left.Outer

	override def outer :Outer = left.outer //this may throw UnsupportedOperationException, hence not a val



}



trait ByAll[+F <: GroupedFrom, G[A] <: MappingAt[A]] extends Extended[F, G] with GroupedFrom {
	override type Grouped = left.Grouped
}



object GroupBy {
	type SubselectGrouped
//	type GroupByAll[+F <: FromClause, G[A] <: MappingAt[A]]
	type GroupBy[+F <: FromClause, T] = GroupByAll[F, MappingOf[T]#TypedProjection]
//	type ByAll
	type By[+F <: GroupedFrom, T] = ByAll[F, MappingOf[T]#TypedProjection]
	type By1[+F <: GroupedFrom, T] = ByAll[F, MappingOf[T]#ColumnProjection]
//	type ByOne[+F <: GroupedFrom, T] = ByAll[F, MappingOf[T]#ColumnProjection]
//	type #:[L <: Label, T] =
	type M[A] = MappingAt[A]
	type F = From[M] Join M Join M GroupByAll M By String ByAll M By1 Int



	trait BaseGroupByAll[+F <: FromClause, G[O] <: BaseMapping[S, O], S] extends GroupByAll[F, G] {
		override val last :RelationSQL[FromLast, G, S, FromLast]
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

