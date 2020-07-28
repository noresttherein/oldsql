package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.BaseMapping
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, JoinedTables}
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation
import net.noresttherein.oldsql.sql.MappingSQL.SQLRelation.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** An empty ''from'' clause, serving both as a base for SQL expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for `With` lists
  * (any chain of `With` classes starts by joining with `Dual`).
  */
sealed class Dual private (override val filter :SQLBoolean[FromClause]) extends FromClause {

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = FromClause
	override type This = Dual
	override type Init = Dual

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def size = 0

	override def subselectSize = 0



	override type Self = Dual

	override def self :Dual = this

	override type Generalized = FromClause



	override type JoinedWith[+P <: FromClause, +J[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R] = P

	override def joinedWith[F <: FromClause](prefix :F, firstJoin :Join.*) :F = prefix

	override type ExtendJoinedWith[+F <: FromClause, +J[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R,
	                               +N[+L <: FromClause, R[O] <: MappingAt[O]] <: L Join R, T[O] <: MappingAt[O]]
		= J[F, T]

	override def extendJoinedWith[F <: FromClause, T[O] <: BaseMapping[X, O], X]
	                             (prefix :F, firstJoin :Join.*, nextJoin :this.type Join T)
			:ExtendJoinedWith[F, firstJoin.LikeJoin, nextJoin.LikeJoin, T] =
		firstJoin.likeJoin[F, T, X](prefix, nextJoin.right)(nextJoin.condition :SQLBoolean[Generalized With T])



	override type Implicit = FromClause

	override def outer :Dual = this

	override type Explicit = FromClause

	override type Outer = Dual

	override type Inner = Dual



//	override type AsSubselectOf[F <: FromClause] = F
//
//	override def asSubselectOf[F <: FromClause](outer :F)(implicit extension :FromClause ExtendedBy F) :F = outer
//

//	override def asSubselect :Option[SubselectFrom { type Implicit = FromClause }] = None

	override type AsSubselectOf[F <: FromClause] = Nothing

	override def asSubselectOf[F <: FromClause](outer :F)(implicit extension :FromClause ExtendedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)


	override type ExtendAsSubselectOf[F <: FromClause, J[+L <: FromClause, R[A] <: MappingAt[A]] <: L Join R,
	                                  T[A] <: MappingAt[A]] =
		F Subselect T

	override def extendAsSubselectOf[F <: FromClause, T[A] <: BaseMapping[S, A], S]
	                                (newOuter :F, next :this.type ProperJoin T)(implicit extension :FromClause ExtendedBy F)
			:newOuter.type Subselect T =
		Subselect[newOuter.type, T, T, S](newOuter, next.right, next.condition) //todo: make sure to reuse the mapping


	override type Row = @~

	override def row :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def row[E <: FromClause](target :E)(implicit stretch :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def tableStack :LazyList[SQLRelation.AnyIn[FromClause]] = LazyList.empty

	override def tableStack[E <: FromClause]
	                       (target :E)(implicit stretch :FromClause ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
		LazyList.empty



	override type SubselectRow = @~

	override def subselectRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def subselectRow[E <: FromClause](target :E)(implicit stretch :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def subselectTableStack :LazyList[SQLRelation.AnyIn[FromClause]] = LazyList.empty

	override def subselectTableStack[E <: FromClause]
	             (target :E)(implicit stretch :FromClause ExtendedBy E) :LazyList[SQLRelation.AnyIn[E]] =
		LazyList.empty



	override def filter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :SQLBoolean[E] =
		filter

	override def subselectFilter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E)
			:SQLBoolean[E] =
		filter



	override type JoinFilter[T[O] <: MappingAt[O]] = Nothing



	protected override def withCondition(filter :SQLBoolean[FromClause]) :Dual =
		if (filter == this.filter) this
		else new Dual(filter)

	override def where(condition :JoinedTables[FromClause] => SQLBoolean[FromClause]) :Dual = {
		val bool = condition(new JoinedTables(this))
		if (bool == True) this
		else new Dual(filter && bool)
	}



	override type Params = @~



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (dual eq this) || dual.filter == filter
		case _ => false
	}



	override def toString :String = if (filter == True) "Dual" else "Dual where " + filter

}






/** An empty ''from'' clause, serving both as a base for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle), and terminator element for join lists
  * (by default any chain of `With` classes is eventually terminated by a `Dual` instance).
  */
object Dual extends Dual(True) {

	protected[sql] override def selfInnerJoin[T[A] <: BaseMapping[X, A], X]
	                                         (right :LastRelation[T, X])(filter :SQLBoolean[FromClause With T])
			:From[T] with (this.type InnerJoin T) =
		From.newJoin[T, X](right)(filter)


	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

