package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome, JoinedEntities, OuterFrom, PrefixOf}
import net.noresttherein.oldsql.sql.Join.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** An empty ''from'' clause, serving both as a base for SQL expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for `AndFrom` lists
  * (any chain of `AndFrom` classes starts by joining with `Dual`).
  */
sealed class Dual private (override val filter :SQLBoolean[FromClause]) extends FromClause {

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = FromClause
//	override type FromNext[E[+L <: FromClause] <: L AndFrom N, N[O] <: MappingAt[O]] = FromLast AndFrom N
	override type This = Dual
	override type Init = Dual

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def lastAsIn[E <: FromSome](implicit extension :FromClause PrefixOf E) :Nothing = last

	override def size = 0
	override def isEmpty :Boolean = true

	override type Generalized = FromClause
	override type Self = Dual

//	override def self :Dual = this

	override def filter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :SQLBoolean[E] =
		filter

	override type Row = @~

	override def row :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def row[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def tableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def tableStack[E <: FromClause]
	                       (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty



	override type Extend[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, T[O] <: MappingAt[O]] = From[T]

	override def extend[T[O] <: BaseMapping[S, O], S]
	                   (next :Relation[T], filter :SQLBoolean[FromClause AndFrom T], join :Join.*) :From[T] =
		From[T, S](next, filter)

	protected[sql] def extend[T[O] <: BaseMapping[S, O], S]
	                         (right :LastRelation[T, S], filter :SQLBoolean[FromClause AndFrom T]) :this.type AndFrom T =
		From.narrow(this, right, filter)


	override type AppendedTo[+P <: FromClause] = P

	override def appendedTo[P <: FromClause](prefix :P) :P = prefix


	override type JoinWith[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, F <: FromClause] = F

	override def joinWith[F <: FromClause](suffix :F, join :Join.*) :F = suffix

	override type JoinedWith[+P <: FromClause, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] = P

	override def joinedWith[F <: FromClause](prefix :F, firstJoin :TrueJoin.*) :F = prefix

	override def joinedAsSubselect[F <: FromSome](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"Dual.joinedAsSubselect($prefix)")


	override def from[F <: OuterFrom](suffix :F) :F = suffix

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (next :Relation[M])
//	                 (implicit cast :InferSubject[this.type, Subselect, M, T, S])
	                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:From[T] =
		From(cast(next))



	override type Explicit = FromClause
	override type Inner = FromClause
	override type Implicit = FromClause
	override type Outer = Dual

	override def outer :Dual = this

	override def subselectSize = 0



	override def subselectFilter :SQLBoolean[FromClause] = filter

	override def subselectFilter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :SQLBoolean[E] =
		filter


	override type SubselectRow = @~

	override def subselectRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def subselectRow[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def subselectTableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def subselectTableStack[E <: FromClause]
	             (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty



	override type AsSubselectOf[+F <: FromClause] = Nothing

	override def asSubselectOf[F <: FromClause](outer :F)(implicit extension :FromClause ExtendedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)



	override type Params = @~

//	override type JoinFilter[T[O] <: MappingAt[O]] = Nothing

//	override type JoinFilter[E[L <: FromSome] <: L AndFrom T, T[O] <: MappingAt[O]] = Nothing

	override def where(filter :SQLBoolean[FromClause]) :Dual =
		if (filter == this.filter || filter == True) this
		else new Dual(this.filter && filter)

	override def where(condition :JoinedEntities[FromClause] => SQLBoolean[FromClause]) :Dual = {
		val bool = condition(new JoinedEntities(this))
		if (bool == True) this
		else new Dual(filter && bool)
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (dual eq this) || dual.filter == filter
		case _ => false
	}


	override def toString :String = if (filter == True) "Dual" else "Dual where " + filter

}






/** An empty ''from'' clause, serving both as a base for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle), and terminator element for join lists
  * (by default any chain of `AndFrom` classes is eventually terminated by a `Dual` instance).
  */
object Dual extends Dual(True) {

	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

