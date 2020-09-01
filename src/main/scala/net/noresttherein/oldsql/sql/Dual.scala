package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause.{DiscreteFrom, ExtendedBy, FreeFromSome, FromSome, JoinedEntities, OuterFrom, OuterFromSome, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** An empty ''from'' clause, serving both as a base for SQL selects not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
  * lists (any chain of `AndFrom` classes starts by joining with `Dual` - typically indirectly through
  * the [[net.noresttherein.oldsql.sql.From From]] pseudo join). `Dual`'s `Generalized` type is set to `FromClause`;
  * thus the `Generalized` form of any `FromClause` being ''incomplete'' and open-ended at the beginning.
  * As the result, it is a supertype of any ''from'' clause prepending any number of relations to this clause.
  * The drawback is that the `T => T#Generalized` type transformation is not a fixed point operation.
  */
sealed class Dual private (override val filter :SQLBoolean[FromClause]) extends DiscreteFrom { thisClause =>

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = FromClause
	override type FromNext[E[+L <: FromClause] <: FromClause] = Nothing
	override type This = Dual

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def lastAsIn[E <: FromClause](implicit extension :FromClause PrefixOf E) :Nothing = last

	override type Generalized = FromClause
	override type Self = Dual


	override def where(filter :SQLBoolean[FromClause]) :Dual =
		if (filter == this.fullFilter || filter == True) this
		else new Dual(this.fullFilter && filter)

	override def where(condition :JoinedEntities[FromClause] => SQLBoolean[FromClause]) :Dual = {
		val bool = condition(new JoinedEntities(this))
		if (bool == True) this
		else new Dual(fullFilter && bool)
	}


	override type JoinFilter[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	              (next :F)(filter :JoinFilter[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
		throw new UnsupportedOperationException(s"Dual.filterNext($next)")


	override def fullFilter :SQLBoolean[FromClause] = filter

	override def fullFilter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :SQLBoolean[E] =
		filter


	override def isEmpty :Boolean = true
	override def fullSize = 0

	override type Params = @~

	override def isParameterized :Boolean = false

	override type FullRow = @~

	override def fullRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def fullRow[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def fullTableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def fullTableStack[E <: FromClause]
	                           (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty



	override type Extend[J[+L <: FromSome, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] = From[T]

	override def extend[T[O] <: BaseMapping[S, O], S]
	                   (next :Relation[T], filter :SQLBoolean[FromClause AndFrom T], join :Join.*) :From[T] =
		From[T, S](next, filter)

	protected[sql] override def extend[T[O] <: BaseMapping[S, O], S]
	                                  (right :LastRelation[T, S], filter :SQLBoolean[FromClause AndFrom T])
			:this.type AndFrom T =
		From.narrow(this, right, filter)


	override type AppendedTo[+P <: DiscreteFrom] = P

	override def appendedTo[P <: DiscreteFrom](prefix :P) :P = prefix


	override type JoinWith[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R, F <: FromClause] = F

	override def joinWith[F <: FromClause](suffix :F, join :Join.*) :F = suffix


	override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] = P

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :TrueJoin.*) :F = prefix

	override type JoinedWithSubselect[+P <: FromClause] = Nothing

	override def joinedWithSubselect[F <: DiscreteFrom](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"Dual.joinedWithSubselect($prefix)")



	override def isSubselect = false
	override def isValidSubselect = false

	override type Explicit = FromClause
	override type Inner = DiscreteFrom
	override type Implicit = FromClause
	override type Outer = Dual
	override type Base = FromClause
	override type DefineBase[+I <: FromClause] = I

	override def outer :Dual = this
	override def base :FromClause = this

	override def innerSize = 0



	override def filter[E <: FromClause](target :E)(implicit extension :FromClause ExtendedBy E) :SQLBoolean[E] =
		filter


	override type InnerRow = @~

	override def innerRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def innerRow[E <: FromClause]
	                     (target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, @~] =
		ChainTuple.EmptyChain

	override def innerTableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def innerTableStack[E <: FromClause]
	             (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty


	override type OuterRow = @~

	override def outerRow :ChainTuple[FromClause, @~] = ChainTuple.EmptyChain

	override def outerRow[E <: FromClause]
	                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[FromClause, @~] =
		ChainTuple.EmptyChain


	override type AsSubselectOf[+F <: FromClause] = Nothing

	override def asSubselectOf[F <: FromClause](outer :F)(implicit extension :FromClause ExtendedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)

//	override type BaseSubselectOf[+F <: FromSome, T[O] <: MappingAt[O]] = F Subselect T
//
//	protected override def baseSubselectOf[F <: FromSome, T[O] <: BaseMapping[S, O], S]
//	                            (outer :F, next :LastRelation[T, S], condition :SQLBoolean[FromClause AndFrom T])
//			:outer.type Subselect T =
//		Subselect[outer.type, T, S](outer, next)(fullFilter && condition)


	override type FromSubselect[+F <: FromSome] = F { type Implicit = FromClause }

	override type FromRelation[T[O] <: MappingAt[O]] = From[T]

	override def from[F <: FreeFromSome](suffix :F) :F = suffix

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (next :Relation[M])
//	                 (implicit cast :InferSubject[this.type, Subselect, M, T, S])
	                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:From[T] =
		From(cast(next))

	override def fromSubselect[F <: FromSome]
	                          (subselect :F)(implicit extension :subselect.Implicit ExtendedBy FromClause)
			:F { type Implicit = FromClause; type DefineBase[+I <: FromClause] = subselect.DefineBase[I] } =
		subselect.asInstanceOf[F { type Implicit = FromClause; type DefineBase[+I <: FromClause] = subselect.DefineBase[I] }]



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (dual eq this) || dual.fullFilter == fullFilter
		case _ => false
	}


	override def toString :String = if (fullFilter == True) "Dual" else "Dual where " + fullFilter

}






/** An empty ''from'' clause, serving both as a base for expressions not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle), and terminator element for join lists -
  * any chain of [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] types is eventually terminated by a `Dual` instance,
  * either directly or, more often, indirectly through the pseudo join [[net.noresttherein.oldsql.sql.From From]].
  * Note that the [[net.noresttherein.oldsql.sql.Dual Dual]] ''can'' contain a filter condition for the ''where'' clause,
  * so when pattern matching for the empty clause it is necessary to either match by class or the `unapply` method
  * of this object.
  */
object Dual extends Dual(True) {

	def unapply(source :FromClause) :Boolean = source.isInstanceOf[Dual]
}

