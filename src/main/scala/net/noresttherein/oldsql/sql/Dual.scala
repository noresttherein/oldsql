package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{As, ExtendedBy, FreeFrom, FromClauseMatrix, JoinedMappings, NonEmptyFrom, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
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
sealed class Dual private (override val filter :GlobalBoolean[FromClause])
	extends DiscreteFrom with FromClauseMatrix[Dual]
{ thisClause =>

	override type LastMapping[O] = Nothing
	override type LastTable[-F <: FromClause] = Nothing
	override type FromLast = FromClause
	override type FromNext[E[+L <: FromClause] <: FromClause] = Nothing

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def lastAsIn[E <: FromClause](implicit extension :FromClause PrefixOf E) :Nothing = last

	override type Generalized = FromClause
	override type Dealiased = Dual
	override type Self = Dual
	override type Copy = Dual

	override def filtered[S >: GlobalScope <: GlobalScope](condition :SQLBoolean[FromClause, S]) :Dual =
		if (filter == condition || condition == True) this
		else new Dual(condition && filter)

	override def where(condition :JoinedMappings[Dual] => GlobalBoolean[FromClause]) :Dual = {
		val bool = condition(new JoinedMappings(this))
		if (bool == True) this
		else new Dual(filter && bool)
	}


	override type FilterNext[E[+L <: FromSome] <: L Extended N, S <: FromClause Extended N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	override def filterNext[F <: FromClause AndFrom N, N[O] <: MappingAt[O]]
	              (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
		throw new UnsupportedOperationException(s"Dual.filterNext($next)")



	override def isParameterized :Boolean = false

	override type Params = @~
	override type BoundParamless = DiscreteFrom { type Params = @~ }
	override type Paramless = Dual
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(params: @~) :Dual = this

	protected override def decoratedBind[D <: BoundParamless](params: @~)(decorate :Dual => D) :D =
		decorate(this)



	override def isEmpty :Boolean = true
	override def fullSize = 0


	override type FullRow = @~

	override def fullRow :ChainTuple[FromClause, GlobalScope, @~] = ChainTuple.EmptyChain

	override def fullRow[E <: FromClause]
	                    (target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, GlobalScope, @~] =
		ChainTuple.EmptyChain

	override def fullTableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def fullTableStack[E <: FromClause]
	                           (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty



	override type Extend[J[+L <: FromSome, R[O] <: T[O]] <: L Extended R, T[O] <: MappingAt[O]] = From[T]

	override def extend[T[O] <: BaseMapping[S, O], S]
	                   (next :Relation[T], filter :GlobalBoolean[FromClause AndFrom T], join :JoinLike.*) :From[T] =
		From[T, S](next, filter)

	protected[sql] override def extend[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                  (right :LastRelation[T, S], alias :Option[A],
	                                   filter :GlobalBoolean[FromClause AndFrom T]) :this.type AndFrom T As A =
		From.custom(this, right, alias, filter)


	override type JoinWith[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L AndFrom R, F <: FromClause] = F

	override def joinWith[F <: FromClause](suffix :F, join :JoinLike.*) :F = suffix


	override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] = P

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :F = prefix

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = Nothing

	override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"Dual.joinedWithSubselect($prefix)")

	override def appendedTo[P <: DiscreteFrom](prefix :P) :P = prefix



	override def isSubselect = false
	override def isValidSubselect = false

	override type Explicit = FromClause
	override type Inner = DiscreteFrom
	override type Implicit = FromClause
	override type Outer = Dual
	override type Base = FromClause
	override type DefineBase[+I <: FromClause] = I

	override val outer :Dual = this
	override def base :FromClause = this

	override def innerSize = 0



	override def filter[E <: FromClause](target :E)(implicit extension :FromClause PartOf E) :GlobalBoolean[E] =
		filter


	override type InnerRow = @~

	override def innerRow :ChainTuple[FromClause, GlobalScope, @~] = ChainTuple.EmptyChain

	override def innerRow[E <: FromClause]
	                     (target :E)(implicit extension :FromClause ExtendedBy E) :ChainTuple[E, GlobalScope, @~] =
		ChainTuple.EmptyChain

	override def innerTableStack :LazyList[RelationSQL.AnyIn[FromClause]] = LazyList.empty

	override def innerTableStack[E <: FromClause]
	             (target :E)(implicit extension :FromClause ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty


	override type OuterRow = @~

	override def outerRow :ChainTuple[FromClause, GlobalScope, @~] = ChainTuple.EmptyChain

	override def outerRow[E <: FromClause]
	                     (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[FromClause, GlobalScope, @~] =
		ChainTuple.EmptyChain


	override type AsSubselectOf[+F <: FromClause] = Nothing

	override def asSubselectOf[F <: FromClause](outer :F)(implicit extension :FromClause ExtendedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)



	override type FromSubselect[+F <: NonEmptyFrom] = F { type Implicit = FromClause }

	override type FromRelation[T[O] <: MappingAt[O]] = From[T]

	override def from[F <: FreeFrom](suffix :F) :F = suffix

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (next :Relation[M])
//	                 (implicit cast :InferSubject[this.type, Subselect, M, T, S])
	                 (implicit cast :Conforms[Relation[M], Relation[T], Relation[MappingOf[S]#TypedProjection]])
			:From[T] =
		From(cast(next))

	override def fromSubselect[F <: NonEmptyFrom]
	                          (subselect :F)(implicit extension :subselect.Implicit ExtendedBy FromClause)
			:F { type Implicit = FromClause; type DefineBase[+I <: FromClause] = subselect.DefineBase[I] } =
		subselect.asInstanceOf[F { type Implicit = FromClause; type DefineBase[+I <: FromClause] = subselect.DefineBase[I] }]



	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.dual(this)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (dual eq this) || dual.filter == filter
		case _ => false
	}


	override def toString :String = if (filter == True) "Dual" else "Dual where " + filter

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

