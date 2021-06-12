package net.noresttherein.oldsql.sql


import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.{ChunkedString, InferTypeParams}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.schema.Relation.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroundFrom, JoinedMappings, NonEmptyFrom, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.RelationSQL
import net.noresttherein.oldsql.sql.ast.SQLLiteral.True
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An empty ''from'' clause, serving both as a base for SQL selects not needing any input tables
  * (like 'SELECT _ FROM DUAL' in Oracle) and an initial element for [[net.noresttherein.oldsql.sql.AndFrom AndFrom]]
  * lists (any chain of `AndFrom` classes starts by joining with `Dual` - typically indirectly through
  * the [[net.noresttherein.oldsql.sql.From From]] pseudo join). `Dual`'s `Generalized` type is set to `RowProduct`;
  * thus the `Generalized` form of any `RowProduct` being ''incomplete'' and open-ended at the beginning.
  * As the result, it is a supertype of any ''from'' clause prepending any number of relations to this clause.
  * The drawback is that the `T => T#Generalized` type transformation is not a fixed point operation.
  */
sealed class Dual private (override val filter :GlobalBoolean[RowProduct])
	extends FromClause with FromClauseTemplate[Dual]
{ thisClause =>

	override type LastMapping[O] = Nothing
	override type Last[-F <: RowProduct] = Nothing
	override type FromLast = RowProduct
	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def lastAsIn[E <: RowProduct](implicit expansion :RowProduct PrefixOf E) :Nothing = last

	override type Generalized = RowProduct
	override type Dealiased = Dual
	override type Self = Dual
	override type Copy = Dual

	override def filtered[S >: GlobalScope <: GlobalScope](condition :SQLBoolean[RowProduct, S]) :Dual =
		if (filter == condition || condition == True) this
		else new Dual(condition && filter)

	override def where(condition :JoinedMappings[Dual] => GlobalBoolean[RowProduct]) :Dual = {
		val bool = condition(new JoinedMappings(this))
		if (bool == True) this
		else new Dual(filter && bool)
	}


	override type FilterNext[E[+L <: FromSome] <: L Expanded N, S <: RowProduct Expanded N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	              (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
		throw new UnsupportedOperationException(s"Dual.filterNext($next)")



	override def paramCount = 0
	override def lastParamOffset :Nothing = throw new UnsupportedOperationException("Dual.lastParamOffset")
	override def isParameterized :Boolean = false
	override def isSubselectParameterized :Boolean = false

	override type LastParam = Nothing
	override type Params = @~
	override type AppliedParam = Nothing
	override type GeneralizedParamless = FromClause
	override type Paramless = Dual
	override type BoundParamless = FromClause { type Params = @~ } //todo: FromClause | WithClause
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :Nothing) :Nothing = throw new UnsupportedOperationException("Dual.bind(" + param + ")")
	override def bind(params: @~) :Dual = this

	protected override def decoratedBind[D <: BoundParamless](params: @~)(decorate :Dual => D) :D =
		decorate(this)



	override def isEmpty :Boolean = true
	override def fullSize = 0


	override type FullRow = @~

	override def fullRow :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
		ChainTuple.EmptyChain

	override def fullTableStack :LazyList[RelationSQL.AnyIn[RowProduct]] = LazyList.empty

	override def fullTableStack[E <: RowProduct]
	                           (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty



	override type Expand[J[+L <: FromSome, R[O] <: T[O]] <: L Expanded R, T[O] <: MappingAt[O]] = From[T]

	override def expand[T[O] <: BaseMapping[S, O], S]
	                   (next :Table[T], filter :GlobalBoolean[RowProduct AndFrom T], join :JoinLike.*) :From[T] =
		From[T, S](next, filter)

	protected[sql] override def expand[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                  (right :LastTable[T, S], alias :Option[A],
	                                   filter :GlobalBoolean[RowProduct NonParam T]) :this.type NonParam T As A =
		From.custom(this, right, alias, filter)


	override type JoinWith[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L NonParam R, F <: RowProduct] = F

	override def joinWith[F <: RowProduct](suffix :F, join :JoinLike.*) :F = suffix


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = P

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :F = prefix

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = Nothing

	override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"Dual.joinedWithSubselect($prefix)")

	override def appendedTo[P <: FromClause](prefix :P) :P = prefix



	override def isSubselect = false
	override def isValidSubselect = false

	override type Explicit = RowProduct
	override type Inner = FromClause
	override type Implicit = RowProduct
	override type Outer = Dual
	override type Base = RowProduct
	override type DefineBase[+I <: RowProduct] = I

	override val outer :Dual = this
	override def base :RowProduct = this

	override def size = 0



	override def filter[E <: RowProduct](target :E)(implicit expansion :RowProduct PartOf E) :GlobalBoolean[E] =
		filter


	override type Row = @~

	override def row :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
		ChainTuple.EmptyChain

	override def tableStack :LazyList[RelationSQL.AnyIn[RowProduct]] = LazyList.empty

	override def tableStack[E <: RowProduct]
	             (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
		LazyList.empty


	override type OuterRow = @~

	override def outerRow :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain

	override def outerRow[E <: RowProduct]
	                     (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[RowProduct, GlobalScope, @~] =
		ChainTuple.EmptyChain



	override type AsSubselectOf[+F <: RowProduct] = Nothing

	override def asSubselectOf[F <: RowProduct](outer :F)(implicit expansion :RowProduct ExpandedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)



	override type FromSubselect[+F <: NonEmptyFrom] = F { type Implicit = RowProduct }

	override type FromRelation[T[O] <: MappingAt[O]] = From[T]

	override def from[F <: GroundFrom](suffix :F) :F = suffix

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (next :Table[M])
	                 (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:From[T] =
		From(cast(next))

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
	                 (next :StaticTable[A, M])
	                 (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
			:From[T] As A =
		From(cast(next))

	override def fromSubselect[F <: NonEmptyFrom]
	                          (subselect :F)(implicit expansion :subselect.Implicit ExpandedBy RowProduct)
			:F { type Implicit = RowProduct; type DefineBase[+I <: RowProduct] = subselect.DefineBase[I] } =
		subselect.asInstanceOf[F { type Implicit = RowProduct; type DefineBase[+I <: RowProduct] = subselect.DefineBase[I] }]


	protected override def defaultSpelling(context :SQLContext)
	                                      (implicit spelling :SQLSpelling) :SpelledSQL[@~, RowProduct] =
		if (filter == True) spelling.emptyFrom(context)
		else spelling.emptyFrom(context) && (spelling(filter)(_, _))

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext = spelling.newContext

	override def parameterization :Parameterization[@~, Dual] = Parameterization.paramless[Dual]


	protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Y = matcher.dual(this)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]

	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (dual eq this) || dual.filter == filter
		case _ => false
	}


	override def chunkedString :ChunkedString =
		if (filter == True) Dual.chunkedString else Dual.chunkedString + " where " + filter.toString

	override def toString :String = if (filter == True) "Dual" else "Dual where " + filter.toString
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

	def unapply(source :RowProduct) :Boolean = source.isInstanceOf[Dual]

	override val chunkedString = ChunkedString("Dual")
}

