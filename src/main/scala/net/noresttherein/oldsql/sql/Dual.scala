package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.morsels.{ChunkedString, InferTypeParams}
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.RowProduct.{As, EmptyRow, ExpandedBy, GroundRow, NonEmptyRow, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** An empty ''from'' clause, serving both as a base for SQL selects not needing any input tables
  * (like 'SELECT ... FROM DUAL' in the Oracle DBMS) and an initial element
  * for [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] lists (any chain of `AndFrom` classes starts by joining
  * with `Dual` - typically indirectly through the [[net.noresttherein.oldsql.sql.From From]] pseudo join).
  * `Dual`'s `Generalized` type is set to `RowProduct`, so the `Generalized` form of any `RowProduct`
  * is ''incomplete'' and open-ended at the beginning. As the result, it is a supertype of any ''from'' clause
  * prepending any number of relations to that clause. The drawback is that the `T => T#Generalized` type transformation
  * is not a fixed point operation.
  *
  * The companion object to this class extends it and is the canonical instance. Note however that it is possible
  * to create other instances, for example to prepend a ''with'' clause.
  */
sealed class Dual private (override val withClause :WithClause, override val filter :SingleBoolean[RowProduct])
	extends FromClause with FromClauseTemplate[Dual] with EmptyRow
{ thisClause =>
	private def this(filter :SingleBoolean[RowProduct]) = this(filter.outerWithClause, filter)

//	override type LastMapping[O] = Nothing
//	override type Last[-F <: RowProduct] = Nothing
//	override type FromLast = RowProduct
//	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override def last :Nothing = throw new NoSuchElementException("Dual.last")

	override def lastAsIn[E <: RowProduct](implicit expansion :RowProduct PrefixOf E) :Nothing = last

	override def selfClass :Class[Dual] = classOf[Dual]
//	override type Generalized = RowProduct
	override type Complete = Dual
	override type NoAlias = Dual
	override type Self = Dual
	override type Copy = Dual

	override def filtered[S >: Single <: Single](condition :SQLBoolean[RowProduct, S]) :Dual =
		if (filter == condition || condition == True) this
		else new Dual(filter && condition)

	override def define(withClause :WithClause) :Dual =
		if (withClause.isEmpty || withClause.forall(this.withClause.contains)) this
		else new Dual(this.withClause ++ withClause, filter)


//	override type FilterNext[E[+L <: FromSome] <: RowProduct Expanded N,
//	                         S <: RowProduct Expanded N, G <: S, N[O] <: MappingAt[O]] = Nothing
//
//	override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
//	              (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) :Nothing =
//		throw new UnsupportedOperationException(s"Dual.filterNext($next)")



	override def paramCount = 0
	override def lastParamOffset :Nothing = throw new UnsupportedOperationException("Dual.lastParamOffset")
	override def isParameterized :Boolean = false
	override def isExplicitParameterized :Boolean = false

	override type LastParam            = Nothing
	override type Params               = @~
	override type AppliedParam         = Nothing
	override type GeneralizedParamless = FromClause
	override type Paramless            = Dual
	override type BoundParamless       = FromClause { type Params = @~ } //todo: FromClause | WithClause
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :Nothing) :Nothing = throw new UnsupportedOperationException("Dual.bind(" + param + ")")
	override def bind(params: @~) :Dual = this

	protected override def decoratedBind[D <: BoundParamless](params: @~)(decorate :Dual => D) :D =
		decorate(this)



	override def isEmpty :Boolean = true
	override def fullSize = 0


//	override type FullRow = @~
//
//	override def fullRow :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain
//
//	override def fullRow[E <: RowProduct]
//	                    (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
//		ChainTuple.EmptyChain
//
//	override def fullTableStack :Seq[RelationSQL.AnyIn[RowProduct]] = Nil
//
//	override def fullTableStack[E <: RowProduct]
//	                           (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
//		LazyList.empty



	override type JoinWith[J[+L <: FromSome, R[O] <: MappingAt[O]] <: L NonParam R, F <: RowProduct] = F

	override def joinWith[F <: RowProduct](suffix :F, join :JoinLike.__) :F = suffix


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = P

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :F = prefix

	override type SelectedFrom[+P <: NonEmptyRow] = Nothing

	override def selectedFrom[F <: NonEmptyRow](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"Dual.selectedFrom($prefix)")

	override def appendedTo[P <: FromClause](prefix :P) :P = prefix



	override def isSubselect = false
	override def isValidSubselect = false

	override type Explicit = RowProduct
	override type Inner    = FromClause
	override type Implicit = RowProduct
	override type Outer    = Dual
	override type Base     = RowProduct
	override type DefineBase[+I <: RowProduct] = I

	override val outer :Dual = this
	override def base :RowProduct = this

//	override def size = 0



	override def filter[E <: RowProduct](target :E)(implicit expansion :RowProduct PartOf E) :SingleBoolean[E] =
		filter


//	override type Row = @~
//
//	override def row :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain
//
//	override def row[E <: RowProduct]
//	                (target :E)(implicit expansion :RowProduct ExpandedBy E) :ChainTuple[E, GlobalScope, @~] =
//		ChainTuple.EmptyChain
//
//	override def tableStack :Seq[RelationSQL.AnyIn[RowProduct]] = Nil
//
//	override def tableStack[E <: RowProduct]
//	             (target :E)(implicit expansion :RowProduct ExpandedBy E) :LazyList[RelationSQL.AnyIn[E]] =
//		LazyList.empty
//
//
//	override type OuterRow = @~
//
//	override def outerRow :ChainTuple[RowProduct, GlobalScope, @~] = ChainTuple.EmptyChain
//
//	override def outerRow[E <: RowProduct]
//	                     (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[RowProduct, GlobalScope, @~] =
//		ChainTuple.EmptyChain



	override type AsSubselectOf[+F <: RowProduct] = Nothing

	override def asSubselectOf[F <: RowProduct](outer :F)(implicit expansion :RowProduct ExpandedBy F) :Nothing =
		throw new UnsupportedOperationException("Can't represent Dual as a subselect of " + outer)



	override type FromSubselect[+F <: NonEmptyRow] = F { type Implicit = RowProduct }

	protected[sql] override def from[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                (right :LastTable[T, S], alias :Option[A],
	                                 filter :SingleBoolean[GeneralizedFromTable[T]]) :this.type DualJoin T As A =
		From.any(this, right, alias, filter)

	override type FromTable[T[O] <: MappingAt[O]] = From[T]

	override def from[F <: GroundRow](suffix :F) :F = suffix

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

	override def fromSubselect[F <: NonEmptyRow]
	                          (subselect :F)(implicit expansion :subselect.Implicit ExpandedBy RowProduct)
			:F { type Implicit = RowProduct; type DefineBase[+I <: RowProduct] = subselect.DefineBase[I] } =
		subselect.asInstanceOf[F { type Implicit = RowProduct; type DefineBase[+I <: RowProduct] = subselect.DefineBase[I] }]

/*
	override type Expand[J[+L <: Self, R[O] <: T[O]] <: L AndFrom R, T[O] <: MappingAt[O]] = From[T]

	override def expand[T[O] <: BaseMapping[S, O], S]
	                   (next :Table[T], filter :SingleBoolean[RowProduct AndFrom T], join :JoinLike.__) :From[T] =
		From[T, S](next, filter)

	protected[sql] override def expand[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                  (right :LastTable[T, S], alias :Option[A],
	                                   filter :SingleBoolean[RowProduct NonParam T]) :this.type NonParam T As A =
		From.any(this, right, alias, filter)
*/



	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.inWhere.sqlParamCount(filter)

	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, RowProduct])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		if (filter == True) spelling.emptyFrom(context)
		else spelling.emptyFrom(context) && (spelling.inWhere(filter)(this, _, params))

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[@~] = spelling.newContext

	override def parameterization :Parameterization[@~, Dual] = Parameterization.paramless[Dual]

	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, RowProduct])
			:Nothing =
		throw new IndexOutOfBoundsException(
			"Cannot return GroupingSpellingContext for relation #" + position + " in " + this + "."
		)



	protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.dual(this)


	override def homomorphic(that :RowProduct) :Boolean = that match {
		case dual :EmptyRow => (this eq dual) || (filter homomorphic dual.filter)
		case _ => false
	}
	override def isomorphic(that :RowProduct) :Boolean = that match {
		case dual :EmptyRow => (this eq dual) || (filter isomorphic dual.filter)
		case _ => false
	}
	override def identical(that :RowProduct) :Boolean = that match {
		case dual :Dual => (this eq dual) || (filter identical dual.filter)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case dual :Dual => (this eq dual) || filter == dual.filter
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Dual]
	override def hashCode :Int = if (filter == True) System.identityHashCode(Dual) else filter.hashCode

	override def chunkedString :ChunkedString =
		if (filter == True) Dual.chunkedString else Dual.chunkedString + " where " + filter.toString

	override def typeString(res :StringBuilder) :StringBuilder = res ++= "Dual"

	override def toString :String = if (filter == True) "Dual" else "Dual where " + filter.toString
}






/** An empty ''from'' clause, serving both as a base for SQL selects not needing any input tables
  * (like 'SELECT ... FROM DUAL' in the Oracle DBMS) and an initial element
  * for [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] lists (any chain of `AndFrom` classes starts by joining
  * with `Dual` - typically indirectly through the [[net.noresttherein.oldsql.sql.From From]] pseudo join).
  * `Dual`'s `Generalized` type is set to `RowProduct`, so the `Generalized` form of any `RowProduct`
  * is ''incomplete'' and open-ended at the beginning. As the result, it is a supertype of any ''from'' clause
  * prepending any number of relations to that clause. The drawback is that the `T => T#Generalized` type transformation
  * is not a fixed point operation.
  *
  * Note that the [[net.noresttherein.oldsql.sql.Dual Dual]] ''can'' contain a filter condition for the ''where'' clause,
  * or a non-empty ''with'' clause, so when pattern matching for the empty clause it is necessary to either match
  * by class or the `unapply` method of this object.
  */
object Dual extends Dual(True) {

	def unapply(source :RowProduct) :Boolean = source.isInstanceOf[Dual]

	override val chunkedString = ChunkedString("Dual")
}

