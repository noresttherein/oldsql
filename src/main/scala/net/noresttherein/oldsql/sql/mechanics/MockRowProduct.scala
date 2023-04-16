package net.noresttherein.oldsql.sql.mechanics

import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.slang.castTypeParam
import net.noresttherein.oldsql.sql
import net.noresttherein.oldsql.sql.{AndFrom, FromClause, FromSome, Join, RowProduct, Seal, SingleBoolean, SQLBoolean, SQLDialect, WithClause}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, GroundRow, NonEmptyRow, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}





/** Used as a ''from'' clause in mock `SelectSQL` implementations used to reform a `CompoundSelectSQL`
  * with an arbitrary expression.
  */
private[sql] object MockRowProduct extends RowProduct {
//	override type LastMapping = this.type
//	override type Last = this.type
	override type FromLast = this.type
//	override type FromNext = this.type
	override type Generalized = this.type
	override type Complete = this.type
	override type NoAlias = this.type
	override type Self = this.type
//	override type Copy = this.type

	private def unsupported(method :String) = throw new UnsupportedOperationException("MockRowProduct." + method)

	override def last = unsupported("last")
	override def lastAsIn[E <: FromSome](implicit expansion :FromLast PrefixOf E) = unsupported("lastAsIn")

	override def generalizedClass = classOf[MockRowProduct.type].castParam[Generalized]
	override def selfClass = classOf[MockRowProduct.type].castParam[Self]

//	override type FilterNext = this.type

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) =
		unsupported("filter")

	 protected override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                                 (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		 unsupported("filterNext")

	override def filtered[S >: Single <: Single](filter :SQLBoolean[Generalized, S]) = unsupported("filtered")

//	override type LastParam = this.type
//	override type Params = this.type
//	override type AppliedParam = this.type
//	override type GeneralizedParamless = this.type
//	override type Paramless = this.type
//	override type BoundParamless = this.type
//	override type DecoratedParamless = this.type

	override def bind(param :LastParam) = unsupported("bind")

	override def bind(params :Params) = unsupported("params")

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) =
		unsupported("decoratedBind")

	override def isExplicitParameterized = false
	override def paramCount = 0
	override def lastParamOffset = unsupported("lastParamOffset")

	override def isEmpty = true
	override def fullSize = 0

	override type FullRow = Nothing

	override def fullRow[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
		unsupported("fullRow")

	override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
		unsupported("fullTableStack")

//	override type JoinedWith = this.type

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) = unsupported("joinedWith")

//	override type SelectedFrom = this.type

	override def selectedFrom[F <: RowProduct.NonEmptyRow](prefix :F) = unsupported("selectedFrom")

	override def appendedTo[F <: FromClause](prefix :F) = unsupported("appendedTo")

	override def isValidSubselect = false

//	override type Explicit = this.type
//	override type Inner = this.type
	override type Implicit = this.type
	override type Outer = this.type
	override val outer :Outer = this
	override type Base = Nothing
	override type DefineBase[+I <: RowProduct] = Nothing

	override def base = unsupported("base")
	override def fromClause = unsupported("fromClause")

	override type Row = Nothing

	override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
		unsupported("row")

	override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
		unsupported("tableStack")

	override type OuterRow = Nothing

	override def outerRow[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E) =
		unsupported("outerRow")

//	override type AsSubselectOf = this.type

	override def asSubselectOf[F <: RowProduct.NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F) =
		unsupported("asSubselectOf")

//	override type FromTable = this.type
//	override protected[sql] type SingletonFromTable = this.type
//	override protected[sql] type GeneralizedFromTable = this.type
//
	protected[sql] override def from[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                (right :LastTable[T, S], alias :Option[A],
	                                 filter :SingleBoolean[GeneralizedFromTable[T]]) :SingletonFromTable[T] As A =
		unsupported("from")

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Table[M])
	                 (implicit infer :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]]) :Nothing =
		unsupported("from")

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
	                 (first :StaticTable[A, M])
	                 (implicit infer :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
			:Nothing =
		unsupported("from")
//	override type FromSubselect = this.type

	override def from[F <: NonEmptyRow with GroundRow](subselect :F) :Nothing = unsupported("from")

	override def fromSubselect[F <: NonEmptyRow]
	                          (subselect :F)(implicit expansion :subselect.Implicit ExpandedBy Generalized) =
		unsupported("fromSubselect")

	override def withClause = WithClause.empty

	protected override def sqlParamCount(implicit spelling :SQLSpelling) = 0

	override def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                     (implicit spelling :SQLSpelling) =
		unsupported("spell")

	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) =
		unsupported("defaultSpelling")

	override def spellingContext(implicit spelling :SQLSpelling) = unsupported("spellingContext")

	override def parameterization = unsupported("parameterization")

	protected override def groupingSpellingContext[P](position :Int, context :SQLContext[P],
	                                                  params :Parameterization[P, Generalized]) =
		unsupported("groupingSpellingContext")

	override def homomorphic(that :RowProduct) = that eq this

	override def isomorphic(that :RowProduct) = that eq this

	override def identical(that :RowProduct) = that eq this

	override def chunkedString = "MockRowProduct"

	override def typeString(res :StringBuilder) = res ++= "MockRowProduct"

	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause(seal :Seal) :Unit = ()

	override def toString = "MockRowProduct"
}
