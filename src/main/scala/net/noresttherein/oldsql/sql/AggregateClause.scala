package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.@~
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.{ChunkedString, InferTypeParams}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.RowProduct.{AggregateOfGeneralized, As, ExpandedBy, GroundRow, NonEmptyRow, PartOf, PrefixOf, RowProductTemplate}
import net.noresttherein.oldsql.sql.SelectFrom.GenericSelectFrom
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ChainTuple, EmptySQL, RelationSQL}
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A base type for all ''from'' clauses of SQL ''selects'' which make use of aggregate functions.
  * This encompasses both queries with a ''group by'' clause
  * - [[net.noresttherein.oldsql.sql.GroupByClause GroupByClause]] - and a special
  * [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] implementation used for ''selects'' which feature
  * an aggregate function in their ''select'' clause, aggregating all rows into a single result.
  */
trait AggregateClause extends RowProduct with RowProductTemplate[AggregateClause] { thisClause =>

	override type FromNext[E[+L <: FromSome] <: RowProduct] = Nothing

	override type Generalized >: Complete <: AggregateClause {
		type LastMapping[O]       = thisClause.LastMapping[O]
		type FromLast             = thisClause.FromLast
		type Generalized         <: thisClause.Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
		type Explicit            <: thisClause.Explicit
		type Implicit            <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base                <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: AggregateClause {
		type LastMapping[O]      = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast            = thisClause.FromLast
		type Generalized         = thisClause.Generalized
		type Complete            = thisClause.Complete
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type LastParam           = thisClause.LastParam
		type Params              = thisClause.Params
		type FullRow             = thisClause.FullRow
		type Explicit            = thisClause.Explicit
		type Implicit            = thisClause.Implicit
		type Base                = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                 = thisClause.Row
		type OuterRow            = thisClause.OuterRow
	}

	override type NoAlias >: Self <: AggregateClause {
		type LastMapping[O]      = thisClause.LastMapping[O]
//		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast            = thisClause.FromLast
		type Generalized         = thisClause.Generalized
		type Complete            = thisClause.Complete
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type LastParam           = thisClause.LastParam
		type Params              = thisClause.Params
		type FullRow             = thisClause.FullRow
		type Explicit            = thisClause.Explicit
		type Implicit            = thisClause.Implicit
		type Base                = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                 = thisClause.Row
		type OuterRow            = thisClause.OuterRow
	}

	override type Self <: AggregateClause {
		type LastMapping[O]      = thisClause.LastMapping[O]
		type FromLast            = thisClause.FromLast
		type Generalized         = thisClause.Generalized
		type Complete            = thisClause.Complete
		type GeneralizedDiscrete = thisClause.GeneralizedDiscrete
		type LastParam           = thisClause.LastParam
		type Params              = thisClause.Params
		type FullRow             = thisClause.FullRow
		type Explicit            = thisClause.Explicit
		type Inner               = thisClause.Inner
		type Implicit            = thisClause.Implicit
		type Base                = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row                 = thisClause.Row
		type OuterRow            = thisClause.OuterRow
	}



	override type FilterNext[E[+L <: FromSome] <: RowProduct Expanded T,
	                         S <: RowProduct Expanded T, G <: S, T[O] <: MappingAt[O]] = Nothing

	protected override def filterNext[F <: RowProduct AndFrom N, N[O] <: MappingAt[O]]
	                       (next :F)(filter :FilterNext[next.GeneralizedLeft, next.FromLast, next.Generalized, N]) =
		throw new UnsupportedOperationException(s"GroupByClause.filterNext (on $this)")


	/** Type constructor for this clause. It applies all grouping/aggregate clauses in the suffix of this type
	  * to an arbitrary ungrouped, non empty ''from'' clause `U`. Syntactically, this replaces
	  * the [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] portion of this type
	  * (that is, the longest prefix type conforming to [[net.noresttherein.oldsql.sql.FromClause FromClause]],
	  * including both the grouped relations in the ''from'' clause and all
	  * [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]] relations) with `U`.
	  */
	type Grouping[+U <: FromSome] <: AggregateClause

	/** The ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * This is the ''generalized'' supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete this.Discrete]].
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Generalized`;
	  * for [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Generalized`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.GeneralizedDiscrete`.
	  */
	type GeneralizedDiscrete >: Discrete <: FromSome {
		type Generalized <: thisClause.GeneralizedDiscrete
	}

	/** The self typ of the ''from'' clause containing all the aggregated relations in its
	  * [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * For [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] it is `clause.Self`;
	  * for [[net.noresttherein.oldsql.sql.GroupBy GroupBy]] it is `left.Self`;
	  * [[net.noresttherein.oldsql.sql.By By]]
	  * (and [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) define it as `left.Discrete`.
	  */
	type Discrete <: FromSome {
		type Generalized = thisClause.GeneralizedDiscrete
	}

	/** The [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] forming the prefix of this clause which contains
	  * all the aggregated relations in its [[net.noresttherein.oldsql.sql.RowProduct.Explicit ''explicit'']] portion.
	  * These relations are ''not'' available to SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * based on this clause.
	  */
	override val fromClause :Discrete

	private[sql] final def generalizedFromClause :GeneralizedDiscrete = fromClause //todo: delete this in Scala3

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.GeneralizedDiscrete GeneralizedDiscrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.RowProduct.Implicit ''implicit'']] prefix with
	  * [[net.noresttherein.oldsql.sql.RowProduct RowProduct]]. Equals `fromClause.Explicit`.
	  */
	type GeneralizedFrom = fromClause.Explicit

	/** The supertype of [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]]
	  * which replaces its whole [[net.noresttherein.oldsql.sql.RowProduct.Outer outer]] prefix with
	  * the upper bound for the left side of the [[net.noresttherein.oldsql.sql.AndFrom join]] between the
	  * outer and inner sections. Equals `fromClause.Inner`.
	  */
	type InnerFrom = fromClause.Inner



//	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] <: Generalized {
//		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete //volatile issue
//	}
//	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*)
//		:JoinedWith[F, firstJoin.LikeJoin] { type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete }

	override type SelectedFrom[+P <: NonEmptyRow] <: JoinedWith[P, Subselect] {
		type Row = thisClause.Row
		type Explicit = thisClause.Explicit
		type Generalized <: thisClause.Generalized
		type GeneralizedDiscrete <: thisClause.GeneralizedDiscrete
//		type Discrete <: GeneralizedDiscrete //volatile
	}


	protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.aggregateClause(this)

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AggregateClause]

}




object AggregateClause {

	def unapply(from :RowProduct) :Opt[FromSome] = from match {
		case aggro :Aggregated.__ => Got(aggro.clause)
		case group :GroupByClause => Got(group.fromClause)
		case _ => Lack
	}
}






/** A special [[net.noresttherein.oldsql.sql.FromSome FromSome]] adapter used when selecting
  * an aggregate [[net.noresttherein.oldsql.sql.ast.AggregateSQL! expression]], coalescing all rows from the adapted
  * clause into a single result. It can be thought of as a ''group by'' clause with zero grouping expressions.
  * Its existence is needed as such aggregate functions, when applied to an ungrouped ''from'' clause `F`,
  * produce an SQL expression based on an [[net.noresttherein.oldsql.sql.AggregateClause aggregate clause]] with `F`
  * as its [[net.noresttherein.oldsql.sql.AggregateClause.Discrete Discrete]] type, and thus cannot be selected
  * from `F` (or used in any other capacity in clause `F`).
  *
  * It is considered a part of the implementors' interface, exposed to applications
  * which add their own aggregate [[net.noresttherein.oldsql.sql.AggregateFunction functions]] or
  * SQL dialects (requiring access to the whole [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] hierarchy).
  * Applications which satisfy themselves with the standard aggregate functions defined in
  * [[net.noresttherein.oldsql.sql.AggregateFunction$ AggregateFunction]] should have no need for this class, as appropriate
  * 'shortcut' methods are defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]
  * implicit extension class for [[net.noresttherein.oldsql.sql.FromSome non-empty, ungrouped]] clauses.
  * Be warned that several standard methods (in particular `where`) are unsupported, as this type should be used only
  * as an intermediate value created directly before calling one of its `select` methods.
  *
  * The [[net.noresttherein.oldsql.sql.RowProduct.filter filter]] condition for this type is always empty
  * ([[net.noresttherein.oldsql.sql.SQLBoolean.True True]]); in fact,
  * no [[net.noresttherein.oldsql.sql.SingleBoolean GlobalBoolean]]`[Aggregated[_]]` can be legally constructed.
  *
  * @tparam F a non-empty, ungrouped ''from'' clause which rows are being aggregated for the purpose of selecting
  *           a result of an aggregate function. As with [[net.noresttherein.oldsql.sql.GroupByClause]], all relations
  *           following the last [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  *           (or [[net.noresttherein.oldsql.sql.Dual Dual]]/[[net.noresttherein.oldsql.sql.From From]]) are unavailable
  *           to [[net.noresttherein.oldsql.sql.SQLExpression SQL expressions]] based on this clause other than
  *           as arguments to aggregate functions.
  * @author Marcin Mościcki
  */
sealed trait Aggregated[+F <: FromSome] extends DecoratedRow[F] with AggregateClause { thisClause =>

	override type LastMapping[O] = Nothing
	override type Last[E <: RowProduct] = Nothing
	override type FromLast = RowProduct

	/** Throws a `NoSuchElementException`. */
	override def last :Nothing = throw new NoSuchElementException(s"($this).last")

	/** Throws a `NoSuchElementException`. */
	override def lastAsIn[E <: FromSome](implicit expansion :FromLast PrefixOf E) :Nothing = last

	override def generalizedClass :Class[Aggregated[clause.Generalized]] = classOf[Aggregated[clause.Generalized]]
	override def selfClass        :Class[Aggregated[clause.Self]] = classOf[Aggregated[clause.Self]]

	override type Bound = FromSome
	override type Generalized = Aggregated[clause.Generalized]
	override type Complete    = Aggregated[clause.Complete]
	override type NoAlias     = Aggregated[clause.NoAlias]
	override type Self        = Aggregated[clause.Self]
	override type Copy        = Aggregated[clause.Copy]


	override def decorate[C <: FromSome](from :C) :Aggregated[C] = Aggregated(from)


	/** Always returns [[net.noresttherein.oldsql.sql.SQLBoolean.True True]]. For the actual, ungrouped ''where'' clause
	  * use `this.clause.filter`.
	  */
	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GroupedBoolean[E] = True

	/** Throws an `UnsupportedOperationException`. */ //consider: allowing True
	override def filtered[S >: Single <: Single](condition :SQLBoolean[Generalized, S]) :Nothing =
		throw new UnsupportedOperationException(s"($this).where($condition)")

	override def define(withClause :WithClause) :Aggregated[clause.Copy] = Aggregated(withClause, clause.selfAsCopy)

	override type AppliedParam = Aggregated[clause.AppliedParam]
	override type GeneralizedParamless = Aggregated[clause.GeneralizedParamless]
	override type Paramless = Aggregated[clause.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :LastParam) :AppliedParam = decorate(clause.bind(param))
	override def bind(params :Params) :Paramless = decorate(clause.bind(params))

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))


	override def isEmpty :Boolean = outer.isEmpty
	override def fullSize :Int = outer.fullSize
	override def size :Int = 0


	override type FullRow = clause.OuterRow

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow] =
		clause.outerRow(target)(explicitSpan expand expansion)

	override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:LazyList[RelationSQL.from[E]#__] =
		outer.fullTableStack(target)(explicitSpan.expand(expansion))


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		Aggregated[clause.JoinedWith[P, J]]

	override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.__) :JoinedWith[P, firstJoin.LikeJoin] =
		decorate(clause.joinedWith(prefix, firstJoin))

	override type SelectedFrom[+P <:  NonEmptyRow] = Aggregated[clause.SelectedFrom[P]]

	override def selectedFrom[P <: NonEmptyRow](prefix :P) :SelectedFrom[P] =
		decorate(clause.selectedFrom(prefix))

	override def appendedTo[P <: FromClause](prefix :P) :Aggregated[clause.JoinedWith[P, NonParam]] =
		decorate(clause.appendedTo(prefix))



	override type Explicit = Aggregated[clause.Explicit]
	override type Inner = Aggregated[clause.Inner]
	override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
	override type DefineBase[+I <: RowProduct] = clause.DefineBase[I]

	override def base :Base = clause.base

	override type Row = @~

	/** Returns an empty chain expression. */
	override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:ChainTuple[E, Single, @~] =
		EmptySQL

	/** Returns an empty list. */
	override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:LazyList[RelationSQL.from[E]#__] =
		LazyList.empty

	override type OuterRow = clause.OuterRow

	override def outerRow[E <: RowProduct](target :E)(implicit expansion :Implicit ExpandedBy E)
			:ChainTuple[E, Single, OuterRow] =
		clause.outerRow(target)


	override type AsSubselectOf[+P <: NonEmptyRow] = Aggregated[clause.AsSubselectOf[P]]

	override def asSubselectOf[P <: NonEmptyRow](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
			:Aggregated[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
		decorate(clause.asSubselectOf(newOuter))



	override type Grouping[+D <: FromSome] = Aggregated[D]
	override type GeneralizedDiscrete = clause.Generalized
	override type Discrete = clause.Self


	override type FromTable[T[O] <: MappingAt[O]] = Self SelectFrom T

	protected[sql] override type SingletonFromTable[T[O] <: MappingAt[O]] = this.type GenericSelectFrom T
	protected[sql] override type GeneralizedFromTable[T[O] <: MappingAt[O]] = Generalized GenericSelectFrom T

	protected[sql] override def from[T[O] <: BaseMapping[S, O], S, A <: Label]
	                                (right :LastTable[T, S], alias :Option[A],
	                                 filter :SingleBoolean[GeneralizedFromTable[T]]) :this.type GenericSelectFrom T As A =
		/* This cast is safe, because there is no GlobalBoolean[Aggregated[_]] which is not GlobalBoolean[RowProduct],
		 * as the only class extending SQLExpression[AggregateClause] is AggregateSQL, which is local, so the filter
		 * must use the nearest upper type, which is RowProduct/EmptyClause, so it in reality uses only the last table.
		 * Anyway, this method is damn unlikely to ever get used. Would be nice to have some sort of assertion, though.
		 */
		SelectFrom.empty(this, right, alias)(filter.asInstanceOf[SingleBoolean[RowProduct SelectFrom T]])

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	                 (first :Table[M])
	                 (implicit cast :InferTypeParams[Table[M], Table[T], Table[MappingOf[S]#TypedProjection]])
			:Self SelectFrom T =
		SelectFrom.empty(self, LastTable[T, S](cast(first)), None)(True)
//		throw new UnsupportedOperationException(s"($this).from($first)")

	override def from[M[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S, A <: Label]
	                 (first :StaticTable[A, M])
	                 (implicit cast :InferTypeParams[StaticTable[A, M], StaticTable[A, T], Table[MappingOf[S]#TypedProjection]])
			:Self SelectFrom T As A =
		SelectFrom.empty(self, LastTable[T, S](cast(first)), Some(first.name))(True)
//		throw new UnsupportedOperationException(s"($this).from($first)")

	override type FromSubselect[+S <: NonEmptyRow] = Nothing

	//these could probably be made to work, but I'm lazy and this class shouldn't be used for much else than a select call
	/** Throws `UnsupportedOperationException`. */
	override def from[S <: NonEmptyRow with GroundRow](subselect :S) :Nothing =
		throw new UnsupportedOperationException(s"($this).from($subselect)")

	/** Throws `UnsupportedOperationException`. */
	override def fromSubselect[E <: NonEmptyRow]
	                          (subselect :E)(implicit expansion :subselect.Implicit ExpandedBy Generalized) :Nothing =
		throw new UnsupportedOperationException(s"($this).fromSubselect($subselect)")



	protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.aggregated(this)


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int = spelling.sqlParamCount(clause)
//	override def spell(context :SQLContext)(implicit spelling :SQLSpelling) :SpelledSQL[Params, Generalized] =
//		spelling(this)(context)

	override def spell[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                     (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling(this)(context, params)

	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val res = clause.spell(context, params.ungroup[clause.Generalized])
		SpelledSQL(res.sql, res.setter, res.context.aggregate)
	}

	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] = clause.spellingContext.aggregate

	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P] =
		if (context.groupings != 0 || context.parent.isEmpty)
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the " + position + "-th last relation in " +
				(typeString :String) + ": context " + context + " is not for an aggregated clause (parameterization: " +
				params + ")."
			)
		else
			groupingSpellingContext(outer)(position, context.outer, params.outer)



	override def chunkedString :ChunkedString = clause.chunkedString + " aggregated"

	override def typeString(res :StringBuilder) :StringBuilder = clause.typeString(res) ++= " aggregated"


	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause(seal :Seal) :Unit = ()
}






object Aggregated {

	def apply[F <: FromSome](discrete :F) :Aggregated[F] = apply(WithClause.empty, discrete)

	def apply[F <: FromSome](withClause :WithClause, discrete :F) :Aggregated[F] = {
		val ctes = withClause ++ discrete.withClause
		new Aggregated[discrete.type] {
			override val clause :discrete.type = discrete
			override val withClause = ctes
			override val fromClause = discrete.self
			override val outer = clause.outer
			override val parameterization = clause.parameterization.aggregate(self)
		}
	}

	def unapply[F <: FromSome](from :Aggregated[F]) :Opt[F] = Got(from.clause)

	def unapply[F <: RowProduct](from :DecoratedRow[F]) :Opt[F] = from match {
		case _ :Aggregated[_] => Got(from.clause)
		case _ => Lack
	}

	def unapply(from :RowProduct) :Opt[FromSome] = from match {
		case aggro :Aggregated.__ => Got(aggro.clause)
		case _ => Lack
	}


	type __ = Aggregated[_ <: FromSome]
}



