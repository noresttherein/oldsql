
package net.noresttherein.oldsql.sql


/*
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Relation.{DerivedTable, Table}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.AndWith.RecursiveAndWithFactory
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, NonSubselect}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyFrom, NonEmptyFromTemplate, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.WithClause.CommonTableExpression
import net.noresttherein.oldsql.sql.ast.RelationSQL
import net.noresttherein.oldsql.sql.ast.QuerySQL
import net.noresttherein.oldsql.sql.ast.SQLLiteral.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SQLScribe}







//todo: we need to define if TopSelect/GroundSelect[Query] are parameterized with RowProduct, or any WithClause.
//  - RowProduct is cleaner, more intuitive, and makes union and friends easy
//  - WithClause allows the separator between WithClause and FromClause to be a Subselect-like type,
//    which makes it behave nicely with GroupBy. We might though make a special case when *creating* selects
//    and gobble up the WithClause.
//  - Alternatively, we might get rid of WithClause and allow With/AndWith in any place in FromClause
//    This Alexander's solution, apart from being somewhat messy, causes a problem with the dual state
//    of empty/non empty of With (it must be non-empty at the very least to be used with As, and empty not to be used
//    with Subselect). Seams solvable, though.

//todo: With/AndWith for MappingQuerySQL. Currently conflicts with erasure, but we likely will make it
//  the single Query class and QuerySQL only a type alias, especially with SQLExpression refactor for Scala 3


trait WithClause extends NonEmptyFrom with NonEmptyFromTemplate[WithClause, WithClause] { thisClause =>
	override type Last[O <: RowProduct] = JoinedRelation[O, LastMapping]
	override type FromLast >: Generalized <: WithClause


	override type Generalized >: Dealiased <: WithClause {
//			type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized <: thisClause.Generalized //all these below must be <: because of From
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type Base <: thisClause.Base
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Dealiased >: Self <: WithClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: WithClause {
		type LastMapping[O] = thisClause.LastMapping[O]
		type FromLast = thisClause.FromLast
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Base = thisClause.Base
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}


	override type AppliedParam <: BoundParamless
	override type Paramless <: BoundParamless
	override type BoundParamless = WithClause { type Params = @~ }

	override def filter :GlobalBoolean[Generalized] = True
	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :GlobalBoolean[E] = True

	override def filtered[P >: GlobalScope <: GlobalScope](filter :SQLBoolean[Generalized, P]) :Copy =
		throw new UnsupportedOperationException(s"$this.where")

	override type JoinFilter = Nothing

	override def filtered(condition :Nothing) :Copy =
		throw new UnsupportedOperationException(s"$this.on: this call should have been impossible.")

	override type FromNext[E[+W <: RowProduct] <: RowProduct] = Nothing

	override type FilterNext[E[+L <: FromSome] <: L Expanded N, S <: RowProduct Expanded N, G <: S, N[O] <: MappingAt[O]] =
		Nothing

	override def filterNext[F <: AndFrom[RowProduct, N], N[O] <: MappingAt[O]](next :F)(filter :Nothing) :Nothing =
		throw new UnsupportedOperationException(s"$this.filterNext: this call should have been impossible.")



	override def fromClause :Nothing = throw new UnsupportedOperationException(s"$this.fromClause")

	protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Option[Y] = None //todo:


	private[sql] override def concrete_RowProduct_subclass_must_extend_FromClause_or_GroupByClause :Nothing =
		throw new UnsupportedOperationException

}






object WithClause {

	implicit class WithClauseExtension[W <: WithClause](private val thisClause :W) extends AnyVal {
		def andWith[A <: Label, V](alias :A, cte :QuerySQL[RowProduct, V]) :W AndWith MappingOf[V]#TypedProjection As A =
			AndWith(thisClause, alias, cte)

		def recursive[A <: Label](alias :A) :RecursiveAndWithFactory[W, A] =
			new RecursiveAndWithFactory[W, A](thisClause, alias)
	}



	trait CommonTableExpression[+M[O] <: MappingAt[O]] extends DerivedTable[M] {
		protected type Row[O] <: M[O] //= select.ResultMapping[O]
		type Subject
		type Name <: Label
		type CTEs <: WithClause //NextWith Row As Name

		def ctes :CTEs
		def alias :Name
		val select :QuerySQL[RowProduct, Subject]// { type ResultMapping[O] <: M[O] }

		override def apply[O] :M[O] = relation.row[O]
		override def export[O] :MappingAt[O] = relation.export[O]

		override def sql :String = alias + " as " + relation.sql

		protected def relation :Table[Row] //= select
	}

	def CommonTableExpression[A <: Label, V]
	                         (name :A)(cte :(With[MappingOf[V]#TypedProjection] As A) => QuerySQL[RowProduct, V])
			:CommonTableExpression[MappingOf[V]#TypedProjection]
				{ type Name = A; type Subject = V; type CTEs = With[MappingOf[V]#TypedProjection] As A } =
		new CommonTableExpression[MappingOf[V]#TypedProjection] {
			override type Row[O] = BaseMapping[V, O]
			override type Subject = V
			override type Name = A
			override type CTEs = With[MappingOf[V]#TypedProjection] As A
			override val alias = name
			override val ctes :CTEs = With[A, MappingOf[V]#TypedProjection, V](this)
			override val select = cte(ctes)
			override val relation = select
			//todo: `this` leaks from the constructor, is not really thread safe; lets just mention it in the docs
		}

	def CommonTableExpression[L <: WithClause, A <: Label, V]
	                         (left :L, name :A)(cte :(L AndWith MappingOf[V]#TypedProjection As A) => QuerySQL[RowProduct, V])
			:CommonTableExpression[MappingOf[V]#TypedProjection]
				{ type Name = A; type Subject = V; type CTEs = L AndWith MappingOf[V]#TypedProjection As A } =
		new CommonTableExpression[MappingOf[V]#TypedProjection] {
			override type Row[O] = BaseMapping[V, O]
			override type Subject = V
			override type Name = A
			override type CTEs = L AndWith MappingOf[V]#TypedProjection As A
			override val alias = name
			override val ctes :CTEs = AndWith(left, this)
			override val select = cte(ctes)
			override val relation = select
			//todo: `this` leaks from the constructor, is not really thread safe; lets just mention it in the docs
		}

}






trait NextWith[+L <: RowProduct, R[O] <: MappingAt[O]] 
	extends NonSubselect[L, R] with WithClause with NonEmptyFromTemplate[L NextWith R, L NextWith R] 
{ thisClause =>

	override type LastMapping[O] = R[O]
	override type FromLast = RowProduct NextWith R

	override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :Last[E] = last.asIn[E]

	override val right :CommonTableExpression[R] = null
	override def condition :GlobalBoolean[Generalized] = True

	override type Alias = right.Name
	override def alias :Alias = aliasOpt.get

	override type Generalized >: Dealiased <: (left.Generalized NextWith R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit //for Dual it's either lack of this, or Generalized/FromLast = RowProduct
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Dealiased >: Self <: (left.Self NextWith R) {
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
	}

	override type Self <: (left.Self NextWith R) {
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
	}

	//types used by As
	type LeftBound >: WithClause <: RowProduct
	type DealiasedLeft[+W <: LeftBound] <: W NextWith R
	type WithLeft[+W <: LeftBound] = DealiasedLeft[W] As Alias


	//todo:
	override type JoinedWith[+P <: RowProduct, +J[+W <: P, M[O] <: MappingAt[O]] <: W AndFrom M] = Nothing
	override type JoinedWithSubselect[+P <: NonEmptyFrom] = Nothing
	override type AsSubselectOf[+F <: NonEmptyFrom] = Nothing

	override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :Nothing =
		throw new UnsupportedOperationException(s"$this.joinedWith($prefix, ${firstJoin.name})")

	override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :Nothing =
		throw new UnsupportedOperationException(s"$this.joinedWithSubselect($prefix)")

	override def appendedTo[P <: FromClause](prefix :P) :Nothing =
		throw new UnsupportedOperationException(s"$this.appendedTo($prefix)")

	override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit expansion :Implicit ExpandedBy F) :Nothing =
		throw new UnsupportedOperationException(s"$this.asSubselectOf($newOuter)")



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[NextWith.*]
}






object NextWith {
	type * = NextWith[RowProduct, M] forSome { type M[O] <: MappingAt[O] }
}






trait With[M[O] <: MappingAt[O]] extends NextWith[Dual, M] with NonEmptyFromTemplate[With[M], With[M]] { thisClause =>

	override type Generalized = RowProduct NextWith M
	override type Dealiased = With[M]
	override type DealiasedCopy = With[M]
	override type Self <: Dealiased
	override type LeftBound = RowProduct
	override type DealiasedLeft[+W <: RowProduct] = W NextWith M
	override type WithLeft[+W <: RowProduct] = W NextWith M As Alias


	override type LastParam = Nothing
	override type Params = @~
	override type AppliedParam = Nothing
	override type Paramless = Self
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :Nothing) :Nothing = left.bind(param)
	override def bind(params :Params) :Self = self

	protected override def decoratedBind[D <: BoundParamless](params: @~)(decorate :Self => D) :D =
		decorate(self)

	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		ChainTuple.EmptyChain ~ last.expand(target)


	override type DefineBase[+I <: RowProduct] = I
	override type Explicit = RowProduct NextWith M
	override type Inner = RowProduct NextWith M

	override def base :RowProduct = Dual

	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:ChainTuple[E, GlobalScope, Row] =
		ChainTuple.EmptyChain ~ last.expand(target)


	override def name = "with"

	override def toString :String = "with " + right //already incorporates alias into the string

}






object With { 

	def apply[A <: Label, V](alias :A, cte :QuerySQL[RowProduct, V]) :With[MappingOf[V]#TypedProjection] As A =
		CommonTableExpression[A, V](alias)(_ => cte).ctes
	
//	def apply[A <: Label, M[O] <: MappingAt](alias :A, cte :MappingQuerySQL[RowProduct, M]) :With[M] As A =
//		CommonTableExpression[A, M](alias, _ => cte).ctes

	def recursive[A <: Label](alias :A) :RecursiveWithFactory[A] =
		new RecursiveWithFactory[A](alias)

	class RecursiveWithFactory[A <: Label] private[With] (private val alias :Label) extends AnyVal {
		def as[V](cte :(With[MappingOf[V]#TypedProjection] As A) => QuerySQL[RowProduct, V])
				:With[MappingOf[V]#TypedProjection] As A =
			CommonTableExpression(alias.asInstanceOf[A])(cte).ctes
	}


	private[sql] def apply[A <: Label, M[O] <: BaseMapping[S, O], S]
	                      (cte :CommonTableExpression[M] { type Name = A; type CTEs = With[Row] As A }) :With[M] As A =
		new With[M] with AbstractExpanded[Dual, M, S] {
			override val left :Dual.type = Dual
			override val right = cte
			override val last = RelationSQL[Generalized, M, S, FromLast](cte, 0)
			override val aliasOpt = Some(cte.alias)
			override val outer = left.outer
			override def lastRelation = last

			override def narrow :Dual.type NextWith M = this.asInstanceOf[Dual.type NextWith M]

			override type Alias = cte.Name
			override type WithLeft[+L <: RowProduct] = L NextWith M As A
			override type Self = With[M] As A
			override type Copy = With[M] As A

			override def aliased[N <: Label](alias :N) :With[M] As N = {
				CommonTableExpression[N, right.Subject](alias) { withClause =>
					val replacement = withClause.last.asInstanceOf[RelationSQL[withClause.Generalized, M, S, withClause.FromLast]]
					val substitute = SQLScribe.replaceRelation[M, S, M, S, Generalized, withClause.Generalized](
						generalized, withClause.generalized, last, replacement
					)
					substitute(right.select) match {
						case query :QuerySQL[RowProduct, S @unchecked] if query.toTable.row == right.row => query
						case other => throw new IllegalStateException(
							s"The result of renaming last view of $this to $alias transformed query ${right.select} " +
							s"into a non-query expression $other (or a query with a different mapping)."
						)
					}
				}
				??? //fixme: the above expression doesn't have mapping type M
			}

		}.asInstanceOf[With[M] As A]

}






trait AndWith[+L <: WithClause, R[O] <: MappingAt[O]]
	extends NextWith[L, R] with NonEmptyFromTemplate[L AndWith R, L AndWith R]
{ thisClause =>
	override type Generalized = left.Generalized AndWith R
	override type Dealiased = left.Self AndWith R
	override type Self <: Dealiased
	override type LeftBound = WithClause
	override type DealiasedLeft[+W <: WithClause] = W AndWith R
	override type WithLeft[+W <: WithClause] = W AndWith R As Alias

	protected override def narrow :left.type AndWith R

	override type LastParam = left.LastParam
	override type Params = left.Params
	override type AppliedParam = WithLeft[left.AppliedParam]
	override type Paramless = WithLeft[left.Paramless]
	override type DecoratedParamless[D <: BoundParamless] = D


	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		CommonTableExpression[left.AppliedParam, Alias, right.Subject](l, alias) { withClause =>
			val substitute = SQLScribe.applyParam(self, withClause :RowProduct, param, lastParamOffset)
			substitute(right.select) match {
				case query :QuerySQL[RowProduct, right.Subject] if query.toTable.row == right.row => query
				case other => throw new IllegalStateException(
					s"The result of binding last parameter of $this to $param transformed query ${right.select} " +
					s"into a non-query expression $other (or a query with a different mapping)."
				)
			}
		}.ctes
		??? //fixme: above expression does not have AndWith R type
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		CommonTableExpression[left.Paramless, Alias, right.Subject](l, alias) { withClause =>
			val substitute = SQLScribe.applyParams(self, withClause :RowProduct)(params)
			substitute(right.select) match {
				case query :QuerySQL[RowProduct, right.Subject] if query.toTable.row == right.row => query
				case other => throw new IllegalStateException(
					s"The result of binding parameters of $this to $params transformed query ${right.select} " +
						s"into a non-query expression $other (or a query with a different mapping)."
				)
			}
		}.ctes
		??? //fixme: above expression does not have AndWith R type
	}

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))


	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)



	override type DefineBase[+I <: RowProduct] = I
	override type Explicit = left.Explicit AndWith R
	override type Inner = left.Inner AndWith R

	override type Row = left.Row ~ last.Subject

	override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
			:ChainTuple[E, GlobalScope, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)

	override def name = "with"
	override def toString :String = left + " with " + right

}






object AndWith {
	def apply[L <: WithClause, A <: Label, V]
	         (left :L, alias :A, cte :QuerySQL[RowProduct, V])
			:L AndWith MappingOf[V]#TypedProjection As A =
		CommonTableExpression[L, A, V](left, alias)(ctes => cte.basedOn(ctes)).ctes



	private[sql] def apply[L <: WithClause, A <: Label, R[O] <: MappingAt[O]]
	                      (left :L, right :CommonTableExpression[R] { type Name = A; type CTEs = L AndWith Row As A })
			:L AndWith R As A =
		???



	class RecursiveAndWithFactory[W <: WithClause, A <: Label](preceding :W, alias :A) {
		def as[V](select :(W AndWith MappingOf[V]#TypedProjection As A) => QuerySQL[RowProduct, V])
				:W AndWith MappingOf[V]#TypedProjection As A =
			CommonTableExpression[W, A, V](preceding, alias)(select).ctes
	}

}

//todo: SubselectWith and WithParam

*/
