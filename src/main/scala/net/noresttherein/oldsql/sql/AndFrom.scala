package net.noresttherein.oldsql.sql

import scala.annotation.showAsInfix

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.morsels.{ChunkedString, Lazy}
import net.noresttherein.oldsql.schema.{Relation, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.FromSome.FromSomeTemplate
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyRow, NonEmptyRowTemplate, PartOf, PrefixOf, RowComposition, RowDecomposition}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling.GroupingSpellingContext
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ChainTuple, JoinedParam, JoinedRelation, JoinedTable, RelationSQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** A join between an existing [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], representing a relation
  * or a joined list of relations, and another mapping. It is the root of a hierarchy of classes representing
  * ungrouped ''from'' clauses (i.e., without a ''group by'' clause). Subclasses exist for various SQL join kinds
  * (inner, outer, left, right), clauses for subselects nested under other clauses, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type. Note however that for most functions
  * to work properly, more specific type than `AndFrom` may be needed, typically
  * the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] form of this clause.
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN <: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN <: AndFrom`.
  *
  * @tparam L the left side of this join: a `RowProduct` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause.
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Join]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
//Consider: this is now really used only in FromLast, can have a less fancy name,
// and maybe use AndFrom for NonSubselect with FromClause. Alternatively, we can use it for SelectFrom.
// We also always have PseudoJoin, although it is quite similar to JoinLike
@showAsInfix
trait AndFrom[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Expanded[L, R] with FromSome with AndFromTemplate[L, R, L AndFrom R]
{ thisClause =>

	override val left :L //overrides independent definitions from Expanded and AndFromTemplate

	override type Last[O <: RowProduct] <: JoinedRelation[O, R] { type FromLast = RowProduct AndFrom R }
	override type FromLast = RowProduct AndFrom R

	override type Generalized >: Complete <: (left.Generalized AndFrom R) {
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete AndFrom R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row      = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type NoAlias >: Self <: (left.Self AndFrom R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row      = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self AndFrom R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row      = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}



	override def narrow :left.type AndFrom R



	/** A proof that the generalized form of this type expands its left side.
	  * Used as evidence required by some implicits.
	  */
	def generalizedExpansion[P <: LeftBound] :P PrefixOf GeneralizedLeft[P]

	/** A proof that this type expands its left side. Used as evidence required by some implicits. */
	def expansion[P <: LeftBound] :P PrefixOf WithLeft[P]



	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.filter]]
	  */
	override def condition :SingleBoolean[Generalized]

	private[this] lazy val cachedFilter = filter(generalized)

	override def filter :SingleBoolean[Generalized] = cachedFilter


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.sqlParamCount(left) + spelling.sqlParamCount(right) + spelling.inWhere.sqlParamCount(condition)

	protected override def groupingSpellingContext[P]
	                       (position :Int, context :SQLContext[P], params :Parameterization[P, Generalized])
			:GroupingSpellingContext[P] =
		if (position == 0)
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the last relation in an ungrouped clause " +
				(typeString :String) + " (using " + context + "and " + params + ")."
			)
		else if (context.isAggregated)
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the " + position + "-th last relation in " +
				(typeString :String) + "because the context is for an aggregate clause: " + context +
				" (parameterization: " + params + ")."
			)
		else if (position < size) //this select
			groupingSpellingContext(left)(position - 1, context.shrink(), params.left)
		else if (context.isSubselect) //leap to outer
			groupingSpellingContext(outer)(position - size, context.outer, params.outer[Implicit])
		else
			throw new IllegalArgumentException(
				"Cannot return a GroupingSpellingContext for the " + position + "-th last relation in " +
				(typeString :String) + " because no context for the outer select is present in " + context +
				" (parameterization: " + params + ")."
			)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AndFrom.__]

}






/** A matching pattern and factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables
  * joined together.
  */
object AndFrom {

	/** Create a ''from'' clause expanding the `left` clause with the relation `right` for mapping `R`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method.
	  * This method will create a [[net.noresttherein.oldsql.sql.From From]]`[R]` instance if `left` is empty (`Dual`),
	  * or an [[net.noresttherein.oldsql.sql.InnerJoin L InnerJoin R]] otherwise. This method's use is somewhat limited
	  * as the result type of `L AndFrom R` is too abstract (its `Generalized` form is undefined) for many purposes.
	  * Prefer using the factory methods of `RowProduct` and, in particular, its extension methods from
	  * [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]], such as `join`, `leftJoin`, etc.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast implicit witness providing type inference of the subject type of the mapping `R` (and `T`).
	  */
//	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
//	         (left :L, right :Table[R], filter :SingleBoolean[L#Generalized NonParam R] = True)
//	         (implicit cast :BaseMappingSubject[R, T, S]) :L NonParam R =
//		cast.back.join(left.expand(LastTable[T, S](cast(right)), None, cast.column(filter)))


	/** Splits any `AndFrom` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](join :L Adjoin R) :Opt[(L, Relation[R])] = join match {
		case join :AndFrom[L @unchecked, R @unchecked] => Got((join.left, join.right))
		case _ => Lack
	}

	/** Matches all `AndFrom` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(RowProduct, Relation.__)] = from match {
		case join :AndFrom.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def andFromDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L AndFrom R, L, R, AndFrom, RowProduct] =
		decomposition.asInstanceOf[ExpandedDecomposition[L AndFrom R, L, R, AndFrom, RowProduct]]

	private[this] val decomposition =
		new ExpandedDecomposition[RowProduct AndFrom MappingAt, RowProduct, MappingAt, AndFrom, RowProduct]



	/** A factory interface of and for the [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] class/instances,
	  * with a covariant upper bound on produced objects. It implements the filtering methods
	  * from [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate RowProductTemplate]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]] by delegating to
	  * [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.withCondition withCondition]].
	  * For simplicity, and because public interfaces do not implement `NonEmptyRowTemplate` with the narrowed
	  * 'self' type to the [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause, it uses a single self type.
	  * Final implementation classes for `As` narrow down the result type by extending again `NonEmptyTemplate`
	  * providing the narrowed type as the 'lower' upper bound. All new methods declared here specify their return type
	  * in terms of member types of this instance rather than `L`, `R` and `U`, so instances conforming to `U As A`
	  * suffer no limitations to their functionality.
	  */
	trait AndFromTemplate[+L <: RowProduct, R[O] <: MappingAt[O], +U <: (L AndFrom R) with AndFromTemplate[L, R, U]]
		extends FromSomeTemplate[U]
	{ thisClause :U with AndFromTemplate[L, R, U] => //self type is the only reason why everything can't be in AndFrom

		val left :L
		//pseudo override of the declaration from the self type to widen the visibility to public
		def condition :SingleBoolean[Generalized]

		/** The upper bound on the left side of [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.WithLeft WithLeft]]. */
		type LeftBound >: FromSome <: RowProduct //this is required by As

		/** The `Generalized` type with the left side substituted for `F`. */
		type GeneralizedLeft[+F <: LeftBound] <: F AndFrom R

		/** The same type as `this.WithLeft[F]`, but without a [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause
		  * for the last relation. It is the `NoAlias` type of this instance with the left side of the last join
		  * substituted for `F`.
		  */
		type NoAliasLeft[+F <: LeftBound] <: GeneralizedLeft[F]

		/** This `Self` type with the left side substituted for `F`. */
		type WithLeft[+F <: LeftBound] <: NoAliasLeft[F] //todo: rename to SwapLeft/JoinLeft/Left/Expansion/LastJoin/JoinLast

		/** A join of the same kind as this clause, but with the left clause substituted for `left`.
		  * This is a lower-level 'virtual constructor' method and application code should prefer using the API
		  * provided by [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate NonEmptyRowTemplate]]
		  * (and [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate RowProductTemplate]]).
		  * @param left   the exact instance used as [[net.noresttherein.oldsql.sql.AndFrom.left left]] property
		  *               of the result.
		  * @param filter the exact boolean expression to use as the filter condition of the result.
		  *               It will not be anchored or altered in any way by this method.
		  */ //we could make it F <: LeftBound, but we'd need to narrow it down without making it volatile
		def withLeft[F <: FromSome](left :F)(filter :SingleBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]


		/** A copy of this clause with [[net.noresttherein.oldsql.sql.AndFrom.condition condition]] property
		  * replaced with the given filter expression. This does not replace the whole ''where'' clause,
		  * as the conditions (if present) of the left clause remain unchanged. It is the target of
		  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] and other filtering methods
		  * (which add to the condition, rather then completely replace it).
		  * @param filter a Boolean expression to include as-is in the returned instance.
		  */
		def withCondition(filter :SingleBoolean[Generalized]) :Copy

		override def filtered[P >: Single <: Single](filter :SQLBoolean[Generalized, P]) :Copy =
			if (filter == True) selfAsCopy
			else withCondition(condition && filter)


		override type JoinFilter = left.FilterNext[GeneralizedLeft, FromLast, Generalized, LastMapping]

		override def filtered(condition :left.FilterNext[GeneralizedLeft, FromLast, Generalized, LastMapping]) :Copy =
			left.`->filterNext`[U, LastMapping](this)(condition)

		override def on(condition :JoinFilter) :U = filtered(condition)

	}



	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = AndFrom[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** [[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]] type
	  * of all [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] with mapping `M` on their right side.
	  */
	type AndFromLast[M[O] <: MappingAt[O]] = RowProduct AndFrom M

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] instances joining any clause
	  * with relation of mapping `M`. It is [[net.noresttherein.oldsql.sql.AndFrom.FromLast FromLast]] type of
	  * all [[net.noresttherein.oldsql.sql.FromSome FromSome]] implementations.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct AndFrom M

	/** A curried type constructor for `AndFrom` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L AndFrom R }

	/** A curried type constructor for `AndFrom` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L AndFrom R }

}






//consider: moving to AndFrom; rename to NonParamJoin
@showAsInfix
trait NonParam[+L <: RowProduct, R[O] <: MappingAt[O]] extends AndFrom[L, R] with AndFromTemplate[L, R, L NonParam R] {
	thisClause =>

	override type Last[O <: RowProduct] = JoinedTable[O, R]

	override val last :JoinedTable[RowProduct AndFrom R, R]
	override def right :Table[R] = last.table
	def table :Table[R] = last.table

	override def lastAsIn[E <: RowProduct](implicit expansion :FromLast PrefixOf E) :JoinedTable[E, R] =
		last.asIn[E]


	override type Generalized >: Complete <: (left.Generalized NonParam R) {
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete NonParam R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type NoAlias >: Self <: (left.Self NonParam R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self NonParam R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}
	override type GeneralizedLeft[+F <: LeftBound] <: F NonParam R

	override def narrow :left.type NonParam R


	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, FullRow] =
		left.fullRow(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)


	override type LastParam  = left.LastParam
	override type Params     = left.Params
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))

}






object NonParam {

	implicit def nonParamDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L NonParam R, L, R, NonParam, RowProduct] =
		decomposition.asInstanceOf[ExpandedDecomposition[L NonParam R, L, R, NonParam, RowProduct]]

	private[this] val decomposition =
		new ExpandedDecomposition[RowProduct NonParam MappingAt, RowProduct, MappingAt, NonParam, RowProduct]



	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = NonParam[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.NonParam NonParam]] instances joining any clause
	  * with relation of mapping `M`.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct NonParam M

	/** A curried type constructor for `AndFrom` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L NonParam R }

	/** A curried type constructor for `AndFrom` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L NonParam R }

}






/** Supertype of ''from'' clauses with size 1, that is [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] links
  * which contain the first table of the most nested clause.
  * It has two main subtypes: [[net.noresttherein.oldsql.sql.From From]] for ''ground'' clauses,
  * and [[net.noresttherein.oldsql.sql.Subselect Subselect]] for clauses starting a dependent ''select'' of their
  * left side.
  */
//I am not sure this has a lot of purpose; its Generalized type is unknown, for Subselect it's <:< SelectFrom,
// but for From >: SelectFrom, so it may be hard to do any generic stuff.
//todo: use this in GetTable
@showAsInfix
trait SelectFrom[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonParam[L, R] with AndFromTemplate[L, R, L SelectFrom R]
{ thisClause =>
	override val left :L //we could try to use it in AndFromTemplate
	override type Complete >: NoAlias <: OuterLeft[left.Complete with LeftBound] {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Inner       = thisClause.Inner
	}
	override type NoAlias >: Self <: OuterLeft[left.Self with LeftBound] {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Inner       = thisClause.Inner
	}
	override type Self <: OuterLeft[left.Self with LeftBound] {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type Inner       = thisClause.Inner
	}
	type OuterLeft[+F <: LeftBound] >: (F with NonEmptyRow) Subselect R <: F SelectFrom R
	override type LeftBound >: NonEmptyRow <: RowProduct
	override type Explicit  = RowProduct AndFrom R
	override type Implicit  = left.Generalized //problem: not preserved by Generalized=left.Generalized NonParam R
	override type Outer     = left.Self
	override type Inner     = OuterLeft[LeftBound]
	override type DefineBase[+I <: RowProduct] = I
	override type Base      = left.Generalized
	override def base :Base = outer

	override def size = 1
	override def isExplicitParameterized = false

	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, @~ ~ last.Subject] =
		ChainTuple() ~ last.expand(target)(expansion, implicitly[Single <:< Single])


	override type OuterRow = left.FullRow

	override def outerRow[E <: RowProduct]
	                     (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, Single, left.FullRow] =
		left.fullRow(target)
	//in Scala 3, this should be replaced with F Subselect R, but it needs either a union lower bound or a volatile product upper bound
	override type AsSubselectOf[+F <: NonEmptyRow] <: F Subselect R //fixme: add the As clause in As

	override def withClause :WithClause = table.withClause ++ condition.outerWithClause


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val aliased = spelling.table(table, aliasOpt)(generalized, context.subselect, params)
		val sql = (spelling.FROM + " ") +: aliased
		if (filter == True) sql else sql && (spelling.inWhere(filter)(self, _, params))
	}
	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] = {
		val ctx = left.spellingContext
		ctx.subselect(spelling.alias(right, aliasOpt)(generalized, ctx))
	}

}



object SelectFrom {

	/** Create a ''from'' clause of a dependent ''select'' using `left`as its
	  * [[net.noresttherein.oldsql.sql.RowProduct.outer outer]] clause, with table `right` for mapping `R`
	  * as the sole relation in the actual ''from'' clause. The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method.
	  *
	  * The result will be a [[net.noresttherein.oldsql.sql.From From]]`[R]` instance if `left` is empty (`Dual`),
	  * or an [[net.noresttherein.oldsql.sql.Subselect L Subselect R]] otherwise, or a special implementation
	  * in the corner case of a nominally empty clause whose type should nevertheless be preserved, as
	  * with [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] (a ''from'' clause of an SQL ''select'' without
	  * a ''group by'' clause, but using aggregate expressions in its ''select'' clause).
	  * @param left   a ''from'' clause of an outer ''select'' containing the list of implicitly available relations.
	  * @param right  the first table of the actual ''from'' clause of the created instance.
	  * @param filter an optional join condition narrowing the cross join, used in the ''where'' clause
	  *               of the generated SQL.
	  * @param cast   implicit witness providing type inference of the subject type of the mapping `R` (and `T`).
	  */
	def apply[L <: RowProduct, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[L#Generalized NonParam R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L SelectFrom R =
		cast.back.join(left.from(LastTable[T, S](cast(right)), None, cast.column(filter)))

	//todo: make this public
//	private[sql] def apply[L <: OuterRow, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
//	                      (left :L, right :Table[R], filter :GlobalBoolean[RowProduct SelectFrom R])
//	                      (implicit cast :BaseMappingSubject[R, T, S]) :L SelectFrom R =
//		cast.back.join(empty(left, LastTable[T, S](cast(right)), None)(cast.column(filter)))

	/** Refinement of `RowProduct` defining clause types which can occur as the left (outer) clauses
	  * of a [[net.noresttherein.oldsql.sql.SelectFrom SelectFrom]] instances other than
	  * [[net.noresttherein.oldsql.sql.From From]] and [[net.noresttherein.oldsql.sql.Subselect Subselect]].
	  */
	private[sql] type OuterRow = RowProduct {
		type SingletonFromTable[T[O] <: MappingAt[O]] <: ThisType GenericSelectFrom T
		type GeneralizedFromTable[T[O] <: MappingAt[O]] <: Generalized GenericSelectFrom T
		type AppliedParam <: RowProduct {
			type GeneralizedFromTable[T[O] <: MappingAt[O]] <: Generalized GenericSelectFrom T
		}
		type Paramless <: RowProduct {
			type Params = @~; type GeneralizedFromTable[T[O] <: MappingAt[O]] <: Generalized GenericSelectFrom T
		}
	}

	/** A special `SelectFrom` implementation used only by [[net.noresttherein.oldsql.sql.Aggregated Aggregated]],
	  * which cannot be a left side of [[net.noresttherein.oldsql.sql.Subselect Subselect]] due to being empty,
	  * but which cannot really be discarded.
	  */
	private[sql] def empty[T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (clause :OuterRow, relation :LastTable[T, S], asOpt :Option[A])
	                      (cond :SingleBoolean[RowProduct SelectFrom T]) :clause.type GenericSelectFrom T As A =
		new GenericSelectFrom[clause.type, T] with AbstractExpanded[clause.type, T, S] {
			override val left :clause.type   = clause
			override val last                = relation
			override val aliasOpt            = asOpt
			override val condition           = cond
			override val outer               = left.self
			override val parameterization    = left.parameterization.subselect(self)
			override def lastRelation        = last
			override lazy val tableStack     = tableStack(generalized).toIndexedSeq
			override lazy val fullTableStack = fullTableStack(generalized).toIndexedSeq
			override lazy val withClause     = super.withClause

			override type Alias = A

			override def narrow :left.type SelectFrom T = this

			override def withCondition(filter :SingleBoolean[Generalized]) =
				//this trick depends on the fact that the only valid/used left sides are empty clauses
				// with left.Generalized = RowProduct and Aggregated, which guarantees that filter is GlobalBoolean[FromLast]
				left.from[T, S, A](last, aliasOpt, filter)
//				SelectFrom.any(left, last, aliasOpt)(filter)

			override def withLeft[F <: RowProduct](newLeft :F)(filter :SingleBoolean[newLeft.Generalized NonParam T]) =
				newLeft.from(last, aliasOpt, filter)

			override def aliased[N <: Label](alias :N) =
				SelectFrom.empty[T, S, N](left, last, Option(alias))(condition)

			override def bind(param :LastParam) :AppliedParam = left.bind(param).from(last, aliasOpt, condition)
			override def bind(params :Params) :Paramless = left.bind(params).from(last, aliasOpt, condition)

			override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F) =
				Subselect[newOuter.type, T, S, A](newOuter, last, aliasOpt)(condition)

			override def tableStack[E <: RowProduct]
			                       (target :E)(implicit stretch :Generalized ExpandedBy E) :LazyList[RelationSQL.from[E]#__] =
				LazyList.cons(last.expand[Generalized, E](target).upcast, LazyList.empty[RelationSQL.from[E]#__])
//				last.expand[Generalized, E](target) #:: LazyList.empty[RelationSQL.from[E]#__]

			override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.selectFrom[left.type, T, S](this)
		}.asInstanceOf[clause.type GenericSelectFrom T As A]


		private[sql] trait GenericSelectFrom[+L <: RowProduct, R[O] <: MappingAt[O]] extends SelectFrom[L, R] {
			thisClause =>
			override val condition        :SingleBoolean[RowProduct SelectFrom R]
			override def generalizedClass :Class[Generalized] = classOf[left.Generalized GenericSelectFrom R]
			override def selfClass        :Class[Self] = classOf[left.Self GenericSelectFrom R].asInstanceOf[Class[Self]]

			override type LeftBound = RowProduct
			override type GeneralizedLeft[+F <: RowProduct] = F NonParam R
			override type NoAliasLeft[+F <: RowProduct]     = F SelectFrom R
			override type WithLeft[+F <: RowProduct]        = F SelectFrom R As Alias
			override type OuterLeft[+F <: RowProduct]       = F SelectFrom R
			override type Generalized = left.Generalized GenericSelectFrom R
			override type Complete    = left.Complete GenericSelectFrom R
			override type NoAlias     = left.Self GenericSelectFrom R
			override type Self        = left.Self GenericSelectFrom R As Alias
			override type NoAliasCopy = left.type GenericSelectFrom R
			override type Copy        = left.type GenericSelectFrom R As Alias

			override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
				condition.basedOn(target)

			override def expansion[P <: RowProduct] = PrefixOf.itself[P].expand[SelectFrom, R].as[Alias]

			override type AppliedParam = left.AppliedParam SelectFrom R
			override type GeneralizedParamless = left.GeneralizedParamless SelectFrom R
			override type Paramless = left.Paramless SelectFrom R

			override type JoinedWith[+P <: RowProduct, +J[+F <: P, T[O] <: MappingAt[O]] <: F NonParam T] = Nothing
			override type SelectedFrom[+P <: NonEmptyRow] = Nothing //P Subselect R As Alias

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) = appendedTo(prefix)

			override def appendedTo[P <: FromClause](prefix :P) =
				throw new UnsupportedOperationException(
					"Clause " + this + " cannot be joined with another clause (" + prefix + ")."
				)

			override def selectedFrom[F <: NonEmptyRow](prefix :F) =
				throw new UnsupportedOperationException(
					"Clause " + this + " cannot be selected from another clause (" + prefix + ")."
				)


			override type AsSubselectOf[+F <: NonEmptyRow] = F Subselect R As Alias

			override def generalizedExpansion[F <: LeftBound] :F PrefixOf (F NonParam R) =
				PrefixOf.itself[F].expand[NonParam, R]


			override def canEqual(that :Any) :Boolean = that.isInstanceOf[SelectFrom.__]

			override def name :String = "from"

			override def chunkedString :ChunkedString = {
				if (condition == True) left.chunkedString + " from " + last.relation.toString
				else left.chunkedString + " from " + last.relation.toString + " where " + condition.toString
			}

			override def typeString(res :StringBuilder) :StringBuilder =
				left.typeString(res) ++= " from " ++= right.refString

			override def toString :String =
				if (condition == True) left.toString + " from " + last.relation
				else left.toString + " from " + last.relation + " where " + condition

		}



	implicit def selectFromDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L SelectFrom R, L, R, SelectFrom, RowProduct] =
		decomposition.asInstanceOf[ExpandedDecomposition[L SelectFrom R, L, R, SelectFrom, RowProduct]]

	private[this] val decomposition =
		new ExpandedDecomposition[RowProduct SelectFrom MappingAt, RowProduct, MappingAt, SelectFrom, RowProduct]



	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = SelectFrom[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.SelectFrom SelectFrom]] instances joining any clause
	  * with relation of mapping `M`.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct SelectFrom M

	/** A curried type constructor for `SelectFrom`, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L SelectFrom R }

	/** A curried type constructor for `SelectFrom`, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L SelectFrom R }

}






/** A `RowProduct` constituting of exactly one table or SQL relation.
  * This is a specialized subclass of `AndFrom[Dual, T]`, so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  */
sealed trait From[T[O] <: MappingAt[O]]
	extends SelectFrom[Dual, T] with NonSubselect[Dual, T] with AndFromTemplate[Dual, T, From[T]]
{ thisClause =>
	override def generalizedClass :Class[RowProduct NonParam T] = classOf[RowProduct NonParam T]
	override def selfClass        :Class[Self] = classOf[From[T]].asInstanceOf[Class[Self]]

	override type Generalized = RowProduct NonParam T //todo: make it FromLast
	override type Complete    = From[T]
	override type NoAlias     = From[T]
	override type Self       <: From[T]

	override type LeftBound = RowProduct
	override type GeneralizedLeft[+L <: RowProduct] = L NonParam T
	override type NoAliasLeft[+L <: RowProduct]     = L SelectFrom T
	override type WithLeft[+L <: RowProduct]       <: L SelectFrom T
	override type OuterLeft[+F <: RowProduct]       = F SelectFrom T
//	override type GeneralizedRight[R[O] <: MappingAt[O]] = RowProduct NonParam R
//	override type WithRight[R[O] <: MappingAt[O]] = From[R]

	override def withLeft[L <: RowProduct] //wider bound
	                     (newLeft :L)(filter :SingleBoolean[newLeft.Generalized NonParam T]) :WithLeft[L]

//	override def withRight[R[O] <: BaseMapping[S, O], S]
//	                      (table :LastTable[R, S])(filter :GlobalBoolean[GeneralizedRight[R]]) :WithRight[R]


	override def filter :SingleBoolean[RowProduct NonParam T] =
		if (left.filter eq True) condition else left.filter && condition

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
		filter.basedOn(target)


	override type AppliedParam = Nothing
	override type GeneralizedParamless = Generalized
	override type Paramless = Self

	override def bind(param :Nothing) :Nothing = left.bind(param)
	override def bind(params :Params) :Self = self


	override def fullSize = 1

	override def generalizedExpansion[F <: RowProduct] :F PrefixOf (F NonParam T) =
		PrefixOf.itself[F].expand[NonParam, T]

	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] <: P J T
	override type SelectedFrom[+P <: NonEmptyRow] = JoinedWith[P, Subselect] //fixme: add the lost As clause

	//defaultSpelling inherited from SelectFrom will always create a subcontext, even if the passed one is empty.

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.__]

	override def name :String = "from"

	override def chunkedString :ChunkedString =
		if (filter == True) From.chunkedString + last.relation.toString
		else From.chunkedString + last.relation.toString + " where " + filter.toString

	override def typeString(res :StringBuilder) :StringBuilder = res ++= "from " ++= right.refString

	override def toString :String =
		if (filter == True) "from " + last.relation
		else "from " + last.relation + " where " + filter
}






/** A `RowProduct` factory for both single table queries, and starting points for arbitrary join lists.
  * Example (see the `AndFrom` class documentation for explanation of the filters):
  * {{{
  *     val users = From(Users) whereLast (_.name === "Jack")
  *     val guests = From(Hotels) join GuestsBook on (_.id === _.hotelId) join People on (_.guestId === _.id)
  * }}}
  */
object From {

	/** Creates a ''from'' clause consisting of a single relation (table, view, select) with `Mapping` `R`.
	  * It can be later filtered using the [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast]] method, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param table the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`,
	  *              represented here by types `R =:= T`, split in order to separate the inference of the mapping
	  *              type and its subject type and remove the need to specify the type parameter for the method
	  *              explicitly.
	  * @param cast an implicit witness providing the type inference of the subject type of the relation's mapping
	  *             as well as casting functions between associated classes parameterized with `T` and `R`.
	  * @return an unfiltered `From[R]`.
	  */
	def apply[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (table :Table[R])(implicit cast :BaseMappingSubject[R, T, S]) :From[R] =
		cast.back(cast(table).from)

	/** Creates a ''from'' clause consisting of a single table (or view, select) with `Mapping` `R`, using the name
	  * of the table as the alias given in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause.
	  * The alias can be changed if needed as normal,
	  * using [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.as as]] method. The result can be later
	  * filtered using [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast]] methods, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param table the table for the ''from'' clause, parameterized with a `String` literal with the name of the table
	  *              and `BaseMapping` subtype with subject type `S`, represented here by types `R =:= T`,
	  *              split in order to separate the inference of the mapping type and its subject type and remove
	  *              the need to specify the type parameter for the method explicitly.
	  * @param cast an implicit witness providing the type inference of the subject type of the relation's mapping
	  *             as well as casting functions between associated classes parameterized with `T` and `R`.
	  * @return an unfiltered `From[R] As N`.
	  */
	def apply[N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (table :StaticTable[N, R])(implicit cast :BaseMappingSubject[R, T, S]) :From[R] As N =
		cast.back[({ type F[M[O] <: MappingAt[O]] = From[M] As N })#F](
			From.any(Dual, cast(table).toSQL, Some(table.name), True)
		)


	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. This is a lower level factory method accepting an optional, additional filter
	  * for the ''where'' clause, but providing no type inference support and thus generally requiring the type
	  * parameters to be specified explicitly. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param table  the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`.
	  * @param filter an optional boolean condition for the associated ''where'' clause.
	  * @return a `From[R]`.
	  */
	def apply[T[O] <: BaseMapping[S, O], S]
	         (table :Table[T], filter: SingleBoolean[RowProduct NonParam T] = True) :From[T] =
		if (filter == True) table.from
		else From(table.toSQL, None, filter)


	private[oldsql] def apply[T[O] <: BaseMapping[S, O], S, A <: Label]
	                         (relation :LastTable[T, S], alias :Option[A], filter :SingleBoolean[RowProduct NonParam T])
			:From[T] =
		From.any(Dual, relation, alias, filter)

	private[sql] def any[T[O] <: BaseMapping[S, O], S, A <: Label]
	                    (dual :Dual, relation :LastTable[T, S], asOpt :Option[A],
	                     cond :SingleBoolean[RowProduct NonParam T])
			:DualJoin[dual.type, T] As A =
		new DualJoin[dual.type, T] with AbstractExpanded[dual.type, T, S] {
			override val left :dual.type  = dual
			override val last             = relation
			override val aliasOpt         = asOpt
			override val condition        = cond
			override val outer            = left.outer
			override val parameterization = Parameterization.paramless[Dual DualJoin T As A]
			override def lastRelation     = last
			override def tableStack       = Nil //super[EmptyJoin].tableStack.toIndexedSeq
			override def fullTableStack   = tableStack
			override lazy val withClause  = super.withClause

			override def narrow :dual.type SelectFrom T = this

			override type Alias   = A
			override type WithLeft[+L <: RowProduct] = L SelectFrom T As A
			override type Self    = Dual DualJoin T As A
			override type NoAliasCopy = left.type DualJoin T
			override type Copy    = left.type DualJoin T As A

			override def withCondition(filter :SingleBoolean[RowProduct NonParam T]) =
				From.any(left, last, aliasOpt, filter)

			override def withLeft[F <: RowProduct](newLeft :F)(filter :SingleBoolean[newLeft.Generalized NonParam T]) =
				newLeft.from(last, aliasOpt, filter)

//			override def withRight[R[O] <: BaseMapping[X, O], X]
//			                      (table :LastTable[R, X])(filter :GlobalBoolean[RowProduct NonParam R]) :From[R] =
//				From.custom(left, table, None, filter)

			override def aliased[N <: Label](alias :N) =
				any(left, last, Option(alias), condition)

			override def expansion[P <: RowProduct] = PrefixOf.itself[P].expand[SelectFrom, T].as[A]


			override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = P J T As A
			override type SelectedFrom[+P <: NonEmptyRow] = P Subselect T As A

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) =
				firstJoin.aliasedJoin[F, T, S, A](prefix, last, aliasOpt)(filter)

			override def selectedFrom[F <: NonEmptyRow](prefix :F) =
				Subselect[F, T, S, A](prefix, last, aliasOpt)(filter)

			override def appendedTo[P <: FromClause](prefix :P) = prefix.from(last, aliasOpt, filter)


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExpandedBy E) :LazyList[RelationSQL.from[E]#__] =
				LazyList.cons(last.expand[Generalized, E](target).upcast, LazyList.empty[RelationSQL.from[E]#__])
				//this crashes scalac:
//				last.expand[Generalized, E](target).upcast #::[RelationSQL.AnyIn[E]] LazyList.empty[RelationSQL.AnyIn[E]]


			override type AsSubselectOf[+F <: NonEmptyRow] = F Subselect T As A

			override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F) =
				Subselect[newOuter.type, T, S, A](newOuter, last, aliasOpt)(filter)


			override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.from[T, S](this)

		}.asInstanceOf[dual.type DualJoin T As A]



	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply[M[O] <: MappingAt[O]](from :RowProduct Adjoin M) :Opt[Table[M]] = from match {
		case f :From[M @unchecked] => Got(f.right)
		case _ => Lack
	}

	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply(from :RowProduct) :Opt[Table.__] = from match {
		case f :From.__ => Got(f.table)
		case _ => Lack
	}



	implicit def fromComposition[T[O] <: MappingAt[O]] :FromComposition[T] =
		composition.asInstanceOf[FromComposition[T]]

	private[this] val composition = new FromComposition[MappingAt]


	class FromComposition[M[O] <: MappingAt[O]] extends RowComposition[From[M], Dual, Dual] {
		override type E[+D <: Dual] = From[M]
		override type S[+D >: Dual <: Dual] = From[M]

		override def prefix[A >: Dual <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def expansion[A <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def unapply(from :From[M]) :Dual = from.left

		override def apply[D <: Dual](template :From[M], dual :D) :From[M] =
			if (template.left.filter == dual.filter) template //this assumes Dual is sealed and thus it doesn't matter
			else template.withCondition(dual.filter && template.condition) //which instance is used if the filter is right

		override def upcast[A >: Dual <: Dual] :RowDecomposition[From[M], A, Dual] =
			this.asInstanceOf[RowDecomposition[From[M], A, Dual]]

		override def cast[A <: Dual] :RowDecomposition[From[M], A, Dual] =
			this.asInstanceOf[RowDecomposition[From[M], A, Dual]]

	}



	/** Type alias for `From` with the erased type parameter, covering all instances of `From`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = From[M] forSome { type M[O] <: MappingAt[O] }

	/** Type alias for the upper bound of all [[net.noresttherein.oldsql.sql.FromClause FromClause]] subclasses
	  * which join their left side with `M` as the mapping type of the last table.
	  * It is the [[net.noresttherein.oldsql.sql.AndFrom.FromLast FromLast]] type of
	  * [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] (all joins, [[net.noresttherein.oldsql.sql.From From]]
	  * and [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]) and, as such,
	  * it it opens every [[net.noresttherein.oldsql.sql.RowProduct!.Generalized Generalized]] form
	  * of all non-aggregated clauses. In particular, it is also
	  * the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type used for the mapping of a last
	  * table in a ''from'' clause.
	  */
	type Last[M[O] <: MappingAt[O]] = RowProduct AndFrom M

	/** A template `From` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
	final val template :From.__ = From(Table.Dummy)


	private[sql] val chunkedString = ChunkedString("from ")
}



@showAsInfix
private trait DualJoin[+L <: Dual, T[O] <: MappingAt[O]]
	extends From[T] with SelectFrom[L, T] with AndFromTemplate[L, T, L DualJoin T]
