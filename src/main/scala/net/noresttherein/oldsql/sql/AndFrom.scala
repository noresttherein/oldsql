package net.noresttherein.oldsql.sql

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.RowProduct.{As, ClauseComposition, ClauseDecomposition, ClauseGeneralization, ExtendedBy, NonEmptyFrom, NonEmptyFromTemplate, ParamlessFrom, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.MappingSQL.{RelationSQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.mechanics.RowProductMatcher
import net.noresttherein.oldsql.sql.FromClause.FromClauseTemplate
import net.noresttherein.oldsql.sql.ast.MappingSQL.TableSQL.LastTable
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple






/** A join between an existing [[net.noresttherein.oldsql.sql.RowProduct RowProduct]], representing a relation
  * or a joined list of relations, and another mapping. It is the root of a hierarchy of classes representing
  * ungrouped ''from'' clauses (i.e., without a ''group by'' clause). Subclasses exist for various SQL join kinds
  * (inner, outer, left, right), clauses for subselects nested under other clauses, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type. Note however that for most functions
  * to work properly, more specific type than `AndFrom` may be needed, typically
  * the [[net.noresttherein.oldsql.sql.RowProduct.Generalized Generalized]] form of this clause.
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
trait AndFrom[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Extended[L, R] with FromSome with AndFromTemplate[L, R, L AndFrom R]
{ thisClause =>

	override val left :L //overrides independent definitions from Extended and AndFromTemplate

	override type FromLast = RowProduct AndFrom R

	override type Generalized >: Dealiased <: (left.Generalized AndFrom R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Dealiased >: Self <: (left.Self AndFrom R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self AndFrom R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}



	protected override def narrow :left.type AndFrom R



	/** A proof that the generalized form of this type extends its left side.
	  * Used as evidence required by some implicits.
	  */
	def generalizedExtension[P <: FromSome] :P PrefixOf GeneralizedLeft[P]

	/** A proof that this type extends its left side. Used as evidence required by some implicits. */
	def extension[P <: FromSome] :P PrefixOf WithLeft[P]



	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.filter]]
	  */
	override def condition :GlobalBoolean[Generalized]

	private[this] val cachedFilter = Lazy { filter(generalized) }

	override def filter :GlobalBoolean[Generalized] = cachedFilter.get



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AndFrom.*]

}






/** A matching pattern and factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables
  * joined together.
  */
object AndFrom {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.where where]] method.
	  * @param left the first relation of the ''from'' clause.
	  * @param right the second relation of the ''from'' clause.
	  */
	def apply[L[O] <: MappingAt[O], LG[O] <: BaseMapping[A, O], A,
	          R[O] <: MappingAt[O], RG[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :JoinedRelationSubject[From, L, LG, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RG, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)


	/** Create a ''from'' clause extending the `left` clause with the relation `right` for mapping `R`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
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
	def apply[L <: FromClause, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized NonParam R] = True)
	         (implicit cast :InferSubject[L, NonParam, R, T, S]) :L NonParam R =
	{
		val condition = cast.cast[NonParam.WithLeft[L#Generalized]#F, GlobalScope, Boolean](filter)
		cast(left.extend(LastTable[T, S](cast(right)), None, condition))
	}


	/** Splits any `AndFrom` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](join :L Compound R) :Option[(L, Relation[R])] = join match {
		case join :AndFrom[L @unchecked, R @unchecked] => Some((join.left, join.right))
		case _ => None
	}

	/** Matches all `AndFrom` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(RowProduct, Relation.*)] = from match {
		case join :AndFrom.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def andFromDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L AndFrom R, L, R, AndFrom, RowProduct] =
		decomposition.asInstanceOf[ExtendedDecomposition[L AndFrom R, L, R, AndFrom, RowProduct]]

	private[this] val decomposition =
		new ExtendedDecomposition[RowProduct AndFrom MappingAt, RowProduct, MappingAt, AndFrom, RowProduct]



	/** A factory interface of and for the [[net.noresttherein.oldsql.sql.AndFrom AndFrom]] class/instances,
	  * with a covariant upper bound on produced objects. It implements the filtering methods
	  * from [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate RowProductTemplate]] and
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate NonEmptyFromTemplate]] by delegating to
	  * [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.withCondition withCondition]].
	  * For simplicity, and because public interfaces do not implement `NonEmptyFromTemplate` with the narrowed
	  * 'self' type to the [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause, it uses  a single self type.
	  * Final implementation classes for `As` narrow down the result type by extending again `NonEmptyMatrix`
	  * providing the narrowed type as the 'lower' upper bound.
	  */
	trait AndFromTemplate[+L <: RowProduct, R[O] <: MappingAt[O], +U <: (L AndFrom R) with AndFromTemplate[L, R, U]]
		extends FromClauseTemplate[U] with NonEmptyFromTemplate[U, U]
	{ thisClause :U with AndFromTemplate[L, R, U] => //self type is the only reason why everything can't be in AndFrom

		val left :L
		def condition :GlobalBoolean[Generalized]

		/** The `Generalized` type with the left side substituted for `F`. */
		type GeneralizedLeft[+F <: FromSome] <: F AndFrom R

		/** The same type as `this.WithLeft[F]`, but without a [[net.noresttherein.oldsql.sql.RowProduct.As As]] clause
		  * for the last relation. It is the `Dealiased` type of this instance with the left side of the last join
		  * substituted for `F`.
		  */
		type DealiasedLeft[+F <: FromSome] <: GeneralizedLeft[F]

		/** This `Self` type with the left side substituted for `F`. */
		type WithLeft[+F <: FromSome] <: DealiasedLeft[F]

		/** The upper bound on the left side of [[net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate.WithLeft WithLeft]]. */
		type LeftBound = FromSome //this is required by As

		/** A join of the same kind as this clause, but with the left clause substituted for `left`. */
		def withLeft[F <: FromSome](left :F)(filter :GlobalBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]


		/** A copy of this clause with the `condition` being replaced with the given `filter`.
		  * This does not replace the whole ''where'' filter, as the conditions (if present) of the left clause remain
		  * unchanged. It is the target of the `where` and other filtering methods (which add to the condition, rather
		  * then completely replacing it).
		  */
		def withCondition(filter :GlobalBoolean[Generalized]) :Copy

		override def filtered[P >: GlobalScope <: GlobalScope](filter :SQLBoolean[Generalized, P]) :Copy =
			withCondition(condition && filter)


		override type JoinFilter = left.FilterNext[GeneralizedLeft, FromLast, Generalized, LastMapping]

		override def filtered(condition :left.FilterNext[GeneralizedLeft, FromLast, Generalized, LastMapping]) :Copy =
			left.bridgeFilterNext[U, LastMapping](this)(condition)

	}



	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = AndFrom[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

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






trait NonParam[+L <: RowProduct, R[O] <: MappingAt[O]] extends AndFrom[L, R] with AndFromTemplate[L, R, L NonParam R] {
	thisClause =>

	override type Last[O <: RowProduct] = JoinedTable[O, R]

	override val last :JoinedTable[RowProduct AndFrom R, R]
	override def right :Table[R] = last.table

	override def lastAsIn[E <: RowProduct](implicit extension :FromLast PrefixOf E) :JoinedTable[E, R] =
		last.asIn[E]


	override type Generalized >: Dealiased <: (left.Generalized NonParam R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Dealiased >: Self <: (left.Self NonParam R) {
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self NonParam R) {
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

//	type GeneralizedRight[T[O] <: MappingAt[O]] <: L NonParam R
//	type WithRight[T[O] <: MappingAt[O]] <: GeneralizedRight[T]
//
//	def withRight[T[O] <: BaseMapping[S, O], S]
//	             (table :LastTable[T, S])(filter :GlobalBoolean[GeneralizedRight[T]]) :WithRight[T]

	protected override def narrow :left.type NonParam R


	override type FullRow = left.FullRow ~ last.Subject

	override def fullRow[E <: RowProduct]
	                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
		left.fullRow(target)(extension.extendFront[left.Generalized, R]) ~ last.extend(target)


	override type DefineBase[+I <: RowProduct] = I
	override def base :Base = outer

}






object NonParam {

	implicit def nonParamDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L NonParam R, L, R, NonParam, RowProduct] =
		decomposition.asInstanceOf[ExtendedDecomposition[L NonParam R, L, R, NonParam, RowProduct]]

	private[this] val decomposition =
		new ExtendedDecomposition[RowProduct NonParam MappingAt, RowProduct, MappingAt, NonParam, RowProduct]



	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = NonParam[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

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






/** A `RowProduct` constituting of exactly one table or SQL relation.
  * This is a specialized subclass of `AndFrom[Dual, T]`, so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`.
  *///consider: making it a subclass of Subselect. The relation of being a subselect of a clause and grafting would be easier
sealed trait From[T[O] <: MappingAt[O]]
	extends NonParam[Dual, T] with NonSubselect[Dual, T] with AndFromTemplate[Dual, T, From[T]]
{ thisClause =>
	override val last :JoinedTable[RowProduct AndFrom T, T]
	override def right :Table[T] = last.table
	def table :Table[T] = last.relation

	override def lastAsIn[E <: RowProduct](implicit extension :FromLast PrefixOf E) :Last[E] =
		last.asIn[E]

	override type Last[O <: RowProduct] = JoinedTable[O, T]
	override type Generalized = RowProduct NonParam T
	override type Dealiased = From[T]
	override type Self <: From[T]

	override type GeneralizedLeft[+L <: RowProduct] = L NonParam T
	override type DealiasedLeft[+L <: RowProduct] = L NonParam T
	override type WithLeft[+L <: RowProduct] <: L NonParam T
//	override type GeneralizedRight[R[O] <: MappingAt[O]] = RowProduct NonParam R
//	override type WithRight[R[O] <: MappingAt[O]] = From[R]

	override def withLeft[L <: FromClause]
	                     (newLeft :L)(filter :GlobalBoolean[newLeft.Generalized NonParam T]) :WithLeft[L]

//	override def withRight[R[O] <: BaseMapping[S, O], S]
//	                      (table :LastTable[R, S])(filter :GlobalBoolean[GeneralizedRight[R]]) :WithRight[R]


	override def filter :GlobalBoolean[RowProduct NonParam T] =
		if (left.filter eq True) condition else left.filter && condition

	override def filter[E <: RowProduct](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
		filter.basedOn(target)


	override type LastParam = Nothing
	override type Params = @~
	override type AppliedParam = Nothing
	override type Paramless = Self
	override type DecoratedParamless[D <: BoundParamless] = D

	override def bind(param :Nothing) :Nothing = left.bind(param)
	override def bind(params :Params) :Self = self

	protected override def decoratedBind[D <: BoundParamless](params: @~)(decorate :Self => D) :D =
		decorate(self)


	override def fullSize = 1

	override def generalizedExtension[F <: FromSome] :F PrefixOf (F NonParam T) = PrefixOf.itself[F].extend[NonParam, T]

	//overriden for clarity
	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] <: P J T
	override type JoinedWithSubselect[+P <: NonEmptyFrom] = JoinedWith[P, Subselect]

	override type Explicit = RowProduct AndFrom T
	override type Inner = RowProduct AndFrom T
	override type Implicit = RowProduct
	override type Outer = Dual
	override type Base = RowProduct

	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct](target :E)(implicit extension :Generalized ExtendedBy E)
			:ChainTuple[E, GlobalScope, Row] =
		ChainTuple.EmptyChain ~ last.extend(target)

	override type AsSubselectOf[+F <: NonEmptyFrom] <: F Subselect T


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.*]

	override def name :String = "from"

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

	/** A template `From` instance using a dummy mapping for use as a polymorphic factory of `From`/`InnerJoin` clauses. */
	final val template :From.* = From(Relation.Dummy)

	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. It can be later filtered using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom.whereLast whereLast]] method, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`,
	  *                 represented here by types `R =:= T`, split in order to separate the inference of the mapping
	  *                 type and its subject type and remove the need to specify the type parameter for the method
	  *                 explicitly.
	  * @param cast an implicit witness providing the type inference of the subject type of the relation's mapping
	  *             as well as casting functions between associated classes parameterized with `T` and `R`.
	  * @return an unfiltered `From[R]`.
	  */
	def apply[R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (relation :Table[R])(implicit cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
			:From[R] =
		cast(From(LastTable[T, S](cast(relation)), None, True))


	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. This is a lower level factory method accepting an optional, additional filter
	  * for the ''where'' clause, but providing no type inference support and thus generally requiring the type
	  * parameters to be specified explicitly. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`.
	  * @param filter an optional boolean condition for the associated ''where'' clause.
	  * @return a `From[R]`.
	  */
	def apply[T[O] <: BaseMapping[S, O], S]
	         (relation :Table[T], filter: GlobalBoolean[RowProduct NonParam T] = True) :From[T] =
		From(LastTable[T, S](relation), None, filter)


	private[sql] def apply[T[O] <: BaseMapping[S, O], S, A <: Label]
	                      (relation :LastTable[T, S], alias :Option[A], filter :GlobalBoolean[RowProduct NonParam T])
			:From[T] =
		custom(Dual, relation, alias, filter)

	private[sql] def custom[T[O] <: BaseMapping[S, O], S, A <: Label]
	                       (dual :Dual, relation :LastTable[T, S], asOpt :Option[A],
	                        cond :GlobalBoolean[RowProduct NonParam T])
			:EmptyJoin[dual.type, T] As A =
		new EmptyJoin[dual.type, T] with AbstractExtended[dual.type, T, S] {
			override val left :dual.type = dual
			override val last = relation
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override def lastRelation :TableSQL[FromLast, T, S, FromLast] = last

			override def narrow :dual.type NonParam T = this

			override type Alias = A
			override type WithLeft[+L <: RowProduct] = L NonParam T As A
			override type Self = Dual EmptyJoin T As A
			override type DealiasedCopy = left.type EmptyJoin T
			override type Copy = left.type EmptyJoin T As A

			override def withCondition(filter :GlobalBoolean[RowProduct NonParam T]) =
				From.custom(left, last, aliasOpt, filter)

			override def withLeft[F <: FromClause](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized NonParam T]) =
				newLeft.extend(last, aliasOpt, filter)

//			override def withRight[R[O] <: BaseMapping[X, O], X]
//			                      (table :LastTable[R, X])(filter :GlobalBoolean[RowProduct NonParam R]) :From[R] =
//				From.custom(left, table, None, filter)

			override def aliased[N <: Label](alias :N) =
				custom(left, last, Option(alias), condition)

			override def extension[P <: FromSome] =
				PrefixOf.itself[P].extend[NonParam, T].as[A]


			override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = P J T As A
			override type JoinedWithSubselect[+P <: NonEmptyFrom] = P Subselect T As A

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) =
				firstJoin.aliasedJoin[F, T, S, A](prefix, last, aliasOpt)(filter)

			override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) =
				Subselect[F, T, S, A](prefix, last, aliasOpt)(filter)

			override def appendedTo[P <: FromClause](prefix :P) = prefix.extend(last, aliasOpt, filter)


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.extend[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override type AsSubselectOf[+F <: NonEmptyFrom] = F Subselect T As A

			override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F) =
				Subselect[newOuter.type, T, S, A](newOuter, last, aliasOpt)(filter)


			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.from[T, S](this)

		}.asInstanceOf[dual.type EmptyJoin T As A]



	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply[M[O] <: MappingAt[O]](from :RowProduct Compound M) :Option[Table[M]] = from match {
		case f :From[M @unchecked] => Some(f.right)
		case _ => None
	}

	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply(from :RowProduct) :Option[Table.*] = from match {
		case f :From.* => Some(f.table)
		case _ => None
	}



	implicit def fromComposition[T[O] <: MappingAt[O]] :FromComposition[T] =
		composition.asInstanceOf[FromComposition[T]]

	private[this] val composition = new FromComposition[MappingAt]


	class FromComposition[M[O] <: MappingAt[O]] extends ClauseComposition[From[M], Dual, Dual] {
		override type E[+D <: Dual] = From[M]
		override type S[+D >: Dual <: Dual] = From[M]
		override type G[+D >: Dual <: Dual] = D AndFrom M

		override def prefix[A >: Dual <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def extension[A <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def unapply(from :From[M]) :Dual = from.left

		override def apply[D <: Dual](template :From[M], dual :D) :From[M] =
			if (template.left.filter == dual.filter) template //this assumes Dual is sealed and thus it doesn't matter
			else template.withCondition(dual.filter && template.condition) //which instance is used if the filter is right

		override def upcast[A >: Dual <: Dual] :ClauseDecomposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseDecomposition[From[M], A, Dual]]

		override def cast[A <: Dual] :ClauseDecomposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseDecomposition[From[M], A, Dual]]

		override def generalized[A >: Dual <: Dual] :ClauseGeneralization[A AndFrom M, A, Dual]
				{ type G[+B >: A <: Dual] = E[B]; type S[+B >: A <: Dual] = E[B]; type E[+B <: Dual] = B AndFrom M } =
			this.asInstanceOf[ClauseGeneralization[A AndFrom M, A, Dual] {
				type G[+B >: A <: Dual] = E[B]; type S[+B >: A <: Dual] = E[B]; type E[+B <: Dual] = B AndFrom M
			}]
	}



	/** Type alias for `From` with the erased type parameter, covering all instances of `From`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = From[M] forSome { type M[O] <: MappingAt[O] }



	private[sql] trait EmptyJoin[+L <: Dual, T[O] <: MappingAt[O]]
		extends From[T] with NonParam[L, T] with AndFromTemplate[L, T, L EmptyJoin T]


	private class FromTemplate[L <: Dual, T[O] <: BaseMapping[S, O], S, A <: Label]
	              (override val left :L, override val last :LastTable[T, S], override val aliasOpt :Option[A],
	               override val condition :GlobalBoolean[RowProduct AndFrom T])
		extends EmptyJoin[L, T] with AbstractExtended[L, T, S]
	{
		override def lastRelation :TableSQL[FromLast, T, S, FromLast] = last
		override val outer = left.outer

		override def narrow :left.type NonParam T = this.asInstanceOf[left.type NonParam T]

		override type Alias = A
		override type WithLeft[+F <: RowProduct] = F NonParam T As A
		override type Self = Dual EmptyJoin T As A
		override type DealiasedCopy = L EmptyJoin T
		override type Copy = L EmptyJoin T As A

		override def withCondition(filter :GlobalBoolean[RowProduct NonParam T]) =
			From.custom(left, last, aliasOpt, filter)

		override def withLeft[F <: FromClause](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized NonParam T]) =
			newLeft.extend(last, aliasOpt, filter)

		override def aliased[N <: Label](alias :N) =
			custom(left, last, Option(alias), condition)


		override def extension[P <: FromSome] =
			PrefixOf.itself[P].extend[NonParam, T].as[A]


		override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] = P J T As A
		override type JoinedWithSubselect[+P <: NonEmptyFrom] = P Subselect T As A

		override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) =
			firstJoin.aliasedJoin[F, T, S, A](prefix, last, aliasOpt)(filter)

		override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) =
			Subselect[F, T, S, A](prefix, last, aliasOpt)(filter)

		override def appendedTo[P <: FromClause](prefix :P) =
			prefix.extend(last, aliasOpt, filter)


		override def tableStack[E <: RowProduct]
		             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
			last.extend[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


		override type AsSubselectOf[+F <: NonEmptyFrom] = F Subselect T As A

		override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F) =
			Subselect[newOuter.type, T, S, A](newOuter, last, aliasOpt)(filter)


		override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.from[T, S](this)
	}

}


