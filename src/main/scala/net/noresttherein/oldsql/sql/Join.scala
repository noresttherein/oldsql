package net.noresttherein.oldsql.sql

import scala.annotation.{implicitNotFound, showAsInfix}

import net.noresttherein.oldsql.collection.Chain.~
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.Table
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.Table.StaticTable
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.Expanded.{AbstractExpanded, ExpandedComposition, ExpandedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.Join.{AbstractJoin, JoinComposition}
import net.noresttherein.oldsql.sql.RowProduct.{As, ExpandedBy, NonEmptyRow, NonEmptyRowTemplate, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLDialect.SQLSpelling
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ChainTuple, RelationSQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.TableSQL.LastTable
import net.noresttherein.oldsql.sql.mechanics.{RowProductVisitor, SpelledSQL, SQLScribe}
import net.noresttherein.oldsql.sql.mechanics.MappingReveal.BaseMappingSubject
import net.noresttherein.oldsql.sql.mechanics.SpelledSQL.{Parameterization, SQLContext}






/** Common upper type for `AndFrom` subclasses which accept arbitrary `Mapping` subtypes as joined relations,
  * as well as a wide range of clauses as the left side.
  * It is a root of a tree with two branches: [[net.noresttherein.oldsql.sql.Join Join]] subtypes, which represent
  * various actual join kinds (inner, outer, etc), and [[net.noresttherein.oldsql.sql.Subselect Subselect]],
  * which is a linearization of the ''from'' clause of a ''dependent select'' (aka ''subselect''),
  * virtually 'joined' with the ''from'' clause of its outer select as if by a special kind of join.
  */ //this class is almost unused; it seems to serve only the purpose of 'not join param'. Currently only 'joinWith'
@showAsInfix
sealed trait JoinLike[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonParam[L, R] with AndFromTemplate[L, R, L JoinLike R]
{ thisClause =>

	override type Generalized >: Complete <: (left.Generalized JoinLike R) {
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete JoinLike R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type NoAlias >: Self <: (left.Self JoinLike R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override type Self <: (left.Self JoinLike R) {
		type Generalized = thisClause.Generalized
		type Complete    = thisClause.Complete
		type LastParam   = thisClause.LastParam
		type Params      = thisClause.Params
		type FullRow     = thisClause.FullRow
		type Explicit    = thisClause.Explicit
		type Inner       = thisClause.Inner
		type Implicit    = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row         = thisClause.Row
		type OuterRow    = thisClause.OuterRow
	}

	override def narrow :left.type JoinLike R

	override type GeneralizedLeft[+F <: LeftBound] = F GeneralizedJoin R
	override type NoAliasLeft[+F <: LeftBound]     = F LikeJoin R
	override type WithLeft[+F <: LeftBound]       <: F LikeJoin R // <: because alias is kept
//	override type GeneralizedRight[T[O] <: MappingAt[O]] <: L JoinLike T


	/** The generalized version of this join kind: it is `Join` for all real joins and `Subselect` for `Subselect`. */
	type GeneralizedJoin[+F <: LeftBound, T[O] <: MappingAt[O]] >: (F LikeJoin T) <:
		(F JoinLike T) with NonEmptyRowTemplate[F GeneralizedJoin T, F GeneralizedJoin T]
	{
		type GeneralizedJoin[+S <: LeftBound, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
		type LeftBound = thisClause.LeftBound
	}

	/** This join type, fully parameterized with arbitrary prefix clause and relation mapping. Used by copy constructors
	  * of this class.
	  * @see [[net.noresttherein.oldsql.sql.JoinLike.likeJoin]]
	  */ //consider: renaming to SelfJoin/EgoJoin
	type LikeJoin[+F <: LeftBound,
	              T[O] <: MappingAt[O]] <: (F JoinLike T) with NonEmptyRowTemplate[F LikeJoin T, F LikeJoin T]
	{
		type GeneralizedJoin[+S <: LeftBound, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
		type LikeJoin[+S <: LeftBound, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
		type LeftBound = thisClause.LeftBound
	}



	protected[sql] final def aliasedJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                                    (left :F, right :LastTable[T, X], alias :Option[A])
	                                    (filter :SingleBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T As A =
		likeJoin(left, right, alias)(filter)

	/** Creates a new clause of the same class as this instance, using provided argument as its properties
	  * in their exact form.
	  */
	protected def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                      (left :F, right :LastTable[T, X], alias :Option[A])
	                      (filter :SingleBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T As A

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join. It is the lower
	  * level variant of the other `likeJoin` method which accepts a `Relation[T]`.
	  * @param left the ''from'' clause containing all relations preceding `right` in the new clause.
	  * @param right a `TableSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]` representing the last joined table.
	  *              It must be the [[net.noresttherein.oldsql.sql.ast.JoinedRelation.isDefault default]] view
	  *              of its table, or `IllegalArgumentException` will be thrown.
	  * @param filter the exact join condition expression to use for the result. It will ''not'' be anchored by this method.
	  */ //todo: this allows includes/excludes on right, not only the table itself. Problematic semantics, complicates spelling
	@throws[IllegalArgumentException]("if right is not default (represents an altered view of its table).")
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :LastTable[T, X])
	            (filter :SingleBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		if (!right.isDefault)
			throw new IllegalArgumentException(
				"Cannot use a JoinedTable with non empty includes/excludes within a join: " + right + ". " +
				"Alter the Table directly instead."
			)
		else if (right.index != 0)
			throw new IllegalArgumentException(
				"Cannot join a JoinedTable other than 'last' (index=0): " + right + "."
			)
		else
			likeJoin(left, right, None)(filter)

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join.
	  * @param left the ''from'' clause containing all relations preceding `right` in the new clause.
	  * @param right the last table (possibly a ''table expression'') of the new clause.
	  * @param filter the exact join condition expression to use for the result. It will ''not'' be anchored by this method.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :Table[T])(filter :SingleBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		likeJoin(left, LastTable[T, X](right))(filter)

	/** Joins the two clauses using this join kind. If the first join of the second, `right` clause
	  * is not the `From` pseudo join, the existing join is used instead of this join.
	  * @throws UnsupportedOperationException if this clause is a `Subselect` and the right clause contains
	  *                                       a `JoinParam` synthetic join.
	  */
	def likeJoin[P <: FromSome, S <: FromSome](left :P, right :S) :right.JoinedWith[P, LikeJoin]



//todo: def by(fk :R[FromLast] => ForeignKey[FromLast]) :L LikeJoin R

}






/** A matching pattern for [[net.noresttherein.oldsql.sql.JoinLike JoinLike]] classes and a scope for
  * associated type classes and type constructors.
  */
object JoinLike {

	/** Matches all `JoinLike` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :JoinLike[L @unchecked, R @unchecked] => Got((join.left, join.right))
		case _ => Lack
	}

	/** Matches all `JoinLike` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(RowProduct, Table.__)]  = from match {
		case join :JoinLike.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def joinLikeDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExpandedDecomposition[L JoinLike R, L, R, JoinLike, RowProduct] =
		decomposition.asInstanceOf[ExpandedDecomposition[L JoinLike R, L, R, JoinLike, RowProduct]]

	private[this] val decomposition =
		new ExpandedDecomposition[RowProduct JoinLike MappingAt, RowProduct, MappingAt, JoinLike, RowProduct]


	/** Type alias for `JoinLike` with erased type parameters, covering all instances of `JoinLike`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = JoinLike[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.JoinLike JoinLike]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = RowProduct JoinLike R

	/** A curried type constructor for `JoinLike` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Join/Left[ ]#Right
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L JoinLike R }

	/** A curried type constructor for `JoinLike` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L JoinLike R }
}






/** Base trait for 'true' join implementations representing a real SQL join between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression. Subclasses represent various join kinds
  * (inner, outer, left outer, right outer).
  * It is the [[net.noresttherein.oldsql.sql.RowProduct!.Generalized generalized]] form of all extending classes, meaning
  * that all join conditions for subclasses use this type instead the proper concrete subclass in the `RowProduct`
  * parameter of the `GlobalBoolean`.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  */
@showAsInfix
sealed trait Join[+L <: FromSome, R[O] <: MappingAt[O]]
	extends JoinLike[L, R] with NonSubselect[L, R] with AndFromTemplate[L, R, L Join R]
{ thisClause =>

	override type Generalized = left.Generalized Join R
	override type Complete    = left.Complete Join R
	override type NoAlias     = left.Self LikeJoin R
	override type Self       <: left.Self LikeJoin R

	override def generalizedClass :Class[left.Generalized Join R] = classOf[left.Generalized Join R]
	override def narrow           :WithLeft[left.type]

	override type LeftBound   = FromSome
//	override type GeneralizedRight[T[O] <: MappingAt[O]] >: WithRight[T] <: L GeneralizedJoin T
//	override type WithRight[T[O] <: MappingAt[O]] <: L LikeJoin T
	override type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F Join T

	override type LikeJoin[+F <: FromSome,
	                       T[O] <: MappingAt[O]] <: (F Join T) with NonEmptyRowTemplate[F LikeJoin T, F LikeJoin T]
	{
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	override def likeJoin[P <: FromSome, S <: RowProduct](left :P, right :S) :right.JoinedWith[P, LikeJoin] =
		right.joinedWith(left, this)


	override def filter :SingleBoolean[Generalized] = super.filter //overriden due to overload ambiguity

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
		left.filter(target)(expansion.expandFront[left.Generalized, R]) && condition.basedOn(target)


	override type AppliedParam = WithLeft[left.AppliedParam] //WithLeft because it is overriden by As
	override type GeneralizedParamless = left.GeneralizedParamless LikeJoin R
	override type Paramless = WithLeft[left.Paramless]

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		if (condition == True)
			withLeft[l.type](l)(True)
		else {
			val unfiltered = withLeft[l.type](l)(True)
			val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param)
			withLeft[l.type](l)(substitute(condition))
		}
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		withLeft[l.type](l)(substitute(condition))
	}



	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[P, J]]

	override type SelectedFrom[+P <: NonEmptyRow] = WithLeft[left.SelectedFrom[P]]


	override type Explicit = left.Explicit Join R
	override type Inner    = left.Inner LikeJoin R
	override type Implicit = left.Implicit
	override type Outer    = left.Outer

	override type DefineBase[+I <: RowProduct] = left.DefineBase[I]
	override def base :Base = left.base

	override type Row      = left.Row ~ last.Subject
	override type OuterRow = left.OuterRow

	override def row[E <: RowProduct]
	                (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, Single, Row] =
		left.row(target)(expansion.expandFront[left.Generalized, R]) ~ last.expand(target)


	override type AsSubselectOf[+F <: NonEmptyRow] = WithLeft[left.AsSubselectOf[F]]

	override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized Join R](generalized, unfiltered)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}


	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] = {
		val ctx = left.spellingContext
		ctx.join(spelling.alias(right, aliasOpt)(generalized, ctx))
	}

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Join.__]

}






object Join {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] InnerJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
	          R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :BaseMappingSubject[L, LA, A], castR :BaseMappingSubject[R, RB, B]) :From[L] InnerJoin R =
		InnerJoin(left, right)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome { type Generalized <: G }, G <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[G Join R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `Join` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :Join[L @unchecked, R @unchecked] => Got((join.left, join.right))
		case _ => Lack
	}

	/** Matches all `Join` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Table.__)]  = from match {
		case join :Join.__ => Got((join.left, join.right))
		case _ => Lack
	}



	@implicitNotFound("I do not know how to compose a join ${L} ${J} ${R}.\n" +
	                  "Missing implicit JoinComposition[${L}, ${R}, ${J}].")
	class JoinComposition[L <: FromSome, R[O] <: MappingAt[O],
	                      J[+A <: FromSome, B[O] <: MappingAt[O]] <:
	                        (A Join B) { type LikeJoin[+X <: FromSome, Y[O] <: MappingAt[O]] <: X J Y }]
		extends ExpandedComposition[L, R, J, FromSome, MappingAt]
	{
		override def apply[C <: FromSome](template :L J R, left :C) :C J R = template.withLeft(left)(True)

		def apply[A <: FromSome, B[O] <: BaseMapping[S, O], S]
		         (template :L J R, left :A, right :Table[B])
		         (condition :SingleBoolean[template.GeneralizedJoin[left.Generalized, B]]) :A J B =
			template.likeJoin[A, B, S](left, right)(condition)
	}


	implicit def joinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, Join] =
		composition.asInstanceOf[JoinComposition[L, R, Join]]

	private[this] val composition = new JoinComposition[FromSome, MappingAt, Join]



	/** Type alias for `Join` with erased type parameters, covering all instances of `Join`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = Join[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all proper [[net.noresttherein.oldsql.sql.Join Join]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome Join R

	/** A curried type constructor for `Join` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L Join R }

	/** A curried type constructor for `Join` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L Join R }



	private[sql] trait AbstractJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S]
		extends AbstractExpanded[L, R, S] with Join[L, R]
	{ thisClause =>
		override val last :TableSQL[FromLast, R, S]
		protected override def lastRelation :TableSQL[FromLast, R, S] = last
		//todo: in Scala 3 check if the overloads are more sensible
		override lazy val tableStack = (super.tableStack :Seq[RelationSQL.from[Generalized]#__]).toIndexedSeq
		override lazy val fullTableStack = (super.fullTableStack :Seq[RelationSQL.from[Generalized]#__]).toIndexedSeq
		override lazy val withClause = left.withClause ++ table.withClause ++ condition.outerWithClause

		override type GeneralizedLeft[+F <: FromSome] = F GeneralizedJoin R
		override type NoAliasLeft[+F <: FromSome] = F LikeJoin R
		override type WithLeft[+F <: FromSome] = F LikeJoin R As Alias
//		override type GeneralizedRight[T[O] <: MappingAt[O]] = L GeneralizedJoin T
//		override type WithRight[T[O] <: MappingAt[O]] = L LikeJoin T
		override type Self = WithLeft[left.Self]

		override def withLeft[F <: FromSome](newLeft :F)(filter :SingleBoolean[newLeft.Generalized GeneralizedJoin R])
				:F LikeJoin R As Alias =
			likeJoin(newLeft, last, aliasOpt)(filter)

//		override def withRight[T[O] <: BaseMapping[X, O], X]
//		                      (table :LastTable[T, X])(filter :GlobalBoolean[L GeneralizedJoin T]) :L LikeJoin T =
//			likeJoin(left, table, None)(filter)


		override def expansion[P <: FromSome] :P PrefixOf (P LikeJoin R As Alias) =
			PrefixOf.itself[P].expand[LikeJoin, R].as[Alias]

		override def generalizedExpansion[P <: FromSome] :P PrefixOf (P GeneralizedJoin R) =
			PrefixOf.itself[P].expand[GeneralizedJoin, R]

		override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) :JoinedWith[F, firstJoin.LikeJoin] =
			withLeft(left.joinedWith(prefix, firstJoin))(condition :SingleBoolean[left.Generalized GeneralizedJoin R])

		override def selectedFrom[F <: NonEmptyRow](prefix :F) :SelectedFrom[F] =
			withLeft(left.selectedFrom(prefix))(condition :SingleBoolean[left.Generalized GeneralizedJoin R])

		override def appendedTo[P <: FromClause](prefix :P) :JoinedWith[P, NonParam] =
			withLeft(left.appendedTo(prefix))(condition :SingleBoolean[left.Generalized GeneralizedJoin R])


		protected override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.join[L, R, S](this)
	}

}






/** A standard join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, filtered by the conjunction of this join's `condition` with the combined filter expression
  * of the left side.
  */
@showAsInfix
sealed trait InnerJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L InnerJoin R]
{ thisClause =>
	override def selfClass :Class[Self] = classOf[left.Self InnerJoin R].asInstanceOf[Class[Self]]

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F InnerJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :SingleBoolean[left.Generalized Join T]) :F InnerJoin T As A =
		InnerJoin(left, right, alias)(filter)


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.join(this, spelling.INNER_JOIN)(context, params)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin.__]

	override def name :String = "join"
}






object InnerJoin {

	/** A template `InnerJoin` instance with a dummy mapping, for use as a polymorphic factory of `InnerJoin` joins. */
	final val template :InnerJoin.__ = InnerJoin(Table.Dummy, Table.Dummy)

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] InnerJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
	          R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :BaseMappingSubject[L, LA, A], castR :BaseMappingSubject[R, RB, B]) :From[L] InnerJoin R =
		castR.back.join(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome { type Generalized <: G }, G <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[G Join R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L InnerJoin R = //consider: why don't we anchor filter?
		cast.back.join(apply(left, LastTable[T, S](cast(right)), None)(cast.column(filter)))

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The `String` literal with the name of the table is used as the alias, allowing easier access to the relation.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left  a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last table in the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param cast  an implicit witness providing proper type inference for the mapping of the last relation
	  *              and conversions of associated classes between instances parameterized with the more generic `R`
	  *              and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R As N`.
	  */
	def apply[L <: FromSome, N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :StaticTable[N, R])
	         (implicit cast :BaseMappingSubject[R, T, S]) :L InnerJoin R As N =
		cast.back.alias(apply(left, LastTable[T, S](cast(right)), Some(right.name))(True))


	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :SingleBoolean[prefix.Generalized Join R]) :L InnerJoin R As A =
		new InnerJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left      = prefix
			override val last      = next
			override val aliasOpt  = asOpt
			override val condition = cond
			override val outer     = left.outer
			override val fullSize  = left.fullSize + 1
			override val parameterization = left.parameterization.join[left.Self, R, left.Self InnerJoin R].as(self)

			override type Alias       = A
			override type NoAliasCopy = left.type InnerJoin R
			override type Copy        = left.type InnerJoin R As A

			override def narrow :left.type InnerJoin R As A = this.asInstanceOf[left.type InnerJoin R As A]

			//needs to be private because the result is This
			override def withCondition(filter :SingleBoolean[Generalized]) =
				InnerJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				InnerJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.innerJoin[L, R, S](this)

		}.asInstanceOf[L InnerJoin R As A]



	/** Matches all `InnerJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :InnerJoin[L @unchecked, R @unchecked] => Got(join.left -> join.right)
		case _ => Lack
	}

	/** Matches all `InnerJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Table.__)] = from match {
		case join :InnerJoin.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def innerJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, InnerJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, InnerJoin]]

	/** Type alias for `InnerJoin` with erased type parameters, covering all instances of `InnerJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = InnerJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.InnerJoin InnerJoin]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome InnerJoin R

	/** A curried type constructor for `InnerJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L InnerJoin R }

	/** A curried type constructor for `InnerJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L InnerJoin R }
}






/** A symmetrical outer join of two (or more) SQL relations, that is one where every row in either of them occurs
  * at least once in the result, matched with an artificial row consisting of only null values from the other table
  * if no true match exist. The left side, which may consist of any number of relations greater than zero, is treated
  * as a single relation for this purpose. The join is filtered by the conjunction of this join's `condition`
  * with the combined filter expression of the left side.
  */
@showAsInfix
sealed trait OuterJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L OuterJoin R]
{ thisClause =>
	override def selfClass :Class[Self] = classOf[left.Self OuterJoin R].asInstanceOf[Class[Self]]

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F OuterJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :SingleBoolean[left.Generalized Join T]) :F OuterJoin T As A =
		OuterJoin(left, right, alias)(filter)


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.join(this, spelling.OUTER_JOIN)(context, params)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[OuterJoin.__]

	override def name :String = "outer join"
}






object OuterJoin {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :OuterJoin.__ = OuterJoin(Table.Dummy, Table.Dummy)

	/** Create an outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.outerJoin outerJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] OuterJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :BaseMappingSubject[L, LA, A], castR :BaseMappingSubject[R, RB, B]) :From[L] OuterJoin R =
		castR.back.join(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create an outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.outerJoin outerJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome { type Generalized <: G }, G <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[G Join R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L OuterJoin R =
		cast.back.join(apply(left, LastTable[T, S](cast(right)), None)(cast.column(filter)))

	/** Create an outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The `String` literal with the name of the table is used as the alias, allowing easier access to the relation.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last table in the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L OuterJoin R As N`.
	  */
	def apply[L <: FromSome, N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :StaticTable[N, R])
	         (implicit cast :BaseMappingSubject[R, T, S]) :L OuterJoin R As N =
		cast.back.alias(apply(left, LastTable[T, S](cast(right)), Some(right.name))(True))


	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :SingleBoolean[prefix.Generalized Join R]) :L OuterJoin R As A =
		new OuterJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left      = prefix
			override val last      = next
			override val aliasOpt  = asOpt
			override val condition = cond
			override val outer     = left.outer
			override val fullSize  = left.fullSize + 1
			override val parameterization = left.parameterization.join[left.Self, R, left.Self OuterJoin R].as(self)

			override type Alias = A
			override type NoAliasCopy = left.type OuterJoin R
			override type Copy = left.type OuterJoin R As A

			override def narrow :left.type OuterJoin R As A = this.asInstanceOf[left.type OuterJoin R As A]

			//needs to be private because the result is This
			override def withCondition(filter :SingleBoolean[Generalized]) =
				OuterJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				OuterJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.outerJoin[L, R, S](this)

		}.asInstanceOf[L OuterJoin R As A]



	/** Matches all `OuterJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :OuterJoin[L @unchecked, R @unchecked] => Got(join.left -> join.right)
		case _ => Lack
	}

	/** Matches all `OuterJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Table.__)] = from match {
		case join :OuterJoin.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def outerJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, OuterJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, OuterJoin]]



	/** Type alias for `OuterJoin` with erased type parameters, covering all instances of `OuterJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = OuterJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.OuterJoin OuterJoin]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome OuterJoin R

	/** A curried type constructor for `OuterJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L OuterJoin R }

	/** A curried type constructor for `OuterJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L OuterJoin R }
}






/** A left outer join of two (or more) SQL relations, that is one where every row in the preceding (left) clause/tables
  * occurs at least once in the result, matched with an artificial row consisting of only null values from the right table
  * if no true match exist. The left side, which may consist of any number of relations greater than zero, is treated
  * as a single relation for this purpose. The join is filtered by the conjunction of this join's `condition`
  * with the combined filter expression of the left side.
  */ //consider: type 'NotRightJoin' as the left side
@showAsInfix
sealed trait LeftJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L LeftJoin R]
{ thisClause =>
	override def selfClass :Class[Self] = classOf[left.Self LeftJoin R].asInstanceOf[Class[Self]]

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F LeftJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :SingleBoolean[left.Generalized Join T]) :F LeftJoin T As A =
		LeftJoin(left, right, alias)(filter)


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.join(this, spelling.LEFT_JOIN)(context, params)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin.__]

	override def name = "left join"
}






object LeftJoin {

	/** A template `LeftJoin` instance with a dummy mapping, for use as a polymorphic factory of `LeftJoin` joins. */
	final val template :LeftJoin.__ = LeftJoin(Table.Dummy, Table.Dummy)

	/** Create a left outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.leftJoin leftJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] LeftJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
	          R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :BaseMappingSubject[L, LA, A], castR :BaseMappingSubject[R, RB, B]) :From[L] LeftJoin R =
		castR.back.join(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a left outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.leftJoin leftJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L LeftJoin R`.
	  */
	def apply[L <: FromSome { type Generalized <: G }, G <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[G Join R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L LeftJoin R =
		cast.back.join(apply(left, LastTable[T, S](cast(right)), None)(cast.column(filter)))

	/** Create a left outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The `String` literal with the name of the table is used as the alias, allowing easier access to the relation.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last table in the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L LeftJoin R As N`.
	  */
	def apply[L <: FromSome, N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :StaticTable[N, R])
	         (implicit cast :BaseMappingSubject[R, T, S]) :L LeftJoin R As N =
		cast.back.alias(apply(left, LastTable[T, S](cast(right)), Some(right.name))(True))


	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :SingleBoolean[prefix.Generalized Join R]) :L LeftJoin R As A =
		new LeftJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left      = prefix
			override val last      = next
			override val aliasOpt  = asOpt
			override val condition = cond
			override val outer     = left.outer
			override val fullSize  = left.fullSize + 1
			override val parameterization = left.parameterization.join[left.Self, R, left.Self LeftJoin R].as(self)
			override type Alias       = A
			override type NoAliasCopy = left.type LeftJoin R
			override type Copy        = left.type LeftJoin R As A

			override def narrow :left.type LeftJoin R As A = this.asInstanceOf[left.type LeftJoin R As A]

			override def withCondition(filter :SingleBoolean[Generalized]) =
				LeftJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				LeftJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def visit[Y](visitor :RowProductVisitor[Y]) :Y = visitor.leftJoin[L, R, S](this)

		}.asInstanceOf[L LeftJoin R As A]



	/** Matches all `LeftJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :LeftJoin[L @unchecked, R @unchecked] => Got(join.left -> join.right)
		case _ => Lack
	}

	/** Matches all `LeftJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Table.__)] = from match {
		case join :LeftJoin.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def leftJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, LeftJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, LeftJoin]]



	/** Type alias for `LeftJoin` with erased type parameters, covering all instances of `LeftJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = LeftJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.LeftJoin LeftJoin]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome LeftJoin R

	/** A curried type constructor for `LeftJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L LeftJoin R }

	/** A curried type constructor for `LeftJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L LeftJoin R }
}






/** A right outer join of two (or more) SQL relations, that is one where every row in the right table occurs
  * at least once in the result, matched with an artificial row consisting of only null values from the tables
  * of the left side if no true match exist. The left side, which may consist of any number of relations greater than
  * zero, is treated as a single relation for this purpose. The join is filtered by the conjunction of this join's
  * `condition` with the combined filter expression of the left side.
  */
@showAsInfix
sealed trait RightJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L RightJoin R]
{ thisClause =>
	override def selfClass :Class[Self] = classOf[left.Self RightJoin R].asInstanceOf[Class[Self]]

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F RightJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :SingleBoolean[left.Generalized Join T]) :F RightJoin T As A =
		RightJoin(left, right, alias)(filter)


	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
		spelling.join(this, spelling.RIGHT_JOIN)(context, params)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin.__]

	override def name = "right join"
}






object RightJoin {

	/** A template `RightJoin` instance with a dummy mapping, used as a polymorphic factory of `RightJoin` joins.  */
	final val template : RightJoin.__ = RightJoin(Table.Dummy, Table.Dummy)

	/** Create a right outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is an alternative to
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.rightJoin rightJoin]] `right`,
	  * which should be generally preferred.
	  * @param left  the first relation of the ''from'' clause, using a `LA[O] <: BaseMapping[A, O]` `Mapping` type.
	  * @param right the second relation of the ''from'' clause, using a `RB[O] <: BaseMapping[B, O]` `Mapping` type.
	  * @param castL implicit witness providing proper type inference for the subject of the left relation
	  *              and conversions of associated classes between instances parameterized with `L` and `LA`.
	  * @param castR implicit witness providing proper type inference for the subject of the right relation
	  *              and conversions of associated classes between instances parameterized with `R` and `RB`.
	  * @tparam L  the type constructor for the mapping of the first relation, accepting the `Origin` type.
	  * @tparam LA the same type as `L`, but with an upper bound of `BaseMapping`, separating the inference of types `L`
	  *            and its subject type `A`.
	  * @tparam R  the type constructor for the mapping of the second relation, accepting the `Origin` type.
	  * @tparam RB the same type as `B`, but with an upper bound of `BaseMapping`, separating the inference of types `R`
	  *            and its subject type `B`.
	  * @return an unfiltered `From[L] RightJoin R` clause joining the two relations.
	  */
	def apply[L[O] <: MappingAt[O], LA[O] <: BaseMapping[A, O], A,
	          R[O] <: MappingAt[O], RB[O] <: BaseMapping[B, O], B]
	         (left :Table[L], right :Table[R])
	         (implicit castL :BaseMappingSubject[L, LA, A], castR :BaseMappingSubject[R, RB, B]) :From[L] RightJoin R =
		castR.back.join(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a right outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.rightJoin rightJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L RightJoin R`.
	  */
	def apply[L <: FromSome { type Generalized <: G }, G <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :SingleBoolean[G Join R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L RightJoin R =
		cast.back.join(apply(left, LastTable[T, S](cast(right)), None)(cast.column(filter)))

	/** Create a right outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The `String` literal with the name of the table is used as the alias, allowing easier access to the relation.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left   a ''from'' clause containing the list of relations preceding `right`.
	  * @param right  the last table in the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *               `Mapping` type.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L RightJoin R As N`.
	  */
	def apply[L <: FromSome, N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :StaticTable[N, R])
	         (implicit cast :BaseMappingSubject[R, T, S]) :L RightJoin R As N =
		cast.back.alias(apply(left, LastTable[T, S](cast(right)), Some(right.name))(True))


	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :SingleBoolean[prefix.Generalized Join R]) :L RightJoin R As A =
		new RightJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left      = prefix
			override val last      = next
			override val aliasOpt  = asOpt
			override val condition = cond
			override val outer     = left.outer
			override val fullSize  = left.fullSize + 1
			override val parameterization = left.parameterization.join[left.Self, R, left.Self RightJoin R].as(self)

			override type Alias       = A
			override type NoAliasCopy = left.type RightJoin R
			override type Copy        = left.type RightJoin R As A

			override def narrow :left.type RightJoin R As A = this.asInstanceOf[left.type RightJoin R As A]

			override def withCondition(filter :SingleBoolean[Generalized]) =
				RightJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				RightJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.rightJoin[L, R, S](this)

		}.asInstanceOf[L RightJoin R As A]



	/** Matches all `RightJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case join :RightJoin[L @unchecked, R @unchecked] => Got(join.left -> join.right)
		case _ => Lack
	}

	/** Matches all `RightJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Opt[(FromSome, Table.__)] = from match {
		case join :RightJoin.__ => Got((join.left, join.right))
		case _ => Lack
	}



	implicit def rightJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, RightJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, RightJoin]]



	/** Type alias for `RightJoin` with erased type parameters, covering all instances of `RightJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = RightJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.RightJoin RightJoin]] clauses with mapping `R`
	  * as their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = FromSome RightJoin R

	/** A curried type constructor for `RightJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L RightJoin R }

	/** A curried type constructor for `RightJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L RightJoin R }
}






/** A special artificial join type serving as a ''from'' clause for dependent ''selects'' (''selects'' occurring
  * inside another ''select'', either in its ''select'' or ''where'' clause). It has direct access to all tables
  * from the outer clause, while providing its own additional relations in its (explicit) ''from'' clause.
  * It is represented as a pseudo join between the outer clause (the left side), forming the ''implicit'' portion
  * of this clause, and the first member of the ''subselect's'' ''from'' tables (the right side),
  * starting the ''explicit'' portion of this clause. Joining additional relations has the effect of adding them
  * to that ''explicit'' portion, available only to the ''subselect'' (and, potentially, its own ''subselects'').
  * This allows any [[net.noresttherein.oldsql.sql.SQLExpression.Single global]] SQL expressions
  * based on the outer ''select's'' clause to be used as expressions based on this select, as it doesn't differ
  * from any other [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] join-like expansion. Note that it is possible
  * to recursively nest ''subselects'' to an arbitrary depth, modelled by a repeated use of this join type.
  * In that case all relations/mappings to the left of the first occurrence of `Subselect` in the type definition
  * are the ''from'' clause of the most outer, independent ''select'', while relations/mappings in between subsequent
  * `Subselect`s form the ''from'' clauses of subsequent nested ''selects'', with any relations/mappings
  * following its last occurrence in the type being exclusive to the deepest ''subselect'' in the chain.
  *
  * This clause type is referred to in the documentation almost exclusively as a 'subselect join', or `Subselect` join,
  * while `RowProduct` subtypes which contain a `Subselect` anywhere in their (dynamic) type definition are called
  * ''subselect'' clauses, regardless of whether `Subselect` is the last 'join' or if occurs before. The latter term
  * is also sometimes used for the list of relations from between two subselect boundaries, in particular
  * for all relations to the right of the last `Subselect` join. This does not take into account
  * ''selects'' occurring inside a ''from'' clause as a relation, as they are independent expressions, with none
  * having access to the ''from'' clause relations of the other. On the other hand, clauses without a `Subselect`
  * in their concrete definition are most often called ''outer'' clauses, after the independent,
  * ''outer'' select expressions which can be based on them. All subselect clauses conform to
  * [[net.noresttherein.oldsql.sql.RowProduct.SelectedFrom SelectedFrom]], providing the type is instantiated at least
  * to the point of the last `Subselect` and no
  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]] join
  * is present to its right. Additionally, direct subselect clauses of some outer clause `F` (that is,
  * those without any `Subselect` to the right of `F` in their type definition) conform to
  * [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect L#DirectSubselect]] if both clauses are at least
  * in their generalized form. Outer clauses are also valid ''from'' clauses for a subselect of any other select,
  * but do not conform to any of the above, thus this relationship is typically established instead
  * by the [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf]]`[F]` type alias, which covers
  * both independent clauses and actual subselects (but not those of an indirect subselect,
  * that is with another occurrence of `Subselect` to the right of `F`). Note that while it is possible to construct
  * a `RowProduct` instance with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
  * (or [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) to the right of a subselect join, such a clause
  * will be invalid: it will conform to neither of the types listed above, representing the relation of being
  * a subselect clause, which will make it impossible to construct
  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] based on it.
  *
  * @tparam L the type of outer ''select'' 's ''from'' clause.
  * @tparam R the right side of this join - the first relation of the ''from'' clause of the represented subselect.
  *
  * @see [[net.noresttherein.oldsql.sql.RowProduct.SelectedFrom]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.TopRow]]
  */
//consider: renaming to SelectJoin, SubselectFrom.
@showAsInfix
sealed trait Subselect[+L <: NonEmptyRow, R[O] <: MappingAt[O]]
	extends JoinLike[L, R] with SelectFrom[L, R] with AndFromTemplate[L, R, L Subselect R]
{ thisClause =>

	override def generalizedClass :Class[left.Generalized Subselect R] = classOf[left.Generalized Subselect R]
	override def selfClass        :Class[Self] = classOf[left.Self Subselect R].asInstanceOf[Class[Self]]

	override type Generalized = left.Generalized Subselect R
	override type Complete    = left.Complete Subselect R
	override type NoAlias     = left.Self Subselect R
	override type Self       <: left.Self Subselect R

	//widened bounds
	override type LeftBound = NonEmptyRow
	override type OuterLeft[+F <: NonEmptyRow] = F Subselect R
//	override type GeneralizedRight[R[O] <: MappingAt[O]] = WithRight[R] //<: F Subselect R
//	override type WithRight[R[O] <: MappingAt[O]] <: F Subselect R
	override type GeneralizedJoin[+F <: NonEmptyRow, T[O] <: MappingAt[O]] = F Subselect T
	override type LikeJoin[+F <: NonEmptyRow, T[O] <: MappingAt[O]]        = F Subselect T

	override def withLeft[F <: NonEmptyRow](left :F)(filter :SingleBoolean[left.Generalized Subselect R]) :WithLeft[F] //wider bound

//	override def withRight[R[O] <: BaseMapping[S, O], S]
//	                      (table :LastTable[R, S])(filter :GlobalBoolean[WithRight[R]]) :WithRight[R] =
//		likeJoin(left, table, None)(filter)

	protected override def likeJoin[F <: NonEmptyRow, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :SingleBoolean[left.Generalized Subselect T]) :F Subselect T As A =
		Subselect(left, right, alias)(filter)

	override def likeJoin[F <: NonEmptyRow, T[O] <: BaseMapping[X, O], X] //wider bound
	                     (left :F, right :Table[T])(filter :SingleBoolean[left.Generalized Subselect T]) :F Subselect T =
		Subselect(left, LastTable[T, X](right), None)(filter)

	override def likeJoin[P <: NonEmptyRow, S <: FromSome](left :P, right :S) :right.SelectedFrom[P] =
		right.selectedFrom(left)


	override def filter :SingleBoolean[Generalized] = super.filter //overriden due to Scala's stupid overloading rules

	override def filter[E <: RowProduct](target :E)(implicit expansion :Generalized PartOf E) :SingleBoolean[E] =
		condition.basedOn(target)

	override type JoinedWith[+P <: RowProduct, +J[+F <: P, M[O] <: MappingAt[O]] <: F NonParam M] =
		WithLeft[left.JoinedWith[P, J]]

	override type SelectedFrom[+P <: NonEmptyRow] = WithLeft[left.SelectedFrom[P]]


	override type AppliedParam = WithLeft[left.AppliedParam]
	override type GeneralizedParamless = left.GeneralizedParamless Subselect R
	override type Paramless = WithLeft[left.Paramless]

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		if (condition == True)
			withLeft[l.type](l)(True)
		else { //even if last param is under grouping, a grouping expression may use it and thus propagate to anything using it
			val unfiltered = withLeft[l.type](l)(True)
			val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param)
			withLeft[l.type](l)(substitute(condition))
		}
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		withLeft[l.type](l)(substitute(condition))
	}


	override def isSubselect = true //if it will accept Dual as the left side the standard definition in RowProduct must be changed.
	override def isValidSubselect = true

	override type Inner    = NonEmptyRow Subselect R
	override type Outer    = left.Self

	override type AsSubselectOf[+F <: NonEmptyRow] = WithLeft[F]

	override def asSubselectOf[F <: NonEmptyRow](newOuter :F)(implicit expansion :Implicit ExpandedBy F)
			:WithLeft[newOuter.type] =
	{
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		val unfiltered = withLeft[newOuter.type](newOuter)(True)
		val substitute = SQLScribe.shiftBack(generalized, unfiltered.generalized)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}


	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
		spelling.sqlParamCount(right) + spelling.inWhere.sqlParamCount(condition)

	protected override def defaultSpelling[P](context :SQLContext[P], params :Parameterization[P, Generalized])
	                                         (implicit spelling :SQLSpelling) :SpelledSQL[P] =
	{
		val aliased = spelling.table(right, aliasOpt)(generalized, context.subselect, params)
		val sql = (spelling.FROM + " ") +: aliased
		if (condition == True) sql else sql && (spelling.inWhere(filter)(self, _, params))
	}
	override def spellingContext(implicit spelling :SQLSpelling) :SQLContext[Params] = {
		val ctx = left.spellingContext
		ctx.subselect(spelling.alias(right, aliasOpt)(generalized, ctx))
	}


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Subselect.__]

	override def name = "subselect"
}






object Subselect {

	/** A template `Subselect` instance with a dummy mapping, for use as a polymorphic factory of `Subselect` joins. */
	final val template :Subselect.__ = Subselect(From(Table.Dummy), Table.Dummy)


	/** Create a ''from'' clause of a subselect of a select with `outer` as its ''from'' clause, using the given
	  * relation `first` as the sole member of its (actual) ''from'' clause. The result is a special kind of
	  * an artificial 'join' between the ''implicit'' portion (the ''from'' clause of the outer select, providing
	  * access to all its relations, without them appearing in the ''from'' clause of the generated select SQL)
	  * and the ''explicit'' portion (the relations which actually appear in the ''from'' clause of the generated
	  * subselect SQL), consisting of the relation `first` and any other, subsequently joined with the result.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `outer` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] `first`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `fullFilter` DSL instead.
	  * @param outer  a ''from'' clause containing the list of relations preceding `first`.
	  * @param first  the first (and currently only) relation of the actual ''from'' clause of the created subselect,
	  *               using the `T[O] <: BaseMapping[S, O]` `Mapping` type.
	  * @param filter an optional join condition linking the subselect with the outer clause.
	  * @param cast   an implicit witness providing proper type inference for the mapping of the last relation
	  *               and conversions of associated classes between instances parameterized with the more generic `R`
	  *               and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L Subselect R`.
	  */
	def apply[L <: NonEmptyRow { type Generalized <: G }, G <: NonEmptyRow, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (outer :L, first :Table[R], filter :SingleBoolean[G Subselect R] = True)
	         (implicit cast :BaseMappingSubject[R, T, S]) :L Subselect R =
		cast.back.join(Subselect[L, T, S, Nothing](outer, LastTable[T, S](cast(first)), None)(cast.column(filter)))

	/** Create a ''from'' clause of a subselect of a select with `outer` as its ''from'' clause, using the given
	  * relation `first` as the sole member of its (actual) ''from'' clause. The string literal with the name
	  * of the table is used as the alias in the [[net.noresttherein.oldsql.sql.RowProduct.As as]] clause,
	  * allowing easier access to the relation.  The result is a special kind of
	  * an artificial 'join' between the ''implicit'' portion (the ''from'' clause of the outer select, providing
	  * access to all its relations, without them appearing in the ''from'' clause of the generated select SQL)
	  * and the ''explicit'' portion (the relations which actually appear in the ''from'' clause of the generated
	  * subselect SQL), consisting of the relation `first` and any other, subsequently joined with the result.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyRowTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `outer` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] `first`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `fullFilter` DSL instead.
	  * @param outer a ''from'' clause containing the list of relations preceding `first`.
	  * @param first the first (and currently only) relation of the actual ''from'' clause of the created subselect,
	  *              using the `T[O] <: BaseMapping[S, O]` `Mapping` type.
	  * @param cast  an implicit witness providing proper type inference for the mapping of the last relation
	  *              and conversions of associated classes between instances parameterized with the more generic `R`
	  *              and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L Subselect R As N`.
	  */
	def apply[L <: NonEmptyRow, N <: Label, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (outer :L, first :StaticTable[N, R])
	         (implicit cast :BaseMappingSubject[R, T, S]) :L Subselect R As N =
		cast.back.alias(apply(outer, LastTable[T, S](cast(first)), Some(first.name))(True))



	private[sql] def apply[L <: NonEmptyRow, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :SingleBoolean[prefix.Generalized Subselect R]) :L Subselect R As A =
		new Subselect[prefix.type, R] with AbstractExpanded[prefix.type, R, S] {
			override val left :prefix.type = prefix
			override val last              = next
			override val aliasOpt          = asOpt
			override val condition         = cond
			override val outer             = left.self
			override val fullSize          = left.fullSize + 1
			override val parameterization  = left.parameterization.subselect(self)
			override def lastRelation      = last
			override lazy val withClause   = super.withClause

			override type Alias = A
			override type WithLeft[+F <: NonEmptyRow] = F Subselect R As A
			override type NoAliasCopy = left.type Subselect R
			override type Copy = left.type Subselect R As A

			override def narrow :left.type Subselect R As A = this.asInstanceOf[left.type Subselect R As A]

//			override type WithRight[T[O] <: MappingAt[O]] = prefix.type Subselect T

			override def withCondition(filter :SingleBoolean[Generalized]) =
				Subselect[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: NonEmptyRow](newLeft :F)(filter :SingleBoolean[newLeft.Generalized Subselect R]) =
				Subselect[F, R, S, A](newLeft, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				Subselect[left.type, R, S, N](left, last, Option(alias))(condition)

			override def generalizedExpansion[F <: NonEmptyRow] = PrefixOf.itself[F].expand[Subselect, R]
			override def expansion[F <: NonEmptyRow] = PrefixOf.itself[F].expand[Subselect, R].as[A]


			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.__) =
				withLeft(left.joinedWith(prefix, firstJoin))(condition)

			override def selectedFrom[F <: NonEmptyRow](prefix :F) =
				withLeft(left.selectedFrom(prefix))(condition)

			override def appendedTo[P <: FromClause](prefix :P) =
				withLeft(left.appendedTo(prefix))(condition)


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExpandedBy E) :LazyList[RelationSQL.from[E]#__] =
				LazyList.cons(last.expand(target).upcast, LazyList.empty[RelationSQL.from[E]#__])
//				last.expand(target) #:: LazyList.empty[RelationSQL.from[E]#__]


			override def visit[Y](visitor :RowProductVisitor[Y]) = visitor.subselect[L, R, S](this)

		}.asInstanceOf[L Subselect R As A]



	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Adjoin R) :Opt[(L, Table[R])] = from match {
		case subselect :Subselect[L @unchecked, R @unchecked] => Got(subselect.left -> subselect.right)
		case _ => Lack
	}

	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply(from :RowProduct) :Opt[(NonEmptyRow, Table.__)] = from match {
		case join :Subselect.__ => Got(join.left -> join.right)
		case _ => Lack
	}



	implicit def subselectComposition[L <: NonEmptyRow, R[O] <: MappingAt[O]]
			:ExpandedComposition[L, R, Subselect, NonEmptyRow, MappingAt]
				{ type Generalized[+A <: NonEmptyRow, B[O] <: MappingAt[O]] = A Subselect B } =
		composition.asInstanceOf[ExpandedComposition[L, R, Subselect, NonEmptyRow, MappingAt] {
			type Generalized[+A <: NonEmptyRow, B[O] <: MappingAt[O]] = A Subselect B
		}]

	private[this] val composition =
		new ExpandedComposition[NonEmptyRow, MappingAt, Subselect, NonEmptyRow, MappingAt] {
			override def apply[C <: NonEmptyRow](template :Subselect[NonEmptyRow, MappingAt], clause :C) =
				template.withLeft(clause)(True)
		}



	/** Type alias for `Subselect` with erased type parameters, covering all instances of `Subselect`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type __ = Subselect[_ <: NonEmptyRow, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound for all [[net.noresttherein.oldsql.sql.Subselect Subselect]] clauses
	  * starting with mapping `R` on their right side.
	  */
	type LUB[R[O] <: MappingAt[O]] = NonEmptyRow Subselect R

	/** A curried type constructor for `Subselect` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Left[ ]#Right
	type WithLeft[L <: NonEmptyRow] = { type F[R[O] <: MappingAt[O]] = L Subselect R }

	/** A curried type constructor for `Subselect` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */ //todo: rename to Right[ ]#Left
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: NonEmptyRow] = L Subselect R }
}

