package net.noresttherein.oldsql.sql


import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Relation.Table
import net.noresttherein.oldsql.sql.AndFrom.AndFromTemplate
import net.noresttherein.oldsql.sql.RowProduct.{As, ExtendedBy, NonEmptyFrom, NonEmptyFromTemplate, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.Extended.{AbstractExtended, ExtendedComposition, ExtendedDecomposition, NonSubselect}
import net.noresttherein.oldsql.sql.Join.{AbstractJoin, JoinComposition}
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject
import net.noresttherein.oldsql.sql.Compound.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.MappingSQL.{RelationSQL, TableSQL}
import net.noresttherein.oldsql.sql.ast.MappingSQL.TableSQL.LastTable
import net.noresttherein.oldsql.sql.ast.SQLTerm.True
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.{RowProductMatcher, SQLScribe}






/** Common upper type for `AndFrom` subclasses which accept arbitrary `Mapping` subtypes as joined relations.
  * It is the root of a tree with two branches: [[net.noresttherein.oldsql.sql.Join Join]] subtypes
  * which represent various actual join kinds (inner, outer, etc) and [[net.noresttherein.oldsql.sql.Subselect Subselect]]
  * which is a synthetic linearization of the ''from'' clause of a subselect joined with the ''from'' clause
  * of its outer select.
  */ //this class is almost unused; it seems to serve only the purpose of 'not join param'
sealed trait JoinLike[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonParam[L, R] with AndFromTemplate[L, R, L JoinLike R]
{ thisClause =>

	override val last :JoinedTable[RowProduct AndFrom R, R]
	override def right :Table[R] = last.table

	override def lastAsIn[E <: RowProduct](implicit extension :FromLast PrefixOf E) :Last[E] =
		last.asIn[E]


	override type Last[F <: RowProduct] = JoinedTable[F, LastMapping]

	override type Generalized >: Dealiased <: (left.Generalized JoinLike R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	type Dealiased >: Self <: (left.Self JoinLike R) {
		type Generalized = thisClause.Generalized
		type Params = thisClause.Params
		type FullRow = thisClause.FullRow
		type Explicit = thisClause.Explicit
		type Implicit = thisClause.Implicit
		type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
		type Row = thisClause.Row
		type OuterRow = thisClause.OuterRow
	}

	override type Self <: (left.Self JoinLike R) {
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

	protected override def narrow :left.type JoinLike R

	override type GeneralizedLeft[+F <: FromSome] = F GeneralizedJoin R
	override type DealiasedLeft[+F <: FromSome] = F LikeJoin R
//	override type WithLeft[+F <: FromSome] <: F LikeJoin R // <: because alias is kept
//	override type GeneralizedRight[T[O] <: MappingAt[O]] <: L JoinLike T


	/** The generalized version of this join kind: it is `Join` for all real joins and `Subselect` for `Subselect`. */
	type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: 
		(F JoinLike T) with NonEmptyFromTemplate[F GeneralizedJoin T, F GeneralizedJoin T]
	{
		type GeneralizedJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.GeneralizedJoin[S, M]
	}

	/** This join type, fully parameterized with arbitrary prefix clause and relation mapping. Used by copy constructors
	  * of this class.
	  * @see [[net.noresttherein.oldsql.sql.JoinLike.likeJoin]]
	  */
	type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <: 
		(F GeneralizedJoin T) with NonEmptyFromTemplate[F LikeJoin T, F LikeJoin T]
	{
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}



	protected[sql] final def aliasedJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                                    (left :F, right :LastTable[T, X], alias :Option[A])
	                                    (filter :GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T As A =
		likeJoin(left, right, alias)(filter)

	protected def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                      (left :F, right :LastTable[T, X], alias :Option[A])
	                      (filter :GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T As A

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join. It is the lower
	  * level variant of the other `likeJoin` method which accepts a `Relation[T]`
	  * @param left the ''from'' clause containing all relations preceding `right` in the new clause.
	  * @param right a `RelationSQL[RowProduct AndFrom T, T, S, RowProduct AndFrom T]` representing the last joined relation.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :LastTable[T, X])
	            (filter :GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		likeJoin(left, right, None)(filter)

	/** Creates a join of the same kind as this one between the `left` prefix clause and `right` relation given
	  * as parameters, using the provided `filter` as the `condition` stored in the created join.
	  */
	def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X]
	            (left :F, right :Table[T])(filter :GlobalBoolean[left.Generalized GeneralizedJoin T]) :F LikeJoin T =
		likeJoin(left, LastTable[T, X](right))(filter)

	/** Joins the two clauses using this join kind. If the first join of the second, `right` clause
	  * is not the `From` pseudo join, the existing join is used instead of this join.
	  * @throws UnsupportedOperationException if this clause is a `Subselect` and the right clause contains
	  *                                       a `JoinParam` synthetic join.
	  */
	def likeJoin[P <: FromSome, S <: FromSome](left :P, right :S) :right.JoinedWith[P, LikeJoin]


	override type LastParam = left.LastParam
	override type Params = left.Params
	override type DecoratedParamless[D <: BoundParamless] = D

	protected override def decoratedBind[D <: BoundParamless](params :Params)(decorate :Paramless => D) :D =
		decorate(bind(params))

	override type DefineBase[+I <: RowProduct] = I
	override def base :Base = outer

	override def generalizedExtension[P <: FromSome] :P PrefixOf (P GeneralizedJoin R) =
		PrefixOf.itself[P].extend[GeneralizedJoin, R]


//todo: def using(fk :R[FromLast] => ForeignKey[FromLast]) :L LikeJoin R

}






/** Factory and matching pattern for [[net.noresttherein.oldsql.sql.JoinLike JoinLike]] classes. */
object JoinLike {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using 
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `JoinLike` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :JoinLike[L @unchecked, R @unchecked] => Some((join.left, join.right))
		case _ => None
	}

	/** Matches all `JoinLike` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(RowProduct, Table.*)]  = from match {
		case join :JoinLike.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def joinLikeDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
			:ExtendedDecomposition[L JoinLike R, L, R, JoinLike, RowProduct] =
		decomposition.asInstanceOf[ExtendedDecomposition[L JoinLike R, L, R, JoinLike, RowProduct]]

	private[this] val decomposition =
		new ExtendedDecomposition[RowProduct JoinLike MappingAt, RowProduct, MappingAt, JoinLike, RowProduct]



	/** Type alias for `JoinLike` with erased type parameters, covering all instances of `JoinLike`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = JoinLike[_ <: RowProduct, T] forSome { type T[O] <: MappingAt[O] }

	/** A curried type constructor for `JoinLike` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L JoinLike R }

	/** A curried type constructor for `JoinLike` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L JoinLike R }

	type JoinWith[J[F <: FromSome, R[O] <: MappingAt[O]] <: F JoinLike R] = {
		type Left[L <: FromSome] = { type Right[R[O] <: MappingAt[O]] = L J R }
	}

}






/** Base trait for 'true' join implementations representing a real SQL join between relations, rather than a synthetic
  * `Subselect` representing a subselect of another select expression. Subclasses represent various join kinds
  * (inner, outer, left outer, right outer).
  * It is the [[net.noresttherein.oldsql.sql.RowProduct.Generalized generalized]] form of all extending classes, meaning
  * that all join conditions for subclasses use this type instead the proper concrete subclass in the `RowProduct`
  * parameter of the `GlobalBoolean`.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  */
sealed trait Join[+L <: FromSome, R[O] <: MappingAt[O]]
	extends JoinLike[L, R] with NonSubselect[L, R] with AndFromTemplate[L, R, L Join R]
{ thisClause =>

	override type Generalized = left.Generalized Join R
	override type Dealiased = left.Self LikeJoin R
	override type Self <: left.Self LikeJoin R

	protected override def narrow :WithLeft[left.type]

//	override type GeneralizedRight[T[O] <: MappingAt[O]] >: WithRight[T] <: L GeneralizedJoin T
//	override type WithRight[T[O] <: MappingAt[O]] <: L LikeJoin T
	override type GeneralizedJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F Join T

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] <:
		(F Join T) with NonEmptyFromTemplate[F LikeJoin T, F LikeJoin T]
	{
		type LikeJoin[+S <: FromSome, M[O] <: MappingAt[O]] = thisClause.LikeJoin[S, M]
	}

	override def likeJoin[P <: FromSome, S <: RowProduct](left :P, right :S) :right.JoinedWith[P, LikeJoin] =
		right.joinedWith(left, this)

	
	override def filter[E <: RowProduct](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
		left.filter(target)(extension.extendFront[left.Generalized, R]) && condition.basedOn(target)


	override type AppliedParam = left.AppliedParam LikeJoin R
	override type Paramless = left.Paramless LikeJoin R

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param, lastParamOffset)
		withLeft[l.type](l)(substitute(condition))
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		withLeft[l.type](l)(substitute(condition))
	}



	override type JoinedWith[+P <: RowProduct, +J[+K <: P, T[O] <: MappingAt[O]] <: K NonParam T] =
		WithLeft[left.JoinedWith[P, J]]

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]


	override type Explicit = left.Explicit Join R
	override type Inner = left.Inner LikeJoin R
	override type Implicit = left.Implicit
	override type Outer = left.Outer
	override type Row = left.Row ~ last.Subject
	override type OuterRow = left.OuterRow


	override type AsSubselectOf[+F <: NonEmptyFrom] = WithLeft[left.AsSubselectOf[F]]

	override def asSubselectOf[F <: NonEmptyFrom](newOuter :F)(implicit extension :Implicit ExtendedBy F)
			:AsSubselectOf[F] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
	{
		val newLeft = left.asSubselectOf(newOuter)
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		//  this would however introduce problem with JoinLike.as: one of the relation becoming unavailable
		val unfiltered = withLeft[newLeft.Generalized](newLeft.generalized)(True)
		val substitute = SQLScribe.shiftBack[Generalized, newLeft.Generalized Join R](
			generalized, unfiltered, extension.length, size + 1
		)
		withLeft[newLeft.type](newLeft)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Join.*]

}






object Join {


	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		InnerJoin(left, right, filter)


	/** Matches all `Join` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :Join[L @unchecked, R @unchecked] => Some((join.left, join.right))
		case _ => None
	}

	/** Matches all `Join` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Table.*)]  = from match {
		case join :Join.* => Some((join.left, join.right))
		case _ => None
	}



	@implicitNotFound("I do not know how to decompose ${F} into a join ${L} ${J} ${R}.\n" +
	                  "Missing implicit JoinComposition[${F}, ${L}, ${R}, ${J}].")
	class JoinComposition[L <: FromSome, R[O] <: MappingAt[O],
	                      J[+A <: FromSome, B[O] <: MappingAt[O]] <:
	                        (A Join B) { type LikeJoin[+X <: FromSome, Y[O] <: MappingAt[O]] <: X J Y }]
		extends ExtendedComposition[L J R, L, R, J, FromSome, MappingAt]
	{
		override type Generalized[+A <: FromSome, B[O] <: MappingAt[O]] = A Join B

		override def apply[C <: FromSome](template :L J R, left :C) :C J R = template.withLeft(left)(True)

		def apply[A <: FromSome, B[O] <: BaseMapping[S, O], S]
		         (template :L J R, left :A, right :Table[B])
		         (condition :GlobalBoolean[template.GeneralizedJoin[left.Generalized, B]]) :A J B =
			template.likeJoin[A, B, S](left, right)(condition)
	}


	implicit def joinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, Join] =
		composition.asInstanceOf[JoinComposition[L, R, Join]]

	private[this] val composition = new JoinComposition[FromSome, MappingAt, Join]



	/** Type alias for `Join` with erased type parameters, covering all instances of `Join`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Join[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Join` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L Join R }

	/** A curried type constructor for `Join` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L Join R }



	private[sql] trait AbstractJoin[L <: FromSome, R[O] <: BaseMapping[S, O], S]
		extends AbstractExtended[L, R, S] with Join[L, R]
	{ thisClause =>
		override val last :TableSQL[FromLast, R, S, FromLast]
		protected override def lastRelation :TableSQL[FromLast, R, S, FromLast] = last

		override type WithLeft[+F <: FromSome] = F LikeJoin R As Alias
//		override type GeneralizedRight[T[O] <: MappingAt[O]] = L GeneralizedJoin T
//		override type WithRight[T[O] <: MappingAt[O]] = L LikeJoin T
		override type Self = WithLeft[left.Self]

		override def withLeft[F <: FromSome](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized GeneralizedJoin R])
				:F LikeJoin R As Alias =
			likeJoin(newLeft, last, aliasOpt)(filter)

//		override def withRight[T[O] <: BaseMapping[X, O], X]
//		                      (table :LastTable[T, X])(filter :GlobalBoolean[L GeneralizedJoin T]) :L LikeJoin T =
//			likeJoin(left, table, None)(filter)


		override def extension[P <: FromSome] :P PrefixOf (P LikeJoin R As Alias) =
			PrefixOf.itself[P].extend[LikeJoin, R].as[Alias]

		override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) :JoinedWith[F, firstJoin.LikeJoin] =
			withLeft(left.joinedWith(prefix, firstJoin))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])

		override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) :JoinedWithSubselect[F] =
			withLeft(left.joinedWithSubselect(prefix))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])

		override def appendedTo[P <: FromClause](prefix :P) :JoinedWith[P, NonParam] =
			withLeft(left.appendedTo(prefix))(condition :GlobalBoolean[left.Generalized GeneralizedJoin R])


		protected override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.join[L, R, S](this)
	}

}






/** A standard join of two SQL relations. Represents a cross join of all relations from the left side with the relation
  * on the right, filtered by the conjunction of this join's `condition` with the combined filter expression
  * of the left side.
  */
sealed trait InnerJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L InnerJoin R]
{ thisClause =>

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F InnerJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :GlobalBoolean[left.Generalized Join T]) :F InnerJoin T As A =
		InnerJoin(left, right, alias)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[InnerJoin.*]

	override def name :String = "join"

}






object InnerJoin {

	/** A template `InnerJoin` instance with a dummy mapping, for use as a polymorphic factory of `InnerJoin` joins. */
	final val template :InnerJoin.* = InnerJoin(Relation.Dummy, Relation.Dummy)

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RB, B])
			:From[L] InnerJoin R =
		castR(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a cross join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.join join]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, InnerJoin, R, T, S]) :L InnerJoin R =
		cast(apply(left, LastTable[T, S](cast(right)), None)(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L InnerJoin R As A =
		new InnerJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type Alias = A
			override type DealiasedCopy = left.type InnerJoin R
			override type Copy = left.type InnerJoin R As A

			protected override def narrow :left.type InnerJoin R As A = this.asInstanceOf[left.type InnerJoin R As A]

			//needs to be private because the result is This
			override def withCondition(filter :GlobalBoolean[Generalized]) =
				InnerJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				InnerJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.innerJoin[L, R, S](this)

		}.asInstanceOf[L InnerJoin R As A]



	/** Matches all `InnerJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :InnerJoin[L @unchecked, R @unchecked] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `InnerJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Table.*)] = from match {
		case join :InnerJoin.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def innerJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, InnerJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, InnerJoin]]

	/** Type alias for `InnerJoin` with erased type parameters, covering all instances of `InnerJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = InnerJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `InnerJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L InnerJoin R }

	/** A curried type constructor for `InnerJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L InnerJoin R }

}






/** A symmetrical outer join of two (or more) SQL relations, that is one where every row in either of them occurs
  * at least once in the result, matched with an artificial row consisting of only null values from the other table
  * if no true match exist. The left side, which may consist of any number of relations greater than zero, is treated
  * as a single relation for this purpose. The join is filtered by the conjunction of this join's `condition`
  * with the combined filter expression of the left side.
  */
sealed trait OuterJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L OuterJoin R]
{ thisClause =>

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F OuterJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :GlobalBoolean[left.Generalized Join T]) :F OuterJoin T As A =
		OuterJoin(left, right, alias)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[OuterJoin.*]

	override def name :String = "outer join"

}






object OuterJoin {

	/** A template `OuterJoin` instance with a dummy mapping, for use as a polymorphic factory of `OuterJoin` joins. */
	final val template :OuterJoin.* = OuterJoin(Relation.Dummy, Relation.Dummy)

	/** Create an outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], OuterJoin, R, RB, B])
			:From[L] OuterJoin R =
		castR(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create an outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.outerJoin outerJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L InnerJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, OuterJoin, R, T, S]) :L OuterJoin R =
		cast(apply(left, LastTable[T, S](cast(right)), None)(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L OuterJoin R As A =
		new OuterJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type Alias = A
			override type DealiasedCopy = left.type OuterJoin R
			override type Copy = left.type OuterJoin R As A

			protected override def narrow :left.type OuterJoin R As A = this.asInstanceOf[left.type OuterJoin R As A]

			//needs to be private because the result is This
			override def withCondition(filter :GlobalBoolean[Generalized]) =
				OuterJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				OuterJoin[left.type, R, S, N](left, last, Option(alias))(condition)


			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.outerJoin[L, R, S](this)
		
		}.asInstanceOf[L OuterJoin R As A]



	/** Matches all `OuterJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :OuterJoin[L @unchecked, R @unchecked] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `OuterJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Table.*)] = from match {
		case join :OuterJoin.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def outerJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, OuterJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, OuterJoin]]



	/** Type alias for `OuterJoin` with erased type parameters, covering all instances of `OuterJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = OuterJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `OuterJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L OuterJoin R }

	/** A curried type constructor for `OuterJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L OuterJoin R }

}






/** A left outer join of two (or more) SQL relations, that is one where every row in the preceding (left) clause/tables
  * occurs at least once in the result, matched with an artificial row consisting of only null values from the right table
  * if no true match exist. The left side, which may consist of any number of relations greater than zero, is treated
  * as a single relation for this purpose. The join is filtered by the conjunction of this join's `condition`
  * with the combined filter expression of the left side.
  */ //consider type 'NotRightJoin' as the left side
sealed trait LeftJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L LeftJoin R]
{ thisClause =>

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F LeftJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :GlobalBoolean[left.Generalized Join T]) :F LeftJoin T As A =
		LeftJoin(left, right, alias)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[LeftJoin.*]

	override def name = "left join"

}






object LeftJoin {

	/** A template `LeftJoin` instance with a dummy mapping, for use as a polymorphic factory of `LeftJoin` joins. */
	final val template :LeftJoin.* = LeftJoin(Relation.Dummy, Relation.Dummy)

	/** Create a left outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], LeftJoin, R, RB, B])
			:From[L] LeftJoin R =
		castR(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a left outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.leftJoin leftJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L LeftJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, LeftJoin, R, T, S]) :L LeftJoin R =
		cast(apply(left, LastTable[T, S](cast(right)), None)(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L LeftJoin R As A =
		new LeftJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type Alias = A
			override type DealiasedCopy = left.type LeftJoin R
			override type Copy = left.type LeftJoin R As A

			protected override def narrow :left.type LeftJoin R As A = this.asInstanceOf[left.type LeftJoin R As A]

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				LeftJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				LeftJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.leftJoin[L, R, S](this)
			
		}.asInstanceOf[L LeftJoin R As A]



	/** Matches all `LeftJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :LeftJoin[L @unchecked, R @unchecked] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `LeftJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Table.*)] = from match {
		case join :LeftJoin.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def leftJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, LeftJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, LeftJoin]]



	/** Type alias for `LeftJoin` with erased type parameters, covering all instances of `LeftJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = LeftJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `LeftJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L LeftJoin R }

	/** A curried type constructor for `LeftJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L LeftJoin R }

}






/** A right outer join of two (or more) SQL relations, that is one where every row in the right table occurs
  * at least once in the result, matched with an artificial row consisting of only null values from the tables
  * of the left side if no true match exist. The left side, which may consist of any number of relations greater than
  * zero, is treated as a single relation for this purpose. The join is filtered by the conjunction of this join's
  * `condition` with the combined filter expression of the left side.
  */
sealed trait RightJoin[+L <: FromSome, R[O] <: MappingAt[O]]
	extends Join[L, R] with AndFromTemplate[L, R, L RightJoin R]
{ thisClause =>

	override type LikeJoin[+F <: FromSome, T[O] <: MappingAt[O]] = F RightJoin T

	protected override def likeJoin[F <: FromSome, T[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :F, right :LastTable[T, X], alias :Option[A])
	                               (filter :GlobalBoolean[left.Generalized Join T]) :F RightJoin T As A =
		RightJoin(left, right, alias)(filter)


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[RightJoin.*]

	override def name = "right join"

}






object RightJoin {

	/** A template `RightJoin` instance with a dummy mapping, used as a polymorphic factory of `RightJoin` joins.  */
	final val template : RightJoin.* = RightJoin(Relation.Dummy, Relation.Dummy)


	/** Create a right outer join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
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
	         (implicit castL :JoinedRelationSubject[From, L, LA, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], RightJoin, R, RB, B])
			:From[L] RightJoin R =
		castR(apply(From(left), LastTable[RB, B](castR(right)), None)(True))

	/** Create a right outer join between the `left` side, given as a non empty clause/list of relations,
	  * and the the `right` relation representing the last joined table, relation or some temporary surrogate mapping.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `left` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.rightJoin rightJoin]] `right`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `filter` DSL instead.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause, using the `T[O] <: BaseMapping[S, O]`
	  *              `Mapping` type.
	  * @param filter an optional join condition narrowing the join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L RightJoin R`.
	  */
	def apply[L <: FromSome, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Table[R], filter :GlobalBoolean[L#Generalized Join R] = True)
	         (implicit cast :InferSubject[L, RightJoin, R, T, S]) :L RightJoin R =
		cast(apply(left, LastTable[T, S](cast(right)), None)(cast(filter)))



	private[sql] def apply[L <: FromSome, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :GlobalBoolean[prefix.Generalized Join R]) :L RightJoin R As A =
		new RightJoin[prefix.type, R] with AbstractJoin[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.outer
			override val fullSize = left.fullSize + 1

			override type Alias = A
			override type DealiasedCopy = left.type RightJoin R
			override type Copy = left.type RightJoin R As A

			protected override def narrow = this.asInstanceOf[left.type RightJoin R As A]

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				RightJoin[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) = 
				RightJoin[left.type, R, S, N](left, last, Option(alias))(condition)

			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.rightJoin[L, R, S](this)
			
		}.asInstanceOf[L RightJoin R As A]



	/** Matches all `RightJoin` instances, splitting them into their left (all relations but the last one)
	  * and right (the last relation) sides.
	  */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case join :RightJoin[L @unchecked, R @unchecked] => Some(join.left -> join.right)
		case _ => None
	}

	/** Matches all `RightJoin` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :RowProduct) :Option[(FromSome, Table.*)] = from match {
		case join :RightJoin.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def rightJoinComposition[L <: FromSome, R[O] <: MappingAt[O]] :JoinComposition[L, R, RightJoin] =
		Join.joinComposition.asInstanceOf[JoinComposition[L, R, RightJoin]]



	/** Type alias for `RightJoin` with erased type parameters, covering all instances of `RightJoin`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = RightJoin[_ <: FromSome, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `RightJoin` instances, accepting the left `FromSome` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: FromSome] = { type F[R[O] <: MappingAt[O]] = L RightJoin R }

	/** A curried type constructor for `RightJoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromSome` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromSome] = L RightJoin R }

}






/** A special artificial join type serving as a ''from'' clause for subselects (selects occurring inside another select,
  * either in the ''select'' or ''where'' clause). It has direct access to all tables from the outer clause,
  * while providing its own additional relations in its (explicit) ''from'' clause. It is represented as a join between
  * the outer clause (the left side), forming the 'implicit' portion of this clause, and the first member
  * of the subselect's ''from'' list (the right side), starting the 'explicit' portion of this clause.
  * Joining additional relations has the effect of adding them to that 'explicit' portion, available only to the
  * subselect (and, potentially, its own subselects). This allows any SQL expressions based on the outer select's clause
  * to be used as expressions based on this select, as it doesn't differ from any other `RowProduct` join-like extension.
  * Note that it is possible to recursively nest subselects to an arbitrary depth, modelled by a repeated use
  * of this join type. In that case all relations/mappings to the left of the first occurrence of `Subselect`
  * in the type definition are the ''from'' clause of the most outer, independent select, while relations/mappings
  * in between subsequent `Subselect`s form the ''from'' clauses of subsequent nested selects,
  * with any relations/mappings following its last occurrence in the type being exclusive to the deepest subselect
  * in the chain.
  *
  * This clause type is referred to in the documentation almost exclusively as a 'subselect join', or `Subselect` join,
  * while `RowProduct` subtypes which contain a `Subselect` anywhere in their (dynamic) type definition are called
  * ''subselect'' clauses, regardless of whether `Subselect` is the last 'join' or occurs before. The latter term
  * is also sometimes used for the list of relations from between two subselect boundaries, in particular
  * for all relations to the right of the last `Subselect` join. This does not take into account
  * ''selects'' occurring inside a ''from'' clause as a relation, as they are independent expressions, with none
  * having access to the ''from'' clause relations of the other. On the other hand, clauses without a `Subselect`
  * in their concrete definition are most often called ''outer'' clauses, after the independent,
  * 'outer' select expressions which can be based on them. All subselect clauses conform to
  * [[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom SubselectFrom]], providing the type is instantiated at least
  * to the point of the last `Subselect` and no
  * [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]/[[net.noresttherein.oldsql.sql.GroupParam GroupParam]] join
  * is present to its right. Additionally, direct subselect clauses of some outer clause `F` (that is,
  * those without any `Subselect` to the right of `F` in their type definition) conform to
  * [[net.noresttherein.oldsql.sql.RowProduct.DirectSubselect F.DirectSubselect]] if both clauses are at least in their generalized form.
  * Outer clauses are also valid ''from'' clauses for a subselect of any other select, but do not conform to any
  * of the above, thus this relationship is typically established instead
  * by the [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf SubselectOf[F] ]] type alias, which covers
  * both independent clauses and actual subselects (but not those of an indirect subselect,
  * that is with another occurrence of `Subselect` to the right of `F`). Note that while it is possible to construct
  * a `RowProduct` instance with a [[net.noresttherein.oldsql.sql.JoinParam JoinParam]]
  * (or [[net.noresttherein.oldsql.sql.GroupParam GroupParam]]) to the right of a subselect join, such a clause
  * will be invalid: it will conform to neither of the types listed above, representing the relation of being
  * a subselect clause, which will make it impossible to construct
  * a [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] based on it.
  *
  * @tparam F the type of outer select's ''from'' clause.
  * @tparam T the right side of this join - the first relation of the ''from'' clause of the represented subselect.
  *
  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectFrom]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.SubselectOf]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.TopFrom]]
  */ //consider :renaming to SelectFrom. Could be used as a join between With clause and first select. Then DirectSubselect can be Subselect
sealed trait Subselect[+F <: NonEmptyFrom, T[O] <: MappingAt[O]]
	extends JoinLike[F, T] with AndFromTemplate[F, T, F Subselect T]
{ thisClause =>

	override type Generalized = left.Generalized Subselect T
	override type Dealiased = left.Self Subselect T
	override type Self <: left.Self Subselect T

	//widened bounds
	override type WithLeft[+L <: NonEmptyFrom] <: L Subselect T
//	override type GeneralizedRight[R[O] <: MappingAt[O]] = WithRight[R] //<: F Subselect R
//	override type WithRight[R[O] <: MappingAt[O]] <: F Subselect R
	override type GeneralizedJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R
	override type LikeJoin[+L <: FromSome, R[O] <: MappingAt[O]] = L Subselect R

	override def withLeft[L <: NonEmptyFrom](left :L)(filter :GlobalBoolean[left.Generalized Subselect T]) :WithLeft[L] //wider bound

//	override def withRight[R[O] <: BaseMapping[S, O], S]
//	                      (table :LastTable[R, S])(filter :GlobalBoolean[WithRight[R]]) :WithRight[R] =
//		likeJoin(left, table, None)(filter)

	protected override def likeJoin[L <: NonEmptyFrom, R[O] <: BaseMapping[X, O], X, A <: Label]
	                               (left :L, right :LastTable[R, X], alias :Option[A])
	                               (filter :GlobalBoolean[left.Generalized Subselect R]) :L Subselect R As A =
		Subselect(left, right, alias)(filter)

	override def likeJoin[L <: NonEmptyFrom, R[O] <: BaseMapping[X, O], X] //wider bound
	                     (left :L, right :Table[R])(filter :GlobalBoolean[left.Generalized Subselect R]) :L Subselect R =
		Subselect(left, LastTable[R, X](right), None)(filter)

	override def likeJoin[P <: NonEmptyFrom, S <: FromSome](left :P, right :S) :right.JoinedWithSubselect[P] =
		right.joinedWithSubselect(left)


	override type AppliedParam = WithLeft[left.AppliedParam]
	override type Paramless = WithLeft[left.Paramless]

	override def bind(param :LastParam) :AppliedParam = {
		val l = left.bind(param)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParam(self, unfiltered.generalized, param, lastParamOffset)
		withLeft[l.type](l)(substitute(condition))
	}

	override def bind(params :Params) :Paramless = {
		val l = left.bind(params)
		val unfiltered = withLeft[l.type](l)(True)
		val substitute = SQLScribe.applyParams(self, unfiltered.generalized)(params)
		withLeft[l.type](l)(substitute(condition))
	}


	override def isSubselectParameterized = false
	override def isSubselect = true //if it will accept Dual as the left side the standard definition in RowProduct must be changed.
	override def isValidSubselect = true

	override type Explicit = RowProduct AndFrom T
	override type Inner = NonEmptyFrom Subselect T
	override type Implicit = left.Generalized
	override type Outer = left.Self



	override def filter[E <: RowProduct](target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
		condition.basedOn(target)


	override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
		WithLeft[left.JoinedWith[P, J]]

	override type JoinedWithSubselect[+P <: NonEmptyFrom] = WithLeft[left.JoinedWithSubselect[P]]



	override type Row = @~ ~ last.Subject

	override def row[E <: RowProduct]
	             (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, @~ ~ last.Subject] =
		ChainTuple.EmptyChain ~ last.extend(target)(extension, implicitly[GlobalScope <:< GlobalScope])


	override type OuterRow = left.FullRow

	override def outerRow[E <: RowProduct]
	             (target :E)(implicit extension :Implicit ExtendedBy E) :ChainTuple[E, GlobalScope, left.FullRow] =
		left.fullRow(target)



	override type AsSubselectOf[+O <: NonEmptyFrom] = WithLeft[O]

	override def asSubselectOf[O <: NonEmptyFrom](newOuter :O)(implicit extension :Implicit ExtendedBy O)
			:WithLeft[newOuter.type] =
	{
		//todo: refactor joins so they take functions creating conditions and move this to the constructor
		val unfiltered = withLeft[newOuter.type](newOuter)(True)
		val substitute = SQLScribe.shiftBack(generalized, unfiltered.generalized, extension.length, 1)
		withLeft[newOuter.type](newOuter)(substitute(condition))
	}



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Subselect.*]

	override def name = "subselect"

}






object Subselect {

	/** A template `Subselect` instance with a dummy mapping, for use as a polymorphic factory of `Subselect` joins. */
	final val template :Subselect.* = Subselect(From(Relation.Dummy), Relation.Dummy)


	/** Create a ''from'' clause of a subselect of a select with `outer` as its ''from'' clause, using the given
	  * relation `first` as the sole member of its (actual) ''from'' clause. The result is a special kind of
	  * an artificial 'join' between the ''implicit'' portion (the ''from'' clause of the outer select, providing
	  * access to all its relations, without them appearing in the ''from'' clause of the generated select SQL)
	  * and the ''explicit'' portion (the relations which actually appear in the ''from'' clause of the generated
	  * subselect SQL), consisting of the relation `first` and any other, subsequently joined with the result.
	  * The ''where'' clause can be subsequently specified using
	  * the [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.on on]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.NonEmptyFromTemplate.whereLast whereLast]],
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] or
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] method. It is a lower level method;
	  * it is generally recommended to use
	  * `outer` [[net.noresttherein.oldsql.sql.FromSome.FromSomeExtension.subselect subselect]] `first`
	  * [[net.noresttherein.oldsql.sql.RowProduct.RowProductTemplate.where where]] `fullFilter` DSL instead.
	  * @param outer a ''from'' clause containing the list of relations preceding `first`.
	  * @param first the first (and currently only) relation of the actual ''from'' clause of the created subselect,
	  *              using the `T[O] <: BaseMapping[S, O]` `Mapping` type.
	  * @param filter an optional join condition linking the subselect with the outer clause.
	  * @param cast an implicit witness providing proper type inference for the mapping of the last relation
	  *             and conversions of associated classes between instances parameterized with the more generic `R`
	  *             and its narrowed down form of `T` with the required upper bound of `BaseMapping`.
	  * @return an `L Subselect R`.
	  */
	def apply[L <: NonEmptyFrom, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (outer :L, first :Table[R], filter :GlobalBoolean[L#Generalized Subselect R] = True)
	         (implicit cast :InferSubject[L, Subselect, R, T, S]) :L Subselect R =
		cast(Subselect[L, T, S, Nothing](outer, LastTable[T, S](cast(first)), None)(cast(filter)))



	private[sql] def apply[L <: NonEmptyFrom, R[O] <: BaseMapping[S, O], S, A <: Label]
	                      (prefix :L, next :LastTable[R, S], asOpt :Option[A])
	                      (cond :GlobalBoolean[prefix.Generalized Subselect R]) :L Subselect R As A =
		new Subselect[prefix.type, R] with AbstractExtended[prefix.type, R, S] {
			override val left = prefix
			override val last = next
			override val aliasOpt = asOpt
			override val condition = cond
			override val outer = left.self
			override val fullSize = left.fullSize + 1
			override def lastRelation = last

			override type Alias = A
			override type WithLeft[+F <: NonEmptyFrom] = F Subselect R As A
			override type DealiasedCopy = left.type Subselect R
			override type Copy = left.type Subselect R As A

			override def narrow :left.type Subselect R As A = this.asInstanceOf[left.type Subselect R As A]

//			override type WithRight[T[O] <: MappingAt[O]] = prefix.type Subselect T

			override def withCondition(filter :GlobalBoolean[Generalized]) =
				Subselect[left.type, R, S, A](left, last, aliasOpt)(filter)

			override def withLeft[F <: NonEmptyFrom](newLeft :F)(filter :GlobalBoolean[newLeft.Generalized Subselect R]) =
				Subselect[F, R, S, A](newLeft, last, aliasOpt)(filter)

			override def aliased[N <: Label](alias :N) =
				Subselect[left.type, R, S, N](left, last, Option(alias))(condition)

			override def extension[F <: NonEmptyFrom] = PrefixOf.itself[F].extend[Subselect, R].as[A]


			override def joinedWith[F <: FromSome](prefix :F, firstJoin :Join.*) =
				withLeft(left.joinedWith(prefix, firstJoin))(condition :GlobalBoolean[left.Generalized Subselect R])

			override def joinedWithSubselect[F <: NonEmptyFrom](prefix :F) =
				withLeft(left.joinedWithSubselect(prefix))(condition :GlobalBoolean[left.Generalized Subselect R])

			override def appendedTo[P <: FromClause](prefix :P) =
				withLeft(left.appendedTo(prefix))(condition :GlobalBoolean[left.Generalized Subselect R])


			override def tableStack[E <: RowProduct]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.extend(target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def matchWith[Y](matcher :RowProductMatcher[Y]) :Option[Y] = matcher.subselect[L, R, S](this)

		}.asInstanceOf[L Subselect R As A]



	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply[L <: RowProduct, R[O] <: MappingAt[O]](from :L Compound R) :Option[(L, Table[R])] = from match {
		case subselect :Subselect[L @unchecked, R @unchecked] => Some(subselect.left -> subselect.right)
		case _ => None
	}

	/** Matches all `Subselect` instances, splitting them into their left (implicit) and right (explicit) sides. */
	def unapply(from :RowProduct) :Option[(NonEmptyFrom, Table.*)] = from match {
		case join :Subselect.* => Some(join.left -> join.right)
		case _ => None
	}



	implicit def subselectComposition[L <: NonEmptyFrom, R[O] <: MappingAt[O]]
			:ExtendedComposition[L Subselect R, L, R, Subselect, NonEmptyFrom, MappingAt]
				{ type Generalized[+A <: NonEmptyFrom, B[O] <: MappingAt[O]] = A Subselect B } =
		composition.asInstanceOf[ExtendedComposition[L Subselect R, L, R, Subselect, NonEmptyFrom, MappingAt] {
			type Generalized[+A <: NonEmptyFrom, B[O] <: MappingAt[O]] = A Subselect B
		}]

	private[this] val composition =
		new ExtendedComposition[NonEmptyFrom Subselect MappingAt, NonEmptyFrom, MappingAt, Subselect, NonEmptyFrom, MappingAt] {
			override def apply[C <: NonEmptyFrom](template :Subselect[NonEmptyFrom, MappingAt], clause :C) =
				template.withLeft(clause)(True)
		}



	/** Type alias for `Subselect` with erased type parameters, covering all instances of `Subselect`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = Subselect[_ <: NonEmptyFrom, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `Subselect` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: NonEmptyFrom] = { type F[R[O] <: MappingAt[O]] = L Subselect R }

	/** A curried type constructor for `Subselect` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: NonEmptyFrom] = L Subselect R }

}

