package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.{BaseMapping, Relation}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.AndFrom.JoinedRelationSubject
import net.noresttherein.oldsql.sql.AndFrom.JoinedRelationSubject.InferSubject
import net.noresttherein.oldsql.sql.Extended.{BaseExtended, ExtendedComposition, NonSubselect}
import net.noresttherein.oldsql.sql.FromClause.{As, ClauseComposition, ExtendedBy, FromSome, PrefixOf, UngroupedFrom}
import net.noresttherein.oldsql.sql.Join.JoinWith
import net.noresttherein.oldsql.sql.MappingSQL.{BaseComponentSQL, RelationSQL}
import net.noresttherein.oldsql.sql.MappingSQL.RelationSQL.LastRelation
import net.noresttherein.oldsql.sql.SQLScribe.{ReplaceRelation, SubstituteComponents}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple.EmptyChain






/** A join between an existing [[net.noresttherein.oldsql.sql.FromClause FromClause]], representing a relation
  * or a joined list of relations, and another mapping. It is the root of a hierarchy of classes representing
  * ungrouped ''from'' clauses (i.e., without a ''group by'' clause). Subclasses exist for various SQL join kinds
  * (inner, outer, left, right), clauses for subselects nested under other clauses, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type. Note however that for most functions
  * to work properly, more specific type than `AndFrom` may be needed, typically
  * the [[net.noresttherein.oldsql.sql.FromClause#Generalized Generalized]] form of this clause.
  *
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point. This is the root of the class hierarchy of non-empty
  * ''from'' clauses: this includes both [[net.noresttherein.oldsql.sql.TrueJoin true joins]]
  * (inner, outer, left outer, right outer), synthetic combined ''from'' clauses of a
  * [[net.noresttherein.oldsql.sql.Subselect subselect]] and its outer select, as well as non-SQL sources of values
  * used in SQL select statements, such as statement [[net.noresttherein.oldsql.sql.JoinParam parameters]].
  *
  * Note that, as with all generic types taking exactly two arguments, it can be written in the infix notation:
  * `val usersGuns :From[Users] Join UserGuns Join Guns`. This class is covariant regarding its left side,
  * so a sequence of joined mappings `X0 J1 X1 J2 X2 .. JN XN &lt;: X0 Join X1 Join X2 ... Join XN`
  * if for all `JN &lt;: AndFrom`.
  *
  * @tparam L the left side of this join: a `FromClause` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause.
  * @see [[net.noresttherein.oldsql.sql.InnerJoin]]
  * @see [[net.noresttherein.oldsql.sql.OuterJoin]]
  * @see [[net.noresttherein.oldsql.sql.LeftJoin]]
  * @see [[net.noresttherein.oldsql.sql.RightJoin]]
  * @see [[net.noresttherein.oldsql.sql.From]]
  * @see [[net.noresttherein.oldsql.sql.Subselect]]
  * @see [[net.noresttherein.oldsql.sql.JoinParam]]
  */
trait AndFrom[+L <: FromClause, R[O] <: MappingAt[O]] extends FromSome with Extended[L, R] { thisClause =>

	override type FromLast = FromClause AndFrom R

	/** The `Generalized` type with the `FromClause` of the left side substituted for `F`. */
	type GeneralizedLeft[+F <: FromSome] <: (F AndFrom R) {
		type GeneralizedLeft[+S <: FromSome] <: thisClause.GeneralizedLeft[S]
	}

	/** This type with the `FromClause` of the left side substituted for `F`. */
	type WithLeft[+F <: FromSome] <: GeneralizedLeft[F] {
		type WithLeft[+S <: FromSome] <: thisClause.WithLeft[S]
	}


	/** A join of the same kind as this clause, but with the left clause substituted for `left`. */
	def withLeft[F <: FromSome](left :F)(filter :SQLBoolean[GeneralizedLeft[left.Generalized]]) :WithLeft[F]

	override type Generalized >: Self <: (left.Generalized AndFrom R) {
		type Generalized <: thisClause.Generalized
		type Explicit <: thisClause.Explicit
		type Implicit <: thisClause.Implicit
	}

	override type Self <: (left.Self AndFrom R) {
		type Generalized = thisClause.Generalized
		type Self = thisClause.Self
		type Explicit = thisClause.Explicit
		type Inner = thisClause.Inner
		type Implicit = thisClause.Implicit
		type Outer = thisClause.Outer
	}

	override type This >: this.type <: L AndFrom R


	/** A proof that the generalized form of this type extends its left side.
	  * Used as evidence required by some implicits.
	  */
	def generalizedExtension[P <: FromSome] :P PrefixOf GeneralizedLeft[P]

	/** A proof that this type extends its left side. Used as evidence required by some implicits. */
	def extension[P <: FromSome] :P PrefixOf WithLeft[P]

	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `FromClause`, as they become proper path types instead of projections.
	  */
	protected override def narrow :left.type AndFrom R



	/** Apply a join condition to the last two relations in this clause. The condition is combined using `&&` with
	  * `this.condition` and becomes a part of `this.filter` representing the ''where'' clause of the SQL statement.
	  * This works exactly like 'where', but instead of a single argument representing all joined relations,
	  * the filter function should take as its arguments the last two relations, i.e, the last relation defined
	  * by the left side of this join, if any, and the right side of this join.
	  * This method is only possible to call if the left side of this join can be statically determined
	  * to be a [[net.noresttherein.oldsql.sql.FromClause.FromSome FromSome]] subtype, i.e. a non-empty from clause.
	  * Its signature then takes the form of
	  * {{{
	  *     def on(condition :(JoinedRelation[GeneralizedLeft[left.FromLast], left.LastMapping],
	  *                        JoinedRelation[FromLast, R]) => SQLBoolean[Generalized]) :This
	  * }}}
	  * For example, for the type `From[L] InnerJoin R` it takes the form of
	  * {{{
	  *     def on(condition :(JoinedRelation[FromClause AndFrom L TrueJoin R, L],
	  *                        JoinedRelation[FromClause AndFrom R, R])
	  *                       => SQLBoolean[FromClause AndFrom L TrueJoin R]) :This
	  * }}}
	  * If the left type is too abstract, the type of `condition` argument will likewise be abstract, preventing
	  * the caller from providing a value; in turn, `Dual` defines it simply as `Nothing`.
	  * @param condition a function accepting the expressions for the last two relations in this clause and creating
	  *                  an SQL expression for the join condition.
	  * @return a `Join` instance of the same kind as this one, with the same left and right sides,
	  *         but with the join condition being the conjunction of this join's condition and the `SQLBoolean`
	  *         returned by the passed filter function.
	  */
	def on(condition :left.JoinFilter[GeneralizedLeft, FromLast, Generalized, R]) :This =
		left.filterNext[this.type, R](this)(condition)



	override def canEqual(that :Any) :Boolean = that.isInstanceOf[AndFrom.*]

}






/** A matching pattern and factory for ''from'' clauses of SQL SELECT statements representing non-empty list of tables
  * joined together.
  */
object AndFrom {

	/** Create an (inner) cross join between the given two relations `left` and `right`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method.
	  * @param left the first relation of the ''from'' clause.
	  * @param right the second relation of the ''from'' clause.
	  */
	def apply[L[O] <: MappingAt[O], LG[O] <: BaseMapping[A, O], A,
		      R[O] <: MappingAt[O], RG[O] <: BaseMapping[B, O], B]
	         (left :Relation[L], right :Relation[R])
	         (implicit castL :JoinedRelationSubject[From, L, LG, MappingOf[A]#TypedProjection],
	                   castR :InferSubject[From[L], InnerJoin, R, RG, B])
			:From[L] InnerJoin R =
		InnerJoin(left, right)


	/** Create a ''from'' clause extending the `left` clause with the relation `right` for mapping `R`.
	  * The ''where'' clause can be subsequently specified using the [[net.noresttherein.oldsql.sql.Join#on on]] or
	  * [[net.noresttherein.oldsql.sql.FromClause#where where]] method.
	  * This method will create a [[net.noresttherein.oldsql.sql.From From[R]]] instance if `left` is empty (`Dual`),
	  * or an [[net.noresttherein.oldsql.sql.InnerJoin L InnerJoin R]] otherwise. This method's use is somewhat limited
	  * as the result type of `L AndFrom R` is too abstract (its `Generalized` form is undefined) for many purposes.
	  * Prefer using the factory methods of `FromClause` and, in particular, its extension methods from
	  * [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]], such as `join`, `leftJoin`, etc.
	  * @param left a ''from'' clause containing the list of relations preceding `right`.
	  * @param right the last relation of the created ''from'' clause.
	  * @param filter an optional join condition narrowing the cross join; can be used as either the ''on'' or ''where''
	  *               clause in the generated SQL.
	  * @param cast implicit witness providing type inference of the subject type of the mapping `R` (and `T`).
	  */
	def apply[L <: UngroupedFrom, R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S]
	         (left :L, right :Relation[R], filter :SQLBoolean[L#Generalized AndFrom R] = True)
	         (implicit cast :JoinedRelationSubject[WithLeft[L]#F, R, T, MappingOf[S]#TypedProjection]) :L AndFrom R =
		cast(left.extend[T, S](LastRelation[T, S](cast(right)), cast.downtype[WithLeft[L#Generalized]#F, Boolean](filter)))



	/** Splits any `AndFrom` into its left (all relations but the last one) and right (the last relation) sides. */
	def unapply[L <: FromClause, R[O] <: MappingAt[O]](join :L Using R) :Option[(L, Relation[R])] = join match {
		case _ :AndFrom[_, _] => Some((join.left, join.right))
		case _ => None
	}

	/** Matches all `AndFrom` subclasses, extracting their `left` and `right` sides in the process. */
	def unapply(from :FromClause) :Option[(FromClause, Relation.*)] = from match {
		case join :AndFrom.* => Some((join.left, join.right))
		case _ => None
	}



	implicit def andFromDecomposition[L <: FromClause, R[O] <: MappingAt[O]]
			:ExtendedComposition[L AndFrom R, L, R, AndFrom, FromClause] =
		decomposition.asInstanceOf[ExtendedComposition[L AndFrom R, L, R, AndFrom, FromClause]]

	private[this] val decomposition =
		new ExtendedComposition[FromClause AndFrom MappingAt, FromClause, MappingAt, AndFrom, FromClause]



	/** An SQL expression rewriter shifting back references to all relations before the last `Subselect` join
	  * by `extension` positions. Used when a subselect clause is 'transplanted' onto another clause,
	  * extending the `Implicit` clause of the subselect.
	  * @param old a subselect clause serving as SQL expression base.
	  * @param extending a new subselect clause with some additional relations inserted between `F#Implicit`
	  *                  and the mapping joined in with a `Subselect` join.
	  * @param extension the difference in relations number between `F` and `G`.
	  * @param subselectSize number of relations in the explicit ''from'' clause of subselects `F` and `G`.
	  */
	private[sql] def shiftBack[F <: FromClause, G <: FromClause]
	                          (old :F, extending :G, extension :Int, subselectSize :Int) :SQLScribe[F, G] =
		new SubstituteComponents[F, G] {
			protected[this] override val oldClause = old
			protected[this] override val newClause = extending

			private[this] val relations = extending.tableStack.to(Array)

			override def relation[T[A] <: BaseMapping[E, A], E, O >: F <: FromClause](e :RelationSQL[F, T, E, O])
					:BaseComponentSQL[G, M, T, _ >: G <: FromClause] forSome { type M[A] <: MappingAt[A] } =
//				(if (e.shift < subselectSize) e //todo: we must ensure we are reusing the mapping instance
//				 else RelationSQL[G, T, E, G](e.relation, e.shift + extension)).asInstanceOf[RelationSQL[G, T, E, G]]
				(if (e.shift < subselectSize) e.asInstanceOf[RelationSQL[G, T, E, G]]
				 else relations(e.shift + extension).asInstanceOf[RelationSQL[G, T, E, G]]).asInstanceOf[RelationSQL[G, T, E, G]]


			protected override def extended[S <: FromClause, N <: FromClause]
			                               (subselect :S, replacement :N)
			                               (implicit oldExt :oldClause.Generalized ExtendedBy S,
			                                         newExt :newClause.Generalized ExtendedBy N) =
				shiftBack[S, N](subselect, replacement, extension, subselectSize + oldExt.length)
		}






	/** An existential upper bound of all `AndFrom` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type * = AndFrom[_ <: FromClause, M] forSome { type M[O] <: MappingAt[O] }

	/** A curried type constructor for `AndFrom` instances, accepting the left `FromClause` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithLeft[L <: FromClause] = { type F[R[O] <: MappingAt[O]] = L AndFrom R }

	/** A curried type constructor for `AndFrom` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `FromClause` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `FromClause` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: FromClause] = L AndFrom R }






	/** An implicit witness serving as type inference helper for the `Mapping` type of a joined
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]. It separates the inference of the mapping type itself
	  * from its `Subject` type, thus allowing for both to function properly in methods with the type parameters
	  * in the form of `[R &lt;: BaseMapping[S, _], S]` (and similar). It is used in particular when constructing new
	  * `Join` instances to remove explicit type parameters from the call. Apart from this purely independent role,
	  * it provides identity casting methods, similar to those of `=:=`, for various relevant types
	  * (`SQLExpression`, `Relation`, `F`) between those parameterized with `R` and `T` to eliminate the need
	  * for casting inside methods using this support.
	  * @tparam F a type constructor for a `AndFrom` 'join' extending some ''from'' clause with the provided mapping type.
	  * @tparam R a type constructor for a `Mapping` accepting its `Origin` type; it is the 'input' type used by the
	  *           arguments of the method accepting this instance, declared only with the `MappingAt[O]` upper bound.
	  * @tparam T the type `R`, but with the upper bound of `U`. It generally should not be used in the method signature
	  *           (in particular, it cannot appear before this argument), but can be used in the method's body.
	  *           As the type argument `U` is generally a concrete type, the type `R` is typically a type parameter
	  *           with an explicit upper bound consistent with `U`, such as `R[O] &lt;: BaseMapping[S, O]`.
	  * @tparam U an upper bound for the mapping type `R` (and `T`) required for the implementation.
	  *           It is a concrete type with only its type parameters being abstract, typically given as
	  *           `BaseMapping[S, O]`, where `S` is a free type parameter of the method. It allows the inference
	  *           of its (and, by extension, `R`'s) type parameters (`S`).
	  */ //todo: move this to Using/Extended
	@implicitNotFound("Failed to infer the Subject type of mapping ${R}: cannot prove that " +
		              "${R}[O] <: ${T}[O] with ${U}. This may be caused by the inferred type ${T} or its subject type S " +
	                  "occurring before the implicit parameter JoinedRelationSubject[${F}, ${R}, ${T}, ${U}] " +
	                  "(alias InferSubject[?, ?, ${R}, ${T}, ?]) or in the method's result type.")
	sealed abstract class JoinedRelationSubject[F[M[O] <: MappingAt[O]] <: FromClause,
	                                            R[O] <: MappingAt[O], T[O] <: U[O], +U[O] <: BaseMapping[_, O]]
		extends (F[T] => F[R])
	{
		def apply(rows :Relation[R]) :Relation[T]

		def apply(join :F[T]) :F[R]

		def downtype[E[M[O] <: MappingAt[O]] <: FromClause, X](e :SQLExpression[E[R], X]) :SQLExpression[E[T], X]

		def downtype[E[M[O] <: MappingAt[O]] <: FromClause, X](e :ColumnSQL[E[R], X]) :ColumnSQL[E[T], X]

		def apply[L <: FromSome, J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
                 (e :ColumnSQL[L J R, X]) :ColumnSQL[L J T, X]

		def apply[L <: FromSome, J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
		         (e :SQLExpression[L J R, X]) :SQLExpression[L J T, X]

		def self :JoinedRelationSubject[F, T, T, U]
	}



	object JoinedRelationSubject {
		import BaseMapping.AnyAt

		private[this] val instance =
			new JoinedRelationSubject[Using.WithLeft[FromClause]#F, AnyAt, AnyAt, AnyAt] {
				override def apply(rows :Relation[AnyAt]) = rows

				override def apply(join :FromClause Using AnyAt) = join

				override def downtype[F[M[O] <: MappingAt[O]] <: FromClause, X](e :ColumnSQL[F[AnyAt], X]) = e
				override def downtype[F[M[O] <: MappingAt[O]] <: FromClause, X](e :SQLExpression[F[AnyAt], X]) = e

				override def apply[L <: FromSome, J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
				                  (e :ColumnSQL[J[L, AnyAt], X]) = e

				override def apply[L <: FromSome, J[A <: FromSome, B[O] <: MappingAt[O]] <: A Join B, X]
				                  (e :SQLExpression[J[L, AnyAt], X]) = e

				override def self = this
			}

		implicit def identity[J[M[O] <: MappingAt[O]] <: _ Using M, R[O] <: BaseMapping[_, O]]
				:JoinedRelationSubject[J, R, R, R] =
			instance.asInstanceOf[JoinedRelationSubject[J, R, R, R]]


		/** A simplified form of the implicit witness
		  * [[net.noresttherein.oldsql.sql.AndFrom.JoinedRelationSubject JoinedRelationSubject]], it guides the compiler
		  * into proper inference of both the type of a mapping, and its `Subject` type as defined by `BaseMapping`.
		  * It is used when constructing a new `Join` subtype to avoid the need for explicit specification
		  * of the joined mapping type and its subject.
		  * It accepts five type parameters: first three are input parameters - the left side of the join `L`, the join
		  * type `J`, given as a two-argument type constructor, and the (origin-accepting) type constructor
		  * for the mapping `R` on the right side of the join. These should be specified as the types of parameters
		  * passed to the constructor function. The last two type parameters are the output: the type `T[O] =:= R[O]`
		  * (in reality, but not provable at the spot), but with the `BaseMapping` trait as its upper bound,
		  * and the type parameter `S` given by `R` to `BaseMapping` for the subject type. The object itself
		  * contains methods for converting various types related to the use case between those parameterized with `T`,
		  * and those parameterized with `R`. As the result, the parameters with more generic types, but which can be
		  * inferred automatically by the compiler, can be converted to the more specific variants,
		  * used in the construction on the join, and then converted back to a type expressed in terms of the input
		  * parameters.
		  */
		type InferSubject[L <: FromSome, J[+F <: FromSome, M[O] <: MappingAt[O]] <: F Join M,
		                  R[O] <: MappingAt[O], T[O] <: BaseMapping[S, O], S] =
			JoinedRelationSubject[JoinWith[J]#Left[L]#Right, R, T, MappingOf[S]#TypedProjection]

	}

}






/** A `FromClause` constituting of exactly one table or SQL relation.
  * This is a specialized subclass of `AndFrom[Dual, T]`, so that we can write the type From[T] instead, especially
  * in larger clauses like `From[Children] Join Daemons`. For all intents and purposes it is treated
  * as `Dual InnerJoin T` (which would be illegal due to the restriction of the left side of joins to non empty clauses).
  * All non empty ''from'' clauses start with this instance unless the first 'relation' is an unbound join parameter
  * represented by a `JoinParam`.
  */
sealed trait From[T[O] <: MappingAt[O]] extends AndFrom[Dual, T] with NonSubselect[Dual, T] {
	override val left :Dual = Dual

	def table :Relation[T] = last.relation

//	override def filter :SQLBoolean[FromClause AndFrom T] =
//		if (left.filter eq True) condition else left.filter && condition

	override type GeneralizedLeft[+L <: FromClause] = L AndFrom T
	override type WithLeft[+L <: FromClause] = L AndFrom T
	override type Generalized = FromClause AndFrom T
	override type Self = From[T]
	override type This >: this.type <: From[T]

	override def withLeft[F <: UngroupedFrom](newLeft :F)(filter :SQLBoolean[newLeft.Generalized AndFrom T]) :F AndFrom T

	override def generalizedExtension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].extend[AndFrom, T]

	override def extension[F <: FromSome] :F PrefixOf (F AndFrom T) = PrefixOf.itself[F].extend[AndFrom, T]



	override type AppendedTo[+P <: UngroupedFrom] = P AndFrom T

	override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] = P J T


	override type Implicit = FromClause
	override type Outer = Dual
	override type Explicit = FromClause AndFrom T
	override type Inner = UngroupedFrom AndFrom T

	override def outer :Dual = left

	override type SubselectRow = @~ ~ last.Subject

	override def subselectRow[E <: FromClause]
	                         (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, @~ ~ last.Subject] =
		EmptyChain ~ last.stretch(target)(extension)


	override type AsSubselectOf[+F <: FromSome] = F Subselect T


	override type Params = @~


	/** Specify an alias for this relation. This is not necessary and may be overriden in case of conflicts,
	  * but can be used as the default value and/or help with debugging.
	  * @param alias the alias for the relation as in 'from users as u'.
	  * @return a new `From` instance, with a new relation (not equal to this.last).
	  */
	def as[A <: Label](alias :A) :From[(T As A)#T]


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[From.*]

	override def name :String = "from"

	override def toString :String =
		if (filter == True()) "from " + last.relation
		else "from " + last.relation + " where " + filter

}






/** A `FromClause` factory for both single table queries, and starting points for arbitrary join lists.
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
	  * the [[net.noresttherein.oldsql.sql.FromClause#where where]] or
	  * [[net.noresttherein.oldsql.sql.AndFrom#whereLast whereLast]] method, providing the condition for the ''where''
	  * clause associated with the created clause. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]]:
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
	         (relation :Relation[R])(implicit cast :JoinedRelationSubject[From, R, T, MappingOf[S]#TypedProjection])
			:From[R] =
		cast(From(LastRelation[T, S](cast(relation)), True))


	/** Creates a ''from'' clause consisting of a single relation (table, view, select, or even a surrogate temporary
	  * mapping) with `Mapping` `R`. This is a lower level factory method accepting an optional, additional filter
	  * for the ''where'' clause, but providing no type inference support and thus generally requiring the type
	  * parameters to be specified explicitly. The clause can be also subsequently joined with other relations
	  * using join methods defined in [[net.noresttherein.oldsql.sql.FromClause.FromSomeExtension FromSomeExtension]]:
	  * `join`, `outerJoin`, `leftJoin`, `rightJoin` and `subselect`.
	  * @param relation the relation for the ''from'' clause, parameterized with a `BaseMapping` with subject type `S`.
	  * @param filter an optional boolean condition for the associated ''where'' clause.
	  * @return a `From[R]`.
	  */
	def apply[T[O] <: BaseMapping[S, O], S]
	         (relation :Relation[T], filter: SQLBoolean[FromClause AndFrom T] = True) :From[T] =
		From(LastRelation[T, S](relation), filter)


	private[sql] def apply[T[O] <: BaseMapping[S, O], S]
	                      (relation :LastRelation[T, S], filter :SQLBoolean[FromClause AndFrom T]) :From[T] =
		narrow(Dual, relation, filter)

	private[sql] def narrow[T[O] <: BaseMapping[S, O], S]
	                       (dual :Dual, relation :LastRelation[T, S], cond :SQLBoolean[FromClause AndFrom T])
			:From[T] with (dual.type AndFrom T) =
		new From[T] with AndFrom[dual.type, T] with BaseExtended[dual.type, T, S] {
			override val left :dual.type = dual
			override val last = relation
			override val condition = cond
			override def size = 1

			override def narrow :AndFrom[dual.type, T] = this
			override type This = From[T] with (dual.type AndFrom T)


			override def withCondition(filter :SQLBoolean[FromClause AndFrom T]) =
				From.narrow(left, last, filter)

			override def withLeft[F <: UngroupedFrom](newLeft :F)(filter :SQLBoolean[newLeft.Generalized AndFrom T])
					:F AndFrom T =
				newLeft.extend(last, filter)


			override def appendedTo[P <: UngroupedFrom](prefix :P) :P AndFrom T = prefix.extend(last, filter)

			override def joinedWith[F <: FromSome](prefix :F, firstJoin :TrueJoin.*) :firstJoin.LikeJoin[F, T] =
				firstJoin.likeJoin[F, T, S](prefix, right)(condition)

			override def joinedAsSubselect[F <: FromSome](prefix :F) :Subselect[F, T] =
				Subselect[F, T, S](prefix, last)(condition)


			override def subselectTableStack[E <: FromClause]
			             (target :E)(implicit stretch :Generalized ExtendedBy E) :LazyList[RelationSQL.AnyIn[E]] =
				last.stretch[Generalized, E](target) #:: LazyList.empty[RelationSQL.AnyIn[E]]


			override def asSubselectOf[F <: FromSome](newOuter :F)(implicit extension :Implicit ExtendedBy F)
					:newOuter.type Subselect T =
				Subselect[newOuter.type, T, S](newOuter, last)(condition)


			override def as[A <: Label](alias :A) :From[(T As A)#T] = {
				val source = FromClause.AliasedRelation[T, A](last.relation, alias)
				val aliased = RelationSQL.last[(T As A)#T, (T As A)#T, S](source)
				type Res = FromClause AndFrom (T As A)#T //todo: condition from a function
				val unfiltered = From[(T As A)#T, S](aliased, True)
				val replacement = aliased \ (unfiltered.last.mapping.body :T[FromClause AndFrom (T As A)#T])
				val substitute = new ReplaceRelation[T, S, (T As A)#T, S, Generalized, Res](
					generalized, unfiltered)(last, replacement
                )
				From[(T As A)#T, S](aliased, substitute(condition))
			}

		}



	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply[M[O] <: MappingAt[O]](from :FromClause Using M) :Option[Relation[M]] = from match {
		case _ :From.* => Some(from.right)
		case _ => None
	}

	/** Matches all `From` instances, extracting their relation in the process. */
	def unapply(from :FromClause) :Option[Relation.*] = from match {
		case f :From.* => Some(f.table)
		case _ => None
	}

	
	
	implicit def fromDecomposition[T[O] <: MappingAt[O]] :FromComposition[T] =
		decomposition.asInstanceOf[FromComposition[T]]
	
	private[this] val decomposition = new FromComposition[MappingAt]


	class FromComposition[M[O] <: MappingAt[O]] extends ClauseComposition[From[M], Dual, Dual] {
		override type E[+D <: Dual] = From[M]
		override type S[+D >: Dual <: Dual] = From[M]

		override def prefix[A >: Dual <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def extension[A <: Dual] :A PrefixOf From[M] =
			PrefixOf.itself[From[M]].asInstanceOf[A PrefixOf From[M]]

		override def apply(from :From[M]) :Dual = from.left

		override def upcast[A >: Dual <: Dual] :ClauseComposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseComposition[From[M], A, Dual]]

		override def cast[A <: Dual] :ClauseComposition[From[M], A, Dual] =
			this.asInstanceOf[ClauseComposition[From[M], A, Dual]]
	}
	
	
	
	/** Type alias for `From` with the erased type parameter, covering all instances of `From`.
	  * Provided for the purpose pattern matching, as the relation type parameter of the higher kind cannot
	  * be matched directly with the wildcard '_'.
	  */
	type * = From[M] forSome { type M[O] <: MappingAt[O] }

}


