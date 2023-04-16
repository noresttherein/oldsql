package net.noresttherein.oldsql.sql

import scala.annotation.{implicitNotFound, showAsInfix}

import net.noresttherein.oldsql.morsels.ChunkedString
import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.Relation
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, ExpandingClause, NonEmptyRow, NonEmptyRowTemplate, PrefixOf, RowComposition, RowDecomposition}
import net.noresttherein.oldsql.sql.SQLBoolean.True
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{ChainTuple, JoinedRelation, RelationSQL}






/** A skeleton [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] implementation combining another ''from''
  * clause `L` and an additional relation with mapping `R`. The relationship between the constituent clause
  * and this clause is undefined; in particular, the relations from the clause `L` may not be a part of this clause.
  * It is a root of a class hierarchy of various SQL joins, subselect clauses and other implementations sharing the same
  * basic recursive interface. It is too generic to be of much use in the client code, as most operations
  * are not available at this level of abstraction and, as such, is considered more implementation oriented,
  * providing definitions for some additional abstract methods. Instances and subclasses which are not
  * subtypes of [[net.noresttherein.oldsql.sql.Join Join]] are often called 'pseudo joins'.
  *
  * Together with the empty clause [[net.noresttherein.oldsql.sql.Dual Dual]] it forms a heterogeneous list-like
  * structure with the information about all joined relations encoded in its type.
  * The given mapping doesn't have to represent a table at this point - it might be for example a table component
  * to be 'planted' in a particular table at a later point.
  *
  * @tparam L the left side of this join: a `RowProduct` listing all preceding relations.
  * @tparam R the right side of this join: a mapping type constructor for the last relation in this clause, accepting
  *           the `Origin` type as the unspecified parameter.
  * @see [[net.noresttherein.oldsql.sql.Expanded]]
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.AndBy]]
  * @see [[net.noresttherein.oldsql.sql.ParamClause]]
  */ //consider: renaming to Nexus/Link/Attach/Unite/Conjoin/Relate/PseudoJoin
//todo: make it covariant in R. As Subject must still be preserved, this is not of great practical utility though
@showAsInfix
trait Adjoin[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends NonEmptyRow with NonEmptyRowTemplate[L Adjoin R, L Adjoin R]
{ thisClause =>

	override type LastMapping[O] = R[O]
	override type Last[F <: RowProduct] <: JoinedRelation[F, R]
	override type FromLast >: Generalized <: RowProduct Adjoin R


	/** A `RowProduct` constituting a pre-existing joined list of relations - may be empty (`Dual`). */
	val left :L

	/** The last SQL relation in this clause. This is the relation understood in a very abstract way
	  * and in a global sense, as a unique database object, rather than an entry in the list of relations
	  * in the ''from'' clause - for that have a look at [[net.noresttherein.oldsql.sql.RowProduct.last last]].
	  * Specialized `Adjoin` subclasses use dedicated implementations, narrowing the value
	  * to a particular [[net.noresttherein.oldsql.schema.Relation Relation]] subtype
	  * (such as [[net.noresttherein.oldsql.schema.Table Table]]), allowing its use as the argument to various
	  * factory methods. In some other cases, the 'relation' can represent a query parameter
	  * ([[net.noresttherein.oldsql.sql.ParamClause ParamClause]]) or a grouping expression
	  * ([[net.noresttherein.oldsql.sql.GroupBy GroupBy]]/[[net.noresttherein.oldsql.sql.By By]])
	  * and may impose constraints on its reuse.
	  *
	  * Additionally, it can potentially serve as a placeholder value, with a `RowProduct` depending on a relation
	  * with a `Mapping` for a component not representing any single database object, but which can be replaced
	  * at a later date by 'rooting' it to a table containing this type of component. For this reason,
	  * care should be taken when using this property in an abstract context not to assume anything particular about
	  * the interpretation of this value.
	  */
	def right :Relation[R] = last.relation

	/** The right side of the join - a representation of a table/relation alias, containing the mapping of its schema
	  * as a subtype of [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]]`[`[[net.noresttherein.oldsql.sql.RowProduct.FromLast FromLast]]`, `[[net.noresttherein.oldsql.sql.RowProduct.LastMapping LastMapping]]`]`.
	  * For [[net.noresttherein.oldsql.sql.FromClause proper]] ''from'' clauses, it identifies
	  * the SQL [[net.noresttherein.oldsql.schema.Table table]]
	  * (a [[net.noresttherein.oldsql.schema.BaseTable base table]], a [[net.noresttherein.oldsql.schema.View view]] or
	  * a [[net.noresttherein.oldsql.schema.Table.TableExpression select]]) which is being added to the clause
	  * and its index, to distinguish between possible multiple occurrences of the same relation.
	  * Other classes use specific [[net.noresttherein.oldsql.sql.ast.JoinedRelation JoinedRelation]] subtypes adapting
	  * other sources of values to the common interface for uniform use in  SQL expressions based on the type
	  * of this instance.
	  *
	  * It is an [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]] for the subject of the relation's mapping,
	  * so it can be used directly as part of larger expressions. True to its name, it is treated always
	  * as the last entry on the list and the expression `join.left.last` is an instance representing only
	  * the last relation in the prefix clause `join.left`. In particular, it is not a valid reference to a relation
	  * from the point of view of `join`. Its type parameter makes it incompatible for direct use
	  * in SQL expressions based on this clause and casting it will result in generating invalid SQL.
	  * You can however use the method [[net.noresttherein.oldsql.sql.Adjoin.lastAsIn lastAsIn]] in the following way:
	  * {{{
	  *     def secondLast[T1[O] <: MappingAt[O], T2 <: MappingAt[O]](from :RowProduct AndFrom T1 AndFrom T2) =
	  *         from.left.lastAsIn[RowProduct AndFrom T1 AndFrom T2]
	  * }}}
	  * The relation can be used in this way in expressions based on clauses containing this instance as a prefix
	  * (which where created by joining/adding additional [[net.noresttherein.oldsql.sql.Expanded Expanded]] links to it).
	  * Moreover, any two clauses with types sharing a suffix, can freely exchange relations from that suffix and,
	  * by transitivity, any SQL expressions based on the clause type representing that suffix,
	  * with the differing prefixes replaced with a wildcard or abstract type.
	  *
	  * Note however that specialized `Adjoin` implementations can use dedicated implementations
	  * of [[net.noresttherein.oldsql.schema.Relation Relation]] or its [[net.noresttherein.oldsql.schema.Mapping Mapping]],
	  * tied not only to this particular implementation, but also to this instance.
	  *
	  * All instances included in standard implementations of `RowProduct` represent an unaltered view of the relation,
	  * that is they have empty [[net.noresttherein.oldsql.sql.ast.JoinedRelation.includes includes]] and
	  * [[net.noresttherein.oldsql.sql.ast.JoinedRelation.excludes excludes]] list, meaning the definitive
	  * column set for any component is the same as in the wrapped [[net.noresttherein.oldsql.schema.Relation Relation]].
	  * Deviating from this rule will not result in an error, but can introduce unexpected alterations
	  * to the final SQL.
	  */
	override val last :Last[FromLast]

	/** The join condition joining the right side to the left side. It is used as either the ''on'' clause of the
	  * SQL standard for true joins, or the ''where''/''having'' clause. It is not the complete filter
	  * condition, as it doesn't include any join conditions defined on the left side of this join.
	  * @see [[net.noresttherein.oldsql.sql.RowProduct.filter]]
	  */ //declared here to have a single equals/hashCode implementation
	protected def condition :GroupedBoolean[Generalized]


	override type Generalized >: Complete <: (left.Generalized Adjoin R) {
		type FromLast     = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete Adjoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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

	override type NoAlias >: Self <: (left.Self Adjoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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

	override type Self <: (left.Self Adjoin R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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


	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `RowProduct`, as they become proper path types instead of projections.
	  */
	def narrow :left.type Adjoin R


	override def fullSize :Int = left.fullSize + 1

	override def isParameterized  :Boolean = left.isParameterized
	override def isValidSubselect :Boolean = left.isValidSubselect
	override def paramCount       :Int = left.paramCount
	override def lastParamOffset  :Int = left.lastParamOffset match {
		case n if n < 0 => n
		case n => n + 1
	}

	override type Base = DefineBase[Implicit]


//	protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
//		spelling.sqlParamCount(left) + spelling.sqlParamCount(right) + spelling.sqlParamCount(condition)


	override def homomorphic(that :RowProduct) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case join :Adjoin[_, _] if generalizedClass == join.generalizedClass =>
			(last homomorphic join.last) && (left homomorphic join.left) && (condition homomorphic join.condition)
		case _ => false
	}
	override def isomorphic(that :RowProduct) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case join :Adjoin[_, _] if generalizedClass == join.generalizedClass =>
			(last isomorphic join.last) && (left isomorphic join.left) && (condition isomorphic join.condition)
		case _ => false
	}
	override def identical(that :RowProduct) :Boolean = that match {
		case self :AnyRef if this eq self => true
		case join :Adjoin[_, _] if canEqual(join) && join.canEqual(this) =>
			(last identical join.last) && (left identical join.left) && (condition identical join.condition)
		case _ => false
	}
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case join :Adjoin[_, _] if canEqual(join) && join.canEqual(this) =>
			last == join.last && left == join.left && condition == join.condition
		case _ => false
	}
	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Adjoin.__]
	override def hashCode :Int = (left.hashCode * 31 + last.hashCode) * 31 + condition.hashCode


	/** Name of the join for use by the `toString` method. */
	def name :String

	override def chunkedString :ChunkedString = {
		var res = left.chunkedString + (" " + name + " ") + right.toString
		if (aliasOpt.nonEmpty)
			res += " as " + alias
		if (condition != True)
			res = res + " on " + condition.toString
		res
	}

	override def typeString(res :StringBuilder) :StringBuilder =
		left.typeString(res) += ' ' ++= name += ' ' ++= right.refString

}




object Adjoin {

	/** An existential upper bound of all `Adjoin` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = Adjoin[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.Adjoin Adjoin]] instances joining any clause
	  * with relation of mapping `M`.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct Adjoin M


	/** A curried type constructor for `Adjoin` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Adjoin R }

	/** A curried type constructor for `Adjoin` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Adjoin R }

}






/** A [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] implementation which expands an existing ''from'' clause `L`
  * with a new relation (an SQL table, view, select, or some synthetic placeholder) `R`. All relations
  * from the left side are considered a part of this clause, witnessed by an implicit value of
  * of `L` [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy ExpandedBy]] `R`.
  * Thus, all [[net.noresttherein.oldsql.sql.SQLExpression SQLExpression]]s based on the left side (parameterized
  * with `L`) are convertible to expressions based on this clause.
  *
  * It is an implementation oriented trait, grouping most common definitions of methods declared in `RowProduct`,
  * useful only when operating on the most abstract level, as most domain specific functions are available only
  * for more specialized types.
  *
  * @see [[net.noresttherein.oldsql.sql.AndFrom]]
  * @see [[net.noresttherein.oldsql.sql.By]]
  * @see [[net.noresttherein.oldsql.sql.RowProduct.ExpandedBy]]
  * @author Marcin Mościcki
  */ //other words :Expand, Tack, Include, PseudoJoin
@showAsInfix
trait Expanded[+L <: RowProduct, R[O] <: MappingAt[O]]
	extends Adjoin[L, R] with ExpandingClause[L] with NonEmptyRowTemplate[L Expanded R, L Expanded R]
{ thisClause =>
	//AndBy#Expanded = RowProduct AndBy R, which is not Expanded (due to GroupBy)
//	override type FromLast >: Generalized <: RowProduct Expanded R

	override type Generalized >: Complete <: (left.Generalized Expanded R) {
		type FromLast     = thisClause.FromLast
		type Generalized <: thisClause.Generalized
		type Explicit    <: thisClause.Explicit
		type Implicit    <: thisClause.Implicit
		type DefineBase[+I <: RowProduct] <: thisClause.DefineBase[I]
	}

	override type Complete >: NoAlias <: (left.Complete Expanded R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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

	override type NoAlias >: Self <: (left.Self Expanded R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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

	override type Self <: (left.Self Expanded R) {
		type Last[O <: RowProduct] = thisClause.Last[O]
		type FromLast    = thisClause.FromLast
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


	/** Narrows this instance to one parameterized with the singleton type of its left side. This is helpful when
	  * using member types of `RowProduct`, as they become proper path types instead of projections.
	  */
	def narrow :left.type Expanded R


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[Expanded.__]

	private[sql] override def concrete_ExpandingClause_subclass_must_extend_Expanded_or_ExpandedDecorator(seal :Seal) :Unit = ()
}






object Expanded {


	/** An existential upper bound of all `Expanded` instances that can be used in casting or pattern matching
	  * without generating compiler warnings about erasure.
	  */
	type __ = Expanded[_ <: RowProduct, M] forSome { type M[O] <: MappingAt[O] }

	/** The least upper bound of all [[net.noresttherein.oldsql.sql.Expanded Expanded]] instances joining any clause
	  * with relation of mapping `M`.
	  */
	type LUB[M[O] <: MappingAt[O]] = RowProduct Expanded M

	/** A curried type constructor for `Expanded` instances, accepting the left `RowProduct` type parameter
	  * and returning a type with a member type `F` accepting the type constructor for the right relation.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithLeft[L <: RowProduct] = { type F[R[O] <: MappingAt[O]] = L Expanded R }

	/** A curried type constructor for `Expanded` instances, accepting the right mapping type parameter
	  * and returning a type with a member type `F` accepting the left `RowProduct` type.
	  * A convenience alias for use wherever a single-argument type constructor for a `RowProduct` is required.
	  */
	type WithRight[R[O] <: MappingAt[O]] = { type F[L <: RowProduct] = L Expanded R }



	/** A base trait for implementations of [[net.noresttherein.oldsql.sql.Expanded Expanded]] which narrows
	  * down the mapping type to the actually required `BaseMapping` (due to some scala bug preventing
	  * the use of `TypedMapping` in the `TypedComponentSQL` instead).
	  */ //todo: remove this and move implementation to Expanded when we get rid of the BaseMapping bound on everything
	trait AbstractExpanded[+L <: RowProduct, R[O] <: BaseMapping[S, O], S] extends Expanded[L, R] { thisClause =>
		//todo: why don't we use last (and narrow down Last)?
		protected def lastRelation :RelationSQL[FromLast, R, S, FromLast]

		override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.from[E]#__] =
		LazyList.cons[RelationSQL.from[E]#__](
				lastRelation.expand(target).upcast,
				left.fullTableStack(target)(expansion.expandFront[left.Generalized, R])
			) //this: crashes the compiler:
//			lastRelation.expand(target) #:: left.fullTableStack(target)(expansion.expandFront[left.Generalized, R])


		override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.from[E]#__] =
			LazyList.cons[RelationSQL.from[E]#__](
				lastRelation.expand(target).upcast,
				left.tableStack(target)(expansion.expandFront[left.Generalized, R])
			)
//			lastRelation.expand(target) #:: left.tableStack(target)(expansion.expandFront[left.Generalized, R])
	}






	/** A marker trait for [[net.noresttherein.oldsql.sql.Expanded Expanded]] implementations other than
	  * the [[net.noresttherein.oldsql.sql.Subselect Subselect]] pseudo join. While not mandatory, it is recommended
	  * that all such clauses expand this type, as several implicit parameters responsible for traversing
	  * composite ''from'' clauses, in particular accessors for the joined relations, depend on any `Expanded` type
	  * to conform to either it, or `Subselect`, with no provision for other cases. Note that this holds only
	  * for `Expanded` subtypes, and there are other clauses, in particular
	  * [[net.noresttherein.oldsql.sql.GroupBy GroupBy]], which are neither.
	  */ //todo: update docs with usage once With is fully incorporated
	@showAsInfix //consider: renaming to Expanded
	trait NonSubselect[+L <: RowProduct, R[O] <: MappingAt[O]]
		extends Expanded[L, R] with NonEmptyRowTemplate[L NonSubselect R, L NonSubselect R]
	{ thisClause => //From currently extends this trait, but the contract is actually the same as for Subselect in its case
		override def isExplicitParameterized :Boolean = left.isExplicitParameterized

		override type Implicit = left.Implicit
		override type Outer = left.Outer

		override type OuterRow = left.OuterRow

		override def outerRow[E <: RowProduct]
		             (target :E)(implicit expansion :Implicit ExpandedBy E) :ChainTuple[E, Single, OuterRow] =
			left.outerRow(target)

		override def withClause :WithClause = left.withClause ++ right.withClause ++ condition.outerWithClause

//		protected override def sqlParamCount(implicit spelling :SQLSpelling) :Int =
//			spelling.sqlParamCount(spelling.sqlParamCount(right)) + spelling.sqlParamCount(condition)
	} //consider: a companion object with standard factory methods and type constructors
	//todo: test that there is a Decomposition type class for NonSubselect or create it

	object NonSubselect {
		type __ = NonSubselect[_ <: RowProduct, R] forSome { type R[O] <: MappingAt[O] }
	}






	/** A witness for the fact that any subtype of a join-like clause `L J R`,
	  * `J[A <: U, B[O] <: R[O]] <: A `[[net.noresttherein.oldsql.sql.Expanded Expanded]]` B`
	  * can be decomposed into its prefix clause `L` and appended table/relation mapping `R`.
	  * It is used, often as an instance of its super type `RowDecomposition[F, L, U]`, as a helper implicit
	  * in computation of evidence which requires recursive deconstruction of an input clause type.
	  * @tparam F the decomposed clause, the 'in' type parameter of this evidence.
	  * @tparam L the left side of the `Expanded` clause `F`.
	  * @tparam J the join-like type constructor of `F` (or its supertype).
	  * @tparam R the last relation mapping in clause `F`.
	  * @tparam U the upper bound on the left side of `J`, that is the type of clauses which can be substituted for L.
	  */
	@implicitNotFound("I do not know how to decompose ${F} into an Expanded subtype ${L} ${J} ${R}.\n" +
	                  "Missing implicit ExpandedDecomposition[${F}, ${L}, ${R}, ${J}, ${U}].")
	class ExpandedDecomposition[-F <: L J R, L <: U, R[O] <: MappingAt[O],
	                            J[+A <: U, B[O] <: R[O]] <: A Expanded B, U <: RowProduct]
		extends RowDecomposition[F, L, U]
	{
		override type E[+A <: U] = A J R
		override type S[+A >: L <: U] = A J R

		@inline final override def prefix[A >: L <: U] :A PrefixOf (A J R) = new PrefixOf[A, A J R](1)
		@inline final override def expansion[A <: U] :A PrefixOf (A J R) = new PrefixOf[A, A J R](1)

		@inline final override def unapply(join :F) :L = join.left

		override def upcast[A >: L <: U] :ExpandedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExpandedDecomposition[A J R, A, R, J, U]]

		override def cast[A <: U] :ExpandedDecomposition[A J R, A, R, J, U] =
			this.asInstanceOf[ExpandedDecomposition[A J R, A, R, J, U]]
	}


	@implicitNotFound("I do not know how to decompose ${L} ${J} ${R} into an Expanded subtype.\n" +
	                  "Missing implicit ExpandedComposition[${L}, ${R}, ${J}, ${U}, ${M}].")
	abstract class ExpandedComposition[L <: U, R[O] <: M[O],
	                                   J[+A <: U, B[O] <: M[O]] <: A Expanded B, U <: RowProduct, M[O] <: MappingAt[O]]
		extends ExpandedDecomposition[L J R, L, R, J, U] with RowComposition[L J R, L, U]
	{ self =>
		override def upcast[A >: L <: U] :ExpandedComposition[A, R, J, U, M] =
			this.asInstanceOf[ExpandedComposition[A, R, J, U, M]]

		override def cast[A <: U] :ExpandedComposition[A, R, J, U, M] =
			this.asInstanceOf[ExpandedComposition[A, R, J, U, M]]
	}


//	implicit def expandedDecomposition[L <: RowProduct, R[O] <: MappingAt[O]]
//			:ExpandedDecomposition[L Expanded R, L, R, Expanded, RowProduct] =
//		decomposition.asInstanceOf[ExpandedDecomposition[L Expanded R, L, R, Expanded, RowProduct]]
//
//	private[this] val decomposition =
//		new ExpandedDecomposition[RowProduct Expanded MappingAt, RowProduct, MappingAt, Expanded, RowProduct]

}
