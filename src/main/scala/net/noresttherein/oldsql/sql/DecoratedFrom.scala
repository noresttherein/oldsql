package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator.FromSomeDecoratorComposition
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ClauseComposition, ClauseDecomposition, ExtendedBy, ExtendingClause, NonEmptyFrom, PartOf, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** Base trait for all `FromClause` implementations which serve as wrappers to other clauses. Decorators introduce
  * no new relations or ''where'' clause fragments and in most cases simply delegate to the corresponding method
  * of the wrapped clause `F`. Currently no implementations are used by the library and it exists largely
  * to future proof the design and allow an extension point for custom classes. One such usage could be, for example,
  * providing custom annotations which, in conjunction with a custom `SQLWriter`, could be used to modify the generated
  * SQL statements. For this reason this type is recognized and handled by all built-in types processing
  * `FromClause` or `SQLExpression` types. This is a bare bones root type designed for flexibility and convenience
  * due to no upper bound on the wrapped clause type. Most practical implementations will likely prefer to extend
  * the more fleshed out [[net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator FromSomeDecorator]].
  * @author Marcin Mościcki
  */ //this could benefit from being derived from Any
trait DecoratedFrom[+F <: FromClause] extends FromClause { thisClause =>
	val clause :F
	/** The upper bound for the decorated clause used in this clause's dynamic type. */
	type Bound >: clause.FromLast <: FromClause

	/** Wraps a (possibly modified) copy of the underlying clause in a new decorator instance. */
	def withClause[C <: Bound](body :C) :DecoratedFrom[C]


	override type Params = clause.Params

	override def isParameterized :Boolean = clause.isParameterized



	override type Implicit = clause.Implicit
	override type Outer = clause.Outer
	override type Base = DefineBase[Implicit]
	override type OuterRow = clause.OuterRow

	override def isValidSubselect :Boolean = clause.isValidSubselect



	protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] = matcher.decorator(this)

}






object DecoratedFrom {


	/** A marker trait for [[net.noresttherein.oldsql.sql.FromClause FromClause]] decorators which include
	  * all relations from the decorated clause. This is the de facto real root of the decorator type hierarchy,
	  * with only the special [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] adapter not being its subtype.
	  * It is analogous to the [[net.noresttherein.oldsql.sql.Extended Extended]] base trait for joins and join-like
	  * classes.
	  */
	trait ExtendingDecorator[+F <: FromClause] extends DecoratedFrom[F] with ExtendingClause[F] {
		override def isEmpty :Boolean = clause.isEmpty
		override def fullSize :Int = clause.fullSize

		override type LastMapping[O] = clause.LastMapping[O]
		override type FullRow = clause.FullRow
		override type InnerRow = clause.InnerRow
		override type OuterRow = clause.OuterRow


		protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] =
			matcher.extendingDecorator(this)

		private[sql] override def concrete_ExtendingClause_subclass_must_extend_Extended_or_ExtendingDecorator :Nothing =
			throw new UnsupportedOperationException
	}



	/** Default base trait for non empty `DecoratedFrom` implementations. Introduces the type constructors
	  * for the `Generalized` and `Self` types and implements most `FromClause` methods by delegating to the
	  * underlying clause. It requires all implementing classes to be applicable to any non empty clause `F`.
	  */
	trait FromSomeDecorator[+F <: FromSome] extends FromSome with ExtendingDecorator[F] { thisClause =>
		override type LastMapping[O] = clause.LastMapping[O]
		override type Bound = FromSome
		override type FromLast = GeneralizedClause[clause.FromLast]
		override type Generalized = GeneralizedClause[clause.Generalized]
		override type Self = WithClause[clause.Self]

		override type This <: WithClause[clause.This] {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Self = thisClause.Self
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: FromClause] = thisClause.DefineBase[I]
			type InnerRow = thisClause.InnerRow
			type OuterRow = thisClause.OuterRow
			type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
				thisClause.JoinedWith[P, J]
			type JoinedWithSubselect[+P <: NonEmptyFrom] = thisClause.JoinedWithSubselect[P]
			type FromRelation[T[O] <: MappingAt[O]] = thisClause.FromRelation[T]
			type FromSubselect[+S <: NonEmptyFrom] = thisClause.FromSubselect[S]
		}


		override def last :JoinedRelation[FromLast, LastMapping] = clause.last.asIn[FromLast]

		type GeneralizedClause[+G <: FromSome] <: FromSomeDecorator[G] {
			type GeneralizedClause[+S <: FromSome] = thisClause.GeneralizedClause[S]
		}

		type WithClause[+G <: FromSome] <: GeneralizedClause[G] {
			type WithClause[+S <: FromSome] = thisClause.WithClause[S]
		}

		def withClause[C <: FromSome](from :C) :WithClause[C]



		override type FullRow = clause.FullRow

		override def fullRow[E <: FromClause]
		             (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, GlobalScope, FullRow] =
			clause.fullRow(target)(extension.unwrapFront)

		override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.fullTableStack(target)(extension.unwrapFront)


		override type JoinedWith[+P <: FromClause, +J[+L <: P, R[O] <: MappingAt[O]] <: L AndFrom R] =
			WithClause[clause.JoinedWith[P, J]]

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
			withClause(clause.joinedWith(prefix, firstJoin))

		override type JoinedWithSubselect[+P <:  NonEmptyFrom] = WithClause[clause.JoinedWithSubselect[P]]

		override def joinedWithSubselect[P <: NonEmptyFrom](prefix :P) :JoinedWithSubselect[P] =
			withClause(clause.joinedWithSubselect(prefix))

		override def appendedTo[P <: DiscreteFrom](prefix :P) :WithClause[clause.JoinedWith[P, AndFrom]] =
			withClause(clause.appendedTo(prefix))



		override type Explicit = GeneralizedClause[clause.Explicit]
		override type Inner = WithClause[clause.Inner]
		override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
		override type DefineBase[+I <: FromClause] = clause.DefineBase[I]

		override def base :Base = clause.base

		override def filter[E <: FromClause]
		                   (target :E)(implicit extension :Generalized PartOf E) :GlobalBoolean[E] =
			clause.filter(target)(extension.unwrapFront)

		override type InnerRow = clause.InnerRow

		override def innerRow[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:ChainTuple[E, GlobalScope, clause.InnerRow] =
			clause.innerRow(target)(extension.unwrapFront)

		override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.innerTableStack(target)(extension.unwrapFront)

		override type OuterRow = clause.OuterRow

		override def outerRow[E <: FromClause](target :E)(implicit extension :Implicit ExtendedBy E)
				:ChainTuple[E, GlobalScope, clause.OuterRow] =
			clause.outerRow(target)

		override type AsSubselectOf[+P <: NonEmptyFrom] = WithClause[clause.AsSubselectOf[P]]

		override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit extension :Implicit ExtendedBy P)
				:WithClause[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
			withClause(clause.asSubselectOf(newOuter))


		protected override def matchWith[Y](matcher :FromClauseMatcher[Y]) :Option[Y] =
			matcher.fromSomeDecorator(this)

	}



	object FromSomeDecorator {

		@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} and the " +
		                  "decorated clause ${C}.\nMissing implicit FromSomeDecoratorComposition[${F}, ${C}, ${D}, ${U}].")
		class FromSomeDecoratorComposition
		            [F <: D[C], C <: FromSome,
		             D[+B <: FromSome] <: FromSomeDecorator[B] { type WithClause[+P <: FromSome] <: D[P] }]
			extends DecoratorComposition[F, C, D, FromSome]
		{
			@inline final override def apply[B <: FromSome](template :F, clause :B) :D[B] = template.withClause(clause)
		}

		implicit def genericDecoratorComposition
		             [F <: D[C], C <: FromSome,
		              D[+B <: FromSome] <: FromSomeDecorator[B] { type WithClause[+P <: FromSome] <: D[P] }]
				:FromSomeDecoratorComposition[F, C, D] =
			composition.asInstanceOf[FromSomeDecoratorComposition[F, C, D]]

	}



	/** Implicit witness providing a type constructor for the decorator type `F`. It is used in type inference
	  * to disassemble the type `F` into the decorator constructor `D` and the decorated clause `C`.
	  * Type parameters in the form of `[D[+C <: FromSome] <: FromSomeDecorator, C <: FromSome]`
	  * can be properly inferred from the type `F` if it is a type formed from applying the single argument
	  * type constructor directly to the decorated type (for example, for `F =:= FromSomeDecorator[C]`, the types
	  * would be properly instantiated as `D[X] =:= FromSomeDecorator[X]` and `C =:= C`) or if `F` is formed
	  * from applying a multi argument type constructor, with the decorated clause being the last type parameter
	  * (so, for `trait Deco[F, +C <: FromSome] extends FromSomeDecorator[C]`, `Deco[F, C]` will be correctly unified as
	  * `D[X] =:= Deco[F, X], C =:= C`). However, if the `C` type argument is not the last one, or the type definition
	  * is more complex, automatic partial unification will fail - as it would for
	  * [[net.noresttherein.oldsql.sql.Alias Alias]], which takes the clause as its first argument.
	  * This order can be important for readability reasons, so to proof various implicit factory methods
	  * which recursively scan `FromClause` subtypes against any future decorator extensions which would break
	  * the type inference preventing from the materializing of the implicit value, the factory method can rely on
	  * this type class. Any decorator implementations for which default resolution would fail can provide an implicit
	  * of this type with the proper deconstruction in its companion object.
	  */
	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} " +
	                  "and the decorated clause ${C}.\nMissing implicit DecoratorDecomposition[${F}, ${C}, ${D}, ${U}].")
	class DecoratorDecomposition[F <: D[C], C <: U, D[+B <: U] <: ExtendingDecorator[B], U <: FromClause]
		extends ClauseDecomposition[F, C, U]
	{
		override type E[+A <: U] = D[A]
		override type S[+A >: C <: U] = D[A]

		@inline final override def prefix[A >: C <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]
		@inline final override def extension[A <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]

		@inline final override def strip(decorator :F) :C = decorator.clause

		override def upcast[A >: C <: U] :DecoratorDecomposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorDecomposition[D[A], A, D, U]]

		override def cast[A <: U] :DecoratorDecomposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorDecomposition[D[A], A, D, U]]
	}


	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} and " +
	                  "the decorated clause ${C}.\nMissing implicit DecoratorDecomposition[${F}, ${C}, ${D}, ${U}].")
	abstract class DecoratorComposition[F <: D[C], C <: U, D[+B <: U] <: ExtendingDecorator[B], U <: FromClause]
		extends DecoratorDecomposition[F, C, D, U] with ClauseComposition[F, C, U]


	implicit def DecoratorDecomposition[D[+C <: FromSome] <: FromSomeDecorator[C], F <: FromSome]
			:DecoratorDecomposition[D[F], F, D, FromSome] =
		decomposition.asInstanceOf[DecoratorDecomposition[D[F], F, D, FromSome]]

	private[this] val decomposition =
		new DecoratorDecomposition[FromSomeDecorator[FromSome], FromSome, FromSomeDecorator, FromSome]

	private[this] val composition = new FromSomeDecoratorComposition[Alias[FromSome, ""], FromSome, Alias.WithLabel[""]#F]
}
