package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator.FromSomeDecoratorComposition
import net.noresttherein.oldsql.sql.RowProduct.{ExpandedBy, ExpandingClause, NonEmptyFrom, PartOf, PrefixOf, RowComposition, RowDecomposition}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.RelationSQL
import net.noresttherein.oldsql.sql.ast.TupleSQL.ChainTuple
import net.noresttherein.oldsql.sql.mechanics.RowProductVisitor






/** Base trait for all `RowProduct` implementations which serve as wrappers to other clauses. Decorators introduce
  * no new relations or ''where'' clause fragments and in most cases simply delegate to the corresponding method
  * of the wrapped clause `F`. Currently no implementations are used by the library and it exists largely
  * to future proof the design and allow an extension point for custom classes. One such usage could be, for example,
  * providing custom annotations which, in conjunction with a custom `SQLWriter`, could be used to modify the generated
  * SQL statements. For this reason this type is recognized and handled by all built-in types processing
  * `RowProduct` or `SQLExpression` types. This is a bare bones root type designed for flexibility and convenience
  * due to no upper bound on the wrapped clause type. Most practical implementations will likely prefer to extend
  * the more fleshed out [[net.noresttherein.oldsql.sql.DecoratedFrom.FromSomeDecorator FromSomeDecorator]].
  * @author Marcin Mościcki
  */ //this could benefit from being derived from Any
trait DecoratedFrom[+F <: RowProduct] extends RowProduct { thisClause =>
	val clause :F

	/** The upper bound for the decorated clause used in this clause's dynamic type. */
	type Bound >: clause.FromLast <: RowProduct
	type LeftBound = Bound

//	override type LastMapping[O] = clause.LastMapping[O]
//	override type Last[O <: RowProduct] <: clause.Last[O]

	/** Wraps a (possibly modified) copy of the underlying clause in a new decorator instance. */
	def withClause[C <: Bound](body :C) :DecoratedFrom[C]


	override def paramCount :Int = clause.paramCount
	override def lastParamOffset :Int = clause.lastParamOffset
	override def isParameterized :Boolean = clause.isParameterized
	override def isSubselectParameterized :Boolean = clause.isSubselectParameterized

	override type Params = clause.Params
	override type LastParam = clause.LastParam

	override type Implicit = clause.Implicit
	override type Outer = clause.Outer
	override type Base = DefineBase[Implicit]
	override type OuterRow = clause.OuterRow

	override def isValidSubselect :Boolean = clause.isValidSubselect


	override def withClause :WithClause = clause.withClause
	override def collect[X](fun :PartialFunction[SQLExpression.*, X]) :Seq[X] = clause.collect(fun)

	protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Y = matcher.decorator(this)

}






object DecoratedFrom {


	/** A marker trait for [[net.noresttherein.oldsql.sql.RowProduct RowProduct]] decorators which include
	  * all relations from the decorated clause. This is the de facto real root of the decorator type hierarchy,
	  * with only the special [[net.noresttherein.oldsql.sql.Aggregated Aggregated]] adapter not being its subtype.
	  * It is analogous to the [[net.noresttherein.oldsql.sql.Expanded Expanded]] base trait for joins and join-like
	  * classes.
	  */
	trait ExpandingDecorator[+F <: RowProduct] extends DecoratedFrom[F] with ExpandingClause[F] {
		override def isEmpty :Boolean = clause.isEmpty
		override def fullSize :Int = clause.fullSize

		override type LastMapping[O] = clause.LastMapping[O]
		override type Last[O <: RowProduct] = clause.Last[O]
		override type FullRow = clause.FullRow
		override type Row = clause.Row
		override type OuterRow = clause.OuterRow


		protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Y =
			matcher.expandingDecorator(this)

		private[sql] override def concrete_ExpandingClause_subclass_must_extend_Expanded_or_ExpandedDecorator :Nothing =
			throw new UnsupportedOperationException
	}



	/** Default base trait for non empty `DecoratedFrom` implementations. Introduces the type constructors
	  * for the `Generalized` and `Self` types and implements most `RowProduct` methods by delegating to the
	  * underlying clause. It requires all implementing classes to be applicable to any non empty clause `F`.
	  */
	trait FromSomeDecorator[+F <: FromSome] extends ExpandingDecorator[F] with FromSome { thisClause =>

		override type LastMapping[O] = clause.LastMapping[O]

		override type Bound = FromSome
		override type FromLast = GeneralizedClause[clause.FromLast]
		override type Generalized = GeneralizedClause[clause.Generalized]
		override type Dealiased = WithClause[clause.Dealiased]
		override type Self = WithClause[clause.Self]

		override type Copy = WithClause[clause.Copy] {
			type FromLast = thisClause.FromLast
			type Generalized = thisClause.Generalized
			type Dealiased = thisClause.Dealiased
			type Params = thisClause.Params
			type FullRow = thisClause.FullRow
			type Explicit = thisClause.Explicit
			type Inner = thisClause.Inner
			type Implicit = thisClause.Implicit
			type Outer = thisClause.Outer
			type Base = thisClause.Base
			type DefineBase[+I <: RowProduct] = thisClause.DefineBase[I]
			type Row = thisClause.Row
			type OuterRow = thisClause.OuterRow
		}


		override def last :Last[FromLast] = clause.lastAsIn[FromLast]


		type GeneralizedClause[+G <: FromSome] <: FromSomeDecorator[G] {
			type GeneralizedClause[+S <: FromSome] = thisClause.GeneralizedClause[S]
		}

		type DealiasedLeft[+G <: FromSome] <: GeneralizedClause[G] {
			type DealiasedLeft[+S <: FromSome] = thisClause.DealiasedLeft[S]
		}

		//the defining type because As overrides it.
		type WithLeft[+G <: FromSome] <: DealiasedLeft[G] {
			type WithLeft[+S <: FromSome] = thisClause.WithLeft[S]
		}
		type WithClause[+G <: FromSome] = WithLeft[G]

		def withClause[C <: FromSome](from :C) :WithClause[C]


		override type JoinFilter = clause.JoinFilter

		override def filtered(condition :clause.JoinFilter) :Copy =
			withClause(clause.filtered(condition))


		//todo: AppliedParam
		//fixme: DecoratedParamless with two type params to handle GeneralizedParamless
		override type GeneralizedParamless = clause.DecoratedParamless[WithClause[clause.Paramless]]
		override type Paramless = clause.DecoratedParamless[WithClause[clause.Paramless]]
		override type DecoratedParamless[D <: BoundParamless] = clause.DecoratedParamless[D]

		override def bind(params :Params) :Paramless =
			decoratedBind[BoundParamless, WithClause[clause.Paramless]](clause)(params)(withClause[clause.Paramless])

		protected override def decoratedBind[D <: BoundParamless]
		                                    (params :Params)(decorate :Paramless => D) :DecoratedParamless[D] =
			decoratedBind[BoundParamless, D]( //the cast is safe because the function is called only when Paramless = WithClause[clause.Paramless]
				clause)(params)(withClause[clause.Paramless] _ andThen decorate.asInstanceOf[WithClause[clause.Paramless] => D]
			)



		override type FullRow = clause.FullRow

		override def fullRow[E <: RowProduct]
		             (target :E)(implicit expansion :Generalized ExpandedBy E) :ChainTuple[E, GlobalScope, FullRow] =
			clause.fullRow(target)(expansion.unwrapFront)

		override def fullTableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.fullTableStack(target)(expansion.unwrapFront)


		override type JoinedWith[+P <: RowProduct, +J[+L <: P, R[O] <: MappingAt[O]] <: L NonParam R] =
			WithClause[clause.JoinedWith[P, J]]

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :Join.*) :JoinedWith[P, firstJoin.LikeJoin] =
			withClause(clause.joinedWith(prefix, firstJoin))

		override type SelectedFrom[+P <:  NonEmptyFrom] = WithClause[clause.SelectedFrom[P]]

		override def selectedFrom[P <: NonEmptyFrom](prefix :P) :SelectedFrom[P] =
			withClause(clause.selectedFrom(prefix))

		override def appendedTo[P <: FromClause](prefix :P) :WithClause[clause.JoinedWith[P, NonParam]] =
			withClause(clause.appendedTo(prefix))



		override type Explicit = GeneralizedClause[clause.Explicit]
		override type Inner = WithClause[clause.Inner]
		override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
		override type DefineBase[+I <: RowProduct] = clause.DefineBase[I]

		override def base :Base = clause.base

		override def filter[E <: RowProduct]
		                   (target :E)(implicit expansion :Generalized PartOf E) :GlobalBoolean[E] =
			clause.filter(target)(expansion.unwrapFront)

		override type Row = clause.Row

		override def row[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:ChainTuple[E, GlobalScope, clause.Row] =
			clause.row(target)(expansion.unwrapFront)

		override def tableStack[E <: RowProduct](target :E)(implicit expansion :Generalized ExpandedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.tableStack(target)(expansion.unwrapFront)

		override type OuterRow = clause.OuterRow

		override def outerRow[E <: RowProduct](target :E)(implicit expansion :Implicit ExpandedBy E)
				:ChainTuple[E, GlobalScope, clause.OuterRow] =
			clause.outerRow(target)

		override type AsSubselectOf[+P <: NonEmptyFrom] = WithClause[clause.AsSubselectOf[P]]

		override def asSubselectOf[P <: NonEmptyFrom](newOuter :P)(implicit expansion :Implicit ExpandedBy P)
				:WithClause[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
			withClause(clause.asSubselectOf(newOuter))


		protected override def applyTo[Y](matcher :RowProductVisitor[Y]) :Y =
			matcher.fromSomeDecorator(this)

	}



	object FromSomeDecorator {

		@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} and the " +
		                  "decorated clause ${C}.\nMissing implicit FromSomeDecoratorComposition[${F}, ${C}, ${D}].")
		class FromSomeDecoratorComposition
		            [F <: D[C], C <: FromSome,
		             D[+B <: FromSome] <: FromSomeDecorator[B] { type WithLeft[+P <: FromSome] <: D[P] }]
			extends DecoratorComposition[F, C, D, FromSome]
		{ self =>
//			override type G[+A >: C <: FromSome] = Generalized[A]
			type Generalized[+A <: FromSome] >: D[A] <: FromSomeDecorator[A]

			@inline final override def apply[B <: FromSome](template :F, clause :B) :D[B] = template.withClause(clause)

		}

		implicit def fromSomeDecoratorComposition
		             [F <: D[C], C <: FromSome,
		              U[+B <: FromSome] >: D[B] <: FromSomeDecorator[B] { type GeneralizedClause[+P <: FromSome] = U[P] },
		              D[+B <: FromSome] <: FromSomeDecorator[B] {
			              type WithLeft[+P <: FromSome] <: D[P]; type GeneralizedClause[+P <: FromSome] = U[P]
		             }]
				:FromSomeDecoratorComposition[F, C, D] { type Generalized[+B <: FromSome] = U[B] } =
			composition.asInstanceOf[FromSomeDecoratorComposition[F, C, D] {
				type Generalized[+B <: FromSome] = U[B]
			}]

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
	  * [[net.noresttherein.oldsql.sql.Aliased Aliased]], which takes the clause as its first argument.
	  * This order can be important for readability reasons, so to proof various implicit factory methods
	  * which recursively scan `RowProduct` subtypes against any future decorator extensions which would break
	  * the type inference preventing from the materializing of the implicit value, the factory method can rely on
	  * this type class. Any decorator implementations for which default resolution would fail can provide an implicit
	  * of this type with the proper deconstruction in its companion object.
	  */
	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} " +
	                  "and the decorated clause ${C}.\nMissing implicit DecoratorDecomposition[${F}, ${C}, ${D}, ${U}].")
	class DecoratorDecomposition[-F <: D[C], C <: U, D[+B <: U] <: ExpandingDecorator[B], U <: RowProduct]
		extends RowDecomposition[F, C, U]
	{
		override type E[+A <: U] = D[A]
		override type S[+A >: C <: U] = D[A]

		@inline final override def prefix[A >: C <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]
		@inline final override def expansion[A <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]

		@inline final override def unapply(decorator :F) :C = decorator.clause

		override def upcast[A >: C <: U] :DecoratorDecomposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorDecomposition[D[A], A, D, U]]

		override def cast[A <: U] :DecoratorDecomposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorDecomposition[D[A], A, D, U]]
	}


	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} and " +
	                  "the decorated clause ${C}.\nMissing implicit DecoratorDecomposition[${F}, ${C}, ${D}, ${U}].")
	abstract class DecoratorComposition[F <: D[C], C <: U, D[+B <: U] <: ExpandingDecorator[B], U <: RowProduct]
		extends DecoratorDecomposition[F, C, D, U] with RowComposition[F, C, U]


	implicit def DecoratorDecomposition[D[+C <: FromSome] <: FromSomeDecorator[C], F <: FromSome]
			:DecoratorDecomposition[D[F], F, D, FromSome] =
		decomposition.asInstanceOf[DecoratorDecomposition[D[F], F, D, FromSome]]

	private[this] val decomposition =
		new DecoratorDecomposition[FromSomeDecorator[FromSome], FromSome, FromSomeDecorator, FromSome]

	private[this] val composition =
		new FromSomeDecoratorComposition[FromSomeDecorator[FromSome], FromSome, FromSomeDecorator]
}
