package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.MappingAt
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.DiscreteFrom.FromSome
import net.noresttherein.oldsql.sql.FromClause.{ClauseComposition, ExtendedBy, PrefixOf}
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
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
  * the more fleshed out [[net.noresttherein.oldsql.sql.DecoratedFrom.GenericDecorator GenericDecorator]].
  * @author Marcin Mościcki
  */ //this could benefit from being derived from Any
trait DecoratedFrom[+F <: FromClause] extends FromClause { thisClause =>
	val clause :F

	override type LastMapping[O] = clause.LastMapping[O]

	override def isEmpty :Boolean = clause.isEmpty
	override def fullSize :Int = clause.fullSize

	/** Wraps a (possibly modified) copy of the underlying clause in a new decorator instance. */
	def copy[C <: clause.FromLast](body :C) :DecoratedFrom[C]


	override type Params = clause.Params

	override def isParameterized :Boolean = clause.isParameterized

	override type FullRow = clause.FullRow



	override type Implicit = clause.Implicit
	override type Outer = clause.Outer
	override type Base = DefineBase[Implicit]

	override def outer :Outer = clause.outer

	override def isSubselect :Boolean = clause.isSubselect
	override def isValidSubselect :Boolean = clause.isValidSubselect
	override def innerSize :Int = clause.innerSize


	override type InnerRow = clause.InnerRow
	override type OuterRow = clause.OuterRow

}






object DecoratedFrom {


	type FromSomeDecorator[+F <: FromSome] = FromSome with DecoratedFrom[F]

	/** Default base trait for non empty `DecoratedFrom` implementations. Introduces the type constructors
	  * for the `Generalized` and `Self` types and implements most `FromClause` methods by delegating to the
	  * underlying clause. It requires all implementing classes to be applicable to any non empty clause `F`.
	  */
	trait GenericDecorator[+F <: FromSome] extends FromSome with DecoratedFrom[F] { thisClause =>

		override type FromLast = GeneralizedClause[clause.FromLast]
		override type Generalized = GeneralizedClause[clause.Generalized]
		override type Self = WithClause[clause.Self]
		override type This <: WithClause[clause.This]

		override def last :JoinedRelation[FromLast, LastMapping] = clause.last.extend[FromLast]

		type GeneralizedClause[+G <: FromSome] <: GenericDecorator[G] {
			type GeneralizedClause[+S <: FromSome] = thisClause.GeneralizedClause[S]
		}

		type WithClause[+G <: FromSome] <: GeneralizedClause[G] {
			type WithClause[+S <: FromSome] = thisClause.WithClause[S]
		}

		override def copy[C <: clause.FromLast](from :C) :WithClause[C] = withClause(from)

		def withClause[C <: FromSome](from :C) :WithClause[C]



		override def fullFilter[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
			clause.fullFilter(target)(extension.unwrapFront)

		override def fullRow[E <: FromClause]
		                    (target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, FullRow] =
			clause.fullRow(target)(extension.unwrapFront)

		override def fullTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.fullTableStack(target)(extension.unwrapFront)


		override type AppendedTo[+P <: DiscreteFrom] = WithClause[clause.AppendedTo[P]]

		override def appendedTo[P <: DiscreteFrom](prefix :P) :WithClause[clause.AppendedTo[P]] =
			withClause(clause.appendedTo(prefix))

		override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
			WithClause[clause.JoinedWith[P, J]]

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :TrueJoin.*) :JoinedWith[P, firstJoin.LikeJoin] =
			withClause(clause.joinedWith(prefix, firstJoin))

		override type JoinedWithSubselect[+P <: FromSome] = WithClause[clause.JoinedWithSubselect[P]]

		override def joinedWithSubselect[P <: FromSome](prefix :P) :JoinedWithSubselect[P] =
			withClause(clause.joinedWithSubselect(prefix))



		override type Explicit = GeneralizedClause[clause.Explicit]
		override type Inner = WithClause[clause.Inner]
		override type Base = clause.DefineBase[clause.Implicit] //a supertype of clause.Base (in theory, equal in practice)
		override type DefineBase[+I <: FromClause] = clause.DefineBase[I]

		override def base :Base = clause.base

		override def filter[E <: FromClause]
		                   (target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
			clause.filter(target)(extension.unwrapFront)

		override def innerRow[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:ChainTuple[E, clause.InnerRow] =
			clause.innerRow(target)(extension.unwrapFront)

		override def innerTableStack[E <: FromClause](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.innerTableStack(target)(extension.unwrapFront)

		override def outerRow[E <: FromClause](target :E)(implicit extension :Implicit ExtendedBy E)
				:ChainTuple[E, clause.OuterRow] =
			clause.outerRow(target)

		override type AsSubselectOf[+P <: FromSome] = WithClause[clause.AsSubselectOf[P]]

		override def asSubselectOf[P <: FromSome](newOuter :P)(implicit extension :Implicit ExtendedBy P)
				:WithClause[clause.AsSubselectOf[P]] { type Implicit = newOuter.Generalized; type Outer = newOuter.Self } =
			withClause(clause.asSubselectOf(newOuter))


	}



	/** Introduces an alias name to the last relation in the wrapped clause, translating to an `as` keyword
	  * in the underlying SQL.
	  */
	class Alias[+F <: FromSome, N <: Label](override val clause :F, val alias :N) extends GenericDecorator[F] {

		override type GeneralizedClause[+G <: FromSome] = G Alias N
		override type WithClause[+G <: FromSome] = G Alias N
		override type This = clause.This Alias N

		override def withClause[G <: FromSome](from :G) :G Alias N = Alias(from, alias)

		override def where(filter :SQLBoolean[Generalized]) :This =
			withClause(clause where filter.asInstanceOf[SQLBoolean[clause.Generalized]]) //todo: eliminate the cast


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Alias[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case as :Alias[_, _] if as canEqual this => clause == as.clause && alias == as.alias
			case _ => false
		}

		override def hashCode :Int = clause.hashCode * 31 + alias.hashCode

		override def toString :String = clause match {
			case join :AndFrom.* if join.condition != True => //inject the 'as' alias before the condition to reduce confusion.
				join.left.toString + " " + join.name + " " + join.right + " as " + alias + " on " + join.condition
			case _ => clause.toString + " as " + alias
		}
	}



	object Alias {
		def apply[F <: FromSome, N <: Label](from :F, name :N) :F Alias N =
			new Alias(from, name)

		def unapply(clause :FromClause) :Option[(FromSome, Label)] = clause match {
			case alias :Alias[_, _] => Some((alias.clause, alias.alias))
			case _ => None
		}

		def unapply[F <: FromClause](clause :DecoratedFrom[F]) :Option[(F, Label)] = clause match {
			case alias :Alias[_, _] => Some((clause.clause, alias.alias))
			case _ => None
		}

		def unapply[F <: FromSome, N <: Label](alias :Alias[F, N]) :Some[(F, N)] =
			Some((alias.clause, alias.alias))

		type * = Alias[_ <: FromSome, _ <: Label]

		type WithLabel[A <: Label] = { type F[+C <: FromSome] = C Alias A }
		type WithClause[C <: FromSome] = { type F[A <: Label] = C Alias A }


		implicit def aliasDecomposition[F <: FromSome, A <: Label]
				:DecoratorComposition[F Alias A, F, WithLabel[A]#F, FromSome] =
			decomposition.asInstanceOf[DecoratorComposition[F Alias A, F, WithLabel[A]#F, FromSome]]

	}



	/** Implicit witness providing a type constructor for the decorator type `F`. It is used in type inference
	  * to disassemble the type `F` into the decorator constructor `D` and the decorated clause `C`.
	  * Type parameters in the form of `[D[+C &lt;: FromSome] &lt;: FromSomeDecorator, C &lt;: FromSome]`
	  * can be properly inferred from the type `F` if it is a type formed from applying the single argument
	  * type constructor directly to the decorated type (for example, for `F =:= GenericDecorator[C]`, the types
	  * would be properly instantiated as `D[X] =:= GenericDecorator[X]` and `C =:= C`) or if `F` is formed
	  * from applying a multi argument type constructor, with the decorated clause being the last type parameter
	  * (so, for `trait Deco[F, +C &lt;: FromSome] extends GenericDecorator[C]`, `Deco[F, C]` will be correctly unified as
	  * `D[X] =:= Deco[F, X], C =:= C`). However, if the `C` type argument is not the last one, or the type definition
	  * is more complex, automatic partial unification will fail - as it would for
	  * [[net.noresttherein.oldsql.sql.DecoratedFrom.Alias Alias]], which takes the clause as its first argument.
	  * This order can be important for readability reasons, so to proof various implicit factory methods
	  * which recursively scan `FromClause` subtypes against any future decorator extensions which would break
	  * the type inference preventing from the materializing of the implicit value, the factory method can rely on
	  * this type class. Any decorator implementations for which default resolution would fail can provide an implicit
	  * of this type with the proper deconstruction in its companion object.
	  */
	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} " +
	                  "and the decorated clause ${C}.\nMissing implicit DecoratorUpcasting[${F}, ${D}, ${C}, ${U}].")
	sealed abstract class DecoratorUpcasting[-F <: D[C], +C <: U, +D[+B <: U] <: DecoratedFrom[B], U <: FromClause]


	@implicitNotFound("I do not know how to decompose ${F} into a DecoratedFrom type constructor ${D} " +
	                  "and the decorated clause ${C}.\nMissing implicit DecoratorComposition[${F}, ${C}, ${D}, ${U}].")
	final class DecoratorComposition[F <: D[C], C <: U, D[+B <: U] <: DecoratedFrom[B], U <: FromClause]
		extends DecoratorUpcasting[F, C, D, U] with ClauseComposition[F, C, U]
	{
		override type E[+A <: U] = D[A]
		override type S[+A >: C <: U] = D[A]

		@inline override def prefix[A >: C <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]
		@inline override def extension[A <: U] :A PrefixOf D[A] = PrefixOf.itself[A].wrap[D]

		@inline override def apply(decorator :F) :C = decorator.clause

		override def upcast[A >: C <: U] :DecoratorComposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorComposition[D[A], A, D, U]]

		override def cast[A <: U] :DecoratorComposition[D[A], A, D, U] =
			this.asInstanceOf[DecoratorComposition[D[A], A, D, U]]
	}


	implicit def DecoratorDecomposition[D[+C <: FromSome] <: FromSomeDecorator[C], F <: FromSome]
			:DecoratorComposition[D[F], F, D, FromSome] =
		decomposition.asInstanceOf[DecoratorComposition[D[F], F, D, FromSome]]

	private[this] val decomposition =
		new DecoratorComposition[FromSomeDecorator[FromSome], FromSome, FromSomeDecorator, FromSome]
}
