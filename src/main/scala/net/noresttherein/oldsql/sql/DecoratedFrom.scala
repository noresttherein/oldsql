package net.noresttherein.oldsql.sql

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.sql.FromClause.{ExtendedBy, FromSome}
import net.noresttherein.oldsql.sql.MappingSQL.{JoinedRelation, RelationSQL}
import net.noresttherein.oldsql.sql.SQLTerm.True
import net.noresttherein.oldsql.sql.TupleSQL.ChainTuple



/** Base trait for all `FromClause` implementations which serve as wrappers to other clauses. Decorators introduce
  * no new relations or ''where'' clause fragments and in most cases simply delegate to the corresponding method
  * of the wrapped clause `F`. This is a bare bones root type designed for flexibility and convenience
  * due to no upper bound on the wrapped clause type. Most practical implementations will likely prefer to extend
  * the more fleshed out [[net.noresttherein.oldsql.sql.DecoratedFrom.GenericDecorator GenericDecorator]].
  * @author Marcin Mościcki
  */
trait DecoratedFrom[+F <: FromClause] extends FromClause { //this could benefit from being derived from Any
	val clause :F

	override type LastMapping[O] = clause.LastMapping[O]

	override def isEmpty :Boolean = clause.isEmpty
	override def size :Int = clause.size

	override type Init = clause.Init

	/** Wraps a (possibly modified) copy of the underlying clause in a new decorator instance. */
	def copy[C <: clause.FromLast](body :C) :DecoratedFrom[C]

	override type Row = clause.Row



	override type Implicit = clause.Implicit
	override type Outer = clause.Outer

	override def outer :Outer = clause.outer

	override def isSubselect = clause.isSubselect
	override def subselectSize :Int = clause.subselectSize

	override type SubselectRow = clause.SubselectRow


	override type Params = clause.Params

}






object DecoratedFrom {


	type FromSomeDecorator[+F <: FromSome] = FromSome with DecoratedFrom[F]

	/** Default base trait for non empty `DecoratedFrom` implementations. Introduces the type constructors
	  * for the `Generalized` and `Self` types and implements most `FromClause` methods by delegating to the
	  * underlying clause. It requires all implementing classes to be applicable to any non empty clause `F`.
	  */
	trait GenericDecorator[+F <: FromSome] extends FromSome with DecoratedFrom[F] { thisClause =>

		override type FromLast = GeneralizedClause[clause.FromLast]

		override def last :JoinedRelation[FromLast, LastMapping] = clause.last.extend[FromLast]


		type GeneralizedClause[+G <: FromSome] <: GenericDecorator[G] {
			type GeneralizedClause[+S <: FromSome] = thisClause.GeneralizedClause[S]
		}

		type WithClause[+G <: FromSome] <: GeneralizedClause[G] {
			type GeneralizedClause[+S <: FromSome] = thisClause.GeneralizedClause[S]
			type WithClause[+S <: FromSome] = thisClause.WithClause[S]
		}

		override type Generalized = GeneralizedClause[clause.Generalized]
		override type Self = WithClause[clause.Self]
		override type This <: WithClause[clause.This]

		override def copy[C <: clause.FromLast](from :C) :WithClause[C] = withClause(from)

		def withClause[C <: FromSome](from :C) :WithClause[C]

//		override def where(filter :SQLBoolean[Generalized]) :This =
//			copy(clause.where(filter.asInstanceOf[SQLBoolean[clause.Generalized]])) //todo: eliminate the cast



		override def filter[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
			clause.filter(target)(extension.unwrapFront)

		override def row[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E) :ChainTuple[E, Row] =
			clause.row(target)(extension.unwrapFront)

		override def tableStack[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.tableStack(target)(extension.unwrapFront)


		override type AppendedTo[+P <: FromClause] = WithClause[clause.AppendedTo[P]]

		override def appendedTo[P <: FromClause](prefix :P) :WithClause[clause.AppendedTo[P]] =
			withClause(clause.appendedTo(prefix))

		override type JoinedWith[+P <: FromSome, +J[+L <: FromSome, R[O] <: MappingAt[O]] <: L Join R] =
			WithClause[clause.JoinedWith[P, J]]

		override def joinedWith[P <: FromSome](prefix :P, firstJoin :TrueJoin.*) :JoinedWith[P, firstJoin.LikeJoin] =
			withClause(clause.joinedWith(prefix, firstJoin))

		override def joinedAsSubselect[P <: FromSome](prefix :P) :JoinedWith[P, Subselect] =
			withClause(clause.joinedAsSubselect(prefix))



		override type Explicit = GeneralizedClause[clause.Explicit]
		override type Inner = WithClause[clause.Inner]

		override def subselectFilter[E <: FromSome]
		                            (target :E)(implicit extension :Generalized ExtendedBy E) :SQLBoolean[E] =
			clause.subselectFilter(target)(extension.unwrapFront)

		override def subselectRow[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E)
				:ChainTuple[E, clause.SubselectRow] =
			clause.subselectRow(target)(extension.unwrapFront)

		override def subselectTableStack[E <: FromSome](target :E)(implicit extension :Generalized ExtendedBy E)
				:LazyList[RelationSQL.AnyIn[E]] =
			clause.subselectTableStack(target)(extension.unwrapFront)


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
				join.left.toString + " " + join.joinName + " " + join.right + " as " + alias + " on " + join.condition
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

		implicit def aliasUpcasting[F <: FromSome, A <: Label] :DecoratorUpcasting[F Alias A, WithLabel[A]#F, F] =
			upcasting.asInstanceOf[DecoratorUpcasting[F Alias A, WithLabel[A]#F, F]]
	}



	/** Implicit witness providing a type constructor for the decorator type `T`. It is used in type inference
	  * to disassemble the type `T` into the decorator constructor `D` and the decorated clause `F`.
	  * Tpe parameters in the form of `[D[+C &lt;: FromSome] &lt;: FromSomeDecorator, F &lt;: FromSome]`
	  * can be properly inferred from the type `T` if it is a type formed from applying the single argument
	  * type constructor directly to the decorated type (for example, for `T =:= GenericDecorator[C]`, the types
	  * would be properly instantiated as `D[X] =:= GenericDecorator[X]` and `F =:= C`) or if `T` is formed
	  * from applying a multi argument type constructor, with the decorated clause being the last type parameter
	  * (so, for `trait Deco[T, +F &lt;: FromSome] extends GenericDecorator`, `Deco[T, C]` will be correctly unified as
	  * `D[X] =:= Deco[T, X], F =:= C`). However, if the `F` type argument is not the last one, or the type definition
	  * is more complex, automatic partial unification will fail - as it would for
	  * [[net.noresttherein.oldsql.sql.DecoratedFrom.Alias Alias]], which takes the clause as its first argument.
	  * This order can be important for readability reasons, so to proof various implicit factory methods
	  * which recursively scan `FromClause` subtypes against any future decorator extensions which would break
	  * the type inference preventing from the materializing of the implicit value, the factory method can rely on
	  * this type class. Any decorator implementations for which default resolution would fail can provide an implicit
	  * of this type with the proper deconstruction in its companion object.
	  */
	@implicitNotFound("I do not know how to separate the FROM clause ${T} into a DecoratedFrom type constructor ${D} " +
	                  "and the decorated clause ${F}.")
	class DecoratorUpcasting[-T <: D[F], +D[+C <: FromSome] <: FromSomeDecorator[C], +F <: FromSome]

	implicit def DecoratorUpcasting[D[+C <: FromSome] <: FromSomeDecorator[C], F <: FromSome]
			:DecoratorUpcasting[D[F], D, F] =
		upcasting.asInstanceOf[DecoratorUpcasting[D[F], D, F]]


	private[this] val upcasting = new DecoratorUpcasting[FromSomeDecorator[FromSome], FromSomeDecorator, FromSome]
}
