package net.noresttherein.oldsql.model

import java.{lang => j}

import net.noresttherein.oldsql.model.ComposedOf.{CollectionOf, DecomposableTo, ExtractAs}
import net.noresttherein.oldsql.model.Restraint.Comparison.{GT, GTE, LT, LTE}
import net.noresttherein.oldsql.model.Restraint.{Comparison, Equality, Exists, False, ForAll, IsNull, Membership, Restrainer, StringLike, True}
import net.noresttherein.oldsql.model.Restrictive.{ArithmeticRestrictive, Collection, ComposedRestrictive, Literal, NegatedRestrictive, SizeOf, SubclassRestrictive, SwitchRestrictive, TranslableTerm}
import net.noresttherein.oldsql.model.Restrictive.Arithmetic.{DIV, MINUS, MULT, Operator, PLUS, REM}
import net.noresttherein.oldsql.model.types.{ArithmeticSupport, OrderingSupport}
import net.noresttherein.oldsql.morsels.PropertyPath
import net.noresttherein.oldsql.slang._

import scala.reflect.runtime.universe.{typeOf, Type, TypeTag}
import scala.reflect.{classTag, ClassTag}
import scala.reflect.runtime.universe




/** Arbitrary term of type `V` which can be used to create `Restraint`s on type `T`.
  * May represent both values known statically (literals) and expressions being functions of `T`,
  * where `T` is some constrained entity. Instances of this type are typically tested with a predicate
  * or compared with other instances to produce Boolean conditions.
  * @see [[net.noresttherein.oldsql.model.Restraint.Restrainer Restrainer]]
  */
trait Restrictive[-T, V] extends (T=>V) with TranslableTerm[T, V] with implicits with Serializable {
	//todo: this being  a function conflicts with an implicit conversion providing 'apply(Int)' to String.

	/** Retrieve the value of part `V` described by this instance from the outer type `T`.*/
	def apply(whole :T) :V

	def andThen[P](next :Restrictive[V, P]) :Restrictive[T, P] = next compose this

	def compose[X, S<:T](nest :Restrictive[X, S]) :Restrictive[X, V] = nest match {
		case _ :ComposedRestrictive[_, _, _] => nest andThen this
		case _ => ComposedRestrictive(nest, this)
	}

	def definedFor :Type


	//todo: create a special type class for types which class/type can be tested; this makes little sense in most cases in SQL
	def ifInstanceOf[S :ClassTag, O](subclass :Restrictive[S, O]) :Restrictive[T, Option[O]] =
		new SubclassRestrictive(this, implicitly[ClassTag[S]].runtimeClass, subclass)

	//todo: make the first parameter of the tuple accept literals.
	/** Create a 'switch' expression comparing the value of this term with the given sequence of guard values
	  * and selecting the branch for the matching guard or the default result in case of no match.
	  * This transaltes into a 'DECODE'-like expression in SQL.
	  * @param cases: a sequence of pairs associating guard values to compare with this term with alternative branch
	  *             expressions
	  * @param default the expression serving as the catch-all branch selected when no guards in `cases` matches this term.
	  */
	def cases[S<:T, O](cases :(TranslableTerm[S, V], Restrictive[S, O])*)(default :Restrictive[S, O]) :Restrictive[S, O] =
		new SwitchRestrictive[S, V, O](this, cases.map { case (guard, branch) => (guard(), branch) }, default)


	def unary_-(implicit math :ArithmeticSupport[V]) :Restrictive[T, V] =
		new NegatedRestrictive(this)

	def +[S<:T](other :TranslableTerm[S, V])(implicit math :ArithmeticSupport[V]) :Restrictive[S, V] =
		new ArithmeticRestrictive(this, PLUS, other())

	def -[S<:T](other :TranslableTerm[S, V])(implicit math :ArithmeticSupport[V]) :Restrictive[S, V] =
		new ArithmeticRestrictive(this, MINUS, other())

	def *[S<:T](other :TranslableTerm[S, V])(implicit math :ArithmeticSupport[V]) :Restrictive[S, V] =
		new ArithmeticRestrictive(this, MULT, other())

	def /[S<:T](other :TranslableTerm[S, V])(implicit math :ArithmeticSupport[V]) :Restrictive[S, V] =
		new ArithmeticRestrictive(this, DIV, other())

	def %[S<:T](other :TranslableTerm[S, V])(implicit math :ArithmeticSupport[V]) :Restrictive[S, V] =
		new ArithmeticRestrictive(this, REM, other())


	/** Create a `Restrainer` testing if this term is less then the value of the key supplied as its parameter. */
	def ?< (implicit ordered :OrderingSupport[V]) :Restrainer[T, V] = Comparison(this, LT)
	/** Create a `Restrainer` testing if this term is less then or equal the value of the key supplied as its parameter. */
	def ?<=(implicit ordered :OrderingSupport[V]) :Restrainer[T, V] = Comparison(this, LTE)
	/** Create a `Restrainer` testing if this term is greater then the value of the key supplied as its parameter. */
	def ?> (implicit ordered :OrderingSupport[V]) :Restrainer[T, V] = Comparison(this, GT)
	/** Create a `Restrainer` testing if this term is greater then or equal the value of the key supplied as its parameter. */
	def ?>=(implicit ordered :OrderingSupport[V]) :Restrainer[T, V] = Comparison(this, GTE)


	/** Creates a `Restraint` mandating that this term be less then `other`. */
	def < [S <: T](other :TranslableTerm[S, V])(implicit ordered :OrderingSupport[V]) :Restraint[S] =
		Comparison(this, LT, other())

	/** Creates a `Restraint` mandating that this term be less then or equal `other`. */
	def <=[S <: T](other :TranslableTerm[S, V])(implicit ordered :OrderingSupport[V]) :Restraint[S] =
		Comparison(this, LTE, other())

	/** Creates a `Restraint` mandating that this term be greater then `other`. */
	def > [S <: T](other :TranslableTerm[S, V])(implicit ordered :OrderingSupport[V]) :Restraint[S] =
		Comparison(this, GT, other())

	/** Creates a `Restraint` mandating that this term be greater then or equal `other`. */
	def >=[S <: T](other :TranslableTerm[S, V])(implicit ordered :OrderingSupport[V]) :Restraint[S] =
		Comparison(this, GTE, other())


//	/** Creates a `Restraint` mandating that this term be less then `other`. */
//	def < [S <: T](other :V)(implicit ordered :OrderingSupport[V]) :Restraint[S] = this <  Literal(other)
//
//	/** Creates a `Restraint` mandating that this term be less then or equal `other`. */
//	def <=[S <: T](other :V)(implicit ordered :OrderingSupport[V]) :Restraint[S] = this <= Literal(other)
//
//	/** Creates a `Restraint` mandating that this term be greater then `other`. */
//	def > [S <: T](other :V)(implicit ordered :OrderingSupport[V]) :Restraint[S] = this >  Literal(other)
//
//	/** Creates a `Restraint` mandating that this term be greater then or equal `other`. */
//	def >=[S <: T](other :V)(implicit ordered :OrderingSupport[V]) :Restraint[S] = this >= Literal(other)



	/** Checks if this `String` value matches a pattern string.
	  * @param pattern a string pattern in the 'command line' format, i.e. ussing '?' and '*' as wildcard symbols
	  *                escaped with '\'.
	  */
	def like[S<:T](pattern :String)(implicit ev :Restrictive[S, V] <:< Restrictive[S, String]) :Restraint[S] =
		StringLike(ev(this), pattern)



	/** Tests if this term is null using an explicit 'is null' SQL expression. */
	def isNull :Restraint[T] = IsNull(this)

	/** Create a `Restrainer` for T which will compare this expression with a given precomputed value of type `V`. */
	def ?== :Restrainer[T, V] = Equality(this)

	/** Create a restraint testing this expression and another expression for equality. Equivalent to `Equality(this, other)`. */
	def ===[S<:T](other :TranslableTerm[S, V]) :Restraint[S] = Equality(this, other())

//	/** Create a restriction testing this expression for equality with the given precomputed value. */
//	def ===(other :V) :Restraint[T] = Equality(this, Literal[T, V](other))


	/** Create a `Restrainer` for `T` which will test if this expression is a member of the given set of
	  * precomputed values of type `V`.
	  */
	def in_? :Restrainer[T, Set[V]] = Membership(this)

	/** Checks if a given collection expression - either an inlined sequence or an embedded select - contains this value. */
	def in[C, S<:T](collection :TranslableTerm[S, C])(implicit deco :C ExtractAs V) :Restraint[S] =
		Membership(this, collection())

	/** Create a restriction testing if the value of this expression is a member of the given collection.
	  * Equivalent to `Membership(this, collection)`.
	  */
	def in[S<:T](collection :Iterable[Restrictive[S, V]]) :Restraint[S] = Membership(this, collection)

//	/** Create a `Restriction` checking if this value is a member of the given collection of literals. */
//	def in[C](collection :C)(implicit decomposition :C ExtractAs V) :Restraint[T] =
//		Membership(this, decomposition(collection).map(Literal.apply[T, V]))


	/** Checks if this expression, which must be a collection expression representing either an inlined SQL sequence
	  * or an embedded select, contains an element satisfying the given predicate.
	  */
	def exists[E](predicate :Restraint[E])(implicit items :V CollectionOf E) :Restraint[T] =
		Exists(this, predicate)

	/** Checks if the given predicate holds for all elements of this collection. */
	def forall[E](predicate :Restraint[E])(implicit items :V CollectionOf E) :Restraint[T] =
		ForAll(this, predicate)

	/** An expression returning the size of this collection: a 'select count(*)' if this term is an embedded select
	  * or a precomputed value if this term is an inlined collection.
	  */
	def size[E](implicit composite :V CollectionOf E) :Restrictive[T, Int] =
		SizeOf(this)



	def canEqual(that :Any) :Boolean = that.getClass == getClass
}






/** Factory for `Restrictive` instances - constrainable terms which can be used to create Restraints. */
object Restrictive {

	/** Create a constrainable term representing a value of type T. Equivalent to `Self[T]`. */
	@inline def apply[T :TypeTag] :Restrictive[T, T] = Self[T]()


	
	abstract class <%<[S, T] extends (S=>T)

	abstract class =%=[S, T] extends (S<%<T) {
		def apply(left :S) :T = right(left)
		def right(left :S) :T
		def left(right :T) :S
	}

	object <%< {
//		@inline implicit final def symmetrical[S, T](implicit equiv: S=%=T) :T=%=S = adapt(equiv.left, equiv.right)

		implicit final val (byteUnoxing, byteBoxing) = adapt((_:j.Byte).byteValue, j.Byte.valueOf :Byte=>j.Byte)
		implicit final val (shortUnboxing, shortBoxing) = adapt((_ :j.Short).shortValue, j.Short.valueOf :Short=>j.Short)
		implicit final val (intUnboxing, intBoxing) = adapt((_ :j.Integer).intValue, j.Integer.valueOf :Int=>j.Integer)
		implicit final val (longUnboxing, longBoxing) = adapt((_ :j.Long).longValue, j.Long.valueOf :Long=>j.Long)
		implicit final val (floatUnboxing, floatBoxing) = adapt((_ :j.Float).floatValue, j.Float.valueOf :Float=>j.Float)
		implicit final val (doubleUnboxing, doubleBoxing) = adapt((_ :j.Double).doubleValue, j.Double.valueOf :Double=>j.Double)
		implicit final val (charUnboxing, charBoxing) = adapt((_ :j.Character).charValue, j.Character.valueOf :Char=>j.Character)
		implicit final val (booleanUnboxing, booleanBoxing) = adapt((_ :j.Boolean).booleanValue, j.Boolean.valueOf :Boolean=>j.Boolean)

		implicit final def nullToOption[X >: Null] :X=%=Option[X] = nullBoxing.asInstanceOf[X=%=Option[X]]
		implicit final def optionToNull[X >: Null] :Option[X]=%=X = nullUnboxing.asInstanceOf[Option[X]=%=X]
		private[this] final val (nullUnboxing, nullBoxing) = adapt((_ :Option[Null]).orNull, Option.apply :Null=>Option[Null])

		implicit final val byte2Short = adapt((_ :Byte).toShort)
		implicit final val byte2Int = adapt((_ :Byte).toInt)
		implicit final val byte2Long = adapt((_ :Byte).toLong)
		implicit final val byte2Float = adapt((_ :Byte).toFloat)
		implicit final val byte2Double = adapt((_ :Byte).toDouble)
		implicit final val short2Int = adapt((_ :Short).toInt)
		implicit final val short2Long = adapt((_ :Short).toLong)
		implicit final val short2Float = adapt((_ :Short).toFloat)
		implicit final val short2Double = adapt((_ :Short).toDouble)
		implicit final val int2Long = adapt((_ :Int).toLong)
		implicit final val int2Float = adapt((_ :Int).toFloat)
		implicit final val int2Double = adapt((_ :Int).toDouble)
		implicit final val long2Float = adapt((_ :Long).toFloat)
		implicit final val long2Double = adapt((_ :Long).toDouble)
		implicit final val float2Double = adapt((_ :Float).toDouble)

		@inline private def adapt[S, T](right :S=>T, left :T=>S) :(S=%=T, T=%=S) =
			(new adapt(right, left), new adapt(left, right))

		private class adapt[S, T](r :S=>T, l :T=>S) extends (S =%= T) {
			override def right(left :S) :T = r(left)
			override def left(right :T) :S = l(right)
		}
		
		@inline private def adapt[S, T](cast :S=>T) :S<%<T = { s :S => cast(s) }
	}
	





	/**  A type serving as a common umbrella for `Restrictive[T, V]` and arbitrary literal values.
	  *  The name does not mean that it contains information about its mapping to SQL, or even that it can be done.
	  *  Instead, the responsibility for ensuring this is carried by methods accepting it as an argument.
	  *  Without mapping information only standard types like `String` and numbers can be freely used
	  *  as SQL expressions, prohibiting creation of other literal expressions in most scenarios.
	  *  However, when a `Restrictive[T, V]` is being compared with another value, a it already carries the mapping
	  *  information (or serves as a witness to its existence) which can be used to translate the scala value of the same
	  *  type into an SQL literal. Accepting values of this type instead of `Restrictive` as a method parameter
	  *  allows both `Restrictive` instances and arbitrary literals lifted to this type by an implicit conversion.
	  *  This is essentially equivalent to using `Either[Restrictive[T, V], V]`, but with a shorter notation,
	  *  without the need for handling both cases individually by the method, and without creating an extra object
	  *  as extending types are mandated to implement `Restrictive[V, T]`.
	  */
	sealed trait TranslableTerm[-T, V] { this :Restrictive[T, V] =>
		@inline final private[Restrictive] def apply() :Restrictive[T, V] = this
	}






	/** Factory for constrainable terms representing (possibly nested) properties of some entities. */
	object Property {

		/** A constrainable expression representing a property with value type V of type T specified by the given reflected property. */
		@inline def apply[T, V](property :PropertyPath[T, V]) :Restrictive[T, V] = new PropertyRestrictive[T, V](property)

		/** A constrainable expression representing a property with value type V of type T specified by the given function.
		  * @param property a function returning some property of T, for example `_.address.street`.
		  */
		@inline def apply[T :TypeTag, V](property :T=>V) :Restrictive[T, V] = apply(PropertyPath(property))

		/** Check if the given expression represents a property of T (is an instance created by this factory). */
		def unapply[T, V](restrictive :Restrictive[T, V]) :Option[PropertyPath[T, V]] = restrictive match {
			case  property :PropertyRestrictive[T, V] => Some(property.property)
			case _ => None
		}
	}



	/** Factory for constrainable expressions representing constrained domain itself. */
	object Self {
		/** Expression denoting constrained value of type `T`. When seen as a function `T=>V`, it is an identity function. */
		def apply[T :TypeTag]() :Restrictive[T, T] = new SelfRestrictive[T, T](typeOf[T])

		/** Check if the given `Restrictive` is the value `T` itself created by this factory. */
		def unapply[T, V](restrictive :Restrictive[T, V]) :Option[Type] = restrictive match {
			case self :SelfRestrictive[_, _] => Some(self.definedFor)
			case _ => None
		}
	}



	/** Factory for constrainable expressions representing statically precomputed values.
	  * While they may contain any supported scala values, they are replaced by literals in the generated queries.
	  * Because this requires mapping to SQL expressions, it is possible only to create literals for selected
	  * standard types. In order to somewhat alleviate the problem, comparison expressions for a `Restrictive[T, V]`
	  * accept any value of type `V`, as its decomposition can be handled by the other instance which required
	  * that knowledge for its creation.
	  */
	object Literal {

		/** A literal expression of type V, seen as a function `T=>V` */
		@inline private[model] def apply[T, V](value :V) :Restrictive[T, V] = new LiteralRestrictive[T, V](value)

		/** Check if the given constrainee is a literal expression. */
		def unapply[V](restrictive :Restrictive[_, V]) :Option[V] = restrictive match {
			case literal :LiteralRestrictive[_, V] => Some(literal.value)
			case _ => None
		}

/*
		//todo: define what should go here
		trait LiteralSupport[T]

		object LiteralSupport {
			implicit final val ByteLiterals = new LiteralSupport[Byte]{}
			implicit final val ShortLiterals = new LiteralSupport[Short]{}
			implicit final val IntLiterals = new LiteralSupport[Int] {}
			implicit final val LongLiterals = new LiteralSupport[Long] {}
			implicit final val FloatLiterals = new LiteralSupport[Float] {}
			implicit final val DoubleLiterals = new LiteralSupport[Double] {}
			implicit final val CharLiterals = new LiteralSupport[Char]{}
			implicit final val BooleanLiterals = new LiteralSupport[Boolean]{}
			implicit final val BigIntLiterals = new LiteralSupport[BigInt]{}
			implicit final val bigDecimalLiterals = new LiteralSupport[BigDecimal]{}
			implicit final val StringLiterals = new LiteralSupport[String]{}
		}
*/

	}



	/** A factory and matcher for SQL expressions being inlined collections (that is, in the form of "('Minsc', 'Boo')". */
	object Collection {
		def apply[T, C, RC, E](values :RC)
		                      (implicit items :C ComposedOf E, restrictives :RC DecomposableTo Restrictive[T, E])
				:Restrictive[T, C] =
			new CollectionRestrictive(restrictives(values))

		def apply[T, C, E](values :Iterable[Restrictive[T, E]])(implicit items :C ComposedOf E) :Restrictive[T, C] =
			new CollectionRestrictive(values)



		def unapply[T, C](restrictive :Restrictive[T, C])
				:Option[(Iterable[Restrictive[T, E]], C ComposedOf E) forSome { type E }] =
			restrictive match {
				case col :CollectionRestrictive[T, C, e] => Some((col.values, col.composite))
				case _ =>None
			}

	}



	/** A factory and matcher for binary arithmetic operators (such as '+' and '/'). */
	object Arithmetic {
		//todo: having ArithmeticSupport should mean we also have support for literals

		def apply[T, N :ArithmeticSupport](left :Restrictive[T, N], op :Operator, right :TranslableTerm[T, N]) :Restrictive[T, N] =
			new ArithmeticRestrictive(left, op, right())

		def apply[T, N :ArithmeticSupport](left :TranslableTerm[T, N], op :Operator, right :Restrictive[T, N]) :Restrictive[T, N] =
			new ArithmeticRestrictive(left(), op, right)



		def unapply[T, N](term :Restrictive[T, N]) :Option[(Restrictive[T, N], Operator, Restrictive[T, N])] = term match {
			case ArithmeticRestrictive(left, op, right) => Some((left, op, right))
			case _ => None
		}



		/** Factory and matcher for negated numbers, i.e. expressions in the form of `-x`. */
		object Minus {
			def apply[T, N :ArithmeticSupport](term :Restrictive[T, N]) :Restrictive[T, N] =
				new NegatedRestrictive(term)

			def unapply[T, N](term :Restrictive[T, N]) :Option[Restrictive[T, N]] = term match {
				case NegatedRestrictive(x) => Some(x)
				case _ => None
			}
		}



		final val PLUS :Operator = new Operator("+") {
			override def apply[T :ArithmeticSupport](left :T, right :T) = implicitly[ArithmeticSupport[T]].plus(left, right)
		}
		final val MINUS :Operator = new Operator("-") {
			override def apply[T :ArithmeticSupport](left :T, right :T) = implicitly[ArithmeticSupport[T]].minus(left, right)
		}
		final val MULT :Operator = new Operator("*") {
			override def apply[T :ArithmeticSupport](left :T, right :T) = implicitly[ArithmeticSupport[T]].times(left, right)
		}
		final val DIV :Operator = new Operator("/") {
			override def apply[T :ArithmeticSupport](left :T, right :T) = implicitly[ArithmeticSupport[T]].div(left, right)
		}
		final val REM :Operator = new Operator("%") {
			override def apply[T :ArithmeticSupport](left :T, right :T) = implicitly[ArithmeticSupport[T]].rem(left, right)
		}



		sealed abstract class Operator private[Arithmetic](val symbol :String) extends Serializable {
			def apply[T :ArithmeticSupport](left :T, right :T) :T
		}




	}



	/** A factory and matcher for expressions returning the size of a collection - either an inlined SQL sequence,
	  * an embedded select or the final result set.
	  */
	object SizeOf {
		def apply[T, C, E](collection :Restrictive[T, C])(implicit composite :C ComposedOf E) :Restrictive[T, Int] =
			new SizeRestrictive(collection)

		def unapply[T, Int](restrictive :Restrictive[T, Int]) :Option[(Restrictive[T, C], C ComposedOf E) forSome { type C; type E }] =
			restrictive match {
				case size :SizeRestrictive[T, c, e] => Some((size.collection, size.composite))
				case _ => None
			}
	}



	/** A factory and matcher for conditional expressions with two branches selected based on whether a predicate
	  * represented by a `Restraint` instance is satisfied.
	  */
	object IfElse {
		def apply[T, O](condition :Restraint[T])(ifTrue :Restrictive[T, O])(ifFalse :Restrictive[T, O]) :Restrictive[T, O] =
			new IfElseRestrictive(condition, ifTrue, ifFalse)

		def unapply[T, O](term :Restrictive[T, O]) :Option[(Restraint[T], Restrictive[T, O], Restrictive[T, O])] =
			term match {
				case IfElseRestrictive(condition, ifTrue, ifFalse) => Some((condition, ifTrue, ifFalse))
				case _ => None
			}
	}



	/** A factory and matcher for 'switch'/'case' expressions which match a given expression with a sequence of values
	  * and execute the branch expression corresponding to the first found match, or the default branch otherwise.
	  */
	object Cases {

		def apply[T :TypeTag, O](cases :(TranslableTerm[T, T], Restrictive[T, O])*)(default :Restrictive[T, O]) :Restrictive[T, O] =
			apply[T, T, O](Self())(cases :_*)(default)

		def apply[X, T, O](term :Restrictive[X, T])(cases :(TranslableTerm[X, T], Restrictive[X, O])*)(default :Restrictive[X, O]) :Restrictive[X, O] =
			new SwitchRestrictive(term, cases map { case (guard, branch) => guard() -> branch }, default)
			
		

		def unapply[T, O](term :Restrictive[T, O]) :Option[(Restrictive[T, S], Seq[(Restrictive[T, S], Restrictive[T, O])], Restrictive[T, O]) forSome { type S }] =
			term match {
				case SwitchRestrictive(what, cases, default) => Some((what, cases, default))
				case _ => None
			}
	}



	/** A factory and matchers for expressions testing if a given value is of a given type.
	  * This is implemented in scala as an `isInstanceOf` check followed by an expression depending on the narrowed
	  * value, returned in an `Option`. The translation to SQL varies and depends on the mapping definition for
	  * the down cast expression.
	  */
	object DownCast {
		def apply[T >: S :TypeTag, S :ClassTag, O](forSubclass :Restrictive[S, O]) :Restrictive[T, Option[O]] =
			new SubclassRestrictive(Self[T](), classTag[S].runtimeClass, forSubclass)

		def apply[T, U >: S, S :ClassTag, O](term :Restrictive[T, U], forSubclass :Restrictive[S, O]) :Restrictive[T, Option[O]] =
			new SubclassRestrictive(term, classTag[S].runtimeClass, forSubclass)


		def unapply[T, O](term :Restrictive[T, Option[O]]) :Option[(Restrictive[T, U], Class[_], Restrictive[S, O]) forSome { type U >: S; type S }] =
			term match {
				case SubclassRestrictive(t, subclass, cond) => Some((t, subclass, cond))
				case _ => None
			}
	}





	private case class ComposedRestrictive[-X, Y, Z](first :Restrictive[X, Y], second :Restrictive[Y, Z])
		extends Restrictive[X, Z]
	{
		override def apply(x :X) :Z = second(first(x))

		override def andThen[P](next :Restrictive[Z, P]) :Restrictive[X, P] = next match {
			case _ :LiteralRestrictive[_, _] => next compose this
			case _ :SizeRestrictive[_, _, _] => next compose this
			case _ => new ComposedRestrictive(first, next compose second)
		}


		override def compose[W, S <: X](nest :Restrictive[W, S]) :Restrictive[W, Z] =
			new ComposedRestrictive(first compose nest, second)

		override def definedFor :Type = first.definedFor

		override def toString :String = first.toString + " ~> " + second
	}



	private case class PropertyRestrictive[-T, V](property :PropertyPath[T, V]) extends Restrictive[T, V] {

		override def apply(whole :T) :V = property(whole)

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, V] = nest match {
			case PropertyRestrictive(p) => new PropertyRestrictive(p andThen property)
			case LiteralRestrictive(value) => new LiteralRestrictive(property(value))
			case _ :SelfRestrictive[_, _] => nest andThen this
			case _ :ComposedRestrictive[_, _, _] => nest andThen this
			case _ :IfElseRestrictive[_, _] => nest andThen this
			case _ :CollectionRestrictive[_, _, _] => throw new IllegalArgumentException(s"Can't call Restrictive $this for a collection $nest.")
			case _ :SizeRestrictive[_, _, _] => throw new IllegalArgumentException(s"Can't call Restrictive $this for collection size: $nest.")
			case _ => super.compose(nest)
		}

		def definedFor :Type = property.definedFor

		override def toString :String = property.toString

	}



	private case class LiteralRestrictive[-T, V](value :V) extends Restrictive[T, V] {

		override def apply(whole :T) :V = value

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, V] =
			this.asInstanceOf[LiteralRestrictive[X, V]]


		def definedFor :Type = typeOf[Any]

		override def toString :String = value.toString
	}



	private case class SelfRestrictive[-T, V>:T](definedFor :Type) extends Restrictive[T, V] {

		override def apply(whole :T) :V = whole

		override def andThen[P](next :Restrictive[V, P]) :Restrictive[T, P] = next.asInstanceOf[Restrictive[T, P]]

		override def compose[X, S<:T](nest :Restrictive[X, S]) :Restrictive[X, V] = nest.asInstanceOf[Restrictive[X, V]]


		override def toString = "self"
	}



	private case class CollectionRestrictive[-T, C, E](values :Iterable[Restrictive[T, E]])
	                                                  (implicit val composite :C ComposedOf E)
		extends Restrictive[T, C]
	{
		override def apply(whole :T) :C = composite.composer(values.map(_(whole)))

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, C] =
			new CollectionRestrictive[X, C, E](values.map(_.compose(nest)))

		//todo: this will fail at some later time if items are empty; check if we really need this method
		override def definedFor :Type = values.head.definedFor


		override def toString :String = values.mkString(composite + "(", ", ", ")")
	}



	private[model] case class ArithmeticRestrictive[-T, N](left :Restrictive[T, N], op :Operator, right :Restrictive[T, N])
	                                               (implicit val arithmetic :ArithmeticSupport[N])
		extends Restrictive[T, N]
	{
		override def apply(whole :T) :N = op(left(whole), right(whole))

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, N] =
			new ArithmeticRestrictive(left compose nest, op, right compose nest)

		override def definedFor :Type = left.definedFor

		override def toString :String = "(" + left + " " + op + " " + right + ")"
	}



	private[model] case class NegatedRestrictive[-T, N](term :Restrictive[T, N])(implicit val arithmetic :ArithmeticSupport[N])
		extends Restrictive[T, N]
	{
		override def apply(whole :T) :N = arithmetic.negate(term(whole))

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, N] =
			new NegatedRestrictive(term compose nest)

		override def definedFor :Type = term.definedFor

		override def toString :String = "-" + term
	}



	private case class SizeRestrictive[-T, C, E](collection :Restrictive[T, C])
	                                            (implicit val composite :C ComposedOf E)
		extends Restrictive[T, Int]
	{
		override def apply(whole :T) :Int = composite.decomposer(collection(whole)).size

		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, Int] =
			new SizeRestrictive(collection compose nest)

		override def definedFor :Type = collection.definedFor

		override def toString :String = "count(" + collection + ")"
	}



	private case class IfElseRestrictive[-T, O](condition :Restraint[T], ifTrue :Restrictive[T, O], ifFalse :Restrictive[T, O])
		extends Restrictive[T, O]
	{
		override def apply(whole :T) :O = if (condition(whole)) ifTrue(whole) else ifFalse(whole)

//		override def andThen[P](next :Restrictive[O, P]) :Restrictive[T, P] =
//			new IfElseRestrictive(condition, next compose ifTrue, next compose ifFalse)
//
//		override def compose[X, S <: T](nest :Restrictive[X, S]) :Restrictive[X, O] =
//			new IfElseRestrictive(condition derive nest, ifTrue compose nest, ifFalse compose nest)

		override def definedFor :Type = ifTrue.definedFor

		override def toString :String = "if (" +condition + ") {" + ifTrue + "} else {" + ifFalse + "}"
	}



	private case class SwitchRestrictive[X, T, O](
			term :Restrictive[X, T],
			cases :Seq[(Restrictive[X, T], Restrictive[X, O])], default :Restrictive[X, O]
        ) extends Restrictive[X, O]
	{
		override def apply(whole :X) :O = {
			val switch = this(whole)
			cases collectFirst { case (guard, opt) if guard(whole) == switch => opt(whole) } getOrElse default(whole)
		}

		override def definedFor :Type = term.definedFor

		override def compose[Z, S <: X](nest :Restrictive[Z, S]) :Restrictive[Z, O] =
			new SwitchRestrictive(
				term compose nest, 
				cases map { case (guard, branch) => (guard compose nest, branch compose nest) }, 
				default compose nest
			)
	}



	private case class SubclassRestrictive[-T, S, O](term :Restrictive[T, _ >: S], subclass :Class[_], conditional :Restrictive[S, O])
		extends Restrictive[T, Option[O]]
	{
		override def apply(t :T) :Option[O] = {
			val sup = term(t)
			if (subclass.isInstance(sup)) Some(conditional(sup.asInstanceOf[S]))else None
		}

		override def definedFor :Type = term.definedFor

		override def compose[X, L <: T](nest :Restrictive[X, L]) :Restrictive[X, Option[O]] =
			new SubclassRestrictive(term compose nest, subclass, conditional)

		override def toString :String = s"($term).cast[${subclass.getName}]{ $conditional }"
	}


}

