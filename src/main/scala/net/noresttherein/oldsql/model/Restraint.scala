package net.noresttherein.oldsql.model

import scala.reflect.runtime.universe.{typeOf, Type, TypeTag}
import scala.collection.{breakOut, Iterable}
import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Restraint.{Conjunction, Disjunction, False, NestedRestraint, Not, True}
import net.noresttherein.oldsql.model.Restraint.ExistentialRestraint.AbstractExistentialRestraint
import net.noresttherein.oldsql.model.Restraint.Restrainer.{AbstractTermRestrainer, MappedRestrainer, NestedRestrainer}
import net.noresttherein.oldsql.model.Restrictive.{Collection, IfElse, Literal, Property, Self}
import net.noresttherein.oldsql.morsels.PropertyChain
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.SaferCasts._

import scala.reflect.ClassTag
import scala.util.matching.Regex



/** A restriction on type T, serving as a search filter which can be used to create queries and references with values
  * composed of `T`. It is contravariant regarding it's type parameter because:
  *
  *   a) it is much more convenient in the implementation and
  *
  *   b) it seems natural when viewed as a filter function.
  *
  * Intuitively, any condition that can be used to narrow down
  * a set of values of `T` can be used to narrow down a set of values of `S&lt;:T`. However, the problem starts when it is
  * treated as a result set of values of `T`, because then it would have to be covariant with regards to `T` -
  * a set of values of `S` is of course a set of values of `T>:S`. This becomes an issue when used inside a
  * `Kin[T]`, which is covariant with regards to `T` and care has to be taken when dealing with such cases.
  */
trait Restraint[-T] extends (T => Boolean) with implicits with Serializable {

	/** Creates a restraint implementing the negated condition of this restraint (i.e. selecting a value ''iff''
	  * it was not selected by this instance. The domain stays the same.
	  */
	def unary_! :Restraint[T] = Not(this)

	/** Create a restriction satisfied if and only if both this restriction and the given argument are satisfied. */
	def &&[S<:T](other :Restraint[S]) :Restraint[S] = other match {
		case True => this
		case False => False
		case Conjunction(restrictions) => Conjunction(other +: restrictions)
		case _ => Conjunction(Seq(this, other))
	}

	/** Create a restriction satisfied if and only if any of this and the given argument are satisfied. */
	def ||[S<:T](other :Restraint[S]) :Restraint[S] = other match {
		case True => True
		case False => this
		case Disjunction(restrictions) => Disjunction(other +: restrictions)
		case _ => Disjunction(Seq(this, other))
	}

	/** An alias for `&&` which may be used to force an implicit conversion from objects which do declare a `&&` method. */
	final def and[S<:T](other :Restraint[S]) :Restraint[S] = this && other

	/** An alias for `||` which may be used to force an implicit conversion from objects which do declare a `||` method. */
	final def or[S<:T](other :Restraint[S]) :Restraint[S] = this || other


	/** Represent this restraint as one of a new, parent type `X`. */
	def derive[X, S<:T](nest :Restrictive[X, S]) :Restraint[X] =
		new NestedRestraint[X, S](nest, this)

	/** Uses this instance as a boolean condition to create a `Restrictive` implementing a conditional expression.
	  * @return a `Restrictive` which is equivalent to `ifTrue` whenever this `Restraint` holds and `ifFalse` otherwise.
	  */
	def ifElse[S<:T, O](ifTrue :Restrictive[S, O])(ifFalse :Restrictive[S, O]) :Restrictive[S, O] =
		IfElse[S, O](this)(ifTrue)(ifFalse)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[Restraint[_]]

}






/** `Restraint` factory, allowing to create search filters. */
object Restraint {
	import Comparison._
	import implicits._

	/** A `Restraint` factory for some type `T`, working on some part of `T` expressed as type `P`. `P` may be just `T`
	  * (structural identity restriction), a value of a single property of `T`, or something else.
	  * An implementation is supposed to store the static definition of the restriction - i.e. what is constrained,
	  * and in what way. In addition to creating `Restraint`s based on both the 'whole' and 'part' values,
	  * it also provides accessors retrieving the constrained part `P` from the whole entity `T`.
	  * While there is some overlap with the `Restrictive` values, restrainers encapsulate not only any values
	  * taking part in matching (as in `Restrictive`s), but also the ''way'' matching is performed.
	  * @see [[net.noresttherein.oldsql.model.Restrictive Restrictive]]
	  */
	trait Restrainer[-T, P] extends (P => Restraint[T]) {
		//todo: combining restrainers into logical formulas

		/** Constrained part of target type equals the given value - same as `===` */
		def apply(key :P) :Restraint[T]

		/** Value of the constrained part of target type is the same as in the given value. */
		def as(value :T) :Restraint[T]

		/** Retrieve the value of the constrained part of the passed argument if possible.
		  * If this method returns `Some(x)`, `x` should be a valid argument for this restrainer's factory methods.
		  */
		def from(value :T) :Option[P]

		/** Retrieve the value of the constrained part from the passed restriction if possible.
		  * If the restriction does not contain an appropriate value or is incompatible with this restrainer
		  * return `None`. If this method returns `Some(x)`, `x` should be a valid argument for this restrainer's factory methods.
		  */
		def from[X <: T](restraint :Restraint[X]) :Option[P]

		/** Retrieve the value of the constrained part from the passed restraint if possible. Same as `from(Restraint[T])`. */
		def unapply[X <: T](restraint :Restraint[X]) :Option[P] = from(restraint)

		/** Value of the constrained part of target type is a member of passed collection. */
		def in :Restrainer[T, Set[P]]


		/** Represent the underlying restraint factory as a function of a new key type `K`. */
		def compose[K](oldKey :P=>K, newKey :K=>P) :Restrainer[T, K] =
			new MappedRestrainer[T, P, K](this)(oldKey, newKey)

		/** Create a restrainer working on some other, larger type `X` from which values of `S&lt;:T` can be derived. */
		def derive[X, S<:T](nest :Restrictive[X, S]) :Restrainer[X, P] = new NestedRestrainer(this, nest)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[Restrainer[_, _]]
	}



	object Restrainer {

		/** A base class for `Restrainer` implementations comparing an expression `Term` derived from the constrainted
		  * type `T` with a given value of `V` as a key. The character of the comparison is left to the subclass.
		  * @tparam T the type for which this factory creates restraints.
		  * @tparam V the key type, which is derived from the constrained type `T` by the member `Term`.
		  */
		abstract class AbstractTermRestrainer[-T, V] extends Restrainer[T, V] {
			protected val Term :Restrictive[T, V]

			override def from(value :T) :Option[V] = Option(Term(value))

			override def as(value :T) :Restraint[T] = apply(Term(value))

			override def in :Restrainer[T, Set[V]] = Membership(Term)


			override def canEqual(that :Any) :Boolean = that.getClass == getClass

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :AbstractTermRestrainer[_, _] =>
					canEqual(other) && other.canEqual(this) && Term == other.Term
				case _ => false
			}

			override def hashCode :Int = Term.hashCode * 31 + getClass.hashCode

			protected def op :String

			override def toString :String = "(" + Term + " " + op + " ?)"
		}



		private case class NestedRestrainer[-T, M, P](target :Restrainer[M, P], nest :Restrictive[T, M])
			extends Restrainer[T, P]
		{
			override def apply(key :P) :Restraint[T] = target(key) derive nest

			override def as(value :T) :Restraint[T] = target.as(nest(value)).derive(nest)

			override def from(value :T) :Option[P] = target.from(nest(value))

			override def from[X <: T](restraint :Restraint[X]) :Option[P] = restraint match {
				case NestedRestraint(p, r) if p == nest => target.from(r.asInstanceOf[Restraint[M]])
				case _ => None
			}

			override def in :Restrainer[T, Set[P]] = new NestedRestrainer(target.in, nest)

			override def toString :String = nest.toString + "(" + target + ")"
		}


		private case class MappedRestrainer[-T, X, Y](backing :Restrainer[T, X])(back :X=>Y, there :Y=>X)
			extends Restrainer[T, Y]
		{
			override def apply(value: Y): Restraint[T] = backing(there(value))

			override def from(value: T): Option[Y] = backing.from(value).map(back)

			override def from[S <: T](restriction: Restraint[S]): Option[Y] = backing.from(restriction).map(back)

			override def in: Restrainer[T, Set[Y]] = backing.in.compose(_.map(back), _.map(there))

			override def as(value: T): Restraint[T] = backing.as(value)

			override def toString :String = backing + ".compose(" + back + ", "+ there +")"
		}

	}





	/** Convenience factory class for `Restraint[T]`, allowing for automatic type inference of generic parameters.
	  * All methods return a `Restrainer`.
	  */
	trait RestrainerFactory[T] extends Any {
		/** Constrain the whole value of target type T */
		def self(implicit tag :TypeTag[T]) :Restrainer[T, T] = Equality[T]()

		/** Constrain the value of the given property of T */
		def Property[P](property :PropertyChain[T, P]) :Restrainer[T, P] =
			Restraint.Property(property)

		/** Constrain the value of the given property of T */
		def Property[P](property :T=>P)(implicit tag :TypeTag[T]) :Restrainer[T, P] =
			Restraint.Property(PropertyChain(property))
	}



	/** Returns a Restrainer for type T. RestrainerFactory allows for two-step creation of restrictions.
	  * In the first step, calling a specific method of the returned factory creates a Restrainer defining what
	  * is being constrained, and in the second, the call on returned Restrainer with a value for the restriction
	  * will return an actual restriction.
	  * @tparam T the type being constrained
	  */
	@inline def For[T] :RestrainerFactory[T] = new RestrainerFactory[T] {}


	/** A Restrainer constraining the value of the given property of T */
	@inline def Property[T, P](property :PropertyChain[T, P]) :Restrainer[T, P] =
		Equality(property)

	/** A Restrainer constraining the value of the given property of T */
	@inline def Property[T :TypeTag, P](property :T=>P) :Restrainer[T, P] =
		Equality(PropertyChain(property))




	/** An adapter class for a `Restraint` working for some other type `P` which can be derived from `T`. */
	case class NestedRestraint[-T, P](nest :Restrictive[T, P], restraint :Restraint[P])
		extends Restraint[T]
	{
		override def apply(t :T) :Boolean = restraint(nest(t))

		override def derive[X, S <: T](outer :Restrictive[X, S]) :Restraint[X] =
			new NestedRestraint(outer andThen nest, restraint)

		override def toString :String = nest.toString + " " + restraint
	}



	/** An all-accepting filter meaning 'all values of type T', serving as a neutral element for conjunction of restraints. */
	case object True extends Restraint[Any] {
		override def apply(x :Any) :Boolean = true
		override def &&[S<:Any](other :Restraint[S]) :Restraint[S] = other
		override def ||[S<:Any](other :Restraint[S]) :Restraint[S] = this

		override def derive[X, S <: Any](nest :Restrictive[X, S]) :Restraint[X] = this
	}


	/** An all-rejecting filter meaning 'nothing', serving as a neutral element for disjunction of restraints. */
	case object False extends Restraint[Any] {
		override def apply(x :Any) :Boolean = false
		override def &&[S<:Any](other :Restraint[S]) :Restraint[S] = this
		override def ||[S<:Any](other :Restraint[S]) :Restraint[S] = other

		override def derive[X, S <: Any](nest :Restrictive[X, S]) :Restraint[X] = this
	}



	/** Adapts a `Restrictive[Boolean]`: this restraint passes whenever `term` is true.  */
	case class BooleanRestraint[-T](term :Restrictive[T, Boolean]) extends Restraint[T] {
		override def apply(t :T) :Boolean = term(t)

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
			new BooleanRestraint(term compose nest)

		override def toString :String = term.toString
	}



	/** Negation of the filter implemented by the given restraint. */
	case class Not[-T](condition :Restraint[T]) extends Restraint[T] {
		override def apply(t :T) :Boolean = !condition(t)

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] = new Not(condition derive nest)
	}



	/** A `Restraint` explicitly testing if the given term is null by the explicit `SQL` 'IS NULL' expression. */
	case class IsNull[-T, V](term :Restrictive[T, V]) extends Restraint[T] {
		override def apply(x :T) :Boolean = term(x) == null

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] = new IsNull(term compose nest)
	}



	/** `Restraint` representing a logical conjunction (and) of restraints on the same type. */
	case class Conjunction[-T](restrictions :Seq[Restraint[T]]) extends Restraint[T] {
		override def apply(t :T) :Boolean = restrictions.forall { _(t) }

		override def &&[S <: T](other: Restraint[S]): Restraint[S] = other match {
			case True => this
			case False => False
			case Conjunction(others) => Conjunction(restrictions ++: others)
			case _ => Conjunction(restrictions :+ other)
		}

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
			new Conjunction(restrictions.map(_.derive(nest)))

		override def toString :String = restrictions.mkString("("," and ", ")")
	}



	/** Restraint representing a logical disjunction (or) of restrictions on the same type. */
	case class Disjunction[-T](restrictions :Seq[Restraint[T]]) extends Restraint[T] {
		override def apply(t :T) :Boolean = restrictions.exists { _(t) }

		override def ||[S <: T](other: Restraint[S]): Restraint[S] = other match {
			case True => True
			case False => this
			case Disjunction(others) => Disjunction(restrictions ++: others)
			case _ => Disjunction(restrictions :+ other)
		}

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
			new Disjunction(restrictions.map(_.derive(nest)))

		override def toString :String = restrictions.mkString("(", " or ", ")")
	}



	/** Tests if the given expression is of a specified class. */
/*
	//todo: we need some special type class for this, as we can't work with arbitrary classes in sql.
	case class TypeIs[-T](subclass :Class[_]) extends Restraint[T] {
		import java.{lang => j}

		override def apply(t :T) :Boolean =
			subclass.isInstance(t) || subclass.isPrimitive &&
				(subclass match {
					case j.Integer.TYPE => classOf[j.Integer]
					case j.Long.TYPE => classOf[j.Long]
					case j.Double.TYPE => classOf[j.Double]
					case j.Float.TYPE => classOf[j.Float]
					case j.Boolean.TYPE => classOf[j.Boolean]
					case j.Character.TYPE => classOf[j.Character]
					case j.Byte.TYPE => classOf[j.Byte]
					case j.Short.TYPE => classOf[j.Short]
					case j.Void.TYPE => classOf[Unit]
				}).isInstance(t)

		override def toString :String = "isInstanceOf[" + subclass.getName + "]"
	}

	object TypeIs {
		def apply[T, S :ClassTag]() :Restraint[T] =
			new TypeIs[T](implicitly[ClassTag[S]].runtimeClass)
	}
*/






	/** Create restraints comparing two expressions for equality and matches such restraints. */
	object Equality {

		/** A `Restrainer` constraining the whole value of `T`. */
		def apply[T :TypeTag]() :Restrainer[T, T] = new EqualityRestrainer[T, T](Self())

		/** A `Restrainer` comparing the given term derived from `T` to a specified literal. */
		def apply[T, P](term :Restrictive[T, P]) :Restrainer[T, P] = new EqualityRestrainer(term)

		/** Create a restriction meaning 'values of `T` equal to this value'. */
		def apply[T :TypeTag](value :T) :Restraint[T] =
			new EqualityTest[T, T](Self[T](), Literal[T, T](value))

		/** Create a restriction representing an equality test between the given to terms. */
		def apply[T, V](left :Restrictive[T, V], right :Restrictive[T, V]) :Restraint[T] =
			new EqualityTest(left, right)

		/** Create a restriction representing an equality test between the given to terms. */
		def apply[T, V](left :Restrictive[T, V], right :V) :Restraint[T] =
			new EqualityTest(left, Literal(right))

		/** Create a restriction representing an equality test between the given to terms. */
		def apply[T, V](left :V, right :Restrictive[T, V]) :Restraint[T] =
			new EqualityTest(Literal(left), right)



		/** Check if the given restriction is an equality restriction with a specified value.
		  * It will also match membership restrictions for singleton sets, so this check should be performed before
		  * any future membership check.
		  */
		def unapply[T](restraint :Restraint[T]) :Option[(Restrictive[T, E], Restrictive[T, E]) forSome { type E }] =
			restraint match {
				case EqualityTest(left, right) => Some((left, right))
//				case Comparison(left, EQ, right) => Some((left, right)) //this loses the information that left and right are of the same type
				case Membership(left, Collection(right, _), _) if right.size == 1 =>
					Some((left.asInstanceOf[Restrictive[T, Any]], right.head.asInstanceOf[Restrictive[T, Any]]))
				case _ => Comparison.unapply(restraint) collect { case (l, EQ, r) => (l, r) }
//				case _ => None
			}



		private case class EqualityTest[-T, V](left :Restrictive[T, V], right :Restrictive[T, V])
			extends Restraint[T]
		{
			override def apply(t :T) :Boolean = left(t) == right(t)

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
				EqualityTest(left compose nest, right compose nest)

			override def toString :String = "(" + left + " = " + right + ")"
		}

		private class EqualityRestrainer[-T, K](val Term :Restrictive[T, K]) extends AbstractTermRestrainer[T, K] {

			override def apply(key :K) :Restraint[T] = Equality(Term, Literal(key))

			override def from[X <: T](restraint :Restraint[X]) :Option[K] = restraint match {
				case Equality(Term, Literal(v)) => Some(v.asInstanceOf[K])
				case _ => None
			}

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restrainer[X, K] =
				new EqualityRestrainer(Term compose nest)

			protected override def op = "="
		}

	}






	private class Comparison[-T, V](val left :Restrictive[T, V], val relation :Relation, val right :Restrictive[T, V])
	                               (implicit val ordering :OrderingSupport[V])
		extends Restraint[T]
	{
		override def apply(t :T) :Boolean = relation(left(t), right(t))

		override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
			new Comparison(left compose nest, relation, right compose nest)

		override def equals(other: Any): Boolean = other match {
			case self :AnyRef if self eq this => true
			case that: Comparison[_, _] =>
				left == that.left && relation == that.relation && right == that.right && ordering == that.ordering
			case _ => false
		}

		override def hashCode(): Int = {
			val state = Seq(left, relation, right, ordering)
			state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
		}

		override def toString :String = s"($left $relation $right)"
	}



	/** Factory of `Restraint`s comparing the magnitude of two expressions with one of
	  * the standard `<`, `<=`, `>`, `>=` operators.
	  */
	object Comparison {

		/** A factory of `Restraint`s comparing the 'self' value (i.e. an identity Restrictive`[T]`)
		  * and supplied 'key' parameters using the relation provided here.
		  */
		def apply[T :TypeTag :OrderingSupport](comparison :Relation) :Restrainer[T, T] =
			new ComparisonRestrainer(Self(), comparison)

		//todo: having OrderingSupport should mean we also have mapping for literals

		/** A factory of `Restraint`s comparing the value of the given term `term` with a supplied 'key' parameter
		  * using the relation provided here.
		  */
		def apply[T, P :OrderingSupport](term :Restrictive[T, P], comparison :Relation) :Restrainer[T, P] =
			new ComparisonRestrainer(term, comparison)


		/** A `Restraint` verifying if the given relation `rel` holds between the two terms: `left ''rel'' right`. */
		def apply[T, V](left :Restrictive[T, V], rel :Relation, right :Restrictive[T, V])
		               (implicit ordering :OrderingSupport[V]) :Restraint[T] =
			new Comparison(left, rel, right)

		/** A `Restraint` verifying if the given relation `rel` holds between the two terms: `left ''rel'' right`. */
		def apply[T, V](left :Restrictive[T, V], rel :Relation, right :V)
		               (implicit ordering :OrderingSupport[V]) :Restraint[T] =
			new Comparison(left, rel, Literal(right))

		/** A `Restraint` verifying if the given relation `rel` holds between the two terms: `left ''rel'' right`. */
		def apply[T, V](left :V, rel :Relation, right :Restrictive[T, V])
		               (implicit ordering :OrderingSupport[V]) :Restraint[T] =
			new Comparison(Literal(left), rel, right)



		def unapply[T](restraint :Restraint[T])
				:Option[(Restrictive[T, V], Relation, Restrictive[T, V])] forSome { type V } =
			restraint match {
				case comp :Comparison[T, v] => Some((comp.left, comp.relation, comp.right))
				case _ => None
			}


		/** A relation comparing the magnitude of two comparable terms, i.e. `<`, `<=`, `>`, `>=`, `==`.
		  * This is simply a token used to carry the information about its nature in `Restriction`s and doesn't
		  * implement anything. Constants exist in [[net.noresttherein.oldsql.model.Restraint.Comparison Comparison]].
		  */
		final class Relation private[Comparison](val symbol :String, expect :Int, invert :Boolean = false)
			extends Serializable
		{
			def apply[T :OrderingSupport](left :T, right :T) :Boolean =
				implicitly[OrderingSupport[T]].compare(left, right).signum == expect ^ invert

			override def equals(that :Any) :Boolean = that match {
				case other :Relation => symbol == other.symbol
				case _ => false
			}

			override def hashCode :Int = symbol.hashCode

			override def toString :String = symbol
		}

		/** The `<` relation. */
		final val LT = new Relation("<", -1)
		/** The `<=` relation. */
		final val LTE = new Relation("<=", 1, true)
		/** The `>` relation. */
		final val GT = new Relation(">", 1)
		/** The `>=` relation. */
		final val GTE = new Relation(">=", -1, true)
		/** The `==` relation. */
		final val EQ = new Relation("=", 0)


		/** Type class for types which magnitude can be compared in SQL, not only in scala.
		  * Implicit values are provided for all standard numeric types and `String`.
		  */ //todo: this is just a stub
		trait OrderingSupport[V] extends Ordering[V] with Serializable

		object OrderingSupport {
			@inline def apply[T :OrderingSupport] :OrderingSupport[T] = implicitly[OrderingSupport[T]]

			def unapply[T](restraint :Restraint[T]) :Option[OrderingSupport[_]] = restraint match {
				case cmp :Comparison[T, _] => Some(cmp.ordering)
				case _ => None
			}

			//todo: stub
			implicit def numericOrdering[T :Numeric] :OrderingSupport[T] = {
				(x :T, y :T) => implicitly[Numeric[T]].compare(x, y)
			}

			//todo: case sensitive/insensitive
			implicit object OfString extends OrderingSupport[String] {
				override def compare(x :String, y :String) :Int = x compare y
			}
		}



		private class ComparisonRestrainer[-T, K](val Term :Restrictive[T, K], Cmp :Relation)
		                                         (implicit order :OrderingSupport[K])
			extends AbstractTermRestrainer[T, K]
		{
			override def apply(key :K) :Restraint[T] = Comparison(Term, Cmp, Literal(key))

			override def from[X](restraint :Restraint[X]) :Option[K] = restraint match {
				case Comparison(Term, Cmp, Literal(right)) => Some(right.asInstanceOf[K])
				case _ => None
			}

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restrainer[X, K] =
				new ComparisonRestrainer(Term compose nest, Cmp)

			protected override def op :String = Cmp.toString
		}

	}






	/** Factory of `Restraint`s implementing SQL ''like'' expression for `String`s.
	  * The patterns accepted here are '''not''' in the SQL format, but rather common glob format:
	  *   - '?' will match any single character;
	  *   - '*' will match any number of characters;
	  *   - '\' is used to escape the former;
	  *   - any other character matches itself.
	  */
	object StringLike {

		/** Creates a `Restraint` implementing the SQL expression `matched like pattern`.
		  * @param matched a `String` expression to compare with the pattern `pattern`.
		  * @param pattern a pattern to match in the common command line format (and '''not''' the SQL format);
		  *                '?' and '*' special characters are used to match a single and any number of any characters,
		  *                respectively, with `\` working as a string expression.
		  */
		def apply[T](matched :Restrictive[T, String], pattern :String) :Restraint[T] =
			new Like(matched, pattern)


		def unapply[T](restraint :Restraint[T]) :Option[(Restrictive[T, String], String)] = restraint match {
			case like :Like[T] => Some((like.term, like.pattern))
			case _ => None
		}

		/** Compares the given literaL string `term` with the provided pattern. */
		def matches(term :String, pattern :String) :Boolean = {
			val quoted = Regex.quote(pattern)
			val regex = new StringBuilder(quoted.length)

			var i = 0; var escapes = 0; val len = quoted.length
			while (i < len) {
				quoted(i) match {
					case '\\' => escapes += 1; regex += '\\'
					case '*' if escapes % 2 == 0 => escapes = 0; regex ++= ".*"
					case '?' if escapes % 2 == 0 => escapes = 0; regex ++= "."
					case c => escapes = 0; regex += c
				}
				i += 1
			}

			new Regex(regex.toString).unapplySeq(term).isDefined
		}

		private case class Like[T](term :Restrictive[T, String], pattern :String) extends Restraint[T] {
			override def apply(t :T) :Boolean = matches(term(t), pattern)

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
				new Like(term compose nest, pattern)
		}

	}





	/** Create `Restraint`s checking if a value is a member of a given set of values and matches such instances.
	  * All created instances test against inlined value sets (rather than a result set from a select from another table),
	  * but the set can contain arbitrary expressions derivable from the root entity `T`.
	  *
	  * Example: `hero.name in ('Minsc', name)`.
	  */
	object Membership {
		import Restrictive._

		/** Create a `Restrainer` for type `T` accepting a set of values to test against. */
		def apply[T :TypeTag]() :Restrainer[T, Set[T]] = new MembershipRestrainer[T, T](Self())

		/** A `Restrainer` testing a value `P` derived from `T` for being part of a given literal collection. */
		def apply[T, P](term :Restrictive[T, P]) :Restrainer[T, Set[P]] = new MembershipRestrainer(term)


		/** Create a `Restraint` testing if the left expression is part of the collection of the right expression. */
		def apply[T, C, E](member :Restrictive[T, E], collection :Restrictive[T, C])
		                  (implicit deco :C DecomposableTo E) :Restraint[T] =
			new MembershipTest(member, collection)


		/** Create a `Restraint` representing membership test for the value specified by the first argument
		  * and the collection of terms given by the second argument.
		  */
		def apply[T, E](member :Restrictive[T, E], values :Iterable[Restrictive[T, E]]) :Restraint[T] = values.size match {
//			case 0 => False
//			case 1 => Equality(member, values.head)
			case _ => new MembershipTest[T, Iterable[E], E](member, Collection(values))
		}


		/** Create a `Restraint` representing membership test for the value specified by the first argument
		  * and the collection of terms given by the second argument. Passed value set is decomposed using
		  * an implicit `composite` information before creating the restraint into a sequence.
		  * @tparam T the root entity type from which the tested expression is derived.
		  * @tparam C a collection of literal values of type `E`.
		  * @tparam E the element type - the type of the expression which membership is tested.
		  */
		def apply[T, C, E](member :Restrictive[T, E], values :C)(implicit composite :C ComposedOf E) :Restraint[T] = {
			val decomposed = composite.decomposer(values)
			decomposed.size match {
//				case 0 => False
//				case 1 => Equality(member, decomposed.head)
				case _ => new MembershipTest[T, C, E](member, Collection(decomposed.map(Literal(_))))(composite.decomposer)
			}
		}



		/** Check if the given restriction is a membership test for a set.
		  * This will also match all effective equality, whether it was created by Equality or MemberOf(Set(singleton)),
		  * so check for equality before checking for membership restriction.
		  */
		def unapply[T](restraint :Restraint[T]) :Option[(Restrictive[T, E], Restrictive[T, C], C DecomposableTo E) forSome { type E; type C }] =
			restraint match {
				case m :MembershipTest[T, c, e] => Some((m.member, m.values, m.decomposer))
//				case Equality(left, right) => Some((left, right, DecomposableTo.Subclass()))
				case _ => None
			}



		private case class MembershipTest[-T, C, E](member :Restrictive[T, E], values :Restrictive[T, C])
		                                           (implicit val decomposer :C DecomposableTo E)
			extends Restraint[T]
		{
			override def apply(t :T) :Boolean = decomposer(values(t)).toSet(member(t))

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restraint[X] =
				MembershipTest(member compose nest, values compose nest)

			override def toString :String = member.toString + " in " + values
		}



		private class MembershipRestrainer[-T, K](Term :Restrictive[T, K]) extends Restrainer[T, Set[K]] {

			override def apply(key :Set[K]) :Restraint[T] = Term in key

			override def as(value :T) :Restraint[T] = Term in Set(Literal(Term(value)))

			override def from(value :T) :Option[Set[K]] = Option(Term(value)) map { Set(_) }

			override def from[X <: T](restraint :Restraint[X]) :Option[Set[K]] = restraint match {
				case Membership(Term, Collection(items, _), _) =>
					items.collect[K, Set[K]] { case Literal(v) => v.asInstanceOf[K] }(breakOut) providing (_.size == items.size)

				case Membership(Term, Literal(items), deco) =>
					//todo: wrong! may be Iterable[Set[K]] after decomposer
					Some(deco.asInstanceOf[DecomposableTo[Any, Any]].apply(items).toSet.asInstanceOf[Set[K]])

				case False => Some(Set())
				case _ => None
			}

			override def in :Restrainer[T, Set[Set[K]]] = compose(Set(_), _.flatten)

			override def derive[X, S <: T](nest :Restrictive[X, S]) :Restrainer[X, Set[K]] =
				new MembershipRestrainer(Term compose nest)

			override def toString :String = "(" + Term + " in ?)"
		}

	}





	//todo: this is useful only for the complete result set as it doesn't contain the expression being tested!
	/** A base trait for `Restraint` implementations which lift a given `Restraint[E]` to a work for a collection `T`
	  * with `E` as elements.
	  * @tparam T the restrained type, being a collection type containing values of type `E`.
	  * @tparam E element type of the restrained collection and type parameter for the adapted `Restraint` instance.
	  */
	trait ExistentialRestraint[-T, E] extends Restraint[T] {
		/** The restraint for individual collection elements.*/
		def condition :Restraint[E]
		/** Decomposes the collection `T` into individual `E` values. */
		def decomposer :DecomposableTo[T, E]
	}


	object ExistentialRestraint {

		def unapply[T](restriction: Restraint[T]): Option[(Restraint[E], C DecomposableTo E) forSome {type E; type C >: T}] =
			restriction.ifSubclass[ExistentialRestraint[T, Any]] { e => (e.condition, e.decomposer) }


		abstract class AbstractExistentialRestraint[-T, E]
			extends ExistentialRestraint[T, E]
		{
			protected def name :String

			override def toString :String = s"$name[$decomposer]{$condition}"


			override def canEqual(that :Any) :Boolean = that.getClass == getClass

			override def equals(that: Any) :Boolean = that match {
				case e: ExistentialRestraint[_, _] =>
					(e eq this) || canEqual(e) && e.canEqual(this) && condition == e.condition &&
						decomposer.compatibleWith(e.decomposer)
				case _ => false
			}

			override def hashCode :Int = condition.hashCode
		}

	}



	private class ForAll[-T, E](val condition :Restraint[E])(implicit val decomposer :T DecomposableTo E)
		extends AbstractExistentialRestraint[T, E]
	{
		override def apply(t :T) :Boolean = decomposer(t) forall condition

		override def name = "ForAll"
	}


	/** Factory for expressions testing if a predicate (given as another `Restraint` instance) holds for all
	  * elements of a collection, being either the whole result set, a literal (inlined) sequence, or an embedded select.
	  */
	object ForAll {
		def apply[T, E](condition :Restraint[E])(implicit composite :T ComposedOf E) :ExistentialRestraint[T, E] =
			new ForAll[T, E](condition)(composite.decomposer)

		def unapply[T](restraint :Restraint[T]) :Option[(Restraint[E], T DecomposableTo E) forSome { type E }] =
			restraint match {
				case all :ForAll[T, e] => Some((all.condition, all.decomposer))
				case _ => None
			}
	}



	private class Exists[-T, E](val condition :Restraint[E])(implicit val decomposer :T DecomposableTo E)
		extends AbstractExistentialRestraint[T, E]
	{
		override def apply(t :T) :Boolean = decomposer(t) exists condition

		override def name = "Exists"
	}

	/** Factory for expressions testing if a given collection contains an element which satisfies the given
	  * predicate (in the form of another `Restraint` instance). The collection can be either the whole result set,
	  * a literal (inlined) sequence, or an embedded select.
	  */
	object Exists {
		def apply[T, E](condition :Restraint[E])(implicit composite :T ComposedOf E) :ExistentialRestraint[T, E] =
			new Exists(condition)(composite.decomposer)

		def unapply[T](restraint :Restraint[T]) :Option[(Restraint[E], T DecomposableTo E) forSome { type E }] =
			restraint match {
				case exists :Exists[T, e] =>
					Some((exists.condition, exists.decomposer))
				case _ => None
			}
	}



	private class Where[T, E](val condition :Restraint[E], val as :T ComposedOf E) extends AbstractExistentialRestraint[T, E] {
		override def decomposer :DecomposableTo[T, E] = as.decomposer

		override def apply(result :T) :Boolean = as.decomposer(result).forall(condition)

		override def name = "Where"
	}

	/** A factory for pseudo restraints selecting all entities `E` (rows in a table) using a given `Restraint`
	  * for individual entities as the filter (the ''where'' clause). This can appear only as the root `Restraint`
	  * under a `Kin` and should not be nested/composed with other restrictives/restraints.
	  */
	object Where {

		/** Create a `Restraint` selecting all instances of `E` passing the given filter as a composite type `T`. */
		def apply[T, E](where :Restraint[E])(implicit as :T ComposedOf E) :Restraint[T] = new Where(where, as)

		def unapply[T](restraint :Restraint[T]) :Option[(Restraint[E], T ComposedOf E) forSome { type E }] =
			restraint match {
				case where :Where[T, e] => Some((where.condition, where.as))
				case _ => None
			}
	}



	/** Factory for expressions testing if a given expression (represented as an `Option` in scala
	  * and a result set in SQL) has exactly one element.
	  */
	object IsSome {
		def apply[T](restraint :Restraint[T]) :ExistentialRestraint[Option[T], T] = Exists[Option[T], T](restraint)

		def unapply[T, E](restraint :Restraint[T])(implicit option :T ComposedOf E) :Option[Restraint[E]] =
			if (option != DecomposableTo.Optional())
				None
			else restraint match {
				case Exists(condition, deco) if deco compatibleWith option.decomposer =>
					Some(condition.asInstanceOf[Restraint[E]])
				case _ => None
			}
	}


}

