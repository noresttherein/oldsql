package com.hcore.ogre.model


//import com.hcore.ogre.model.Restriction.Constrainee.Literal.{LiteralMembershipOpressor, LiteralEqualityOpressor}

import com.hcore.ogre.model.ComposedOf.{DecomposableTo, ComposableFrom}
import com.hcore.ogre.model.Restriction.Restrictive.Literal.{LiteralEqualityOpressor, LiteralMembershipOpressor}
import com.hcore.ogre.model.Restriction.Restrictive.{Literal, Property, Self}
import com.hcore.ogre.model.Restriction._
import com.hcore.ogre.model.Restriction.ExistentialRestriction.AbstractExistentialRestriction
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.morsels.Names
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions

import scala.collection.Set
import scala.reflect.runtime.universe.{Type, TypeTag, typeOf}
//import Restriction.Constrainee._

//implicits
import extensions._
import SaferCasts._

/** A Restriction on type T, serving as a search filter which can be used to create queries and references with values composed of T.
  * It is contravariant regarding it's type parameter because a) it is much more convenient in the implementation and
  * b) it seems natural when viewed as a filter function. Intuitively, any condition that can be used to narrow down
  * a set of values of T can be used to narrow down a set of values of S<:T. However, the problem starts when it is
  * treated as a result set of values of T, because then it would have to be covariant with regards to T -
  * a set of values of S is of course a set of values of T>:S. This becomes an issue when used inside a
  * Reference[T], which is covariant with regards to T and care has to be taken when dealing with such cases.
  */
trait Restriction[-T] {

	/** Create a restriction satisfied if and only if both this restriction and the given argument are satisfied. */
	def && [S <: T](other :Restriction[S]) :Restriction[S] = other match {
		case Conjunction(restrictions) => Conjunction(other +: restrictions)
		case _ => Conjunction(Seq(this, other))
	}

	/** Create a restriction satisfied if and only if any of this and the given argument are satisfied. */
	def ||[S<:T](other :Restriction[S]) :Restriction[S] = other match {
		case Disjunction(restrictions) => Disjunction(other +: restrictions)
		case _ => Disjunction(Seq(this, other))
	}

	def expand[X](property :PropertyChain[X, T]) :Restriction[X]

	def canEqual(that :Any) = that.isInstanceOf[Restriction[_]]



//	def definedFor :Type
//	def isApplicableTo[X :TypeTag] :Boolean

//	def only[S<:T :TypeTag] :Restriction[Any] = new Restriction
	
}


/** Restrictions factory, allowing to create search filters. */
object Restriction {

	/** Arbitrary term of type V which can be used to create Restrictions on type T.
	  * May represent both values known statically (literals) and expressions being functions of T, where T is some constrained entity.
	  */
	trait Restrictive[-T, V] {
		def definedFor :Type
		/** Create a opressor for T which will compare this expression with given literal values of type V. */
		def ==? :Opressor[V, T]

		/** Create a restriction testing this expression and another expression for equality. Equivalent to Equality(this, other). */
		def ===[S<:T](other :Restrictive[S, V]) :Restriction[S] = Equality(this, other)

		/** Create a restriction testing this expression for equality with the given literal */
		def ===(other :V) :Restriction[T] = this === Restrictive.Literal[T, V](other)

		/** Create a opressor for T which will test if this expression is a member of given sets of literals of type V*/
		def in_? :Opressor[Set[V], T]

		/** Create a restriction testing if the value of this expression is a member of given collection. Equivalent to Membership(this, others). */
		def in [S<:T](others :Iterable[Restrictive[S, V]]) :Restriction[S] = Membership(this, others)
		
//		def in (others :Iterable[V]) :Restriction[T] = Membership(this, others.map(Literal[T, V](_)))

		def ++:[X](prefix :PropertyChain[X, T]) :Restrictive[X, V]

		def ++:[X :TypeTag](prefix : X=>T) :Restrictive[X, V] = PropertyChain(prefix) ++: this
		
		def canEqual(that :Any) = that.getClass==getClass
	}

	@inline
	implicit def Restrictive[T, V](property :PropertyChain[T, V]) :Restrictive[T, V] = Restrictive.Property(property)
	@inline
	implicit def Restrictive[T :TypeTag, V](property :T=>V) :Restrictive[T, V] = Restrictive.Property(property)
//	@inline
//	implicit def Inhibitor[T, V](value :V) :Constrainee[T, V] = Constrainee.Literal[T, V](value)



	/** Factory for Constrainee instances - constrainable terms which can be used to create Restrictions. */
	object Restrictive {

		/** Create a constrainable term representing a value of type T. Equivalent to Self[T]. */
		def apply[T :TypeTag] :Restrictive[T, T] = Self[T]

		implicit def literalSeq[T, V](literals :Iterable[V]) :Iterable[Restrictive[T, V]] = literals.map(Literal[T, V](_))
//		@inline
//		implicit def apply[T, V](property :PropertyChain[T, V]) :Constrainee[T, V] = Property(property)
//		@inline
//		implicit def apply[T :TypeTag, V](property :T=>V) :Constrainee[T, V] = Property(property)
//		@inline
//		implicit def apply[T, V](value :V) :Constrainee[T, V] = Literal[T, V](value)

		/** Factory for constrainable terms representing (possibly nested) properties */
		object Property {

			/** Constrainable expression representing a property with value type V of type T specified by the given reflected property. */
			@inline
			def apply[T, V](property :PropertyChain[T, V]) :Restrictive[T, V] = new PropertyRestrictive[T, V](property)

			/** Constrainable expression representing a property with value type V of type T specified by the given function.
			  * @param property a function returning some property of T, for example _.address.street
			  */
			@inline
			def apply[T :TypeTag, V](property :T=>V) :Restrictive[T, V] = apply(PropertyChain(property))

			/** Check if the given expression represents a property of T (is an instance created by this factory). */
			def unapply[T, V](constrainee :Restrictive[T, V]) :Option[PropertyChain[T, V]] =
				constrainee.ifSubclass[PropertyRestrictive[T, V]](_.property)
		}

		/** Factory for constrainable expressions representing constrained domain itself. */
		object Self {
			/** Expression denoting constrained value of type T. When seen as a function T=>V, it is an identity function. */
			def apply[T :TypeTag] :Restrictive[T, T] = new SelfRestrictive[T, T](typeOf[T]) //self.asInstanceOf[SelfConstrainee[T, T]]

			/** Check if the given constrainee is the value T itself created by this factory. */
			def unapply[T, V](constrainee :Restrictive[T, V]) :Option[Type] =
				constrainee.ifSubclassOf[SelfRestrictive[_, _]] { _.definedFor }
		}

		/** Factory for constrainable expressions representing literal values. */
		object Literal {
			/** A literal expression of type V, seen as a function T=>V */
			@inline
			def apply[T, V](value :V) :Restrictive[T, V] = new LiteralRestrictive[T, V](value)

			/** Check if the given constrainee is a literal expression. */
			def unapply[V](constrainee :Restrictive[_, V]) :Option[V] =
				constrainee.ifSubclassOf[LiteralRestrictive[_, V]](_.value)


			class LiteralEqualityOpressor[V, T](val literal :V) extends Opressor[V, T] {
				override def apply(value: V): Restriction[T] =
					if (literal==value) True
					else False

				override def from(value: T): Option[V] = literal.providing(value==literal)

				override def from(restriction: Restriction[_ <: T]): Option[V] =
					literal.providing(restriction==True)

				override def in: Opressor[Set[V], T] = new LiteralMembershipOpressor[V, T](literal)

				override def like(value: T): Restriction[T] =
					if (literal==value) True
					else False

				override def toString = s"$literal=?"
			}

			class LiteralMembershipOpressor[P, T](val literal :P) extends Opressor[Set[P], T] {
				override def apply(value: Set[P]): Restriction[T] =
					if (value(literal)) True
					else False

				override def from(value: T): Option[Set[P]] = Some(Set(literal).filter(_==value))

				override def from(restriction: Restriction[_ <: T]): Option[Set[P]] =
					Set(literal).providing(restriction==True)

				override def in: Opressor[Set[Set[P]], T] = map(Set(_:Set[P]), _.flatten)

				override def like(value: T): Restriction[T] =
					if (literal==value) True
					else False

				override def toString = s"$literal in ?"
			}
		}
		

		case class PropertyRestrictive[-T, V](property :PropertyChain[T, V]) extends Restrictive[T, V] {
			def definedFor = property.definedFor
			override def ==? = PropertyEquals(property)
			override def in_? = PropertyIn(property)

			override def ++:[X](prefix: PropertyChain[X, T]): Restrictive[X, V] =
				new PropertyRestrictive(prefix andThen property)

			override def toString = property.toString

		}
		
		case class LiteralRestrictive[-T, V](value :V) extends Restrictive[T, V] {
			def definedFor = typeOf[Any]
			override def ==? : Opressor[V, T] = new LiteralEqualityOpressor[V, T](value)

			override def in_? : Opressor[Set[V], T] = new LiteralMembershipOpressor[V, T](value)


			override def ++:[X](prefix: PropertyChain[X, T]): Restrictive[X, V] = this.asInstanceOf[LiteralRestrictive[X, V]]

			override def toString = value.toString
		}
		
		case class SelfRestrictive[-T, V>:T](definedFor :Type) extends Restrictive[T, V] {

			override def ==? = Equality[V](definedFor)
			override def in_? = Membership[V](definedFor)

			override def ++:[X](prefix: PropertyChain[X, T]): Restrictive[X, V] = new PropertyRestrictive(prefix)

			override def toString = "self"
//			def asSelf :Option[SelfConstrainee[T, V]] = Some(this)
		}
//		private[this] val self = new SelfConstrainee[Any, Any]
	}
	





	/** A Restriction factory for some type T, working on some part of T expressed as type P. P may be just T
	  * (structural identity restriction), a value of a single property of T, or something else.
	  * An implementation is supposed to store the static definition of the restriction - i.e. what is constrained,
	  * and provide factory methods returning Restriction[T] and expecting the values for the constrained aspect of T.
	  *
	  */
	trait Opressor[P, -T] extends (P => Restriction[T]) {
		//todo: combining opressors into logical formulas
		/** Constrained part of target type equals the given value */
		def === (key :P) :Restriction[T] = this(key)

		/** Constrained part of target type equals the given value - same as === */
		def apply(key :P) :Restriction[T]

		/** Value of the constrained part of target type is a member of passed collection */
//		def in(values :Set[P]) :Restriction[T]
		def in :Opressor[Set[P], T]

		/** Value of the constrained part of target type is the same as in the given value */
		def like(value :T) :Restriction[T]

		/** Retrieve the value of the constrained part of the passed argument if possible.
		  * If this method returns Some(x), x should be a valid argument for this opressor factory methods.
		  */
		def from(value :T) :Option[P]

		/** Retrieve the value of the constrained part from the passed restriction if possible.
		  * If the restriction does not contain an appropriate value or is incompatible with this opressor
		  * return None. If this method returns Some(x), x should be a valid argument for this opressor factory methods.
		  */
		def from(restriction :Restriction[_<:T]) :Option[P]

		/** Retrieve the value of the restriction part from the passed restriction if possible. Same as from(Restriction[T]). */
		def unapply(restriction :Restriction[_<:T]) :Option[P] = from(restriction)

		/** Represent the underlying restriction factory as a function of new key type X */
		def map[X](there :P=>X, back :X=>P) :Opressor[X, T] = new MappedOpressor[P, X, T](this)(there, back)
	}







	/** Convenience factory class for Restriction[T], allowing for automatic type inference of generic parameters. All methods return an Opressor */
	class OpressorFactory[T] private[Restriction] () {
		/** Constrain the whole value of target type T */
		def Value(implicit tag :TypeTag[T]) :Opressor[T, T] = Equality[T]()

		/** Constrain the value of the given property of T */
		def Property[P](property :PropertyChain[T, P]) :PropertyOpressor[P, T] =
			Restriction.Property(property)

		/** Constrain the value of the given property of T */
		def Property[P](property :T=>P)(implicit tag :TypeTag[T]) :PropertyOpressor[P, T] =
			Restriction.Property(PropertyChain(property))
	}
	private val factory = new OpressorFactory[Any]





	/** Returns a Opressor for type T. OpressorFactory allows for two-step creation of restrictions.
	  * In the first step, calling a specific method of the returned factory creates a Opressor defining what
	  * is being constrained, and in the second, the call on returned Opressor with a value for the restriction
	  * will return an actual restriction.
	  * @tparam T the type being constrained
	  */
	def For[T] :OpressorFactory[T] = factory.asInstanceOf[OpressorFactory[T]]


	/** A Opressor constraining the value of the given property of T */
	def Property[T, P](property :PropertyChain[T, P]) :PropertyOpressor[P, T] =
		PropertyEquals(property)

	/** A Opressor constraining the value of the given property of T */
	def Property[T :TypeTag, P](property :T=>P) :PropertyOpressor[P, T] =
		PropertyEquals(PropertyChain(property))



	

	/** An all-accepting filter meaning 'all values of type T', serving as a neutral element for conjunction of restrictions. */
	case object True extends Restriction[Any] {
		override def expand[X](property: PropertyChain[X, Any]): Restriction[X] = this
	}


	/** An all-rejecting filter meaning 'nothing', serving as a neutral element for disjunction of restrictions. */
	case object False extends Restriction[Any] {
		override def expand[X](property: PropertyChain[X, Any]): Restriction[X] = this
	}


//	private class TypeRestriction[-T :TypeTag] extends Restriction[T] {
//		override def expand[X](property: PropertyChain[X, T]): Restriction[X] = new TypeRestriction
//	}


	/** Restriction representing a logical conjunction (and) of restrictions on the same type */
	case class Conjunction[-T](restrictions :Seq[Restriction[T]]) extends Restriction[T] {

		override def &&[S <: T](other: Restriction[S]): Restriction[S] = other match {
			case Conjunction(others) => Conjunction(restrictions ++: others)
			case _ => Conjunction(restrictions :+ other)
		}


		override def expand[X](property: PropertyChain[X, T]): Restriction[X] =
			Conjunction(restrictions.map(_.expand(property)))

		override def toString = restrictions.mkString("("," and ", ")")
	}
	
//	object Conjunction {
//		class ConjunctionOpressor[]
//	}


	/** Restriction representing a logical disjunction (or) of restrictions on the same type */
	case class Disjunction[-T](restrictions :Seq[Restriction[T]]) extends Restriction[T] {

		override def ||[S <: T](other: Restriction[S]): Restriction[S] = other match {
			case Disjunction(others) => Disjunction(restrictions ++: others)
			case _ => Disjunction(restrictions :+ other)
		}

		override def expand[X](property: PropertyChain[X, T]): Restriction[X] =
			Conjunction(restrictions.map(_.expand(property)))

		override def toString = restrictions.mkString("(", " or ", ")")
	}



	/** Create restrictions comparing two expressions for equality and matches such restrictions */
	object Equality {


		/** A Opressor constraining the whole value of T */
		def apply[T :TypeTag]() :Opressor[T, T] = new EqualityOpressor[T, T] //opressor.asInstanceOf[Opressor[T, T]]

		/** Create a restriction meaning 'values of T equal to value'. */
		def apply[T :TypeTag](value :T) :Restriction[T] = new EqualityTest[T, T](Restrictive.Self[T], Restrictive.Literal[T, T](value))

		/** Create a restriction representing an equality test between the given to terms. */
		def apply[T, V](left :Restrictive[T, V], right :Restrictive[T, V]) :Restriction[T] = new EqualityTest(left, right)

		/** Check if the given restriction is an equality restriction with a specified value.
		  * It will also match membership restrictions for singleton sets, so this check should be performed before
		  * any future membership check.
	      */
		def unapply[T](restriction :Restriction[T]) :Option[(Restrictive[T, Any], Restrictive[T, Any])] =
			restriction.ifSubclass[EqualityTest[T, Any]] {
				e => (e.left, e.right)
			} orElse Membership.unapply(restriction).collect { case (left, right) if right.size==1 => (left, right.head) }

		private[Restriction] def apply[T](tpe :Type) :Opressor[T, T] = new EqualityOpressor[T, T](tpe)

		private[Restriction] case class EqualityTest[-T, V](left :Restrictive[T, V], right :Restrictive[T, V]) extends Restriction[T] {

			override def expand[X](property: PropertyChain[X, T]): Restriction[X] =
				EqualityTest(property ++: left, property ++: right)

			override def toString = s"$left=$right"
		}


		private[Restriction] class EqualityOpressor[L>:R, -R](private val tpe :Type) extends Opressor[L, R] {
			def this()(implicit tag :TypeTag[L]) = this(typeOf[L])

			override def apply(value: L): Restriction[R] = LiteralEquality(value, tpe)

			def like(value :R) :Restriction[R] = apply(value)

			def from(value: R): Option[L] = Some(value)

			def from(restriction :Restriction[_<:R]) = restriction match {
				case Equality(Self(tpe), Literal(v)) if tpe <:< this.tpe =>
					Some(v.asInstanceOf[L])
				case Equality(Literal(v), Self(tpe)) if tpe <:< this.tpe =>
					Some(v.asInstanceOf[L])
				case _ => None
			}


			def in :Opressor[Set[L], R] = Membership[L](tpe)

			override def toString = "self=?"
		}

//		private val opressor = new EqualityOpressor[Any, Any]

	}



	/** Create restrictions comparing value for equality and matches such restrictions */
	object LiteralEquality {
		import Restrictive._
		/** A Opressor constraining the whole value of T */
		@inline
		def apply[T :TypeTag]() :Opressor[T, T] = Equality[T]() //opressor.asInstanceOf[Opressor[T, T]]

		/** Create a restriction meaning 'values of T equal to value'. */
		@inline
		def apply[T :TypeTag](value :T) :Restriction[T] = Equality(Restrictive.Literal[T, T](value))//new Equality(value)



		/** Check if the given restriction is an equality test against a literal value created by Equality or LiteralEquality.
		  * It will also match membership restrictions for singleton sets, so this check should be performed before
		  * any future membership check.
		  */
		def unapply[T](restriction :Restriction[T]) :Option[_>:T] = restriction match {
			case Equality(Self(_), Literal(value)) => Some(value)
			case Equality(Literal(value), Self(_)) => Some(value)
			case _ => None
		}

		private[Restriction] def apply[T](value :T, tpe :Type) :Restriction[T] = Equality[T](tpe)===value

	}





	/** Create restrictions checking if a value is a member of a given set of values and matches such restrictions. */
	object Membership {
		import Restrictive._

		/** Create a opressor for type T accepting set of values to test against. */
		def apply[T :TypeTag]() :Opressor[Set[T], T] = new MembershipOpressor[T, T] //opressor.asInstanceOf[Opressor[Set[T], T]]

		/** Create a restriction meaning 'values of T equal to any member of values'.
		  * It might perform eagar optimisation by returning Unsatisfiable/Equality restrictions for empty/singleton sets.
		  */
		def apply[T :TypeTag](values :Iterable[T]) :Restriction[T] = values.size match {
			case 0 => False
			case 1 => Equality(values.head)
			case _ => new MembershipTest[T, T](Self[T], values.map(Literal[T, T](_)))
		}

		/** Create a restriction representing membership test for the value specified by the first argument and the collection of terms given by the second argument. */
		def apply[T, V](member :Restrictive[T, V], values :Iterable[Restrictive[T, V]]) :Restriction[T] = values.size match {
			case 0 => False
			case 1 => Equality(member, values.head)
			case _ => new MembershipTest[T, V](member, values)
		}

		/** Check if the given restriction is a membership test for a set.
		  * This will also match all effective equality, whether it was created by Equality or MemberOf(Set(singleton)),
		  * so check for equality before checking for membership restriction.
		  */
		def unapply[T](restriction :Restriction[T]) :Option[(Restrictive[T, Any], Set[Restrictive[T, Any]])] =
			restriction.ifSubclassOf[Equality.EqualityTest[T, Any]] {
				e => (e.left, Set(e.right))
			} orElse restriction.ifSubclassOf[MembershipTest[T, Any]] {
				e => (e.member, e.values.toSet)
			}

		private[Restriction] def apply[T](tpe :Type) :Opressor[Set[T], T] = new MembershipOpressor[T, T](tpe)

		private case class MembershipTest[T, V](member :Restrictive[T, V], values :Iterable[Restrictive[T, V]]) extends Restriction[T] {

			override def expand[X](property: PropertyChain[X, T]): Restriction[X] =
				MembershipTest(property ++: member, values.map(property ++: _))

			override def toString = values.mkString(s"$member in (", ", ", ")")
		}

		private[Restriction] class MembershipOpressor[P>:T, -T](private val tpe :Type) extends Opressor[Set[P], T] {
			def this()(implicit tag :TypeTag[P]) = this(typeOf[P])

			override def apply(values: Set[P]): Restriction[T] = Membership(tpe)(values)

			override def from(value: T): Option[Set[P]] = Some(Set(value))

			override def from(restriction: Restriction[_<:T]): Option[Set[P]] = restriction match {
				case Membership(Self(tpe), values) if tpe <:< this.tpe =>
					values.collect { case Literal(v) => v.asInstanceOf[P] }.toSet.providing(_.size==values.size)
				case _ => None
			}

			override def in: Opressor[Set[Set[P]], T] = map(Set(_:Set[P]), _.flatten)

			override def like(value: T): Restriction[T] = apply(Set(value))

			override def toString = "value in ?"
		}
//		private val opressor = new MembershipOpressor[Any]
	}




	/** Create restrictions checking if a value is a member of a given set of literals and matches such restrictions */
	object MemberOf {
		import Restrictive._

		/** Create a opressor for type T accepting set of values to test against. */
		def apply[T :TypeTag]() :Opressor[Set[T], T] = Membership[T]() //opressor.asInstanceOf[Opressor[Set[T], T]]

		/** Create a restriction meaning 'values of T equal to any member of values'.
		  * It might perform eagar optimisation by returning Unsatisfiable/Equality restrictions for empty/singleton sets.
		  */
		def apply[T :TypeTag](values :Set[T]) :Restriction[T] = Membership(Self[T], values.map(Literal[T, T] _))

		/** Check if the given restriction is a membership test for a set of literal values, created by Membership or MemberOf..
		  * This will also match all effective equality, whether it was created by Equality or MemberOf(Set(singleton)),
		  * so check for equality before checking for membership restriction.
	      */
		def unapply[T](restriction :Restriction[T]) :Option[Set[_>:T]] = restriction match {
			case Membership(Self(_), values) =>
				values.collect { case Literal(v) => v }.toSet.providing(_.size==values.size)
			case _ => None
		}


	}






	/** Factory of restrictions working on the value of type P of some property of E. */
	case class PropertyOpressor[P, -E] private[Restriction] (property :PropertyChain[E, P]) extends Opressor[P, E] {

		def apply(propertyValue :P) :Restriction[E] = property === propertyValue //PropertyRestriction(property, propertyValue)

		//		def in(values :Set[P]) :Restriction[E] = PropertyIn(property, values)

		def like(ownerValue :E) :Restriction[E] = property === property(ownerValue) //PropertyRestriction(property, property(ownerValue))

		def from(value: E): Option[P] = Some(property(value))

		override def from(restriction: Restriction[_<:E]): Option[P] = restriction match {
			case PropertyEquals(prop, value) if prop==property => Some(value.asInstanceOf[P])
			case _ => None
		}

		override def in: Opressor[Set[P], E] = PropertyIn(property)

		override def toString = s"$property=?"
	}




	/** Creates and matches restrictions comparing the value of a given property of the target to a specified value */
	object PropertyEquals {
		def apply[E, P](property :PropertyChain[E, P]) :PropertyOpressor[P, E] =
			new PropertyOpressor(property)

		def apply[E :TypeTag, P](property :E=>P) :PropertyOpressor[P, E] =
			new PropertyOpressor(PropertyChain(property))

		def apply[E, P](property :PropertyChain[E, P], value :P) :Restriction[E] =
//			new PropertyRestriction(property, Equality(value))
			property === value

		def apply[E :TypeTag, P](property :E=>P, value :P) :Restriction[E] =
			apply(PropertyChain(property), value)

		def apply[E, P](value :E, property :PropertyChain[E, P]) :Restriction[E] =
//			new Restriction[E](property, Equality(property(value)))
			property === property(value)

		def apply[E :TypeTag, P](value :E, property :E=>P) :Restriction[E] =
//			apply(value, PropertyChain(property))
			property === property(value)


		def unapply[E](that :Restriction[E]) :Option[(PropertyChain[E, Any], Any)] = that match {
			case Equality(Property(property), Literal(literal)) => Some(property, literal)
			case Equality(Literal(literal), Property(property)) => Some(property, literal)
//			case PropertyRestriction(property, LiteralEquality(value)) => Some(property, value)
			case _ => None
		}
	}




	/** Creates and matches restrictions checking if the value of a given property of the target is contained in the given set. */
	object PropertyIn {
		def apply[E, P](property :PropertyChain[E, P]) :Opressor[Set[P], E] =
			new PropertyInOpressor(property)

		def apply[E :TypeTag, P](property :E=>P) :Opressor[Set[P], E] =
			new PropertyInOpressor(PropertyChain(property))


		def apply[E, P](property :PropertyChain[E, P], values :Set[P]) :Restriction[E] =
			//new PropertyRestriction(property, MemberOf(values))
			property in values

		def apply[E :TypeTag, P](property :E=>P, values :Set[P]) :Restriction[E] =
			apply(PropertyChain(property), values)


		def unapply[E](that :Restriction[E]) :Option[(PropertyChain[E, Any], Set[Any])] = that match {
			case Membership(Property(property), values) =>
				values.collect { case Literal(v) => v }.providing(_.size==values.size).map((property, _))
//			case PropertyRestriction(property, MemberOf(values)) => Some(property, values)
			case PropertyEquals(property, value) => Some((property, Set(value)))
			case _ => None
		}

		private case class PropertyInOpressor[P, -E](property :PropertyChain[E, P]) extends Opressor[Set[P], E] {

			override def apply(values: Set[P]): Restriction[E] = PropertyIn(property, values)

			override def from(value: E): Option[Set[P]] = Some(Set(property(value)))

			override def from(restriction: Restriction[_<:E]): Option[Set[P]] = restriction match {
				case PropertyIn(prop, values) if prop==property => Some(values.asInstanceOf[Set[P]])
				case _ => None
			}

			override def like(value: E): Restriction[E] = PropertyIn(property, Set(property(value)))

			override def in: Opressor[Set[Set[P]], E] = map(Set(_), _.flatten)

			override def toString = s"$property in ?"
		}
	}



	trait ExistentialRestriction[-T, E] extends Restriction[T] {
		def element :Restriction[E]

		def composition :ComposableFrom[_>:T, E] = composedOf.composition
		def decomposition :DecomposableTo[T, E] = composedOf.decomposition

		def composedOf :ComposedOf[_>:T, E]

	}



	object ExistentialRestriction {

		def unapply[T](restriction: Restriction[T]): Option[(Restriction[E], C ComposedOf E) forSome {type E; type C >: T}] =
			restriction.ifSubclass[ExistentialRestriction[T, Any]] { e => (e.element, e.composedOf) }

		abstract class AbstractExistentialRestriction[-T, E](val element: Restriction[E], comp: ComposedOf[T, E])
			extends ExistentialRestriction[T, E]
		{
			def composedOf: ComposedOf[_ >: T, E] = comp

			protected def name = Names.unqualifiedClassName(this)

			override def expand[X](property: PropertyChain[X, T]): Restriction[X] =
				throw new UnsupportedOperationException(s"Existential restrictions cannot be expanded: $property.$this")


			override def toString = s"$name[$composition]($element)"

			override def equals(that: Any) = that match {
				case e: ExistentialRestriction[_, _] =>
					(e eq this) || canEqual(e) && e.canEqual(this) && element == e.element && composition.compatibleWith(e.composition)
				case _ => false
			}

			override def hashCode = element.hashCode

		}

	}



	class ForAll[T, E](element :Restriction[E])(implicit composition :T ComposedOf E)
		extends AbstractExistentialRestriction[T, E](element, composition)
	{

		override def canEqual(that :Any) = that.isInstanceOf[ForAll[_, _]]
	}


	object ForAll {
		def apply[T, E](element :Restriction[E])(implicit composition :T ComposedOf E) :ExistentialRestriction[T, E] =
			new ForAll(element)

		def unapply[T, E](restriction :Restriction[T])(implicit composition :T ComposedOf E) :Option[Restriction[E]] =
			restriction.asSubclass[ForAll[T, E]].filter(_.decomposition.compatibleWith(composition.decomposition)).map{ _.element }
	}



	class Exists[T, E](element :Restriction[E])(implicit composition :T ComposedOf E)
		extends AbstractExistentialRestriction[T, E](element, composition)
	{


		override def canEqual(that :Any) = that.isInstanceOf[Exists[_, _]]
	}





	object Exists {
		def apply[T, E](element :Restriction[E])(implicit composition :T ComposedOf E) :ExistentialRestriction[T, E] =
			new Exists(element)

		def unapply[T, E](restriction :Restriction[T])(implicit composition :T ComposedOf E) :Option[Restriction[E]] =
			restriction.asSubclass[Exists[T, E]].filter(_.decomposition.compatibleWith(composition.decomposition)).map{ _.element }


	}

	object IsSome {
		def apply[T](restriction :Restriction[T]) :ExistentialRestriction[Option[T], T] = Exists[Option[T], T](restriction)

		def unapply[T](restriction :Restriction[T]) :Option[Restriction[_]] =
			restriction.asSubclass[ExistentialRestriction[T, _]].filter(_.decomposition==DecomposableTo.optional[Any]).map { _.element }
	}






	private case class MappedOpressor[X, Y, -T](backing :Opressor[X, T])(back :X=>Y, there :Y=>X) extends Opressor[Y, T] {

		override def apply(value: Y): Restriction[T] = backing(there(value))

		override def from(value: T): Option[Y] = backing.from(value).map(back)

		override def from(restriction: Restriction[_<:T]): Option[Y] = backing.from(restriction).map(back)

		override def in: Opressor[Set[Y], T] = backing.in.map(_.map(back), _.map(there))

		override def like(value: T): Restriction[T] = backing.like(value)
	}




}
