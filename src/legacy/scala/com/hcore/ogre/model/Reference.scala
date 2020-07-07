package com.hcore.ogre.model

import com.hcore.ogre.model.MappedReference.{PropertyMapper, ReferenceMapper}
import com.hcore.ogre.model.Reference.{Empty, Full}
import com.hcore.ogre.model.Restriction.{ForAll, Opressor}
import com.hcore.ogre.slang.matching
import com.hcore.ogre.slang.options.extensions
import matching.&&

import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.collection.generic.CanBuildFrom



//implicits
import extensions._


/** A value of type T or an identification of such a value. A reference can be either Full -
  * contain a value or be able to compute it without significant cost and outside resources - or Empty,
  * in which case it should contain information required to locate and compute the given value. An exception
  * to this case is Unknown, which is both Empty and doesn't contain any information about the value, but should
  * be used sparingly as most operations won't be able to do anything useful with it and may result in an error.
  *
  * References are particularly useful to represent associations between different database entities or
  * resources which can be otherwise expensive to compute and shouldn't be fetched by default - they shield model classes
  * from the implementation details about how those associations are implemented and resolved. A reference thus might contain an associated,
  * pre-fetched entity, or a foreign key to it. They can be used for any type, including collections, and an empty
  * reference for a to-many relationship can for example specify a reverse foreign key (foreign key on the target entity
  * and referenced key on this entity). They can also serve as actual search filters - queries are after all only
  * a filter specification on a last.
  *
  * As the reference is immutable (at least in the interface - implementations may use mutable state as long as the client
  * will never see different results for the same call on the reference), it is covariant in regards to the value type -
  * a Reference[A] is intuitively a Reference[B] for B>:A. Their dual nature of value-or-specification-of-a-value has
  * however a hidden contravariant element to it in the latter form: in the most trivial example, a reference specifying
  * 'all instances of the given type' can be interpreted differently when viewed as a reference to a super type - larger
  * set of theoretically possible values. In most cases it won't matter and the set of possible values is constrained externally
  * in an explicit way, but it is something to be aware of.
  *
  * @tparam T type of the associated value.
  */
trait Reference[+T] {
	//todo: find a more unique, hip name - maybe a guide? matchmaker? envoy?
	/** This reference as an option - Some(value) if the value is available and None if not */
	def toOpt :Option[T]

	/** Return the value, if it is available. If this reference is empty it will throw NoSuchElementException */
	def get = toOpt.get


	def map[X](fun :ReferenceMapper[T, X]) :Reference[X] = MappedReference(this, fun)

	def map[X](property :PropertyChain[T, X]) :Reference[X] = map(PropertyMapper(property))


	@deprecated("refactor to a separate fetchable property", "now")
	def join :T = throw new UnsupportedOperationException(s"$this.join")


	def canEqual(that :Any) = that.isInstanceOf[Reference[_]]

	override def equals(that :Any) = that match {
		case a:Reference[_] => a.canEqual(this) && toOpt==a.toOpt
		case _ => false
	}

	override def hashCode = toOpt.hashCode

	override def toString = toOpt.map(o => s"@Full($o)") getOrElse "@empty"

}





object Reference {

	class MutableReference[T](var toOpt :Option[T]) extends Reference[T]

	trait CloneableReference[T, R<:CloneableReference[T, R]] extends Reference[T] { this :R =>
		def clone(value :Option[T]) :R
	}


	implicit def asOption[T](reference :Reference[T]) :Option[T] = reference.toOpt

	/** Create a Full reference with value T and no other information */
	def apply[T](value :T) :Reference[T] = Full(value)


	/** Create a reference in a two step process - the first step (this one) returns a ReferenceComposer containing
	  * all information required to compute the referenced value. In a second step, a call on the returned instance
	  * will specify the expected result type for the found values (single value, Seq, Set, etc). Implicit convertion
	  * exists converting a ReferenceComposer[T] to a Reference[T].
	  *
	  * Example: Reference(PropertyEquals(idProperty, id)).single
	  *
	  * This call delegates to Satisfying(restriction)
	  *
	  * @param restriction a filter with all parameters on the universe of values for T
	  * @tparam T value type on which the given filter operates.
	  * @return A factory for references of different types that can be built from a collection of T values.
	  */
	def apply[T](restriction :Restriction[T]) :ReferenceComposer[T] = Satisfying(restriction)


	/** Create a reference in a three step process - the first stop (this one) accepts a definition of how to look for
	  * the value(s) (for example, by a primary key) and returns a ReferenceFactory. The second step specifies paramaters
	  * for the first step and provides all information needed to find the value - for example, the id of the entity or
	  * its actual value, and returns a ReferenceComposer. Returned reference composer can be used to specify the
	  * desired type of the created reference and how can it be constructed from values found by the filter created
	  * in the previous steps.
	  *
	  * Example: Reference(PropertyEquals(foreignKey))(referencedId).in[Seq]
	  * Above chained call will create a reference to a Seq[T], assuming foreignKey is a property of T, and all
	  * intermediate results (breaks between parameter lists) can be stored for reuse or future completion.
	  *
	  * This call delegates to Satisfying(constrainer)
	  */

	def apply[K, T](opressor :Opressor[K, T]) :ReferenceFactory[K, T, T] = Satisfying(opressor)


	implicit def singleReference[T](factory :ReferenceComposer[T]) :Reference[T] = factory.single
	implicit def optionReference[T](factory :ReferenceComposer[T]) :Reference[Option[T]] = factory.optional
	implicit def collectionReference[C<:Iterable[T], T](factory :ReferenceComposer[T])(implicit cbf :CanBuildFrom[_, T, C]) :Reference[C] = factory.as[C]


	/** Container for a specification of a filter on type T, which can request to force the view of the result set
	  * to a different type, depending on expected amount of values - it can be a single value, or just Reference[T],
	  * a reference to an Option[T], or a collection. Resolving a reference for which the number of results cannot be
	  * coerced into the requested type will result in an error, rather than ommiting superflous results.
	  * @tparam T
	  */
	trait ReferenceComposer[T] {
		/** Return result set as a single element */
		def single :Reference[T]

		/** Return result set as zero or one element. Note that Reference[Option[T]] is conceptually different than
		  * Option[Reference[T]] - in the latter case the number of results is known before hand, while in the former
		  * only when resolving the reference.
		  */
		def optional :Reference[Option[T]] = as[Option[T]]

		/** Return the result set as a composite type C. A composite type is a type which can be constructed
		  * and deconstructed to Iterable[T] (possibly with limitations to its size). The main examples for
		  * composites of T are: just T, Option[T], _<:Iterable[T].
		  * @param expand implicit definition of how the result is mapped into a collection of T values.
		  * @tparam C reference value type
		  */
		def as[C](implicit expand :C ComposedOf T) :Reference[C]

		/** A shorthand for as[C[T]] - you can write just reference.in[Seq] to obtain a Reference[Seq[T]]. */
		def in[C[X]](implicit expand :C[T] ComposedOf T) :Reference[C[T]] = as[C[T]]
	}

	
	/** A shorthand for a reference factory producing Reference[E] based on key K. */
	type SingletonReferenceFactory[K, E] = GenericReferenceFactory[K, E, E, Reference[E]]
	
	/** Shorthand type for reference factory returning just Reference[X] */
	type ReferenceFactory[K, E, X] = GenericReferenceFactory[K, E, X, Reference[X]]


	/** Represents a static way of referencing a given entity E as type X by some sort of 'key' K.
	  * Generally, full references created by this instance will contain a value of type X
	  * (from which the value of K could be deduced) while empty references will hold the value of the key K.
	  * This may mean something simple like 'all entities of type E where a given property of value type K
	  * has the given value' or something more complex.
	  * Instances should implement a sensible equals method - two instances of a given factory class should be equal
	  * if and only if they return equal references from their respective factory methods for all
	  * arguments (especially empty(key)). Similarly, the equivalencyToken should implement a less strict equality abstracting
	  * from the composite type X of created references, where equivalency tokens for two factories are equal if the same value of K
	  * refers to the same set of entities.
	  * @tparam K 'key' type - type of value used to create empty references, for example value type of a foreign key property
	  * @tparam E type of underlying referenced entity
	  * @tparam X type parameter of returned references - should satisfy X ConsistsOf E
	  * @tparam R a specific subclass of Reference being the result type of all factory methods
	  */
	trait GenericReferenceFactory[K, E, X, +R<:Reference[X]] {
		/** Composition definition for reference type X and target entity type E */
		def items :X ComposedOf E
		
		/** Create a lazy reference, which value will be evaluated lazily only when toOpt or get is called. */
		def delayed(key :K, value : =>X) :R
		
		/** Create an empty or full instance, depending on whether value is defined. Same as apply. */
		def create(key :K, value :Option[X]) :R = apply(key, value)
		
		/** Create an empty or full instance, depending on whether value is defined. */
		def apply(key :K, value :Option[X]) :R = value.map(full) getOrElse empty(key)

		/** Create a full instance with the given value */
		def full(value :X) :R

		/** Create a full instance based on (assumingly complete) collection of elements. Equivalent to full(composition(_)) */
		def forItems(items :Iterable[E]) :Option[R] = this.items.composition.attempt(items).map(full)

		/** Create a full instance based on (assumingly complete) collection of elements and the key pointing to them.
		  * Equivalent to apply(key, composition(items)).
		  */
		def forItems(key :K, items :Iterable[E]) = this.items.composition.attempt(items).map(x => create(key, Some(x)))

		/** Create an empty instance referencing all entities of type E with the specific value as the key, as understood by this factory. */
		def empty(key :K) :R

		/** Create an empty instance referencing al entities of type E with the specific value as the key, as understood by this factory. Same as empty. */
		def apply(key :K) :R = empty(key)

		/** Try to retrieve the key out of the given target item. */
		def keyFor(item :E) :Option[K]

		/** Try to retrieve the key out of the given reference. In case of non-empty result, empty(key) should return reference equal to the argument */
		def keyOf[F>:R <:Reference[X]](ref :F) :Option[K]

		/** Retrieve the key out of the given reference or throw an exception if not possible. Relies on keyOf() implementation */
		def forceKeyOutOf[F>:R <:Reference[X]](ref :F) :K =
			keyOf(ref) getOrElse (throw new IllegalArgumentException(s"$this: can't find key in $ref"))

		def unapply(ref :Reference[X]) :Option[(K, Option[X])] = keyOf(ref).map((_, ref.toOpt))

		def adapt(ref :Reference[X]) :Option[R] = unapply(ref) map { case (key, value) => apply(key, value) }


//		def map[Y](mapper :ReferenceMapper[E, Y]) :ReferenceFactory[K, Y, Y]
//		/** Return a factory for references referencing a given property of X. */
//		def property[X](property :PropertyPath[X, X]) :ReferenceFactory[K, X, X]
//
//		/** Return a factory for references referencing a given property of X. */
//		def property[X](property :X=>X)(implicit tag :TypeTag[X]) :ReferenceFactory[K, X, X] =
//			this.property(PropertyPath(property))

		/** Return a factory creating references for type Y, where Y is another composite type for referenced entity E. */
		def as[Y](implicit composition :Y ComposedOf E) :ReferenceFactory[K, E, Y]

		/** A shorthand for as[Y[E]], working on higher kinded single-param generic result type Y.
		  * Thus, Writing in[Seq] will create an instance returning Reference[Seq[E]]
	      */
		def in[Y[V]](implicit composition: Y[E] ComposedOf E) :ReferenceFactory[K, E, Y[E]] = as[Y[E]]

		/** A weaker comparison than equals, which should be true if and only if empty references returned by both factories
		  * are equal for all possible key values. Conceptually this represents factories always referring to the same underlying
		  * entities, but possibly exporting them as different composite types or even reference types. For example,
		  * factories returning Reference[Seq[E]] and Reference[Set[E]] should be equivalent, if they always resolve the same key
		  * to the same list of E passed as constructor arguments to created full references.
		  */
		def equivalent(other :GenericReferenceFactory[K, E, _, _]) :Boolean = equivalencyToken==other.equivalencyToken

		/** A hash value which will be the same for any two equivalent instances as defined by the equivalent() method.
		  * Of course, the euqals/hashCode contract means that hashCodes for those two instances should also be equal.
		  */
		def equivalencyToken :Any

		def canEqual(that :Any) = that.isInstanceOf[GenericReferenceFactory[_, _, _, _]]
	}



	trait HigherKindReferenceFactory[K, E, X, R[T]<:Reference[T]] extends GenericReferenceFactory[K, E, X, R[X]] {

		override def as[Y](implicit composition: ComposedOf[Y, E]): HigherKindReferenceFactory[K, E, Y, R]

		override def in[Y[V]](implicit composition: ComposedOf[Y[E], E]): HigherKindReferenceFactory[K, E, Y[E], R] =
			as[Y[E]]
	}


	trait ReferenceFactorySupport[K, E, X] extends ReferenceFactory[K, E, X] {
		override def delayed(key: K, value: => X): Reference[X] = Lazy(value)

		override def full(value: X): Reference[X] = Full(value)

	}


//	type ConstrainedReferenceFactory[K, T, C] = TypedConstrainedReferenceFactory[K, T, C, Reference[C]]

	case class RestrictedReferenceFactory[K, E, X](opressor :Opressor[K, E])(implicit val items :X ComposedOf E)
		extends HigherKindReferenceFactory[K, E, X, Reference]
	{
//		private[this] implicit val FullOf = CompositeReference.DecomposeTo[E]

		override def delayed(key :K, value: => X): Reference[X] = Lazy(value)
		
		override def full(value: X): Reference[X] = Full(value) //Reference(composition(Seq(value)))

		override def empty(key: K): Reference[X] = Satisfying(opressor(key)).as[X](items)

		override def keyFor(item: E): Option[K] = opressor.from(item)

		override def keyOf[F>:Reference[X]<:Reference[X]](ref: F): Option[K] = ref match {
			case Satisfying(opressor(key)) => Some(key)
			case items.Full(values) =>
				if (values.isEmpty) None
				else opressor.from(values.head).filter(v => values.tail.forall(v==_))
			case _ => None
		}


		override def as[Y](implicit composition: ComposedOf[Y, E]): RestrictedReferenceFactory[K, E, Y] =
			new RestrictedReferenceFactory[K, E, Y](opressor)(composition)

		override def in[Y[V]](implicit composition: Y[E] ComposedOf E) :RestrictedReferenceFactory[K, E, Y[E]] = as[Y[E]]


		def equivalencyToken :Any = opressor


		override def canEqual(that: Any): Boolean = that.isInstanceOf[RestrictedReferenceFactory[_,_,_]]

		override def equals(that :Any) = that match {
			case c:RestrictedReferenceFactory[_, _, _] =>
				(this eq c) || c.canEqual(this) && c.opressor==opressor && c.items==items
			case _ => false
		}



		override def toString = s"$items($opressor)"
	}


	/** Implementation delegating all calls to another reference factory - useful as a base class for extendeding functionality */
	case class ReferenceFactoryProxy[K, E, X, +R<:Reference[X]](protected val references :GenericReferenceFactory[K, E, X, R])
		extends GenericReferenceFactory[K, E, X, R]
	{
		override def items: ComposedOf[X, E] = references.items

		override def empty(key: K): R = references.empty(key)

		override def delayed(key: K, value: => X): R = references.delayed(key, value)

		override def equivalencyToken: Any = references.equivalencyToken

		override def keyFor(item: E): Option[K] = references.keyFor(item)

		override def keyOf[F>:R<:Reference[X]](ref: F): Option[K] = references.keyOf(ref)

		override def full(value: X): R = references.full(value)

		override def as[Y](implicit composition: ComposedOf[Y, E]): ReferenceFactory[K, E, Y] =
			references.as[Y]
	}


	/** Factory and matcher for references about which values' we don't know anything or don't care.
	  * It should be used very sparingly, as in most cases nothing sensible can be done with such a reference
	  * and passing it will produce an error, but it can be useful to mean 'not changed' in updated values,
	  * or save defining otherwise expensive to compute to-many relationships as Option[Reference[T]] - assuming
	  * this practice is shared by the whole codebase.
	  */
	object Unknown {
		/** Create an empty, unknown reference to some value of X */
		def apply[X]() :Reference[X] = instance.asInstanceOf[Reference[X]]

		/** Check if this reference was explicitly created as unknown by the accompanied factory method. */
		def unapply[X](ref :Reference[X]) :Boolean = ref==instance

		private case object instance extends Empty[Nothing] {
			override def toString = "@Unknown"
		}

	}



	private class Full[T](value :T) extends Reference[T] {
		def toOpt = Some(value)
	}

	private trait Empty[T] extends Reference[T] {
		def toOpt = None
	}


	/** Factory and matcher for full references, i.e. those with an associated, computed value. */
	object Full {
		/** Create a full reference containing the given value and no other information. */
		def apply[T](value :T) :Reference[T] = new Full(value)

		/** Check if this reference has a ready value associated with it.
		  * It will always return the value of the passed reference, regardless of any other information about it
		  * and whether it was created by this object or not.
		  */
		def unapply[T](ref :Reference[T]) :Option[T] = ref.toOpt

	}


	/** Check if a reference is empty, i.e. contains no value */
	object Empty {
//		def apply[T] :NewRef[T] = new Empty[T] {}
		/** Checks if ref.toOpt.isEmpty */
		def unapply(ref :Reference[Any]) :Boolean = ref.toOpt.isEmpty

	}





	/** Reference all instances of a given type in some universe, as defined by the resolver.
	  * Depending on the implementaion of a handler, it can return all rows in a last, or in a collection of tables
	  * for related types.
	  */
	object All {
		/** Create a composer as the first step of reference creation. Returned composer can be asked to produce
		  * a reference exporting values of the given type as a desired composite type (for example, a collection or option).
		  * Example: All[E].as[Seq[E]]
		  *
		  * @tparam T type of the given
		  * @return
		  */
		def apply[T] = composer.asInstanceOf[ReferenceComposer[T]]

		/** Was this reference explicitly created as 'All' by this object? */
		def unapply[T](reference :Reference[T]) :Boolean = reference==Everything

		/** Create an 'All' reference as Reference[X] - exact interpretation will depend on the client code
		  * and may not always make sense, i.e. All.as[Int] doesn't mean 'all integers', but 'all of an integer'.
		  * Generally, All.as[Entity] will mean 'there should be exactly one Entity', and All.as[Seq[Entity]]
		  * 'all entities in a seq'. It is shorter to write than All[X].as[X] or All[X].as[Seq[X]].
		  */
		def as[X] :Reference[X] = Everything.asInstanceOf[Reference[X]]

		private case object Everything extends Empty[Nothing] {
			override def toString = "All"
		}
		private val composer = new ReferenceComposer[Any] {
			override def single: Reference[Any] = as[Any] //Everything
			override def as[C](implicit expand: C ComposedOf Any): Reference[C] = as[C] //Everything
		}
	}


	private case class RestrictedReference[T](restriction :Restriction[T], toOpt :Option[T]=None) extends Reference[T] {
		override def toString = toOpt match {
			case None => s"Empty[$restriction]"
			case Some(x) => s"$restriction($x)"
		}
	}


	/** Factory and inspection of references specifying all instances of a given type which satisfy a given restriction. */
	object Satisfying {

		/** Initiate the first step of a chained building process for a Reference[C[T]] for some composite type C.
		  * Accepts an opressor, defining how
		  * to look for a value (for example, by a foreign key to another entity), and returns a factory,
		  * which will allow specifying actual parameters for the restriction (i.e, the id of a target entity) and in
		  * what collection the values should be returned.
		  * @param constrainer Restriction factory accepting keys of type K and producing instances of Restriction[T]
		  * @tparam K key type for the constrainer which will be accepted later by the returned factory
		  * @tparam T constrained type and the 'element' type of the finally constructed reference.
		  * @return a factory object accepting actual parameters needed to construct the reference
		  */
		def apply[K, T](constrainer :Opressor[K, T]) :RestrictedReferenceFactory[K, T, T] =
			new RestrictedReferenceFactory[K, T, T](constrainer)


		/** Create a composer allowing to create references containing values of T - for example Reference[T],
		  * Reference[Option[T]], Reference[Seq[T]] and so on.
		  * @param restriction specification of how to look for the value (for example, a query filter with parameters)
		  * @tparam T filtered type
		  * @return a factory of references with values being various collections of T
		  */
		def apply[T](restriction :Restriction[T]) :ReferenceComposer[T] = new ForAllComposer(restriction)


		/** Check if the given reference is a restriction reference created by this instance and return restriction held by it.
		  * Even if this reference was in fact created by this object, a check is performed if the composition used to
		  * create it is the same as the one passed implicitly, to assert that the restriction type parameter for the restriction
		  * stored in this reference is actually compatible with the one specified implicitly. If they are not, this method
		  * will not match. The check is performed using the compatibility comparison method for ComposedOf, so it is not 100% bullet proof.
		  *
		  * @param reference reference to match
		  * @param composition specification on what type T C should be decomposed to and how
		  */
		def unapply[C, T](reference :Reference[C])(implicit composition :C ComposedOf T) :Option[Restriction[_<:T]] =
			reference match {
				case RestrictedReference(ForAll(restriction), _) => Some(restriction)
				case _ => None
			}
//			reference match {
//				case (all :AllInstancesSatisfying[_, _]) && composition(_) =>
//					Some(all.restriction.asInstanceOf[Restriction[_<:T]])
//				case _ => None
//			}



//		private case class AllInstancesSatisfying[C, T](restriction :Restriction[T], override val composedOf :ComposedOf[C, T])
//			extends BaseCompositeReference[C, T]()(composedOf)


		private class ForAllComposer[T](restriction :Restriction[T]) extends ReferenceComposer[T] {
			override def single: Reference[T] = new RestrictedReference[T](ForAll[T, T](restriction))//new AllInstancesSatisfying[T, T](restriction, ComposedOf.itself)//new AllThat[T, T](restriction, ConsistsOf.itself)

			override def as[C](implicit composition: C ComposedOf T): Reference[C] =
				new RestrictedReference[C](ForAll[C, T](restriction)) //new AllInstancesSatisfying[C, T](restriction, composition)//AllThat(restriction, composition)
		}

	}




	object Single {
		def apply[K, T](constrainer :Opressor[K, T]) :RestrictedReferenceFactory[K, T, T] =
			new RestrictedReferenceFactory[K, T, T](constrainer)

		def apply[T](restriction :Restriction[T]) :Reference[T] = Satisfying(restriction)

		def apply[T](value :T) :Reference[T] = Full[T](value)

		def unapply[T](reference :Reference[T]) :Option[Restriction[_<:T]] = reference match {
			case Satisfying(restriction) => Some(restriction)
			case _ => None
		}

	}




	/** Creates lazy instances - arguments of passed methods are evaluated only when the returned instance is accessed.
	  * Passed expressions should be thread safe and idempotent, as there is no guarantee they'll be executed at most once
	  * when the returned instance
	  */
	object Lazy {
		/** Create a full reference with a lazy value. Argument will be evaluated when toOpt (or get) method of the returned reference as called.
		  * @param value a pass-by-name expression evaluating to the value of the reference, which should be idempotent and thread safe.
		  */
		def apply[T](value : =>T) :Reference[T] = new LazyReference(()=>Some(value))

		/** Create a reference factory delegating all calls to the specified target reference factory. Expression will be evaluated
		  * when any of the returned factory methods are called. This is useful when the cycles between references would make
		  * initialization otherwise impossible.
		  * @param factory a pass-by-name expression evaluating to a factory - should be idempotent and thread safe.
		  */
		def Factory[K, T, C, R<:Reference[C]](factory : =>GenericReferenceFactory[K, T, C, R]) :GenericReferenceFactory[K, T, C, R] =
			new LazyReferenceFactory(() => factory)


		class LazyReference[T](resolve : ()=>Option[T]) extends Reference[T] {
			@volatile
			private[this] var value :Option[T] = null
			
			def toOpt = {
				if (value==null)
					value = resolve()
				value
			}
			
			override def toString = 
				if (value==null) "Lazy(?)"
				else super.toString
			
		}
		


		class LazyReferenceFactory[K, E, X, R<:Reference[X]](factory : ()=>GenericReferenceFactory[K, E, X, R])
			extends GenericReferenceFactory[K, E, X, R]
		{
			@volatile
			private[this] var resolved :GenericReferenceFactory[K, E, X, R] = null

			protected def backing :GenericReferenceFactory[K, E, X, R] =
				resolved.providing(_!=null) getOrElse { resolved = factory(); resolved }

			override def items :X ComposedOf E = backing.items

			override def delayed(key: K, value: => X): R = backing.delayed(key, value)

			override def apply(key: K, value: Option[X]): R = backing(key, value)

			override def full(value: X): R = backing.full(value)

			override def empty(key: K): R = backing.empty(key)

			override def keyFor(item: E): Option[K] = backing.keyFor(item)

			override def keyOf[F>:R<:Reference[X]](ref: F): Option[K] = backing.keyOf(ref)

			override def as[Y](implicit composition: ComposedOf[Y, E]): ReferenceFactory[K, E, Y] =
				new LazyReferenceFactory[K, E, Y, Reference[Y]](() => backing.as[Y])

			
			override def equivalencyToken: Any = backing.equivalencyToken

			
			override def equals(that :Any) = that match {
				case lazyfac :LazyReferenceFactory[_, _, _, _] => (this eq lazyfac) || this.backing==lazyfac.backing
				case fac :GenericReferenceFactory[_, _, _, _] => backing==fac
				case _ => false
			}

			override def hashCode = backing.hashCode

			override def toString = Option(resolved).map(_.toString) getOrElse "LazyFactory(?)"
		}
	}




}


