package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Kin.{Present, Unknown}
import net.noresttherein.oldsql.model.MappedKin.{KinMapper, PropertyMapper}
import net.noresttherein.oldsql.model.Restraint.{Restrainer, True}
import net.noresttherein.oldsql.morsels.PropertyPath

import scala.collection.generic.CanBuildFrom



/** A value of type T or an identification of such a value. A kin can be either `Present` -
  * contain a value or be able to compute it without significant cost and outside resources - or `Absent`,
  * in which case it should contain information required to locate and compute the given value. An exception
  * to this case is `Unknown`, which is both Empty and doesn't contain any information about the value, but should
  * be used sparingly as most operations won't be able to do anything useful with it and may result in an error.
  *
  * Kin are particularly useful to represent associations between different database entities or
  * resources which can be otherwise expensive to compute and shouldn't be fetched by default - they shield model classes
  * from the implementation details about how those associations are implemented and resolved.
  * An instance thus might contain an associated, pre-fetched entity, or a foreign key to it.
  * They can be used for any type, including collections, and an empty Kin for a to-many relationship
  * can for example specify a reverse foreign key (foreign key on the target entity and Kind key on this entity).
  * They can also serve as actual search filters - queries are after all only a filter specification on a table.
  *
  * As a kin is immutable (at least in the interface - implementations may use mutable state as long as the client
  * will never see different results for the same call on the `Kin`), it is covariant in regards to the value type -
  * a `Kin[A]` is intuitively a `Kin[B]` for `B>:A`. Their dual nature of value-or-specification-of-a-value has
  * however a hidden contravariant element to it in the latter form: in the most trivial example, a `Kin` specifying
  * 'all instances of the given type' can be interpreted differently when viewed as a `Kin` to a super type - larger
  * set of theoretically possible values. In most cases it won't matter and the set of possible values is constrained
  * externally in an explicit way, but it is something to be aware of.
  *
  * @tparam T type of the associated value.
  * @see [[net.noresttherein.oldsql.model.Kin.Present Present]]
  * @see [[net.noresttherein.oldsql.model.Kin.Absent Absent]]
  * @see [[net.noresttherein.oldsql.model.Kin.Unknown Unknown]]
  * @see [[net.noresttherein.oldsql.model.Kin.OptKin OptKin]]
  * @see [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]]
  */
abstract class Kin[+T] extends Serializable {
//	type Item
//
//	def composer :T ComposableFrom Item
//	def decompose :(_<:T) DecomposableTo Item = ???
//	def decompose :Iterable[Item]

	def isEmpty :Boolean = toOpt.isEmpty

	@inline final def nonEmpty :Boolean = isDefined

	@inline final def isDefined :Boolean = !isEmpty

	@inline final def isAbsent :Boolean = isEmpty
	
	@inline final def isPresent :Boolean = !isEmpty


	def toOpt :Option[T]

	def get :T = toOpt.get

	@inline final def getOrElse[U >: T](default: => U): U =
		if (isEmpty) default else get

	@inline final def default[U >: T](default :U) :U =
		if (isEmpty) default else get
	
	@inline final def orNull[U >: T](implicit ev: Null <:< U): U = this getOrElse ev(null)


	/** Returns the first element of the contents, in the sense and order appropriate to its type.
	  *   - For collections, this is the `this.get.head` element;
	  *   - For maps, it is a pair `k->v` made from the tuple `this.get.head` (see [[net.noresttherein.oldsql.model.-> ->]];
	  *   - For `Option`, it is equivalent to `this.get.get`;
	  *   - For non-composite types, it is simply `this.get`;
	  *  The primary purpose of this method is use as part of lambda functions used as descriptors
	  *  specifying which relationships should be fetched together with a queried entity, providing a clearer
	  *  view and a uniform access to the related entity's properties.
	  *  @throws NoSuchElementException if this kin is empty
	  */
	def fetch[E](implicit composite :T DecomposableTo E) :E = composite.first(get)

	
	def map[X](fun :KinMapper[T, X]) :Kin[X] = MappedKin(this, fun)

	def map[X](property :PropertyPath[T, X]) :Kin[X] = map(PropertyMapper(property))
	
//	@inline final def map[O](f: T => O): Kin[O] =
//		if (isEmpty) Unknown else Present(f(this.get))
//
//	@inline final def flatMap[O](f: T => Kin[O]): Kin[O] =
//		if (isEmpty) Unknown else f(this.get)
//
//	@inline final def flatten[O](implicit ev: T <:< Kin[O]): Kin[O] =
//		if (isEmpty) Unknown else ev(this.get)

	/** If the given condition is false, return an `Unknown` instance. Otherwise return `this`.
	  * Note that importing implicit conversion [[Kin$.?:]] will patch any type with the same method,
	  * creating a conditional expression producing a `Kin` instance.
	  */
	@inline final def ?:(condition :Boolean) :Kin[T] =
		if (condition && !isEmpty) this
		else Unknown

	@inline final def filter(p: T => Boolean): Kin[T] =
		if (isEmpty || p(this.get)) this else Unknown

	@inline final def filterNot(p: T => Boolean): Kin[T] =
		if (isEmpty || !p(this.get)) this else Unknown


	@inline final def withFilter(p: T => Boolean): Kin[T] =
		if (isEmpty || p(this.get)) this else Unknown



	@inline final def contains[U >: T](elem: U): Boolean =
		!isEmpty && this.get == elem

	@inline final def exists(p: T => Boolean): Boolean =
		!isEmpty && p(this.get)

	@inline final def forall(p: T => Boolean): Boolean = isEmpty || p(this.get)

	@inline final def foreach[O](f: T => O) :Unit =
		if (!isEmpty) f(this.get)


	@inline final def collect[O](pf: PartialFunction[T, O]): Kin[O] =
		if (!isEmpty) pf.lift(this.get) else Unknown

	@inline final def orElse[U >: T](alternative: => Kin[U]): Kin[U] =
		if (isEmpty) alternative else this

	/** Returns `this` if it contains a value or `alternative` otherwise. The difference from [[orElse]] is that
	  * the argument is evaluated eagerly, guaranteeing that no closure will be created and should have better performance
	  * if the alternative value was computed beforehand.
	  */
	@inline final def ifEmpty[U >: T](alternative :Kin[U]) :Kin[U] =
		if (isEmpty) alternative else this


	
	def canEqual(that :Any) :Boolean = that.isInstanceOf[Kin[_]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :Kin[_] if other canEqual this =>  toOpt == other.toOpt
		case _ => false
	}
	
	override def hashCode :Int = if (isEmpty) Unknown.hashCode else get.hashCode
	
	override def toString :String = if (isEmpty) "Absent" else "Present(" + get + ")"

}






object Kin {

	@inline final implicit def optionToKin[T](opt :Option[T]) :Kin[T] =  opt match {
		case None => Unknown
		case Some(x) => Present(x)
	}

	@inline final implicit def kinToOption[T](kin :Kin[T]) :Option[T] = kin.toOpt

	/** Implicit conversion adding a conditional factory method for `Kin` instances to any value.
	  * Importing it allows to write: `condition ?: value` to produce a `Present(value)` if `condition` is true
	  * or a blank otherwise.
	  */
	@inline final implicit def ?:[T](value :T) :KinConditional[T] =
		new KinConditional(Kin(value))

	class KinConditional[T](private val kin :Kin[T]) extends AnyVal {
		/** Returns this value as `Present(value)` if the left-hand condition evaluates to `true` or `Unknown` otherwise. */
		@inline def ?:(condition :Boolean) :Kin[T] =
			if (!condition || kin.isEmpty) Unknown
			else kin
	}


	/** Converts the option to a present or absent kin based on whether it is defined or empty. */
	@inline final def some_?[T](value :Option[T]) : Kin[T] =
		if (value.isEmpty) Unknown else Present(value.get)



	@inline final def apply[T](value :T) :Kin[T] =
		if (value == null) Unknown else Present(value)

	/** Create a `Kin[T]` in a two step process - the first step (this one) returns a `KinComposer` containing
	  * all information required to compute the target value. In a second step, a call on the returned instance
	  * will specify the expected result type for the found values (single value, `Seq`, `Set`, etc).
	  * Implicit conversion exists converting a `KinComposer[T]` to a `Kin[T]`.
	  *
	  * Example: `Kin[Hero](Equality(_.id, id)).single`.
	  *
	  * This call delegates to `AllWhere(Restraint)`.
	  *
	  * @param restraint a filter with all parameters on the universe of values for `T`.
	  * @tparam T value type on which the given filter operates.
	  * @return A factory for `Kin` of different types that can be built from a collection of `T` values.
	  */
	def apply[T](restraint :Restraint[T]) :KinComposer[T] = AllWhere(restraint)


	/** Create a `Kin` in a three step process - the first stop (this one) accepts a definition of how to look for
	  * the value(s) (for example, by a primary key) and returns a `KinFactory`. The second step specifies parameters
	  * for the first step and provides all information needed to find the value - for example, the id of the entity or
	  * its actual value, and returns a `KinComposer`. Returned `KinComposer` can be used to specify the
	  * desired (composite) type of the created `Kin` and how can it be constructed from values found by the filter
	  * created in the previous steps.
	  *
	  * Example: `Kin[Familiar](Equality(_.owner))(hero).in[Seq]`
	  *
	  * This call delegates to `AllWhere(Restrainer)`.
	  * @param restrainer a factory of `Restraint` instances for entity `T` using values of `K` as parameters.
	  * @tparam T entity type referenced by the final `Kin`.
	  * @tparam K key type - the variable element of the search filter, such as a value of a property.
	  */
	def apply[T, K](restrainer :Restrainer[T, K]) :KinFactory[K, T, T] = AllWhere(restrainer)



	implicit def singleResult[T](factory :KinComposer[T]) :Kin[T] = factory.one
	implicit def optionalResult[T](factory :KinComposer[T]) :Kin[Option[T]] = factory.optional
	implicit def collectionResult[C<:Iterable[T], T](factory :KinComposer[T])(implicit cbf :CanBuildFrom[_, T, C]) :Kin[C] =
		factory.as[C]



	/** Container for a specification of a filter on type `T`, which can request to force the view of the result set
	  * to a different type, depending on expected amount of values - it can be a single value, or just `Kin[T]`,
	  * a `Kin[Option[T]]`, or of a collection. Resolving a `Kin` for which the number of results cannot be
	  * coerced into the requested type will result in an error, rather than omitting extraneous results.
	  * @tparam T the element type returned by a query from which the values of the `Kin` created by this instance
	  *           are composed.
	  */
	trait KinComposer[T] {
		/** Return result set as a single element. */
		def one :Kin[T]

		/** Return result set as zero or one elements. Note that `Kin[Option[T]]` is conceptually different than
		  * `Option[Kin[T]]` - in the latter case the number of results is known before hand, while in the former
		  * only when resolving the Kin.
		  */
		def optional :Kin[Option[T]] = as[Option[T]]

		/** Return the result set as a composite type `C`. A composite type is a type which can be constructed
		  * and deconstructed to `Iterable[T]` (possibly with limitations to its size). The main examples for
		  * composites of `T` are: just `T`, `Option[T]`, `_&lt;:Iterable[T]`.
		  * @param expand implicit definition of how the result is mapped into a collection of T values.
		  * @tparam C Kin value type.
		  */
		def as[C](implicit expand :C ComposedOf T) :Kin[C]

		/** A shorthand for `as[C[T]]` - you can write just Kin.in[Seq] to obtain a `Kin[Seq[T]]`. */
		def in[C[X]](implicit expand :C[T] ComposedOf T) :Kin[C[T]] = as[C[T]]
	}






	/** A shorthand for a `GenericKinFactory` producing `Kin[E]` based on key type `K`. */
	type SingletonKinFactory[K, E] = GenericKinFactory[K, E, E, Kin[E]]

	/** Shorthand type for a `GenericKinFactory` returning just `Kin[X]` */
	type KinFactory[K, E, X] = GenericKinFactory[K, E, X, Kin[X]]


	/** Represents a static way of referencing a given entity `E` as type `X` by some sort of 'key' `K`.
	  * Generally, present `Kin` created by this instance will contain a value of type `X`
	  * (from which the value of `K` could be deduced) while absent `Kin` will hold the value of the key `K`.
	  * This may mean something simple like 'all entities of type `E` where a given property of value type `K`
	  * has the given value' or something more complex.
	  * Instances should implement a sensible equals method - two instances of a given factory class should be equal
	  * if and only if they return equal `Kin` from their respective factory methods for all
	  * arguments (especially `absent(key)`). Similarly, the equivalencyToken should implement a less strict equality
	  * abstracting over the composite type `X` of created `Kin`, where equivalency tokens for two factories are equal
	  * if the same value of `K` refers to the same set of entities.
	  * @tparam K 'key' type - type of value used to create absent `Kin`, for example value type of a foreign key property.
	  * @tparam E type of the underlying referenced entities.
	  * @tparam X type parameter of returned `Kin` - should satisfy `X ConsistsOf E`.
	  * @tparam R a specific subclass of `Kin` being the result type of all factory methods.
	  */
	trait GenericKinFactory[K, E, X, +R <: Kin[X]] {

		/** Composition definition for `Kin` type `X` and the target entity type `E`. */
		def result :X ComposedOf E

		/** Create a lazy Kin, which value will be evaluated lazily only when `toOpt` or `get` is called. */
		def delayed(key :K, value : =>X) :R

		/** Create an absent or present instance, depending on whether `value` is defined. Same as `apply`. */
		def create(key :K, value :Option[X]) :R = apply(key, value)

		/** Create an absent or present instance, depending on whether `value` is defined. */
		def apply(key :K, value :Option[X]) :R = value.map(present) getOrElse absent(key)

		/** Create a present `Kin` with the given value. */
		def present(value :X) :R

		/** Create a present instance based on a (assumed complete) collection of elements.
		  * Equivalent to `present(composition.attempt(_))`.
		  */
		def forItems(items :Iterable[E]) :Option[R] = result.composer.attempt(items).map(present)

		/** Create a present instance based on a (assumed complete) collection of elements and the key pointing to them.
		  * Equivalent to `apply(key, composition(items))`.
		  */
		def forItems(key :K, items :Iterable[E]) :Option[R] =
			result.composer.attempt(items).map(x => create(key, Some(x)))

		/** Create an absent instance referencing all entities of type `E` with the specific value as the key,
		  * as understood by this factory.
		  */
		def absent(key :K) :R

		/** Create an absent instance referencing al entities of type `E` with the specific value as the key,
		  * as understood by this factory. Same as `absent`.
		  */
		def apply(key :K) :R = absent(key)

		/** Try to retrieve the key out of the given target item. */
		def keyFor(item :E) :Option[K]

		/** Try to retrieve the key out of the given `Kin`. In case of a non-empty result, `absent(key)` should return
		  * a `Kin` equal to the argument.
		  */
		def keyOf[F >: R <:Kin[X]](kin :F) :Option[K]

		/** Retrieve the key out of the given `Kin` or throw an exception if not possible. Relies on the `keyOf()` implementation. */
		def forceKeyOutOf[F>:R <:Kin[X]](kin :F) :K =
			keyOf(kin) getOrElse { throw new IllegalArgumentException(s"$this: can't find key in $kin") }

		/** Tries to retrieve both the key and the value from a `Kin`. Returned `Option` is non-empty if and only if
		  * the call to `keyOf` succeeds in retrieving the key. The second value in the pair is simply `kin.toOpt`
		  */
		def unapply(kin :Kin[X]) :Option[(K, Option[X])] = keyOf(kin).map((_, kin.toOpt))

		/** Attempts to convert a given `Kin` to the specific `Kin` type `R` produced by this factory.
		  * Returned option is non-empty if the key can be retrieved from `kin`. The `Kin` itself inside the
		  * option will be absent or present based on whether `kin` is present.
		  */
		def adapt(kin :Kin[X]) :Option[R] = unapply(kin) map { case (key, value) => apply(key, value) }


//		def map[Y](mapper :KinMapper[E, Y]) :KinFactory[K, Y, Y]
//		/** Return a factory for Kins referencing a given property of X. */
//		def property[X](property :PropertyPath[X, X]) :KinFactory[K, X, X]
//
//		/** Return a factory for Kins referencing a given property of X. */
//		def property[X](property :X=>X)(implicit tag :TypeTag[X]) :KinFactory[K, X, X] =
//			this.property(PropertyPath(property))

		/** Return a factory creating `Kin` for type `Y`, where `Y` is another composite type for the target entity `E`. */
		def as[Y](implicit composition :Y ComposedOf E) :KinFactory[K, E, Y]

		/** A shorthand for `as[Y[E]]`, working on higher kinded single-param generic result type `Y`.
		  * Thus, writing `in[Seq]` will create an instance returning `Kin[Seq[E]]`.
		  */
		def in[Y[V]](implicit composition: Y[E] ComposedOf E) :KinFactory[K, E, Y[E]] = as[Y[E]]

		/** A weaker comparison than equals, which should be true if and only if absent `Kin` returned by both factories
		  * are equal for all possible key values. Conceptually this represents factories always referring to the same
		  * underlying entities, but possibly exporting them as different composite types or even `Kin` types. For example,
		  * factories returning `Kin[Seq[E]]` and `Kin[Set[E]]` should be equivalent, if they always resolve the same key
		  * to the same list of `E` valules passed as constructor arguments to a created present `Kin`.
		  */
		def equivalent(other :GenericKinFactory[K, E, _, _]) :Boolean = equivalencyToken == other.equivalencyToken

		/** A hash value which will be the same for any two equivalent instances as defined by the `equivalent()` method.
		  * Of course, the `euqals/hashCode` contract means that hash codes for those two instances should also be equal.
		  */
		def equivalencyToken :Any

		def canEqual(that :Any) :Boolean = that.isInstanceOf[GenericKinFactory[_, _, _, _]]

	}






	/** Factory and matcher for present `Kin`, i.e. those with an associated, computed value.
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Present {
		/** Create a present `Kin` containing the given value and no other information. */
		@inline def apply[T](value :T) :Kin[T] = new Present(value)

		/** Check if this `Kin` has a present value associated with it.
		  * It will always return the value of the passed reference, regardless of any other information about it
		  * and whether it was created by this object or not.
		  */
		@inline def unapply[T](kin :Kin[T]) :Option[T] = kin.toOpt

	}

	private[oldsql] class Present[+T] private[oldsql] (override val get :T) extends Kin[T] {
		override def isEmpty = false

		final override def toOpt :Some[T] = Some(get)


		override def map[X](fun :KinMapper[T, X]) :Kin[X] = Present(fun(get))

		override def map[X](property :PropertyPath[T, X]) :Kin[X] = Present(property(get))

		override def equals(that :Any) :Boolean = that match {
			case x :AnyRef if x eq this => true
			case kin :Kin[_] if kin.nonEmpty && (kin canEqual this) => get == kin.get
			case _ => false
		}

		final override def hashCode :Int = get.hashCode

	}



	/** Check if a `Kin` is absent, i.e. contains no value.
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Absent {
		/** Creates an absent instance. */
		@inline def apply[T, E] :Kin[T] { type Item = E } = Unknown.asInstanceOf[Kin[T] { type Item = E }]

		/** Checks if kin.toOpt.isEmpty */
		@inline def unapply[T](kin :Kin[T]) :Boolean = kin.isEmpty

	}

	private[oldsql] class Absent[+T] extends Kin[T] {
		override def isEmpty = true
		override def get :Nothing = throw new NoSuchElementException(this+".get")
		override def toOpt :Option[Nothing] = None

		override def equals(that :Any) :Boolean = that match {
			case kin :Kin[_] if kin canEqual this => kin.isEmpty
			case _ => false
		}

		override def hashCode :Int = Unknown.hashCode

		override def toString :String = "Absent"
	}



	/** `Kin` about which values' we don't know anything or don't care.
	  * It should be used very sparingly, as in most cases nothing sensible can be done with such an instance
	  * and passing it will produce an error, but it can be useful to mean 'not changed' in updated values,
	  * or save defining otherwise expensive to compute ''to-many'' relationships as `Option[Kin[T]]` - assuming
	  * this practice is shared by the whole codebase.
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Unknown extends Kin[Nothing] {
		def apply[X] :Kin[X] = this

		override def isEmpty = true
		override def get :Nothing = throw new NoSuchElementException("Unknown")
		override def toOpt :Option[Nothing] = None

		override def map[X](fun :KinMapper[Nothing, X]) :Kin[X] = this
		override def map[X](property :PropertyPath[Nothing, X]) :Kin[X] = this

		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def equals(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def hashCode :Int = java.lang.System.identityHashCode(this)

		override def toString = "Unknown"
	}



	/** A flattened `Kin[Option[T]]` which extends the standard `Present`/`Absent` division by adding a third state:
	  * `Naught`/`Nonexistent`. `Present` retains its meaning of containing a value, while `Absent` and `Nonexistent`
	  *  make a distinction between no such value existing (the latter) and it being simply missing from this instance.
	  *  @see [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]]
	  */
	class OptKin[+T] private[Kin] (kin :Kin[Option[T]]) extends Kin[T] {
		override def toOpt :Option[T] = kin.toOpt.flatten

		override def get :T = kin.get.get

		override def isEmpty :Boolean = kin.isEmpty || kin.get.isEmpty

		def isNaught :Boolean = kin.nonEmpty && kin.get.isEmpty

		override def toString :String =
			if (kin.isEmpty) kin.toString
			else kin.get match {
				case Some(present) => "Present(" + present + ")"
				case None => "Nonexistent"
			}
	}

	object OptKin {
		def apply[T](value :T) :OptKin[T] = new OptKin(Present(Some(value)))

		def apply[T]() :OptKin[T] = new OptKin(Unknown)

	}



	/** A factory and matcher for a special type of absent `Kin`: those which specify that the value is not merely
	  * absent in this instance, but does not exist at all.
	  */
	object Nonexistent {
		def apply[T]() :OptKin[T] = instance

		def unapply(kin :Kin[_]) :Boolean = kin match {
			case opt :OptKin[_] => opt.isNaught
			case Present(None) => true
			case _ => false
		}

		private[this] final val instance = new OptKin[Nothing](None)
	}







	/** Reference all instances of a given type in some universe, as defined by the resolver.
	  * Depending on the implementation of a handler, it can return all rows in a table, or in a collection of tables
	  * for related types.
	  */
	object All {
		/** Create a composer as the first step of Kin creation. Returned composer can be asked to produce
		  * a `Kin` exporting values of the given type as a desired composite type (for example, a collection or option).
		  * Example: `All.of[E].in[Seq]`.
		  * @see [[net.noresttherein.oldsql.model.Kin.All.apply[T] apply[T] ]]
		  */
		def of[T] :KinComposer[T] = composer.asInstanceOf[KinComposer[T]]

		/** Create an 'All' kin as `Kin[X]` - exact interpretation will depend on the client code
		  * and may not always make sense, i.e. `All[Int]()` doesn't mean 'all integers', but 'all of an integer'.
		  * Generally, `All[''Entity'']()` will mean 'there should be exactly one ''Entity'', and `All[Seq[Entity]]()`
		  * 'all entities in a sequence'. It is shorter to write than `All.of[X].one` or `All.of[X].as[Seq[X]]`.
		  */
		@inline def apply[T] :AllKinAs[T] = new AllKinAs[T] {}

		/** Create a `Kin` referencing all available values of `E`, returned as `T`. */
		def apply[T, E]()(implicit composite :T ComposedOf E) :Kin[T] =
			RestrainedKin[T, E](True)



		/** Was this Kin explicitly created as 'All' by this object? */
		def unapply[T](kin :Kin[T]) :Boolean = kin match {
			case RestrainedKin(True, _) => true
			case _ => false
		}



		trait AllKinAs[T] extends Any {
			@inline def apply[E]()(implicit composite :T ComposedOf E) :Kin[T] = All[T, E]()
		}

		private val composer = new KinComposer[Any] {
			override def one: Kin[Any] = All[Any, Any]()
			override def as[C](implicit expand: C ComposedOf Any): Kin[C] = All[C, Any]()
		}
	}



	/** Factory and inspection of `Kin` specifying all instances of a given type which satisfy a given `Restraint`. */
	object AllWhere {

		/** Initiate the first step of a chained building process for a `Kin[C[T]]` for some composite type `C`.
		  * Accepts an `Restrainer`, defining how to look for a value (for example, by a foreign key to another entity),
		  * and returns a factory, which will allow specifying actual parameters for the `Restraint`
		  * (i.e, the id of a target entity) and in what collection the values should be returned.
		  * @param restrainer Restraint factory accepting keys of type K and producing instances of `Restraint[T]`.
		  * @tparam K key type for the restrainer which will be accepted later by the returned factory
		  * @tparam T constrained type and the 'element' type of the finally constructed `Kin`.
		  * @return a factory object accepting actual parameters needed to construct the `Kin`.
		  */
		def apply[K, T](restrainer :Restrainer[T, K]) :KinFactory[K, T, T] =
			new RestrainedKinFactory[K, T, T](restrainer)

		/** Create a composer allowing to create `Kin` containing values of `T` - for example `Kin[T]`,
		  * `Kin[Option[T]]`, `Kin[Seq[T]]` and so on.
		  * @param restraint specification of how to look for the value (for example, a query filter with parameters)
		  * @tparam T filtered type
		  * @return a factory of Kins with values being various collections of T
		  */
		def apply[T](restraint :Restraint[T]) :KinComposer[T] = new RestrainedComposer(restraint)



		/** Check if the given `Kin` is a restrained `Kin` created by this instance and return the `Restraint` held by it.
		  * Even if this `Kin` was in fact created by this object, a check is performed if the composition used to
		  * create it is the same as the one passed implicitly, to assert that the element type parameter for the `Restraint`
		  * stored in this `Kin` is actually compatible with the one specified implicitly. If they are not, this method
		  * will not match. The check is performed using the compatibility comparison method for `ComposedOf`,
		  * so it is not 100% bullet proof. Note that the domain for the returned `Restrained` is a subtype of `T`
		  * rather than `T`, as due to `Kin` being covariant, it is possible that `C` is a supertype of the type
		  * provided when the instance was created.
		  *
		  * @param kin `Kin` to match.
		  * @param composition specification on what type `T` type `C` should be decomposed to and how.
		  * @return the restraint stored in this instance if it exists and is compatible with `composition`.
		  */
		def unapply[C, T](kin :Kin[C])(implicit composition :C ComposedOf T) :Option[Restraint[_<:T]] =
			kin match {
				case r :RestrainedKin[C, e] if r.as.decomposer == composition.decomposer =>
					Some(r.where)
				case _ => None
			}


		private class RestrainedComposer[T](restraint :Restraint[T]) extends KinComposer[T] {
			override def one: Kin[T] = new RestrainedKin[T, T](restraint)

			override def as[C](implicit composition: C ComposedOf T): Kin[C] =
				new RestrainedKin[C, T](restraint)
		}

	}




	/** A factory and extractor for `Kin` referencing single entities (''to-one'' relationships). */
	object Single {
		def apply[K, T](restrainer :Restrainer[T, K]) :KinFactory[K, T, T] =
			new RestrainedKinFactory[K, T, T](restrainer)

		def apply[T](restraint :Restraint[T]) :Kin[T] = AllWhere(restraint)

		def apply[T](value :T) :Kin[T] = Present[T](value)

		def unapply[T](Kin :Kin[T]) :Option[Restraint[_<:T]] = Kin match {
			case AllWhere(restraint) => Some(restraint) //this will check if Kin uses ComposedOf.itself
			case _ => None
		}

	}






	private case class RestrainedKin[T, E](where :Restraint[E], toOpt :Option[T]=None)
	                                      (implicit val as :T ComposedOf E)
		extends Kin[T]
	{
		override def isEmpty :Boolean = toOpt.isEmpty

		override def get :T = toOpt match {
			case None => throw new NoSuchElementException(this + ".get")
			case Some(item) => item
		}


		private def name :String = where match {
			case True => "All[" + as.arity + "]"
//			case Where(True, as) => "All[" + as.arity + "]"
//			case Where(_, _) => restraint.toString
			case _ if toOpt.isEmpty => "Absent{" + where + "}"
			case _ => "Present{" + where + "}"
		}

		override def toString :String = toOpt match {
			case None => name
			case Some(x) => s"$name($x)"
		}
	}


	trait HigherKindKinFactory[K, E, X, +R[T] <: Kin[T]] extends GenericKinFactory[K, E, X, R[X]] {

		override def as[Y](implicit composition: ComposedOf[Y, E]): HigherKindKinFactory[K, E, Y, R]

		override def in[Y[V]](implicit composition: ComposedOf[Y[E], E]): HigherKindKinFactory[K, E, Y[E], R] =
			as[Y[E]]
	}

//	trait BaseKinFactory[K, E, X] extends KinFactory[K, E, X] {
//		override def delayed(key: K, value: => X): Kin[X] { type Item = E } = Lazy(value)
//
//		override def present(value: X): Kin[X] { type Item = E } = Present(value)
//	}

	private class RestrainedKinFactory[K, E, X](private val Restrainer :Restrainer[E, K])(implicit val result :X ComposedOf E)
		extends HigherKindKinFactory[K, E, X, Kin]
	{

		override def delayed(key :K, value: => X): Kin[X] = Lazy(value)

		override def present(value: X): Kin[X] = Present(value)

		override def absent(key: K): Kin[X] = AllWhere(Restrainer(key)).as[X](result)

		override def keyFor(item: E): Option[K] = Restrainer.from(item)

		override def keyOf[F >: Kin[X] <: Kin[X]](ref: F): Option[K] = ref match {
			case AllWhere(Restrainer(key)) => Some(key)
			case result.Present(values) =>
				if (values.isEmpty) None
				else Restrainer.from(values.head).filter(v => values.tail.forall(v == _))
			case _ => None
		}


		override def as[Y](implicit composition: ComposedOf[Y, E]): RestrainedKinFactory[K, E, Y] =
			new RestrainedKinFactory[K, E, Y](Restrainer)(composition)

		override def in[Y[V]](implicit composition: Y[E] ComposedOf E) :RestrainedKinFactory[K, E, Y[E]] = as[Y[E]]


		def equivalencyToken :Any = Restrainer


		override def canEqual(that: Any): Boolean = that.isInstanceOf[RestrainedKinFactory[_,_,_]]

		override def equals(that :Any) :Boolean = that match {
			case c:RestrainedKinFactory[_, _, _] =>
				(this eq c) || c.canEqual(this) && c.Restrainer == Restrainer && c.result == result
			case _ => false
		}

		override def hashCode :Int = Restrainer.hashCode * 31 + result.hashCode

		override def toString = s"$result($Restrainer)"
	}






	/** Creates lazy `Kin` instances - arguments of passed methods are evaluated only when the returned instance is accessed.
	  * Passed expressions should be thread safe and idempotent, as there is no guarantee they'll be executed at most once
	  * when the returned instance is evaluated.
	  */
	object Lazy {

		/** Create a present `Kin` with a lazy value. Argument will be evaluated when `toOpt` (or `get`) method
		  * of the returned `Kin` as called.
		  * @param value a pass-by-name expression evaluating to the value of the `Kin`, which should be idempotent and thread safe.
		  */
		def apply[T](value : =>T) :Kin[T] = new LazyKin(()=>Some(value))

		/** Create a `Kin` factory delegating all calls to the specified target `Kin` factory. Expression will be evaluated
		  * when any of the returned factory methods are called. This is useful when the cycles between `Kin` would make
		  * initialization otherwise impossible.
		  * @param factory a pass-by-name expression evaluating to a factory - should be idempotent and thread safe.
		  */
		def Factory[K, T, C, R<:Kin[C]](factory : =>GenericKinFactory[K, T, C, R]) :GenericKinFactory[K, T, C, R] =
			new LazyKinFactory(() => factory)



		class LazyKin[T](resolve : ()=>Option[T]) extends Kin[T] {
//			type Item = T
//			override def composer :T ComposableFrom T = ComposableFrom.itself
//			override def decompose :Iterable[T] = toOpt.toIterable

			override def isEmpty :Boolean = toOpt.isEmpty

			@volatile
			private[this] var value :Option[T] = _

			def toOpt :Option[T] = {
				if (value == null)
					value = resolve()
				value
			}

			override def toString :String = if (value == null) "Lazy(?)" else super.toString

		}



		class LazyKinFactory[K, E, X, R<:Kin[X]](factory : ()=>GenericKinFactory[K, E, X, R])
			extends GenericKinFactory[K, E, X, R]
		{
			@volatile
			private[this] var resolved :GenericKinFactory[K, E, X, R] = _

			protected def backing :GenericKinFactory[K, E, X, R] = {
				if (resolved == null)
					resolved = factory()
				resolved
			}


			override def result :X ComposedOf E = backing.result

			override def delayed(key: K, value: => X): R = backing.delayed(key, value)

			override def apply(key: K, value: Option[X]): R = backing(key, value)

			override def present(value: X): R = backing.present(value)

			override def absent(key: K): R = backing.absent(key)

			override def keyFor(item: E): Option[K] = backing.keyFor(item)

			override def keyOf[F>:R<:Kin[X]](ref: F): Option[K] = backing.keyOf(ref)

			override def as[Y](implicit composition: ComposedOf[Y, E]): KinFactory[K, E, Y] =
				new LazyKinFactory[K, E, Y, Kin[Y]](() => backing.as[Y])


			override def equivalencyToken: Any = backing.equivalencyToken


			override def equals(that :Any) :Boolean = that match {
				case lazyfac :LazyKinFactory[_, _, _, _] => (this eq lazyfac) || this.backing == lazyfac.backing
				case fac :GenericKinFactory[_, _, _, _] => backing == fac
				case _ => false
			}

			override def hashCode :Int = backing.hashCode

			override def toString :String = Option(resolved).map(_.toString) getOrElse "LazyFactory(?)"

		}

	}




	/** Implementation delegating all calls to another Kin factory - useful as a base class for extending functionality. */
	case class KinFactoryProxy[K, E, X, +R<:Kin[X]](protected val target :GenericKinFactory[K, E, X, R])
		extends GenericKinFactory[K, E, X, R]
	{
		override def result: ComposedOf[X, E] = target.result

		override def absent(key: K): R = target.absent(key)

		override def delayed(key: K, value: => X): R = target.delayed(key, value)

		override def equivalencyToken: Any = target.equivalencyToken

		override def keyFor(item: E): Option[K] = target.keyFor(item)

		override def keyOf[F >: R <: Kin[X]](kin: F): Option[K] = target.keyOf(kin)

		override def present(value: X): R = target.present(value)

		override def as[Y](implicit composition: ComposedOf[Y, E]): KinFactory[K, E, Y] = target.as[Y]
	}


	
}






