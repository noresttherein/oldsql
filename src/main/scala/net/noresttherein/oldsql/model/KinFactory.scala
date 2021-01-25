package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{MissingKeyException, NonexistentEntityException}
import net.noresttherein.oldsql.model.ComposedOf.{ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Kin.{Derived, Nonexistent, Present}
import net.noresttherein.oldsql.model.KinFactory.RequiredKinFactoryDecorator
import net.noresttherein.oldsql.morsels.Lazy






/** A factory of references `R <: Kin[X]` to an entity or entities of type `E`, collected as type `X`,
  * based on some sort of 'key' `K`.
  * Generally, [[net.noresttherein.oldsql.model.Kin.Present present]] [[net.noresttherein.oldsql.model.Kin Kin]]
  * created by this instance will contain a value of type `X` (from which the value of `K` could be deduced),
  * while [[net.noresttherein.oldsql.model.Kin.Absent absent]] `Kin` will hold the value of the key `K`.
  * This may mean something simple like 'all entities of type `E` where a given property of value type `K`
  * has the given value' or something more complex.
  *
  * Instances should implement a sensible equals method - two instances of a given factory class should be equal
  * if and only if they return equal `Kin` from their respective factory methods for all
  * arguments (especially `absent(key)`). Similarly, `equivalencyToken` method should implement a less strict
  * equality, abstracting over the composite type `X` of created `Kin`, where equivalency tokens for two factories
  * are equal if the same value of `K` refers to the same set of entities.
  * @tparam K 'key' type - type of value used to create absent `Kin`, for example a value type
  *           of a foreign key property.
  * @tparam E type of the underlying referenced entities.
  * @tparam X type parameter of returned `Kin` - should satisfy
  *           `X `[[net.noresttherein.oldsql.model.ComposedOf ComposedOf]]` E`.
  * @tparam R a specific subclass of `Kin` being the result type of all factory methods.
  * @see [[net.noresttherein.oldsql.model.Kin]]
  * @author Marcin Mościcki
  */
trait GenericKinFactory[K, E, X, +R <: Kin[X]] extends RelatedEntityFactory[K, E, X, Kin[X]] {

	/** Casts this to its [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]] supertype,
	  * for the same kin/reference type `R` as this instance (rather than standard `Kin[X]`).
	  */
	def narrow[U >: R <: Kin[X]] :RelatedEntityFactory[K, E, X, U] =
		this.asInstanceOf[RelatedEntityFactory[K, E, X, U]]

	/** Composition definition for `Kin` type `X` and the target entity type `E`. */
	implicit override def composition :X ComposableFrom E

	/** Create a lazy `Kin`, which value will be evaluated lazily only when `toOption` or `get` is called. */
	override def delay(key :K, value: => Option[X]) :R

	/** Create an absent (missing) or present instance, depending on whether `value` is defined. */
	override def apply(key :K, value :Option[X]) :R = value.map(present) getOrElse missing(key)

	/** Create a present `Kin` with the given value. */
	override def present(value :X) :R

	/** Create a present `Kin` with the given key and value. */
	override def present(key :K, value :X) :R = apply(key, Some(value))

	/** Create an absent instance referencing al entities of type `E` with the specific value as the key,
	  * as understood by this factory. Same as `absent`.
	  */
	override def apply(key :K) :R = missing(key)

	/** Create an absent instance, referencing all entities of type `E` with the specific value as the key,
	  * as understood by this factory. Same as `missing`
	  */
	override def absent(key :K) :R

	/** Create an absent instance with a missing value, referencing all entities of type `E` with the specific value
	  * as the key, as understood by this factory.
	  */
	override def missing(key :K) :R

	/** Create an absent kin for an entity which does not exist. This is used in situations like
	  * `null` foreign keys or data constraint failures. Many factories do not support this method and throw a
	  * [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]] instead.
	  */
	override def nonexistent(key :K) :R = nonexistent

	/** Create an absent kin for an entity which does not exist. This is used in situations like
	  * `null` foreign keys or data constraint failures. Many factories do not support this method and throw a
	  * [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]] instead.
	  */
	override def nonexistent :R

	/** An instance carrying neither key nor value. Never returned from other methods, but saving an entity with
	  * such a property excludes the property from the SQL ''insert''/''update'' column list.
	  * Note that the type is `Kin[X]` rather than the specific kin type `R`. This is to allow `Kin.Unknown`
	  * to be used by all factories and is of little consequence, as this value should never be returned from a mapping
	  * to the application, and is used only to compare with given arguments when writing a value of `R` to the database.
	  * Subclasses can still override it to some other value if they so wish, though.
	  * @return `Kin.`[[net.noresttherein.oldsql.model.Kin.Unknown Unknown]].
	  */
	override def unknown :Kin[X] = Kin.Unknown

	/** Create a present instance based on an (assumed complete) collection of entities.
	  * It will first check for a key common to all the items and, if found, delegate to
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.apply(key:K,value:Option[X]) apply]]`(key, value)`,
	  * where `value = composition.attempt(items)`. Otherwise, if `value` is defined, it is returned
	  * as [[net.noresttherein.oldsql.model.GenericKinFactory.present present]].
	  * If `value` is undefined (`items` are insufficient or otherwise invalid elements of the referenced type),
	  * this method will always return `None` - even if a common key could be extracted from the collection.
	  */
	override def forItems(items :Iterable[E]) :Opt[R] = composition.attempt(items) match {
		case opt @ Got(value) => keyFrom(items).map(apply(_, opt.toOption)) orElse Got(present(value))
		case _ => Lack
	}

	/** Create a present instance based on an (assumed complete) collection of entities and the key pointing
	  * to them. It is a variant of `forItems(items)` (potentially) containing also the key which should be consistent
	  * with all the values. Despite being given a key as an argument, it checks all items for a key with
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyFrom keyFrom]]`(item)`. If any of the items yields
	  * a non empty result differing from `Some(key)` the method returns `None`.
	  */
	override def forItems(key :K, items :Iterable[E]) :Opt[R] =
		if (keyFrom(items).exists(_ != key)) Lack
		else composition.attempt(items).map(x => present(key, x))

	/** Decomposes the given `Kin` into individual entities. */
	override def itemsOf(kin :Kin[X]) :Opt[Iterable[E]] = valueOf(kin).flatMap(decompose)

//	override def keyFor(value :X) :Option[K]

	/** Try to retrieve the key out of the given target item. */
	override def keyFrom(item :E) :Opt[K]

	/** Try to retrieve the key out of the given `Kin`. In case of a non-empty result, `absent(key)` should return
	  * a `Kin` equal to the argument.
	  */
	def keyOf(kin :Kin[X]) :Opt[K]

	/** Retrieve the key out of the given `Kin` or throw a `MissingKeyException` if not possible.
	  * Relies on the `keyOf()` implementation.
	  */
	override def forceKeyOutOf(kin :Kin[X]) :K =
		keyOf(kin) getOrElse { throw new MissingKeyException(s"No $this key in $kin.") }

	/** Returns `kin.toOption`. */
	override def valueOf(kin :Kin[X]) :Opt[X] = kin.opt

	/** Tries to retrieve both the key and the value from a `Kin`.
	  * @return `keyOf(kin)`.
	  */
	override def unapply(kin :Kin[X]) :Opt[K] = keyOf(kin)

	/** A proxy to this factory which
	  * throws a [[net.noresttherein.oldsql.exceptions.MissingKeyException MissingKeyException]]
	  * or a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]]
	  * from its [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] methods, essentially
	  * enforcing non-null semantics on (foreign) keys. All absent references produced by the factory will contain a key
	  * and be '[[net.noresttherein.oldsql.model.Kin.isMissing missing]]'.
	  */
	override def required :GenericKinFactory[K, E, X, R] =
		if (isRequired) this else new RequiredKinFactoryDecorator(this)

	//new methods
	/** Attempts to convert a given `Kin` to the specific `Kin` type `R` produced by this factory.
	  * Returned option is non-empty if the key can be retrieved from `kin`. The `Kin` itself inside the
	  * option will be absent or present based on whether `kin` is present.
	  */
	def adapt(kin :Kin[X]) :Opt[R] =
		unapply(kin) map { key => apply(key, kin.toOption) } orElse kin.opt.map(present)


	/** Return a factory creating `Kin` for type `Y`, where `Y` is another composite type for the target entity `E`. */
	def as[Y](implicit composition :Y ComposedOf E) :KinFactory[K, E, Y]

	/** A shorthand for `as[Y[E]]`, working on higher kinded single-param generic result type `Y`.
	  * Thus, writing `in[Seq]` will create an instance returning `Kin[Seq[E]]`.
	  */
	def in[Y[V]](implicit composition: Y[E] ComposedOf E) :KinFactory[K, E, Y[E]] = as[Y[E]].required


	override def canEqual(that :Any) :Boolean = that.isInstanceOf[GenericKinFactory[_, _, _, _]]
}






/**
  * @author Marcin Mościcki
  */
object KinFactory {

	/** Create a `Kin` factory delegating all calls to the specified target `Kin` factory. Expression will be evaluated
	  * when any of the returned factory methods are called. This is useful when the cycles between `Kin` would make
	  * initialization otherwise impossible.
	  * @param factory a pass-by-name expression evaluating to a factory - should be idempotent and thread safe.
	  */
	def delay[K, E, T, R <: Kin[T]](factory: => GenericKinFactory[K, E, T, R]) :GenericKinFactory[K, E, T, R] =
		new LazyKinFactory[K, E, T, R](() => factory)

	def delay[K, E, T, R[X] <: Kin[X]](factory :() => HigherKindKinFactory[K, E, T, R]) :HigherKindKinFactory[K, E, T, R] =
		new LazyKinFactory[K, E, T, R[T]](factory) with HigherKindKinFactory[K, E, T, R] {
			protected override def target =
				super.target.asInstanceOf[HigherKindKinFactory[K, E, T, R]]

			override def required =
				if (isEvaluated) target.required else KinFactory.delay(() => factory().required)

			override def as[Y](implicit composition :ComposedOf[Y, E]) =
				if (isEvaluated) target.as[Y] else KinFactory.delay(() => factory().as[Y])
		}


	/** A shorthand for a `GenericKinFactory` producing `Kin[E]` based on key type `K`. */
	type SingletonKinFactory[K, E] = GenericKinFactory[K, E, E, Kin[E]]

	type DerivedKinFactory[K, E, T] = HigherKindKinFactory[K, E, T, Derived.Of[E]#Kin]


	trait HigherKindKinFactory[K, E, X, +R[T] <: Kin[T]] extends GenericKinFactory[K, E, X, R[X]] {
		override def required :HigherKindKinFactory[K, E, X, R] =
			if (isRequired) this else new HigherKindRequiredKinFactory(this)

		override def as[Y](implicit composition :Y ComposedOf E): HigherKindKinFactory[K, E, Y, R]
		override def in[Y[V]](implicit composition :Y[E] ComposedOf E): HigherKindKinFactory[K, E, Y[E], R] =
			as[Y[E]].required
	}


	trait BaseKinFactory[K, E, X] extends GenericKinFactory[K, E, X, Kin[X]] {
		implicit def result :X ComposedOf E
		implicit override def composition :X ComposableFrom E = result.composer
		implicit protected def decomposition :X DecomposableTo E = result.decomposer

		override def present(value :X) :Kin[X] = Present(value)
		override def nonexistent :Kin[X] = Nonexistent()

		override def decompose(value :X) :Opt[Iterable[E]] = Got(result.decomposer(value))
		override def itemsOf(kin :Kin[X]) :Opt[Iterable[E]] =
			if (kin.isPresent) Got(result.decomposer(kin.get)) else Lack

		override def isRequired = false
	}

	trait BaseDerivedKinFactory[K, E, X]
		extends BaseKinFactory[K, E, X] with HigherKindKinFactory[K, E, X, Derived.Of[E]#Kin]
		   with RequiredKinFactory[K, E, X, Derived[E, X]]
	{
		override def apply(key :K, value :Option[X]) :Derived[E, X] = delay(key, value)

		override def present(value :X) :Derived[E, X] = Derived.present(value)
	}



	trait RequiredKinFactory[K, E, X, +R <: Kin[X]] extends GenericKinFactory[K, E, X, R] {
		override def absent(key :K) :R = missing(key)

		override def nonexistent :Nothing =
			throw new NonexistentEntityException("No " + this +" key value found.")

		override def nonexistent(key :K) :Nothing =
			throw new NonexistentEntityException(s"No value for $this key $key.")

		override def required :this.type = this
		override def isRequired :Boolean = true
	}



	/** Implementation delegating all calls to another Kin factory - useful as a base class for extending functionality. */
	private[oldsql] trait KinFactoryProxy[K, E, X, +R <: Kin[X]] extends GenericKinFactory[K, E, X, R] {
		protected def target :GenericKinFactory[K, E, X, R]

		override def composition = target.composition

		override def delay(key: K, value: => Option[X]): R = target.delay(key, value)
		override def apply(key :K, value :Option[X]) :R = target(key, value)

		override def present(value: X): R = target.present(value)
		override def present(key :K, value :X) :R = target.present(key, value)
		override def absent(key :K) :R = target.absent(key)
		override def missing(key: K): R = target.missing(key)
		override def nonexistent(key :K) :R = target.nonexistent(key)
		override def nonexistent :R = target.nonexistent
		override def unknown :Kin[X] = target.unknown

		override def decompose(value :X) :Opt[Iterable[E]] = target.decompose(value)
		override def itemsOf(kin :Kin[X]) :Opt[Iterable[E]] = target.itemsOf(kin)

		override def keyFor(value :X) :Opt[K] = target.keyFor(value)
		override def keyFrom(item: E): Opt[K] = target.keyFrom(item)
		override def keyFrom(items :Iterable[E]) :Opt[K] = target.keyFrom(items)
		override def keyOf(kin: Kin[X]): Opt[K] = target.keyOf(kin)
		override def forceKeyOutOf(kin :Kin[X]) :K = target.forceKeyOutOf(kin)
		override def unapply(kin :Kin[X]) :Opt[K] = target.unapply(kin)

		override def optionalKeys(nonexistent :Kin[X]) :RelatedEntityFactory[Option[K], E, X, Kin[X]] =
			target.optionalKeys(nonexistent)
		override def optionalKeys :RelatedEntityFactory[Option[K], E, X, Kin[X]] = target.optionalKeys
		override def optional :RelatedEntityFactory[Option[K], E, X, Option[Kin[X]]] = target.optional
		override def required :GenericKinFactory[K, E, X, R] = target.required
		override def isRequired :Boolean = target.isRequired

		override def as[Y](implicit composition: ComposedOf[Y, E]): KinFactory[K, E, Y] = target.as[Y]


		override def equivalencyToken: Any = target.equivalencyToken

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :KinFactoryProxy[_, _, _, _] if other.getClass == getClass => other.target == target
			case _ => false
		}

		override def hashCode :Int = target.hashCode
	}



	private[oldsql] class RequiredKinFactoryDecorator[K, E, X, +R <: Kin[X]]
	                                                 (protected override val target :GenericKinFactory[K, E, X, R])
		extends KinFactoryProxy[K, E, X, R] with RequiredKinFactory[K, E, X, R]

	private[oldsql] class HigherKindRequiredKinFactory[K, E, X, +R[T] <: Kin[T]]
	                                                  (protected override val target :HigherKindKinFactory[K, E, X, R])
		extends KinFactoryProxy[K, E, X, R[X]] with HigherKindKinFactory[K, E, X, R]
		   with RequiredKinFactory[K, E, X, R[X]]
	{
		override def as[Y](implicit composition :Y ComposedOf E) :HigherKindKinFactory[K, E, Y, R] = target.as[Y]
	}



	private class LazyKinFactory[K, E, X, +R <: Kin[X]](factory : () => GenericKinFactory[K, E, X, R])
		extends KinFactoryProxy[K, E, X, R]
	{
		private[this] val lazyBacking = Lazy(factory())
		protected def isEvaluated = lazyBacking.isInitialized

		protected override def target :GenericKinFactory[K, E, X, R] = lazyBacking

		override def required :GenericKinFactory[K, E, X, R] =
			if (lazyBacking.isInitialized) target.required else new LazyKinFactory(() => target.required)

		override def as[Y](implicit composition :Y ComposedOf E): KinFactory[K, E, Y] =
			if (lazyBacking.isInitialized) target.as[Y]
			else new LazyKinFactory[K, E, Y, Kin[Y]](() => target.as[Y])

		override def equivalencyToken: Any = target.equivalencyToken


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[LazyKinFactory[_, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case lazyfac :LazyKinFactory[_, _, _, _] if lazyfac canEqual this => this.target == lazyfac.target
			case fac :GenericKinFactory[_, _, _, _] => target == fac
			case _ => false
		}

		override def hashCode :Int = target.hashCode

		override def toString :String =
			if (lazyBacking.isInitialized) lazyBacking.get.toString else "LazyKinFactory(?)"
	}
}
