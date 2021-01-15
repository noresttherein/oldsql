package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.exceptions.{MismatchedKeyException, MissingKeyException, NonexistentEntityException}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.RelatedEntityFactory.{OptionalKeyRelatedEntityFactory, OptionalRelatedEntityFactory, RequiredRelatedEntityFactory}






/** A factory for an option-like type `R` referencing an entity or entities of type `E` as type `X` using some sort 
  * of 'key' `K`. Provides methods for creating instances of `R` as well as extracting the keys from values of `X`.
  * For example, `K` may be a primary key of some entity `P` referenced by a foreign key in `E`, `X =:= Seq[E]` be
  * the collection of all `E` with the value of that foreign key equal to a given value of `K`, and `R` be `Kin[Seq[E]]`.
  * 
  * This is a generalization of [[net.noresttherein.oldsql.model.GenericKinFactory GenericKinFactory]], allowing
  * applications to take advantage of the functionality implemented using [[net.noresttherein.oldsql.model.Kin Kin]]
  * reference without introducing a dependence on external classes to their domain model.
  * 
  * Generally, 'full' references created by this instance will contain a value of type `X` 
  * (from which the value of `K` could be deduced), while 'empty' references can hold a value of the key `K`,
  * identifying the entities comprising the value of the full reference. This may mean something simple like 
  * 'all entities of type `E` where a given property of value type `K` has the given value' or something more complex.
  * Containing the key isn't strictly required - for example
  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.EntityOptionFactory EntityOptionFactory]] is a degenerated
  * implementation using `R =:= Option[X]` - but all functions of the API loading the referenced entities 
  * based on a previously created reference will be unavailable, and in order to update a relationship, the reference
  * must be non-empty. It is the application's responsibility to ensure that only valid reference values are passed 
  * when loading/saving values.
  * 
  * Instances should implement a sensible equals method - two instances of a given factory class should be equal
  * if and only if they return equal `Kin` from their respective factory methods for all
  * arguments (especially `absent(key)`). Similarly, `equivalencyToken` method should implement a less strict equality
  * abstracting over the composite type `X` of created `Kin`, where equivalency tokens for two factories are equal
  * if the same value of `K` refers to the same set of entities.
  * @tparam K 'key' type - type of value potentially stored in empty references, for the type of a foreign key property.
  * @tparam E type of the referenced entities.
  * @tparam X references type, contained by 'full' references - 
  *           should satisfy `X `[[net.noresttherein.oldsql.model.ComposedOf ComposedOf]]` E`.
  * @tparam R a specific reference type being the result type of all factory methods.
  * @see [[net.noresttherein.oldsql.model.GenericKinFactory]]
  */
trait RelatedEntityFactory[K, E, X, R] extends Serializable {

	/** Composition definition for the composite value type `X` and the target entity type `E`. */
	implicit def composition :X ComposableFrom E

	/** Create a lazy reference, which value will be evaluated lazily only when `toOption` or `get` is called. */
	def delay(key :K, value: => Option[X]) :R

	/** Create an empty or full reference, depending on whether `value` is defined. */
	def apply(key :K, value :Option[X]) :R //= value.map(present) getOrElse missing(key)

	/** Create a full reference with the given value. */
	def present(value :X) :R

	/** Create an empty instance referencing al entities of type `E` with the specific value as the key,
	  * as understood by this factory. Same as `absent`.
	  */
	def apply(key :K) :R = absent(key)

	/** Create an empty instance, referencing all entities of type `E` with the specific value as the key,
	  * as understood by this factory. The difference from
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.missing missing]] is that there is no guarantee that
	  * the referenced values exist at all.
	  */
	def absent(key :K) :R

	/** Create a reference with a missing value, referencing all entities of type `E` with the specific value
	  * as the key, as understood by this factory.
	  * The difference from [[net.noresttherein.oldsql.model.RelatedEntityFactory.absent absent]] is that the referenced
	  * entities are known to exist - or at least it is an error if they do not.
	  */
	def missing(key :K) :R

	/** Create an empty instance for an entity which simply does not exist. This is used in situations like
	  * `null` foreign keys. Many factories do not support this method and throw a
	  * [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]] instead.
	  */
	def nonexistent(key :K) :R = nonexistent

	/** Create an empty instance for an entity which simply does not exist. This is used in situations like
	  * `null` foreign keys. Many factories do not support this method and throw a
	  * [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]] instead.
	  */
	def nonexistent :R

	/** An instance carrying neither key nor value. Never returned from other methods, but saving an entity with
	  * such a property excludes the property from the SQL ''insert''/''update'' column list.
	  */
	def unknown :R


	/** Create a full reference containing the given (assumed complete) collection of entities.
	  * It will first check for a key common to all the items and, if found, delegate to
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.apply(key:K,value:Option[X]) apply]]`(key, value)`,
	  * where `value = composition.attempt(items)`. Otherwise, if `value` is defined, it is returned
	  * as [[net.noresttherein.oldsql.model.RelatedEntityFactory.present present]].
	  * If `value` is undefined (`items` are insufficient or otherwise invalid elements of the referenced type),
	  * this method will always return `None` - even if a common key could be extracted from the collection.
	  */
	def forItems(items :Iterable[E]) :Option[R] = keyFrom(items) match {
		case Some(key) => Some(apply(key, composition.attempt(items)))
		case _ => composition.attempt(items).map(present)
	}

	/** Create a full reference containing the given (assumed complete) collection of entities and the key pointing
	  * to them. It is a variant of `forItems(items)` (potentially) containing also the key which should be consistent
	  * with all the values. Despite being given a key as an argument, it checks all items for a key with
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyFrom keyFrom]]`(item)`. If any of the items yields
	  * a non empty result differing from `Some(key)` the method returns `None`.
	  */
	def forItems(key :K, items :Iterable[E]) :Option[R] =
		if (keyFrom(items).exists(_ != key)) None
		else composition.attempt(items).map(x => apply(key, Some(x)))


	/** Decomposes the referenced value into individual entities. */
	def decompose(value :X) :Option[Iterable[E]]

	/** Decomposes the reference into individual entities. */
	def itemsOf(ref :R) :Option[Iterable[E]] = valueOf(ref).flatMap(decompose)

	/** Try to retrieve the key out of the kin value. */
	def keyFor(value :X) :Option[K] = decompose(value).flatMap(keyFrom)

	/** Try to retrieve the key out of the given target entity. */
	def keyFrom(item :E) :Option[K]

	/** Attempt to retrieve the key shared by all of the given entities.
	  * This method relies on [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyFrom(items:E) keyFrom]]`(item)`,
	  * applying it to every entity in the collection. If the same key is returned for all of its elements,
	  * then it is returned in `Some`. Otherwise - if the collection is empty, `None` is returned for some item,
	  * or two different keys are found, `None` is returned.
	  */
	def keyFrom(items :Iterable[E]) :Option[K] =
		if (items.isEmpty) None
		else {
			val keys = items.view.map(keyFrom)
			val hd = keys.head
			if (hd.isEmpty)
				if (keys.forall(_.isEmpty)) composition.attempt(items).flatMap(x => keyOf(present(x)))
				else None
			else if (keys.forall(_ == hd)) hd
			else None
		}

	/** Try to retrieve the key out of the given reference. */
	def keyOf(ref :R) :Option[K]

	/** Retrieve the key out of the given reference or throw an exception if not possible.
	  * Relies on [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyOf keyOf]]`()` implementation.
	  * @throws MissingKeyException if the given argument does not contain a key.
	  */ //consider: a standard java base exception class for MissingKeyException
	def forceKeyOutOf(ref :R) :K =
		keyOf(ref) getOrElse { throw new MissingKeyException(s"No $this key in $ref.") }

	/** Attempts to retrieve the referenced value from the argument */
	def valueOf(ref :R) :Option[X]

	/** Tries to retrieve a key from the argument reference.
	  * @return `keyOf(ref)`
	  */
	def unapply(ref :R) :Option[K] = keyOf(ref)

	/** An adapter to this factory handling relationships which might be missing, such as with nullable foreign keys.
	  * Relies on this factory's [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]]
	  * method for the value associated with `None` key value. If this class throws an exception from the method,
	  * consider the overloaded variant accepting a `nonexistent` value or
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.optional optional]] method which lifts both the key `K`
	  * and reference `R` types to options.
	  */
	def optionalKeys :RelatedEntityFactory[Option[K], E, X, R] = new OptionalKeyRelatedEntityFactory(this)

	/** An adapter to this factory handling relationships which might be missing, such as with nullable foreign keys.
	  * @param nonexistent the reference value returned for `None` keys.
	  */
	def optionalKeys(nonexistent: R) :RelatedEntityFactory[Option[K], E, X, R] =
		new OptionalKeyRelatedEntityFactory(this, nonexistent)

	/** An adapter to this factory handling relationships which might be missing, such as nullable foreign keys.
	  * Maps `None` keys to a `None` reference value used as
	  * the [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] value.
	  */
	def optional :RelatedEntityFactory[Option[K], E, X, Option[R]] = new OptionalRelatedEntityFactory(this)

	/** A proxy to this factory which
	  * throws a [[net.noresttherein.oldsql.exceptions.MissingKeyException MissingKeyException]]
	  * or a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]]
	  * from its [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] methods, essentially
	  * enforcing non-null semantics on (foreign) keys. All absent references produced by the factory will contain a key
	  * and will be [[net.noresttherein.oldsql.model.Kin.isMissing missing]].
	  */
	def required :RelatedEntityFactory[K, E, X, R] =
		if (isRequired) this else new RequiredRelatedEntityFactory(this)

	/** If true, every reference produced and accepted by this factory must carry either a key, or a value or both.
	  * This implies that there is no [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]]
	  * reference and that method will always
	  * throw a [[net.noresttherein.oldsql.exceptions.MissingKeyException MissingKeyException]]
	  * or a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.required]]
	  */
	def isRequired :Boolean

	/** A weaker comparison than equals, which should be true if and only if absent references returned by both
	  * factories have equal keys. Conceptually this represents factories always referring
	  * to the same set of logical entities, but possibly exporting them as different composite types
	  * or even reference types. For example, factories returning `Kin[Seq[E]]` and `Kin[Set[E]]` should be equivalent,
	  * if they always resolve the same key to the same list of `E` values passed as constructor arguments
	  * to a created full reference.
	  */
	def equivalent(other :RelatedEntityFactory[K, E, _, _]) :Boolean = equivalencyToken == other.equivalencyToken

	/** A hash value which will be the same for any two equivalent instances as defined by the `equivalent()` method. */
	def equivalencyToken :Any


	def canEqual(that :Any) :Boolean = that.isInstanceOf[RelatedEntityFactory[_, _, _, _]]
}






object RelatedEntityFactory {

	/** Represents entity relationship with `E` at the end as `Option[X]`. The referenced value is always assumed
	  * to exist and the option is used only to handle the case when the entity is not loaded. Similarly,
	  * passing `None` with a saved entity is understood as 'property unchanged', excluding mapped columns from
	  * the SQL ''insert'' or ''update''.
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.EntityOptionOptionFactory]]
	  */
	class EntityOptionFactory[K, E, X](key :E => Option[K], override val equivalencyToken :AnyRef)
	                                  (implicit result :X ComposedOf E)
		extends RelatedEntityFactory[K, E, X, Option[X]]
	{
		implicit override def composition :X ComposableFrom E = result.composer

		override def delay(key :K, value : => Option[X]) :Option[X] = None
		override def apply(key :K, value :Option[X]) :Option[X] = value

		override def present(value :X) :Option[X] = Some(value)
		override def absent(key :K) :Option[X] = None
		override def missing(key :K) :Option[X] = None
		override def nonexistent(key :K) :Option[X] =
			throw new NonexistentEntityException(s"No entity for $this key $key.")
		override def nonexistent :Option[X] = throw new NonexistentEntityException(toString + ".nonexistent")
		override def unknown :Option[X] = None

		override def decompose(value :X) :Option[Iterable[E]] = Some(result.decomposer(value))
		override def itemsOf(ref :Option[X]) :Option[Iterable[E]] = ref.map(result.decomposer.apply)


		override def keyFor(value :X) :Option[K] = None
		override def keyFrom(item :E) :Option[K] = None

		override def keyOf(ref :Option[X]) :Option[K] = ref.flatMap { x =>
			val keys = result.decomposer(x).view.map(key)
			val res = keys.headOption.flatten
			if (!keys.forall(_ == res))
				throw new MismatchedKeyException(
					keys.toSet.mkString("Keys of entities in the collection differ: ", ", ", ".")
				)
			res
		}

		override def valueOf(ref :Option[X]) :Option[X] = ref

		override def isRequired = true
	}



	/** Represents optional entity relationship with `E` at the end as `Option[Option[E]]`. The outer option
	  * specifies if the relationship was loaded: `Some(None)` means that relationship state is unknown and,
	  * when an entity with such a property is updated, the relationship state will remain unchanged.
	  * On the other hand, `None` means that the relationship is known to not exist
	  * (such as with a `null` foreign key).
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.EntityOptionFactory]]
	  */
	class EntityOptionOptionFactory[K, E, X](key :E => Option[K], override val equivalencyToken :AnyRef)
	                                        (implicit result :X ComposedOf E)
		extends RelatedEntityFactory[K, E, X, Option[Option[X]]]
	{
		implicit override def composition :X ComposableFrom E = result.composer

		override def delay(key :K, value : => Option[X]) :Option[Option[X]] = None
		override def apply(key :K, value :Option[X]) :Option[Option[X]] = value map present getOrElse absent(key)

		override def present(value :X) :Option[Option[X]] = Some(Some(value))
		override def absent(key :K) :Option[Option[X]] = unknown
		override def missing(key :K) :Option[Option[X]] = unknown
		override val unknown :Option[Option[X]] = Some(None)
		override def nonexistent :Option[Option[X]] = None

		override def decompose(value :X) :Option[Iterable[E]] = Some(result.decomposer(value))
		override def itemsOf(ref :Option[Option[X]]) :Option[Iterable[E]] = ref.flatten.map(result.decomposer.apply)

		override def keyFor(value :X) :Option[K] = None
		override def keyFrom(item :E) :Option[K] = None

		override def keyOf(ref :Option[Option[X]]) :Option[K] = ref.flatten.flatMap { x =>
			val keys = result.decomposer(x).view.map(key)
			val res = keys.headOption.flatten
			if (!keys.forall(_ == res))
				throw new MismatchedKeyException(
					keys.toSet.mkString("Keys of entities in the collection differ: ", ", ", ".")
				)
			res
		}

		override def valueOf(ref :Option[Option[X]]) :Option[X] = ref.flatten

		override def isRequired :Boolean = false
	}




	private[oldsql] case class OptionalKeyRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R],
	                                                                       override val nonexistent :R)
		extends RelatedEntityFactory[Option[K], E, X, R]
	{
		def this(backing :RelatedEntityFactory[K, E, X, R]) = this(backing, backing.nonexistent)

		override def composition :X ComposableFrom E = backing.composition

		override def delay(key :Option[K], value : => Option[X]) :R = key match {
			case Some(k) => backing.delay(k, value)
			case _ => nonexistent
		}

		override def apply(key :Option[K], value :Option[X]) :R = key match {
			case Some(k) => backing(k, value)
			case _ => value match {
				case Some(v) => backing.present(v)
				case _ => nonexistent
			}
		}

		override def present(value :X) :R = backing.present(value)

		override def missing(key :Option[K]) :R = key match {
			case Some(k) => backing.missing(k)
			case _ => nonexistent
		}

		override def absent(key :Option[K]) :R = key match {
			case Some(k) => backing.absent(k)
			case _ => unknown
		}

		override val unknown :R = backing.unknown

		override def decompose(value :X) :Option[Iterable[E]] = backing.decompose(value)
		override def itemsOf(ref :R) :Option[Iterable[E]] = backing.itemsOf(ref)

		override def keyFor(value :X) :Option[Option[K]] = backing.keyFor(value).map(Some.apply)
		override def keyFrom(items :Iterable[E]) :Option[Option[K]] = backing.keyFrom(items).map(Some.apply)
		override def keyFrom(item :E) :Option[Option[K]] = Some(backing.keyFrom(item))
		override def keyOf(ref :R) :Option[Option[K]] = Some(backing.keyOf(ref))
		override def valueOf(ref :R) :Option[X] = backing.valueOf(ref)

		override def isRequired = false

		override def equivalencyToken :Any =
			new EquivalencyTokenWrapper(backing.equivalencyToken) {
				override def toString = token.toString + ".optionalKeys"
			}

		override def toString = "Option[" + backing.toString + "]"
	}




	private[oldsql] case class OptionalRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R])
		extends RelatedEntityFactory[Option[K], E, X, Option[R]]
	{ //perhaps this could be done for any higher type with polymorphic functions as wrappers/unwrappers?
		override def composition = backing.composition

		override def delay(key :Option[K], value : => Option[X]) = key match {
			case Some(k) => Some(backing.delay(k, value))
			case _ => nonexistent
		}

		override def apply(key :Option[K], value :Option[X]) :Option[R] = key match {
			case Some(k) => Some(backing(k, value))
			case _ => value match {
				case Some(v) => Some(backing.present(v))
				case _ => nonexistent
			}
		}

		override def present(value :X) = Some(backing.present(value))

		override def missing(key :Option[K]) = key match {
			case Some(k) => Some(backing.missing(k))
			case _ => nonexistent
		}

		override def absent(key :Option[K]) = key match {
			case Some(k) => Some(backing.absent(k))
			case _ => None
		}

		override def nonexistent = None
		override val unknown = Some(backing.unknown)

		override def decompose(value :X) :Option[Iterable[E]] = backing.decompose(value)
		override def itemsOf(ref :Option[R]) :Option[Iterable[E]] = ref.flatMap(backing.itemsOf)

		override def keyFrom(item :E) = backing.keyFrom(item) match {
			case None => None
			case some => Some(some)
		}

		override def keyOf(ref :Option[R]) = ref match {
			case Some(r) => backing.keyOf(r) match {
				case None => None
				case some => Some(some)
			}
			case _ => None
		}

		override def valueOf(ref :Option[R]) = ref match {
			case Some(r) => backing.valueOf(r)
			case _ => None
		}

		override def isRequired :Boolean = false

		override def equivalencyToken =
			new EquivalencyTokenWrapper(backing.equivalencyToken) {
				override def toString = token.toString + ".optional"
			}

		override def toString = backing.toString + ".optional"
	}




	private[oldsql] case class RequiredRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R])
		extends RelatedEntityFactory[K, E, X, R]
	{
		implicit override def composition = backing.composition

		override def delay(key :K, value : => Option[X]) :R = backing.delay(key, value)
		override def apply(key :K, value :Option[X]) :R = backing(key, value)
		override def present(value :X) :R = backing.present(value)
		override def missing(key :K) :R = backing.missing(key)
		override def absent(key :K) :R = backing.absent(key)
		override def nonexistent(key :K) = throw new NonexistentEntityException(s"No entity for $this key $key.")
		override def nonexistent = throw new NonexistentEntityException(toString + ": no key found")
		override def unknown :R = backing.unknown

		override def decompose(value :X) :Option[Iterable[E]] = backing.decompose(value)
		override def itemsOf(ref :R) :Option[Iterable[E]] = backing.itemsOf(ref)

		override def keyFor(value :X) :Option[K] = backing.keyFor(value)
		override def keyFrom(items :Iterable[E]) :Option[K] = backing.keyFrom(items)
		override def keyFrom(item :E) :Option[K] = backing.keyFrom(item)
		override def keyOf(ref :R) :Option[K] = backing.keyOf(ref)
		override def valueOf(ref :R) :Option[X] = backing.valueOf(ref)

		override def required :RelatedEntityFactory[K, E, X, R] = this

		override def isRequired :Boolean = true

		override def equivalencyToken :Any = backing.equivalencyToken

		override def toString = backing.toString + ".required"
	}






	class EquivalencyTokenWrapper(val token :Any) extends Serializable {
		override def equals(that :Any) :Boolean = that match {
			case wrap :EquivalencyTokenWrapper => (this eq wrap) || (getClass == wrap.getClass) && token == wrap.token
			case _ => false
		}
		override def hashCode :Int = getClass.hashCode * 31 + token.hashCode
	}

}
