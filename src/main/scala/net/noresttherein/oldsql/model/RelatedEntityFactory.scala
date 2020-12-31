package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.exceptions.MissingKeyException
import net.noresttherein.oldsql.model.RelatedEntityFactory.{OptionalKeyRelatedEntityFactory, OptionalRelatedEntityFactory, RequiredRelatedEntityFactory}






/** A factory for an option-like type `R` referencing an entity or entities of type `E` as type `X` using some sort 
  * of 'key' `K`. Provides methods for creating instances of `R` as well as extracting the keys from values of `X`.
  * For example, `K` may be a primary key of some entity `P` referenced by a foreign key in `E`, `X =:= Seq[E]` be
  * the collection of all `E` with the value of that foreign key equal to a given value of `K`, and `R` be `Kin[Seq[E]]`.
  * 
  * This is a generalization of [[net.noresttherein.oldsql.model.Kin.GenericKinFactory GenericKinFactory]], allowing
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
  * @see [[net.noresttherein.oldsql.model.Kin.GenericKinFactory]]           
  */
trait RelatedEntityFactory[K, E, X, R] {

	/** Composition definition for the composite value type `X` and the target entity type `E`. */
	def result :X ComposedOf E

	/** Create a lazy reference, which value will be evaluated lazily only when `toOption` or `get` is called. */
	def delayed(key :K, value: => Option[X]) :R

	/** Create an empty or full reference, depending on whether `value` is defined. Same as `apply`. */
	def create(key :K, value :Option[X]) :R = apply(key, value)

	/** Create an empty or full reference, depending on whether `value` is defined. */
	def apply(key :K, value :Option[X]) :R = value.map(present) getOrElse missing(key)

	/** Create a full reference with the given value. */
	def present(value :X) :R

	/** Create a reference with a missing value, referencing all entities of type `E` with the specific value
	  * as the key, as understood by this factory.
	  * The difference from [[net.noresttherein.oldsql.model.RelatedEntityFactory.absent absent]] is that the referenced
	  * entities are known to exist - or at least it is an error if they do not.
	  */
	def missing(key :K) :R

	/** Create an empty instance for an entity which simply does not exist. This is used in situations like
	  * `null` foreign keys.
	  */
	def nonexistent :R

	/** Create an empty instance, referencing all entities of type `E` with the specific value as the key,
	  * as understood by this factory. The difference from
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.missing missing]] is that there is no guarantee that
	  * the referenced values exist at all.
	  * @return `missing(key)` unless overriden.
	  */
	def absent(key :K) :R = missing(key)

	/** Create an empty instance referencing al entities of type `E` with the specific value as the key,
	  * as understood by this factory. Same as `absent`.
	  */
	def apply(key :K) :R = absent(key)

	/** Create a full reference containing the given (assumed complete) collection of entities.
	  * @return `result.composer.attempt(items).map(present)`.
	  */
	def forItems(items :Iterable[E]) :Option[R] = result.composer.attempt(items).map(present)

	/** Create a full reference containing the given (assumed complete) collection of entities and the key pointing
	  * to them. It is a variant of `forItems(items)` (potentially) containing also the key
	  * @return `result.composer.attempt(items).map(x => create(key, Some(x)))`.
	  */
	def forItems(key :K, items :Iterable[E]) :Option[R] =
		result.composer.attempt(items).map(x => create(key, Some(x)))


	/** Try to retrieve the key out of the given target entity. */
	def keyFor(item :E) :Option[K]

	/** Try to retrieve the key out of the given reference. */
	def keyOf(ref :R) :Option[K]

	/** Retrieve the key out of the given reference or throw an exception if not possible.
	  * Relies on [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyOf keyOf]]`()` implementation.
	  * @throws IllegalArgumentException if the given argument does not contain a key.
	  */ //consider: a dedicated exception. Would be easier to catch it when loading and fill with information about the owning entity.
	def forceKeyOutOf(ref :R) :K =
		keyOf(ref) getOrElse { throw new MissingKeyException(s"$this: no key in $ref.") }

	/** Attempts to retrieve the referenced value from the argument */
	def valueOf(ref :R) :Option[X]

	/** Tries to retrieve both a key and a value from the argument reference. Returned `Option` is non-empty
	  * if and only if the call to [[net.noresttherein.oldsql.model.RelatedEntityFactory.keyOf keyOf]] succeeds
	  * in retrieving the key. The element of the pair is simply
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.valueOf valueOf]]`(ref)`.
	  */
	def unapply(ref :R) :Option[(K, Option[X])] = keyOf(ref).map(_ -> valueOf(ref))

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
	  * from its [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]] method, essentially
	  * enforcing non-null semantics on (foreign) keys. All absent references produced by the factory will contain a key.
	  */
	def required :RelatedEntityFactory[K, E, X, R] =
		if (isRequired) this else new RequiredRelatedEntityFactory(this)

	/** If true, every reference produced and accepted by this factory must carry either a key, or a value or both.
	  * This implies that there is no [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]]
	  * reference and that method will always
	  * throw a [[net.noresttherein.oldsql.exceptions.MissingKeyException MissingKeyException]].
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.required]]
	  */
	def isRequired :Boolean

	/** A weaker comparison than equals, which should be true if and only if absent references returned by both
	  * factories are equal for all possible key values. Conceptually this represents factories always referring
	  * to the same underlying entities, but possibly exporting them as different composite types
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

	/** Represents entity relationship with `E` at the end as `Option[X]`. This conflates both the case
	  * of the relationship not being loaded, and the relationship target not existing
	  * (such as with a `null` foreign key). Handling of `None` when an owning entity is passed for update depends
	  * on the [[net.noresttherein.oldsql.schema.Mapping mapping]] details. Generally, if a value is mandatory
	  * (non-null columns), it will typically be treated as 'unchanged'/'do not update'. Otherwise the default policy
	  * is to treat it as a lack of value and any previous relationship will be erased.
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.EntityOptionOptionFactory]]
	  */
	class EntityOptionFactory[K, E, X](implicit override val result :X ComposedOf E, key :E => Option[K],
	                                   override val equivalencyToken :AnyRef)
		extends RelatedEntityFactory[K, E, X, Option[X]]
	{
		override def delayed(key :K, value : => Option[X]) :Option[X] = None//Option(value)
//			throw new UnsupportedOperationException("Cannot create an Option with a lazy value, sorry.")

		override def present(value :X) :Option[X] = Some(value)
		override def missing(key :K) :Option[X] = None
		override def nonexistent :Option[X] = None

		override def keyFor(item :E) :Option[K] = None

		override def keyOf(ref :Option[X]) :Option[K] = ref.flatMap { x =>
			val keys = result.decomposer(x).view.map(key)
			val res = keys.headOption.flatten
			if (!keys.forall(_ == res))
				throw new IllegalArgumentException(
					keys.toSet.mkString("Keys of entities in the collection differ: ", ", ", ".")
				)
			res
		}

		override def valueOf(ref :Option[X]) :Option[X] = ref

		override def unapply(ref :Option[X]) :Option[(K, Option[X])] = keyOf(ref).map(_ -> ref)

		override def isRequired = false
	}



	/** Represents optional entity relationship with `E` at the end as `Option[Option[X]]`. The outer option
	  * specifies if the relationship was loaded: `Some(None)` means that relationship state is unknown and,
	  * when an entity with such a property is updated, the relationship state will remain unchanged.
	  * On the other hand, `None` means that the relationship is known to not exist
	  * (such as with a `null` foreign key).
	  * @see [[net.noresttherein.oldsql.model.RelatedEntityFactory.EntityOptionFactory]]
	  */
	class EntityOptionOptionFactory[K, E, X](implicit override val result :X ComposedOf E, key :E => Option[K],
	                                         override val equivalencyToken :AnyRef)
		extends RelatedEntityFactory[K, E, X, Option[Option[X]]]
	{
		override def delayed(key :K, value : => Option[X]) :Option[Option[X]] = None //Option(value)
//			throw new UnsupportedOperationException("Cannot create an Option with a lazy value, sorry.")

		override def present(value :X) :Option[Option[X]] = Some(Some(value))
		override def missing(key :K) :Option[Option[X]] = Some(None)
		override def nonexistent :Option[Option[X]] = None

		override def keyFor(item :E) :Option[K] = None

		override def keyOf(ref :Option[Option[X]]) :Option[K] = ref.flatten.flatMap { x =>
			val keys = result.decomposer(x).view.map(key)
			val res = keys.headOption.flatten
			if (!keys.forall(_ == res))
				throw new IllegalArgumentException(
					keys.toSet.mkString("Keys of entities in the collection differ: ", ", ", ".")
				)
			res
		}

		override def valueOf(ref :Option[Option[X]]) :Option[X] = ref.flatten
		override def unapply(ref :Option[Option[X]]) :Option[(K, Option[X])] = keyOf(ref).map(_ -> ref.flatten)

		override def isRequired :Boolean = false
	}






	private[oldsql] class OptionalKeyRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R],
	                                                                  override val nonexistent :R)
		extends RelatedEntityFactory[Option[K], E, X, R]
	{
		def this(backing :RelatedEntityFactory[K, E, X, R]) = this(backing, backing.nonexistent)

		override def result :ComposedOf[X, E] = backing.result

		override def delayed(key :Option[K], value : => Option[X]) :R = key match {
			case Some(k) => backing.delayed(k, value)
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
			case _ => nonexistent
		}

		override def keyFor(item :E) :Option[Option[K]] = Some(backing.keyFor(item))
		override def keyOf(ref :R) :Option[Option[K]] = Some(backing.keyOf(ref))
		override def valueOf(ref :R) :Option[X] = backing.valueOf(ref)

		override def isRequired = false

		override def equivalencyToken :Any =
			new EquivalencyTokenWrapper(backing.equivalencyToken) {
				override def toString = "OptionKey[" + token + "]"
			}

		override def toString = backing.toString + ".optionKey"
	}




	private[oldsql] class OptionalRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R])
		extends RelatedEntityFactory[Option[K], E, X, Option[R]]
	{ //perhaps this could be done for any higher type with polymorphic functions as wrappers/unwrappers?
		override def result = backing.result

		override def delayed(key :Option[K], value : => Option[X]) = key match {
			case Some(k) => Some(backing.delayed(k, value))
			case _ => None
		}

		override def apply(key :Option[K], value :Option[X]) :Option[R] = key match {
			case Some(k) => Some(backing(k, value))
			case _ => value match {
				case Some(v) => Some(backing.present(v))
				case _ => None
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

		override def keyFor(item :E) = backing.keyFor(item) match {
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
				override def toString = "Option[" + token + "]"
			}

		override def toString = backing.toString + ".optional"
	}




	private[oldsql] class RequiredRelatedEntityFactory[K, E, X, R](backing :RelatedEntityFactory[K, E, X, R])
		extends RelatedEntityFactory[K, E, X, R]
	{
		override def result = backing.result

		override def delayed(key :K, value : => Option[X]) = backing.delayed(key, value)

		override def apply(key :K, value :Option[X]) :R = backing(key, value)

		override def present(value :X) = backing.present(value)

		override def missing(key :K) = backing.missing(key)

		override def absent(key :K) :R = backing.absent(key)

		override def nonexistent = throw new MissingKeyException(toString + ": no key found")

		override def keyFor(item :E) = backing.keyFor(item)

		override def keyOf(ref :R) = backing.keyOf(ref)

		override def valueOf(ref :R) = backing.valueOf(ref)

		override def required :RelatedEntityFactory[K, E, X, R] = this

		override def isRequired :Boolean = true

		override def equivalencyToken =
			new EquivalencyTokenWrapper(backing.equivalencyToken) {
				override def toString = "Required[" + token + "]"
			}

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