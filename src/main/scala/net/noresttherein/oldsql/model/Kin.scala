package net.noresttherein.oldsql.model

import java.io.ObjectOutputStream

import scala.collection.{EvidenceIterableFactory, Factory, IterableFactory, IterableOps}
import scala.reflect.runtime.universe.{typeOf, TypeTag}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{AbsentKinException, IncompatibleElementTypeException}
import net.noresttherein.oldsql.model.ComposedOf.{CollectionOf, ComposableFrom, ConstructFrom, DecomposableTo}
import net.noresttherein.oldsql.model.Kin.{Derived, Property, Recomposed, Unknown}
import net.noresttherein.oldsql.model.Kin.Derived.{AbsentBase, PresentBase}
import net.noresttherein.oldsql.model.Kin.Present.PresentPropertiesKin
import net.noresttherein.oldsql.model.KinFactory.{BaseDerivedKinFactory, BaseKinFactory, DerivedKinFactory}
import net.noresttherein.oldsql.model.Restraint.{Restrainer, True}
import net.noresttherein.oldsql.morsels.generic.Self






/** A value of type `T` or an identification of such a value. It is used as a reference to a database resource
  * or resources (typically entity/entities, but also individual components of an entity).  A kin can be either
  * [[net.noresttherein.oldsql.model.Kin.Present$ Present]] - contain a value or be able to compute it
  * without a significant cost and outside resources - or [[net.noresttherein.oldsql.model.Kin.Absent$ Absent]],
  * in which case it should contain information required to locate and compute the given value. Absent kin can be either
  * [[net.noresttherein.oldsql.model.Kin.Missing$ Missing]] (if the value exists, in the logical sense - a corresponding
  * entity is expected to exist in the database) or [[net.noresttherein.oldsql.model.Kin.Nonexistent$ Nonexistent]] -
  * if it is known to not exist. Nonexistent kin typically carry no 'key', but it need not always be the case.
  * It is possible that the information about the distinction is unavailable, and thus checking only for these two cases
  * can miss some absent kin. In particular, a special [[net.noresttherein.oldsql.model.Kin.Unknown$ Unknown]] kin
  * is neither present nor does it contain any information about the value. It should be used sparingly
  * as most operations won't be able to do anything useful with it and may result in an error. However, if a member
  * of an updated entity, `Unknown` means unchanged - the relationship will not be updated.
  * Belonging to any of the above groups isn't determined by the class, but rather whether a value is present
  * and what additional information is known. This allows a single class to serve in all the above roles, depending
  * on given parameters, which is of particular importance for lazily evaluated instances.
  *
  * Independent of the above, an absent kin can, instead of some 'key'/specification of the referenced value, contain
  * one or more kin, the joint values of which comprise the referenced value. This allows adding a new element
  * to a collection without loading it to memory first. Additionally, ''many to many'' relationships implemented
  * with join tables can produce such kin if the rows of the join table are loaded, but not the related entities
  * referenced by their foreign keys. This allows kin to be 'partially loaded' - containing both absent and present kin
  * as their part. They are called ''collective'' kin and can be created and matched using
  * [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]] object.
  *
  * All of the above statuses are defined in terms of appropriate polymorphic methods of this trait -
  * `isXxx` for status `Xxx` and not the actual implementation subclass. In particular, both present and absent
  * instances can be modeled using the same class, and the exact status may even be not known at creation time,
  * such as with [[net.noresttherein.oldsql.model.Kin.Lazy$ lazy]] kin. The hidden specification of the referenced values
  * is however class/implementation dependent, and mapping components can typically handle directly only kin
  * constrained in a specific way (such as the mapped foreign key) and compatible implementation class.
  *
  * `Kin` are particularly useful to represent relationships between different database entities or
  * resources which can be otherwise expensive to compute and shouldn't be fetched by default - they shield model
  * classes from the implementation details about how those associations are implemented and resolved.
  * An instance thus might contain an associated, pre-fetched entity, or a foreign key to it.
  * They can be used for any type, including collections, and an absent `Kin` for a to-many relationship
  * can for example specify a reverse foreign key (foreign key on the target entity and the primary key on this entity).
  * They can also serve as actual search filters - queries are after all only a filter specification on a table.
  *
  * This trait, for reasons of clarity, uniformity and conciseness, specifies only the composite value type, providing
  * no information about the actual entity type from which it is derived. All operations which are dependent on
  * this element type in their definition require an implicit
  * [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]] (or
  * [[net.noresttherein.oldsql.model.ComposedOf ComposedOf ComposedOf]]), which might sometimes be inconvenient.
  * Similarly, an absent instance without any information about the composition type cannot be used to create
  * a present instance.  Whenever decomposition of the value type `T` is needed for a kin, it might be more practical
  * to use [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[E, T]` subtype, which knows both how to derive
  * the value type `T` from the element/entity type `E`, and how to extract all element values from kin value
  * if present. In order to avoid the inconvenience of providing both type arguments (likely repeating type `E`
  * as part of type `T`), one of specific type aliases can be used:
  *   - [[net.noresttherein.oldsql.model.Kin.One One]]`[E]` is a kin for a single entity (a ''to-one'' relationship);
  *   - [[net.noresttherein.oldsql.model.Kin.Supposed Supposed]]`[E]` is a kin for `Option[E]` (a ''to-zero-or-one''
  *     relationship);
  *   - [[net.noresttherein.oldsql.model.Kin.Many Many]]`[E]` is a derived kin for `Iterable[E]`;
  *   - [[net.noresttherein.oldsql.model.Kin.KinSeq KinSeq]] and [[net.noresttherein.oldsql.model.Kin.KinSet KinSet]]
  *     are `Many` subtypes for `Seq[E]` and `Set[E]` as expected;
  *   - finally, [[net.noresttherein.oldsql.model.Kin.Of Of]]`[C, E]` is a type alias for `Derived[C[E], E]`, allowing
  *     shorter and cleaner type signatures: `Set Of Swords`.
  *
  * When creating kin for collection types, prefer using the appropriate interface type, rather than one specifying
  * a single, default Scala implementation: `Seq[E]` instead of `List[E]`, `Map[K, E]` instead of `HashMap[K, E]`.
  * This allows loaded kin to use special tracking implementations, allowing optimisations when the kin for the
  * new collection is saved. For the same reason, prefer, where possible, to use the methods of the present collection
  * to create derived collections, rather than creating new collections from scratch and filling them with
  * elements manually.
  *
  * As a kin is immutable (at least in the interface - implementations may use mutable state as long as the client
  * will never see different results for the same call on the `Kin`), it is covariant in regards to the value type -
  * a `Kin[A]` is intuitively a `Kin[B]` for `B >: A`. Their dual nature of value-or-specification-of-a-value has
  * however a hidden contravariant element to it in the latter form: in the most trivial example, a `Kin` specifying
  * 'all instances of the given type' can be interpreted differently when viewed as a `Kin` to a super type - larger
  * set of theoretically possible values. In most cases it won't matter and the set of possible values is constrained
  * externally in an explicit way, but it is something to be aware of.
  *
  * @tparam T type of the represented value.
  * @see [[net.noresttherein.oldsql.model.Kin.Present$ Present]]
  * @see [[net.noresttherein.oldsql.model.Kin.Absent$ Absent]]
  * @see [[net.noresttherein.oldsql.model.Kin.Unknown$ Unknown]]
  * @see [[net.noresttherein.oldsql.model.Kin.Nonexistent$ Nonexistent]]
  * @see [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]]
  * @author Marcin Mościcki
  */
trait Kin[+T] extends Serializable {

	/** A `Kin` is absent if it doesn't contain a value. */
	@inline final def isAbsent :Boolean = !isPresent

	/** Is this `Kin` absent and known to not exist at all? Not all implementations support this level of information;
	  * this method can still return `false`, even if the entity (or entities) does not exist, but the fact could not
	  * be determined when this instance was created. Implies [[net.noresttherein.oldsql.model.Kin.isAbsent isAbsent]]
	  * and complements [[net.noresttherein.oldsql.model.Kin.isMissing isMissing]], although both `isMissing`
	  * and `isNonexistent` can return `false`, even if `isAbsent` is `true`.
	  */
	def isNonexistent :Boolean = false

	/** Is this `Kin` absent in this instance, but is otherwise known to exist? Not all implementations support this
	  * level of information; this method can still return `false`, even if the entity (or entities) does exist,
	  * but the fact could not be determined when this instance was created.
	  * Implies [[net.noresttherein.oldsql.model.Kin.isAbsent isAbsent]] and complements
	  * [[net.noresttherein.oldsql.model.Kin.isNonexistent isNonexistent]], although both `isMissing` and `isNonexistent`
	  * can return `false`, even if `isAbsent` is `true`.
	  */
	def isMissing :Boolean = false

	/** Is this `Kin` the [[net.noresttherein.oldsql.model.Kin.Unknown Unknown]] instance? Unknown kin are absent
	  * and feature not information at all about the referenced values. It is used to indicate 'no change'
	  * to a kin property of an entity and sometimes as a temporary value.
	  */
	def isUnknown :Boolean = false

	/** Does this `Kin` contain a value? */
	def isPresent :Boolean = toOption.isDefined

	/** A type level equivalent of method [[net.noresttherein.oldsql.model.Kin.isPresent isPresent]]. Any `Kin` type
	  * can be refined by setting this property to `true`, marking it as ''present'':
	  * {{{
	  *     val boo :Kin[Hamster] { type isLoaded = true }
	  * }}}
	  * Some load functions refine the returned entity type by setting this property, and application code
	  * can similarly do so, requiring as arguments entities with certain relationships loaded.
	  */
	type isPresent <: Boolean with Singleton //todo: actually use it.

	/** The referenced value in `Some` if this kin is present, or `None` if it is absent. */
	def toOption :Option[T]

	/** The referenced value as [[net.noresttherein.oldsql.collection.Opt.Got Got]]`(x)` if this kin is present,
	  * or [[net.noresttherein.oldsql.collection.Opt.Lack Lack]] if it is absent.*/
	def opt :Opt[T] = if (isPresent) Got(get) else Lack //todo: make this primary, rather than toOption

	/** Returns the value of this kin, if present, or throws
	  * [[net.noresttherein.oldsql.exceptions.AbsentKinException AbsentKinException]] (a `NoSuchElementException`
	  * subclass) otherwise.
	  */
	def get :T = if (isPresent) toOption.get else throw new AbsentKinException(this)

	/** Returns the value of this kin. It is the same as [[net.noresttherein.oldsql.model.Kin.get get]],
	  * but required implicit evidence gives a soft guarantee that the call will not throw an
	  * [[net.noresttherein.oldsql.exceptions.AbsentKinException AbsentKinException]]. 'Soft guarantee' means here that
	  * it is guaranteed as long as the application does not attempt to cast down this type itself.
	  * The [[net.noresttherein.oldsql.model.Kin.isPresent! isPresent]] member type, which this method relies on,
	  * is not directly tied to kin value, and a cast to `Kin[_] { type isPresent = true }` will be always successful,
	  * which creates a doorway for introducing discrepancies when not careful.
	  */
	@inline final def apply()(implicit present :this.type <:< (Kin[_] { type isPresent = true })) :T = get

	/** Returns the first element of the contents, in the sense and order appropriate to its type.
	  *   - For collections, this is the `this.get.head` element;
	  *   - For maps, it is a pair `k->v` made from the tuple `this.get.head` (see [[net.noresttherein.oldsql.model.-> ->]];
	  *   - For `Option`, it is equivalent to `this.get.get`;
	  *   - For non-composite types, it is simply `this.get`;
	  *  The primary purpose of this method is use in lambda functions used as descriptors
	  *  specifying which relationships should be fetched together with a queried entity, providing a clearer
	  *  view and a uniform access to the related entity's properties.
	  *  @throws NoSuchElementException if this kin is absent
	  */
	def fetch[E](implicit decomposition :T DecomposableTo E) :E = decomposition.first(get)


	/** Decomposes this kin value to individual elements `E` based on the implicit
	  * [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]]`[T, E]`.
	  * It is equivalent to `toOption.map(decomposition(_))`.
	  */
	def decompose[E](implicit decomposition :T DecomposableTo E) :Option[Iterable[E]] =
		if (isPresent) Some(decomposition(get)) else None

	/** If this kin is an instance of [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, E]`
	  * (with composition compatible element type-wise with implicit decomposition of `T to E`), returns
	  * kin for all parts of the referenced collection. By default, this will be simply `this::Nil`, but
	  * [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]] kin return all listed constituents.
	  * instances of classes not derived from `Derived`, or using incompatible decomposition return `None`.
	  */
	def explode[E](implicit decomposition :T DecomposableTo E) :Option[Iterable[Derived[E, _]]] = None


	@inline final def getOrElse[U >: T](default: => U): U = if (isPresent) get else default

	@inline final def default[U >: T](default :U) :U = if (isPresent) get else default


	@inline final def orElse[U >: T](alternative: => Kin[U]) :Kin[U] = if (isPresent) this else alternative

	/** Returns `this` if it contains a value or `alternative` otherwise. The difference from
	  * [[net.noresttherein.oldsql.model.Kin.orElse orElse]] is that
	  * the argument is evaluated eagerly, guaranteeing that no closure will be created and should have better performance
	  * if the alternative value was computed beforehand.
	  */
	@inline final def ifAbsent[U >: T](alternative :Kin[U]) :Kin[U] = if (isAbsent) alternative else this

	@inline final def ifUnknown[U >: T](alternative :Kin[U]) :Kin[U] = if (isUnknown) alternative else this

	@inline final def ifNonexistent[U >: T](alternative :Kin[U]) :Kin[U] = if (isNonexistent) alternative else this


	/** Lifts this kin to a [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Iterable[T]]`
	  * for a singleton collection. Returned instance will retain this kin's status and contain `this.get::Nil` if
	  * it is present. Useful when including kin for a single entity
	  * in a [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]] kin.
	  */
	def singleton :Kin[Iterable[T]] = singleton(Iterable)

	/** Lifts this kin to a [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, C[T]]`
	  * for a singleton collection. Returned instance will retain this kin's status and contain `factory(his.get)` if
	  * it is present. Useful when including kin for a single entity
	  * in a [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]] kin.
	  */
	def singleton[C[+X] <: Iterable[X]](factory :IterableFactory[C]) :Kin[C[T]] =
		Recomposed(this)(DecomposableTo.Self(), ComposableFrom.Collection.of[T](factory))

	/** Converts this instance to [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[E, U]`.
	  * If it already derives from `Derived` and uses the same composition as the one provided implicitly,
	  * it returns itself. Otherwise the returned instance will be an adapter wrapping this instance.
	  * The latter case can be matched using `Derived`'s
	  * [[net.noresttherein.oldsql.model.Kin.Derived.unapply unapply]] method.
	  */
	def recompose[E, U >: T](implicit composition :U ComposedOf E) :Derived[E, U] =
		Recomposed(this)(composition.decomposer, composition.composer)



	/** Creates a [[net.noresttherein.oldsql.model.Kin.Derived derived]] kin encompassing all values of `E`
	  * referenced by this instance and the given instance. This is semantically equivalent to adding the given element
	  * to the logical collection referenced by this instance. Present kin may opt to eagerly add the element
	  * to the collection instead of creating a collective proxy.
	  * @return `this ++ Derived.present(item::Nil)`.
	  */
	def +[E, U >: T](item :E)(implicit composition :U CollectionOf E) :Kin[U] =
		this ++[E, List[E], U] Derived.many(item::Nil)

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin encompassing all values of `E`
	  * referenced by this instance and the value of the given kin. This is semantically equivalent to adding the given
	  * element to the logical collection referenced by this instance. Present kin may opt to eagerly add
	  * the new element (if present) instead of creating a collective proxy.
	  * @return `this ++ item.singleton`.
	  */
	def +[E, U >: T](item :Kin[E])(implicit composition :U CollectionOf E) :Kin[U] =
		this ++[E, List[E], U] item.singleton(List)

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Derived derived]] kin encompassing all values of `E`
	  * referenced by this instance and the given value. This is semantically equivalent to prepending
	  * (in the sense defined by implicit `composition.composer.builder`) the given element to the logical collection
	  * referenced by this instance. For most collection types without an imposed order, this is equivalent
	  * to `this + item`. Present kin may opt to eagerly add the new element instead of creating a collective proxy.
	  * @return `Derived.present(item::Nil) ++: this`.
	  */
	def +:[E, U >: T](item :E)(implicit composition :U CollectionOf E) :Kin[U] =
		Derived.many(item::Nil) ++:[E, List[E], U] this

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin encompassing all values of `E`
	  * referenced by this instance and the value of the given kin. This is semantically equivalent to prepending
	  * (in the sense defined by implicit `composition.composer.builder`) the given element to the logical collection
	  * referenced by this instance. For most collection types without an imposed order, this is equivalent
	  * * to `this + item`. Present kin may opt to eagerly add the new element (if present)
	  * instead of creating a collective proxy.
	  * @return `item.singleton ++: this`.
	  */
	def +:[E, U >: T](item :Kin[E])(implicit composition :U CollectionOf E) :Kin[U] =
		item.singleton(List) ++:[E, List[E], U] this

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Derived derived]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to creating a union of the two referenced logical
	  * collections (in the sense defined by implicit `composition.composer.builder`). Present kin may opt to eagerly
	  * add the new element instead of creating a collective proxy.
	  * @return `this ++ Derived.present(items)`.
	  */
	def ++[E, U >: T](items :Iterable[E])(implicit composition :U CollectionOf E) :Kin[U] =
		this ++[E, Iterable[E], U] Derived.many(items)

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to creating a union of the two referenced logical
	  * collections (in the sense defined by implicit `composition.composer.builder`). Present kin may opt
	  * to eagerly add the new element (if Present) instead of creating a collective proxy.
	  */
	def ++[E, X, U >: T](items :Kin[X])
	                    (implicit composition :U CollectionOf E, decomposition :X DecomposableTo E) :Kin[U] =
		items match {
			case composite :Derived[E @unchecked, X @unchecked]
				if composite.composition compatibleWith decomposition => this ++[E, X, U] composite
			case _ => this ++[E, U, U] Recomposed(items)(decomposition, composition.composer)
		}

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to creating a union of the two referenced logical
	  * collections (in the sense defined by implicit `composition.composer.builder`). Present kin may opt
	  * to eagerly add the new element (if Present) instead of creating a collective proxy.
	  */
	def ++[E, X, U >: T](items :Kin.Derived[E, X])(implicit composition :U CollectionOf E) :Kin[U] =
		Recomposed(this)(composition.decomposer, composition.composer) ++[E, X, U] items


	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to prepending the given collection to the one referenced
	  * by this kin (in the sense defined by implicit `composition.composer.builder`). For most collection types
	  * without an imposed order, this is equivalent to `this ++ items`. Present kin may opt to eagerly
	  * add the new element (if Present) instead of creating a collective proxy.
	  * @return `Derived.present(items) ++: this`.
	  */
	def ++:[E, U >: T](items :Iterable[E])(implicit composition :U CollectionOf E) :Kin[U] =
		Derived.many(items) ++:[E, Iterable[E], U] this

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to prepending the given logical collection
	  * to the one referenced by this kin (in the sense defined by implicit `composition.composer.builder`).
	  * For most collection types without an imposed order, this is equivalent to `this ++ items`.
	  * Present kin may opt to eagerly add the new element (if Present) instead of creating a collective proxy.
	  */
	def ++:[E, X, U >: T](items :Kin[X])
	                     (implicit composition :U CollectionOf E, decomposition :X DecomposableTo E) :Kin[U] =
		items match {
			case composite :Derived[E @unchecked, X @unchecked]
				if composite.composition compatibleWith decomposition => composite ++:[E, X, U] this
			case _ => Recomposed(items)(decomposition, composition.composer) ++:[E, U, U] this
		}

	/** Creates a [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin including all elements of
	  * this and the given kin. This is semantically equivalent to prepending the given logical collection
	  * to the one referenced by this kin (in the sense defined by implicit `composition.composer.builder`).
	  * For most collection types without an imposed order, this is equivalent to `this ++ items`.
	  * Present kin may opt to eagerly add the new element (if Present) instead of creating a collective proxy.
	  */
	def ++:[E, X, U >: T](items :Kin.Derived[E, X])(implicit composition :U CollectionOf E) :Kin[U] =
		items ++:[E, X, U] Recomposed(this)(composition.decomposer, composition.composer)

	//todo: -
	/** Create a kin for the value of property `property` of `T` in this kin. */
	def property[X, U >: T](property :PropertyPath[U, X]) :Kin[X] = Property.one(this, property)

	/** Create a kin for a collection of values of property `property :X` of all individual elements `E`
	  * comprising the value of this kin.
	  * @param property a property of item type `E` of which value `T` of this kin is composed.
	  * @param in a factory for a type `C[_]` used to collect property values of all items in this kin.
	  *           This is typically the companion object to the collection type.
	  * @param decomposition implicit information how the value `T` of this kin decomposes to individual elements `E`.
	  */
	def properties[E, X, C[_]](property :PropertyPath[E, X], in :IterableFactory[C])
	                          (implicit decomposition :T DecomposableTo E) :Kin[C[X]] =
		properties(property, ComposableFrom.Collection.of[X](in))

	/** Create a kin for a collection of values of property `property :X` of all individual elements `E`
	  * comprising the value of this kin.
	  * @param property a property of item type `E` of which value `T` of this kin is composed.
	  * @param in a factory for a type `C[_]` used to collect property values of all items in this kin.
	  *           This is typically the companion object to the collection type.
	  * @param decomposition implicit information how the value `T` of this kin decomposes to individual elements `E`.
	  */
	def properties[E, X, C[_], Ev[_]](property :PropertyPath[E, X], in :EvidenceIterableFactory[C, Ev])
	                                 (implicit decomposition :T DecomposableTo E, ev :Ev[X]) :Kin[C[X]] =
		properties(property, ComposableFrom.Collection.of[X](in))

	/** Create a kin for a collection of values of property `property :X` of all individual elements `E`
	  * comprising the value of this kin.
	  * @param property a property of item type `E` of which value `T` of this kin is composed.
	  * @param as information about the composition of the required type `C` from individual property values `X`.
	  * @param decomposition implicit information how the value `T` of this kin decomposes to individual elements `E`.
	  */
	@throws[IncompatibleElementTypeException](
		"if this kin is a DerivedKin and its composition is incompatible with the passed decomposition (that is, " +
		"type E of intended elements doesn't match the internal element type of this instance."
	)
	def properties[E, X, C](property :PropertyPath[E, X], as :C ComposableFrom X)
	                       (implicit decomposition :T DecomposableTo E) :Kin[C] //=
//		Property(this, property)(decomposition, as)


//	@inline final def map[O](f: T => O): Kin[O] =
//		if (isAbsent) Unknown else Present(f(this.get))
//
	@inline final def flatMap[O](f: T => Kin[O]): Kin[O] =
		if (isPresent) f(this.get) else Unknown

	@inline final def filter(p: T => Boolean): Kin[T] =
		if (isAbsent || p(this.get)) this else Unknown

	@inline final def filterNot(p: T => Boolean): Kin[T] =
		if (isPresent && p(this.get)) Unknown else this


	@inline final def withFilter(p: T => Boolean): Kin[T] =
		if (isAbsent || p(this.get)) this else Unknown



	@inline final def contains[U >: T](elem: U): Boolean =
		!isAbsent && this.get == elem

	@inline final def exists(p: T => Boolean): Boolean =
		!isAbsent && p(this.get)

	@inline final def forall(p: T => Boolean): Boolean = isAbsent || p(this.get)

	@inline final def foreach[O](f: T => O) :Unit =
		if (!isAbsent) f(this.get)



	def canEqual(that :Any) :Boolean = that.isInstanceOf[Kin[_]]

	/** Kin equality is determined primarily as equality on the value type `T`. Implementation classes which
	  * include additional information - the specification needed to load the absent value - can override it,
	  * adding the 'key' value equality to the condition. Equality does not take into account various specific
	  * cases of being absent - kin instances differing only in their `isMissing` properties will compare equal.
	  * The exception is [[net.noresttherein.oldsql.model.Kin.Unknown Unknown]] which is equal to no other kin.
	  */
	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case other :Kin[_] if other canEqual this =>
			if (isPresent) other.isPresent && get == other.get else other.isAbsent
		case _ => false
	}

	override def hashCode :Int = if (isAbsent) Unknown.hashCode else get.hashCode

	override def toString :String = toOption match {
		case Some(x) => "Present(" + x + ")"
		case _ if isMissing => "Missing"
		case _ if isNonexistent => "Nonexistent"
		case _ => "Absent"
	}

}






object Kin {

	/** Converts the option to a present or absent kin based on whether it is defined or empty. */
	@inline final def some_?[T](value :Option[T]) : Kin[T] =
		if (value.isEmpty) Unknown else Present(value.get)


	@inline final def apply[T](value :T) :Kin[T] =
		if (value == null) Unknown else Present(value)




	implicit def singleResult[T](composer :KinCollector[T]) :Kin[T] = composer.one
	implicit def optionalResult[T](composer :KinCollector[T]) :Kin[Option[T]] = composer.option
	implicit def multipleResults[C <: Iterable[T], T](composer :KinCollector[T])
	                                                 (implicit factory :Factory[T, C]) :Kin[C] =
		composer.as[C]

	/** Container for a specification of a filter on type `T`, which can be requested to force the view
	  * of the result set to a different type, depending on expected amount of values - it can be a single value
	  * (simply [[net.noresttherein.oldsql.model.Kin Kin]]`[T]`), a `Kin[Option[T]]`, or of a collection.
	  * Resolving a `Kin` for which the number of results cannot be coerced into the requested type will result
	  * in an error, rather than omitting extraneous results.
	  * @tparam T the element type returned by a query from which the values of the `Kin` created by this instance
	  *           are composed.
	  */
	trait KinCollector[T] {
		/** Return the result as a single element. If the result set is empty or has more elements,
		  * a `KinCompositionException` will be thrown when the returned `Kin` is resolved.
		  */
		def apply() :Kin[T] = one

		/** Return the result as a single element. If the result set is empty or has more elements,
		  * a `KinCompositionException` will be thrown when the returned `Kin` is resolved.
		  */
		def one :Kin[T]

		/** Return the result set as zero or one elements. Note that `Kin[Option[T]]` is conceptually different than
		  * `Option[Kin[T]]` - in the latter case the number of results is known before hand, while in the former
		  * only when resolving the Kin. If the result set has more than one element, a `KinCompositionException`
		  * will be thrown when the returned `Kin` is result
		  */
		def option :Kin[Option[T]] = as[Option[T]]

		/** the Return result set as zero or one elements. Note that `Kin[Option[T]]` is conceptually different than
		  * `Option[Kin[T]]` - in the latter case the number of results is known before hand, while in the former
		  * only when resolving the `Kin`. If the result set has more than one element, a `KinCompositionException`
		  * will be thrown when the returned `Kin` is result
		  */
		def supposed :Kin[Option[T]] = as[Option[T]]

		/** Return the result in an `Iterable[T]` of unspecified order and without duplicate removal. */
		def many :Kin[Iterable[T]] = as[Iterable[T]]

		/** Return the result as a hash set. */
		def set :Kin[Set[T]] = as[Set[T]]

		/** Return the result as a sequence. */
		def seq :Kin[Seq[T]] = as[Seq[T]]

		/** Return the result set as composite type `C`. A composite type is a type which can be constructed
		  * and deconstructed to `Iterable[T]` (possibly with limitations to its size). The main examples for
		  * composites of `T` are: just `T`, `Option[T]`, `_ <: Iterable[T]`.
		  * @param expand implicit definition of how the result is mapped into a collection of T values.
		  * @tparam C `Kin` value type.
		  */
		def as[C](implicit expand :C ComposableFrom T) :Kin[C]

		/** A shorthand for `as[C[T]]` - you can write just Kin.in[Seq] to obtain a `Kin[Seq[T]]`. */
		def in[C[X]](implicit expand :C[T] ComposableFrom T) :Kin[C[T]] = as[C[T]]

		/** Return the result set as a collection `C[E]` produced by the given factory (collection companion object). */
		def in[C[X] <: Iterable[X]](factory :IterableFactory[C]) :Kin[C[T]] =
			as(ComposableFrom.Collection.of[T](factory))
	}



	/** Create a `Kin[T]` in a two step process - the first step (this one) returns a `KinCollector` containing
	  * all information required to compute the target value. In a second step, a call on the returned instance
	  * will specify the expected result type for the found values (single value, `Seq`, `Set`, etc).
	  * Implicit conversion exists converting a `KinCollector[T]` to a `Kin[T]`.
	  *
	  * Example: `Kin[Familiar](Equal(_.name, "boo")).one`.
	  *
	  * This call delegates to `AllWhere(restraint)`.
	  *
	  * @tparam T value type on which the given filter operates.
	  * @return A factory for `Kin` of different types that can be built from a collection of `T` values.
	  * @param restraint a filter with all parameters on the universe of values for `T`.
	  */
	def apply[T](restraint :Restraint[T]) :KinCollector[T] = AllWhere(restraint)

	/** Create a `Kin` in a three step process - the first stop (this one) accepts a definition of how to look for
	  * the value(s) (for example, by a primary key) and returns a `KinFactory`. The second, optional step
	  * defines the result type carried by the returned Kin, if it should be something else than the entity type `T`
	  * itself. It is done with one of the methods of the factory which return new factories for a composite type,
	  * such as `Kin[Option[T]]` or `Kin[Set[T]]`; for example:
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.as as]]`Option[T]` or
	  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]]`[Set]`.
	  * The final step specifies the value for the key parameter `K` from the first step and returns a `Kin` carrying
	  * that key together with all information needed to find the value(s) - for example, the property of the queried
	  * entity with a value of `K` - and construct the return type from the collection of matching entities.
	  *  Example:
	  * {{{
	  *     Kin(Equal{ boo :Familiar => boo.species.id)).in(Seq)("hamster")
	  * }}}
	  * @tparam T entity type referenced by the final `Kin`.
	  * @tparam K key type - the variable element of the search filter, such as a value of a property.
	  * @param restrainer a factory of `Restraint` instances for entity `T` using values of `K` as parameters.
	  * @return [[net.noresttherein.oldsql.model.Kin.AllWhere AllWhere]]`(restrainer)`.
	  */
	def apply[T, K](restrainer :Restrainer[T, K]) :KinFactory[K, T, T] = AllWhere(restrainer)




	/** A factory of [[net.noresttherein.oldsql.model.Kin Kin]] referencing all instances of a given type
	  * in some universe, as defined by the resolver. Depending on the implementation of a handler, it can return
	  * all rows in a table, or in a collection of tables for related types.
	  */
	object All {
		/** Create a composer as the first step of Kin creation. Returned composer can be asked to produce
		  * a `Kin` exporting values of the given type as a desired composite type (for example, a collection or option).
		  * Example: `All.of[E].in[Seq]`.
		  * @see [[net.noresttherein.oldsql.model.Kin.All.apply[T] apply]]`[T]`
		  */
		def of[T] :KinCollector[T] = composer.asInstanceOf[KinCollector[T]]

		/** Create an 'All' kin as `Kin[X]` - exact interpretation will depend on the client code
		  * and may not always make sense, i.e. `All[Int]()` doesn't mean 'all integers', but 'all of an integer'.
		  * Generally, `All[''Entity'']()` will mean 'there should be exactly one ''Entity'', and `All[Seq[Entity]]()`
		  * 'all entities in a sequence'. It is shorter to write than `All.of[X].one` or `All.of[X].as[Seq[X]]`.
		  */
		@inline def apply[T] :AllKinAs[T] = new AllKinAs[T] {}

		/** Create a `Kin` referencing all available values of `E`, returned as `T`. */
		def apply[T, E]()(implicit composition :T ComposableFrom E) :Kin[T] = Restrained[E, T](True)

		/** Was this Kin explicitly created as 'All' by this object? */
		def unapply[T](kin :Kin[T]) :Boolean = kin match {
			case Restrained(True, _) => true
			case _ => false
		}


		trait AllKinAs[T] extends Any {
			@inline final def apply[E]()(implicit composition :T ComposableFrom E) :Kin[T] = All[T, E]()
		}

		private object composer extends KinCollector[Any] {
			override def one: Kin[Any] = All[Any, Any]()(ComposableFrom.itself)
			override def as[C](implicit expand: C ComposableFrom Any): Kin[C] = All[C, Any]()
		}
	}



	/** A factory and match pattern of `Kin` referencing all instances of a given type which satisfy a given `Restraint`. */
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
		def apply[K, T](restrainer :Restrainer[T, K]) :KinFactory[K, T, T] = Restrained(restrainer)

		/** Create a composer allowing to create `Kin` containing values of `T` - for example `Kin[T]`,
		  * `Kin[Option[T]]`, `Kin[Seq[T]]` and so on.
		  * @param restraint specification of how to look for the value (for example, a query filter with parameters)
		  * @tparam T filtered type
		  * @return a factory of Kins with values being various collections of T
		  */
		def apply[T](restraint :Restraint[T]) :KinCollector[T] = new RestrainedCollector(restraint)

		/** Check if the given `Kin` is a restrained `Kin` created by this instance and return the `Restraint` held by it.
		  * Even if this `Kin` was in fact created by this object, a check is performed if the composition used to
		  * create it is the same as the one passed implicitly, to assert that the element type parameter for the `Restraint`
		  * stored in this `Kin` is actually compatible with the one specified implicitly. If they are not, this method
		  * will not match. The check is performed using the compatibility comparison method for `ComposedOf`,
		  * so it is not 100% bullet proof. Note that the domain for the returned `Restrained` is a subtype of `E`
		  * rather than `E`, as due to `Kin` being covariant, it is possible that `T` is a supertype of the type
		  * provided when the instance was created.
		  *
		  * @param kin `Kin` to match.
		  * @param decomposition specification of what type `E` type `T` should be decomposed to and how.
		  * @return the restraint stored in this instance if it exists and is compatible with `composition`.
		  */
		def unapply[E, T](kin :Kin[T])(implicit decomposition :T DecomposableTo E) :Opt[Restraint[_ <: E]] =
			kin match {
				case r :Restrained[e, T] if r.composition compatibleWith decomposition => Got(r.where)
				case _ => Lack
			}

		@SerialVersionUID(KinVer)
		private class RestrainedCollector[T](restraint :Restraint[T]) extends KinCollector[T] {
			override def one: Kin[T] = Restrained.missing[T, T](restraint)

			override def as[C](implicit composition: C ComposableFrom T): Kin[C] =
				Restrained.missing[T, C](restraint)
		}
	}






	/** A factory for [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin with the value type being
	  * the result of application of a specific type constructor.
	  * @define Ref
	  * @define Val
	  */
	sealed trait SpecificKinType[C[_]] extends Any {
		protected implicit def composition[T] :C[T] ComposedOf T
		@inline protected implicit final def decomposer[T] :C[T] DecomposableTo T = composition[T].decomposer

		/** The type of `Kin` produced by this factory */
		type Ref[T] = Derived[T, C[T]]

		/** A factory creating present and absent $Ref`[T]` kin in several configurations and providing some operations
		  * on them, using [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` as a hidden filter to identify
		  * the referenced value. All absent instances created by this factory are automatically
		  * [[net.noresttherein.oldsql.model.Kin.isMissing missing]], as there is no support for
		  * [[net.noresttherein.oldsql.model.Kin.isNonexistent nonexistent]]
		  * [[net.noresttherein.oldsql.model.Kin.Derived Derived]] values.
		  */
		def apply[K, T](restrainer :Restrainer[T, K]) :DerivedKinFactory[K, T, C[T]] =
			Restrained.required(restrainer).in[C]

		/** An absent (and [[net.noresttherein.oldsql.model.Kin.isMissing missing]]) $Ref`[T]`, specifying
		  * a filtering expression on `T`, carried by `restraint`. Note that it is possible to create such references
		  * for any type, including those not mapped and completely unrelated to the domain model;
		  * if the type information included is insufficient for unique identification of a data source for `T`
		  * (i.e., table), an exception will be thrown when an attempt of resolving it takes place.
		  */
		def apply[T](restraint :Restraint[T]) :Ref[T] = Restrained.missing[T, C[T]](restraint)(composition.composer)

		/** A present $Ref`[T]` without any specific, external information about its key(s) or identifier(s).
		  * The appropriate data must be carried by the contents of `value`; whether they are treated jointly,
		  * having a common key, or individually, depends on the mapping of the component it is used for.
		  * Maintaining data integrity of all objects in the reachable graph is the responsibility of the application;
		  * Inconsistencies involving bidirectional relationships may result in exceptions being thrown
		  * when discovered, or in unexpected behaviour. Note that a kin for an empty collection or option is still
		  * considered ''present'' according to the established semantics, as there is typically no distinction
		  * in SQL databases between undefined and zero number of results.
		  */
		def apply[T](value :C[T]) :Ref[T] = Derived.present[T, C[T]](value)

		/** A present $Ref`[T]` carrying both the $Val value and a filter expression `restraint` which
		  * specifies the restrictions that all returned values of `T` must satisfy in order to be selected.
		  * This constructor is more often used by the framework than the client code, after loading a value
		  * from the database and thus having both the entity and the specification provided in order to fetch it,
		  * or derived from read column data. If the specific query parameters present in `restraint` are inconsistent
		  * with `value`, the behaviour depends on the mapping implementation, which may report an error,
		  * attempt some reconciliation, or simply always pick one of the two sources as the source of effective values.
		  */
		def apply[T](restraint :Restraint[T], value :C[T]) :Ref[T] = Restrained(restraint, Some(value))

		/** A composite $Ref`[T]` carrying a filter condition `restraint` serving as a specification of
		  * referenced values or their source. The value of the created kin is given as a by-name initializing
		  * expression which will be invoked at most once: after first access to
		  * [[net.noresttherein.oldsql.model.Kin.toOption toOption]],
		  * [[net.noresttherein.oldsql.model.Kin.isPresent isPresent]] or a related method, all future calls
		  * will work on the cached results. This laziness allows introducing cyclic relationships by referencing
		  * other entities captured in a closure only after all model objects are constructed.
		  * It is application's responsibility to assure that `value` is consistent with `restraint` and with itself.
		  * Mismatched keys or sides of bidirectional relationships may result in an error when the kin is resolved
		  * or written to the database, or in erroneous behaviour. The general policy is to always prefer the
		  * information from `value` and interpret `restraint` as the original ''source'' of the value.
		  */
		def delay[T](restraint :Restraint[T], value: => Option[C[T]]) :Ref[T] = Restrained.delay(restraint, value)

		/** A present [[net.noresttherein.oldsql.model.Kin.Derived derived]] kin which value will be lazily computed
		  * on demand and which carries no external key/id information in the form of the query filter
		  * [[net.noresttherein.oldsql.model.Restraint Restraint]], but which must be extracted from `value` itself
		  * by the mapping responsible for this component.
		  */
		def delay[T](value: => C[T]) :Ref[T] = Derived.delay(value)

		/** Matches stock `Derived` implementations carrying a [[net.noresttherein.oldsql.model.Restraint Restraint]]
		  * and using [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Collection ComposableFrom.Collection]],
		  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Iterable ComposableFrom.Iterable]],
		  * composition. This includes all instances created by this object as well as sibling factories under
		  * [[net.noresttherein.oldsql.model.Kin Kin]], if that composition type was provided explicitly.
		  */
		def unapply[T](kin :Kin[C[T]]) :Opt[Restraint[_ <: T]] = kin match {
			case AllWhere(restraint) => Got(restraint)
			case _ => Lack
		}
	}



	/** A type alias for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, T]`, that is a 'to-one'
	  * relationship with entity `T`. Aside from carrying this decomposition explicitly as part of its type,
	  * without a need for implicit `ComposableFrom` at usage point, it has one additional subtle difference
	  * from the simple [[net.noresttherein.oldsql.model.Kin Kin]]`[T]`: it can only reference a single entity,
	  * disallowing [[net.noresttherein.oldsql.model.Kin.isNonexistent nonexistent]] instances.
	  * While creating a class violating this restriction is technically possible, it cannot be done by any API method
	  * and the framework never returns such instances to the application, throwing a `NonexistentEntityException`.
	  * @see [[net.noresttherein.oldsql.model.Kin.One$]]
	  */
	type One[T] = Derived[T, T]

	/** A factory and an extractor for `Kin` referencing single entities (''to-one'' relationships).
	  * The referenced entity is mandatory: all absent kin created by this instance or factories it returns
	  * are [[net.noresttherein.oldsql.model.Kin.isMissing missing]] instances. Additionally, all factories are
	  * [[net.noresttherein.oldsql.model.KinFactory.RequiredKinFactory RequiredKinFactory]] instances,
	  * for which `isRequired == true`.
	  * The kin produced here wrap [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` instances
	  * and can be matched with other extractors defined in [[net.noresttherein.oldsql.model.Kin$ Kin]] object.
	  * This match pattern is not however considered exhaustive for the `One[T]` type, as other (in particular
	  * lower level) implementations of `Derived` exist.
	  * @define Ref `One`
	  * @define Val `T`
	  */
	object One extends SpecificKinType[Self] {
		protected override implicit def composition[T] :T ComposedOf T = ComposedOf.self
		override type Ref[T] = One[T]
	}



	/** A [[net.noresttherein.oldsql.model.Kin Kin]] which might resolve to at most a single result, the existence
	  * of which cannot be determined beforehand. This is a type alias for
	  * [[net.noresttherein.oldsql.model.Derived Derived]]`[T, Option[T]]` which, in addition to the standard
	  * `Kin[Option[T]]` interface, carries information about the decomposition and composition of the value type
	  * to the underlying referenced entities. This makes it friendlier to use in generic code,
	  * as no implicit `Option[T] `[[net.noresttherein.oldsql.model.ComposedOf ComposedOf]]` T` is required.
	  * Note that `Derived` class doesn't support referencing not existing values naturally
	  * (there is no [[net.noresttherein.oldsql.model.Kin.Nonexistent$ Nonexistent]] kin implementation provided for it),
	  * so all ''to-at-most-one'' relationships must be declared as either `Supposed[T]`
	  * or `Option[`[[net.noresttherein.oldsql.model.Kin.One One]]`[T]]` (typically the latter for foreign keys).
	  * Due to the information about the data existence being unavailable beforehand in the `restraint`,
	  * but determined only after executing a dedicated query, this type isn't normally used for foreign keys,
	  * but for their inverses (in one-to-one relationships) and arbitrary queries specified using
	  * [[net.noresttherein.oldsql.model.Restraint Restraint]] API and passed for execution to the framework.
	  * While there are no technical limitations preventing its existence, no nonexistent `Derived` kin can be created
	  * with the provided API or are ever returned to the application.
	  * @see [[net.noresttherein.oldsql.model.Kin.Supposed$]]
	  */
	type Supposed[T] = Derived[T, Option[T]]

	/** A factory and a matching pattern for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Option[T]]`
	  * instances. The kin produced here wrap [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` instances
	  * and can be matched with other extractors defined in [[net.noresttherein.oldsql.model.Kin$ Kin]] object.
	  * Other implementations - in particular lower level ones, using more specific information - exist and will
	  * be matched by neither this object nor the kin factories it creates.
	  * @define Ref `Supposed`
	  * @define Val `Option[T]`
	  */
	object Supposed extends SpecificKinType[Option] {
		implicit protected override def composition[T] :Option[T] ComposedOf T = ComposedOf.option
		override type Ref[T] = Supposed[T]

		/** Equivalent to `Supposed[T](None)`. */
		def none[T] :Supposed[T] = Derived.present[T, Option[T]](None)

		/** Equivalent to `Supposed[T](Some(value))`. */
		def some[T](value :T) :Supposed[T] = Derived.present[T, Option[T]](Some(value))
	}



	/** A representation of a ''to-many'' relationship with entity `T`. This includes both the inverse sides
	  * of foreign keys, as well as true ''many-to-many'' relationships - values for both cases behave in the same way,
	  * the implementation details being transparent, and can be used interchangeably. The framework will perform
	  * required load and cascade operations behind the scenes when it encounters a reference which cannot be written
	  * directly to the underlying table(s).
	  *
	  * It is a type alias for a generic [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Iterable[T]]`,
	  * which is the default collection type for value-based model classes which do not specify any particular ordering
	  * or explicitly require uniqueness. The added benefit over its `Kin[Iterable[T]]` supertype is that this type
	  * carries information about the (de)composition of its value type (which, as `Derived` is covariant,
	  * might have been specified originally as any concrete collection type), making it possible to use in contexts
	  * where no exact information about it are available. No implicit
	  * [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]] is required by its methods
	  * and gone is the risk of incompatible decompositions (for example, with an `IndexedSeq[E]` to `(Int, E)`).
	  * The drawback, aside from a less clean and fixed type, is that it enforces natural decomposition of
	  * collections into their elements (excluding decompositions with additional information, such as indices of
	  * elements from an `IndexedSeq`) and is limited to `Iterable` itself.
	  * @see [[net.noresttherein.oldsql.model.Kin.Many$]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSeq]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSet]]
	  * @see [[net.noresttherein.oldsql.model.Kin.Of]]
	  */ //consider: a special collection type and Kin for essentially a Set using entity id as equality
	type Many[T] = Derived[T, Iterable[T]]

	/** A factory and a matching pattern for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Iterable[T]]`
	  * kin. The kin produced here wrap [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` instances
	  * and can be matched with other extractors defined in [[net.noresttherein.oldsql.model.Kin$ Kin]] object.
	  * The expressiveness of that API should be sufficient for most cases, as it allows filters on joined tables
	  * (if the relationship is mapped to a property in the domain model), but do not translate directly to
	  * more complex ''many-to-many'' and non binary relationships, requiring either resolving, or generating additional
	  * DML statements when saved as parts of enclosing entities. All `Restraint` subexpressions are accepted
	  * by all method handling references, as long as the restraint domain - the entity type the filter works on -
	  * uniquely identifies underlying storage through the defined mapping schema.
	  * Other implementations - in particular lower level ones, using more specific information - exist and will
	  * be matched by neither this object nor the kin factories it creates.
	  * @define Ref `Many`
	  * @define Val `Iterable[T]`
	  */
	object Many extends SpecificKinType[Iterable] {
		implicit protected override def composition[T] :Iterable[T] ComposedOf T =
			cmp.asInstanceOf[Iterable[T] ComposedOf T]

		def empty[T] :Many[T] = none.asInstanceOf[Many[T]]

		private[this] val cmp = ComposedOf[Iterable[Any], Any]
		private[this] val none = Derived.empty[Any, Iterable[Any]]

		override type Ref[T] = Many[T]
	}



	/** A representation of a ''to-many'' relationship with entity `T`, mapped to `Seq[T]`. This includes both
	  * the inverse sides of foreign keys, as well as true ''many-to-many'' relationships - values for both cases behave
	  * in the same way, the implementation details being transparent, and can be used interchangeably. The framework
	  * will perform required load and cascade operations behind the scenes when it encounters a reference which cannot
	  * be written directly to the underlying table(s). The order in the sequence may be persistent, or undefined
	  * and different between application runs, depending on the details of the mapping producing the references
	  * and writing them to the database.
	  *
	  * It is a type alias for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Seq[T]]`
	  * and subtype of the more generic [[net.noresttherein.oldsql.model.Kin.Many! Many]]`[T]`. It is provided
	  * for convenience as one of the most common choices for plural relationship collections.
	  * The added benefit over its `Kin[Seq[T]]` supertype is that this type carries information about
	  * the (de)composition of its value type (which, as `Derived` is covariant,
	  * might have been specified originally as any concrete sequence type), making it possible to use in contexts
	  * where no exact information about it are available. This will be the default implicit
	  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Collection.apply Collection()]], but custom
	  * implementations can also be used, for example to sort the elements when reading/writing. This also means
	  * that different implicit composition type classes present at creation and usage point will not result
	  * in discrepancies.
	  * @see [[net.noresttherein.oldsql.model.Kin.Many]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSeq$]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSet]]
	  */
	type KinSeq[E] = Derived[E, Seq[E]]

	/** A factory and a matching pattern for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Seq[T]]`
	  * kin. The kin produced here wrap [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` instances
	  * and can be matched with other extractors defined in [[net.noresttherein.oldsql.model.Kin$ Kin]] object.
	  * Other implementations - in particular lower level ones, using more specific information - exist and will
	  * be matched by neither this object nor the kin factories it creates.
	  * As this type may be used to reflect many types of relationships between underlying tables,
	  * many of the handling details - in particular transient, present instances - are left to the mapping
	  * implementation used for this value type. All information required to update or create the referenced entities
	  * must be carried by the elements of the sequence; whether they are treated jointly,
	  * having a common key, or individually, depends again on the mapping of the component it is used for.
	  * The same goes for the order of elements in the sequence: it can be preserved, changed according
	  * to a predefined sorting comparisons, or completely ignored, with future reads returning the elements
	  * in any order. Maintaining data integrity of all objects in the reachable graph is the responsibility
	  * of the application; Inconsistencies involving bidirectional relationships may result in exceptions
	  * being thrown when discovered, or in unexpected behaviour. Note that a kin for an empty collection is still
	  * considered ''present'' according to the established semantics, as it is the whole `Seq[T]`
	  * which is the value type of the `KinSeq` kin, not the individual elements.
	  * @define Ref `KinSeq`
	  * @define Val `Seq[T]`
	  */
	object KinSeq extends SpecificKinType[Seq] {
		implicit protected override def composition[T] :Seq[T] ComposedOf T = cmp.asInstanceOf[Seq[T] ComposedOf T]
		def empty[T] :KinSeq[T] = none.asInstanceOf[KinSeq[T]]

		private[this] val cmp = ComposedOf[Seq[Any], Any]
		private[this] val none = Derived.empty[Any, Seq[Any]]

		override type Ref[T] = Derived[T, Seq[T]]
	}



	/** A representation of a ''to-many'' relationship with entity `T`, mapped to `Set[T]`. This includes both
	  * the inverse sides of foreign keys, as well as true ''many-to-many'' relationships - values for both cases behave
	  * in the same way, the implementation details being transparent, and can be used interchangeably. The framework
	  * will perform required load and cascade operations behind the scenes when it encounters a reference which cannot
	  * be written directly to the underlying table(s). The order of elements in the sequence is unspecified and will
	  * possibly be different for different reads of the same collection, especially for different application runs.
	  * While the exact `Set` implementation returned is left to the mapping for the relationship, it should be
	  * compatible with any `Set` subclasses. Without any extra information, this will almost always result in
	  * some form of a hash set being used. For this reason it is paramount that `equals` stays consistent with
	  * row equality of the underlying table; while this is generally not an issue for entity classes having
	  * a unique primary key/identifier, value-based classes must compare the values of all columns, and in the
	  * same manner as the database engine; this might come into play with international `String` characters,
	  * values which are mapped with a loss of precision or automatic corrections, and more complex types derived
	  * from the basic SQL types (for example, `URL` equality is not isomorphic with equality on `String`s passed
	  * to their constructors).
	  *
	  * It is a type alias for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Set[T]]`
	  * and subtype of the more generic [[net.noresttherein.oldsql.model.Kin.Many! Many]]`[T]`. It is provided
	  * for convenience as one of the most common choices for plural relationship collections.
	  * The added benefit over its `Kin[Set[T]]` supertype is that this type carries information about
	  * the (de)composition of its value type (which, as `Derived` is covariant,
	  * might have been specified originally as any concrete set type), making it possible to use in contexts
	  * where no exact information about it are available. This will be the default implicit
	  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Collection.apply Collection()]], but custom
	  * implementations can also be used, for example to sort the elements when reading/writing. This also means
	  * that different implicit composition type classes present at creation and usage point will not result
	  * in discrepancies.
	  * @see [[net.noresttherein.oldsql.model.Kin.Many]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSeq]]
	  * @see [[net.noresttherein.oldsql.model.Kin.KinSet$]]
	  */
	type KinSet[E] = Derived[E, Set[E]]

	/** A factory and a matching pattern for [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[T, Set[T]]`
	  * kin. The kin produced here wrap [[net.noresttherein.oldsql.model.Restraint Restraint]]`[T]` instances
	  * and can be matched with other extractors defined in [[net.noresttherein.oldsql.model.Kin$ Kin]] object.
	  * Other implementations - in particular lower level ones, using more specific information - exist and will
	  * be matched by neither this object nor the kin factories it creates. Applications will often deal with
	  * the simplest present implementations when creating new kin from scratch and using them to update
	  * relationship status between entities. The appropriate data must be carried by the set elements;
	  * whether they are treated jointly, having a common key, or individually, depends on the mapping of the component
	  * it is used for. Maintaining data integrity of all objects in the reachable graph is the responsibility
	  * of the application; Inconsistencies involving bidirectional relationships may result in exceptions
	  * being thrown when discovered, or in unexpected behaviour. Note that a kin for an empty collection is still
	  * considered ''present'' according to the established semantics, as it is the whole `Set[T]`
	  * which is the value type of the `KinSet` kin, not the individual elements.
	  * @define Ref `KinSet`
	  * @define Val `Set[T]`
	  */
	object KinSet extends SpecificKinType[Set] {
		implicit protected override def composition[T] :Set[T] ComposedOf T = cmp.asInstanceOf[Set[T] ComposedOf T]
		override type Ref[T] = Derived[T, Set[T]]

		def empty[T] :KinSet[T] = none.asInstanceOf[KinSet[T]]

		private[this] val cmp = ComposedOf[Set[Any], Any]
		private[this] val none = Derived.empty[Any, Set[Any]]
	}






	/** Factory and matcher for present `Kin`, i.e. those with an associated, computed value.
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Present {

		/** Create a present `Kin` containing the given value and no other information. */
		@inline def apply[T](value :T) :Kin[T] = new BarePresentKin[T](value)

		/** Create a present `Kin` for a ''to-one'' relationship. Returned kin decomposes its value to itself,
		  * declaring the argument type as the source entity.
		  */
		@inline def one[T](value :T) :One[T] = Derived.one(value)

		/** Create a present `Kin` containing the given collection. Returned object will be an instance
		  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]]`[E, T[E]]`, carrying the composition
		  * and decomposition information.
		  */
		def apply[T[X] <: Iterable[X] with IterableOps[X, T, T[X]], E](value :T[E]) :T Of E =
			Derived.many[E, T](value)(
				ComposableFrom.Collection.of[E]((value :IterableOps[E, T, T[E]]).iterableFactory)
			)

		/** Create a present, [[net.noresttherein.oldsql.model.Kin.Derived derived]] kin containing the given
		  * value, decomposable using the implicit `T CollectionOf E`.
		  */
		@inline def many[E, T](items :T)(implicit composition :T ComposedOf E) :Derived[E, T] =
			Derived.present(items)

		/** Create a ''present'' kin for an empty collection `T[_]` based on its companion object. */
		def empty[T[X]](collection :IterableFactory[T]) :T Of Nothing =
			Derived.empty[Nothing, T[Nothing]](ComposableFrom.Collection.of[Nothing](collection))

		/** Create a ''present'' kin for an empty collection type `T` consisting of entities `E`. */
		@inline def empty[E, T](implicit constructor :T ConstructFrom E) :Kin[T] = Derived.empty[E, T]

		/** Check if this `Kin` has a present value associated with it.
		  * @return `kin.toOption`.
		  */
		@inline def unapply[T](kin :Kin[T]) :Opt[T] = kin.opt


		@SerialVersionUID(KinVer)
		private class BarePresentKin[+T](override val get :T) extends Present[T]

		@SerialVersionUID(KinVer)
		private class PresentPropertiesKin[E, C, X, +T](outer :Kin[C], prop :PropertyPath[E, X], as :ComposableFrom[T, X])
		                                               (implicit decomposition :DecomposableTo[C, E])
			extends Derived[X, T] with Present[T]
		{
			implicit override def composition :ComposableFrom[T, X] = as //ComposableFrom.Properties(prop)(as)
			override val items = Some(decomposition(outer.get).map(prop.fun))
			override val get = as(items.get)
			override lazy val hashCode = get.hashCode * 31 + composition.hashCode

			override def ++[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) =
				Derived.present((composition.composer.builder ++= composition.decomposer(get) ++= items).result())

			override def ++:[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) =
				Derived.present((composition.composer.builder ++= items ++= composition.decomposer(get)).result())

			override def property[Z, U >: T](property :PropertyPath[U, Z]) :Derived[Z, Z] =
				Property.one(this, property)
		}

	}


	private[oldsql] trait Present[+T] extends Kin[T] { outer => //consider: having toOption as a val
		override type isPresent = true
		override def isPresent = true

		final override def toOption :Some[T] = Some(get)

		override def +[E, U >: T](item :E)(implicit composition :U CollectionOf E) =
			Derived.present((composition.composer.builder ++= composition.decomposer(get) += item).result())

		override def +:[E, U >: T](item :E)(implicit composition :U CollectionOf E) =
			Derived.present((composition.composer.builder += item ++= composition.decomposer(get)).result())

		override def ++[E, U >: T](items :Iterable[E])(implicit composition :U CollectionOf E) =
			if (items.isEmpty) this
			else Derived.present((composition.composer.builder ++= composition.decomposer(get) ++= items).result())

		override def ++:[E, U >: T](items :Iterable[E])(implicit composition :U CollectionOf E) =
			if (items.isEmpty) this
			else Derived.present((composition.composer.builder ++= items ++= composition.decomposer(get)).result())


		override def property[X, U >: T](property :PropertyPath[U, X]) :Kin[X] = Present(property(get))

		override def properties[E, X, C](prop :PropertyPath[E, X], as :ComposableFrom[C, X])
		                                (implicit decomposition :DecomposableTo[T, E]) :Derived[X, C] =
			new PresentPropertiesKin(this, prop, as)
	}




	/** Check if a `Kin` is absent, i.e. contains no value.
	  * There are two special absent kin types: [[net.noresttherein.oldsql.model.Kin.Unknown Unknown]], which is
	  * used when no information about the referenced value is known, particularly during updates, in order to
	  * exclude said kin from the updated columns, and [[net.noresttherein.oldsql.model.Kin.Nonexistent$ Nonexistent()]],
	  * which is used to signify that the referenced value does not exist. Other absent instances, particularly
	  * [[net.noresttherein.oldsql.model.Kin.Missing$ missing]] - as determined by
	  * their [[net.noresttherein.oldsql.model.Kin.isAbsent isAbsent]] and
	  * [[net.noresttherein.oldsql.model.Kin.isMissing isMissing]] methods - should contain information uniquely
	  * identifying the referenced value. The nature of this information is implementation dependent.
	  * Such absent instances can be created using [[net.noresttherein.oldsql.model.Kin.AllWhere AllWhere]]
	  * and [[net.noresttherein.oldsql.model.Kin.One One]].
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Absent {
		/** Checks if the matched kin doesn't contain a value.
		  * @return `kin.isAbsent`.
		  */
		@inline def unapply[T](kin :Kin[T]) :Boolean = kin.isAbsent
	}

	private[oldsql] trait Absent extends Kin[Nothing] {
		override def isPresent = false
		override def toOption = None
		override def toString = "Absent"
	}



	/** `Kin` about which values we don't know anything or don't care.
	  * It should be used very sparingly, as in most cases nothing sensible can be done with such an instance
	  * and passing it in a query will produce an error, but it can be useful to signify 'unchanged' in updated values,
	  * or save defining otherwise expensive to compute ''to-many'' relationships as `Option[Kin[T]]` - assuming
	  * this practice is shared by the whole codebase. All `Unknown` instances, including those extending
	  * [[net.noresttherein.oldsql.model.Kin.Derived Derived]] are equal.
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	final val Unknown :Unknown = new DerivedUnknown[Nothing]

	sealed trait Unknown extends Kin[Nothing] { this :Derived[_, Nothing] =>
		/** Casts this instance to `Derived[E, X]`. */
		def apply[E]() :Derived[E, Nothing] = this.asInstanceOf[Derived[E, Nothing]]

		override def isPresent = false
		override def isUnknown = true
		override def get :Nothing = throw new NoSuchElementException("Unknown")
		override def toOption :Option[Nothing] = None

		override def singleton[C[+X] <: Iterable[X]](factory :IterableFactory[C]) :Kin[Nothing] = this

		override def ++[E, X, U](items :Derived[E, X])(implicit composition :U CollectionOf E) :Nothing =
			throw new UnsupportedOperationException("Cannot combine Unknown kin with " + items + " into a single instance.")

		override def ++:[E, X, U](items :Derived[E, X])(implicit composition :U CollectionOf E) :Nothing =
			throw new UnsupportedOperationException("Cannot combine Unknown kin with " + items + " into a single instance.")

		override def property[X, U](property :PropertyPath[U, X]) :Kin[Nothing] = this
		override def properties[E, X, C](property :PropertyPath[E, X], as :C ComposableFrom X)
		                                (implicit decomposition :DecomposableTo[Nothing, E]) :Kin[Nothing] = this

		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this
		override def equals(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this
		override lazy val hashCode :Int = java.lang.System.identityHashCode(this)

	}

	@SerialVersionUID(KinVer)
	private final class DerivedUnknown[E] extends Derived[E, Nothing] with Unknown {
		override def composition :Nothing ComposableFrom E =  ComposableFrom.Nothing()
		override def items = None

		override def property[X, U](property :PropertyPath[U, X]) :Derived[X, Nothing] = this()
		override def properties[I, X, C](property :PropertyPath[I, X], as :ComposableFrom[C, X])
		                                (implicit decomposition :DecomposableTo[Nothing, I]) :Derived[X, Nothing] =
			this()

		private def readResolve() :Unknown = Unknown

		override def toString = "Unknown"
	}



	/** A factory and matcher for a special type of absent `Kin`: those which specify that the value is not merely
	  * absent in this instance, but does not exist at all. This special value allows model classes to do away
	  * with properties `Option[Kin[X]]` and `Kin[Option[X]]`, if they so wish.
	  * The ''absent''/''missing''/''nonexistent'' division allows to implement all these cases as simply `Kin[X]`.
	  * In the first case, all [[net.noresttherein.oldsql.model.Kin.Absent$ absent]] kin would always be either
	  * [[net.noresttherein.oldsql.model.Kin.Missing$ missing]] or nonexistent. In the latter, values would be
	  * initially defined simply as absent, and only substituted with nonexistent during their resolution.
	  * Note that, by default, [[net.noresttherein.oldsql.model.Kin.Derived Derived]] are never nonexistent,
	  * only [[net.noresttherein.oldsql.model.Kin.Derived$.empty empty]] (carrying an empty collection),
	  * as the distinction is rarely modeled in database schemas. This can however be changed with a custom
	  * [[net.noresttherein.oldsql.model.KinFactory KinFactory]].
	  * @see [[net.noresttherein.oldsql.model.Kin Kin]]
	  */
	object Nonexistent {
		/** A reusable kin instance signifying that the value in question does not exist.*/
		def apply() :Kin[Nothing] = instance

		/** A ''nonexistent'' kin, upcast to an arbitrary type `T`. */
		def as[T] :Kin[T] = instance

		/** Checks if the logical value referenced by the argument kin is known to not exist.
		  * @return `kin.isNonexistent`.
		  */
		def unapply(kin :Kin[_]) :Boolean = kin.isNonexistent

		private[this] object instance extends Nonexistent[Nothing] //an object for automatic serialization
	}

	private[oldsql] trait Nonexistent[+T] extends Kin[T] {
		final override def isPresent = false
		final override def isNonexistent = true
		final override def toOption :Option[T] = None

		override def singleton[C[+X] <: Iterable[X]](factory :IterableFactory[C]) :Kin[C[T]] =
			Derived.empty[T, C[T]](ComposableFrom.Collection.of[T](factory))

		override def ++[E, X, U >: Nothing](items :Derived[E, X])(implicit composition :U CollectionOf E) :Derived[E, U] =
			if (items.composition == composition.composer)
				items.asInstanceOf[Derived[E, U]] //this will always be false unless we override compatibleWith with a heuristic
			else Collective(items.parts.toList :_*)(composition.composer)

		override def ++:[E, X, U >: T](items :Derived[E, X])(implicit composition :U CollectionOf E) :Derived[E, U] =
			this ++ items

		override def properties[E, X, C](property :PropertyPath[E, X], as :ComposableFrom[C, X])
		                                (implicit decomposition :DecomposableTo[T, E]) :Kin[C] =
			Nonexistent()

		override def toString = "Nonexistent"
	}




	/** A matcher for a special type of absent `Kin`: those which specify that the value is merely
	  * missing from this instance, but does exist in the logical sense.
	  * Together with [[net.noresttherein.oldsql.model.Kin.Nonexistent$ Nonexistent]], it can model both
	  * `Option[Kin[T]]` and `Kin[Option[T]]` as simply `Kin[T]`, representing the cases of `Some[Kin[T]]`
	  * and `Kin[Some[T]]`. Missing kin are created in implementation dependent ways, typically by
	  * [[net.noresttherein.oldsql.model.KinFactory factories]], and should contain information sufficient
	  * for locating and loading the referenced value. For this reason, no factory method is present here.
	  */
	object Missing {
		/** Checks if the value referenced by the argument kin is known to exist, and only missing from this instance
		  * (that is, it simply isn't loaded).
		  * @return `kin.isMissing`.
		  */
		def unapply(kin :Kin[_]) :Boolean = kin.isMissing
	}

	private[oldsql] trait Missing extends Absent {
		final override def isMissing = true
		override def toString = "Missing"
	}




	/** A type alias for kin of a composite type `C[E]` derived from individual entities `E`.
	  * Due to accepting a type constructor for the collection type as its parameter in place of the whole,
	  * parameterized type (and a short name) it considerably declutters declaration of ''to-many'' collections,
	  * especially when written in the infix notation: compare `Derived[Spells, Seq[Spells]` with `Seq Of Spells`.
	  * @see [[net.noresttherein.oldsql.model.Kin.Derived]]
	  */
	type Of[C[_], E] = Derived[E, C[E]]

	/** A lower level [[net.noresttherein.oldsql.model.Kin Kin]] interface which is aware of how the referenced value `T`
	  * is composed of entities `E`. This of course applies to all collection types, but also `Option[E]` and `E` itself;
	  * it cannot be assumed that a composite kin can handle an arbitrary number of entities. Virtually all kin returned
	  * to the application will be actually `Derived`, and a `Derived` may be required to build the referenced value.
	  * Unlike properties/statuses mentioned in the documentation of `Kin`, all 'composite' kin are in fact
	  * instances of this trait.
	  *
	  * In most cases, applications will be better of with the simpler `Kin` interface,
	  * but there are some valid use cases when the use of this type is advised:
	  *   1. `Derived` kin cannot be [[net.noresttherein.oldsql.model.Kin.Nonexistent nonexistent]]:
	  *      if an instance should be able to represent a lack of value, it should be explicitly reflected in
	  *      the derived ('collection') type by using [[Option]] or a similar type as type argument for `T`.
	  *      This allows a distinction on the type level impossible with `Kin`.
	  *   1. The need for generic code on the application side, requiring the knowledge about thee relationship of
	  *      the value type and the underlying database entity.
	  *   1. Creating custom `Kin` implementations - in that case it is recommended to extend from `Derived` instead.
	  *
	  * In order to make working with this type somewhat more convenient as well as a self-documenting measure,
	  * several standard type aliases exist representing most common cases:
	  *   1. [[net.noresttherein.oldsql.model.Kin.One One]]`[E]` - a `Derived[E, E] for `''to-one'' relationships,
	  *   1. [[net.noresttherein.oldsql.model.Kin.Supposed Supposed]]`[E]` - a `Derived[E, Option[E]]`
	  *      for ''to-zero-or-one'' relationships,
	  *   1. [[net.noresttherein.oldsql.model.Kin.Many Many]]`[E]` - a `Derived[E, Iterable[E]]`
	  *      for generic ''to-many'' relationships,
	  *   1. `C `[[net.noresttherein.oldsql.model.Kin.Of Of]]` E` - a `Derived[E, C[E]]` in a shorter syntax,
	  *      avoiding listing the element type twice in ''to-many'' relationships.
	  *
	  * @tparam E referenced entity type. This can be both an application model class, and a tuple - for example
	  *           when referencing a `Map` or an `IndexedSeq`.
	  * @tparam T the value type of this kin, which can be assembled from individual values of `E` using its
	  *           [[net.noresttherein.oldsql.model.Kin.Derived.composition composition]].
	  * @see [[net.noresttherein.oldsql.model.Kin.Of]]
	  */ //consider: renaming; ItemKin? KinItems? Items?
	trait Derived[E, +T] extends Kin[T] { //somewhere here fits the future Unique
		/** Derived kin do not have `Nonexistent` instances. Instead, one should use
		  * either `Derived[E, Option[E]]` ([[net.noresttherein.oldsql.model.Kin.Supposed Supposed]]`[E]`)
		  * or `Option[Derived[E, E]]` (`Option[`[[net.noresttherein.oldsql.model.Kin.One One]]`[E]]`).
		  * @return `false`.
		  */ //was overriden only in EntityPropertyKin, and that as a forwarder to another instance
		final override def isNonexistent :Boolean = false

		/** All derived kin are assumed missing, but existing.
		  * @return `isAbsent`.
		  */
		override def isMissing :Boolean = !isPresent

		/** The element type - a type alias for the `E` type parameter. */
		type Element = E

		/** The factory and specification of how to create the referenced composite type `T` from values of some
		  * entity type `E`.
		  */
		def composition :T ComposableFrom E

		/** For present kin, convert (if required) their value `T` to a collection of `E`. Absent kin return `None`. */
		def items :Option[Iterable[E]]

		/** All kin, the combined values of which are used to create the value of this kin. Equivalent to calling
		  * [[net.noresttherein.oldsql.model.Kin.explode explode]] with a compatible decomposition type:
		  * by default it returns a singleton collection with this instance as its element, but
		  * [[net.noresttherein.oldsql.model.Kin.Collective$ Collective]] kin override it to return its constituents.
		  */
		def parts :Iterable[Derived[E, _]] = this::Nil

		/** Returns this kin if it uses `composition` compatible with the implicitly provided one, or an adapter
		  * performing the conversion by decomposing this instance and composing the result (if present).
		  */
		override def recompose[X, U >: T](implicit composition :U ComposedOf X) :Derived[X, U] =
			if (this.composition == composition.composer) this.asInstanceOf[Derived[X, U]]
			else Recomposed(this)(composition.decomposer, composition.composer)

		override def explode[X](implicit decomposition :T DecomposableTo X) :Option[Iterable[Derived[X, _]]] =
			if (decomposition compatibleWith composition) Some(parts.asInstanceOf[Iterable[Derived[X, _]]])
			else None


		override def +[I, U >: T](item :I)(implicit composition :U CollectionOf I) :Derived[I, U] =
			this ++[I, List[I], U] Derived.many(item::Nil)

		override def +[I, U >: T](item :Kin[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
			this ++[I, List[I], U] item.singleton(List)

		override def +:[I, U >: T](item :I)(implicit composition :U CollectionOf I) :Derived[I, U] =
			Derived.many(item::Nil) ++:[I, List[I], U] this

		override def +:[I, U >: T](item :Kin[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
			item.singleton(List) ++:[I, List[I], U] this

		override def ++[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
			this ++[I, Iterable[I], U] Derived.many(items)

		override def ++[I, X, U >: T](items :Kin[X])
		                             (implicit composition :U CollectionOf I, decomposition :X DecomposableTo I) :Derived[I, U] =
			items match {
				case composite :Derived[I @unchecked, X @unchecked]
					if composite.composition compatibleWith decomposition => this ++[I, X, U] composite
				case _ => this ++[I, U, U] Recomposed(items)(decomposition, composition.composer)
			}

		override def ++[I, X, U >: T](items :Derived[I, X])(implicit composition :U CollectionOf I) :Derived[I, U] = {
			val self =
				if (this.composition compatibleWith composition.decomposer)
					parts.asInstanceOf[Iterable[Derived[I, _]]]
				else
					Recomposed(this)(composition.decomposer, composition.composer)::Nil
			Collective(self ++: items.parts.toSeq :_*)(composition.composer)
		}

		override def ++:[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
			Derived.many(items) ++:[I, Iterable[I], U] this

		override def ++:[I, X, U >: T](items :Kin[X])
		                              (implicit composition :U CollectionOf I, decomposition :X DecomposableTo I) :Derived[I, U] =
			items match {
				case composite :Derived[I @unchecked, X @unchecked]
					if composite.composition compatibleWith decomposition => composite ++:[I, X, U] this
				case _ => Recomposed(items)(decomposition, composition.composer) ++:[I, U, U] this
			}

		override def ++:[I, X, U >: T](items :Derived[I, X])(implicit composition :U CollectionOf I) :Derived[I, U] = {
			val self =
				if (this.composition compatibleWith composition.decomposer)
					parts.toSeq.asInstanceOf[Seq[Derived[I, _]]]
				else
					Recomposed(this)(composition.decomposer, composition.composer)::Nil
			Collective(items.parts ++: self :_*)(composition.composer)
		}


		override def property[X, U >: T](property :PropertyPath[U, X]) :Derived[X, X] =
			Property.one(this, property)

		override def properties[I, X, C[_]](property :PropertyPath[I, X], in :IterableFactory[C])
		                                   (implicit decomposition :T DecomposableTo I) :Derived[X, C[X]] =
			properties(property, ComposableFrom.Collection.of[X](in))

		override def properties[I, X, C[_], Ev[_]](property :PropertyPath[I, X], in :EvidenceIterableFactory[C, Ev])
		                                          (implicit decomposition :T DecomposableTo I, ev :Ev[X])
				:Derived[X, C[X]] =
			properties(property, ComposableFrom.Collection.of[X](in))

		@throws[IncompatibleElementTypeException](
			"if this.composition is incompatible with decomposition (that is, type I of intended elements doesn't match" +
			" type E of elements of this instance."
		)
		override def properties[I, X, C](property :PropertyPath[I, X], as :C ComposableFrom X)
		                                (implicit decomposition :T DecomposableTo I) :Derived[X, C] =
			if (decomposition compatibleWith composition)
				Property(this.asInstanceOf[Derived[I, T]], property)(as)
			else
				throw new IncompatibleElementTypeException(
					s"Cannot create a Kin for property $property of elements of $this: decomposition $decomposition " +
					"doesn't match this kin's composition."
				)


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Derived[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :Derived[_, _] if other canEqual this =>
				composition == other.composition &&
					(if (isPresent) other.isPresent && get == other.get else other.isAbsent)
			case _ => false
		}

		override lazy val hashCode :Int =
			if (isAbsent) composition.hashCode else get.hashCode * 31 + composition.hashCode

		override def toString :String = toOption match {
			case Some(x) => "Present[" + composition + "](" + x + ")"
			case _ if isMissing => "Missing[" + composition + "]"
			case _ if isNonexistent => "Nonexistent[" + composition + "]"
			case _ => "Absent[" + composition + "]"
		}
	}


	/** A factory and matcher of [[net.noresttherein.oldsql.model.Kin.Derived Derived]]:
	  * [[net.noresttherein.oldsql.model.Kin Kin]] subtype carrying information about the composition of its value
	  * from individual entities.
	  * @see [[net.noresttherein.oldsql.model.Kin.Collective$]]
	  */
	object Derived {
		type Of[E] = { type Kin[T] = Derived[E, T] }

		/** A ''present'', composite kin for an empty collection type `T` using the given implicit composition. */
		def empty[E, T](implicit constructor :T ConstructFrom E) :Derived[E, T] = new EmptyKin[E, T]

		/** A present kin for the given standard collection type and its natural decomposition into its elements. */
		def many[E, T[X] <: Iterable[X]](items :T[E])(implicit constructor :T[E] ConstructFrom E)
				:Derived[E, T[E]] =
			new PresentIterable(items)

		/** A present kin for an arbitrary composite type `T` using the implicit composition `T CollectionOf E`.
		  * Note that the collection may be empty (or `None`) and the kin will still be considered ''present'' -
		  * what matters is that it contains all referenced entities.
		  */
		def present[E, T](items :T)(implicit composition :T ComposedOf E) :Derived[E, T] =
			new PresentBase[E, T](items)(composition.decomposer, composition.composer) with PresentOverrides[E, T]

		/** A present kin for a singular composite kin - a kin which value is decomposed to itself. */
		def one[T](item :T) :Derived[T, T] = present(item)

		/** A lazy, present `Derived` instance with the value of the given by-name parameter. */
		def delay[E, T](value: => T)(implicit composite :T ComposedOf E) :Derived[E, T] =
			new LazyDerived(value)


		/** Matches `Derived[E, T]` instances, checking that it's `composition` is compatible with the implicit
		  * `decomposition`. If you wish to match all `Derived` instances instead, you should do so by its type instead.
		  */
		def unapply[E, T](kin :Kin[T])(implicit decomposition :T DecomposableTo E) :Opt[Derived[E, T]] =
			kin match {
				case derived :Derived[E @unchecked, T @unchecked] if derived.composition compatibleWith decomposition =>
					Got(derived)
				case _ => Lack
			}



		@SerialVersionUID(KinVer)
		private[oldsql] class PresentBase[E, +T](override val get :T)(implicit decomposition :T DecomposableTo E,
		                                                              override val composition :T ComposableFrom E)
			extends Derived[E, T]
		{
			override def items :Some[Iterable[E]] = Some(decomposition(get))
			override def toOption :Option[T] = Some(get)
		}

		private trait PresentOverrides[E, +T] extends Derived[E, T] with Present[T] {
			override def ++[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
				if (items.isEmpty && (composition.decomposer compatibleWith this.composition))
					this.asInstanceOf[Derived[I, U]]
				else Derived.present((composition.composer.builder ++= composition.decomposer(get) ++= items).result())

			override def ++:[I, U >: T](items :Iterable[I])(implicit composition :U CollectionOf I) :Derived[I, U] =
				if (items.isEmpty && (composition.decomposer compatibleWith this.composition))
					this.asInstanceOf[Derived[I, U]]
				else Derived.present((composition.composer.builder ++= items ++= composition.decomposer(get)).result())

			override def property[X, U >: T](property :PropertyPath[U, X]) :Derived[X, X] =
				Property.one(this, property)
		}

		@SerialVersionUID(KinVer)
		private class PresentIterable[E, +T <: Iterable[E]]
		                             (override val get :T)(implicit override val composition :T ConstructFrom E)
			extends Derived[E, T] with PresentOverrides[E, T]
		{
			override def items :Option[T] = Some(get)
		}

		@SerialVersionUID(KinVer)
		private class EmptyKin[E, +T](implicit override val composition :T ConstructFrom E) extends Derived[E, T] {
			def this(factory :Factory[E, T]) = this()(ComposableFrom.Collection()(factory))

			override def isPresent = true
			override val toOption = Some(composition(None))
			override val items = Some(None)
			override def parts :Iterable[Derived[E, _]] = Nil
		}

		@SerialVersionUID(KinVer)
		private[oldsql] class AbsentBase[E, +T](override val isMissing :Boolean = false)
		                                       (implicit override val composition :T ComposableFrom E)
			extends Derived[E, T]
		{
			override def isPresent = false
			override def toOption :Option[T] = None
			override def items :Option[Iterable[E]] = None
		}

		@SerialVersionUID(KinVer)
		private class RecomposedKin[E, X, +T](val kin :Kin[X])(implicit val decomposition :X DecomposableTo E,
		                                                       override val composition :T ComposableFrom E)
			extends Derived[E, T]
		{
			override def isMissing = kin.isMissing
			override def isPresent = kin.isPresent
			override def items :Option[Iterable[E]] = kin.toOption.map(decomposition.apply)
			override lazy val toOption = kin.toOption.map(x => composition(decomposition(x)))
		}

		@SerialVersionUID(KinVer)
		private[oldsql] class LazyDerived[E, +T](value: => T)(implicit composite :T ComposedOf E)
			extends Delayed(() => Some(value)) with Derived[E, T]
		{
			override def composition = composite.composer
			override def items = Some(composite.decomposer(get))

			private def writeReplace = toOption match {
				case Some(t) => Derived.present(t)
				case _ => Unknown
			}
		}

	}



	/** Factory and matchers of `Kin` adapters which optionally map its value by decomposing it into individual
	  * elements and composing using a new [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom composition]].
	  */
	object Recomposed {

		/** An adapter of an arbitrary kin for a value decomposable to values of `E` to a `Derived[E, T]`. */
		def apply[E, X, T](kin :Kin[X])(implicit decomposition :X DecomposableTo E, constructor :T ComposableFrom E)
				:Derived[E, T] =
			if (kin.isUnknown)
				throw new IllegalArgumentException("Unknown kin cannot be recomposed.")
			else if (kin.isNonexistent)
				throw new IllegalArgumentException(s"Nonexistent kin $kin cannot be recomposed.")
			else new Recomposed(kin)

		/** Matches kin adapter created by [[net.noresttherein.oldsql.model.Kin.Recomposed.apply Recomposed(kin)]].
		  * @return the adapted kin, with its decomposition to the element type and the composition used for creating
		  *         the desired value.
		  */
		def unapply[E, T](kin :Derived[E, T]) :Opt[(Kin[X], X DecomposableTo E, T ComposableFrom E) forSome { type X }] =
			kin match {
				case adapter :Recomposed[E, x, T] => Got((adapter.kin, adapter.decomposition, adapter.composition))
				case _ => Lack
			}

		/** Matches kin adapter created by [[net.noresttherein.oldsql.model.Kin.Recomposed.apply Recomposed(kin)]].
		  * @return the adapted kin, with its decomposition to the element type and the composition used for creating
		  *         the desired value.
		  */
		def unapply[T](kin :Kin[T]) :Opt[(Kin[X], X DecomposableTo E, T ComposableFrom E) forSome { type X; type E }] =
			kin match {
				case adapter :Recomposed[e, x, T] => Got((adapter.kin, adapter.decomposition, adapter.composition))
				case _ => Lack
			}

	}

	@SerialVersionUID(KinVer)
	private class Recomposed[E, X, +T](val kin :Kin[X])(implicit val decomposition :X DecomposableTo E,
	                                                    override val composition :T ComposableFrom E)
		extends Derived[E, T]
	{
//			override def isNonexistent :Boolean = kin.isNonexistent
		override def isMissing = kin.isMissing
		override def isPresent = kin.isPresent
		override def items :Option[Iterable[E]] = kin.toOption.map(decomposition.apply)
		override lazy val toOption = kin.toOption.map(x => composition(decomposition(x)))
	}



	/** A factory and matcher of 'collective' kin: [[net.noresttherein.oldsql.model.Kin.Derived Derived]]
	  * which constitute of other - typically more than one - kin.
	  */
	object Collective {
		//		def apply(kin :Iterable[Kin[E]])
		/** A composite kin for `T`, built from all the elements of the decomposed values of the argument kin. */
		def apply[E, T](kin :Derived[E, _]*)(implicit composition :T ConstructFrom E) :Derived[E, T] =
			if (kin.exists(_.isUnknown))
				throw new IllegalArgumentException(
					"Unknown cannot be combined with other kin to form a collective kin: "+kin + "."
				)
			else new Collective(kin)

		/** Extracts the [[net.noresttherein.oldsql.model.Kin.Derived.parts parts]] of the argument kin,
		  * matching all `Derived`.
		  * @return `Some(kin.parts)`.
		  */
		def unapply[E, T](kin :Derived[E, T]) :Opt[Iterable[Derived[E, _]]] = Got(kin.parts)

		/** Matches all [[net.noresttherein.oldsql.model.Kin.Derived Derived]] with composition matching
		  * the implicit decomposition, extracting its [[net.noresttherein.oldsql.model.Kin.Derived.parts parts]].
		  * Most derived kin yield themselves as a singleton collection,
		  * but [[net.noresttherein.oldsql.model.Kin.Collective$ collective]] kin
		  * result in all their individual constituents instead.
		  * @return `kin.`[[net.noresttherein.oldsql.model.Kin.explode explode]].
		  */
		def unapply[E, T](kin :Kin[T])(implicit decomposition :T DecomposableTo E) :Opt[Iterable[Derived[E, _]]] =
			kin.explode[E]
	}


	@SerialVersionUID(KinVer)
	private class Collective[E, +T](kin :Iterable[Derived[E, _]])
	                               (implicit override val composition :T ConstructFrom E)
		extends Derived[E, T]
	{
		override def isPresent = kin.forall(_.isPresent)
		override def toOption :Option[T] = if (isAbsent) None else Some(composition(kin.flatMap(_.items.get)))
		override def items :Option[Iterable[E]] = if (isAbsent) None else Some(kin.flatMap(_.items.get))
		override def parts :Iterable[Derived[E, _]] = kin

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Collective[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case collective :Collective[_, _] if collective canEqual this =>
				collective.composition == composition && collective.toOption == toOption && collective.parts == parts
			case _ => false
		}

		override lazy val hashCode :Int = kin.hashCode * 31 + composition.hashCode

		override def toString = kin.mkString("Kin[" + composition + "](", ", ", ")")
	}




	/** Creates lazy `Kin` instances - arguments of passed methods are evaluated only when the returned instance is accessed.
	  * Passed expressions should be thread safe and idempotent, as there is no guarantee they'll be executed at most once
	  * when the returned instance is evaluated.
	  */
	object Delayed {
		/** Create a present `Kin` with a lazy value. Argument will be evaluated when `toOption` (or `get`) method
		  * of the returned `Kin` as called.
		  * @param value a pass-by-name expression evaluating to the value of the `Kin`, which should be idempotent and thread safe.
		  */
		def apply[T](value: => T) :Kin[T] = new Delayed(() => Some(value))
	}


	@SerialVersionUID(KinVer)
	private[oldsql] class Delayed[+T](resolve : () => Option[T]) extends Kin[T] {
		if (resolve == null) throw new NullPointerException("Null initializer of a LazyKin.")

		@volatile @transient private[this] var eval = resolve
		@volatile private[this] var value :Option[T] = _
		private[this] var cache :Option[T] = _

		def toOption :Option[T] = {
			if (cache != null) {
				val v = value
				if (v != null)
					cache = v
				else {
					val init = eval
					if (init == null) {
						if (cache == null)
							cache = value
					} else {
						val res = init()
						cache = if (res == null) None else res
						value = cache
						eval = null
					}
				}
			}
			cache
		}

		override def properties[E, X, C](property :PropertyPath[E, X], as :ComposableFrom[C, X])
		                                (implicit decomposition :DecomposableTo[T, E]) :Kin[C] =
			toOption match {
				case Some(t) => Present(as(decomposition(t).view.map(property.fun)))
//				case _ if isMissing => Missing
				case _ => Unknown
			}

		private def writeReplace = toOption match {
			case Some(t) => Present(t)
			case _ => Unknown
		}

		override def toString :String = if (value == null) "Delayed(?)" else super.toString
	}






	/** A factory and matcher for [[net.noresttherein.oldsql.model.Kin kin]] for values of properties of other kin.
	  * It uses [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Property Property]] and
	  * [[net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.Properties Properties]] compositions which
	  * simply apply the given property function to another entity, or to all individual elements the argument kin
	  * decomposes to, collecting the result. It is unique in that this process is obviously not reversible and
	  * there is no matching [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]]. This means
	  * that having a value of a certain property typically tells one nothing about which entity it came from,
	  * and some methods of the [[net.noresttherein.oldsql.model.Kin.Derived Derived]] and
	  * [[net.noresttherein.oldsql.model.GenericKinFactory GenericKinFactory]] come with restrictions.
	  * Instances created by this factory (and, indirectly, by [[net.noresttherein.oldsql.model.Kin.property property]]
	  * and [[net.noresttherein.oldsql.model.Kin.properties properties]] methods of `Kin`) retain a reference to
	  * the kin of the owning entity (the function argument), which can be extracted by this object's
	  * [[net.noresttherein.oldsql.model.Kin.Property.unapply unapply]] method.
	  */ //consider: renaming to ByProperty
	object Property {
		/** Create a kin for a collection of values of property `property :P` of all individual elements `E`
		  * comprising the composite type `X` from kin `owners`.
		  */
		@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
		def apply[E, X, P, T](owners :Kin[X], property :PropertyPath[E, P])
		                     (implicit decomposition :X ComposedOf E, composition :T ComposableFrom P)
				:Derived[P, T] =
			if (owners.isUnknown)
				throw new IllegalArgumentException(s"Cannot create a kin for property $property of Unknown.")
			else
				apply(owners.recompose, property)

		/** Create a kin for a collection of values of property `property :P` of all individual elements `E`
		  * comprising the composite type `X` from kin `owners`.
		  */
		@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
		def apply[E, X, P, T](owners :Derived[E, X], property :PropertyPath[E, P])
		                     (implicit composition :T ComposableFrom P) :Derived[P, T] =
			if (owners.isUnknown)
				throw new IllegalArgumentException(s"Cannot create a kin for property $property of Unknown.")
			else
				new Property[E, X, P, T](owners, property)

		/** Create a kin for a collection of values of property `property :P` of all individual elements `E`
		  * comprising the composite type `X` from kin `owners`.
		  */
		@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
		def apply[E, X, P, T](owners :Derived[E, X], property :PropertyPath[E, P], value :Option[T])
		                     (implicit composition :T ComposableFrom P) :Derived[P, T] =
			if (owners.isUnknown)
				throw new IllegalArgumentException(s"Cannot create a kin for property $property of Unknown.")
			else
				new Property[E, X, P, T](owners, property, value)

		/** Create a kin for the value of property `property` of the value `E` of kin `owner`. */
		@throws[IllegalArgumentException]("If `owner` is Unknown or Nonexistent.")
		def one[E, T](owner :Kin[E], property :PropertyPath[E, T]) :One[T] =
			apply(owner, property)

		/** Create a lazy kin for a collection of values of property `property :P` of all individual elements `E`
		  * comprising the composite type `X` from kin `owners`.
		  */
		@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
		def delay[E, X, P, T](owners :Derived[E, X], property :PropertyPath[E, P], value: => Option[T])
		                     (implicit composition :T ComposableFrom P) :Derived[P, T] =
			if (owners.isUnknown)
				throw new IllegalArgumentException(s"Cannot create a kin for property $property of Unknown.")
			else
				new Property[E, X, P, T](owners, property) {
					override lazy val toOption = value orElse owner.items.map {
						items => this.composition(items.view.map(property.fun))
					}
					private def writeReplace = new Property(owners, property, toOption)(this.composition)
				}

		/** Create a lazy kin for the value of property `property` of the value `E` of kin `owner`. */
		@throws[IllegalArgumentException]("If `owners` is Unknown or Nonexistent.")
		def delay[E, T](owner :Kin[E], property :PropertyPath[E, T], value: => Option[T]) :Derived[T, T] =
			if (owner.isNonexistent)
				throw new IllegalArgumentException(s"Cannot create a kin for property $property of Nonexistent.")
			else
				delay[E, E, T, T](owner.recompose, property, value)

		/** Matches kin instances created by this factory (or, indirectly,
		  * by [[net.noresttherein.oldsql.model.Kin.property property]] and
		  * [[net.noresttherein.oldsql.model.Kin.properties properties]] methods of `Kin`).
		  * Returns the kin for the owner(s) of the property, the property itself, and composition type class
		  * for assembling the result type ''from the value(s) of the property'' (which is not the same as
		  * [[net.noresttherein.oldsql.model.Kin.Derived.composition composition]] property of `Derived`).
		  */
		def unapply[P, T](kin :Derived[P, T])
				:Opt[(Derived[E, _], PropertyPath[E, P], T ComposableFrom P)] forSome { type E } =
			kin match {
				case prop :Property[e, x, P, T] => Got((prop.owner, prop.property, prop.composition))
				case _ => Lack
			}

		/** Matches kin instances created by this factory (or, indirectly,
		  * by [[net.noresttherein.oldsql.model.Kin.property property]] and
		  * [[net.noresttherein.oldsql.model.Kin.properties properties]] methods of `Kin`).
		  * Returns the kin for the owner(s) of the property, the property itself, and composition type class
		  * for assembling the result type ''from the value(s) of the property'' (which is not the same as
		  * [[net.noresttherein.oldsql.model.Kin.Derived.composition composition]] property of `Derived`).
		  */
		def unapply[T](kin :Kin[T])
				:Opt[(Derived[E, _], PropertyPath[E, P], T ComposableFrom P) forSome { type E; type P }] =
			kin match {
				case prop :Property[e, x, p, T] => Got((prop.owner, prop.property, prop.composition))
				case _ => Lack
			}


		/** A factory of references to the value of property `property` of entity `E` referenced by `Kin` handled
		  * by `factory`. Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin Kin]]`[P]`,
		  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `P`
		  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
		  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
		  * a supertype of the 'real' factory [[net.noresttherein.oldsql.model.Kin.Property.required required]],
		  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
		  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin instead of throwing
		  * a `NonexistentEntityException` as the latter one.
		  * @see [[net.noresttherein.oldsql.model.Kin.Property.required[K,E:TypeTag,P,Y]* apply]]`(factory, property)`.
		  */
		def apply[K, E :TypeTag, P, Y](factory :DerivedKinFactory[K, E, Y], property :E => P) :KinFactory[K, P, P] =
			new PropertyValuesKinFactory[K, E, Y, P, P](factory, PropertyPath(property))

		/** A factory of references to the value of property `property` of entity `E` referenced by `Kin` handled
		  * by `factory`. Returned factory creates instances of [[net.noresttherein.oldsql.model.Kin.One One]]`[P]`,
		  * but the value type can be changed to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `P`
		  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
		  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
		  * and all obtained through it by adapting to other composite types, create only instances
		  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
		  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
		  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
		  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
		  * @see [[net.noresttherein.oldsql.model.Kin.Property.apply[K,E:TypeTag,P,Y]* apply]]`(factory, property)`.
		  */
		def required[K, E :TypeTag, P, Y](factory :DerivedKinFactory[K, E, Y], property :E => P)
				:DerivedKinFactory[K, P, P] =
			new PropertyValuesDerivedFactory[K, E, Y, P, P](factory, PropertyPath(property))



		@SerialVersionUID(KinVer)
		private class PropertyValuesKinFactory[K, E, X, P, T]
		                                      (entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P])
		                                      (implicit override val result :T ComposedOf P, entity :TypeTag[E])
			extends BaseKinFactory[K, P, T]
		{
			private val argType = typeOf[E]

			override def delay(key :K, value : => Option[T]) :Derived[P, T] =
				Property.delay(entities.absent(key), property, value)

			override def apply(key :K, value :Option[T]) :Derived[P, T] =
				Property(entities.absent(key), property, value)

			override def absent(key :K) :Derived[P, T] = Property(entities.absent(key), property)
			override def missing(key :K) :Derived[P, T] = Property(entities.missing(key), property)

			override def keyFrom(item :P) :Opt[K] = Lack

			override def keyOf(kin :Kin[T]) :Opt[K] = kin match {
				//owner.Element <:< prop.definedFor, so elements are safe to pass of as E even if owner is not (invariance)
				case Property(owner :Derived[E @unchecked, _], prop, _) //unchecked type not necessarily true, but^
					if prop.definedFor <:< argType && property == prop => owner.items match {
					case Some(items) => entities.keyFrom(items)
					case _ => Lack
				}
				case _ => Lack
			}

			override def required :DerivedKinFactory[K, P, T] =
				new PropertyValuesDerivedFactory(entities, property)

			override def notRequired :KinFactory[K, P, T] =
				if (isRequired) new PropertyValuesKinFactory(entities, property) else this

			override def as[Y](implicit composition :Y ComposedOf P) :DerivedKinFactory[K, P, Y] =
				new PropertyValuesDerivedFactory[K, E, X, P, Y](entities, property)


			override def equivalencyToken :Any = (entities, property, result)

			final private def friendEntities = entities
			final private def friendProperty = property

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[PropertyValuesKinFactory[_, _, _, _, _]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case other :PropertyValuesKinFactory[_, _, _, _, _] if canEqual(other) && (other canEqual this) =>
					argType =:= other.argType && property == other.friendProperty &&
						result == other.result && entities == other.friendEntities
				case _ => false
			}

			override def hashCode :Int = (entities.hashCode * 31 + property.hashCode) * 31 + result.hashCode

			override def toString :String =
				result.toString + (if (isRequired) "(" else "?(") + entities + "." + property + ")"
		}



		@SerialVersionUID(KinVer)
		private class PropertyValuesDerivedFactory[K, E, X, P, T]
		                                          (entities :DerivedKinFactory[K, E, X], property :PropertyPath[E, P])
		                                          (implicit override val result :T ComposedOf P, entity :TypeTag[E])
			extends PropertyValuesKinFactory(entities, property) with BaseDerivedKinFactory[K, P, T]

	}



	/** A kin for a property `T` (possibly a chained one) of an entity `E` referenced by another kin. */
	@SerialVersionUID(KinVer)
	private class Property[E, X, P, +T](val owner :Derived[E, X], val property :PropertyPath[E, P],
	                                    value :Option[T])
	                                   (implicit val composition :T ComposableFrom P)
		extends Derived[P, T]
	{ //consider: having a TypeTag[E]
		def this(owner :Derived[E, X], property :PropertyPath[E, P])(implicit composition :T ComposableFrom P) =
			this(owner, property, owner.items.map { items => composition(items.view.map(property.fun)) })

		override def isPresent :Boolean = owner.isPresent
		override def isMissing :Boolean = owner.isMissing
		override def items :Option[Iterable[P]] = owner.items.map(_.map(property.fun))
		override def toOption :Option[T] = value

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Property[_, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case prop :Property[_, _, _, _] if prop canEqual this =>
				prop.composition == prop.composition && prop.property == property && prop.owner == owner
			case _ => false
		}

		override lazy val hashCode :Int = (owner.hashCode * 31 + property.hashCode) * 31 + composition.hashCode

		override def toString :String = owner.toString + ".properties[" + composition + "](_." + property + ")"
	}






	/** A kin for a collection `T` of entities `E` satisfying
	  * a given [[net.noresttherein.oldsql.model.Restraint Restraint]]`[E]`.
	  * @author Marcin Mościcki
	  */
	trait Restrained[E, +T] extends Derived[E, T] {
		val where :Restraint[E]

		override def get :T = toOption match {
			case Some(item) => item
			case _ => throw new AbsentKinException(this)
		}

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[Restrained[_, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :Restrained[_, _] if other canEqual this =>
				composition == other.composition && where == other.where && toOption == other.toOption
			case _ => false
		}

		override lazy val hashCode :Int = (where.hashCode * 31 + toOption.hashCode) * 31 + composition.hashCode

		override def toString :String = toOption match {
			case Some(x) => "Present{" + where + "}(" + x + ")"
			case _ if where == True => "All[" + composition + "]"
			case _ if isMissing => "Missing[" + composition + "]{" + where + "}"
			case _ if isNonexistent => "Nonexistent[" + composition + "]{" + where + "}"
			case _ => "Absent[" + composition + "]{" + where + "}"
		}
	}



	/** Factory and matcher for Kin specifying the referenced values through reflected filter functions of `E`.
	  * Each [[net.noresttherein.oldsql.model.Kin.Restrained Restrained]]`[E, T]`
	  * carries a [[net.noresttherein.oldsql.model.Restraint Restraint]]`[E]`. This object contains various
	  * direct factory methods for creating all kinds of restrained kin, but applications may be better served
	  * with the quasi-DSL of object [[net.noresttherein.oldsql.model.Kin.AllWhere AllWhere]].
	  */
	object Restrained {
		type Of[E] = { type Kin[T] = Restrained[E, T] }

		/** A restrained kin using the given `Restraint` as the filter specifying the referenced values. */
		def apply[E, T](where :Restraint[E], value :Option[T])(implicit as :T ComposedOf E) :Restrained[E, T] =
			if (value.isDefined) new PresentRestrained[E, T](where, value)
			else new AbsentRestrained(where)(as.composer)

		/** A restrained kin using the given `Restraint` as the filter specifying the referenced values. */
		def apply[E, T](where :Restraint[E])(implicit composition :T ComposableFrom E) :Restrained[E, T] =
			new AbsentRestrained(where)

		/** A ''present'' restrained carrying `value` kin using the given `Restraint` as the filter specifying
		  * the referenced values. */
		def present[E, T](where :Restraint[E], value :T)(implicit as :T ComposedOf E) :Restrained[E, T] =
			new PresentRestrained[E, T](where, value)

		/** A ''missing'' (an empty, but referencing values which are assumed to exist) restrained kin using
		  * the given `Restraint` as the filter specifying the referenced values. This kin has a strictly narrower
		  * meaning than [[net.noresttherein.oldsql.model.Kin.Restrained.absent Restrained.absent]].
		  */
		def missing[E, T](where :Restraint[E])(implicit composition :T ComposableFrom E) :Restrained[E, T] =
			new AbsentRestrained[E, T](where)

		/** An ''absent'' (empty) restrained kin using the given `Restraint` as the filter specifying
		  * the referenced values.
		  */
		def absent[E, T](where :Restraint[E])(implicit composition :T ComposableFrom E) :Restrained[E, T] =
			new AbsentRestrained[E, T](where, false)

		/** A restrained kin using the given `Restraint` as the filter specifying the referenced values. Kin value
		  * will be lazily computed by evaluating the given ''by-name'' expression `value`. */
		def delay[E, T](where :Restraint[E], value: => Option[T])(implicit as :T ComposedOf E) :Restrained[E, T] =
			new LazyRestrained(where, () => value)

		/** Matches all instances of [[net.noresttherein.oldsql.model.Kin.Restrained Restrained]] kin,
		  * created by this factory object, [[net.noresttherein.oldsql.model.Kin.AllWhere AllWhere]] or client code.
		  */
		def unapply[E, T](kin :Derived[E, T]) :Opt[(Restraint[E], T ComposableFrom E)] = kin match {
			case restrained :Restrained[E, T] => Got((restrained.where, restrained.composition))
			case _ => Lack
		}

		/** Matches all instances of [[net.noresttherein.oldsql.model.Kin.Restrained Restrained]] kin,
		  * created by this factory object, [[net.noresttherein.oldsql.model.Kin.AllWhere AllWhere]] or client code.
		  */
		def unapply[T](kin :Kin[T]) :Opt[(Restraint[E], T ComposableFrom E) forSome { type E }] = kin match {
			case restrained :Restrained[e, T] => Got((restrained.where, restrained.composition))
			case _ => Lack
		}


		/** A factory of [[net.noresttherein.oldsql.model.Kin.Restrained Restrained]] kin referencing all values
		  * satisfying a given [[net.noresttherein.oldsql.model.Restraint Restraint]] (a domain model-level expression
		  * serving as a filter on the entity type `E`). Returned factory creates instances of
		  * [[net.noresttherein.oldsql.model.Kin Kin]]`[E]`, but the value type can be changed
		  * to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
		  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
		  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. Note that this factory is not simply
		  * a supertype of the 'real' factory [[net.noresttherein.oldsql.model.Kin.Restrained.required required]],
		  * as it is ''not required'' - its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]]
		  * method returns [[net.noresttherein.oldsql.model.Kin.Nonexistent Nonexistent]] kin (which is ''not''
		  * a `Restrained` instance) instead of throwing a `NonexistentEntityException` as the latter one.
		  * @param restrainer a factory of instances of [[net.noresttherein.oldsql.model.Restraint Restraint]]`[E]`,
		  *                   which can be obtained from factory objects in
		  *                   [[net.noresttherein.oldsql.model.Restraint$ Restraint]] object, such as
		  *                   [[net.noresttherein.oldsql.model.Restraint.Equal Equal]], or - if
		  *                   `net.noresttherein.oldsql.model.implicits._` are imported - by calling one of the
		  *                   factory methods in [[net.noresttherein.oldsql.model.Restrictive Restrictive]],
		  *                   such as [[net.noresttherein.oldsql.model.Restrictive.?== ?==]] or
		  *                   [[net.noresttherein.oldsql.model.Restrictive.?< ?<]] on a lambda expression of `E`.
		  * @see [[net.noresttherein.oldsql.model.Kin.Restrained.required[E,K]* apply]]`(restrainer)`.
		  */
		def apply[E, K](restrainer :Restrainer[E, K]) :KinFactory[K, E, E] =
			new RestrainedKinFactory[K, E, E](restrainer)

		/** A factory of [[net.noresttherein.oldsql.model.Kin.Restrained Restrained]] kin referencing all values
		  * satisfying a given [[net.noresttherein.oldsql.model.Restraint Restraint]] (a domain model-level expression
		  * serving as a filter on the entity type `E`). Returned factory creates instances of
		  * [[net.noresttherein.oldsql.model.Kin.One One]]`[E]`, but the value type can be changed
		  * to any [[net.noresttherein.oldsql.model.ComposedOf composite]] of `E`
		  * with its [[net.noresttherein.oldsql.model.GenericKinFactory.as as]] and
		  * [[net.noresttherein.oldsql.model.GenericKinFactory.in in]] methods. As this factory,
		  * and all obtained through it by adapting to other composite types, create only instances
		  * of [[net.noresttherein.oldsql.model.Kin.Derived Derived]] kin, it is automatically
		  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.isRequired required]]:
		  * its [[net.noresttherein.oldsql.model.GenericKinFactory.nonexistent nonexistent]] method
		  * throws a [[net.noresttherein.oldsql.exceptions.NonexistentEntityException NonexistentEntityException]].
		  * @param restrainer a factory of instances of [[net.noresttherein.oldsql.model.Restraint Restraint]]`[E]`,
		  *                   which can be obtained from factory objects in
		  *                   [[net.noresttherein.oldsql.model.Restraint$ Restraint]] object, such as
		  *                   [[net.noresttherein.oldsql.model.Restraint.Equal Equal]], or - if
		  *                   `net.noresttherein.oldsql.model.implicits._` are imported - by calling one of the
		  *                   factory methods in [[net.noresttherein.oldsql.model.Restrictive Restrictive]],
		  *                   such as [[net.noresttherein.oldsql.model.Restrictive.?== ?==]] or
		  *                   [[net.noresttherein.oldsql.model.Restrictive.?< ?<]] on a lambda expression of `E`.
		  * @see [[net.noresttherein.oldsql.model.Kin.Restrained.apply[K,E:TypeTag,P,Y]* apply]]`(factory, property)`.
		  */
		def required[E, K](restrainer :Restrainer[E, K]) :DerivedKinFactory[K, E, E] =
			new RequiredRestrainedKinFactory[K, E, E](restrainer)


		@SerialVersionUID(KinVer)
		private class PresentRestrained[E, T](override val where :Restraint[E], override val get :T)
		                                     (implicit as :T ComposedOf E)
			extends PresentBase[E, T](get)(as.decomposer, as.composer) with Restrained[E, T]
		{
			def this(where :Restraint[E], toOption :Option[T])(implicit as :T ComposedOf E) =
				this(where, toOption.get)
		}

		@SerialVersionUID(KinVer)
		private class AbsentRestrained[E, T](override val where :Restraint[E], isMissing :Boolean = true)
		                                    (implicit override val composition :T ComposableFrom E)
			extends AbsentBase[E, T](isMissing) with Restrained[E, T]

		@SerialVersionUID(KinVer)
		private class LazyRestrained[E, T](override val where :Restraint[E], init :() => Option[T])
		                                  (implicit as :T ComposedOf E)
			extends Delayed[T](init) with Restrained[E, T]
		{
			override def composition :T ComposableFrom E = as.composer
			override def items :Option[Iterable[E]] = toOption.map(as.decomposer.apply)

			private def writeReplace = toOption match {
				case Some(t) => Derived.present(t)
				case _ => Unknown
			}
		}




		@SerialVersionUID(KinVer)
		private class RestrainedKinFactory[K, E, X](private val restrainer :Restrainer[E, K])
		                                           (implicit val result :X ComposedOf E)
			extends BaseKinFactory[K, E, X]
		{
			override def delay(key :K, value: => Option[X]): Derived[E, X] =
				Restrained.delay(restrainer(key), value)

			override def apply(key :K, value :Option[X]) :Derived[E, X] = Restrained(restrainer(key), value)
			override def present(value :X) :Derived[E, X] = Restrained(Restraint.False, Some(value))
			override def present(key :K, value :X) :Derived[E, X] = Restrained.present(restrainer(key), value)
			override def missing(key: K): Derived[E, X] = Restrained.missing(restrainer(key))
			override def absent(key: K): Derived[E, X] = Restrained.absent(restrainer(key))

			override def keyFrom(item: E): Opt[K] = restrainer.from(item)

			override def keyOf(ref: Kin[X]): Opt[K] = ref match {
				case AllWhere(this.restrainer(key)) => Got(key)
				case result.decomposer(values) =>
					if (values.isEmpty) Lack
					else restrainer.from(values.head) match {
						case res @ Got(k) if values.forall(_ == k) => res
						case _ => Lack
					}
				case _ => Lack
			}

			override def required :KinFactory[K, E, X] =
				if (isRequired) this else new RequiredRestrainedKinFactory(restrainer)

			override def notRequired :KinFactory[K, E, X] =
				if (isRequired) new RestrainedKinFactory(restrainer) else this

			override def as[Y](implicit composition: Y ComposedOf E): KinFactory[K, E, Y] =
				new RestrainedKinFactory[K, E, Y](restrainer)


			override def equivalencyToken :Any = restrainer

			override def canEqual(that: Any): Boolean = that.isInstanceOf[RestrainedKinFactory[_,_,_]]

			override def equals(that :Any) :Boolean = that match {
				case c:RestrainedKinFactory[_, _, _] =>
					(this eq c) || c.canEqual(this) && c.restrainer == restrainer && c.result == result
				case _ => false
			}

			override def hashCode :Int = restrainer.hashCode * 31 + result.hashCode

			override def toString = result.toString + (if (isRequired) "" else "?") + "{" + restrainer + "}"
		}


		@SerialVersionUID(KinVer)
		private class RequiredRestrainedKinFactory[K, E, X](restrainer :Restrainer[E, K])
		                                                   (implicit result :X ComposedOf E)
			extends RestrainedKinFactory[K, E, X](restrainer) with BaseDerivedKinFactory[K, E, X]
		{
			override def apply(key :K, value :Option[X]) = Restrained(restrainer(key), value)
			override def present(value :X) :Restrained[E, X] = Restrained.present(Restraint.False, value)

			override def notRequired :KinFactory[K, E, X] = new RestrainedKinFactory(restrainer)

			override def notRequired(nonexistent :Kin[X]) :KinFactory[K, E, X] =
				if (nonexistent == Nonexistent()) notRequired
				else super.notRequired(nonexistent)

			override def as[Y](implicit composition: Y ComposedOf E): DerivedKinFactory[K, E, Y] =
				new RequiredRestrainedKinFactory[K, E, Y](restrainer)
		}

	}



	private[oldsql] final val KinVer = 1L
}

