package net.noresttherein.oldsql.model

import scala.collection.{immutable, EvidenceIterableFactory, Factory, IterableFactory, MapFactory, SortedMapFactory}
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.IllegalKinArityException
import net.noresttherein.oldsql.model.ComposedOf.{Arity, CollectionOf, ComposableFrom, ConstructFrom, DecomposableTo, ExtractAs}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.ToCollection
import net.noresttherein.oldsql.model.Kin.Derived

//here be implicits
import net.noresttherein.oldsql.slang._







/**  A pair, used as the element type to which `Map` is decomposed by `ComposedOf` and related classes.
  *  Unlike `Tuple2`, it is not final, which means it can be mocked during property discovery, allowing
  *  of entity relationships mapped to `Map` instances being crossed by chained property expressions used to
  *  create filter and fetch descriptors.
  */
case class ->[+L, +R](_1 :L, _2 :R) extends Product2[L, R] with Serializable

object -> {
	implicit def fromTuple2[_1, _2](t2 :(_1, _2)) : _1->_2 = ->(t2._1, t2._2)
	implicit def toTuple2[_1, _2](pair :_1->_2) :(_1, _2) = (pair._1, pair._2)
}



/** Proof that type `C` is in some way composed of values of `E` and can be converted to and from `Iterable[E]`
  * (possibly with some restrictions in place). Serves as a single umbrella for collection types
  * `( _<: Iterable[E])`, `Option[E]` and `E` itself, allowing to create queries for `E` with result type `C`.
  * In order to statically exclude the identity composition (as it can lead to non obvious bugs, you can demand
  * an implicit of its subtype instead: `C CollectionOf E`.
  * It is generally written in the code using infix notation for clarity: `C ComposedOf E`.
  * @see [[net.noresttherein.oldsql.model.ComposedOf.CollectionOf CollectionOf]]
  */
trait ComposedOf[C, E] extends Serializable {
	def arity :Arity = composer.arity
//		if (composition.arity == decomposition.arity) composition.arity
//		else throw new UnsupportedOperationException(s"ComposedOf.arity: composition arity (${composition.arity}) is different than decomposition arity (${decomposition.arity}) in $this. Most probabaly a bug.")

	implicit val composer :C ComposableFrom E
	implicit val decomposer :C DecomposableTo E

	def apply(items: Iterable[E]): C = composer(items)

	def apply(composite: C): Iterable[E] = decomposer(composite)

	def rebuild(value: C): C = composer(decomposer(value))

	def unapply(composite :C) :Opt[Iterable[E]] = decomposer.unapply(composite)

	def unapply(kin :Kin[C]) :Opt[Derived[E, C]] =
		kin match {
			case composite :Derived[E @unchecked, C] if composite.composition compatibleWith decomposer =>
				Got(composite)
			case _ => Lack
		}


	def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = composer.compatibleWith(other)
	def compatibleWith[X <: C](other: DecomposableTo[X, _]) :Boolean = decomposer.compatibleWith(other)
	def compatibleWith[X <: C](other : X ComposedOf _) :Boolean =
		other.composer.compatibleWith(composer) && compatibleWith(other.decomposer)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedOf[_, _]]

	override def equals(obj: scala.Any): Boolean = obj match {
		case that :ComposedOf[_, _] =>
			(that eq this) || that.canEqual(this) && composer == that.composer && decomposer == that.decomposer
		case _ => false
	}

	override def hashCode :Int = composer.hashCode * 31 + decomposer.hashCode

	override def toString :String = (composer, decomposer) match {
		case (c :ToCollection[_, _], DecomposableTo.Iterable()) => c.toString
		case (c, d) =>
			val cs = c.toString; val ds = d.toString
			if (cs == ds) cs
			else cs + "-" + ds
	}
}






abstract class LowPriorityComposedOfImplicits {

	/** Combines implicitly available composer and decomposer for the type pair `C`, `E` into a `C ComposedOf E`.
	  * Returned instance overrides equality in terms of equality of its constituents. This may potentially lead to
	  * issues with custom collection classes, as the default implicit value for `C ComposableFrom E` is based on
	  * an implicit `CanBuildFrom[_, E, C]`. Currently all standard scala collections however return always the same
	  * builder factory instance, resulting in the desired behaviour of two instances created by this method
	  * for the same pair of types being equal.
	  */
	implicit def composedOf[C, E](implicit compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E])
			:C ComposedOf E =
		ComposedOf[C, E](compose, decompose)

	/** Combines implicitly available constructor and extractor for the type pair `C`, `E` into a `C CollectionOf E`.
	  * Returned instance overrides equality in terms of equality of its constituents. This may potentially lead to
	  * issues with custom collection classes, as the default implicit value for `C ComposableFrom E` is based on
	  * an implicit `CanBuildFrom[_, E, C]`. Currently all standard scala collections however return always the same
	  * builder factory instance, resulting in the desired behaviour of two instances created by this method
	  * for the same pair of types being equal.
	  */
	implicit def collectionOf[C, E](implicit compose :C ConstructFrom E, decompose :C ExtractAs E) :C CollectionOf E =
		CollectionOf(compose, decompose)
}



abstract class ImplicitFallbackComposedOfItself extends LowPriorityComposedOfImplicits {

	/** A witness that any type `T` consists of itself. This is useful as it eliminates special treatment of result types
	  * in most scenarios, but as it is always implicitly available, some care needs to be taken to make sure that it
	  * is not used instead of the proper one for actual composite types.
	  */
	implicit def itself[T] :T ComposedOf T = ComposedOf.self
}



object ComposedOf extends ImplicitFallbackComposedOfItself {

	/** Summons the implicitly available instance of `C ComposedOf E`. */
	def apply[C, E](implicit ev :C ComposedOf E) :C ComposedOf E = ev

	/** Creates a custom composition from the passed functions. Note that unless the functions provided
	  * override equals in a meaningful way, subsequent calls to this method will yield objects which won't be equal.
	  */
	def apply[C, E](compose :Iterable[E] => C, decompose :C => Iterable[E]) :C ComposedOf E =
		new Hybrid(ComposableFrom(compose), DecomposableTo(decompose))

	/** Create a `C ComposedOf E` using the provided composer and decomposer. */
	def apply[C, E](compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		if (compose == ComposableFrom.Self() && decompose == DecomposableTo.Self())
			ComposedOf.itself.asInstanceOf[C ComposedOf E]
		else
			new Hybrid(compose, decompose)



	/** A witness that `Option[E]` is composed of `E` and can be converted to and from a (singleton) `Iterable[E]`. */
	implicit def option[T] :Option[T] CollectionOf T = optionInstance.asInstanceOf[Option[T] CollectionOf T]

//	/** An implicit value witnessing that `Iterable[E]` is composed of values of `E`. */
//	implicit def iterable[E] :Iterable[E] CollectionOf E =
//		CollectionOf(ComposableFrom.iterable[E], DecomposableTo.iterable[E])

	implicit def collection[C[X] <: Iterable[X], E](implicit factory :Factory[E, C[E]]) :C[E] CollectionOf E =
		CollectionOf(ComposableFrom.collection, DecomposableTo.iterable)

	/** An implicit value witnessing that `Map[K, V]` can be converted to and from a collection of `K->V`. */
	implicit def map[K, V] :Map[K, V] CollectionOf (K -> V) = mapInstance.asInstanceOf[Map[K, V] CollectionOf (K -> V)]



	/** An implicit witness that type `C` is strictly composed of a variable or multiple number of values of type `E`.
	  * This particularly excludes instances of `E ComposedOf E` so that operations which either make no sense or can
	  * be misleading when dealing with singleton values can require a `C CollectionOf E` instead of the more generic
	  * `C ComposedOf E`.
	  */
	trait CollectionOf[C, E] extends ComposedOf[C, E] {
		override implicit val composer :C ConstructFrom E
		override implicit val decomposer :C ExtractAs E

		override def toString :String = composer.toString
	}


	/** A factory for `C CollectionOf E` objects responsible for converting between `C` and `Iterable[E]`. */
	object CollectionOf {
		/** Summon an implicitly available instance of `C CollectionOf E`. */
		def apply[C, E](implicit composition :C CollectionOf E) :C CollectionOf E = composition

		/** Creates an instance of `C CollectionOf E` from the given constructor and extractor the same way as
		  * [[net.noresttherein.oldsql.model.ComposedOf.apply ComposedOf(compose, decompose)]]. Note that instances
		  * of `C CollectionOf E` will equal (symmetrically) instances of `C ComposedOf E` as long as their corresponding
		  * constituents are equal.
		  */
		def apply[C, E](compose :C ConstructFrom E, decompose :C ExtractAs E) :C CollectionOf E =
			new CollectionOf[C, E] {
				override val composer = compose
				override val decomposer = decompose
			}

		def apply[C <: Iterable[E], E](factory :Factory[E, C]) :C CollectionOf E =
			apply(ComposableFrom.Collection()(factory), DecomposableTo.Iterable())
	}




	/** Restriction on the number of values on one side of a relation. When used in ComposedOf, it defines how many elements
	  * Iterable[E] obtained from C can have.
	  */
	sealed trait Arity

	object Arity {
		trait AtMostOne extends Arity
		trait AtLeastOne extends Arity

		/** Represents that a collection/result set/end of a relationship must always consist of exactly one value. */
		case object _1 extends AtMostOne with AtLeastOne {
			override def toString = "1"
		}

		/** Represents that a collection/result set/end of a relationship must consist of no more than one value. */
		case object _0_1 extends AtMostOne {
			override def toString = "0-1"
		}

		/** Represents that a collection/result set/end of a relationship can consist of any number of elements. */
		case object _0_n extends Arity {
			override def toString = "0-n"
		}

		/** Represents that a collection/result set/end of a relationship must be non-empty. */
		case object _1_n extends AtLeastOne { //todo: actually use it
			override def toString = "1-n"
		}

		object AtMostOne {
			def apply(arity :Arity) :Boolean = arity.isInstanceOf[AtMostOne]
			def unapply(arity :Arity) :Option[AtMostOne] = arity.asSubclass[AtMostOne]
		}

		object AtLeastOne {
			def apply(arity :Arity) :Boolean = arity.isInstanceOf[AtLeastOne]
			def unapply(arity :Arity) :Option[AtLeastOne] = arity.asSubclass[AtLeastOne]
		}
	}


	//objects for unique deserialization
	private[model] def self[T] :T ComposedOf T = selfInstance.asInstanceOf[T ComposedOf T]
	private object selfInstance extends Hybrid[Any, Any](ComposableFrom.itself[Any], DecomposableTo.itself[Any])

	private object optionInstance extends CollectionOf[Option[Any], Any] {
		implicit override val composer = ComposableFrom.option
		implicit override val decomposer = DecomposableTo.option
	}

	private object mapInstance extends CollectionOf[Map[Any, Any], Any -> Any] {
		implicit override val composer = ComposableFrom.map
		implicit override val decomposer = DecomposableTo.map
	}

	private class Hybrid[C, E](val composer :ComposableFrom[C, E], val decomposer :DecomposableTo[C, E])
		extends ComposedOf[C, E]






	/** A way to create value of `C` from `Iterable[E]`. `C` might be the type `E` itself, a collection of `E`
	  * or some custom composite object constructed from multiple parts such as a matrix. For this reason
	  * the construction might not be possible for all collections of `E`, in particular based on its size.
	  * @see [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]]
	  * @see [[net.noresttherein.oldsql.model.ComposedOf ComposedOf]]
	  */
	trait ComposableFrom[+C, -E] extends Serializable { composition =>
		def arity :Arity

		def empty :C = apply(Seq())

		def apply(items :Iterable[E]) :C = attempt(items) getOrElse {
			throw new IllegalKinArityException(s"Can't perform composition $this for $items.")
		}

		def attempt(items :Iterable[E]) :Opt[C]

//		def compose[T <: E, X](first :T ComposedOf X) :C ComposedOf X

		def decomposeWith[W >: C, T <: E](decomposition :DecomposableTo[W, T]) :ComposedOf[W, T] =
			new ComposedOf.Hybrid[W, T](this, decomposition)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposableFrom[_, _]]

		def compatibleWith[X >: C](other :ComposableFrom[X, _]) :Boolean = this eq other

		def compatibleWith(decomposer :DecomposableTo[C, _]) :Boolean
	}



	/** A trait extended by `ComposableFrom` implementations using actual collection classes as `C`.
	  * This especially excludes the identity composition `C=:=E`. As an implicit value for latter is always available,
	  * it might easily lead to situations where a collection type is treated as composed of itself rather than its
	  * items, leading to unobvious bugs.
	  */
	trait ConstructFrom[+C, -E] extends ComposableFrom[C, E] {
		def builder :Builder[E, C]
		def decomposeWith[W >: C, T <: E](decomposition :ExtractAs[W, T]) :CollectionOf[W, T] =
			CollectionOf(this, decomposition)
	}

	object ConstructFrom {
		/** Summons an implicitly available instance of `T ConstructFrom E`. */
		@inline def apply[T, E](implicit compose :T ConstructFrom E) :T ConstructFrom E = compose


		def unapply[T, E](composition :T ComposableFrom E) :Opt[T ConstructFrom E] = composition match {
			case c :ConstructFrom[T, E] => Got(c)
			case _ => Lack
		}

		def unapply[T, E](composite :T ComposedOf E) :Opt[T ConstructFrom E] = unapply(composite.composer)

		def unapply[T, E](kin :Derived[E, T]) :Opt[T ConstructFrom E] = unapply(kin.composition)

		def unapply[T](kin :Kin[T]) :Opt[ConstructFrom[T, _]] = kin match {
			case c :Derived[_, _] if c.composition.isInstanceOf[ConstructFrom[_, _]] =>
				Got(c.composition.asInstanceOf[ConstructFrom[T, _]])
			case _ => Lack
		}
	}



	sealed abstract class FallbackComposableFromImplicits {
		implicit def ordered[S <: Iterable[V], V](implicit factory :Factory[V, S]) :S ConstructFrom (Int -> V) =
			ComposableFrom.Ordered()
	}



	/** A factory and matcher as well as the container for implicit values of `C ComposableFrom E` which carry
	  * information about how values of type `C` can be created from a collection of values of type `E`.
	  */
	object ComposableFrom extends FallbackComposableFromImplicits {

		/** Summons an implicitly available instance of `T ComposableFrom E`. */
		def apply[T, E](implicit ev :T ComposableFrom E) : T ComposableFrom E = ev

		/** Creates a custom composer backed by the given function. */
		def apply[T, E](compose :Iterable[E] => T) :ComposableFrom[T, E] = new Custom(compose)


		def unapply[T, E](composedOf :T ComposedOf E) :Opt[T ComposableFrom E] = Got(composedOf.composer)

		def unapply[T, E](kin :Derived[E, T]) :Opt[T ComposableFrom E] = Got(kin.composition)

		def unapply[T](kin :Kin[T]) :Opt[ComposableFrom[T, _]] = kin match {
			case c :Derived[_, T] => Got(c.composition)
			case _ => Lack
		}


		/** A composition for type `Map[K, V]` out of `Iterable[(K->V)]`. */ //todo: other map types
		implicit def map[K, V] :Map[K, V] ConstructFrom (K -> V) = ToMap.asInstanceOf[Map[K, V] ConstructFrom (K->V)]

		implicit def factory[C, T](implicit factory :Factory[T, C]) :C ComposableFrom T =
			new Custom(items => (factory.newBuilder ++= items).result())

		implicit def collection[C <: Iterable[T], T](implicit factory :Factory[T, C]) :C ConstructFrom T =
			if (iterableFactoryOf(factory).contains(scala.collection.Iterable))
				Iterable().asInstanceOf[C ConstructFrom T]
			else
				Collection[C, T]()

//		/** A composition for type `Iterable[T]` itself using an identity function. */
//		implicit def iterable[T] :Iterable[T] ConstructFrom T = ToIterable.asInstanceOf[ConstructFrom[Iterable[T], T]]

		/** Composition of `Option[T]` out of an `Iterable[T]`. If the input iterable contains more than one element,
		  * an exception is thrown.
		  */ //consider: making these two not ComposableFrom
		implicit def option[T] :Option[T] ConstructFrom T = ToOption.asInstanceOf[ConstructFrom[Option[T], T]]

//		/** Composition of `Opt[T]` out of an `Iterable[T]`. If the input iterable contains more than one element,
//		  * an exception is thrown. Note that there may be no advantage to gain out of using `Opt` in place of `Option`
//		  * in case the composite type is generic, as it would result in boxing either way.
//		  */
//		implicit def opt[T] :Opt[T] ConstructFrom T = ToOpt.asInstanceOf[ConstructFrom[Opt[T], T]]

		/** Identity composition available for all types witnessing that a singleton `Iterable[T]` can be converted to
		  * the type `T` itself. This instance will always throw an exception if the input collection contains
		  * any number of elements different than 1.
		  */
		implicit def itself[T] :T ComposableFrom T = ToSelf.asInstanceOf[ComposableFrom[T, T]]




		/** A factory and  matcher for singular 'composition' representation of an entity property.
		  * The composite type in this case is the property type `T`, while the element type is the entity type `E`.
		  */
		object Property {
			def apply[T, E](property :PropertyPath[E, T]) :T ComposableFrom E =
				new ToProperty[T, E](property)

			def unapply[T, E](composition :T ComposableFrom E) :Opt[PropertyPath[E, T]] = composition match {
				case prop :ToProperty[T, E] => Got(prop.property)
				case _ => Lack
			}

			def unapply[T, E](composedOf :T ComposedOf E) :Opt[PropertyPath[E, T]] = unapply(composedOf.composer)

			def unapply[T, E](kin :Derived[E, T]) :Opt[PropertyPath[E, T]] = unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[PropertyPath[_, T]] = kin match {
				case composite :Derived[e, T] => unapply(composite.composition)
				case _ => Lack
			}
		}

		/** A factory and matcher for collections of the values of a certain property of the element type `E`. */
		object Properties {
			def apply[T, P, E](property :PropertyPath[E, P])
			                  (implicit composition :T ComposableFrom P) :T ComposableFrom E =
				new ToProperties(property, composition)

			def unapply[T, E](composition :T ComposableFrom E)
					:Opt[(PropertyPath[E, P], T ComposableFrom P) forSome { type P }] =
				composition match {
					case prop :ToProperties[T, p, E] => Got(prop.property, prop.compose)
					case _ => Lack
				}

			def unapply[T, E](composition :T ComposedOf E)
					:Opt[(PropertyPath[E, P], T ComposableFrom P) forSome { type P }] =
				unapply(composition.composer)

			def unapply[T, E](kin :Derived[E, T])
					:Opt[(PropertyPath[E, P], T ComposableFrom P) forSome { type P }] =
				unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[(PropertyPath[E, P], T ComposableFrom P) forSome { type P; type E }] =
				kin match {
					case composite :Derived[e, T] => unapply(composite.composition)
					case _ => Lack
				}
		}

		/** A factory and matcher for identity compositions witnessing that `S ComposedOf T` for any type `S >: T`. */
		object Self {
			def apply[T]() :T ComposableFrom T = ToSelf.asInstanceOf[T ComposableFrom T]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == ToSelf

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composer == ToSelf

			def unapply[T, E](kin :Derived[E, T]) :Boolean = kin.composition == ToSelf

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case c :Derived[_, _] => c.composition == ToSelf
				case _ => false
			}
		}


		/** A factory and matcher for the composition of `Option[T]` out of one or zero values of `T`. */
		object Option {
			def apply[T](): Option[T] ConstructFrom T = ToOption.asInstanceOf[Option[T] ConstructFrom T]

			def of[T] :Option[T] ConstructFrom T = apply()

			def unapply[T, E](composition :T ComposableFrom E) :Boolean = composition == ToOption

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.composer == ToOption

			def unapply[T, E](kin :Derived[E, T]) :Boolean = kin.composition == ToOption

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case c :Derived[_, _] => c.composition == ToOption
				case _ => false
			}
		}

//		/** A factory and matcher for the composition of `Option[T]` out of one or zero values of `T`. */
//		object Opt {
//			def apply[T](): Opt[T] ConstructFrom T = ToOpt.asInstanceOf[Opt[T] ConstructFrom T]
//
//			def of[T] :Opt[T] ConstructFrom T = apply()
//
//			def unapply[T, E](composition :T ComposableFrom E) :Boolean = composition == ToOpt
//
//			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.composer == ToOpt
//
//			def unapply[T, E](kin :Derived[E, T]) :Boolean = kin.composition == ToOpt
//
//			def unapply[T](kin :Kin[T]) :Boolean = kin match {
//				case c :Derived[_, _] => c.composition == ToOpt
//				case _ => false
//			}
//		}


		/** A factory and matcher for the composition of the type `Iterable[T]` itself. */
		object Iterable {
			def apply[T](): Iterable[T] ConstructFrom T = ToIterable.asInstanceOf[Iterable[T] ConstructFrom T]

			def of[T] :Iterable[T] ConstructFrom T = apply()

			def unapply[T, E](composition :T ComposableFrom E) :Boolean = composition == ToIterable

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.composer == ToIterable

			def unapply[T, E](kin :Derived[E, T]) :Boolean = kin.composition == ToIterable

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case c :Derived[_, _] => c.composition == ToIterable
				case _ => false
			}
		}


		/** A factory and matcher for instances of `C ConstructFrom E` for types `C <: Iterable[E]` using
		  * implicitly available `CanBuildFrom[_, E, C]`.
		  */
		object Collection {
			def apply[T, E]()(implicit factory :Factory[E, T]) :T ConstructFrom E =
				new ToCollection[T, E](factory)

			/** Returns a factory of instances `_ ComposedOf E`, allowing a possibly shorter syntax of
			  * `Collection.of[E].in[Seq]` rather than `Collection[Seq[E], E]()`.
			  */
			@inline def of[E] :CollectionComposer[E] = new CollectionComposer[E] {}

			trait CollectionComposer[E] extends Any {
				final def as[T <: Iterable[E]](factory :Factory[E, T]) :T ConstructFrom E =
					new ToCollection[T, E](factory)

				final def in[C[X] <: Iterable[X]](implicit factory :Factory[E, C[E]]) :C[E] ConstructFrom E =
					new ToCollection[C[E], E](factory)

				final def apply[C[X]](factory :IterableFactory[C]) :C[E] ConstructFrom E =
					new ToCollection[C[E], E](factory)

				final def apply[C[X], P[X]](factory :EvidenceIterableFactory[C, P])
				                           (implicit ev :P[E]) :C[E] ConstructFrom E =
					new ToCollection[C[E], E](factory)
			}

			def unapply[T, E](composition :T ComposableFrom E) :Opt[Factory[E, T]] = composition match {
				case col :ToCollection[T, E] => Got(col.factory)
				case _ => Lack
			}

			def unapply[T, E](composedOf :T ComposedOf E) :Opt[Factory[E, T]] = unapply(composedOf.composer)

			def unapply[T, E](kin :Derived[E, T]) :Opt[Factory[E, T]] = unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[Factory[_, T]] = kin match {
				case c :Derived[e, T] => unapply(c.composition)
				case _ => Lack
			}
		}


		/** A factory and matcher for composition of `Map[K, V]` and subclasses from instances of `K -> V`.
		  * @see [[net.noresttherein.oldsql.model.-> ->]]
		  */
		object Map {
			def apply[M[A, B] <: Map[A, B], K, V]()(implicit factory :Factory[(K, V), M[K, V]])
					:M[K, V] ConstructFrom (K -> V) =
				new ToMap[M, K, V](factory)("Map[" + factory.innerClassName + "]")

			@inline def of[K, V] :MapComposer[K, V] = new MapComposer[K, V] {}

			sealed trait MapComposer[K, V] extends Any {
				final def apply() :Map[K, V] ConstructFrom (K -> V) =
					ToMap.asInstanceOf[Map[K, V] ConstructFrom (K -> V)]

				final def in[M[_, _]](implicit factory :Factory[(K, V), M[K, V]]) :M[K, V] ConstructFrom (K -> V) =
					new ToMap[M, K, V](factory)("Map[" + factory.innerClassName + "]")

				final def apply[M[_, _]](factory :MapFactory[M]) :M[K, V] ConstructFrom (K -> V) =
					new ToMap[M, K, V](factory)(factory.toString)

				final def apply[M[_, _]](factory :SortedMapFactory[M])
				                        (implicit ord :Ordering[K]) :M[K, V] ConstructFrom (K -> V) =
					new ToMap[M, K, V](factory)(factory.toString)
			}


			def unapply[T, E](composition :T ComposableFrom E) :Opt[Factory[(_, _), _]] = composition match {
				case ToMap(factory) => Got(factory)
				case _ => Lack
			}

			def unapply[T, E](composition :T ComposedOf E) :Opt[Factory[(_, _), _]] =
				unapply(composition.composer)

			def unapply[T, E](kin :Derived[E, T]) :Opt[Factory[(_, _), _]] =
				unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[Factory[(_, _), _]] = kin match {
				case c :Derived[e, T] => unapply(c.composition)
				case _ => Lack
			}
		}


		/** A factory and matcher for composition of `Seq[V]` and subclasses from instances
		  * of `Int `[[net.noresttherein.oldsql.model.-> ->]]` V`, where the first element of the pair is the index
		  * of element of the second element. More precisely, the sequence is constructed by sorting the input ascending
		  * by index - gaps in numeration result in subsequent elements being at lower position than stated rather than
		  * `null` or other filler elements at missing indices.
		  */
		object Ordered {
			def apply[S, V]()(implicit factory :Factory[V, S]) :S ConstructFrom (Int -> V) =
				new ToOrdered[S, V](factory)("Ordered[" + factory.innerClassName + "]")

			@inline def of[V] :OrderedComposer[V] = new OrderedComposer[V] {}

			sealed trait OrderedComposer[V] extends Any {
				final def as[S](factory :Factory[V, S]) :S ConstructFrom (Int -> V) =
					new ToOrdered[S, V](factory)("Ordered[" + factory.innerClassName + "]")

				final def in[S[_]](implicit factory :Factory[V, S[V]]) :S[V] ConstructFrom (Int -> V) = as(factory)

				final def apply[S[_]](factory :IterableFactory[S]) :S[V] ConstructFrom (Int -> V) =
					new ToOrdered[S[V], V](factory)(factory.toString)
			}


			def unapply[T, E](composition :T ComposableFrom E) :Opt[Factory[_, _]] = composition match {
				case idx :ToOrdered[_, _] => Got(idx.factory)
				case _ => Lack
			}

			def unapply[T, E](composition :T ComposedOf E) :Opt[Factory[_, _]] = unapply(composition.composer)

			def unapply[T, E](kin :Derived[E, T]) :Opt[Factory[_, _]] = unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[Factory[_, _]] = kin match {
				case c :Derived[_, _] => unapply(c.composition)
				case _ => Lack
			}
		}



		/** A custom `C ComposableFrom E` instance backed by the provided function. */
		class Custom[+C, -E](private val compose :Iterable[E] => C, override val arity :Arity = Arity._0_n)
			extends ComposableFrom[C, E]
		{
			override def apply(items: Iterable[E]): C = compose(items)
			override def attempt(items: Iterable[E]): Opt[C] = Got(compose(items))

			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean =
				other.canEqual(this) && this == other

			override def compatibleWith(decomposer :DecomposableTo[C, _]) :Boolean = false

			override def toString :String = arity.toString
		}

		object Custom {
			def apply[T, E](compose :Iterable[E] => T, arity :Arity = Arity._0_n) :T ComposableFrom E =
				new Custom(compose, arity)

			def unapply[T, E](composition :T ComposableFrom E) :Opt[Iterable[E] => T] = composition match {
				case custom :Custom[T, E] => Got(custom.compose)
				case _ => Lack
			}

			def unapply[T, E](composedOf :T ComposedOf E) :Opt[Iterable[E] => T] = unapply(composedOf.composer)

			def unapply[T, E](kin :Derived[E, T]) :Opt[Iterable[E] => T] = unapply(kin.composition)

			def unapply[T](kin :Kin[T]) :Opt[Iterable[E] => T] forSome { type E } = kin match {
				case c :Derived[e, T] => unapply[T, e](c.composition)
				case _ => Lack
			}
		}



		object Nothing {
			def apply[E]() :Nothing ComposableFrom E = ToNothing.asInstanceOf[Nothing ComposableFrom E]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean = composition == ToNothing
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.composer == ToNothing

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case derived :Derived[_, _] => derived.composition == ToNothing
				case _ => false
			}
		}



		private case class ToProperties[+T, P, -E](property :PropertyPath[E, P], compose :T ComposableFrom P)
			extends ComposableFrom[T, E]
		{
			override def arity = compose.arity

			override def apply(items :Iterable[E]) = compose(items.map(property.fun))
			override def attempt(items :Iterable[E]) = compose.attempt(items.map(property.fun))

			override def compatibleWith(decomposer :DecomposableTo[T, _]) = false

			override def toString = compose.toString + "(_." + property + ")"
		}

		private class ToProperty[T, -E](prop :PropertyPath[E, T]) extends ToProperties[T, T, E](prop, Self()) {
			override def attempt(items :Iterable[E]) =
				if (items.isEmpty || items.sizeIs > 1) Lack
				else Got(property.fun(items.head))

			override def apply(items :Iterable[E]) =
				if (items.isEmpty || items.sizeIs > 1)
					throw new IllegalKinArityException("Expected a single entity for " + this + ", got: " + items)
				else property.fun(items.head)

			override def toString = "_." + property.name
		}

		private case object ToSelf extends ComposableFrom[Any, Any] {
			def arity :Arity = Arity._1

			override def apply(items :Iterable[Any]) :Any =
				if (items.size == 1) items.head
				else throw new IllegalKinArityException("Not a single result: " + items + ".")

			override def attempt(items: Iterable[Any]) :Opt[Any] =
				if (items.size == 1) Got(items.head) else Lack

			override def compatibleWith(decomposer :DecomposableTo[Any, _]) =
				decomposer == DecomposableTo.Self()

			override def toString = "One" //">:"
		}

		private case object ToOption extends ConstructFrom[Option[Any], Any] {
			def arity :Arity = Arity._0_1

			override def builder = new Builder[Any, Option[Any]] {
				var res :Option[Any] = None

				override def clear() :Unit = res = None

				override def result() = res

				override def addOne(elem :Any) :this.type = {
					if (res.nonEmpty)
						throw new IllegalKinArityException(
							"Attempted to return more than one result as an Option: " + res.get + ", "+ elem + "."
						)
					res = Some(elem)
					this
				}
			}

			override def apply(items :Iterable[Any]) :Option[Any] =
				if (items.sizeIs > 1)
					throw new IllegalKinArityException("Expected 0-1 results, got " + items + ".")
				else items.headOption

			override def attempt(items: Iterable[Any]) :Opt[Option[Any]] =
				if (items.sizeIs <= 1) Got(items.headOption) else Lack

			override def compatibleWith(decomposer :DecomposableTo[Option[Any], _]) =
				DecomposableTo.Option.unapply(decomposer) //|| DecomposableTo.Iterable.unapply(decomposer)

			override def toString = "Option"
		}

//		private case object ToOpt extends ConstructFrom[Opt[Any], Any] {
//			def arity :Arity = Arity._0_1
//
//			override def builder = new Builder[Any, Opt[Any]] {
//				var res :Opt[Any] = Lack
//
//				override def clear() :Unit = res = Lack
//
//				override def result() = res
//
//				override def addOne(elem :Any) :this.type = {
//					if (res.nonEmpty)
//						throw new IllegalKinArityException(
//							"Attempted to return more than one result as an Option: " + res.get + ", "+ elem + "."
//						)
//					res = Got(elem)
//					this
//				}
//			}
//
//			override def apply(items :Iterable[Any]) :Opt[Any] =
//				if (items.sizeIs > 1)
//					throw new IllegalKinArityException("Expected 0-1 results, got " + items + ".")
//				else if (items.isEmpty) Lack
//				else Got(items.head)
//
//			override def attempt(items: Iterable[Any]) :Opt[Opt[Any]] =
//				if (items.isEmpty) Got(Lack)
//				else if (items.sizeIs <= 1) Got(Got(items.head))
//				else Lack
//
//			override def compatibleWith(decomposer :DecomposableTo[Opt[Any], _]) =
//				DecomposableTo.Opt.unapply(decomposer)
//
//			override def toString = "Opt"
//		}

		private[ComposedOf] class ToCollection[+C, -E](val factory :Factory[E, C])
			extends Custom[C, E](factory.fromSpecific) with ConstructFrom[C, E]
		{
			override def builder = factory.newBuilder

			def iterableFactory = iterableFactoryOf(factory)


			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = canEqual(other)
			override def compatibleWith(decomposer :DecomposableTo[C, _]) :Boolean =
				DecomposableTo.Iterable.unapply(decomposer)

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ToCollection[_, _]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case col :ToCollection[_, _] if col canEqual this => (iterableFactory, col.iterableFactory) match {
					case (Some(x), Some(y)) => x == y
					case _ => false
				}
				case _ => false
			}

			override def hashCode :Int = iterableFactory.mapOrElse(_.hashCode, System.identityHashCode(this))

			override def toString :String = iterableFactory.mapOrElse(_.innerClassName, factory.innerClassName)
		}

		private object ToIterable extends ToCollection[Iterable[Any], Any](scala.Iterable) {
			override def apply(items :Iterable[Any]) :Iterable[Any] = items
			override def attempt(items :Iterable[Any]) :Opt[Iterable[Any]] = Got(items)

			override def toString = "Iterable"
		}

		private case class ToMap[M[_, _], K, V](factory :Factory[(K, V), M[K, V]])(override val toString :String)
			extends Custom[M[K, V], K -> V](
				map => factory.fromSpecific(map.view.map { case _1 -> _2 => (_1,  _2) }))
			   with ConstructFrom[M[K, V], K -> V]
		{
			override def builder = new Builder[K -> V, M[K, V]] {
				private val target = factory.newBuilder

				override def clear() :Unit = target.clear()

				override def result() = target.result()

				override def addOne(elem :K -> V) = {
					target += ((elem._1, elem._2)); this
				}
			}

			override def compatibleWith(decomposer :DecomposableTo[M[K, V], _]) :Boolean =
				DecomposableTo.Map.unapply(decomposer)
		}

		private object ToMap extends ToMap[Map, Any, Any](immutable.Map)("Map")

		private class ToOrdered[S, E](val factory :Factory[E, S])(override val toString :String)
			extends Custom[S, Int -> E](_.toArray.sorted(Ordering.by((_ :Int -> E)._1)).view.map(_._2).to(factory))
			   with ConstructFrom[S, Int -> E]
		{
			override def builder =
				Array.newBuilder[Int -> E].mapResult(_.sorted(Ordering.by((_ :Int -> E)._1)).view.map(_._2).to(factory))

			def iterableFactory = iterableFactoryOf(factory)

			override def compatibleWith(decomposer :DecomposableTo[S, _]) :Boolean =
				DecomposableTo.Ordered.unapply(decomposer)

			override def canEqual(that :Any) :Boolean = that.isInstanceOf[ToOrdered[_, _]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case col :ToOrdered[_, _] if col canEqual this => (iterableFactory, col.iterableFactory) match {
					case (Some(x), Some(y)) => x == y
					case _ => false
				}
				case _ => false
			}

			override def hashCode :Int = iterableFactory.mapOrElse(_.hashCode, System.identityHashCode(this))
		}

		private object ToOrdered extends ToOrdered[Seq[Any], Any](Seq)("Seq")

		private object ToNothing extends ComposableFrom[Nothing, Any] {
			override def arity = Arity._0_1
			override def attempt(items :Iterable[Any]) = Lack
			override def compatibleWith(decomposer :DecomposableTo[Nothing, _]) = false
			override def toString = "Nothing"
		}



		private val ToFactoryClass = scala.collection.Iterable.iterableFactory.getClass
		private val iterableFactoryField = ToFactoryClass.getFields.find(_.getType == classOf[IterableFactory[Iterable]])

		private def iterableFactoryOf[E, T](factory :Factory[E, T]) :Option[IterableFactory[Iterable]] =
			if (ToFactoryClass isAssignableFrom factory.getClass)
				iterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[Iterable]])
			else None
	}






	/** A way to obtain `Iterable[E]` from `C`. */
	trait DecomposableTo[-C, +E] extends Serializable { decomposition =>
		def arity :Arity

		def apply(composite :C) :Iterable[E]

		def unapply(composite :C) :Opt[Iterable[E]] = Got(apply(composite))

		def unapply(kin :Kin[C]) :Opt[Iterable[E]] =
			if (kin.isPresent) Got(apply(kin.get)) else Lack

		def first(composite :C) :E

		def composeWith[W <: C, T >: E](composition :ComposableFrom[W, T]) :W ComposedOf T =
			new ComposedOf.Hybrid(composition, this)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[DecomposableTo[_, _]]

		def compatibleWith[X <: C](other :DecomposableTo[X, _]): Boolean = other eq this

		def compatibleWith(composer :ComposableFrom[C, _]) :Boolean = composer.compatibleWith(this)
	}



	/** A narrowed down version of `C DecomposableTo E` which attests that the created composite can constitute
	  * of variable/multiple number of values of `E`. This specifically excludes `E DecomposableTo E` to make
	  * some operations available only for actual collection types as well as to eliminate the risk associated with
	  * an always available implicit value for the identity decomposition.
	  */
	trait ExtractAs[-C, +E] extends DecomposableTo[C, E] {
		def composeWith[W <: C, T >: E](composition :W ConstructFrom T) :W CollectionOf T =
			CollectionOf(composition, this)
	}

	/** Factory and matcher for extractors of individual elements from collection types. */
	object ExtractAs {
		def apply[T, E](implicit extract :T ExtractAs E) :T ExtractAs E = extract


		def unapply[T, E](implicit decomposition :T DecomposableTo E) :Opt[T ExtractAs E] = decomposition match {
			case extract :ExtractAs[T, E] => Got(extract)
			case _ => Lack
		}
		def unapply[T, E](composite :T ComposedOf E) :Opt[T ExtractAs E] = unapply(composite.decomposer)

	}



	abstract class FallbackDecomposableToImplicits {
		/** A decomposer of type `T` into a singleton `Iterable[T]`. */
		implicit def itself[T] :T DecomposableTo T = DecomposableTo.FromSelf.asInstanceOf[DecomposableTo[T, T]]
	}

	abstract class IndexedDecomposableToImplicits extends FallbackDecomposableToImplicits {
		implicit def indexed[T] :Iterable[T] ExtractAs (Int -> T) = DecomposableTo.Ordered()
	}



	/** Factory and container of implicit values for disassemblers converting a composite type `C` into an `Iterable[E]`. */
	object DecomposableTo extends IndexedDecomposableToImplicits {

		/** Summons an implicitly available instance of `C DecomposableTo E`. */
		def apply[C, E](implicit ev :C DecomposableTo E) :C DecomposableTo E = ev

		/** Creates a custom decomposer using the specified function and of the given arity. */
		def apply[T, E](decompose :T => Iterable[E], arity :Arity = Arity._0_n) :T DecomposableTo E =
			new Custom(decompose, arity)


		def unapply[T, E](composite :T ComposedOf E) :Opt[T DecomposableTo E] = Got(composite.decomposer)

		def unapply[T, E](kin :Derived[E, T]) :Opt[Iterable[E]] = kin.items

		def unapply[T, E](kin :Kin[T])(implicit decomposition :T DecomposableTo E) :Opt[Iterable[E]] =
			kin match {
				case composite :Derived[E @unchecked, T @unchecked]
						if composite.composition compatibleWith decomposition =>
					Got(composite.items.asInstanceOf[Iterable[E]])
				case _ => Lack
			}

		/** A decomposer of type `Map[K, V]` into an `Iterable[K -> V]`. */
		implicit def map[K, V] :Map[K, V] ExtractAs (K -> V) = FromMap.asInstanceOf[ExtractAs[Map[K, V], K->V]]

		/** A decomposer of `Iterable[T]` - and, by extension, any of its subtypes - returning it as-is. */
		implicit def iterable[T] :Iterable[T] ExtractAs T = FromIterable.asInstanceOf[ExtractAs[Iterable[T], T]]

		/** A decomposer of `Option[T]` into an `Iterable[T]`. */ //consider: making these two DecomposableTo
		implicit def option[T] :Option[T] ExtractAs T = FromOption.asInstanceOf[ExtractAs[Option[T], T]]

//		/** A decomposer of `Opt[T]` into an `Iterable[T]`. */
//		implicit def opt[T] :Opt[T] ExtractAs T = FromOpt.asInstanceOf[ExtractAs[Opt[T], T]]



		/** A factory and matcher for the identity decomposition witnessing that values of `T` can be extracted from `T` itself. */
		object Self {
			def apply[T]() :T DecomposableTo T = FromSelf.asInstanceOf[T DecomposableTo T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean = decomposition == FromSelf

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.decomposer == FromSelf
		}


		/** A factory and matcher for the decomposition of `Option[T]` into zero or one values of `T`. */
		object Option {
			def apply[T]() :Option[T] ExtractAs T = FromOption.asInstanceOf[Option[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean = decomposition == FromOption

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.decomposer == FromOption
		}


//		/** A factory and matcher for the decomposition of `Opt[T]` into zero or one values of `T`. */
//		object Opt {
//			def apply[T]() :Opt[T] ExtractAs T = FromOpt.asInstanceOf[Opt[T] ExtractAs T]
//
//			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean = decomposition == FromOpt
//
//			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.decomposer == FromOpt
//		}


		/** A factory and matcher for the decomposition of `Iterable[E]` returning simply its argument as the collection
		  * of elements.
		  */
		object Iterable {
			def apply[T]() :Iterable[T] ExtractAs T = FromIterable.asInstanceOf[Iterable[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean = decomposition == FromIterable

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean = composedOf.decomposer == FromIterable
		}


		/** A factory and matcher for the decomposition of `Map[K, V]` returning its elements as `K->V`.
		  * @see [[net.noresttherein.oldsql.model.-> ->]]
		  */
		object Map {
			def apply[K, V]() :Map[K, V] ExtractAs (K -> V) = FromMap.asInstanceOf[Map[K, V] ExtractAs (K->V)]

			def unapply[T, E](decomposer :T DecomposableTo E) :Boolean = decomposer == FromMap

			def unapply[T, E](composition :T ComposedOf E) :Boolean = composition.decomposer == FromMap
		}


		/** A factory and matcher for the decomposition of `Seq[V]`
		  * into `Int `[[net.noresttherein.oldsql.model.-> ->]]` V`, where the first element of the pair
		  * is the zero-based index of the given element in the sequence.
		  */
		object Ordered {
			def apply[V]() :Iterable[V] ExtractAs (Int -> V) = FromIndexed.asInstanceOf[Iterable[V] ExtractAs (Int -> V)]

			def unapply[T, E](decomposer :T DecomposableTo E) :Boolean = decomposer == FromIndexed

			def unapply[T, E](composition :T ComposedOf E) :Boolean = composition.decomposer == FromIndexed
		}



		/** A custom decomposer using the provided function. */
		case class Custom[C, E](decompose :C => Iterable[E], arity :Arity = Arity._0_n) extends DecomposableTo[C, E] {
			override def apply(composite: C): Iterable[E] = decompose(composite)

			override def first(composite :C) :E = decompose(composite).head

			override def toString :String = arity.toString
		}


		private[ComposedOf] case object FromSelf extends DecomposableTo[Any, Any] {
			override def arity = Arity._1

			override def apply(composite: Any): Iterable[Any] = Some(composite)
			override def first(composite :Any) :Any = composite

			override def toString = "One" //">:"
		}

		private case object FromOption extends ExtractAs[Option[Any], Any] {
			override def arity = Arity._0_1

			override def apply(composite :Option[Any]) :Iterable[Any] = composite
			override def first(composite :Option[Any]) :Any = composite.get

			override def toString = "Option"
		}

//		private case object FromOpt extends ExtractAs[Opt[Any], Any] {
//			override def arity = Arity._0_1
//
//			override def apply(composite :Opt[Any]) :Iterable[Any] =
//				if (composite.isDefined) composite.get::Nil else Nil
//			override def first(composite :Opt[Any]) :Any = composite.get
//
//			override def toString = "Opt"
//		}

		private case object FromIterable extends ExtractAs[Iterable[Any], Any] {
			override def arity = Arity._0_n

			override def apply(composite :Iterable[Any]): Iterable[Any] = composite
			override def first(composite :Iterable[Any]) :Any = composite.head

			override def toString = "Iterable"
		}

		private case object FromMap extends ExtractAs[Map[Any, Any], Any->Any] {
			override def arity :Arity = Arity._0_n

			override def apply(composite :Map[Any, Any]) :Iterable[Any -> Any] =
				composite.map { case (_1, _2) => ->(_1, _2) }

			override def first(composite :Map[Any, Any]) :Any -> Any = {
				val (_1, _2) = composite.head
				->(_1, _2)
			}

			override def toString = "Map"
		}

		private case object FromIndexed extends ExtractAs[Iterable[Any], Int -> Any] {
			override def arity = Arity._0_n

			override def apply(composite :Iterable[Any]) :Iterable[Int -> Any] =
				composite.view.zipWithIndex.map { case (v, idx) => ->(idx, v) }

			override def first(composite :Iterable[Any]) :Int -> Any = ->(0, composite.head)

			override def toString = "Ordered"
		}

	}

}

