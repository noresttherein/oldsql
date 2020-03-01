package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.ComposedOf.{Arity, ComposableFrom, DecomposableTo}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom.{Collection, Custom}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.SaferCasts._

import scala.collection.generic.CanBuildFrom




/** Proof that type `C` is in some way composed of values of `E` and can be converted to and from `Iterable[E]`
  * (possibly with some restrictions in place). Serves as a single umbrella for collection types
  * `( _&lt;: Iterable[E])`, `Option[E]` and `E` itself, allowing to create queries for `E` with result type `C`.
  * In order to statically exclude the identity composition (as it can lead to non obvious bugs, you can demand
  * an implicit of its subtype instead: `C CollectionOf E`.
  * It is generally written in the code using infix notation for clarity: `C ComposedOf E`.
  * @see [[net.noresttherein.oldsql.model.ComposedOf.CollectionOf CollectionOf]]
  */
trait ComposedOf[C, E] {
	def arity :Arity = composer.arity
//		if (composition.arity == decomposition.arity) composition.arity
//		else throw new UnsupportedOperationException(s"ComposedOf.arity: composition arity (${composition.arity}) is different than decomposition arity (${decomposition.arity}) in $this. Most probabaly a bug.")

	implicit val composer :C ComposableFrom E
	implicit val decomposer :C DecomposableTo E

	def apply(items: Iterable[E]): C = composer(items)

	def apply(composite: C): Iterable[E] = decomposer(composite)


	def unapply(composite :C) :Some[Iterable[E]] = decomposer.unapply(composite)

	def unapply(kin :Kin[C]) :Option[C ComposedOf E] = kin match {
		case ComposedOf(composed) if this.compatibleWith(composed) =>
			Some(composed.asInstanceOf[ComposedOf[C, E]])
		case ComposableFrom(co) && DecomposableTo(deco) if compatibleWith(decomposer) =>
			Some(ComposedOf(composer, deco.asInstanceOf[DecomposableTo[C, E]]))
		case _ :CompositeKin[_, _] => None
		case _ => Some(this)
	}



	def rebuild(value: C): C = composer(decomposer(value))


	def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = composer.compatibleWith(other)

	def compatibleWith[X <: C](other: DecomposableTo[X, _]) :Boolean = decomposer.compatibleWith(other)

	def compatibleWith[X <:C](other : X ComposedOf _) :Boolean =
		other.composer.compatibleWith(composer) && compatibleWith(other.decomposer)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedOf[_, _]]

	override def equals(obj: scala.Any): Boolean = obj match {
		case that :ComposedOf[_, _] =>
			(that eq this) || that.canEqual(this) && composer == that.composer && decomposer == that.decomposer
		case _ => false
	}

	override def hashCode :Int = (composer, decomposer).hashCode

//	override def toString :String =


	object Present {

		def unapply(kin :Kin[C]) :Option[Seq[E]] =
			kin match {
				case DecomposableTo(deco) && Kin.Present(value) =>
					if (decomposer == deco)
						Some(decomposer(value).toSeq)
					else {
						throw new IllegalArgumentException(
							s"Cannot decompose kin $kin: associated decomposer $deco doesn't seem to be compatible " +
								s"with the requested decomposer $decomposer.")
					}
				case Kin.Present(value) =>
					Some(apply(value).toSeq)
				case _ => None
			}
	}


	override def toString :String = {
		val (c, d) = (composer.toString, decomposer.toString)
		if (c == d) c
		else if (c == "Collection" && d == "Iterable") "Collection"
		else c + "/" +  "d"
	}

}





object ComposedOf {

	/** Summons the implicitly available instance of `C ComposedOf E`. */
	def apply[C, E](implicit ev :C ComposedOf E) :C ComposedOf E = ev

	/** Creates a custom composition from the passed functions. Note that unless the functions provided
	  * override equals in a meaningful way, subsequent calls to this method will yield objects which won't be equal.
	  */
	def apply[C, E](compose :Iterable[E] => C, decompose :C=>Iterable[E]) :C ComposedOf E =
		new Hybrid(ComposableFrom(compose), DecomposableTo(decompose))

	/** Create a `C ComposedOf E` using the provided composer and decomposer. */
	def apply[C, E](compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		new Hybrid(compose, decompose)


	/** Extracts an instance of `ComposedOf` out of the reference if available. */
	def unapply[T](reference :Kin[T]) :Option[ComposedOf[_<:T, _]] = reference match {
		case c :CompositeKin[_, _] =>
			val cast = c.asInstanceOf[CompositeKin[T, Any]]
			Some(cast.items)
		case _ => None
	}



	/** An implicit value witnessing that `Iterable[E]` is composed of values of `E`. */
	implicit def iterable[E] :Iterable[E] CollectionOf E = CollectionOf(ComposableFrom.iterable[E], DecomposableTo.iterable[E])

	/** Combines implicitly available composer and decomposer for the type pair `C`, `E` into a `C ComposedOf E`.
	  * Returned instance overrides equality in terms of equality of its constituents. This may potentially lead to
	  * issues with custom collection classes, as the default implicit value for `C ComposableFrom E` is based on
	  * an implicit `CanBuildFrom[_, E, C]`. Currently all standard scala collections however return always the same
	  * builder factory instance, resulting in the desired behaviour of two instances created by this method
	  * for the same pair of types being equal.
	  */
	implicit def combine[C, E](implicit compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		new Hybrid[C, E](compose, decompose)

	/** A witness that `Option[E]` is composed of `E` and can be converted to and from a (singleton) `Iterable[E]`. */
	implicit def option[T] :Option[T] CollectionOf T = opt.asInstanceOf[Option[T] CollectionOf T]

	/** A witness that any type `T` consists of itself. This is useful as it eliminates special treatment of result types
	  * in most scenarios, but as it is always implicitly available, some care needs to be taken to make sure that it
	  * is not used instead of the proper one for actual composite types.
	  */
	implicit def itself[T] :T ComposedOf T = identity.asInstanceOf[ComposedOf[T, T]]




	/** An implicit witness that type `C` is strictly composed of a variable or multiple number of values of type `E`.
	  * This particularly excludes instances of `C ComposedOf E` so that operations which either make no sense or can
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
		@inline implicit def combined[C, E](implicit compose :C ConstructFrom E, decompose :C ExtractAs E) :C CollectionOf E =
			apply(compose, decompose)

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



		def unapply[T](reference :Kin[T]) :Option[CollectionOf[_<:T, _]] = reference match {
			case c :CompositeKin[_, _] if c.items.isInstanceOf[CollectionOf[_, _]] =>
				Some(c.items.asInstanceOf[CollectionOf[T, _]])
			case _ =>
				None
		}

	}




	/** Restriction on the number of values on one side of a relation. When used in ComposedOf, it defines how many elements
	  * Iterable[E] obtained from C can have.
	  */
	trait Arity

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
		case object _1_n extends AtLeastOne {
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



	private[this] val identity = ComposedOf[Any, Any](ComposableFrom.itself[Any], DecomposableTo.itself[Any])
	private[this] val opt = CollectionOf(ComposableFrom.Optional[Any](), DecomposableTo.Optional[Any]())

	private class Hybrid[C, E](val composer :ComposableFrom[C, E], val decomposer :DecomposableTo[C, E])
		extends ComposedOf[C, E]






	/** A way to create value of `C` from `Iterable[E]`. `C` might be the type `E` itself, a collection of `E`
	  * or some custom composite object constructed from multiple parts such as a matrix. For this reason
	  * the construction might not be possible for all collections of `E`, in particular based on its size.
	  * @see [[net.noresttherein.oldsql.model.ComposedOf.DecomposableTo DecomposableTo]]
	  * @see [[net.noresttherein.oldsql.model.ComposedOf ComposedOf]]
	  */
	trait ComposableFrom[+C, -E]  { composition =>
		def arity :Arity

		def empty :C = apply(Seq())

		def apply(items :Iterable[E]) :C = attempt(items) getOrElse {
			throw new IllegalArgumentException(s"Can't perform composition $this for $items.")
		}


		def attempt(items :Iterable[E]) :Option[C]

		def decomposeWith[W>:C, T<:E](decomposition :DecomposableTo[W, T]) :ComposedOf[W, T] =
			new ComposedOf.Hybrid[W, T](this, decomposition)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposableFrom[_, _]]

		def compatibleWith[X>:C](other :ComposableFrom[X, _]) :Boolean = this eq other

	}



	/** A trait extended by `ComposableFrom` implementations using actual collection classes as `C`.
	  * This especially excludes the identity composition `C=:=E`. As an implicit value for latter is always available,
	  * it might easily lead to situations where a collection type is treated as composed of itself rather than its
	  * items, leading to unobvious bugs.
	  */
	trait ConstructFrom[+C, -E] extends ComposableFrom[C, E] {
		def decomposeWith[W >: C, T <: E](decomposition :ExtractAs[W, T]) :CollectionOf[W, T] =
			CollectionOf(this, decomposition)
	}

	object ConstructFrom {
		/** Summons an implicitly available instance of `C ConstructFrom E`. */
		@inline def apply[C, E](implicit compose :C ConstructFrom E) :C ConstructFrom E = compose


		def unapply[C, E](composite :C ComposedOf E) :Option[ConstructFrom[C, E]] =
			composite.composer match {
				case c :ConstructFrom[C, E] => Some(c)
				case _ => None
			}

		def unapply[T](kin :Kin[T]) :Option[ConstructFrom[T, _]] = kin match {
			case c :CompositeKin[_, _] if c.items.isInstanceOf[ConstructFrom[_, _]] =>
				Some(c.items.asInstanceOf[ConstructFrom[T, _]])
			case _ => None
		}
	}



	sealed abstract class FallbackComposableFromImplicits {
		implicit def canBuildFrom[C, T](implicit cbf :CanBuildFrom[_, T, C]) :C ComposableFrom T =
			new Custom(items => (cbf() ++= items).result())

		implicit def collection[C<:Iterable[T], T](implicit cbf :CanBuildFrom[_, T, C]) :C ConstructFrom T =
			Collection[C, T]()

	}



	/** Factory and matcher as well as the container for implicit values of `C ComposableFrom E` which carry
	  * information about how values of type `C` can be created from a collection of values of type `E`.
	  */
	object ComposableFrom extends FallbackComposableFromImplicits {

		/** Summons an implicitly available instance of `C ComposableFrom E`. */
		def apply[C, E](implicit ev :C ComposableFrom E) : C ComposableFrom E = ev

		/** Creates a custom composer backed by the given function. */
		def apply[C, E](compose :Iterable[E]=>C) :ComposableFrom[C, E] = new Custom(compose)



		def unapply[T](kin :Kin[T]) :Option[ComposableFrom[T, _]] = kin match {
			case c :CompositeKin[_, _] => Some(c.composer.asInstanceOf[ComposableFrom[T, _]])
			case _ => None
		}


		/** Identity composition available for all types witnessing that a singleton `Iterable[T]` can be converted to
		  * the type `T` itself. This instance will always throw an exception if the input collection contains
		  * any number of elements different than 1.
		  */
		implicit def itself[T] :T ComposableFrom T = Super.asInstanceOf[ComposableFrom[T, T]]

		/** Composition of `Option[T]` out of an `Iterable[T]`. If the input iterable contains more than one element,
		  * an exception is thrown.
		  */
		implicit def optional[T] :Option[T] ConstructFrom T = Opt.asInstanceOf[ConstructFrom[Option[T], T]]

		/** A composition for type `Iterable[T]` itself using an identity function. */
		implicit def iterable[T] :Iterable[T] ConstructFrom T = Iter.asInstanceOf[ConstructFrom[Iterable[T], T]]



		/** Factory and matcher for identity compositions witnessing that `S ComposedOf T` for any type `S >: T`. */
		object Superclass {
			def apply[T]() :T ComposableFrom T = Super.asInstanceOf[T ComposableFrom T]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Super

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composer == Super

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Super)) => true
				case _ => false
			}
		}

		/** Factory and matcher for the composition of `Option[T]` out of one or zero values of `T`. */
		object Optional {
			def apply[T](): Option[T] ConstructFrom T = Opt.asInstanceOf[Option[T] ConstructFrom T]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Opt

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composer == Opt

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Opt)) => true
				case _ => false
			}

		}

		/** Factory and matcher for the composition of the type `Iterable[T]` itself. */
		object Iterable {
			def apply[T](): Iterable[T] ConstructFrom T = Iter.asInstanceOf[Iterable[T] ConstructFrom T]


			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Iter

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composer == Iter

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Iter)) => true
				case _ => false
			}
		}

		/** Factory and matcher for instances of `C ConstructFrom E` for types `C &lt;: Iterable[E]` using
		  * implicitly available `CanBuildFrom[_, E, C]`.
		  */
		object Collection {
			/** Returns a factory of instances `_ ComposedOf E`, allowing a possibly shorter syntax of
			  * `Collection[E].in[Seq]` rather than `Collection[Seq[E], E]()`.
			  */
			@inline def apply[E] :CollectionComposer[E] = new CollectionComposer[E] {}

			trait CollectionComposer[E] extends Any {
				def as[T <: Iterable[E]](implicit cbf :CanBuildFrom[_, E, T]) :T ConstructFrom E =
					new Collection[T, E]

				def in[C[X] <: Iterable[X]](implicit cbf :CanBuildFrom[_, E, C[E]]) :C[E] ConstructFrom E =
					new Collection[C[E], E]
			}



			def apply[T <: Iterable[E], E]()(implicit cbf :CanBuildFrom[_, E, T]) :T ConstructFrom E =
				new Collection[T, E]


			def unapply[T, E](composition :T ComposableFrom E) :Option[CanBuildFrom[_, E, T]] = composition match {
				case col :Collection[T, E] => Some(col.cbf)
				case _ => None
			}

			def unapply[T, E](composedOf :T ComposedOf E) :Option[CanBuildFrom[_, E, T]] = composedOf.composer match {
				case col :Collection[T, E] => Some(col.cbf)
				case _ => None
			}

			def unapply[T](kin :Kin[T]) :Option[CanBuildFrom[_, _, T]] = kin match {
				case ComposedOf(comp) => unapply(comp)
				case _ => None
			}

		}



		/** A custom `C ComposableFrom E` instance backed by the provided function. */
		case class Custom[C, E](private val compose :Iterable[E]=>C, arity :Arity = Arity._0_n)
			extends ComposableFrom[C, E]
		{
			override def apply(items: Iterable[E]): C = compose(items)
			override def attempt(items: Iterable[E]): Option[C] = Some(compose(items))

			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean =
				other.canEqual(this) && this == other

			override def toString :String = arity.toString
		}



		private case object Super extends ComposableFrom[Any, Any] {
			def arity = Arity._1
			override def attempt(items: Iterable[Any]) :Option[Any] = items.size == 1 ifTrue items.head
			override def toString = "One" //">:"
		}


		private case object Opt extends ConstructFrom[Option[Any], Any] {
			def arity = Arity._0_1

			override def attempt(items: Iterable[Any]) :Option[Option[Any]] =
				items.headOption.providing(items.size <= 1) orElse None

			override def toString = "Option"
		}


		private[ComposedOf] class Collection[C, E](implicit val cbf :CanBuildFrom[_, E, C])
			extends Custom[C, E](iter => (cbf() ++= iter).result) with ConstructFrom[C, E]
		{   //cbf is often a reusable, single instance
			override def canEqual(that :Any) :Boolean = that.isInstanceOf[Collection[_, _]]

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case col :Collection[_, _] if col canEqual this => col.cbf == cbf
				case _ => false
			}

			override def hashCode :Int = cbf.hashCode

			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = canEqual(other)

			override def toString = "Collection"
		}


		private object Iter extends Collection[Iterable[Any], Any] {
			override def apply(items :Iterable[Any]) :Iterable[Any] = items
			override def attempt(items :Iterable[Any]) :Option[Iterable[Any]] = Some(items)

			override def toString = "Iterable"
		}

	}








	/** A way to obtain `Iterable[E]` from `C`. */
	trait DecomposableTo[-C, +E]  { decomposition =>
		def arity :Arity

		def apply(composite :C) :Iterable[E]

		def unapply(composite :C) = Some(apply(composite))

		def first(composite :C) :E

		def composeWith[W<:C, T>:E](composition :ComposableFrom[W, T]) :W ComposedOf T =
			new ComposedOf.Hybrid(composition, this)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[DecomposableTo[_, _]]

		def compatibleWith[X <: C](other: DecomposableTo[X, _]): Boolean = other eq this

	}



	/** A narrowed down version of `C DecomposableTo E` which attests that the created composite can constitute
	  * out of variable/multiple number of values of `E`. This specifically excludes `E DecomposableTo E` to make
	  * some operations available only for actual collection types as well as eliminate the risk associated with
	  * an always available implicit value for the identity decomposition.
	  */
	trait ExtractAs[-C, +E] extends DecomposableTo[C, E] {
		def composeWith[W<:C, T>:E](composition :W ConstructFrom T) :W CollectionOf T =
			CollectionOf(composition, this)
	}

	/** Factory and matcher for extractors of individual elements from collection types. */
	object ExtractAs {
		def apply[C, E](implicit extract :C ExtractAs E) :C ExtractAs E = extract


		def unapply[C, E](composite :C ComposedOf E) :Option[C ExtractAs E] =
			composite.decomposer match {
				case deco :ExtractAs[C, E] => Some(deco)
				case _ => None
			}

		def unapply[T](kin :Kin[T]) :Option[T ExtractAs _] = kin match {
			case c :CompositeKin[T, _] if c.decomposer.isInstanceOf[ExtractAs[_, _]] =>
				Some(c.decomposer.asInstanceOf[T ExtractAs _])
			case _ => None
		}

	}




	/** Factory and container of implicit values for disassemblers converting a composite type `C` into an `Iterable[E]`. */
	object DecomposableTo {

		/** Summons an implicitly available instance of `C DecomposableTo E`. */
		def apply[C, E](implicit ev :C DecomposableTo E) :C DecomposableTo E = ev

		/** Creates a custom decomposer using the specified function and of the given arity. */
		def apply[C, E](decompose :C=>Iterable[E], arity :Arity = Arity._0_n) :DecomposableTo[C, E] =
			new Custom(decompose, arity)


		def unapply[T](kin :Kin[T]) :Option[DecomposableTo[_<:T, _]] = kin match {
			case c :CompositeKin[_, _] => Some(c.decomposer)
			case _ => None
		}

		/** A decomposer of `Iterable[T]` - and, by extension, any of its subtypes - returning it as-is. */
		implicit def iterable[T] :Iterable[T] ExtractAs T = Iter.asInstanceOf[ExtractAs[Iterable[T], T]]

		/** A decomposer of `Option[T]` into an `Iterable[T]`. */
		implicit def optional[T] :Option[T] ExtractAs T = Opt.asInstanceOf[ExtractAs[Option[T], T]]

		/** A decomposer of type `T` into a singleton `Iterable[T]`. */
		implicit def itself[T] :T DecomposableTo T = Sub.asInstanceOf[DecomposableTo[T, T]]


		/** Factory and matcher for the identity decomposition witnessing that values of `T` can be extracted from `T` itself. */
		object Subclass {
			def apply[T]() :T DecomposableTo T = Sub.asInstanceOf[T DecomposableTo T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Sub

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposer == Sub

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Sub)) => true
				case _ => false
			}
		}

		/** Factoryand matcher for the decomposition of `Option[T]` into zero or one values of `T`. */
		object Optional {
			def apply[T]() :Option[T] ExtractAs T = Opt.asInstanceOf[Option[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Opt

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposer == Opt

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Opt)) => true
				case _ => false
			}
		}

		/** Factory and matchere for the decomposition of `Iterable[E]` returning simply its argument as the collection
		  * of elements.
		  */
		object Iterable {
			def apply[T]() :Iterable[T] ExtractAs T = Iter.asInstanceOf[Iterable[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Iter

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposer == Iter

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Iter)) => true
				case _ => false
			}
		}



		/** A custom decomposer using the provided function. */
		case class Custom[C, E](decompose :C=>Iterable[E], arity :Arity = Arity._0_n) extends DecomposableTo[C, E] {
			override def apply(composite: C): Iterable[E] = decompose(composite)

			override def first(composite :C) :E = decompose(composite).head

			override def toString :String = arity.toString
		}



		private case object Sub extends DecomposableTo[Any, Any] {
			def arity = Arity._1

			override def apply(composite: Any): Iterable[Any] = Seq(composite)
			override def first(composite :Any) :Any = composite

			override def toString = "One" //">:"
		}

		private case object Opt extends ExtractAs[Option[Any], Any] {
			def arity = Arity._0_1

			override def apply(composite :Option[Any]) :Iterable[Any] = composite.toSeq
			override def first(composite :Option[Any]) :Any = composite.get

			override def toString = "Option"
		}

		private case object Iter extends ExtractAs[Iterable[Any], Any] {
			def arity = Arity._0_n

			override def apply(composite: Iterable[Any]): Iterable[Any] = composite
			override def first(composite :Iterable[Any]) :Any = composite.head

			override def toString = "Iterable"
		}

	}


}

