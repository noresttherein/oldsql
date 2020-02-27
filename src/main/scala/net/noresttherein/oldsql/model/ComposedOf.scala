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
	def arity :Arity = composition.arity
//		if (composition.arity == decomposition.arity) composition.arity
//		else throw new UnsupportedOperationException(s"ComposedOf.arity: composition arity (${composition.arity}) is different than decomposition arity (${decomposition.arity}) in $this. Most probabaly a bug.")

	val composition :C ComposableFrom E
	val decomposition :C DecomposableTo E

	def apply(items: Iterable[E]): C = composition(items)

	def apply(composite: C): Iterable[E] = decomposition(composite)


	def unapply(composite :C) :Some[Iterable[E]] = decomposition.unapply(composite)

	def unapply(kin :Kin[C]) :Option[C ComposedOf E] = kin match {
		case ComposedOf(composed) if this.compatibleWith(composed) =>
			Some(composed.asInstanceOf[ComposedOf[C, E]])
		case ComposableFrom(co) && DecomposableTo(deco) if compatibleWith(decomposition) =>
			Some(ComposedOf(composition, deco.asInstanceOf[DecomposableTo[C, E]]))
		case _ :CompositeKin[_, _] => None
		case _ => Some(this)
	}



	def rebuild(value: C): C = composition(decomposition(value))


	def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = composition.compatibleWith(other)

	def compatibleWith[X <: C](other: DecomposableTo[X, _]) :Boolean = decomposition.compatibleWith(other)

	def compatibleWith[X <:C](other : X ComposedOf _) :Boolean =
		other.composition.compatibleWith(composition) && compatibleWith(other.decomposition)


	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComposedOf[_, _]]

	override def equals(obj: scala.Any): Boolean = obj match {
		case that :ComposedOf[_, _] =>
			(that eq this) || that.canEqual(this) && composition == that.composition && decomposition == that.decomposition
		case _ => false
	}

	override def hashCode :Int = (composition, decomposition).hashCode

//	override def toString :String =


	object Present {

		def unapply(kin :Kin[C]) :Option[Seq[E]] =
			kin match {
				case DecomposableTo(deco) && Kin.Present(value) =>
					if (decomposition == deco)
						Some(decomposition(value).toSeq)
					else {
						throw new IllegalArgumentException(
							s"Cannot decompose kin $kin: associated decomposition $deco doesn't seem to be compatible " +
								s"with the requested decomposition $decomposition.")
					}
				case Kin.Present(value) =>
					Some(apply(value).toSeq)
				case _ => None
			}
	}


	override def toString :String = {
		val (c, d) = (composition.toString, decomposition.toString)
		if (c == d) c
		else if (c == "Collection" && d == "Iterable") "Collection"
		else c + "/" +  "d"
	}

}





object ComposedOf {

	def apply[C, E](implicit ev :C ComposedOf E) :C ComposedOf E = ev

	def apply[C, E](compose :Iterable[E] => C, decompose :C=>Iterable[E]) :C ComposedOf E =
		new Hybrid(ComposableFrom(compose), DecomposableTo(decompose))

	def apply[C, E](compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		new Hybrid(compose, decompose)

	def unapply[T](reference :Kin[T]) :Option[ComposedOf[_<:T, _]] = reference match {
		case c :CompositeKin[_, _] =>
			val cast = c.asInstanceOf[CompositeKin[T, Any]]
			Some(cast.items)
		case _ => None
	}



	implicit def iterable[E] :Iterable[E] CollectionOf E = CollectionOf(ComposableFrom.iterable[E], DecomposableTo.iterable[E])

	implicit def hybrid[C, E](implicit compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		new Hybrid[C, E](compose, decompose)

	implicit def option[T] :Option[T] CollectionOf T = opt.asInstanceOf[Option[T] CollectionOf T]

	implicit def itself[T] :T ComposedOf T = identity.asInstanceOf[ComposedOf[T, T]]




	/** An implicit witness that type `C` is strictly composed of a variable or multiple number of values of type `E`. */
	trait CollectionOf[C, E] extends ComposedOf[C, E] {
		override val composition :C ConstructFrom E
		override val decomposition :C ExtractAs E

		override def toString :String = composition.toString
	}

	object CollectionOf {
		@inline implicit def combined[C, E](implicit compose :C ConstructFrom E, decompose :C ExtractAs E) :C CollectionOf E =
			apply(compose, decompose)

		def apply[C, E](implicit composition :C CollectionOf E) :C CollectionOf E = composition

		def apply[C, E](compose :C ConstructFrom E, decompose :C ExtractAs E) :C CollectionOf E =
			new CollectionOf[C, E] {
				override val composition = compose
				override val decomposition = decompose
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

		case object _1 extends AtMostOne with AtLeastOne {
			override def toString = "1"
		}
		case object _0_1 extends AtMostOne {
			override def toString = "0-1"
		}
		case object _0_n extends Arity {
			override def toString = "0-n"
		}
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



	private[this] val identity = ComposedOf[Any, Any](ComposableFrom.self[Any], DecomposableTo.self[Any])
	private[this] val opt = CollectionOf(ComposableFrom.Optional[Any](), DecomposableTo.Optional[Any]())

	private class Hybrid[C, E](val composition :ComposableFrom[C, E], val decomposition :DecomposableTo[C, E])
		extends ComposedOf[C, E]



	/** A way to create value of C from Iterable[E] */
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



	/** A trait extended by `ComposableFrom` implementations using actual collection classes as `C`. */
	trait ConstructFrom[+C, -E] extends ComposableFrom[C, E] {
		def decomposeWith[W >: C, T <: E](decomposition :ExtractAs[W, T]) :CollectionOf[W, T] =
			CollectionOf(this, decomposition)
	}

	object ConstructFrom {
		@inline def apply[C, E](implicit compose :C ConstructFrom E) :C ConstructFrom E = compose


		def unapply[C, E](composite :C ComposedOf E) :Option[ConstructFrom[C, E]] =
			composite.composition match {
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



	object ComposableFrom extends FallbackComposableFromImplicits {
		def apply[C, E](implicit ev :C ComposableFrom E) : C ComposableFrom E = ev

		def apply[C, E](compose :Iterable[E]=>C) :ComposableFrom[C, E] = new Custom(compose)

		def unapply[T](kin :Kin[T]) :Option[ComposableFrom[T, _]] = kin match {
			case c :CompositeKin[_, _] => Some(c.composition.asInstanceOf[ComposableFrom[T, _]])
			case _ => None
		}


		implicit def self[T] :T ComposableFrom T = Super.asInstanceOf[ComposableFrom[T, T]]

		implicit def optional[T] :Option[T] ConstructFrom T = Opt.asInstanceOf[ConstructFrom[Option[T], T]]

		implicit def iterable[T] :Iterable[T] ConstructFrom T = Iter.asInstanceOf[ConstructFrom[Iterable[T], T]]



		object Superclass {
			def apply[T]() :T ComposableFrom T = Super.asInstanceOf[T ComposableFrom T]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Super

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Super

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Super)) => true
				case _ => false
			}
		}

		object Optional {
			def apply[T](): Option[T] ConstructFrom T = Opt.asInstanceOf[Option[T] ConstructFrom T]

			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Opt

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Opt

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Opt)) => true
				case _ => false
			}

		}

		object Iterable {
			def apply[T](): Iterable[T] ConstructFrom T = Iter.asInstanceOf[Iterable[T] ConstructFrom T]


			def unapply[T, E](composition :T ComposableFrom E) :Boolean =
				composition == Iter

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Iter

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(ComposableFrom(Iter)) => true
				case _ => false
			}
		}

		object Collection {

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

			def unapply[T, E](composedOf :T ComposedOf E) :Option[CanBuildFrom[_, E, T]] = composedOf.composition match {
				case col :Collection[T, E] => Some(col.cbf)
				case _ => None
			}

			def unapply[T](kin :Kin[T]) :Option[CanBuildFrom[_, _, T]] = kin match {
				case ComposedOf(comp) => unapply(comp)
				case _ => None
			}

		}



		case class Custom[C, E](private val compose :Iterable[E]=>C) extends ComposableFrom[C, E]  {
			def arity = Arity._0_n
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








	/** A way to obtain Iterable[E] from C */
	trait DecomposableTo[-C, +E]  { decomposition =>
		def arity :Arity

		def apply(composite :C) :Iterable[E]

		def unapply(composite :C) = Some(apply(composite))

		def composeWith[W<:C, T>:E](composition :ComposableFrom[W, T]) :W ComposedOf T =
			new ComposedOf.Hybrid(composition, this)

		def canEqual(that :Any) :Boolean = that.isInstanceOf[DecomposableTo[_, _]]

		def compatibleWith[X <: C](other: DecomposableTo[X, _]): Boolean = other eq this

	}



	trait ExtractAs[-C, +E] extends DecomposableTo[C, E] {
		def composeWith[W<:C, T>:E](composition :W ConstructFrom T) :W CollectionOf T =
			CollectionOf(composition, this)
	}

	object ExtractAs {
		def apply[C, E](implicit extract :C ExtractAs E) :C ExtractAs E = extract


		def unapply[C, E](composite :C ComposedOf E) :Option[C ExtractAs E] =
			composite.decomposition match {
				case deco :ExtractAs[C, E] => Some(deco)
				case _ => None
			}

		def unapply[T](kin :Kin[T]) :Option[T ExtractAs _] = kin match {
			case c :CompositeKin[T, _] if c.decomposition.isInstanceOf[ExtractAs[_, _]] =>
				Some(c.decomposition.asInstanceOf[T ExtractAs _])
			case _ => None
		}

	}




	object DecomposableTo {

		def apply[C, E](implicit ev :C DecomposableTo E) :C DecomposableTo E = ev

		def apply[C, E](decompose :C=>Iterable[E], multiplicity :Arity = Arity._0_n) :DecomposableTo[C, E] =
			new DecomposableTo[C, E] {
				override def arity :Arity = multiplicity
				override def apply(composite: C): Iterable[E] = decompose(composite)
				override def toString = multiplicity.toString
			}


		def unapply[T](kin :Kin[T]) :Option[DecomposableTo[_<:T, _]] = kin match {
			case c :CompositeKin[_, _] => Some(c.decomposition)
			case _ => None
		}

		implicit def iterable[T] :Iterable[T] ExtractAs T = Iter.asInstanceOf[ExtractAs[Iterable[T], T]]

		implicit def optional[T] :Option[T] ExtractAs T = Opt.asInstanceOf[ExtractAs[Option[T], T]]

		implicit def self[T] :T DecomposableTo T = Sub.asInstanceOf[DecomposableTo[T, T]]


		object Subclass {
			def apply[T]() :T DecomposableTo T = Sub.asInstanceOf[T DecomposableTo T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Sub

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition == Sub

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Sub)) => true
				case _ => false
			}
		}

		object Optional {
			def apply[T]() :Option[T] ExtractAs T = Opt.asInstanceOf[Option[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Opt

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition == Opt

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Opt)) => true
				case _ => false
			}
		}

		object Iterable {
			def apply[T]() :Iterable[T] ExtractAs T = Iter.asInstanceOf[Iterable[T] ExtractAs T]

			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Iter

			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition == Iter

			def unapply[T](kin :Kin[T]) :Boolean = kin match {
				case ComposedOf(DecomposableTo(Iter)) => true
				case _ => false
			}
		}



		case class Custom[C, E](decompose :C=>Iterable[E]) extends DecomposableTo[C, E] {
			def arity = Arity._0_n

			override def apply(composite: C): Iterable[E] = decompose(composite)

			override def toString :String = arity.toString
		}



		private case object Sub extends DecomposableTo[Any, Any] {
			def arity = Arity._1

			override def apply(composite: Any): Iterable[Any] = Seq(composite)
			override def toString = "One" //">:"
		}

		private case object Opt extends ExtractAs[Option[Any], Any] {
			def arity = Arity._0_1
			override def apply(composite :Option[Any]) :Iterable[Any] = composite.toSeq
			override def toString = "Option"
		}

		private case object Iter extends ExtractAs[Iterable[Any], Any] {
			def arity = Arity._0_n
			override def apply(composite: Iterable[Any]): Iterable[Any] = composite
			override def toString = "Iterable"
		}

	}


}

