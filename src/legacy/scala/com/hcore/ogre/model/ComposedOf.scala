package com.hcore.ogre.model

import com.hcore.ogre.model.ComposedOf._
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.slang.{SaferCasts, matching}
import matching.&&
import com.hcore.ogre.morsels.necromancy.PropertyChain

import scala.collection.generic.CanBuildFrom


//implicits
import extensions._
import SaferCasts._


/** Proof that type C is in some way composed of values of T and can be converted to and from Iterable[T] (possibly with some restrictions in place).
  * Serves as a single umbrella for collection types (<:< Iterable[T]), Option[T] and T itself, allowing to create queries for T with result type C.
  * Will be generally written in the code using infix notation for clarity: C ComposedOf T.
  */
trait ComposedOf[C, T] {
	def arity :Arity =
		if (composition.arity==decomposition.arity) composition.arity
		else throw new UnsupportedOperationException(s"ComposedOf.arity: composition arity (${composition.arity}) is different than decomposition arity (${decomposition.arity}) in $this. Most probabaly a bug.")

	val composition :ComposableFrom[C, T]
	val decomposition :DecomposableTo[C, T]

	def apply(items: Iterable[T]): C = composition(items)

	def apply(composite: C): Iterable[T] = decomposition(composite)


	def unapply(composite :C) = decomposition.unapply(composite)

	def unapply(reference :Reference[C]) :Option[C ComposedOf T] = reference match {
		case ComposedOf(composed) if this.compatibleWith(composed) =>
			Some(composed.asInstanceOf[ComposedOf[C, T]])
		case ComposableFrom(co) && DecomposableTo(deco) if compatibleWith(decomposition) =>
			Some(ComposedOf(composition, deco.asInstanceOf[DecomposableTo[C, T]]))
		case c:CompositeReference[_, _] => None
		case _ => Some(this)
	}



	def rebuild(value: C): C = composition(decomposition(value))


	def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = composition.compatibleWith(other)

	def compatibleWith[X <: C](other: DecomposableTo[X, _]) :Boolean = decomposition.compatibleWith(other)

	def compatibleWith[X <:C](other : X ComposedOf _) :Boolean =
		other.composition.compatibleWith(this.composition) && compatibleWith(other.decomposition)


	def canEqual(that :Any) = that.isInstanceOf[ComposedOf[_, _]]

	override def equals(obj: scala.Any): Boolean = obj match {
		case that :ComposedOf[_, _] =>
			(that eq this) || that.canEqual(this) && composition==that.composition && decomposition==that.decomposition
		case _ => false
	}

	override def hashCode = (composition, decomposition).hashCode



	object Full {

		def unapply(reference :Reference[C]) :Option[Seq[T]] =
			reference match {
				case DecomposableTo(deco) && Reference.Full(value) =>
					if (decomposition==deco)
						Some(decomposition(value).toSeq)
					else
						throw new IllegalArgumentException(s"Cannot decompose reference $reference: associated decomposition $deco doesn't seem to be compatible with requested decomposition ${decomposition}")
				case Reference.Full(value) =>
					Some(apply(value).toSeq)
				case _ => None
			}
	}



}





object ComposedOf {

	def apply[C, E](implicit ev :C ComposedOf E) :C ComposedOf E = ev

	def apply[C, E](compose :Iterable[E] => C, decompose :C=>Iterable[E]) :C ComposedOf E =
		Hybrid(ComposableFrom(compose), DecomposableTo(decompose))


	def apply[C, E](compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		Hybrid(compose, decompose)

	implicit def iterable[E] :Iterable[E] ComposedOf E = Hybrid(ComposableFrom.iterable[E], DecomposableTo.iterable[E])

	implicit def hybrid[C, E](implicit compose :ComposableFrom[C, E], decompose :DecomposableTo[C, E]) :C ComposedOf E =
		new Hybrid[C, E](compose, decompose)


	implicit def itself[T] :ComposedOf[T, T] = identity.asInstanceOf[ComposedOf[T, T]]

	def unapply[T](reference :Reference[T]) :Option[ComposedOf[_<:T, _]] = reference match {
		case c:CompositeReference[_, _] =>
			val cast = c.asInstanceOf[CompositeReference[T, Any]]
			Some(cast.items)
		case _ => None
	}

	/** Restriction on the number of values on one side of a relation. When used in ComposedOf, it defines how many elements
	  * Iterable[E] obtained from C can have.
	  */
	trait Arity

	object Arity {
		trait AtMostOne extends Arity
		trait AtLeastOne extends Arity

		case object _1 extends AtMostOne with AtLeastOne
		case object _0_1 extends AtMostOne
		case object _0_n extends Arity
		case object _1_n extends AtLeastOne

		object AtMostOne {
			def apply(arity :Arity) = arity.isInstanceOf[AtMostOne]
			def unapply(arity :Arity) = arity.asSubclass[AtMostOne]
		}
		object AtLeastOne {
			def apply(arity :Arity) = arity.isInstanceOf[AtLeastOne]
			def unapply(arity :Arity) = arity.asSubclass[AtLeastOne]
		}
	}



	private val identity = ComposedOf[Any, Any](ComposableFrom.superclass[Any], DecomposableTo.subclass[Any])

	case class Hybrid[C, E](composition :ComposableFrom[C, E], decomposition :DecomposableTo[C, E]) extends ComposedOf[C, E] {
		override def toString = {
			val (c, d) = (composition.toString, decomposition.toString)
			if (c==d) c else s"($c, $d)"
		}
	}


	/** A way to create value of C from Iterable[E] */
	trait ComposableFrom[+C, -E]  { composition =>
		def arity :Arity

		def empty :C = apply(Seq())

		def apply(items :Iterable[E]) :C = attempt(items) getOrElse {
			throw new IllegalArgumentException(s"Cann't perform composition $this for $items.")
		}

//		def unapply(composition :ComposableFrom[_, _]) = compatibleWith(composition)

		def attempt(items :Iterable[E]) :Option[C]

		def decomposeWith[W>:C, T<:E](decomposition :DecomposableTo[W, T]) :ComposedOf[W, T] =
			ComposedOf.Hybrid[W, T](this, decomposition)

		def canEqual(that :Any) = that.isInstanceOf[ComposableFrom[_, _]]

		def compatibleWith[X>:C](other :ComposableFrom[X, _]) :Boolean = this eq other

		//		def unapply[X>:C](reference :Reference[X]) :Boolean = Composition.unapply(reference).exists(compatibleWith _)

	}

	/** A way to obtain Iterable[E] from C */
	trait DecomposableTo[-C, +E]  { decomposition =>
		def arity :Arity

		def apply(composite :C) :Iterable[E]
		def unapply(composite :C) = Some(apply(composite))

		def composeWith[W<:C, T>:E](composition :ComposableFrom[W, T]) :W ComposedOf T = ComposedOf.Hybrid(composition, this)

		def canEqual(that :Any) = that.isInstanceOf[DecomposableTo[_, _]]

		def compatibleWith[X <: C](other: DecomposableTo[X, _]): Boolean = other eq this

	}






	object ComposableFrom {
		def apply[C, E](implicit ev :C ComposableFrom E) : C ComposableFrom E = ev

		def apply[C, E](compose :Iterable[E]=>C) :ComposableFrom[C, E] = new Custom(compose)

		def unapply[T](reference :Reference[T]) :Option[ComposableFrom[T, _]] = reference match {
			case c:CompositeReference[_, _] => Some(c.composition.asInstanceOf[ComposableFrom[T, _]])
			case _ => None
		}


		implicit def superclass[T] :ComposableFrom[T, T] = Super.asInstanceOf[ComposableFrom[T, T]]

		implicit def optional[T] :ComposableFrom[Option[T], T] = Opt.asInstanceOf[ComposableFrom[Option[T], T]]

		def iterable[T] :ComposableFrom[Iterable[T], T] = Iter.asInstanceOf[ComposableFrom[Iterable[T], T]]

		implicit def canBuildFrom[C, T](implicit cbf :CanBuildFrom[_, T, C]) :ComposableFrom[C, T] =
			new Custom(items => (cbf() ++= items).result())

		implicit def collection[C<:Iterable[T], T](implicit cbf :CanBuildFrom[_, T, C]) :ComposableFrom[C, T] =
			new Col[C, T](items => (cbf() ++= items).result())


		object Superclass {
			def unapply[T, E](composition :ComposableFrom[T, E]) :Boolean =
				composition == Super
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Super
		}

		object Iterable {
			def unapply[T, E](composition :ComposableFrom[T, E]) :Boolean =
				composition == Iter
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Iter
		}

		object Optional {
			def unapply[T, E](composition :ComposableFrom[T, E]) :Boolean =
				composition == Opt
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition == Opt
		}

		object Collection {
			def unapply[T, E](composition :ComposableFrom[T, E]) :Boolean =
				composition.isInstanceOf[Col[_,_]]
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.composition.isInstanceOf[Col[_,_]]
		}



		private case object Super extends ComposableFrom[Any, Any] {
			def arity = Arity._1
			override def attempt(items: Iterable[Any]) = items.head.providing(items.size==1)

			override def toString = ">:"
		}


		private case object Opt extends ComposableFrom[Option[Any], Any] {
			def arity = Arity._0_1

			override def attempt(items: Iterable[Any]) =
				items.headOption.providing(items.size<=1) orElse None

			override def toString = "Option"
		}

		private class Col[C, T](compose :Iterable[T]=>C) extends Custom[C, T](compose) {
			//			def arity = Arity._0_n

			override def canEqual(that :Any) = that.isInstanceOf[Col[_, _]]

			override def equals(that :Any) = that match {
				case c :Col[_, _] => super.equals(c)
				case _ => false
			}

			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean = canEqual(other)

			override def toString = "Collection"
		}



		private val Iter = new Col[Iterable[Any], Any](Predef.identity[Iterable[Any]]) {
			override def toString = "Iterable"
		}


		case class Custom[C, E](private val compose :Iterable[E]=>C) extends ComposableFrom[C, E]  {
			def arity = Arity._0_n
			override def apply(items: Iterable[E]): C = compose(items)
			override def attempt(items: Iterable[E]): Option[C] = Some(compose(items))

			override def compatibleWith[X >: C](other: ComposableFrom[X, _]): Boolean =
				other.canEqual(this) && this==other

			override def toString = "Custom"
		}

//		case class PropertyOf[P, E](private val property :PropertyPath[E, P]) extends ComposableFrom[P, E] {
//			def arity = Arity._1
//
//			def attempt(items :Iterable[E]) = property(items.head).providing(items.size==1)
//
//			override def compatibleWith[X >: P](other :ComposableFrom[X, _]) :Boolean =
//				other.canEqual(this) && this == other
//
//			override def toString = s"_.$property"
//		}

	}





	object DecomposableTo {
		def apply[C, E](implicit ev :C DecomposableTo E) :C DecomposableTo E = ev

		def apply[C, E](decompose :C=>Iterable[E], multiplicity :Arity =Arity._0_n) :DecomposableTo[C, E] =
			new DecomposableTo[C, E] {
				def arity = multiplicity
				override def apply(composite: C): Iterable[E] = decompose(composite)
			}

		implicit def iterable[T] :DecomposableTo[Iterable[T], T] = Iter.asInstanceOf[DecomposableTo[Iterable[T], T]]

		implicit def optional[T] :DecomposableTo[Option[T], T] = Opt.asInstanceOf[DecomposableTo[Option[T], T]]

		implicit def subclass[T] :DecomposableTo[T, T] = Sub.asInstanceOf[DecomposableTo[T, T]]

		
		object Iterable {
			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition == Iter
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition == Iter
		} 
		
		object Optional {
			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition==Opt
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition==Iter
		}
		
		object Subclass {
			def unapply[T, E](decomposition :DecomposableTo[T, E]) :Boolean =
				decomposition==Sub
			def unapply[T, E](composedOf :T ComposedOf E) :Boolean =
				composedOf.decomposition==Sub
		}
		

		def unapply[T](reference :Reference[T]) :Option[DecomposableTo[_<:T, _]] = reference match {
			case c:CompositeReference[_, _] => Some(c.decomposition)
			case _ => None
		}



		case class Custom[C, E](decompose :C=>Iterable[E]) extends DecomposableTo[C, E] {
			def arity = Arity._0_n

			override def apply(composite: C): Iterable[E] = decompose(composite)

			override def toString = "Custom"
		}


		private case object Iter extends DecomposableTo[Iterable[Any], Any] {
			def arity = Arity._0_n
			override def apply(composite: Iterable[Any]): Iterable[Any] = composite
			override def toString = "Iterable"
		}


		private case object Sub extends DecomposableTo[Any, Any] {
			def arity = Arity._1

			override def apply(composite: Any): Iterable[Any] = Seq(composite)
			override def toString = ">:"
		}

		private case object Opt extends DecomposableTo[Option[Any], Any] {
			def arity = Arity._0_1
			override def apply(composite :Option[Any]) :Iterable[Any] = composite.toSeq
			override def toString = "Option"
		}




	}


}
