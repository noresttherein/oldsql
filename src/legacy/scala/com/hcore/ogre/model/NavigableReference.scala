package com.hcore.ogre.model

import com.hcore.ogre.model.ComposedOf.DecomposableTo
import com.hcore.ogre.model.Reference.GenericReferenceFactory
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.SaferCasts


//implicits
import SaferCasts._



trait NavigableReference[+T] extends Reference[T] {
	def fetch[S>:T, E](composition :S ComposedOf E) :E

//	def toOpt = None
	
}



object NavigableReference {

	def unapply[K, T, C, R<:Reference[C]](factory :GenericReferenceFactory[K, T, C, R]) :Option[R] =
		factory.ifSubclass[GenericNavigableReferenceFactory[K, T, C, R]] { _.scout }

	def from[K, T, C, R<:Reference[C]](factory :GenericReferenceFactory[K, T, C, R]) :Option[R] = unapply(factory)

	def from[K, T, C, R<:Reference[C]](factory :GenericReferenceFactory[K, T, C, R], value : =>Option[C]) :Option[R] =
		factory.ifSubclass[GenericNavigableReferenceFactory[K, T, C, R]] { _.scout(value) }

	implicit class ReferencePathfinder[T](val ref :Reference[T]) extends AnyVal {

		def fetch[E](implicit composition :T ComposedOf E) :E =
			ref.ifSubclass[NavigableReference[T]].orElse(_.fetch(composition)) {
				composition.decomposition match {
					case DecomposableTo.Subclass() => ref.get.asInstanceOf[E]
					case DecomposableTo.Iterable() => ref.get.asInstanceOf[Iterable[E]].head
					case DecomposableTo.Optional() => ref.get.asInstanceOf[Option[E]].get
					case _ =>
						throw new UnsupportedOperationException(s"fetch called for a non-navigable reference $ref. Navigation through references is only supported for PathReference instances. Either your mapping of this reference component doesn't support navigation or you called fetch for a non-navigable object. Implement mockValue for all mappings on path so that NavigableReference instances are returned.")
				}
//				ref.get.asInstanceOf[E].providing(composition.decomposition==Decomposition.Subclass) getOrElse {
//					throw new UnsupportedOperationException(s"fetch called for a non-navigable reference $ref. Navigation through references is only supported for PathReference instances. Either your mapping of this reference component doesn't support navigation or you called fetch for a non-navigable object. Implement mockValue for all mappings on path so that NavigableReference instances are returned.")
//				}
//				ref.toOpt.flatMap(_.asInstanceOf[E].providing(composition.decomposition==Decomposition.subclass)) getOrElse {
//					throw new UnsupportedOperationException(s"fetch called for a non-navigable reference $ref. Navigation through references is only supported for PathReference instances. Either your mapping of this reference component doesn't support navigation or you called fetch for a non-navigable object. Implement mockValue for all mappings on path so that NavigableReference instances are returned.")
//				}
			}

	}

	implicit class CompositeReferencePathfinder[T, E](val ref :CompositeReference[T, E]) extends AnyVal {
		def fetch :E =
			ref.ifSubclassOf[NavigableReference[T]].orElse(_.fetch(ref.items.asInstanceOf[T ComposedOf E])) {
				throw new UnsupportedOperationException(s"fetch called for a non-navigable reference $ref. Navigation through references is only supported for PathReference instances. Either your mapping of this reference component doesn't support navigation or you called fetch for a non-navigable object. Implement mockValue for all mappings on path so that NavigableReference instances are returned.")
			}
	}


	implicit def fetchOne[T](reference :Reference[T]) :T = reference.fetch[T]
	implicit def fetchOption[T](reference :Reference[Option[T]]) :T = reference.fetch[T]
	implicit def fetchMany[T](reference :Reference[Iterable[T]]) :T = reference.fetch[T]


	trait CompositeNavigableReference[+T, E] extends CompositeReference[T, E] with NavigableReference[T] {

		override def fetch[S >: T, X](composition: ComposedOf[S, X]): X =
			if (composition.compatibleWith(this.items)) fetch.asInstanceOf[X]
			else throw new IllegalArgumentException(s"Can't fetch $composition from $this (decomposed with ${this.items}")

		def fetch :E
	}

	trait SingletonNavigableReference[+T] extends NavigableReference[T] {
		override def fetch[S >: T, X](composition: ComposedOf[S, X]): X =
			if (composition==ComposedOf.itself) fetch.asInstanceOf[X]
			else throw new IllegalArgumentException(s"Can't fetch decomposed $composition value from a singleton reference $this")

		def fetch :T


	}

	val SingletonFetchProperty = PropertyChain[SingletonNavigableReference[_]](_.fetch)
	val CompositeFetchProperty = PropertyChain[CompositeNavigableReference[_, _]](_.fetch)
	val IterableReferenceGetProperty = PropertyChain[Reference[Iterable[_]]](_.get.head)
	val OptionReferenceGetProperty = PropertyChain[Reference[Option[_]]](_.get.get)
	val ReferenceGetProperty = PropertyChain[Reference[_]](_.get)


	trait GenericNavigableReferenceFactory[K, E, X, R<:Reference[X]] extends GenericReferenceFactory[K, E, X, R] {
		def scout :R = scout(None)
//		def scout(value : =>C) :R = scout(Some(value))
		def scout(value : =>Option[X]) :R
	}

	type NavigableReferenceFactory[K, E, X] = GenericNavigableReferenceFactory[K, E, X, Reference[X]]


}



