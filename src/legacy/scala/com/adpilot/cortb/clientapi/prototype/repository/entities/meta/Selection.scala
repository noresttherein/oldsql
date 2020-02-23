package com.adpilot.cortb.clientapi.prototype.repository.entities.meta

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.Many.All
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.{PropertyMultiReference, Full, PropertyReference, One}
import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Selection.SelectMany
import com.adpilot.cortb.clientapi.util.ObjectProperty.AnyProperty

import scala.collection.generic.CanBuildFrom

sealed trait Selection[T, E] {
	val reference :Reference[T]
	def items :Option[Iterable[E]]
	def items(items :T) :Iterable[E]

	def apply(items :Iterable[E]) :T

//	def all :Selection[Seq[E], E] = SelectMany()
}



object Selection {

	class SelectOne[E](val reference :One[E]) extends Selection[E, E] {
		def items = reference.toOpt.map(x => Seq(x))

		def items(item: E): Iterable[E] = Seq(item)

		def apply(items: Iterable[E]): E =
			if (items.size == 1) items.head
			else throw new IllegalArgumentException(s"expected exactly one result for $reference. Got ${items.size}: $items")

		override def toString = s"One($reference)"
	}


	object SelectOne {
		def apply[E](reference :One[E]) = new SelectOne(reference)

		def unapply[T, E](selection :Selection[T, E]) :Option[Reference[E]] = selection match {
			case s:SelectOne[_] => Some(s.reference.asInstanceOf[Reference[E]])
			case _ => None
		}
	}




	class SelectOpt[E](val reference :Reference[Option[E]]) extends Selection[Option[E], E] {
		def items = reference.toOpt.map(_.toSeq)

		def items(item :Option[E]) = item.toSeq

		def apply(items: Iterable[E]): Option[E] =
			if (items.size <= 1) items.headOption
			else throw new IllegalArgumentException(s"expected at most one result for $reference. Got ${items.size}: $items")

		override def toString = s"Maybe($reference)"
	}



	object SelectOpt {
		def apply[E](reference :Reference[Option[E]]) = new SelectOpt(reference)

		def unapply[T, E](selection :Selection[T, E]) :Option[Reference[Option[E]]] = selection match {
			case m :SelectOpt[E] => Some(m.reference)
			case _ => None
		}
	}




	class SelectMany[T<:Iterable[E], E](val reference :Reference[T])(implicit cbf :CanBuildFrom[_, E, T]) extends Selection[T, E] {
		def items = reference.toOpt

		def items(items :T) :Iterable[E] = items

		def apply(items: Iterable[E]): T = (cbf() ++= items).result()

		def asIterable :Reference[Iterable[E]] = reference

		override def toString = s"Many($reference)"
	}

	object SelectMany {
		def apply[T<:Iterable[E], E](reference :Reference[T])(implicit cbf :CanBuildFrom[_, E, T]) = new SelectMany[T, E](reference)

		def unapply[T, E](selection :Selection[T, E]) :Option[Reference[Iterable[E]]] = selection match {
			case s:SelectMany[T, E] => Some(s.asIterable)
			case _ => None
		}

	}




	implicit def selectOne[E](reference :One[E]) :SelectOne[E] = new SelectOne(reference)

	implicit def maybeOne[E](reference :Reference[Option[E]]) :SelectOpt[E] = new SelectOpt(reference)

	implicit def selectMany[T<:Iterable[E], E](reference :Reference[T])(implicit cbf :CanBuildFrom[_, E, T]) :SelectMany[T, E] = new SelectMany(reference)


	object ByProperty {
		def unapply[T, E](selection :Selection[T, E]) =
			selection.reference match {
				case PropertyReference(property, key) => Some(property.asInstanceOf[AnyProperty[E]], key)
				case _ => None
			}
	}

	object ByPropertyValues {
		def unapply[T, E](selection :Selection[T, E]) =
			selection.reference match {
				case PropertyReference(property, key) => Some(property.asInstanceOf[AnyProperty[E]], Seq(key))
				case PropertyMultiReference(property, keys) => Some(property.asInstanceOf[AnyProperty[E]], keys)
				case _ => None
			}
	}

	object ByInstances {
		def unapply[T, E](selection :Selection[T, E]) = selection.items.map(_.toSeq)
	}

	object SelectAll {
		class SelectAllBuilder[E] {
			def in[T[E]<:Iterable[E]](implicit cbf :CanBuildFrom[_, E, T[E]]) = SelectMany[T[E], E](All())
			def as[T<:Iterable[E]](implicit cbf :CanBuildFrom[_, E, T]) = SelectMany[T, E](All())
		}

		def apply[E] = new SelectAllBuilder[E]

		def asSeq[E] = SelectAll[E].in[Seq]

		def unapply[T, E](selection :Selection[T, E]) = selection.reference match {
			case All() => true
			case _ => false
		}
	}
	
	object SelectSeq {
		def apply[E](reference :Reference[Seq[E]]) = SelectMany[Seq[E], E](reference)
	}
	
	object SelectSet {
		def apply[E](reference :Reference[Set[E]]) = SelectMany[Set[E], E](reference)
	}

	def unapply[T, E](selection :Selection[T, E]) = Some(selection.reference, selection.items)

//
//	val intRef = One(1)
//	val intOptRef = One(Option(1))
//
//	def takesIntSelection[X](selection :SelectionOf[X, Int]) :X = selection.reference.toOpt.get
//	def takesSelection[X, E](selection :SelectionOf[X, E]) :X = selection.reference.toOpt.get
//
//	takesIntSelection(intRef)
//
//	def generic[X](ref :Reference[X]) :X = takesSelection[X, X](ref)
//	def genericOpt[X](ref :Reference[Option[X]]) :Option[X] = takesSelection(ref)
//
//	generic(intRef)
//	generic(intOptRef)
//	genericOpt(intOptRef)
}
