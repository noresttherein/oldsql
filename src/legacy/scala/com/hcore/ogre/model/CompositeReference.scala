package com.hcore.ogre.model

import com.hcore.ogre.model.ComposedOf.{ComposableFrom, DecomposableTo}
import com.hcore.ogre.model.MappedReference.{ReferenceMapper, CompositeMapper}
import com.hcore.ogre.model.Reference.Lazy.LazyReference
import com.hcore.ogre.model.Reference.{Full, SingletonReferenceFactory, ReferenceFactory, GenericReferenceFactory}
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions

//implicits
import SaferCasts._
import extensions._


/** A composite reference is a reference for type T, which can be constructed from and deconstructed to
  * a collection of values of type E. It is used primarily to represent collection types of entities in a database,
  * but can be adapted to represent higher-level data-structures such as trees or graphs that can be serialized
  * into many rows (or at least in theory, even tables). It is expected that the specification on how to obtain
  * a value for a composite reference limits itself to type E, but any actual details are left for subclasses.
  *
  * This class no longer is covariant in regard to T, so you can't safely cast a Reference[T] to CompositeReference[T, _],
  * even if the underlying class is a CompositeReference, but should use CompositeReference[_<:T, _] instead!
  *
  * @tparam T composite value type of this reference
  * @tparam E element type - T can be mapped into an Iterable[E] and back, possibly with some restriction on the number
  *           of results or their values.
  */
trait CompositeReference[+T, E] extends Reference[T] { //extends ComposableReference[T, E] with DecomposableReference[T, E] {
	def composition :ComposableFrom[T, E]
	def decomposition :DecomposableTo[_<:T, E]
	def decompose :Option[Iterable[E]] //= toOpt.map(decomposition(_))

	def items :ComposedOf[_<:T, E]

	def mapItems[X, XS](map :ReferenceMapper[E, X])(implicit as :XS ComposedOf X) :Reference[XS]

	override def canEqual(that: Any): Boolean = that.isInstanceOf[CompositeReference[_, _]]
}


/** Operations on references for some composite type C which value can be represented as some sort of collection or composite
  * of items of another type T. Best examples are of course all subtypes of Iterable[T], but also Option[T] as well as
  * identity representation to a super type. The motivation is to be able to accept and return instances of Reference[C],
  * where composite value C can be mapped to a collection of E, which themselves can be mapped to individual rows in a last.
 * 
 */
object CompositeReference {

	def unapply[T](reference :Reference[T]) :Option[CompositeReference[T, _]] =
		reference.asSubclass[CompositeReference[T, _]]

	
	def apply[T](single :Reference[T]) :CompositeReference[T, T] = new SingletonReferenceWrapper(single)

	trait AbstractCompositeReference[T, E] extends CompositeReference[T, E] {
		def items :ComposedOf[T, E]
		def composition = items.composition
		def decomposition = items.decomposition
		def decompose = toOpt.map(items(_))

		def mapItems[X, XS](map :ReferenceMapper[E, X])(implicit as :XS ComposedOf X) :Reference[XS] =
			super.map(CompositeMapper[T, E, X, XS](map)(items, as))
	}

	class BaseCompositeReference[T, E](implicit val items :ComposedOf[T, E]) extends AbstractCompositeReference[T, E] {
		override def toOpt: Option[T] = None
	}
	
	case class SingletonReferenceWrapper[T](single :Reference[T]) extends BaseCompositeReference[T, T] {
		override def toOpt = single.toOpt
		override def toString = s"Composite($single)"
	}





	trait FlattenedReference[+T, RC, C, E] extends CompositeReference[T, E] {
		def key :Reference[RC]
		def headItems :RC DecomposableTo Reference[C]
		def tailItems :C DecomposableTo E

		override def toString = key.toString

		override lazy val toOpt =  key.toOpt.map(headItems(_)).flatMap { references =>
			val values = references.flatMap(_.toOpt).flatMap(tailItems(_))
			composition(values).providing(values.size == references.size)
		}
	}


	object FlattenedReference {
		def apply[T, RC, C, E](ref :Reference[RC], headComposition :RC DecomposableTo Reference[C], tailDecomposition :C DecomposableTo E)(implicit compose :T ComposedOf E)
			:FlattenedReference[T, RC, C, E] =
			new ReferenceSeq[T, RC, C, E](ref)(compose, headComposition, tailDecomposition)
		
		def apply[T, E](references :Seq[Reference[E]])(implicit compose :T ComposedOf E) :FlattenedReference[T, Seq[Reference[E]], E, E] =
			new ReferenceSeq[T, Seq[Reference[E]], E, E](Full(references))
		
		def apply[T, C, E](references :Seq[Reference[C]], elements :C DecomposableTo E)(implicit compose :T ComposedOf E) :FlattenedReference[T, Seq[Reference[C]], C, E] =
			new ReferenceSeq[T, Seq[Reference[C]], C, E](Full(references))(compose, DecomposableTo[Seq[Reference[C]], Reference[C]], elements)

		
		def unapply[T, E](reference :Reference[T])(implicit composition :T ComposedOf E) 
				:Option[(Reference[RC], RC DecomposableTo Reference[C], C DecomposableTo E) forSome { type RC; type C }] =
			reference match {
				case r:FlattenedReference[_, _, _, _] if r.decomposition.compatibleWith(composition.decomposition) =>
					val cast = r.asInstanceOf[FlattenedReference[T, Any, Any, E]]
					Some(cast.key, cast.headItems, cast.tailItems)
				case _ => None
			}

		private case class ReferenceSeq[T, RC, C, E](key :Reference[RC])(
				implicit ev :T ComposedOf E, val headItems :RC DecomposableTo Reference[C], val tailItems :C DecomposableTo E)
			extends BaseCompositeReference[T, E] with FlattenedReference[T, RC, C, E]
		{
			override def toString = key.toString
		}
	}


	object ReferenceCollection {
		def apply[X, E](references :Seq[Reference[E]], as :X ComposedOf E) :CompositeReference[X, E] =
			FlattenedReference(references, DecomposableTo.subclass[E])(as)

		def apply[E](references :Seq[Reference[E]]) :CompositeReference[Seq[E], E] =
			FlattenedReference(references, DecomposableTo.subclass[E])(ComposedOf[Seq[E], E])


	}



	class FlattenedReferenceFactory[HK, HX, TK, TX, E, X](
			head :ReferenceFactory[HK, Reference[TX], HX],
			tail :ReferenceFactory[TK, E, TX])(implicit val items :X ComposedOf E)
		extends GenericReferenceFactory[Reference[HX], E, X, Reference[X]]
	{ factory =>
		override def full(value: X): Reference[X] = Full(value)

		override def empty(key: Reference[HX]): Reference[X] =
			FlattenedReference[X, HX, TX, E](key, head.items.decomposition, tail.items.decomposition)

		override def keyFor(item: E): Option[Reference[HX]] =
			tail.keyFor(item).map(tail.empty).flatMap(head.keyFor).map(head.empty)

		override def keyOf[F >: Reference[X] <: Reference[X]](ref: F): Option[Reference[HX]] =
			ref match {
				case FlattenedReference(head(key, value), _, _) =>
					Some(head(key, value))
				case Full(items.decomposition(values)) =>
					tail.forItems(values).flatMap(tailValue => head.forItems(Seq(tailValue))) orElse {
						val tailValues = values.map(Seq(_)).map(tail.forItems)
						tailValues.collect { case Some(v) => v }.providing(_.size==tailValues.size).flatMap(head.forItems)
					}
//				case FlattenedReference(Full(refsComposite), headItems, tailItems) if tailItems==tail.items.decomposition =>
//					val refs = headItems(refsComposite)
//					???
				case _ => None
			}

		override def delayed(keyRef: Reference[HX], value: => X): Reference[X] = 
			new LazyReference[X](()=>Some(value)) with AbstractCompositeReference[X, E] with FlattenedReference[X, HX, TX, E] {
				override def items = factory.items
				override def headItems = head.items.decomposition
				override def tailItems = tail.items.decomposition
				override def key = keyRef

			}

		override def as[Y](implicit composition: ComposedOf[Y, E]): ReferenceFactory[Reference[HX], E, Y] =
			new FlattenedReferenceFactory(head, tail)(composition)

		override def equivalencyToken: Any = (head.equivalencyToken, tail.equivalencyToken)


	}



}
