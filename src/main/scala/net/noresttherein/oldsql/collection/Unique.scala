package net.noresttherein.oldsql.collection


import net.noresttherein.oldsql.collection.Unique.{UniqueSeqAdapter, UniqueSetAdapter}

import scala.collection.immutable.{IndexedSeq, Iterable, Seq, Set}
import scala.collection.mutable.Builder
import scala.collection.{AbstractSeq, AbstractSet, IterableFactory, IterableFactoryDefaults, IterableOps}


/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `apply(Int)`,
  * `toSet`, `toSeq`, `toIndexedSeq` and `size` implementations. This class is used for the column lists exported
  * by mappings in order to quickly find the column mapping for a given column result in a `ResultSet`.
  * @tparam T element type.
  */
trait Unique[+T] extends Iterable[T] with IterableOps[T, Unique, Unique[T]] with IterableFactoryDefaults[T, Unique] { unique =>

	override def iterableFactory :IterableFactory[Unique] = Unique

	/** The `n`-th element in this collection.
	  * @param n the index in the `[0..size-1]` range.
	  */
	def apply(n :Int) :T

	/** The index of the given element in this collection, or `-1` if it does not contain `elem`. */
	def indexOf[U >: T](elem :U) :Int

	/** Checks if this collection contains the given element as defined by `equals`. */
	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0


	override def toIndexedSeq :IndexedSeq[T] = new UniqueSeqAdapter(this)

	override def toSeq :Seq[T] = new UniqueSeqAdapter(this)

	override def toSet[U >: T] :Set[U] = new UniqueSetAdapter(this)

	/** Prepends an element to the front of the collection if it isn't already present.
	  * @return `this` ''iff'' it already contains `elem`, or a `Unique` instance containing the given element
	  *        followed by all elements from `this` if it does not.
	  */
	def +:[U >: T](elem :U) :Unique[U]

	/** Appends an element at the end of the collection if it isn't already present.
	  * @return `this` ''iff'' it already contains `elem`, or a `Unique` instance containing all elements of `this`,
	  *        followed by `elem` if it does not.
	  */
	def :+[U >: T](elem :U) :Unique[U]

	override def stringPrefix = "Unique"
}






/** Companion object serving as a factory for Unique - sequence-like collections with fast indexOf operations. */
object Unique extends IterableFactory[Unique] {


	override def from[T](elems :IterableOnce[T]) :Unique[T] = elems match {
		case _ :Unique[_] => elems.asInstanceOf[Unique[T]]
		case seq :UniqueSeqAdapter[_] => seq.toUniqueSeq.asInstanceOf[Unique[T]]
		case set :UniqueSetAdapter[_] => set.toUniqueSeq.asInstanceOf[Unique[T]]
		case _ => (newBuilder[T] ++= elems).result
	}

	override def newBuilder[T] :Builder[T, Unique[T]] = new UniqueBuilder[T]()

	override def empty[E] :Unique[E] = reusableEmpty

	private[this] val reusableEmpty = new IndexedUnique[Nothing](IndexedSeq.empty, Map.empty)


	/** A proxy to a lazily computed `Unique[T]`. The initializer will be called when any of the methods on the proxy
	  * is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	def Lazy[T](init: => Unique[T]) :Unique[T] = new LazyUnique[T](() => init)




	implicit def uniqueToSeq[T](unique :Unique[T]) :Seq[T] = unique.toSeq

	implicit def uniqueToSet[T](unique :Unique[T]) :Set[T] = unique.toSet

	/** An implicit extension of any `Iterable` adding a `toUniqueSeq` method which converts it to a `Unique` instance.*/
	implicit class implicitUnique[T](private val elems :Iterable[T]) extends AnyVal {
		/** A shorthand method for converting `this` collection to a `Unique`.
		  * It delegates to `Unique.from`, meaning if `this` already is a `Unique` or collection created by
		  * one of the `toSeq`, `toIndexedSeq`, `toSet` methods of a `Unique`, it will simply return the underlying
		  * `Unique` instance.
		  */
		def toUniqueSeq :Unique[T] = from(elems)
	}






	private class UniqueBuilder[T](
			private[this] var items :Builder[T, IndexedSeq[T]] = IndexedSeq.newBuilder[T],
			private[this] var index :Map[T, Int] = Map[T, Int]())
		extends Builder[T, Unique[T]]
	{

		override def addOne(elem :T) :this.type = {
			if (!index.contains(elem)) {
				index = index.updated(elem, index.size)
				items += elem
			}
			this
		}

		override def clear() :Unit = {
			index = Map[T, Int]()
			items = IndexedSeq.newBuilder
		}

		override def result() :Unique[T] = new IndexedUnique(items.result, index.withDefaultValue(-1))
	}



	private class LazyUnique[T](private[this] var initialize: () => Unique[T]) extends Unique[T] {
		@volatile private[this] var initialized :Unique[T] = _
		private[this] var fastAccess :Unique[T] = _

		protected def items :Unique[T] = {
			if (fastAccess == null) {
				var init = initialized
				if (init != null) fastAccess = init
				else synchronized {
					init = initialized
					if (init != null) fastAccess = init
					else {
						fastAccess = initialize()
						initialized = fastAccess
						initialize = null
					}
				}
			}
			fastAccess
		}

		override def apply(idx :Int) :T = items(idx)

		override def indexOf[U >: T](elem :U) :Int = items.indexOf(elem)

		override def +:[U >: T](elem :U) :Unique[U] = elem +: items

		override def :+[U >: T](elem :U) :Unique[U] = items :+ elem


		override def concat[B >: T](suffix :IterableOnce[B]) :Unique[B] = items ++ suffix

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def iterator :Iterator[T] = items.iterator
	}






	private class IndexedUnique[+T](items :IndexedSeq[T], index :Map[T, Int]) extends Unique[T] {
		override def iterator :Iterator[T] = items.iterator

		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0


		override def apply(idx :Int) :T = items(idx)

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])

		override def +:[U >: T](elem :U) :Unique[U] =
			if (contains(elem)) this
			else new IndexedUnique(items :+ elem, index.asInstanceOf[Map[U, Int]].updated(elem, size))

		override def :+[U >: T](elem :U) :Unique[U] =
			if (contains(elem)) this
			else
                new IndexedUnique(
					items :+ elem,
                    index.asInstanceOf[Map[U, Int]].map(pair => pair._1 -> (pair._2 + 1)).updated(elem, 0)
                )

		override def concat[U >: T](that :IterableOnce[U]) :Unique[U] =
			if (that.iterator.isEmpty)
				this
			else
                (new UniqueBuilder(
					IndexedSeq.newBuilder[U] ++= items,
					index.asInstanceOf[Map[U, Int]]) ++= that
				).result()

	}



	private class UniqueSeqAdapter[+T](unique :Unique[T]) extends AbstractSeq[T] with IndexedSeq[T] {
		override def length :Int = unique.size

		override def apply(idx :Int) :T = unique(idx)

		override def indexOf[U >: T](elem :U, start :Int) :Int = {
			val i = unique.indexOf(elem)
			if (i < start) -1 else i
		}

		override def lastIndexOf[U >: T](elem :U, end :Int) :Int = {
			val i = unique.indexOf(elem)
			if (i > end) -1 else i
		}

		override def contains[U >: T](elem :U) :Boolean = unique.contains(elem)

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toSet[U >: T] :Set[U] = unique.toSet

		def toUniqueSeq :Unique[T] = unique
	}



	private class UniqueSetAdapter[T](unique :Unique[T]) extends AbstractSet[T] with Set[T] {
		override def size :Int = unique.size

		override def contains(elem :T) :Boolean = unique.contains(elem)

		override def incl(elem :T) :Set[T] = if (contains(elem)) this else Set(toSeq:_*)

		override def excl(elem :T) :Set[T] = if (contains(elem)) Set(toSeq:_*) - elem else this

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toIndexedSeq :IndexedSeq[T] = unique.toIndexedSeq

		override def toSeq :Seq[T] = unique.toSeq

		def toUniqueSeq :Unique[T] = unique
	}


}

