package net.noresttherein.oldsql.collection

import scala.collection.immutable.{ArraySeq, IndexedSeq, Iterable, Seq, Set}
import scala.collection.mutable.Builder
import scala.collection.{AbstractSeq, AbstractSet, Factory, IterableFactory, IterableFactoryDefaults, IterableOps}
import scala.reflect.ClassTag

import net.noresttherein.oldsql.collection.companionFactoryOf
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.collection.Unique.{UniqueSeqAdapter, UniqueSetAdapter}






/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `apply(Int)`,
  * `toSet`, `toSeq`, `toIndexedSeq` and `size` implementations. It can be thought as a mix of [[Set]] and [[Seq]]:
  * no two elements in the collection are equal, but their position is in the collection is important and features
  * in equality (essentially having `Seq` semantics), unlike `Set` equality even for implementations preserving
  * insertion order. Note that this means that seq-like append/prepend operations will lead to no change
  * in the collection if the added items already exists.
  *
  * This class is used for the column lists exported by mappings in order to quickly find the column mapping
  * for a given column result in a `ResultSet`.
  * @tparam T element type.
  */ //consider: renaming to SeqSet or smth, Unique may be a good name for a collection of entities using their id as equality
trait Unique[+T]
	extends Iterable[T] with IterableOps[T, Unique, Unique[T]] with IterableFactoryDefaults[T, Unique]
	   with Serializable
{ unique =>

	override def knownSize :Int = size
	override def iterableFactory :IterableFactory[Unique] = Unique

	override def toIndexedSeq :IndexedSeq[T] = new UniqueSeqAdapter(this)
	override def toSeq :Seq[T] = toIndexedSeq
	override def toSet[U >: T] :Set[U] = new UniqueSetAdapter(this)

	override def to[C1](factory :Factory[T, C1]) :C1 = companionFactoryOf(factory) match {
		case Got(Seq) | Got(IndexedSeq) => toSeq.asInstanceOf[C1]
		case Got(Set) => toSet.asInstanceOf[C1]
		case _ => super.to(factory)
	}


	def reverseIterator :Iterator[T]

	/** The `n`-th element in this collection.
	  * @param n the index in the `[0..size-1]` range.
	  */
	def apply(n :Int) :T

	/** The index of the given element in this collection, or `-1` if it does not contain `elem`. */
	def indexOf[U >: T](elem :U) :Int

	/** Checks if this collection contains the given element as defined by `equals`. */
	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0

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


	def :++[U >: T](elems :IterableOnce[U]) :Unique[U]
//	def ++:[U >: T](elems :IterableOnce[U]) :Unique[U] = prependedAll(elems)

	def -[U >: T](elem :U) :Unique[T]


	/** Verifies if the element sets of the two collections are equal.
	  * @return value equal to `this.toSet == other.toSet`.
	  */
	def contentsEqual[U](other :Unique[U]) :Boolean =
		size == other.size && other.forall(contains)


	override def className = "Unique"

}






/** Companion object serving as a factory for Unique - sequence-like collections with fast indexOf operations. */
object Unique extends IterableFactory[Unique] {

	override def from[T](elems :IterableOnce[T]) :Unique[T] = elems match {
		case unique :Unique[T] => unique
		case seq :UniqueSeqAdapter[T] => seq.toUnique
		case set :UniqueSetAdapter[T] => set.toUnique
		case iter :Iterable[_] if iter.isEmpty => empty[T]
		case iter :Iterator[_] if iter.isEmpty => empty[T]
		case iter :Iterable[T] if iter.sizeIs <= 1 =>
			new SingletonUnique[T](iter.head)
		case iter :Iterable[T] if iter.sizeIs <= SmallUniqueLimit =>
			new SmallUnique[T](iter.toArray(ClassTag[T](classOf[AnyRef])))
		case iter :Iterable[T] =>
			val seq = iter.toIndexedSeq
			val map = seq.view.zipWithIndex.toMap
			new IndexedUnique(seq, map)
		case _ => (newBuilder[T] ++= elems).result()
	}

	override def newBuilder[T] :Builder[T, Unique[T]] = new UniqueBuilder[T]()

	override def empty[E] :Unique[E] = EmptyUnique //reusableEmpty

//	private[this] val reusableEmpty = new IndexedUnique[Nothing](IndexedSeq.empty, Map.empty)



	/** A specialized light `Unique` implementation for collections containing only one element. */
	def single[T](singleton :T) :Unique[T] = new SingletonUnique(singleton)



	/** A `Unique[T]` with lazily evaluated contents. The initializer will be called only when any of the methods
	  * on the proxy is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	@inline def Lazy[T](init: => IterableOnce[T]) :Unique[T] = delayed(from(init))

	/** A proxy to a lazily computed `Unique[T]`. The initializer will be called when any of the methods on the proxy
	  * is called. It will be executed at most once, withing a `synchronized` block for the proxy.
	  * Once computed, it remains thread safe but will incur no additional synchronization penalty.
	  */
	def delayed[T](init: => Unique[T]) :Unique[T] = new LazyUnique[T](() => init)




	implicit def uniqueToSeq[T](unique :Unique[T]) :Seq[T] = unique.toSeq

	implicit def uniqueToSet[T](unique :Unique[T]) :Set[T] = unique.toSet

	/** An implicit extension of a ''by-name'' expression evaluating to a `Unique[T]` instance, adding a `delayed`
	  * method which creates a proxy using it to initialize its target. */
	implicit class DelayedUnique[T](initializer: => Unique[T]) {
		/** Treats the `this` argument as a ''by-name'' expression to be evaluated only when the created `Unique`
		  * proxy's contents are accessed.
		  * @return `Unique.delayed(initializer)`.
		  * @see [[net.noresttherein.oldsql.collection.Unique.delayed delayed]]
		  */
		@inline def delayed :Unique[T] = Unique.delayed(initializer)
	}






	private class UniqueBuilder[T] private (private[this] var items :Builder[T, IndexedSeq[T]],
	                                        private[this] var index :Map[T, Int],
	                                        private[this] var array :Array[T],
	                                        private[this] var smallSize :Int)
		extends Builder[T, Unique[T]]
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, -1)

		def this() = this(null, null, new Array[Any](SmallUniqueLimit).asInstanceOf[Array[T]], 0)

		override def addOne(elem :T) :this.type = {
			if (smallSize >= 0) {
				if (array.indexOf(elem) < 0)
					if (smallSize < SmallUniqueLimit) {
						array(smallSize) = elem
						smallSize += 1
					} else {
						if (items == null)
							items = IndexedSeq.newBuilder[T]
						items ++= array
						index = array.view.zipWithIndex.toMap
						smallSize = -1
					}
			} else if (!index.contains(elem)) {
				index = index.updated(elem, index.size)
				items += elem
			}
			this
		}

		override def clear() :Unit = {
			smallSize = 0
			index = null
			items.clear()
			if (array == null)
				array = new Array[Any](SmallUniqueLimit).asInstanceOf[Array[T]]
			else
				java.util.Arrays.fill(array.asInstanceOf[Array[AnyRef]], null)
		}

		override def sizeHint(size :Int) :Unit =
			if (size >= SmallUniqueLimit && smallSize == 0) {
				if (items == null)
					items = IndexedSeq.newBuilder[T]
				items sizeHint size
				index = Map.empty
			}

		override def result() :Unique[T] = {
			val res = smallSize match {
				case 0 => empty[T]
				case 1 => new SingletonUnique(array(0))
				case n if n >= 0 => new SmallUnique[T](array.take(n))
				case _ => new IndexedUnique(items.result(), index)
			}
			clear()
			res
		}
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

		override def :++[U >: T](elems :IterableOnce[U]) :Unique[U] =
			if (elems.iterator.isEmpty) this
			else items :++ elems

//		override def ++:[U >: T](elems :IterableOnce[U]) :Unique[U] =
//			if (elems.iterator.isEmpty) this
//			else elems ++: items : @nowarn

		override def -[U >: T](elem :U) :Unique[T] = items - elem

		override def concat[B >: T](suffix :IterableOnce[B]) :Unique[B] = items ++ suffix

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def iterator :Iterator[T] = items.iterator
		override def reverseIterator :Iterator[T] = items.reverseIterator
	}






	private class IndexedUnique[+T](items :IndexedSeq[T], map :Map[T, Int]) extends Unique[T] {
		private[this] val index = map.withDefaultValue(-1)

		override def iterator :Iterator[T] = items.iterator
		override def reverseIterator :Iterator[T] = items.reverseIterator

		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0


		override def foreach[U](f :T => U) :Unit = items foreach f

		override def apply(idx :Int) :T = items(idx)

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


		override def :++[U >: T](elems :IterableOnce[U]) :Unique[U] = concat(elems)

//		override def ++:[U >: T](elems :IterableOnce[U]) :Unique[U] =
//			if (elems.iterator.isEmpty)
//				this
//			else
//	            (new UniqueBuilder ++= elems ++= this).result()

		override def -[U >: T](elem :U) :Unique[T] =
			index.get(elem.asInstanceOf[T]) match {
				case Some(i) =>
					val view = items.view
					new IndexedUnique[T]((view.take(i) ++ view.drop(i + 1)).toIndexedSeq, index - elem.asInstanceOf[T])
				case _ => this
			}


		override def concat[U >: T](that :IterableOnce[U]) :Unique[U] =
			if (that.iterator.isEmpty)
				this
			else
                (new UniqueBuilder(
					IndexedSeq.newBuilder[U] ++= items,
					index.asInstanceOf[Map[U, Int]]
                ) ++= that).result()

	}






	private class SmallUnique[+T](elements :Array[T]) extends Unique[T] {
		override def size = elements.length
		override def knownSize = size
		override def last = elements(elements.length - 1)
		override def head = elements(0)
		override def tail = new SmallUnique(elements.tail)
		override def init = new SmallUnique(elements.init)

		override def iterator = elements.iterator
		override def reverseIterator = toIndexedSeq.reverseIterator
		override def toIndexedSeq = ArraySeq.unsafeWrapArray(elements)

		override def foreach[U](f :T => U) :Unit = elements.foreach(f)
		override def map[B](f :T => B) :Unique[B] = new SmallUnique(elements.map(f)(ClassTag(classOf[Any])))

		override def apply(n :Int) = elements(n)
		override def indexOf[U >: T](elem :U) = elements.indexOf(elem.asInstanceOf[T])

		override def -[U >: T](elem :U) = indexOf(elem) match {
			case n if n < 0 => this
			case n =>
				val res = Array.ofDim[T](elements.length - 1)(ClassTag(elements.getClass))
				var i = 0; var j = 0
				while (i < elements.length) {
					if (i != n) {
						res(j) = elements(i)
						j += 1
					}
					i += 1
				}
				new SmallUnique(res)
		}

		override def +:[U >: T](elem :U) = indexOf(elem) match {
			case n if n >= 0 => this
			case _ if elements.length >= SmallUniqueLimit =>
				val seq = elements.view.prepended(elem).toIndexedSeq
				val map = seq.view.zipWithIndex.toMap
				new IndexedUnique[U](seq, map)
			case _ =>
				val res = Array.ofDim[U](elements.length + 1)(ClassTag(elements.getClass))
				res(0) = elem
				var i = 0
				while (i < elements.length) {
					res(i + 1) = elements(i); i += 1
				}
				new SmallUnique(res)
		}

		override def :+[U >: T](elem :U) = indexOf(elem) match {
			case n if n >= 0 => this
			case _ if elements.length >= SmallUniqueLimit =>
				val seq = elements.view.appended(elem).toIndexedSeq
				val map = seq.view.zipWithIndex.toMap
				new IndexedUnique(seq, map)
			case _ =>
				val res = Array.copyOf(elements.asInstanceOf[Array[U]], elements.length + 1)
				res(elements.length) = elem
				new SmallUnique(res)
		}

		override def :++[U >: T](elems :IterableOnce[U]) = concat(elems)

		override def concat[U >: T](suffix :IterableOnce[U]) :Unique[U] = suffix match {
			case iterable :Iterable[U] if iterable.isEmpty => this
			case iterator :Iterator[_] if iterator.isEmpty => this
			case _ =>
				val builder = new UniqueBuilder[U]
				val suffixSize = suffix.knownSize
				if (suffixSize >= 0)
					builder.sizeHint(elements.length + suffixSize)
				(builder ++= this ++= suffix).result()
		}
	}


	private[this] final val SmallUniqueLimit = 16




	private class SingletonUnique[+T](override val head :T) extends Unique[T] {
		override def last :T = head
		override def tail = Unique.empty[T]
		override def init = Unique.empty[T]

		override def iterator = Iterator.single(head)
		override def reverseIterator = Iterator.single(head)

		override def foreach[U](f :T => U) :Unit = f(head)

		override def apply(n :Int) =
			if (n == 0) head
			else throw new IndexOutOfBoundsException(s"$n/1")

		override def indexOf[U >: T](elem :U) = if (head == elem) 0 else -1

		override def +:[U >: T](elem :U) =
			if (elem == head) this else Unique(elem, head)

		override def :+[U >: T](elem :U) =
			if (elem == head) this else Unique(head, elem)

		override def :++[U >: T](elems :IterableOnce[U]) =
			if (elems.iterator.isEmpty) this
			else Unique.from(head::elems.iterator.to(List))

		override def -[U >: T](elem :U) :Unique[T] =
			if (elem == head ) Unique.empty[T] else this

		override def toString = "Unique(" + head + ")"
	}




	class EmptyUnique extends Unique[Nothing] {
		override def apply(n :Int) :Nothing = throw new IndexOutOfBoundsException("Unique()(" + n + ")")

		override def indexOf[U >: Nothing](elem :U) :Int = -1

		override def +:[U >: Nothing](elem :U) :Unique[U] = new SingletonUnique(elem)
		override def :+[U >: Nothing](elem :U) :Unique[U] = new SingletonUnique(elem)
		override def :++[U >: Nothing](elems :IterableOnce[U]) :Unique[U] = from(elems)
		override def -[U >: Nothing](elem :U) :Unique[Nothing] = this

		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
	}

	private val EmptyUnique = new EmptyUnique






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

		def toUnique :Unique[T] = unique
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

		def toUnique :Unique[T] = unique
	}


}

