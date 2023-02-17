package net.noresttherein.oldsql.collection

import scala.collection.immutable.{ArraySeq, HashSet, IndexedSeq, Iterable, Seq, Set}
import scala.collection.mutable.Builder
import scala.collection.{mutable, AbstractSeq, AbstractSet, Factory, IterableFactory, IterableFactoryDefaults, IterableOps}
import scala.reflect.ClassTag

import net.noresttherein.oldsql.SerialVer
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.collection.Unique.{UniqueSeqAdapter, UniqueSetAdapter}
import net.noresttherein.oldsql.exceptions.raise
import net.noresttherein.oldsql.slang.mappingMethods

//implicits
import net.noresttherein.oldsql.slang.castTypeParam






/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `apply(Int)`,
  * `toSet`, `toSeq`, `toIndexedSeq` and `size` implementations. It can be thought of as a mix of [[Set]] and [[Seq]]:
  * no two elements in the collection are equal, but their position in the collection is important and features
  * in equality (essentially having `Seq` semantics), unlike `Set` equality, including implementations preserving
  * insertion order. Note that this means that sequence-like append/prepend operations will lead to no change
  * in the collection if the added items already exists.
  *
  * Implementations are optimised more towards fast access than efficient incremental expansion.
  * It is recommended to use [[net.noresttherein.oldsql.collection.Unique.newBuilder newBuilder]]
  * when removing or adding multiple elements, especially through methods other than `+` and `++`.
  *
  * This class is most prominently used for the column lists exported by mappings in order to quickly find
  * the column mapping for a given column result in a `ResultSet`.
  * @tparam T element type.
  */ //consider: renaming to SeqSet, RankedSet, Ranked, Ranking or smth, Unique may be a good name for a collection of entities using their id as equality - or we could use Distinct
trait Unique[+T] //todo: revise .view usage for includes/excludes - probably not worth creating a view for it
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


	def reverse :Unique[T] = reverseIterator to Unique

	def reverseIterator :Iterator[T]

	/** The `n`-th element in this collection.
	  * @param n the index in the `[0..size-1]` range.
	  */
	def apply(n :Int) :T

	/** The index of the given element in this collection, or `-1` if it does not contain `elem`. */
	def indexOf[U >: T](elem :U) :Int


	/** Same as the standard [[net.noresttherein.oldsql.collection.Unique.indexOf indexOf]],
	  * but returns the index in an `Opt[Int]` instead of returning `-1` if no such element exists.
	  */
	def getIndexOf[U >: T](elem :U) :Option[Int] = indexOf(elem) match {
		case n if n >= 0 => Some(n)
		case _ => None
	}

	/** Same as the standard [[net.noresttherein.oldsql.collection.Unique.indexOf indexOf]],
	  * but throws a [[NoSuchElementException]] if the element is not found in this collection.
	  * The exception's message lists all the contents, so this method should not be used
	  * if this collection can be large or this might pose a security vulnerability, but the extra information
	  * is much more helpful than in a [[IndexOutOfBoundsException]] which most likely would be caused by returning
	  * `-1`.
	  */
	def sureIndexOf[U >: T](elem :U) :Int = indexOf(elem) match {
		case n if n < 0 => throw new NoSuchElementException("No " + elem + " in " + this + ".")
		case n => n
	}

	/** Checks if this collection contains the given element as defined by `equals`. */
	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0

	/** Adds an element to the collection, if it is not already present.
	  * The difference from [[net.noresttherein.oldsql.collection.Unique.+: +:]] and
	  * [[net.noresttherein.oldsql.collection.Unique.:+ :+]] is that the element can be added at the front
	  * or at the back and does not change its position in the collection if it is already present.
	  * @return `this` ''iff'' it already contains `elem`, or a `Unique` instance containing all elements of `this`,
	  *        followed by `elem` if it does not.
	  */
	def +[U >: T](elem :U) :Unique[U]

	/** Prepends an element to the front of the collection. If the element is already present, it is moved to the front
	  * of the collection, with the preceding elements shift back one place.
	  */
	def +:[U >: T](elem :U) :Unique[U]

	/** Appends an element at the end of the collection. If the element is already present, it is moved to the back
	  * of the collection, with the following elements shift forward one place.
	  */
	def :+[U >: T](elem :U) :Unique[U]

	/** Appends the given elements to this collection, with semantics equivalent to calling
	  * [[net.noresttherein.oldsql.collection.Unique.:+ :+]] for every element in `elems` in its iteration order.
	  */
	def :++[U >: T](elems :IterableOnce[U]) :Unique[U]

	/** Prepends the given elements to this collection, with semantics equivalent to calling
	  * [[net.noresttherein.oldsql.collection.Unique.+: +:]] for every element in `elems`
	  * in its ''reverse'' iteration order.
	  */
	def ++:[U >: T](elems :Iterable[U]) :Unique[U]

	/** Adds the given elements to this collection. The exact order of the elements in the returned collection
	  * is unspecified.
	  */
	def ++[U >: T](elems :IterableOnce[U]) :Unique[U]

	/** Removes the given element from this collection, if present. */
	def -[U >: T](elem :U) :Unique[T]

	/** Removes the given elements from this collection. */
	def --[U >: T](elems :IterableOnce[U]) :Unique[T]


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
		case iter :Iterable[T] if iter.sizeIs == 1 =>
			new SingletonUnique[T](iter.head)
		case iter :Iterable[T] if iter.sizeIs <= SmallUniqueCap =>
			new SmallUnique[T](iter.toArray(ClassTag[T](classOf[AnyRef])))
		case iter :Iterable[T] =>
			val seq = iter.toIndexedSeq
			val map = seq.view.zipWithIndex.toMap
			new IndexedUnique(seq, map)
		case _ => (newBuilder[T] ++= elems).result()
	}

	override def newBuilder[T] :Builder[T, Unique[T]] = new UniqueBuilder[T]()

	override def empty[E] :Unique[E] = EmptyUnique //reusableEmpty

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


	def unapplySeq[T](elems :Unique[T]) :Got[Seq[T]] = Got(elems.toSeq)


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

	object Implicits {
		implicit def uniqueToSeq[T](unique :Unique[T]) :Seq[T] = unique.toSeq
		implicit def uniqueToSet[T](unique :Unique[T]) :Set[T] = unique.toSet
	}




	/** A builder of `Unique` instances working in two modes, depending on the constructor used
	  * and constructor parameters as well as the number of elements added.
	  * In case the parameter list contains
	  *   - no arguments,
	  *   - `SmallUnique`,
	  *   - `smallSize >= 0 && smallSize <= SmallUniqueCap` (if non zero, then arrays `small` and `hashes`
	  *     should be not null and contain that many elements and their computed hash codes in their prefixes),
	  * then it tries first to build a `SmallUnique`, appending elements to to the `small` array.
	  * However, if
	  *   - the number of elements added exceeds `SmallUniqueCap`.
	  *   - `smallSize < 0` is given as a constructor argument,
	  *   - an `IndexedSeq` builder and an index `Map` are given as arguments,
	  *   - or `sizeHint` is called for a number greater than `SmallUniqueCap`,
	  * it switches to the `IndexedUnique` building mode (by setting `smallSize = -1`, where elements are appended
	  * to the builder `large` and inserted in the map `index` associated with the next free index.
	  */
	@SerialVersionUID(SerialVer)
	private final class UniqueBuilder[T] private (private[this] var large     :Builder[T, IndexedSeq[T]],
	                                              private[this] var index     :Map[T, Int], //<-^builds IndexedUnique
	                                              private[this] var small     :Array[T],   //<- builds SmallUnique
	                                              private[this] var hashes    :Array[Int],
	                                              private[this] var smallSize :Int) //number of elements in small or -1 if large is used instead
		extends Builder[T, Unique[T]]
	{
		def this(items :Builder[T, IndexedSeq[T]], index :Map[T, Int]) =
			this(items, index, null, null, -1)

		def this(unique :SmallUnique[T]) =
			this(null, null,
				Array.copyOf(unique.contents, SmallUniqueCap),
				Array.copyOf(unique.hashCodes, SmallUniqueCap),
				unique.size)

		def this() =
			this(null, null, null, null, 0)

		private def smallAddOne(elem :T, hash :Int) :this.type = {
			if (small == null) {
				small = new Array[Any](SmallUniqueCap).asInstanceOf[Array[T]]
				hashes = new Array[Int](SmallUniqueCap)
				small(0) = elem
				hashes(0) = hash
				smallSize = 1
			} else {
				val i = hashes.indexOf(hash)
				if (i < 0 || small(i) != elem)
					if (smallSize < SmallUniqueCap) {
						small(smallSize) = elem
						hashes(smallSize) = hash
						smallSize += 1
					} else {
						if (large == null)
							large = IndexedSeq.newBuilder[T]
						large ++= small
						index = small.view.zipWithIndex.toMap
						smallSize = -1
					}
			}
			this
		}

		private def bigAddOne(elem :T) :this.type = {
			if (!index.contains(elem)) {
				index = index.updated(elem, index.size)
				large += elem
			}
			this
		}

		override def addOne(elem :T) :this.type =
			if (smallSize >= 0)
				smallAddOne(elem, elem.hashCode)
			else
				bigAddOne(elem)

		override def addAll(xs :IterableOnce[T]) :this.type =
			if (smallSize >= 0) xs match {
				case unique :SmallUnique[T] =>
					var i = 0; val count = unique.size
					val elems = unique.contents; val hashes = unique.hashCodes
					while (i < count & smallSize >= 0) {
						smallAddOne(elems(i), hashes(i))
						i += 1
					}
					while (i < count) {
						bigAddOne(elems(i))
						i += 1
					}
					this
				case unique :SingletonUnique[T] => addOne(unique.head)
				case _ => super.addAll(xs)
			} else xs match {
				case unique :IndexedUnique[T] =>
					val elems = unique.toSeq
					val size = index.size
					large ++= elems
					index ++= Iterator.tabulate(elems.length)(i => (elems(i), i + size))
					this
				case _ => super.addAll(xs)
			}


		override def clear() :Unit = {
			smallSize = 0
			index = null
			large = null
			if (small != null)
				java.util.Arrays.fill(small.asInstanceOf[Array[AnyRef]], null)
		}

		override def sizeHint(size :Int) :Unit =
			if (size >= SmallUniqueCap) {
				if (large == null) {
					large = IndexedSeq.newBuilder[T]
					index = Map.empty
				}
				large sizeHint size
				var i = 0
				while (i < smallSize) {
					val elem = small(i)
					large += elem
					index = index.updated(elem, i)
					i += 1
				}
				smallSize = -1 //add directly to the builder, skip the array
			}

		override def result() :Unique[T] = {
			val res = smallSize match {
				case 0 => empty[T]
				case 1 => new SingletonUnique(small(0))
				case SmallUniqueCap => new SmallUnique(small, hashes)
				case n if n >= 0 => new SmallUnique[T](small.take(n), hashes.take(n))
				case _ => new IndexedUnique(large.result(), index)
			}
			clear()
			res
		}
	}



	@SerialVersionUID(1L)
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
		override def +[U >: T](elem :U) :Unique[U] = items + elem

		override def :++[U >: T](elems :IterableOnce[U]) :Unique[U] =
			if (elems.iterator.isEmpty) this else items :++ elems

		override def ++:[U >: T](elems :Iterable[U]) :Unique[U] = elems ++: items
		override def --[U >: T](elems :IterableOnce[U]) :Unique[T] = items -- elems
		override def -[U >: T](elem :U) :Unique[T] = items - elem

		override def concat[B >: T](suffix :IterableOnce[B]) :Unique[B] = items ++ suffix

		override def foreach[U](f :T => U) :Unit = items foreach f
		override def iterator :Iterator[T] = items.iterator
		override def reverse :Unique[T] = items.reverse
		override def reverseIterator :Iterator[T] = items.reverseIterator

		private def writeReplace = items
	}





	//we might consider LinkedHashMap implementation for lesser footprint and faster building, but
	//  1. it's mutable, hence likely problematic even if not shared,
	//  2. it doesn't have any provision for iterating in the reverse order.
	@SerialVersionUID(1L)
	private class IndexedUnique[+T](items :IndexedSeq[T], map :Map[T, Int]) extends Unique[T] {
		def this(items :IndexedSeq[T]) = this(items, items.view.mapWithIndex { (t, i) => (t, i) }.toMap)

		private[this] val index = map.withDefaultValue(-1)

		override def iterator :Iterator[T] = items.iterator
		override def reverse = new IndexedUnique(items.reverse)
		override def reverseIterator :Iterator[T] = items.reverseIterator

		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def apply(idx :Int) :T = items(idx)

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])

		override def +[U >: T](elem :U) :Unique[U] =
			if (contains(elem)) this
			else new IndexedUnique(elem +: items, index.asInstanceOf[Map[U, Int]].updated(elem, size))

		override def :+[U >: T](elem :U) :Unique[U] = indexOf(elem) match {
			case -1 =>
				new IndexedUnique(items :+ elem, index.asInstanceOf[Map[U, Int]].updated(elem, size))
			case n =>
				val b = IndexedSeq.newBuilder[U]; b sizeHint size
				var i = 0
				val it = items.iterator
				while (i < n) {
					b += it.next()
					i += 1
				}
				it.next(); i += 1; val len = items.length
				var newIndex = index
				while (i < len) {
					val e = it.next()
					b += e
					newIndex = newIndex.updated(e, i - 1)
					i += 1
				}
				b += elem
				new IndexedUnique(b.result(), newIndex.asInstanceOf[Map[U, Int]].updated(elem, len - 1))
		}

		override def +:[U >: T](elem :U) :Unique[U] =
			if (contains(elem)) this
			else
                new IndexedUnique(
					elem +: items,
                    index.asInstanceOf[Map[U, Int]].map(pair => pair._1 -> (pair._2 + 1)).updated(elem, 0)
                )

		override def :++[U >: T](elems :IterableOnce[U]) :Unique[U] = {
			val it = elems.iterator
			if (it.isEmpty) this
			else {
				val intersection = mutable.HashSet.empty[U]
				var size = 0
				val suffix = List.newBuilder[U]
				while (it.hasNext) {
					val e = it.next()
					if (index.contains(e.asInstanceOf[T]))
						intersection += e
					else
						size += 1
					suffix += e
				}
				val res = new UniqueBuilder(IndexedSeq.newBuilder[U], Map.empty[U, Int])
				res sizeHint this.size + size
				val self = items.iterator
				while (self.hasNext) {
					val e = self.next()
					if (!intersection(e))
						res += e
				}
				(res ++= suffix.result()).result()
			}
		}

		override def ++:[U >: T](elems :Iterable[U]) :Unique[U] =
			if (elems.isEmpty) this
			else (new UniqueBuilder(IndexedSeq.newBuilder[U] ++= elems, elems.view.zipWithIndex.toMap) ++= items).result()

		override def -[U >: T](elem :U) :Unique[T] =
			index.get(elem.asInstanceOf[T]) match {
				case Some(i) =>
					val view = items.view; val tail = view.drop(i + 1)
					var newIndex = index - elem.asInstanceOf[T]
					tail foreach { e => newIndex = newIndex.updated(e, index(e) - 1) }
					new IndexedUnique[T]((view.take(i) ++ tail).toIndexedSeq, newIndex)
				case _ => this
			}

		override def --[U >: T](elems :IterableOnce[U]) :Unique[T] =
			elems match {
				case _ if isEmpty => this
				case empty :Iterable[_] if empty.isEmpty => this
				case _ =>
					val excludes = elems.iterator.toSet
					val it = items.iterator
					val b = newSpecificBuilder
					while (it.hasNext) {
						val e = it.next()
						if (!excludes(e))
							b += e
					}
					b.result()
		}


		override def concat[U >: T](that :IterableOnce[U]) :Unique[U] =
			if (that.iterator.isEmpty)
				this
			else
                (new UniqueBuilder(IndexedSeq.newBuilder[U] ++= items, index.asInstanceOf[Map[U, Int]]) ++= that).result()

		override def toIndexedSeq :IndexedSeq[T] = items
		override def toSeq :Seq[T] = items
		private[Unique] def indices :Map[_ <: T, Int] = map

		private[this] def writeReplace = new UniqueSerializer(this)
	}






	@SerialVersionUID(1L)
	private final class SmallUnique[+T] private[Unique] (elements :Array[T], hashes :Array[Int]) extends Unique[T] {
		def this(elements :Array[T]) = this(elements, elements.map(_.hashCode))

		private[Unique] def contents[U >: T]  :Array[U]   = elements.asInstanceOf[Array[U]]
		private[Unique] def hashCodes :Array[Int] = hashes

		@inline override def size = elements.length
		override def knownSize = size
		override def last = elements(elements.length - 1)
		override def head = elements(0)
		override def tail = new SmallUnique(elements.tail)
		override def init = new SmallUnique(elements.init)

		override def iterator = elements.iterator
		override def reverse = new SmallUnique(elements.reverse, hashes.reverse)
		override def reverseIterator = toIndexedSeq.reverseIterator
		override def toIndexedSeq = ArraySeq.unsafeWrapArray(elements)

		override def foreach[U](f :T => U) :Unit = elements.foreach(f)
		override def map[B](f :T => B) :Unique[B] = new SmallUnique(elements.map(f)(ClassTag(classOf[Any])))
		override def filterNot(pred :T => Boolean) :Unique[T] = filterImpl(pred, false)
		override def filter(pred :T => Boolean) :Unique[T] = filterImpl(pred, true)

		private def filterImpl(pred :T => Boolean, truth :Boolean) :Unique[T] = {
			val filtered = Array.copyOf(elements, elements.length)
			val filteredHashes = new Array[Int](elements.length)
			var i = 0; var j = 0; val len = elements.length
			while (i < len) {
				if (pred(filtered(i)) == truth) {
					filtered(j) = filtered(i)
					filteredHashes(j) = hashes(i)
					j += 1
				}
				i += 1
			}
			if (j == 0) Unique.empty[T]
			else if (j == 1) Unique.single(filtered(0))
			else new SmallUnique[T](Array.copyOf(filtered, j), Array.copyOf(filteredHashes, j))
		}

		override def apply(n :Int) = elements(n)

		override def indexOf[U >: T](elem :U) :Int = indexOf(elem, elem.hashCode)
		def indexOf[U >: T](elem :U, hash :Int) :Int = hashes.indexOf(hash) match {
			case n if n >= 0 && elements(n) == elem => n
			case n => n
		}

		override def -[U >: T](elem :U) = indexOf(elem) match {
			case n if n < 0 => this
			case n =>
				val res = Array.ofDim[T](elements.length - 1)(ClassTag(elements.getClass.getComponentType))
				val hs  = new Array[Int](elements.length - 1)
				var i = 0; var j = 0
				while (i < elements.length) {
					if (i != n) {
						res(j) = elements(i)
						hs(j) = hs(i)
						j += 1
					}
					i += 1
				}
				new SmallUnique(res)
		}

		override def --[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case _ =>
				val remove = new Array[Boolean](elements.length)
				var size = elements.length
				val it = elems.iterator
				while (it.hasNext)
					indexOf(it.next()) match {
						case n if n < 0 =>
						case n =>
							remove(n) = true
							size -= 1
					}
				size match {
					case 0 => Unique.empty
					case 1 =>
						var i = elements.length - 1
						while (remove(i))
							i -= 1
						new SingletonUnique(elements(i))
					case _ =>
						val res = Array.ofDim[T](size)(ClassTag(elements.getClass.getComponentType))
						val hs = new Array[Int](size)
						var i = 0; var j = 0
						while (j < size) {
							if (!remove(i)) {
								res(j) = elements(i)
								hs(j) = hashes(i)
								j += 1
							}
							i += 1
						}
						new SmallUnique(res, hs)
				}
		}

		override def +:[U >: T](elem :U) = {
			val hash = elem.hashCode
			indexOf(elem, hash) match {
				case n if n >= 0 =>
					val res = Array.copyOf(elements.asInstanceOf[Array[U]], size)
					val hs = Array.copyOf(hashes, size)
					var i = n
					while (i > 0) {
						res(i) = res(i - 1)
						hs(i) = hs(i - 1)
						i -= 1
					}
					res(0) = elem
					hs(0) = hash
					new SmallUnique(res, hs)
				case _ if elements.length >= SmallUniqueCap =>
					val seq = elements.view.prepended(elem).toIndexedSeq
					val map = seq.view.zipWithIndex.toMap
					new IndexedUnique[U](seq, map)
				case _ =>
					val res = Array.ofDim[U](elements.length + 1)(ClassTag(elements.getClass.getComponentType))
					val hs  = new Array[Int](elements.length + 1)
					res(0) = elem
					hs(0) = hash
					var i = 0
					while (i < elements.length) {
						res(i + 1) = elements(i)
						hs(i + 1) = hashes(i)
						i += 1
					}
					new SmallUnique(res, hs)
			}
		}

		override def :+[U >: T](elem :U) = {
			val hash = elem.hashCode
			indexOf(elem, hash) match {
				case n if n >= 0 =>
					val res = Array.copyOf(elements.asInstanceOf[Array[U]], size)
					val hs = Array.copyOf(hashes, size)
					var i = n
					while (i < elements.length - 1) {
						res(i) = res(i + 1)
						hs(i) = hs(i + 1)
						i += 1
					}
					res(i) = elem
					hs(i) = hash
					new SmallUnique(res, hs)
				case _ => append(elem, hash)
			}
		}

		override def +[U >: T](elem :U) = {
			val hash = elem.hashCode
			indexOf(elem, hash) match {
				case n if n >= 0 => this
				case _ => append(elem, hash)
			}
		}

		private def append[U >: T](elem :U, hash :Int) :Unique[U] =
			if (elements.length >= SmallUniqueCap) {
				val seq = elements.view.appended(elem).toIndexedSeq
				val map = seq.view.zipWithIndex.toMap
				new IndexedUnique(seq, map)
			} else {
				val size = elements.length
				val res = Array.copyOf(elements.asInstanceOf[Array[U]], size + 1)
				val hs = Array.copyOf(hashes, size + 1)
				res(size) = elem
				hs(size) = hash
				new SmallUnique(res, hs)
			}

		override def ++:[U >: T](elems :Iterable[U]) = (newBuilder[U] ++= this ++= elems).result()

		override def :++[U >: T](elems :IterableOnce[U]) :Unique[U] = {
			val it = elems.iterator
			if (it.isEmpty) this
			else {
				val intersection = mutable.HashSet.empty[U]
				var size = 0
				val suffix = List.newBuilder[U]
				while (it.hasNext) {
					val e = it.next()
					if (contains(e.asInstanceOf[T]))
						intersection += e
					else
						size += 1
					suffix += e
				}
				val res = newBuilder[U]
				res sizeHint this.size + size
				val self = iterator
				while (self.hasNext) {
					val e = self.next()
					if (!intersection(e))
						res += e
				}
				(res ++= suffix.result()).result()
			}
		}

		override def concat[U >: T](suffix :IterableOnce[U]) :Unique[U] = suffix match {
			case iterable :Iterable[U] if iterable.isEmpty => this
			case iterator :Iterator[_] if iterator.isEmpty => this
			case small :SmallUnique[U] if small.size + elements.length <= SmallUniqueCap =>
				def concatSmall = {
					val smallSize = small.size
					var size = elements.length
					val tmpItems = Array.copyOf(elements.asInstanceOf[Array[U]], smallSize + size)
					val tmpHashes = Array.copyOf(hashes, smallSize + size)
					val smallItems = small.contents
					val smallHashes = small.hashCodes
					var i = 0
					while (i < smallSize) {
						val h = smallHashes(i);
						val e = smallItems(i)
						val idx = hashes.indexOf(h)
						if (idx < 0 || elements(i) != e) {
							tmpItems(size) = e
							tmpHashes(size) = h
							size += 1
						}
						i += 1
					}
					if (size == tmpItems.length)
						new SmallUnique(tmpItems, tmpHashes)
					else {
						val resItems = Array.copyOf(tmpItems, size)
						val resHashes = Array.copyOf(tmpHashes, size)
						new SmallUnique(resItems, resHashes)
					}
				}
				concatSmall
			case _ =>
				val builder = new UniqueBuilder[U](this)
//				val suffixSize = suffix.knownSize
//				if (suffixSize >= 0)
//					builder.sizeHint(elements.length + suffixSize)
				(builder ++= suffix).result()
		}

		override def toSeq :Seq[T] = ArraySeq.unsafeWrapArray(elements)

		private def writeReplace = new UniqueSerializer(elements)
	}


	private[this] final val SmallUniqueCap = 16 //low in case we use it with objects with expensive equals/hashCode




	@SerialVersionUID(1L)
	private class SingletonUnique[+T](override val head :T) extends Unique[T] {
		override def last :T = head
		override def tail = Unique.empty[T]
		override def init = Unique.empty[T]

		override def iterator = Iterator.single(head)
		override def reverse :Unique[T] = this
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

		override def +[U >: T](elem :U) =
			if (elem == head) this else Unique(head, elem)

		override def ++:[U >: T](elems :Iterable[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case _ => (newBuilder[U] ++= elems += head).result()
		}

		override def :++[U >: T](elems :IterableOnce[U]) = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case unique :Unique[U] => head +: unique
			case _ =>
				Unique.from(new Iterator[U] {
					private[this] val it = elems.iterator
					private[this] var i = 0
					override def hasNext = it.hasNext
					override def next() = { i += 1; if (i == 1) SingletonUnique.this.head else it.next() }
				})
		}

		override def concat[B >: T](suffix :IterableOnce[B]) :Unique[B] = suffix match {
			case unique :Unique[B] => unique + head
			case _ => this :++ suffix
		}

		override def -[U >: T](elem :U) :Unique[T] =
			if (elem == head) Unique.empty[T] else this

		override def --[U >: T](elems :IterableOnce[U]) :Unique[T] = elems match {
			case empty :Iterable[_] if empty.isEmpty => this
			case unique :Unique[U] => if (unique.contains(head)) Unique.empty else this
			case set :HashSet[U @unchecked] => if (set(head)) Unique.empty else this
			case set :mutable.HashSet[U @unchecked] => if (set(head)) Unique.empty else this
			case set :mutable.LinkedHashSet[U @unchecked] => if (set(head)) Unique.empty else this
			case _ =>
				val it = elems.iterator
				var unequal = false
				while (it.hasNext && { unequal = it.next() != head; unequal })
					()
				if (unequal) this else Unique.empty
		}

		override def toSeq :Seq[T] = head::Nil

		override def toString = "Unique(" + head + ")"

		private def writeReplace = new UniqueSerializer[Any](Array[Any](head))
	}




	@SerialVersionUID(1L)
	private[oldsql] class EmptyUnique extends Unique[Nothing] {
		override def apply(n :Int) :Nothing = throw new IndexOutOfBoundsException("Unique()(" + n + ")")

		override def indexOf[U >: Nothing](elem :U) :Int = -1

		override def +:[U >: Nothing](elem :U) :Unique[U] = new SingletonUnique(elem)
		override def :+[U >: Nothing](elem :U) :Unique[U] = new SingletonUnique(elem)
		override def +[U >: Nothing](elem :U) :Unique[U] = new SingletonUnique(elem)
		override def concat[U >: Nothing](elems :IterableOnce[U]) :Unique[U] = from(elems)
		override def :++[U >: Nothing](elems :IterableOnce[U]) :Unique[U] = from(elems)
		override def ++:[U >: Nothing](elems :Iterable[U]) :Unique[U] = from(elems)
		override def -[U >: Nothing](elem :U) :Unique[Nothing] = this
		override def --[U >: Nothing](elems :IterableOnce[U]) :Unique[Nothing] = this

		override def iterator :Iterator[Nothing] = Iterator.empty
		override def reverse :Unique[Nothing] = this
		override def reverseIterator :Iterator[Nothing] = Iterator.empty
		override def toSeq :Seq[Nothing] = Nil

		private def readResolve = EmptyUnique
	}

	private val EmptyUnique = new EmptyUnique






	@SerialVersionUID(1L)
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

		private def writeReplace = (unique :Unique[Any]) to ArraySeq
	}



	@SerialVersionUID(1L)
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

		private def writeReplace = unique to HashSet
	}



	@SerialVersionUID(1L)
	private class UniqueSerializer[+E](elems :Array[E]) extends Serializable {
		def this(elems :Unique[E]) = this((elems :Unique[Any]).to(Array).castParam[E])

		private def readResolve =
			if (elems.length <= SmallUniqueCap) new SmallUnique(elems)
			else Unique.from(ArraySeq.unsafeWrapArray(elems))
	}
}

