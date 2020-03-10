package net.noresttherein.oldsql.collection


import net.noresttherein.oldsql.collection.Unique.{IndexedUnique, UniqueSeqAdapter, UniqueSetAdapter}

import scala.collection.immutable.{IndexedSeq, Seq}
import scala.collection.mutable.{Builder, ListBuffer}
import scala.collection.{AbstractSeq, AbstractSet, GenTraversableOnce, IterableLike}
import scala.collection.generic.{CanBuildFrom, GenericCompanion, GenericTraversableTemplate, TraversableFactory}


/** A collection of unique items in a specific order providing `O(1)` `indexOf(T)`, `contains(T)`, `toSet`, `toSeq`
  * and `size` implementations.
  * This class is used for the column lists exported by mappings in order to quickly find the column mapping for
  * a given column result in a `ResultSet`.
  *
  * @tparam T element type.
  */
trait Unique[+T] extends collection.immutable.Iterable[T] with IterableLike[T, Unique[T]]
                    with GenericTraversableTemplate[T, Unique]
{ unique =>

	protected[this] override def newBuilder :Builder[T, Unique[T]] = Unique.newBuilder

	override def genericBuilder[B] :Builder[B, Unique[B]] = Unique.newBuilder[B]

	override def companion :GenericCompanion[Unique] = Unique

	def apply(idx :Int) :T

	def indexOf[U >: T](elem :U) :Int

	def contains[U >: T](elem :U) :Boolean = indexOf(elem) >= 0


	override def toIndexedSeq :IndexedSeq[T] = new UniqueSeqAdapter(this)

	override def toSeq :Seq[T] = new UniqueSeqAdapter(this)

	override def toSet[U >: T] :Set[U] = new UniqueSetAdapter(this)


	def +:[U >: T, That](elem :U)(implicit bf :CanBuildFrom[Unique[T], U, That]) :That

	def :+[U >: T, That](elem :U)(implicit bf :CanBuildFrom[Unique[T], U, That]) :That

}




/** Companion object serving as a factory for Unique - sequence-like collections with fast indexOf operations. */
object Unique extends TraversableFactory[Unique] {


	def apply[T](elems :Iterable[T]) :Unique[T] = elems match {
		case _ :Unique[_] => elems.asInstanceOf[Unique[T]]
		case seq :UniqueSeqAdapter[_] => seq.toUniqueSeq.asInstanceOf[Unique[T]]
		case set :UniqueSetAdapter[_] => set.toUniqueSeq.asInstanceOf[Unique[T]]
		case _ => (newBuilder[T] ++= elems).result
	}

	def newBuilder[T] :Builder[T, Unique[T]] = new UniqueBuilder[T]()



	implicit def uniqueToSeq[T](unique :Unique[T]) :Seq[T] = unique.toSeq

	implicit def uniqueToSet[T](unique :Unique[T]) :Set[T] = unique.toSet

	implicit class implicitUnique[T](private val elems :Iterable[T]) extends AnyVal {
		/** A shorthand method for converting any Seq to an Unique.
		  * Implementation always returns this, but there is an implicit conversion available from Seq to Unique,
		  * so instead of writing Unique(aSeq) you can write aSeq.indexed.
		  * If aSeq's dynamic type was already an Unique, it returns itself cast to Unique.
		  * Of course, if aSeq static type already was an Unique, it is a simple static noOp call.
		  */
		final def unique :Unique[T] = apply(elems)
	}



	private class UniqueBuilder[T](
			private[this] var items :Builder[T, IndexedSeq[T]] = IndexedSeq.newBuilder[T],
			private[this] var index :Map[T, Int] = Map[T, Int]())
		extends Builder[T, Unique[T]]
	{

		override def +=(elem :T) :this.type = {
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



	private class IndexedUnique[+T](items :IndexedSeq[T], index :Map[T, Int]) extends Unique[T] {
		override def iterator :Iterator[T] = items.iterator

		override def size :Int = index.size
		override def isEmpty :Boolean = size == 0


		override def apply(idx :Int) :T = items(idx)

		override def foreach[U](f :T => U) :Unit = items foreach f

		override def indexOf[U >: T](elem :U) :Int = index(elem.asInstanceOf[T])

		override def +:[U >: T, That](elem :U)(implicit bf :CanBuildFrom[Unique[T], U, That]) :That =
			if (bf == Unique.ReusableCBF)
				if (contains(elem)) this.asInstanceOf[That]
				else new IndexedUnique(items :+ elem, index.asInstanceOf[Map[U, Int]].updated(elem, size)).asInstanceOf[That]
			else if (contains(elem)) {
				val b = bf(this)
				b sizeHint size
				(b ++= this).result
			} else {
				val b = bf(this)
				b sizeHint size + 1
				(b ++= this += elem).result
			}

		override def :+[U >: T, That](elem :U)(implicit bf :CanBuildFrom[Unique[T], U, That]) :That =
			if (bf == Unique.ReusableCBF)
				if (contains(elem)) this.asInstanceOf[That]
				else
                    new IndexedUnique(
						items :+ elem,
	                    index.asInstanceOf[Map[U, Int]].mapValues(_ + 1).updated(elem, 0)
                    ).asInstanceOf[That]
			else if (contains(elem)) {
				val b = bf(this)
				b sizeHint size
				(b ++= this).result
			} else {
				val b = bf(this)
				b sizeHint size + 1
				(b += elem ++= this).result
			}

		override def ++[U >: T, That](that :GenTraversableOnce[U])(implicit bf :CanBuildFrom[Unique[T], U, That]) :That =
			if (bf == ReusableCBF)
				if (that.isEmpty)
					this.asInstanceOf[That]
				else
	                (new UniqueBuilder(
						IndexedSeq.newBuilder[U] ++= items,
						index.asInstanceOf[Map[U, Int]]) ++= that.seq
					).result.asInstanceOf[That]
			else
				(bf(this) ++= this ++= that.seq).result

	}



	private class UniqueSeqAdapter[+T](unique :Unique[T]) extends AbstractSeq[T] with IndexedSeq[T] {
		override def length :Int = unique.size

		override def apply(idx :Int) :T = unique(idx)

		override def indexOf[U >: T](elem :U) :Int = unique.indexOf(elem)

		override def lastIndexOf[U >: T](elem :U) :Int = unique.indexOf(elem)

		override def contains[U >: T](elem :U) :Boolean = unique.contains(elem)

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toSet[U >: T] :Set[U] = unique.toSet

		def toUniqueSeq :Unique[T] = unique
	}



	private class UniqueSetAdapter[T](unique :Unique[T]) extends AbstractSet[T] with Set[T] {
		override def size :Int = unique.size

		override def contains(elem :T) :Boolean = unique.contains(elem)

		override def +(elem :T) :Set[T] = if (contains(elem)) this else Set(toSeq:_*)

		override def -(elem :T) :Set[T] = if (contains(elem)) Set(toSeq:_*) - elem else this

		override def iterator :Iterator[T] = unique.iterator

		override def foreach[U](f :T => U) :Unit = unique foreach f

		override def toIndexedSeq :IndexedSeq[T] = unique.toIndexedSeq

		override def toSeq :Seq[T] = unique.toSeq

		def toUniqueSeq :Unique[T] = unique
	}


}

