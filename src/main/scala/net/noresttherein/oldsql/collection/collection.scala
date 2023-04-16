package net.noresttherein.oldsql

import java.lang.reflect.Field

import scala.annotation.nowarn
import scala.collection.{Factory, IterableFactory, IterableOps, MapFactory}
import scala.collection.mutable.Builder


import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.ComparableFactory





//todo: rename to collections: no conflict with scala.collection
package object collection {
	//todo: MultiSet
	//todo: compare also factories with implicit evidence
	private val IterableToFactoryClass = scala.collection.Iterable.iterableFactory.getClass
	private val iterableFactoryField :Opt[Field] =
		IterableToFactoryClass.getFields.find(_.getType == classOf[IterableFactory[Iterable]])
	private val MapToFactoryClass = scala.collection.Map.mapFactory.getClass
	private val mapFactoryField :Opt[Field] = MapToFactoryClass.getFields.find(_.getType == classOf[MapFactory[Map]])

	private[oldsql] def companionFactoryOf[E, T](factory :Factory[E, T]) :Opt[Any] =
		factory match {
			case comparable: ComparableFactory[_, _] => Got(comparable.factory)
			case _ if IterableToFactoryClass isAssignableFrom factory.getClass =>
				iterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[Iterable]])
			case _ if MapToFactoryClass isAssignableFrom factory.getClass =>
				mapFactoryField.map(_.get(factory).asInstanceOf[MapFactory[Map]])
			case _ => None
		}

}


package collection {

	import scala.collection.immutable.ArraySeq


	class Permutation private (override val toIndexedSeq :IndexedSeq[Int])
		extends AnyVal with IterableOnce[Int] with IterableOps[Int, IndexedSeq, IndexedSeq[Int]]
	{
		def apply(i :Int) :Int = toIndexedSeq(i)

		override def size   :Int = toIndexedSeq.length
		def length :Int = toIndexedSeq.length

		@throws[IllegalArgumentException]("if items.length != this.length.")
		def reorder[T](items :Seq[T]) :Seq[T] =
			if (items.length != toIndexedSeq.length)
				throw new IllegalArgumentException(
					"Cannot reorder " + items + " according to " + this + " because its length " +
						items.length + " is different than " + length + "."
				)
			else if (items.isEmpty)
				items
			else {
				val res = new Array[Any](toIndexedSeq.length).asInstanceOf[Array[T]]
				val it = items.iterator
				var i = 0
				while (it.hasNext) {
					res(toIndexedSeq(i)) = it.next()
					i += 1
				}
				ArraySeq.unsafeWrapArray(res)
			}

		@nowarn
		override def toIterable :Iterable[Int] = toIndexedSeq
		override def toSeq :Seq[Int] = toIndexedSeq
		override def iterator :Iterator[Int] = toIndexedSeq.iterator
		protected override def coll :IndexedSeq[Int] = toIndexedSeq

		override def iterableFactory :IterableFactory[IndexedSeq] = IndexedSeq

		protected override def fromSpecific(coll :IterableOnce[Int]) :IndexedSeq[Int] =
			coll match {
				case items :Iterable[Int] => items.toIndexedSeq
				case _ => coll.iterator.toIndexedSeq
			}
		protected override def newSpecificBuilder :Builder[Int, IndexedSeq[Int]] = IndexedSeq.newBuilder

		override def toString :String = toIndexedSeq.mkString("Permutation(", ", ", ")")
	}


	object Permutation {
		val empty = new Permutation(IndexedSeq.empty)

		def identity(size :Int) :Permutation = new Permutation(0 until size)

		def fromSeq(indices :IndexedSeq[Int]) :Permutation =
			if (indices.isEmpty)
				new Permutation(indices)
			else if (indices.sorted != indices.indices)
				throw new IllegalArgumentException(
					indices.toString + " is not a valid permutation."
				)
			else
				new Permutation(indices)
	}

}