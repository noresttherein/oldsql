package net.noresttherein.oldsql.collection

import scala.annotation.tailrec
import scala.collection.immutable.AbstractSeq
import scala.collection.{Factory, SeqFactory}
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.collection.ReversedList.{NonEmpty, ReversedFactory}




/** A linked list with a reverse iteration order, granting an O(1) append operation.
  * At the same time, prepending converts it to list and returns a new `List[E]` in anticipation of future prepends.
  */
private[oldsql] sealed abstract class ReversedList[+E] extends AbstractSeq[E] {

	override def appendedAll[B >: E](elems :IterableOnce[B]) :ReversedList[B] = elems match {
		case col :Iterable[B] => col.foldLeft(this :ReversedList[B])(_ appended _)
		case _ => elems.iterator.foldLeft(this :ReversedList[B])(_ appended _)
	}
	@inline final override def appended[B >: E](elem :B) :ReversedList[B] = new NonEmpty(this, elem, length + 1)

	@inline final override def prependedAll[B >: E](elems :IterableOnce[B]) :Seq[B] = elems ++: toList
	@inline final override def prepended[B >: E](elem :B) :Seq[B] = elem :: toList

	override def reduce[B >: E](op :(B, B) => B) :B = reduceRight(op)
	override def reduceOption[B >: E](op :(B, B) => B) :Option[B] = reduceRightOption(op)
	override def fold[A1 >: E](z :A1)(op :(A1, A1) => A1) :A1 = foldRight(z)(op)

	override def reduceLeft[B >: E](op :(B, E) => B) :B =
		if (init.isEmpty) last else init.foldLeft(last :B)(op)

	override def reduceRight[B >: E](op :(E, B) => B) :B = {
		@tailrec def rec(elems :Seq[E] = init, acc :B = last) :B = elems match {
			case link :NonEmpty[E] => rec(link.init, op(link.last, acc))
			case other if other.nonEmpty => other.foldRight(acc)(op)
			case _ => acc
		}
		rec()
	}
	override def foldRight[B](z :B)(op :(E, B) => B) :B = {
		@tailrec def rec(elems :ReversedList[E] = this, acc :B = z) :B = elems match {
			case link :NonEmpty[E] => rec(link.init, op(link.last, acc))
			case other if other.nonEmpty => other.foldRight(acc)(op)
			case _ => acc
		}
		rec()
	}

	override def map[B](f :E => B) :Seq[B] =
		if (isEmpty) ReversedList.Empty
		else init.map(f) :+ f(last)

	override def foreach[U](f :E => U) :Unit =
		if (!isEmpty) {
			init.foreach(f)
			f(last)
		}


	override def to[C](factory :Factory[E, C]) :C = factory match {
		case _ :ReversedFactory[_] => this.asInstanceOf[C]
		case _ => factory.fromSpecific(reversed)
	}

//	override def toList :List[E] = list
	override def toList :List[E] = {
		@tailrec def rec(link :Seq[E] = this, acc :List[E] = Nil) :List[E] = link match {
			case nonEmpty :NonEmpty[E] => rec(nonEmpty.init, nonEmpty.last :: acc)
			case other if other.nonEmpty => other ++: acc
			case _ => acc
		}
		rec()
	}

	override def reverse :Seq[E] = inverse
	private lazy val inverse = last::init.reverse.toList

	override def reverseIterator :Iterator[E] = new Iterator[E] {
		var rest :Seq[E] = ReversedList.this
		var iter :Iterator[E] = _

		override def hasNext :Boolean =
			if (iter == null) rest.nonEmpty else iter.hasNext

		override def next() :E = rest match {
			case link :NonEmpty[E] =>
				val res = link.last; rest = init; res
			case other if other.nonEmpty =>
				iter = other.reverseIterator
				rest = Nil
				iter.next()
			case _ =>
				iter.next()
		}
	}

//	override def iterableFactory :SeqFactory[ReversedList] = ReversedList
	override def className :String = "Reversed"
}






private[oldsql] object ReversedList extends SeqFactory[ReversedList] {
	def :+[A](elem :A) :ReversedList[A] = new NonEmpty(Empty, elem, 1)

	override def from[A](source :IterableOnce[A]) :ReversedList[A] = source match {
		case rev :ReversedList[A] => rev
		case iter :Iterable[A] => if (iter.isEmpty) Empty else (newBuilder[A] ++= iter).result()
		case iter :Iterator[A] => if (!iter.hasNext) Empty else (newBuilder[A] ++= iter).result()
		case _ => (newBuilder[A] ++= source).result()
	}
	override def empty[A] :ReversedList[A] = Empty

	override def newBuilder[A] :Builder[A, ReversedList[A]] = new ReversedBuilder[A]

	private class ReversedFactory[A] extends Factory[A, ReversedList[A]] {
		override def fromSpecific(it :IterableOnce[A]) = ReversedList.from(it)
		override def newBuilder = ReversedList.newBuilder
	}

	private class ReversedBuilder[A](private[this] var buffer :ReversedList[A] = Empty) extends Builder[A, ReversedList[A]] {
		override def clear() :Unit = buffer = Empty

		override def addOne(elem :A) = {
			buffer = new NonEmpty(buffer, elem, buffer.length + 1)
			this
		}
		override def result() = { val res = buffer; buffer = Empty; res }
	}

	private object Empty extends ReversedList[Nothing] {
		override def knownSize = 0
		override def length = 0
		override def apply(i :Int) = throw new IndexOutOfBoundsException(i)
		override def iterator = Iterator.empty
	}

	private[oldsql] class NonEmpty[+E](override val init :ReversedList[E], override val last :E, override val length :Int)
		extends ReversedList[E]
	{
		override def knownSize = length

		override def apply(i :Int) :E =
			if (i >= length) throw new IndexOutOfBoundsException(i)
			else if (i == length - 1) last
			else init(i)

		override def iterator = reversed.iterator
	}

}



