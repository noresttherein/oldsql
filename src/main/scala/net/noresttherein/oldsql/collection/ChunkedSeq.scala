package net.noresttherein.oldsql.collection

import scala.collection.immutable.{AbstractSeq, LinearSeq, LinearSeqOps, SeqOps}
import scala.collection.{IterableFactory, IterableFactoryDefaults, SeqFactory}
import scala.collection.mutable.Builder






/** A recursive concatenation of sequences and individual elements into a larger sequence. Provides `O(1)` concatenation
  * from either side and similar characteristics to [[scala.collection.mutable.UnrolledBuffer UnrolledBuffer]].
  * Every concatenation creates a new object which stores the two concatenated parts.
  * This class will perform worse in general use than standard sequences, but is useful when concatenations greatly
  * prevail over access - in particular as a temporary structure.
  * @author Marcin Mościcki
  */
/*
sealed trait ChunkedSeq[+E] extends AbstractSeq[E]
//	extends Seq[E] with SeqOps[E, ChunkedSeq, ChunkedSeq[E]] with IterableFactoryDefaults[E, ChunkedSeq]
{
	override def empty :ChunkedSeq[E] = ChunkedSeq.Empty

	override def iterableFactory :SeqFactory[ChunkedSeq] = ChunkedSeq
	override def className :String = "ChunkedSeq"
}



object ChunkedSeq extends SeqFactory[ChunkedSeq] {
	override def from[A](source :IterableOnce[A]) :ChunkedSeq[A] = source match {
		case elems :ChunkedSeq[A] => elems
		case elems :Iterable[A] if elems.isEmpty => Empty
//		case elems :LinearSeq[A] => new LinearChunk(elems)
		case elems :IndexedSeq[A] => new IndexedChunk[A](elems)
		case src :Seq[A] => new SeqChunk[A] { override val elems = src }
		case _ => (newBuilder[A] ++= source).result()
	}

	override def empty[A] :ChunkedSeq[A] = Empty

	override def newBuilder[A] :Builder[A, ChunkedSeq[A]] = List.newBuilder.mapResult {
		case Nil => Empty
		case list => new LinearChunk(list)
	}


	private trait SeqChunk[+E] extends ChunkedSeq[E] {
		protected val elems :Seq[E]
		override def knownSize = elems.knownSize
		override def length = elems.length
		override def apply(i :Int) = elems(i)
		override def iterator = elems.iterator
		override def empty :Empty.type = Empty
	}



	private class IndexedChunk[+E](elems :IndexedSeq[E], offset :Int = 0) extends IndexedSeq[E] with ChunkedSeq[E] {
		override def knownSize = elems.knownSize match {
			case -1 => -1
			case n => n - offset
		}
		override def length = elems.length - offset

		override def apply(i :Int) :E =
			if (i < 0) throw new IndexOutOfBoundsException(i)
			else elems(i + offset)

		override def iterator = elems.iterator.drop(offset)

		override def tail = elems.length - 1 match {
			case this.offset => Empty
			case _ => new IndexedChunk(elems, offset + 1)
		}
	}

	private final class PrependOne[+E](override val head :E, override val tail :ChunkedSeq[E])
		extends ChunkedSeq[E]
	{
		override def knownSize :Int = tail.knownSize match {
			case -1 => -1
			case n => n + 1
		}
		override def length = 1 + tail.length

		override def apply(i :Int) :E =
			if (i < 0) throw new IndexOutOfBoundsException(i)
			else if (i == 0) head
			else tail(i - 1)

		override def iterator :Iterator[E] =
			if (tail.isEmpty) Iterator.single(head)
			else Iterator.single(head) ++ tail.iterator
	}

	private final class AppendOne[+E](override val init :ChunkedSeq[E], override val last :E)
		extends ChunkedSeq[E]
	{
		override def knownSize :Int = tail.knownSize match {
			case -1 => -1
			case n => n + 1
		}
		override def length = 1 + tail.length

		override def apply(i :Int) :E =
			if (i < 0) throw new IndexOutOfBoundsException(i)
			else init.length match {
				case len if len > i => throw new IndexOutOfBoundsException(i)
				case len if len == i => last
				case _ => init(i)
			}

		override def iterator :Iterator[E] =
			if (tail.isEmpty) Iterator.single(head)
			else Iterator.single(head) ++ tail.iterator
	}

	private final class PrependSeq[E](first :Seq[E], second :ChunkedSeq[E]) extends ChunkedSeq[E] {
		override def knownSize :Int = (first.knownSize, second.knownSize) match {
			case (-1, _) => -1
			case (_, -1) => -1
			case (n, m) => n + m
		}
		override def length = first.length + second.length
		override def head = first.head
		override def tail = if (first.sizeIs > 1) new PrependSeq(first.tail, second) else second

		override def apply(i :Int) :E =
			if (i < 0) throw new IndexOutOfBoundsException(i)
			else if (first.sizeIs > i) first(i)
			else second(i - first.length)

		override def iterator :Iterator[E] = first.iterator ++ second.iterator
	}

	private object Empty extends ChunkedSeq[Nothing] {
		override def knownSize = 0
		override def length = 0
		override def apply(i :Int) :Nothing = throw new IndexOutOfBoundsException("ChunkedSeq()(" + i + ")")
		override def head :Nothing = throw new NoSuchElementException("ChunkedSeq().head")
		override def tail :Nothing = throw new UnsupportedOperationException("ChunkedSeq().tail")
		override def iterator = Iterator.empty[Nothing]
	}
}
*/
