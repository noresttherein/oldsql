package net.noresttherein.oldsql.morsels

import java.lang

import scala.annotation.tailrec
import scala.collection.immutable.{IndexedSeqOps, SeqOps, StringOps, WrappedString}
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.morsels.ChunkedString.{ChunkList, ReverseChunkList, Chunk}






/** A recursive sequence of strings with O(1) concatenation on both sides without a preference,
  * used as a temporary structure when building longer strings.
  */
sealed trait ChunkedString extends CharSequence with Seq[Char] with SeqOps[Char, Seq, ChunkedString] with Serializable {
	override def charAt(index :Int) :Char = apply(index)
	override def subSequence(start :Int, end :Int) :CharSequence = slice(start, end)

	override def empty :ChunkedString = ChunkedString.empty

	protected override def fromSpecific(coll :IterableOnce[Char]) :ChunkedString = coll match {
		case chunks :ChunkedString => chunks
		case _ => new Chunk(coll.toString)
	}

	protected override def newSpecificBuilder :Builder[Char, ChunkedString] =
		(new StringBuilder).mapResult(new Chunk(_))

	def +(char :Char) :ChunkedString = this + String.valueOf(char)
	def +(string :String) :ChunkedString = if (string.length == 0) this else this + ChunkedString(string)
	def +(string :ChunkedString) :ChunkedString = if (string.isEmpty) this else new ReverseChunkList(string::this::Nil)
	def +:(char :Char) :ChunkedString = String.valueOf(char) +: this
	def +:(string :String) :ChunkedString = if (string.isEmpty) this else ChunkedString(string) +: this
	def +:(string :ChunkedString) :ChunkedString = if (string.isEmpty) this else new ChunkList(string::this::Nil)

	protected def appendTo(builder :java.lang.StringBuilder) :java.lang.StringBuilder

	override def toString :String = appendTo(new java.lang.StringBuilder).toString
}




object ChunkedString {
	val empty :ChunkedString = new ReversedStringList(Nil)

	private val emptyChunk = new Chunk("")

	def apply(string :String) :ChunkedString =
		if (string.length == 0) empty else new Chunk(string)

	implicit def fromString(s :String) :ChunkedString = new Chunk(s)



	private class Chunk(s :String)
		extends ChunkedString with IndexedSeq[Char] with IndexedSeqOps[Char, IndexedSeq, Chunk]
	{
		override def head :Char = s.charAt(0)
		override def last :Char = s.charAt(s.length - 1)
		override def apply(i :Int) = s.charAt(i)
		override def length = s.length
		override def isEmpty = s.length == 0
		override def iterator = new StringOps(s).iterator
		override def reverseIterator = new StringOps(s).reverseIterator

		override def empty = emptyChunk

		override def newSpecificBuilder :Builder[Char, Chunk] =
			(new StringBuilder).mapResult(new Chunk(_))

		protected override def fromSpecific(coll :IterableOnce[Char]) :Chunk = coll match {
			case s :Chunk => s
			case _ => new Chunk((new StringBuilder ++= coll).result())
		}

		override def +(string :String) = new ReversedStringList(string::s::Nil)

		override def +(string :ChunkedString) = string match {
			case simple :Chunk => new ReversedStringList(simple.toString::s::Nil)
			case _ => new ReverseChunkList(string::this::Nil)
		}

		override def +:(string :String) = new StringList(s::string::Nil)

		override def +:(string :ChunkedString) = string match {
			case simple :Chunk => new StringList(simple.toString::s::Nil)
			case _ => new ChunkList(string::this::Nil)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) = builder.append(s)

		override def toString = s
	}



	private class ReversedStringList(reversed :List[String]) extends ChunkedString {
		override def head = reversed.last.charAt(0)
		override def last = { val h = reversed.head; h.charAt(h.length - 1) }
		override def apply(i :Int) :Char = {
			var offset = i
			val chunk = reversed.reverseIterator
			while (chunk.hasNext) {
				val s = chunk.next()
				if (offset < s.length)
					return s.charAt(i)
				else offset -= s.length
			}
			throw new StringIndexOutOfBoundsException(s"""$i-th char of "$this"""")
		}

		override def isEmpty = reversed.isEmpty
		override lazy val length = (0 /: reversed)(_ + _.length)

		override def iterator = reversed.reverseIterator.flatten(new WrappedString(_))

		override def +(string :String) =
			if (string.length == 0) this else new ReversedStringList(string::reversed)

		override def +(string :ChunkedString) = string match {
			case _ if string.isEmpty => this
			case _ :Chunk => new ReversedStringList(string.toString::reversed)
			case _ => new ReverseChunkList(string::this::Nil)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) = {
			reversed.reverseIterator.foreach(builder.append); builder
		}

		override def toString = {
			val res = new java.lang.StringBuilder
			reversed.reverseIterator.foreach(res.append)
			res.toString
		}
	}



	private class StringList(parts :List[String]) extends ChunkedString {
		override def apply(idx :Int) :Char = {
			@tailrec def rec(i :Int = idx, chunks :List[String] = parts) :Char = chunks match {
				case h::t => if (h.length > i) h.charAt(i) else rec(i - h.length, t)
				case _ => throw new StringIndexOutOfBoundsException(s"""$idx-th char of "$this"""")
			}
			rec()
		}

		override lazy val length = (0 /: parts)(_ + _.length)

		override def iterator = parts.iterator.flatten(new WrappedString(_))

		override def +:(string :String) =
			if (string.length == 0) this else new StringList(string::parts)

		override def +:(string :ChunkedString) = string match {
			case _ if string.isEmpty => this
			case _ :Chunk => new StringList(string.toString::parts)
			case _ => super.+:(string)
		}

		override protected def appendTo(builder :java.lang.StringBuilder) = {
			parts.foreach(builder.append); builder
		}

		override def toString = {
			val res = new java.lang.StringBuilder
			parts.foreach(res.append)
			res.toString
		}
	}



	private class ReverseChunkList(reversed :List[ChunkedString]) extends ChunkedString {
		override def head = reversed.last.head
		override def last = reversed.head.last

		override def apply(i :Int) :Char = {
			var offset = i
			val chunk = reversed.reverseIterator
			while (chunk.hasNext) {
				val s = chunk.next()
				if (offset < s.length)
					return s.charAt(i)
				else offset -= s.length
			}
			throw new StringIndexOutOfBoundsException(s"""$i-th char of "$this"""")
		}

		override def isEmpty = reversed.isEmpty
		override lazy val length = (0 /: reversed)(_ + _.length)

		override def iterator = reversed.reverseIterator.flatten

		override def +(string :ChunkedString) =
			if (string.isEmpty) this else new ReverseChunkList(string::reversed)

		protected override def appendTo(builder :lang.StringBuilder) = {
			reversed.reverseIterator.foreach(_.appendTo(builder)); builder
		}
	}



	private class ChunkList(parts :List[ChunkedString]) extends ChunkedString {
		override def head = parts.head.head
		override def last = parts.last.last
		override def apply(idx :Int) :Char = {
			@tailrec def rec(i :Int = idx, chunks :List[ChunkedString] = parts) :Char = chunks match {
				case h::t => if (h.length > i) h.charAt(i) else rec(h.length - i, t)
				case _ => throw new StringIndexOutOfBoundsException(s"""$idx-th char of "$this"""")
			}
			rec()
		}

		override def isEmpty = parts.isEmpty
		override lazy val length = (0 /: parts)(_ + _.length)

		override def iterator = parts.iterator.flatten

		override def +:(string :ChunkedString) =
			if (string.isEmpty) this else new ChunkList(string::parts)

		protected override def appendTo(builder :lang.StringBuilder) = {
			parts.foreach(_.appendTo(builder)); builder
		}
	}


}

