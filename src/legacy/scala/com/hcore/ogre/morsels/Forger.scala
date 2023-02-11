package com.hcore.ogre.morsels

import com.hcore.ogre.morsels.Forger.Forgerable

import scala.annotation.tailrec


//implicits

class Forger(val primeLimit :Long, val primeIncrease :Long=1000) {
	def this() = this(Forger.DefaultTotalLimit, Forger.DefaultRangeFactor)

	private var bytePrimes = Forger.BytePrimes
	private var shortPrimes = Forger.ShortPrimes
	private var longPrimes = Forger.LongPrimes
//	private var usedPrimes = Forger.FirstPrimes :Seq[Long]
	private var sieved = Forger.PrecomputeLimit

	def nextByte :Option[Byte] = bytePrimes match {
		case Seq() => None
		case Seq(head, rest @_*) => bytePrimes=rest; Some(head.toByte)
	}

	def nextShort :Option[Short] = shortPrimes match {
		case Seq() => nextByte.map(_.toShort)
		case Seq(head, rest @_*) => shortPrimes=rest; Some(head.toShort)
	}


	def nextInt :Option[Int] = longPrimes match {
		case Seq(head, tail @_*) if head <= Int.MaxValue.toLong =>
			longPrimes = tail
			Some(head.toInt)
//		case Seq() if sieved <= Int.MaxValue.toLong =>
//			nextLong match {
//				case Some(x) if x<=Int.MaxValue.toLong => Some(x.toInt)
//				case Some(x) => longPrimes = x +: longPrimes; None
//				case None => None
//			}
		case _ => nextShort.map(_.toInt) orElse nextByte.map(_.toInt)
	}


	def nextLong :Option[Long] = longPrimes match {
		case Seq() => nextShort.map(_.toLong) orElse nextByte.map(_.toLong)
//			Seq(sieved*primeIncrease, primeLimit).min.providing(_ > sieved).flatMap { end =>
//				longPrimes = nextPrimes(usedPrimes, sieved+1, end)
//				usedPrimes = longPrimes ++: usedPrimes
//				sieved = end
//				nextLong
//			}
		case Seq(head, tail @_*) =>
			longPrimes = tail
			Some(head)
	}


	def same(x :Any, y :Any) :Boolean = (x, y) match {
		case (r :AnyRef, s :AnyRef) => r eq s
		case _ => x==y
	}



	@inline
	final def next[X :Forgerable] :Option[X] = implicitly[Forgerable[X]].next(this)
}



object Forger {
	class Forgerable[X](private[Forger] val forge :Forger=>Option[X]) extends AnyVal{
		@inline
		final def next(forger :Forger) :Option[X] = forge(forger) 
	}

	object Forgerable {
//		class ForgerableAnyRef[T<:AnyRef](val contructor :() => T) extends Forgerable[T](nj)
		implicit val FalseByte = new Forgerable(_.nextByte)
		implicit val FalseShort = new Forgerable(_.nextShort)
		implicit val FalseInt = new Forgerable(_.nextInt)
		implicit val FalseLong = new Forgerable(_.nextLong)

	}

	private[Forger] val DefaultRangeFactor = 100
	private[Forger] val DefaultTotalLimit = Int.MaxValue.toLong
	private[Forger] val PrecomputeLimit = 100000L //Short.MaxValue.toLong * DefaultRangeFactor
//	private[Forger] val BytePrimes = 1L +: nextPrimes(Seq(), 2, Byte.MaxValue)
//	private[Forger] val ShortPrimes = nextPrimes(BytePrimes.tail, 256, Short.MaxValue)
//	private[Forger] val LongPrimes = nextPrimes(ShortPrimes ++: BytePrimes.tail, Short.MaxValue.toLong+1, PrecomputeLimit)
//	private[Forger] val FirstPrimes = LongPrimes ++: ShortPrimes ++: BytePrimes.tail
	val primes = 1L+:nextPrimes(Seq(), 2, PrecomputeLimit)
	val BytePrimes = primes.takeWhile(_<=Byte.MaxValue.toLong)
	val ShortPrimes = primes.dropWhile(_<=Byte.MaxValue.toLong).takeWhile(_<=Short.MaxValue.toLong)
	val LongPrimes = primes.dropWhile(_<=Short.MaxValue.toLong)



	private[Forger] def nextPrimes(used :Seq[Long], start :Long, end :Long) :Seq[Long] = {
		val range = (start until end).toSeq
//		System.err.println(s"searching $start - $end: ${range.size} * ${used.size} = ${range.size*used.size}")
		val prefiltered = (range /: used)((left, prime) => left.filterNot(_ % prime == 0))
//		System.err.println(s"filter proper ${prefiltered.size} * ${used.size}")
		System.err.println(s"")
		@tailrec
		def eratosthenes(candidates :Seq[Long]=prefiltered, acc :Seq[Long]=Seq()) :Seq[Long] = candidates match {
			case Seq() => acc.reverse
			case Seq(head, tail @_*) =>
//				System.err.println(s"found $head")
				eratosthenes(tail.filterNot(_ % head == 0), head+:acc)
		}
		eratosthenes()
	}
	
	
}
