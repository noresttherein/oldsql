package com.hcore.ogre.morsels

import org.scalatest.{FlatSpec, Matchers}


class ForgerSpec extends  FlatSpec with Matchers {

	"Forger" should "produce different values for different number types" in {
		val forger = new Forger
		var taken=0
		while (taken+1<=Byte.MaxValue) {
			val (b, s, i, l) = (forger.next[Byte], forger.next[Short], forger.next[Int], forger.next[Long])

			(b.map(_.toLong).toSet ++ s.map(_.toLong) ++ i.map(_.toLong) ++ l.map(_.toLong)).size should equal(b.size+s.size+i.size+l.size)
			taken+=4
		}
	}

//	"Forger" should "produce correct primes" in {
//		System.err.println(Forger.BytePrimes.fullSize)
//		System.err.println(Forger.ShortPrimes.fullSize)
//		System.err.println(Forger.LongPrimes.fullSize)
//		System.err.println(Forger.primes.fullSize)
//		System.err.println(Forger.primes.last +" / "+Int.MaxValue)
//
//	}
}
