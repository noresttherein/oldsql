package net.noresttherein.oldsql.morsels

import scala.annotation.implicitNotFound



/** Basic type-level arithmetic
  * @author Marcin Mościcki
  */
package object abacus {
	/** Int literals which can be used as types and have their values obtained implicitly. */
	type Numeral = Int with Singleton

	/** Attests that `N + 1 == M`  */
	@implicitNotFound("Can't add 1 to ${N}. Either the result is not ${M} or we ran out of integers, sorry. " +
		              "You can always introduce your own as a hack fix.")
	class Inc[M <: Numeral :ValueOf, N <: Numeral :ValueOf] {
		val m :M = valueOf[M]
		val n :N = valueOf[N]
	}

	@implicitNotFound("Can't witness that ${M} + 1 = ${N}, where ${M}, ${N} >= 0: Either there is no equality, " +
	                  "at least one of the values is negative, or we ran out of integers - sorry!.")
	class PositiveInc[M <: Numeral :ValueOf, N <: Numeral :ValueOf] extends Inc[M, N]

	@implicitNotFound("Can't witness that ${M} + 1 = ${N}, where ${M}, ${N} <= 0: Either there is no equality, " +
		              "at least one of the values is positive, or we ran out of integers - sorry!.")
	class NegativeInc[M <: Numeral :ValueOf, N <: Numeral :ValueOf] extends Inc[M, N]

	type Dec[M <: Numeral, N <: Numeral] = Inc[N, M]

	type Positive[N <: Numeral] = PositiveInc[N, _]
	
	type Negative[N <: Numeral] = NegativeInc[N, _]
	
	
	
	object Inc {
		implicit val m_1 :NegativeInc[-1, 0]  = new NegativeInc[-1, 0]
		implicit val m_2 :NegativeInc[-2, -1] = new NegativeInc[-2, -1]
		implicit val m_3 :NegativeInc[-3, -2] = new NegativeInc[-3, -2]
		implicit val m_4 :NegativeInc[-4, -3] = new NegativeInc[-4, -3]
		implicit val m_5 :NegativeInc[-5, -4] = new NegativeInc[-5, -4]
		implicit val m_6 :NegativeInc[-6, -5] = new NegativeInc[-6, -5]
		implicit val m_7 :NegativeInc[-7, -6] = new NegativeInc[-7, -6]
		implicit val m_8 :NegativeInc[-8, -7] = new NegativeInc[-8, -7]
		implicit val m_9 :NegativeInc[-9, -8] = new NegativeInc[-9, -8]
		implicit val m_10 :NegativeInc[-10, -9] = new NegativeInc[-10, -9]
		implicit val m_11 :NegativeInc[-11, -10] = new NegativeInc[-11, -10]
		implicit val m_12 :NegativeInc[-12, -11] = new NegativeInc[-12, -11]
		implicit val m_13 :NegativeInc[-13, -12] = new NegativeInc[-13, -12]
		implicit val m_14 :NegativeInc[-14, -13] = new NegativeInc[-14, -13]
		implicit val m_15 :NegativeInc[-15, -14] = new NegativeInc[-15, -14]
		implicit val m_16 :NegativeInc[-16, -15] = new NegativeInc[-16, -15]
		implicit val m_17 :NegativeInc[-17, -16] = new NegativeInc[-17, -16]
		implicit val m_18 :NegativeInc[-18, -19] = new NegativeInc[-18, -19]
		implicit val m_19 :NegativeInc[-19, -18] = new NegativeInc[-19, -18]
		implicit val m_20 :NegativeInc[-20, -19] = new NegativeInc[-20, -19]
		implicit val m_21 :NegativeInc[-21, -20] = new NegativeInc[-21, -20]
		implicit val m_22 :NegativeInc[-22, -21] = new NegativeInc[-22, -21]

		implicit val _1 :PositiveInc[0, 1]    = new PositiveInc[0, 1]
		implicit val _2 :PositiveInc[1, 2]    = new PositiveInc[1, 2]
		implicit val _3 :PositiveInc[2, 3]    = new PositiveInc[2, 3]
		implicit val _4 :PositiveInc[3, 4]    = new PositiveInc[3, 4]
		implicit val _5 :PositiveInc[4, 5]    = new PositiveInc[4, 5]
		implicit val _6 :PositiveInc[5, 6]    = new PositiveInc[5, 6]
		implicit val _7 :PositiveInc[6, 7]    = new PositiveInc[6, 7]
		implicit val _8 :PositiveInc[7, 8]    = new PositiveInc[7, 8]
		implicit val _9 :PositiveInc[8, 9]    = new PositiveInc[8, 9]
		implicit val _10 :PositiveInc[9, 10]  = new PositiveInc[9, 10]
		implicit val _11 :PositiveInc[10, 11] = new PositiveInc[10, 11]
		implicit val _12 :PositiveInc[11, 12] = new PositiveInc[11, 12]
		implicit val _13 :PositiveInc[12, 13] = new PositiveInc[12, 13]
		implicit val _14 :PositiveInc[13, 14] = new PositiveInc[13, 14]
		implicit val _15 :PositiveInc[14, 15] = new PositiveInc[14, 15]
		implicit val _16 :PositiveInc[15, 16] = new PositiveInc[15, 16]
		implicit val _17 :PositiveInc[16, 17] = new PositiveInc[16, 17]
		implicit val _18 :PositiveInc[17, 18] = new PositiveInc[17, 18]
		implicit val _19 :PositiveInc[18, 19] = new PositiveInc[18, 19]
		implicit val _20 :PositiveInc[19, 20] = new PositiveInc[19, 20]
		implicit val _21 :PositiveInc[20, 21] = new PositiveInc[20, 21]
		implicit val _22 :PositiveInc[21, 22] = new PositiveInc[21, 22]
		implicit val _23 :PositiveInc[22, 23] = new PositiveInc[22, 23]
		implicit val _24 :PositiveInc[23, 24] = new PositiveInc[23, 24]
		implicit val _25 :PositiveInc[24, 25] = new PositiveInc[24, 25]
		implicit val _26 :PositiveInc[25, 26] = new PositiveInc[25, 26]
		implicit val _27 :PositiveInc[26, 27] = new PositiveInc[26, 27]
		implicit val _28 :PositiveInc[27, 28] = new PositiveInc[27, 28]
		implicit val _29 :PositiveInc[28, 29] = new PositiveInc[28, 29]
		implicit val _30 :PositiveInc[29, 30] = new PositiveInc[29, 30]
	}






	/** Attests that `A + B == C`. */
	@implicitNotFound("Can't add ${A} and ${B}. Either the result does not equal ${C} or, more likely, we ran out of numbers." +
	                  "You can introduce your own implicit Sum[A, B, C] as a fix if the match adds up.")
	sealed abstract class Sum[A <: Numeral, B <: Numeral, C <: Numeral] extends ((A, B) => C) {
		def minusFirst(sum :C, first :A) :B
		def minusSecond(sum :C, second :B) :A
	}

	object Sum {
		private[this] val sum = new Sum[Numeral, Numeral, Numeral] {
			override def apply(a :Numeral, b :Numeral) = (a + b).asInstanceOf[Numeral]
			override def minusFirst(sum :Numeral, first :Numeral) = (sum - first).asInstanceOf[Numeral]
			override def minusSecond(sum :Numeral, second :Numeral) = (sum - second).asInstanceOf[Numeral]
		}

		private[abacus] def apply[A <: Numeral, B <: Numeral, C <: Numeral]() :Sum[A, B, C] = sum.asInstanceOf[Sum[A, B, C]]

		implicit def plusZero[A <: Numeral] :Sum[A, 0, A] = Sum[A, 0, A]()

		implicit def positive[A <: Numeral, M <: Numeral, N <: Numeral, S <: Numeral, T <: Numeral]
		                     (implicit decN :PositiveInc[M, N], sum :Sum[A, M, S], incS :Inc[S, T]) :Sum[A, N, T] =
			Sum()

		implicit def negative[A <: Numeral, M <: Numeral, N <: Numeral, S <: Numeral, T <: Numeral]
		                     (implicit incM :NegativeInc[M, N], sum :Sum[A, N, T], decT :Inc[S, T]) :Sum[A, M, S] =
			Sum()

	}



	@implicitNotFound("Cannot prove that ${A} < ${B}.")
	final class LT[A <: Numeral, B <: Numeral] private[abacus]()
	private[this] val lt = new LT[Numeral, Numeral]

	implicit def LT[A <: Numeral, B <: Numeral, N <: Numeral, S <: Numeral]
	               (implicit neg :Sum[B, N, 0], cmp :Sum[A, N, S], lt :Negative[S]) :LT[A, B] =
		this.lt.asInstanceOf[LT[A, B]]



	@implicitNotFound("Cannot prove that ${A} <= ${B}")
	final class LTE[A <: Numeral, B <: Numeral] private[abacus]()
	private[this] val lte = new LTE[Numeral, Numeral]

	implicit def LTE[A <: Numeral, B <: Numeral, N <: Numeral, S <: Numeral]
	                (implicit neg :Sum[B, N, 0], cmp :Sum[A, N, S], lt :NegativeInc[_, S]) :LTE[A, B] =
		lte.asInstanceOf[LTE[A, B]]



}

