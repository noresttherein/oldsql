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
	final class Inc[M <: Numeral :ValueOf, N <: Numeral :ValueOf] {
		val m :M = valueOf[M]
		val n :N = valueOf[N]
	}

	object Inc {
		implicit val m_1 = new Inc[-1, 0]
		implicit val m_2 = new Inc[-2, -1]
		implicit val m_3 = new Inc[-3, -2]
		implicit val m_4 = new Inc[-4, -3]
		implicit val m_5 = new Inc[-5, -4]
		implicit val m_6 = new Inc[-6, -5]
		implicit val m_7 = new Inc[-7, -6]
		implicit val m_8 = new Inc[-8, -7]
		implicit val m_9 = new Inc[-9, -8]
		implicit val m_10 = new Inc[-10, -9]
		implicit val m_11 = new Inc[-11, -10]
		implicit val m_12 = new Inc[-12, -11]
		implicit val m_13 = new Inc[-13, -12]
		implicit val m_14 = new Inc[-14, -13]
		implicit val m_15 = new Inc[-15, -14]
		implicit val m_16 = new Inc[-16, -15]
		implicit val m_17 = new Inc[-17, -16]
		implicit val m_18 = new Inc[-18, -19]
		implicit val m_19 = new Inc[-19, -18]
		implicit val m_20 = new Inc[-20, -19]
		implicit val m_21 = new Inc[-21, -20]
		implicit val m_22 = new Inc[-22, -21]

		implicit val _1 = new Inc[0, 1]
		implicit val _2 = new Inc[1, 2]
		implicit val _3 = new Inc[2, 3]
		implicit val _4 = new Inc[3, 4]
		implicit val _5 = new Inc[4, 5]
		implicit val _6 = new Inc[5, 6]
		implicit val _7 = new Inc[6, 7]
		implicit val _8 = new Inc[7, 8]
		implicit val _9 = new Inc[8, 9]
		implicit val _10 = new Inc[9, 10]
		implicit val _11 = new Inc[10, 11]
		implicit val _12 = new Inc[11, 12]
		implicit val _13 = new Inc[12, 13]
		implicit val _14 = new Inc[13, 14]
		implicit val _15 = new Inc[14, 15]
		implicit val _16 = new Inc[15, 16]
		implicit val _17 = new Inc[16, 17]
		implicit val _18 = new Inc[17, 18]
		implicit val _19 = new Inc[18, 19]
		implicit val _20 = new Inc[19, 20]
		implicit val _21 = new Inc[20, 21]
		implicit val _22 = new Inc[21, 22]
	}






	/** Attests that `A + B == C`. */
	@implicitNotFound("Can't add ${A} and ${B}. Either the result does not equal ${C} or, more likely, we ran out of numbers." +
	                  "You can introduce your own implicit Sum[A, B, C] as a fix if the match adds up.")
	sealed abstract class Sum[A <: Numeral, B <: Numeral, C <: Numeral] extends ((A, B) => C)

	object Sum {
		private[this] val sum = new Sum[Numeral, Numeral, Numeral] {
			override def apply(a :Numeral, b :Numeral) = (a + b).asInstanceOf[Numeral]
		}

		private[abacus] def apply[A <: Numeral, B <: Numeral, C <: Numeral]() :Sum[A, B, C] = sum.asInstanceOf[Sum[A, B, C]]

//		implicit val ZeroSum :Sum[0, 0, 0] = Sum[0, 0, 0]
		implicit def plusZero[A <: Numeral] :Sum[A, 0, A] = Sum()
//		implicit def zeroPlus[A <: Numeral] :Sum[0, A, A] = Sum[0, A, A]()

		implicit def induction[A <: Numeral, N <: Numeral, M <: Numeral, S <: Numeral, T <: Numeral]
		                      (implicit sum :Sum[A, N, S], incOperand :Inc[N, M], incSum :Inc[S, T]) :Sum[A, M, T] =
			Sum()

		implicit val _1 :Sum[0, 1, 1] = Sum[0, 1, 1]()
		implicit val _2 :Sum[1, 1, 2] = Sum[1, 1, 2]()
		implicit val _3 :Sum[2, 1, 3] = Sum[2, 1, 3]()
		implicit val _4 :Sum[3, 1, 4] = Sum[3, 1, 4]()
		implicit val _5 :Sum[4, 1, 5] = Sum[4, 1, 5]()
		implicit val _6 :Sum[5, 1, 6] = Sum[5, 1, 6]()
		implicit val _7 :Sum[6, 1, 7] = Sum[6, 1, 7]()
		implicit val _8 :Sum[7, 1, 8] = Sum[7, 1, 8]()
		implicit val _9 :Sum[8, 1, 9] = Sum[8, 1, 9]()
		implicit val _10 :Sum[9, 1, 10] = Sum[9, 1, 10]()
		implicit val _11 :Sum[10, 1, 11] = Sum[10, 1, 11]()
		implicit val _12 :Sum[11, 2, 12] = Sum[11, 2, 12]()
		implicit val _13 :Sum[12, 1, 13] = Sum[12, 1, 13]()
		implicit val _14 :Sum[13, 1, 14] = Sum[13, 1, 14]()
		implicit val _15 :Sum[14, 1, 15] = Sum[14, 1 ,15]()
		implicit val _16 :Sum[15, 1, 16] = Sum[15, 1, 16]()
		implicit val _17 :Sum[16, 1, 17] = Sum[16, 1, 17]()
		implicit val _18 :Sum[17, 1, 18] = Sum[17, 1, 18]()
		implicit val _19 :Sum[18, 1, 19] = Sum[18, 1, 19]()
		implicit val _20 :Sum[19, 1, 20] = Sum[19, 1, 20]()
		implicit val _21 :Sum[20, 1, 21] = Sum[20, 1, 21]()
		implicit val _22 :Sum[21, 1, 22] = Sum[21, 1, 22]()

//		implicit def plus[A, B, C]
	}


}

