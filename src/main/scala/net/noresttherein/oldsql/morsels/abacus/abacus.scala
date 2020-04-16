package net.noresttherein.oldsql.morsels

/**
  * @author Marcin Mościcki
  */
package object abacus {
	type INT = Int with Singleton

	final class Plus[N <: INT :ValueOf, M <: INT :ValueOf] {
		val n = valueOf[N]
		val m = valueOf[M]
	}

	object Plus {
		implicit val m_1 = new Plus[-1, 0]
		implicit val m_2 = new Plus[-2, -1]
		implicit val m_3 = new Plus[-3, -2]
		implicit val m_4 = new Plus[-4, -3]
		implicit val m_5 = new Plus[-5, -4]
		implicit val m_6 = new Plus[-6, -5]
		implicit val m_7 = new Plus[-7, -6]
		implicit val m_8 = new Plus[-8, -7]
		implicit val m_9 = new Plus[-9, -8]
		implicit val m_10 = new Plus[-10, -9]
		implicit val m_11 = new Plus[-11, -10]
		implicit val m_12 = new Plus[-12, -11]
		implicit val m_13 = new Plus[-13, -12]
		implicit val m_14 = new Plus[-14, -13]
		implicit val m_15 = new Plus[-15, -14]
		implicit val m_16 = new Plus[-16, -15]
		implicit val m_17 = new Plus[-17, -16]
		implicit val m_18 = new Plus[-18, -19]
		implicit val m_19 = new Plus[-19, -18]
		implicit val m_20 = new Plus[-20, -19]
		implicit val m_21 = new Plus[-21, -20]
		implicit val m_22 = new Plus[-22, -21]

		implicit val _1 = new Plus[0, 1]
		implicit val _2 = new Plus[1, 2]
		implicit val _3 = new Plus[2, 3]
		implicit val _4 = new Plus[3, 4]
		implicit val _5 = new Plus[4, 5]
		implicit val _6 = new Plus[5, 6]
		implicit val _7 = new Plus[6, 7]
		implicit val _8 = new Plus[7, 8]
		implicit val _9 = new Plus[8, 9]
		implicit val _10 = new Plus[9, 10]
		implicit val _11 = new Plus[10, 11]
		implicit val _12 = new Plus[11, 12]
		implicit val _13 = new Plus[12, 13]
		implicit val _14 = new Plus[13, 14]
		implicit val _15 = new Plus[14, 15]
		implicit val _16 = new Plus[15, 16]
		implicit val _17 = new Plus[16, 17]
		implicit val _18 = new Plus[17, 18]
		implicit val _19 = new Plus[18, 19]
		implicit val _20 = new Plus[19, 20]
		implicit val _21 = new Plus[20, 21]
		implicit val _22 = new Plus[21, 22]
	}

/*
	sealed abstract class Sum[A <: N, B <: N, C <: N] extends ((A, B) => C)

	object Sum {
		private[this] val sum = new Sum[N, N, N] {
			override def apply(a :N, b :N) = (a + b).asInstanceOf[N]
		}

		private[abacus] def apply[A <: N, B <: N, C <: N]() :Sum[A, B, C] = sum.asInstanceOf[Sum[A, B, C]]

		implicit def plusZero[A <: N] :Sum[A, 0, A] = Sum[A, 0, A]()

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
*/

}

