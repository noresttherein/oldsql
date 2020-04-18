package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.abacus.{Inc, INT}


/**
  * @author Marcin Mościcki
  */
trait Origin extends Any {

	def canEqual(that :Any) :Boolean = that.isInstanceOf[Origin]

}



object Origin {

	sealed trait UniqueOrigin {
		type O <: Origin
	}

	implicit def Unique :UniqueOrigin = new UniqueOrigin { type T = Origin }



	/** A wrapper over an `Int` with a literal type used for indexing on the type level by parameterizing with ##[N]. */
	type @#[N <: INT] = I forSome { type I <: Index[N] }
/*
	class @#[+N <: INT](val n :N) {
		def this()(implicit value :ValueOf[N]) = this(value.value)

		def ++[I >: N <: INT, O <: INT](implicit inc :Inc[I, O]): @#[O] = new @#[O](inc.n)
		def --[M <: INT, I >: N <: INT](implicit inc :Inc[M, I]): @#[M] = new @#[M](inc.m :M)
	}

	object @# {
		@inline def apply[N <: INT: @#]: @#[N] = implicitly[@#[N]]

		@inline def apply[N <: INT](n :N): @#[N] = new @#[N](n)

		implicit def literal[N <: INT :ValueOf]: @#[N] = new @#[N]

		def unique[N <: INT :ValueOf]: @#[_ <: INT] = new @#[N]
	}
*/



	trait Index[I] extends Any with Origin



	final class NextIndex[N, M](val index :Int) extends AnyVal with Index[M] {
		override def toString :String = "_" + index
	}

//	def Index[N <: Int with Singleton, M <: Int with Singleton :ValueOf] :NextIndex[N, M] =
//		new NextIndex[N, M](valueOf[M])
	def Index[N <: Int with Singleton :ValueOf] :Index[N] = new NextIndex[N, N](valueOf[N])

//	type #^

	type _0 = Index[0]
	final val _0: _0 = Index[0]

	type _1 = Index[1]
	final val _1: _1 = Index[1]

	type _2 = Index[2]
	final val _2: _2 = Index[2]

	type _3 = Index[3]
	final val _3: _3 = Index[3]

	type _4 = Index[4]
	final val _4: _4 = Index[4]

	type _5 = Index[5]
	final val _5: _5 = Index[5]

	type _6 = Index[6]
	final val _6: _6 = Index[6]

	type _7 = Index[7]
	final val _7: _7 = Index[7]

	type _8 = Index[8]
	final val _8: _8 = Index[8]

	type _9 = Index[9]
	final val _9: _9 = Index[9]

	type _10 = Index[10]
	final val _10: _10 = Index[10]

	type _11 = Index[11]
	final val _11: _11 = Index[11]

	type _12 = Index[12]
	final val _12: _12 = Index[12]

	type _13 = Index[13]
	final val _13: _13 = Index[13]

	type _14 = Index[14]
	final val _14: _14 = Index[14]

	type _15 = Index[15]
	final val _15: _15 = Index[15]

	type _16 = Index[16]
	final val _16: _16 = Index[16]

	type _17 = Index[17]
	final val _17: _17 = Index[17]

	type _18 = Index[18]
	final val _18: _18 = Index[18]

	type _19 = Index[19]
	final val _19: _19 = Index[19]

	type _20 = Index[20]
	final val _20: _20 = Index[20]

	type _21 = Index[21]
	final val _21: _21 = Index[21]

	type _22 = Index[22]
	final val _22: _22 = Index[22]

}

