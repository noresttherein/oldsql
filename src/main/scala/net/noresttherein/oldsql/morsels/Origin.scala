package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.morsels.abacus.{Inc, INT}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms


/**
  * @author Marcin Mościcki
  */
trait Origin extends Any {

	def canEqual(that :Any) :Boolean = that.isInstanceOf[Origin]

}



object Origin {


	/** A type wrapper over an `Int` with a literal type used for indexing mappings on type level
	  * by parameterizing with ##[N]. It is an existential type so that different instances are incompatible with
	  * each other despite potentially sharing the same the underlying index. Values of this type do
	  * not and should not exist but are phantom types to allow safe casting from one to another.
	  */
	type ##[N <: INT] = I forSome { type I <: Rank[N] }

	trait Rank[I] extends Any with Origin

	object Rank {

//		def apply[N <: INT :ValueOf] :Rank[N] = new Rank[N](valueOf[N])

		def of[I <: Rank[_]](implicit value :GetRank[I]) :Int = value.value

		final class GetRank[I <: Rank[_]] private[Rank](val value :Int)

		implicit def GetIndexValue[O, I <: Rank[N], N <: INT](implicit nudge :Conforms[O, I, Rank[N]], value :ValueOf[N]) =
			new GetRank[I](valueOf[N])
	}



	trait Unique {
		type O
	}

	def unique :Unique = new Unique {}
}

