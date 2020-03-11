package net.noresttherein.oldsql.collection



/**
  * @author Marcin Mościcki
  */
sealed trait Chain extends Serializable



object Chain {


	case class ~[+T <: Chain, +H](tail :T, head :H) extends Chain

	sealed class @~ private[Chain]

	case object @~ extends @~



	implicit class expandChain[C <: Chain](private val chain :C) extends AnyVal {
		@inline def ~[H](head :H) :C ~ H = new ~(chain, head)
	}

}
