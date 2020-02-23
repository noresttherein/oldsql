package com.adpilot.cortb.clientapi.util


class Repeat(val count :Int) extends AnyVal {
	def times(block : =>Unit): Unit =
		for (i<- 0 until count) block

}

object Repeat {
	implicit def repeat(times :Int) = new Repeat(times)
}