package com.adpilot.cortb.clientapi.util


object Return {
	def apply[T](value :T)(block :T=>Unit) :T = {
		block(value)
		value
	}
}
