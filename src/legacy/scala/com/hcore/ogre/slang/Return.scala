package com.hcore.ogre.slang

object Return {
	def apply[T](value :T)(block :T=>Unit) :T = {
		block(value)
		value
	}
}
