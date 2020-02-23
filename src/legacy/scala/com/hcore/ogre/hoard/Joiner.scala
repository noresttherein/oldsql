package com.hcore.ogre.hoard

import com.hcore.ogre.model.Restriction.Restrictive


class Joiner {

}



object Joiner {
	def apply[K, T, V](key :Restrictive[K, V], references :Restrictive[T, V]) = ???
}
