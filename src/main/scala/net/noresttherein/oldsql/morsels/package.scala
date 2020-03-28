package net.noresttherein.oldsql

/**
  * @author Marcin Mościcki
  */
package object morsels {

	final class LUB[-X, -Y, +U] private ()

	object LUB {
		implicit def lub[X] :LUB[X, X, X] = new LUB[X, X, X]
	}


}
