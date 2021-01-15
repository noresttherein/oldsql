package net.noresttherein.oldsql

import scala.reflect.runtime.universe.TypeTag



package object model {

	/** Shorthand type for a `GenericKinFactory` returning just `Kin[X]` */
	type KinFactory[K, E, X] = GenericKinFactory[K, E, X, Kin[X]]


	def self[T :TypeTag] :Restrictive[T, T] = Restrictive.Self[T]()

}