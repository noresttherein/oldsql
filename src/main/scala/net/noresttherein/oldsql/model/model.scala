package net.noresttherein.oldsql

import scala.reflect.runtime.universe.TypeTag



package object model {

	def self[T :TypeTag] :Restrictive[T, T] = Restrictive.Self[T]()

}