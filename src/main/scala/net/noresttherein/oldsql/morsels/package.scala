package net.noresttherein.oldsql

/**
  * @author Marcin Mościcki
  */
package object morsels {

	final class LUB[-X, -Y, +U] private ()

	object LUB {
		implicit def lub[X] :LUB[X, X, X] = new LUB[X, X, X]
	}


	/** Mixin trait implementing equality as equality of classes of the compared objects.
	  * Extended by singleton-like classes which prefer to avoid the additional overhead of `object` definitions.
	  * The main difference lies in serialization, as even if only one instance is created directly by the application,
	  * serialization and deserialization can introduce others.
	  */
	private[oldsql] trait Contextless {
		override def equals(that :Any) = that.getClass == getClass
		override def hashCode :Int = getClass.hashCode
	}

}
