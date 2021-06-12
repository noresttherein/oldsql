package net.noresttherein




package object oldsql {

//	type ?[+T] = Kin[T]


	private[oldsql] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()

	/** Library version; standard three level versioning, every level having three digits.
	  * Version 1.14.27 would be 1_014_027.
	  */
	final val OldSQLVer = 1L

	final val SharedImplDeprecation = "This class is an implementation artefact introduce to reduce code repetition " +
	                                  "and should not be referenced, as it may disappear without notice. " +
	                                  "Use one of its subclasses instead."

	final val DeprecatedAlways = "initial release"
}
