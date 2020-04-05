package net.noresttherein

import net.noresttherein.oldsql.model.Kin


package object oldsql {
	type ?[+T] = Kin[T]


	private[oldsql] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()
}
