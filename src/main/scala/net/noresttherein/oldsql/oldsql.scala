package net.noresttherein

import net.noresttherein.oldsql.model.Kin


package object oldsql {
	type ?[+T] = Kin[T]
}
