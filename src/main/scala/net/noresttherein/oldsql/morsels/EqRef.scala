package net.noresttherein.oldsql.morsels

import net.noresttherein.oldsql.slang.saferCasting


private[oldsql] final class EqRef[K](val key :K) extends Serializable {
	override def equals(that :Any) :Boolean = that match {
		case other :EqRef[_] => key.asAnyRef eq other.key.asAnyRef
		case _ => false
	}
	override def hashCode :Int = System.identityHashCode(key)
	override def toString :String = key.toString
}
