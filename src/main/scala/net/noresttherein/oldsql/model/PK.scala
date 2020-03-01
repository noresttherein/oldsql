package net.noresttherein.oldsql.model

/**
  * @author Marcin Mościcki
  */
abstract class PK[T] {

	def canEqual(that :Any) :Boolean = that.isInstanceOf[PK[_]]
}


object PK {



	class TransientPK[T] extends PK[T] {
		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TransientPK[_]]

		override def equals(that :Any) :Boolean = that match {
			case ref :AnyRef => ref eq this
			case _ => false
		}

		override def hashCode :Int = System.identityHashCode(this)

		override def toString :String = "PK#" + hashCode.toHexString
	}



}