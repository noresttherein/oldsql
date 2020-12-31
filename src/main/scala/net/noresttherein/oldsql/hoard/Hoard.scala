package net.noresttherein.oldsql.hoard

import net.noresttherein.oldsql.hoard
import net.noresttherein.oldsql.model.{ComposedOf, Kin, EntityIdentity, Restraint}
import net.noresttherein.oldsql.model.PropertyPath

import scala.collection.immutable




/**
  * @author Marcin Mościcki
  */
trait Hoard { outer =>
	type Pile[T, K] <: hoard.Pile[T] { type PK = K; type Session = outer.Session }

	type Session <: Hoard.Session

	def newSession() :Session

	def inSession[T](block :Session=>T) :T = { //todo: logging
		val session = newSession()
		try {
			block(session)
		} catch {
			case e :Exception => try {
				session.close()
				throw e
			} finally {throw e }
		}
	}


	def apply[T, PK](pk :PK)(implicit pile :Pile[T, PK], session :Session = newSession()) :T

	def get[T, PK](pk :PK)(implicit pile :Pile[T, PK], session :Session = newSession()) :Option[T]

	def load[T, C](kin :Kin[C])(implicit pile :Pile[T, _], composite :C ComposedOf T, session :Session = newSession()) :C

	def limit[T](restraint :Restraint[T], max :Int, fetch :PropertyPath[T, Any]*)
	            (implicit pile :Pile[T, _], session :Session = newSession()) :Seq[T]

	def build[T, C](restraint :Restraint[T], fetch :PropertyPath[T, Any]*)
	               (implicit pile :Pile[T, _], composite :C ComposedOf T, session :Session = newSession()) :C

	def save[T](entity :T)(implicit pile :Pile[T, _], session :Session = newSession()) :T
	def insert[T](entity :T)(implicit pile :Pile[T, _], session :Session = newSession()) :T
	def update[T](entity :T)(implicit pile :Pile[T, _], session :Session = newSession()) :T


	def deleteBy[T, K](pk :K)(implicit pile :Pile[T, K], session :Session) :Unit
	def delete[T](entity :T)(implicit pile :Pile[T, _], session :Session) :Unit
	def delete[T](entities :Iterable[T])(implicit pile :Pile[T, _], session :Session) :Unit
	def delete[T](filter :Restraint[T])(implicit pile :Pile[T, _], session :Session) :Unit
	def delete[T, C](kin :Kin[C])(implicit pile :Pile[T, _], composite :C ComposedOf T, session :Session) :Unit

/*
	trait Pile[T, K] extends hoard.Pile[T] {
		type Session = outer.Session
		type PK = K

		override def newSession() :Session = outer.newSession()
		
		override def get(pk :K)(implicit session :Session) :Option[T] = ???
		
		override def load[C](kin :Kin[C])(implicit composite :C ComposedOf T, session :Session) :C = ???
		
		override def limit(restraint :Restraint[T], max :Int, fetch :PropertyPath[T, Any]*)(implicit session :Session) :immutable.Seq[T] = ???
		
		override def as[C](restraint :Restraint[T], fetch :PropertyPath[T, Any]*)
		                  (implicit composite :C ComposedOf T, session :Session) :C = ???
		
		override def save(entity :T)(implicit session :Session) :T = ???
		override def insert(entity :T)(implicit session :Session) :T = ???
		override def update(entity :T)(implicit session :Session) :T = ???
		
		override protected def deleteBy(pk :K)(implicit session :Session) :Unit = ???
		override protected def delete(entity :T)(implicit session :Session) :Unit = ???
		override protected def delete(entities :Iterable[T])(implicit session :Session) :Unit = ???
		override protected def delete(filter :Restraint[T])(implicit session :Session) :Unit = ???
		override protected def delete[C](kin :Kin[C])(implicit composite :C ComposedOf T, session :Session) :Unit = ???
	}
*/



}






object Hoard {
	trait Session extends AutoCloseable

	class EntityNotFoundException(msg :String, cause :Exception) extends Exception(msg, cause) {
		def this(msg :String) = this(msg, null)
		def this() = this(null, null)
	}

}


