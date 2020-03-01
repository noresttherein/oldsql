package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.PrimaryKey.NoPrimaryKeyException


/**
  * @author Marcin Mościcki
  */
trait PrimaryKey[T, PK] extends (T => Option[PK]) {
	def apply(entity :T) :Option[PK]

	def get(entity :T) :PK = apply(entity) match {
		case Some(key) => key
		case None => throw new NoPrimaryKeyException(entity)
	}

	def transient :PK
}



object PrimaryKey {
	class NoPrimaryKeyException(entity :Any) extends RuntimeException("Entity " + entity + " does not have a primary key.")
}
