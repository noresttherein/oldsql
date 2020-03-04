package net.noresttherein.oldsql.model

import net.noresttherein.oldsql.model.PrimaryKey.NoPrimaryKeyException


trait Entity[T, K <: PK[T]] { this :T =>
	val pk :K
}


trait PrimaryKey[T, PK] extends (T => PK) {
	def apply(entity :T) :PK

	def transient :PK

	def isTransient(key :PK) :Boolean

	def isPersistent(key :PK) :Boolean

	def isUnknown(key :PK) :Boolean
}



object PrimaryKey {


	abstract class EntityPK[T <: Entity[T, K], K <: PK[T]] extends PrimaryKey[T, K] {
		override def apply(entity :T) :K = entity.pk

		override def transient :K

		override def isTransient(key :K) :Boolean = key.isTransient

		override def isPersistent(key :K) :Boolean = key.isPersistent

		override def isUnknown(key :K) :Boolean = false
	}


	abstract class OptionKey[T, PK] extends PrimaryKey[T, Option[PK]] {

		def get(entity :T) :PK = apply(entity) getOrElse {
			throw new NoPrimaryKeyException(entity)
		}

		def transient :Option[Nothing] = None

		override def isTransient(key :Option[PK]) :Boolean = key.isEmpty

		override def isPersistent(key :Option[PK]) :Boolean = key.isDefined

		override def isUnknown(key :Option[PK]) :Boolean = false
	}



	class NoPrimaryKeyException(entity :Any) extends RuntimeException("Entity " + entity + " does not have a primary key.")
}
