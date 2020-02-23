package com.adpilot.cortb.clientapi.prototype.repository.entities.meta

import com.adpilot.cortb.clientapi.prototype.repository.entities.Model.{HasId, Id}

//type Ref[T] = ForeignKey[Long, T]



/*


sealed trait EntityReference[+Id, +Entity] {
	def keyOpt :Option[Id]
	def toOpt :Option[Entity]
}

object EntityReference {
	implicit def asOption[I, E](ref :EntityReference[I, E]) :Option[E] = ref.toOpt

	type Ref[T] = EntityReference[Id, T]

//	def apply[K](id :K) :Empty[K] = Empty(id)
	def apply(id :Id) :Empty[Id] = Empty(id)

	def apply[E<:HasId](e :E) :EntityReference[Id, E] = e.id match {
		case Some(id) => Full(id, e)
		case None => Transient(e)
	}
}



sealed trait ForeignKey[+Id, +Entity] extends EntityReference[Id, Entity] {
	val key :Id
	def keyOpt = Some(key)
//	def toOpt :Option[Entity]
}

object ForeignKey {
//	implicit def asOption[I, E](key :ForeignKey[I, E]) :Option[E] = key.toOpt
	
//	type Ref[T] = ForeignKey[Id, T]
	type Ref[T] = EntityReference.Ref[T]

	def apply[K](id :K) :Empty[K] = Empty(id)
}

case class Empty[+Id](key :Id) extends ForeignKey[Id, Nothing] {
	def toOpt :Option[Nothing] = None
}

case class Full[+Id, +Entity](key :Id, value :Entity) extends ForeignKey[Id, Entity] {
	def toOpt :Some[Entity] = Some(value)
}

case class Transient[+Entity](value :Entity) extends EntityReference[Nothing, Entity] {
	def toOpt :Some[Entity] = Some(value)
	def keyOpt = None
}

object Full {
	implicit def valueOf[I,T](key :Full[I, T]) :T = key.value
}
*/






/*


sealed trait PrimaryKey {
	def toOpt :Option[Long]
}

object PrimaryKey {
	implicit def apply(maybe :Option[Long]) = maybe match {
		case None => Unknown
		case Some(x) => Id(x)
	}

	def apply(id :Long) = Id(id)

	implicit def asOption(key :PrimaryKey) = key.toOpt
}

case object Unknown extends PrimaryKey {
	override def toOpt: Option[Nothing] = None
}



*/




/*

final case class Id(value :Long)  {
	def toOpt: Some[Long] = Some(value)
}


trait HasId {
	val id :Option[Id]
}
*/

//abstract class EnumType[+T](protected val value :T) {
//	override def toString = "<"+value+">"
//	
//	def unapply()
//}
//
//abstract class Enums[T, E<:EnumType[T]](create :T => E) {
//	protected def value(v :T) :E = create(v)
//}