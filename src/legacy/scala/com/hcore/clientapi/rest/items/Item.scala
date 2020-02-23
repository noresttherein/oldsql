package com.hcore.clientapi.rest.items

import com.hcore.clientapi.entities.Model.Id


sealed trait Item[+E]

case class IdItem[+E](id :Id) extends Item[E]

case class PropertyItem[E, T](property :Item.Property[E, T], value :T) extends Item[E] {
//	def lowered = property.tpe.lower(value)
}

case class ValueItem[E](value :E) extends Item[E]

case object AbsentItem extends Item[Nothing]


object Item {
	implicit def itemAsOption[E](item :Item[E]) :Option[E] = item match {
		case ValueItem(value) => Some(value)
		case _ => None	
	}

	def apply[E]() :Item[E] = AbsentItem

	def apply[E](value :E) :Item[E] = ValueItem(value)

	def apply[E](id :Id) :Item[E] = IdItem[E](id)

	def apply[E, T](property :Property[E, T], value :T) :Item[E] = PropertyItem(property, value)
	
//	def apply[E](property :String, value :Any) :Item[E] = PropertyItem[E](property, value)

	
	def meta[E :ItemMeta] :ItemMeta[E] = implicitly[ItemMeta[E]]
	
	type Property[E, T] = ItemMeta.Property[E, T]
}




sealed trait Items[+I]

case class ItemsValues[I](items :Seq[I]) extends Items[I]

case class ItemsByProperty[I, T](property :Item.Property[I, T], value :T) extends Items[I]

case class ItemsByPropertyValues[I, T](property :Item.Property[I, T], values :Seq[T]) extends Items[I]

case object ItemsAbsent extends Items[Nothing]

case object ItemsAll extends Items[Nothing]



object Items {
	def All[I] :Items[I] = ItemsAll

	def apply[I]() :Items[I] = ItemsAbsent

	def apply[I](items :Seq[I]) :Items[I] = ItemsValues(items)

	def apply[I, T](property :Item.Property[I, T], value :T) :Items[I] = ItemsByProperty(property, value)

	def apply[I, T](property :Item.Property[I, T], values :Seq[T]) :Items[I] = ItemsByPropertyValues(property, values)

}




