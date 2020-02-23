package com.hcore.ogre.mapping

import com.hcore.ogre.sql.SQLForm

/*

trait ColumnDefinition[T] {
	def form :SQLForm[T]
	def name :String
}


object ColumnDefinition {

	def apply[T :SQLForm](name :String) :ColumnDefinition[T] =
		new ColumnDef[T](name, SQLForm[T])

	case class ColumnDef[T](name :String, form :SQLForm[T]) extends ColumnDefinition[T] {
		override def toString = s"$name:$form"
	}
}
*/
