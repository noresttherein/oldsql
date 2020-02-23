package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.entities.meta.Reference.One
import com.adpilot.cortb.clientapi.util.ObjectProperty

import scala.slick.jdbc.SetParameter

trait TableFilter[T] {
	val value :T
	val Setter :SetParameter[T]
	val whereClause :String
//	def whereClause(alias :String) :String = whereClause
}

object TableFilter {

	def apply[T :SetParameter](condition :String, v :T) :TableFilter[T] = new TableFilter[T] {
		val value = v
		val Setter = implicitly[SetParameter[T]]
		val whereClause = condition
	}

//	def apply[T :SetParameter](condition :String=>String, v :T) :TableFilter[T] = new TableFilter[T] {
//		val value = v
//		val Setter = implicitly[SetParameter[T]]
//		val whereClause = condition
//
//	}

}



class ForeignTableFilter[S, X] private (filter :TableFilter[Seq[X]], val join :Option[S=>One[_]]) extends TableFilter[Seq[X]] {
	val value = filter.value
	val Setter = filter.Setter
	val whereClause = filter.whereClause
}

object ForeignTableFilter {
//	val JoinProperty = ObjectProperty[One[Any]](_.join)
	def JoinProperty[X] = ObjectProperty[One[Any]](_.join).asInstanceOf[ObjectProperty[One[X], X]]

	def apply[S, X](filter :TableFilter[Seq[X]], join :S=>One[_]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, Some(join))

	def apply[S, X](filter :TableFilter[Seq[X]]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, None)

	def apply[S, X](filter :TableFilter[Seq[X]], join :Option[S=>One[_]]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, join)

}

