package com.hcore.ogre.mapping

import com.hcore.ogre.model.Reference
import com.hcore.ogre.morsels.necromancy.PropertyChain

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



class ForeignTableFilter[S, X] private (filter :TableFilter[Seq[X]], val join :Option[S=>Reference[_]]) extends TableFilter[Seq[X]] {
	val value = filter.value
	val Setter = filter.Setter
	val whereClause = filter.whereClause
}

object ForeignTableFilter {
//	val JoinProperty = ObjectProperty[Reference[Any]](_.join)
	def JoinProperty[X] = PropertyChain[Reference[Any]](_.join).asInstanceOf[PropertyChain[Reference[X], X]]

	def apply[S, X](filter :TableFilter[Seq[X]], join :S=>Reference[_]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, Some(join))

	def apply[S, X](filter :TableFilter[Seq[X]]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, None)

	def apply[S, X](filter :TableFilter[Seq[X]], join :Option[S=>Reference[_]]) :ForeignTableFilter[S, X] =
		new ForeignTableFilter[S, X](filter, join)

}

