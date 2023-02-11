package com.adpilot.cortb.clientapi.golem


import java.sql.ResultSet
import java.util.Date

import com.adpilot.cortb.clientapi.golem.sql.RValue

//import com.adpilot.cortb

trait ColumnType[T] {
	def name = ""
	def inspect = ""

//	def get(column :String)(resultSet :ResultSet) :T
}

object ColumnType {
	def mapped[X, Y :ColumnType](pack :Y=>X, unpack :X=>Y) :ColumnType[X] = new ColumnType[X]{}

	implicit def asRValue[T :ColumnType] :RValue.Type[T] = new RValue.Type[T]{}
}



//object SQLTypes {
//	implicit object DateColumnType extends ColumnType[Date] {
//		def get(column :String)(resultSet :ResultSet) :Date = resultSet.getDate(column)
//	}
////	implicit val DateColumnType = new ColumnType[Date] {}
//	implicit val StringColumnType = new ColumnType[String] {}
//	implicit val IntColumnType = new ColumnType[Int] {}
//	implicit val LongColumnType = new ColumnType[Long] {}
//	implicit val BooleanColumnType = new ColumnType[Boolean] {}
//
//	implicit def OptionColumnType[T :ColumnType] :ColumnType[Option[T]] = new ColumnType[Option[T]]{}
//
//
//}
import ColumnType._
class ColumnDef[T :ColumnType] protected[golem] (protected[golem] val owner :RowDef, val name :String) extends RValue[T] {
	import com.adpilot.cortb.clientapi.golem.sql._
	
	final def columnType = implicitly[ColumnType[T]]

//	def apply[T<:QueryRow.Source]()(implicit row :QueryRow[T]) :this.type = this
//	import ColumnCondition._
//
//	def ===(value :T) : ColumnCondition[T] = Equals(this, value)
//	def <  (value :T)(implicit ord :Ordering[T]) :ColumnCondition[T] = LessThan(this, value)
//	def <= (value :T)(implicit ord :Ordering[T]) :ColumnCondition[T] = LessThanEqual(this, value)
//	def >  (value :T)(implicit ord :Ordering[T]) :ColumnCondition[T] = GreaterThan(this, value)
//	def >= (value :T)(implicit ord :Ordering[T]) :ColumnCondition[T] = GreaterThanEqual(this, value)
//	def like(value :T)(implicit toString : T => String) :ColumnCondition[T] = Like(this, value)

//	import ColumnUpdate._

//	def := (value :T) :ColumnUpdate[T] = SetValue(this, value)

//	def := ()

//	def qName = source + "." + name

//	override def toString = qName

//	def inspect = qName +" :" + myType.name

//	def prefixed(prefix :String) = new ColumnDef[T](source, prefix + name)
}



//sealed abstract class ColumnCondition[T :ColumnType] {
//	val column :ColumnDef[T]
//}
//
//object ColumnCondition {
//	protected[golem] sealed case class Equals[T :ColumnType] protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//
//	protected[golem] sealed case class LessThan[T :ColumnType :Ordering] protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//
//	protected[golem] sealed case class LessThanEqual[T :ColumnType :Ordering]  protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//
//	protected[golem] sealed case class GreaterThan[T :ColumnType :Ordering]  protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//
//	protected[golem] sealed case class GreaterThanEqual[T :ColumnType :Ordering]  protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//
//	protected[golem] sealed case class Like[T <% String :ColumnType]  protected[golem] (column :ColumnDef[T], value :T) extends ColumnCondition[T]
//}


sealed abstract class ColumnUpdate[T :ColumnType] {
	val column :ColumnDef[T]
}

object ColumnUpdate {
	protected[golem] sealed case class SetValue[T :ColumnType](column :ColumnDef[T], value :T) extends ColumnUpdate[T]

	protected[golem] sealed case class UpdateValue[T :ColumnType](column :ColumnDef[T], operation :ColumnDef[T])
}








