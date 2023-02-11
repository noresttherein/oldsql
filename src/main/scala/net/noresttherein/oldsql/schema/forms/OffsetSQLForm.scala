package net.noresttherein.oldsql.schema.forms

import java.sql.{CallableStatement, JDBCType, PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.schema.{ColumnForm, ColumnReadForm, ColumnWriteForm, SQLForm, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormProxy
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormProxy
import net.noresttherein.oldsql.schema.forms.UnspecifiedForm.{BaseFormAdapter, UnspecifiedColumnFormAdapter}






//consider: not making it a proxy to make it incompatible with the underlying form
private[schema] trait OffsetReadForm[+T] extends BaseFormAdapter[SQLReadForm[T]] with ReadFormProxy[T] {
	protected def offset :Int
	override def isUniversal = false
	override def opt(res :ResultSet, position :Int) :Opt[T] = form.opt(res, offset + position)
	override def apply(res :ResultSet, position :Int) :T = form(res, offset + position)
	override def register(call :CallableStatement, position :Int) :Unit = form.register(call, offset + position)
	override def >>(shift :Int) :SQLReadForm[T] =
		if (shift == 0) this else form >> offset + shift

	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Column types of offset form " + this + " are not well defined.")

	override def comparable(other :SQLReadForm[_]) :Boolean =
		this == other || columnCount == 0 && other.columnCount == 0

	override def canEqual(that :Any) = that match {
		case other :OffsetReadForm[_] => offset == other.offset && getClass == other.getClass
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + offset.hashCode

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) text.get + ">"
		else if (offset > 0) form.toString + ">>" + offset
		else form.toString + "<<" + -offset
}


private[schema] class OffsetColumnReadForm[+T](protected override val form :ColumnReadForm[T],
                                               protected override val offset :Int)
	extends BaseFormAdapter(form) with OffsetReadForm[T] with ColumnReadForm[T] with UnspecifiedColumnFormAdapter
{
	override def >>(shift :Int) :ColumnReadForm[T] =
		if (shift == 0) this else form >> offset + shift
}





private[schema] trait OffsetWriteForm[-T] extends BaseFormAdapter[SQLWriteForm[T]] with WriteFormProxy[T] {
	protected def offset :Int
	override def isUniversal = false

	override def columnTypes :Seq[JDBCType] =
		throw new UnsupportedOperationException("Column types of offset form " + this + " are not well defined.")

	override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
		form.set(statement, offset + position, value)

	override def setNull(statement :PreparedStatement, position :Int) :Unit =
		form.setNull(statement, offset + position)

	override def >>(shift :Int) :SQLWriteForm[T] =
		if (shift == 0) this else form >> offset + shift

	override def canEqual(that :Any) :Boolean = that match {
		case other :OffsetWriteForm[_] => offset == other.offset && getClass == other.getClass
		case _ => false
	}
	override def hashCode :Int = form.hashCode * 31 + offset.hashCode

	private[schema] override lazy val cachedString :String =
		if (text.isDefined) "<" + text.get
		else if (offset >= 0) form.toString + ">>" + offset
		else form.toString + "<<" + -offset
}


private[schema] class OffsetColumnWriteForm[-T](protected override val form :ColumnWriteForm[T],
                                                protected override val offset :Int)
	extends BaseFormAdapter(form) with OffsetWriteForm[T] with SingletonColumnWriteForm[T]
	   with UnspecifiedColumnFormAdapter
{
	override def >>(shift :Int) :ColumnWriteForm[T] =
		if (shift == 0) this else form >> offset + shift
}






private[schema] trait OffsetForm[T]
	extends BaseFormAdapter[SQLForm[T]] with OffsetReadForm[T] with OffsetWriteForm[T] with SQLForm[T]
{
	override def >>(shift :Int) :SQLForm[T] = if (shift == 0) this else form >> offset + shift

	private[schema] override lazy val cachedString :String =
		if (offset >= 0) form.toString + ">>" + offset
		else form.toString + "<<" + -offset
}

private[schema] class OffsetSQLForm[T](override val form :SQLForm[T], override val offset :Int)
	extends BaseFormAdapter(form) with OffsetForm[T]

private[schema] class OffsetColumnForm[T](override val form :ColumnForm[T], override val offset :Int)
	extends BaseFormAdapter(form) with UnspecifiedColumnFormAdapter with OffsetForm[T]
	   with ColumnForm[T] with SingletonColumnWriteForm[T]
{
	override def >>(shift :Int) :ColumnForm[T] = if (shift == 0) this else form >> offset + shift
}
