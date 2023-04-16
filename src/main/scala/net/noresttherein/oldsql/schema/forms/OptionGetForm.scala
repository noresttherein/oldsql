package net.noresttherein.oldsql.schema.forms

import java.sql.JDBCType

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.schema.{ColumnForm, SQLForm}
import net.noresttherein.oldsql.schema.SQLForm.{AbstractOptMappedForm, NullValue}







case class OptionGetForm[T](override val form :SQLForm[Option[T]])
	extends AbstractOptMappedForm[Option[T], T]()(form)
{
	override val nulls = NullValue.NotNull

	protected override def map(s :Option[T]) :Opt[T] = s
	protected override def unmap(t :T) :Opt[Option[T]] = Got(Option(t))

	private[oldsql] override lazy val cachedString :String = form.toString + ".get"
}



class OptionGetColumnForm[T](override val form :ColumnForm[Option[T]])
	extends OptionGetForm[T](form) with ColumnForm[T]
{
	override def sqlType :JDBCType = form.sqlType
}
