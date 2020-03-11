package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.schema.SQLForm.{MappedSQLForm, Tuple2Form}
import net.noresttherein.oldsql.schema.SQLReadForm.{AbstractTuple2ReadForm, MappedSQLReadForm, SeqReadForm}
import net.noresttherein.oldsql.schema.SQLWriteForm.{AbstractTuple2WriteForm, EmptyWriteForm, MappedSQLWriteForm, SeqWriteForm}
import net.noresttherein.oldsql.slang._


trait SQLForm[T] extends SQLReadForm[T] with SQLWriteForm[T] {
	def apply(position :Int)(res :ResultSet) :T

	def opt(position :Int)(res :ResultSet) :Option[T]

	def literal(value :T) :String

	def nullValue :T
	//	def columnCount :Int


	def as[X](map :T => X)(unmap :X => T) :SQLForm[X] =
		MappedSQLForm[X, T](t => Option(map(t)), x => Option(unmap(x)), map(nullValue))(this)

	def as[X](map :T => X, nullValue :X)(unmap :X => T) :SQLForm[X] =
		MappedSQLForm[X, T](t=>Option(map(t)), x=>Option(unmap(x)), nullValue)(this)

	def asOpt[X >: Null](map :T => Option[X])(unmap :X => Option[T]) :SQLForm[X] =
		MappedSQLForm[X, T](map, unmap, null)(this)

	def asOpt[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :SQLForm[X] =
		MappedSQLForm[X, T](map, unmap, nullValue)(this)

	override def asOpt :SQLForm[Option[T]] = SQLForm.OptionType(this)


	def *[O](other :SQLForm[O]) :SQLForm[(T, O)] = new Tuple2Form()(this, other)

	override def toString :String = this.innerClassName

	def compatible(other :SQLForm[_]) :Boolean = this == other
}





trait BaseColumnForm {
	def sqlType :Int
}


trait ColumnForm[T] extends SQLForm[T] with ColumnReadForm[T] with ColumnWriteForm[T] {

	override def as[X](map :T => X)(unmap :X => T) :ColumnForm[X] =
		MappedSQLForm.column[X, T](t => Option(map(t)), x => Option(unmap(x)), map(nullValue))(this)

	override def as[X](map :T => X, nullValue :X)(unmap :X => T) :ColumnForm[X] =
		MappedSQLForm.column[X, T](t => Option(map(t)), x => Option(unmap(x)), nullValue)(this)

	override def asOpt[X >: Null](map :T => Option[X])(unmap :X => Option[T]) :ColumnForm[X] =
		MappedSQLForm.column[X, T](map, unmap, null)(this)

	override def asOpt[X](map :T => Option[X], nullValue :X)(unmap :X => Option[T]) :ColumnForm[X] =
		MappedSQLForm.column[X, T](map, unmap, nullValue)(this)



	override def compatible(other: SQLForm[_]): Boolean = other match {
		case a :ColumnForm[_] => a.sqlType == sqlType
		case _ => false
	}
}






trait RecordForm[T] extends SQLForm[T]






object SQLForm extends JDBCTypes {

	def combine[T](read :SQLReadForm[T], write :SQLWriteForm[T]) :SQLForm[T] =
		new CombinedForm[T](read, write)

	def combine[T](read :ColumnReadForm[T], write :ColumnWriteForm[T]) :ColumnForm[T] =
		if (read.sqlType != write.sqlType)
			throw new IllegalArgumentException(
				s"Can't combine column forms $read and $write with different underlying sql types: ${read.sqlType} != ${write.sqlType}."
			)
		else
			new CombinedForm[T](read, write) with ColumnForm[T] {
				override val sqlType = read.asInstanceOf[ColumnReadForm[T]].sqlType
			}



	implicit def optionForm[T :SQLForm] :SQLForm[Option[T]] = new OptionForm[T]
	implicit def tuple2Form[T1 :SQLForm, T2 :SQLForm] :SQLForm[(T1, T2)] = new Tuple2Form[T1, T2]



	class NullValue[+T](val value :T) {
		override def toString :String = value + ":Null"
	}

	object NullValue {
		def apply[T](value :T) :NullValue[T] = new NullValue[T](value)

		def apply[T :NullValue] :NullValue[T] = implicitly[NullValue[T]]

		def Null[T :NullValue] :T = implicitly[NullValue[T]].value

		implicit def nullable[T>:Null] :NullValue[T] = new NullValue[T](null)
		implicit val int = new NullValue(0)
		implicit val long = new NullValue(0L)
		implicit val short = new NullValue[Short](0)
		implicit val byte = new NullValue[Byte](0)
		implicit val boolean = new NullValue[Boolean](false)
		implicit val char = new NullValue[Char](0)
		implicit val float = new NullValue[Float](0)
		implicit val double = new NullValue[Double](0)
		implicit val unit = new NullValue(())

	}






	trait NullableForm[T >: Null] extends SQLForm[T] {
		def nullValue :Null = null
		override def nullLiteral :String = "null"
	}



	trait NonLiteralForm[T] extends SQLForm[T] {
		override def literal(value: T): String = throw new UnsupportedOperationException(getClass.getName+".literal")
		override def nullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".nullLiteral")
		override def inlineLiteral(value: T): String = throw new UnsupportedOperationException(getClass.getName+".inlineLiteral")
		override def inlineNullLiteral :String = throw new UnsupportedOperationException(getClass.getName+".inlineNullLiteral")
	}



	abstract class AbstractEmptyForm[T] extends SQLForm[T] with EmptyWriteForm[T] {
		override def apply(position: Int)(res: ResultSet): T = nullValue

		def apply(column :String)(res :ResultSet) :T = nullValue

		override def opt(position :Int)(rs :ResultSet) :Option[T] = None

		override def readColumns = 0
	}


	class EmptyForm[T](nullExpr : =>T) extends AbstractEmptyForm[T] with NonLiteralForm[T] {
		def nullValue :T = nullExpr
	}

	object EmptyForm {
		def apply[T](nullExpr : =>T) :EmptyForm[T] = new EmptyForm[T](nullExpr)
		def unapply[T](form :SQLForm[T]) :Boolean = form.isInstanceOf[EmptyForm[_]]
	}






	implicit case object UnitForm extends EmptyForm[Unit](()) {
		override def toString = "UNIT"
	}

	case object NothingForm extends EmptyForm[Nothing](throw new UnsupportedOperationException("SQLType.NothingType")) {
		override def toString = "NOTHING"
	}

	case object NoneForm extends EmptyForm[Option[Nothing]](None) {
		override def toString = "NONE"
	}



	class UnknownForm[T] extends EmptyForm[T](throw new UnsupportedOperationException("SQLType.UnknownType")) {
		override def toString = "UNKNOWN"
	}

	object Unknown {
		def apply[T]() :UnknownForm[T] = unknown.asInstanceOf[UnknownForm[T]]
		def apply[T](form :SQLForm[T]) :Boolean = form.isInstanceOf[UnknownForm[_]]

		def unapply[T](form :SQLForm[T]) :Boolean = apply(form)

		private val unknown = new UnknownForm[Any]
	}






	case class CombinedForm[T](read :SQLReadForm[T], write :SQLWriteForm[T]) extends SQLForm[T] {

		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			write.set(position)(statement, value)

		override def setNull(position :Int)(statement :PreparedStatement) :Unit =
			write.setNull(position)(statement)

		override def opt(position: Int)(res: ResultSet): Option[T] = read.opt(position)(res)

		override def nullValue: T = read.nullValue

		override def literal(value: T): String = write.literal(value)
		override def nullLiteral: String = write.nullLiteral

		override def inlineLiteral(value: T): String = write.inlineLiteral(value)
		override def inlineNullLiteral: String = write.inlineNullLiteral

		override def writtenColumns: Int = write.writtenColumns

		override def readColumns: Int = read.readColumns

		override def toString = s"($read & $write)"
	}



	object MappedSQLForm {

		def apply[T, S :SQLForm](map :S=>Option[T], unmap :T=>Option[S], nullValue :T) :MappedSQLForm[T, S] = SQLForm[S] match {
			case t :ColumnForm[_] =>
				column(map, unmap, nullValue)(t.asInstanceOf[ColumnForm[S]])
			case t :RecordForm[_] =>
				new MappedSQLForm[T, S](map, unmap, nullValue) with RecordForm[T]
			case _ =>
				new MappedSQLForm[T, S](map, unmap, nullValue)

		}

		def column[T, S :ColumnForm](map :S=>Option[T], unmap :T=>Option[S], nullValue :T) :MappedColumnForm[T, S] =
			new MappedColumnForm(map, unmap, nullValue)

	}



	class MappedSQLForm[T, S](map :S => Option[T], val unmap :T => Option[S], nullValue :T)(implicit override val source :SQLForm[S])
		extends MappedSQLReadForm[T, S](map, nullValue) with MappedSQLWriteForm[T, S] with SQLForm[T]
	{
		override def toString = s"<=$source=>"
	}



	class MappedColumnForm[T, S](read :S => Option[T], write :T => Option[S], nullValue :T)(implicit override val source :ColumnForm[S])
		extends MappedSQLForm[T, S](read, write, nullValue) with ColumnForm[T]
	{
		override def sqlType: Int = source.sqlType

		override def apply(column: String)(res: ResultSet): T =
			source.opt(column)(res).flatMap(map) getOrElse this.nullValue

		override def opt(position: Int)(res: ResultSet): Option[T] =
			source.opt(position)(res).flatMap(map)

		override def writtenColumns :Int = source.writtenColumns
		override def readColumns :Int = source.readColumns

	}









	class OptionForm[T](implicit form :SQLForm[T]) extends SQLForm[Option[T]] {
		override def readColumns :Int = form.readColumns
		override def writtenColumns :Int = form.writtenColumns


		override def set(position :Int)(statement :PreparedStatement, value :Option[T]) :Unit = value match {
			case Some(x) => form.set(position)(statement, x)
			case _ => form.setNull(position)(statement)
		}
		override def setNull(position :Int)(statement :PreparedStatement) :Unit = form.setNull(position)(statement)

		override def opt(position :Int)(res :ResultSet) :Option[Option[T]] = Some(form.opt(position)(res))
		override def apply(position :Int)(res :ResultSet) :Option[T] = form.opt(position)(res)

		override def literal(value :Option[T]) :String = value match {
			case Some(x) => form.literal(x)
			case _ => form.nullLiteral
		}
		override def inlineLiteral(value :Option[T]) :String = value match {
			case Some(x) => form.inlineLiteral(x)
			case _ => form.inlineNullLiteral
		}

		override def nullValue :Option[T] = None

		override def nullLiteral :String = form.nullLiteral
		override def inlineNullLiteral :String = form.inlineNullLiteral

		private def some :SQLForm[T] = form

		override def equals(that :Any) :Boolean = that match {
			case opt :OptionForm[_] => opt.some == form
			case _ => false
		}

		override def hashCode :Int = form.hashCode

		override def toString :String = "Option[" + form + "]"
	}



	class Tuple2Form[L, R](implicit val _1 :SQLForm[L], implicit val _2 :SQLForm[R])
		extends AbstractTuple2ReadForm[L, R] with AbstractTuple2WriteForm[L, R] with SQLForm[(L, R)]
	{
		override def equals(that :Any) :Boolean = that match {
			case t :Tuple2Form[_, _] => (t eq this) || t._1 == _1 && t._2 == _2
			case _ => false
		}

		override def hashCode :Int = (_1, _2).hashCode

		override def toString = s"(${_1},${_2})"
	}



	case class SeqForm[T](forms :Seq[SQLForm[T]]) extends SQLForm[Seq[T]] with SeqWriteForm[T] with SeqReadForm[T] {
		override def toString :String = forms.mkString("Seq(",",",")")
	}



/*
	import shapeless.::



	implicit case object HNilForm extends AbstractEmptyForm[HNil] with AbstractHListWriteForm[HNil] {
		def ::[X](f :SQLForm[X]) :HListForm[X, HNil] = new HListForm(f, this)

		override def nullValue: HNil = HNil
		override def nullLiteral: String = "()"
		override def literal(value :HNil) = "()"

		override def inlineLiteral(value: HNil): String = ""
		override def inlineNullLiteral: String = ""

		override protected[sql] def elementsLiteral(sb: StringBuilder, value :HNil): StringBuilder = sb
		override def toString = "HNIL"
	}



	case class HListForm[H, T>:Null<:HList](head :SQLForm[H], tail :SQLForm[T] with AbstractHListWriteForm[T])
		extends SQLForm[H::T] with HListReadForm[H, T] with HListWriteForm[H, T]
	{
		override def toString = s"$head::" + tail.toString
	}
*/




}


