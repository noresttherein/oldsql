package net.noresttherein.oldsql.schema.forms

import java.lang.{Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{ByteArrayInputStream, InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Date, JDBCType, NClob, PreparedStatement, Ref, ResultSet, RowId, SQLXML, Time, Timestamp, Types}
import java.sql.JDBCType._
import java.time.format.DateTimeFormatter

import net.noresttherein.oldsql.schema.SQLForm
import net.noresttherein.oldsql.schema.ColumnForm.{JDBCForm, NullableJDBCForm}
import net.noresttherein.oldsql.schema.SQLForm.{NonLiteralForm, NullValue}






/** A trait extended by all [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]s and
  * [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]s in order to bring implicit forms for all
  * standard JDBC types into the implicit search scope. This allows extracting of these forms into a separate
  * companion object (and share them between read and write forms).
  */
//trait JDBCTypes



/** Implicit `SQLForm` definitions for all SQL types defined by JDBC in `java.sql.Types`. These are standard
  * implementations using the default classes associated by JDBC with the a given SQL type, except where
  * a scala counterpart to the type exists (for example `BigDecimalForm` is a form of `scala.BigDecimal`, not
  * `java.math.BigDecimal`. Forms for competing Java classes, including all Java boxed primitives, can be
  * found in [[net.noresttherein.oldsql.schema.forms.JavaForms JavaForms]]. All these forms are in the implicit search scope
  * for all form classes, so, prefer to rely on implicit resolution by using `SQLForm[Xxx]` to access a form
  * for the type `Xxx` instead of explicit references to declarations in this object and others.
  */
trait JDBCTypes {

	private[this] implicit val NotNull = NullValue.NotNull



/*
	def get[X](value: X): Option[SQLForm[X]] = (value match {
		case _ :Number => PartialFunction.condOpt(value) {
			case _ :Int | _ :JInt => IntForm
			case _ :Long | _ :JLong => LongForm
			case _ :Double | _ :JDouble => DoubleForm
			case _ :Float | _ :JFloat => FloatForm
			case _ :BigDecimal => BigDecimalForm
			case _ :JBigDecimal => SQLForms.JavaBigDecimalForm
			case _ :Byte | _ :JByte => ByteForm
			case _ :Short | _ :JShort => ShortForm
		}
		case _ => PartialFunction.condOpt(value) {
			case _ :String => StringForm
			case _ :Boolean | _ :JBoolean => BooleanForm
			case _ :Char | _ :JChar => CharForm
			case _ :sql.Timestamp => TimestampForm
			case _ :sql.Date => DateForm
			case _ :sql.Time => TimeForm
			case _ :Array[Byte] => BytesForm
			case _ :sql.Blob => BlobForm
			case _ :sql.NClob => NClobForm
			case _ :sql.Clob => ClobForm
			case _ :URL => URLForm
			case _ :ByteArrayInputStream => BinaryStreamForm
//			case _ :InputStream => BinaryStreamForm
			case _ :Reader => CharacterStreamForm
			case _ :sql.Ref => RefForm
			case _ :sql.RowId => RowIdForm
			case _ :sql.Array => SQLArrayForm
			case _ :sql.SQLXML => SQLXMLForm
		}
	}).asInstanceOf[Option[SQLForm[X]]]
*/






	val BasicLiteralTypes = Seq(
		BigDecimalForm, BooleanForm, ByteForm, DateForm, DoubleForm, FloatForm, IntForm, LongForm, ShortForm,
		StringForm, TimeForm, TimestampForm, URLForm
	)



	//todo: support for arbitrary NullValue type classes
	//todo: remove NonLiteralForm as the base class where possible.
	implicit case object SQLArrayForm extends NullableJDBCForm[sql.Array](ARRAY) with NonLiteralForm[sql.Array] {

		override def set(statement :PreparedStatement, position :Int, value :sql.Array) :Unit =
			statement.setArray(position, value)

		protected override def read(res :ResultSet, column :Int) :sql.Array = res.getArray(column)
	}


	case object ASCIIStreamForm extends NullableJDBCForm[InputStream](LONGVARCHAR) with NonLiteralForm[InputStream] {

		override def set(statement :PreparedStatement, position :Int, value :InputStream) :Unit =
			statement.setAsciiStream(position, value)

		protected override def read(res :ResultSet, column :Int) :InputStream = res.getAsciiStream(column)
	}


	implicit case object BigDecimalForm extends NullableJDBCForm[BigDecimal](DECIMAL) {

		override def set(statement :PreparedStatement, position :Int, value :BigDecimal) :Unit =
			statement.setBigDecimal(position, value.bigDecimal)

		protected override def read(res :ResultSet, column :Int) :BigDecimal = res.getBigDecimal(column)
	}



	case object BinaryStreamForm extends NullableJDBCForm[InputStream](LONGVARBINARY) with NonLiteralForm[InputStream] {

		override def set(statement :PreparedStatement, position :Int, value :InputStream) :Unit =
			statement.setBinaryStream(position, value)

		protected override def read(res :ResultSet, column :Int) :InputStream = res.getBinaryStream(column)
	}


	implicit case object BlobForm extends NullableJDBCForm[Blob](BLOB) with NonLiteralForm[Blob] {

		override def set(statement :PreparedStatement, position :Int, value :Blob) :Unit =
			statement.setBlob(position, value)

		protected override def read(res :ResultSet, column :Int) :Blob = res.getBlob(column)
	}


	implicit case object BooleanForm extends JDBCForm[Boolean](BOOLEAN) {

		override def set(statement :PreparedStatement, position :Int, value :Boolean) :Unit =
			statement.setBoolean(position, value)

		protected override def read(res :ResultSet, column :Int) :Boolean = res.getBoolean(column)
	}


	implicit case object ByteForm extends JDBCForm[Byte](TINYINT) {

		override def set(statement :PreparedStatement, position :Int, value :Byte) :Unit =
			statement.setByte(position, value)

		protected override def read(res :ResultSet, column :Int) :Byte = res.getByte(column)
	}


	implicit case object BytesForm extends NullableJDBCForm[Array[Byte]](BINARY) with NonLiteralForm[Array[Byte]] {

		override def set(statement :PreparedStatement, position :Int, value :Array[Byte]) :Unit =
			statement.setBytes(position, value)

		protected override def read(res :ResultSet, column :Int) :Array[Byte] = res.getBytes(column)
	}


	implicit case object CharForm extends JDBCForm[Char](CHAR) {
		override protected def read(res :ResultSet, position :Int) :Char =
			res.getString(position) match {
				case null => 0
				case s if s.length != 1 =>
					throw new IllegalArgumentException(
						"Read '" + s + "' instead of a single Char from result set at index " + position)
				case c => c.charAt(0)
			}

		override def set(statement :PreparedStatement, position :Int, value :Char) :Unit =
			statement.setString(position, String.valueOf(value))
	}


	implicit case object CharacterStreamForm extends NullableJDBCForm[Reader](LONGVARCHAR) with NonLiteralForm[Reader] {

		override def set(statement :PreparedStatement, position :Int, value :Reader) :Unit =
			statement.setCharacterStream(position, value)

		protected override def read(res :ResultSet, column :Int) :Reader = res.getCharacterStream(column)
	}


	implicit case object ClobForm extends NullableJDBCForm[Clob](CLOB) with NonLiteralForm[Clob] {

		override def set(statement :PreparedStatement, position :Int, value :Clob) :Unit =
			statement.setClob(position, value)

		protected override def read(res :ResultSet, column :Int) :Clob = res.getClob(column)
	}


	implicit case object DateForm extends NullableJDBCForm[sql.Date](DATE) {

		override def set(statement :PreparedStatement, position :Int, value :Date) :Unit =
			statement.setDate(position, value)

		protected override def read(res :ResultSet, column :Int) :sql.Date = res.getDate(column)

		override def literal(value :sql.Date) :String =
			if (value == null) "null" else format.format(value.toLocalDate)

		private[this] val format = DateTimeFormatter.ofPattern("'uuuu-MM-dd'")
	}


	implicit case object DoubleForm extends JDBCForm[Double](DOUBLE) {

		override def set(statement :PreparedStatement, position :Int, value :Double) :Unit =
			statement.setDouble(position, value)

		protected override def read(res :ResultSet, column :Int) :Double = res.getDouble(column)
	}


	implicit case object FloatForm extends JDBCForm[Float](FLOAT) {

		override def set(statement :PreparedStatement, position :Int, value :Float) :Unit =
			statement.setFloat(position, value)

		protected override def read(res :ResultSet, column :Int) :Float = res.getFloat(column)
	}


	implicit case object IntForm extends JDBCForm[Int](INTEGER) {

		override def set(statement :PreparedStatement, position :Int, value :Int) :Unit =
			statement.setInt(position, value)

		protected override def read(res :ResultSet, column :Int) :Int = res.getInt(column)
	}


	implicit case object LongForm extends JDBCForm[Long](BIGINT) {

		override def set(statement :PreparedStatement, position :Int, value :Long) :Unit =
			statement.setLong(position, value)

		protected override def read(res :ResultSet, column :Int) :Long = res.getLong(column)
	}


	case object NCharacterStreamForm extends NullableJDBCForm[Reader](LONGNVARCHAR) with NonLiteralForm[Reader] {

		override def set(statement :PreparedStatement, position :Int, value :Reader) :Unit =
			statement.setNCharacterStream(position, value)

		protected override def read(res :ResultSet, column :Int) :Reader = res.getNCharacterStream(column)
	}


	implicit case object NClobForm extends NullableJDBCForm[NClob](NCLOB) with NonLiteralForm[NClob] {

		override def set(statement :PreparedStatement, position :Int, value :NClob) :Unit =
			statement.setNClob(position, value)

		protected override def read(res :ResultSet, column :Int) :NClob = res.getNClob(column)
	}


	case object NStringForm extends NullableJDBCForm[String](NVARCHAR) {

		override def set(statement :PreparedStatement, position :Int, value :String) :Unit =
			statement.setNString(position, value)

		protected override def read(res :ResultSet, column :Int) :String = res.getNString(column)

		override def literal(value :String) :String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"
	}


	implicit case object RefForm extends NullableJDBCForm[sql.Ref](REF) with NonLiteralForm[Ref] {

		override def set(statement :PreparedStatement, position :Int, value :Ref) :Unit =
			statement.setRef(position, value)

		protected override def read(res :ResultSet, column :Int) :Ref = res.getRef(column)
	}


	implicit case object RowIdForm extends NullableJDBCForm[sql.RowId](REF) with NonLiteralForm[RowId] {

		override protected def read(res :ResultSet, position :Int) :RowId = res.getRowId(position)

		override def set(statement :PreparedStatement, position :Int, value :RowId) :Unit =
			statement.setRowId(position, value)
	}


	implicit case object ShortForm extends JDBCForm[Short](SMALLINT) {

		override def set(statement :PreparedStatement, position :Int, value :Short) :Unit =
			statement.setShort(position, value)

		protected override def read(res :ResultSet, column :Int) :Short = res.getShort(column)
	}


	implicit case object SQLXMLForm extends NullableJDBCForm[SQLXML](JDBCType.SQLXML) with NonLiteralForm[SQLXML] {

		override def set(statement :PreparedStatement, position :Int, value :SQLXML) :Unit =
			statement.setSQLXML(position, value)

		protected override def read(res :ResultSet, column :Int) :SQLXML = res.getSQLXML(column)
	}


	implicit case object StringForm extends NullableJDBCForm[String](VARCHAR) {

		override def set(statement :PreparedStatement, position :Int, value :String) :Unit =
			statement.setString(position, value)

		protected override def read(res :ResultSet, column :Int) :String = res.getString(column)

		override def literal(value: String): String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"
	}


	implicit case object TimestampForm extends NullableJDBCForm[sql.Timestamp](TIMESTAMP) with NonLiteralForm[sql.Timestamp] {

		override def set(statement :PreparedStatement, position :Int, value :Timestamp) :Unit =
			statement.setTimestamp(position, value)

		protected override def read(res :ResultSet, column :Int) :sql.Timestamp = res.getTimestamp(column)
	}


	implicit case object TimeForm extends NullableJDBCForm[sql.Time](TIME) with NonLiteralForm[sql.Time] {

		override def set(statement :PreparedStatement, position :Int, value :Time) :Unit =
			statement.setTime(position, value)

		protected override def read(res :ResultSet, column :Int) :sql.Time = res.getTime(column)
	}


	implicit case object URLForm extends NullableJDBCForm[URL](NVARCHAR) {

		override def set(statement :PreparedStatement, position :Int, value :URL) :Unit =
			statement.setURL(position, value)

		protected override def read(res :ResultSet, column :Int) :URL = res.getURL(column)

		override def toString = "URL"
	}


	class NullForm[T >: Null] extends NullableJDBCForm[T](NULL) {
		override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
			statement.setNull(position, NULL.getVendorTypeNumber)

		protected override def read(res :ResultSet, column :Int) :T = null

		override def equals(that :Any) :Boolean = that.isInstanceOf[NullForm[_]]
	}


	object NullForm extends NullForm[Null] {
		def as[T >: Null] :NullForm[T] = this.asInstanceOf[NullForm[T]]
	}


}
