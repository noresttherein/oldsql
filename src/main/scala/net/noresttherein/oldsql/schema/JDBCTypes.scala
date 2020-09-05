package net.noresttherein.oldsql.schema

import java.lang.{Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{ByteArrayInputStream, InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Date, NClob, PreparedStatement, Ref, ResultSet, RowId, SQLXML, Time, Timestamp, Types}
import java.sql.Types._
import java.time.format.DateTimeFormatter

import net.noresttherein.oldsql.schema.ColumnForm.{JDBCForm, JDBCSQLType, NullableJDBCForm}
import net.noresttherein.oldsql.schema.JavaForms.JavaBigDecimalForm
import net.noresttherein.oldsql.schema.SQLForm.{NonLiteralForm, NullValue}



trait JDBCTypes



/** Implicit `SQLForm` definitions for all SQL types defined by JDBC in `java.sql.Types`. These are standard
  * implementations using the default classes associated by JDBC with the a given SQL type, except where
  * a scala counterpart to the type exists (for example `BigDecimalForm` is a form of `scala.BigDecimal`, not
  * `java.math.BigDecimal`. Forms for competing Java classes, including all Java boxed primitives, can be
  * found in [[net.noresttherein.oldsql.schema.JavaForms]]. All these forms are in the implicit search scope for
  * all form classes, so, prefer to rely on implicit resolution by using `SQLForm[Xxx]` to access a form
  * for the type `Xxx` instead of explicit references to declarations in this object and others.
  */
object JDBCTypes {

	private[this] implicit val NotNull = NullValue.NotNull



	def get[X](value: X): Option[SQLForm[X]] = (value match {
		case _ :Number => PartialFunction.condOpt(value) {
			case _ :JDBCSQLType | _ :JInt => IntForm
			case _ :Long | _ :JLong => LongForm
			case _ :Double | _ :JDouble => DoubleForm
			case _ :Float | _ :JFloat => FloatForm
			case _ :BigDecimal => BigDecimalForm
			case _ :JBigDecimal => JavaBigDecimalForm
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






	val BasicLiteralTypes = Seq(BigDecimalForm, BooleanForm, ByteForm, DateForm, DoubleForm, FloatForm, IntForm, LongForm, ShortForm, StringForm, TimeForm, TimestampForm, URLForm)



	implicit case object SQLArrayForm extends NullableJDBCForm[sql.Array](ARRAY) with NonLiteralForm[sql.Array] {

		override def set(position :Int)(statement :PreparedStatement, value :sql.Array) :Unit =
			statement.setArray(position, value)

		protected override def read(position: Int)(res: ResultSet): sql.Array = res.getArray(position)
	}


	case object ASCIIStreamForm extends NullableJDBCForm[InputStream](LONGVARCHAR) with NonLiteralForm[InputStream] {

		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setAsciiStream(position, value)

		protected override def read(column: Int)(res: ResultSet): InputStream = res.getAsciiStream(column)
	}


	implicit case object BigDecimalForm extends NullableJDBCForm[BigDecimal](DECIMAL) {

		override def set(position :Int)(statement :PreparedStatement, value :BigDecimal) :Unit =
			statement.setBigDecimal(position, value.bigDecimal)

		protected override def read(column: Int)(res: ResultSet): BigDecimal = res.getBigDecimal(column)
	}



	case object BinaryStreamForm extends NullableJDBCForm[InputStream](LONGVARBINARY) with NonLiteralForm[InputStream] {

		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setBinaryStream(position, value)

		protected override def read(column: Int)(res: ResultSet): InputStream = res.getBinaryStream(column)
	}


	implicit case object BlobForm extends NullableJDBCForm[Blob](BLOB) with NonLiteralForm[Blob] {

		override def set(position :Int)(statement :PreparedStatement, value :Blob) :Unit =
			statement.setBlob(position, value)

		protected override def read(column: Int)(res: ResultSet): Blob = res.getBlob(column)
	}


	implicit case object BooleanForm extends JDBCForm[Boolean](BOOLEAN) {

		override def set(position :Int)(statement :PreparedStatement, value :Boolean) :Unit =
			statement.setBoolean(position, value)

		protected override def read(column: Int)(res: ResultSet): Boolean = res.getBoolean(column)
	}


	implicit case object ByteForm extends JDBCForm[Byte](TINYINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Byte) :Unit =
			statement.setByte(position, value)

		protected override def read(column: Int)(res: ResultSet): Byte = res.getByte(column)
	}


	implicit case object BytesForm extends NullableJDBCForm[Array[Byte]](BINARY) with NonLiteralForm[Array[Byte]] {

		override def set(position :Int)(statement :PreparedStatement, value :Array[Byte]) :Unit =
			statement.setBytes(position, value)

		protected override def read(column: Int)(res: ResultSet) :Array[Byte] = res.getBytes(column)
	}


	implicit case object CharForm extends JDBCForm[Char](CHAR) {
		override protected def read(position :Int)(res :ResultSet) :Char =
			res.getString(position) match {
				case null => 0
				case s if s.length != 1 =>
					throw new IllegalArgumentException(
						"Read '" + s + "' instead of a single Char from result set at index " + position)
				case c => c.charAt(0)
			}

		override def set(position :Int)(statement :PreparedStatement, value :Char) :Unit =
			statement.setString(position, String.valueOf(value))
	}


	implicit case object CharacterStreamForm extends NullableJDBCForm[Reader](LONGVARCHAR) with NonLiteralForm[Reader] {

		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setCharacterStream(position, value)

		protected override def read(column: Int)(res: ResultSet): Reader = res.getCharacterStream(column)
	}


	implicit case object ClobForm extends NullableJDBCForm[Clob](CLOB) with NonLiteralForm[Clob] {

		override def set(position :Int)(statement :PreparedStatement, value :Clob) :Unit =
			statement.setClob(position, value)

		protected override def read(column: Int)(res: ResultSet): Clob = res.getClob(column)
	}


	implicit case object DateForm extends NullableJDBCForm[sql.Date](DATE) {

		override def set(position :Int)(statement :PreparedStatement, value :Date) :Unit =
			statement.setDate(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Date = res.getDate(column)

		override def literal(value :sql.Date) :String =
			if (value == null) "null" else format.format(value.toLocalDate)

		private[this] val format = DateTimeFormatter.ofPattern("'uuuu-MM-dd'")
	}


	implicit case object DoubleForm extends JDBCForm[Double](DOUBLE) {

		override def set(position :Int)(statement :PreparedStatement, value :Double) :Unit =
			statement.setDouble(position, value)

		protected override def read(column: Int)(res: ResultSet): Double = res.getDouble(column)
	}


	implicit case object FloatForm extends JDBCForm[Float](FLOAT) {

		override def set(position :Int)(statement :PreparedStatement, value :Float) :Unit =
			statement.setFloat(position, value)

		protected override def read(column: Int)(res: ResultSet): Float = res.getFloat(column)
	}


	implicit case object IntForm extends JDBCForm[Int](INTEGER) {

		override def set(position :Int)(statement :PreparedStatement, value :Int) :Unit =
			statement.setInt(position, value)

		protected override def read(column: Int)(res: ResultSet): Int = res.getInt(column)
	}


	implicit case object LongForm extends JDBCForm[Long](BIGINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Long) :Unit =
			statement.setLong(position, value)

		protected override def read(column: Int)(res: ResultSet): Long = res.getLong(column)
	}


	case object NCharacterStreamForm extends NullableJDBCForm[Reader](LONGNVARCHAR) with NonLiteralForm[Reader] {

		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setNCharacterStream(position, value)

		protected override def read(column: Int)(res: ResultSet): Reader = res.getNCharacterStream(column)
	}


	implicit case object NClobForm extends NullableJDBCForm[NClob](NCLOB) with NonLiteralForm[NClob] {

		override def set(position :Int)(statement :PreparedStatement, value :NClob) :Unit =
			statement.setNClob(position, value)

		protected override def read(column: Int)(res: ResultSet): NClob = res.getNClob(column)
	}


	case object NStringForm extends NullableJDBCForm[String](NVARCHAR) {

		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setNString(position, value)

		protected override def read(column: Int)(res: ResultSet): String = res.getNString(column)

		override def literal(value :String) :String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"
	}


	implicit case object RefForm extends NullableJDBCForm[sql.Ref](REF) with NonLiteralForm[Ref] {

		override def set(position :Int)(statement :PreparedStatement, value :Ref) :Unit =
			statement.setRef(position, value)

		protected override def read(column: Int)(res: ResultSet): Ref = res.getRef(column)
	}


	implicit case object RowIdForm extends NullableJDBCForm[sql.RowId](REF) with NonLiteralForm[RowId] {

		override protected def read(position :Int)(res :ResultSet) :RowId = res.getRowId(position)

		override def set(position :Int)(statement :PreparedStatement, value :RowId) :Unit =
			statement.setRowId(position, value)
	}


	implicit case object ShortForm extends JDBCForm[Short](SMALLINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Short) :Unit =
			statement.setShort(position, value)

		protected override def read(column: Int)(res: ResultSet): Short = res.getShort(column)
	}


	implicit case object SQLXMLForm extends NullableJDBCForm[SQLXML](Types.SQLXML) with NonLiteralForm[SQLXML] {

		override def set(position :Int)(statement :PreparedStatement, value :SQLXML) :Unit =
			statement.setSQLXML(position, value)

		protected override def read(column: Int)(res: ResultSet): SQLXML = res.getSQLXML(column)
	}


	implicit case object StringForm extends NullableJDBCForm[String](VARCHAR) {

		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setString(position, value)

		protected override def read(column: Int)(res: ResultSet): String = res.getString(column)

		override def literal(value: String): String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"
	}


	implicit case object TimestampForm extends NullableJDBCForm[sql.Timestamp](TIMESTAMP) with NonLiteralForm[sql.Timestamp] {

		override def set(position :Int)(statement :PreparedStatement, value :Timestamp) :Unit =
			statement.setTimestamp(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Timestamp = res.getTimestamp(column)
	}


	implicit case object TimeForm extends NullableJDBCForm[sql.Time](TIME) with NonLiteralForm[sql.Time] {

		override def set(position :Int)(statement :PreparedStatement, value :Time) :Unit =
			statement.setTime(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Time = res.getTime(column)
	}


	implicit case object URLForm extends NullableJDBCForm[URL](NVARCHAR) {

		override def set(position :Int)(statement :PreparedStatement, value :URL) :Unit =
			statement.setURL(position, value)

		protected override def read(column: Int)(res: ResultSet): URL = res.getURL(column)

		override def toString = "URL"
	}


	class NullForm[T >: Null] extends NullableJDBCForm[T](NULL) {
		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			statement.setNull(position, NULL)

		protected override def read(column: Int)(res: ResultSet): T = null

		override def equals(that :Any) :Boolean = that.isInstanceOf[NullForm[_]]
	}


	object NullForm extends NullForm[Null] {
		def as[T >: Null] :NullForm[T] = this.asInstanceOf[NullForm[T]]
	}


}
