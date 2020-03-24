package net.noresttherein.oldsql.schema

import java.lang.{Boolean => JBoolean, Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Date, NClob, PreparedStatement, Ref, ResultSet, SQLXML, Time, Timestamp, Types}
import java.sql.Types._
import java.time.format.DateTimeFormatter

import net.noresttherein.oldsql.schema.ColumnForm.NullableColumnForm
import net.noresttherein.oldsql.schema.SQLForm.{NonLiteralForm, NullValue}



trait SQLTypes {
	def get[X](x :X) :Option[SQLForm[X]]
}



/** Includes implicit `SQLForm` declarations for all SQL types defined by JDBC in `java.sql.Types`. */
trait JDBCTypes extends SQLTypes {
	type JDBCSQLType = Int

	private[this] implicit val NotNull = NullValue.NotNull


	def apply[T :SQLForm] :SQLForm[T] = implicitly[SQLForm[T]]


	override def get[X](value: X): Option[SQLForm[X]] = PartialFunction.condOpt(value){
		case _ :Boolean => BooleanForm
		case _ :BigDecimal => BigDecimalForm
		case _ :Byte => ByteForm
		case _ :Short => ShortForm
		case _ :Int => IntForm
		case _ :Long => LongForm
		case _ :Float => FloatForm
		case _ :Double => DoubleForm
		case _ :sql.Date => DateForm
		case _ :sql.Time => TimeForm
		case _ :sql.Timestamp => TimestampForm
		case _ :URL => URLForm

	}.asInstanceOf[Option[SQLForm[X]]]






/*
	implicit def OptionForm[T :SQLForm] :SQLForm[Option[T]] =
		SQLForm[T].biflatMap(t => Some(Option(t)), None)(o => o)

	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		implicitly[ColumnForm[T]].biflatMap(t => Some(Option(t)), None)(o => o)

	implicit def SomeForm[T :SQLForm] :SQLForm[Some[T]] = SQLForm[T].as(Some(_))(_.get)

	implicit def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		implicitly[ColumnForm[T]].as(Some(_))(_.get)
*/






	abstract class JDBCForm[T](val sqlType :JDBCSQLType)(implicit override val nulls :NullValue[T]) extends ColumnForm[T] {
		override def nullValue :T = nulls.value
	}

	abstract class NullableJDBCForm[T >: Null](sqlType :JDBCSQLType)
		extends JDBCForm[T](sqlType) with NullableColumnForm[T]
	{
		override val nulls :NullValue[T] = NullValue.Null
	}





	val BasicLiteralTypes = Seq(BigDecimalForm, BooleanForm, ByteForm, DateForm, DoubleForm, FloatForm, IntForm, LongForm, ShortForm, StringForm, TimeForm, TimestampForm, URLForm)


	implicit case object SQLArrayForm extends NullableJDBCForm[sql.Array](ARRAY) with NonLiteralForm[sql.Array] {

		override def set(position :Int)(statement :PreparedStatement, value :sql.Array) :Unit =
			statement.setArray(position, value)

		protected override def read(position: Int)(res: ResultSet): sql.Array = res.getArray(position)

		override def toString = "ARRAY"
	}

	case object ASCIIStreamForm extends NullableJDBCForm[InputStream](LONGVARCHAR) with NonLiteralForm[InputStream] {

		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setAsciiStream(position, value)

		protected override def read(column: Int)(res: ResultSet): InputStream = res.getAsciiStream(column)

		override def toString = "LONGVARCHAR"
	}

	implicit case object BigDecimalForm extends NullableJDBCForm[BigDecimal](DECIMAL) {

		override def set(position :Int)(statement :PreparedStatement, value :BigDecimal) :Unit =
			statement.setBigDecimal(position, value.bigDecimal)

		protected override def read(column: Int)(res: ResultSet): BigDecimal = res.getBigDecimal(column)

		override def toString = "DECIMAL"
	}

	implicit case object JavaBigDecimalForm extends NullableJDBCForm[JBigDecimal](DECIMAL) {

		override def set(position :Int)(statement :PreparedStatement, value :JBigDecimal) :Unit =
			statement.setBigDecimal(position, value)

		protected override def read(column: Int)(res: ResultSet): JBigDecimal = res.getBigDecimal(column)

		override def toString = "JDECIMAL"
	}

	case object BinaryStreamForm extends NullableJDBCForm[InputStream](LONGVARBINARY) with NonLiteralForm[InputStream] {

		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setBinaryStream(position, value)

		protected override def read(column: Int)(res: ResultSet): InputStream = res.getBinaryStream(column)

		override def toString = "LONGVARBINARY"
	}

	implicit case object BlobForm extends NullableJDBCForm[Blob](BLOB) with NonLiteralForm[Blob] {

		override def set(position :Int)(statement :PreparedStatement, value :Blob) :Unit =
			statement.setBlob(position, value)

		protected override def read(column: Int)(res: ResultSet): Blob = res.getBlob(column)

		override def toString = "BLOB"
	}

	implicit case object BooleanForm extends JDBCForm[Boolean](BOOLEAN) {

		override def set(position :Int)(statement :PreparedStatement, value :Boolean) :Unit =
			statement.setBoolean(position, value)

		protected override def read(column: Int)(res: ResultSet): Boolean = res.getBoolean(column)

		override def toString = "BOOLEAN"
	}

	case object JavaBooleanForm extends NullableJDBCForm[JBoolean](BOOLEAN) {

		override def set(position :Int)(statement :PreparedStatement, value :JBoolean) :Unit =
			statement.setBoolean(position, value)

		protected override def read(column :Int)(res :ResultSet) :JBoolean = {
			val bool = res.getBoolean(column)
			if (res.wasNull) null else bool
		}

		override def toString = "JBOOLEAN"
	}

	implicit case object ByteForm extends JDBCForm[Byte](TINYINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Byte) :Unit =
			statement.setByte(position, value)

		protected override def read(column: Int)(res: ResultSet): Byte = res.getByte(column)

		override def toString = "TINYINT"
	}

	implicit case object JavaByteForm extends NullableJDBCForm[JByte](TINYINT) {

		override def set(position :Int)(statement :PreparedStatement, value :JByte) :Unit =
			statement.setByte(position, value)

		protected override def read(column: Int)(res: ResultSet): JByte = {
			val byte = res.getByte(column)
			if (res.wasNull) null else byte
		}

		override def toString = "JBYTE"
	}

	implicit case object BytesForm extends NullableJDBCForm[Array[Byte]](BINARY) with NonLiteralForm[Array[Byte]] {

		override def set(position :Int)(statement :PreparedStatement, value :Array[Byte]) :Unit =
			statement.setBytes(position, value)

		protected override def read(column: Int)(res: ResultSet) :Array[Byte] = res.getBytes(column)

		override def toString = "BINARY"
	}

	implicit case object CharacterStreamForm extends NullableJDBCForm[Reader](LONGVARCHAR) with NonLiteralForm[Reader] {

		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setCharacterStream(position, value)

		protected override def read(column: Int)(res: ResultSet): Reader = res.getCharacterStream(column)

		override def toString = "LONGVARCHAR"
	}

	implicit case object ClobForm extends NullableJDBCForm[Clob](CLOB) with NonLiteralForm[Clob] {

		override def set(position :Int)(statement :PreparedStatement, value :Clob) :Unit =
			statement.setClob(position, value)

		protected override def read(column: Int)(res: ResultSet): Clob = res.getClob(column)

		override def toString = "CLOB"
	}

	implicit case object DateForm extends NullableJDBCForm[sql.Date](DATE) {

		override def set(position :Int)(statement :PreparedStatement, value :Date) :Unit =
			statement.setDate(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Date = res.getDate(column)

		override def literal(value :sql.Date) :String = if (value == null) "null" else format.format(value.toLocalDate)

		override def toString = "DATE"

		private[this] val format = DateTimeFormatter.ofPattern("'uuuu-MM-dd'")
	}

	implicit case object DoubleForm extends JDBCForm[Double](DOUBLE) {

		override def set(position :Int)(statement :PreparedStatement, value :Double) :Unit =
			statement.setDouble(position, value)

		protected override def read(column: Int)(res: ResultSet): Double = res.getDouble(column)

		override def toString = "DOUBLE"
	}

	implicit case object JavaDoubleForm extends NullableJDBCForm[JDouble](DOUBLE) {

		override def set(position :Int)(statement :PreparedStatement, value :JDouble) :Unit =
			statement.setDouble(position, value)

		protected override def read(column: Int)(res: ResultSet): JDouble = {
			val double = res.getDouble(column)
			if (res.wasNull) null else double
		}

		override def toString = "JDOUBLE"
	}

	implicit case object FloatForm extends JDBCForm[Float](FLOAT) {

		override def set(position :Int)(statement :PreparedStatement, value :Float) :Unit =
			statement.setFloat(position, value)

		protected override def read(column: Int)(res: ResultSet): Float = res.getFloat(column)

		override def toString = "FLOAT"
	}

	implicit case object JavaFloatForm extends NullableJDBCForm[JFloat](FLOAT) {

		override def set(position :Int)(statement :PreparedStatement, value :JFloat) :Unit =
			statement.setFloat(position, value)

		protected override def read(column: Int)(res: ResultSet): JFloat = {
			val float = res.getFloat(column)
			if (res.wasNull) null else float
		}

		override def toString = "JFLOAT"
	}

	implicit case object IntForm extends JDBCForm[Int](INTEGER) {

		override def set(position :Int)(statement :PreparedStatement, value :Int) :Unit =
			statement.setInt(position, value)

		protected override def read(column: Int)(res: ResultSet): Int = res.getInt(column)

		override def toString = "INT"
	}

	implicit case object JavaIntForm extends NullableJDBCForm[JInt](INTEGER) {

		override def set(position :Int)(statement :PreparedStatement, value :JInt) :Unit =
			statement.setInt(position, value)

		protected override def read(column: Int)(res: ResultSet): JInt = {
			val int = res.getInt(column)
			if (res.wasNull) null else int
		}

		override def toString = "JINT"
	}

	implicit case object LongForm extends JDBCForm[Long](BIGINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Long) :Unit =
			statement.setLong(position, value)

		protected override def read(column: Int)(res: ResultSet): Long = res.getLong(column)

		override def toString = "BIGINT"
	}

	implicit case object JavaLongForm extends NullableJDBCForm[JLong](BIGINT) {

		override def set(position :Int)(statement :PreparedStatement, value :JLong) :Unit =
			statement.setLong(position, value)

		protected override def read(column: Int)(res: ResultSet): JLong = {
			val long = res.getLong(column)
			if (res.wasNull) null else long
		}

		override def toString = "JLONG"
	}

	case object NCharacterStreamForm extends NullableJDBCForm[Reader](LONGNVARCHAR) with NonLiteralForm[Reader] {

		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setNCharacterStream(position, value)

		protected override def read(column: Int)(res: ResultSet): Reader = res.getNCharacterStream(column)

		override def toString = "LONGNVARCHAR"
	}

	implicit case object NClobForm extends NullableJDBCForm[NClob](NCLOB) with NonLiteralForm[NClob] {

		override def set(position :Int)(statement :PreparedStatement, value :NClob) :Unit =
			statement.setNClob(position, value)

		protected override def read(column: Int)(res: ResultSet): NClob = res.getNClob(column)

		override def toString = "NCLOB"
	}

	case object NStringForm extends NullableJDBCForm[String](NVARCHAR) {

		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setNString(position, value)

		protected override def read(column: Int)(res: ResultSet): String = res.getNString(column)

		override def literal(value :String) :String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"

		override def toString = "NVARCHAR"
	}

	implicit case object RefForm extends NullableJDBCForm[Ref](REF) with NonLiteralForm[Ref] {

		override def set(position :Int)(statement :PreparedStatement, value :Ref) :Unit =
			statement.setRef(position, value)

		protected override def read(column: Int)(res: ResultSet): Ref = res.getRef(column)

		override def toString = "REF"
	}

	implicit case object ShortForm extends JDBCForm[Short](SMALLINT) {

		override def set(position :Int)(statement :PreparedStatement, value :Short) :Unit =
			statement.setShort(position, value)

		protected override def read(column: Int)(res: ResultSet): Short = res.getShort(column)

		override def toString = "SMALLINT"
	}

	implicit case object JavaShortForm extends NullableJDBCForm[JShort](SMALLINT) {

		override def set(position :Int)(statement :PreparedStatement, value :JShort) :Unit =
			statement.setShort(position, value)

		protected override def read(column: Int)(res: ResultSet): JShort = {
			val short = res.getShort(column)
			if (res.wasNull) null else short
		}

		override def toString = "JSHORT"
	}


	implicit case object SQLXMLForm extends NullableJDBCForm[SQLXML](Types.SQLXML) with NonLiteralForm[SQLXML] {

		override def set(position :Int)(statement :PreparedStatement, value :SQLXML) :Unit =
			statement.setSQLXML(position, value)

		protected override def read(column: Int)(res: ResultSet): SQLXML = res.getSQLXML(column)

		override def toString = "SQLXML"
	}

	implicit case object StringForm extends NullableJDBCForm[String](VARCHAR) {

		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setString(position, value)

		protected override def read(column: Int)(res: ResultSet): String = res.getString(column)

		override def literal(value: String): String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"

		override def toString = "VARCHAR"
	}

	implicit case object TimestampForm extends NullableJDBCForm[sql.Timestamp](TIMESTAMP) with NonLiteralForm[sql.Timestamp] {

		override def set(position :Int)(statement :PreparedStatement, value :Timestamp) :Unit =
			statement.setTimestamp(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Timestamp = res.getTimestamp(column)

		override def toString = "TIMESTAMP"
	}

	implicit case object TimeForm extends NullableJDBCForm[sql.Time](TIME) with NonLiteralForm[sql.Time] {

		override def set(position :Int)(statement :PreparedStatement, value :Time) :Unit =
			statement.setTime(position, value)

		protected override def read(column: Int)(res: ResultSet): sql.Time = res.getTime(column)

		override def toString = "TIME"
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

		override def toString = "NULL"
	}





}
