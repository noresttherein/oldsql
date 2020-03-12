package net.noresttherein.oldsql.schema

import java.lang.{Boolean => JBoolean, Byte => JByte, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Date, NClob, PreparedStatement, Ref, ResultSet, SQLXML, Time, Timestamp, Types}
import java.sql.Types._
import java.time.format.DateTimeFormatter

import net.noresttherein.oldsql.schema.SQLForm.{NonLiteralForm, NullableForm}



trait SQLTypes {
	def get[X](x :X) :Option[SQLForm[X]]
}



trait JDBCTypes extends SQLTypes {

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
		SQLForm[T].asOpt(t => Some(Option(t)), None)(o => o)

	implicit def OptionColumnForm[T :ColumnForm] :ColumnForm[Option[T]] =
		implicitly[ColumnForm[T]].asOpt(t => Some(Option(t)), None)(o => o)

	implicit def SomeForm[T :SQLForm] :SQLForm[Some[T]] = SQLForm[T].as(Some(_))(_.get)

	implicit def SomeColumnForm[T :ColumnForm] :ColumnForm[Some[T]] =
		implicitly[ColumnForm[T]].as(Some(_))(_.get)
*/






	abstract class JDBCForm[T](val sqlType :Int) extends SQLForm[T]





	val BasicLiteralTypes = Seq(BigDecimalForm, BooleanForm, ByteForm, DateForm, DoubleForm, FloatForm, IntForm, LongForm, ShortForm, StringForm, TimeForm, TimestampForm, URLForm)


	implicit case object SQLArrayForm extends JDBCForm[sql.Array](ARRAY) with ColumnForm[sql.Array]
		                                 with NullableForm[sql.Array] with NonLiteralForm[sql.Array]
	{
		override def set(position :Int)(statement :PreparedStatement, value :sql.Array) :Unit =
			statement.setArray(position, value)

		override def apply(position: Int)(res: ResultSet): sql.Array = res.getArray(position)

		override def toString = "ARRAY"
	}

	case object ASCIIStreamForm extends JDBCForm[InputStream](LONGVARCHAR) with ColumnForm[InputStream]
		                           with NullableForm[InputStream] with NonLiteralForm[InputStream]
	{

		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setAsciiStream(position, value)

		override def apply(column: Int)(res: ResultSet): InputStream = res.getAsciiStream(column)

		override def toString = "LONGVARCHAR"
	}

	implicit case object BigDecimalForm extends JDBCForm[BigDecimal](DECIMAL)
		                                   with ColumnForm[BigDecimal] with NullableForm[BigDecimal]
	{
		override def set(position :Int)(statement :PreparedStatement, value :BigDecimal) :Unit =
			statement.setBigDecimal(position, value.bigDecimal)

		override def apply(column: Int)(res: ResultSet): BigDecimal = res.getBigDecimal(column)

		override def toString = "DECIMAL"
	}

	implicit case object JavaBigDecimalForm extends JDBCForm[JBigDecimal](DECIMAL)
		with ColumnForm[JBigDecimal] with NullableForm[JBigDecimal]
	{
		override def set(position :Int)(statement :PreparedStatement, value :JBigDecimal) :Unit =
			statement.setBigDecimal(position, value)

		override def apply(column: Int)(res: ResultSet): JBigDecimal = res.getBigDecimal(column)

		override def toString = "JDECIMAL"
	}

	case object BinaryStreamForm extends JDBCForm[InputStream](LONGVARBINARY) with ColumnForm[InputStream]
		                            with NullableForm[InputStream] with NonLiteralForm[InputStream]
	{
		override def set(position :Int)(statement :PreparedStatement, value :InputStream) :Unit =
			statement.setBinaryStream(position, value)

		override def apply(column: Int)(res: ResultSet): InputStream = res.getBinaryStream(column)

		override def toString = "LONGVARBINARY"
	}

	implicit case object BlobForm extends JDBCForm[Blob](BLOB) with ColumnForm[Blob]
		                             with NullableForm[Blob] with NonLiteralForm[Blob]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Blob) :Unit =
			statement.setBlob(position, value)

		override def apply(column: Int)(res: ResultSet): Blob = res.getBlob(column)

		override def toString = "BLOB"
	}

	implicit case object BooleanForm extends JDBCForm[Boolean](BOOLEAN) with ColumnForm[Boolean] {
		override def set(position :Int)(statement :PreparedStatement, value :Boolean) :Unit =
			statement.setBoolean(position, value)

		override def apply(column: Int)(res: ResultSet): Boolean = res.getBoolean(column)

		override def nullValue: Boolean = false

		override def toString = "BOOLEAN"
	}

	case object JavaBooleanForm extends JDBCForm[JBoolean](BOOLEAN) with ColumnForm[JBoolean] with NullableForm[JBoolean] {

		override def set(position :Int)(statement :PreparedStatement, value :JBoolean) :Unit =
			statement.setBoolean(position, value)

		override def apply(column :Int)(res :ResultSet) :JBoolean =
			Option(res.getBoolean(column) :JBoolean).filterNot(_ => res.wasNull).orNull

		override def toString = "JBOOLEAN"
	}

	implicit case object ByteForm extends JDBCForm[Byte](TINYINT) with ColumnForm[Byte] {
		override def set(position :Int)(statement :PreparedStatement, value :Byte) :Unit =
			statement.setByte(position, value)

		override def apply(column: Int)(res: ResultSet): Byte = res.getByte(column)

		override def nullValue: Byte = 0

		override def toString = "TINYINT"
	}

	implicit case object JavaByteForm extends JDBCForm[JByte](TINYINT)
		                                 with ColumnForm[JByte] with NullableForm[JByte]
	{

		override def set(position :Int)(statement :PreparedStatement, value :JByte) :Unit =
			statement.setByte(position, value)

		override def apply(column: Int)(res: ResultSet): JByte =
			Option(res.getByte(column) :JByte).filterNot(_ => res.wasNull).orNull

		override def toString = "JBYTE"
	}

	implicit case object BytesForm extends JDBCForm[Array[Byte]](BINARY) with ColumnForm[Array[Byte]]
		                              with NullableForm[Array[Byte]] with NonLiteralForm[Array[Byte]]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Array[Byte]) :Unit =
			statement.setBytes(position, value)

		override def apply(column: Int)(res: ResultSet) :Array[Byte] = res.getBytes(column)

		override def toString = "BINARY"
	}

	implicit case object CharacterStreamForm extends JDBCForm[Reader](LONGVARCHAR) with ColumnForm[Reader]
		                                        with NullableForm[Reader] with NonLiteralForm[Reader]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setCharacterStream(position, value)

		override def apply(column: Int)(res: ResultSet): Reader = res.getCharacterStream(column)

		override def toString = "LONGVARCHAR"
	}

	implicit case object ClobForm extends JDBCForm[Clob](CLOB) with ColumnForm[Clob]
		                             with NullableForm[Clob] with NonLiteralForm[Clob]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Clob) :Unit =
			statement.setClob(position, value)

		override def apply(column: Int)(res: ResultSet): Clob = res.getClob(column)

		override def toString = "CLOB"
	}

	implicit case object DateForm extends JDBCForm[sql.Date](DATE) with NullableForm[sql.Date] with ColumnForm[sql.Date] {
		override def set(position :Int)(statement :PreparedStatement, value :Date) :Unit =
			statement.setDate(position, value)

		override def apply(column: Int)(res: ResultSet): sql.Date = res.getDate(column)

		override def literal(value :sql.Date) :String = if (value == null) "null" else format.format(value.toLocalDate)

		override def toString = "DATE"

		private[this] val format = DateTimeFormatter.ofPattern("'uuuu-MM-dd'")
	}

	implicit case object DoubleForm extends JDBCForm[Double](DOUBLE) with ColumnForm[Double] {
		override def set(position :Int)(statement :PreparedStatement, value :Double) :Unit =
			statement.setDouble(position, value)

		override def apply(column: Int)(res: ResultSet): Double = res.getDouble(column)

		override def nullValue = 0.0

		override def toString = "DOUBLE"
	}

	implicit case object JavaDoubleForm extends JDBCForm[JDouble](DOUBLE) with ColumnForm[JDouble]
		                                   with NullableForm[JDouble]
	{

		override def set(position :Int)(statement :PreparedStatement, value :JDouble) :Unit =
			statement.setDouble(position, value)

		override def apply(column: Int)(res: ResultSet): JDouble =
			Option(res.getDouble(column) :JDouble).filterNot(_ => res.wasNull).orNull

		override def toString = "JDOUBLE"
	}

	implicit case object FloatForm extends JDBCForm[Float](FLOAT) with ColumnForm[Float] {
		override def set(position :Int)(statement :PreparedStatement, value :Float) :Unit =
			statement.setFloat(position, value)

		override def apply(column: Int)(res: ResultSet): Float = res.getFloat(column)

		override def nullValue = 0.0F

		override def toString = "FLOAT"
	}

	implicit case object JavaFloatForm extends JDBCForm[JFloat](FLOAT) with ColumnForm[JFloat]
		                                  with NullableForm[JFloat]
	{
		override def set(position :Int)(statement :PreparedStatement, value :JFloat) :Unit =
			statement.setFloat(position, value)

		override def apply(column: Int)(res: ResultSet): JFloat =
			Option(res.getFloat(column) :JFloat).filterNot(_ => res.wasNull).orNull

		override def toString = "JFLOAT"
	}

	implicit case object IntForm extends JDBCForm[Int](INTEGER) with ColumnForm[Int] {

		override def set(position :Int)(statement :PreparedStatement, value :Int) :Unit =
			statement.setInt(position, value)

		override def apply(column: Int)(res: ResultSet): Int = res.getInt(column)

		override def nullValue = 0

		override def toString = "INT"
	}

	implicit case object JavaIntForm extends JDBCForm[JInt](INTEGER) with ColumnForm[JInt] with NullableForm[JInt] {

		override def set(position :Int)(statement :PreparedStatement, value :JInt) :Unit =
			statement.setInt(position, value)

		override def apply(column: Int)(res: ResultSet): JInt =
			Option(res.getInt(column) :JInt).filterNot(_ => res.wasNull).orNull

		override def toString = "JINT"
	}

	implicit case object LongForm extends JDBCForm[Long](BIGINT) with ColumnForm[Long] {
		override def set(position :Int)(statement :PreparedStatement, value :Long) :Unit =
			statement.setLong(position, value)

		override def apply(column: Int)(res: ResultSet): Long = res.getLong(column)

		override def nullValue = 0L

		override def toString = "BIGINT"
	}

	implicit case object JavaLongForm extends JDBCForm[JLong](BIGINT) with ColumnForm[JLong] with NullableForm[JLong] {

		override def set(position :Int)(statement :PreparedStatement, value :JLong) :Unit =
			statement.setLong(position, value)

		override def apply(column: Int)(res: ResultSet): JLong =
			Option(res.getLong(column) :JLong).filterNot(_ => res.wasNull).orNull

		override def toString = "JLONG"
	}

	case object NCharacterStreamForm extends JDBCForm[Reader](LONGNVARCHAR) with ColumnForm[Reader]
		                                with NullableForm[Reader] with NonLiteralForm[Reader]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Reader) :Unit =
			statement.setNCharacterStream(position, value)

		override def apply(column: Int)(res: ResultSet): Reader = res.getNCharacterStream(column)

		override def toString = "LONGNVARCHAR"
	}

	implicit case object NClobForm extends JDBCForm[NClob](NCLOB) with ColumnForm[NClob]
		                              with NullableForm[NClob] with NonLiteralForm[NClob]
	{
		override def set(position :Int)(statement :PreparedStatement, value :NClob) :Unit =
			statement.setNClob(position, value)

		override def apply(column: Int)(res: ResultSet): NClob = res.getNClob(column)
		override def toString = "NCLOB"
	}

	case object NStringForm extends JDBCForm[String](NVARCHAR) with ColumnForm[String] with NullableForm[String] {
		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setNString(position, value)

		override def apply(column: Int)(res: ResultSet): String = res.getNString(column)

		override def literal(value :String) :String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"

		override def toString = "NVARCHAR"
	}

	implicit case object RefForm extends JDBCForm[Ref](REF) with ColumnForm[Ref]
		with NullableForm[Ref] with NonLiteralForm[Ref]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Ref) :Unit =
			statement.setRef(position, value)

		override def apply(column: Int)(res: ResultSet): Ref = res.getRef(column)

		override def toString = "REF"
	}

	implicit case object ShortForm extends JDBCForm[Short](SMALLINT) with ColumnForm[Short] {

		override def set(position :Int)(statement :PreparedStatement, value :Short) :Unit =
			statement.setShort(position, value)

		override def apply(column: Int)(res: ResultSet): Short = res.getShort(column)

		override def nullValue :Short = 0.toShort

		override def toString = "SMALLINT"
	}

	implicit case object JavaShortForm extends JDBCForm[JShort](SMALLINT) with ColumnForm[JShort]
		                                  with NullableForm[JShort]
	{
		override def set(position :Int)(statement :PreparedStatement, value :JShort) :Unit =
			statement.setShort(position, value)

		override def apply(column: Int)(res: ResultSet): JShort =
			Option(res.getShort(column) :JShort).filterNot(_ => res.wasNull).orNull

		override def toString = "JSHORT"
	}


	implicit case object SQLXMLForm extends JDBCForm[SQLXML](Types.SQLXML) with ColumnForm[SQLXML]
		                               with NullableForm[SQLXML] with NonLiteralForm[SQLXML]
	{
		override def set(position :Int)(statement :PreparedStatement, value :SQLXML) :Unit =
			statement.setSQLXML(position, value)

		override def apply(column: Int)(res: ResultSet): SQLXML = res.getSQLXML(column)

		override def toString = "SQLXML"
	}

	implicit case object StringForm extends JDBCForm[String](VARCHAR) with ColumnForm[String] with NullableForm[String] {
		override def set(position :Int)(statement :PreparedStatement, value :String) :Unit =
			statement.setString(position, value)

		override def apply(column: Int)(res: ResultSet): String = res.getString(column)

		override def literal(value: String): String =
			if (value == null) "null"
			else "'" + value.replace("'", "''") + "'"

		override def toString = "VARCHAR"
	}

	implicit case object TimestampForm extends JDBCForm[sql.Timestamp](TIMESTAMP) with ColumnForm[sql.Timestamp]
		                                  with NullableForm[sql.Timestamp] with NonLiteralForm[sql.Timestamp]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Timestamp) :Unit =
			statement.setTimestamp(position, value)

		override def apply(column: Int)(res: ResultSet): sql.Timestamp = res.getTimestamp(column)

		override def toString = "TIMESTAMP"
	}

	implicit case object TimeForm extends JDBCForm[sql.Time](TIME) with ColumnForm[sql.Time]
		                             with NullableForm[sql.Time] with NonLiteralForm[sql.Time]
	{
		override def set(position :Int)(statement :PreparedStatement, value :Time) :Unit =
			statement.setTime(position, value)

		override def apply(column: String)(res: ResultSet): sql.Time = res.getTime(column)

		override def apply(column: Int)(res: ResultSet): sql.Time = res.getTime(column)

		override def toString = "TIME"
	}


	implicit case object URLForm extends JDBCForm[URL](NVARCHAR) with ColumnForm[URL] with NullableForm[URL] {
		override def set(position :Int)(statement :PreparedStatement, value :URL) :Unit =
			statement.setURL(position, value)

		override def apply(column: Int)(res: ResultSet): URL = res.getURL(column)

		override def toString = "URL"
	}

	class NullForm[T>:Null] extends JDBCForm[T](NULL) with ColumnForm[T] with NullableForm[T] {
		override def set(position :Int)(statement :PreparedStatement, value :T) :Unit =
			statement.setNull(position, NULL)

		override def apply(column: Int)(res: ResultSet): T = null

		override def equals(that :Any) :Boolean = that.isInstanceOf[NullForm[_]]

		override def toString = "NULL"
	}





}
