package com.hcore.ogre.sql


import java.lang.{Byte=>JByte, Short=>JShort, Integer=>JInt, Long=>JLong, Boolean=>JBoolean, Double=>JDouble, Float=>JFloat}
import java.io.{Reader, InputStream}
import java.net.URL
import java.sql
import java.sql.{Clob, Blob, Types, ResultSet, Ref, NClob, SQLXML}

import com.hcore.ogre.sql.SQLForm.{NonLiteralForm, JDBCForm, NullableForm}

import scala.slick.jdbc.{PositionedResult, PositionedParameters}


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

	implicit def OptionType[T :SQLForm] :SQLForm[Option[T]] = SQLForm[T].asOpt(t=>Some(Option(t)), None)(o => o)

	implicit def OptionAtomicType[T :AtomicForm] :AtomicForm[Option[T]] =
		implicitly[AtomicForm[T]].asOpt(t=>Some(Option(t)), None)(o => o)

	implicit def SomeType[T :SQLForm] :SQLForm[Some[T]] = SQLForm[T].as(Some(_))(_.get)

	implicit def SomeAtomicType[T :AtomicForm] :AtomicForm[Some[T]] =
		implicitly[AtomicForm[T]].as(Some(_))(_.get)




	abstract class JDBCForm[T](val sqlType :Int) extends SQLForm[T] {
		def apply(params :PositionedParameters, value :T) :Unit = params.setObject(value.asInstanceOf[AnyRef], sqlType)

		override def apply(res: PositionedResult): T = {
			val r = apply(res.currentPos)(res.rs)
			res.skip
			r
		}
	}





	val BasicLiteralTypes = Seq(BigDecimalForm, BooleanForm, ByteForm, DateForm, DoubleForm, FloatForm, IntForm, LongForm, ShortForm, StringForm, TimeForm, TimestampForm, URLForm)


	implicit case object SQLArrayForm extends JDBCForm[sql.Array](sql.Types.ARRAY) with AtomicForm[sql.Array] with NullableForm[sql.Array] with NonLiteralForm[sql.Array] {
		override def apply(column: String)(res: ResultSet): sql.Array = res.getArray(column)
		override def apply(position: Int)(res: ResultSet): sql.Array = res.getArray(position)
		override def toString = "ARRAY"
	}

	case object ASCIIStreamForm extends JDBCForm[InputStream](Types.LONGVARCHAR) with AtomicForm[InputStream] with NullableForm[InputStream] with NonLiteralForm[InputStream] {
		override def apply(column: String)(res: ResultSet): InputStream = res.getAsciiStream(column)
		override def apply(column: Int)(res: ResultSet): InputStream = res.getAsciiStream(column)
		override def toString = "LONGVARCHAR"
	}

	implicit case object BigDecimalForm extends JDBCForm[BigDecimal](Types.DECIMAL) with AtomicForm[BigDecimal] with NullableForm[BigDecimal] {
		override def apply(column: String)(res: ResultSet): BigDecimal = res.getBigDecimal(column)
		override def apply(column: Int)(res: ResultSet): BigDecimal = res.getBigDecimal(column)
		override def toString = "DECIMAL"
	}

	case object BinaryStreamForm extends JDBCForm[InputStream](Types.LONGVARBINARY) with AtomicForm[InputStream] with NullableForm[InputStream] with NonLiteralForm[InputStream] {
		override def apply(column: String)(res: ResultSet): InputStream = res.getBinaryStream(column)
		override def apply(column: Int)(res: ResultSet): InputStream = res.getBinaryStream(column)
		override def toString = "LONGVARBINARY"
	}

	implicit case object BlobForm extends JDBCForm[Blob](Types.BLOB) with AtomicForm[Blob] with NullableForm[Blob] with NonLiteralForm[Blob] {
		override def apply(column: String)(res: ResultSet): Blob = res.getBlob(column)
		override def apply(column: Int)(res: ResultSet): Blob = res.getBlob(column)
		override def toString = "BLOB"
	}

	implicit case object BooleanForm extends JDBCForm[Boolean](Types.BOOLEAN) with AtomicForm[Boolean] {
		override def apply(column: String)(res: ResultSet): Boolean = res.getBoolean(column)
		override def apply(column: Int)(res: ResultSet): Boolean = res.getBoolean(column)

		override def nullValue: Boolean = false
		override def toString = "BOOLEAN"
	}

	case object JavaBooleanForm extends JDBCForm[JBoolean](Types.BOOLEAN) with AtomicForm[JBoolean] {
		override def apply(column :String)(res :ResultSet) :JBoolean =
			Option(res.getBoolean(column) :JBoolean).filterNot(_ => res.wasNull).orNull

		override def apply(column :Int)(res :ResultSet) :JBoolean =
			Option(res.getBoolean(column) :JBoolean).filterNot(_ => res.wasNull).orNull

		override def nullValue :JBoolean = null
		override def toString = "JBOOLEAN"
	}

	implicit case object ByteForm extends JDBCForm[Byte](Types.TINYINT) with AtomicForm[Byte] {
		override def apply(column: String)(res: ResultSet): Byte = res.getByte(column)
		override def apply(column: Int)(res: ResultSet): Byte = res.getByte(column)
		override def nullValue: Byte = 0
		override def toString = "TINYINT"
	}

	implicit case object JavaByteForm extends JDBCForm[JByte](Types.TINYINT) with AtomicForm[JByte] with NullableForm[JByte] {
		override def apply(column: String)(res: ResultSet): JByte = Option(res.getByte(column) :JByte).filterNot(_ => res.wasNull).orNull
		override def apply(column: Int)(res: ResultSet): JByte = Option(res.getByte(column) :JByte).filterNot(_ => res.wasNull).orNull
		override def toString = "JBYTE"
	}

	implicit case object BytesForm extends JDBCForm[Array[Byte]](Types.BINARY) with AtomicForm[Array[Byte]] with NullableForm[Array[Byte]] with NonLiteralForm[Array[Byte]] {
		override def apply(column: String)(res: ResultSet) :Array[Byte] = res.getBytes(column)
		override def apply(column: Int)(res: ResultSet) :Array[Byte] = res.getBytes(column)
		override def toString = "BINARY"
	}

	implicit case object CharacterStreamForm extends JDBCForm[Reader](Types.LONGVARCHAR) with AtomicForm[Reader] with NullableForm[Reader] with NonLiteralForm[Reader] {
		override def apply(column: String)(res: ResultSet): Reader = res.getCharacterStream(column)
		override def apply(column: Int)(res: ResultSet): Reader = res.getCharacterStream(column)
		override def toString = "LONGVARCHAR"
	}

	implicit case object ClobForm extends JDBCForm[Clob](Types.CLOB) with AtomicForm[Clob] with NullableForm[Clob] with NonLiteralForm[Clob] {
		override def apply(column: String)(res: ResultSet): Clob = res.getClob(column)
		override def apply(column: Int)(res: ResultSet): Clob = res.getClob(column)
		override def toString = "CLOB"
	}

	implicit case object DateForm extends JDBCForm[sql.Date](Types.DATE) with AtomicForm[sql.Date] with NullableForm[sql.Date] with NonLiteralForm[sql.Date] {
		override def apply(column: String)(res: ResultSet): sql.Date = res.getDate(column)
		override def apply(column: Int)(res: ResultSet): sql.Date = res.getDate(column)
		override def toString = "DATE"
	}

	implicit case object DoubleForm extends JDBCForm[Double](Types.DOUBLE) with AtomicForm[Double] {
		override def apply(column: String)(res: ResultSet): Double = res.getDouble(column)
		override def apply(column: Int)(res: ResultSet): Double = res.getDouble(column)

		override def nullValue = 0.0
		override def toString = "DOUBLE"
	}

	implicit case object JavaDoubleForm extends JDBCForm[JDouble](Types.DOUBLE) with AtomicForm[JDouble] with NullableForm[JDouble] {
		override def apply(column: String)(res: ResultSet): JDouble =
			Option(res.getDouble(column) :JDouble).filterNot(_ => res.wasNull).orNull

		override def apply(column: Int)(res: ResultSet): JDouble =
			Option(res.getDouble(column) :JDouble).filterNot(_ => res.wasNull).orNull
		override def toString = "JDOUBLE"
	}

	implicit case object FloatForm extends JDBCForm[Float](Types.FLOAT) with AtomicForm[Float] {
		override def apply(column: String)(res: ResultSet): Float = res.getFloat(column)
		override def apply(column: Int)(res: ResultSet): Float = res.getFloat(column)

		override def nullValue = 0.0F
		override def toString = "FLOAT"
	}

	implicit case object JavaFloatForm extends JDBCForm[JFloat](Types.FLOAT) with AtomicForm[JFloat] with NullableForm[JFloat] {
		override def apply(column: String)(res: ResultSet): JFloat =
			Option(res.getFloat(column) :JFloat).filterNot(_ => res.wasNull).orNull

		override def apply(column: Int)(res: ResultSet): JFloat =
			Option(res.getFloat(column) :JFloat).filterNot(_ => res.wasNull).orNull

		override def toString = "JFLOAT"
	}

	implicit case object IntForm extends JDBCForm[Int](Types.INTEGER) with AtomicForm[Int] {
		override def apply(column: String)(res: ResultSet): Int = res.getInt(column)
		override def apply(column: Int)(res: ResultSet): Int = res.getInt(column)

		override def nullValue = 0
		override def toString = "INTEGER"
	}

	implicit case object JavaIntForm extends JDBCForm[JInt](Types.INTEGER) with AtomicForm[JInt] with NullableForm[JInt] {
		override def apply(column: String)(res: ResultSet): JInt =
			Option(res.getInt(column) :JInt).filterNot(_ => res.wasNull).orNull

		override def apply(column: Int)(res: ResultSet): JInt =
			Option(res.getInt(column) :JInt).filterNot(_ => res.wasNull).orNull

		override def toString = "JINTEGER"
	}

	implicit case object LongForm extends JDBCForm[Long](Types.BIGINT) with AtomicForm[Long] {
		override def apply(column: String)(res: ResultSet): Long = res.getLong(column)
		override def apply(column: Int)(res: ResultSet): Long = res.getLong(column)

		override def nullValue = 0L
		override def toString = "BIGINT"
	}

	implicit case object JavaLongForm extends JDBCForm[JLong](Types.BIGINT) with AtomicForm[JLong] with NullableForm[JLong] {
		override def apply(column: String)(res: ResultSet): JLong =
			Option(res.getLong(column) :JLong).filterNot(_ => res.wasNull).orNull

		override def apply(column: Int)(res: ResultSet): JLong =
			Option(res.getLong(column) :JLong).filterNot(_ => res.wasNull).orNull
		override def toString = "JLONG"
	}

	case object NCharacterStreamForm extends JDBCForm[Reader](Types.LONGNVARCHAR) with AtomicForm[Reader] with NullableForm[Reader] with NonLiteralForm[Reader] {
		override def apply(column: String)(res: ResultSet): Reader = res.getNCharacterStream(column)
		override def apply(column: Int)(res: ResultSet): Reader = res.getNCharacterStream(column)
		override def toString = "LONGNVARCHAR"
	}

	implicit case object NClobForm extends JDBCForm[NClob](Types.NCLOB) with AtomicForm[NClob] with NullableForm[NClob] with NonLiteralForm[NClob] {
		override def apply(column: String)(res: ResultSet): NClob = res.getNClob(column)
		override def apply(column: Int)(res: ResultSet): NClob = res.getNClob(column)
		override def toString = "NCLOB"
	}

	case object NStringForm extends JDBCForm[String](Types.NVARCHAR) with AtomicForm[String] with NullableForm[String] {
		override def apply(column: String)(res: ResultSet): String = res.getNString(column)
		override def apply(column: Int)(res: ResultSet): String = res.getNString(column)
		override def literal(value :String) = s"'$value'"
		override def toString = "NVARCHAR"
	}

	implicit case object RefForm extends JDBCForm[Ref](Types.REF) with AtomicForm[Ref] with NullableForm[Ref] with NonLiteralForm[Ref] {
		override def apply(column: String)(res: ResultSet): Ref = res.getRef(column)
		override def apply(column: Int)(res: ResultSet): Ref = res.getRef(column)
		override def toString = "REF"
	}

	implicit case object ShortForm extends JDBCForm[Short](Types.SMALLINT) with AtomicForm[Short] {
		override def apply(column: String)(res: ResultSet): Short = res.getShort(column)
		override def apply(column: Int)(res: ResultSet): Short = res.getShort(column)

		override def nullValue = 0.toShort
		override def toString = "SMALLINT"
	}

	implicit case object JavaShortForm extends JDBCForm[JShort](Types.SMALLINT) with AtomicForm[JShort] with NullableForm[JShort] {
		override def apply(column: String)(res: ResultSet): JShort =
			Option(res.getShort(column) :JShort).filterNot(_ => res.wasNull).orNull

		override def apply(column: Int)(res: ResultSet): JShort =
			Option(res.getShort(column) :JShort).filterNot(_ => res.wasNull).orNull

		override def toString = "JSHORT"
	}


	implicit case object SQLXMLForm extends JDBCForm[SQLXML](Types.SQLXML) with AtomicForm[SQLXML] with NullableForm[SQLXML] with NonLiteralForm[SQLXML] {
		override def apply(column: String)(res: ResultSet): SQLXML = res.getSQLXML(column)
		override def apply(column: Int)(res: ResultSet): SQLXML = res.getSQLXML(column)
		override def toString = "SQLXML"
	}

	implicit case object StringForm extends JDBCForm[String](Types.VARCHAR) with AtomicForm[String] with NullableForm[String] {
		override def apply(column: String)(res: ResultSet): String = res.getString(column)
		override def apply(column: Int)(res: ResultSet): String = res.getString(column)
		override def literal(value: String): String = s"'$value'"
		override def toString = "VARCHAR"
	}

	implicit case object TimestampForm extends JDBCForm[sql.Timestamp](Types.TIMESTAMP) with AtomicForm[sql.Timestamp] with NullableForm[sql.Timestamp] with NonLiteralForm[sql.Timestamp] {
		override def apply(column: String)(res: ResultSet): sql.Timestamp = res.getTimestamp(column)
		override def apply(column: Int)(res: ResultSet): sql.Timestamp = res.getTimestamp(column)
		override def toString = "TIMESTAMP"
	}

	implicit case object TimeForm extends JDBCForm[sql.Time](Types.TIME) with AtomicForm[sql.Time] with NullableForm[sql.Time] with NonLiteralForm[sql.Time] {
		override def apply(column: String)(res: ResultSet): sql.Time = res.getTime(column)
		override def apply(column: Int)(res: ResultSet): sql.Time = res.getTime(column)
		override def toString = "TIME"
	}


	implicit case object URLForm extends JDBCForm[URL](Types.NVARCHAR) with AtomicForm[URL] with NullableForm[URL] {
		override def apply(column: String)(res: ResultSet): URL = res.getURL(column)
		override def apply(column: Int)(res: ResultSet): URL = res.getURL(column)
		override def toString = "URL"
	}

	class NullForm[T>:Null] extends JDBCForm[T](Types.NULL) with AtomicForm[T] with NullableForm[T] {
		override def apply(column: String)(res: ResultSet): T = null
		override def apply(column: Int)(res: ResultSet): T = null

		override def equals(that :Any) = that.isInstanceOf[NullForm[_]]
		override def toString = "NULL"
	}





}
