package com.adpilot.cortb.clientapi.golem.sql


class JDBCTypes {
	import java.io.{InputStream, Reader}
	import java.net.URL
	import java.sql.{Blob, Clob, Date, Time, Timestamp}
	import java.util.Calendar

	import RValue.Type
	import RValue.Type._


	class BooleanType extends Bool
	class CharType extends Sortable[Char] with In[Char]
	class StringType extends Sortable[String] with In[String] with Like[String] with Additive[String]

	class ByteType extends Integ[Byte] with In[Byte]
	class ShortType extends Integ[Short] with In[Short]
	class IntType extends Integ[Int] with In[Int]
	class LongType extends Integ[Long] with In[Long]
	class BigDecimalType extends Real[BigDecimal] with In[BigDecimal]
	class FloatType extends Real[Float] with In[Float]
	class DoubleType extends Real[Double] with In[Double]

	class DateType(calendar :Calendar) extends Sortable[Date] with In[Date] {
		def this() = this(Calendar.getInstance())
	}
	class TimeType(calendar :Calendar) extends Sortable[Time] with In[Time] {
		def this() = this(Calendar.getInstance())
	}
	class TimestampType(calendar :Calendar) extends Sortable[Timestamp] with In[Timestamp] {
		def this() = this(Calendar.getInstance())
	}

	class URLType extends Sortable[URL] with In[URL]

	class ByteArrayType extends Type[Array[Byte]]
	class BlobType extends Type[Blob]
	class ClobType extends Type[Clob]

	class CharStreamType extends Type[Reader]
	class BinaryStreamType extends Type[InputStream]
	class ASCIIStreamType extends Type[InputStream]

	implicit val BooleanType = new BooleanType
	implicit val CharType = new CharType
	implicit val StringType = new StringType

	implicit val ByteType = new ByteType
	implicit val ShortType = new ShortType
	implicit val IntType = new IntType
	implicit val LongType = new LongType
	implicit val BigDecimalType = new BigDecimalType
	implicit val FloatType = new FloatType
	implicit val DoubleType = new DoubleType

	implicit val DateType = new DateType
	implicit val TimeType = new TimeType
	implicit val TimestampType = new TimestampType

	implicit val URLType = new URLType

	implicit val ByteArrayType = new ByteArrayType
	implicit val BlobType = new BlobType
	implicit val ClobType = new ClobType

	implicit val CharStreamType = new CharStreamType
	implicit val BinaryStreamType = new BinaryStreamType
	val ASCIIStreamType = new ASCIIStreamType

	//todo: Ref, RowId, Array, Object, NClob, NString, NCharacterStream
}

object JDBCTypes extends JDBCTypes
