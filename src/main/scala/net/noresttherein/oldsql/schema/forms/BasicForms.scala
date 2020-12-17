package net.noresttherein.oldsql.schema.forms

import java.lang.{Boolean => JBoolean, Byte => JByte, Character => JChar, Double => JDouble, Float => JFloat, Integer => JInt, Long => JLong, Short => JShort}
import java.math.{BigDecimal => JBigDecimal}
import java.io.{InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Date, JDBCType, NClob, PreparedStatement, Ref, ResultSet, RowId, SQLXML, Time, Timestamp, Types}
import java.sql.JDBCType._
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime, ZonedDateTime, ZoneId, ZoneOffset}
import java.time.temporal.{ChronoField, TemporalAccessor}

import net.noresttherein.oldsql.collection.NaturalMap
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.Stateless
import net.noresttherein.oldsql.schema.ColumnForm
import net.noresttherein.oldsql.schema.ColumnForm.JDBCObjectForm
import net.noresttherein.oldsql.schema.SQLForm.{NonLiteralForm, NullValue}






/** A trait extended by all [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]s and
  * [[net.noresttherein.oldsql.schema.SQLWriteForm SQLWriteForm]]s in order to bring implicit forms for all
  * standard JDBC types into the implicit search scope. This allows extracting of these forms into a separate
  * companion object (and share them between read and write forms).
  */
//trait BasicForms


/** A collection of [[net.noresttherein.oldsql.schema.ColumnForm column forms]] for types explicitly supported
  * by the JDBC `ResultSet` and `PreparedStatement` API. This is the same set of forms as in
  * [[net.noresttherein.oldsql.schema.forms.BasicForms BasicForms]], but all are for not-null columns 
  * and will throw a `NullPointerException` when a `null` value is read or passed as a parameter.
  * This trait is extended by the [[net.noresttherein.oldsql.schema.forms.SQLForms$ SQLForms]] object,
  * which brings them into implicit search scope for all form types. Instead of referencing them directly, prefer
  * summoning an implicit value like this: [[net.noresttherein.oldsql.schema.ColumnForm.apply[T:ColumnForm] ColumnForm[Int] ]].
  */
trait NotNullBasicForms { this :BasicForms =>
	private implicit val NotNull = NullValue.NotNull
	
	implicit val NotNullSQLArray          :ColumnForm[sql.Array]     = SQLArrayForm
	val NotNullASCIIStream                :ColumnForm[InputStream]   = ASCIIStreamForm
	implicit val NotNullBigDecimal        :ColumnForm[BigDecimal]    = BigDecimalForm
	val NotNullBinaryStream               :ColumnForm[InputStream]   = BinaryStreamForm
	implicit val NotNullBlob              :ColumnForm[Blob]          = BlobForm
	implicit val NotNullBoolean           :ColumnForm[Boolean]       = BooleanForm
	implicit val NotNullByte              :ColumnForm[Byte]          = ByteForm
	implicit val NotNullBytes             :ColumnForm[Array[Byte]]   = BytesForm
	implicit val NotNullChar              :ColumnForm[Char]          = CharForm
	implicit val NotNullCharacterStream   :ColumnForm[Reader]        = CharacterStreamForm
	implicit val NotNullClob              :ColumnForm[Clob]          = ClobForm
	implicit val NotNullDate              :ColumnForm[Date]          = DateForm
	implicit val NotNullDouble            :ColumnForm[Double]        = DoubleForm
	implicit val NotNullFloat             :ColumnForm[Float]         = FloatForm
	implicit val NotNullInt               :ColumnForm[Int]           = IntForm
	implicit val NotNullLong              :ColumnForm[Long]          = LongForm
	val NotNullNCharacterStream           :ColumnForm[Reader]        = NCharacterStreamForm
	implicit val NotNullNClob             :ColumnForm[NClob]         = NCLobForm
	val NotNullNString                    :ColumnForm[String]        = NStringForm
	implicit val NotNullRef               :ColumnForm[Ref]           = RefForm
	implicit val NotNullRowId             :ColumnForm[RowId]         = RowIdForm
	implicit val NotNullShort             :ColumnForm[Short]         = ShortForm
	implicit val NotNullSQLXML            :ColumnForm[SQLXML]        = SQLXMLForm
	implicit val NotNullString            :ColumnForm[String]        = StringForm
	implicit val NotNullTime              :ColumnForm[Time]          = TimeForm
	implicit val NotNullTimestamp         :ColumnForm[Timestamp]     = TimestampForm
	implicit val NotNullURL               :ColumnForm[URL]           = URLForm

	implicit val NotNullJavaBigDecimal    :ColumnForm[JBigDecimal]   = JavaBigDecimalForm
	implicit val NotNullJavaBoolean       :ColumnForm[JBoolean]      = JavaBooleanForm
	implicit val NotNullJavaByte          :ColumnForm[JByte]         = JavaByteForm
	implicit val NotNullJavaChar          :ColumnForm[JChar]         = JavaCharForm
	implicit val NotNullJavaDouble        :ColumnForm[JDouble]       = JavaDoubleForm
	implicit val NotNullJavaFloat         :ColumnForm[JFloat]        = JavaFloatForm
	implicit val NotNullJavaInt           :ColumnForm[JInt]          = JavaIntForm
	implicit val NotNullJavaLong          :ColumnForm[JLong]         = JavaLongForm
	implicit val NotNullJavaShort         :ColumnForm[JShort]        = JavaShortForm

	implicit val NotNullInstant           :ColumnForm[Instant]        = InstantForm
	implicit val NotNullLocalTime         :ColumnForm[LocalTime]      = LocalTimeForm
	implicit val NotNullLocalDate         :ColumnForm[LocalDate]      = LocalDateForm
	implicit val NotNullLocalDateTime     :ColumnForm[LocalDateTime]  = LocalDateTimeForm
	implicit val NotNullOffsetTime        :ColumnForm[OffsetTime]     = OffsetTimeForm
	implicit val NotNullOffsetDateTime    :ColumnForm[OffsetDateTime] = OffsetDateTimeForm
	implicit val NotNullZonedDateTime     :ColumnForm[ZonedDateTime]  = ZonedDateTimeForm
	
	
	val BasicLiteralForms = Seq(
		NotNullBigDecimal, NotNullBoolean, NotNullByte, NotNullDate, NotNullDouble, NotNullFloat, NotNullInt,
		NotNullLong, NotNullShort, NotNullString, NotNullTime, NotNullTimestamp, NotNullURL
	)

	
	val NotNullType :NaturalMap[Class, ColumnForm] = NaturalMap(
		Assoc(classOf[sql.Array], NotNullSQLArray),
		Assoc(classOf[BigDecimal], NotNullBigDecimal),
		Assoc(classOf[Blob], NotNullBlob),
		Assoc(classOf[Boolean], NotNullBoolean),
		Assoc(classOf[Byte], NotNullByte),
		Assoc(classOf[Array[Byte]], NotNullBytes), //are we sure this must be the default?
		Assoc(classOf[Char], NotNullChar),
		Assoc(classOf[Reader], NotNullCharacterStream),
		Assoc(classOf[Clob], NotNullClob),
		Assoc(classOf[Date], NotNullDate),
		Assoc(classOf[Double], NotNullDouble),
		Assoc(classOf[Float], NotNullFloat),
		Assoc(classOf[Int], NotNullInt),
		Assoc(classOf[Long], NotNullLong),
		Assoc(classOf[NClob], NotNullNClob),
		Assoc(classOf[Ref], NotNullRef),
		Assoc(classOf[RowId], NotNullRowId),
		Assoc(classOf[Short], NotNullShort),
		Assoc(classOf[SQLXML], NotNullSQLXML),
		Assoc(classOf[String], NotNullString),
		Assoc(classOf[Time], NotNullTime),
		Assoc(classOf[Timestamp], NotNullTimestamp),
		Assoc(classOf[URL], NotNullURL),
		
		Assoc(classOf[JBigDecimal], NotNullJavaBigDecimal),
		Assoc(classOf[JBoolean], NotNullJavaBoolean),
		Assoc(classOf[JByte], NotNullJavaByte),
		Assoc(classOf[JChar], NotNullJavaChar),
		Assoc(classOf[JDouble], NotNullJavaDouble),
		Assoc(classOf[JFloat], NotNullJavaFloat),
		Assoc(classOf[JInt], NotNullJavaInt),
		Assoc(classOf[JLong], NotNullJavaLong),
		Assoc(classOf[JShort], NotNullJavaShort),

		Assoc(classOf[Instant], NotNullInstant),
		Assoc(classOf[LocalDate], NotNullLocalDate),
		Assoc(classOf[LocalTime], NotNullLocalTime),
		Assoc(classOf[LocalDateTime], NotNullLocalDateTime),
		Assoc(classOf[OffsetTime], NotNullOffsetTime),
		Assoc(classOf[OffsetDateTime], NotNullOffsetDateTime),
		Assoc(classOf[ZonedDateTime], NotNullZonedDateTime)
	)
	
}






/** A collection of [[net.noresttherein.oldsql.schema.ColumnForm column forms]] for types explicitly supported
  * by the JDBC `ResultSet` and `PreparedStatement` API. This is the same set of forms as in
  * [[net.noresttherein.oldsql.schema.forms.BasicForms BasicForms]], but all are for not-null columns
  * and will throw a `NullPointerException` when a `null` value is read or passed as a parameter.
  * This trait is extended by the [[net.noresttherein.oldsql.schema.forms.SQLForms$ SQLForms]] object,
  * which brings them into implicit search scope for all form types. Instead of referencing them directly, prefer
  * summoning an implicit value like this: [[net.noresttherein.oldsql.schema.ColumnForm.apply[T:ColumnForm] ColumnForm[Int] ]].
  */ //anonymous classes are PITA on stack traces ...
trait BasicForms extends NotNullBasicForms {
	import SQLForms.{BasicJDBCForm => Basic, AlternateJDBCForm => Alternate}
	import ColumnForm.jdbc


	implicit def SQLArrayForm(implicit nulls :NullValue[sql.Array]) :ColumnForm[sql.Array] =
		new Basic[sql.Array](ARRAY) {
			override def set(statement :PreparedStatement, position :Int, value :sql.Array) :Unit =
				statement.setArray(position, value)

			protected override def read(res :ResultSet, column :Int) :sql.Array = res.getArray(column)
		}

	def ASCIIStreamForm(implicit nulls :NullValue[InputStream]) :ColumnForm[InputStream] =
		new Basic[InputStream](LONGVARCHAR) with NonLiteralForm[InputStream] {
			override def set(statement :PreparedStatement, position :Int, value :InputStream) :Unit =
				statement.setAsciiStream(position, value)

			protected override def read(res :ResultSet, column :Int) :InputStream = res.getAsciiStream(column)
		}

	implicit def BigDecimalForm(implicit nulls :NullValue[BigDecimal]) :ColumnForm[BigDecimal] =
		new Basic[BigDecimal](DECIMAL) {
			override def set(statement :PreparedStatement, position :Int, value :BigDecimal) :Unit =
				statement.setBigDecimal(position, value.bigDecimal)

			protected override def read(res :ResultSet, column :Int) :BigDecimal = res.getBigDecimal(column)
		}

	def BinaryStreamForm(implicit nulls :NullValue[InputStream]) :ColumnForm[InputStream] =
		new Basic[InputStream](LONGVARBINARY) with NonLiteralForm[InputStream] {
			override def set(statement :PreparedStatement, position :Int, value :InputStream) :Unit =
				statement.setBinaryStream(position, value)

			protected override def read(res :ResultSet, column :Int) :InputStream = res.getBinaryStream(column)
		}

	implicit def BlobForm(implicit nulls :NullValue[Blob]) :ColumnForm[Blob] =
		new Basic[Blob](BLOB) with NonLiteralForm[Blob] {
			override def set(statement :PreparedStatement, position :Int, value :Blob) :Unit =
				statement.setBlob(position, value)

			protected override def read(res :ResultSet, column :Int) :Blob = res.getBlob(column)
		}

	implicit def BooleanForm(implicit nulls :NullValue[Boolean]) :ColumnForm[Boolean] =
		new Basic[Boolean](BOOLEAN) {
			override def set(statement :PreparedStatement, position :Int, value :Boolean) :Unit =
				statement.setBoolean(position, value)

			protected override def read(res :ResultSet, column :Int) :Boolean = res.getBoolean(column)
		}

	implicit def ByteForm(implicit nulls :NullValue[Byte]) :ColumnForm[Byte] =
		new Basic[Byte](TINYINT) {
			override def set(statement :PreparedStatement, position :Int, value :Byte) :Unit =
				statement.setByte(position, value)

			protected override def read(res :ResultSet, column :Int) :Byte = res.getByte(column)
		}

	implicit def BytesForm(implicit nulls :NullValue[Array[Byte]]) :ColumnForm[Array[Byte]] =
		new Basic[Array[Byte]](BINARY) with NonLiteralForm[Array[Byte]] {
			override def set(statement :PreparedStatement, position :Int, value :Array[Byte]) :Unit =
				statement.setBytes(position, value)

			protected override def read(res :ResultSet, column :Int) :Array[Byte] = res.getBytes(column)
		}

	implicit def CharForm(implicit nulls :NullValue[Char]) :ColumnForm[Char] =
		new Basic[Char](CHAR) {
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

	implicit def CharacterStreamForm(implicit nulls :NullValue[Reader]) :ColumnForm[Reader] =
		new Basic[Reader](LONGVARCHAR) with NonLiteralForm[Reader] {
			override def set(statement :PreparedStatement, position :Int, value :Reader) :Unit =
				statement.setCharacterStream(position, value)

			protected override def read(res :ResultSet, column :Int) :Reader = res.getCharacterStream(column)
		}

	implicit def ClobForm(implicit nulls :NullValue[Clob]) :ColumnForm[Clob] =
		new Basic[Clob](CLOB) with NonLiteralForm[Clob] {
			override def set(statement :PreparedStatement, position :Int, value :Clob) :Unit =
				statement.setClob(position, value)

			protected override def read(res :ResultSet, column :Int) :Clob = res.getClob(column)
		}

	implicit def DateForm(implicit nulls :NullValue[Date]) :ColumnForm[Date] =
		new Basic[Date](DATE) {
			override def set(statement :PreparedStatement, position :Int, value :Date) :Unit =
				statement.setDate(position, value)

			protected override def read(res :ResultSet, column :Int) :Date = res.getDate(column)

			override def literal(value :Date) :String = formatDate(value.toLocalDate)
		}

	implicit def DoubleForm(implicit nulls :NullValue[Double]) :ColumnForm[Double] =
		new Basic[Double](DOUBLE) {
			override def set(statement :PreparedStatement, position :Int, value :Double) :Unit =
				statement.setDouble(position, value)

			protected override def read(res :ResultSet, column :Int) :Double = res.getDouble(column)
		}

	implicit def FloatForm(implicit nulls :NullValue[Float]) :ColumnForm[Float] =
		new Basic[Float](FLOAT) {
			override def set(statement :PreparedStatement, position :Int, value :Float) :Unit =
				statement.setFloat(position, value)

			protected override def read(res :ResultSet, column :Int) :Float = res.getFloat(column)
		}

	implicit def IntForm(implicit nulls :NullValue[Int]) :ColumnForm[Int] =
		new Basic[Int](INTEGER) {
			override def set(statement :PreparedStatement, position :Int, value :Int) :Unit =
				statement.setInt(position, value)

			protected override def read(res :ResultSet, column :Int) :Int = res.getInt(column)
		}

	implicit def LongForm(implicit nulls :NullValue[Long]) :ColumnForm[Long] =
		new Basic[Long](BIGINT) { //todo: is BIGINT really the right type for long?
			override def set(statement :PreparedStatement, position :Int, value :Long) :Unit =
				statement.setLong(position, value)

			protected override def read(res :ResultSet, column :Int) :Long = res.getLong(column)
		}

	def NCharacterStreamForm(implicit nulls :NullValue[Reader]) :ColumnForm[Reader] =
		new Basic[Reader](LONGVARCHAR) with NonLiteralForm[Reader] {
			override def set(statement :PreparedStatement, position :Int, value :Reader) :Unit =
				statement.setNCharacterStream(position, value)

			protected override def read(res :ResultSet, column :Int) :Reader = res.getNCharacterStream(column)
		}

	implicit def NCLobForm(implicit nulls :NullValue[NClob]) :ColumnForm[NClob] =
		new Basic[NClob](NCLOB) with NonLiteralForm[NClob] {
			override def set(statement :PreparedStatement, position :Int, value :NClob) :Unit =
				statement.setNClob(position, value)

			protected override def read(res :ResultSet, column :Int) :NClob = res.getNClob(column)
		}

	def NStringForm(implicit nulls :NullValue[String]) :ColumnForm[String] =
		new Basic[String](NVARCHAR) {
			override def set(statement :PreparedStatement, position :Int, value :String) :Unit =
				statement.setNString(position, value)

			protected override def read(res :ResultSet, column :Int) :String = res.getNString(column)

			override def literal(value :String) :String =
				if (value == null) "NULL"
				else "'" + value.replace("'", "''") + "'"
		}

	implicit def RefForm(implicit nulls :NullValue[Ref]) :ColumnForm[Ref] =
		new Basic[Ref](REF) with NonLiteralForm[Ref] {
			override def set(statement :PreparedStatement, position :Int, value :Ref) :Unit =
				statement.setRef(position, value)

			protected override def read(res :ResultSet, column :Int) :Ref = res.getRef(column)
		}

	implicit def RowIdForm(implicit nulls :NullValue[RowId]) :ColumnForm[RowId] =
		new Basic[RowId](ROWID) with NonLiteralForm[RowId] {
			override def set(statement :PreparedStatement, position :Int, value :RowId) :Unit =
				statement.setRowId(position, value)

			override protected def read(res :ResultSet, position :Int) :RowId = res.getRowId(position)
		}
	implicit def ShortForm(implicit nulls :NullValue[Short]) :ColumnForm[Short] =
		new Basic[Short](SMALLINT) {
			override def set(statement :PreparedStatement, position :Int, value :Short) :Unit =
				statement.setShort(position, value)

			protected override def read(res :ResultSet, column :Int) :Short = res.getShort(column)
		}

	implicit def SQLXMLForm(implicit nulls :NullValue[SQLXML]) :ColumnForm[SQLXML] =
		new Basic[SQLXML](JDBCType.SQLXML) with NonLiteralForm[SQLXML] { //todo: is it really non literal?
			override def set(statement :PreparedStatement, position :Int, value :SQLXML) :Unit =
				statement.setSQLXML(position, value)

			protected override def read(res :ResultSet, column :Int) :SQLXML = res.getSQLXML(column)
		}

	implicit def StringForm(implicit nulls :NullValue[String]) :ColumnForm[String] =
		new Basic[String](VARCHAR) {
			override def set(statement :PreparedStatement, position :Int, value :String) :Unit =
				statement.setString(position, value)

			protected override def read(res :ResultSet, column :Int) :String = res.getString(column)

			override def literal(value: String): String =
				if (value == null) "NULL"
				else "'" + value.replace("'", "''") + "'"
		}

	implicit def TimeForm(implicit nulls :NullValue[Time]) :ColumnForm[Time] =
		new Basic[Time](TIME) {
			override def set(statement :PreparedStatement, position :Int, value :Time) :Unit =
				statement.setTime(position, value)

			protected override def read(res :ResultSet, column :Int) :sql.Time = res.getTime(column)

			override def literal(value :Time) :String = formatTime(value.toLocalTime)
		}

	implicit def TimestampForm(implicit nulls :NullValue[Timestamp]) :ColumnForm[Timestamp] =
		new Basic[Timestamp](TIMESTAMP) {
			override def set(statement :PreparedStatement, position :Int, value :Timestamp) :Unit =
				statement.setTimestamp(position, value)

			protected override def read(res :ResultSet, column :Int) :sql.Timestamp = res.getTimestamp(column)

			override def literal(value :Timestamp) :String = formatTimestamp(value.toLocalDateTime)
		}

	implicit def URLForm(implicit nulls :NullValue[URL]) :ColumnForm[URL] =
		new Basic[URL](NVARCHAR) {
			override def set(statement :PreparedStatement, position :Int, value :URL) :Unit =
				statement.setURL(position, value)

			protected override def read(res :ResultSet, column :Int) :URL = res.getURL(column)

			override def toString = "URL"
		}


	def NullForm[T >: Null] :ColumnForm[T] =
		new Basic[T](NULL)(NullValue.Null) with Stateless {
			override def set(statement :PreparedStatement, position :Int, value :T) :Unit =
				statement.setNull(position, NULL.getVendorTypeNumber)

			protected override def read(res :ResultSet, column :Int) :T = null
		}

	


	
	
	implicit def JavaBigDecimalForm(implicit nulls :NullValue[JBigDecimal]) :ColumnForm[JBigDecimal] =
		new Alternate[JBigDecimal](DECIMAL) {
			override def set(statement :PreparedStatement, position :Int, value :JBigDecimal) :Unit =
				statement.setBigDecimal(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JBigDecimal = res.getBigDecimal(column)
		}

	implicit def JavaBooleanForm(implicit nulls :NullValue[JBoolean]) :ColumnForm[JBoolean] =
		new Alternate[JBoolean](BOOLEAN) {
			override def set(statement :PreparedStatement, position :Int, value :JBoolean) :Unit =
				statement.setBoolean(position, value)
	
			protected override def read(res :ResultSet, column :Int) :JBoolean = {
				val bool = res.getBoolean(column)
				if (res.wasNull) null else bool
			}
		}

	implicit def JavaByteForm(implicit nulls :NullValue[JByte]) :ColumnForm[JByte] =
		new Alternate[JByte](TINYINT) {
			override def set(statement :PreparedStatement, position :Int, value :JByte) :Unit =
				statement.setByte(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JByte = {
				val byte = res.getByte(column)
				if (res.wasNull) null else byte
			}
		}

	implicit def JavaCharForm(implicit nulls :NullValue[JChar]) :ColumnForm[JChar] =
		new Alternate[JChar](CHAR) {
			override def set(statement :PreparedStatement, position :Int, value :JChar) :Unit =
				statement.setString(position, if (value == null) null else String.valueOf(value))
			
			override protected def read(res :ResultSet, position :Int) :JChar =
				res.getString(position) match {
					case null => null
					case s if s.length != 1 => throw new IllegalArgumentException(
						"Read '" + s + "' instead of a single Char from result set at index " + position
					)
					case c => c.charAt(0)
				}
		}

	implicit def JavaDoubleForm(implicit nulls :NullValue[JDouble]) :ColumnForm[JDouble] =
		new Alternate[JDouble](DOUBLE) {
			override def set(statement :PreparedStatement, position :Int, value :JDouble) :Unit =
				statement.setDouble(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JDouble = {
				val double = res.getDouble(column)
				if (res.wasNull) null else double
			}
		}

	implicit def JavaFloatForm(implicit nulls :NullValue[JFloat]) :ColumnForm[JFloat] =
		new Alternate[JFloat](FLOAT) {
			override def set(statement :PreparedStatement, position :Int, value :JFloat) :Unit =
				statement.setFloat(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JFloat = {
				val float = res.getFloat(column)
				if (res.wasNull) null else float
			}
		}

	implicit def JavaIntForm(implicit nulls :NullValue[JInt]) :ColumnForm[JInt] =
		new Alternate[JInt](INTEGER) {
			override def set(statement :PreparedStatement, position :Int, value :JInt) :Unit =
				statement.setInt(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JInt = {
				val int = res.getInt(column)
				if (res.wasNull) null else int
			}
		}

	implicit def JavaLongForm(implicit nulls :NullValue[JLong]) :ColumnForm[JLong] =
		new Alternate[JLong](BIGINT) {
			override def set(statement :PreparedStatement, position :Int, value :JLong) :Unit =
				statement.setLong(position, value)
	
			protected override def read(res: ResultSet, column :Int) :JLong = {
				val long = res.getLong(column)
				if (res.wasNull) null else long
			}
		}

	implicit def JavaShortForm(implicit nulls :NullValue[JShort]) :ColumnForm[JShort] =
		new Alternate[JShort](SMALLINT) {
			override def set(statement :PreparedStatement, position :Int, value :JShort) :Unit =
				statement.setShort(position, value)

			protected override def read(res :ResultSet, column :Int) :JShort = {
				val short = res.getShort(column)
				if (res.wasNull) null else short
			}
		}






	//todo: verify how these work when the timezones differ
	implicit def InstantForm(implicit nulls :NullValue[Instant], timeZone :ZoneId = ZoneId.systemDefault())
			:ColumnForm[Instant] =
		new JDBCObjectForm[Instant](TIMESTAMP_WITH_TIMEZONE) {
			override def literal(value :Instant) :String =
				formatTimestamp(value.atOffset(timeZone.getRules.getOffset(value)))
		}

	implicit def LocalTimeForm(implicit nulls :NullValue[LocalTime]) :ColumnForm[LocalTime] =
		new JDBCObjectForm[LocalTime](TIME) {
			override def literal(value :LocalTime) :String = formatTime(value)
		}

	implicit def LocalDateForm(implicit nulls :NullValue[LocalDate]) :ColumnForm[LocalDate] =
		new JDBCObjectForm[LocalDate](DATE) {
			override def literal(value :LocalDate) :String = formatDate(value)
		}

	implicit def LocalDateTimeForm(implicit nulls :NullValue[LocalDateTime]) :ColumnForm[LocalDateTime] =
		new JDBCObjectForm[LocalDateTime](TIMESTAMP) {
			override def literal(value :LocalDateTime) :String = formatTimestamp(value)
		}

	implicit def OffsetTimeForm(implicit nulls :NullValue[OffsetTime]) :ColumnForm[OffsetTime] =
		new JDBCObjectForm[OffsetTime](TIME_WITH_TIMEZONE) {
			override def literal(value :OffsetTime) :String = formatTimeWithOffset(value)
		}

	implicit def OffsetDateTimeForm(implicit nulls :NullValue[OffsetDateTime]) :ColumnForm[OffsetDateTime] =
		new JDBCObjectForm[OffsetDateTime](TIMESTAMP_WITH_TIMEZONE) {
			override def literal(value :OffsetDateTime) :String = formatTimestampWithOffset(value)
		}

	implicit def ZonedDateTimeForm(implicit nulls :NullValue[ZonedDateTime]) :ColumnForm[ZonedDateTime] =
		new JDBCObjectForm[ZonedDateTime](TIMESTAMP_WITH_TIMEZONE) {
			override def literal(value :ZonedDateTime) :String = formatTimestampWithTimeZone(value)
		}



	private def formatDate(time :LocalDate) :String =
		if (time == null) "NULL" else "DATE '" + DateFormat.format(time) + "'"

	private def formatTime(time :LocalTime) :String =
		if (time == null) "NULL"
		else "TIME '" + (time.getNano match {
			case 0 => TimeFormat.format(time)
			case millis if millis % 1_000_000 == 0 => TimeWithMillisFormat.format(time)
			case _ => TimeWithNanosFormat.format(time).substring(0, TimeFormatLength)
		}) + "'"

	private def formatTimeWithOffset(time :OffsetTime) :String =
		if (time == null) "NULL"
		else "TIME WITH TIME ZONE '" + (time.getNano match {
			case 0 => TimeWithTimeZoneFormat.format(time)
			case millis if millis % 1_000_000 == 0 => TimeWithTimeZoneWithMillisFormat.format(time)
			case _ =>
				val nanosWithZone = TimeWithTimeZoneWithNanosFormat.format(time)
				nanosWithZone.substring(0, TimeFormatLength) + nanosWithZone.substring(TimeFormatLength + 3)
		}) + "'"

	private def formatTimestamp(time :TemporalAccessor) :String =
		if (time == null) "NULL"
		else "TIMESTAMP '" + (time.get(ChronoField.NANO_OF_SECOND) match {
			case 0 => TimestampFormat.format(time)
			case millis if millis % 1_000_000 == 0 => TimestampWithMillisFormat.format(time)
			case _ => TimestampWithNanosFormat.format(time).substring(0, TimestampFormatLength)
		}) + "'"

	private def formatTimestampWithOffset(time :TemporalAccessor) :String =
		if (time == null) "NULL"
		else "TIMESTAMP WITH TIME ZONE '" + (time.get(ChronoField.NANO_OF_SECOND) match {
			case 0 => TimestampWithOffsetFormat.format(time)
			case millis if millis % 1_000_000 == 0 => TimestampWithOffsetWithMillisFormat.format(time)
			case _ =>
				val nanosWithZone = TimestampWithOffsetWithNanosFormat.format(time)
				nanosWithZone.substring(0, TimestampFormatLength) + nanosWithZone.substring(TimestampFormatLength + 3)
		}) + "'"

	private def formatTimestampWithTimeZone(time :TemporalAccessor) :String =
		if (time == null) "NULL"
		else "TIMESTAMP WITH TIME ZONE '" + (time.get(ChronoField.NANO_OF_SECOND) match {
			case 0 => TimestampWithTimeZoneFormat.format(time)
			case millis if millis % 1_000_000 == 0 => TimestampWithTimeZoneWithMillisFormat.format(time)
			case _ =>
				val nanosWithZone = TimestampWithTimeZoneWithNanosFormat.format(time)
				nanosWithZone.substring(0, TimestampFormatLength) + nanosWithZone.substring(TimestampFormatLength + 3)
		}) + "'"


	//ofPattern formats `"` as `'`!
	private val DateFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd")

	private final val TimeFormatLength = "HH:mm:ss.nnnnnn".length
	private val TimeFormat = DateTimeFormatter.ofPattern("HH:mm:ss")
	private val TimeWithMillisFormat = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")
	private val TimeWithNanosFormat = DateTimeFormatter.ofPattern("HH:mm:ss.nnnnnnnnn")

	private val TimeWithTimeZoneFormat = DateTimeFormatter.ofPattern("HH:mm:ssx")
	private val TimeWithTimeZoneWithMillisFormat = DateTimeFormatter.ofPattern("HH:mm:ss.SSS x")
	private val TimeWithTimeZoneWithNanosFormat = DateTimeFormatter.ofPattern("HH:mm:ss.nnnnnnnnn x")

	private final val TimestampFormatLength = "uuuu-MM-dd HH:mm:ss.nnnnnn".length
	private val TimestampFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss")
	private val TimestampWithMillisFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS")
	private val TimestampWithNanosFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.nnnnnnnnn")

	private val TimestampWithOffsetFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss x")
	private val TimestampWithOffsetWithMillisFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS x")
	private val TimestampWithOffsetWithNanosFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.nnnnnnnnn x")
	
	private val TimestampWithTimeZoneFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss VV")
	private val TimestampWithTimeZoneWithMillisFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.SSS VV")
	private val TimestampWithTimeZoneWithNanosFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd HH:mm:ss.nnnnnnnnn VV")

}






