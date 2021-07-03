package net.noresttherein.oldsql.pixies

import java.io.{InputStream, Reader}
import java.net.URL
import java.sql
import java.sql.{Blob, Clob, Connection, Date, JDBCType, NClob, ParameterMetaData, PreparedStatement, Ref, ResultSet, ResultSetMetaData, RowId, SQLWarning, SQLXML, Time, Timestamp}
import java.util.Calendar

import scala.annotation.nowarn

import net.noresttherein.oldsql.sql.enquote






/**
  * @author Marcin Mościcki
  */
private[oldsql] class RecordingPreparedStatement(val params :Int) extends PreparedStatement {
	private[this] val types  = new Array[JDBCType](params)
	private[this] val values = new Array[Any](params)

	def parameterValues       :collection.Seq[Any]      = values
	def parameterTypes        :collection.Seq[JDBCType] = types
	def apply(i :Int)         :Any                      = values(i - 1)
	def parameterType(i :Int) :JDBCType                 = types(i - 1)

	private def unsupported = throw new UnsupportedOperationException


	override def execute(sql :String, autoGeneratedKeys :Int) :Boolean = unsupported
	override def execute(sql :String, columnIndexes :Array[Int]) :Boolean = unsupported
	override def execute(sql :String, columnNames :Array[String]) :Boolean = unsupported
	override def execute(sql :String) :Boolean = unsupported
	override def executeUpdate(sql :String, autoGeneratedKeys :Int) :Int = unsupported
	override def executeUpdate(sql :String, columnIndexes :Array[Int]) :Int = unsupported
	override def executeUpdate(sql :String, columnNames :Array[String]) :Int = unsupported
	override def executeUpdate(sql :String) :Int = unsupported
	override def executeQuery(sql :String) :ResultSet = unsupported
	override def executeBatch() :Array[Int] = unsupported
	override def executeLargeUpdate(sql :String) :Long = unsupported
	override def executeLargeUpdate(sql :String, autoGeneratedKeys :Int) :Long = unsupported
	override def executeLargeUpdate(sql :String, columnIndexes :Array[Int]) :Long = unsupported
	override def executeLargeUpdate(sql :String, columnNames :Array[String]) :Long = unsupported
	override def executeLargeBatch() :Array[Long] = unsupported

	override def getResultSet :ResultSet = unsupported
	override def getUpdateCount :Int = unsupported
	override def getMoreResults :Boolean = unsupported
	override def getMoreResults(current :Int) :Boolean = unsupported
	override def getGeneratedKeys :ResultSet = unsupported
	override def getLargeUpdateCount :Long = unsupported
	override def setLargeMaxRows(max :Long) :Unit = unsupported
	override def getLargeMaxRows :Long = unsupported

	override def addBatch(sql :String) :Unit = unsupported
	override def clearBatch() :Unit = unsupported

	override def setCursorName(name :String) :Unit = unsupported
	override def getMaxFieldSize :Int = unsupported
	override def setMaxFieldSize(max :Int) :Unit = unsupported
	override def getMaxRows :Int = unsupported
	override def setMaxRows(max :Int) :Unit = unsupported
	override def setEscapeProcessing(enable :Boolean) :Unit = unsupported
	override def getQueryTimeout :Int = unsupported
	override def setQueryTimeout(seconds :Int) :Unit = unsupported
	override def setFetchDirection(direction :Int) :Unit = unsupported
	override def getFetchDirection :Int = unsupported
	override def setFetchSize(rows :Int) :Unit = unsupported
	override def getFetchSize :Int = unsupported
	override def getResultSetConcurrency :Int = unsupported
	override def getResultSetType :Int = unsupported
	override def getResultSetHoldability :Int = unsupported
	override def setPoolable(poolable :Boolean) :Unit = unsupported
	override def isPoolable :Boolean = unsupported

	override def getWarnings :SQLWarning = unsupported
	override def clearWarnings() :Unit = unsupported

	override def closeOnCompletion() :Unit = unsupported
	override def isCloseOnCompletion :Boolean = unsupported

	override def getConnection :Connection = unsupported

	override def isClosed :Boolean = unsupported

	override def cancel() :Unit = unsupported
	override def close() :Unit = unsupported

	//todo: find good implementations
	override def enquoteLiteral(`val` :String) :String = enquote(`val`)
	override def enquoteIdentifier(identifier :String, alwaysQuote :Boolean) :String = enquote(identifier)
	override def enquoteNCharLiteral(`val` :String) :String = enquote(`val`)
	override def isSimpleIdentifier(identifier :String) :Boolean = unsupported

	override def isWrapperFor(iface :Class[_]) :Boolean = false
	override def unwrap[T](iface :Class[T]) :T = unsupported


	override def execute() :Boolean = unsupported
	override def executeQuery() :ResultSet = unsupported
	override def executeUpdate() :Int = unsupported

	override def addBatch() :Unit = unsupported
	override def clearParameters() :Unit = unsupported

	import java.sql.JDBCType._

	private def set(index :Int, value :Any, tpe :JDBCType) :Unit = { values(index - 1) = value; types(index - 1) = tpe }

	override def setArray(parameterIndex :Int, x :sql.Array) :Unit = set(parameterIndex, x, ARRAY)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = set(parameterIndex, x, LONGVARCHAR)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = set(parameterIndex, x, LONGVARCHAR)
	override def setAsciiStream(parameterIndex :Int, x :InputStream) :Unit = set(parameterIndex, x, LONGVARCHAR)
	override def setBigDecimal(parameterIndex :Int, x :java.math.BigDecimal) :Unit = set(parameterIndex, x, DECIMAL)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = set(parameterIndex, x, BINARY)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = set(parameterIndex, x, BINARY)
	override def setBinaryStream(parameterIndex :Int, x :InputStream) :Unit = set(parameterIndex, x, BINARY)
	override def setBoolean(parameterIndex :Int, x :Boolean) :Unit = set(parameterIndex, x, BOOLEAN)
	override def setByte(parameterIndex :Int, x :Byte) :Unit = set(parameterIndex, x, TINYINT)
	override def setBytes(parameterIndex :Int, x :Array[Byte]) :Unit = set(parameterIndex, x, VARBINARY)
	override def setBlob(parameterIndex :Int, x :Blob) :Unit = set(parameterIndex, x, BLOB)
	override def setBlob(parameterIndex :Int, inputStream :InputStream, length :Long) :Unit = set(parameterIndex, inputStream, BLOB)
	override def setBlob(parameterIndex :Int, inputStream :InputStream) :Unit = set(parameterIndex, inputStream, BLOB)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Int) :Unit = set(parameterIndex, reader, LONGVARCHAR)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Long) :Unit = set(parameterIndex, reader, LONGVARCHAR)
	override def setCharacterStream(parameterIndex :Int, reader :Reader) :Unit = set(parameterIndex, reader, LONGVARCHAR)
	override def setClob(parameterIndex :Int, x :Clob) :Unit = set(parameterIndex, x, CLOB)
	override def setClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = set(parameterIndex, reader, CLOB)
	override def setClob(parameterIndex :Int, reader :Reader) :Unit = set(parameterIndex, reader, CLOB)
	override def setDate(parameterIndex :Int, x :Date, cal :Calendar) :Unit = set(parameterIndex, x, DATE)
	override def setDate(parameterIndex :Int, x :Date) :Unit = set(parameterIndex, x, DATE)
	override def setDouble(parameterIndex :Int, x :Double) :Unit = set(parameterIndex, x, DOUBLE)
	override def setFloat(parameterIndex :Int, x :Float) :Unit = set(parameterIndex, x, FLOAT)
	override def setInt(parameterIndex :Int, x :Int) :Unit = set(parameterIndex, x, INTEGER)
	override def setLong(parameterIndex :Int, x :Long) :Unit = set(parameterIndex, x, BIGINT)
	override def setNCharacterStream(parameterIndex :Int, value :Reader, length :Long) :Unit = set(parameterIndex, value, LONGNVARCHAR)
	override def setNCharacterStream(parameterIndex :Int, value :Reader) :Unit = set(parameterIndex, value, LONGNVARCHAR)
	override def setNClob(parameterIndex :Int, value :NClob) :Unit = set(parameterIndex, value, NCLOB)
	override def setNClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = set(parameterIndex, reader, NCLOB)
	override def setNClob(parameterIndex :Int, reader :Reader) :Unit = set(parameterIndex, reader, NCLOB)
	override def setNString(parameterIndex :Int, value :String) :Unit = set(parameterIndex, value, NVARCHAR)
	override def setNull(parameterIndex :Int, sqlType :Int, typeName :String) :Unit = setNull(parameterIndex, sqlType)
	override def setNull(parameterIndex :Int, sqlType :Int) :Unit = {
		val tpe = try { JDBCType.valueOf(sqlType) } catch {
			case _ :IllegalArgumentException => NULL
		}
		set(parameterIndex, null, tpe)
	}
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int) :Unit = {
		val tpe = try { JDBCType.valueOf(targetSqlType) } catch {
			case _ :IllegalArgumentException => OTHER
		}
		set(parameterIndex, x, tpe)
	}
	override def setObject(parameterIndex :Int, x :Any) :Unit = set(parameterIndex, x, JDBCType.OTHER)
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int, scaleOrLength :Int) :Unit = setObject(parameterIndex, x, targetSqlType)
	override def setRef(parameterIndex :Int, x :Ref) :Unit = set(parameterIndex, x, REF)
	override def setRowId(parameterIndex :Int, x :RowId) :Unit = set(parameterIndex, x, ROWID)
	override def setShort(parameterIndex :Int, x :Short) :Unit = set(parameterIndex, x, SMALLINT)
	override def setString(parameterIndex :Int, x :String) :Unit = set(parameterIndex, x, VARCHAR)
	override def setSQLXML(parameterIndex :Int, xmlObject :SQLXML) :Unit = set(parameterIndex, xmlObject, JDBCType.SQLXML)
	override def setTime(parameterIndex :Int, x :Time) :Unit = set(parameterIndex, x, TIME)
	override def setTimestamp(parameterIndex :Int, x :Timestamp) :Unit = set(parameterIndex, x, TIMESTAMP)
	override def setTime(parameterIndex :Int, x :Time, cal :Calendar) :Unit = set(parameterIndex, x, TIME)
	override def setTimestamp(parameterIndex :Int, x :Timestamp, cal :Calendar) :Unit = set(parameterIndex, x, TIMESTAMP)
	override def setURL(parameterIndex :Int, x :URL) :Unit = set(parameterIndex, x, DATALINK)
	@nowarn
	override def setUnicodeStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = set(parameterIndex, x, LONGVARCHAR)


	override def getMetaData :ResultSetMetaData = unsupported
	override def getParameterMetaData :ParameterMetaData = unsupported


	override def toString :String = {
		val res = new StringBuilder
		res append '{'
		var i = 0;
		while (i < params) {
			val value = values(i)
			val tpe = types(i)
			if (i > 0) res append ", "
			if (value == null) res append '?' else res append value
			res append " :"
			if (tpe == null) res append '?' else res append tpe
			i += 1
		}
		res append '}'
		res.toString()
	}
}


