package net.noresttherein.oldsql.pixies

import java.io.{InputStream, Reader}
import java.net.URL
import java.sql.{Blob, CallableStatement, Clob, Date, JDBCType, NClob, ParameterMetaData, PreparedStatement, Ref, ResultSetMetaData, RowId, SQLType, SQLXML, Time, Timestamp}
import java.{sql, util}
import java.util.Calendar

import scala.annotation.nowarn

import net.noresttherein.oldsql.sql.jdbc.{CallableStatementProxy, PreparedStatementProxy}






private[pixies] class InjectionPreparedStatement(stmt :PreparedStatement, startOffset :Int, indices :RearrangedIndexing,
                                                 setNotExposedToNull :Boolean = false)
	extends PreparedStatementProxy
{
	protected override def statement :PreparedStatement = stmt
	@inline protected final def underlying(index :Int) :Int = startOffset + indices(index)

	if (setNotExposedToNull && !indices.isSurjection) {
		var i = indices.underlyingColumnCount
		while (i > 0) {
			if (!indices.isCovered(i))
				stmt.setNull(startOffset + i, JDBCType.NULL.ordinal)
			i -= 1
		}
	}

	override def setArray(parameterIndex :Int, x :sql.Array) :Unit = stmt.setArray(underlying(parameterIndex), x)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = stmt.setAsciiStream(underlying(parameterIndex), x)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = stmt.setAsciiStream(underlying(parameterIndex), x, length)
	override def setAsciiStream(parameterIndex :Int, x :InputStream) :Unit = stmt.setAsciiStream(underlying(parameterIndex), x)
	override def setBigDecimal(parameterIndex :Int, x :java.math.BigDecimal) :Unit = stmt.setBigDecimal(underlying(parameterIndex), x)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = stmt.setBinaryStream(underlying(parameterIndex), x, length)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = stmt.setBinaryStream(underlying(parameterIndex), x, length)
	override def setBinaryStream(parameterIndex :Int, x :InputStream) :Unit = stmt.setBinaryStream(underlying(parameterIndex), x)
	override def setBoolean(parameterIndex :Int, x :Boolean) :Unit = stmt.setBoolean(underlying(parameterIndex), x)
	override def setByte(parameterIndex :Int, x :Byte) :Unit = stmt.setByte(underlying(parameterIndex), x)
	override def setBytes(parameterIndex :Int, x :Array[Byte]) :Unit = stmt.setBytes(underlying(parameterIndex), x)
	override def setBlob(parameterIndex :Int, x :Blob) :Unit = stmt.setBlob(underlying(parameterIndex), x)
	override def setBlob(parameterIndex :Int, inputStream :InputStream, length :Long) :Unit = stmt.setBlob(underlying(parameterIndex), inputStream, length)
	override def setBlob(parameterIndex :Int, inputStream :InputStream) :Unit = stmt.setBlob(underlying(parameterIndex), inputStream)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Int) :Unit = stmt.setCharacterStream(underlying(parameterIndex), reader, length)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Long) :Unit = stmt.setCharacterStream(underlying(parameterIndex), reader, length)
	override def setCharacterStream(parameterIndex :Int, reader :Reader) :Unit = stmt.setCharacterStream(underlying(parameterIndex), reader)
	override def setClob(parameterIndex :Int, x :Clob) :Unit = stmt.setClob(underlying(parameterIndex), x)
	override def setClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = stmt.setClob(underlying(parameterIndex), reader, length)
	override def setClob(parameterIndex :Int, reader :Reader) :Unit = stmt.setClob(underlying(parameterIndex), reader)
	override def setDate(parameterIndex :Int, x :Date, cal :Calendar) :Unit = stmt.setDate(underlying(parameterIndex), x)
	override def setDate(parameterIndex :Int, x :Date) :Unit = stmt.setDate(underlying(parameterIndex), x)
	override def setDouble(parameterIndex :Int, x :Double) :Unit = stmt.setDouble(underlying(parameterIndex), x)
	override def setFloat(parameterIndex :Int, x :Float) :Unit = stmt.setFloat(underlying(parameterIndex), x)
	override def setInt(parameterIndex :Int, x :Int) :Unit = stmt.setInt(underlying(parameterIndex), x)
	override def setLong(parameterIndex :Int, x :Long) :Unit = stmt.setLong(underlying(parameterIndex), x)
	override def setNCharacterStream(parameterIndex :Int, value :Reader, length :Long) :Unit = stmt.setNCharacterStream(underlying(parameterIndex), value, length)
	override def setNCharacterStream(parameterIndex :Int, value :Reader) :Unit = stmt.setNCharacterStream(underlying(parameterIndex), value)
	override def setNClob(parameterIndex :Int, value :NClob) :Unit = stmt.setNClob(underlying(parameterIndex), value)
	override def setNClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = stmt.setNClob(underlying(parameterIndex), reader, length)
	override def setNClob(parameterIndex :Int, reader :Reader) :Unit = stmt.setNClob(underlying(parameterIndex), reader)
	override def setNString(parameterIndex :Int, value :String) :Unit = stmt.setNString(underlying(parameterIndex), value)
	override def setNull(parameterIndex :Int, sqlType :Int, typeName :String) :Unit = stmt.setNull(underlying(parameterIndex), sqlType, typeName)
	override def setNull(parameterIndex :Int, sqlType :Int) :Unit = stmt.setNull(underlying(parameterIndex), sqlType)
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int) :Unit = stmt.setObject(underlying(parameterIndex), x, targetSqlType)
	override def setObject(parameterIndex :Int, x :Any) :Unit = stmt.setObject(underlying(parameterIndex), x)
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int, scaleOrLength :Int) :Unit = stmt.setObject(underlying(parameterIndex), x, targetSqlType, scaleOrLength)
	override def setRef(parameterIndex :Int, x :Ref) :Unit = stmt.setRef(underlying(parameterIndex), x)
	override def setRowId(parameterIndex :Int, x :RowId) :Unit = stmt.setRowId(underlying(parameterIndex), x)
	override def setShort(parameterIndex :Int, x :Short) :Unit = stmt.setShort(underlying(parameterIndex), x)
	override def setString(parameterIndex :Int, x :String) :Unit = stmt.setString(underlying(parameterIndex), x)
	override def setSQLXML(parameterIndex :Int, xmlObject :SQLXML) :Unit = stmt.setSQLXML(underlying(parameterIndex), xmlObject)
	override def setTime(parameterIndex :Int, x :Time) :Unit = stmt.setTime(underlying(parameterIndex), x)
	override def setTimestamp(parameterIndex :Int, x :Timestamp) :Unit = stmt.setTimestamp(underlying(parameterIndex), x)
	override def setTime(parameterIndex :Int, x :Time, cal :Calendar) :Unit = stmt.setTime(underlying(parameterIndex), x, cal)
	override def setTimestamp(parameterIndex :Int, x :Timestamp, cal :Calendar) :Unit = stmt.setTimestamp(underlying(parameterIndex), x, cal)
	override def setURL(parameterIndex :Int, x :URL) :Unit = stmt.setURL(underlying(parameterIndex), x)
	@nowarn
	override def setUnicodeStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = stmt.setUnicodeStream(underlying(parameterIndex), x, length)


	override def getMetaData :ResultSetMetaData = stmt.getMetaData
	override def getParameterMetaData :ParameterMetaData = stmt.getParameterMetaData

	override def toString :String =
		"InjectionPreparedStatement(" + statement + ", " + startOffset + " + " + indices + ")"
}



private[pixies] class RearrangedPreparedStatement(stmt :PreparedStatement, startOffset :Int, indices :RearrangedIndexing,
                                                  setNotExposedToNull :Boolean = false)
	extends PreparedStatementProxy
{
	import indices.{isMapped => has}
	protected override def statement :PreparedStatement = stmt
	@inline private final def underlying(index :Int) :Int = startOffset + indices(index)

	if (setNotExposedToNull && !indices.isSurjection) {
		var i = indices.underlyingColumnCount
		while (i > 0) {
			if (!indices.isCovered(i))
				stmt.setNull(startOffset + i, JDBCType.NULL.ordinal)
			i -= 1
		}
	}

	override def setArray(parameterIndex :Int, x :sql.Array) :Unit = if (has(parameterIndex)) stmt.setArray(underlying(parameterIndex), x)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = if (has(parameterIndex)) stmt.setAsciiStream(underlying(parameterIndex), x)
	override def setAsciiStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = if (has(parameterIndex)) stmt.setAsciiStream(underlying(parameterIndex), x, length)
	override def setAsciiStream(parameterIndex :Int, x :InputStream) :Unit = if (has(parameterIndex)) stmt.setAsciiStream(underlying(parameterIndex), x)
	override def setBigDecimal(parameterIndex :Int, x :java.math.BigDecimal) :Unit = if (has(parameterIndex)) stmt.setBigDecimal(underlying(parameterIndex), x)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = if (has(parameterIndex)) stmt.setBinaryStream(underlying(parameterIndex), x, length)
	override def setBinaryStream(parameterIndex :Int, x :InputStream, length :Long) :Unit = if (has(parameterIndex)) stmt.setBinaryStream(underlying(parameterIndex), x, length)
	override def setBinaryStream(parameterIndex :Int, x :InputStream) :Unit = if (has(parameterIndex)) stmt.setBinaryStream(underlying(parameterIndex), x)
	override def setBoolean(parameterIndex :Int, x :Boolean) :Unit = if (has(parameterIndex)) stmt.setBoolean(underlying(parameterIndex), x)
	override def setByte(parameterIndex :Int, x :Byte) :Unit = if (has(parameterIndex)) stmt.setByte(underlying(parameterIndex), x)
	override def setBytes(parameterIndex :Int, x :Array[Byte]) :Unit = if (has(parameterIndex)) stmt.setBytes(underlying(parameterIndex), x)
	override def setBlob(parameterIndex :Int, x :Blob) :Unit = if (has(parameterIndex)) stmt.setBlob(underlying(parameterIndex), x)
	override def setBlob(parameterIndex :Int, inputStream :InputStream, length :Long) :Unit = if (has(parameterIndex)) stmt.setBlob(underlying(parameterIndex), inputStream, length)
	override def setBlob(parameterIndex :Int, inputStream :InputStream) :Unit = if (has(parameterIndex)) stmt.setBlob(underlying(parameterIndex), inputStream)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Int) :Unit = if (has(parameterIndex)) stmt.setCharacterStream(underlying(parameterIndex), reader, length)
	override def setCharacterStream(parameterIndex :Int, reader :Reader, length :Long) :Unit = if (has(parameterIndex)) stmt.setCharacterStream(underlying(parameterIndex), reader, length)
	override def setCharacterStream(parameterIndex :Int, reader :Reader) :Unit = if (has(parameterIndex)) stmt.setCharacterStream(underlying(parameterIndex), reader)
	override def setClob(parameterIndex :Int, x :Clob) :Unit = if (has(parameterIndex)) stmt.setClob(underlying(parameterIndex), x)
	override def setClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = if (has(parameterIndex)) stmt.setClob(underlying(parameterIndex), reader, length)
	override def setClob(parameterIndex :Int, reader :Reader) :Unit = if (has(parameterIndex)) stmt.setClob(underlying(parameterIndex), reader)
	override def setDate(parameterIndex :Int, x :Date, cal :Calendar) :Unit = if (has(parameterIndex)) stmt.setDate(underlying(parameterIndex), x)
	override def setDate(parameterIndex :Int, x :Date) :Unit = if (has(parameterIndex)) stmt.setDate(underlying(parameterIndex), x)
	override def setDouble(parameterIndex :Int, x :Double) :Unit = if (has(parameterIndex)) stmt.setDouble(underlying(parameterIndex), x)
	override def setFloat(parameterIndex :Int, x :Float) :Unit = if (has(parameterIndex)) stmt.setFloat(underlying(parameterIndex), x)
	override def setInt(parameterIndex :Int, x :Int) :Unit = if (has(parameterIndex)) stmt.setInt(underlying(parameterIndex), x)
	override def setLong(parameterIndex :Int, x :Long) :Unit = if (has(parameterIndex)) stmt.setLong(underlying(parameterIndex), x)
	override def setNCharacterStream(parameterIndex :Int, value :Reader, length :Long) :Unit = if (has(parameterIndex)) stmt.setNCharacterStream(underlying(parameterIndex), value, length)
	override def setNCharacterStream(parameterIndex :Int, value :Reader) :Unit = if (has(parameterIndex)) stmt.setNCharacterStream(underlying(parameterIndex), value)
	override def setNClob(parameterIndex :Int, value :NClob) :Unit = if (has(parameterIndex)) stmt.setNClob(underlying(parameterIndex), value)
	override def setNClob(parameterIndex :Int, reader :Reader, length :Long) :Unit = if (has(parameterIndex)) stmt.setNClob(underlying(parameterIndex), reader, length)
	override def setNClob(parameterIndex :Int, reader :Reader) :Unit = if (has(parameterIndex)) stmt.setNClob(underlying(parameterIndex), reader)
	override def setNString(parameterIndex :Int, value :String) :Unit = if (has(parameterIndex)) stmt.setNString(underlying(parameterIndex), value)
	override def setNull(parameterIndex :Int, sqlType :Int, typeName :String) :Unit = if (has(parameterIndex)) stmt.setNull(underlying(parameterIndex), sqlType, typeName)
	override def setNull(parameterIndex :Int, sqlType :Int) :Unit = if (has(parameterIndex)) stmt.setNull(underlying(parameterIndex), sqlType)
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int) :Unit = if (has(parameterIndex)) stmt.setObject(underlying(parameterIndex), x, targetSqlType)
	override def setObject(parameterIndex :Int, x :Any) :Unit = if (has(parameterIndex)) stmt.setObject(underlying(parameterIndex), x)
	override def setObject(parameterIndex :Int, x :Any, targetSqlType :Int, scaleOrLength :Int) :Unit = if (has(parameterIndex)) stmt.setObject(underlying(parameterIndex), x, targetSqlType, scaleOrLength)
	override def setRef(parameterIndex :Int, x :Ref) :Unit = if (has(parameterIndex)) stmt.setRef(underlying(parameterIndex), x)
	override def setRowId(parameterIndex :Int, x :RowId) :Unit = if (has(parameterIndex)) stmt.setRowId(underlying(parameterIndex), x)
	override def setShort(parameterIndex :Int, x :Short) :Unit = if (has(parameterIndex)) stmt.setShort(underlying(parameterIndex), x)
	override def setString(parameterIndex :Int, x :String) :Unit = if (has(parameterIndex)) stmt.setString(underlying(parameterIndex), x)
	override def setSQLXML(parameterIndex :Int, xmlObject :SQLXML) :Unit = if (has(parameterIndex)) stmt.setSQLXML(underlying(parameterIndex), xmlObject)
	override def setTime(parameterIndex :Int, x :Time) :Unit = if (has(parameterIndex)) stmt.setTime(underlying(parameterIndex), x)
	override def setTimestamp(parameterIndex :Int, x :Timestamp) :Unit = if (has(parameterIndex)) stmt.setTimestamp(underlying(parameterIndex), x)
	override def setTime(parameterIndex :Int, x :Time, cal :Calendar) :Unit = if (has(parameterIndex)) stmt.setTime(underlying(parameterIndex), x, cal)
	override def setTimestamp(parameterIndex :Int, x :Timestamp, cal :Calendar) :Unit = if (has(parameterIndex)) stmt.setTimestamp(underlying(parameterIndex), x, cal)
	override def setURL(parameterIndex :Int, x :URL) :Unit = if (has(parameterIndex)) stmt.setURL(underlying(parameterIndex), x)
	@nowarn
	override def setUnicodeStream(parameterIndex :Int, x :InputStream, length :Int) :Unit = if (has(parameterIndex)) stmt.setUnicodeStream(underlying(parameterIndex), x, length)


	override def getMetaData :ResultSetMetaData = stmt.getMetaData
	override def getParameterMetaData :ParameterMetaData = stmt.getParameterMetaData

	override def toString :String =
		"RearrangedPreparedStatement(" + statement + ", " + startOffset + " + " + indices + ")"
}



private[oldsql] object RearrangedPreparedStatement {
	def apply(stmt :PreparedStatement, startIndex :Int, indexing :RearrangedIndexing, setUnmappedToNull :Boolean = false)
			:PreparedStatement =
		if (indexing.isInjection) new InjectionPreparedStatement(stmt, startIndex - 1, indexing, setUnmappedToNull)
		else new RearrangedPreparedStatement(stmt, startIndex - 1, indexing, setUnmappedToNull)

	def apply(stmt :PreparedStatement, indexing :RearrangedIndexing) :PreparedStatement =
		RearrangedPreparedStatement(stmt, 1, indexing)
}






//While technically extending InjectionPreparedStatement, it is highly unlikely to be used in that capacity,
// as out parameters are set by read forms, not write forms (which set statement parameters).
private[pixies] class InjectionCallableStatement private[pixies](stmt :CallableStatement,
                                                                 startOffset :Int, indexing :RearrangedIndexing)
	extends InjectionPreparedStatement(stmt, startOffset, indexing) with CallableStatementProxy
{
	protected override def statement :CallableStatement = stmt

	override def registerOutParameter(parameterIndex :Int, sqlType :Int) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType)
	override def registerOutParameter(parameterIndex :Int, sqlType :Int, scale :Int) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType, scale)
	override def registerOutParameter(parameterIndex :Int, sqlType :Int, typeName :String) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType, typeName)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType, scale :Int) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType, scale)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType, typeName :String) :Unit = stmt.registerOutParameter(underlying(parameterIndex), sqlType, typeName)

	override def getArray(parameterIndex :Int) :sql.Array = stmt.getArray(underlying(parameterIndex))
	@nowarn
	override def getBigDecimal(parameterIndex :Int, scale :Int) :java.math.BigDecimal = stmt.getBigDecimal(underlying(parameterIndex), scale)
	override def getBigDecimal(parameterIndex :Int) :java.math.BigDecimal = stmt.getBigDecimal(underlying(parameterIndex))
	override def getBoolean(parameterIndex :Int) :Boolean = stmt.getBoolean(underlying(parameterIndex))
	override def getByte(parameterIndex :Int) :Byte = stmt.getByte(underlying(parameterIndex))
	override def getBytes(parameterIndex :Int) :Array[Byte] = stmt.getBytes(underlying(parameterIndex))
	override def getBlob(parameterIndex :Int) :Blob = stmt.getBlob(underlying(parameterIndex))
	override def getCharacterStream(parameterIndex :Int) :Reader = stmt.getCharacterStream(underlying(parameterIndex))
	override def getClob(parameterIndex :Int) :Clob = stmt.getClob(underlying(parameterIndex))
	override def getDate(parameterIndex :Int, cal :Calendar) :Date = stmt.getDate(underlying(parameterIndex), cal)
	override def getDate(parameterIndex :Int) :Date = stmt.getDate(underlying(parameterIndex))
	override def getDouble(parameterIndex :Int) :Double = stmt.getDouble(underlying(parameterIndex))
	override def getFloat(parameterIndex :Int) :Float = stmt.getFloat(underlying(parameterIndex))
	override def getInt(parameterIndex :Int) :Int = stmt.getInt(underlying(parameterIndex))
	override def getLong(parameterIndex :Int) :Long = stmt.getLong(underlying(parameterIndex))
	override def getNClob(parameterIndex :Int) :NClob = stmt.getNClob(underlying(parameterIndex))
	override def getNCharacterStream(parameterIndex :Int) :Reader = stmt.getNCharacterStream(underlying(parameterIndex))
	override def getNString(parameterIndex :Int) :String = stmt.getNString(underlying(parameterIndex))
	override def getObject(parameterIndex :Int) :AnyRef = stmt.getObject(underlying(parameterIndex))
	override def getObject(parameterIndex :Int, map :util.Map[String, Class[_]]) :AnyRef = stmt.getObject(underlying(parameterIndex), map)
	override def getObject[T](parameterIndex :Int, `type` :Class[T]) :T = stmt.getObject(underlying(parameterIndex), `type`)
	override def getRef(parameterIndex :Int) :Ref = stmt.getRef(underlying(parameterIndex))
	override def getRowId(parameterIndex :Int) :RowId = stmt.getRowId(underlying(parameterIndex))
	override def getShort(parameterIndex :Int) :Short = stmt.getShort(underlying(parameterIndex))
	override def getString(parameterIndex :Int) :String = stmt.getString(underlying(parameterIndex))
	override def getSQLXML(parameterIndex :Int) :SQLXML = stmt.getSQLXML(underlying(parameterIndex))
	override def getTime(parameterIndex :Int) :Time = stmt.getTime(underlying(parameterIndex))
	override def getTime(parameterIndex :Int, cal :Calendar) :Time = stmt.getTime(underlying(parameterIndex), cal)
	override def getTimestamp(parameterIndex :Int) :Timestamp = stmt.getTimestamp(underlying(parameterIndex))
	override def getTimestamp(parameterIndex :Int, cal :Calendar) :Timestamp = stmt.getTimestamp(underlying(parameterIndex), cal)
	override def getURL(parameterIndex :Int) :URL = stmt.getURL(underlying(parameterIndex))

	override def toString :String = "InjectionCallableStatement(" + statement + ", " + startOffset + " + " + indexing + ")"
}



private[oldsql] class RearrangedCallableStatement private (stmt :CallableStatement,
                                                           startOffset :Int, indices :RearrangedIndexing)
	extends RearrangedPreparedStatement(stmt, startOffset, indices) with CallableStatementProxy
{
	import indices.{isMapped => has}
	protected override def statement :CallableStatement = stmt
	private[this] var wasCovered = true
	
	@inline private final def underlying(index :Int) :Int = { 
		wasCovered = true 
		startOffset + indices(index)
	}
	@inline private def returnNull = { wasCovered = false; null }


	override def wasNull :Boolean = !wasCovered || stmt.wasNull

	override def registerOutParameter(parameterIndex :Int, sqlType :Int) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType)
	override def registerOutParameter(parameterIndex :Int, sqlType :Int, scale :Int) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType, scale)
	override def registerOutParameter(parameterIndex :Int, sqlType :Int, typeName :String) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType, typeName)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType, scale :Int) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType, scale)
	override def registerOutParameter(parameterIndex :Int, sqlType :SQLType, typeName :String) :Unit = if (!has(parameterIndex)) returnNull else stmt.registerOutParameter(underlying(parameterIndex), sqlType, typeName)

	override def getArray(parameterIndex :Int) :sql.Array = if (!has(parameterIndex)) returnNull else stmt.getArray(underlying(parameterIndex))
	@nowarn
	override def getBigDecimal(parameterIndex :Int, scale :Int) :java.math.BigDecimal = if (!has(parameterIndex)) returnNull else stmt.getBigDecimal(underlying(parameterIndex), scale)
	override def getBigDecimal(parameterIndex :Int) :java.math.BigDecimal = if (!has(parameterIndex)) returnNull else stmt.getBigDecimal(underlying(parameterIndex))
	override def getBoolean(parameterIndex :Int) :Boolean = if (!has(parameterIndex)) { wasCovered = false; false } else stmt.getBoolean(underlying(parameterIndex))
	override def getByte(parameterIndex :Int) :Byte = if (!has(parameterIndex)) { wasCovered = false; 0 } else stmt.getByte(underlying(parameterIndex))
	override def getBytes(parameterIndex :Int) :Array[Byte] = if (!has(parameterIndex)) returnNull else stmt.getBytes(underlying(parameterIndex))
	override def getBlob(parameterIndex :Int) :Blob = if (!has(parameterIndex)) returnNull else stmt.getBlob(underlying(parameterIndex))
	override def getCharacterStream(parameterIndex :Int) :Reader = if (!has(parameterIndex)) returnNull else stmt.getCharacterStream(underlying(parameterIndex))
	override def getClob(parameterIndex :Int) :Clob = if (!has(parameterIndex)) returnNull else stmt.getClob(underlying(parameterIndex))
	override def getDate(parameterIndex :Int, cal :Calendar) :Date = if (!has(parameterIndex)) returnNull else stmt.getDate(underlying(parameterIndex), cal)
	override def getDate(parameterIndex :Int) :Date = if (!has(parameterIndex)) returnNull else stmt.getDate(underlying(parameterIndex))
	override def getDouble(parameterIndex :Int) :Double = if (!has(parameterIndex)) { wasCovered = false; 0.0 } else stmt.getDouble(underlying(parameterIndex))
	override def getFloat(parameterIndex :Int) :Float = if (!has(parameterIndex)) { wasCovered = false; 0.0f } else stmt.getFloat(underlying(parameterIndex))
	override def getInt(parameterIndex :Int) :Int = if (!has(parameterIndex)) { wasCovered = false; 0 } else stmt.getInt(underlying(parameterIndex))
	override def getLong(parameterIndex :Int) :Long = if (!has(parameterIndex)) { wasCovered = false; 0L } else stmt.getLong(underlying(parameterIndex))
	override def getNClob(parameterIndex :Int) :NClob = if (!has(parameterIndex)) returnNull else stmt.getNClob(underlying(parameterIndex))
	override def getNCharacterStream(parameterIndex :Int) :Reader = if (!has(parameterIndex)) returnNull else stmt.getNCharacterStream(underlying(parameterIndex))
	override def getNString(parameterIndex :Int) :String = if (!has(parameterIndex)) returnNull else stmt.getNString(underlying(parameterIndex))
	override def getObject(parameterIndex :Int) :AnyRef = if (!has(parameterIndex)) returnNull else stmt.getObject(underlying(parameterIndex))
	override def getObject(parameterIndex :Int, map :util.Map[String, Class[_]]) :AnyRef = if (!has(parameterIndex)) returnNull else stmt.getObject(underlying(parameterIndex), map)
	override def getObject[T](parameterIndex :Int, `type` :Class[T]) :T = if (!has(parameterIndex)) returnNull.asInstanceOf[T] else stmt.getObject(underlying(parameterIndex), `type`)
	override def getRef(parameterIndex :Int) :Ref = if (!has(parameterIndex)) returnNull else stmt.getRef(underlying(parameterIndex))
	override def getRowId(parameterIndex :Int) :RowId = if (!has(parameterIndex)) returnNull else stmt.getRowId(underlying(parameterIndex))
	override def getShort(parameterIndex :Int) :Short = if (!has(parameterIndex)) { wasCovered = false; 0 } else stmt.getShort(underlying(parameterIndex))
	override def getString(parameterIndex :Int) :String = if (!has(parameterIndex)) returnNull else stmt.getString(underlying(parameterIndex))
	override def getSQLXML(parameterIndex :Int) :SQLXML = if (!has(parameterIndex)) returnNull else stmt.getSQLXML(underlying(parameterIndex))
	override def getTime(parameterIndex :Int) :Time = if (!has(parameterIndex)) returnNull else stmt.getTime(underlying(parameterIndex))
	override def getTime(parameterIndex :Int, cal :Calendar) :Time = if (!has(parameterIndex)) returnNull else stmt.getTime(underlying(parameterIndex), cal)
	override def getTimestamp(parameterIndex :Int) :Timestamp = if (!has(parameterIndex)) returnNull else stmt.getTimestamp(underlying(parameterIndex))
	override def getTimestamp(parameterIndex :Int, cal :Calendar) :Timestamp = if (!has(parameterIndex)) returnNull else stmt.getTimestamp(underlying(parameterIndex), cal)
	override def getURL(parameterIndex :Int) :URL = if (!has(parameterIndex)) returnNull else stmt.getURL(underlying(parameterIndex))

	override def toString :String = "RearrangedCallableStatement(" + statement + ", " + startOffset + " + " + indices + ")"
}



private[oldsql] object RearrangedCallableStatement {
	def apply(stmt :CallableStatement, startIndex :Int, indexing :RearrangedIndexing) :CallableStatement =
		if (indexing.isInjection) new InjectionCallableStatement(stmt, startIndex - 1, indexing)
		else new RearrangedCallableStatement(stmt, startIndex - 1, indexing)

	def apply(stmt :CallableStatement, indexing :RearrangedIndexing) :CallableStatement =
		RearrangedCallableStatement(stmt, 1, indexing)

//	def sliced(stmt :CallableStatement, startIndex :Int, gaps :IndexedSeq[(Int, Int)], indexRange :Int) : CallableStatement =
//		RearrangedCallableStatement(stmt, startIndex, RearrangedIndexing.sliced(gaps, indexRange))
//
//	def sliced(stmt :CallableStatement, gaps :IndexedSeq[(Int, Int)], indexRange :Int) :CallableStatement =
//		RearrangedCallableStatement(stmt, 1, RearrangedIndexing.sliced(gaps, indexRange))
//
//	def permutation(stmt :CallableStatement, startIndex :Int, permutation :IndexedSeq[Int]) :CallableStatement =
//		RearrangedCallableStatement(stmt, startIndex, RearrangedIndexing.permutation(permutation))
//
//	def permutation(stmt :CallableStatement, permutation :IndexedSeq[Int]) :CallableStatement =
//		RearrangedCallableStatement(stmt, 1, RearrangedIndexing.permutation(permutation))
//
//	def inverse(stmt :CallableStatement, permutation :IndexedSeq[Int]) :CallableStatement =
//		RearrangedCallableStatement(stmt, 1, RearrangedIndexing.inverse(permutation))
}
