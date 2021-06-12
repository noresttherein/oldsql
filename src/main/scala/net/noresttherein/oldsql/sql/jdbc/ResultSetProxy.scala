package net.noresttherein.oldsql.sql.jdbc

import java.io.{InputStream, Reader}
import java.sql.{Blob, Clob, Date, NClob, Ref, ResultSet, RowId, SQLType, SQLXML, Time, Timestamp}
import java.{sql, util}
import java.util.Calendar

import scala.annotation.nowarn






/**
  * @author Marcin Mościcki
  */
private[sql] class ResultSetProxy(rs :ResultSet) extends ResultSet {
	override def next() = rs.next()
	override def close() = rs.close()
	override def wasNull() = rs.wasNull()

	override def findColumn(columnLabel :String) = rs.findColumn(columnLabel)

	override def getArray(columnIndex :Int) = rs.getArray(columnIndex)
	override def getArray(columnLabel :String) = rs.getArray(columnLabel)
	override def getAsciiStream(columnIndex :Int) = rs.getAsciiStream(columnIndex)
	override def getAsciiStream(columnLabel :String) = rs.getAsciiStream(columnLabel)
	@nowarn override def getBigDecimal(columnIndex :Int, scale :Int) = rs.getBigDecimal(columnIndex, scale)
	@nowarn override def getBigDecimal(columnLabel :String, scale :Int) = rs.getBigDecimal(columnLabel, scale)
	override def getBigDecimal(columnIndex :Int) = rs.getBigDecimal(columnIndex)
	override def getBigDecimal(columnLabel :String) = rs.getBigDecimal(columnLabel)
	override def getBinaryStream(columnIndex :Int) = rs.getBinaryStream(columnIndex)
	override def getBinaryStream(columnLabel :String) = rs.getBinaryStream(columnLabel)
	override def getBlob(columnIndex :Int) = rs.getBlob(columnIndex)
	override def getBlob(columnLabel :String) = rs.getBlob(columnLabel)
	override def getBoolean(columnIndex :Int) = rs.getBoolean(columnIndex)
	override def getBoolean(columnLabel :String) = rs.getBoolean(columnLabel)
	override def getByte(columnIndex :Int) = rs.getByte(columnIndex)
	override def getByte(columnLabel :String) = rs.getByte(columnLabel)
	override def getBytes(columnIndex :Int) = rs.getBytes(columnIndex)
	override def getBytes(columnLabel :String) = rs.getBytes(columnLabel)
	override def getCharacterStream(columnIndex :Int) = rs.getCharacterStream(columnIndex)
	override def getCharacterStream(columnLabel :String) = rs.getCharacterStream(columnLabel)
	override def getClob(columnIndex :Int) = rs.getClob(columnIndex)
	override def getClob(columnLabel :String) = rs.getClob(columnLabel)
	override def getDate(columnIndex :Int) = rs.getDate(columnIndex)
	override def getDate(columnLabel :String) = rs.getDate(columnLabel)
	override def getDate(columnIndex :Int, cal :Calendar) = rs.getDate(columnIndex, cal)
	override def getDate(columnLabel :String, cal :Calendar) = rs.getDate(columnLabel, cal)
	override def getDouble(columnIndex :Int) = rs.getDouble(columnIndex)
	override def getDouble(columnLabel :String) = rs.getDouble(columnLabel)
	override def getFloat(columnIndex :Int) = rs.getFloat(columnIndex)
	override def getFloat(columnLabel :String) = rs.getFloat(columnLabel)
	override def getInt(columnIndex :Int) = rs.getInt(columnIndex)
	override def getInt(columnLabel :String) = rs.getInt(columnLabel)
	override def getLong(columnLabel :String) = rs.getLong(columnLabel)
	override def getLong(columnIndex :Int) = rs.getLong(columnIndex)
	override def getNCharacterStream(columnIndex :Int) = rs.getNCharacterStream(columnIndex)
	override def getNCharacterStream(columnLabel :String) = rs.getNCharacterStream(columnLabel)
	override def getNClob(columnIndex :Int) = rs.getNClob(columnIndex)
	override def getNClob(columnLabel :String) = rs.getNClob(columnLabel)
	override def getNString(columnIndex :Int) = rs.getNString(columnIndex)
	override def getNString(columnLabel :String) = rs.getNString(columnLabel)
	override def getObject(columnIndex :Int) = rs.getObject(columnIndex)
	override def getObject(columnLabel :String) = rs.getObject(columnLabel)
	override def getObject(columnIndex :Int, map :util.Map[String, Class[_]]) = rs.getObject(columnIndex, map)
	override def getObject(columnLabel :String, map :util.Map[String, Class[_]]) = rs.getObject(columnLabel, map)
	override def getObject[T](columnIndex :Int, `type` :Class[T]) = rs.getObject(columnIndex, `type`)
	override def getObject[T](columnLabel :String, `type` :Class[T]) = rs.getObject(columnLabel, `type`)
	override def getRef(columnIndex :Int) = rs.getRef(columnIndex)
	override def getRef(columnLabel :String) = rs.getRef(columnLabel)
	override def getRowId(columnIndex :Int) = rs.getRowId(columnIndex)
	override def getRowId(columnLabel :String) = rs.getRowId(columnLabel)
	override def getShort(columnIndex :Int) = rs.getShort(columnIndex)
	override def getShort(columnLabel :String) = rs.getShort(columnLabel)
	override def getString(columnLabel :String) = rs.getString(columnLabel)
	override def getString(columnIndex :Int) = rs.getString(columnIndex)
	override def getSQLXML(columnIndex :Int) = rs.getSQLXML(columnIndex)
	override def getSQLXML(columnLabel :String) = rs.getSQLXML(columnLabel)
	override def getTime(columnIndex :Int) = rs.getTime(columnIndex)
	override def getTime(columnLabel :String) = rs.getTime(columnLabel)
	override def getTime(columnIndex :Int, cal :Calendar) = rs.getTime(columnIndex, cal)
	override def getTime(columnLabel :String, cal :Calendar) = rs.getTime(columnLabel, cal)
	override def getTimestamp(columnIndex :Int) = rs.getTimestamp(columnIndex)
	override def getTimestamp(columnLabel :String) = rs.getTimestamp(columnLabel)
	override def getTimestamp(columnIndex :Int, cal :Calendar) = rs.getTimestamp(columnIndex, cal)
	override def getTimestamp(columnLabel :String, cal :Calendar) = rs.getTimestamp(columnLabel, cal)
	@nowarn override def getUnicodeStream(columnIndex :Int) = rs.getUnicodeStream(columnIndex)
	@nowarn override def getUnicodeStream(columnLabel :String) = rs.getUnicodeStream(columnLabel)
	override def getURL(columnIndex :Int) = rs.getURL(columnIndex)
	override def getURL(columnLabel :String) = rs.getURL(columnLabel)


	override def updateArray(columnIndex :Int, x :sql.Array) = rs.updateArray(columnIndex, x)
	override def updateArray(columnLabel :String, x :sql.Array) = rs.updateArray(columnLabel, x)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Int) = rs.updateAsciiStream(columnIndex, x, length)
	override def updateAsciiStream(columnLabel :String, x :InputStream, length :Int) = rs.updateAsciiStream(columnLabel, x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Long) = rs.updateAsciiStream(columnIndex, x, length)
	override def updateAsciiStream(columnLabel :String, x :InputStream, length :Long) = rs.updateAsciiStream(columnLabel, x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream) = rs.updateAsciiStream(columnIndex, x)
	override def updateAsciiStream(columnLabel :String, x :InputStream) = rs.updateAsciiStream(columnLabel, x)
	override def updateBigDecimal(columnIndex :Int, x :java.math.BigDecimal) = rs.updateBigDecimal(columnIndex, x)
	override def updateBigDecimal(columnLabel :String, x :java.math.BigDecimal) = rs.updateBigDecimal(columnLabel, x)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Int) = rs.updateBinaryStream(columnIndex, x, length)
	override def updateBinaryStream(columnLabel :String, x :InputStream, length :Int) = rs.updateBinaryStream(columnLabel, x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Long) = rs.updateBinaryStream(columnIndex, x, length)
	override def updateBinaryStream(columnLabel :String, x :InputStream, length :Long) = rs.updateBinaryStream(columnLabel, x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream) = rs.updateBinaryStream(columnIndex, x)
	override def updateBinaryStream(columnLabel :String, x :InputStream) = rs.updateBinaryStream(columnLabel, x)
	override def updateBlob(columnIndex :Int, x :Blob) = rs.updateBlob(columnIndex, x)
	override def updateBlob(columnLabel :String, x :Blob) = rs.updateBlob(columnLabel, x)
	override def updateBlob(columnIndex :Int, inputStream :InputStream, length :Long) = rs.updateBlob(columnIndex, inputStream, length)
	override def updateBlob(columnLabel :String, inputStream :InputStream, length :Long) = rs.updateBlob(columnLabel, inputStream, length)
	override def updateBlob(columnIndex :Int, inputStream :InputStream) = rs.updateBlob(columnIndex, inputStream)
	override def updateBlob(columnLabel :String, inputStream :InputStream) = rs.updateBlob(columnLabel, inputStream)
	override def updateBoolean(columnIndex :Int, x :Boolean) = rs.updateBoolean(columnIndex, x)
	override def updateBoolean(columnLabel :String, x :Boolean) = rs.updateBoolean(columnLabel, x)
	override def updateByte(columnIndex :Int, x :Byte) = rs.updateByte(columnIndex, x)
	override def updateByte(columnLabel :String, x :Byte) = rs.updateByte(columnLabel, x)
	override def updateBytes(columnIndex :Int, x :Array[Byte]) = rs.updateBytes(columnIndex, x)
	override def updateBytes(columnLabel :String, x :Array[Byte]) = rs.updateBytes(columnLabel, x)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Int) = rs.updateCharacterStream(columnIndex, x, length)
	override def updateCharacterStream(columnLabel :String, reader :Reader, length :Int) = rs.updateCharacterStream(columnLabel, reader, length)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Long) = rs.updateCharacterStream(columnIndex, x, length)
	override def updateCharacterStream(columnLabel :String, reader :Reader, length :Long) = rs.updateCharacterStream(columnLabel, reader, length)
	override def updateCharacterStream(columnLabel :String, reader :Reader) = rs.updateCharacterStream(columnLabel, reader)
	override def updateCharacterStream(columnIndex :Int, x :Reader) = rs.updateCharacterStream(columnIndex, x)
	override def updateClob(columnIndex :Int, x :Clob) = rs.updateClob(columnIndex, x)
	override def updateClob(columnLabel :String, x :Clob) = rs.updateClob(columnLabel, x)
	override def updateClob(columnIndex :Int, reader :Reader, length :Long) = rs.updateClob(columnIndex, reader, length)
	override def updateClob(columnLabel :String, reader :Reader, length :Long) = rs.updateClob(columnLabel, reader, length)
	override def updateClob(columnIndex :Int, reader :Reader) = rs.updateClob(columnIndex, reader)
	override def updateClob(columnLabel :String, reader :Reader) = rs.updateClob(columnLabel, reader)
	override def updateDate(columnIndex :Int, x :Date) = rs.updateDate(columnIndex, x)
	override def updateDate(columnLabel :String, x :Date) = rs.updateDate(columnLabel, x)
	override def updateDouble(columnIndex :Int, x :Double) = rs.updateDouble(columnIndex, x)
	override def updateDouble(columnLabel :String, x :Double) = rs.updateDouble(columnLabel, x)
	override def updateFloat(columnIndex :Int, x :Float) = rs.updateFloat(columnIndex, x)
	override def updateFloat(columnLabel :String, x :Float) = rs.updateFloat(columnLabel, x)
	override def updateInt(columnIndex :Int, x :Int) = rs.updateInt(columnIndex, x)
	override def updateInt(columnLabel :String, x :Int) = rs.updateInt(columnLabel, x)
	override def updateLong(columnIndex :Int, x :Long) = rs.updateLong(columnIndex, x)
	override def updateLong(columnLabel :String, x :Long) = rs.updateLong(columnLabel, x)
	override def updateNCharacterStream(columnIndex :Int, x :Reader, length :Long) = rs.updateNCharacterStream(columnIndex, x, length)
	override def updateNCharacterStream(columnLabel :String, reader :Reader, length :Long) = rs.updateNCharacterStream(columnLabel, reader, length)
	override def updateNCharacterStream(columnIndex :Int, x :Reader) = rs.updateNCharacterStream(columnIndex, x)
	override def updateNCharacterStream(columnLabel :String, reader :Reader) = rs.updateNCharacterStream(columnLabel, reader)
	override def updateNClob(columnIndex :Int, nClob :NClob) = rs.updateNClob(columnIndex, nClob)
	override def updateNClob(columnLabel :String, nClob :NClob) = rs.updateNClob(columnLabel, nClob)
	override def updateNClob(columnIndex :Int, reader :Reader, length :Long) = rs.updateNClob(columnIndex, reader, length)
	override def updateNClob(columnLabel :String, reader :Reader, length :Long) = rs.updateNClob(columnLabel, reader, length)
	override def updateNClob(columnIndex :Int, reader :Reader) = rs.updateNClob(columnIndex, reader)
	override def updateNClob(columnLabel :String, reader :Reader) = rs.updateNClob(columnLabel, reader)
	override def updateNString(columnIndex :Int, nString :String) = rs.updateNString(columnIndex, nString)
	override def updateNString(columnLabel :String, nString :String) = rs.updateNString(columnLabel, nString)
	override def updateNull(columnIndex :Int) = rs.updateNull(columnIndex)
	override def updateNull(columnLabel :String) = rs.updateNull(columnLabel)
	override def updateRef(columnIndex :Int, x :Ref) = rs.updateRef(columnIndex, x)
	override def updateRef(columnLabel :String, x :Ref) = rs.updateRef(columnLabel, x)
	override def updateRowId(columnIndex :Int, x :RowId) = rs.updateRowId(columnIndex, x)
	override def updateRowId(columnLabel :String, x :RowId) = rs.updateRowId(columnLabel, x)
	override def updateShort(columnIndex :Int, x :Short) = rs.updateShort(columnIndex, x)
	override def updateShort(columnLabel :String, x :Short) = rs.updateShort(columnLabel, x)
	override def updateString(columnIndex :Int, x :String) = rs.updateString(columnIndex, x)
	override def updateString(columnLabel :String, x :String) = rs.updateString(columnLabel, x)
	override def updateTime(columnIndex :Int, x :Time) = rs.updateTime(columnIndex, x)
	override def updateTimestamp(columnIndex :Int, x :Timestamp) = rs.updateTimestamp(columnIndex, x)
	override def updateObject(columnIndex :Int, x :Any, scaleOrLength :Int) = rs.updateObject(columnIndex, x, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any) = rs.updateObject(columnIndex, x)
	override def updateTime(columnLabel :String, x :Time) = rs.updateTime(columnLabel, x)
	override def updateTimestamp(columnLabel :String, x :Timestamp) = rs.updateTimestamp(columnLabel, x)
	override def updateObject(columnLabel :String, x :Any, scaleOrLength :Int) = rs.updateObject(columnLabel, x, scaleOrLength)
	override def updateObject(columnLabel :String, x :Any) = rs.updateObject(columnLabel, x)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType, scaleOrLength :Int) = rs.updateObject(columnIndex, x, targetSqlType, scaleOrLength)
	override def updateObject(columnLabel :String, x :Any, targetSqlType :SQLType, scaleOrLength :Int) = rs.updateObject(columnLabel, x, targetSqlType, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType) = rs.updateObject(columnIndex, x, targetSqlType)
	override def updateObject(columnLabel :String, x :Any, targetSqlType :SQLType) = rs.updateObject(columnLabel, x, targetSqlType)
	override def updateSQLXML(columnIndex :Int, xmlObject :SQLXML) = rs.updateSQLXML(columnIndex, xmlObject)
	override def updateSQLXML(columnLabel :String, xmlObject :SQLXML) = rs.updateSQLXML(columnLabel, xmlObject)




	override def getStatement = rs.getStatement
	override def getMetaData = rs.getMetaData
	override def getCursorName = rs.getCursorName

	override def getConcurrency = rs.getConcurrency
	override def getHoldability = rs.getHoldability
	override def getType = rs.getType
	override def setFetchDirection(direction :Int) = rs.setFetchDirection(direction)
	override def getFetchDirection = rs.getFetchDirection
	override def setFetchSize(rows :Int) = rs.setFetchSize(rows)
	override def getFetchSize = rs.getFetchSize

	override def isBeforeFirst = rs.isBeforeFirst
	override def isAfterLast = rs.isAfterLast
	override def isFirst = rs.isFirst
	override def isLast = rs.isLast
	override def getRow = rs.getRow

	override def beforeFirst() = rs.beforeFirst()
	override def afterLast() = rs.afterLast()
	override def first() = rs.first()
	override def last() = rs.last()
	override def absolute(row :Int) = rs.absolute(row)
	override def relative(rows :Int) = rs.relative(rows)
	override def previous() = rs.previous()
	override def moveToInsertRow() = rs.moveToInsertRow()
	override def moveToCurrentRow() = rs.moveToCurrentRow()

	override def cancelRowUpdates() = rs.cancelRowUpdates()

	override def rowUpdated() = rs.rowUpdated()
	override def rowInserted() = rs.rowInserted()
	override def rowDeleted() = rs.rowDeleted()

	override def insertRow() = rs.insertRow()
	override def updateRow() = rs.updateRow()
	override def deleteRow() = rs.deleteRow()
	override def refreshRow() = rs.refreshRow()

	override def getWarnings = rs.getWarnings
	override def clearWarnings() = rs.clearWarnings()

	override def isClosed = rs.isClosed


	override def isWrapperFor(iface :Class[_]) = iface.isInstance(this) || rs.isWrapperFor(iface)
	override def unwrap[T](iface :Class[T]) =
		if (iface.isInstance(this)) this.asInstanceOf[T] else rs.unwrap(iface)

}
