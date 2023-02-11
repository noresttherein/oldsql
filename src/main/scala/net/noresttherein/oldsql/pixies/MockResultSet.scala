package net.noresttherein.oldsql.pixies

import java.io.{InputStream, Reader}
import java.sql.{Blob, Clob, Date, NClob, Ref, ResultSet, RowId, SQLException, SQLFeatureNotSupportedException, SQLXML, Time, Timestamp}
import java.{sql, util}
import java.util.Calendar






/**
  * @author Marcin Mościcki
  */
private[oldsql] class MockResultSet(data :IndexedSeq[Any], columnNames :Map[String, Int]) extends ResultSet {
	def this(data :IndexedSeq[Any], columnNames :IndexedSeq[String]) =
		this(data, columnNames.view.zipWithIndex.toMap)

	def this(data :IndexedSeq[Any]) = this(data, Map.empty[String, Int])

	if (columnNames.size != 0 && columnNames.size != data.size)
		throw new IllegalArgumentException(
			"Column names number does not match the number of column values: " +
				columnNames + " (" + columnNames.size + ") vs " + data + " (" + data.size + ")."
		)

	private def unsupported = throw new SQLFeatureNotSupportedException

	private[this] var lastReadColumn = -1

	@inline private def getAs[X](column :Int) :X = {
		lastReadColumn = column
		data(column).asInstanceOf[X]
	}
	@inline private def getAs[X](column :String) :X = columnNames.getOrElse(column, -1) match {
		case -1 => throw new SQLException("No column named " + column + " in " + this + ".")
		case n => getAs(n)
	}

	override def next() = unsupported
	override def close() = unsupported

	override def wasNull() =
		if (lastReadColumn < 0)
			throw new SQLException("No columns values read from " + this + ".")
		else
			data(lastReadColumn) == null

	override def findColumn(columnLabel :String) = columnNames.getOrElse(columnLabel, -1) match {
		case -1 => throw new SQLException("No column " + columnLabel + " in " + this + ".")
		case n => n
	}

	override def getString(columnIndex :Int) = getAs(columnIndex)
	override def getBoolean(columnIndex :Int) = getAs(columnIndex)
	override def getByte(columnIndex :Int) = getAs(columnIndex)
	override def getShort(columnIndex :Int) = getAs(columnIndex)
	override def getInt(columnIndex :Int) = getAs(columnIndex)
	override def getLong(columnIndex :Int) = getAs(columnIndex)
	override def getFloat(columnIndex :Int) = getAs(columnIndex)
	override def getDouble(columnIndex :Int) = getAs(columnIndex)
	override def getBigDecimal(columnIndex :Int, scale :Int) = getAs[java.math.BigDecimal](columnIndex).setScale(scale)
	override def getBytes(columnIndex :Int) = getAs(columnIndex)
	override def getDate(columnIndex :Int) = getAs(columnIndex)
	override def getTime(columnIndex :Int) = getAs(columnIndex)
	override def getTimestamp(columnIndex :Int) = getAs(columnIndex)
	override def getAsciiStream(columnIndex :Int) = getAs(columnIndex)
	override def getUnicodeStream(columnIndex :Int) = getAs(columnIndex)
	override def getBinaryStream(columnIndex :Int) = getAs(columnIndex)
	override def getObject(columnIndex :Int) = getAs(columnIndex)
	override def getBigDecimal(columnIndex :Int) = getAs(columnIndex)
	override def getCharacterStream(columnIndex :Int) = getAs(columnIndex)
	override def getRef(columnIndex :Int) = getAs(columnIndex)
	override def getBlob(columnIndex :Int) = getAs(columnIndex)
	override def getClob(columnIndex :Int) = getAs(columnIndex)
	override def getArray(columnIndex :Int) = getAs(columnIndex)
	override def getObject(columnIndex :Int, map :util.Map[String, Class[_]]) = getAs(columnIndex)
	override def getDate(columnIndex :Int, cal :Calendar) = getAs(columnIndex)
	override def getTime(columnIndex :Int, cal :Calendar) = getAs(columnIndex)
	override def getTimestamp(columnIndex :Int, cal :Calendar) = getAs(columnIndex)
	override def getURL(columnIndex :Int) = getAs(columnIndex)
	override def getNClob(columnIndex :Int) = getAs(columnIndex)
	override def getSQLXML(columnIndex :Int) = getAs(columnIndex)
	override def updateSQLXML(columnIndex :Int, xmlObject :SQLXML) = getAs(columnIndex)
	override def getNString(columnIndex :Int) = getAs(columnIndex)
	override def getNCharacterStream(columnIndex :Int) = getAs(columnIndex)
	override def getObject[T](columnIndex :Int, `type` :Class[T]) = getAs(columnIndex)

	override def getString(columnLabel :String) = getAs(columnLabel)
	override def getBoolean(columnLabel :String) = getAs(columnLabel)
	override def getByte(columnLabel :String) = getAs(columnLabel)
	override def getShort(columnLabel :String) = getAs(columnLabel)
	override def getInt(columnLabel :String) = getAs(columnLabel)
	override def getLong(columnLabel :String) = getAs(columnLabel)
	override def getFloat(columnLabel :String) = getAs(columnLabel)
	override def getDouble(columnLabel :String) = getAs(columnLabel)
	override def getBigDecimal(columnLabel :String, scale :Int) = getAs(columnLabel)
	override def getBytes(columnLabel :String) = getAs(columnLabel)
	override def getDate(columnLabel :String) = getAs(columnLabel)
	override def getTime(columnLabel :String) = getAs(columnLabel)
	override def getTimestamp(columnLabel :String) = getAs(columnLabel)
	override def getAsciiStream(columnLabel :String) = getAs(columnLabel)
	override def getUnicodeStream(columnLabel :String) = getAs(columnLabel)
	override def getBinaryStream(columnLabel :String) = getAs(columnLabel)
	override def getObject(columnLabel :String) = getAs(columnLabel)
	override def getBigDecimal(columnLabel :String) = getAs(columnLabel)
	override def getCharacterStream(columnLabel :String) = getAs(columnLabel)
	override def getRef(columnLabel :String) = getAs(columnLabel)
	override def getBlob(columnLabel :String) = getAs(columnLabel)
	override def getClob(columnLabel :String) = getAs(columnLabel)
	override def getObject(columnLabel :String, map :util.Map[String, Class[_]]) = getAs(columnLabel)
	override def getArray(columnLabel :String) = getAs(columnLabel)
	override def getDate(columnLabel :String, cal :Calendar) = getAs(columnLabel)
	override def getTime(columnLabel :String, cal :Calendar) = getAs(columnLabel)
	override def getTimestamp(columnLabel :String, cal :Calendar) = getAs(columnLabel)
	override def getURL(columnLabel :String) = getAs(columnLabel)
	override def getNClob(columnLabel :String) = getAs(columnLabel)
	override def getSQLXML(columnLabel :String) = getAs(columnLabel)
	override def updateSQLXML(columnLabel :String, xmlObject :SQLXML) = getAs(columnLabel)
	override def getNString(columnLabel :String) = getAs(columnLabel)
	override def getNCharacterStream(columnLabel :String) = getAs(columnLabel)
	override def getObject[T](columnLabel :String, `type` :Class[T]) = getAs(columnLabel)

	override def getStatement = null
	override def getMetaData = unsupported
	override def getWarnings = null
	override def clearWarnings() = ()
	override def getCursorName = unsupported

	override def isBeforeFirst = false
	override def isAfterLast = false
	override def isFirst = true
	override def isLast = true

	override def beforeFirst() = unsupported
	override def afterLast() = unsupported
	override def first() = true
	override def last() = false

	override def getRow = 1
	override def absolute (row :Int) = unsupported
	override def relative(rows :Int) = unsupported
	override def previous() = unsupported
	override def setFetchDirection(direction :Int) = unsupported
	override def getFetchDirection = ResultSet.FETCH_UNKNOWN
	override def setFetchSize(rows :Int) = unsupported
	override def getFetchSize = unsupported
	override def getType = ResultSet.TYPE_FORWARD_ONLY
	override def getConcurrency = ResultSet.CONCUR_READ_ONLY
	override def rowUpdated() = false
	override def rowInserted() = false
	override def rowDeleted() = false
	override def getHoldability = ResultSet.CLOSE_CURSORS_AT_COMMIT
	override def isClosed = false

	override def refreshRow() = ()
	override def cancelRowUpdates() = ()
	override def moveToCurrentRow() = ()
	override def insertRow() = unsupported
	override def updateRow() = unsupported
	override def deleteRow() = unsupported
	override def moveToInsertRow() = unsupported

	override def unwrap[T](iface :Class[T]) = unsupported
	override def isWrapperFor(iface :Class[_]) = false

	override def updateNull(columnIndex :Int) = unsupported
	override def updateBoolean(columnIndex :Int, x :Boolean) = unsupported
	override def updateByte(columnIndex :Int, x :Byte) = unsupported
	override def updateShort(columnIndex :Int, x :Short) = unsupported
	override def updateInt(columnIndex :Int, x :Int) = unsupported
	override def updateLong(columnIndex :Int, x :Long) = unsupported
	override def updateFloat(columnIndex :Int, x :Float) = unsupported
	override def updateDouble(columnIndex :Int, x :Double) = unsupported
	override def updateBigDecimal(columnIndex :Int, x :java.math.BigDecimal) = unsupported
	override def updateString(columnIndex :Int, x :String) = unsupported
	override def updateBytes(columnIndex :Int, x :Array[Byte]) = unsupported
	override def updateDate(columnIndex :Int, x :Date) = unsupported
 	override def updateTime(columnIndex :Int, x :Time) = unsupported
	override def updateTimestamp(columnIndex :Int, x :Timestamp) = unsupported
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Int) = unsupported
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Int) = unsupported
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Int) = unsupported
	override def updateObject(columnIndex :Int, x :Any, scaleOrLength :Int) = unsupported
	override def updateObject(columnIndex :Int, x :Any) = unsupported
	override def updateNull(columnLabel :String) = unsupported
	override def updateBoolean(columnLabel :String, x :Boolean) = unsupported
	override def updateByte(columnLabel :String, x :Byte) = unsupported
	override def updateShort(columnLabel :String, x :Short) = unsupported
	override def updateInt(columnLabel :String, x :Int) = unsupported
	override def updateLong(columnLabel :String, x :Long) = unsupported
	override def updateFloat(columnLabel :String, x :Float) = unsupported
	override def updateDouble(columnLabel :String, x :Double) = unsupported
	override def updateBigDecimal(columnLabel :String, x :java.math.BigDecimal) = unsupported
	override def updateString(columnLabel :String, x :String) = unsupported
	override def updateBytes(columnLabel :String, x :Array[Byte]) = unsupported
	override def updateDate(columnLabel :String, x :Date) = unsupported
	override def updateTime(columnLabel :String, x :Time) = unsupported
	override def updateTimestamp(columnLabel :String, x :Timestamp) = unsupported
	override def updateAsciiStream(columnLabel :String, x :InputStream, length :Int) = unsupported
	override def updateBinaryStream(columnLabel :String, x :InputStream, length :Int) = unsupported
	override def updateCharacterStream(columnLabel :String, reader :Reader, length :Int) = unsupported
	override def updateObject(columnLabel :String, x :Any, scaleOrLength :Int) = unsupported
	override def updateObject(columnLabel :String, x :Any) = unsupported
	override def updateRef(columnIndex :Int, x :Ref) = unsupported
	override def updateRef(columnLabel :String, x :Ref) = unsupported
	override def updateBlob(columnIndex :Int, x :Blob) = unsupported
	override def updateBlob(columnLabel :String, x :Blob) = unsupported
	override def updateClob(columnIndex :Int, x :Clob) = unsupported
	override def updateClob(columnLabel :String, x :Clob) = unsupported
	override def updateArray(columnIndex :Int, x :sql.Array) = unsupported
	override def updateArray(columnLabel :String, x :sql.Array) = unsupported
	override def getRowId(columnIndex :Int) = unsupported
	override def getRowId(columnLabel :String) = unsupported
	override def updateRowId(columnIndex :Int, x :RowId) = unsupported
	override def updateRowId(columnLabel :String, x :RowId) = unsupported
	override def updateNString(columnIndex :Int, nString :String) = unsupported
	override def updateNString(columnLabel :String, nString :String) = unsupported
	override def updateNClob(columnIndex :Int, nClob :NClob) = unsupported
	override def updateNClob(columnLabel :String, nClob :NClob) = unsupported
	override def updateNCharacterStream(columnIndex :Int, x :Reader, length :Long) = unsupported
	override def updateNCharacterStream(columnLabel :String, reader :Reader, length :Long) = unsupported
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Long) = unsupported
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Long) = unsupported
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Long) = unsupported
	override def updateAsciiStream(columnLabel :String, x :InputStream, length :Long) = unsupported
	override def updateBinaryStream(columnLabel :String, x :InputStream, length :Long) = unsupported
	override def updateCharacterStream(columnLabel :String, reader :Reader, length :Long) = unsupported
	override def updateBlob(columnIndex :Int, inputStream :InputStream, length :Long) = unsupported
	override def updateBlob(columnLabel :String, inputStream :InputStream, length :Long) = unsupported
	override def updateClob(columnIndex :Int, reader :Reader, length :Long) = unsupported
	override def updateClob(columnLabel :String, reader :Reader, length :Long) = unsupported
	override def updateNClob(columnIndex :Int, reader :Reader, length :Long) = unsupported
	override def updateNClob(columnLabel :String, reader :Reader, length :Long) = unsupported
	override def updateNCharacterStream(columnIndex :Int, x :Reader) = unsupported
	override def updateNCharacterStream(columnLabel :String, reader :Reader) = unsupported
	override def updateAsciiStream(columnIndex :Int, x :InputStream) = unsupported
	override def updateBinaryStream(columnIndex :Int, x :InputStream) = unsupported
	override def updateCharacterStream(columnIndex :Int, x :Reader) = unsupported
 	override def updateAsciiStream(columnLabel :String, x :InputStream) = unsupported
	override def updateBinaryStream(columnLabel :String, x :InputStream) = unsupported
	override def updateCharacterStream(columnLabel :String, reader :Reader) = unsupported
	override def updateBlob(columnIndex :Int, inputStream :InputStream) = unsupported
	override def updateBlob(columnLabel :String, inputStream :InputStream) = unsupported
	override def updateClob(columnIndex :Int, reader :Reader) = unsupported
	override def updateClob(columnLabel :String, reader :Reader) = unsupported
	override def updateNClob(columnIndex :Int, reader :Reader) = unsupported
	override def updateNClob(columnLabel :String, reader :Reader) = unsupported
}
