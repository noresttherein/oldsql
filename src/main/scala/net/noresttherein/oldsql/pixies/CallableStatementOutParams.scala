package net.noresttherein.oldsql.pixies

import java.io.{InputStream, Reader}
import java.{sql, util}
import java.math.BigInteger
import java.sql.{Blob, CallableStatement, Clob, Date, NClob, Ref, ResultSet, RowId, SQLFeatureNotSupportedException, SQLWarning, SQLXML, Time, Timestamp, Types}
import java.sql.ParameterMetaData.{parameterModeIn, parameterModeOut}
import java.util.Calendar

import scala.annotation.nowarn

//import scala.annotation.nowarn






/** A somewhat curious adapter of `CallableStatement` to `ResultSet` interface, presenting ''out'' parameters
  * of the former as columns of the latter. This allows direct use of
  * [[net.noresttherein.oldsql.schema.SQLReadForm SQLReadForm]]s for this purpose without expanding the interface.
  * Note that most other methods make little sense; where possible, they either virtualize a 1-row `ResultSet`
  * or are no-ops, but some throw `SQLFeatureNotSupportedException`. Only the accessors methods `getXxx` should
  * be used.
  * @param call the adapted stored procedure call with ''out'' parameters to read.
  * @param indices a mapping of input indices of virtual 'columns' to the indices of the corresponding ''out''
  *                parameters. It's length must be one greater than the number of mocked columns.
  *                It allows to hide ''in'' parameters, making ''out'' parameters appear consecutive
  *                (crucial with multi column forms) or create a view of the parameter list specifically for
  *                a particular form.
  * @author Marcin Mościcki
  */
private[oldsql] class CallableStatementOutParams private (val call :CallableStatement, indices :Array[Int])
	extends ResultSet
{
	def this(call :CallableStatement) =
		this(call, Array.tabulate(call.getParameterMetaData.getParameterCount + 1)(identity))


	private[this] var hasNext = true

	//not thread safe but c'mon, it won't be used by forms
	override def next() :Boolean = { val res = hasNext; hasNext = false; res }
	override def previous() :Boolean = { hasNext = true; true }
	override def beforeFirst() :Unit = hasNext = true
	override def first() :Boolean = { hasNext = true; true }
	override def last() :Boolean = { hasNext = true; true }
	override def afterLast() :Unit = hasNext = false

	override def isBeforeFirst = hasNext
	override def isFirst = hasNext
	override def isLast = hasNext
	override def isAfterLast = !hasNext
	override def getRow :Int = { if (hasNext) 1 else 0 }

	override def absolute(row :Int) = row match {
		case _ if row > 1 => hasNext = false; false
		case _ => hasNext = true; true
	}

	override def relative(rows :Int) =
		if (hasNext && rows <= 0) true
		else false

	override def getMetaData = call.getMetaData

	override def getCursorName = unsupported("getCursorName")
	override def findColumn(columnLabel :String) = unsupported(s"findColumn($columnLabel)")

	override def getWarnings :SQLWarning = call.getWarnings
	override def clearWarnings() :Unit = call.clearWarnings()

	override def close() :Unit = call.close()
	override def isClosed = !hasNext


	override def setFetchDirection(direction :Int) :Unit =
		if (direction != ResultSet.FETCH_FORWARD) unsupported(s"setFetchDirection($direction)")

	override def getFetchDirection :Int = ResultSet.FETCH_FORWARD
	override def getFetchSize :Int = 1
	override def setFetchSize(rows :Int) :Unit = ()

	override def getHoldability = ResultSet.CLOSE_CURSORS_AT_COMMIT
	override def getConcurrency = ResultSet.CONCUR_READ_ONLY
	override def getType :Int = ResultSet.TYPE_FORWARD_ONLY

	override def rowUpdated() = false
	override def rowInserted() = false
	override def rowDeleted() = false

	override def updateRow() :Unit = ()
	override def insertRow() :Unit = ()
	override def deleteRow() :Unit = unsupported("deleteRow")
	override def refreshRow() :Unit = ()
	override def cancelRowUpdates() :Unit = unsupported("cancelRowUpdates")
	override def moveToInsertRow() :Unit = ()
	override def moveToCurrentRow() :Unit = ()


	override def getStatement = call
	override def unwrap[T](iface :Class[T]) = call.unwrap(iface)
	override def isWrapperFor(iface :Class[_]) = call.isWrapperFor(iface)



	override def getArray(parameter :Int) = call.getArray(indices(parameter))
	override def getArray(parameterName :String) = call.getArray(parameterName)
	override def getAsciiStream(parameter :Int) :InputStream = new ClobStream(call.getClob(indices(parameter)))
	override def getAsciiStream(parameterName :String) :InputStream = new ClobStream(call.getClob(parameterName))
	override def getBigDecimal(parameter :Int) = call.getBigDecimal(indices(parameter))
	override def getBigDecimal(parameterName :String) = call.getBigDecimal(parameterName)
	override def getBinaryStream(parameter :Int) :InputStream = new BlobStream(call.getBlob(indices(parameter)))
	override def getBinaryStream(parameterName :String) :InputStream = new BlobStream(call.getBlob(parameterName))
	override def getBlob(parameter :Int) = call.getBlob(indices(parameter))
	override def getBlob(parameterName :String) = call.getBlob(parameterName)
	override def getBoolean(parameter :Int) = call.getBoolean(indices(parameter))
	override def getBoolean(parameterName :String) = call.getBoolean(parameterName)
	override def getByte(parameter :Int) = call.getByte(indices(parameter))
	override def getByte(parameterName :String) = call.getByte(parameterName)
	override def getBytes(parameterName :String) = call.getBytes(parameterName)
	override def getBytes(parameter :Int) = call.getBytes(indices(parameter))
	override def getCharacterStream(parameter :Int) :Reader = call.getCharacterStream(indices(parameter))
	override def getCharacterStream(parameterName :String) :Reader = call.getCharacterStream(parameterName)
	override def getClob(parameter :Int) = call.getClob(indices(parameter))
	override def getClob(parameterName :String) = call.getClob(parameterName)
	override def getDate(parameter :Int) = call.getDate(indices(parameter))
	override def getDate(parameter :Int, cal :Calendar) = call.getDate(indices(parameter), cal)
	override def getDate(parameterName :String) = call.getDate(parameterName)
	override def getDate(parameterName :String, cal :Calendar) = call.getDate(parameterName, cal)
	override def getDouble(parameter :Int) = call.getDouble(indices(parameter))
	override def getDouble(parameterName :String) = call.getDouble(parameterName)
	override def getFloat(parameter :Int) = call.getFloat(indices(parameter))
	override def getFloat(parameterName :String) = call.getFloat(parameterName)
	override def getInt(parameter :Int) = call.getInt(indices(parameter))
	override def getInt(parameterName :String) = call.getInt(parameterName)
	override def getLong(parameter :Int) = call.getLong(indices(parameter))
	override def getLong(parameterName :String) = call.getLong(parameterName)
	override def getNCharacterStream(parameter :Int) :Reader = call.getNCharacterStream(indices(parameter))
	override def getNCharacterStream(parameterName :String) :Reader = call.getNCharacterStream(parameterName)
	override def getNClob(parameter :Int) = call.getNClob(indices(parameter))
	override def getNClob(parameterName :String) = call.getNClob(parameterName)
	override def getNString(parameter :Int) = call.getNString(indices(parameter))
	override def getNString(parameterName :String) = call.getNString(parameterName)
	override def getRef(parameter :Int) = call.getRef(indices(parameter))
	override def getRef(parameterName :String) = call.getRef(parameterName)
	override def getRowId(parameter :Int) = call.getRowId(indices(parameter))
	override def getRowId(parameterName :String) = call.getRowId(parameterName)
	override def getShort(parameter :Int) = call.getShort(indices(parameter))
	override def getShort(parameterName :String) = call.getShort(parameterName)
	override def getSQLXML(column :Int) :SQLXML = call.getSQLXML(indices(column))
	override def getSQLXML(parameterName :String) :SQLXML = call.getSQLXML(parameterName)
	override def getString(parameter :Int) = call.getString(indices(parameter))
	override def getString(parameterName :String) = call.getString(parameterName)
	override def getTime(parameter :Int) = call.getTime(indices(parameter))
	override def getTime(parameter :Int, cal :Calendar) = call.getTime(indices(parameter), cal)
	override def getTime(parameterName :String) = call.getTime(parameterName)
	override def getTime(parameterName :String, cal :Calendar) = call.getTime(parameterName, cal)
	override def getObject[T](parameter :Int, `type` :Class[T]) :T = call.getObject(indices(parameter), `type`)
	override def getObject[T](parameterName :String, `type` :Class[T]) :T = call.getObject(parameterName, `type`)
	override def getTimestamp(parameter :Int) = call.getTimestamp(indices(parameter))
	override def getTimestamp(parameter :Int, cal :Calendar) = call.getTimestamp(indices(parameter), cal)
	override def getTimestamp(parameterName :String) = call.getTimestamp(parameterName)
	override def getTimestamp(parameterName :String, cal :Calendar) = call.getTimestamp(parameterName, cal)
	override def getObject(parameter :Int) = call.getObject(indices(parameter))
	override def getObject(parameter :Int, map :(util.Map[String, Class[_]])) = call.getObject(indices(parameter), map)
	override def getObject(parameterName :String) = call.getObject(parameterName)
	override def getObject(parameterName :String, map :util.Map[String, Class[_]]) = call.getObject(parameterName, map)
	override def getURL(parameter :Int) = call.getURL(indices(parameter))
	override def getURL(parameterName :String) = call.getURL(parameterName)

	override def getUnicodeStream(parameter :Int) = unsupported("getUnicodeStream")
	override def getUnicodeStream(parameterName :String) = unsupported("getUnicodeStream")

	override def getBigDecimal(parameterName :String, scale :Int) =
		try {
			java.math.BigDecimal.valueOf(call.getLong(parameterName), scale)
		} catch {
			case _ :Exception =>
				new java.math.BigDecimal(new BigInteger(call.getString(parameterName)), scale)
		}
	@nowarn
	override def getBigDecimal(parameter :Int, scale :Int) = call.getBigDecimal(indices(parameter), scale)


	override def wasNull() = call.wasNull()



	override def updateNull(parameter :Int) :Unit = call.setNull(indices(parameter), Types.OTHER)
	override def updateNull(parameterName :String) :Unit = call.setNull(parameterName, Types.OTHER)

	override def updateArray(parameter :Int, x :sql.Array) :Unit = call.setArray(indices(parameter), x)
	override def updateArray(parameterName :String, x :sql.Array) :Unit = unsupported(s"updateArray($parameterName)")
	override def updateAsciiStream(parameter :Int, x :InputStream) :Unit = call.setAsciiStream(indices(parameter), x)
	override def updateAsciiStream(parameterName :String, x :InputStream) :Unit = call.setAsciiStream(parameterName, x)
	override def updateAsciiStream(parameter :Int, x :InputStream, length :Int) :Unit = call.setAsciiStream(indices(parameter), x, length)
	override def updateAsciiStream(parameterName :String, x :InputStream, length :Long) :Unit = call.setAsciiStream(parameterName, x, length)
	override def updateAsciiStream(parameter :Int, x :InputStream, length :Long) :Unit = call.setAsciiStream(indices(parameter), x, length)
	override def updateAsciiStream(parameterName :String, x :InputStream, length :Int) :Unit = call.setAsciiStream(parameterName, x, length)
	override def updateBigDecimal(parameter :Int, x :java.math.BigDecimal) :Unit = call.setBigDecimal(indices(parameter), x)
	override def updateBigDecimal(parameterName :String, x :java.math.BigDecimal) :Unit = call.setBigDecimal(parameterName, x)
	override def updateBinaryStream(parameter :Int, x :InputStream) :Unit = call.setBinaryStream(indices(parameter), x)
	override def updateBinaryStream(parameterName :String, x :InputStream) :Unit = call.setBinaryStream(parameterName, x)
	override def updateBinaryStream(parameter :Int, x :InputStream, length :Int) :Unit = call.setBinaryStream(indices(parameter), x, length)
	override def updateBinaryStream(parameterName :String, x :InputStream, length :Int) :Unit = call.setBinaryStream(parameterName, x, length)
	override def updateBinaryStream(parameter :Int, x :InputStream, length :Long) :Unit = call.setBinaryStream(indices(parameter), x, length)
	override def updateBinaryStream(parameterName :String, x :InputStream, length :Long) :Unit = call.setBinaryStream(parameterName, x, length)
	override def updateBlob(parameter :Int, x :Blob) :Unit = call.setBlob(indices(parameter), x)
	override def updateBlob(parameterName :String, x :Blob) :Unit = call.setBlob(parameterName, x)
	override def updateBlob(parameter :Int, x :InputStream) :Unit = call.setBlob(indices(parameter), x)
	override def updateBlob(parameterName :String, x :InputStream) :Unit = call.setBlob(parameterName, x)
	override def updateBlob(parameter :Int, x :InputStream, length :Long) :Unit = call.setBlob(indices(parameter), x, length)
	override def updateBlob(parameterName :String, x :InputStream, length :Long) :Unit = call.setBlob(parameterName, x, length)
	override def updateBoolean(parameter :Int, x :Boolean) :Unit = call.setBoolean(indices(parameter), x)
	override def updateBoolean(parameterName :String, x :Boolean) :Unit = call.setBoolean(parameterName, x)
	override def updateByte(parameter :Int, x :Byte) :Unit = call.setByte(indices(parameter), x)
	override def updateByte(parameterName :String, x :Byte) :Unit = call.setByte(parameterName, x)
	override def updateBytes(parameter :Int, x :Array[Byte]) :Unit = call.setBytes(indices(parameter), x)
	override def updateBytes(parameterName :String, x :Array[Byte]) :Unit = call.setBytes(parameterName, x)
	override def updateCharacterStream(parameter :Int, x :Reader) :Unit = call.setCharacterStream(indices(parameter), x)
	override def updateCharacterStream(parameterName :String, x :Reader) :Unit = call.setCharacterStream(parameterName, x)
	override def updateCharacterStream(parameter :Int, x :Reader, length :Int) :Unit = call.setCharacterStream(indices(parameter), x, length)
	override def updateCharacterStream(parameterName :String, x :Reader, length :Int) :Unit = call.setCharacterStream(parameterName, x, length)
	override def updateCharacterStream(parameter :Int, x :Reader, length :Long) :Unit = call.setCharacterStream(indices(parameter), x, length)
	override def updateCharacterStream(parameterName :String, x :Reader, length :Long) :Unit = call.setCharacterStream(parameterName, x, length)
	override def updateClob(parameter :Int, x :Clob) :Unit = call.setClob(indices(parameter), x)
	override def updateClob(parameterName :String, x :Clob) :Unit = call.setClob(parameterName, x)
	override def updateClob(parameter :Int, x :Reader) :Unit = call.setClob(indices(parameter), x)
	override def updateClob(parameterName :String, x :Reader) :Unit = call.setClob(parameterName, x)
	override def updateClob(parameter :Int, x :Reader, length :Long) :Unit = call.setClob(indices(parameter), x, length)
	override def updateClob(parameterName :String, x :Reader, length :Long) :Unit = call.setClob(parameterName, x, length)
	override def updateDate(parameter :Int, x :Date) :Unit = call.setDate(indices(parameter), x)
	override def updateDate(parameterName :String, x :Date) :Unit = call.setDate(parameterName, x)
	override def updateDouble(parameter :Int, x :Double) :Unit = call.setDouble(indices(parameter), x)
	override def updateDouble(parameterName :String, x :Double) :Unit = call.setDouble(parameterName, x)
	override def updateFloat(parameter :Int, x :Float) :Unit = call.setFloat(indices(parameter), x)
	override def updateFloat(parameterName :String, x :Float) :Unit = call.setFloat(parameterName, x)
	override def updateInt(parameter :Int, x :Int) :Unit = call.setInt(indices(parameter), x)
	override def updateInt(parameterName :String, x :Int) :Unit = call.setInt(parameterName, x)
	override def updateLong(parameter :Int, x :Long) :Unit = call.setLong(indices(parameter), x)
	override def updateLong(parameterName :String, x :Long) :Unit = call.setLong(parameterName, x)
	override def updateNCharacterStream(parameter :Int, x :Reader) :Unit = call.setNCharacterStream(indices(parameter), x)
	override def updateNCharacterStream(parameterName :String, x :Reader) :Unit = call.setNCharacterStream(parameterName, x)
	override def updateNCharacterStream(parameter :Int, x :Reader, length :Long) :Unit = call.setNCharacterStream(indices(parameter), x, length)
	override def updateNCharacterStream(parameterName :String, x :Reader, length :Long) :Unit = call.setNCharacterStream(parameterName, x, length)
	override def updateNClob(parameter :Int, x :NClob) :Unit = call.setNClob(indices(parameter), x)
	override def updateNClob(parameterName :String, x :NClob) :Unit = call.setNClob(parameterName, x)
	override def updateNClob(parameter :Int, x :Reader) :Unit = call.setNClob(indices(parameter), x)
	override def updateNClob(parameterName :String, x :Reader) :Unit = call.setNClob(parameterName, x)
	override def updateNClob(parameter :Int, x :Reader, length :Long) :Unit = call.setNCharacterStream(indices(parameter), x, length)
	override def updateNClob(parameterName :String, x :Reader, length :Long) :Unit = call.setNClob(parameterName, x, length)
	override def updateNString(parameter :Int, x :String) :Unit = call.setNString(indices(parameter), x)
	override def updateNString(parameterName :String, x :String) :Unit = call.setNString(parameterName, x)
	override def updateRef(parameter :Int, x :Ref) :Unit = call.setRef(indices(parameter), x)
	override def updateRef(parameterName :String, x :Ref) :Unit = unsupported(s"updateRef($parameterName)")
	override def updateRowId(parameter :Int, x :RowId) :Unit = call.setRowId(indices(parameter), x)
	override def updateRowId(parameterName :String, x :RowId) :Unit = call.setRowId(parameterName, x)
	override def updateShort(parameter :Int, x :Short) :Unit = call.setShort(indices(parameter), x)
	override def updateShort(parameterName :String, x :Short) :Unit = call.setShort(parameterName, x)
	override def updateString(parameter :Int, x :String) :Unit = call.setString(indices(parameter), x)
	override def updateString(parameterName :String, x :String) :Unit = call.setString(parameterName, x)
	override def updateSQLXML(parameter :Int, x :SQLXML) :Unit = call.setSQLXML(indices(parameter), x)
	override def updateSQLXML(parameterName :String, x :SQLXML) :Unit = call.setSQLXML(parameterName, x)
	override def updateTime(parameter :Int, x :Time) :Unit = call.setTime(indices(parameter), x)
	override def updateTime(parameterName :String, x :Time) :Unit = call.setTime(parameterName, x)
	override def updateTimestamp(parameter :Int, x :Timestamp) :Unit = call.setTimestamp(indices(parameter), x)
	override def updateTimestamp(parameterName :String, x :Timestamp) :Unit = call.setTimestamp(parameterName, x)
	override def updateObject(parameter :Int, x :Any, scaleOrLength :Int) :Unit = call.setObject(indices(parameter), x, Types.OTHER, scaleOrLength)
	override def updateObject(parameterName :String, x :Any, scaleOrLength :Int) :Unit = call.setObject(parameterName, x, Types.OTHER, scaleOrLength)
	override def updateObject(parameter :Int, x :Any) :Unit = call.setObject(indices(parameter), x)
	override def updateObject(parameterName :String, x :Any) :Unit = call.setObject(parameterName, x)



	def unsupported(method :String) :Nothing =
		throw new SQLFeatureNotSupportedException(s"Operation `$method` unsupported by a CallableStatementAdapter $this.")


	private class ClobStream(parent :Clob) extends InputStream {
		private[this] val stream = parent.getAsciiStream
		override def read() = stream.read()
		override def close() :Unit = { stream.close(); parent.free() }
	}
	private class BlobStream(parent :Blob) extends InputStream {
		private[this] val stream = parent.getBinaryStream
		override def read() = stream.read()
		override def close() :Unit = { stream.close(); parent.free() }
	}



	override def toString :String = s"ResultSet($call)"

}







object CallableStatementOutParams {

	/** A `ResultSet` implementation which presents ''out'' parameters of a stored procedure called by `call`
	  * as columns of the result set. The result set will have `indexMapping.length-1` columns, and
	  * `n-th` column will map to parameter `indexMapping(n)`.
	  */
	def apply(call :CallableStatement, indexMapping :Array[Int]) :CallableStatementOutParams =
		new CallableStatementOutParams(call, indexMapping)

	/** A mock `ResultSet` where `n-th` column is the `n-th` parameter of the result set.
	  * It is the caller's responsibility to ensure only columns mapping to ''out'' parameters are accessed.
	  */
	def apply(call :CallableStatement) :CallableStatementOutParams =
		call.getParameterMetaData.getParameterCount match {
			case n if n < identity.length => new CallableStatementOutParams(call, identity)
			case n => new CallableStatementOutParams(call, Array.tabulate(n + 1)(Predef.identity))
		}

	/** A mock `ResultSet` consisting of `n` columns, where `n` is the number of ''out'' parameters of the
	  * given `CallableStatement`. This filters out ''in'' parameters, essentially presenting the `i`-th ''out''
	  * parameter (starting from `1` and not counting ''in'' parameters) as `i-k`, where `k` is the number of ''in''
	  * parameters preceding that parameter.
	  */
	def filter(call :CallableStatement) :CallableStatementOutParams = {
		val meta = call.getParameterMetaData
		var i = 0; val params = meta.getParameterCount
		val build = Array.newBuilder[Int]; build sizeHint params + 1
		var mode = parameterModeIn
		while (i <= params) { //the array has a dummy at 0 so we can use argument positions directly as indices
			while (i < params && mode != parameterModeIn && mode != parameterModeOut) {
				i += 1
				mode = meta.getParameterMode(i)
			}
			if (mode == parameterModeIn | mode == parameterModeOut)
				build += i
		}
		new CallableStatementOutParams(call, build.result())
	}



	private[this] val identity = Array.tabulate[Int](100)(Predef.identity)
}