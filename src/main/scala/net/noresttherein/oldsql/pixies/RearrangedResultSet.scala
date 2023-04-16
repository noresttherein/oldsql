package net.noresttherein.oldsql.pixies

import java.io.{InputStream, Reader}
import java.sql.{Blob, CallableStatement, Clob, Date, NClob, ParameterMetaData, PreparedStatement, Ref, ResultSet, ResultSetMetaData, RowId, SQLException, SQLType, SQLXML, Time, Timestamp}
import java.{sql, util}
import java.net.URL
import java.util.Calendar

import scala.annotation.nowarn

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.pixies.Rearrangement.ComposedIndexing
import net.noresttherein.oldsql.pixies.RearrangedResultSet.RearrangedResultSetMetaData
import net.noresttherein.oldsql.sql.jdbc.{CallableStatementProxy, PreparedStatementProxy, ResultSetMetaDataProxy, ResultSetProxy}






//todo: rename the package to imps
/** A [[java.sql.ResultSet ResultSet]] proxy which rearranges the columns of the underlying result set
  * according to the given index translation strategy
  * [[net.noresttherein.oldsql.pixies.Rearrangement Rearrangement]]. It is the same as
  * [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]], but assumes that the translation
  * is a [[net.noresttherein.oldsql.pixies.Rearrangement.isInjection injection]], i.e. every read (exposed)
  * index has a corresponding index in the underlying `ResultSet`, which makes it slightly more efficient.
  *
  * All access operations (`getXxx` and `updateXxx`) translate the given argument, 'exposed' index
  * to the underlying one before delegating to the same method of the adapted `ResultSet`.
  * The translation is calculated as:
  * {{{
  *     def underlying(exposed :Int) = startOffset + indexing(exposed)
  *     def exposed(underlying :Int) = indexing.inverse(underlying - startIndex)
  * }}}
  * @param rs          The adapted `ResultSet`.
  * @param startOffset The offset of the mapped index range; added to the 'underlying' index returned by
  *                    `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.apply Rearrangement(_)]].
  *                    Note that `ResultSet` columns are numbered starting with `1`, while the offset is zero-based:
  *                    if reading should start as normal with column `#1`, then `startOffset` should be zero.
  *                    In other words, it specifies the number of initial columns to ignore rather than a column index.
  * @param indices     A mapping of indices of read columns to a certain section of columns in the `ResultSet`,
  *                    relative to `startOffset`.
  * @author Marcin Mościcki
  */
private[pixies] class InjectionResultSet private[pixies] (rs :ResultSet, startOffset :Int, indices :Rearrangement)
	extends ResultSetProxy(rs)
{
	//	import indexing.{apply => publicIndex, inverse => underlying, columnCount}
	@inline private def publicIndex(index :Int) :Int = indices.inverse(index - startOffset)
	@inline private def underlying(index :Int) :Int = startOffset + indices(index)

	override def wasNull :Boolean = rs.wasNull
	override def getMetaData :ResultSetMetaData =
		new RearrangedResultSetMetaData(startOffset, indices, rs.getMetaData)

	override def findColumn(columnLabel :String) :Int = publicIndex(rs.findColumn(columnLabel))

	override def getArray(columnIndex :Int) = rs.getArray(underlying(columnIndex))
	override def getAsciiStream(columnIndex :Int) = rs.getAsciiStream(underlying(columnIndex))
	@nowarn
	override def getBigDecimal(columnIndex :Int, scale :Int) = rs.getBigDecimal(underlying(columnIndex), scale)
	override def getBigDecimal(columnIndex :Int) = rs.getBigDecimal(underlying(columnIndex))
	override def getBinaryStream(columnIndex :Int) = rs.getBinaryStream(underlying(columnIndex))
	override def getBlob(columnIndex :Int) = rs.getBlob(underlying(columnIndex))
	override def getBoolean(columnIndex :Int) = rs.getBoolean(underlying(columnIndex))
	override def getByte(columnIndex :Int) = rs.getByte(underlying(columnIndex))
	override def getBytes(columnIndex :Int) = rs.getBytes(underlying(columnIndex))
	override def getCharacterStream(columnIndex :Int) = rs.getCharacterStream(underlying(columnIndex))
	override def getClob(columnIndex :Int) = rs.getClob(underlying(columnIndex))
	override def getDate(columnIndex :Int) = rs.getDate(underlying(columnIndex))
	override def getDate(columnIndex :Int, cal :Calendar) = rs.getDate(underlying(columnIndex), cal)
	override def getDouble(columnIndex :Int) = rs.getDouble(underlying(columnIndex))
	override def getFloat(columnIndex :Int) = rs.getFloat(underlying(columnIndex))
	override def getInt(columnIndex :Int) = rs.getInt(underlying(columnIndex))
	override def getLong(columnIndex :Int) = rs.getLong(underlying(columnIndex))
	override def getNCharacterStream(columnIndex :Int) = rs.getNCharacterStream(underlying(columnIndex))
	override def getNClob(columnIndex :Int) = rs.getNClob(underlying(columnIndex))
	override def getNString(columnIndex :Int) = rs.getNString(underlying(columnIndex))
	override def getObject(columnIndex :Int) = rs.getObject(underlying(columnIndex))
	override def getObject(columnIndex :Int, map :util.Map[String, Class[_]]) = rs.getObject(underlying(columnIndex), map)
	override def getObject[T](columnIndex :Int, `type` :Class[T]) = rs.getObject(underlying(columnIndex), `type`)
	override def getRef(columnIndex :Int) = rs.getRef(underlying(columnIndex))
	override def getRowId(columnIndex :Int) = rs.getRowId(underlying(columnIndex))
	override def getShort(columnIndex :Int) = rs.getShort(underlying(columnIndex))
	override def getString(columnIndex :Int) = rs.getString(underlying(columnIndex))
	override def getSQLXML(columnIndex :Int) = rs.getSQLXML(underlying(columnIndex))
	override def getTime(columnIndex :Int) = rs.getTime(underlying(columnIndex))
	override def getTime(columnIndex :Int, cal :Calendar) = rs.getTime(underlying(columnIndex), cal)
	override def getTimestamp(columnIndex :Int) = rs.getTimestamp(underlying(columnIndex))
	override def getTimestamp(columnIndex :Int, cal :Calendar) = rs.getTimestamp(underlying(columnIndex), cal)
	@nowarn
	override def getUnicodeStream(columnIndex :Int) = rs.getUnicodeStream(underlying(columnIndex))
	override def getURL(columnIndex :Int) = rs.getURL(underlying(columnIndex))


	override def updateArray(columnIndex :Int, x :sql.Array) = rs.updateArray(underlying(columnIndex), x)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Int) = rs.updateAsciiStream(underlying(columnIndex), x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Long) = rs.updateAsciiStream(underlying(columnIndex), x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream) = rs.updateAsciiStream(underlying(columnIndex), x)
	override def updateBigDecimal(columnIndex :Int, x :java.math.BigDecimal) = rs.updateBigDecimal(underlying(columnIndex), x)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Int) = rs.updateBinaryStream(underlying(columnIndex), x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Long) = rs.updateBinaryStream(underlying(columnIndex), x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream) = rs.updateBinaryStream(underlying(columnIndex), x)
	override def updateBlob(columnIndex :Int, x :Blob) = rs.updateBlob(underlying(columnIndex), x)
	override def updateBlob(columnIndex :Int, inputStream :InputStream, length :Long) = rs.updateBlob(underlying(columnIndex), inputStream, length)
	override def updateBlob(columnIndex :Int, inputStream :InputStream) = rs.updateBlob(underlying(columnIndex), inputStream)
	override def updateBoolean(columnIndex :Int, x :Boolean) = rs.updateBoolean(underlying(columnIndex), x)
	override def updateByte(columnIndex :Int, x :Byte) = rs.updateByte(underlying(columnIndex), x)
	override def updateBytes(columnIndex :Int, x :Array[Byte]) = rs.updateBytes(underlying(columnIndex), x)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Int) = rs.updateCharacterStream(underlying(columnIndex), x, length)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Long) = rs.updateCharacterStream(underlying(columnIndex), x, length)
	override def updateCharacterStream(columnIndex :Int, x :Reader) = rs.updateCharacterStream(underlying(columnIndex), x)
	override def updateClob(columnIndex :Int, x :Clob) = rs.updateClob(underlying(columnIndex), x)
	override def updateClob(columnIndex :Int, reader :Reader, length :Long) = rs.updateClob(underlying(columnIndex), reader, length)
	override def updateClob(columnIndex :Int, reader :Reader) = rs.updateClob(underlying(columnIndex), reader)
	override def updateDate(columnIndex :Int, x :Date) = rs.updateDate(underlying(columnIndex), x)
	override def updateDouble(columnIndex :Int, x :Double) = rs.updateDouble(underlying(columnIndex), x)
	override def updateFloat(columnIndex :Int, x :Float) = rs.updateFloat(underlying(columnIndex), x)
	override def updateInt(columnIndex :Int, x :Int) = rs.updateInt(underlying(columnIndex), x)
	override def updateLong(columnIndex :Int, x :Long) = rs.updateLong(underlying(columnIndex), x)
	override def updateNCharacterStream(columnIndex :Int, x :Reader, length :Long) = rs.updateNCharacterStream(underlying(columnIndex), x, length)
	override def updateNCharacterStream(columnIndex :Int, x :Reader) = rs.updateNCharacterStream(underlying(columnIndex), x)
	override def updateNClob(columnIndex :Int, nClob :NClob) = rs.updateNClob(underlying(columnIndex), nClob)
	override def updateNClob(columnIndex :Int, reader :Reader, length :Long) = rs.updateNClob(underlying(columnIndex), reader, length)
	override def updateNClob(columnIndex :Int, reader :Reader) = rs.updateNClob(underlying(columnIndex), reader)
	override def updateNString(columnIndex :Int, nString :String) = rs.updateNString(underlying(columnIndex), nString)
	override def updateNull(columnIndex :Int) = rs.updateNull(underlying(columnIndex))
	override def updateRef(columnIndex :Int, x :Ref) = rs.updateRef(underlying(columnIndex), x)
	override def updateRowId(columnIndex :Int, x :RowId) = rs.updateRowId(underlying(columnIndex), x)
	override def updateShort(columnIndex :Int, x :Short) = rs.updateShort(underlying(columnIndex), x)
	override def updateString(columnIndex :Int, x :String) = rs.updateString(underlying(columnIndex), x)
	override def updateTime(columnIndex :Int, x :Time) = rs.updateTime(underlying(columnIndex), x)
	override def updateTimestamp(columnIndex :Int, x :Timestamp) = rs.updateTimestamp(underlying(columnIndex), x)
	override def updateObject(columnIndex :Int, x :Any, scaleOrLength :Int) = rs.updateObject(underlying(columnIndex), x, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any) = rs.updateObject(underlying(columnIndex), x)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType, scaleOrLength :Int) = rs.updateObject(underlying(columnIndex), x, targetSqlType, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType) = rs.updateObject(underlying(columnIndex), x, targetSqlType)
	override def updateSQLXML(columnIndex :Int, xmlObject :SQLXML) = rs.updateSQLXML(underlying(columnIndex), xmlObject)

	override def toString :String = "InjectionResultSet(" + rs + ", " + startOffset + " + " + indices + ")"
}




/** A [[java.sql.ResultSet ResultSet]] proxy which rearranges the columns of the underlying result set
  * according to the given index translation [[net.noresttherein.oldsql.pixies.Rearrangement Rearrangement]].
  * All access operations (`getXxx` and `updateXxx`) translate the given argument, 'exposed' index
  * to the underlying one before delegating to the same method of the adapted `ResultSet`.
  * The translation is calculated as:
  * {{{
  *     def exposed(underlying :Int) = indexing.inverse(underlying - startIndex)
  *     def underlying(exposed :Int) = startOffset + indexing(exposed)
  * }}}
  * @param rs          The adapted `ResultSet`.
  * @param startOffset The offset of the mapped index range; added to the 'underlying' index returned by
  *                    `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.apply Rearrangement(_)]].
  *                    Note that `ResultSet` columns are numbered starting with `1`, while the offset is zero-based:
  *                    if reading should start as normal with column `#1`, then `startOffset` should be zero.
  *                    In other words, it specifies the number of initial columns to ignore rather than a column index.
  * @param indices     A mapping of indices read columns to a certain section of columns in the `ResultSet`,
  *                    relative to `startOffset`.
  * @author Marcin Mościcki
  */
private[pixies] class RearrangedResultSet private (rs :ResultSet, startOffset :Int, indices :Rearrangement)
	extends ResultSetProxy(rs)
{
	import indices.{isMapped => has, isCovered}
	private[this] var wasCovered = true

	@inline private def publicIndex(index :Int) :Int = indices.inverse(index - startOffset)
	@inline private def underlying(index :Int) :Int = {
		wasCovered = true
		startOffset + indices(index)
	}
	@inline private def returnNull :Null = { wasCovered = false; null }

	override def getMetaData :ResultSetMetaData =
		new RearrangedResultSetMetaData(startOffset, indices, rs.getMetaData)

	override def findColumn(columnLabel :String) :Int = {
		val i = rs.findColumn(columnLabel)
		if (!isCovered(i))
			throw new SQLException(
				"Column #" + i + ": " + columnLabel + " is not exposed by indexing translation " + indices + "."
			)
		publicIndex(i)
	}

	override def wasNull :Boolean = !wasCovered || rs.wasNull

	override def getArray(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getArray(underlying(columnIndex))
	override def getAsciiStream(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getAsciiStream(underlying(columnIndex))
	@nowarn
	override def getBigDecimal(columnIndex :Int, scale :Int) = if (!has(columnIndex)) returnNull else rs.getBigDecimal(underlying(columnIndex), scale)
	override def getBigDecimal(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getBigDecimal(underlying(columnIndex))
	override def getBinaryStream(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getBinaryStream(underlying(columnIndex))
	override def getBlob(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getBlob(underlying(columnIndex))
	override def getBoolean(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; false } else rs.getBoolean(underlying(columnIndex))
	override def getByte(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; 0 } else rs.getByte(underlying(columnIndex))
	override def getBytes(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getBytes(underlying(columnIndex))
	override def getCharacterStream(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getCharacterStream(underlying(columnIndex))
	override def getClob(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getClob(underlying(columnIndex))
	override def getDate(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getDate(underlying(columnIndex))
	override def getDate(columnIndex :Int, cal :Calendar) = if (!has(columnIndex)) returnNull else rs.getDate(underlying(columnIndex), cal)
	override def getDouble(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; 0.0 } else rs.getDouble(underlying(columnIndex))
	override def getFloat(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; 0.0f } else rs.getFloat(underlying(columnIndex))
	override def getInt(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; 0 } else rs.getInt(underlying(columnIndex))
	override def getLong(columnIndex :Int) = if (!has(columnIndex))  { wasCovered = false; 0 } else rs.getLong(underlying(columnIndex))
	override def getNCharacterStream(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getNCharacterStream(underlying(columnIndex))
	override def getNClob(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getNClob(underlying(columnIndex))
	override def getNString(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getNString(underlying(columnIndex))
	override def getObject(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getObject(underlying(columnIndex))
	override def getObject(columnIndex :Int, map :util.Map[String, Class[_]]) = if (!has(columnIndex)) returnNull else rs.getObject(underlying(columnIndex), map)
	override def getObject[T](columnIndex :Int, `type` :Class[T]) = if (!has(columnIndex)) returnNull.asInstanceOf[T] else rs.getObject(underlying(columnIndex), `type`)
	override def getRef(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getRef(underlying(columnIndex))
	override def getRowId(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getRowId(underlying(columnIndex))
	override def getShort(columnIndex :Int) = if (!has(columnIndex)) { wasCovered = false; 0 } else rs.getShort(underlying(columnIndex))
	override def getString(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getString(underlying(columnIndex))
	override def getSQLXML(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getSQLXML(underlying(columnIndex))
	override def getTime(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getTime(underlying(columnIndex))
	override def getTime(columnIndex :Int, cal :Calendar) = if (!has(columnIndex)) returnNull else rs.getTime(underlying(columnIndex), cal)
	override def getTimestamp(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getTimestamp(underlying(columnIndex))
	override def getTimestamp(columnIndex :Int, cal :Calendar) = if (!has(columnIndex)) returnNull else rs.getTimestamp(underlying(columnIndex), cal)
	@nowarn
	override def getUnicodeStream(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getUnicodeStream(underlying(columnIndex))
	override def getURL(columnIndex :Int) = if (!has(columnIndex)) returnNull else rs.getURL(underlying(columnIndex))


	override def updateArray(columnIndex :Int, x :sql.Array) = if (has(columnIndex)) rs.updateArray(underlying(columnIndex), x)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Int) = if (has(columnIndex)) rs.updateAsciiStream(underlying(columnIndex), x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream, length :Long) = if (has(columnIndex)) rs.updateAsciiStream(underlying(columnIndex), x, length)
	override def updateAsciiStream(columnIndex :Int, x :InputStream) = if (has(columnIndex)) rs.updateAsciiStream(underlying(columnIndex), x)
	override def updateBigDecimal(columnIndex :Int, x :java.math.BigDecimal) = if (has(columnIndex)) rs.updateBigDecimal(underlying(columnIndex), x)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Int) = if (has(columnIndex)) rs.updateBinaryStream(underlying(columnIndex), x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream, length :Long) = if (has(columnIndex)) rs.updateBinaryStream(underlying(columnIndex), x, length)
	override def updateBinaryStream(columnIndex :Int, x :InputStream) = if (has(columnIndex)) rs.updateBinaryStream(underlying(columnIndex), x)
	override def updateBlob(columnIndex :Int, x :Blob) = if (has(columnIndex)) rs.updateBlob(underlying(columnIndex), x)
	override def updateBlob(columnIndex :Int, inputStream :InputStream, length :Long) = if (has(columnIndex)) rs.updateBlob(underlying(columnIndex), inputStream, length)
	override def updateBlob(columnIndex :Int, inputStream :InputStream) = if (has(columnIndex)) rs.updateBlob(underlying(columnIndex), inputStream)
	override def updateBoolean(columnIndex :Int, x :Boolean) = if (has(columnIndex)) rs.updateBoolean(underlying(columnIndex), x)
	override def updateByte(columnIndex :Int, x :Byte) = if (has(columnIndex)) rs.updateByte(underlying(columnIndex), x)
	override def updateBytes(columnIndex :Int, x :Array[Byte]) = if (has(columnIndex)) rs.updateBytes(underlying(columnIndex), x)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Int) = if (has(columnIndex)) rs.updateCharacterStream(underlying(columnIndex), x, length)
	override def updateCharacterStream(columnIndex :Int, x :Reader, length :Long) = if (has(columnIndex)) rs.updateCharacterStream(underlying(columnIndex), x, length)
	override def updateCharacterStream(columnIndex :Int, x :Reader) = if (has(columnIndex)) rs.updateCharacterStream(underlying(columnIndex), x)
	override def updateClob(columnIndex :Int, x :Clob) = if (has(columnIndex)) rs.updateClob(underlying(columnIndex), x)
	override def updateClob(columnIndex :Int, reader :Reader, length :Long) = if (has(columnIndex)) rs.updateClob(underlying(columnIndex), reader, length)
	override def updateClob(columnIndex :Int, reader :Reader) = if (has(columnIndex)) rs.updateClob(underlying(columnIndex), reader)
	override def updateDate(columnIndex :Int, x :Date) = if (has(columnIndex)) rs.updateDate(underlying(columnIndex), x)
	override def updateDouble(columnIndex :Int, x :Double) = if (has(columnIndex)) rs.updateDouble(underlying(columnIndex), x)
	override def updateFloat(columnIndex :Int, x :Float) = if (has(columnIndex)) rs.updateFloat(underlying(columnIndex), x)
	override def updateInt(columnIndex :Int, x :Int) = if (has(columnIndex)) rs.updateInt(underlying(columnIndex), x)
	override def updateLong(columnIndex :Int, x :Long) = if (has(columnIndex)) rs.updateLong(underlying(columnIndex), x)
	override def updateNCharacterStream(columnIndex :Int, x :Reader, length :Long) = if (has(columnIndex)) rs.updateNCharacterStream(underlying(columnIndex), x, length)
	override def updateNCharacterStream(columnIndex :Int, x :Reader) = if (has(columnIndex)) rs.updateNCharacterStream(underlying(columnIndex), x)
	override def updateNClob(columnIndex :Int, nClob :NClob) = if (has(columnIndex)) rs.updateNClob(underlying(columnIndex), nClob)
	override def updateNClob(columnIndex :Int, reader :Reader, length :Long) = if (has(columnIndex)) rs.updateNClob(underlying(columnIndex), reader, length)
	override def updateNClob(columnIndex :Int, reader :Reader) = if (has(columnIndex)) rs.updateNClob(underlying(columnIndex), reader)
	override def updateNString(columnIndex :Int, nString :String) = if (has(columnIndex)) rs.updateNString(underlying(columnIndex), nString)
	override def updateNull(columnIndex :Int) = if (has(columnIndex)) updateNull(underlying(columnIndex))
	override def updateRef(columnIndex :Int, x :Ref) = if (has(columnIndex)) rs.updateRef(underlying(columnIndex), x)
	override def updateRowId(columnIndex :Int, x :RowId) = if (has(columnIndex)) rs.updateRowId(underlying(columnIndex), x)
	override def updateShort(columnIndex :Int, x :Short) = if (has(columnIndex)) rs.updateShort(underlying(columnIndex), x)
	override def updateString(columnIndex :Int, x :String) = if (has(columnIndex)) rs.updateString(underlying(columnIndex), x)
	override def updateTime(columnIndex :Int, x :Time) = if (has(columnIndex)) rs.updateTime(underlying(columnIndex), x)
	override def updateTimestamp(columnIndex :Int, x :Timestamp) = if (has(columnIndex)) rs.updateTimestamp(underlying(columnIndex), x)
	override def updateObject(columnIndex :Int, x :Any, scaleOrLength :Int) = if (has(columnIndex)) rs.updateObject(underlying(columnIndex), x, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any) = if (has(columnIndex)) rs.updateObject(underlying(columnIndex), x)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType, scaleOrLength :Int) = if (has(columnIndex)) rs.updateObject(underlying(columnIndex), x, targetSqlType, scaleOrLength)
	override def updateObject(columnIndex :Int, x :Any, targetSqlType :SQLType) = if (has(columnIndex)) rs.updateObject(underlying(columnIndex), x, targetSqlType)
	override def updateSQLXML(columnIndex :Int, xmlObject :SQLXML) = if (has(columnIndex)) rs.updateSQLXML(underlying(columnIndex), xmlObject)

	override def toString :String = "RearrangedResultSet(" + rs + ", " + startOffset + " + " + indices + ")"
}




/** A factory of [[java.sql.ResultSet ResultSet]] proxies which rearrange the columns of underlying result sets
  * according to the given index translation [[net.noresttherein.oldsql.pixies.Rearrangement Rearrangement]].
  */
private[oldsql] object RearrangedResultSet {
	/** A [[java.sql.ResultSet ResultSet]] proxy which rearranges the columns of the underlying result set
	  * according to the given index translation `indexing`.
	  * All access operations (`getXxx` and `updateXxx`) translate the given, 'exposed' argument index
	  * to the underlying one before delegating to the same method of the adapted `ResultSet`.
	  * The translation is calculated as:
	  * {{{
	  *     def exposed(underlying :Int) = indexing.inverse(underlying - startIndex + 1)
	  *     def underlying(exposed :Int) = startIndex - 1 + indexing(exposed)
	  * }}}
	  * @param rs         The adapted `ResultSet`.
	  * @param startIndex The index of the first column in the mapped index range of `rs`;
	  *                   added to the 'underlying' index returned by
	  *                   `indexing.`[[net.noresttherein.oldsql.pixies.Rearrangement.apply Rearrangement(_)]].
	  * @param indexing   A mapping of indices of read columns to a certain section of columns in the `ResultSet`,
	  *                   relative to `startIndex`.
	  */
	def apply(rs :ResultSet, startIndex :Int, indexing :Rearrangement) :ResultSet =
		if (indexing.isInjection) new InjectionResultSet(rs, startIndex - 1, indexing)
		else new RearrangedResultSet(rs, startIndex - 1, indexing)

	/** Delegates to `RearrangedResultSet(rs, 1, indexing)`. */
	def apply(rs :ResultSet, indexing :Rearrangement) :ResultSet =
		if (indexing.isInjection) new InjectionResultSet(rs, 0, indexing)
		else new RearrangedResultSet(rs, 0, indexing)


	/** `ResultSetMetaData` proxy associated with [[net.noresttherein.oldsql.pixies.RearrangedResultSet RearrangedResultSet]].
	  * @param startOffset The offset of the mapped real index range; indices below it are ignored.
	  *                    Indexing rearrangement starts at this position, with `startOffset` being added
	  *                    to the mapping of the `Rearrangement.apply` in order to obtain the actual index
	  *                    in the underlying `ResultSetMetaData`.
	  * @param indexing    mapping of indices of read columns to a certain section of columns in the `ResultSet`,
	  *                    relative to `startOffset`.
	  * @param meta        the adapted meta data.
	  */
	private[oldsql] class RearrangedResultSetMetaData
	                      (startOffset :Int, indexing :Rearrangement, meta :ResultSetMetaData)
		extends ResultSetMetaDataProxy(meta)
	{
		if (startOffset < 0)
			throw new IndexOutOfBoundsException("Negative offset of the mapped index range: " + startOffset)

		private def realIndex(index :Int) :Int = startOffset + indexing(index)

		override def getColumnCount :Int = indexing.underlyingColumnCount

		override def isAutoIncrement(column :Int) :Boolean = meta.isAutoIncrement(realIndex(column))
		override def isCaseSensitive(column :Int) :Boolean = meta.isCaseSensitive(realIndex(column))
		override def isSearchable(column :Int) :Boolean = meta.isSearchable(realIndex(column))
		override def isCurrency(column :Int) :Boolean = meta.isCurrency(realIndex(column))
		override def isNullable(column :Int) :Int = meta.isNullable(realIndex(column))
		override def isSigned(column :Int) :Boolean = meta.isSigned(realIndex(column))
		override def isReadOnly(column :Int) :Boolean = meta.isReadOnly(realIndex(column))
		override def isWritable(column :Int) :Boolean = meta.isWritable(realIndex(column))
		override def isDefinitelyWritable(column :Int) :Boolean = meta.isDefinitelyWritable(realIndex(column))

		override def getColumnDisplaySize(column :Int) :Int = meta.getColumnDisplaySize(realIndex(column))
		override def getColumnLabel(column :Int) :String = meta.getColumnLabel(realIndex(column))
		override def getColumnName(column :Int) :String = meta.getColumnName(realIndex(column))
		override def getSchemaName(column :Int) :String = meta.getSchemaName(realIndex(column))
		override def getPrecision(column :Int) :Int = meta.getPrecision(realIndex(column))
		override def getScale(column :Int) :Int = meta.getScale(realIndex(column))
		override def getTableName(column :Int) :String = meta.getTableName(realIndex(column))
		override def getCatalogName(column :Int) :String = meta.getCatalogName(realIndex(column))
		override def getColumnType(column :Int) :Int = meta.getColumnType(realIndex(column))
		override def getColumnTypeName(column :Int) :String = meta.getColumnTypeName(realIndex(column))
		override def getColumnClassName(column :Int) :String = meta.getColumnClassName(realIndex(column))
	}

}
