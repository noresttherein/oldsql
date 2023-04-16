package net.noresttherein.oldsql.sql.jdbc

import java.sql.ResultSetMetaData






/**
  * @author Marcin Mościcki
  */
class ResultSetMetaDataProxy(meta :ResultSetMetaData) extends ResultSetMetaData {
	override def getColumnCount :Int = meta.getColumnCount

	override def isAutoIncrement(column :Int) :Boolean = meta.isAutoIncrement(column)
	override def isCaseSensitive(column :Int) :Boolean = meta.isCaseSensitive(column)
	override def isSearchable(column :Int) :Boolean = meta.isSearchable(column)
	override def isCurrency(column :Int) :Boolean = meta.isCurrency(column)
	override def isNullable(column :Int) :Int = meta.isNullable(column)
	override def isSigned(column :Int) :Boolean = meta.isSigned(column)
	override def isReadOnly(column :Int) :Boolean = meta.isReadOnly(column)
	override def isWritable(column :Int) :Boolean = meta.isWritable(column)
	override def isDefinitelyWritable(column :Int) :Boolean = meta.isDefinitelyWritable(column)

	override def getColumnDisplaySize(column :Int) :Int = meta.getColumnDisplaySize(column)
	override def getColumnLabel(column :Int) :String = meta.getColumnLabel(column)
	override def getColumnName(column :Int) :String = meta.getColumnName(column)
	override def getSchemaName(column :Int) :String = meta.getSchemaName(column)
	override def getPrecision(column :Int) :Int = meta.getPrecision(column)
	override def getScale(column :Int) :Int = meta.getScale(column)
	override def getTableName(column :Int) :String = meta.getTableName(column)
	override def getCatalogName(column :Int) :String = meta.getCatalogName(column)
	override def getColumnType(column :Int) :Int = meta.getColumnType(column)
	override def getColumnTypeName(column :Int) :String = meta.getColumnTypeName(column)
	override def getColumnClassName(column :Int) :String = meta.getColumnClassName(column)

	override def unwrap[T](iface :Class[T]) :T = meta.unwrap(iface)
	override def isWrapperFor(iface :Class[_]) :Boolean = meta.isWrapperFor(iface)
}
