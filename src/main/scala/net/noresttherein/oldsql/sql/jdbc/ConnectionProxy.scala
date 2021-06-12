package net.noresttherein.oldsql.sql.jdbc

import java.sql.{Connection, Savepoint, ShardingKey}
import java.util
import java.util.Properties
import java.util.concurrent.Executor






/**
  * @author Marcin Mościcki
  */ //todo: factory methods with wrappers for other types we might want to proxy such as ResultSet, PreparedStatement, etc.
private[sql]  class ConnectionProxy(connection :Connection) extends Connection {
	override def createStatement() = connection.createStatement()
	override def createStatement(resultSetType :Int, resultSetConcurrency :Int) =
		connection.createStatement(resultSetType, resultSetConcurrency)
	override def createStatement(resultSetType :Int, resultSetConcurrency :Int, resultSetHoldability :Int) =
		connection.createStatement(resultSetType, resultSetConcurrency, resultSetHoldability)

	override def prepareStatement(sql :String) = connection.prepareStatement(sql)
	override def prepareStatement(sql :String, resultSetType :Int, resultSetConcurrency :Int) =
		connection.prepareStatement(sql, resultSetType, resultSetConcurrency)
	override def prepareStatement(sql :String, resultSetType :Int, resultSetConcurrency :Int, resultSetHoldability :Int) =
		connection.prepareStatement(sql, resultSetType, resultSetConcurrency, resultSetHoldability)
	override def prepareStatement(sql :String, autoGeneratedKeys :Int) =
		connection.prepareStatement(sql, autoGeneratedKeys)
	override def prepareStatement(sql :String, columnIndexes :Array[Int]) =
		connection.prepareStatement(sql, columnIndexes)
	override def prepareStatement(sql :String, columnNames :Array[String]) =
		connection.prepareStatement(sql, columnNames)

	override def prepareCall(sql :String) = connection.prepareCall(sql)
	override def prepareCall(sql :String, resultSetType :Int, resultSetConcurrency :Int) =
		connection.prepareCall(sql, resultSetType, resultSetConcurrency)
	override def prepareCall(sql :String, resultSetType :Int, resultSetConcurrency :Int, resultSetHoldability :Int) =
		connection.prepareCall(sql, resultSetType, resultSetConcurrency, resultSetHoldability)

	override def createClob() = connection.createClob()
	override def createBlob() = connection.createBlob()
	override def createNClob() = connection.createNClob()
	override def createSQLXML() = connection.createSQLXML()
	override def createArrayOf(typeName :String, elements :Array[AnyRef]) =
		connection.createArrayOf(typeName, elements)
	override def createStruct(typeName :String, attributes :Array[AnyRef]) =
		connection.createStruct(typeName, attributes)

	override def nativeSQL(sql :String) = connection.nativeSQL(sql)

	override def setShardingKeyIfValid(shardingKey :ShardingKey, superShardingKey :ShardingKey, timeout :Int) =
		connection.setShardingKeyIfValid(shardingKey, superShardingKey, timeout)
	override def setShardingKeyIfValid(shardingKey :ShardingKey, timeout :Int) =
		connection.setShardingKeyIfValid(shardingKey, timeout)
	override def setShardingKey(shardingKey :ShardingKey, superShardingKey :ShardingKey) =
		connection.setShardingKey(shardingKey, superShardingKey)
	override def setShardingKey(shardingKey :ShardingKey) = connection.setShardingKey(shardingKey)

	override def getTypeMap = connection.getTypeMap
	override def setTypeMap(map :util.Map[String, Class[_]]) = connection.setTypeMap(map)
	override def setClientInfo(name :String, value :String) = connection.setClientInfo(name, value)
	override def setClientInfo(properties :Properties) = connection.setClientInfo(properties)
	override def getClientInfo(name :String) = connection.getClientInfo(name)
	override def getClientInfo = connection.getClientInfo
	override def getMetaData = connection.getMetaData
	override def setSchema(schema :String) = connection.setSchema(schema)
	override def getSchema = connection.getSchema
	override def setCatalog(catalog :String) = connection.setCatalog(catalog)
	override def getCatalog = connection.getCatalog

	override def setNetworkTimeout(executor :Executor, milliseconds :Int) =
		connection.setNetworkTimeout(executor, milliseconds)
	override def getNetworkTimeout = connection.getNetworkTimeout

	override def setHoldability(holdability :Int) = connection.setHoldability(holdability)
	override def getHoldability = connection.getHoldability
	override def setTransactionIsolation(level :Int) = connection.setTransactionIsolation(level)
	override def getTransactionIsolation = connection.getTransactionIsolation
	override def setReadOnly(readOnly :Boolean) = connection.setReadOnly(readOnly)
	override def isReadOnly = connection.isReadOnly
	override def setAutoCommit(autoCommit :Boolean) = connection.setAutoCommit(autoCommit)
	override def getAutoCommit = connection.getAutoCommit

	override def getWarnings = connection.getWarnings
	override def clearWarnings() = connection.clearWarnings()

	override def setSavepoint() = connection.setSavepoint()
	override def setSavepoint(name :String) = connection.setSavepoint(name)

	override def commit() = connection.commit()
	override def rollback() = connection.rollback()
	override def rollback(savepoint :Savepoint) = connection.rollback(savepoint)
	override def releaseSavepoint(savepoint :Savepoint) = connection.releaseSavepoint(savepoint)

	override def abort(executor :Executor) = connection.abort(executor)
	override def beginRequest() = connection.beginRequest()
	override def endRequest() = connection.endRequest()

	override def close() = connection.close()
	override def isClosed = connection.isClosed
	override def isValid(timeout :Int) = connection.isValid(timeout)

	override def unwrap[T](iface :Class[T]) = connection.unwrap(iface)
	override def isWrapperFor(iface :Class[_]) = connection.isWrapperFor(iface)
}

