package net.noresttherein.oldsql.sql

import java.sql.{CallableStatement, Connection, PreparedStatement, Statement}

import javax.sql.DataSource
import net.noresttherein.oldsql.{ThreadAsset, TransactionAPI}
import net.noresttherein.oldsql.Asset.TransactionAPIAsset
import net.noresttherein.oldsql.ManagedAsset.ManagedTransactionAPIAsset






/**
  * @author Marcin Mościcki
  */
trait SQLTransaction extends TransactionAPI {
	protected def connection :Connection

	override def commit() :Unit = connection.commit()
	override def clean() :Unit = connection.close()
	override def isOpen :Boolean = !connection.isClosed //consider: isValid instead (db roundtrip)

//	def createStatement(sql :String) :Statement = connection.createStatement()
	def prepareStatement(sql :String) :PreparedStatement = connection.prepareStatement(sql)
	def prepareCall(sql :String) :CallableStatement = connection.prepareCall(sql)

	//	protected def apply[T](block :Connection => T) :T
//	def close() :Unit
}



object SQLTransaction {
	def apply(connection :Connection) :SQLTransaction = new SQLConnectionAdapter(connection)

	def apply(database :DataSource) :SQLTransaction = SQLTransaction(database.getConnection)



	class SQLConnectionAdapter(protected val connection :Connection) extends SQLTransaction {
		private var isRolledback :Boolean = false //todo: proxy to Connection so its rollback() also sets this flag

		override def rollback() :Unit = {
			isRolledback = true; connection.rollback()
		} //todo: savepoint api

		override def willRollback :Boolean = isRolledback


		private def status = if (connection.isClosed) "closed" else "open"

		override def toString = s"SQLTransaction($connection:$status)"
	}
}






trait SQLAsset extends TransactionAPIAsset {
	override type Transaction <: SQLTransaction

//	def execute[T](block :Connection => T)(implicit transaction :Transaction) :T = transaction(block)
}





object SQLAsset {

	def apply(dataSource :DataSource) :SQLAsset =
		new DataSourceAsset(dataSource)

	def apply(name: => String, dataSource :DataSource) :SQLAsset =
		new DataSourceAsset(dataSource) {
			override def toString = name
		}

	class DataSourceAsset(protected val dataSource :DataSource) extends SQLAsset {
		override type Transaction = SQLTransaction

		protected override def newTransaction :Transaction = SQLTransaction(dataSource)

		override def noTransaction :Transaction = null

		override def toString = s"SQLAsset($dataSource)"
	}

}





trait ManagedSQLAsset extends ManagedTransactionAPIAsset {
	override type Transaction <: SQLTransaction

//	def execute[T](block :Connection => T) :T = currentTransaction(block)
}






object ManagedSQLAsset {

	def apply(dataSource :DataSource) :ManagedSQLAsset =
		ManagedSQLAsset(s"ManagedSQLAsset($dataSource)", dataSource)

	def apply(name: => String, dataSource: DataSource) :ManagedSQLAsset =
		new ManagedDataSourceAsset(dataSource) with ThreadAsset {
			override def toString = name
		}

	abstract class ManagedDataSourceAsset(protected val dataSource :DataSource) extends ManagedSQLAsset {
		override type Transaction = SQLTransaction

		protected override def newTransaction :SQLTransaction = SQLTransaction(dataSource)
	}

}

