package net.noresttherein.oldsql.exceptions






/** Exception thrown when a new transaction for a particular [[net.noresttherein.oldsql.Asset Asset]] cannot be created
  * for whatever reason. */
class AssetUnavailableException(message :String, cause :Throwable)
	extends BaseOldSQLException(message, cause)
{
	def this(message :String) = this(message, null)
	def this(cause :Throwable) = this(cause.getMessage, cause)

	override def addInfo(msg :String) :OldSQLException = new AssetUnavailableException(msg, this)
}


/** Exception thrown by [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]]s when an existing,
  * open transaction context is expected but none exists.
  * @see [[net.noresttherein.oldsql.ManagedAsset.inTransaction]]
  */
class TransactionUnavailableException(message :String, cause :Throwable) extends BaseOldSQLException(message, cause) {
	def this(message :String) = this(message, null)
	def this() = this("No transaction currently in progress.", null)

	override def addInfo(msg :String) :OldSQLException = new TransactionUnavailableException(msg, this)
}


/** Exception thrown from [[net.noresttherein.oldsql.Asset.abort Asset.abort]] methods to abort any transaction
  * currently in progress and mark it for rollback. Can be also thrown explicitly by client code to a similar effect.
  */
class TransactionAbortedException(message :String, cause :Throwable) extends BaseOldSQLException(message, cause) {
	def this(message :String) = this(message, null)
	def this(cause :Throwable) = this(s"Transaction rolled back on request: ${cause.getMessage}.", cause)
	def this() = this("Transaction rolled back on request.", null)

	override def addInfo(msg :String) :OldSQLException = new TransactionAbortedException(msg, this)
}


/** Exception thrown by [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]]s when a new transaction is requested,
  * but another one is already in progress and the asset does not support nested transactions.
  * @see [[net.noresttherein.oldsql.ManagedAsset.transaction]]
  */
class PreexistingTransactionException(message :String, cause :Throwable) extends BaseOldSQLException(message, cause) {
	def this(message :String) = this(message, null)
	def this(cause :Throwable) = this(s"Transaction already in progress: ${cause.getMessage}.", cause)
	def this() = this("Transaction already in progress.", null)

	override def addInfo(msg :String) :OldSQLException = new PreexistingTransactionException(msg, this)
}


/** Exception thrown by [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]],
  * [[net.noresttherein.oldsql.Asset Asset]]s and [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]]s
  * when any method other than [[net.noresttherein.oldsql.TransactionAPI.clean clean]] is called for a closed transaction,
  * or a closed transaction is passed to [[net.noresttherein.oldsql.Asset.inTransaction inTransaction]] or
  * [[net.noresttherein.oldsql.Asset.transactional transactional]] methods.
  */
class TransactionClosedException(message :String, cause :Throwable) extends BaseOldSQLException(message, cause) {
	def this(message :String) = this(message, null)
	def this(cause :Throwable) = this(s"Transaction already closed: ${cause.getMessage}.", cause)
	def this() = this("Transaction already closed.", null)

	override def addInfo(msg :String) :OldSQLException = new TransactionClosedException(msg, this)
}


/** Exception thrown by [[net.noresttherein.oldsql.TransactionAPI transactions]],
  * [[net.noresttherein.oldsql.Asset Asset]]s and [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]]s
  * when an open transaction is expected but the passed transaction/existing transaction context has been either
  * rolled back or marked for roll back.
  */
class TransactionRolledBackException(message :String, cause :Throwable)
	extends TransactionClosedException(message, cause)
{
	def this(message :String) = this(message, null)
	def this(cause :Throwable) = this(s"Transaction already rolled back: ${cause.getMessage}.", cause)
	def this() = this("Transaction already rolled back.", null)

	override def addInfo(msg :String) :OldSQLException = new TransactionRolledBackException(msg, this)
}
