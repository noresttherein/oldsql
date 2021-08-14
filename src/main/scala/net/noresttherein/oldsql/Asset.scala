package net.noresttherein.oldsql

import net.noresttherein.oldsql.ManagedAsset.{AbstractManagedAsset, ManagedTransactionAPIAsset}
import net.noresttherein.oldsql.exceptions.{PreexistingTransactionException, TransactionAbortedException, TransactionUnavailableException}






/** Basic transactional API, optionally used as the base type by [[net.noresttherein.oldsql.Asset Asset]]
  * and [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]] implementations. It is recommended that applications
  * use corresponding methods of those classes, which work with implicit transaction contexts, rather than this API,
  * as it makes migrations between the mentioned two interfaces easier.
  */
trait TransactionAPI {
	def commit() :Unit
	def rollback() :Unit
	def clean() :Unit = ()
	def isOpen :Boolean
	def willRollback :Boolean
}






/** High level API of some transactional data source or service. 
  * This interface is responsible only of managing of open transactions, abstracting completely over the nature
  * of the underlying resource. It uses inversion of control, hiding the creation, committing and, potentially, 
  * rolling back of transactions in favour of executing transactional code in the form of functions taking
  * `Transaction` as their arguments. The transaction life cycle is managed by this instance and methods
  * provided here should be generally preferred to any methods declared by the concrete definition of 
  * [[net.noresttherein.oldsql.Asset.Transaction Transaction]], as it allows higher portability and easier migration
  * between this interface and its sister [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]].
  * @see [[net.noresttherein.oldsql.Asset.transaction]]
  * @see [[net.noresttherein.oldsql.Asset.transactional]]
  * @see [[net.noresttherein.oldsql.ManagedAsset]]      
  */
trait Asset {
	
	/** The transaction type specific to this asset. It can, but doesn't have to, extend
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]] interface. In either case, direct use of the latter
	  * is discouraged in favour of using methods of `Asset` interface.
	  */
	type Transaction

	/** Create a fresh transaction. This is a hook method for subclasses, providing all transactions used by this asset.
	  * @throws net.noresttherein.oldsql.exceptions.AssetUnavailableException if the transaction cannot be created
	  *         because of connection or pooling issues.
	  * @throws net.noresttherein.oldsql.exceptions.PreexistingTransactionException if a transaction context already
	  *         exists and the asset does not support nested transactions
	  */
	protected def newTransaction :Transaction

	/** A dummy instance - possibly `null` of [[net.noresttherein.oldsql.Asset.Transaction Transaction]] used
	  * as the default argument to [[net.noresttherein.oldsql.Asset.transactional transactional]] to indicate
	  * that a fresh transaction should be created for the executed block. It should not be used explicitly
	  * by application code.
	  */
	def noTransaction :Transaction

	/** True if the implicitly available transaction is open and not marked for rollback.
	  * The default implementation checks only if an implicit `Transaction` exists, which makes it of limited use
	  * when considered in itself. A part of the reason behind it - apart from subclasses possibly performing
	  * some actual checks - is a symmetry with [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]], making
	  * the code dependent on this interface more easily portable to the former.
	  */
	def isTransactional(implicit transaction :Transaction = noTransaction) :Boolean = transaction != null

	/** Commits the implicitly available transaction. This is a hook method by which subclasses provide the required
	  * implementation, and should not be exposed in general.
	  */
	protected def commit()(implicit transaction :Transaction) :Unit



	/** True if the implicitly available transaction is either already rolled back or marked for roll back. */
	def willRollback(implicit transaction :Transaction) :Boolean

	/** Flags the implicitly available transaction for being rolled back. The moment of actual rollback is unspecified:
	  * it can happen as part of execution of this method, or at some other point in the future 
	  * (presumably at the end of the block for which the transaction has been created). Note that this method
	  * can potentially throw any kind of exception. When rolling back as a consequence of catching an exception,
	  * its overloaded variant `rollback(exception)` can be used, which will suppress any exceptions thrown 
	  * by this method under the argument exception. Explicit calling of this method is optional as, by default,
	  * any exception thrown by a [[net.noresttherein.oldsql.Asset.transactional transactional]] block will result
	  * in the transaction being rolled back. The latter approach should generally be preferred over explicit 
	  * invocations of this method as more portable.
	  * @see [[net.noresttherein.oldsql.Asset.abort]]
	  * @see [[net.noresttherein.oldsql.Asset.transaction]]
	  * @see [[net.noresttherein.oldsql.Asset.rollback(cause:Throwable)*]]      
	  */
	def rollback()(implicit transaction :Transaction) :Unit

	/** Flags the implicitly available transaction for being rolled back. The moment of actual rollback is unspecified:
	  * it can happen as part of execution of this method, or at some other point in the future 
	  * (presumably at the end of the block for which the transaction has been created). This is a low-level method
	  * and its explicit calls are discouraged in favour of simply throwing an exception, if possible, as, by default,
	  * any exception thrown by a [[net.noresttherein.oldsql.Asset.transactional transactional]] block will result
	  * in the transaction being rolled back, as the latter approach is more portable.
	  * @see [[net.noresttherein.oldsql.Asset.abort]]
	  * @see [[net.noresttherein.oldsql.Asset.transaction]]
	  * @see [[net.noresttherein.oldsql.Asset.transactional]]      
	  */
	final def rollback(cause :Throwable)(implicit transaction :Transaction) :Unit =
		try {
			rollback(cause)
		} catch {
			case e :Exception => cause.addSuppressed(e)
			case e :Throwable => e.addSuppressed(cause); throw e
		}

	private def rollbackAndThrow(result :Throwable)(implicit transaction :Transaction) :Nothing = {
		try { rollback() } catch { case e :Throwable => result.addSuppressed(e) }
		throw result
	}


	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]], which
	  * will result in the transaction being rolled back. Note that the exception thrown is not treated any differently
	  * to other exception types, and all - by default - result in the transaction being rolled back. It is provided
	  * mainly as a convenience alternative to explicit throwing of an exception. Overloaded variants of this method
	  * exist which accept common argument lists of `Exception` constructors.
	  */
	def abort()(implicit transaction :Transaction) :Nothing =
		throw new TransactionAbortedException(s"Rolling back transaction $transaction for $this")
//		rollbackAndThrow(new TransactionAbortedException())

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given exception as the direct cause, which will result in the transaction being rolled back. 
	  * Note that the exception thrown is not treated any differently to other exception types, and all - by default - 
	  * result in the transaction being rolled back. It is provided mainly as a convenience alternative to explicit 
	  * throwing of an exception. Overloaded variants of this method exist which accept common argument lists of 
	  * `Exception` constructors.
	  */
	def abort(cause :Throwable)(implicit transaction :Transaction) :Nothing =
		throw new TransactionAbortedException(s"Rolling back transaction $transaction for $this: ${cause.getMessage}.", cause)
//		rollbackAndThrow(new TransactionAbortedException(cause))

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given message, which will result in the transaction being rolled back. Note that the exception thrown 
	  * is not treated any differently to other exception types, and all - by default - result in the transaction being 
	  * rolled back. It is provided mainly as a convenience alternative to explicit throwing of an exception. 
	  * Overloaded variants of this method exist which accept common argument lists of `Exception` constructors.
	  */
	def abort(message :String)(implicit transaction :Transaction) :Nothing =
		throw new TransactionAbortedException(message)
//		rollbackAndThrow(new TransactionAbortedException(message))

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given message and cause, which will result in the transaction being rolled back. Note that 
	  * the exception thrown is not treated any differently to other exception types, and all - by default - 
	  * result in the transaction being rolled back. It is provided mainly as a convenience shorthand for explicit 
	  * throwing of an exception. Overloaded variants of this method exist which accept common argument lists of 
	  * `Exception` constructors.
	  */
	def abort(message :String, cause :Throwable)(implicit transaction :Transaction) :Nothing =
		throw new TransactionAbortedException(message, cause)
//		rollbackAndThrow(new TransactionAbortedException(message, cause))


	/** Executes the given block in a preexisting transaction context.
	  * The transaction will ''not'' be committed (or rolled back) when control is passed from the block back to
	  * this method. It simply summons an implicit [[net.noresttherein.oldsql.Asset.Transaction Transaction]]
	  * and passes it as the argument to the block parameter.
	  *
	  * Note that you can declare the transaction argument of the block as `implicit`, making it available to
	  * any methods requiring an implicit transaction:
	  * {{{
	  *     asset.inTransaction { implicit tx =>
	  *         ...
	  *     }
	  * }}}
	  * @return the value returned by `block`.
	  * @see [[net.noresttherein.oldsql.Asset.transaction]] which runs the block in a fresh transaction instead.
	  * @see [[net.noresttherein.oldsql.Asset.transactional]] which will reuse an implicit transaction if one 
	  *      is available and create a fresh one in the other case.      
	  */
//	@deprecated("This method does not make sense. What was I thinking?", "release")
	def inTransaction[T](block :Transaction => T)(implicit transaction :Transaction) :T = block(transaction)

	/** Starts a fresh [[net.noresttherein.oldsql.Asset.Transaction transaction]] and passes it to the given
	  * function. The transaction will be automatically committed once control returns from the block.
	  * Any exceptions thrown, both by the block and from transaction API, will result in the transaction being
	  * rolled back instead. This behaviour can be overriden on a case by case basis by overriding
	  * [[net.noresttherein.oldsql.Asset.caseRecoverable caseRecoverable]], listing exceptions which should be
	  * propagated (or wrapped), while still making an attempt to commit the transaction. If the commit fails 
	  * - in either scenario - the transaction is being rolled back. All other thrown `Throwable` are caught and 
	  * automatically propagated after rolling back the transaction. Exceptions thrown from transactional API
	  * (i.e., when committing or rolling back) are suppressed under the original cause, while other `Throwable`
	  * take precedence over the original exception (which is suppressed under the rethrown `Throwable`).
	  * If this asset overrides [[net.noresttherein.oldsql.Asset.caseThrown caseThrown]], any caught exceptions
	  * are mapped with that partial function before being rethrown. This behaviour should be generally reserved
	  * for 'technical' exceptions thrown by the underlying service and not any exceptions thrown by the application
	  * or this library.
	  *
	  * Note that you can declare the transaction argument of the block as `implicit`, making it available to
	  * any methods requiring an implicit transaction:
	  * {{{
	  *     asset.transaction { implicit tx =>
	  *         ...
	  *     }
	  * }}}
	  * @return the value returned by `block`.
	  * @see [[net.noresttherein.oldsql.Asset.inTransaction]]
	  * @see [[net.noresttherein.oldsql.Asset.transactional]]      
	  */
	def transaction[T](block :Transaction => T) :T = {
		implicit val transaction = newTransaction
		try {
			val result = block(transaction)
			if (willRollback && isTransactional) rollback()
			else commit()
			result
		} catch {
			case e :Exception =>
				caseRecoverable.andThen(recover(_)).applyOrElse(e, onFailure(_:Exception))
			case e :Throwable =>
				rollbackAndThrow(e)
		}
	}

	/** Executes the given block in a transaction. If an implicit transaction is available, it will be used
	  * as the argument for the block and it will be neither committed nor rolled back by this method before it returns
	  * (outside of the block itself calling [[net.noresttherein.oldsql.Asset.rollback rollback]]).
	  * If no implicit value of [[net.noresttherein.oldsql.Asset.Transaction Transaction]] is present 
	  * (or there is a conflict between competing values), a new instance will be created for the benefit of this
	  * block only. In that case, the transaction will be always closed when the method returns: if no exception
	  * is thrown, or the thrown exception is handled by 
	  * [[net.noresttherein.oldsql.Asset.caseRecoverable caseRecoverable]], the transaction is committed; 
	  * otherwise it is rolled back. 
	  * 
	  * This method is thus equivalent to either [[net.noresttherein.oldsql.Asset.inTransaction inTransaction(block)]]
	  * or [[net.noresttherein.oldsql.Asset.transaction transaction(block)]], depending on the presence of 
	  * a preexisting implicit transaction.
	  *
	  * Note that you can declare the transaction argument of the block as `implicit`, making it available to
	  * any methods requiring an implicit transaction:
	  * {{{
	  *     asset.transactional { implicit tx =>
	  *         ...
	  *     }
	  * }}}
	  * @return the value returned by `block`.
	  */	
	def transactional[T](block :Transaction => T)(implicit transaction :Transaction = noTransaction) :T =
		if (transaction == noTransaction)
			this.transaction(block)
		else
			block(transaction)
//			inTransaction(block)



	private def recover(error :Exception)(implicit transaction :Transaction) :Nothing = try {
			commit(); throw error
		} catch {
			case e :Exception => e.addSuppressed(error); onFailure(e)
			case e :Throwable => e.addSuppressed(error); throw e
		}

	private def onFailure(error :Exception)(implicit transaction :Transaction) :Nothing = {
		rollback(error)
		throw caseThrown.applyOrElse(error, identity[Exception])
	}


	/** A hook method for implementations allowing them to list exceptions which should result in the transaction
	  * being committed despite them being thrown. The exception which will be propagated will be the one
	  * returned by this partial function for the caught exception.
	  * @see [[net.noresttherein.oldsql.Asset.caseThrown]]
	  */
	protected def caseRecoverable :PartialFunction[Exception, Exception] = PartialFunction.empty

	/** A hook method for implementations allowing them to map any caught exception being rethrown.
	  * It is generally used to wrap any exceptions from the underlying service/database to specific application-level
	  * exception types in order to abstract over the underlying implementation and provide portability.
	  */
	protected def caseThrown :PartialFunction[Exception, Exception] = PartialFunction.empty

}






object Asset {

	/** Crates a new `Asset` using the provided by-name expression as the factory function of new transactions.
	  * Other transaction life cycle methods delegate to the corresponding methods of
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]].
	  */
	def apply(factory: => TransactionAPI) :Asset = Asset(null, factory)
	
	/** Crates a new `Asset` using the provided by-name expression as the factory function of new transactions.
	  * Other transaction life cycle methods delegate to the corresponding methods of
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]].
	  * @param name the name used in the `toString` implementation of the created asset and in exception messages.
	  * @param factory generator of fresh transactions. Each evaluation should return a new transaction instance.             
	  */
	def apply(name :String, factory: => TransactionAPI) :Asset = new TransactionAPIAsset {
		type Transaction = TransactionAPI

		protected override def newTransaction = factory

		override def toString = if (name == null) super.toString else name
	}



	/** Adapts the given `ManagedAsset` to the `Asset` interface. Returned asset overrides implementations
	  * of all methods executing transactional blocks, delegating them to their counterparts of the adapted
	  * instance.
	  */
	def apply(asset :ManagedAsset) :Asset = new Asset {
		override type Transaction = asset.Transaction

		protected override def newTransaction = //asset.trustedStartTransaction
			throw new UnsupportedOperationException(
				s"Asset adapter of $asset does not support explicit transaction creation."
			)

		override def noTransaction = null.asInstanceOf[Transaction]

		override def isTransactional(implicit transaction :Transaction) :Boolean = {
			assertSame; asset.isTransactional
		}

		protected override def commit()(implicit transaction :Transaction) :Unit =
			throw new UnsupportedOperationException(s"Asset adapter of $asset does not support explicit commits.")

		override def willRollback(implicit transaction :Transaction) = { //transaction.willRollback
			assertSame; asset.willRollback
		}

		override def rollback()(implicit transaction :Transaction) :Unit = { //transaction.rollback()
			assertSame; asset.rollback()
		}

		override def abort()(implicit transaction :Transaction) = {
			assertSame; asset.abort()
		}

		override def abort(cause :Throwable)(implicit transaction :Transaction) = {
			assertSame; asset.abort(cause)
		}

		override def abort(message :String)(implicit transaction :Transaction) = {
			assertSame; asset.abort(message)
		}

		override def abort(message :String, cause :Throwable)(implicit transaction :Transaction) = {
			assertSame; asset.abort(message, cause)
		}


		override def inTransaction[T](block :Transaction => T)(implicit transaction :Transaction) :T = {
			assertSame; asset.transactional(block(transaction))
		}

		override def transaction[T](block :Transaction => T) :T = asset.transaction {
			asset.trustedTransaction match {
				case Some(t) => block(t)
				case _ => throw new TransactionUnavailableException(
					s"Transaction unavailable in the transaction block of $asset: this is a bug."
				)
			}
		}

		override def transactional[T](block :Transaction => T)(implicit transaction :Transaction) :T =
			if (transaction == noTransaction)
				asset.transactional {
					asset.trustedTransaction match {
						case Some(t) => block(t)
						case _ => throw new TransactionUnavailableException(
							s"Transaction unavailable in the transactional block of $asset: this is a bug."
						)
					}
				}
			else
				asset.transactional { assertSame; block(transaction) }


		private def assertSame(implicit given :Transaction) :Unit = asset.trustedTransaction match {
			case Some(t) if t != given => throw new IllegalArgumentException(
				s"Transaction in progress $t for $this is not the same as the one implicitly passed: $given."
			)
			case _ => ()
		}

		override def toString = asset.toString
	}



	trait TransactionAPIAsset extends Asset {
		override type Transaction <: TransactionAPI

		override def noTransaction :Transaction = null.asInstanceOf[Transaction]

		override def isTransactional(implicit transaction :Transaction) :Boolean = transaction.isOpen

		override def willRollback(implicit transaction :Transaction) :Boolean = transaction.willRollback

		override def rollback()(implicit transaction :Transaction) :Unit = {
			transaction.rollback(); transaction.clean()
		}

		protected override def commit()(implicit transaction :Transaction) :Unit = {
			transaction.commit(); transaction.clean()
		}
	}

}






/** High level API of some transactional data source or service. It is a sister trait to
  * [[net.noresttherein.oldsql.Asset Asset]], but where the latter used implicit transaction parameters in order
  * to propagate the transactional context between method calls, this interface depends on some other means, such
  * as thread local variables or JNDI, to do the job, removing the transaction from the API almost completely.
  * 
  * This interface is responsible only of managing of open transactions, abstracting completely over the nature
  * of the underlying resource. It uses inversion of control, hiding the creation, committing and, potentially, 
  * rolling back of transactions in favour of executing transactional blocks. The transaction life cycle is managed by 
  * this instance and methods provided here should be generally preferred to any methods declared by the concrete 
  * definition of [[net.noresttherein.oldsql.ManagedAsset.Transaction Transaction]], as it allows higher portability 
  * and easier migration between this interface and its sister [[net.noresttherein.oldsql.Asset Asset]].
  * 
  * The default implementation of `ManagedAsset` is [[net.noresttherein.oldsql.ThreadAsset ThreadAsset]].
  * Additionally, a base class providing a framework for typical implementations is present 
  * in [[net.noresttherein.oldsql.ManagedAsset.AbstractManagedAsset AbstractManagedAsset]]. 
  * @see [[net.noresttherein.oldsql.ManagedAsset.transaction]]
  * @see [[net.noresttherein.oldsql.ManagedAsset.transactional]]
  * @see [[net.noresttherein.oldsql.Asset]]
  */
trait ManagedAsset {

	/** The transaction type specific to this asset. It can, but doesn't have to, extend
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]] interface. In either case, direct use of the latter
	  * is discouraged in favour of using methods of `Asset` interface.
	  */
	type Transaction

	/** Creates a fresh transaction, throwing an exception if one already exists. Implementation is left to subclasses.
	  * @throws net.noresttherein.oldsql.exceptions.PreexistingTransactionException if an open transactional context 
	  *                                                                             is already present.
	  * @throws net.noresttherein.oldsql.exceptions.AssetUnavailableException if the transaction cannot be created
	  *                                                                       for some technical reason.
	  */
	protected def startTransaction() :Transaction

	protected[oldsql] def trustedStartTransaction :Transaction = startTransaction()

	/** Returns transactional context for the current thread if one exists, creating a fresh one if none exists.
	  * Implementation is left to subclasses.
	  * @throws net.noresttherein.oldsql.exceptions.AssetUnavailableException if the transaction cannot be created
	  *                                                                       for some technical reason.
	  */
	protected def acquireTransaction() :Transaction

	/** Returns the current transaction, or throws an exception if none exists. 
	  * Default implementation is based on [[net.noresttherein.oldsql.ManagedAsset.transactionOpt transactionOpt]].
	  * @throws net.noresttherein.oldsql.exceptions.TransactionUnavailableException if no open transactional context 
	  *                                                                             exists for this thread.
	  */
	protected def currentTransaction :Transaction = transactionOpt match {
		case Some(t) => t
		case _ => throw new TransactionUnavailableException("No current transaction for " + this)
	}

	/** Returns any open transaction, if one is present. Implementation is left to subclasses. */
	protected def transactionOpt :Option[Transaction]

	protected[oldsql] def trustedTransaction :Option[Transaction] = transactionOpt


	/** Checks if there is an open transaction in this thread's context. If `true`, calls to
	  * [[net.noresttherein.oldsql.ManagedAsset.inTransaction inTransaction]] will not fail with
	  * an [[net.noresttherein.oldsql.exceptions.TransactionUnavailableException TransactionUnavailableException]].
	  */
	def isTransactional :Boolean


	/** True if the underlying transaction is either already rolled back or marked for roll back.
	  * @throws net.noresttherein.oldsql.exceptions.TransactionUnavailableException if no transactional context 
	  *                                                                             is present. 
	  */
	def willRollback :Boolean

	/** Flags the implicitly available transaction for being rolled back. The moment of actual rollback is unspecified:
	  * it can happen as part of execution of this method, or at some other point in the future 
	  * (presumably at the end of the block for which the transaction has been created). This is a low-level method
	  * and its explicit calls are discouraged in favour of simply throwing an exception, if possible, as, by default,
	  * any exception thrown by a [[net.noresttherein.oldsql.Asset.transactional transactional]] block will result
	  * in the transaction being rolled back, as the latter approach is more portable.
	  * @see [[net.noresttherein.oldsql.ManagedAsset.abort]]
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transaction]]
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transactional]]
	  */
	def rollback() :Unit

	/** Flags the implicitly available transaction for being rolled back. The moment of actual rollback is unspecified:
	  * it can happen as part of execution of this method, or at some other point in the future 
	  * (presumably at the end of the block for which the transaction has been created). Note that this method
	  * can potentially throw any kind of exception. When rolling back as a consequence of catching an exception,
	  * its overloaded variant `rollback(exception)` can be used, which will suppress any exceptions thrown 
	  * by this method under the argument exception. Explicit calling of this method is optional as, by default,
	  * any exception thrown by a [[net.noresttherein.oldsql.ManagedAsset.transactional transactional]] block 
	  * will result in the transaction being rolled back. The latter approach should generally be preferred over 
	  * explicit invocations of this method as more portable.
	  * @see [[net.noresttherein.oldsql.ManagedAsset.abort]]
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transaction]]
	  * @see [[net.noresttherein.oldsql.ManagedAsset.rollback(cause:Throwable)*]]
	  */
	def rollback(cause :Throwable) :Unit


	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]], which
	  * will result in the transaction being rolled back. Note that the exception thrown is not treated any differently
	  * to other exception types, and all - by default - result in the transaction being rolled back. It is provided
	  * mainly as a convenience alternative to explicit throwing of an exception. Overloaded variants of this method
	  * exist which accept common argument lists of `Exception` constructors.
	  */
	def abort() :Nothing = throw new TransactionAbortedException

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given exception as the direct cause, which will result in the transaction being rolled back. 
	  * Note that the exception thrown is not treated any differently to other exception types, and all - by default - 
	  * result in the transaction being rolled back. It is provided mainly as a convenience alternative to explicit 
	  * throwing of an exception. Overloaded variants of this method exist which accept common argument lists of 
	  * `Exception` constructors.
	  */
	def abort(cause :Throwable) :Nothing = throw new TransactionAbortedException(cause)

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given message, which will result in the transaction being rolled back. Note that the exception thrown 
	  * is not treated any differently to other exception types, and all - by default - result in the transaction being 
	  * rolled back. It is provided mainly as a convenience alternative to explicit throwing of an exception. 
	  * Overloaded variants of this method exist which accept common argument lists of `Exception` constructors.
	  */
	def abort(message :String) :Nothing = throw new TransactionAbortedException(message)

	/** Throws a [[net.noresttherein.oldsql.exceptions.TransactionAbortedException TransactionAbortedException]]
	  * with the given message and cause, which will result in the transaction being rolled back. Note that 
	  * the exception thrown is not treated any differently to other exception types, and all - by default - 
	  * result in the transaction being rolled back. It is provided mainly as a convenience shorthand for explicit 
	  * throwing of an exception. Overloaded variants of this method exist which accept common argument lists of 
	  * `Exception` constructors.
	  */
	def abort(message :String, cause :Throwable) :Nothing = throw new TransactionAbortedException(message, cause)



	/** Executes the given block in a preexisting transaction context. The transaction will ''not'' be committed 
	  * (or rolled back) when control is passed from the block back to this method. The check of the existence 
	  * of a transaction for the current thread can be performed explicitly, before `block` is executed, or deferred 
	  * until a transaction is actually needed by any method called from the block. The latter case would make 
	  * this method superfluous when considered in itself, as it would have the same effect as executing the block 
	  * explicitly outside of this method; a large part of the reason behind its existence however is the symmetry with
	  * [[net.noresttherein.oldsql.Asset Asset]], which allows the code written for the latter to be adapted with
	  * minimal changes for the use with this interface.
	  * @return the value returned by `block`.
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transaction]] which runs the block in a fresh transaction instead.
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transactional]] which will reuse an implicit transaction if one 
	  *      is available and create a fresh one in the other case.      
	  */
	@deprecated("This method is useless. What was I thinking?", "release")
	def inTransaction[T](block: => T) :T

	/** Starts a fresh [[net.noresttherein.oldsql.ManagedAsset.Transaction transaction]] and executes the by-name
	  * argument block. The transaction will be automatically committed once control returns from the block.
	  * Any exceptions thrown, both by the block and from transaction API, will result in the transaction being
	  * rolled back instead. Some implementation may however choose to commit the transaction despite certain 
	  * exceptions being thrown; each such case should be documented explicitly in the class documentation. 
	  * If the commit fails - in either scenario - the transaction is being rolled back. All other thrown `Throwable` 
	  * are caught and automatically propagated after rolling back the transaction. Exceptions thrown from 
	  * transactional API (i.e., when committing or rolling back) are suppressed under the original cause, while other 
	  * `Throwable` take precedence over the original exception (which is suppressed under the rethrown `Throwable`).
	  * Note that implementation are free to wrap any caught exception being thrown, although standard exceptions
	  * from the [[net.noresttherein.oldsql.exceptions]] package should be used whenever a suitable class is available.
	  * @return the value returned by `block`.
	  * @see [[net.noresttherein.oldsql.ManagedAsset.inTransaction]]
	  * @see [[net.noresttherein.oldsql.ManagedAsset.transactional]]
	  */
	def transaction[T](block: => T) :T

	/** Executes the given block in a transaction. If an implicit transaction is available, it will be used
	  * as the argument for the block and it will be neither committed nor rolled back by this method before it returns
	  * (outside of the block itself calling [[net.noresttherein.oldsql.Asset.rollback rollback]]).
	  * If no implicit value of [[net.noresttherein.oldsql.Asset.Transaction Transaction]] is present 
	  * (or there is a conflict between competing values), a new instance will be created for the benefit of this
	  * block only. In that case, the transaction will be always closed when the method returns: if no exception
	  * is thrown, or the thrown exception is handled by 
	  * [[net.noresttherein.oldsql.Asset.caseRecoverable caseRecoverable]], the transaction is committed; 
	  * otherwise it is rolled back. 
	  *
	  * This method is thus equivalent to either [[net.noresttherein.oldsql.Asset.inTransaction inTransaction(block)]]
	  * or [[net.noresttherein.oldsql.Asset.transaction transaction(block)]], depending on the presence of 
	  * a preexisting implicit transaction.
	  * @return the value returned by `block`.
	  */
	def transactional[T](block: => T) :T

}






object ManagedAsset {

	abstract class AbstractManagedAsset extends ManagedAsset {

		protected def newTransaction :Transaction

		protected override def startTransaction() :Transaction = transactionOpt match {
			case Some(res) =>
				throw new PreexistingTransactionException(
					s"Cannot start a transaction for $this as one is already in progress: $res."
				)
			case _ => newTransaction
		}

		protected override def acquireTransaction() :Transaction = transactionOpt match {
			case Some(res) => res
			case _ => startTransaction()
		}

		protected override def currentTransaction :Transaction =
			currentTransaction(s"No transaction for $this in progress.")

		protected def currentTransaction(failureMessage: => String) :Transaction = transactionOpt match {
			case Some(res) => res
			case _ => throw new TransactionUnavailableException(failureMessage)
		}


		protected[oldsql] def clean() :Unit


		override def isTransactional :Boolean = transactionOpt.isDefined



		protected def commit(transaction :Transaction) :Unit


		protected def willRollback(transaction :Transaction) :Boolean

		override def willRollback :Boolean = willRollback(currentTransaction)

		protected def rollback(transaction :Transaction) :Unit

		override def rollback() :Unit =
			try {
				rollback(currentTransaction(noTransactionToRollbackMessage))
			} finally { clean() }

		override def rollback(cause :Throwable) :Unit =
			rollback(currentTransaction(noTransactionToRollbackMessage), cause)

		protected[oldsql] def noTransactionToRollbackMessage =
			s"Cannot roll back as no transaction for $this is in progress."

		private def rollback(transaction :Transaction, cause :Throwable) :Unit =
			try {
				rollback(transaction)
			} catch {
				case e :Exception => cause.addSuppressed(e)
				case e :Throwable =>
					e.addSuppressed(cause)
					clean()
					throw e
			} finally {
				clean()
			}


		private def rollbackAndThrow(transaction :Transaction)(result :Throwable) :Nothing = {
			try {
				rollback(transaction)
			} catch {
				case e :Throwable => result.addSuppressed(e)
			} finally {
				clean()
			}
			throw result
		}


		override def inTransaction[T](block: => T) :T = {
			currentTransaction; block
		}

		override def transaction[T](block: => T) :T = {
			val transaction = startTransaction()
			try {
				val result = block
				commit(transaction)
				result
			} catch {
				case e :Exception =>
					caseRecoverable.andThen(recover(transaction) _).applyOrElse(e, onFailure(transaction))
				case e :Throwable =>
					rollbackAndThrow(transaction)(e)
			}
		}

		override def transactional[T](block: => T) :T = 
			if (isTransactional) block else transaction(block)


		private def recover(transaction :Transaction)(error :Exception) :Nothing =
			try {
				commit(transaction)
				throw error
			} catch {
				case e :Exception =>
					e.addSuppressed(error)
					onFailure(transaction)(e)
				case e :Throwable =>
					e.addSuppressed(error)
					throw e
			}

		private def onFailure(transaction :Transaction)(error :Exception) :Nothing = {
			rollback(transaction, error)
			throw caseThrown.applyOrElse(error, identity[Exception])
		}

		protected def caseRecoverable :PartialFunction[Exception, Exception] = PartialFunction.empty
		protected def caseThrown :PartialFunction[Exception, Exception] = PartialFunction.empty

	}




	trait ManagedTransactionAPIAsset extends AbstractManagedAsset {
		override type Transaction <: TransactionAPI

		override def isTransactional :Boolean = transactionOpt.exists(_.isOpen)

		protected override def willRollback(transaction :Transaction) :Boolean = transaction.willRollback

		protected override def rollback(transaction :Transaction) :Unit = {
			transaction.rollback(); transaction.clean()
		}

		protected override def commit(transaction :Transaction) :Unit = {
			transaction.commit(); transaction.clean()
		}
	}

}






/** Default implementation of [[net.noresttherein.oldsql.ManagedAsset ManagedAsset]], which stores current transaction
  * context in a thread local variable.
  * @see [[net.noresttherein.oldsql.Asset]]
  */
trait ThreadAsset extends AbstractManagedAsset {
	private val threadTransaction = new ThreadLocal[Transaction]

	protected[oldsql] def setTransaction(transaction :Transaction) :Unit = threadTransaction.get match {
		case null => threadTransaction.set(transaction)
		case other =>
			throw new PreexistingTransactionException(
				s"Cannot start a transaction for $this for thread $threadFormat as one is already in progress: $other."
			)
	}

	protected override def startTransaction() :Transaction = threadTransaction.get match {
		case null => newTransaction
		case other =>
			throw new PreexistingTransactionException(
				s"Cannot start a transaction for $this for thread $threadFormat as one is already in progress: $other."
			)
	}

	protected override def acquireTransaction() :Transaction = threadTransaction.get match {
		case null => val res = newTransaction; threadTransaction.set(res); res
		case res => res
	}

	protected override def currentTransaction :Transaction = threadTransaction.get match {
		case null =>
			throw new TransactionUnavailableException(s"No transaction for $this in progress for thread $threadFormat.")
		case res => res
	}

	protected override def currentTransaction(failureMessage: =>String) :Transaction = threadTransaction.get match {
		case null => throw new TransactionUnavailableException(failureMessage)
		case res => res
	}

	protected override def transactionOpt :Option[Transaction] = Option(threadTransaction.get)


	protected[oldsql] override def clean() :Unit = threadTransaction.remove()


	override def isTransactional :Boolean = threadTransaction.get != null


	protected[oldsql] override def noTransactionToRollbackMessage :String =
		s"Cannot roll back as no transaction for $this for thread $threadFormat is in progress."

	protected def threadFormat :String = {
		val thread = Thread.currentThread
		val group = thread.getThreadGroup
		if (group != null) group.getName + "/" + thread.getName + "#" + thread.getId
		else thread.getName + "#" + thread.getId
	}

}






object ThreadAsset {

	/** Crates a new `ThreadAsset` using the provided by-name expression as the factory function of new transactions.
	  * Other transaction life cycle methods delegate to the corresponding methods of
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]].
	  */
	def apply(factory: => TransactionAPI) :ThreadAsset = ThreadAsset(null, factory)

	/** Crates a new `ThreadAsset` using the provided by-name expression as the factory function of new transactions.
	  * Other transaction life cycle methods delegate to the corresponding methods of
	  * [[net.noresttherein.oldsql.TransactionAPI TransactionAPI]].
	  * @param name the name used in the `toString` implementation of the created asset and in exception messages.
	  * @param factory generator of fresh transactions. Each evaluation should return a new transaction instance.
	  */
	def apply(name :String, factory: => TransactionAPI) :ThreadAsset =
		new ThreadAsset with ManagedTransactionAPIAsset {
			override type Transaction = TransactionAPI

			protected override def newTransaction = factory

			override def toString = if (name == null) super.toString else name
		}


	/** Adapts the given `Asset` to the `ManagedAsset` interface. Returned asset overrides implementations
	  * of all methods executing transactional blocks, delegating them to their counterparts of the adapted
	  * instance.
	  */
	def apply(asset :Asset) :ThreadAsset = new ThreadAsset {
		type Transaction = asset.Transaction

		protected override def newTransaction =
			throw new UnsupportedOperationException(
				s"ThreadAsset adapter of $asset does not support explicit transaction creation."
			)

		protected override def commit(transaction :Transaction) :Unit =
			throw new UnsupportedOperationException(
				s"ThreadAsset adapter of $asset does not support explicit transaction commit."
			)

		protected override def willRollback(transaction :Transaction) = asset.willRollback(transaction)

		protected override def rollback(transaction :Transaction) :Unit = asset.rollback()(transaction)

		override def isTransactional = super.isTransactional && asset.isTransactional(currentTransaction)


		override def abort() = asset.abort()(currentTransaction)
		override def abort(cause :Throwable) = asset.abort(cause)(currentTransaction)
		override def abort(message :String) = asset.abort(message)(currentTransaction)
		override def abort(message :String, cause :Throwable) = asset.abort(message, cause)(currentTransaction)


		override def inTransaction[T](block : => T) = asset.transactional { _ => block }(currentTransaction)

		override def transaction[T](block : => T) =
			try {
				asset.transaction { t => setTransaction(t); block }
			} finally {
				clean()
			}


		override def toString = asset.toString
	}

}


