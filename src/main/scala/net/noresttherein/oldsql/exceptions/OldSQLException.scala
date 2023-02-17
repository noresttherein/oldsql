package net.noresttherein.oldsql.exceptions

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable





/** Base ''trait'' of all exceptions thrown directly the framework, which either describe a specific situation,
  * or are thrown due to caller error (rather than an internal error). It is a trait so it can be mixed in to
  * standard Scala/Java exception classes, such as `NoSuchElementException`. The default, generic implementation
  * which, at the same time, serves as a base class for more specific implementations is
  * [[net.noresttherein.oldsql.exceptions.BaseOldSQLException BaseOldSQLException]]. An instance can be created
  * simply by factory `apply` method of the [[net.noresttherein.oldsql.exceptions.OldSQLException$ companion]] object
  * of this class.
  */
trait OldSQLException extends Throwable {

	/** A 'virtual constructor' returning an exception of the same type as this one, with the given message,
	  * and this exception as its cause. This is useful when we want to preserve both the original exception type
	  * and the initial exception as the reason, in case someone higher on the call stack would want to handle it,
	  * but also to add additional contextual information about the failed operation, such as original parameters.
	  * This is different than [[java.lang.Throwable.addSuppressed addSuppressed]] in that the latter implies
	  * another error (though possibly related) which happened when processing this exception, while this method
	  * is used in catch-rethrow scenarios. Also, the latter accepts a new exception instance from the caller,
	  * who may pass any type of the exception but typically does not know the precise type of this
	  * exception.
	  */
	def addInfo(msg :String) :OldSQLException

	/** Standard [[Throwable.getSuppressed getSuppressed]] array as a scala [[Seq]]. */
	def suppressed :Seq[Throwable] = ArraySeq.unsafeWrapArray(getSuppressed)

	/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
	  * The first exception of the returned list is the original cause (one without a cause), while this exception
	  * closes the list.
	  */
	lazy val causeQueue :Seq[Throwable] = {
		val dejaVu = mutable.Set[Throwable]()
		@tailrec def push(e :Throwable, result :List[Throwable]) :Seq[Throwable] = e.getCause match {
			case null => e::result
			case cause if dejaVu(cause) =>
				val cycle = new BaseOldSQLException(s"Cycle detected in the exception stack: ${e.getCause} already present.")
				cycle::e::result
			case cause => dejaVu += e; push(cause, e::result)
		}
		push(this, Nil)
	}

	/** Standard [[Throwable.getCause getCause]] wrapped in an [[Option]]. */
	val cause :Option[Throwable] = Option(getCause)

	/** Denullified [[Throwable.getMessage getMessage]] returning an empty string instead of `null` if no message
	  * was provided. */
	val message :String = if (getMessage == null) "" else getMessage

	def className :String = getClass.getName

	override def toString :String = {
		val s :String = className
		val message :String = getLocalizedMessage
		if (message != null) s + ": " + message else s
	}
}



object OldSQLException {
	def apply(msg :String, cause :Throwable = null) :OldSQLException =
		new BaseOldSQLException(msg, cause) {
			override def className = classOf[OldSQLException].getName
		}

	def unapply(e :Throwable) :Option[(String, Option[Throwable])] = e match {
		case ex :OldSQLException => Some((ex.message, ex.cause))
		case _ => None
	}
}



/** Base class of all dedicated exceptions - other than the standard Java/Scala types - thrown by the library. */
class BaseOldSQLException protected (val msg :String, init :Throwable,
                                     enableSuppression :Boolean, writableStackTrace :Boolean)
	extends RuntimeException(msg, init, enableSuppression, writableStackTrace) with OldSQLException
{
	def this(message :String, cause :Throwable) = this(message, cause, true, true)
	def this(cause :Throwable) = this(cause.toString, cause)
	def this(message :String) = this(message, null)
	def this() = this(null, null)

	override def addInfo(msg :String) :OldSQLException = OldSQLException(msg, this)

//	override def toString :String = {
//		val msg = getLocalizedMessage
//		if (msg == null) classOf[OldSQLException].getName
//		else classOf[OldSQLException].getName + ": " + msg
//	}
}





/** An exception indicating that the error is (most likely) a result of a bug in the framework
  * (or its custom extensions), rather than in the application code or a data error.
  */
trait Bug extends OldSQLException

object Bug {

	def apply(msg :String, cause :Throwable = null) :Bug = new BugException(msg, cause)

	def unapply(e :Throwable) :Option[(String, Option[Throwable])] = e match {
		case bug :Bug => Some((bug.message, bug.cause))
		case _ => None
	}

	private class BugException(msg :String, cause :Throwable = null)
		extends BaseOldSQLException(msg, cause) with Bug
	{
		override def addInfo(msg :String) :OldSQLException = Bug(msg, this)
		override def className = classOf[Bug].getName
	}
}

