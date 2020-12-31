package net.noresttherein.oldsql.exceptions


import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq
import scala.collection.mutable






trait OldSQLException extends Throwable {

	def suppressed :Seq[Throwable] = ArraySeq.unsafeWrapArray(getSuppressed)

	/** Reverses the exception stack, where `_.getCause` is treated as the next element on the list.
	  * The first exception of the returned list is the original cause (one without a cause), while this exception
	  * closes the list.
	  */
	def causeQueue :Seq[Throwable] = {
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

	val cause :Option[Throwable] = Option(getCause)

	val message :String = if (getMessage == null) "" else getMessage
}



/** Base class of all dedicated exceptions - other than the standard Java/Scala types - thrown by the library. */
class BaseOldSQLException protected (val msg :String, init :Throwable,
                                 enableSuppression :Boolean, writableStackTrace :Boolean)
	extends Exception(msg, init, enableSuppression, writableStackTrace) with OldSQLException
{
	def this(message :String, cause :Throwable) = this(message, cause, true, true)
	def this(cause :Throwable) = this(cause.toString, cause)
	def this(message :String) = this(message, null)
	def this() = this(null, null)
}



