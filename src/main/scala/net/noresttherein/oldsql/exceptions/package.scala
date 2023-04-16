package net.noresttherein.oldsql

import scala.reflect.{classTag, ClassTag}
import scala.util.Try

import net.noresttherein.oldsql.exccceptions.RethrowContext






/**
  * @author Marcin Mościcki
  */
package object exceptions {

	private[oldsql] def newThrowable[E <: Throwable :ClassTag] :E =
		(Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[E]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[E]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[E]
		} recover {
			case ex :Exception => throw new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of a guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get

	private[oldsql] def newThrowable[E <: Throwable :ClassTag](msg :String) :E =
		(Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[E]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[E]
		} recover {
			case ex :Exception => throw new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass}($msg) as a result of a guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	private[oldsql] def newThrowable[E <: Throwable :ClassTag](msg :String, cause :Throwable) :E =
		(Try{
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, cause)
				.asInstanceOf[E]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg)
				.asInstanceOf[Throwable].initCause(cause).asInstanceOf[E]
		} recover {
			case _ :Exception => throw new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass}($msg, $cause) as a result of a guard failure: no constructor (String, Throwable) or (String)."
			)
		}).get


	private[oldsql] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw newThrowable[E]

	private[oldsql] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw newThrowable[E](msg)


	private[oldsql] final def rethrow[T](action: => T)(errorMessage: => String) :T =
		try { action } catch {
			case e :Throwable => throw pushErrorMessage(errorMessage).applyOrElse(e, identity[Throwable])
		}

	private[oldsql] final def guard[E <: Throwable] = new RethrowGuard[E] {}

	sealed trait RethrowGuard[E <: Throwable] extends Any {
		def apply[T](action: => T)(errorMessage: => String)(implicit E :ClassTag[E]) :T =
			try { action } catch {
				case E(e) =>
					throw pushErrorMessage(errorMessage).applyOrElse(
						e, { ex :Throwable => ex.addSuppressed(new RethrowContext(errorMessage, e)); ex }
					)
			}
	}


	private[oldsql] def pushErrorMessage(msg: => String) :PartialFunction[Throwable, Throwable] = {
		case e :OldSQLException                 => e.addInfo(msg)
		case e :NumberFormatException           => new NumberFormatException(msg).initCause(e)
		case e :IllegalArgumentException        => new IllegalArgumentException(msg, e)
		case e :UnsupportedOperationException   => new UnsupportedOperationException(msg, e)
		case e :NoSuchElementException          => new NoSuchElementException(msg).initCause(e)
		case e :IllegalStateException           => new IllegalStateException(msg).initCause(e)
		case e :ArrayIndexOutOfBoundsException  => new ArrayIndexOutOfBoundsException(msg).initCause(e)
		case e :StringIndexOutOfBoundsException => new StringIndexOutOfBoundsException(msg).initCause(e)
		case e :IndexOutOfBoundsException       => new IndexOutOfBoundsException(msg).initCause(e)
		case e :ClassCastException              => new ClassCastException(msg).initCause(e)
		case e :InterruptedException            => new InterruptedException(msg).initCause(e)
		case e :NullPointerException            => new NullPointerException(msg).initCause(e)
		case e :AbstractMethodError             => new AbstractMethodError(msg).initCause(e)
		case e :AssertionError                  => new AssertionError(msg, e)
	}

}



package exccceptions {
	class RethrowContext(msg :String, cause :Throwable = null) extends Exception(msg, cause, false, false)
}