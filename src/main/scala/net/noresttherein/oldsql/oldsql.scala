package net.noresttherein

import net.noresttherein.oldsql.collection.Chain.@~






package object oldsql {

	/** Library version; standard three level versioning, every level having three digits.
	  * Version 1.14.27 would be 1_014_027.
	  */
	final val OldSQLVer = 1L

	final val ImplArtifactDeprecation = "This class is an implementation artifact introduced to reduce code repetition " +
	                                    "and should not be referenced by the client code, as it may disappear " +
	                                    "without notice."

	final val SharedImplDeprecation = ImplArtifactDeprecation + "  Use one of its subclasses instead."

	final val DeprecatedAlways = "initial release"



	private[oldsql] def publishMutable() :Unit = java.lang.invoke.VarHandle.releaseFence()

	private[oldsql] val anyArg = { _ :Any => @~ }



	/** An implicit argument used to 'seal' methods to package `oldsql`. A method `private[scope]` or `protected[scope]`
	  * cannot be used outside package/class `scope`, but, unless final, it can be overriden by extending classes
	  * with a matching `public` definition. In some cases we can't declare the method `final` in all open classes,
	  * but we don't want it to leak in any form, including overriding, to client code. Accepting an implicit parameter
	  * of `Seal` (a value of which is always available) ensures that the method cannot be overriden, as `Seal`
	  * cannot be referenced outside this package. Note that this should not be dependent upon in the context 
	  * of security, as all such declarations are public in the bytecode and can thus be easily accessed from `Java`. 
	  */
	private[oldsql] final class Seal

	private[oldsql] object Seal {
		@inline def apply() :Seal = instance
		implicit final val instance = new Seal
	}

	/** A value wrapper with visibility restricted to package `oldsql`, ensuring that any definition including it
	  * can neither be used nor overriden by extending classes from outside this package. A declaration
	  * of `private[oldsql] val x :Int` is usable only within the specified scope, but an extending class from any
	  * package can always override it with a `val x :Int`. Adding a `final` modifier solves this source
	  * of interface leak but is not always possible if other classes from withing the package override/implement
	  * the field. Declaring it as `protected[oldsql] val x :Sealed[Int]` makes overriding impossible, as class `Sealed`
	  * can be neither extended, nor referenced from outside the package. Inlined implicit boxing and unboxing reduces 
	  * the syntax cost of this pattern. Note that this should not be dependent upon in the context of security, 
	  * as all such declarations are public in the bytecode and can thus be easily accessed from `Java`. 
	  */
	private[oldsql] class Sealed[+T](val value :T) extends AnyVal

	private[oldsql] object Sealed {
		@inline def apply[T](value :T) :Sealed[T] = new Sealed(value)

		@inline implicit def seal[T](value :T) :Sealed[T] = new Sealed(value)
		@inline implicit def unseal[T](value :Sealed[T]) :T = value.value
	}
}

