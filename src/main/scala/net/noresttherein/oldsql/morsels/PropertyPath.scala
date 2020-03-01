package net.noresttherein.oldsql.morsels

import java.lang.reflect.{Method, Modifier}
import net.noresttherein.oldsql.morsels.InvocationReflection.Trace
import net.noresttherein.oldsql.morsels.PropertyPath.UpdatableProperty
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.slang._

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._


/** A chain of zero-argument method calls starting from type `X` and returning a value of type `Y`,
  * for example `(_:Company).department.director.favoritePony`.
  * This class represents both the function itself, so it can be used to obtain a value of `Y`
  * given input `X`, but also its reflection - it knows what methods are called in sequence.
  * This has two important bonuses: first, it has a printable name/toString listing the names
  * of called methods (so `property.name` will return "department.director.favoritePony" in our example),
  * but all instances representing the same property chain will be equal, regardless of the implementation
  * of the function used to create it (so `PropertyPath[Company](_.department.director.favoritePony)`,
  * `PropertyPath((_:Company).department andThen _.favoritePony)`,
  * `PropertyPath[Company]{ x => val brony = x.department.director; brony.favoritePony }` will all be equal).
  * This makes it an easy descriptor for identifying and indexing properties of objects without resorting to traditional
  * use of Strings whenever the methods to be called are not statically known.
  *
  * This class tries to take into account that methods may be overriden, and an overriding property is
  * considered equal to the overriden property in the super class - this may or may not be desirable;
  * on one hand, we would expect that since both calls will return the same value for the same argument,
  * they should be equal; on another, static result type may be different and invoking a `PropertyPath[S, Int]` on a value
  * of `T>:S` may throw a  `ClassCastException` (or other), even if the underlying property is declared/first defined
  * in class `T`, while an equal `PropertyPath[T, Int]` / `PropertyPath[T, Any]` may not.
  * This decision was made as it was designed to facilitate the creation of property mappings where such coalescence
  * is performed, rather than be a generic reflection mechanism. If this might be an issue, check the `definedFor`
  * property which returns reflected static type of the argument of the function used to create this instance.
  * Do not assume that it is safe to cast a property chain to a given argument type because it equals an instance
  * which static type would guarantee a safe call on this type. Instead, use them as keys - given a 'safe' instance
  * computed beforehand based on a function which is known to properly represent the property in question, compare it
  * with the passed function/property path, and if they are equal, always use 'yours' rather then the given one.
  *
  * @param definedFor Static argument type (`X`) used to create this instance. It might be different than `X` in place
  *                   of usage and is not necessarily the type of the class where the property was declared.
  * @param fun function returning the value of the property which was used to create this instance.
  * @tparam X type of accepted argument (property owner).
  * @tparam Y return type of the property.
  * @author Marcin Mościcki
  */
sealed abstract class PropertyPath[-X, +Y] private[PropertyPath](final val definedFor :Type, final val fun :X=>Y)  {

	/** Concatenated names of all methods called by fun/this instance on its argument, separated by '.'. */
	def name :String

	/** Chain call all properties defined by this sequence and return the value returned by the last one. */
	def apply(x :X) :Y = fun(x)

	/** The (potentially erased) runtime class of the argument type `X`. */
	def argumentClass :Class[_] =
		runtimeMirror(getClass.getClassLoader).runtimeClass(definedFor.dealias.erasure.typeSymbol.asClass)

	/** Can this chain be safely invoked for arguments of type T? Checks if typeOf[T]&lt;:&lt;this.definedFor. */
	def isApplicableTo[T :TypeTag] :Boolean = typeOf[T] <:< definedFor

	/** Return an instance representing chained call of properties in this instance, followed by invoking the calls
	  * specified by suffix on the return value.
	  */
	def andThen[Z](suffix :PropertyPath[Y, Z]) :PropertyPath[X, Z]

	/** Return an instance representing chained property call of properties specified by prefix, followed by this chain. */
	def compose[Z](prefix :PropertyPath[Z, X]) :PropertyPath[Z, Y] = prefix andThen this

	/** Return an instance representing chained call of properties in this instance, followed by invoking the calls
	  * specified by suffix on the return value.
	  */
	def andThen[YY>:Y, Z](suffix :YY=>Z)(implicit tag :TypeTag[YY]) :PropertyPath[X, Z] = this andThen PropertyPath(suffix)

	/** Return an instance representing chained property call of properties specified by prefix, followed by this chain. */
	def compose[Z :TypeTag](prefix :Z=>X) :PropertyPath[Z, Y] = PropertyPath(prefix) andThen this

	/** Is this instance a proper prefix of the given property chain, meaning when invoked on the same argument XX,
	  * property will make make the same calls as this instance, followed by at least one more chained property getter.
	  * This implies that property.name.startsWith(this.name), but not the other way round.
	  */
	def prefixOf[XX <: X, Z](property :PropertyPath[XX, Z]) :Boolean

	/** If this instance equals prefix andThen suffix for some PropertyPath suffix, return this suffix. */
	def drop[XX <: X, Z](property :PropertyPath[XX, Z]) :Option[PropertyPath[Z, Y]]

	/** Is this a single property accessor representing a single method call?*/
	def isSimple :Boolean

	/** Given a function which clones it's argument and substitutes the value of this property to its second argument,
	  * create an instance which can be used to both get and set the value of the represented property.
	  * No check is performed if this function represents calls which are in any way related to the chain calls represented by this instance!
	  */
	def updatable[XX <: X :TypeTag, YY >: Y](set: (XX, YY) => XX): UpdatableProperty[XX, YY]



	override def toString :String = name

}






/** Reflection mechanism, which, given a function `X=>Y`, investigates what methods are called on the argument `X`
  * to return value `Y`. It is intended to create identifiers for class properties (zero argument methods), including
  * chained properties of the form `(_:Company).department.director.favouritePony`. When given a function of this form,
  * a `PropertyPath` can be created which will list all methods called and provide a String representation of the call
  * as it would appear in the code (i.e. "department.director.favouritePony". `PropertyPath`s of matching types
  * can be concatenated or 'subtracted' in a type safe manner to represent adding or removing property calls to
  * and from the ends of the chain and are type safe identifiers of reflected properties. They also have a meaningful
  * `equals` implementation (slightly stronger than equality of names, but by design not differentiating between
  * overriden properties).
  *
  * It is important to note, that passed function doesn't have to be a literal property chain; intermediate  results
  * can be stored, it may (though shouldn't) produce side effects, or be composed dynamically of individual functions
  * (for example `f1.flatMap(f2) andThen f3`). It shouldn't however be conditional or contain any inspection of the returned
  * values, as they will be mocks. The only things validated are:
  *   1. only a single method call is made for each object (so `{ x => x._1; x._2 }` will result in an exception
  *      at reflection time),
  *   1. the value returned by the function is actually the value returned by the mock (so `{ x => x._1; 1 }`
  *      will produce an exception at reflection time).
  *
  * This is implemented using runtime code generation, by recursive deep mocking of the argument type and all
  * returned values on a best effort basis. It is an impossibility to provide a generic, robust solution to this
  * problem which would work in every case; any meaningful validation performed in the constructors will always foil it
  * and the extent to which this implementation goes in order to be able to call constructors of instrumented classes
  * may prove to be an issue in the case of large, complex graphs and constructors performing additional logic apart
  * from storing the state. In particular, known limitations and potential issues are:
  *
  *   - all classes which properties are reflected on the path must be not final, be public and have
  *     a public or protected constructor;
  *   - classes taken  as constructor arguments by the reflected classes must be instantiable through a public
  *     constructor or instrumentable themselves;
  *   - the constructor should perform minimal validation, in particular no domain-specific validation.
  *     Implementation will try its best to provide non-null and not-mocked arguments as well as try several
  *     super constructors before giving up;
  *   - cycles between constructors can cause exceptions;
  *   - the constructor's shouldn't allocate large structures or claim resources, otherwise leaks
  *     (or at least bottlenecks) can occur.
  *   - reliance on a large number of varying injected arguments can lead to potentially creating huge graphs of
  *     mock classes and instances in an attempt to trace the call;
  *   - the methods called need to be idempotent, reflecting only the state of the entity and represent
  *     persistent properties of the entities (possibly calculated from other fields). While their actual
  *     implementations are never called and this is not enforced, an error will result when attempting to map
  *     it to persistent storage;
  *   - no support for abstract types defined in outside scopes (generic classes are handled correctly as long as their
  *     type arguments are fully instantiated when seen/followed from type `X`);
  *
  * While these conditions severely limits its application, it shouldn't be a problem for a typical business domain
  * model for which it is intended as a replacement of traditional `String` literals.
  */
object PropertyPath {
	/** Shortcut for optional infix notation for PropertyPath: val property :X===>Y. */
	type ===>[-X, +Y] = PropertyPath[X, Y]



	/** PropertyPath enriched by a copy function which clones the argument and substitutes the value of the property for
	  * the one given. This doesn't represent a mutable property (var).
	  */
	sealed trait UpdatableProperty[X, Y] extends PropertyPath[X, Y] {
		def update(x :X, value :Y) :X
	}


	/** Return a property chain which provides an update method (expected to clone the argument rather than mutate its state
	  * in addition to the getter. No check is performed that passed getter and setter are actually related in any way,
	  * so use this in static context, when it is apparent.
	  */
	@inline def UpdatableProperty[S :TypeTag, T](get :S => T, set :(S, T) => S) :UpdatableProperty[S, T] =
		PropertyPath.property(get).updatable(set)






	/** Factory for property chains */
	trait PropertyTracer[X] extends Any {

		/** Discover what properties are accessed by function `property` and create their reflection.
		  * @param property a function constituting of chained property calls on its argument.
		  */
		@inline def apply[Y](property :X=>Y)(implicit tag :TypeTag[X]) :ReflectedPropertyPath[X, Y] =
			PropertyPath.property(property)

		/** Check if this function represents a chained property call (i.e. `_.department.director.favoritePony`),
		  * and if so, return their reflected representation as an option.
		  */
		@inline def maybe[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[ReflectedPropertyPath[X, Y]] =
			PropertyPath.maybe(property)

		/** Assuming the given function is equivalent to a single zero-argument method call on the argument,
		  * return its reflected representation.
		  */
		@inline def simple[Y](property :X=>Y)(implicit tag :TypeTag[X]) :SimpleProperty[X, Y] =
			PropertyPath.simple(property)

		/** Check if this function represents a single zero-argument method call on its argument,
		  * and if so, return its reflected representation as an option.
		  */
		@inline def ifSimple[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[SimpleProperty[X, Y]] =
			PropertyPath.ifSimple(property)

		/** Return a hacked, manually created instance representing the given property '''without''' performing any
		  * reflection or mocking.
		  * @param property backing function returning the value of the represented property
		  * @param name the name of the represented property, or names of chained properties separated by '.'.
		  */
		def hacked[Y](property :X=>Y, name :String)(implicit tag :TypeTag[X]) :PropertyPath[X, Y] =
			new HackedPropertyPath[X, Y](name, property, typeOf[X])

		/** Reflect the property and return its name. */
		@inline def name[Y](property :X=>Y)(implicit tag :TypeTag[X]) :String =
			PropertyPath.property(property).name

		/** Try to reflect the property given the function and return its name in an option if successful. */
		@inline def nameOpt[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[String] =
			PropertyPath.maybe(property).map(_.name)

	}


	/** Convenience factory for property chains (reflected representations of chained zer-argument method calls),
	  * which takes a single type parameter of the argument type (class type declaring the properties we want to reflect).
	  * Saves specifying both the argument and return types, as the latter can be easily inferred by the compiler.
	  * @tparam X type of the class declaring the first property in the chain we want to inspect.
	  */
	@inline def apply[X] :PropertyTracer[X] = new PropertyTracer[X] {}



	/** Check if this function represents a chained property call (i.e. _.department.director.favoritePony),
	  * and if so, return their reflected representation as an option.
	  */
	@inline def maybe[X :TypeTag, Y](property :X=>Y) :Option[ReflectedPropertyPath[X, Y]] =
		scala.util.Try { this.property(property) }.toOption

	/** Assuming the given function is a single zero-argument method call on the argument,
	  * return its reflected representation.
	  */
	@inline def simple[X :TypeTag, Y](property :X=>Y) :SimpleProperty[X, Y] =
		this.property(property) match {
			case s :SimpleProperty[_, _] => s.asInstanceOf[SimpleProperty[X, Y]]
			case p => throw new IllegalArgumentException(s"Passed function doesn't represent a single property call: $p")
		}

	/** Check if this function represents a single zero-argument method call on its argument,
	  * and if so, return its reflected representation as an option.
	  */
	@inline def ifSimple[X :TypeTag, Y](property :X=>Y) :Option[SimpleProperty[X, Y]] =
		this.property(property).asSubclass[SimpleProperty[X, Y]]

	/** Equivalent to apply(property) */
	@inline def property[X :TypeTag, Y](property :X=>Y) :ReflectedPropertyPath[X, Y] =
		property.asSubclassOf[ReflectedPropertyPath[X, Y]] getOrElse trace(property)

	/** Assuming property constitutes of chained calls of zero-argument methods starting with type X,
	  * create a reflected representation which can be compared, composed and even subtracted in type safer manner
	  * with other property chain instances.
	  */
	def apply[X :TypeTag, Y](property :X=>Y) :ReflectedPropertyPath[X, Y] =
		property.asSubclassOf[ReflectedPropertyPath[X, Y]] getOrElse trace(property)


	/** Return a hacked, manually created instance representing the given property '''without''' performing
	  * any reflection or mocking.
	  * @param property backing function returning the value of the represented property
	  * @param name the name of the represented property, or names of chained properties separated by '.'.
	  */
	def apply[X :TypeTag, Y](property :X=>Y, name :String) :PropertyPath[X, Y] =
		new HackedPropertyPath[X, Y](name, property, typeOf[X])


	/** Reflect the property, creating a property chain and return its name. */
	def nameOf[X :TypeTag, Y](property :X=>Y) :String = apply(property).name

	/** Reflect the property, creating a property chain and return its name, swallowing any failure in None. */
	def nameOpt[X :TypeTag, Y](property :X=>Y) :Option[String] = maybe(property).map(_.name)






	private def trace[S :TypeTag, T](property :S => T) :ReflectedPropertyPath[S, T] = {
		def rec[X](ownerType :Type, stack :List[Trace]) :ReflectedPropertyPath[X, T] = stack match {
			case Trace(_, method, resultType)::Nil =>
				new SimpleProperty[X, T](ownerType, PropertyCall(method))
			case Trace(_, method, resultType)::tail =>
				new ChainedProperty[X, T](ownerType, PropertyCall(method), rec(resultType, tail))
			case Nil =>
				throw new PropertyReflectionException(s"Reflected call of $property for ${typeOf[S]} didn't call any methods on the argument.")
		}
		rec(typeOf[S], InvocationReflection(property))
	}






	/** Automatic reflection didn't work? Try using this stand-in instead. It will equal (symmetrically) any
	  * other `PropertyPath`, reflected or manually created, based solely on the name.
	  * Be warned that drop function will always return None, as without reflection we have no means of
	  * decomposing the associated function into the dropped prefix and suffix functions.
	  */
	class HackedPropertyPath[-X, +Y] private[PropertyPath](val name :String, getter :X=>Y, argType :Type)
		extends PropertyPath[X, Y](argType, getter)
	{
		def this(name :String, fun :X=>Y)(implicit argTag :TypeTag[X]) = this(name, fun, typeOf[X])

		if (name.length==0 || name(0).isWhitespace || name(name.length-1).isWhitespace)
			throw new IllegalArgumentException(s"Illegal property name: '$name'")

		def isSimple :Boolean = !name.contains('.')

		/** Returns a hacked, updatable instance. */
		override def updatable[XX <: X : TypeTag, YY >: Y](set: (XX, YY) => XX): UpdatableProperty[XX, YY] =
			new HackedPropertyPath[XX, YY](name, fun, definedFor) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		/** Returns None. */
		override def drop[XX <: X, Z](property: PropertyPath[XX, Z]): Option[PropertyPath[Z, Y]] = None


		/** Checks if the name of the resulting property starts with this property's name. */
		override def prefixOf[XX <: X, Z](property: PropertyPath[XX, Z]): Boolean = property.name startsWith name

		/** Return a hacked instance with a name being the concatenation of both names separated by a '.'
		  * and function this.fun andThen suffix.fun. */
		override def andThen[Z](suffix: PropertyPath[Y, Z]): PropertyPath[X, Z] =
			new HackedPropertyPath[X, Z](name + '.' +suffix.name, fun andThen suffix.fun, definedFor)

		/** Return a hacked instance with a name being the concatenation of both names separated by a '.'
		  * and function prefix.fun adnThen this.fun.
		  */
		def prepend[S, XX<:X](prefix :PropertyPath[S, XX]) :PropertyPath[S, Y] =
			new HackedPropertyPath[S, Y](prefix.name+"."+name, prefix.fun andThen fun, prefix.definedFor)

		override def equals(that :Any) :Boolean = that match {
			case p:PropertyPath[_, _] =>
				(definedFor<:<p.definedFor || p.definedFor<:<definedFor) && name==p.name
			//				definedFor =:= p.definedFor && name == p.name
			case _ => false
		}

		override def hashCode :Int = name.hashCode
	}






	/** Base class for the reflected, proper instances of the `PropertyPath`. */
	sealed abstract class ReflectedPropertyPath[-X, +Y] private[PropertyPath]
			(argType :Type, private[PropertyPath] val method :PropertyCall, fun :X=>Y)
		extends PropertyPath[X, Y](argType, fun)
	{

		def drop[XX <: X, Z](property: PropertyPath[XX, Z]): Option[ReflectedPropertyPath[Z, Y]]

		/** Return a proper reflected instance representing composition of this function followed by the invocation of
		  * the given function.
		  */
		def andThen[Z](suffix: ReflectedPropertyPath[Y, Z]): ReflectedPropertyPath[X, Z]
	}



	/** PropertyPath consisting of a single property call. */
	sealed class SimpleProperty[-S, +T] private[PropertyPath](tpe :Type, method :PropertyCall, _property :S=>T)
		extends ReflectedPropertyPath[S, T](tpe, method, _property)
	{

		private[PropertyPath] def this(tpe :Type, method :PropertyCall) =
			this(tpe, method, x => method.method.invoke(x).asInstanceOf[T])

		final val name = method.name


		final override def andThen[Z](suffix: PropertyPath[T, Z]): PropertyPath[S, Z] = suffix match {
			case p :HackedPropertyPath[_, _] =>
				p.asInstanceOf[HackedPropertyPath[T, Z]] prepend this
			case r :ReflectedPropertyPath[_, _] =>
				this andThen r.asInstanceOf[ReflectedPropertyPath[T, Z]]
		}


		final override def andThen[Z](suffix: ReflectedPropertyPath[T, Z]): ReflectedPropertyPath[S, Z] =
			new ChainedProperty[S, Z](definedFor, this.method, fun andThen suffix.fun, suffix)

		final override def prefixOf[XX <: S, Z](property: PropertyPath[XX, Z]): Boolean = property match {
			case p:ChainedProperty[_, _] => method == p.method
			case p:HackedPropertyPath[_, _] => p == this
			case _ => false
		}

		final override def drop[XX <: S, Z](property: PropertyPath[XX, Z]): Option[ReflectedPropertyPath[Z, T]] = None

		final override def isSimple: Boolean = true


		final override def updatable[XX <: S : TypeTag, YY >: T](set: (XX, YY) => XX): SimpleProperty[XX, YY] with UpdatableProperty[XX, YY] =
			new SimpleProperty[XX, YY](typeOf[XX], method, fun) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		override def equals(that :Any) :Boolean = that match {
			case s :SimpleProperty[_,_] =>
				(this eq s) || method == s.method
			case _ :HackedPropertyPath[_, _] => that == this
			case _ => false
		}

		override def hashCode :Int = name.hashCode

	}



	private class ChainedProperty[-S, +T]
			(tpe :Type, _method :PropertyCall, _property :S=>T, private final val tail :ReflectedPropertyPath[_, T])
		extends ReflectedPropertyPath[S, T](tpe, _method, _property)
	{
		private[PropertyPath] def this(tpe :Type, method :PropertyCall, tail :ReflectedPropertyPath[Any, T]) =
			this(tpe, method, x => tail.fun(method.method.invoke(x)), tail)

		final val name = method.name + "." + tail.name



		final override def andThen[Z](suffix: PropertyPath[T, Z]): PropertyPath[S, Z] = suffix match {
			case p :HackedPropertyPath[_, _] =>
				p.asInstanceOf[HackedPropertyPath[T, Z]] prepend this
			case p :ReflectedPropertyPath[_, _] =>
				andThen(p.asInstanceOf[ReflectedPropertyPath[T, Z]])
		}

		final override def andThen[Z](suffix: ReflectedPropertyPath[T, Z]): ReflectedPropertyPath[S, Z] =
			new ChainedProperty[S, Z](definedFor, this.method, this.fun andThen suffix.fun, tail andThen suffix)

		final override def prefixOf[XX <: S, Z](property: PropertyPath[XX, Z]): Boolean = property match {
			case p :ChainedProperty[_, _] =>
				this.method == p.method && tail.prefixOf(p.tail)
			case _ => false
		}


		final override def drop[XX <: S, Z](property: PropertyPath[XX, Z]): Option[ReflectedPropertyPath[Z, T]] = property match {
			case p :HackedPropertyPath[_, _] =>
				val next = p.name.indexOf('.')
				if (next >= 0)
					if (p.name.substring(0, next) == this.method.name)
						tail.drop(new HackedPropertyPath[Nothing, Z](p.name.substring(next+1), _ => ???, tail.definedFor))
					else None
				else
					tail.asInstanceOf[ReflectedPropertyPath[Z, T]] providing p.name == method.name
			case p :ReflectedPropertyPath[_, _] if method != p.method => None
			case p :SimpleProperty[_, _] => Some(tail.asInstanceOf[ReflectedPropertyPath[Z, T]])
			case p :ChainedProperty[_, _] => tail.drop(p.tail)
			case _ => None
		}

		final override def isSimple: Boolean = false



		override def updatable[XX <: S : TypeTag, YY >: T](set: (XX, YY) => XX): UpdatableProperty[XX, YY] =
			new ChainedProperty[XX, YY](typeOf[XX], method, fun, tail) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		override def equals(that :Any) :Boolean = that match {
			case p :ChainedProperty[_, _] =>
				(this eq p) || (method == p.method && tail == p.tail)
			case _ :HackedPropertyPath[_, _] => that == this
			case _ => false
		}

		override def hashCode :Int = name.hashCode
	}






	/** A wrapper for all exceptions thrown during the reflection */
	class PropertyReflectionException(msg :String, cause :Throwable) extends RuntimeException(msg, cause) {
		def this(msg :String) = this(msg, null)
		def this(cause :Throwable) = this(null, cause)
	}



	final private[PropertyPath] class PropertyCall(private[PropertyPath] val method :Method) {
		def name :String = method.getName

		private lazy val supers = {
			def declarations(clazz :Class[_], method :Method, res :ListBuffer[Method]=ListBuffer()) :ListBuffer[Method] = {
				def overridden(m :Method) =
					!Modifier.isPrivate(m.getModifiers) &&
						m.getName == method.getName &&
						m.getReturnType.isAssignableFrom(method.getReturnType)

				clazz.getDeclaredMethods.filter(overridden).foreach(res += _)
				Option(clazz.getSuperclass).foreach(declarations(_, method, res))
				clazz.getInterfaces.foreach(declarations(_, method, res))
				res
			}
			declarations(method.getDeclaringClass, method).toSet
		}

		override def equals(that :Any) :Boolean = that match {
			case p:PropertyCall =>
				(this eq p) || (method == p.method) || (method.getName==p.method.getName && (supers & p.supers).nonEmpty)
			case _ => false
		}

		override def hashCode :Int = method.hashCode

		override def toString :String = method.toString

	}



	private[PropertyPath] def PropertyCall(method :Method) :PropertyCall = {
		var overflow = PropertyCallCache.mappingCount - MaxPropertyCallCacheSize
		if (overflow > 0) {
			val i = PropertyCallCache.entrySet().iterator()
			while (overflow > 0 && i.hasNext) {
				PropertyCallCache.remove(i.next())
				overflow -= 1
			}
		}
		PropertyCallCache.computeIfAbsent(method, method => new PropertyCall(method))
	}







	private[this] val MaxPropertyCallCacheSize = 4000
	private[this] val PropertyCallCache =
		new java.util.concurrent.ConcurrentHashMap[Method, PropertyCall](MaxPropertyCallCacheSize)


}
