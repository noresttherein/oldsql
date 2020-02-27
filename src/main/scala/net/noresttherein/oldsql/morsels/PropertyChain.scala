package net.noresttherein.oldsql.morsels

import java.{lang=>j}
import j.reflect.{Field, Method, Modifier}

import net.bytebuddy.ByteBuddy
import net.bytebuddy.matcher.ElementMatchers._
import net.bytebuddy.implementation.MethodDelegation._
import net.bytebuddy.implementation.bind.annotation.{Origin, RuntimeType, This}
import net.bytebuddy.implementation.FixedValue
import net.noresttherein.oldsql.morsels.PropertyChain.UpdatableProperty
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.slang._

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.concurrent.{Map => ConcurrentMap, TrieMap => ConcurrentTrieMap}
import scala.reflect.runtime.universe._


/** A chain of zero-argument method calls starting from type `T` and returning a value of type `Y`,
  * for example `(_:Company).department.director.favoritePony`.
  * This class represents both the function itself, so it can be used to obtain a value of `Y`
  * given input `X`, but also its reflection - it actually knows what methods are called in sequence.
  * This has two important bonuses: first, it has a printable name/toString listing the names
  * of called methods (so `property.name` will return "department.director.favoritePony" in our example),
  * but all instances representing the same property chain will be equal, regardless of the implementation
  * of the function used to create it (so `PropertyChain[Company](_.department.director.favoritePony)`,
  * `PropertyChain((_:Company).department andThen _.favoritePony)`,
  * `PropertyChain[Company]{ x => val brony = x.department.director; brony.favoritePony }` will all be equal).
  * This makes it an easy handle for identifying and indexing properties of objects without resorting to traditional
  * use of Strings whenever the methods to be called are not statically known.
  *
  * This class tries to take into account that methods may be overriden, and an overriding property is
  * considered equal to the overriden property in the super class - this may or may not be desirable;
  * on one hand, we would expect that since both calls will return the same value for the same argument,
  * they should be equal; on another, static result type may be different and, depending on the actual
  * implementation of the function used to create the instance, invoking a `PropertyChain[S, Int]` on a value
  * of `T>:S` may throw a  `ClassCastException` (or other), even if the underlying property is declared/first defined
  * in class `T`, while an equal `PropertyChain[T, Int]` / `PropertyChain[T, Any]` may not. If this might be an issue, check
  * `definedFor` property which returns reflected static type of the argument of the function used to create this instance.
  * Do not assume that it is safe to cast a property chain to a given argument type because it equals an instance
  * which static type would guarantee a safe call on this type. Instead, use them as keys - given a 'safe' instance computed beforehand
  * based on a function which is known to properly represent the property in question, compare it with the passed function/property chain,
  * and if they are equal, always use 'yours' rather then the given one.
  *
  * @param definedFor Static argument type (X) used to create this instance. It might be different than X in place of usage
  *                   and is not necessarily the type of the class where the property was declared
  * @param fun function returning the value of the property which was used to create this instance
  * @tparam X type of accepted argument (property owner)
  * @tparam Y return type of the property.
  */
sealed abstract class PropertyChain[-X, +Y] private[PropertyChain] (final val definedFor :Type, final val fun :X=>Y)  {

	/** Concatenated names of all methods called by fun/this instance on its argument, separated by '.' */
	def name :String

	/** Chain call all properties defined by this sequence and return the value returned by the last one. */
	def apply(x :X) :Y = fun(x)

	/** Can this chain be safely invoked for arguments of type T? Checks if typeOf[T]<:<this.definedFor */
	def isApplicableTo[T :TypeTag] :Boolean = typeOf[T] <:< definedFor

	/** Return an instance representing chained call of properties in this instance, followed by invoking the calls
	  * specified by suffix on the return value.
	  */
	def andThen[Z](suffix :PropertyChain[Y, Z]) :PropertyChain[X, Z]

	/** Return an instance representing chained property call of properties specified by prefix, followed by this chain */
	def compose[Z](prefix :PropertyChain[Z, X]) :PropertyChain[Z, Y] = prefix andThen this

	/** Return an instance representing chained call of properties in this instance, followed by invoking the calls
	  * specified by suffix on the return value.
	  */
	def andThen[YY>:Y, Z](suffix :YY=>Z)(implicit tag :TypeTag[YY]) :PropertyChain[X, Z] = this andThen PropertyChain(suffix)

	/** Return an instance representing chained property call of properties specified by prefix, followed by this chain */
	def compose[Z :TypeTag](prefix :Z=>X) :PropertyChain[Z, Y] = PropertyChain(prefix) andThen this

	/** Is this instance a proper prefix of the given property chain, meaning when invoked on the same argument XX,
	  * property will make make the same calls as this instance, followed by at least one more chained property getter.
	  * This implies that property.name.startsWith(this.name), but not the other way round.
	  */
	def prefixOf[XX <: X, Z](property :PropertyChain[XX, Z]) :Boolean

	/** If this instance equals prefix andThen suffix for some PropertyChain suffix, return this suffix */
	def drop[XX <: X, Z](property :PropertyChain[XX, Z]) :Option[PropertyChain[Z, Y]]

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
  * a `PropertyChain` can be created which will list all methods called and provide a String representation of the call
  * as it would appear in the code (i.e. "department.director.favouritePony". `PropertyChain`s of matching types
  * can be concatenated or 'subtracted' in a type safe manner to represent adding or removing property calls to
  * and from the ends of the chain, and are type safe identifiers of reflected properties, and with a proper equals
  * implementation they can be used to index/list interesting values.
  *
  * This is implemented by creating a mock for the argument type, passing it to the given function and recording all
  * invoked calls.
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
  * Known limitations: due to the complexity of scala type system and java erasure, properties of generic classes returning
  * values of type parameters may not be mocked correctly; at this moment, only methods for which the result type is either
  * a single statically known class or type argument taken by the declaring class are supported. Everything here
  * is a heuristic and will fall apart given sufficiently mean use cases involving complex types or type hierarchies.
  * This is not a complete, safe reflection mechanism, and was created solely with the purpose of eliminating storing
  * and passing property/component names as strings. If the methods called do something else then simply return
  * a stored value, perform any validation or computations, unforeseeable results may occur.
  * It should work however for most model classes representing business domain (simple hierarchies of case classes).
  */
object PropertyChain {
	/** Shortcut for optional infix notation for PropertyChain: val property :X===>Y */
	type ===>[-X, +Y] = PropertyChain[X, Y]



	/** PropertyChain enriched by a copy function which clones the argument and substitutes the value of the property for
	  * the one given. This doesn't represent a mutable property (var).
	  */
	sealed trait UpdatableProperty[X, Y] extends PropertyChain[X, Y] {
		def update(x :X, value :Y) :X
	}


	/** Return a property chain which provides an update method (expected to clone the argument rather than mutate its state
	  * in addition to the getter. No check is performed that passed getter and setter are actually related in any way,
	  * so use this in static context, when it is apparent.
	  */
	@inline def UpdatableProperty[S :TypeTag, T](get :S => T, set :(S, T) => S) :UpdatableProperty[S, T] =
		PropertyChain.property(get).updatable(set)

	/** Embeds an update function for the value of the property into the given property.
	  * Both the passed function and this factory are expected to create copies, rather than modify the state of their arguments.
	  */
/*
	object UpdatableProperty {

		/** Return a property chain which provides an update method (expected to clone the argument rather than mutate its state
		  * in addition to the getter. No check is performed that passed getter and setter are actually related in any way,
		  * so use this in static context, when it is apparent.
		  */
		def apply[S :TypeTag, T](get :S=>T, set :(S, T) => S) :UpdatableProperty[S, T] =
			PropertyChain[S](get).updatable(set)

	}
*/



	/** Automatic reflection didn't work? Try using this stand-in instead. It will equal (symmetrically) any
	  * other `PropertyChain`, reflected or manually created, based solely on the name.
	  * Be warned that drop function will always return None, as without reflection we have no means of
	  * decomposing the associated function into the dropped prefix and suffix functions.
	  */
	class HackedPropertyChain[-X, +Y] private[PropertyChain] (val name :String, getter :X=>Y, argType :Type)
		extends PropertyChain[X, Y](argType, getter)
	{
		def this(name :String, fun :X=>Y)(implicit argTag :TypeTag[X]) = this(name, fun, typeOf[X])

		if (name.length==0 || name(0).isWhitespace || name(name.length-1).isWhitespace)
			throw new IllegalArgumentException(s"Illegal property name: '$name'")

		def isSimple :Boolean = !name.contains('.')

		/** Returns a hacked, updatable instance. */
		override def updatable[XX <: X : TypeTag, YY >: Y](set: (XX, YY) => XX): UpdatableProperty[XX, YY] =
			new HackedPropertyChain[XX, YY](name, fun, definedFor) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		/** Returns None. */
		override def drop[XX <: X, Z](property: PropertyChain[XX, Z]): Option[PropertyChain[Z, Y]] = None


		/** Checks if the name of the resulting property starts with this property's name. */
		override def prefixOf[XX <: X, Z](property: PropertyChain[XX, Z]): Boolean = property.name startsWith name

		/** Return a hacked instance with a name being the concatenation of both names separated by a '.'
		  * and function this.fun andThen suffix.fun. */
		override def andThen[Z](suffix: PropertyChain[Y, Z]): PropertyChain[X, Z] =
			new HackedPropertyChain[X, Z](name + '.' +suffix.name, fun andThen suffix.fun, definedFor)

		/** Return a hacked instance with a name being the concatenation of both names separated by a '.'
		  * and function prefix.fun adnThen this.fun.
		  */
		def prepend[S, XX<:X](prefix :PropertyChain[S, XX]) :PropertyChain[S, Y] =
			new HackedPropertyChain[S, Y](prefix.name+"."+name, prefix.fun andThen fun, prefix.definedFor)

		override def equals(that :Any) :Boolean = that match {
			case p:PropertyChain[_, _] =>
				(definedFor<:<p.definedFor || p.definedFor<:<definedFor) && name==p.name
			case _ => false
		}

		override def hashCode :Int = name.hashCode
	}



	/** Base class for the reflected, proper instances of the PropertyChain */
	sealed abstract class ReflectedPropertyChain[-X, +Y] private[PropertyChain] (
			argType :Type, private[PropertyChain] val method :PropertyCall, fun :X=>Y
        ) extends PropertyChain[X, Y](argType, fun)
	{

		def drop[XX <: X, Z](property: PropertyChain[XX, Z]): Option[ReflectedPropertyChain[Z, Y]]

		/** Return a proper reflected instance representing composition of this function followed by the invocation of
		  * the given function.
		  */
		def andThen[Z](suffix: ReflectedPropertyChain[Y, Z]): ReflectedPropertyChain[X, Z]
	}



	/** PropertyChain consisting of a single property call */
	sealed class SimpleProperty[-S, +T] private[PropertyChain](tpe :Type, method :PropertyCall, _property :S=>T)
		extends ReflectedPropertyChain[S, T](tpe, method, _property)
	{

		private[PropertyChain] def this(tpe :Type, method :PropertyCall) =
			this(tpe, method, x => method.method.invoke(x).asInstanceOf[T])

		final val name = method.name



		final override def andThen[Z](suffix: PropertyChain[T, Z]): PropertyChain[S, Z] = suffix match {
			case p :HackedPropertyChain[_, _] =>
				p.asInstanceOf[HackedPropertyChain[T, Z]] prepend this
			case r :ReflectedPropertyChain[_, _] =>
				this andThen r.asInstanceOf[ReflectedPropertyChain[T, Z]]
		}


		final override def andThen[Z](suffix: ReflectedPropertyChain[T, Z]): ReflectedPropertyChain[S, Z] =
			new ChainedProperty[S, Z](definedFor, this.method, fun andThen suffix.fun, suffix)

		final override def prefixOf[XX <: S, Z](property: PropertyChain[XX, Z]): Boolean = property match {
			case p:ChainedProperty[_, _] => method == p.method
			case p:HackedPropertyChain[_, _] => p == this
			case _ => false
		}

		final override def drop[XX <: S, Z](property: PropertyChain[XX, Z]): Option[ReflectedPropertyChain[Z, T]] = None

		final override def isSimple: Boolean = true


		final override def updatable[XX <: S : TypeTag, YY >: T](set: (XX, YY) => XX): SimpleProperty[XX, YY] with UpdatableProperty[XX, YY] =
			new SimpleProperty[XX, YY](typeOf[XX], method, fun) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		override def equals(that :Any) :Boolean = that match {
			case s :SimpleProperty[_,_] =>
				(this eq s) || method == s.method
			case _ :HackedPropertyChain[_, _] => that == this
			case _ => false
		}

		override def hashCode :Int = name.hashCode

	}





	private class ChainedProperty[-S, +T] (
			tpe :Type, _method :PropertyCall, _property :S=>T, private final val tail :ReflectedPropertyChain[_, T])
		extends ReflectedPropertyChain[S, T](tpe, _method, _property)
	{
		private[PropertyChain] def this(tpe :Type, method :PropertyCall, tail :ReflectedPropertyChain[Any, T]) =
			this(tpe, method, x => tail.fun(method.method.invoke(x)), tail)

		final val name = method.name + "." + tail.name



		final override def andThen[Z](suffix: PropertyChain[T, Z]): PropertyChain[S, Z] = suffix match {
			case p :HackedPropertyChain[_, _] =>
				p.asInstanceOf[HackedPropertyChain[T, Z]] prepend this
			case p :ReflectedPropertyChain[_, _] =>
				andThen(p.asInstanceOf[ReflectedPropertyChain[T, Z]])
		}

		final override def andThen[Z](suffix: ReflectedPropertyChain[T, Z]): ReflectedPropertyChain[S, Z] =
			new ChainedProperty[S, Z](definedFor, this.method, this.fun andThen suffix.fun, tail andThen suffix)

		final override def prefixOf[XX <: S, Z](property: PropertyChain[XX, Z]): Boolean = property match {
			case p :ChainedProperty[_, _] =>
				this.method == p.method && tail.prefixOf(p.tail)
			case _ => false
		}


		final override def drop[XX <: S, Z](property: PropertyChain[XX, Z]): Option[ReflectedPropertyChain[Z, T]] = property match {
			case p :HackedPropertyChain[_, _] =>
				val next = p.name.indexOf('.')
				if (next >= 0)
					if (p.name.substring(0, next) == this.method.name)
						tail.drop(new HackedPropertyChain[Nothing, Z](p.name.substring(next+1), _ => ???, tail.definedFor))
					else None
				else
					tail.asInstanceOf[ReflectedPropertyChain[Z, T]] providing p.name == method.name
			case p :ReflectedPropertyChain[_, _] if method != p.method => None
			case p :SimpleProperty[_, _] => Some(tail.asInstanceOf[ReflectedPropertyChain[Z, T]])
			case p :ChainedProperty[_, _] => tail.drop(p.tail)
			case _ => None
		}

		final override def isSimple: Boolean = false




		override def updatable[XX <: S : TypeTag, YY >: T](set: (XX, YY) => XX): UpdatableProperty[XX, YY] =
			new ChainedProperty[XX, YY](typeOf[XX], method, fun, tail) with UpdatableProperty[XX, YY] {
				override def update(x: XX, value: YY): XX = set(x, value)
			}

		override def equals(that :Any) :Boolean = that match {
			case p :HackedPropertyChain[_, _] => that == this
			case p :ChainedProperty[_, _] =>
				(this eq p) || (method == p.method && tail == p.tail)
			case _ => false
		}

		override def hashCode :Int = name.hashCode
	}










	/** Factory for property chains */
	trait PropertyTracer[X] extends Any {

		/** Discover what properties are accessed by function `property` and create their reflection.
		  * @param property a function constituting of chained property calls on its argument.
		  */
		@inline def apply[Y](property :X=>Y)(implicit tag :TypeTag[X]) :ReflectedPropertyChain[X, Y] =
			PropertyChain.property(property)

		/** Check if this function represents a chained property call (i.e. `_.department.director.favoritePony`),
		  * and if so, return their reflected representation as an option.
		  */
		@inline def maybe[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[ReflectedPropertyChain[X, Y]] =
			PropertyChain.maybe(property)

		/** Assuming the given function is equivalent to a single zero-argument method call on the argument,
		  * return its reflected representation.
		  */
		@inline def simple[Y](property :X=>Y)(implicit tag :TypeTag[X]) :SimpleProperty[X, Y] =
			PropertyChain.simple(property)

		/** Check if this function represents a single zero-argument method call on its argument,
		  * and if so, return its reflected representation as an option.
		  */
		@inline def ifSimple[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[SimpleProperty[X, Y]] =
			PropertyChain.ifSimple(property)

		/** Return a hacked, manually created instance representing the given property '''without''' performing any
		  * reflection or mocking.
		  * @param property backing function returning the value of the represented property
		  * @param name the name of the represented property, or names of chained properties separated by '.'.
		  */
		def hacked[Y](property :X=>Y, name :String)(implicit tag :TypeTag[X]) :PropertyChain[X, Y] =
			new HackedPropertyChain[X, Y](name, property, typeOf[X])

		/** Reflect the property and return its name. */
		@inline def name[Y](property :X=>Y)(implicit tag :TypeTag[X]) :String =
			PropertyChain.property(property).name

		/** Try to reflect the property given the function and return its name in an option if successful. */
		@inline def nameOpt[Y](property :X=>Y)(implicit tag :TypeTag[X]) :Option[String] =
			PropertyChain.maybe(property).map(_.name)

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
	@inline def maybe[X :TypeTag, Y](property :X=>Y) :Option[ReflectedPropertyChain[X, Y]] =
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
	@inline def property[X :TypeTag, Y](property :X=>Y) :ReflectedPropertyChain[X, Y] =
		property.asSubclassOf[ReflectedPropertyChain[X, Y]] getOrElse trace(property)

	/** Assuming property constitutes of chained calls of zero-argument methods starting with type X,
	  * create a reflected representation which can be compared, composed and even subtracted in type safer manner
	  * with other property chain instances.
	  */
	def apply[X :TypeTag, Y](property :X=>Y) :ReflectedPropertyChain[X, Y] =
		property.asSubclassOf[ReflectedPropertyChain[X, Y]] getOrElse trace(property)


	/** Return a hacked, manually created instance representing the given property '''without''' performing
	  * any reflection or mocking.
	  * @param property backing function returning the value of the represented property
	  * @param name the name of the represented property, or names of chained properties separated by '.'.
	  */
	def apply[X :TypeTag, Y](property :X=>Y, name :String) :PropertyChain[X, Y] =
		new HackedPropertyChain[X, Y](name, property, typeOf[X])


	/** Reflect the property, creating a property chain and return its name. */
	def nameOf[X :TypeTag, Y](property :X=>Y) :String = apply(property).name

	/** Reflect the property, creating a property chain and return its name, swallowing any failure in None. */
	def nameOpt[X :TypeTag, Y](property :X=>Y) :Option[String] = maybe(property).map(_.name)






	private def trace[S :TypeTag, T](property :S=>T) :ReflectedPropertyChain[S, T] = {
		var root :Any = null
		def errorMsg(msg :String) =
			s"""Failed to trace a property call on ${typeOf[S]} starting with "${tracedPropertyName(root)}" ($property):\n$msg"""

		try {
			root = createMock(typeOf[S])
			val res = property(root.asInstanceOf[S])

			def retrace[X](trace :Trace, parent :Type) :ReflectedPropertyChain[X, T] = {
				val mock = trace.owner
				val traceField = mock.getClass.getField(TraceFieldName)
				val next = if (traceField == null) null else traceField.get(trace.owner).asInstanceOf[Trace]
				if (next == null)
					if (mock == res)
						new SimpleProperty[X, T](parent, PropertyCall(trace.method))
					else
						throw new PropertyReflectionException(s"Value returned by the function ($res) is not the value returned by the last method call.")
				else
					new ChainedProperty[X, T](parent, PropertyCall(trace.method), retrace[Any](next, trace.returns))
			}

			val traceField = root.getClass.getField(TraceFieldName)
			if (traceField == null)
				throw new PropertyReflectionException(s"Mock created for the argument type does not have the trace field $TraceFieldName: $root. $BugInfo")
			val trace = traceField.get(root).asInstanceOf[Trace]
			if (trace == null)
				throw new PropertyReflectionException(s"Function doesn't call any (non final) methods on its argument.")

			retrace[S](trace, typeOf[S])

		} catch {
			case e :ClassCastException =>
				throw new PropertyReflectionException(errorMsg(s"Couldn't create a mock of the correct class (likely due to an abstract return type of a method).\n${e.getMessage}"), e)
			case e :ScalaReflectionException =>
				throw new PropertyReflectionException(errorMsg(s"Couldn't determine class for an abstract type. Either a return type of last method call is not fully instantiated in the place of method declaration or you have found a bug.\n${e.getMessage}"), e)
			case e :Exception =>
				throw new PropertyReflectionException(errorMsg(e.getMessage), e)
		}
	}



	private def createMock(subjectType :Type) :Any = {
		val subjectClass = classFor(subjectType) //todo: maybe it _can_ extend final classes?
		if (!subjectClass.isPrimitive && (subjectClass.getModifiers & Modifier.FINAL) == 0)
			extendClass(subjectType, subjectClass)
		else
			mockValue(subjectType, subjectClass)
	}

	private def extendClass(subjectType :Type, subjectClass :Class[_]) :Any = {
		var mockClass = MockCache.get(subjectClass).orNull
		if (mockClass == null) subjectClass.synchronized {
			mockClass = MockCache.get(subjectClass).orNull
			if (mockClass == null) {
				mockClass = createMockClass(subjectType, subjectClass)
				MockCache.put(subjectClass, mockClass)
			}
		}
		mockClass.getConstructor().newInstance()
	}

	private def createMockClass(tpe :Type, clazz :Class[_]) :Class[_] =
		new ByteBuddy().subclass(clazz)
			.method(not(named(TypeMethodName))).intercept(to(new MethodInterceptor()))
			.defineMethod(TypeMethodName, classOf[AnyRef], Modifier.PUBLIC).intercept(FixedValue.value(tpe))
			.defineField(TraceFieldName, classOf[Trace], Modifier.PUBLIC)
			.make().load(getClass.getClassLoader).getLoaded

	private def mockValue(subjectType :Type, subjectClass :Class[_]) :Any = subjectClass match {
		//todo: random
		case JavaString => "42"
		case j.Integer.TYPE | JavaInteger => 42
		case j.Long.TYPE | JavaLong => 42L
		case j.Double.TYPE | JavaDouble => 42.0
		case j.Boolean.TYPE | JavaBoolean => true
		case j.Byte.TYPE | JavaByte => 42.toByte
		case j.Character.TYPE | JavaChar => 42.toChar
		case j.Float.TYPE | JavaFloat => 42F
		case j.Short.TYPE | JavaShort => 42.toShort
		case j.Void.TYPE | ScalaUnit => ()
		case _ => null
	}



	@tailrec private def tracedPropertyName(mock :Any, res :StringBuilder = new StringBuilder) :String =
		if (mock == null)
			res.toString
		else {
			val traceField = mock.getClass.getField(TraceFieldName)
			if (traceField == null)
				res.toString
			else {
				val trace = traceField.get(mock).asInstanceOf[Trace]
				if (trace == null)
					res.toString
				else {
					if (res.nonEmpty)
						res += '.'
					tracedPropertyName(trace.owner, res ++= trace.method.getName)
				}
			}
		}



	private class Trace(val owner :Any, val method :Method, val returns :Type) {
		override def toString :String = s"$owner.$method :$returns"
	}
	
	private class MethodInterceptor {

		@RuntimeType
		def intercept(@This owner :Any, @Origin method :Method): Any = {
			if (method.getParameterCount > 0)
				throw new PropertyReflectionException(s"Intercepted a call on object $owner to a method with parameters: $method.")
			val ownerClass = owner.getClass
			val typeMethod = ownerClass.getMethod(TypeMethodName)
			if (typeMethod == null)
				throw new PropertyReflectionException(s"Intercepted a call on object $owner without a $TypeMethodName method: $method. $BugInfo")
			val ownerType = typeMethod.invoke(owner).asInstanceOf[Type]

			val traceField = ownerClass.getField(TraceFieldName)
			if (traceField == null)
				throw new PropertyReflectionException(s"Intercepted a call on object $owner without a $TraceFieldName: $method. $BugInfo")
			val prev = traceField.get(owner).asInstanceOf[Trace]
			if (prev != null)
				throw new PropertyReflectionException(s"Intercepted a second call on object $owner: $method. Previous call was: ${prev.method}.")

			val methods = ownerType.member(TermName(method.getName)).alternatives.filter(validPropertyMethod)
			val returnType = methods match {
				case Seq() => throw new PropertyReflectionException(s"Couldn't find a method symbol for $method in type $ownerType of $owner. $BugInfo")
				case Seq(m) =>
					m.asMethod.returnType.dealias.asSeenFrom(ownerType, m.asMethod.owner.asClass)
				case _ => throw new IllegalArgumentException(s"Multiple method symbols for $method in type $ownerType of $owner: $methods")
			}
			val next = createMock(returnType)
			traceField.set(owner, new Trace(next, method, returnType))
			next
		}

		private[this] def validPropertyMethod(s :Symbol) :Boolean = s.isMethod && s.asMethod.paramLists.flatten.isEmpty
		
	}




	/** A wrapper for all exceptions thrown during the reflection */
	class PropertyReflectionException(msg :String, cause :Throwable) extends RuntimeException(msg, cause) {
		def this(msg :String) = this(msg, null)
		def this(cause :Throwable) = this(null, cause)
	}


	final private[PropertyChain] class PropertyCall(private[PropertyChain] val method :Method) {
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

	private[PropertyChain] def PropertyCall(method :Method) :PropertyCall = {
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

	private[this] val MockCache :ConcurrentMap[Class[_], Class[_]] = ConcurrentTrieMap[Class[_], Class[_]]()



	private val runtime = runtimeMirror(getClass.getClassLoader)
	private def classFor(tpe :Type) :Class[_] = runtime.runtimeClass(tpe.dealias.erasure.typeSymbol.asClass)



	@inline private[this] final val TraceFieldName = "__oldsql__trace__"
	@inline private[this] final val TypeMethodName = "__oldsql__type__"
	@inline private[this] final val BugInfo = "This is a bug!"
	
	@inline private[this] final val JavaByte = classOf[j.Byte] :Any
	@inline private[this] final val JavaShort = classOf[j.Short] :Any
	@inline private[this] final val JavaInteger = classOf[j.Integer] :Any
	@inline private[this] final val JavaLong = classOf[j.Long] :Any
	@inline private[this] final val JavaFloat = classOf[j.Float] :Any
	@inline private[this] final val JavaDouble = classOf[j.Double] :Any
	@inline private[this] final val JavaChar = classOf[j.Character] :Any
	@inline private[this] final val JavaBoolean = classOf[j.Boolean] :Any
	@inline private[this] final val ScalaUnit = classOf[Unit] :Any
	@inline private[this] final val JavaString = classOf[String] :Any

}
