package net.noresttherein.oldsql

import java.lang.{StringBuilder => JStringBuilder}

import scala.annotation.tailrec
import scala.collection.{IterableOnce, IterableOps}
import scala.reflect.{classTag, ClassTag}
import scala.util.Try

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Got






package object slang {

	private[oldsql] object && {
		def unapply[T](value :T) :Opt[(T, T)] = Got((value, value))
	}



	/** Additional extension methods for collections of the standard library framework. */
	implicit class IterableExtension[C[X] <: Iterable[X], E](private val self :IterableOps[E, C, C[E]]) extends AnyVal {
		/** Maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the second elements
		  * of the tuples returned by the given function) are returned in a collection of the same dynamic type
		  * as this collection.
		  */
		def mapWith[A, O](z :A)(f :(A, E) => (A, O)) :C[O] =
			self.view.scanLeft((z, null.asInstanceOf[O])) {
				(acc, e) => f(acc._1, e)
			}.tail.map(_._2).to(self.iterableFactory)

		/** Flat maps this collection from left to right with an accumulating state updated by the mapping function.
		  * The state is discarded after the operation and only the mapping results (the collections returned by
		  * by the given function) are returned in a collection of the same dynamic type as this collection.
		  */
		def flatMapWith[A, O](z :A)(f :(A, E) => (A, IterableOnce[O])) :C[O] =
			self.view.scanLeft((z, Nil :IterableOnce[O])) {
				(acc, e) => f(acc._1, e)
			}.flatMap(_._2).to(self.iterableFactory)

		/** Maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def mapWithIndex[O](f :(Int, E) => O) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			self foreach { e => b += f(i, e); i += 1 }
			b.result()
		}

		/** Flat maps this collection in order consistent with `foreach`, passing as the first argument the index
		  * of the mapped element.
		  */
		def flatMapWithIndex[O](f :(Int, E) => IterableOnce[O]) :C[O] = {
			var i = 0
			val b = self.iterableFactory.newBuilder[O]
			self foreach { e => b ++= f(i, e); i += 1 }
			b.result()
		}
	}



	/** Implicit conversion patching any object with methods providing prettified/shortened class names. */
	private[oldsql] implicit class classNameMethods(private val self :Any) extends AnyVal {

		/** An approximation of the imported type symbol of the class of this object, as it would be referenced
		  * in code. First, the whole package prefix and all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. Then, dropped is the prefix up until and including to the last '$'. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. If the class is anonymous, a simple '.anon' replaces its whole anonymous name section,
		  * and prepended to it is the directly preceding/enclosing class name, that is the inner-most class name
		  * from the non-anonymous prefix. Primitive types are capitalized to their Scala names and arrays are formatted
		  * recursively as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed,
		  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
		  * will throw it off, as will identifiers quoted in backticks. Finally, for the obvious reason, the name
		  * of the anonymous class is synthetic and the same for all anonymous inner classes of the same enclosing
		  * class/object.
		  */
		@inline def innerClassName: String = innerNameOf(self.getClass)

		/** An approximation of the type name of the class of the given object, as it would appear in code.
		  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
		  * The demangling proceeds as follows: first, all trailing '$' characters are dropped.
		  * Then, all escape sequences for special characters which are legal for use in identifiers are unescaped
		  * to their original forms. All individual '$' signs (used in name mangling of inner classes as the separators)
		  * are replaced with a '.', and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class
		  * is specialized, its mangled type parameters are resolved and composed in a type parameter list
		  * in Scala's syntax. Primitive types are capitalized to their Scala names and arrays are formatted recursively
		  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used
		  * for informational, debugging purposes, and not for identifiers or in any sort of reflection operations,
		  * as it can fail to produce the correct and unique type representation for a number of reasons. Most notably,
		  * any kind of generic, non-specialized classes will not have any type arguments listed, and only `@specialized`
		  * type parameters of specialized classes will be shown. Use of '$' in a demangled name will throw it off,
		  * as will identifiers quoted in backticks. Finally, for the obvious reason, the name of the anonymous class
		  * is synthetic.
		  */
		@inline def localClassName :String = localNameOf(self.getClass)

		/** An abbreviated qualified name of the class of this object, demangled to an approximation of how it would
		  * appear in code. All package names are replaced with their first letters, while the class name is demangled
		  * as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def abbrevClassName :String = abbrevNameOf(self.getClass)

		/** An approximation of the full, qualified and demangled name of the class of this object, as it would
		  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
		  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
		  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
		  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
		  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
		  * to their Scala names and arrays are formatted recursively as 'Array['`classNameOf(element)`']'.
		  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
		  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique
		  * type representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
		  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
		  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
		  * Finally, anonymous classes receive synthetic names for the obvious reason.
		  */
		@inline def className :String = fullNameOf(self.getClass)
	}



	/** An approximation of the imported type symbol of the class of the given object, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline private[oldsql] def innerClassNameOf(o :Any) :String = innerNameOf(o.getClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	@inline private[oldsql] def innerNameOf[C :ClassTag] :String = innerNameOf(classTag[C].runtimeClass)

	/** An approximation of the imported type symbol of the given class, as it would be referenced
	  * in code. First, the whole package prefix and all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * Then, dropped is the prefix up until and including to the last '$'. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * If the class is anonymous, a simple '.anon' replaces its whole anonymous name section, and prepended to it
	  * is the directly preceding/enclosing class name, that is the inner-most class name from the non-anonymous prefix.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`innerClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic and the same
	  * for all anonymous inner classes of the same enclosing class/object.
	  */
	private[oldsql] def innerNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val len = qualified.length
			val start = qualified.lastIndexOf('.') + 1
			val anon = qualified.indexOf("$$anon", start)
			val end =
				if (anon >= 0) anon
				else if (start == len) len
				     else trimTrailingDollars(qualified)
			var i = start
			val res = new JStringBuilder(end - i + 5) //5 for anon
			while (i < end) qualified.charAt(i) match {
				case '$' =>
					i += 1
					if (qualified.startsWith(specializationPrefix, i))
						i = demangleSpecialization(qualified, i, end, res)
					else {
						val jump = unescape(qualified, i, res)
						if (jump == i) //no escape sequence, individual '$' treated as a class name separator
							res.delete(0, res.length)
						i = jump
					}
				case c => res append c; i += 1
			}
			if (anon >= 0)
				res append ".anon"
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + innerNameOf(elem) + "]"
	}



	/** An approximation of the type name of the class of the given object, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline private[oldsql] def localClassNameOf(obj :Any): String = localNameOf(obj.getClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	@inline private[oldsql] def localNameOf[C :ClassTag] :String = localNameOf(classTag[C].runtimeClass)

	/** An approximation of the type name of the given class, as it would appear in code.
	  * It doesn't include the package prefix, but includes the demangled names of all enclosing classes/objects.
	  * The demangling proceeds as follows: first, all trailing '$' characters are dropped. Then, all escape sequences
	  * for special characters which are legal for use in identifiers are unescaped to their original forms.
	  * All individual '$' signs (used in name mangling of inner classes as the separators) are replaced with a '.',
	  * and so is the double '$$' in '$$anon' sequence for anonymous classes. If the class is specialized, its mangled
	  * type parameters are resolved and composed in a type parameter list in Scala's syntax.
	  * Primitive types are capitalized to their Scala names and arrays are formatted recursively
	  * as 'Array['`localClassNameOf(element)`']'. This algorithm is a heuristic and can only be used for informational,
	  * debugging purposes, and not for identifiers or in any sort of reflection operations, as it can fail to produce
	  * the correct and unique type representation for a number of reasons. Most notably, any kind of generic,
	  * non-specialized classes will not have any type arguments listed, and only `@specialized` type parameters
	  * of specialized classes will be shown. Use of '$' in a demangled name will throw it off, as will identifiers
	  * quoted in backticks. Finally, for the obvious reason, the name of the anonymous class is synthetic.
	  */
	private[oldsql] def localNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qualified = clazz.getName
			val end = trimTrailingDollars(qualified)
			val start = qualified.lastIndexOf('.') + 1
			val res = new JStringBuilder(end - start)
			demangleClassName(qualified, start, end, res)
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + localNameOf(elem) + "]"
	}



	/** An abbreviated qualified name of the class of the given object, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def abbrevClassNameOf(obj :Any) :String = abbrevNameOf(obj.getClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def abbrevNameOf[C :ClassTag] :String = abbrevNameOf(classTag[C].runtimeClass)

	/** An abbreviated qualified name of the given class, demangled to an approximation of how it would
	  * appear in code. All package names are replaced with their first letters, while the class name is demangled
	  * as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`abbrevNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	private[oldsql] def abbrevNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qname = clazz.getName
			val end = trimTrailingDollars(qname)
			val sb = new JStringBuilder(end)
			val start = clazz.getPackage match {
				case null => 0
				case p =>
					val pname = p.getName; val plen = pname.length
					var i = 0
					if (plen > 0) {
						sb append pname.charAt(0)
						i += 1
					}
					while (i < plen) pname.charAt(i) match {
						case '.' => sb append '.' append pname.charAt(i + 1); i += 2
						case _ => i += 1
					}
					if (i > 0) {
						sb append '.'
						i += 1 //skip the '.' separating the package and class name
					}
					i
			}
			demangleClassName(qname, start, end, sb)
			sb.toString

		case c if c.isPrimitive => "Array[" + abbrevNameOf(c).capitalize + "]"
		case c => "Array[" + abbrevNameOf(c) + "]"
	}



	/** An approximation of the full, qualified and demangled name of the class of the given object, as it would appear
	  * in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences for characters
	  * which are legal for use in identifiers are unescaped. Encoding of type arguments for `@specialized` classes
	  * is resolved and replaced with a parameter list, as it would occur in the code. Finally, all individual '$'
	  * (used in particular for separating names of nested classes) are replaced with '.', as is the double '$$'
	  * of '$$anon' marking an anonymous class. Primitive types are capitalized to their Scala names and arrays
	  * are formatted recursively as 'Array['`fullNameOf(element)`']'. This algorithm is a heuristic and can
	  * only be used for informational, debugging purposes, and not for identifiers or in any sort of reflection
	  * operations, as it can fail to produce the correct and unique type representation for a number of reasons.
	  * Most notably, any kind of generic, non-specialized classes will not have any type arguments listed,
	  * and only `@specialized` type parameters of specialized classes will be shown. Use of '$' in a demangled name
	  * will throw it off, as will identifiers quoted in backticks. Finally, anonymous classes receive synthetic names
	  * for the obvious reason.
	  */
	@inline private[oldsql] def classNameOf(obj :Any) :String = fullNameOf(obj.getClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	@inline private[oldsql] def fullNameOf[T :ClassTag] :String = fullNameOf(classTag[T].runtimeClass)

	/** An approximation of the full, qualified and demangled name of the given class, as it would
	  * appear in code. Demangling proceeds as follows: first, all trailing '$' are dropped and escape sequences
	  * for characters which are legal for use in identifiers are unescaped. Encoding of type arguments
	  * for `@specialized` classes is resolved and replaced with a parameter list, as it would occur in the code.
	  * Finally, all individual '$' (used in particular for separating names of nested classes) are replaced
	  * with '.', as is the double '$$' of '$$anon' marking an anonymous class. Primitive types are capitalized
	  * to their Scala names and arrays are formatted recursively as 'Array['`fullNameOf(element)`']'.
	  * This algorithm is a heuristic and can only be used for informational, debugging purposes, and not
	  * for identifiers or in any sort of reflection operations, as it can fail to produce the correct and unique type
	  * representation for a number of reasons. Most notably, any kind of generic, non-specialized classes
	  * will not have any type arguments listed, and only `@specialized` type parameters of specialized classes
	  * will be shown. Use of '$' in a demangled name will throw it off, as will identifiers quoted in backticks.
	  * Finally, anonymous classes receive synthetic names for the obvious reason.
	  */
	private[oldsql] def fullNameOf(clazz :Class[_]) :String = clazz.getComponentType match {
		case _ if clazz == java.lang.Void.TYPE => "Unit"

		case null =>
			val qname = clazz.getName
			val start = qname.lastIndexOf('.') + 1
			val end = trimTrailingDollars(qname)
			val res = new JStringBuilder(qname.length)
			var i = 0
			while (i < start) {
				res append qname.charAt(i); i += 1
			}
			demangleClassName(qname, start, end, res)
			res.toString

		case elem if elem.isPrimitive => "Array[" + elem.getName.capitalize + "]"
		case elem => "Array[" + fullNameOf(elem) + "]"
	}




	private def trimTrailingDollars(input :String) :Int = {
		var i = input.length - 1
		while (i >= 0 && input.charAt(i) == '$')
			i -= 1
		i + 1
	}


	private def demangleClassName(input :String, offset :Int, end :Int, result :JStringBuilder) :Unit = {
		var i  = offset
		while (i < end) input.charAt(i) match {
			case '$' =>
				i += 1
				if (input.startsWith(specializationPrefix, i))
					i = demangleSpecialization(input, i, end, result)
				else if (input.startsWith("anon", i)) {
					result append "anon"; i += 4
				} else
					i = unescape(input, i, result)

			case c => result append c; i += 1
		}
	}


	private def unescape(input :String, offset :Int, result :JStringBuilder) :Int = {
		var s = escapes.length - 1
		var symbol = ""
		while ({ symbol = escapes(s); !input.startsWith(symbol, offset)}) //escapes has "" at 0 as a guard
			s -= 1
		val name = symbols(s)
		result append name
		offset + symbol.length
	}


	private def demangleSpecialization(input :String, offset :Int, end :Int, result :JStringBuilder) :Int = {
		//inputStartsWith("mc", offset)
		val rollbackPoint = result.length
		result append '['

		def rollback() = {
			result.delete(rollbackPoint, result.length)
			result append '.' //for the '$' starting '$mc'.
			offset //no infinite loop as input doesn't starti with a '$' and "mc" will get normal char treatment
		}
		@tailrec def demangle(pos :Int, params :List[String] = Nil) :Int =
			if (pos == end)  //end of input encoutered before the closing sequence of specialized parameters encoding
				rollback()
			else {
				val param = input.charAt(pos)
				var t = typeParamCodes.length - 1
				while (t >= 0 && typeParamCodes(t) != param)
					t -= 1
				if (t >= 0)
					demangle(pos + 1, typeParamNames(t)::params)
				else if (input.startsWith("$sp", pos) && params.nonEmpty) {
					params foreach { p => result append p append ',' }
					result.delete(result.length - 1, result.length) //last ','
					result append ']'
					pos + 3
				} else  //illegal specialization mangling, revert to standard behaviour
					rollback()
			}
		demangle(offset + 2)
	}



	private[this] val escapes = Array(//empty string at start as an old fashioned guard which maps to '.' at the same time
		"", "tilde", "bang", "at", "hash", "percent", "up", "amp", "times", "minus", "plus", "eq", "less", "greater",
		"qmark", "div", "bar", "bslash", "colon"
	)
	private[this] val symbols = Array(
		".", "~", "!", "@", "#", "%", "^", "&", "*", "-", "+", "=", "<", ">", "?", "/", "|", "\\", ":"
	)

	private[this] val typeParamCodes = Array('S', 'V', 'Z', 'C', 'F', 'B', 'D', 'J', 'I')
	private[this] val typeParamNames = Array("Short", "Unit", "Bool", "Char", "Float", "Byte", "Double", "Long", "Int")

	private[this] final val specializationPrefix = "mc"






	/** Implicit conversion to a lazy value of type T which can be lifted to an Option[T] by one of its methods. */
	private[oldsql] implicit class ProvidingAndUnless[T](expr: =>T) {

		/** Returns Some(this) if passed condition is true, None otherwise;
		  * `this` is passed by name and evaluated only if condition is true!
		  */
		@inline def providing(condition :Boolean) :Option[T] =
			if (condition) Some(expr) else None

		/** Returns Some(this) if passed condition is true for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		@inline def providing(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (condition(x)) Some(x) else None
		}


		/** Returns Some(this) if passed condition is false, None otherwise;
		  * `this` is passed by name and evaluated only if condition is false!
		  */
		@inline def unless(condition :Boolean) :Option[T] =
			if (!condition) Some(expr) else None


		/** Returns Some(this) if passed condition is false for this, None otherwise;
		  * `this` is evaluated once, before passing its value to the condition!
		  */
		@inline def unless(condition :T => Boolean) :Option[T] = {
			val x = expr
			if (!condition(x)) Some(x) else None
		}

	}



	private[oldsql] implicit class IfTrueAndIfFalse(private val condition :Boolean) extends AnyVal {
		@inline def ifTrue[T](expr: => T) :Option[T] = if (condition) Some(expr) else None

		@inline def ifFalse[T](expr: => T) :Option[T] = if (!condition) Some(expr) else None

		@inline def thenMaybe[T](expr : => Option[T]) :Option[T] = if (condition) expr else None

		@inline def otherwiseMaybe[T](expr : => Option[T]) :Option[T] = if (!condition) expr else None
	}



	private[oldsql] implicit class OptionGuardExtension[T](opt : => Option[T]) {
		@inline def orNoneIf(expr :Boolean) :Option[T] =
			if (expr) None else opt

		@inline def orNoneUnless(expr :Boolean) :Option[T] =
			if (expr) opt else None

		@inline def mapOrElse[X](expr : T => X, default : => X) :X = opt match {
			case Some(t) => expr(t)
			case _ => default
		}
	}





	private[oldsql] implicit class CastingExtension[X](private val value :X) extends AnyVal {
		def downcast[S <: X] :S = value.asInstanceOf[S]

		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
		  * expression itself (`this`).
		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
		  * more importantly, against an expression changing type silently due to a refactor.
		  * @tparam U the (super)type of `this` expression.
		  * @tparam Y the target type of the expression after casting.
		  */
		@inline def castTo[U >: X, Y] :Y = value.asInstanceOf[Y]

		/** A safer casting expression which, in addition to the target type, accepts also the type of the cast
		  * expression itself (`this`). Both types are given as a single argument function `X => Y`,
		  * with `X` being the type to which `this` must conform, and `Y` the desired target type.
		  * Providing both is a defence against inadvertent casting from a wrongly presumed source type and,
		  * more importantly, against an expression changing type silently due to a refactor.
		  */
		@inline def castWith[F <: X => Any](implicit function :ReturnTypeOf[F]) :function.Return =
			value.asInstanceOf[function.Return]


		@inline def asSubclass[S <: X](implicit S :ClassTag[S]) :Option[S] = S.unapply(value)

		@inline def asSubclassOf[S](implicit S :ClassTag[S]) :Option[S] = S.unapply(value)

		@inline def ifSubclassOf[S] :CastValueGuard[X, S] = new CastValueGuard[X, S](value)

		@inline def ifSubclass[S <: X] :CastValueGuard[X, S] = ifSubclassOf[S]

//		@inline def explicitCast[F>:T, S] :S = value.asInstanceOf[S]

	}

	private[oldsql] final class ReturnTypeOf[F <: Nothing => Any] private { type Return }

	private[oldsql] object ReturnTypeOf {
		implicit def returnTypeOf[X, Y] :ReturnTypeOf[X => Y] { type Return = Y } =
			instance.asInstanceOf[ReturnTypeOf[X => Y] { type Return = Y }]
		private[this] val instance = new ReturnTypeOf[Nothing]
	}



	private[oldsql] class CastValueGuard[T, S](val value :T) extends AnyVal {

		def apply[X](block :S=>X)(implicit S :ClassTag[S]) :Option[X] = value match {
			case S(s) => Some(block(s))
			case _ => None
		}

		def orElse[X](block :S=>X)(alternative : =>X)(implicit S: ClassTag[S]) :X = value match {
			case S(s) => block(s)
			case _ => alternative
		}
	}




	private[oldsql] implicit class TypeParameterCastingExtension1[G[_]](private val value: G[_]) extends AnyVal {
		@inline def withTypeParam[S] :G[S] = value.asInstanceOf[G[S]]
	}

	private[oldsql] implicit class TypeParameterCastingExtension2[G[_, _]](private val value :G[_, _]) extends AnyVal {
		@inline def with2TypeParams[S, T] :G[S, T] = value.asInstanceOf[G[S, T]]
	}

	private[oldsql] implicit class TypeParameterCastingExtension3[G[_, _, _]](private val value :G[_, _, _]) extends AnyVal {
		@inline def with3TypeParams[S, T, U] :G[S, T, U] = value.asInstanceOf[G[S, T, U]]
	}






	private[oldsql] final def raise[E <: Throwable :ClassTag](msg :String) :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance(msg).asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance(msg, null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (String) or (String, Throwable).",
				ex
			)
		}).get

	private[oldsql] final def raise[E <: Throwable :ClassTag] :Nothing =
		throw (Try {
			classTag[E].runtimeClass.getDeclaredConstructor().newInstance().asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String]).newInstance("").asInstanceOf[Throwable]
		} orElse Try {
			classTag[E].runtimeClass.getConstructor(classOf[String], classOf[Throwable]).newInstance("", null).asInstanceOf[Throwable]
		} recover {
			case ex :Exception => new IllegalArgumentException(
				s"Can't throw ${classTag[E].runtimeClass} as a result of guard failure: no constructor (), (String) or (String, Throwable).",
				ex
			)
		}).get

}
