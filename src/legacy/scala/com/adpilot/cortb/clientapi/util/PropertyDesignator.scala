package com.adpilot.cortb.clientapi.util

import java.lang.reflect.{Modifier, Method}

import com.adpilot.cortb.clientapi.util.PropertyDesignator.PropertyMethod
import org.mockito.exceptions.base.MockitoException
import org.mockito.exceptions.verification.SmartNullPointerException
import org.mockito.internal.stubbing.defaultanswers.ReturnsDeepStubs

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.reflect.runtime.universe._

//import javassist.util.proxy.{MethodHandler, MethodFilter, ProxyFactory}

import org.mockito.Mockito
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer

import scala.reflect.ClassTag
import scala.reflect.classTag

class PropertyDesignator[-T] protected[PropertyDesignator] (val calls :Seq[PropertyMethod], getter :T=>Any) {

	def apply(entity :T) :Any = getter(entity)

	def name :String = calls.map(_.name).mkString(".")
//	def className = method.getDeclaringClass.getName
//	def shortClassName = className.substring(className.lastIndexOf('.')+1)

	override def toString = name

	def canEqual(other: Any): Boolean = other.isInstanceOf[PropertyDesignator[_]]

	override def equals(other: Any): Boolean = other match {
		case that: PropertyDesignator[_] =>
			(that canEqual this) && (calls == that.calls)
		case _ => false
	}

	override def hashCode(): Int =
		name.hashCode
}


object PropertyDesignator {
	import OptionOps._

	def attempt[T :ClassTag :TypeTag](property :T=>Any, transitive :Boolean=false) :Option[PropertyDesignator[T]] =
		try {
			if (!transitive)
				Some(apply(property))
			else
				Some(this.transitive(property))
		} catch {
			case e :IllegalArgumentException => None
		}

	def apply[T :ClassTag :TypeTag](property :T=>Any) :PropertyDesignator[T] =
		try {
			create(property)
		}catch {
			case e:SmartNullPointerException =>
				throw new IllegalArgumentException(s"Can't identify a property - passed function is most probably not a direct property call on the argument\n${e.getMessage()}", e)
		}


	def transitive[T :ClassTag :TypeTag](property :T=>Any) :PropertyDesignator[T] = create(property)

	private val runtime = runtimeMirror(getClass.getClassLoader)
	private def classFor(tpe :Type) :Class[_] = runtime.runtimeClass(tpe.typeSymbol.asClass)

	private def create[T :ClassTag :TypeTag](property :T=>Any) = {
		val rootType = implicitly[TypeTag[T]].tpe
		val clazz = implicitly[ClassTag[T]].runtimeClass
//		val rootType = new TypeLiteral[T]{}

//		System.err.println(s"rootType: $rootType: ${rootType.getRawType()}")

		var callStack :Seq[(Method, Type, Any)] = Seq()

		def callPath = callStack.reverse.map(_._1.getName).mkString(".")


		def same(a :Any, b :Any) = (a, b) match {
			case (0, null) => true
			case (null, 0) => true
			case (x:AnyRef, y:AnyRef) => x eq y //System.err.println(s"refs: $x :${(x.getClass()).unless(x==null) getOrElse null}, $y :${(x.getClass()).unless(x==null) getOrElse null}"); x eq y
			case (_, _ :AnyRef) => false //System.err.println(s"$a, $b :AnyRef"); false
			case (_ :AnyRef, _) => false //System.err.println(s"$a :AnyRef, $b");  false
			case _ => a == b
		}

		def errorMsg(msg :String) =
			s"Cannot create a property path starting with ${clazz.getName}.$callPath:\n$msg"

		def noArgMethod(s :Symbol) = s.isMethod && s.asMethod.paramLists.flatten.isEmpty


		val mock = Mockito.mock(clazz, new Answer[Any] {
			override def answer(invocation: InvocationOnMock): Any = {
				val self = invocation.getMock
				val method = invocation.getMethod

				if (method.getParameterTypes.length>0)
					throw new IllegalArgumentException(errorMsg(s"referenced a non-empty argument method $method"))

				for ((_, _, parent) <- callStack.headOption if !same(parent, self))
					throw new IllegalArgumentException(errorMsg(s"argument function doesn't form a property chain - ${method} called on ${self.getClass}, while expected a property of last returned instance of ${parent.getClass}"))

//				val mirror = runtime.reflect(self)

				val parentType = callStack.headOption.map(_._2) getOrElse rootType
				val typeMapping = (parentType.typeConstructor.typeParams zip parentType.typeArgs.map(_.dealias)).toMap

				System.err.println(s"type mapping for $parentType: ${parentType.typeParams} -> ${parentType.typeArgs}")
				val methods =
					for (symbol <- parentType.member(TermName(method.getName)).alternatives; if noArgMethod(symbol))
						yield symbol.asMethod

				val returnType = methods match {
					case Seq() => throw new IllegalArgumentException(s"coulnd't find a method symbol for $method in type $parentType!. Programming error, sorry")
					case Seq(m) =>
						val possiblyGeneric = m.asMethod.returnType
//						possiblyGeneric
						typeMapping.getOrElse(possiblyGeneric.typeSymbol, possiblyGeneric)
					case _ => throw new IllegalArgumentException(s"multiple method symbols for $method in $parentType: $methods")
				}
//				val parentType = callStack.headOption.map(_._2) getOrElse rootType
//				val returnType = parentType.getReturnType(method)

				val res =
					try{
						Mockito.mock(classFor(returnType), this)
					} catch {
						case e :MockitoException => Mockito.RETURNS_SMART_NULLS.answer(invocation)
					}


				//					try{
				//						Mockito.mock(method.getReturnType, this)
				//					} catch {
				//						case e :MockitoException => Mockito.RETURNS_SMART_NULLS.answer(invocation)
				//					}

				callStack = (method, returnType, res) +: callStack
				res
			}
		})

		try {
			val res = property(mock.asInstanceOf[T])

			for ((method, _, value) <- callStack.headOption if !same(value, res))
				throw new IllegalArgumentException(errorMsg(s"argument function doesn't form a property chain - returned value $res is not the result $value of last method call of $method"))
		} catch {
			case e :IllegalArgumentException =>
				throw e
			case e :SmartNullPointerException =>
				throw new IllegalArgumentException(errorMsg(s"argument function calls a method on an unmockable object.\n${e.getMessage}"), e)
			case e :ScalaReflectionException =>
				throw new IllegalArgumentException(errorMsg(s"attempted to return an instance of not fully instantiated type: ${e.getMessage} "), e)
			case e :Exception =>
				throw new IllegalArgumentException(errorMsg(s"unexpected exception when executing argument function - probably your fault.\n${e.getMessage}"), e)
		}

		callStack match {
			case Seq() =>
				throw new IllegalArgumentException(errorMsg(s"argument function doesn't call any method of its argument!"))
//			case Seq((single, _)) =>
//				new DirectProperty[S, T](PropertyMethod(single), property)
			case _ =>
				val calls = callStack.map{ case (method, _, _) => PropertyMethod(method) }.reverse
				new PropertyDesignator[T](calls, property)
		}
	}



	class PropertyMethod private[PropertyDesignator] (private val method :Method) {
		if (method.getParameterTypes.length>0)
			throw new IllegalArgumentException(s"non-zero argument $method is not a property")

		private def declarations(clazz :Class[_], method :Method, res :ListBuffer[Method]=ListBuffer()) :ListBuffer[Method] = {
			def overridden(m :Method) =
				!Modifier.isPrivate(m.getModifiers) &&
					m.getName == method.getName &&
					m.getReturnType.isAssignableFrom(method.getReturnType)

			clazz.getDeclaredMethods.filter(overridden).foreach(res += _)
			Option(clazz.getSuperclass).foreach(declarations(_, method, res))
			clazz.getInterfaces.foreach(declarations(_, method, res))
			res
		}

		val supers = declarations(method.getDeclaringClass, method).toSet


		def name = method.getName

		override def equals(that :Any) = that match {
			case o:PropertyMethod =>
				(this eq o ) || name==o.name && (supers & o.supers).nonEmpty
			case _ => false
		}

		override def hashCode = name.hashCode
	}

	def PropertyMethod(method :Method) = new PropertyMethod(method)



	/*
		def javassist[T :ClassTag](property : T=>Any) = {
			var methods :Seq[Method] = Seq()
			val factory = new ProxyFactory
			val clazz = classTag[T].runtimeClass

			factory.setSuperclass(clazz)

			factory.setFilter(new MethodFilter {
				override def isHandled(m: Method): Boolean = true
			})
			val handler = new MethodHandler {
				override def invoke(self: scala.Any, thisMethod: Method, proceed: Method, args: Array[AnyRef]): AnyRef = {
					methods = thisMethod +: methods
	//				proceed.invoke(self, args)
					null
				}
			}
			val mock = factory.create(new Array[Class[_]](0), new Array[AnyRef](0), handler).asInstanceOf[T]

			property(mock)

			methods match {
				case Seq() => throw new IllegalArgumentException(s"Cannot identify target property: passed function doesn't call any methods of $clazz")
				case Seq(m) => new PropertyIdentifier(m)
				case _ => throw new IllegalArgumentException(s"Cannot identify target property; passed function calls several methods of $clazz: $methods")
			}

		}
	*/
}
