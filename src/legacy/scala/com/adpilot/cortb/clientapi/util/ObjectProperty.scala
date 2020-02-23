package com.adpilot.cortb.clientapi.util

import java.lang.reflect.{Modifier, Method}

import com.adpilot.cortb.clientapi.util.ObjectProperty.{PropertySeq, TransitiveProperty, PropertyMethod, DirectProperty}
import com.adpilot.cortb.clientapi.util.PropertyDesignator._
import org.mockito.Mockito
import org.mockito.exceptions.base.MockitoException
import org.mockito.exceptions.verification.SmartNullPointerException
import org.mockito.internal.stubbing.defaultanswers.ReturnsDeepStubs
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer

import scala.collection.mutable.ListBuffer
import scala.reflect._
import scala.reflect.runtime.universe._
import scala.util.Try


import OptionOps._



sealed trait PropertyChain[-S, +T] {
	def name :String

	final def andThen[X](property :PropertyChain[T, X]) :PropertyChain[S, X] =
		property.prepend(this)

	def prepend[X](property :PropertyChain[X, S]) :PropertyChain[X, T]


	def prefixOf[SS <: S, X](property :PropertyChain[SS, X]) =
		calls.size<=property.calls.size &&
			property.name.startsWith(name) && (property.name.length==name.length || property.name(name.length)=='.') &&
			(calls zip property.calls).forall{ case (a, b) => a==b }


	def drop[SS <: S, X](property :PropertyChain[SS, X]) :Option[PropertyChain[X, T]] =
		if (property.prefixOf(this)) drop(property.calls.size)
		else None

	protected def drop(n :Int) :Option[PropertyChain[Any, T]]


	def isDirect = calls.size==1

	def chain :PropertyChain[S, T] = this


	override def equals(that :Any) = that match {
		case o :PropertyChain[_, _] =>
			(this eq o) || (o.canEqual(this) && name == o.name && calls == o.calls)
		case _ => false
	}

	def canEqual(that :Any) = that.isInstanceOf[PropertyChain[_, _]]

	override def hashCode = name.hashCode

	override def toString = name


	protected def calls :Seq[PropertyMethod]
	protected def calls(that :PropertyChain[_, _]) :Seq[PropertyMethod] = that.calls
}



sealed trait ObjectProperty[-S, +T] extends PropertyChain[S, T] {
	//todo: should include, not extend PropertyChain, as inequality between them may lead to bugs

	val fun :S=>T

	def apply(source :S) :T = fun(source)

	def andThen[X](property :ObjectProperty[T, X]) :ObjectProperty[S, X] =
		property.prepend(this)

	def prepend[X](property :PropertyChain[X, S]) :PropertyChain[X, T] = property match {
		case p:ObjectProperty[_, _] =>
			prepend(p.asInstanceOf[ObjectProperty[X, S]])
		case _ =>
			new PropertySeq[X, T](calls(property) ++: calls)
	}

	def prepend[X](property :ObjectProperty[X, S]) :ObjectProperty[X, T] =
		new TransitiveProperty[X, T](calls(property) ++: calls, (property.apply _) andThen (apply _))


	protected def drop(n :Int) :Option[PropertyChain[Any, T]] =
		(new PropertySeq[Any, T](calls.drop(n))).providing(n>0 && n < calls.size)


	def mutable[SS <: S, TT>:T](set :(SS, TT)=>SS) :MutableProperty[SS, TT]

	def asDirect = this match {
		case d:DirectProperty[S, T] => Some(d)
		case _ => None
	}

	override def chain :PropertyChain[S, T] = new PropertySeq[S, T](calls)
}




trait MutableProperty[S, T] extends ObjectProperty[S, T] {
	def update(obj :S, value :T) :S
}




object ObjectProperty { factory =>
	type AnyProperty[X] = ObjectProperty[X, Any]
	

	class DirectProperty[-S, +T] protected[ObjectProperty] (protected val method :PropertyMethod, val fun :S=>T) extends ObjectProperty[S, T] {

		def name = method.name

		override def prepend[X](property: ObjectProperty[X, S]): ObjectProperty[X, T] =
			new TransitiveProperty[X, T](calls(property) :+ method, (property.apply _) andThen (apply _))

		protected[ObjectProperty] def calls = Seq(method)


		override def mutable[SS <: S, TT >: T](set: (SS, TT) => SS): DirectProperty[SS, TT] with MutableProperty[SS, TT] =
			new DirectProperty[SS, TT](method, fun) with MutableProperty[SS, TT] {
				override def update(obj: SS, value: TT): SS = set(obj, value)
			}

		override def canEqual(that: Any): Boolean = that.isInstanceOf[DirectProperty[_, _]]


	}


	private class TransitiveProperty[-S, +T] protected[ObjectProperty] (protected[ObjectProperty] val calls :Seq[PropertyMethod], val fun :S=>T) extends ObjectProperty[S, T] {
		val name = calls.map(_.name).mkString(".")

		override def prepend[X](property: ObjectProperty[X, S]): ObjectProperty[X, T] =
			new TransitiveProperty[X, T](calls(property) ++: calls, (property.apply _) andThen (apply _))

		override def mutable[SS <: S, TT >: T](set: (SS, TT) => SS): ObjectProperty[SS, TT] with MutableProperty[SS, TT] =
			new TransitiveProperty[SS, TT](calls, fun) with MutableProperty[SS, TT] {
				override def update(obj: SS, value: TT): SS = set(obj, value)
			}


		override def canEqual(that: Any): Boolean = that.isInstanceOf[TransitiveProperty[_, _]]
	}

	private class PropertySeq[-S, +T] protected[ObjectProperty] (protected[ObjectProperty] val calls :Seq[PropertyMethod]) extends PropertyChain[S, T] {
		val name = calls.map(_.name).mkString(".")

		override def canEqual(that :Any) = that.isInstanceOf[PropertySeq[_, _]]

		override def prepend[X](property: PropertyChain[X, S]): PropertyChain[X, T] =
			new PropertySeq[X, T](calls(property) ++: calls)

		protected def drop(n :Int) :Option[PropertyChain[Any, T]] =
			(new PropertySeq[Any, T](calls.drop(n))).providing(n>0 && n < calls.size)

	}

	def name[S :TypeTag](property :S=>Any) :String = apply(property).name


	def apply[S :TypeTag, T](property :S=>T) :ObjectProperty[S, T] = path(property)

	def apply[S] :PropertyPathBuilder[S] = new PropertyPathBuilder[S]

	class PropertyPathBuilder[S] {

		def apply[T](property :S=>T)(implicit c :TypeTag[S]) :ObjectProperty[S, T] =
			factory.path(property)

		def any(property :S=>Any, direct :Boolean=false)(implicit c :TypeTag[S]) :AnyProperty[S] =
			factory.any(property, direct)

		def path[T](property :S=>T, direct :Boolean=false)(implicit c :TypeTag[S]) :ObjectProperty[S, T] =
			factory.path(property, direct)

		def direct[T](property :S=>T)(implicit c :TypeTag[S]) :DirectProperty[S, T] =
			factory.direct(property)

		def ifDirect[T](property :S=>T)(implicit c :TypeTag[S]) :Option[DirectProperty[S, T]] =
			factory.ifDirect(property)
	}



	def any[S :TypeTag](property :S=>Any, direct :Boolean=true) :ObjectProperty[S, Any] =
		if (direct) this.direct(property)
		else path(property)


	def optional[S :TypeTag, T](property :S=>T) :Option[ObjectProperty[S, T]] =
		Try(path(property)).toOption

	def ifDirect[S :TypeTag, T](property :S=>T) :Option[DirectProperty[S, T]] =
		path(property) match {
			case p :DirectProperty[_, _] => Some(p.asInstanceOf[DirectProperty[S, T]])
			case p => None
		}




	def direct[S :TypeTag, T](property :S=>T) :DirectProperty[S, T] =
		path(property) match {
			case p :DirectProperty[_, _] => p.asInstanceOf[DirectProperty[S, T]]
			case p =>
				throw new IllegalArgumentException(s"Property $p is not a direct property of ${implicitly[TypeTag[S]].tpe}")
		}


	def path[S :TypeTag, T](property :S=>T) :ObjectProperty[S, T] =
		follow[S, T](property)

	def path[S :TypeTag, T](property :S=>T, direct :Boolean) :ObjectProperty[S, T] =
		if (direct) this.direct(property)
		else path(property)


	private def follow[S :TypeTag, T](property :S=>T) = {


		val rootType = typeTag[S].tpe
		val rootClass = classFor(rootType)

		var callStack :Seq[(Method, Type, Any)] = Seq()

		def callPath = callStack.reverse.map(_._1.getName).mkString(".")


		def same(a :Any, b :Any) = (a, b) match {
			case (0, null) => true
			case (null, 0) => true
			case (x:AnyRef, y:AnyRef) => x eq y
			case (_, _ :AnyRef) => false
			case (_ :AnyRef, _) => false
			case _ => a == b
		}

		def noArgMethod(s :Symbol) = s.isMethod && s.asMethod.paramLists.flatten.isEmpty

		def errorMsg(msg :String) =
			s"Cannot create a property path starting with ${rootClass.getName}.$callPath:\n$msg"


		val mock = Mockito.mock(rootClass, new Answer[Any] {
			override def answer(invocation: InvocationOnMock): Any = {
				val self = invocation.getMock
				val method = invocation.getMethod

				if (method.getParameterTypes.length>0)
					throw new PropertyReflectionException(errorMsg(s"referenced a non-empty argument method $method"))

				for ((_, _, parent) <- callStack.headOption if !same(parent, self))
					throw new PropertyReflectionException(errorMsg(s"argument function doesn't form a property chain - $method called on ${self.getClass}, while expected a property of last returned instance of ${parent.getClass}"))


				val parentType = callStack.headOption.map(_._2) getOrElse rootType
				val typeMapping = (parentType.typeConstructor.typeParams zip parentType.typeArgs.map(_.dealias)).toMap

				val methods = parentType.member(TermName(method.getName)).alternatives.filter(noArgMethod)

				val returnType = methods match {
					case Seq() => throw new IllegalArgumentException(s"coulnd't find a method symbol for $method in type $parentType!. Programming error, sorry")
					case Seq(m) => //m.asMethod.returnType
						val possiblyGeneric = m.asMethod.returnType
						typeMapping.getOrElse(possiblyGeneric.typeSymbol, possiblyGeneric)
					case _ => throw new IllegalArgumentException(s"multiple method symbols for $method in $parentType: $methods")
				}

				val res =
					try{
						Mockito.mock(classFor(returnType), this)
					} catch {
						case e :MockitoException => Mockito.RETURNS_SMART_NULLS.answer(invocation)
					}

				callStack = (method, returnType, res) +: callStack
				res
			}
		})

		try {
			val res = property(mock.asInstanceOf[S])

			for ((method, _, value) <- callStack.headOption if !same(value, res))
				throw new PropertyReflectionException(errorMsg(s"argument function doesn't form a property chain - returned value $res is not the result $value of last method call of $method"))
		} catch {
			case e :PropertyReflectionException =>
				throw e
			case e :ClassCastException =>
				throw new PropertyReflectionException(errorMsg(s"couldn't create a mock of the correct class. Is the argument type generic? This won't work.\n${e.getMessage}"), e)
			case e :ScalaReflectionException =>
				throw new PropertyReflectionException(errorMsg(s"couldn't determine class for an abstract type. Either a return type of last method call is not fully instantiated in the place of method declaration or you have found a bug.\n{e.getMessage}"), e)
			case e :SmartNullPointerException =>
				throw new PropertyReflectionException(errorMsg(s"argument function calls a method on an unmockable object.\n${e.getMessage}"), e)
			case e :Exception =>
				throw new PropertyReflectionException(errorMsg(s"unexpected exception when executing argument function - probably your fault.\n${e.getMessage}"), e)
		}

		callStack match {
			case Seq() =>
				throw new PropertyReflectionException(errorMsg(s"argument function doesn't call any method of its argument!"))
			case Seq((single, _, _)) =>
				new DirectProperty[S, T](PropertyMethod(single), property)
			case _ =>
				val calls = callStack.map{ case (method, _, _) => PropertyMethod(method) }.reverse
				new TransitiveProperty[S, T](calls, property)
		}

	}


	class PropertyReflectionException(msg :String, cause :Throwable) extends RuntimeException(msg, cause) {
		def this(msg :String) = this(msg, null)
		def this(cause :Throwable) = this(null, cause)
	}


	class PropertyMethod private[ObjectProperty] (private val method :Method) {
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
				(this eq o ) || name==o.name && (method==o.method || (supers & o.supers).nonEmpty)
			case _ => false
		}

		override def hashCode = name.hashCode
	}

	private def PropertyMethod(method :Method) = new PropertyMethod(method)

	private val runtime = runtimeMirror(getClass.getClassLoader)
	private def classFor(tpe :Type) :Class[_] = runtime.runtimeClass(tpe.dealias.erasure.typeSymbol.asClass)

}



object MutableProperty {
	def apply[S :TypeTag, T](get :S=>T, set :(S, T) => S) :MutableProperty[S, T] =
		ObjectProperty[S](get).mutable(set)

	def direct[S :TypeTag, T](get :S=>T, set :(S, T)=>S) :DirectProperty[S, T] with MutableProperty[S, T] =
		ObjectProperty[S].direct(get).mutable(set)
}