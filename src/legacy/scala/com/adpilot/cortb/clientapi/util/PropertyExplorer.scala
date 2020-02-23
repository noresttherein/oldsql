package com.adpilot.cortb.clientapi.util

import com.adpilot.cortb.clientapi.util.ObjectProperty._
import org.mockito.Mockito
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer

import scala.reflect.{ClassTag, classTag}
import scala.reflect.runtime.universe._

class PropertyExplorer[X, Y] private[PropertyExplorer](val map :X=>Y) {
//	private val mock = Mockito.mock(classOf[X], Mockito.RETURNS_DEEP_STUBS)
	
//	def


}


object PropertyExplorer {
	
	def apply[X :TypeTag, Y :TypeTag](map :X=>Y, properties :ObjectProperty[X, _]*) :PropertyExplorer[X, Y] = {

		val mock = Mockito.mock(classFor(typeOf[X]), new Answer[Any]{
			override def answer(invocation: InvocationOnMock): AnyRef = {
				???
			}
		})
		???
	}

	private val runtime = runtimeMirror(getClass.getClassLoader)
	private def classFor(tpe :Type) :Class[_] = runtime.runtimeClass(tpe.dealias.erasure.typeSymbol.asClass)

	
}


	
class UniqueValueGenerator {
	var counter :Int = 0



	def next[T :ClassTag] :Option[T] = next(classTag[T].runtimeClass.asInstanceOf[Class[T]])
	
	def next[T](clazz :Class[T]) :Option[T] = {
		primitives.get(clazz).map(_().asInstanceOf[T]).orElse {
			???
		}
	}


	private def answerFor[T :ClassTag](value :Int => T) = classTag[T].runtimeClass -> (()=> { counter+=1; value(counter) })

	private val primitives = Map[Class[_], ()=>Any](
//		answerFor[Boolean](_=>true)
		answerFor[Int](identity),
		answerFor[Long](_.toLong),
		answerFor[Short](_.toShort),
		answerFor[Char](_.toChar),
		answerFor[Byte](_.toByte),
		answerFor[String](_.toString)
	)

	
}

object UniqueValueGenerator {
	implicit class UniqueValue[T](val value :T) {
		def sameAs(other :Any) :Boolean = (value, other) match {
			case (x :Int, y :Int) => x==y
			case (x :Short, y :Short) => x==y
			case (x :Byte, y :Byte) => x==y
			case (x :Long, y :Long) => x==y
			case (x :Char, y :Char) => x==y
			case (x :String, y :String) => x==y
			case (null, _) => false
			case (_, null) => false
			case (x :AnyRef, y :AnyRef) => x eq y
			case (x :Boolean, y :Boolean) => false //todo
			case _ => false
		}

		override def equals(other :Any) = other match {
			case x:UniqueValue[_] => sameAs(x.value)
			case _ => false
		}

		override def hashCode = value.hashCode
	}
}