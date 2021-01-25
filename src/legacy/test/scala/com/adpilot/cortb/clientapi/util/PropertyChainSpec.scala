package com.adpilot.cortb.clientapi.util

import org.scalatest._

class PropertyChainSpec extends FlatSpec with Matchers {

	class Subject {
		val intVal = 1
		val stringVal = "hello"

	}


	class Data(val intVal :Int, val otherInt :Int, val stringVal :String) {
		def sameInt = intVal
	}

	class SubSubject extends Subject {
		override val intVal = 2
	}

	class Topic(val subject :Subject)
	class Forum(val topic :Topic)


	trait Base {
		def name :String //= "Base"
	}
	class Derived extends Base {
		override def name = "Derived"
	}
	trait Another extends Base

	"ObjectProperty" should "create equal instances for different calls of the same property of an object" in {
		val f = (s:Subject) => s.stringVal

		val id1 = ObjectProperty[Subject](_.stringVal)
		val id2 = ObjectProperty[Subject]((x :Subject) => x.stringVal)
		val id3 = ObjectProperty.direct((_:Subject).stringVal)
		val id4 = ObjectProperty.path(f)

		id1 should equal(id2)
		id2 should equal(id1)
		id1 should equal(id3)
		id1 should equal(id4)
		id2 should equal(id3)
		id2 should equal(id4)
		id3 should equal(id4)
	}

	"ObjectProperty" should "identify a property of a class without a default constructor" in {
		val id1 = ObjectProperty[Data]((_:Data).intVal)
		val id2 = ObjectProperty[Data]((_:Data).intVal)

		id1 should equal(id2)
	}

	"ObjectProperty" should "return different instances for different methods" in {
		val id1 = ObjectProperty[Data](_.intVal)
		val id2 = ObjectProperty[Data](_.otherInt)
		val id3 = ObjectProperty[Data](_.stringVal)
		val id4 = ObjectProperty[Data](_.sameInt)

		id1 shouldNot equal(id2)
		id1 shouldNot equal(id3)
		id1 shouldNot equal(id4)
		id2 shouldNot equal(id3)
		id2 shouldNot equal(id4)
		id3 shouldNot equal(id4)
	}

	"ObjectProperty" should "return equal instance for an overriden property" in {
		val id1 = ObjectProperty[Subject](_.intVal)
		val id2 = ObjectProperty[SubSubject](_.intVal)
//		System.err.println(id1)
//		System.err.println(id2)
		id1 should equal(id2)

		val id3 = ObjectProperty[Base](_.name)
		val id4 = ObjectProperty[Derived](_.name)
		val id5 = ObjectProperty[Another](_.name)
//		System.err.println(id3)
//		System.err.println(id4)
		id3 should equal(id4)
		id3 should equal(id5)
		id4 should equal(id5)
	}

	"ObjectProperty" should "handle transitive properties" in {
		val byte :Byte = 0
		val Null :AnyRef = null

		val id1 = ObjectProperty.path((_:Forum).topic.subject.intVal)
		id1.name should equal ("topic.subject.intVal")
	}

	class Ref[T](val item :T)
	class Chain(val next :Ref[Chain])
	
	"ObjectProperty" should "handle generic properties" in {

		val id = ObjectProperty[Chain](_.next.item.next.item.next)
		
		id.name should equal ("next.item.next.item.next")
	}

	trait Abstract { val field :Int }
	class Concrete(val field :Int) extends Abstract

	"ObjectProperty" should "handle abstract properties" in {

		
		val id1 = ObjectProperty[Abstract](_.field)
		val id2 = ObjectProperty[Concrete](_.field)
		
		id1 should equal(id2)
	}

	class Mean { def FY :Int = ??? }

	"ObjectProperty" should "handle properties throwing an exception" in {

		val id = ObjectProperty[Mean](_.FY)
		
		id.name should equal("FY")
	}
}
