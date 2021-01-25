package com.adpilot.cortb.clientapi.util

import com.adpilot.cortb.clientapi.prototype.repository.entities.{Order, Campaign}
import org.scalatest._

class PropertyDesignatorSpec extends FlatSpec with Matchers {

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

	"PropertyIdentifier" should "identify called AnyRef property on target object" in {
		val id1 = PropertyDesignator[Subject]((_ :Subject).stringVal)
		val id2 = PropertyDesignator[Subject]((x :Subject) => x.stringVal)
//		val id3 = PropertyIdentifier[Subject]((x :Subject) => { x.intVal+1; x.stringVal })

		id1 should equal(id2)
//		id1 should equal(id3)
//		id2 should equal(id3)
		val s = new Subject
		s.intVal

	}

	"PropertyIdentifier" should "identify a property of a class without a default constructor" in {
		val id1 = PropertyDesignator[Data]((_:Data).intVal)
		val id2 = PropertyDesignator[Data]((_:Data).intVal)

		id1 should equal(id2)
	}

	"PropertyIdentifier" should "return different instances for different methods" in {
		val id1 = PropertyDesignator[Data](_.intVal)
		val id2 = PropertyDesignator[Data](_.otherInt)
		val id3 = PropertyDesignator[Data](_.stringVal)
		val id4 = PropertyDesignator[Data](_.sameInt)

		id1 shouldNot equal(id2)
		id1 shouldNot equal(id3)
		id1 shouldNot equal(id4)

	}

	"PropertyIdentifier" should "return equal instance for an overriden property" in {
		val id1 = PropertyDesignator[Subject](_.intVal)
		val id2 = PropertyDesignator[SubSubject](_.intVal)
//		System.err.println(id1)
//		System.err.println(id2)
		id1 should equal(id2)

		val id3 = PropertyDesignator[Base](_.name)
		val id4 = PropertyDesignator[Derived](_.name)
		val id5 = PropertyDesignator[Another](_.name)
//		System.err.println(id3)
//		System.err.println(id4)
		id3 should equal(id4)
		id3 should equal(id5)
		id4 should equal(id5)
	}

	"PropertyIdentifier" should "handle transitive properties" in {
		val id1 = PropertyDesignator[Forum](_.topic.subject.intVal)
	}

//	case class One[T](item :T)
//	case class Parent(id :Long)
//	case class Child(id :Long, parent :One[Parent])

	"PropertyIdentifier" should "handle generic properties" in {
//		val id = PropertyDesignator[Child](_.parent.item)
		val id = PropertyDesignator[Order](_.campaign.join.product.join.country)
		id.name should equal ("campaign.join.product.join.country")
	}
}
