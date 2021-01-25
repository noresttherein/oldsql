package com.hcore.ogre.morsels.necromancy

import com.hcore.ogre.morsels.necromancy.PropertyChain.HackedPropertyChain
import org.mockito.Mockito
import org.scalatest.{FlatSpec, Matchers}

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

	"PropertyPath" should "create equal instances for different calls of the same property of an object" in {
		val f = (s:Subject) => s.stringVal
		def m(s :Subject) = s.stringVal

		val id1 = PropertyChain[Subject](_.stringVal)
		val id2 = PropertyChain[Subject]((x :Subject) => x.stringVal)
		val id3 = PropertyChain.single(m)
		val id4 = PropertyChain.property(f)

		id1 should equal(id2)
		id2 should equal(id1)
		id1 should equal(id3)
		id1 should equal(id4)
		id2 should equal(id3)
		id2 should equal(id4)
		id3 should equal(id4)
	}

	"PropertyPath" should "identify a property of a class without a default constructor" in {
		val id1 = PropertyChain[Data]((_:Data).intVal)
		val id2 = PropertyChain[Data]((_:Data).intVal)

		id1 should equal(id2)
	}

	"PropertyPath" should "return different instances for different methods" in {
		val id1 = PropertyChain[Data](_.intVal)
		val id2 = PropertyChain[Data](_.otherInt)
		val id3 = PropertyChain[Data](_.stringVal)
		val id4 = PropertyChain[Data](_.sameInt)

		id1 shouldNot equal(id2)
		id1 shouldNot equal(id3)
		id1 shouldNot equal(id4)
		id2 shouldNot equal(id3)
		id2 shouldNot equal(id4)
		id3 shouldNot equal(id4)
	}

	"PropertyPath" should "return an equal instance for an overriden property" in {
		val id1 = PropertyChain[Subject](_.intVal)
		val id2 = PropertyChain[SubSubject](_.intVal)
		id1 should equal(id2)

		val id3 = PropertyChain[Base](_.name)
		val id4 = PropertyChain[Derived](_.name)
		val id5 = PropertyChain[Another](_.name)
		id3 should equal(id4)
		id3 should equal(id5)
		id4 should equal(id5)
	}

//	"PropertyPath" should "be compatible with super-types of the argument as long as they define the same property" in {
//		val id3 = PropertyPath[Base](_.name)
//		val id4 = PropertyPath[Derived](_.name)
//		val id5 = PropertyPath[Another](_.name)
//
//		id5.asInstanceOf[PropertyPath[Base, String]](new Base{ val name = "I have bad feelings about this" })
//	}

	"PropertyPath" should "accept transitive properties" in {
		val byte :Byte = 0
		val Null :AnyRef = null

		val id1 = PropertyChain.property((_:Forum).topic.subject.intVal)
		id1.name should equal ("topic.subject.intVal")
	}

	class Ref[T](val item :T)
	class Chain(val next :Ref[Chain])
	
	"PropertyPath" should "accept generic properties" in {
//		Mockito.mock(classOf[Chain], Mockito.RETURNS_DEEP_STUBS).next.item.next.item.next
		val id = PropertyChain[Chain](_.next.item.next.item.next)
		
		id.name should equal ("next.item.next.item.next")
	}

	trait Abstract { val field :Int }
	class Concrete(val field :Int) extends Abstract

	"PropertyPath" should "accept abstract properties" in {

		
		val id1 = PropertyChain[Abstract](_.field)
		val id2 = PropertyChain[Concrete](_.field)
		
		id1 should equal(id2)
	}

	class Mean { def FY :Int = ??? }

	"PropertyPath" should "accept properties throwing an exception" in {

		val id = PropertyChain[Mean](_.FY)
		
		id.name should equal("FY")
	}


	"PropertyPath" should "concatenate correctly two properties" in {
		val forum = new Forum(new Topic(new Subject))
		val a1 = PropertyChain[Forum](_.topic)
		val a2 = PropertyChain[Topic](_.subject.intVal)
		val b1 = PropertyChain[Forum](_.topic.subject)
		val b2 = PropertyChain[Subject](_.intVal)
		val expect = PropertyChain[Forum](_.topic.subject.intVal)

		val sums = Seq(a1 andThen a2, a2 compose a1, b1 andThen b2, b2 compose b1)
		sums.foreach { sum =>
			sum should equal(expect)
			sum(forum) should equal(expect(forum))
		}
	}

	"PropertyPath" should "recognize a property prefix" in {
		val property = PropertyChain[Forum](_.topic.subject.intVal)
		val prefix1 = PropertyChain[Forum](_.topic)
		val prefix2 = PropertyChain[Forum](_.topic.subject)

		prefix1.prefixOf(property) should equal(true)
		prefix2.prefixOf(property) should equal(true)
	}

	"PropertyPath" should "correctly drop a property prefix" in {
		val property = PropertyChain[Forum](_.topic.subject.intVal)
		val prefix1 = PropertyChain[Forum](_.topic)
		val suffix1 = PropertyChain[Topic](_.subject.intVal)
		val prefix2 = PropertyChain[Forum](_.topic.subject)
		val suffix2 = PropertyChain[Subject](_.intVal)

		property.drop(prefix1) should equal(Some(suffix1))
		property.drop(prefix2) should equal(Some(suffix2))
	}


	"HackedProperty" should "equal reflected instances" in {
		val reflected = PropertyChain[Forum](_.topic.subject.intVal)
		val prefix1 = PropertyChain[Forum](_.topic)
		val prefix2 = PropertyChain[Forum](_.topic.subject)
		val suffix1 = reflected.drop(prefix1).get
		val suffix2 = reflected.drop(prefix2).get

		val hacked = new HackedPropertyChain[Forum, Int]("topic.subject.intVal", _.topic.subject.intVal)
		val hackedPrefix1 = new HackedPropertyChain[Forum, Topic]("topic", _.topic)
		val hackedPrefix2 = new HackedPropertyChain[Forum, Subject]("topic.subject", _.topic.subject)


		hacked should equal(reflected)
		reflected should equal(hacked)

		(reflected drop hackedPrefix1) should equal(Some(suffix1))
		Some(suffix1) should equal (reflected drop hackedPrefix1)
		(reflected drop hackedPrefix2) should equal(Some(suffix2))
		Some(suffix2) should equal (reflected drop hackedPrefix2)

		(hackedPrefix1 andThen suffix1) should equal(reflected)
		reflected should equal (hackedPrefix1 andThen suffix1)
		(hackedPrefix2 andThen suffix2) should equal(reflected)
		reflected should equal(hackedPrefix2 andThen suffix2)

		(hackedPrefix1 andThen suffix1) should equal(hacked)
		hacked should equal(hackedPrefix2 andThen suffix2)
	}



	case class Origin(master :Master)

	case class Master(id :Option[Long], slave :Option[Slave])

	case class Slave(id :Option[Long])

	"PropertyPath" should "properly reflect option properties" in {
		val chain = PropertyChain[Origin](_.master.slave.get.id)
		chain.name should equal("master.slave.get.id")
	}


	"PropertyPath" should "properly reflect generic root types" in {
		val chain = PropertyChain[Seq[Option[Int]]](c => c.head.get)
		chain.name should equal("last.get")
	}

}
