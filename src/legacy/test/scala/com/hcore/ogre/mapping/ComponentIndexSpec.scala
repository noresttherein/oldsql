package com.hcore.ogre.mapping

import com.hcore.ogre.morsels.necromancy.PropertyChain
import org.scalatest.{FlatSpec, Matchers}


class ComponentIndexSpec extends FlatSpec with Matchers {

	"ComponentIndex" should "correctly identify properties of a simple mapping" in {
		val index = ComponentIndex(Cars, false)

		Cars.columns.size should equal(15)
		Cars.components.size should equal(5)
//		System.err.println(Cars.subcomponents)
		Cars.subcomponents.size should equal(21)

		Cars.subcomponents.foreach { c=>
			index(Cars(c, _)).end should equal(c)
		}

//		System.err.println(index)
	}

/*	"ComponentIndex" should "correctly find prefix property component for a given transitive property" in {
		val index = ComponentIndex(Cars)
		val property = (_:Car).model.version.get
		System.err.println(PropertyPath[Option[String]](_.get))

		val Some((path, suffix)) = index.follow(PropertyPath(property))

		path should equal(Cars(_.model)(_.version))
		suffix should equal(PropertyPath[Option[String]](_.get))
	}*/
}
