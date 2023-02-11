package com.hcore.ogre.repository

import com.hcore.ogre.hoard.MappingFilter
import com.hcore.ogre.mapping.ties.Pathfinder
import com.hcore.ogre.mapping.{Car, Cars}
import com.hcore.ogre.model.Restriction
import com.hcore.ogre.sql.SQLFormula
import org.scalatest.{FlatSpec, Matchers}


class MappingFilterSpec extends FlatSpec with Matchers {

	import MappingFilterSpec._
	import SQLFormula.TermFormulas



	"MappingFilter" should "translate a restriction into a SQLExpression" in {
		val root = filter.source.last
		val expected = root(_.make) === "Mazda".?  && root(_.model)(_.name) === "MX-5".? &&
			(root(_.model)(_.version) in Seq("NB".?, "NBFL".?)) ||
			root(_.drive)(_.engine)(_.displacement)===2000.?

		val where = filter(restriction)
//		System.err.println(where)
//		System.err.println(expected)
		(where equivalent expected) should equal(true)
	}
}




object MappingFilterSpec {
	val pathfinder = Pathfinder.guild.enlist(Cars)
	val filter = new MappingFilter[Cars.type, Car](Cars, pathfinder)

	import Restriction.Restrictive
	val restriction :Restriction[Car] = ((_:Car).make) === "Mazda" && ((_:Car).model.name) === "MX-5" &&
		(((_:Car).model.version) in Seq(Some("NB"), Some("NBFL"))) ||
		((_:Car).drive.engine.displacement)===2000



}
