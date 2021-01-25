package com.hcore.ogre.sql

import com.hcore.ogre.mapping.{Mapping, Car, Cars}
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.SQLFormula.{SelectFormula, SQLHNil}
import org.scalatest.{Matchers, FlatSpec}

import extensions._

class SQLPrinterSpec extends FlatSpec with Matchers  {

	def columnList(prefix :String, m :Mapping) =
		m.selectable.flatMap(_.sqlName).map(name => prefix.providing(_.length>0).map(_+"."+name) getOrElse name).mkString(", ")

	"SQLPrinter" should "print simple select of individual components" in {
		def expected(m :Mapping) =
			s"select ${columnList("cars", m)} from cars as cars where cars.make='Mazda'"
		val from = From(Cars) where (_.last \ (_.make) === "Mazda")

		val all = from select (_.last)
		SQLPrinter(all) should equal(expected(Cars))

		val make = from select (_.last \ (_.make))
		SQLPrinter(make) should equal(expected(Cars.make))

		val drivetrain = from select (_.last \ (_.drive))
		SQLPrinter(drivetrain) should equal(expected(Cars.drive))

	}

	"SQLPrinter" should "print selects with multiple occurrences of the same last" in {
		val from = From(Cars) join Cars on (_(_.make)===_(_.make))
		val sel = from select (t => t.prev :: t.last ::SQLHNil)

		SQLPrinter(sel) should equal (s"select ${columnList("cars", Cars)}, ${columnList("cars1", Cars)} from cars as cars, cars as cars1 where cars.make=cars1.make")
	}

	"SQLPrinter" should "print a select with a subselect" in {

		val select =
			From(Cars) where (t => t.exists(
				t from Cars where (t => t.prev \ (_.make)===t.last \ (_.make)) subselect (t => t.last \ (_.model) \ (_.version)))
			) join Cars on (_(_.body)(_.doors)===_(_.body)(_.doors)) select (t => t.prev \ (_.model) \ (_.name) :: t.last \ (_.model) \ (_.name) :: SQLHNil)
		SQLPrinter(select) should equal(
			"select cars.name, cars1.name from cars as cars, cars as cars1 " +
				"where (exists(select cars2.version from cars as cars2 where cars.make=cars2.make) and cars.doors=cars1.doors)")

		val select2 = From(Cars) select (t =>
			t.last(_.make) ::
				t.single(t from Cars on (_(_.drive)(_.engine)(_.fuel)===_(_.drive)(_.engine)(_.fuel)) subselect (_.last(_.make))) ::
					SQLHNil)
		SQLPrinter(select2) should equal("select cars.make, (select cars1.make from cars as cars1 where cars.fuelType=cars1.fuelType) from cars as cars")
	}
}
