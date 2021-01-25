package com.hcore.ogre.mapping


import com.hcore.ogre.mapping.Fuel.Gasoline
import com.hcore.ogre.mapping.Transmission.Manual
import org.scalatest.{FlatSpec, Matchers}

class ComponentValuesSpec extends FlatSpec with Matchers {
	val mx_5 = Car("Mazda", Model("MX-5", 4, Some("roadster")),
		Drivetrain(Engine(Gasoline, 2000, true), Transmission(6, Manual)),
		Dimensions(2315, 3915, 1730, 1235, 998), Body(2)
	)
	val car = ComponentValues(Cars)(mx_5)
	val drive = car :\ Cars.drive
	val model = car :\ Cars.model

	val enginePath = Cars \\ Cars.drive :\ Cars.drive.engine
	val engine = car \ enginePath

	"ComponentValues" should
		"deassemble component values from a preset value" in {
			car(Cars.make) should equal(mx_5.make)
			car(Cars.model) should equal(mx_5.model)
			car(Cars.drive) should equal(mx_5.drive)
			car(Cars.dimensions) should equal(mx_5.dimensions)

			model(Cars.model.name) should equal(mx_5.model.name)
			model(Cars.model.generation) should equal(mx_5.model.generation)
			model(Cars.model.version) should equal(mx_5.model.version)

			car(enginePath) should equal(mx_5.drive.engine)
			engine(Cars.drive.engine.fuel) should equal(Gasoline)
		}

	"ComponentValues" should
		"deassemble a value to column values which can be reassembled to the same value" in {
			val columns = ComponentValues(Cars)(mx_5, Cars.columns)
			val phoenix = Cars.assemble(columns)

			phoenix should equal(Some(mx_5))
	}

	"ComponentValues" should
		"override individual component values in column values" in {
			val na = mx_5.copy(model=mx_5.model.copy(generation=1), drive=mx_5.drive.copy(transmission=Transmission(5, Manual)))
			val transmissionPath = Cars \\ Cars.drive \ (_.transmission)
			val generationPath = Cars \\ Cars.model \ (_.generation)
			val changed = transmissionPath.lift.toSeq ++ generationPath.lift
			val columns = ComponentValues(Cars)(mx_5, Cars.columns)
			val changes = ComponentValues(Cars)(na, changed)
			val overriden = changes orElse columns
			val phoenix = Cars.assemble(overriden)

			overriden(transmissionPath) should equal(na.drive.transmission)
			overriden(generationPath) should equal(na.model.generation)

			phoenix should equal(Some(na))
		}


	"ComponentValues" should
		"override individual component values in ComponentValues with values for parent components of overriden values" in {
			val na = mx_5.copy(model=mx_5.model.copy(generation=1), drive=mx_5.drive.copy(transmission=Transmission(5, Manual)))
			val transmissionPath = Cars \\ Cars.drive \ (_.transmission)
			val generationPath = Cars \\ Cars.model \ (_.generation)
			val changed = transmissionPath.lift.toSeq ++ generationPath.lift
			val columns = ComponentValues(Cars)(mx_5)
			val changes = ComponentValues(Cars)(na, changed)
			val overriden = changes orElse columns
			val phoenix = Cars.assemble(overriden)


			overriden(transmissionPath) should equal(na.drive.transmission)
			overriden(generationPath) should equal(na.model.generation)

			phoenix should equal(Some(na))
		}


	
}
