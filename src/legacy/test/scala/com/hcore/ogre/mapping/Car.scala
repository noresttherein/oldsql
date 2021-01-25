package com.hcore.ogre.mapping

import com.hcore.ogre.mapping.support.StaticMapping


case class Car(make :String, model :Model, drive :Drivetrain, dimensions :Dimensions, body :Body)

case class Dimensions(wheelbase :Int, length :Int, width :Int, height :Int, weight :Int)

case class Body(doors :Int)

case class Drivetrain(engine :Engine, transmission :Transmission)

case class Model(name :String, generation :Int, version :Option[String])

case class Engine(fuel :Fuel, displacement :Int, turbocharged :Boolean)

trait Fuel

object Fuel {
	case object Gasoline extends Fuel
	case object Oil extends Fuel
}

case class Transmission(gears :Int, change :Transmission.Type)

object Transmission{
	trait Type
	case object Manual extends Type
	case object Automatic extends Type
}


object Cars extends StaticMapping[Car] {
	implicit val fuelType = ColumnType.mapped[Fuel, String](_ match {
		case "Gasoline" => Fuel.Gasoline
		case "Oil" => Fuel.Oil
	})(_.toString)

	implicit val transmissionType = ColumnType.mapped[Transmission.Type, String](_ match {
		case "Manual" => Transmission.Manual
		case "Automatic" => Transmission.Automatic
	})(_.toString)


	val make = column("make", _.make)

	val model = new StaticComponent(_.model) {
		val name = column("name", _.name)
		val generation = column("generation", _.generation)
		val version = column("version", _.version)

		override protected def construct(implicit res: Values): Model =
			Model(name, generation, version)

		override def toString = "model"
	}

	val drive = new StaticComponent(_.drive) {
		val engine = new StaticComponent(_.engine) {
			val fuel = column("fuelType", _.fuel)
			val displacement = column("displacement", _.displacement)
			val turbocharged = column("turbocharged", _.turbocharged)

			override protected def construct(implicit res: Values): Engine =
				Engine(fuel, displacement, turbocharged)

			override def toString = "engine"
		}

		val transmission = new StaticComponent(_.transmission) {
			val gears = column("gears", _.gears)
			val change = column("type", _.change)

			override protected def construct(implicit res: Values): Transmission =
				Transmission(gears, change)

			override def toString = "transmission"
		}

		override protected def construct(implicit res: Values): Drivetrain =
			Drivetrain(engine, transmission)

		override def toString = "drive"
	}

	val dimensions = new StaticComponent(_.dimensions) {
		val wheelbase = column("wheelbase", _.wheelbase)
		val length = column("length", _.length)
		val width = column("width", _.width)
		val height = column("height", _.height)
		val weight = column("weight", _.weight)

		override protected def construct(implicit res: Values): Dimensions =
			Dimensions(wheelbase, length, width, height, weight)

		override def toString="dimensions"
	}

	val body = new StaticComponent(_.body) {
		val doors = column("doors", _.doors)

		override protected def construct(implicit res: Values): Body = Body(doors)

		override def toString = "body"
	}

	override protected def construct(implicit res: Cars.Values): Car = Car(
		make, model, drive, dimensions, body
	)

	override def sqlName= Some("cars")

	override def toString = "Cars"
}