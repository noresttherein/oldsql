package net.noresttherein.oldsql

import java.lang.reflect.Field

import scala.collection.{Factory, IterableFactory, MapFactory}

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.morsels.ComparableFactory






package object collection {

	//todo: compare also factories with implicit evidence
	private val IterableToFactoryClass = scala.collection.Iterable.iterableFactory.getClass
	private val iterableFactoryField :Opt[Field] =
		IterableToFactoryClass.getFields.find(_.getType == classOf[IterableFactory[Iterable]])
	private val MapToFactoryClass = scala.collection.Map.mapFactory.getClass
	private val mapFactoryField :Opt[Field] = MapToFactoryClass.getFields.find(_.getType == classOf[MapFactory[Map]])

	private[oldsql] def companionFactoryOf[E, T](factory :Factory[E, T]) :Opt[Any] =
		factory match {
			case comparable: ComparableFactory[_, _] => Got(comparable.factory)
			case _ if IterableToFactoryClass isAssignableFrom factory.getClass =>
				iterableFactoryField.map(_.get(factory).asInstanceOf[IterableFactory[Iterable]])
			case _ if MapToFactoryClass isAssignableFrom factory.getClass =>
				mapFactoryField.map(_.get(factory).asInstanceOf[MapFactory[Map]])
			case _ => None
		}

}