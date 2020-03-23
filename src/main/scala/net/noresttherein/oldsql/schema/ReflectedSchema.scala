package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.{==>, PropertyReflectionException}
import net.noresttherein.oldsql.morsels.Extractor.{IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.ComponentExtractor
import net.noresttherein.oldsql.slang._

import scala.reflect.runtime.universe.TypeTag





/**
  * @author Marcin Mościcki
  */
trait ReflectedSchema[O, S] extends RowSchema[O, S] { composite =>
	protected implicit val subjectType :TypeTag[S]

	abstract class PropertyExtractor[T](override val lifted :ComponentMapping[T]) extends Selector[T] {
		val property :PropertyPath[S, T] = try {
			lifted.extractor.requisite.map(PropertyPath.property(_)) getOrElse
				PropertyPath.property(lifted.extractor.optional.andThen(_.get))
		} catch {
			case e :PropertyReflectionException =>
				throw new PropertyReflectionException(s"Failed to reflect extractor for $lifted from $this; " + e.getMessage, e)
		}
	}

	override protected def selectorFor[T](component :ComponentMapping[T]) :PropertyExtractor[T] =
		component.extractor match {
			case _ :IdentityExtractor[_] =>
				new PropertyExtractor[S](component.asInstanceOf[ComponentMapping[S]]) with IdentityExtractor[S].asInstanceOf[PropertyExtractor[T]]
			case req :RequisiteExtractor[S, T] =>
				new PropertyExtractor[T](component) with RequisiteExtractor[S, T] {
					override val getter = req.getter
					override val optional = super.optional
					override val requisite = super.requisite
					override def apply(x :S) = getter(x)
				}
			case _ =>
				new PropertyExtractor[T](component) with OptionalExtractor[S, T] {
					override val optional = component.extractor.optional
					override def get(x :S) = optional(x)
				}

		}

	override def apply[T](component :Component[T]) :PropertyExtractor[T] =
		super.apply(component).asInstanceOf[PropertyExtractor[T]]


}





//trait ReflectedRootSchema[O, S] extends ReflectedSchema[O, S] with RowRootSchema[O, S]