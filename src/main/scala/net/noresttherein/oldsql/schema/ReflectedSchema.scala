package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.{==>, PropertyReflectionException}
import net.noresttherein.oldsql.morsels.Extractor.{IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.ComponentExtractor
import net.noresttherein.oldsql.schema.Mapping.ComponentExtractor.{IdentityComponent, RequisiteComponent}
import net.noresttherein.oldsql.slang._

import scala.reflect.runtime.universe.TypeTag





/**
  * @author Marcin Mościcki
  */
trait ReflectedSchema[O, S] extends RowSchema[O, S] { composite =>
	protected implicit val subjectType :TypeTag[S]

	private def selectorProperty[T](component :ComponentMapping[T]) :PropertyPath[S, T] =
		try {
			component.extractor.requisite.map(PropertyPath.property(_)) getOrElse
				PropertyPath.property(component.extractor.optional.andThen(_.get))
		} catch {
			case e :PropertyReflectionException =>
				throw new PropertyReflectionException(s"Failed to reflect extractor for $component from $this; " + e.getMessage, e)
		}

	trait PropertyExtractor[T] extends ComponentExtractor[O, S, T] {
		val property :PropertyPath[S, T]
		override def toString :String = "Extractor(" + lifted + ", " + property + ")"
	}

	override protected def selectorFor[T](component :ComponentMapping[T]) :PropertyExtractor[T] =
		component.extractor match {
			case _ :IdentityExtractor[_] =>
				new IdentityComponent[O, S](component.asInstanceOf[Component[S]]) with PropertyExtractor[S] {
					override val property = selectorProperty(component.asInstanceOf[ComponentMapping[S]])
				}.asInstanceOf[PropertyExtractor[T]]

			case req :RequisiteExtractor[S, T] =>
				new RequisiteComponent[O, S, T](component, req.getter) with PropertyExtractor[T] {
					override val property = selectorProperty(component)
				}
			case opt =>
				new ComponentExtractor.OptionalComponent[O, S, T](component, opt.optional) with PropertyExtractor[T] {
					override val property = selectorProperty(component)
				}
		}

	override def apply[T](component :Component[T]) :PropertyExtractor[T] =
		super.apply(component).asInstanceOf[PropertyExtractor[T]]


}


