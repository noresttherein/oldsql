package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.{==>, PropertyReflectionException}
import net.noresttherein.oldsql.morsels.Extractor.{ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ComponentExtractor
import net.noresttherein.oldsql.schema.ComponentExtractor.{ConstantComponent, EmptyComponent, IdentityComponent, RequisiteComponent}
import net.noresttherein.oldsql.slang._

import scala.reflect.runtime.universe.TypeTag





/**
  * @author Marcin Mościcki
  */
trait ReflectedMapping[S, O] extends MappingSupport[S, O] { composite =>
	protected implicit val subjectType :TypeTag[S]

	private def selectorProperty[T](component :ComponentMapping[T]) :PropertyPath[S, T] =
		try {
			component.extractor.requisite.map(PropertyPath.property(_)) getOrElse
				PropertyPath.property(component.extractor.optional.andThen(_.get))
		} catch {
			case e :PropertyReflectionException =>
				throw new PropertyReflectionException(
					s"Failed to reflect extractor ${component.extractor} for $component from $this; " + e.getMessage, e
				)
		}

	trait ExtractorProperty[T] extends ComponentExtractor[S, T, O] {
		val property :PropertyPath[S, T]
		override def toString :String = "Extractor(" + property + "=" + export + ")"
	}

	override protected def selectorFor[T](component :ComponentMapping[T]) :ExtractorProperty[T] =
		component.extractor match {
			case _ :IdentityExtractor[_] => //fixme: doomed to fail - PropertyPath will throw up
				new IdentityComponent[S, O](component.asInstanceOf[Component[S]]) with ExtractorProperty[S] {
					override val property = selectorProperty(component.asInstanceOf[ComponentMapping[S]])
				}.asInstanceOf[ExtractorProperty[T]]

			case const :ConstantExtractor[_, _] => //fixme: doomed to fail - PropertyPath will throw up
				new ConstantComponent[T, O](component, const.constant.asInstanceOf[T]) with ExtractorProperty[T] {
					override val property = selectorProperty(component)
				}

			case req :RequisiteExtractor[S, T] =>
				new RequisiteComponent[S, T, O](component, req.getter) with ExtractorProperty[T] {
					override val property = selectorProperty(component)
				}
			case _ :EmptyExtractor[_, _] => //fixme: doomed to fail -> PropertyPath will throw up
				new EmptyComponent[T, O](component) with ExtractorProperty[T] {
					override val property = selectorProperty(component)
				}
			case opt =>
				new ComponentExtractor.OptionalComponent[S, T, O](component, opt.optional) with ExtractorProperty[T] {
					override val property = selectorProperty(component)
				}
		}

	override def apply[T](component :Component[T]) :ExtractorProperty[T] =
		super.apply(component).asInstanceOf[ExtractorProperty[T]]


}


