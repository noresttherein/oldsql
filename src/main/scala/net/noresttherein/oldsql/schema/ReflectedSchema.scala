package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.{==>, PropertyReflectionException}
import net.noresttherein.oldsql.schema.Mapping.ComponentSelector
import net.noresttherein.oldsql.slang._

import scala.reflect.runtime.universe.TypeTag





/**
  * @author Marcin Mościcki
  */
trait ReflectedSchema[O, S] extends RowSchema[O, S] { composite =>
	protected implicit val subjectType :TypeTag[S]

	class PropertySelector[T](override val lifted :ComponentMapping[T])
		extends ComponentSelector[composite.type, composite.Owner, S, T]
	{
		override val extractor = lifted.extractor
		override val optional = extractor.optional
		override val requisite = extractor.requisite

		val property = try {
			requisite.map(PropertyPath.property(_)) getOrElse PropertyPath.property(optional.andThen(_.get))
		} catch {
			case e :PropertyReflectionException =>
				throw new PropertyReflectionException(s"Failed to reflect extractor for $lifted from $this; " + e.getMessage, e)
		}
	}

	override protected def selectorFor[T](component :ComponentMapping[T]) :PropertySelector[T] =
		new PropertySelector(component)

	override def apply[T](component :Component[T]) :PropertySelector[T] =
		super.apply(component).asInstanceOf[PropertySelector[T]]


/*
	override def apply[T](component :Component[T]) :PropertySelector[T] = {
		if (fastSelectors == null)
			if (selectors != null)
				fastSelectors = selectors
			else
				initialize()
		fastSelectors.getOrElse(component, throw new IllegalArgumentException(
			s"Component $component is not a part of mapping $this."
		)).asInstanceOf[PropertySelector[T]]
	}

	override protected def finalizeInitialization() :Unit = {
		fastSelectors = Map()
		subcomponents foreach { c =>
			fastSelectors = fastSelectors.updated(c, new PropertySelector(c.asInstanceOf[ComponentMapping[Any]]))
		}
		selectors = fastSelectors
	}

	@volatile private[this] var selectors :Map[AnyComponent, PropertySelector[_]] = _
	private[this] var fastSelectors :Map[AnyComponent, PropertySelector[_]] = _
*/
}





trait ReflectedRootSchema[O, S] extends ReflectedSchema[O, S] with RowRootSchema[O, S]