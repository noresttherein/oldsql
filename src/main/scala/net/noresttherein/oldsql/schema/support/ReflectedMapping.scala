package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.model.PropertyPath.PropertyReflectionException
import net.noresttherein.oldsql.morsels.Extractor.{ConstantExtractor, EmptyExtractor, IdentityExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.ComponentExtractor
import net.noresttherein.oldsql.schema.ComponentExtractor.{ColumnExtractor, ConstantColumn, ConstantComponent, EmptyColumn, EmptyComponent, IdentityColumn, IdentityComponent, OptionalColumn, RequisiteColumn, RequisiteComponent}

import scala.reflect.runtime.universe.TypeTag






/**
  * @author Marcin Mościcki
  */
trait ReflectedMapping[S, O] extends MappingFrame[S, O] { composite =>
	protected implicit val subjectType :TypeTag[S]

	private def selectorProperty[T](component :FrameComponent[T]) :PropertyPath[S, T] =
		try {
			component.extractor.requisite.map(PropertyPath.property(_)) getOrElse
				PropertyPath.property(component.extractor.optional.andThen(_.get))
		} catch {
			case e :PropertyReflectionException =>
				throw new PropertyReflectionException(
					s"Failed to reflect extractor ${component.extractor} for $component from $this; " + e.getMessage, e
				)
		}



	//todo: ColumnExtractor
	trait ComponentProperty[T] extends ComponentExtractor[S, T, O] {
		val property :PropertyPath[S, T]
		override def toString :String = "Extractor(" + property + "=" + export + ")"
	}
	
	
	trait ColumnProperty[T] extends ComponentProperty[T] with ColumnExtractor[S, T, O]

	
	
	override protected def selectorFor[T](component :FrameComponent[T]) :ComponentProperty[T] =
		component.extractor match {
			case _ :IdentityExtractor[_] => //fixme: doomed to fail - PropertyPath will throw up
				new IdentityComponent[S, O](component.asInstanceOf[Component[S]]) with ComponentProperty[S] {
					override val property = selectorProperty(component.asInstanceOf[FrameComponent[S]])
				}.asInstanceOf[ComponentProperty[T]]

			case const :ConstantExtractor[_, _] => //fixme: doomed to fail - PropertyPath will throw up
				new ConstantComponent[T, O](component, const.constant.asInstanceOf[T]) with ComponentProperty[T] {
					override val property = selectorProperty(component)
				}

			case req :RequisiteExtractor[S, T] =>
				new RequisiteComponent[S, T, O](component, req.getter) with ComponentProperty[T] {
					override val property = selectorProperty(component)
				}
			case _ :EmptyExtractor[_, _] => //fixme: doomed to fail -> PropertyPath will throw up
				new EmptyComponent[T, O](component) with ComponentProperty[T] {
					override val property = selectorProperty(component)
				}
			case opt =>
				new ComponentExtractor.OptionalComponent[S, T, O](component, opt.optional) with ComponentProperty[T] {
					override val property = selectorProperty(component)
				}
		}


	protected override def selectorFor[T](column :FrameColumn[T]) :ColumnProperty[T] =
		column.extractor match {
			case _ :IdentityExtractor[_] => //fixme: doomed to fail - PropertyPath will throw up
				new IdentityColumn[S, O](column.asInstanceOf[Column[S]]) with ColumnProperty[S] {
					override val property = selectorProperty(column.asInstanceOf[FrameColumn[S]])
				}.asInstanceOf[ColumnProperty[T]]

			case const :ConstantExtractor[_, _] => //fixme: doomed to fail - PropertyPath will throw up
				new ConstantColumn[T, O](column, const.constant.asInstanceOf[T]) with ColumnProperty[T] {
					override val property = selectorProperty(column)
				}

			case req :RequisiteExtractor[S, T] =>
				new RequisiteColumn[S, T, O](column, req.getter) with ColumnProperty[T] {
					override val property = selectorProperty(column)
				}
			case _ :EmptyExtractor[_, _] => //fixme: doomed to fail -> PropertyPath will throw up
				new EmptyColumn[T, O](column) with ColumnProperty[T] {
					override val property = selectorProperty(column)
				}
			case opt =>
				new OptionalColumn[S, T, O](column, opt.optional) with ColumnProperty[T] {
					override val property = selectorProperty(column)
				}
			
		}
	
	
	
	
	override def apply[T](component :Component[T]) :ComponentProperty[T] =
		super.apply(component).asInstanceOf[ComponentProperty[T]]

	override def apply[T](column :Column[T]) :ColumnProperty[T] =
		super.apply(column).asInstanceOf[ColumnProperty[T]]
	

}
