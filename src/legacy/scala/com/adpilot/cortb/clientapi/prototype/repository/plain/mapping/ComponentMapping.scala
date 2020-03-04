package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping

import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnMapping._
import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.Mapping._

import scala.slick.jdbc.PositionedResult


trait ComponentMapping[E, C] extends Mapping[C] {
	def value(entity: E) :C

}



object ComponentMapping {


//	trait UniversalComponentMapping[E, C] extends ComponentMapping[E, C] with UniversalMapping[E]
/*
	trait AbstractComponentMapping[E, C] extends ComponentMapping[E, C] {

		lazy val columns = adaptColumns(_.columns)
		lazy val selectable = adaptColumns(_.selectable)
		lazy val queryable = adaptColumns(_.queryable)
		lazy val updatable = adaptColumns(_.updatable)
		lazy val insertable = adaptColumns(_.insertable)
		lazy val generated = adaptColumns(_.generated)

		//TODO FIXME:  this will return duplicate columns for every column that is a part of a component.
		protected def adaptColumns(columns :ComponentMapping[C, _]=>Seq[ColumnMapping[_, _]]) =
			components.flatMap(c => columns(c).map(adapt(_)))

		protected def adapt[T, X](c :ColumnMapping[T, X]) :Column[X]
	}
*/

	trait ComponentMappingAdapter[E, C, M<:Mapping[C]] extends ComponentMapping[E, C] with MappingAdapter[C, C, M]

	trait ComponentMappingDecorator[E, C, M<:ComponentMapping[E, C]]
		extends ComponentMappingAdapter[E, C, M]
	{
		override def value(entity: E): C = adaptedMapping.value(entity)
	}

	trait DirectComponentMappingAdapter[E, C, M<:Mapping[C]]
		extends ComponentMappingAdapter[E, C, M] with DirectMappingDecorator[C, M]

	trait AbstractComponentMappingAdapter[E, C, M<:Mapping[C]] 
		extends ComponentMappingAdapter[E, C, M] with AbstractMappingDecorator[C, M] 


	trait AbstractDirectComponentMappingAdapter[E, C, M<:Mapping[C]]
		extends ComponentMappingAdapter[E, C, M] with DirectMappingDecorator[C, M]
	
	trait AbstractDirectComponentMappingDecorator[E, C, M<:ComponentMapping[E, C]] 
		extends AbstractDirectComponentMappingAdapter[E, C, M] with ComponentMappingDecorator[E, C, M]
	


	class EmbeddedMappingComponent[E, C, M<:Mapping[C]](val adaptedMapping :M, pick :E=>C, protected val prefix :Option[String])
		extends AbstractComponentMappingAdapter[E, C, M] with AbstractDeepMappingDecorator[C, M]
	{ adapter =>
		type Component[T] = AdapterComponent[T]

		class Column[T](col :AdaptedColumn[T]) extends DecoratorColumn[T](col)
		{
			override lazy val name = prefix.map(_ + adaptedMapping.name) getOrElse adaptedMapping.name
		}


		def value(entity: E): C = pick(entity)


		protected def adaptComponent[X](c: adaptedMapping.Component[X]): Component[X] =
			new DecoratorComponent[X](c)

		protected def adaptColumn[X](c: adaptedMapping.Column[X]): Column[X] = new Column[X](c)

	} 



}



