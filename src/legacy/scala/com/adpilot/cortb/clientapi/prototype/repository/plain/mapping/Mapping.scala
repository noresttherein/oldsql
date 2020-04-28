package com.adpilot.cortb.clientapi.prototype.repository.plain.mapping


import com.adpilot.cortb.clientapi.prototype.repository.plain.mapping.ColumnValues.ColumnIndex
import com.adpilot.cortb.clientapi.util.{OptionOps, Repeat}

import scala.reflect.ClassTag
import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}


import Mapping._
import ColumnMapping._
import ComponentMapping._
import ColumnOption._


import Repeat._
import OptionOps._

trait AnyMapping {
	type ResultType
	type Component[_] <: AnyMapping
	type AnyColumn <: Component[_]
	type Component[T] <: ComponentMapping[ResultType, T] with Component[_]
	type Column[T] <: ColumnMapping[ResultType, T] with Component[T] with AnyColumn


	def readValue(res :PositionedResult) :ResultType
	def assemble(res :ColumnValues) :ResultType

	def selectable :Seq[Column[_]]


	def valuesFor(component :Component[_]) :ColumnValues => ColumnValues
}



trait Mapping[E] extends GetResult[E] with AnyMapping { self =>
	type ResultType = E
	type Component[T] <: ComponentMapping[E, T]
	type Column[T] <: ColumnMapping[E, T] with Component[T]
	type Component[_] = Component[_]
	type AnyColumn = Column[_]


	def components :Seq[Component[_]]
	def nestedComponents :Seq[Component[_]]

	def columns :Seq[Column[_]]
	def querable :Seq[Column[_]]
	def selectable :Seq[Column[_]]
	def updatable :Seq[Column[_]]
	def insertable :Seq[Column[_]]
	def generated :Seq[Column[_]]
	
	def columnsWith[O[X]<:ColumnOption[X] :ColumnOptionType] = columns.filter(_.enabled[O])
	def columnsWithout[O[X]<:ColumnOption[X] :ColumnOptionType] = columns.filterNot(_.enabled[O])


	def InsertParameters :SetParameter[E]
	def UpdateParameters :SetParameter[E]

//	final def params(columns :Iterable[Column[_]])(value :E) = columns.map(c => c.paramFrom(value))



//	def valuesFor(component :Component[_])(values :ColumnValues) :ColumnValues
	def valuesFor(component :Component[_]) :ColumnValues => ColumnValues

	
	final def readValue(res :PositionedResult) :ResultType = apply(res)

	final def assemble(values :ColumnValues) :ResultType = apply(values)

	private lazy val selectableIndex = ColumnIndex(selectable)

	def apply(res: PositionedResult): E = {
		val entity = apply(ColumnValues(selectableIndex, res))
		selectable.size times { res.skip }
		entity
	}

	def apply(values :ColumnValues): E


	def qualified(prefix :String) :Mapping[E] = new QualifiedMapping[E](this, prefix)



/*
	def selectIncluding(explicitColumns :Column[_]*) :Mapping[E] = new AbstractMapping[E] {

		val columns = {
			def mapColumn[T](column :Column[T]) = new EmbeddedColumn(column, column.value, None, Seq(ReadOnly))
//				ColumnMapping(column.name, column.value _, ReadOnly)
			val extra = explicitColumns.flatMap {
				case c @ ExplicitSelect(_) => Some(mapColumn(c))
				case c if c.disabled[NoSelect] => None
				case c => throw new IllegalArgumentException(s"Attempted to explicitly select non-selectable column $c in $this")
			}
			val standard = self.columns.map(mapColumn(_))

			standard ++: extra
		}

		override def apply(values: ColumnValues): E = ???

		override def valuesFor(component: Component[_])(values: ColumnValues): ColumnValues = ???
	}
*/

//	def withValues(values :ColumnValues.Value[_]*) :Mapping[E] =
//		new DirectMappingAdapter[E, self.type] {
//			import ColumnValues.Value
//			private val ommitedColumns = values.map(v => (v.column :ColumnMapping[_, _]) -> v.value).toMap[ColumnMapping[_, _], Any]
//
//			val adaptedMapping = self :self.type
//
//			override def selectable: Seq[Column[_]] = adaptedMapping.selectable.filterNot(ommitedColumns.keySet.contains(_))
//
//			override def apply(values: ColumnValues): E = {
//				val all = ColumnValues(selectable.view.zipWithIndex.map { case (col, i) => valueFor(values)(col, i) })
//				adaptedMapping(values)
//			}
//
//			private def valueFor[T](values :ColumnValues)(column :Column[T], idx :Int) =
//				Value[T](column :ColumnMapping[_, T], ommitedColumns.getOrElse(column, values(idx)).asInstanceOf[T])
//		}
	
}

object Mapping {

	def apply[E](columns :Seq[ColumnMapping[E, _]], map :Seq[_]=>E) :Mapping[E] =
		new ColumnSeqMapping[E](columns, map)

	
	
/*
	trait AbstractComponentSeqMapping[E] extends Mapping[E] {

		lazy val columns = adaptColumns(_.columns)
		lazy val selectable = adaptColumns(_.selectable)
		lazy val queryable = adaptColumns(_.queryable)
		lazy val updatable = adaptColumns(_.updatable)
		lazy val insertable = adaptColumns(_.insertable)
		lazy val generated = adaptColumns(_.generated)

		//TODO FIXME:  this will return duplicate columns for every column that is a part of a component.
		protected def adaptColumns(columns :ComponentMapping[E, _]=>Seq[ColumnMapping[_, _]]) =
			components.flatMap(c => columns(c).map(adapt(_)))

		protected def adapt[T, X](c :ColumnMapping[T, X]) :Column[X]
	}
*/

	trait UniversalMapping[E] extends Mapping[E] {
		type Component[X] = ComponentMapping[E, X]
		type Column[X] = ColumnMapping[E, X]
	}

	
	trait AbstractMapping[E] extends UniversalMapping[E] {

		def components = columns
		def nestedComponents = columns

		import ColumnOption._

		lazy val querable = columnsWithout(NoQuery)
		lazy val selectable = columnsWithout(NoSelect)
		lazy val updatable = columnsWithout(NoUpdate)
		lazy val insertable = columnsWithout(NoInsert)
		lazy val generated = columnsWith(AutoGen)

		lazy val InsertParameters: SetParameter[E] = SetParameters(insertable:_*)
		lazy val UpdateParameters: SetParameter[E] = SetParameters(updatable:_*)

//		override def valuesFor(component: Component[_])(values: ColumnValues): ColumnValues =
//			ColumnValues(component.selectable.map(columnValue(_)(values)))
//
//		protected def valueFor[T](column :Column[T])(values :ColumnValues) :T = columnValue(column)(values).value
//
//		protected def columnValue[T](column :Column[T])(values :ColumnValues) :ColumnValues.Value[T] =
//			columns.indexOf(column).providing(_>=0).map(i => ColumnValues.Value(column, values(i))) getOrElse {
//				throw new IllegalArgumentException(s"column $column is not a column of $this")
//			}

	}

	
	class ColumnSeqMapping[E](val columns :Seq[ColumnMapping[E, _]], map :Seq[_] => E)
		extends AbstractMapping[E]
	{

		override def valuesFor(component: Component[_]) = identity

		override def apply(values: ColumnValues): E =
			map(columns.map(c => values(c) getOrElse {
				throw new IllegalArgumentException(s"no value for column $c of ColumnSeqMapping in $values")
			}))
	}




	trait MappingAdapter[E, S, M<:Mapping[S]] extends Mapping[E] {
		val adaptedMapping :M
		
		type AdaptedComponent[X] = adaptedMapping.Component[X]
		type AdaptedColumn[X] = adaptedMapping.Column[X]		
	}


	trait MappingDecorator[E, M<:Mapping[E]] extends MappingAdapter[E, E, M]


	trait DirectMappingDecorator[E, M<:Mapping[E]] extends MappingDecorator[E, M] {
		type Component[X] = adaptedMapping.Component[X]
		type Column[X] = adaptedMapping.Column[X]

		override def components = adaptedMapping.components
		override def nestedComponents = adaptedMapping.nestedComponents
		override def columns = adaptedMapping.columns
		override def selectable = adaptedMapping.selectable
		override def querable = adaptedMapping.querable
		override def updatable = adaptedMapping.updatable
		override def insertable = adaptedMapping.insertable
		override def generated = adaptedMapping.generated

		def InsertParameters = adaptedMapping.InsertParameters
		def UpdateParameters = adaptedMapping.UpdateParameters

		
		override def apply(res: PositionedResult): E = adaptedMapping(res)

		override def apply(values: ColumnValues): E = adaptedMapping(values)


		override def valuesFor(c: Component[_]) =
			adaptedMapping.valuesFor(c)
	}
	
	
	trait AbstractMappingAdapter[E, S, M<:Mapping[S]] extends MappingAdapter[E, S, M] {
		import ColumnOption._

		lazy val components = adaptComponents(adaptedMapping.components)
		lazy val nestedComponents = adaptComponents(adaptedMapping.nestedComponents)
		lazy val columns = adaptColumns(adaptedMapping.columns)

		lazy val selectable = filterNot[NoSelect]
		lazy val querable =  filterNot[NoQuery]
		lazy val updatable = filterNot[NoUpdate]
		lazy val insertable = filterNot[NoInsert]
		lazy val generated = columns.filter(_.enabled(AutoGen))

		lazy val InsertParameters = SetParameters(insertable:_*)
		lazy val UpdateParameters = SetParameters(updatable:_*)

		
		
		private def filterNot[O[X] <:ColumnOption[X] :ColumnOptionType] =
			columns.filterNot(_.enabled(ColumnOptionType[O]))


		protected def adaptComponents(components :Seq[adaptedMapping.Component[_]]) :Seq[Component[_]] =
			components.map(c => adapt(c))

		protected def adaptColumns(columns :Seq[adaptedMapping.Column[_]]) :Seq[Column[_]] =
			columns.map(c => adaptColumn(c))

		protected final def adapt[X](c :adaptedMapping.Component[X]) :Component[X] = c match {
			case _:ColumnMapping[_, _] => adaptColumn[X](c.asInstanceOf[adaptedMapping.Column[X]])
			case _ => adaptComponent[X](c)
		}
		protected def adaptComponent[X](component :adaptedMapping.Component[X]) :Component[X]
		protected def adaptColumn[X](column :adaptedMapping.Column[X]) :Column[X]
		
	}
	
	
	trait UniversalMappingAdapter[E, S, M<:Mapping[S]] extends AbstractMappingAdapter[E, S, M] with UniversalMapping[E]

	
	
	trait AbstractMappingDecorator[E, C<:Mapping[E]] extends AbstractMappingAdapter[E, E, C] with MappingDecorator[E, C] {

		override def apply(res: PositionedResult): E = adaptedMapping(res)

		override def apply(values :ColumnValues) :E = adaptedMapping(values)
	}
	
	trait UniversalMappingDecorator[E, M<:Mapping[E]] extends AbstractMappingDecorator[E, M] with UniversalMapping[E]
	
	
	
	
	
	
	trait AbstractDeepMappingAdapter[E, S, M<:Mapping[S]] extends AbstractMappingAdapter[E, S, M] {
		adapter =>

		
		trait AdapterComponent[X] extends ComponentMapping[E, X] {
			def adaptedComponent :AdaptedComponent[X]
		}

		trait AdapterColumn[X] extends AdapterComponent[X] with ColumnAdapter[E, X, AdaptedColumn[X]] {
			def adaptedComponent = adaptedMapping
		}


		protected abstract class AbstractComponent[X](val adaptedMapping :AdaptedComponent[X])
			extends ComponentMappingAdapter[E, X, AdaptedComponent[X]] with AdapterComponent[X]
		{
			final def adaptedComponent = adaptedMapping
		}

		protected abstract class DirectComponent[X](comp :AdaptedComponent[X])
			extends AbstractComponent[X](comp) with DirectComponentMappingAdapter[E, X, AdaptedComponent[X]]

		protected abstract class DirectColumn[X](val adaptedMapping :AdaptedColumn[X])
			extends AdapterColumn[X] with DirectColumnAdapter[E, X, AdaptedColumn[X]]


		type Component[X] <: AdapterComponent[X]
		type Column[X] <: AdapterColumn[X] with Component[X]

		
		protected val valuesAdapter :ColumnValues=>ColumnValues =
			ColumnSelection.Adapted(columns.map(c => c.adaptedMapping->c))

		override def valuesFor(component: Component[_]): ColumnValues => ColumnValues =
			valuesAdapter andThen adaptedMapping.valuesFor(component.adaptedComponent)
	}

	trait AbstractDeepMappingDecorator[E, M<:Mapping[E]]
		extends AbstractDeepMappingAdapter[E, E, M] with AbstractMappingDecorator[E, M]
	{ adapter =>

		protected class DecoratorComponent[X](comp :AdaptedComponent[X])
			extends DirectComponent[X](comp) with ComponentMappingDecorator[E, X, AdaptedComponent[X]]

		protected class DecoratorColumn[X](col :AdaptedColumn[X])
			extends DirectColumn[X](col) with DirectColumnDecorator[E, X, AdaptedColumn[X]]
	}


	trait AbstractMappedMapping[E, S, M<:Mapping[S]] extends AbstractDeepMappingAdapter[E, S, Mapping[S]] {

		trait Component[X] extends AdapterComponent[X] {
			def value(entity: E): X = adaptedComponent.value(fromResultType(entity))
		}

		class Column[X](col :AdaptedColumn[X]) extends DirectColumn[X](col) with Component[X]

		private class ComponentImpl[X](comp :AdaptedComponent[X]) extends DirectComponent[X](comp) with Component[X]

		protected def adaptComponent[X](comp :adaptedMapping.Component[X]) :Component[X] =
			new ComponentImpl[X](comp)

		protected def adaptColumn[X](column :adaptedMapping.Column[X]) :Column[X] =
			new Column[X](column)

//		val InsertParameters = SetParameter[E]{
//			case (entity, params) => adaptedMapping.InsertParameters(fromResultType(entity), params)
//		}
//		val UpdateParameters = SetParameter[E]{
//			case (entity, params) => adaptedMapping.UpdateParameters(fromResultType(entity), params)
//		}

		def apply(values: ColumnValues): E = toResultType(adaptedMapping(values))

		def toResultType(value :S) :E
		def fromResultType(value :E) :S
	}


	class MappedMapping[E, S](val adaptedMapping :Mapping[S], fromBase: S=>E, toBase: E=>S)
		extends AbstractMappedMapping[E, S, Mapping[S]]
	{
		def toResultType(value: S): E = fromBase(value)

		def fromResultType(value: E): S = toBase(value)
	}
	
	
	
	class QualifiedMapping[E](val adaptedMapping :Mapping[E], prefix :String)
		extends AbstractDeepMappingDecorator[E, Mapping[E]]
	{
		type Component[X] = AdapterComponent[X]

		class Column[X](col :AdaptedColumn[X]) extends DecoratorColumn[X](col) {
			override val name = prefix+ "." +col.name
		}

		protected def adaptColumn[X](column: adaptedMapping.Column[X]): Column[X] =
			new Column[X](column)

		protected def adaptComponent[X](component: adaptedMapping.Component[X]): Component[X] =
			new DecoratorComponent[X](component)

	}



	
	
	
	
	def prefixOption(prefix :String) = Option(prefix).filter(_.length>0)







	trait QueryParam {
		def set(ps :PositionedParameters)
	}

	object QueryParam {
		implicit def apply[T :SetParameter](value :T) :QueryParam = new QueryParam {
			override def set(ps: PositionedParameters): Unit = ps >> value
		}
	}

	implicit object SetParameterSeq extends SetParameter[Seq[QueryParam]] {
		override def apply(values: Seq[QueryParam], params: PositionedParameters): Unit =
			values.foreach(_.set(params))
	}


	object SetParameters {
		def apply[E](columns :ColumnMapping[E, _]*) = SetParameter[E](
			(entity, params) => columns.foreach(_.paramFrom(entity).set(params))
		)

		def repeat[T](setter :SetParameter[T]) = SetParameter[Seq[T]](
			(seq, params) => seq.foreach(setter(_, params))
		)


		implicit val NoOp :SetParameter[Any] = SetParameter[Any]((_, _) => ())
		implicit val Unit :SetParameter[Unit] = SetParameter[Unit]((_, _) => ())
	}

	def GetColumns[E](columns :ColumnMapping[E, _]*) = GetResult[Seq[Any]](
		params => columns.map(_(params))
	)
}
