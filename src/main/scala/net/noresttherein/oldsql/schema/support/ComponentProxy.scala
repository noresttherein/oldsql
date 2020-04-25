package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.{MappingNest, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.{Buff, ComponentExtractor, GenericMapping, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, NoQuery, NoSelect, NoUpdate}

import scala.collection.mutable




/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[S, O] extends GenericMapping[S, O] with MappingNest[MappingOf[S]] {

	override def buffs :Seq[Buff[S]] = egg.buffs

	override def nullValue :Option[S] = egg.nullValue

}






object ComponentProxy {


	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is. */
	trait ShallowProxy[S, O] extends ComponentProxy[S, O] with ShallowAdapter[TypedMapping[S, O], S, S, O] {
		protected override val egg :Component[S]

		override def apply[T](component :Component[T]) :Selector[T] =
			(if (component eq egg)
				 ComponentExtractor.ident(egg)
			 else
				 egg(component)
			).asInstanceOf[Selector[T]]


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(egg)) egg.selectForm(egg.selectable)
			else egg.selectForm(components)

		override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.queryForm(egg.queryable)
			else egg.queryForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.updateForm(egg.updatable)
			else egg.updateForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components.contains(egg)) egg.insertForm(egg.insertable)
			else egg.insertForm(components)

		override def selectForm :SQLReadForm[S] = egg.selectForm
		override def queryForm :SQLWriteForm[S] = egg.queryForm
		override def updateForm :SQLWriteForm[S] = egg.updateForm
		override def insertForm :SQLWriteForm[S] = egg.insertForm

//		override def writeForm(filter :Mapping.ColumnFilter) :SQLWriteForm[S] = egg.writeForm(filter)
//		override def readForm(filter :Mapping.ColumnFilter) :SQLReadForm[S] = egg.readForm(filter)



		override def assemble(pieces :Pieces) :Option[S] = egg.optionally(pieces.compatible[egg.type](egg))


		override def toString :String = "->" + egg
	}






	/** A skeleton trait for a mapping proxy which needs to adapt every component of the proxied mapping. */
	trait DeepProxy[S, O] extends ComponentProxy[S, O] {

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq egg)
				ComponentExtractor.ident(adaptEgg).asInstanceOf[Selector[T]]
			else
				ComponentExtractor[S, T, O](export(component))(egg.apply(dealias(component)))

		override def export[T](component :Component[T]) :Component[T] =
			if (component eq egg)
				adaptEgg.asInstanceOf[Component[T]]
			else
				adapt(egg.export(dealias(component)))

		private[this] def alias[T](component :egg.Component[T]) :Component[T] = adapt(egg.export(component))

		private[this] def column[T](component :egg.Component[T]) :Component[T] = adaptColumn(egg.export(component))

		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'export' form under this mapping. All column and component lists  of this instance contain
		  * components of `egg` (and `egg` itself) mapped with this method.
		  * @param component a 'export' component of `egg`.
		  */
		protected def adapt[T](component :egg.Component[T]) :Component[T]


		protected def adaptColumn[T](component :egg.Component[T]) :Component[T] = adapt(component)

		protected def adaptEgg :Component[S] = this

		/** A hook method left for subclasses to implement the mapping of the adapted components back to their
		  * originating components of the adapted mapping. Used in implementing `apply(component)` returning
		  * the extractor for the component.
		  * @param export a subcomponent of this instance; may be a component adapted from `egg` or a (sub)component
		  *               of `egg` unchanged, or even `egg` itself.
		  */
		protected def dealias[T](export :Component[T]) :egg.Component[T]

		override def components :Unique[Component[_]] = egg.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_))

		override def columns :Unique[Component[_]] = egg.columns.map(column(_))
		override def selectable :Unique[Component[_]] = egg.selectable.map(column(_))
		override def queryable :Unique[Component[_]] = egg.queryable.map(column(_))
		override def updatable :Unique[Component[_]] = egg.updatable.map(column(_))
		override def autoUpdated :Unique[Component[_]] = egg.autoUpdated.map(column(_))
		override def insertable :Unique[Component[_]] = egg.insertable.map(column(_))
		override def autoInserted :Unique[Component[_]] = egg.autoInserted.map(column(_))

		override def pick[T](component :Component[T], subject :S) :Option[T] =
			if (component eq egg) Some(subject.asInstanceOf[T])
			else egg.pick(dealias(component), subject)

		override def apply[T](component :Component[T], subject :S) :T =
			if (component eq egg) subject.asInstanceOf[T]
			else egg(dealias(component), subject)



		override def assemble(pieces :Pieces) :Option[S] = egg.optionally(pieces.asInstanceOf[egg.Pieces])


		override def toString :String = "->>" + egg
	}






	//todo: look into removing Origin and Subject parameters
	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts as well as the
	  * reverse.
	  */
	abstract class EagerDeepProxy[+M <: MappingOf[S], S, O](protected override val egg :M)
		extends DeepProxy[S, O] with MappingNest[M]
	{
		private[this] val exports = mutable.Map[Mapping, ComponentExtractor[S, _, O]]()
		private[this] val originals = mutable.Map[Mapping.MappingFrom[O], Mapping]()

		preInit()

		override val columns :Unique[Component[_]] = egg.columns.map(alias(_, true))
		override val selectable :Unique[Component[_]] = columnsWithout(NoSelect)
		override val queryable :Unique[Component[_]] = columnsWithout(NoQuery)
		override val updatable :Unique[Component[_]] = columnsWithout(NoUpdate)
		override val autoUpdated :Unique[Component[_]] = columnsWith(AutoUpdate)
		override val autoInserted :Unique[Component[_]] = columnsWith(AutoInsert)

		override val components :Unique[Component[_]] = egg.components.map(alias(_, false))
		override val subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_, false))

		{
			val adapted = adaptEgg
			exports.put(adapted, ComponentExtractor.ident[S, O](adapted))
			originals.put(adapted, egg)
			oldsql.publishMutable()
		}


		private[this] def alias[T](component :egg.Component[T], column :Boolean) :Component[T] =
			exports.getOrElse(component, {
				val base :egg.Selector[T] =
					if (egg eq component)
						ComponentExtractor.ident[S, egg.Origin](egg).asInstanceOf[egg.Selector[T]]
					else
	                    egg.apply(component)
				val export =
					if (column) adaptColumn(base.export)
					else adapt(base.export)
				val selector = ComponentExtractor[S, T, O](export)(base)
				this.exports.put(component, selector)
				this.exports.put(base.export, selector)
				if (!originals.contains(export))
					originals.put(export, base.export)
				selector
			}).export.asInstanceOf[Component[T]]


		override def export[T](component :Component[T]) :Component[T] = apply(component).export


		override def apply[T](component :Component[T]) :Selector[T] =
			exports.getOrElse(component, { //if component is not the public version, we know nothing about it
				val comp = egg.export(component.asInstanceOf[egg.Component[T]])
				exports.getOrElse(comp,
					throw new NoSuchElementException(
						s"Component $comp of $egg (public version of $component) is not on the mapping's subcomponents list."
					)
				)
			}).asInstanceOf[Selector[T]]

		/** Method called from the `EagerDeepProxy` constructor before any component lists are initialized. */
		protected def preInit(): Unit = ()

		protected override def dealias[T](component :Component[T]) :egg.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[egg.Component[T]]


	}



}

