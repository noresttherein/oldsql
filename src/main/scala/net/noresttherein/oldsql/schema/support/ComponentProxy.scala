package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor, TypedMapping}
import net.noresttherein.oldsql.schema.{Buff, GenericMapping, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, NoQuery, NoSelect, NoUpdate}

import scala.collection.mutable




/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[O, S] extends GenericMapping[O, S] with MappingNest[TypedMapping[S]] {

	override def buffs :Seq[Buff[S]] = egg.buffs

	override def nullValue :Option[S] = egg.nullValue

}






object ComponentProxy {


	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is. */
	trait ShallowProxy[O, S] extends ComponentProxy[O, S] with ShallowAdapter[Component[O, S], O, S, S] {
		protected override val egg :Component[S]

		override def apply[T](component :Component[T]) :Selector[T] =
			(if (component eq egg)
				 ComponentExtractor.ident(egg)
			 else
				 egg(component)
			).asInstanceOf[Selector[T]]


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(egg)) egg.selectForm(selectable)
			else egg.selectForm(components)

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
	trait DeepProxy[O, S] extends ComponentProxy[O, S] {

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq egg)
				ComponentExtractor.ident(adaptEgg).asInstanceOf[Selector[T]]
			else
				ComponentExtractor[O, S, T](lift(component))(egg.apply(dealias(component)))

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq egg)
				adaptEgg.asInstanceOf[Component[T]]
			else
				adapt(egg.lift(dealias(component)))

		private[this] def alias[T](component :egg.Component[T]) :Component[T] = adapt(egg.lift(component))

		private[this] def column[T](component :egg.Component[T]) :Component[T] = adaptColumn(egg.lift(component))

		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'lifted' form under this mapping. All column and component lists  of this instance contain
		  * components of `egg` (and `egg` itself) mapped with this method.
		  * @param component a component of `egg` or `egg` itself.
		  */
		protected def adapt[T](component :egg.Component[T]) :Component[T]


		protected def adaptColumn[T](component :egg.Component[T]) :Component[T] = adapt(component)

		protected def adaptEgg :Component[S] = this

		/** A hook method left for subclasses to implement the mapping of the adapted components back to their
		  * originating components of the adapted mapping. Used in implementing `apply(component)` returning
		  * the extractor for the component.
		  * @param lifted a subcomponent of this instance; may be a component adapted from `egg` or a (sub)component
		  *               of `egg` unchanged, or even `egg` itself.
		  */
		protected def dealias[T](lifted :Component[T]) :egg.Component[T]

		override def components :Unique[Component[_]] = Unique(adapt(egg)) //egg.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = adapt(egg) +: egg.subcomponents.map(alias(_))

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

		override def pick[T](component :Component[T], values :Pieces) :Option[T] =
			values.get(apply(component))

		override def apply[T](component :Component[T], subject :S) :T =
			if (component eq egg) subject.asInstanceOf[T]
			else egg(dealias(component), subject)

		override def apply[T](component :Component[T], values :Pieces) :T =
			values(apply(component))



		override def assemble(pieces :Pieces) :Option[S] = egg.optionally(pieces.asInstanceOf[egg.Pieces])


		override def toString :String = "->>" + egg
	}


	//todo: look into removing Owner and Subject parameters
	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts as well as the
	  * reverse.
	  */
	abstract class EagerDeepProxy[M <: TypedMapping[S], O, S](protected override val egg :M)
		extends DeepProxy[O, S] with MappingNest[M]
	{
		private[this] val lifted = mutable.Map[Mapping, ComponentExtractor[O, S, _]]()
		private[this] val originals = mutable.Map[Mapping.AnyComponent[O], Mapping]()

		override val columns :Unique[Component[_]] = egg.columns.map(alias(_, true))
		override val selectable :Unique[Component[_]] = columnsWithout(NoSelect)
		override val queryable :Unique[Component[_]] = columnsWithout(NoQuery)
		override val updatable :Unique[Component[_]] = columnsWithout(NoUpdate)
		override val autoUpdated :Unique[Component[_]] = columnsWith(AutoUpdate)
		override val autoInserted :Unique[Component[_]] = columnsWith(AutoInsert)

		override val components :Unique[Component[_]] = Unique(adaptEgg)
		override val subcomponents :Unique[Component[_]] =
			adaptEgg +: egg.subcomponents.map(alias(_, false))


		{
			val adapted = adaptEgg
			lifted.put(adapted, ComponentExtractor.ident[O, S](adapted))
			originals.put(adapted, egg)
		}

		private[this] def alias[T](component :egg.Component[T], column :Boolean) :Component[T] =
			lifted.getOrElse(component, {
				val base :egg.Selector[T] =
					if (egg eq component)
						ComponentExtractor.ident[egg.Owner, S](egg).asInstanceOf[egg.Selector[T]]
					else
	                    egg.apply(component)
				val lifted =
					if (column) adaptColumn(base.lifted)
					else adapt(base.lifted)
				val selector = ComponentExtractor[O, S, T](lifted)(base)
				this.lifted.put(component, selector)
				this.lifted.put(base.lifted, selector)
				originals.put(lifted, base.lifted)
				selector
			}).lifted.asInstanceOf[Component[T]]

		override def lift[T](component :Component[T]) :Component[T] = apply(component).lifted

		override def apply[T](component :Component[T]) :Selector[T] =
			lifted.getOrElse(component,
				throw new IllegalArgumentException(s"$component is not a component of $this.")
			).asInstanceOf[Selector[T]]

		protected override def dealias[T](component :Component[T]) :egg.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[egg.Component[T]]


	}



}

