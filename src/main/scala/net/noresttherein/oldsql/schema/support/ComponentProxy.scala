package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentExtractor, ComponentFor}
import net.noresttherein.oldsql.schema.{Mapping, Buff, SQLReadForm, SQLWriteForm, AbstractMapping}

import scala.collection.mutable




/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[O, S] extends AbstractMapping[O, S] with MappingNest[ComponentFor[S]] {

	override def buffs :Seq[Buff[S]] = egg.buffs

	override def nullValue :Option[S] = egg.nullValue

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentProxy[_, _]]

}






object ComponentProxy {


	/** A skeleton of a mapping proxy which uses the components of the proxied mapping as-is. */
	trait ShallowProxy[O, S] extends ComponentProxy[O, S] with MappingAdapter[Component[O, S], O, S, S] {
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


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ShallowProxy[_, _]]

		override def toString :String = "->" + egg
	}



	/** A skeleton trait for a mapping proxy which needs to adapt every component of the proxied mapping. */
	trait DeepProxy[O, S] extends ComponentProxy[O, S] {

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq egg)
				ComponentExtractor.ident(component).asInstanceOf[Selector[T]]
			else
				ComponentExtractor[O, S, T](lift(component))(egg.apply(dealias(component)))

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq egg)
				adapt(egg).asInstanceOf[Component[T]]
			else
				adapt(egg.lift(dealias(component)))

		private[this] def alias[T](component :egg.Component[T]) :Component[T] = adapt(egg.lift(component))

		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'lifted' form under this mapping. All column and compmonent lists  of this instance contain
		  * components of `egg` (and `egg` itself) mapped with this method.
		  * @param component a component of `egg` or `egg` itself.
		  */
		protected def adapt[T](component :egg.Component[T]) :Component[T]

		/** A hook method left for subclasses to implement the mapping of the adapted components back to their
		  * originating components of the adapted mapping. Used in implementing `apply(component)` returning
		  * the extractor for the component.
		  * @param lifted a subcomponent of this instance; may be a component adapted from `egg` or a (sub)component
		  *               of `egg` unchanged, or even `egg` itself.
		  */
		protected def dealias[T](lifted :Component[T]) :egg.Component[T]

		override def components :Unique[Component[_]] = Unique(adapt(egg)) //egg.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_))

		override def columns :Unique[Component[_]] = egg.columns.map(alias(_))
		override def selectable :Unique[Component[_]] = egg.selectable.map(alias(_))
		override def queryable :Unique[Component[_]] = egg.queryable.map(alias(_))
		override def updatable :Unique[Component[_]] = egg.updatable.map(alias(_))
		override def autoUpdated :Unique[Component[_]] = egg.autoUpdated.map(alias(_))
		override def insertable :Unique[Component[_]] = egg.insertable.map(alias(_))
		override def autoInserted :Unique[Component[_]] = egg.autoInserted.map(alias(_))

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



		override def assemble(pieces :Pieces) :Option[S] = pieces.get(apply(adapt(egg)))



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DeepProxy[_, _]]

		override def toString :String = "->>" + egg
	}



	/** A `DeepProxy` implementation which eagerly initializes all column and component lists and creates
	  * a fixed mapping between components of the adapted mapping and their adapted counterparts as well as the
	  * reverse.
	  */
	abstract class EagerDeepProxy[M <: ComponentFor[S], O, S] (protected override val egg :M)
		extends DeepProxy[O, S] with MappingNest[M]
	{
		private[this] val lifted = mutable.Map[Mapping, ComponentExtractor[O, S, _]]()
		private[this] val originals = mutable.Map[Mapping.AnyComponent[O], Mapping]() 

		override val components :Unique[Component[_]] = Unique(alias(egg))

		override val subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_))
		override val columns :Unique[Component[_]] = egg.columns.map(alias(_))

		override val selectable :Unique[Component[_]] = egg.selectable.map(alias(_))
		override val queryable :Unique[Component[_]] = egg.queryable.map(alias(_))
		override val updatable :Unique[Component[_]] = egg.updatable.map(alias(_))
		override val autoUpdated :Unique[Component[_]] = egg.autoUpdated.map(alias(_))
		override val insertable :Unique[Component[_]] = egg.insertable.map(alias(_))
		override val autoInserted :Unique[Component[_]] = egg.autoInserted.map(alias(_))

		{
			val adapted = adapt[S](egg)
			lifted.put(adapted, ComponentExtractor.ident[O, S](adapted))
			originals.put(adapted, egg)
		}

		private[this] def alias[T](component :egg.Component[T]) :Component[T] =
			lifted.getOrElse(component, {
				val base :egg.Selector[T] =
					if (egg eq component)
						ComponentExtractor.ident[egg.Owner, S](egg).asInstanceOf[egg.Selector[T]]
					else
	                    egg.apply(component)
				val lifted = adapt(base.lifted)
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

