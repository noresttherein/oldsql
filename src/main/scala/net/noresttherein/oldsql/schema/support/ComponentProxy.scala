package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, MappingNest, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping, MappingExtract, GenericMapping, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, NoQuery, NoSelect, NoUpdate}
import net.noresttherein.oldsql.schema.MappingExtract.ColumnExtract

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


		override def apply[T](component :Component[T]) :Extract[T] =
			(if (component eq egg)
				 MappingExtract.ident(egg)
			 else
				 egg(component)
			).asInstanceOf[Extract[T]]
		
		override def apply[T](column :Column[T]) :ColumnExtract[S, T, O] =
			(if (column eq egg)
				MappingExtract.ident(column)
			else
				 egg(column)
			).asInstanceOf[ColumnExtract[S, T, O]]


		
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



		override def assemble(pieces :Pieces) :Option[S] =
			egg.assemble(pieces.compatible[egg.type](egg))


		override def toString :String = "->" + egg
	}






	/** A skeleton trait for a mapping proxy which needs to adapt every component of the proxied mapping. */
	trait DeepProxy[S, O] extends ComponentProxy[S, O] {

		override def apply[T](component :Component[T]) :Extract[T] =
			if (component.isInstanceOf[ColumnMapping[_, _]])
				apply(component.asInstanceOf[Column[T]])
			else if (component eq egg)
				MappingExtract.ident(adaptEgg).asInstanceOf[Extract[T]]
			else
				MappingExtract[S, T, O](export(component))(egg.apply(dealias(component)))

		override def apply[T](column :Column[T]) :ColumnExtract[S, T, O] =
			if (column eq egg)
				MappingExtract.ident(adaptEgg.asInstanceOf[Column[S]]).asInstanceOf[ColumnExtract[S, T, O]]
			else
				MappingExtract[S, T, O](export(column))(egg.apply(dealias(column)))
		


		override def export[T](component :Component[T]) :Component[T] =
			if (component.isInstanceOf[ColumnMapping[_, _]])
				export(component.asInstanceOf[Column[T]])
			else if (component eq egg)
				adaptEgg.asInstanceOf[Component[T]]
			else
				adapt(egg.export(dealias(component)))
		
		override def export[T](column :Column[T]) :Column[T] =
			if (column eq egg)
				adaptEgg.asInstanceOf[Column[T]]
			else
				adaptColumn(egg.export(dealias(column)))
		
		

		private[this] def alias[T](component :egg.Component[T]) :Component[T] = component match {
			case column :ColumnMapping[_, _] =>
				adaptColumn(egg.export(column.asInstanceOf[egg.Column[T]]))
			case _ =>
				adapt(egg.export(component))
		}

		private[this] def alias[T](column :egg.Column[T]) :Column[T] = adaptColumn(egg.export(column))



		/** A hook method left for subclasses to implement adapting of a component of the adapted mapping
		  * to its 'export' form under this mapping. All column and component lists  of this instance contain
		  * components of `egg` (and `egg` itself) mapped with this method.
		  * @param component a 'export' component of `egg`.
		  */
		protected def adapt[T](component :egg.Component[T]) :Component[T]

		protected def adaptColumn[T](column :egg.Column[T]) :Column[T] //= adapt(component)

		protected def adaptEgg :Component[S] = this



		/** A hook method left for subclasses to implement the mapping of the adapted components back to their
		  * originating components of the adapted mapping. Used in implementing `apply(component)` returning
		  * the extract for the component.
		  * @param export a subcomponent of this instance; may be a component adapted from `egg` or a (sub)component
		  *               of `egg` unchanged, or even `egg` itself.
		  */
		protected def dealias[T](export :Component[T]) :egg.Component[T]
		
		protected def dealias[T](export :Column[T]) :egg.Column[T]

		
		
		override def components :Unique[Component[_]] = egg.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_))

		override def columns :Unique[Column[_]] = egg.columns.map(alias(_))
		override def selectable :Unique[Column[_]] = egg.selectable.map(alias(_))
		override def queryable :Unique[Column[_]] = egg.queryable.map(alias(_))
		override def updatable :Unique[Column[_]] = egg.updatable.map(alias(_))
		override def autoUpdated :Unique[Column[_]] = egg.autoUpdated.map(alias(_))
		override def insertable :Unique[Column[_]] = egg.insertable.map(alias(_))
		override def autoInserted :Unique[Column[_]] = egg.autoInserted.map(alias(_))

		
		
		override def pick[T](component :Component[T], subject :S) :Option[T] =
			if (component eq egg) Some(subject.asInstanceOf[T])
			else egg.pick(dealias(component), subject)

		override def apply[T](component :Component[T], subject :S) :T =
			if (component eq egg) subject.asInstanceOf[T]
			else egg(dealias(component), subject)



		override def assemble(pieces :Pieces) :Option[S] = //use egg.assemble to bypass buffs on the egg
			egg.assemble(pieces.asInstanceOf[egg.Pieces]) //and use only those on the proxy


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
		private[this] val exports = mutable.Map[Mapping, MappingExtract[S, _, O]]()
		private[this] val originals = mutable.Map[MappingFrom[O], Mapping]()

		preInit()

		override val columns :Unique[Column[_]] = egg.columns.map(alias(_))
		override val selectable :Unique[Column[_]] = columnsWithout(NoSelect)
		override val queryable :Unique[Column[_]] = columnsWithout(NoQuery)
		override val updatable :Unique[Column[_]] = columnsWithout(NoUpdate)
		override val autoUpdated :Unique[Column[_]] = columnsWith(AutoUpdate)
		override val autoInserted :Unique[Column[_]] = columnsWith(AutoInsert)

		override val components :Unique[Component[_]] = egg.components.map(alias(_))
		override val subcomponents :Unique[Component[_]] = egg.subcomponents.map(alias(_))

		{
			val adapted = adaptEgg
			exports.put(adapted, MappingExtract.ident[S, O](adapted))
			originals.put(adapted, egg)
			oldsql.publishMutable()
		}



		private[this] def alias[T](component :egg.Component[T]) :Component[T] =
			exports.getOrElse(component, {
				val (base, export, selector) = component match {
					case col :ColumnMapping[_, _] =>
						val column = col.asInstanceOf[egg.Column[T]]
						val (base, export) =
							if (egg eq component)
								MappingExtract.ident(column).asInstanceOf[egg.Extract[T]] ->
									adaptEgg.asInstanceOf[Column[T]]
							else {
	                            val base = egg.apply(column)
								(base, adaptColumn(base.export))
							}
						(base, export, MappingExtract(export)(base))
					case _ =>
						val (base, export) =
							if (egg eq component)
								MappingExtract.ident(component).asInstanceOf[egg.Extract[T]] ->
									adaptEgg.asInstanceOf[Component[T]]
							else {
								val base = egg.apply(component)
								(base, adapt(base.export))
							}
						(base, export, MappingExtract(export)(base))
				}
				this.exports.put(component, selector)
				this.exports.put(base.export, selector)
				if (!originals.contains(export))
					originals.put(export, base.export)
				selector
			}).export.asInstanceOf[Component[T]]



		@inline private[this] def alias[T](column :egg.Column[T]) :Column[T] =
			alias(column :egg.Component[T]).asInstanceOf[Column[T]]



		override def export[T](component :Component[T]) :Component[T] = apply(component).export

		override def export[T](column :Column[T]) :Column[T] = apply(column).export



		override def apply[T](component :Component[T]) :Extract[T] =
			exports.getOrElse(component, { //if component is not the export version, we know nothing about it
				val comp = egg.export(component.asInstanceOf[egg.Component[T]])
				exports.getOrElse(comp,
					throw new NoSuchElementException(
						s"Component $comp of $egg (export version of $component) is not on the mapping's subcomponents list."
					)
				)
			}).asInstanceOf[Extract[T]]

		override def apply[T](column :Column[T]) :ColumnExtract[S, T, O] =
			apply(column :Component[T]).asInstanceOf[ColumnExtract[S, T, O]]



		/** Method called from the `EagerDeepProxy` constructor before any component lists are initialized. */
		protected def preInit(): Unit = ()

		protected override def dealias[T](component :Component[T]) :egg.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[egg.Component[T]]

		protected override def dealias[T](column :Column[T]) :egg.Column[T] =
			originals.getOrElse(column, column).asInstanceOf[egg.Column[T]]


	}



}

