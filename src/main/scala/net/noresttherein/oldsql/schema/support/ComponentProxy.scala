package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{Component, ComponentSelector, GeneralSelector, ComponentFor, SingletonFor}
import net.noresttherein.oldsql.schema.{Mapping, Buff, SQLReadForm, SQLWriteForm, SubMapping}

import scala.collection.mutable




/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[O, S] extends SubMapping[O, S] {
	protected val adaptee :ComponentFor[S]

	override def buffs :Seq[Buff[S]] = adaptee.buffs

	override def sqlName :Option[String] = adaptee.sqlName

	override def nullValue :Option[S] = adaptee.nullValue



	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentProxy[_, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case proxy :ComponentProxy[_, _] => canEqual(proxy) && proxy.canEqual(this) && adaptee == proxy.adaptee
		case _ => false
	}

	override def hashCode :Int = adaptee.hashCode

}






object ComponentProxy {


	trait ShallowProxy[O, S] extends ComponentProxy[O, S] with MappingAdapter[Component[O, S], O, S, S] {
		protected override val adaptee :Component[S]

		override def apply[T](component :Component[T]) :Selector[T] =
			(if (component eq adaptee)
				 ComponentSelector.ident(this, adaptee)
			 else
				 adaptee(component)
			).asInstanceOf[Selector[T]]

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq adaptee) component
			else adaptee.lift(component)


		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components.contains(adaptee)) adaptee.selectForm(selectable)
			else adaptee.selectForm(components)

		override def selectForm :SQLReadForm[S] = adaptee.selectForm
		override def queryForm :SQLWriteForm[S] = adaptee.queryForm
		override def updateForm :SQLWriteForm[S] = adaptee.updateForm
		override def insertForm :SQLWriteForm[S] = adaptee.insertForm

//		override def writeForm(filter :Mapping.ColumnFilter) :SQLWriteForm[S] = adaptee.writeForm(filter)
//		override def readForm(filter :Mapping.ColumnFilter) :SQLReadForm[S] = adaptee.readForm(filter)



		override def assemble(values :Values) :Option[S] = adaptee.optionally(values.identical[adaptee.type](adaptee))


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ShallowProxy[_, _]]

		override def toString :String = "->" + adaptee
	}



	trait DeepProxy[O, S] extends ComponentProxy[O, S] {

		override def apply[T](component :Component[T]) :Selector[T] =
			if (component eq adaptee)
				ComponentSelector[this.type, O, S, T](component)(Extractor.ident[S].asInstanceOf[S =?> T])
			else
				ComponentSelector[this.type, O, S, T](lift(component))(adaptee.apply(dealias(component)).extractor)

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq adaptee)
				component
			else
				adapt(adaptee.lift(dealias(component)))

		protected def adapt[T](component :adaptee.Component[T]) :Component[T]

		private[this] def alias[T](component :adaptee.Component[T]) :Component[T] = adapt(adaptee.lift(component))

		protected def dealias[T](lifted :Component[T]) :adaptee.Component[T]

		override def components :Unique[Component[_]] = Unique(adapt(adaptee)) //adaptee.components.map(alias(_))
		override def subcomponents :Unique[Component[_]] = adaptee.subcomponents.map(alias(_))

		override def columns :Unique[Component[_]] = adaptee.columns.map(alias(_))
		override def selectable :Unique[Component[_]] = adaptee.selectable.map(alias(_))
		override def queryable :Unique[Component[_]] = adaptee.queryable.map(alias(_))
		override def updatable :Unique[Component[_]] = adaptee.updatable.map(alias(_))
		override def autoUpdated :Unique[Component[_]] = adaptee.autoUpdated.map(alias(_))
		override def insertable :Unique[Component[_]] = adaptee.insertable.map(alias(_))
		override def autoInserted :Unique[Component[_]] = adaptee.autoInserted.map(alias(_))

		override def valueOf[T](component :Component[T], subject :S) :Option[T] =
			if (component eq adaptee) Some(subject.asInstanceOf[T])
			else adaptee.valueOf(dealias(component), subject)

		override def valueOf[T](component :Component[T], values :Values) :Option[T] =
			values.get(apply(component))

		override def apply[T](component :Component[T], subject :S) :T =
			if (component eq adaptee) subject.asInstanceOf[T]
			else adaptee(dealias(component), subject)

		override def apply[T](component :Component[T], values :Values) :T =
			values(apply(component))



		override def assemble(values :Values) :Option[S] = apply(adapt(adaptee)).get(values)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DeepProxy[_, _]]

		override def toString :String = "->>" + adaptee
	}



	abstract class EagerDeepProxy[M <: SingletonFor[S], O, S] private
			(protected val adaptee :M,
			 lifts :mutable.Map[Mapping, GeneralSelector[_, _]],
			 dealias :mutable.Map[Mapping.AnyComponent[O], Mapping])
		extends DeepProxy[O, S]
	{
		def this(adaptee :M) = this(
			adaptee,
			mutable.Map[Mapping, GeneralSelector[_, _]](),
			mutable.Map[Mapping.AnyComponent[O], Mapping]()
		)


		override val components :Unique[Component[_]] = Unique(lift(adaptee, lifts, dealias)) //adaptee.components.map(lift(_, lifts, dealias))

		override val subcomponents :Unique[Component[_]] = adaptee.subcomponents.map(lift(_, lifts, dealias))
		override val columns :Unique[Component[_]] = adaptee.columns.map(lift(_, lifts, dealias))

		override val selectable :Unique[Component[_]] = adaptee.selectable.map(lift(_, lifts, dealias))
		override val queryable :Unique[Component[_]] = adaptee.queryable.map(lift(_, lifts, dealias))
		override val updatable :Unique[Component[_]] = adaptee.updatable.map(lift(_, lifts, dealias))
		override val autoUpdated :Unique[Component[_]] = adaptee.autoUpdated.map(lift(_, lifts, dealias))
		override val insertable :Unique[Component[_]] = adaptee.insertable.map(lift(_, lifts, dealias))
		override val autoInserted :Unique[Component[_]] = adaptee.autoInserted.map(lift(_, lifts, dealias))

		{
			val adapted = adapt[S](adaptee)
			lifts.put(adapted, ComponentSelector.ident[this.type, O, S](adapted))
			dealias.put(adapted, adaptee)
		}
		private[this] val lifted = lifts.toMap
		private[this] val originals = dealias.toMap

		protected def lift[T](component :adaptee.Component[T],
		                      lifts :mutable.Map[Mapping, GeneralSelector[_, _]],
		                      dealias :mutable.Map[AnyComponent, Mapping]) :Component[T] =
			lifts.getOrElse(component, {
				val base :adaptee.Selector[T] =
					if (adaptee eq component)
						ComponentSelector.ident[adaptee.type, adaptee.Owner, S](adaptee).asInstanceOf[adaptee.Selector[T]]
					else
	                    adaptee.apply(component)
				val lifted = adapt(base.lifted)
				val selector = ComponentSelector[this.type, O, S, T](lifted)(base.extractor)
				lifts.put(component, selector)
				lifts.put(base.lifted, selector)
				dealias.put(lifted, base.lifted)
				selector
			}).lifted.asInstanceOf[Component[T]]

		override def lift[T](component :Component[T]) :Component[T] = apply(component).lifted

		override def apply[T](component :Component[T]) :Selector[T] =
			lifted.getOrElse(component,
				throw new IllegalArgumentException(s"$component is not a component of $this.")
			).asInstanceOf[Selector[T]]

		protected override def dealias[T](component :Component[T]) :adaptee.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[adaptee.Component[T]]


	}



}

