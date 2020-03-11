package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentSelector, Selector, SingletonComponent}
import net.noresttherein.oldsql.schema.{AnyMapping, Buff, ComponentValues, Mapping, SQLReadForm, SQLWriteForm, SubMapping}

import scala.collection.mutable


/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[O <: AnyMapping, S] extends SubMapping[O, S] {
	protected val adaptee :Mapping[S]

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


	trait ShallowProxy[O <: AnyMapping, S] extends ComponentProxy[O, S] {
		protected override val adaptee :Component[S]

		override def apply[T](component :Component[T]) :Selector[this.type, O, S, T] =
			(if (component eq adaptee)
				 Selector.ident(this, adaptee)
			 else
				 adaptee(component)
			).asInstanceOf[Selector[this.type, O, S, T]]

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq adaptee) component
			else adaptee.lift(component)

		override def components :Unique[Component[_]] = adaptee.components
		override def subcomponents :Unique[Component[_]] = adaptee.subcomponents
		override def columns :Unique[Component[_]] = adaptee.columns

		override def selectable :Unique[Component[_]] = adaptee.selectable
		override def queryable :Unique[Component[_]] = adaptee.queryable
		override def updatable :Unique[Component[_]] = adaptee.updatable
		override def autoUpdated :Unique[Component[_]] = adaptee.autoUpdated
		override def insertable :Unique[Component[_]] = adaptee.insertable
		override def autoInserted :Unique[Component[_]] = adaptee.autoInserted

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = adaptee.selectForm(components)

		override def selectForm :SQLReadForm[S] = adaptee.selectForm
		override def queryForm :SQLWriteForm[S] = adaptee.queryForm
		override def updateForm :SQLWriteForm[S] = adaptee.updateForm
		override def insertForm :SQLWriteForm[S] = adaptee.insertForm

//		override def writeForm(filter :Mapping.ColumnFilter) :SQLWriteForm[S] = adaptee.writeForm(filter)
//		override def readForm(filter :Mapping.ColumnFilter) :SQLReadForm[S] = adaptee.readForm(filter)



		override def assemble(values :Values) :Option[S] = adaptee.optionally(values.asInstanceOf[adaptee.Values])


		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ShallowProxy[_, _]]

		override def toString :String = "->" + adaptee
	}



	trait DeepProxy[O <: AnyMapping, S] extends ComponentProxy[O, S] {

		override def apply[T](component :Component[T]) :Selector[this.type, O, S, T] =
			if (component eq adaptee)
				Selector[this.type, O, S, T](component)(Extractor.ident[S].asInstanceOf[S =?> T])
			else
				Selector[this.type, O, S, T](lift(component))(adaptee.apply(dealias(component)).extractor)

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



	abstract class EagerDeepProxy[M <: Mapping[S] with Singleton, O <: AnyMapping, S] private
			(protected val adaptee :M,
			 lifts :mutable.Map[AnyMapping, ComponentSelector[_, _]],
			 dealias :mutable.Map[Mapping.AnyComponent[O], AnyMapping])
		extends DeepProxy[O, S]
	{
		def this(adaptee :M) = this(
			adaptee,
			mutable.Map[AnyMapping, ComponentSelector[_, _]](),
			mutable.Map[Mapping.AnyComponent[O], AnyMapping]()
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
			lifts.put(adapted, Selector.ident[this.type, O, S](adapted))
			dealias.put(adapted, adaptee)
		}
		private[this] val lifted = lifts.toMap
		private[this] val originals = dealias.toMap

		protected def lift[T](component :adaptee.Component[T],
		                      lifts :mutable.Map[AnyMapping, ComponentSelector[_, _]],
		                      dealias :mutable.Map[AnyComponent, AnyMapping]) :Component[T] =
			lifts.getOrElse(component, {
				val base :Selector[adaptee.type, adaptee.Owner, S, T] =
					if (adaptee eq component) Selector.ident[adaptee.type, adaptee.Owner, S](adaptee).asInstanceOf[Selector[adaptee.type, adaptee.Owner, S, T]]
					else adaptee.apply(component)
				val lifted = adapt(base.lifted)
				val selector = Selector[this.type, O, S, T](lifted)(base.extractor)
				lifts.put(component, selector)
				lifts.put(base.lifted, selector)
				dealias.put(lifted, base.lifted)
				selector
			}).lifted.asInstanceOf[Component[T]]

		override def lift[T](component :Component[T]) :Component[T] = apply(component).lifted

		override def apply[T](component :Component[T]) :Selector[this.type, O, S, T] =
			lifted.getOrElse(component,
				throw new IllegalArgumentException(s"$component is not a component of $this.")
			).asInstanceOf[Selector[this.type, O, S, T]]

		protected override def dealias[T](component :Component[T]) :adaptee.Component[T] =
			originals.getOrElse(component, component).asInstanceOf[adaptee.Component[T]]


	}



}

