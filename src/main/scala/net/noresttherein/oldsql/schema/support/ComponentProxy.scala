package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, ComponentSelector, Selector, SingletonComponent}
import net.noresttherein.oldsql.schema.{AnyMapping, Buff, ComponentValues, Mapping, SQLReadForm, SQLWriteForm, SubMapping}

import scala.collection.mutable


/**
  * @author Marcin Mościcki
  */
trait ComponentProxy[O <: AnyMapping, S] extends SubMapping[O, S] {
	protected val adaptee :Component[S]

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

		override def apply[T](component :Component[T]) :Selector[this.type, O, S, T] =
			(if (component eq adaptee)
				 Selector(this, adaptee)(identity[S])
			 else
				 adaptee(component)
				).asInstanceOf[Selector[this.type, O, S, T]]

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq adaptee) component
			else adaptee.lift(component)

		override def components :Seq[Component[_]] = adaptee.components
		override def subcomponents :Seq[Component[_]] = adaptee.subcomponents
		override def columns :Seq[Component[_]] = adaptee.columns

		override def selectable :Seq[Component[_]] = adaptee.selectable
		override def queryable :Seq[Component[_]] = adaptee.queryable
		override def updatable :Seq[Component[_]] = adaptee.updatable
		override def autoUpdated :Seq[Component[_]] = adaptee.autoUpdated
		override def insertable :Seq[Component[_]] = adaptee.insertable
		override def autoInserted :Seq[Component[_]] = adaptee.autoInserted
		override def selectForm(components :Seq[Component[_]]) :SQLReadForm[S] = adaptee.selectForm(components)

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
				Selector(this, adaptee)(identity[S]).asInstanceOf[Selector[this.type, O, S, T]]
			else {
				val Selector(adaptpick, adaptsurepick, _) = adaptee(dealias(component))
				Selector[this.type, O, S, T](lift(component), adaptpick, adaptsurepick)
			}

		override def lift[T](component :Component[T]) :Component[T] =
			if (component eq adaptee) component
			else adapt(adaptee.lift(component))

		protected def adapt[T](component :Component[T]) :Component[T]

		private[this] def alias[T](component :Component[T]) :Component[T] = adapt(adaptee.lift(component))

		protected def dealias[T](lifted :Component[T]) :Component[T]

		override def components :Seq[Component[_]] = adaptee.components.map(alias(_))
		override def subcomponents :Seq[Component[_]] = adaptee.subcomponents.map(alias(_))

		override def columns :Seq[Component[_]] = adaptee.columns.map(alias(_))
		override def selectable :Seq[Component[_]] = adaptee.selectable.map(alias(_))
		override def queryable :Seq[Component[_]] = adaptee.queryable.map(alias(_))
		override def updatable :Seq[Component[_]] = adaptee.updatable.map(alias(_))
		override def autoUpdated :Seq[Component[_]] = adaptee.autoUpdated.map(alias(_))
		override def insertable :Seq[Component[_]] = adaptee.insertable.map(alias(_))
		override def autoInserted :Seq[Component[_]] = adaptee.autoInserted.map(alias(_))

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



		override def assemble(values :Values) :Option[S] = apply(adaptee).get(values)



		override def canEqual(that :Any) :Boolean = that.isInstanceOf[DeepProxy[_, _]]

		override def toString :String = "->>" + adaptee
	}



	abstract class EagerDeepProxy[M <: SingletonComponent[O, S], O <: AnyMapping, S] private
			(protected val adaptee :M,
			 lifts :mutable.Map[Mapping.AnyComponent[O], ComponentSelector[M, _]],
			 dealias :mutable.Map[Mapping.AnyComponent[O], Mapping.AnyComponent[O]])
		extends DeepProxy[O, S]
	{
		def this(adaptee :M) = this(
			adaptee,
			mutable.Map[Mapping.AnyComponent[O], ComponentSelector[M, _]](),
			mutable.Map[Mapping.AnyComponent[O], Mapping.AnyComponent[O]]()
		)


		override val components :Seq[Component[_]] = adaptee.components.map(lift(_, lifts, dealias))

		override val subcomponents :Seq[Component[_]] = adaptee.subcomponents.map(lift(_, lifts, dealias))
		override val columns :Seq[Component[_]] = adaptee.columns.map(lift(_, lifts, dealias))

		override val selectable :Seq[Component[_]] = adaptee.selectable.map(lift(_, lifts, dealias))
		override val queryable :Seq[Component[_]] = adaptee.queryable.map(lift(_, lifts, dealias))
		override val updatable :Seq[Component[_]] = adaptee.updatable.map(lift(_, lifts, dealias))
		override val autoUpdated :Seq[Component[_]] = adaptee.autoUpdated.map(lift(_, lifts, dealias))
		override val insertable :Seq[Component[_]] = adaptee.insertable.map(lift(_, lifts, dealias))
		override val autoInserted :Seq[Component[_]] = adaptee.autoInserted.map(lift(_, lifts, dealias))

		lifts.put(adaptee, Selector[M, O, S, S](adaptee, identity[S] _))
		dealias.put(adaptee, adaptee)
		private[this] val lifted = lifts.toMap
		private[this] val originals = dealias.toMap

		protected def lift[T](component :Component[T],
		                      lifts :mutable.Map[AnyComponent, ComponentSelector[M, _]],
		                      dealias :mutable.Map[AnyComponent, AnyComponent]) :Component[T] =
			lifts.getOrElse(component, {
				val base = adaptee(component)
				val lifted = adapt(base.lifted)
				val selector = Selector[M, O, S, T](lifted, base.pick, base.surepick)
				lifts.put(component, selector)
				lifts.put(base.lifted, selector)
				dealias.put(lifted, base.lifted)
				lifted
			}).asInstanceOf[Component[T]]

		override def lift[T](component :Component[T]) :Component[T] = apply(component).lifted
//			lifted.getOrElse(component,
//				throw new IllegalArgumentException(s"$component is not a component of $this.")
//			).lifted.asInstanceOf[Component[T]]

		override def apply[T](component :Component[T]) :Selector[this.type, O, S, T] =
			lifted.getOrElse(component,
				throw new IllegalArgumentException(s"$component is not a component of $this.")
			).asInstanceOf[Selector[this.type, O, S, T]]

		protected override def dealias[T](component :Component[T]) :Component[T] =
			originals.getOrElse(component, component).asInstanceOf[Component[T]]


	}



}

