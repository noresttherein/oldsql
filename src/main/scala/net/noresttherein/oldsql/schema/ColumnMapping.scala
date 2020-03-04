package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.ComponentPath.{DirectComponent, TypedComponentPath}
import net.noresttherein.oldsql.schema.Mapping.{prefixOption, Buff, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.Buff.BuffType

import scala.util.Try


trait ColumnMapping[T, O<:AnyMapping] extends SubMapping[T, O] { column =>
	type Component[X] <: ColumnMapping[X, O]

	def self :Component[T]

	override def sqlName = Some(name)

	def name :String

	final def components :Seq[Nothing] = Seq()
	final def subcomponents :Seq[Component[T]] = Seq()

	final def columns :Seq[Component[T]] = Seq(self)
	final def queryable :Seq[Component[T]] = Seq(self)
	final def selectable :Seq[Component[T]] = Seq(self)
	final def updatable :Seq[Component[T]] = Seq(self)
	final def insertable :Seq[Component[T]] = Seq(self)
	final def generated :Seq[Component[T]] = Seq(self)
//
//	final def queryable :Seq[Component[T]] = selfUnless(NoQuery)
//	final def selectable :Seq[Component[T]] = selfUnless(NoSelect)
//	final def updatable :Seq[Component[T]] = selfUnless(NoUpdate)
//	final def insertable :Seq[Component[T]] = selfUnless(NoInsert)
//	final def generated :Seq[Component[T]] = self.providing(AutoGen.enabled(modifiers)).toSeq

	protected def selfUnless(buff :BuffType) :Seq[Component[T]] =
		if (buff.enabled(buffs)) Seq() else Seq(self)

	def form :SQLForm[T]
	override def selectForm :SQLForm[T] = form
	override def queryForm :SQLForm[T] = form
	override def updateForm :SQLForm[T] = form
	override def insertForm :SQLForm[T] = form



	override def assemble(values: Values): Option[T] = None


/*
	protected object morphism extends ValueIdentityMorphism[this.type, Component[T], T] {
		def source = column
		def target = self
		override def components =
			ComponentMorphism.homomorphism[Component[T]#Component, Component](_ => self)
	}
	protected val path = ComponentPath.DirectComponent[this.type, Component[T], T](morphism)
*/

	override implicit def \\[X](component: Component[X]) :TypedComponentPath[this.type, component.type, X] = ???
//		if (component == self)
//			path.asInstanceOf[TypedComponentPath[this.type, component.type, X]]
//		else
//			throw new IllegalArgumentException(s"Can't lift unexpected component $component of column $this. The only component should be $self.")
	//		DirectComponent(this :this.type)(component)(ValueMorphism.identity[T].asInstanceOf[ValueMorphism[T, X]], ComponentMorphism.homomorphism(_ => self))


	def withOptions(opts :Seq[Buff[T]]) :ColumnMapping[T, O] = ColumnMapping(name, opts:_*)(form)

	def renamed(name :String) :ColumnMapping[T, O] = ColumnMapping(name, buffs:_*)(form)

	override def prefixed(prefix :String) :ColumnMapping[T, O] = ColumnMapping(prefix+name, buffs:_*)(form)

	def prefixed(prefix :Option[String]) :ColumnMapping[T, O] = prefix.map(prefixed) getOrElse this

	override def qualified(prefix :String) :ColumnMapping[T, O] =
		prefixOption(prefix).map(p => prefixed(p + ".")) getOrElse this




	override def toString :String = name //s"$name[$columnType]"

	override def introString :String = name
}



object ColumnMapping {

	def apply[T :SQLForm, O <: AnyMapping](name :String, opts :Buff[T]*) :ColumnMapping[T, O] =
		new BaseColumn(name, opts)

	def adapt[T, O <: AnyMapping](mapping :TypedMapping[T]) :Option[ColumnMapping[T, O]] = mapping match {
		case c :ColumnMapping[_, _] => Some(c.asInstanceOf[ColumnMapping[T, O]])
		case _ => Try(new ColumnView[T, O](mapping.asComponent)).toOption
	}



	def apply[T, O <: AnyMapping](mapping :Mapping[T]) :ColumnMapping[T, O] = new ColumnView[T, O](mapping)


	//	def unapply[T](mapping :Mapping[T]) :Option[ColumnMapping[T]] = mapping.asSubclass[ColumnMapping[T]]




	class BaseColumn[T, O <: AnyMapping](val name :String, override val buffs :Seq[Buff[T]])(implicit val form :SQLForm[T])
		extends ColumnMapping[T, O]
	{

		type Component[X] = ColumnMapping[X, O]

		def self: Component[T] = this

	}

	trait ColumnSubstitute[T, S, O <: AnyMapping] extends ColumnMapping[T, O] {
		override type Component[X] = ColumnMapping[X, O]

		override def self: Component[T] = this

		val adaptee :ColumnMapping[T, O]

		override def name: String = adaptee.name

	}

	trait ColumnImpostor[T, O <: AnyMapping] extends ColumnSubstitute[T, T, O] {
		override def form: SQLForm[T] = adaptee.form

		override def buffs :Seq[Buff[T]] = adaptee.buffs

		override def assemble(values: Values): Option[T] =
			(values :\ adaptee).result(adaptee)
	}

	class ColumnOverride[T, O <: AnyMapping](val adaptee :ColumnMapping[T, O], nameOverride :Option[String]=None, buffsOverride :Option[Seq[Buff[T]]]=None)
		extends ColumnImpostor[T, O]
	{
		override val buffs :Seq[Buff[T]] = buffsOverride getOrElse adaptee.buffs
		override val name :String = nameOverride getOrElse adaptee.name
	}




	class ColumnView[T, O <: AnyMapping](val adaptee :Mapping[T]) extends ColumnMapping[T, O] {
		type Component[X] = ColumnMapping[X, O]

		if (adaptee.columns.size!=1 || adaptee.selectForm.readColumns!=1 || adaptee.insertForm.writtenColumns!=1 || adaptee.updateForm.writtenColumns!=1)
			throw new IllegalArgumentException(s"Expected a column, got multiple column mapping :$adaptee{${adaptee.columns}}")

		override val name :String = adaptee.sqlName getOrElse {
			throw new IllegalArgumentException(s"Expected a column, got mapping without sqlName :$adaptee{${adaptee.columns}}")
		}


		override val form = SQLForm.combine(adaptee.selectForm, adaptee.insertForm)

		override def self: Component[T] = this

	}


}

