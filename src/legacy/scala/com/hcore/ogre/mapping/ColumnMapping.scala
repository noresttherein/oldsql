package com.hcore.ogre.mapping


import com.hcore.ogre.mapping.ComponentPath.{SelfPath, DirectComponent, TypedComponentPath, MappedComponent}
import com.hcore.ogre.mapping.MappingMorphism.{ValueIdentityMorphism, ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.MappingExtension._
import com.hcore.ogre.mapping.Mapping._
import com.hcore.ogre.mapping.support.SymLinkMapping
import com.hcore.ogre.slang.SaferCasts
import com.hcore.ogre.slang.options.extensions
import extensions._
import SaferCasts._
import com.hcore.ogre.sql.SQLForm

import scala.slick.jdbc.{GetResult, PositionedParameters, PositionedResult, SetParameter}
import scala.util.Try


trait ColumnMapping[T] extends Mapping[T] with SetParameter[T] { column =>
	type Component[X] <: ColumnMapping[X]

	def self :Component[T]
//	final def self = SelfPath(this)

	override def sqlName = Some(name)

	def name :String

//	override def columnDefinition = Some(ColumnDefinition(name)(columnType.sqlForm))

//	override def mockValue = Option(columnType.nullValue)

	override def scoutValue(ctx: ReferenceContext[this.type]): Option[T] = Option(columnType.nullValue)

	override def nullValue :Some[T] = Some(columnType.nullValue)

	def columnType :ColumnType[T]
	
	
	final def components :Seq[Nothing] = Seq()
	final def subcomponents :Seq[Component[T]] = Seq() //Seq(self)

	final def columns :Seq[Component[T]] = Seq(self)
	final def querable :Seq[Component[T]] = Seq(self)
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

	protected def selfUnless(opt :MappingExtensionType) =
		if (opt.enabled(modifiers)) Seq() else Seq(self)

	override def selectForm = columnType.sqlForm
	override def queryForm = columnType.sqlForm
	override def updateForm = columnType.sqlForm
	override def insertForm = columnType.sqlForm


	override def InsertParameters: SetParameter[T] = this
	override def UpdateParameters: SetParameter[T] = this
	override def QueryParameters: SetParameter[T] = this


	def apply(v1: T, v2: PositionedParameters): Unit = columnType.Setter(v1, v2)


	final override def apply(res :PositionedResult) :T =
		columnType(res)

	override def assemble(values: Values): Option[T] = None


	protected object morphism extends ValueIdentityMorphism[this.type, Component[T], T] {
		def source = column
		def target = self
		override def components =
			ComponentMorphism.homomorphism[Component[T]#Component, Component](_ => self)
	}
	protected val path = DirectComponent[this.type, Component[T], T](morphism)

	override implicit def \\[X](component: Component[X]) :TypedComponentPath[this.type, component.type, X] =
		if (component==self)
			path.asInstanceOf[TypedComponentPath[this.type, component.type, X]]
		else
			throw new IllegalArgumentException(s"Can't lift unexpected component $component of column $this. The only component should be $self.")
//		DirectComponent(this :this.type)(component)(ValueMorphism.identity[T].asInstanceOf[ValueMorphism[T, X]], ComponentMorphism.homomorphism(_ => self))


	def withOptions(opts :Seq[MappingExtension[T]]) :ColumnMapping[T] = ColumnMapping(name, opts:_*)(columnType)

	def renamed(name :String) :ColumnMapping[T] = ColumnMapping(name, modifiers:_*)(columnType)

	override def prefixed(prefix :String) :ColumnMapping[T] = ColumnMapping(prefix+name, modifiers:_*)(columnType)

	def prefixed(prefix :Option[String]) :ColumnMapping[T] = prefix.map(prefixed) getOrElse this

	override def qualified(prefix :String) :ColumnMapping[T] = prefixOption(prefix).map(p => prefixed(p + ".")) getOrElse this




	override def toString = name //s"$name[$columnType]"

	override def introString = name
}



object ColumnMapping {

	def apply[T :ColumnType](name :String, opts :MappingExtension[T]*) :ColumnMapping[T] =
		new BaseColumn(name, opts)

	def adapt[T](mapping :TypedMapping[T]) :Option[ColumnMapping[T]] = mapping match {
		case c:ColumnMapping[_] => Some(c.asInstanceOf[ColumnMapping[T]])
		case _ => Try(new ColumnView[T](mapping.asMapping)).toOption
	}



	def apply[T](mapping :Mapping[T]) :ColumnMapping[T] = new ColumnView[T](mapping)


//	def unapply[T](mapping :Mapping[T]) :Option[ColumnMapping[T]] = mapping.asSubclass[ColumnMapping[T]]




	class BaseColumn[T](val name :String, override val modifiers :Seq[MappingExtension[T]])(implicit val columnType :ColumnType[T])
		extends ColumnMapping[T]
	{
		type Component[X] = ColumnMapping[X]

		def self: Component[T] = this

//		override val selectable = super.selectable
//		override val queryable = super.queryable
//		override val updatable = super.updatable
//		override val insertable = super.insertable
//		override val generated = super.generated
//		val InsertParameters = this.unless(enabled(NoInsert)) getOrElse SetParameters.Empty
//		val UpdateParameters = this.unless(enabled(NoUpdate)) getOrElse SetParameters.Empty
//		val QueryParameters = this.unless(enabled(NoQuery)) getOrElse SetParameters.Empty
//		def InsertParameters = this
//		def UpdateParameters = this
//		def QueryParameters = this
//

	}

	trait ColumnSubstitute[T, S] extends ColumnMapping[T] {
		override type Component[X] = ColumnMapping[X]

		override def self: Component[T] = this

		val adaptee :ColumnMapping[T]

		override def name: String = adaptee.name

//		def InsertParameters = this.unless(enabled(NoInsert)) getOrElse SetParameters.Empty
//		def UpdateParameters = this.unless(enabled(NoUpdate)) getOrElse SetParameters.Empty
//		def QueryParameters = this.unless(enabled(NoQuery)) getOrElse SetParameters.Empty
//

	}

	trait ColumnImpostor[T] extends ColumnSubstitute[T, T] {
		override def columnType: ColumnType[T] = adaptee.columnType

		override def modifiers = adaptee.modifiers

		override def scoutValue(ctx: ReferenceContext[this.type]): Option[T] =
			adaptee.scoutValue(ctx :\ adaptee)

		override def assemble(values: Values): Option[T] =
			(values :\ adaptee).result(adaptee)
	}

	class ColumnOverride[T](val adaptee :ColumnMapping[T], nameOverride :Option[String]=None, modifiersOverride :Option[Seq[MappingExtension[T]]]=None)
		extends ColumnImpostor[T]
	{
		override val modifiers = modifiersOverride getOrElse adaptee.modifiers
		override val name = nameOverride getOrElse adaptee.name
	}




	class ColumnView[T](val adaptee :Mapping[T]) extends ColumnMapping[T] {
		type Component[X] = ColumnMapping[X]

		if (adaptee.columns.size!=1 || adaptee.selectForm.readColumns!=1 || adaptee.insertForm.writtenColumns!=1 || adaptee.updateForm.writtenColumns!=1)
			throw new IllegalArgumentException(s"Expected column, got multiple column mapping :$adaptee{${adaptee.columns}}")

		override val name = adaptee.sqlName getOrElse {
			throw new IllegalArgumentException(s"Expected column, got mapping without sqlName :$adaptee{${adaptee.columns}}")
		}

//		override def columnDefinition = Some(adaptee.columnDefinition getOrElse {
//			throw new IllegalArgumentException(s"Expected column, got mapping without column definition :${adaptee.schemaString}")
//		})

		override val columnType = new ColumnType[T] {
			override def sqlForm: SQLForm[T] = SQLForm.combine(adaptee.selectForm, adaptee.insertForm)
			override def Getter: GetResult[T] = GetResult[T](sqlForm(_))
			override def Setter: SetParameter[T] = SetParameter[T]((t, params) => sqlForm(params, t))
			override def nullValue: T = sqlForm.nullValue
		}

		override def self: Component[T] = this

		override def UpdateParameters: SetParameter[T] = adaptee.UpdateParameters
		override def InsertParameters: SetParameter[T] = adaptee.InsertParameters
		override def QueryParameters: SetParameter[T] = adaptee.QueryParameters
	}


}

