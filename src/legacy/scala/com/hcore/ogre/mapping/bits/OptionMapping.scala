package com.hcore.ogre.mapping.bits

import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.MappingExtension.ExplicitSelect
import com.hcore.ogre.mapping.Mapping.{ReferenceContext, SetParameters}
import com.hcore.ogre.mapping._
import com.hcore.ogre.mapping.support.MappingAdapter.DirectMappingAdapter
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.{SQLWriteForm, SQLForm}


//implicits

import extensions._


trait OptionMapping[T, M<:Mapping[T]] extends Mapping[Option[T]] {
	val get :M with Component[T]
}


object OptionMapping {

	def apply[T](mapping :Mapping[T]) :OptionMapping[T, mapping.type] =
		new DirectOptionMapping[T, mapping.type] {
			val get :mapping.type = mapping
		}

	trait DirectOptionMapping[T, M<:Mapping[T]] extends OptionMapping[T, M] { box =>
		type Component[X] = Mapping[X]

		val get :M

		private[this] lazy val getPath = DirectComponent[this.type, get.type, T](new MappingMorphism[this.type, get.type] {
			def source = box
			def target = get
			val value = ValueMorphism(identity[Option[T]])
			val components = ComponentMorphism.identity[Mapping]
		})

		override lazy val modifiers = ExplicitSelect(None).unless(ExplicitSelect.enabled(get)) ++: get.modifiers.map(_.map(Some(_)))

		override def sqlName = get.sqlName
		override def nullValue = Some(None)
//		override def mockValue = Some(get.mockValue)
		override def scoutValue(ctx :ReferenceContext[this.type]) = Some(get.scoutValue(ctx :\ get))

		def components = Seq(get)
		def subcomponents = InverseIndexSeq(get +: get.subcomponents)

		def columns = get.columns
		def selectable = get.selectable
		def querable = get.querable
		def insertable = get.insertable
		def updatable = get.updatable
		def generated = get.generated

		override def InsertParameters = SetParameters(this)(_.InsertParameters, insertable)
		override def UpdateParameters = SetParameters(this)(_.UpdateParameters, updatable)
		override def QueryParameters = SetParameters(this)(_.QueryParameters, querable)

		override def selectForm = get.selectForm.map(Option(_), None)
		override def queryForm = form(get.queryForm)
		override def updateForm = form(get.updateForm)
		override def insertForm = form(get.insertForm)

		private def form(form :SQLWriteForm[T]) = form.iflatMap(identity[Option[T]] _)

		override def assemble(values: Values): Option[Option[T]] = Some(values.get(get))

		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
			if (component eq get)
				getPath.asInstanceOf[TypedComponentPath[this.type, component.type, X]]
			else
				(getPath :\ component.asInstanceOf[get.Component[X]]).asInstanceOf[TypedComponentPath[this.type, component.type, X]]

	}
}
