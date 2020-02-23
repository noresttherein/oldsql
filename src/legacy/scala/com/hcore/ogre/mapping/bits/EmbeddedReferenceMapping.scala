package com.hcore.ogre.mapping.bits


import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.{ReferenceContext, SetParameters}
import com.hcore.ogre.mapping.{MappingMorphism, Mapping}
import com.hcore.ogre.model.{NavigableReference, Reference}
import com.hcore.ogre.model.Reference.GenericReferenceFactory
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.sql.{SQLWriteForm, SQLForm}


//implicits


trait EmbeddedReferenceMapping[T, M<:Mapping[T], R<:Reference[T]] extends Mapping[R] {
	val value :M with Component[T]
}


object EmbeddedReferenceMapping {

	def apply[K, T, R >: Null <:Reference[T]](value :Mapping[T], key :Mapping[K], reference :GenericReferenceFactory[K, T, T, R]) :EmbeddedReferenceMapping[T, value.type, R] =
		new ShallowEmbeddedReferenceMapping[K, T, value.type, R](value, key, reference)


	class ShallowEmbeddedReferenceMapping[K, T, M<:Mapping[T], R >: Null <:Reference[T] ](valueMapping :M, keyMapping :Mapping[K], factory :GenericReferenceFactory[K, T, T, R])
		extends EmbeddedReferenceMapping[T, M, R]
	{ box =>
		type Component[X] = Mapping[X]

		val value = valueMapping
		val key :Mapping[K] = ImportedMapping(keyMapping)

		private[this] val valuePath = DirectComponent[this.type, value.type, T](new MappingMorphism[this.type, value.type] {
			def source = box
			def target = box.value
			val value = ValueMorphism((_:R).toOpt)
			val components = ComponentMorphism.identity[Mapping]
		})

		private[this] val keyPath = DirectComponent[this.type, key.type, K](new MappingMorphism[this.type, key.type] {
			override def source = box
			override def target = key
			override val value = ValueMorphism(factory.keyOf)
			override val components = ComponentMorphism.empty[Component]
		})

		override val modifiers = value.modifiers.map(_.map(factory.full))

		override def sqlName = value.sqlName
		override def nullValue = Some(null)
//		override def mockValue = NavigableReference.from(factory, value.mockValue) orElse
//			value.mockValue.map(factory.full) orElse key.mockValue.map(factory.empty)

		override def scoutValue(ctx: ReferenceContext[this.type]): Option[R] =
			NavigableReference.from(factory, value.scoutValue(ctx :\ value)) orElse
				value.scoutValue(ctx :\ value).map(factory.full) orElse key.scoutValue.map(factory.empty)

		def components = Seq(value)
		def subcomponents = InverseIndexSeq(value +: value.subcomponents)

		def columns = value.columns
		def selectable = value.selectable
		def querable = value.querable
		def insertable = value.insertable
		def updatable = value.updatable
		def generated = value.generated

		override def InsertParameters = SetParameters(this)(_.InsertParameters, insertable)
		override def UpdateParameters = SetParameters(this)(_.UpdateParameters, updatable)
		override def QueryParameters = SetParameters(this)(_.QueryParameters, querable)

		override def selectForm = value.selectForm.map(factory.full)
		override def queryForm = form(value.queryForm)
		override def updateForm = form(value.updateForm)
		override def insertForm = form(value.insertForm)
		private def form(form :SQLWriteForm[T]) = form.iflatMap[R](_.toOpt)


		override def assemble(values: Values): Option[R] =
			values.get(value).map(factory.full) orElse values.get(key).map(factory.empty)


		override implicit def \\[X](component: Component[X]): TypedComponentPath[this.type, component.type, X] =
			if (component eq value)
				valuePath.asInstanceOf[TypedComponentPath[this.type, component.type, X]]
			else if (component eq key)
				keyPath.asInstanceOf[TypedComponentPath[this.type, component.type, X]]
			else
				(valuePath :\ component.asInstanceOf[value.Component[X]]).asInstanceOf[TypedComponentPath[this.type, component.type, X]]

	}
}
