package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.Mapping.TypedMapping
import com.hcore.ogre.mapping.MappingMorphism.{StructuralIdentityMorphism, ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.{MappingMorphism, AnyMapping, Mapping}
import com.hcore.ogre.mapping.support.MappedMapping.DedicatedMappedMapping
import com.hcore.ogre.mapping.support.MappingAdapter.{GenericMappingSubstitute, MappingSubstitute, MappingMorphismAdapter}
import com.hcore.ogre.sql.SQLForm.MappedSQLForm
import com.hcore.ogre.sql.SQLReadForm


class MappedMapping[T, S, M<:Mapping[S]](val adaptee :M, val toResultType: S=>T, val fromResultType: T=>S)
	extends DedicatedMappedMapping[T, S, M]



object MappedMapping {
	def apply[T, S, M<:TypedMapping[S]](mapping :M, mapped :S=>T, unmapped :T=>S) :M MappedAs T =
		new GenericMappedMapping[T, S, M] {
			override protected val toResultType = mapped
			override protected val fromResultType = unmapped
			override val adaptee: M = mapping
		}


	trait MappedAs[M<:AnyMapping, T] extends Mapping[T] {
		val adaptee :M
		type Component[X] >: M#Component[X] <:Mapping[X]

		def reverseMorphism :MappingMorphism[M, this.type]

		def apply[C<:M#AnyComponent](component :M=>C) :C
	}
	
	trait DedicatedMappedMapping[V, T, M<:Mapping[T]]
		extends MappingSubstitute[V, T, M]
	{
//		override def apply[C <: M#AnyComponent](component: M => C): C =
//			component(adaptee)

		override def modifiers = adaptee.modifiers.map(_.map(toResultType))

//		override def columnDefinition = adaptee.columnDefinition.map { coldef =>
//			ColumnDefinition[V](coldef.name)(
//				MappedSQLForm(
//					(t:T) => Some(toResultType(t)),
//					(v :V)=>Some(fromResultType(v)),
//					toResultType(coldef.form.nullValue)
//			)(coldef.form))
//		}

		protected val toResultType :T=>V
		protected val fromResultType :V=>T

		protected def morphism = new SubstituteMorphism {
			override def value = ValueMorphism.homomorphism(fromResultType)
		}

		override def assemble(values: Values): Option[V] =
			values.morph(morphism).result(adaptee).map(toResultType)


		override def selectForm: SQLReadForm[V] = adaptee.selectForm.map(toResultType)

		override protected def contentsEqual(that: MappingAdapter[_, _]): Boolean = that match {
			case map :DedicatedMappedMapping[_, _, _] =>
				map.toResultType == toResultType &&
					map.fromResultType == fromResultType
			case _ => false
		}

		override def hashCode = (adaptee, toResultType, fromResultType).hashCode

		override def toString = s"Mapped($adaptee)"

	}



	trait GenericMappedMapping[V, T, M<:TypedMapping[T]]
		extends GenericMappingSubstitute[V, M] with MappedAs[M, V]
	{ substitute =>
		override def apply[C <: M#AnyComponent](component: M => C): C =
			component(adaptee)

		override def modifiers = adaptee.modifiers.map(_.map(toResultType))

		protected val toResultType :T=>V
		protected val fromResultType :V=>T

		protected val morphism = new SubstituteMorphism {
			override def value = ValueMorphism.homomorphism(fromResultType)
		}


		val reverseMorphism = new MappingMorphism[M, this.type] {
			def source = adaptee
			def target = substitute
			def value = ValueMorphism.homomorphism(toResultType)
			def components = ComponentMorphism.identity[M#Component]
		}


		override def assemble(values: Values): Option[V] =
			values.morph(morphism).result(adaptee).map(toResultType)


		override def selectForm: SQLReadForm[V] = adaptee.selectForm.map(toResultType)

		override protected def contentsEqual(that: MappingAdapter[_, _]): Boolean = that match {
			case map :GenericMappedMapping[_, _, _] =>
				map.toResultType == toResultType &&
					map.fromResultType == fromResultType
			case _ => false
		}

		override def hashCode = (adaptee, toResultType, fromResultType).hashCode

		override def toString = s"Mapped($adaptee)"

	}

}
