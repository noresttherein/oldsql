package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.{AnyMapping, Mapping, SQLReadForm, SubMapping}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping


//class MappedMapping[T, S, M<:Mapping[S]](val adaptee :M, val toResultType: S=>T, val fromResultType: T=>S)
//	extends DedicatedMappedMapping[T, S, M]



object MappedMapping {
	def apply[M<:TypedMapping[S], S, T](mapping :M, mapped :S=>T, unmapped :T=>S) :M MappedAs T = ???
//		new GenericMappedMapping[T, S, M] {
//			override protected val toResultType = mapped
//			override protected val fromResultType = unmapped
//			override val adaptee: M = mapping
//		}


	trait MappedAs[M<:AnyMapping, T] extends SubMapping[M#Owner, T] {

		val adaptee :M
//		type Component[X] = M#Component[X] //>: M#Component[X] <: SubMapping[X, Owner]

//		def reverseMorphism :MappingMorphism[M, this.type]

		def apply[C<:M#AnyComponent](component :M=>C) :C
	}

/*
	trait DedicatedMappedMapping[V, T, M<:Mapping[T]]
		extends MappingSubstitute[V, T, M]
	{
		override def buffs = adaptee.modifiers.map(_.map(toResultType))

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

		override def hashCode :Int = (adaptee, toResultType, fromResultType).hashCode

		override def toString = s"Mapped($adaptee)"

	}
*/
}

