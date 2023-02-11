package com.hcore.ogre.morsels

import com.hcore.ogre.morsels.TypeDef.{Invariant, Contravariant, Covariant}
import com.hcore.ogre.sql.SQLForm.NothingForm$


class TypeDef[X] extends Covariant[X] with Contravariant[X] with Invariant[X]


/** A workaround over the fact that scala doesn't allow to override type definitions.
  * Using these type holders you can define for example a member field
  * <code>val element :Invariant[T<:UpperBound] = Invariant[UpperBound]</code> in a super class, and override it with
  * <code>override val element = Invariant[LowerType]</code> in subclass. Now in both classes you can use
  * element.Type to designate the type used by this instance.
  */
object TypeDef {
	def apply[X] :TypeDef[X] = instance.asInstanceOf[TypeDef[X]]

	private val instance = new TypeDef[Nothing]


	trait Invariant[X] {
		type Type = X
	}

	object Invariant {
		def apply[X] = instance.asInstanceOf[Invariant[X]]
		
		private[this] val instance = new Invariant[Any]{}
	}



	trait Covariant[+X] {
		type Type<:X
	}


	object Covariant {
		def apply[X] :Covariant[X] = NothingType

		val NothingType = new Covariant[Nothing]{}
	}


	trait Contravariant[-X] {
		type Type >: X
	}

	object Contravariant {
		def apply[X] :Contravariant[X] = AnyType

		val AnyType = new Contravariant[Any] {}
	}

}
