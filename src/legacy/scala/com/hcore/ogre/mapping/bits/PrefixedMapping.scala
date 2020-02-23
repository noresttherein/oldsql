package com.hcore.ogre.mapping.bits

import com.hcore.ogre.mapping.MappingMorphism.ComponentMorphism
import com.hcore.ogre.mapping.Mapping
import com.hcore.ogre.mapping.support.MappingAdapter
import com.hcore.ogre.mapping.support.MappingAdapter.FullMappingProxy


class PrefixedMapping[E, M<:Mapping[E]](val adaptee :M, val prefix :String)
	extends FullMappingProxy[E, M] //with MappingDecorator[E, M]
{ parent =>
	type Component[X] = ComponentProxy[X]


	override protected def adapt[X](component: AdaptedComponent[X]) :Component[X] =
//		if (component.components.isEmpty)
//			new ComponentImpostor[X](component) {
//				val adaptee = component
//				override val sqlName = adaptee.sqlName.map(prefix + _)
//			}
//		else
			new PrefixedMapping[X, AdaptedComponent[X]](component, prefix) with FullComponentProxy[X] {
				override def adaptedComponent = adaptee
			}

	override val sqlName = adaptee.sqlName.map(prefix + _)



	override protected def contentsEqual(that: MappingAdapter[_, _]): Boolean = that match {
		case p:PrefixedMapping[_, _] => p.prefix==prefix
		case _ => false
	}

	override def hashCode = (getClass, adaptee, prefix).hashCode


	override def toString = s"'$prefix'+$adaptee"

}


class QualifiedMapping[E](adaptedMapping :Mapping[E], prefix :String)
	extends PrefixedMapping[E, Mapping[E]](adaptedMapping, Mapping.prefixOption(prefix).map(_+".") getOrElse "")
