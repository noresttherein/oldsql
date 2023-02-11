package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.ComponentPath.{DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.support.MappingAdapter.{MappingProxy, MappingImpostor}
import com.hcore.ogre.mapping.{ComponentPath, AnyMapping, MappingMorphism, Mapping}


trait ComponentMapping[E, P<:Mapping[E], T] extends Mapping[T] {
	def path :TypedComponentPath[P, this.type, T]
	def pick :E=>Option[T] = path.pick
}


object ComponentMapping {

//	class LiftedComponent[, C<:Mapping[V]](val adaptee :C) extends MappingImpostor[V, C] with ComponentMappin



	trait ComponentAdapter[E, P<:Mapping[E], T, C<:Mapping[T]] extends ComponentMapping[E, P, T] with MappingProxy[T, C] {
		override def canEqual(that :Any) = that.isInstanceOf[ComponentAdapter[_, _, _, _]]
	}

	trait ComponentProxy[E, P<:Mapping[E], T, C<:Mapping[T]] extends ComponentMapping[E, P, T] with MappingProxy[T, C] {
		override def canEqual(that :Any) = that.isInstanceOf[ComponentProxy[_, _, _, _]]

		override protected def contentsEqual(that: MappingAdapter[_, _]): Boolean = that match {
			case d:ComponentProxy[_, _, _, _] => d.path==path
			case _ => false
		}
	}



	def box[E, P<:Mapping[E] { type Component[X] >: ComponentProxy[E, P, X, _] <: Mapping[X] }, T, C<:Mapping[T]](
			parent :P, mapping :C, value :ValueMorphism[E, T], structure :ComponentMorphism[C#Component, P#Component]) :ComponentProxy[E, P, T, C] =
		new ComponentProxy[E, P, T, C] with MappingImpostor[T, C] { component =>
			override val adaptee = mapping

			val path = DirectComponent[P, component.type, T](MappingMorphism[P, component.type](
				parent, component, value, structure.asInstanceOf[ComponentMorphism[component.Component, P#Component]]
			))
		}



	@inline
	def box[E, P<:Mapping[E] { type Component[X] >: ComponentProxy[E, P, X, _] <: Mapping[X] }, T, C<:Mapping[T]](morphism :MappingMorphism[P, C]) :ComponentProxy[P#ResultType, P, C#ResultType, C] =
		box[E, P, T, C](morphism.source, morphism.target, morphism.value, morphism.components)
}
