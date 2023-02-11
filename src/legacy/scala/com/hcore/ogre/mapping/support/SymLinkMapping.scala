package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.ComponentPath.{MorphismPath, DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.ComponentValues.ComponentValuesProxy
import com.hcore.ogre.mapping.Mapping.MappingExtension.SymLink
import com.hcore.ogre.mapping.MappingPath.{TypedDirectPath, TypedMappingLink}
import com.hcore.ogre.mapping.support.MappingAdapter.{FullMappingProxy, MappingImpostor}
import com.hcore.ogre.mapping._


/** A mapping serving as a 'symbolic link' of a kind to another mapping;
  * it has exactly the same components as the target mapping and will delegate all operations to it.
  * It marks itself however by adding a SymLink modifier to the list of modifiers of the target. This way,
  * assuming the modifiers will be preserved by any mapping/morphing/lifting operations applied to it
  * (as they are by default), it can be recognised as a link to the target component and the target can
  * be retrieved. It can be passed as an argument to a mapping's component for reference, which would normally
  * either require the component to 'supress' (not lift/hide the columns) the argument component from its component list,
  * or result in duplicate entries in mapping X component list - one for the target, and possibly one resulting from lifting
  * the reference passed to the other subcomponent. It remains mapping's responsibility - if it creates sym links -
  * to recognise the duplicate originating from use of this component, remove it from its subcomponents list and
  * return a SymLinkComponentPath when asked for a path to the lifted representation of this component.
  */
trait SymLinkMapping[X<:AnyMapping, M<:Mapping[T], T] extends Mapping[T] with FullMappingProxy[T, M] {
	type Component[V] = SymLinkMapping[X, _<:Mapping[V], V] with ComponentProxy[V]

	def target :TypedComponentPath[X, M, T]

	override def modifiers = SymLink(target) +: adaptee.modifiers

	override protected def contentsEqual(that: MappingAdapter[_, _]): Boolean = that match {
		case l:SymLinkMapping[_,_,_] => l.target==target
		case _ => false
	}

	override def hashCode = (adaptee, target).hashCode

	override def toString = "=>"+target
}



object SymLinkMapping {

	def apply[X<:AnyMapping, M<:Mapping[T], T](target :TypedComponentPath[X, M, T]) :SymLinkMapping[X, M, T] =
		new BaseSymLink(target)

	def path[X<:AnyMapping, Y<:X#Component[V], V](target :ComponentPath[X, _<:AnyMapping], default :MappingMorphism[X, Y]) :TypedComponentPath[X, Y, V] =
		new SymLinkComponentPath(target, default)


	class BaseSymLink[P<:AnyMapping, M<:Mapping[T], T](val target :TypedComponentPath[P, M, T]) extends SymLinkMapping[P, M, T] {
		def this(source :P, target :M with P#Component[T]) =
			this((source \\ target.asInstanceOf[source.Component[T]]).asInstanceOf[TypedComponentPath[P, M, T]])

		protected val adaptee = target.end

		override protected def adapt[X](component: AdaptedComponent[X]): Component[X] =
			new BaseSymLink[P, AdaptedComponent[X], X]((target :+ component).asInstanceOf[TypedComponentPath[P, AdaptedComponent[X], X]]) with FullComponentProxy[X] {
				override def adaptedComponent = adaptee
			}
	}

	/** A substitute path to a component which is a sym link for another mapping and not present in ComponentValues.
	  * All instances produced by values adapted by this path will first check if the argument mapping is not the target of the given path,
	  * and if so, return values that would be returned for that mapping by ComponentValues[X].
	  */
	case class SymLinkComponentPath[X<:AnyMapping, Z<:AnyMapping, Y<:X#Component[V], V](target :ComponentPath[X, Z], morphism :MappingMorphism[X, Y])
		extends DirectComponent[X, Y, V] with MorphismPath[X, Y]
	{ link =>

		override def walk(values: ComponentValues[X]): ComponentValues[Y] =
			super.walk(values.stick(target))


		override def tailString = end.toString + "=>" + target.tailString
	}

}
