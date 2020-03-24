package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.Mapping
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, Component, TypedMapping}
import net.noresttherein.oldsql.schema.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy


/** A mapping serving as a 'symbolic link' of a kind to another mapping;
  * it has exactly the same components as the target mapping and will delegate all operations to it.
  * It marks itself however by adding a SymLink modifier to the list of modifiers of the target. This way,
  * assuming the modifiers will be preserved by any mapping/morphing/lifting operations applied to it
  * (as they are by default), it can be recognised as a link to the target component and the target can
  * be retrieved. It can be passed as an argument to a mapping's component for reference, which would normally
  * either require the component to 'suppress' (not lift/hide the columns) the argument component from its component list,
  * or result in duplicate entries in mapping X component list - one for the target, and possibly one resulting from lifting
  * the reference passed to the other subcomponent. It remains mapping's responsibility - if it creates sym links -
  * to recognise the duplicate originating from use of this component, remove it from its subcomponents list and
  * return a SymLinkComponentPath when asked for a path to the lifted representation of this component.
  */
/*
trait SymLinkMapping[X <: AnyComponent[O], Y <: Component[O, S], O, S] extends MappingNest[Y]  {
//	type Component[V] = SymLinkMapping[X, _<:Mapping[V], V] with ComponentProxy[V]

	def target :ComponentPath[X, Y, O]

	override def buffs = SymLink(target) +: egg.buffs

	override def canEqual(that :Any) :Boolean = that.isInstanceOf[SymLinkMapping[_, _, _, _]]

	override def equals(that :Any) :Boolean = that match {
		case self :AnyRef if self eq this => true
		case symlink :SymLinkMapping[_, _, _, _] if symlink canEqual this =>
			egg == symlink.egg && target == symlink.target
		case _ => false
	}

	override def hashCode :Int = egg.hashCode * 31 + target.hashCode

	override def toString :String = "@" + target
}
*/






object SymLinkMapping {

/*
	def apply[X <: AnyComponent[O], Y <: Component[O, S], O, S](target :ComponentPath[X, Y, O]) :SymLinkMapping[X, Y, O, S] =
		new BaseSymLink(target)

	def path[X <: Mapping, Y <: X#Component[V], V](target :ComponentPath[X, _<:Mapping, O]) :ComponentPath[X, Y, O] =
		new SymLinkComponentPath(target, default)


	class BaseSymLink[X <: AnyComponent[O], Y <: Component[O, T], O, T](val target :ComponentPath[X, Y, O])
		extends SymLinkMapping[X, Y, O]
	{
//		def this(source :X, target :Y) =
//			this((source \\ target.asInstanceOf[source.Component[T]]).asInstanceOf[TypedComponentPath[P, M, T]])

		protected override val egg = target.end

		override protected def adapt[X](component: AdaptedComponent[X]): Component[X] =
			new BaseSymLink[P, AdaptedComponent[X], X]((target :+ component).asInstanceOf[TypedComponentPath[P, AdaptedComponent[X], X]]) with FullComponentProxy[X] {
				override def adaptedComponent = egg
			}
	}
*/

	/** A substitute path to a component which is a sym link for another mapping and not present in ComponentValues.
	  * All instances produced by values adapted by this path will first check if the argument mapping is not the target of the given path,
	  * and if so, return values that would be returned for that mapping by ComponentValues[X].
	  */

/*
	case class SymLinkComponentPath[X<:Mapping, Z<:Mapping, Y<:X#Component[V], V](target :ComponentPath[X, Z], morphism :MappingMorphism[X, Y])
		extends DirectComponent[X, Y, V] with MorphismPath[X, Y]
	{ link =>

		override def walk(values: ComponentValues[X]): ComponentValues[Y] =
			super.walk(values.stick(target))


		override def tailString = end.toString + "=>" + target.tailString
	}
*/

}

