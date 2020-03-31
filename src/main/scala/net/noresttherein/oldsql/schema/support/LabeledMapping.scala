package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.schema.{GenericMapping, Mapping}
import net.noresttherein.oldsql.schema.support.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.Component
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted



trait LabeledMapping[N <: Label, O, S] extends GenericMapping[O, S]





object LabeledMapping {
	type Label = String with Singleton
	type @:[N <: Label, M <: Component[_, _]] = MappingLabel[N, M]


	def apply[N <: Label, M <: Component[_, _]](label :N, mapping :M) :N @: M =
		new MappingLabel[N, M](mapping)(new ValueOf[N](label))

	class MappingLabel[N <: Label :ValueOf, M <: Component[_, _]](val egg :M)
		extends ShallowProxy[M#Owner, M#Subject] with Adapted[M] with LabeledMapping[N, M#Owner, M#Subject]
	{
		def label :N = valueOf[N]

		override def equals(that :Any) :Boolean = that match {
			case other :MappingLabel[_, _] =>
				(other eq this) || (other canEqual this) && other.label == label && other.egg == egg
			case _ => false
		}
		override def hashCode :Int = egg.hashCode * 31 + label.hashCode

		override def toString :String = "'" + label + "'@:" + egg
	}
}
