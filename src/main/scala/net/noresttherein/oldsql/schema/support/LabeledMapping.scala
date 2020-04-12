package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.GenericMapping
import net.noresttherein.oldsql.schema.support.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.{Component, ConcreteMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ShallowAdapter}
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth



trait LabeledMapping[N <: Label, S, O] extends GenericMapping[S, O]





object LabeledMapping {
	type Label = String with Singleton
//	type @:[N <: Label, M <: ConcreteMapping] = MappingLabel[N, M]
	trait @:[N <: Label, M <: ConcreteMapping] extends Adapted[M] with LabeledMapping[N, M#Subject, M#Origin]



	def apply[N <: Label, M <: Component[S, O], S, O]
	         (label :N, mapping :M)(implicit infer :IsBoth[M, M, Component[S, O]]) :N @: M =
		new MappingLabel[N, M, S, O](mapping)(new ValueOf[N](label))



	class MappingLabel[N <: Label, M <: Component[S, O], S, O](val egg :M)(implicit singleton :ValueOf[N])
		extends ShallowProxy[S, O] with (N @: M)
	{
		def this(label :N, egg :M) = this(egg)(new ValueOf(label))

		val label :N = singleton.value

		override def equals(that :Any) :Boolean = that match {
			case other :MappingLabel[_, _, _, _] =>
				(other eq this) || (other canEqual this) && other.label == label && other.egg == egg
			case _ => false
		}
		override def hashCode :Int = egg.hashCode * 31 + label.hashCode

		override def toString :String = "'" + label + "'@:" + egg

	}



}
