package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.{GenericMapping, Mapping}
import net.noresttherein.oldsql.schema.support.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.{OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, ShallowAdapter}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms



trait LabeledMapping[N <: Label, S, O] extends GenericMapping[S, O]






object LabeledMapping {
	/** A type of string literals used to label mappings on the type level for ease of access. */
	type Label = String with Singleton

	sealed trait @:[N <: Label, M <: Mapping] extends Adapted[M] with LabeledMapping[N, M#Subject, M#Origin] {
		def label :N
	}



	def apply[N <: Label, M <: TypedMapping[S, O], S, O]
	         (label :N, mapping :M)(implicit infer :Conforms[M, M, TypedMapping[S, O]]) :N @: M =
		new MappingLabel[N, M, S, O](mapping)(new ValueOf[N](label))



	implicit def LabeledMappingProjection[N <: Label, S, A, B]
			:OriginProjection[LabeledMapping[N, S, A], A, LabeledMapping[N, S, B], B] =
		OriginProjection()

	implicit def LabeledMappingAdapterProjection[N <: Label, M <: Mapping, A, R <: Mapping, B]
	                                            (implicit alias :OriginProjection[M, A, R, B])
			:OriginProjection[N @: M, A, N @: R, B] =
		labeled => (labeled.label @: alias(labeled.egg).asInstanceOf[TypedMapping[Any, Any]]).asInstanceOf[N @: R]



	class MappingLabel[N <: Label, M <: TypedMapping[S, O], S, O](val egg :M)(implicit singleton :ValueOf[N])
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
