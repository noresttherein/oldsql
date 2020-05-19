package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.schema.{ColumnMapping, TypedMapping, Mapping}
import net.noresttherein.oldsql.schema.Mapping.{OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms



trait LabeledMapping[N <: Label, S, O] extends TypedMapping[S, O]






object LabeledMapping {
	/** A type of string literals used to label mappings on the type level for ease of access. */
	type Label = String with Singleton

	trait LabeledColumn[N <: Label, S, O] extends ColumnMapping[S, O] with LabeledMapping[N, S, O]



	def apply[N <: Label, M <: RefinedMapping[S, O], S, O]
	         (label :N, mapping :M)(implicit infer :Conforms[M, M, RefinedMapping[S, O]]) :N @: M =
		new MappingLabel[N, M, S, O](mapping)(new ValueOf[N](label))



	sealed trait @:[N <: Label, M <: Mapping] extends Adapted[M] with LabeledMapping[N, M#Subject, M#Origin] {
		def label :N
	}



	object @: {
		def unapply(mapping :Mapping) :Option[(Label, Mapping)] = mapping match {
			case labeled: @:[_, _] => Some(labeled.label -> labeled.egg)
			case _ => None
		}

		def unapply[N <: Label, M <: Mapping](labeled :N @: M) :Some[(N, M)] =
			Some(labeled.label -> labeled.egg)
	}



	implicit def LabeledMappingProjection[N <: Label, S, A, B]
			:OriginProjection[LabeledMapping[N, S, A], A, LabeledMapping[N, S, B], B] =
		OriginProjection()

	implicit def LabeledMappingAdapterProjection[N <: Label, M <: Mapping, A, R <: Mapping, B]
	                                            (implicit alias :OriginProjection[M, A, R, B])
			:OriginProjection[N @: M, A, N @: R, B] =
		labeled => (labeled.label @: alias(labeled.egg).asInstanceOf[RefinedMapping[Any, Any]]).asInstanceOf[N @: R]



	class MappingLabel[N <: Label, M <: RefinedMapping[S, O], S, O](val egg :M)(implicit singleton :ValueOf[N])
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

		override def toString :String = "'" + label + "@:" + egg

	}



}
