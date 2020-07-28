package net.noresttherein.oldsql.schema.bits

import scala.annotation.unchecked.uncheckedVariance

import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingSeal, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{AdapterSeal, DelegateAdapter}
import net.noresttherein.oldsql.schema.bits.MappingAdapter.ColumnAdapter.ColumnFormProxy
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}



/** Super type for `Mapping` implementations marked with a `String` literal type for the purpose of identification
  * and ease of reference. Note that the mapping is labeled on type-level only by default and the label value is not
  * required to be present. This is the most generic common interface, actual subclasses should generally extend
  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]] instead.
  * You can label any mapping with the right associative method `@:` defined by implicit conversion. This will
  * return [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@: N @: M]], adapting the mapping `M`
  * to this interface, while preserving its type for access.
  * @tparam N a `String` literal with a name serving as an identifier for this mapping.
  */
trait AbstractLabeledMapping[N <: Label] extends Mapping { self :MappingSeal => }



/** A base trait for custom `LabeledMapping` implementations. */
trait LabeledMapping[N <: Label, S, O] extends TypedMapping[S, O] with AbstractLabeledMapping[N] {
	type LabeledProjection[A] = LabeledMapping[N, S, A]
}






object LabeledMapping {

	/** A type of string literals used to label mappings on the type level for ease of access. */
	type Label = String with Singleton //todo: replace its usages with LabelPath.Label once we start using it

	/** A base trait for labeled columns. */
	trait LabeledColumn[N <: Label, S, O] extends ColumnMapping[S, O] with LabeledMapping[N, S, O]

	def LabeledColumn[N <: Label, S, O](label :N, column :ColumnMapping[S, O]) :N @: ColumnMapping[S, O] =
		new ColumnLabel[N, S, O](column)(new ValueOf[N](label))



	def apply[N <: Label, M <: RefinedMapping[S, O], S, O](label :N, mapping :M) :N @: M =
		new MappingLabel[N, M, S, O](mapping)(new ValueOf[N](label))



	/** Adapter mapping attaching a `String` literal type `N` as a label to the mapping type `M`.
	  * Note that this mapping's `Origin` and `Subject` types are equal to the types defined in the adapted mapping,
	  * but are not declared in the type signature directly. For this reason instances of this type won't be
	  * adapted automatically to `MappingOf`.
	  */
	sealed trait @:[N <: Label, +M <: Mapping]
		extends LabeledMapping[N, M#Subject @uncheckedVariance, M#Origin @uncheckedVariance]
		   with MappingAdapter[M, M#Subject @uncheckedVariance, M#Origin @uncheckedVariance]
	{ this :MappingSeal with AdapterSeal =>

		def label :N

		override def toString = "'" + label + "@:" + body
	}



	object @: {
		def unapply(mapping :Mapping) :Option[(Label, Mapping)] = mapping match {
			case labeled: @:[_, _] => Some(labeled.label -> labeled.body)
			case _ => None
		}

		def unapply[N <: Label, M <: Mapping](labeled :N @: M) :Some[(N, M)] =
			Some(labeled.label -> labeled.body)


		type LabeledProjection[L <: Label, M[O] <: MappingAt[O]] = { type WithOrigin[O] = L @: M[O] }

		implicit def projection[L <: Label, M <: Mapping](implicit body :ExactProjection[M])
				:ProjectionDef[L @: M, ({ type P[X] = L @: body.WithOrigin[X] })#P, M#Subject] =
			body.lift[({ type T[+X <: Mapping] = L @: X })#T]

	}



	class MappingLabel[N <: Label, M <: RefinedMapping[S, O], S, O](val backer :M)(implicit singleton :ValueOf[N])
		extends DirectProxy[S, O] with DelegateAdapter[M, S, O] with (N @: M)
	{
		def this(label :N, backer :M) = this(backer)(new ValueOf(label))

		override type Subject = S
		override type Origin = O

		override val label :N = singleton.value

	}



	class ColumnLabel[N <: Label, S, O](column :ColumnMapping[S, O], columnName :String, columnBuffs :Seq[Buff[S]] = Nil)
	                                   (implicit form :ColumnForm[S], labelValue :ValueOf[N])
		extends ColumnFormProxy[ColumnMapping[S, O], S, O](column, columnName, columnBuffs)
		   with LabeledColumn[N, S, O] with (N @: ColumnMapping[S, O])
	{
		def this(column :ColumnMapping[S, O])(implicit label :ValueOf[N]) =
			this(column, column.name, column.buffs)(column.form, label)

		override val label :N = labelValue.value

		override def toString = "'" + label + "@:" + name + "[" + form + "]"
	}



}
