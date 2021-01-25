package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnForm, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter
import net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.ColumnFormAdapter
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter






/** Super type for `Mapping` implementations marked with a `String` literal type for the purpose of identification
  * and ease of reference. Note that the mapping is labeled on type-level only by default and the label value is not
  * required to be present. This is the most generic common interface, actual subclasses should generally extend
  * [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]] instead.
  * You can label any mapping with the right associative method `@:` defined by implicit conversion. This will
  * return [[net.noresttherein.oldsql.schema.bits.LabeledMapping.@: N @: M]], adapting the mapping `M`
  * to this interface, while preserving its type for access.
  * @tparam N a `String` literal with a name serving as an identifier for this mapping.
  */
trait AbstractLabeledMapping[N <: Label] extends Mapping



/** A base trait for custom `LabeledMapping` implementations. */
trait LabeledMapping[N <: Label, S, O] extends BaseMapping[S, O] with AbstractLabeledMapping[N] {
	type LabeledProjection[A] = LabeledMapping[N, S, A]
}






object LabeledMapping {

	/** A type of string literals used to label mappings on the type level for ease of access. */
	type Label = String with Singleton //todo: replace its usages with LabelPath.Label once we start using it

	/** A base trait for labeled columns. */
	trait LabeledColumn[N <: Label, S, O] extends ColumnMapping[S, O] with LabeledMapping[N, S, O]

	type Of[S] = { type As[N <: Label] = { type P[O] = LabeledMapping[N, S, O] } }

	def LabeledColumn[N <: Label, S, O](label :N, column :ColumnMapping[S, O]) :N @: ColumnMapping[S, O] =
		new ColumnLabel[N, S, O](column)(new ValueOf[N](label))



	def apply[N <: Label, M <: RefinedMapping[S, O], S, O](label :N, mapping :M) :N @: M =
		new MappingLabel[N, M, S, O](mapping)(new ValueOf[N](label))



	/** Adapter mapping attaching a `String` literal type `N` as a label to the mapping type `M`.
	  * Note that this mapping's `Origin` and `Subject` types are equal to the types defined in the adapted mapping,
	  * but are not declared in the type signature directly as parameters. For this reason instances of this type
	  * won't be adapted automatically to `MappingOf`.
	  * See [[net.noresttherein.oldsql.schema.bits.LabeledMapping.LabeledProjection LabeledProjection]]
	  * for the appropriate type constructor.
	  */
	sealed trait @:[N <: Label, M <: Mapping]
		extends LabeledMapping[N, M#Subject, M#Origin]
		   with MappingAdapter[M, M#Subject, M#Origin]
	{
		def label :N

		def upcast[U >: M <: Mapping { type Subject <: M#Subject; type Origin <: M#Origin }] :N @: U =
			this.asInstanceOf[N @: U]

		override def mappingName :String = "'" + label + "@:" + body.mappingName
	}



	object @: {
		def unapply(mapping :Mapping) :Opt[(Label, Mapping)] = mapping match {
			case labeled: @:[_, _] => Got(labeled.label -> labeled.body)
			case _ => Lack
		}

		def unapply[N <: Label, M <: Mapping](labeled :N @: M) :Opt[(N, M)] =
			Got(labeled.label -> labeled.body)


		type LabeledProjection[L <: Label, M[O] <: MappingAt[O]] = { type WithOrigin[O] = L @: M[O] }

		implicit def projection[L <: Label, M <: Mapping](implicit body :ExactProjection[M])
				:ProjectionDef[L @: M, ({ type P[X] = L @: body.WithOrigin[X] })#P, M#Subject] =
			body.lift[({ type T[X <: Mapping] = L @: X })#T]

	}



	class MappingLabel[N <: Label, M <: RefinedMapping[S, O], S, O](protected val backer :M)
	                                                               (implicit singleton :ValueOf[N])
		extends DirectProxy[S, O] with DelegateAdapter[M, S, O] with (N @: M)
	{
		def this(label :N, backer :M) = this(backer)(new ValueOf(label))

		override type Subject = S
		override type Origin = O

		override val label :N = singleton.value
	}



	class ColumnLabel[N <: Label, S, O](column :ColumnMapping[S, O], columnName :String, columnBuffs :Seq[Buff[S]] = Nil)
	                                   (implicit form :ColumnForm[S], labelValue :ValueOf[N])
		extends ColumnFormAdapter[ColumnMapping[S, O], S, O](column, columnName, columnBuffs)
		   with LabeledColumn[N, S, O] with (N @: ColumnMapping[S, O])
	{
		def this(column :ColumnMapping[S, O])(implicit label :ValueOf[N]) =
			this(column, column.name, column.buffs)(column.form, label)

		override val label :N = labelValue.value

		override def toString :String = "'" + label + "@:" + name + "[" + form + "]"
	}



}
