package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.{Chain, NaturalMap}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, ColumnMappingExtract, MappingExtract, MappingSchema, SchemaMapping}
import net.noresttherein.oldsql.schema.MappingSchema.{EmptySchema, FlatMappingSchema, NonEmptyFlatSchema, NonEmptySchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{@||, |-|, ||, FlatSchemaMapping, LabeledSchemaColumn, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, ChainPrefixSchema, NonEmptyChainMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy






/** A mapping for `Chain` heterogeneous lists which is a `SchemaMapping` and its own `MappingSchema` at the same time. */
trait ChainMapping[V <: Chain, C <: Chain, O] extends BaseChainMapping[V, C, O] {

	override val schema :ChainMapping[V, C, O] = this

	/** Appends a new component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[T, M <: |-|[T, _ <: Chain, _ <: Chain]](component :M) :ChainMapping[V ~ T, C ~ M, O] =
		new NonEmptyChainMapping(this, component)


	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[T, MV <: Chain, MC <: Chain](component: |-|[T, MV, MC]) :ChainMapping[V ~ T, C ~ |-|[T, MV, MC], O] =
		new NonEmptyChainMapping[V, C, T, |-|[T, MV, MC], O](this, component)



	/** Appends a new column to this schema with the given name. */
	def col[T :ColumnForm](name :String, buffs :Buff[T]*) :ChainMapping[V ~ T, C ~ ||[T], O] =
		append[T, ||[T]](SchemaColumn[T, O](name, buffs :_*))



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[V ~ T, C ~ (N @|| T), O] =
		append[T, N @|| T](LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*) :ChainMapping[V ~ T, C ~ (N @|| T), O] =
		append[T, N @|| T](LabeledSchemaColumn[N, T, O](label, name, buffs:_*))


	private[schema] def asPrefix[T] :MappingSchema[V ~ T, V, C, O] =
		new ChainPrefixSchema[V ~ T, V, C, O](this)

}






object ChainMapping {

	def apply[O] :FlatChainMapping[@~, @~, O] = empty.asInstanceOf[FlatChainMapping[@~, @~, O]]

	private[this] val empty = new EmptyChainMapping[Any]



	trait BaseChainMapping[V <: Chain, C <: Chain, O] extends SchemaMapping[V, V, C, O] with MappingSchema[V, V, C, O] {

		override val schema :MappingSchema[V, V, C, O] = this

		override def export[T](component :Component[T]) :Component[T] = component

		override def export[T](column :Column[T]) :Column[T] = column


//		abstract override val extracts :NaturalMap[Component, Extract] = packedExtracts
//
//		abstract override val columnExtracts :NaturalMap[Column, ColumnExtract] = packedColumnExtracts

	}



	trait BaseFlatChainMapping[V <: Chain, C <: Chain, O]
		extends BaseChainMapping[V, C, O] with FlatSchemaMapping[V, V, C, O] with FlatMappingSchema[V, V, C, O]
	{
		override val schema :FlatMappingSchema[V, V, C, O] = this
	}





	trait FlatChainMapping[V <: Chain, C <: Chain, O]
		extends ChainMapping[V, C, O] with BaseFlatChainMapping[V, C, O]
	{
		override val schema :FlatChainMapping[V, C, O] = this

		protected def col[T, M <: ||[T]](component :M) :FlatChainMapping[V ~ T, C ~ M, O] =
			new NonEmptyFlatChainMapping(this, component)

		override def col[T :ColumnForm](name :String, buffs :Buff[T]*) :FlatChainMapping[V ~ T, C ~ ||[T], O] =
			col[T, ||[T]](SchemaColumn(name, buffs:_*))

		override def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[V ~ T, C ~ (N @|| T), O] =
			col[T, N @|| T](LabeledSchemaColumn(name, buffs:_*))


		private[schema] override def asPrefix[T] :FlatMappingSchema[V ~ T, V, C, O] =
			new FlatChainPrefixSchema[V ~ T, V, C, O](this)
	}






	private class EmptyChainMapping[O] extends EmptySchema[@~, O] with FlatChainMapping[@~, @~, O]



	private class NonEmptyChainMapping[V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                  (prefix :ChainMapping[V, C, O], next :M)
		extends NonEmptySchema[V ~ T, V, C, T, M, O](
			                   prefix.asPrefix, next,
			                   MappingExtract.req(next.refine.withOrigin[O])(Chain.last))
		   with ChainMapping[V ~ T, C ~ M, O]



	private class NonEmptyFlatChainMapping[V <: Chain, C <: Chain, T, M <: ||[T], O]
	                                      (prefix :FlatChainMapping[V, C, O], next :M)
		extends NonEmptyFlatSchema[V ~ T, V, C, T, M, O](
		                           prefix.asPrefix, next, ColumnExtract.req(next.withOrigin[O])(Chain.last))
		   with FlatChainMapping[V ~ T, C ~ M, O]





	/** Adapts a `MappingSchema` with the owning mapping's subject type `V <: Chain` to a `MappingSchema`
	  * with outer subject type being a `Chain` longer by one entry. This is essentially a component
	  * mapping for a prefix chain `V` of a chain `S`.
	  * @tparam S the outer subject type of this schema.
	  * @tparam V the subject chain type of this schema, a prefix of the chain `S`.
	  * @tparam C the chain with all components in this schema.
	  */
	private[schema] class ChainPrefixSchema[S <: V ~ Any, V <: Chain, C <: Chain, O]
	                                       (protected val backer :MappingSchema[V, V, C, O])
		extends MappingSchema[S, V, C, O] with DirectProxy[V, O]
	{

		override def optionally(pieces :Pieces) :Option[V] = backer.optionally(pieces)


		override def unapply(subject :S) :Option[V] = backer.unapply(subject.init)

		override def disassemble(subject :S) :V = backer.disassemble(subject.init)



		override def members :C = backer.members

		override def last[M](implicit nonEmpty :C <:< (Chain ~ M)) :M = backer.members.last

		override def prev[I <: Chain, P <: Chain]
		                 (implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any)) :MappingSchema[S, I, P, O] =
			backer.prev compose Chain.init[V] _



		override def extract[X](component :Component[X]) :MappingExtract[S, X, O] =
			backer.extract(component) compose Chain.init[V] _

		override def extract[X](column :Column[X]) :ColumnMappingExtract[S, X, O] =
			backer.extract(column) compose Chain.init[V] _



		override val packedExtracts :NaturalMap[Component, PackedExtract] =
			backer.packedExtracts.map(oldsql.schema.composeExtractAssoc(this, Chain.init[V] _)(_))

		override val packedColumnExtracts :NaturalMap[Column, PackedColumnExtract] =
			backer.packedColumnExtracts.map(oldsql.schema.composeColumnExtractAssoc(this, Chain.init[V] _)(_))



		override def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O] =
			backer compose (extractor andThen Chain.init[V] _)



		override protected[schema] def componentsReversed :List[Component[_]] = backer.componentsReversed
		override protected[schema] def subcomponentsReversed :List[Component[_]] = backer.subcomponentsReversed
		override protected[schema] def columnsReversed :List[Column[_]] = backer.columnsReversed

	}



	private[schema] class FlatChainPrefixSchema[S <: V ~ Any, V <: Chain, C <: Chain, O]
	                                           (protected override val backer :FlatMappingSchema[V, V, C, O])
		extends ChainPrefixSchema[S, V, C, O](backer) with FlatMappingSchema[S, V, C, O]
	{
		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V, C, O] =
			backer compose (extractor andThen Chain.init[V] _)

		override def prev[I <: Chain, P <: Chain]
		                   (implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any)) :FlatMappingSchema[S, I, P, O] =
			backer.prev compose Chain.init[V] _
	}


}


