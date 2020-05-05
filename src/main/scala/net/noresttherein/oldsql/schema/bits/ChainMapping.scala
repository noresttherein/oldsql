package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, NaturalMap}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, ColumnMappingExtract, MappingExtract, MappingSchema, SchemaMapping, SQLForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.MappingSchema.{EmptySchema, FlatMappedFlatSchema, FlatMappedSchema, FlatMappingSchema, FlatNonEmptySchema, GetLabeledComponent, GetSchemaComponent, MappedFlatSchema, MappedSchema, NonEmptySchema, SchemaFlattening}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn, MappedFlatSchemaMapping, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, ChainPrefixSchema, NonEmptyChainMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy






/** A mapping for `Chain` heterogeneous lists which is a `SchemaMapping` and its own `MappingSchema` at the same time. */
trait ChainMapping[C <: Chain, R <: Chain, O] extends BaseChainMapping[C, R, O] {

	override val schema :ChainMapping[C, R, O] = this

	/** Appends a new component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[M <: Subschema[_, _, T], T](component :M) :ChainMapping[C ~ M, R ~ T, O] =
		new NonEmptyChainMapping[C, M, R, T, O](this, component)


	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[L <: Chain, V <: Chain, T](component :Subschema[L, V, T]) :ChainMapping[C ~ |*|[L, V, T], R ~ T, O] =
		new NonEmptyChainMapping[C, |*|[L, V, T], R, T, O](this, component)



	/** Appends a new column to this schema with the given name. */
	def col[T :ColumnForm](name :String, buffs :Buff[T]*) :ChainMapping[C ~ ||[T], R ~ T, O] =
		append[||[T], T](SchemaColumn[T, O](name, buffs :_*))



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
		append[N @|| T, T](LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def lbl[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
		append[N @|| T, T](LabeledSchemaColumn[N, T, O](label, name, buffs:_*))


	private[schema] def asPrefix[T] :MappingSchema[C, R, R ~ T, O] =
		new ChainPrefixSchema[C, R, R, R ~ T, O](this)

}






object ChainMapping {

	def apply[O] :FlatChainMapping[@~, @~, O] = empty.asInstanceOf[FlatChainMapping[@~, @~, O]]

	private[this] val empty = new EmptyChainMapping[Any]



	trait BaseChainMapping[C <: Chain, R <: Chain, O] extends SchemaMapping[C, R, R, O] with MappingSchema[C, R, R, O] {
		override type Components = C
		override type Schema[Q] = MappingSchema[C, R, R, Q]

		override val schema :MappingSchema[C, R, R, O] = this

		override def export[T](component :Component[T]) :Component[T] = component

		override def export[T](column :Column[T]) :Column[T] = column


		override val extracts :NaturalMap[Component, Extract] = outerExtracts

		override val columnExtracts :NaturalMap[Column, ColumnExtract] = outerColumnExtracts


		override def apply[N <: Label, T]
		                  (label :N)(implicit get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O])
				:Extract[T] =
			super.apply(label)

		override def /[M <: LabeledMapping[N, T, O], N <: Label, T]
		              (label :N)(implicit get :GetLabeledComponent[C, R, M, N, T, O]) :M =
			super./[M, N, T](label)


		override def apply[I <: Numeral, T]
		                  (idx :I)(implicit get :GetSchemaComponent[C, R, Component[T], I, T, O])
				:MappingExtract[R, T, O] =
			super.apply[I, T](idx)

		override def /[M <: Component[T], I <: Numeral, T]
		              (idx :I)(implicit get :MappingSchema.GetSchemaComponent[C, R, M, I, T, O]) :M =
			super./[M, I, T](idx)


	}



	trait BaseFlatChainMapping[C <: Chain, R <: Chain, O]
		extends BaseChainMapping[C, R, O] with FlatSchemaMapping[C, R, R, O] with FlatMappingSchema[C, R, R, O]
	{
		override val schema :FlatMappingSchema[C, R, R, O] = this
	}





	trait FlatChainMapping[C <: Chain, R <: Chain, O]
		extends ChainMapping[C, R, O] with BaseFlatChainMapping[C, R, O]
	{
		override val schema :FlatChainMapping[C, R, O] = this

		protected def col[M <: ||[T], T](component :M) :FlatChainMapping[C ~ M, R ~ T, O] =
			new NonEmptyFlatChainMapping[C, M, R, T, O](this, component)

		override def col[T :ColumnForm](name :String, buffs :Buff[T]*) :FlatChainMapping[C ~ ||[T], R ~ T, O] =
			col[||[T], T](SchemaColumn[T, O](name, buffs:_*))

		override def lbl[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ChainMapping[C ~ (N @|| T), R ~ T, O] =
			col[N @|| T, T](LabeledSchemaColumn(name, buffs:_*))


		private[schema] override def asPrefix[T] :FlatMappingSchema[C, R, R ~ T, O] =
			new FlatChainPrefixSchema[C, R, R, R ~ T, O](this)
	}






	private def prefix[C <: Chain] :(C ~ Any) => C = init.asInstanceOf[(C ~ Any) => C]
	private[this] val init = (_:Chain ~ Any).init



	private class EmptyChainMapping[O] extends EmptySchema[@~, O] with FlatChainMapping[@~, @~, O]



	private class NonEmptyChainMapping[C <: Chain, M <: TypedMapping[T, O], R <: Chain, T, O]
	                                  (prefix :ChainMapping[C, R, O], next :M)
		extends NonEmptySchema[C, M, R, T, R ~ T, O](
				               prefix.asPrefix[T], next, MappingExtract.req(next)((row :R ~ T) => row.last))
		   with ChainMapping[C ~ M, R ~ T, O]



	private class NonEmptyFlatChainMapping[C <: Chain, M <: ColumnMapping[T, O], R <: Chain, T, O]
	                                      (prefix :FlatChainMapping[C, R, O], next :M)
		extends FlatNonEmptySchema[C, M, R, T, R ~ T, O](
		                       prefix.asPrefix[T], next, MappingExtract.req(next)((row :R ~ T) => row.last))
		   with FlatChainMapping[C ~ M, R ~ T, O]





	/** Adapts a `MappingSchema` with the owning mapping's subject type `T &lt;: Chain` to a `MappingSchema` 
	  * with outer subject type being a `Chain` longer by one entry. This is essentially a component
	  * mapping for a prefix chain `R` of a chain `S`.
	  * @tparam C the chain with all components in this schema.
	  * @tparam R the subject chain type of this schema, a prefix of the chain `T`.
	  * @tparam T the outer subject type of the extended schema, the init of the outer subject type of this mapping.
	  * @tparam S the outer subject type of this schema.           
	  */
	private[schema] class ChainPrefixSchema[C <: Chain, R <: Chain, T <: Chain, S <: T ~ Any, O]
	                                       (protected val egg :MappingSchema[C, R, T, O])
		extends MappingSchema[C, R, S, O] with ShallowProxy[R, O]
	{

		override def optionally(pieces :Pieces) :Option[R] = egg.optionally(pieces)


		override def unapply(subject :S) :Option[R] = egg.unapply(subject.init)

		override def disassemble(subject :S) :R = egg.disassemble(subject.init)



		override def members :C = egg.members

		override def last[M <: Component[_]](implicit nonEmpty :C <:< (Chain ~ M)) :M = egg.members.last

		override def prev[I <: Chain, V <: Chain]
		                 (implicit comps :C <:< (I ~ Any), vals :R <:< (V ~ Any)) :MappingSchema[I, V, S, O] =
			egg.prev compose prefix[T]



		override def extract[X](component :Component[X]) :MappingExtract[S, X, O] =
			egg.extract(component) compose prefix[T]

		override def extract[X](column :Column[X]) :ColumnMappingExtract[S, X, O] =
			egg.extract(column) compose prefix[T]



		override val outerExtracts :NaturalMap[Component, OuterExtract] =
			egg.outerExtracts.map(schema.composeExtractAssoc(this, prefix[T])(_))

		override val outerColumnExtracts :NaturalMap[Column, OuterColumnExtract] =
			egg.outerColumnExtracts.map(schema.composeColumnExtractAssoc(this, prefix[T])(_))



		override def compose[X](extractor :X => S) :MappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[T])

		override def compose[X](extractor :X =?> S) :MappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[T])



		override protected[schema] def componentsReversed :List[Component[_]] = egg.componentsReversed

		override protected[schema] def subcomponentsReversed :List[Component[_]] = egg.subcomponentsReversed

		override protected[schema] def columnsReversed :List[Column[_]] = egg.columnsReversed

	}



	private[schema] class FlatChainPrefixSchema[C <: Chain, R <: Chain, T <: Chain, S <: T ~ Any, O]
	                                           (protected override val egg :FlatMappingSchema[C, R, T, O])
		extends ChainPrefixSchema[C, R, T, S, O](egg) with FlatMappingSchema[C, R, S, O]
	{
		override def compose[X](extractor :X => S) :FlatMappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[T])

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[C, R, X, O] =
			egg compose (extractor andThen prefix[T])

		override def prev[I <: Chain, V <: Chain]
		                 (implicit comps :C <:< (I ~ Any), vals :R <:< (V ~ Any)) :FlatMappingSchema[I, V, S, O] =
			egg.prev.compose(prefix[T])

	}


}


