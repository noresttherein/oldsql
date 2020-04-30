package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, ChainPrefixSchema, FlatChainPrefixSchema, NonEmptyChainMapping}
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, MappingExtract, MappingSchema, SchemaMapping}
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptySchema, EmptySchema, FlatMappingSchema, MappedFlatSchema, MappedSchema, NonEmptySchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.bits.LiteralIndexMapping.NonEmptyIndexMapping


/** A mapping for `LiteralIndex` maps indexed on type level with literal types.
  * It is a `SchemaMapping` and its own `MappingSchema` at the same time, with the values individual components
  * in the schema being the values in the index at the corresponding positions.
  * @author Marcin Mościcki
  */
trait LiteralIndexMapping[+C <: Chain, R <: LiteralIndex, O] extends BaseChainMapping[C, R, O] {

	override val schema :LiteralIndexMapping[C, R, O] = this

	/** Appends a new column component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[K <: Label, M <: Subschema[_, _, T], T](key :K, component :M)
			:LiteralIndexMapping[C ~ M, R |~ (K :~ T), O] =
		new NonEmptyIndexMapping[C, M, R, K, T, O](this, key, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Label, L <: Chain, V <: Chain, T](key :K, component :Subschema[L, V, T])
			:LiteralIndexMapping[C ~ |*|[L, V, T], R |~ (K :~ T), O] =
		new NonEmptyIndexMapping[C, |*|[L, V, T], R, K, T, O](this, key, component)


	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :LiteralIndexMapping[C ~ (N @|| T), R |~ (N :~ T), O] =
		append[N, N @|| T, T](name, LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
			:LiteralIndexMapping[C ~ (N @|| T), R |~ (N :~ T), O] =
		append[N, N @|| T, T](label, LabeledSchemaColumn[N, T, O](label, name, buffs:_*))




	private[schema] def asPrefix[T <: LiteralIndex.Item] :MappingSchema[C, R, R |~ T, O] =
		new ChainPrefixSchema[C, R, R, R |~ T, O](this)

}






object LiteralIndexMapping {
	import LiteralIndex.{Item, Key}

	def apply[O] :FlatLiteralIndexMapping[@~, @~, O] = empty.asInstanceOf[FlatLiteralIndexMapping[@~, @~, O]]

	private[this] val empty = new EmptyIndexMapping[Any]


	
	trait FlatLiteralIndexMapping[+C <: Chain, R <: LiteralIndex, O] 
		extends LiteralIndexMapping[C, R, O] with FlatSchemaMapping[C, R, R, O] with FlatMappingSchema[C, R, R, O] 
	{
		override val schema :FlatLiteralIndexMapping[C, R, O] = this

		protected def col[K <: Label, M <: Subschema[_, _, T], T](key :K, component :M)
				:FlatLiteralIndexMapping[C ~ M, R |~ (K :~ T), O] =
			new NonEmptyFlatIndexMapping[C, M, R, K, T, O](this, key, component)


		override def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*)
				:FlatLiteralIndexMapping[C ~ (N @|| T), R |~ (N :~ T), O] =
			col[N, N @|| T, T](name, LabeledSchemaColumn(name, buffs:_*))

		override def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
				:FlatLiteralIndexMapping[C ~ (N @|| T), R |~ (N :~ T), O] =
			col[N, N @|| T, T](label, LabeledSchemaColumn(label, name, buffs:_*))



		override def map[S](assemble :R => S, disassemble :S => R) :FlatSchemaMapping[C, R, S, O] =
			new MappedFlatSchema[C, R, S, O](this compose disassemble, assemble)



		private[schema] override def asPrefix[T <: Item] :FlatMappingSchema[C, R, R |~ T, O] =
			new FlatChainPrefixSchema[C, R, R, R |~ T, O](this)
	}
	





	private class EmptyIndexMapping[O] extends EmptySchema[@~, O] with LiteralIndexMapping[@~, @~, O]

	

	private class NonEmptyIndexSchema[+C <: Chain, +M <: TypedMapping[T, O], R <: LiteralIndex, K <: Key, T, S, O]
	                                 (first :MappingSchema[C, R, S, O], key :K, next :M,
	                                  extract :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[LiteralIndex, |~, Item, C, M, R, T, K :~ T, S, O](first, next, extract, _.last.value)
	{
		override protected def link(init :R, last :T) :R |~ (K :~ T) = init |~ key :~ last

		override def compose[X](extractor :X => S) :MappingSchema[C ~ M, R |~ (K :~ T), X, O] =
			new NonEmptyIndexSchema[C, M, R, K, T, X, O](init.compose(extractor), key, last,
			                                             this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MappingSchema[C ~ M, R |~ (K :~ T), X, O] =
			new NonEmptyIndexSchema[C, M, R, K, T, X, O](init compose extractor, key, last,
			                                             this.extractor compose extractor)

	}



	private class NonEmptyFlatIndexSchema[+C <: Chain, +M <: TypedMapping[T, O], R <: LiteralIndex, K <: Key, T, S, O]
	                                     (override val init :FlatMappingSchema[C, R, S, O], key :K, next :M,
	                                      extract :MappingExtract[S, T, O])
		extends NonEmptyIndexSchema[C, M, R, K, T, S, O](init, key, next, extract)
		   with FlatMappingSchema[C ~ M, R |~ (K :~ T), S, O]
	{

		override def prev[P <: Chain, V <: Chain](implicit comps :C ~ M <:< (P ~ Any), vals :R |~ (K :~ T) <:< (V ~ Any))
				:FlatMappingSchema[P, V, S, O] =
			init.asInstanceOf[FlatMappingSchema[P, V, S, O]]

		override def compose[X](extractor :X => S) :FlatMappingSchema[C ~ M, R |~ (K :~ T), X, O] =
			new NonEmptyFlatIndexSchema[C, M, R, K, T, X, O](init compose extractor, key, last,
                                                             this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[C ~ M, R |~ (K :~ T), X, O] =
			new NonEmptyFlatIndexSchema[C, M, R, K, T, X, O](init compose extractor, key, last,
		                                                     this.extractor compose extractor)
	}





	private class NonEmptyIndexMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: LiteralIndex, K <: Key, T, O]
	                                  (prefix :LiteralIndexMapping[C, R, O], key :K, next :M)
		extends NonEmptyIndexSchema[C, M, R, K, T, R |~ (K :~ T), O](
		                            prefix.asPrefix[K :~ T], key, next,
		                            MappingExtract.req(next)((row :R |~ (K :~ T)) => row.last.value))
		   with LiteralIndexMapping[C ~ M, R |~ (K :~ T), O]



	private class NonEmptyFlatIndexMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: LiteralIndex, K <: Key, T, O]
	                                      (prefix :FlatLiteralIndexMapping[C, R, O], key :K, next :M)
		extends NonEmptyFlatIndexSchema[C, M, R, K, T, R |~ (K :~ T), O](
		                                prefix.asPrefix[K :~ T], key, next,
		                                MappingExtract.req(next)((row :R |~ (K :~ T)) => row.last.value))
		   with FlatLiteralIndexMapping[C ~ M, R |~ (K :~ T), O]
	
	
}


