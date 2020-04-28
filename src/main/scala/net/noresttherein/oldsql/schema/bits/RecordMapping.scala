package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Record.{#>, |#, Item, Key}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, MappingExtract, MappingSchema}
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, ChainPrefixSchema, FlatChainPrefixSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.bits.RecordMapping.NonEmptyRecordMapping
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptySchema, EmptySchema, FlatMappingSchema, MappedFlatMappingSchema}


/** A mapping of `Record` instances - maps indexed on the type level with string literals.
  * It is a `SchemaMapping` and its own `MappingSchema` at the same time. The keys of the record are not mapped;
  * the component corresponding to each entry maps only the value, with its key being provided by the component
  * on assembly.
  * @author Marcin Mościcki
  */
trait RecordMapping[+C <: Chain, R <: Record, O] extends BaseChainMapping[C, R, O] {

	override val schema :RecordMapping[C, R, O] = this

	/** Appends a new column component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[K <: Label, M <: Subschema[_, _, T], T](key :K, component :M)
			:RecordMapping[C ~ M, R |# (K #> T), O] =
		new NonEmptyRecordMapping[C, M, R, K, T, O](this, key, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Label, L <: Chain, V <: Chain, T](key :K, component :Subschema[L, V, T])
			:RecordMapping[C ~ |*|[L, V, T], R |# (K #> T), O] =
		new NonEmptyRecordMapping[C, |*|[L, V, T], R, K, T, O](this, key, component)



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :RecordMapping[C ~ (N @|| T), R |# (N #> T), O] =
		append[N, N @|| T, T](name, LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
			:RecordMapping[C ~ (N @|| T), R |# (N #> T), O] =
		append[N, N @|| T, T](label, LabeledSchemaColumn[N, T, O](label, name, buffs:_*))






	private[schema] def asPrefix[T <: Record.Item] :MappingSchema[C, R, R |# T, O] =
		new ChainPrefixSchema[C, R, R, R |# T, O](this)

}







object RecordMapping {
	import Record.{Item, Key}

	def apply[O] :FlatRecordMapping[@~, @~, O] = empty.asInstanceOf[FlatRecordMapping[@~, @~, O]]

	private[this] val empty = new EmptyRecordMapping[Any]



	trait FlatRecordMapping[+C <: Chain, R <: Record, O]
		extends RecordMapping[C, R, O] with FlatSchemaMapping[C, R, R, O] with FlatMappingSchema[C, R, R, O]
	{
		override val schema :FlatRecordMapping[C, R, O] = this

		protected def col[K <: Label, M <: Subschema[_, _, T], T](key :K, component :M)
				:FlatRecordMapping[C ~ M, R |# (K #> T), O] =
			new NonEmptyFlatRecordMapping[C, M, R, K, T, O](this, key, component)


		override def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*)
				:FlatRecordMapping[C ~ (N @|| T), R |# (N #> T), O] =
			col[N, N @|| T, T](name, LabeledSchemaColumn(name, buffs:_*))

		override def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
				:FlatRecordMapping[C ~ (N @|| T), R |# (N #> T), O] =
			col[N, N @|| T, T](label, LabeledSchemaColumn(label, name, buffs:_*))



		override def map[S](assemble :R => S, disassemble :S => R) :FlatSchemaMapping[C, R, S, O] =
			new MappedFlatMappingSchema[C, R, S, O](this compose disassemble, assemble)



		private[schema] override def asPrefix[T <: Item] :FlatMappingSchema[C, R, R |# T, O] =
			new FlatChainPrefixSchema[C, R, R, R |# T, O](this)
	}






	private class EmptyRecordMapping[O] extends EmptySchema[@~, O] with RecordMapping[@~, @~, O]



	private class NonEmptyRecordSchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Record, K <: Key, T, S, O]
	                                  (first :MappingSchema[C, R, S, O], key :K, next :M,
	                                   extract :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[Record, |#, Item, C, M, R, T, K #> T, S, O](first, next, extract, _.last._2)
	{
		override protected def link(init :R, last :T) :R |# (K #> T) = init |# key #> last

		override def compose[X](extractor :X => S) :MappingSchema[C ~ M, R |# (K #> T), X, O] =
			new NonEmptyRecordSchema[C, M, R, K, T, X, O](init.compose(extractor), key, component,
			                                              this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MappingSchema[C ~ M, R |# (K #> T), X, O] =
			new NonEmptyRecordSchema[C, M, R, K, T, X, O](init compose extractor, key, component,
			                                              this.extractor compose extractor)

	}



	private class NonEmptyFlatRecordSchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Record, K <: Key, T, S, O]
	                                      (override val init :FlatMappingSchema[C, R, S, O], key :K, next :M,
	                                       extract :MappingExtract[S, T, O])
		extends NonEmptyRecordSchema[C, M, R, K, T, S, O](init, key, next, extract)
			with FlatMappingSchema[C ~ M, R |# (K #> T), S, O]
	{
		override def compose[X](extractor :X => S) :FlatMappingSchema[C ~ M, R |# (K #> T), X, O] =
			new NonEmptyFlatRecordSchema[C, M, R, K, T, X, O](init compose extractor, key, component,
			                                                  this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[C ~ M, R |# (K #> T), X, O] =
			new NonEmptyFlatRecordSchema[C, M, R, K, T, X, O](init compose extractor, key, component,
			                                                  this.extractor compose extractor)
	}





	private class NonEmptyRecordMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: Record, K <: Key, T, O]
	                                   (prefix :RecordMapping[C, R, O], key :K, next :M)
		extends NonEmptyRecordSchema[C, M, R, K, T, R |# (K #> T), O](
		                             prefix.asPrefix[K #> T], key, next,
		                             MappingExtract.req(next)((row :R |# (K #> T)) => row.last._2))
		   with RecordMapping[C ~ M, R |# (K #> T), O]



	private class NonEmptyFlatRecordMapping[+C <: Chain, +M <: TypedMapping[T, O], R <: Record, K <: Key, T, O]
	                                       (prefix :FlatRecordMapping[C, R, O], key :K, next :M)
		extends NonEmptyFlatRecordSchema[C, M, R, K, T, R |# (K #> T), O](
		                                 prefix.asPrefix[K #> T], key, next,
		                                 MappingExtract.req(next)((row :R |# (K #> T)) => row.last._2))
		   with FlatRecordMapping[C ~ M, R |# (K #> T), O]


}

