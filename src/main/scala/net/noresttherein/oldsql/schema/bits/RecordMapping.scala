package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.{Chain, Record}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Record.{#>, |#}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, BaseFlatChainMapping, ChainPrefixSchema, FlatChainPrefixSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.bits.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema}
import net.noresttherein.oldsql.schema.bits.RecordMapping.NonEmptyRecordMapping
import net.noresttherein.oldsql.schema.bits.SchemaMapping.{@||, |-|, ||, LabeledSchemaColumn}
import net.noresttherein.oldsql.schema.forms.{SQLReadForms, SQLWriteForms}






/** A mapping of `Record` instances - maps indexed on the type level with string literals.
  * It is a `SchemaMapping` and its own `MappingSchema` at the same time. The keys of the record are not mapped;
  * the component corresponding to each entry maps only the value, with its key being provided by the component
  * on assembly.
  * @author Marcin Mościcki
  */
trait RecordMapping[V <: Record, C <: Chain, O] extends BaseChainMapping[V, C, O] {

	override val schema :RecordMapping[V, C, O] = this

	/** Appends a new column component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[K <: Label, T, M <: |-|[T, _ <: Chain, _ <: Chain]](key :K, component :M)
			:RecordMapping[V |# (K #> T), C ~ M, O] =
		new NonEmptyRecordMapping[V, C, K, T, M, O](this, key, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Label, T, MV <: Chain, MC <: Chain](key :K, component: |-|[T, MV, MC])
			:RecordMapping[V |# (K #> T), C ~ |-|[T, MV, MC], O] =
		new NonEmptyRecordMapping[V, C, K, T, |-|[T, MV, MC], O](this, key, component)



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :RecordMapping[V |# (N #> T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](name, LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
			:RecordMapping[V |# (N #> T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](label, LabeledSchemaColumn[N, T, O](label, name, buffs:_*))






	private[schema] def asPrefix[T <: Record.Item] :MappingSchema[V |# T, V, C, O] =
		new ChainPrefixSchema[V |# T, V, C, O](this)

}







object RecordMapping {
	import Record.{Item, Key}

	def apply[O] :FlatRecordMapping[@~, @~, O] = empty.asInstanceOf[FlatRecordMapping[@~, @~, O]]

	private[this] val empty = new EmptyRecordMapping[Any]



	trait FlatRecordMapping[V <: Record, C <: Chain, O]
		extends RecordMapping[V, C, O] with BaseFlatChainMapping[V, C, O]
	{
		override val schema :FlatRecordMapping[V, C, O] = this

		protected def col[K <: Label, T, M <: ||[T]](key :K, component :M)
				:FlatRecordMapping[V |# (K #> T), C ~ M, O] =
			new NonEmptyFlatRecordMapping[V, C, K, T, M, O](this, key, component)


		override def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*)
				:FlatRecordMapping[V |# (N #> T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](name, LabeledSchemaColumn(name, buffs:_*))

		override def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
				:FlatRecordMapping[V |# (N #> T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](label, LabeledSchemaColumn(label, name, buffs:_*))



		private[schema] override def asPrefix[T <: Item] :FlatMappingSchema[V |# T, V, C, O] =
			new FlatChainPrefixSchema[V |# T, V, C, O](this)
	}






	private class EmptyRecordMapping[O] extends EmptySchema[@~, O] with RecordMapping[@~, @~, O]



	private class NonEmptyRecordSchema[S, V <: Record, C <: Chain, K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                  (first :MappingSchema[S, V, C, O], key :K, next :M,
	                                   extract :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[Record, Item, |#, S, V, C, T, K #> T, M, O](first, next, extract, _.last._2)
	{
		override protected def link(init :V, last :T) :V |# (K #> T) = init |# key #> last

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V |# (K #> T), C ~ M, O] =
			new NonEmptyRecordSchema(init compose extractor, key, last, this.extractor compose extractor)
	}



	private class NonEmptyFlatRecordSchema[S, V <: Record, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                      (override val init :FlatMappingSchema[S, V, C, O], key :K, next :M,
	                                       extract :MappingExtract[S, T, O])
		extends NonEmptyRecordSchema[S, V, C, K, T, M, O](init, key, next, extract)
		   with BaseNonEmptyFlatSchema[Record, Key #> Any, |#, S, V, C, T, K #> T, M, O]
		   with FlatMappingSchema[S, V |# (K #> T), C ~ M, O]
	{
		override val selectForm = SQLReadForms.RecordReadForm(init.selectForm, new ValueOf(key), last.selectForm)
		override val filterForm = SQLWriteForms.RecordWriteForm(init.filterForm, last.filterForm)
		override val updateForm = SQLWriteForms.RecordWriteForm(init.updateForm, last.updateForm)
		override val insertForm = SQLWriteForms.RecordWriteForm(init.insertForm, last.insertForm)
		override def writeForm(op :WriteOperationType) :SQLWriteForm[V |# (K, T)] = op.form(this)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V |# (K #> T), C ~ M, O] =
			new NonEmptyFlatRecordSchema(init compose extractor, key, last, this.extractor compose extractor)
	}





	private class NonEmptyRecordMapping[V <: Record, C <: Chain, K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                   (prefix :RecordMapping[V, C, O], key :K, next :M)
		extends NonEmptyRecordSchema[V |# (K #> T), V, C, K, T, M, O](
		                             prefix.asPrefix[K #> T], key, next,
		                             MappingExtract.req(next.refine.withOrigin[O]) {
			                             (row :V |# (K #> T)) => row.last._2
		                             })
		   with RecordMapping[V |# (K #> T), C ~ M, O]



	private class NonEmptyFlatRecordMapping[V <: Record, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                       (prefix :FlatRecordMapping[V, C, O], key :K, next :M)
		extends NonEmptyFlatRecordSchema[V |# (K #> T), V, C, K, T, M, O](
		                                 prefix.asPrefix[K #> T], key, next,
		                                 ColumnExtract.req(next.withOrigin[O])((row :V |# (K #> T)) => row.last._2))
		   with FlatRecordMapping[V |# (K #> T), C ~ M, O]


}

