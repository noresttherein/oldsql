package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, BaseFlatChainMapping, ChainPrefixSchema, FlatChainPrefixSchema}
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, MappingExtract, MappingSchema, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{@||, |-|, ||, LabeledSchemaColumn}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.bits.LiteralIndexMapping.NonEmptyIndexMapping
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping


/** A mapping for `LiteralIndex` maps indexed on type level with literal types.
  * It is a `SchemaMapping` and its own `MappingSchema` at the same time, with the values individual components
  * in the schema being the values in the index at the corresponding positions.
  * @author Marcin Mościcki
  */
trait LiteralIndexMapping[V <: LiteralIndex, C <: Chain, O] extends BaseChainMapping[V, C, O] {

	override val schema :LiteralIndexMapping[V, C, O] = this

	/** Appends a new column component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[K <: Label, T, M <: |-|[T, _ <: Chain, _ <: Chain]](key :K, component :M)
			:LiteralIndexMapping[V |~ (K :~ T), C ~ M, O] =
		new NonEmptyIndexMapping[V, C, K, T, M, O](this, key, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Label, T, MV <: Chain, MC <: Chain](key :K, component: |-|[T, MV, MC])
			:LiteralIndexMapping[V |~ (K :~ T), C ~ |-|[T, MV, MC], O] =
		new NonEmptyIndexMapping[V, C, K, T, |-|[T, MV, MC], O](this, key, component)


	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :LiteralIndexMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](name, LabeledSchemaColumn[N, T, O](name, buffs:_*))

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
			:LiteralIndexMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](label, LabeledSchemaColumn[N, T, O](label, name, buffs:_*))




	private[schema] def asPrefix[T <: LiteralIndex.Item] :MappingSchema[V |~ T, V, C, O] =
		new ChainPrefixSchema[V |~ T, V, C, O](this)

}






object LiteralIndexMapping {
	import LiteralIndex.{Item, Key}

	def apply[O] :FlatLiteralIndexMapping[@~, @~, O] = empty.asInstanceOf[FlatLiteralIndexMapping[@~, @~, O]]

	private[this] val empty = new EmptyIndexMapping[Any]


	
	trait FlatLiteralIndexMapping[V <: LiteralIndex, C <: Chain, O]
		extends LiteralIndexMapping[V, C, O] with BaseFlatChainMapping[V, C, O]
	{
		override val schema :FlatLiteralIndexMapping[V, C, O] = this

		protected def col[K <: Label, T, M <: ||[T]](key :K, component :M)
				:FlatLiteralIndexMapping[V |~ (K :~ T), C ~ M, O] =
			new NonEmptyFlatIndexMapping[V, C, K, T, M, O](this, key, component)


		override def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*)
				:FlatLiteralIndexMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](name, LabeledSchemaColumn(name, buffs:_*))

		override def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
				:FlatLiteralIndexMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](label, LabeledSchemaColumn(label, name, buffs:_*))



		private[schema] override def asPrefix[T <: Item] :FlatMappingSchema[V |~ T, V, C, O] =
			new FlatChainPrefixSchema[V |~ T, V, C, O](this)
	}
	





	private class EmptyIndexMapping[O] extends EmptySchema[@~, O] with LiteralIndexMapping[@~, @~, O]

	

	private class NonEmptyIndexSchema[S, V <: LiteralIndex, C <: Chain,
		                              K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                 (first :MappingSchema[S, V, C, O], key :K, next :M,
	                                  extract :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[LiteralIndex, Item, |~, S, V, C, T, K :~ T, M, O](first, next, extract, _.last.value)
	{
		override protected def link(init :V, last :T) :V |~ (K :~ T) = init |~ key :~ last

		override def compose[X](extractor :X => S) :MappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyIndexSchema(init.compose(extractor), key, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyIndexSchema(init compose extractor, key, last, this.extractor compose extractor)

	}



	private class NonEmptyFlatIndexSchema[S, V <: LiteralIndex, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                     (override val init :FlatMappingSchema[S, V, C, O], key :K, next :M,
	                                      extract :MappingExtract[S, T, O])
		extends NonEmptyIndexSchema[S, V, C, K, T, M, O](init, key, next, extract)
		   with BaseNonEmptyFlatSchema[LiteralIndex, Item, |~, S, V, C, T, K :~ T, M, O]
		   with FlatMappingSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		override val selectForm = SQLReadForm.LiteralIndexReadForm(init.selectForm, new ValueOf(key), last.selectForm)
		override val queryForm = SQLWriteForm.LiteralIndexWriteForm(init.queryForm, last.queryForm)
		override val updateForm = SQLWriteForm.LiteralIndexWriteForm(init.updateForm, last.updateForm)
		override val insertForm = SQLWriteForm.LiteralIndexWriteForm(init.insertForm, last.insertForm)


		override def compose[X](extractor :X => S) :FlatMappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyFlatIndexSchema(init compose extractor, key, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyFlatIndexSchema(init compose extractor, key, last, this.extractor compose extractor)
	}





	private class NonEmptyIndexMapping[V <: LiteralIndex, C <: Chain, K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                  (prefix :LiteralIndexMapping[V, C, O], key :K, next :M)
		extends NonEmptyIndexSchema[V |~ (K :~ T), V, C, K, T, M, O](
		                            prefix.asPrefix[K :~ T], key, next,
		                            MappingExtract.req(next.refine.withOrigin[O]) {
			                            (row :V |~ (K :~ T)) => row.last.value
		                            })
		   with LiteralIndexMapping[V |~ (K :~ T), C ~ M, O]



	private class NonEmptyFlatIndexMapping[V <: LiteralIndex, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                      (prefix :FlatLiteralIndexMapping[V, C, O], key :K, next :M)
		extends NonEmptyFlatIndexSchema[V |~ (K :~ T), V, C, K, T, M, O](
		                                prefix.asPrefix[K :~ T], key, next,
		                                ColumnExtract.req(next.withOrigin[O])((row :V |~ (K :~ T)) => row.last.value))
		   with FlatLiteralIndexMapping[V |~ (K :~ T), C ~ M, O]
	
	
}


