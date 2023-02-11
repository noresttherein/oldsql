package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{Chain, Listing}
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.Listing.{:~, |~, Item, Key}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, MappingExtract, SQLWriteForm}
import net.noresttherein.oldsql.schema.bits.ChainMapping.{BaseChainMapping, BaseFlatChainMapping, ChainPrefixSchema, FlatChainPrefixSchema}
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.bits.ListingMapping.NonEmptyListingMapping
import net.noresttherein.oldsql.schema.bits.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema}
import net.noresttherein.oldsql.schema.bits.SchemaMapping.{@|-|, @||, |-|, ||, LabeledSchemaColumn}






/** A mapping for [[net.noresttherein.oldsql.collection.Listing Listing]] dictionaries indexed on type level
  * by arbitrary types. It is a [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]] and its own
  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] at the same time, with the values
  * of individual components in the schema being the values in the listing at the corresponding positions.
  * @author Marcin Mościcki
  */
trait ListingMapping[V <: Listing, C <: Chain, O] extends BaseChainMapping[V, C, O] {

	override val schema :ListingMapping[V, C, O] = this

	/** Appends a new column component to this schema. Full static type of the column will be encoded in the
	  * component chain of the returned schema.
	  */
	protected def append[K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain]](key :K, component :M)
			:ListingMapping[V |~ (K :~ T), C ~ M, O] =
		new NonEmptyListingMapping[V, C, K, T, M, O](this, key, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Label, T, MV <: Chain, MC <: Chain](component: @|-|[K, T, MV, MC])
			:ListingMapping[V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		new NonEmptyListingMapping[V, C, K, T, @|-|[K, T, MV, MC], O](this, component.label, component)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  */
	def comp[K <: Key, T, MV <: Chain, MC <: Chain](key :K, component: |-|[T, MV, MC])
			:ListingMapping[V |~ (K :~ T), C ~ |-|[T, MV, MC], O] =
		new NonEmptyListingMapping[V, C, K, T, |-|[T, MV, MC], O](this, key, component)

	/** Appends a new column Labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*) :ListingMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](name, LabeledSchemaColumn[N, T, O](name, buffs))

	/** Appends to this schema a new column Labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
			:ListingMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
		append[N, T, N @|| T](label, LabeledSchemaColumn[N, T, O](label, name, buffs))




	private[schema] def asPrefix[T <: Listing.Item] :MappingSchema[V |~ T, V, C, O] =
		new ChainPrefixSchema[V |~ T, V, C, O](this)

}






object ListingMapping {

	def apply[O] :FlatListingMapping[@~, @~, O] = empty.asInstanceOf[FlatListingMapping[@~, @~, O]]

	private[this] val empty = new EmptyListing[Any]



	trait FlatListingMapping[V <: Listing, C <: Chain, O]
		extends ListingMapping[V, C, O] with BaseFlatChainMapping[V, C, O]
	{
		override val schema :FlatListingMapping[V, C, O] = this

		protected def col[K <: Key, T, M <: ||[T]](key :K, component :M)
				:FlatListingMapping[V |~ (K :~ T), C ~ M, O] =
			new NonEmptyFlatListingMapping[V, C, K, T, M, O](this, key, component)


		override def col[N <: Label, T :ColumnForm](name :N, buffs :Buff[T]*)
				:FlatListingMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](name, LabeledSchemaColumn(name, buffs))

		override def col[N <: Label, T :ColumnForm](label :N, name :String, buffs :Buff[T]*)
				:FlatListingMapping[V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T, N @|| T](label, LabeledSchemaColumn(label, name, buffs))



		private[schema] override def asPrefix[T <: Item] :FlatMappingSchema[V |~ T, V, C, O] =
			new FlatChainPrefixSchema[V |~ T, V, C, O](this)
	}






	private class EmptyListing[O] extends EmptySchema[@~, O] with ListingMapping[@~, @~, O]



	private class NonEmptyListingSchema[S, V <: Listing, C <: Chain,
		                                K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                   (first :MappingSchema[S, V, C, O], key :K, next :M,
	                                    extract :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[Listing, Item, |~, S, V, C, T, K :~ T, M, O](first, next, extract, _.last.value)
	{
		protected override def link(init :V, last :T) :V |~ (K :~ T) = init |~ key :~ last

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyListingSchema(init compose extractor, key, last, this.extractor compose extractor)
	}



	private class NonEmptyFlatListingSchema[S, V <: Listing, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                       (override val init :FlatMappingSchema[S, V, C, O], key :K, next :M,
	                                                     extract :MappingExtract[S, T, O])
		extends NonEmptyListingSchema[S, V, C, K, T, M, O](init, key, next, extract)
		   with BaseNonEmptyFlatSchema[Listing, Item, |~, S, V, C, T, K :~ T, M, O]
		   with FlatMappingSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		override val selectForm = (init.selectForm |~ key :~ last.selectForm)(new ValueOf(key))
		override val filterForm = init.filterForm |~ key :~ last.filterForm
		override val insertForm = init.insertForm |~ key :~ last.insertForm
		override val updateForm = init.updateForm |~ key :~ last.updateForm
		protected override def newWriteForm(op :WriteOperationView) :SQLWriteForm[V |~ (K :~ T)] = op.form(this)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyFlatListingSchema(init compose extractor, key, last, this.extractor compose extractor)
	}





	private class NonEmptyListingMapping[V <: Listing, C <: Chain, K <: Key, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                    (prefix :ListingMapping[V, C, O], key :K, next :M)
		extends NonEmptyListingSchema[V |~ (K :~ T), V, C, K, T, M, O](
		                            prefix.asPrefix[K :~ T], key, next,
		                            MappingExtract.req(next.refine.withOrigin[O]) {
			                            (row :V |~ (K :~ T)) => row.last.value
		                            })
		   with ListingMapping[V |~ (K :~ T), C ~ M, O]



	private class NonEmptyFlatListingMapping[V <: Listing, C <: Chain, K <: Key, T, M <: ||[T], O]
	                                        (prefix :FlatListingMapping[V, C, O], key :K, next :M)
		extends NonEmptyFlatListingSchema[V |~ (K :~ T), V, C, K, T, M, O](
		                                prefix.asPrefix[K :~ T], key, next,
		                                ColumnExtract.req(next.withOrigin[O])((row :V |~ (K :~ T)) => row.last.value))
		   with FlatListingMapping[V |~ (K :~ T), C ~ M, O]
	
	
}


