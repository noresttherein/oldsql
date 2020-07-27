package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema, NonEmptyFlatSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.IndexedMappingSchema.{FlatIndexedMappingSchema, NonEmptyIndexedSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, @||, ||, FlatSchemaMapping, FlatSchemaMappingAdapter, FlatSchemaMappingProxy, LabeledSchemaColumn, MappedSchema, MappedSchemaMapping, MappingSchemaDelegate, OptMappedSchema, SchemaMappingAdapter, SchemaMappingProxy, StaticSchemaMapping}
import net.noresttherein.oldsql.schema.IndexedSchemaMapping.{DelegateIndexedSchemaMapping, FlatIndexedSchemaMapping, FlatIndexedSchemaMappingAdapter, FlatIndexedSchemaMappingProxy, IndexedSchemaMappingAdapter, IndexedSchemaMappingProxy, MappedFlatIndexedSchemaMapping, MappedIndexedSchema, MappedIndexedSchemaMapping, OptMappedIndexSchema}
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{AdapterFactoryMethods, BaseAdapter, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, MappedMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters
import net.noresttherein.oldsql.schema.Buff.{BuffType, FlagBuffType}
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.DelegateMapping
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.WriteOperationType




/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] variant where all components are indexed
  * with `String` literals for access.
  * @author Marcin Mościcki
  */
trait IndexedMappingSchema[S, V <: LiteralIndex, C <: Chain, O] extends MappingSchema[S, V, C, O] {
	//fixme: make labels unique
	//todo: extract the extensible part

//	override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
//			:IndexedMappingSchema[S, I, P, O]


	protected def append[K <: Label, T, MV <: Chain, MC <: Chain, M <: @|-|[K, T, MV, MC]](label :K, component: M, value :S =?> T)
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ M, O] =
		new NonEmptyIndexedSchema[S, V, C, K, T, M, O](
			this, label, component, MappingExtract(component.withOrigin[O])(value)
		)

	/** Appends the given component to this schema.
	  * @param label a string literal used to indexed the components and their values.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value an extract returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, component: @|-|[K, T, MV, MC], value :Extractor[S, T])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, component, value)

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value an extract returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[K <: Label :ValueOf, T, MV <: Chain, MC <: Chain](component: @|-|[K, T, MV, MC], value :Extractor[S, T])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](valueOf[K], component, value)


	/** Appends the given component to this schema.
	  * @param label a string literal used to indexed the components and their values.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, value :S => T, component: @|-|[K, T, MV, MC])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, component, MappingExtract.req(component.withOrigin[O])(value))

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
	  */
	def comp[K <: Label :ValueOf, T, MV <: Chain, MC <: Chain](value :S => T, component: @|-|[K, T, MV, MC])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](valueOf[K], component, MappingExtract.req(component.withOrigin[O])(value))

	/** Appends the given component to this schema.
	  * @param label a string literal used to indexed the components and their values.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
	  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
	  *              of the component's columns.
	  */
	def optcomp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, value :S => Option[T], component: @|-|[K, T, MV, MC])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, component, MappingExtract.opt(component.withOrigin[O])(value))

	/** Appends the given component to this schema.
	  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
	  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
	  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
	  *              of the component's columns.
	  */
	def optcomp[K <: Label :ValueOf, T, MV <: Chain, MC <: Chain](value :S => Option[T], component: @|-|[K, T, MV, MC])
			:IndexedMappingSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		append[K, T, MV, MC, @|-|[K, T, MV, MC]](valueOf[K], component, MappingExtract.opt(component.withOrigin[O])(value))



	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param value an extract function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
			:IndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		col(name, name, value, buffs:_*)

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label the label used to access the column in the schema.
	  * @param name the name of the column.
	  * @param value an extract function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def col[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
			:IndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
	{
		val column = LabeledSchemaColumn[N, T, O](label, name, buffs :_*)
		append[N, T, @~, @~, @||[N, T]](label, column, ColumnExtract.req(column)(value))
	}

	/** Appends a new column labeled with its name to this schema.
	  * @param name a string literal with the name of the column.
	  * @param value an extract function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def optcol[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
			:IndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		optcol[N, T](name, name, value, buffs :_*)

	/** Appends to this schema a new column labeled with a string different from its name.
	  * @param label a string literal used to access the column in the schema.
	  * @param name the name of the column.
	  * @param value an extract function returning the value for the column from the enclosing mapping's subject `S`.
	  * @param buffs a vararg list of buffs modifying the handling of the column.
	  * @tparam N the singleton type of the string literal used as the column name.
	  * @tparam T the mapped column type.
	  */
	def optcol[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
			:IndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
	{
		val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
		append[N, T, @~, @~, @||[N, T]](label, column, ColumnExtract.opt(column)(value))
	}






	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	override def map(constructor :V => S) :IndexedSchemaMapping[S, V, C, O] =
		new MappedIndexedSchema(this, constructor)

	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. This will result in slightly more efficient
	  * assembly than the other overloaded `map` method, as no chain with the values of all components will be assembled
	  * as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components as listed by the chain `V`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	override def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S]) :IndexedSchemaMapping[S, V, C, O] =
		map(apply(constructor, _))



	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may
	  * not produce the subject value for all input rows.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	override def optMap(constructor :V => Option[S]) :IndexedSchemaMapping[S, V, C, O] =
		new OptMappedIndexSchema(this, constructor)

	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
	  * the subject value for all input rows. This will result in slightly more efficient assembly than the other
	  * overloaded `flatMap` method, as no chain with the values of all components will be assembled as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components as listed by the chain `V`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	override def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]])
			:IndexedSchemaMapping[S, V, C, O] =
		optMap(apply(constructor, _))



	override def compose[X](extractor :X => S) :IndexedMappingSchema[X, V, C, O]

	override def compose[X](extractor :X =?> S) :IndexedMappingSchema[X, V, C, O]

}






object IndexedMappingSchema {

	def apply[S, O] :FlatIndexedMappingSchema[S, @~, @~, O] = empty.asInstanceOf[EmptyIndexedSchema[S, O]]

	private[this] val empty = new EmptyIndexedSchema[Any, Any]



	trait FlatIndexedMappingSchema[S, V <: LiteralIndex, C <: Chain, O]
		extends FlatMappingSchema[S, V, C, O] with IndexedMappingSchema[S, V, C, O]
	{ outer =>

//		override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
//				:FlatIndexedMappingSchema[S, I, P, O]



		override def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T](name, name, value, buffs :_*)


		override def col[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
			new NonEmptyFlatIndexedSchema(
				this, label, column, MappingExtract(column)(Extractor.req(value))
			)
		}



		override def optcol[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
			optcol(name, name, value, buffs:_*)


		override def optcol[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, buffs:_*)
			new NonEmptyFlatIndexedSchema(
				this, label, column, MappingExtract(column)(Extractor(value))
			)
		}



		override def map(constructor :V => S) :FlatIndexedSchemaMapping[S, V, C, O] =
			new MappedIndexedSchema[S, V, C, O](this, constructor) with FlatIndexedSchemaMapping[S, V, C, O]
				with MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
			{//this override is only to narrow the type, the property is initialized by the extended class to the same value
				override val backer = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S])
				:FlatIndexedSchemaMapping[S, V, C, O] =
			map(apply(constructor, _))

		override def optMap(constructor :V => Option[S]) :FlatIndexedSchemaMapping[S, V, C, O] =
			new OptMappedIndexSchema[S, V, C, O](this, constructor) with FlatIndexedSchemaMapping[S, V, C, O]
				with MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
			{//this override is only to narrow the type, the property is initialized by the extended class to the same value
				override val backer = outer
			}

		override def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]]) :FlatIndexedSchemaMapping[S, V, C, O] =
			optMap(apply(constructor, _))


		override def compose[X](extractor :X => S) :FlatIndexedMappingSchema[X, V, C, O]

		override def compose[X](extractor :X =?> S) :FlatIndexedMappingSchema[X, V, C, O]

	}






	private[schema] class EmptyIndexedSchema[S, O] extends EmptySchema[S, O] with FlatIndexedMappingSchema[S, @~, @~, O] {


//		override def prev[I <: Chain, P <: Chain](implicit vals: @~ <:< (I ~ Any), comps: @~ <:< (P ~ Any))
//				:FlatIndexedMappingSchema[S, I, P, O] =
//			throw new UnsupportedOperationException("EmptyIndexedSchema.prev")

		override def compose[X](extractor :X => S) :EmptyIndexedSchema[X, O] = this.asInstanceOf[EmptyIndexedSchema[X, O]]

		override def compose[X](extractor :X =?> S) :EmptyIndexedSchema[X, O] = this.asInstanceOf[EmptyIndexedSchema[X, O]]
	}



	private[schema] class NonEmptyIndexedSchema[S, V <: LiteralIndex, C <: Chain,
	                                            K <: Label, T, M <: @|-|[K, T, _ <: Chain, _ <: Chain], O]
	                                           (override val init :IndexedMappingSchema[S, V, C, O],
	                                            protected val label :K, component :M, selector :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[LiteralIndex, Label :~ Any, |~, S, V, C, T, K :~ T, M, O](
		                           init, component, selector, _.last.value)
		   with IndexedMappingSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		protected override def link(init :V, last :T) :V |~ (K :~ T) = init |~ label :~ last


//		override prev[I <: Chain, P <: Chain](implicit cs :C ~ M <:< (P ~ Any), vs :V |~ (K :~ T) <:< (I ~ Any))
//				:IndexedMappingSchema[S, I, P, O] =
//			init.asInstanceOf[IndexedMappingSchema[S, I, P, O]]

		override def compose[X](extractor :X => S) :NonEmptyIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyIndexedSchema(init compose extractor, label, component, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptyIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyIndexedSchema(init compose extractor, label, component, this.extractor compose extractor)
	}



	private[schema] class NonEmptyFlatIndexedSchema[S, V <: LiteralIndex, C <: Chain,
	                                                K <: Label, T, M <: @||[K, T], O]
	                                               (override val init :FlatIndexedMappingSchema[S, V, C, O],
	                                                key :K, comp :M, extractor :MappingExtract[S, T, O])
		extends NonEmptyIndexedSchema[S, V, C, K, T, M, O](init, key, comp, extractor)
		   with BaseNonEmptyFlatSchema[LiteralIndex, Label :~ Any, |~, S, V, C, T, K :~ T, M, O]
		   with FlatIndexedMappingSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		//these shortcut implementations work because column mappings moved their buff handling to their forms.
		override val selectForm =
			SQLReadForm.LiteralIndexReadForm(init.selectForm, new ValueOf(key), component.selectForm)
		override val queryForm = SQLWriteForm.LiteralIndexWriteForm(init.queryForm, component.queryForm)
		override val updateForm = SQLWriteForm.LiteralIndexWriteForm(init.updateForm, component.updateForm)
		override val insertForm = SQLWriteForm.LiteralIndexWriteForm(init.insertForm, component.insertForm)
		override def writeForm(op :WriteOperationType) = op.form(this)

		override def compose[X](extractor :X => S) :NonEmptyFlatIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyFlatIndexedSchema(init compose extractor, label, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptyFlatIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyFlatIndexedSchema(init compose extractor, label, last, this.extractor compose extractor)
	}


}






trait IndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
	extends SchemaMapping[S, V, C, O] with AdapterFactoryMethods[({ type A[X] = IndexedSchemaMapping[X, V, C, O] })#A, S, O]
{
	override val schema :IndexedMappingSchema[S, V, C, O]


	protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:IndexedSchemaMapping[S, V, C, O] =
		new CustomizedMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with IndexedSchemaMappingProxy[this.type, S, V, C, O]


	override def prefixed(prefix :String) :IndexedSchemaMapping[S, V, C, O] =
		if (prefix.length == 0)
			this
		else
			new PrefixedMapping[this.type, S, O](prefix, this) with DelegateIndexedSchemaMapping[S, V, C, O]

	override def renamed(name :String) :IndexedSchemaMapping[S, V, C, O] =
		new RenamedMapping[this.type, S, O](name, this) with DelegateIndexedSchemaMapping[S, V, C, O]


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :IndexedSchemaMapping[X, V, C, O] =
		new MappedIndexedSchemaMapping[IndexedSchemaMapping[S, V, C, O], S, X, V, C, O](this, there, back)
}






object IndexedSchemaMapping {

	@inline def apply[S] :FlatIndexedMappingSchema[S, @~, @~, _] = IndexedMappingSchema[S, Any]



	trait FlatIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
		extends IndexedSchemaMapping[S, V, C, O] with FlatSchemaMapping[S, V, C, O]
		   with AdapterFactoryMethods[({ type A[X] = FlatIndexedSchemaMapping[X, V, C, O] })#A, S, O]
	{
		override val schema :FlatIndexedMappingSchema[S, V, C, O]


		protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatIndexedSchemaMapping[S, V, C, O] =
			new CustomizedMapping[this.type, S, O](this, op, include, exclude)
				with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]


		override def prefixed(prefix :String) :FlatIndexedSchemaMapping[S, V, C, O] =
			if (prefix.length == 0)
				this
			else
				new PrefixedMapping[this.type, S, O](prefix, this) with DelegateFlatIndexedSchemaMapping[S, V, C, O]

		override def renamed(name :String) :FlatIndexedSchemaMapping[S, V, C, O] =
			new RenamedMapping[this.type, S, O](name, this) with DelegateFlatIndexedSchemaMapping[S, V, C, O]


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:FlatIndexedSchemaMapping[X, V, C, O] =
			new MappedFlatIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
	}






	trait IndexedSchemaMappingAdapter[+M <: RefinedMapping[T, O], T, S, V <: LiteralIndex, C <: Chain, O]
		extends IndexedSchemaMapping[S, V, C, O] with SchemaMappingAdapter[M, T, S, V, C, O]
		   with AdapterFactoryMethods[({ type A[X] = IndexedSchemaMappingAdapter[M, T, X, V, C, O] })#A, S, O]
	{
		protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:IndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			new CustomizedMapping[this.type, S, O](this, op, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateIndexedSchemaMapping[S, V, C, O]
				with IndexedSchemaMappingAdapter[M, T, S, V, C, O]


		override def prefixed(prefix :String) :IndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			if (prefix.length == 0)
				this
			else
                new PrefixedMapping[this.type, S, O](prefix, this)
	                with ComposedAdapter[M, S, S, O] with DelegateIndexedSchemaMapping[S, V, C, O]
					with IndexedSchemaMappingAdapter[M, T, S, V, C, O]

		override def renamed(name :String) :IndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			new RenamedMapping[this.type, S, O](name, this)
				with ComposedAdapter[M, S, S, O] with DelegateIndexedSchemaMapping[S, V, C, O]
				with IndexedSchemaMappingAdapter[M, T, S, V, C, O]



		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:IndexedSchemaMappingAdapter[M, T, X, V, C, O] =
			new MappedIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
				with ComposedAdapter[M, S, X, O] with IndexedSchemaMappingAdapter[M, T, X, V, C, O]
			{
				override def as[Z](there :X =?> Z, back :Z =?> X)(implicit nulls :NullValue[Z])
						:IndexedSchemaMappingAdapter[M, T, Z, V, C, O] =
					backer.as(map andThen there, back andThen unmap)
			}
	}



	trait IndexedSchemaMappingProxy[M <: IndexedSchemaMapping[S, V, C, O], S, V <: LiteralIndex, C <: Chain, O]
		extends IndexedSchemaMappingAdapter[M, S, S, V, C, O] with SchemaMappingProxy[M, S, V, C, O]
	{
		override val schema = body.schema
	}



	trait FlatIndexedSchemaMappingAdapter[M <: RefinedMapping[T, O], T, S, V <: LiteralIndex, C <: Chain, O]
		extends FlatIndexedSchemaMapping[S, V, C, O] with IndexedSchemaMappingAdapter[M, T, S, V, C, O]
		   with FlatSchemaMappingAdapter[M, T, S, V, C, O]
		   with AdapterFactoryMethods[({ type A[X] = FlatIndexedSchemaMappingAdapter[M, T, X, V, C, O] })#A, S, O]
	{
		protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			new CustomizedMapping[this.type, S, O](this, op, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateFlatIndexedSchemaMapping[S, V, C, O]
				with FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O]


		override def prefixed(prefix :String) :FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			if (prefix.length == 0)
				this
			else
				new PrefixedMapping[this.type, S, O](prefix, this)
					with ComposedAdapter[M, S, S, O] with DelegateFlatIndexedSchemaMapping[S, V, C, O]
					with FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O]

		override def renamed(name :String) :FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O] =
			new RenamedMapping[this.type, S, O](name, this)
				with ComposedAdapter[M, S, S, O] with DelegateFlatIndexedSchemaMapping[S, V, C, O]
				with FlatIndexedSchemaMappingAdapter[M, T, S, V, C, O]


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:FlatIndexedSchemaMappingAdapter[M, T, X, V, C, O] =
			new MappedFlatIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
				with FlatIndexedSchemaMappingAdapter[M, T, X, V, C, O] with ComposedAdapter[M, S, X, O]
			{
				override def as[Z](there :X =?> Z, back :Z =?> X)(implicit nulls :NullValue[Z])
						:FlatIndexedSchemaMappingAdapter[M, T, Z, V, C, O] =
					backer.as(map andThen there, back andThen unmap)
			}
	}



	trait FlatIndexedSchemaMappingProxy[M <: FlatIndexedSchemaMapping[S, V, C, O], S, V <: LiteralIndex, C <: Chain, O]
		extends FlatIndexedSchemaMappingAdapter[M, S, S, V, C, O] with IndexedSchemaMappingProxy[M, S, V, C, O]
		   with FlatSchemaMappingProxy[M, S, V, C, O]
	{
		override val schema = body.schema
	}






	private trait DelegateIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
		extends DelegateMapping[IndexedSchemaMapping[S, V, C, O], S, O] with IndexedSchemaMapping[S, V, C, O]
	{
		override val schema = backer.schema
	}

	private trait DelegateFlatIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
		extends DelegateMapping[FlatIndexedSchemaMapping[S, V, C, O], S, O] with FlatIndexedSchemaMapping[S, V, C, O]
	{
		override val schema = backer.schema
	}






	private[schema] class MappedIndexedSchemaMapping[+M <: IndexedSchemaMapping[T, V, C, O], T,
		                                             S, V <: LiteralIndex, C <: Chain, O]
	                      (protected override val backer :M,
	                       protected override val map :T =?> S, protected override val unmap :S =?> T)
	                      (implicit protected override val nulls :NullValue[S])
		extends MappedMapping[T, S, O] with IndexedSchemaMapping[S, V, C, O]
	{
		override val schema :IndexedMappingSchema[S, V, C, O] = backer.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:IndexedSchemaMapping[X, V, C, O] =
			new MappedIndexedSchemaMapping[M, T, X, V, C, O](backer, map andThen there, back andThen unmap)(
			                                                 nullValue extract there)
	}



	private[schema] class MappedFlatIndexedSchemaMapping[+M <: FlatIndexedSchemaMapping[T, V, C, O], T,
		                                                 S, V <: LiteralIndex, C <: Chain, O]
	                      (protected override val backer :M,
	                       protected override val map :T =?> S, protected override val unmap :S =?> T)
	                      (implicit protected override val nulls :NullValue[S])
		extends MappedMapping[T, S, O] with FlatIndexedSchemaMapping[S, V, C, O]
	{
		override val schema :FlatIndexedMappingSchema[S, V, C, O] = backer.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:FlatIndexedSchemaMapping[X, V, C, O] =
		{
			val assemble = map andThen there
			val disassemble = back andThen unmap
			new MappedFlatIndexedSchemaMapping[M, T, X, V, C, O](backer, assemble, disassemble)(nullValue extract there)
		}
	}






	private[schema] class MappedIndexedSchema[S, V <: LiteralIndex, C <: Chain, O]
	                      (protected override val backer :IndexedMappingSchema[S, V, C, O], constructor :V => S)
		extends MappedSchema[S, V, C, O](backer, constructor) with IndexedSchemaMapping[S, V, C, O]
		   with MappingSchemaDelegate[IndexedMappingSchema[S, V, C, O], S, V, C, O]



	private[schema] class OptMappedIndexSchema[S, V <: LiteralIndex, C <: Chain, O]
	                      (protected override val backer :IndexedMappingSchema[S, V, C, O], constructor :V => Option[S])
		extends OptMappedSchema[S, V, C, O](backer, constructor) with IndexedSchemaMapping[S, V, C, O]
		   with MappingSchemaDelegate[IndexedMappingSchema[S, V, C, O], S, V, C, O]

}






abstract class AbstractIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
                                           (protected override val backer :IndexedMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[IndexedMappingSchema[S, V, C, O], S, V, C, O] with IndexedSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
			({ type A[M <: RefinedMapping[S, O], X] = IndexedSchemaMappingAdapter[M, S, X, V, C, O] })#A,
			IndexedMappingSchema[S, V, C, O], S, V, C, O]
{
	protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new CustomizedMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with IndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def prefixed(prefix :String) :IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[this.type, S, O](prefix, this)
			with DelegateAdapter[this.type, S, O] with IndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def renamed(name :String) :IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[this.type, S, O](name, this)
			with DelegateAdapter[this.type, S, O] with IndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:IndexedSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with IndexedSchemaMappingAdapter[this.type, S, X, V, C, O]

}






abstract class AbstractFlatIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
                                               (protected override val backer :FlatIndexedMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
	   with FlatIndexedSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
			({ type A[M <: RefinedMapping[S, O], X] = FlatIndexedSchemaMappingAdapter[M, S, X, V, C, O] })#A,
			FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
{
	protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new CustomizedMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def prefixed(prefix :String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[this.type, S, O](prefix, this)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def renamed(name :String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[this.type, S, O](name, this)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedFlatIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O]
}

