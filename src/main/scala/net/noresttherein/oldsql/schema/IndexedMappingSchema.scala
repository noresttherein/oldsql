package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema, NonEmptyFlatSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.IndexedMappingSchema.{FlatIndexedMappingSchema, NonEmptyIndexedSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, @||, ||, FlatSchemaMapping, LabeledSchemaColumn, MappedSchema, OptMappedSchema}
import net.noresttherein.oldsql.schema.IndexedSchemaMapping.{FlatIndexedSchemaMapping, MappedIndexedSchema, OptMappedIndexSchema}




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
		append[N, T, @~ ~ T, @~ ~ ||[T], @||[N, T]](label, column, ColumnExtract.req(column)(value))
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
		append[N, T, @~ ~ T, @~ ~ ||[T], @||[N, T]](label, column, ColumnExtract.opt(column)(value))
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
			new MappedIndexedSchema[S, V, C, O](this, constructor) with FlatIndexedSchemaMapping[S, V, C, O] {
				override val schema = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S])
				:FlatIndexedSchemaMapping[S, V, C, O] =
			map(apply(constructor, _))

		override def optMap(constructor :V => Option[S]) :FlatIndexedSchemaMapping[S, V, C, O] =
			new OptMappedIndexSchema[S, V, C, O](this, constructor) with FlatIndexedSchemaMapping[S, V, C, O] {
				override val schema = outer
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

		override def compose[X](extractor :X => S) :NonEmptyFlatIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyFlatIndexedSchema(init compose extractor, label, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptyFlatIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyFlatIndexedSchema(init compose extractor, label, last, this.extractor compose extractor)
	}


}






trait IndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O] extends SchemaMapping[S, V, C, O] {
	override val schema :IndexedMappingSchema[S, V, C, O]
}






object IndexedSchemaMapping {

	@inline def apply[S] :FlatIndexedMappingSchema[S, @~, @~, _] = IndexedMappingSchema[S, Any]


	trait FlatIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
		extends IndexedSchemaMapping[S, V, C, O] with FlatSchemaMapping[S, V, C, O]
	{
		override val schema :FlatIndexedMappingSchema[S, V, C, O]
	}



	private[schema] class MappedIndexedSchema[S, V <: LiteralIndex, C <: Chain, O]
	                      (override val schema :IndexedMappingSchema[S, V, C, O], constructor :V => S)
		extends MappedSchema[S, V, C, O](schema, constructor) with IndexedSchemaMapping[S, V, C, O]



	private[schema] class OptMappedIndexSchema[S, V <: LiteralIndex, C <: Chain, O]
	                      (override val schema :IndexedMappingSchema[S, V, C, O], constructor :V => Option[S])
		extends OptMappedSchema[S, V, C, O](schema, constructor)
		   with IndexedSchemaMapping[S, V, C, O]

}






abstract class AbstractIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
                                           (contents :IndexedMappingSchema[S, V, C, O])
	extends AbstractSchemaMapping[S, V, C, O](contents) with IndexedSchemaMapping[S, V, C, O]
{
	implicit override val schema :IndexedMappingSchema[S, V, C, O] = contents
}



abstract class AbstractFlatIndexedSchemaMapping[S, V <: LiteralIndex, C <: Chain, O]
                                               (contents :FlatIndexedMappingSchema[S, V, C, O])
	extends AbstractIndexedSchemaMapping[S, V, C, O](contents) with FlatIndexedSchemaMapping[S, V, C, O]
{
	implicit override val schema :FlatIndexedMappingSchema[S, V, C, O] = contents
}

