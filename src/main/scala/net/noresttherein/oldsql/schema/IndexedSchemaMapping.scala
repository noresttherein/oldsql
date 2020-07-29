package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, LiteralIndex}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~, UniqueKey}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.MappingSchema.{BaseNonEmptyFlatSchema, BaseNonEmptySchema, EmptySchema, FlatMappingSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.IndexedMappingSchema.{ExtensibleFlatIndexedSchema, FlatIndexedMappingSchema}
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, @||, |-|, FlatSchemaMapping, FlatSchemaMappingAdapter, FlatSchemaMappingProxy, LabeledSchemaColumn, MappedSchema, MappingSchemaDelegate, OptMappedSchema, SchemaMappingAdapter, SchemaMappingProxy, StaticSchemaMapping}
import net.noresttherein.oldsql.schema.IndexedSchemaMapping.{DelegateIndexedSchemaMapping, FlatIndexedSchemaMapping, FlatIndexedSchemaMappingAdapter, FlatIndexedSchemaMappingProxy, IndexedSchemaMappingAdapter, IndexedSchemaMappingProxy, MappedFlatIndexedSchemaMapping, MappedIndexedSchema, MappedIndexedSchemaMapping, OptMappedIndexSchema}
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{AdapterFactoryMethods, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, MappedMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.Mapping.RefinedMapping
import net.noresttherein.oldsql.schema.support.DelegateMapping
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.WriteOperationType




/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] variant where all components are indexed
  * with `String` literals for access.
  * @author Marcin Mościcki
  */
trait IndexedMappingSchema[S, V <: LiteralIndex, C <: Chain, O] extends MappingSchema[S, V, C, O] {
	override def compose[X](extractor :X =?> S) :IndexedMappingSchema[X, V, C, O]
}






object IndexedMappingSchema {

	/** An empty, extensible component list to be used as a schema for a mapping of the subject `S`.
	  * The `FlatIndexedMappingSchema` is a `Mapping`
	  * for a [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]] with the values of all its components
	  * indexed by `String` literals for ease of access.
	  * Provides chaining methods for appending new components (in an immutable way) and, once the component list
	  * is complete, mapping their values collected in a `LiteralIndex` to the subject `S`, creating
	  * a [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] in the process.
	  */
	def apply[S, O] :ExtensibleFlatIndexedSchema[S, @~, @~, O] = empty.asInstanceOf[EmptyIndexedSchema[S, O]]

	/** An empty, extensible component list to be used as a schema for a mapping of the subject `S`.
	  * The `FlatIndexedMappingSchema` is a `Mapping`
	  * for a [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]] with the values of all its components
	  * indexed by `String` literals for ease of access.
	  * Provides chaining methods for appending new components (in an immutable way) and, once the component list
	  * is complete, mapping their values collected in a `LiteralIndex` to the subject `S`, creating
	  * a [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] in the process.
	  * @param buffs buffs intended for the built `IndexedSchemaMapping`. They are ''not'' the buffs of the intermediate
	  *              schema mappings, but are inherited by all added columns as well as components created by the use
	  *              of given factory functions.
	  */
	def apply[S, O](buffs :Buff[S]*) :ExtensibleFlatIndexedSchema[S, @~, @~, O] =
		new EmptyIndexedSchema[S, O]("", buffs)

	/** An empty, extensible component list to be used as a schema for a mapping of the subject `S`.
	  * The `FlatIndexedMappingSchema` is a `Mapping`
	  * for a [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]] with the values of all its components
	  * indexed by `String` literals for ease of access.
	  * Provides chaining methods for appending new components (in an immutable way) and, once the component list
	  * is complete, mapping their values collected in a `LiteralIndex` to the subject `S`, creating
	  * a [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] in the process.
	  * @param columnPrefix a prefix `String` which will be prepended to the names of all columns added to this
	  *                     schema. This prefix will be also passed to all components created by the use
	  *                     of given factory functions.
	  * @param buffs buffs intended for the built `IndexedSchemaMapping`. They are ''not'' the buffs of the intermediate
	  *              schema mappings, but are inherited by all added columns as well as components created by the use
	  *              of given factory functions.
	  */
	def apply[S, O](columnPrefix :String, buffs :Buff[S]*) :ExtensibleFlatIndexedSchema[S, @~, @~, O] =
		new EmptyIndexedSchema[S, O](columnPrefix, buffs)

	private[this] val empty = new EmptyIndexedSchema[Any, Any]



	/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] variant where all components are columns
	  * indexed with `String` literals for access.
	  */
	trait FlatIndexedMappingSchema[S, V <: LiteralIndex, C <: Chain, O]
		extends FlatMappingSchema[S, V, C, O] with IndexedMappingSchema[S, V, C, O]
	{
		override def compose[X](extractor :X =?> S) :FlatIndexedMappingSchema[X, V, C, O]
	}





	/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] where all components are labeled (indexed
	  * with `String` literals for access), providing factory methods for larger schemas by adding new components.
	  * Unlike [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleMappingSchema ExtensibleMappingSchema]],
	  * the chain with value types `V` must not be abstract when adding new components in order to guarantee the
	  * uniqueness of labels.
	  */
	trait ExtensibleIndexedSchema[S, V <: LiteralIndex, C <: Chain, O] extends IndexedMappingSchema[S, V, C, O] {
		protected def prefix :String

		private[IndexedMappingSchema] def columnPrefix :String = prefix


		protected[schema] def conveyBuffs[T](extractor :S => T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) packedBuffs.flatMap(_.cascade(extractor))
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: packedBuffs.flatMap(_.cascade(extractor))

		protected[schema] def conveyBuffs[T](extractor :S =?> T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) cascadeBuffs(packedBuffs, toString)(extractor)
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: cascadeBuffs(packedBuffs, toString)(extractor)



		protected def append[K <: Label, T, MV <: Chain, MC <: Chain, M <: @|-|[K, T, MV, MC]]
		                    (label :K, component: M, value :MappingExtract[S, T, O])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ M, O] =
			new NonEmptyIndexedSchema[S, V, C, K, T, M, O](this, label, component, value, prefix)

		/** Appends the given component to this schema.
		  * @param label a string literal used to indexed the components and their values.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component of any other mapping.
		  * @param value an extract returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, component: |-|[T, MV, MC], value :S =?> T)
		                                                 (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		{
			val labeled = component labeled[K] label
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, labeled, MappingExtract(labeled.withOrigin[O])(value))
		}

		/** Appends the given component to this schema.
		  * @param component a labeled `SchemaMapping` of any origin type to add as the component.
		  *                  It must be a unique object, not appearing as a component of any other mapping.
		  * @param value an extract returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain](component: @|-|[K, T, MV, MC], value :S =?> T)
		                                                 (implicit label :ValueOf[K], unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](label.value, component, MappingExtract(component.withOrigin[O])(value))



		/** Appends a new labeled component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The label can be used to access the component by passing it as the argument
		  * to the [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/, /]] method.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema.
		  *              The label must be not be already present in the value indexed chain `V` in order to uniquely
		  *              identify the component and provide a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @param unique an implicit witness attesting that type `K` is not a member of any `K :~ _` pair from
		  *               the `LiteralIndex` of component values `V`.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain]
		        (label :K, value :S => T, buffs :Buff[T]*)(component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
		        (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			comp(label, value, component(prefix, conveyBuffs(value, buffs)))

		/** Appends a new labeled component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The label can be used to access the component by passing it as the argument
		  * to the [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/, /]] method.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema.
		  *              The label must be not be already present in the value indexed chain `V` in order to uniquely
		  *              identify the component and provide a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param columnPrefix a string which should be passed to the given factory function as a prefix
		  *                     which should be prepended to all columns of the component created
		  *                     by the function `component`. This prefix will be first appended to the column prefix
		  *                     specified for all components of this schema at the creation of the empty schema
		  *                     from which this instance derives itself.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @param unique an implicit witness attesting that type `K` is not a member of any `K :~ _` pair from
		  *               the `LiteralIndex` of component values `V`.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain]
		        (label :K, value :S => T, columnPrefix :String, buffs :Buff[T]*)
		        (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
		        (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			comp(label, value, component(prefix + columnPrefix, conveyBuffs(value, buffs)))

		/** Appends the given component to this schema.
		  * @param label a string literal used to indexed the components and their values.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component of any other mapping.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, value :S => T, component: |-|[T, MV, MC])
		                                                 (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		{
			val labeled = component labeled[K] label
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, labeled, MappingExtract.req(labeled.withOrigin[O])(value))
		}

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component of any other mapping.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[K <: Label, T, MV <: Chain, MC <: Chain](value :S => T, component: @|-|[K, T, MV, MC])
		                                                 (implicit label :ValueOf[K], unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](
				label.value, component, MappingExtract.req(component.withOrigin[O])(value)
			)



		/** Appends a new labeled, optional component to this schema. The component will inherit any column prefix
		  * and all buffs provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method. The label can be used to access the component
		  * by passing it as the argument to the
		  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/, /]] method. The extractor function
		  * may not produce a value for all instances of the subject type `S`, in which case the component
		  * will be omitted from a database write. The impact its lack will have on the assembly of the ''packed'' value
		  * depends on the implementation of the outer mapping based on this schema.
		  * Mappings based directly on this schema, created with this instance's special `map` and `optMap` methods
		  * will not produce a value if any of the optional components is missing, as the value indexed chain `V`
		  * will be impossible to assemble.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema.
		  *              The label must be not be already present in the value indexed chain `V` in order to uniquely
		  *              identify the component and provide a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @param unique an implicit witness attesting that type `K` is not a member of any `K :~ _` pair from
		  *               the `LiteralIndex` of component values `V`.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[K <: Label, T, MV <: Chain, MC <: Chain]
		           (label :K, value :S => Option[T], buffs :Buff[T]*)(component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
		           (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			optcomp(label, value, component(prefix, conveyBuffs(Extractor.opt(value), buffs)))

		/** Appends a new labeled, optional component to this schema. The component will inherit any column prefix
		  * and all buffs provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method. The label can be used to access the component
		  * by passing it as the argument to the
		  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/, /]] method. The extractor function
		  * may not produce a value for all instances of the subject type `S`, in which case the component
		  * will be omitted from a database write. The impact its lack will have on the assembly of the ''packed'' value
		  * depends on the implementation of the outer mapping based on this schema.
		  * Mappings based directly on this schema, created with this instance's special `map` and `optMap` methods
		  * will not produce a value if any of the optional components is missing, as the value chain `V`
		  * will be impossible to assemble.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema.
		  *              The label must be not be already present in the value indexed chain `V` in order to uniquely
		  *              identify the component and provide a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param columnPrefix a string which should be passed to the given factory function as a prefix
		  *                     which should be prepended to all columns of the component created
		  *                     by the function `component`. This prefix will be first appended to the column prefix
		  *                     specified for all components of this schema at the creation of the empty schema
		  *                     from which this instance derives itself.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @param unique an implicit witness attesting that type `K` is not a member of any `K :~ _` pair from
		  *               the `LiteralIndex` of component values `V`.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[K <: Label, T, MV <: Chain, MC <: Chain]
		           (label :K, value :S => Option[T], columnPrefix :String, buffs :Buff[T]*)
		           (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
		           (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			optcomp(label, value, component(prefix + columnPrefix, conveyBuffs(Extractor.opt(value), buffs)))

		/** Appends the given component to this schema.
		  * @param label a string literal used to indexed the components and their values.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component of any other mapping.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[K <: Label, T, MV <: Chain, MC <: Chain](label :K, value :S => Option[T], component: |-|[T, MV, MC])
		                                                    (implicit unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
		{
			val labeled = component labeled[K] label
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](label, labeled, MappingExtract.opt(labeled.withOrigin[O])(value))
		}

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component of any other mapping.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[K <: Label, T, MV <: Chain, MC <: Chain](value :S => Option[T], component: @|-|[K, T, MV, MC])
		                                                    (implicit label :ValueOf[K], unique :UniqueKey[V, K])
				:ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ @|-|[K, T, MV, MC], O] =
			append[K, T, MV, MC, @|-|[K, T, MV, MC]](
				label.value, component, MappingExtract.opt(component.withOrigin[O])(value)
			)



		/** Appends a new column labeled with its name to this schema.
		  * @param name a string literal with the name of the column.
		  * @param value an extract function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
		                                  (implicit unique :UniqueKey[V, N])
				:ExtensibleIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
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
		                                  (implicit unique :UniqueKey[V, N])
				:ExtensibleIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, prefix + name, buffs :_*)
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
		                                     (implicit unique :UniqueKey[V, N])
				:ExtensibleIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
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
		                                     (implicit unique :UniqueKey[V, N])
				:ExtensibleIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, prefix + name, buffs:_*)
			append[N, T, @~, @~, @||[N, T]](label, column, ColumnExtract.opt(column)(value))
		}



		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions,
		  * provided with component and column definitions when building this schema, for disassembly of its subject `S`
		  * before writing to the database, and the function specified here for assembling its subject from the index
		  * of subjects of all top-level components of this schema. If any of the components in this schema
		  * is optional (was created with one of the `optcomp` and `optcol` methods) and does not produce a value
		  * during assembly, this function will not be called and the created mapping will likewise fail to produce
		  * a value from the passed [[net.noresttherein.oldsql.schema.Mapping#Pieces Pieces]].
		  * @param constructor a function accepting a chain with the values of all components as they appear in the
		  *                    components chain `C`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
		  */
		def map(constructor :V => S) :IndexedSchemaMapping[S, V, C, O] =
			new MappedIndexedSchema(this, constructor, packedBuffs)

		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions,
		  * provided with component and column definitions when building this schema, for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the index
		  * of subjects of all top-level components of this schema. This will result in slightly more efficient
		  * assembly than the other overloaded `map` method, as no chain with the values of all components will be assembled
		  * as an intermediate step. If any of the components in this schema
		  * is optional (was created with one of the `optcomp` and `optcol` methods) and does not produce a value
		  * during assembly, this function will not be called and the created mapping will likewise fail to produce
		  * a value from the passed [[net.noresttherein.oldsql.schema.Mapping#Pieces Pieces]].
		  * @param constructor a function which number of arguments and their types match the subject types of all
		  *                    components as listed by the chain `V`. The keys in the index are ignored.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
		  */
		def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S]) :IndexedSchemaMapping[S, V, C, O] =
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
		def optMap(constructor :V => Option[S]) :IndexedSchemaMapping[S, V, C, O] =
			new OptMappedIndexSchema(this, constructor, packedBuffs)

		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extract functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the
		  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
		  * the subject value for all input rows.
		  * @param constructor a function which number of arguments and their types match the subject types of all
		  *                    components as listed by the chain `V`. The keys in the index are ignored.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
		  */
		def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]])
				:IndexedSchemaMapping[S, V, C, O] =
			optMap(apply(constructor, _))

	}





	/** A [[net.noresttherein.oldsql.schema.MappingSchema MappingSchema]] where all components are labeled columns
	  * (indexed with `String` literals for access), providing factory methods for larger schemas by adding new columns.
	  */
	trait ExtensibleFlatIndexedSchema[S, V <: LiteralIndex, C <: Chain, O]
		extends ExtensibleIndexedSchema[S, V, C, O] with FlatIndexedMappingSchema[S, V, C, O]
	{ outer =>

		override def col[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
		                                           (implicit unique :UniqueKey[V, N])
				:ExtensibleFlatIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
			col[N, T](name, name, value, buffs :_*)


		override def col[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
		                                           (implicit unique :UniqueKey[V, N])
				:ExtensibleFlatIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, prefix + name, buffs:_*)
			new NonEmptyFlatIndexedSchema(
				this, label, column, MappingExtract(column)(Extractor.req(value)), prefix
			)
		}



		override def optcol[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
		                                              (implicit unique :UniqueKey[V, N])
				:ExtensibleFlatIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
			optcol(name, name, value, buffs:_*)


		override def optcol[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
		                                              (implicit unique :UniqueKey[V, N])
				:ExtensibleFlatIndexedSchema[S, V |~ (N :~ T), C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, prefix + name, buffs:_*)
			new NonEmptyFlatIndexedSchema(
				this, label, column, MappingExtract(column)(Extractor(value)), prefix
			)
		}



		override def map(constructor :V => S) :FlatIndexedSchemaMapping[S, V, C, O] =
			new MappedIndexedSchema[S, V, C, O](this, constructor, packedBuffs)
				with FlatIndexedSchemaMapping[S, V, C, O]
				with MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
			{//this override is only to narrow the type, the property is initialized by the extended class to the same value
				override val backer = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S])
				:FlatIndexedSchemaMapping[S, V, C, O] =
			map(apply(constructor, _))

		override def optMap(constructor :V => Option[S]) :FlatIndexedSchemaMapping[S, V, C, O] =
			new OptMappedIndexSchema[S, V, C, O](this, constructor, packedBuffs)
				with FlatIndexedSchemaMapping[S, V, C, O]
				with MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
			{//this override is only to narrow the type, the property is initialized by the extended class to the same value
				override val backer = outer
			}

		override def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]])
				:FlatIndexedSchemaMapping[S, V, C, O] =
			optMap(apply(constructor, _))

	}






	private[schema] class EmptyIndexedSchema[S, O]
	                      (protected override val prefix :String = "", protected override val packedBuffs :Seq[Buff[S]] = Nil)
		extends EmptySchema[S, O] with ExtensibleFlatIndexedSchema[S, @~, @~, O]
	{
		override def compose[X](extractor :X =?> S) :EmptyIndexedSchema[X, O] =
			new EmptyIndexedSchema(prefix)
	}



	private class NonEmptyIndexedSchema[S, V <: LiteralIndex, C <: Chain,
	                                    K <: Label, T, M <: @|-|[K, T, _ <: Chain, _ <: Chain], O]
	                                   (override val init :IndexedMappingSchema[S, V, C, O],
	                                    protected val label :K, component :M, selector :MappingExtract[S, T, O],
	                                    protected override val prefix :String)
		extends BaseNonEmptySchema[LiteralIndex, Label :~ Any, |~, S, V, C, T, K :~ T, M, O](
		                           init, component, selector, _.last.value)
		   with ExtensibleIndexedSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		protected override val packedBuffs = init.outerBuffs

		protected override def link(init :V, last :T) :V |~ (K :~ T) = init |~ label :~ last

		override def compose[X](extractor :X =?> S) :NonEmptyIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyIndexedSchema(init compose extractor, label, component, this.extractor compose extractor, prefix)
	}



	private class NonEmptyFlatIndexedSchema[S, V <: LiteralIndex, C <: Chain,
	                                        K <: Label, T, M <: @||[K, T], O]
	                                       (override val init :FlatIndexedMappingSchema[S, V, C, O],
	                                        key :K, comp :M, extractor :MappingExtract[S, T, O], prefix :String)
		extends NonEmptyIndexedSchema[S, V, C, K, T, M, O](init, key, comp, extractor, prefix)
		   with BaseNonEmptyFlatSchema[LiteralIndex, Label :~ Any, |~, S, V, C, T, K :~ T, M, O]
		   with ExtensibleFlatIndexedSchema[S, V |~ (K :~ T), C ~ M, O]
	{
		//these shortcut implementations work because column mappings moved their buff handling to their forms.
		override val selectForm =
			SQLReadForm.LiteralIndexReadForm(init.selectForm, new ValueOf(key), component.selectForm)
		override val queryForm = SQLWriteForm.LiteralIndexWriteForm(init.queryForm, component.queryForm)
		override val updateForm = SQLWriteForm.LiteralIndexWriteForm(init.updateForm, component.updateForm)
		override val insertForm = SQLWriteForm.LiteralIndexWriteForm(init.insertForm, component.insertForm)
		override def writeForm(op :WriteOperationType) = op.form(this)

		override def compose[X](extractor :X =?> S) :NonEmptyFlatIndexedSchema[X, V, C, K, T, M, O] =
			new NonEmptyFlatIndexedSchema(init compose extractor, label, last, this.extractor compose extractor, prefix)
	}


}






/** A `Mapping` which has all its components listed in its type as the `Chain` parameter `C`.
  * The added benefit over the standard [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]] is that
  * all components are labeled with unique `String` literal types. This allows easy access by the
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/ /]] method:
  * {{{
  *     val ownedSquirrels = this / "squirrelCount"
  * }}}.
  * @tparam S the subject type of this mapping.
  * @tparam V a `LiteralIndex` containing the types of all components in `C` in their exact order, indexed
  *           by the labels of the components.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order. All components
  *           are labeled mappings, instances of [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]].
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  *           Consult [[net.noresttherein.oldsql.schema.Mapping#Origin Mapping.Origin]]
  */
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

	/** Starts building a `IndexedSchemaMapping` of `S` by chaining methods adding new columns and components.
	  * Once the component list is complete, the [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]]
	  * containing their indexed values can be mapped into the 'packed' subject type `S`:
	  * {{{
	  *     IndexedSchemaMapping[PetKeeper].col("squirrels", _.squirrels).col("hamsters", _.hamsters).map {
	  *         index => PetKeeper(index("squirrels"), index("hamsters"))
	  *     }
	  * }}}
	  * This is equivalent to [[net.noresttherein.oldsql.schema.IndexedMappingSchema.apply IndexedMappingSchema[S, _]]],
	  * but the origin type is omitted with the intent of the constructed mapping being used as a
	  * [[net.noresttherein.oldsql.schema.SchemaMapping.|-| |-|]] mapping, included as a component in some larger
	  * [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * @return an empty, extensible [[net.noresttherein.oldsql.schema.IndexedMappingSchema IndexedMappingSchema]].
	  * @tparam S the subject type of th constructed mapping.
	  */
	@inline def apply[S] :ExtensibleFlatIndexedSchema[S, @~, @~, _] = IndexedMappingSchema[S, Any]

	/** Starts building a [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] of `S`
	  * by chaining methods adding new columns and components.
	  * Once the component list is complete, the [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]]
	  * containing their indexed values can be mapped into the 'packed' subject type `S`:
	  * {{{
	  *     IndexedSchemaMapping[PetKeeper].col("squirrels", _.squirrels).col("hamsters", _.hamsters).map {
	  *         index => PetKeeper(index("squirrels"), index("hamsters"))
	  *     }
	  * }}}
	  * This is equivalent to [[net.noresttherein.oldsql.schema.IndexedMappingSchema.apply IndexedMappingSchema[S, _]]],
	  * but the origin type is omitted with the intent of the constructed mapping being used as a
	  * [[net.noresttherein.oldsql.schema.SchemaMapping.|-| |-|]] mapping, included as a component in some larger
	  * [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * @param buffs the buffs for the constructed [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]],
	  *              inherited by all columns appended to the returned schema and those components which are created
	  *              with passed factory functions.
	  * @return an empty, extensible [[net.noresttherein.oldsql.schema.IndexedMappingSchema IndexedMappingSchema]].
	  * @tparam S the subject type of th constructed mapping.
	  */
	@inline def apply[S](buffs :Buff[S]*) :ExtensibleFlatIndexedSchema[S, @~, @~, _] =
		IndexedMappingSchema[S, Any](buffs :_*)

	/** Starts building a [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]] of `S`
	  * by chaining methods adding new columns and components.
	  * Once the component list is complete, the [[net.noresttherein.oldsql.collection.LiteralIndex LiteralIndex]]
	  * containing their indexed values can be mapped into the 'packed' subject type `S`:
	  * {{{
	  *     IndexedSchemaMapping[PetKeeper].col("squirrels", _.squirrels).col("hamsters", _.hamsters).map {
	  *         index => PetKeeper(index("squirrels"), index("hamsters"))
	  *     }
	  * }}}
	  * This is equivalent to [[net.noresttherein.oldsql.schema.IndexedMappingSchema.apply IndexedMappingSchema[S, _]]],
	  * but the origin type is omitted with the intent of the constructed mapping being used as a
	  * [[net.noresttherein.oldsql.schema.SchemaMapping.|-| |-|]] mapping, included as a component in some larger
	  * [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * @param columnPrefix a `String` prepended to the names of all columns added to this schema. It is also passed
	  *                     to those added components which are created using passed factory functions, in order
	  *                     to append the prefix to all their columns, too.
	  * @param buffs the buffs for the constructed [[net.noresttherein.oldsql.schema.IndexedSchemaMapping IndexedSchemaMapping]],
	  *              inherited by all columns appended to the returned schema and those components which are created
	  *              with passed factory functions.
	  * @return an empty, extensible `IndexedMappingSchema`.
	  * @return an empty, extensible [[net.noresttherein.oldsql.schema.IndexedMappingSchema IndexedMappingSchema]].
	  * @tparam S the subject type of th constructed mapping.
	  */
	@inline def apply[S](columnPrefix :String, buffs :Buff[S]*) :ExtensibleFlatIndexedSchema[S, @~, @~, _] =
		IndexedMappingSchema[S, Any](columnPrefix, buffs :_*)



	/** A `Mapping` which has all its columns listed in its type as the `Chain` parameter `C`. Aside those direct
	  * columns, the mapping contains no other public components (there may however be hidden, synthetic components used
	  * to assemble the value index from the column values). The added benefit over the standard
	  * [[net.noresttherein.oldsql.schema.SchemaMapping.FlatSchemaMapping FlatSchemaMapping]] is that
	  * all columns are labeled with unique `String` literal types. This allows easy access by the
	  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport#/ /]] method:
	  * {{{
	  *     val ownedSquirrels = this / "squirrelCount"
	  * }}}.
	  * @tparam S the subject type of this mapping.
	  * @tparam V a `LiteralIndex` containing the types of all columns in `C` in their exact order, indexed
	  *           by the column labels.
	  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order. All columns
	  *           are labeled, instances of [[net.noresttherein.oldsql.schema.SchemaMapping.@|| @||]].
	  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
	  *           but coming from different sources (especially different aliases for a table occurring more then once
	  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
	  *           included in a query can be used in the creation of SQL expressions used by that query.
	  *           Consult [[net.noresttherein.oldsql.schema.Mapping#Origin Mapping.Origin]]
	  */
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
	                      (protected override val backer :IndexedMappingSchema[S, V, C, O],
	                       constructor :V => S, buffs :Seq[Buff[S]] = Nil)
		extends MappedSchema[S, V, C, O](backer, constructor, buffs) with IndexedSchemaMapping[S, V, C, O]
		   with MappingSchemaDelegate[IndexedMappingSchema[S, V, C, O], S, V, C, O]



	private[schema] class OptMappedIndexSchema[S, V <: LiteralIndex, C <: Chain, O]
	                      (protected override val backer :IndexedMappingSchema[S, V, C, O],
	                       constructor :V => Option[S], buffs :Seq[Buff[S]] = Nil)
		extends OptMappedSchema[S, V, C, O](backer, constructor, buffs) with IndexedSchemaMapping[S, V, C, O]
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

