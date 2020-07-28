package net.noresttherein.oldsql.schema

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.{Chain, LiteralIndex, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.LiteralIndex.{:~, |~}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.MappingSchema.{GetLabeledComponent, GetSchemaComponent}
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, @||, |-|, ||, FlatSchemaMapping, LabeledSchemaColumn, MappedFlatSchema, MappedSchema, OptMappedFlatSchema, OptMappedSchema, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.{ConstantMapping, CustomizedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.{DelegateMapping, LazyMapping}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.schema.Buff.{BuffType, FlagBuffType}
import net.noresttherein.oldsql.schema.bits.MappingAdapter.DelegateAdapter






/** A list of components of some `SchemaMapping` for subject type `S`, with all their types encoded in this class's type.
  * This is the full list of components, ignoring the fact that some of them might not be available or are optional
  * for some types of database operations.
  * Each component of type `T` additionally has a `MappingExtract[S, T, O]` associated with it by this instance,
  * which can be accessed using the [[net.noresttherein.oldsql.schema.MappingSchema#extract extract(component)]]
  * method. A schema itself is a mapping for the chain `V` containing the values of all of its components in order,
  * but is more typically used as the basis of a `SchemaMapping` instance for some entity type `S`.
  * This can happen either by directly mapping the chain of values `V` with its
  * [[net.noresttherein.oldsql.schema.MappingSchema#map map]] method, or indirectly in an enclosing mapping's
  * `assemble` method, where components in this schema can be individually accessed, without constructing
  * an intermediate chain of values. There is an automatically available implicit conversion from non-empty
  * schemas (where `C` and `V` are not empty) which add methods for retrieving its components:
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#last last]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#apply apply()]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#prev prev]].
  *
  * @tparam S the entity type of an owning `SchemaMapping`. This schema disassembles this value into a chain
  *           of component values `V`.
  * @tparam V a `Chain` containing the subject types of all schema  in the chain `C`, which is the subject type
  *           of this mapping.
  * @tparam C a `Chain` listing the types of all components in this schema. All components must implement
  *           `SchemaMapping.|-|[_, _, _]`.
  * @tparam O a marker type denoting the origin of the mapping used to distinguish between different instances
  *           of the same class but representing different tables or different occurrences of a table in the
  *           ''from'' clause of an SQL select.
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.FlatSchemaMapping]]
  */
trait MappingSchema[S, V <: Chain, C <: Chain, O] extends BaseMapping[V, O] {

	/** The subject type of this schema.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema#Subject]]
	  */
	type Unpacked = V

	//fixme: C might omit mandatory components! schema.forSelect(...).forUpdate(...) can exclude a component necessary
	//  for update, but not select. We need special subclasses SelectSchema
	/** The chain listing components in this schema. In default implementation, these are all direct, non-synthetic
	  * components. Customizing this mapping by including and excluding certain components for the purpose of
	  * a particular SQL statement will produce `MappingSchema` instances with component chains being subsequences
	  * of this type. It follows that this list doesn't necessarily reflect a set of column particular
	  * to any single database operation and modification targeted at one type of access (such as update)
	  * may be invalid for another (such as select).
	  */
	type Components = C


	/** The `Subject` type of the outer mapping to which this schema belongs. */
	type Packed = S

	/** The subject type of this schema. In default implementation, it is a chain
	  * of subject types of components in the `Components` chain. There are some specialized indexed implementation
	  * where each entry in the chain is a key-value pair, where values are the components' subject types and keys
	  * are some arbitrary literal types used as field names. Regardless of the details, on the ''n''-th position
	  * there is always a value containing the of the value of the ''n''-th component in the schema.
	  * Excluding components for the purpose of a particular database operation will likewise exclude the
	  * associated entries in the value chain, which will always stay consistent with the component chain.
	  */
	override type Subject = V

	/** Upper bound on the self type of this schema, parameterized with the origin type.
	  * This alias provides a convenient type declaration which in practical applications are much too verbose
	  * to write by hand.
	  */
//	type WithOrigin[Q] = MappingSchema[S, V, C, Q]

	/** Base self type of this schema. */
	type Schema = MappingSchema[S, V, C, O]



	override def optionally(pieces :Pieces) :Option[V] = pieces.assemble(this) //no buffs.

	protected def packedBuffs :Seq[Buff[S]] = Nil

	private[schema] final def outerBuffs :Seq[Buff[S]] = packedBuffs

	/** Returns the chain with the values of all components from the subject value of the enclosing `SchemaMapping`.
	  * @return a chain of component values inside `Some` as long as all of them returned `Some`
	  *         from their `optionally` method, and `None` in the case when at least one of them didn't have a value
	  *         in the given subject.
	  */
	def unapply(subject :S) :Option[V]

	/** Returns the chain with the values of all components from the subject value of the enclosing `SchemaMapping`
	  * This method will ask the extractors given for all components to produce a value for that component.
	  * If at least one of them fails, a `NoSuchElementException` will be thrown.
	  * @return a chain of values for all components on the component chain `C`.
	  */
	def disassemble(subject :S) :V



	/** Fully typed list of components in this schema as a `Chain`. This list might not be exhaustive and,
	  * by necessity, some columns/components might not be applicable to all SQL operations.
	  * The components are listed as [[net.noresttherein.oldsql.schema.SchemaMapping#|-| |-|]] subtypes,
	  * without any reference to their `Origin` type in their signature. This is so they not become inconsistent
	  * with this instance's `Origin` as a result of an origin projection (casting on the last type parameter).
	  * The appropriate `BaseMapping` subtype for every component is determined by an implicit `OriginProjection[M]`
	  * declared for the component mapping `M`; safe downcasting can be performed by calling `component.withOrigin[X]`.
	  */
	def members :C



	/** Returns the `MappingExtract` for the component labeled with the given string literal in the schema.
	  * If more than one component with the same label exist, the last occurrence is selected.
	  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
	  *            of this schema.
	  * @return a `MappingExtract` for the found component, wrapping the getter function from the ''packed'' type.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
	  */
	def apply[N <: Label, T](label :N)(implicit get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]])
			:MappingExtract[S, T, O] =
		get.extract(this, label)

	/** Returns the component labeled with the given string literal in the schema. If more than one component with
	  * the same label exist, the last occurrence in the component chain `C` is selected. The return type
	  * is a projection from the base `|-|` (or its related subclass) of unknown `Origin` to a full
	  * `SchemaMapping` with the same `Origin` type as this instance.
      * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
	  *            of this schema.
	  * @param projection an implicit specifying the appropriate `LabeledSchemaMapping[N, T, _, _, _]` subclass
	  *                   for the accessed type `M`.
	  * @return a `LabeledSchemaMapping[N, T, _, _, O]` or its subclass.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[N <: Label, T, M <: @|-|[N, T, _, _]]
	     (label :N)(implicit get :GetLabeledComponent[N, V, C, T, M], projection :OriginProjection[M, T])
			:projection.WithOrigin[O] =
		projection(get(this, label))



	/** Returns the `MappingExtract` for the component at the given position in the schema.
	  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
	  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
	  *            on the component list `C`.
	  * @return a `MappingExtract` for the found component, wrapping the getter function from the ''packed'' type.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
	  */
	def apply[I <: Numeral, T](idx :I)(implicit get :GetSchemaComponent[I, V, C, T, |-|[T, _, _]])
			:MappingExtract[S, T, O] =
		get.extract(this, idx)

	/** Returns the component at the given position in the schema. The return type is a projection from the base `|-|`
	  * (or its related subclass) of unknown `Origin` to a full `SchemaMapping` with the same `Origin` type
	  * as this instance.
	  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
	  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
	  *            on the component list `C`.
	  * @param projection an implicit specifying the proper `SchemaMapping` subclass for the accessed mapping `M`.
	  * @return a component at the `idx` position on the component list `C`
	  *         of a subtype of `SchemaMapping[T, _, _, O]`.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[I <: Numeral, T, M <: |-|[T, _, _]]
	     (idx :I)(implicit get :GetSchemaComponent[I, V, C, T, M], projection :OriginProjection[M, T])
			:projection.WithOrigin[O] =
		projection(get(this, idx))



	//consider: perhaps custom evidence?
	/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
	def apply[M](implicit nonEmpty :C <:< (Chain ~ M)) :M = last

	/** The last component, exactly as appearing on the component list. */
	def last[M](implicit nonEmpty :C <:< (Chain ~ M)) :M

	/** The schema for the chain `I`, containing all components of this schema except for the last one. */
	def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any)): MappingSchema[S, I, P, O]



	/** All components in this schema in the inverse order to their desired appearance on the `components` list.
	  * Used to make the `components` method implementation more efficient by bypassing much less effective
	  * unions of `Unique` instances. Default implementation actually is based on `components` in order to allow
	  * custom `MappingSchema` implementations as this method is package protected and thus impossible to override
	  * by client classes.
	  */
	protected[schema] def componentsReversed :List[Component[_]] = components.toList.reverse

	/** All subcomponents in this schema in the inverse order to their desired appearance on the `subcomponents` list.
	  * Used to make the `subcomponents` method implementation more efficient by bypassing much less effective
	  * unions of `Unique` instances. Default implementation actually is based on `subcomponents` in order to allow
	  * custom `MappingSchema` implementations as this method is package protected and thus impossible to override
	  * by client classes.
	  */
	protected[schema] def subcomponentsReversed :List[Component[_]] = subcomponents.toList.reverse

	/** All columns in this schema in the inverse order to their desired appearance on the `columns` list.
	  * Used to make the `columns` method implementation more efficient by bypassing much less effective
	  * unions of `Unique` instances. Default implementation actually is based on `columns` in order to allow
	  * custom `MappingSchema` implementations as this method is package protected and thus impossible to override
	  * by client classes.
	  */
	protected[schema] def columnsReversed :List[Column[_]] = columns.toList.reverse


	/** An `Extract` of the owning mapping, representing this schema's component with subject type `T`
	  * as part of the 'packed' value `S`.
	  */
	type PackedExtract[T] = MappingExtract[S, T, O]

	/** A `ColumnExtract` of the owning mapping, representing this schema's column with subject type `T`
	  * as part of the 'packed' value `S`.
	  */
	type PackedColumnExtract[T] = ColumnMappingExtract[S, T, O]



	/** A `MappingExtract` presenting a component of this schema as a component
	  * of the associated `SchemaMapping[S, V, C, O]`. This method, unlike its original template `apply(component)`
	  * presenting a component as an extract ''of this schema'', must handle `this` as an argument.
	  */
	def extract[T](component :Component[T]) :MappingExtract[S, T, O] =
		packedExtracts(component)

	/** A `ColumnExtract` presenting a column of this schema as a column of the associated `SchemaMapping[S, V, C, O]`. */
	def extract[T](column :Column[T]) :ColumnMappingExtract[S, T, O] =
		packedColumnExtracts(column)

	/** A `MappingExtract` of the associated `SchemaMapping[S, V, C, O]` for the last component in this schema. */
	def lastExtract[T](implicit nonEmpty :C <:< (Chain ~ |-|[T, _ <: Chain, _ <: Chain])) :MappingExtract[S, T, O] =
		extract(last.withOrigin[O])


	/** The extracts for all components in this schema, including indirect an synthetic ones, from the outer mapping's
	  * subject type `Packed` (rather than the `Subject` of this mapping, which always is the value chain
	  * with types of individual components. This map, unlike [[net.noresttherein.oldsql.schema.Mapping#extracts extracts]],
	  * must contain the whole schema itself as a component key.
	  */
	def packedExtracts :NaturalMap[Component, PackedExtract]

	/** The extracts for all columns in this schema, including indirect ones, from the outer mapping's
	  * subject type `Packed` (rather than the `Subject` of this mapping, which always is the value chain
	  * with types of individual components.
	  */
	def packedColumnExtracts :NaturalMap[Column, PackedColumnExtract]



	//fixme: this doesn't map packedBuffs
	/** Adapts this schema for some other subject type `X` from which the value of the current enclosing mapping
	  * subject type `S` can be derived. This has the effect of composing the extractor for every component
	  * in this schema with the given function.
	  * @param extractor a function returning the owning mapping's subject value from some other value type.
	  * @tparam X the new target type for the enclosing mapping.
	  * @return An instance exactly equivalent to one where all method calls appending components used in building
	  *         of this schema have their passed extractor composed with the given function.
	  */
	def compose[X](extractor :X => S) :MappingSchema[X, V, C, O]

	/** Adapts this schema for some other subject type `X` from which the value of the current enclosing mapping
	  * subject type `S` can be derived. This has the effect of composing the extractor for every component
	  * in this schema with the given extractor.
	  * @param extractor an `Extractor` returning the owning mapping's subject value from some other value type.
	  * @tparam X the new target type for the enclosing mapping.
	  * @return An instance exactly equivalent to one where all method calls appending components used in building
	  *         of this schema have their passed extractor composed with the given extractor.
	  */
	def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O]






	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	def map(constructor :V => S) : SchemaMapping[S, V, C, O] =
		new MappedSchema(this, constructor, packedBuffs)

	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. This will result in slightly more efficient
	  * assembly than the other overloaded `map` method, as no chain with the values of all components will be assembled
	  * as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components listed by the chain `V`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S]) :SchemaMapping[S, V, C, O] =
		map { v :V => v.feedTo(constructor) }



	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may
	  * not produce the subject value for all input rows.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	def optMap(constructor :V => Option[S]) :SchemaMapping[S, V, C, O] =
		new OptMappedSchema(this, constructor, packedBuffs)


	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
	  * the subject value for all input rows. This will result in slightly more efficient assembly than the other
	  * overloaded `flatMap` method, as no chain with the values of all components will be assembled as an intermediate step.
	  * @param constructor a function which number of arguments and their types match the subject types of all
	  *                    components as listed by the chain `V`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
	  */
	def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]]) :SchemaMapping[S, V, C, O] =
		optMap { row :V => row.feedTo(constructor) }



	override def toString = "Schema[" + members + "]"

}






object MappingSchema {
	//todo: examples in the doc.
	//todo: sqlName; columnPrefix?

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a `SchemaMapping`.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[S, O] :ExtensibleFlatMappingSchema[S, @~, @~, O] = EmptySchema[S, O]()

	def apply[S, O](buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, O] = EmptySchema[S, O](buffs)






	/** A `MappingSchema` where every component is a column (extending the `SchemaColumn` interface). */
	trait FlatMappingSchema[S, V <: Chain, C <: Chain, O] extends MappingSchema[S, V, C, O] {

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
				:FlatMappingSchema[S, I, P, O]


		override def compose[X](extractor :X => S) :FlatMappingSchema[X, V, C, O]

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V, C, O]



		override def map(constructor :V => S) :FlatSchemaMapping[S, V, C, O] =
			new MappedFlatSchema(this, constructor, packedBuffs)

		override def map[F](constructor :F)(implicit apply :ChainApplication[V, F, S]) :FlatSchemaMapping[S, V, C, O] =
			map(apply(constructor, _))



		override def optMap(constructor :V => Option[S]) :FlatSchemaMapping[S, V, C, O] =
			new OptMappedFlatSchema(this, constructor, packedBuffs)

		override def optMap[F](constructor :F)(implicit apply :ChainApplication[V, F, Option[S]])
				:FlatSchemaMapping[S, V, C, O] =
			optMap(apply(constructor, _))


		override def toString = "FlatSchema[" + members + "]"
	}






	/** A `MappingSchema` with factory methods for schema columns and components used to build
	  * (in a purely functional way) a `MappingSchema` and a `SchemaMapping` by chaining calls.
	  * This is a separate class from the `MappingSchema` as different schema variants have slightly
	  * different methods with conflicting signatures.
	  */
	sealed trait ExtensibleMappingSchema[S, V <: Chain, C <: Chain, O] extends MappingSchema[S, V, C, O] {

		override def export[T](component :Component[T]) :Component[T] = component

		override def export[T](column :Column[T]) :Column[T] = column

		protected[schema] def conveyBuffs[T](extractor :S => T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) packedBuffs.flatMap(_.cascade(extractor))
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: packedBuffs.flatMap(_.cascade(extractor))

		protected[schema] def conveyBuffs[T](extractor :S =?> T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) schema.cascadeBuffs(packedBuffs, toString)(extractor)
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: schema.cascadeBuffs(packedBuffs, toString)(extractor)




		/** Appends a component to this schema. This is different from
		  * [[net.noresttherein.oldsql.schema.MappingSchema#append append]] in that it is overriden by
		  * `FlatMappingSchema` to return always `FlatMappingSchema`, regardless of the `M` component type.
		  * It should ''never'' be exposed outside `MappingSchema` as it can completely break the contract.
		  * It is worth having for the purpose of schema manipulating implicits which create new schemas
		  * by carrying over components from another schema. This kind of use is safe and lets us have a single
		  * method and simply cast down the result rather than separate for each schema type which would like
		  * to support the functionality.
		  */
		protected[MappingSchema] def carryOver[T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC]]
		                                      (component :M, extractor :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ M, O] =
			append[T, MV, MC, M](component, extractor)


		protected[schema] def append[T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC]](component :M, extractor :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptySchema[S, V, C, T, M, O](
				this, component, MappingExtract(component.withOrigin[O])(extractor)
			)




		/** Appends the given component to this schema. This component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[T, MV <: Chain, MC <: Chain](component: |-|[T, MV, MC], value :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			append[T, MV, MC, |-|[T, MV, MC]](component, value)

		/** Appends the given component to this schema. This component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain](component: @|-|[N, T, MV, MC], value :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			append[T, MV, MC, @|-|[N, T, MV, MC]](component, value)



		/** Appends a new component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def comp[T, MV <: Chain, MC <: Chain](value :S => T, buffs :Buff[T]*)
		                                     (component :Seq[Buff[T]] => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			comp(value, component(conveyBuffs(value, buffs)))

		/** Appends the given component to this schema. The component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[T, MV <: Chain, MC <: Chain](value :S => T, component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			comp(component, MappingExtract.req(component.withOrigin[O])(value))

		/** Appends a new labeled component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain]
		        (label :N, value :S => T, buffs :Buff[T]*)(component :Seq[Buff[T]] => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(value, component(conveyBuffs(value, buffs)) labeled[N] label)

		/** Appends a new labeled component to this schema.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param component the component to append.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain]
		        (label :N, value :S => T, component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(value, component labeled[N] label)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain](value :S => T, component: @|-|[N, T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(component, MappingExtract.req(component.withOrigin[O])(value))



		/** Appends a new component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def optcomp[T, MV <: Chain, MC <: Chain](value :S => Option[T], buffs :Buff[T]*)
		                                        (component :Seq[Buff[T]] => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			optcomp(value, component(conveyBuffs(Extractor(value), buffs)))

		/** Appends the given component to this schema. The component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[T, MV <: Chain, MC <: Chain](value :S => Option[T], component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			new ExtensibleNonEmptySchema[S, V, C, T, |-|[T, MV, MC], O](
				this, component, MappingExtract.opt(component.withOrigin[O])(value)
			)

		/** Appends a new labeled component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain]
		           (label :N, value :S => Option[T], buffs :Buff[T]*)(component :Seq[Buff[T]] => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			optcomp(value, component(conveyBuffs(Extractor(value), buffs)) labeled label)

		/** Appends a new labeled component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param component a mapping to append as a component to this schema under the given label.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain]
		           (label :N, value :S => Option[T], component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			optcomp(value, component labeled label)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain](value :S => Option[T], component: @|-|[N, T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(component, MappingExtract.opt(component.withOrigin[O])(value))



		/** Appends a new column to this schema with the given name. The column will have the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  */
		def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ ||[T], O] =
			append[T, @~, @~, ||[T]](SchemaColumn[T, O](name, conveyBuffs(value, buffs) :_*), Extractor.req(value))

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)

		/** Appends a new column of the given name to this schema. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning the value for the column from the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ ||[T], O] =
		{
			val extractor = Extractor(value)
			append[T, @~, @~, ||[T]](SchemaColumn[T, O](name, conveyBuffs(extractor, buffs) :_*), extractor)
		}

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[S, V ~ T, C ~ ||[T], O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		/** Appends a new column labeled with its name to this schema.  The column will receive the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			lbl(name, name, value, buffs :_*)

		/** Appends to this schema a new column labeled with a string different from its name.
		  * The column will receive the buffs specified here, followed by any buffs conveyed by this schema,
		  * given to it at its initialization.
		  * @param label the label used to access the column in the schema.
		  * @param name the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def lbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, conveyBuffs(value, buffs) :_*)
			append[T, @~, @~, N @|| T](column, Extractor.req(value))
		}



		/** Appends a new column labeled with its name to this schema.  The column will receive the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			optlbl(name, name, value, buffs:_*)

		/** Appends to this schema a new column labeled with a string different from its name.
		  * The column will receive the buffs specified here, followed by any buffs conveyed by this schema,
		  * given to it at its initialization.
		  * @param label a string literal used to access the column in the schema.
		  * @param name the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def optlbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
		{
			val extractor = Extractor(value)
			val column = LabeledSchemaColumn[N, T, O](label, name, conveyBuffs(extractor, buffs) :_*)
			append[T, @~, @~, N @|| T](column, extractor)
		}

	}






	/** A `FlatMappingSchema` with factory methods for schema columns and components used to build a
	  * `FlatMappingSchema` (and a `FlatSchemaMapping`) by chaining calls with component declarations.
	  * This class extends `ExtensibleMappingSchema`, but inherited non-column component factory methods
	  * switch back to building a general `MappingSchema` so that the process can diverge at any time.
	  */
	sealed trait ExtensibleFlatMappingSchema[S, V <: Chain, C <: Chain, O]
		extends ExtensibleMappingSchema[S, V, C, O] with FlatMappingSchema[S, V, C, O]
	{

		protected[MappingSchema] override def carryOver[T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC]]
		                                               (component :M, extractor :S =?> T)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptyFlatSchema[S, V, C, T, ||[T], O](
				this, component.asInstanceOf[||[T]], MappingExtract(component.withOrigin[O])(extractor)
			).asInstanceOf[ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O]]



		protected[schema] def col[T, M <: ||[T]](column :M, value :S =?> T)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptyFlatSchema[S, V, C, T, M, O](
				this, column, MappingExtract(column.withOrigin[O])(value)
			)



		override def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(SchemaColumn[T, O](name, buffs:_*), value)

		override def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)



		override def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(SchemaColumn[T, O](name, buffs :_*), Extractor(value))

		override def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		override def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs :_*), value)

		override def lbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(LabeledSchemaColumn[N, T, O](label, name, buffs :_*), value)

		override def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs :_*), Extractor(value))

		override def optlbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(LabeledSchemaColumn[N, T, O](label, name, buffs :_*), Extractor(value))

	}






	/** Additional methods for `MappingSchema` instances which could cause conflicts in multiple inheritance scenarios. */
	implicit class MappingSchemaMethods[S, V <: Chain, C <: Chain, O]
	                                   (private val self :MappingSchema[S, V, C, O]) extends AnyVal
	{
		/** Transforms this schema into an equivalent `FlatMappingSchema` by recursively replacing each component in the
		  * chain `C` with its columns. This process loses all information about replaced components and the new schema
		  * does not reference this instance in any way other than using the same column instances. It is however
		  * intended to be used as part of inlining of the enclosing mapping, which will retain the references to
		  * all components and use them for the assembly exactly as the enclosing mapping.
		  * This method is extracted from the schema class to an extension method to avoid conflict with the like
		  * method in `SchemaMapping` for `ChainMapping` and similar classes which implement both `SchemaMapping`
		  * and `MappingSchema` traits.
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping#flatten]]
		  */
		def flatten[FC <: Chain, FR <: Chain]
		           (implicit flatterer :SchemaFlattening[V, C, FR, FC]) :FlatMappingSchema[S, FR, FC, O] =
			self match {
				case flat :FlatMappingSchema[_, _, _, _] => flat.asInstanceOf[FlatMappingSchema[S, FR, FC, O]]
				case _ => flatterer(self)
			}

	}






	@implicitNotFound("Cannot concatenate schemas ${PC} + ${SV} (with subject types ${PV} + ${SR}). Possible reasons:\n" +
		              "1) any of the schemas contains non-column components,\n" +
		              "2) the type of any of the schemas is not fully known,\n" +
		              "3) the subject types don't match the subject types of the components," +
		              "4) result cannot be unified with provided concatenated type ${C} (${V}).")
	sealed trait ColumnSchemaConcat[PV <: Chain, PC <: Chain, SR <: Chain, SV <: Chain, V <: Chain, C <: Chain] {
		def apply[S, O](prefix :ExtensibleFlatMappingSchema[S, PV, PC, O], suffix: FlatMappingSchema[S, SR, SV, O])
				:ExtensibleFlatMappingSchema[S, V, C, O]
	}

	object ColumnSchemaConcat {
		private[this] val emptyCat = new ColumnSchemaConcat[Chain, Chain, @~, @~, Chain, Chain] {
			override def apply[S, O](prefix :ExtensibleFlatMappingSchema[S, Chain, Chain, O],
			                         suffix :FlatMappingSchema[S, @~, @~, O]) =
				prefix
		}

		implicit def concatEmpty[V <: Chain, C <: Chain] :ColumnSchemaConcat[V, C, @~, @~, V, C] =
			emptyCat.asInstanceOf[ColumnSchemaConcat[V, C, @~, @~, V, C]]

		implicit def concatColumns[PV <: Chain, PC <: Chain,
			                       SV <: Chain, SC <: Chain, T, M <: ||[T], V <: Chain, C <: Chain]
		                          (implicit init :ColumnSchemaConcat[PV, PC, SV, SC, V, C])
				:ColumnSchemaConcat[PV, PC, SV ~ T, SC ~ M, V ~ T, C ~ M] =
			new ColumnSchemaConcat[PV, PC, SV ~ T, SC ~ M, V ~ T, C ~ M] {

				override def apply[S, O](prefix :ExtensibleFlatMappingSchema[S, PV, PC, O],
				                         suffix :FlatMappingSchema[S, SV ~ T, SC ~ M, O]) =
					init(prefix, suffix.prev).carryOver[T, @~, @~, M](suffix.last, suffix.lastExtract)
			}
	}






	@implicitNotFound("Cannot inline schema ${C}\n (with subject type ${V}).\n Possible reasons: " +
	                  "the types are not fully known; subject types don't match the component types; " +
	                  "the result cannot be unified with result types ${FC} (${FV}).")
	abstract class SchemaFlattening[V <: Chain, C <: Chain, FV <: Chain, FC <: Chain] private[MappingSchema] {
		def apply[S, O](schema :MappingSchema[S, V, C, O]) :ExtensibleFlatMappingSchema[S, FV, FC, O]
	}



	sealed abstract class ComponentSchemaFlattening {

		private[this] final val empty = new SchemaFlattening[@~, @~, @~, @~] {
			override def apply[S, O](schema :MappingSchema[S, @~, @~, O]) =
				EmptySchema()
		}

		implicit def emptyIsFlat[S, O] :SchemaFlattening[@~, @~, @~, @~] =
			empty.asInstanceOf[SchemaFlattening[@~, @~, @~, @~]]

		implicit def appendFlattenedComponent[V <: Chain, C <: Chain, PV <: Chain, PC <: Chain,
			                                  T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC],
			                                  SV <: Chain, SC <: Chain,  FV <: Chain, FC <: Chain]
		                                     (implicit prefix :SchemaFlattening[V, C, PV, PC],
		                                      hint :Conforms[M, M, |-|[T, MV, MC]],
		                                      flatten :SchemaFlattening[MV, MC, SV, SC],
		                                      concat :ColumnSchemaConcat[PV, PC, SV, SC, FV, FC])
				:SchemaFlattening[V ~ T, C ~ M, FV, FC] =
			new SchemaFlattening[V ~ T, C ~ M, FV, FC] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O]) =
					concat(prefix(schema.prev), flatten(schema.last.schema.withOrigin[O]) compose schema.lastExtract)
			}

		implicit def appendFlattenedIndexedComponent[V <: LiteralIndex, C <: Chain, PV <: Chain, PC <: Chain,
		                                             T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC],
		                                             SV <: Chain,  SC <: Chain, FV <: Chain, FC <: Chain]
		                                            (implicit prefix :SchemaFlattening[V, C, PV, PC],
		                                             hint :Conforms[M, M, |-|[T, MV, MC]],
		                                             flatten :SchemaFlattening[MV, MC, SV, SC],
		                                             concat :ColumnSchemaConcat[PV, PC, SV, SC, FV, FC])
				:SchemaFlattening[V |~ (LiteralIndex.Key :~ T), C ~ M, FV, FC] =
			appendFlattenedComponent(prefix, hint, flatten, concat)
				.asInstanceOf[SchemaFlattening[V |~ (LiteralIndex.Key :~ T), C ~ M, FV, FC]]

	}

	object SchemaFlattening extends ComponentSchemaFlattening {

		implicit def appendColumn[V <: Chain, C <: Chain, T, M <: ||[T], FV <: Chain, FC <: Chain]
		                         (implicit init :SchemaFlattening[V, C, FV, FC])
				:SchemaFlattening[V ~ T, C ~ M, FV ~ T, FC ~ M] =
			new SchemaFlattening[V ~ T, C ~ M, FV ~ T, FC ~ M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O]) =
					init(schema.prev).carryOver[T, @~, @~, M](schema.last, schema.lastExtract)
			}

		implicit def appendIndexedColumn[V <: LiteralIndex, C <: Chain, T, M <: ||[T], FV <: Chain, FC <: Chain]
		                                (implicit init :SchemaFlattening[V, C, FV, FC])
				:SchemaFlattening[V |~ (LiteralIndex.Key :~ T), C ~ M, FV ~ T, FC ~ M] =
			appendColumn(init).asInstanceOf[SchemaFlattening[V |~ (LiteralIndex.Key :~ T), C ~ M, FV ~ T, FC ~ M]]
	}






/*
	@implicitNotFound("Can't exclude components ${E} from ${C} of schema:\n|-|[${S}, ${V}, ${C}].\n " +
	                  "Include and exclude type parameters must be concrete Chain subtypes with literal Int and String " +
	                  "element types denoting the indices/labels of top level components to exclude.\n" +
		              "Missing implicit CustomizeSchema[${S}, ${V}, ${C}, ${E}]: enable -Xlog-implicits " +
	                  "for a more detailed reason.")
	abstract class CustomizeSchema[S, V <: Chain, C <: Chain, E <: Chain] {
		type Values <: Chain
		type Components <: Chain

		def apply[O](schema: |-|[S, V, C], include :Iterable[|-|[_, _, _]],
		          prohibit :BuffType, explicit :BuffType, optional :BuffType, nonDefault :FlagBuffType)
				:MappingSchema[S, Values, Components, O]

		def apply[O](schema: |||[S, V, C], include :Iterable[|-|[_, _, _]],
		          prohibit :BuffType, explicit :BuffType, optional :BuffType, nonDefault :FlagBuffType)
				:FlatMappingSchema[S, Values, Components, O]
	}



	sealed abstract class LowPrioritySchemaCustomizationImplicits {

		implicit def include[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], E,
		                     I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain]
		                    (implicit prefix :FilterSchema[S, V, C, E, I, FV, FC], plus :PositiveInc[I, J])
				:FilterSchema[S, V ~ T, C ~ M, E, J, FV ~ T, FC ~ M] =
			new FilterSchema {
				def apply[O](schema: |-|[S, V ~ T, C ~ M]) =
					prefix(schema.prev).carryOver(schema.last, schema.extract(schema.last))

				def apply[O](schema: |||[S, V ~ T, C ~ M]) =
					prefix(schema.prev).carryOver(schema.last, schema.extract(schema.last))
			}

		implicit def includeIndexed[S, V <: LiteralIndex, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], E,
		                            I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain]
		                           (implicit prefix :FilterSchema[S, V, C, E, I, FV, FC], plus :PositiveInc[I, J])
				:FilterSchema[S, V |~ (LiteralIndex.Key :~ T), C ~ M, E, J, FV ~ T, FC ~ M] =
			include(prefix, plus).asInstanceOf[
				FilterSchema[S, V |~ (LiteralIndex.Key :~ T), C ~ M, E, J, FV ~ T, FC ~ M]
			]

	}



	object CustomizeSchema extends LowPrioritySchemaCustomizationImplicits {

		@implicitNotFound("Subschema ${M} at index ${N} is not on the exclude list ${E}.\n+" +
		                  "Missing implicit ExcludeComponent[${M}, ${N}, ${E}.")
		class ExcludeComponent[M, N <: Numeral, E <: Chain] private[CustomizeSchema] ()

		private[this] val instance = new ExcludeComponent[Mapping, 0, Chain]

		implicit def excludeByIndex[M <: Mapping, N <: Numeral, E <: Chain]
		                           (implicit included :ChainContains[E, N]) =
			instance.asInstanceOf[ExcludeComponent[M, N, E]]

		implicit def excludeByLabel[M <: LabeledMapping[L, _, _], N <: Numeral, L <: Label, E <: Chain]
		                           (implicit inferLabel :Conforms[M, M, @|-|[L, _, _, _]], included :ChainContains[E, L]) =
			instance.asInstanceOf[ExcludeComponent[M, N, E]]




		@implicitNotFound("Failed to exclude components ${E} from the component chain ${C}.\n" +
		                  "This is typically caused by a non-existing component index or label string on the exclude list. " +
		                  "No implicit for FilterSchema[${S}, ${V}, ${C}, ${E}, ${N}, ${FV}, ${FC}, ${O}].")
		abstract class FilterSchema[S, V <: Chain, C <: Chain, E, N <: Numeral, FV <: Chain, FC <: Chain] {
			def apply[O](schema: |-|[S, V, C]) :ExtensibleMappingSchema[S, FV, FC, O]
			def apply[O](schema: |||[S, V, C]) :ExtensibleFlatMappingSchema[S, FV, FC, O]
		}

		private[this] val empty = new FilterSchema[Any, @~, @~, @~, 0, @~, @~] {
			override def apply[O](schema: |-|[Any, @~, @~]) = EmptySchema()
			override def apply[O](schema: |||[Any, @~, @~]) = EmptySchema()
		}

		implicit def emptySchema[S, O, E <: Chain] :FilterSchema[S, @~, @~, E, 0, @~, @~] =
			empty.asInstanceOf[FilterSchema[S, @~, @~, E, 0, @~, @~]]


		implicit def exclude[S, V <: Chain, C <: Chain, T, M <: |-|[T, _, _], E <: Chain,
		                     I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain]
		                    (implicit prefix :FilterSchema[S, V, C, E, I, FV, FC], inc :Inc[I, J],
		                     exclude :ExcludeComponent[M, I, E]) :FilterSchema[S, V ~ T, C ~ M, E, J, FV, FC] =
			new FilterSchema {
				override def apply[O](schema: |-|[S, V ~ T, C ~ M]) = prefix(schema.prev)
				override def apply[O](schema: |||[S, V ~ T, C ~ M]) = prefix(schema.prev)
			}

		implicit def excludeIndexed[S, V <: LiteralIndex, C <: Chain, T, M <: |-|[T, _, _], E <: Chain,
		                            I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain]
		                           (implicit prefix :FilterSchema[S, V, C, E, I, FV, FC], inc :Inc[I, J],
		                            exclude :ExcludeComponent[M, I, E])
				:FilterSchema[S, V |~ (LiteralIndex.Key :~ T), C ~ M, E, J, FV, FC] =
			this.exclude[S, V, C, T, M, E, I, J, FV, FC](prefix, inc, exclude)
				.asInstanceOf[FilterSchema[S, V |~ (LiteralIndex.Key :~ T), C ~ M, E, J, FV, FC]]




		@implicitNotFound("Not all items in chain ${E} identify schemas in chain ${C}. " +
			              "Valid members are String literals (component labels) and Int literals (component indices).")
		class ComponentsExist[C <: Chain, E <: Chain] private[CustomizeSchema] ()

		private[this] val exists = new ComponentsExist[Chain, Chain]

		implicit def noExclusions[C <: Chain] :ComponentsExist[C, @~] = exists.asInstanceOf[ComponentsExist[C, @~]]

		implicit def excludedIndex[C <: Chain, N <: Numeral, E <: Chain]
		                          (implicit previousExist :ComponentsExist[C, E], lastExists :ChainGet[C, N, _])
				:ComponentsExist[C, E ~ N] =
			instance.asInstanceOf[ComponentsExist[C, E ~ N]]

		implicit def excludeLabel[C <: Chain, L <: Label, E <: Chain]
		                         (implicit previousExist :ComponentsExist[C, E],
		                          lastExists :ItemExists[C, ({ type T[X] = X <:< @|-|[L, _, _, _] })#T, _])
				:ComponentsExist[C, E ~ L] =
			instance.asInstanceOf[ComponentsExist[C, E ~ L]]



		implicit def customize[S, V <: Chain, C <: Chain, E <: Chain, FV <: Chain, FC <: Chain]
		                      (implicit filter :FilterSchema[S, V, C, E, _, FV, FC], allExist :ComponentsExist[C, E])
				:CustomizeSchema[S, V, C, E] { type Values = FV; type Components = FC } =
			new CustomizeSchema[S, V, C, E] {
				override type Values = FV
				override type Components = FC

				override def apply[O](schema: |-|[S, V, C],
				                      includes :Iterable[|-|[_, _, _]], ban :BuffType,
				                      explicit :BuffType, optional :BuffType, nonDefault :FlagBuffType) =
				{
					val filtered = filter(schema)
					val excludes = excluded[O](filtered, schema)
					new CustomizedSchema(filtered,
					                     includes.asInstanceOf[Iterable[MappingSchema[_, _, _, O]]], ban, explicit,
					                     excludes, optional, nonDefault)
				}

				override def apply[O](schema: |||[S, V, C],
				                      includes :Iterable[|-|[_, _, _]], ban :BuffType,
				                      explicit :BuffType, optional :BuffType, nonDefault :FlagBuffType) =
				{
					val filtered = filter(schema)
					val excludes = excluded[O](filtered, schema)
					new CustomizedFlatSchema(filtered,
					                         includes.asInstanceOf[Iterable[MappingSchema[_, _, _, O]]], ban, explicit,
					                         excludes, optional, nonDefault)
				}


				private def excluded[O](filtered: |-|[S, FV, FC], schema: |-|[S, V, C]) = {
					val components = schema.members.toSeq.asInstanceOf[Seq[MappingSchema[_, _, _, O]]]
					val remaining = filtered.members.toSeq.asInstanceOf[Seq[|-|[_, _, _]]].toSet
					components.filterNot(remaining)
				}
			}

	}
*/






	@implicitNotFound("No ${M} <: @|-|[${N}, ${T}, _, _] present in the schema ${C}\n(with values ${V}).")
	sealed abstract class GetLabeledComponent[N <: Label, V <: Chain, C <: Chain, T, +M <: @|-|[N, T, _, _]] {
		def apply[S, O](schema :MappingSchema[S, V, C, O], label :N) :M
		def extract[S, O](schema :MappingSchema[S, V, C, O], label :N) :MappingExtract[S, T, O]
	}



	object GetLabeledComponent {

		implicit def last[N <: Label, V <: Chain, C <: Chain, M <: @|-|[N, T, _ <: Chain, _ <: Chain], T]
				:GetLabeledComponent[N, V ~ T, C ~ M, T, M] =
			new GetLabeledComponent[N, V ~ T, C ~ M, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O], label :N) = schema.last

				override def extract[S, O](schema: MappingSchema[S, V ~ T, C ~ M, O], label :N) =
					schema.lastExtract
			}

		implicit def previous[N <: Label, V <: Chain, C <: Chain, X, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain], L <: Mapping]
		                     (implicit get :GetLabeledComponent[N, V, C, T, M])
				:GetLabeledComponent[N, V ~ X, C ~ L, T, M] =
			new GetLabeledComponent[N, V ~ X, C ~ L, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], label :N) =
					get(schema.prev, label)

				override def extract[S, O](schema: MappingSchema[S, V ~ X, C ~ L, O], label :N) =
					get.extract(schema.prev, label)
			}
	}






	@implicitNotFound("No ${M} <: |-|[${T}, _, _] present at index ${I} in the schema ${C}\n(with values ${V}).")
	sealed abstract class GetSchemaComponent[I <: Numeral, V <: Chain, C <: Chain, T, +M <: |-|[T, _, _]] {
		def apply[S, O](schema :MappingSchema[S, V, C, O], idx :I) :M
		def extract[S, O](schema :MappingSchema[S, V, C, O], idx :I) :MappingExtract[S, T, O]
	}



	object GetSchemaComponent {

		implicit def singleton[T, M <: |-|[T, _ <: Chain, _ <: Chain]]
				:GetSchemaComponent[0, @~ ~ T, @~ ~ M, T, M] =
			new GetSchemaComponent[0, @~ ~ T, @~ ~ M, T, M] {
				override def apply[S, O](schema :MappingSchema[S, @~ ~ T, @~ ~ M, O], idx :0) = schema.last

				override def extract[S, O](schema :MappingSchema[S, @~ ~ T, @~ ~ M, O], idx :0) =
					schema.lastExtract
			}

		implicit def last[I <: Numeral, J <: Numeral, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain,  _ <: Chain]]
		                 (implicit inc :Inc[I, J], size :GetSchemaComponent[I, V, C, _, _])
				:GetSchemaComponent[J, V ~ T, C ~ M, T, M] =
			new GetSchemaComponent[J, V ~ T, C ~ M, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O], idx :J) :M = schema.last

				override def extract[S, O](schema: MappingSchema[S, V ~ T, C ~ M, O], idx :J) =
					schema.lastExtract
			}

		implicit def previous[I <: Numeral, V <: Chain, C <: Chain, X, L <: Mapping,
		                      T, M <: |-|[T, _ <: Chain, _ <: Chain]]
		                     (implicit get :GetSchemaComponent[I, V, C, T, M])
				:GetSchemaComponent[I, V ~ X, C ~ L, T, M] =
			new GetSchemaComponent[I, V ~ X, C ~ L, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], idx :I) = get(schema.prev, idx)

				override def extract[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], idx :I) =
					get.extract(schema.prev, idx)
			}
	}






	private[schema] class EmptySchema[S, O] extends ConstantMapping[@~, O](@~) with FlatMappingSchema[S, @~, @~, O] {
		private[this] val result = Some(@~)

		override def unapply(subject :S): Option[@~] = result

		override def disassemble(subject :S): @~ = @~



		override def prev[I <: Chain, P <: Chain](implicit vals: @~ <:< (I ~ Any), comps: @~ <:< (P ~ Any))
				:FlatMappingSchema[S, I, P, O] =
			throw new UnsupportedOperationException("EmptySchema.prev")

		override def last[M](implicit nonEmpty: @~ <:< (Chain ~ M)) :M =
			throw new NoSuchElementException("EmptySchema.last")



		override def members: @~ = @~

		private[this] val extractor :MappingExtract[S, @~, O] = MappingExtract.const(this)(@~)

		override def packedExtracts :NaturalMap[Component, PackedExtract] = NaturalMap.empty
//			NaturalMap.single[Component, PackedExtract, @~](this, extractor)

		override def packedColumnExtracts :NaturalMap[Column, PackedColumnExtract] = NaturalMap.empty


		override def extract[X](component :Component[X]) :MappingExtract[S, X, O] =
			if (component eq this)
				extractor.asInstanceOf[MappingExtract[S, X, O]]
			else
				throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping schema.")

		override def extract[X](column :Column[X]) :ColumnMappingExtract[S, X, O] =
			throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping schema.")



		override def compose[X](extractor :X => S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]

		override def compose[X](extractor :X =?> S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]



		protected[schema] override def componentsReversed :List[Nothing] = Nil
		protected[schema] override def subcomponentsReversed :List[Component[_]] = Nil
		protected[schema] override def columnsReversed :List[Column[_]] = Nil
	}



	private[schema] class ExtensibleEmptySchema[S, O](protected override val packedBuffs :Seq[Buff[S]] = Nil)
		extends EmptySchema[S, O] with ExtensibleFlatMappingSchema[S, @~, @~, O]



	object EmptySchema {
		private[this] val empty = new ExtensibleEmptySchema[Any, Any]

		def apply[S, O]() :ExtensibleFlatMappingSchema[S, @~, @~, O] = empty.asInstanceOf[ExtensibleEmptySchema[S, O]]

		def apply[S, O](buffs :Seq[Buff[S]]) :ExtensibleFlatMappingSchema[S, @~, @~, O] =
			new ExtensibleEmptySchema[S, O](buffs)

		def unapply(schema :MappingSchema[_, _, _, _]) :Boolean = schema.isInstanceOf[EmptySchema[_, _]]
	}






	/** Base class for all non-empty schemas serving  as a list link responsible for a single component of the schema.
	  * The value of this component (and, what follows, the whole schema) is an arbitrary `Chain`, being
	  * a subclass of a chain variant `B`. As such, it combines the `Chain` of the preceding schema with the value
	  * of the last component `T` into a larger chain `V L E`, where `L` is the type constructor of the link for
	  * the particular chain type `B`.
	  * @param init the schema for the preceding components in the chain.
	  * @param element the last component in the chain, associated with this.
	  * @param extractor extracts the value of the last component from the subject of th owning mapping.
	  * @param lastValue extracts the value of the last component from the chain which is the subject of this mapping.
	  * @tparam S the subject type of the enclosing ('result') mapping (sometimes referred to as the 'owner' or 'parent').
	  * @tparam B the upper bound of the chain variant which this schema's subject(s) conform to; it is the upper bound
	  *           for the `init` part of the chain link `L[_, _]`.
	  * @tparam U the upper bound of all elements in the chains of type `B`, serving as a bound for the link type `L`.
	  * @tparam L the link type constructor accepting the chain being a subtype of `B` of initial component subjects
	  *           and an entry value ''derived'' from the subject of the last component.
	  * @tparam V the `Chain` (possibly of some specific kind) derived from the values of all components in the schema
	  *           (but not necessarily consisting exactly of them), forming the initial part of subject type
	  *           of this mapping.
	  * @tparam C the (regular) `Chain` listing the types of all preceding components in the schema.
	  * @tparam T the subject type of the last component in this schema.
	  * @tparam E the last ''entry'' type of the chain which is the subject of this mapping, derived from `T`.
	  * @tparam M the type of the last subschema contained by this instance.
	  * @tparam O the origin type serving as a discriminator between different instances by tagging them on the type level.
	  */
	private[schema] abstract class BaseNonEmptySchema[B <: Chain, U, L[+X <: B, +Y <: U] <: X ~ Y,
	                                                  S, V <: B, C <: Chain,
		                                              T, E <: U, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                                                 (val init :MappingSchema[S, V, C, O], element :M,
	                                                  val extractor :MappingExtract[S, T, O], lastValue :V L E => T)
		extends MappingSchema[S, V L E, C ~ M, O] with LazyMapping[V L E, O]
	{

		override def unapply(subject :S) :Option[V L E] =
			for (i <- init.unapply(subject); l <- extractor.get(subject)) yield link(i, l)

		override def disassemble(subject :S) :V L E = link(init.disassemble(subject), extractor(subject))

		protected def link(init :V, last :T) :V L E

		override def assemble(pieces :Pieces) :Option[V L E] =
			for (i <- pieces.get(initExtract); l <- pieces.get(componentExtract)) yield link(i, l)



		override val members :C ~ M = init.members ~ element

		@inline protected final def component :Component[T] = (element :RefinedMapping[T, element.Origin]).withOrigin[O]

		override def last[N](implicit nonEmpty :C ~ M <:< (Chain ~ N)) :N = component.asInstanceOf[N]

		override def prev[I <: Chain, P <: Chain](implicit vals :L[V, E] <:< (I ~ Any), comps :C ~ M <:< (P ~ Any))
				:MappingSchema[S, I, P, O] =
			init.asInstanceOf[MappingSchema[S, I, P, O]]



		override def apply[X](comp :Component[X]) :Extract[X] =
			if (comp eq this) selfExtract.asInstanceOf[Extract[X]]
			else if (comp == component) componentExtract.asInstanceOf[Extract[X]]
			else extracts(comp)

		override def apply[X](column :Column[X]) :ColumnExtract[X] = //fiixme
			if (column == component) componentExtract.asInstanceOf[ColumnExtract[X]]
			else columnExtracts(column)

		override def extract[X](comp :Component[X]) :PackedExtract[X] =
			if (comp eq this) unpackSelf.asInstanceOf[PackedExtract[X]]
			else if (comp == component) extractor.asInstanceOf[PackedExtract[X]]
			else packedExtracts(comp)

		override def extract[X](col :Column[X]) :PackedColumnExtract[X] =
			if (col eq component) extractor.asInstanceOf[PackedColumnExtract[X]]
			else packedColumnExtracts(col)

		override def lastExtract[X](implicit nonEmpty :C ~ M <:< (Chain ~ |-|[X, _ <: Chain, _ <: Chain]))
				:MappingExtract[S, X, O] =
			extractor.asInstanceOf[MappingExtract[S, X, O]]

		private[this] val unpackSelf :PackedExtract[V L E] = MappingExtract.opt(this)(unapply)
		private[this] val selfExtract :Extract[V L E] = MappingExtract.ident(this)
		private[this] val initExtract :Extract[V] = MappingExtract.req(init) { vs :(V L E) => vs.init }
		private[this] val componentExtract :Extract[T] = MappingExtract.req(component.withOrigin[O])(lastValue)

		override val extracts :NaturalMap[Component, Extract] =
			(init.extracts.map(schema.composeExtractAssoc(initExtract)(_)) ++
				component.withOrigin[O].extracts.map(schema.composeExtractAssoc(componentExtract)(_))
			).updated(init, initExtract).updated(component.withOrigin[O], componentExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] =
			NaturalMap.delayed(schema.selectColumnExtracts(this)(extracts))

		override val packedExtracts :NaturalMap[Component, PackedExtract] =
			init.packedExtracts ++ component.extracts.map(schema.composeExtractAssoc(extractor)(_))
				.updated[PackedExtract, T](component, extractor).updated[PackedExtract, V L E](this, unpackSelf)

		override val packedColumnExtracts = NaturalMap.delayed(schema.selectColumnExtracts(toString)(packedExtracts))



		override protected[schema] def componentsReversed :List[Component[_]] =
			component :: init.componentsReversed

		override protected[schema] def subcomponentsReversed :List[Component[_]] =
			component :: component.subcomponents.toList reverse_::: init :: init.subcomponentsReversed

		protected[schema] override def columnsReversed :List[Column[_]] =
			component.columns.toList reverse_::: init.columnsReversed

		override val components :Unique[Component[_]] = Unique.Lazy(componentsReversed.reverse)
		override val subcomponents :Unique[Component[_]] = Unique.Lazy(subcomponentsReversed.reverse)
		override val columns :Unique[Column[_]] = Unique.Lazy(columnsReversed.reverse)

	}






	private[schema] trait BaseNonEmptyFlatSchema[B <: Chain, U, L[+X <: B, +Y <: U] <: X ~ Y,
	                                             S, V <: B, C <: Chain,
	                                             T, E <: U, M <: ||[T], O]
		extends FlatMappingSchema[S, V L E, C ~ M, O]
	{ this :BaseNonEmptySchema[B, U, L, S, V, C, T, E, M, O] =>

		val init :FlatMappingSchema[S, V, C, O]

		override def prev[I <: Chain, P <: Chain](implicit vals :V L E <:< (I ~ Any), comps :C ~ M <:< (P ~ Any))
				:FlatMappingSchema[S, I, P, O] =
			init.asInstanceOf[FlatMappingSchema[S, I, P, O]]
	}






	private[schema] class NonEmptySchema[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
	                      (prev :MappingSchema[S, V, C, O], comp :M, extractor :MappingExtract[S, T, O])
		extends BaseNonEmptySchema[Chain, Any, ~, S, V, C, T, T, M, O](prev, comp, extractor, Chain.last)
	{
		protected override def link(init :V, last :T) :V ~ T = init ~ last
		//todo: buffs
		override def compose[X](extractor :X => S) :MappingSchema[X, V ~ T, C ~ M, O] =
			new NonEmptySchema(init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V ~ T, C ~ M, O] =
			new NonEmptySchema(init compose extractor, last, this.extractor compose extractor)

	}



	private[schema] class ExtensibleNonEmptySchema[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
                          (prev :ExtensibleMappingSchema[S, V, C, O], last :M, extractor :MappingExtract[S, T, O])
		extends NonEmptySchema[S, V, C, T, M, O](prev, last, extractor) with ExtensibleMappingSchema[S, V ~ T, C ~ M, O]
	{
		protected override val packedBuffs :Seq[Buff[S]] = init.packedBuffs
	}






	private[schema] class NonEmptyFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T], O]
	                                        (override val init :FlatMappingSchema[S, V, C, O], next :M,
	                                         get :MappingExtract[S, T, O])
		extends NonEmptySchema[S, V, C, T, M, O](init, next, get)
		   with BaseNonEmptyFlatSchema[Chain, Any, ~, S, V, C, T, T, M, O]
	{
		//these shortcut implementations work because column mappings moved their buff handling to their forms.
		override val selectForm = SQLReadForm.ChainReadForm(init.selectForm, last.selectForm)
		override val queryForm = SQLWriteForm.ChainWriteForm(init.queryForm, last.queryForm)
		override val updateForm = SQLWriteForm.ChainWriteForm(init.updateForm, last.updateForm)
		override val insertForm = SQLWriteForm.ChainWriteForm(init.insertForm, last.insertForm)
		override def writeForm(op :WriteOperationType) :SQLWriteForm[V ~ T] = op.form(this)

		override def compose[X](extractor :X => S) :NonEmptyFlatSchema[X, V, C, T, M, O] =
			new NonEmptyFlatSchema(init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptyFlatSchema[X, V, C, T, M, O] =
			new NonEmptyFlatSchema(init compose extractor, last, this.extractor compose extractor)
	}



	private[schema] class ExtensibleNonEmptyFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T], O]
	                      (prev :ExtensibleFlatMappingSchema[S, V, C, O], next :M, get :MappingExtract[S, T, O])
		extends NonEmptyFlatSchema[S, V, C, T, M, O](prev, next, get)
		   with ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O]
	{
		protected override val packedBuffs :Seq[Buff[S]] = init.packedBuffs
	}






	private[schema] trait MappingSchemaProxy[S, V <: Chain, C <: Chain, O]
		extends MappingSchema[S, V, C, O] with DelegateMapping[MappingSchema[S, V, C, O], V, O]
	{
		override def unapply(subject :S) :Option[V] = backer.unapply(subject)
		override def disassemble(subject :S) :V = backer.disassemble(subject)
		override def members :C = backer.members

		override def assemble(pieces :Pieces) :Option[V] = backer.assemble(pieces)

		override def last[M](implicit nonEmpty :C <:< (Chain ~ M)) :M = backer.last

		override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
				:MappingSchema[S, I, P, O] =
			backer.prev

		override def packedExtracts :NaturalMap[Component, PackedExtract] = backer.packedExtracts
		override def packedColumnExtracts :NaturalMap[Column, PackedColumnExtract] = backer.packedColumnExtracts
	}



	private[schema] trait FlatMappingSchemaProxy[S, V <: Chain, C <: Chain, O]
		extends MappingSchemaProxy[S, V, C, O] with FlatMappingSchema[S, V, C, O]
		   with DelegateMapping[FlatMappingSchema[S, V, C, O], V, O]
	{
		override def prev[I <: Chain, P <: Chain]
		                 (implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any)) :FlatMappingSchema[S, I, P, O] =
			backer.prev
	}



	private[schema] class CustomizedSchema[S, V <: Chain, C <: Chain, O]
	                      (filtered: MappingSchema[S, V, C, O],
	                       include :Iterable[RefinedMapping[_, O]], prohibited :BuffType, explicit :BuffType,
	                       exclude :Iterable[RefinedMapping[_, O]], optional :BuffType, nonDefault :FlagBuffType)
		extends CustomizedMapping[MappingSchema[S, V, C, O], V, O](
		                          filtered, include, prohibited, explicit, exclude, optional, nonDefault
			) with MappingSchemaProxy[S, V, C, O] with DelegateAdapter[MappingSchema[S, V, C, O], V, O]
	{

		override def compose[X](extractor :X => S) :MappingSchema[X, V, C, O] =
			new CustomizedSchema(backer compose extractor, include, prohibited, explicit, exclude, optional, nonDefault)

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O] =
			new CustomizedSchema(backer compose extractor, include, prohibited, explicit, exclude, optional, nonDefault)

	}



	private[schema] class CustomizedFlatSchema[S, V <: Chain, C <: Chain, O]
	                      (override val backer :FlatMappingSchema[S, V, C, O],
	                       include :Iterable[RefinedMapping[_, O]], prohibited :BuffType, explicit :BuffType,
	                       exclude :Iterable[RefinedMapping[_, O]], optional :BuffType, nonDefault :FlagBuffType)
		extends CustomizedSchema(backer, include, prohibited, explicit, exclude, optional, nonDefault)
		   with FlatMappingSchemaProxy[S, V, C, O] with DelegateAdapter[FlatMappingSchema[S, V, C, O], V, O]
	{
		override def compose[X](extractor :X => S) :FlatMappingSchema[X, V, C, O] =
			new CustomizedFlatSchema(backer compose extractor,
			                         include, prohibited, explicit, exclude, optional, nonDefault)

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V, C, O] =
			new CustomizedFlatSchema(backer compose extractor,
			                         include, prohibited, explicit, exclude, optional, nonDefault)
	}


}


