package net.noresttherein.oldsql.schema

import scala.annotation.implicitNotFound
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.{Chain, IndexedChain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication}
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.schema.Mapping.{OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, @||, |-|, ||, FlatSchemaMapping, LabeledSchemaColumn, MappedFlatSchema, MappedSchema, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.{ConstantMapping, CustomizedMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.Label
import net.noresttherein.oldsql.schema.support.{DelegateMapping, LazyMapping}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.OperationType.WriteOperationType
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy






/** A list of components of some `SchemaMapping` for subject type `S`, with all their types encoded in this class's type.
  * This is the full list of components, ignoring the fact that some of them might not be available or are optional
  * for some types of database operations.
  * Each component of type `T` additionally has a `MappingExtract[S, T, O]` associated with it by this instance,
  * which can be accessed using the [[net.noresttherein.oldsql.schema.MappingSchema.extract extract(component)]]
  * method. A schema itself is a mapping for the chain `V` containing the values of all of its components in order,
  * but is more typically used as the basis of a `SchemaMapping` instance for some entity type `S`.
  * This can happen either by directly mapping the chain of values `V` with its
  * [[net.noresttherein.oldsql.schema.MappingSchema.map map]] method, or indirectly in an enclosing mapping's
  * `assemble` method, where components in this schema can be individually accessed, without constructing
  * an intermediate chain of values. There is an automatically available implicit conversion from non-empty
  * schemas (where `C` and `V` are not empty) which add methods for retrieving its components:
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaExtension.last last]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaExtension.apply apply()]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaExtension.prev prev]].
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
trait MappingSchema[S, V <: Chain, C <: Chain, O] extends BaseMapping[V, O] with MappingSchemaSupport {

	/** The subject type of this schema.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.Subject]]
	  */
	override type Unpacked = V

	//fixme: C might omit mandatory components! schema.forSelect(...).forUpdate(...) can exclude a component necessary
	//  for update, but not select. We need special subclasses SelectSchema or perhaps better, a SchemaForm.
	override type Components = C

	/** The `Subject` type of the outer mapping to which this schema belongs. */
	override type Packed = S

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
	override type Schema = MappingSchema[S, V, C, O]

	protected override def schema :Schema = this



	override def optionally(pieces :Pieces) :Option[V] = pieces.assemble(this) //no buffs.

	/** The buffs intended for the outer mapping of `S` based on this schema. */
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
	  * The components are listed as [[net.noresttherein.oldsql.schema.SchemaMapping.|-| |-|]] subtypes,
	  * without any reference to their `Origin` type in their signature. This is so they not become inconsistent
	  * with this instance's `Origin` as a result of an origin projection (casting on the last type parameter).
	  * The appropriate `BaseMapping` subtype for every component is determined by an implicit `OriginProjection[M]`
	  * declared for the component mapping `M`; safe downcasting can be performed by calling `component.withOrigin[X]`.
	  */
	def members :C



	//consider: perhaps custom evidence?
	/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
	def apply[M](implicit nonEmpty :C <:< (Chain ~ M)) :M = last

	/** The last component, exactly as appearing on the component list. */
	def last[M](implicit nonEmpty :C <:< (Chain ~ M)) :M

	/** The schema for the chain `I`, containing all components of this schema except for the last one. */
	def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any)) :MappingSchema[S, I, P, O]



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
	  * with types of individual components. This map, unlike [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]],
	  * must contain the whole schema itself as a component key.
	  */
	def packedExtracts :NaturalMap[Component, PackedExtract]

	/** The extracts for all columns in this schema, including indirect ones, from the outer mapping's
	  * subject type `Packed` (rather than the `Subject` of this mapping, which always is the value chain
	  * with types of individual components.
	  */
	def packedColumnExtracts :NaturalMap[Column, PackedColumnExtract]



	/** Adapts this schema for some other subject type `X` from which the value of the current enclosing mapping
	  * subject type `S` can be derived. This has the effect of composing the extractor for every component
	  * in this schema with the given extractor. It is used in operations transforming this schema, such as flattening,
	  * and when the outer, 'packed' mapping is mapped. Note that all ''packed'' buffs (those intended
	  * for the outer mapping based on this schema) associated with this instance are lost and thus it should not be used
	  * before the packed `SchemaMapping`'s creation. Once it is created and the buffs are passed to it,
	  * this property becomes redundant as all buffs which cascaded to the components are preserved and the 'map'
	  * function for the packed mapping handles its buffs independently.
	  * @param extractor an `Extractor` returning the owning mapping's subject value from some other value type.
	  * @tparam X the new target type for the enclosing mapping.
	  * @return An instance exactly equivalent to one where all method calls appending components used in building
	  *         of this schema have their passed extractor composed with the given extractor.
	  */
	def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O]



	override def toString = "Schema[" + members + "]"

}






object MappingSchema {
	//todo: examples in the doc.

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * If you wish only to create a subcomponent for use in another schema and thus you do not care about the
	  * origin type, you may start with the analogous factory method
	  * of the [[net.noresttherein.oldsql.schema.SchemaMapping$ SchemaMapping]] which is parameterized
	  * only with the subject type.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @tparam O the `Origin` type given to the created `MappingSchema` and the final `SchemaMapping`.
	  * @return an [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[S, O] :ExtensibleFlatMappingSchema[S, @~, @~, O] = EmptySchema[S, O]()

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * If you wish only to create a subcomponent for use in another schema and thus you do not care about the
	  * origin type, you may start with the analogous factory method
	  * of the [[net.noresttherein.oldsql.schema.SchemaMapping$ SchemaMapping]] which is parameterized
	  * only with the subject type.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @param buffs a list of `Buff`s for the created `SchemaMapping`. They will not show as the buffs
	  *              of the returned schema, but will be nevertheless inherited by all all columns appended to it
	  *              and those components for which factory functions will be given.
	  * @return an [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[S, O](buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, O] = EmptySchema[S, O](buffs)


	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * If you wish only to create a subcomponent for use in another schema and thus you do not care about the
	  * origin type, you may start with the analogous factory method
	  * of the [[net.noresttherein.oldsql.schema.SchemaMapping$ SchemaMapping]] which is parameterized
	  * only with the subject type.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @param columnPrefix a `String` prepended to all column names. It will be also passed as an argument
	  *                     to all factory functions used to create new components for the schema.
	  * @param buffs a list of `Buff`s for the created `SchemaMapping`. They will not show as the buffs
	  *              of the returned schema, but will be nevertheless inherited by all all columns appended to it
	  *              and those components for which factory functions will be given.
	  * @return an [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[S, O](columnPrefix :String, buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, O] =
		EmptySchema[S, O](columnPrefix, buffs)






	trait MappingSchemaSupport extends Mapping {

		/** The scala type to which the schema maps. It is the `Subject` type of the outer mapping of this schema. */
		type Packed

		/** The subject type of the schema. In the default implementation, it is a chain of subject types of components
		  * in the `Components` chain. There are some specialized indexed implementation where each entry in the chain
		  * is a key-value pair, where values are the components' subject types and keys are some arbitrary literal types
		  * used as field names. Regardless of the details, on the ''n''-th position there is always a value containing
		  * the subject of the ''n''-th component in the schema. Excluding components for the purpose of a particular
		  * database operation will likewise exclude the associated entries in the value chain,
		  * which will always stay consistent with the component chain.
		  */
		type Unpacked <: Chain

		/** The chain listing components in the schema. In the default implementation, these are all direct,
		  * non-synthetic components. These are the [[net.noresttherein.oldsql.schema.SchemaMapping.|-| |-|]] subtypes
		  * which were appended to the schema explicitly by the client code and are `Mapping`s
		  * with an unspecified `Origin` type to make them invariant during this schema's projection
		  * to a different origin type). Customizing the the schema by including and excluding certain components
		  * for the purpose of a particular SQL statement will produce `MappingSchema`s with `Components` chains
		  * being subsequences of this schema. It follows that this list doesn't necessarily reflect a set of columns
		  * particular to any single database operation and modification targeted at one type of access (such as update)
		  * may be invalid for another (such as select).
		  */
		type Components <: Chain

		/** The full type of the schema.
		  * This alias provides a convenient type declaration which in practical applications are much too verbose
		  * to write by hand. Note that this is the upper bound of the schema type, with `SchemaMapping` subclasses
		  * using subtypes of this type with additional features, such as `FlatMappingSchema` for `FlatSchemaMapping`.
		  */
		type Schema = MappingSchema[Packed, Unpacked, Components, Origin]

		/** The associated schema. */
		protected def schema :Schema


		/** Returns the `MappingExtract` for the component labeled with the given string literal in the schema.
		  * If more than one component with the same label exist, the last occurrence is selected.
		  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
		  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
		  *            of this schema.
		  * @return a `MappingExtract` for the found component, wrapping the getter function from the `Subject` type.
		  * @tparam N the string singleton type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
		  */
		def apply[N <: Label, T](label :N)(implicit get :GetLabeledComponent[N, Unpacked, Components, T, @|-|[N, T, _, _]])
				:MappingExtract[Packed, T, Origin] =
			get.extract(schema, label)

		/** Returns the component labeled with the given string literal in the schema. If more than one component with
		  * the same label exist, the last occurrence in the component chain `C` is selected. The return type
		  * is a projection from the base `|-|` (or its related subclass) of unknown `Origin` to a full
		  * `SchemaMapping` with the same `Origin` type as this instance.
		  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
		  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
		  *            of this schema.
		  * @param project an implicit specifying the appropriate `LabeledSchemaMapping[N, T, _, _, _]` subclass
		  *                for the accessed type `M`.
		  * @return a `LabeledSchemaMapping[N, T, _, _, Origin]` or its subclass.
		  * @tparam N the string singleton type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @tparam M the full type of the returned component, as present on the component list `C`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N) MappingSchema.apply(label)]]
		  */
		def /[N <: Label, T, M <: @|-|[N, T, _, _]]
		     (label :N)(implicit get :GetLabeledComponent[N, Unpacked, Components, T, M], project :OriginProjection[M, T])
				:project.WithOrigin[Origin] =
			project(get(schema, label))


		/** Returns the `MappingExtract` for the component at the given position in the schema.
		  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
		  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
		  *            on the component list `C`.
		  * @return a `MappingExtract` for the found component, wrapping the getter function from the `Subject` type.
		  * @tparam I the `Int` literal type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
		  */
		def apply[I <: Numeral, T](idx :I)(implicit get :GetSchemaComponent[I, Unpacked, Components, T, |-|[T, _, _]])
				:MappingExtract[Packed, T, Origin] =
			get.extract(schema, idx)

		/** Returns the component at the given position in the schema. The return type is a projection
		  * from the base `|-|` (or its related subclass) of unknown `Origin` to a full `MappingSchema`
		  * with the same `Origin` type as this instance.
		  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
		  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
		  *            on the component list `C`.
		  * @param project an implicit specifying the proper `SchemaMapping` subclass for the accessed mapping `M`.
		  * @return a component at the `idx` position on the component list `C`
		  *         of a subtype of `SchemaMapping[T, _, _, O]`.
		  * @tparam M the full type of the returned component, as present on the component list `C`.
		  * @tparam I the `Int` literal type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N) MappingSchema.apply(label)]]
		  */
		def /[I <: Numeral, T, M <: |-|[T, _, _]]
		     (idx :I)(implicit get :GetSchemaComponent[I, Unpacked, Components, T, M], project :OriginProjection[M, T])
				:project.WithOrigin[Origin] =
			project(get(schema, idx))

	}



	/** A `MappingSchema` where every component is a column (extending the `SchemaColumn` interface). */
	trait FlatMappingSchema[S, V <: Chain, C <: Chain, O] extends MappingSchema[S, V, C, O] {

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
				:FlatMappingSchema[S, I, P, O]

//		override def lastExtract[T](implicit nonEmpty :C <:< (Chain ~ |-|[T, _ <: Chain, _ <: Chain])) :ColumnMappingExtract[S, T, O] =
//			extract(last)
		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V, C, O]

		override def toString :String = "FlatSchema[" + members + "]"
	}






	/** A `MappingSchema` with factory methods for schema columns and components used to build
	  * (in a purely functional way) a `MappingSchema` and a `SchemaMapping` by chaining calls.
	  * This is a separate class from the `MappingSchema` as different schema variants have slightly
	  * different methods with conflicting signatures.
	  */
	sealed trait ExtensibleMappingSchema[S, V <: Chain, C <: Chain, O] extends MappingSchema[S, V, C, O] {
		/** Prefix appended to all column names. */
		protected def prefix :String

		private[MappingSchema] final def columnPrefix :String = prefix


		override def export[T](component :Component[T]) :Component[T] = component

		override def export[T](column :Column[T]) :Column[T] = column

		protected[schema] def conveyBuffs[T](extractor :S => T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) packedBuffs.flatMap(_.cascade(extractor))
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: packedBuffs.flatMap(_.cascade(extractor))

		protected[schema] def conveyBuffs[T](extractor :S =?> T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) cascadeBuffs(packedBuffs, toString)(extractor)
			else if (packedBuffs.isEmpty) buffs
			else buffs ++: cascadeBuffs(packedBuffs, toString)(extractor)



		protected[schema] def append[T, M <: |-|[T, _ <: Chain, _ <: Chain]]
		                            (component :M, extract :MappingExtract[S, T, O])
				:ExtensibleMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptySchema[S, V, C, T, M, O](this, component, extract)


		protected def append[T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC]](component :M, extractor :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptySchema[S, V, C, T, M, O](
				this, component, MappingExtract(component.withOrigin[O])(extractor)
			)


		/** Appends the given component to this schema. This component will not inherit any buffs or column prefix
		  * associated with this schema.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component in any other mapping instance.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[T, MV <: Chain, MC <: Chain](component: |-|[T, MV, MC], value :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			append[T, MV, MC, |-|[T, MV, MC]](component, value)

		/** Appends the given component to this schema. This component will not inherit any buffs or column prefix
		  * associated with this schema.
		  * @param component a `SchemaMapping` of any origin type to add as the component. It must be a unique object,
		  *                  not appearing as a component in any other mapping instance.
		  * @param value     an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain](component: @|-|[N, T, MV, MC], value :S =?> T)
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			append[T, MV, MC, @|-|[N, T, MV, MC]](component, value)



		/** Appends a new component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[T, MV <: Chain, MC <: Chain](value :S => T, buffs :Buff[T]*)
		                                     (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			comp(value, component(prefix, conveyBuffs(value, buffs)))

		/** Appends a new component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method.
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
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[T, MV <: Chain, MC <: Chain](value :S => T, columnPrefix :String, buffs :Buff[T]*)
		                                     (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			comp(value, component(prefix + columnPrefix, conveyBuffs(value, buffs)))

		/** Appends the given component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param component a `SchemaMapping` of any origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[T, MV <: Chain, MC <: Chain](value :S => T, component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			append(component, MappingExtract.req(component.withOrigin[O])(value))

		/** Appends a new labeled component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The label can be used to access the component by passing it as the argument
		  * to the [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport./, /]] method.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema. The label serves the purpose
		  *              of identifying the component and providing a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain]
		        (label :N, value :S => T, buffs :Buff[T]*)(component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(value, component(prefix, conveyBuffs(value, buffs)) labeled[N] label)

		/** Appends a new labeled component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The label can be used to access the component by passing it as the argument
		  * to the [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport./, /]] method.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema. The label serves the purpose
		  *              of identifying the component and providing a direct access to it.
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
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain]
		        (label :N, value :S => T, columnPrefix :String, buffs :Buff[T]*)
		        (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(value, component(prefix + columnPrefix, conveyBuffs(value, buffs)) labeled[N] label)

		/** Appends a new labeled component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param component the component to append.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain](label :N, value :S => T, component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			comp(value, component labeled[N] label)

		/** Appends the given component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param component a `SchemaMapping` of any origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, T, MV <: Chain, MC <: Chain](value :S => T, component: @|-|[N, T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			append(component, MappingExtract.req(component.withOrigin[O])(value))



		/** Appends a new optional component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The extractor function may not produce a value for all instances
		  * of the subject type `S`, in which case the component will be omitted from a database write. The impact
		  * its lack will have on the assembly of the ''packed'' value depends on the implementation of the outer mapping
		  * based on this schema. Built in implementations, created directly from this schema by mapping the
		  * value chain `V` with this type's special `map` and `optMap` methods, will not produce a value
		  * if any of the optional components is missing, as the value chain `V` will be impossible to assemble.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[T, MV <: Chain, MC <: Chain](value :S => Option[T], buffs :Buff[T]*)
		                                        (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			optcomp(value, component(prefix, conveyBuffs(Extractor(value), buffs)))

		/** Appends a new optional component to this schema. The component will inherit any column prefix and all buffs
		  * provided for the outer mapping of `S` at the initialization of this schema. Inherited buffs will follow
		  * any buffs passed to this method. The extractor function may not produce a value for all instances
		  * of the subject type `S`, in which case the component will be omitted from a database write. The impact
		  * its lack will have on the assembly of the ''packed'' value depends on the implementation of the outer mapping
		  * based on this schema. Built in implementations, created directly from this schema by mapping the
		  * value chain `V` with this type's special `map` and `optMap` methods, will not produce a value
		  * if any of the optional components is missing, as the value chain `V` will be impossible to assemble.
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
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[T, MV <: Chain, MC <: Chain](value :S => Option[T], columnPrefix :String, buffs :Buff[T]*)
		                                        (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			optcomp(value, component(prefix + columnPrefix, conveyBuffs(Extractor(value), buffs)))


		/** Appends the given component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param component a `SchemaMapping` of any origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[T, MV <: Chain, MC <: Chain](value :S => Option[T], component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ |-|[T, MV, MC], O] =
			append(component, MappingExtract.opt(component.withOrigin[O])(value))

		/** Appends a new labeled, optional component to this schema. The component will inherit any column prefix
		  * and all buffs provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method. The label can be used to access the component
		  * by passing it as the argument to the
		  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport./, /]] method. The extractor function
		  * may not produce a value for all instances of the subject type `S`, in which case the component
		  * will be omitted from a database write. The impact its lack will have on the assembly of the ''packed'' value
		  * depends on the implementation of the outer mapping based on this schema.
		  * Mappings based directly on this schema, created with this instance's special `map` and `optMap` methods
		  * will not produce a value if any of the optional components is missing, as the value chain `V`
		  * will be impossible to assemble.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema. The label serves the purpose
		  *              of identifying the component and providing a direct access to it.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component in addition to any inherited from this schema.
		  * @param component constructor function for the component accepting the column name prefix and buffs
		  *                  which should be bestowed on the included component instance.
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain]
		           (label :N, value :S => Option[T], buffs :Buff[T]*)(component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			optcomp(value, component(prefix, conveyBuffs(Extractor(value), buffs)) labeled label)

		/** Appends a new labeled, optional component to this schema. The component will inherit any column prefix
		  * and all buffs provided for the outer mapping of `S` at the initialization of this schema.
		  * Inherited buffs will follow any buffs passed to this method. The label can be used to access the component
		  * by passing it as the argument to the
		  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaSupport./, /]] method. The extractor function
		  * may not produce a value for all instances of the subject type `S`, in which case the component
		  * will be omitted from a database write. The impact its lack will have on the assembly of the ''packed'' value
		  * depends on the implementation of the outer mapping based on this schema.
		  * Mappings based directly on this schema, created with this instance's special `map` and `optMap` methods
		  * will not produce a value if any of the optional components is missing, as the value chain `V`
		  * will be impossible to assemble.
		  * @param label a `String` literal (or just a singleton type in generic code) which will be attached
		  *              to the created component in order to turn into a `LabeledMapping` instance of
		  *              [[net.noresttherein.oldsql.schema.SchemaMapping.@|-| @|-|]], the form in which it will
		  *              appear at the end of the component list of the returned schema. The label serves the purpose
		  *              of identifying the component and providing a direct access to it.
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
		  * @return a `MappingSchema` with the same column prefix and packed buffs, and with the created component
		  *         appended to its component list `C`.
		  * @tparam T the subject type of the new component.
		  * @tparam MV the ''unpacked'' form of the new component - a `Chain` containing the subject types
		  *            of all components in the returned schema.
		  * @tparam MC a `Chain` containing all subcomponents of the new component.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain]
		           (label :N, value :S => Option[T], columnPrefix :String, buffs :Buff[T]*)
		           (component :(String, Seq[Buff[T]]) => |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			optcomp(value, component(prefix, conveyBuffs(Extractor(value), buffs)) labeled label)

		/** Appends a new labeled component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param component a mapping to append as a component to this schema under the given label.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain](label :N, value :S => Option[T], component: |-|[T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			optcomp(value, component labeled label)

		/** Appends the given component to this schema. The component will not inherit any column prefix or buffs
		  * associated with this schema.
		  * @param component a `SchemaMapping` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[N <: Label, T, MV <: Chain, MC <: Chain](value :S => Option[T], component: @|-|[N, T, MV, MC])
				:ExtensibleMappingSchema[S, V ~ T, C ~ @|-|[N, T, MV, MC], O] =
			append(component, MappingExtract.opt(component.withOrigin[O])(value))



		/** Appends a new column to this schema with the given name. The column will have the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  */
		def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[S, V ~ T, C ~ ||[T], O] =
		{
			val column = SchemaColumn[T, O](name, conveyBuffs(value, buffs) :_*)
			append(column, ColumnExtract.req(column.withOrigin[O])(value))
		}

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
			append(column, ColumnExtract.req(column)(value))
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




		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject
		  * from the values of all top-level components of this schema. The function can take many forms, both in terms
		  * of the argument and the returned value. For the latter, both `S` and `Option[S]` results are supported.
		  * As for the former, it can take either the value chain `V` with values of all top-level components
		  * (the subject of this schema) or any other set of arguments for which an implicit
		  * [[net.noresttherein.oldsql.collection.Chain.ChainApplication ChainApplication]][V, F, S]
		  * (or `ChainApplication[V, F, Option[S]]`) exists, allowing the indirect application of the value chain.
		  * Alternatively, it can take the subjects of all top-level components as individual arguments, in the order
		  * in which they appear on the value and component chains. The latter approach is recommended, because it
		  * avoids assembling the value chain by this schema altogether, as well as the linear access tax of elements
		  * from the chain. This allows direct passing of factory methods from companion objects as arguments
		  * to this method. Note that the values of components are accessed 'forcibly'
		  * from the [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] passed for assembly rather than
		  * by the `Option` returning `get` method and, instead, `NoSuchElementException` exceptions are caught
		  * and translated to a `None` result in the [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
		  * method. The created `Mapping`, regardless if by mapping the value chain or using direct component access,
		  * will thus require the values for all listed components to be present in order for the whole assembly
		  * to succeed.
		  *
		  * The `SchemaMapping` created by this method will be thus more efficient both in assembly and disassembly
		  * than a mapping created with the standard [[net.noresttherein.oldsql.schema.Mapping.map map]] method
		  * as declared by `Mapping` by skipping the intermediate steps.
		  * @param constructor a function whose argument(s) contain all the subjects of components in the chain `C`.
		  *                    It can take either: a) the value chain `V` itself; b) a scala tuple with the same elements;
		  *                    c) subjects of all components as separate arguments.
		  *                    It must return either `S` or `Option[S]`.
		  */
		def map[F](constructor :F)(implicit apply :SubjectConstructor[S, V, C, O, F]) :SchemaMapping[S, V, C, O] =
			new MappedSchema(this, constructor, packedBuffs)

//		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
//		  * provided with component and column definitions when building this schema for disassembly of its subject
//		  * before writing to the database, and the function specified here for assembling its subject from the
//		  * chain of subjects of all top-level components of this schema. Note that if, during the assembly,
//		  * any of the optional components defined in this schema fails to produce a value, the whole chain `V`
//		  * with the unpacked values will be impossible to assemble, and the whole assembly of the packed subject `S`
//		  * will yield no value.
//		  * @param constructor a function accepting a chain with the values of all components as they appear in the
//		  *                    components chain `C`.
//		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
//		  */
//		def map(constructor :V => S) : SchemaMapping[S, V, C, O] =
//			map[V => S](constructor)(SubjectConstructor.map())
//
//		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
//		  * provided with component and column definitions when building this schema for disassembly of its subject
//		  * before writing to the database, and the function specified here for assembling its subject from the
//		  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
//		  * the subject value for all input rows; this will also be the case if any of the optional components
//		  * in the schema fail to produce a value, as the intermediate value chain `V` will be impossible to assemble.
//		  * This method is preferable to the standard `Mapping` method it overloads because the values for the
//		  * components will be extracted directly from the subject value `S`, without building an intermediate
//		  * 'unpacked' value chain during disassembly.
//		  * @param constructor a function accepting a chain with the values of all components as they appear in the
//		  *                    components chain `C`.
//		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.optMap]]
//		  */
//		def optMap(constructor :V => Option[S]) :SchemaMapping[S, V, C, O] =
//			map[V => Option[S]](constructor)(SubjectConstructor.optMap())


	}






	/** A `FlatMappingSchema` with factory methods for schema columns and components used to build a
	  * `FlatMappingSchema` (and a `FlatSchemaMapping`) by chaining calls with component declarations.
	  * This class extends `ExtensibleMappingSchema`, but inherited non-column component factory methods
	  * switch back to building a general `MappingSchema` so that the process can diverge at any time.
	  */
	sealed trait ExtensibleFlatMappingSchema[S, V <: Chain, C <: Chain, O]
		extends ExtensibleMappingSchema[S, V, C, O] with FlatMappingSchema[S, V, C, O]
	{

		protected[schema] def col[T, M <: ||[T]](column :M, extract :MappingExtract[S, T, O])
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptyFlatSchema(this, column, extract)


		protected def col[T, M <: ||[T]](column :M, value :S =?> T)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			new ExtensibleNonEmptyFlatSchema[S, V, C, T, M, O](
				this, column, MappingExtract(column.withOrigin[O])(value)
			)

		protected def col[T, M <: ||[T]](value :S => T, column :M) :ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			col(column, ColumnExtract.req(column.withOrigin[O])(value))

		override def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(value, SchemaColumn[T, O](name, buffs:_*))

		override def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)



		protected def optcol[T, M <: ||[T]](value :S => Option[T], column :M)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O] =
			col(column, ColumnExtract.opt(column.withOrigin[O])(value))

		override def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			optcol(value, SchemaColumn[T, O](name, buffs :_*))

		override def optcol[T](value :S => Option[T], buffs :Buff[T]*)(implicit form :ColumnForm[T], tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ ||[T], O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		override def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(value, LabeledSchemaColumn[N, T, O](name, buffs :_*))

		override def lbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			col(value, LabeledSchemaColumn[N, T, O](label, name, buffs :_*))

		override def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			optcol(value, LabeledSchemaColumn[N, T, O](name, buffs :_*))

		override def optlbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[S, V ~ T, C ~ (N @|| T), O] =
			optcol(value, LabeledSchemaColumn[N, T, O](label, name, buffs :_*))



		override def map[F](constructor :F)(implicit apply :SubjectConstructor[S, V, C, O, F]) :FlatSchemaMapping[S, V, C, O] =
			new MappedFlatSchema(this, constructor, packedBuffs)

//		override def map(constructor :V => S) :FlatSchemaMapping[S, V, C, O] =
//			map[V => S](constructor)
//
//		override def optMap(constructor :V => Option[S]) :FlatSchemaMapping[S, V, C, O] =
//			map[V => Option[S]](constructor)

	}






	/** Additional methods for `MappingSchema` instances which could cause conflicts in multiple inheritance scenarios. */
	implicit class MappingSchemaExtension[S, V <: Chain, C <: Chain, O]
	                                     (private val self :MappingSchema[S, V, C, O]) extends AnyVal
	{
		/** Transforms this schema into an equivalent `FlatMappingSchema` by recursively replacing each component in the
		  * chain `C` with its columns. This process loses all information about replaced components and the new schema
		  * does not reference this instance in any way other than using the same column instances. It is however
		  * intended to be used as part of inlining of the enclosing mapping, which will retain the references to
		  * all components and use them for the assembly exactly as the original owning mapping. The operation
		  * can be useful in mapping specific select statements by matching the schema's columns to the columns
		  * in the ''select'' clause.
		  * This method is extracted from the schema class to an extension method to avoid conflict with the like
		  * method in `SchemaMapping` for `ChainMapping` and similar classes which implement both `SchemaMapping`
		  * and `MappingSchema` traits.
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.flatten]]
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
					init(prefix, suffix.prev).col(suffix.last, suffix.lastExtract)
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

		implicit def appendFlattenedIndexedComponent[V <: IndexedChain, C <: Chain, PV <: Chain, PC <: Chain,
		                                             K <: IndexedChain.Key, T, MV <: Chain, MC <: Chain, M <: |-|[T, MV, MC],
		                                             SV <: Chain,  SC <: Chain, FV <: Chain, FC <: Chain]
		                                            (implicit prefix :SchemaFlattening[V, C, PV, PC],
		                                             hint :Conforms[M, M, |-|[T, MV, MC]],
		                                             flatten :SchemaFlattening[MV, MC, SV, SC],
		                                             concat :ColumnSchemaConcat[PV, PC, SV, SC, FV, FC])
				:SchemaFlattening[V |~ (K :~ T), C ~ M, FV, FC] =
			appendFlattenedComponent(prefix, hint, flatten, concat)
				.asInstanceOf[SchemaFlattening[V |~ (K :~ T), C ~ M, FV, FC]]

	}

	object SchemaFlattening extends ComponentSchemaFlattening {

		implicit def appendColumn[V <: Chain, C <: Chain, T, M <: ||[T], FV <: Chain, FC <: Chain]
		                         (implicit init :SchemaFlattening[V, C, FV, FC])
				:SchemaFlattening[V ~ T, C ~ M, FV ~ T, FC ~ M] =
			new SchemaFlattening[V ~ T, C ~ M, FV ~ T, FC ~ M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O]) =
					init(schema.prev).col(schema.last, schema.lastExtract)
			}

		implicit def appendIndexedColumn[V <: IndexedChain, C <: Chain, K <: IndexedChain.Key, T,
		                                 M <: ||[T], FV <: Chain, FC <: Chain]
		                                (implicit init :SchemaFlattening[V, C, FV, FC])
				:SchemaFlattening[V |~ (K :~ T), C ~ M, FV ~ T, FC ~ M] =
			appendColumn(init).asInstanceOf[SchemaFlattening[V |~ (K :~ T), C ~ M, FV ~ T, FC ~ M]]
	}






	@implicitNotFound("No ${M} <: @|-|[${N}, ${T}, _, _] present in the schema ${C}\n(with values ${V}).")
	sealed abstract class GetLabeledComponent[N <: Label, V <: Chain, C <: Chain, T, +M <: @|-|[N, T, _, _]] {
		def apply[S, O](schema :MappingSchema[S, V, C, O], label :N) :M
		def extract[S, O](schema :MappingSchema[S, V, C, O], label :N) :MappingExtract[S, T, O]
	}



	object GetLabeledComponent {

		implicit def last[N <: Label, V <: Chain, C <: Chain, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain]]
				:GetLabeledComponent[N, V ~ T, C ~ M, T, M] =
			new GetLabeledComponent[N, V ~ T, C ~ M, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ T, C ~ M, O], label :N) = schema.last

				override def extract[S, O](schema: MappingSchema[S, V ~ T, C ~ M, O], label :N) =
					schema.lastExtract
			}

		implicit def lastIndexed[N <: Label, V <: IndexedChain, C <: Chain, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain]]
				:GetLabeledComponent[N, V |~ (N :~ T), C ~ M, T, M] =
			last[N, V, C, T, M].asInstanceOf[GetLabeledComponent[N, V |~ (N :~ T), C ~ M, T, M]]


		implicit def previous[N <: Label, V <: Chain, C <: Chain, X, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain], L <: Mapping]
		                     (implicit get :GetLabeledComponent[N, V, C, T, M])
				:GetLabeledComponent[N, V ~ X, C ~ L, T, M] =
			new GetLabeledComponent[N, V ~ X, C ~ L, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], label :N) =
					get(schema.prev, label)

				override def extract[S, O](schema: MappingSchema[S, V ~ X, C ~ L, O], label :N) =
					get.extract(schema.prev, label)
			}

		implicit def previousIndexed[N <: Label, V <: IndexedChain, C <: Chain, X <: (_ <: Label) :~ _,
			                         T, M <: @|-|[N, T, _ <: Chain, _ <: Chain], L <: Mapping]
		                            (implicit get :GetLabeledComponent[N, V, C, T, M])
				:GetLabeledComponent[N, V |~ X, C ~ L, T, M] =
			previous[N, V, C, X, T, M, L].asInstanceOf[GetLabeledComponent[N, V |~ X, C ~ L, T, M]]
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

		implicit def indexedSingleton[N <: Label, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain]]
				:GetSchemaComponent[0, @~ |~ (N :~ T), @~ ~ M, T, M] =
			singleton[T, M].asInstanceOf[GetSchemaComponent[0, @~ |~ (N :~ T), @~ ~ M, T, M]]


		implicit def last[I <: Numeral, J <: Numeral, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain,  _ <: Chain]]
		                 (implicit inc :Inc[I, J], size :GetSchemaComponent[I, V, C, _, _])
				:GetSchemaComponent[J, V ~ T, C ~ M, T, M] =
			singleton[T, M].asInstanceOf[GetSchemaComponent[J, V ~ T, C ~ M, T, M]]

		implicit def lastIndexed[I <: Numeral, J <: Numeral, V <: IndexedChain, C <: Chain,
			                     N <: Label, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain]]
		                        (implicit inc :Inc[I, J], size :GetSchemaComponent[I, V, C, _, _])
				:GetSchemaComponent[J, V |~ (N :~ T), C ~ M, T, M] =
			last[I, J, V, C, T, M].asInstanceOf[GetSchemaComponent[J, V |~ (N :~ T), C ~ M, T, M]]


		implicit def previous[I <: Numeral, V <: Chain, C <: Chain, X, L <: Mapping,
		                      T, M <: |-|[T, _ <: Chain, _ <: Chain]]
		                     (implicit get :GetSchemaComponent[I, V, C, T, M])
				:GetSchemaComponent[I, V ~ X, C ~ L, T, M] =
			new GetSchemaComponent[I, V ~ X, C ~ L, T, M] {
				override def apply[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], idx :I) = get(schema.prev, idx)

				override def extract[S, O](schema :MappingSchema[S, V ~ X, C ~ L, O], idx :I) =
					get.extract(schema.prev, idx)
			}

		implicit def previousIndexed[I <: Numeral, V <: IndexedChain, C <: Chain, X <: (_ <: Label) :~ _, L <: Mapping,
		                             N <: Label, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain]]
		                            (implicit get :GetSchemaComponent[I, V, C, T, M])
				:GetSchemaComponent[I, V |~ X, C ~ L, T, M] =
			previous[I, V, C, X, L, T, M].asInstanceOf[GetSchemaComponent[I, V |~ X, C ~ L, T, M]]
	}






	/** Implicit rules of application of some functional type `F` to the values of components
	  * from a `MappingSchema[S, V, C, O]`, in order to produce the target subject type `S` of the `SchemaMapping`
	  * owning the schema.
	  */
	@implicitNotFound("I don't know how to use ${F} to construct the subject ${S} from chain ${V}.\n" +
	                  "Missing implicit SubjectConstructor[${S}, ${V}, ${C}, ${O}, ${F}].")
	trait SubjectConstructor[S, V <: Chain, C <: Chain, O, F] {
		def apply(schema :MappingSchema[S, V, C, O], f :F) :ComponentValues[S, O] => Option[S]
	}


	sealed abstract class ChainSubjectConstructors {
		/** Applies a value of the functional type `F` to the value chain `V` assembled by a `MappingSchema[S, V, C, O]`
		  * using an implicitly available [[net.noresttherein.oldsql.collection.Chain.ChainApplication ChainApplication]].
		  * This provides implicit values adapting constructor functions accepting either the value chain `F` itself,
		  * all its elements as separate parameters, or a tuple consisting of the same elements as the chain `V`.
		  */
		implicit def mapValueChain[S, V <: Chain, C <: Chain, O, F]
		                          (implicit apply :ChainApplication[V, F, S]) :SubjectConstructor[S, V, C, O, F] =
			(schema :MappingSchema[S, V, C, O], f :F) =>
				(pieces :ComponentValues[S, O]) => pieces.get(schema).map(apply(f, _))

		/** Applies a value of the functional type `F` to the value chain `V` assembled by a `MappingSchema[S, V, C, O]`
		  * using an implicitly available [[net.noresttherein.oldsql.collection.Chain.ChainApplication ChainApplication]].
		  * This provides implicit values adapting constructor functions accepting either the value chain `F` itself,
		  * all its elements as separate parameters, or a tuple consisting of the same elements as the chain `V`.
		  */
		implicit def optMapValueChain[S, V <: Chain, C <: Chain, O, F]
		                             (implicit apply :ChainApplication[V, F, Option[S]]) :SubjectConstructor[S, V, C, O, F] =
			(schema :MappingSchema[S, V, C, O], f :F) =>
				(pieces :ComponentValues[S, O]) => pieces.get(schema).flatMap(apply(f, _))
	}


	object SubjectConstructor extends ChainSubjectConstructors {

		def apply[S, V <: Chain, C <: Chain, O, F]
		         (construct :(MappingSchema[S, V, C, O], F) => ComponentValues[S, O] => Option[S])
				:SubjectConstructor[S, V, C, O, F] =
			(schema :MappingSchema[S, V, C, O], f :F) => construct(schema, f)

		def apply[S, V <: Chain, C <: Chain, O, F]
		         (construct :(MappingSchema[S, V, C, O], F, ComponentValues[S, O]) => Option[S])
				:SubjectConstructor[S, V, C, O, F] =
			(schema :MappingSchema[S, V, C, O], f :F) => (pieces :ComponentValues[S, O]) => construct(schema, f, pieces)


		/** A [[net.noresttherein.oldsql.schema.MappingSchema.SubjectConstructor SubjectConstructor]] which assembles
		  * the value chain `V` using the provided schema and maps the result with to the intended subject type.
		  */
		implicit def map[S, V <: Chain, C <: Chain, O]() :SubjectConstructor[S, V, C, O, V => S] =
			(schema :MappingSchema[S, V, C, O], f :V => S) =>
				(pieces :ComponentValues[S, O]) => pieces.get(schema).map(f)

		/** A [[net.noresttherein.oldsql.schema.MappingSchema.SubjectConstructor SubjectConstructor]] which assembles
		  * the value chain `V` using the provided schema and flat maps the result with to the intended subject type.
		  */
		implicit def optMap[S, V <: Chain, C <: Chain, O]() :SubjectConstructor[S, V, C, O, V => Option[S]] =
			(schema :MappingSchema[S, V, C, O], f :V => Option[S]) =>
				(pieces :ComponentValues[S, O]) => pieces.get(schema).flatMap(f)


		@inline def SAM[S, V <: Chain, C <: Chain, O, F]
		               (construct :SubjectConstructor[S, V, C, O, F]) :SubjectConstructor[S, V, C, O, F] =
			construct



		type X[T] = |-|[T, _ <: Chain, _ <: Chain]

		@inline private def get[S, T, O](schema :MappingSchema[S, _ <: Chain, _ <: Chain ~ X[T], O])
		                                (implicit pieces :ComponentValues[S, O]) :T =
			pieces(schema.lastExtract)



		implicit def optMap1[Vs <: Chain, CA <: X[A], A, Y, Z] = SAM {
			(schema :MappingSchema[Y, Vs, @~ ~CA, Z], f :A => Option[Y]) => (pieces :ComponentValues[Y, Z]) =>
				pieces.get(schema.lastExtract) flatMap f
		}

		implicit def optMap2[Vs <: @~ ~_~_, CA <: X[A], A, CB <: X[B], B, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB, Z], f :(A, B) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = schema.prev
							f(get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap3[Vs <: @~ ~_~_~_, CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC, Z], f :(A, B, C) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev
							f(get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap4[Vs <: @~ ~_~_~_~_, CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD, Z], f :(A, B, C, D) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev
							f(get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap5[Vs <: @~ ~_~_~_~_~_,
		                     CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE, Z], f :(A, B, C, D, E) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							f(get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap6[Vs <: @~ ~_~_~_~_~_~_,
			                 CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                     Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF, Z], f :(A, B, C, D, E, F) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev
							f(get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap7[Vs <: @~ ~_~_~_~_~_~_~_,
		                     CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                     CG <: X[G], G,
		                     Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG, Z], f :(A, B, C, D, E, F, G) => Option[Y]) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev
							f(get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def optMap8[Vs <: @~ ~_~_~_~_~_~_~_~_,
		                     CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                     CG <: X[G], G, CH <: X[H], H,
		                     Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH, Z],
				 f :(A, B, C, D, E, F, G, H) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev
							f(get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap9[Vs <: @~ ~_~_~_~_~_~_~_~_~_,
		                     CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                     CG <: X[G], G, CH <: X[H], H, CI <: X[I], I,
		                     Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI, Z],
				 f :(A, B, C, D, E, F, G, H, I) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev
							f(get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap10[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ, Z],
				 f :(A, B, C, D, E, F, G, H, I, J) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							f(get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap11[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev
							f(get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap12[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev
							f(get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap13[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev
							f(get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap14[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev
							f(get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap15[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							f(get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap16[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev
							f(get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap17[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev
							f(get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap18[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev
							f(get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap19[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                      CS <: X[S], S,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev
							f(get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap20[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                      CS <: X[S], S, CT <: X[T], T,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							f(get(c19),
							  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap21[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_ ~ _,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                      CS <: X[S], S, CT <: X[T], T, CU <: X[U], U,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT~CU, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							val c20 = c19.prev
							f(get(c20), get(c19),
							  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def optMap22[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_ ~ _,
		                      CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                      CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                      CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                      CS <: X[S], S, CT <: X[T], T, CU <: X[U], U, CV <: X[V], V,
		                      Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT~CU~CV, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Option[Y]) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							val c20 = c19.prev; val c21 = c20.prev
							f(get(c21), get(c20), get(c19),
							  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
							  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}



		implicit def map1[Vs <: Chain, CA <: X[A], A, Y, Z] = SAM {
			(schema :MappingSchema[Y, Vs, @~ ~CA, Z], f :A => Y) => (pieces :ComponentValues[Y, Z]) =>
				pieces.get(schema.lastExtract) map f
		}

		implicit def map2[Vs <: @~ ~_~_, CA <: X[A], A, CB <: X[B], B, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB, Z], f :(A, B) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = schema.prev
							Some(f(get(c1), get(c0)))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map3[Vs <: @~ ~_~_~_, CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC, Z], f :(A, B, C) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev
							Some(f(get(c2), get(c1), get(c0)))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map4[Vs <: @~ ~_~_~_~_, CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD, Z], f :(A, B, C, D) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev
							Some(f(get(c3), get(c2), get(c1), get(c0)))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map5[Vs <: @~ ~_~_~_~_~_,
		                  CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE, Z], f :(A, B, C, D, E) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							Some(f(get(c4), get(c3), get(c2), get(c1), get(c0)))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map6[Vs <: @~ ~_~_~_~_~_~_,
			              CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                  Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF, Z], f :(A, B, C, D, E, F) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev
							Some(f(get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)))
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map7[Vs <: @~ ~_~_~_~_~_~_~_,
		                  CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                  CG <: X[G], G,
		                  Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG, Z], f :(A, B, C, D, E, F, G) => Y) => {
					implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev
							Some(
								f(get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
							)
						} catch {
							case _ :NoSuchElementException => None
						}
				}
			}

		implicit def map8[Vs <: @~ ~_~_~_~_~_~_~_~_,
		                  CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                  CG <: X[G], G, CH <: X[H], H,
		                  Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH, Z],
				 f :(A, B, C, D, E, F, G, H) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev
							Some(
								f(get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map9[Vs <: @~ ~_~_~_~_~_~_~_~_~_,
		                  CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                  CG <: X[G], G, CH <: X[H], H, CI <: X[I], I,
		                  Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI, Z],
				 f :(A, B, C, D, E, F, G, H, I) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev
							Some(
								f(get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map10[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ, Z],
				 f :(A, B, C, D, E, F, G, H, I, J) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							Some(
								f(get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0))
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map11[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev
							Some(
								f(get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map12[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev
							Some(
								f(get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map13[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev
							Some(
								f(get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map14[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev
							Some(
								f(get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map15[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							Some(
								f(get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map16[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev
							Some(
								f(get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map17[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev
							Some(
								f(get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map18[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev
							Some(
								f(get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map19[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                   CS <: X[S], S,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev
							Some(
								f(get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map20[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                   CS <: X[S], S, CT <: X[T], T,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							Some(
								f(get(c19),
								  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map21[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_ ~ _,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                   CS <: X[S], S, CT <: X[T], T, CU <: X[U], U,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT~CU, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							val c20 = c19.prev
							Some(
								f(get(c20), get(c19),
								  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
			}

		implicit def map22[Vs <: @~ ~_~_~_~_~_~_~_~_~_~_ ~ _~_~_~_~_~_~_~_~_~_ ~ _,
		                   CA <: X[A], A, CB <: X[B], B, CC <: X[C], C, CD <: X[D], D, CE <: X[E], E, CF <: X[F], F,
		                   CG <: X[G], G, CH <: X[H], H, CI <: X[I], I, CJ <: X[J], J, CK <: X[K], K, CL <: X[L], L,
		                   CM <: X[M], M, CN <: X[N], N, CO <: X[O], O, CP <: X[P], P, CQ <: X[Q], Q, CR <: X[R], R,
		                   CS <: X[S], S, CT <: X[T], T, CU <: X[U], U, CV <: X[V], V,
		                   Y, Z] =
			SAM {
				(schema :MappingSchema[Y, Vs, @~ ~CA~CB~CC~CD~CE~CF~CG~CH~CI~CJ~CK~CL~CM~CN~CO~CP~CQ~CR~CS~CT~CU~CV, Z],
				 f :(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => Y) =>
					{ implicit pcs :ComponentValues[Y, Z] =>
						try {
							val c0 = schema; val c1 = c0.prev; val c2 = c1.prev; val c3 = c2.prev; val c4 = c3.prev
							val c5 = c4.prev; val c6 = c5.prev; val c7 = c6.prev; val c8 = c7.prev; val c9 = c8.prev
							val c10 = c9.prev; val c11 = c10.prev; val c12 = c11.prev; val c13 = c12.prev; val c14 = c13.prev
							val c15 = c14.prev; val c16 = c15.prev; val c17 = c16.prev; val c18 = c17.prev; val c19 = c18.prev
							val c20 = c19.prev; val c21 = c20.prev
							Some(
								f(get(c21), get(c20), get(c19),
								  get(c18), get(c17), get(c16), get(c15), get(c14), get(c13), get(c12), get(c11), get(c10),
								  get(c9), get(c8), get(c7), get(c6), get(c5), get(c4), get(c3), get(c2), get(c1), get(c0)
								)
							)
						} catch {
							case _ :NoSuchElementException => None
						}
					}
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



		override def compose[X](extractor :X =?> S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]



		protected[schema] override def componentsReversed :List[Nothing] = Nil
		protected[schema] override def subcomponentsReversed :List[Component[_]] = Nil
		protected[schema] override def columnsReversed :List[Column[_]] = Nil
	}



	private[schema] class ExtensibleEmptySchema[S, O](protected override val prefix :String = "",
	                                                  protected override val packedBuffs :Seq[Buff[S]] = Nil)
		extends EmptySchema[S, O] with ExtensibleFlatMappingSchema[S, @~, @~, O]
	{
		override def compose[X](extractor :X =?> S) :ExtensibleEmptySchema[X, O] =
			new ExtensibleEmptySchema[X, O](prefix)
	}



	object EmptySchema {
		private[this] val empty = new ExtensibleEmptySchema[Any, Any]

		def apply[S, O]() :ExtensibleFlatMappingSchema[S, @~, @~, O] = empty.asInstanceOf[ExtensibleEmptySchema[S, O]]

		def apply[S, O](buffs :Seq[Buff[S]]) :ExtensibleFlatMappingSchema[S, @~, @~, O] =
			new ExtensibleEmptySchema[S, O]("", buffs)

		def apply[S, O](prefix :String, buffs :Seq[Buff[S]]) :ExtensibleFlatMappingSchema[S, @~, @~, O] =
			new ExtensibleEmptySchema[S, O](prefix, buffs)

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
			(init.extracts.map(composeExtractAssoc(initExtract)(_)) ++
				component.withOrigin[O].extracts.map(composeExtractAssoc(componentExtract)(_))
			).updated(init, initExtract).updated(component.withOrigin[O], componentExtract)

		override val columnExtracts :NaturalMap[Column, ColumnExtract] =
			NaturalMap.delayed(selectColumnExtracts(this)(extracts))

		override val packedExtracts :NaturalMap[Component, PackedExtract] =
			init.packedExtracts ++ component.extracts.map(composeExtractAssoc(extractor)(_))
				.updated[PackedExtract, T](component, extractor).updated[PackedExtract, V L E](this, unpackSelf)

		override val packedColumnExtracts = NaturalMap.delayed(selectColumnExtracts(toString)(packedExtracts))



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

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V ~ T, C ~ M, O] =
			new NonEmptySchema(init compose extractor, last, this.extractor compose extractor)
	}



	private[schema] class ExtensibleNonEmptySchema[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain], O]
                          (prev :ExtensibleMappingSchema[S, V, C, O], last :M, extractor :MappingExtract[S, T, O])
		extends NonEmptySchema[S, V, C, T, M, O](prev, last, extractor) with ExtensibleMappingSchema[S, V ~ T, C ~ M, O]
	{
		protected override val prefix = prev.columnPrefix
		protected override val packedBuffs :Seq[Buff[S]] = init.packedBuffs
	}






	private[schema] class NonEmptyFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T], O]
	                                        (override val init :FlatMappingSchema[S, V, C, O], next :M,
	                                         get :MappingExtract[S, T, O])
		extends NonEmptySchema[S, V, C, T, M, O](init, next, get)
		   with BaseNonEmptyFlatSchema[Chain, Any, ~, S, V, C, T, T, M, O]
	{
		//these shortcut implementations work because column mappings moved their buff handling to their forms.
		override val selectForm = SQLReadForms.ChainReadForm(init.selectForm, last.selectForm)
		override val queryForm = SQLWriteForms.ChainWriteForm(init.queryForm, last.queryForm)
		override val updateForm = SQLWriteForms.ChainWriteForm(init.updateForm, last.updateForm)
		override val insertForm = SQLWriteForms.ChainWriteForm(init.insertForm, last.insertForm)
		override def writeForm(op :WriteOperationType) :SQLWriteForm[V ~ T] = op.form(this)

		override def compose[X](extractor :X =?> S) :NonEmptyFlatSchema[X, V, C, T, M, O] =
			new NonEmptyFlatSchema(init compose extractor, last, this.extractor compose extractor)
	}



	private[schema] class ExtensibleNonEmptyFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T], O]
	                      (prev :ExtensibleFlatMappingSchema[S, V, C, O], next :M, get :MappingExtract[S, T, O])
		extends NonEmptyFlatSchema[S, V, C, T, M, O](prev, next, get)
		   with ExtensibleFlatMappingSchema[S, V ~ T, C ~ M, O]
	{
		protected override val prefix = prev.columnPrefix
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



	/** Result of filtering the components lift of the schema `original`,
	  * adding the components removed from the `members` chain as well as synthetic components specific to `original`.
	  * Used as the backing mapping of `CustomizedSchema`.
	  */
	private[schema] class FilteredSchema[S, V <: Chain, C <: Chain, O]
	                                    (original :MappingSchema[S, _ <: Chain, _ <: Chain, O],
	                                     protected override val backer :MappingSchema[S, V, C, O])
		extends DirectProxy[V, O] with MappingSchemaProxy[S, V, C, O]
	{
		override def apply[T](component :Component[T]) :Extract[T] = extracts(component)
		override def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)
		override def export[T](component :Component[T]) :Component[T] = extracts(component).export
		override def export[T](column :Column[T]) :Column[T] = columnExtracts(column).export


		override val packedExtracts :NaturalMap[Component, PackedExtract] =
			original.packedExtracts.updated[PackedExtract, V](backer, MappingExtract.opt(backer)(backer.unapply)) ++
				backer.packedExtracts

		override val packedColumnExtracts :NaturalMap[Column, PackedColumnExtract] =
			original.packedColumnExtracts ++ backer.packedColumnExtracts

		override val extracts =
			(backer.extracts /: original.extracts) { case (acc, Assoc(component, _)) =>
					if (acc.contains(component)) acc
					else acc.updated[Extract, component.Subject](component, MappingExtract.none(component))
			}.updated[Extract, V](backer, MappingExtract.ident(backer))
		override val columnExtracts = oldsql.schema.selectColumnExtracts(this)(extracts)

		override val components :Unique[Component[_]] = original.components ++ backer.components
		override val subcomponents :Unique[Component[_]] = original.subcomponents ++ backer.subcomponents

		override val columns :Unique[Column[_]] = original.columns
		override val selectable :Unique[Column[_]] = original.selectable
		override val queryable :Unique[Column[_]] = original.queryable
		override val updatable :Unique[Column[_]] = original.updatable
		override val insertable :Unique[Column[_]] = original.insertable
		override val autoUpdated :Unique[Column[_]] = original.autoUpdated
		override val autoInserted :Unique[Column[_]] = original.autoInserted

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O] =
			new FilteredSchema(original compose extractor, backer compose extractor)
	}



	private[schema] class CustomizedSchema[S, V <: Chain, C <: Chain, O]
	                      (original :MappingSchema[S, _ <: Chain, _ <: Chain, O], filtered: MappingSchema[S, V, C, O],
	                       op :OperationType, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]])
		extends CustomizedMapping[MappingSchema[S, V, C, O], V, O](
		                          new FilteredSchema[S, V, C, O](original, filtered), op, include, exclude)
		   with MappingSchemaProxy[S, V, C, O]
	{
		def this(original :MappingSchema[S, _ <: Chain, _ <: Chain, O], filtered :MappingSchema[S, V, C, O],
		         op :OperationType, include :Iterable[RefinedMapping[_, O]]) =
			this(original, filtered, op, include, {
				val components = original.members.toSeq.asInstanceOf[Seq[SchemaMapping[_, _, _, O]]]
				val remaining = filtered.members.toSeq.asInstanceOf[Seq[SchemaMapping[_, _, _, O]]].toSet
				components.filterNot(remaining)
			})

		if (filtered.components.exists(op.prohibited.enabled))
			throw new IllegalArgumentException(
				s"$members is an invalid $op schema as it contains a component with the ${op.prohibited} Buff."
			)

		override def unapply(subject :S) :Option[V] = filtered.unapply(subject)
		override def disassemble(subject :S) :V = filtered.disassemble(subject)
		override def assemble(pieces :Pieces) :Option[V] = filtered.assemble(pieces)

		override def compose[X](extractor :X =?> S) :MappingSchema[X, V, C, O] =
			new CustomizedSchema(original compose extractor, filtered compose extractor, op, include, exclude)
	}



	private[schema] class CustomizedFlatSchema[S, V <: Chain, C <: Chain, O]
	                      (original :FlatMappingSchema[S, _ <: Chain, _ <: Chain, O],
	                       filtered :FlatMappingSchema[S, V, C, O],
	                       op :OperationType, include :Iterable[RefinedMapping[_, O]])
		extends CustomizedSchema[S, V, C, O](original, filtered, op, include)
		   with FlatMappingSchema[S, V, C, O]
	{
		override def prev[I <: Chain, P <: Chain](implicit vals :V <:< (I ~ Any), comps :C <:< (P ~ Any))
				:FlatMappingSchema[S, I, P, O] =
			filtered.prev

		override def compose[X](extractor :X =?> S) :FlatMappingSchema[X, V, C, O] =
			new CustomizedFlatSchema(original compose extractor, filtered compose extractor, op, include)
	}


}


