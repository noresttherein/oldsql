package net.noresttherein.oldsql.schema

import java.sql.{CallableStatement, JDBCType, PreparedStatement, ResultSet}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ArraySeq
import scala.collection.IterableOps

import net.noresttherein.oldsql.{slang, OperationView}
import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, UpdateView, WriteOperationView}
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.{IncompatibleMappingsException, NoSuchComponentException}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalFilter, OptionalInsert, OptionalSelect, OptionalUpdate, SelectPreset, Virtual}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingBound, MappingTemplate, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ArbitraryProjection, ExactProjection, IsomorphicProjection}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormOptLiterals
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.@:
import net.noresttherein.oldsql.schema.bits.LabelPath.Label
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.bits.OptionMapping.Optional
import net.noresttherein.oldsql.schema.support.{MappingAdapter, MappingPrototype, ReorderedMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.sql.{RowProduct, RowShape, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.Single
import net.noresttherein.oldsql.sql.ast.{BoundMappingParam, BoundParam, LooseComponent, MappingLiteral, SQLLiteral}
import net.noresttherein.oldsql.sql.mechanics.RelationCount


//implicits
import slang._






//todo: provide a semi-complete list of base traits to extend.
/** A `Mapping` describes how a Scala type (declared as the member type `Subject` of this trait)
  * is decomposed into the relational format of the underlying database. In particular, it declares all the columns used,
  * but it can have a hierarchical structure with components nested to an arbitrary level. It is bidirectional
  * in its nature and invariant regarding its subject type, but it is entirely possible to provide read-only
  * (or write-only) implementations and mechanisms exist to define which columns are included in which operations,
  * allowing to automatically omit some from all INSERT or UPDATE statements for example. In fact,
  * the choice can be quite arbitrary and done in a case by case basis, for example having `BLOB` columns
  * excluded by default from SelectView statements and requiring their explicit mention. This makes it different
  * from [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], which is used for actual interacting
  * with the JDBC `ResultSet` and `PreparedStatement` API, but uses a fixed, strictly positional column schema
  * and does not carry any information about column names or other meta data.
  *
  * `Mapping` instances used by this mapping to handle separate fragments of the mapped type are referred to as
  * ''components'' throughout the library; it is important to note however that mappings for a database table,
  * for a whole query row of multiple joined tables, an 'Address' component of a handful of columns and a single
  * column are not distinct in nature, with only the columns being treated specially in some circumstances.
  *
  * This polymorphism and structural nature, apart from promoting reusability and encouraging data encapsulation
  * in the domain model rather than flat, closely following the table schema, mappings, have several implications.
  * First, components of a component of a mapping are called sometimes subcomponents when the distinction
  * is being made, but are themselves also valid components of this mapping. It is encouraged that the assembly
  * (and disassembly) process remains strictly hierarchical, with any instance responsible only of handling
  * the transformation between its subject type and the values of its direct components. This allows some optimizations
  * and increases reusability, but is not always feasible, and a `Mapping` can always check the value
  * of any of its subcomponents or columns in its assembly process.
  *
  * Second, as a component type can easily appear several times as a part
  * of a larger mapping (a business having several addresses, or a person having several phone numbers),
  * they naturally must use different column names. It is a good design principle to think ahead and
  * have reusable components accept at least a prefix/suffix to include in column names, but exceptions will always
  * exist, especially when dealing with preexisting schemas. A `Mapping` is therefore completely free to translate
  * a component it uses by modifying the column names as it sees fit. Additionally, any mapping can declare
  * [[net.noresttherein.oldsql.schema.Buff Buff]]s which can modify the handling of all of its subcomponents
  * (such as making them read-only). Their handling is the responsibility of the mapping class and not all components
  * support it. A logical component being part of a larger mapping can thus exist in several versions:
  * the original instance of the implementing class, the decorator serving as the operative version with modifications
  * applied by the entity mapping, as used by an SQL statement, and possibly any number in between. The operative,
  * 'public' version of each component is referred to as a ''export'' component, and only those public, export instances
  * must appear in the components (and columns) lists declared as part of this generic interface
  * (as opposed to the declarations of individual components). It is important to be furthermore aware that a component
  * instance may be public/'export' from a point of view of the enclosing mapping, but not the root mapping.
  * The library performs this translation automatically when passing on the `Pieces` with selected values and several
  * base classes exist fully handling this translation behind the scenes, but it is important to remember
  * when implementing custom `Mapping`s with new functionality.
  *
  * This is the root interface of the mapping class hierarchy, used almost exclusively throughout the library,
  * with subclasses - with the exception of adapters exposing the adapted mapping publicly - simply providing different
  * implementations or features directed 'inwards', towards the implementing classes, rather than providing an additional
  * public interface. Every concrete mapping however needs to define two types: the mapped scala type `Subject`
  * and a marker phantom type `Origin` which serves solely to introduce static distinction between several
  * instances of the same component type, but coming from different sources. In fact, many generic operations
  * are impossible to reliably implement without asserting that the handled `Mapping` actually defines those types
  * (that is, those types are equal for all instances of the mapping type). This is done through the type alias
  * defined here [[net.noresttherein.oldsql.schema.Mapping.Component Component]]`[T]`, and global, static refinements
  * from the companion object: [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingOf MappingOf]]. For uniformity and interoperability, these
  * are all defined as structural narrowing of the trait `Mapping` itself rather than separate classes. However, all
  * concrete implementations should not extend this trait directly, but rather one of the implementation traits:
  * [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] (the ''LUB'' type of all concrete mappings),
  * [[net.noresttherein.oldsql.schema.bases.LazyMapping LazyMapping]] (an optimized variant of the former),
  * [[net.noresttherein.oldsql.schema.bases.MappingFrame MappingFrame]] (a framework base trait providing methods
  * and types for declaring individual columns and components), or one of the other base types.
  *
  * There are several reasons for the separation of this trait: first, moving out the implementation of several methods
  * declared here allows them to be overriden by some 'interface' traits with abstract, narrowed definitions,
  * which would conflict with the default implementations returning more generic types. Second, several types
  * (in particular related to the SQL DSL) rely heavily on the infix notation of two-argument type constructors,
  * preventing them from accepting the type parameters of `BaseMapping` or `TypedMapping`. Abstract type parameters
  * of existential types such as `Component[_]` are not unified, leading to absurd situations
  * where 'obviously' equal types are not unified by the compiler, almost completely preventing their type safe use.
  * This is particularly visible with classes with a type parameter `C[M <: TypedMapping[_, _]]`
  * or `C[M <: BaseMapping[_, _]]`, defining their Origin and Subject as `M#Origin` and `M#Subject`. While from the
  * inside of the class `m.Origin =:= this.Origin` where `m :M`, when parameterized with a
  * `X <: TypedMapping[S, O]`, the classes `Subject` and `Origin` types are unrelated to `S, O`:
  * {{{
  *     trait C[M <: TypedMapping[_, _]] extends BaseMapping[M#Subject, M#Origin]
  *     def assemble[M <: TypedMapping[S, O], S, O](c :C[M]) :BaseMapping[S, O] = c //type clash
  * }}}
  * The third reason is the limitation of the type inferer which, when faced with
  * a method with a signature in the form of `[M <: TypedMapping[S, O], S, O](m :M)` will, when applied to
  * `m :BaseMapping[Int, O]` infer types `BaseMapping[Int, O], Nothing, Nothing` causing a compile error.
  * On the other hand, defining the type parameter as `[M <: TypedMapping[_, _]]` assigns new distinct types to the
  * missing type parameters, which are not unified even with `m.Subject`/`m.Origin` itself, leading to a lot of issues.
  * This can be circumvented with implicit parameters, but at the cost of additional complexity.
  * Finally, there is a bug in the compiler which prevents the use of a refining type constructor such as
  * `TypedMapping` as a type parameter in some scenarios, requiring a proper class type `BaseMapping`.
  *
  * Concrete implementing classes should accept a type parameter `O` defining their `Origin` type, so without much
  * loss of generality, a type constructor `M[O] <: MappingAt[O]` can be passed instead of the full mapping type.
  *
  * This trait is `Serializable` as it is referenced by [[net.noresttherein.oldsql.schema.SQLReadForm read]] and
  * [[net.noresttherein.oldsql.schema.SQLWriteForm write]] forms returned by its methods, which may require
  * serialization. In this use case, it poses no problems, as forms are fully independent. However, every `Mapping`
  * is required to correctly identify its components in order to provide their corresponding
  * [[net.noresttherein.oldsql.schema.MappingExtract extracts]], but all implementation use inherited reference identity
  * as `equality`. As the result, a serialized and deserialized component will no longer be correctly recognized by
  * the mapping for the table it came from, leading to `NoSuchElementException` being thrown by various methods.
  * For this reason, instances of this class should not be serialized in contexts where their reference would
  * be exposed after deserialization. An exception can be made for mappings of globally defined (i.e., reachable by
  * a path consisting of static identifiers) singleton objects extending
  * [[net.noresttherein.oldsql.schema.Table Table]] relation, as they are deserialized to the same instance.
  *
  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
  * @see [[net.noresttherein.oldsql.schema.bases.BaseMapping]]
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  */ //todo: consider serialization. The problem are all the functions, which are not serializable...
/*  Reasons why it's Mapping, not TypedMapping[_, _]:
 *    1. SchemaMapping.III is of unknown Origin
 *    1. TableMapping (single object Table with Mapping) doesn't have an Origin parameter
 *    1. Buggy inference of the Subject type in [O] TypedMapping[_, O]
 *  However, trait TypedMapping[_, _] has much better type inference for [S, O]TypedMapping[S, O]
 */
trait Mapping extends MappingTemplate[TypedMapping, TypedColumn] { self =>
	//Mapping instead of TypedMapping is useful in SchemaMapping.|-| which doesn't specify an Origin.
//consider: renaming to RowSchema? Less ambiguous, but how do you call a TableMapping then? Alternatives: Blueprint, Meta, Scheme (weak)
	/** Self type of this mapping, refining the `Mapping` interface with this mapping's
	  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]]
	  * and [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] types.
	  * This is just a convenience type alias allowing to refer to this object as an instance of the most commonly
	  * used mapping type [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]].
	  * @see [[net.noresttherein.oldsql.schema.Mapping.refine]]
	  */
	type Refine = TypedMapping[Subject, Origin]

	/** A type alias for a generic `Mapping` with the same `Subject` type as this mapping and the `Origin` provided as
	  * the type parameter. Used in particular in expressions like `MappingOf[S]#Projection` to obtain a type
	  * constructor for mappings with definitions of both the `Subject` and the `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  */ //todo: rename Transferred
	type Projection[O] = TypedMapping[Subject, O]

	/** A type alias for the [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]] with the provided
	  * `Origin` type and `Subject` type bound from above by the subject of this mapping. Used primarily
	  * in the expression `MappingOf[S]#BoundProjection` as a sort of a curried type constructor. This allows type
	  * unification of multiple mappings to a mapping for the LUB type of their subjects.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
	  */
	type BoundProjection[O] = MappingBound[Subject, O]

	/** A type alias for the [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] trait with the provided
	  * `Origin` type and the same `Subject` type as this mapping. Used primarily in the expression
	  * `MappingOf[S]#TypedProjection` as a sort of a curried type constructor for the `BaseMapping` trait.
	  * @see [[net.noresttherein.oldsql.schema.bases.BaseMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedComponent]]
	  */ //todo: get rid of it in Scala 3, when BaseMapping will not be arguments of any methods
	type TypedProjection[O] = BaseMapping[Subject, O]

	/** A type alias for the [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] trait with the provided
	  * `Origin` type and the same `Subject` type as this mapping. Used primarily in the expression
	  * `MappingOf[S]#ColumnProjection` as a sort of a curried type constructor for the `TypedColumn` trait.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
	  */
	type ColumnProjection[O] = TypedColumn[Subject, O]

	//todo: get rid of it in Scala 3, when BaseMapping will not be arguments of any methods
	type TypedColumnProjection[O] = BaseColumn[Subject, O]

	/** A container with values for components of this mapping required to assemble the subject.
	  * It is a [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] instance parameterized with
	  * the subject and origin of this mapping. It is used by methods
	  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] and
	  * [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] when assembling the subject of this mapping
	  * from individual column values returned by a `ResultSet`, or by explicitly preset values from another source.
	  * Each `Pieces` instance is, at least in theory, dedicated to a particular component instance (its class
	  * and position in the larger mapping structure). From the type safety point of view however it is sufficient that
	  * the subject and origin types of this mapping matches the type parameters of the `Pieces`'s.
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues]]
	  */
	type Pieces = ComponentValues[Subject, Origin]

	/** Any `Mapping` with the same `Origin` type as this mapping and an unspecified `Subject` type.
	  * Note that it is not the same as `Component[_]`, as the latter is a narrowing mandating that the mapping
	  * has the definition for the `Subject` type (which is of an unknown type). It is also not a direct
	  * analogue of `AnyColumn`, as the `TypedColumn` trait, extending `BaseMapping` defines both
	  * the `Origin` and the `Subject` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingAt]]
	  */
	type AnyComponent = MappingAt[Origin]

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin type as this mapping
	  * and unknown (but determined) `Subject` type.
	  * as this instance and thus a valid subcomponent type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Column]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  */
	type AnyColumn = ColumnMapping { type Origin = self.Origin }

	/** Any [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with the same `Origin` type as this
	  * mapping and the provided `Subject` type. It is typically used not in the context of components of this
	  * mapping, which is the domain of the more generic `Component` member type, but as part of a pseudo curried
	  * type constructor `MappingAt[O]#TypedComponent`, to simply denote any `BaseMapping` instance with
	  * the provided `Subject` and `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedProjection]]
	  */ //todo: remove it together with references to BaseMapping in the API
	type TypedComponent[T] = BaseMapping[T, Origin]

	/** Any [[net.noresttherein.oldsql.schema.bases.BaseColumn BaseColumn]] with the same `Origin` type as this
	  * mapping and the provided `Subject` type. It is typically used not in the context of components of this
	  * mapping, which is the domain of the more generic `Column` member type, but as part of a pseudo curried
	  * type constructor `MappingAt[O]#TypedColumnComponent`, to simply denote any `BaseColumn instance with
	  * the provided `Subject` and `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Column]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedColumnProjection]]
	  *///todo: remove it together with references to BaseColumn in the API
	type TypedColumnComponent[T] = BaseColumn[T, Origin]

	/** An [[net.noresttherein.oldsql.schema.Mapping.export export]]
	  * [[net.noresttherein.oldsql.schema.Mapping.Component component]] of this mapping.
	  * An export component of a mapping is the definitive version of the component in terms of buffs, column names
	  * and other properties which might change when a mapping is included as a component.
	  * This is a type refinement achieved through casting
	  * on the marker type [[net.noresttherein.oldsql.schema.Mapping.SuperMapping SuperMapping]]` = this.type`.
	  * Any component of this instance can be converted to its export version (including export components themselves)
	  * conforming to this type by calling `this.export(component)`. This level of type checking can be too cumbersome
	  * in general use, but it allows methods to formally declare that their arguments must be export components
	  * as a form of self documentation.
	  */ //todo: refactor export/apply to return this type.
	type ExportComponent[T] = TypedMapping[T, Origin] { type SuperMapping = Mapping.this.type }

	/** An [[net.noresttherein.oldsql.schema.Mapping.export export]]
	  * [[net.noresttherein.oldsql.schema.Mapping.Column column]] of this mapping.
	  * An export column of a mapping is the definitive version of the column in terms of its name and buffs,
	  * which might change when a column is included as a part of other component.
	  * This is a type refinement achieved through casting
	  * on the marker type [[net.noresttherein.oldsql.schema.Mapping.SuperMapping SuperMapping]]` = this.type`.
	  * Any column of this instance can be converted to its export version (including export columns themselves)
	  * conforming to this type by calling `this.export(column)`. This level of type checking can be too cumbersome
	  * in general use, but it allows methods to formally declare that their arguments must be export columns
	  * as a form of self documentation.
	  */
	type ExportColumn[T] = TypedColumn[T, Origin] { type SuperMapping = Mapping.this.type }

	/** An enclosing mapping of which this instance is guaranteed
	  * to be an [[net.noresttherein.oldsql.schema.Mapping.ExportComponent ExportComponent]]. This type shouldn't be,
	  * in general, defined by user code; components and columns from properties of this Mapping
	  * will have it set; similarly, [[net.noresttherein.oldsql.schema.Mapping.export export]]
	  * methods return components as `ExportComponent` instances.
	  */ //todo: actually use the ExportComponent
	type SuperMapping <: MappingAt[Origin]

	/** An extract for the value of some component of this mapping, with the subject type `T`, which carries
	  * additionally the ''export'' version of that component (from the point of view of this mapping).
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.export]]
	  */ //todo: include SuperMapping info to mark them as export components
	type Extract[T] = MappingExtract[Subject, T, Origin]

	/** An extract for the value of some column of this mapping, with the subject type `T`, which carries
	  * additionally the ''export'' version of that column (from the point of view of this mapping).
	  * @see [[net.noresttherein.oldsql.schema.ColumnExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.export]]
	  */ //todo: include SuperMapping info to mark as an export components
	type ColumnExtract[T] = ColumnMappingExtract[Subject, T, Origin]

	/** A type alias for a dictionary mapping all components (and subcomponents) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.extracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ColumnExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  */ //consider: 1) renaming to Extracts; 2) a dedicated collection. Could construct the map lazily and have defaults
	type ExtractMap = NaturalMap[Component, Extract]

	/** A type alias for a dictionary mapping all columns (including indirect) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.columnExtracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnMappingExtract]]
	  */
	type ColumnExtractMap = NaturalMap[Column, ColumnExtract]


	/** Is this mapping an instance of [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] ? */
	@inline final def isColumn :Boolean = this.isInstanceOf[ColumnMapping]


	/** Attempts to retrieve or assemble the value of `Subject` from the passed `ComponentValues` for this instance.
	  * Standard implementation will test several sources together with `pieces` before giving up:
	  * a ready value present for this mapping in the `pieces`, assembling the result from subcomponents and, finally,
	  * a default coming from an attached `OptionalSelect` (or related). By default it forwards to
	  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] and should stay consistent with it.
	  *
	  * This method should not, as a rule, be called directly by other mappings from their `assemble` method,
	  * as it can interfere with aliasing of the components to their ''export'' versions performed by `Pieces`.
	  * Instead, mappings should obtain the values of subcomponents through the overloaded
	  * [[net.noresttherein.oldsql.haul.ComponentValues.get get]] method of `ComponentValues`. While subclasses
	  * can override this method, especially in order to handle custom `buffs` or perform some other validation,
	  * in most cases required logic can and should be implemented in `assemble`, as in some cases - such as,
	  * for example, in altered mappings (which are adapters changing the effective buffs), this method (or
	  * `optionally`) might not be called at all. It is though safe to call by adapters from their `optionally` method.
	  *
	  * @throws NoSuchElementException if no value can be provided (`optionally` returns `None`).
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */
	def apply(pieces: Pieces): Subject

	/** Attempts to retrieve or assemble the value for the mapped `Subject` from the given
	  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]. This is the top-level method which can,
	  * together with passed `pieces`, produce the result in several ways. By default it forwards the call
	  * to the [[net.noresttherein.oldsql.haul.ComponentValues.assemble assemble]] method of `ComponentValues`,
	  * which, again by default, will first check if it has a predefined value stored for this mapping, and,
	  * only if not, dispatch back to this instance's [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  * method, which is responsible for the actual assembly of the subject from the values of the subcomponents,
	  * recursively obtained from `pieces`. If all of the above fails, this method will check for a predefined value
	  * stored in an attached [[net.noresttherein.oldsql.schema.Buff.OptionalSelect$ OptionalSelect]] (or related) buff
	  * if it exists. Additionally, any `AuditBuff`s present can modify the returned value and subclasses are free to
	  * handle other buffs or implement additional behaviour directly.
	  *
	  * Mapping implementations should generally prefer of overloaded
	  * [[net.noresttherein.oldsql.haul.ComponentValues.get get]] methods of `ComponentValues` over calling this
	  * method directly. This method should not, as a rule, be called directly by other mappings from their `assemble` method,
	  * as it can interfere with aliasing of the components to their ''export'' versions performed by `Pieces`.
	  * Instead, mappings should obtain the values of subcomponents through the overloaded
	  * [[net.noresttherein.oldsql.haul.ComponentValues.get get]] method of `ComponentValues`. While subclasses
	  * can override this method, especially in order to handle custom `buffs` or perform some other validation,
	  * in most cases required logic can and should be implemented in `assemble`, as in some cases - such as,
	  * for example, in altered mappings (which are adapters changing the effective buffs), this method might not be
	  * called at all. It is however safe to call by adapter mappings from their `optionally` method.
	  *
	  * Returning `Lack` signifies that neither the value of this mapping nor its required components were available
	  * in `pieces`, but, while there is an overlap, doesn't signify that the corresponding columns in the `ResultSet`
	  * were `null`. First, a `null` column generally means that the datatype (as implemented by
	  * [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]]) supports `null` values, be it directly as
	  * Scala `null` or by mapping to some special value: typically, such a column would be mapped to an `Option[_]`
	  * type, and `null` values to `None` (and this method would return `Got(None)`). Second, it is possible,
	  * although discouraged, for a composite mapping to return `null` despite some or all of its columns being
	  * not null, if validation fails or the result cannot be assembled for other reasons. The most common cases
	  * for returning `None` are:
	  *   - outer joins, where normally not-null columns hold null values, which are treated as 'unavailable
	  *     in this query', rather than 'unavailable in general',
	  *   - when mapping a table with a class hierarchy, for components of classes other than that for a particular row,
	  *   - sometimes, missing optional columns, for components which are not included in all queries.
	  * Ultimately, exact semantics will be defined by applications and custom mapping implementations.
	  * Bundled classes never throw an (accidental)  `NullPointerException` as a result of some mapping returning
	  * `Got(null)` here (or, equivalently, `null` from
	  * [[net.noresttherein.oldsql.schema.Mapping.apply(pieces:Pieces) apply]]`(pieces)`.)
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */ //consider: renaming to attempt/perhaps; potential clash with user columns? opt clashes with extension methods
	def optionally(pieces: Pieces): Opt[Subject]

	/** Attempts to assemble the value of this mapping from the values of subcomponents stored in the passed
	  * `ComponentValues`. This is the final dispatch target of other constructor methods declared here or
	  * in [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] and should not be called directly.
	  *
	  * The values of components should be collected using overloaded
	  * [[net.noresttherein.oldsql.haul.ComponentValues.get get]] method of `ComponentValues`, and never by
	  * directly calling `optionally`/`apply` on the components, as it can interfere with aliasing of components
	  * to their ''export'' versions and, in some cases, can even lead to infinite recursion.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply apply]]
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues.subject ComponentValues.subject]]
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues.optionally ComponentValues.optionally]]
	  */
	def assemble(pieces :Pieces) :Opt[Subject]


	/** Default value returned by the select forms of this mapping when the assembly process fails
	  * due to required columns being `null`. It is ''not'' used by the `optionally`/`apply` methods,
	  * as it can be in fact a [[net.noresttherein.oldsql.schema.SQLForm.NullValue.NotNull NotNull]] instance
	  * which will throw a [[net.noresttherein.oldsql.exceptions.NullValueException NullValueException]] instead.
	  */
	def nullValue :NullValue[Subject]


	/** Optional flags and annotations modifying the way this mapping is used. They are primarily used
	  * to include or exclude columns from a given type of SQL statements. For this reason they typically appear
	  * only on columns. If a larger component declares a buff, it is typically inherited by all its subcomponents
	  * (and, transitively, by all columns). This is not a strict requirement however, and custom mapping
	  * implementations can interpret attached buffs as pertaining only to the mapping itself, without affecting
	  * its subcomponents. As those subcomponents can in turn have their own buffs attached, care needs to be taken
	  * in order to avoid conflicts and undesired interactions.
	  */
	def buffs :Buffs[Subject]

	/** Verifies if this mapping has a [[net.noresttherein.oldsql.schema.Buff Buff]] of the given type
	  * or [[net.noresttherein.oldsql.schema.Buff.BuffType.implies implied]] by that type.
	  * @return `buff.active(this.buffs)`.
	  */ //not named active only to avoid conflicts with user columns of the same name
	def inEffect(buff :BuffType) :Boolean = buff.active(buffs)



	/** Creates a `ComponentValues` instance providing values for all columns to be used as parameters
	  * of a specific SQL statement type. Only the values for the bottom columns are required
	  * (in their export versions), but, rather than obtaining them directly from the given subject argument,
	  * every component on the inclusion path to the column should be given a chance to inspect the value
	  * (in particular with the purpose of applying buffs pertinent to the given operation). This is typically done
	  * by collecting the results of delegation to the overloaded methods of all direct components of this mapping.
	  * Simple `Mapping` implementations which are aware of all their subcomponents, can create the result directly,
	  * but in general it is the overloaded variant of this method accepting a `ComponentValuesBuilder`
	  * which should be overriden if a mapping wishes to perform some validation directly (rather than automatically
	  * through audit buffs), as it is the one being used by any mapping containing this mapping as a component.
	  * This method should always produce results consistent with those of the sibling methods specific
	  * to each operation type.
	  * @return a `ComponentValues` instance build by the builder passed to the overloaded sibling method.
	  */ //fixme: all writtenValues method should take Opt[Subject] to allow default values
	def writtenValues[T](op :WriteOperationView, subject :Subject) :ComponentValues[Subject, Origin] = {
		val res = ComponentValues(refine).newBuilder
		writtenValues(op, subject, res)
		res.result()
	}

	/** Derives from this mapping's subject the values for all columns from the given subject to be used as parameters
	  * of a particular SQL statement type. This is the right place to perform any validation
	  * should a subclass require to do so manually. The default behaviour is to apply all the appropriate audit buffs
	  * on this instance in order to the subject, and then invoke the same method on every ''direct'' component
	  * with the value extracted from `subject` by the `MappingExtract` for the given component.
	  * This method should stay consistent with the sibling methods specific to each operation type,
	  * but the direction of delegation between the two varies between subclasses.
	  * @return `writtenValues(UpdateView, subject, collector)` unless overriden.
	  */ //fixme: we are not aliasing the components to their export versions here! collector should alias components and control the recursion
	def writtenValues[T](op :WriteOperationView, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit

	/** Creates a `ComponentValues` instance providing values for all columns to be used as statement parameters
	  * of a ''WHERE'' clause of an SQL select filtering on this mapping's subject. Only the values
	  * for the bottom columns are required (in their export versions), but, rather than obtaining them directly
	  * from the given subject argument, every component on the inclusion path to the column should be given a chance
	  * to inspect the value (in particular with the purpose of applying buffs pertinent to the filter operation).
	  * This is typically done by collecting the results of delegation to the overloaded methods of all direct components
	  * of this mapping. Simple `Mapping` implementations which are aware of all their subcomponents, can create
	  * the result directly, but in general it is the overloaded variant of this method accepting
	  * a `ComponentValuesBuilder` which should be overriden if a mapping wishes to perform some validation directly
	  * (rather than automatically through `FilterAudit` buffs), as it is the one being used by any mapping
	  * containing this mapping as a component. This method should always produce results consistent with those
	  * of the more generic `writtenValues`, but the direction of delegation varies between implementations.
	  * @return a `ComponentValues` instance build by the builder passed to the overloaded sibling method.
	  */
	def filterValues(subject :Subject) :ComponentValues[Subject, Origin] = {
		val res = ComponentValues(refine).newBuilder
		filterValues(subject, res)
		res.result()
	}

	/** Derives the values for all columns from the given subject to be used as statement parameters of a ''WHERE''
	  * clause of an SQL select filtering on this mapping's subject. This is the right place to perform any validation
	  * should a subclass require to do so manually. The default behaviour is to apply all the `FilterAudit` buffs
	  * on this instance in order to the subject, and then invoke the same method on every ''direct'' component
	  * with the value extracted from `subject` by the `MappingExtract` for the given component.
	  * This method should stay consistent with [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]],
	  * but the direction of delegation between the two varies between subclasses.
	  * @return `writtenValues(FilterView, subject, collector)` unless overriden.
	  */
	def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(FilterView, subject, collector)

	/** Creates a `ComponentValues` instance providing values for all columns to be used as statement parameters
	  * of an SQL insert of this mapping's subject. Only the values for the bottom columns are required
	  * (in their export versions), but, rather than obtaining them directly from the given subject argument,
	  * every component on the inclusion path to the column should be given a chance to inspect the value
	  * (in particular with the purpose of applying buffs pertinent to the insert operation). This is typically done
	  * by collecting the results of delegation to the overloaded methods of all direct components of this mapping.
	  * Simple `Mapping` implementations which are aware of all their subcomponents, can create the result directly,
	  * but in general it is the overloaded variant of this method accepting a `ComponentValuesBuilder`
	  * which should be overriden if a mapping wishes to perform some validation directly (rather than automatically
	  * through `InsertAudit` buffs), as it is the one being used by any mapping containing this mapping as a component.
	  * This method should always produce results consistent with those of the more generic `writtenValues`,
	  * but the direction of delegation varies between implementations.
	  * @return a `ComponentValues` instance build by the builder passed to the overloaded sibling method.
	  */
	def insertValues(subject :Subject) :ComponentValues[Subject, Origin] = {
		val res = ComponentValues(refine).newBuilder
		insertValues(subject, res)
		res.result()
	}

	/** Derives the values for all columns from the given subject to be used as statement parameters
	  * of an SQL insert of this mapping's subject. This is the right place to perform any validation
	  * should a subclass require to do so manually. The default behaviour is to apply all the `InsertAudit` buffs
	  * on this instance in order to the subject, and then invoke the same method on every ''direct'' component
	  * with the value extracted from `subject` by the `MappingExtract` for the given component.
	  * This method should stay consistent with [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]],
	  * but the direction of delegation between the two varies between subclasses.
	  * @return `writtenValues(InsertView, subject, collector)` unless overriden.
	  */
	def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(InsertView, subject, collector)

	/** Creates a `ComponentValues` instance providing values for all columns to be used as statement parameters
	  * of an SQL update of this mapping's subject. Only the values for the bottom columns are required
	  * (in their export versions), but, rather than obtaining them directly from the given subject argument,
	  * every component on the inclusion path to the column should be given a chance to inspect the value
	  * (in particular with the purpose of applying buffs pertinent to the update operation). This is typically done
	  * by collecting the results of delegation to the overloaded methods of all direct components of this mapping.
	  * Simple `Mapping` implementations which are aware of all their subcomponents, can create the result directly,
	  * but in general it is the overloaded variant of this method accepting a `ComponentValuesBuilder`
	  * which should be overriden if a mapping wishes to perform some validation directly (rather than automatically
	  * through `UpdateAudit` buffs), as it is the one being used by any mapping containing this mapping as a component.
	  * This method should always produce results consistent with those of the more generic `writtenValues`,
	  * but the direction of delegation varies between implementations.
	  * @return a `ComponentValues` instance build by the builder passed to the overloaded sibling method.
	  */
	def updateValues(subject :Subject) :ComponentValues[Subject, Origin] = {
		val res = ComponentValues(refine).newBuilder
		updateValues(subject, res)
		res.result()
	}

	/** Derives the values for all columns from the given subject to be used as statement parameters
	  * of an SQL update of this mapping's subject. This is the right place to perform any validation
	  * should a subclass require to do so manually. The default behaviour is to apply all the `UpdateAudit` buffs
	  * on this instance in order to the subject, and then invoke the same method on every ''direct'' component
	  * with the value extracted from `subject` by the `MappingExtract` for the given component.
	  * This method should stay consistent with [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]],
	  * but the direction of delegation between the two varies between subclasses.
	  * @return `writtenValues(UpdateView, subject, collector)` unless overriden.
	  */
	def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(UpdateView, subject, collector)



	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * This is the same as [[net.noresttherein.oldsql.schema.Mapping.unary_+ +]]`this`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.-]]
	  */
	def + :ComponentSelection[Subject, Origin] = IncludedComponent(refine)

	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * This is the same as [[net.noresttherein.oldsql.schema.Mapping.unary_- -]]`this`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.+]]
	  */
	def - :ComponentSelection[Subject, Origin] = ExcludedComponent(refine)

	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.unary_-]]
	  */
	def unary_+ :ComponentSelection[Subject, Origin] = IncludedComponent(refine)

	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.unary_+]]
	  */
	def unary_- :ComponentSelection[Subject, Origin] = IncludedComponent(refine)


	/** A mapping like this instance but with [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] replaced
	  * with the given collection. The buffs cascade to the components of the new mapping: the ''export'' version
	  * of every component from this mapping has the new buffs as its suffix.
	  *///todo: rename to buffed; move down to other adapter methods?
	def withBuffs(buffs :Buffs[Subject]) :Component[Subject]

	/** A mapping like this instance but with [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] replaced
	  * with the given list. The buffs cascade to the components of the new mapping: the ''export'' version
	  * of every component from this mapping has the new buffs prepended to its list.
	  */
	def withBuffs(buffs :Seq[Buff[Subject]]) :Component[Subject]


	/** A modified version this mapping with the given components included/excluded ''by default''
	  * from all applicable database operations. The columns in the altered mapping will have the same order
	  * as in this instance, regardless of their presence and position in `first`/`rest`. This method is similar to
	  * [[net.noresttherein.oldsql.schema.Mapping.forSelect forSelect]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forFilter forFilter]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forUpdate forUpdate]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forInsert forInsert]] in that it creates a mapping
	  * with copies or adapters of these components having certain buffs added and removed. Where it differs
	  * is that, primarily, ''all'' operation types are affected and, secondarily, in case a component cannot
	  * be included/excluded for some - or even all - operations types, these changes will not affect said operations
	  * with regard to that component. This differs from individual methods in that they throw
	  * an `IllegalArgumentException` if the change cannot be made for some of the listed components.
	  * The easiest way to obtain arguments for this method is to call one of
	  * [[net.noresttherein.oldsql.schema.Mapping.+ +]] and [[net.noresttherein.oldsql.schema.Mapping.- -]] methods
	  * on a component of this mapping.
	  *
	  * Applications probably will find the analogical methods of
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation]] or
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter ComponentSQL]] more useful.
	  * @param first a component to include or exclude, wrapped in either
	  *              [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  *              if the component is to be included, or in
	  *              [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
	  *              if it should be excluded.
	  * @param rest  optional subsequent components to include/exclude.
	  *///first/rest split so that there is no conflict with apply() extension method returning the value of the component
	def apply(first :ComponentSelection[_, Origin], rest :ComponentSelection[_, Origin]*) :Component[Subject]

	/** A modified version this mapping with the given components included/excluded ''by default''
	  * from all applicable database operations. The columns in the altered mapping will have the same order
	  * as in this instance, regardless of their presence and position in `include`/`exclude`.This method is similar to
	  * [[net.noresttherein.oldsql.schema.Mapping.forSelect forSelect]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forFilter forFilter]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forUpdate forUpdate]],
	  * [[net.noresttherein.oldsql.schema.Mapping.forInsert forInsert]] in that it creates a mapping
	  * with copies or adapters of these components having certain buffs added and removed. Where it differs
	  * is that, primarily, ''all'' operation types are affected and, secondarily, in case a component cannot
	  * be included/excluded for some - or even all - operations types, these changes will not affect said operations
	  * with regard to that component. This differs from individual methods in that they throw
	  * an `IllegalArgumentException` if the change cannot be made for some of the listed components.
	  *
	  * Applications will probably find the analogical methods of
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation]] or
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter ComponentSQL]] more useful.
	  * @param include a collection of components of this mapping which should be included in every operation, if possible.
	  * @param exclude a collection of components of this mapping which should be excluded from every operation, if possible.
	  *///consider: maybe this should be named alterBuffs? We should do something to make sure this is not used instead of ComponentSQL.alter by mistake
	def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[Subject]

	/** Target method for `forSelect`, `forFilter`, `forUpdate` and `forInsert`. Responsible for creating an
	  * adapter (typically an [[net.noresttherein.oldsql.schema.support.PatchedMapping AlteredMapping]] subclass)
	  * with modified buffs on certain components so as to include or exclude them ''by default''.
	  * All components/columns which are not covered by either the `include` or the `exclude` list are left
	  * unmodified, meaning they will be included in the operation only if they would be included by this mapping
	  * by default.
	  * @param op      designator of the database operation type for which the mapping is being modified.
	  *                It is the source of buffs used in the process.
	  * @param include a list of (additional) components of this mapping to include in the operation.
	  *                Must not contain components having the `no` buff. All components on the list will
	  *                have their `Explicit` buff removed (if present) and, if the included component
	  *                is not a column, all its subcomponents with the `Explicit` buff will have that buff
	  *                removed.
	  * @param exclude a list of components which should be excluded from the operation. Must contain
	  *                components whose export versions have the `Optional` buff. All components on this list
	  *                will receive the `NonDefault` buff (if not already present) and so will
	  *                all their subcomponents with the `Optional` buff.
	  * @throws IllegalArgumentException if a component of `include` contains buff
	  *                                  `op.`[[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]],
	  *                                  or a component of `exclude` does not contain buff
	  *                                  `op.`[[net.noresttherein.oldsql.OperationView.Optional Optional]].
	  */
	protected def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Component[Subject]

	/** A modified version of this mapping with the given components included/excluded ''by default''
	  * from the SQL select operations.
	  * @param include components of this instance which should be included as part of the ''select'' clause.
	  *                Their ''export'' versions must not have the `NoSelect` buff. All members will
	  *                have their `ExplicitSelect` buff removed if present and so will their subcomponents.
	  * @param exclude components of this instance which should be excluded from the ''select'' clause.
	  *                All members must have the `OptionalSelect` buff present and will receive, together
	  *                with all their selectable subcomponents, the `NoSelectByDefault` buff.
	  * @return an adapter delegating to this mapping for assembly, but having some of its components recursively
	  *         replaced as per the rules for the `include` and `exclude` lists.
	  * @throws IllegalArgumentException if a component on the `include` list has the `NoSelect` buff or
	  *                                  a component on the `exclude` list doesn't have the `OptionalSelect` buff.
	  *///consider: 1) renaming to selectView; 2) removing these, it doesn't seem like they are being used at all
	def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[Subject]

	/** A modified version of this mapping with the given components included/excluded ''by default''
	  * from the column lists used as part of the ''where'' clause of an SQL select. This modifies the filter
	  * conditions generated from SQL DSL (and higher level expressions) comparing subjects of this mapping.
	  * @param include components of this instance which should be included as part of the ''where'' clause.
	  *                Their ''export'' versions must not have the `NoFilter` buff. All members will
	  *                have their `ExplicitFilter` buff removed if present and so will their subcomponents.
	  * @param exclude components of this instance which should be excluded from the ''where'' clause.
	  *                All members must have the `OptionalFilter` buff present and will receive, together
	  *                with all their selectable subcomponents, the `NoFilterByDefault` buff.
	  * @return an adapter delegating to this mapping for assembly, but having some of its components recursively
	  *         replaced as per the rules for the `include` and `exclude` lists.
	  * @throws IllegalArgumentException if a component on the `include` list has the `NoFilter` buff or
	  *                                  a component on the `exclude` list doesn't have the `OptionalFilter` buff.
	  */
	def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[Subject]

	/** A modified version of this mapping with the given components included/excluded ''by default''
	  * from the SQL insert operations.
	  * @param include components of this instance which should be included as part of the insert statement.
	  *                Their ''export'' versions must not have the `NoInsert` buff. All members will
	  *                have their `ExplicitInsert` buff removed if present and so will their subcomponents.
	  * @param exclude components of this instance which should be excluded from the select clause.
	  *                All members must have the `OptionalInsert` buff present and will receive, together
	  *                with all their selectable subcomponents, the `NoInsertByDefault` buff.
	  * @return an adapter delegating to this mapping for assembly, but having some of its components recursively
	  *         replaced as per the rules for the `include` and `exclude` lists.
	  * @throws IllegalArgumentException if a component on the `include` list has the `NoInsert` buff or
	  *                                  a component on the `exclude` list doesn't have the `OptionalInsert` buff.
	  */
	def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[Subject]

	/** A modified version of this mapping with the given components included/excluded ''by default''
	  * from the SQL update operations.
	  * @param include components of this instance which should be included in the updated columns list.
	  *                Their ''export'' versions must not have the `NoUpdate` buff. All members will
	  *                have their `ExplicitUpdate` buff removed if present and so will their subcomponents.
	  * @param exclude components of this instance which should be excluded from the updated columns list.
	  *                All members must have the `OptionalUpdate` buff present and will receive, together
	  *                with all their selectable subcomponents, the `NoUpdateByDefault` buff.
	  * @return an adapter delegating to this mapping for assembly, but having some of its components recursively
	  *         replaced as per the rules for the `include` and `exclude` lists.
	  * @throws IllegalArgumentException if a component on the `include` list has the `NoUpdate` buff or
	  *                                  a component on the `exclude` list doesn't have the `OptionalUpdate` buff.
	  */
	def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[Subject]

//todo: think of a better name for this method (customize clashes with the declaration in StaticMapping)
//	def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
//		op match {
//			case SelectView => forSelect(include, exclude)
//			case FilterView => forFilter(include, exclude)
//			case UpdateView => forUpdate(include, exclude)
//			case InsertView => forUpdate(include, exclude)
//		}


	//consider: all renaming/mapping methods to lose Origin in order to *not* be components of this mapping
	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string.
	  * This is equivalent to `prefixed(prefix + ".")` unless prefix is an empty string, in which case it is
	  * a no-op (or at least returns a mapping with exact same column name).
	  */
	def qualified(prefix :String) :Component[Subject]

	//todo: suffixed
	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string. */
	def prefixed(prefix :String)  :Component[Subject]

	/** An adapter of this mapping with the names of all its ''export'' columns replaced with the given function. */
	def renamed(naming :String => String) :Component[Subject]

	/** An adapter of this mapping with the columns reordered according to the given permutation.
	  * The columns on other lists,
	  * such as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectable selectable]]
	  * or [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault selectedByDefault]]
	  * are similarly reordered to stay consistent with
	  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]].
	  * Columns of individual components, including exported lists such as `(this.selectedByDefault(component))`
	  * are ''not'' reordered,
	  * and neither are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.components components]]
	  * or [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.subcomponents subcomponents]].
	  * @param permutation A sequence of length equal to `this.columns.size` containing every value
	  *                    from range `[0,columns.size)` exactly once. The `n`-th position defines the index
	  *                    of `this.columns(n)` on the returned mapping's `columns` list.
	  */
	def reorder(permutation :IndexedSeq[Int]) :Component[Subject]

	/** An adapter of this mapping with the columns reordered according to the given relation.
	  * If `precedes(column1, column2)`, then `column1` will come in
	  * [[net.noresttherein.oldsql.schema.Mapping.columns columns]] property and all other column lists before `column2`.
	  * The reverse is ''not'' true, i.e. return value of `false` is taken to mean 'no precedence relation
	  * between the arguments', rather than 'the second argument must come before the first argument'.
	  *
	  * Columns of individual components, including exported lists such as `(this.selectedByDefault(component))`
	  * are ''not'' reordered,
	  * and neither are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.components components]]
	  * or [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.subcomponents subcomponents]].
	  * @param precedes a function defined for ''export'' columns of this mapping, defining the order
	  *                 in which the argument columns must appear in the returned mapping adapter.
	  */
	@throws[IllegalArgumentException]("if there are two columns c1, c2 such that precedes(c1, c2) && precedes(c2, c1).")
	def reorder(precedes :(TypedColumn[_, Origin], TypedColumn[_, Origin]) => Boolean) :Component[Subject]


	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will be returned. If `back` extractor will return `None`, a `null` value will be written
	  * to the database.
	  *///consider: renaming to map if it won't interfere with type inference
	def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X]) :Component[X]

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will be returned.
	  */
	def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X]) :Component[X]

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If `there` or this mapping's `optionally` subject constructor returns `None`,
	  * implicitly provided here `NullValue.value` will be returned. If `back` returns `None`, `null` value will
	  * be written in the database.
	  */
	def optMap[X](there :Subject => Option[X], back :X => Option[Subject])(implicit nulls :NullValue[X]) :Component[X]


	/** Lifts this mapping by encapsulating the subject values in an `Option`. The created mapping will
	  * always return `Some(x)` from its `optionally` and `assemble` methods, where `x` is the value returned by
	  * the method of this instance. This means that `Some(None)` is returned when this component has no value
	  * associated in a given `Pieces` instance, and that `apply(Pieces)` method of the returned mapping will
	  * never fail. This instance is exposed as the public `get` field of the returned mapping, allowing direct
	  * access to any components definitions in this mapping.
	  */
	def inOption :Optional[this.type]



	/** Creates an SQL literal expression of the given value. The expression will use this mapping's column set
	  * default to the spelling scope/operation type of its usage.
	  */
	def apply(value :Subject) :SQLLiteral[Subject] = MappingLiteral(refine, value)

	/** Creates an SQL parameter expression of the given value. The expression will use this mapping's column set
	  * default to the spelling scope/operation type of its usage.
	  */
	def ?(value :Subject) :BoundParam[Subject] = BoundMappingParam(refine, value)


	/** A mapping intersects another mapping if their subjects share a supertype `S` such that
	  * for at least one [[net.noresttherein.oldsql.schema.Mapping.ColumnExtract ColumnExtract]] in the first mapping,
	  * there is an equivalent column extract in the second and both are defined for `S`.
	  * Equivalence is understood here in terms of extracts as functions, rather than columns themselves:
	  * two extracts are equivalent if they will both always return the same value (or no value) for the same argument.
	  * the names and buffs on their corresponding columns may differ.
	  *
	  * This method essentially answers if the two mappings are related, that is their subjects come from the same
	  * entity inheritance hierarchy, with equivalent mapping extracts for their least upper bound.
	  * If `this `[[net.noresttherein.oldsql.schema.Mapping.intersect intersect]]` that` is non empty, then this method
	  * will always return `true`. The implication in the other direction is not guaranteed, as mappings may recognise
	  * the relationship despite being unable to actually compare the extracts.
	  *
	  * The default implementation checks if the mappings
	  * are [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]] and, failing that,
	  * if `this `[[net.noresttherein.oldsql.schema.Mapping.intersect intersect]]` that` is non empty.
	  */ //todo: Scala 3 ...
	def intersects(that :Mapping) :Boolean = homomorphic(that) || intersect(that).nonEmpty

	/** An ''intersection'' of two mappings is the maximal collection of their column extract pairs such that:
	  *   1. Both extracts are defined for some supertype of `this.Subject` and `that.Subject`,
	  *      that is their widened argument type is the same;
	  *   1. Both extracts will always return the same value (or no value) when applied to the same argument.
	  *
	  * The column names, as well as buffs, may differ. As, in general, such a collection may not be computable
	  * for a pair of arbitrary mappings, implementations are always allowed to return an empty set,
	  * and true results can in practice be obtained only for related mapping types - either through wrapping,
	  * or sharing a supertype with a non empty column list. Note that the last condition can be checked only
	  * if the common upper bound is a concrete class, or if special provisions are in place.
	  *
	  * This method covers mappings for different types coming from the same inheritance tree.
	  * It will produce an empty collection for a mapping and its component, or for mappings o unrelated Scala types,
	  * even if their actual SQL form is the same (for example, `T` and `Option[T]`).
	  *
	  * The default implementation is an all-or-nothing affair, returning either the complete column sets
	  * if the mappings are [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]], or an empty collection
	  * otherwise.
	  */ //todo: deferred until macros in Scala 3
	def intersect(that :Mapping) :Iterable[(ColumnExtract[X], that.ColumnExtract[X]) forSome { type X }] =
		if ((this homomorphic that) && (columns homomorphic that.columns))
			columns.map { c =>
				(apply(c), that(that.counterpart(refine, c)))
					.asInstanceOf[(ColumnExtract[X], that.ColumnExtract[X]) forSome { type X }]
			}
		else
			Nil

	/* Todo: what we need:
	 *   1. identical    - 'deep equals', obvious, universally useful.
	 *   1. sameType     - (or same/compatible?) we can substitute one for another without causing any typing errors in the future:
	 *                     same class, type parameters, in particular Subject, same column set,
	 *                     but possibly different column names, buffs and whatever else. Same or different column order?
	 *                     This is meant to be a fast check (unlike homomorphic...)
	 *   1. equivalent   - (sameSQL?) same column sets, names, include/exclude buffs - same generated SQL.
	 *                     May be a proxy. Has limited use though, only things like export components in a MappingFrame?
	 *   1. isomorphic   - same column sets, same *effective* 'projecting' buffs, possibly different column names.
	 *                     May be a deep proxy. Means 'type compatible in SQL'.
	 *   1. homomorphic  - same column sets, possibly different buffs and column names, all export components in one
	 *                     have an export component in the other. Used by proxies. Different column order?
	 *   1  submetaOf    - (or extensionOf?) deep equals, but may have extra columns. Same order of shared columns,
	 *                     new columns at the end? Guaranteed subtyping of Subjects.
	 *   1. subtypeOf    - extending class, includes all the columns of the other, doesn't care about names, buffs -
	 *                     like typeEquals.
	 *   1. epimorphic   - homomorphic with extra columns.
	 *   1. related      - share non empty subset of columns, obviously different buffs, order, name, no subtyping (proxies).
	 *                     Basically just a way of declaratively informing that we can union select them.
	 * Would be still useful to have a way to compare just column sets, with different mapping Subjects.
	 * It is important to remember that buff equality is very tricky: some are simply incomparable (GeneratingBuff),
	 * Some may have irrelevant info (constraints)?
	 * And once we start adding programmatically 'include'/'exclude', there is no telling how they look.
	 * What we need is meaningful comparison of SQLExpression and Clause, and we should start with semantically
	 * defining those:
	 *    1. SQL.equals      - deep equals, but uses eq/equals for Mappings
	 *    1. SQL.equivalent  - deep equals, uses Mapping.equivalent. Same SQL a must.
	 *    1. SQL.compatible  - same signature, i.e. the column types in a ResultSet. Mappings isomorphic.
	 *    1. SQL.homomorphic - will equal after anchoring in the same clause (with all group by quirkiness).
	 * In Clause, we mainly need equality, type compatibility (same mapping types) and whatever needed for SQL:
	 *    1. Clause.equals      - Exact same relations (including alterations), SQL.equals for where/having/group by.
	 *    1. Clause.equivalent  - Same SQL, whatever it takes.
	 *    1. Clause.compatible  - nominal mappings of relations are sameType, isomorphic/homomorphic column sets
	 *
	 */


	/** If a mapping `m1` is a submapping of a mapping `m2`, then
	  *   1. `m1.Subject <:< m2.Subject`, and
	  *   1. for every column of `m2` there is a column in `m1` of the same type, such that their extractors
	  *      return the same value for any instance of `m1.Subject`, i.e. they represent the same part of
	  *      the mapped subjects.
	  *
	  * The column names and their buffs may differ. This relationship allows comparing
	  * of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] for the two mappings,
	  * as well as perform unions (and other set operations)
	  * on [[net.noresttherein.oldsql.schema.Relation Relation]] relations with these mappings.
	  *
	  * The default implementation first checks
	  * for a full [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphism]] between the two mappings,
	  * and then, if either is a derivative of its [[net.noresttherein.oldsql.schema.Mapping.original original]],
	  * checks if `original submappingOf that.original`.
	  */
	def submappingOf(that :Mapping) :Boolean = //consider: renaming to epimorphic
		homomorphic(that) || (this != original || that != that.original) && (original submappingOf that.original)
	//a good name is extensionOf, but we may want to reserve it for class hierarchy mappings


	/** If two mappings `m1` and `m2` are homomorphic then they share the same subject and there is a bijection
	  * between their [[net.noresttherein.oldsql.schema.Mapping.export export]] components such that:
	  *   1. paired components are homomorphic themselves,
	  *   1. paired components represent the same part of the subject -
	  *      their [[net.noresttherein.oldsql.schema.Mapping.Extract extract]] functions are referentially equivalent,
	  *      that is will always return the same value for the same argument (root mapping subject type).
	  *
	  * Column names and buffs of the components, non-export components, as well as any additional information may
	  * differ. In practical terms it means that for every component of one mapping,
	  * including non-export components and subcomponents, there is a component in the second mapping representing
	  * the same part of the subject entity. The order of components and columns in the collection properties
	  * of this mapping can likewise be permutations of each other.
	  *
	  * Homomorphism doesn't imply anything about the supertype of the two mappings other that they are both
	  * [[net.noresttherein.oldsql.schema.Mapping.MappingOf MappingOf]]`[S]`; in particular,
	  * a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]] is typically homomorphic with
	  * the adapted mapping and with other adapters of the same mapping.
	  *
	  * Homomorphic mappings can, in some situations, be interchangeable within SQL expressions:
	  * an [[net.noresttherein.oldsql.schema.Relation.export export]] mapping of a
	  * [[net.noresttherein.oldsql.schema.Relation Relation]] is in reality often homomorphic with its row mapping,
	  * although this method isn't required to recognise all such cases. Implementations can yield theoretical
	  * false negatives and be based on some specific factors leveraging the knowledge about mapping's implementation.
	  *
	  * Homomorphism is, by definition, a symmetrical relation. This check can yield false negatives
	  * and can often be determined only in one direction,
	  * as with a [[net.noresttherein.oldsql.schema.support.MappingDecorator MappingDecorator]], which knows
	  * it is homomorphic with its [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]],
	  * but determining it from the point of view of the backing mapping is much harder. In order to ensure symmetry
	  * and to avoid hard-coded type checks for the argument, the implementation is split into two checks
	  * of [[net.noresttherein.oldsql.schema.Mapping.uniHomomorphic uniHomomorphic]], one performed by `this` mapping,
	  * and another by `that`. It is the latter method which should, in general, be overriden by subclasses,
	  * rather than `homomorphic` itself. Similarly, in order to ensure the relation between homomorphism
	  * and isomorphism, the first check done by this method is comparing the two mappings
	  * with `this `[[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]]` that`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnCollectionExtension.homomorphic]]
	  */ //todo: even more wishful thinking...
	def homomorphic(that :Mapping) :Boolean =
//		(this isomorphic that) || (this != original || that != that.original) && (original homomorphic that.original)
		(this isomorphic that) || (this uniHomomorphic that) || (that uniHomomorphic this)

	/** Directly checks if this mapping recognizes the argument
	  * as [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]] with itself.
	  * This is one half of the implementation of method `homomorphic`, which invokes this method twice,
	  * with positions of `this` and `that` swapped.
	  *
	  * The default implementation checks for homomorphism of the respective
	  * [[net.noresttherein.oldsql.schema.Mapping.original original]] mappings, unless both `this` and `that` are
	  * already original, in which case it will return `false`.
	  * Note that this method will, in general, yield false negatives in some situations when `that uniHomomorphic this`
	  * is `true`, prompting the existence of the bidirectional check in `homomorphic`.
	  */
	def uniHomomorphic(that :Mapping) :Boolean =
		(this != original || that != that.original) && (original homomorphic that.original)

	/** If two mappings `m1` and `m2` are isomorphic then they share the same subject type and there is a bijection
	  * between their export component sets such that:
	  *   1. paired components are isomorphic themselves,
	  *   1. paired components represent the same part of the subject -
	  *      their [[net.noresttherein.oldsql.schema.Mapping.Extract extract]] functions are referentially equivalent,
	  *      that is they will always return the same value for the same argument (root mapping subject type),
	  *   1. the relation of being a subcomponent of another component is preserved,
	  *   1. paired components have the same views, i.e. lists
	  *      [[net.noresttherein.oldsql.schema.Mapping.selectedByDefault selectedByDefault]],
	  *      [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]],
	  *      [[net.noresttherein.oldsql.schema.Mapping.insertedByDefault insertedByDefault]],
	  *      [[net.noresttherein.oldsql.schema.Mapping.updatedByDefault updatedByDefault]] are isomorphic.
	  *
	  * Column names, as well as any additional information, may differ. The buffs, in so far as they do not affect
	  * column availability status for all database operations and the last point above is preserved, may also differ.
	  * The above conditions mean that isomorphic mappings have the same shape, i.e. their default read and write forms
	  * are referentially equivalent. Intuitively, being isomorphic implies that any SQL statements based
	  * on the two mappings will share the same expression tree shape. Also, being isomorphic implies, by definition,
	  * being homomorphic. While this relation doesn't explicitly require the types of isomorphic mappings to be equal,
	  * this condition is typically enforced independently by comparing only instances of some shared supertype.
	  *
	  * Isomorphic mappings are considered interchangeable within SQL expressions: an expression referring to a
	  * [[net.noresttherein.oldsql.schema.Relation Relation]]`[M]` (in particular, expressions for columns
	  * of the relation) can be evaluated for a different instance of `Relation[M]` - using a different instance of `M` -
	  * if their row mappings are isomorphic. In other words, while neither the mappings, nor the relations, nor
	  * the expressions are equal (one mapping will not recognize the components of another), the expressions
	  * can be substituted for one another when treated as a whole.
	  *
	  * This method may yield false negatives, but not false positives - it may not recognize general isomorphism
	  * and base the result on some additional shared property (in particular, the classes
	  * of the mappings).
	  *
	  * The default implementation first checks for [[net.noresttherein.oldsql.schema.Mapping.identical deep equality]]
	  * of the two mappings, and then performs two unidirectional checks
	  *  `(this `[[net.noresttherein.oldsql.schema.Mapping.uniIsomorphic uniIsomorphic]]` that) && that uniIsomorphic this`.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnCollectionExtension.isomorphic]]
	  */ //todo: for now this is wishful thinking, we need Extractor equality to benefit in any way from this fact
	def isomorphic(that :Mapping) :Boolean = //should it preserve the order of components/columns?
		(this identical that) || uniIsomorphic(that) || that.uniIsomorphic(this)

	/** Directly checks if this mapping recognizes the argument
	  * as [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with itself.
	  * This is one half of the implementation of method `isomorphic`, which invokes this method twice,
	  * with positions of `this` and `that` swapped.
	  *
	  * The default implementation checks for transitive isomorphism implied by isomorphisms of pairs
	  * `(this, `[[net.noresttherein.oldsql.schema.Mapping.original original]]`)`, `(original, that.original)` and
	  * (that, that.original), However, if both `this` and `that` are already original, then `false` will be returned
	  * in order to prevent infinite recursion.
	  * Note that this method will, in general, yield false negatives in some situations when `that uniIsomorphic this`
	  * is `true`, prompting the existence of the bidirectional check in `isomorphic`.
	  */
	def uniIsomorphic(that :Mapping) :Boolean =
		(this != original || that != that.original) &&
			(this isomorphic original) && (that isomorphic that.original) && (original isomorphic that.original)

	/** Two mappings are ''compatible'' ''iff'' they are isomorphic and of the same class.
	  * This check is used by SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]] and some other higher types
	  * parameterized with a mapping type to determine if their copy built around another mapping instance
	  * will be of the same type and will not cause a [[ClassCastException]] at any following point in time.
	  * Note that even [[net.noresttherein.oldsql.schema.Mapping.identical identical]] mappings are still
	  * not interchangeable as they will not recognize the components of each other - it may be necessary to
	  * replace any components of the old mapping in any collaborating objects
	  * with their [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.counterparts counterparts]]
	  * in the new mapping.
	  *
	  * This check is weaker than `this identical that` in two ways:
	  *   1. The column names, as well as buffs not affecting column set projections
	  *      for particular operations/syntactical scopes may differ;
	  *   1. The definitions of the components may differ in case of non table mappings: for example,
	  *      two instances of [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]`[_, S, _]` can be compatible
	  *      even if their backing expressions are completely different, as long as they are of the same type
	  *      and have the same [[net.noresttherein.oldsql.schema.SQLForm SQLForm]].
	  *
	  * The default implementation approximates the type check with class equality, which should be sufficient
	  * for concrete mappings of application domain model classes. Generic implementations for which it does not hold
	  * should override this method.
	  */
	def compatible(that :Mapping) :Boolean = getClass == that.getClass && isomorphic(that)

	/** Two mappings are equivalent ''iff'' their [[net.noresttherein.oldsql.schema.Mapping.columns columns]],
	  * [[net.noresttherein.oldsql.schema.Mapping.selectedByDefault selectedByDefault]],
	  * [[net.noresttherein.oldsql.schema.Mapping.filteredByDefault filteredByDefault]],
	  * [[net.noresttherein.oldsql.schema.Mapping.updatedByDefault updatedByDefault]],
	  * [[net.noresttherein.oldsql.schema.Mapping.insertedByDefault insertedByDefault]] properties are
	  *   1. [[net.noresttherein.oldsql.schema.ColumnMapping.ColumnCollectionExtension.equivalent equivalent]]
	  *      (the paired columns have the same name and form), and
	  *   1. the extracts of paired columns from all of the above collections are referentially equivalent,
	  *      that is they will always return the same value (or no value) for any argument.
	  *
	  * The above conditions guarantee that, unless the mapping has been altered or non-standard extensions are in play,
	  * they are translated to the same SQL.
	  *
	  * Due to technical limitations, this method may yield false negatives, and the default implementation
	  * requires the two mappings to be [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]],
	  * which imposes stronger restriction than strictly necessary.
	  */
	def equivalent(that :Mapping) :Boolean = //UnboundParam doesn't support columnNamed
		try {
			isomorphic(that) && columns.forall(c => c equivalent that.columnNamed(c.name))
		} catch { case _ :NoSuchComponentException => false }

	/** Checks for 'structural equality' or 'deep equality' between this mapping and the argument.
	  * Two mappings are identical if
	  *   1. they are of the same class or 'public class' - their most specific supertype exposed
	  *      to the rest of the application,
	  *   1. they are [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]],
	  *   1. the buffs of their corresponding components are equal,
	  *   1. the names of the corresponding columns are equal.
	  *
	  * Note that the requirement of buff equality will typically cause the check to fail for any mapping
	  * including a [[net.noresttherein.oldsql.schema.Buff.GeneratedBuff GeneratedBuff]], unless
	  * the mappings share the exact same instance.
	  * Being identical implies being [[net.noresttherein.oldsql.schema.Mapping.compatible compatible]].
	  *
	  * This method may yield false negatives, as implementations may restrict themselves to comparing only
	  * related mappings, for example instances of the same mapping class.
	  * A common case of identity between non referentially equal instances is a deserialized `Mapping`
	  * and the original instance.
	  *
	  * The default implementation simply checks for ''referential'' equality (`this eq that`).
	  */
	def identical(that :Mapping) :Boolean = this eq that

	/** Fixed to referential equality. In order to avoid unnecessary calls, many places check equality directly
	  * through `eq` instead of `==`.
	  */
	final override def equals(that :Any) :Boolean = this eq that.asInstanceOf[AnyRef]

	/** Unused by most `Mapping` classes as, by default, they do not override referential equality behaviour.
	  * Provided here for those few classes which do and to future proof us against overrides without
	  * the `override` keyword.
	  */
	def canEqual(that :Any) :Boolean = that.isInstanceOf[Mapping]

	/** Fixed to `System.identityHashCode(this)`. */
	final override def hashCode :Int = System.identityHashCode(this)


	/** A minimal string reference name of this mapping describing its 'type' (in natural sense).
	  * It is used in `toString` implementations of SQL [[net.noresttherein.oldsql.sql.SQLExpression expressions]]
	  * for the mapped type and thus should be kept as short as possible so that they remain usable.
	  * It serves as the root of other formatting log strings implementations (as well as the standard `toString`),
	  * especially those of adapter classes, so should not include information about the mapped type, contents
	  * or anything that might be duplicated when formatting a debug string of an adapter class for this mapping
	  * (such as a [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping foreign key]] mapping).
	  * Defaults to the local class name of this instance or the column name in case of columns, and should not deviate
	  * significantly from this spirit, with only generic adapter classes being required
	  * to introduce additional distinction.
	  */
	def mappingName :String  = this.localClassName

	/** Appends the list of [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] to
	  * [[net.noresttherein.oldsql.schema.Mapping.toString toString]] and shouldn't deviate from this information
	  * unless necessary, with only adapter classes having a legitimate reason for doing so if it would result
	  * in duplication of information.
	  */
	def buffString :String =
		if (buffs.nonEmpty) buffs.mkString(toString + "(", ", ", ")") else toString

	/** Lists this mapping's [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] and
	  * [[net.noresttherein.oldsql.schema.Mapping.columns columns]] together with the basic information
	  * about this mapping.
	  */
	def columnString :String = columns.view.map(_.buffString).mkString(buffString + "{", ", ", "}")

	def debugString :String = {
		/** Use recursion to print the ''export'' (by this mapping) version of every subcomponent.
		  * @param ident whitespace prefix to start every new line with
		  * @param wasColumn was the last printed component a sibling column, meaning we should print this column inline
		  * @return `true` if `mapping` is a column.
		  */
		def rec[T](mapping :Component[T], res :StringBuilder, ident :String = "", wasColumn :Boolean = false) :Boolean =
			mapping match {
				case column :ColumnMapping =>
					if (wasColumn) //print all sibling columns in a single line
						res ++= "; "
					else //last component was not a column or mapping is the first component - we are at the start of a new line
						res ++= ident ++= " "
					res ++= column.debugString
					true
				case _ =>
					res ++= "\n" ++= ident ++= mapping.mappingName //print basic mapping info
					if (mapping.buffs.nonEmpty) { //print (buffs)
						res ++= "("
						mapping.buffs.foreach(res ++= _.toString ++= ", ")
						res.delete(res.length - 2, res.length)
						res ++= ")"
					}
					res ++= "{" //export and print {components}
					if (mapping.components.isEmpty)
						res ++= "}"
					else {
						res ++= "\n"
						(false /: mapping.components.map(export(_)))(
							(wasColumn, comp) => rec(comp, res, ident + "  ", wasColumn)
						)
						res ++= "\n"  ++= ident ++= "}"
					}
					false
			}
		val res = new StringBuilder
		rec(this, res)
		res.toString
	}

	/** Brief, most important information about this mapping such as the name of the mapped type or its some other
	  * indicator, where available, but not list the full column list. Defaults to
	  * [[net.noresttherein.oldsql.schema.Mapping.mappingName mappingName]] (local class name) or column name
	  * and [[net.noresttherein.oldsql.schema.SQLForm form]] in case of columns. As a rule of the thumb,
	  * it should include enough information to identify it and its value type (if reasonable and possible)
	  * within the enclosing scope, without any debugging details about its internals such as its buffs.
	  */ //todo: the whole toString collection requires serious rethinking
	override def toString :String = mappingName


	/** A seal method implemented only by [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] to enforce
	  * that every concrete implementation extends `BaseMapping`, required by the `sql` package.
	  */
	private[schema] def every_concrete_Mapping_must_extend_BaseMapping(seal :Seal) :Unit

	@transient private val notSerialized = true //after deserialization becomes false

	/** Checks if this mapping has been serialized and deserialized to a new object. A deserialized mapping is
	  * a deep copy of the original mapping, incompatible with the original. All `Mapping` methods accepting components
	  * will throw a `NoSuchElementException` if given a deserialized component as the argument (and vice versa).
	  * Similarly, any mixed use of these two mappings will most likely result in an exception being thrown,
	  * possibly deferred in time. `Mapping` trait is serialized as it a part of the object graph of (serializable)
	  * [[net.noresttherein.oldsql.sql.ast.SelectSQL SelectSQL]] and because it may be useful to contain references to
	  * tables (and thus mappings) inside [[net.noresttherein.oldsql.model.Kin Kin]] objects and similar.
	  * In general, it is best to avoid serialization of this trait unless it is part of another object and not
	  * exposed anymore to the outside, becoming an implementation detail.
	  *
	  * This doesn't include those `Mapping` instances which are serialized using a proxy (via a `writeReplace` method).
	  * This is the case with all singleton objects or other classes deserialized with `readResolve`
	  * to the same instance. If custom serialization is required, this method would be best overriden for consistency,
	  * so that it returns `true` ''iff'' the component set of this mapping is a deep copy of another mapping.
	  * This information is provided solely as a debugging help,
	  */
	def isDeserialized :Boolean = !notSerialized
}






private[schema] sealed abstract class Rank1MappingImplicits {
	//exists for use as the right side of SQLExpression.=== and similar, which will instantiate type F before applying conversion
	implicit def mappingSQL[F <: RowProduct, C <: Mapping, S, O <: RowProduct] //can't use TableOffset as we might be converting a component of a table
                           (mapping :C)(implicit origin :C <:< MappingAt[O], base :F <:< O,
                                        offset :RelationCount.In[O], projection :OriginProjection[C, S])
			:SQLExpression[F, Single, S] =
		LooseComponent[F, projection.WithOrigin[F], S](projection[F](mapping), offset.offset)
}



object Mapping extends Rank1MappingImplicits {

	implicit def componentSQL[F <: RowProduct, C <: Mapping, S]//can't use TableOffset as we might be converting a component of a table
                             (mapping :C)(implicit origin :C <:< MappingAt[F], offset :RelationCount.In[F],
                                          projection :OriginProjection[C, S])
			:LooseComponent[F, projection.WithOrigin, S] =
		LooseComponent(mapping)


	/** Narrowing of the `Mapping` trait to subtypes which define
	  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type as `S`.
	  */
	type MappingOf[S] = Mapping { type Subject = S } //consider: rename to MappingFor

	/** Narrowing of the `Mapping` trait to subtypes which define
	  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type as `O`.
	  * While mapping types are expected to have the `Origin` as a free type parameter, conversion from one
	  * origin type to another may require more than casting if the `Origin` type occurs in its type signature
	  * several times.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
	  */ //consider: at is not the proper preposition associated with 'Origin', but 'of' would be misleading. Rename Origin to ...? Lineage, sphere, family, line
	type MappingAt[O] = Mapping { type Origin = O } //consider: MappingIn //MappingFrom/ComponentOf //

	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S` and `Origin` as `O`. */
	type TypedMapping[S, O] = Mapping { type Subject = S; type Origin = O }

	/** Narrowing of the `Mapping` trait to subtypes of the given `Origin` type `O` and where the `Subject` type
	  * is bound from above by `S`.
	  */
	type MappingBound[S, O] = Mapping { type Subject <: S; type Origin = O }

	/** Refines the mapping given as its first/left argument by narrowing it down to instances with
	  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type equal to the second type argument.
	  * While accepting an `Origin` type as a type argument is recommended
	  * for all [[net.noresttherein.oldsql.schema.Mapping Mapping]] implementations as it makes SQL using them
	  * much more readably, an alternative exists in not having any declarations for `Origin` at all,
	  * and instead using this refinement. In that case
	  * `([O] :=> M At O) <:< `[[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]]`[O]`.
	  * In this case, the mapping type cannot define its `Origin` type or put any bounds on it.
	  * This comes useful with implementations as singleton objects, as it allows the object
	  * to be a [[net.noresttherein.oldsql.schema.Table Table]] at the same time.
	  */ //todo: rename to asIn/asFrom or from/first/firstIn
	type AsFrom[M <: Mapping, O] = M { type Origin = O } //consider: renaming to OfOrigin

	/** Narrowing of the `Mapping` trait to subtypes which define
	  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type as `S`.
	  */
	type of[S] = Mapping { type Subject = S }

	/** Narrowing of the `Mapping` trait to subtypes which define
	  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type as `O`.
	  * While mapping types are expected to have the `Origin` as a free type parameter, conversion from one
	  * origin type to another may require more than casting if the `Origin` type occurs in its type signature
	  * several times.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
	  */
	type at[O] = Mapping { type Origin = O }

	/** A curried type constructor refining any `Mapping` subtype with a definition of `Origin` type accepted
	  * as the second argument (to member type `at`).
	  * @see [[net.noresttherein.oldsql.schema.Mapping.AsFrom]]
	  */
	type project[M <: Mapping] = { type to[O] = M AsFrom O }

//	/** A curried definition of [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[S, O]`,
//	  * containing a single type constructor `P[O] = TypedMapping[S, O]`. Similar types are defined
//	  * in companion objects of various `Mapping` subtypes, allowing their use as type parameters to classes/methods
//	  * which require the definition of a mapping accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
//	  * type.
//	  */

	/** A fragment of [[net.noresttherein.oldsql.schema.Mapping Mapping]] interface declaring properties and methods
	  * providing access to its components and columns. Apart from `Mapping` itself, it is extended by
	  * other templates for various mixins and specialist mapping implementations of kind `Comp`
	  * shared with all their [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] components,
	  * Note that the mapping can still have non-export components/columns of generic `Mapping`/`TypedColumn` types.
	  * @tparam Comp $ComponentParamInfo
	  * @tparam Col  $ColumnParamInfo
	  * @define ComponentParamInfo A type constructor for a `Mapping` subtype being a supertype of this mapping
	  *                            and all its ''export'' components. Its type parameters define
	  *                            the component's [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]]
	  *                            and [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] types, respectively.
	  *                            Due to technical difficulties, it is not used as a self type for this trait,
	  *                            but any concrete subclass must be also a subtype of `Comp[this.Subject, this.Origin]`.
	  * @define ColumnParamInfo A type constructor for a `TypedColumn` subtype being a supertype
	  *                         of all ''export'' columns.
	  * @define mapping `Mapping`
	  * @define column `TypedColumn`
	  * @define ExportSubcomponentsInfo [[net.noresttherein.oldsql.schema.Mapping.export Export]] columns
	  *                                 and subcomponents of a mapping's component are ''not'' necessarily
	  *                                 ''export'' columns and components of the parent mapping. Hence, calling
	  *                                 [[net.noresttherein.oldsql.schema.Mapping.columns component.columns]]
	  *                                 may return columns with invalid - from the point of view of this mapping -
	  *                                 [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] or even names.
	  */ //TODO: BaseMappingTemplate[Comp[v] <: TypedMapping[v, O], Col[v] <: TypedColumn[v, O] with Comp[v, O], S, O]
	trait MappingTemplate[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]] {
		//consider: we could try to make type parameters member types instead, moving back everything to mapping
		//  this would mean that MappingFrame etc. could extend this trait.
		/** The mapped entity type. */
		type Subject

		/** A phantom marker type identifying the origin of this mapping. It is used to statically distinguish between
		  * different instances of the same mapping class, but mapping different portions of the result set -
		  * in particular, they work like aliases for repeated occurrences of a table and its components
		  * in a joined query. In addition, it is used by the SQL DSL to ensure that an SQL expression refers only
		  * to components coming from the one particular query, preventing accidental use of other, non-joined mappings.
		  * This type should ''not'' be used for other purposes, to keep values or be interpreted in any way,
		  * such as actual alias names for joined tables. All concrete `Mapping` implementations are expected
		  * to take `Origin` as a type parameter (by convention, and to leverage Scala's partial kind unification,
		  * the last one) for seamless use in SQL expressions. All components of a mapping must have the same
		  * `Origin` type as their enclosing mapping. By induction, this means that for any 'root' mapping (normally,
		  * a mapping for a database table), the whole subcomponent tree shares the same origin, which provides
		  * type safety by disallowing the use of mappings with other `Origin` types in methods of this trait.
		  *
		  * Casting a `Mapping` to a different `Origin` should be safe. In order to abbreviate the code and provide
		  * better type safety, direct casting to that effect should be avoided, and instead
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]] extension method
		  * should be used. It relies on the existence of an implicit
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] which defines the result type
		  * of such a cast. If the type inferer can unify a mapping type `X` with some
		  * `M[O] <: Mapping { type Subject = S; type Origin = O }`, which will happen automatically
		  * for any mapping class which accepts its origin type as the last type parameter, an implicit
		  * `OriginProjection[M, S]` will exist, and `m.withOrigin[O]` will be an instance of `M[O]` for any `m :M[_]`.
		  * If such a conversion cannot be performed or is unsuitable, the mapping class should declare its own implicit
		  * `OriginProjection` within its companion object. This makes it possible to define the result type of the cast
		  * in case a mapping class doesn't accept the `Origin` type as its last type parameter, or it can appear
		  * in more than one place in the type signature.
		  *
		  * The definitions of this type vary between use cases, but, most notably, SQL DSL specifies the origin
		  * of any component of a table `T[O]` from a ''from'' clause
		  * `F <: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as a type `F <: O <: RowProduct` which starts
		  * with a wildcard `RowProduct` type (either `RowProduct` itself or one of its subtypes, used
		  * as broad classification of their derived classes), 'joined' with table `T`, for example
		  * `RowProduct AndFrom T Join X`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingAt]]
		  */ //mention the alternative of refining this.type instead of accepting it as a parameter
		type Origin

        /** A refinement of this object's singleton type defining its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
          * It is particularly useful for mapping components defined as member objects (and inheriting `Origin` from the enclosing mapping
          * and [[net.noresttherein.oldsql.schema.TableMapping TableMapping]] singleton objects, which do not define `Origin` in any way.
          * It allows the use of a mapping object as a type parameter to [[net.noresttherein.oldsql.sql.From From]],
          * [[net.noresttherein.oldsql.sql.Join Join]] and related classes: `From[Mages.withOrigin]`.
          */ //consider: renaming to Of/of, from
        type withOrigin[O] = this.type { type Origin = O }

		/** A refinement of type `Mapping` defining its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
		  * It is an abstract type bound: concrete implementations for classes from application's domain model
		  * are welcome to override it with a definition narrowing their own type, as an alternative to
		  * accepting a type argument for the value of `Origin` type. For example,
		  * defining `Hamsters#AsFrom[O] = Hamsters { type Origin = O }` allows the use of `Hamsters#AsFrom`
		  * where a type constructor conforming to [[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]]
		  * is needed. This is used in particular by implementations as singleton objects, which refine `this.type`.
		  *
		  * A second use case for this type is as a type parameter for invariant higher types, denoting an unspecified
		  * mapping type of the same `Subject` as this instance and the given `Origin`.
		  */
		type AsFrom[O] <: Comp[Subject, O]

		type TypedAsFrom[O] <: BaseMapping[Subject, O]

		/** Any mapping with the same origin marker type, making it a supertype of all valid component types
		  * of this mapping. It is also occasionally used as a part of the expression `MappingOf[S]#Component`
		  * as a sort of a curried type constructor for the narrowed down `TypedMapping`, not necessarily
		  * in the context of components of any particular mapping instance. The relation of being a component
		  * is transitive: all components of components of a mapping are considered components of that mapping,
		  * and must be correctly handled when passed as an argument to any of its methods. A single 'logical' component
		  * - a mapping for a particular column subset of a table - can exist in multiple copies, as separate components
		  * of a mapping, differing in the declared column names or buffs. This is because a mapping is allowed to adapt
		  * any component class by changing these properties. One of these components is however considered
		  * the ''export'', operative version and is used for assembly. This aliasing of multiple versions is handled
		  * by [[net.noresttherein.oldsql.schema.Mapping.export export]] method and
		  * [[net.noresttherein.oldsql.schema.Mapping.Extract extracts]] for these components. The relation of being
		  * an export component of a mapping is not transitive: an export component of an export component of a mapping
		  * is not necessarily an export component of that mapping.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedMapping]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.Column]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
		  */ //consider: renaming it to Brick or Part
		type Component[T] = TypedMapping[T, Origin]
		//todo: SpecificComponent/SpecificColumn subtypes returned by SQLMapping and similar, so that we don't need all the overrides

		/** Any [[net.noresttherein.oldsql.schema.ColumnMapping TypedColumn]] with the same origin marker type
		  * as this instance and thus a valid subcomponent type of this mapping. It is also occasionally used as a part
		  * of the expression `MappingOf[S]#Column` as a sort of a curried type constructor for the `TypedColumn` trait,
		  * which doesn't necessarily describe columns of any particular mapping instance. All comments regarding
		  * different versions of the same logical [[net.noresttherein.oldsql.schema.Mapping.Component component]]
		  * and their ''export'' version apply also to columns.
		  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.ColumnProjection]]
		  */
		type Column[T] = TypedColumn[T, Origin]

		/** A type container redeclaring several member types of `Mapping`, such as
		  * [[net.noresttherein.oldsql.schema.Mapping.Component Component]]
		  * and [[net.noresttherein.oldsql.schema.Mapping.Extract Extract]],
		  * by replacing the most generic [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]
		  * in their definition with its subtype `M[S, O]`. It is used in a way similar to
		  * [[net.noresttherein.oldsql.schema.Mapping.Projection Projection]], by a type projection
		  * from some refinement of `Mapping`, for example type `TypedMapping[S, O]#like[TypedColumn]#Extract[Hamster]`
		  * equals [[net.noresttherein.oldsql.schema.ColumnMappingExtract ColumnMappingExtract]]`[S, Hamster, O]`.
		  * It is used by specific subtypes of `Mapping` whose components are all of type `M[T, Origin]`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.specific]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate]]
		  */ //todo: rename to specific
		type like[M[T, O] <: TypedMapping[T, O]] = {
			/** A mapping `M` with [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type `T`
			  * and the same [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type as this mapping.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
			  */
			type Component[T]     = M[T, Origin]

			/** An ''export'' component `M` of this mapping
			  * (sharing with it [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type)
			  * of[[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type `T`.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.ExportComponent]]
			  */
			type Export[T]        = M[T, Origin] { type SuperMapping = MappingTemplate.this.type }

		 	/** A type constructor for mapping of kind `M`
			  * sharing [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type with this mapping
			  * and using the supplied [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
			  */ //todo: rename to Transferred
			type Projection[O]    = M[Subject, O]

			/** An extract for component `M` of this mapping
			  * with [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type `T`.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.Extract]]
			  */
			type Extract[T]       = SpecificExtract[M[T, Origin], Subject, T, Origin]

			/** A `NaturalMap` associating with components of this mapping
			  * [[net.noresttherein.oldsql.schema.Mapping.as.Extract extracts]] for their ''export'' versions
			  * of kind `M`, preserving their [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
			  * It is the type of property [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] of mappings
			  * extending [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate MappingTemplate]]`[M, _[_]]`.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.ExtractMap]]
			  */
			type ExtractMap       = NaturalMap[MappingTemplate.this.Component, Extract]

			/** A `NaturalMap` associating with columns of this mapping
			  * [[net.noresttherein.oldsql.schema.Mapping.as.Extract extracts]] for their ''export'' versions
			  * of kind `M`, preserving their [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
			  * It is the type of property [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] of mappings
			  * extending [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate MappingTemplate]]`[_[_], M]]`.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.ColumnExtractMap]]
			  */
			type ColumnExtractMap = NaturalMap[MappingTemplate.this.Column, Extract]

			/** A mapping adapter, of an arbitrary [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type `T`,
			  *  for this mapping cast down to kind `M`.
			  */
			type Adapter[T]       = MappingAdapter[M[Subject, Origin], T, Origin]

			/** A mapping adapter for this mapping cast down to kind `M`,
			  * sharing its [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
			  */
			type Decorator        = MappingAdapter[M[Subject, Origin], Subject, Origin]
		}

		/** A type container with definitions of analogues to some member types of `Mapping`,
		  * such as [[net.noresttherein.oldsql.schema.Mapping.Component Component]]
		  * and [[net.noresttherein.oldsql.schema.Mapping.Extract Extract]],
		  * by replacing the most generic [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[M[_]#Subject, O]`
		  * in their definition with its subtype `M[`[[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]`]`.
		  * It is used in a way similar to [[net.noresttherein.oldsql.schema.Mapping.Projection Projection]],
		  * by a type projection from some refinement of `Mapping` representing an owning component,
		  * for example type `TypedMapping[Cage, O]#like[Squirrels]#Extract`
		  * is equal to `SpecificExtract[Squirrels[O], Cage, Squirrel, O]`, accepting a component subject type.
		  * It is used to describe a specific component of `Mapping`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.like]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate]]
		  */ //todo: rename to project
		type specific[M[O] <: TypedMapping[_, O]] = {
			/** A mapping instance of the specified type ''used solely as a path element'' to access member
			  * types of `M[Origin]`. As no instances of the type container `specific` exists, neither `it`
			  * has any actual value.
			  */
			val it :M[Origin]

			/** An ''export'' component `M` of this mapping
			  * (sharing with it [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type)
			  * of[[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type `T`.
			  * @see [[net.noresttherein.oldsql.schema.Mapping.ExportComponent]]
			  */
			type Export    = M[Origin] { type SuperMapping = MappingTemplate.this.type }

			/** An extract for a specific component of this mapping `M[Origin]`, with a fixed
			  * with [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]].
			  * @see [[net.noresttherein.oldsql.schema.Mapping.Extract]]
			  */
			type Extract   = SpecificExtract[M[Origin], Subject, M[Origin]#Subject, Origin] //replace with it.Subject

			/** A mapping adapter to a specific component `M[Origin]` with a fixed
			  *  [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
			  */
			type Adapter   = MappingAdapter[M[Origin], M[Origin]#Subject, Origin]

			/** A mapping adapter for this mapping cast down to kind `M[Origin]` of a fixed
			  * [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
			  */
			type Decorator = MappingAdapter[M[Origin], M[Origin]#Subject, Origin]
		}

		//forms are declared & defined here rather than in Mapping because specific templates like StableMappingTemplate
		// declare their overrides, and it is more convenient for them not to extend Mapping

		/** Default read form (included columns) of a select statement for this mapping.
		  * This form can span multiple columns, listed by
		  * the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault selectedByDefault]] property.
		  * As the above list ignores any [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]
		  * buffs on this instance, this form will always be non-empty (read at least one column from the `ResultSet`),
		  * even for components which should never be selected (for example, because they do not map to database columns
		  * at all). It is the responsibility of the caller to control if this component should be included
		  * in the result set or not.
		  *
		  * Implementations should not eagerly reference the columns of this instance (i.e., call
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatable updatable]] or any of its sibling
		  * methods before the constructor of this instance completes). Doing so may lead to initialization errors,
		  * such as access of an uninitialized field, due to reference cycles introduced by foreign key components.
		  */
		def selectForm :SQLReadForm[Subject] = MappingReadForm.select(refine)

		/** Read form for a select statement for this mapping including the given components of this mapping.
		  * It is a way to modify the default list of used columns by including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] buff).
		  * All components on the list (and their columns) are first aliased to their operative versions
		  * by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
		  * Note that `ExplicitSelect` buffs are ignored only if declared on the export version of a listed component
		  * and not any of their subcomponents: if both one of the components and its column declare
		  * an `ExplicitSelect` buff, the column will not be included, unless it is also (directly) present in
		  * `components` argument. On the other hand, omitting a component with an `OptionalSelect` buff will omit
		  * all its columns, including those with the same buff (just as all its mandatory columns).
		  * @param components a list of components which should be included in the form. It can include both direct
		  *                   components and subcomponents, not only columns. The `ExplicitSelect` buff is ignored,
		  *                   if present, but including a component with `NoSelect` buff (or one implying it)
		  *                   will result in an exception; any subcomponents with said buff of any of these components
		  *                   are however silently ignored as by default. The list must cover, directly or indirectly,
		  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
		  *                   ''do not'' include the buff `NoSelectByDefault` or any that imply it).
		  * @return a read form including the exact set of columns covered by the specified components.
		  * @throws IllegalArgumentException if the column set covered by the given components (in their export versions)
		  *         includes a column with the `NoSelect` buff (or one which implies it), or if a column of this mapping
		  *         exists which does not belong to this set and does not have the `NoSelectByDefault` buff.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping.
		  */
		def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject] =
			MappingReadForm.select(refine, components)

		/** Default read form (included columns) for a component of this mapping used inside a ''select'' clause.
		  * If the component is optional, it becomes automatically included - this method will not return an empty form.
		  * This form can span multiple columns, listed by
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault selectedByDefault]]`(component)`.
		  * @param component any component of this mapping, export or not.
		  * @return a read form covering the default, from the point of view of this mapping, column set of `component`,
		  *         once any [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buffs declared
		  *         directly by it are suppressed.
		  * @throws IllegalArgumentException if the component contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] buff,
		  *                                  or cannot be used in a ''select'' clause for another reason.
		  * @throws NoSuchComponentException if `component` is not a component of this mapping.
		  */
		def selectForm[T](component :Component[T]) :SQLReadForm[T] = MappingReadForm.select(refine, component)

		/** Read form for a component of this mapping used inside a ''select'' clause, consisting of its specified
		  * `subcomponents`. It is a way to modify the default list of used columns by including those which normally
		  * are not (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] buff).
		  * All components on the list (and their columns) are first aliased to their operative versions
		  * by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method. The column list stays consistent
		  * with [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault selectedByDefault]]
		  * with the same arguments. Note that `ExplicitSelect` buffs are ignored only if declared on the export version
		  * of a listed component and not any of their subcomponents: if both one of the components and its column
		  * declare an `ExplicitSelect` buff, the column will not be included, unless it is also (directly) present in
		  * `components` argument. On the other hand, omitting a component with an `OptionalSelect` buff will omit
		  * all its columns, including those with the same buff (just as all its mandatory columns).
		  * @param component     a component of this mapping of the desired type.
		  * @param subcomponents a collection of components of this mapping (not necessarily those of `subcomponents`,
		  *                      including export versions for this mapping), defining the column set used by the form.
		  *                      It must cover all non-optional columns of `component`, that is all columns
		  *                      without an `OptionalSelect` buff not inherited from `component`.
		  * @throws IllegalArgumentException if either `component` or one of `subcomponents` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] buff,
		  *                                  or `component` (in its export version for this mapping) contains
		  *                                  any columns without a
		  *                                  [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]
		  *                                  buff - not counting those inherited from `component` or some
		  *                                  its supercomponent - which are not covered by `subcomponents`.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping,
		  *                                  or `component` is not a component of this mapping.
		  */
		def selectForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLReadForm[T] =
			MappingReadForm.select(refine, component, subcomponents)

		/** Default write form used to set a statement parameter of this mapping's `Subject` type, when used
		  * in a ''where'' clause to compare with another entity of the same type. This form can span multiple columns,
		  * listed by the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault filteredByDefault]]
		  * property.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView)* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`)`.
		  * Implementations should not eagerly reference the columns of this instance (i.e., call
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filterable filterable]] or any of its sibling
		  * methods before the constructor of this instance completes). Doing so may lead to initialization errors,
		  * such as access of an uninitialized field, due to reference cycles introduced by foreign key components.
		  */ //consider: PK? all?
		def filterForm :SQLWriteForm[Subject] = newWriteForm(FilterView)

		/** A write form (included columns) used in ''WHERE'' clauses of ''select'', ''update'' and ''delete''
		  * statements when this mapping's subject is used directly in the SQL DSL or other filter conditions.
		  * It is a way to modify the default list of used columns by including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitFilter ExplicitFilter]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff). It is the same list
		  * as the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault filteredByDefault]]
		  * property. All components on the list (and their columns) are first aliased to their operative versions
		  * by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
		  * Note that `ExplicitFilter` buffs are ignored only if declared on the export version of a listed component
		  * and not any of their subcomponents: if both one of the components and its column declare
		  * an `ExplicitFilter` buff, the column will not be included, unless it is also (directly) present in
		  * `components` argument. On the other hand, omitting a component with an `OptionalFilter` buff will omit
		  * all its columns, including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,components* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`, components)`.
		  * @param components a list of components which should be included in the form. It can include both direct
		  *                   components and subcomponents, not only columns. The `ExplicitFilter` buff is ignored,
		  *                   if present, but including a component with `NoFilter` buff (or one implying it)
		  *                   will result in an exception; any subcomponents with said buff of any of these components
		  *                   are however silently ignored as in by default. The list must cover, directly
		  *                   or indirectly, all mandatory columns of this mapping (i.e. those, which - in their
		  *                   operative version - ''do not'' include the buff `NoFilterByDefault` or any that imply it).
		  * @return a write form including the exact set of columns covered by the specified components, equivalent
		  *         to [[[net.noresttherein.oldsql.schema.Mapping.writeForm(op:OperationView,components:Unique[Component[_]] writeForm]]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`, components)`.
		  * @throws IllegalArgumentException if the column set covered by the given components (in their operative
		  *                                  versions) includes a column
		  *                                  with a [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] buff
		  *                                  (or one which implies it), or if a column of this mapping exists which
		  *                                  does not belong to this set and does not have a
		  *                                  [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
		  *                                  buff.
		  * @throws NoSuchComponentException if any of the given mappings is not a component of this instance.
		  */
		def filterForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] = newWriteForm(FilterView, components)

		/** Default write form of the specified component of this mapping, used to set statement parameters
		  * for a value of `T`, when the component is used in a ''where'' clause, or when comparing two values
		  * of type `T` in general. This form can span multiple columns, as returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault(component* filteredByDefault]]`(component)`.
		  * This may be different both in number and in applied buffs to
		  * `component.`[[net.noresttherein.oldsql.schema.Mapping.filterForm filterForm]], which is based on
		  * the ''export'' columns of `component` itself, rather than this mapping.
		  *
		  * If this component contains a [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
		  * buff (or any buff implying it), it will be annulled for all its subcomponents and columns, effectively
		  * including the component as by
		  * [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable* apply]]`(Seq(component), Nil)`;
		  * this effect applies however only to that particular buff instance, and if a component or column declares
		  * another instance of [[net.noresttherein.oldsql.schema.Buff.ExplicitFilter ExplicitFilter]],
		  * it will not be included in  the form.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_])* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`, component)`.
		  * @param component any component of this mapping.
		  * @throws IllegalArgumentException if `component` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] buff.
		  * @throws NoSuchComponentException if `component` is not a component of this mapping.
		  */ //consider: PK? all?
		def filterForm[T](component :Component[T]) :SQLWriteForm[T] = newWriteForm(FilterView, component)

		/** A write form used to set statement parameters for a value of `T` when the specified component is used
		  * in a ''where'' clause or another comparison SQL expression, which includes all
		  * of its specified subcomponents. It allows to modify the column set used with respect to
		  * [[net.noresttherein.oldsql.schema.Mapping.filterForm[T](component:Component[T])* filterForm]]`(component)`,
		  * including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitFilter ExplicitFilter]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff).
		  * If a [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]] buff is present on
		  * either `component` or any of the `subcomponents`, it is cancelled; this effect applies automatically
		  * to all its subcomponents, but it doesn't cancel any other instances of `NoFilterByDefault`.
		  * If both one of the subcomponents and its column declare an `ExplicitFilter` buff,
		  * the column will not be included, unless it is also (directly) present in `subcomponents` argument.
		  * On the other hand, omitting a component with an `OptionalFilter` buff will omit all its columns,
		  * including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_],subcomponents:Unique[Component[_]])* newWriteForm]]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`, component, subcomponents)`.
		  * @param component     a component of this mapping of the desired type.
		  * @param subcomponents a collection of components of this mapping (not necessarily those of `subcomponents`,
		  *                      including export versions for this mapping), defining the column set used by the form.
		  *                      It must cover all non-optional columns of `component`, that is all columns
		  *                      without an `OptionalFilter` buff not inherited from `component`.
		  * @throws IllegalArgumentException if either `component` or one of `subcomponents` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] buff,
		  *                                  or `component` (in its export version for this mapping) contains
		  *                                  any columns without
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
		  *                                  buff - not counting those inherited from `component`
		  *                                  or some its supercomponent - which are not covered by `subcomponents`.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping,
		  *                                  or `component` is not a component of this mapping.
		  */
		def filterForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			newWriteForm(FilterView, component, subcomponents)

		/** Default write form (included columns) used for insert statements of this mapping.
		  * This form can span multiple columns, listed by
		  * the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault insertedByDefault]] property.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView)* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`)`.
		  * Implementations should not eagerly reference the columns of this instance (i.e., call
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertable insertable]] or any of its sibling
		  * methods before the constructor of this instance completes). Doing so may lead to initialization errors,
		  * such as access of an uninitialized field, due to reference cycles introduced by foreign key components.
		  */
		def insertForm :SQLWriteForm[Subject] = newWriteForm(InsertView)

		/** A write form (included columns) used in insert statements of this mapping's subjects.
		  * It is a way to modify the default list of used columns by including those which normally are not
		  * of used columns by including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert ExplicitInsert]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] buff). It is the same list
		  * as the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault insertedByDefault]]
		  * property. All components on the list (and their columns) are first aliased to their operative versions
		  * by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
		  * Note that `ExplicitInsert` buffs are ignored only if declared on the export version of a listed component
		  * and not any of their subcomponents: if both one of the components and its column declare
		  * an `ExplicitInsert` buff, the column will not be included, unless it is also (directly) present in
		  * `components` argument. On the other hand, omitting a component with an `OptionalInsert` buff will omit
		  * all its columns, including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,components* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`, components)`.
		  * @param components a list of components which should be included in the form. It can include both direct
		  *                   components and subcomponents, not only columns. The `ExplicitInsert` buff is ignored,
		  *                   if present, but including a component
		  *                   with a [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] buff
		  *                   (or one implying it) will result in an exception; any subcomponents with said buff of any
		  *                   of these components are however silently ignored as in by default. The list must cover,
		  *                   directly or indirectly, all mandatory columns of this mapping (i.e. those, which
		  *                   - in their operative version - ''do not'' include
		  *                   a [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] buff
		  *                   or any that imply it).
		  * @return a write form including the exact set of columns covered by the specified components, equivalent
		  *         to [[[net.noresttherein.oldsql.schema.Mapping.writeForm(op:OperationView,components:Unique[Component[_]] writeForm]]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`, components)`.
		  * @throws IllegalArgumentException if the column set covered by the given components (in their operative versions)
		  *         includes a column with the `NoInsert` buff (or one which implies it), or if a column of this mapping
		  *         exists which does not belong to this set and does not have the `NoInsertByDefault` buff.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping.
		  */
		def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] = newWriteForm(InsertView, components)


		/** Default write form of the specified component of this mapping, used to set statement parameters
		  * for a value of `T`, when inserting a row with this component.
		  * This form can span multiple columns, as returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault(component* insertedByDefault]]`(component)`.
		  * This may be different both in number and in applied buffs to
		  * `component.`[[net.noresttherein.oldsql.schema.Mapping.insertForm insertForm]], which is based on
		  * the ''export'' columns of `component` itself, rather than this mapping.
		  *
		  * If this component contains a [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]
		  * buff (or any buff implying it), it will be annulled for all its subcomponents and columns, effectively
		  * including the component as by
		  * [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable* apply]]`(Seq(component), Nil)`;
		  * this effect applies however only to that particular buff instance, and if a component or column declares
		  * another instance of [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert ExplicitInsert]],
		  * it will not be included in  the form.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_])* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`, component)`.
		  * @param component any component of this mapping.
		  * @throws IllegalArgumentException if `component` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] buff.
		  * @throws NoSuchComponentException if `component` is not a component of this mapping.
		  */
		def insertForm[T](component :Component[T]) :SQLWriteForm[T] = newWriteForm(InsertView, component)

		/** A write form used to set statement parameters for a value of `T` when inserting a row containing
		  * the specified component, which includes all of its specified subcomponents. It allows to modify
		  * the column set used with respect to [[net.noresttherein.oldsql.schema.Mapping.insertForm[T](component:Component[T])* insertForm]]`(component)`,
		  * including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert ExplicitInsert]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] buff).
		  * If a [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] buff is present on
		  * either `component` or any of the `subcomponents`, it is cancelled; this effect applies automatically
		  * to all its subcomponents, but it doesn't cancel any other instances of `NoInsertByDefault`.
		  * If both one of the subcomponents and its column declare an `ExplicitInsert` buff,
		  * the column will not be included, unless it is also (directly) present in `subcomponents` argument.
		  * On the other hand, omitting a component with an `OptionalInsert` buff will omit all its columns,
		  * including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_],subcomponents:Unique[Component[_]])* newWriteForm]]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`, component, subcomponents)`.
		  * @param component     a component of this mapping of the desired type.
		  * @param subcomponents a collection of components of this mapping (not necessarily those of `subcomponents`,
		  *                      including export versions for this mapping), defining the column set used by the form.
		  *                      It must cover all non-optional columns of `component`, that is all columns
		  *                      without an `OptionalInsert` buff not inherited from `component`.
		  * @throws IllegalArgumentException if either `component` or one of `subcomponents` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] buff,
		  *                                  or `component` (in its export version for this mapping)
		  *                                  contains any columns without a
		  *                                  [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]
		  *                                  buff - not counting those inherited from `component`
		  *                                  or some its supercomponent - which are not covered by `subcomponents`.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping,
		  *                                  or `component` is not a component of this mapping.
		  */
		def insertForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			newWriteForm(InsertView, component, subcomponents)

		/** Default write form (included columns) used for update statements of this mapping.
		  * This form can span multiple columns, listed by
		  * the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault updatedByDefault]] property.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView)* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`)`.
		  * Implementations should not eagerly reference the columns of this instance (i.e., call
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatable updatable]] or any of its sibling
		  * methods before the constructor of this instance completes). Doing so may lead to initialization errors,
		  * such as access of an uninitialized field, due to reference cycles introduced by foreign key components.
		  */
		def updateForm :SQLWriteForm[Subject] = newWriteForm(UpdateView)

		/** A write form (included columns) used in update statements of this mapping's subjects.
		  * It is a way to modify the default list of used columns by including those which normally are not
		  * of used columns by including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate ExplicitUpdate]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buff). It is the same list
		  * as the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault updatedByDefault]]
		  * property. All components on the list (and their columns) are first aliased to their operative versions
		  * by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
		  * Note that `ExplicitUpdate` buffs are ignored only if declared on the export version of a listed component
		  * and not any of their subcomponents: if both one of the components and its column declare
		  * an `ExplicitUpdate` buff, the column will not be included, unless it is also (directly) present in
		  * `components` argument. On the other hand, omitting a component with an `OptionalUpdate` buff will omit
		  * all its columns, including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,components* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`, components)`.
		  * @param components a list of components which should be included in the form. It can include both direct
		  *                   components and subcomponents, not only columns. The `ExplicitUpdate` buff is ignored,
		  *                   if present, but including a component with `NoUpdate` buff (or one implying it)
		  *                   will result in an exception; any subcomponents with said buff of any of these components
		  *                   are however silently ignored as in by default. The list must cover, directly or indirectly,
		  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
		  *                   ''do not'' include the buff `NoUpdateByDefault` or any that imply it).
		  * @return a write form including the exact set of columns covered by the specified components, equivalent
		  *         to [[[net.noresttherein.oldsql.schema.Mapping.writeForm(op:OperationView,components:Unique[Component[_]] writeForm]]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`, components)`.
		  * @throws IllegalArgumentException if the column set covered by the given components (in their operative
		  *                                  versions) includes a column
		  *                                  with a [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff
		  *                                  (or one which implies it), or if a column of this mapping exists which
		  *                                  does not belong to this set and does not have a
		  *                                  [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
		  *                                  buff.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping.
		  */
		def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] = newWriteForm(UpdateView, components)

		/** Default write form of the specified component of this mapping, used to set statement parameters
		  * for a value of `T`, when updating a row with this component.
		  * This form can span multiple columns, as returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault(component* updatedByDefault]]`(component)`.
		  * This may be different both in number and in applied buffs to
		  * `component.`[[net.noresttherein.oldsql.schema.Mapping.updateForm updateForm]], which is based on
		  * the ''export'' columns of `component` itself, rather than this mapping.
		  *
		  * If this component contains a [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
		  * buff (or any buff implying it), it will be annulled for all its subcomponents and columns,
		  * effectively including the component as by
		  * [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable* apply]]`(Seq(component), Nil)`;
		  * this effect applies however only to that particular buff instance, and if a component or column declares
		  * another instance of [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate ExplicitUpdate]],
		  * it will not be included in  the form.
		  *
		  * The default implementation forwards the call to the factory method
		  * [[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_])* newWriteForm]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`, component)`.
		  * @param component any component of this mapping.
		  * @throws IllegalArgumentException if `component` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff.
		  * @throws NoSuchComponentException if `component` is not a component of this mapping.
		  */
		def updateForm[T](component :Component[T]) :SQLWriteForm[T] = newWriteForm(UpdateView, component)

		/** A write form used to set statement parameters for a value of `T` when updating a row containing
		  * the specified component, which includes all of its specified subcomponents. It allows to modify
		  * the column set used with respect to [[net.noresttherein.oldsql.schema.Mapping.updateForm[T](component:Component[T])* updateForm]]`(component)`,
		  * including those which normally are not
		  * (have an [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate ExplicitUpdate]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buff).
		  * If a [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]] buff is present on
		  * either `component` or any of the `subcomponents`, it is cancelled; this effect applies automatically
		  * to all its subcomponents, but it doesn't cancel any other instances of `NoUpdateByDefault`.
		  * If both one of the subcomponents and its column declare an `ExplicitUpdate` buff,
		  * the column will not be included, unless it is also (directly) present in `subcomponents` argument.
		  * On the other hand, omitting a component with an `OptionalUpdate` buff will omit all its columns,
		  * including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call to the factory method
		  * [[[net.noresttherein.oldsql.schema.Mapping.newWriteForm(op:OperationView,component:Component[_],subcomponents:Unique[Component[_]])* newWriteForm]]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`, component, subcomponents)`.
		  * @param component     a component of this mapping of the desired type.
		  * @param subcomponents a collection of components of this mapping (not necessarily those of `subcomponents`,
		  *                      including export versions for this mapping), defining the column set used by the form.
		  *                      It must cover all non-optional columns of `component`, that is all columns
		  *                      without an `OptionalUpdate` buff not inherited from `component`.
		  * @throws IllegalArgumentException if either `component` or one of `subcomponents` contains
		  *                                  a [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff,
		  *                                  or `component` (in its export version for this mapping)
		  *                                  contains any columns without a
		  *                                  [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
		  *                                  buff - not counting those inherited from `component`
		  *                                  or some its supercomponent - which are not covered by `subcomponents`.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping,
		  *                                  or `component` is not a component of this mapping.
		  */
		def updateForm[T](component :Component[T], subcomponents :Unique[Component[_]]) :SQLWriteForm[T] =
			newWriteForm(UpdateView, component, subcomponents)


		/** Default write form (included columns) of this mapping used for the given SQL statement type.
		  * The columns included by the form are listed
		  * by the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns defaultColumns]] property.
		  * The default implementation forwards the call to the method specific to the given statement type,
		  * and their results should remain consistent if overriden.
		  * @param op a projection selecting columns which should be included in the form.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
		  */
		def writeForm(op :WriteOperationView) :SQLWriteForm[Subject] = op.form(refine)

		/** Default write form (included columns) of this mapping used for the given SQL statement type.
		  * The default implementation forwards the call to the method specific to the given statement type,
		  * and their results should remain consistent if overriden.
		  * @param op         a projection selecting columns which should be included in the form.
		  * @param components a list of components which should be included in the form. It can include both direct
		  *                   components and subcomponents, not only columns.
		  *                   An [[net.noresttherein.oldsql.OperationView.Explicit Explicit]] buff buff is ignored
		  *                   if present, but including a component with
		  *                   an `op.`[[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff
		  *                   (or one implying it) will result in an exception; any subcomponents with said buff
		  *                   of any of these components are however silently ignored as in by default.
		  *                   The list must cover, directly or indirectly, all mandatory columns of this mapping
		  *                   (i.e. those, which - in their operative version - ''do not'' include
		  *                   an `op.`[[net.noresttherein.oldsql.OperationView.Optional Optional]] buff
		  *                   or any that imply it).
		  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
		  * @throws IllegalArgumentException if any of the components are annotated with
		  *                                  an `op.Prohibited` buff, or any of the non-covered columns
		  *                                  do not have an `op.Optional` buff.
		  * @throws NoSuchComponentException if any of the components are not a component of this mapping.
		  */
		def writeForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[Subject] =
			op.form(refine, components)

		/** The default write form used to set statement parameters for the specified component's subject type,
		  * when used in the context of an SQL statement/clause defined by `op`. This form can span multiple columns,
		  * as returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component* defaultColumns]]`(op, component)`.
		  * This may be different both in number and in applied buffs to
		  * `component.`[[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm]], which is based
		  * on the ''export'' columns of `component` itself, rather than this mapping.
		  *
		  * If this component contains an `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff
		  * (or any buff implying it), it will be annulled for all its subcomponents and columns, effectively including
		  * the component as by the [[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable* apply]]`(Seq(component), Nil)`;
		  * this effect applies however only to that particular buff instance, and if a component or column declares
		  * another instance of `op.`[[net.noresttherein.oldsql.OperationView.Explicit Explicit]],
		  * it will not be included in  the form.
		  *
		  * The default implementation forwards the call to `op`, which invokes the method of this mapping specific
		  * to its operation type in double dispatch.
		  * @param op        a projection selecting columns which should be included in the form.
		  * @param component any component of this mapping.
		  * @throws IllegalArgumentException if `component` contains an
		  *                                  `op.`[[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff,
		  *                                  or any of the ''not'' included columns do not contain
		  *                                  a [[net.noresttherein.oldsql.OperationView.Optional]] buff.
		  * @throws NoSuchComponentException if `component` is not a component of this mapping.
		  */
		def writeForm[T](op :WriteOperationView, component :Component[T]) :SQLWriteForm[T] =
			op.form(refine, component)

		/** A write form used to set statement parameters for a value of `T` in the context of an SQL statement/clause
		  * defined by `op`, which includes all the specified subcomponents. It allows to modify
		  * the column set used with respect to the
		  * [[net.noresttherein.oldsql.schema.Mapping.writeForm[T](op:OperationView,component:Component[T])* default]]
		  * form of `component`, including those which normally are not
		  * (have an `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff),
		  * or excluding those which normally are used, but are not mandatory
		  * (have an `op.`[[net.noresttherein.oldsql.OperationView.Optional Optional]] buff).
		  * If an `op.NonDefault`buff is present on either `component` or any of the `subcomponents`, it is cancelled;
		  * this effect applies automatically to all its subcomponents, but it doesn't cancel any other instances
		  * of `NonDefault`. If both one of the subcomponents and its column declare
		  * an `op.`[[net.noresttherein.oldsql.OperationView.Explicit Explicit]] buff,
		  * the column will not be included, unless it is also (directly) present in `subcomponents` argument.
		  * On the other hand, omitting a component with an `Optional` buff will omit all its columns,
		  * including those with the same buff (just as all its mandatory columns).
		  *
		  * The default implementation forwards the call in double dispatch to `op`, which invokes the variant
		  * specific to its type.
		  * @param op            a projection selecting columns which should be included in the form.
		  * @param component     a component of this mapping of the desired type.
		  * @param subcomponents a collection of components of this mapping (not necessarily those of `subcomponents`,
		  *                      including export versions for this mapping), defining the column set used by the form.
		  *                      It must cover all non-optional columns of `component`, that is all columns
		  *                      without an `OptionalInsert` buff not inherited from `component`.
		  * @throws IllegalArgumentException if either `component` or one of `subcomponents` contains
		  *                                  an [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff,
		  *                                  or `component` (in its export version for this mapping)
		  *                                  contains any columns without an
		  *                                  `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]]
		  *                                  buff - not counting those inherited from `component`
		  *                                  or some its supercomponent - which are not covered by `subcomponents`.
		  * @throws NoSuchComponentException if the list contains a mapping which is not a component of this mapping,
		  *                                  or `component` is not a component of this mapping.
		  */
		def writeForm[T](op :WriteOperationView, component :Component[T], subcomponents :Unique[Component[_]])
				:SQLWriteForm[T] =
			op.form(refine, component, subcomponents)

		/** A low level write form including the listed columns and only them.
		  * It is not specific to any operation type and thus ignores all typical buffs except for
		  * [[net.noresttherein.oldsql.schema.Buff.Virtual]]. It also does not depend on
		  * [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]] methods, instead
		  * using the column's extract directly to obtain the value for a column.
		  */
		def writeForm(columns :Unique[Column[_]]) :SQLWriteForm[Subject] =
			if (columns.isEmpty)
				SQLWriteForm.empty
			else
				columns.view.map { c =>
					def columnForm[X](extract :ColumnMappingExtract[Subject, X, Origin]) :SQLWriteForm[Subject] =
						if (Virtual.active(extract.export))
							throw new IllegalArgumentException(
								columns.mkString(
									"Cannot use a virtual column " + extract.export + " of " + this + " in a write form: ",
									", ", "."
								)
							)
						else
							extract.export.form compose extract
					columnForm(apply(c))
				}.reduce(_ + _)

		/** A write form for the given component of this mapping, including all of its columns which do not feature
		  * a [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]] buff
		  * in their [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] versions).
		  * This is a lower-level method which ignores all standard buffs which affect if a column is included
		  * in an [[net.noresttherein.oldsql.OperationView OperationView]]/form except for the latter.
		  * It also bypasses [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]] method,
		  * using directly the columns' [[net.noresttherein.oldsql.schema.ColumnExtract extracts]] to retrieve
		  * column values. For the above reason, this method is used only in some specific cases (mainly for setting
		  * JDBC parameters of the `Subject` type in other contexts than saving/updating entities)
		  * and its overloaded variants accepting an [[net.noresttherein.oldsql.OperationView OperationView]]
		  * should be use whenever a value for the component is inserted, updated or compared with another one.
		  */
		def writeForm[T](component :Component[T]) :SQLWriteForm[T] = {
			val forms = columnsWithout(component, Virtual).view.map { column =>
				def columnForm[X](extract :ColumnMappingExtract[T, X, Origin]) :SQLWriteForm[T] =
					if (Virtual.active(extract.export))
						throw new IllegalArgumentException(
							columns.mkString(
								"Cannot use a virtual column " + extract.export + " of " + this + " in a write form: ",
								", ", "."
							)
						)
					else
						extract.export.form compose extract
				columnForm(component(column.original))
			}
			if (forms.isEmpty) SQLWriteForm.empty
			else forms.reduce(_ + _)
		}

		/** A write form for the given component of this mapping, including all of the given columns and only them.
		  * This is a lower-level method which ignores all standard buffs which affect if a column is included
		  * in an [[net.noresttherein.oldsql.OperationView OperationView]]/form
		  * except for [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]].
		  * It also bypasses [[net.noresttherein.oldsql.schema.Mapping.writtenValues writtenValues]] method,
		  * using directly the columns' [[net.noresttherein.oldsql.schema.ColumnExtract extracts]] to retrieve
		  * column values. For the above reason, this method is used only in some specific cases (mainly for setting
		  * JDBC parameters of the `Subject` type in other contexts than saving/updating entities)
		  * and its overloaded variants accepting an [[net.noresttherein.oldsql.OperationView OperationView]]
		  * should be use whenever a value for the component is inserted, updated or compared with another one.
		  */
		def writeForm[T](component :Component[T], columns :Unique[Column[_]]) :SQLWriteForm[T] =
			if (columns.isEmpty)
				SQLWriteForm.empty
			else
				columns.view.map { c =>
					val export = this.export(c)
					if (Virtual.active(export))
						throw new IllegalArgumentException(
							columns.mkString(
								"Cannot use a virtual column " + export + " of " + this + " in a write form: ",
								", ", "."
							)
						)
					def columnForm[X](extract :ColumnMappingExtract[T, X, Origin]) :SQLWriteForm[T] =
						extract.export.form compose extract
					columnForm(component(c))
				}.reduce(_ + _)


		/** Default write form (included columns) of this mapping used for the given SQL statement type.
		  * This method is the generic implementation for
		  * [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm]], as well as the form properties specific
		  * to individual statement types.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
		  */
		protected def newWriteForm(op :WriteOperationView) :SQLWriteForm[Subject] = MappingWriteForm(op, refine)

		/** Default write form (included columns) of this mapping used for the given SQL statement type.
		  * This method is the generic implementation for
		  * [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm]], as well as the form properties specific
		  * to individual statement types.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
		  */
		protected def newWriteForm(op :WriteOperationView, components :Unique[Component[_]]) :SQLWriteForm[Subject] =
			MappingWriteForm(op, refine, components)

		/** Default write form for the given mapping, when used as a component of this mapping in the context
		  * of the specified SQL statement/clause. This method is the default generic implementation for
		  * [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm]], as well as methods specific
		  * to individual statement types.
		  */
		protected def newWriteForm[T](op :WriteOperationView, component :Component[T]) :SQLWriteForm[T] =
			MappingWriteForm(op, refine, component)

		/** A write form for the given component which includes the specified subcomponents, when used in the context
		  * of the specified SQL statement/clause. This method is the default generic implementation for
		  * [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm]], as well as methods specific
		  * to individual statement types.
		  */
		protected def newWriteForm[T](op :WriteOperationView, component :Component[T], subcomponents :Unique[Component[_]])
				:SQLWriteForm[T] =
			MappingWriteForm(op, refine, component, subcomponents)



		/** A reference to this instance as a `TypedMapping` with its own `Origin` and `Subject` types.
		  * This is a simple cast, and this method is final, so any extending type must be also a subtype of `Comp`.
		  */
		//Consider: it is final and returns this, but MappingTemplate doesn't have (and can't have) Comp self type.
		// This is a potential problem for classes which extend MappingTemplate, but not Comp,
		// such as DeepProxy.BackerMapping. However, the method is erased, and the only place
		// where we depend on Comp return type (rather than Component) is in selfExtract, which is overridden
		// in the proxy class. This is not ideal and we should refactor it.
		final def refine :Comp[Subject, Origin] = this.asInstanceOf[Comp[Subject, Origin]]

		/** The mapping of which this mapping is the export version (or in other way derived, while preserving
		  * the `Subject` type). Default implementation returns simply this instance,
		  * but proxy mapping implementations which know to be used as export version of their
		  * [[net.noresttherein.oldsql.schema.support.DelegateMapping.backer backer]] return `backer.original` instead.
		  * The intention is that this method will return the original mapping of a specific class, as defined
		  * by the application code. A fully reliable implementation is impossible in light of extensive use of
		  * polymorphism of this class and is difficult to even formally express due to its existential nature,
		  * so it should be relied only as a last resort fallback where the only alternative would be throwing
		  * an exception. It is used in particular to relate corresponding components of different,
		  * homomorphic instances of the same mapping class.
		  *
		  * The original is always [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]] with this instance
		  * and `original.original == original`. Typically, this is a domain model class (or an instance
		  * directly created by the application for dynamically created mappings), unwrapped of all framework decorators.
		  */
		//Consider: renaming to nominal and make it official that for any stack of proxies, any transformations,
		// it is the original from which we started, in particular
		// ComponentSQL.{anchored, altered, export).original == ComponentSQL.mapping.
		def original :Component[Subject] = refine

		/** An identity extract for this very mapping. */ //consider: renaming to wholeExtract
		protected def selfExtract :SpecificExtract[Comp[Subject, Origin], Subject, Subject, Origin] =
			SpecificExtract.ident(refine)

		/** A dictionary mapping all subcomponents of this mapping to extracts with their ''export'' versions.
		  * It is used during the assembly process to alias all components to their operative versions, in order
		  * to make sure that even if a subcomponent implementation asks for a value of one of its components
		  * which have been modified by some enclosing component, the mapping passed
		  * to its [[net.noresttherein.oldsql.haul.ComponentValues Pieces]] is substituted with the export version
		  * as defined by this mapping (the root). This is both to alias all versions to the same instance
		  * for the purpose of presetting a value, as well as using possibly overriden
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method, modifying the assembly process
		  * producing the subject value.
		  * Unlike the [[net.noresttherein.oldsql.schema.Mapping.subcomponents subcomponents]] list, this collection
		  * must contain not only all export components, but also all their original versions as defined by their
		  * parent mappings. It must also contain entries for all mapping instances which can be potentially used in the
		  * assembly process, even if they are not exposed - failing to include such a hidden component will most likely
		  * result in an exception being thrown from [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method.
		  * This again stands in contrast to the `subcomponents` list in that the latter may potentially omit 'synthetic'
		  * components - implementation artifacts used to assemble intermediate values and/or proxies.
		  * In some circumstances it may be acceptable for a component to be absent from this list - this is the case
		  * with private components managed by another mapping and guaranteed to not be used for the assembly
		  * (for example, only serving as a group of columns used directly by its parent), or a table mapping
		  * implementation which is known to use all its components as-is (performing no aliasing, with every component
		  * being its export version), but these are all exceptions which should be avoided in general. The map may,
		  * but doesn't have to if this mapping is not a column, contain an identity extract for this instance.
		  *
		  * The general way of building this map is by starting with entries for all mappings on the `subcomponents`
		  * list (which would map to themselves, being export components) and then, for every one of them, adding
		  * all `extracts` maps with ''their'' components, composed with the extract of the enclosing direct component.
		  * If a given component has not been changed in any way (it is its own export version and used directly
		  * for the assembly), this approach is sufficient. If the component however is a decorator applying
		  * some changes, then there might be a need to cascade those changes to the components of the decorated mapping.
		  * This depends on the implementation of the decorator, but the export components of an export component
		  * from the `components` list of this mapping aren't guaranteed to be export components of this mapping.
		  * If it is not the case, they should be adapted to include the changes made to their parent, in the process
		  * creating new export components of this mapping (which should, in general, be present
		  * on the `subcomponents` list). All the original versions of such subcomponents should receive
		  * additional entries in this map by associating them with the extracts of their export adapters.
		  *
		  * For example, if a mapping of a `Person` class contains a component of `Address`, but adapts it by adding
		  * a column prefix "home_", the columns "street", "city", "country" of the address component are replaced
		  * with columns "home_street", "home_city", "home_country" in the person mapping. The latter should
		  * in that case include entries mapping the new columns to the extracts containing them
		  * (created from the extracts provided by the address component), but also the original three columns,
		  * mapping "street" to the extract for "home_street" and so on. This does not apply however if the prefix
		  * was added directly to the mapping class of `Address`, as it would result in its columns already having
		  * their final names and all being their own export versions.
		  *
		  * This is an implementation-oriented method, with all base classes intended for extension by the client code
		  * already providing a proper implementation.
		  */
		def extracts          :NaturalMap[Component, like[Comp]#Extract]

		/** A dictionary mapping all columns of this mapping, direct and indirect, to extracts
		  * with their ''export'' versions. It is used during the assembly process to alias all components
		  * to their operative versions, in order to make sure that even if a subcomponent implementation asks
		  * for a value of one of its columns which have been modified by some enclosing component, the column passed
		  * to its `Pieces` is substituted with the export version as defined by this mapping (the root).
		  * This is both to alias all versions to the same instance for the purpose of presetting a value,
		  * as well as using their [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method
		  * (instead of the aliased component's), possibly modified by modifications to the buff list.
		  * If this component is a column, the map is a singleton containing this mapping and an identity extract.
		  * Unlike the [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list, this collection
		  * must contain not only all export columns, but also all their original versions as defined by their
		  * parent mappings. It must also contain entries for all mapping instances which can be potentially used in the
		  * assembly process, even if they are not exposed - failing to include such a hidden component will most likely
		  * result in an exception being thrown from [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] method documentation for more information
		  *     about implementation.
		  */
		def columnExtracts    :NaturalMap[Column, like[Col]#Extract]

		//fixme: go through *all* overrides of these methods (and forms) to make sure they are lazy
		/** Direct component mappings of this mapping, including any top-level columns. Always empty for columns.
		  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
		  * or mapped components and aliases for other components of the `Mapping`. For example, if a foreign key
		  * consists of columns with a business meaning by themselves, a mapping could include these columns directly
		  * as normal and create an 'ephemeral' component for the foreign key, being a mapping assembling its subject
		  * from these columns in the normal way, but which is not featured on any components list. For all non-column
		  * components however this list will cover all columns defined directly by the mapping. The components
		  * on the list are always the ''export'' versions, but their subcomponents are not, generally,
		  * the ''export'' versions from the point of view of this mapping.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  */
		def components        :Unique[Comp[_, Origin]]

		/** All transitive components of this mapping (i.e. components/columns declared directly by this mapping,
		  * by it's components or other subcomponents), or all that this mapping cares to expose, as instances
		  * of `this.`[[net.noresttherein.oldsql.schema.Mapping.Component Component]]`[_]`. It is typically defined by
		  * recursive ''flat mapping'' over the [[net.noresttherein.oldsql.schema.Mapping.components components]] list
		  * and including the direct components themselves. This list is always empty for columns, thus ending
		  * any similar recursion. Some mappings may wish not to expose some of the components they define, primarily
		  * in the case of adapted or mapped components and aliases for other components of the `Mapping`.
		  * For all non-column components however this list will cover all columns defined by the mapping.
		  * The components on the list are always the ''export'' versions, but their subcomponents are not, generally,
		  * the ''export'' versions from the point of view of this mapping.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  */
		def subcomponents     :Unique[Comp[_, Origin]]


		/** All direct and transitive columns declared within this mapping. This will include columns which are
		  * read-only, write-only and so on. Column mappings returns a singleton list containing themselves.
		  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping,
		  * and together cover the whole mapping [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]].
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  */
		def columns           :Unique[Col[_, Origin]]

		//todo: mandatorySelect etc
		/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` buff).
		  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView)]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault]]
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  */ //consider: renaming to allSelectable. Reduced name conflict chance, emphasises the difference from selectedByDefault
		def selectable        :Unique[Col[_, Origin]] = columnsWithout(NoSelect)

		/** All columns which can be part of an SQL statement's ''where'' clause (don't have the `NoFilter` buff active).
		  * Non filterable columns cannot occur in a ''group by'' clause, or a ''union'' query. This in particular
		  * should be true for `CLOB`, `BLOB` and similar SQL types.
		  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView)]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault]]
		  */ //consider: renaming to comparable
		def filterable        :Unique[Col[_, Origin]] = columnsWithout(NoFilter)

		/** All columns which can occur in an insert statement (don't have the `NoInsert` buff active).
		  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView)]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault]]
		  */
		def insertable        :Unique[Col[_, Origin]] = columnsWithout(NoInsert)

		/** All columns which can be updated on existing database records (don't have the `NoUpdate` buff active).
		  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView)]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault]]
		  */
		def updatable         :Unique[Col[_, Origin]] = columnsWithout(NoUpdate)
		//todo: arguments/callArguments ('callable') (stupid name)

		/** Columns autogenerated by the database on insert (have the `AutoInsert` buff); this implies
		  * being non-insertable. All columns on the list are the ''export'' (operative) versions from the point of view
		  * of this mapping. The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView)]]
		  */
		def autoInserted      :Unique[Col[_, Origin]] = columnsWith(AutoInsert)

		/** All columns which are updated by the database on each update statement (have the `AutoUpdate` buff)
		  * and could/should be returned to the application). All columns on the list are the ''export'' (operative)
		  * versions from the point of view of this mapping. The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  */
		def autoUpdated       :Unique[Col[_, Origin]] = columnsWith(AutoUpdate)

		//fixme: an inconsistency between selectable and selectedByDefault, where the latter ignores NoSelectByDefault
		// on this mapping, but the former returns only columns without NoSelect. Also, on ColumnMapping
		// these collections are always equal (that is, empty or containing the column itself).
		/** Columns included in ''select'' clauses using this component unless the list is modified by an explicit
		  * request for including or excluding certain optional component. These are all ''export'' columns of
		  * this mapping which do not have a [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]
		  * buff in any form (note that this also includes buff [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]]).
		  * However, if this component itself has an `NoSelectByDefault` (declared or inherited, but not a `NoSelect`)
		  * buff, it is suppressed for the purpose of this method. This results in effectively including this component
		  * in an enclosing mapping. `NoSelectByDefault` buffs on any subcomponents of this mapping (in particular,
		  * columns) remain active and are inherited by its columns.
		  *
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions. This list should always be consistent
		  * with [[net.noresttherein.oldsql.schema.Mapping.defaultColumns defaultColumns]]`(`[[net.noresttherein.oldsql.OperationView.SelectView SelectView]]`)`,
		  * it is provided here as a shorthand and in order to allow its caching.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @return The default implementation delegates to
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns includedColumns]]`(NoSelectByDefault, this)`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView)]]
		  */
		def selectedByDefault :Unique[Col[_, Origin]] = includedColumns(NoSelectByDefault, refine)

		/** Columns included when comparing this component with another [[net.noresttherein.oldsql.sql.SQLExpression]],
		  * with the value type being equal to `this.Subject`, especially inside ''where'' clauses of SQL ''selects'',
		  * unless an explicit request is made for including or excluding certain optional component.
		  * These are all ''export'' columns of
		  * this mapping which do not have a [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
		  * buff in any form (note that this also includes buff [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]]).
		  * However, if this component itself has an `NoFilterByDefault` (declared or inherited, but not a `NoFilter`)
		  * buff, it is suppressed for the purpose of this method. This results in effectively including this component
		  * in an enclosing mapping. `NoFilterByDefault` buffs on any subcomponents of this mapping (in particular,
		  * columns) remain active and are inherited by its columns.
		  *
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions. This list should always be consistent
		  * with [[net.noresttherein.oldsql.schema.Mapping.defaultColumns defaultColumns]]`(`[[net.noresttherein.oldsql.OperationView.FilterView FilterView]]`)`,
		  * it is provided here as a shorthand and in order to allow its caching.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @return The default implementation delegates to
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns includedColumns]]`(NoFilterByDefault, this)`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView)]]
		  */
		def filteredByDefault :Unique[Col[_, Origin]] = includedColumns(NoFilterByDefault, refine)

		/** Columns included in all SQL ''insert'' statements for this component, unless certain optional components
		  * are explicitly included or excluded. These are all ''export'' columns of
		  * this mapping which do not have a [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]
		  * buff in any form (note that this also includes buff [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]]).
		  * However, if this component itself has an `NoInsertByDefault` (declared or inherited, but not a `NoInsert`)
		  * buff, it is suppressed for the purpose of this method. This results in effectively including this component
		  * in an enclosing mapping. `NoInsertByDefault` buffs on any subcomponents of this mapping (in particular,
		  * columns) remain active and are inherited by its columns.
		  *
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions. This list should always be consistent
		  * with [[net.noresttherein.oldsql.schema.Mapping.defaultColumns defaultColumns]]`(`[[net.noresttherein.oldsql.OperationView.InsertView InsertView]]`)`,
		  * it is provided here as a shorthand and in order to allow its caching.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @return The default implementation delegates to
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns includedColumns]]`(NoInsertByDefault, this)`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView)]]
		  */
		def insertedByDefault :Unique[Col[_, Origin]] = includedColumns(NoInsertByDefault, refine)

		/** Columns included in all SQL ''update'' statements for this component, unless certain optional components
		  * are explicitly included or excluded. These are all ''export'' columns of
		  * this mapping which do not have a [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
		  * buff in any form (note that this also includes buff [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]]).
		  * However, if this component itself has an `NoUpdateByDefault` (declared or inherited, but not a `NoUpdate`)
		  * buff, it is suppressed for the purpose of this method. This results in effectively including this component
		  * in an enclosing mapping. `NoUpdateByDefault` buffs on any subcomponents of this mapping (in particular,
		  * columns) remain active and are inherited by its columns.
		  *
		  * The columns are listed in the same order as they appear
		  * in the full [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]] list,
		  * barring omissions. This list should always be consistent
		  * with [[net.noresttherein.oldsql.schema.Mapping.defaultColumns defaultColumns]]`(`[[net.noresttherein.oldsql.OperationView.UpdateView UpdateView]]`)`,
		  * it is provided here as a shorthand and in order to allow its caching.
		  *
		  * Overrides should happen either by `def` or `lazy val` rather than a precomputed constant,
		  * as reference cycles introduced by foreign key components can lead to initialization errors
		  * (a [[NullPointerException]] being thrown due to access to an uninitialized field) if a foreign key component
		  * is accessed before the constructor of the referenced table mapping returns.
		  * @return The default implementation delegates to
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns includedColumns]]`(NoUpdateByDefault, this)`.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView)]]
		  */
		def updatedByDefault  :Unique[Col[_, Origin]] = includedColumns(NoUpdateByDefault, refine)


		//todo: override it in all the same places.
		/** All columns of this mapping, which must be included in an SQL ''select'' returning this component.
		  * These are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] columns
		  * which do not have an [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] buff.
		  * If this component itself has an `OptionalSelect` buff, it is ignored for this purpose.
		  */
		def mandatorySelect   :Unique[Col[_, Origin]] = updatedByDefault.filter(OptionalSelect.inactive)

		/** All columns of this mapping, which must be included when values of this component are compared in SQL.
		  * These are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] columns
		  * which do not have an [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff.
		  * If this component itself has an `OptionalFilter` buff, it is ignored for this purpose.
		  */
		def mandatoryFilter   :Unique[Col[_, Origin]] = filteredByDefault.filter(OptionalFilter.inactive)

		/** All columns of this component, which must be included in an SQL ''insert'' for a table of this mapping.
		  * These are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] columns
		  * which do not have an [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] buff.
		  * If this component itself has an `OptionalInsert` buff, it is ignored for this purpose.
		  */
		def mandatoryInsert   :Unique[Col[_, Origin]] = insertedByDefault.filter(OptionalInsert.inactive)

		/** All columns of this mapping, which must be included in an SQL ''update'' setting a value of this component.
		  * These are [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] columns
		  * which do not have an [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buff.
		  * If this component itself has an `OptionalUpdate` buff, it is ignored for this purpose.
		  */
		def mandatoryUpdate   :Unique[Col[_, Origin]] = mandatoryUpdate.filter(OptionalUpdate.inactive)


		/** All columns of  the given component, in their ''export'' versions for this mapping.
		  * @return `component.columns.map(export(_))` by default.
		  */ //todo: verify that all methods which take a component accept the mapping itself.
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def columns[T](component :Component[T])      :Unique[Col[_, Origin]] = component.columns.map(export(_))

		/** All columns of `component` which are considered selectable by this mapping. $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWithout(buff:BuffType,component:Component[_]) columnsWithout]]`(NoSelect, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault[T](component:Component[T])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def selectable[T](component :Component[T])   :Unique[Col[_, Origin]] = columnsWithout(component, NoSelect)

		/** All columns of `component` which can occur in SQL comparisons with other SQL expressions (in particular,
		  * inside SQL ''where'' clauses) by this mapping.
		  * $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWithout(buff:BuffType,component:Component[_]) columnsWithout]]`(NoFilter, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault[T](component:Component[T])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def filterable[T](component :Component[T])   :Unique[Col[_, Origin]] = columnsWithout(component, NoFilter)

		/** All columns of `component` which are considered insertable by this mapping. $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWithout(buff:BuffType,component:Component[_]) columnsWithout]]`(NoInsert, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault[T](component:Component[T])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def insertable[T](component :Component[T])   :Unique[Col[_, Origin]] = columnsWithout(component, NoInsert)

		/** All columns of `component` which are considered updatable by this mapping. $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWithout(buff:BuffType,component:Component[_]) columnsWithout]]`(NoUpdate, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault[T](component:Component[T])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def updatable[T](component :Component[T])    :Unique[Col[_, Origin]] = columnsWithout(component, NoUpdate)

		/** All columns of `component` which are considered auto inserted (generated by the database on insert)
		  * by this mapping. $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.AutoInsert AutoInsert]] active.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWith(buff:BuffType,component:Component[_]) columnsWit]]`(NoUpdate, component)`,
		  *         in an undefined order.
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def autoInserted[T](component :Component[T]) :Unique[Col[_, Origin]] = columnsWith(component, AutoInsert)

		/** All columns of `component` which are considered auto updated (generated by the database on update)
		  * by this mapping. $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with [[net.noresttherein.oldsql.schema.Buff.AutoUpdate AutoUpdate]] active.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWith(buff:BuffType,component:Component[_]) columnsWith]]`(AutoUpdate, component)`,
		  *         in an undefined order.
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def autoUpdated[T](component :Component[T])  :Unique[Col[_, Origin]] = columnsWith(component, AutoUpdate)


		/** All columns of `component` which are specified as selected by default this mapping. This is the column set
		  * used when an SQL expression for the given component is used inside a ''select'' clause. If the component
		  * has a [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]] buff
		  * (but not [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]]), this will have the effect of
		  * 'including' it: the buff becomes suppressed and is not inherited by any of its subcomponents or columns.
		  * If the buff appears however again under its component tree, then columns with it will not be included
		  * in the result.
		  * $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with `NoSelectByDefault` inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns(buff:BuffType,component:Component[_]) includedColumns]]`(NoSelectByDefault, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def selectedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			includedColumns(NoSelectByDefault, component)

		/** All columns of `component` which are specified as filtered by default (included in component vs component
		  * comparisons in SQL expressions, particularly in ''where'' clauses) by this mapping. If the component
		  * has a [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]] buff
		  * (but not [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]]), this will have the effect of
		  * 'including' it: the buff becomes suppressed and is not inherited by any of its subcomponents or columns.
		  * If the buff appears however again under its component tree, then columns with it will not be included
		  * in the result.
		  * $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with `NoFilterByDefault` inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns(buff:BuffType,component:Component[_]) includedColumns]]`(NoFilterByDefault, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def filteredByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			includedColumns(NoFilterByDefault, component)

		/** All columns of `component` which are specified as inserted by default by this mapping. If the component
		  * has a [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] buff
		  * (but not [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]]), this will have the effect of
		  * 'including' it: the buff becomes suppressed and is not inherited by any of its subcomponents or columns.
		  * If the buff appears however again under its component tree, then columns with it will not be included
		  * in the result.
		  * $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with `NoInsertByDefault` inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns(buff:BuffType,component:Component[_]) includedColumns]]`(NoInsertByDefault, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def insertedByDefault[T](component :Component[T]) :Unique[Col[_, Origin]] =
			includedColumns(NoInsertByDefault, component)

		/** All columns of `component` which are specified as updated by default by this mapping. If the component
		  * has a [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]] buff
		  * (but not [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]]), this will have the effect of
		  * 'including' it: the buff becomes suppressed and is not inherited by any of its subcomponents or columns.
		  * If the buff appears however again under its component tree, then columns with it will not be included
		  * in the result.
		  * $ExportSubcomponentsInfo
		  * All subcomponents need exporting before use; this method exports all columns of `component`
		  * and returns those with `NoUpdateByDefault` inactive.
		  * @return the same column set as [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.includedColumns(buff:BuffType,component:Component[_]) includedColumns]]`(NoUpdateByDefault, component)`,
		  *         in an undefined order.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns(op:OperationView,component:Component[_])* ]]
		  */
		@throws[NoSuchElementException]("if the argument is not a (sub)component of this mapping.")
		def updatedByDefault[T](component :Component[T])  :Unique[Col[_, Origin]] =
			includedColumns(NoUpdateByDefault, component)

		/** All columns of this mapping (in their [[net.noresttherein.oldsql.schema.Mapping.ExportColumn export]]
		  * versions) which can be used as part of the given SQL statement type. Default implementation forwards
		  * the call to the method specific to the given statement type, and should remain consistent with them
		  * if overriden.
		  * @param op An `enum`-like instance representing a particular database operation/statement type.
		  *           It defines projections of the full column set of this mapping which can be used in statements
		  *           of that type.
		  * @return all columns without a `op.`[[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectable]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filterable]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatable]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertable]]
		  */
		def columns(op :OperationView) :Unique[Col[_, Origin]] = op.applicableColumns(this)

		/** Lists the [[net.noresttherein.oldsql.schema.Mapping.ExportColumn export]] versions (from the point of view
		  * of this mapping) of the columns of the given component which can be used in the SQL statement type
		  * specified by `op`. The standard implementation forwards the call to the method specific to the
		  * statement type represented by `op` and should remain consistent with them, if overriden.
		  * @param op An `enum`-like instance representing a particular database operation/statement type.
		  *           It defines projections of the full column set of this mapping which can be used in statements
		  *           of that type.
		  * @return all columns without a `op.Prohibited` buff.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectable[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filterable[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatable[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertable[T](component:Component[T])]]
		  */
		def columns[T](op :OperationView, component :Component[T]) :Unique[Col[_, Origin]] =
			op.applicableColumns[Comp, Col, Origin](this, component)

		/** The default column set of this mapping (in their [[net.noresttherein.oldsql.schema.Mapping.ExportColumn export]]
		  * versions) which should be used as part of a specific SQL statement type. This method has the effect
		  * of effectively including this mapping: if it contains
		  * an `op.`[[net.noresttherein.oldsql.OperationView.Explicit Explicit]] buff it will be ignored,
		  * although any such buffs on its subcomponents will remain. This makes it different from simply
		  * filtering out all columns which contain (directly or inherited) buff
		  * `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]]. It follows then that,
		  * for column mappings, the effect is equivalent
		  * to [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns(op:OperationView) columns]]`(op)`.
		  *
		  * The standard implementation forwards the call to the `xxxByDefault` method specific to the given
		  * statement type. This method is only useful on root relation mappings, or on components of mappings
		  * which do not apply any changes to their subcomponents: in other cases the returned column set
		  * will not reflect any alterations defined by an enclosing mapping.
		  * @param op An `enum`-like instance representing a particular database operation/statement type.
		  *           It defines projections of the full column set of this mapping which can be used in statements
		  *           of that type.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault]]
		  */
		def defaultColumns(op :OperationView) :Unique[Col[_, Origin]] = op.defaultColumns(this)

		/** The default column set for a particular SQL statement type `op` (from the point of view of this mapping)
		  * of the given component, including `component` itself, if possible. The latter (and the fact that
		  * [[net.noresttherein.oldsql.schema.Mapping.ExportColumn export]] columns of a component are not necessarily
		  * ''export'' (and thus correct) columns of the enclosing mapping) makes it different from
		  * `component.`[[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.defaultColumns defaultColumns]] if
		  *   1. this mapping modifies any of these columns in some way (such as prepending a prefix to their name), or
		  *   1. the component is not included by default in the specified operation type.
		  * In the latter case, this method suppresses the
		  * `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff in all the subcomponents
		  * and columns of `component`. Note however that, if another instance of the buff (typically, in the form of
		  * a `op.`[[net.noresttherein.oldsql.OperationView.Explicit Explicit]] buff) is present on a subcomponent,
		  * it will be in effect, excluding those columns from the returned set. The result is essentially the same
		  * as [[[net.noresttherein.oldsql.schema.Mapping.apply(include:Iterable[Component[_]],exclude:Iterable[Component[_]])* altering]]]
		  * this mapping by including the `component` argument, and then taking its default column set for `op`,
		  * but all returned columns are still ''unchanged'' columns of this mapping, meaning they may still have
		  * the `Explicit` buff active.
		  *
		  * The standard implementation forwards to the `xxxByDefault` method specific to the given statement type,
		  * and should remain consistent with them if overriden.
		  * @param op        a selector of columns.
		  * @param component a direct or indirect component of this mapping, including this mapping itself.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.selectedByDefault[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.filteredByDefault[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.updatedByDefault[T](component:Component[T])]]
		  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.insertedByDefault[T](component:Component[T])]]
		  */
		def defaultColumns[T](op :OperationView, component :Component[T]) :Unique[Col[_, Origin]] =
			op.defaultColumns[Comp, Col, Origin](this, component)

		/** All columns from the `columns` list with a given buff active. */
		def columnsWith(buff :BuffType) :Unique[Col[_, Origin]] =
			columns.filter(buff.active)

		/** All columns without the given buff. */
		def columnsWithout(buff :BuffType) :Unique[Col[_, Origin]] =
			columns.filter(buff.inactive)

		/** All columns of `component` [[net.noresttherein.oldsql.schema.Mapping.export exported]] by this mapping
		  * which have the given buff. $ExportSubcomponentsInfo
		  * @return The same column set as
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]]`(component).filter(buff.active)`,
		  *         in an undefined order.
		  */
		def columnsWith(component :Component[_], buff :BuffType) :Unique[Col[_, Origin]] =
			component.columns.view map (export(_)) filter buff.active to Unique

		/** All columns of `component` [[net.noresttherein.oldsql.schema.Mapping.export exported]] by this mapping
		  * which do not have the given buff. $ExportSubcomponentsInfo
		  * @return The same column set as
		  *         [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columns columns]]`(component).filter(buff.active)`,
		  *         in an undefined order.
		  */
		def columnsWithout(component :Component[_], buff :BuffType) :Unique[Col[_, Origin]] =
			component.columns.view map (export(_)) filter buff.inactive to Unique

		/** Lists all columns of the given component of this mapping which,
		  * in their [[net.noresttherein.oldsql.schema.Mapping.export export]] versions for this mapping,
		  * do not the specified buff type. This is similar to
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.columnsWithout(buff:BuffType,component:Component[_])* columnsWithout]],
		  * but if the buff is present on the argument component itself, it becomes suppressed (not active on any
		  * of the columns of `component`). It does not mean however that this method returns the complete column
		  * set of the component, as if another copy of the buff is declared or inherited by a column, it will
		  * be excluded from the result.
		  * @param excludedBuff any buff type marking columns which should be excluded from the result.
		  * @param component a component of this mapping or this mapping itself.
		  */
		def includedColumns(excludedBuff :BuffType, component :Component[_]) :Unique[Col[_, Origin]] =
			export(component) match {
				case excludedBuff.Inactive() =>
					columnsWithout(component, excludedBuff)
				case comp =>
					val declaration = comp.buffs.tag
					comp.columns.view.map(export(_)).filter { column =>
						def predicate[T](column :Column[T]) = {
							val zipper = (column.buffs.zipper :>> declaration)
							excludedBuff.inactive( //remove any excludedBuff inherited from component or its supercomponents
								if (zipper.isBottom) column.buffs
								else zipper.replace(zipper.stack.filter(excludedBuff.inactive(_))).buffs
							)
						}
						predicate(column)
					} to Unique
//					val included = comp.withBuffs(comp.buffs.filter(excludedBuff.inactive))
//					included.columnsWithout(excludedBuff).map(counterpart(included, _))
			}



		/** Retrieves the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]]
		  * for the given component. Default implementation performs a lookup in
		  * [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]], but if no extract is found
		  * and `component eq this` than an identity extract is created as needed.
		  * Overrides should keep this contract.
		  * @throws NoSuchElementException if `component` is not a subcomponent of this mapping.
		  */ //consider: a method returning an Opt would be useful in DisassembledValues
		@throws[NoSuchElementException]("if the component is not a subcomponent of this instance.")
		def apply[T](component :Component[T]) :SpecificExtract[Comp[T, Origin], Subject, T, Origin] =
			if (component eq this)
				extracts.getOrElse[like[Comp]#Extract, T](component, null) match {
					case null =>
						selfExtract.asInstanceOf[SpecificExtract[Comp[T, Origin], Subject, T, Origin]]
					case ex => ex
				}
			else extracts(component)

		/** Retrieves the [[net.noresttherein.oldsql.schema.ColumnMappingExtract ColumnMappingExtract]]
		  * for the given column. Default implementation performs a lookup in
		  * [[net.noresttherein.oldsql.schema.Mapping.columnExtracts columnExtracts]].
		  * @throws NoSuchElementException if `column` is not a subcomponent of this mapping.
		  */
		@throws[NoSuchElementException]("if the argument is not a column of this instance.")
		def apply[T](column :Column[T]) :SpecificExtract[Col[T, Origin], Subject, T, Origin] =
			columnExtracts(column)

		/** Adapts the given subcomponent (a component being the end of some path of components starting
		  * with this instance) to its public version as present in the `subcomponents` and `components` list.
		  * For every valid transitive subcomponent of this mapping, there is exactly one equal to its export form
		  * on the `subcomponents` list. This process can modify the mapping definition by changing names of the columns
		  * (for example, prefixing them) and updating their buffs by cascading the buffs present on this instance.
		  * By default it returns the `export` property of the extract of the component, returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.apply[T](component:Component[T]) apply(component)]].
		  * Unlike the [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] map, this method
		  * is guaranteed to return `this` if `component eq this` (that is, this mapping is considered the export
		  * component of itself by this method). Passing an argument which is not a (sub)component of this mapping,
		  * export or otherwise, may, but does not have to, result in throwing a [[NoSuchElementException]]
		  * (typically a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]]).
		  */ //todo: rename to actual
		@throws[NoSuchElementException]("if the component is not a subcomponent of this instance.")
		def export[T](component :Component[T]) :Comp[T, Origin] =
			if (component eq this) this.asInstanceOf[Comp[T, Origin]]
			else apply(component).export

		/** Adapts the given column of one of the transitive subcomponents of this instance to the version present
		  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
		  * exactly one equal to its export version on the `columns` list. This process can change the name and buffs
		  * of the column, reflecting additional information present in this mapping.
		  * By default it returns the `export` property of the extract of the column, returned by
		  * [[net.noresttherein.oldsql.schema.Mapping.apply[T](column:Column[T]) apply(column)]].
		  * Unlike [[net.noresttherein.oldsql.schema.Mapping.columnExtracts columnExtracts]] map, this method
		  * is guaranteed to return `this` if `column eq this` (that is, this mapping is considered the export component
		  * of itself by this method). Passing an argument which is not a direct or indirect column of this mapping,
		  * export or otherwise, may, but does not have to, result in throwing a [[NoSuchElementException]]
		  * (typically a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]]).
		  */ //consider: renaming it to operative perhaps? it can easily conflict with an `export` column in client apps
		//todo: migrate these methods to always throwing NoSuchComponentException
		@throws[NoSuchElementException]("if the argument is not a column of this instance.")
		def export[T](column :Column[T]) :Col[T, Origin] =
			if (column eq this) this.asInstanceOf[Col[T, Origin]]
			else apply(column).export

		/** Same as [[net.noresttherein.oldsql.schema.Mapping.export export]]`(component)`, but instead of throwing
		  * a `NoSuchElementException`, it instead simply returns the argument itself.
		  * @return `extracts.getOrElse(component, component)`.
		  */
		def exportOrNot[T](component :Component[T]) :Component[T] = {
			val extract = extracts.getOrElse[like[Comp]#Extract, T](component, null)
			if (extract != null) extract.export else component
		}

		/** Same as [[net.noresttherein.oldsql.schema.Mapping.export export]]`(column)`, but instead of throwing
		  * a `NoSuchElementException`, it instead simply returns the argument itself.
		  * @return `extracts.getOrElse(column, column)`.
		  */
		def exportOrNot[T](column :Column[T]) :Column[T] = {
			val extract = columnExtracts.getOrElse[like[Col]#Extract, T](column, null) //todo: make sure there is no object allocation here
			if (extract != null) extract.export else column
		}

		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterpart of its component `component` in this mapping.
		  * A counterpart is a component mapping the same part of the mappings'
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]].
		  * This method is particularly useful when:
		  *   1. `other` is another instance of the same
		  *      [[net.noresttherein.oldsql.schema.bases.StaticMapping StaticMapping]], especially a nominal mapping
		  *      of a different [[net.noresttherein.oldsql.schema.Relation Relation]] of the same type;
		  *   1. `other` is an instance created from the same input as this mapping
		  *      (for example, another [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]s
		  *      for the same/equal/isomorphic SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]);
		  *   1. They are both [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] mappings
		  *      or other adapters (such as [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]],
		  *      [[net.noresttherein.oldsql.schema.support.PatchedMapping PatchedMapping]] or
		  *      [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]) of the same
		  *      [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.original original]] mapping.
		  *
		  * This method is a heuristic and does not guarantee type safety in the general case, as it relies
		  * on a dynamic comparison of component/column lists and declarations given by both `this` and `other`.
		  * @return the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] version
		  *         of a component of this mapping mirroring the given component.
		  */ //it's tempting to force other to have the same Subject, but it's a problem in contexts where we don't know it
		def counterpartOpt[T, O](other :MappingAt[O], component :TypedMapping[T, O]) :Opt[Comp[T, Origin]] =
			try {
				if (component == other)
					Got(this.asInstanceOf[Comp[T, Origin]])
				else if (other == this)
					Got(export(component.withOrigin[Origin]))
				else if (contains(component.original.withOrigin[Origin])) //the contains check will yield false positives if other is a subcomponent of this
					Got(export(component.original.withOrigin[Origin]))
				else if (other isomorphic refine)
					Got(subcomponents(other.subcomponents.sureIndexOf(other.export(component))).asInstanceOf[Comp[T, Origin]])
				else
					Lack
			} catch {
				case _ :NoSuchComponentException => Lack
			}

		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterpart of its column `column` in this mapping.
		  * A counterpart is a column mapping the same part of the mappings'
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]].
		  * This method is particularly useful when:
		  *   1. `other` is another instance of the same
		  *      [[net.noresttherein.oldsql.schema.bases.StaticMapping StaticMapping]], especially a nominal mapping
		  *      of a different [[net.noresttherein.oldsql.schema.Relation Relation]] of the same type;
		  *   1. `other` is an instance created from the same input as this mapping
		  *      (for example, another [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]s
		  *      for the same/equal/isomorphic SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]);
		  *   1. They are both [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] mappings
		  *      or other adapters (such as [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]],
		  *      [[net.noresttherein.oldsql.schema.support.PatchedMapping PatchedMapping]] or
		  *      [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]) of the same
		  *      [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.original original]] mapping.
		  *
		  * Note that this method is a heuristic and does not guarantee type safety in the general case, as it relies
		  * on dynamic comparison of component/column lists and declarations given by both `this` and `other`.
		  * @return the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] version
		  *         of a column of this mapping mirroring the given column.
		  */
		def counterpartOpt[T, O](other :MappingAt[O], column :TypedColumn[T, O]) :Opt[Col[T, Origin]] =
			try {
				if (column == other)
					Got(this.asInstanceOf[Col[T, Origin]])
				else if (this == other) //explicit implicits necessary because the most generic implicit is in lexical scope
					Got(export(column.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection)))
				else if (contains(column.original.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection)))
					Got(export(column.original.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection)))
				else if ((other isomorphic refine) || (other.columns isomorphic columns))
					Got(columns(other.columns.sureIndexOf(other.export(column))).asInstanceOf[Col[T, Origin]])
				else
					Lack
			} catch {
				case _ :NoSuchComponentException => Lack
			}

		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterpart of its component `component` in this mapping.
		  * A counterpart is a component mapping the same part of the mappings'
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]].
		  * This method is particularly useful when:
		  *   1. `other` is another instance of the same
		  *      [[net.noresttherein.oldsql.schema.bases.StaticMapping StaticMapping]], especially a nominal mapping
		  *      of a different [[net.noresttherein.oldsql.schema.Relation Relation]] of the same type;
		  *   1. `other` is an instance created from the same input as this mapping
		  *      (for example, another [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]s
		  *      for the same/equal/isomorphic SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]);
		  *   1. They are both [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] mappings
		  *      or other adapters (such as [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]],
		  *      [[net.noresttherein.oldsql.schema.support.PatchedMapping PatchedMapping]] or
		  *      [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]) of the same
		  *      [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.original original]] mapping.
		  *
		  * This method is a heuristic and does not guarantee type safety in the general case, as it relies
		  * on a dynamic comparison of component/column lists and declarations given by both `this` and `other`.
		  * It should be used only when `other` is known to be in fact compatible with this instance.
		  * @return the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] version
		  *         of a component of this mapping mirroring the given component.
		  */ //it's tempting to force other to have the same Subject, but it's a problem in contexts where we don't know it
		@throws[IncompatibleMappingsException]("if other is not isomorphic with this mapping.")
		@throws[NoSuchComponentException]("if component is not a component of other.")
		def counterpart[T, O](other :MappingAt[O], component :TypedMapping[T, O]) :Comp[T, Origin] =
			if (component == other)
				this.asInstanceOf[Comp[T, Origin]]
			else if (this == other)
				export(component.withOrigin[Origin])
			else if (contains(component.original.withOrigin[Origin])) //the contains check will yield false positives if other is a subcomponent of this
				counterpartOpt(other, component) match {
					case Got(res) => res
					case _ =>
						throw new IncompatibleMappingsException(
							"Mappings " + this + " and " + other + " do not seem to be related - " + component +
								" has no counterpart in the former.."
						)
				}
			else
				throw new NoSuchComponentException(
					"Cannot find a counterpart of component " + other + "/" + component + " in " + this + "."
				)


		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterpart of its column `column` in this mapping.
		  * A counterpart is a column mapping the same part of the mappings'
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.Subject Subject]].
		  * This method is particularly useful when:
		  *   1. `other` is another instance of the same
		  *      [[net.noresttherein.oldsql.schema.bases.StaticMapping StaticMapping]], especially a nominal mapping
		  *      of a different [[net.noresttherein.oldsql.schema.Relation Relation]] of the same type;
		  *   1. `other` is an instance created from the same input as this mapping
		  *      (for example, another [[net.noresttherein.oldsql.sql.SQLMapping SQLMapping]]s
		  *      for the same/equal/isomorphic SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]);
		  *   1. They are both [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] mappings
		  *      or other adapters (such as [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]],
		  *      [[net.noresttherein.oldsql.schema.support.PatchedMapping PatchedMapping]] or
		  *      [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]) of the same
		  *      [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.original original]] mapping.
		  *
		  * This method is a heuristic and does not guarantee type safety in the general case, as it relies
		  * on dynamic comparison of component/column lists and declarations given by both `this` and `other`.
		  * It should be used only when `other` is known to be in fact compatible with this instance.
		  * @return the [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.export export]] version
		  *         of a column of this mapping mirroring the given column.
		  */
		@throws[IncompatibleMappingsException]("if other is not isomorphic with this mapping.")
		@throws[NoSuchComponentException]("if column is not a column of other.")
		def counterpart[T, O](other :MappingAt[O], column :TypedColumn[T, O]) :Col[T, Origin] =
			if (column == other)
				this.asInstanceOf[Col[T, Origin]]
			else if (other == this) //explicit implicits necessary because the most generic implicit is in lexical scope
				export(column.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection))
			else if (contains(column.original.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection)))
				export(column.original.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection))
			else if ((other isomorphic refine) || (other.columns isomorphic columns))
				columns(other.columns.sureIndexOf(other.export(column))).asInstanceOf[Col[T, Origin]]
			else
				throw new IncompatibleMappingsException(
					"Cannot find a counterpart of column " + column + " from " + other + " in " + this + "."
				)

		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterparts of its components `components` in this mapping.
		  * This method is equivalent to
		  * {{{
		  *     components.map(counterpart(other, _))
		  * }}}
		  * As a often executed operation, it is provided here for convenience, but may be also somewhat
		  * faster than the above code, depending on the implementations of the `isomorphic` method
		  * used to compare the mappings.
		  */
		@throws[IncompatibleMappingsException]("if other is not isomorphic with this mapping.")
		@throws[NoSuchComponentException]("if any of the components are not a component of other.")
		def counterparts[T, O](other :TypedMapping[T, O], components :Unique[TypedMapping[_, O]])
				:Unique[Comp[_, Origin]] =
			if (other == this || contains(other.withOrigin[Origin]) || components.withOrigin[Origin].forall(contains(_)))
				components.withOrigin[Origin].map(export(_))
			else {
				val otherToThis = this.subcomponents.view.filter((c :Comp[_, Origin]) => other.contains(c.withOrigin[O]))
				                      .map(c => other.export(c.withOrigin[O]) -> c).toMap
				if (components.forall(otherToThis.contains))
					components.map(otherToThis)
				else if (original == other.original) {
					val originals = other.original.subcomponents.view.map(c => other.export(c) -> c.withOrigin[Origin]).toMap
					components.map(c => export(originals(c)))
				} else if ((other isomorphic refine) && other.subcomponents.size == subcomponents.size)
					components.map { c => subcomponents(other.subcomponents.sureIndexOf(other.export(c))) }
				else
					throw new IncompatibleMappingsException(
						"Cannot find counterparts of components " + components + " from " + other + " in " + this + "."
					)
			}

		//erasure conflict; maybe we can deal with it with magnets or type classes?
		/** Given an [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] mapping `other`,
		  * attempt to locate the counterparts of its columns `columns` in this mapping.
		  * This method is equivalent to
		  * {{{
		  *     columns.map(counterpart(other, _))
		  * }}}
		  * As a often executed operation, it is provided here for convenience, but may be also somewhat
		  * faster than the above code, depending on the implementations of the `isomorphic` method
		  * used to compare the mappings.
		  */
		@throws[IncompatibleMappingsException]("if other does not match this mapping.")
		@throws[NoSuchComponentException]("if any of the components are not a component of other.")
		def counterpartColumns[T, O](other :TypedMapping[T, O], columns :Unique[TypedColumn[_, O]])
				:Unique[Col[_, Origin]] =
			if (other == this || contains(other.withOrigin[Origin]) || columns.withOrigin[Origin].forall(contains(_)))
				columns.withOrigin[Origin].map(export(_))
			else {
				val otherToThis = this.columns.view.filter((c :Col[_, Origin]) => other.contains(c.withOrigin[O]))
				                       .map(c => other.export(c.withOrigin[O]) -> c).toMap
				if (columns.forall(otherToThis.contains))
					columns.map(otherToThis)
				else if (original == other.original) {
					val originals = other.original.columns.view.map {
						c => other.export(c) -> c.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection)
					}.toMap
					columns.map(c => export(originals(c)))
				} else if ((other isomorphic refine) || (other.columns isomorphic columns))
					columns.map { c => this.columns(other.columns.indexOf(other.export(c))) }
				else
					throw new IncompatibleMappingsException(
						"Cannot find counterparts of columns " + columns + " from " + other + " in " + this + "."
					)
			}


		/** Given a related mapping `other`, attempt to locate the counterparts of all its columns in this mapping.
		  * This is similar to
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingTemplate.counterpartColumns counterpartColumns]]`(other, other.columns)`,
		  * but the result is a map associating the columns of the argument with counterpart export columns of this mapping,
		  * and any columns of `other` which do not have a counterpart are omitted.
		  * A counterpart column is defined as a column for the same (possibly composite) property of the subjects
		  * of the two mappings, which must share a common supertype other than `Any`.
		  *
		  * Implementations may restrict themselves to arguments coming from the same mapping hierarchy, throwing
		  * an `IncompatibleMappingsException` if passed an unrelated mapping.
		  */
		@throws[IncompatibleMappingsException]("if other does not match this mapping.")
		def counterparts[T, O](other :TypedMapping[T, O]) :NaturalMap[other.Column, like[Col]#Component] = {
			def exportColumn[X](key :TypedColumn[X, O]) =
				Assoc[other.Column, like[Col]#Component, X](
					key, export(key.withOrigin[Origin](ColumnMapping.columnMappingOriginProjection))
				)
			def columnAt[X](key :TypedColumn[X, O]) =
				Assoc[other.Column, like[Col]#Component, X](
					key, this.columns(other.columns.columnIndex(other.export(key))).castParam1[X]
				)
			if (other == this || contains(other.withOrigin[Origin]) || other.columns.withOrigin[Origin].forall(contains(_)))
				other.columns.view.map(exportColumn(_)) to NaturalMap
			else if ((other isomorphic refine) || (other.columns isomorphic columns))
				other.columns.view.map(columnAt(_)) to NaturalMap
			else
				throw new IncompatibleMappingsException(
					"Cannot find counterparts of columns " + other.columns + " from " + other + " in " + this + "."
				)
		}

		/** Verifies if the given mapping is a (sub)component of this mapping. If this method returns `true`,
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] and
		  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] methods returning the export version
		  * of the component and its extract will not throw an exception. Default implementation simply checks
		  * if `component` is a key of [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] map of this mapping,
		  * but [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] yields also `true` for itself.
		  */
		def contains[T](component :Component[T]) :Boolean = extracts.contains(component)

		/** Returns an ''export'' column of this mapping under the given a name. */
		@throws[NoSuchComponentException]("if this mapping doesn't declare a direct or indirect column with the given name.")
		def columnNamed(name :String) :Col[_, Origin] //todo: UnboundParam doesn't support this method - check all usages

	}




	/** Adds factory methods for `MappingPath`s from the implicitly enriched `Mapping` instance to its components. */
	@inline implicit def mappingPathConstructor[X <: Mapping, M <: TypedMapping[S, O], S, O]
	                                           (self :X)(implicit hint :InferTypeParams[X, M, TypedMapping[S, O]])
			:MappingPathConstructor[M, S, O] =
		new MappingPathConstructor[M, S, O](self)

	/** Adds a `\` method to any `Mapping`, creating a `ComponentPath` from it to one of its components. */
	class MappingPathConstructor[M <: TypedMapping[S, O], S, O](private val self :M) extends AnyVal {

		/** Creates a `ComponentPath` leading from this (wrapped) mapping to its specified component. */
		def \[X <: Mapping, C <: TypedMapping[T, O], T]
		     (component :X)(implicit hint :InferTypeParams[X, C, TypedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component)

		def \[X <: Mapping, C <: TypedMapping[T, O], T]
		     (component :M => X)(implicit hint :InferTypeParams[X, C, TypedMapping[T, O]])
				:ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))

		def apply[X <: Mapping, C <: TypedMapping[T, O], T]
		         (component :M => X)(implicit hint :InferTypeParams[X, C, TypedMapping[T, O]])
				:ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))
	}


	implicit class MappingExtension[M <: Mapping](private val self :M) extends AnyVal {
		/** Converts this mapping to an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]]
		  * based on the ''from'' clause of its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type.
		  * The expression stands for the default column set of the mapping specific to the context of its usage,
		  * defined by an [[net.noresttherein.oldsql.OperationView OperationView]].
		  */
		def toSQL[F <: RowProduct, S](implicit origin :M <:< MappingAt[F], offset :RelationCount.In[F],
		                              projection :OriginProjection[M, S])
				:LooseComponent[F, projection.WithOrigin, S] =
			LooseComponent(self)

		/** Creates an SQL literal expression of the given value. The expression will use this mapping's column set
		  * default to the spelling scope/operation type of its usage.
		  */
		def literal[S](value :S)(implicit project :OriginProjection[M, S]) :MappingLiteral[project.WithOrigin, S] =
			MappingLiteral(self, value)

		/** Creates an SQL parameter expression of the given value. The expression will use this mapping's column set
		  * default to the spelling scope/operation type of its usage.
		  */
		def param[S](value :S)(implicit project :OriginProjection[M, S]) :BoundMappingParam[project.WithOrigin, S] =
			BoundMappingParam(self, value)
	}


	/** Adds a right-associative method `@:` to any `Mapping` with defined `Origin` and `Subject` types,
	  * which attaches a label type to it by wrapping it in `L @: M`.
	  */
	implicit class RefinedMappingExtension[M <: TypedMapping[_, _]](private val self :M) extends AnyVal {
		/** Attaches a label (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		@inline def @:[L <: Label](label :L) :L @: M = LabeledMapping[L, M, M#Subject, M#Origin](label, self)

		/** Attaches a label `L` (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		@inline def :@[L <: Label :ValueOf] :L @: M = LabeledMapping[L, M, M#Subject, M#Origin](valueOf[L], self)

//		/** Lifts this mapping by encapsulating the subject values in an `Option`. The created mapping will
//		  * always return `Some(x)` from its `optionally` and `assemble` methods, where `x` is the value returned by
//		  * the method of this instance. This means that `Some(None)` is returned when this component has no value
//		  * associated in a given `Pieces` instance, and that `apply(Pieces)` method of the returned mapping will
//		  * never fail. This instance is exposed as the public `get` field of the returned mapping, allowing direct
//		  * access to any components definitions in this mapping.
//		  */
//		@inline def asOption :Optional[M] = OptionMapping(self)
	}


	@inline implicit def MappingCollectionExtension
	                     [C[X] <: Iterable[X] with IterableOps[X, C, C[X]], T <: Mapping, M[S, A] <: TypedMapping[S, A], O]
	                     (mappings :C[T])(implicit originType :T <:< MappingAt[O], mappingType :T <:< M[_, O])
			:MappingCollectionExtension[C, M, O] =
		new MappingCollectionExtension[C, M, O](mappings.asInstanceOf[C[M[_, O]]])


	/** Extension methods for whole collections of components, allowing their comparison and origin projection.
	  * Adds a `withOrigin[O]` method to any `Iterable` subtype using mapping `M` as its element type,
	  * which substitutes their `Origin` type to type `O`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  */
	class MappingCollectionExtension[C[X] <: Iterable[X] with IterableOps[X, C, C[X]], M[X, O] <: TypedMapping[X, O], A] private[Mapping]
	                                (private val self :C[M[_, A]])
		extends AnyVal
	{
		/** Converts the element mappings to `Mapping` type with type `O` as its `Origin` type.
		  *  This is a simple cast changing the `Origin` phantom member type.
		  */ //consider: rename to ofOrigin/fromOrigin/from/asFrom
		@inline def withOrigin[O] :C[M[_, O]] = self.asInstanceOf[C[M[_, O]]]

		/** Maps all these components to their counterparts in another mapping.
		  * @param owner a mapping to which these components belong.
		  * @param other a mapping [[net.noresttherein.oldsql.schema.Mapping.isomorphic isomorphic]] with `owner`.
		  */
		def counterparts[O](owner :MappingAt[A], other :MappingAt[O]) :C[TypedMapping[_, O]] =
			if (!(owner isomorphic other))
				throw new IllegalArgumentException("Mapping " + other + " is not isomorphic with mapping " + owner + ".")
			else if (!self.forall(owner.contains(_)))
				throw new IncompatibleMappingsException(
					"Components " + self.filterNot(owner.contains(_)) + " do not belong to mapping " + owner + "."
				)
			else
				self.map(other.counterpart(owner, _))

		/** Finds the position of the given component in this collection (assuming it has a fixed iterations order).
		  * Similar to `Seq.indexOf`, but instead of returning `-1` if the component is not found,
		  * it throws a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]].
		  */
		private[oldsql] def componentIndex(component :MappingAt[A]) :Int = {
			val idx = self match {
				case seq :collection.Seq[M[_, A]] => seq.indexOf(component)
				case unique :Unique[M[_, A]] => unique.indexOf(component)
				case _ if self.isEmpty => -1
				case _ =>
					var i = 0; val iter = self.iterator
					while (iter.hasNext) {
						i += 1; iter.next()
					}
					i
			}
			if (idx >= 0) idx
			else throw new NoSuchComponentException(
				"Component " + component + " is not present among " + self + "; is it an export version for the same mapping?"
			)
		}
	}



	@inline implicit def MappingOriginProjector[M <: Mapping, S](mapping :M)(implicit evidence :OriginProjection[M, S])
			:MappingOriginProjector[M, S] =
		new MappingOriginProjector[M, S](mapping)

	/** Adds a `withOrigin[O]` method to any `Mapping` subtype, which substitutes its `Origin` type to type `O`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  */
	class MappingOriginProjector[M <: Mapping, S] private[Mapping] (private val self :M) extends AnyVal {

		/** Converts this mapping to one where `type Origin = O`. The result type is determined by the implicit
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]].
		  * If `M <: T[A1, ..., AN, A]` where `T[_, ..., _, O] <: BaseMapping[_, O]`, then the result type will
		  * be inferred as `T[A1, ..., AN, O]`. If the origin type parameter `A` is not the last one, or the mapping
		  * defines its `Origin` type by other means entirely, type inference will fail and an implicit
		  * `OriginProjection` value with the correct `WithOrigin` type should be provided in the companion object to `M`.
		  * See the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] class documentation
		  * for a detailed information about providing custom projections and cases when it is required.
		  */ //consider: rename to ofOrigin/from;
		//todo: see if it will work as a method in Mapping, with OriginProjection[this.type, Subject]
		@inline def withOrigin[O](implicit projection :OriginProjection[M, S]) :projection.WithOrigin[O] =
			self.asInstanceOf[projection.WithOrigin[O]]

		/** Upcasts this instance to a type `P[M#Origin] <: TypedMapping[M#Subject, M#Origin]` such that
		  * the type constructor `P` substitutes all occurrences of the `Origin` type in `M`'s type signature
		  * with its type parameter. This is equivalent to simply calling
		  * [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]]`[O]` -
		  * a useful alternative when the projection invariant mapping type is required without changing its origin,
		  * as concrete `Origin` types are typically long.
		  */
		@inline def selfProjection(implicit projection :OriginProjection[M, S]) :projection.WithOrigin[M#Origin] =
			self.asInstanceOf[projection.WithOrigin[M#Origin]]
	}


	/** Converts a mapping `M` with `type Origin = A forSome { type A }` to a mapping with `type Origin = B`, where `B`
	  * is the type parameter of the `apply` method. On the value level, it is always a no-op cast
	  * of the argument mapping. On the type level, it must be complete in the sense that, in the projected
	  * mapping type `WithOrigin[B]`, all references to `A` are replaced with the type `B`. If the mapping type `M`
	  * is a generic type which defines its `Origin` type to be equal to its last type parameter `O` (typically
	  * by extending `BaseMapping[S, O]`) and `O` doesn't occur anywhere else in the definition of `M`,
	  * as is the recommended practice, then `M =:= WithOrigin[A]` and type `WithOrigin` is inferred automatically
	  * by the compiler with partial unification. An implicit value of this class is in that case provided by the
	  * `BaseMapping` object. There are cases however when either the appropriate projection type cannot be inferred
	  * or is inferred incorrectly. Typical examples of the former include `Mapping` subclasses with their `Origin`
	  * type parameter not in the last position, or types which define their `Origin` type in terms of the `Origin` type
	  * of some value, such as the labeled mapping wrapper `N @: M`, which defines its origin type to be equal to the
	  * `Origin` of the adapted mapping. An example of the latter would be a class which requires that the `Origin` type
	  * argument occurs elsewhere in `M` for the value to be valid, such as:
	  * {{{
	  *     abstract class Adapter[+M <: Mapping, S, O](val component :M) extends BaseMapping[S, O]
	  *
	  *     val a :Adapter[BaseMapping[Int, "A"], Int, "A"] = ???
	  *     //a.component is a valid component of a
	  *     val b = a.withOrigin["B"] //b :Adapter[BaseMapping[Int, "A"], Int, "B"]
	  *     //b.component is not a valid component of b as it has a different `Origin` type.
	  * }}}
	  * Such practice is discouraged, but not prohibited as it can lead to shorter type signatures.
	  * Both situations can be amended by defining an implicit `ExactProjection[M]` in the companion object
	  * of the class of `M`. Fixing the above example would require:
	  * {{{
	  *     object Adapter {
	  *         implicit def projection[M <: Mapping, X, O](implicit m :ExactProjection[M, _])
	  *                 :ProjectionDef[Adapter[M, X, O], ({ type P[A] = Adapter[m.WithOrigin[A], X, A], X, A] })#P, X] =
	  *             m.map[({ type L[+T <: Mapping, A] = Adapter[T, X, A] })#L, M]
	  *     }
	  *
	  *     val c = a.withOrigin["C"] //c :Adapter[BaseMapping[Int, "C"], Int, "C"]
	  * }}}
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection ExactProjection]]`[M]`
	  * is the implementation subtype of `OriginProjection`, invariant in its type parameter; every instance
	  * of `OriginProjection[M, _]` is also an instance of `ExactProjection[_ <: M]`.
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ProjectionDef ProjectionDef]] is a type alias
	  * which shortens the type signature and causes an implicit `ExactProjection[M]` to be picked up as the value
	  * for an implicit parameter of `OriginProjection[M, M#Subject]` (which it would not by default).
	  *
	  * Implicit values for `OriginProjection` should generally be provided in the form of an
	  * `ExactProjection`/`ProjectionDef`. For mapping types which do not have other mapping types in their
	  * type signature, the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.isomorphism isomorphism]] method
	  * of the companion object will be generally sufficient. Adapters of other mappings, such as in the earlier example,
	  * should use one of the transformation methods defined in the `ExactProjection`, if possible, with custom
	  * instances being created only as a last resort, as offering no type safety.
	  * @tparam M the type of the mapping being projected. In practice, `M <: MappingOf[S]` always holds,
	  *           but the constraint is removed here to simplify type signatures of methods accepting
	  *           an `OriginProjection` as a parameter.
	  * @tparam S the `Subject` type of the mapping `M`, preserved by the projected mapping `WithOrigin[_]`.
	  */ //todo: rename to OriginTransfer. projection is better for OperationView
	@implicitNotFound("Cannot project mapping ${M} to another Origin type: " +
		              "no implicit OriginProjection[${M}, ${S}].")
	sealed trait OriginProjection[-M <: Mapping, S] extends Serializable { self =>

		/** A type such that `M <: WithOrigin[_]` which does not reference the origin type `O` in its signature
		  * anywhere except as the type parameter. In other words, a conversion `WithOrigin[A] => WithOrigin[B]`
		  * replaces every reference to `A` with the type `B`. This in particular means that all components and extracts
		  * returned by the mapping after conversion define their `Origin` type as `B`, consistently with the converted
		  * mapping.
		  */ //this is BaseMapping bound as it is required by MappingSQL hierarchy due to a compiler bug.
		type WithOrigin[O] <: BaseMapping[S, O] //consider: renaming to OfOrigin/fromOrigin/from/AsFrom
//
//		/** A type constructor for an adapter to the projected mapping. This type is not directly related
//		  * to this class's functionality, but instead is a convenience curried constructor to be used
//		  * through Scala type projection: `OriginProjection[M, S]#Adapter`.
//		  */
//		type Adapter[O] = MappingAdapter[M, S, O]

		/** Casts the mapping of type `M` to a type where all references to its current origin type are replaced
		  * by the `O` type.
		  */
		@inline final def apply[O](mapping :M) :WithOrigin[O] = mapping.asInstanceOf[WithOrigin[O]]

		/** A projection from any `WithOrigin[O]` to `WithOrigin[X]`. */
		@inline implicit def isomorphism[O] :IsomorphicProjection[WithOrigin, S, O] =
			this.asInstanceOf[IsomorphicProjection[WithOrigin, S, O]]
//				:ExactProjection[WithOrigin[O]] { type WithOrigin[A] = OriginProjection.this.WithOrigin[A] } = //:IsomorphicProjection[WithOrigin, S, O] =
//			this.asInstanceOf[ExactProjection[WithOrigin[O]] { type WithOrigin[A] = OriginProjection.this.WithOrigin[A] }]

		/** A projection to an upper bound of this projection. */
		@inline def superProjection[U[O] >: WithOrigin[O] <: BaseMapping[S, O]]
				:OriginProjection[M, S] { type WithOrigin[O] = U[O] } =
			this.asInstanceOf[ArbitraryProjection[M, U, S]]

		/** Lifts a projection of a mapping type `M` to one casting from mapping `A[M, _]` to `A[WithOrigin[O], O]`.
		  * It is the responsibility of the caller to make sure that type `A` does not reference directly
		  * the origin type in its definition. This method is suitable for mappings with `Subject` type
		  * different from the subject of the adapted mapping.
		  * @tparam C the type of mapping being projected, a subtype of `M`.
		  * @tparam A type constructor of the adapter mapping for which the projection is being created,
		  *           accepting the adapted mapping type and the `Origin` type as type parameters.
		  * @tparam T the subject type of the argument mapping of the created projection.
		  */
		@inline def mapCo[C <: M, A[+B <: Mapping, Q] <: BaseMapping[T, Q], T]
				:ExactProjection[A[C, C#Origin]] { type WithOrigin[O] = A[self.WithOrigin[O], O] } =
			this.asInstanceOf[ExactProjection[A[C, C#Origin]] { type WithOrigin[O] = A[self.WithOrigin[O], O] }]

		/** Lifts a projection of a mapping type `M` to one casting from mapping `A[M, X, _]` to `A[WithOrigin[O], X, O]`.
		  * This is equivalent to the `mapCo` method, differing only in the adapter type constructor `A` accepting
		  * the subject type of the mapping as the type parameter, which matches the type signature of
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]].
		  * It is the responsibility of the caller to make sure that type `A` does not reference directly
		  * the origin type in its definition. This method is suitable for mappings with `Subject` type
		  * different from the subject of the adapted mapping.
		  * @tparam C the type of mapping being projected, a subtype of `M`.
		  * @tparam A type constructor of the adapter mapping for which the projection is being created,
		  *           accepting the adapted mapping type and the `Subject`/`Origin` type pair as type parameters.
		  * @tparam T the subject type of the argument mapping of the created projection.
		  * @tparam O the origin type of the argument mapping of the created projection.
		  */
		@inline def adaptCo[C <: M, A[+B <: Mapping, X, Q] <: BaseMapping[X, Q], T, O]
				:ExactProjection[A[C, T, O]] { type WithOrigin[X] = A[self.WithOrigin[X], T, X] } =
			this.asInstanceOf[ExactProjection[A[C, T, O]] { type WithOrigin[X] = A[self.WithOrigin[X], T, X] }]
	}



	object OriginProjection {

		/** Summon an implicit instance of `OriginProjection[M, S].` */
		@inline def apply[M <: Mapping, S](implicit projection :OriginProjection[M, S]) :projection.type =
			projection

		/** Summon an implicit instance of `ExactProjection[M]`. */
		@inline def apply[M <: Mapping](implicit projection :ExactProjection[M]) :projection.type =
			projection


		/** An subtype of `OriginProjection` invariant in the projected mapping type. It serves as the implementation
		  * interface of `OriginProjection`: mapping classes providing their own implicit projections should
		  * declare them as this type rather the base type. The reason behind it is that the factory methods
		  * for derived projections of higher mapping kinds defined here do not require
		  * the adapter mapping type to be covariant in its adapted mapping type parameter.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ProjectionDef]]
		  */
		@implicitNotFound("Cannot project mapping ${M} to another Origin type: no (unique) implicit ExactProjection[${M}]")
		trait ExactProjection[M <: Mapping] extends OriginProjection[M, M#Subject] { self =>

			/** Lifts a projection of mapping type `M` to one casting from mapping `A[M]` to `A[WithOrigin[O]]`.
			  * This method is an applicable projection source for `Mapping` types serving as proxies to their type
			  * argument, having the same `Subject` and `Origin` types.  It is the responsibility of the caller
			  * to make sure that type `A` does not reference directly the origin type in its definition.
			  * @tparam A type constructor of the adapter mapping, accepting the mapping `M` as its type parameter.
			  */
			@inline def lift[A[B <: Mapping] <: BaseMapping[B#Subject, B#Origin]]
					:ExactProjection[A[M]] { type WithOrigin[O] = A[self.WithOrigin[O]] } =
				this.asInstanceOf[ExactProjection[A[M]] { type WithOrigin[O] = A[self.WithOrigin[O]] }]

			/** Lifts a projection of a mapping type `M` to one casting from mapping `A[M, _]` to `A[WithOrigin[O], O]`.
			  * It is the responsibility of the caller to make sure that type `A` does not reference directly
			  * the origin type in its definition. This method is suitable for mappings with `Subject` type
			  * different from the subject of the adapted mapping.
			  * @tparam A type constructor of the adapter mapping for which the projection is being created,
			  *           accepting the adapted mapping type and the `Origin` type as type parameters.
			  * @tparam S the subject type of the argument mapping of the created projection.
			  */
			@inline def map[A[B <: Mapping, Q] <: BaseMapping[S, Q], S]
					:ExactProjection[A[M, M#Origin]] { type WithOrigin[O] = A[self.WithOrigin[O], O] } =
				this.asInstanceOf[ExactProjection[A[M, M#Origin]] { type WithOrigin[O] = A[self.WithOrigin[O], O] }]

			/** Lifts a projection of a mapping type `M` to one casting from mapping `A[M, X, _]` to `A[WithOrigin[O], X, O]`.
			  * This is equivalent to the `mapCo` method, differing only in the adapter type constructor `A` accepting
			  * the subject type of the mapping as the type parameter, which matches the type signature of
			  * [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]].
			  * It is the responsibility of the caller to make sure that type `A` does not reference directly
			  * the origin type in its definition. This method is suitable for mappings with `Subject` type
			  * different from the subject of the adapted mapping.
			  * @tparam A type constructor of the adapter mapping for which the projection is being created,
			  *           accepting the adapted mapping type and the `Subject`/`Origin` type pair as type parameters.
			  * @tparam S the subject type of the argument mapping of the created projection.
			  * @tparam O the origin type of the argument mapping of the created projection
			  */
			@inline def adapt[A[B <: Mapping, T, Q] <: BaseMapping[T, Q], S, O]
					:ExactProjection[A[M, S, O]] { type WithOrigin[X] = A[self.WithOrigin[X], S, X] } =
				this.asInstanceOf[ExactProjection[A[M, S, O]] { type WithOrigin[X] = A[self.WithOrigin[X], S, X] }]
		}


		/** A straightforward alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
		  * introducing an upper bound `WithOrigin[X] <: U[X]`.
		  */
		type ProjectionBound[-M <: Mapping, +U[A] <: BaseMapping[S, A], S, O] =
			OriginProjection[M, S] { type WithOrigin[X] <: U[X] }

		/** A straightforward alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
		  * lifting the member type `WithOrigin` to the type parameter `P`. This (the 'Aux' pattern) allows the type
		  * to be used by other parameters of a method accepting this type, as well as shortens the notations somewhat.
		  */
		type ArbitraryProjection[-M <: Mapping, P[O] <: BaseMapping[S, O], S] =
			OriginProjection[M, S] { type WithOrigin[X] = P[X] }

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection ExactProjection]]
		  * of single-argument functor types, accepting their `Origin` type as their argument. This shortens
		  * the notation considerably, especially if `M` is a natural single-argument type constructor
		  * (and not a type lambda).
		  */
		type IsomorphicProjection[M[X] <: BaseMapping[S, X], S, O] = ProjectionDef[M[O], M, S]
//			ExactProjection[M[O]] { type WithOrigin[X] = M[X] }

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection ExactProjection]]
		  * of the same kind as [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]], that is accepting
		  * two invariant type parameters: their `Subject` and `Origin` types. This shortens the notation considerably,
		  * especially if `M` is a natural type constructor (and not a type lambda). */
		type TypedProjection[M[X, Y] <: BaseMapping[X, Y], S, O] =
			ExactProjection[M[S, O]] { type WithOrigin[X] = M[S, X] }

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection ExactProjection]]
		  * which accepts its `WithOrigin` type as the second argument `P`. This shortens the notation considerably
		  * if `P` is a natural single-argument type constructor (and not a type lambda).
		  */
		type ProjectsAs[M <: Mapping, P[O] <: BaseMapping[M#Subject, O]] =
			ExactProjection[M] { type WithOrigin[O] = P[O] }


		/** A somewhat superfluous type alias explicitly combining
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection.ExactProjection ExactProjection]]`[M]`
		  * with its supertype [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]`[M, S]`.
		  * This is because implicit declarations of the former are not picked up as values for implicit parameters
		  * of the latter. For this reason, this is the default type of all implicit values for `OriginProjection`.
		  */
		type ProjectionDef[M <: Mapping, P[Q] <: BaseMapping[S, Q], S] =
			OriginProjection[M, S] with ExactProjection[M] { type WithOrigin[X] = P[X] }

		/** A curried type constructor for refining an arbitrary `Mapping` subtype with a definition
		  * of its `Origin` type.
		  * @see [[net.noresttherein.oldsql.schema.Mapping.AsFrom]]
		  */
		type of[M <: Mapping] = { type at[O] = M AsFrom O }



		/** A natural projection of a `BaseMapping` subtype made by providing a different `Origin` type
		  * to its type constructor.
		  * @tparam M a type constructor for a `BaseMapping` subtype which sets all references to the `Origin` type
		  *           to its type parameter.
		  * @tparam S the subject type of the projected mapping.
		  * @tparam O the input origin typ of the projected mapping.
		  */
		def isomorphism[M[A] <: BaseMapping[S, A], S, O] :IsomorphicProjection[M, S, O] =
			CastingProjection.asInstanceOf[IsomorphicProjection[M, S, O]]
//		def isomorphism[M[A] <: BaseMapping[S, A], S, O] :ProjectionDef[M[O], M, S] =
//			CastingProjection.asInstanceOf[ProjectionDef[M[O], M, S]]

		def typed[M[X, Y] <: BaseMapping[X, Y], S, O] :TypedProjection[M, S, O] =
			CastingProjection.asInstanceOf[TypedProjection[M, S, O]]

		private[oldsql] def projectAs[M <: Mapping, P[O] <: BaseMapping[M#Subject, O]]
				:ExactProjection[M] { type WithOrigin[O] = P[O] } =
			CastingProjection.asInstanceOf[ExactProjection[M] { type WithOrigin[O] = P[O] }]

		private[oldsql] def define[M <: Mapping, P[O] <: BaseMapping[S, O], S] :ProjectionDef[M, P, S] =
			CastingProjection.asInstanceOf[ProjectionDef[M, P, S]]


		private[this] final val CastingProjection = new ExactProjection[MappingOf[Any]] {
			override type WithOrigin[O] = BaseMapping[Any, O]
		}
	}



	//this is here to have a clear precedence hierarchy with Mapping subtypes declaring their own projections
	@inline implicit def mappingOriginProjection[S]
			:OriginProjection[MappingOf[S], S] with ExactProjection[MappingOf[S]] { type WithOrigin[O] = BaseMapping[S, O] } =
		OriginProjection.projectAs[MappingOf[S], MappingOf[S]#TypedProjection]






	/** A thin wrapper over a component of some mapping `MappingAt[O]` which selects it to be included or
	  * excluded from some database operation, depending on which of the two subclasses of this type is the class
	  * of this object. Instances of this trait are typically created by calling either
	  * [[net.noresttherein.oldsql.schema.Mapping.+ +]] or [[net.noresttherein.oldsql.schema.Mapping.- -]]
	  * on a component of the enclosing mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent]]
	  */
	sealed trait ComponentSelection[T, O] extends Serializable {
		val component :TypedMapping[T, O]
	}

	/** Marks the wrapped component for being included in some database operation.
	  * Each mapping has a factory method wrapping it in this class: [[net.noresttherein.oldsql.schema.Mapping.+ +]].
	  */
	case class IncludedComponent[T, O](component :TypedMapping[T, O]) extends ComponentSelection[T, O]

	/** Marks the wrapped component for being excluded from some database operation.
	  * Each mapping has a factory method wrapping it in this class: [[net.noresttherein.oldsql.schema.Mapping.- -]].
	  */
	case class ExcludedComponent[T, O](component :TypedMapping[T, O]) extends ComponentSelection[T, O]




	//todo: move to forms
	//todo: non-aliasing form, on demand form and implementations with different ComponentValues in general
	private[oldsql] class MappingReadForm[S, O] private[oldsql] //consider: making it public and using in SQLExpression for the columns
	                      (protected final val root :MappingAt[O],
	                       protected final val mapping :TypedMapping[S, O],
	                       protected final val columns :Unique[TypedColumn[_, O]],
	                       protected final val read :TypedColumn[_, O] => SQLReadForm[_])
		extends SQLReadForm[S] //could extend CompositeReadForm, but doesn't for extra speed
	{   //todo: we must use also ExtraSelect and CustomSelect columns here
		def this(mapping :TypedMapping[S, O], columns :Unique[TypedColumn[_, O]],
		         read :TypedColumn[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm) =
			this(mapping, mapping, columns, read)

		private[this] val forms = columns.view.map(read).to(Array)
		override val columnCount: Int = (0 /: forms)(_ + _.columnCount)
		override val columnTypes = ArraySeq.unsafeWrapArray(forms).flatMap(_.columnTypes) //remember the form can have a zero width
		override val shape = RowShape(columnTypes)

		override def opt(res: ResultSet, position: Int) :Opt[S] = {
			val columnValues = new Array[Any](forms.length)
			var i = forms.length - 1
			while (i >= 0) {
				columnValues(i) = forms(i).opt(res, position + i).orNull
				i -= 1
			}
			val entity = root.refine
			if (root eq mapping) { //maybe we should mandate that pieces.get works for the associated mapping, too?
				val pieces = ColumnValues(mapping)(ArraySeq.unsafeWrapArray(columnValues))(columns.indexOf)
				mapping.optionally(pieces)
			} else {
				val pieces = ColumnValues(entity)(ArraySeq.unsafeWrapArray(columnValues))(columns.indexOf)
				pieces.get(mapping)
			}
		}

		override def nullValue: S = mapping.nullValue.value
		override def nulls :NullValue[S] = mapping.nullValue

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = 0; var c = 0; val end = forms.length
			while (c < end) {
				val form = forms(i)
				form.register(call, i)
				i += form.columnCount; c += 1
			}
		}

		override def reorder(permutation :IndexedSeq[Int]) :SQLReadForm[S] = {
			val reorderedColumns = (0 until columns.size).view.map(i => columns(permutation(i))) to Unique
			new MappingReadForm[S, O](root, mapping, reorderedColumns, read)
		}

		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :MappingReadForm[_, _] =>
				columns.toSeq.view.map(_.form.sqlType) == other.columns.toSeq.view.map(_.form.sqlType)
			case _ => super.comparable(other)
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :MappingReadForm[_, _] if other canEqual this =>
				(mapping identical other.mapping) && (columns identical other.columns) &&
					columns.view.map(read) == other.columns.view.map(other.read)
			case _ => false
		}
		override def hashCode :Int = ArraySeq.unsafeWrapArray(forms).hashCode

		private def mappingString = mapping.mappingName
		//todo: should be lazy
		override def toString :String = columns.map(read).mkString(s"$mappingString{", ",", "}>")
	}




	object MappingReadForm {
		//fixme: we cannot reorder columns here.
		//todo: we should allow including Extra columns: it is the only way to create forms for mappings
		//  of aligned component expressions
		def apply[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O], columns :Unique[TypedColumn[_, O]],
		                forms :TypedColumn[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
				:SQLReadForm[S] =
		{
			if (!columns.forall(mapping.contains(_)))
				throw new IllegalArgumentException(
					columns.mkString("Cannot create a read form for " + mapping + " containing columns ", ", ",
						             columns.filterNot(mapping.contains(_)).mkString(
							             " because column(s) ", ", ", " do not belong to this component."
						             )
					)
				)
				new MappingReadForm[S, O](entity, mapping, columns, forms)
		}

		def apply[S, O](mapping :TypedMapping[S, O], columns :Unique[TypedColumn[_, O]],
		                forms :TypedColumn[_, O] => SQLReadForm[_]) :SQLReadForm[S] =
			new MappingReadForm[S, O](mapping, columns, forms)


		def select[S](mapping :MappingOf[S]) :SQLReadForm[S] = {
			val columns = mapping.selectedByDefault ++ SelectPreset.Active.columns(mapping)
			new MappingReadForm[S, mapping.Origin](mapping, columns, _.selectForm)
		}

		def select[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLReadForm[S] = {
			val exports = components.map(mapping.export(_))
			if (exports.exists(NoSelect.active))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
			val columns = exports.flatMap(mapping.selectedByDefault(_))
			val mandatory = mapping.selectable.filter(OptionalSelect.inactive(_))
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a select form for $mapping using $components: missing mandatory columns ", ", ", "."
				))
			//add a value for SelectPreset manually because column.optionally doesn't check for it
			val extra = columns ++ SelectPreset.Active.columns(mapping)
			new MappingReadForm[S, O](mapping, extra, _.selectForm)
		}

		def fullSelect[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](
				mapping, mapping.selectable ++ SelectPreset.Active.columns(mapping)
			)

		def select[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLReadForm[S] = {
			val columns = root.selectedByDefault(component)
			val extras = component.columns.map(root.export(_)).filter(SelectPreset.active)
			new MappingReadForm[S, O](root, component, columns ++ extras, _.selectForm)
		}

		def select[S, O](root :MappingAt[O], component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLReadForm[S] =
		{
			val export = root.export(component)
			if (NoSelect.active(export))
				throw new IllegalArgumentException(
					"Cannot select component " + component + " with subcomponents " + subcomponents +
					" because the component is non-selectable in " + root + " (as " + export + ")."
				)
			val selectable = root.selectable(component)
			val selected = subcomponents.flatMap { sub =>
				val export = root.export(sub)
				if (NoSelect.active(export))
					throw new IllegalArgumentException(
						"Cannot select component " + component + " of " + root + " with subcomponents " + subcomponents +
						" because " + sub + " (as " + export + ") is non-selectable."
					)
				root.selectedByDefault(export)
			}
			val missing = selectable.find { col => !selected.contains(col) && OptionalSelect.inactive(col) }
			missing foreach { col =>
				throw new IllegalArgumentException(
					"Cannot select component " + component + " of " + root + " with subcomponents " + subcomponents +
					" because a mandatory column " + col + " is missing from the specified list."
				)
			}
			val presets = component.columns.map(root.export(_)).filter(SelectPreset.active)
			new MappingReadForm[S, O](root, export, selected ++ presets, _.selectForm)
		}


		def autoInsert[S](mapping :MappingOf[S], subject :S) :SQLReadForm[S] =
			new MappingReturnForm[S, mapping.Origin](mapping, subject, mapping.autoInserted)

		def autoUpdate[S](mapping :MappingOf[S], subject :S) :SQLReadForm[S] =
			new MappingReturnForm[S, mapping.Origin](mapping, subject, mapping.autoUpdated)

		private class MappingReturnForm[S, O] private[schema] //consider: making it public and using in SQLExpression for the columns
		                               (parent :TypedMapping[S, O], original :S, updated :Unique[TypedColumn[_, O]],
		                                form :TypedColumn[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
			extends MappingReadForm[S, O](parent, updated, form)
		{
			private[this] val forms = columns.view.map(read).to(Array)
			private[this] val readColumnCount = columns.size

			override def opt(res :ResultSet, position :Int) :Opt[S]  = {
				val columnValues = new Array[Any](readColumnCount)
				var i = readColumnCount - 1
				while (i >= 0) {
					columnValues(i) = forms(i).opt(res, position + i).orNull
					i -= 1
				}
				val pieces = ColumnValues(mapping)(ArraySeq.unsafeWrapArray(columnValues))(columns.indexOf)
				mapping.optionally(pieces orElse ColumnValues(mapping)(original))
			}

			private def entity = original

			override def canEqual(that :Any) :Boolean = that match {
				case self :AnyRef if this eq self => true
				case other :MappingReturnForm[_, _] => entity == other.entity
				case _ => false
			}
		}
	}






	private[schema] class MappingWriteForm[S, O] private[schema]
	                      (private val op :WriteOperationView, private val root :MappingAt[O],
	                       private val mapping :TypedMapping[S, O], private val columns :Unique[TypedColumn[_, O]])
		extends SQLWriteForm[S] with WriteFormOptLiterals[S]
	{ //todo: non-nullable columns should throw an early exception and not leave it to the database.
		def this(op :WriteOperationView, mapping :TypedMapping[S, O], columns :Unique[TypedColumn[_, O]]) =
			this(op, mapping, mapping, columns)
		//fixme: buff handling: should we use op.form(column) or column.form?
		private[this] val fasterColumns = columns.toArray //speeds up the iteration
		private[this] val forms = fasterColumns.map(op.form(_).castParam[Any]) //fasterColumns.map(op.form(_))
		//or we could prohibit NoSelect in columns and add presets ourselves.
		private[this] val presets = fasterColumns.map(op.Preset.get(_).orNull)
		private[this] val extracts = //extracts for the columns for fast access
			if (root eq mapping)
				fasterColumns.map(mapping(_))
			else {
				val allColumns = root.columns
				val mappingColumns = mapping.columns
				val mappingColumnIndices = mappingColumns.map(c => allColumns.sureIndexOf(root.export(c)))
				fasterColumns.map { col =>
					val mappingColumn = mappingColumns(mappingColumnIndices.sureIndexOf(allColumns.sureIndexOf(col)))
					mapping(mappingColumn)
				}
			}

		override val columnCount: Int = (0 /: forms)(_ + _.columnCount)
		override val columnTypes :Seq[JDBCType] = ArraySeq.unsafeWrapArray(forms).flatMap(_.columnTypes)
		override val shape :RowShape = RowShape(columnTypes)

		override lazy val split = {
			val result = new Array[ColumnWriteForm[S]](columnCount)
			var i = result.length
			var j = result.length
			while (i > 0) {
				i -= 1
				forms(i) match {
					case columnForm :ColumnWriteForm[Any] =>
						j -= 1
						result(j) = columnForm compose extracts(i)
					case _ =>
				}
			}
			ArraySeq.unsafeWrapArray(result)
		}

		override def set(statement :PreparedStatement, position :Int, subject :S) :Unit =
			if (subject == null)
				setNull(statement, position)
			else {
//				val subject = value.asInstanceOf[mapping.Subject]
				val values = mapping.writtenValues(op, subject)
				var i = 0; val len = fasterColumns.length
				var offset = position
				while (i < len) {
					val column = fasterColumns(i)//.asInstanceOf[TypedColumn[Any, O]]
					val form = forms(i)
					val value = presets(i) match {
						case null => values get column
						case buff => Got(buff.value)
					}
					form.setOpt(statement, offset, value)
					offset += form.columnCount
					i += 1
				}
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = 0; val len = forms.length
			var offset = position
			while (i < len) {
				val form = forms(i)
				form.setNull(statement, offset)
				offset += form.columnCount
				i += 1
			}
		}

		override def optLiteral(value :Opt[S], inline :Boolean): String = {
//			val subject = value.asInstanceOf[mapping.Subject]
			val res = new java.lang.StringBuilder
			if (!inline)
				res append '('
			val values = value match {
				case Got(v) => mapping.writtenValues(op, v)
				case _ => ComponentValues[S, O]()
			}
			var i = 0; val len = forms.length
			while (i < len) {
				if (i > 0)
					res append ',' append ' '
				val form = forms(i).castParam[Any]
				res append (values.get(fasterColumns(i)) match {
					case Got(literal) =>
						if (literal == null) form.nullLiteral(inline)
						else form.literal(literal, inline)
					case _ => form.nullLiteral(inline)
				})
				i += 1
			}
			if (!inline)
				res append ')'
			res.toString
		}

		override def optColumnLiterals(value :Opt[S]) :Seq[String] = {
			val res = new Array[String](forms.length)
			val values = value match {
				case Got(v) => mapping.writtenValues(op, v)
				case _ => ComponentValues[S, O]()
			}
			var i = forms.length
			while (i > 0) {
				i -= 1
				val form = forms(i).castParam[Any]
				val literal = values.get(fasterColumns(i)) match {
					case Got(v) =>
						if (v == null) form.nullLiteral
						else form.literal(v)
					case _ => form.nullLiteral
				}
				res(i) = literal
			}
			ArraySeq.unsafeWrapArray(res)
		}

		override def reorder(permutation :IndexedSeq[Int]) :SQLWriteForm[S] = {
			val reorderedColumns = (0 until columns.size).map(i => columns(permutation(i))) to Unique
			new MappingWriteForm[S, O](op, root, mapping, reorderedColumns)
		}

		override def comparable(other :SQLWriteForm[_]) :Boolean = other match {
			case _ if this eq other => true
			case other :MappingWriteForm[_, _] =>
				columnCount == other.columnCount &&
					(columns.toSeq.view.map(_.form.sqlType) == other.columns.toSeq.view.map(_.form.sqlType) ||
						super.comparable(other))
			case _ => super.comparable(other)
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :MappingWriteForm[_, _] if form canEqual this =>
				form.mapping == mapping && form.columns == columns && form.op == op
			case _ => false
		}
		override def hashCode :Int = (op.hashCode * 31 + mapping.hashCode) * 31 + columns.hashCode

		private def mappingString = mapping.mappingName

		override lazy val toString :String = columns.map(op.form(_)).mkString(s"<$mappingString{", ",", "}")
	}





	object MappingWriteForm {
		//Fixme: we cannot reorder columns because it will mess with reforming.
		// We should also allow all Extra and similar columns here, because they need to be taken into account when reforming.
		//todo: document, mention that columns will not appear in the same order as in the argument list,
		// but instead use a normalized order specific to the entity mapping

		def apply[S, O](op :WriteOperationView, mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
			default(op, mapping)

		def apply[S, O](op :WriteOperationView, mapping :TypedMapping[S, O], components :Unique[MappingAt[O]])
				:SQLWriteForm[S] =
			custom[S, O](op, mapping, components)//, op.Prohibited, op.NonDefault, op.Preset, op.form(_))

		def apply[S, O](op :WriteOperationView, entity :MappingAt[O], mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
			component[S, O](op, entity, mapping)

		def apply[S, O](op :WriteOperationView, entity :MappingAt[O], mapping :TypedMapping[S, O],
		                subcomponents :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
			component[S, O](op, entity, mapping, subcomponents)


		def filter[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(FilterView, mapping)

		def filter[S, O](mapping :TypedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			apply[S, O](FilterView, mapping, components)

		def fullFilter[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(FilterView, mapping)

		def filter[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
			component(FilterView, entity, mapping)

		def filter[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			component(FilterView, entity, mapping, subcomponents)


		def insert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(InsertView, mapping)

		def insert[S, O](mapping :TypedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			custom(InsertView, mapping, components)

		def fullInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(InsertView, mapping)

		def insert[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
			component(InsertView, entity, mapping)

		def insert[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			component(InsertView, entity, mapping, subcomponents)


		def update[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(UpdateView, mapping)

		def update[S, O](mapping :TypedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			custom(UpdateView, mapping, components)

		def fullUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(UpdateView, mapping)

		def update[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O]) :SQLWriteForm[S] =
			component(UpdateView, entity, mapping)

		def update[S, O](entity :MappingAt[O], mapping :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			component(UpdateView, entity, mapping, subcomponents)


		private def custom[S, O](op :WriteOperationView,
		                         mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
		{ //fixme: hey, this should use Mapping.writtenValues!
			val exports = components.view.map(mapping.export(_))
			if (exports.exists(op.Prohibited.active))
				throw new IllegalArgumentException(
					s"Can't create a $op write form for $mapping using $exports: ${op.Prohibited} buff present among selection."
				)
			val columns = exports.flatMap { mapping.defaultColumns(op, _) }.to(Unique)
			val mandatory = mapping.columnsWithout(op.Optional)
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a $op write form for $mapping using $exports: missing mandatory columns ", ", ", "."
				))

			val extras = columns ++ op.Preset.Active.columns(mapping)
			//fixme: inconsistent with op.defaultColumns(mapping, components)!
			new MappingWriteForm[S, O](op, mapping, extras)
		}

		private def default[S](op :WriteOperationView, mapping :MappingOf[S]) :SQLWriteForm[S] = {
			val columns = mapping.defaultColumns(op) ++ mapping.columnsWith(op.Preset)
			new MappingWriteForm[S, mapping.Origin](op, mapping, columns)
		}

		private def full[S](op :WriteOperationView, mapping :MappingOf[S]) :SQLWriteForm[S] =
			new MappingWriteForm[S, mapping.Origin](
				op, mapping, mapping.columns(op) ++ mapping.columnsWith(op.Preset)
			)

		private def component[S, O](op :WriteOperationView, root :MappingAt[O], mapping :TypedMapping[S, O])
				:SQLWriteForm[S] =
		{
			val export = root.export(mapping)
			if (op.Prohibited.active(export))
				throw new IllegalArgumentException(
					"Cannot " + op + " component " + mapping + " because it contains " + op.Prohibited  + " as " +
					export + " in " + root + "."
				)
			val columns = op.defaultColumns(root, export) ++ op.applicableColumns(root, export).filter(op.Preset.active)
			new MappingWriteForm[S, O](op, root, mapping, columns)
		}

		private def component[S, O](op :WriteOperationView, root :MappingAt[O], mapping :TypedMapping[S, O],
		                            subcomponents :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
		{
			val export = root.export(mapping)
			if (op.Prohibited.active(export))
				throw new IllegalArgumentException(
					"No " + op + " for component " + mapping + " because it contains " + op.Prohibited  + " as " +
						export + " in " + root + "."
				)
			val columns = subcomponents.flatMap { sub =>
				val export = root.export(sub)
				if (op.Prohibited.active(export))
					throw new IllegalArgumentException(
						"No " + op + " for component " + mapping + " of " + root + " with " + subcomponents +
						" because " + sub + " (as " + export + ") contains buff " + op.Prohibited + "."
					)
				op.defaultColumns(root, export)
			}
			val allColumns = op.applicableColumns(root, export)
			val mandatory = allColumns.filter(op.Optional.inactive)
			if (mandatory.exists(col => !columns.contains(col) && op.Optional.inactive(col)))
				throw new IllegalArgumentException(
					mandatory.filter(col => !columns.contains(col) && op.Optional.inactive(col)).mkString(
						"No " + op + " for component " + mapping + " of " + root + " with " + subcomponents +
						" because the list misses its non optional columns ", ", ", "."
					)
				)
			val extras = allColumns.filter(op.Preset.active)
			new MappingWriteForm[S, O](op, root, mapping, columns ++ extras)
		}

	}


}


/**
  * @tparam Comp $ComponentParamInfo
  * @tparam Col  $ColumnParamInfo
  * @tparam S    $SubjectParamInfo
  * @tparam O    $OriginParamInfo
  * @define SubjectParamInfo [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type, that is the type of objects
  *                          read and written to a particular table (or a view, query, or table fragment).
  * @define OriginParamInfo  A marker [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type,
  *                          used to distinguish between several instances of the same mapping class,
  *                          but coming from different sources (especially different aliases for a table
  *                          occurring more then once in a join). At the same time, it adds additional type safety
  *                          by ensuring that only components of mappings included in a query can be used
  *                          in creation of SQL expressions used by that query.
  */
trait SpecificMapping[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
	extends Mapping with MappingTemplate[Comp, Col]
{
	override type Subject = S
	override type Origin  = O
}
