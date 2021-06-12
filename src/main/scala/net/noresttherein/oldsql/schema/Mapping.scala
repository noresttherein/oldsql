package net.noresttherein.oldsql.schema

import java.sql.{CallableStatement, PreparedStatement, ResultSet}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.{slang, OperationType}
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffType, ExtraSelect, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, MappingBound, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ArbitraryProjection, ExactProjection, IsomorphicProjection}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLWriteForm.WriteFormLiteralsBackFeed
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.bits.OptionMapping.Optional
import net.noresttherein.oldsql.sql.{AndFrom, RowProduct, SQLExpression}
import net.noresttherein.oldsql.sql.SQLExpression.GlobalScope
import net.noresttherein.oldsql.sql.ast.LooseComponent
import net.noresttherein.oldsql.sql.mechanics.TableCount


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
  * excluded by default from SELECT statements and requiring their explicit mention. This makes it different
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
  * defined here [[net.noresttherein.oldsql.schema.Mapping.Component[T] Component]], and global, static refinements
  * from the companion object: [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]],
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
  * preventing them from accepting the type parameters of `BaseMapping` or `RefinedMapping`. Abstract type parameters
  * of existential types such as `Component[_]` are not unified, leading to absurd situations
  * where 'obviously' equal types are not unified by the compiler, almost completely preventing their type safe use.
  * This is particularly visible with classes with a type parameter `C[M <: RefinedMapping[_, _]]`
  * or `C[M <: BaseMapping[_, _]]`, defining their Origin and Subject as `M#Origin` and `M#Subject`. While from the
  * inside of the class `m.Origin =:= this.Origin` where `m :M`, when parameterized with a
  * `X <: RefinedMapping[S, O]`, the classes `Subject` and `Origin` types are unrelated to `S, O`:
  * {{{
  *     trait C[M <: RefinedMapping[_, _]] extends BaseMapping[M#Subject, M#Origin]
  *     def assemble[M <: RefinedMapping[S, O], S, O](c :C[M]) :BaseMapping[S, O] = c //type clash
  * }}}
  * The third reason is the limitation of the type inferer which, when faced with
  * a method with a signature in the form of `[M <: RefinedMapping[S, O], S, O](m :M)` will, when applied to
  * `m :BaseMapping[Int, O]` infer types `BaseMapping[Int, O], Nothing, Nothing` causing a compile error.
  * On the other hand, defining the type parameter as `[M <: RefinedMapping[_, _]]` assigns new distinct types to the
  * missing type parameters, which are not unified even with `m.Subject`/`m.Origin` itself, leading to a lot of issues.
  * This can be circumvented with implicit parameters, but at the cost of additional complexity.
  * Finally, there is a bug in the compiler which prevents the use of a refining type constructor such as
  * `RefinedMapping` as a type parameter in some scenarios, requiring a proper class type `BaseMapping`.
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
  * @define ExportSubcomponentsInfo [[net.noresttherein.oldsql.schema.Mapping.export Export]] columns and subcomponents
  *                                 of a mapping's component are ''not'' necessarily ''export'' columns and components
  *                                 of the parent mapping. Hence, calling
  *                                 [[net.noresttherein.oldsql.schema.Mapping.columns component.columns]]
  *                                 may return columns with invalid - from the point of view of this mapping -
  *                                 [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] or even names.
  */ //todo: consider serialization. The problem are all the functions, which are not serializable...
trait Mapping {

	/** The mapped entity type. */
	type Subject

	/** A phantom marker type identifying the origin of this mapping. It is used to statically distinguish between
	  * different instances of the same mapping class, but mapping different portions of the result set -
	  * in particular, they work like aliases for repeated occurrences of a table and its components in a joined query.
	  * In addition, it is used by the SQL DSL to ensure that an SQL expression refers only to components
	  * coming from the one particular query, preventing accidental use of other, non-joined mappings.
	  * This type should ''not'' be used for other purposes, to keep values or be interpreted in any way, such as actual
	  * alias names for joined tables. All concrete `Mapping` implementations are expected to take `Origin`
	  * as a type parameter (by convention, and to leverage Scala's partial kind unification, the last one) for
	  * seamless use in SQL expressions. All components of a mapping must have the same `Origin` type as their
	  * enclosing mapping. By induction, this means that for any 'root' mapping (normally, a mapping for a database
	  * table), the whole subcomponent tree shares the same origin, which provides type safety by disallowing
	  * the use of mappings with other `Origin` types in methods of this trait.
	  *
	  * Casting a `Mapping` to a different `Origin` should be safe. In order to abbreviate the code and provide
	  * better type safety, direct casting to that effect should be avoided, and instead
	  * [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]] extension method
	  * should be used. It relies on the existence of an implicit
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] which defines the result type
	  * of such a cast. If the type inferer can unify a mapping type `X` with some
	  * `M[O] <: Mapping { type Subject = S; type Origin = O }`, which will happen automatically for any mapping class
	  * which accepts its origin type as the last type parameter, an implicit `OriginProjection[M, S]` will exist,
	  * and `m.withOrigin[O]` will be an instance of `M[O]` for any `m :M[_]`. If such a conversion cannot be performed
	  * or is unsuitable, the mapping class should declare its own implicit `OriginProjection` within its companion
	  * object. This makes it possible to define the result type of the cast in case a mapping class doesn't accept
	  * the `Origin` type as its last type parameter, or it can appear in more than one place in the type signature.
	  *
	  * The definitions of this type vary between use cases, but, most notably, SQL DSL specifies the origin
	  * of any component of a table `T[O]` from a ''from'' clause
	  * `F <: `[[net.noresttherein.oldsql.sql.RowProduct RowProduct]] as a type `F <: O <: RowProduct` which starts
	  * with a wildcard `RowProduct` type (either `RowProduct` itself or one of its subtypes, used
	  * as broad classification of their derived classes), 'joined' with table `T`, for example
	  * `RowProduct AndFrom T Join X`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingAt]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Of.P]]
	  */
	type Origin

	/** A type alias for a generic `Mapping` with the same `Subject` type as this mapping and the `Origin` provided as
	  * the type parameter. Used in particular in expressions like `MappingOf[S]#Projection` to obtain a type
	  * constructor for mappings with definitions of both the `Subject` and the `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  */ //todo: rename Project
	type Projection[O] = RefinedMapping[Subject, O]

	/** A type alias for the [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]] with the provided
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
	  * `MappingOf[S]#ColumnProjection` as a sort of a curried type constructor for the `ColumnMapping` trait.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
	  */
	type ColumnProjection[O] = ColumnMapping[Subject, O]

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

	/** An extract for the value of some component of this mapping, with the subject type `T`, which carries
	  * additionally the ''export'' version of that component (from the point of view of this mapping).
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.export]]
	  */
	type Extract[T] = MappingExtract[Subject, T, Origin]

	/** An extract for the value of some column of this mapping, with the subject type `T`, which carries
	  * additionally the ''export'' version of that column (from the point of view of this mapping).
	  * @see [[net.noresttherein.oldsql.schema.ColumnExtract]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.export]]
	  */
	type ColumnExtract[T] = ColumnMappingExtract[Subject, T, Origin]


	/** Any mapping with the same origin marker type, making it a supertype of all valid component types of this mapping.
	  * It is also occasionally used as a part of the expression `MappingOf[S]#Component` as a sort of a curried type
	  * constructor for the narrowed down `RefinedMapping`, not necessarily in the context of components
	  * of any particular mapping instance. The relation of being a component is transitive: all components of
	  * components of a mapping are considered components of that mapping, and must be correctly handled when passed
	  * as an argument to any of its methods. A single 'logical' component - a mapping for a particular column subset
	  * of a table - can exist in multiple copies, as separate components of a mapping, differing in the declared
	  * column names or buffs. This is because a mapping is allowed to adapt any component class by changing these
	  * properties. One of these components is however considered the ''export'', operative version and is used
	  * for assembly. This aliasing of multiple versions is handled by
	  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method and
	  * [[net.noresttherein.oldsql.schema.Mapping.Extract extracts]] for these components. The relation of being
	  * an export component of a mapping is not transitive: an export component of an export component of a mapping
	  * is not necessarily an export component of that mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Column]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Projection]]
	  */ //consider: renaming it to Brick
	type Component[T] = RefinedMapping[T, Origin] 
	//todo: SpecificComponent/SpecificColumn subtypes returned by SQLMapping and similar, so that we don't need all the overrides

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin marker type
	  * as this instance and thus a valid subcomponent type of this mapping. It is also occasionally used as a part
	  * of the expression `MappingOf[S]#Column` as a sort of a curried type constructor for the `ColumnMapping` trait,
	  * which doesn't necessarily describe columns of any particular mapping instance. All comments regarding
	  * different versions of the same logical [[net.noresttherein.oldsql.schema.Mapping.Component component]]
	  * and their ''export'' version apply also to columns.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ColumnProjection]]
	  */
	type Column[T] = ColumnMapping[T, Origin]

	/** Any `Mapping` with the same `Origin` type as this mapping and an unspecified `Subject` type.
	  * Note that it is not the same as `Component[_]`, as the latter is a narrowing mandating that the mapping
	  * has the definition for the `Subject` type (which is of an unknown type). It is also not a direct
	  * analogue of `AnyColumn`, as the `ColumnMapping` trait, extending `BaseMapping` defines both
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
	type AnyColumn = Column[_]

	/** Any [[net.noresttherein.oldsql.schema.bases.BaseMapping BaseMapping]] with the same `Origin` type as this
	  * mapping and the provided `Subject` type. It is typically used not in the context of components of this
	  * mapping, which is the domain of the more generic `Component` member type, but as part of a pseudo curried
	  * type constructor `MappingAt[O]#TypedComponent`, to simply denote any `BaseMapping` instance with
	  * the provided `Subject` and `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.TypedProjection]]
	  * @see [[net.noresttherein.oldsql.schema.bases.BaseMapping]]
	  */ //todo: remove it together with references to BaseMapping in the API
	type TypedComponent[T] = BaseMapping[T, Origin]

	/** A type alias for a dictionary mapping all components (and subcomponents) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.extracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ColumnExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  */ //consider: a dedicated collection. Could construct the map lazily and have defaults
	type ExtractMap = NaturalMap[Component, Extract]

	/** A type alias for a dictionary mapping all columns (including indirect) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.columnExtracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnMappingExtract]]
	  */
	type ColumnExtractMap = NaturalMap[Column, ColumnExtract]




	/** A reference to this instance as a `RefinedMapping` with its own `Origin` and `Subject` types. */
	@inline final def refine :RefinedMapping[Subject, Origin] = this

	/** Is this mapping an instance of [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] ? */
	@inline final def isColumn :Boolean = this.isInstanceOf[ColumnMapping[_, _]]


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
	def apply(pieces: Pieces): Subject =
		optionally(pieces) match {
			case Got(result) => result
			case _ => throw new NoSuchElementException(s"Can't assemble $this from $pieces.")
		}

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
	  */
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
	  * which will throw a `NullPointerException` instead.
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
	  */
	def inEffect(buff :BuffType) :Boolean = buff.active(buffs)

	/** A mapping like this instance but with [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] replaced
	  * with the given collection. The buffs cascade to the components of the new mapping: the ''export'' version
	  * of every component from this mapping has the new buffs as its suffix.
	  */
	def withBuffs(buffs :Buffs[Subject]) :Component[Subject]

	/** A mapping like this instance but with [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] replaced
	  * with the given list. The buffs cascade to the components of the new mapping: the ''export'' version
	  * of every component from this mapping has the new buffs prepended to its list.
	  */
	def withBuffs(buffs :Seq[Buff[Subject]]) :Component[Subject] = withBuffs(Buffs[Subject](refine, buffs :_*))



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
	  */ //fixme: all writtenValues method should take Option[Subject] to allow default values
	def writtenValues[T](op :WriteOperationType, subject :Subject) :ComponentValues[Subject, Origin] = {
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
	  * @return `writtenValues(UPDATE, subject, collector)` unless overriden.
	  */
	def writtenValues[T](op :WriteOperationType, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit

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
	  * @return `writtenValues(FILTER, subject, collector)` unless overriden.
	  */
	def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(FILTER, subject, collector)

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
	  * @return `writtenValues(INSERT, subject, collector)` unless overriden.
	  */
	def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(INSERT, subject, collector)

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
	  * @return `writtenValues(UPDATE, subject, collector)` unless overriden.
	  */
	def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		writtenValues(UPDATE, subject, collector)




	/** Retrieves the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]]
	  * for the given component. Default implementation performs lookup in
	  * [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]], but if no extract is found
	  * and `component eq this` than an identity extract is created as needed.
	  * Overrides should keep this contract.
	  * @throws NoSuchElementException if `component` is not a subcomponent of this mapping.
	  */
	def apply[T](component :Component[T]) :Extract[T] =
		if (component eq this)
			extracts.getOrElse(component, null.asInstanceOf[Extract[T]]) match {
				case null => MappingExtract.ident(component).asInstanceOf[Extract[T]]
			    case ex => ex
			}
		else extracts(component)

	/** Retrieves the [[net.noresttherein.oldsql.schema.ColumnMappingExtract ColumnMappingExtract]]
	  * for the given column. Default implementation performs lookup in
	  * [[net.noresttherein.oldsql.schema.Mapping.columnExtracts columnExtracts]].
	  * @throws NoSuchElementException if `column` is not a subcomponent of this mapping.
	  */
	def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)


	/** A dictionary mapping all subcomponents of this mapping to extracts with their ''export'' versions.
	  * It is used during the assembly process to alias all components to their operative versions, in order to make sure
	  * that even if a subcomponent implementation asks for a value of one of its components which have been modified
	  * by some enclosing component, the mapping passed to its [[net.noresttherein.oldsql.haul.ComponentValues Pieces]]
	  * is substituted with the export version as defined by this mapping (the root). This is both to alias all versions
	  * to the same instance for the purpose of presetting a value, as well as using possibly overriden
	  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method, modifying the assembly process
	  * producing the subject value.
	  * Unlike the [[net.noresttherein.oldsql.schema.Mapping.subcomponents subcomponents]] list, this collection
	  * must contain not only all export components, but also all their original versions as defined by their
	  * parent mappings. It must also contain entries for all mapping instances which can be potentially used in the
	  * assembly process, even if they are not exposed - failing to include such a hidden component will most likely
	  * result in an exception being thrown from [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method.
	  * This again stands in contrast to the `subcomponents` list in that the latter may potentially omit 'synthetic'
	  * components - implementation artifacts used to assemble intermediate values and/or proxies. In some circumstances
	  * it may be acceptable for a component to be absent from this list - this is the case with private components
	  * managed by another mapping and guaranteed to not be used for the assembly (for example, only serving as a group
	  * of columns used directly by its parent), or a table mapping implementation which is known to use all
	  * its components as-is (performing no aliasing, with every component being its export version), but these are all
	  * exceptions which should be avoided in general. The map may, but doesn't have to if this mapping is not a column,
	  * contain an identity extract for this instance.
	  *
	  * The general way of building this map is by starting with entries for all mappings on the `subcomponents` list
	  * (which would map to themselves, being export components) and then, for every one of them, adding
	  * all `extracts` maps with ''their'' components, composed with the extract of the enclosing direct component.
	  * If a given component has not been changed in any way (it is its own export version and used directly
	  * for the assembly), this approach is sufficient. If the component however is a decorator applying some changes,
	  * then there might be a need to cascade those changes to the components of the decorated mapping. This depends
	  * on the implementation of the decorator, but the export components of an export component
	  * from the `components` list of this mapping aren't guaranteed to be export components of this mapping.
	  * If it is not the case, they should be adapted to include the changes made to their parent, in the process
	  * creating new export components of this mapping (which should, in general, be present on the `subcomponents` list).
	  * All the original versions of such subcomponents should receive additional entries in this map by associating
	  * them with the extracts of their export adapters.
	  *
	  * For example, if a mapping of a `Person` class contains a component of `Address`, but adapts it by adding
	  * a column prefix "home_", the columns "street", "city", "country" of the address component are replaced
	  * with columns "home_street", "home_city", "home_country" in the person mapping. The latter should in that case
	  * include entries mapping the new columns to the extracts containing them (created from the extracts provided
	  * by the address component), but also the original three columns, mapping "street" to the extract for "home_street"
	  * and so on. This does not apply however if the prefix was added directly to the mapping class of `Address`,
	  * as it would result in its columns already having their final names and all being their own export versions.
	  *
	  * This is an implementation-oriented method, with all base classes intended for extension by the client code
	  * already providing a proper implementation.
	  */
	def extracts :NaturalMap[Component, Extract]

	/** A dictionary mapping all columns of this mapping, direct and indirect, to extracts with their ''export'' versions.
	  * It is used during the assembly process to alias all components to their operative versions, in order to make sure
	  * that even if a subcomponent implementation asks for a value of one of its columns which have been modified
	  * by some enclosing component, the column passed to its `Pieces` is substituted with the export version
	  * as defined by this mapping (the root). This is both to alias all versions to the same instance for the purpose
	  * of presetting a value, as well as using their [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]]
	  * method (instead of the aliased component's), possibly modified by modifications to the buff list.
	  * If this component is a column, the map is a singleton containing this mapping and an identity extract.
	  * Unlike the [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list, this collection
	  * must contain not only all export columns, but also all their original versions as defined by their
	  * parent mappings. It must also contain entries for all mapping instances which can be potentially used in the
	  * assembly process, even if they are not exposed - failing to include such a hidden component will most likely
	  * result in an exception being thrown from [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] method documentation for more information
	  *     about implementation.
	  */
	def columnExtracts :NaturalMap[Column, ColumnExtract]


	/** Adapts the given subcomponent (a component being the end of some path of components starting with this instance)
	  * to its public form as present in the `subcomponents` and `components` list. For every valid transitive
	  * subcomponent of this mapping, there is exactly one equal to its export form on the `subcomponents` list.
	  * This process can modify the mapping definition by changing names of the columns (prefixing them) and updating
	  * their buffs by cascading the buffs present on this instance.
	  * By default it returns the `export` property of the extract of the component, returned by
	  * [[net.noresttherein.oldsql.schema.Mapping.apply[T](component:Component[T]) apply(component)]].
	  * Unlike [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] map, this method
	  * is guaranteed to return `this` if `column eq this` (that is, this mapping is considered the export component
	  * of itself by this method).
	  */
	@throws[NoSuchElementException]("if the component is not a subcomponent of this instance.")
	def export[T](component :Component[T]) :Component[T] =
		if (component eq this) component else apply(component).export //extra check in order to not create a MappingExtract

	/** Same as [[net.noresttherein.oldsql.schema.Mapping.export export]]`(component)`, but instead of throwing
	  * `NoSuchElementException`, it instead simply returns the argument itself.
	  * @return `extracts.getOrElse(component, component)`.
	  */
	def exportOrNot[T](component :Component[T]) :Component[T] = {
		val extract = extracts.getOrElse[Extract, T](component, null)
		if (extract != null) extract.export else component
	}

	/** Adapts the given column of one of the transitive subcomponents of this instance to the form present
	  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
	  * exactly one equal to its export version on the `columns` list. This process can change the name and buffs
	  * of the column, reflecting additional information present in this mapping.
	  * By default it returns the `export` property of the extract of the column, returned by
	  * [[net.noresttherein.oldsql.schema.Mapping.apply[T](column:Column[T]) apply(column)]].
	  * Unlike [[net.noresttherein.oldsql.schema.Mapping.columnExtracts columnExtracts]] map, this method
	  * is guaranteed to return `this` if `column eq this` (that is, this mapping is considered the export component
	  * of itself by this method).
	  */ //consider: renaming it to operative perhaps? it can easily conflict with an `export` column in client apps
	def export[T](column :Column[T]) :Column[T] = apply(column).export //columns contain themselves in extractMaps

	/** Same as [[net.noresttherein.oldsql.schema.Mapping.export export]]`(column)`, but instead of throwing
	  * `NoSuchElementException`, it instead simply returns the argument itself.
	  * @return `extracts.getOrElse(column, column)`.
	  */
	def exportOrNot[T](column :Column[T]) :Column[T] = {
		val extract = columnExtracts.getOrElse[ColumnExtract, T](column, null) //todo: make sure there is no object allocation here
		if (extract != null) extract.export else column
	}

	/** Verifies if the given mapping is a (sub)component of this mapping. If this method returns `true`,
	  * [[net.noresttherein.oldsql.schema.Mapping.export export]] and
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] methods returning the export version of the component
	  * and its extract will not throw an exception. Default implementation simply checks if `component`
	  * is a key of [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]] map of this mapping.
	  */
	def contains[T](component :Component[T]) :Boolean = extracts.contains(component)



	/** Direct component mappings of this mapping, including any top-level columns. Always empty for columns.
	  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
	  * or mapped components and aliases for other components of the `Mapping`. For example, if a foreign key
	  * consists of columns with a business meaning by themselves, a mapping could include these columns directly
	  * as normal and create an 'ephemeral' component for the foreign key, being a mapping assembling its subject
	  * from these columns in the normal way, but which is not featured on any components list. For all non-column
	  * components however this list will cover all columns defined directly by the mapping. The components on the list
	  * are always the ''export'' versions, but their subcomponents are not, generally, the ''export'' versions
	  * from the point of view of this mapping.
	  */ //todo: subcomponents of these should have 'export' names, just not buffs
	def components :Unique[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared directly by this mapping,
	  * by it's components or other subcomponents), or all that this mapping cares to expose, as instances
	  * of `this.`[[net.noresttherein.oldsql.schema.Mapping.Component Component]]`[_]`. It is typically defined
	  * by recursive ''flat mapping'' over the [[net.noresttherein.oldsql.schema.Mapping.components components]] list
	  * and including the direct components themselves. This list is always empty for columns, thus ending any similar
	  * recursion. Some mappings may wish not to expose some of the components they define, primarily in the case
	  * of adapted or mapped components and aliases for other components of the `Mapping`. For all non-column components
	  * however this list will cover all columns defined by the mapping. The components on the list are always
	  * the ''export'' versions, but their subcomponents are not, generally, the ''export'' versions from the point
	  * of view of this mapping.
	  *///todo: subcomponents of these should have 'export' names, just not buffs
	def subcomponents :Unique[Component[_]]

	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only,
	  * write-only and so on. Column mappings returns a singleton list containing themselves. All columns on the list
	  * are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def columns :Unique[Column[_]]

	/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` buff).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def selectable :Unique[Column[_]] = columnsWithout(NoSelect)

	/** All columns which can be part of an SQL statement's where clause (don't have the `NoFilter` buff active).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def filterable :Unique[Column[_]] = columnsWithout(NoFilter)

	/** All columns which can occur in an insert statement (don't have the `NoInsert` buff active).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def insertable :Unique[Column[_]] = columnsWithout(NoInsert)

	/** All columns which can be updated on existing database records (don't have the `NoUpdate` buff active).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def updatable :Unique[Column[_]] = columnsWithout(NoUpdate)

	/** Columns autogenerated by the database on insert (have the `AutoInsert` buff); this implies being non-insertable.
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def autoInserted :Unique[Column[_]] = columnsWith(AutoInsert)

	/** All columns which are updated by the database on each update statement (have the `AutoUpdate` buff)
	  * and could/should be returned to the application). All columns on the list are the ''export'' (operative)
	  * versions from the point of view of this mapping.
	  */
	def autoUpdated :Unique[Column[_]] = columnsWith(AutoUpdate)

	/** Columns included in ''select'' clauses using this component unless the list is modified by an explicit
	  * request for including or excluding certain optional component. This list should always be consistent
	  * with [[net.noresttherein.oldsql.schema.Mapping.columns columns]]`.filter(`[[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]`.inactive)`,
	  * it is provided here as a shorthand and in order to allow its caching.
	  */
	def selectedByDefault :Unique[Column[_]] = columnsWithout(NoSelectByDefault)

	/** Columns included when comparing this component with another [[net.noresttherein.oldsql.sql.SQLExpression]],
	  * with the value type being equal to `this.Subject`, inside ''where'' clauses of SQL ''selects'', unless
	  * an explicit request is made for including or excluding certain optional component. This list should always
	  * be consistent with
	  * [[net.noresttherein.oldsql.schema.Mapping.columns columns]]`.filter(`[[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]`.inactive)`,
	  * it is provided here as a shorthand and in order to allow its caching.
	  */
	def filteredByDefault :Unique[Column[_]] = columnsWithout(NoFilterByDefault)

	/** Columns included in all SQL ''insert'' statements for this component, unless certain optional components
	  * are explicitly included or excluded. This list should always be consistent with
	  * [[net.noresttherein.oldsql.schema.Mapping.columns columns]]`.filter(`[[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]`.inactive)`,
	  * it is provided here as a shorthand and in order to allow its caching.
	  */
	def insertedByDefault :Unique[Column[_]] = columnsWithout(NoInsertByDefault)

	/** Columns included in all SQL ''update'' statements for this component, unless certain optional components
	  * are explicitly included or excluded. This list should always be consistent with
	  * [[net.noresttherein.oldsql.schema.Mapping.columns columns]]`.filter(`[[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]],`.inactive)`,
	  * it is provided here as a shorthand and in order to allow its caching.
	  */
	def updatedByDefault :Unique[Column[_]] = columnsWithout(NoUpdateByDefault)

	/** All columns of `component` which are considered selectable by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoSelect.inactive)`,
	  *         in an undefined order.
	  */
	def selectable(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoSelect)

	/** All columns of `component` which are considered filterable (usable in SQL ''where'' clauses) by this mapping.
	  * $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoFilter.inactive)`,
	  *         in an undefined order.
	  */
	def filterable(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoFilter)

	/** All columns of `component` which are considered insertable by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoInsert.inactive)`,
	  *         in an undefined order.
	  */
	def insertable(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoInsert)

	/** All columns of `component` which are considered updatable by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoUpdate.inactive)`,
	  *         in an undefined order.
	  */
	def updatable(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoUpdate)

	/** All columns of `component` which are considered auto inserted (generated by the database on insert)
	  * by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.AutoInsert AutoInsert]] active.
	  * @return the same column set as `component.columns.map(export(_)).filter(AutoInsert.active)`,
	  *         in an undefined order.
	  */
	def autoInserted(component :Component[_]) :Unique[Column[_]] = columnsWith(component, AutoInsert)

	/** All columns of `component` which are considered auto updated (generated by the database on update)
	  * by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.AutoUpdate AutoUpdate]] active.
	  * @return the same column set as `component.columns.map(export(_)).filter(AutoUpdate.active)`,
	  *         in an undefined order.
	  */
	def autoUpdated(component :Component[_]) :Unique[Column[_]] = columnsWith(component, AutoUpdate)

	/** All columns of `component` which are specified as selected by default this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoSelectByDefault.inactive)`,
	  *         in an undefined order.
	  */
	def selectedByDefault(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoSelectByDefault)

	/** All columns of `component` which are specified as filtered by default (included in component vs component
	  * comparisons in ''where'' clauses) by this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoFilterByDefault.inactive)`,
	  *         in an undefined order.
	  */
	def filteredByDefault(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoFilterByDefault)

	/** All columns of `component` which are specified as inserted by default this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoInsertByDefault.inactive)`,
	  *         in an undefined order.
	  */
	def insertedByDefault(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoInsertByDefault)

	/** All columns of `component` which are specified as updated by default this mapping. $ExportSubcomponentsInfo
	  * All subcomponents need exporting before use; this method exports all columns of `component`
	  * and returns those with [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]] inactive.
	  * @return the same column set as `component.columns.map(export(_)).filter(NoSelectByDefault.inactive)`,
	  *         in an undefined order.
	  */
	def updatedByDefault(component :Component[_]) :Unique[Column[_]] = columnsWithout(component, NoUpdateByDefault)

	/** All selectable columns of this mapping from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.selectable(component:Component[_]) selectable]]`(this)`.
	  * May be more convenient when chaining methods.
	  * @param parent a mapping of which this mapping is a component.
	  */
	def selectableIn(parent :Component[_]) :Unique[Column[_]] = parent.selectable(refine)

	/** All filterable columns of this mapping from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.filterable(component:Component[_]) selectable]]`(this)`.
	  * May be more convenient when chaining methods.
	  * @param parent a mapping of which this mapping is a component.
	  */
	def filterableIn(parent :Component[_]) :Unique[Column[_]] = parent.filterable(refine)

	/** All insertable columns of this mapping from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.insertable(component:Component[_]) insertable]]`(this)`.
	  * May be more convenient when chaining methods.
	  * @param parent a mapping of which this mapping is a component.
	  */
	def insertableIn(parent :Component[_]) :Unique[Column[_]] = parent.insertable(refine)

	/** All updatable columns of this mapping from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.updatable(component:Component[_]) updatable]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def updatableIn(parent :Component[_]) :Unique[Column[_]] = parent.updatable(refine)

	/** All columns of this mapping which are generated by the database on insert from the point of view 
	  * of a supercomponent of this mapping. It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.autoInserted(component:Component[_]) autoInserted]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def autoInsertedIn(parent :Component[_]) :Unique[Column[_]] = parent.autoInserted(refine)

	/** All columns of this mapping which are generated by the database on update from the point of view 
	  * of a supercomponent of this mapping. It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.autoUpdated(component:Component[_]) autoUpdated]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def autoUpdatedIn(parent :Component[_]) :Unique[Column[_]] = parent.autoUpdated(refine)

	/** All columns of this mapping selectable by default from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.selectedByDefault(component:Component[_]) selectedByDefault]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def selectedByDefaultIn(parent :Component[_]) :Unique[Column[_]] = parent.selectedByDefault(refine)

	/** All columns of this mapping filterable by default from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.filteredByDefault(component:Component[_]) filteredByDefault]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def filteredByDefaultIn(parent :Component[_]) :Unique[Column[_]] = parent.filteredByDefault(refine)

	/** All columns of this mapping insertable by default from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.insertedByDefault(component:Component[_]) insertedByDefault]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def insertedByDefaultIn(parent :Component[_]) :Unique[Column[_]] = parent.insertedByDefault(refine)

	/** All columns of this mapping updatable by default from the point of view of a supercomponent of this mapping.
	  * It is a shortcut for (which should always stay consistent with) 
	  * `parent.`[[net.noresttherein.oldsql.schema.Mapping.updatedByDefault(component:Component[_]) updatedByDefault]]`(this)`
	  * @param parent a mapping of which this mapping is a component.
	  */
	def updatedByDefaultIn(parent :Component[_]) :Unique[Column[_]] = parent.updatedByDefault(refine)

	/** All columns from the `columns` list with a given buff active. */
	def columnsWith(buff :BuffType) :Unique[Column[_]] =
		columns.filter(buff.active)

	/** All columns without the given buff. */
	def columnsWithout(buff :BuffType) :Unique[Column[_]] =
		columns.filter(buff.inactive)

	/** All columns of `component` [[net.noresttherein.oldsql.schema.Mapping.export exported]] by this mapping
	  * which have the given buff. $ExportSubcomponentsInfo
	  * @return the same column set as `component.columns.view.map(export(_)).filter(buff.active)`,
	  *         in an undefined order.
	  */
	def columnsWith(component :Component[_], buff :BuffType) :Unique[Column[_]] =
		component.columns.view.map(export(_)).filter(buff.active).to(Unique)

	/** All columns of `component` [[net.noresttherein.oldsql.schema.Mapping.export exported]] by this mapping
	  * which do not have the given buff. $ExportSubcomponentsInfo
	  * @return the same column set as `component.columns.view.map(export(_)).filter(buff.active)`,
	  *         in an undefined order.
	  */
	def columnsWithout(component :Component[_], buff :BuffType) :Unique[Column[_]] =
		component.columns.view.map(export(_)).filter(buff.inactive).to(Unique)


	/** All columns (in their operative versions) of this mapping which can be used as part of the given SQL statement type.
	  * Delegates to the property specific to this operation type. Subclasses can reverse the order of delegation,
	  * but it should always remain consistent with these methods.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.selectable]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.filterable]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.updatable]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.insertable]]
	  */
	def columns(op :OperationType) :Unique[Column[_]] = op match {
		case SELECT => selectable
		case FILTER => filterable
		case INSERT => insertable
		case UPDATE => updatable
	}

	/** Returns an ''export'' column of this mapping under the given a name. */
	@throws[NoSuchComponentException]("if this mapping doesn't declare a direct or indirect column with the given name.")
	def columnNamed(name :String) :Column[_]



	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.-]]
	  */
	def + :ComponentSelection[Subject, Origin] = IncludedComponent(refine)

	/** Wraps this mapping in a [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  * marking that it should be used in some database operation. Result can be used in
	  * [[net.noresttherein.oldsql.schema.Mapping.apply apply]] and similar methods of other classes.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.+]]
	  */
	def - :ComponentSelection[Subject, Origin] = ExcludedComponent(refine)

	/** A modified version this mapping with the given components included/excluded ''by default''
	  * from all applicable database operations. This method is similar to
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
	  * @param adjustments a sequence of components of this mapping wrapped in either
	  *                    [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludedComponent]],
	  *                    if the component is to be included, or in
	  *                    [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludedComponent]]
	  *                    if it should be excluded.
	  */
	def apply(adjustments :ComponentSelection[_, Origin]*) :RefinedMapping[Subject, Origin]

	/** A modified version this mapping with the given components included/excluded ''by default''
	  * from all applicable database operations. This method is similar to
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
	  * Applications probably will find the analogical methods of
	  * [[net.noresttherein.oldsql.schema.Relation.apply(components* Relation]] or
	  * [[net.noresttherein.oldsql.sql.ast.ComponentSQL.alter ComponentSQL]] more useful.
	  * @param include a collection of components of this mapping which should be included in every operation, if possible.
	  * @param exclude a collection of components of this mapping which should be excluded from every operation, if possible.
	  */ //consider: maybe this should be named alterBuffs?
	def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :RefinedMapping[Subject, Origin]

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
	  */
	def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

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
	def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

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
	def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

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
	def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]


//todo: think of a better name for this method (customize clashes with the declaration in StaticMapping)
//	def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
//		op match {
//			case SELECT => forSelect(include, exclude)
//			case FILTER => forFilter(include, exclude)
//			case UPDATE => forUpdate(include, exclude)
//			case INSERT => forUpdate(include, exclude)
//		}



	/** Read form of a select statement for this mapping including the given components of this mapping.
	  * It is a way to modify the default list of used columns by including those which normally aren't
	  * (have the `ExplicitSelect` buff), or excluding those which normally are used, but are not mandatory
	  * (have the `OptionalSelect` buff). All components on the list (and their columns) are first aliased to their
	  * operative versions by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
	  * @param components a list of components which should be included in the form. It can include both direct
	  *                   components and subcomponents, not only columns. The `ExplicitSelect` buff is ignored,
	  *                   if present, but including a component with `NoSelect` buff (or one implying it) will result
	  *                   in an exception; any subcomponents with said buff of any of these components are however
	  *                   silently ignored as in by default. The list must cover, directly or indirectly,
	  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
	  *                   ''do not'' include the buff `NoSelectByDefault` or any that imply it).
	  * @return a write form including the exact set of columns covered by the specified components.
	  * @throws IllegalArgumentException if the column set covered by the given components (in their operative versions)
	  *        includes a column with the `NoSelect` buff (or one which implies it), or if a column of this mapping
	  *        exists which does not belong to this set and does not have the `NoSelectByDefault` buff.
	  */ //fixme: proper inclusion semantics, using only default, not all selectable columns of the component
	def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject]

	/** Default read form (included columns) of a select statement for this mapping. */
	def selectForm :SQLReadForm[Subject]

	def selectForm[T](component :Component[T]) :SQLReadForm[T]


	/** A write form (included columns) used in ''WHERE'' clauses of select statements when this mapping's subject
	  * is used directly in the SQL DSL or other filter conditions. It is a way to modify the default list
	  * of used columns by including those which normally aren't (have the `ExplicitFilter` buff), or excluding those
	  * which normally are used, but are not mandatory (have the `OptionalFilter` buff). All components on the list
	  * (and their columns) are first aliased to their operative versions by the
	  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
	  * Should be equivalent to [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm(FILTER, components)]],
	  * but the direction of delegation between the two methods is left for subclasses to decide.
	  * @param components a list of components which should be included in the form. It can include both direct
	  *                   components and subcomponents, not only columns. The `ExplicitFilter` buff is ignored,
	  *                   if present, but including a component with `NoFilter` buff (or one implying it) will result
	  *                   in an exception; any subcomponents with said buff of any of these components are however
	  *                   silently ignored as in by default. The list must cover, directly or indirectly,
	  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
	  *                   ''do not'' include the buff `NoFilterByDefault` or any that imply it).
	  * @return a write form including the exact set of columns covered by the specified components.
	  * @throws IllegalArgumentException if the column set covered by the given components (in their operative versions)
	  *        includes a column with the `NoFilter` buff (or one which implies it), or if a column of this mapping
	  *        exists which does not belong to this set and does not have the `NoFilterByDefault` buff.
	  */ //fixme: proper inclusion semantics, using only default, not all filterable columns of the component
	def filterForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included parameters) of a filter for a particular subject value. */ //todo: PK? all?
	def filterForm :SQLWriteForm[Subject]

	def filterForm[T](component :Component[T]) :SQLWriteForm[T]

	/** A write form (included columns) used in insert statements of this mapping's subjects.
	  * It is a way to modify the default list of used columns by including those which normally aren't
	  * (have the `ExplicitInsert` buff), or excluding those which normally are used, but are not mandatory
	  * (have the `OptionalInsert` buff). All components on the list (and their columns) are first aliased to their
	  * operative versions by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
	  * Should be equivalent to [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm(INSERT, components)]],
	  * but the direction of delegation between the two methods is left for subclasses to decide.
	  * @param components a list of components which should be included in the form. It can include both direct
	  *                   components and subcomponents, not only columns. The `ExplicitInsert` buff is ignored,
	  *                   if present, but including a component with `NoInsert` buff (or one implying it) will result
	  *                   in an exception; any subcomponents with said buff of any of these components are however
	  *                   silently ignored as in by default. The list must cover, directly or indirectly,
	  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
	  *                   ''do not'' include the buff `NoInsertByDefault` or any that imply it).
	  * @return a write form including the exact set of columns covered by the specified components.
	  * @throws IllegalArgumentException if the column set covered by the given components (in their operative versions)
	  *        includes a column with the `NoInsert` buff (or one which implies it), or if a column of this mapping
	  *        exists which does not belong to this set and does not have the `NoInsertByDefault` buff.
	  */ //fixme: proper inclusion semantics, using only default, not all insertable columns of the component
	def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]
	//fixme: subcomponent.insertForm is not the export form if its columns are not export columns!!!
	/** Default write form (included columns) used for insert statements of this mapping. */
	def insertForm :SQLWriteForm[Subject]

	def insertForm[T](component :Component[T]) :SQLWriteForm[T]

	/** A write form (included columns) used in update statements of this mapping's subjects.
	  * It is a way to modify the default list of used columns by including those which normally aren't
	  * (have the `ExplicitUpdate` buff), or excluding those which normally are used, but are not mandatory
	  * (have the `OptionalUpdate` buff). All components on the list (and their columns) are first aliased to their
	  * operative versions by the [[net.noresttherein.oldsql.schema.Mapping.export export]] method.
	  * Should be equivalent to [[net.noresttherein.oldsql.schema.Mapping.writeForm writeForm(UPDATE, components)]],
	  * but the direction of delegation between the two methods is left for subclasses to decide.
	  * @param components a list of components which should be included in the form. It can include both direct
	  *                   components and subcomponents, not only columns. The `ExplicitUpdate` buff is ignored,
	  *                   if present, but including a component with `NoUpdate` buff (or one implying it) will result
	  *                   in an exception; any subcomponents with said buff of any of these components are however
	  *                   silently ignored as in by default. The list must cover, directly or indirectly,
	  *                   all mandatory columns of this mapping (i.e. those, which - in their operative version -
	  *                   ''do not'' include the buff `NoUpdateByDefault` or any that imply it).
	  * @return a write form including the exact set of columns covered by the specified components.
	  * @throws IllegalArgumentException if the column set covered by the given components (in their operative versions)
	  *        includes a column with the `NoUpdate` buff (or one which implies it), or if a column of this mapping
	  *        exists which does not belong to this set and does not have the `NoUpdateByDefault` buff.
	  */ //fixme: proper inclusion semantics, using only default, not all updatable columns of the component
	def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) used for update statements of this mapping. */
	def updateForm :SQLWriteForm[Subject]

	def updateForm[T](component :Component[T]) :SQLWriteForm[T]

	/** Default write form (included columns) of this mapping used for the given SQL statement type.
	  * It should return a form equal to the one of the property specific to the operation type, but
	  * the direction of delegation between them is left for subclasses.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
	  */ //fixme: proper inclusion semantics, using only default, not all pertinent columns of the component
	def writeForm(op :WriteOperationType, components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) of this mapping used for the given SQL statement type.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.filterForm]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.insertForm]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.updateForm]]
	  */
	def writeForm(op :WriteOperationType) :SQLWriteForm[Subject]



	//consider: all renaming/mapping methods to lose Origin in order to *not* be components of this mapping
	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string.
	  * This is equivalent to `prefixed(prefix + ".")` unless prefix is an empty string, in which case it is
	  * a no-op (or at least returns a mapping with exact same column name).
	  */
	def qualified(prefix :String) :Component[Subject]

	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string. */
	def prefixed(prefix :String) :Component[Subject]

	/** An adapter of this mapping with the names of all its ''export'' columns replaced with the given function. */
	def renamed(naming :String => String) :Component[Subject]


	/** Lifts this mapping by encapsulating the subject values in an `Option`. The created mapping will
	  * always return `Some(x)` from its `optionally` and `assemble` methods, where `x` is the value returned by
	  * the method of this instance. This means that `Some(None)` is returned when this component has no value
	  * associated in a given `Pieces` instance, and that `apply(Pieces)` method of the returned mapping will
	  * never fail. This instance is exposed as the public `get` field of the returned mapping, allowing direct
	  * access to any components definitions in this mapping.
	  */
	def inOption :Optional[this.type]


	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will be returned. If `back` extractor will return `None`, a `null` value will be written
	  * to the database.
	  *///consider: renaming to map if it won't interfere with type inference
	def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X] = null) :Component[X]

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will be returned.
	  */
	def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		as(there, back)

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If `there` or this mapping's `optionally` subject constructor returns `None`,
	  * implicitly provided here `NullValue.value` will be returned. If `back` returns `None`, `null` value will
	  * be written in the database.
	  */
	def optMap[X](there :Subject => Option[X], back :X => Option[Subject])
	             (implicit nulls :NullValue[X] = null) :Component[X] =
		as(there, back)



	/** Unused by most `Mapping` classes as, by default, they do not override referential equality behaviour.
	  * Provided here for those few classes which do and to future proof us against overrides without
	  * the `override` keyword.
	  */
	def canEqual(that :Any) :Boolean = that.isInstanceOf[Mapping]


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
				case column :ColumnMapping[_, _] =>
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
	protected[oldsql] def every_concrete_Mapping_must_extend_BaseMapping :Nothing

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






sealed abstract class LowPriorityMappingImplicits {
	//exists for use as the right side of SQLExpression.=== and similar, which will instantiate type F before applying conversion
	implicit def mappingSQL[F <: RowProduct, C <: Mapping, S, O <: RowProduct] //can't use TableOffset as we might be converting a component of a table
                           (mapping :C)(implicit origin :C <:< MappingAt[O], base :F <:< O,
                                                 offset :TableCount[O, _ <: Numeral], projection :OriginProjection[C, S])
			:SQLExpression[F, GlobalScope, S] =
		LooseComponent(projection[F](mapping), offset.offset)

}



object Mapping extends LowPriorityMappingImplicits {

	/** Adds factory methods for `MappingPath`s from the implicitly enriched `Mapping` instance to its components. */
	@inline implicit def mappingPathConstructor[X <: Mapping, M <: RefinedMapping[S, O], S, O]
	                                           (self :X)(implicit hint :InferTypeParams[X, M, RefinedMapping[S, O]])
			:MappingPathConstructor[M, S, O] =
		new MappingPathConstructor[M, S, O](self)

	/** Adds a `\` method to any `Mapping`, creating a `ComponentPath` from it to one of its components. */
	class MappingPathConstructor[M <: RefinedMapping[S, O], S, O](private val self :M) extends AnyVal {

		/** Creates a `ComponentPath` leading from this (wrapped) mapping to its specified component. */
		def \[X <: Mapping, C <: RefinedMapping[T, O], T]
		     (component :X)(implicit hint :InferTypeParams[X, C, RefinedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component)

		def \[X <: Mapping, C <: RefinedMapping[T, O], T]
		     (component :M => X)(implicit hint :InferTypeParams[X, C, RefinedMapping[T, O]])
				:ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))

		def apply[X <: Mapping, C <: RefinedMapping[T, O], T]
		         (component :M => X)(implicit hint :InferTypeParams[X, C, RefinedMapping[T, O]])
				:ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))
	}



	implicit def componentSQL[F <: RowProduct, C <: Mapping, S]//can't use TableOffset as we might be converting a component of a table
                             (mapping :C)(implicit origin :C <:< MappingAt[F], offset :TableCount[F, _ <: Numeral],
                                                   projection :OriginProjection[C, S])
			:LooseComponent[F, projection.WithOrigin, S] =
		LooseComponent(mapping)


	implicit class MappingExtension[M <: Mapping](private val self :M) extends AnyVal { //consider: rename to *
//		def toSQL[F <: RowProduct, S](from :F)(implicit origin :M <:< MappingAt[F], offset :TableCount[F, _ <: Numeral],
//		                                                projection :OriginProjection[M, S])
//				:LooseComponent[F, projection.WithOrigin, S] =
//			toSQL[F, S]

		def toSQL[F <: RowProduct, S](implicit origin :M <:< MappingAt[F], offset :TableCount[F, _ <: Numeral],
		                                       projection :OriginProjection[M, S])
				:LooseComponent[F, projection.WithOrigin, S] =
			LooseComponent(self)
	}



	/** Adds a right-associative method `@:` to any `Mapping` with defined `Origin` and `Subject` types,
	  * which attaches a label type to it by wrapping it in `L @: M`.
	  */
	implicit class RefinedMappingExtension[M <: RefinedMapping[_, _]](private val self :M) extends AnyVal {
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




	@inline implicit def MappingOriginProjector[M <: Mapping, S](mapping :M)(implicit subject :M <:< MappingOf[S])
			:MappingOriginProjector[M, S] =
		new MappingOriginProjector[M, S](mapping)

	/** Adds a `withOrigin[O]` method to any `Mapping` subtype, which substitutes its `Origin` type to type `O`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  */
	class MappingOriginProjector[M <: Mapping, S](private val self :M) extends AnyVal {

		/** Converts this mapping to one where `type Origin = O`. The result type is determined by the implicit
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]].
		  * If `M <: C[A1, ..., AN, A]` where `C[_, ..., _, O] <: BaseMapping[_, O]`, then the result type will
		  * be inferred as `C[A1, ..., AN, O]`. If the origin type parameter `A` is not the last one, or the mapping
		  * defines its `Origin` type by other means entirely, type inference will fail and an implicit
		  * `OriginProjection` value with the correct `WithOrigin` type should be provided in the companion object to `M`.
		  * See the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] class documentation
		  * for a detailed information about providing custom projections and cases when it is required.
		  */
		@inline def withOrigin[O](implicit projection :OriginProjection[M, S]) :projection.WithOrigin[O] =
			self.asInstanceOf[projection.WithOrigin[O]]

		/** Upcasts this instance to a type `P[M#Origin] <: RefinedMapping[M#Subject, M#Origin]` such that
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
	  */
	@implicitNotFound("Cannot project mapping ${M} (of ${S}) to another Origin type: " +
		              "no implicit OriginProjection[${M}, ${S}].")
	sealed trait OriginProjection[-M <: Mapping, S] extends Serializable { self =>

		/** A type such that `M <: WithOrigin[_]` which does not reference the origin type `O` in its signature
		  * anywhere except as the type parameter. In other words, a conversion `WithOrigin[A] => WithOrigin[B]`
		  * replaces every reference to `A` with the type `B`. This in particular means that all components and extracts
		  * returned by the mapping after conversion define their `Origin` type as `B`, consistently with the converted
		  * mapping.
		  */ //this is TypeMapping bound as it is required by MappingSQL hierarchy due to a compiler bug.
		type WithOrigin[O] <: BaseMapping[S, O]

		/** Casts the mapping of type `M` to a type where all references to its current origin type are replaced
		  * by the `O` type.
		  */
		@inline final def apply[O](mapping :M) :WithOrigin[O] = mapping.asInstanceOf[WithOrigin[O]]

		/** A projection from any `WithOrigin[O]` to `WithOrigin[X]`. */
		@inline def isomorphism[O] :IsomorphicProjection[WithOrigin, S, O] =
			this.asInstanceOf[IsomorphicProjection[WithOrigin, S, O]]

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
		type IsomorphicProjection[M[X] <: BaseMapping[S, X], S, O] =
			ExactProjection[M[O]] { type WithOrigin[X] = M[X] }

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


		/** A natural projection of a `BaseMapping` subtype made by providing a different `Origin` type
		  * to its type constructor.
		  * @tparam M a type constructor for a `BaseMapping` subtype which sets all references to the `Origin` type
		  *           to its type parameter.
		  * @tparam S the subject type of the projected mapping.
		  * @tparam O the input origin typ of the projected mapping.
		  */
		def isomorphism[M[A] <: BaseMapping[S, A], S, O] :IsomorphicProjection[M, S, O] =
			CastingProjection.asInstanceOf[IsomorphicProjection[M, S, O]]

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






	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S`. */
	type MappingOf[S] = Mapping { type Subject = S } //todo: rename to MappingFor

	/** Narrowing of the `Mapping` trait to subtypes which define the `Origin` type as `O`.
	  * While mapping types are expected to have the `Origin` as a free type parameter, conversion from one
	  * origin type to another may require more
	  */ //todo: at is not the proper preposition associated with 'Origin', but 'of' would be misleading. Rename Origin to ...? Lineage, sphere, family, line
	type MappingAt[O] = Mapping { type Origin = O } //MappingFrom/ComponentOf

	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S` and `Origin` as `O`. */
	type RefinedMapping[S, O] = Mapping { type Subject = S; type Origin = O }

	/** Narrowing of the `Mapping` trait to subtypes of the given `Origin` type `O` and where the `Subject` type
	  * is bound from above by `S`.
	  */
	type MappingBound[S, O] = Mapping { type Subject <: S; type Origin = O }

	type RefinedOf[O] = RefinedMapping[_, O]

	type RefinedAt[S] = RefinedMapping[S, _]

	type CompatibleMapping[M <: Mapping] = Mapping {
		type Origin = M#Origin
		type Subject = M#Subject
	}

	type ConcreteSubclass[M <: Mapping] = M {
		type Origin = M#Origin
		type Subject = M#Subject
	}

	/** A curried definition of [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]]`[S, O]`,
	  * containing a single type constructor `P[O] = RefinedMapping[S, O]`. Similar types are defined
	  * in companion objects of various `Mapping` subtypes, allowing their use as type parameters to classes/methods
	  * which require the definition of a mapping accepting its [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]]
	  * type.
	  */
	type Of[S] = { type P[O] = RefinedMapping[S, O] }

	type Last[M[O] <: MappingAt[O]] = RowProduct AndFrom M


	/** A thin wrapper over a component of some mapping `MappingAt[O]` which selects it to be included or
	  * excluded from some database operation, depending on which of the two subclasses of this type is the class
	  * of this object. Instances of this trait are typically created by calling either
	  * [[net.noresttherein.oldsql.schema.Mapping.+ +]] or [[net.noresttherein.oldsql.schema.Mapping.- -]]
	  * on a component of the enclosing mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent]]
	  */
	sealed trait ComponentSelection[T, O] extends Serializable {
		val component :RefinedMapping[T, O]
	}

	/** Marks the wrapped component for being included in some database operation.
	  * Each mapping has a factory method wrapping it in this class: [[net.noresttherein.oldsql.schema.Mapping.+ +]].
	  */
	case class IncludedComponent[T, O](component :RefinedMapping[T, O]) extends ComponentSelection[T, O]

	/** Marks the wrapped component for being excluded from some database operation.
	  * Each mapping has a factory method wrapping it in this class: [[net.noresttherein.oldsql.schema.Mapping.- -]].
	  */
	case class ExcludedComponent[T, O](component :RefinedMapping[T, O]) extends ComponentSelection[T, O]



	trait ColumnFilter {
		def apply[S](mapping :MappingOf[S]) :Unique[mapping.Column[_]] =
			mapping.columns.filter(test[mapping.Origin])

		def filter[O](columns :Unique[ColumnMapping[_, O]]) :Unique[ColumnMapping[_, O]] =
			columns.filter(test[O])

		def test[O](column :MappingAt[O]) :Boolean

		def read[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			MappingReadForm[S, mapping.Origin](mapping, apply(mapping :mapping.type))

		def write[S](op :WriteOperationType, mapping :MappingOf[S]) :SQLWriteForm[S] =
			MappingWriteForm[S, mapping.Origin](op, mapping.refine, apply(mapping :mapping.type))
	}



	object ColumnFilter {

		def apply(pred :Mapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def test[O](column :MappingAt[O]) :Boolean = pred(column)
			}


		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingAt[O]): Boolean = buff.active(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingAt[O]): Boolean = buff.inactive(column)
		}

		class ReadFilter(buff :BuffType) extends WithoutBuff(buff) {
			override def write[S](op :WriteOperationType, mapping :MappingOf[S]) :SQLWriteForm[S] =
				SQLWriteForm.unsupported(s"$this.write for $mapping")
		}

		class WriteFilter(buff :BuffType) extends WithoutBuff(buff) {
			override def read[S](mapping: MappingOf[S]) =
				SQLReadForm.unsupported(s"$this: read for $mapping")
		}


		case object ForSelect extends ReadFilter(NoSelectByDefault) {
			override def read[S](mapping: MappingOf[S]) :SQLReadForm[S] = mapping.selectForm
		}

		case object ForFilter extends WriteFilter(NoFilterByDefault)

		case object ForUpdate extends WriteFilter(NoUpdateByDefault)

		case object ForInsert extends WriteFilter(NoInsertByDefault)


		case object AllColumns extends ColumnFilter {
			override def apply[S](mapping :MappingOf[S]) :Unique[mapping.Column[_]] = mapping.columns
			override def filter[O](columns :Unique[ColumnMapping[_, O]]) :Unique[ColumnMapping[_, O]] = columns
			override def test[O](column :MappingAt[O]) = true
		}
	}






	private[schema] class MappingReadForm[S, O] private[schema] //consider: making it public and using in SQLExpression for the columns
	                      (private val mapping :RefinedMapping[S, O], private val columns :Unique[ColumnMapping[_, O]],
	                       private val read :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
		extends SQLReadForm[S] //could extend CompositeReadForm, but doesn't for extra speed
	{
		private[this] val forms = columns.view.map(read).to(Array)
		override val readColumns: Int = (0 /: forms)(_ + _.readColumns)

		override def opt(res: ResultSet, position: Int): Opt[S] = {
			val columnValues = new Array[Any](forms.length)
			var i = forms.length - 1
			while (i >= 0) {
				columnValues(i) = forms(i).opt(res, position + i).orNull
				i -= 1
			}
			val pieces = ColumnValues(mapping)(ArraySeq.unsafeWrapArray(columnValues))(columns.indexOf)
			mapping.optionally(pieces)
		}

		override def nullValue: S = mapping.nullValue.value
		override def nulls :NullValue[S] = mapping.nullValue

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = 0; var c = 0; val end = forms.length
			while (c < end) {
				val form = forms(i)
				form.register(call, i)
				i += form.readColumns; c += 1
			}
		}

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :MappingReadForm[_, _] if form canEqual this =>
				form.mapping == mapping && form.columns == columns && form.columns.map(form.read) == columns.map(read)
			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + columns.hashCode

		private def mappingString = mapping.mappingName

		override def toString :String = columns.map(read).mkString(s"$mappingString{", ",", "}>")

	}




	object MappingReadForm {

		def apply[S, O](mapping :RefinedMapping[S, O], columns :Unique[ColumnMapping[_, O]],
		                forms :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm) :SQLReadForm[S] =
			new MappingReadForm[S, O](mapping, columns, forms)


		def select[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLReadForm[S] = {
			val exports = components.view.map(mapping.export(_))
			//todo: should adding an ExplicitSelect to a column of component with ExplicitSelect require explicitly
			// listing both? how to differentiate from inherited buffs?
			if (exports.exists(NoSelect.active))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
			val columns = exports.flatMap( //fixme: this excludes columns with ExplicitSelect which were included in `components`
				_.columns.view.map(mapping.export(_)).filter(NoSelectByDefault.inactive)
			).to(Unique)
			val mandatory = mapping.selectable.filter(OptionalSelect.inactive(_))
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a select form for $mapping using $components: missing mandatory columns ", ", ", "."
				))

			val extra = columns ++ ExtraSelect.Active.columns(mapping)
			new MappingReadForm[S, O](mapping, extra, _.selectForm)
		}


		def defaultSelect[S](mapping :MappingOf[S]) :SQLReadForm[S] = {
			val columns = mapping.selectedByDefault ++ ExtraSelect.Active.columns(mapping)
			new MappingReadForm[S, mapping.Origin](mapping, columns, _.selectForm)
		}

		def fullSelect[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](
				mapping, mapping.selectable ++ ExtraSelect.Active.columns(mapping)
			)


		def autoInsert[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](mapping, mapping.autoInserted)

		def autoUpdate[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](mapping, mapping.autoUpdated)

	}






	private[schema] class MappingWriteForm[S, O] private[schema]
	                      (private val op :WriteOperationType,
	                       private val mapping :RefinedMapping[S, O], private val columns :Unique[ColumnMapping[_, O]])
		extends SQLWriteForm[S] with WriteFormLiteralsBackFeed[S]
	{ //todo: non-nullable columns should throw an early exception and not leave it to the database.
//		def this(op :WriteOperationType, mapping :RefinedMapping[S, O], columns :Unique[ColumnMapping[_, O]]) =
//			this(mapping, columns, op.Extra, op.form(_))

		private[this] val fasterColumns = columns.toArray //speeds up the iteration
		private[this] val extracts = fasterColumns.map(mapping(_)) //extracts for the columns for fast access
		private[this] val forms = fasterColumns.map(_.form) //fasterColumns.map(op.form(_))
		private[this] val extras = fasterColumns.map(op.Extra.get(_).orNull)

		override lazy val split = ArraySeq.unsafeWrapArray(extracts.map { ex =>
			def compose[T](extract :ColumnMappingExtract[S, T, O]) :ColumnWriteForm[S] =
				extract.export.form compose extract
			compose(ex)
		})

		override def set(statement :PreparedStatement, position :Int, subject :S) :Unit =
			if (subject == null)
				setNull(statement, position)
			else {
//				val subject = value.asInstanceOf[mapping.Subject]
				val values = mapping.writtenValues(op, subject)
				var i = 0; val len = fasterColumns.length
				var offset = position
				while (i < len) {
					val column = fasterColumns(i)//.asInstanceOf[ColumnMapping[Any, O]]
					val form = forms(i).asInstanceOf[SQLWriteForm[Any]]
					val value = extras(i) match {
						case null => values get column
						case buff => Got(buff.value)
					}
					form.setOpt(statement, offset, value)
					offset += form.writtenColumns
					i += 1
				}
			}

		override def setNull(statement :PreparedStatement, position :Int) :Unit = {
			var i = 0; val len = forms.length
			var offset = position
			while (i < len) {
				val form = forms(i)
				form.setNull(statement, offset)
				offset += form.writtenColumns
				i += 1
			}
		}

		override def literal(subject: S, inline :Boolean): String = {
//			val subject = value.asInstanceOf[mapping.Subject]
			val res = new java.lang.StringBuilder
			if (!inline)
				res append '('
			var i = 0; val len = forms.length
			while (i < len) {
				if (i > 0)
					res append ',' append ' '
				val form = forms(i).asInstanceOf[SQLWriteForm[Any]]
				res append (extracts(i).opt(subject) match {
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

		override def nullLiteral(inline :Boolean): String = {
			val res = new java.lang.StringBuilder
			if (!inline)
				res append '('
			var i = 0; val len = forms.length
			while (i < len) {
				if (i > 0)
					res append ',' append ' '
				res append forms(i).nullLiteral(inline)
				i += 1
			}
			if (!inline)
				res append ')'
			res.toString
		}

		override val writtenColumns: Int = (0 /: forms)(_ + _.writtenColumns)


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :MappingWriteForm[_, _] if form canEqual this =>
				form.mapping == mapping && form.columns == columns && form.op == op
			case _ => false
		}

		override def hashCode :Int = (op.hashCode * 31 + mapping.hashCode) * 31 + columns.hashCode


		private def mappingString = mapping.mappingName

		override def toString :String = columns.map(op.form(_)).mkString(s"<$mappingString{", ",", "}")
	}





	object MappingWriteForm {

		def apply[S, O](op :WriteOperationType, mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]])
				:SQLWriteForm[S] =
			custom[S, O](op, mapping, components)//, op.Prohibited, op.NonDefault, op.Extra, op.form(_))

		def apply[S, O](op :WriteOperationType, mapping :RefinedMapping[S, O]) :SQLWriteForm[S] =
			default(op, mapping)


		def filter[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			apply[S, O](FILTER, mapping, components)

		def defaultFilter[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(FILTER, mapping)

		def fullFilter[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(FILTER, mapping)


		def update[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			custom(UPDATE, mapping, components)

		def defaultUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(UPDATE, mapping)

		def fullUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(UPDATE, mapping)


		def insert[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]]) :SQLWriteForm[S] =
			custom(INSERT, mapping, components)

		def defaultInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(INSERT, mapping)

		def fullInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(INSERT, mapping)



		private def custom[S, O](op :WriteOperationType,
		                         mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]])
				:SQLWriteForm[S] =
		{ //fixme: hey, this should use Mapping.writtenValues!
			val exports = components.view.map(mapping.export(_))
			if (exports.exists(op.Prohibited.active))
				throw new IllegalArgumentException(
					s"Can't create a $op write form for $mapping using $exports: ${op.Prohibited} buff present among selection."
				)
			val columns = exports.flatMap { //fixme: this excludes ExplicitXxx components/columns even if present in `components`
				c => c.columns.view.map(mapping.export(_)).filter(op.NonDefault.inactive)
			}.to(Unique)
			val mandatory = mapping.columns.filter(op.Optional.inactive)
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a $op write form for $mapping using $exports: missing mandatory columns ", ", ", "."
				))

			val extras = columns ++ op.Extra.Active.columns(mapping)
			new MappingWriteForm[S, O](op, mapping, extras)
		}

		private def default[S](op :WriteOperationType, mapping :MappingOf[S]) :SQLWriteForm[S] = {
			val columns = op.Prohibited.Inactive.columns(mapping) ++ op.Extra.Active.columns(mapping)
			new MappingWriteForm[S, mapping.Origin](op, mapping, columns)
		}

		private def full[S](op :WriteOperationType, mapping :MappingOf[S]) :SQLWriteForm[S] =
			new MappingWriteForm[S, mapping.Origin](
				op, mapping, op.columns[mapping.Origin](mapping) ++ op.Extra.Active.columns(mapping)
			)

	}


}
