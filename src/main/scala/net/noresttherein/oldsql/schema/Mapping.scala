package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import scala.annotation.implicitNotFound
import scala.collection.immutable.ArraySeq

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, MappingReadForm, MappingSeal, MappingWriteForm, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.{EmptyForm, NullValue}
import net.noresttherein.oldsql.schema.Buff.{AbstractValueBuff, AutoInsert, AutoUpdate, BuffType, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValueBuffType}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, LabeledMapping, MappedMapping, OptionMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.MappingPath.ComponentPath
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.support.MappingFrame
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{FunctorProjection, ProjectsAs}
import net.noresttherein.oldsql.schema.bits.OptionMapping.Optional
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.FromClause
import net.noresttherein.oldsql.sql.FromClause.TableShift
import net.noresttherein.oldsql.sql.MappingSQL.FreeComponent


//implicits
import net.noresttherein.oldsql.collection.Unique.implicitUnique






/** A `Mapping` describes how a scala type (declared as the member type `Subject` of this trait)
  * is decomposed into the relational format of the underlying database. In particular, it declares all the columns used,
  * but it can have a hierarchical structure of arbitrary level, with nested components - and indeed the columns
  * themselves, being also `Mapping` implementations. It is bidirectional in its nature and invariant regarding
  * its subject type, but it is entirely possible to provide read-only (or write-only) implementations and mechanisms
  * exist to define which columns are included in which operations, allowing to automatically omit some from all INSERT
  * or UPDATE statements for example. In fact, the choice can be quite arbitrary and done in a case by case basis,
  * for example having `BLOB` columns excluded by default from SELECT statements and requiring their explicit
  * mention. This makes it different from [[net.noresttherein.oldsql.schema.SQLForm SQLForm]], which is used for actual
  * interacting with the JDBC `ResultSet` and `PreparedStatement` API, but uses a fixed, strictly positional
  * column schema and does not carry any information about column names or other meta data.
  *
  * `Mapping` instances used by this mapping to handle separate fragments of the mapped type are referred to as
  * ''Components'' throughout the library; it is important to note however that a `Mapping` for a database table,
  * for a whole query row of multiple joined tables, an 'Address' component of a handful of columns and a single
  * column are not distinct in nature, with only the columns being treated specially in some circumstances.
  *
  * This homomorphism and structural nature, apart from promoting reusability and encouraging data encapsulation
  * in the domain model rather than flat mappings, closely following the table schema, have several implications.
  * First, a components of a component of a mapping are called sometimes subcomponents when the distinction
  * is being made, but are themselves also valid components of this mapping. It is encouraged that the assembly
  * (and disassembly) process remains strictly hierarchical, with any instance responsible only on handling
  * the transformation between its subject type and the values of its direct components. This simplifies several
  * implementation details and increases reusability, but is not always feasible and a `Mapping` can always check
  * the value of any of its subcomponents or columns in its process.
  *
  * Second, as a component type can easily appear several times as a part
  * of a larger mapping (a business having several addresses of different types, or a person having several phone
  * numbers), they naturally must use different column names. It is a good design principle to think ahead and
  * have reusable components accept at least a prefix/suffix to include in column names, but exceptions will always
  * exist, especially when dealing with preexisting schemas. A `Mapping` is therefore completely free to translate
  * a component it uses by modifying the column names as it sees fit. Additionally, any mapping can  declare
  * [[net.noresttherein.oldsql.schema.Buff Buffs]] which can modify the handling of all of its subcomponents
  * (such as making them read-only). Their handling is the responsibility of the mapping class and not all components
  * support it. A logical component being part of a larger mapping can thus exist in several versions:
  * the original instance of the implementing class, the operative version with modifications applied by the
  * entity mapping, as used by an SQL statement, and possibly any number in between. The operative, 'public' version
  * of each component is referred to as a ''export'' component, and only those public, export instances must appear
  * in the components (and columns) lists declared as part of this generic interface (as opposed to the declarations
  * of individual components). It is important to be furthermore aware that a component instance may be public/'export'
  * from a point of view of the enclosing mapping, but not the root mapping. The library performs this translation
  * automatically when passing on the `Pieces` with selected values and several base classes exist fully handling
  * this translation under scenes, but it is important to remember when implementing custom `Mapping`s
  * with new functionality.
  *
  * This is the root interface of the mapping class hierarchy, used almost exclusively throughout the library
  * (with subclasses, with the exception of adapters exposing the adapted mapping publicly, simply providing different
  * implementations or features directed 'inwards', towards the implementing classes, rather than providing additional
  * public interface. Every concrete mapping however needs to define two types: the mapped scala type `Subject`
  * and a marker phantom type `Origin` which serves solely to introduce static type distinction between several
  * instances of the same component type but coming from different sources. In fact, many generic operations
  * are impossible to reliably implement without asserting that the handled `Mapping` actually defines those types
  * (that is, those types are equal for all instances of the mapping type). This is done through the type alias
  * defined here [[net.noresttherein.oldsql.schema.Mapping#Component[T] Component]], and global, static refinements
  * from the companion object: [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingAt MappingAt]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingOf MappingOf]]. For uniformity and interoperability, these
  * are all defined as structural narrowing of the trait `Mapping` itself rather than separate classes, but all
  * concrete implementations should extend at least [[net.noresttherein.oldsql.schema.TypedMapping]] instead
  * of this trait directly.
  *
  * The reasons for these distinctions are threefold: first, several types rely heavily on the infix notation
  * of two-argument type constructors, preventing them from accepting the type parameters of `TypedMapping`
  * or `RefinedMapping`. Abstract type parameters of existential types such as `Component[_]` are not unified, leading
  * to absurd situations where 'obviously' equal types are not unified by the compiler, almost completely preventing
  * their type safe use. This is particularly visible with classes with a type parameter `C[M &lt;: RefinedMapping[_, _]]`
  * or `C[M &lt;: TypedMapping[_, _]]`, defining their Origin and Subject as `M#Origin` and `M#Subject`. While from the
  * inside of the class `m.Origin =:= this.Origin` where `m :M`, when parameterized with a
  * `X &lt;: RefinedMapping[S, O]`, the classes `Subject` and `Origin` types are unrelated to `S, O`:
  * {{{
  *     trait C[M <: RefinedMapping[_, _]] extends TypedMapping[M#Subject, M#Origin]
  *     def assemble[M <: RefinedMapping[S, O], S, O](c :C[M]) :TypedMapping[S, O] = c //type clash
  * }}}
  * The second reason is the limitation of the type inferer which, when faced with
  * a method with a signature in the form of `[M &lt;: RefinedMapping[S, O], S, O](m :M)` will, when applied to
  * `m :TypedMapping[Int, O]` infer types `TypedMapping[Int, O], Nothing, Nothing` causing a compile error.
  * On the other hand, defining the type parameter as `[M &lt;: RefinedMapping[_, _]]` assigns new distinct types to the
  * missing type parameters, which are not unified even with `m.Subject`/`m.Origin` itself, leading to a lot of issues.
  * This can be circumvented with implicit parameters, but at the cost of additional complexity.
  * Finally, there is a bug in the compiler which prevents the use of a refining type constructor such as
  * `RefinedMapping` as a type parameter in some scenarios, requiring a proper class type `TypedMapping`.
  *
  * Concrete implementing classes should accept a type parameter `O` defining their `Origin` type, so without much
  * loss of generality, a type constructor `M[O] &lt;: MappingAt[O]` can be passed instead of the full mapping type.
  *
  * @see [[net.noresttherein.oldsql.schema.TypedMapping]]
  * @see [[MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
  */
trait Mapping { this :MappingSeal =>

	/** The mapped entity type. */
	type Subject


	/** A phantom marker type denoting the origin of this mapping. It is used to statically distinguish between
	  * different instances of the same mapping class, but mapping different portions of the result set -
	  * in particular, they work like aliases for repeated occurrences of a table and its components in a joined query.
	  * In addition, it is used by the SQL DSL to ensure that an SQL expression refers only to components
	  * coming from the one particular query, preventing accidental use of other, non-joined mappings.
	  * This type should ''not'' be used for other purposes, keep values or be interpreted in any way, such as actual
	  * alias names for joined tables. All concrete `Mapping` implementations are expected to take `Origin`
	  * as a type parameter (by convention, and to leverage scala's partial kind unification, the last one) for
	  * seamless use in SQL expressions.
	  *
	  * Casting a `Mapping` to a different `Origin` should be safe. In order to abbreviate the code and provide
	  * better type safety, direct casting to that effect should be avoided, and instead the implicitly available
	  * [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjection#withOrigin withOrigin]] method should
	  * be used. It relies on the existence of an implicit
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] which defines the result type
	  * of such a cast. If the type inferer can unify a mapping type `X` with some
	  * `M[O] &lt;: Mapping { type Origin = O }`, which will happen automatically for any mapping class which accepts
	  * its origin type as the last type parameter, an implicit `OriginProjection[M]` will exist, and
	  * `m.withOrigin[O] =:= M[O]` for any `m :M[_]`. If such a conversion cannot be performed or is unsuitable,
	  * the mapping class should declare its own implicit `OriginProjection` within its companion object.
	  * This makes it possible to define the result type of the cast in case a mapping class doesn't accept
	  * the `Origin` type as its last type parameter, or it can appear in more than one place in the type signature.
	  */
	type Origin

	/** A type alias for a generic `Mapping` with the same subject type as this mapping and the origin provided as
	  * the type parameter. Used in particular in expressions like `MappingOf[S]#Projection` to obtain a type
	  * constructor for mappings with definitions of both the `Subject` and the `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Component]]
	  */
	type Projection[O] = RefinedMapping[Subject, O]

	def refine :RefinedMapping[Subject, Origin] = this

	/** A type alias for the [[net.noresttherein.oldsql.schema.TypedMapping TypedMapping]] trait with the provided
	  * `Origin` type and the same `Subject` type as this mapping. Used primarily in the expression
	  * `MappingOf[S]#TypedProjection` as a sort of a curried type constructor for the `TypedMapping` trait.
	  * @see [[net.noresttherein.oldsql.schema.TypedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#TypedComponent]]
	  */
	type TypedProjection[O] = TypedMapping[Subject, O]

	/** A type alias for the [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] trait with the provided
	  * `Origin` type and the same `Subject` type as this mapping. Used primarily in the expression
	  * `MappingOf[S]#ColumnProjection` as a sort of a curried type constructor for the `ColumnMapping` trait.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Projection]]
	  */
	type ColumnProjection[O] = ColumnMapping[Subject, O]

	/** A container with values for components of this mapping required to assemble the subject.
	  * It is a [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] instance parameterized with
	  * the subject of this mapping. Each `Pieces` instance is, at least in theory, dedicated
	  * to a particular component instance (its class and position in the larger mapping structure).
	  * From the type safety point of view however it is sufficient that the subject type of this mapping matches
	  * the subject type of the `Pieces`'s type parameter.
	  */
	type Pieces = ComponentValues[Subject, Origin]

	/** An extract of the value for some component of this mapping with the subject type `T`, which carries
	  * additionally the export version of that component (from the point of view of this mapping).
	  */
	type Extract[T] = MappingExtract[Subject, T, Origin]

	/** An extract of the value for some column of this mapping with the subject type `T`, which carries
	  * additionally the export version of that column (from the point of view of this mapping).
	  */
	type ColumnExtract[T] = ColumnMappingExtract[Subject, T, Origin]



	/** Any mapping with the same origin marker type, making it a supertype of all valid component types of this mapping.
	  * It is also occasionally used as a part of the expression `MappingOf[S]#Component` as a sort of a curried type
	  * constructor for the narrowed down `RefinedMapping`, not necessarily in the context of components
	  * of any particular mapping instance.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Column]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Projection]]
	  */
	type Component[T] = RefinedMapping[T, Origin]

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin marker type
	  * as this instance and thus a valid subcomponent type of this mapping. It is also occasionally used as a part
	  * of the expression `MappingOf[S]#Column` as a sort of a curried type constructor for the `ColumnMapping` trait,
	  * which doesn't necessarily describe columns of any particular mapping instance.
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#ColumnProjection]]
	  */
	type Column[T] = ColumnMapping[T, Origin]

	/** Any `Mapping` with the same `Origin` type as this mapping and an unspecified `Subject` type.
	  * Note that it is not the same as `Component[_]`, as the latter is narrowing mandating that the mapping
	  * has the definition for the `Subject` type (which is of an unknown type). It is also not a direct
	  * analogue of `AnyColumn`, as the `ColumnMapping` trait, extending `TypedMapping` defines both
	  * the `Origin` and the `Subject` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.MappingAt]]
	  */
	type AnyComponent = MappingAt[Origin]

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin type as this mapping
	  * and unknown (but determined) `Subject` type.
	  * as this instance and thus a valid subcomponent type of this mapping.
	  * @see [[[net.noresttherein.oldsql.schema.Mapping#Column]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
	  */
	type AnyColumn = Column[_]

	/** Any [[net.noresttherein.oldsql.schema.TypedMapping TypedMapping]] with the same `Origin` type as this
	  * mapping and the provided `Subject` type. It is typically used not in the context of components of this
	  * mapping, which is the domain of the more generic `Component` member type, but as part of a pseudo curried
	  * type constructor `MappingAt[O]#TypedComponent`, to simply denote any `TypedMapping` instance with
	  * the provided `Subject` and `Origin` types.
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Component]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#TypedProjection]]
	  * @see [[net.noresttherein.oldsql.schema.TypedMapping]]
	  */
	type TypedComponent[T] = TypedMapping[T, Origin]

	/** A type alias for a dictionary mapping all components (and subcomponents) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping#extracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#ColumnExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.MappingExtract]]
	  */
	type ExtractMap = NaturalMap[Component, Extract]

	/** A type alias for a dictionary mapping all columns (including indirect) of this mapping, both their export,
	  * original, and any in between forms, to their extracts. The dictionary is type safe in regard to the components'
	  * `Subject` type, which is shared by both the key and the value of every entry.
	  * @see [[net.noresttherein.oldsql.schema.Mapping#columnExtracts]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#ExtractMap]]
	  * @see [[net.noresttherein.oldsql.schema.ColumnMappingExtract]]
	  */
	type ColumnExtractMap = NaturalMap[Column, ColumnExtract]






	/** Attempts to retrieve or assemble the value of `Subject` from the passed `ComponentValues` for this instance.
	  * Standard implementation will test several sources together with `pieces` before giving up:
	  * a ready value present for this mapping in the `pieces`, assembling the result from subcomponents and, finally,
	  * a default coming from an attached `OptionalSelect` (or related). By default it forwards to
	  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] and should stay consistent with it.
	  * Mapping implementations for concrete domain model classes should typically override `assemble` instead of
	  * this method directly.
	  * @throws NoSuchElementException if no value can be provided (`optionally` returns `None`).
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */
	def apply(pieces: Pieces): Subject =
		optionally(pieces) getOrElse {
			throw new NoSuchElementException(s"Can't assemble $this from $pieces.")
		}

	/** Attempts to retrieve or assemble the value for the mapped `Subject` from the given `ComponentValues`.
	  * This is the top-level method which can, together with passed `pieces`, produce the result in several ways.
	  * By default it forwards the call to the [[net.noresttherein.oldsql.schema.ComponentValues.assemble assemble]]
	  * method of `ComponentValues` (which, by default, will first check if it has a predefined value stored
	  * for this mapping, and, only if not, forward to this instance's
	  * [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method which is responsible for the actual
	  * assembly of the subject from the values of the subcomponents, recursively obtained from `pieces`.
	  *
	  * If all of the above fails, this method will check for a predefined value stored in an attached
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] (or related) buff if it exists.
	  * Additionally, any `AuditBuff`s present can modify the returned value and subclasses are free to
	  * handle other buffs or implement additional behaviour directly.
	  *
	  * Returning `None` signifies that the value of this mapping was not available in `pieces`, and is different
	  * from a 'null' value explicitly retrieved by the query. The two sometimes will get conflated in practice
	  * (which can be unavoidable). It is generally used for cases such as outer joins, or missing columns from
	  * the select clause. It is therefore perfectly possible for `Some(null)` (or some other representation of the
	  * `null` value for type `Subject`) to be returned. This approach is discouraged however, as it can possibly
	  * cause `NullPointerException`s higher up the stack by mappings/forms not prepared to handle `null` values
	  * (which is most common). Likewise, even if data for this mapping is missing, it may decide to return here some
	  * default 'missing' value; it is ultimately always up to the mapping implementation to handle the matter.
	  * The above distinction is complicated further by the fact that the mapped type `Subject` itself can be
	  * an `Option` type, used to signify either or both of these cases.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */
	def optionally(pieces: Pieces): Option[Subject]

	/** Attempts to assemble the value of this mapping from the values of subcomponents stored in the passed
	  * `ComponentValues`. This is the final dispatch target of other constructor methods declared here or
	  * in [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] and should not be called directly.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply apply]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.root ComponentValues.root]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.optionally ComponentValues.optionally]]
	  */
	def assemble(pieces :Pieces) :Option[Subject]


	def nullValue :NullValue[Subject]



	/** Extract the value for the given component from the given `Subject` instance. */
	def pick[T](component :Component[T], subject :Subject) :Option[T] = apply(component).get(subject)

	/** Extracts - or assembles - the value for the given component from the given `Pieces` instance. */
	def pick[T](component :Component[T], pieces :Pieces) :Option[T] = pieces.get(export(component))

	/** Extract the value for the given component from the given `Subject` instance. */
	def apply[T](component :Component[T], subject :Subject) :T = apply(component)(subject)

	/** Extracts - or assembles - the value for the given component from the given `Pieces` instance. */
	def apply[T](component :Component[T], pieces :Pieces) :T = pieces(export(component))






	/** Retrieves the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]]
	  * for the given component.
	  * @throws NoSuchElementException if `component` is not a subcomponent of this mapping.
	  */
	def apply[T](component :Component[T]) :Extract[T] = extracts(component)

	/** Retrieves the [[net.noresttherein.oldsql.schema.ColumnMappingExtract ColumnMappingExtract]]
	  * for the given column.
	  * @throws NoSuchElementException if `column` is not a subcomponent of this mapping.
	  */
	def apply[T](column :Column[T]) :ColumnExtract[T] = columnExtracts(column)


	//todo: decide definitely if it should include identity extractor for this
	def extracts :NaturalMap[Component, Extract]

	def columnExtracts :NaturalMap[Column, ColumnExtract]



	/** Adapts the given subcomponent (a component being the end of some path of components starting with this instance)
	  * to its public form as present in the `subcomponents` and `components` list. For every valid transitive
	  * subcomponent of this mapping, there is exactly one equal to its export form on the `subcomponents` list.
	  * This process can modify the mapping definition by changing names of the columns (prefixing them) and updating
	  * their buffs by cascading the buffs present on this instance.
	  * By default it returns the `export` property of the extract for the component returned by
	  * [[net.noresttherein.oldsql.schema.Mapping#apply[T](component:Component[T]) apply(component)]].
	  */
	def export[T](component :Component[T]) :Component[T] = apply(component).export

	/** Adapts the given column of one of the transitive subcomponents of this instance to the form present
	  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
	  * exactly one equal to its export version on the `columns` list. This process can change the name and buffs
	  * of the column, reflecting additional information present in this mapping.
	  * By default this is an identity operation as no adaptation of subcomponents takes place.
	  */
	def export[T](column :Column[T]) :Column[T] = apply(column).export



	/** Direct component mappings of this mapping, including any top-level columns. Always empty for columns.
	  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
	  * or mapped components and aliases for other components of the `Mapping`. For all non-column components however
	  * this list will cover all columns defined directly by the mapping. The components on the list are always
	  * the ''export'' versions, but their subcomponents are not, generally, the ''export'' versions from the point
	  * of view of this mapping.
	  */
	def components :Unique[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared by it's components or
	  * other subcomponents), or all that this mapping cares to expose, as instances of this.Component[_].
	  * It is typically defined by recursive ''flat mapping'' over the `components` list and including
	  * the direct components themselves. This list is always empty for columns, thus ending any similar recursion.
	  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
	  * or mapped components and aliases for other components of the `Mapping`. For all non-column components however
	  * this list will cover all columns defined by the mapping. The components on the list are always
	  * the ''export'' versions, but their subcomponents are not, generally, the ''export'' versions from the point
	  * of view of this mapping.
	  */
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

	/** All columns which can be part of an SQL statement's where clause (don't have the `NoQuery` buff enabled).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def queryable :Unique[Column[_]] = columnsWithout(NoQuery)

	/** All columns which can be updated on existing database records (don't have the `NoUpdate` buff enabled).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def updatable :Unique[Column[_]] = columnsWithout(NoUpdate)

	/** All columns which are updated by the database on each update statement (have the `AutoUpdate` buff)
	  * and could/should be returned to the application). All columns on the list are the ''export'' (operative)
	  * versions from the point of view of this mapping.
	  */
	def autoUpdated :Unique[Column[_]] = columnsWith(AutoUpdate)

	/** All columns which can occur in an insert statement (don't have the `NoInsert` buff enabled).
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def insertable :Unique[Column[_]] = columnsWithout(NoInsert)

	/** Columns autogenerated by the database on insert (have the `AutoInsert` buff); this implies being non-insertable.
	  * All columns on the list are the ''export'' (operative) versions from the point of view of this mapping.
	  */
	def autoInserted :Unique[Column[_]] = columnsWith(AutoInsert)

	/** All columns from the `columns` list with a given buff enabled. */
	def columnsWith(buff :BuffType) :Unique[Column[_]] =
		columns.filter(buff.enabled)

	/** All columns without the given buff. */
	def columnsWithout(buff :BuffType) :Unique[Column[_]] =
		columns.filter(buff.disabled)



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
	  */
	def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

	/** A modified version of this mapping with the given components included/excluded ''by default''
	  * from the column lists used as part of the ''where'' clause of an SQL select. This modifies the filter
	  * conditions generated from SQL DSL (and higher level expressions) comparing subjects of this mapping.
	  * @param include components of this instance which should be included as part of the ''where'' clause.
	  *                Their ''export'' versions must not have the `NoQuery` buff. All members will
	  *                have their `ExplicitQuery` buff removed if present and so will their subcomponents.
	  * @param exclude components of this instance which should be excluded from the ''where'' clause.
	  *                All members must have the `OptionalQuery` buff present and will receive, together
	  *                with all their selectable subcomponents, the `NoQueryByDefault` buff.
	  * @return an adapter delegating to this mapping for assembly, but having some of its components recursively
	  *         replaced as per the rules for the `include` and `exclude` lists.
	  */
	def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

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
	  */
	def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

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
	  */
	def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]



	/** Read form of a select statement for this mapping including the given components of this mapping.
	  * The list provided here can include any components, not just the columns.
	  */
	def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject]

	/** Default read form (included columns) of a select statement for this mapping. */
	def selectForm :SQLReadForm[Subject]

	def queryForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included parameters) of a filter for a particular subject value. */ //todo: PK? all?
	def queryForm :SQLWriteForm[Subject]

	def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) of update statements for this mapping. */
	def updateForm :SQLWriteForm[Subject]

	def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) of insert statements for this mapping. */
	def insertForm :SQLWriteForm[Subject]



	/** Optional flags and annotations modifying the way this mapping is used. They are primarily used
	  * to include or exclude columns from a given type of SQL statements. For this reason they typically appear
	  * only on columns. If a larger component declares a buff, it is typically inherited by all its subcomponents
	  * (and, transitively, by all columns). This iss not a strict requirement however, and custom mapping
	  * implementations can interpret attached buffs as pertaining only to the mapping itself, without affecting
	  * its subcomponents. As those subcomponents can in turn have their own buffs attached, care needs to be taken
	  * in order to avoid conflicts and undesired interactions.
	  */
	def buffs :Seq[Buff[Subject]]

	/** A mapping like this instance but with [[net.noresttherein.oldsql.schema.Mapping#buffs buffs]] replaced
	  * with the given list. The buffs cascade to the components of the new mapping: the ''export'' version
	  * of every component from this mapping has the new buffs prepended to its list. Note that any buffs from
	  * this mapping which cascaded to any of its components as part of its initialization are ''not'' removed
	  * from that components.
	  */
//	def withBuffs(buffs :Seq[Buff[Subject]]) :Component[Subject] =
//		BuffedMapping[RefinedMapping[Subject, Origin], Subject, Origin](this, buffs :_*)



	/** The SQL/DDL-related name associated with this mapping. This must be defined for all column and table
	  * mappings, but is typically empty in all other cases.
	  */ //consider: removing this altogether, probably once we've implemented acutal table schema
	def sqlName :Option[String] = None






	//consider: all renaming/mapping methods to lose Origin in order to *not* be components of this mapping
	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string.
	  * This is equivalent to `prefixed(prefix + ".")` unless prefix is an empty string, in which case it is
	  * a no-op (or at least returns a mapping with exact same column name).
	  */
	def qualified(prefix :String) :Component[Subject]

	/** An adapter of this mapping with the names of all its ''export'' columns prefixed with the given string. */
	def prefixed(prefix :String) :Component[Subject]

	/** A Mapping with exactly the same components, buffs and implementation as this one, but with an `sqlName` equal
	  * to the given name. */
	def renamed(name :String) :Component[Subject]



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



	override def toString :String = sqlName getOrElse this.unqualifiedClassName

	def columnString :String = columns.mkString(toString + "{", ", ", "}")

	def debugString :String = {
		/** Use recursion to print the ''export'' (by this mapping) version of every subcomponent.
		  * @param ident whitespace prefix to start every new line with
		  * @param wasColumn was the last printed component a sibling column, meaning we should print this column inline
		  * @return true if `mapping` is a column.
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
					res ++= "\n" ++= ident ++= mapping.toString //print basic mapping info
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
//		components.map(c => export(c).debugString).mkString("\n" + toString + "{\n", "; ", "\n}")
	}

}






sealed abstract class LowPriorityMappingImplicits {
	@inline implicit def refinedMappingOriginProjection[S, A]
			:OriginProjection[RefinedMapping[S, A]] { type WithOrigin[O] = RefinedMapping[S, O] } =
		OriginProjection.projectAs[RefinedMapping[S, _], MappingOf[S]#Projection]

}



object Mapping extends LowPriorityMappingImplicits {

	//enforcing extension of TypedMapping is needed because of a bug in scalac where using RefinedMapping as a type parameter
	/** Self type declared by the root `Mapping` trait used to enforce that all concrete `Mapping` implementations
	  * extend `TypedMapping` without sealing the trait `Mapping` itself.
	  */
	sealed trait MappingSeal extends Mapping



	/** Adds factory methods for `MappingPath`s from the implicitly enriched `Mapping` instance to its components. */
	@inline implicit def mappingPathConstructor[X <: Mapping, M <: RefinedMapping[S, O], S, O]
	                                           (self :X)(implicit hint :Conforms[X, M, RefinedMapping[S, O]])
			:MappingPathConstructor[M, S, O] =
		new MappingPathConstructor[M, S, O](self)

	/** Adds a `\` method to any `Mapping`, creating a `ComponentPath` from it to one of its components. */
	class MappingPathConstructor[M <: RefinedMapping[S, O], S, O](private val self :M) extends AnyVal {

		/** Creates a `ComponentPath` leading from this (wrapped) mapping to its specified component. */
		def \[X <: Mapping, C <: RefinedMapping[T, O], T]
		     (component :X)(implicit hint :Conforms[X, C, RefinedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component)

		def \[X <: Mapping, C <: RefinedMapping[T, O], T]
		     (component :M => X)(implicit hint :Conforms[X, C, RefinedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))

		def apply[X <: Mapping, C <: RefinedMapping[T, O], T]
		         (component :M => X)(implicit hint :Conforms[X, C, RefinedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
			ComponentPath(self, component(self))
	}



	implicit def mappingSQLFormula[F <: FromClause, C <: Mapping, M[A] <: TypedMapping[X, A], X, N <: Numeral]
                 (mapping :C)
                 (implicit conforms :Conforms[C, M[F], TypedMapping[X, F]], offset :TableShift[F, M, N],
                  projection :FunctorProjection[M])
			:FreeComponent[F, M, X] =
		FreeComponent(mapping, offset.tables)



	/** Adds a right-associative method `@:` to any `Mapping` with defined `Origin` and `Subject` types,
	  * which attaches a label type to it by wrapping it in `L @: M`.
	  */
	implicit class MappingLabeling[M <: RefinedMapping[_, _]](private val self :M) extends AnyVal {
		/** Attaches a label (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		@inline def @:[L <: Label](label :L) :L @: M = LabeledMapping[L, M, M#Subject, M#Origin](label, self)

		/** Attaches a label `L` (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		@inline def :@[L <: Label :ValueOf] :L @: M = LabeledMapping[L, M, M#Subject, M#Origin](valueOf[L], self)
	}







	/** Adds a `withOrigin[B]()` method to any `Mapping` with a well defined `Origin` type, which substitutes its origin
	  * to type `B`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  */
	implicit class MappingOriginProjection[M <: Mapping](private val self :M) extends AnyVal {

		/** Converts this mapping to one where `type Origin = O`. The result type is determined by the implicit
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]].
		  * If `M &lt;: C[A1, ..., AN, A]` where `C[_, ..., _, O] &lt;: TypedMapping[_, O]`, then the result type will
		  * be inferred as `C[A1, ..., AN, O]`. If the origin type parameter `A` is not the last one, or the mapping
		  * defines its `Origin` type by other means entirely, type inference will fail and an implicit
		  * `OriginProjection` value with the correct `WithOrigin` type should be provided in the companion object to `M`.
		  * See the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] class documentation
		  * for a detailed information about providing custom projections and cases when it is required.
		  */
		@inline def withOrigin[O](implicit projection :OriginProjection[M]) :projection.WithOrigin[O] =
			self.asInstanceOf[projection.WithOrigin[O]]

	}



	/** Converts a mapping `M` with `type Origin = A forSome { type A }` to a mapping with `type Origin = B`, where `B`
	  * is the type parameter of the `apply` method. This conversion must be complete in the sense that in the projected
	  * mapping type `WithOrigin[B]` all references to `A` are replaced with the type `B`. If the mapping type `M`
	  * is a generic type which defines its `Origin` type to be equal to its last type parameter `O` (typically
	  * by extending `TypedMapping[_, O]`) and `O` doesn't occur anywhere else in the definition of `M`,
	  * as is the recommended practice, then `M =:= WithOrigin[A]` and type `WithOrigin` is inferred automatically
	  * by the compiler by partial unification. An implicit value of this class is in that case provided by the eponymous
	  * companion method. There are cases however when either the appropriate projection type cannot be inferred
	  * or is inferred incorrectly. Typical examples of the former include `Mapping` subclasses with their `Origin`
	  * type parameter not in the last position, or types which define their `Origin` type in terms of the `Origin` type
	  * of some value, such as the labeled mapping wrapper `N @: M`, which defines its origin type to be equal to the
	  * `Origin` of the adapted mapping. An example of the latter would be a class which requires that the `Origin` type
	  * argument occurs elsewhere in `M` for the value to be valid, such as:
	  * {{{
	  *     abstract class Adapter[+M <: Mapping, S, O](val component :M) extends TypedMapping[S, O]
	  *
	  *     val a :Adapter[TypedMapping[Int, "A"], Int, "A"] = ???
	  *     //a.component is a valid component of a
	  *     val b = a.withOrigin["B"] //b :Adapter[TypedMapping[Int, "A"], Int, "B"]
	  *     //b.component is not a valid component of b as it has a different `Origin` type.
	  * }}}
	  * Such practice is discouraged, but not prohibited as it can lead to shorter type signatures.
	  * Both situations can be amended by defining an implicit `OriginProjection[M]` in the companion object
	  * of the class of `M`. Fixing the above example would require:
	  * {{{
	  *     object Adapter {
	  *         implicit def projection[M <: Mapping, X](implicit m :OriginProjection[M])
	  *                 :OriginProjection[Adapter[M, X, _] { type WithOrigin[A] = Adapter[m.WithOrigin[A], X, A], X, A] } =
	  *             m.lift[({ type L[+T <: Mapping, A] = Adapter[T, X, A] })#L, M]
	  *     }
	  *
	  *     val c = a.withOrigin["C"] //c :Adapter[TypedMapping[Int, "C"], Int, "C"]
	  * }}}
	  *
	  * The only source of `OriginProjection` instances is the eponymous, implicit factory method and transformation
	  * methods defined in this class.
	  */
	@implicitNotFound("Cannot project mapping ${M} to another Origin type: no (unique) implicit OriginProjection[${M}].")
	sealed abstract class OriginProjection[-M <: Mapping] { self =>

		/** A type such that `M &lt;: WithOrigin[_]` which does not reference the origin type `O` in its signature
		  * anywhere except as the type parameter. In other words, a conversion `WithOrigin[A] => WithOrigin[B]`
		  * replaces every reference to `A` with the type `B`. This in particular means that all components and extracts
		  * returned by the mapping after conversion define their `Origin` type as `B`, consistently with the converted
		  * mapping.
		  */ //fixme: this should preserve the subject type
		type WithOrigin[O] <: MappingAt[O]

		/** Casts the mapping of type `M` to a type where all references to its current origin type are replaced
		  * by the `O` type.
		  */
		@inline final def apply[O](mapping :M) :WithOrigin[O] = mapping.asInstanceOf[WithOrigin[O]]


		/** A projection from any `WithOrigin[_]` to `WithOrigin[O]`. */
		@inline def isomorphism :FunctorProjection[WithOrigin] = this.asInstanceOf[FunctorProjection[WithOrigin]]

		/** Lifts a projection of mapping type `M` to one casting from mapping `T[M]` to `T[WithOrigin[O]]`.
		  * It is the responsibility of the caller to make sure that type `T` does not reference directly the origin type
		  * in its definition.
		  */
		@inline implicit def substitute[T[+_ <: Mapping] <: Mapping, S <: M]
				:OriginProjection[T[S]] { type WithOrigin[O] = T[self.WithOrigin[O]] } =
			this.asInstanceOf[OriginProjection[T[S]] { type WithOrigin[O] = T[self.WithOrigin[O]] }]

		/** Lifts a projection of mapping type `M` to one casting from mapping `T[M, _]` to `T[WithOrigin[O], O]`.
		  * It is the responsibility of the caller to make sure that type `T` does not reference directly the origin type
		  * in its definition.
		  */
		@inline def lift[T[+_ <: MappingAt[O], O] <: MappingAt[O], C <: M]
				:OriginProjection[T[C, _]] { type WithOrigin[O] = T[self.WithOrigin[O], O] } =
			this.asInstanceOf[OriginProjection[T[C, _]] { type WithOrigin[O] = T[self.WithOrigin[O], O] }]

	}



	object OriginProjection {

		def apply[M <: Mapping](implicit projection :OriginProjection[M])
				:OriginProjection[M] { type WithOrigin[O] = projection.WithOrigin[O] } =
			projection

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
		  * which is guaranteed to preserve the `Subject` type of the projected mapping.
		  */
		type RefinedProjection[M <: Mapping, S] = OriginProjection[M] { type WithOrigin[O] <: RefinedMapping[S, O] }

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]]
		  * of single-argument functor types, accepting their `Origin` type as their argument. This shortens
		  * the notation considerably, especially if `M` is a natural single-argument type constructor
		  * (and not a type lambda).
		  */
		type FunctorProjection[M[O] <: MappingAt[O]] = OriginProjection[M[_]] { type WithOrigin[O] = M[O] }

		/** Type alias for [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] which accepts
		  * its `WithOrigin` type as the second argument `P`. This shortens the notation considerably if `P`
		  * is a natural single-argument type constructor (and not a type lambda).
		  */
		type ProjectsAs[-M <: Mapping, P[O] <: MappingAt[O]] = OriginProjection[M] { type WithOrigin[O] = P[O] }



		def default[M[A] <: MappingAt[A]] :FunctorProjection[M] = CastingProjection.asInstanceOf[FunctorProjection[M]]

		private[oldsql] def projectAs[M <: Mapping, P[O] <: MappingAt[O]] :M ProjectsAs P =
			CastingProjection.asInstanceOf[M ProjectsAs P]


		private[this] final val CastingProjection :OriginProjection[Mapping] = new OriginProjection[Mapping] {
			override type WithOrigin[O] = MappingAt[O]
		}

	}



	//this is here to have clear precedence hierarchy with Mapping subtypes declaring their own projections
	@inline implicit def originProjection[M[O] <: MappingAt[O]] :M[_] ProjectsAs M = OriginProjection.default[M]







	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S`. */
	type MappingOf[S] = Mapping { type Subject = S }

	/** Narrowing of the `Mapping` trait to subtypes which define the `Origin` type as `O`.
	  * While mapping types are expected to have the `Origin` as a free type parameter, conversion from one
	  * origin type to another may require more
	  */ //todo: at is not the proper preposition associated with 'Origin', but 'of' would be misleading. Rename Origin to ...? Lineage, sphere, family, line
	type MappingAt[O] = Mapping { type Origin = O }

	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S` and `Origin` as `O`. */
	type RefinedMapping[S, O] = Mapping { type Subject = S; type Origin = O }

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






	trait ColumnFilter {
		def apply[S](mapping :MappingOf[S]) :Unique[mapping.Column[_]] =
			mapping.columns.filter(test[mapping.Origin])

		def filter[O](columns :Unique[ColumnMapping[_, O]]) :Unique[ColumnMapping[_, O]] =
			columns.filter(test[O])

		def test[O](column :MappingAt[O]) :Boolean

		def read[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			MappingReadForm[S, mapping.Origin](mapping, apply(mapping :mapping.type))

		def write[S](mapping :MappingOf[S])(forms :mapping.Component[_] => SQLForm[_]) :SQLWriteForm[S] =
			MappingWriteForm[S, mapping.Origin](mapping, apply(mapping :mapping.type), forms)
	}



	object ColumnFilter {

		def apply(pred :Mapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def test[O](column :MappingAt[O]) :Boolean = pred(column)
			}



		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingAt[O]): Boolean = buff.enabled(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingAt[O]): Boolean = buff.disabled(column)
		}

		class ReadFilter(buff :BuffType) extends WithoutBuff(buff) {
			override def write[S](mapping :MappingOf[S])(forms :mapping.Component[_] => SQLForm[_]) :SQLWriteForm[S] =
				SQLWriteForm.unsupported(s"$this.write for $mapping")
		}

		class WriteFilter(buff :BuffType) extends WithoutBuff(buff) {
			override def read[S](mapping: MappingOf[S]) =
				EmptyForm(throw new UnsupportedOperationException(s"$this: read for $mapping"))
		}



		case object ForSelect extends ReadFilter(NoSelectByDefault) {
			override def read[S](mapping: MappingOf[S]) :SQLReadForm[S] = mapping.selectForm
		}

		case object ForQuery extends WriteFilter(NoQueryByDefault)

		case object ForUpdate extends WriteFilter(NoUpdateByDefault)

		case object ForInsert extends WriteFilter(NoInsertByDefault)


		case object AllColumns extends ColumnFilter {
			override def apply[S](mapping :MappingOf[S]) :Unique[mapping.Column[_]] = mapping.columns
			override def filter[O](columns :Unique[ColumnMapping[_, O]]) :Unique[ColumnMapping[_, O]] = columns
			override def test[O](column :MappingAt[O]) = true
		}
	}






	private[schema] class MappingReadForm[S, O] private[schema]
	                      (private val mapping :RefinedMapping[S, O], private val columns :Unique[ColumnMapping[_, O]],
	                       private val read :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
		extends SQLReadForm[S]
	{
		private[this] val columnArray = columns.toArray
		override val readColumns: Int = (0 /: columns)(_ + read(_).readColumns)

		override def opt(position: Int)(res: ResultSet): Option[S] = {
			var i = position //consider: not precompute the values (which wraps them in Option) but read on demand.
			val columnValues = columnArray.map { c => i += 1; read(c).opt(i - 1)(res) }
			val pieces = ComponentValues(mapping)(ArraySeq.unsafeWrapArray(columnValues))(columns.indexOf)
			mapping.optionally(pieces)
		}

		override def nullValue: S = mapping.nullValue.value

		override def nulls :NullValue[S] = mapping.nullValue


		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :MappingReadForm[_, _] if form canEqual this =>
				form.mapping == mapping && form.columns == columns && form.columns.map(form.read) == columns.map(read)
			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + columns.hashCode

		private def mappingString = mapping.sqlName getOrElse mapping.unqualifiedClassName

		override def toString :String = columns.map(read).mkString(s"<$mappingString{", ",", "}")

	}






	object MappingReadForm {

		def apply[S, O](mapping :RefinedMapping[S, O], columns :Unique[ColumnMapping[_, O]],
		                forms :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm) :SQLReadForm[S] =
			new MappingReadForm[S, O](mapping, columns, forms)



		def select[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLReadForm[S] = {
			val columns = components.toSeq.map(mapping.export(_)).flatMap(_.selectable.map(mapping.export(_))).toUnique

			if (columns.exists(NoSelect.enabled))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
			val mandatory = mapping.selectable.filter(OptionalSelect.disabled(_))
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a select form for $mapping using $components: missing mandatory columns ", ", ", "."
				))

			val extra = columns ++ ExtraSelect.Enabled(mapping)
			new MappingReadForm[S, O](mapping, extra, _.selectForm)
		}

		def defaultSelect[S](mapping :MappingOf[S]) :SQLReadForm[S] = {
			val columns = mapping.selectable.filter(NoSelectByDefault.disabled) ++ ExtraSelect.Enabled(mapping)
			new MappingReadForm[S, mapping.Origin](mapping, columns, _.selectForm)
		}


		def fullSelect[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](mapping, mapping.selectable ++ ExtraSelect.Enabled(mapping))



		def autoInsert[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](mapping, mapping.autoInserted)

		def autoUpdate[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			new MappingReadForm[S, mapping.Origin](mapping, mapping.autoUpdated)

	}






	private[schema] class MappingWriteForm[S, O] private[schema]
	                      (private val mapping :RefinedMapping[S, O], private val columns :Unique[ColumnMapping[_, O]],
	                       private val extra :ValueBuffType, private val write :ColumnMapping[_, O] => SQLWriteForm[_])
		extends SQLWriteForm[S]
	{
		private[this] val extracts = columns.toArray.map(mapping(_))

		override def set(position :Int)(statement :PreparedStatement, value :S) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				val subject = value.asInstanceOf[mapping.Subject]
				var i = 0
				var offset = position
				columns foreach { c =>
					val column = c.asInstanceOf[ColumnMapping[Any, O]]
					val form = write(column).asInstanceOf[SQLWriteForm[Any]]
					//extras(columns.indexOf(column))
					val columnValue = extra.Value(column) match {
						case some :Some[_] => some
						case _ => extracts(i).get(subject)
					}
					form.setOpt(offset)(statement, columnValue)
					offset += form.writtenColumns
					i += 1
				}
			}

		override def setNull(position :Int)(statement :PreparedStatement) :Unit = {
			var i = position
			columns foreach { column =>
				val form = write(column)
				form.setNull(i)(statement)
				i += form.writtenColumns
			}
		}

		override def literal(value: S, inline :Boolean): String = {
			val subject = value.asInstanceOf[mapping.Subject]
			var i = 0
			val literals = columns.toSeq.map { c =>
				val form = write(c).asInstanceOf[SQLWriteForm[Any]]
				extracts(i).get(subject) match {
					case Some(literal) =>
						if (literal == null) form.nullLiteral(inline)
						else form.literal(literal, inline)
					case _ => form.nullLiteral(inline)
				}
				i += 1
			}
			if (inline) literals.mkString("", ", ", "")
			else literals.mkString("(", ", ", ")")
		}

		override def nullLiteral(inline :Boolean): String = {
			val literals = columns.toSeq.map(write(_).nullLiteral)
			if (inline) literals.mkString("", ", ", "")
			else literals.mkString("(", ", ", ")")
		}


		override def literal(value: S): String = literal(value, false)

		override def inlineLiteral(value: S): String = literal(value, true)

		override def nullLiteral: String = nullLiteral(false)

		override def inlineNullLiteral: String = nullLiteral(true)

		override val writtenColumns: Int = (0 /: columns)(_ + write(_).writtenColumns)



		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case form :MappingWriteForm[_, _] if form canEqual this =>
				form.mapping == mapping && form.columns == columns && form.extra == extra &&
					form.columns.map(form.write) == columns.map(write)
			case _ => false
		}

		override def hashCode :Int = (mapping.hashCode * 31 + extra.hashCode) * 31 + columns.hashCode


		private def mappingString = mapping.sqlName getOrElse mapping.unqualifiedClassName

		override def toString :String = columns.map(write).mkString(s"$mappingString{", ",", "}>")
	}





	object MappingWriteForm {

		def apply[S, O](mapping :RefinedMapping[S, O], columns :Unique[ColumnMapping[_, O]],
		                write :ColumnMapping[_, O] => SQLWriteForm[_],
		                replacement :ValueBuffType = AbstractValueBuff) :SQLWriteForm[S] =
			new MappingWriteForm[S, O](mapping, columns, replacement, write)


		def query[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]],
		                write :MappingAt[O] => SQLWriteForm[_] = (_:MappingAt[O]).queryForm) :SQLWriteForm[S] =
			custom(mapping, components, NoQuery, NoQueryByDefault, ExtraQuery, write)

		def defaultQuery[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoQuery, ExtraQuery, _.queryForm)

		def fullQuery[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.queryable, ExtraQuery, _.queryForm)



		def update[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]],
		                 write :MappingAt[O] => SQLWriteForm[_] = (_:MappingAt[O]).updateForm) :SQLWriteForm[S] =
			custom(mapping, components, NoUpdate, NoUpdateByDefault, ExtraUpdate, write)

		def defaultUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoUpdate, ExtraUpdate, _.updateForm)

		def fullUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraUpdate, _.updateForm)



		def insert[S, O](mapping :RefinedMapping[S, O], components :Unique[MappingAt[O]],
		                 write :MappingAt[O] => SQLWriteForm[_] = (_:MappingAt[O]).insertForm) :SQLWriteForm[S] =
			custom(mapping, components, NoInsert, NoInsertByDefault, ExtraInsert, write)

		def defaultInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoInsert, ExtraInsert, (_:mapping.Component[_]).insertForm)

		def fullInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraInsert, _.insertForm)



		private def custom[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]],
		                         prohibit :BuffType, nonDefault :BuffType, extra :ValueBuffType,
		                         write :MappingAt[O] => SQLWriteForm[_]) :SQLWriteForm[S] =
		{
			val exports = components.map(mapping.export(_))
			if (exports.exists(prohibit.enabled))
				throw new IllegalArgumentException(
					s"Can't create a write form for $mapping using $exports: $prohibit buff present among selection."
				)

			val columns = exports flatMap {
				c => c.columns.map(mapping.export(_)).filter(prohibit.disabled)
			}
			val mandatory = mapping.columns.filter(nonDefault.disabled)
			val missing = mandatory.toSet - columns.toSet
			if (missing.nonEmpty)
				throw new IllegalArgumentException(missing.mkString(
					s"Can't create a write form for $mapping using $exports: missing mandatory columns ", ", ", "."
				))

			val extras = columns ++ extra.Enabled(mapping)
			new MappingWriteForm[S, O](mapping, extras, extra, write)
		}

		private def default[S](mapping :MappingOf[S])(filterNot :BuffType, mandatory :ValueBuffType,
		                                              write :mapping.Column[_] => SQLWriteForm[_]) :SQLWriteForm[S] = {
			val columns = filterNot.Disabled(mapping) ++ mandatory.Enabled(mapping)
			new MappingWriteForm[S, mapping.Origin](mapping, columns, mandatory, write)
		}

		private def full[S](mapping :MappingOf[S])(columns :Unique[mapping.Column[_]], extra :ValueBuffType,
		                                           write :mapping.Column[_] => SQLWriteForm[_]) :SQLWriteForm[S] =
			new MappingWriteForm[S, mapping.Origin](mapping, columns ++ extra.Enabled(mapping), extra, write)

	}



}



/** The de facto base trait of all `Mapping` implementations.
  *
  * `Mapping` remains the main outside interface
  * as it allows easy parameterizing with the mapping type without the extra `Origin` and `Subject` type parameters,
  * but it severely limits available options of what is possible to do, especially when several instances with same or
  * related types are available. While extending it is not currently strictly required and the library just demands
  * that the two member types are defined on a `Mapping` via the  `RefinedMapping[S, O]` type alias narrowing the `Mapping`,
  * Doing so is the most convenient way to achieve it and provides implementations of several methods which could
  * not be done without.
  *
  * @tparam S The subject type, that is the type of objects read and written to a particular table (or a view, query,
  *           or table fragment).
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  *           Consult [[net.noresttherein.oldsql.schema.Mapping#Origin Mapping.Origin]]
  */
trait TypedMapping[S, O] extends MappingSeal { self =>
	override type Origin = O
	override type Subject = S
	//for nicer compiler output
	override type Extract[T] = MappingExtract[S, T, O]
	override type ColumnExtract[T] = ColumnMappingExtract[S, T, O]
	override type AnyComponent = MappingAt[O]
	override type Component[T] = RefinedMapping[T, O]
	override type Column[T] = ColumnMapping[T, O]



	override def apply(pieces: Pieces): S =
		optionally(pieces) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $pieces")
		}

	override def optionally(pieces: Pieces): Option[S] = pieces.assemble(this) match {
		case res if buffs.isEmpty => res //very common case
		case Some(res) => Some((res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) })
		case _ =>
			val res = OptionalSelect.Value(this)
			if (res.isDefined) res
			else ExtraSelect.Value(this)
	}

	override def assemble(pieces :Pieces) :Option[S]

	override def nullValue :NullValue[S] = NullValue.NotNull



	def apply[M >: this.type <: RefinedMapping[S, O], X <: Mapping, C <: RefinedMapping[T, O], T]
	         (component :M => X)(implicit hint :Conforms[X, C, RefinedMapping[T, O]]) :ComponentPath[M, C, S, T, O] =
		ComponentPath(this :M, component(this))



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this, components)

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.query(this, components)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.update(this, components)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.insert(this, components)

	override def selectForm: SQLReadForm[S] = MappingReadForm.defaultSelect(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.defaultQuery(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.defaultUpdate(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.defaultInsert(this)



	override def buffs :Seq[Buff[S]] = Nil



	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		CustomizedMapping.select[TypedMapping[S, O], S, O](this, include, exclude)

	override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		CustomizedMapping.query[TypedMapping[S, O], S, O](this, include, exclude)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		CustomizedMapping.update[TypedMapping[S, O], S, O](this, include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[S] =
		CustomizedMapping.insert[TypedMapping[S, O], S, O](this, include, exclude)



	override def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else prefixed(prefix + ".")

	override def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else PrefixedMapping[TypedMapping[S, O], S, O](prefix, this)

	override def renamed(name :String) :Component[S] =
		if (sqlName.contains(name)) this
		else RenamedMapping[TypedMapping[S, O], S, O](name, this)



	override def inOption :Optional[this.type] = OptionMapping.singleton(this)

	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[TypedMapping[S, O], S, X, O](this, there, back)

}






object TypedMapping {
	type * = M[O] forSome { type M[A] <: TypedMapping[_, A]; type O }

	type From[O] = TypedMapping[_, O]

	type Of[S] = TypedMapping[S, _]

	type AnyFrom[O] = M[O] forSome { type M[A] <: TypedMapping[_, A] }

	type AnyOf[S] = M[S] forSome { type M[X] <: TypedMapping[X, _] }
}






/** A `Mapping` subclass which, in its `optionally` (and indirectly `apply`) method, declares aliasing
  * of its components on the passed `Pieces`. Some mappings (and possibly their components) allow
  * declaring a column prefix to be added to all its columns, as well as additional buffs, which should be inherited
  * by all of its subcomponents (including columns), so a component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. As it is the latter version of the component
  * which is used by the framework to create any SQL statements, and thus also by the `Pieces`, but typically
  * the former is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces`
  * implementation substitutes any component passed to it with its export representation before looking for its value.
  *
  */
trait RootMapping[S, O] extends TypedMapping[S, O] {

	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased(this))

}



