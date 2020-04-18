package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.abacus.INT
import net.noresttherein.oldsql.schema.Mapping.{MappingReadForm, MappingWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{EmptyForm, NullValue}
import net.noresttherein.oldsql.schema.Buff.{AbstractValuedBuff, AutoInsert, AutoUpdate, BuffType, ExplicitSelect, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValuedBuffType}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, LabeledMapping, MappedMapping, OptionMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.MappingPath.SelfPath
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.support.MappingFrame
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import net.noresttherein.oldsql.sql.MappingFormula.FreeComponent

import scala.annotation.implicitNotFound





/** A `Mapping` describes how a scala type (declared as the member type `Subject` of this trait)
  * is decomposed into the relational format of the underlying database. In particular, it declares all the columns used,
  * but it can have a hierarchical structure of arbitrary limit, with nested components - and indeed the columns
  * themselves - being also `Mapping` implementations. It is bidirectional in its nature and invariant regarding
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
  * column are not distinct in nature, with only the columns being treated specially in some rare circumstances.
  *
  * This homomorphism and structural nature, apart from promoting reusability and encouraging data encapsulation
  * in the domain model rather than flat, simple mappings, have several implications. First, a components
  * of a component of a mapping are called sometimes subcomponents when the distinction is being made, but are
  * themselves also valid components of this mapping. It is encouraged that the assembly (and disassembly) process
  * remains strictly hierarchical, with any instance responsible only on handling the transformation between
  * its subject type and the values of its direct components. This simplifies several implementation details and
  * increases reusability, but is not always feasible and a `Mapping` can always check the value of any of its
  * subcomponents or columns in its process.
  *
  * Second, as a component type can easily appear several times as part
  * of a larger mapping (a business having several addresses of different types, or a person having several phone
  * numbers), they naturally must use different column names. It is a good design principle to think ahead and
  * have reusable components accept at least a prefix/suffix to include in column names, but exceptions will always
  * exist, especially when dealing with preexisting schemas. A `Mapping` is therefore completely free to translate
  * a component it uses by modifying the column names as it sees fit. Additionally, any mapping can (at least
  * in theory) declare [[net.noresttherein.oldsql.schema.Buff Buffs]] which can modify the handling of all of its
  * subcomponents (such as making them read-only). This means that a component being part of a larger mapping can
  * exist in several versions: as declared in its own implementation, the final, effective version as used by
  * an SQL statement, and possibly any number in between. The effective, 'public' version of each component is
  * referred to as a ''export'' component, and only those public, export instances must appear in the components
  * (and columns) lists declared as part of this generic interface (as opposed to the declarations of individual
  * components). It is important to be furthermore aware that a component instance may be public/'export'
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
  * are impossible to reliably implement without asserting that the handles `Mapping` actually defines those types
  * (that is, those types are equal for all instances of the mapping type). This is done through the type aliases
  * defined here [[net.noresttherein.oldsql.schema.Mapping#Component[T] Component]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingFrom MappingFrom]] and their global, static counterparts
  * from the companion object: [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingFrom MappingFrom]],
  * [[net.noresttherein.oldsql.schema.Mapping.MappingOf MappingOf]]. For uniformity and interoperability, these
  * are all defined as structural narrowing of the trait `Mapping` itself rather than separate classes, but all
  * concrete implementations should extend at least [[net.noresttherein.oldsql.schema.GenericMapping]] instead
  * of this trait directly.
  *
  * The reasons for these distinctions are twofold: first, several types rely heavily on the infix notation
  * of two-argument type constructors, preventing them from accepting the type parameters of `GenericMapping`
  * or `TypedMapping`. Abstract type parameters of existential types such as `Component[_]` are not unified, leading
  * to absurd situations where 'obviously' equal types are not unified by the compiler, almost completely preventing
  * their type safe use. The second reason is the limitation of the type inferer which, when faces with
  * method with a signature in the form of `[M &lt;: TypedMapping[S, O], O, S](m :M)` will, when applied to
  * `m :GenericMapping[O, Int]` infer types `GenericMapping[O, Int], Nothing, Nothing` causing a compile error.
  * On the other hand, defining the type parameter as `[M &lt;: TypedMapping[_, _]]` assigns new distinct types to the
  * missing type parameters, which are not unified even with `m.Subject`/`m.Origin` itself, leading to a lot of issues.
  * This can be circumvented with implicit parameters, but at the cost of additional complexity.
  * @see [[net.noresttherein.oldsql.schema.GenericMapping]]
  * @see [[MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
  */
trait Mapping {
	/** The mapped entity type. */
	type Subject


	/** A phantom marker type denoting the origin of this mapping. It is used to statically distinguish between
	  * different instances of the same mapping class, but mapping different portions of the result set -
	  * in particular, they work like aliases for repeated occurrence of a table and its components in a joined query.
	  * In addition, it is used by the SQL DSL to ensure that an SQL expression refers only to components
	  * coming from the one particular query, preventing accidental use of other, non-joined mappings.
	  * This type should ''not'' be used for other purposes, keep values or be interpreted in any way, such as actual
	  * alias names for joined tables. All concrete `Mapping` implementations are expected to take `Origin`
	  * as a type parameter (by convention, and to leverage scala's partial kind unification, the last one).
	  * Casting a `Mapping` to a different `Origin` should be safe. It is possible however for a `Mapping` type to
	  * have other type parameters which depend on the `Origin` type, in which case simple projection on that
	  * parameter alone will not work. This is the case for example with
	  * [[net.noresttherein.oldsql.schema.SchemaMapping SchemaMapping]], which lists all its components
	  * in a type parameter, which may require additional casting. For that reason,
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] companion implicit is introduced.
	  * The library will not cast a `Mapping` directly, but rely on existence of that implicit
	  * (or a `Mapping` factory [[net.noresttherein.oldsql.schema.RowSource RowSource]]. Default implicit value for
	  * the former will cast any `MappingFrom[X]` to `MappingFrom[Y]`, which will be in most cases unsuitable.
	  * If you wish to declare that your `MappingFrom[X]` subtype can be safely projected from one origin to another,
	  * introduce an implicit `OriginProjection` in implicit scope (such as in the mapping's companion object).
	  * It will be delegated to for such projections, even when it is a no-op cast (as it should be). The simplest
	  * method however is to extend either [[net.noresttherein.oldsql.schema.Mapping.OfFreeOrigin OfFreeOrigin]]
	  * or [[net.noresttherein.oldsql.schema.Mapping.FreeOriginMapping]] which will introduce such an implicit
	  * automatically.
	  */
	type Origin

	/** A container with values for components of this mapping required to assemble the subject.
	  * It is a [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] instance parameterized with
	  * some super type of this mapping. In practice, this almost always be the singleton type of this mapping,
	  * but some generic code softens this restriction. Each `Pieces` instance is, at least in theory, dedicated
	  * to a particular component instance (its class and position in the larger mapping structure).
	  * From the type safety point of view however it is sufficient that the subject type of this mapping matches
	  * the subject type of the `Pieces`'s type parameter. This additional restriction serves solely to prevent
	  * accidental passing of an instance prepared for one mapping to another, such as a component of this mapping.
	  */
	type Pieces = ComponentValues[_ >: this.type <: Component[Subject]]

	/** An extractor of the value for some component of this mapping with the subject type `T`, which carries
	  * additionally the export version of that component (from the point of view of this mapping).
	  */
	type Selector[T] = ComponentExtractor[Subject, T, Origin]

	type AnyComponent = Component[_]

	/** Any mapping with the same origin marker type, making it a supertype of all valid component types of this mapping. */
	type Component[T] = Mapping.TypedMapping[T, Origin]

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin marker type
	  * as this instance and thus a valid subcomponent type of this mapping.
	  */
	type Column[T] = ColumnMapping[T, Origin]



//	def from[O2] :Mapping.TypedMapping[Subject, O2]

	/** Adapts the given subcomponent (a component being the end of some path of components starting with this instance)
	  * to its public form as present in the `subcomponents` and `components` list. For every valid transitive
	  * subcomponent of this mapping, there is exactly one equal to its export form on the `subcomponents` list.
	  * This process can modify the mapping definition by changing names of the columns (prefixing them) and updating
	  * their buffs by cascading the buffs present on this instance.
	  * By default it returns the `export` property of the selector for the component returned by
	  * [[net.noresttherein.oldsql.schema.Mapping#apply[T](component:Component[T]) apply(component)]].
	  */
	def export[T](component :Component[T]) :Component[T] = apply(component).export

	/** Adapts the given column of one of the transitive subcomponents of this instance to the form present
	  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
	  * exactly one equal to its export version on the `columns` list. This process can change the name and buffs
	  * of the column, reflecting additional information present in this mapping.
	  * By default this is an identity operation as no adaptation of subcomponents takes place.
	  */
//	def export[T](column :Column[T]) :Column[T] = column



	/** Retrieves the [[net.noresttherein.oldsql.schema.ComponentExtractor ComponentExtractor]]
	  * for the given component.
	  * @throws NoSuchElementException if `component` is not a subcomponent of this mapping.
	  */
	def apply[T](component :Component[T]) :Selector[T]



	/** Direct component mappings of this mapping, including any top-level columns. Always empty for columns.
	  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
	  * or mapped components and aliases for other components of the `Mapping`. For all non-column components however
	  * this list will cover all columns defined by the mapping.
	  */
	def components :Unique[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared by it's components or
	  * other subcomponents), or all that this mapping cares to expose, as instances of this.Component[_].
	  * This list should include all mapped columns. It is typically defined by recursive ''flat mapping''
	  * over the `components` list and including the direct components themselves. This list is always empty
	  * for columns, thus ending any similar recursion.
	  * Some mappings may wish not to expose some of the components they define, primarily in the case of adapted
	  * or mapped components and aliases for other components of the `Mapping`. For all non-column components however
	  * this list will cover all columns defined by the mapping.
	  */
	def subcomponents :Unique[Component[_]]


	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only,
	  * write-only and so on. Column mappings returns a singleton list containing themselves. */
	def columns :Unique[Component[_]]

	/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` buff). */
	def selectable :Unique[Component[_]] = columnsWithout(NoSelect)

	/** All columns which can be part of an SQL statement's where clause (don't have the `NoQuery` buff enabled). */
	def queryable :Unique[Component[_]] = columnsWithout(NoQuery)

	/** All columns which can be updated on existing database records (don't have the `NoUpdate` buff enabled). */
	def updatable :Unique[Component[_]] = columnsWithout(NoUpdate)

	/** All columns which are updated by the database on each update statement (have the `AutoUpdate` buff)
	  * and could/should be returned to the application). */
	def autoUpdated :Unique[Component[_]] = columnsWith(AutoUpdate)

	/** All columns which can occur in an insert statement (don't have the `NoInsert` buff enabled). */
	def insertable :Unique[Component[_]] = columnsWithout(NoInsert)

	/** Columns autogenerated by the database on insert (have the `AutoInsert` buff); this implies being non-insertable. */
	def autoInserted :Unique[Component[_]] = columnsWith(AutoInsert)

	/** All columns with a given buff enabled. */
	def columnsWith(buff :BuffType) :Unique[Component[_]] =
		columns.filter(buff.enabled)

	/** All columns without the given buff. */
	def columnsWithout(buff :BuffType) :Unique[Component[_]] =
		columns.filter(buff.disabled)



	def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

	def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

	def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]

	def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject]



	/** Read form of a select statement for this mapping including the given components of this mapping.
	  * The list provided here can include any components, not just the columns.
	  */
	def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject]

	/** Default read form (included columns) of a select statement for this mapping. */
	def selectForm :SQLReadForm[Subject]

	def queryForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included parameters) of a query filter for this mapping. */ //todo: PK? all?
	def queryForm :SQLWriteForm[Subject]

	def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) of update statements for this mapping. */
	def updateForm :SQLWriteForm[Subject]

	def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject]

	/** Default write form (included columns) of insert statements for this mapping. */
	def insertForm :SQLWriteForm[Subject]

//	/** A read form containing only the columns of this mapping selected by the given filter. */
//	def readForm(filter :ColumnFilter) :SQLReadForm[Subject]
//
//	/** A write form containing only the columns of this mapping selected by the given filter. */
//	def writeForm(filter :ColumnFilter) :SQLWriteForm[Subject]



	/** Optional flags and annotations modifying the way this mapping is used. They are primarily used
	  * to include or exclude columns from a given type of SQL statements. For this reason they typically appear
	  * only on columns. If a larger component declares a buff, it is typically inherited by all its subcomponents
	  * (and, transitively, by all columns). This iss not a strict requirement however, and custom mapping
	  * implementations can interpret attached buffs as pertaining only to the mapping itself, without affecting
	  * its subcomponents. As those subcomponents can in turn have their own buffs attached, care needs to be taken
	  * in order to avoid conflicts and undesired interactions.
	  */
	def buffs :Seq[Buff[Subject]]



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
	  * By default it forwards the call to the [[net.noresttherein.oldsql.schema.ComponentValues.result result]] method
	  * of `ComponentValues` (which, by default, will first check if it has a predefined value stored for this mapping,
	  * and, only if not, forward to this instance's [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  * method which is responsible for the actual assembly of the subject from the values of the subcomponents,
	  * recursively obtained from `pieces`.
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
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.value ComponentValues.value]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.getValue ComponentValues.getValue]]
	  */
	def assemble(pieces :Pieces) :Option[Subject]

	def nullValue :Option[Subject] //todo: why do we need it at all?


	/** Extract the value for the given component from the given `Subject` instance. */
	def pick[T](component :Component[T], subject :Subject) :Option[T] = apply(component).get(subject)

	/** Extracts - or assembles - the value for the given component from the given `Pieces` instance. */
	def pick[T](component :Component[T], pieces :Pieces) :Option[T] = pieces.get(apply(component))

	/** Extract the value for the given component from the given `Subject` instance. */
	def apply[T](component :Component[T], subject :Subject) :T = apply(component)(subject)

	/** Extracts - or assembles - the value for the given component from the given `Pieces` instance. */
	def apply[T](component :Component[T], pieces :Pieces) :T = pieces(apply(component))



	/** The SQL/DDL-related name associated with this mapping. This must be defined for all column and table
	  * mappings, but is typically empty in all other cases.
	  */
	def sqlName :Option[String] = None

	/** An adapter of this mapping with the names of all its public columns prefixed with the given string.
	  * This is equivalent to `prefixed(prefix + ".")` unless prefix is an empty string, in which case it is
	  * a no-op (or at least returns a mapping with exact same column nams).
	  */
	def qualified(prefix :String) :Component[Subject]

	/** An adapter of this mapping with the names of all its public columns prefixed with the given string. */
	def prefixed(prefix :String) :Component[Subject]

	def renamed(name :String) :Component[Subject]

	/** Lifts this mapping by encapsulating the subject values in an `Option`. The created mapping will
	  * always return `Some(x)` from its `optionally` and `assemble` methods, where `x` is the value returned by
	  * the method of this instance. This means that `Some(None)` is returned when this component has no value
	  * associated in a given `Pieces` instance, and that `apply(Pieces)` method of the returned mapping will
	  * never fail. This instance is exposed as the public `get` field of the returned mapping, allowing direct
	  * access to any components definitions in this mapping.
	  */
	def inOption :OptionMapping[this.type, Subject, Origin] = OptionMapping(this)

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will b;;e returned.
	  */
	def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[this.type, Subject, X, Origin](this, there, back)

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If `there` or this mapping's `optionally` subject constructor returns `None`,
	  * implicitly provided here `NullValue.value` will be returned. If `back` returns `None`, `null` value will
	  * be written in the database.
	  */
	def flatMap[X](there :Subject => Option[X], back :X => Option[Subject])
	              (implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping.opt[this.type, Subject, X, Origin](this, there, back)



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
					res ++= "\n" ++= ident ++= mapping.toString //print basic mappping info
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






/** The de facto base trait of all `Mapping` implementations.
  *
  * `Mapping` remains the main outside interface
  * as it allows easy parameterizing with the mapping type without the extra `Origin` and `Subject` type parameters,
  * but it severely limits available options of what is possible to do, especially when several instances with same or
  * related types are available. While extending it is not currently strictly required and the library just demands
  * that the two member types are defined on a `Mapping` via the  `TypedMapping[S, O]` type alias narrowing the `Mapping`,
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
trait GenericMapping[S, O] extends Mapping { self =>
	type Origin = O
	type Subject = S

	def buffs :Seq[Buff[S]] = Nil



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this :this.type, components)

	override def queryForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.query(this :this.type, components)

	override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.update(this :this.type, components)

	override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
		MappingWriteForm.insert(this :this.type, components)

	override def selectForm: SQLReadForm[S] = MappingReadForm.defaultSelect(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.defaultQuery(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.defaultUpdate(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.defaultInsert(this)
	//fixme: ColumnFilter needs to know what type of read/write column form to use
//	override def writeForm(filter :ColumnFilter) :SQLWriteForm[S] = filter.write(this)
//	override def readForm(filter :ColumnFilter) :SQLReadForm[S] = filter.read(this)



	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
		CustomizedMapping.select(this :this.type, include, exclude)

	override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
		CustomizedMapping.query(this :this.type, include, exclude)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
		CustomizedMapping.update(this :this.type, include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]] = Nil) :Component[Subject] =
		CustomizedMapping.insert(this :this.type, include, exclude)



	override def apply(pieces: Pieces): S =
		optionally(pieces) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $pieces")
		}

	//fixme: a huge issue here: buffs can cascade, but it generally will be the original implementation (without buffs)
	//fixme: which will be used for assembly. What are we going to do about it?
	override def optionally(pieces: Pieces): Option[S] = //todo: perhaps extract this to MappingReadForm for speed (not really needed as the buffs cascaded to columns anyway)
		pieces.result(this) map { res => (res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) } } orElse
			OptionalSelect.Value(this) orElse ExtraSelect.Value(this)

	def assemble(pieces :Pieces) :Option[S]

	def nullValue :Option[S] = None


	def qualified(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else prefixed(prefix + ".")

	def prefixed(prefix :String) :Component[S] =
		if (prefix.length == 0) this
		else PrefixedMapping(prefix, this)

	def renamed(name :String) :Component[S] =
		if (sqlName.contains(name)) this
		else RenamedMapping(name, this)

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
trait RootMapping[S, O] extends GenericMapping[S, O] {
	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased { c => apply[c.Subject](c).export })

	//todo:
//	def mappingName :String
//	override def sqlName = Some(mappingName)
}






object Mapping {

	@inline
	implicit def mappingSelfPath[M <: Mapping, X <: TypedMapping[S, O], S, O]
	                            (mapping :M)(implicit help :Conforms[M, X, TypedMapping[S, O]]) :SelfPath[X, S, O] =
		SelfPath.typed[X, S, O](mapping)



//	implicit def mappingSQLFormula[M[A] <: MappingFrom[A], I <: INT :ValueOf](mapping :M[_ <: Index[I]]) :FreeComponent[M, @#[I]] =
//		new FreeComponent(mapping, valueOf[I])



	/** Adds a right-associative method `@:` to any `Mapping` with well defined `Origin` and `Subject` types,
	  * which attaches a label type to it by wrapping it in `L @: M`.
	  */
	implicit class MappingLabelling[M <: TypedMapping[_, _]](private val self :M) extends AnyVal {
		/** Attaches a label (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		def @:[L <: Label](label :L) :L @: M = LabeledMapping[L, M, M#Subject, M#Origin](label, self)
	}







	/** Adds a `withOrigin[B]()` method to any `Mapping` with a well defined `Origin` type, which substitutes its origin
	  * to type `B`.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  */
	implicit class MappingAliasing[M <: Mapping](private val self :M) extends AnyVal {
		/** Substitutes the `Origin` type of this mapping to type `B`. Follow this method call with `()`
		  * to apply the result and infer the result type. The returned mapping type is provided by an implicit
		  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]], see its documentation for
		  * more information.
		  */
		@inline def withOrigin[B] = new ApplyOriginProjection[M, B](self)
	}

	/** An applicable wrapper over mapping `M` which converts it to `Origin` type `B` by the use of an implicit
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]].
	  */
	class ApplyOriginProjection[M <: Mapping, B](private val self :M) extends AnyVal {
		@inline def apply[A, R <: Mapping]()(implicit alias :OriginProjection[M, A, R, B]) :R = alias(self)
	}



	/** Converts a mapping `M` with `Origin=A` to a mapping with `Origin=B`. While the erased value itself doesn't
	  * generally need any conversion other than casting from erased type parameter `A` to `B`, the type
	  * of the aliased mapping depends on the specific `Mapping` subclass.  Default conversion will produce only
	  * the generic `Mapping` and `GenericMapping` type instances as the aliased types. If you wish
	  * your custom mapping class be retained in the aliased type, provide your own implicit value of this class
	  * in the mapping companion's object (or another related scope, with the option of explicit import).
	  * In most cases, such as with `GenericMapping[S, O]`, this is a simple erased casting
	  * `(_ :M[A]).asInstanceOf[M[B]]`. A convenience method `OriginProjection[M, A, B]() :OriginProjection[M[A], A, M[B], B]`
	  * can be then used for the implicit value. Furthermore, if the `Origin` type parameter is the last type parameter
	  * of the class, all type parameters can be inferred from the expected return type:
	  * {{{
	  *     abstract class Table[E, O] extends GenericMapping[E, O]
	  *
	  *     implicit def TableAlias[A, B] :OriginProjection[Table[E, A], A, Table[E, B], B] = OriginProjection()
	  * }}}
	  *
	  * Note however that this conversion should replace all occurrences of the origin type `A` with `B` in the
	  * aliased type's definition. If the type refers to its `Origin` in its other type arguments, those arguments
	  * should be rewritten, too: in that case the above example is not sufficient.
	  * Alternatively, a `Mapping` can extend [[net.noresttherein.oldsql.schema.Mapping.OfFreeOrigin OfFreeOrigin]]
	  * (or [[net.noresttherein.oldsql.schema.Mapping.FreeOriginMapping]] to declare that they can be safely cast
	  * from one origin type (which must be their last type parameter) to another.
	  * This automatically will introduce an implicit `MappingAlias[M[X1, X2,..., A], A, M[X1, X2, ..., B], B]`.
	  */
	@implicitNotFound("Cannot alias mapping ${X} from origin ${A} as ${Y} from ${B}. " +
	                  "If the aliased mapping type depends on the Origin type, provide your own implicit aliasing" +
	                  "OriginProjection[X, A, Y, B] in the mapping's companion object.")
	abstract class OriginProjection[-X <: Mapping, A, +Y <: Mapping, B] extends (X => Y)

//	def OriginProjection[M <: MappingFrom[O], O, AM <: MappingFrom[A], A](alias :M => AM) :OriginProjection[M, O, AM, A] =
//		alias(_)

	@inline def OriginProjection[M[O] <: MappingFrom[O], A, B]() :OriginProjection[M[A], A, M[B], B] = AnyOrigin()


	private[oldsql] def AnyOrigin[M <: MappingFrom[O], O, AM <: MappingFrom[A], A]() =
		CastingProjection.asInstanceOf[OriginProjection[M, O, AM, A]]

	private[this] final val CastingProjection :OriginProjection[Mapping, Any, Mapping, Any] = mapping => mapping

	@inline implicit def BaseMappingProjection[O, A] :OriginProjection[MappingFrom[O], O, MappingFrom[A], A] =
		AnyOrigin()

	@inline implicit def TypedMappingProjection[S, O, A] :OriginProjection[TypedMapping[S, O], O, TypedMapping[S, A], A] =
		AnyOrigin()

	@inline implicit def GenericMappingProjection[S, O, A] :OriginProjection[GenericMapping[S, O], O, GenericMapping[S, A], A] =
		AnyOrigin()

	@inline implicit def FreeOriginProjection[M[O] <: OfFreeOrigin[O], A, B] :OriginProjection[M[A], A, M[B], B] =
		AnyOrigin()


	/** A marker `Mapping` base trait declaring that extending classes can be safely cast from one `O` type argument
	  * to another and that it substitutes all references to the `Origin` member types in all its fields and
	  * other type parameters. Extending this trait will automatically introduce a
	  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] implicit value to perform the
	  * projection. There is a subtype `FreeOriginMapping[S, O]` which can  be used as a replacement base type
	  * for `GenericMapping` which performs the same function.
	  */
	trait OfFreeOrigin[O] extends Mapping {
		type Origin = O
	}

	/** A base trait for mappings which do not interpret their origin type in any way, in particular never handle
	  * any values of that type, and casting of which from their `Origin` type parameter (which must come last)
	  * to another is safe and completely converts the mapping to another origin, i.e. replaces all occurrences
	  * of the old origin with the new one.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OfFreeOrigin]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.OriginProjection]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping#Origin]]
	  */
	trait FreeOriginMapping[S, O] extends GenericMapping[S, O] with OfFreeOrigin[O] {
		override type Origin = O
	}



	/** This is an implementation artifact which may disappear from the library without notice. ''Do not use
	  * it in the client code''.
	  * A sealed trait mixed-in by `GenericMapping`, which enforces that any of its (proper) subclasses,
	  * abstract or not, are also `GenericMapping` instances. Many generic methods and classes require
	  * their `Mapping` type parameter to have definitions of both or one of `Origin` and `Subject`.
	  * Unfortunately, declaring `[M &lt;: TypedMapping[_, _]]` leads to `M#Origin`, `M#Subject` being instantiated
	  * early as unique types, not unifiable with types `O, S` where `C &lt;: TypedMapping[S, O]]` is the type argument
	  * for `M`. Somewhat counterintuitively, `[M &lt;: Mapping]` on the other hand, leads to
	  * `M#Origin =:= O` and `M#Subject =:= S` in the same circumstances. While the generic class/method will
	  * likely need to resort to casting down `M#Subject` to `m.Subject` for some `m :M`, in the interface at least
	  * it provides type safety. The only exception would be `M =:= Mapping` itself, so we simply warn
	  * against any references to it by the client application.
	  */
//	sealed trait Mapping extends Mapping


	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S`. */
	type MappingOf[S] = Mapping { type Subject = S }

	/** Narrowing of the `Mapping` trait to subtypes which define the `Origin` type as `O`.
	  * While mapping types are expected to have the `Origin` as a free type parameter, conversion from one
	  * origin type to another may require more
	  */
	type MappingFrom[O] = Mapping { type Origin = O }
	/** Narrowing of the `Mapping` trait to subtypes which define the `Subject` type as `S` and `Origin` as `O`. */
	type TypedMapping[S, O] = Mapping { type Origin = O; type Subject = S }
	type ConcreteMapping[O] = TypedMapping[_, O]

//	type MappingKind[O] = m[O] forSome { type m[A] <: MappingFrom[A] }

//	type MappingFrom[O] = { type T[S] = TypedMapping[S, O] }
//	type MappingOf[S] = { type T[O] = TypedMapping[S, O] }



//	type SingletonMapping = Mapping with Singleton
//	type SingletonOf[S] = Mapping with Singleton { type Subject = S }
//	type SingletonFrom[O] = Mapping with Singleton { type Origin = O }
//	type TypedSingleton[S, O] = TypedMapping[S, O] with Singleton //Mapping with Singleton { type Origin = O; type Subject = S }


	type CompatibleMapping[M <: Mapping] = Mapping {
		type Origin = M#Origin
		type Subject = M#Subject
	}

//	type TypeCompatibleMapping[M <: Mapping] = Mapping {
//		type Subject = M#Subject
//	}

	type ConcreteSubclass[M <: Mapping] = M {
		type Origin = M#Origin
		type Subject = M#Subject
	}






	trait ColumnFilter {
		def apply[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
			mapping.columns.filter(test[mapping.Origin])

		def filter[O](columns :Unique[MappingFrom[O]]) :Unique[MappingFrom[O]] =
			columns.filter(test[O])

		def test[O](column :MappingFrom[O]) :Boolean

		def read[S](mapping :MappingOf[S]) :SQLReadForm[S] =
			MappingReadForm[S, mapping.Origin](mapping, apply(mapping :mapping.type))

		def write[S](mapping :MappingOf[S])(forms :mapping.AnyComponent => SQLForm[_]) :SQLWriteForm[S] =
			MappingWriteForm[S, mapping.Origin](mapping, apply(mapping :mapping.type), forms)
	}



	object ColumnFilter {

		def apply(pred :Mapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def test[O](column :MappingFrom[O]) :Boolean = pred(column)
			}



		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingFrom[O]): Boolean = buff.enabled(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :MappingFrom[O]): Boolean = buff.disabled(column)
		}

		class WriteFilter(modifier :BuffType) extends WithoutBuff(modifier) {
			override def read[S](mapping: MappingOf[S]) =
				EmptyForm(throw new UnsupportedOperationException(s"$this: read for $mapping"))
		}



		case object ForSelect extends WithoutBuff(NoSelectByDefault) {
			override def read[S](mapping: MappingOf[S]) :SQLReadForm[S] = mapping.selectForm
//			override def write[S](mapping: Mapping[S]) :SQLWriteForm[Any] = SQLWriteForm.empty
		}

		case object ForQuery extends WriteFilter(NoQueryByDefault) {
//			override def write[S](mapping :Mapping[S]) :SQLWriteForm[S] = mapping.queryForm
		}

		case object ForUpdate extends WriteFilter(NoUpdateByDefault) {
//			override def write[S](mapping :Mapping[S]) :SQLWriteForm[S] = mapping.updateForm
		}

		case object ForInsert extends WriteFilter(NoInsertByDefault) {
//			override def write[S](mapping: Mapping[S]) :SQLWriteForm[S] = mapping.insertForm
		}


		case object AllColumns extends ColumnFilter {
			override def apply[S](mapping :MappingOf[S]) :Unique[mapping.Component[_]] = mapping.columns
			override def filter[O](columns :Unique[MappingFrom[O]]) :Unique[MappingFrom[O]] = columns
			override def test[O](column :MappingFrom[O]) = true
		}
	}






	private class MappingReadForm[S, O] private[Mapping]
			(mapping :TypedMapping[S, O], columns :Unique[MappingFrom[O]], read :MappingFrom[O]=>SQLReadForm[_] = (_:MappingFrom[O]).selectForm)
		extends SQLReadForm[S]
	{
		private[this] val columnArray = columns.toArray
		override val readColumns: Int = (0 /: columns)(_ + read(_).readColumns)

		override def opt(position: Int)(res: ResultSet): Option[S] = {
			var i = position //consider: not precompute the values (which wraps them in Option) but read on demand.
			val columnValues = columnArray.map { c => i += 1; read(c).opt(i - 1)(res) }
			val pieces = ComponentValues(mapping) { comp =>
				val idx = columns.indexOf(mapping.export(comp))
				if (idx >= 0) columnValues(idx)
				else None
			}
			mapping.optionally(pieces)
		}

		override def nullValue: S = mapping.nullValue getOrElse {
			mapping.apply(ComponentValues(mapping :mapping.type) { comp =>
				val lifted = mapping.export(comp)
				if (columns.contains(lifted))
					Some(read(lifted).nullValue)
				else None
			})
		}

		private def mappingString = mapping.sqlName getOrElse mapping.unqualifiedClassName

		override def toString :String = columns.map(read).mkString(s"<$mappingString{", ",", "}")
	}






	object MappingReadForm {

		def apply[S, O](mapping :TypedMapping[S, O], columns :Unique[MappingFrom[O]],
		                forms :MappingFrom[O] => SQLReadForm[_] = (_:MappingFrom[O]).selectForm) :SQLReadForm[S] =
			new MappingReadForm[S, O](mapping, columns, forms)



		def select[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]]) :SQLReadForm[S] = {
			val columns = components.map(mapping.export(_)).flatMap(_.selectable)

			if (columns.exists(NoSelect.enabled))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
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






	private class MappingWriteForm[S, O] private (
             mapping :TypedMapping[S, O],
             columns :Unique[MappingFrom[O]],
             substitute :ValuedBuffType,
             write :MappingFrom[O] => SQLWriteForm[_])
		extends SQLWriteForm[S]
	{
//		private[this] val extras = columns.map(substitute.Value.unapply(_))(breakOut) :IndexedSeq[Option[Any]]

		override def set(position :Int)(statement :PreparedStatement, value :S) :Unit =
			if (value == null)
				setNull(position)(statement)
			else {
				val subject = value.asInstanceOf[mapping.Subject]
				var i = position
				columns foreach { c =>
					val column = c.asInstanceOf[TypedMapping[Any, O]]
					val form = write(column).asInstanceOf[SQLWriteForm[Any]]
					//extras(columns.indexOf(column))
					form.setOpt(i)(statement, substitute.Value(column) orElse mapping(column).get(subject))
					i += form.writtenColumns
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
			val literals = columns.toSeq.map { c =>
				val comp = c.asInstanceOf[mapping.Component[Any]]
				val form = write(c).asInstanceOf[SQLWriteForm[Any]]
				mapping(comp).get(subject) match {
					case Some(literal) =>
						if (literal == null) form.nullLiteral(inline)
						else form.literal(literal, inline)
					case _ => form.nullLiteral(inline)
				}
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


		override def toString :String = columns.map(write).mkString(s"$mapping{", ",", "}>")
	}





	object MappingWriteForm {

		def apply[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]],
		                write :MappingFrom[O] => SQLWriteForm[_],
		                replacement :ValuedBuffType = AbstractValuedBuff) :SQLWriteForm[S] =
			new MappingWriteForm[S, O](mapping, components, replacement, write)


		def query[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]],
		                write :MappingFrom[O] => SQLWriteForm[_] = (_:MappingFrom[O]).queryForm) :SQLWriteForm[S] =
			custom(mapping, components, NoQuery, ExtraQuery, write)

		def defaultQuery[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoQuery, ExtraQuery, _.queryForm)

		def fullQuery[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.queryable, ExtraQuery, _.queryForm)



		def update[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]],
		                 write :MappingFrom[O] => SQLWriteForm[_] = (_:MappingFrom[O]).updateForm) :SQLWriteForm[S] =
			custom(mapping, components, NoUpdate, ExtraUpdate, write)

		def defaultUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoUpdate, ExtraUpdate, _.updateForm)

		def fullUpdate[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraUpdate, _.updateForm)



		def insert[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]],
		                 write :MappingFrom[O] => SQLWriteForm[_] = (_:MappingFrom[O]).insertForm) :SQLWriteForm[S] =
			custom(mapping, components, NoInsert, ExtraInsert, write)

		def defaultInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			default(mapping)(NoInsert, ExtraInsert, (_:mapping.AnyComponent).insertForm)

		def fullInsert[S](mapping :MappingOf[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraInsert, _.insertForm)



		private def custom[S, O](mapping :TypedMapping[S, O], components :Unique[MappingFrom[O]],
		                         prohibit :BuffType, mandatory :ValuedBuffType,
		                         write :MappingFrom[O] => SQLWriteForm[_]) :SQLWriteForm[S] =
		{
			if (components.exists(prohibit.enabled))
				throw new IllegalArgumentException(
					s"Can't create a write form for $mapping using $components: $prohibit buff present among selection"
				)
			val extra = components.flatMap(mapping.export(_).columns.toSeq.filter(prohibit.disabled) ++ mandatory.Enabled(mapping))
			new MappingWriteForm[S, O](mapping, extra, mandatory, write)
		}

		private def default[S](mapping :MappingOf[S])(filterNot :BuffType, mandatory :ValuedBuffType,
		                                              write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] = {
			val columns = filterNot.Disabled(mapping) ++ mandatory.Enabled(mapping)
			new MappingWriteForm[S, mapping.Origin](mapping, columns, mandatory, write)
		}

		private def full[S](mapping :MappingOf[S])(columns :Unique[mapping.AnyComponent], extra :ValuedBuffType,
		                                           write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] =
			new MappingWriteForm[S, mapping.Origin](mapping, columns ++ extra.Enabled(mapping), extra, write)

	}



}
