package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, ConstantExtractor, EmptyExtractor, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, ComponentExtractor, ConcreteMapping, MappingReadForm, MappingWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{EmptyForm, NullValue}
import net.noresttherein.oldsql.schema.support.{LabeledMapping, MappedMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.Buff.{AbstractValuedBuff, AutoInsert, AutoUpdate, BuffType, ExplicitSelect, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValuedBuffType}
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.schema.MappingPath.{ComponentPath, SelfPath}
import net.noresttherein.oldsql.schema.support.LabeledMapping.{@:, Label}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, AdaptedAs}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth





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
  * referred to as a ''lifted'' component, and only those public, lifted instances must appear in the components
  * (and columns) lists declared as part of this generic interface (as opposed to the declarations of individual
  * components). It is important to be furthermore aware that a component instance may be public/'lifted'
  * from a point of view of the enclosing mapping, but not the root mapping. The library performs this translation
  * automatically when passing on the `Pieces` with selected values and several base classes exist fully handling
  * this translation under scenes, but it is important to remember when implementing custom `Mapping`s
  * with new functionality.
  *
  * This is the root interface of the mapping class hierarchy, used almost exclusively throughout the library
  * (with subclasses, with the exception of adapters exposing the adapted mapping publicly, simply providing different
  * implementations or features directed 'inwards', towards the implementing classes, rather than providing additional
  * public interface. Every concrete mapping however needs to define two types: the mapped scala type `Subject`
  * and a marker phantom type `Owner` which serves solely to introduce static type distinction between several
  * instances of the same component type but coming from different sources. In fact, many generic operations
  * are impossible to reliably implement without asserting that the handles `Mapping` actually defines those types
  * (that is, those types are equal for all instances of the mapping type). This is done through the type aliases
  * defined here [[net.noresttherein.oldsql.schema.Mapping#Component[T] Component]],
  * [[net.noresttherein.oldsql.schema.Mapping.AnyComponent AnyComponent]] and their global, static counterparts
  * from the companion object: [[net.noresttherein.oldsql.schema.Mapping.Component Component]],
  * [[net.noresttherein.oldsql.schema.Mapping.AnyComponent AnyComponent]],
  * [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]. For uniformity and interoperability, these
  * are all defined as structural narrowing of the trait `Mapping` itself rather than separate classes, but all
  * concrete implementations should extend at least [[net.noresttherein.oldsql.schema.GenericMapping]] instead
  * of this trait directly.
  *
  * The reasons for these distinctions are twofold: first, several types rely heavily on the infix notation
  * of two-argument type constructors, preventing them from accepting the type parameters of `GenericMapping`
  * or `Component`. Abstract type parameters of existential types such as `Component[_]` are not unified, leading
  * to absurd situations where 'obviously' equal types are not unified by the compiler, almost completely preventing
  * their type safe use. The second reason is the limitation of the type inferer which, when faces with
  * method with a signature in the form of `[M &lt;: Component[O, S], O, S](m :M)` will, when applied to
  * `m :GenericMapping[O, Int]` infer types `GenericMapping[O, Int], Nothing, Nothing` causing a compile error.
  * On the other hand, defining the type parameter as `[M &lt;: Component[_, _]]` assigns new distinct types to the
  * missing type parameters, which are not unified even with `m.Subject`/`m.Owner` itself, leading to a lot of issues.
  * This can be circumvented with implicit parameters, but at the cost of additional complexity.
  * @see [[net.noresttherein.oldsql.schema.GenericMapping]]
  * @see [[net.noresttherein.oldsql.schema.MappingSupport]]
  * @see [[net.noresttherein.oldsql.schema.ColumnMapping]]
  */
trait Mapping { mapping :ConcreteMapping =>
	/** The mapped entity type. */
	type Subject


	/** A phantom marker type denoting the origin of this mapping. It is used to statically distinguish between
	  * different instances of the same mapping class, but mapping different portions of the result set -
	  * in particular, work as aliased names for repeated occurrence of a table and its components in a joined query.
	  * In addition, it is used by the SQL DSL to ensure that an SQL expression refers only to components
	  * coming from the one particular query, preventing accidental use of other, non-joined mappings.
	  */
	type Owner

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
	  * additionally the lifted version of that component (from the point of view of this mapping).
	  */
	type Selector[T] = ComponentExtractor[Owner, Subject, T]

	type AnyComponent = Component[_]

	/** Any mapping with the same origin marker type, making it a supertype of all valid component types of this mapping. */
	type Component[T] = Mapping.Component[Owner, T]

	/** Any [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] with the same origin marker type
	  * as this instance and thus a valid subcomponent type of this mapping.
	  */
	type Column[T] = ColumnMapping[Owner, T]



	/** Adapts the given subcomponent (a component being the end of some path of components starting with this instance)
	  * to its public form as present in the `subcomponents` and `components` list. For every valid transitive
	  * subcomponent of this mapping, there is exactly one equal to its lifted form on the `subcomponents` list.
	  * This process can modify the mapping definition by changing names of the columns (prefixing them) and updating
	  * their buffs by cascading the buffs present on this instance.
	  * By default it returns the `lifted` property of the selector for the component returned by
	  * [[net.noresttherein.oldsql.schema.Mapping#apply[T](component:Component[T]) apply(component)]].
	  */
	def lift[T](component :Component[T]) :Component[T] = apply(component).lifted

	/** Adapts the given column of one of the transitive subcomponents of this instance to the form present
	  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
	  * exactly one equal to its lifted version on the `columns` list. This process can change the name and buffs
	  * of the column, reflecting additional information present in this mapping.
	  * By default this is an identity operation as no adaptation of subcomponents takes place.
	  */
//	def lift[T](column :Column[T]) :Column[T] = column



	/** Retrieves the [[net.noresttherein.oldsql.schema.Mapping.ComponentExtractor ComponentExtractor]]
	  * for the given component.
	  * @throws NoSuchElementException if `component` is not a subcomponent of this mapping.
	  */
	def apply[T](component :Component[T]) :Selector[T]



	/** Direct component mappings of this mapping, including any top-level columns. Always empty for columns. */
	def components :Unique[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared by it's components or
	  * other subcomponents), or all that this mapping cares to expose, as instances of this.Component[_].
	  * This list should include all selectable columns. It is typically defined by recursive ''flat mapping''
	  * over the `components` list and including the direct components themselves. This list is always empty
	  * for columns, thus ending any similar recursion.
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


	/** Read form of a select statement for this mapping including the given components of this mapping.
	  * The list provided here can include any components, not just the columns.
	  */
	def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject]

	/** Default read form (included columns) of a select statement for this mapping. */
	def selectForm :SQLReadForm[Subject]

	/** Default write form (included parameters) of a query filter for this mapping. */ //todo: PK? all?
	def queryForm :SQLWriteForm[Subject]

	/** Default write form (included columns) of update statements for this mapping. */
	def updateForm :SQLWriteForm[Subject]

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
	def inOption :OptionMapping[this.type, Owner, Subject] = OptionMapping(this)

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If this mapping's `optionally` subject constructor returns `None`, implicitly provided here
	  * `NullValue.value` will be returned.
	  */
	def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[this.type, Owner, Subject, X](this, there, back)

	/** Transform this mapping to a new subject type `X` by mapping all values before writing and after reading them
	  * by this mapping. If `there` or this mapping's `optionally` subject constructor returns `None`,
	  * implicitly provided here `NullValue.value` will be returned. If `back` returns `None`, `null` value will
	  * be written in the database.
	  */
	def flatMap[X](there :Subject => Option[X], back :X => Option[Subject])
	              (implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping.opt[this.type, Owner, Subject, X](this, there, back)



	def canEqual(that :Any) :Boolean = that.isInstanceOf[Mapping]



	override def toString :String = sqlName getOrElse this.unqualifiedClassName

	def columnString :String = columns.mkString(toString + "{", ", ", "}")

	def debugString :String = {
		/** Use recursion to print the ''lifted'' (by this mapping) version of every subcomponent.
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
					res ++= "{" //lift and print {components}
					if (mapping.components.isEmpty)
						res ++= "}"
					else {
						res ++= "\n"
						(false /: mapping.components.map(lift(_)))(
							(wasColumn, comp) => rec(comp, res, ident + "  ", wasColumn)
						)
						res ++= "\n"  ++= ident ++= "}"
					}
					false
			}
		val res = new StringBuilder
		rec(this, res)
		res.toString
//		components.map(c => lift(c).debugString).mkString("\n" + toString + "{\n", "; ", "\n}")
	}

}






/** The de facto base trait of all `Mapping` implementations.
  *
  * `Mapping` remains the main outside interface
  * as it allows easy parameterizing with the mapping type without the extra `Owner` and `Subject` type parameters,
  * but it severely limits available options of what is possible to do, especially when several instances with same or
  * related types are available. While extending it is not currently strictly required and the library just demands
  * that the two member types are defined on a `Mapping` via the  `Component[O, S]` type alias narrowing the `Mapping`,
  * Doing so is the most convenient way to achieve it and provides implementations of several methods which could
  * not be done without
  *
  * @tparam O marker type used to distinguish between several instances of the same mapping class, but coming from
  *           different sources (especially different aliases for a table occurring more then once in a join).
  *           At the same time, it adds additional type safety by ensuring that only components of mappings included
  *           in a query can be used in the creation of SQL expressions used by that query.
  * @tparam S the subject type, that is the type of objects read and written to a particular table (or a view, query,
  *           or table fragment).
  */ //todo: if type parameter `O` stays, it should be last, so that type inferer can infer Component[S, _] type functor
trait GenericMapping[O, S] extends ConcreteMapping { self =>
	type Owner = O
	type Subject = S

	def buffs :Seq[Buff[S]] = Nil



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this :this.type, components)

	override def selectForm: SQLReadForm[S] = MappingReadForm.defaultSelect(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.defaultQuery(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.defaultUpdate(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.defaultInsert(this)
	//fixme: ColumnFilter needs to know what type of read/write column form to use
//	override def writeForm(filter :ColumnFilter) :SQLWriteForm[S] = filter.write(this)
//	override def readForm(filter :ColumnFilter) :SQLReadForm[S] = filter.read(this)



	override def apply(pieces: Pieces): S =
		optionally(pieces) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $pieces")
		}

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
  * of its components on the passed `Pieces`. Some mappings (and possibly their of its components) allow
  * declaring a column prefix to be added to all its columns, as well as additional buffs, which should be inherited
  * by all of its subcomponents (including columns), so a component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. As it is the latter version of the component
  * which is used by the framework to create any SQL statements, and thus also by the `Pieces`, but typically
  * the former is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces`
  * implementation substitutes any component passed to it with its lifted representation before looking for its value.
  *
  */
trait RootMapping[O, S] extends GenericMapping[O, S] {
	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased { c => apply[c.Subject](c).lifted })
}






object Mapping {
	@inline
	implicit def mappingSelfPath[M <: Mapping, X <: Component[O, S], O, S]
	                            (mapping :M)(implicit help :IsBoth[M, X, Component[O, S]]) :SelfPath[X, O, S] =
		SelfPath.typed[X, O, S](mapping)

	implicit class MappingLabelling[M <: Component[_, _]](private val self :M) extends AnyVal {
		/** Attaches a label (a string literal) to this mapping, transforming it into a `LabeledMapping` with the
		  * literal type included as its type parameter.
		  */
		def @:[L <: Label](label :L) :L @: M = LabeledMapping[L, M, M#Owner, M#Subject](label, self)
	}



	/** This is an implementation artifact which may disappear from the library without notice. ''Do not use
	  * it in the client code''.
	  * A sealed trait mixed-in by `GenericMapping`, which enforces that any of its (proper) subclasses,
	  * abstract or not, are also `GenericMapping` instances. Many generic methods and classes require
	  * their `Mapping` type parameter to have definitions of both or one of `Owner` and `Subject`.
	  * Unfortunately, declaring `[M &lt;: Component[_, _]]` leads to `M#Owner`, `M#Subject` being instantiated
	  * early as unique types, not unifiable with types `O, S` where `C &lt;: Component[O, S]]` is the type argument
	  * for `M`. Somewhat counterintuitively, `[M &lt;: ConcreteMapping]` on the other hand, leads to
	  * `M#Owner =:= O` and `M#Subject =:= S` in the same circumstances. While the generic class/method will
	  * likely need to resort to casting down `M#Subject` to `m.Subject` for some `m :M`, in the interface at least
	  * it provides type safety. The only exception would be `M =:= ConcreteMapping` itself, so we simply warn
	  * against any references to it by the client application.
	  */
	sealed trait ConcreteMapping extends Mapping



	type TypedMapping[S] = ConcreteMapping { type Subject = S } //todo: MappingOf
	type AnyComponent[O] = ConcreteMapping { type Owner = O } //todo: MappingFrom
	type Component[O, S] = ConcreteMapping { type Owner = O; type Subject = S } //todo: TypedMapping

//	type MappingFrom[O] = { type T[S] = Component[O, S] }
//	type MappingOf[S] = { type T[O] = Component[O, S] }



//	type SingletonMapping = Mapping with Singleton
//	type TypedSingleton[S] = Mapping with Singleton { type Subject = S }
//	type AnySingleton[O] = Mapping with Singleton { type Owner = O }
//	type SingletonComponent[O, S] = Component[O, S] with Singleton //Mapping with Singleton { type Owner = O; type Subject = S }


	type CompatibleMapping[M <: Mapping] = ConcreteMapping {
		type Owner = M#Owner
		type Subject = M#Subject
	}

//	type TypeCompatibleMapping[M <: Mapping] = Mapping {
//		type Subject = M#Subject
//	}

	type ConcreteSubclass[M <: ConcreteMapping] = M {
		type Owner = M#Owner
		type Subject = M#Subject
	}







	/** A `ComponentExtractor` describes the parent-child relationship between a mapping and its component.
	  * It serves three functions:
	  *   - provides a means of extracting the value of the component from the value of the parent;
	  *   - retrieves the value of a component from `ComponentValues`;
	  *   - provides the canonical, 'lifted' version of the component, that is the version with any wholesale
	  *     modifications declared in the parent mapping (or some other mapping on the path to the subcomponent),
	  *     applied to the original version of the component. This includes buffs and column prefix declarations
	  *     defined for all subcomponents of a mapping.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](Component[T] ]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]]
	  */
	trait ComponentExtractor[O, -S, T] extends Extractor[S, T] {
		//todo: rename exported
		val lifted :Component[O, T]

		override def compose[X](extractor :Extractor[X, S]) :ComponentExtractor[O, X, T] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[O, X, T]]

			case const :ConstantExtractor[_, S] => try {
				ComponentExtractor.const(lifted)(apply(const.constant))
			} catch {
				case _ :Exception => ComponentExtractor.opt(lifted)(const.getter andThen optional)
			}

			case req :RequisiteExtractor[X, S] =>
				ComponentExtractor.opt(lifted)(req.getter andThen optional)

			case _ :EmptyExtractor[_, _] =>
				ComponentExtractor.none[O, T](lifted)

			case _ =>
				val first = extractor.optional; val second = optional
				ComponentExtractor.opt(lifted)(first(_) flatMap second)
		}

		override def compose[X](first :X => S) :ComponentExtractor[O, X, T] =
			ComponentExtractor.opt(lifted)(first andThen optional)

		def andThen[Y](selector :ComponentExtractor[O, T, Y]) :ComponentExtractor[O, S, Y] =
			selector compose this

		override def toString :String = "Extractor(" + lifted + ")"
	}



	object ComponentExtractor {
		def apply[O, S, T](component :Component[O, T], pick :S => Option[T], surepick :Option[S=>T]) :ComponentExtractor[O, S, T] =
			surepick match {
				case Some(sure) => req(component)(sure)
				case _ => opt(component)(pick)
			}

		def apply[O, S, T](component :Component[O, T])(extractor :Extractor[S, T]) :ComponentExtractor[O, S, T] =
			extractor match {
				case _ :IdentityExtractor[_] => ident(component).asInstanceOf[ComponentExtractor[O, S, T]]
				case c :ConstantExtractor[_, T] => const(component)(c.constant)
				case requisite :RequisiteExtractor[S, T] => new RequisiteComponent(component, requisite.getter)
				case _ :EmptyExtractor[_, _] => none(component)
				case _ => new OptionalComponent(component, extractor.optional) //todo: FromOptionExtractor
			}



		def req[O, S, T](component :Component[O, T])(requisite :S => T) :ComponentExtractor[O, S, T] =
			new RequisiteComponent[O, S, T](component, requisite)

		def opt[O, S, T](component :Component[O, T])(selector :S => Option[T]) :ComponentExtractor[O, S, T] =
			new OptionalComponent[O, S, T](component, selector)

		def ident[O, T](component :Component[O, T]) :ComponentExtractor[O, T, T] =
			new IdentityComponent[O, T](component)

		def const[O, T](component :Component[O, T])(value :T) :ComponentExtractor[O, Any, T] =
			new ConstantComponent[O, T](component, value)

		def none[O, T](component :Component[O, T]) :ComponentExtractor[O, Any, T] =
			new EmptyComponent(component)



		def unapply[O, S, T](selector :ComponentExtractor[O, S, T]) :Option[(S => Option[T], Option[S => T], Component[O, T])] =
			Some(selector.optional, selector.requisite, selector.lifted)



		class OptionalComponent[O, S, T](val lifted :Component[O, T], override val optional :S => Option[T])
			extends ComponentExtractor[O, S, T] with OptionalExtractor[S, T]
		{
			override def get(x :S) :Option[T] = optional(x)

			override def toString :String = "Optional(" + lifted + ")"
		}



		class RequisiteComponent[O, -S, T](final val lifted :Component[O, T], override val getter :S => T)
			extends ComponentExtractor[O, S, T] with RequisiteExtractor[S, T]
		{
			override def apply(x :S) :T = getter(x)

			override def compose[X](extractor :X =?> S) :ComponentExtractor[O, X, T] = extractor match {
				case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[O, X, T]]
				case _ :ConstantExtractor[_, _] => super.compose(extractor)
				case req :RequisiteExtractor[X, S] =>
					new RequisiteComponent[O, X, T](lifted, req.getter andThen getter)
				case _ =>
					val first = extractor.optional
					new OptionalComponent[O, X, T](lifted, first(_).map(getter))
			}

			override def compose[W](extractor :RequisiteExtractor[W, S]) :RequisiteComponent[O, W, T] = extractor match {
				case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[O, W, T]]
				case const :ConstantExtractor[_, S] => try {
					new ConstantComponent[O, T](lifted, getter(const.constant))
				} catch {
					case _ :Exception => new RequisiteComponent[O, W, T](lifted, extractor.getter andThen getter)
				}
				case _ =>
					new RequisiteComponent[O, W, T](lifted, extractor.getter andThen getter)
			}

			override def compose[W](req :W => S) :RequisiteComponent[O, W, T] =
				new RequisiteComponent[O, W, T](lifted, req andThen getter)

			override def toString :String = "Requisite(" + lifted + ")"
		}



		class IdentityComponent[O, S](component :Component[O, S])
			extends RequisiteComponent[O, S, S](component, identity[S]) with IdentityExtractor[S]
		{
			override def compose[W](extractor :W =?> S) :ComponentExtractor[O, W, S] = extractor match {
				case comp :ComponentExtractor[_, _, _] if comp.lifted == lifted =>
					comp.asInstanceOf[ComponentExtractor[O, W, S]]

				case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[O, W, S]]

				case c :ConstantExtractor[_, _] => const(lifted)(c.constant.asInstanceOf[S])

				case e :RequisiteExtractor[_, _] => req(lifted)(e.getter.asInstanceOf[W => S])

				case _ => opt(lifted)(extractor.optional)
			}

			override def compose[W](extractor :RequisiteExtractor[W, S]) :RequisiteComponent[O, W, S] = extractor match {
				case comp :RequisiteComponent[_, _, _] => comp.asInstanceOf[RequisiteComponent[O, W, S]]
				case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[O, W, S]]
				case _ => new RequisiteComponent[O, W, S](lifted, extractor.getter)
			}

			override def compose[W](req :W => S) = new RequisiteComponent[O, W, S](lifted, req)

			override def toString :String = "Identity(" + lifted + ")"
		}



		class ConstantComponent[O, S](component :Component[O, S], const :S)
			extends RequisiteComponent[O, Any, S](component, (_ :Any) => const) with ConstantExtractor[Any, S]
		{
			override def constant :S = const

			override def compose[W](extractor :W =?> Any) :ComponentExtractor[O, W, S] = this

			override def compose[W](extractor :RequisiteExtractor[W, Any]) :RequisiteComponent[O, W, S] = this

			override def compose[W](req :W => Any) :RequisiteComponent[O, W, S] = this

			override def toString :String = "Const(" + component + "=" + constant + ")"
		}



		class EmptyComponent[O, T](component :Component[O, T])
			extends OptionalComponent[O, Any, T](component, Extractor.none.optional) with EmptyExtractor[Any, T]
		{
			override def compose[W](extractor :W =?> Any) :EmptyComponent[O, T] = this

			override def compose[W](req :W => Any) :EmptyComponent[O, T] = this

			override def toString :String = "Empty(" + component + ")"
		}


	}






	trait ColumnFilter {
		def apply[S](mapping :TypedMapping[S]) :Unique[mapping.Component[_]] =
			mapping.columns.filter(test[mapping.Owner])

		def filter[O](columns :Unique[AnyComponent[O]]) :Unique[AnyComponent[O]] =
			columns.filter(test[O])

		def test[O](column :AnyComponent[O]) :Boolean

		def read[S](mapping :TypedMapping[S]) :SQLReadForm[S] =
			MappingReadForm[mapping.Owner, S](mapping, apply(mapping :mapping.type))

		def write[S](mapping :TypedMapping[S])(forms :mapping.AnyComponent => SQLForm[_]) :SQLWriteForm[S] =
			MappingWriteForm[mapping.Owner, S](mapping, apply(mapping :mapping.type), forms)
	}



	object ColumnFilter {

		def apply(pred :Mapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def test[O](column :AnyComponent[O]) :Boolean = pred(column)
			}



		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :TypedMapping[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :AnyComponent[O]): Boolean = buff.enabled(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :TypedMapping[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :AnyComponent[O]): Boolean = buff.disabled(column)
		}

		class WriteFilter(modifier :BuffType) extends WithoutBuff(modifier) {
			override def read[S](mapping: TypedMapping[S]) =
				EmptyForm(throw new UnsupportedOperationException(s"$this: read for $mapping"))
		}



		case object ForSelect extends WithoutBuff(NoSelectByDefault) {
			override def read[S](mapping: TypedMapping[S]) :SQLReadForm[S] = mapping.selectForm
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
			override def apply[S](mapping :TypedMapping[S]) :Unique[mapping.Component[_]] = mapping.columns
			override def filter[O](columns :Unique[AnyComponent[O]]) :Unique[AnyComponent[O]] = columns
			override def test[O](column :AnyComponent[O]) = true
		}
	}






	private class MappingReadForm[O, S] private[Mapping]
			(mapping :Component[O, S], columns :Unique[AnyComponent[O]], read :AnyComponent[O]=>SQLReadForm[_] = (_:AnyComponent[O]).selectForm)
		extends SQLReadForm[S]
	{
		private[this] val columnArray = columns.toArray
		override val readColumns: Int = (0 /: columns)(_ + read(_).readColumns)

		override def opt(position: Int)(res: ResultSet): Option[S] = {
			var i = position //consider: not precompute the values (which wraps them in Option) but read on demand.
			val columnValues = columnArray.map { c => i += 1; read(c).opt(i - 1)(res) }
			val pieces = ComponentValues(mapping) { comp =>
				val idx = columns.indexOf(mapping.lift(comp))
				if (idx >= 0) columnValues(idx)
				else None
			}
			mapping.optionally(pieces)
		}

		override def nullValue: S = mapping.nullValue getOrElse {
			mapping.apply(ComponentValues(mapping :mapping.type) { comp =>
				val lifted = mapping.lift(comp)
				if (columns.contains(lifted))
					Some(read(lifted).nullValue)
				else None
			})
		}

		private def mappingString = mapping.sqlName getOrElse mapping.unqualifiedClassName

		override def toString :String = columns.map(read).mkString(s"<$mappingString{", ",", "}")
	}






	object MappingReadForm {

		def apply[O, S](mapping :Component[O, S], columns :Unique[AnyComponent[O]],
		                              forms :AnyComponent[O] => SQLReadForm[_] = (_:AnyComponent[O]).selectForm) :SQLReadForm[S] =
			new MappingReadForm[O, S](mapping, columns, forms)



		def select[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]]) :SQLReadForm[S] = {
			val columns = components.map(mapping.lift(_)).flatMap(_.selectable)

			if (columns.exists(NoSelect.enabled))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
			val extra = columns ++ ExtraSelect.Enabled(mapping)
			new MappingReadForm[O, S](mapping, extra, _.selectForm)
		}

		def defaultSelect[S](mapping :TypedMapping[S]) :SQLReadForm[S] = {
			val columns = mapping.selectable.filter(NoSelectByDefault.disabled) ++ ExtraSelect.Enabled(mapping)
			new MappingReadForm[mapping.Owner, S](mapping, columns, _.selectForm)
		}


		def fullSelect[S](mapping :TypedMapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.selectable ++ ExtraSelect.Enabled(mapping))



		def autoInsert[S](mapping :TypedMapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.autoInserted)

		def autoUpdate[S](mapping :TypedMapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.autoUpdated)

	}






	private class MappingWriteForm[O, S] private (
			mapping :Component[O, S],
			columns :Unique[AnyComponent[O]],
			substitute :ValuedBuffType,
			write :AnyComponent[O] => SQLWriteForm[_])
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
					val column = c.asInstanceOf[Component[O, Any]]
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

		def apply[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
		                              write :AnyComponent[O] => SQLWriteForm[_],
		                              replacement :ValuedBuffType = AbstractValuedBuff) :SQLWriteForm[S] =
			new MappingWriteForm[O, S](mapping, components, replacement, write)


		def query[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
		                              write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).queryForm) :SQLWriteForm[S] =
			custom(mapping, components, NoQuery, ExtraQuery, write)

		def defaultQuery[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoQuery, ExtraQuery, _.queryForm)

		def fullQuery[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.queryable, ExtraQuery, _.queryForm)



		def update[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
			                           write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).updateForm) :SQLWriteForm[S] =
			custom(mapping, components, NoUpdate, ExtraUpdate, write)

		def defaultUpdate[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoUpdate, ExtraUpdate, _.updateForm)

		def fullUpdate[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraUpdate, _.updateForm)



		def insert[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
		                               write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).insertForm) :SQLWriteForm[S] =
			custom(mapping, components, NoInsert, ExtraInsert, write)

		def defaultInsert[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoInsert, ExtraInsert, (_:mapping.AnyComponent).insertForm)

		def fullInsert[S](mapping :TypedMapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraInsert, _.insertForm)



		private def custom[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
		                                       prohibit :BuffType, mandatory :ValuedBuffType,
		                                       write :AnyComponent[O] => SQLWriteForm[_]) :SQLWriteForm[S] =
		{
			if (components.exists(prohibit.enabled))
				throw new IllegalArgumentException(
					s"Can't create a write form for $mapping using $components: $prohibit buff present among selection"
				)
			val extra = components.flatMap(mapping.lift(_).columns.toSeq.filter(prohibit.disabled) ++ mandatory.Enabled(mapping))
			new MappingWriteForm[O, S](mapping, extra, mandatory, write)
		}

		private def default[S](mapping :TypedMapping[S])(filterNot :BuffType, mandatory :ValuedBuffType,
		                                                 write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] = {
			val columns = filterNot.Disabled(mapping) ++ mandatory.Enabled(mapping)
			new MappingWriteForm[mapping.Owner, S](mapping, columns, mandatory, write)
		}

		private def full[S](mapping :TypedMapping[S])(columns :Unique[mapping.AnyComponent], extra :ValuedBuffType,
		                                              write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] =
			new MappingWriteForm[mapping.Owner, S](mapping, columns ++ extra.Enabled(mapping), extra, write)

	}



}
