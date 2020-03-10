package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.InverseIndexSeq.implicitIndexing
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, MappingReadForm, MappingWriteForm, Selector}
import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.support.{MappedMapping, PrefixedMapping}
import net.noresttherein.oldsql.schema.support.MappedMapping.MappedAs
import net.noresttherein.oldsql.schema.Buff.{AbstractValuedBuff, BuffType, ExplicitSelect, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValuedBuffType}
import net.noresttherein.oldsql.schema.Mapping.ComponentSelector
import net.noresttherein.oldsql.slang._

import scala.collection.breakOut




/** Root interface of the  mapping class hierarchy to be used when the mapped type is of little importance (for example
  * in collections of mappings for various types). All implementations are required to extend `Mapping[T]` instead,
  * which is parameterized with the mapped type. The main reason for this duplicity is the limitation of scala type
  * inference: a generic method `m[M&lt;:Mapping[T], T](m :M)` will infer type parameters `M`, `Nothing` when given
  * an argument of type `M&lt;:Mapping[T]`. On the other hand, when a type parameter is declared `M&lt;:Mapping[_]`,
  * the `Subject` is instantiated early as 'Any' and it is not equal to `m.Subject`, leading to other problems later.
  */
sealed trait AnyMapping { mapping =>
	/** The mapped entity type. */
	type Subject

	/** The mapping which this instance is a part of. This may be the mapping directly enclosing this mapping,
	  * but more typically will be a 'root' mapping, such as a table mapping. The 'Owner' tag is passed down
	  * the components comprising such a mapping in order to identify them as containing columns being part of
	  * the owner mapping, thus adding type safety when creating sql formulas, which can thus enforce that
	  * only components/columns belonging to the selected tables (or otherwise root mappings) can be used
	  * in the where or select clause.
	  */
	type Owner <: AnyMapping

	/** A container with values for components of this mapping required to assemble the subject. */
	type Values = ComponentValues[this.type]


	type AnyComponent = Component[_]
	type Component[T] = Mapping.Component[Owner, T] //<: SubMapping[T, Owner]
	type Column[T] = ColumnMapping[Owner, T]

	def asComponent :this.type with Component[Subject]


	/** Adapts the given subcomponent (a component being the end of some path of components starting with this instance)
	  * to its exported form as present in the `subcomponents` and `components` list. For every valid transitive
	  * subcomponent of this mapping, there is exactly one equal to its lifted form on the `subcomponents` list.
	  * This process can modify the mapping definition by changing names of the columns (prefixing them) and updating
	  * their buffs by cascading the buffs present on this instance.
	  * By default this is an identity operation as no adaptation of subcomponents takes place.
	  */
	def lift[T](component :Component[T]) :Component[T] = apply(component).lifted

	/** Adapts the given column of one of the transitive subcomponents of this instance to the form present
	  * in the `columns` list (and others). For every column of every subcomponent of this instance, there is
	  * exactly one equal to its lifted version on the `columns` list. This process can change the name and buffs
	  * of the column, reflecting additional information present in this mapping.
	  * By default this is an identity operation as no adaptation of subcomponents takes place.
	  */
//	def lift[T](column :Column[T]) :Column[T] = column



	def apply[T](component :Component[T]) :Selector[this.type, Owner, Subject, T]



	/** Direct component mappings of this mapping, including any top-level columns. */
	def components :Seq[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared by it's components or
	  * other subcomponents), or all that this mapping cares to expose, as instances of this.Component[_].
	  * This list should include all selectable columns.
	  */
	def subcomponents :Seq[Component[_]]

	@deprecated
	def columnsFor(filter :ColumnFilter) :Seq[AnyComponent]

	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only,
	  * write-only and so on. */
	def columns :Seq[Component[_]]
	/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` flag). */
	def selectable :Seq[Component[_]]
	/** All columns which can be part of an SQL statement's where clause (don't have the `NoQuery` flag set). */
	def queryable :Seq[Component[_]]
	/** All columns which can be updated on existing database records (don't have the `NoUpdate` flag set). */
	def updatable :Seq[Component[_]]
	/** All columns which are updated by the database on each update statement (and should be returned to the application). */
	def autoUpdated :Seq[Component[_]]
	/** All columns which can occur in an insert statement (don't have the `NoInsert` flag set). */
	def insertable :Seq[Component[_]]
	/** Columns autogenerated by the database; this implies being non-insertable. */
	def autoInserted :Seq[Component[_]]

	/** All columns with a given buff provided. */
	def columnsWith(buff :BuffType) :Seq[Component[_]] =
		columns.filter(buff.enabled)

	/** All columns without the given buff. */
	def columnsWithout(buff :BuffType) :Seq[Component[_]] =
		columns.filter(buff.disabled)


	/** Read form of a select statement for this mapping including the given components of this mapping. */
	def selectForm(components :Seq[Component[_]]) :SQLReadForm[Subject]

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
	  * only on columns. If a larger component declares a buff, it should be inherited by all its subcomponents
	  * (and, transitively, by all columns). As those subcomponents can in turn have their own buffs attached,
	  * care needs to be taken in order to avoid conflicts and undesired interactions.
	  */
	def buffs :Seq[Buff[Subject]]



	/** Attempts to retrieve or assemble the value of `Subject` from the passed `ComponentValues` for this instance.
	  * Standard implementation will test several sources together with `values` before giving up:
	  * a ready value present for this mapping, assembling the result from subcomponents and finally,
	  * a default coming from an attached `OptionalBuff` (or related). By default it forwards to
	  * [[net.noresttherein.oldsql.schema.AnyMapping.optionally optionally]] and should stay consistent with it.
	  * @throws NoSuchElementException if no value can be provided (`optionally` returns `None`).
	  * @see [[net.noresttherein.oldsql.schema.AnyMapping.assemble assemble]]
	  */
	def apply(values: Values): Subject =
		optionally(values) getOrElse {
			throw new NoSuchElementException(s"Can't assemble $this from $values.")
		}

	/** Attempts to retrieve or assemble the value for the mapped `Subject` from the given `ComponentValues`.
	  * This is the top-level method which can, together with passed `values`, produce the result in several ways.
	  * By default it forwards the call to the [[net.noresttherein.oldsql.schema.ComponentValues.result result]] method
	  * of `ComponentValues` (which, by default, will first check if it has a predefined value stored for this mapping,
	  * and, only if not, forward to this instance's [[net.noresttherein.oldsql.schema.AnyMapping.assemble assemble]]
	  * method which is responsible for actual assembly of the value from the values of the subcomponents, recursively
	  * obtained from `values`.
	  *
	  * If all of the above fails, this method will check for a predefined value stored in an attached
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] (or related) buff if it exists.
	  *
	  * Returning `None` signifies that the value of this mapping was not available in `values`, and is different
	  * from a 'null' value explicitly retrieved by the query. While the two sometimes will get conflated in practice
	  * (which can be unavoidable). It is generally used for the cases such as outer joins, or missing columns from
	  * the select clause. It is therefore perfectly possible for `Some(null)` (or some other representation of the
	  * `null` value for type `Subject`) to be returned. Likewise, even if data for this mapping is missing,
	  * it may decide to return here some default 'missing' value; it is ultimately always up to the mapping
	  * implementation to handle the matter. The above distinction is complicated further by the fact that the mapped
	  * type `Subject` itself can be an `Option` type, used to signify either or both of these cases.
	  * @see [[net.noresttherein.oldsql.schema.AnyMapping.assemble assemble]]
	  */
	def optionally(values: Values): Option[Subject] //=
//		values.result(this) orElse OptionalSelect.Value[Subject](this)

	/** Attempts to assemble the value of this mapping from the values of subcomponents stored in the passed
	  * `ComponentValues`. This is the final dispatch target of other constructor methods declared here or
	  * in [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] and should not be called directly.
	  * @see [[net.noresttherein.oldsql.schema.AnyMapping.optionally optionally]]
	  * @see [[net.noresttherein.oldsql.schema.AnyMapping.apply apply]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.value ComponentValues.value]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.getValue ComponentValues.getValue]]
	  */
	def assemble(values :Values) :Option[Subject]

	def nullValue :Option[Subject]



	def valueOf[T](component :Component[T], subject :Subject) :Option[T] = apply(component).get(subject)

	def valueOf[T](component :Component[T], values :Values) :Option[T] = apply(component).get(values)

	def apply[T](component :Component[T], subject :Subject) :T = apply(component)(subject)

	def apply[T](component :Component[T], values :Values) :T = apply(component)(values)



	def sqlName :Option[String] = None

	def isSymLink[T](component :Component[T]) :Boolean = ??? /*symLinkTarget(component).isDefined

	def symLinkTarget[T](component :Component[T]) :Option[ComponentPath[this.type, _<:AnyMapping]] =
		SymLink.startsWith[this.type, T](this :this.type)(component).map(_.target)
*/
	def qualified(prefix :String) :Component[Subject]

	def prefixed(prefix :String) :Component[Subject]


	def map[X](there :Subject=>X, back :X=>Subject) :this.type MappedAs X =
		MappedMapping[this.type, Subject, X](this, there, back)


	override def toString :String = sqlName getOrElse this.unqualifiedClassName

	def introString :String = components.mkString(this + "{", ", ", "}")


}






trait Mapping[S] extends AnyMapping { self =>

	type Subject = S

	def buffs :Seq[Buff[S]] = Nil

	override def asComponent :this.type = this


	override def columnsFor(filter :ColumnFilter) :Seq[AnyComponent] = filter(this :this.type)

	override def selectForm(components :Seq[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this :this.type, components)

	override def selectForm: SQLReadForm[S] = MappingReadForm.defaultSelect(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.defaultQuery(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.defaultUpdate(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.defaultInsert(this)
	//fixme: ColumnFilter needs to know what type of read/write column form to use
//	override def writeForm(filter :ColumnFilter) :SQLWriteForm[S] = filter.write(this)
//	override def readForm(filter :ColumnFilter) :SQLReadForm[S] = filter.read(this)



	override def apply(values: Values): S =
		optionally(values) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $values")
		}

	override def optionally(values: Values): Option[S] = //todo: perhaps extract this to MappingReadForm for speed (not really needed as the buffs cascaded to columns anyway)
		values.result(this) map { res => (res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) } } orElse
			OptionalSelect.Value(this) orElse ExtraSelect.Value(this)

	def assemble(values :Values) :Option[S]

	def nullValue :Option[S] = None



	def qualified(prefix :String) :Component[S] = PrefixedMapping.qualified(prefix, this)

	def prefixed(prefix :String) :Component[S] = PrefixedMapping(prefix, this)

}



trait SubMapping[O <: AnyMapping, S] extends Mapping[S] {
	type Owner = O
//	type Component[X] = SubMapping[X, O]

//	override final def asComponent :this.type = this

}



trait RootMapping[S] extends Mapping[S] {
	type Owner = this.type
//	type Component[X] = SubMapping[S, this.type]

//	override final def asComponent :this.type = this
}






object Mapping {
	type TypedMapping[S] = AnyMapping { type Subject = S }
//	type Component[S, O] = AnyMapping { type Subject=S; type Parent=O }
//	type AnyComponent[O] = AnyMapping { type Parent=O }
	type AnyComponent[O <: AnyMapping] = Component[O, _]
	type Component[O <: AnyMapping, S] = Mapping[S] { type Owner = O }//AnyMapping { type Subject = S; type Owner = O }

	type SingletonMapping = AnyMapping with Singleton
	type TypedSingleton[S] = AnyMapping with Singleton { type Subject = S }
	type SingletonComponent[O <: AnyMapping, S] = Mapping[S] with Singleton { type Owner = O } //AnyMapping with Singleton { type Subject = S; type Owner = O }


	type ComponentCompatibleMapping[M <: AnyMapping] = AnyMapping {
		type Component[X] = Mapping.Component[M#Owner, X]
	}

	type CompatibleMapping[M <: AnyMapping] = AnyMapping {
		type Owner = M#Owner
		type Subject = M#Subject
		type Component[X] = M#Component[X] //SubMapping[X, M]
	}

	type TypeCompatibleMapping[M<:AnyMapping] = AnyMapping {
		type Subject = M#Subject
	}






	sealed trait ComponentSelector[M <: SingletonMapping, T] {
		def pick :M#Subject => Option[T]
		def surepick :Option[M#Subject => T]

		def apply(whole :M#Subject) :T

		def get(whole :M#Subject) :Option[T]

		def apply(values :M#Values) :T

		def get(values :M#Values) :Option[T]

		val lifted :Component[M#Owner, T]
	}

	/** A selector describes the parent-child relationship between a mapping and its component.
	  * It serves three functions:
	  *   - provides a means of extracting the value of the component from the value of the parent;
	  *   - retrieves the value of a component from `ComponentValues`;
	  *   - provides the canonical, 'lifted' version of the component, that is the version with any wholesale
	  *     modifications declared in the parent mapping (or some other mapping on the path to the subcomponent),
	  *     applied to the original version of the component. This includes buffs and column prefix declarations
	  *     defined for all subcomponents of a mapping.
	  * @see [[net.noresttherein.oldsql.schema.AnyMapping.apply[T](Component[T] ]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]]
	  */
	trait Selector[M <: SingletonMapping { type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		extends ComponentSelector[M, T]
	{
		def pick :S => Option[T]
		def surepick :Option[S => T]

		def apply(whole :S) :T = get(whole) getOrElse {
			throw new NoSuchElementException(s"No value for $lifted in $whole.")
		}

		def get(whole :S) :Option[T] = pick(whole)

		def apply(values :M#Values) :T = values(this)

		def get(values :M#Values) :Option[T] = values.get(this)

		val lifted :Component[O, T]

		override def toString :String = "Selector(" + lifted + ")"
	}



	object Selector {
		def apply[M <: SingletonMapping { type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		         (component :Component[O, T], pick :S => Option[T], surepick :Option[S=>T]) :Selector[M, O, S, T] =
			new CustomSelector(component, pick, surepick)


		def apply[M <: SingletonMapping { type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		         (component :Component[O, T], requisite :S => T) :Selector[M, O, S, T] =
			new RequisiteSelector[M, O, S, T](component, requisite)

		def apply[O <: AnyMapping, S, T](mapping :Component[O, S], component :Component[O, T])(requisite :S => T) :Selector[mapping.type, O, S, T] =
			new RequisiteSelector[mapping.type, O, S, T](component, requisite)



		def opt[M <: SingletonMapping{ type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		       (component :Component[O, T], selector :S => Option[T]) :Selector[M, O, S, T] =
			new OptionalSelector[M, O, S, T](component, selector)

		def opt[O <: AnyMapping, S, T](mapping :Component[O, S], component :Component[O, T])(selector :S => Option[T]) :Selector[mapping.type, O, S, T] =
			new OptionalSelector[mapping.type, O, S, T](component, selector)



		def unapply[O <: AnyMapping, T, S](selector :Selector[_ <: SingletonMapping { type Owner = O; type Subject = S }, O, S, T])
				:Option[(S=>Option[T], Option[S=>T], Component[O, T])] =
			Some(selector.pick, selector.surepick, selector.lifted)


		class RequisiteSelector[M <: SingletonMapping { type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		                       (val lifted :Component[O, T], val extractor :S => T)
			extends Selector[M, O, S, T]
		{
			val pick = (s :S) => Some(extractor(s))
			val surepick = Some(extractor)

			override def apply(whole :S) :T = extractor(whole)
		}

		class OptionalSelector[M <: SingletonMapping { type Owner = O; type Subject = S}, O <: AnyMapping, S, T]
		                      (val lifted :Component[O, T], val pick :S => Option[T])
			extends Selector[M, O, S, T]
		{
			override def surepick = None
		}

		private class CustomSelector[M <: SingletonMapping { type Owner = O; type Subject = S }, O <: AnyMapping, S, T]
		                            (val lifted :Component[O, T], val pick :S => Option[T], val surepick :Option[S => T])
			extends Selector[M, O, S, T]
	}



	trait ColumnFilter {
		def apply[S](mapping :Mapping[S]) :Seq[mapping.Component[_]] =
			mapping.columns.filter(c => apply(mapping :mapping.type, c))

		def apply[M <: AnyMapping, S](mapping :M, column :M#Component[S]) :Boolean

		def read[S](mapping :Mapping[S]) :SQLReadForm[S] =
			MappingReadForm[mapping.Owner, S](mapping, apply(mapping :mapping.type))

		def write[S](mapping :Mapping[S])(forms :mapping.AnyComponent => SQLForm[_]) :SQLWriteForm[S] =
			MappingWriteForm[mapping.Owner, S](mapping, apply(mapping :mapping.type), forms)
	}



	object ColumnFilter {

		def apply(filter :AnyMapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def apply[M <: AnyMapping, T](mapping :M, column :M#Component[T]) :Boolean = filter(column)
			}



		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :Mapping[S]) :Seq[mapping.Component[_]] =
				mapping.components.filter(c => apply(mapping :mapping.type, c))

			def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]): Boolean =
				buff.enabled(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :Mapping[S]) :Seq[mapping.Component[_]] =
				mapping.components.filter(c => apply(mapping :mapping.type, c))

			def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]): Boolean =
				buff.disabled(column)
		}

		class WriteFilter(modifier :BuffType) extends WithoutBuff(modifier) {
			override def read[S](mapping: Mapping[S]) =
				EmptyForm(throw new UnsupportedOperationException(s"$this: read for $mapping"))
		}



		case object ForSelect extends WithoutBuff(NoSelectByDefault) {
			override def read[S](mapping: Mapping[S]) :SQLReadForm[S] = mapping.selectForm
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
			override def apply[S](mapping :Mapping[S]) :Seq[mapping.Component[_]] = mapping.columns
			override def apply[M<:AnyMapping, T](mapping :M, column :M#Component[T]) = true
		}
	}






	private class MappingReadForm[O <: AnyMapping, S] private[Mapping]
			(mapping :Component[O, S], columns :Seq[AnyComponent[O]], read :AnyComponent[O]=>SQLReadForm[_] = (_:AnyComponent[O]).selectForm)
		extends SQLReadForm[S]
	{

		override def opt(position: Int)(res: ResultSet): Option[S] = {
			var i = position
			val columnValues = columns.map{ c => i += 1; read(c).opt(i - 1)(res) }(breakOut) :IndexedSeq[Option[Any]]
			val values = ComponentValues(mapping) { comp =>
				val idx = columns.indexOf(mapping.lift(comp))
				if (idx >= 0) columnValues(idx)
				else None
			}
			mapping.optionally(values)
		}


		override val readColumns: Int = (0 /: columns)(_ + read(_).readColumns)

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

		def apply[O <: AnyMapping, S](mapping :Component[O, S], columns :Seq[AnyComponent[O]],
		                              forms :AnyComponent[O] => SQLReadForm[_] = (_:AnyComponent[O]).selectForm) :SQLReadForm[S] =
			new MappingReadForm[O, S](mapping, columns.indexed, forms)



		def select[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]]) :SQLReadForm[S] = {
			val columns = components.map(mapping.lift(_)).flatMap(_.selectable)

			if (columns.exists(NoSelect.enabled))
				throw new IllegalArgumentException(
					s"Can't create a select form for $mapping using $components: NoSelect buff present among the selection."
				)
			val extra = ExtraSelect.Enabled(mapping) ++: columns
			new MappingReadForm[O, S](mapping, extra.indexed, _.selectForm)
		}

		def defaultSelect[S](mapping :Mapping[S]) :SQLReadForm[S] = {
			val columns = ExtraSelect.Enabled(mapping) ++: mapping.selectable.filter(NoSelectByDefault.disabled)
			new MappingReadForm[mapping.Owner, S](mapping, columns.indexed, _.selectForm)
		}


		def fullSelect[S](mapping :Mapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, (ExtraSelect.Enabled(mapping) ++: mapping.selectable).indexed)



		def autoInsert[S](mapping :Mapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.autoInserted.indexed)

		def autoUpdate[S](mapping :Mapping[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.autoUpdated.indexed)

	}






	private class MappingWriteForm[O <: AnyMapping, S] private (
			mapping :Component[O, S],
			columns :Seq[AnyComponent[O]],
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
					substitute.Value(column) orElse mapping(column).get(subject) match {
						case Some(x) =>
							if (x == null) form.setNull(i)(statement)
							else form.set(i)(statement, x)
						case _ =>
							form.setNull(i)(statement)
					}
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
			val literals = columns.map { c =>
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
			val literals = columns.map(write(_).nullLiteral)
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

		def apply[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]],
		                              write :AnyComponent[O] => SQLWriteForm[_],
		                              replacement :ValuedBuffType = AbstractValuedBuff) :SQLWriteForm[S] =
			new MappingWriteForm[O, S](mapping, components.indexed, replacement, write)


		def query[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]],
		                              write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).queryForm) :SQLWriteForm[S] =
			custom(mapping, components, NoQuery, ExtraQuery, write)

		def defaultQuery[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoQuery, ExtraQuery, _.queryForm)

		def fullQuery[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.queryable, ExtraQuery, _.queryForm)



		def update[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]],
			                           write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).updateForm) :SQLWriteForm[S] =
			custom(mapping, components, NoUpdate, ExtraUpdate, write)

		def defaultUpdate[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoUpdate, ExtraUpdate, _.updateForm)

		def fullUpdate[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraUpdate, _.updateForm)



		def insert[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]],
		                               write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).insertForm) :SQLWriteForm[S] =
			custom(mapping, components, NoInsert, ExtraInsert, write)

		def defaultInsert[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			default(mapping)(NoInsert, ExtraInsert, (_:mapping.AnyComponent).insertForm)

		def fullInsert[S](mapping :Mapping[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraInsert, _.insertForm)



		private def custom[O <: AnyMapping, S](mapping :Component[O, S], components :Seq[AnyComponent[O]],
		                                       prohibit :BuffType, mandatory :ValuedBuffType,
		                                       write :AnyComponent[O] => SQLWriteForm[_]) :SQLWriteForm[S] =
		{
			if (components.exists(prohibit.enabled))
				throw new IllegalArgumentException(
					s"Can't create a write form for $mapping using $components: $prohibit buff present among selection"
				)
			val extra = mandatory.Enabled(mapping) ++: components.flatMap(mapping.lift(_).columns.filter(prohibit.disabled))
			new MappingWriteForm[O, S](mapping, extra.indexed, mandatory, write)
		}

		private def default[S](mapping :Mapping[S])(filterNot :BuffType, mandatory :ValuedBuffType,
		                                            write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] = {
			val columns = mandatory.Enabled(mapping) ++: filterNot.Disabled(mapping)
			new MappingWriteForm[mapping.Owner, S](mapping, columns.indexed, mandatory, write)
		}

		private def full[S](mapping :Mapping[S])(columns :Seq[mapping.AnyComponent], extra :ValuedBuffType,
		                                         write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] =
			new MappingWriteForm[mapping.Owner, S](mapping, (extra.Enabled(mapping) ++: columns).indexed, extra, write)

	}



}
