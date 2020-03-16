package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Unique.implicitUnique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, MappingReadForm, MappingWriteForm, ComponentSelector}
import net.noresttherein.oldsql.schema.SQLForm.EmptyForm
import net.noresttherein.oldsql.schema.support.{MappedMapping, PrefixedMapping, MappedAs}
import net.noresttherein.oldsql.schema.Buff.{AbstractValuedBuff, AutoInsert, AutoUpdate, BuffType, ExplicitSelect, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValuedBuffType}
import net.noresttherein.oldsql.schema.Mapping.GeneralSelector
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.slang._





/** Root interface of the  mapping class hierarchy to be used when the mapped type is of little importance (for example
  * in collections of mappings for various types). All implementations are required to extend `Mapping[T]` instead,
  * which is parameterized with the mapped type. The main reason for this duplicity is the limitation of scala type
  * inference: a generic method `m[M&lt;:Mapping[T], T](m :M)` will infer type parameters `M`, `Nothing` when given
  * an argument of type `M&lt;:Mapping[T]`. On the other hand, when a type parameter is declared `M&lt;:Mapping[_]`,
  * the `Subject` is instantiated early as 'Any' and it is not equal to `m.Subject`, leading to other problems later.
  */
sealed trait Mapping { mapping =>
	/** The mapped entity type. */
	type Subject

	/** The mapping which this instance is a part of. This may be the mapping directly enclosing this mapping,
	  * but more typically will be a 'root' mapping, such as a table mapping. The 'Owner' tag is passed down
	  * the components comprising such a mapping in order to identify them as containing columns being part of
	  * the owner mapping, thus adding type safety when creating sql formulas, which can thus enforce that
	  * only components/columns belonging to the selected tables (or otherwise root mappings) can be used
	  * in the where or select clause.
	  */
	type Owner// <: Mapping

	/** A container with values for components of this mapping required to assemble the subject. */
	type Pieces = ComponentValues[this.type]

	type Selector[T] = ComponentSelector[this.type, Owner, Subject, T]

	type AnyComponent = Component[_]
	type Component[T] = Mapping.Component[Owner, T] //<: SubMapping[T, Owner]
	type Column[T] = ColumnMapping[Owner, T]

//	def asComponent :this.type with Mapping[Subject] { type Owner = mapping.Owner } //Component[Subject]


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



	def apply[T](component :Component[T]) :Selector[T]



	/** Direct component mappings of this mapping, including any top-level columns. */
	def components :Unique[Component[_]]

	/** All transitive components of this mapping (i.e. components/columns declared by it's components or
	  * other subcomponents), or all that this mapping cares to expose, as instances of this.Component[_].
	  * This list should include all selectable columns.
	  */
	def subcomponents :Unique[Component[_]] //= components.flatMap(_.subcomponents)


	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only,
	  * write-only and so on. */
	def columns :Unique[Component[_]] //= components.flatMap(_.columns)
	/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` flag). */
	def selectable :Unique[Component[_]] = columnsWithout(NoSelectByDefault)
	/** All columns which can be part of an SQL statement's where clause (don't have the `NoQuery` flag set). */
	def queryable :Unique[Component[_]] = columnsWithout(NoQueryByDefault)
	/** All columns which can be updated on existing database records (don't have the `NoUpdate` flag set). */
	def updatable :Unique[Component[_]] = columnsWithout(NoUpdateByDefault)
	/** All columns which are updated by the database on each update statement (and should be returned to the application). */
	def autoUpdated :Unique[Component[_]] = columnsWith(AutoUpdate)
	/** All columns which can occur in an insert statement (don't have the `NoInsert` flag set). */
	def insertable :Unique[Component[_]] = columnsWithout(NoInsertByDefault)
	/** Columns autogenerated by the database; this implies being non-insertable. */
	def autoInserted :Unique[Component[_]] = columnsWith(AutoInsert)

	/** All columns with a given buff provided. */
	def columnsWith(buff :BuffType) :Unique[Component[_]] =
		columns.filter(buff.enabled)

	/** All columns without the given buff. */
	def columnsWithout(buff :BuffType) :Unique[Component[_]] =
		columns.filter(buff.disabled)


	/** Read form of a select statement for this mapping including the given components of this mapping. */
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
	  * only on columns. If a larger component declares a buff, it should be inherited by all its subcomponents
	  * (and, transitively, by all columns). As those subcomponents can in turn have their own buffs attached,
	  * care needs to be taken in order to avoid conflicts and undesired interactions.
	  */
	def buffs :Seq[Buff[Subject]]



	/** Attempts to retrieve or assemble the value of `Subject` from the passed `ComponentValues` for this instance.
	  * Standard implementation will test several sources together with `values` before giving up:
	  * a ready value present for this mapping, assembling the result from subcomponents and finally,
	  * a default coming from an attached `OptionalBuff` (or related). By default it forwards to
	  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] and should stay consistent with it.
	  * @throws NoSuchElementException if no value can be provided (`optionally` returns `None`).
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */
	def apply(values: Pieces): Subject =
		optionally(values) getOrElse {
			throw new NoSuchElementException(s"Can't assemble $this from $values.")
		}

	/** Attempts to retrieve or assemble the value for the mapped `Subject` from the given `ComponentValues`.
	  * This is the top-level method which can, together with passed `values`, produce the result in several ways.
	  * By default it forwards the call to the [[net.noresttherein.oldsql.schema.ComponentValues.result result]] method
	  * of `ComponentValues` (which, by default, will first check if it has a predefined value stored for this mapping,
	  * and, only if not, forward to this instance's [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
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
	  * @see [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]
	  */
	def optionally(values: Pieces): Option[Subject]

	/** Attempts to assemble the value of this mapping from the values of subcomponents stored in the passed
	  * `ComponentValues`. This is the final dispatch target of other constructor methods declared here or
	  * in [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]] and should not be called directly.
	  * @see [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]]
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply apply]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.value ComponentValues.value]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues.getValue ComponentValues.getValue]]
	  */
	def assemble(values :Pieces) :Option[Subject]

	def nullValue :Option[Subject] //todo: why do we need it at all?



	def valueOf[T](component :Component[T], subject :Subject) :Option[T] = apply(component).get(subject)

	def valueOf[T](component :Component[T], values :Pieces) :Option[T] = apply(component).get(values)

	def apply[T](component :Component[T], subject :Subject) :T = apply(component)(subject)

	def apply[T](component :Component[T], values :Pieces) :T = apply(component)(values)



	def sqlName :Option[String] = None

	def isSymLink[T](component :Component[T]) :Boolean = ??? /*symLinkTarget(component).isDefined

	def symLinkTarget[T](component :Component[T]) :Option[ComponentPath[this.type, _<:Mapping]] =
		SymLink.startsWith[this.type, T](this :this.type)(component).map(_.target)
*/

	def qualified(prefix :String) :Component[Subject]

	def prefixed(prefix :String) :Component[Subject]

	def inOption :OptionMapping[this.type, Owner, Subject] = OptionMapping(this)

	def map[X](there :Subject => X, back :X => Subject) :this.type MappedAs X =
		MappedMapping[this.type, Owner, Subject, X](this, there, back)

	def optmap[X](there :Subject => Option[X], back :X => Option[Subject]) :this.type MappedAs X =
		MappedMapping.opt[this.type, Owner, Subject, X](this, there, back)



	override def toString :String = sqlName getOrElse this.unqualifiedClassName

	def introString :String = components.mkString(this + "{", ", ", "}")


}






trait BaseMapping[S] extends Mapping { self =>

	type Subject = S

	def buffs :Seq[Buff[S]] = Nil

//	override def asComponent :this.type = this


	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
		MappingReadForm.select(this :this.type, components)

	override def selectForm: SQLReadForm[S] = MappingReadForm.defaultSelect(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.defaultQuery(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.defaultUpdate(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.defaultInsert(this)
	//fixme: ColumnFilter needs to know what type of read/write column form to use
//	override def writeForm(filter :ColumnFilter) :SQLWriteForm[S] = filter.write(this)
//	override def readForm(filter :ColumnFilter) :SQLReadForm[S] = filter.read(this)



	override def apply(values: Pieces): S =
		optionally(values) getOrElse {
			throw new IllegalArgumentException(s"Can't assemble $this from $values")
		}

	override def optionally(values: Pieces): Option[S] = //todo: perhaps extract this to MappingReadForm for speed (not really needed as the buffs cascaded to columns anyway)
		values.result(this) map { res => (res /: SelectAudit.Audit(this)) { (acc, f) => f(acc) } } orElse
			OptionalSelect.Value(this) orElse ExtraSelect.Value(this)

	def assemble(values :Pieces) :Option[S]

	def nullValue :Option[S] = None



	override def qualified(prefix :String) :Component[S] = PrefixedMapping.qualified(prefix, this)

	override def prefixed(prefix :String) :Component[S] = PrefixedMapping(prefix, this)

//	override def inOption :OptionMapping[this.type, Owner, S] = OptionMapping(this)

}



trait SubMapping[O, S] extends BaseMapping[S] {
	type Owner = O
}



trait RootMapping[S] extends BaseMapping[S] {
	type Owner = this.type
}






object Mapping {
	type TypedMapping[O, S] = SubMapping[O, S]
	type Component[O, S] = Mapping { type Owner = O; type Subject = S }
	type ComponentFor[S] = Mapping { type Subject = S }
	type AnyComponent[O] = Mapping { type Owner = O }

	type SingletonComponent[O, S] = Component[O, S] with Singleton //Mapping with Singleton { type Owner = O; type Subject = S }
	type SingletonFor[S] = Mapping with Singleton { type Subject = S }
	type AnySingleton[O] = Mapping with Singleton { type Owner = O }
	type SingletonMapping = Mapping with Singleton

	type ComponentCompatibleMapping[M <: Mapping] = Mapping {
		type Component[X] = Mapping.Component[M#Owner, X]
	}

	type CompatibleMapping[M <: Mapping] = Mapping {
		type Owner = M#Owner
		type Subject = M#Subject
//		type Component[X] = M#Component[X]
	}

	type TypeCompatibleMapping[M <: Mapping] = Mapping {
		type Subject = M#Subject
	}






	sealed trait GeneralSelector[M <: Mapping, T] {
		def optional :M#Subject => Option[T]
		def requisite :Option[M#Subject => T]
		def extractor :Extractor[M#Subject, T]

		def apply(whole :M#Subject) :T

		def get(whole :M#Subject) :Option[T]

		def apply(values :ComponentValues[M]) :T

		def get(values :ComponentValues[M]) :Option[T]

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
	  * @see [[net.noresttherein.oldsql.schema.Mapping.apply[T](Component[T] ]]
	  * @see [[net.noresttherein.oldsql.schema.ComponentValues ComponentValues]]
	  */
	trait ComponentSelector[M <: Component[O, S], O, S, T]
		extends GeneralSelector[M, T]
	{
		def optional :S => Option[T]
		def requisite :Option[S => T]

		def extractor :Extractor[S, T] = requisite.map(Extractor.requisite[S, T]) getOrElse Extractor(optional)

		def apply(whole :S) :T = get(whole) getOrElse {
			throw new NoSuchElementException(s"No value for $lifted in $whole.")
		}

		def get(whole :S) :Option[T] = optional(whole)

		def apply(values :ComponentValues[M]) :T = values(this)

		def get(values :ComponentValues[M]) :Option[T] = values.get(this)

		val lifted :Component[O, T]

		override def toString :String = "Selector(" + lifted + ")"
	}



	object ComponentSelector {
		def apply[M <: SingletonComponent[O, S], O, S, T]
		         (component :Component[O, T], pick :S => Option[T], surepick :Option[S=>T]) :ComponentSelector[M, O, S, T] =
			new CustomSelector(component, pick, surepick)

		def apply[M <: SingletonComponent[O, S], O, S, T]
		         (component :Component[O, T])(extractor :Extractor[S, T]) :ComponentSelector[M, O, S, T] =
			extractor match {
				case requisite :RequisiteExtractor[S, T] => new RequisiteSelector(component, requisite)
				case _ => new ExtractorSelector(component, extractor)
			}

		@inline def apply[O, S, T](mapping :Component[O, S], component :Component[O, T])
		                          (extractor :Extractor[S, T]) :ComponentSelector[mapping.type, O, S, T] =
			apply[mapping.type, O, S, T](component)(extractor)



		def req[M <: SingletonComponent[O, S], O, S, T]
		       (component :Component[O, T], requisite :S => T) :ComponentSelector[M, O, S, T] =
			new RequisiteSelector[M, O, S, T](component, Extractor.requisite(requisite))

		def req[O, S, T](mapping :Component[O, S], component :Component[O, T])(requisite :S => T) :ComponentSelector[mapping.type, O, S, T] =
			new RequisiteSelector[mapping.type, O, S, T](component, Extractor.requisite(requisite))



		def opt[M <: SingletonComponent[O, S], O, S, T]
		       (component :Component[O, T], selector :S => Option[T]) :ComponentSelector[M, O, S, T] =
			new OptionalSelector[M, O, S, T](component, selector)

		def opt[O, S, T](mapping :Component[O, S], component :Component[O, T])(selector :S => Option[T]) :ComponentSelector[mapping.type, O, S, T] =
			new OptionalSelector[mapping.type, O, S, T](component, selector)



		def ident[M <: SingletonComponent[O, S], O, S]
		         (component :Component[O, S]) :ComponentSelector[M, O, S, S] =
			new RequisiteSelector[M, O, S, S](component, Extractor.ident[S]) {
				override def apply(whole :S) :S = whole
				override def get(whole :S) :Option[S] = Some(whole)
			}

		def ident[O, S](mapping :Component[O, S], component :Component[O, S]) :ComponentSelector[mapping.type, O, S, S] =
			ident[mapping.type, O, S](component)



		def unapply[O, T, S](selector :ComponentSelector[_ <: SingletonMapping { type Owner = O; type Subject = S }, O, S, T])
				:Option[(S=>Option[T], Option[S=>T], Component[O, T])] =
			Some(selector.optional, selector.requisite, selector.lifted)



		private class ExtractorSelector[M <: SingletonComponent[O, S], O, S, T]
		                               (val lifted :Component[O, T], extract :Extractor[S, T])
			extends ComponentSelector[M, O, S, T]
		{
			override def extractor :Extractor[S, T] = extract
			override def optional :S => Option[T] = extract.optional
			override def requisite :Option[S => T] = extract.requisite

			override def get(whole :S) :Option[T] = extract(whole)
		}

		private class RequisiteSelector[M <: SingletonComponent[O, S], O, S, T]
		                               (lifted :Component[O, T], extract :RequisiteExtractor[S, T])
			extends ExtractorSelector[M, O, S, T](lifted, extract)
		{
			override def extractor :RequisiteExtractor[S, T] = extract
			override def apply(whole :S) :T = extractor.get(whole)
		}

		private class OptionalSelector[M <: SingletonComponent[O, S], O, S, T]
		                              (val lifted :Component[O, T], val optional :S => Option[T])
			extends ComponentSelector[M, O, S, T]
		{
			override def requisite :Option[Nothing] = None
		}

		private class CustomSelector[M <: SingletonComponent[O, S], O, S, T]
		                            (val lifted :Component[O, T], val optional :S => Option[T], val requisite :Option[S => T])
			extends ComponentSelector[M, O, S, T]
	}






	trait ColumnFilter {
		def apply[S](mapping :ComponentFor[S]) :Unique[mapping.Component[_]] =
			mapping.columns.filter(test[mapping.Owner])

		def filter[O](columns :Unique[AnyComponent[O]]) :Unique[AnyComponent[O]] =
			columns.filter(test[O])

		def test[O](column :AnyComponent[O]) :Boolean

		def read[S](mapping :ComponentFor[S]) :SQLReadForm[S] =
			MappingReadForm[mapping.Owner, S](mapping, apply(mapping :mapping.type))

		def write[S](mapping :ComponentFor[S])(forms :mapping.AnyComponent => SQLForm[_]) :SQLWriteForm[S] =
			MappingWriteForm[mapping.Owner, S](mapping, apply(mapping :mapping.type), forms)
	}



	object ColumnFilter {

		def apply(pred :Mapping => Boolean) :ColumnFilter =
			new ColumnFilter {
				override def test[O](column :AnyComponent[O]) :Boolean = pred(column)
			}



		class WithBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :ComponentFor[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :AnyComponent[O]): Boolean = buff.enabled(column)
		}

		class WithoutBuff(buff :BuffType) extends ColumnFilter {
			def components[S](mapping :ComponentFor[S]) :Unique[mapping.Component[_]] =
				mapping.components.filter(test)

			def test[O](column :AnyComponent[O]): Boolean = buff.disabled(column)
		}

		class WriteFilter(modifier :BuffType) extends WithoutBuff(modifier) {
			override def read[S](mapping: ComponentFor[S]) =
				EmptyForm(throw new UnsupportedOperationException(s"$this: read for $mapping"))
		}



		case object ForSelect extends WithoutBuff(NoSelectByDefault) {
			override def read[S](mapping: ComponentFor[S]) :SQLReadForm[S] = mapping.selectForm
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
			override def apply[S](mapping :ComponentFor[S]) :Unique[mapping.Component[_]] = mapping.columns
			override def filter[O](columns :Unique[AnyComponent[O]]) :Unique[AnyComponent[O]] = columns
			override def test[O](column :AnyComponent[O]) = true
		}
	}






	private class MappingReadForm[O, S] private[Mapping]
			(mapping :Component[O, S], columns :Unique[AnyComponent[O]], read :AnyComponent[O]=>SQLReadForm[_] = (_:AnyComponent[O]).selectForm)
		extends SQLReadForm[S]
	{

		override def opt(position: Int)(res: ResultSet): Option[S] = {
			var i = position
			val columnValues = columns.toIndexedSeq.map{ c => i += 1; read(c).opt(i - 1)(res) }
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

		def defaultSelect[S](mapping :ComponentFor[S]) :SQLReadForm[S] = {
			val columns = mapping.selectable.filter(NoSelectByDefault.disabled) ++ ExtraSelect.Enabled(mapping)
			new MappingReadForm[mapping.Owner, S](mapping, columns, _.selectForm)
		}


		def fullSelect[S](mapping :ComponentFor[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.selectable ++ ExtraSelect.Enabled(mapping))



		def autoInsert[S](mapping :ComponentFor[S]) :SQLReadForm[S] =
			new MappingReadForm[mapping.Owner, S](mapping, mapping.autoInserted)

		def autoUpdate[S](mapping :ComponentFor[S]) :SQLReadForm[S] =
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

		def defaultQuery[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
			default(mapping)(NoQuery, ExtraQuery, _.queryForm)

		def fullQuery[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.queryable, ExtraQuery, _.queryForm)



		def update[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
			                           write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).updateForm) :SQLWriteForm[S] =
			custom(mapping, components, NoUpdate, ExtraUpdate, write)

		def defaultUpdate[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
			default(mapping)(NoUpdate, ExtraUpdate, _.updateForm)

		def fullUpdate[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
			full(mapping :mapping.type)(mapping.insertable, ExtraUpdate, _.updateForm)



		def insert[O, S](mapping :Component[O, S], components :Unique[AnyComponent[O]],
		                               write :AnyComponent[O] => SQLWriteForm[_] = (_:AnyComponent[O]).insertForm) :SQLWriteForm[S] =
			custom(mapping, components, NoInsert, ExtraInsert, write)

		def defaultInsert[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
			default(mapping)(NoInsert, ExtraInsert, (_:mapping.AnyComponent).insertForm)

		def fullInsert[S](mapping :ComponentFor[S]) :SQLWriteForm[S] =
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

		private def default[S](mapping :ComponentFor[S])(filterNot :BuffType, mandatory :ValuedBuffType,
		                                                 write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] = {
			val columns = filterNot.Disabled(mapping) ++ mandatory.Enabled(mapping)
			new MappingWriteForm[mapping.Owner, S](mapping, columns, mandatory, write)
		}

		private def full[S](mapping :ComponentFor[S])(columns :Unique[mapping.AnyComponent], extra :ValuedBuffType,
		                                              write :mapping.AnyComponent => SQLWriteForm[_]) :SQLWriteForm[S] =
			new MappingWriteForm[mapping.Owner, S](mapping, columns ++ extra.Enabled(mapping), extra, write)

	}



}
