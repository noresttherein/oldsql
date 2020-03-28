package net.noresttherein.oldsql.schema

import java.sql.{PreparedStatement, ResultSet}

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, IdentityExtractor, OptionalExtractor, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Mapping.{ColumnFilter, ComponentExtractor, ConcreteSubclass, MappingReadForm, MappingWriteForm}
import net.noresttherein.oldsql.schema.SQLForm.{EmptyForm, NullValue}
import net.noresttherein.oldsql.schema.support.{MappedMapping, PrefixedMapping}
import net.noresttherein.oldsql.schema.Buff.{AbstractValuedBuff, AutoInsert, AutoUpdate, BuffType, ExplicitSelect, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, SelectAudit, ValuedBuffType}
import net.noresttherein.oldsql.schema.bits.OptionMapping
import net.noresttherein.oldsql.schema.MappingPath.{ComponentPath, SelfPath}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, AdaptedAs}
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.slang.InferTypeParams.IsBoth





/** Root interface of the  mapping class hierarchy to be used when the mapped type is of little importance (for example
  * in collections of mappings for various types). All implementations are required to extend `Mapping[T]` instead,
  * which is parameterized with the mapped type. The main reason for this duplicity is the limitation of scala type
  * inference: a generic method `m[M&lt;:Mapping[T], T](m :M)` will infer type parameters `M`, `Nothing` when given
  * an argument of type `M&lt;:Mapping[T]`. On the other hand, when a type parameter is declared `M&lt;:Mapping[_]`,
  * the `Subject` is instantiated early as 'Any' and it is not equal to `m.Subject`, leading to other problems later.
  * @see [[net.noresttherein.oldsql.schema.GenericMapping]]
  */
trait Mapping { mapping =>
	/** The mapped entity type. */
	type Subject

	/** The mapping which this instance is a part of. This may be the mapping directly enclosing this mapping,
	  * but more typically will be a 'root' mapping, such as a table mapping. The 'Owner' tag is passed down
	  * the components comprising such a mapping in order to identify them as containing columns being part of
	  * the owner mapping, thus adding type safety when creating sql formulas, which can thus enforce that
	  * only components/columns belonging to the selected tables (or otherwise root mappings) can be used
	  * in the where or select clause.
	  */
	type Owner

	/** A container with values for components of this mapping required to assemble the subject. */
	type Pieces = ComponentValues[_ >: this.type <: Component[Subject]]

	type Selector[T] = ComponentExtractor[Owner, Subject, T]

	type AnyComponent = Component[_]
	type Component[T] = Mapping.Component[Owner, T]
	type Column[T] = ColumnMapping[Owner, T]



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
	def subcomponents :Unique[Component[_]]


	/** All direct and transitive columns declared within this mapping. This will include columns which are read-only,
	  * write-only and so on. */
	def columns :Unique[Component[_]]
	/** All columns which can be listed in the select clause of an SQL statement (don't have the `NoSelect` flag). */
	def selectable :Unique[Component[_]] = columnsWithout(NoSelect)
	/** All columns which can be part of an SQL statement's where clause (don't have the `NoQuery` flag set). */
	def queryable :Unique[Component[_]] = columnsWithout(NoQuery)
	/** All columns which can be updated on existing database records (don't have the `NoUpdate` flag set). */
	def updatable :Unique[Component[_]] = columnsWithout(NoUpdate)
	/** All columns which are updated by the database on each update statement (and should be returned to the application). */
	def autoUpdated :Unique[Component[_]] = columnsWith(AutoUpdate)
	/** All columns which can occur in an insert statement (don't have the `NoInsert` flag set). */
	def insertable :Unique[Component[_]] = columnsWithout(NoInsert)
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
	def apply(pieces: Pieces): Subject =
		optionally(pieces) getOrElse {
			throw new NoSuchElementException(s"Can't assemble $this from $pieces.")
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


	def pick[T](component :Component[T], subject :Subject) :Option[T] = apply(component).get(subject)

	def pick[T](component :Component[T], pieces :Pieces) :Option[T] = pieces.get(apply(component))

	def apply[T](component :Component[T], subject :Subject) :T = apply(component)(subject)

	def apply[T](component :Component[T], pieces :Pieces) :T = pieces(apply(component))



	def sqlName :Option[String] = None

	def qualified(prefix :String) :Component[Subject]

	def prefixed(prefix :String) :Component[Subject]



	def inOption :OptionMapping[this.type, Owner, Subject] = OptionMapping(this)

	def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X] = null) :Component[X] =
		MappedMapping[this.type, Owner, Subject, X](this, there, back)

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






trait GenericMapping[O, S] extends Mapping { self =>
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


	def qualified(prefix :String) :Component[Subject] =
		if (prefix.length == 0) this
		else PrefixedMapping(prefix + ".", this)

	def prefixed(prefix :String) :Component[Subject] =
		if (prefix.length == 0) this
		else PrefixedMapping(prefix, this)


}



trait RootMapping[O, S] extends GenericMapping[O, S] {
	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased { c => apply[c.Subject](c).lifted })
}






object Mapping {
	@inline
	implicit def mappingSelfPath[M <: Mapping, X <: Component[O, S], O, S]
	                            (mapping :M)(implicit help :IsBoth[M, X, Component[O, S]]) :SelfPath[X, O, S] =
		SelfPath.typed[X, O, S](mapping)



	type TypedMapping[S] = Mapping { type Subject = S }
	type AnyComponent[O] = Mapping { type Owner = O }
	type Component[O, S] = Mapping { type Owner = O; type Subject = S }

	type SingletonMapping = Mapping with Singleton
	type TypedSingleton[S] = Mapping with Singleton { type Subject = S }
	type AnySingleton[O] = Mapping with Singleton { type Owner = O }
	type SingletonComponent[O, S] = Component[O, S] with Singleton //Mapping with Singleton { type Owner = O; type Subject = S }


	type CompatibleMapping[M <: Mapping] = Mapping {
		type Owner = M#Owner
		type Subject = M#Subject
	}

	type TypeCompatibleMapping[M <: Mapping] = Mapping {
		type Subject = M#Subject
	}

	type ConcreteSubclass[M <: Mapping] = M {
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
		val lifted :Component[O, T]

		override def compose[X](extractor :Extractor[X, S]) :ComponentExtractor[O, X, T] = extractor match {
			case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[O, X, T]]
			case req :RequisiteExtractor[X, S] =>
				ComponentExtractor.opt(lifted)(req.getter andThen optional)
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
				case requisite :RequisiteExtractor[S, T] => new RequisiteComponent(component, requisite.getter)
				case _ => new OptionalComponent(component, extractor.optional)
			}



		def req[O, S, T](component :Component[O, T])(requisite :S => T) :ComponentExtractor[O, S, T] =
			new RequisiteComponent[O, S, T](component, requisite)



		def opt[O, S, T](component :Component[O, T])( selector :S => Option[T]) :ComponentExtractor[O, S, T] =
			new OptionalComponent[O, S, T](component, selector)



		def ident[O, S](component :Component[O, S]) :ComponentExtractor[O, S, S] =
			new IdentityComponent[O, S](component)



		def unapply[O, S, T](selector :ComponentExtractor[O, S, T]) :Option[(S => Option[T], Option[S => T], Component[O, T])] =
			Some(selector.optional, selector.requisite, selector.lifted)



		class OptionalComponent[O, S, T](final val lifted :Component[O, T], final override val optional :S => Option[T])
			extends ComponentExtractor[O, S, T] with OptionalExtractor[S, T]
		{
			override def get(x :S) :Option[T] = optional(x)
		}



		class RequisiteComponent[O, S, T](final val lifted :Component[O, T], override val getter :S => T)
			extends ComponentExtractor[O, S, T] with RequisiteExtractor[S, T]
		{
			override def apply(x :S) :T = getter(x)

			override def compose[X](extractor :X =?> S) :ComponentExtractor[O, X, T] = extractor match {
				case _ :IdentityExtractor[_] => this.asInstanceOf[ComponentExtractor[O, X, T]]
				case req :RequisiteExtractor[X, S] =>
					new RequisiteComponent[O, X, T](lifted, req.getter andThen getter)
				case _ =>
					val first = extractor.optional
					new OptionalComponent[O, X, T](lifted, first(_).map(getter))
			}

			override def compose[W](extractor :RequisiteExtractor[W, S]) :RequisiteComponent[O, W, T] = extractor match {
				case _ :IdentityExtractor[_] => this.asInstanceOf[RequisiteComponent[O, W, T]]
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
