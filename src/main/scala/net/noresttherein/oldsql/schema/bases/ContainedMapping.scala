package net.noresttherein.oldsql.schema.bases

import java.sql.{CallableStatement, ResultSet}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.model.{PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, MappingExtract, SQLReadForm}
import net.noresttherein.oldsql.schema.support.EmptyMapping
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, NoSelect, OptionalSelect}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StableColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.RelVar
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormNullValue
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping}
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.{ForeignKeyRelationColumnMapping, ForeignKeyRelationMapping}






/** A base trait for mappings with a fixed structure, with columns known statically, and which should be created
  * manually, rather than by some generic code. While it exposes mutator methods to facilitate creation and declaration
  * of columns, it is expected that they'll be used solely in constructors of derived classes by those classes
  * themselves and, once created, it will be seen as immutable from the outside. If an attempt is made to modify
  * this instance once any of its accessor methods for components had been called previously, an exception
  * will be thrown.
  *
  * This mapping is flat: all mapped columns are direct components of this instance. It can have components,
  * but they are nominally empty: any column and component fields of such components are in fact, from the point of view
  * of the framework, components of this mapping, with the components only grouping them into subsets. They are still
  * useful, as they can define [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct]] method
  * in terms of the columns and components of this mapping, assembling some property values of subject `S`.
  * As another restriction, all components must extend
  * [[net.noresttherein.oldsql.schema.bases.ContainedMapping.ContainedComponent ContainedComponent]]; it is impossible
  * to embed as components instances of arbitrary [[net.noresttherein.oldsql.schema.Mapping mapping]] classes.
  *
  * On the plus side, it is an [[net.noresttherein.oldsql.schema.bases.ExportMapping ExportMapping]]: all subcomponents
  * are their own ''export'' versions, making [[net.noresttherein.oldsql.schema.Mapping.export export]]
  * an identity operation, requiring no lookup. This makes
  * [[net.noresttherein.oldsql.haul.ComponentValues.aliased aliasing]] of
  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] used for the assembly more efficient,
  * as retrieving a value of any column becomes (typically) a simple array lookup.
  *
  * It exists exclusively as a way to facilitate creating schema descriptions and its interface is directed
  * towards implementing classes; it is not treated in any special way by other parts of the library,
  * different from other implementations of `Mapping`.
  *
  * @tparam S the `Subject` type of the mapped entity.
  * @tparam O A marker [[net.noresttherein.oldsql.schema.Mapping.Origin]] type, used to distinguish between
  *           several instances of the same mapping class, but coming from different sources (especially different
  *           aliases for a table occurring more then once in a join). At the same time, it adds additional type safety
  *           by ensuring that only components of mappings included in a query can be used in creation
  *           of SQL expressions used by that query.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  */
trait ContainedMapping[S, O]
	extends StaticMapping[S, O] with LazyMapping[S, O] with RelatedMapping[S, O] with ExportMapping
{ root =>

	sealed trait AbstractComponent[T] extends BaseMapping[T, O] with ExportMapping {
		protected[ContainedMapping] def componentSelector :S =?> T

		private[ContainedMapping] final val extract :root.Extract[T] = MappingExtract(this)(componentSelector)

		@inline private[ContainedMapping] final def belongsTo(mapping :ContainedMapping[_, _]) =
			mapping == root

		/** Returns the value of this component in an option if an implicit `ComponentValues` instance
		  * for the enclosing composite mapping is present.
		  */
		@inline final def ?(implicit values :root.Pieces) :Option[T] = values.get(extract)

		/** Buffs 'inherited' from the enclosing mapping. It uses the value extract `S => T` to map all buffs applied
		  * to the enclosing mapping to adapt them to this value type. This means that if there are any
		  * [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]s among the inherited buffs, this component
		  * must have a value for the value associated with that buff or an exception will be thrown - either
		  * when accessing/initializing the buff list or when the value for the buff is actually needed.
		  */ //todo: inherit buffs in extending classes
		protected final def inheritedBuffs :Seq[Buff[T]] = conveyBuffs(componentSelector, toString)
	}



	abstract class ContainedComponent[T](selector :S =?> T, override val buffs :Buff[T]*)
	                                    (implicit override val nullValue :NullValue[T] = NullValue.NotNull)
		extends AbstractComponent[T] with EmptyMapping[T, O] with StaticMapping[T, O]
	{
		protected[ContainedMapping] override def componentSelector :S =?> T = selector

		root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a component of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
		}
	}



	private trait AbstractColumn[T] extends AbstractComponent[T] with ColumnMapping[T, O] {
		protected[ContainedMapping] val index = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a column of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
			columnsBuffer += this
			columnsBuffer.length - 1
		}
	}


	private class ContainedColumn[T](override val name :String, selector :S =?> T, override val buffs :Seq[Buff[T]])
	                                (implicit override val form :ColumnForm[T])
		extends AbstractColumn[T] with SimpleColumn[T, O] with StableColumn[T, O]
	{
		protected[ContainedMapping] override def componentSelector = selector
	}



	private class FKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                         (override val componentSelector :S =?> R, rename :String => String, buffs :Seq[Buff[R]])
	                         (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X])
		extends ForeignKeyRelationMapping[M, C, K, E, T, R, X, O](rename, buffs, factory)(table, pk)
			with AbstractComponent[R]
	{
		def this(selector :S =?> R, prefix :String, buffs :Seq[Buff[R]])
		        (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X]) =
			this(selector, prefix + _, buffs)(factory, table, pk)

		root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a component of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
		}
	}


	private class FKColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X]
	                      (override val componentSelector :S =?> R, name :String, buffs :Seq[Buff[R]])
	                      (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => ColumnMapping[K, X])
		extends ForeignKeyRelationColumnMapping[M, K, E, T, R, X, O](name, buffs, factory)(table, pk)
			with AbstractColumn[R]



//	private class FKColumn[M[A] <: RefinedMapping[E, A], K, E, R]
//	                      (name :String, selector :S =?> R, factory :RelatedEntityFactory[K, E, _, R], buffs :Buff[R]*)
//	                      (override val table :RelVar[M], pk : => ColumnMapping[K, _])
//		extends ContainedColumn[R](name, selector, buffs :_*) with ForeignKeyColumnMapping[K, R, O]
//	{
//		override lazy val target = pk.withOrigin[Target]
//		override val key =
//	}



	override def apply[T](component :Component[T]) :Extract[T] = component match {
		case comp :ContainedMapping[_, _]#ContainedComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[Extract[T]]
		case _ =>
			throw new IllegalArgumentException(s"$component is not a component of $this.")
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
		case comp :ContainedMapping[_, _]#ContainedComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[ColumnExtract[T]]
		case _ =>
			throw new IllegalArgumentException(s"$column is not a column of $this.")
	}

	private[this] var componentsBuffer :ListBuffer[AbstractComponent[_]] = ListBuffer.empty
	private[this] var columnsBuffer :ListBuffer[AbstractColumn[_]] = ListBuffer.empty

	private val lazyComponents = Lazy(synchronized {
		val comps = Unique.from(componentsBuffer)
		componentsBuffer = null
		comps
	})

	private val lazyColumns = Lazy(synchronized {
		lazyComponents.get
		val cols = Unique.from(columnsBuffer)
		columnsBuffer = null
		cols
	})

	private[this] val lazyExtracts = Lazy {
		def entry[T](component :AbstractComponent[T]) =
			Assoc[Component, Extract, T](component, component.extract)

		NaturalMap(lazyComponents.map(entry(_)) :_*)
	}

	final override def extracts :ExtractMap = lazyExtracts
	final override def components :Unique[Component[_]] = lazyComponents
	final override def subcomponents :Unique[Component[_]] = lazyComponents
	final override def columns :Unique[Column[_]] = lazyColumns



	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FILTER, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UPDATE, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(INSERT, subject)

	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] = {
		val res = new IndexedColumnValuesBuilder
		writtenValues(op, subject, res) //StaticMapping delegates this to specific methods
		res.result()
	}


	private class IndexedColumnValuesBuilder extends ComponentValuesBuilder[S, O] {
		private[this] val columns = lazyColumns.get
		private[this] val columnCount = columns.size
		private[this] val values = new Array[Option[Any]](columnCount)
		private[this] var componentValues = Map.empty[RefinedMapping[_, O], Option[_]]

		override def add[T](component :RefinedMapping[T, O], result :Option[T]) :this.type = export(component) match {
			case col :ContainedMapping[_, _]#AbstractColumn[_] =>
				values(col.index) = result; this
			case _ =>
				componentValues = componentValues.updated(component, result)
				this
		}

		override def result() =
			if (componentValues.isEmpty)
				ColumnValues(ArraySeq.unsafeWrapArray(values)) {
					case column :ContainedMapping[_, _]#AbstractColumn[_] => column.index
					case _ => -1
				}
			else {

				var i = 0
				while (i < columnCount) {
					val value = values(i)
					if (value != null)
						componentValues = componentValues.updated(columns(i), value)
					i += 1
				}
				ComponentValues(componentValues)
			}
	}



	private class ReadForm(columns :Unique[Column[_]],
	                       read :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
	extends SQLReadForm[S] with ReadFormNullValue[S] {
		override val nulls = root.nullValue

		private[this] val fastColumns = columns.view.map {
			case col :ContainedMapping[_, _]#AbstractColumn[_] if col belongsTo root => col
			case col => throw new IllegalArgumentException(s"$col is not a column of $root.")
		}.toArray
		private[this] val forms = fastColumns.map(read)

		override def readColumns :Int = fastColumns.length

		override def opt(res :ResultSet, position :Int) :Option[S] = {
			val vals = new Array[Option[Any]](fastColumns.length)
			java.util.Arrays.fill(vals.asInstanceOf[Array[AnyRef]], None)
			var i = 0
			while (i < forms.length) {
				vals(fastColumns(i).index) = forms(i).opt(res, position + i)
				i += 1
			}
			val pieces = ColumnValues(root)(ArraySeq.unsafeWrapArray(vals)) {
				case col :ContainedMapping[_, _]#AbstractColumn[_] => col.index
				case _ => -1
			}

			optionally(pieces)
		}

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = 0; val end = fastColumns.length
			while (i < end) {
				forms(i).register(call, position + i)
				i += 1
			}
		}

		override def toString :String = columns.map(read).mkString(s"$root{", ",", "}>")
	}



	private[this] val lazySelectForm = Lazy(new ReadForm(selectedByDefault))

	override def selectForm :SQLReadForm[S] = lazySelectForm

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val selected = components map {
			case col :ColumnMapping[_, O @unchecked] => col
			case comp => throw new IllegalArgumentException(
				s"$comp is not a column of $this. ContainedMapping.selectForm accepts only columns."
			)
		}

		if (selected.exists(NoSelect.enabled))
			throw new IllegalArgumentException(
				s"Can't create a select form for $root using $components: NoSelect buff present among the selection."
			)
		if (selectable.exists(c => !columns.contains(c) && OptionalSelect.disabled(c))) {
			val missing = (selectable.toSet -- columns.toSet).filter(OptionalSelect.disabled)
			throw new IllegalArgumentException(
				missing.mkString(
					s"Can't create a select form for $root using $components: missing mandatory columns ", ", ", ""
				)
			)
		}
		val extra = selected ++ ExtraSelect.Enabled(root)
		new ReadForm(selected :++ extra)
	}



	protected def conveyBuffs[T](extractor :Extractor[S, T], component: => String = "") :Seq[Buff[T]] =
		if (buffs == null)
			throw new IllegalStateException(s"$this.buffs is null: overrides must happen before any component declarations.")
		else
			schema.cascadeBuffs(buffs, toString + " to subcomponent '" + component + "'")(extractor)






	protected def column[T :ColumnForm](name :String, property :S => T, buffs :Buff[T]*) :Column[T] =
		new ContainedColumn[T](name, property, buffs)

	protected def column[T](property :S => T, buffs :Buff[T]*)
	                       (implicit form :ColumnForm[T], tag :TypeTag[S]) :Column[T] =
		column(PropertyPath.nameOf(property), property, buffs :_*)

	protected def optcolumn[T :ColumnForm](name :String, property :S => Option[T], buffs :Buff[T]*) :Column[T] =
		new ContainedColumn[T](name, property, buffs)

//	protected def optcolumn[T](property :S => Option[T], buffs :Buff[T]*)
//	                          (implicit form :ColumnForm[T], tag :TypeTag[S]) :Column[T] =
//		optcolumn(PropertyPath.nameOf(property), property, buffs :_*) //fixme: this name likely doesn't make sense


	protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                             (name :String, property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => ColumnMapping[K, _],
	                              factory :RelatedEntityFactory[K, E, T, R]) :ForeignKeyColumnMapping[K, R, O] =
		new FKColumn[M, K, E, T, R, ()](property, name, buffs)(factory, table, key.asInstanceOf[M[()] => ColumnMapping[K, ()]])


	protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                             (property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, T, R])
	                             (rename :String => String) :ForeignKeyMapping[C, K, R, O] =
		new FKComponent[M, C, K, E, T, R, ()](property, rename, buffs)(factory, table, key.asInstanceOf[M[()] => C[()]])



}
