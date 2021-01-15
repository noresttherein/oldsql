package net.noresttherein.oldsql.schema.bases

import java.sql.{CallableStatement, ResultSet}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.model.{PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Optional}
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.schema.{cascadeBuffs, filterColumnExtracts, Buff, ColumnExtract, ColumnForm, ColumnMapping, MappingExtract, SQLReadForm}
import net.noresttherein.oldsql.schema.support.{EffectivelyEmptyMapping, EmptyMapping}
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
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.{AbstractForeignKeyEntityMapping, ForeignKeyEntityColumnMapping, ForeignKeyEntityMapping, InverseForeignKeyMapping}
import net.noresttherein.oldsql.schema.bits.OptionMapping.{MappedOptionMapping, Optional}
import net.noresttherein.oldsql.schema.support.MappingProxy.{OpaqueColumnProxy, OpaqueProxy}






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
  * [[net.noresttherein.oldsql.schema.bases.FlatMapping.FlatComponent FlatComponent]]; it is impossible
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
trait FlatMapping[S, O]
	extends StaticMapping[S, O] with LazyMapping[S, O] with RelatedMapping[S, O] with ExportMapping
{ root =>

	/** Base trait for all components of `FlatMapping`, including columns, synthetic relationship components
	  * and user components. Components for business model objects should be derived from
	  * [[net.noresttherein.oldsql.schema.bases.FlatMapping.FlatComponent FlatComponent]] rather than this trait.
	  */
	sealed trait AbstractComponent[T] extends BaseMapping[T, O] {
		protected[FlatMapping] def componentSelector :S =?> T

		private[FlatMapping] final val extract :root.Extract[T] = MappingExtract(this)(componentSelector)

		@inline private[FlatMapping] final def belongsTo(mapping :FlatMapping[_, _]) =
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

		protected[FlatMapping] def registerSelf() :Unit = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a component of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
		}

	}



	/** Base class for all user defined components of the outer `FlatMapping`. Provides column factory methods
	  * mirroring those from the outer mapping; each new column becomes both a column of `FlatMapping`
	  * and of this component. This mapping cannot have true, independent subcomponents; it can however
	  * use components of the root mapping by registering them in this instance. These components should be created
	  * normally in the most outer scope of the `FlatMapping` component tree, and after or during construction
	  * passed to method [[net.noresttherein.oldsql.schema.bases.FlatMapping.FlatComponent.borrow borrow]]
	  * of this instance. Such semi-components are not exported to the outer mapping and are ignored
	  * by methods creating forms and [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] for write
	  * operations in order to avoid processing them twice. They will be however correctly recognized by
	  * `ComponentValues` during assembly. All buffs on the borrowed components are ignored when they are processed
	  * by this mapping.
	  *
	  * This class should not be used as a base class for column implementations, as such classes
	  * would not be correctly recognized as columns and result in exceptions being thrown when used.
	  * All columns of `FlatMapping` should be created using its factory methods (or factory methods of this class).
	  */
	abstract class FlatComponent[T :TypeTag](selector :S =?> T, newBuffs :Buff[T]*)
	                                        (implicit override val nullValue :NullValue[T] = NullValue.NotNull)
		extends AbstractComponent[T] with StaticMapping[T, O] with EffectivelyEmptyMapping[T, O] with ExportMapping
		   with RelatedMapping[T, O] with LazyMapping[T, O]
	{ self =>
		protected[FlatMapping] override def componentSelector :S =?> T = selector

		registerSelf()

		override val buffs :Seq[Buff[T]] = newBuffs ++: inheritedBuffs
		private[this] var initExtractMap :NaturalMap[Component, Extract] = NaturalMap.empty
		private[this] val lazyExtracts = Lazy(synchronized { val res = initExtractMap; initExtractMap = null; res })

		protected[schema] override val lazyColumns :Lazy[Unique[Column[_]]] =
			Lazy(lazyColumnExtracts.get.collect { case Assoc(col, _) => col }.to(Unique))

		protected[schema] val lazyComponents :Lazy[Unique[Component[_]]] =
			Lazy(lazyExtracts.get.collect { case Assoc(comp, _) => comp }.to(Unique))

		final override def extracts :NaturalMap[Component, Extract] = lazyExtracts.get
		final override def components :Unique[Component[_]] = lazyComponents

		/** Adds the given component of the outer mapping to the waiting list of this mapping's components.
		  * Once any of the column, component or extract collections of this mapping are accessed, they will contain
		  * all registered components (including columns created by this class's factory methods) in the order
		  * in which this method was invoked. All components remain their export versions as in the outer
		  * `FlatMapping`. This method will not modify any properties of the outer mapping:
		  * while the registered component will be present twice in the component tree of the root mapping,
		  * it will feature only once in its column, component and extracts list.
		  * During assembly, retrieving a value for this subcomponent may require a `Map` lookup
		  * in [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]], depending on
		  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] implementation.
		  */
		protected def borrow[X](component :AbstractComponent[X], property :T =?> X) :component.type = {
			val extract = MappingExtract(component)(property)
			synchronized {
				if (initExtractMap == null)
					throw new IllegalStateException(
						s"Cannot include $component as a component of $root.$this: the owning component has already been initialized."
					)
				initExtractMap = initExtractMap.updated[Extract, X](component, extract :Extract[X])
			}
			component
		}

		protected def column[X :ColumnForm](name :String, property :T => X, buffs :Buff[X]*) :Column[X] =
			borrow(new FlatColumn[X](name, extract andThen property, buffs), property)

		protected def column[X](property :T => X, buffs :Buff[X]*)
		                       (implicit form :ColumnForm[X]) :Column[X] =
			column(PropertyPath.nameOf(property), property, buffs :_*)

		protected def optcolumn[X :ColumnForm](name :String, property :T => Option[X], buffs :Buff[X]*) :Column[X] =
			borrow(new FlatColumn[X](name, extract andThen property, buffs), property)


		protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, X, R]
		                             (name :String, property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => ColumnMapping[K, _],
		                              factory :RelatedEntityFactory[K, E, X, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		{
			val selector = Extractor.req(property)
			val cascaded = buffs ++: cascadeBuffs(this.buffs, toString)(selector)
			borrow(
				new FKColumn[M, K, E, X, R, ()](extract andThen selector, name, cascaded)(
					factory, table, key.asInstanceOf[M[()] => ColumnMapping[K, ()]]
				), selector
			)
		}

		protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
		                             (property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, X, R])
		                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val cascaded = buffs ++: cascadeBuffs(this.buffs, toString)(selector)
			borrow(
				new FKComponent[M, C, K, E, X, R, ()](extract andThen selector, rename, cascaded)(
					factory, table, key.asInstanceOf[M[()] => C[()]]
				), selector
			)
		}

		protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
		                       (property :T => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
		                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
				:ForeignKeyMapping[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val cascaded = buffs ++: cascadeBuffs(this.buffs, toString)(selector)
			borrow(
				new InverseFKComponent[M, C, K, E, X, R, ()](extract andThen selector, cascaded)(key, reference)(
					table, fk.asInstanceOf[M[()] => ForeignKeyMapping[MappingAt, C, K, _, ()]]
				), selector
			)
		}
	}



	private trait AbstractColumn[T] extends AbstractComponent[T] with ColumnMapping[T, O] {
		protected[FlatMapping] val index = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a column of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
			columnsBuffer += this
			columnsBuffer.length - 1
		}
	}


	private class FlatColumn[T](override val name :String, selector :S =?> T, override val buffs :Seq[Buff[T]])
	                           (implicit override val form :ColumnForm[T])
		extends AbstractColumn[T] with SimpleColumn[T, O] with StableColumn[T, O]
	{
		protected[FlatMapping] override def componentSelector = selector
	}



	/** An [[net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueProxy opaque]] proxy to component `backer`
	  * of another mapping, serving as a foreign key. This implementation mirrors
	  * [[net.noresttherein.oldsql.schema.support.CoveredMapping CoveredMapping]], but all subcomponents
	  * and subcolumns created as adapters to components and columns of `backer` become
	  * [[net.noresttherein.oldsql.schema.bases.FlatMapping.AbstractComponent components]] and
	  * [[net.noresttherein.oldsql.schema.bases.FlatMapping.AbstractColumn columns]] of the enclosing `FlatMapping`.
	  */
	private class MirrorComponent[M <: RefinedMapping[T, X], T, X]
	                             (override val componentSelector :S =?> T, override val buffs :Seq[Buff[T]] = Nil)
	                             (override val backer :M, rename :String => String = identity[String])
		extends OpaqueProxy[T, O](backer) with AbstractComponent[T]
	{ mirror =>
		protected override def adapt[U](component :backer.Component[U]) :Component[U] = {
			val extract = backer(component)
			val compBuffs = cascadeBuffs(this)(extract)
			val selector = componentSelector andThen extract
			new MirrorComponent[RefinedMapping[U, X], U, X](selector, compBuffs)(component, rename)
		}

		protected override def adapt[U](column :backer.Column[U]) :Column[U] = {
			val columnExtract = backer(column)
			val columnBuffs = cascadeBuffs(this)(columnExtract)
			val selector = componentSelector andThen columnExtract
			column match {
				case simple :SimpleColumn[U @unchecked, X @unchecked] =>
					new FlatColumn[U](rename(column.name), selector, columnBuffs)(simple.form)
				case _ =>
					new OpaqueColumnProxy[U, O](column, rename(column.name), columnBuffs)
						with AbstractColumn[U]
					{
						protected[FlatMapping] override val componentSelector = selector
					}
			}
		}


		def mirror[U](component :backer.Component[U]) :Component[U] = alias(component)
		def mirror[U](column :backer.Column[U]) :Column[U] = alias(column)
	}


	private class FKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                         (override val componentSelector :S =?> R, rename :String => String, buffs :Seq[Buff[R]])
	                         (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X])
		extends ForeignKeyEntityMapping[M, C, K, E, T, R, X, O](rename, buffs, factory)(table, pk)
			with AbstractComponent[R]
	{
		def this(selector :S =?> R, prefix :String, buffs :Seq[Buff[R]])
		        (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X]) =
			this(selector, prefix + _, buffs)(factory, table, pk)

		private[this] val lazyKey = Lazy {
			val extract = Extractor.opt(factory.keyOf)
			val keyBuffs = cascadeBuffs(this)(extract)
			val selector = componentSelector andThen extract
			new MirrorComponent[C[X], K, X](selector, keyBuffs)(target, rename)
		}
		override def key :Component[K] = lazyKey.get

		override def local[U](subKey :RefinedMapping[U, X]) :Component[U] = lazyKey.get.mirror(subKey)
		override def local[U](subKey :ColumnMapping[U, X]) :Column[U] = lazyKey.get.mirror(subKey)

		root.synchronized { //this component will be created lazily; it must be before root lists are accessed
			registerSelf()
			lateComponents += (() => lazyKey.get)
		}
	}

	private class FKColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X]
	                      (override val componentSelector :S =?> R, name :String, buffs :Seq[Buff[R]])
	                      (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => ColumnMapping[K, X])
		extends ForeignKeyEntityColumnMapping[M, K, E, T, R, X, O](name, buffs, factory)(table, pk)
			with AbstractColumn[R]
	{
		private[this] val lazyKey = Lazy {
			val extract = Extractor.opt(factory.keyOf)
			val keyBuffs = cascadeBuffs(this)(extract)
			val selector = componentSelector andThen extract
			if (target.isInstanceOf[SimpleColumn[_, _]])
				new FlatColumn[K](name, selector, keyBuffs)(target.form)
			else
				new OpaqueColumnProxy[K, O](target, name, keyBuffs) with AbstractColumn[K] {
					override val componentSelector = selector
				}
		}
		override def key :Column[K] = lazyKey

		root synchronized {
			lateComponents += (() => lazyKey.get)
		}
	}


	private class InverseFKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                                (override val componentSelector :S =?> R, buffs :Seq[Buff[R]])
	                                (key :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                                (table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X])
		extends InverseForeignKeyMapping[M, C, K, E, T, R, X, O](key, factory, buffs)(table, fk)
		   with AbstractComponent[R] with EffectivelyEmptyMapping[R, O] //with ExportMapping
	{
		key match {
			case comp :FlatMapping[_, _]#AbstractComponent[_] if comp belongsTo root =>
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $key given as the local referenced key for a foreign key inverse is not a component of $root."
				)
		}
		registerSelf()
	}



	override def apply[T](component :Component[T]) :Extract[T] = component match {
		case comp :FlatMapping[_, _]#FlatComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[Extract[T]]
		case _ =>
			throw new IllegalArgumentException(s"$component is not a component of $this.")
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
		case comp :FlatMapping[_, _]#FlatComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[ColumnExtract[T]]
		case _ =>
			throw new IllegalArgumentException(s"$column is not a column of $this.")
	}

	private[this] var lateComponents :ListBuffer[() => AbstractComponent[_]] = ListBuffer.empty
	private[this] var componentsBuffer :ListBuffer[AbstractComponent[_]] = ListBuffer.empty
	private[this] var columnsBuffer :ListBuffer[AbstractColumn[_]] = ListBuffer.empty

	private val lazyComponents = Lazy(synchronized {
		while (lateComponents.nonEmpty) {
			val late = lateComponents
			lateComponents = ListBuffer.empty
			late.foreach { f => f() }
		}
		lateComponents = null
		val comps = Unique.from(componentsBuffer)
		componentsBuffer = null
		comps
	})

	protected[schema] override val lazyColumns :Lazy[Unique[Column[_]]] = Lazy(synchronized {
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
			case col :FlatMapping[_, _]#AbstractColumn[_] =>
				values(col.index) = result; this
			case _ =>
				componentValues = componentValues.updated(component, result)
				this
		}

		override def result() =
			if (componentValues.isEmpty)
				ColumnValues(ArraySeq.unsafeWrapArray(values)) {
					case column :FlatMapping[_, _]#AbstractColumn[_] => column.index
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
			case col :FlatMapping[_, _]#AbstractColumn[_] if col belongsTo root => col
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
				case col :FlatMapping[_, _]#AbstractColumn[_] => col.index
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
				s"$comp is not a column of $this. FlatMapping.selectForm accepts only columns."
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
		new FlatColumn[T](name, property, buffs)

	protected def column[T](property :S => T, buffs :Buff[T]*)
	                       (implicit form :ColumnForm[T], tag :TypeTag[S]) :Column[T] =
		column(PropertyPath.nameOf(property), property, buffs :_*)

	protected def optcolumn[T :ColumnForm](name :String, property :S => Option[T], buffs :Buff[T]*) :Column[T] =
		new FlatColumn[T](name, property, buffs)



	protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                             (name :String, property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => ColumnMapping[K, _],
	                              factory :RelatedEntityFactory[K, E, T, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		new FKColumn[M, K, E, T, R, ()](property, name, buffs ++: conveyBuffs(property, name))(
			factory, table, key.asInstanceOf[M[()] => ColumnMapping[K, ()]]
		)

	protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                             (property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, T, R])
	                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		new FKComponent[M, C, K, E, T, R, ()](property, rename, buffs ++: conveyBuffs(property, s"FK($table)"))(
			factory, table, key.asInstanceOf[M[()] => C[()]]
		)

	protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                       (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:ForeignKeyMapping[M, C, K, R, O] =
		new InverseFKComponent[M, C, K, E, T, R, ()](property, buffs ++: conveyBuffs(property, key.toString))(
			key, reference)(
			table, fk.asInstanceOf[M[()] => ForeignKeyMapping[MappingAt, C, K, _, ()]]
		)

}

