package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.InverseIndexSeq
import net.noresttherein.oldsql.collection.InverseIndexSeq.implicitIndexing
import net.noresttherein.oldsql.morsels.PropertyPath
import net.noresttherein.oldsql.schema.Buff.{AutoGen, AutoInsert, AutoUpdate, NoInsert, NoQuery, NoSelect, NoUpdate, Unmapped}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingReadForm, MappingWriteForm, Selector}
import net.noresttherein.oldsql.schema.support.ComponentProxy.{EagerDeepProxy, ShallowProxy}
import net.noresttherein.oldsql.slang._

import scala.collection.{mutable, AbstractSeq, IndexedSeqOptimized}
import scala.collection.mutable.{Builder, ListBuffer}
import scala.reflect.runtime.universe.TypeTag






/** Convenience base trait for mappings which are defined statically, rather than dynamically (i.e the mapped type and
  * schema is mostly known). This includes table mappings and other instances where columns are known statically and
  * should be created manually, rather than by some generic code. While it exposes mutator methods to facilitate
  * creation and declaration of components, it is expected that they'll be used solely in the constructors of derived
  * classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called
  * previously, an exception will be thrown.
  *
  * It exists exclusively as a way to facilitate creating schema descriptions and it's interface is directed
  * towards implementing classes; no other place in the system handles it in any special way, different from other
  * implementations of `Mapping[_]`.
  *
  * @tparam S value type of the mapped entity
  */
trait RowSchema[S] extends Mapping[S] { composite =>

	/** Base trait for all components of this mapping. Creating an instance automatically lists it within owning mapping
	  * components as well any contained subcomponents and columns within parent mapping appropriate lists.
	  * This registering is delayed, as in most cases properties listing its subcomponents will not be initialized before 
	  * its constructor is over, so instead it adds itself only to a 'waiting list', which is processed and its contents
	  * appropriately registered within the parent mapping once either this instance's `include()` method is called, parent
	  * mapping explicitly requests initializing all waiting lists via its 'initialize()' method, or any of the parent
	  * mapping component accessor methods are called. If for any reason this component is not fully initialized before
	  * one of that happens, and exception will be thrown during its initialization. For this reasons, all components
	  * should be created eagerly whenever possible - be very cautious with declaring members of this type as lazy val
	  * or object. As a consequence of the above, simple creation of this instance registers it permanently with
	  * the parent mapping - no other call is needed.
	  * @tparam T value type of this component
	  */
	trait ComponentMapping[T]  extends SubMapping[Owner, T] with Selector[composite.type, Owner, S, T] { self =>

		def opt(implicit values :composite.Values) :Option[T] =
			values.get(this)

		private[RowSchema] def belongsTo(mapping :RowSchema[_]) :Boolean = mapping eq composite

		private[this] var initialized = false

		/** Manually trigger initialization of this instance within the enclosing mapping. */
		private[RowSchema] final def include() :this.type = synchronized {
			if (!initialized) {
				init()
				initialized = true
			}
			this
		}

		/** Register itself and all its subcomponents within the parent mapping. This method will be called only once. */
		protected[RowSchema] def init() :Unit = composite.synchronized {
			if (!isSymLink(this)) {
				initComponents += this
				initSubcomponents += this
				subcomponents foreach { c => liftSubcomponent(c) }
			}
		}

		protected[RowSchema] def liftSubcomponent[U](subcomponent :Component[U]) :Unit =
			if (!isSymLink(subcomponent)) composite.synchronized {
				if (!initSelectors.contains(subcomponent)) {
					val selector = apply(subcomponent)
					val picksub = (s :S) => pick(s).flatMap(selector.pick)
					val surepicksub = surepick.flatMap(st => selector.surepick.map(tu => st andThen tu))

					if (columnPrefix == null)
						throw new IllegalStateException(s"$this.columnPrefix is null: overrides with a val must happen before any component declarations!")
					if (buffs == null)
						throw new IllegalStateException(s"$this.buffs is null: overrides with a val must happen before any component declarations!")

					val subbuffs = selector.surepick.mapOrElse(pick => buffs.map(_.map(pick)), buffs.map(
						buff => buff.map((t: T) => selector.pick(t) getOrElse {
							throw new IllegalArgumentException(s"Can't apply buff $buff for subcomponent $subcomponent of $composite/$self: no value for $t.")
						})
					)) ++: subcomponent.buffs

					val lifted = subcomponent match {
						case column :ColumnMapping[Owner, U] =>
							composite.include(initPreceding(new ColumnComponent[U](
								picksub, surepicksub, columnPrefix + column.name, subbuffs
							)(column.form)))
						case _ =>
							new LiftedComponent[U](subcomponent, picksub, surepicksub, columnPrefix, subbuffs)
					}
					initSelectors = initSelectors.updated(subcomponent, lifted)
				}
			}



		val lifted :ComponentMapping[T] = this

		/** Buffs 'inherited' from the enclosing mapping. It uses the value selector `S=>T` to map all buffs applied
		  * to the enclosing mapping to adapt them to this value type. This means that if there are any
		  * [[net.noresttherein.oldsql.schema.Buff.ValuedBuff ValuedBuff]]s among the inherited buffs, this component
		  * must have a value for the value associated with that buff or an exception will be thrown - either
		  * when accessing/initializing the buff list or when the value for the buff is actually needed.
		  */
		protected final def inheritedBuffs :Seq[Buff[T]] = transitBuffs(pick, toString)

		/** The column prefix prepended to all columns at the behest of the enclosing `RowSchema` or an empty string. */
		protected final def inheritedPrefix :String = testedPrefix

		/** The column prefix prepended to all columns by this component ''and'' the enclosing `RowSchema`.
		  * Defaults to `inheritedPrefix`. */
		protected def columnPrefix :String = inheritedPrefix



		delayInit(this)

	}



	/** A proxy class for non-direct subcomponents of the enclosing `RowSchema` which incorporates any column prefix
	  * and buffs defined in it and any other enclosing components into the adapted component.
	  * It serves at the same time as its own `Selector` and the lifted 'effective' version of the component.
	  */
	private class LiftedComponent[T](original :Component[T], val pick :S=>Option[T], val surepick :Option[S=>T],
	                                 override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends ShallowProxy[Owner, T] with ComponentMapping[T]
	{
		override protected val adaptee :Component[T] = original

		protected[RowSchema] override def init() :Unit = composite.synchronized {
			if (!isSymLink(this)) {
				initSubcomponents += this
				subcomponents foreach { c => liftSubcomponent(c) }
			}
		}

		override def toString :String = "^" + adaptee + "^"
	}



	/** Base trait for components which have a value for all instances of `T`. */
	trait MandatoryComponent[T] extends ComponentMapping[T] {
		final override def pick :S => Some[T] = (e :S) => Some(extractor(e))
		final override def surepick = Some(extractor)
		protected def extractor :S=>T

		override def buffs :Seq[Buff[T]] = inheritedBuffs
	}

	/** Base trait for components which might not have a value for all instances of `T` (such as properties declared
	  * by some subclass of `T`. This is independent from the
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect ]]
	  */
	trait OptionalComponent[T] extends ComponentMapping[T] {
		final override def surepick :None.type = None
		def pick :S => Option[T]

		override def buffs :Seq[Buff[T]] = inheritedBuffs
	}

	/** Convenience base component class which initializes accessor fields using the constructor argument getter. */
	abstract class BaseComponent[T](value :S=>T) extends ComponentMapping[T] {
		override val surepick = Some(value)
		override val pick :S => Some[T] = (e :S) => Some(value(e))
		override val buffs :Seq[Buff[T]] = inheritedBuffs
	}

	/** Convenience base class for creating components of this instance which themselves contain statically enumerated
	  * subcomponents.
	  * @param value returns value of this component on a given parent entity
	  * @param prefix prefix which will be added to all columns, direct and transitive within this instance.
	  *               All columns accessed through this instance methods will be affected, not only when viewed
	  *               from parent mappings. A column which is defined within a subcomponent of this component
	  *               will not show the prefix however when viewed in the scope of that component - only
	  *               the proxy seen as `this.Column[_]` will.
	  * @tparam T value type of this component
	  */
	abstract class ComponentSchema[T](value :S => T, prefix :String="")
		extends BaseComponent[T](value) with RowSchema[T]
	{
		override final val columnPrefix = composite.testedPrefix + prefix
	}


	abstract class BaseOptionalComponent[T](val pick :S => Option[T])
		extends OptionalComponent[T]
	{
		override val buffs :Seq[Buff[T]] = inheritedBuffs
	}

	abstract class OptionalComponentSchema[T](value :S => Option[T], prefix :String = "")
		extends BaseOptionalComponent[T](value) with RowSchema[T]
	{
		override final val columnPrefix = composite.testedPrefix + prefix
	}
/*
	class SymLinkComponent[C<:Mapping[T], T](target :TypedComponentPath[this.type, C, T]) extends BaseSymLink[this.type, C, T](target) with Component[T] {
		override protected[RowSchema] val pick: (S) => Option[T] = target.pick
		override protected[RowSchema] val surepick: Option[(S) => T] = target.surepick
		override private[RowSchema] def init() = Path(comp => target.lift(comp.adaptedComponent))
	}
*/



/*
	class EmbeddedComponent[T, C<:Mapping[T]](value :S=>T, mapping :C) extends MappingImpostor[T, C] with Component[T] {
		override protected[RowSchema] val pick = (e: S) => Some(value(e))
		override protected[RowSchema] val surepick = Some(value)
		override val adaptee: C = mapping
		override def toString = s"@$adaptee"
	}
*/






	/** Base trait for column components declared directly under this mapping. In general, there should be little need
	  * to descend your classes directly from it, as column functionality is limited and constructor methods provided
	  * within `RowSchema` should be sufficient. As with the base `ComponentMapping` trait, simple creation of
	  * this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options (buffs).
	  * @tparam T value type of this column.
	  */
//	trait Column[T] extends ColumnMapping[Owner, T] with ComponentMapping[T]



	/** A column for which a value exists in all instances of `S`. It doesn't mean that it is mandatory in any particular
	  * sql statement. It automatically inherits the enclosing mapping's `columnPrefix` and buffs.
	  */
	private class MandatoryColumn[T :SQLForm](value :S=>T, name :String, opts :Seq[Buff[T]])
		extends StandardColumn[Owner, T](testedPrefix + name, transitBuffs(s => Some(value(s)), testedPrefix + name) ++: opts)
		   with ComponentMapping[T]
	{
		override val surepick = Some(value)
		override val pick = (e :S) => Some(value(e))
	}


	/** A lower-level column component which accepts values for all abstract fields and initializes them verbatim.
	  * This column will ''not'' automatically inherit the enclosing mapping's `columnPrefix` or buffs
	  */
	private class ColumnComponent[T](val pick :S=>Option[T], val surepick :Option[S=>T], name :String, buffs :Seq[Buff[T]])
	                                (implicit sqlForm :SQLForm[T])
		extends StandardColumn[Owner, T](name, buffs) with ComponentMapping[T]
	{
		def this(value :S=>T, name :String, buffs :Seq[Buff[T]])(implicit form :SQLForm[T]) =
			this((e :S) => Some(value(e)), Some(value), name, buffs)
	}





	/** A builder adapting a given column template to a column of this `RowSchema`. */
	class ColumnCopist[T](template :ColumnMapping[_, T], name :Option[String], buffs :Seq[Buff[T]]) {
		def this(source :ColumnMapping[_, T]) = this(source, None, source.buffs)

		template match {
			case self :RowSchema[_]#ComponentMapping[_] if self.belongsTo(composite) =>
				if (buffs == null)
					throw new IllegalStateException(s"$this.buffs is null: overrides must be defined before any components.")
				if (testedPrefix.length > 0 || buffs.nonEmpty )
					throw new IllegalArgumentException(
						s"Can't use column $template of the same mapping $composite as the template for a new column."

					)
			case _ =>
		}

		def apply(pick :S=>T) :Column[T] =
			initPreceding(new ColumnComponent[T](pick, testedPrefix + (name getOrElse template.name), buffs)(template.form))

		def prefixed(prefix :Option[String]) :ColumnCopist[T] =
			new ColumnCopist(template, prefix.mapOrElse(p => Some(p + (name getOrElse template.name)), name), buffs)

		def prefixed(prefix :String) :ColumnCopist[T] = prefixed(Some(prefix))

		def named(name :String) :ColumnCopist[T] =
			new ColumnCopist(template, Some(name), buffs)

		def set(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs)

		def add(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs ++: this.buffs)

		def remove(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, this.buffs.filterNot(buffs.contains) )
	}





	protected def column[T](col :ColumnMapping[_, T]) :ColumnCopist[T] =
		new ColumnCopist(col)

	protected def column[T](col :ColumnMapping[_, T], name :String) :ColumnCopist[T] =
		new ColumnCopist[T](col, Some(name), col.buffs)

	protected def column[T](col :ColumnMapping[_, T], buffs :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, None, buffs)

	protected def column[T](col :ColumnMapping[_, T], name :String, buffs :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, Some(name), buffs)




	protected def column[T](name :String, pick :S=>T, buffs :Buff[T]*)
	                       (implicit form :SQLForm[T]) :Column[T] =
		initPreceding(new MandatoryColumn[T](pick, name, buffs))

	protected def column[T](pick :S=>T, buffs :Buff[T]*)(implicit form :SQLForm[T], subject :TypeTag[S]) :Column[T] =
		column[T](PropertyPath.nameOf(pick), pick, buffs:_*)



	protected def optcolumn[T](name :String, pick :S=>Option[T], buffs :Buff[T]*)
	                          (implicit form :SQLForm[T]) :Column[T] =
		initPreceding(new ColumnComponent(pick, None, testedPrefix + name, transitBuffs(pick) ++: buffs))

	protected def optcolumn[T](pick :S=>Option[T], buffs :Buff[T]*)(implicit form :SQLForm[T], subject :TypeTag[S]) :Column[T] =
		optcolumn[T](PropertyPath.nameOf(pick), pick, buffs: _*)



	protected def unmapped[T :SQLForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new ColumnComponent[T](_ => None, None, name, Unmapped +: buffs))



	protected def autoins[T](name :String, pick :S=>Option[T], options :Buff[T]*)
	                        (implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](name, pick(_:S).get, AutoInsert +: options:_*)

	protected def autoins[T](pick :S=>Option[T], options :Buff[T]*)
	                        (implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](PropertyPath.nameOf(pick), pick(_:S).get, AutoInsert +: options :_*)


//	protected def foreignKey[T :SQLForm]()
/*
	def symLink[T](component :Component[T]) :SymLinkComponent[component.type, T] =
		symLink(component.path)

	def symLink[M<:Mapping[T], T](path :TypedComponentPath[this.type, M, T]) :SymLinkComponent[M, T] =
		new SymLinkComponent[M, T](path)




	protected def embed[T](value :S=>T, mapping :Mapping[T]) :EmbeddedComponent[T, mapping.type] =
	//		initialize(new EmbeddedComponent[T, mapping.type](value, mapping))
		new EmbeddedComponent[T, mapping.type](value, mapping)
*/



	override def assemble(values: Values): Option[S] =
		try {
			isDefined(values) ifTrue construct(values)
		} catch {
			case _ :NoSuchElementException => None
		}

	protected def construct(implicit res :Values) :S

	protected def isDefined(values :Values) :Boolean = true

	implicit def valueOf[T](component :ComponentMapping[T])(implicit values :Values) :T = values(component)
	implicit def valueOf[T](component :Component[T])(implicit values :Values) :T = values(apply(component))





	override def apply[T](component :Component[T]) :Selector[this.type, Owner, S, T] = component match {
		case lifted :RowSchema[_]#ComponentMapping[T] if lifted belongsTo this =>
			lifted.asInstanceOf[ComponentMapping[T]]
		case _  =>
			if (fastSelectors == null) {
				val initialized = selectors
				if (initialized != null)
					fastSelectors = initialized
				else {
					initialize()
					fastSelectors = selectors
				}
			}
			fastSelectors.getOrElse(component, throw new NoSuchElementException(
				s"Component $component is not a part of mapping $this."
			)).asInstanceOf[ComponentMapping[T]]
	}

	private def update(source :AnyComponent, lifted :ComponentMapping[_]) :Unit = synchronized {
		if (isInitialized)
			throw new IllegalStateException(
				s"Cannot lift component $source of $this to $lifted: components have already been exported. Create components eagerly!"
			)
		initSelectors = initSelectors.updated(source, lifted)
	}

	private[this] var initSelectors = Map[AnyComponent, ComponentMapping[_]]()
	@volatile private[this] var selectors :Map[AnyComponent, ComponentMapping[_]] = _
	private[this] var fastSelectors :Map[AnyComponent, ComponentMapping[_]] = _



	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override. */
	protected def columnPrefix :String = ""

	private final def testedPrefix :String = columnPrefix match {
		case null => throw new IllegalStateException(
			s"$this.columnPrefix is null: override with a val must happen before any component declarations."
		)
		case prefix => prefix
	}



	private def safeTransitBuffs[T](pick :S => T) :Seq[Buff[T]] =
		if (buffs == null)
			throw new IllegalStateException(s"$this.buffs is null: overrides must happen before any component declarations.")
		else
			buffs.map(_.map(pick))

	private def transitBuffs[T](pick :S => Option[T], component :String = "") :Seq[Buff[T]] =
		if (buffs == null)
			throw new IllegalStateException(s"$this.buffs is null: overrides must happen before any component declarations.")
		else
			buffs.map(buff => buff.map{ subject => pick(subject) getOrElse {
				throw new IllegalArgumentException(
					s"Can't apply buff $buff of $this to subcomponent '$component': selector function returned no value for $subject."
				)
			}})






	private def include[T](column :ComponentMapping[T]) :ComponentMapping[T] = synchronized {
		if (isInitialized)
			throw new IllegalStateException(
				s"Cannot initialize column $column of $this: components have already been exported. Create components eagerly!"
			)
		if (column.columns.size != 1 || column.sqlName.isEmpty)
			throw new IllegalStateException(
				s"Attempted to include $column as a column of $this: included component's columns: ${column.columns}; sqlName: ${column.sqlName}"
			)
		if (!isSymLink(column)) {
			initColumns += column
			if (NoSelect.disabled(column)) initSelectable += column
			if (NoQuery.disabled(column)) initQueryable += column
			if (NoUpdate.disabled(column)) initUpdatable += column
			if (NoInsert.disabled(column)) initInsertable += column
			if (AutoInsert.enabled(column)) initAutoInsert += column
			if (AutoUpdate.enabled(column)) initAutoUpdate += column
		}
		column
	}



	private def delayInit(c :ComponentMapping[_]) :Unit = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Cannot initialize mapping component $c on $this: components have already been exported.")
		else
			delayedInits += c
	}

	protected final def initPreceding() :Unit = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	@inline protected final def initPreceding[T](value :T) :T = { initPreceding(); value }

	/** Keeps components which have been created, but not initialized (their columns and components have not been
	  * enlisted with this instance). Modified only under synchronization lock of this instance, its contents
	  * are flushed either when initialization occurs (a call to `initialize()`, either explicit or due to accessing
	  * a column or component list of this instance), or when a new component (typically a column) is created
	  * and needs to be initialized, in order to preserve relative order of declarations in the column/component
	  * lists.
	  */
	private[this] val delayedInits = new ListBuffer[ComponentMapping[_]]



	protected final def isInitialized :Boolean = initComponents.isInitialized

	protected final def initialize() :Unit =
		if (!isInitialized) synchronized {
			if (!isInitialized) {
				delayedInits.foreach(_.include())
				delayedInits.clear()

				initComponents.initialize()
				initColumns.initialize()
				initSelectable.initialize()
				initQueryable.initialize()
				initUpdatable.initialize()
				initInsertable.initialize()
				initAutoInsert.initialize()
				initAutoUpdate.initialize()
//				for (c <- components; if !isSymLink(c); sub <- c.subcomponents; if !isSymLink(sub))
//					initSubcomponents += sub
				initSubcomponents.initialize()

				fastSelectors = initSelectors
				selectors = fastSelectors
				initSelectors = null
			}
		}



	/** A sequence which works as a mutable buffer during initialization, but once it is completed requires
	  * no synchronization. All appends must be synchronized with the enclosing class's lock, immediately
	  * followed by a test of `isInitialized`, signifying that no further modification can be done
	  * (will most likely throw a `NullPointerException`). The fields of this type are initialized
	  * by a call to `initialize()`, which must likewise happen inside a lock and with a guard, as initialization
	  * is not idempotent. The initialization copies the components from a mutable list to a `@volatile`,
	  * immutable one which is used by all collection methods. To eliminate the penalty of a `@volatile` field,
	  * there is another, non-volatile `var` initialized with the same value and which is always checked first.
	  * Only if the field is `null`, the value of the volatile field is copied and, in case that one is `null`, too,
	  * the outer `initialize()` method is called. This works because once the volatile field is initialized,
	  * it never changes value (and its value is immutable), hence reassigns to the cacheable `var` do not result
	  * in any change of state.
	  */
	private[this] class LateInitComponents(private[this] var uninitialized :Builder[ComponentMapping[_], List[ComponentMapping[_]]])
		extends AbstractSeq[ComponentMapping[_]] with InverseIndexSeq[ComponentMapping[_]]
	{
		def this() = this(List.newBuilder)

		@volatile
		private[this] var initialized :InverseIndexSeq[ComponentMapping[_]] = _
		private[this] var cached :InverseIndexSeq[ComponentMapping[_]] = _

		def +=(comp :ComponentMapping[_]) :Unit =
			if (isInitialized)
				throw new IllegalStateException(
					s"Can't enlist component $comp for $composite: the list of components has been already exported."
				)
			else
				uninitialized += comp

		def isInitialized :Boolean = initialized != null

		def initialize() :Unit = {
			cached = uninitialized.result.indexed
			initialized = cached
			uninitialized = null
		}

		private def items :InverseIndexSeq[ComponentMapping[_]] =
			if (cached != null)
				cached
			else if (initialized != null) {
				cached = initialized
				cached
			} else {
				composite.initialize()
				uninitialized = null
				cached = initialized
				cached
			}

		override def length :Int = items.length

		override def apply(idx :Int) :ComponentMapping[_] = items(idx)

		override def foreach[U](f :ComponentMapping[_] => U) :Unit = items.foreach(f)

		override def iterator :Iterator[ComponentMapping[_]] = items.iterator

		override def indexOf[B >: ComponentMapping[_]](elem :B) :Int = items.indexOf(elem)

		override def lastIndexOf[B >: ComponentMapping[_]](elem :B) :Int = items.lastIndexOf(elem)

	}



	private[this] final val initComponents = new LateInitComponents
	private[this] final val initColumns = new LateInitComponents
	private[this] final val initSelectable = new LateInitComponents
	private[this] final val initQueryable = new LateInitComponents
	private[this] final val initUpdatable = new LateInitComponents
	private[this] final val initAutoUpdate = new LateInitComponents
	private[this] final val initInsertable = new LateInitComponents
	private[this] final val initAutoInsert = new LateInitComponents
	private[this] final val initSubcomponents = new LateInitComponents

	final override def components :Seq[Component[_]] = initComponents
	final override def columns :Seq[Component[_]] = initColumns
	final override def selectable :Seq[Component[_]] = initSelectable
	final override def queryable :Seq[Component[_]] = initQueryable
	final override def updatable :Seq[Component[_]] = initUpdatable
	final override def autoUpdated :Seq[Component[_]] = initAutoUpdate
	final override def insertable :Seq[Component[_]] = initInsertable
	final override def autoInserted :Seq[Component[_]] = initAutoInsert
	final override def subcomponents :Seq[Component[_]] = initSubcomponents

	final override val selectForm = MappingReadForm.defaultSelect(this)
	final override val queryForm = MappingWriteForm.defaultQuery(this)
	final override val insertForm = MappingWriteForm.defaultInsert(this)
	final override val updateForm = MappingWriteForm.defaultUpdate(this)


}






trait RowSubSchema[O <: AnyMapping, S] extends RowSchema[S] with SubMapping[O, S]






object RowSchema {

}

