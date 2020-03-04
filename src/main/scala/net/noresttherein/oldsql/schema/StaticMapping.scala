package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.InverseIndexSeq
import net.noresttherein.oldsql.collection.InverseIndexSeq.implicitIndexing
import net.noresttherein.oldsql.morsels.PropertyPath
import net.noresttherein.oldsql.schema.ColumnMapping.BaseColumn
import net.noresttherein.oldsql.schema.ComponentPath.{SelfPath, TypedComponentPath}
import net.noresttherein.oldsql.schema.Mapping.{Buff, MappingReadForm, MappingWriteForm}
import net.noresttherein.oldsql.schema.Mapping.Buff.{AutoGen, NoInsert, NoQuery, NoSelect, NoUpdate}
import net.noresttherein.oldsql.slang._

import scala.collection.{mutable, AbstractSeq, IndexedSeqOptimized}
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag






/** Convenience base trait for mappings which are defined statically, rather than dynamically (i.e the mapped type and
  * schema is mostly known). This includes table mappings and other instances, where columns are known statically and
  * should be created manually, rather than by some generic code. While it exposes mutator methods to facilitate
  * creation and declaration of components, it is expected that they'll be used solely in the constructors of derived
  * classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called
  * previously, an exception will be thrown.
  *
  * It exists exclusively as a way to facilitate creating elegant schema descriptions and it's interface is directed
  * towards implementing classes; no other place in the system handles it in any special way, different from other
  * implementations of `Mapping[_]`.
  *
  * @tparam S value type of the mapped entity
  */
trait StaticMapping[S] extends Mapping[S] { composite =>

	/** Base trait for all components of this mapping. Creating an instance automatically lists it within owning mapping
	  * components as well any contained subcomponents and columns within parent mapping appropriate lists.
	  * This registering is delayed, as in most cases properties listing its subcomponents will not be initialized before 
	  * its constructor is over, so instead it adds itself only to a 'waiting list', which is processed and its contents
	  * appropriately registered within the parent mapping once either this instance `include()` method is called, parent
	  * mapping explicitly requests initializing all waiting lists via one of its 'init' methods, or any of the parent
	  * mapping component accessor methods are called. If for any reason this component is not fully initialized before
	  * one of that happens, and exception will be thrown during its initialization. For this reasons, all components
	  * should be created eagerly whenever possible - be very cautious with declaring members of this type as lazy val
	  * or object. As a consequence of the above, simple creation of this instance registers it permanently with
	  * the parent mapping - no other call is needed.
	  * @tparam T value type of this component
	  */
	trait Component[T]  extends SubMapping[T, Owner] { self =>

		def opt(implicit values :composite.Values) :Option[T] =
			values.get(this)

		private[StaticMapping] def belongsTo(mapping :StaticMapping[_]) :Boolean = mapping eq composite

		/** A path from parent mapping to this component, serving at the same time as a trigger for single-time
		  * initialization. As this value is (and should) be lazy, when it's requested for the first time, it will call
		  * `init()` method. For this reason it should be called only after full creation of this instance (with any
		  * implemented subclasses and filling all its components).
		  */
		private[StaticMapping] final lazy val path = init()

		/** Manually trigger initialization of this instance within the parent mapping. */
		protected[StaticMapping] final def include() :this.type = { path; this }

		/** Register itself and all its subcomponents within the parent mapping. This method will be called only once. */
		private[StaticMapping] def init() : TypedComponentPath[composite.type, this.type, T] = composite.synchronized {
			initComponents += this
			Path(???)
		}


		private[StaticMapping] def Path(lift :Component[_]=>Option[composite.Component[_]]) :TypedComponentPath[composite.type, self.type, T] = ???
//			DirectComponent(composite :composite.type)(self :self.type)(
//				surepick.map(ValueMorphism.homomorphism(_)) getOrElse ValueMorphism(pick), ComponentMorphism[Component, composite.Component](lift))



		/** Select the value of this component from the parent entity. This value will be used to pass to its mapping methods on deassembling to column values. */
		protected[StaticMapping] def pick :S=>Option[T]
		protected[StaticMapping] def surepick :Option[S=>T]



		delayInit(this)

	}



	object Component {

		implicit def valueOf[T](component :composite.Component[T])(implicit values :Values) :T =
			values(component)

	}

	trait MandatoryComponent[T] extends Component[T] {
		final override protected[StaticMapping] def pick :S => Some[T] = (e :S) => Some(selector(e))
		final override protected[StaticMapping] def surepick = Some(selector)

		protected def selector :S=>T
	}

	trait OptionalComponent[T] extends Component[T] {
		final override protected[StaticMapping] def surepick :None.type = None
		def pick :S => Option[T]
	}


	/** Convenience base component class which initializes accessor fields based on the constructor argument getter. */
	abstract class BaseComponent[T](value :S=>T) extends Component[T] {
		override protected[StaticMapping] val surepick = Some(value)
		override protected[StaticMapping] val pick :S => Some[T] = (e :S) => Some(value(e))

	}

	/** Convenience base class for creating components of this instance which themselves contain statically enumerated
	  * subcomponents.
	  * @param value returns value of this component on a given parent entity
	  * @param columnPrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                     All columns accessed through this instance methods will be affected, not only when viewed
	  *                     from parent mappings. A column which is defined within a subcomponent of this component
	  *                     will not show the prefix however when viewed in the scope of that component - only
	  *                     the proxy seen as `this.Column[_]` will.
	  * @tparam T value type of this component
	  */
	abstract class StaticComponent[T](value :S=>T, override final val columnPrefix :String="") extends BaseComponent[T](value) with StaticMapping[T] {
		override def toString :String = sqlName orElse (columnPrefix.length > 0).ifTrue(columnPrefix) getOrElse super.toString
	}

	//	trait VirtualComponent[T, S] extends Component[T] with MappingSubstitute[T, S, Component[S]] {
	//		val target :composite.Component[S]
	//
	//
	//	}


/*
	class SymLinkComponent[C<:Mapping[T], T](target :TypedComponentPath[this.type, C, T]) extends BaseSymLink[this.type, C, T](target) with Component[T] {
		override protected[StaticMapping] val pick: (S) => Option[T] = target.pick
		override protected[StaticMapping] val surepick: Option[(S) => T] = target.surepick
		override private[StaticMapping] def init() = Path(comp => target.lift(comp.adaptedComponent))
	}
*/



/*
	class EmbeddedComponent[T, C<:Mapping[T]](value :S=>T, mapping :C) extends MappingImpostor[T, C] with Component[T] {
		override protected[StaticMapping] val pick = (e: S) => Some(value(e))
		override protected[StaticMapping] val surepick = Some(value)
		override val adaptee: C = mapping
		override def toString = s"@$adaptee"
	}
*/





	/** Base trait for all column components of this mapping. In general, there should be little need to descend your classes directly from it,
	  * as column functionality is limited and constructor methods provided within StaticMapping should be sufficient. As with the base Component
	  * trait, simple creation of this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options. This trait encompasses both direct and indirect (proxies for instances declared within subcomponents of the parent mapping,
	  * rather than at the top level) and doesn't register itself under direct compoments list, unlike the base Component[_] trait! For direct instances,
	  * use DirectColumn.
	  *
	  * @tparam T value type of this component
	  */
	trait Column[T] extends ColumnMapping[T, Owner] with Component[T] {
		override private[StaticMapping] def init() =  {
			composite.include(this)
			Path(_ => Some(this))
		}
	}



	object Column {
		implicit def valueOf[T](column :composite.Column[T])(implicit values :Values) :T =
			values(column)
	}


	trait DirectColumn[T] extends Column[T] {
		override private[StaticMapping] def init() = composite.synchronized {
			val path = super.init()
			initComponents += this
			path
		}
	}



	private class ColumnComponent[T :SQLForm](value :S=>T, name :String, options :Seq[Buff[T]])
		extends BaseColumn[T, Owner](columnPrefix + name, options) with DirectColumn[T]
	{
		val surepick = Some(value)
		val pick = (e :S) => Some(value(e))
	}


	private class ColumnCopy[T](val pick :S=>Option[T], val surepick :Option[S=>T], name :String, options :Seq[Buff[T]])
	                           (implicit sqlForm :SQLForm[T])
		extends BaseColumn[T, Owner](columnPrefix + name, options) with DirectColumn[T]
	{
		def this(column :ColumnMapping[T, _], value :S=>T, name :Option[String], options :Seq[Buff[T]]) =
			this((e:S) => Some(value(e)), Some(value), name getOrElse column.name, options)(column.form)
	}




	implicit def valueOf[T](component :Component[T])(implicit values :Values) :T = values(component)


/*
	@inline
	final def apply[C<:AnyComponent](component :this.type=>C) :ComponentPath[this.type, C] =
		SelfPath(this :this.type) \ component




	override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] =
		symLinkTarget(component) match {
			case Some(target) => SymLinkMapping.path[this.type, component.type, T](target, component.path.morphism)
			case _ => component.path
		}
*/




	override def assemble(values: Values): Option[S] =
		try {
			isDefined(values) ifTrue construct(values)
		} catch {
			case _ :NoSuchElementException => None
		}

	protected def construct(implicit res :Values) :S


	protected def isDefined(values :Values) :Boolean = true




	private def delayInit(c :Component[_]) :Unit = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Cannot initialize mapping component $c on $this: components have already been exported.")
		else
			delayedInits += c
	}

	private def include[T](column :Component[T]) :Component[T] = synchronized {
		if (isInitialized)
			throw new IllegalArgumentException(s"Cannot initialize column $column of $this: components have already been exported. Create components eagerly!")
		if (column.columns.size != 1 || column.sqlName.isEmpty)
			throw new IllegalArgumentException(s"Attempted to include $column as a column of $this: included component's columns: ${column.columns}; sqlName: ${column.sqlName}")
		if (!isSymLink(column)) {
			initColumns += column
			if (!column.enabled(NoSelect)) initSelectable += column
			if (!column.enabled(NoQuery)) initQueryable += column
			if (!column.enabled(NoUpdate)) initUpdatable += column
			if (!column.enabled(NoInsert)) initInsertable += column
			if (column.enabled(AutoGen)) initGenerated += column
		}
		column
	}

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
				initGenerated.initialize()

				//todo: make subcomponents type compatible with components
//				for (c <- components; if !isSymLink(c); sub <- c.subcomponents; if !isSymLink(sub))
//					initSubcomponents += sub
				initSubcomponents.initialize()
			}
		}

	protected final def initialize[T](value :T) :T = { initialize(); value }

	protected final def initializeBefore[T](block: =>T) :T = { initialize(); block }


	private[this] val delayedInits = new ListBuffer[Component[_]]

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
	  * it never changes value (and its value is immutable), hence reassigns to the cachable `var` do not result
	  * in any change of state.
	  */
	private[this] class LateInitComponents extends AbstractSeq[Component[_]] {
		private[this] var uninitialized = new ListBuffer[Component[_]]
		@volatile
		private[this] var initialized :InverseIndexSeq[Component[_]] = _
		private[this] var cached :InverseIndexSeq[Component[_]] = _

		def +=(comp :Component[_]) :Unit =
			if (isInitialized)
				throw new IllegalStateException(s"Can't enlist component $comp for $composite: the list of components has been already exported.")
			else
                uninitialized += comp

		def isInitialized :Boolean = initialized != null

		def initialize() :Unit = {
			cached = uninitialized.toList.indexed
			initialized = cached
		}

		private def items :InverseIndexSeq[Component[_]] =
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

		override def apply(idx :Int) :Component[_] = items(idx)

		override def foreach[U](f :Component[_] => U) :Unit = items.foreach(f)

		override def iterator :Iterator[Component[_]] = items.iterator

		override def indexOf[B >: Component[_]](elem :B) :Int = items.indexOf(elem)

		override def lastIndexOf[B >: Component[_]](elem :B) :Int = items.lastIndexOf(elem)
	}



	private[this] final val initComponents = new LateInitComponents
	private[this] final val initColumns = new LateInitComponents
	private[this] final val initSelectable = new LateInitComponents
	private[this] final val initQueryable = new LateInitComponents
	private[this] final val initUpdatable = new LateInitComponents
	private[this] final val initInsertable = new LateInitComponents
	private[this] final val initGenerated = new LateInitComponents
	private[this] final val initSubcomponents = new LateInitComponents

	final override def components :Seq[Component[_]] = initComponents
	final override def columns :Seq[Component[_]] = initColumns
	final override def selectable :Seq[Component[_]] = initSelectable
	final override def queryable :Seq[Component[_]] = initQueryable
	final override def updatable :Seq[Component[_]] = initUpdatable
	final override def insertable :Seq[Component[_]] = initInsertable
	final override def generated :Seq[Component[_]] = initGenerated
	final override def subcomponents :Seq[Component[_]] = initSubcomponents



	override def selectForm: SQLReadForm[S] = MappingReadForm.selectForm(this)
	override def queryForm: SQLWriteForm[S] = MappingWriteForm.queryForm(this)
	override def updateForm: SQLWriteForm[S] = MappingWriteForm.updateForm(this)
	override def insertForm: SQLWriteForm[S] = MappingWriteForm.insertForm(this)






	/** Prefix added to given names to all created instances of this.Column[_]. Defaults to "", subclasses may override. */
	protected def columnPrefix :String = ""

	override val buffs :Seq[Buff[S]] = Seq()







	class ColumnCopist[T](source :ColumnMapping[T, _], name :Option[String], options :Seq[Buff[T]]) {
		def this(source :ColumnMapping[T, _]) = this(source, None, source.buffs)

		def apply(pick :S=>T) :Column[T] =
			source match {
				case c :StaticMapping[_]#Column[_] if c.belongsTo(composite) => c.asInstanceOf[Column[T]]
				case _ => initialize(new ColumnCopy[T](source, pick, name, options))
			}

		def prefixed(prefix :Option[String]) :ColumnCopist[T] =
			new ColumnCopist(source, prefix.map(_ + (name getOrElse source.name)), options)

		def prefixed(prefix :String) :ColumnCopist[T] = prefixed(Some(prefix))

		def named(name :String) :ColumnCopist[T] =
			new ColumnCopist(source, Some(name), options)

		def set(options :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, options)

		def add(options :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, options ++: this.options)

		def remove(options :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, this.options.filterNot(options.contains) )
	}





	protected def column[T](col :ColumnMapping[T, _]) :ColumnCopist[T] =
		new ColumnCopist(col)

	protected def column[T](col :ColumnMapping[T, _], name :String) :ColumnCopist[T] =
		new ColumnCopist(col, Some(name), col.buffs)

	protected def column[T](col :ColumnMapping[T, _], options :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, None, options)

	protected def column[T](col :ColumnMapping[T, _], name :String, options :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, Some(name), options)




	protected def column[T](name :String, pick :S=>T, options :Buff[T]*)
	                       (implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		initialize(new ColumnComponent[T](pick, name, options))

	protected def column[T](pick :S=>T, options :Buff[T]*)(implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](PropertyPath.nameOf(pick), pick, options:_*)





	protected def autoins[T](name :String, pick :S=>Option[T], options :Buff[T]*)
	                        (implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](name, pick(_:S).get, AutoGen +: options:_*)

	protected def autoins[T](pick :S=>Option[T], options :Buff[T]*)
	                        (implicit form :SQLForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](PropertyPath.nameOf(pick), pick(_:S).get, options :_*)


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
}






trait StaticComponent[S, O <: AnyMapping] extends StaticMapping[S] with SubMapping[S, O]






object StaticMapping {

}


