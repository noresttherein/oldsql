package com.hcore.ogre.mapping.support

import com.hcore.ogre.mapping.ColumnMapping._
import com.hcore.ogre.mapping.ComponentPath.{SelfPath, DirectComponent, TypedComponentPath}
import com.hcore.ogre.mapping.MappingMorphism.{ComponentMorphism, ValueMorphism}
import com.hcore.ogre.mapping.Mapping.MappingExtension._
import com.hcore.ogre.mapping.Mapping._
import com.hcore.ogre.mapping.support.MappingAdapter.{MappingSubstitute, MappingImpostor}
import com.hcore.ogre.mapping.support.SymLinkMapping.BaseSymLink
import com.hcore.ogre.mapping._
import com.hcore.ogre.morsels.InverseIndexSeq
import com.hcore.ogre.morsels.InverseIndexSeq.implicitIndexing
import com.hcore.ogre.slang.options.extensions
import extensions._
import com.hcore.ogre.morsels.necromancy.PropertyChain
import com.hcore.ogre.slang.options.extensions
import com.hcore.ogre.sql.{SQLReadForm, SQLWriteForm}

import scala.collection.mutable.ListBuffer

/** Convenience base trait for mappings which are defined statically, rather than dynamically (i.e the mapped type and schema is mostly known).
  * This includes table mappings and other instances, where columns are known statically and should be created manually, rather than by some generic code.
  * While it exposes mutator methods to facilitate creation and declaration of components, it is expected that they'll be used solely in
  * the constructors of derived classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called previously, an exception will be thrown.
  *
  * It exists exclusively as a way to facilitate creating elegant schema descriptions and it's interface is directed towards implementing
  * classes; no other place in the system handles it in any special way, different from other implementations of Mapping[_].
  *
  * @tparam E value type of the mapped entity
  */
trait StaticMapping[E] extends Mapping[E] { composite =>

	/** Base trait for all components of this mapping. Creating an instance automatically lists it within owning mapping components
	  * as well any contained subcomponents and columns within parent mapping appropriate lists. 
	  * This registering is delayed, as in most cases properties listing its subcomponents will not be initialized before 
	  * its constructor is over, so instead it adds itself only to a 'waiting list', which is processed and its contents appropriately
	  * registered within the parent mapping once either this instance include() method is called, parent mapping explicitly 
	  * requests initializing all waiting lists via one of its 'init' methods, or any of the parent mapping component accessor methods are called.
	  * If for any reason this component is not fully initialized before one of that happens, and exception will be thrown during its initialization.
	  * For this reasons, all components should be created eagerly whenever possible - be very caucious with declaring memebers
	  * of this type as lazy val or object. As a consequence of the above, simple creation of this instance registers it permamently with
	  * the parent mapping - no other call is needed. If for some unforseen reason you will need an instance of it which is not really a part
	  * of the parent mapping, derive from EphemericComponent instead, but this may cause bugs in various places, as whole library assumes
	  * that for any object of type x.Component[_] can be safely passed to any methods of x.
	  *
	  * @tparam T value type of this component
	  */
	trait Component[T]  extends Mapping[T] { self =>

		def opt(implicit values :composite.Values) :Option[T] =
			values.get(this)

		private[StaticMapping] def belongsTo(mapping :StaticMapping[_]) = mapping eq composite

		/** A path from parent mapping to this component, serving at the same time as a trigger for single-time initialization.
		  * As this value is (and should) be lazy, when it's requested for the first time, it will call init() method. For this reason
		  * it should be called only after full creation of this instance (with any implemented subclasses and filling all its compoments).
		  */
		private[StaticMapping] final lazy val path = init()
//		protected def optional = false

		/** Manually trigger initialization of this instance within the parent mapping. */
		protected[StaticMapping] final def include() :this.type = { path; this }

		/** Register itself and all its subcompoments within the parent mapping. This method will be called only once. */
		private[StaticMapping] def init() : TypedComponentPath[composite.type, this.type, T] = composite.synchronized {
			directComps += this
			Path(componentsLift)
		}


		private[StaticMapping] def Path(lift :Component[_]=>Option[composite.Component[_]]) :TypedComponentPath[composite.type, self.type, T] =
			DirectComponent(composite :composite.type)(self :self.type)(
				surepick.map(ValueMorphism.homomorphism(_)) getOrElse ValueMorphism(pick), ComponentMorphism[Component, composite.Component](lift))

		private[StaticMapping] def componentsLift = {
			val liftedColumns = columns.map(liftColumn(_)).toMap[Component[_], composite.Component[_]]
			val liftedComponents = subcomponents.map { sub =>
				val comp = liftedColumns.get(sub) getOrElse lift(sub)
				sub -> (comp.include() :composite.Component[_])
			}.toMap[Component[_], composite.Component[_]]
			liftedComponents.orElse(liftedColumns).lift
//			liftedComponents
		}


		/** Select the value of this component from the parent entity. This value will be used to pass to its mapping methods on deassembling to column values. */
		protected[StaticMapping] def pick :E=>Option[T]
		protected[StaticMapping] def surepick :Option[E=>T]


		private def liftColumn[Q](column :Component[Q]) =
			column -> new LiftedColumn[Q](column)


		private def lift[Q](component :Component[Q]) :composite.Component[Q] =
			new LiftedComponent[Q](component)


		delayInit(this)



		private[StaticMapping] class LiftedComponent[Q](component :Component[Q]) extends MappingImpostor[Q, Component[Q]] with composite.Component[Q] {
			private val pathsuffix = (self :self.type) \\ (component :component.type)
			override protected val adaptee = component

			override protected[StaticMapping] val pick = (e :E) => self.pick(e).flatMap(pathsuffix.pick) :Option[Q]
			override protected[StaticMapping] val surepick =
				for (f <- self.surepick; g <-pathsuffix.surepick) yield (f andThen g)


			override private[StaticMapping] def init() =
				Path(comp => pathsuffix.lift(comp.asInstanceOf[component.Component[_]]).flatMap(self.path.lift(_)))

			override def toString = s"^$component"
		}


		private[StaticMapping] class LiftedColumn[Q](column :Component[Q]) extends LiftedComponent[Q](column) {
			private[StaticMapping] override def init() : TypedComponentPath[composite.type, this.type, Q] = {
				composite.include(this)
				Path(c => Some(this))
			}
		}


	}



	object Component {

		implicit def valueOf[T](component :composite.Component[T])(implicit values :Values) :T =
			values(component)

	}

	trait MandatoryComponent[T] extends Component[T] {
		final override protected[StaticMapping] def pick = (e :E) => Some(selector(e))
		final override protected[StaticMapping] def surepick = Some(selector)

		protected def selector :E=>T
	}

	trait OptionalComponent[T] extends Component[T] {
		final override protected[StaticMapping] def surepick = None
		def pick :(E => Option[T])
	}

	/** A component which is not registered as a part of the parent mapping on creation, or, most likely, ever. */
//	trait EphemericComponent[T] extends Component[T] {
//		private[StaticMapping] override def init() = Path(componentsLift.get)
//	}

	/** Convenience base component class which initializes accessor fields based on the constructor argument getter. */
	abstract class BaseComponent[T](value :E=>T) extends Component[T] {
		override protected[StaticMapping] val surepick = Some(value)
		override protected[StaticMapping] val pick =  (e :E) => Some(value(e))

	}

	/** Conveniance base class for creating components of this instance which themseles contain statically enumerated subcomponents
	  *
	  * @param value returns value of this component on a given parent entity
	  * @param columnPrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                     All columns accessed through this instance methods will be affected, not only when viewed from parent mappings.
	  *                     A column which is defined within a subcomponent of this component will not show the prefix however when viewed
	  *                     in the scope of that component - only the 'proxy' seen as this.Column[_] will.
	  * @tparam T value type of this component
	  */
	abstract class StaticComponent[T](value :E=>T, override final val columnPrefix :String="") extends BaseComponent[T](value) with StaticMapping[T] {
		override def toString = sqlName orElse columnPrefix.providing(_.length>0) getOrElse super.toString
	}

//	trait VirtualComponent[T, S] extends Component[T] with MappingSubstitute[T, S, Component[S]] {
//		val target :composite.Component[S]
//
//
//	}


	class SymLinkComponent[C<:Mapping[T], T](target :TypedComponentPath[this.type, C, T]) extends BaseSymLink[this.type, C, T](target) with Component[T] {
		override protected[StaticMapping] val pick: (E) => Option[T] = target.pick
		override protected[StaticMapping] val surepick: Option[(E) => T] = target.surepick
		override private[StaticMapping] def init() = Path(comp => target.lift(comp.adaptedComponent))
	}



	class EmbeddedComponent[T, C<:Mapping[T]](value :E=>T, mapping :C) extends MappingImpostor[T, C] with Component[T] {
		override protected[StaticMapping] val pick = (e: E) => Some(value(e))
			override protected[StaticMapping] val surepick = Some(value)
			override val adaptee: C = mapping
			override def toString = s"@$adaptee"
	}





	/** Base trait for all column components of this mapping. In general, there should be little need to descend your classes directly from it,
	  * as column functionality is limited and constructor methods provided within StaticMapping should be sufficient. As with the base Component
	  * trait, simple creation of this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options. This trait encompasses both direct and indirect (proxies for instances declared within subcomponents of the parent mapping,
	  * rather than at the top level) and doesn't register itself under direct compoments list, unlike the base Component[_] trait! For direct instances,
	  * use DirectColumn.
	  *
	  * @tparam T value type of this component
	  */
	trait Column[T] extends ColumnMapping[T] with Component[T] {
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
			directComps += this
			path
		}
	}



	private class ColumnComponent[T :ColumnType](value :E=>T, name :String, options :Seq[MappingExtension[T]])
		extends BaseColumn[T](columnPrefix + name, options) with DirectColumn[T]
	{
		val surepick = Some(value)
		val pick = (e :E) => Some(value(e))
	}


	private class ColumnCopy[T](val pick :E=>Option[T], val surepick :Option[E=>T], name :String, options :Seq[MappingExtension[T]])(implicit ev :ColumnType[T])
		extends BaseColumn[T](columnPrefix + name, options) with DirectColumn[T]
	{
		def this(column :ColumnMapping[T], value :E=>T, name :Option[String], options :Seq[MappingExtension[T]]) =
			this((e:E)=> Some(value(e)), Some(value), name getOrElse column.name, options)(column.columnType)
	}




	implicit def valueOf[T](component :Component[T])(implicit values :Values) :T = values(component)


	@inline
	final def apply[C<:Component[_]](component :this.type=>C) :ComponentPath[this.type, C] =
		SelfPath(this :this.type) \ component



	
	override implicit def \\[T](component: Component[T]): TypedComponentPath[this.type, component.type, T] =
		symLinkTarget(component) match {
			case Some(target) => SymLinkMapping.path[this.type, component.type, T](target, component.path.morphism)
			case _ => component.path
		}




	override def assemble(values: Values): Option[E] =
		try {
			construct(values).providing(isDefined(values))
		} catch {
			case e :NoSuchElementException => None
		}

	protected def construct(implicit res :Values) :E


	protected def isDefined(values :Values) :Boolean = true



	lazy val InsertParameters = SetParameters(this)(_.InsertParameters, insertable)
	lazy val UpdateParameters = SetParameters(this)(_.UpdateParameters, updatable)
	lazy val QueryParameters = SetParameters(this)(_.QueryParameters, querable)

	final lazy val components = afterInit(export(directComps))
	final lazy val columns = afterInit(export(allColumns))
	final lazy val selectable = afterInit(export(selects))
	final lazy val querable = afterInit(export(queries))
	final lazy val updatable = afterInit(export(updates))
	final lazy val insertable = afterInit(export(inserts))
	final lazy val generated = afterInit(export(auto))


	override def selectForm: SQLReadForm[E] = MappingReadForm.selectForm(this)
	override def queryForm: SQLWriteForm[E] = MappingWriteForm.queryForm(this)
	override def updateForm: SQLWriteForm[E] = MappingWriteForm.updateForm(this)
	override def insertForm: SQLWriteForm[E] = MappingWriteForm.insertForm(this)

	final lazy val subcomponents =
		components.filterNot(isSymLink(_)).flatMap(
			c => c +: c.subcomponents.flatMap (
				sub => c.path.lift(sub) :Option[Component[_]]
			).filterNot(isSymLink(_))
		).indexed


	private def include[T](column :Component[T]) = synchronized {
		if (exported)
			throw new IllegalArgumentException(s"cannot initialize column $column of $this: components have already been exported. Create components eagerly!")
		if (column.columns.size!=1 || column.sqlName.isEmpty)
			throw new IllegalArgumentException(s"attempted to include $column as a column of $this: included component's columns: ${column.columns}; sqlName: ${column.sqlName}")
		if (!isSymLink(column)) {
			allColumns += column
			if (!column.enabled(NoQuery)) queries += column
			if (!column.enabled(NoSelect)) selects += column
			if (!column.enabled(NoInsert)) inserts += column
			if (!column.enabled(NoUpdate)) updates += column
			if (column.enabled(AutoGen)) auto += column
		}
		column
	}
	
	private[this] val directComps = new ListBuffer[Component[_]]
	private[this] val allColumns = new ListBuffer[Component[_]]()
	private[this] val selects = new ListBuffer[Component[_]]()
	private[this] val queries = new ListBuffer[Component[_]]()
	private[this] val updates = new ListBuffer[Component[_]]()
	private[this] val inserts = new ListBuffer[Component[_]]()
	private[this] val auto = new ListBuffer[Component[_]]()
	@volatile
	private[this] var exported = false


	private def delayInit(c :Component[_]) :Unit =
		if (exported)
			throw new IllegalStateException(s"cannot initialize mapping component $c on $this: components have already been exported")
		else
			delayedInits += c

	private[this] val delayedInits = new ListBuffer[Component[_]]()

	protected final def initialize() = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	protected final def initAfter[T](value :T) = { initialize(); value }

	protected final def afterInit[T](block : =>T) = { initialize(); block }

	private def export[T](list :ListBuffer[T]) :InverseIndexSeq[T] = afterInit {
		exported = true
		list.toList
	}



	/** Prefix added to given names to all created instances of this.Column[_]. Defaults to "", subclasses may override. */
	protected def columnPrefix :String = ""

	override val modifiers :Seq[MappingExtension[E]] = Seq()




	


	class ColumnCopist[T](source :ColumnMapping[T], name :Option[String], options :Seq[MappingExtension[T]]) {
		def this(source :ColumnMapping[T]) = this(source, None, source.modifiers)

		def apply(pick :E=>T) :Column[T] =
			source match {
				case c:StaticMapping[_]#Column[_] if c.belongsTo(composite) => c.asInstanceOf[Column[T]]
				case _ => initAfter(new ColumnCopy[T](source, pick, name, options))
			}

		def prefixed(prefix :Option[String]) :ColumnCopist[T] =
			new ColumnCopist(source, prefix.map(_ + (name getOrElse source.name)), options)

		def prefixed(prefix :String) :ColumnCopist[T] = prefixed(Some(prefix))

		def named(name :String) :ColumnCopist[T] =
			new ColumnCopist(source, Some(name), options)

		def set(options :MappingExtension[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, options)

		def add(options :MappingExtension[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, options ++: this.options)

		def remove(options :MappingExtension[T]*) :ColumnCopist[T] =
			new ColumnCopist(source, name, this.options.filterNot(options.contains) )
	}





	protected def column[T](col :ColumnMapping[T]) :ColumnCopist[T] =
		new ColumnCopist(col)

	protected def column[T](col :ColumnMapping[T], name :String) :ColumnCopist[T] =
		new ColumnCopist(col, Some(name), col.modifiers)

	protected def column[T](col :ColumnMapping[T], options :Seq[MappingExtension[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, None, options)

	protected def column[T](col :ColumnMapping[T], name :String, options :Seq[MappingExtension[T]]) :ColumnCopist[T] =
		new ColumnCopist(col, Some(name), options)



	


	protected def column[T :ColumnType](name :String, pick :E=>T, options :MappingExtension[T]*) :Column[T] =
		initAfter(new ColumnComponent[T](pick, name, options))


	protected def column[T :ColumnType](pick :E=>T)(name :String, options :MappingExtension[T]*) :Column[T] =
		column[T](name, pick, options:_*)





	protected def autoins[T :ColumnType](name :String, pick :E=>Option[T], options :MappingExtension[T]*) :Column[T] =
		this.column[T](name, pick(_:E).get, AutoGen +: options:_*)



	def symLink[T](component :Component[T]) :SymLinkComponent[component.type, T] =
		symLink(component.path)

	def symLink[M<:Mapping[T], T](path :TypedComponentPath[this.type, M, T]) :SymLinkComponent[M, T] =
		new SymLinkComponent[M, T](path)

//	def symLink[M<:Mapping](path :ComponentPath[this.type, M]) :SymLinkComponent[M, path.ValueType] =
//		new SymLinkComponent[M, path.ValueType](path)


	protected def embed[T](value :E=>T, mapping :Mapping[T]) :EmbeddedComponent[T, mapping.type] =
//		initAfter(new EmbeddedComponent[T, mapping.type](value, mapping))
		new EmbeddedComponent[T, mapping.type](value, mapping)

}




