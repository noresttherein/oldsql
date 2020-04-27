package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Unique.implicitUnique
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, ComponentExtractor, GenericMapping, RootMapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, BuffMappingFailureException, Ignored, NoInsert, NoQuery, NoSelect, NoUpdate, ReadOnly}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.Mapping.MappingOf
import net.noresttherein.oldsql.schema.support.ComponentProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, AdaptedAs}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.{MappedMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.ComponentExtractor.ColumnExtractor
import net.noresttherein.oldsql.slang._

import scala.collection.AbstractSeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag





//todo: provide examples
/** Convenience base trait for mappings with a fixed, hierarchical structure, which subjects are assembled
  * from subcomponents of arbitrary depth. This includes table mappings and other instances where columns are known
  * statically and should be created manually, rather than by some generic code. While it exposes mutator methods
  * to facilitate creation and declaration of components, it is expected that they'll be used solely in the constructors
  * of derived classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called
  * previously, an exception will be thrown.
  *
  * It exists exclusively as a way to facilitate creating schema descriptions and its interface is directed
  * towards implementing classes; it is not treated in any special way by other parts of the library,
  * different from other implementations of `Mapping`.
  *
  * @tparam S the `Subject` type of the mapped entity.
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  */
trait MappingFrame[S, O] extends StaticMapping[S, O] { composite =>

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
	trait FrameComponent[T] extends GenericMapping[T, O] { self =>

		/** Returns the value of this component in an option if an implicit `ComponentValues` instance 
		  * for the enclosing composite mapping is present. 
		  */
		def ?(implicit values :composite.Pieces) :Option[T] = values.get(selector)



		private[MappingFrame] def belongsTo(mapping :MappingFrame[_, _]) :Boolean = mapping eq composite

		protected[schema] def extractor :Extractor[S, T]

		/** The `ComponentExtractor` for this component from the enclosing mapping.
		  * The call to this method will trigger the initialization of this component if it was not initialized before.
		  */
		def selector :ComponentExtractor[S, T, O] = {
			if (fastSelector == null) {
				val s = safeSelector
				if (s != null)
					fastSelector = s
				else {
					fastSelector = init()
					safeSelector = fastSelector
				}
			}
			fastSelector
		}

		@volatile
		private[this] var safeSelector :ComponentExtractor[S, T, O] = _
		private[this] var fastSelector :ComponentExtractor[S, T, O] = _

		private[this] var initialized = false

		/** Manually trigger initialization of this instance within the enclosing mapping. */
		private[MappingFrame] final def include() :this.type = synchronized {
			if (!initialized) {
				initialized = true
				fastSelector = init()
				safeSelector = fastSelector
			}
			this
		}

		/** Register itself and all its subcomponents within the parent mapping. This method will be called only once. */
		protected[MappingFrame] def init() :ComponentExtractor[S, T, O] = composite.synchronized {
			initComponents += this
			initSubcomponents += this
			columns foreach { col => exportColumn(col) }
			subcomponents foreach { comp => exportSubcomponent(comp) }
			selectorFor(this)
		}



		/** Lifts ('exports') a column of this component to the column of the enclosing composite `MappingFrame`.
		  * This method is called both during delayed initialization of standard components, and eagerly
		  * by `ExportComponent` in its constructor. For this reason it should not touch any column/component lists.
		  */
		protected[MappingFrame] def exportColumn[U](column :Column[U]) :FrameColumn[U] =
			exportSubcomponent(column).asInstanceOf[FrameColumn[U]]

		/** Lifts ('exports') a component of this component to the subcomponent of the enclosing composite `MappingFrame`.
		  * This method is called both during delayed initialization of standard components, and eagerly
		  * by `ExportComponent` in its constructor. For this reason it should not touch any column/component lists.
		  */
		protected[MappingFrame] def exportSubcomponent[U](subcomponent :Component[U]) :FrameComponent[U] =
			subcomponent match {
				case mine :MappingFrame[_, _]#FrameComponent[_] if mine belongsTo composite =>
					mine.asInstanceOf[FrameComponent[U]]

				case _ => composite.synchronized {
					initSelectors.getOrElse(subcomponent, {
						val fromMe = apply(subcomponent)
						val fromComposite = extractor andThen fromMe

						if (columnPrefix == null)
							throw new IllegalStateException(
								s"$this.columnPrefix is null: overrides with a val must happen before any component declarations!")
						if (buffs == null)
							throw new IllegalStateException(
								s"$this.buffs is null: overrides with a val must happen before any component declarations!")

						val subbuffs = subcomponent.buffs :++ schema.cascadeBuffs(this)(fromMe)

						val selector = subcomponent match {
							case column :ColumnMapping[_, _] =>
								val lifted = new ColumnComponent[U](
									fromComposite, columnPrefix + column.name, subbuffs)(column.form
								)
								selectorFor(lifted)
							case _ =>
								val lifted = new ExportComponent[U](
									subcomponent, fromComposite, columnPrefix, subbuffs
								)
								selectorFor(lifted)
						}
						initSelectors = initSelectors.updated(subcomponent, selector)
						selector
					}).export.asInstanceOf[FrameComponent[U]]
				}
			}



		/** Buffs 'inherited' from the enclosing mapping. It uses the value selector `S=>T` to map all buffs applied
		  * to the enclosing mapping to adapt them to this value type. This means that if there are any
		  * [[net.noresttherein.oldsql.schema.Buff.ValuedBuff ValuedBuff]]s among the inherited buffs, this component
		  * must have a value for the value associated with that buff or an exception will be thrown - either
		  * when accessing/initializing the buff list or when the value for the buff is actually needed.
		  */
		protected final def inheritedBuffs :Seq[Buff[T]] = conveyBuffs(extractor, toString)

		/** The buffs for this component, including inherited buffs from the enclosing mapping at the back  */
		override def buffs :Seq[Buff[T]] = {
			val mine = super.buffs
			val inherited = inheritedBuffs
			if (mine.isEmpty) inherited
			else if (inherited.isEmpty) mine
			else mine ++: inherited
		}

		/** The column prefix prepended to all columns at the behest of the enclosing `MappingFrame` or an empty string. */
		protected final def inheritedPrefix :String = verifiedPrefix

		/** The column prefix prepended to all columns by this component ''and'' the enclosing `MappingFrame`.
		  * Defaults to `inheritedPrefix`. */
		protected def columnPrefix :String = inheritedPrefix


		//enlist ourselves on the list of uninitialized components of the enclosing mapping
		delayInit(this)

	}






	/** A proxy class for non-direct subcomponents of the enclosing `MappingFrame` which incorporates any column prefix
	  * and buffs defined in it and any other enclosing components into the adapted component.
	  * Its components are at the same time the export components of the enclosing mapping.
	  */
	private class ExportComponent[T](target :Component[T], val extractor :Extractor[S, T],
	                                 override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[Component[T], T, O](target) with FrameComponent[T]
	{
		def adapted :Component[T] = egg

		protected[MappingFrame] override def init() :ComponentExtractor[S, T, O] = {
			initSubcomponents += this
			initSelectors.getOrElse(target, selectorFor(this)).asInstanceOf[ComponentExtractor[S, T, O]]
		}

		override def canEqual(that :Any) :Boolean = that match {
			case comp :MappingFrame[_, _]#ExportComponent[_] => comp belongsTo composite
			case _ => false
		}

		override protected def adapt[X](component :egg.Component[X]) :Component[X] =
			exportSubcomponent(component)

		override protected def adaptColumn[X](column :egg.Column[X]) :Column[X] =
			exportColumn(column)

		override def toString :String = "^" + egg + "^"

	}



	/** Base trait for components which have a value for all instances of `T`. */
	trait MandatoryComponent[T] extends FrameComponent[T] {
		protected[schema] override def extractor :RequisiteExtractor[S, T] = Extractor.req(extract)
		protected def extract :S => T
	}

	/** Base trait for components which might not have a value for all instances of `T` (such as properties declared
	  * by some subclass of `T`. This is independent from the
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect ]]
	  */
	trait OptionalComponent[T] extends FrameComponent[T] {
		protected[schema] override def extractor = Extractor(extract)
		protected def extract :S => Option[T]
	}

	trait ReadOnlyComponent[T] extends FrameComponent[T] {
		protected[schema] override def extractor :Extractor[S, Nothing] = Extractor.none
	}

	/** Convenience base component class which initializes the extractor using the constructor argument getter.
	  * @param value extractor function for the value of this component.
	  * @param opts buffs specific to this component. This list will be extended with buffs inherited
	  *             from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *             to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.BaseOptionalComponent]]
	  */
	abstract class BaseComponent[T](value :S => T, opts :Buff[T]*) extends FrameComponent[T] {
		protected[schema] override val extractor :Extractor[S, T] = Extractor.req(value)
		override val buffs :Seq[Buff[T]] = opts ++: inheritedBuffs
	}

	/** Convenience base class for components of this instance which themselves contain individually defined
	  * static subcomponents.
	  * @param value  returns value of this component on a given parent entity.
	  * @param prefix prefix which will be added to all columns, direct and transitive within this instance.
	  *               All columns accessed through this instance methods will be affected, not only when viewed
	  *               from parent mappings. A column which is defined within a subcomponent of this component
	  *               will not show the prefix however when viewed in the scope of that component - only
	  *               the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *               of this component will be the concatenation of the column prefix defined by the enclosing
	  *               mapping and this prefix.
	  * @param opts   buffs specific to this component. This list will be extended with buffs inherited
	  *               from the enclosing schema. The export versions of all subcomponents of this component will
	  *               inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class ComponentFrame[T](value :S => T, prefix :String, opts :Seq[Buff[T]] = Nil)
		extends BaseComponent[T](value, opts :_*) with MappingFrame[T, O]
	{
		def this(value :S => T, buffs :Buff[T]*) = this(value, "", buffs)

		final override val columnPrefix = composite.verifiedPrefix + prefix
	}



	/** Convenience base class for components which may not have a value for some instances of the enclosing mapping's
	  * subject type `S`.
	  * @param value extractor function for the value of this component.
	  * @param opts buffs specific to this component. This list will be extended with buffs inherited
	  *             from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *             to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.BaseComponent]]
	  */
	abstract class BaseOptionalComponent[T](value :S => Option[T], opts :Buff[T]*)
		extends OptionalComponent[T]
	{
		protected[schema] override val extractor = Extractor(value)
		override val buffs :Seq[Buff[T]] = opts ++: inheritedBuffs
	}

	/** Convenience base class for optional components of this instance which themselves contain individually defined
	  * static subcomponents.
	  * @param value  returns value of this component on a given parent entity.
	  * @param prefix prefix which will be added to all columns, direct and transitive within this instance.
	  *               All columns accessed through this instance methods will be affected, not only when viewed
	  *               from parent mappings. A column which is defined within a subcomponent of this component
	  *               will not show the prefix however when viewed in the scope of that component - only
	  *               the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *               of this component will be the concatenation of the column prefix defined by the enclosing
	  *               mapping and this prefix.
	  * @param opts   buffs specific to this component. This list will be extended with buffs inherited
	  *               from the enclosing schema. The export versions of all subcomponents of this component will
	  *               inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class OptionalComponentFrame[T](value :S => Option[T], prefix :String, opts :Seq[Buff[T]] = Nil)
		extends BaseOptionalComponent[T](value, opts :_*) with MappingFrame[T, O]
	{
		def this(value :S => Option[T], buffs :Buff[T]*) = this(value, "", buffs)

		final override val columnPrefix = composite.verifiedPrefix + prefix
	}






	/** An adapter allowing embedding of any other component, regardless of the `Origin` type, as part of this mapping.
	  * The embedded component is not exposed to the outside. The components of the embedded mapping are exported
	  * as components of the enclosing mapping by incorporating the `columnPrefix` and `buffs` given here (including
	  * those inherited from the enclosing mapping). At the same time, they form the components of this instance exposed
	  * in its component and column lists.
	  * @param component the embedded component.
	  * @param extractor the extractor for the value of this component.
	  * @param columnPrefix the prefix string prepended to the names of all (export) columns of `body`.
	  *                     The value of `columnPrefix` of the enclosing mapping is ''not'' prepended automatically
	  *                     to this value - it should be done by the caller if required.
	  * @param buffs the buffs attached to this component, inherited by all of its export subcomponents. The buffs
	  *              of the enclosing mapping are ''not'' automatically added to this list - it should be done
	  *              by the caller if required.
	  * @tparam T the value type of this component.
	  */
	private class CompositeComponent[T] private[MappingFrame]
	                                (component :MappingOf[T], protected[schema] val extractor :Extractor[S, T],
	                                 override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[MappingOf[T], T, O](component) with FrameComponent[T]
	{ nest =>

		protected[MappingFrame] override def init() :ComponentExtractor[S, T, O] = {
			if (initSelectors contains component.asInstanceOf[AnyComponent])
				throw new IllegalArgumentException(
					s"Can't embed mapping $component as a component of $composite as it is already present."
				)
			val selector = selectorFor(this)
			initSelectors = initSelectors.updated(component.asInstanceOf[AnyComponent], selector)
			initComponents += this
			initSubcomponents += this //don't export subcomponents as its done by EagerDeepProxy
			selector
		}

		protected override def adapt[X](component :egg.Component[X]) :Component[X] =
			exportSubcomponent(component.asInstanceOf[Component[X]])

		protected override def adaptColumn[X](component :egg.Column[X]) :Column[X] =
			exportColumn(component.asInstanceOf[Column[X]])


		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def toString :String = "{{" + egg + "}}"

	}






	/** Base trait for columns of the enclosing `MappingFrame`. It is extended by all directly declared columns
	  * as well as the export versions of transitive columns from its subcomponents. In general, there should be 
	  * little need to extend your classes directly from it, as column functionality is limited and constructor methods 
	  * provided within `MappingFrame` should be sufficient. As with the base `FrameComponent` trait, simple creation of
	  * this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options (buffs). It will also be registered on the subcomponents list, but not the one
	  * for direct components.
	  * @tparam T value type of this column.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.DirectColumn]]
	  */
	trait FrameColumn[T] extends FrameComponent[T] with ColumnMapping[T, O] {

		override def selector :ColumnExtractor[S, T, O] = super.selector.asInstanceOf[ColumnExtractor[S, T, O]]

		protected[MappingFrame] override def init() :ComponentExtractor[S, T, O] = {
			includeColumn(this)
			selectorFor(this)
		}
	}



	/** A column declared directly under this mapping.
	  * It automatically inherits the enclosing mapping's `columnPrefix` and buffs.
	  */
	protected class DirectColumn[T :ColumnForm](override val extractor :S =?> T, name :String, opts :Seq[Buff[T]])
		extends StandardColumn[T, O](verifiedPrefix + name, opts ++ conveyBuffs(extractor, verifiedPrefix + name))
		   with FrameColumn[T]
	{
		def this(value :S => T, name :String, opts :Seq[Buff[T]]) =
			this(Extractor.req(value), name, opts)

		override val buffs :Seq[Buff[T]] = opts :++ inheritedBuffs

		protected[MappingFrame] override def init() :ColumnExtractor[S, T, O] = {
			initComponents += this
			composite.includeColumn(this)
			selectorFor(this)
		}
	}



	/** A lower-level column component which accepts values for all abstract fields and initializes them verbatim.
	  * This column will ''not'' automatically inherit the enclosing mapping's `columnPrefix` or buffs.
	  * It is not included on the direct components list of this mapping.
	  */
	private class ColumnComponent[T](val extractor :Extractor[S, T], name :String, override val buffs :Seq[Buff[T]])
	                                (implicit sqlForm :ColumnForm[T])
		extends StandardColumn[T, O](name, buffs) with FrameColumn[T]
	{
		def this(value :S => T, name :String, buffs :Seq[Buff[T]])(implicit form :ColumnForm[T]) =
			this(Extractor.req(value), name, buffs)

		def this(pick :S => Option[T], surepick :Option[S=>T], name :String, buffs :Seq[Buff[T]])
		        (implicit form :ColumnForm[T]) =
			this(surepick map Extractor.req[S, T] getOrElse Extractor(pick), name, buffs)

		protected[MappingFrame] override def init() :ColumnExtractor[S, T, O] = {
			composite.includeColumn(this)
			selectorFor(this)
		}
	}


	
	
	
	
	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * This is the most generic variant of the method, accepting complete column name prefix and buff list of
	  * the exported component. This allows to bypass their inheritance from the enclosing mapping and remove
	  * any desired buffs from those already present on the component.
	  * @param mapping any component mapping.
	  * @param extractor an `Extractor` returning the value of the component from the enclosing mapping's subject.
	  * @param fullPrefix a string added as a prefix to all column names of the exported component.
	  * @param fullBuffs complete list of buffs of the exported component.
	  * @tparam T the subject type of the embedded component.
	  * @return The 'export' version of the adapted component, with its name prefix and buffs reset.
	  */
	protected def component[T](mapping :MappingOf[T], extractor :S =?> T, fullPrefix :String, fullBuffs :Seq[Buff[T]])
			:Component[T] =
		synchronized {
			initPreceding()
			initPreceding(new CompositeComponent(mapping, extractor, fullPrefix, fullBuffs))
		}

	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
 	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the created component. They will precede any buffs already present
	  *              on the component and buffs inherited from the enclosing mapping will be appended at the end.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  */
	protected def component[T](columnPrefix :String, value :S => T, buffs :Buff[T]*)(implicit mapping :MappingOf[T])
			:Component[T] =
	{
		val extractor = Extractor.req(value)
		val allBuffs = buffs ++: mapping.buffs ++: conveyBuffs(extractor, mapping.toString)
		component(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}

	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param buffs buffs to attach to the created component. They will precede any buffs already present
	  *              on the component and buffs inherited from the enclosing mapping will be appended at the end.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  */
	protected def component[T](value :S => T, buffs :Buff[T]*)(implicit mapping :MappingOf[T]) :Component[T] =
		component("", value, buffs :_*)

	/** Embeds a read-only component directly under this instance, regardless of its `Origin` type.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the created component. They will precede any buffs already present
	  *              on the component and buffs inherited from the enclosing mapping will be appended at the end.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  */
	protected def component[T](columnPrefix :String = "", buffs :Seq[Buff[T]] = Nil)(implicit mapping :MappingOf[T])
		:Component[T] =
	{
		val extractor = Extractor.none :S =?> T
		val allBuffs = buffs ++: mapping.buffs ++: conveyBuffs(extractor, mapping.toString)
		component(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}






	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * The export version will have exactly the buffs and column name prefix specified here; this allows
	  * bypassing their inheritance from this mapping as well as removing any buffs present on the component.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param extractor an extractor returning the value of this component for a given subject value of this mapping.
	  * @param fullPrefix a string prepended to all column names
	  * @param fullBuffs buffs to attach to the front of the created component's buff list.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  */
	protected def embed[M <: Component[T], T]
	                   (component :M, extractor :S =?> T, fullPrefix :String, fullBuffs :Seq[Buff[T]]) :M =
		synchronized {
			initPreceding()
			if (initSelectors contains component)
				throw new IllegalArgumentException(s"Can't embed the component $component in $this for the second time.")

			val export = new ExportComponent[T](component, extractor, fullPrefix, fullBuffs)
			initComponents += export
			initPreceding()
			initSelectors = initSelectors.updated(component, export.selector)
			component
		}



	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  */
	protected def embed[M <: Component[T], T](value :S => T, columnPrefix :String, buffs :Buff[T]*)
	                                         (implicit component :M) :M =
	{
		val extractor = Extractor.req(value)
		val allBuffs = buffs ++: component.buffs ++: conveyBuffs(extractor, component.toString)
		embed(component, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}

	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  */
	protected def embed[M <: Component[T], T](value :S => T, buffs :Buff[T]*)(implicit component :M) :M =
		embed(value, "", buffs :_*)

	/** Embeds a read-only mapping of type `T` as a component of this instance. The 'export' version of the component
	  * will be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list. The component will
	  * be automatically annotated with the `ReadOnly` buff.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly]]
	  */
	protected def embed[M <: Component[T], T](columnPrefix :String = "", buffs :Seq[Buff[T]] = Nil)
	                                         (implicit component :M) :M =
	{
		val extractor = Extractor.none :S =?> T
		val allBuffs = ReadOnly[T] +: buffs ++: component.buffs ++: conveyBuffs(extractor, component.toString)
		embed(component, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}






	/** A builder adapting a given column template to a column of this `MappingFrame`. */
	protected class ColumnCopist[T] private[MappingFrame](template :ColumnMapping[T, _], name :Option[String], buffs :Seq[Buff[T]]) {
		private[MappingFrame] def this(source :ColumnMapping[T, _]) = this(source, None, source.buffs)

		template match {
			case self :MappingFrame[_, _]#FrameComponent[_] if self.belongsTo(composite) =>
				if (buffs == null)
					throw new IllegalStateException(s"$this.buffs is null: overrides must be defined before any components.")
				if (verifiedPrefix.length > 0 || buffs.nonEmpty )
					throw new IllegalArgumentException(
						s"Can't use column $template of the same mapping $composite as the template for a new column."
					)
			case _ =>
		}

		/** Create a new column using the passed function as its getter and enlist it on all applicable column lists
		  * of this mapping.
		  * @param getter a function returning the value for this column from the mapped entity.
		  * @return a new column, being a direct component of this mapping.
		  */
		def apply(getter :S => T) :Column[T] = composite.synchronized {
			val column = new ColumnComponent[T](getter, verifiedPrefix + (name getOrElse template.name), buffs)(template.form)
			initPreceding()
			initComponents += column
			column
		}

		/** Create a new column using the passed function as its getter and enlist it on all applicable column lists
		  * of this mapping.
		  * @param getter a function returning the value for this column from the mapped entity. If the function
		  *               returns `None`, the `nullValue` from the `ColumnForm` of the template column is used.
		  * @return a new column, being a direct component of this mapping.
		  */
		def opt(getter :S => Option[T]) :Column[T] = composite.synchronized {
			val column = new ColumnComponent[T](getter, None, verifiedPrefix + (name getOrElse template.name), buffs)(template.form)
			initPreceding()
			initComponents += column
			column
		}


		/** Sets the name of the created column to the concatenation of the given prefix and the name
		  * of the template column. If `prefix` is `None` the template column's name is used unchanged.
		  */
		def prefixed(prefix :Option[String]) :ColumnCopist[T] =
			new ColumnCopist(template, prefix.mapOrElse(p => Some(p + (name getOrElse template.name)), name), buffs)

		/** Sets the name of the created column to the concatenation of the given prefix and the name
		  * of the template column. */
		def prefixed(prefix :String) :ColumnCopist[T] = prefixed(Some(prefix))

		/** Sets the name of the created column, replacing the name of the template column. */
		def named(name :String) :ColumnCopist[T] =
			new ColumnCopist(template, Some(name), buffs)

		/** Replaces the `buff` list of the template column with the provided buffs. */
		def set(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs)

		/** Prepends the given buffs to the list of buffs of the template column. */
		def add(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs ++: this.buffs)

		/** Removes all provided buffs from the buff list of the created column. The buffs are compared using
		  * their `equals` method. Note that this means that some buffs such as those which generate a new value
		  * on every access implement `equals` as reference equality, so the exact instance from the buff list
		  * must be used for it to be removed.
		  */
		def remove(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, this.buffs.filterNot(buffs.contains) )
	}




	/** Create a column based on the given column.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[T, _]) :ColumnCopist[T] =
		new ColumnCopist(template)

	/** Create a column based on the given column, giving it a new name.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[T, _], name :String) :ColumnCopist[T] =
		new ColumnCopist[T](template, Some(name), template.buffs)

	/** Create a column based on the given column, replacing its buff list with the provided buffs.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[T, _], buffs :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(template, None, buffs)

	/** Create a column based on the given column, replacing its name and buff list with the provided values.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[T, _], name :String, buffs :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(template, Some(name), buffs)



	/** Create a new column as a direct component of this mapping.
	  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
	  * @param value the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @param form an implicit form defining how the column value `T` is mapped to the underlying SQL type
	  *             of the JDBC API.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def column[T](name :String, value :S => T, buffs :Buff[T]*)
	                       (implicit form :ColumnForm[T]) :Column[T] =
		initPreceding(new DirectColumn[T](value, name, buffs))

	/** Create a new column representing a property of the mapped subject as a direct component of this mapping.
	  * The column's name will be the concatenation of this mapping's `columnPrefix` and the name of the property
	  * as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName` as the `value`
	  * argument will set the column name to "firstName".
	  * @param value the getter function for the value of this column. Should represent a simple property of the
	  *              mapped subject of this mapping.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @param form an implicit form defining how the column value `T` is mapped to the underlying SQL type
	  *             of the JDBC API.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def column[T](value :S => T, buffs :Buff[T]*)(implicit form :ColumnForm[T], subject :TypeTag[S]) :Column[T] =
		column[T](PropertyPath.nameOf(value), value, buffs:_*)



	/** Create a new column for an optional property of the mapped subject as a direct component of this mapping.
	  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
	  * instead.
	  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
	  * @param value the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @param form an implicit form defining how the column value `T` is mapped to the underlying SQL type
	  *             of the JDBC API.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def optcolumn[T](name :String, value :S => Option[T], buffs :Buff[T]*)
	                          (implicit form :ColumnForm[T]) :Column[T] =
		initPreceding(new DirectColumn[T](Extractor(value), name, buffs))


	/** Create a new column for an optional property of the mapped subject as a direct component of this mapping.
	  * The name of the column will be the concatenation of this mapping's `columnPrefix` and the name of the
	  * getter property as seen in scala and reflected by `PropertyPath`.  For example, passing `_.firstName`
	  * as the `value` argument will set the column name to "firstName".
	  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
	  * instead.
	  * @param value the getter function for the value of this column. Should represent a simple property of the
	  *              mapped subject of this mapping.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @param form an implicit form defining how the column value `T` is mapped to the underlying SQL type
	  *             of the JDBC API.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  * @see [[net.noresttherein.oldsql.model.PropertyPath]]
	  */
	protected def optcolumn[T](value :S => Option[T], buffs :Buff[T]*)
	                          (implicit form :ColumnForm[T], subject :TypeTag[S]) :Column[T] =
		optcolumn[T](PropertyPath.nameOf(value), value, buffs: _*)



	/** Create a new column as a direct component of this instance, which is not mapped to any property
	  * of this mapping's subject. The created column will appear only on the `columns` property of this mapping
	  * and not any of the lists dedicated to particular operations, as it will define buffs indicating it should
	  * be ignored by every database operation. Such a column can still be used manually when creating SQL statements,
	  * for example to narrow down the result set, excluding rows which should be ignored by the application.
	  * @param name the name of the column (the complete name will include `this.columnPrefix`).
	  * @param buffs the buffs to attach to the created column. This list will be prepended with `Buff.Ignored[T]`.
	  * @return a new column, enlisted on the `columns`, `components` and `subcomponents` property of this mapping
	  *         and none other.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Ignored]]
	  */
	protected def column[T :ColumnForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new ColumnComponent[T](_ => None, None, name, Ignored[T] +: buffs))



	/** Create a new column as a direct component of this instance, which value is never updated or inserted
	  * by the application.
	  * @param name the name of the column (the complete name will include `this.columnPrefix`).
	  * @param buffs the buffs to attach to the created column. This list will be prepended with `Buff.ReadOnly[T]`.
	  * @return a new column, enlisted on the `columns`, `selectable`, `components` and `subcomponents`
	  *         properties of this mapping and none other.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly]]
	  */
	protected def readOnly[T :ColumnForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new ColumnComponent[T](_ => None, None, name, ReadOnly[T] +: buffs))



	/** Create a new database-generated column as a direct component of this mapping. The column is excluded
	  * from insert statement's parameters, but included in the `ResultSet` read as its result and assigned to
	  * the inserted entity.
	  * @param name the name of the column (the complete name will include `this.columnPrefix`).
	  * @param value the getter function for the value for this column.
	  * @param buffs the buffs attached to this column. `Buff.AutoInsert` and mapped buffs of `this.buffs` are added
	  *              to the list automatically.
	  * @return a new column, which will not appear on the `insertable` column list (and possibly others, based on
	  *         buffs defined here and inherited from this mapping).
	  */
	protected def autoins[T](name :String, value :S => Option[T], buffs :Buff[T]*)
	                        (implicit form :ColumnForm[T], tpe :TypeTag[S]) :Column[T] =
		column[T](name, value(_:S).get, AutoInsert[T] +: buffs :_*)

	/** Create a new database-generated column as a direct component of this mapping. The column is excluded
	  * from insert statement's parameters, but included in the `ResultSet` read as its result and assigned to
	  * the inserted entity.
	  * The name of the column will be the concatenation of this mapping's `columnPrefix` and the name of the
	  * getter property as seen in scala and reflected by `PropertyPath`.  For example, passing `_.firstName`
	  * as the `value` argument will set the column name to "firstName".
	  * @param value the getter function for the value of this column. Should represent a simple property of the
	  *              mapped subject of this mapping.
	  * @param buffs the buffs attached to this column. `Buff.AutoInsert` and mapped buffs of `this.buffs` are added
	  *              to the list automatically.
	  * @return a new column, which will not appear on the `insertable` column list (and possibly others, based on
	  *         buffs defined here and inherited from this mapping).
	  * @see [[net.noresttherein.oldsql.model.PropertyPath]]
	  */
	protected def autoins[T](value :S => Option[T], buffs :Buff[T]*)
	                        (implicit form :ColumnForm[T], tpe :TypeTag[S]) :Column[T] =
		autoins[T](PropertyPath.nameOf(value), value, buffs :_*)






	/** Renames a column of this mapping. The column might be either directly declared by this mapping or
	  * any of its subcomponents preceding this call. The new column will have the buffs of its export version,
	  * as would be returned by `apply(column)`. It is not required that `column` is an instance of
	  * `ColumnMapping` or that it upholds its invariants: only that it declares exactly one column
	  * and has non empty `sqlName`. This means that adapted/mapped columns are accepted.
	  * @throws IllegalArgumentException if `column` is not a subcolumn of this mapping or it is an export column
	  *                                  (one declared directly under this mapping or lifted from a subcomponent).
	  * @throws NoSuchElementException if `column` is not a subcomponent of this mapping.
	  * @throws IllegalStateException if any of the component lists have been already exported.
	  */
	protected def rename[T](name :String, column :Column[T]) :Column[T] = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Cannot rename column $column of $this: components have already been exported.")

		initPreceding()

		val current = column match {
			case comp :MappingFrame[_, _]#FrameComponent[_] if comp belongsTo this =>
				throw new IllegalArgumentException(
					s"Cannot rename column $column which is an export component of the enclosing MappingFrame $this."
				)
			case _ =>
				initSelectors.getOrElse(column, 
					throw new IllegalArgumentException(
						s"Cannot rename column $column as it is not a part of mapping $this."
				)).asInstanceOf[Selector[T]]
		}
		val export = current.export
		val extract = Extractor(current.optional, current.requisite) //don't retain the reference to export

		val lists = initComponents::initSubcomponents::initColumns::initSelectable::initQueryable::
			initUpdatable::initInsertable::initAutoInsert::initAutoUpdate::Nil

		val replacement = new ColumnComponent[T](extract, name, export.buffs)(column.form) {
			//we'll replace the export column with this manually
			override def init() :ColumnExtractor[S, T, O] = selectorFor(this) 
		}

		lists foreach { list => //swap the replaced component with the renamed version
			val uninitialized = list.uninitialized
			val i = uninitialized.indexOf(export)
			if (i >= 0)
				uninitialized(i) = replacement
		}

		//we now need to find all versions of the replaced component (as components of some subcomponent)
		//and replace their mapping from export to replacement
		val selector = selectorFor(replacement)
		initSelectors = initSelectors map { lift =>
			if (lift._2.export == export) lift._1 -> selector
			else lift
		}
		//just to be sure in case equals is wonky and to trigger (empty) initialization
		initSelectors = initSelectors.updated(column, replacement.selector) 
		replacement
	}






	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :FrameComponent[T])(implicit pieces :Pieces) :T =
		pieces(component.selector)




	
	
	override def export[T](component :Component[T]) :Component[T] = component match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[Component[T]]
		case _ =>
			apply(component).export
	}

	override def export[T](column :Column[T]) :Column[T] = column match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[Column[T]]
		case _ =>
			apply(column).export
	}

	override def apply[T](component :Component[T]) :Selector[T] = component match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[FrameComponent[T]].selector
		case _  =>
			selectors.getOrElse(component,
				throw new IllegalArgumentException(s"Component $component is not a part of mapping $this.")
			).asInstanceOf[Selector[T]]
	}

	override def apply[T](column :Column[T]) :ColumnSelector[T] = column match {
		case export :MappingFrame[_, _]#FrameColumn[_] if export belongsTo this =>
			export.asInstanceOf[FrameColumn[T]].selector
		case wonky :MappingFrame[_, _]#FrameComponent[_] if wonky belongsTo this => wonky.selector match {
			case selector :ColumnExtractor[S @unchecked, T @unchecked, O @unchecked] => selector
			case selector =>
				throw new IllegalStateException(
					s"Component $column of $this implements FrameComponent but not FrameColumn " +
					s"and its extractor $selector is not a ColumnExtractor."
				)
		}
		case _ =>
			selectors.getOrElse(column,
				throw new IllegalArgumentException(s"Column $column is not a part of mapping $this.")
			).asInstanceOf[ColumnSelector[T]]
	}



	/** An hook point for subclasses to extend the functionality by providing a specialized selector
	  * for the given component, offering additional features. This method is called once for every component
	  * which is not a column (including subcomponents) when the component's initialization is being performed.
	  */
	protected def selectorFor[T](component :FrameComponent[T]) :Selector[T] =
		ComponentExtractor[S, T, O](component)(component.extractor)

	/** A hook point for subclasses to extend the functionality by providing a specialized selector
	  * for the given column, offering additional features. This method is called once for every column
	  * (including export versions of non-direct columns) when the column's initialization is being performed.
	  */
	protected def selectorFor[T](column :FrameColumn[T]) :ColumnSelector[T] =
		ComponentExtractor[S, T, O](column)(column.extractor)

	private[this] var initSelectors = Map[AnyComponent, Selector[_]]()
	@volatile private[this] var safeSelectors :Map[AnyComponent, Selector[_]] = _
	private[this] var fastSelectors :Map[AnyComponent, Selector[_]] = _

	private def selectors :Map[AnyComponent, Selector[_]] = {
		if (fastSelectors == null) {
			val initialized = safeSelectors
			if (initialized != null)
				fastSelectors = initialized
			else {
				initialize()
				fastSelectors = safeSelectors
			}
		}
		fastSelectors
	}



	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override.
	  * Overrides with a `val` or `var` (or any used in their implementation) must happen ''before'' any components
	  * of this mapping are initialized - in practice before any column declarations.
	  */
	protected def columnPrefix :String = ""

	private final def verifiedPrefix :String = columnPrefix match {
		case null => throw new IllegalStateException(
			s"$this.columnPrefix is null: override with a val must happen before any component declarations."
		)
		case prefix => prefix
	}




	/** Adapts the buffs attached to this instance for a subcomponent. The buffs are mapped with the given
	  * value extractor; if the extractor fails to produce a value for the component out of a value of a buff,
	  * an `IllegalArgumentException` will be thrown. Given the lazy nature of some types of buffs, this may
	  * happen not from this method, but when the buff is actually used.
	  * @param extractor extractor of the value for the subcomponent inheriting the values.
	  * @param component a string identifying the component included in error messages for debug purposes.
	  * @return this mapping's list of buffs mapped with the given extractor.
	  * @throws IllegalStateException if `this.buffs` is `null`, which can typically happen if `buffs` is overriden
	  *                               by subclass with a `val` defined after some components are initialized.
	  * @throws IllegalArgumentException if `this.buffs` contains a buff with a value which can't be mapped with
	  *                                  the given extractor, because it failed to produce the value for the target
	  *                                  component.
	  */
	protected def conveyBuffs[T](extractor :Extractor[S, T], component: => String = "") :Seq[Buff[T]] =
		if (buffs == null)
			throw new IllegalStateException(s"$this.buffs is null: overrides must happen before any component declarations.")
		else //todo: move to schema.cascadeBuffs
	        extractor.requisite.map(pick => buffs.flatMap(_.cascade(pick))) getOrElse {
		        val pick = extractor.optional
		        buffs.flatMap { buff =>
			        buff.cascade { s => pick(s) getOrElse {
				        throw new BuffMappingFailureException(
					        s"Can't apply buff $buff of $this to subcomponent '$component': selector function returned no value for $s."
				        )
			        }}
		        }
	        }



	/** Includes a column belonging to this instance on all appropriate column lists declared in `Mapping`. */
	private def includeColumn[T](column :FrameColumn[T]) :FrameColumn[T] = synchronized {
		if (isInitialized)
			throw new IllegalStateException(
				s"Cannot initialize column $column of $this: components have already been exported. Create components eagerly!"
			)

		initSubcomponents += column
		initColumns += column
		if (NoSelect.disabled(column)) initSelectable += column
		if (NoQuery.disabled(column)) initQueryable += column
		if (NoUpdate.disabled(column)) initUpdatable += column
		if (NoInsert.disabled(column)) initInsertable += column
		if (AutoInsert.enabled(column)) initAutoInsert += column
		if (AutoUpdate.enabled(column)) initAutoUpdate += column

		column
	}



	private def delayInit(c :FrameComponent[_]) :Unit = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Cannot initialize mapping component $c on $this: components have already been exported.")
		else
			delayedInits += c
	}

	/** Initializes all already instantiated components of this instance which haven't been yet initialized in the
	  * order of their definition. As components are by default initialized lazily to allow their extension by
	  * subclasses, it is possible that another component registers itself and its subcomponents on the subcomponents
	  * before a component which definition precedes it. While this has no effect on the functionality, it can be
	  * somewhat inconvenient from the debugging perspective. For this reason, all concrete classes declared
	  * by this mapping (or any built-in mappings extending it) initialize all already defined, but not
	  * initialized components before their own initialization. Nothing else is needed to preserve the order
	  * unless a subclass decides to initialize a component manually by its `include()` method. It can then
	  * use this method before the call to `include()` in order to preserve the order of component definition
	  * on all column/component lists.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.initPreceding[T](value:T) initPreceding(value)]]
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.FrameComponent.include() FrameComponent.include()]]
	  */
	protected final def initPreceding() :Unit = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	/** An ease-of-life variant of no-argument `initPreceding()` which returns the given argument after finishing
	  * the initialization of all already defined components. It allows to 'inject' the initialization as an intermediate
	  * step into any expression.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.initPreceding() initPreceding()]]
	  */
	@inline protected final def initPreceding[T](value :T) :T = { initPreceding(); value }

	/** Keeps components which have been created, but not initialized (their columns and components have not been
	  * enlisted with this instance). Modified only under synchronization lock of this instance, its contents
	  * are flushed either when initialization occurs (a call to `initialize()`, either explicit or due to accessing
	  * a column or component list of this instance), or when a new component (typically a column) is created
	  * and needs to be initialized, in order to preserve relative order of declarations in the column/component
	  * lists.
	  */
	private[this] val delayedInits = new ListBuffer[FrameComponent[_]]


	/** Tests if the component lists of this instance have already been initialized and can not be changed any further.
	  * This happens either when any of the column/component lists of this instance are accessed, or when a subclass
	  * manually triggers the initialization by a call to [[net.noresttherein.oldsql.schema.support.MappingFrame.initialize() initialize()]].
	  * Any attempt to create new components after this happens will result in `IllegalStateException` thrown
	  * by the constructor.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.initialize() initialize()]]
	  */
	protected final def isInitialized :Boolean = initComponents.isInitialized

	/** Finalizes the initialization of all already created, but not initialized components of this mapping,
	  * assigning them to their respective component/column lists. The lists become frozen in the process,
	  * making attempts of creation of any additional components in the future fail with an `IllegalStateException`.
	  * As the last step of this process `finalizeInitialization()` is called which allows subclasses to perform
	  * any additional initialization required by its introduced features.
	  * It is important to remember that singleton object definitions in scala are initialized lazily, meaning their
	  * constructors are being called only when the object is being used for the first time. This can make any
	  * components of this mapping defined as singleton objects invisible to this method, resulting in an invalid
	  * instance. For this reason components should be instead created as standard `val`s or, at least, initialized
	  * either by their `include()` method or by referencing any of their members before this method is called.
	  * It is triggered automatically the first time any of the component lists of this instance is accessed, but
	  * can be also called explicitly by a subclass. It is idempotent and synchronized, meaning any calls after
	  * the first one are ignored.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.finalizeInitialization()]]
	  */
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
				initSubcomponents.initialize()

				fastSelectors = initSelectors
				safeSelectors = fastSelectors
				initSelectors = null

				finalizeInitialization()
			}
		}

	/** A hook method called by `initialize()` after component lists are initialized. By default it does nothing,
	  * but subclasses may override it to implement any additional initialization steps they require.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame.initialize() initialize()]]
	  */
	protected def finalizeInitialization() :Unit = ()



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
	private[this] class LateInitComponents[C <: FrameComponent[_]](var uninitialized :ListBuffer[C])
		extends AbstractSeq[C]
	{
		def this() = this(ListBuffer.empty)

		@volatile
		private[this] var initialized :Unique[C] = _
		private[this] var cached :Unique[C] = _

		def +=(comp :C) :Unit =
			if (isInitialized)
				throw new IllegalStateException(
					s"Can't enlist component $comp for $composite: the list of components has been already exported."
				)
			else
				uninitialized += comp

		def isInitialized :Boolean = initialized != null

		def initialize() :Unit = {
			cached = uninitialized.result.toUnique
			initialized = cached
			uninitialized = null
		}

		def items :Unique[C] =
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

		override def apply(idx :Int) :C = items(idx)

		override def foreach[U](f :C => U) :Unit = items.foreach(f)

		override def iterator :Iterator[C] = items.iterator

		override def indexOf[B >: C](elem :B, start :Int) :Int = items.indexOf(elem, start)

		override def lastIndexOf[B >: C](elem :B, end :Int) :Int = items.lastIndexOf(elem, end)

	}



	private[this] final val initComponents = new LateInitComponents[FrameComponent[_]]
	private[this] final val initSubcomponents = new LateInitComponents[FrameComponent[_]]
	private[this] final val initColumns = new LateInitComponents[FrameColumn[_]]
	private[this] final val initSelectable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initQueryable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initUpdatable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initAutoUpdate = new LateInitComponents[FrameColumn[_]]
	private[this] final val initInsertable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initAutoInsert = new LateInitComponents[FrameColumn[_]]

	final override def components :Unique[Component[_]] = initComponents.items
	final override def subcomponents :Unique[Component[_]] = initSubcomponents.items
	final override def columns :Unique[Column[_]] = initColumns.items
	final override def selectable :Unique[Column[_]] = initSelectable.items
	final override def queryable :Unique[Column[_]] = initQueryable.items
	final override def updatable :Unique[Column[_]] = initUpdatable.items
	final override def autoUpdated :Unique[Column[_]] = initAutoUpdate.items
	final override def insertable :Unique[Column[_]] = initInsertable.items
	final override def autoInserted :Unique[Column[_]] = initAutoInsert.items

	override val selectForm :SQLReadForm[S] = SQLReadForm.Lazy(super.selectForm)
	override val queryForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.queryForm)
	override val insertForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.insertForm)
	override val updateForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.updateForm)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X] = null) :this.type AdaptedAs X =
		MappedMapping[this.type, S, X, O](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X] = null) :this.type AdaptedAs X =
		MappedMapping.opt[this.type, S, X, O](this, there, back)


	override def qualified(prefix :String) :Adapted[this.type] = PrefixedMapping.qualified(prefix, this)

	override def prefixed(prefix :String) :Adapted[this.type] = PrefixedMapping(prefix, this)

	override def renamed(name :String) :Adapted[this.type] = RenamedMapping.shallow(name, this)
}






/** A `MappingFrame` extension which, in its `optionally` (and indirectly `apply`) method, declares aliasing
  * of its components on the passed `Pieces`. As `MappingFrame` (and possibly any of its components) allows
  * declaring a column prefix to be added to all its columns as well as additional buffs which should be inherited
  * by all of its subcomponents (including columns), the component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. This is in particular necessary if a
  * component not extending the inner [[net.noresttherein.oldsql.schema.support.MappingFrame.FrameComponent FrameComponent]]
  * class is embedded in the mapping. As it is the latter version of the component which is used by the framework
  * to create any SQL statements, and thus also by the `Pieces`, but typically the original component as defined
  * is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces` implementation
  * substitutes any component passed to it with its export representation before looking for its value.
  */
trait RootMappingFrame[S, O] extends MappingFrame[S, O] with RootMapping[S, O]

