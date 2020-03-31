package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Unique.implicitUnique
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema.Buff.{AutoGen, AutoInsert, AutoUpdate, NoInsert, NoQuery, NoSelect, NoUpdate, Unmapped}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentExtractor, MappingReadForm, MappingWriteForm, TypedMapping}
import net.noresttherein.oldsql.schema.support.ComponentProxy.{EagerDeepProxy, ShallowProxy}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, AdaptedAs}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.{MappedMapping, PrefixedMapping}
import net.noresttherein.oldsql.schema.support.RenamedMapping.DeeplyRenamedMapping
import net.noresttherein.oldsql.slang._

import scala.collection.AbstractSeq
import scala.collection.mutable.{Builder, ListBuffer}
import scala.reflect.runtime.universe.TypeTag




//todo: name DeclarativeMapping ?
//todo: provide examples
/** Convenience base trait for mappings which are defined statically, rather than dynamically (i.e their mapped type
  * and schema are known). This includes table mappings and other instances where columns are known statically and
  * should be created manually, rather than by some generic code. While it exposes mutator methods to facilitate
  * creation and declaration of components, it is expected that they'll be used solely in the constructors of derived
  * classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called
  * previously, an exception will be thrown.
  *
  * It exists exclusively as a way to facilitate creating schema descriptions and it's interface is directed
  * towards implementing classes; no other place in the system handles it in any special way, different from other
  * implementations of `Mapping`.
  *
  * @tparam S value type of the mapped entity
  */
trait MappingSupport[O, S] extends StaticMapping[O, S] { composite =>

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
	trait ComponentMapping[T] extends GenericMapping[O, T] { self =>

		def ?(implicit values :composite.Pieces) :Option[T] = values.get(selector)



		private[MappingSupport] def belongsTo(mapping :MappingSupport[_, _]) :Boolean = mapping eq composite

		protected[schema] def extractor :Extractor[S, T]

		def selector :ComponentExtractor[O, S, T] = {
			if (fastSelector == null) {
				val s = safeSelector
				if (s != null) fastSelector = s
				else include()
			}
			fastSelector
		}

		@volatile
		private[this] var safeSelector :ComponentExtractor[O, S, T] = _
		private[this] var fastSelector :ComponentExtractor[O, S, T] = _

		private[this] var initialized = false

		/** Manually trigger initialization of this instance within the enclosing mapping. */
		private[MappingSupport] final def include() :this.type = synchronized {
			if (!initialized) {
				fastSelector = selectorFor(this)
				safeSelector = fastSelector
				init()
				initialized = true
			}
			this
		}

		/** Register itself and all its subcomponents within the parent mapping. This method will be called only once. */
		protected[MappingSupport] def init() :Unit = composite.synchronized {
			initComponents += this
			initSubcomponents += this
			columns foreach { liftSubcomponent(_, true) }
			subcomponents foreach { c => liftSubcomponent(c, false) } //or should it be components?
		}



		protected[MappingSupport] def liftSubcomponent[U](subcomponent :Component[U], column :Boolean) :ComponentMapping[U] =
			subcomponent match {
				case mine :MappingSupport[_, _]#ComponentMapping[_] if mine belongsTo composite =>
					mine.asInstanceOf[ComponentMapping[U]]

				case _ => composite.synchronized {
					initSelectors.getOrElse(subcomponent, {
						val extractsub = apply(subcomponent)
						val subextractor = extractor andThen extractsub

						if (columnPrefix == null)
							throw new IllegalStateException(
								s"$this.columnPrefix is null: overrides with a val must happen before any component declarations!")
						if (buffs == null)
							throw new IllegalStateException(
								s"$this.buffs is null: overrides with a val must happen before any component declarations!")

						val subbuffs = subcomponent.buffs :++
							extractsub.requisite.mapOrElse(
								pick => buffs.map(_.map(pick)),
								buffs.map(
									buff => buff.map((t :T) => extractsub.get(t) getOrElse {
										throw new IllegalArgumentException(
											s"Can't apply buff $buff for subcomponent $subcomponent of $composite/$self: no value for $t."
										)
									})
								)
							)

						val lifted = subcomponent match {
							case column :ColumnMapping[_, _] =>
								new ColumnComponent[U](
									subextractor.optional, subextractor.requisite, columnPrefix + column.name, subbuffs
								)(column.form)

							case _ if column =>
								if (subcomponent.sqlName.isEmpty)
									throw new IllegalArgumentException(
										s"Subomponent $subcomponent on the columns list of component $this of mapping " +
											s"$composite does not have an sqlName."
									)
								val name = columnPrefix + subcomponent.sqlName.get
								new LiftedColumn[U](subcomponent, subextractor, name, subbuffs)

							case _ =>
								new LiftedComponent[U](subcomponent, subextractor, columnPrefix, subbuffs)
						}
						initSelectors = initSelectors.updated(subcomponent, lifted.selector)
						lifted.selector
					}).lifted.asInstanceOf[ComponentMapping[U]]
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

		/** The column prefix prepended to all columns at the behest of the enclosing `MappingSupport` or an empty string. */
		protected final def inheritedPrefix :String = testedPrefix

		/** The column prefix prepended to all columns by this component ''and'' the enclosing `MappingSupport`.
		  * Defaults to `inheritedPrefix`. */
		protected def columnPrefix :String = inheritedPrefix


		//enlist ourselves on the list of uninitialized components of the enclosing mapping
		delayInit(this)

	}



	/** A proxy class for non-direct subcomponents of the enclosing `MappingSupport` which incorporates any column prefix
	  * and buffs defined in it and any other enclosing components into the adapted component.
	  * It serves at the same time as its own `ComponentExtractor` and the lifted 'effective' version of the component.
	  * Its components are at the same time the lifted components of the enclosing mapping.
	  */
	private class LiftedComponent[T](target :Component[T], val extractor :Extractor[S, T],
	                                 override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[Component[T], O, T](target) with ComponentMapping[T]
	{
		def adapted :Component[T] = egg

		protected[MappingSupport] override def init() :Unit = composite.synchronized {
			initSubcomponents += this //don't lift subcomponents, don't include in the composite.components list.
		}

		override def canEqual(that :Any) :Boolean = that match {
			case comp :MappingSupport[_, _]#LiftedComponent[_] => comp belongsTo composite
			case _ => false
		}

		override protected def adapt[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component, false)

		override protected def adaptColumn[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component, true)

		override def toString :String = "^" + egg + "^"

	}



	private class LiftedColumn[T](target :Component[T], extractor :Extractor[S, T], name :String, buffs :Seq[Buff[T]])
		extends LiftedComponent[T](target, extractor, name, buffs)
	{
		protected[MappingSupport] override def init() :Unit = composite.include(this)
	}



	/** Base trait for components which have a value for all instances of `T`. */
	trait MandatoryComponent[T] extends ComponentMapping[T] {
		protected[schema] override def extractor :RequisiteExtractor[S, T] = Extractor.req(extract)
		protected def extract :S => T
	}

	/** Base trait for components which might not have a value for all instances of `T` (such as properties declared
	  * by some subclass of `T`. This is independent from the
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalSelect ]]
	  */
	trait OptionalComponent[T] extends ComponentMapping[T] {
		protected def extract :S => Option[T]
		protected[schema] override def extractor = Extractor(extract)
	}

	/** Convenience base component class which initializes the extractor using the constructor argument getter.
	  * @param value extractor function for the value of this component.
	  * @param opts buffs specific to this component. This list will be extended with buffs inherited
	  *             from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *             to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.BaseOptionalComponent]]
	  */
	abstract class BaseComponent[T](value :S => T, opts :Buff[T]*) extends ComponentMapping[T] {
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
	  *               the lifted proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *               of this component will be the concatenation of the column prefix defined by the enclosing
	  *               mapping and this prefix.
	  * @param opts   buffs specific to this component. This list will be extended with buffs inherited
	  *               from the enclosing schema. The lifted versions of all subcomponents of this component will
	  *               inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class ComponentSchema[T](value :S => T, prefix :String, opts :Seq[Buff[T]] = Nil)
		extends BaseComponent[T](value, opts :_*) with MappingSupport[O, T]
	{
		def this(value :S => T, buffs :Buff[T]*) = this(value, "", buffs)

		final override val columnPrefix = composite.testedPrefix + prefix
	}



	/** Convenience base class for components which may not have a value for some instances of the enclosing mapping's
	  * subject type `S`.
	  * @param value extractor function for the value of this component.
	  * @param opts buffs specific to this component. This list will be extended with buffs inherited
	  *             from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *             to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.BaseComponent]]
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
	  *               the lifted proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *               of this component will be the concatenation of the column prefix defined by the enclosing
	  *               mapping and this prefix.
	  * @param opts   buffs specific to this component. This list will be extended with buffs inherited
	  *               from the enclosing schema. The lifted versions of all subcomponents of this component will
	  *               inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class OptionalComponentSchema[T](value :S => Option[T], prefix :String, opts :Seq[Buff[T]] = Nil)
		extends BaseOptionalComponent[T](value, opts :_*) with MappingSupport[O, T]
	{
		def this(value :S => Option[T], buffs :Buff[T]*) = this(value, "", buffs)

		override final val columnPrefix = composite.testedPrefix + prefix
	}



/*
	class SymLinkComponent[C<:Mapping[T], T](target :TypedComponentPath[this.type, C, T]) extends BaseSymLink[this.type, C, T](target) with Component[T] {
		override protected[MappingSupport] val pick: (S) => Option[T] = target.pick
		override protected[MappingSupport] val surepick: Option[(S) => T] = target.surepick
		override private[MappingSupport] def init() = Path(comp => target.lift(comp.adaptedComponent))
	}
*/



	/** An adapter allowing embedding of any other component, regardless of the `O` type, as part of this mapping.
	  * The embedded component is not exposed to the outside. The components of the embedded mapping are lifted
	  * to components of the enclosing mapping by incorporating the `columnPrefix` and `buffs` given here (including
	  * those inherited from the enclosing mapping). At the same time, they form the components of this instance exposed
	  * in its component and column lists.
	  * @param body the embedded component.
	  * @param extractor the extractor for the value of this component.
	  * @param columnPrefix the prefix string prepended to the names of all (lifted) columns of `body`.
	  *                     The value of `columnPrefix` of the enclosing mapping is ''not'' prepended automatically
	  *                     to this value - it should be done by the caller if required.
	  * @param buffs the buffs attached to this component, inherited by all of its lifted subcomponents. The buffs
	  *              of the enclosing mapping are ''not'' automatically added to this list - it should be done
	  *              by the caller if required.
	  * @tparam T the value type of this component.
	  */
	final class EmbeddedComponent[T] private[MappingSupport]
	                             (val body :TypedMapping[T], protected[schema] val extractor :Extractor[S, T],
	                              override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[TypedMapping[T], O, T](body) with ComponentMapping[T]
	{ nest =>

		protected[MappingSupport] override def init() :Unit = composite.synchronized {
			initComponents += this
			initSubcomponents += this //don't lift subcomponents as its done by EagerDeepProxy
		}

		protected override def adapt[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component.asInstanceOf[Component[X]], false)

		protected override def adaptColumn[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component.asInstanceOf[Component[X]], true)


		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def toString :String = "{{" + body + "}}"

	}






	/** Base trait for column components declared directly under this mapping. In general, there should be little need
	  * to descend your classes directly from it, as column functionality is limited and constructor methods provided
	  * within `MappingSupport` should be sufficient. As with the base `ComponentMapping` trait, simple creation of
	  * this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options (buffs).
	  * @tparam T value type of this column.
	  */
//	trait Column[T] extends ColumnMapping[O, T] with ComponentMapping[T]



	/** A column declared directly under this mapping.
	  * It automatically inherits the enclosing mapping's `columnPrefix` and buffs.
	  */
	private class DirectColumn[T :ColumnForm](override val extractor :S =?> T, name :String, opts :Seq[Buff[T]])
		extends StandardColumn[O, T](testedPrefix + name, opts ++ conveyBuffs(extractor, testedPrefix + name))
		   with ComponentMapping[T]
	{
		def this(value :S => T, name :String, opts :Seq[Buff[T]]) =
			this(Extractor.req(value), name, opts)

		override val buffs :Seq[Buff[T]] = opts :++ inheritedBuffs

		protected[MappingSupport] override def init() :Unit = {
			initComponents += this
			composite.include(this)
		}
	}



	/** A lower-level column component which accepts values for all abstract fields and initializes them verbatim.
	  * This column will ''not'' automatically inherit the enclosing mapping's `columnPrefix` or buffs.
	  * It is not included on the direct components list of this mapping.
	  */
	private class ColumnComponent[T](val extractor :Extractor[S, T], name :String, override val buffs :Seq[Buff[T]])
	                                (implicit sqlForm :ColumnForm[T])
		extends StandardColumn[O, T](name, buffs) with ComponentMapping[T]
	{
		def this(value :S => T, name :String, buffs :Seq[Buff[T]])(implicit form :ColumnForm[T]) =
			this(Extractor.req(value), name, buffs)

		def this(pick :S => Option[T], surepick :Option[S=>T], name :String, buffs :Seq[Buff[T]])
		        (implicit form :ColumnForm[T]) =
			this(surepick map Extractor.req[S, T] getOrElse Extractor(pick), name, buffs)

		protected[MappingSupport] override def init() :Unit = composite.include(this)
	}


	/** Embeds any component under this instance. This method creates a component exposing the wrapped `component`
	  * through its `body` property as `component.type`. The wrapper component has a single 'phantom' subcomponent,
	  * being the passed component; each subcomponent of `component` receives a lifted version exposed as a subcomponent
	  * of the wrapper on its `subcomponents` list (and, for columns, all applicable column lists).
 	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the created component. This list is expanded with any buffs inherited from
	  *              this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return a wrapper of the given mapping, automatically registered on this mapping's `components` list.
	  */
	protected def embed[T](component :TypedMapping[T], value :S => T, columnPrefix :String, buffs :Seq[Buff[T]] = Nil)
			:Component[T] =
	{
		val extractor = Extractor.req(value)
		val allBuffs = buffs ++: conveyBuffs(extractor, component.toString)
		initPreceding(new EmbeddedComponent(component :component.type, extractor, testedPrefix + columnPrefix, allBuffs))
	}

	/** Embeds any component under this instance. This is equivalent to `embed(component, value, "", buffs)` - see
	  * that method's description for more information.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param buffs buffs to attach to the created component. This list is expanded with any buffs inherited from
	  *              this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return a wrapper of the given mapping, automatically registered on this mapping's `components` list.
	  */
	protected def embed[T](component :TypedMapping[T], value :S => T, buffs :Buff[T]*) :Component[T] =
		embed(component, value, "", buffs)






	/** A builder adapting a given column template to a column of this `MappingSupport`. */
	protected class ColumnCopist[T] private[MappingSupport](template :ColumnMapping[_, T], name :Option[String], buffs :Seq[Buff[T]]) {
		private[MappingSupport] def this(source :ColumnMapping[_, T]) = this(source, None, source.buffs)

		template match {
			case self :MappingSupport[_, _]#ComponentMapping[_] if self.belongsTo(composite) =>
				if (buffs == null)
					throw new IllegalStateException(s"$this.buffs is null: overrides must be defined before any components.")
				if (testedPrefix.length > 0 || buffs.nonEmpty )
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
			val column = new ColumnComponent[T](getter, testedPrefix + (name getOrElse template.name), buffs)(template.form)
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
			val column = new ColumnComponent[T](getter, None, testedPrefix + (name getOrElse template.name), buffs)(template.form)
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
	protected def column[T](template :ColumnMapping[_, T]) :ColumnCopist[T] =
		new ColumnCopist(template)

	/** Create a column based on the given column, giving it a new name.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[_, T], name :String) :ColumnCopist[T] =
		new ColumnCopist[T](template, Some(name), template.buffs)

	/** Create a column based on the given column, replacing its buff list with the provided buffs.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[_, T], buffs :Seq[Buff[T]]) :ColumnCopist[T] =
		new ColumnCopist(template, None, buffs)

	/** Create a column based on the given column, replacing its name and buff list with the provided values.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[_, T], name :String, buffs :Seq[Buff[T]]) :ColumnCopist[T] =
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
	  * @param buffs the buffs to attach to the created column. This list will be prepended with `Buff.Unmapped[T]`.
	  * @return a new column, enlisted on the `columns`, `components` and `subcomponents` property of this mapping
	  *         and none other.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Unmapped]]
	  */
	protected def unmapped[T :ColumnForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new ColumnComponent[T](_ => None, None, name, Unmapped[T] +: buffs))



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






	private class RenamedColumn[X](name :String, comp :Component[X],
	                               override val extractor :S =?> X, override val buffs :Seq[Buff[X]])
		extends EagerDeepProxy[Component[X], O, X](comp) with ComponentMapping[X]
	{
		protected override def adapt[V](component :egg.Component[V]) :Component[V] =
			new RenamedColumn[V](name :String, component, extractor andThen egg(component), component.buffs)

		override def init() :Unit = ()
	}



	/** Renames a column of this mapping. The column might be either directly declared by this mapping or
	  * any of its subcomponents preceding this call. The new column will have the buffs of its lifted version,
	  * as would be returned by `apply(column)`. It is not required that `column` is an instance of
	  * `ColumnMapping` or that it upholds its invariants: only that it declares exactly one column
	  * and has non empty `sqlName`. This means that adapted/mapped columns are accepted.
	  * @throws IllegalArgumentException if `column` is not a column (declares more or less than one column).
	  * @throws NoSuchElementException if `column` is not a subcomponent of this mapping.
	  * @throws IllegalStateException if any of the component lists have been already exported.
	  */
	protected def rename[T](name :String, column :Component[T]) :Component[T] = synchronized {
		if (column.sqlName.isEmpty)
			throw new IllegalArgumentException(s"Cannot rename component $column of $this as it does not have sqlName.")
		if (column.columns.size != 1)
			throw new IllegalArgumentException(s"Cannot rename component $column of $this as it is not a column.")
		if (initSelectors == null)
			throw new IllegalStateException(s"Cannot rename column $column of $this: components have already been exported.")

		initPreceding()

		val current = initSelectors.getOrElse(column, throw new NoSuchElementException(
			s"Cannot rename column $column as it is not a part of mapping $this"
		)).asInstanceOf[Selector[T]]
		val lifted = current.lifted
		val extract = Extractor(current.optional, current.requisite) //don't retain the reference to lifted

		val lists = initComponents::initSubcomponents::initColumns::initSelectable::initQueryable::
			initUpdatable::initInsertable::initAutoInsert::initAutoUpdate::Nil

		val replacement = column match {
			case col :ColumnMapping[O, T] =>
				val replacement = new ColumnComponent[T](extract, name, lifted.buffs)(col.form) {
					override def init() :Unit = () //we'll replace the lifted column with this manually
				}
				lists foreach { list => //swap the replaced component with the renamed version
					val uninitialized = list.uninitialized
					val i = uninitialized.indexOf(lifted)
					if (i >= 0)
						uninitialized(i) = replacement
				}
				replacement

			case _ =>
				val adapted = lifted match {
					case lift :MappingSupport[_, _]#LiftedComponent[_] => lift.adapted.asInstanceOf[Component[T]]
					case _ => lifted //wtf???
				}

				val replacement = new RenamedColumn(name, adapted, extract, lifted.buffs)

				def includeColumn(list :LateInitComponents, col :Component[_]) :Unit = {
					val uninitialized = list.uninitialized
					val i = uninitialized.indexOf(lifted)
					if (i >= 0)
						uninitialized.insert(i + 1, col.asInstanceOf[ComponentMapping[_]])
					else
						uninitialized += col.asInstanceOf[ComponentMapping[_]]
				}


				replacement.columns foreach { c =>
					includeColumn(initColumns, column)
					if (NoSelect.disabled(column)) includeColumn(initSelectable, column)
					if (NoQuery.disabled(column)) includeColumn(initQueryable, column)
					if (NoUpdate.disabled(column)) includeColumn(initUpdatable, column)
					if (NoInsert.disabled(column)) includeColumn(initInsertable, column)
					if (AutoInsert.enabled(column)) includeColumn(initAutoInsert, column)
					if (AutoUpdate.enabled(column)) includeColumn(initAutoUpdate, column)
				}

				val subs = initSubcomponents.uninitialized
				var i = subs.indexOf(lifted)
				assert(i >= 0, s"Cannot rename column $column (from $lifted) of mapping $this as it is not on the subcomponents list")

				subs(i) = replacement
				replacement.subcomponents.foreach { c =>
					i += 1; subs.insert(i, c.asInstanceOf[ComponentMapping[_]])
				}

				i = initComponents.uninitialized.indexOf(lifted)
				if (i >= 0)
					initComponents.uninitialized(i) = replacement
				replacement
		}

		val old = lifted.subcomponents
		lists foreach { list =>
			list.uninitialized.filterInPlace(!old.contains(_))
		}

		//we now need to find all versions of the replaced component (as components of some subcomponent)
		//and replace their mapping from lifted to replacement
		val selector = selectorFor(replacement)
		initSelectors = initSelectors map { lift =>
			if (lift._2.lifted == lifted) lift._1 -> selector
			else lift
		}

		replacement
	}






	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :ComponentMapping[T])(implicit pieces :Pieces) :T =
		pieces(component.selector)




	override def lift[T](component :Component[T]) :Component[T] = component match {
		case lifted :MappingSupport[_, _]#ComponentMapping[_] if lifted belongsTo this =>
			lifted.asInstanceOf[Component[T]]
		case _ =>
			apply(component).lifted
	}

	override def apply[T](component :Component[T]) :Selector[T] = component match {
		case lifted :MappingSupport[_, _]#ComponentMapping[_] if lifted belongsTo this =>
			lifted.asInstanceOf[ComponentMapping[T]].selector
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
			fastSelectors.getOrElse(component, throw new IllegalArgumentException(
				s"Component $component is not a part of mapping $this."
			)).asInstanceOf[Selector[T]]
	}



	/** An extension point for subclasses to extend the functionality by providing a specialized selector
	  * for the given component, offering additional features. This method is called once for every component
	  * (including subcomponents),  when the component's initialization is being
	  */
	protected def selectorFor[T](component :ComponentMapping[T]) :Selector[T] =
		ComponentExtractor[O, S, T](component)(component.extractor)

	private[this] var initSelectors = Map[AnyComponent, Selector[_]]()
	@volatile private[this] var selectors :Map[AnyComponent, Selector[_]] = _
	private[this] var fastSelectors :Map[AnyComponent, Selector[_]] = _



	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override.
	  * Overrides with a `val` or `var` (or any used in their implementation) must happen ''before'' any components
	  * of this mapping are initialized - in practice before any column declarations.
	  */
	protected def columnPrefix :String = ""

	private final def testedPrefix :String = columnPrefix match {
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
	protected def conveyBuffs[T](extractor :Extractor[S, T], component :String = "") :Seq[Buff[T]] =
		if (buffs == null)
			throw new IllegalStateException(s"$this.buffs is null: overrides must happen before any component declarations.")
		else
	        extractor.requisite.map(pick => buffs.map(_.map(pick))) getOrElse {
		        val pick = extractor.optional
		        buffs.map { buff =>
			        buff.map{ s => pick(s) getOrElse {
				        throw new IllegalArgumentException(
					        s"Can't apply buff $buff of $this to subcomponent '$component': selector function returned no value for $s."
				        )
			        }}
		        }
	        }



	/** Includes a column belonging to this instance on all appropriate column lists declared in `Mapping`. */
	private def include[T](column :ComponentMapping[T]) :ComponentMapping[T] = synchronized {
		if (isInitialized)
			throw new IllegalStateException(
				s"Cannot initialize column $column of $this: components have already been exported. Create components eagerly!"
			)
		if (column.columns.size != 1 || column.sqlName.isEmpty)
			throw new IllegalStateException(
				s"Attempted to include $column as a column of $this: included component's columns: ${column.columns}; sqlName: ${column.sqlName}"
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



	private def delayInit(c :ComponentMapping[_]) :Unit = synchronized {
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
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.initPreceding[T](value:T) initPreceding(value)]]
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.ComponentMapping.include() ComponentMapping.include()]]
	  */
	protected final def initPreceding() :Unit = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	/** An ease-of-life variant of no-argument `initPreceding()` which returns the given argument after finishing
	  * the initialization of all already defined components. It allows to 'inject' the initialization as an intermediate
	  * step into any expression.
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.initPreceding() initPreceding()]]
	  */
	@inline protected final def initPreceding[T](value :T) :T = { initPreceding(); value }

	/** Keeps components which have been created, but not initialized (their columns and components have not been
	  * enlisted with this instance). Modified only under synchronization lock of this instance, its contents
	  * are flushed either when initialization occurs (a call to `initialize()`, either explicit or due to accessing
	  * a column or component list of this instance), or when a new component (typically a column) is created
	  * and needs to be initialized, in order to preserve relative order of declarations in the column/component
	  * lists.
	  */
	private[this] val delayedInits = new ListBuffer[ComponentMapping[_]]


	/** Tests if the component lists of this instance have already been initialized and can not be changed any further.
	  * This happens either when any of the column/component lists of this instance are accessed, or when a subclass
	  * manually triggers the initialization by a call to [[net.noresttherein.oldsql.schema.MappingSupport.initialize() initialize()]].
	  * Any attempt to create new components after this happens will result in `IllegalStateException` thrown
	  * by the constructor.
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.initialize() initialize()]]
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
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.finalizeInitialization()]]
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
				selectors = fastSelectors
				initSelectors = null

				finalizeInitialization()
			}
		}

	/** A hook method called by `initialize()` after component lists are initialized. By default it does nothing,
	  * but subclasses may override it to implement any additional initialization steps they require.
	  * @see [[net.noresttherein.oldsql.schema.MappingSupport.initialize() initialize()]]
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
	private[this] class LateInitComponents(var uninitialized :ListBuffer[ComponentMapping[_]])
		extends AbstractSeq[ComponentMapping[_]]
	{
		def this() = this(ListBuffer.empty)

		@volatile
		private[this] var initialized :Unique[ComponentMapping[_]] = _
		private[this] var cached :Unique[ComponentMapping[_]] = _

		def +=(comp :ComponentMapping[_]) :Unit =
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

		def items :Unique[ComponentMapping[_]] =
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

		override def indexOf[B >: ComponentMapping[_]](elem :B, start :Int) :Int = items.indexOf(elem, start)

		override def lastIndexOf[B >: ComponentMapping[_]](elem :B, end :Int) :Int = items.lastIndexOf(elem, end)

	}



	private[this] final val initComponents = new LateInitComponents
	private[this] final val initSubcomponents = new LateInitComponents
	private[this] final val initColumns = new LateInitComponents
	private[this] final val initSelectable = new LateInitComponents
	private[this] final val initQueryable = new LateInitComponents
	private[this] final val initUpdatable = new LateInitComponents
	private[this] final val initAutoUpdate = new LateInitComponents
	private[this] final val initInsertable = new LateInitComponents
	private[this] final val initAutoInsert = new LateInitComponents

	final override def components :Unique[Component[_]] = initComponents.items
	final override def subcomponents :Unique[Component[_]] = initSubcomponents.items
	final override def columns :Unique[Component[_]] = initColumns.items
	final override def selectable :Unique[Component[_]] = initSelectable.items
	final override def queryable :Unique[Component[_]] = initQueryable.items
	final override def updatable :Unique[Component[_]] = initUpdatable.items
	final override def autoUpdated :Unique[Component[_]] = initAutoUpdate.items
	final override def insertable :Unique[Component[_]] = initInsertable.items
	final override def autoInserted :Unique[Component[_]] = initAutoInsert.items

	override val selectForm :SQLReadForm[S] = SQLReadForm.Lazy(super.selectForm)
	override val queryForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.queryForm)
	override val insertForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.insertForm)
	override val updateForm :SQLWriteForm[S] = SQLWriteForm.Lazy(super.updateForm)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X] = null) :this.type AdaptedAs X =
		MappedMapping[this.type, O, S, X](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X] = null) :this.type AdaptedAs X =
		MappedMapping.opt[this.type, O, S, X](this, there, back)


	override def qualified(prefix :String) :Adapted[this.type] = PrefixedMapping.qualified(prefix, this)

	override def prefixed(prefix :String) :Adapted[this.type] = PrefixedMapping(prefix, this)

}






/** A `MappingSupport` extension which, in its `optionally` (and indirectly `apply`) method, declares aliasing
  * of its components on the passed `Pieces`. As `MappingSupport` (and possibly any of its components) allows
  * declaring a column prefix to be added to all its columns as well as additional buffs which should be inherited
  * by all of its subcomponents (including columns), the component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. This is in particular necessary if a
  * component not extending the inner [[net.noresttherein.oldsql.schema.MappingSupport.ComponentMapping ComponentMapping]]
  * class is embedded in the mapping. As it is the latter version of the component which is used by the framework
  * to create any SQL statements, and thus also by the `Pieces`, but typically the original component as defined
  * is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces` implementation
  * substitutes any component passed to it with its lifted representation before looking for its value.
  */
trait RootMappingSupport[O, S] extends MappingSupport[O, S] with RootMapping[O, S]


