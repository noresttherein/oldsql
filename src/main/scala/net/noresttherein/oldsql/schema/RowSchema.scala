package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.collection.Unique.implicitUnique
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.RequisiteExtractor
import net.noresttherein.oldsql.schema.Buff.{AutoGen, AutoInsert, AutoUpdate, NoInsert, NoQuery, NoSelect, NoUpdate, Unmapped}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentFor, ComponentExtractor, MappingReadForm, MappingWriteForm}
import net.noresttherein.oldsql.schema.support.ComponentProxy.{EagerDeepProxy, ShallowProxy}
import net.noresttherein.oldsql.slang._

import scala.collection.AbstractSeq
import scala.collection.mutable.{Builder, ListBuffer}
import scala.reflect.runtime.universe.TypeTag





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
trait RowSchema[O, S] extends AbstractMapping[O, S] { composite =>

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
	trait ComponentMapping[T] extends AbstractMapping[Owner, T] { self =>

		def ?(implicit values :composite.Pieces) :Option[T] = values.get(selector)



		private[RowSchema] def belongsTo(mapping :RowSchema[_, _]) :Boolean = mapping eq composite

		protected[schema] def extractor :Extractor[S, T]

		def selector :ComponentExtractor[Owner, S, T] = {
			if (fastSelector == null) {
				val s = safeSelector
				if (s != null) fastSelector = s
				else include()
			}
			fastSelector
		}

		@volatile
		private[this] var safeSelector :ComponentExtractor[Owner, S, T] = _
		private[this] var fastSelector :ComponentExtractor[Owner, S, T] = _

		private[this] var initialized = false

		/** Manually trigger initialization of this instance within the enclosing mapping. */
		private[RowSchema] final def include() :this.type = synchronized {
			if (!initialized) {
				fastSelector = selectorFor(this)
				safeSelector = fastSelector
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

		protected[RowSchema] def liftSubcomponent[U](subcomponent :Component[U]) :Component[U] = subcomponent match {
			case mine :RowSchema[_, _]#ComponentMapping[_] if mine belongsTo composite => subcomponent

			case _ if isSymLink(subcomponent) => subcomponent

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
						case column :ColumnMapping[_, U] =>
							new ColumnComponent[U](
								subextractor.optional, subextractor.requisite, columnPrefix + column.name, subbuffs
							)(column.form)
						case _ =>
							new LiftedComponent[U](subcomponent, subextractor, columnPrefix, subbuffs)
					}
					initSelectors = initSelectors.updated(subcomponent, lifted.selector)
					lifted.selector
				}).lifted.asInstanceOf[Component[U]]
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

		/** The column prefix prepended to all columns at the behest of the enclosing `RowSchema` or an empty string. */
		protected final def inheritedPrefix :String = testedPrefix

		/** The column prefix prepended to all columns by this component ''and'' the enclosing `RowSchema`.
		  * Defaults to `inheritedPrefix`. */
		protected def columnPrefix :String = inheritedPrefix


		//enlist ourselves on the list of uninitialized components of the enclosing mapping
		delayInit(this)

	}



	/** A proxy class for non-direct subcomponents of the enclosing `RowSchema` which incorporates any column prefix
	  * and buffs defined in it and any other enclosing components into the adapted component.
	  * It serves at the same time as its own `ComponentExtractor` and the lifted 'effective' version of the component.
	  * Its components * are at the same time the lifted components of the enclosing mapping.
	  */
	private class LiftedComponent[T](target :Component[T], val extractor :Extractor[S, T],
	                                 override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[Component[T], Owner, T](target) with ComponentMapping[T]
	{
		protected[RowSchema] override def init() :Unit = composite.synchronized {
			if (!isSymLink(this)) { //don't lift subcomponents as its done in `adapt` by EagerDeepProxy
				initSubcomponents += this        //don't include ourselves on the composite.components list.
			}
		}

		override def canEqual(that :Any) :Boolean = that match {
			case comp :RowSchema[_, _]#LiftedComponent[_] => comp belongsTo composite
			case _ => false
		}

		override protected def adapt[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component)

		override def toString :String = "^" + egg + "^"
	}



	/** Base trait for components which have a value for all instances of `T`. */
	trait MandatoryComponent[T] extends ComponentMapping[T] {
		protected[schema] override def extractor :RequisiteExtractor[S, T] = Extractor.req(extract)
		protected def extract :S=>T
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
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.BaseOptionalComponent]]
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
		extends BaseComponent[T](value, opts :_*) with RowSchema[O, T]
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
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.BaseComponent]]
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
		extends BaseOptionalComponent[T](value, opts :_*) with RowSchema[O, T]
	{
		def this(value :S => Option[T], buffs :Buff[T]*) = this(value, "", buffs)

		override final val columnPrefix = composite.testedPrefix + prefix
	}



/*
	class SymLinkComponent[C<:Mapping[T], T](target :TypedComponentPath[this.type, C, T]) extends BaseSymLink[this.type, C, T](target) with Component[T] {
		override protected[RowSchema] val pick: (S) => Option[T] = target.pick
		override protected[RowSchema] val surepick: Option[(S) => T] = target.surepick
		override private[RowSchema] def init() = Path(comp => target.lift(comp.adaptedComponent))
	}
*/



	/** An adapter allowing embedding of any other component, regardless of the `Owner` type, as part of this mapping.
	  * The embedded component is exposed by the `body` property. The components of the embedded mapping are lifted
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
	  * @tparam C the type of the embedded component, typically its singleton type.
	  * @tparam T the value type of this component.
	  */
	final class EmbeddedComponent[C <: Mapping.ComponentFor[T], T] private[RowSchema]
	                             (val body :C, protected[schema] val extractor :Extractor[S, T],
	                              override val columnPrefix :String, override val buffs :Seq[Buff[T]])
		extends EagerDeepProxy[C, O, T](body) with ComponentMapping[T]
	{ nest =>

		protected[RowSchema] override def init() :Unit = composite.synchronized {
			if (!isSymLink(this)) { //don't lift subcomponents as its done by EagerDeepProxy
				initComponents += this
				initSubcomponents += this
			}
		}

		protected override def adapt[X](component :egg.Component[X]) :Component[X] =
			liftSubcomponent(component.asInstanceOf[Component[X]])

		override def canEqual(that :Any) :Boolean = that.asInstanceOf[AnyRef] eq this

		override def toString :String = "{{" + body + "}}"

		initPreceding()
	}






	/** Base trait for column components declared directly under this mapping. In general, there should be little need
	  * to descend your classes directly from it, as column functionality is limited and constructor methods provided
	  * within `RowSchema` should be sufficient. As with the base `ComponentMapping` trait, simple creation of
	  * this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options (buffs).
	  * @tparam T value type of this column.
	  */
//	trait Column[T] extends ColumnMapping[Owner, T] with ComponentMapping[T]



	/** A column for which a value exists in all instances of `S`. It doesn't mean that it is mandatory in any particular
	  * SQL statement. It automatically inherits the enclosing mapping's `columnPrefix` and buffs.
	  */
	private class MandatoryColumn[T :ColumnForm](value :S => T, name :String, opts :Seq[Buff[T]])
		extends StandardColumn[Owner, T](testedPrefix + name, opts ++ conveyBuffs(value, testedPrefix + name))
		   with ComponentMapping[T]
	{
		override val buffs = opts ++ inheritedBuffs
		override val extractor :RequisiteExtractor[S, T] = Extractor.req(value)

		composite.include(initPreceding(this))
	}


	/** A lower-level column component which accepts values for all abstract fields and initializes them verbatim.
	  * This column will ''not'' automatically inherit the enclosing mapping's `columnPrefix` or buffs
	  */
	private class ColumnComponent[T](val extractor :Extractor[S, T], name :String, override val buffs :Seq[Buff[T]])
	                                (implicit sqlForm :ColumnForm[T])
		extends StandardColumn[Owner, T](name, buffs) with ComponentMapping[T]
	{
		def this(value :S => T, name :String, buffs :Seq[Buff[T]])(implicit form :ColumnForm[T]) =
			this(Extractor.req(value), name, buffs)

		def this(pick :S => Option[T], surepick :Option[S=>T], name :String, buffs :Seq[Buff[T]])
		        (implicit form :ColumnForm[T]) =
			this(surepick map Extractor.req[S, T] getOrElse Extractor(pick), name, buffs)

		composite.include(initPreceding(this))
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
	protected def embed[T](component :ComponentFor[T], value :S => T, columnPrefix :String, buffs :Seq[Buff[T]] = Nil)
			:EmbeddedComponent[component.type, T] =
	{
		val extractor = Extractor.req(value)
		val allBuffs = buffs ++: conveyBuffs(extractor, component.toString)
		new EmbeddedComponent(component :component.type, extractor, testedPrefix + columnPrefix, allBuffs)
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
	protected def embed[T](component :ComponentFor[T], value :S => T, buffs :Buff[T]*) :EmbeddedComponent[component.type, T] =
		embed(component, value, "", buffs)






	/** A builder adapting a given column template to a column of this `RowSchema`. */
	protected class ColumnCopist[T] private[RowSchema] (template :ColumnMapping[_, T], name :Option[String], buffs :Seq[Buff[T]]) {
		private[RowSchema] def this(source :ColumnMapping[_, T]) = this(source, None, source.buffs)

		template match {
			case self :RowSchema[_, _]#ComponentMapping[_] if self.belongsTo(composite) =>
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
		def apply(getter :S => T) :Column[T] =
			new ColumnComponent[T](getter, testedPrefix + (name getOrElse template.name), buffs)(template.form)

		/** Create a new column using the passed function as its getter and enlist it on all applicable column lists
		  * of this mapping.
		  * @param getter a function returning the value for this column from the mapped entity. If the function
		  *               returns `None`, the `nullValue` from the `ColumnForm` of the template column is used.
		  * @return a new column, being a direct component of this mapping.
		  */
		def opt(getter :S => Option[T]) :Column[T] =
			new ColumnComponent[T](getter, None, testedPrefix + (name getOrElse template.name), buffs)(template.form)

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
		initPreceding(new MandatoryColumn[T](value, name, buffs))

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
		initPreceding(new ColumnComponent(value, None, testedPrefix + name, buffs ++ conveyBuffs(value, testedPrefix + name)))

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
	  * @returns a new column, which will not appear on the `insertable` column list (and possibly others, based on
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


//	protected def foreignKey[T :ColumnForm]()
/*
	def symLink[T](component :Component[T]) :SymLinkComponent[component.type, T] =
		symLink(component.path)

	def symLink[M<:Mapping[T], T](path :TypedComponentPath[this.type, M, T]) :SymLinkComponent[M, T] =
		new SymLinkComponent[M, T](path)


*/



	/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
	  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
	  * introducing default values and any manipulation of the final values. The standard implementation
	  * invokes [[net.noresttherein.oldsql.schema.RowSchema.construct construct(pieces)]] as long as
	  * [[net.noresttherein.oldsql.schema.RowSchema.isDefined isDefined(pieces)]] returns `true`. Additionally,
	  * all `NoSuchElementException` exceptions (thrown by default by components `apply` method when no value can
	  * be assembled or is predefined) are caught and result in returning `None`. All other exceptions,
	  * including `NullPointerException` which may result from unavailable columns, are propagated. Subclasses should
	  * override those methods instead of `assemble`.
	  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
	  *        a `NoSuchElementException` is caught.
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.construct construct]]
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.isDefined isDefined]]
	  */
	override def assemble(pieces: Pieces): Option[S] =
		try {
			isDefined(pieces) ifTrue construct(pieces)
		} catch {
			case _ :NoSuchElementException => None
		}

	/** The final target of the assembly process for this mapping invoked directly by `assemble` (and, indirectly,
	  * `optionally`/`apply`. It is left to implement for subclasses and, in order to make it the simplest possible,
	  * is not responsible for recognizing whether a value can be assembled, but rather this functionality is
	  * shared by `isDefined` method, which can force `assemble` to return `None` without calling `construct`,
	  * and catching later any `NoSuchElementException`s thrown from  this method and resulting from a failed assembly
	  * of a subcomponent. Another difference is that `pieces` is declared as an implicit parameter, which coupled
	  * with an implicit conversion of `Component[O, T]` to `T` in its presence, allows to use the components directly
	  * as arguments to the constructor of the returned subject instance. For example:
	  * {{{
	  *     case class Address(country :String, city :String, zip :String, street :String, no :String)
	  *
	  *     class AddressSchema[O] extends RowSchema[O, Address] {
	  *         val country = column(_.country)
	  *         val city = column(_.city)
	  *         val zip = column(_.zip)
	  *         val street = column(_.street)
	  *         val no = column(_.no)
	  *
	  *         override def construct(implicit pieces :Pieces) :Address =
	  *             Address(country, city, zip, street, no)
	  *     }
	  * }}}
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.isDefined isDefined]]
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.assemble assemble]]
	  */
	protected def construct(implicit pieces :Pieces) :S

	/** Verifies the presence of necessary subcomponents in the input pieces for the assembly to be successful.
	  * This method is called from `assemble` in order to possibly prevent it from proceeding with the assembly
	  * and  calling `construct`, but return `None` instead. The contract obliges it only detect the situations
	  * where `construct` would certainly fail with an exception, but not necessarily all of them. It is designed
	  * primarily with the thought about outer joins where all columns of a table can carry `null` values.
	  * For this reason, it simply always returns `true`, but entity tables override it with a check of availability
	  * of the primary key. The subclasses are free to implement any condition here.
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.construct construct]]
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.assemble assemble]]
	  */
	protected def isDefined(pieces :Pieces) :Boolean = true



	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :ComponentMapping[T])(implicit pieces :Pieces) :T =
		if (component belongsTo this) pieces(component.selector)
		else pieces(apply(component))

	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :Component[T])(implicit pieces :Pieces) :T =
		pieces(apply(component))




	override def lift[T](component :Component[T]) :Component[T] = component match {
		case lifted :RowSchema[_, _]#ComponentMapping[_] if lifted belongsTo this =>
			lifted.asInstanceOf[Component[T]]
		case _ =>
			apply(component).lifted
	}

	override def apply[T](component :Component[T]) :Selector[T] = component match {
		case lifted :RowSchema[_, _]#ComponentMapping[_] if lifted belongsTo this =>
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
		ComponentExtractor[Owner, S, T](component)(component.extractor)

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
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.initPreceding[T](value:T) initPreceding(value)]]
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.ComponentMapping.include() ComponentMapping.include()]]
	  */
	protected final def initPreceding() :Unit = synchronized {
		delayedInits.foreach(_.include())
		delayedInits.clear()
	}

	/** An ease-of-life variant of no-argument `initPreceding()` which returns the given argument after finishing
	  * the initialization of all already defined components. It allows to 'inject' the initialization as an intermediate
	  * step into any expression.
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.initPreceding() initPreceding()]]
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
	  * manually triggers the initialization by a call to [[net.noresttherein.oldsql.schema.RowSchema.initialize() initialize()]].
	  * Any attempt to create new components after this happens will result in `IllegalStateException` thrown
	  * by the constructor.
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.initialize() initialize()]]
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
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.finalizeInitialization()]]
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
	  * @see [[net.noresttherein.oldsql.schema.RowSchema.initialize() initialize()]]
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
	private[this] class LateInitComponents(private[this] var uninitialized :Builder[ComponentMapping[_], List[ComponentMapping[_]]])
		extends AbstractSeq[ComponentMapping[_]] 
	{
		def this() = this(List.newBuilder)

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
			cached = uninitialized.result.toUniqueSeq
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


}






/** A `RowSchema` extension which, in its `optionally` (and indirectly `apply`) method, declares aliasing
  * of its components on the passed `Pieces`. As `RowSchema` (and possibly any of its components) allows
  * declaring a column prefix to be added to all its columns as well as additional buffs which should be inherited
  * by all of its subcomponents (including columns), the component, as defined, can be a different instance from its
  * final representation included on the mapping's component/column lists. This is in particular necessary if a
  * component not extending the inner [[net.noresttherein.oldsql.schema.RowSchema.ComponentMapping ComponentMapping]]
  * class is embedded in the mapping. As it is the latter version of the component which is used by the framework
  * to create any SQL statements, and thus also by the `Pieces`, but typically the original component as defined
  * is used in the assembly process, there is a need to introduce a mapping step in which the `Pieces` implementation
  * substitutes any component passed to it with its lifted representation before looking for its value.
  */
trait RowRootSchema[O, S] extends RowSchema[O, S] with RootMapping[O, S] {
	override def optionally(pieces :Pieces) :Option[S] =
		super.optionally(pieces.aliased { c => apply[c.Subject](c).lifted })
}


