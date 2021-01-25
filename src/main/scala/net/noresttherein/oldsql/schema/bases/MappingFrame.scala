package net.noresttherein.oldsql.schema.bases

import java.sql.{CallableStatement, ResultSet}

import scala.collection.AbstractSeq
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.model.{PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.model.RelatedEntityFactory.KeyExtractor
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, RequisiteExtractor}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnExtract, ColumnForm, ColumnMapping, ColumnMappingExtract, MappingExtract, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{AutoInsert, AutoUpdate, ExtraSelect, Ignored, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalSelect, ReadOnly}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StandardColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormNullValue
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.{ForeignKeyEntityColumnMapping, ForeignKeyEntityMapping, InverseForeignKeyMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.{DeepProxy, OpaqueColumnProxy}
import net.noresttherein.oldsql.schema.Relation.RelVar
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping}
import net.noresttherein.oldsql.schema.support.{BuffedMapping, EffectivelyEmptyMapping, MappingDeclaredBuffs}

//here be implicits
import net.noresttherein.oldsql.slang._






//todo: provide examples
/** A base trait for mappings with a fixed, hierarchical structure, which subjects are assembled
  * from subcomponents of arbitrary depth. This includes table mappings and other instances where columns are known
  * statically and should be created manually, rather than by some generic code. While it exposes mutator methods
  * to facilitate creation and declaration of components, it is expected that they'll be used solely in constructors
  * of derived classes by those classes themselves and, once created, it will be seen as immutable from the outside.
  * If an attempt is made to modify this instance once any of its accessor methods for components had been called
  * previously, an exception will be thrown.
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
  * @see [[net.noresttherein.oldsql.schema.bases.SimpleMapping]]
  */ //todo: document unsafeCascade rules
trait MappingFrame[S, O] extends StaticMapping[S, O] with RelatedMapping[S, O] { frame =>

	/** Base trait for all components of this mapping. Creating an instance automatically lists it within owning mapping
	  * components as well any contained subcomponents and columns within parent mapping appropriate lists.
	  * This registering is delayed, as in most cases properties listing its subcomponents will not be initialized before
	  * its constructor is over, so instead it adds itself only to a 'waiting list', which is processed and its contents
	  * appropriately registered within the parent mapping at a later time, when this instance's constructor returns.
	  * This initialization is triggered as the result of a call to this instance's `include()` or `extract` methods,
	  * the enclosing mapping explicitly requests initializing all components in the waiting lists via its
	  * 'initPreceding()' method, or its own full initialization is triggered either explicitly, by its `initialize()`
	  * method, or when any of its component/column lists are accessed. If for any reason this component is not fully
	  * initialized before one of that happens (typically either because its constructor is called outside the enclosing
	  * mapping's constructor or the mapping's component properties are accessed before all components are created),
	  * an exception will be thrown during its initialization. For this reasons, all components should be created eagerly.
	  * As a consequence of the above, simple creation of this instance registers it permanently with the enclosing
	  * mapping - no other call is needed.
	  *
	  * There are two broad categories of instances: custom classes explicitly derived from it or one of its
	  * subclasses, provided as base classes for that purpose, and wrappers of other mappings. The former are slightly
	  * more efficient and are more concise to declare withing the enclosing mapping class, while the latter offers
	  * more flexibility by allowing dynamic resolution of actual component implementations and their full reuse.
	  * The best of both worlds can be achieved by simply mixing in this trait into a ready component class.
	  *
	  * @tparam T subject type of this component.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.RequisiteComponent]]
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.OptionalComponent]]
	  */
	trait FrameComponent[T] extends BaseMapping[T, O] { self =>

		/** Returns the value of this component in an option if an implicit `ComponentValues` instance
		  * for the enclosing composite mapping is present.
		  */
		@inline final def ?(implicit values :frame.Pieces) :Option[T] = values.get(extract)

		@inline private[MappingFrame] final def belongsTo(mapping :MappingFrame[_, _]) :Boolean = mapping eq frame


		/** Extractor for the value of the component to be used before the component is fully initialized
		  * and the `extract` property is computed. Typically given as the constructor parameter or initialized
		  * in the constructor, it is used as the template for the `MappingExtract` for the component.
		  */
		protected[schema] def componentSelector :Extractor[S, T]

		/** The `MappingExtract` for this component from the enclosing mapping.
		  * The call to this method will trigger the initialization of this component if it was not initialized before.
		  */
		protected[MappingFrame] def extract :frame.Extract[T] = {
			if (fastExtract == null) {
				val s = safeExtract
				if (s != null)
					fastExtract = s
				else {
					include()
					if (fastExtract == null)
						throw new IllegalStateException(s"$this.extract called during initialization of the component.")
				}
			}
			fastExtract
		}

		@volatile
		private[this] var safeExtract :frame.Extract[T] = _
		private[this] var fastExtract :frame.Extract[T] = _

		private[this] var initialized = false

		/** Manually trigger initialization of this instance within the enclosing mapping. */
		private[MappingFrame] final def include() :this.type = frame.synchronized {
			if (!initialized) {
				initialized = true
				fastExtract = init()
				safeExtract = fastExtract
				initExtracts = initExtracts.updated(this, fastExtract)
			}
			this
		}

		/** Register itself and all its subcomponents within the parent mapping. This method will be called only once. */
		protected[MappingFrame] def init() :frame.Extract[T] = frame.synchronized {
			initComponents += this
			initSubcomponents += this
			extracts foreach { exportExtract(_) }
			for (column <- columns if !extracts.contains(column))
				exportColumn(column)
			for (component <- subcomponents if !extracts.contains(component))
				exportSubcomponent(component)
			extractFor(this)
		}

		protected[MappingFrame] def delayInit() :Unit = frame synchronized {
			if (isInitialized)
				throw new IllegalStateException(
					s"Cannot initialize component $this of $frame: components have already been exported.")
			else
				initQueue += this
		}


		/** Lifts ('exports') a column of this component to the column of the enclosing composite `MappingFrame`.
		  * This method is called both during delayed initialization of standard components, and eagerly
		  * by `ExportComponent` in its constructor. For this reason it should not touch any column/component lists.
		  */
		protected[MappingFrame] def exportColumn[U](column :Column[U]) :Column[U] =
			exportSubcomponent(column).asInstanceOf[Column[U]]

		/** Lifts ('exports') a component of this component to the subcomponent of the enclosing composite `MappingFrame`.
		  * This method is called both during delayed initialization of standard components, and eagerly
		  * by `ExportComponent` in its constructor. For this reason it should not touch any column/component lists.
		  */
		protected[MappingFrame] def exportSubcomponent[U](subcomponent :Component[U]) :Component[U] =
			exportExtract(subcomponent, apply(subcomponent)).export

		protected[MappingFrame] def exportExtract[U](sub :Assoc[Component, Extract, U]) :Unit =
			exportExtract(sub._1, sub._2)

		protected[MappingFrame] def exportExtract[U](subextract :MappingExtract[T, U, O]) :frame.Extract[U] =
			exportExtract(subextract.export, subextract)

		protected[MappingFrame] def exportExtract[U](subcomponent :Component[U], subextract :MappingExtract[T, U, O])
				:frame.Extract[U] =
			frame.synchronized {
				if (initExtracts contains subcomponent)
					throw new IllegalStateException(
						s"A duplicate of subcomponent $subcomponent of $this already exists under $frame.")

				val export = subextract.export
				val result = initExtracts.get(export) getOrElse {
					verifyOverrides()

					val fromFrame = subextract compose componentSelector
					val frameBuffs = frame.buffs.unsafeCascade(fromFrame)
					val subbuffs = export.buffs.zipper.locate(frame.buffs).replace(frameBuffs).buffs

					val frameExtract = export match {
						case column :ColumnMapping[_, _] =>
							val adapted = new ColumnComponent[U](
								fromFrame, renameColumn(column.name), subbuffs)(column.form
							)
							extractFor(adapted)
						case _ =>
							val adapted = new ExportComponent[U](
								export, fromFrame, columnPrefix, renameColumn, subbuffs
							)
							extractFor(adapted)
					}
					initExtracts = initExtracts.updated(export, frameExtract)
					frameExtract
				}
				initExtracts = initExtracts.updated(subcomponent, result)
				result
			}

		protected[MappingFrame] def verifyOverrides() :Unit = {
			if (componentSelector == null)
				throw new IllegalStateException(
					s"$this.componentSelector is null: overrides with a val must happen before any component declarations!")
			if (columnPrefix == null)
				throw new IllegalStateException(
					s"$this.columnPrefix is null: overrides with a val must happen before any component declarations!")
			if (relativePrefix == null)
				throw new IllegalStateException(
					s"$this.relativePrefix is null: overrides with a val must happen before any component declarations!")
			if (renameColumn == null)
				throw new IllegalStateException(
					s"$this.renameColumn is null: overrides with a val must happen before any component declarations!")
			if (buffs == null)
				throw new IllegalStateException(
					s"$this.buffs is null: overrides with a val must happen before any component declarations!")

		}

		/** Buffs 'inherited' from the enclosing mapping. It uses the value extract `S => T` to map all buffs applied
		  * to the enclosing mapping to adapt them to this value type. This means that if there are any
		  * [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]s among the inherited buffs, this component
		  * must have a value for the value associated with that buff or an exception will be thrown - either
		  * when accessing/initializing the buff list or when the value for the buff is actually needed.
		  */
		protected final def inheritedBuffs :Buffs[T] = frame.buffs.unsafeCascade(componentSelector)

//		/** The buffs for this component, including buffs inherited from the outer mapping (if not empty).  */
//		override def buffs :Buffs[T] = {
//			val mine = super.buffs
//			val inherited = inheritedBuffs
//			if (inherited.isEmpty) mine
//			else inherited.declare(this, mine)
//		}

		/** The column prefix prepended to all columns at the behest of the enclosing `MappingFrame` or an empty string. */
		protected final def inheritedPrefix :String = verifiedPrefix

		/** The column prefix prepended to all columns of this component, ''not'' including the similar prefix
		  * of the enclosing `MappingFrame`. It becomes, together with
		  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix outer.columnPrefix]] the default value
		  * of [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.columnPrefix fullColumnPrefix]].
		  * Note however that this property may be unused if a component overrides the latter or
		  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.renameColumn renameColumn]].
		  * @return an empty string.
		  */
		protected def relativePrefix :String = ""

		/** The column prefix prepended to all columns by this component ''and'' the enclosing `MappingFrame`.
		  * Note that this property may be unused if a component overrides
		  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.renameColumn renameColumn]] instead.
		  * @return [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix outer.columnPrefix]] `+`
		  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.relativePrefix relativePrefix]]
		  */
		protected def columnPrefix :String =
			if (relativePrefix == null)
				throw new IllegalStateException(
					s"$this.relativePrefix is null: overrides with a val must happen before any component declarations!")
			else inheritedPrefix + relativePrefix

		/** A hook function used to rename columns when exporting them to the outer frame. The function should
		  * return full column names: outer mapping's
		  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]] is not prepended
		  * to the result
		  * @return [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.columnPrefix columnPrefix]]` + _`.
		  */
		protected def renameColumn :String => String = columnPrefix + _

		//enlist ourselves on the list of uninitialized components of the enclosing mapping
		delayInit()
	}



	/** A mixin trait marking components which should be initialized at the last possible moment, when
	  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.initialize initialize]] is called for the enclosing
	  * `MappingFrame`. These components will be put on the enclosing mapping's column and component lists,
	  * together with its subcomponents after all components, not extending this trait, have been initialized.
	  * Calling [[net.noresttherein.oldsql.schema.bases.MappingFrame.initPreceding initPreceding]] after creation
	  * of this component will not trigger its initialization.
	  * This feature is useful for any components which feature lazily evaluated members, in particular subcomponents.
	  * It allows to break infinite initialization loops between several mappings referencing each other - the intent
	  * is that the initialization of these late init components triggers once all tables have been created, albeit
	  * potentially without some minor subcomponents.
	  */
	trait LateInitComponent[T] extends FrameComponent[T] {
		protected[MappingFrame] override def delayInit() :Unit = {
			if (isInitialized)
				throw new IllegalStateException(
					s"Cannot initialize component $this of $frame: components have already been exported.")
			else
				lateInit += this
		}
	}



	/** A proxy class for non-direct subcomponents of the enclosing `MappingFrame` which incorporates any column prefix
	  * and buffs defined in it and any other enclosing components into the adapted component.
	  * Its components are at the same time the export components of the enclosing mapping.
	  */
	private class ExportComponent[T](override val backer :Component[T], val componentSelector :S =?> T,
	                                 override val columnPrefix :String, override val renameColumn :String => String,
	                                 override val buffs :Buffs[T])
		extends DeepProxy[T, O](backer) with FrameComponent[T]
	{
		def this(component :Component[T], selector :S =?> T, rename :String => String, buffs :Buffs[T]) =
			this(component, selector, "", rename, buffs)

		def this(component :Component[T], selector :S =?> T, prefix :String, buffs :Buffs[T]) =
			this(component, selector, prefix, prefix + _, buffs)

		protected[MappingFrame] override def init() :MappingExtract[S, T, O] = frame.synchronized {
			initSubcomponents += this
			extracts foreach { exportExtract(_) }
			initExtracts.getOrElse[frame.Extract, T](backer, extractFor(this))
		}

		protected override def adapt[X](component :backer.Component[X]) :Component[X] = exportSubcomponent(component)
		protected override def adapt[X](column :backer.Column[X]) :Column[X] = exportColumn(column)

		override def assemble(pieces :Pieces) :Opt[T] = backer.assemble(pieces)

		override def toString :String = "^" + backer + "^"
	}



	/** Base trait for components which have a value for all instances of `T`. Subclasses are required to
	  * implement the `pick` property with an extractor function `S => T`. It is primarily useful as a mixin for
	  * ready component classes, while components specific to the enclosing mapping (and belonging
	  * to their implementation) would benefit from extending one of its subtypes provided for that purpose.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseComponent]]
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.ComponentFrame]]
	  */
	trait RequisiteComponent[T] extends FrameComponent[T] {
		protected[schema] override def componentSelector :RequisiteExtractor[S, T] = Extractor.req(pick)
		protected def pick :S => T
	}

	/** Base trait for components which might not have a value for all instances of `T` (such as properties declared
	  * by some subclass of `T`. The component is considered not to be a part of the mapping for some owning entities
	  * of type `S` and, in particular, the underlying columns are excluded from their updates and inserts.
	  *
	  * There are four broad cases when a certain value `T` might be unavailable for
	  * the enclosing mapping's subject instance `S`:
	  *   1. It is a property of type `Option[S]` or similar and signifies the lack of some attribute of the
	  *      represented object from the domain model, in which case the option type itself can be considered
	  *      the mapped value.
	  *   1. The value is not present at a given lifecycle phase of the mapped entity, for example a database
	  *      generated primary key.
	  *   1. It is part of a different table.
	  *   1. The column(s) with the value for the component is not included in every database query (for example,
	  *      for efficiency reasons or because it is a part of a joined table) and therefore neither it is present
	  *      when the entity is being updated. It is a case of value being missing on a given instance, rather than
	  *      for the represented abstract (or real life) entity.
	  *   1. It is a part of a denormalized relation and its existence depends on value(s) of other component(s).
	  *      This is for example the case with mapping of a class with its subclasses to a single database table.
	  *
	  *  Of the above, the first case should use a
	  *  [[net.noresttherein.oldsql.schema.bases.MappingFrame.RequisiteComponent RequisiteComponent]] instance,
	  *  while the last two are clear candidates for this type of the component. The second and third cases
	  *  are less clear cut and can conceivably  use either of the approaches.
	  *
	  * Optional components can cause conflicts with [[net.noresttherein.oldsql.schema.Buff.ValueBuff value buffs]]
	  * declared on the outer frame or some of its ancestors. If `pick` function defined by the component returns
	  * no value for a value stored in such a buff,
	  * a [[net.noresttherein.oldsql.exceptions.BuffMappingFailureException BuffMappingFailureException]]
	  * will be thrown. This can happen both when the component's buffs are created - either in this component's
	  * constructor or when its buffs are accessed during any database operation - and when the value of the said buff
	  * is being requested - it depends on whether the value buff carries a constant or a generator expression.
	  * An optional component is safe from this problem if one of the following conditions holds:
	  *   1. No value buffs are among the outer frame's buffs;
	  *   1. `pick` always returns a value for all value buffs declared or inherited by the outer frame;
	  *   1. All unsafe buffs are not [[net.noresttherein.oldsql.schema.Buff.cascades cascading]];
	  *   1. This component does not inherit buffs from its outer frame.
	  *
	  *  Note that while optional components will often be used in combination with the
	  *  `OptionalSelect`/`OptionalInsert`/`OptionalUpdate` buffs, none of them is automatically implied.
	  *
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseOptionalComponent]]
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.OptionalFrame]]
	  * @see [[net.noresttherein.oldsql.schema.bits.OptionMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Buff.OptionalSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect Buff.ExplicitSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate Buff.OptionalUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate Buff.ExplicitUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalInsert Buff.OptionalInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert Buff.ExplicitInsert]]
	  */ //todo: should this come already with an OptionalInsert/Update/Select buff?
	trait OptionalComponent[T] extends FrameComponent[T] { //would be beneficial to allow overriding of the extractor for efficiency of Opt vs Option
		protected[schema] override def componentSelector :S =?> T = Extractor(pick)
		protected def pick :S => Option[T]
	}

	trait ReadOnlyComponent[T] extends FrameComponent[T] {
		protected[schema] override def componentSelector :Extractor[S, Nothing] = Extractor.none
	}


	/** Convenience base component class which initializes the extractor using the constructor argument getter.
	  * @param pick           extractor function for the value of this component.
	  * @param relativePrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                       It is relative to this component, not the outer `MappingFrame`: the `columnPrefix` declared
	  *                       by the latter is combined with this prefix when exporting columns to the outer mapping.
	  *                       All columns accessed through this instance methods will be affected, not only when viewed
	  *                       from parent mappings. A column which is defined within a subcomponent of this component
	  *                       will not show the prefix however when viewed in the scope of that component - only
	  *                       the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *                       of this component will be the concatenation of the column prefix defined by the enclosing
	  *                       mapping and this prefix.
	  * @param declaredBuffs  buffs specific to this component. This list will be extended with buffs inherited
	  *                       from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *                       to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseOptionalComponent]]
	  */
	abstract class BaseComponent[T](protected override val pick :S => T, override val relativePrefix :String,
	                                declaredBuffs :Buff[T]*)
		extends RequisiteComponent[T]
	{
		def this(pick :S => T, declaredBuffs :Buff[T]*) = this(pick, "", declaredBuffs :_*)

		override val buffs :Buffs[T] = inheritedBuffs.declare(this, declaredBuffs :_*)
	}

	/** Convenience base class for components of this instance which themselves contain individually defined
	  * static subcomponents.
	  * @param pick           returns value of this component on a given parent entity.
	  * @param relativePrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                       It is relative to this component, not the outer `MappingFrame`: the `columnPrefix` declared
	  *                       by the latter is combined with this prefix when exporting columns to the outer mapping.
	  *                       All columns accessed through this instance methods will be affected, not only when viewed
	  *                       from parent mappings. A column which is defined within a subcomponent of this component
	  *                       will not show the prefix however when viewed in the scope of that component - only
	  *                       the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *                       of this component will be the concatenation of the column prefix defined by the enclosing
	  *                       mapping and this prefix.
	  * @param declaredBuffs  buffs specific to this component. This list will be extended with buffs inherited
	  *                       from the enclosing schema. The export versions of all subcomponents of this component will
	  *                       inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class ComponentFrame[T](protected override val pick :S => T,
	                                 protected override val relativePrefix :String, declaredBuffs :Buff[T]*)
		extends MappingFrame[T, O] with RequisiteComponent[T]
	{
		def this(value :S => T, buffs :Buff[T]*) = this(value, "", buffs :_*)

		override val buffs :Buffs[T] = inheritedBuffs.declare(this, declaredBuffs :_*)
		protected override val columnPrefix :String = relativePrefix + inheritedPrefix
	}

	/** Convenience base class for components which may not have a value for some instances of the enclosing mapping's
	  * subject type `S`.
	  * @param pick extractor function for the value of this component.
	  * @param declaredBuffs buffs specific to this component. This list will be extended with buffs inherited
	  *                      from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *                      to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseComponent]]
	  */
	abstract class BaseOptionalComponent[T](protected override val pick :S => Option[T],
	                                        protected override val relativePrefix :String, declaredBuffs :Buff[T]*)
		extends OptionalComponent[T]
	{
		def this(value :S => Option[T], buffs :Buff[T]*) = this(value, "", buffs :_*)

		override val buffs :Buffs[T] = inheritedBuffs.declare(this, declaredBuffs :_*)
	}

	/** Convenience base class for optional components of this instance which themselves contain individually defined
	  * static subcomponents.
	  * @param pick           returns value of this component on a given parent entity.
	  * @param relativePrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                       It is relative to this component, not the outer `MappingFrame`: the `columnPrefix` declared
	  *                       by the latter is combined with this prefix when exporting columns to the outer mapping.
	  *                       All columns accessed through this instance methods will be affected, not only when viewed
	  *                       from parent mappings. A column which is defined within a subcomponent of this component
	  *                       will not show the prefix however when viewed in the scope of that component - only
	  *                       the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *                       of this component will be the concatenation of the column prefix defined by the enclosing
	  *                       mapping and this prefix.
	  * @param declaredBuffs  buffs specific to this component. This list will be extended with buffs inherited
	  *                       from the enclosing schema. The export versions of all subcomponents of this component will
	  *                       inherit these buffs.
	  * @tparam T value type of this component.
	  */
	abstract class OptionalFrame[T](protected override val pick :S => Option[T],
	                                protected override val relativePrefix :String, declaredBuffs :Buff[T]*)
		extends MappingFrame[T, O] with OptionalComponent[T]
	{
		def this(value :S => Option[T], buffs :Buff[T]*) = this(value, "", buffs :_*)

		override val buffs :Buffs[T] = inheritedBuffs.declare(this, declaredBuffs :_*)
		protected override val columnPrefix :String = relativePrefix + inheritedPrefix
	}



	/** An adapter allowing embedding of any other component, regardless of the `Origin` type, as part of this mapping.
	  * The embedded component is not exposed to the outside. The components of the embedded mapping are exported
	  * as components of the enclosing mapping by incorporating the `columnPrefix` and `buffs` given here (including
	  * those inherited from the enclosing mapping). At the same time, they form the components of this instance exposed
	  * in its component and column lists.
	  * @param component the embedded component.
	  * @param componentSelector the extractor for the value of this component.
	  * @param columnPrefix the prefix string prepended to the names of all (export) columns of `body`.
	  *                     The value of `columnPrefix` of the enclosing mapping is ''not'' prepended automatically
	  *                     to this value - it should be done by the caller if required.
	  * @param buffs the buffs attached to this component, inherited by all of its export subcomponents.
	  *              The buffs of the enclosing mapping are ''not'' automatically added to this list -
	  *              it should be done by the caller if required.
	  * @tparam T the value type of this component.
	  */
	private class CompositeComponent[T] private[MappingFrame]
	              (component :MappingOf[T], protected[schema] val componentSelector :S =?> T,
	               override val columnPrefix :String, override val renameColumn :String => String,
	               override val buffs :Buffs[T])
		extends DeepProxy[T, O](component) with FrameComponent[T]
	{ nest =>
		def this(component :MappingOf[T], selector :S =?> T, columnPrefix :String, buffs :Buffs[T]) =
			this(component, selector, columnPrefix, columnPrefix + _, buffs)

		def this(component :MappingOf[T], selector :S =?> T, rename :String => String, buffs :Buffs[T]) =
			this(component, selector, "", rename, buffs)

		protected[MappingFrame] override def init() :MappingExtract[S, T, O] = frame.synchronized {
			val cast = component.asInstanceOf[Component[T]]
			if (initExtracts contains cast)
				throw new IllegalArgumentException(
					s"Can't embed mapping $component as a component of $frame as it is already present.")
			val extract = extractFor(this)
			initExtracts = initExtracts.updated(cast, extract)
			cast.extracts foreach { assoc => exportExtract(assoc) }
			initComponents += this
			initSubcomponents += this //don't export subcomponents as its done by DeepProxy
			extract
		}

		protected override def adapt[X](component :backer.Component[X]) :Component[X] =
			exportSubcomponent(component.asInstanceOf[Component[X]])

		protected override def adapt[X](component :backer.Column[X]) :Column[X] =
			exportColumn(component.asInstanceOf[Column[X]])


		override def toString :String = "{{" + backer + "}}"

	}



	private class FKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                         (override val componentSelector :S =?> R, rename :String => String, declared :Seq[Buff[R]])
	                         (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X])
		extends ForeignKeyEntityMapping[M, C, K, E, T, R, X, O](
				rename, factory, frame.buffs.unsafeCascade(componentSelector).declare(declared :_*))(table, pk)
           with LateInitComponent[R]
	{
		def this(selector :S =?> R, prefix :String, buffs :Seq[Buff[R]])
		        (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X]) =
			this(selector, prefix + _, buffs)(factory, table, pk)

		override val columnPrefix = "" //as ForeignKeyEntityMapping already handles renaming
	}

	private class InverseFKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                                (override val componentSelector :S =?> R, key :C[O], declared :Seq[Buff[R]])
	                                (factory :RelatedEntityFactory[K, E, T, R],
	                                 table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X])
		extends InverseForeignKeyMapping[M, C, K, E, T, R, X, O](
				key, factory, frame.buffs.unsafeCascade(componentSelector).declare(declared :_*))(table, fk)
		   with FrameComponent[R] with EffectivelyEmptyMapping[R, O]
	{
		key match {
			case comp :MappingFrame[_, _]#FrameComponent[_] if comp belongsTo frame =>
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $key given as the local referenced key for a foreign key inverse is not a component of $frame.")
		}
	}



	/** Base trait for columns of the enclosing `MappingFrame`. It is extended by all directly declared columns
	  * as well as the export versions of transitive columns from its subcomponents. In general, there should be
	  * little need to extend your classes directly from it, as column functionality is limited and constructor methods
	  * provided within `MappingFrame` should be sufficient. As with the base `FrameComponent` trait, simple creation of
	  * this instance schedules its registration within the parent mapping on the appropriate column lists, depending on
	  * declared column options (buffs). It will also be registered on the subcomponents list, but not the one
	  * for direct components.
	  * @tparam T value type of this column.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.DirectColumn]]
	  */
	trait FrameColumn[T] extends FrameComponent[T] with ColumnMapping[T, O] {
		protected[MappingFrame] val index :Int = nextColumnIndex()

		protected[MappingFrame] override def extract :frame.ColumnExtract[T] =
			super.extract.asInstanceOf[ColumnMappingExtract[S, T, O]]

		protected[MappingFrame] override def init() :MappingExtract[S, T, O] = frame.synchronized {
			includeColumn(this)
			extractFor(this)
		}
	}


	/** A column declared directly under this mapping.
	  * It automatically inherits the enclosing mapping's `columnPrefix` and buffs.
	  */
	protected class DirectColumn[T :ColumnForm]
	                            (protected[schema] override val componentSelector :S =?> T, name :String,
	                             declaredBuffs :Seq[Buff[T]])
		extends StandardColumn[T, O](verifiedPrefix + name,
		                             frame.buffs.unsafeCascade(componentSelector).declare(declaredBuffs :_*))
		   with FrameColumn[T]
	{
		def this(value :S => T, name :String, buffs :Seq[Buff[T]]) =
			this(Extractor.req(value), name, buffs)

		protected[MappingFrame] override def init() :ColumnMappingExtract[S, T, O] = frame.synchronized {
			initComponents += this
			frame.includeColumn(this)
			extractFor(this)
		}
	}


	/** A lower-level column component which accepts values for all abstract fields and initializes them verbatim.
	  * This column will ''not'' automatically inherit the enclosing mapping's `columnPrefix` or buffs.
	  * It is not included on the direct components list of this mapping.
	  */
	private class ColumnComponent[T](override val componentSelector :S =?> T, name :String,
	                                 override val buffs :Buffs[T])
	                                (implicit sqlForm :ColumnForm[T])
		extends StandardColumn[T, O](name, buffs) with FrameColumn[T]
	{
		def this(selector :S =?> T, name :String, buffs :Seq[Buff[T]])(implicit form :ColumnForm[T]) =
			this(selector, name, frame.buffs.unsafeCascade(selector).declare(buffs :_*))

		def this(value :S => T, name :String, buffs :Buffs[T])(implicit form :ColumnForm[T]) =
			this(Extractor.req(value), name, buffs)

		def this(pick :S => Option[T], surepick :Option[S => T], name :String, buffs :Buffs[T])
		        (implicit form :ColumnForm[T]) =
			this(surepick map Extractor.req[S, T] getOrElse Extractor(pick), name, buffs)

		protected[MappingFrame] override def init() :ColumnMappingExtract[S, T, O] = frame.synchronized {
			frame.includeColumn(this)
			extractFor(this)
		}
	}


	private class FKColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X]
	                      (override val componentSelector :S =?> R, suffix :String, declaredBuffs :Seq[Buff[R]])
	                      (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => ColumnMapping[K, X])
		extends ForeignKeyEntityColumnMapping[M, K, E, T, R, X, O](
				                              verifiedPrefix + suffix, factory,
		                                      frame.buffs.unsafeCascade(componentSelector).declare(declaredBuffs :_*)
		                                     )(table, pk)
		   with FrameColumn[R] with LateInitComponent[R]
	{ fk =>
		private[this] val lazyKey = Lazy {
			val selector = KeyExtractor(factory)
			val keyBuffs = buffs.unsafeCascade(selector).declare()
			if (target.isInstanceOf[SimpleColumn[_, _]])
				new ColumnComponent[K](componentSelector andThen selector, name, keyBuffs)(target.form)
			else
				new OpaqueColumnProxy[K, O](target, name, keyBuffs)
					with FrameColumn[K] with LateInitComponent[K]
				{
					override val componentSelector = fk.componentSelector andThen selector
				}
		}
		override def key :Column[K] = lazyKey

		override lazy val columns = Unique(key)

		protected[MappingFrame] override def init() :frame.ColumnExtract[R] = frame synchronized {
			initComponents += this
			initSubcomponents += this
			lazyKey.get //trigger construction and registration in the initQueue
			extractFor(this)
		}
	}

//	private class InverseFKColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X]
//	                             (override val componentSelector :S =?> R, opts :Seq[Buff[R]])
//	                             (key :ColumnMapping[K, O], factory :RelatedEntityFactory[K, E, T, R])
//	                             (table :RelVar[M], fk :M[X] => ForeignKeyColumnMapping[MappingAt, K, _, X])
//		extends InverseForeignKeyColumnMapping[M, K, E, T, R, X, O](key, factory, opts)(table, fk)
//		   with FrameColumn[R]
//	{
//		key match {
//			case col :MappingFrame[_, _]#FrameColumn[_] if col belongsTo frame =>
//			case _ =>
//				throw new IllegalArgumentException(
//					s"Column $key given as the local referenced key for a foreign key inverse is not a component of $frame."
//				)
//
//		}
//		override val buffs = super.buffs
//	}






	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * This is the most generic variant of the method, accepting a function renaming all columns and a buff list of
	  * the exported component. This allows to bypass their inheritance from the enclosing mapping and remove
	  * any desired buffs from those already present on the component.
	  * @param mapping any component mapping.
	  * @param extractor an `Extractor` returning the value of the component from the enclosing mapping's subject.
	  * @param rename a function returning the name for the exported column for the name of a `mapping`'s column.
	  *               It should return full column names -
	  *               [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]]
	  *               will not be prepended to the result.
	  * @param fullBuffs complete list of buffs of the exported component.
	  * @tparam T the subject type of the embedded component.
	  * @return The 'export' version of the adapted component, with its name prefix and buffs reset.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](mapping :MappingOf[T], extractor :S =?> T, fullBuffs :Buffs[T])(rename :String => String)
			:Component[T] =
		synchronized {
			initPreceding() //not redundant because must precede the component's constructor
			initPreceding(new CompositeComponent(mapping, extractor, rename, fullBuffs))
		}

	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * This is a generic variant of the method, accepting complete column name prefix and a buff list of
	  * the exported component. This allows to bypass their inheritance from the enclosing mapping and remove
	  * any desired buffs from those already present on the component.
	  * @param mapping any component mapping.
	  * @param extractor an `Extractor` returning the value of the component from the enclosing mapping's subject.
	  * @param fullPrefix a string added as a prefix to all column names of the exported component.
	  * @param fullBuffs complete list of buffs of the exported component.
	  * @tparam T the subject type of the embedded component.
	  * @return The 'export' version of the adapted component, with its name prefix and buffs reset.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](mapping :MappingOf[T], extractor :S =?> T, fullPrefix :String, fullBuffs :Buffs[T])
			:Component[T] =
		synchronized {
			initPreceding() //not redundant because must precede the component's constructor
			initPreceding(new CompositeComponent(mapping, extractor, fullPrefix, fullBuffs))
		}

	/** Embeds any component, regardless of its `Origin` type, directly under this instance.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
 	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param rename a function returning the name for the exported column for the name of a `mapping`'s column.
	  *               It should return full column names -
	  *               [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]]
	  *               will not be prepended to the result.
	  * @param buffs buffs to attach to the created component. They will precede any buffs already present
	  *              on the component and buffs inherited from the enclosing mapping will be appended at the end.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */ //in Scala 2 this signature is ambiguous with the following one
	protected def embed[T](value :S => T, buffs :Buff[T]*)(rename :String => String)(implicit mapping :MappingOf[T])
			:Component[T] =
	{
		val extractor = Extractor.req(value)
		val allBuffs = (mapping.buffs +/: this.buffs.cascade(value)).declare(buffs :_*)
		embed(mapping, extractor, allBuffs)(rename)
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
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](columnPrefix :String, value :S => T, buffs :Buff[T]*)(implicit mapping :MappingOf[T])
			:Component[T] =
	{
		val extractor = Extractor.req(value)
		val allBuffs = (mapping.buffs +/: this.buffs.cascade(value)).declare(buffs :_*)
		embed(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
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
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](value :S => T, buffs :Buff[T]*)(implicit mapping :MappingOf[T]) :Component[T] =
		embed("", value, buffs :_*)

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
	  *              `ReadOnly[T]` is automatically added to this list.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](columnPrefix :String, buffs :Buff[T]*)(implicit mapping :MappingOf[T]) :Component[T] = {
		val extractor = Extractor.none :S =?> T
		val allBuffs = mapping.buffs.declare(ReadOnly[T] +: buffs :_*) //we don't cascade frame flags without a function
		embed(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}

	/** Embeds a read-only component directly under this instance, regardless of its `Origin` type.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param buffs buffs to attach to the created component. They will precede any buffs already present
	  *              on the component and buffs inherited from the enclosing mapping will be appended at the end.
	  *              `ReadOnly[T]` is automatically added to this list.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](buffs :Buff[T]*)(implicit mapping :MappingOf[T]) :Component[T] =
		embed("", buffs :_*)

	/** Embeds a read-only component directly under this instance, regardless of its `Origin` type.
	  * All its columns and subcomponents are similarly embedded after adapting and included in the `subcomponents`
	  * list (and all appropriate column lists for columns). The embedded component is not exposed to this mapping
	  * and should not be used directly, but only through its created export version and the export versions
	  * of all its components and columns, exposed via the generic `Mapping` API.
	  * A `ReadOnly[T]` buff will be automatically attached to the component.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @tparam T the subject type of the embedded component.
	  * @return the 'export' version of the given mapping, automatically registered on this mapping's `components` list.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def embed[T](implicit mapping :MappingOf[T]) :Component[T] = embed[T]()



	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * The export version will have exactly the buffs and columns will be renamed with the function specified here;
	  * this allows bypassing their inheritance from this mapping as well as removing any buffs present on the component.
	  * @param component a mapping embedded as a component in the enclosing mapping.
	  * @param extractor an extractor returning the value of this component for a given subject value of this mapping.
	  * @param rename a function returning the name for the exported column for the name of a `mapping`'s column.
	  *               It should return full column names -
	  *               [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]]
	  *               will not be prepended to the result.
	  * @param fullBuffs buffs to attach to the front of the created component's buff list.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component.
	  */
	protected def component[M <: Component[T], T]
	                       (component :M, extractor :S =?> T, fullBuffs :Buffs[T])(rename :String => String) :M =
		synchronized {
			initPreceding()
			if (initExtracts contains component)
				throw new IllegalArgumentException(s"Can't embed the component $component in $this for a second time.")

			val export = new ExportComponent[T](component, extractor, rename, fullBuffs)
			initComponents += export
			initPreceding()
			component
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
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component.
	  */
	protected def component[M <: Component[T], T]
	                       (component :M, extractor :S =?> T, fullPrefix :String, fullBuffs :Buffs[T]) :M =
		synchronized {
			initPreceding()
			if (initExtracts contains component)
				throw new IllegalArgumentException(s"Can't embed the component $component in $this for a second time.")

			val export = new ExportComponent[T](component, extractor, fullPrefix, fullBuffs)
			initComponents += export
			initPreceding()
			component
		}

	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param rename a function returning the name for the exported column for the name of a `mapping`'s column.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def component[M <: Component[T], T](value :S => T, buffs :Buff[T]*)(rename :String => String)
	                                             (implicit mapping :M) :M =
	{
		val extractor = Extractor.req(value)
		val allBuffs = (mapping.buffs +/: this.buffs.cascade(value)).declare(buffs :_*)
		component(mapping, extractor, allBuffs)(rename)
	}

	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @throws IllegalArgumentException if the mapping is already embedded in this instance as part of another
	  *                                  component
	  */
	protected def component[M <: Component[T], T](columnPrefix :String, value :S => T, buffs :Buff[T]*)
	                                             (implicit mapping :M) :M =
	{
		val extractor = Extractor.req(value)
		val allBuffs = mapping.buffs.declare(ReadOnly[T] +: buffs :_*) //we don't cascade frame flags without a function
		component(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}

	/** Embeds a mapping of type `T` as a component of this instance. The 'export' version of the component will
	  * be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param value a getter function returning the value of this component for a given subject value of this mapping.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  */
	protected def component[M <: Component[T], T](value :S => T, buffs :Buff[T]*)(implicit mapping :M) :M =
		component("", value, buffs :_*)

	/** Embeds a read-only mapping of type `T` as a component of this instance. The 'export' version of the component
	  * will be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list. The component will
	  * be automatically annotated with the `ReadOnly` buff.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param columnPrefix a string prepended to all column names; this prefix is prepended with the value
	  *                     of this mapping's `columnPrefix`.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly$ Buff.ReadOnly]]
	  */
	protected def component[M <: Component[T], T](columnPrefix :String, buffs :Buff[T]*)(implicit mapping :M) :M = {
		val extractor = Extractor.none :S =?> T
		val allBuffs = mapping.buffs.declare(ReadOnly[T] +: buffs :_*)
		component(mapping, extractor, verifiedPrefix + columnPrefix, allBuffs)
	}

	/** Embeds a read-only mapping of type `T` as a component of this instance. The 'export' version of the component
	  * will be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list. The component will
	  * be automatically annotated with the `ReadOnly` buff.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @param buffs buffs to attach to the front of the created component's buff list.
	  *              That list is further expanded with any buffs inherited from this mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `component` argument.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly$ Buff.ReadOnly]]
	  */
	protected def component[M <: Component[T], T](buffs :Buff[T]*)(implicit mapping :M) :M =
		component("", buffs :_*)

	/** Embeds a read-only mapping of type `T` as a component of this instance. The 'export' version of the component
	  * will be included on the `components` list of this mapping, and all its subcomponents and columns will likewise
	  * receive export versions included on the `subcomponents` and appropriate columns list. The component will
	  * be automatically annotated with the `ReadOnly` buff.
	  * @param mapping a mapping embedded as a component in the enclosing mapping.
	  * @tparam T the value type of the embedded component.
	  * @return the `mapping` argument.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly$ Buff.ReadOnly]]
	  */
	protected def component[T](implicit mapping :Component[T]) :mapping.type =
		component[mapping.type, T]("")(mapping)



	protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                             (value :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], pk :M[_] => C[_], reference :RelatedEntityFactory[K, E, T, R])
	                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		initPreceding(new FKComponent(value, rename, buffs)(reference, table, pk.asInstanceOf[M[()] => C[()]]))

	protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                       (value :S => R, key :C[O], reference :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:ForeignKeyMapping[M, C, K, R, O] =
		initPreceding(new InverseFKComponent[M, C, K, E, T, R, ()](value, key, buffs)(
			reference, table, fk.asInstanceOf[M[()] => ForeignKeyMapping[MappingAt, C, K, _, ()]]
		))



	/** A builder adapting a given column template to a column of this `MappingFrame`. */
	protected class ColumnCopist[T] private[MappingFrame]
	                            (template :ColumnMapping[T, _], name :Option[String], buffs :Buffs[T])
	{
		private[MappingFrame] def this(source :ColumnMapping[T, _]) = this(source, None, source.buffs)

		template match {
			case self :MappingFrame[_, _]#FrameComponent[_] if self.belongsTo(frame) =>
				if (frame.buffs == null)
					throw new IllegalStateException(s"$frame.buffs is null: overrides must be defined before any components.")
				if (verifiedPrefix.length > 0 || buffs.nonEmpty)
					throw new IllegalArgumentException(
						s"Can't use column $template of the same mapping $frame as the template for a new column.")
			case _ =>
		}

		/** Create a new column using the passed function as its getter and enlist it on all applicable column lists
		  * of this mapping.
		  * @param getter a function returning the value for this column from the mapped entity.
		  * @return a new column, being a direct component of this mapping.
		  */
		def apply(getter :S => T) :Column[T] = frame.synchronized {
//			val fullBuffs = frame.buffs.cascade(getter).declare(buffs)
			val fullName = name getOrElse template.name
			val column = new ColumnComponent[T](getter, fullName, buffs)(template.form)
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
		def opt(getter :S => Option[T]) :Column[T] = frame.synchronized {
//			val fullBuffs = frame.buffs.unsafeCascade(getter).declare(buffs :_*)
			val fullName = name getOrElse template.name
			val column = new ColumnComponent[T](getter, None, fullName, buffs)(template.form)
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
			new ColumnCopist(template, name, Buffs(buffs :_*))

		/** Replaces the `buff` list of the template column with the provided buffs. */
		def set(buffs :Buffs[T]) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs)

		/** Prepends the given buffs to the list of buffs of the template column. */
		def add(buffs :Buff[T]*) :ColumnCopist[T] =
			new ColumnCopist(template, name, buffs ++: this.buffs.declared)

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
	protected def column[T](template :ColumnMapping[T, _], buffs :Buffs[T]) :ColumnCopist[T] =
		new ColumnCopist(template, None, buffs)

	/** Create a column based on the given column, replacing its name and buff list with the provided values.
	  * @return a builder instance allowing to override any properties of the given template column.
	  */
	protected def column[T](template :ColumnMapping[T, _], name :String, buffs :Buffs[T]) :ColumnCopist[T] =
		new ColumnCopist(template, Some(name), buffs)



	/** Create a new column as a direct component of this mapping. The column will inherit any buffs declared
	  * by this mapping and its name will be the concatenation of this mapping's
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and `name`.
	  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
	  * @param value the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @param form an implicit form defining how the column value `T` is mapped to the underlying SQL type
	  *             of the JDBC API.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def column[T](name :String, value :S => T, buffs :Buff[T]*)(implicit form :ColumnForm[T]) :Column[T] =
		initPreceding(new DirectColumn[T](value, name, buffs))

	/** Create a new column representing a property of the mapped subject as a direct component of this mapping.
	  * The column's name will be the concatenation of this mapping's `columnPrefix` and the name of the property
	  * as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName` as the `value`
	  * argument will set the column name to "firstName". Any buffs declared on this mapping will be inherited
	  * by the created column.
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
	  * instead. The column will inherit any buffs declared on this mapping and its name will be the concatenation
	  * of this mapping's [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]] and `name`.
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


	/** Create a new column as a direct component of this instance, which value is never updated or inserted
	  * by the application. The column will inherit any buffs declared on this mapping and its name will be the concatenation
	  * of this mapping's [[net.noresttherein.oldsql.schema.bases.MappingFrame.columnPrefix columnPrefix]] and `name`.
	  * @param name the name of the column (the complete name will include `this.columnPrefix`).
	  * @param buffs the buffs to attach to the created column. This list will be prepended with `Buff.ReadOnly[T]`.
	  * @return a new column, enlisted on the `components`, `subcomponents`, `columns`, `selectable`, `selectedByDefault`,
	  *         `filterable`, `filteredByDefault` properties of this mapping, depending on the buffs given here
	  *         and inherited from this mapping.
	  * @see [[net.noresttherein.oldsql.schema.Buff.ReadOnly$ Buff.ReadOnly]]
	  */
	protected def column[T :ColumnForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new DirectColumn[T](Extractor.none, name, ReadOnly[T] +: buffs))


	/** Create a new column as a direct component of this instance, which is not mapped to any property
	  * of this mapping's subject. The created column will appear only on the `columns` property of this mapping
	  * and not any of the lists dedicated to particular operations, as it will define buffs indicating it should
	  * be ignored by every database operation. Such a column can still be used manually when creating SQL statements,
	  * for example to narrow down the result set, excluding rows which should be ignored by the application.
	  * @param name the name of the column (the complete name will include `this.columnPrefix`).
	  * @param buffs the buffs to attach to the created column. This list will be prepended with `Buff.Ignored[T]`.
	  * @return a new column, enlisted on the `columns`, `components` and `subcomponents` property of this mapping
	  *         and none other.
	  * @see [[net.noresttherein.oldsql.schema.Buff.Ignored$ Buff.Ignored]]
	  */
	protected def ignored[T :ColumnForm](name :String, buffs :Buff[T]*) :Column[T] =
		initPreceding(new DirectColumn[T](Extractor.none, name, Ignored[T] +: buffs))


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



	protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                             (name :String, value :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], pk :M[_] => ColumnMapping[K, _],
	                              reference :RelatedEntityFactory[K, E, T, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		initPreceding(new FKColumn(value, name, buffs)(reference, table, pk.asInstanceOf[M[()] => ColumnMapping[K, ()]]))


//	protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], K, E, X, R]
//	                                    (value :S => R, key :ColumnMapping[K, O],
//	                                     reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
//	                                    (table :RelVar[M], fk :M[_] => ForeignKeyColumnMapping[MappingAt, K, _, _])
//			:ForeignKeyColumnMapping[M, K, R, O] =
//		initPreceding(new InverseFKColumn(value, buffs)(key, reference)(
//			table, fk.asInstanceOf[M[()] => ForeignKeyColumnMapping[MappingAt, K, _, ()]]
//		))



	/** Renames a column of this mapping. The column might be either directly declared by this mapping or
	  * any of its subcomponents preceding this call. The new column will have the buffs of its export version,
	  * as would be returned by `apply(column)`. It is not required that `column` is an instance of
	  * `ColumnMapping` or that it upholds its invariants: only that it declares exactly one column
	  * and has non empty `name`. This means that adapted/mapped columns are accepted.
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
				initExtracts.getOrElse(column,
					throw new IllegalArgumentException(
						s"Cannot rename column $column as it is not a part of mapping $this."
				))
		}
		val export = current.export
		val extractor = Extractor(current.optional, current.requisite) //don't retain the reference to export

		val lists = initComponents::initSubcomponents::initColumns::
			initSelectable::initFilterable:: initUpdatable::initInsertable::initAutoInsert::initAutoUpdate::
			initSelectedByDefault::initFilteredByDefault::initUpdatedByDefault::initInsertedByDefault::Nil
		//todo: handle columns with custom assemble
		val replacement = new ColumnComponent[T](extractor, name, export.buffs)(column.form) {
			//we'll replace the export column with this manually
			override val index = current.export.asInstanceOf[FrameColumn[T]].index
			override def init() :ColumnMappingExtract[S, T, O] = extractFor(this)
		}
		//creating the above column increased automatically this count, but we've overriden the index to that of
		//the current column
		initColumnCount -= 1
		initColumnIndex(replacement.index) = replacement

		lists foreach { list => //swap the replaced component with the renamed version
			val uninitialized = list.uninitialized
			val i = uninitialized.indexOf(export)
			if (i >= 0)
				uninitialized(i) = replacement
		}


		//we now need to find all versions of the replaced component (as components of some subcomponent)
		//and replace their mapping from export to replacement
		val extract = extractFor(replacement)
		initExtracts = initExtracts map { lift =>
			if (lift._2.export == export)
				Assoc[Column, ColumnExtract, T](lift._1.asInstanceOf[Column[T]], extract)
			else lift
		}
		//just to be sure in case equals is wonky and to trigger (empty) initialization
		initExtracts = initExtracts.updated(column, replacement.extract)
		replacement
	}






	/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
	  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
	  */
	implicit def valueOf[T](component :FrameComponent[T])(implicit pieces :Pieces) :T =
		pieces(component.extract)


	private[this] final val initBuffs = Lazy {
		val bs = declaredBuffs
		if (bs == null)
			throw new IllegalStateException(
				s"$this.buffs is null: overrides must happen before any component declarations.")
		Buffs(this, bs :_*)
	}

	/** Optional flags and annotations modifying the way this mapping is used: primarily to include or exclude columns
	  * from a given type of SQL statements. It is inherited by columns and components of this mapping with the
	  * exception of those created using one of `embed` methods. As the result, declaring this component, for example,
	  * as [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] will propagate this flag to
	  * all columns (including indirect ''export'' columns) and its check for the given buff will be positive.
	  * However, Any component on the path to a column, including the column itself, can declare the same buff,
	  * overriding the definition provided here. Additionally, some buffs
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradict]] other buffs; in that case any buff
	  * declared 'lower' and contradicting a given of these buffs, will 'shadow' it: any check for the shadowed buff
	  * on the component with the contradicting buff or its subcomponent will be negative.
	  *
	  * Most subclasses will choose to override
	  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.declaredBuffs declaredBuffs]] instead (if at all).
	  * @return `Buffs(this, `[[net.noresttherein.oldsql.schema.bases.MappingFrame.declaredBuffs declaredBuffs]]`:_*)`.
	  */
	override def buffs :Buffs[S] = initBuffs.get

	/** Buffs declared by this instance (rather than inherited). It becomes
	  * the [[net.noresttherein.oldsql.schema.Buffs.front front]] portion of this instance's
	  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.buffs buffs]]. This property is for use
	  * in `Buffs` constructor only and it should never be used in place of `buffs.declared`, as extending classes
	  * can override directly `buffs` rather than this property.
	  */
	protected def declaredBuffs :Seq[Buff[S]] = Nil

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
	protected def conveyBuffs[T](extractor :S =?> T, component: => String = "") :Buffs[T] =
		buffs.unsafeCascade(extractor)



	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FILTER, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(INSERT, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UPDATE, subject)

	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] = {
		val res = new IndexedColumnValuesBuilder
		writtenValues(op, subject, res) //StaticMapping delegates this to specific methods
		res.result()
	}

	private class IndexedColumnValuesBuilder extends ComponentValuesBuilder[S, O] {
		private[this] val values = new Array[Option[Any]](columnCount)
		private[this] var componentValues = Map.empty[RefinedMapping[_, O], Option[_]]

		override def addOpt[T](component :RefinedMapping[T, O], result :Opt[T]) :this.type = export(component) match {
			case col :MappingFrame[_, _]#FrameColumn[_] =>
				values(col.index) = result; this
			case _ =>
				componentValues = componentValues.updated(component, result)
				this
		}

		override def result() =
			if (componentValues.isEmpty)
				ColumnValues(ArraySeq.unsafeWrapArray(values)) {
					case column :MappingFrame[_, _]#FrameColumn[_] => column.index
					case _ => -1
				}
			else {
				var i = 0
				while (i < columnCount) {
					val value = values(i)
					if (value != null)
						componentValues = componentValues.updated(columnIndex(i), value)
					i += 1
				}
				ComponentValues(componentValues)
			}
	}



	override def export[T](component :Component[T]) :Component[T] = component match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[Component[T]]
		case self if self eq this => self
		case _ =>
			apply(component).export
	}

	override def export[T](column :Column[T]) :Column[T] = column match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[Column[T]]
		case _ =>
			apply(column).export
	}

	override def exportOrNot[T](component :Component[T]) :Component[T] = component match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[Component[T]]
		case _ =>
			val extract = extracts.getOrElse[Extract, T](component, null) //todo: make sure this doesn't create an object
			if (extract != null) extract.export else component
	}

	override def exportOrNot[T](column :Column[T]) :Column[T] = column match {
		case export :MappingFrame[_, _]#FrameColumn[_] if export belongsTo this =>
			export.asInstanceOf[Column[T]]
		case _ =>
			val extract = columnExtracts.getOrElse[ColumnExtract, T](column, null) //todo: make sure this doesn't create an object
			if (extract != null) extract.export else column
	}

	override def apply[T](component :Component[T]) :Extract[T] = component match {
		case export :MappingFrame[_, _]#FrameComponent[_] if export belongsTo this =>
			export.asInstanceOf[FrameComponent[T]].extract
		case _  =>
			extracts.getOrElse(component,
				throw new NoSuchComponentException(s"Component $component is not a part of mapping $this.")
			)
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
		case export :MappingFrame[_, _]#FrameColumn[_] if export belongsTo this =>
			export.asInstanceOf[FrameColumn[T]].extract
		case wonky :MappingFrame[_, _]#FrameComponent[_] if wonky belongsTo this =>
			wonky.extract.asInstanceOf[ColumnExtract[T]]
		case _ =>
			columnExtracts.getOrElse(column,
				throw new NoSuchComponentException(s"Column $column is not a part of mapping $this.")
			)
	}



	/** An hook point for subclasses to extend the functionality by providing a specialized extract
	  * for the given component, offering additional features. This method is called once for every component
	  * which is not a column (including subcomponents) when the component's initialization is being performed.
	  */
	protected def extractFor[T](component :FrameComponent[T]) :Extract[T] =
		MappingExtract[S, T, O](component)(component.componentSelector)

	/** A hook point for subclasses to extend the functionality by providing a specialized extract
	  * for the given column, offering additional features. This method is called once for every column
	  * (including export versions of non-direct columns) when the column's initialization is being performed.
	  */
	protected def extractFor[T](column :FrameColumn[T]) :ColumnExtract[T] =
		ColumnExtract[S, T, O](column)(column.componentSelector)


	private[this] var initExtracts = NaturalMap.empty[Component, Extract]
	@volatile private[this] var safeExtracts :NaturalMap[Component, Extract] = _
	private[this] var fastExtracts :NaturalMap[Component, Extract] = _

	final override def extracts :NaturalMap[Component, Extract] = {
		if (fastExtracts == null) {
			val initialized = safeExtracts
			if (initialized != null)
				fastExtracts = initialized
			else {
				initialize()
				fastExtracts = safeExtracts
			}
		}
		fastExtracts
	}

	@volatile private[this] var safeColumnExtracts :NaturalMap[Column, ColumnExtract] = _
	private[this] var fastColumnExtracts :NaturalMap[Column, ColumnExtract] = _

	final override def columnExtracts :NaturalMap[Column, ColumnExtract] = {
		if (fastColumnExtracts == null) {
			val initialized = safeColumnExtracts
			if (initialized != null)
				fastColumnExtracts = initialized
			else {
				initialize()
				fastColumnExtracts = safeColumnExtracts
			}
		}
		fastColumnExtracts
	}




	/** A sequence which works as a mutable buffer during initialization, but once it is completed requires
	  * no synchronization. All appends must be synchronized with the enclosing class's lock, immediately
	  * followed by a get of `isInitialized`, signifying that no further modification can be done
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
					s"Can't enlist component $comp for $frame: the list of components has been already exported.")
			else
				uninitialized += comp

		def isInitialized :Boolean = initialized != null

		def initialize() :Unit = {
			cached = uninitialized.result().to(Unique)
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
				frame.initialize()
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
	private[this] final val initFilterable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initInsertable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initUpdatable = new LateInitComponents[FrameColumn[_]]
	private[this] final val initAutoInsert = new LateInitComponents[FrameColumn[_]]
	private[this] final val initAutoUpdate = new LateInitComponents[FrameColumn[_]]
	private[this] final val initSelectedByDefault = new LateInitComponents[FrameColumn[_]]
	private[this] final val initFilteredByDefault = new LateInitComponents[FrameColumn[_]]
	private[this] final val initInsertedByDefault = new LateInitComponents[FrameColumn[_]]
	private[this] final val initUpdatedByDefault = new LateInitComponents[FrameColumn[_]]

	final override def components :Unique[Component[_]] = initComponents.items
	final override def subcomponents :Unique[Component[_]] = initSubcomponents.items
	final override def columns :Unique[Column[_]] = initColumns.items
	final override def selectable :Unique[Column[_]] = initSelectable.items
	final override def filterable :Unique[Column[_]] = initFilterable.items
	final override def insertable :Unique[Column[_]] = initInsertable.items
	final override def updatable :Unique[Column[_]] = initUpdatable.items
	final override def autoInserted :Unique[Column[_]] = initAutoInsert.items
	final override def autoUpdated :Unique[Column[_]] = initAutoUpdate.items
	final override def selectedByDefault :Unique[Column[_]] = initSelectedByDefault.items
	final override def filteredByDefault :Unique[Column[_]] = initFilteredByDefault.items
	final override def insertedByDefault :Unique[Column[_]] = initInsertedByDefault.items
	final override def updatedByDefault :Unique[Column[_]] = initUpdatedByDefault.items



	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override.
	  * Overrides with a `val` or `var` (or any used in their implementation) must happen ''before'' any components
	  * of this mapping are initialized - in practice before any column declarations.
	  */
	protected override def columnPrefix :String = ""

	private final def verifiedPrefix :String = columnPrefix match {
		case null => throw new IllegalStateException(
			s"$this.columnPrefix is null: override with a val must happen before any component declarations.")
		case prefix => prefix
	}


	/** Includes a column belonging to this instance on all appropriate column lists declared in `Mapping`. */
	private def includeColumn[T](column :FrameColumn[T]) :FrameColumn[T] = synchronized {
		if (isInitialized)
			throw new IllegalStateException(
				s"Cannot initialize column $column of $this: components have already been exported. Create components eagerly!"
			)

		initSubcomponents += column
		initColumns += column
		initColumnIndex += column
		if (NoSelect.inactive(column)) initSelectable += column
		if (NoFilter.inactive(column)) initFilterable += column
		if (NoUpdate.inactive(column)) initUpdatable += column
		if (NoInsert.inactive(column)) initInsertable += column
		if (AutoInsert.active(column)) initAutoInsert += column
		if (AutoUpdate.active(column)) initAutoUpdate += column
		if (NoSelectByDefault.inactive(column)) initSelectedByDefault += column
		if (NoFilterByDefault.inactive(column)) initFilteredByDefault += column
		if (NoUpdateByDefault.inactive(column)) initUpdatedByDefault += column
		if (NoInsertByDefault.inactive(column)) initInsertedByDefault += column

		column
	}


	private[this] final var initColumnIndex = new ListBuffer[FrameColumn[_]]

	/** Contains the column with `index = n` at `n`-th position. */
	private[this] var columnIndex :Array[FrameColumn[_]] = _

	/** Number of columns ''created'' (not necessarily currently contained) by this instance.
	  * Increased and assigned to a column as soon as it is created.
	  */
	private[this] var initColumnCount = 0
	private[this] var columnCount :Int = _

	private def nextColumnIndex() :Int = synchronized {
		if (isInitialized)
			throw new IllegalStateException(s"Can't add another column to $this: the mapping has already been initialized.")
		val res = initColumnCount; initColumnCount += 1; res
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
	  *
	  * @throws IllegalStateException if reentered while a another call to `initPreceding` is present on the stack
	  *                               as a result of being called from initialization code of a component or
	  *                               if the mapping is already initialized.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.initPreceding[T](value:T) initPreceding(value)]]
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.FrameComponent.include() FrameComponent.include()]]
	  */
	protected final def initPreceding() :Unit = synchronized {
		while (initQueue.nonEmpty) { //initialized components may enqueue new components for initialization
			if (initializationState == 2)
				throw new IllegalStateException(
					s"Cannot initialize component lists of $this: the mapping initialization is completed.")
			if (initializationState == 1)
				throw new IllegalStateException(
					s"Cannot initialize component lists of $this: initPreceding is already in progress.")
			initializationState = 1
			val elems = initQueue //guard against ConcurrentModificationException
			initQueue = ListBuffer.empty
			elems.foreach(_.include())
			initializationState = 0
		}
	}

	/** An ease-of-life variant of no-argument `initPreceding()` which returns the given argument after finishing
	  * the initialization of all already defined components. It allows to 'inject' the initialization as an intermediate
	  * step into any expression.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.initPreceding() initPreceding()]]
	  */
	@inline protected final def initPreceding[T](value :T) :T = { initPreceding(); value }

	/** Keeps components which have been created, but not initialized (their columns and components have not been
	  * enlisted with this instance). Modified only under synchronization lock of this instance, its contents
	  * are flushed either when initialization occurs (a call to `initialize()`, either explicit or due to accessing
	  * a column or component list of this instance), or when a new component (typically a column) is created
	  * and needs to be initialized, in order to preserve relative order of declarations in the column/component
	  * lists.
	  */
	private[this] var initQueue = new ListBuffer[FrameComponent[_]]
	private[this] var lateInit = new ListBuffer[FrameComponent[_]]

	//0 - uninitialized; 1 (temporary) initPreceding in progress; 2 - initialized
	@volatile private[this] var initializationState = 0

	/** Tests if the component lists of this instance have already been initialized and can not be changed any further.
	  * This happens either when any of the column/component lists of this instance are accessed, or when a subclass
	  * manually triggers the initialization by a call to
	  * [[net.noresttherein.oldsql.schema.bases.MappingFrame.initialize() initialize()]].
	  * Any attempt to create new components after this happens will result in `IllegalStateException` thrown
	  * by the constructor.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.initialize() initialize()]]
	  */
	protected final def isInitialized :Boolean = initializationState == 2

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
	  * @throws IllegalStateException if called when the initialization is already in progress, but has not completed.
	  *                               This can be in particular caused by calling from initialization of a component.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.finalizeInitialization()]]
	  */
	protected final def initialize() :Unit =
		if (!isInitialized) synchronized {
			if (!isInitialized) {
				initPreceding()
				while (lateInit.nonEmpty) {
					val elems = lateInit
					lateInit = ListBuffer.empty
					elems.foreach(_.include())
					initPreceding()
				}

				initComponents.initialize()
				initColumns.initialize()
				initSelectable.initialize()
				initFilterable.initialize()
				initUpdatable.initialize()
				initInsertable.initialize()
				initAutoInsert.initialize()
				initAutoUpdate.initialize()
				initSelectedByDefault.initialize()
				initFilteredByDefault.initialize()
				initUpdatedByDefault.initialize()
				initInsertedByDefault.initialize()
				initSubcomponents.initialize()

				columnIndex = initColumnIndex.toArray
				initColumnIndex = null
				columnCount = initColumnCount

				fastExtracts = initExtracts
				safeExtracts = fastExtracts
				initExtracts = null

				fastColumnExtracts = schema.filterColumnExtracts(this)(fastExtracts)
				safeColumnExtracts = fastColumnExtracts

				finalizeInitialization()
				initializationState = 2

				oldsql.publishMutable()
			}
		}

	/** A hook method called by `initialize()` after component lists are initialized. By default it does nothing,
	  * but subclasses may override it to implement any additional initialization steps they require.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.initialize() initialize()]]
	  */
	protected def finalizeInitialization() :Unit = ()



	private class ReadForm(columns :Unique[ColumnMapping[_, O]],
	                       read :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
		extends SQLReadForm[S] with ReadFormNullValue[S]
	{
		private[this] val fastColumns = columns.toArray
		private val forms :Array[SQLReadForm[_]] = fastColumns.map(read)
		override val readColumns: Int = fastColumns.length //(0 /: forms)(_ + _.readColumns)

		override def opt(res :ResultSet, position :Int): Opt[S] = {
			val values = new Array[Any](columnCount)
			java.util.Arrays.fill(values.asInstanceOf[Array[AnyRef]], None)

			var i = position
			val end = position + fastColumns.length
			while (i < end) fastColumns(i) match {
				case c :MappingFrame[_, _]#FrameColumn[_] =>
					i += 1; values(c.index) = forms(i).opt(res, i - 1).orNull
				case c =>
					throw new IllegalArgumentException(s"Non-export column $c passed to SQLReadForm $this of $frame.")
			}

			val pieces = ComponentValues(frame)(ArraySeq.unsafeWrapArray(values)) {
				case column :MappingFrame[_, _]#FrameColumn[_] => column.index
				case _ => -1
			}
			frame.optionally(pieces)
		}

		override def nulls :NullValue[S] = frame.nullValue


		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = 0; val end = fastColumns.length
			while (i < end) {
				forms(i).register(call, position + i)
				i += 1
			}
		}

		override def toString :String = columns.map(read).mkString(s"$frame{", ",", "}>")
	}



	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		//_.selectable aren't export components, but should have correct column names
		val columns = components.map(frame.export(_)).flatMap(_.selectedByDefault)

		if (columns.exists(NoSelect.active))
			throw new IllegalArgumentException(
				s"Can't create a select form for $frame using $components: NoSelect buff present among the selection.")

		if (selectable.exists(c => !columns.contains(c) && OptionalSelect.inactive(c))) {
			val missing = (selectable.toSet -- columns.toSet).filter(OptionalSelect.inactive)
			throw new IllegalArgumentException(
				missing.mkString(
					s"Can't create a select form for $frame using $components: missing mandatory columns ", ", ", ""
				)
			)
		}

		val extra = columns ++ ExtraSelect.Active.columns(frame)
		new ReadForm(columns :++ extra)
	}

	override val selectForm :SQLReadForm[S] = SQLReadForm.delayed(new ReadForm(selectedByDefault))
	override val filterForm :SQLWriteForm[S] = SQLWriteForm.delayed(super.filterForm)
	override val insertForm :SQLWriteForm[S] = SQLWriteForm.delayed(super.insertForm)
	override val updateForm :SQLWriteForm[S] = SQLWriteForm.delayed(super.updateForm)
	override def writeForm(op :WriteOperationType) :SQLWriteForm[S] = op.form(this)

}

