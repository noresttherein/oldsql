package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationView.WriteOperationView
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.{SpecificExtract, Mapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{BuffType, FilterAudit, FilterPreset, InsertAudit, InsertDefault, InsertPreset, NoFilter, NoInsert, NoUpdate, SelectAudit, SelectDefault, SelectPreset, UpdateAudit, UpdateDefault, UpdatePreset}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.schema.bases.LazyMapping.LazyMappingTemplate
import net.noresttherein.oldsql.schema.bases.StableMapping.StableMappingTemplate






/** A `Mapping` mixin trait which caches buff information in member fields and overrides the methods for assembly
  * and disassembly taking advantage of these flags.
  */ //todo: move to Mapping, just like OptimizedColumn; todo: buffs?
trait OptimizedMappingAssembly extends Mapping {

	override def optionally(pieces :Pieces) :Opt[Subject] = pieces.assemble(refine) match {
		case res if buffs.isEmpty => res
		case res if res.isDefined => selectAudit match {
			case Got(audit) => Got(audit(res.get))
			case _ => res
		}
		case _ => selectDefault
	}
	//consider: having pseudo lazy, *final* class implementing optionally in a non-volatile field
	private lazy val selectAudit   = if (SelectAudit.active(refine)) Got(SelectAudit.fold(refine)) else Lack
	private lazy val selectDefault = SelectDefault.Value(refine) orElse SelectPreset.Value(refine)


	override def writtenValues[T](op :WriteOperationView, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		op.writtenValues(refine, subject, collector)

	override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isFilterable) {
			val audited = filterAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.filterValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isInsertable) {
			val audited = insertAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.insertValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
		if (isUpdatable) {
			val audited = updateAudit(subject)
			def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
				case Got(value) => comp.updateValues(value, collector)
				case _ =>
			}
			components foreach { c :Component[_] => componentValues(c) }
		}

	private lazy val isFilterable  = NoFilter.inactive(this)
	private lazy val isInsertable  = NoInsert.inactive(this)
	private lazy val isUpdatable   = NoUpdate.inactive(this)
	private lazy val filterAudit   = FilterAudit.fold(refine)
	private lazy val insertAudit   = InsertAudit.fold(refine)
	private lazy val updateAudit   = UpdateAudit.fold(refine)
//	private lazy val insertDefault = InsertDefault.Value(refine)
//	private lazy val updateDefault = UpdateDefault.Value(refine)

}






/** A convenience base trait for simple mappings which overrides a selection of `Mapping` methods for efficiency,
  * storing often used values in lazy member fields.
  * The implemented methods include forms, column, component and extract lists (except for `components`, `extracts`
  * by filtering the result of the abstract method `columns` based on their applied buffs, but also `optionally`
  * and the `writtenValues` family of methods to benefit from stored buff information.
  * Laziness is dictated by the fact that properties `columns`, `extracts` and `buffs` may be uninitialized
  * before this thread's initialization.
  * @see [[net.noresttherein.oldsql.schema.bases.StableMapping]]
  */
trait LazyMapping[S, O] extends LazyMappingTemplate[TypedMapping, TypedColumn, S, O]



object LazyMapping {

	/** A convenience base trait for simple mappings which overrides a selection of `Mapping` methods for efficiency,
	  * storing often used values in lazy member fields.
	  * The implemented methods include forms, column, component and extract lists (except for `components`, `extracts`
	  * by filtering the result of the abstract method `columns` based on their applied buffs, but also `optionally`
	  * and the `writtenValues` family of methods to benefit from stored buff information.
	  * Laziness is dictated by the fact that properties `columns`, `extracts` and `buffs` may be uninitialized
	  * before this thread's initialization.
	  * @tparam Comp A type constructor for a `Mapping` subtype being a supertype of this mapping and
	  *              all its ''export'' components. Its type parameters define
	  *              the component's [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]]
	  *              and [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] types, respectively.
	  * @tparam Col A type constructor for a `TypedColumn` subtype being a supertype of all ''export'' columns.
	  * @see [[net.noresttherein.oldsql.schema.bases.LazyMapping]]
	  * @see [[net.noresttherein.oldsql.schema.bases.StableMapping]]
	  */
	trait LazyMappingTemplate[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], S, O]
		extends BaseMapping[S, O] with MappingTemplate[Comp, Col] with OptimizedMappingAssembly
	{
		private lazy val lazySelectForm :SQLReadForm[S]  = super.selectForm
		private lazy val lazyFilterForm :SQLWriteForm[S] = super.filterForm
		private lazy val lazyInsertForm :SQLWriteForm[S] = super.insertForm
		private lazy val lazyUpdateForm :SQLWriteForm[S] = super.updateForm

		override def selectForm: SQLReadForm[S]  = lazySelectForm
		override def filterForm: SQLWriteForm[S] = lazyFilterForm
		override def insertForm: SQLWriteForm[S] = lazyInsertForm
		override def updateForm: SQLWriteForm[S] = lazyUpdateForm

		override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] =
			if (components == selectedByDefault) selectForm else super.selectForm(components)

		override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == filteredByDefault) filterForm else super.filterForm(components)

		override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == insertedByDefault) insertForm else super.insertForm(components)

		override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[S] =
			if (components == updatedByDefault) updateForm else super.updateForm(components)


		private lazy val lazySelfExtract    :SpecificExtract[Comp[S, O], S, S, O] = SpecificExtract.ident(refine)

		private lazy val lazyColumnExtracts :like[Col]#ColumnExtractMap =
			schema.filterColumnExtracts(refine)(extracts).asInstanceOf[like[Col]#ColumnExtractMap]

		protected override def selfExtract    :SpecificExtract[Comp[S, O], S, S, O] = lazySelfExtract

		override def columnExtracts :NaturalMap[Column, like[Col]#Extract] = lazyColumnExtracts

		private lazy val lazySubcomponents :Unique[Comp[_, Origin]] =
			components.flatMap { c => export(c) +: c.components.map(export(_)) }

		//columns of a column is the column itself, so works for direct columns, too
		private lazy val lazyColumns           :Unique[Col[_, O]] = components.flatMap(_.columns).map(export(_))
		private lazy val lazySelectable        :Unique[Col[_, O]] = super.selectable
		private lazy val lazyFilterable        :Unique[Col[_, O]] = super.filterable
		private lazy val lazyInsertable        :Unique[Col[_, O]] = super.insertable
		private lazy val lazyUpdatable         :Unique[Col[_, O]] = super.updatable
		private lazy val lazyAutoInserted      :Unique[Col[_, O]] = super.autoInserted
		private lazy val lazyAutoUpdated       :Unique[Col[_, O]] = super.autoUpdated
		private lazy val lazySelectedByDefault :Unique[Col[_, O]] = super.selectedByDefault
		private lazy val lazyFilteredByDefault :Unique[Col[_, O]] = super.filteredByDefault
		private lazy val lazyInsertedByDefault :Unique[Col[_, O]] = super.insertedByDefault
		private lazy val lazyUpdatedByDefault  :Unique[Col[_, O]] = super.updatedByDefault
		private lazy val lazyMandatorySelect   :Unique[Col[_, O]] = super.mandatorySelect
		private lazy val lazyMandatoryFilter   :Unique[Col[_, O]] = super.mandatoryFilter
		private lazy val lazyMandatoryInsert   :Unique[Col[_, O]] = super.mandatoryInsert
		private lazy val lazyMandatoryUpdate   :Unique[Col[_, O]] = super.mandatoryUpdate
		private lazy val lazySelectPreset      :Unique[Col[_, O]] = super.columnsWith(SelectPreset)
		private lazy val lazyFilterPreset      :Unique[Col[_, O]] = super.columnsWith(FilterPreset)
		private lazy val lazyInsertPreset      :Unique[Col[_, O]] = super.columnsWith(InsertPreset)
		private lazy val lazyUpdatePreset      :Unique[Col[_, O]] = super.columnsWith(UpdatePreset)


		override def subcomponents     :Unique[Comp[_, O]] = lazySubcomponents
		override def columns           :Unique[Col[_, O]]  = lazyColumns
		override def selectable        :Unique[Col[_, O]]  = lazySelectable
		override def filterable        :Unique[Col[_, O]]  = lazyFilterable
		override def insertable        :Unique[Col[_, O]]  = lazyInsertable
		override def updatable         :Unique[Col[_, O]]  = lazyUpdatable
		override def autoInserted      :Unique[Col[_, O]]  = lazyAutoInserted
		override def autoUpdated       :Unique[Col[_, O]]  = lazyAutoUpdated
		override def selectedByDefault :Unique[Col[_, O]]  = lazySelectedByDefault
		override def filteredByDefault :Unique[Col[_, O]]  = lazyFilteredByDefault
		override def insertedByDefault :Unique[Col[_, O]]  = lazyInsertedByDefault
		override def updatedByDefault  :Unique[Col[_, O]]  = lazyUpdatedByDefault
		override def mandatorySelect   :Unique[Col[_, O]]  = lazyMandatorySelect
		override def mandatoryFilter   :Unique[Col[_, O]]  = lazyMandatoryFilter
		override def mandatoryInsert   :Unique[Col[_, O]]  = lazyMandatoryInsert
		override def mandatoryUpdate   :Unique[Col[_, O]]  = lazyMandatoryUpdate

		override def columnsWith(buff :BuffType) :Unique[Col[_, O]] = buff match {
			case SelectPreset => lazySelectPreset
			case FilterPreset => lazyFilterPreset
			case InsertPreset => lazyInsertPreset
			case UpdatePreset => lazyUpdatePreset
			case _ => super.columnsWith(buff)
		}

		private lazy val lazyColumnMap :Map[String, Col[_, Origin]] =
			columns.view.map { c => (c.name, c) }.toMap

		override def columnNamed(name :String) :Col[_, O] =
			lazyColumnMap.getOrElse(name, null.asInstanceOf[Col[_, O]]) match {
				case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
				case res => res
			}

		override def apply[T](component :Component[T]) :SpecificExtract[Comp[T, O], S, T, O] =
			if (component eq this) lazySelfExtract.asInstanceOf[like[Comp]#Extract[T]]
			else extracts(component)

		override def apply[T](column :Column[T]) :SpecificExtract[Col[T, O], S, T, O] =
			columnExtracts(column)
	}

}






/** A stackable mixin trait which defines all column and component lists as `val`s initialized by the call to super.
  * It caches also extracts and some buff information required by `optionally` to provide a faster implementation.
  * It must come in linearization order after the final definitions of all standard `Mapping` properties,
  * in particular buffs, all column, component and extract lists, which are in most cases overridden here
  * as `abstract override` methods. The implementations are very similar to those in
  * [[net.noresttherein.oldsql.schema.bases.LazyMapping LazyMapping]], the difference between the two being
  * that this trait is not really considered a base trait for extension by custom `Mapping` classes,
  * as the precomputed values are not lazy and would be referenced before the initialization of the extending class.
  *
  * Note that such buffs should not contain foreign key or inverse foreign key components, as eager initialization
  * accesses the column lists of all components, which can lead to the component accessing the referenced table mapping
  * before its initialization, due to likely existence of cycles within relationships. It is thus best used as a mixin
  * for concrete mapping classes dedicated to components in the application's domain model,
  * and not with general purpose abstract implementations.
  * @see [[net.noresttherein.oldsql.schema.bases.LazyMapping]]
  */ //fixme: this probably should be removed because it's incompatible with lazy components in a foreign key
trait StableMapping extends StableMappingTemplate[TypedMapping, TypedColumn]


object StableMapping {

	trait StableMappingTemplate[+Comp[T, Q] <: TypedMapping[T, Q], +Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		extends Mapping with MappingTemplate[Comp, Col]
	{
		override def optionally(pieces :Pieces) :Opt[Subject] = pieces.assemble(refine) match {
			case res if noBuffs => res
			case res if res.isDefined => if (noSelectAudit) res else Got(selectAudit(res.get))
			case _ => selectDefault
		}


		override def writtenValues[T](op :WriteOperationView, subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
			op.writtenValues(refine, subject, collector)

		override def filterValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
			if (isFilterable) {
				val audited = filterAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
					case Got(value) => comp.filterValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		override def insertValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
			if (isInsertable) {
				val audited = insertAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
					case Got(value) => comp.insertValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		override def updateValues[T](subject :Subject, collector :ComponentValuesBuilder[T, Origin]) :Unit =
			if (isUpdatable) {
				val audited = updateAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).opt(audited) match {
					case Got(value) => comp.updateValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		private val isFilterable = NoFilter.inactive(this)
		private val isInsertable = NoInsert.inactive(this)
		private val isUpdatable  = NoUpdate.inactive(this)
		private val filterAudit  = FilterAudit.fold(refine)
		private val insertAudit  = InsertAudit.fold(refine)
		private val updateAudit  = UpdateAudit.fold(refine)

		private val noBuffs       = buffs.isEmpty
		private val noSelectAudit = SelectAudit.inactive(this)
		private val selectAudit   = SelectAudit.fold(refine)
		private val selectDefault = SelectDefault.Value(refine)


		override def columnNamed(name :String) :Col[_, Origin] =
			columnMap.getOrElse(name, null.asInstanceOf[Col[_, Origin]]) match {
				case null => throw new NoSuchComponentException("No column '" + name + "' in " + this + ".")
				case res => res
			}

		protected override val selfExtract   :SpecificExtract[Comp[Subject, Origin], Subject, Subject, Origin] =
			super.selfExtract
		//	private val selfExtract = extracts.getOrElse(refine, MappingExtract.ident(refine))

		//consider: extracting these to another trait so that super properties may be vals.
		// They would have to be initialized before this trait's initialization.
		//It is unsafe to have them here as they may delegate to each other in an unspecified direction,
		// and if it happens to be different than declaration order then the accessed val will be null.
//		abstract override val extracts       :NaturalMap[Component, like[Comp]#Extract] = super.extracts
//		abstract override val columnExtracts :NaturalMap[Column, like[Col]#Extract]     = super.columnExtracts
//
//		abstract override val subcomponents :Unique[Comp[_, Origin]] = super.subcomponents
//		abstract override val components    :Unique[Comp[_, Origin]] = super.components
//		abstract override val columns       :Unique[Col[_, Origin]]  = super.columns

		override val selectable        :Unique[Col[_, Origin]] = super.selectable
		override val filterable        :Unique[Col[_, Origin]] = super.filterable
		override val insertable        :Unique[Col[_, Origin]] = super.insertable
		override val updatable         :Unique[Col[_, Origin]] = super.updatable
		override val autoInserted      :Unique[Col[_, Origin]] = super.autoInserted
		override val autoUpdated       :Unique[Col[_, Origin]] = super.autoUpdated
		override val selectedByDefault :Unique[Col[_, Origin]] = super.selectedByDefault
		override val filteredByDefault :Unique[Col[_, Origin]] = super.filteredByDefault
		override val insertedByDefault :Unique[Col[_, Origin]] = super.insertedByDefault
		override val updatedByDefault  :Unique[Col[_, Origin]] = super.updatedByDefault
		override val mandatorySelect   :Unique[Col[_, Origin]] = super.mandatorySelect
		override val mandatoryFilter   :Unique[Col[_, Origin]] = super.mandatoryFilter
		override val mandatoryInsert   :Unique[Col[_, Origin]] = super.mandatoryInsert
		override val mandatoryUpdate   :Unique[Col[_, Origin]] = super.mandatoryUpdate
		private  val selectPresets     :Unique[Col[_, Origin]] = super.columnsWith(SelectPreset)
		private  val filterPresets     :Unique[Col[_, Origin]] = super.columnsWith(FilterPreset)
		private  val insertPresets     :Unique[Col[_, Origin]] = super.columnsWith(InsertPreset)
		private  val updatePresets     :Unique[Col[_, Origin]] = super.columnsWith(UpdatePreset)

		override def columnsWith(buff :BuffType) :Unique[Col[_, Origin]] = buff match {
			case SelectPreset => selectPresets
			case FilterPreset => filterPresets
			case InsertPreset => insertPresets
			case UpdatePreset => updatePresets
			case _ => super.columnsWith(buff)
		}

		private val columnMap :Map[String, Col[_, Origin]] = columns.view.map { c => (c.name, c) }.toMap

		abstract override val selectForm :SQLReadForm[Subject]  = super.selectForm
		abstract override val filterForm :SQLWriteForm[Subject] = super.filterForm
		abstract override val insertForm :SQLWriteForm[Subject] = super.insertForm
		abstract override val updateForm :SQLWriteForm[Subject] = super.updateForm


		abstract override def selectForm(components :Unique[Component[_]]) :SQLReadForm[Subject] =
			if (components == selectedByDefault) selectForm else super.selectForm(components)

		abstract override def filterForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
			if (components == filteredByDefault) filterForm else super.filterForm(components)

		abstract override def insertForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
			if (components == insertedByDefault) insertForm else super.insertForm(components)

		abstract override def updateForm(components :Unique[Component[_]]) :SQLWriteForm[Subject] =
			if (components == updatedByDefault) updateForm else super.updateForm(components)
	}
}

