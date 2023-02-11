package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.Opt.Lack
import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.morsels.Stateless
import net.noresttherein.oldsql.schema.{Buff, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Buff.{AbstractValueBuff, AuditBuffType, BuffType, CustomInsert, CustomSQLBuff, CustomSQLBuffType, CustomUpdate, ExplicitFilter, ExplicitInsert, ExplicitSelect, ExplicitUpdate, ExtraFilter, ExtraInsert, ExtraSelect, ExtraUpdate, FilterAudit, FilterPreset, FlagBuffType, InactiveBuffType, InsertAudit, InsertDefault, InsertPreset, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalFilter, OptionalInsert, OptionalSelect, OptionalUpdate, SelectAudit, SelectDefault, SelectPreset, SpecificBuffType, SQLBuffType, UpdateAudit, UpdateDefault, UpdatePreset, ValueBuffType}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, MappingTemplate, TypedMapping}
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope
import net.noresttherein.oldsql.sql.SQLDialect.SpellingScope.{InsertScope, SelectScope, UpdateScope, WhereScope}






/** A marker identifying a context of use of a [[net.noresttherein.oldsql.schema.Mapping Mapping]] and determining
  * the exact column set to use for any mapping. The inclusion of a column/component is driven by presence of
  * related [[net.noresttherein.oldsql.schema.Buff.BuffType buff types]]. It lists the buff types specific
  * to the given operation and provides methods delegating to one of the variants declared by the mapping,
  * specific to this operation type. It allows the use of mappings in SQL expressions abstracting over
  * their intended usage and generalize code for ''select'', ''update'' and ''insert'', as well as the use of a mapping
  * in a ''where'' clause of an SQL query.
  * Standard implementations correspond to main SQL operation types:
  * [[net.noresttherein.oldsql.OperationView.SelectView select]],
  * [[net.noresttherein.oldsql.OperationView.InsertView insert]]
  * [[net.noresttherein.oldsql.OperationView.UpdateView update]] as well as
  * [[net.noresttherein.oldsql.OperationView.FilterView where clause]] (and all other contexts in which a mapping
  * is compared column-wise with another expression). Other implementations are however also possible.
  * @author Marcin Mościcki
  */ //consider: renaming to OperationColumns/MappingProjection/OperationProjection/ColumnsView/MappingView/MappingProjection
trait OperationView { //todo: CallView/ArgumentView

	/** A buff marking a column/component as not allowed in a given operation type or, more specifically,
	  * that the column is not used in this statement type as a part of the mapping - it can still be used by the
	  * framework explicitly (see [[net.noresttherein.oldsql.OperationView.Preset Preset]]. Example: `NoSelect`.
	  */
	val Prohibited :FlagBuffType

	/** A buff marking a column/component as not included by default in the operation, but possibly still allowed
	  * if included explicitly. It is implied by both [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]]
	  * and [[net.noresttherein.oldsql.OperationView.Explicit Explicit]]. Example: `NoSelectByDefault`.
	  */
	val NonDefault :BuffType

	/** A buff implying [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]], used internally
	  * in order to flag an
	  * [[net.noresttherein.oldsql.OperationView.Optional Optional]]/[[net.noresttherein.oldsql.OperationView.Explicit Explicit]]
	  * component for being excluded from the operation. Should not be used in the application code in order
	  * to statically mark optional components - use one of the former buff types instead.
	  */
	val Exclude :FlagBuffType

	/** A buff marking that a column/component must be included in the operation explicitly, as it is not included
	  * by the mapping in the standard process. This buff implies
	  * [[net.noresttherein.oldsql.OperationView.Optional Optional]] and
	  * [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]]. Example: `ExplicitSelect` for CLOB/BLOB types.
	  */
	val Explicit :BuffType

	/** A buff marking that a column/component can be excluded from this operation, but it must happen explicitly,
	  * as it is included by default. Example `OptionalSelect`.
	  */
	val Optional :BuffType

	/** A synthetic buff having an opposite meaning to [[net.noresttherein.oldsql.OperationView.Optional Optional]].
	  * There is no need to use it under normal circumstances, as simply lack of `Optional` buff will make the component
	  * mandatory for this operation, but it is useful for cancelling any or all preexisting `Optional` buffs, including
	  * those inherited from enclosing components retroactively.
	  */
	val Mandatory :FlagBuffType

	/** A buff carrying a transforming function working on the mapping subject type, which must be applied to
	  * every value written or read by this operation. For example, `InsertAudit` and `UpdateAudit` can be used
	  * to implement validation of the data before it is written to the database.
	  */
	val Audit :AuditBuffType

	/** A buff carrying a default value which should be inserted/updated if the written entity is missing one
	  * for the component (in write operations) or returned if none can be assembled (when selecting). */
	val Default :ValueBuffType

	/** A buff marking a column/component as not used as part of the owning mapping, but forcibly always included
	  * by the framework, with the value specified by the buff instance. For example, `FilterPreset` will apply
	  * an additional filter on the queried table or view. It always implies
	  * [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]]
	  * and [[net.noresttherein.oldsql.OperationView.Default Default]].
	  */
	val Preset :ValueBuffType

	/** A buff implying [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]], but providing
	  * an SQL [[net.noresttherein.oldsql.sql.SQLExpression expression]] to be used for the column value
	  * instead of using a statement parameter or selected from a table.
	  */
	val Extra :SQLBuffType

	//fixme: this buff plain can't work. When spelling, we have no value for the annotated column.
	//  even if we managed to introduce it to Parameterization, we would have to wait with the whole spelling
	//  process until the application of an Incantation. Very inefficient.
	/** A buff type implying [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] and providing
	  * an SQL expression to use instead of the column value extracted from an entity.
	  * It is very similar to [[net.noresttherein.oldsql.OperationView.Extra Extra]],
	  * but the expression is not a constant, but instead created by a function of the annotated column value.
	  */ //there is no CustomFilter and there clashes when mixing in an InactiveBuffType to CustomSQLBuffType
	val Custom :SpecificBuffType[CustomSQLBuff]

	/** Spelling scope most closely representing this operation type. While nested expressions (such as SQL ''selects''
	  * can use different scopes, this is the root one, used for spelling top-level statements.
	  */
	val spellingScope :SpellingScope

	/** All columns, direct or indirect, of the given mapping which are applicable to this operation type.
	  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff.
	  *
	  * Note that (export) columns of a component of a mapping `M` are not necessarily their export versions
	  * for mapping `M`; for this reason, this method should generally be used only for top-level mappings
	  * (such as table mappings), unless this identity is otherwise guaranteed, or only preserved column properties
	  * (such as `column.`[[net.noresttherein.oldsql.schema.ColumnMapping.form form]]) are used. For proper components
	  * consider using `columns(root, mapping)` instead.
	  * @see [[net.noresttherein.oldsql.OperationView.defaultColumns]]
	  */
	def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
	                     (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]]

	/** All export columns of `component`, direct or indirect, which are applicable to this operation type
	  * from the point of view of the containing mapping `mapping`.
	  * These are all columns from `component.`[[net.noresttherein.oldsql.schema.Mapping.columns columns]] list
	  * ''without'' the [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff after exporting
	  * by `mapping`.
	  * @see [[net.noresttherein.oldsql.OperationView.defaultColumns]]
	  */
	def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
	                     (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]]

	/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
	  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff. Any `NonDefault` buff on `mapping`
	  * itself is ignored.
	  *
	  * Note that (export) columns of a component of a mapping `M` are not necessarily their export versions
	  * for mapping `M`; for this reason, this method should generally be used only for top-level mappings
	  * (such as table mappings), unless this identity is otherwise guaranteed, or only preserved column properties
	  * (such as `column.`[[net.noresttherein.oldsql.schema.ColumnMapping.form form]]) are used. For proper components
	  * consider using `defaultColumns(root, mapping)` instead.
	  * @see [[net.noresttherein.oldsql.OperationView.applicableColumns]]
	  */ //todo: validate it is used only for 'root'/table mappings
	def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
	                  (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]]

	/** Export - from the point of view of `root` - columns of `component`, both direct and indirect,
	  * which are used by default in this operation type. These are all columns from
	  * `component.`[[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buff after exporting by `root`.
	  * However, if `component` itself - in its export version for `root` - has a `NonDefault` buff,
	  * it will be ignored by this method, as if it was included in `root` by calling `root(component.+)`.
	  * @see [[net.noresttherein.oldsql.OperationView.applicableColumns]]
	  */
	def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
	                  (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]]

	//todo: implement these methods in Mapping
	def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
	                    (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
		defaultColumns(mapping).filter(Optional.inactive(_))

	def mandatoryColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
	                    (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]] =
		defaultColumns(root, component).filter(Optional.inactive(_))

	/** Creates a version of the given mapping with buffs manipulated in such a way that the specified components
	  * are included or excluded from this operation type by default. It can only include components which are
	  * allowed for this operation type (have the `Explicit` buff) and exclude those which are specifically marked
	  * as optional with the `Optional` buff.
	  */
	def alter[S, O](mapping :TypedMapping[S, O],
	                include :Iterable[TypedMapping[_, O]],
	                exclude :Iterable[TypedMapping[_, O]] = Nil) :TypedMapping[S, O]

	//todo: implement this in Mapping
	def alter[S, O](mapping :TypedMapping[S, O], columns :Unique[TypedColumn[_, O]]) :TypedMapping[S, O] = ???
}






object OperationView {
	//todo: we need CallView/ParamView
	/** Interface of SQL operation types reading and assembling objects from the database. Provides access to
	  * operation-specific forms and narrows down the types of several buffs. The only current subclass is
	  * [[net.noresttherein.oldsql.OperationView.SelectView SelectView]]. */
	trait ReadOperationView extends OperationView {
		override val Explicit :ValueBuffType
		override val Optional :ValueBuffType

		/** Default read form (defining the column set) for this SQL operation returning values of `S` mapped by
		  * the given mapping. For [[net.noresttherein.oldsql.OperationView.SelectView SelectView]]
		  * it is `mapping.`[[net.noresttherein.oldsql.schema.Mapping.selectForm selectForm]].
		  */
		def form[S](mapping :MappingOf[S]) :SQLReadForm[S]

		/** A read form of the given `mapping` consisting of all default (export) columns of the mapping's
		  * ''export'' versions of the passed components. The given `components` collection must cover all columns
		  * which should be returned by this operation and not consist solely of the optional columns, which would
		  * not be used by method [[net.noresttherein.oldsql.OperationView.ReadOperationView.form[S](mapping* form]]`(mapping)`.
		  * The column set is defined by flat mapping the results of
		  * `mapping.`[[net.noresttherein.oldsql.schema.Mapping.selectedByDefault(component:* selectedByDefault]]`(component)`
		  * in a `Unique` guaranteeing single occurrence of every column.
		  */
		@throws[IllegalArgumentException]("if any of the components contains this.Prohibited buff, or the combined " +
		                                  "column set does not include all mandatory columns (without this.Optional buff).")
		@throws[NoSuchComponentException]("if any of the mappings in the collection is not a component of mapping.")
		def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLReadForm[S]

		/** Default read form of the given `component` of the relation mapping `root`. This form is based on columns
		  * included in [[net.noresttherein.oldsql.OperationView.defaultColumns(columns[O](mapping:MappingAt[O],component:MappingAt[O])* defaultColumns]]`(root, component)`
		  * and, unlike `form(component)`, uses the definitive buff information.
		  */
		@throws[IllegalArgumentException]("if component contains a this.Prohibited buff.")
		@throws[NoSuchComponentException]("if component is not a component of mapping.")
		def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLReadForm[S]

		/** A read form of the given `component` of the relation mapping `root` consisting of the specified components.
		  * The components may be components of `root` rather than `component`: the latter are still supported, but
		  * it is possible to also use their ''export'' versions for `root` (which are not components of `component`),
		  * or even components which are not subcomponents of `component`, if it depends on outside information.
		  */
		@throws[IllegalArgumentException]("if any of the components contains this.Prohibited buff, or the combined " +
		                                  "column set does not include all mandatory columns of component " +
		                                  "(without this.Optional buff).")
		@throws[NoSuchComponentException]("if component or one of the listed subcomponents is not a component of mapping.")
		def form[S, O](root :MappingAt[O], component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLReadForm[S]
	}


	/** Interface of SQL/DML operation types which require setting statement parameters. In addition to the buff
	  * information inherited from `OperationView`, it provides access to operation-specific
	  * [[net.noresttherein.oldsql.schema.SQLWriteForm write forms]] and disassembling mapping subjects.
	  */
	trait WriteOperationView extends OperationView {
		/** A buff type implying [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] and providing
		  * an SQL expression to use instead of the column value extracted from an entity.
		  * It is very similar to [[net.noresttherein.oldsql.OperationView.Extra Extra]],
		  * but the expression is not a constant, but instead created by a function of the annotated column value.
		  */ //there is no CustomFilter and there clashes when mixing in an InactiveBuffType to CustomSQLBuffType
		val Custom :SpecificBuffType[CustomSQLBuff]

		/** Default write form (defining the column set) for this SQL operation returning values of `S` mapped by
		  * the given mapping. For example, for [[net.noresttherein.oldsql.OperationView.InsertView InsertView]]
		  * it is `mapping.`[[net.noresttherein.oldsql.schema.Mapping.insertForm insertForm]].
		  */
		def form[S](mapping :MappingOf[S]) :SQLWriteForm[S]

//		def form[S, O](mapping :TypedColumn[S, O]) :ColumnWriteForm[S]

		/** A write form of the given `mapping` consisting of all default (export) columns of the mapping's
		  * ''export'' versions of the passed components. The given `components` collection must cover all columns
		  * which should be written  by this operation (or, more generally, included among the statement parameters
		  * set by the form) and not consist solely of the optional columns, which would
		  * not be used by method [[net.noresttherein.oldsql.OperationView.WriteOperationView.form[S](mapping* form]]`(mapping)`.
		  * The column set is defined by flat mapping the results of
		  * `mapping.`[[net.noresttherein.oldsql.schema.Mapping.insertedByDefault(component:* insertedByDefault]]`(component)`
		  * (for `InsertView`, or a corresponding method for this operation) in a `Unique`, guaranteeing
		  * a single occurrence of every column.
		  */
		@throws[IllegalArgumentException]("if any of the components contain a this.Prohibited buff, or together they " +
		                                  "do not cover all mandatory columns of mapping (without this.Optional buffs).")
		@throws[NoSuchComponentException]("if any of the elements of the collection is not a component of mapping.")
		def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLWriteForm[S]

		/** The definitive, specific to this operation type, version of the write form for `component`, based on
		  * its default ''export'' columns, from the point of view of the table/relation mapping `root`.
		  * This is conceptually different than simple `form(root.export(component))`, as columns of even an export
		  * component of mapping `root` are not automatically columns of `root`, and thus the component itself
		  * cannot be a provider of the definitive form.
		  * This method, like its other overloads, simply forwards to the operation-specific method of `root`.
		  */
		@throws[IllegalArgumentException]("if component contains a this.Prohibited buff.")
		@throws[NoSuchComponentException]("if component is not a component of mapping.")
		def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S]

		/** The definitive, from the point of view of the table/relation mapping `root`, and specific
		  * to this operation type, version of the write form for `component`,
		  * using its specified subcomponents. The given `subcomponents` collection must cover all columns
		  * which should be written  by this operation (or, more generally, included among the statement parameters
		  * set by the form) and not consist solely of the optional columns, which would not be used by method
		  * [[net.noresttherein.oldsql.OperationView.WriteOperationView.form[S](mapping* form]]`(root, component)`.
		  * The column set is defined by flat mapping the results of
		  * `mapping.`[[net.noresttherein.oldsql.schema.Mapping.insertedByDefault(component:* insertedByDefault]]`(subcomponent)`
		  * (for `InsertView`, or a corresponding method for this operation) in a `Unique`, guaranteeing
		  * a single occurrence of every column.
		  * This method, like its other overloads, simply forwards to the operation-specific method of `root`.
		  */
		@throws[IllegalArgumentException]("if any of the components contain a this.Prohibited buff, or together they " +
		                                  "do not cover all mandatory columns of component (without this.Optional buffs).")
		@throws[NoSuchComponentException]("if component, or any of the elements of the collection is not a component of mapping.")
		def form[S, O](root :MappingAt[O], component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S]

		/** Equivalent to `mapping.`[[net.noresttherein.oldsql.schema.Mapping.writtenValues[T]* writtenValues]]`(this, subject)`,
		  * but invokes directly the method corresponding to this operation type
		  * (such as [[net.noresttherein.oldsql.schema.Mapping.insertValues[T]* insertValues]]).
		  */
		def writtenValues[S, O](mapping :TypedMapping[S, O], subject :S) :ComponentValues[S, O]

		/** Equivalent to `mapping.`[[net.noresttherein.oldsql.schema.Mapping.writtenValues[T]* writtenValues]]`(this, subject, collector)`,
		  * but invokes directly the method corresponding to this operation type
		  * (such as [[net.noresttherein.oldsql.schema.Mapping.insertValues[T]* insertValues]]).
		  */
		def writtenValues[S, T, O](mapping :TypedMapping[S, O], subject :S, collector :ComponentValuesBuilder[T, O]) :Unit
	}



	/** The type of the [[net.noresttherein.oldsql.OperationView.SelectView$ SelectView]] singleton identifying
	  * an SQL ''select'' operation, or using a mapping in the context of a ''select'' clause.
	  */
	sealed abstract class SelectView extends ReadOperationView {
		override val Prohibited :FlagBuffType  = NoSelect
		override val NonDefault :BuffType      = NoSelectByDefault
		override val Exclude    :FlagBuffType  = FlagBuffType("ExcludeFromSelect")(NoSelectByDefault)
		override val Explicit   :ValueBuffType = ExplicitSelect
		override val Optional   :ValueBuffType = OptionalSelect
		override val Mandatory  :FlagBuffType  = FlagBuffType.contradict("MandatorySelect")(OptionalSelect)
		override val Audit      :AuditBuffType = SelectAudit
		override val Default    :ValueBuffType = SelectDefault
		override val Preset     :ValueBuffType = SelectPreset
		override val Extra      :SQLBuffType   = ExtraSelect
		override val Custom     :SpecificBuffType[CustomSQLBuff] = NoCustomBuffs

		override val spellingScope :SpellingScope = SelectScope

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                              (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.selectable

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                              (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.selectable(component)

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.selectedByDefault

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.selectedByDefault(component)

		override def alter[S, O](mapping :TypedMapping[S, O],
		                         include :Iterable[TypedMapping[_, O]],
		                         exclude :Iterable[TypedMapping[_, O]]) :TypedMapping[S, O] =
			mapping.forSelect(include, exclude)

		override def form[S](mapping :MappingOf[S]) :SQLReadForm[S] = mapping.selectForm

		override def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLReadForm[S] =
			mapping.selectForm(components)

		override def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLReadForm[S] =
			root.selectForm(component)

		override def form[S, O](root :MappingAt[O], component :TypedMapping[S, O],
		                        subcomponents :Unique[TypedMapping[_, O]]) :SQLReadForm[S] =
			root.selectForm(component, subcomponents)

		override def toString = "SELECT"
	}

	/** Identifies the SQL ''select'' operation and denotes the use of mappings inside ''select'' clauses. */
	implicit object SelectView extends SelectView



	/** The type of the [[net.noresttherein.oldsql.OperationView.FilterView$ FilterView]] object identifying
	  * the context of using a component for filtering a query result or, in general, of performing a column-wise
	  * comparison of its subject in SQL, be it a database row or a JDBC parameter.
	  */
	sealed abstract class FilterView extends WriteOperationView {
		override val Prohibited :FlagBuffType      = NoFilter
		override val NonDefault :BuffType          = NoFilterByDefault
		override val Exclude    :FlagBuffType      = FlagBuffType("ExcludeFromFilter")(NoFilterByDefault)
		override val Explicit   :BuffType          = ExplicitFilter
		override val Optional   :BuffType          = OptionalFilter
		override val Mandatory  :FlagBuffType      = FlagBuffType.contradict("MandatoryFilter")(OptionalFilter)
		override val Audit      :AuditBuffType     = FilterAudit
		override val Default    :ValueBuffType     = AbstractValueBuff
		override val Preset     :ValueBuffType     = FilterPreset
		override val Extra      :SQLBuffType       = ExtraFilter
		override val Custom     :SpecificBuffType[CustomSQLBuff] = NoCustomBuffs

		override val spellingScope :SpellingScope = WhereScope

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                              (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.filterable

		def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                     (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O]) :Unique[Col[_, O]] =
			root.filterable(component)

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.filteredByDefault

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.filteredByDefault(component)

		override def alter[S, O](mapping :TypedMapping[S, O],
		                         include :Iterable[TypedMapping[_, O]],
		                         exclude :Iterable[TypedMapping[_, O]]) :TypedMapping[S, O] =
			mapping.forFilter(include, exclude)

		override def writtenValues[S, O](mapping :TypedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.filterValues(subject)

		override def writtenValues[S, T, O](mapping :TypedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.filterValues(subject, collector)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.filterForm
//		override def form[S, O](mapping :TypedColumn[S, O]) :ColumnWriteForm[S] = mapping.filterForm

		override def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.filterForm(components)

		override def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S] =
			root.filterForm(component)

		override def form[S, O](root :MappingAt[O],
		                        component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			root.filterForm(component, subcomponents)

		override def toString = "FILTER"
	}

	/** Denotes the context of comparing the columns of [[net.noresttherein.oldsql.schema.Mapping Mapping]] instances
	  * in SQL. This includes their use in ''where'' clauses, but also wherever their columns are compared for equality.
	  */
	implicit object FilterView extends FilterView



	sealed abstract class InsertView extends WriteOperationView {
		override val Prohibited :FlagBuffType      = NoInsert
		override val NonDefault :BuffType          = NoInsertByDefault
		override val Exclude    :FlagBuffType      = FlagBuffType("ExcludeFromInsert")(NoInsertByDefault)
		override val Explicit   :BuffType          = ExplicitInsert
		override val Optional   :BuffType          = OptionalInsert
		override val Mandatory  :FlagBuffType      = FlagBuffType.contradict("MandatoryInsert")(OptionalInsert)
		override val Audit      :AuditBuffType     = InsertAudit
		override val Default    :ValueBuffType     = InsertDefault
		override val Preset     :ValueBuffType     = InsertPreset
		override val Extra      :SQLBuffType       = ExtraInsert
		override val Custom     :CustomSQLBuffType = CustomInsert

		override val spellingScope :SpellingScope = InsertScope

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                              (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.insertable

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                              (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.insertable(component)

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.filteredByDefault

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.insertedByDefault(component)

		override def alter[S, O](mapping :TypedMapping[S, O],
		                         include :Iterable[TypedMapping[_, O]],
		                         exclude :Iterable[TypedMapping[_, O]]) :TypedMapping[S, O] =
			mapping.forInsert(include, exclude)

		override def writtenValues[S, O](mapping :TypedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.insertValues(subject)

		override def writtenValues[S, T, O](mapping :TypedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.insertValues(subject, collector)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.insertForm
//		override def form[S, O](mapping :TypedColumn[S, O]) :ColumnWriteForm[S] = mapping.insertForm

		override def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.insertForm(components)

		override def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S] =
			root.insertForm(component)

		override def form[S, O](root :MappingAt[O],
		                        component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			root.insertForm(component, subcomponents)

		override def toString = "INSERT"
	}

	implicit object InsertView extends InsertView



	sealed abstract class UpdateView extends WriteOperationView {
		override val Prohibited :FlagBuffType      = NoUpdate
		override val NonDefault :BuffType          = NoUpdateByDefault
		override val Exclude    :FlagBuffType      = FlagBuffType("ExcludeFromUpdate")(NoUpdateByDefault)
		override val Explicit   :BuffType          = ExplicitUpdate
		override val Optional   :BuffType          = OptionalUpdate
		override val Mandatory  :FlagBuffType      = FlagBuffType.contradict("MandatoryUpdate")(OptionalUpdate)
		override val Audit      :AuditBuffType     = UpdateAudit
		override val Default    :ValueBuffType     = UpdateDefault
		override val Preset     :ValueBuffType     = UpdatePreset
		override val Extra      :SQLBuffType       = ExtraUpdate
		override val Custom     :CustomSQLBuffType = CustomUpdate

		override val spellingScope :SpellingScope = UpdateScope

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                              (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.updatable

		override def applicableColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                              (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.updatable(component)

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q]]
		                           (mapping :MappingTemplate[Comp, Col]) :Unique[Col[_, mapping.Origin]] =
			mapping.updatedByDefault

		override def defaultColumns[Comp[T, Q] <: TypedMapping[T, Q], Col[T, Q] <: TypedColumn[T, Q] with Comp[T, Q], O]
		                           (root :MappingTemplate[Comp, Col] { type Origin = O }, component :MappingAt[O])
				:Unique[Col[_, O]] =
			root.updatedByDefault(component)

		override def alter[S, O](mapping :TypedMapping[S, O],
		                         include :Iterable[TypedMapping[_, O]],
		                         exclude :Iterable[TypedMapping[_, O]]) :TypedMapping[S, O] =
			mapping.forUpdate(include, exclude)

		override def writtenValues[S, O](mapping :TypedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.updateValues(subject)

		override def writtenValues[S, T, O](mapping :TypedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.updateValues(subject, collector)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.updateForm

		override def form[S, O](mapping :TypedMapping[S, O], components :Unique[TypedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.updateForm(components)

		override def form[S, O](root :MappingAt[O], component :TypedMapping[S, O]) :SQLWriteForm[S] =
			root.updateForm(component)

		override def form[S, O](root :MappingAt[O],
		                        component :TypedMapping[S, O], subcomponents :Unique[TypedMapping[_, O]])
				:SQLWriteForm[S] =
			root.updateForm(component, subcomponents)

		override def toString = "UPDATE"
	}

	implicit object UpdateView extends UpdateView

	//consider: a CompleteView extends ReadOperationView with WriteOperationView

	private[this] val NoCustomBuffs :SpecificBuffType[CustomSQLBuff] =
		new SpecificBuffType[CustomSQLBuff] with InactiveBuffType with Stateless {
			override def unapply[T](buff :Buff[T]) = Lack
			override def unapply[T](buffs :Iterable[Buff[T]]) = Lack
			override def toString :String = "InactiveCustomSQL"
		}

	private[oldsql] val GroupByView :WriteOperationView = FilterView

	final val operations = Seq[OperationView](SelectView, FilterView, InsertView, UpdateView)


}

