package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.Buff.{AbstractValueBuff, AuditBuffType, BuffType, ExplicitFilter, ExplicitInsert, ExplicitSelect, ExplicitUpdate, ExtraFilter, ExtraInsert, ExtraSelect, ExtraUpdate, FilterAudit, FlagBuffType, InsertAudit, InsertDefault, NoFilter, NoFilterByDefault, NoInsert, NoInsertByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalFilter, OptionalInsert, OptionalSelect, OptionalUpdate, SelectAudit, SelectDefault, UpdateAudit, UpdateDefault, ValueBuffType}
import net.noresttherein.oldsql.schema.{ColumnMapping, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}






/** A marker identifying a single SQL operation type (or rather, its aspect/usage). It is used to generalize
  * the code for ''select'', ''update'' and ''insert'', as well as the use of a mapping in a ''where'' clause
  * of an SQL query. It lists the buff types specific to the given operation and provides methods delegating
  * to one of the variants declared by the mapping, specific to this operation type.
  * @author Marcin Mościcki
  */
sealed trait OperationType {

	/** A buff marking a column/component as not allowed in a given operation type or, more specifically,
	  * that the column is not used in this statement type as part of the mapping - it can still be used by the
	  * framework explicitly (see [[net.noresttherein.oldsql.OperationType.extra extra]]. Example: `NoSelect`.
	  */
	val prohibited :FlagBuffType

	/** A buff marking a column/component as not used as part of the owning mapping, but forcibly always included
	  * by the framework, with the value specified by the buff instance. For example, `ExtraFilter` will apply
	  * an additional filter on the queried table or view. It always implies
	  * [[net.noresttherein.oldsql.OperationType.prohibited prohibited]].
	  */
	val extra :ValueBuffType

	/** A buff marking a column/component as not included by default in the operation, but possibly still allowed
	  * if included explicitly. It is implied by both [[net.noresttherein.oldsql.OperationType.prohibited prohibited]]
	  * and [[net.noresttherein.oldsql.OperationType.explicit explicit]]. Example: `NoSelectByDefault`.
	  */
	val nonDefault :BuffType

	/** A buff implying [[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]], used internally
	  * in order to flag an
	  * [[net.noresttherein.oldsql.OperationType.optional optional]]/[[net.noresttherein.oldsql.OperationType.explicit explicit]]
	  * component for being excluded from the operation. Should not be used in the application code in order
	  * to statically mark optional components - use one of the former buff types instead.
	  */
	val exclude :FlagBuffType

	/** A buff marking that a column/component must be included in the operation explicitly, as it is not included
	  * by the mapping in the standard process. This buff implies
	  * [[net.noresttherein.oldsql.OperationType.optional optional]] and
	  * [[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]]. Example: `ExplicitSelect` for CLOB/BLOB types.
	  */
	val explicit :BuffType
	
	/** A buff marking that a column/component can be excluded from this operation, but it must happen explicitly,
	  * as it is included by default. Example `OptionalSelect`. 
	  */
	val optional :BuffType
	
	/** A buff carrying a transforming function working on the mapping subject type, which must be applied to
	  * every value written or read by this operation. For example, `InsertAudit` and `UpdateAudit` can be used
	  * to implement validation of the data before it is written to the database.
	  */
	val audit :AuditBuffType

	/** A buff carrying a default value which should be inserted/updated if the written entity is missing one
	  * for the component (in write operations) or returned if none can be assembled (when selecting). */
	val default :ValueBuffType

	/** All columns, direct or indirect, of the given mapping which are applicable to this operation type. 
	  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationType.prohibited prohibited]] buff.
	  * @see [[net.noresttherein.oldsql.OperationType.defaultColumns]]
	  */
	def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

	/** Columns, both direct and indirect, of the given mapping which are used by default in this operation type.
	  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]] buff.
	  * @see [[net.noresttherein.oldsql.OperationType.columns]]
	  */
	def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]]

	/** Creates a version of the given mapping with buffs manipulated in such a way that the specified components
	  * are included or excluded from this operation type by default. It can only include components which are
	  * allowed for this operation type (have the `explicit` buff) and exclude those which are specifically marked
	  * as optional with the `optional` buff.
	  */
	def alter[S, O](mapping :RefinedMapping[S, O],
	                include :Iterable[RefinedMapping[_, O]],
	                exclude :Iterable[RefinedMapping[_, O]] = Nil) :RefinedMapping[S, O]
}






object OperationType {

	sealed trait ReadOperationType extends OperationType {
		override val explicit :ValueBuffType
		override val optional :ValueBuffType

		def form[S](mapping :MappingOf[S]) :SQLReadForm[S]
		def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLReadForm[S]
	}


	sealed trait WriteOperationType extends OperationType {
		def form[S](mapping :MappingOf[S]) :SQLWriteForm[S]
		def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S]

		def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S) :ComponentValues[S, O]
		def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S, collector :ComponentValuesBuilder[T, O]) :Unit
	}



	sealed abstract class SELECT extends ReadOperationType {
		override val prohibited :FlagBuffType  = NoSelect
		override val extra      :ValueBuffType = ExtraSelect
		override val nonDefault :BuffType      = NoSelectByDefault
		override val exclude    :FlagBuffType  = FlagBuffType("ExcludeFromSelect")(NoSelectByDefault)
		override val explicit   :ValueBuffType = ExplicitSelect
		override val optional   :ValueBuffType = OptionalSelect
		override val audit      :AuditBuffType = SelectAudit
		override val default    :ValueBuffType = SelectDefault

		override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.selectable

		override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.selectedByDefault

		override def alter[S, O](mapping :RefinedMapping[S, O],
		                         include :Iterable[RefinedMapping[_, O]],
		                         exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forSelect(include, exclude)

		override def form[S](mapping :MappingOf[S]) :SQLReadForm[S] = mapping.selectForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLReadForm[S] =
			mapping.selectForm(components)
	}

	implicit case object SELECT extends SELECT



	sealed abstract class FILTER extends WriteOperationType {
		override val prohibited :FlagBuffType  = NoFilter
		override val extra      :ValueBuffType = ExtraFilter
		override val nonDefault :BuffType      = NoFilterByDefault
		override val exclude    :FlagBuffType  = FlagBuffType("ExcludeFromFilter")(NoFilterByDefault)
		override val explicit   :BuffType      = ExplicitFilter
		override val optional   :BuffType      = OptionalFilter
		override val audit      :AuditBuffType = FilterAudit
		override val default    :ValueBuffType = AbstractValueBuff

		override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.filterable

		override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.filteredByDefault

		override def alter[S, O](mapping :RefinedMapping[S, O],
		                         include :Iterable[RefinedMapping[_, O]],
		                         exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forFilter(include, exclude)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.filterValues(subject, collector)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.filterValues(subject)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.filterForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.filterForm(components)
	}

	implicit case object FILTER extends FILTER



	sealed abstract class INSERT extends WriteOperationType {
		override val prohibited :FlagBuffType  = NoInsert
		override val extra      :ValueBuffType = ExtraInsert
		override val nonDefault :BuffType      = NoInsertByDefault
		override val exclude    :FlagBuffType  = FlagBuffType("ExcludeFromInsert")(NoInsertByDefault)
		override val explicit   :BuffType      = ExplicitInsert
		override val optional   :BuffType      = OptionalInsert
		override val audit      :AuditBuffType = InsertAudit
		override val default    :ValueBuffType = InsertDefault

		override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.insertable

		override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.insertedByDefault

		override def alter[S, O](mapping :RefinedMapping[S, O],
		                         include :Iterable[RefinedMapping[_, O]],
		                         exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forInsert(include, exclude)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.insertValues(subject, collector)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.insertValues(subject)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.insertForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.insertForm(components)
	}

	implicit case object INSERT extends INSERT



	sealed abstract class UPDATE extends WriteOperationType {
		override val prohibited :FlagBuffType  = NoUpdate
		override val extra      :ValueBuffType = ExtraUpdate
		override val nonDefault :BuffType      = NoUpdateByDefault
		override val exclude    :FlagBuffType  = FlagBuffType("ExcludeFromUpdate")(NoUpdateByDefault)
		override val explicit   :BuffType      = ExplicitUpdate
		override val optional   :BuffType      = OptionalUpdate
		override val audit      :AuditBuffType = UpdateAudit
		override val default    :ValueBuffType = UpdateDefault

		override def columns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.updatable

		override def defaultColumns[O](mapping :MappingAt[O]) :Unique[ColumnMapping[_, O]] = mapping.updatedByDefault

		override def alter[S, O](mapping :RefinedMapping[S, O],
		                         include :Iterable[RefinedMapping[_, O]],
		                         exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forUpdate(include, exclude)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.updateValues(subject, collector)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.updateValues(subject)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.updateForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.updateForm(components)
	}

	implicit case object UPDATE extends UPDATE



	final val operations = Seq[OperationType](SELECT, FILTER, INSERT, UPDATE)

}

