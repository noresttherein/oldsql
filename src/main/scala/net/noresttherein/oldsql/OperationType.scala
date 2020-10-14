package net.noresttherein.oldsql

import net.noresttherein.oldsql.collection.Unique
import net.noresttherein.oldsql.schema.Buff.{AuditBuffType, BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraInsert, ExtraQuery, ExtraSelect, ExtraUpdate, FlagBuffType, InsertAudit, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit, ValueBuffType}
import net.noresttherein.oldsql.schema.{ColumnMapping, ComponentValues, SQLReadForm, SQLWriteForm}
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.schema.Mapping.{MappingOf, RefinedMapping}



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
	  * by the framework, with the value specified by the buff instance. For example, `ExtraQuery` will apply
	  * an additional filter on the queried table or view. It always implies 
	  * [[net.noresttherein.oldsql.OperationType.prohibited prohibited]].
	  */
	val extra :ValueBuffType
	
	/** A buff marking a column/component as not included by default in the operation, but possibly still allowed
	  * if included explicitly. Example: `NoSelectByDefault`. It is implied by both
	  * [[net.noresttherein.oldsql.OperationType.prohibited prohibited]] and
	  * [[net.noresttherein.oldsql.OperationType.explicit explicit]].
	  */
	val nonDefault :FlagBuffType
	
	/** A buff marking that a column/component must be included in the operation explicitly, as it is not included
	  * by the mapping in the standard process. Example: `ExplicitSelect` for CLOB/BLOB types. This buff implies
	  * [[net.noresttherein.oldsql.OperationType.optional optional]] and
	  * [[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]].
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

	/** All columns, direct or indirect, of the given mapping which are applicable to this operation type. 
	  * These are all columns from its [[net.noresttherein.oldsql.schema.Mapping.columns columns]] list ''without''
	  * the [[net.noresttherein.oldsql.OperationType.prohibited prohibited]] buff.
	  */
	def columns[S, O](mapping :RefinedMapping[S, O]) :Unique[ColumnMapping[_, O]]

	/** Creates a version of the given mapping with buffs manipulated in such a way that the specified components
	  * are included or excluded from this operation type by default. It can only include components which are
	  * allowed for this operation type (have the `explicit` buff) and exclude those which are specifically marked
	  * as optional with the `optional` buff.
	  */
	def customize[S, O](mapping :RefinedMapping[S, O],
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
		override val prohibited = NoSelect
		override val extra = ExtraSelect
		override val nonDefault = NoSelectByDefault
		override val explicit = ExplicitSelect
		override val optional = OptionalSelect
		override val audit = SelectAudit

		override def columns[S, O](mapping :RefinedMapping[S, O]) :Unique[ColumnMapping[_, O]] = mapping.selectable

		override def customize[S, O](mapping :RefinedMapping[S, O],
		                             include :Iterable[RefinedMapping[_, O]],
		                             exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forSelect(include, exclude)

		override def form[S](mapping :MappingOf[S]) :SQLReadForm[S] = mapping.selectForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLReadForm[S] =
			mapping.selectForm(components)
	}

	implicit case object SELECT extends SELECT


	sealed abstract class QUERY extends WriteOperationType {
		override val prohibited = NoQuery
		override val extra = ExtraQuery
		override val nonDefault = NoQueryByDefault
		override val explicit = ExplicitQuery
		override val optional = OptionalQuery
		override val audit = QueryAudit

		override def columns[S, O](mapping :RefinedMapping[S, O]) :Unique[ColumnMapping[_, O]] = mapping.queryable

		override def customize[S, O](mapping :RefinedMapping[S, O],
		                             include :Iterable[RefinedMapping[_, O]],
		                             exclude :Iterable[RefinedMapping[_, O]]) :RefinedMapping[S, O] =
			mapping.forQuery(include, exclude)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O],
		                                    subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			mapping.queryValues(subject, collector)

		override def writtenValues[S, T, O](mapping :RefinedMapping[S, O], subject :S) :ComponentValues[S, O] =
			mapping.queryValues(subject)

		override def form[S](mapping :MappingOf[S]) :SQLWriteForm[S] = mapping.queryForm

		override def form[S, O](mapping :RefinedMapping[S, O], components :Unique[RefinedMapping[_, O]]) :SQLWriteForm[S] =
			mapping.queryForm(components)
	}

	implicit case object QUERY extends QUERY


	sealed abstract class UPDATE extends WriteOperationType {
		override val prohibited = NoUpdate
		override val extra = ExtraUpdate
		override val nonDefault = NoUpdateByDefault
		override val explicit = ExplicitUpdate
		override val optional = OptionalUpdate
		override val audit = UpdateAudit

		override def columns[S, O](mapping :RefinedMapping[S, O]) :Unique[ColumnMapping[_, O]] = mapping.updatable

		override def customize[S, O](mapping :RefinedMapping[S, O],
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


	sealed abstract class INSERT extends WriteOperationType {
		override val prohibited = NoInsert
		override val extra = ExtraInsert
		override val nonDefault = NoInsertByDefault
		override val explicit = ExplicitInsert
		override val optional = OptionalInsert
		override val audit = InsertAudit

		override def columns[S, O](mapping :RefinedMapping[S, O]) :Unique[ColumnMapping[_, O]] = mapping.insertable

		override def customize[S, O](mapping :RefinedMapping[S, O],
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

}

