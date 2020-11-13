package net.noresttherein.oldsql.schema.support

import scala.collection.mutable.Builder

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.Buff.{BuffType, FlagBuffType}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import MappingAdapter.{Adapted, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.CustomizedMapping.overrides
import net.noresttherein.oldsql.schema.support.MappingProxy.EagerDeepProxy






/** A `Mapping` adapter specific to a single database operation. It modifies the buffs on a mapping
  * to include or exclude certain columns from that operation by default.
  * @author Marcin Mościcki
  */
class CustomizedMapping[+M <: RefinedMapping[S, O], S, O] private
                       (protected override val backer :M,
                        substitutions :NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component])
	extends EagerDeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	def this(source :M, op :OperationType,
	         includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]]) =
		this(source, overrides(source, op, includes, excludes))



	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		substitutions.getOrElse(column, column).asInstanceOf[Column[T]]

	//no overrides of forSelect, forInsert, etc because there is no simple way to reconcile old overrides with new,
	//as the new overrides are created with the assumption they are overriding the components from backer, not this

}






object CustomizedMapping {
	import OperationType.operations
	type Override[S, O] = Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S]
	type Overrides[O] = NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component]


	def apply[M <: RefinedMapping[S, O], S, O]
	         (source :M, includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Adapted[M] =
		new CustomizedMapping[M, S, O](source, overrides(source, includes, excludes))
			with DelegateAdapter[M, S, O]

	def apply[M <: RefinedMapping[S, O], S, O]
	         (source :M, op :OperationType,
	          includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]] = Nil) :Adapted[M] =
		new CustomizedMapping[M, S, O](source, op, includes, excludes) with DelegateAdapter[M, S, O]

	def select[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, SELECT, include, exclude)

	def filter[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
	          (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, FILTER, include, exclude)

	def update[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, UPDATE, include, exclude)

	def insert[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, INSERT, include, exclude)



	private[schema] def overrides[S, O]
	                    (source :RefinedMapping[S, O], op :OperationType,
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Overrides[O] =
		overrides(source, includes, excludes, op.prohibited, op.nonDefault, op.exclude, op.optional)


	private[schema] def overrides[S, O]
	                    (source :RefinedMapping[S, O],
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]],
	                     prohibited :BuffType, nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType)
			:Overrides[O] =
	{
		val excludeExports = excludes.map(source.export(_)).to(Unique)
		val includeExports = includes.map(source.export(_)).filterNot(excludeExports)
		val included = this.include(source, includeExports, prohibited, nonDefault)
		val excluded = this.exclude(source, excludeExports, nonDefault, exclude, optional)
		included ++ excluded
	}

	private[schema] def overrides[S, O]
	                    (source :RefinedMapping[S, O],
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Overrides[O] =
	{
		val excludeExports = excludes.map(source.export(_)).to(Unique)
		val includeExports = includes.map(source.export(_)).filterNot(excludeExports)
		val included = include(source, includeExports)
		val excluded = exclude(source, excludeExports)
		included ++ excluded
	}



	/** Modifies the buffs of the given components so that they are included by default in a particular operation.
	  * It removes the `explicit` buff from all the components in the `components` collection and their subcomponents,
	  * but if the `prohibited` buff is present on any of the components, an `IllegalArgumentException` is raised.
	  * If it is present on any of the subcomponents, the presence of `explicit` buff is ignored and the subcomponent
	  * remains unchanged.
	  */
	private def include[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]],
	                       prohibited :BuffType, nonDefault :BuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			include(mapping, component, prohibited, nonDefault, builder)
		builder.result()
	}

	//todo: for non-columns, include only their default subcomponents, not all of them.
	private def include[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                          prohibited :BuffType, nonDefault :BuffType,
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		component match { //already is export
			case comp if prohibited.enabled(comp) =>
				throw new IllegalArgumentException(
					s"Can't include component $component of $mapping as it has the $prohibited buff."
				)
			case column :ColumnMapping[_, O @unchecked] =>
				if (nonDefault.enabled(column))
					builder += new Override(column, column.withBuffs(column.buffs.filter(nonDefault.disabled)))
			case export =>
				if (nonDefault.enabled(export)) {
					val buffs = export.buffs.filter(nonDefault.disabled)
					val buffed = BuffedMapping.nonCascading[RefinedMapping[T, O], T, O](export, buffs:_*)
					builder += new Override[T, O](export, buffed)
				}
				export.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] =>
						val col = mapping.export(column)
						if (prohibited.disabled(col) && nonDefault.enabled(col))
							builder += new Override(col, col.withBuffs(col.buffs.filter(nonDefault.disabled)))
					case sub =>
						val comp = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						if (prohibited.disabled(comp) && nonDefault.enabled(comp)) {
							val buffs = comp.buffs.filter(nonDefault.disabled)
							val buffed = BuffedMapping.cascading[RefinedMapping[Any, O], Any, O](comp, buffs :_*)
							builder += new Override[Any, O](comp, buffed)
						}
				}
		}
	


	private def include[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]]) :Overrides[O] = {
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			include(mapping, component, builder)
		builder.result()
	}


	private def include[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
	{
		def ops(export :Mapping) =
			operations.filter(o => o.prohibited.disabled(export) && o.nonDefault.enabled(export))

		def substitute[V](column :ColumnMapping[V, O]) = {
			val optional = ops(column)
			if (optional.nonEmpty) {
				val disabled = column.buffs.filter { b => optional.forall(_.nonDefault.disabled(b)) }
				builder += new Override(column, column.withBuffs(disabled))
			}
		}

		component match {
			case column :ColumnMapping[_, O @unchecked] => substitute(column)

			case export =>
				val optional = ops(export)
				if (optional.nonEmpty) {
					val disabled = export.buffs.filter { b => optional.forall(_.nonDefault.disabled(b)) }
					val buffed = BuffedMapping.nonCascading[RefinedMapping[T, O], T, O](export, disabled :_*)
					builder += new Override[T, O](export, buffed)
				}
				export.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] => substitute(mapping.export(column))

					case sub =>
						val export = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						val optional = ops(export)
						if (optional.nonEmpty) {
							val disabled = export.buffs.filter { b => optional.forall(_.nonDefault.disabled(b)) }
							val buffed = BuffedMapping.cascading[RefinedMapping[Any, O], Any, O](export, disabled :_*)
							builder += new Override[Any, O](export, buffed)
						}
				}
		}
	}



	/** Modifies the buffs of the given components so that they are not included by default in a particular operation.
	  * It adds the  `nonDefault` flag to all the components in the `components` collection and their subcomponents,
	  * but if the `optional` buff is missing on any of the listed components, an `IllegalArgumentException` is raised.
	  * Subcomponents of the listed components without the `optional` buff simply remain unchanged.
	  */
	private def exclude[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]],
	                       nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			this.exclude(mapping, component, nonDefault, exclude, optional, builder)
		builder.result()
	}

	private def exclude[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                          nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType,
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		component match {
			case comp if optional.disabled(comp) =>
				throw new IllegalArgumentException(
					s"Can't exclude component $comp of $mapping as it does not have the $optional buff."
				)
			case column :ColumnMapping[_, O @unchecked] =>
				if (nonDefault.disabled(column))
					builder += new Override(column, column.withBuffs(exclude[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] =>
						val col = mapping.export(column).asInstanceOf[ColumnMapping[Any, O]]
						if (nonDefault.disabled(col) && optional.enabled(col))
							builder += new Override[Any, O](col, col.withBuffs(exclude[Any] +: col.buffs))
					case sub =>
						val comp = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						if (nonDefault.disabled(comp) && optional.enabled(comp)) {
							val buffs = exclude[Any] +: comp.buffs
							val buffed = BuffedMapping.cascading[RefinedMapping[Any, O], Any, O](comp, buffs :_*)
							builder += new Override[Any, O](comp, buffed)
						}
				}

		}



	private def exclude[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]]) :Overrides[O] = {
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			exclude(mapping, component, builder)
		builder.result()
	}


	private def exclude[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
	{
		def ops(export :RefinedMapping[_, O]) =
			operations.filter(o => o.optional.enabled(export) && o.nonDefault.disabled(export))

		def substitute[V](column :ColumnMapping[V, O]) = {
			val optional = ops(column)
			if (optional.nonEmpty)
				builder += new Override(column, column.withBuffs(optional.map(_.exclude[V]) ++: column.buffs))
		}

		component match {
			case column :ColumnMapping[_, O @unchecked] => substitute(column)

			case export =>
				export.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] => substitute(mapping.export(column))

					case sub =>
						val export = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						val optional = ops(export)
						if (optional.nonEmpty) {
							val buffs = optional.map(_.exclude[Any]) ++: export.buffs
							val buffed = BuffedMapping.cascading[RefinedMapping[Any, O], Any, O](export, buffs :_*)
							builder += new Override[Any, O](export, buffed)
						}
				}
		}
	}


}

