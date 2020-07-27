package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.{-#>, Assoc}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.EagerDeepProxy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.{Buff, ColumnMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.schema.bits.CustomizedMapping.{exclude, include}
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{Adapted, DelegateAdapter}
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{INSERT, QUERY, SELECT, UPDATE}






/** A `Mapping` adapter specific to a single database operation. It modifies the buffs on a mapping
  * to include or exclude certain columns from that operation by default.
  * @author Marcin Mościcki
  */
class CustomizedMapping[+M <: RefinedMapping[S, O], S, O] protected
                       (protected override val backer :M,
                        protected val substitutions :NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component])
	extends EagerDeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	def this(source :M, includes :Iterable[RefinedMapping[_, O]], prohibited :BuffType, explicit :BuffType,
	         excludes :Iterable[RefinedMapping[_, O]], optional :BuffType, nonDefault :FlagBuffType) =
		this(source,
		     include(source, includes, prohibited, explicit) ++ exclude(source, excludes, optional, nonDefault)
		)

	def this(source :M, op :OperationType, includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]]) =
		this(source,
		     include(source, includes, op.prohibited, op.explicit) ++
			 exclude(source, excludes, op.optional, op.nonDefault)
	     )



	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		substitutions.getOrElse(column, column).asInstanceOf[Column[T]]



	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(include, NoSelect, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

	override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(include, NoQuery, ExplicitQuery, exclude, OptionalQuery, NoQueryByDefault)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(include, NoUpdate, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Component[S] =
		customize(include, NoInsert, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)

	private def customize(include :Iterable[Component[_]], no :BuffType, explicit :BuffType,
	                        exclude :Iterable[Component[_]], optional :BuffType, nonDefault :FlagBuffType) :Component[S] =
	{
		val includes = CustomizedMapping.include(backer, include, no, optional)
		val excludes = CustomizedMapping.exclude(backer, exclude, optional, nonDefault)
		new CustomizedMapping[M, S, O](backer, substitutions ++ includes ++ excludes)
	}

}






object CustomizedMapping {
	type Override[S, O] = Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S]
	type Overrides[O] = NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component]


	def apply[M <: RefinedMapping[S, O], S, O]
	         (source :M, op :OperationType,
	          includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]] = Nil) :Adapted[M] =
	{
		val included = include(source, includes, op.prohibited, op.explicit)
		val excluded = exclude(source, excludes, op.optional, op.nonDefault)
		new CustomizedMapping[M, S, O](source, included ++ excluded) with DelegateAdapter[M, S, O]
	}

	def select[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, SELECT, include, exclude)

	def query[M <: RefinedMapping[S, O], S, O]
	         (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
	         (implicit inferS :Conforms[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, QUERY, include, exclude)

	def update[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, UPDATE, include, exclude)

	def insert[M <: RefinedMapping[S, O], S, O]
	          (source :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :Conforms[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](source, INSERT, include, exclude)



	private[schema] def customize[M <: RefinedMapping[S, O], S, O]
	                    (source :M, op :OperationType,
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]]) :Adapted[M] =
	{
		val included = include(source, includes, op.prohibited, op.explicit)
		val excluded = exclude(source, excludes, op.optional, op.nonDefault)
		new CustomizedMapping[M, S, O](source, included ++ excluded) with DelegateAdapter[M, S, O]
	}


	/** Modifies the buffs of the given components so that they are included by default in a particular operation.
	  * It removes the `explicit` buff from all the components in the `components` collection and their subcomponents,
	  * but if the `prohibited` buff is present on any of the components, an `IllegalArgumentException` is raised.
	  * If it is present on any of the subcomponents, the presence of `explicit` buff is ignored and the subcomponent
	  * remains unchanged.
	  */
	private[schema] def include[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]],
	                               prohibited :BuffType, explicit :BuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			include(mapping, component, prohibited, explicit, builder)
		builder.result()
	}

	//todo: for non-columns, include only their default subcomponents, not all of them.
	private[schema] def include[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                                  prohibited :BuffType, explicit :BuffType,
	                                  builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		mapping.export(component) match {
			case comp if prohibited.enabled(comp) =>
				throw new IllegalArgumentException(
					s"Can't include component $component of $mapping as it has the $prohibited buff."
				)
			case column :ColumnMapping[_, O @unchecked] =>
				if (explicit.enabled(column))
					builder += new Override(column, column.withBuffs(column.buffs.filter(explicit.disabled)))
			case lifted =>
				if (explicit.enabled(lifted)) {
					val buffs = lifted.buffs.filter(explicit.disabled)
					val buffed = BuffedMapping.nonCascade[RefinedMapping[T, O], T, O](lifted, buffs:_*)
					builder += new Override[T, O](lifted, buffed)
				}
				lifted.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] =>
						val col = mapping.export(column)
						if (prohibited.disabled(col) && explicit.enabled(col))
							builder += new Override(col, col.withBuffs(col.buffs.filter(explicit.disabled)))
					case sub =>
						val comp = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						if (prohibited.disabled(comp) && explicit.enabled(comp)) {
							val buffs = comp.buffs.filter(explicit.disabled)
							val buffed = BuffedMapping.cascade[RefinedMapping[Any, O], Any, O](comp, buffs :_*)
							builder += new Override[Any, O](comp, buffed)
						}
				}

		}



	/** Modifies the buffs of the given components so that they are not included by default in a particular operation.
	  * It adds the  `nonDefault` flag to all the components in the `components` collection and their subcomponents,
	  * but if the `optional` buff is missing on any of the listed components, an `IllegalArgumentException` is raised.
	  * Subcomponents of the listed components without the `optional` buff simply remain unchanged.
	  */
	private[schema] def exclude[O](mapping :RefinedMapping[_, O], components :Iterable[RefinedMapping[_, O]],
	                               optional :BuffType, nonDefault :FlagBuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components)
			exclude(mapping, component, optional, nonDefault, builder)
		builder.result()
	}

	private[schema] def exclude[T, O](mapping :RefinedMapping[_, O], component :RefinedMapping[T, O],
	                                  optional :BuffType, nonDefault :FlagBuffType,
	                                  builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		mapping.export(component) match {
			case comp if optional.disabled(comp) =>
				throw new IllegalArgumentException(
					s"Can't exclude component $comp of $mapping as it does not have the $optional buff.")
			case column :ColumnMapping[_, O @unchecked] =>
				if (nonDefault.disabled(column))
					builder += new Override(column, column.withBuffs(nonDefault[T] +: column.buffs))
			case lifted =>
				lifted.subcomponents foreach {
					case column :ColumnMapping[_, O @unchecked] =>
						val col = mapping.export(column).asInstanceOf[ColumnMapping[Any, O]]
						if (nonDefault.disabled(col) && optional.enabled(col))
							builder += new Override[Any, O](col, col.withBuffs(nonDefault[Any] +: col.buffs))
					case sub =>
						val comp = mapping.export(sub).asInstanceOf[RefinedMapping[Any, O]]
						if (nonDefault.disabled(comp) && optional.enabled(comp)) {
							val buffs = nonDefault[Any] +: comp.buffs
							val buffed = BuffedMapping.cascade[RefinedMapping[Any, O], Any, O](comp, buffs :_*)
							builder += new Override[Any, O](comp, buffed)
						}
				}

		}



//	private class CustomizedMappingAdapter[M <: RefinedMapping[S, O], S, O](mapping :M, substitutions :Overrides[O])
//		extends CustomizedMapping[M, S, O](mapping, substitutions) with DelegateAdapter[M, S, O]

}
