package net.noresttherein.oldsql.schema.support

import scala.collection.mutable.Builder

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.Buff.{BuffType, FlagBuffType}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.AlteredMapping.{overrides, Override, Overrides}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy






/** A `Mapping` proxy which substitutes some components of the original, while retaining the others.
  * This is typically used for modifying the buffs on some of the components or columns, by substituting components
  * of `backer` with adapters in the form of [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]
  * or similar copies of columns for the same form. Most notably, it is used to alter the buffs defining
  * which columns should take part in which database operations, creating in the process a mapping which
  * ''by default'' includes/excludes certain columns which were excluded/included in the origin for the purpose
  * of a single operation type.
  * @see [[net.noresttherein.oldsql.schema.support.AdjustedMapping AdjustedMapping]]
  * @see [[net.noresttherein.oldsql.OperationType]]
  * @author Marcin Mościcki
  */
class AlteredMapping[+M <: RefinedMapping[S, O], S, O]
                    (protected override val backer :M,
                     substitutions :NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component])
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	def this(original :M, op :OperationType,
	         includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]]) =
		this(original, overrides(original, op, includes, excludes))

	
	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		substitutions.getOrElse(column, column).asInstanceOf[Column[T]]

	//no overrides of forSelect, forInsert, etc because there is no simple way to combine old overrides with new,
	//as the new overrides would be created with the assumption they are overriding the components from backer, not this
	//we'd need for this a mechanic of contradicting and disabling buffs
}






/** A factory for mapping adapters which modify buffs on chosen components and columns in order to affect
  * if they should be used in a particular database operation. All columns are by default both eligible and mandatory
  * for all database operations and, in order to exclude a particular one - both completely and only by default -
  * they require [[net.noresttherein.oldsql.schema.Buff Buff]]s being applied. The exact buffs affected by this factory
  * are defined in every [[net.noresttherein.oldsql.OperationType OperationType]] instance:
  *   1. [[net.noresttherein.oldsql.OperationType.prohibited prohibited]] (for example,
  *      [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]]): completely disallows the use of a component
  *      in that operation type under any circumstances. Providing a component with this buff in the 'include' list
  *      will result in an `IllegalArgumentException`.
  *   1. [[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]] (such as
  *      [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]): the buff which excludes
  *      the component from a particular operation type. All columns which do not carry this buff are included
  *      by default, and in most cases are impossible to use in this capacity without adapting the mapping first.
  *      It occurs typically in the form of [[net.noresttherein.oldsql.OperationType.explicit explicit]] buff 
  *      ([[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]]), which can be removed
  *      by this procedure, making the component selectable (or insertable, updatable, etc) normally.
  *   1. [[net.noresttherein.oldsql.OperationType.optional optional]]: buffs such as 
  *      [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] are the opposite of the `nonDefault`
  *      buffs: they mark components which can be omitted from the selected/updated column set. Listing a component
  *      without this buff in the 'excludes' argument will result in an `IllegalArgumentException`. At the same time,
  *      the buff provides a default value for the component which will be returned in place of the one
  *      in the table/entity object.
  *   1. [[net.noresttherein.oldsql.OperationType.exclude exclude]]: instead of replacing an `OptionalXxx`
  *      with an `ExplicitXxx` to exclude a component, this synthetic buff implying `NoXxxByDefault` is added
  *      (unless the component already has a `NoXxxByDefault` attached) to force this change.
  * 
  * It is important to remember that buffs can be declared on any component, and that they cascade: the default
  * behaviour implemented by all bundled classes is for compatible buffs to be passed on to every subcomponent and
  * column of the buffed mapping. Such inherited buffs always follow the more deeply/directly defined buffs, and
  * may be shadowed by, or come to conflict with, a similar buff defined on a subcomponent. For example,
  * when having an `ExplicitSelect` buff both on a component and one of its columns, if that component is passed
  * in the 'include' list, the change described earlier will affect the whole component tree under it,
  * ''with the exception of'' that single column. In order for the column to be included in the column set,
  * it would have to be specified alongside its ancestor component, because its `ExplicitSelect` will remain
  * active. Similarly, an inherited added `exclude` buff is contradicted by an `optional` buff coming before it
  * (that is, defined somewhere under the component tree of the component which is excluded) and the buffed column
  * must be individually specified in the 'exclude' list to be omitted.
  * 
  * Another consequence of buff inheritance and performing the changes on the level of single components
  * (rather than manually for the whole tree) is that passing to be included a component which already is included
  * by default has no effect; in particular, any its columns which are not included by default are unaffected
  * (and will not be included). The same goes for excluding components. On the other hand, if a column or component
  * under one of the listed components has the `prohibited`/does not have the `optional` buff, it will be ignored
  * as previously, without raising an error. An exception is thrown only if current buffs of the explicitly
  * listed components do not allow the changes to be made.
  * 
  * It is important to note that this algorithm only simulates the effect that changing buffs on a component would
  * have on the buffs of its [[net.noresttherein.oldsql.schema.Mapping.export export]] subcomponents by a different 
  * process. Mapping implementations are allowed to handle buffs differently, ignore them or implement completely
  * custom behaviour, which can potentially interfere with how this factory operates. In particular, non-standard
  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] methods can be bypassed.
  * 
  * Remember also that [[net.noresttherein.oldsql.OperationType.extra OperationType.extra]] columns
  * (having o [[net.noresttherein.oldsql.schema.Buff.ExtraSelect ExtraSelect]] buff or its sibling) imply
  * `prohibited` (`NoSelect`); despite taking part in every operation of the given type, they cannot appear
  * on the 'include' list, but rather are included automatically at a later stage.
  * 
  * @see [[net.noresttherein.oldsql.schema.support.AdjustedMapping]]
  * @see [[net.noresttherein.oldsql.schema.support.BuffedMapping]]     
  * @see [[net.noresttherein.oldsql.schema.Buffs]]     
  */ //fixme: exclude changes replace completely any includes changes on the same component rather than going on top of them
object AlteredMapping {
	type Override[S, O] = Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S]
	type Overrides[O] = NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component]

	
	def apply[M <: RefinedMapping[S, O], S, O]
	         (original :M, op :OperationType,
	          includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]] = Nil) :Adapted[M] =
		new AlteredMapping[M, S, O](original, op, includes, excludes) with DelegateAdapter[M, S, O]

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''select'' clauses to include components on the `include` list and exclude all the components on the `exclude` 
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoSelectByDefault` to every component on the `exclude` list.
	  * Passing a non-column component applies the effect to its whole subtree, but changes made on a higher level
	  * have lower precedence than buff declarations on subcomponents. This means that certain portions of 
	  * the component tree may be not affected by this operation - if inherited buffs would conflict with those
	  * declared on a column/component, include also that subcomponent.
	  * @param original a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param include  a collection of components of `original`, which must not have 
	  *                 [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]] buff. The buff can be present
	  *                 however on their subcomponents, which will be simply unaffected by the change.
	  * @param exclude  a collection of components of `original`, which must have 
	  *                 [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] buff. It will be removed
	  *                 from these components and all their subcomponents which inherited it, but any additional,
	  *                 explicit declaration of this buff on their subcomponents will remain in effect (specifying
	  *                 that they, and their whole subtrees - barring additional buffs - should be included in
	  *                 the ''select'' clause.
	  * @param inferS   an implicit witness which guides the typer of the compiler to correctly infer the type
	  *                 arguments `S` and `O` given to `original` mapping.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from 
	  *         `NoSelectByDefault` buffs, and all components on the `excludes` list which didn't have that buff 
	  *         receive one.                 
	  */
	@throws[IllegalArgumentException]("if a mapping with NoSelect buff is present on the include list, " +
	                                  "or a mapping without OptionalSelect buff is present on the exclude list.")
	def select[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, SELECT, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''where'' clause to include components on the `include` list and exclude all the components on the `exclude` 
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoFilterByDefault` to every component on 
	  * the `exclude` list.
	  * Passing a non-column component applies the effect to its whole subtree, but changes made on a higher level
	  * have lower precedence than buff declarations on subcomponents. This means that certain portions of 
	  * the component tree may be not affected by this operation - if inherited buffs would conflict with those
	  * declared on a column/component, include also that subcomponent.
	  * @param original a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param include  a collection of components of `original`, which must not have 
	  *                 [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]] buff. The buff can be present
	  *                 however on their subcomponents, which will be simply unaffected by the change.
	  * @param exclude  a collection of components of `original`, which must have 
	  *                 [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff. It will be removed
	  *                 from these components and all their subcomponents which inherited it, but any additional,
	  *                 explicit declaration of this buff on their subcomponents will remain in effect (specifying
	  *                 that they, and their whole subtrees - barring additional buffs - should be included wherever
	  *                 this mapping's or its component's subject is compared in the ''where'' clause of an 
	  *                 SQL ''select''.
	  * @param inferS   an implicit witness which guides the typer of the compiler to correctly infer the type
	  *                 arguments `S` and `O` given to `original` mapping.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from 
	  *         `NoFilterByDefault` buffs, and all components on the `excludes` list which didn't have that buff 
	  *         receive one.                 
	  */
	@throws[IllegalArgumentException]("if a mapping with NoFilter buff is present on the include list, " +
		                              "or a mapping without OptionalFilter buff is present on the exclude list.")	
	def filter[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
	          (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, FILTER, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''Insert'' statements to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoInsertByDefault` to every component on
	  * the `exclude` list.
	  * Passing a non-column component applies the effect to its whole subtree, but changes made on a higher level
	  * have lower precedence than buff declarations on subcomponents. This means that certain portions of
	  * the component tree may be not affected by this operation - if inherited buffs would conflict with those
	  * declared on a column/component, include also that subcomponent.
	  * @param original a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param include  a collection of components of `original`, which must not have
	  *                 [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]] buff. The buff can be present
	  *                 however on their subcomponents, which will be simply unaffected by the change.
	  * @param exclude  a collection of components of `original`, which must have
	  *                 [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] buff. It will be removed
	  *                 from these components and all their subcomponents which inherited it, but any additional,
	  *                 explicit declaration of this buff on their subcomponents will remain in effect (specifying
	  *                 that they, and their whole subtrees - barring additional buffs - should be inserted by
	  *                 SQL ''inserts'' for this mapping.
	  * @param inferS   an implicit witness which guides the typer of the compiler to correctly infer the type
	  *                 arguments `S` and `O` given to `original` mapping.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from
	  *         `NoInsertByDefault` buffs, and all components on the `excludes` list which didn't have that buff
	  *         receive one.
	  */
	@throws[IllegalArgumentException]("if a mapping with NoInsert buff is present on the include list, " +
	                                  "or a mapping without OptionalInsert buff is present on the exclude list.")
	def insert[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, UPDATE, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''update'' statements to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoUpdateByDefault` to every component on
	  * the `exclude` list.
	  * Passing a non-column component applies the effect to its whole subtree, but changes made on a higher level
	  * have lower precedence than buff declarations on subcomponents. This means that certain portions of
	  * the component tree may be not affected by this operation - if inherited buffs would conflict with those
	  * declared on a column/component, include also that subcomponent.
	  * @param original a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param include  a collection of components of `original`, which must not have
	  *                 [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buff. The buff can be present
	  *                 however on their subcomponents, which will be simply unaffected by the change.
	  * @param exclude  a collection of components of `original`, which must have
	  *                 [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buff. It will be removed
	  *                 from these components and all their subcomponents which inherited it, but any additional,
	  *                 explicit declaration of this buff on their subcomponents will remain in effect (specifying
	  *                 that they, and their whole subtrees - barring additional buffs - should be updated by
	  *                 SQL ''updates'' for this mapping.
	  * @param inferS   an implicit witness which guides the typer of the compiler to correctly infer the type
	  *                 arguments `S` and `O` given to `original` mapping.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from
	  *         `NoUpdateByDefault` buffs, and all components on the `excludes` list which didn't have that buff
	  *         receive one.
	  */
	@throws[IllegalArgumentException]("if a mapping with NoUpdate buff is present on the include list, " +
	                                  "or a mapping without OptionalUpdate buff is present on the exclude list.")
	def update[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, INSERT, include, exclude)



	private[schema] def overrides[S, O]
	                    (original :RefinedMapping[S, O], op :OperationType,
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Overrides[O] =
		overrides(original, includes, excludes, op.prohibited, op.nonDefault, op.exclude, op.optional)


	private[schema] def overrides[S, O]
	                    (original :RefinedMapping[S, O],
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]],
	                     prohibited :BuffType, nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType)
			:Overrides[O] =
	{
		val excludeExports = excludes.map(original.export(_)).to(Unique)
		val includeExports = includes.map(original.export(_)).filterNot(excludeExports)
		val included = this.include(original, includeExports, prohibited, nonDefault)
		val excluded = this.exclude(original, excludeExports, nonDefault, exclude, optional)
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
	//  requires distinction between inherited and declared buffs
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


}






/** A mapping proxy with `includes` and `excludes` lists of components of the `backer` mapping.
  * It adapts each component of `backer` on any of these lists by modifying its
  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] in such a way that they are included/excluded
  * from all database operations where it is possible.
  * All components on the `included` list which do not carry
  * an `op.`[[net.noresttherein.oldsql.OperationType.prohibited prohibited]] buff for any operation
  * `op <: `[[net.noresttherein.oldsql.OperationType OperationType]], have all
  * `op.`[[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]] buffs removed.
  * This typically means removing `op.`[[net.noresttherein.oldsql.OperationType.explicit explicit]].
  * All components on the `excluded` list which carry an `op.`[[net.noresttherein.oldsql.OperationType.optional optional]],
  * but not an `op.`[[net.noresttherein.oldsql.OperationType.nonDefault nonDefault]],
  * (so, those which actually use [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
  * rather than [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]], for example),
  * receive a special `nonDefault` buff.  Other listed components are silently ignored.
  *
  * All components passed to public constructors (and public factory methods) are first substituted with their
  * ''export'' version for `mapping`.
  *
  * While this class overrides [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]`(adjustments)`
  * in order to combine the lists, with the selection type of the new argument always taking precedence over
  * the adjustments given as constructor arguments to this instance. This implementation is however very likely
  * to be overriden if a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]] is mixed in.
  * Two special late mix in traits however are provided with overrides of this method returning some `Mapping` subtype:
  * [[net.noresttherein.oldsql.schema.support.AdjustedMapping.AdjustedMappingMerger AdjustedMappingMerger]]
  * and [[net.noresttherein.oldsql.schema.support.AdjustedMapping.SpecificAdjustedMapping SpecificAdjustedMapping]].
  * @see [[net.noresttherein.oldsql.schema.Mapping.ComponentSelection]]
  */
class AdjustedMapping[+M <: RefinedMapping[S, O], S, O] private[schema]
      (mapping :M)(val includes :Unique[RefinedMapping[_, O]], val excludes :Unique[RefinedMapping[_, O]])
	extends AlteredMapping[M, S, O](mapping, AdjustedMapping.overrides(mapping, includes, excludes))
{
	private def this(mapping :M, includes :Iterable[RefinedMapping[_, O]])(excludes :Unique[RefinedMapping[_, O]]) =
		this(mapping)(includes.view.map(mapping.export(_)).filterNot(excludes.contains(_)).to(Unique), excludes)

	def this(mapping :M, includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]]) =
		this(mapping, includes)(excludes.view.map(mapping.export(_)).to(Unique))

	def this(mapping :M, adjustments :Iterable[ComponentSelection[_, O]]) =
		this(mapping, AdjustedMapping.includes(mapping, adjustments))(AdjustedMapping.excludes(mapping, adjustments))


	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :RefinedMapping[S, O] = {
		val newIncludes = include.view.map(dealias(_)).to(Unique)
		val newExcludes = exclude.view.map(dealias(_)).to(Unique)
		AdjustedMapping[M, S, O](backer,
			(includes.view ++ newIncludes).filterNot(newExcludes.contains).to(Unique),
			(excludes.view.filterNot(newIncludes.contains) ++ newExcludes).to(Unique)
		)
	}

}






object AdjustedMapping {
	import OperationType.operations

	private[schema] def apply[A <: RefinedMapping[S, O], S, O]
	                         (original :RefinedMapping[S, O], adjustments :Iterable[ComponentSelection[_, O]],
	                          alter :(Unique[RefinedMapping[_, O]], Unique[RefinedMapping[_, O]]) => A) :A =
	{
		val incl = includes(original, adjustments)
		val excl = excludes(original, adjustments)
		alter(incl, excl)
	}

	private[schema] def apply[A <: RefinedMapping[S, O], S, O]
	                         (original :RefinedMapping[S, O],
	                          include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]],
	                          alter :(Unique[RefinedMapping[_, O]], Unique[RefinedMapping[_, O]]) => A) :A =
	{
		val excl = exclude.view.map(original.export(_)).to(Unique)
		val incl = include.view.map(original.export(_)).filterNot(excl.contains(_)).to(Unique)
		alter(incl, excl)
	}



	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the column set used in all SQL operations
	  * to include or exclude listed components corresponding to the wrapper case class used.
	  * This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]], and so on,
	  * together with all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] those buffs),
	  * from the [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludeComponent]] instances,
	  * and adding buffs implying the above to every
	  * [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludeComponent]] instance which does not have
	  * a conflicting buff. These changes are independent for all operation types, possibly resulting in different
	  * column subsets being changed, and a failure to include/exclude component is not treated as an error,
	  * but silently ignored. Passing a non-column component applies the effect to its whole subtree, but changes made
	  * on a higher level have lower precedence than buff declarations on subcomponents. This means that certain
	  * portions of the component tree may be not affected by this operation - if inherited buffs would conflict
	  * with those declared on a column/component, specify also that subcomponent.
	  * @param original    a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param adjustments a list of components of `original` wrapped in one of the case classes extending
	  *                    [[net.noresttherein.oldsql.schema.Mapping.ComponentSelection ComponentSelection]]:
	  *                    [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludeComponent]] and
	  *                    [[net.noresttherein.oldsql.schema.Mapping.ExcludedComponent ExcludeComponent]].
	  *                    They may be created by no-arg methods [[net.noresttherein.oldsql.schema.Mapping.+ Mapping.+]]
	  *                    and [[net.noresttherein.oldsql.schema.Mapping.- Mapping.-]].
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the 'include' components and their subcomponents from `NoSelectByDefault` buffs and
	  *         its analogue for other operation types, and all components on the `excludes` list which didn't have
	  *         that buff receive one.
	  */
	def apply[M <: RefinedMapping[S, O], S, O]
	         (original :M, adjustments :Iterable[ComponentSelection[_, O]]) :Adapted[M] =
	{
		val includes = this.includes(original, adjustments)
		val excludes = this.excludes(original, adjustments)
		AdjustedMapping[M, S, O](original, includes, excludes)
	}


	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in all SQL
	  * operations to include components on the `include` list and exclude all the components on the `exclude`
	  * list, where existing buffs allow it. This is done by dropping all
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]], and so on,
	  * together with all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] those buffs),
	  * from the `include` list, and adding a buff implying the above to every component on the `exclude` list.
	  * These changes are independent for all operation types, possibly resulting in different column subsets
	  * being changed, and a failure to include/exclude component is not treated as an error, but silently ignored.
	  * Passing a non-column component applies the effect to its whole subtree, but changes made on a higher level
	  * have lower precedence than buff declarations on subcomponents. This means that certain portions of
	  * the component tree may be not affected by this operation - if inherited buffs would conflict with those
	  * declared on a column/component, specify also that subcomponent.
	  * @param original a mapping (any in theory, but in practice the 'root mapping for some table).
	  * @param include  a collection of components of `original`. It may include any components, with those
	  *                 already included or with all `NoXxx` buffs being unaffected.
	  * @param exclude  a collection of components of `original`. It may include any components, including those
	  *                 which are excluded by default and those which are prevented from being excluded by not having
	  *                 `OptionalXxx` buffs.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from
	  *         `NoXxxByDefault` buffs, and all components on the `excludes` list which didn't have that buffs,
	  *         but have any `OptionalXxx` receive matching ones.
	  */
	def apply[M <: RefinedMapping[S, O], S, O]
	         (original :M, includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Adapted[M] =
		new AdjustedMapping[M, S, O](original, includes, excludes) with DelegateAdapter[M, S, O]

	
	
	private[schema] def includes[O](mapping :RefinedMapping[_, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Unique[RefinedMapping[_, O]] =
		adjustments.view.collect { case IncludedComponent(c) => mapping.export(c) }.to(Unique)

	private[schema] def excludes[O](mapping :RefinedMapping[_, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Unique[RefinedMapping[_, O]] =
		adjustments.view.collect { case ExcludedComponent(c) => mapping.export(c) }.to(Unique)



	private[schema] def overrides[S, O]
	                    (original :RefinedMapping[S, O],
	                     includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]])
			:Overrides[O] =
	{
		val excludeExports = excludes.map(original.export(_)).to(Unique)
		val includeExports = includes.map(original.export(_)).filterNot(excludeExports)
		val included = include(original, includeExports)
		val excluded = exclude(original, excludeExports)
		included ++ excluded
	}

	private[schema] def overrides[S, O](original :RefinedMapping[S, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Overrides[O] =
	{
		val includes = this.includes(original, adjustments)
		val excludes = this.excludes(original, adjustments)
		overrides(original, includes, excludes)
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



	/** A mixin for [[net.noresttherein.oldsql.schema.support.AdjustedMapping AdjustedMapping]]
	  * subclasses which overrides [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]`(adjustments)`
	  * in order to return an `AlteredMapping` subclass `A[S]`, which is a composition of the present
	  * and given adjustments, and which delegates directly to this instance's backer, rather than this instance.
	  * Actual creation of instances of `A[S]` should happen in method
	  * [[net.noresttherein.oldsql.schema.support.AdjustedMapping.AdjustedMappingMerger.adjustedMapping adjustedMapping]],
	  * the implementation of which is left for subclasses. This trait implements only the logic of combining
	  * `includes` and `excludes` of this `AdjustedMapping` with the modification list given as the argument,
	  * in order to minimize the amount of code required of subclasses.
	  *
	  * This trait is typically mixed in after
	  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]] by adapter classes
	  * which combine [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]] with business interface `A`.
	  * @tparam A a type constructor (accepting `Subject` type) for a `Mapping` subtype which methods return always
	  *           instances of `A[_]`. This is typically some specialized business interface. Not that `backer`
	  *           is not required to implement `A[S]`, and this trait requires only a generic
	  *           [[net.noresttherein.oldsql.schema.Mapping.RefinedMapping RefinedMapping]].
	  * @tparam S [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of the `backer` of this trait and,
	  *           in effect, also this trait.
	  * @tparam O [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the `backer` of this trait and,
	  *           by extension, also this trait.
	  * @see [[net.noresttherein.oldsql.schema.support.AdjustedMapping.SpecificAdjustedMapping]]
	  */
	private[schema] trait AdjustedMappingMerger[+A[X] <: MappingFactoryMethods[A, X, O], S, O]
		extends MappingFactoryMethods[A, S, O]
	{ this :AdjustedMapping[RefinedMapping[S, O], S, O] => //self type, not extension because BaseMapping implements abstract methods from MappingFactoryMethods

		protected def adjustments :Seq[ComponentSelection[_, O]] =
			(includes.view.map(_.+) ++ excludes.view.map(_.-)).toSeq

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] = {
			val newIncludes = include.view.map(dealias(_)).to(Unique)
			val newExcludes = exclude.view.map(dealias(_)).to(Unique)
			adjustedMapping(
				(includes.view ++ newIncludes).filterNot(newExcludes.contains).to(Unique),
				(excludes.view.filterNot(newIncludes.contains) ++ newExcludes).to(Unique)
			)
		}

		protected def adjustedMapping(includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]]) :A[S]
	}



	/** A mixin for [[net.noresttherein.oldsql.schema.support.AdjustedMapping AdjustedMapping]]
	  * subclasses which overrides [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]`(adjustments)`
	  * in order to return an `AlteredMapping` subclass `A[S]`, which is a composition of the present
	  * and given adjustments, and which delegates directly to this instance's backer, rather than this instance.
	  * It takes advantage of the fact that `this.backer`'s `apply` method already returns an instance of `A[S]`
	  * and uses it for implementation.
	  *
	  * This trait is typically mixed in after
	  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter ComposedAdapter]] by adapter classes
	  * in order to avoid creating an adapter to an adapter.
	  * @tparam A a type constructor (accepting `Subject` type) for a `Mapping` subtype which methods return always
	  *           instances of `A[_]`. Almost always this be some specialized subtype of
	  *           [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]], extended also
	  *           by subclasses of this trait.
	  * @tparam M the type of the `backer` mapping to which this trait delegates.
	  * @tparam S [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type of the `backer` of this trait and,
	  *           in effect, also this trait.
	  * @tparam O [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the `backer` of this trait and,
	  *           by extension, also this trait.
	  * @see [[net.noresttherein.oldsql.schema.support.AdjustedMapping.AdjustedMappingMerger]]
	  */
	private[schema] trait SpecificAdjustedMapping
	                      [+A[X] <: MappingFactoryMethods[A, X, O], +M <: MappingFactoryMethods[A, S, O], S, O]
		   extends MappingFactoryMethods[A, S, O]
	{ this :AdjustedMapping[M, S, O] =>

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] = {
			val newIncludes = include.view.map(dealias(_)).to(Unique)
			val newExcludes = exclude.view.map(dealias(_)).to(Unique)
			backer(
				(includes.view ++ newIncludes).filterNot(newExcludes.contains),
				excludes.view.filterNot(newIncludes.contains) ++ newExcludes
			)
		}
	}
}

