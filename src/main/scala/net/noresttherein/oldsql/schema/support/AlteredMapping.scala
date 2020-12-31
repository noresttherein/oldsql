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






object AlteredMapping {
	type Override[S, O] = Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S]
	type Overrides[O] = NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component]

	
	def apply[M <: RefinedMapping[S, O], S, O]
	         (original :M, op :OperationType,
	          includes :Iterable[RefinedMapping[_, O]], excludes :Iterable[RefinedMapping[_, O]] = Nil) :Adapted[M] =
		new AlteredMapping[M, S, O](original, op, includes, excludes) with DelegateAdapter[M, S, O]

	def select[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, SELECT, include, exclude)

	def filter[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
	          (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, FILTER, include, exclude)

	def update[M <: RefinedMapping[S, O], S, O]
	          (original :M, include :Iterable[RefinedMapping[_, O]], exclude :Iterable[RefinedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, RefinedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](original, UPDATE, include, exclude)

	def insert[M <: RefinedMapping[S, O], S, O]
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


	
	
	def apply[M <: RefinedMapping[S, O], S, O]
	         (original :M, adjustments :Iterable[ComponentSelection[_, O]]) :Adapted[M] =
	{
		val includes = this.includes(original, adjustments)
		val excludes = this.excludes(original, adjustments)
		AdjustedMapping[M, S, O](original, includes, excludes)
	}

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

