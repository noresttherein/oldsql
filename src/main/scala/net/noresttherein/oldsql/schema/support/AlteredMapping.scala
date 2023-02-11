package net.noresttherein.oldsql.schema.support

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.Builder

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, SelectView, UpdateView}
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.schema.{babushkaSort, Buffs, MappingExtract}
import net.noresttherein.oldsql.schema.Buff.{BuffType, FlagBuffType, ValueBuffType}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.support.AlteredMapping.annulled
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AbstractDelegateAdapter, Adapted, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DeepProxy
import net.noresttherein.oldsql.schema.support.PatchedMapping.{overrides, Override, Overrides}






/** A `Mapping` proxy which substitutes some components of the original, while retaining the others.
  * This is typically used for modifying the buffs on some of the components or columns, by substituting components
  * of `backer` with adapters in the form of [[net.noresttherein.oldsql.schema.support.BuffedMapping BuffedMapping]]
  * or similar copies of columns for the same form. Most notably, it is used to alter the buffs defining
  * which columns should take part in which database operations, creating in the process a mapping which
  * ''by default'' includes/excludes certain columns which were excluded/included in the origin for the purpose
  * of a single operation type. The companion object contains factory methods which manipulate buffs controlling
  * the effective (default) column sets, the class itself isn't specific to that application and can be used
  * for other purposes by providing suitable `substitutions`.
  * @see [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]]
  * @see [[net.noresttherein.oldsql.OperationView]]
  * @author Marcin Mościcki
  */
class PatchedMapping[+M <: TypedMapping[S, O], S, O]
                    (protected override val backer :M,
                     substitutions :NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component])
	extends DeepProxy[S, O](backer) with DelegateMapping[M, S, O]
{
	def this(original :M, op :OperationView,
	         includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]]) =
		this(original, overrides(original, op, includes, excludes))

	substitutions.filter {
		entry => entry._1.isInstanceOf[TypedColumn[_, _]] != entry._2.isInstanceOf[TypedColumn[_, _]]
	} match {
		case empty if empty.isEmpty => ()
		case mismatches =>
			throw new IllegalArgumentException(
				"Cannot alter mapping " + backer + " by substituting components " + substitutions + ": " +
				"columns can be substituted only by columns; illegal substitutions: " + mismatches + "."
			)
	}

	protected override def adapt[T](component :backer.Component[T]) :Component[T] =
		substitutions.getOrElse(component, component)

	protected override def adapt[T](column :backer.Column[T]) :Column[T] =
		substitutions.getOrElse(column, column).asInstanceOf[Column[T]]


	override def apply[T](component :Component[T]) :Extract[T] = {
		val extract = extracts.getOrElse(component, null.asInstanceOf[Extract[T]])
		if (extract != null)
			extract
		else {
			val export = substitutions.getOrElse(component, null.asInstanceOf[Component[T]])
			if (export != null)
				MappingExtract(export)(extracts(export))
			else throw new NoSuchComponentException(
				"Mapping " + component + " is neither a component of the altered mapping " + backer +
				" nor does it have a defined substitute in " + substitutions + "."
			)
		}
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] =
		apply(column :Component[T]).asInstanceOf[ColumnExtract[T]]

	override def export[T](component :Component[T]) :Component[T] = {
		val extract = extracts.getOrElse(component, null.asInstanceOf[Extract[T]])
		if (extract != null)
			extract.export
		else {
			val export = substitutions.getOrElse(component, null.asInstanceOf[Component[T]])
			if (export != null) export
			else throw new NoSuchComponentException(
				"Mapping " + component + " is neither a component of the altered mapping " + backer +
				" nor does it have a defined substitute in " + substitutions + "."
			)
		}
	}

	override def export[T](column :Column[T]) :Column[T] = export(column :Component[T]).asInstanceOf[Column[T]]

	//no overrides of forSelect, forInsert, etc because there is no simple way to combine old overrides with new,
	//as the Overrides would be created with the assumption they are overriding the components from backer, not this
	//we'd need for this a mechanic of contradicting and disabling buffs

	override def mappingName :String = "Patched(" + backer.mappingName + ")"
}






/** A factory for mapping adapters which modify buffs on chosen components and columns in order to affect
  * if they should be used in a particular database operation. All columns are by default both eligible and mandatory
  * for all database operations and, in order to exclude a particular one - both completely and only by default -
  * they require [[net.noresttherein.oldsql.schema.Buff Buff]]s being applied. The exact buffs affected by this factory
  * are defined in every [[net.noresttherein.oldsql.OperationView OperationType]] instance:
  *   1. [[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] (for example,
  *      [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]]): completely disallows the use of a component
  *      in that operation type under any circumstances. Providing a component with this buff in the 'include' list
  *      will result in an `IllegalArgumentException`.
  *   1. [[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] (such as
  *      [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]): the buff which excludes
  *      the component from a particular operation type. All columns which do not carry this buff are included
  *      by default, and in most cases are impossible to use in this capacity without adapting the mapping first.
  *      It occurs typically in the form of [[net.noresttherein.oldsql.OperationView.Explicit Explicit]] buff
  *      ([[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]]), which can be removed
  *      by this procedure, making the component selectable (or insertable, updatable, etc) normally.
  *   1. [[net.noresttherein.oldsql.OperationView.Optional Optional]]: buffs such as
  *      [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] are the opposite of the `NonDefault`
  *      buffs: they mark components which can be omitted from the selected/updated column set. Listing a component
  *      without this buff in the 'excludes' argument will result in an `IllegalArgumentException`. At the same time,
  *      the buff provides a default value for the component which will be returned in place of the one
  *      in the table/entity object.
  *   1. [[net.noresttherein.oldsql.OperationView.Exclude Exclude]]: instead of replacing an `OptionalXxx`
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
  * active. Similarly, an inherited added `Exclude` buff is contradicted by an `Optional` buff coming before it
  * (that is, defined somewhere under the component tree of the component which is excluded) and the buffed column
  * must be individually specified in the 'Exclude' list to be omitted.
  *
  * Another consequence of buff inheritance and performing the changes on the level of single components
  * (rather than manually for the whole tree) is that passing to be included a component which already is included
  * by default has no effect; in particular, any its columns which are not included by default are unaffected
  * (and will not be included). The same goes for excluding components. On the other hand, if a column or component
  * under one of the listed components has the `Prohibited`/does not have the `Optional` buff, it will be ignored
  * as previously, without raising an error. An exception is thrown only if current buffs of the explicitly
  * listed components do not allow the changes to be made.
  *
  * It is important to note that this algorithm only simulates the effect that changing buffs on a component would
  * have on the buffs of its [[net.noresttherein.oldsql.schema.Mapping.export export]] subcomponents by a different
  * process. Mapping implementations are allowed to handle buffs differently, ignore them or implement completely
  * custom behaviour, which can potentially interfere with how this factory operates. In particular, non-standard
  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] methods can be bypassed.
  *
  * Remember also that [[net.noresttherein.oldsql.OperationView.Preset OperationType.Preset]] columns
  * (having o [[net.noresttherein.oldsql.schema.Buff.SelectPreset SelectPreset]] buff or its sibling) imply
  * `Prohibited` (`NoSelect`); despite taking part in every operation of the given type, they cannot appear
  * on the 'include' list, but rather are included automatically at a later stage.
  *
  * @see [[net.noresttherein.oldsql.schema.support.AlteredMapping]]
  * @see [[net.noresttherein.oldsql.schema.support.BuffedMapping]]
  * @see [[net.noresttherein.oldsql.schema.Buffs]]
  */
object PatchedMapping {
	/** A type alias for a single entry of a [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]]
	  * mapping one component [[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`[S, O]`
	  * to another of the same type.
	  */
	type Override[S, O] = Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S]

	/** A type alias for a [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]] mapping one components
	  * of a mapping with [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type `O` to others, preserving
	  * their [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
	  */
	type Overrides[O] = NaturalMap[MappingAt[O]#Component, MappingAt[O]#Component]

	/** A [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]] entry mapping `component` to `substitute`. */
	@inline def Override[S, O](component :TypedMapping[S, O], substitute :TypedMapping[S, O]) :Override[S, O] =
		Assoc[MappingAt[O]#Component, MappingAt[O]#Component, S](component, substitute)

	/** An empty [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]] mapping components of a mapping
	  * with `type `[[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] ` = O` to other mappings with the
	  * same [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] types.
	  */
	@inline def Overrides[O]() :Overrides[O] = NaturalMap.empty

	/** A singleton [[net.noresttherein.oldsql.collection.NaturalMap NaturalMap]] mapping a component of a mapping
	  * with `type `[[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] ` = O` to another mapping with the
	  * same [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type.
	  * This is a convenience method used to avoid explicitly providing type constructor parameters to `NaturalMap`.
	  */
	@inline def Overrides[S, O](component :TypedMapping[S, O], substitute :TypedMapping[S, O]) :Overrides[O] =
		NaturalMap.single[MappingAt[O]#Component, MappingAt[O]#Component, S](component, substitute)


	def apply[M <: TypedMapping[S, O], S, O](original :M, substitutions :Overrides[O]) :Adapted[M] =
		new PatchedMapping[M, S, O](original, substitutions)
			with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O]

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The goal is for all components on the `include` list to
	  * be included in operation `op`, while all columns on the `exclude` list to be excluded.
	  * @param original a mapping (any in theory, but in practice the 'root' mapping for some table).
	  * @param op       a type of database operation (`SelectView`, `InsertView`, etc).
	  * @param include a collection of components of `original`, which must not have
	  *                the [[net.noresttherein.oldsql.OperationView.Prohibited op.Prohibited]] buff (in any form).
	  * @param exclude a collection of components of `original`, with each item having
	  *                the [[net.noresttherein.oldsql.OperationView.Optional op.Optional]] buff.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the components on the `includes` list and their subcomponents from
	  *         the [[net.noresttherein.oldsql.OperationView.NonDefault op.NonDefault]] buffs, and all components on
	  *         the `excludes` list which didn't have the `NonDefault` buff receive one.
	  */
	@throws[IllegalArgumentException]("if a mapping with NoXxx buff is present on the include list, " +
	                                  "or a mapping without OptionalXxx buff is present on the exclude list.")
	def apply[M <: TypedMapping[S, O], S, O]
	         (op :OperationView, original :M,
	          include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil) :Adapted[M] =
		PatchedMapping[M, S, O](original, overrides(original, op, include, exclude))

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''select'' clauses to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoSelectByDefault` to every component on the `exclude` list.
	  * Components on the `include` list additionally receive
	  * an [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] buff if it was dropped together
	  * with `NoSelectByDefault`
	  * (presumably in the form of [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]]).
	  *
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
	def select[M <: TypedMapping[S, O], S, O]
	          (original :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, TypedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](SelectView, original, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''where'' clause to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoFilterByDefault` to every component on
	  * the `exclude` list. Components on the `include` list additionally receive
	  * an [[net.noresttherein.oldsql.schema.Buff.OptionalFilter OptionalFilter]] buff if it was dropped together
	  * with `NoFilterByDefault`
	  * (presumably in the form of [[net.noresttherein.oldsql.schema.Buff.ExplicitFilter ExplicitFilter]]).
	  *
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
	def filter[M <: TypedMapping[S, O], S, O]
	          (original :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
	          (implicit inferS :InferTypeParams[M, M, TypedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](FilterView, original, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''Insert'' statements to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoInsertByDefault` to every component on
	  * the `exclude` list. Components on the `include` list additionally receive
	  * an [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]] buff if it was dropped together
	  * with `NoInsertByDefault`
	  * (presumably in the form of [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert ExplicitInsert]]).

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
	def insert[M <: TypedMapping[S, O], S, O]
	          (original :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, TypedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](InsertView, original, include, exclude)

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in SQL
	  * ''update'' statements to include components on the `include` list and exclude all the components on the `exclude`
	  * list. This is done by dropping all [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]]
	  * buffs (and all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] this buff)
	  * from the `include` list, and adding a buff implying `NoUpdateByDefault` to every component on
	  * the `exclude` list. Components on the `include` list additionally receive
	  * an [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate OptionalUpdate]] buff if it was dropped together
	  * with `NoUpdateByDefault`
	  * (presumably in the form of [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate ExplicitUpdate]]).

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
	def update[M <: TypedMapping[S, O], S, O]
	          (original :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]] = Nil)
              (implicit inferS :InferTypeParams[M, M, TypedMapping[S, O]]) :Adapted[M] =
		apply[M, S, O](UpdateView, original, include, exclude)



	/** Creates a mapping with an isomorphic component structure to `mapping`, but on which all columns of the mirror
	  * component to `component` argument have [[net.noresttherein.oldsql.schema.Buff.NoSelect NoSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilter NoFilter]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsert NoInsert]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] buffs, preventing them (and, in the result,
	  * essentially also `component`) from being used as part of the mapping in any context.
	  * The above applies only if the component
	  * has [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoFilterByDefault NoFilterByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoUpdateByDefault NoUpdateByDefault]] buffs, respectively -
	  * it cannot exclude a mandatory component. The result will be a
	  * [[net.noresttherein.oldsql.schema.support.PatchedMapping PatchedMapping]] adapter to `mapping`,
	  * or `mapping` itself if no changes are necessary.
	  *
	  * This method is useful for example to guarantee that the ''having'' clause doesn't use any additional
	  * columns with respect to the ''group by'' clause.
	  */
	def prohibitIncluding[S, T, O](mapping :TypedMapping[S, O], component :TypedMapping[T, O])
			:TypedMapping[S, O] =
	{
		val ops = OperationView.operations.view
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]

		//Add a ProhibitedXxx if it has NoXxxByDefault
		def substitute[X](column :TypedColumn[X, O]) = {
			val export = mapping.export(column)
			val fixing = ops.filter(_.NonDefault.active(export)).map(_.Prohibited[X]).to(Buffs)
			if (fixing.nonEmpty)
				builder += Override(export, export.withBuffs(fixing +/: column.buffs))
		}
		component.columns.foreach(substitute(_))
		val substitutions = builder.result()

		if (substitutions.isEmpty) mapping else PatchedMapping[TypedMapping[S, O], S, O](mapping, substitutions)
	}



	private[schema] def overrides[S, O]
	                    (original :TypedMapping[S, O], op :OperationView,
	                     includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]])
			:Overrides[O] =
		overrides(original, includes, excludes, op.Prohibited, op.NonDefault, op.Exclude, op.Optional)


	private[schema] def overrides[S, O]
	                    (original :TypedMapping[S, O],
	                     includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]],
	                     prohibited :BuffType, nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType)
			:Overrides[O] =
	{
		val excludeExports = excludes.view.map(original.export(_)).to(ArraySeq)
		val includeExports = includes.view.map(original.export(_)).filterNot(annulled(excludes)(_)).to(ArraySeq)
		val included = this.include(original, includeExports, prohibited, optional, nonDefault)
		val excluded = this.exclude(original, excludeExports, nonDefault, exclude, optional)
		included ++ excluded
	}




	/** Modifies the buffs of the given components so that they are included by default in a particular operation.
	  * It removes the `Explicit` buff from all the components in the `components` collection and their subcomponents,
	  * but if the `Prohibited` buff is present on any of the components, an `IllegalArgumentException` is raised.
	  * If it is present on any of the subcomponents, the presence of `Explicit` buff is ignored and the subcomponent
	  * remains unchanged. If `Optional` is not present after that, than we add it to allow stacking of alterations
	  */
	private def include[O](mapping :TypedMapping[_, O], components :Seq[TypedMapping[_, O]],
	                       prohibited :BuffType, optional :BuffType, nonDefault :BuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- babushkaSort(components))
			include(mapping, component, prohibited, optional, nonDefault, builder)
		builder.result()
	}

	private def include[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                          prohibited :BuffType, optional :BuffType, nonDefault :BuffType,
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		component match { //already is export
			case comp if prohibited.active(comp) =>
				throw new IllegalArgumentException(
					s"Can't include component $component of $mapping as it has the $prohibited buff."
				)
			case column :TypedColumn[T, O] @unchecked =>
				if (nonDefault.active(column)) {
					val buffed = column.withBuffs(includedBuffs(column.buffs.declare(), optional, nonDefault))
					builder += Override(column, buffed)
				}
			case export =>
				if (nonDefault.active(export)) { //in practice, true if there is an ExplicitXxx buff
					val buffed = export.withBuffs(includedBuffs(export.buffs.declare(), optional, nonDefault))
					builder += Override[T, O](export, buffed)

					@inline def substitute[X](component :TypedMapping[X, O]) :Unit =
						builder += Override(component, buffed.export(component))
					export.extracts.keySet foreach { sub :TypedMapping[_, O] => substitute(sub) }
				}
		}

	/** Removes all `nonDefault` buffs from `buffs` and adds an `optional` buff, as long as one was present and
	  * it is either a `ValueBuffType` or a `FlagBuffType`.
	  */
	private[schema] def includedBuffs[T](buffs :Buffs[T], optional :BuffType, nonDefault :BuffType) = {
		val madeDefault = buffs.filter(nonDefault.inactive[T])
		if (optional.active(madeDefault))
			madeDefault
		else
			optional match {
				case valued :ValueBuffType => valued.get(buffs) match {
					case Got(buff) => buff.reassign(valued) +: madeDefault
					case _ => madeDefault
				}
				case flag :FlagBuffType => flag[T] +: madeDefault
				case _ => madeDefault
			}
	}

	/** Modifies the buffs of the given components so that they are not included by default in a particular operation.
	  * It adds the  `NonDefault` flag to all the components in the `components` collection and their subcomponents,
	  * but if the `Optional` buff is missing on any of the listed components, an `IllegalArgumentException` is raised.
	  * Subcomponents of the listed components without the `Optional` buff simply remain unchanged.
	  */
	private def exclude[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]],
	                       nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType)
			:Overrides[O] =
	{
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components; if !annulled(components)(component))
			this.exclude(mapping, component, nonDefault, exclude, optional, builder)
		builder.result()
	}

	private def exclude[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                          nonDefault :BuffType, exclude :FlagBuffType, optional :BuffType,
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
		component match {
			case comp if optional.inactive(comp) =>
				throw new IllegalArgumentException(
					s"Can't exclude component $comp of $mapping as it does not have the $optional buff."
				)
			case comp if nonDefault.active(comp) => //it already won't be included
			case column :TypedColumn[T, O] @unchecked =>
				builder += Override(column, column.withBuffs(Buffs.single(exclude[T]) +/: column.buffs))
			case export =>
				val buffed = export.withBuffs(Buffs.single(exclude[T]) +/: export.buffs)
				builder += Override[T, O](export, buffed)

				@inline def substitute[X](component :TypedMapping[X, O]) :Unit =
					builder += Override[X, O](component, buffed.export(component))
				export.extracts.keySet.foreach { sub :TypedMapping[_, O] => substitute(sub) }
		}

}






/** A mapping proxy with `includes` and `excludes` lists of components of the `backer` mapping.
  * It adapts each component of `backer` on any of these lists by modifying its
  * [[net.noresttherein.oldsql.schema.Mapping.buffs buffs]] in such a way that they are included/excluded
  * from all database operations where it is possible.
  * All components on the `included` list which do not carry
  * an `op.`[[net.noresttherein.oldsql.OperationView.Prohibited Prohibited]] buff for any operation
  * `op <: `[[net.noresttherein.oldsql.OperationView OperationType]], have all
  * `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]] buffs removed,
  * while preserving This typically means removing `op.`[[net.noresttherein.oldsql.OperationView.Explicit Explicit]].
  * All components on the `excluded` list which carry an `op.`[[net.noresttherein.oldsql.OperationView.Optional Optional]],
  * but not an `op.`[[net.noresttherein.oldsql.OperationView.NonDefault NonDefault]],
  * (so, those which actually use [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
  * rather than [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect ExplicitSelect]], for example),
  * receive a special `NonDefault` buff.  Other listed components are silently ignored.
  *
  * All components passed to public constructors (and public factory methods) are first substituted with their
  * ''export'' version in `mapping`.
  *
  * This class overrides [[net.noresttherein.oldsql.schema.Mapping.apply(adjustments* apply]]`(adjustments)`
  * in order to flatten the delegation chain, with the selection type of the new argument always taking precedence over
  * the adjustments given as constructor arguments to this instance. This implementation will however be overriden
  * if a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]] is mixed in.
  * A special late mix in traits is provided to counteract it, with an override of this method returning
  * a `MappingAdapter` to the original mapping:
  * [[net.noresttherein.oldsql.schema.support.AlteredMapping.AlteredMappingTemplate AlteredMappingTemplate]].
  * @param mapping  the altered mapping.
  * @param includes export components of `mapping` which should be adapted by dropping their `NoXxxByDefault` buffs.
  * @param excludes export components of `mapping` which should be adapted by adding `NoXxxByDefault` buffs for
  *                 all operation types.
  * @see [[net.noresttherein.oldsql.schema.Mapping.ComponentSelection]]
  */ //consider: renaming to MappingView
class AlteredMapping[+M <: TypedMapping[S, O], S, O] private
                    (mapping :M, val includes :Unique[TypedMapping[_, O]], val excludes :Unique[TypedMapping[_, O]],
                     substitutions :Overrides[O])
	extends PatchedMapping[M, S, O](mapping, substitutions)
{
	/** The `includes` and `excludes` arguments must contain only export components of `mapping`. */
	private[oldsql] def this(mapping :M)(includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]]) =
		this(mapping, includes, excludes, AlteredMapping.overrides(mapping, includes, excludes))

	private def this(mapping :M, includes :Iterable[TypedMapping[_, O]])(excludes :Unique[TypedMapping[_, O]]) =
		this(mapping)(includes.view.map(mapping.export(_)).filterNot(annulled(excludes)(_)).to(Unique), excludes)

//	private def this(mapping :M, selection :(Unique[TypedMapping[_, O]], Unique[TypedMapping[_, O]])) =
//		this(mapping)(selection._1, selection._2)

	def this(mapping :M, includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]]) =
		this(mapping, includes)(excludes.view.map(mapping.export(_)).to(Unique))

	def this(mapping :M, adjustments :Iterable[ComponentSelection[_, O]]) =
		this(mapping, AlteredMapping.includes(mapping, adjustments))(AlteredMapping.excludes(mapping, adjustments))

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :TypedMapping[S, O] =
		//we can't use AlteredMapping.stack because the collections may contain already changed components
		new AlteredMapping[TypedMapping[S, O], S, O](this, include, exclude) {
			private[this] val target = AlteredMapping.this.backer
			override def assemble(pieces :Pieces) :Opt[S] = target.assemble(pieces)
		}

	override def mappingName :String = {
		val included = includes.view.map("+" + _.mappingName).mkString(",")
		val excluded = excludes.view.map("-" + _.mappingName).mkString(",")
		backer.mappingName + "(" + included + (if (includes.nonEmpty && excludes.nonEmpty) "," else "") + excluded + ")"
	}
}






object AlteredMapping {
	import OperationView.operations

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the column set used in all SQL operations
	  * to include or exclude listed components corresponding to the wrapper case class used.
	  * This is done by replacing all [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] and so on,
	  * by [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]], etc.,
	  * dropping also all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] the former,
	  * from the [[net.noresttherein.oldsql.schema.Mapping.IncludedComponent IncludeComponent]] instances,
	  * and adding buffs implying the former to every
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
	  *                    and [[net.noresttherein.oldsql.schema.Mapping.- Mapping.-]], which can be also used as
	  *                    prefixes: [[net.noresttherein.oldsql.schema.Mapping.unary_+ +]]`component` and
	  *                    [[net.noresttherein.oldsql.schema.Mapping.unary_- -]]`component`.
	  * @return a `Mapping` which features new ''export'' versions of the given components (and their subcomponents),
	  *         which strip the 'include' components and their subcomponents from `NoSelectByDefault` buffs and
	  *         its analogue for other operation types, and all components on the `excludes` list which didn't have
	  *         that buff receive one.
	  */
	def apply[M <: TypedMapping[S, O], S, O]
	         (original :M, adjustments :Iterable[ComponentSelection[_, O]]) :Adapted[M] =
	{
		val includes = this.includes(original, adjustments)
		val excludes = this.excludes(original, adjustments)
		AlteredMapping[M, S, O](original, includes, excludes)
	}

	/** Create an adapter to mapping `original`, with the same column set and component structure, but with changes
	  * to effective buffs on the chosen components. The intent is to modify the default column set used in all SQL
	  * operations to include components on the `include` list and exclude all the components on the `exclude`
	  * list, where existing buffs allow it. This is done by replacing all
	  * [[net.noresttherein.oldsql.schema.Buff.NoSelectByDefault NoSelectByDefault]],
	  * [[net.noresttherein.oldsql.schema.Buff.NoInsertByDefault NoInsertByDefault]] and so on,
	  * by [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]],
	  * [[net.noresttherein.oldsql.schema.Buff.OptionalInsert OptionalInsert]], etc.,
	  * dropping also all buffs which [[net.noresttherein.oldsql.schema.Buff.BuffType.implies imply]] the former,*
	  * from the `include` list, and adding said buffs enforcing exclusion by default to every component on the `exclude`
	  * list, unless it doesn't have (directly or implied) an 'optional' buff for an operation.
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
	def apply[M <: TypedMapping[S, O], S, O]
	         (original :M, include :Iterable[TypedMapping[_, O]], exclude :Iterable[TypedMapping[_, O]])
			:Adapted[M] =
		new AlteredMapping[M, S, O](original, include, exclude)
			with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O] with AlteredMappingTemplate[M, S, O]



	/** A predicate testing if `components` contain a supercomponent of `component` (or `component` itself). */
	private[oldsql] def annulled[O](components :Iterable[TypedMapping[_, O]])(component :TypedMapping[_, O]) :Boolean =
		components.exists { c => c == component || c.contains(component.original) }


	private[oldsql] def stack[O](mapping :TypedMapping[_, O],
	                             oldIncludes :Unique[TypedMapping[_, O]], oldExcludes :Unique[TypedMapping[_, O]],
	                             includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]])
        :(Unique[TypedMapping[_, O]], Unique[TypedMapping[_, O]]) =
		stack(oldIncludes, oldExcludes,
			includes.view.map(mapping.export(_)).filterNot(annulled(excludes)(_)).to(Unique),
			excludes.view.map(mapping.export(_)).to(Unique)
		)

	private[oldsql] def stack[O](oldIncludes :Unique[TypedMapping[_, O]], oldExcludes :Unique[TypedMapping[_, O]],
	                             includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
		:(Unique[TypedMapping[_, O]], Unique[TypedMapping[_, O]]) =
		((oldIncludes.view ++ includes).filterNot(annulled(excludes)(_)).to(Unique),
		 (oldExcludes.view.filterNot(includes.toSet) ++ excludes).to(Unique))

/*
	private[oldsql] def stackSelections[O](mapping     :TypedMapping[_, O],
	                                       includes    :Unique[TypedMapping[_, O]],
	                                       excludes    :Unique[TypedMapping[_, O]],
	                                       overrides   :Overrides[O],
	                                       newIncludes :Unique[TypedMapping[_, O]],
	                                       newExcludes :Unique[TypedMapping[_, O]])
			:(Unique[TypedMapping[_, O]], Unique[TypedMapping[_, O]], Overrides[O]) =
	{ //assumes newIncludes do not contain subcomponents of newExcludes
		val oldIncludes = includes.filterNot(annulled(newExcludes)(_)) //components whose substitutions remain the same
		val oldExcludes = excludes.filterNot(newIncludes.toSet) //not annulled, because we allow excluding a subcomponent of an included component
		val extraIncludes = newIncludes.filterNot(oldIncludes.toSet) //components which need buff modifications
		val extraExcludes = newExcludes.filterNot(annulled(oldExcludes)(_))
		val allIncludes = oldIncludes ++ extraIncludes
		val allExcludes = oldExcludes ++ extraExcludes

		val newOverrides =
			AlteredMapping.include(mapping, extraIncludes) ++ AlteredMapping.exclude(mapping, extraExcludes)
		val (oldOverrides, changed) = overrides.partition {
			entry => oldIncludes.contains(entry.key) || oldExcludes.contains(entry.key)
		}
		val legacyOverrides = changed.map { entry =>
			def newOverride[V](entry :Override[V, O]) :Override[V, O] =
				Override(entry.key, newOverrides(entry.key))
			newOverride(entry)
		}
		val allOverrides = oldOverrides ++ legacyOverrides ++
			AlteredMapping.include(mapping, extraIncludes) ++ AlteredMapping.exclude(mapping, extraExcludes)

		(allIncludes, allExcludes, allOverrides)
	}
*/


	private[schema] def includes[O](mapping :TypedMapping[_, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Unique[TypedMapping[_, O]] =
		adjustments.view.collect { case IncludedComponent(c) => mapping.export(c) }.to(Unique)

	private[schema] def excludes[O](mapping :TypedMapping[_, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Unique[TypedMapping[_, O]] =
		adjustments.view.collect { case ExcludedComponent(c) => mapping.export(c) }.to(Unique)


	private[schema] def overrides[S, O](original :TypedMapping[S, O], adjustments :Iterable[ComponentSelection[_, O]])
			:Overrides[O] =
	{
		val includes = this.includes(original, adjustments)
		val excludes = this.excludes(original, adjustments)
		overrides(original, includes, excludes)
	}

	private[schema] def overrides[S, O]
	                    (original :TypedMapping[S, O],
	                     includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]])
			:Overrides[O] =
	{
		val excludeExports = excludes.view.map(original.export(_)).to(Unique)
		val includeExports = includes.view.map(original.export(_)).filterNot(annulled(excludeExports)(_)).to(Unique)
		overrides(original, includeExports, excludeExports)
	}

	private def overrides[S, O](original :TypedMapping[S, O],
	                            includes :Unique[TypedMapping[_, O]], excludes :Unique[TypedMapping[_, O]])
			:Overrides[O] =
	{
		val included = include(original, includes)
		val excluded = exclude(original, excludes)
		//Any buff with 'exclude' changes will replace its counterpart with 'include' changes, but the nature
		// of excluding is such that it effectively annuls the include changes, so the effect is the same.
		included ++ excluded
	}

	private def include[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]]) :Overrides[O] = {
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		//buffed subcomponents of an included component will be replaced by an independently buffed version
		// if it occurs in the include list, but the changes applied to buffs are incremental: they involve both
		// inherited and declared buffs, so we'll effectively just reintroduce the same changes that the previous
		// version inherited from its supercomponent.
		for (component <- babushkaSort(components.toSeq))
			include(mapping, component, builder)
		builder.result()
	}


	private def include[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
	{
		val excludedOps = operations.filter(o => o.Prohibited.inactive(component) && o.NonDefault.active(component))
		if (excludedOps.nonEmpty) {
			val withoutExcludes = component.buffs.filter { buff => excludedOps.forall(_.NonDefault.inactive(buff)) }
			val withOptional = (withoutExcludes.declare() /: excludedOps) { (buffs, op) =>
				if (op.Optional.active(buffs))
					buffs
				else
					op.Optional match {
						case valued :ValueBuffType => valued.get(component) match {
							case Got(buff) => buff.reassign(valued) +: buffs
							case _ => buffs
						}
						case flag :FlagBuffType => flag[T] +: buffs
						case _ => buffs
					}
			}
			val buffed = component.withBuffs(withOptional)
			builder += Override(component, buffed)

			@inline def substitute[X](subcomponent :TypedMapping[X, O]) :Unit =
				builder += Override(subcomponent, buffed.export(subcomponent))
			if (!component.isColumn)
				component.extracts.keySet foreach { subcomponent => substitute(subcomponent) }
		}
	}



	private def exclude[O](mapping :TypedMapping[_, O], components :Iterable[TypedMapping[_, O]]) :Overrides[O] = {
		val builder = NaturalMap.newBuilder[MappingAt[O]#Component, MappingAt[O]#Component]
		for (component <- components; if !annulled(components)(component))
			exclude(mapping, component, builder)
		builder.result()
	}


	private def exclude[T, O](mapping :TypedMapping[_, O], component :TypedMapping[T, O],
	                          builder :Builder[Override[_, O], Overrides[O]]) :Unit =
	{
		val optionalOps = operations.filter(o => o.Optional.active(component) && o.NonDefault.inactive(component))
		if (optionalOps.nonEmpty) {
			val withExcludes = Buffs(optionalOps.map(_.Exclude[T]) :_*) +/: component.buffs
			val buffed = component.withBuffs(withExcludes)
			builder += Override(component, buffed)

			@inline def substitute[X](subcomponent :TypedMapping[X, O]) :Unit =
				builder += Override(subcomponent, buffed.export(subcomponent))
			if (!component.isColumn)
				component.extracts.keySet foreach { subcomponent :TypedMapping[_, O] => substitute(subcomponent) }
		}
	}



	/** A mixin for [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]]
	  * subclasses which overrides [[net.noresttherein.oldsql.schema.Mapping.apply(include* apply]]`(include, exclude)`
	  * in order to return an AlteredMapping adapter to the original mapping
	  * (a [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter ComposedAdapter]]),
	  * which delegates directly to this instance's backer, rather than this instance.
	  */
	trait AlteredMappingTemplate[+M <: TypedMapping[S, O], S, O]
		extends AlteredMapping[TypedMapping[S, O], S, O] with AbstractDelegateAdapter[M, S, O]
	{
		override def assemble(pieces :Pieces) :Opt[S] = backer.assemble(pieces)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
			new AlteredMapping[MappingAdapter[M, S, O], S, O](this, include, exclude)
				with ComposedAdapter[M, S, S, O] with MappingDecorator[M, S, O] with AlteredMappingTemplate[M, S, O]
	}


	private[schema] class ComposedAlteredMapping[+M <: TypedMapping[S, O], S, O]
	                      (mapping :MappingAdapter[M, S, O],
	                       includes :Iterable[TypedMapping[_, O]], excludes :Iterable[TypedMapping[_, O]])
		extends AlteredMapping[MappingAdapter[M, S, O], S, O](mapping, includes, excludes)
		   with ComposedAdapter[M, S, S, O] with MappingDecorator[M, S, O]
	{
		override def assemble(pieces :Pieces) :Opt[S] = body.assemble(pieces)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
			new ComposedAlteredMapping[M, S, O](this, include, exclude)
	}

}

