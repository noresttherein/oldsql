package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, RefinedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, Mapping}






/** Implementations of `Mapping`'s methods which create an adapter to the original mapping,
  * such as `forSelect`, `prefixed`, `map`. All implemented methods return a specific mapping type `A[X]`.
  */
trait MappingFactoryMethods[+A[X] <: RefinedMapping[X, O], S, O] extends Mapping {
	override type Subject = S
	override type Origin = O


	override def apply(adjustments :ComponentSelection[_, O]*) :A[S] =
		apply(
			adjustments.view.collect { case IncludedComponent(c) => c },
			adjustments.view.collect { case ExcludedComponent(c) => c }
		)

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S]

	/** @inheritdoc
	  * @return `alter(SELECT, include, exclude)`.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFactoryMethods.alter alter]] */
	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		alter(SELECT, include, exclude)

	/** @inheritdoc
	  * @return `alter(FILTER, include, exclude)`.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFactoryMethods.alter alter]] */
	override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		alter(FILTER, include, exclude)

	/** @inheritdoc
	  * @return `alter(UPDATE, include, exclude)`.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFactoryMethods.alter alter]] */
	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		alter(UPDATE, include, exclude)

	/** @inheritdoc
	  * @return `alter(INSERT, include, exclude)`.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingFactoryMethods.alter alter]] */
	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		alter(INSERT, include, exclude)

	/** Target method for `forSelect`, `forFilter`, `forUpdate` and `forInsert`. Responsible for creating an
	  * adapter (typically an [[net.noresttherein.oldsql.schema.support.AlteredMapping AlteredMapping]] subclass)
	  * with modified buffs on certain components so as to include or exclude them ''by default''.
	  * All components/columns which are not covered by either the `include` or the `exclude` list are left
	  * unmodified, meaning they will be included in the operation only if they would be included by this mapping
	  * by default.
	  * @param op      designator of the database operation type for which the mapping is being modified.
	  *                It is the source of buffs used in the process.
	  * @param include a list of (additional) components of this mapping to include in the operation.
	  *                Must not contain components having the `no` buff. All components on the list will
	  *                have their `explicit` buff removed (if present) and, if the included component
	  *                is not a column, all its subcomponents with the `explicit` buff will have that buff
	  *                removed.
	  * @param exclude a list of components which should be excluded from the operation. Must contain
	  *                components whose export versions have the `optional` buff. All components on this list
	  *                will receive the `nonDefault` buff (if not already present) and so will
	  *                all their subcomponents with the `optional` buff.
	  * @throws IllegalArgumentException if a component of `include` contains buff
	  *                                  `op.`[[net.noresttherein.oldsql.OperationType.prohibited prohibited]],
	  *                                  or a component of `exclude` does not contain buff
	  *                                  `op.`[[net.noresttherein.oldsql.OperationType.optional optional]].
	  */ //this may conflict with the extension method alter
	protected def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S]



	override def qualified(prefix :String) :A[S] =
		if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

	override def prefixed(prefix :String) :A[S] = renamed(prefix + _)

	override def renamed(naming :String => String) :A[S]



	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :A[X]

	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :A[X] =
		as(there, back)

	override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X]) :A[X] =
		as(there, back)

}






/** Implementations of `ColumnMapping`'s methods which create new columns by modifying the properties of this
  * column. Extended by the `ColumnMapping` trait and all its subtypes which wish to return columns of a specific
  * subtype of `ColumnMapping`. It implements the renaming, customizing and mapping method families by delegating
  * to the `copy` and `as` methods, left to implement for the extending class.
  * @tparam A a `ColumnMapping` subtype returned by all the methods declared here.
  * @tparam S the subject type of this column.
  * @tparam O the origin type of this column.
  */
trait ColumnMappingFactoryMethods[+A[X] <: ColumnMapping[X, O], S, O] extends MappingFactoryMethods[A, S, O] {

	protected def name :String

	protected def form :ColumnForm[S]

	/** A column identical to this one as an instance of `A[S]`. Implemented as `copy(name, buffs)`, it
	  * should be overriden to `this` in classes conforming to `A[S]`. */
	protected def thisColumn :A[S] = copy(name, buffs)

	/** A column with the specified name and buffs, inheriting the form and any other properties from this
	  * instance. It is the target of the `prefixed`, `alter` and related methods.
	  */
	protected def copy(name :String, buffs :Seq[Buff[S]]) :A[S]


	/** A new column, with the same name, form (and optionally other properties), but with the buffs replaced by
	  * the provided list.
	  */
	def withBuffs(buffs :Seq[Buff[S]]) :A[S] = copy(name, buffs)


	override def apply(adjustments :ComponentSelection[_, O]*) :A[S] =
		if (adjustments.size > 1)
			throw new IllegalArgumentException(
				"Multiple mappings listed for including/excluding: " + adjustments + " from " + this + "."
			)
		else if (adjustments.isEmpty)
			thisColumn
		else adjustments.head match {
			case IncludedComponent(c) if c == this =>
				val ops = OperationType.operations.filterNot(_.prohibited.enabled(this))
				if (ops.nonEmpty)
					withBuffs(buffs.filter { buff => ops.forall { op => op.extra.disabled(buff) } })
				else
	                thisColumn

			case ExcludedComponent(c) if c ==  this =>
				val excludes = OperationType.operations.filter(_.optional.enabled(this)).map(_.exclude[S])
				if (excludes.nonEmpty) withBuffs(excludes ++: buffs)
				else thisColumn

			case mod => throw new IllegalArgumentException(
				"Mapping " + mod.component + " is not a component of column " + this + "."
			)
		}


	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		if (include.size > 1 | exclude.size > 1)
			throw new IllegalArgumentException(
				exclude.mkString(
					include.mkString("Multiple mappings listed for including/excluding: +(", ", ", ") -("), ", ", ")."
				)
			)
		else if (exclude.isEmpty)
			if (include.isEmpty)
				thisColumn
			else if (include.head == this) {
				val ops = OperationType.operations.filterNot(_.prohibited.enabled(this))
				if (ops.nonEmpty)
					withBuffs(buffs.filter { buff => ops.forall { op => op.extra.disabled(buff) } })
				else
					thisColumn
			} else
				throw new IllegalArgumentException(
					"Mapping " + include.head + " is not a component of column " + this + "."
				)
		else if (exclude.head == this) {
			val excludes = OperationType.operations.filter(_.optional.enabled(this)).map(_.exclude[S])
			if (excludes.nonEmpty) withBuffs(excludes ++: buffs)
			else thisColumn
		} else
			throw new IllegalArgumentException(
				"Mapping " + exclude.head + " is not a component of column " + this + "."
			)


	protected override def alter(op :OperationType,
	                             include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		if (include.size > 1)
			throw new IllegalArgumentException("Mappings " + include + " are not components of column " + this + ".")
		else if (exclude.size > 1)
			throw new IllegalArgumentException("Mappings " + exclude + " are not components of column " + this + ".")
		else if (exclude.headOption.contains(this) && op.optional.enabled(this))
			withBuffs(op.exclude[S] +: buffs)
		else if (include.headOption.contains(this) && op.explicit.enabled(this))
			withBuffs(buffs.filter(op.explicit.disabled))
		else
			thisColumn



	/** A column with exactly the same components, buffs and implementation as this one, but the new `name`. */
	def rename(name :String) :A[S] = copy(name, buffs)

	/** A column with exactly the same components, buffs and implementation as this one, but the new `name`. */
	override def renamed(naming :String => String) :A[S] = copy(naming(name), buffs)

	override def prefixed(prefix :String) :A[S] =
		if (prefix.length == 0) thisColumn else rename(prefix + name)

	def prefixed(prefix :Option[String]) :A[S] =
		if (prefix.isEmpty) thisColumn else prefixed(prefix.get)

}

