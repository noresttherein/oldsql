package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, SelectView, UpdateView}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.permutation
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnMapping, Mapping}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue






 /* It is tempting to have Mapping extend this trait to avoid duplication of method declarations,
  * but it would be impossible to provide S and O parameters to this trait. These are in turn needed to specify
  * the Subject type of returned adapter mappings. An alternative would be to have A[_, S, O] instead of A[_, S]
  * not only this.Origin. However, BaseMapping requires M <: MappingAt[O], so anything that extends it
  * and MappingPrototype - like SchemaMappingAdapter - would need at the same time adapt a mapping of the same Origin,
  * but provide to MappingPrototype a type constructor for itself accepting an arbitrary origin, which would then
  * differ from the origin of the origin of mapping M in type A[s, o] = SchemaMappingAdapter[M, s, ... o].
  * At least we have the benefit of the ability to mix in this trait late and not be overridden by methods
  * with fixed return types.
  */
/** Implementations of `Mapping`'s methods which create an adapter to the original mapping,
  * such as `forSelect`, `prefixed`, `map`. All implemented methods return a specific mapping type `A[X, Origin]`.
  * @tparam A The type constructor for adapters to this mapping, accepting their `Subject` and `Origin` types.
  *           The only valid value for the second type parameter is `this.Origin`,
  *           and base [[net.noresttherein.oldsql.schema.Mapping Mapping]] trait extends
  *           `MappingPrototype[`[[net.noresttherein.oldsql.schema.Mapping.TypedMapping TypedMapping]]`]`, effectively
  *           defining the upper bound on this type parameter. Though the fact that `A[S, O] <: TypedMapping[S, Origin]`
  *           is not expressible here, it is relied upon by classes like
  *           [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate StaticMappingTemplate]].
  *           This superfluous parameter `O` is necessary because `Mapping` trait cannot possibly provide
  *           its `Origin` type as an argument to this trait because the former is not its type parameter.
  *           Actual `Mapping` subclasses designed to be extended by concrete application components - such as
  *           [[net.noresttherein.oldsql.schema.bases.StaticMapping StaticMapping]] class hierarchy - normally give
  *           `[S, O]MappingAdapter[this.type, S, O]` as this type argument.
  */ //todo: maybe +A[_ <: BaseMapping[S, O], s] will work (not TypedMapping[S, O])
trait MappingPrototype[+A[s] <: TypedMapping[s, O], S, O] extends Mapping {
	override type Subject = S
	override type Origin = O

	@inline private def thisPrototype :MappingPrototype[A, S, O] = this

	override def withBuffs(buffs :Buffs[Subject]) :A[S]

	override def withBuffs(buffs :Seq[Buff[Subject]]) :A[S] =
		thisPrototype.withBuffs(Buffs(refine, buffs :_*))

	override def apply(first :ComponentSelection[_, Origin], rest :ComponentSelection[_, Origin]*) :A[S] = {
		val all = rest.view prepended first
		apply(all collect { case IncludedComponent(c) => c }, all collect { case ExcludedComponent(c) => c })
	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S]

	override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		apply(SelectView, include, exclude)

	override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		apply(FilterView, include, exclude)

	override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		apply(InsertView, include, exclude)

	override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		apply(UpdateView, include, exclude)

	override protected def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:A[S]


	override def qualified(prefix :String) :A[S] =
		if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

	override def prefixed(prefix :String)  :A[S] = renamed(prefix + _)

	override def renamed(naming :String => String) :A[S]


	override def reorder(permutation :IndexedSeq[Int]) :A[S]

	@throws[IllegalArgumentException]("if there are two columns c1, c2 such that precedes(c1, c2) && precedes(c2, c1).")
	override def reorder(precedes :(TypedColumn[_, Origin], TypedColumn[_, Origin]) => Boolean) :A[S] =
		reorder(permutation(columns.toIndexedSeq)(precedes))


	override def as[X](there :Subject =?> X, back :X =?> Subject)(implicit nulls :NullValue[X]) :A[X]

	override def map[X](there :Subject => X, back :X => Subject)(implicit nulls :NullValue[X]) :A[X] =
		as(there, back)

	override def optMap[X](there :Subject => Option[X], back :X => Option[Subject])
	                      (implicit nulls :NullValue[X]) :A[X] =
		as(there, back)

}






/** Implementations of `TypedColumn`'s methods which create new columns by modifying the properties of this
  * column. Extended by the `TypedColumn` trait and all its subtypes which wish to return columns of a specific
  * subtype of `TypedColumn`. It implements the renaming, customizing and mapping method families by delegating
  * to the `copy` and `as` methods, left to implement for the extending class.
  * @tparam A a `TypedColumn` subtype returned by all the methods declared here.
  * @tparam S the subject type of this column.
  * @tparam O the origin type of this column.
  */ //todo: rename to ColumnMappingAdaptersTemplate, move to TypedColumn, no?
trait ColumnMappingPrototype[+A[s] <: TypedColumn[s, O], S, O] extends ColumnMapping with MappingPrototype[A, S, O] {

	/** A new column, with the same name, form (and optionally other properties), but with the buffs replaced by
	  * the provided list.
	  */
	override def withBuffs(buffs :Buffs[Subject]) :A[S] = copy(name, buffs)

	/** A new column, with the same name, form (and optionally other properties), but with the buffs replaced by
	  * the provided list.
	  */
	override def withBuffs(buffs :Seq[Buff[Subject]]) :A[S] =
		withBuffs(Buffs(refine, buffs :_*))


	override def apply(first :ComponentSelection[_, Origin], rest :ComponentSelection[_, Origin]*) :A[S] = {
		rest.view.prepended(first) foreach {
			case IncludedComponent(c) if contains(c) =>
			case IncludedComponent(c) =>
				throw new IllegalArgumentException("Cannot include " + c + " within column " + this + ".")
			case ExcludedComponent(c) =>
				throw new IllegalArgumentException("Cannot exclude " + c + " within column " + this + ".")
		}
		thisColumn
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
				val ops = OperationView.operations.filterNot(_.Prohibited.active(this))
				if (ops.nonEmpty)
					withBuffs(buffs.filter { buff => ops.forall { op => op.Preset.inactive(buff) } })
				else
					thisColumn
			} else
				throw new IllegalArgumentException(
					"Mapping " + include.head + " is not a component of column " + this + "."
				)
		else if (exclude.head == this) {
			val excludes = OperationView.operations.filter(_.Optional.active(this)).map(_.Exclude[Subject])
			if (excludes.nonEmpty) withBuffs(excludes ++: buffs.declared)
			else thisColumn
		} else
			throw new IllegalArgumentException(
				"Mapping " + exclude.head + " is not a component of column " + this + "."
			)


	protected override def apply(op :OperationView,
	                             include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
		if (include.size > 1)
			throw new IllegalArgumentException("Mappings " + include + " are not components of column " + this + ".")
		else if (exclude.size > 1)
			throw new IllegalArgumentException("Mappings " + exclude + " are not components of column " + this + ".")
		else if (exclude.headOption.contains(this) && op.Optional.active(this))
			withBuffs(op.Exclude[Subject] +: buffs.declared)
		else if (include.headOption.contains(this) && op.Explicit.active(this))
			withBuffs(buffs.filter(op.Explicit.inactive(_)))
		else
			thisColumn


	/** A column identical to this one as an instance of `A[S]`. Implemented as `copy(name, buffs)`, it
	  * should be overridden to `this` in classes conforming to `A[S]`. */
	protected def thisColumn :A[S] = copy(name, buffs)

	/** A column with the specified name and buffs, inheriting the form and any other properties from this
	  * instance. It is the target of the `prefixed`, `alter` and related methods.
	  */
	protected def copy(name :String, buffs :Buffs[Subject]) :A[S]

	/** A column with the specified name and buffs, inheriting the form and any other properties from this
	  * instance. It is the target of the `prefixed`, `alter` and related methods.
	  */
	protected def copy(name :String, buffs :Seq[Buff[Subject]]) :A[S] =
		copy(name, Buffs(buffs :_*))

	/** A column with exactly the same components, buffs and implementation as this one, but the new `name`. */
	def renamed(name :String) :A[S] = copy(name, buffs)

	/** A column with exactly the same components, buffs and implementation as this one, but the new `name`. */
	override def renamed(naming :String => String) :A[S] = copy(naming(name), buffs)

	override def prefixed(prefix :String) :A[S] =
		if (prefix.length == 0) thisColumn else renamed(prefix + name)

	def prefixed(prefix :Option[String]) :A[S] =
		if (prefix.isEmpty) thisColumn else prefixed(prefix.get)

	override def reorder(permutation :IndexedSeq[Int]) :A[S] =
		if (permutation.length != 1 || permutation.head != 0)
			throw new IllegalArgumentException(
				"The only valid permutation for a single column is identity Seq(0); got: " + permutation + "."
			)
		else thisColumn

}

