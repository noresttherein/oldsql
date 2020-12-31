package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, MappedTo}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate
import net.noresttherein.oldsql.schema.support.{AdjustedMapping, AlteredMapping, MappedMapping, MappingAdapter, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.MappingExtract


//here be implicits
import net.noresttherein.oldsql.slang._




/** Base trait for client implementations of the `Mapping` type having subjects from the application domain model.
  * Extending classes typically provide individual access to its columns and components, with detailed types
  * for the latter, so they can be used as part of SQL expressions. For this reason, they are also typically accessible
  * through a stable path in the application code, adding further to their static nature.
  * Overrides `optionally` and `assemble`: the former for efficiency, making use of precomputed `Buff` data,
  * the latter for convenience, declaring `Pieces` as implicit and providing implicit conversion
  * from a `Component[T]` to `T`, allowing their use directly as constructor parameters to the assembled subject.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct]]
  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined]]
  */
trait StaticMapping[S, O] extends BaseMapping[S, O]
	with StaticMappingTemplate[({ type A[M <: RefinedMapping[S, O], X] = MappingAdapter[M, X, O] })#A, S, O]
{
	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Adapted[this.type] =
		AdjustedMapping[this.type, S, O](this :this.type, include, exclude)

	protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Adapted[this.type] =
		AlteredMapping[this.type, S, O](this, op, include, exclude)


	override def prefixed(prefix :String) :Adapted[this.type] =
		PrefixedMapping[this.type, S, O](prefix, this :this.type)

	override def renamed(naming :String => String) :Adapted[this.type] =
		RenamedMapping[this.type, S, O](this :this.type, naming)


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X] = null) :this.type MappedTo X =
		MappedMapping[this.type, S, X, O](this, there, back)

}






object StaticMapping {

	/** Base trait for client mapping implementations having subjects from the application domain model,
	  * which are of some specific `Mapping` subclass that needs to be preserved when modifying/mapping
	  * this instance by the use of a custom `MappingAdapter` implementation. In addition to those adapting methods,
	  * it overrides `optionally` for efficiency (by lazily caching needed buffs) and `assemble` for convenience,
	  * by delegating to the
	  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]] method
	  * which declares the `Pieces` argument as implicit. In its presence, the components of this mapping are
	  * implicitly converted to their values when needed, allowing their use directly as arguments to the constructor
	  * of the subject entity.
	  *
	  * It is very similar to
	  * [[net.noresttherein.oldsql.schema.support.MappingFactoryMethods MappingAdapter.MappingFactoryMethods]],
	  * but the adapter type parameter takes an additional parameter, the value of which is always given as `this.type`.
	  * Unfortunately this duplication can't be avoided by a shared implementation, as `this.type` cannot appear
	  * in a type signature making it necessary to accept an additional parameter, but, due to a scalac bug,
	  * this trait is not in practice covariant in its adapter type parameter (and thus cannot be safely mixed into
	  * classes designed for extension).
	  * @tparam A the type of mapping (typically a `MappingAdapter` subtype) returned by the methods
	  *           such as `prefixed`, `forSelect`, `map`, etc. Its first parameter is given as `this.type`
	  *           of this instance, while the second is the subject type of the returned adapter mapping.
	  * @tparam S the subject type of this mapping.
	  * @tparam O the origin type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping]]
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct]]
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined]]
	  *///extends Mapping, not BaseMapping as it narrows abstract declarations from Mapping which are implemented in BaseMapping
	trait StaticMappingTemplate[+A[M <: RefinedMapping[S, O], X] <: RefinedMapping[X, O], S, O]
		extends OptimizedMappingAssembly
	{
		override type Subject = S
		override type Origin = O


		/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
		  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
		  * introducing default values and any manipulation of the final values. The standard implementation
		  * invokes [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct(pieces)]] as long as
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.isDefined isDefined(pieces)]] returns `true`.
		  * Additionally, all `NoSuchElementException` exceptions (thrown by default by components' `apply` methods
		  * when no value is preset or can be assembled) are caught and result in returning `None`. All other exceptions,
		  * including `NullPointerException` (which may also result from unavailable columns), are propagated.
		  * Subclasses should override those methods instead of `assemble`.
		  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
		  *        a `NoSuchElementException` is caught.
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined isDefined]]
		  */
		override def assemble(pieces: Pieces): Option[S] =
			try {
				isDefined(pieces) ifTrue construct(pieces)
			} catch {
				case _ :NoSuchElementException => None
			}

		/** The final target of the assembly process for this mapping invoked directly by `assemble` (and, indirectly,
		  * `optionally`/`apply`). It is left to be implemented by subclasses and, in order to make it the simplest possible,
		  * is not responsible for recognizing whether a value can be assembled, letting exceptions be thrown.
		  * Instead, this functionality is shared by the `isDefined` method - which can force `assemble` to return `None`
		  * without calling `construct` - and the catching of any `NoSuchElementException`s,
		  * resulting from a failed assembly of a subcomponent and thrown from this method, by `assemble`.
		  * Another difference from the latter is that `pieces` is declared as an implicit parameter,
		  * which coupled with an implicit conversion of `RefinedMapping[T, O]` to `T` in its presence,
		  * allows the use the components directly as arguments to the constructor of the returned subject instance.
		  * For example:
		  * {{{
		  *     case class Address(country :String, city :String, zip :String, street :String, no :String)
		  *
		  *     class Addresses[O] extends MappingFrame[Address, O] {
		  *         val country = column(_.country)
		  *         val city = column(_.city)
		  *         val zip = column(_.zip)
		  *         val street = column(_.street)
		  *         val no = column(_.no)
		  *
		  *         override def construct(implicit pieces :Pieces) :Address =
		  *             Address(country, city, zip, street, no)
		  *     }
		  * }}}
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined isDefined]]
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.assemble assemble]]
		  */
		protected def construct(implicit pieces :Pieces) :S

		/** Verifies the presence of necessary subcomponents in the input pieces for the assembly to be successful.
		  * This method is called from `assemble` in order to possibly prevent it from proceeding with the assembly
		  * by calling `construct`, and return `None` instead. The contract obliges it only detect the situations
		  * where `construct` would certainly fail with an exception, but not necessarily all of them. It is designed
		  * primarily with the thought of outer joins where all columns of a table can carry `null` values.
		  * For this reason, it simply always returns `true`, but entity tables override it with a check of availability
		  * of the primary key. The subclasses are free to implement any condition here.
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.assemble assemble]]
		  */
		protected def isDefined(implicit pieces :Pieces) :Boolean = true



		/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
		  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
		  */
		@inline implicit final def valueOf[T](component :Component[T])(implicit pieces :Pieces) :T =
			pieces(apply(component))

		@inline implicit final def componentExtension[T](component :Component[T]) :GetComponentValue[S, T, O] =
			new GetComponentValue[S, T, O](apply(component))



		override def apply(adjustments :ComponentSelection[_, O]*) :A[this.type, S] =
			apply(
				adjustments.view.collect { case IncludedComponent(c) => c },
				adjustments.view.collect { case ExcludedComponent(c) => c }
			)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S]

		override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			alter(SELECT, include, exclude)

		override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			alter(FILTER, include, exclude)

		override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			alter(UPDATE, include, exclude)

		override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			alter(INSERT, include, exclude)

		protected def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:A[this.type, S]



		override def qualified(prefix :String) :A[this.type, S] =
			if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

		override def prefixed(prefix :String) :A[this.type, S]

		override def renamed(naming :String => String) :A[this.type, S]


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :A[this.type, X]

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :A[this.type, X] =
			as(there, back)

		override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
				:A[this.type, X] =
			as(there, back)

	}


	/** Extends any `Component[T]` of some `RefinedMapping[S, O]` with an `apply()` method returning the value
	  * of the component from implicit [[ComponentValues ComponentValues]].
	  */
	class GetComponentValue[S, T, O](private val extract :MappingExtract[S, T, O]) extends AnyVal {

		/** The value of this component. If no value for this component is present in the implicit
		  * `ComponentValues`, it will be assembled using component's
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
		  * @return `pieces(extract)`, where `extract` is
		  *         the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] for this component.
		  * @throws NoSuchElementException if no value is preset for the component and it cannot be assembled due
		  *                                to missing values for its subcomponents.
		  */
		def apply()(implicit pieces :ComponentValues[S, O]) :T = pieces(extract)

		/** The value of this component in an `Option`. If no value for this component is present in the implicit
		  * `ComponentValues`, it will be assembled using the component's
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method. If it cannot be assembled
		  * due to missing subcomponent values, `None` is returned.
		  * @return `pieces.get(extract)`, where `extract` is
		  *         the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] for this component.
		  */
		def ?(implicit pieces :ComponentValues[S, O]) :Option[T] = pieces.get(extract)
	}
}

