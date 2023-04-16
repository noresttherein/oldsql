package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, SelectView, UpdateView}
import net.noresttherein.oldsql.collection.Opt
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.haul.ComponentValues
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.permutation
import net.noresttherein.oldsql.schema.{Mapping, MappingExtract}
import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, ExcludedComponent, IncludedComponent, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate
import net.noresttherein.oldsql.schema.support.{AlteredMapping, MappedMapping, MappingAdapter, PatchedMapping, PrefixedMapping, RenamedMapping, ReorderedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{Adapted, IdentityAdapter, MappedTo}






/** Base trait for client implementations of the `Mapping` type having subjects from the application domain model.
  * Being a static mapping implies that the component structure is determined by the mapping class, and included
  * components are not subject to conditional expressions. While column names and component buffs may differ,
  * if `m1.getClass == m2.getClass` for some `m1, m2 :StaticMapping[S, O]`, it is assumed that `m1` and `m2`
  * are ''isomorphic'' - they share the same subject type and there is a bijection between the components
  * of `m1` and `m2` which preserves the relations of being a subcomponent and being an export version of a component,
  * as well as extracts of corresponding components represent the same accessor functions of the subject. This is
  * reflected by the implementation of [[net.noresttherein.oldsql.schema.bases.StaticMapping.isomorphic isomorphic]]
  * and results in considering such mappings equivalent when used within SQL expressions.
  *
  * Extending classes typically provide individual access to its columns and components, with detailed types
  * for the latter, so they can be used as part of SQL expressions. For this reason, they are also typically accessible
  * through a stable path in the application code, adding further to their static nature. Results of parameterless
  * methods are typically cached/computed on initialization, and implementations are optimized for speed in use
  * at the cost of initialization time.
  *
  * Overrides `optionally` and `assemble`: the former for efficiency, making use of precomputed `Buff` data,
  * the latter for convenience, declaring `Pieces` as implicit and providing implicit conversion
  * from a `Component[T]` to `T`, allowing their use directly as constructor parameters to the assembled subject.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct]]
  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined]]
  */
trait StaticMapping[S, O]
	extends BaseMapping[S, O]
	   with StaticMappingTemplate[({ type A[M <: TypedMapping[S, O], s] = MappingAdapter[M, s, O] })#A, S, O]
{
	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :Adapted[this.type] =
		AlteredMapping[this.type, S, O](this :this.type, include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Adapted[this.type] =
		PatchedMapping[this.type, S, O](op, this :this.type, include, exclude)


	override def prefixed(prefix :String) :Adapted[this.type] =
		PrefixedMapping[this.type, S, O](prefix, this :this.type)

	override def renamed(naming :String => String) :Adapted[this.type] =
		RenamedMapping[this.type, S, O](this :this.type, naming)

	override def reorder(permutation :IndexedSeq[Int]) :Adapted[this.type] =
		if (permutation.length != columns.size)
			throw new IllegalArgumentException(
				"Length of permutation " + permutation + " (" + permutation.length +
					") does not match the number of columns " + columns.size + " in " + this + ": " + columns + "."
			)
		else if (permutation == permutation.indices)
			new IdentityAdapter(this)
		else
			ReorderedMapping[this.type, Subject, Origin](this :this.type, permutation)


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X] = null) :this.type MappedTo X =
		MappedMapping[this.type, S, X, O](this, there, back)

	override def isomorphic(that :Mapping) :Boolean = //extra checks as validation, in themselves they prove nothing
		getClass == that.getClass && subcomponents.size == that.subcomponents.size && (columns isomorphic that.columns)

	override def submappingOf(that :Mapping) :Boolean = that.getClass isAssignableFrom getClass
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
	  * [[net.noresttherein.oldsql.schema.support.MappingPrototype MappingPrototype]],
	  * but the adapter type parameter takes an additional parameter, the value of which is always given as `this.type`.
	  * Unfortunately this duplication can't be avoided by a shared implementation, as `this.type` cannot appear
	  * in a type signature making it necessary to accept an additional parameter, but, due to a scalac bug,
	  * this trait is not in practice covariant in its adapter type parameter (and thus cannot be safely mixed into
	  * classes designed for extension).
	  * @tparam A The type of mapping (typically
	  *           a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]] subtype)
	  *           returned by methods such as `prefixed`, `forSelect`, `map`, etc. Its first parameter
	  *           is always specified as `this.type` of this instance and defines the type of adapter's
	  *           [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]] property,
	  *           while the second and third are [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]]
	  *           and [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] types of the adapter.
	  *           It follows thus that the only valid argument for `A` is `this.Origin`.
	  * @tparam S The subject type of this mapping.
	  * @tparam O The origin type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping]]
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct]]
	  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined]]
	  *///extends Mapping, not BaseMapping as it narrows abstract declarations from Mapping which are implemented in BaseMapping
	trait StaticMappingTemplate[+A[M <: TypedMapping[S, O], s] <: TypedMapping[s, O], S, O]
		extends OptimizedMappingAssembly
	{ self =>
		override type Subject = S
		override type Origin  = O

		/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
		  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
		  * introducing default values and any manipulation of the final values. The standard implementation
		  * invokes [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct(pieces)]] as long as
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.isDefined isDefined(pieces)]] returns `true`.
		  * Additionally, all `NoSuchElementException` exceptions (thrown by default by components' `apply` methods
		  * when no value is preset or can be assembled) are caught and result in returning `Lack`. All other exceptions,
		  * including `NullPointerException` (which may also result from unavailable columns), are propagated.
		  * Subclasses should override those methods instead of `assemble`.
		  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
		  *        a `NoSuchElementException` is caught.
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.isDefined isDefined]]
		  */
		override def assemble(pieces: Pieces): Opt[S] =
			try {
				if (isDefined(pieces)) Got(construct(pieces))
				else Lack
			} catch {
				case _ :NoSuchElementException => Lack
			}

		/** The final target of the assembly process for this mapping invoked directly by `assemble` (and, indirectly,
		  * `optionally`/`apply`). It is left to be implemented by subclasses and, in order to make it the simplest possible,
		  * is not responsible for recognizing whether a value can be assembled, letting exceptions be thrown.
		  * Instead, this functionality is shared by the `isDefined` method - which can force `assemble` to return `None`
		  * without calling `construct` - and the catching of any `NoSuchElementException`s,
		  * resulting from a failed assembly of a subcomponent and thrown from this method, by `assemble`.
		  * Another difference from the latter is that `pieces` is declared as an implicit parameter,
		  * which coupled with an implicit conversion of `TypedMapping[T, O]` to `T` in its presence,
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

		/** Extends any `Component[T]` with an `apply()` method returning the value
		  * of the component from implicit [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]].
		  */
		@inline implicit final def componentExtension[T](component :Component[T]) :GetComponentValue[S, T, O] =
			new GetComponentValue[S, T, O](apply(component))



		override def apply(first :ComponentSelection[_, O], rest :ComponentSelection[_, O]*) :A[this.type, S] = {
			val all = rest.view prepended first
			apply(all collect { case IncludedComponent(c) => c }, all collect { case ExcludedComponent(c) => c })
		}

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S]

		override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			apply(SelectView, include, exclude)

		override def forFilter(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			apply(FilterView, include, exclude)

		override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			apply(InsertView, include, exclude)

		override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			apply(UpdateView, include, exclude)


		protected def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:A[this.type, S]



		override def qualified(prefix :String) :A[this.type, S] =
			if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

		override def prefixed(prefix :String) :A[this.type, S] = renamed (prefix + _)

		override def renamed(naming :String => String) :A[this.type, S]

		override def reorder(permutation :IndexedSeq[Int]) :A[this.type, S]

		override def reorder(precedes :(TypedColumn[_, O], TypedColumn[_, O]) => Boolean) :A[this.type, S] =
			reorder(permutation(columns.toIndexedSeq)(precedes))


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :A[this.type, X]

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :A[this.type, X] =
			as(there, back)

		override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
				:A[this.type, X] =
			as(there, back)

	}


	/** Extends any `Component[T]` of some `TypedMapping[S, O]` with an `apply()` method returning the value
	  * of the component from implicit [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]].
	  */
	class GetComponentValue[S, T, O](private val extract :MappingExtract[S, T, O]) extends AnyVal {

		/** The value of this component. If no value for this component is present in the implicit
		  * `ComponentValues`, it will be assembled using the component's
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
		  * @return `pieces(extract)`, where `extract` is
		  *         the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] for this component.
		  * @throws NoSuchElementException if no value is preset for the component and it cannot be assembled due
		  *                                to missing values for its subcomponents.
		  */
		def apply()(implicit pieces :ComponentValues[S, O]) :T = pieces(extract)

		/** The value of this component. If no value for this component is present in the implicit
		  * `ComponentValues`, it will be assembled using the component's
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method.
		  * @return `pieces(extract)`, where `extract` is
		  *         the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] for this component.
		  * @throws NoSuchElementException if no value is preset for the component and it cannot be assembled due
		  *                                to missing values for its subcomponents.
		  */
		def unary_~(implicit pieces :ComponentValues[S, O]) :T = pieces(extract)

		/** The value of this component in an `Option`. If no value for this component is present in the implicit
		  * `ComponentValues`, it will be assembled using the component's
		  * [[net.noresttherein.oldsql.schema.Mapping.optionally optionally]] method. If it cannot be assembled
		  * due to missing subcomponent values, `None` is returned.
		  * @return `pieces.get(extract)`, where `extract` is
		  *         the [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] for this component.
		  */
		def ?(implicit pieces :ComponentValues[S, O]) :Opt[T] = pieces.get(extract)
	}
}

