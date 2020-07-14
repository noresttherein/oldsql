package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, SelectAudit}
import net.noresttherein.oldsql.schema.{Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, MappedMapping, MappingAdapter, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{Adapted, AdapterFactoryMethods, MappedTo}
import net.noresttherein.oldsql.schema.Mapping.{MappingSeal, RefinedMapping}
import net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters
import net.noresttherein.oldsql.slang._



/** Base trait for client implementations of the `Mapping` type having subjects from the application domain model.
  * Extending classes typically provide individual access to its columns and components, with detailed types
  * for the latter, so they can be used as part of SQL expressions. For this reason, they are also typically accessible
  * through a stable path in the application code, adding further to their static nature.
  * Overrides `optionally` and `assemble`: the former for efficiency, making use of precomputed `Buff` data,
  * the latter for convenience, declaring `Pieces` as implicit and providing implicit conversion
  * from a `Component[T]` to `T`, allowing their use directly as constructor parameters to the assembled subject.
  * @see [[net.noresttherein.oldsql.schema.support.MappingFrame]]
  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#construct]]
  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#isDefined]]
  */
trait StaticMapping[S, O] extends TypedMapping[S, O]
	with StaticMappingAdapters[({ type A[M <: RefinedMapping[S, O], X] = MappingAdapter[M, X, O] })#A, S, O]
{

	override protected def customize(include :Iterable[Component[_]], no :BuffType, explicit :BuffType,
	                                 exclude :Iterable[Component[_]], optional :BuffType, nonDefault :FlagBuffType)
			:Adapted[this.type] =
		CustomizedMapping.customize[this.type, S, O](this, include, no, explicit, exclude, optional, nonDefault)

	override def prefixed(prefix :String) :Adapted[this.type] =
		PrefixedMapping[this.type, S, O](prefix, this :this.type)

	override def renamed(name :String) :Adapted[this.type] =
		RenamedMapping[this.type, S, O](name, this :this.type)



	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X] = null) :this.type MappedTo X =
		MappedMapping[this.type, S, X, O](this :this.type, there, back)

}






object StaticMapping {

	/** Base trait for client mapping implementations having subjects from the application domain model,
	  * which are of some specific `Mapping` subclass that needs to be preserved when modifying/mapping
	  * this instance by the use of a custom `MappingAdapter` implementation. In addition to those adapting methods,
	  * it overrides `optionally` for efficiency (by lazily caching needed buffs) and `assemble` for convenience,
	  * by delegating to the
	  * [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#construct construct]] method
	  * which declares the `Pieces` argument as implicit. In its presence, the components of this mapping are
	  * implicitly converted to their values when needed, allowing their use directly as arguments to the constructor
	  * of the subject entity.
	  *
	  * It is very similar to
	  * [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdapterFactoryMethods MappingAdapter.AdapterFactoryMethods]],
	  * but the adapter type parameter takes an additional parameter which value is always given as `this.type`.
	  * Unfortunately this duplication can't be avoided by a common implementation, as `this.type` cannot appear
	  * in a type signature making it necessary to accept an additional parameter, but, due to a scalac bug,
	  * this trait is not in practice covariant in its adapter type parameter (and thus cannot be safely mixed into
	  * classes designed for extension).
	  * @tparam A the type of mapping (typically a `MappingAdapter` subtype) returned by the methods
	  *           such as `prefixed`, `forSelect`, `map`, etc. Its first parameter is given as `this.type`
	  *           of this instance, while the second is the subject type of the returned adapter mapping.
	  * @tparam S the subject type of this mapping.
	  * @tparam O the origin type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping]]
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#construct]]
	  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#isDefined]]
	  */
	trait StaticMappingAdapters[+A[M <: RefinedMapping[S, O], X] <: RefinedMapping[X, O], S, O] extends Mapping {
		this :MappingSeal =>

		override type Subject = S
		override type Origin = O


		override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
			case res if buffs.isEmpty => res
			case res :Some[S] =>
				if (audits.isEmpty) res else Some((res.get /: audits) { (acc, f) => f(acc) })
			case _ =>
				val res = default.get
				if (res.isDefined) res else explicit
		}

		private val audits = Lazy(SelectAudit.Audit(this))
		private val default = Lazy(OptionalSelect.Value(this))
		private val explicit = Lazy(ExtraSelect.Value(this))



		/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
		  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
		  * introducing default values and any manipulation of the final values. The standard implementation
		  * invokes [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct(pieces)]] as long as
		  * [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined(pieces)]] returns `true`. Additionally,
		  * all `NoSuchElementException` exceptions (thrown by default by components `apply` method when no value can
		  * be assembled or is preset) are caught and result in returning `None`. All other exceptions,
		  * including `NullPointerException` which may result from unavailable columns, are propagated. Subclasses should
		  * override those methods instead of `map`.
		  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
		  *        a `NoSuchElementException` is caught.
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined]]
		  */
		override def assemble(pieces: Pieces): Option[S] =
			try {
				isDefined(pieces) ifTrue construct(pieces)
			} catch {
				case _ :NoSuchElementException => None
			}

		/** The final target of the assembly process for this mapping invoked directly by `map` (and, indirectly,
		  * `optionally`/`apply`. It is left to implement for subclasses and, in order to make it the simplest possible,
		  * is not responsible for recognizing whether a value can be assembled, but rather this functionality is
		  * shared by `isDefined` method, which can force `map` to return `None` without calling `construct`,
		  * and catching later any `NoSuchElementException`s thrown from  this method and resulting from a failed assembly
		  * of a subcomponent. Another difference is that `pieces` is declared as an implicit parameter, which coupled
		  * with an implicit conversion of `RefinedMapping[O, T]` to `T` in its presence, allows to use the components directly
		  * as arguments to the constructor of the returned subject instance. For example:
		  * {{{
		  *     case class Address(country :String, city :String, zip :String, street :String, no :String)
		  *
		  *     class AddressSchema[O] extends StaticMapping[O, Address] {
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
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.isDefined isDefined]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.assemble assemble]]
		  */
		protected def construct(implicit pieces :Pieces) :S

		/** Verifies the presence of necessary subcomponents in the input pieces for the assembly to be successful.
		  * This method is called from `map` in order to possibly prevent it from proceeding with the assembly
		  * and  calling `construct`, but return `None` instead. The contract obliges it only detect the situations
		  * where `construct` would certainly fail with an exception, but not necessarily all of them. It is designed
		  * primarily with the thought of outer joins where all columns of a last can carry `null` values.
		  * For this reason, it simply always returns `true`, but entity tables override it with a check of availability
		  * of the primary key. The subclasses are free to implement any condition here.
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.assemble assemble]]
		  */
		protected def isDefined(implicit pieces :Pieces) :Boolean = true



		/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
		  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
		  */
		@inline implicit final def valueOf[T](component :Component[T])(implicit pieces :Pieces) :T =
			pieces(apply(component))



		override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(include, NoSelect, ExplicitSelect, exclude, OptionalSelect, NoSelectByDefault)

		override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(include, NoQuery, ExplicitQuery, exclude, OptionalQuery, NoQueryByDefault)

		override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(include, NoUpdate, ExplicitUpdate, exclude, OptionalUpdate, NoUpdateByDefault)

		override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(include, NoInsert, ExplicitInsert, exclude, OptionalInsert, NoInsertByDefault)

		protected def customize(include :Iterable[Component[_]], no :BuffType, explicit :BuffType,
		                        exclude :Iterable[Component[_]], optional :BuffType, nonDefault :FlagBuffType)
				:A[this.type, S]



		override def qualified(prefix :String) :A[this.type, S] =
			if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

		override def prefixed(prefix :String) :A[this.type, S]



		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :A[this.type, X]

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :A[this.type, X] =
			as(there, back)

		override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
				:A[this.type, X] =
			as(there, back)

	}

}

