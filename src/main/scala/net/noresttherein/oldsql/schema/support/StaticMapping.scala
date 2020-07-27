package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, ExtraSelect, FlagBuffType, InsertAudit, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate, QueryAudit, SelectAudit, UpdateAudit}
import net.noresttherein.oldsql.schema.{Mapping, TypedMapping}
import net.noresttherein.oldsql.schema.bits.{CustomizedMapping, MappedMapping, MappingAdapter, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{Adapted, AdapterFactoryMethods, MappedTo}
import net.noresttherein.oldsql.schema.Mapping.{MappingSeal, RefinedMapping}
import net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters
import net.noresttherein.oldsql.slang._
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{INSERT, QUERY, SELECT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.schema.ComponentValues.ComponentValuesBuilder



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

	protected override def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:Adapted[this.type] =
		CustomizedMapping[this.type, S, O](this, op, include, exclude)


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
	  *///extends Mapping, not TypedMapping as it narrows abstract declarations from Mapping which are implemented in TypedMapping
	trait StaticMappingAdapters[+A[M <: RefinedMapping[S, O], X] <: RefinedMapping[X, O], S, O] extends Mapping {
		this :MappingSeal =>

		override type Subject = S
		override type Origin = O


		override def writtenValues[T](op :WriteOperationType, subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			op.writtenValues(this, subject, collector)

		override def queryValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isQueryable) {
				val audited = queryAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
					case Some(value) => comp.queryValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		override def updateValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isUpdatable) {
				val audited = updateAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
					case Some(value) => comp.updateValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		override def insertValues[T](subject :S, collector :ComponentValuesBuilder[T, O]) :Unit =
			if (isInsertable) {
				val audited = insertAudit(subject)
				def componentValues[X](comp :Component[X]) :Unit = apply(comp).get(audited) match {
					case Some(value) => comp.insertValues(value, collector)
					case _ =>
				}
				components foreach { c :Component[_] => componentValues(c) }
			}

		private val isQueryable = Lazy(NoQuery.disabled(this))
		private val isUpdatable = Lazy(NoUpdate.disabled(this))
		private val isInsertable = Lazy(NoInsert.disabled(this))
		private val queryAudit = Lazy(QueryAudit.fold(this))
		private val updateAudit = Lazy(UpdateAudit.fold(this))
		private val insertAudit = Lazy(InsertAudit.fold(this))



		override def optionally(pieces :Pieces) :Option[S] = pieces.assemble(this) match {
			case res if buffs.isEmpty => res
			case res :Some[S] =>
				if (isAuditable) Some(selectAudit(res.get)) else res
			case _ =>
				val res = default.get
				if (res.isDefined) res else explicit
		}

		private val isAuditable = Lazy(SelectAudit.enabled(this))
		private val selectAudit = Lazy(SelectAudit.fold(this))
		private val default = Lazy(OptionalSelect.Value(this))
		private val explicit = Lazy(ExtraSelect.Value(this))



		/** Performs the assembly of this mapping's subject from the components. This method is called in a double-dispatch
		  * from `optionally`/`apply`, which should be used by external mappings, as they are responsible for
		  * introducing default values and any manipulation of the final values. The standard implementation
		  * invokes [[net.noresttherein.oldsql.schema.support.StaticMapping#construct construct(pieces)]] as long as
		  * [[net.noresttherein.oldsql.schema.support.StaticMapping#isDefined isDefined(pieces)]] returns `true`. Additionally,
		  * all `NoSuchElementException` exceptions (thrown by default by components `apply` method when no value can
		  * be assembled or is preset) are caught and result in returning `None`. All other exceptions,
		  * including `NullPointerException` which may result from unavailable columns, are propagated. Subclasses should
		  * override those methods instead of `map`.
		  * @return `Some(construct(pieces))` if `isDefined(pieces)` returns `true` or `None` if it returns `false` or
		  *        a `NoSuchElementException` is caught.
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#construct construct]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#isDefined isDefined]]
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
		  * and catching later any `NoSuchElementException`s thrown from this method and resulting from a failed assembly
		  * of a subcomponent. Another difference is that `pieces` is declared as an implicit parameter, which coupled
		  * with an implicit conversion of `RefinedMapping[O, T]` to `T` in its presence, allows to use the components
		  * directly as arguments to the constructor of the returned subject instance. For example:
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
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#isDefined isDefined]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#assemble assemble]]
		  */
		protected def construct(implicit pieces :Pieces) :S

		/** Verifies the presence of necessary subcomponents in the input pieces for the assembly to be successful.
		  * This method is called from `map` in order to possibly prevent it from proceeding with the assembly
		  * and  calling `construct`, and return `None` instead. The contract obliges it only detect the situations
		  * where `construct` would certainly fail with an exception, but not necessarily all of them. It is designed
		  * primarily with the thought of outer joins where all columns of a table can carry `null` values.
		  * For this reason, it simply always returns `true`, but entity tables override it with a check of availability
		  * of the primary key. The subclasses are free to implement any condition here.
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#onstruct construct]]
		  * @see [[net.noresttherein.oldsql.schema.support.StaticMapping.StaticMappingAdapters#assemble assemble]]
		  */
		protected def isDefined(implicit pieces :Pieces) :Boolean = true



		/** Implicitly convert a component of this instance into its subject value by assembling it from implicitly
		  * available `Pieces` for this mapping. This will work for both direct components and indirect subcomponents.
		  */
		@inline implicit final def valueOf[T](component :Component[T])(implicit pieces :Pieces) :T =
			pieces(apply(component))



		override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(SELECT, include, exclude)

		override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(QUERY, include, exclude)

		override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(UPDATE, include, exclude)

		override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[this.type, S] =
			customize(INSERT, include, exclude)

		protected def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
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

