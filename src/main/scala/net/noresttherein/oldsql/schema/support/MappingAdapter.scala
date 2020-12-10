package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.{schema, OperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, ColumnMapping, Mapping, SQLForm}
import net.noresttherein.oldsql.schema.ColumnMapping.StableColumn
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.support.AdjustedMapping.SpecificAdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy






sealed trait AdapterOf[+M <: Mapping] extends Mapping {
	val body :M
	override type Origin = body.Origin
}



/** A public interface of `Mapping` implementations based on some other, single mapping instance.
  * The original instance is exposed as a component of this mapping via the `body` property so that access to
  * the former's components is preserved. The adapted mapping and all its (sub)components are valid (sub)components
  * of this adapter. The contents of the `components`/`subcomponents` and individual column lists may vary, depending
  * on the adapter's function. Most often, unless it is this adapter's main purpose, its component and column lists
  * will consist exactly of the ''export'' versions of the corresponding list in the adapted mapping, but other schemes
  * are also possible, for example the `body` as the only direct component. The body's components might,
  * but do not have to, be ''export'' components of this mapping, as it is free to make any adjustments to them.
  * They have to however be recognized by the `apply` methods returning extracts and included in the `extracts` map.
  *
  * The type parameters of this trait and their bounds are the result of a need to cover various possible cases under
  * a common adapter type extending `BaseMapping` which is required by some classes rather than `RefinedMapping`.
  * For this reason the constraints are more lax than they ideally should be - in particular the mapping type `M`
  * should generally be of the same `Origin` type `O` as this adapter. Unfortunately, this in most cases results
  * in a duplication of the origin type in the type signature as a `M <: MappingAt[_]` type bound results in
  * issues with type unification of the type parameter at the use site (see the
  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type documentation for more information about this limitation).
  *
  * Client code should, if possible, use one of the type aliases defined in the companion object which propagate
  * some combination of the subject and origin types from the original mapping to the adapter.
  *
  * Most implementations will not extend this trait directly, but rather one of its subtypes:
  *   - [[net.noresttherein.oldsql.schema.support.MappingAdapter.BaseAdapter BaseAdapter]], which must be extended
  *     by all concrete classes extending `MappingAdapter`. This is because `BaseAdapter` enforces that the origin type
  *     of the adapted mapping is the same as the origin of the adapter, which is impossible to do here due to
  *     `MappingAdapter` being covariant in the adapted mapping type, but used in type aliases
  *     which define the adapter's origin type to be equal to the adapted mapping's origin type.
  *   - [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]] for adapters
  *     implemented as a [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]] to their `body`;
  *   - [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter ComposedAdapter]] for adapters
  *     of adapters or, more correctly, adapters implemented as a `DelegateMapping` to an adapter of their `body`.
  *
  * This trait overrides factory methods creating adapter mappings such as `map` so they return a `MappingAdapter`
  * to the same mapping type `M` as this mapping (rather than to this adapter as `StaticMapping`).
  *
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.BaseAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.AdaptedAt]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.MappedTo]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.Mapped]]
  * @author Marcin Mościcki
  */
trait MappingAdapter[+M <: Mapping, S, O]
	extends AdapterOf[M] with BaseMapping[S, O]
	   with MappingFactoryMethods[({ type A[X] = MappingAdapter[M, X, O] })#A, S, O]
{
	/** The adapted mapping. It is considered a valid component of this mapping. */
	val body :M { type Origin = O }

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] = fail

	override def prefixed(prefix :String) :MappingAdapter[M, S, O] = fail

	override def as[X](there: S =?> X, back: X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] = fail

	private def fail :Nothing =
		throw new NotImplementedError("This method should have been overriden by MappingAdapter.BaseAdapter and inaccessible.")

	protected[oldsql] def everyConcreteMappingAdapterMustExtendBaseAdapter :Nothing
}






object MappingAdapter {

	type Adapted[M <: Mapping] = MappingAdapter[M, M#Subject, M#Origin]

	type AdaptedAt[M[A] <: MappingAt[A], O] = MappingAdapter[M[O], M[O]#Subject, O]

	type MappedTo[M <: Mapping, S] = MappingAdapter[M, S, M#Origin]

	type Mapped[+M[A] <: MappingAt[A], S, O] = MappingAdapter[M[O], S, O]



	implicit def adapterProjection[M <: MappingAt[O], S, O](implicit body :ExactProjection[M])
			:ProjectionDef[MappingAdapter[M, S, O], ({ type P[X] = MappingAdapter[body.WithOrigin[X], S, X] })#P, S] =
		body.adapt[MappingAdapter, S, O]






	/** The actual base trait for all concrete `MappingAdapter` classes. It is separated from the `MappingAdapter`
	  * to enforce that the adapted mapping is of the same origin as this adapter, which is impossible in the latter
	  * due to its use in type aliases which define the `MappingAdapter`'s `Origin` type based on the origin type
	  * of the mapping `M`.
	  */
	trait BaseAdapter[+M <: MappingAt[O], S, O] extends MappingAdapter[M, S, O] { outer =>
		override val body :M

		private[this] type Adapter[X] = MappingAdapter[M, X, O]

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
			new AdjustedMapping[this.type, S, O](this, include, exclude)
				with ComposedAdapter[M ,S, S, O] with SpecificAdjustedMapping[Adapter, this.type, S, O]

		protected override def alter(op :OperationType,
		                             include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:MappingAdapter[M, S, O] =
			new AlteredMapping[this.type, S, O](this, op, include, exclude) with ComposedAdapter[M, S, S, O]


		override def prefixed(prefix :String) :MappingAdapter[M, S, O] = PrefixedMapping(prefix, this)


		override def as[X](there: S =?> X, back: X =?> S)
		                  (implicit nulls :SQLForm.NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(this, there, back)


		protected[oldsql] override def everyConcreteMappingAdapterMustExtendBaseAdapter :Nothing =
			throw new UnsupportedOperationException
	}



	/** A `MappingAdapter` mix-in implementation for `DelegateMapping` subclasses, exposing its `backer`
	  * as the `body` property. For this reason, it should be mixed-in after the implementation `DelegateMapping`
	  * subtypes.
	  * @tparam M the adapted mapping type.
	  * @tparam S the subject type of this adapter.
	  * @tparam O the origin type of this adapter and the adapted mapping.
	  */
	trait DelegateAdapter[+M <: MappingAt[O], S, O] extends DelegateMapping[M, S, O] with BaseAdapter[M, S, O] {
		if (backer == null)
			throw new NullPointerException(
				getClass.toString + ".backer: DelegateAdapter trait was likely mixed in before the backer field initialization."
			)

		override val body :M = backer
	}



	/** A `MappingAdapter` mix-in implementation for `DelegateMapping` subclasses which `backer` is a `MappingAdapter`
	  * itself. It exposes the `backer`'s `body` property as this adapter's `body`, while delegating all methods
	  * to the `backer` rather than `body`. As it accesses the `backer` property, it should be mixed-in
	  * after the implementation `DelegateMapping` subtypes.
	  * @tparam M the original adapted mapping which should be exposed as the adapted mapping.
	  * @tparam T the subject type of the adapter to which this adapter will delegate.
	  * @tparam S the subject type of this adapter.
	  * @tparam O the origin type of this adapter and the adapted mapping.
	  */
	trait ComposedAdapter[+M <: MappingAt[O], T, S, O]
		extends DelegateMapping[MappingAdapter[M, T, O], S, O] with BaseAdapter[M, S, O]
	{
		if (backer == null)
			throw new NullPointerException(
				getClass.toString + ".backer: ComposedAdapter trait was likely mixed in before the backer field initialization."
			)

		override val body :M = backer.body
	}



	/** A `MappingAdapter` to a `ColumnMapping` which is likewise a `ColumnMapping` itself.
	  * The default implementation will not delegate to the original column, but instead rely on the standard
	  * behaviour as defined by the `ColumnMapping` trait and - possibly modified from the adapted column -
	  * buffs and form of this adapter. The original column is included only for show - the primary reason
	  * for this class is multiple inheritance scenario enforcing a type to be both a column and an adapter.
	  */
	trait ColumnAdapter[M <: ColumnMapping[T, O], T, S, O]
		extends BaseAdapter[M, S, O] with ColumnMapping[S, O]
		   with ColumnMappingFactoryMethods[({ type C[X] = ColumnAdapter[M, T, X, O] })#C, S, O]
	{
		override val body :M

		override protected def thisColumn :ColumnAdapter[M, T, S, O] = this

		override protected def copy(name :String, buffs :Seq[Buff[S]]) :ColumnAdapter[M, T, S, O] =
			ColumnAdapter[M, T, S, O](body, name, buffs)(form)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, T, X, O] =
			ColumnAdapter[M, T, X, O](body, name, schema.mapBuffs(this)(there, back))(
				form.as(there)(back)
			)
	}



	object ColumnAdapter {

		def apply[S, O](name :String, buffs :Buff[S]*)(implicit form :ColumnForm[S])
				:ColumnAdapter[ColumnMapping[S, O], S, S, O] =
			new PseudoColumnProxy(name, buffs)(form)

		def apply[M <: ColumnMapping[S, O], S, O](column :M)
		                                         (name :String = column.name, buffs :Seq[Buff[S]] = column.buffs)
				:ColumnAdapter[M, S, S, O] =
			new ColumnFormAdapter[M, S, S, O](column, name, buffs)(column.form)


		def apply[M <: ColumnMapping[T, O], T, S, O](column :M, name :String, buffs :Seq[Buff[S]])
		                                            (implicit form :ColumnForm[S])
				:ColumnAdapter[M, T, S, O] =
			new ColumnFormAdapter[M, T, S, O](column, name, buffs)(form)



		class ColumnFormAdapter[M <: ColumnMapping[T, O], T, S, O]
		                       (override val body :M, override val name :String, override val buffs :Seq[Buff[S]])
		                       (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[M, T, S, O] with StableColumn[S, O]


		/** A strange beast, being both a `ColumnMapping` (normally a value-oriented class with little polymorphism) and
		  * an `MappingAdapter`, containing some other column as a component (which columns normally can't).
		  * While this breaks the column contract as documented (a column is its only component,
		  * present on the columns lists, but not `components`/`subcomponents`), with careful hiding of this fact
		  * it can remain compatible. This is done by including the adapted column `body` only in the `extracts`
		  * (and `columnExtracts`) maps, which are used only for aliasing to the ''export'' versions of the components.
		  * As the result, the adapted column is not exported, with only this instance being visible to containing
		  * mappings, and all operations modifying a component (such as prefixing the column names) apply only
		  * to this instance and return a new `ColumnFormWrapper`, without modifying the `body`.
		  * At the same time, just in case, the `optionally` method is overriden to check for values for the adapted
		  * columns and passing it to aliasing `ComponentValues` will not result in an exception, as the extract
		  * for it is provided.
		  */
		class ColumnFormWrapper[M <: ColumnMapping[S, O], S, O](column :M, name :String, buffs :Seq[Buff[S]])
		                                                       (implicit override val form :ColumnForm[S] = column.form)
			extends ColumnFormAdapter[M, S, S, O](column, name, buffs)
		{
			def this(column :M) = this(column, column.name, column.buffs)(column.form)

			override def optionally(pieces :Pieces) = super.optionally(pieces) match {
				case some :Some[S] => some
				case _ => body.optionally(pieces)
			}

			private[this] val originalExtract = ColumnExtract.ident(column)

			override val extracts = { //get the self extractor from StableColumn and add one for the body column.
				val superExtracts = NaturalMap.single[Component, ColumnExtract, S](this, apply(this))
				superExtracts.updated[ColumnExtract, S](body :Component[S], originalExtract)
			}
			override val columnExtracts = extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]

			override def apply[T](component :Component[T]) :Extract[T] =
				if (component eq column) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(component)

			override def apply[T](column :Column[T]) :ColumnExtract[T] =
				if (column eq this.column) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(column)

			override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnAdapter[M, S, S, O] =
				new ColumnFormWrapper(body, name, buffs)
		}



		/** This class takes the degeneration of the `ColumnFormWrapper` adapter one step further and only pretends
		  * to be an adapter - the `body` property points to this very instance. In all functionality this class
		  * behaves the same as a `StandardColumn`.
		  */
		class PseudoColumnProxy[S, O](override val name :String, override val buffs :Seq[Buff[S]])
		                             (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[ColumnMapping[S, O], S, S, O] with StableColumn[S, O]
		{
			override val body :ColumnMapping[S, O] = this

//			override def rename(name :String) :ColumnAdapter[ColumnMapping[S, O], S, S, O] =
//				new PseudoColumnProxy(name, buffs)(form)

			override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnAdapter[ColumnMapping[S, O], S, S, O] =
				new PseudoColumnProxy[S, O](name, buffs)
		}

	}




	/** An adapter which changes nothing about the adapted mapping. It can be used where `Adapted[M]` (or similar)
	  * type is required, but the mapping should remain unmodified.
	  */
	class IdentityAdapter[M <: RefinedMapping[S, O], S, O](override protected val backer :M)
		extends DelegateAdapter[M, S, O] with ShallowProxy[S, O]


}