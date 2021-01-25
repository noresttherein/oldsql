package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.{schema, OperationType}
import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnExtract, ColumnForm, ColumnMapping, Mapping, SQLForm}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StableColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.BaseMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn}
import net.noresttherein.oldsql.schema.support.AdjustedMapping.SpecificAdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.{ExportColumnAdapter, ExportComposedColumnAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.{DirectProxy, ExportColumnProxy, ShallowProxy}






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

	override def withBuffs(buffs :Buffs[S]) :MappingAdapter[M, S, O] = fail

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] = fail

	override def renamed(naming :String => String) :MappingAdapter[M, S, O] = fail

	override def as[X](there: S =?> X, back: X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] = fail

	private def fail :Nothing =
		throw new NotImplementedError("This method should have been overriden by MappingAdapter.BaseAdapter and inaccessible.")

	protected[oldsql] def every_concrete_MappingAdapter_must_extend_BaseAdapter :Nothing
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

		override def withBuffs(buffs :Buffs[S]) :MappingAdapter[M, S, O] = BuffedMapping(this, buffs)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
			new AdjustedMapping[this.type, S, O](this, include, exclude)
				with ComposedAdapter[M ,S, S, O] with SpecificAdjustedMapping[Adapter, this.type, S, O]

		protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:MappingAdapter[M, S, O] =
			new AlteredMapping[this.type, S, O](this, op, include, exclude) with ComposedAdapter[M, S, S, O]


		override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
			if (prefix.length == 0) this else PrefixedMapping(prefix, this)

		override def renamed(naming :String => String) :MappingAdapter[M, S, O] = RenamedMapping(this, naming)


		override def as[X](there: S =?> X, back: X =?> S)
		                  (implicit nulls :SQLForm.NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(this, there, back)


		protected[oldsql] override def every_concrete_MappingAdapter_must_extend_BaseAdapter :Nothing =
			throw new UnsupportedOperationException
	}



	/** A `MappingAdapter` mixin implementation for `DelegateMapping` subclasses, exposing its `backer`
	  * as the `body` property. For this reason, it should be mixed-in after the implementation `DelegateMapping`
	  * subtypes.
	  * @tparam M the adapted mapping type.
	  * @tparam S the subject type of this adapter.
	  * @tparam O the origin type of this adapter and the adapted mapping.
	  */
	trait DelegateAdapter[+M <: MappingAt[O], S, O] extends DelegateMapping[M, S, O] with BaseAdapter[M, S, O] {
		if (backer == null)
			throw new NullPointerException(
				getClass.toString + ".backer: DelegateAdapter trait was likely mixed in before backer field initialization."
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
	  * behaviour as defined by the [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]] trait
	  * and - possibly modified from the adapted column - buffs and form of this adapter. The original column
	  * is included only for show - the primary reason for this class is multiple inheritance scenario enforcing a type
	  * to be both a column and an adapter.
	  */
	trait ColumnAdapter[+M <: ColumnMapping[T, O], T, S, O]
		extends BaseAdapter[M, S, O] with ColumnMapping[S, O]
		   with ColumnMappingFactoryMethods[({ type C[X] = ColumnAdapter[M, T, X, O] })#C, S, O]
	{
		override def withBuffs(buffs :Buffs[S]) :ColumnAdapter[M, T, S, O] = copy(name, buffs)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :ColumnAdapter[M, T, S, O] =
			super[ColumnMappingFactoryMethods].apply(include, exclude)

		protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:ColumnAdapter[M, T, S, O] =
			super[ColumnMappingFactoryMethods].alter(op, include, exclude)


		override protected def thisColumn :ColumnAdapter[M, T, S, O] = this

		protected override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, T, S, O] =
			new ExportComposedColumnAdapter[M, T, S, O](this, name, buffs)

		override def prefixed(prefix :String) :ColumnAdapter[M, T, S, O] =
			super[ColumnMappingFactoryMethods].prefixed(prefix)

		override def renamed(naming :String => String) :ColumnAdapter[M, T, S, O] =
			super[ColumnMappingFactoryMethods].renamed(naming)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, T, X, O] =
			MappedMapping.columnAdapter[M, T, S, X, O](this, there, back)
	}



	object ColumnAdapter {

		/** A concrete, but not very useful by itself, implementation of `ColumnAdapter`. It is in fact a regular
		  * [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]], the behaviour of which
		  * is completely driven by its [[net.noresttherein.oldsql.schema.ColumnMapping.form form]].
		  * The 'adapted' column is exposed as `body`, but not used in any of the methods here.
		  * This class can be used where a type must be both
		  * a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]]
		  * and a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] and the adapted column is
		  * a `SimpleColumn` (which behaviour is likewise dependent only in its form). This is however risky,
		  * as the adapted column is not featured in the [[net.noresttherein.oldsql.schema.Mapping.extracts columnExtracts]]
		  * of this, and by extension, any enclosing mapping, meaning that passing it as an argument to any of the methods
		  * is likely to result in a `NoSuchElementException`. For this reason, this is mainly a base class from
		  * which more specialized adapter implementations are derived.
		  */
		class BaseColumnAdapter[+M <: ColumnMapping[T, O], T, S, O]
		                       (override val body :M, override val name :String, override val buffs :Buffs[S])
		                       (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[M, T, S, O] with StableColumn[S, O]



		/** A [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] adapter which considers itself
		  * the export version of the adapted column, with [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * methods and related reflecting this fact. It is the default class for proxies which must either
		  * expose the adapted mapping, or rely on the [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]]
		  * method rather than the [[net.noresttherein.oldsql.schema.ColumnMapping.selectForm selectForm]]
		  * of the adapted column for the assembly.
		  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.ExportComposedColumnAdapter]]
		  */
		class ExportColumnAdapter[+M <: ColumnMapping[S, O], S, O]
		                         (protected override val backer :M, override val name :String,
		                          override val buffs :Buffs[S])
		                         (implicit override val form :ColumnForm[S] = backer.form)
			extends ExportColumnProxy[S, O](backer, name, buffs)
			   with DelegateAdapter[M, S, O] with ColumnAdapter[M, S, S, O]
		{
			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, S, S, O] =
				new ExportColumnAdapter[M, S, O](backer, name, buffs)
		}


		/** An adapter of another column adapter, exposing the original column through its
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]] property, but delegating all
		  * methods to the adapter class. It considers itself the ''export'' version of the adapted adapter,
		  * as well as any other subcolumn that it might define and report itself as the export version of.
		  */
		class ExportComposedColumnAdapter[+M <: ColumnMapping[T, O], T, S, O]
		                                 (protected override val backer :ColumnAdapter[M, T, S, O],
		                                  override val name :String, override val buffs :Buffs[S])
			extends ExportColumnProxy[S, O](backer, name, buffs)
			   with ComposedAdapter[M, S, S, O] with ColumnAdapter[M, T, S, O]
		{
			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, T, S, O] =
				new ExportComposedColumnAdapter(backer, name, buffs)
		}



		/** A strange beast, being both a `ColumnMapping` (normally a value-oriented class with little polymorphism) and
		  * a `MappingAdapter`, containing some other column as a component (which columns normally don't).
		  * While nominally it is an adapter of the wrapped column, in reality all methods use the
		  * [[net.noresttherein.oldsql.schema.ColumnMapping.form form]] of that column, which remains unused.
		  * The adapted column `body` is included only in the `extracts` (and `columnExtracts`) maps,
		  * which are used only for aliasing to the ''export'' versions of the components,
		  * but not in  `components`/`subcomponents`.
		  * As the result, the adapted column is not technically a subcomponent, with only this instance being visible
		  * to containing mappings, and all operations modifying a component (such as prefixing the column names) apply
		  * only to this instance and return a new `SimpleColumnAdapter`, without modifying the `body`.
		  */
		class SimpleColumnAdapter[+M <: SimpleColumn[S, O], S, O](column :M, name :String, buffs :Buffs[S])
			extends BaseColumnAdapter[M, S, S, O](column, name, buffs)(column.form) with SimpleColumn[S, O]
		{
			def this(column :M) = this(column, column.name, column.buffs)

			private[this] val originalExtract = ColumnExtract.ident(body)

			override val extracts :NaturalMap[Component, ColumnExtract] = {
				//get the self extractor for ourselves and add one for the body column.
				val superExtracts = NaturalMap.single[Component, ColumnExtract, S](this, apply(this))
				superExtracts.updated[ColumnExtract, S](body :Component[S], originalExtract)
			}
			override val columnExtracts :NaturalMap[Column, ColumnExtract] =
				extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]

			override def apply[T](component :Component[T]) :Extract[T] =
				if (component eq body) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(component)

			override def apply[T](column :Column[T]) :ColumnExtract[T] =
				if (column eq body) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(column)

			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, S, S, O] =
				new SimpleColumnAdapter(body, name, buffs)

			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, S, X, O] =
				super[BaseColumnAdapter].as(there, back)
		}



		/** This class takes the degeneration of the `SimpleColumnAdapter` adapter one step further and only pretends
		  * to be an adapter - the `body` property points to this very instance. In all functionality this class
		  * behaves the same as a `StandardColumn`.
		  */
		class MockColumnProxy[S, O](override val name :String, override val buffs :Buffs[S])
		                           (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[ColumnMapping[S, O], S, S, O] with StableColumn[S, O]
		{
			override val body :ColumnMapping[S, O] = this

			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[ColumnMapping[S, O], S, S, O] =
				new MockColumnProxy[S, O](name, buffs)
		}

	}




	/** An adapter which changes nothing about the adapted mapping. It can be used where `Adapted[M]` (or similar)
	  * type is required, but the mapping should remain unmodified.
	  */
	class IdentityAdapter[M <: RefinedMapping[S, O], S, O](override protected val backer :M)
		extends DelegateAdapter[M, S, O] with DirectProxy[S, O]

}