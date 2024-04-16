package net.noresttherein.oldsql.schema.support

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.collection.{NaturalMap, Opt}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buffs, ColumnForm, Mapping, SQLForm, Seal}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnAt, SimpleColumn, StableColumn, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AbstractDelegateAdapter, BaseAdapter}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.ExportComposedColumnAdapter
import net.noresttherein.oldsql.schema.support.MappingDecorator.ExportDecorator
import net.noresttherein.oldsql.schema.support.MappingProxy.{DirectProxy, ExportColumnProxy}
import net.noresttherein.oldsql.schema.support.ReorderedMapping.ReorderedMappingComposedAdapter






//when we get rid of BaseMapping requirement on everything we can do:
//trait AbstractAdapter[+M <: Mapping] extends Mapping {
//	type Origin = body.Origin
//	type OriginalSubject = body.Subject
//	val body :M
//}
//type Adapted[+M <: Mapping] = AbstractAdapter[M] { type Subject = OriginalSubject }
//type MappedTo[+M <: Mapping, S] = AbstractAdapter[M] { type Subject = S }
//type AdaptedAt[+M[A] <: MappingAt[A], O] = AbstractAdapter[M[O]] { type Subject = OriginalSubject }
//type Mapped[+M[A] <: MappingAt[A], S, O] = AbstractAdapter[M[O]] { type Subject = S }

/** A public interface of `Mapping` implementations based on some other, single mapping instance.
  * The original instance is exposed as a component of this mapping via the `body` property so that access to
  * the former's components is preserved. The adapted mapping's instance is refined with a definition of
  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type equal to this object's `Origin` type.
  * For this reason, type `M` should ''not'' have any declarations for that type, unless more specific adapter
  * types ensure otherwise that `Origin` types match, and provide an implicit
  * [[net.noresttherein.oldsql.schema.Mapping.OriginProjection OriginProjection]] replacing all definitions
  * of `Origin` types.
  *
  * The adapted mapping and all its (sub)components are valid (sub)components
  * of this adapter. The contents of the `components`/`subcomponents` and individual column lists may vary, depending
  * on the adapter's function. Most often, unless it is this adapter's main purpose, its component and column lists
  * will consist exactly of the ''export'' versions of the corresponding list in the adapted mapping, but other schemes
  * are also possible, for example the `body` as the only direct component. The body's components might,
  * but do not have to, be ''export'' components of this mapping, as it is free to make any adjustments to them.
  * They have to however be recognized by the `apply` methods returning extracts and included in the `extracts` map.
  *
  * The type parameters of this trait and their bounds are the result of a need to cover various possible cases under
  * a common adapter type extending `BaseMapping` which is required by some classes rather than `TypedMapping`.
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
  *   - [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter ColumnAdapter]] for rare adapters
  *     which are column themselves.
  *
  * Some implementations of this trait are also [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]s,
  * but they form a separate branch of the type hierarchy and, unless explicitly documented or evident from
  * the class signature, all extending classes assume this is ''not'' the case in their implementation.
  *
  * This trait overrides factory methods creating adapter mappings such as `map` so they return a `MappingAdapter`
  * to the same mapping type `M` as this mapping (rather than to this adapter as `StaticMapping`).
  *
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.BaseAdapter]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.Adapted]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.AdaptedAt]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.MappedTo]]
  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.Mapped]]
  * @see [[net.noresttherein.oldsql.schema.support.DelegateMapping]]
  * @author Marcin Mościcki
  */ //consider: renaming to DerivedMapping and having MappingAdapter use the same subject type (like MappingProxy)
trait MappingAdapter[+M <: Mapping, S, O]
	extends BaseMapping[S, O] with MappingPrototype[({ type A[s] = MappingAdapter[M, s, O] })#A, S, O]
{ //todo: when requirement for BaseMapping is gone, implement methods here and get rid of BaseMappingAdapter

	/** The adapted mapping. It is considered a valid component of this mapping. If this class extends also
	  * [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]], it will in most cases be
	  * the same object as its [[net.noresttherein.oldsql.schema.support.DelegateMapping.backer backer]] property,
	  * but it doesn't need to be so: in particular 'adapters to adapters of a mapping `M`' (derived
	  * from [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter ComposedAdapter]])
	  * use the adapted adapter as their `backer`, but have the same original `body` properties os them.
	  */
	val body :M { type Origin = O }

	/** Converts a component of this mapping to a corresponding component of the backing mapping. If the argument
	  * already is a component of `body`, it is returned itself; otherwise a component of `body` which served
	  * as the base for the argument is returned, if it exists. In standard implementations, where only ''export''
	  * components of `body` are exported to components uniquely of this instance - and its non-export components
	  * are exported as the same component as their export version - the mapping returned is an ''export'' component.
	  * A complete inverse of the function applied to all components of `body` when exporting them as components
	  * of this instance might not exist - for example this adapter itself might not be an ''export'' version
	  * of the adapted mapping - in which case
	  * a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]] should be thrown.
	  * Definitions of this method are typically inherited from the methods of the same signature
	  * in a subclass of [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]], serving as
	  * the basis for this adapter's implementation. If this delegate mapping is
	  * a [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate ShallowDelegate]] -
	  * which uses the components of `body` as-is, the function is an identity on all of its domain (which might not
	  * be necessarily the complete component set of `body`), with a possible exception of this adapter itself.
	  */
	def unexport[X](component :Component[X]) :Component[X]


	/** Converts a column of this mapping to a corresponding column of the adapted mapping. If the argument
	  * already is a column of `body`, it is returned itself; otherwise a column of `body` which served
	  * as the base for the argument is returned, if it exists. In standard implementations, where only ''export''
	  * columns of `body` are exported as columns uniquely of this instance - and its non-export columns
	  * are exported as the same column as their export version - the object returned is an ''export'' column.
	  * A complete inverse of the function applied to all columns of `body` when exporting them by  this instance
	  * might not exist, in which case
	  * a [[net.noresttherein.oldsql.exceptions.NoSuchComponentException NoSuchComponentException]] should be thrown.
	  * Definitions of this method are typically inherited from the methods of the same signature
	  * in a subclass of [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]], serving as
	  * the basis for this adapter's implementation. If this delegate mapping is
	  * a [[net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate ShallowDelegate]] -
	  * which uses the components of `body` as-is, the function is an identity on all of its domain (which might not
	  * be necessarily the complete component set of `body`), with a possible exception of this adapter itself.
	  */
	def unexport[X](column :Column[X]) :Column[X]


	override def withBuffs(buffs :Buffs[S]) :MappingAdapter[M, S, O] = fail

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] = fail

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:MappingAdapter[M, S, O] =
		fail

	override def renamed(naming :String => String) :MappingAdapter[M, S, O] = fail

	override def reorder(permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] = fail

	override def as[X](there: S =?> X, back: X =?> S)(implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] = fail


	@inline private def fail :Nothing =
		throw new NotImplementedError("This method should have been overridden by MappingAdapter.BaseAdapter and inaccessible.")

	protected[oldsql] def every_concrete_MappingAdapter_must_extend_BaseAdapter(seal :Seal) :Unit

}






object MappingAdapter {

	type Adapted[M <: Mapping] = MappingAdapter[M, M#Subject, M#Origin]
	//todo: rename to AdaptedFor/AdaptedTo
	type AdaptedAt[M[A] <: MappingAt[A], O] = MappingAdapter[M[O], M[O]#Subject, O]
	//todo: rename to MappedAs
	type MappedTo[M <: Mapping, S] = MappingAdapter[M, S, M#Origin]

	type Mapped[+M[A] <: MappingAt[A], S, O] = MappingAdapter[M[O], S, O]


	implicit def adapterProjection[M <: MappingAt[O], S, O](implicit body :ExactProjection[M])
			:ProjectionDef[MappingAdapter[M, S, O], ({ type P[X] = MappingAdapter[body.WithOrigin[X], S, X] })#P, S] =
		body.adapt[MappingAdapter, S, O]



	/** The actual base trait for all concrete `MappingAdapter` classes. It is separated from the `MappingAdapter`
	  * to enforce that the adapted mapping is of the same origin as this adapter, which is impossible in the latter
	  * due to its use in type aliases which define the `MappingAdapter`'s `Origin` type based on the origin type
	  * of the mapping `M`.
	  */ //consider: trying to use M[O] <: MappingAt[O] instead
	trait BaseAdapter[+M <: MappingAt[O], S, O] extends MappingAdapter[M, S, O] { outer =>
		override val body :M

		override def withBuffs(buffs :Buffs[S]) :MappingAdapter[M, S, O] = BuffedMapping(this, buffs)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
			AlteredMapping(this, include, exclude)

		protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:MappingAdapter[M, S, O] =
			PatchedMapping(op, this, include, exclude)

		override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
			if (prefix.length == 0) this else PrefixedMapping(prefix, this)

		override def renamed(naming :String => String) :MappingAdapter[M, S, O] = RenamedMapping(this, naming)

		override def reorder(permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] = {
			ReorderedMapping.validatePermutation(this, permutation)
			if (permutation == permutation.indices)
				this
			else
				ReorderedMapping(this, permutation)
		}


		override def as[X](there: S =?> X, back: X =?> S)
		                  (implicit nulls :SQLForm.NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(this, there, back)


		protected[oldsql] override def every_concrete_MappingAdapter_must_extend_BaseAdapter(seal :Seal) :Unit = ()
	}



	/** A late mixin trait for [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]]
	  * implementations which extend also [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]]
	  * for their implementation. The relation between [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]]
	  * and [[net.noresttherein.oldsql.schema.support.DelegateMapping.backer backer]] properties is unspecified.
	  * This trait simply overrides the `unexport` methods to make them public, assuming a `DelegateMapping`
	  * with their implementation has been extended/mixed in earlier (they are `abstract override` calls to `super`).
	  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter DelegateAdapter]]
	  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter ComposedAdapter]]
	  */
	trait AbstractDelegateAdapter[+M <: MappingAt[O], S, O]
		extends DelegateMapping[MappingAt[O], S, O] with BaseAdapter[M, S, O]
	{
		//resolve the conflict between MappingAdapter and DelegateMapping
		abstract override def unexport[X](component :Component[X]) :Component[X] = super.unexport(component)
		abstract override def unexport[X](column :Column[X]) :Column[X] = super.unexport(column)
	}


	/** A `MappingAdapter` mixin implementation for `DelegateMapping` subclasses, exposing its `backer`
	  * as the `body` property. For this reason, it should be mixed-in after the implementation `DelegateMapping`
	  * subtypes.
	  * @tparam M the adapted mapping type.
	  * @tparam S the subject type of this adapter.
	  * @tparam O the origin type of this adapter and the adapted mapping.
	  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter]]
	  */
	trait DelegateAdapter[+M <: MappingAt[O], S, O]
		extends DelegateMapping[M, S, O] with AbstractDelegateAdapter[M, S, O]
	{
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
	  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter]]
	  */
	trait ComposedAdapter[+M <: MappingAt[O], T, S, O]
		extends DelegateMapping[MappingAdapter[M, T, O], S, O] with AbstractDelegateAdapter[M, S, O]
	{
		if (backer == null)
			throw new NullPointerException(
				getClass.toString + ".backer: ComposedAdapter trait was likely mixed in before the backer field initialization."
			)

		override val body :M = backer.body
	}






	/** A `MappingAdapter` to a `TypedColumn` which is likewise a `TypedColumn` itself.
	  * The default implementation will not delegate to the original column, but instead rely on the standard
	  * behaviour as defined by the [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]] trait
	  * and - possibly modified from the adapted column - buffs and form of this adapter. The original column
	  * is included only for show - the primary reason for this class is multiple inheritance scenario enforcing a type
	  * to be both a column and an adapter.
	  */ //todo: move to top level
	trait ColumnAdapter[+M <: ColumnAt[O], S, O]
		extends BaseAdapter[M, S, O] with BaseColumn[S, O]
		   with ColumnMappingPrototype[({ type C[s] = ColumnAdapter[M, s, O] })#C, S, O]
	{
		override def withBuffs(buffs :Buffs[S]) :ColumnAdapter[M, S, O] = copy(name, buffs)

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :ColumnAdapter[M, S, O] =
			super[ColumnMappingPrototype].apply(include, exclude)

		protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:ColumnAdapter[M, S, O] =
			super[ColumnMappingPrototype].apply(op, include, exclude)


		protected override def thisColumn :ColumnAdapter[M, S, O] = this

		protected override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, S, O] =
			new ExportComposedColumnAdapter[M, S, O](this, name, buffs)

		override def prefixed(prefix :String) :ColumnAdapter[M, S, O] =
			super[ColumnMappingPrototype].prefixed(prefix)

		override def renamed(naming :String => String) :ColumnAdapter[M, S, O] =
			super[ColumnMappingPrototype].renamed(naming)

		override def reorder(permutation :IndexedSeq[Int]) :ColumnAdapter[M, S, O] =
			super[ColumnMappingPrototype].reorder(permutation)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, X, O] =
			MappedMapping.columnAdapter[M, S, X, O](this, there, back)
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
		abstract class BaseColumnAdapter[+M <: ColumnAt[O], S, O]
		                                (override val body :M, override val name :String, override val buffs :Buffs[S])
		                                (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[M, S, O] with StableColumn[S, O]



		/** A [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]] adapter which considers itself
		  * the export version of the adapted column, with [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * methods and related reflecting this fact. It is the default class for proxies which must either
		  * expose the adapted mapping, or rely on the [[net.noresttherein.oldsql.schema.ColumnMapping.assemble assemble]]
		  * method rather than the [[net.noresttherein.oldsql.schema.ColumnMapping.selectForm selectForm]]
		  * of the adapted column for the assembly.
		  * @see [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.ComposedColumnAdapter]]
		  */ //there is a name inconsistency with ExportDecorator
		class ExportColumnAdapter[+M <: TypedColumn[S, O], S, O]
		                         (protected override val backer :M, override val name :String,
		                          override val buffs :Buffs[S])
		                         (implicit override val form :ColumnForm[S] = backer.form)
			extends ExportColumnProxy[S, O](backer, name, buffs)
			   with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O] with ColumnAdapter[M, S, O]
		{
			override def original :TypedColumn[S, O] = backer.original

			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, S, O] =
				new ExportComposedColumnDecorator[M, S, O](this, name, buffs)
		}


		trait ComposedColumnAdapter[+M <: ColumnAt[O], T, S, O]
			extends ComposedAdapter[M, T, S, O] with DelegateMapping[ColumnAdapter[M, T, O], S, O]
			   with ColumnAdapter[M, S, O]

		/** An adapter of another column adapter, exposing the original column through its
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]] property, but delegating all
		  * methods to the adapter class. It considers itself the ''export'' version of the adapted adapter,
		  * as well as any other subcolumn that it might define and report itself as the export version of.
		  */
		class ExportComposedColumnAdapter[+M <: ColumnAt[O], S, O]
		                                 (protected override val backer :ColumnAdapter[M, S, O],
		                                  override val name :String, override val buffs :Buffs[S])
			extends ExportColumnProxy[S, O](backer, name, buffs)
			   with ComposedColumnAdapter[M, S, S, O]
//			   with ComposedAdapter[M, S, S, O] with ColumnAdapter[M, S, O]

		/** A variant of
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.ComposedColumnAdapter ComposedColumnAdapter]]
		  * which is also a [[net.noresttherein.oldsql.schema.support.MappingDecorator MappingDecorator]] -
		  * it forwards the calls of [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] directly to
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]] column, rather than
		  * [[net.noresttherein.oldsql.schema.support.DelegateMapping.backer backer]].
		  * It considers itself the ''export'' version of both the adapter and its `body`.
		  */
		class ExportComposedColumnDecorator[+M <: TypedColumn[S, O], S, O]
		                                   (protected override val backer :ColumnAdapter[M, S, O],
		                                    override val name :String, override val buffs :Buffs[S])
			extends ExportColumnProxy[S, O](backer, name, buffs)
			   with ExportDecorator[M, S, O] with ComposedColumnAdapter[M, S, S, O]
//			   with ComposedAdapter[M, S, S, O] with ExportDecorator[M, S, O] with ColumnAdapter[M, S, O]




		/** A strange beast, being both a [[net.noresttherein.oldsql.schema.ColumnMapping ColumnMapping]]
		  * (normally a value-oriented class with little polymorphism) and
		  * a [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]], containing some other column
		  * as a component (which columns normally don't). While nominally it is an adapter of the wrapped column,
		  * in reality all methods use the [[net.noresttherein.oldsql.schema.ColumnMapping.form form]] of that column,
		  * while the latter remains unused. The adapted column `body` is included only in the `extracts`
		  * (and `columnExtracts`) maps, which are used only for aliasing to the ''export'' versions of the components,
		  * but not in  `components`/`subcomponents`.
		  *
		  * This class is useful as the result of intersection of `MappingAdapter` and `TypedColumn`, in particular
		  * in subclasses of `TypedColumn` which extend a business interface which narrows down the return type
		  * of some methods to return an adapter to the original mapping and which are independently narrowed
		  * by `TypedColumn` to return a column.
		  */
		class SimpleColumnAdapter[+M <: SimpleColumn[S, O], S, O]
		                         (override val body :M, override val name :String, override val buffs :Buffs[S])
			extends ExportDecorator[M, S, O] with ColumnAdapter[M, S, O]
			   with SimpleColumn[S, O] with StableColumn[S, O]
		{
			def this(column :M) = this(column, column.name, column.buffs)

			override val form :ColumnForm[S] = body.form
			private[this] val originalExtract = body(body) //ColumnExtract.ident(body)

			override val extracts :NaturalMap[Component, ColumnExtract] =
				//get the self extractor for ourselves and add one for the body column.
//				val superExtracts = super.extracts //NaturalMap.single[Component, ColumnExtract, S](this, apply(this))
//				superExtracts.updated[ColumnExtract, S](body :Component[S], originalExtract)
				body.extracts.updated[ColumnExtract, S](this, super.apply(this))

			override val columnExtracts :NaturalMap[Column, ColumnExtract] =
				extracts.asInstanceOf[NaturalMap[Column, ColumnExtract]]

			override def apply[T](component :Component[T]) :ColumnExtract[T] =
				if (component eq body) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(component)

			override def apply[T](column :Column[T]) :ColumnExtract[T] =
				if (column eq body) originalExtract.asInstanceOf[ColumnExtract[T]]
				else super.apply(column)

			override def unexport[X](component :Component[X]) :Component[X] = body.asInstanceOf[Component[X]]
			override def unexport[X](column :Column[X]) :Column[X] = body.asInstanceOf[Column[X]]

			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[M, S, O] =
				new SimpleColumnAdapter(body, name, buffs)

			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :ColumnAdapter[M, X, O] =
				super[ColumnAdapter].as(there, back)
		}



		/** This class takes the degeneration of
		  * [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter.SimpleColumnAdapter SimpleColumnAdapter]]
		  * one step further and only pretends to be an adapter - the `body` property points to this very instance.
		  * In all functionality this class behaves the same as a `StandardColumn`.
		  */
		class MockColumnProxy[S, O](override val name :String, override val buffs :Buffs[S])
		                           (implicit override val form :ColumnForm[S])
			extends SimpleColumn[S, O] with ColumnAdapter[TypedColumn[S, O], S, O] with StableColumn[S, O]
		{
			override val body :TypedColumn[S, O] = this

			override def copy(name :String, buffs :Buffs[S]) :ColumnAdapter[TypedColumn[S, O], S, O] =
				new MockColumnProxy[S, O](name, buffs)

			override def unexport[X](component :Component[X]) :Component[X] = component
			override def unexport[X](column :Column[X]) :Column[X] = column
		}
	}




	/** An adapter which changes nothing about the adapted mapping. It can be used where `Adapted[M]` (or similar)
	  * type is required, but the mapping should remain unmodified.
	  */
	class IdentityAdapter[M <: TypedMapping[S, O], S, O](protected override val backer :M)
		extends DirectProxy[S, O] with DelegateAdapter[M, S, O] with MappingDecorator[M, S, O]

}






//fixme: a mapping will not be epimorphic to its MappingProxy if the latter cont
/** A [[net.noresttherein.oldsql.schema.support.MappingAdapter MappingAdapter]]
  * with the same [[net.noresttherein.oldsql.schema.Mapping.Subject Subject]] type as the adapted mapping.
  * It is the public counterpart of implementation focused
  * [[net.noresttherein.oldsql.schema.support.MappingProxy MappingProxy]], with concrete classes typically extending
  * both.
  *
  * It is considered an implementation `trait`, and the application should refer to its instances
  * simply as `MappingAdapter[M, S, O]`. It is public in order to allow easy runtime checks which guarantee
  * the equality of the `Subject` types between the [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]]
  * and the adapter.
  *
  * The implementation assumes that it is [[net.noresttherein.oldsql.schema.Mapping.homomorphic homomorphic]]
  * with its `body`.
  */ //consider: extending AbstractDelegateAdapter
trait MappingDecorator[+M <: TypedMapping[S, O], S, O] extends BaseAdapter[M, S, O] {
	private trait Decorator extends ExportDecorator[M, S, O] with AbstractDelegateAdapter[M, S, O] {
		override val body = MappingDecorator.this.body
	}

	override def withBuffs(buffs :Buffs[S]) :MappingAdapter[M, S, O] =
		new BuffedMapping[S, O](this, buffs) with Decorator

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :MappingAdapter[M, S, O] =
		new AlteredMapping[S, O](this, include, exclude) with Decorator

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:MappingAdapter[M, S, O] =
		new PatchedMapping[S, O](this, op, include, exclude) with Decorator

	override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
		if (prefix.length == 0)
			this
		else
			new PrefixedMapping[S, O](prefix, this) with Decorator

	override def renamed(naming :String => String) :MappingAdapter[M, S, O] =
		new RenamedMapping[S, O](this, naming) with Decorator

	override def reorder(permutation :IndexedSeq[Int]) :MappingAdapter[M, S, O] = {
		ReorderedMapping.validatePermutation(this, permutation)
		if (permutation == permutation.indices)
			this
		else
			new ReorderedMappingComposedAdapter[M, S, O](this, permutation)
	}

	//	override def original :TypedMapping[S, O] = body.original

	override def submappingOf(that :Mapping) :Boolean =
		(this isomorphic that) || (body submappingOf that) || (that match {
			case other :MappingDecorator[_, _, _] => submappingOf(other.body)
			case _ => false
		})

	override def uniHomomorphic(that :Mapping) :Boolean = body homomorphic that

//	abstract override def unexport[X](column :Column[X]) :Column[X] = super.unexport(column)
//	abstract override def unexport[X](component :Component[X]) :Component[X] = super.unexport(component)
}




object MappingDecorator {

	/** Mixin trait for [[net.noresttherein.oldsql.schema.support.MappingAdapter.ComposedAdapter composed]] adapters
	  * which consider themselves the ''export'' versions of their `body` component.
	  * Analogously to [[net.noresttherein.oldsql.schema.support.MappingProxy.ExportProxy ExportProxy]], it delegates
	  * its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method
	  * directly to [[net.noresttherein.oldsql.schema.support.MappingAdapter.body body]].
	  * It is intended to shortcut the delegation process between several adapters/proxies and the component proper.
	  */ //consider: moving to MappingAdapter and renaming to ExportAdapter
	trait ExportDecorator[+M <: TypedMapping[S, O], S, O] extends MappingDecorator[M, S, O] {
		override def assemble(pieces :Pieces) :Opt[S] = body.assemble(pieces)
	}

}
