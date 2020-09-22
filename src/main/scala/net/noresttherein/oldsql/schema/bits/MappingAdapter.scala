package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, ColumnExtract, ColumnForm, ColumnMapping, Mapping, SQLForm, BaseMapping}
import net.noresttherein.oldsql.{schema, OperationType}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingSeal, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping
import net.noresttherein.oldsql.schema.ColumnMapping.StableColumn
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.MappingAdapter.{AdapterFactoryMethods, AdapterSeal}
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, FlagBuffType, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.OperationType.{INSERT, QUERY, SELECT, UPDATE}
import net.noresttherein.oldsql.collection.NaturalMap



sealed trait AdapterOf[+M <: Mapping] extends Mapping { this :MappingSeal =>
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
  *   - [[net.noresttherein.oldsql.schema.bits.MappingAdapter.BaseAdapter BaseAdapter]], which must be extended
  *     by all concrete classes extending `MappingAdapter`. This is because `BaseAdapter` enforces that the origin type
  *     of the adapted mapping is the same as the origin of the adapter, which is impossible to do here due to
  *     `MappingAdapter` being covariant in the adapted mapping type, but used in type aliases
  *     which define the adapter's origin type to be equal to the adapted mapping's origin type.
  *   - [[net.noresttherein.oldsql.schema.bits.MappingAdapter.DelegateAdapter DelegateAdapter]] for adapters
  *     implemented as a [[net.noresttherein.oldsql.schema.support.DelegateMapping DelegateMapping]] to their `body`;
  *   - [[net.noresttherein.oldsql.schema.bits.MappingAdapter.ComposedAdapter ComposedAdapter]] for adapters
  *     of adapters or, more correctly, adapters implemented as a `DelegateMapping` to an adapter of their `body`.
  *
  * This trait overrides factory methods creating adapter mappings such as `map` so they return a `MappingAdapter`
  * to the same mapping type `M` as this mapping (rather than to this adapter as `StaticMapping`).
  *
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.BaseAdapter]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.Adapted]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdaptedAt]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.MappedTo]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.Mapped]]
  * @author Marcin Mościcki
  */
trait MappingAdapter[+M <: Mapping, S, O]
	extends AdapterOf[M] with BaseMapping[S, O]
	   with AdapterFactoryMethods[({ type A[X] = MappingAdapter[M, X, O] })#A, S, O]
{ this :AdapterSeal =>

	/** The adapted mapping. It is considered a valid component of this mapping. */
	val body :M { type Origin = O }

	override def prefixed(prefix :String) :MappingAdapter[M, S, O] =
		throw new NotImplementedError("This method should have been overriden by MappingAdapter.BaseAdapter and inaccessible.")

	override def renamed(name :String) :MappingAdapter[M, S, O] =
		throw new NotImplementedError("This method should have been overriden by MappingAdapter.BaseAdapter and inaccessible.")

	override def as[X](there: S =?> X, back: X =?> S)
	                  (implicit nulls :NullValue[X]) :MappingAdapter[M, X, O] =
		throw new NotImplementedError("This method should have been overriden by MappingAdapter.BaseAdapter and inaccessible.")

}






object MappingAdapter {

	type Adapted[M <: Mapping] = MappingAdapter[M, M#Subject, M#Origin]

	type AdaptedAt[M[A] <: MappingAt[A], O] = MappingAdapter[M[O], M[O]#Subject, O]

	type MappedTo[M <: Mapping, S] = MappingAdapter[M, S, M#Origin]

	type Mapped[+M[A] <: MappingAt[A], S, O] = MappingAdapter[M[O], S, O]



	implicit def adapterProjection[M <: MappingAt[O], S, O](implicit body :ExactProjection[M])
			:ProjectionDef[MappingAdapter[M, S, O], ({ type P[X] = MappingAdapter[body.WithOrigin[X], S, X] })#P, S] =
		body.adapt[MappingAdapter, S, O]




	/** A sealed trait used to enforce that each `MappingAdapter` extends `BaseAdapter`, directly or indirectly.
	  * This is because several methods overriden by the former have only stub definitions, as proper
	  * implementations are impossible without the constraint `M <: MappingAt[O]`.
	  */
	sealed trait AdapterSeal



	/** Implementations of `Mapping`'s methods which create adapter to the original mapping,
	  * such as `forSelect`, `prefixed`, `map`. All implemented methods return a specific mapping type `A[X]`.
	  */
	trait AdapterFactoryMethods[+A[X] <: RefinedMapping[X, O], S, O] extends Mapping { this :MappingSeal =>
		override type Subject = S
		override type Origin = O


		/** @inheritdoc
		  * @return `customize(SELECT, include, exclude)`.
		  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdapterFactoryMethods.customize customize]] */
		override def forSelect(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
			customize(SELECT, include, exclude)

		/** @inheritdoc
		  * @return `customize(QUERY, include, exclude)`.
		  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdapterFactoryMethods.customize customize]] */
		override def forQuery(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
			customize(QUERY, include, exclude)

		/** @inheritdoc
		  * @return `customize(UPDATE, include, exclude)`.
		  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdapterFactoryMethods.customize customize]] */
		override def forUpdate(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
			customize(UPDATE, include, exclude)

		/** @inheritdoc
		  * @return `customize(INSERT, include, exclude)`.
		  * @see [[net.noresttherein.oldsql.schema.bits.MappingAdapter.AdapterFactoryMethods.customize customize]] */
		override def forInsert(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
			customize(INSERT, include, exclude)

		/** Target method for `forSelect`, `forQuery`, `forUpdate` and `forInsert`. Responsible for creating an
		  * adapter (typically a [[net.noresttherein.oldsql.schema.bits.CustomizedMapping CustomizedMapping]] subclass)
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
		  */
		protected def customize(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S]



		override def qualified(prefix :String) :A[S] =
			if (prefix.length == 0) prefixed("") else prefixed(prefix + ".")

		override def prefixed(prefix :String) :A[S]


		
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
	trait ColumnAdapterFactoryMethods[+A[X] <: ColumnMapping[X, O], S, O] extends AdapterFactoryMethods[A, S, O] {
		this :MappingSeal =>

		protected def name :String

		protected def form :ColumnForm[S]

		/** A column identical to this one as an instance of `A[S]`. Implemented as `copy(name, buffs)`, it
		  * should be overriden to `this` in classes conforming to `A[S]`. */
		protected def thisColumn :A[S] = copy(name, buffs)

		/** A column with the specified name and buffs, inheriting the form and any other properties from this
		  * instance. It is the target of the `prefixed`, `customize` and related methods.
		  */
		protected def copy(name :String, buffs :Seq[Buff[S]]) :A[S]


		/** A new column, with the same name, form (and optionally other properties), but with the buffs replaced by
		  * the provided list.
		  */
		def withBuffs(buffs :Seq[Buff[S]]) :A[S] = copy(name, buffs)


		protected override def customize(op :OperationType,
		                                 include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :A[S] =
			if (include.size > 1)
				throw new IllegalArgumentException("Mappings " + include + " are not components of column " + this)
			else if (exclude.size > 1)
			     throw new IllegalArgumentException("Mappings " + exclude + " are not components of column " + this)
			else if (exclude.headOption.contains(this) && op.optional.enabled(this))
				 withBuffs(op.nonDefault[S] +: buffs)
			else if (include.headOption.contains(this) && op.explicit.enabled(this))
			     withBuffs(buffs.filter(op.explicit.disabled))
			else
				thisColumn


		override def renamed(name :String) :A[S] = copy(name, buffs)

		override def prefixed(prefix :String) :A[S] =
			if (prefix.length == 0) thisColumn else renamed(prefix + name)

		def prefixed(prefix :Option[String]) :A[S] =
			if (prefix.isEmpty) thisColumn else prefixed(prefix.get)

	}






	/** The actual base trait for all concrete `MappingAdapter` classes. It is separated from the `MappingAdapter`
	  * to enforce that the adapted mapping is of the same origin as this adapter, which is impossible in the latter
	  * due to its use in type aliases which define the `MappingAdapter`'s `Origin` type based on the origin type
	  * of the mapping `M`.
	  */
	trait BaseAdapter[+M <: MappingAt[O], S, O] extends MappingAdapter[M, S, O] with AdapterSeal {
		override val body :M

		protected override def customize(op :OperationType,
		                                 include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:MappingAdapter[M, S, O] =
			new CustomizedMapping[this.type, S, O](this, op, include, exclude) with ComposedAdapter[M, S, S, O]


		override def prefixed(prefix :String) :MappingAdapter[M, S, O] = PrefixedMapping(prefix, this)

		override def renamed(name :String) :MappingAdapter[M, S, O] = RenamedMapping(name, this)


		override def as[X](there: S =?> X, back: X =?> S)
		                  (implicit nulls :SQLForm.NullValue[X]) :MappingAdapter[M, X, O] =
			MappedMapping.adapter(this, there, back)

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
	  * to the adapter rather than the adapted mapping. As it accesses the `backer` property, it should be mixed-in
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
		   with ColumnAdapterFactoryMethods[({ type C[X] = ColumnAdapter[M, T, X, O] })#C, S, O]
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
		  * to this instance and return a new `ColumnFormProxy`, without modifying the `body`.
		  * At the same time, just in case, the `optionally` method is overriden to check for values for the adapted
		  * columns and passing it to aliasing `ComponentValues` will not result in an exception, as the extract
		  * for it is provided.
		  */
		class ColumnFormProxy[M <: ColumnMapping[S, O], S, O](column :M, name :String, buffs :Seq[Buff[S]])
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
				new ColumnFormProxy(body, name, buffs)
		}



		/** This class takes the degeneration of the `ColumnFormProxy` adapter one step further and only pretends
		  * to be an adapter - the `body` property points to this very instance. In all functionality this class
		  * behaves the same as a `StandardColumn`.
		  */
		class PseudoColumnProxy[S, O](override val name :String, override val buffs :Seq[Buff[S]])
		                             (implicit override val form :ColumnForm[S])
			extends ColumnAdapter[ColumnMapping[S, O], S, S, O] with StableColumn[S, O]
		{
			override val body :ColumnMapping[S, O] = this

//			override def renamed(name :String) :ColumnAdapter[ColumnMapping[S, O], S, S, O] =
//				new PseudoColumnProxy(name, buffs)(form)

			override def copy(name :String, buffs :Seq[Buff[S]]) :ColumnAdapter[ColumnMapping[S, O], S, S, O] =
				new PseudoColumnProxy[S, O](name, buffs)
		}

	}

}