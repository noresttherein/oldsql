package net.noresttherein.oldsql.schema.bases

import java.sql.{CallableStatement, JDBCType, ResultSet}

import net.noresttherein.oldsql.schema.ColumnMapping.TypedColumn
import net.noresttherein.oldsql.schema.Mapping.MappingReadForm

//import scala.annotation.nowarn
import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.OperationView.{FilterView, InsertView, UpdateView, WriteOperationView}
import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory, PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.model.Kin.Derived
import net.noresttherein.oldsql.model.KinFactory.DerivedKinFactory
import net.noresttherein.oldsql.model.RelatedEntityFactory.KeyExtractor
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema.{Buff, Buffs, ColumnForm, MappingExtract, RelVar, Sealed, SQLReadForm}
import net.noresttherein.oldsql.schema.Buff.{SelectPreset, NoSelect, OptionalSelect}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StableColumn, TypedColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormNullValue
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping, JoinedEntityComponent, JoinTableCollectionMapping, RelationshipMapping}
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.{InverseForeignKeyMapping, RelatedEntityForeignKey, RelatedEntityForeignKeyColumn}
import net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.{JoinTableKinMapping, JoinTableManyMapping}
import net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping
import net.noresttherein.oldsql.schema.support.MappingProxy.{OpaqueColumnProxy, OpaqueProxy}






/** A base trait for mappings with a fixed structure, with columns known statically, and which are created
  * manually, rather than by some generic code. While it exposes mutator methods to facilitate creation and declaration
  * of columns, it is expected that they'll be used solely in constructors of derived classes by those classes
  * themselves and, once created, it will be seen as immutable from the outside. If an attempt is made to modify
  * this instance once any of its accessor methods for components had been called previously, an exception
  * will be thrown.
  *
  * This mapping is flat: all mapped columns are direct components of this instance. It can have components,
  * but they are formally empty: any column and component fields of such components are in fact, from the point of view
  * of the framework, components of this mapping, with the components only grouping them into subsets. They are still
  * useful, as they can define [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct]] method
  * in terms of the columns and components of this mapping, assembling some property values of subject `S`.
  * As another restriction, all components must extend
  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.FlatComponent FlatComponent]]; it is impossible
  * to embed as components instances of arbitrary [[net.noresttherein.oldsql.schema.Mapping mapping]] classes.
  *
  * On the plus side, it is an [[net.noresttherein.oldsql.schema.bases.ExportMapping ExportMapping]]: all subcomponents
  * are their own ''export'' versions, making [[net.noresttherein.oldsql.schema.Mapping.export export]]
  * an identity operation, requiring no lookup. This makes
  * [[net.noresttherein.oldsql.haul.ComponentValues.aliased aliasing]] of
  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] used for the assembly more efficient,
  * as retrieving a value of any column becomes (typically) a simple array lookup.
  *
  * It exists exclusively as a way to facilitate creating schema descriptions and its interface is directed
  * towards implementing classes; it is not treated in any special way by other parts of the library,
  * different from other implementations of `Mapping`.
  *
  * @tparam S the `Subject` type of the mapped entity.
  * @tparam O A marker [[net.noresttherein.oldsql.schema.Mapping.Origin]] type, used to distinguish between
  *           several instances of the same mapping class, but coming from different sources (especially different
  *           aliases for a table occurring more then once in a join). At the same time, it adds additional type safety
  *           by ensuring that only components of mappings included in a query can be used in creation
  *           of SQL expressions used by that query.
  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame]]
  */ //todo: rename to FlatMeta
trait SimpleMapping[S, O]
	extends StaticMapping[S, O] with LazyMapping[S, O] with RelatedMapping[S, O] with ExportMapping
{ root =>

	/** Base trait for all components of `SimpleMapping`, including columns, synthetic relationship components
	  * and user components. Components for business model objects should be derived from
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.FlatComponent FlatComponent]] rather than this trait.
	  * Instances of this type are always instances of the enclosing mapping's
	  * [[net.noresttherein.oldsql.schema.Mapping.ExportComponent ExportComponent]]`[T]`.
	  */
	sealed trait AbstractComponent[T] extends BaseMapping[T, O] {
		override type SuperMapping = root.type

		protected[SimpleMapping] def componentSelector :Sealed[S =?> T]

		private[SimpleMapping] final val extract :root.Extract[T] = MappingExtract(this)(componentSelector)

		@inline private[SimpleMapping] final def belongsTo(mapping :SimpleMapping[_, _]) =
			mapping == root

		/** Returns the value of this component in an option if an implicit `ComponentValues` instance
		  * for the enclosing composite mapping is present.
		  */
		@inline final def ?(implicit values :root.Pieces) :Opt[T] = values.get(extract)

		/** Buffs 'inherited' from the enclosing mapping. It uses the value extract `S => T` to map all buffs applied
		  * to the enclosing mapping to adapt them to this value type. This means that if there are any
		  * [[net.noresttherein.oldsql.schema.Buff.ValueBuff ValueBuff]]s among the inherited buffs, this component
		  * must have a value for the value associated with that buff or an exception will be thrown - either
		  * when accessing/initializing the buff list or when the value for the buff is actually needed.
		  */
		protected final def inheritedBuffs :Buffs[T] = root.buffs.unsafeCascade(componentSelector)

		/** Enlists this component in the `components` ''in spe'' property of the enclosing `SimpmleMapping`.
		  * Must be called from within the constructor of a component, and all its subcomponents must likewise
		  * be either eagerly created and appended to `componentsBuffer`, or their constructors
		  * appended to `lateComponents`
		  */
		protected[SimpleMapping] final def registerSelf() :Unit = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a component of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
		}
	}


	/** Base class for all user defined components of the outer `SimpleMapping`. Provides column factory methods
	  * mirroring those from the outer mapping; each new column becomes both a column of `SimpleMapping`
	  * and of this component. This mapping cannot have true, independent subcomponents; it can however
	  * use components of the root mapping by registering them in this instance. These components should be created
	  * normally in the most outer scope of the `SimpleMapping` component tree, and after or during construction
	  * passed to method [[net.noresttherein.oldsql.schema.bases.SimpleMapping.FlatComponent.borrow borrow]]
	  * of this instance. Such semi-components are not exported to the outer mapping and are ignored
	  * by methods creating forms and [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] for write
	  * operations in order to avoid processing them twice. They will be however correctly recognized by
	  * `ComponentValues` during assembly. All buffs on the borrowed components are ignored when they are processed
	  * by this mapping.
	  *
	  * This class should not be used as a base class for column implementations, as such classes
	  * would not be correctly recognized as columns and result in exceptions being thrown when used.
	  * All columns of `SimpleMapping` should be created using its factory methods (or factory methods of this class).
	  */ //todo: relativeSuffix
	abstract class FlatComponent[T](property :S =?> T, relativePrefix :String, override val buffs :Buffs[T])
	                               (implicit nulls :Maybe[NullValue[T]])
		extends AbstractComponent[T] with StaticMapping[T, O] with EffectivelyEmptyMapping[T, O] with ExportMapping
		   with RelatedMapping[T, O] with LazyMapping[T, O]
	{ self =>
		//fixme: if this is empty, we can't select or update it! The motivation was for all transitive columns
		// to have consecutive indices assigned by SimpleMapping, but the columns can still
		// extend FlatColumn/AbstractColumn, together with FlatComponent.Subcolumn

		def this(property :S =?> T, buffs :Buffs[T])(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, "", buffs)

		def this(property :S =?> T, prefix :String, buffs :Buff[T]*)(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, prefix, root.buffs.unsafeCascade(property).declare(buffs :_*))

		def this(property :S =?> T, buffs :Buff[T]*)(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, "", buffs :_*)

		/** Returns `SimpleMapping.this.`[[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]]. */
		protected def inheritedPrefix :String = root.verifiedPrefix
		protected override val columnPrefix :String = inheritedPrefix + relativePrefix
		override val nullValue :NullValue[T] = nulls.opt getOrElse NullValue.NotNull

		protected[SimpleMapping] override def componentSelector :Sealed[S =?> T] = property

		registerSelf()

		private[this] var initExtractMap :NaturalMap[Component, Extract] = NaturalMap.empty
		private[this] lazy val lazyExtracts = synchronized { val res = initExtractMap; initExtractMap = null; res }

		private[this] lazy val lazyColumns :Unique[Column[_]] =
			columnExtracts.collect { case Assoc(col, _) => col }.to(Unique)

		private[this] lazy val lazyComponents :Unique[Component[_]] =
			lazyExtracts.collect { case Assoc(comp, _) => comp }.to(Unique)

		final override def extracts   :NaturalMap[Component, Extract] = lazyExtracts
		final override def components :Unique[Component[_]] = lazyComponents
		final override def columns    :Unique[Column[_]]    = lazyColumns

		/** Adds the given component of the outer mapping to the waiting list of this mapping's components.
		  * Once any of the column, component or extract collections of this mapping are accessed, they will contain
		  * all registered components (including columns created by this class's factory methods) in the order
		  * in which this method was invoked. All components remain their export versions as in the outer
		  * `SimpleMapping`. This method will not modify any properties of the outer mapping:
		  * while the registered component will be present twice in the component tree of the root mapping,
		  * it will feature only once in its column, component and extracts list.
		  * During assembly, retrieving a value for this subcomponent may require a `Map` lookup
		  * in [[net.noresttherein.oldsql.schema.Mapping.extracts extracts]], depending on
		  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] implementation.
		  */
		protected def borrow[X](component :AbstractComponent[X], property :T =?> X) :component.type = {
			val extract = MappingExtract(component)(property)
			synchronized {
				if (initExtractMap == null)
					throw new IllegalStateException(
						s"Cannot include $component as a component of $root.$this: the owning component has already been initialized."
					)
				initExtractMap = initExtractMap.updated[Extract, X](component, extract :Extract[X])
			}
			component
		}

		/** Create a new column for the given property of the subject of outer
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping SimpleMapping]] as its direct column.
		  * While not present on the columns or components lists of this component, it will be handled correctly within
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]] method
		  * (and others). Similarly, the final name of the column will be prefixed with the combined column prefix based on
		  * this mapping's [[net.noresttherein.oldsql.schema.bases.SimpleMapping.FlatComponent.columnPrefix columnPrefix]]
		  * (which - by default - includes the outer mapping's
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] together with any
		  * extension provided as a constructor parameter to this component) and its buffs will include buffs inherited
		  * both from this component and `SimpleMapping`.
		  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
		  * @param property the getter function returning the value for the column from an instance of the mapped subject.
		  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
		  *              of this mapping.
		  * @tparam X the type of the value of this column as present on the mapped entity.
		  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
		  */
		protected def column[X :ColumnForm](name :String, property :T => X, buffs :Buff[X]*) :Column[X] = {
			val fullExtractor = extract andThen property
			val inherited = this.buffs.cascade(property)
			borrow(new FlatColumn[X](columnPrefix + name, fullExtractor, inherited, buffs), property)
		}

		/** Create a new column for the given property of the subject of outer
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping SimpleMapping]] as its direct column.
		  * While not present on the columns or components lists of this component, it will be handled correctly within
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct]] method (and others).
		  * The column's name will be the concatenation of this mapping's
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and the name
		  * of the property as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName`
		  * as the `value` argument will set the column name to "firstName". While not present on this component's
		  * column and component lists, it will be nevertheless accepted within
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]] method
		  * and will inherit buffs both of this component and the outer mapping.
		  * @param property the getter function for the value of this column. Should represent a simple property of the
		  *                 mapped subject of this mapping.
		  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
		  *              of this mapping.
		  * @tparam X the type of the value of this column as present on the mapped entity.
		  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
		  */
		protected def column[X](property :T => X, buffs :Buff[X]*)
		                       (implicit form :ColumnForm[X], subject :TypeTag[T]) :Column[X] =
			column(PropertyPath.nameOf(property), property, buffs :_*)

		/** Create a new column for an optional property of the subject of the outer
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping SimpleMapping]] as its direct column.
		  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
		  * instead. The column will inherit any buffs declared on this mapping and its name will be the concatenation
		  * of this mapping's [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and `name`.
		  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
		  * @param property the getter function returning the value for the column from an instance of the mapped subject.
		  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
		  *              of this mapping.
		  * @tparam X the type of the value of this column as present on the mapped entity.
		  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
		  */
		protected def optcolumn[X :ColumnForm](name :String, property :T => Option[X], buffs :Buff[X]*) :Column[X] = {
			val fullExtract = extract andThenOpt property
			val inherited = this.buffs.unsafeCascade(property)
			borrow(new FlatColumn[X](columnPrefix + name, fullExtract, inherited, buffs), property)
		}

		/** Create a new column for an optional property of the subject of the outer
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping SimpleMapping]] as its direct column.
		  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
		  * instead. The column's name will be the concatenation of this mapping's
		  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and the name
		  * of the property as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName`
		  * as the `value` argument will set the column name to "firstName". Any buffs declared on this mapping
		  * will be inherited by the created column. While not present on this component's column and component lists,
		  * it will be nevertheless accepted within
		  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate.construct construct]] method
		  * and will inherit buffs both of this component and the outer mapping.
		  * @param property the getter function returning the value for the column from an instance of the mapped subject.
		  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
		  *              of this mapping.
		  * @tparam X the type of the value of this column as present on the mapped entity.
		  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
		  */
		protected def optcolumn[X](property :T => Option[X], buffs :Buff[X]*)
		                          (implicit form :ColumnForm[X], subject :TypeTag[T]) :Column[X] =
			optcolumn[X](PropertyPath.nameOf(property), property, buffs :_*)


		protected override def fkImpl[M[A] <: TypedMapping[E, A], K, E, X, R]
		                             (name :String, property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => TypedColumn[K, _],
		                              factory :RelatedEntityFactory[K, E, X, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		{
			val selector = Extractor.req(property)
			val allBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new FKColumn[M, K, E, X, R, Unit](extract andThenReq selector, columnPrefix + name, allBuffs)(
					factory, table, key.asInstanceOf[M[Unit] => TypedColumn[K, Unit]]
				), selector
			)
		}

		protected override def fkImpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
		                             (property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, X, R])
		                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val fullExtract = extract andThenReq selector
			val allBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new FKComponent[M, C, K, E, X, R, Unit](fullExtract, (s :String) => columnPrefix + rename(s), allBuffs)(
					factory, table, key.asInstanceOf[M[Unit] => C[Unit]]
				), selector
			)
		}

		protected override def inverseFKImpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, X, R]
		                       (property :T => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
		                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
				:JoinedEntityComponent[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val allBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new InverseFKComponent[M, C, K, E, X, R, Unit](extract andThenReq selector, allBuffs)(key, reference)(
					table, fk.asInstanceOf[M[Unit] => ForeignKeyMapping[MappingAt, C, K, _, Unit]]
				), selector
			)
		}

		protected override def kinImpl[J[A] <: TypedMapping[JE, A], M[A] <: TypedMapping[E, A],
			                           C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
		                              (property :T => Kin[X], joinTable :RelVar[J],
		                               source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
		                               target :J[JO] => ForeignKeyMapping[M, TC, TK, TR, JO],
		                               linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
		                               buffs :Buff[Kin[X]]*)
		                              (implicit composite :X ComposedOf E, link :TypeTag[JE])
				:JoinTableCollectionMapping[J, M, C, TC, K, TK, Kin[X], O] =
		{
			val selector = Extractor.req(property)
			val allBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new ToManyKinComponent[J, M, C, TC, K, TK, JE, E, X, TR, JO](
					extract andThenReq selector, joinTable, source, target, linkKin, targetKin, allBuffs
				), selector
			)
		}

		protected override def manyImpl[J[A] <: TypedMapping[JE, A], M[A] <: TypedMapping[E, A],
			                            C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
		                               (property :T => Derived[E, X], joinTable :RelVar[J],
		                                source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
		                                target :J[JO] => ForeignKeyMapping[M, TC, TK, TR, JO],
		                                linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
		                                buffs :Buff[Derived[E, X]]*)
		                               (implicit composite :X ComposedOf E, link :TypeTag[JE])
				:JoinTableCollectionMapping[J, M, C, TC, K, TK, Derived[E, X], O] =
		{
			val selector = Extractor.req(property)
			val allBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new ToManyDerivedComponent[J, M, C, TC, K, TK, JE, E, X, TR, JO](
					extract andThen selector, joinTable, source, target, linkKin, targetKin, allBuffs
				), selector
			)
		}
	}




	/** Base class for components which have a value for all instances of `T`.
	  * @param property       extractor function for the value of this component.
	  * @param relativePrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                       It is relative to this component, not the outer `MappingFrame`: the `columnPrefix` declared
	  *                       by the latter is combined with this prefix when exporting columns to the outer mapping.
	  *                       All columns accessed through this instance methods will be affected, not only when viewed
	  *                       from parent mappings. A column which is defined within a subcomponent of this component
	  *                       will not show the prefix however when viewed in the scope of that component - only
	  *                       the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *                       of this component will be the concatenation of the column prefix defined by the enclosing
	  *                       mapping and this prefix.
	  * @param buffs          buffs specific to this component. This list will be extended with buffs inherited
	  *                       from the enclosing schema. Note that these `buffs` are ''not'' automatically conveyed
	  *                       to subcomponents of this component.
	  * @tparam T value type of this component.
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseOptionalComponent]]
	  */
	abstract class RequisiteComponent[T](property :S => T, relativePrefix :String, buffs :Buffs[T])
	                                    (implicit nulls :Maybe[NullValue[T]])
		extends FlatComponent[T](property, relativePrefix, buffs)
	{
		def this(property :S => T, buffs :Buffs[T])(implicit nulls :Maybe[NullValue[T]]) =
			this(property, "", buffs)

		def this(property :S => T, relativePrefix :String, buffs :Buff[T]*)(implicit nulls :Maybe[NullValue[T]]) =
			this(property, relativePrefix, root.buffs.cascade(property).declare(buffs :_*))

		def this(property :S => T, buffs :Buff[T]*)(implicit nulls :Maybe[NullValue[T]]) =
			this(property, "", root.buffs.cascade(property).declare(buffs :_*))
	}



	/** Base class for components which might not have a value for all instances of `T` (such as properties declared
	  * by some subclass of `T`. The component is considered not to be a part of the mapping for some owning entities
	  * of type `S` and, in particular, the underlying columns are excluded from their updates and inserts.
	  *
	  * There are four broad cases when a certain value `T` might be unavailable for
	  * the enclosing mapping's subject instance `S`:
	  *   1. It is a property of type `Option[S]` or similar and signifies the lack of some attribute of the
	  *      represented object from the domain model, in which case the option type itself can be considered
	  *      the mapped value.
	  *   1. The value is not present at a given lifecycle phase of the mapped entity, for example a database
	  *      generated primary key.
	  *   1. It is part of a different table.
	  *   1. The column(s) with the value for the component is not included in every database query (for example,
	  *      for efficiency reasons or because it is a part of a joined table) and therefore neither it is present
	  *      when the entity is being updated. It is a case of value being missing on a given instance, rather than
	  *      for the represented abstract (or real life) entity.
	  *   1. It is a part of a denormalized relation and its existence depends on value(s) of other component(s).
	  *      This is for example the case with mapping of a class with its subclasses to a single database table.
	  *
	  *  Of the above, the first case should use a
	  *  [[net.noresttherein.oldsql.schema.bases.MappingFrame.RequisiteComponent RequisiteComponent]] instance,
	  *  while the last two are clear candidates for this type of the component. The second and third cases
	  *  are less clear cut and can conceivably  use either of the approaches.
	  *
	  * Optional components can cause conflicts with [[net.noresttherein.oldsql.schema.Buff.ValueBuff value buffs]]
	  * declared on the outer frame or some of its ancestors. If `pick` function defined by the component returns
	  * no value for a value stored in such a buff,
	  * a [[net.noresttherein.oldsql.exceptions.BuffMappingFailureException BuffMappingFailureException]]
	  * will be thrown. This can happen both when the component's buffs are created - either in this component's
	  * constructor or when its buffs are accessed during any database operation - and when the value of the said buff
	  * is being requested - it depends on whether the value buff carries a constant or a generator expression.
	  * An optional component is safe from this problem if one of the following conditions holds:
	  *   1. No value buffs are among the outer frame's buffs;
	  *   1. `pick` always returns a value for all value buffs declared or inherited by the outer frame;
	  *   1. All unsafe buffs are not [[net.noresttherein.oldsql.schema.Buff.cascades cascading]];
	  *   1. This component does not inherit buffs from its outer frame.
	  *
	  *  Note that while optional components will often be used in combination with the
	  *  `OptionalSelect`/`OptionalInsert`/`OptionalUpdate` buffs, none of them is automatically implied.
	  *
	  * @param property       returns value of this component on a given parent entity.
	  * @param relativePrefix prefix which will be added to all columns, direct and transitive within this instance.
	  *                       It is relative to this component, not the outer `MappingFrame`: the `columnPrefix` declared
	  *                       by the latter is combined with this prefix when exporting columns to the outer mapping.
	  *                       All columns accessed through this instance methods will be affected, not only when viewed
	  *                       from parent mappings. A column which is defined within a subcomponent of this component
	  *                       will not show the prefix however when viewed in the scope of that component - only
	  *                       the export proxy present on `this.columns` will. Note that the `columnPrefix` property
	  *                       of this component will be the concatenation of the column prefix defined by the enclosing
	  *                       mapping and this prefix.
	  * @param buffs          buffs specific to this component. This list will be extended with buffs inherited
	  *                       from the enclosing schema. The export versions of all subcomponents of this component will
	  *                       inherit these buffs.
	  * @tparam T value type of this component.
	  *
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.BaseOptionalComponent]]
	  * @see [[net.noresttherein.oldsql.schema.bases.MappingFrame.OptionalFrame]]
	  * @see [[net.noresttherein.oldsql.schema.bits.OptionMapping]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalSelect Buff.OptionalSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitSelect Buff.ExplicitSelect]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalUpdate Buff.OptionalUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitUpdate Buff.ExplicitUpdate]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.OptionalInsert Buff.OptionalInsert]]
	  * @see [[net.noresttherein.oldsql.schema.Buff.ExplicitInsert Buff.ExplicitInsert]]
	  */ //todo: should this come already with an OptionalInsert/Update/Select buff?
	abstract class OptionalComponent[T](property :S => Option[T], relativePrefix :String, buffs :Buffs[T])
	                                   (implicit nulls :Maybe[NullValue[T]])
		extends FlatComponent[T](property, relativePrefix, buffs)
	{
		def this(property :S => Option[T], buffs :Buffs[T])(implicit nulls :Maybe[NullValue[T]]) =
			this(property, "", buffs)

		def this(property :S => Option[T], relativePrefix :String, buffs :Buff[T]*)(implicit nulls :Maybe[NullValue[T]]) =
			this(property, relativePrefix, root.buffs.unsafeCascade(property).declare(buffs :_*))

		def this(property :S => Option[T], buffs :Buff[T]*)(implicit nulls :Maybe[NullValue[T]]) =
			this(property, "", root.buffs.unsafeCascade(property).declare(buffs :_*))
	}



	private trait AbstractColumn[T] extends AbstractComponent[T] with BaseColumn[T, O] {
		protected[SimpleMapping] final val index = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $buffString as a column of $root: the root mapping has already been initialized."
				)
			componentsBuffer += this
			columnsBuffer += this
			columnsBuffer.length - 1
		}
	}


	private class FlatColumn[T](override val name :String, selector :S =?> T, override val buffs :Buffs[T])
	                           (implicit override val form :ColumnForm[T])
		extends AbstractColumn[T] with SimpleColumn[T, O] with StableColumn[T, O]
	{
		def this(name :String, selector :S =?> T, inherited :Buffs[T], declared :Seq[Buff[T]])
		        (implicit form :ColumnForm[T]) =
			this(name, selector, inherited.declare(declared :_*))

		def this(name :String, selector :S =?> T, buffs :Seq[Buff[T]])(implicit form :ColumnForm[T]) =
			this(name, selector, root.buffs.unsafeCascade(selector).declare(buffs :_*))

		protected[SimpleMapping] override def componentSelector = selector
	}



	/** An [[net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueProxy opaque]] proxy to component `backer`
	  * of another mapping, serving as a foreign key. This implementation mirrors
	  * [[net.noresttherein.oldsql.schema.support.CoveredMapping CoveredMapping]], but all subcomponents
	  * and subcolumns created as adapters to components and columns of `backer` become
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.AbstractComponent components]] and
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.AbstractColumn columns]] of the enclosing `SimpleMapping`.
	  */
	private class MirrorComponent[M <: TypedMapping[T, X], T, X]
	                             (override val componentSelector :Sealed[S =?> T],
	                              override val buffs :Buffs[T] = Buffs.empty[T])
	                             (override val backer :M, rename :String => String = identity[String])
		extends OpaqueProxy[T, O](backer) with AbstractComponent[T]
	{ mirror =>
		def this(selector :S =?> T, buffs :Seq[Buff[T]])(backer :M, rename :String => String) =
			this(selector, root.buffs.unsafeCascade(selector).declare(buffs :_*))(backer, rename)

		def this(selector :S =?> T, inherited :Buffs[T], declared :Seq[Buff[T]])(backer :M, rename :String => String) =
			this(selector, inherited.declare(declared :_*))(backer, rename)

		protected override def adapt[U](component :backer.Component[U]) :Component[U] = {
			val extract = backer(component)
			val totalBuffs = buffs.unsafeCascade(extract).declare()
			val selector = componentSelector andThen extract
			new MirrorComponent[TypedMapping[U, X], U, X](selector, totalBuffs)(component, rename)
		}

		protected override def adapt[U](column :backer.Column[U]) :Column[U] = {
			val columnExtract = backer(column)
			val columnBuffs = buffs.unsafeCascade(columnExtract).declare()
			val selector = componentSelector andThen columnExtract
			column match {
				case simple :SimpleColumn[U @unchecked, X @unchecked] =>
					new FlatColumn[U](rename(column.name), selector, columnBuffs)(simple.form)
				case _ =>
					new OpaqueColumnProxy[U, X, O](column, rename(column.name), columnBuffs)
						with AbstractColumn[U]
					{
						protected[SimpleMapping] override val componentSelector = selector
					}
			}
		}

		registerSelf()

		def mirror[U](component :backer.Component[U]) :Component[U] = alias(component)
		def mirror[U](column :backer.Column[U]) :Column[U] = alias(column)
	}



	private class FKComponent[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, X]
	              (override val componentSelector :Sealed[S =?> R], rename :String => String, override val buffs :Buffs[R])
	              (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X])
		extends RelatedEntityForeignKey[M, C, K, E, T, R, X, O](rename, factory, buffs)(table, pk)
		   with AbstractComponent[R]
	{
		def this(selector :S =?> R, prefix :String, buffs :Buffs[R])
		        (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X]) =
			this(selector, verifiedPrefix + prefix + _, buffs)(factory, table, pk)

		def this(selector :S =?> R, rename :String => String, buffs :Seq[Buff[R]])
		        (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X]) =
			this(selector, rename, root.buffs.unsafeCascade(selector).declare(buffs :_*))(
				factory, table, pk
			)

		private[this] lazy val lazyKey = {
			val extract = KeyExtractor(factory)
			val selector = componentSelector andThen extract
			new MirrorComponent[C[X], K, X](selector, buffs.cascade(factory.forceKeyOutOf), Nil)(target, rename)
		}
		override def key :Component[K] = lazyKey

		override def local[U](subKey :TypedMapping[U, X]) :Component[U] = lazyKey.mirror(subKey)
		override def local[U](subKey :TypedColumn[U, X]) :Column[U] = lazyKey.mirror(subKey)

		root.synchronized { //this component will be created lazily; it must be before root lists are accessed
			registerSelf()
			lateComponents += (() => lazyKey)
		}
	}


	private class FKColumn[M[A] <: TypedMapping[E, A], K, E, T, R, X]
	                      (override val componentSelector :Sealed[S =?> R], name :String, override val buffs :Buffs[R])
	                      (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => TypedColumn[K, X])
		extends RelatedEntityForeignKeyColumn[M, K, E, T, R, X, O](name, factory, buffs)(table, pk)
		   with AbstractColumn[R]
	{ fk =>
		def this(selector :S =?> R, name :String, buffs :Seq[Buff[R]])
				(factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => TypedColumn[K, X]) =
			this(selector, name, root.buffs.unsafeCascade(selector).declare(buffs :_*))(factory, table, pk)

		//override the underlying key column with one extending AbstractColumn
		private[this] lazy val lazyKey = {
			val extract = KeyExtractor(factory)
			val selector = componentSelector andThen extract
			if (target.isInstanceOf[SimpleColumn[_, _]])
				new FlatColumn[K](name, selector, buffs.cascade(factory.forceKeyOutOf), Nil)(target.form)
			else
				new OpaqueColumnProxy[K, X, O](target, name, Buffs.empty[K]) with AbstractColumn[K] {
					override val buffs = fk.buffs.cascade(factory.forceKeyOutOf).declare()
					override val componentSelector = selector
				}
		}
		override def key :Column[K] = lazyKey

		root synchronized { //last minute init because references target
			lateComponents += (() => lazyKey)
		}
	}


	private trait InverseRelationshipValidation[+M[A] <: MappingAt[A], R]
		extends RelationshipMapping[M, R, O] with AbstractComponent[R]
	{
		key match {
			case comp :SimpleMapping[_, _]#AbstractComponent[_] if comp belongsTo root =>
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $key given as the local referenced key for a foreign key inverse is not a component of $root.")
		}
		registerSelf()
	}

	private class InverseFKComponent[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, X]
	                                (override val componentSelector :Sealed[S =?> R], buffs :Buffs[R])
	                                (key :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                                (table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X])
		extends InverseForeignKeyMapping[M, C, K, E, T, R, X, O](key, factory, buffs)(table, fk)
		   with EffectivelyEmptyMapping[R, O] with InverseRelationshipValidation[M, R]
	{
		def this(selector :S =?> R, buffs :Seq[Buff[R]])
		        (key :C[O], factory :RelatedEntityFactory[K, E, T, R])
		        (table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X]) =
			this(selector, root.buffs.unsafeCascade(selector).declare(buffs :_*))(key, factory)(table, fk)
	}

	private class ToManyDerivedComponent
	              [J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		           C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	              (override val componentSelector :Sealed[S =?> Derived[E, X]], override val joinTable :RelVar[J],
	               back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	               forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	               linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	               override val buffs :Buffs[Derived[E, X]])
	              (implicit composite :X ComposedOf E, link :TypeTag[JE])
		extends JoinTableManyMapping[J, T, C, TC, K, TK, JE, E, X, TR, JO, O](
			joinTable, back, forward, linkKin, targetKin, buffs
		 ) with AbstractComponent[Derived[E, X]] with InverseRelationshipValidation[T, Derived[E, X]]

	private class ToManyKinComponent
	              [J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		           C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	              (override val componentSelector :Sealed[S =?> Kin[X]], override val joinTable :RelVar[J],
	               back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	               forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	               linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	               override val buffs :Buffs[Kin[X]])
	              (implicit composite :X ComposedOf E, link :TypeTag[JE])
		extends JoinTableKinMapping[J, T, C, TC, K, TK, JE, E, X, TR, JO, O](
			joinTable, back, forward, linkKin, targetKin, buffs
		 ) with AbstractComponent[Kin[X]] with InverseRelationshipValidation[T, Kin[X]]




	private[this] lazy val lazyBuffs = {
		val bs = declaredBuffs
		if (bs == null)
			throw new IllegalStateException(
				s"$this.buffs is null: overrides must happen before any component declarations.")
		Buffs(this, bs :_*)
	}

	/** Optional flags and annotations modifying the way this mapping is used: primarily to include or exclude columns
	  * from a given type of SQL statements. It is inherited by columns and components of this mapping with the
	  * exception of those created using one of `embed` methods. As the result, declaring this component, for example,
	  * as [[net.noresttherein.oldsql.schema.Buff.OptionalSelect OptionalSelect]] will propagate this flag to
	  * all columns (including indirect ''export'' columns) and its check for the given buff will be positive.
	  * However, Any component on the path to a column, including the column itself, can declare the same buff,
	  * overriding the definition provided here. Additionally, some buffs
	  * [[net.noresttherein.oldsql.schema.Buff.BuffType.contradicts contradict]] other buffs; in that case any buff
	  * declared 'lower' and contradicting a given of these buffs, will 'shadow' it: any check for the shadowed buff
	  * on the component with the contradicting buff or its subcomponent will be negative.
	  *
	  * Most subclasses will choose to override
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.declaredBuffs declaredBuffs]] instead (if at all).
	  * @return `Buffs(this, `[[net.noresttherein.oldsql.schema.bases.SimpleMapping.declaredBuffs declaredBuffs]]`:_*)`.
	  */
	override def buffs :Buffs[S] = lazyBuffs

	/** Buffs declared by this instance (rather than inherited). It becomes
	  * the [[net.noresttherein.oldsql.schema.Buffs.front front]] portion of this instance's
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.buffs buffs]]. This property is for the use
	  * in `Buffs` constructor only and it should never be used in place of `buffs.declared`, as extending classes
	  * can override directly `buffs` rather than this property.
	  */
	protected def declaredBuffs :Seq[Buff[S]] = Nil



	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FilterView, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(InsertView, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UpdateView, subject)

	override def writtenValues[T](op :WriteOperationView, subject :S) :ComponentValues[S, O] = {
		val res = new IndexedColumnValuesBuilder
		writtenValues(op, subject, res) //StaticMapping delegates this to specific methods
		res.result()
	}

	private class IndexedColumnValuesBuilder extends ComponentValuesBuilder[S, O] {
		private[this] val columns = lazyColumns
		private[this] val columnCount = columns.size
		private[this] val values = new Array[Any](columnCount)
		private[this] var componentValues = Map.empty[TypedMapping[_, O], Any]

		override def addOpt[T](component :TypedMapping[T, O], result :Opt[T]) :this.type = export(component) match {
			case col :SimpleMapping[_, _]#AbstractColumn[_] =>
				values(col.index) = result.orNull; this
			case _ if !result.isEmpty =>
				componentValues = componentValues.updated(component, result.get); this
			case _ => this
		}

		override def result() =
			if (componentValues.isEmpty)
				ColumnValues(ArraySeq.unsafeWrapArray(values)) {
					case column :SimpleMapping[_, _]#AbstractColumn[_] => column.index
					case _ => -1
				}
			else {
				var i = 0
				while (i < columnCount) {
					val value = values(i)
					if (value != null)
						componentValues = componentValues.updated(columns(i), value)
					i += 1
				}
				ComponentValues(componentValues)
			}
	}


	//todo: index-based writtenValues
	private class ReadForm(final val columns :Unique[Column[_]],
	                       final val read :TypedColumn[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
		extends SQLReadForm[S] with ReadFormNullValue[S]
	{
		override val nulls = root.nullValue

		private[this] val fastColumns = columns.view.map {
			case col :SimpleMapping[_, _]#AbstractColumn[_] if col belongsTo root => col
			case col => throw new IllegalArgumentException(s"$col is not a column of $root.")
		}.toArray
		private[this] val forms = fastColumns.map(read)

		override val columnTypes :Seq[JDBCType] = ArraySeq.unsafeWrapArray(forms.flatMap(_.columnTypes))
		override val columnCount :Int = columnTypes.length

		override def opt(res :ResultSet, position :Int) :Opt[S] = {
			val vals = new Array[Any](fastColumns.length)
			java.util.Arrays.fill(vals.asInstanceOf[Array[AnyRef]], None)
			var i = 0
			while (i < forms.length) {
				vals(fastColumns(i).index) = forms(i).opt(res, position + i).orNull
				i += 1
			} //use non-aliasing values for speed - we are an ExportMapping
			val pieces = ColumnValues[S, O](ArraySeq.unsafeWrapArray(vals)) {
				case col :SimpleMapping[_, _]#AbstractColumn[_] => col.index
				case _ => -1
			}
			optionally(pieces)
		}

		override def register(call :CallableStatement, position :Int) :Unit = {
			var i = 0; val end = fastColumns.length
			while (i < end) {
				forms(i).register(call, position + i)
				i += 1
			}
		}


		override def comparable(other :SQLReadForm[_]) :Boolean = other match {
			case _ if other eq this => true
			case _ if columnCount != other.columnCount => false
			case other :SimpleMapping[_, _]#ReadForm =>
				columns.toSeq.view.map(_.form.sqlType) == other.columns.toSeq.view.map(_.form.sqlType)
			case _ =>
				new MappingReadForm[S, O](root, columns) comparable other
		}
		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if this eq self => true
			case other :SimpleMapping[_, _]#ReadForm => //this function equality should work, as there is no reason for closure
				(root identical other.root) &&
					(columns identical other.columns) && (columns.view.map(read) == other.columns.view.map(other.read))
			case _ => false
		}
		private def root = SimpleMapping.this
		override def hashCode :Int = (root.hashCode * 31 + columns.hashCode) * 31 + read.hashCode

		override def toString :String = columns.map(read).mkString(s"$root{", ",", "}>")
	}


	private lazy val lazySelectForm :SQLReadForm[S] = new ReadForm(selectedByDefault)

	override def selectForm :SQLReadForm[S] = lazySelectForm

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val selected = components map {
			case col :TypedColumn[_, O] @unchecked => col
			case comp => throw new IllegalArgumentException(
				s"$comp is not a column of $this. SimpleMapping.selectForm accepts only columns."
			)
		}

		if (selected.exists(NoSelect.active))
			throw new IllegalArgumentException(
				s"Can't create a select form for $root using $components: NoSelect buff present among the selection."
			)
		if (selectable.exists(c => !columns.contains(c) && OptionalSelect.inactive(c))) {
			val missing = (selectable.toSet -- columns.toSet).filter(OptionalSelect.inactive)
			throw new IllegalArgumentException(
				missing.mkString(
					s"Can't create a select form for $root using $components: missing mandatory columns ", ", ", ""
				)
			)
		}
		val extra = selected ++ SelectPreset.Active.columns(root)
		new ReadForm(selected :++ extra)
	}




	/** Prefix added to given names of all created instances of this.Column[_]. Defaults to "", subclasses may override.
	  * Overrides with a `val` or `var` (or any used in their implementation) must happen ''before'' any components
	  * of this mapping are initialized - in practice before any column declarations.
	  */
	protected override def columnPrefix = ""

	private final def verifiedPrefix :String = columnPrefix match {
		case null => throw new IllegalStateException(
			s"$this.columnPrefix is null: override with a val must happen before any component declarations.")
		case prefix => prefix
	}

	/** Constructors of components of this mapping which couldn't be created by their 'parent' components
	  * out of a concern for an infinite recursion caused by referencing cycles possible with foreign keys.
	  * These typically simply return a `lazy val` of some other component.
	  * The list is emptied (repeatedly) when any of the component/column collections of the mapping is accessed.
	  */
	private[this] var lateComponents   :ListBuffer[() => AbstractComponent[_]] = ListBuffer.empty
	private[this] var componentsBuffer :ListBuffer[AbstractComponent[_]] = ListBuffer.empty
	private[this] var columnsBuffer    :ListBuffer[AbstractColumn[_]] = ListBuffer.empty

	private lazy val lazyComponents = synchronized {
		while (lateComponents.nonEmpty) {
			val late = lateComponents
			lateComponents = ListBuffer.empty
			late.foreach { f => f() }
		}
		lateComponents = null
		val comps = Unique.from(componentsBuffer)
		componentsBuffer = null
		comps
	}

	private lazy val lazyColumns :Unique[Column[_]] = synchronized {
		lazyComponents //: @nowarn //initialize
		val cols = Unique.from(columnsBuffer)
		columnsBuffer = null
		cols
	}

	private lazy val lazyExtracts = {
		def entry[T](component :AbstractComponent[T]) =
			Assoc[Component, Extract, T](component, component.extract)
		NaturalMap(lazyComponents.view.map(entry(_)).toSeq :_*)
	}

	final override def extracts      :ExtractMap = lazyExtracts
	final override def columns       :Unique[Column[_]] = lazyColumns
	final override def components    :Unique[Component[_]] = lazyComponents
	final override def subcomponents :Unique[Component[_]] = lazyComponents


	override def apply[T](component :Component[T]) :Extract[T] = component match {
		case comp :SimpleMapping[_, _]#AbstractComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[Extract[T]]
		case _ =>
			throw new NoSuchComponentException(s"$component is not a component of $this.")
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
		case comp :SimpleMapping[_, _]#AbstractComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[ColumnExtract[T]]
		case _ =>
			throw new NoSuchComponentException(s"${column.debugString} is not a column of $this.")
	}




	/** Create a new column as a direct component of this mapping. The column will inherit any buffs declared
	  * by this mapping and its name will be the concatenation of this mapping's
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and `name`.
	  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
	  * @param property the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def column[T :ColumnForm](name :String, property :S => T, buffs :Buff[T]*) :Column[T] =
		new FlatColumn[T](verifiedPrefix + name, property, buffs)

	/** Create a new column representing a property of the mapped subject as a direct component of this mapping.
	  * The column's name will be the concatenation of this mapping's `columnPrefix` and the name of the property
	  * as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName` as the `value`
	  * argument will set the column name to "firstName". Any buffs declared on this mapping will be inherited
	  * by the created column.
	  * @param property the getter function for the value of this column. Should represent a simple property of the
	  *                 mapped subject of this mapping.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def column[T](property :S => T, buffs :Buff[T]*)
	                       (implicit form :ColumnForm[T], tag :TypeTag[S]) :Column[T] =
		column(verifiedPrefix + PropertyPath.nameOf(property), property, buffs :_*)

	/** Create a new column for an optional property of the mapped subject as a direct component of this mapping.
	  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
	  * instead. The column will inherit any buffs declared on this mapping and its name will be the concatenation
	  * of this mapping's [[net.noresttherein.oldsql.schema.bases.SimpleMapping.columnPrefix columnPrefix]] and `name`.
	  * @param name the name of the column. The actual name will have the `columnPrefix` of this mapping prepended to it.
	  * @param property the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def optcolumn[T :ColumnForm](name :String, property :S => Option[T], buffs :Buff[T]*) :Column[T] =
		new FlatColumn[T](verifiedPrefix + name, property, buffs)

	/** Create a new column for an optional property of the mapped subject as a direct component of this mapping.
	  * If the getter function returns `None`, the `nullValue` property of the form for this column will be used
	  * instead. The column's name will be the concatenation of this mapping's `columnPrefix` and the name
	  * of the property as appearing in scala and returned by `PropertyPath`. For example, passing `_.firstName`
	  * as the `value` argument will set the column name to "firstName". Any buffs declared on this mapping
	  * will be inherited by the created column.
	  * @param property the getter function returning the value for the column from an instance of the mapped subject.
	  * @param buffs any buffs modifying how the column is used, in particular excluding it from some column lists
	  *              of this mapping.
	  * @tparam T the type of the value of this column as present on the mapped entity.
	  * @return a new column, enlisted on all column lists of this mapping which aren't excluded by `buffs`.
	  */
	protected def optcolumn[T](property :S => Option[T], buffs :Buff[T]*)
	                          (implicit form :ColumnForm[T], tag :TypeTag[S]) :Column[T] =
		optcolumn(PropertyPath.nameOf(property), property, buffs :_*)



	protected override def fkImpl[M[A] <: TypedMapping[E, A], K, E, T, R]
	                             (name :String, property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => TypedColumn[K, _],
	                              factory :RelatedEntityFactory[K, E, T, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		new FKColumn[M, K, E, T, R, Unit](property, verifiedPrefix + name, buffs)(
			factory, table, key.asInstanceOf[M[Unit] => TypedColumn[K, Unit]]
		)
	//consider: should we be using columnPrefix for fk components with a rename function?
	protected override def fkImpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                             (property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, T, R])
	                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		new FKComponent[M, C, K, E, T, R, Unit](property, rename, buffs)(
			factory, table, key.asInstanceOf[M[Unit] => C[Unit]]
		)

	protected override def inverseFKImpl[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R]
	                       (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:JoinedEntityComponent[M, C, K, R, O] =
		new InverseFKComponent[M, C, K, E, T, R, Unit](property, buffs)(key, reference)(
			table, fk.asInstanceOf[M[Unit] => ForeignKeyMapping[MappingAt, C, K, _, Unit]]
		)

	protected override def kinImpl[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		                           C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                              (property :S => Kin[X], joinTable :RelVar[J],
	                               source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                               target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                               linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                               buffs :Buff[Kin[X]]*)
	                              (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[X], O] =
		new ToManyKinComponent[J, T, C, TC, K, TK, JE, E, X, TR, JO](
			property :S =?> Kin[X], joinTable, source, target, linkKin, targetKin,
			this.buffs.cascade(property).declare(buffs :_*)
		)

	protected override def manyImpl[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
		                            C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, X, TR <: Kin[E], JO]
	                               (property :S => Derived[E, X], joinTable :RelVar[J],
	                                source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                                target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                                linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                                buffs :Buff[Derived[E, X]]*)
	                               (implicit composite :X ComposedOf E, link :TypeTag[JE])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, X], O] =
		new ToManyDerivedComponent[J, T, C, TC, K, TK, JE, E, X, TR, JO](
			property :S =?> Derived[E, X], joinTable, source, target, linkKin, targetKin,
			this.buffs.cascade(property).declare(buffs :_*)
		)

}

