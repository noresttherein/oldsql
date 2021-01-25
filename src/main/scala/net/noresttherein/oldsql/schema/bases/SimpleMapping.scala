package net.noresttherein.oldsql.schema.bases

import java.sql.{CallableStatement, ResultSet}

import scala.collection.immutable.ArraySeq
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.NaturalMap.Assoc
import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.haul.{ColumnValues, ComponentValues}
import net.noresttherein.oldsql.model.{PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Optional}
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.schema.{cascadeBuffs, filterColumnExtracts, Buff, Buffs, ColumnExtract, ColumnForm, ColumnMapping, MappingExtract, SQLReadForm}
import net.noresttherein.oldsql.schema.support.{EffectivelyEmptyMapping, EmptyMapping}
import net.noresttherein.oldsql.schema.Buff.{ExtraSelect, NoSelect, OptionalSelect}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, StableColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.RelVar
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.SQLReadForm.ReadFormNullValue
import net.noresttherein.oldsql.schema.bits.{ForeignKeyColumnMapping, ForeignKeyMapping}
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, UPDATE, WriteOperationType}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ComponentValues.ComponentValuesBuilder
import net.noresttherein.oldsql.model.RelatedEntityFactory.KeyExtractor
import net.noresttherein.oldsql.morsels.witness.Maybe
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.{ForeignKeyEntityColumnMapping, ForeignKeyEntityMapping, InverseForeignKeyMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.{OpaqueColumnProxy, OpaqueProxy}






/** A base trait for mappings with a fixed structure, with columns known statically, and which should be created
  * manually, rather than by some generic code. While it exposes mutator methods to facilitate creation and declaration
  * of columns, it is expected that they'll be used solely in constructors of derived classes by those classes
  * themselves and, once created, it will be seen as immutable from the outside. If an attempt is made to modify
  * this instance once any of its accessor methods for components had been called previously, an exception
  * will be thrown.
  *
  * This mapping is flat: all mapped columns are direct components of this instance. It can have components,
  * but they are nominally empty: any column and component fields of such components are in fact, from the point of view
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
  */
trait SimpleMapping[S, O]
	extends StaticMapping[S, O] with LazyMapping[S, O] with RelatedMapping[S, O] with ExportMapping
{ root =>

	/** Base trait for all components of `SimpleMapping`, including columns, synthetic relationship components
	  * and user components. Components for business model objects should be derived from
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.FlatComponent FlatComponent]] rather than this trait.
	  */
	sealed trait AbstractComponent[T] extends BaseMapping[T, O] {
		protected[SimpleMapping] def componentSelector :S =?> T

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

		protected[SimpleMapping] def registerSelf() :Unit = root.synchronized {
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
	  */
	abstract class FlatComponent[T](property :S =?> T, relativePrefix :String, override val buffs :Buffs[T])
	                               (implicit nulls :Maybe[NullValue[T]])
		extends AbstractComponent[T] with StaticMapping[T, O] with EffectivelyEmptyMapping[T, O] with ExportMapping
		   with RelatedMapping[T, O] with LazyMapping[T, O]
	{ self =>
		def this(property :S =?> T, buffs :Buffs[T])(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, "", buffs)

		def this(property :S =?> T, prefix :String, buffs :Buff[T]*)(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, prefix, root.buffs.unsafeCascade(property).declare(buffs :_*))

		def this(property :S =?> T, buffs :Buff[T]*)(implicit nullValue :Maybe[NullValue[T]]) =
			this(property, "", buffs :_*)

		override val nullValue :NullValue[T] = nulls.opt getOrElse NullValue.NotNull
		protected def inheritedPrefix :String = root.verifiedPrefix
		protected override val columnPrefix :String = inheritedPrefix + relativePrefix

		protected[SimpleMapping] override def componentSelector :S =?> T = property

		registerSelf()

		private[this] var initExtractMap :NaturalMap[Component, Extract] = NaturalMap.empty
		private[this] val lazyExtracts = Lazy(synchronized { val res = initExtractMap; initExtractMap = null; res })

		protected[schema] override val lazyColumns :Lazy[Unique[Column[_]]] =
			Lazy(lazyColumnExtracts.get.collect { case Assoc(col, _) => col }.to(Unique))

		protected[schema] val lazyComponents :Lazy[Unique[Component[_]]] =
			Lazy(lazyExtracts.get.collect { case Assoc(comp, _) => comp }.to(Unique))

		final override def extracts :NaturalMap[Component, Extract] = lazyExtracts.get
		final override def components :Unique[Component[_]] = lazyComponents

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
		  * as the `value` argument will set the column name to "firstName". Any buffs declared on this mapping
		  * will be inherited by the created column. While not present on this component's column and component lists,
		  * it will be nevertheless accepted within
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


		protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, X, R]
		                             (name :String, property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => ColumnMapping[K, _],
		                              factory :RelatedEntityFactory[K, E, X, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		{
			val selector = Extractor.req(property)
			val totalBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new FKColumn[M, K, E, X, R, ()](extract andThenReq selector, columnPrefix + name, totalBuffs)(
					factory, table, key.asInstanceOf[M[()] => ColumnMapping[K, ()]]
				), selector
			)
		}

		protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
		                             (property :T => R, buffs :Buff[R]*)
		                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, X, R])
		                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val fullExtract = extract andThenReq selector
			val fullBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new FKComponent[M, C, K, E, X, R, ()](fullExtract, (s :String) => columnPrefix + rename(s), fullBuffs)(
					factory, table, key.asInstanceOf[M[()] => C[()]]
				), selector
			)
		}

		protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, X, R]
		                       (property :T => R, key :C[O], reference :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
		                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
				:ForeignKeyMapping[M, C, K, R, O] =
		{
			val selector = Extractor.req(property)
			val totalBuffs = this.buffs.cascade(property).declare(buffs :_*)
			borrow(
				new InverseFKComponent[M, C, K, E, X, R, ()](extract andThenReq selector, totalBuffs)(key, reference)(
					table, fk.asInstanceOf[M[()] => ForeignKeyMapping[MappingAt, C, K, _, ()]]
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



	private trait AbstractColumn[T] extends AbstractComponent[T] with ColumnMapping[T, O] {
		protected[SimpleMapping] val index = root.synchronized {
			if (componentsBuffer == null)
				throw new IllegalStateException(
					s"Cannot include $this as a column of $root: the root mapping has already been initialized."
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
	private class MirrorComponent[M <: RefinedMapping[T, X], T, X]
	                             (override val componentSelector :S =?> T, override val buffs :Buffs[T] = Buffs.empty[T])
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
			new MirrorComponent[RefinedMapping[U, X], U, X](selector, totalBuffs)(component, rename)
		}

		protected override def adapt[U](column :backer.Column[U]) :Column[U] = {
			val columnExtract = backer(column)
			val columnBuffs = buffs.unsafeCascade(columnExtract).declare()
			val selector = componentSelector andThen columnExtract
			column match {
				case simple :SimpleColumn[U @unchecked, X @unchecked] =>
					new FlatColumn[U](rename(column.name), selector, columnBuffs)(simple.form)
				case _ =>
					new OpaqueColumnProxy[U, O](column, rename(column.name), columnBuffs)
						with AbstractColumn[U]
					{
						protected[SimpleMapping] override val componentSelector = selector
					}
			}
		}


		def mirror[U](component :backer.Component[U]) :Component[U] = alias(component)
		def mirror[U](column :backer.Column[U]) :Column[U] = alias(column)
	}



	private class FKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	              (override val componentSelector :S =?> R, rename :String => String, override val buffs :Buffs[R])
	              (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => C[X])
		extends ForeignKeyEntityMapping[M, C, K, E, T, R, X, O](rename, factory, buffs)(table, pk)
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

		private[this] val lazyKey = Lazy {
			val extract = KeyExtractor(factory)
			val selector = componentSelector andThen extract
			new MirrorComponent[C[X], K, X](selector, buffs.cascade(factory.forceKeyOutOf), Nil)(target, rename)
		}
		override def key :Component[K] = lazyKey.get

		override def local[U](subKey :RefinedMapping[U, X]) :Component[U] = lazyKey.get.mirror(subKey)
		override def local[U](subKey :ColumnMapping[U, X]) :Column[U] = lazyKey.get.mirror(subKey)

		root.synchronized { //this component will be created lazily; it must be before root lists are accessed
			registerSelf()
			lateComponents += (() => lazyKey.get)
		}
	}


	private class FKColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X]
	                      (override val componentSelector :S =?> R, name :String, override val buffs :Buffs[R])
	                      (factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => ColumnMapping[K, X])
		extends ForeignKeyEntityColumnMapping[M, K, E, T, R, X, O](name, factory, buffs)(table, pk)
		   with AbstractColumn[R]
	{ fk =>
		def this(selector :S =?> R, name :String, buffs :Seq[Buff[R]])
				(factory :RelatedEntityFactory[K, E, T, R], table :RelVar[M], pk :M[X] => ColumnMapping[K, X]) =
			this(selector, name, root.buffs.unsafeCascade(selector).declare(buffs :_*))(factory, table, pk)

		private[this] val lazyKey = Lazy {
			val extract = KeyExtractor(factory)
			val selector = componentSelector andThen extract
			if (target.isInstanceOf[SimpleColumn[_, _]])
				new FlatColumn[K](name, selector, buffs.cascade(factory.forceKeyOutOf), Nil)(target.form)
			else
				new OpaqueColumnProxy[K, O](target, name, Buffs.empty[K]) with AbstractColumn[K] {
					override val buffs = fk.buffs.cascade(factory.forceKeyOutOf).declare()
					override val componentSelector = selector
				}
		}
		override def key :Column[K] = lazyKey

		root synchronized {
			lateComponents += (() => lazyKey.get)
		}
	}


	private class InverseFKComponent[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X]
	                                (override val componentSelector :S =?> R, buffs :Buffs[R])
	                                (key :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                                (table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X])
		extends InverseForeignKeyMapping[M, C, K, E, T, R, X, O](key, factory, buffs)(table, fk)
		   with AbstractComponent[R] with EffectivelyEmptyMapping[R, O] //with ExportMapping
	{
		def this(selector :S =?> R, buffs :Seq[Buff[R]])
		        (key :C[O], factory :RelatedEntityFactory[K, E, T, R])
		        (table :RelVar[M], fk :M[X] => ForeignKeyMapping[MappingAt, C, K, _, X]) =
		this(selector, root.buffs.unsafeCascade(selector).declare(buffs :_*))(key, factory)(table, fk)

		key match {
			case comp :SimpleMapping[_, _]#AbstractComponent[_] if comp belongsTo root =>
			case _ =>
				throw new IllegalArgumentException(
					s"Mapping $key given as the local referenced key for a foreign key inverse is not a component of $root."
				)
		}
		registerSelf()
	}




	private[this] val lazyBuffs = Lazy {
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
	override def buffs :Buffs[S] = lazyBuffs.get

	/** Buffs declared by this instance (rather than inherited). It becomes
	  * the [[net.noresttherein.oldsql.schema.Buffs.front front]] portion of this instance's
	  * [[net.noresttherein.oldsql.schema.bases.SimpleMapping.buffs buffs]]. This property is for the use
	  * in `Buffs` constructor only and it should never be used in place of `buffs.declared`, as extending classes
	  * can override directly `buffs` rather than this property.
	  */
	protected def declaredBuffs :Seq[Buff[S]] = Nil



	override def filterValues(subject :S) :ComponentValues[S, O] = writtenValues(FILTER, subject)
	override def insertValues(subject :S) :ComponentValues[S, O] = writtenValues(INSERT, subject)
	override def updateValues(subject :S) :ComponentValues[S, O] = writtenValues(UPDATE, subject)

	override def writtenValues[T](op :WriteOperationType, subject :S) :ComponentValues[S, O] = {
		val res = new IndexedColumnValuesBuilder
		writtenValues(op, subject, res) //StaticMapping delegates this to specific methods
		res.result()
	}

	private class IndexedColumnValuesBuilder extends ComponentValuesBuilder[S, O] {
		private[this] val columns = lazyColumns.get
		private[this] val columnCount = columns.size
		private[this] val values = new Array[Any](columnCount)
		private[this] var componentValues = Map.empty[RefinedMapping[_, O], Any]

		override def addOpt[T](component :RefinedMapping[T, O], result :Opt[T]) :this.type = export(component) match {
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



	override def apply[T](component :Component[T]) :Extract[T] = component match {
		case comp :SimpleMapping[_, _]#FlatComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[Extract[T]]
		case _ =>
			throw new NoSuchComponentException(s"$component is not a component of $this.")
	}

	override def apply[T](column :Column[T]) :ColumnExtract[T] = column match {
		case comp :SimpleMapping[_, _]#FlatComponent[_] if comp belongsTo this =>
			comp.extract.asInstanceOf[ColumnExtract[T]]
		case _ =>
			throw new NoSuchComponentException(s"$column is not a column of $this.")
	}

	private[this] var lateComponents :ListBuffer[() => AbstractComponent[_]] = ListBuffer.empty
	private[this] var componentsBuffer :ListBuffer[AbstractComponent[_]] = ListBuffer.empty
	private[this] var columnsBuffer :ListBuffer[AbstractColumn[_]] = ListBuffer.empty

	private val lazyComponents = Lazy(synchronized {
		while (lateComponents.nonEmpty) {
			val late = lateComponents
			lateComponents = ListBuffer.empty
			late.foreach { f => f() }
		}
		lateComponents = null
		val comps = Unique.from(componentsBuffer)
		componentsBuffer = null
		comps
	})

	protected[schema] override val lazyColumns :Lazy[Unique[Column[_]]] = Lazy(synchronized {
		lazyComponents.get
		val cols = Unique.from(columnsBuffer)
		columnsBuffer = null
		cols
	})

	private[this] val lazyExtracts = Lazy {
		def entry[T](component :AbstractComponent[T]) =
			Assoc[Component, Extract, T](component, component.extract)

		NaturalMap(lazyComponents.map(entry(_)) :_*)
	}

	final override def extracts :ExtractMap = lazyExtracts
	final override def components :Unique[Component[_]] = lazyComponents
	final override def subcomponents :Unique[Component[_]] = lazyComponents


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



	private class ReadForm(columns :Unique[Column[_]],
	                       read :ColumnMapping[_, O] => SQLReadForm[_] = (_:MappingAt[O]).selectForm)
		extends SQLReadForm[S] with ReadFormNullValue[S]
	{
		override val nulls = root.nullValue

		private[this] val fastColumns = columns.view.map {
			case col :SimpleMapping[_, _]#AbstractColumn[_] if col belongsTo root => col
			case col => throw new IllegalArgumentException(s"$col is not a column of $root.")
		}.toArray
		private[this] val forms = fastColumns.map(read)

		override def readColumns :Int = fastColumns.length

		override def opt(res :ResultSet, position :Int) :Opt[S] = {
			val vals = new Array[Any](fastColumns.length)
			java.util.Arrays.fill(vals.asInstanceOf[Array[AnyRef]], None)
			var i = 0
			while (i < forms.length) {
				vals(fastColumns(i).index) = forms(i).opt(res, position + i).orNull
				i += 1
			}
			val pieces = ColumnValues(root)(ArraySeq.unsafeWrapArray(vals)) {
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

		override def toString :String = columns.map(read).mkString(s"$root{", ",", "}>")
	}


	private[this] val lazySelectForm = Lazy(new ReadForm(selectedByDefault))

	override def selectForm :SQLReadForm[S] = lazySelectForm

	override def selectForm(components :Unique[Component[_]]) :SQLReadForm[S] = {
		val selected = components map {
			case col :ColumnMapping[_, O @unchecked] => col
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
		val extra = selected ++ ExtraSelect.Active.columns(root)
		new ReadForm(selected :++ extra)
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
		column(PropertyPath.nameOf(property), property, buffs :_*)

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



	protected override def fkimpl[M[A] <: RefinedMapping[E, A], K, E, T, R]
	                             (name :String, property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => ColumnMapping[K, _],
	                              factory :RelatedEntityFactory[K, E, T, R]) :ForeignKeyColumnMapping[M, K, R, O] =
		new FKColumn[M, K, E, T, R, ()](property, verifiedPrefix + name, buffs)(
			factory, table, key.asInstanceOf[M[()] => ColumnMapping[K, ()]]
		)
	//consider: should we be using columnPrefix for fk components with a rename function?
	protected override def fkimpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                             (property :S => R, buffs :Buff[R]*)
	                             (table :RelVar[M], key :M[_] => C[_], factory :RelatedEntityFactory[K, E, T, R])
	                             (rename :String => String) :ForeignKeyMapping[M, C, K, R, O] =
		new FKComponent[M, C, K, E, T, R, ()](property, rename, buffs)(
			factory, table, key.asInstanceOf[M[()] => C[()]]
		)

	protected override def inverseFKImpl[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R]
	                       (property :S => R, key :C[O], reference :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	                       (table :RelVar[M], fk :M[_] => ForeignKeyMapping[MappingAt, C, K, _, _])
			:ForeignKeyMapping[M, C, K, R, O] =
		new InverseFKComponent[M, C, K, E, T, R, ()](property, buffs)(key, reference)(
			table, fk.asInstanceOf[M[()] => ForeignKeyMapping[MappingAt, C, K, _, ()]]
		)

}

