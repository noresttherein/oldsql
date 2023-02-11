package net.noresttherein.oldsql.haul

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.NoSuchComponentException
import net.noresttherein.oldsql.haul.ColumnValues.{ColumnValue, ColumnValuesAliasing, EmptyValues}
import net.noresttherein.oldsql.haul.ComponentValues.{AliasedComponentValues, DedicatedComponentValues, FallbackValues}
import net.noresttherein.oldsql.morsels.generic.{=>:, GenericFun, Self}
import net.noresttherein.oldsql.schema.{ColumnMappingExtract, MappingExtract, Table}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.{SimpleColumn, TypedColumn}
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath
import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.OperationView.WriteOperationView

//here be implicits
import net.noresttherein.oldsql.slang._






/** Values for components of the given mapping together with possibly the value for the whole mapping,
  * used as input data for the assembly process defined by some mapping `TypedMapping[S, O]`.
  * By design, it is largely transparent to the client code requesting a value for a column or component
  * if the value actually exists in this object, or if it is assembled from the values for its subcomponents.
  * It is created from row data of an SQL query to map tabular result into an object, as well as by `Mapping`
  * from its subject value to provide values for columns to be used as statement parameters. Some implementations
  * work also the other way round: they start with a subject value `S` and disassemble it into values for the components
  * of the associated mapping before saving an entity, abstracting over exactly which columns take part in a given
  * database operation. It can be also used to modify the values of specific components on an existing subject
  * `x :S` by disassembling `x` into `ComponentValues`, substituting the values for the components we want to change,
  * and reassembling it again to `S` using the mapping.
  *
  * It is designed to support a hierarchical assembly process, where a mapping will assemble the final result from
  * the values of its direct components, which might not be defined explicitly in this instance, but assembled themselves
  * on request from lower level components' values. Specifying the values only for the bottom components, i.e. columns,
  * is the typical use case, but it is possible to set a value for any component in the tree. In the latter case, when
  * a value is requested in turn for its subcomponent, it will be disassembled by default from the predefined
  * higher-level value. It is therefore possible to disassemble the value of the mapping type of this component simply
  * by creating `ComponentValues` instance based on it and asking for the values of all columns of the mapping.
  *
  * As some `Mapping` implementations allow including instances of other `Mapping` classes and changing some of their
  * attributes, such as buffs or column names, it will often be the case that the component instance
  * - and definition - representing the schema which should be used for the assembly is not the original instance.
  * This is caused by the process of a mapping 'exporting' its subcomponents to the public, operative versions.
  * This produces a problem, because the original component will be unaware that one of its own components, which
  * it relies on for the assembly, is not the one that was used when presetting `ComponentValues` and, vice versa,
  * the root mapping may be unaware of the original versions of subcomponents of its components, as it will see
  * only their operative (from the point of view of its components) versions. This is solved by component aliasing:
  * every mapping is required to publish both the operative and original versions of its components and subcomponents
  * and be able to provide the operative version for any of them by its
  * [[net.noresttherein.oldsql.schema.Mapping.export export()]] method. Aliased `ComponentValues` instances will
  * in turn always substitute the component for which a value (or dedicated `ComponentValues`) is being requested with
  * its operative version before checking for a preset value or using it for assembly. This process can be enabled
  * on any mapping by calling one of its `aliased` methods and some implementations returned by the companion object
  * have this feature built in.
  *
  * Care should be taken in situations where a value is present
  * both for a particular component and some of its subcomponents, as the intended behaviour for this case is not clear.
  * The caller might want either to use the higher-level value, or copy it substituting the value for the lower-level
  * component. The default approach is to treat the subcomponents as overrides of particular portions of a component.
  * In fact, implementing this behaviour is only possible
  * when the values are accessed in the descending order, with the mapping only requesting the values either for its
  * direct components, or via a path to the subcomponent. There's no generic way of discovering the path to a given
  * (export) subcomponent of the associated mapping when asked directly. For example, when a mapping for class `Person`
  * containing an `address` component asks directly for the value of the 'street' column, instead of either specifying
  * the path `address \ street` or just the value of `address`, leaving the assembly to the address component,
  * there's no way of checking if there is a preset value for address that should be used as a source of data for
  * the street. For this reason (as well as proper encapsulation and reusability), proper mapping implementations
  * should always ask only for values direct components and always use a full path explicitly specifying
  * any intermediate components in the case when the higher-level assembly process requires checking the value
  * for a component/column which is not directly nested under this mapping.
  *
  * The interface methods can be grouped roughly into three sets: those which return a value for the given component,
  * those that return `ComponentValues` to pass for the assembly to the given component, and modification methods
  * allowing either changing the values, or combining it with another instance.
  *
  * @tparam S the subject type of the target mapping defining the components for which this instance contains values.
  * @tparam O the origin type of the target mapping and all components with values in this instance.
  */ //todo: replace all mentions of 'aliasing' with 'exporting' or 'actualizing'
trait ComponentValues[S, O] extends Cloneable {

	/** The type of components from the associated mapping (sharing the origin type with this instance). */
	type Component[T] = TypedMapping[T, O]

	/** The type of columns from the associated mapping (sharing the origin type with this instance). */
	type Column[T] = TypedColumn[T, O]

	/** The type of the mapping extract for a `Component[T]` of the associated mapping. */
	type Extract[T] = MappingExtract[S, T, O]

	/** The type of the column extract for a `Component[T]` of the associated mapping. */
	type ColumnExtract[T] = ColumnMappingExtract[S, T, O]


	/** Procure a value for the root mapping, either using a preset value or delegating to the mapping's assembly
	  * method. It is not the top-level function - use either `mapping.apply()` or  `this.root()` as the mapping
	  * should always have the last say on the result. This is implemented by a triple
	  * dispatch process - `mapping.apply()/optionally()` invokes this method and performs any validation or
	  * modification, such as providing a default value if none could be obtained. This method in turn will first check
	  * if `preset()` will return a result, and resorts to `mapping.assemble()` if nothing was set.
	  * @param mapping the root mapping associated with this instance. Note that ComponentValues should be always
	  *                statically typed for the singleton type of the associated (this) mapping and passing it to other
	  *                instances of the same mapping type is not supported in general. In fact, some implementations
	  *                store the associated mapping and are free to use it in implementation, and this argument should
	  *                be the exact same instance in that case.
	  */
	def assemble(mapping :TypedMapping[S, O]) :Opt[S] = {
		val res = preset(mapping)
		if (res.isDefined) res
		else mapping.assemble(this)
	}

	/** Delegates to `mapping(this)` without modifying the result. Intended for caching/logging implementations. */
	def subject(mapping :TypedMapping[S, O]) :S = mapping(this)

	/** Delegates to `mapping.optionally(this)` without modifying the result. intended for caching/logging implementations. */
	def optionally(mapping :TypedMapping[S, O]) :Opt[S] = mapping.optionally(this)

	/** Is there a predefined - explicitly set on creation of this/parent `ComponentValues` instance -
	  * value for the top level mapping associated with this instance? If this method returns `Got(x)`,
	  * `assemble` will also return `Got(x)`. If it returns `Lack`, `assemble` will delegate to `root.assemble(this)`.
	  * @param root the mapping associated with this instance.
	  */
	def preset(root :TypedMapping[S, O]) :Opt[S]



	/** Return the value for the given component. This method should always first adapt itself to the given component
	  * by creating `ComponentValues[T, O]` and passing it to `component.apply()` so it can inspect the
	  * returned value. Equivalent to `component(this / component)`. If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar)(party.leader.familiar.name)`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */ //consider: overriding these along with Mapping.apply, at least for some implementations to avoid boxing
	def apply[T](extract :Extract[T]) :T =
		get(extract) getOrElse {
			throw new NoSuchElementException("No value for " + extract + " in " + this)
		}

	/** Return the value for the given component. This method should always first adapt itself to the given component
	  * by creating `ComponentValues[T, O]` and passing it to `component.apply()` so it can inspect the
	  * returned value. Equivalent to `component(this / component)`. If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar)(party.leader.familiar.name)`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](component :Component[T]) :T =
		get(component) getOrElse {
			throw new NoSuchElementException("No value for " + component + " in " + this)
		}

	/** Return the value for the component at the end of the given path. This method will adapt itself in sequence
	  * to all components on the path, eventually creating a `ComponentValues[T, O]` and passing it to
	  * `component.apply()` so it can inspect the returned value. Equivalent to `component(this / path)` and
	  * `component(this / c1 / ... / cn / path.end)`, where `c1 ... cn` are components on the path from the current
	  * mapping to the target component, that is a sequence of directly nested components ending with
	  * the desired component. If this instance is aliased, either explicitly, by a previous call to one
	  * of the `aliased` methods, or as an inherent feature, all components will be substituted with their operative
	  * versions before checking for preset values or calling their methods. This is generally done by some ancestor
	  * mapping's `export` method in order to both use the proper, possibly changed version from the point of view
	  * of the mapping and to allow the preset values be recognized regardless of the version of the component,
	  * by any component in the hierarchy. If this instance is not aliased, a component on the path must be
	  * the exact same instance (or equal to it) for which a value was preset (if any) in order for it to be picked up,
	  * otherwise the values will return `Lack`. If no value is found, `path.end` will be used to assemble it
	  * from the values of lower level components.
	  * @param path a path from the mapping associated with this instance to the component for the desired subject type.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: Component[T], S, T, O]) :T =
		get(path) getOrElse {
			throw new NoSuchElementException("No value for " + path + " in " + this)
		}



	/** Return the value for the given component or `Lack` if it can't be obtained in any way (preset, assembly or
	  * default). Should be equivalent to `component.optionally(this / extract)`, rather than return a value directly
	  * without inspection by the component. If this instance is aliased, either explicitly by a previous call
	  * to one of the `aliased` methods, or as an inherent feature, the component will be substituted with its
	  * operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  */
	def get[T](extract :Extract[T]) :Opt[T] =
		extract.export.optionally(this / extract)

	/** Return the value for the given component or `Lack` if it can't be obtained in any way (preset, assembly or
	  * default). Should be equivalent to `component.optionally(this / component)`, rather than return a value directly
	  * without inspection by the component. If this instance is aliased, either explicitly by a previous call
	  * to one of the `aliased` methods, or as an inherent feature, the component will be substituted with its
	  * operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  */
	def get[T](component :Component[T]) :Opt[T] =
		component.optionally(this / component)

	/** Return the value for the component at the end of the given path or `Lack` if it can't be obtained in any way
	  * (preset, assembly or default). This method will adapt itself in sequence to all components on the path,
	  * eventually creating a `ComponentValues[T, O]` and passing it to `component.optionally()` so it can inspect
	  * the returned value. Equivalent to `component.optionally(this / path)` and
	  * `component.optionally(this / c1 / ... / cn / path.end)`, where `c1 ... cn` are components on the path from
	  * the current mapping to the target component, that is a sequence of directly nested components ending
	  * with the desired component. If this instance is aliased, either explicitly, by a previous call to one of the
	  * `aliased` methods, or as an inherent feature, all components will be substituted with their operative versions
	  * before checking for preset values or calling their methods. This is generally done by some ancestor mapping's
	  * `export` method in order to both use the proper, possibly changed version from the point of view of the mapping
	  * and to allow the preset values be recognized regardless of the version of the component, by any component in the
	  * hierarchy. If this instance is not aliased, a component on the path must be the exact same instance
	  * (or equal to it) for which a value was preset (if any) in order for it to be picked up, otherwise the values
	  * will return `Lack`. If no value is found, `path.end` will be used to assemble it from the values
	  * of lower level components.
	  * @param path a path from the mapping associated with this instance to the component for the desired subject type.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */ //todo: this is not overriden in all the places where other get are overriden
	def get[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: Component[T], S, T, O]) :Opt[T] =
		path.end.optionally(this / path)


	/** Return the value for the given component or `default`, if none can be obtained in any way
	  * (preset, assembly, mapping defaults).
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues.get(extract:Extract[T]) get]]
	  */
	def getOrElse[T, U >: T](extract :Extract[T], default: => U) :U = get(extract) match {
		case Got(res) => res
		case _ => default
	}

	/** Return the value for the given component or `default`, if none can be obtained in any way
	  * (preset, assembly, mapping defaults).
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues.get(component:Component[T]) get]]
	  */
	def getOrElse[T, U >: T](component :Component[T], default: => U) :U = get(component) match {
		case Got(res) => res
		case _ => default
	}


	/** Return `ComponentValues` for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping). If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  */
	def /[T](extract :Extract[T]) :ComponentValues[T, O] =
		this / extract.export

	/** Return `ComponentValues` for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping). If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `Lack`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `/` calls for all components on the path to
	  * the subcomponent as in `(this / party.leader / party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  */
	def /[T](component :Component[T]) :ComponentValues[T, O]

	/** Return `ComponentValues` for the component at the end of the given path. Equivalent to
	  * `this / c1 / ... / cn / path.end`, where `c1 ... cn` are components on the path from
	  * the current mapping to the target component, that is a sequence of directly nested components ending
	  * with the desired component. If this instance is aliased, either explicitly, by a previous call to one of the
	  * `aliased` methods, or as an inherent feature, all components will be substituted with their operative versions
	  * before checking for preset values or calling their methods. This is generally done by some ancestor mapping's
	  * `export` method in order to both use the proper, possibly changed version from the point of view of the mapping
	  * and to allow the preset values be recognized regardless of the version of the component, by any component in the
	  * hierarchy. If this instance is not aliased, a component on the path must be the exact same instance
	  * (or equal to it) for which a value was preset (if any) in order for it to be picked up, otherwise the values
	  * will return `Lack`.
	  * @param path the path from the current mapping to the desired component.
	  */
	def /[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: Component[T], S, T, O]) :ComponentValues[T, O] =
		path.carry(this)



	/** Create `ComponentValues` which will fall back to the given instance when no value for a particular component can
	  * be obtained in any way. The values in this instance will always have precedence, even if they are defined
	  * at a lower level. For example, if this instance contains a value for a single column, and the argument contains
	  * a preset value for the whole mapping, the result of the mapping will still be assembled from lower-level
	  * components so that the value of the column defined here is used, disassembling the values for other components
	  * from the argument.
	  * @param values `ComponentValues` instance with values which should be returned if this instance does not carry
	  *              a value for a requested component.
	  * @return A `ComponentValues` instance combining these values with the given values.
	  */
	def orElse(values :ComponentValues[S, O]) :ComponentValues[S, O] = values match {
		case _ :EmptyValues[_, _] => this
		case _ => new FallbackValues[S, O](this, values)
	}



	/** Create `ComponentValues` which will contain the values from both this and the given instance.
	  * This is similar to `this orElse values`, but the value returned for a component can come from either
	  * instance if they both define it, and subclasses can perform some optimisations when combined with a compatible
	  * implementation. If both the instances share the same aliasing, the returned instance will use the same aliasing.
	  * If only one is aliased or aliasing functions differ, the result is unspecified; by default, the components
	  * will be aliased as per characteristics of both instances in turn (as per two cases, not function composition),
	  * but it is possible for this instance to impose its own aliasing over the values from the second instance.
	  * @return A `ComponentValues` instance guaranteed to have a value as long as it exists in `this` or `values`.
	  *         Defaults to `values orElse this`.
	  * @see [[net.noresttherein.oldsql.haul.ComponentValues.orElse]]
	  */
	def ++(values :ComponentValues[S, O]) :ComponentValues[S, O] = values orElse this



	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances) by passing them
	  * to the given mapping's [[net.noresttherein.oldsql.schema.Mapping.export export]] method before passing
	  * the result to the corresponding method of this instance. The proxy is sticky: descending down the component tree
	  * with `/` maintains the wrapping of all created `ComponentValues`. Methods which combine or change the behaviour
	  * of `ComponentValues` such as `this orElse that` however work as for every other instance,
	  * with `that` not becoming aliased as the result. Subsequent calls result in function composition,
	  * with the most recent one providing the most nested function. This step is required as all mappings, which allow
	  * embedding ready mapping instances as components and changing some of their attributes by adapting them,
	  * end up with several versions of the component for the same subject: the original one, the operative version
	  * exposed on the component lists of the root mapping and possibly any adapters created by the chain
	  * of enclosing subcomponents leading to the component in question. It is the operative versions
	  * which should be used for assembly and for which values are preset, but the actual assembly code
	  * is usually defined by the intermediate components in terms of the component as created or adapted by them.
	  * Aliasing causes all mappings in the hierarchy to use the same, operative or 'export', version as made public
	  * by the root mapping.
	  */ //todo: rename
	def aliased(root :TypedMapping[S, O]) :ComponentValues[S, O] = aliased(ComponentValues.aliasing(root))

	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances
	  * as well as the root mapping to which this instance is dedicated) by substituting all components with
	  * `export` property of the `MappingExtract` associated with it in the given map. If the map doesn't have an entry
	  * for a mapping (in particular the root mapping), a `NoSuchElementException` will be thrown. The proxy is sticky:
	  * descending down the component tree with `/` maintains the wrapping of all created `ComponentValues`.
	  * Methods which combine or change the behaviour of `ComponentValues` such as `this orElse that` however work
	  * as for every other instance, with `that` not becoming aliased as the result. Subsequent calls result
	  * in function composition, with the most recent one providing the most nested function. This step is required
	  * as all mappings, which allow embedding ready mapping instances as components and changing
	  * some of their attributes by adapting them, end up with several versions of the component for the same subject:
	  * the original one, the operative version exposed on the component lists of the root mapping and possibly
	  * any adapters created by the chain of enclosing subcomponents leading to the component in question.
	  * It is the operative versions which should be used for assembly and for which values are preset,
	  * the actual assembly code is usually defined by the intermediate components in terms of the component as created
	  * or adapted by them. Aliasing causes all mappings in the hierarchy to use the same, operative or 'export',
	  * version as made public by the root mapping.
	  */
	def aliased[T](extracts :NaturalMap[MappingAt[O]#Component, TypedMapping[T, O]#Extract]) :ComponentValues[S, O] =
		aliased(ComponentValues.aliasing(extracts))

	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances
	  * as well as the root mapping to which this instance is dedicated) by applying the given function and passing
	  * the returned component to the corresponding method of this instance. The proxy is sticky: descending
	  * down the component tree with `/` maintains the wrapping of all created `ComponentValues`. Methods which combine
	  * or change the behaviour of `ComponentValues` such as `this orElse that` however work as for every other instance,
	  * with `that` not becoming aliased as the result. Subsequent calls result in function composition,
	  * with the most recent one providing the most nested function. This step is required as all mappings
	  * which allow embedding ready mapping instances as components and changing some of their attributes
	  * by adapting them, end up with several versions of the component for the same subject: the original one,
	  * the operative version exposed on the component lists of the root mapping and possibly any adapters created
	  * by the chain of enclosing subcomponents leading to the component in question. It is the operative versions
	  * which should be used for assembly and for which values are preset, the actual assembly code is usually defined
	  * by the intermediate components in terms of the component as created or adapted by them. Aliasing causes
	  * all mappings in the hierarchy to use the same, operative or 'export', version as made public by the root mapping.
	  */
	def aliased(export :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValues[S, O] =
		new AliasedComponentValues[S, O](this, export)

	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances) by passing them
	  * to the given mapping's [[net.noresttherein.oldsql.schema.Mapping.exportOrNot exportOrNot]] method before passing
	  * the result to the corresponding method of this instance.
	  * The proxy is sticky: descending down the component tree with `/` maintains the wrapping of all
	  * created `ComponentValues`. Methods which combine or change the behaviour of `ComponentValues`
	  * such as `this orElse that` however work as for every other instance, with `that` not becoming aliased
	  * as the result. Subsequent calls result in function composition, with the most recent one providing
	  * the most nested function. This step is required as all mappings, which allow embedding ready mapping instances
	  * as components and changing some of their attributes by adapting them, end up with several versions
	  * of the component for the same subject: the original one, the operative version exposed on the component lists
	  * of the root mapping and possibly any adapters created by the chain of enclosing subcomponents leading
	  * to the component in question. It is the operative versions which should be used for assembly
	  * and for which values are preset, but the actual assembly code is usually defined by the intermediate components
	  * in terms of the component as created or adapted by them. Aliasing causes all mappings in the hierarchy
	  * to use the same, operative or 'export', version as made public by the root mapping.
	  */
	def selectivelyAliased(root :TypedMapping[S, O]) :ComponentValues[S, O] =
		aliased(ComponentValues.selectiveAliasing(root))

	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances
	  * as well as the root mapping to which this instance is dedicated) by substituting all components with
	  * `export` property of the `MappingExtract` associated with it in the given map. If the map doesn't have an entry
	  * for a mapping (in particular the root mapping), the mapping itself will be used.
	  * Otherwise it behaves exactly as [[net.noresttherein.oldsql.haul.ComponentValues.aliased(extracts* aliased]]
	  * method.
	  */
	def selectivelyAliased[T](extracts :NaturalMap[MappingAt[O]#Component, TypedMapping[T, O]#Extract])
			:ComponentValues[S, O] =
		aliased(ComponentValues.selectiveAliasing(extracts))

	/** Create proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * [[net.noresttherein.oldsql.schema.MappingExtract MappingExtract]] and `MappingPath` instances
	  * as well as the root mapping to which this instance is dedicated) by substituting all components with
	  * the given function. If the function returns `None` for a mapping (in particular the root mapping),
	  * the mapping itself will be used. Otherwise it behaves exactly
	  * as [[net.noresttherein.oldsql.haul.ComponentValues.aliased(export* aliased]] method.
	  */
	def selectivelyAliased[T](export :MappingAt[O]#Component =>: ({ type E[X] = Option[TypedMapping[X, O]] })#E)
			:ComponentValues[S, O] =
		aliased(ComponentValues.selectiveAliasing(export))



	/** Create `ComponentValues` for any subject type, dedicated to the given mapping. It will remain unchanged
	  * for the whole assembly (returning itself from `/`). For the purpose of all operations, it will behave like
	  * empty values. The only exception comes when the passed component argument is equal to the mapping given here,
	  * in which case it will return this instance (or use it for assembly).
	  * This is useful when one wants to preset component values for a member component from further down the hierarchy,
	  * without knowing its top-level 'export' representation. Can be combined with `orElse` to provide preset values
	  * for selected subcomponents and fallback to default algorithm for all other, or combined using `++` into values
	  * for some composite `X` with other values for its individual components.
	  */
	def tiedTo[X](mapping :TypedMapping[S, O]) :ComponentValues[X, O] = {
		val assoc = NaturalMap.single[MappingAt[O]#Component, ComponentValues.WithOrigin[O]#T, S](mapping, this)
		new DedicatedComponentValues[S, O](assoc).asComponentsOf[X]
	}



	/** Same as [[net.noresttherein.oldsql.haul.ComponentValues.cache cache]]`(table)`. */
	def apply[T](table :Table[MappingOf[T]#Projection]) :FutureValues[T] = cache(table)

	/** A cache of results of mapping the rows of the given table. The value pool is growing: at any given moment,
	  * it contains only values read so far as part of mapping a `ResultSet` and, possibly, those from previous
	  * and future ''selects'' if a given query resulted in executing several SQL statements. It is provided here
	  * to allow resolving of reference types such as [[net.noresttherein.oldsql.model.Kin Kin]] to their values
	  * in other tables, such as by following a foreign key. As the referenced value(s) might not be read at this time,
	  * this must happen lazily. This method always returns a cache, but it might be a permanently empty one
	  * if information about other tables is unavailable (for example, because this instance is not a result of a query).
	  */
	def cache[T](table :Table[MappingOf[T]#Projection]) :FutureValues[T] = FutureValues.Empty


	/** Shortcut for casting the type parameter to a different type reserved for the implementations */
	@inline final private[ComponentValues] def asComponentsOf[X] :ComponentValues[X, O] =
		this.asInstanceOf[ComponentValues[X, O]]


	//todo: remove it; why we were needing it in the first place?
	override def clone() :ComponentValues[S, O] = super.clone().asInstanceOf[ComponentValues[S, O]]

	def canEqual(that :Any) :Boolean = that.isInstanceOf[ComponentValues[_, _]]

	protected def publicClassName = "ComponentValues"
}






/** Factory for containers of disassembled values for components of a mapping, which can be used as input data for
  * the mapping. They are created typically from row data of an SQL query to map tabular result into an object, but can
  * be also used to set the values of specific components on an existing value `x :S` by disassembling `x`
  * into `ComponentValues`, substituting the values for the components one wants to change, and reassembling it again
  * to `S` using the mapping.
  *
  * It is designed to support a hierarchical assembly process, where a mapping will assemble the final result from
  * the values of its direct components, which might not be defined explicitly in this instance, but assembled themselves
  * on request from lower level component values. Specifying the values only for the bottom components, i.e. columns,
  * is the typical use case, but it is possible to set a value for any component in the tree. In the latter case, when
  * a value is requested in turn for its subcomponent, it will be disassembled from the preset higher-level value.
  */ //todo: factory methods for instances with FutureValues
object ComponentValues {
	type WithOrigin[O] = { type T[S] = ComponentValues[S, O] }

	/** Returns `ComponentValues` using the given generic function as the source of values for components.
	  * The return values of the function are used as the preset values yielded by the created instance,
	  * with `null` resulting in the value of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
	  * function - if it is `null` it may result in an assembly failure. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs and thus it should be taken care of by the function itself.
	  * You may wish to consider using `ComponentValues(mapping)(values)` instead, which is compatible with any mapping.
	  * You can supply a `NaturalMap` as an argument. In this case the map need not contain entries for all possible
	  * components; if a component is missing from the map, `Lack` is returned as its value.
	  * @param values A factory of values for components. If the function returns `null`, it is treated
	  *               as lack of value, and the `ComponentValues` will return `Lack` for that component.
	  */
	def apply[S, O](values :MappingAt[O]#Component =>: Self) :ComponentValues[S, O] = values match {
		case map :NaturalMap[MappingAt[O]#Component @unchecked, Self @unchecked] =>
			new ComponentValuesNaturalMap(map)
		case _ => new TypedValues[S, O](values)
	}

	/** Returns `ComponentValues` using the given function as the source of values for components.
	  * The return values of the function are used as the preset values yielded by the created instance,
	  * with `null` resulting in the value of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
	  * function - if it is `null` it may result in an assembly failure. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs and thus it should be taken care of by the function itself.
	  * You may wish to consider using `ComponentValues(mapping)(values)` instead, which is compatible with any mapping.
	  * You can supply a `Map` as an argument. In this case, the map needs not contain entries for all components:
	  * if a component is missing from the map, `Lack` is returned as its value.
	  * @param values factory of values for components, must return a value of the same type as the input mapping
	  *               or `null`.
	  */
	def apply[S, O](values :TypedMapping[_, O] => Any) :ComponentValues[S, O] = values match {
		case map :Map[_, _] =>
			new ComponentValuesMap(map.asInstanceOf[Map[TypedMapping[_, O], Any]])
		case _ => new UntypedValues(values)
	}


	/** Returns `ComponentValues` wrapping the given set of values with their assignment to components happening
	  * by the use of the provided indexing function. An entry of `null` or a negative index results in the
	  * subject of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would yield a value only if explicitly provided here.
	  * Note that no component aliasing will take place, which can result in non-operative version of a component
	  * being used and a failed assembly or a result based on incorrect buffs. You may wish to consider using
	  * `ComponentValues(mapping)(values)(index)` instead.
	  * @param values preset values for selected components of the target mapping, in any order consistent with `index`.
	  * @param index A function returning the index of the value for the component in the given sequence.
	  *              If the component has no preset value, it should return a negative number.
	  */
	def apply[S, O](values :IndexedSeq[Any])(index :MappingAt[O] => Int) :ComponentValues[S, O] =
		new IndexedValues(values, index)


	/** Creates a `ComponentValues` instance with a given preset for the specified mapping. If non-empty,
	  * the values for subcomponents will be extracted from it using the `MappingExtract`s supplied by the mapping.
	  * This is equivalent to `ComponentValues.preset(mapping, result.get)` or `ComponentValues.empty`, based
	  * on whether the `result` is defined. Note that no component aliasing will take place, which can result
	  * in non-operative version of a component being used and a failed assembly or a result based on incorrect buffs.
	  * You may wish to consider using `ComponentValues(mapping)(result)` instead.
	  */
	def apply[S, O](mapping :TypedMapping[S, O], result :Option[S]) :ComponentValues[S, O] = result match {
		case Some(root) => new DisassembledValues(mapping, root)
		case _ => empty[S, O]
	}

	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * Note that no component aliasing will take place, which can result in non-operative version of a component
	  * being used and a failed assembly or a result based on incorrect buffs. You may wish to consider using
	  * `ComponentValues(mapping)(value)` instead, which follows exactly the general `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def apply[S, O](mapping :TypedMapping[S, O], value :S) :ComponentValues[S, O] =
		new DisassembledValues(mapping, value)

	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * Unlike `ComponentValues(mapping, value)`, only the components listed in the third argument
	  * will have their value preset, with the others being assembled from their subcomponents. Note that while you can
	  * provide a value for any component, not only columns, if any other subcomponent relies on the value of one of
	  * its subcomponents directly (rather than extracting it from its assembled subject),
	  * it will likely bypass the check for the preset value and obtain `Lack` instead, resulting
	  * in a failed or incorrect assembly. You may wish to consider using `ComponentValues(mapping)(value, components)`
	  * instead, which follows exactly the general `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def apply[S, O](mapping :TypedMapping[S, O], value :S, components :Unique[TypedMapping[_, O]]) :ComponentValues[S, O] =
		new ChosenDisassembledValues(mapping, value, components)


	/** An empty instance, returning always `Lack` or throwing `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ComponentValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  */
	def apply[S, O]() :ComponentValues[S, O] = ColumnValues.empty

	/** An empty instance, returning always `Lack` or throwing `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ComponentValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  */
	def empty[S, O] :ComponentValues[S, O] = ColumnValues.empty

	/** An empty instance, returning always `Lack` or throwing a `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ComponentValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  * @param source a description of the original source which created this instance for more helpful message in
	  *               thrown exceptions
	  */
	def empty[S, O](source : => String) :ComponentValues[S, O] = ColumnValues.empty(source)


	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * The values for the columns of the mapping are ''always'' extracted from the preset value using
	  * the mapping's `MappingExtract` for the component; the columns' `optionally` (or `apply` and `assemble`)
	  * methods are never called and neither are they being aliased to their operative version provided by the extract.
	  * The `optionally`/`apply` methods of other mappings are being called as normal, allowing them to modify
	  * the preset result. Note that no component aliasing will take place, which can result in non-operative version
	  * of a component being used and a failed assembly or a result based on incorrect buffs.
	  * You may wish to consider using `ComponentValues(mapping)(value)` instead, which follows exactly the general
	  * `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def preset[S, O](mapping :TypedMapping[S, O], value :S) :ComponentValues[S, O] =
		new DisassembledValues[S, S, O](mapping, value)

	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * The values for the columns of the mapping are ''always'' extracted from the preset value using
	  * the mapping's `MappingExtract` for the component; the columns' `optionally` (or `apply` and `assemble`)
	  * methods are never called and neither are they being aliased to their operative version provided by the extract.
	  * The `optionally`/`apply` methods of other mappings are being called as normal, allowing them to modify
	  * the preset result. Note that no component aliasing will take place, which can result in non-operative version
	  * of a component being used and a failed assembly or a result based on incorrect buffs.
	  * You may wish to consider using `ComponentValues(mapping)(value)` instead, which follows exactly the general
	  * `ComponentValues` contract.
	  * @param result the result, top-level value.
	  * @return `ComponentValues.empty` or `ComponentValues.preset(mapping, result.get)`, depending on whether `result`
	  *        is empty.
	  */
	def preset[S, O](mapping :TypedMapping[S, O], result :Option[S]) :ComponentValues[S, O] =
		if (result.isEmpty) empty else preset(mapping, result.get)


	/** Similar to `ComponentValues(mapping, value)`, but the value is not computed until actually needed.
	  * The expression will be evaluated at most once. The values for the subcomponents of the mapping
	  * are extracted from the preset value using the mapping's `MappingExtract` for the component.
	  * Note that the components are not being aliased to their operative version provided by the extract.
	  */
	def later[S, O](mapping :TypedMapping[S, O], value: => S) :ComponentValues[S, O] =
		new LazyDisassembledValues(mapping, () => Some(value)) //this will get boxed, but it's only one call.

	/** Create a lazy, preset instance using the value of the given expression for the component.
	  * Similar to `later`, but the expression might not produce a value (return `Lack`), and used generally
	  * as a fallback default value for when the first choice couldn't be obtained.
	  * Note that the components are not being aliased to their operative version provided by the extract.
	  */
	def fallback[S, O](mapping :TypedMapping[S, O], value: => Opt[S]) :ComponentValues[S, O] =
		new LazyDisassembledValues(mapping, () => value.toOption) //this will get boxed, but it's only one call.


	/** A proxy `ComponentValues` which delegates all assembly-related and '/' calls to the lazily computed
	  * instance initialized with the by-name parameter.
	  */
	def delay[S, O](values: => ComponentValues[S, O]) :ComponentValues[S, O] =
		new LazyComponentValuesProxy(values)


	/** Creates a new builder assembling a `ComponentValues` instance from the provided values for individual components. */
	def newBuilder[S, O] :ComponentValuesBuilder[S, O] = new ComponentValuesMapBuilder[S, O]



	/** Factory for `ComponentValues` which perform aliasing of all passed components before checking
	  * for preset values or using them for assembly. They guarantee that the operative version of the component
	  * is being used, with correct column names and buffs reflecting any changes imposed by their owners.
	  * Should be the first choice for obtaining `ComponentValues` instances.
	  * You can follow this application for additional argument list(s) to create `ComponentValues` instances:
	  * {{{
	  *     ComponentValues(mapping)(subject) //provide a preset value for the mapping and its subcomponents
	  *     ComponentValues(mapping) { comp => Option(value(comp)) } //provide a function with optional preset component values
	  *     ComponentValues(mapping)(values) { comp => indexOf(comp) } //provide a sequence of values and an index function
	  * }}}
	  */
	@inline def apply[S, O](mapping :TypedMapping[S, O]) :ComponentValuesFactory[S, O] =
		new ComponentValuesFactory[S, O](mapping)


	/** Factory for `ComponentValues` instances associated with the given mapping.
	  * @param mapping a mapping for which `ComponentValues` are begin created.
	  * @tparam S the subject type of the associated 'root' mapping.
	  * @tparam O the origin type shared by the associated mapping and its subcomponents.
	  */
	class ComponentValuesFactory[S, O](private val mapping :TypedMapping[S, O]) extends AnyVal { factory =>

		/** Returns `ComponentValues` based on a predefined mapping result. The values for all components will be
		  * obtained by disassembling (extracting) their value from the argument based on the function specified by
		  * the `MappingExtract` provided by the component. The final value returned for a component
		  * is the result of its `optionally` method and may differ from the one extracted from the argument.
		  * Before assembly, every component is aliased using the [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method of the mapping provided earlier. This method can be used to easily get the column values
		  * for a mapping before inserting/updating a value, or as fallback values for another, not complete instance.
		  * @param value proposed result of the mapping.
		  */
		def apply(value :S) :ComponentValues[S, O] =
			new DisassembledValues[S, S, O](mapping, value) with MappingAliasing[S, O] {
				override val mapping = factory.mapping
			}

		/** Returns `ComponentValues` based on a predefined mapping result. If it is not empty, the values
		  * for all components will be obtained by disassembling (extracting) their value from the argument based on
		  * the function specified by the `MappingExtract` provided by the component. This method is equivalent
		  * to either `ComponentValues(mapping)(result.get)` or `ComponentValues(mapping)()`, depending on whether
		  * `result` is defined.
		  * @param result proposed result of the mapping (the final result is that of the mapping's `optionally` method).
		  */
		def apply(result :Option[S]) :ComponentValues[S, O] = result match {
			case Some(value) => apply(value)
			case _ => apply()
		}

		/** Returns `ComponentValues` based on a predefined mapping result. The components listed in the second
		  * argument will be have their values disassembled (extracted from the preset value) using the `MappingExtract`
		  * supplied by the mapping provided earlier and `preset()` calls for all other will always return `Lack`,
		  * so that their value is assembled from lower level components. All components used for assembly
		  * (including those with preset values) are aliased with the mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before invoking their `optionally`
		  * (or `apply`) method. Note that while you can supply any components, not only columns, if any mapping
		  * in the hierarchy relies in its assembly on a value of a non-direct subcomponent, it can bypass the
		  * value preset here. This will likely result in its failure to assemble its subject.
		  * @param value subject of the root mapping serving as input for values of the given components.
		  * @param components list of components which should be used as sources in the assembly process.
		  */
		def apply(value :S, components :Unique[TypedMapping[_, O]]) :ComponentValues[S, O] =
			new ChosenDisassembledValues[S, S, O](mapping, value, components) with MappingAliasing[S, O] {
				override val mapping = factory.mapping
			}


		/** Returns `ComponentValues` using the given generic function as the source of values for components.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with `null` resulting in the value of the component being assembled from its subcomponents.
		  * Preset values are never disassembled into values for subcomponents: providing a value for a component
		  * other than a column (instead of its columns) requires that no other component, including its owner,
		  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
		  * function - if it is `null` it may result in an assembly failure.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods. You can supply a `NaturalMap` as an argument. In that case the map needs not
		  * contain entries for all components - if a component is missing from the map, `Lack` is returned
		  * as its value.
		  * @param values A factory of values for components. If the function returns `null`, it is treated
		  *               as lack of value, and the `ComponentValues` will return `Lack` for that component.
		  */
		def apply(values :MappingAt[O]#Component =>: Self) :ComponentValues[S, O] = values match {
			case map :NaturalMap[MappingAt[O]#Component @unchecked, Self @unchecked] =>
				new ComponentValuesNaturalMap[S, O](map) with MappingAliasing[S, O] {
					override val mapping = factory.mapping
				}
			case _ =>
				new TypedValues[S, O](values) with MappingAliasing[S, O] {
					override val mapping = factory.mapping
				}
		}

		/** Returns `ComponentValues` using the given function as the source of values for components.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with `null` resulting in the value of the component being assembled from its subcomponents.
		  * Preset values are never disassembled into values for subcomponents: providing a value for a component
		  * other than a column (instead of its columns) requires that no other component, including its owner,
		  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
		  * function - if it is `null` it may result in an assembly failure.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods. You can supply a `Map` as an argument; in that case the map may omit entries
		  * for components without preset values and the created instance will return `Lack` as their values.
		  * @param values factory of values for components, must return an option of the same type as the input mapping.
		  */
		def apply(values :TypedMapping[_, O] => Any) :ComponentValues[S, O] = values match {
			case map :Map[_, _] =>
				new ComponentValuesMap[S, O](map.asInstanceOf[Map[TypedMapping[_, O], Any]])
					with MappingAliasing[S, O]
				{
					override val mapping = factory.mapping
				}
			case _ =>
				new UntypedValues[S, O](values) with MappingAliasing[S, O] {
					override val mapping = factory.mapping
				}
		}

		/** Returns `ComponentValues` wrapping the given set of values with their assignment to components happening
		  * by the use of the provided indexing function. An entry of `null` or a negative index results in the
		  * subject of the component being assembled from its subcomponents.
		  * Preset values are never disassembled into values for subcomponents: providing a value for a component
		  * other than a column (instead of its columns) requires that no other component, including its owner,
		  * accesses any of its subcomponents directly, as it would yield a value only if explicitly provided here.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods.
		  * @param values preset values for selected components of the supplied mapping, in any order consistent
		  *               with `index`.
		  * @param index A function returning the index of the value for the component in the given sequence.
		  *              If the component has no preset value, it should return a negative number.
		  */
		def apply(values :IndexedSeq[Any])(index :MappingAt[O] => Int) :ComponentValues[S, O] =
			new IndexedValues[S, O](values, index) with MappingAliasing[S, O] {
				override val mapping = factory.mapping
			}


		/** Create an empty instance returning always `Lack`/another `Empty ComponentValues` or throwing
		  * a `NoSuchElementException`. It will print the provided mapping in its `toString` method and
		  * any exception messages resulting from a lack of value. Note that all components passed to this
		  * instance will be aliased with this mapping's [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method, which guarantees that the operative version of the component is being used for assembly
		  * (and thus it and its subcomponents have the final set of buffs), but is somewhat less efficient
		  * and might be completely unnecessary in most use cases.
		  */
		def apply() :ComponentValues[S, O] =
			new EmptyValues[S, O](mapping.toString) with MappingAliasing[S, O] with ColumnValuesAliasing[S, O] {
				override val mapping = factory.mapping
			}


		/** Creates a builder of a `ComponentValues` instance by collecting values for individual components.
		  * All the components passed to its `add` method are aliased to their export version by the mapping
		  * specified earlier and the created `ComponentValues` will likewise perform component aliasing.
		  */
		def newBuilder :ComponentValuesBuilder[S, O] =
			new ComponentValuesMapBuilder[S, O] {
				val mapping = factory.mapping

				override def addOpt[T](view :WriteOperationView, component :TypedMapping[T, O], result :Opt[T]) :this.type =
					super.addOpt(mapping.export(component), result)

				override def addOpt[T](component :TypedMapping[T, O], result :Opt[T]) :this.type =
					if (result.isDefined)
						super.addOpt(mapping.export(component), result)
					else this

				protected override def result(map :Map[TypedMapping[_, O], Any]) =
					ComponentValues(mapping)(map.withDefaultValue(null))

				protected override def result(map :Map[TypedColumn[_, O], Any]) =
					ColumnValues(mapping)(map.withDefaultValue(null))
			}

	}



	trait ComponentValuesBuilder[S, O] {
		@inline final def add[T](view :WriteOperationView, component :TypedMapping[T, O], value :T) :this.type =
			addOpt(view, component, Got(value))

		def addOpt[T](view :WriteOperationView, component :TypedMapping[T, O], value :Opt[T]) :this.type = {
			view.writtenValues(component, value.get, this)
			this
		}

		@inline final def add[T](component :TypedMapping[T, O], value :T) :this.type =
			addOpt(component, Got(value))

		def addOpt[T](component :TypedMapping[T, O], result :Opt[T]) :this.type

		@inline final def add[T](column :TypedColumn[T, O], value :T) :this.type =
			addOpt(column, Got(value))

		//crashes the compiler with -Ytyper-debug
		def addOpt[T](column :TypedColumn[T, O], result :Opt[T]) :this.type =
			addOpt(column :TypedMapping[T, O], result)

		def result() :ComponentValues[S, O]
	}



	private[haul] class ComponentValuesMapBuilder[S, O] extends ComponentValuesBuilder[S, O] {
		private var map = Map.empty[TypedMapping[_, O], Any]
		private var columnsOnly = true

		override def addOpt[T](component :TypedMapping[T, O], result :Opt[T]) :this.type = {
			if (result.isDefined) {
				map = map.updated(component, result.get)
				if (!component.isInstanceOf[TypedColumn[_, _]])
					columnsOnly = false
			}
			this
		}

		override def result() = {
			val res = map; map = null
			if (columnsOnly) result(res.asInstanceOf[Map[TypedColumn[_, O], Any]])
			else result(res)
		}

		protected def result(map :Map[TypedMapping[_, O], Any]) :ComponentValues[S, O] =
			new ComponentValuesMap(map.withDefaultValue(null))

		protected def result(map :Map[TypedColumn[_, O], Any]) :ColumnValues[S, O] =
			ColumnValues(map.withDefaultValue(null))
	}



	/** A mix-in trait implementing `clone()` by simply returning itself. */
	private[haul] trait ImmutableComponentValues[S, O] extends ComponentValues[S, O] {
		override def clone() :ComponentValues[S, O] = this
	}



	/** Base trait for `ComponentValues` implementations which always pass themselves (after casting) to all
	  * components of the associated mapping.
	  */
	trait SharedComponentValues[S, O] extends ComponentValues[S, O] { outer =>

		override def /[T](extract :Extract[T]) :ComponentValues[T, O] = asComponentsOf[T]

		override def /[T](component :Component[T]) :ComponentValues[T, O] = asComponentsOf[T]

		override def /[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: Component[T], S, T, O]) :ComponentValues[T, O] =
			asComponentsOf[T]
	}



	/** Optimisation of `ComponentValues` which introduces a shortcut path for value lookup if the component
	  * is a [[net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn SimpleColumn]], which uses always the default
	  * implementations of [[net.noresttherein.oldsql.schema.Mapping.optionally, optionally]] and
	  * [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]]. In that case, instead of going through
	  * the process of creating a new instance for the column (or casting itself) and invoking `optionally`,
	  * it straight calls `defined` method on itself and returns the result without passing over control to the column.
	  * Method [[net.noresttherein.oldsql.haul.ComponentValues.preset preset]] is not used for this purpose as
	  * implementations may still opt to have preset values for components other than columns.
	  */ //todo: actually use it once we are sure that SimpleColumn can in fact move all buff handling to its form
	trait SimpleComponentValues[S, O] extends ComponentValues[S, O] {
		protected def defined[T](extract :ColumnExtract[T]) :Opt[T] = defined(extract.export)
		protected def defined[T](column :TypedColumn[T, O]) :Opt[T]

		override def apply[T](component :Component[T]) :T = component match {
			case column :SimpleColumn[T @unchecked, O @unchecked] => defined(column).get
			case _ => component(this.asComponentsOf[T])
		}

		override def get[T](component :Component[T]) :Opt[T] = component match {
			case column :SimpleColumn[T @unchecked, O @unchecked] => defined(column)
			case _ => component.optionally(this / component)
		}

		override def get[T](extract :Extract[T]) :Opt[T] = extract.export match {
			case _ :SimpleColumn[T @unchecked, O @unchecked] => defined(extract.asInstanceOf[ColumnExtract[T]])
			case component => component.optionally(this / component)
		}
	}




	private[haul] def aliasing[T, O](extracts :NaturalMap[MappingAt[O]#Component, TypedMapping[T, O]#Extract])
			:MappingAt[O]#Component =>: MappingAt[O]#Component =
		new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[X](x :TypedMapping[X, O]) :TypedMapping[X, O] = extracts(x).export
		}

	private[haul] def aliasing[O](root :MappingAt[O]) :MappingAt[O]#Component =>: MappingAt[O]#Component =
		new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[T](x :TypedMapping[T, O]) = root.export(x)
		}

	private[haul] def selectiveAliasing[T, O](extracts :NaturalMap[MappingAt[O]#Component, TypedMapping[T, O]#Extract])
			:MappingAt[O]#Component =>: MappingAt[O]#Component =
		new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[X](x :TypedMapping[X, O]) :TypedMapping[X, O] = {
				val extract = extracts.getOrElse[TypedMapping[T, O]#Extract, X](x, null)
				if (extract != null) extract.export else x
			}
		}

	private[haul] def selectiveAliasing[O]
	                  (export :MappingAt[O]#Component =>: ({ type E[X] = Option[TypedMapping[X, O]] })#E) =
		new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[T](x :TypedMapping[T, O]) = export(x) match {
				case Some(e) => e
				case _ => x
			}
		}

	private[haul] def selectiveAliasing[O](root :MappingAt[O]) :MappingAt[O]#Component =>: MappingAt[O]#Component =
		new (MappingAt[O]#Component =>: MappingAt[O]#Component) {
			override def apply[T](x :TypedMapping[T, O]) = root.exportOrNot(x)
		}



	/** A mix-in trait which maps every argument `Component[T]` into a possibly different `Component[T]`
	  * before delegating to the super method. This behaviour is limited only to the components of the associated
	  * mapping: the root mapping itself is not aliased this way. If it should be treated as other components,
	  * the `aliased` property should be used for assembly instead of this instance.
	  * This class is 'shared', meaning no new `ComponentValues` instances will be created for child components.
	  * This added functionality is used to substitute every component mapping used for assembly with its operative,
	  * ''export'' version as defined by the root mapping.
	  *
	  * In order to avoid calling, potentially not cheap, `alias` several times for the same component, this trait's
	  * `preset` and `assemble` do ''not'' alias their arguments. Instead, aliasing is performed by its sister inner
	  * class `aliasing`, serving an adapter to this instance. This type's `assemble` thus assumes that either
	  * its argument is already aliased, or doesn't need aliasing, and similarly `preset`, regardless if called directly
	  * or through `assemble`, performs the lookup for the argument as-is. However, when this lookup fails and
	  * the component's `assemble` is to be called, this type passes the `aliasing` adapter, which in turn
	  * calls `alias` for the argument in all methods, before delegating to this instance. As the result, the instance
	  * passed to the `optionally` method of every component is again the outer, this, instance, closing the cycle.
	  */
	trait ComponentValuesAliasing[S, O] extends SharedComponentValues[S, O] { outer =>
		//consider: this trait leaves room for optimisation for disassembling values which rely on a MappingExtract
		// for every passed component. Aliasing is typically performed by getting an extract for the component,
		// which is then discarded when passing to the component's assembling methods, only to be requested again
		// in order to get the preset value from the root subject. It would be very hard to change this without
		// making the mappings aware of this flow. One way would be to calculate the preset and return
		// a new ComponentValues instance, but this is incompatible with the premise of this implementation.
		// Profiling is needed to ascertain which is more costly: a HashMap look up or a simple object creation.

		/** Maps a component to its export version as per the [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method of the `Mapping` type.
		  */
		protected def alias[T](component :Component[T]) :Component[T]

		/** Aliases every component argument before executing the normal flow. The 'root' methods likewise
		  * alias the associated mapping of `S` and delegate to the corresponding method in the outer class,
		  * but they are not called in the normal flow unless this instance is passed to the root mapping
		  * instead of the enclosing class.
		  */ //todo: rename to ActualizingValues or smth
		protected class AliasingValues extends ComponentValues[S, O] {
//			override def assemble(root :TypedMapping[S, O]) :Opt[S] = outer.assemble(alias(root))
//			override def preset(root :TypedMapping[S, O]) :Opt[S] = outer.preset(alias(root))
//			override def optionally(root :TypedMapping[S, O]) :Opt[S] = alias(root).optionally(outer)
//			override def subject(root :TypedMapping[S, O]) :S = outer.subject(alias(root))
			override def assemble(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
				case null => Lack
				case export => outer.assemble(export)
			}
			override def preset(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
				case null => Lack
				case export => outer.preset(export)
			}
			override def optionally(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
				case null => Lack
				case export => export.optionally(outer)
			}
			override def subject(root :TypedMapping[S, O]) :S = alias(root) match {
				case null => throw new NoSuchComponentException("No export version of component " + root)
				case export => outer.subject(export)
			}

			override def get[T](component :Component[T]) :Opt[T] = alias(component) match {
				case null => Lack
				case export => export.optionally(outer.asComponentsOf[T])
			}

			override def get[T](extract :Extract[T]) :Opt[T] =
				extract.export.optionally(outer.asComponentsOf[T])

			override def /[T](component :Component[T]) :ComponentValues[T, O] = outer.asComponentsOf[T]

			override def aliased(export :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValues[S, O] =
				outer.aliased(export).aliasing

			override def canEqual(that :Any) :Boolean = that match { //isInstanceOf angered the compiler
				case _ :ComponentValuesAliasing[_, _]#AliasingValues => true
				case _ => false
			}

			private def values = outer

			override def equals(that :Any) :Boolean = that match {
				case self :AnyRef if self eq this => true
				case aliasing :ComponentValuesAliasing[_, _]#AliasingValues => outer == aliasing.values
				case _ => false
			}

			override def hashCode :Int = outer.hashCode * 31 + getClass.hashCode

			override def toString :String = "'Aliasing'" + outer
		}

		/** Proxy `ComponentValues` which perform the aliasing of components before delegating to the enclosing
		  * class. It is passed from the `assemble` method of the enclosing class to the `assemble` method of
		  * the associated mapping. Having two instances, each passing over the flow to the other, enables the aliasing
		  * to be done only once, instead of performing it both in the 'root mapping' methods (`assemble`, `preset`)
		  * and 'component' methods (`apply`, `get`), while still ensuring that the aliasing is performed at all
		  * levels of the component hierarchy.
		  */
		val aliasing :ComponentValues[S, O] = new AliasingValues

		override def assemble(root :TypedMapping[S, O]) :Opt[S] = {
			val preset = this.preset(root)
			if (preset.isDefined) preset
			else root.assemble(aliasing)
		}

		/** Aliases the component before passing it to the super method, which will typically be that of
		  * the implementation class to which this trait is mixed in. It is largely irrelevant though,
		  * as in the normal flow this method never gets called - it is always the `AliasingValues` that get
		  * passed to a mapping's `assemble` method.
		  */
		override def get[T](component :Component[T]) :Opt[T] = alias(component) match {
			case null => Lack
			case export => super.get(export)
		}

		/** Aliases the component before passing it to the super method, which will typically be that of
		  * the implementation class to which this trait is mixed in. It is largely irrelevant though,
		  * as in the normal flow this method never gets called - it is always the `AliasingValues` that get
		  * passed to a mapping's `assemble` method.
		  */ //alternatively, we could recreate the extract with the aliased component
		override def get[T](extract :Extract[T]) :Opt[T] = {
			val comp = extract.export
			alias(comp) match {
				case null => Lack
				case same if same eq comp => super.get(extract)
				case export => super.get(export)
			}
		}

		override def aliased(export :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValuesAliasing[S, O] =
			new ComponentValuesAliasing[S, O] {
				protected override def alias[T](component :Component[T]) = outer.alias(export(component))
				override def preset(root :TypedMapping[S, O]) = outer.preset(root)
				override def aliased(again :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValuesAliasing[S, O] =
					outer.aliased(again andThen export)
				override def toString = outer.toString + ".aliased"
			}

		//this should somehow check for equality or void super.equals if the argument isn't aliased
		override def toString :String = "~" + super.toString
	}



	/** A mix-in trait which substitutes every argument `Component[T]` with its ''export'' version, as defined by
	  * the root mapping, before delegating to the super method. This behaviour is limited only to the components
	  * of the root mapping: it is not aliased itself in this way. If it should be treated as other components,
	  * the `aliased` property should be used for assembly instead of this instance.
	  * This class is sticky, meaning it returns itself whenever `ComponentValues` for a child component is requested.
	  */
	trait MappingAliasing[S, O] extends ComponentValuesAliasing[S, O] {
		protected val mapping :MappingAt[O]

		protected override def alias[T](component :Component[T]) :Component[T] =
			mapping.export(component)

		override def canEqual(that :Any) :Boolean =
			that.isInstanceOf[MappingAliasing[_, _]] && super.canEqual(that)

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case vals :MappingAliasing[_, _] if vals canEqual this => mapping == vals.mapping && super.equals(that)
			case _ => false
		}
	}



	/** A decorator `ComponentValues` which substitutes every mapping passed as an argument, be it the associated
	  * mapping for `S` or its component, with possibly another mapping instance provided by the given mapping
	  * function. This is needed in order to always use the operative, ''export'' version of the component
	  * (with possibly modified column names and buffs) for the assembly. The decorator is sticky, reapplying
	  * itself if needed to every `ComponentValues` returned for a component.
	  */
	private[haul] class AliasedComponentValues[S, O]
	                    (values :ComponentValues[S, O], alias :MappingAt[O]#Component =>: TypedMapping[_, O]#Component)
		extends ComponentValues[S, O] with Serializable
	{ outer =>
		def this(values :ComponentValues[S, O],
		         extracts :NaturalMap[MappingAt[O]#Component, TypedMapping[_, O]#Extract]) =
			this(values, aliasing(extracts))

		def this(values :ComponentValues[S, O], root :TypedMapping[_, O]) =
			this(values, aliasing(root))

		protected def aliases = alias
		protected def contents :ComponentValues[S, O] = values

		private[this] val nonAliasingValues = new ComponentValues[S, O] {
			override def preset(root :TypedMapping[S, O]) = values.preset(root)
			override def /[T](component :Component[T]) :ComponentValues[T, O] = outer / component
		}

		protected def noAliasing :ComponentValues[S, O] = nonAliasingValues


		override def assemble(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
			case null => Lack
			case aliased =>
				val preset = values.preset(aliased)
				if (preset.isDefined) preset
				else root.assemble(this)
		}

		override def preset(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
			case null => Lack
			case aliased => values.preset(aliased)
		}

		override def optionally(root :TypedMapping[S, O]) :Opt[S] = alias(root) match {
			case null => Lack
			case aliased => aliased.optionally(noAliasing)
		}

		override def subject(root :TypedMapping[S, O]) :S = alias(root) match {
			case null => throw new NoSuchComponentException(s"No export version of component $root in $this.")
			case aliased => aliased(noAliasing)
		}


		override def get[T](component :Component[T]) :Opt[T] = alias(component) match {
			case null => Lack
			case export =>
				val next = values / export
				if (next eq values) export.optionally(noAliasing.asComponentsOf[T])
				else export.optionally(new AliasedComponentValues[T, O](next, alias).noAliasing)
		}

		//caution: we assume here that the extract is the 'exporting' extract - that its component is the export version
		override def get[T](extract :Extract[T]) :Opt[T] = get(extract.export)

		override def /[T](component :Component[T]) :ComponentValues[T, O] = alias(component) match {
			case null => empty("No export version of component " + component + " in " + this)
			case export =>
				val next = values / export
				if (next eq values) noAliasing.asComponentsOf[T]
				else new AliasedComponentValues[T, O](next, alias)
		}

		override def ++(other :ComponentValues[S, O]) :ComponentValues[S, O] = other match {
			//we check only reference identity for speed; aliases will almost always be either one of the constants
			//defined here or a NaturalMap - most likely a constant, too.
			case aliased :AliasedComponentValues[S @unchecked, O @unchecked] if aliases eq aliased.aliases =>
				new AliasedComponentValues(values ++ aliased.contents, aliases)
			case _ => super.++(other)
		}

		override def aliased(export :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValues[S, O] =
			new AliasedComponentValues(values, alias andThen export)


		override def clone() :ComponentValues[S, O] = {
			val clone = values.clone()
			if (clone eq values) this
			else new AliasedComponentValues(clone, alias)
		}


		override def canEqual(other: Any): Boolean = other.getClass == getClass

		override def equals(other: Any): Boolean = other match {
			case self :AnyRef if self eq this => true
			case that: AliasedComponentValues[_, _] if that canEqual this =>
				aliases == that.aliases && contents == that.contents
			case _ => false
		}

		override def hashCode(): Int = aliases.hashCode * 31 + contents.hashCode

		override def toString :String = "~{" + values.toString + "}"
	}




	/** Implementation of the `orElse` result, using a pair of `ComponentValues` instances, with the second one
	  * being used if the first failed to provide a value for a component. Whenever `ComponentValues`
	  * for a child component is requested, it returns the `orElse` result for the values corresponding to the component
	  * obtained from the pair.
	  */
	private[haul] class FallbackValues[S, O](overrides :ComponentValues[S, O], fallback :ComponentValues[S, O])
		extends ComponentValues[S, O] with Serializable
	{
		override def preset(root: TypedMapping[S, O]): Opt[S] = overrides.preset(root)

		override def assemble(root: TypedMapping[S, O]): Opt[S] = {
			val res = overrides.preset(root)
			if (res.isDefined) res
			else {
				val res = root.assemble(this)
				if (res.isDefined) res
				else fallback.preset(root)
			}
		}

		protected def decorate[T](first :ComponentValues[T, O], second :ComponentValues[T, O]) :ComponentValues[T, O] =
			if ((first eq overrides) & (second eq fallback)) asComponentsOf[T]
			else new FallbackValues[T, O](first, second)

		override def /[T](extract :Extract[T]) :ComponentValues[T, O] =
			decorate(overrides / extract, fallback / extract)

		override def /[T](component :Component[T]) :ComponentValues[T, O] =
			decorate(overrides / component, fallback / component)

		override def clone() :ComponentValues[S, O] =
			decorate(overrides.clone(), fallback.clone())

		private def first = overrides
		private def second = fallback

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :FallbackValues[_, _] if other canEqual this =>
				first == other.first && second == other.second
			case _ => false
		}

		override def hashCode :Int = first.hashCode * 31 + second.hashCode

		override def toString = s"$overrides orElse $fallback"
	}




	/** A `ComponentValues` implementation backed by a `NaturalMap` associating components with their preset
	  * `ComponentValues`. This instance is conditionally shared, meaning it simply returns itself
	  * as the `ComponentValues` for all child components for which no instance has been preset.
	  * @param values a mapping from components of the root mapping to the `ComponentValues` which should be used
	  *               for their assembly. May not cover all components.
	  * @param default a fallback instance used when no value for a component is predefined or assembled.
	  */
	private[haul] class DedicatedComponentValues[S, O] private[haul]
	                    (values :NaturalMap[MappingAt[O]#Component, WithOrigin[O]#T],
	                     default :ComponentValues[S, O] = empty[S, O])
		extends ComponentValues[S, O] with Serializable
	{
		def presets = values
		def defaults = default

		override def preset(root: TypedMapping[S, O]): Opt[S] = {
			val res = values.getOrElse[WithOrigin[O]#T, S](root, null)
			if (res == null) Lack else res.preset(root)
		}

		override def assemble(root: TypedMapping[S, O]): Opt[S] = {
			val res = values.getOrElse[WithOrigin[O]#T, S](root, null)
			if (res == null) Lack else res.assemble(root)
		}


		override def get[T](extract :Extract[T]) :Opt[T] = {
			val vals = values.getOrElse[WithOrigin[O]#T, T](extract.export, null)
			if (vals == null) default.get(extract)
			else vals.optionally(extract.export) orElse default.get(extract)
		}

		override def get[T](component :Component[T]) :Opt[T] = {
			val res = values.getOrElse[WithOrigin[O]#T, T](component, null)
			if (res == null) default.get(component)
			else res.optionally(component) orElse default.get(component)
		}

		override def /[T](component :Component[T]) :ComponentValues[T, O] =
			values.getOrElse[WithOrigin[O]#T, T](component, null.asInstanceOf[ComponentValues[T, O]]) match {
				case null => asComponentsOf[T]
				case vals => vals
			}


//		override def orElse(alt :ComponentValues[S, O]) :ComponentValues[S, O] =
//			new DedicatedComponentValues(values, default orElse alt)

		override def ++(other :ComponentValues[S, O]) :ComponentValues[S, O] = other match {
			case custom :DedicatedComponentValues[S @unchecked, O @unchecked] =>
				new DedicatedComponentValues[S, O](values ++ custom.presets, default ++ custom.defaults)
			case singleton :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (singleton.value.isDefined)
					new DedicatedComponentValues[S, O](
						values.updated[WithOrigin[O]#T, t](singleton.column, singleton.asColumnsOf[t]), default
					)
				else
					this
			case _ => super.++(other)
		}


		override def aliased(export :MappingAt[O]#Component =>: MappingAt[O]#Component) :ComponentValues[S, O] =
			new DedicatedComponentValues[S, O](values, default aliased export)

		override def tiedTo[X](mapping :TypedMapping[S, O]) :ComponentValues[X, O] = asComponentsOf[X]


		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DedicatedComponentValues[_, _] if other canEqual this =>
				presets == other.presets && defaults == other.defaults
			case _ => false
		}

		override def hashCode :Int = presets.hashCode * 31 + defaults.hashCode

		override def toString :String = values.to(Array).map(v => v._1.toString + "->" + v._2).mkString("{", ",", "}")
	}




	private[haul] class LazyComponentValuesProxy[S, O](values: => ComponentValues[S, O]) extends ComponentValues[S, O] {
		private[this] var init = () => values
		@volatile private[this] var initialized :ComponentValues[S, O] = _
		private[this] var cache :ComponentValues[S, O] = _

		protected def backing :ComponentValues[S, O] = {
			if (cache == null) {
				val vs = initialized
				if (vs == null) {
					val i = init
					if (i != null) {
						cache = i()
						initialized = cache
						init = null
					}
				}
			}
			cache
		}

		override def assemble(root :TypedMapping[S, O]) :Opt[S] = backing.assemble(root)
		override def preset(root :TypedMapping[S, O]) :Opt[S] = backing.preset(root)

		override def optionally(root :TypedMapping[S, O]) :Opt[S] = backing.optionally(root)
		override def subject(mapping :TypedMapping[S, O]) :S = backing.subject(mapping)

		override def apply[T](extract :Extract[T]) :T = backing(extract)
		override def apply[T](component :Component[T]) :T = backing(component)

		override def get[T](extract :Extract[T]) :Opt[T] = backing.get(extract)
		override def get[T](component :Component[T]) :Opt[T] = backing.get(component)

		override def /[T](extract :Extract[T]) :ComponentValues[T, O] = backing / extract
		override def /[T](component :Component[T]) :ComponentValues[T, O] = backing / component

		override def orElse(values :ComponentValues[S, O]) :ComponentValues[S, O] = backing orElse values
		override def ++(values :ComponentValues[S, O]) :ComponentValues[S, O] = backing ++ values


		protected def writeReplace = backing

		override def toString :String = backing.toString
	}




	private[haul] class DisassembledValues[R, S, O](root :TypedMapping[R, O], subject :R)
		extends SharedComponentValues[S, O] with ImmutableComponentValues[S, O] with Serializable
	{
		private def mapping = root
		private def value = subject

		override def preset(mapping :TypedMapping[S, O]) =
			if (mapping == root) Got(subject.asInstanceOf[S])
			else if (root.contains(mapping)) root(mapping).opt(subject)
			else Lack

		override def canEqual(that :Any) :Boolean = that.getClass == getClass

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :DisassembledValues[_, _, _] if other canEqual this =>
				mapping == other.mapping && value == other.value
			case _ => false
		}

		override def hashCode :Int = mapping.hashCode * 31 + subject.hashCode

		override def toString = "DisassembledValues(" + subject + ")"
	}



	private[haul] class LazyDisassembledValues[R, S, O](root :TypedMapping[R, O], private var init: () => Option[R])
		extends SharedComponentValues[S, O]
	{
		@volatile private[this] var result :Option[R] = _
		private[this] var cached :Option[R] = _

		private def subject :Opt[R] = {
			if (cached == null) {
				val res = result
				if (res != null)
					cached = res
				else {
					val cons = init
					if (cons != null) {
						cached = cons()
						result = cached
						init = null
					}
				}
			}
			cached
		}

		override def preset(mapping :TypedMapping[S, O]) =
			if (mapping == root) subject.asInstanceOf[Opt[S]]
			else root(mapping).opt(subject.get)


		override def clone() :ComponentValues[S, O] = subject match {
			case Got(value) => new DisassembledValues[R, S, O](root, value)
			case _ => empty
		}

		private def writeReplace :ComponentValues[S, O] = subject match {
			case Got(value) => new DisassembledValues[R, S, O](root, value)
			case _ => empty
		}

		override def toString = {
			if (cached == null) {
				val res = result
				if (res != null)
					cached = res
			}
			cached match {
				case _ if cached == null => "DisassembledValues(?)"
				case Some(value) => "DisassembledValues(" + value + ")"
				case _ => publicClassName + "()"
			}
		}
	}



	private[haul] class ChosenDisassembledValues[R, S, O]
	                    (root :TypedMapping[R, O], value :R, components :Unique[MappingAt[O]])
		extends DisassembledValues[R, S, O](root, value) with Serializable
	{
		override def preset(mapping :TypedMapping[S, O]) :Opt[S] =
			if (mapping == root) Got(value.asInstanceOf[S])
			else {
				val extract = root(mapping)
				if (components.contains(extract.export)) extract.opt(value)
				else Lack
			}

		override def toString :String = components.mkString(publicClassName + "(" + value + ": ", ", ", ")")
	}



	private[haul] class TypedValues[S, O](values :MappingAt[O]#Component =>: Self)
		extends SharedComponentValues[S, O] with ImmutableComponentValues[S, O] with Serializable
	{
		override def preset(component :TypedMapping[S, O]) :Opt[S] =
			try { Opt(values[S](component)) } catch {
				case _ :NoSuchElementException => Lack
			}

		override def canEqual(that :Any) = that.getClass == getClass

		private def fun = values

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :TypedValues[_, _] if other canEqual this => fun == other.fun
			case _ => false
		}

		override def hashCode :Int = values.hashCode

		override def toString :String = publicClassName + "(" + values + ")"
	}


	private[haul] class ComponentValuesNaturalMap[S, O](values :NaturalMap[MappingAt[O]#Component, Self])
		extends TypedValues[S, O](values) with Serializable
	{
		def map = values

		override def preset(component :TypedMapping[S, O]) :Opt[S] =
			Opt(values.getOrElse[Self, S](component, null.asInstanceOf[S]))

		override def ++(other :ComponentValues[S, O]) = other match {
			case same :ComponentValuesNaturalMap[S @unchecked, O @unchecked] =>
				new ComponentValuesNaturalMap(values ++ same.map)
			case single :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (single.value.isDefined)
					new ComponentValuesNaturalMap(values.updated[Self, t](single.column, single.value.get))
				else this
			case _ => super.++(other)
		}
	}


	private[haul] class UntypedValues[S, O](values :TypedMapping[_, O] => Any)
		extends SharedComponentValues[S, O] with ImmutableComponentValues[S, O] with Serializable
	{
		override def preset(component :TypedMapping[S, O]) :Opt[S] =
			Opt(values(component).asInstanceOf[S])

		private def fun = values

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :UntypedValues[_, _] if other canEqual this => fun == other.fun
			case _ => false
		}

		override def hashCode :Int = values.hashCode

		override def toString :String = publicClassName + "(" + values + ")"
	}


	private[haul] class ComponentValuesMap[S, O](values :Map[TypedMapping[_, O], Any])
		extends UntypedValues[S, O](values) with Serializable
	{
		def map = values

		override def preset(component :TypedMapping[S, O]) :Opt[S] =
			Opt(values.getOrElse(component, null).asInstanceOf[S])

		override def ++(other :ComponentValues[S, O]) = other match {
			case same :ComponentValuesMap[S @unchecked, O @unchecked] =>
				new UntypedValues(values ++ same.map)
			case single :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (single.value.isDefined)
					new ComponentValuesMap(values.updated(single.column, single.value))
				else this
			case _ =>
				super.++(other)
		}
	}



	private[haul] class IndexedValues[S, O](values :IndexedSeq[Any], index :MappingAt[O] => Int)
		extends SharedComponentValues[S, O] with ImmutableComponentValues[S, O] with Serializable
	{
		override def preset(component :TypedMapping[S, O]) :Opt[S] = {
			val i = index(component)
			if (i < 0) Lack
			else Opt(values(i)).castParam[S]
		}

		override def toString :String =
			values.map {
				case null => "_"
				case x => String.valueOf(x)
			}.mkString(publicClassName + "(", ", ", ")")


		private def seq = values
		private def idx = index

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :IndexedValues[_, _] if other canEqual this => seq == other.seq && idx == other.idx
			case _ => false
		}

		override def hashCode :Int = values.hashCode * 31 + index.hashCode
	}

}

