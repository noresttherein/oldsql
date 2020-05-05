package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.generic.=#>
import net.noresttherein.oldsql.schema.ComponentValues.{AliasedComponentValues, FallbackValues, StickyComponentValues}
import net.noresttherein.oldsql.schema.Mapping.{MappingFrom, TypedMapping}
import net.noresttherein.oldsql.schema.MappingPath.ComponentPath
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.slang._






/** Values for components of the given mapping together with possibly the value for the whole mapping,
  * used as input data for the assembly process defined by some mapping `TypedMapping[S, O]`.
  * It is created typically from row data of an SQL query to map tabular result into an object, but some implementations
  * work the other way round: they start with a subject value `S` and disassemble it into values for the components
  * of the associated mapping before saving an entity, abstracting over exactly which columns take part in a given
  * database operation. It abut can be also used to modify the values of specific components on an existing subject
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
  * (export) subcomponent of the associated mapping when asked directly. For example, when a mapping for class Person
  * containing an address component asks directly for the value of the 'street' column, instead of either specifying
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
  *
  *
  * @tparam S the subject type of the target mapping defining the components for which this instance contains values.
  * @tparam O the origin type of the target mapping and all components with values in this instance.
  */
trait ComponentValues[S, O] {

	/** Procure a value for the root mapping, either using a preset value or delegating to the mapping's assembly
	  * method. It is not the top-level function - use either `mapping.apply()` or  `this.root()` as the mapping
	  * should always have the last say on the result. This is implemented by a triple
	  * dispatch process - `mapping.apply()/optionally()` invokes this method and performs any validation or modification,
	  * such as providing a default value if none could be obtained. This method in turn will first check
	  * if `preset()` will return a result, and resorts to `mapping.assemble()` if nothing was set.
	  * @param mapping the root mapping associated with this instance. Note that ComponentValues should be always
	  *                statically typed for the singleton type of the associated (this) mapping and passing it to other
	  *                instances of the same mapping type is not supported in general. In fact, some implementations
	  *                store the associated mapping and are free to use it in implementation, and this argument should
	  *                be the exact same instance in that case.
	  */
	def assemble(mapping :TypedMapping[S, O]) :Option[S] = {
		val res = preset(mapping)
		if (res.isDefined) res
		else mapping.assemble(this)
	}

	/** Delegates to `mapping(this)` without modifying the result. Intended for caching/logging implementations. */
	def root(mapping :TypedMapping[S, O]) :S = mapping(this)

	/** Delegates to `mapping.optionally(this)` without modifying the result. intended for caching/logging implementations. */
	def optionally(mapping :TypedMapping[S, O]) :Option[S] = mapping.optionally(this)

	/** Is there a predefined - explicitly set on creation of this/parent `ComponentValues` instance -
	  * value for the top level mapping associated with this instance? If this method returns `Some(x)`,
	  * `assemble` will also return `Some(x)`. If it returns `None`, `assemble` will delegate to `root.assemble(this)`.
	  * @param root the mapping associated with this instance.
	  */
	def preset(root :TypedMapping[S, O]) :Option[S]



	/** Return the value for the given component. This method should always first adapt itself to the given component
	  * by creating `ComponentValues[T, O]` and passing it to `component.apply()` so it can inspect the
	  * returned value. Equivalent to `component(this \ component)`. If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar)(party.leader.familiar.name)`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](extract :MappingExtract[S, T, O]) :T =
		get(extract) getOrElse {
			throw new NoSuchElementException("No value for " + extract + " in " + this)
		}

	/** Return the value for the given component. This method should always first adapt itself to the given component
	  * by creating `ComponentValues[T, O]` and passing it to `component.apply()` so it can inspect the
	  * returned value. Equivalent to `component(this \ component)`. If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar)(party.leader.familiar.name)`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](component :TypedMapping[T, O]) :T =
		get(component) getOrElse {
			throw new NoSuchElementException("No value for " + component + " in " + this)
		}

	/** Return the value for the component at the end of the given path. This method will adapt itself in sequence
	  * to all components on the path, eventually creating a `ComponentValues[T, O]` and passing it to
	  * `component.apply()` so it can inspect the returned value. Equivalent to `component(this \ path)` and
	  * `component(this \ c1 \ ... \ cn \ path.end)`, where `c1 ... cn` are components on the path from the current
	  * mapping to the target component, that is a sequence of directly nested components ending with
	  * the desired component. If this instance is aliased, either explicitly, by a previous call to one
	  * of the `aliased` methods, or as an inherent feature, all components will be substituted with their operative
	  * versions before checking for preset values or calling their methods. This is generally done by some ancestor
	  * mapping's `export` method in order to both use the proper, possibly changed version from the point of view
	  * of the mapping and to allow the preset values be recognized regardless of the version of the component,
	  * by any component in the hierarchy. If this instance is not aliased, a component on the path must be
	  * the exact same instance (or equal to it) for which a value was preset (if any) in order for it to be picked up,
	  * otherwise the values will return `None`. If no value is found, `path.end` will be used to assemble it
	  * from the values of lower level components.
	  * @param path a path from the mapping associated with this instance to the component for the desired subject type.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: TypedMapping[T, O], S, T, O]) :T =
		get(path) getOrElse {
			throw new NoSuchElementException("No value for " + path + " in " + this)
		}



	/** Return the value for the given component or `None` if it can't be obtained in any way (preset, assembly or
	  * default). Should be equivalent to `component.optionally(this \ extract)`, rather than return a value directly
	  * without inspection by the component. If this instance is aliased, either explicitly by a previous call
	  * to one of the `aliased` methods, or as an inherent feature, the component will be substituted with its
	  * operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  */
	def get[T](extract :MappingExtract[S, T, O]) :Option[T] =
		\(extract).optionally(extract.export)

	/** Return the value for the given component or `None` if it can't be obtained in any way (preset, assembly or
	  * default). Should be equivalent to `component.optionally(this \ component)`, rather than return a value directly
	  * without inspection by the component. If this instance is aliased, either explicitly by a previous call
	  * to one of the `aliased` methods, or as an inherent feature, the component will be substituted with its
	  * operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  */
	def get[T](component :TypedMapping[T, O]) :Option[T] =
		\(component).optionally(component)

	/** Return the value for the component at the end of the given path or `None` if it can't be obtained in any way
	  * (preset, assembly or default). This method will adapt itself in sequence to all components on the path,
	  * eventually creating a `ComponentValues[T, O]` and passing it to `component.optionally()` so it can inspect
	  * the returned value. Equivalent to `component.optionally(this \ path)` and
	  * `component.optionally(this \ c1 \ ... \ cn \ path.end)`, where `c1 ... cn` are components on the path from
	  * the current mapping to the target component, that is a sequence of directly nested components ending
	  * with the desired component. If this instance is aliased, either explicitly, by a previous call to one of the
	  * `aliased` methods, or as an inherent feature, all components will be substituted with their operative versions
	  * before checking for preset values or calling their methods. This is generally done by some ancestor mapping's
	  * `export` method in order to both use the proper, possibly changed version from the point of view of the mapping
	  * and to allow the preset values be recognized regardless of the version of the component, by any component in the
	  * hierarchy. If this instance is not aliased, a component on the path must be the exact same instance
	  * (or equal to it) for which a value was preset (if any) in order for it to be picked up, otherwise the values
	  * will return `None`. If no value is found, `path.end` will be used to assemble it from the values
	  * of lower level components.
	  * @param path a path from the mapping associated with this instance to the component for the desired subject type.
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def get[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: TypedMapping[T, O], S, T, O]) :Option[T] =
		\(path).optionally(path.end)


	/** Return `ComponentValues` for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping). If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param extract the extract for the required component of the associated mapping.
	  */
	def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] =
		\(extract.export)

	/** Return `ComponentValues` for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping). If this instance is aliased, either explicitly,
	  * by a previous call to one of the `aliased` methods, or as an inherent feature, the component will be substituted
	  * with its operative version before checking for preset values or calling its methods. This is generally done
	  * by some ancestor mapping's `export` method in order to both use the proper, possibly changed version
	  * from the point of view of the mapping and to allow the preset values be recognized regardless of the
	  * version of the component, by any component in the hierarchy. If this instance is not aliased,
	  * the component argument must be the same instance as the one for which the value was preset (or equal to it)
	  * for it to be picked up, otherwise the values will return `None`. Note that any component of the mapping
	  * to which this instance is dedicated is a valid argument, not only its direct components. Requesting a value
	  * for a non-direct subcomponent however runs the risk of an incorrect or missed value in case when a value has
	  * been preset for its ancestor mapping being a component of the associated mapping of `S`. Typically, when
	  * accessing a direct component, the preset value would be found and later disassembled into values
	  * for its subcomponents if required. If that component were to be skipped and a value requested for its
	  * subcomponent directly, the preset value would in most cases not be picked up. This is not an issue in
	  * the default use case of assembling SQL query results as the values are preset only for the bottom-most column
	  * components, but in order to be fully compatible, a mapping should either extract manually the required value
	  * from the subject of its direct component containing it, chain `\` calls for all components on the path to
	  * the subcomponent as in `(this \ party.leader \ party.leader.familiar) get party.leader.familiar.name`,
	  * or use the overloaded variant of this method accepting a `MappingPath` which would do it automatically.
	  * @param component a component of the associated mapping.
	  */
	def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O]

	/** Return `ComponentValues` for the component at the end of the given path. Equivalent to
	  * `this \ c1 \ ... \ cn \ path.end`, where `c1 ... cn` are components on the path from
	  * the current mapping to the target component, that is a sequence of directly nested components ending
	  * with the desired component. If this instance is aliased, either explicitly, by a previous call to one of the
	  * `aliased` methods, or as an inherent feature, all components will be substituted with their operative versions
	  * before checking for preset values or calling their methods. This is generally done by some ancestor mapping's
	  * `export` method in order to both use the proper, possibly changed version from the point of view of the mapping
	  * and to allow the preset values be recognized regardless of the version of the component, by any component in the
	  * hierarchy. If this instance is not aliased, a component on the path must be the exact same instance
	  * (or equal to it) for which a value was preset (if any) in order for it to be picked up, otherwise the values
	  * will return `None`.
	  * @param path the path from the current mapping to the desired component.
	  */
	def \[T](path :ComponentPath[_ <: TypedMapping[S, O], _ <: TypedMapping[T, O], S, T, O]) :ComponentValues[T, O] =
		path.carry(this)



	/** Crate proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * `MappingExtract` and `MappingPath` instances) by passing them to the given mapping's `export` method
	  * before passing the result to the corresponding method of this instance.
	  * The proxy is sticky: descending down the component tree with `\` maintains the wrapping of all
	  * created `ComponentValues`. Methods which combine or change the behaviour of `ComponentValues`
	  * such as `this orElse that` however work as for every other instance, with `that` not becoming aliased
	  * as the result. Subsequent calls result in function composition, with the most recent one providing
	  * the most nested function. This step is required as all mappings, which allow embedding ready mapping instances
	  * as components and changing some of their attributes by adapting them, end up with several versions
	  * of the component for the same subject: the original one, the operative version exposed on the component lists
	  * of the root mapping and possibly any adapters created by the chain of enclosing subcomponents leading
	  * to the component in question. It is the operative versions which should be used for assembly
	  * and for which values are preset, the actual assembly code is usually defined by the intermediate components
	  * in terms of the component as created or adapted by them. Aliasing causes all mappings in the hierarchy
	  * to use the same, operative or 'export', version as made public by the root mapping.
	  */
	def aliased(root :MappingFrom[O]) :ComponentValues[S, O] = aliased(ComponentValues.aliasing(root))

	/** Crate proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * `MappingExtract` and `MappingPath` instances as well as the root mapping to which this instance is dedicated)
	  * by substituting all components with the `export` property of the `MappingExtract` associated with it
	  * in the given map. If the map doesn't have an entry for a mapping (in particular the root mapping),
	  * a `NoSuchElementException` will be thrown. The proxy is sticky: descending down the component tree with `\`
	  * maintains the wrapping of all created `ComponentValues`. Methods which combine or change the behaviour
	  * of `ComponentValues` such as `this orElse that` however work as for every other instance, with `that`
	  * not becoming aliased as the result. Subsequent calls result in function composition, with the most recent one
	  * providing the most nested function. This step is required as all mappings, which allow embedding
	  * ready mapping instances as components and changing some of their attributes by adapting them, end up
	  * with several versions of the component for the same subject: the original one, the operative version exposed
	  * on the component lists of the root mapping and possibly any adapters created by the chain
	  * of enclosing subcomponents leading to the component in question. It is the operative versions
	  * which should be used for assembly and for which values are preset, the actual assembly code is usually defined
	  * by the intermediate components in terms of the component as created or adapted by them. Aliasing causes
	  * all mappings in the hierarchy to use the same, operative or 'export', version as made public by the root mapping.
	  */
	def aliased(extracts :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Extract]) :ComponentValues[S, O] =
		aliased(ComponentValues.aliasing(extracts))

	/** Crate proxy `ComponentValues` which will perform aliasing of all component arguments (including those in
	  * `MappingExtract` and `MappingPath` instances as well as the root mapping to which this instance is dedicated)
	  * by applying the given function and passing the returned component to the corresponding method of this instance.
	  * The proxy is sticky: descending down the component tree with `\` maintains the wrapping of all
	  * created `ComponentValues`. Methods which combine or change the behaviour of `ComponentValues`
	  * such as `this orElse that` however work as for every other instance, with `that` not becoming aliased
	  * as the result. Subsequent calls result in function composition, with the most recent one providing
	  * the most nested function. This step is required as all mappings, which allow embedding ready mapping instances
	  * as components and changing some of their attributes by adapting them, end up with several versions
	  * of the component for the same subject: the original one, the operative version exposed on the component lists
	  * of the root mapping and possibly any adapters created by the chain of enclosing subcomponents leading
	  * to the component in question. It is the operative versions which should be used for assembly
	  * and for which values are preset, the actual assembly code is usually defined by the intermediate components
	  * in terms of the component as created or adapted by them. Aliasing causes all mappings in the hierarchy
	  * to use the same, operative or 'export', version as made public by the root mapping.
	  */
	def aliased(export :MappingFrom[O]#Component =#> MappingFrom[O]#Component) :ComponentValues[S, O] =
		new AliasedComponentValues[S, O](this, export)



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
	def orElse(values :ComponentValues[S, O]) :ComponentValues[S, O] =
		new FallbackValues[S, O](this, values)



	/** Create `ComponentValues` attached to the given mapping, which will remain unchanged for the whole assembly
	  * (ignoring `\`). Each time a value/values are requested for a mapping/component, it will check
	  * if the argument is equal to the mapping given here and, if so, return this instance, otherwise itself (empty).
	  * This is useful when one wants to preset component values for a component further down the mapping hierarchy,
	  * without knowing its top-level 'export' representation. Can be combined with `orElse` to provide preset values
	  * for selected subcomponents and fallback to default algorithm for all other.
	  */
	def stickTo[X](mapping :TypedMapping[S, O]) :ComponentValues[X, O] = {
		val assoc = NaturalMap.single[MappingFrom[O]#Component, ComponentValues.Origin[O]#T, S](mapping, this)
		new StickyComponentValues[S, O](assoc).crosscast[X]
	}

	/** Create `ComponentValues` which will always first check if their argument is the mapping provided here and,
	  * if so, will return the value that this instance would return. For all other mappings default rules apply.
	  * This allows to implement symlink like behaviour between components: if the mapping was passed as-is for use by
	  * some subcomponents of a shared owner ''M'', more than one 'copy' of target component
	  * could occur on mapping M's component list, originating from exporting different uses of the target mapping.
	  * They would not be equal to each other however, due to different exporting/mapping applied by mappings on the
	  * path to their use. By calling `values.stick(mapping) \ component` we make sure that whenever
	  * any mapping further down the path requests for the value of this component, it will delegate directly to
	  * values specific to the path of original definition.
	  */
	def stick[T](component :TypedMapping[T, O]) :ComponentValues[S, O] = {
		val assoc = NaturalMap.single[MappingFrom[O]#Component, ComponentValues.Origin[O]#T, T](
			component, this \ component
		)
		new StickyComponentValues[S, O](assoc, this)
	}


	/** Shortcut for casting the type parameter to a different type reserved for the implementations */
	@inline final private[ComponentValues] def crosscast[X] :ComponentValues[X, O] =
		this.asInstanceOf[ComponentValues[X, O]]



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
  */
object ComponentValues {
	type Origin[O] = { type T[S] = ComponentValues[S, O] }



	/** Returns `ComponentValues` using the given generic function as the source of values for components.
	  * The return values of the function are used as the preset values yielded by the created instance,
	  * with `None` resulting in the value of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
	  * function - if it is `None` it may result in an assembly failure. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs and thus it should be taken care of by the function itself.
	  * You may wish to consider using `ComponentValues(mapping)(values)` instead, which is compatible with any mapping.
	  * You can supply a `NaturalMap` as an argument.
	  * @param values factory of values for components.
	  */
	def apply[S, O](values :MappingFrom[O]#Component =#> Option) :ComponentValues[S, O] =
		new CustomValues[S, O](values)


	/** Returns `ComponentValues` using the given function as the source of values for components.
	  * The return values of the function are used as the preset values yielded by the created instance,
	  * with `None` resulting in the value of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
	  * function - if it is `None` it may result in an assembly failure. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs and thus it should be taken care of by the function itself.
	  * You may wish to consider using `ComponentValues(mapping)(values)` instead, which is compatible with any mapping.
	  * You can supply a `Map` as an argument.
	  * @param values factory of values for components, must return an option of the same type as the input mapping..
	  */
	def apply[S, O](values :TypedMapping[_, O] => Option[_]) :ComponentValues[S, O] =
		new UntypedValues(values)


	/** Returns `ComponentValues` wrapping the given set of values with their assignment to components happening
	  * by the use of the provided indexing function. An entry of `None` or a negative index results in the
	  * subject of the component being assembled from its subcomponents.
	  * Preset values are never disassembled into values for subcomponents: providing a value for a component
	  * other than a column (instead of its columns) requires that no other component, including its owner,
	  * accesses any of its subcomponents directly, as it would yield a value only if explicitly provided here.
	  * Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs.
	  * @param values preset values for selected components of the supplied mapping, in any order consistent
	  *               with `index`.
	  * @param index A function returning the index of the value for the component in the given sequence.
	  *              If the component has no preset value, it should return a negative number.
	  */
	def apply[S, O](values :IndexedSeq[Option[Any]], index :MappingFrom[O] => Int) :ComponentValues[S, O] =
		new IndexedValues(values, index)


	/** Creates a `ComponentValues` instance with a given preset for the specified mapping. If non-empty,
	  * the values for subcomponents will be extracted from it using the `MappingExtract`s supplied by the mapping.
	  * This is equivalent to `ComponentValues.Preset(mapping, result.get)` or `ComponentValues.Empty`, based
	  * on whether the `result` is defined.
	  */
	def apply[S, O](mapping :TypedMapping[S, O], result :Option[S]) :ComponentValues[S, O] = result match {
		case some :Some[S] => new OverrideMappingValue[S, O](mapping, some)
		case _ => Empty[S, O]
	}



	/** An empty instance, returning always `None` or throwing `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ComponentValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  */
	def Empty[S, O] :ComponentValues[S, O] = empty.asInstanceOf[ComponentValues[S, O]]

	/** An empty instance, returning always `None` or throwing a `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ComponentValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  * @param source a description of the original source which created this instance for more helpful message in
	  *               thrown exceptions
	  */
	def Empty[S, O](source : => String) :ComponentValues[S, O] = new EmptyValues[S, O](source)



	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * The values for the subcomponents of the mapping are ''always'' extracted from the preset value using
	  * the mapping's `MappingExtract` for the component; the components' `optionally` (or `apply` and `assemble`)
	  * methods are never called and neither are they being aliased to their operative version provided by the extract.
	  * The root mapping's `optionally`/`apply` method is being called as normal, allowing it to modify the preset result.
	  * You may wish to consider using `ComponentValues(mapping)(value)` instead, which follows exactly the general
	  * `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def Preset[S, O](mapping :TypedMapping[S, O], value :S) :ComponentValues[S, O] =
		new OverrideMappingValue[S, O](mapping, Some(value))

	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * The values for the subcomponents of the mapping are ''always'' extracted from the preset value using
	  * the mapping's `MappingExtract` for the component; the components' `optionally` (or `apply` and `assemble`)
	  * methods are never called and neither are they being aliased to their operative version provided by the extract.
	  * The root mapping's `optionally`/`apply` method is being called as normal, allowing it to modify the preset result.
	  * You may wish to consider using `ComponentValues(mapping)(value)` instead, which follows exactly the general
	  * `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def Preset[S, O](mapping :TypedMapping[S, O], value :Some[S]) :ComponentValues[S, O] =
		new OverrideMappingValue[S, O](mapping, value)

	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by their owning mapping to extract the value from the given argument.
	  * Unlike `Preset(mapping, value)`, only the components listed in the third argument will have their value preset,
	  * with the others being assembled from their subcomponents. Note that while you can provide a value
	  * for any component, not only columns, if any other subcomponent relies on the value of one of its subcomponents
	  * directly (rather than extracting it from its assembled subject), it will likely bypass the check for the preset
	  * value and obtain `None` instead, resulting in a failed or incorrect assembly. The values for the subcomponents
	  * of the mapping are ''always'' extracted from the preset value using the mapping's `MappingExtract`
	  * for the component; the components' `optionally` (or `apply` and `assemble`) methods are never called and neither
	  * are they being aliased to their operative version provided by the extract. The root mapping's
	  * `optionally`/`apply` methods are being called as normal, allowing it to modify the preset result.
	  * You may wish to consider using `ComponentValues(mapping)(value, components)` instead, which follows exactly
	  * the general `ComponentValues` contract.
	  * @param value the result, top-level value.
	  */
	def Preset[S, O](mapping :TypedMapping[S, O], value :S, components :Unique[TypedMapping[_, O]]) :ComponentValues[S, O] =
		new SelectedDisassembledValues(mapping, value, components)



	/** Similar to `Preset[S, O](mapping, value)`, but the value is not computed until actually needed. The expression
	  * will be evaluated at most once. The values for the subcomponents of the mapping
	  * are ''always'' extracted from the preset value using the mapping's `MappingExtract` for the component;
	  * the components' `optionally` (or `apply` and `assemble`) methods are never called and neither are they
	  * being aliased to their operative version provided by the extract. The root mapping's `optionally`/`apply`
	  * method is being called as normal, allowing it to modify the preset result.
	  */
	def Lazy[S, O](mapping :TypedMapping[S, O], value: => S) :ComponentValues[S, O] =
		new LazyMappingValue[S, O](mapping, value)

	/** Create a lazy, preset instance using the value of the given expression for the component.
	  * Similar to `Lazy`, but the expression might not produce a value (return `None`), and used generally as a fallback
	  * default value for when the first choice couldn't be obtained. The values for the subcomponents of the mapping
	  * are ''always'' extracted from the preset value using the mapping's `MappingExtract` for the component;
	  * the components' `optionally` (or `apply` and `assemble`) methods are never called and neither are they
	  * being aliased to their operative version provided by the extract. The root mapping's `optionally`/`apply`
	  * method is being called as normal, allowing it to modify the preset result.
	  */
	def Fallback[S, O](mapping :TypedMapping[S, O], value: => Option[S]) :ComponentValues[S, O] =
		new LazyMappingResult[S, O](mapping, value)



	/** A proxy `ComponentValues` which delegates all assembly-related and '\' calls to the lazily computed
	  * instance initialized with the by-name parameter.
	  */
	def delay[S, O](values: => ComponentValues[S, O]) :ComponentValues[S, O] =
		new LazyComponentValuesProxy(values)





	/** Factory for `ComponentValues` which perform aliasing of all passed components before checking
	  * for preset values or using them for assembly. They guarantee that the operative version of the component
	  * is being used, with correct column names and buffs reflecting any changes imposed by their owners.
	  * Should be the first choice for obtaining `ComponentValues` instances.
	  * You can follow this application for additional argument list(s) to create `CompononentValues` instances:
	  * {{{
	  *     ComponentValues(mapping)(subject) //provide a preset value for the mapping and its subcomponents
	  *     ComponentValues(mapping) { comp => Option(value) } //provide a function with optional preset component values
	  *     ComponentValues(mapping)(values) { comp => index(comp) } //provide a sequence of values and an index function
	  * }}}
	  */
	@inline def apply[S, O](mapping :TypedMapping[S, O]) :ComponentValuesFactory[S, O] =
		new ComponentValuesFactory[S, O](mapping)



	/** Factory for `ComponentValues` instances associated with the given mapping.
	  * @param mapping a mapping for which `ComponentValues` are begin created.
	  * @tparam S the subject type of the associated 'root' mapping.
	  * @tparam O the origin type shared by the associated mapping and its subcomponents.
	  */
	final class ComponentValuesFactory[S, O](private val mapping :TypedMapping[S, O]) extends AnyVal { outer =>

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
			new AliasedOverrideMappingValue(mapping, Some(value), mapping)


		/** Returns `ComponentValues` based on a predefined mapping result. If it is not empty, the values
		  * for all components will be obtained by disassembling (extracting) their value from the argument based on
		  * the function specified by the `MappingExtract` provided by the component. This method is equivalent
		  * to either `ComponentValues(mapping)(result.get)` or `ComponentValues(mapping)()`, depending on whether
		  * `result` is defined.
		  * @param result proposed result of the mapping (the final result is that of the mapping's `optionally` method).
		  */
		def apply(result :Option[S]) :ComponentValues[S, O] = result match {
			case some :Some[S] => new AliasedOverrideMappingValue(mapping, some, mapping)
			case _ => apply()

		}


		/** Returns `ComponentValues` based on a predefined mapping result. The components listed in the second
		  * argument will be have their values disassembled (extracted from the preset value) using the `MappingExtract`
		  * supplied by the mapping provided earlier and `preset()` calls for all other will always return `None`,
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
			new SelectedDisassembledValues[S, S, O](mapping, value, components) with MappingAliasing[S, O] {
				override val mapping = outer.mapping
			}


		/** Returns `ComponentValues` using the given function as the source of values for components.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with `None` resulting in the value of the component being assembled from its subcomponents.
		  * Preset values are never disassembled into values for subcomponents: providing a value for a component
		  * other than a column (instead of its columns) requires that no other component, including its owner,
		  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
		  * function - if it is `None` it may result in an assembly failure.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods.
		  * You can supply a `Map` as an argument.
		  * @param values factory of values for components, must return an option of the same type as the input mapping..
		  */
		def apply(values :TypedMapping[_, O] => Option[_]) :ComponentValues[S, O] =
			new UntypedValues[S, O](values) with MappingAliasing[S, O] {
				override val mapping = outer.mapping
			}


		/** Returns `ComponentValues` using the given generic function as the source of values for components.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with `None` resulting in the value of the component being assembled from its subcomponents.
		  * Preset values are never disassembled into values for subcomponents: providing a value for a component
		  * other than a column (instead of its columns) requires that no other component, including its owner,
		  * accesses any of its subcomponents directly, as it would read the exact value returned by the provided
		  * function - if it is `None` it may result in an assembly failure.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods.
		  * You can supply a `NaturalMap` as an argument.
		  * @param values factory of values for components.
		  */
		def apply(values :MappingFrom[O]#Component =#> Option) :ComponentValues[S, O] =
			new CustomValues[S, O](values) with MappingAliasing[S, O] {
				override val mapping = outer.mapping
			}


		/** Returns `ComponentValues` wrapping the given set of values with their assignment to components happening
		  * by the use of the provided indexing function. An entry of `None` or a negative index results in the
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
		def apply(values :IndexedSeq[Option[Any]])(index :MappingFrom[O] => Int)
				:ComponentValues[S, O] =
			new IndexedValues[S, O](values, index) with MappingAliasing[S, O] {
				override val mapping = outer.mapping
			}



/*
		/** Return a preset `ComponentValues` for the associated mapping. All `preset()` calls on the returned
		  * instance and any child `ComponentValues` will return a value disassembled from this value using the path
		  * to the requested component as the getter. The value will be computed lazily when any component is accessed,
		  * but at most once.
		  * @param value expression returning proposed value for the mapping.
		  */
		@inline def Lazy(value : => S) :ComponentValues[S, O] = ComponentValues.Lazy(mapping, value)

		/** Return ComponentValues based on a result of a passed ''by name'' expression which will be evaluated
		  * at most once, when a value for the target mapping or some of its components is requested. If the expression
		  * returns `None`, the values will behave exactly like `Empty` `ComponentValues`; if it returns `Some(x)`,
		  * the effect will be the same as with a call to `Predefined(x)` / `apply(x)` or `Lazy(x)`. This method is used
		  * primarily for creating second-choice instances to use as a fallback data source for other `ComponentValues`.
		  */
		@inline def Fallback(value : => Option[S]) :ComponentValues[S, O] = ComponentValues.Fallback(mapping, value)
*/

		/** Create an empty instance returning always `None`/another `Empty ComponentValues` or throwing
		  * a `NoSuchElementException`. It will print the provided mapping in its `toString` method and
		  * any exception messages resulting from a lack of value. Note that all components passed to this
		  * instance will be aliased with this mapping's [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method, which guarantees that the operative version of the component is being used for assembly
		  * (and thus it and its subcomponents have the final set of buffs), but is somewhat less efficient.
		  */
		def apply() :ComponentValues[S, O] =
			new EmptyValues[S, O](mapping.toString) with MappingAliasing[S, O] {
				override val mapping = outer.mapping
			}


	}






	private abstract class AbstractDedicatedMappingValues[S, O](mapping :TypedMapping[S, O]) extends ComponentValues[S, O] {
		override def apply[T](component :TypedMapping[T, O]) :T = apply(mapping(component))

		override def get[T](component :TypedMapping[T, O]) :Option[T] = get(mapping(component))

		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = this \ mapping(component)
	}






	private def aliasing[O](extracts :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Extract])
			:MappingFrom[O]#Component =#> MappingFrom[O]#Component =
		new (MappingFrom[O]#Component =#> MappingFrom[O]#Component) {
			override def apply[T](x :TypedMapping[T, O]):TypedMapping[T, O] = extracts(x).export
		}

	private def aliasing[O](root :MappingFrom[O]) :MappingFrom[O]#Component =#> MappingFrom[O]#Component =
		new (MappingFrom[O]#Component =#> MappingFrom[O]#Component) {
			override def apply[T](x :TypedMapping[T, O]) =
				if (x eq root) root.asInstanceOf[TypedMapping[T, O]]
				else root.export(x)
		}



	trait ComponentValuesAliasing[S, O] extends ComponentValues[S, O] { outer =>
		protected def alias[T](component :TypedMapping[T, O]) :TypedMapping[T, O]

		val aliasing = new ComponentValues[S, O] {
			override def assemble(root :TypedMapping[S, O]) :Option[S] = outer.assemble(alias(root))
			override def preset(root :TypedMapping[S, O]) :Option[S] = outer.preset(alias(root))
			override def optionally(root :TypedMapping[S, O]) :Option[S] = outer.optionally(alias(root))
			override def root(root :TypedMapping[S, O]) :S = outer.root(alias(root))

			override def get[T](component :TypedMapping[T, O]) :Option[T] =
				outer.crosscast[T].optionally(alias(component))

			override def get[T](extract :MappingExtract[S, T, O]) :Option[T] =
				outer.crosscast[T].optionally(alias(extract.export))

			override def \[T](component :TypedMapping[T, O]) = outer.crosscast[T]
		}


		override def assemble(root :TypedMapping[S, O]) :Option[S] = {
			val preset = this.preset(root)
			if (preset.isDefined) preset
			else root.assemble(aliasing)
		}

		override def get[T](component :TypedMapping[T, O]) :Option[T] = super.get(alias(component))

		override def get[T](extract :MappingExtract[S, T, O]) :Option[T] = super.get(alias(extract.export))


		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = crosscast[T]


		override def toString :String = "~" + super.toString
	}



	trait MappingAliasing[S, O] extends ComponentValuesAliasing[S, O] {
		protected val mapping :MappingFrom[O]

		override protected def alias[T](component :TypedMapping[T, O]) :TypedMapping[T, O] =
			mapping.export(component)
	}






	private class AliasedComponentValues[S, O]
	              (values :ComponentValues[S, O], alias :MappingFrom[O]#Component =#> MappingFrom[O]#Component)
		extends ComponentValues[S, O]
	{ outer =>

		def this(values :ComponentValues[S, O],
		         extracts :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Extract]) =
			this(values, aliasing(extracts))

		def this(values :ComponentValues[S, O], root :TypedMapping[_, O]) =
			this(values, aliasing(root))



		private[this] val nonAliasingValues = new ComponentValues[S, O] {
			override def preset(root :TypedMapping[S, O]) = values.preset(root)
			override def \[T](component :TypedMapping[T, O]) = outer \ component
		}

		private def aliased :ComponentValues[S, O] = nonAliasingValues



		override def assemble(root :TypedMapping[S, O]) :Option[S] = {
			val aliased = alias(root)
			val preset = values.preset(aliased)
			if (preset.isDefined) preset
			else root.assemble(this)
		}


		override def preset(root :TypedMapping[S, O]) :Option[S] =
			values.preset(alias(root))

		override def optionally(root :TypedMapping[S, O]) :Option[S] =
			alias(root).optionally(nonAliasingValues)

		override def root(root :TypedMapping[S, O]) :S =
			alias(root)(nonAliasingValues)



		override def get[T](component :TypedMapping[T, O]) :Option[T] = {
			val export = alias(component)
			val next = values \ export
			if (next eq values) export.optionally(nonAliasingValues.crosscast[T])
			else export.optionally(new AliasedComponentValues[T, O](next, alias).aliased)
		}

		override def get[T](extract :MappingExtract[S, T, O]) :Option[T] = get(extract.export)


		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = {
			val export = alias(component)
			val next = values \ export
			if (next eq values) nonAliasingValues.crosscast[T]
			else new AliasedComponentValues[T, O](next, alias)
		}



		override def aliased(export :MappingFrom[O]#Component =#> MappingFrom[O]#Component) =
			new AliasedComponentValues(values, alias andThen export)



		override def toString :String = "~{" + values.toString + "}"
	}






	private class FallbackValues[S, O](overrides :ComponentValues[S, O], fallback :ComponentValues[S, O])
		extends ComponentValues[S, O]
	{
		override def preset(root: TypedMapping[S, O]): Option[S] = overrides.preset(root)

		override def assemble(root: TypedMapping[S, O]): Option[S] = overrides.preset(root) match{
			case some :Some[S] => some
			case _ => root.assemble(this) match {
				case some :Some[S] => some
				case _ => fallback.preset(root)
			}
		}

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] = {
			val first = overrides \ extract; val second = fallback \ extract
			if ((first eq overrides) && (second eq fallback)) crosscast[T]
			new FallbackValues[T, O](first, second)
		}

		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = {
			val first = overrides \ component; val second = fallback \ component
			if ((first eq overrides) && (second eq fallback)) crosscast[T]
			else new FallbackValues[T, O](first, second)
		}




		override def toString = s"$overrides orElse $fallback"

	}






	private class StickyComponentValues[S, O] private[ComponentValues]
	              (values :NaturalMap[MappingFrom[O]#Component, Origin[O]#T], default :ComponentValues[S, O] = Empty[S, O])
		extends ComponentValues[S, O]
	{
		override def preset(root: TypedMapping[S, O]): Option[S] = values.get(root).flatMap(_.preset(root))

		override def assemble(root: TypedMapping[S, O]): Option[S] = values.get(root).flatMap(_.assemble(root))



		override def get[T](extract :MappingExtract[S, T, O]) :Option[T] =
			values.get(extract.export).flatMap(_.optionally(extract.export)) orElse default.get(extract)

		override def get[T](component :TypedMapping[T, O]) :Option[T] =
			values.get(component).flatMap(_.optionally(component)) orElse default.get(component)

		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] =
			values.get(component) match {
				case Some(vals) => vals
				case _ => crosscast[T]
			}



		override def stick[T](component :TypedMapping[T, O]) :ComponentValues[S, O] =
			new StickyComponentValues[S, O](values.updated[Origin[O]#T, T](component, this \ component))




		override def aliased(export :MappingFrom[O]#Component =#> MappingFrom[O]#Component) :ComponentValues[S, O] =
			new StickyComponentValues[S, O](values, default aliased export)


		override def toString :String = values.to(Array).map(v => v._1.toString + "->" + v._2).mkString("{", ",", "}")
	}






	private class LazyComponentValuesProxy[S, O](values: => ComponentValues[S, O]) extends ComponentValues[S, O] {
		private[this] var init = () => values
		@volatile private[this] var initialized :ComponentValues[S, O] = _
		private[this] var cache :ComponentValues[S, O] = _

		private def backing :ComponentValues[S, O] = {
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

		override def assemble(root :TypedMapping[S, O]) :Option[S] = backing.assemble(root)
		override def preset(root :TypedMapping[S, O]) :Option[S] = backing.preset(root)

		override def optionally(root :TypedMapping[S, O]) :Option[S] = backing.optionally(root)
		override def root(mapping :TypedMapping[S, O]) :S = backing.root(mapping)


		override def apply[T](extract :MappingExtract[S, T, O]) = backing(extract)
		override def apply[T](component :TypedMapping[T, O]) = backing(component)

		override def get[T](extract :MappingExtract[S, T, O]) = backing.get(extract)
		override def get[T](component :TypedMapping[T, O]) = backing.get(component)

		override def \[T](extract :MappingExtract[S, T, O]) = backing \ extract
		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = backing \ component

		override def toString :String = backing.toString
	}






	private class EmptyValues[S, O](source : =>String) extends ComponentValues[S, O] {
		def this() = this("Empty")

		def this(mapping :TypedMapping[S, O]) = this(mapping.toString)

		override def preset(root: TypedMapping[S, O]): Option[S] = None

		override def assemble(root: TypedMapping[S, O]): Option[S] = None


		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = crosscast[T]

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] = crosscast[T]



		override def orElse(values: ComponentValues[S, O]): ComponentValues[S, O] = values


		override def toString :String = source
	}

	private[this] val empty :ComponentValues[Any, Any] = new EmptyValues[Any, Any]






	trait PredefinedMappingResult[S, O] extends ComponentValues[S, O] {
		def result :Option[S]

		final override def preset(root :TypedMapping[S, O]) :Option[S] = result

		override def apply[T](extract :MappingExtract[S, T, O]) :T =
			if (result.isDefined) extract(result.get)
			else throw new NoSuchElementException(s"No value for $extract in $this")

		override def get[T](extract :MappingExtract[S, T, O]) :Option[T] =
			if (result.isDefined) extract.get(result.get)
			else None

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] =
			if (result.isEmpty) Empty[T, O]
			else ComponentValues(extract.export, extract.get(result.get))


		override def toString :String = "Predefined(" + result + ")"

	}



	trait PresetMappingValue[S, O] extends PredefinedMappingResult[S, O] {
		def result :Some[S]

		final override def assemble(root :TypedMapping[S, O]) :Option[S] = result

		override def apply[T](extract :MappingExtract[S, T, O]) :T = extract(result.get)

		override def get[T](extract :MappingExtract[S, T, O]) :Option[T] = extract.get(result.get)

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] =
			ComponentValues(extract.export, extract.get(result.get))


		override def toString :String = "Preset(" + result.get + ")"
	}



	private class OverrideMappingResult[S, O](mapping :TypedMapping[S, O], override val result :Option[S])
		extends AbstractDedicatedMappingValues[S, O](mapping) with PredefinedMappingResult[S, O]
	{
		override def toString = "{" + mapping +":=" + result.mapOrElse(_.toString, "_") + "}"
	}



	private class OverrideMappingValue[S, O](mapping :TypedMapping[S, O], override val result :Some[S])
		extends AbstractDedicatedMappingValues[S, O](mapping) with PresetMappingValue[S, O]
	{
		override def toString = "{" + mapping +":=" + result.get + "}"
	}



	private class AliasedOverrideMappingValue[S, O]
	              (mapping :TypedMapping[S, O], override val result :Some[S],
	               alias :TypedMapping[S, O]#Component =#> TypedMapping[S, O]#Component)
		extends PresetMappingValue[S, O]
	{
		def this(mapping :TypedMapping[S, O], result :Some[S],
		         extracts :NaturalMap[MappingFrom[O]#Component, MappingFrom[O]#Extract]) =
			this(mapping, result, aliasing(extracts))

		def this(mapping :TypedMapping[S, O], result :Some[S], root :TypedMapping[_, O]) =
			this(mapping, result, aliasing(root))

		override def optionally(root :TypedMapping[S, O]) :Option[S] = alias(root).optionally(this)


		override def apply[T](extract :MappingExtract[S, T, O]) = extract.get(result.get) match {
			case some :Some[T] =>
				val export = alias(extract.export)
				export(new AliasedOverrideMappingValue[T, O](export, some, alias))
			case _ =>
				alias(extract.export)(Empty[T, O])
		}



		override def get[T](extract :MappingExtract[S, T, O]) = extract.get(result.get) match {
			case some :Some[T] =>
				val export = alias(extract.export)
				export.optionally(new AliasedOverrideMappingValue[T, O](export, some, alias))
			case _ =>
				alias(extract.export).optionally(Empty)
		}

		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = this \ mapping(component)


		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] = extract.get(result.get) match {
			case some :Some[T] => new AliasedOverrideMappingValue(alias(extract.export), some, alias)
			case _ => Empty
		}



		override def aliased(export :MappingFrom[O]#Component =#> MappingFrom[O]#Component) =
			new AliasedOverrideMappingValue(mapping, result, export andThen alias)



		override def toString = "~{" + mapping + ":=" + result.get + "}"
	}






	private class LazyMappingResult[S, O](mapping :TypedMapping[S, O], expr : =>Option[S])
		extends AbstractDedicatedMappingValues[S, O](mapping) with PredefinedMappingResult[S, O]
	{
		override lazy val result :Option[S] = expr

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] =
			if (extract.isIdentity) crosscast[T]
			else new LazyMappingResult[T, O](extract.export, result.flatMap(extract.optional))

		override def toString =
			if (result.isDefined) "{" + mapping + ":=" + result.get + "}"
			else s"{$mapping:=_}"
	}



	private class LazyMappingValue[S, O](mapping :TypedMapping[S, O], expr : => S)
		extends AbstractDedicatedMappingValues[S, O](mapping) with PresetMappingValue[S, O]
	{
		override lazy val result :Some[S] = Some(expr)

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] =
			if (extract.isIdentity)
				crosscast[T]
			else
				extract.requisite match {
					case Some(pick) => new LazyMappingValue[T, O](extract.export, pick(result.get))
					case _ =>
						val pick = extract.optional
						new LazyMappingResult[T, O](extract.export, pick(result.get))
				}

		override def toString =
			if (result.isDefined) "{" + mapping + ":=" + result.get + "}"
			else s"{$mapping:=_}"
	}






	/** Base trait for `ComponentValues` implementations which include preset values for an arbitrary set
	  * of components, in particular columns. Each call for a value for a component of `M` results in this instance 
	  * first checking if the `defined` method returns a value for the component and, if so, simply returns it. 
	  * If no preset value for the component is available, it will be assembled. Calls for `ComponentValues` instances
	  * for subcomponents work similarly: if this instance contains a preset value `x`, `Preset(x)` is
	  * returned; otherwise this instance simply casts itself to the component type. A non obvious implication
	  * of this algorithm is that this instance's `preset` method ''always'' returns `None` and `assemble`
	  * delegates straight to `mapping.assemble`, as the check for the preset value occurred earlier.
	  * @tparam M type of the target mapping defining the components for which this instance contains values.
	  *           In non-abstract cases the singleton type of the mapping object used for assembly
	  */
	private trait UniversalValues[S, O] extends ComponentValues[S, O] { outer =>

		override def \[T](extract :MappingExtract[S, T, O]) :ComponentValues[T, O] = crosscast[T]

		override def \[T](component :TypedMapping[T, O]) :ComponentValues[T, O] = crosscast[T]
	}







	private class SelectedDisassembledValues[R, S, O]
	              (value :R, components :Unique[MappingFrom[O]],
	               extracts :NaturalMap[TypedMapping[R, O]#Component, TypedMapping[R, O]#Extract])
		extends UniversalValues[S, O]
	{
		def this(mapping :TypedMapping[R, O], value :R, components :Unique[MappingFrom[O]]) =
			this(value, components,
			     mapping.extracts.updated[TypedMapping[R, O]#Extract, R](mapping, MappingExtract.ident(mapping))
		     )


		override def preset(mapping :TypedMapping[S, O]) :Option[S] = {
			val extract = extracts(mapping)
			if (components.contains(extract.export)) extract.get(value)
			else None
		}

		override def toString :String = components.mkString("ComponentValues(" + value + ": ", ", ", ")")
	}






	private class CustomValues[S, O](values :TypedMapping[S, O]#Component =#> Option) extends UniversalValues[S, O] {
		override def preset(component :TypedMapping[S, O]) :Option[S] = values(component)

		override def toString :String = "ComponentValues(" + values + ")"
	}





	private class UntypedValues[S, O](values :TypedMapping[_, O] => Option[_]) extends UniversalValues[S, O] {
		override def preset(component :TypedMapping[S, O]) :Option[S] =
			values(component).asInstanceOf[Option[S]]

		override def toString :String = "ComponentValues(" + values + ")"
	}





	private class IndexedValues[S, O](values :IndexedSeq[Option[Any]], index :MappingFrom[O] => Int)
		extends UniversalValues[S, O]
	{
		override def preset(component :TypedMapping[S, O]) :Option[S] = {
			val i = index(component)
			if (i < 0) None
			else values(i).crosstyped[S]
		}

		override def toString :String = values.map {
			case Some(x) => String.valueOf(x)
			case None => "_"
		}.mkString("ComponentValues(", ", ", ")")

	}



}

