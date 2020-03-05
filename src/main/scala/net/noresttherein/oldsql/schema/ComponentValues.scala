package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.schema.ComponentPath.{\:\, TypedComponentPath}
import net.noresttherein.oldsql.schema.ComponentValues.StickyComponentValues
import net.noresttherein.oldsql.schema.Mapping.{AnyComponent, CompatibleMapping, Component, SingletonComponent, SingletonMapping, TypedMapping, TypedSingleton}
import net.noresttherein.oldsql.slang.SaferCasts._
import net.noresttherein.oldsql.slang._


/** Values for components of the given mapping together with possibly the value for the whole mapping,
  * used as input data for the assembly process defined by the mapping `M`.
  * It is created typically from row data of an sql query to map tabular result into an object , but can be also used
  * to set the values of specific components on an existing value `x :M#Subject` by disassembling `x` into `ComponentValues`,
  * substituting the values for the components we want to change, and reassembling it again to M#Subject using the mapping.
  * Concrete instances are generally parameterised by the singleton type of the mapping instance used
  * (so values for some mapping `Table` will have the type of `ComponentValues[Table.type]`).
  *
  * It is designed to support a hierarchical assembly process, where a mapping will assemble the final result from
  * the values of its direct components, which might not be defined explicitly in this instance, but assembled themselves
  * on request from lower level component values. Specifying the values only for the bottom components, i.e. columns is
  * the typical use case, but it is possible to set a value for any component in the tree. In the latter case, when
  * a value is requested in turn for its subcomponent, it will be disassembled from the predefined higher-level value.
  * It is therefore possible to disassemble the value of the mapping type of this component simply by creating
  * `ComponentValues` instance based on it and asking for the values of all columns of the mapping.
  *
  * Care should be taken in situations where a value is present
  * both for a particular component and some of its subcomponents, as the intended behaviour for this case is not clear,
  * as the caller might want either to use the higher-level value, or copy it substituting the value for the lower-level
  * component. In general, implementations will treat the value of the higher level-component as an override for
  * any values lower in the assembly tree, but it is not enforced. In fact, implementing this behaviour is only possible
  * when the values are accessed in the descending order, with the mapping only requesting the values either for its
  * direct components, or via a path to the subcomponent. There's no generic way of discovering the path to a given
  * (lifted) subcomponent of the associated mapping when asked directly. For example, when a mapping for class Person
  * containing an address component asks directly for the value of 'street' column, instead of either specifying
  * the path `address \ street` or just the value of `address`, leaving the assembly to the address component,
  * there's no way of checking if there's a predefined value for address that should be used as a source of data for
  * the street. For this reason (as well as proper encapsulation and reusability), proper mapping implementations
  * should always ask only for values direct components and always use a full path explicitly specifying
  * any intermediate components in the case when the higher-level assembly process requires checking the value
  * for a component/column which is not directly nested under this mapping.
  *
  * The interface methods can be grouped roughly into three sets: those which return a value for the given component,
  * those that return `ComponentValues` to pass for the assembly to the given component, and modification methods allowing
  * either changing the values, or mapping this instance for use by another mapping. As `ComponentValues` can be thought
  * as basically a function from the components of the associated mapping `m` to their values, adjusting the `ComponentValues`
  * for another mapping `n` constitutes of mapping the components of `n` into components of `m` (and possibly mapping
  * the result type for `m` into a result type for `n`).
  *
  * @tparam M type of the target mapping defining the components for which this instance contains values.
  *           In non-abstract cases the singleton type of the mapping object used for assembly
  */
trait ComponentValues[M <: SingletonMapping] {

	/** Compute the value for the root mapping, either using a predefined value or delegating to the mapping's assembly
	  * method. It is not the top-level function - use either `mapping.apply()` or no-argument `apply()` on dedicated
	  * subclasses, as the mapping should always have the last say in the result. This is implemented by a triple
	  * dispatch process - `mapping.apply()/optionally()` invokes this method, performs any validation or modification
	  * such as providing a default value if none could be obtained. This method in turn will generally first check
	  * if `predefined()` will return a result, and resorts to mapping.assemble when nothing was set.
	  * @param root the root mapping associated with this instance. Note that ComponentValues should be always
	  *            statically typed for the singleton type of the associated (this) mapping and passing it to other
	  *            instances of the same mapping type is not supported in general. In fact, some implementations
	  *            store the associated mapping and are free to use it in implementation, and this argument should
	  *            be the exact same instance in that case.
	  * @return
	  */
	def result(root :M) :Option[M#Subject]

	/** Delegates to `root(this)` without modifying the result. Intended for caching/logging implementations. */
	def value(root :M) :M#Subject = root.apply(this.downcast[root.type])

	/** Delegates to `root.optionally(this)` without modifying the result. intended for caching/logging implementations. */
	def getValue(root :M) :Option[M#Subject] = root.optionally(this.downcast[root.type])


	/** Is there a predefined - explicitly set on creation of this/parent `ComponentValues` instance -
	  * value for the top level mapping associated with this instance?
	  * @param root mapping associated with this instance, would be of the singleton type parameter M and equal to
	  *             the one optionally stored in the implementation.
	  */
	private[schema] def predefined(root :M) :Option[M#Subject]

	/** Return the value for the given component. This method should always first adapt itself to the given component
	  * by creating ComponentValues[component.type] and passing it to component.apply() so it can inspect the
	  * returned value. Equivalent to `component(this :\ component)`.
	  * @param component a component of the associated mapping. It is considered best practice to ask only for the values
	  *                  of top-level components, and use the corresponding method accepting a path for subcomponents
	  *                  (see class description).
	  * @throws NoSuchElementException if no value could be provided, be it preset, assembled or default.
	  */
	def apply[T](component :Component[M#Owner, T]) :T

	/** Return the value for the given component or None if it can't be obtained in any way (predefined, assembly or
	  * default). Should be equivalent to component.optionally(this :\ component), rather than return a value directly
	  * without inspection by the component.
	  * @param component  a component of the associated mapping. It is considered best practice to ask only for
	  *                   the values of top-level components, and use the corresponding method accepting a path for
	  *                   subcomponents (see class description).
	  */
	def get[T](component :Component[M#Owner, T]) :Option[T]

	/** Return ComponentValues for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping), first mapping the component arguments of
	  * the passed components to the components of the mapping associated with this instance. This is a generic version
	  * of the `:\` method for implementors. Client code should use `:\` which returns an instance specifically
	  * associated with the component argument.
	  * @param component  a component of the associated mapping. It is considered best practice to ask only for the
	  *                   values of top-level components, and use the corresponding method accepting a path for
	  *                   subcomponents (see class description).
	  */
	def values[C <: SingletonComponent[M#Owner, T], T](component :C) :ComponentValues[C]


	/** Return ComponentValues for the given component of the associated mapping. Returned object will delegate all
	  * calls to this instance (or the parent instance of this mapping), first mapping the component arguments of the
	  * passed components to the components of the mapping associated with this instance. This is a generic version of
	  * `:\` method for implementors. Client code should use `:\` which returns an instance specifically associated
	  * with the component argument.
	  * @param component  a component of the associated mapping. It is considered best practice to ask only for the
	  *                   values of top-level components, and use the corresponding method accepting a path for
	  *                   subcomponents (see class description).
	  */
	def :\[T](component :Component[M#Owner, T]) :ComponentValues[component.type] = values[component.type, T](component)


	/** Return the value for a given subcomponent of the associated mapping specified by the given path.
	  * The implementation will generally recursively descend through all mappings on the path, looking for
	  * any predefined values. Note that because mappings can alter the final result value, the value returned
	  * by this method might not be the same as asking for the value of the target component of x :M#Subject, where
	  * x is the result value of the mapping process of this instance. In particular, there might not be any value
	  * for the target component in the input set, but some component on the path might provide a default one.
	  */
	def apply[C <: SingletonMapping](path :M \:\ C) :C#Subject

	/** Return the value for a given subcomponent of the associated mapping specified by the given path or None
	  * if it couldn't be obtained in any way.
	  * The implementation will generally recursively descend through all mappings on the path, looking for
	  * any predefined values. Note that because mappings can alter the final result value, the value returned
	  * by this method might not be the same as asking for the value of the target component of x :M#Subject, where
	  * x is the result value of the mapping process of this instance. In particular, there might not be any value
	  * for the target component in the input set, but some component on the path might provide a default one.
	  */
	def get[C <: SingletonMapping](path :M \:\ C) :Option[C#Subject]

	/** Return ComponentValues instance associated with the end mapping of the given path, which will serve as a proxy
	  * to this instance (or this instance parent values). This is the generic version of '\' method designed for easier
	  * use be implementations, and client code should always use the latter when possible.
	  * The standard implementation, unless some shortcut is predefined for access by a path directly, will delegate
	  * the proxing process to the given path instance, which will recursively adapt the instance by one step of the path
	  * at a time.
	  */
	def values[C <: SingletonMapping](path :M \:\ C) :ComponentValues[C]


	/** Return ComponentValues instance associated with the given direct component of this mapping.
	  * It is a handy implementation shortcut and will always be equivalent to calling values(path) and values(path.end).
	  */
	def direct[C <: SingletonComponent[M#Owner, T], T](path :TypedComponentPath[M, C, T]) :ComponentValues[C]


	/** Return ComponentValues instance for the given subcomponent of this mapping. Return values will in most cases
	  * by just a proxy to this instance, mapping the arguments into components of the mapping associated with this instance.
	  * The standard solution will delegate to the path argument the process of adapting arguments to those accepted by this instance,
	  * either directly or one-step at a time.
	  */
	def \[C <: SingletonMapping](path :M \:\ C) :ComponentValues[C] = values(path)



	/** A specialized version of apply(path :ComponentPath[M, C]) to make type inference easier, as scalac has problems with member types. */
	@inline final def apply[C <: TypedSingleton[T], T](path :TypedComponentPath[M, C, T]) :T =
		apply[C](path :M \:\ C) //:C#Subject

	/** A specialized version of get(path :ComponentPath[M, C]) to make type inference easier, as scalac has problems with member types. */
	@inline final def get[C <: TypedSingleton[T], T](path :TypedComponentPath[M, C, T]) :Option[T] =
		get[C](path :M \:\ C) //:Option[C#Subject]


	/** Adapt this instance for use with another mapping, which shares components with the one associated with this instance.
	  * This method exists for all adapter and proxy mappings, whose components are exactly components of the proxied mapping.
	  * @param mapping
	  * @tparam C
	  * @return
	  */
	def identical[C <: CompatibleMapping[M] with Singleton](mapping :C) :ComponentValues[C]


/*
	/** Adapt this instance for use with another mapping which is the target of the givne morphism.
	  * Returned instance will use the morphism's result part to map the value for the associated mapping returned
	  * by predefined/result methods, and structural (or ComponentMorphism) to map the input arguments
	  * (components of the morphism target) into this instance arguments (components of the associated mapping).
	  * Please note that morphisms in general might not be lossless - some components might not have representations in the target mapping.
	  */
	def morph[C<:AnyMapping](morphism :MappingMorphism[M, C]) :ComponentValues[C]
*/


	/** Create ComponentValues which will fallback to the given instance when no value for a particular component can
	  * be obtained in any way. The values in this instance will always have precedence, even if they are defined at a lower level.
	  * For example, if this instance contains a value for a single column, and the argument contains a predefined value for the whole mapping,
	  * the result of the mapping will still be assembled from lower-level components so that the value of the column defined here
	  * is used, disassembling the values for other components from the argument.
	  * @param values
	  * @return
	  */
	def orElse(values :ComponentValues[M]) :ComponentValues[M]

	/** Create ComponentValues attached with the given mapping which will ignore any morphing operations (morph, :\, \\, values, etc.).
	  * Each time a value/values are requested for a mapping/component, it will check if the argument is equal to the mapping given here,
	  * and if so return this instance, otherwise itself. This is useful when we want to preset component values for a component
	  * further down the mapping hierarchy, without knowing its lifted representation. Can be combined with orElse to provide
	  * preset values for selected subcomponents and fallback to default algorithm for all other.
	  * @param mapping
	  * @return
	  */
	def stickTo[X <: SingletonMapping](mapping :M) :ComponentValues[X] =
		new StickyComponentValues[M](mapping, this).crosscast[X]


	/** Create ComponentValues which, regardless of any future morphing operations, will always
	  * first check if their argument is the mapping to which the given path points and, if so,
	  * will return the value that this instance would return. For all other mappings default rules apply.
	  * This makes it possible to implement symlink like behaviour between components: if the mapping to which this path
	  * is pointing was passed as-is for use by some subcomponents of mapping M, more than one 'copy' of target component
	  * could occur on mapping M's component list, origininating from lifting different uses of the target mapping.
	  * They would not be equal to each other however, due to different lifting/mapping applied by mappings on the
	  * path to their use. By calling values.stick(path) :\ componentUsingPathTarget we make sure that whenever
	  * any mapping further down the path uses the target of the path, it will delegate directly to values specific
	  * to the path of original definition.
	  * @param path
	  * @tparam X
	  * @return
	  */
	def stick[X <: SingletonMapping](path :M \:\ X) :ComponentValues[M] =
		values(path).stickTo[M](path.end) orElse this

	/** A safer casting method which allows casting this instance, invariant regarding to the mapping type, to a higher mapping type.
	  * Generally used to cast from a singleton-typed instance to the generic type instance.
	  */
	@inline final def upcast[X >: M <: SingletonMapping] :ComponentValues[X] = this.asInstanceOf[ComponentValues[X]]

	/** A safer casting method requiring the associated mapping type to be a subtype of the type parameter; used to cast
	  * a generic instance into one parameterised with singleton type of the target mapping.
	  */
	@inline final def downcast[X <: M] :ComponentValues[X] = this.asInstanceOf[ComponentValues[X]]

	/** Shortcut for casting the type parameter to a different type reserved for the implementations */
	@inline final private[ComponentValues] def crosscast[X <: SingletonMapping] :ComponentValues[X] =
		this.asInstanceOf[ComponentValues[X]]



}





/** Factory for containers of deassembled values for components of a mapping which can be used as input data for the mapping.
  * It is created typically from row data of an sql query to map tabular result into an object , but can be also used to set
  * the values of specific components on an existing value x :M#Subject by deassembling x into ComponentValues,
  * substituting the values for the components we want to change, and reassembling it again to M#Subject using the mapping.
  * Concrete instanceses are generally parameterised by the singleton type of the mapping instance used
  * (so values for some mapping Table will have the type of ComponentValues[Table.type]).
  *
  * It is designed to support a hierarchical assembly process, where a mapping will assemble the final result from the values
  * of its direct components, which might not be defined explicitly in this instance, but assembled themselves on request
  * from lower level component values. Specifying the values only for the bottom components, i.e. columns is the typical use case,
  * but it is possible to set a value for any component in the tree. In the latter case, when a value is requested in turn for
  * its subcomponent, it will be deassembled from the predefined higher-level value. It is therefore possible to deassemble
  * the value of the mapping type of this component simply by creating ComponentValues instance based on it and asking for the
  * values of all columns of the mapping.
  *
  */
object ComponentValues {
	type TypedValues[M <: TypedSingleton[T], T] = ComponentValues[M]


	/** Returns ComponentValues using the given function as the source of values for components. Please note that you can supply a Map
	  * as an argument.
	  * @param mapping mapping used for assembly process
	  * @param values factory of values for components, should always return the value of the type correct for the component argument!
	  */
	@inline def apply[M <: SingletonMapping](mapping :M, values :AnyComponent[M#Owner]=>Option[_]) :ComponentValues[M] =
		generic[M](mapping)(values)


	/** An empty instance, returning always None or throwing NoSuchElementException.
	  * This is the generic version of the method, use the ComponentValuesFactory to get the singleton-typed one by calling
	  * ComponentValues(mapping)(..) instead.
	  */
	@inline def Empty[M <: SingletonMapping] :ComponentValues[M] = empty.crosscast[M]

	/** An empty instance, returning always None or throwing NoSuchElementException.
	  * This is the generic version of the method, use the ComponentValuesFactory to get the singleton-typed one by calling
	  * ComponentValues(mapping)(..) instead.
	  * @param source a description of the original source which created this instance for more helpfull message in thrown exceptions
	  */
	@inline def Empty[M <: SingletonMapping](source : =>String) :ComponentValues[M] = new EmptyValues[M](source)

	/** Create ComponentValues for the given mapping and its value. All values returned by this instance will use the path
	  * to the requested component to pick (disassemble) the value from the given argument. Please not that, as the mapping process
	  * always passes the ComponentValues to the mapping, the returned value might not be this argument, if the mapping decides
	  * to change it. This is the generic version
	  * @param mapping associated mapping
	  * @param value result, top-level value.
	  */
	@inline def Predefined[M <: SingletonMapping](mapping :M, value :M#Subject) :DedicatedMappingValue[M] =
		new ExplicitMappingValue[M](mapping, value)

	/** Similar to Predefined[M](mapping, value), but the value is not computed until actually needed. The expression will be evaluated at most once.
	  * This is the generic version of the method, use the ComponentValuesFactory to get the singleton-typed one by calling
	  * ComponentValues(mapping)(..) instead.
	  */
	@inline def Lazy[M <: SingletonMapping](mapping :M, value : =>M#Subject) :DedicatedMappingValue[M] =
		new LazyPredefinedMappingValue[M](mapping, value)


	/** Create a lazy, predefined instance using the value of the given expression for the component.
	  * Similar to lazy, but the expression might not produce a value (return None), and used generally as a fallback default value
	  * for when the first choice couldn't be obtained.
	  * This is the generic version of the method, use the ComponentValuesFactory to get the singleton-typed one by calling
	  * ComponentValues(mapping)(..) instead.
	  */
	@inline def Fallback[M <: SingletonMapping](mapping :M, value : =>Option[M#Subject]) :ComponentValues[M] =
		new LazyMappingValue[M](mapping, value)



	/** Factory for ComponentValues statically associated with the given mapping (will return ComponentValues[mapping.type]).
	  * Should be the first choice for obtaining ComponentValues instances.
	  */
	@inline def apply(mapping :AnyMapping) :ComponentValuesFactory[mapping.type] =
		new ComponentValuesFactory[mapping.type](mapping)

	/** A generic version of apply(mapping :AnyMapping), which will return ComponentValues associated with the generic type of the mapping.
	  * It is inteded for generic code, for example functions implementing generic morphisms between mappings which operate on type arguments as mapping types.
	  * Concrete cases when ComponentValues are needed for a known, particular mapping instance, should always use apply(AnyMapping) instead.
	  * @param mapping
	  * @tparam M
	  * @return
	  */
	@inline def generic[M <: SingletonMapping](mapping :M) :ComponentValuesFactory[M] =
		new ComponentValuesFactory[M](mapping)






	/** Factory for ComponentValues instances associated with the given mapping.
	  * @param mapping associated mapping
	  * @tparam M static type parameter of returned ComponentValues instances.
	  */
	final class ComponentValuesFactory[M <: SingletonMapping](val mapping :M) extends AnyVal {

		/** Returns ComponentValues based on a predefined mapping result. The values for all components will be obtained
		  * by deassembling (picking) their value from the argument based on the function specified by the path to the given component.
		  * Please note that the final result of the mapping might differ from this argument, as the mapping has always the last say
		  * in what value is returned. This method can be used to get easily get the column values for a mapping before inserting/updating
		  * a value, or as fallback values for other, not complete instance.
		  * @param value proposed result of the mapping.
		  */
		@inline def apply(value :M#Subject) :ComponentValues[M] = Predefined(value)

		/** Returns ComponentValues based on a predefined mapping result. Only the components listed in the second argument
		  * will be deassembled, and predefined() calls for any other will always return None, so that their value is assembled
		  * from lower level components. Useful to avoid ambiguous cases where values are present both for a component and its parent
		  * (see class description). Please not that in particular the value for the associated is NOT predefined, and will be actually assembled.
		  * @param value input for values of the given components.
		  * @param components list of comonents which should be used as sources in the assembly process.
		  */
		@inline def apply(value :M#Subject, components :Seq[AnyComponent[M#Owner]]) :ComponentValues[M] =
			new SelectedDisassembledComponentValues[M](mapping, value, components)

		/** Returns ComponentValues using the given function as the source of values for components. Please note that you can supply a Map
		  * as an argument.
		  * @param values factory of values for components, should always return the value of the type correct for the component argument!
		  * @return
		  */
		def apply(values :AnyComponent[M#Owner]=>Option[_]) :ComponentValues[M] =
			new CustomComponentValues[M](mapping, values)

		/** Create ComponentValues for the given mapping and its value. All values returned by this instance will use the path
		  * to the requested component to pick (dassemble) the value from the given argument. Please not that, as the mapping process
		  * always passes the ComponentValues to the mapping, the returned value might not be this argument, if the mapping decides
		  * to change it. This is the generic version
		  * @param value result, top-level value.
		  */
		@inline def Predefined(value :M#Subject) :DedicatedMappingValue[M] =
			new ExplicitMappingValue[M](mapping, value)

		/** Return a predefined ComponentValues for the associated mapping. All predefined() calls on the returned instance
		  * and any child ComponentValues will return a value deassembled from this value using the path to the requested component
		  * as the getter. The value will be computed lazily when any component is accessed, but at most once.
		  * @param value expression returning proposed value for the mapping.
		  */
		@inline def Lazy(value : =>M#Subject) :DedicatedMappingValue[M] = ComponentValues.Lazy[M](mapping, value)

		/** Return ComponentValues based on a result of a passed by-name expression which will be evaluated at most once, when
		  * a value for the target mapping or some of its components is requested. If the expression returns None, the values
		  * will behave exactly like Empty ComponentValues; if it returnes Some(x), the effect will be the same as with a call
		  * to Predefined(x) / apply(x) or Lazy(x). This method is used primarly for creating second-choice instances to use
		  * as fallback data source for other ComponentValues.
		  */
		@inline def Fallback(value : =>Option[M#Subject]) :ComponentValues[M] = ComponentValues.Fallback[M](mapping, value)

		/** Create an empty instance returning always None/another Empty ComponentValues or throwing NoSuchElementException */
		@inline def Empty :ComponentValues[M] = ComponentValues.Empty[M]

		/** Create an empty instance returning always None/another Empty ComponentValues or throwing NoSuchElementException
		  * @param source description of the originating source creating this instance, usually parent ComponentValues.toString for a more helpful exception message.
		  */
		@inline def Empty(source : =>String) :ComponentValues[M] = ComponentValues.Empty[M](source)
	}






	/** A ComponentValues instance explicitly dedicated to a single Mapping and storing it in a member field.
	  * Note that, while all ComponentValues instance are expected to be created for a particular mapping instance and
	  * in fact even typed with it's singleton type, some implementations may decide to not create new children ComponentValues
	  * for components of the associated mapping instead casting themselves for performance reasons if it would be sufficient,
	  * and it is the main reason while the new methods defined here are not present in the parent trait.
	  */
	trait DedicatedMappingValue[M <: SingletonMapping] extends ComponentValues[M] {
		/** Mapping instance for these values. */
		val mapping :M

		/** The result of the mapping, by delegating to the underlying mapping(this) (which will usually delegate to this instance result(mapping) in turn).
		  * @throws NoSuchElementException (or any other exception thrown by mapping.apply) if the value cannot be assembled from the data in this instance.
		  */
		@inline final def apply() :M#Subject = value(mapping)

		/** The result of the mapping, via mapping.optionally(this) (which will usually delegate to this.result(mapping) in trun).
		  * Please note that this method doesn't swallow any exceptions and leaves it to the mapping to decide if None should be returned
		  * or an exception thrown.
		  * @return Some(value) if it could be assembled from this instance or None
		  */
		@inline final def get :Option[M#Subject] = getValue(mapping)

		/** Makes these values stick and ignore all requests for morphing (including component subselection) until finally asked for
		  * a value for this instance's mapping.
		  * Each time a value/values are requested for a mapping/component, returned values will check if the argument is equal to this instance's mapping
		  * and if so, will return this instance, otherwise itself. This is useful when we want to preset component values for a component
		  * further down the mapping hierarchy, without knowing its lifted representation. Can be combined with orElse to provide
		  * preset values for selected subcomponenents and fallback to default algorithm for all other.
		  */
		@inline final def sticky :ComponentValues[M] = stickTo(mapping)

		override def value(root: M): M#Subject = root.apply(downcast[root.type])

		override def getValue(root: M): Option[M#Subject] = root.optionally(downcast[root.type])


//		def map[T](fun :M#Subject=>T) :Option[T] = get.map(fun)
//
//		def flatMap[T](fun :M#Subject=>Option[T]) :Option[T] = get.flatMap(fun)



		@inline final protected def pathTo[T](component :Component[M#Owner, T]) :TypedComponentPath[M, component.type, T] =
			(mapping \\ mine(component)).asInstanceOf[TypedComponentPath[M, component.type, T]]

		@inline final protected def pathOf[C <: SingletonComponent[M#Owner, T], T](component :C) :TypedComponentPath[M, C, T] =
			(mapping \\ mine(component)).asInstanceOf[TypedComponentPath[M, C, T]]

		@inline final protected def mine[T](component :Component[M#Owner, T]) :mapping.Component[T] =
			component.asInstanceOf[mapping.Component[T]]

		@inline final protected def cast[T](component :M#AnyComponent) :M#Component[T] =
			component.asInstanceOf[M#Component[T]]

		@inline final protected def selftyped :DedicatedMappingValue[mapping.type] =
			this.asInstanceOf[DedicatedMappingValue[mapping.type]]

	}






	/** Skeleton implementation of most methods by delegating to values() methods, which are left for the implementations. */
	trait AbstractComponentValues[M <: SingletonMapping] extends ComponentValues[M] {

		override def apply[T](component: Component[M#Owner, T]): T =
			get(component) getOrElse {
				throw new NoSuchElementException(s"$this: $component")
			}


		override def get[T](component: Component[M#Owner, T]): Option[T] =
			(this :\ component).getValue(component)



		override def apply[C <: SingletonMapping](path: M \:\ C): C#Subject =
			get(path) getOrElse {
				throw new NoSuchElementException(s"$this :$path")
			}


		override def get[C <: SingletonMapping](path: M \:\ C): Option[C#Subject] =
			(this \ path).getValue(path.end)


		override def orElse(values: ComponentValues[M]): ComponentValues[M] =
			new FallbackComponentValues[M](this, values)

//		override def morph[C<:AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
//			new MorphedValues[M, C](this, morphism)
	}


	/** Base class for 'sticky' proxies, which will always wrap transformed backing ComponentValues in a proxy
	  * via factory method implemented by subclasses.
	  */
	abstract class ComponentValuesProxy[M <: SingletonMapping](val mapping :M, values :ComponentValues[M])
		extends AbstractComponentValues[M] with DedicatedMappingValue[M]
	{
		override def result(root: M): Option[M#Subject] =
			if (root eq mapping) values.result(root)
			else
				throw new IllegalArgumentException(s"Passed mapping $root is different than the dedicated mapping $mapping.")

		override private[schema] def predefined(root: M): Option[M#Subject] = values.predefined(root)


		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] =
			direct(pathOf[C, T](component))

/*
		override def values[C <: AnyMapping](path: ComponentPath[M, C]): ComponentValues[C] =
			proxy[C](path.end, values.values[C](path), path.morphism)

		override def direct[C <: M#Component[T], T](path: TypedComponentPath[M, C, T]): ComponentValues[C] =
			proxy[C](path.end, values.direct[C, T](path), path.morphism)


		override def identical[C <: CompatibleMapping[M]](mapping: C): ComponentValues[C] =
			proxy[C](mapping, values.identical[C](mapping), MappingMorphism.identity(this.mapping, mapping))



		override def morph[C <: AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
			proxy[C](morphism.target, values.morph(morphism), morphism)

		def proxy[X<:AnyMapping](target :X, values :ComponentValues[X], morphism :MappingMorphism[M, X]) :ComponentValues[X]
*/
	}





	private class FallbackComponentValues[M <: SingletonMapping]
	                                     (overrides :ComponentValues[M], fallback :ComponentValues[M])
		extends AbstractComponentValues[M]
	{
		override def predefined(root: M): Option[M#Subject] = overrides.predefined(root)

		override def result(root: M): Option[M#Subject] =
			overrides.predefined(root) orElse root.assemble(this.downcast[root.type]) orElse fallback.predefined(root)

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] =
			new FallbackComponentValues[C](overrides.values[C, T](component), fallback.values[C, T](component))

		override def values[C <: SingletonMapping](path: M \:\ C): ComponentValues[C] =
			new FallbackComponentValues[C](overrides.values(path), fallback.values(path))

		override def direct[C <: SingletonComponent[M#Owner, T], T](path :TypedComponentPath[M, C, T]) :ComponentValues[C] =
			new FallbackComponentValues[C](overrides.direct(path), fallback.direct(path))

		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] = {
			val _1 = overrides.identical[C](mapping)
			val _2 = fallback.identical[C](mapping)
			if ((_1 eq overrides) && (_2 eq fallback)) crosscast[C]
			else new FallbackComponentValues[C](_1, _2)
		}


		override def toString = s"$overrides orElse $fallback"
	}







/*
	class MorphedValues[X<:AnyMapping, Y<:AnyMapping](source :ComponentValues[X], morphism :MappingMorphism[X, Y])
		extends AbstractComponentValues[Y] with DedicatedMappingValue[Y]
	{
		override val mapping = morphism.target

		override def result(root: Y): Option[Y#Subject] =
			predefined(root) orElse root.assemble(crosscast[root.type])

		override private[schema] def predefined(root: Y): Option[Y#Subject] =
			source.predefined(morphism.source).flatMap(morphism.value(_))


		override def get[T](component: Y#Component[T]): Option[T] =
			morphism.components(component).flatMap(source.get(_))

		override def values[C <: Y#Component[T], T](component: C): ComponentValues[C] =
			direct(pathOf[C, T](component))


		override def values[C <: AnyMapping](path: ComponentPath[Y, C]): ComponentValues[C] =
			path.walk(this)

		override def direct[C<:Y#Component[T], T](path :TypedComponentPath[Y, C, T]) :ComponentValues[C] =
			morphism.components[T](path.end).map{ src =>
				val backing = source :\ src
				backing.predefined(src).map(v => Predefined[C](path.end, v)) getOrElse {
					morph[C](path.morphism) orElse Fallback[C](path.end, backing.result(src))
				}
				//				(source :\ src).result(src).map(v => Predefined[C](path.end, v))
			} getOrElse Empty[C]

		override def identical[C <: CompatibleMapping[Y]](mapping: C): ComponentValues[C] =
			new MorphedValues[X, C](source, morphism.to[C](mapping))

		override def morph[C<:AnyMapping](next: MappingMorphism[Y, C]): ComponentValues[C] =
			new MorphedValues[X, C](source, morphism andThen next)

		override def toString = s"$source for $mapping"
	}
*/










	trait PredefinedMappingValue[M <: SingletonMapping]
		extends DedicatedMappingValue[M] with AbstractComponentValues[M]
	{
		def value :M#Subject

		final def predefined(root :M) = Some(value)

		final def result(root :M) = Some(value)

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] =
			values(pathOf[C, T](component))

		override def values[C <: SingletonMapping](path: M \:\ C): ComponentValues[C] =
			path.pick(value).map { v => Predefined[C](path.end, v) } getOrElse Empty[C]

		override def direct[C <: SingletonComponent[M#Owner, T], T](path :TypedComponentPath[M, C, T]) :ComponentValues[C] =
			values(path)

//		override def morph[C<:AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
//			morphism.value(value).map(new ExplicitMappingValue[C](morphism.target, _)) getOrElse Empty[C]


		override def toString = s"$mapping := $value"
	}



	class ExplicitMappingValue[M <: SingletonMapping](val mapping :M, val value :M#Subject)
		extends PredefinedMappingValue[M]
	{
		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			new ExplicitMappingValue[C](mapping, value)
	}


	class LazyPredefinedMappingValue[M <: SingletonMapping](val mapping :M, expr : =>M#Subject)
		extends PredefinedMappingValue[M]
	{
		lazy val value :M#Subject = expr

		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			new LazyPredefinedMappingValue[C](mapping, value)

//		override def morph[C<:AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
//			morphism.value.ifSubclass[ValueHomomorphism[M#Subject, C#Subject]] {
//				mono => new LazyPredefinedMappingValue[C](morphism.target, mono.map(value))
//			} getOrElse super.morph[C](morphism)

	}


	class LazyMappingValue[M <: SingletonMapping](val mapping :M, expr : =>Option[M#Subject])
		extends DedicatedMappingValue[M] with AbstractComponentValues[M]
	{
		private[this] lazy val value = expr

		override def result(root: M): Option[M#Subject] = value
		override private[schema] def predefined(root: M): Option[M#Subject] = value

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] =
			values(pathOf[C, T](component))

		override def values[C <: SingletonMapping](path: M \:\ C): ComponentValues[C] =
			new LazyMappingValue[C](path.end, value.flatMap(path.pick))

		override def direct[C <: SingletonComponent[M#Owner, T], T](path: TypedComponentPath[M, C, T]): ComponentValues[C] =
			values(path)

		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			new LazyMappingValue[C](mapping, value)

//		override def morph[C <: AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
//			new LazyMappingValue[C](morphism.target, value.flatMap(morphism.value(_)))

		override def toString = s"$mapping := $value"
	}




	trait PresetComponentValues[M <: SingletonMapping] extends AbstractComponentValues[M] {
		override private[schema] def predefined(root: M): Option[M#Subject] =
			preset(root).flatMap(_.predefined(root))

		override def result(root: M): Option[M#Subject] =
			preset(root).flatMap(_.result(root))

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] =
			preset(component) getOrElse crosscast[C]

		override def values[C <: SingletonMapping](path: ComponentPath[M, C]): ComponentValues[C] =
			path.walk(this)

		override def direct[C <: SingletonComponent[M#Owner, T], T](path: TypedComponentPath[M, C, T]): ComponentValues[C] =
			preset(path.end) getOrElse crosscast[C]

		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			preset(mapping) getOrElse crosscast[C]


//		override def morph[C <: AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] =
//			preset(morphism.target) getOrElse crosscast[C]

		protected def preset[C <: SingletonMapping](component :C) :Option[ComponentValues[C]]

	}


	class StickyComponentValues[M <: SingletonMapping] private[ComponentValues] (values :Map[AnyMapping, ComponentValues[_]])
		extends PresetComponentValues[M]
	{
		def this(mapping :M, values :ComponentValues[M]) = this(Map(mapping->values))

		override protected def preset[C <: SingletonMapping](component: C): Option[ComponentValues[C]] =
			values.get(component).asInstanceOf[Option[ComponentValues[C]]]

		override def toString :String = values.toStream.map(v => v._1+"->"+v._2).mkString("${", ",", "}")
	}





	trait SelectedComponentValues[M <: SingletonMapping] extends AbstractComponentValues[M] with DedicatedMappingValue[M] {
		override private[schema] def predefined(root: M): Option[M#Subject] = None

		override def result(root: M): Option[M#Subject] =
			root.assemble(crosscast[root.type])

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] = ???
//			defined[T](component).map(Predefined[C](component, _)) getOrElse {
//				morph[C](pathOf[C, T](component).morphism)
//			}

		override def values[C <: SingletonMapping](path: ComponentPath[M, C]): ComponentValues[C] = path.walk(this)

		override def direct[C <: SingletonComponent[M#Owner, T], T](path :TypedComponentPath[M, C, T]) :ComponentValues[C] = ???
//			defined(path.end).map(Predefined[C](path.end, _)) getOrElse {
//				morph[C](path.morphism)
//			}

		protected def defined[T](component :Component[M#Owner, T]) :Option[T]

	}







	class SelectedDisassembledComponentValues[M <: SingletonMapping]
	                                         (val mapping :M, value :M#Subject, components :Seq[AnyComponent[M#Owner]])
		extends SelectedComponentValues[M]
	{
		override protected def defined[T](component: Component[M#Owner, T]): Option[T] =
			pathTo(component).pick(value).orNoneUnless(components.contains(component))


		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			new SelectedDisassembledComponentValues[C](mapping, value, components)

		override def toString :String =
			components.map(c => c + "=" + get(c).mapOrElse(_.toString, "?")).mkString(s"[$mapping]{", ", ", "}")
	}


	class CustomComponentValues[M <: SingletonMapping](val mapping :M, vals :AnyComponent[M#Owner]=>Option[_])
		extends SelectedComponentValues[M]
	{
		override protected def defined[T](component: Component[M#Owner, T]): Option[T] =
			vals(component).crosstyped[T]

		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] =
			new CustomComponentValues[C](mapping, vals)

		override def toString = s"[$mapping]{<fun>}"
	}






	private[schema] class EmptyValues[M <: SingletonMapping](source : =>String) extends ComponentValues[M] {
		def this() = this("Empty")


		override def predefined(root: M): Option[M#Subject] = None

		override def result(root: M): Option[M#Subject] = None


		override def apply[T](component: Component[M#Owner, T]): T =
			throw new NoSuchElementException(s"$source: $component")

		override def get[T](component: Component[M#Owner, T]): Option[T] = None

		override def values[C <: SingletonComponent[M#Owner, T], T](component: C): ComponentValues[C] = crosscast[C]


		override def apply[C <: SingletonMapping](path: M \:\ C): C#Subject =
			throw new NoSuchElementException(s"$source: $path")

		override def get[C <: SingletonMapping](path: M \:\ C): Option[C#Subject] =
			None

		override def values[C <: SingletonMapping](path: M \:\ C): ComponentValues[C] =
			crosscast[C]


		override def direct[C <: SingletonComponent[M#Owner, T], T](path :TypedComponentPath[M, C, T]) :ComponentValues[C] =
			crosscast[C]


		override def orElse(values: ComponentValues[M]): ComponentValues[M] = values



		override def identical[C <: CompatibleMapping[M] with Singleton](mapping: C): ComponentValues[C] = crosscast[C]


//		override def morph[C<:AnyMapping](morphism: MappingMorphism[M, C]): ComponentValues[C] = crosscast[C]


		override def toString :String = source
	}

	private[this] val empty :ComponentValues[Nothing] = new EmptyValues[Nothing]



}

