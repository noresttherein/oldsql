package net.noresttherein.oldsql.haul

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.haul.ColumnValues.{AliasedColumnValues, DedicatedColumnValues, EmptyValues, FallbackColumnValues}
import net.noresttherein.oldsql.haul.ComponentValues.{aliasing, selectiveAliasing, AliasedComponentValues, ChosenDisassembledValues, ComponentValuesAliasing, ComponentValuesMap, ComponentValuesNaturalMap, DedicatedComponentValues, DisassembledValues, FallbackValues, GlobalComponentValues, IndexedValues, LazyComponentValuesProxy, MappingAliasing, SimpleComponentValues, TypedValues, UntypedValues}
import net.noresttherein.oldsql.morsels.generic.{=#>, Self}
import net.noresttherein.oldsql.schema.ColumnMapping
import net.noresttherein.oldsql.schema.ColumnMapping.SimpleColumn
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bits.MappingPath.ComponentPath






/** A `ComponentValues` specialization which contains values only for columns. Its primary use is declarative
  * in nature, simplifying handling and allowing optimizations in other places.
  * @see [[net.noresttherein.oldsql.haul.ColumnValues.ColumnValuesAdapter]] a stackable mix-in
  *      overriding the `preset` method.
  */
trait ColumnValues[S, O] extends ComponentValues[S, O] { outer =>

	override def /[T](extract :Extract[T]) :ColumnValues[T, O] = this / extract.export

	override def /[T](path :ComponentPath[_ <: RefinedMapping[S, O], _ <: Component[T], S, T, O])
			:ColumnValues[T, O] =
		path.carry(this)

	override def /[T](component :Component[T]) :ColumnValues[T, O]


	override def orElse(values :ComponentValues[S, O]) :ComponentValues[S, O] = values match {
		case cols :ColumnValues[S @unchecked, O @unchecked] => orElse(cols)
		case _ => super.orElse(values)
	}

	def orElse(values :ColumnValues[S, O]) :ColumnValues[S, O] = values match {
		case _ :EmptyValues[_, _] => this
		case _ => new FallbackColumnValues(this, values)
	}


	override def ++(values :ComponentValues[S, O]) :ComponentValues[S, O] = values match {
		case cols :ColumnValues[S @unchecked, O @unchecked] => this ++ cols
		case _ => super.++(values)
	}

	def ++(values :ColumnValues[S, O]) :ColumnValues[S, O] = orElse(values)


	override def aliased(root :RefinedMapping[S, O]) :ColumnValues[S, O] = aliased(aliasing(root))

	override def aliased[T](extracts :NaturalMap[MappingAt[O]#Component, RefinedMapping[T, O]#Extract])
			:ColumnValues[S, O] =
		aliased(aliasing(extracts))

	override def aliased(aliases :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValues[S, O] =
		new AliasedColumnValues(this, aliases)

	override def selectivelyAliased(root :RefinedMapping[S, O]) :ColumnValues[S, O] =
		aliased(selectiveAliasing(root))

	override def selectivelyAliased[T](extracts :NaturalMap[MappingAt[O]#Component, RefinedMapping[T, O]#Extract])
			:ColumnValues[S, O] =
		aliased(selectiveAliasing(extracts))

	override def tiedTo[X](mapping :RefinedMapping[S, O]) :ColumnValues[X, O] = {
		val assoc = NaturalMap.single[MappingAt[O]#Component, ColumnValues.WithOrigin[O]#T, S](mapping, this)
		new DedicatedColumnValues[X, O](assoc)
	}


	override def clone() :ColumnValues[S, O] = super[ComponentValues].clone().asInstanceOf[ColumnValues[S, O]]


	/** Shortcut for casting the type parameter to a different type reserved for the implementations */
	@inline final private[haul] def asColumnsOf[X] :ColumnValues[X, O] =
		this.asInstanceOf[ColumnValues[X, O]]


	protected override def publicClassName = "ColumnValues"
}






object ColumnValues {
	type WithOrigin[O] = { type T[S] = ColumnValues[S, O] }

	/** Returns `ColumnValues` using the given generic function as the source of values for columns.
	  * The return values of the function are used as the preset values yielded by the created instance.
	  * Note that no component aliasing will take place, which can result in non-operative version of a component
	  * being used and a failed assembly, or a result based on incorrect buffs, and thus it should be taken care of
	  * by the function itself. You may wish to consider using `ColumnValues(mapping)(values)` instead,
	  * which is compatible with any mapping. You can supply a `NaturalMap` as an argument.
	  * @param values factory of values for columns.
	  */
	def apply[S, O](values :MappingAt[O]#Column =#> Self) :ColumnValues[S, O] = values match {
		case map :NaturalMap[MappingAt[O]#Column @unchecked, Self @unchecked] =>
			new ColumnValuesNaturalMap(map)
		case _ =>
			new TypedValues[S, O](values.asInstanceOf[NaturalMap[MappingAt[O]#Component, Self]])
				with ColumnValuesAdapter[S, O] with GlobalColumnValues[S, O] with ImmutableColumnValues[S, O]
	}

	/** Returns `ColumnValues` using the given function as the source of values for columns.
	  * The return values of the function are used as the preset values yielded by the created instance.
	  * Note that no component aliasing will take place, which can result in non-operative version of a component
	  * being used and a failed assembly or a result based on incorrect buffs and thus it should be taken care of
	  * by the function itself. You may wish to consider using `ColumnValues(mapping)(values)` instead,
	  * which is compatible with any mapping. You can supply a `Map` as an argument.
	  * @param values factory of values for columns, must return an option of the same type as the input mapping.
	  */
	def apply[S, O](values :ColumnMapping[_, O] => Any) :ColumnValues[S, O] = values match {
		case map :Map[_, _] =>
			new ColumnValuesMap(map.asInstanceOf[Map[ColumnMapping[_, O], Any]])
		case _ =>
			new UntypedValues[S, O](values.asInstanceOf[Map[RefinedMapping[_, O], Any]])
				with ColumnValuesAdapter[S, O] with GlobalColumnValues[S, O] with ImmutableColumnValues[S, O]
	}


	/** Returns `ColumnValues` wrapping the given set of values with their assignment to columns happening
	  * by the use of the provided indexing function. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs.
	  * @param values preset values for selected columns, in any order consistent with `index`.
	  * @param index A function returning the index of the value for the column in the given sequence.
	  *              If the column has no preset value, it should return a negative number.
	  */
	def apply[S, O](values :IndexedSeq[Any])(index :ColumnMapping[_, O] => Int) :ColumnValues[S, O] =
		new IndexedValues[S, O](values, index.asInstanceOf[MappingAt[O] => Int])
			with ColumnValuesAdapter[S, O]with GlobalColumnValues[S, O] with ImmutableColumnValues[S, O]


	/** Creates a `ColumnValues` instance with a given preset for the specified mapping. If non-empty,
	  * the values for subcomponents will be extracted from it using the `MappingExtract`s supplied by the mapping.
	  * This is equivalent to `ColumnValues(mapping, result.get)` or `ColumnValues.empty`, based
	  * on whether the `result` is defined.
	  */
	def apply[S, O](mapping :RefinedMapping[S, O], result :Option[S]) :ColumnValues[S, O] = result match {
		case Some(value) => apply(mapping, value)
		case _ => empty[S, O]
	}


	/** Create `ComponentValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by the given mapping to extract the value from the given argument.
	  * You may wish to consider using `ComponentValues(mapping)(value)` instead, which follows exactly the general
	  * `ComponentValues` contract. Note that no component aliasing will take place,
	  * which can result in non-operative version of a component being used and a failed assembly or a result
	  * based on incorrect buffs.
	  * @param value the result, top-level value.
	  */
	def apply[S, O](mapping :RefinedMapping[S, O], value :S) :ColumnValues[S, O] =
		new DisassembledValues[S, S, O](mapping, value)
			with ColumnValuesAdapter[S, O] with GlobalColumnValues[S, O] with ImmutableColumnValues[S, O]

	/** Create `ColumnValues` for the given mapping and its subject. All values returned by this instance will use
	  * the `MappingExtract` provided by the given mapping to extract the value from the given argument.
	  * Unlike `ColumnValues(mapping, value)`, only the columns listed in the third argument will have their value
	  * preset. Note that no component aliasing will take place, which can result in non-operative version
	  * of a component being used and a failed assembly or a result based on incorrect buffs.
	  * You may wish to consider using `ColumnValues(mapping)(value, components)` instead, which follows exactly
	  * the general `ColumnValues` contract.
	  * @param value the result, top-level value.
	  */
	def apply[S, O](mapping :RefinedMapping[S, O], value :S, components :Unique[ColumnMapping[_, O]])
			:ColumnValues[S, O] =
		new ChosenDisassembledValues[S, S, O](mapping, value, components)
			with ColumnValuesAdapter[S, O] with GlobalColumnValues[S, O] with ImmutableColumnValues[S, O]


	/** An empty instance, returning always `Lack` or throwing `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ColumnValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  */
	def apply[S, O]() :ColumnValues[S, O] = EmptyValues.asInstanceOf[ColumnValues[S, O]]

	/** An empty instance, returning always `Lack` or throwing `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ColumnValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  */
	def empty[S, O] :ColumnValues[S, O] = EmptyValues.asInstanceOf[ColumnValues[S, O]]

	/** An empty instance, returning always `Lack` or throwing a `NoSuchElementException`. This instance does ''not''
	  * perform aliasing in order to obtain the operative version of the components before calling their
	  * `optionally`, `apply`, `assemble` methods, which may result in incorrect result in the cases where
	  * some buffs are inherited from an owning mapping (or it is modified in any other way for the purpose
	  * of the component). You may wish to consider using `ColumnValues(mapping)()` instead, which is compatible
	  * with any mapping.
	  * @param source a description of the original source which created this instance for more helpful message in
	  *               thrown exceptions
	  */
	def empty[S, O](source : => String) :ColumnValues[S, O] = new EmptyValues[S, O](source)


	/** Create `ColumnValues` with a value preset for a single column. This is different to the disassembling
	  * values `ColumnValues(component, value)` in that it can be created for an arbitrary root subject type `R`,
	  * and the presets for any mappings other than this column are always immediately stated as `Lack`. This makes
	  * it particularly suited for an override such as `ColumnValues(column, value) orElse defaults`,
	  * or joining with other values with `++`. Note that no component aliasing takes place; the column provided
	  * here must be the column which is going to be used as the argument, unless this instance is treated only
	  * as intermediate data to be combined with others.
	  * @param column a column of the root mapping of `R`.
	  * @param value the preset for the specified column.
	  */
	def preset[R, S, O](column :ColumnMapping[S, O], value :S) :ColumnValues[R, O] =
		new PresetColumnValue(column, Got(value))

	/** Create `ColumnValues` with a value preset for a single column. This is different to the disassembling
	  * values `ColumnValues(component, value)` in that it can be created for an arbitrary root subject type `R`,
	  * and the presets for any mappings other than this column are always immediately stated as `Lack`. This makes
	  * it particularly suited for an override such as `ColumnValues(column, value) orElse defaults`,
	  * or joining with other values with `++`. Note that no component aliasing takes place; the column provided
	  * here must be the column which is going to be used as the argument, unless this instance is treated only
	  * as intermediate data to be combined with others.
	  * @param column a column of the root mapping of `R`.
	  * @param value the preset for the specified column.
	  * @return `ColumnValues.empty` or `ColumnValues.preset(column, value.get)`, depending on whether the value
	  *        is empty.
	  */
	def presetOpt[S, O](column :ColumnMapping[S, O], value :Opt[S]) :ColumnValues[S, O] =
		if (value.isEmpty) empty
		else new PresetColumnValue(column, value)


	/** Similar to `preset[S, O](mapping, value)`, but the value is not computed until actually needed. The expression
	  * will be evaluated at most once. Note that no component aliasing takes place; the column provided
	  * here must be the column which is going to be used as the argument, unless this instance is treated only
	  * as intermediate data to be combined with others.
	  * @param column a column of the root mapping of `R`.
	  * @param value the preset for the specified column.
	  */
	def later[R, S, O](column :ColumnMapping[S, O], value: => S) :ColumnValues[R, O] =
		new LazyColumnValue(column, () => Some(value))

	/** Create a lazy, preset instance using the value of the given expression for the component.
	  * Similar to `later`, but the expression might not produce a value (return `Lack`), and used generally
	  * as a fallback default value for when the first choice couldn't be obtained.
	  * Note that no component aliasing takes place; the column provided
	  * here must be the column which is going to be used as the argument, unless this instance is treated only
	  * as intermediate data to be combined with others.
	  * @param column a column of the root mapping of `R`.
	  * @param value the preset for the specified column.
	  */
	def fallback[S, O](column :ColumnMapping[S, O], value: => Opt[S]) :ColumnValues[S, O] =
		new LazyColumnValue(column, () => value.toOption)


	/** A proxy `ColumnValues` which delegates all assembly-related and '/' calls to the lazily computed
	  * instance initialized with the by-name parameter.
	  */
	def delay[S, O](values: => ColumnValues[S, O]) :ColumnValues[S, O] =
		new LazyComponentValuesProxy[S, O](values) with ColumnValuesAdapter[S, O] {
			override def /[T](component :Component[T]) :ColumnValues[T, O] =
				backing.asInstanceOf[ColumnValues[S, O]] / component

			override def ++(other :ColumnValues[S, O]) =
				backing.asInstanceOf[ColumnValues[S, O]] ++ other
		}


	/** Creates a new builder assembling a `ColumnValues` instance from the provided values for individual columns. */
	def newBuilder[S, O] :ColumnValuesBuilder[S, O] = new ColumnValuesMapBuilder[S, O]



	/** Factory for `ColumnValues` which performs aliasing of all passed components before checking
	  * for preset values or using them for assembly. They guarantee that the operative version of the component
	  * is being used, with correct column names and buffs reflecting any changes imposed by their owners.
	  * Should be the first choice for obtaining `ColumnValues` instances.
	  * You can follow this application for additional argument list(s) to create `ColumnValues` instances:
	  * {{{
	  *     ColumnValues(mapping)(subject) //provide a preset value for the mapping and its subcomponents
	  *     ColumnValues(mapping) { comp => Option(value) } //provide a function with optional preset component values
	  *     ColumnValues(mapping)(values) { comp => index(comp) } //provide a sequence of values and an index function
	  * }}}
	  */
	@inline def apply[S, O](mapping :RefinedMapping[S, O]) :ColumnValuesFactory[S, O] =
		new ColumnValuesFactory[S, O](mapping)


	/** Factory for `ColumnValues` instances associated with the given mapping.
	  * @param mapping a mapping for which `ColumnValues` are begin created.
	  * @tparam S the subject type of the associated 'root' mapping.
	  * @tparam O the origin type shared by the associated mapping and its subcomponents.
	  */
	class ColumnValuesFactory[S, O](private val mapping :RefinedMapping[S, O]) extends AnyVal { factory =>

		/** Returns `ColumnValues` based on a predefined mapping result. The values for all columns will be
		  * obtained by disassembling (extracting) their value from the argument based on the function specified by
		  * the `MappingExtract` provided by the mapping. The values for all other components however, including
		  * the root mapping, will be assembled using the normal process.
		  * Before assembly, every component is aliased using the [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method of the mapping provided earlier. This method can be used to easily get the column values
		  * for a mapping before inserting/updating a value, or as fallback values for another, not complete instance.
		  * @param value proposed result of the mapping.
		  */
		def apply(value :S) :ColumnValues[S, O] =
			new DisassembledValues[S, S, O](mapping, value)
				with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
				with ColumnValuesAdapter[S, O] with ImmutableColumnValues[S, O]
			{
				override val mapping = factory.mapping
			}

		/** Returns `ColumnValues` based on a predefined mapping result. If it is not empty, the values
		  * for all columns will be obtained by disassembling (extracting) their value from the argument based on
		  * the function specified by the `MappingExtract` provided by the mapping. The values
		  * for all other components however will be assembled from lower-level components in the normal process.
		  * This method is equivalent to either `ColumnValues(mapping)(result.get)` or `ColumnValues(mapping)()`,
		  * depending on whether `result` is defined.
		  * @param result proposed result of the mapping (the final result is that of the mapping's `optionally` method).
		  */
		def apply(result :Option[S]) :ColumnValues[S, O] = result match {
			case Some(value) => apply(value)
			case _ => apply()
		}


		/** Returns `ColumnValues` based on a predefined mapping result. The columns listed in the second
		  * argument will be have their values disassembled (extracted from the preset value) using
		  * a `MappingExtract` supplied by the mapping provided earlier, while `preset()` calls for all other
		  * columns and components will always return `Lack`, so that their value is assembled from lower level
		  * components. All components used for assembly are aliased with the mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before invoking their `optionally`
		  * (or `apply`) method.
		  * @param value      subject of the root mapping serving as input for values of the given columns.
		  * @param components list of columns which should be used as the value set in the assembly process.
		  */
		def apply(value :S, components :Unique[ColumnMapping[_, O]]) :ColumnValues[S, O] =
			new ChosenDisassembledValues[S, S, O](mapping, value, components)
				with ColumnValuesAdapter[S, O] with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
				with ImmutableColumnValues[S, O]
			{
				override val mapping = factory.mapping
			}


		/** Returns `ColumnValues` using the given generic function as the source of values for columns.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with the presets for all non-column components being `Lack` by contract.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods. You can supply a `NaturalMap` as an argument.
		  * @param values factory of values for columns.
		  */
		def apply(values :MappingAt[O]#Column =#> Self) :ColumnValues[S, O] = values match {
			case map :NaturalMap[MappingAt[O]#Column @unchecked, Self @unchecked] =>
				new ColumnValuesNaturalMap[S, O](map) with ColumnValuesAliasing[S, O] with MappingAliasing[S, O] {
					override val mapping = factory.mapping
				}
			case _ =>
				new TypedValues[S, O](values.asInstanceOf[MappingAt[O]#Component =#> Self])
					with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
					with ColumnValuesAdapter[S, O] with ImmutableColumnValues[S, O]
				{
					override val mapping = factory.mapping
				}
		}

		/** Returns `ColumnValues` using the given function as the source of values for columns.
		  * The return values of the function are used as the preset values yielded by the created instance,
		  * with the presets for non-column components being `Lack` by contract.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods. You can supply a `Map` as an argument.
		  * @param values factory of values for columns, must return an option of the input mapping's subject type.
		  */
		def apply(values :ColumnMapping[_, O] => Any) :ColumnValues[S, O] = values match {
			case map :Map[_, _] =>
				new ColumnValuesMap[S, O](map.asInstanceOf[Map[ColumnMapping[_, O], Any]])
					with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
				{
					override val mapping = factory.mapping
				}
			case _ =>
				new UntypedValues[S, O](values.asInstanceOf[RefinedMapping[_, O] => Any])
					with ColumnValuesAdapter[S, O] with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
				{
					override val mapping = factory.mapping
				}
		}


		/** Returns `ColumnValues` wrapping the given set of values with their assignment to columns happening
		  * by the use of the provided indexing function. An entry of `null` or a negative index results in
		  * no preset value for the column, with the presets for all non-column components being `Lack` by contract.
		  * All components passed to the created instance will be aliased using the supplied mapping's
		  * [[net.noresttherein.oldsql.schema.Mapping.export export]] method before being passed to the given function
		  * or calls to their methods.
		  * @param values preset values for selected columns of the supplied mapping, in any order consistent
		  *               with `index`.
		  * @param index  A function returning the index of the value for the column in the given sequence.
		  *               If the column has no preset value, it should return a negative number.
		  */
		def apply(values :IndexedSeq[Any])(index :MappingAt[O] => Int) :ColumnValues[S, O] =
			new IndexedValues[S, O](values, index)
				with ColumnValuesAdapter[S, O] with ColumnValuesAliasing[S, O] with MappingAliasing[S, O]
				with ImmutableColumnValues[S, O]
			{
				override val mapping = factory.mapping
			}


		/** Create an empty instance returning always `Lack`/another empty `ColumnValues` or throwing
		  * a `NoSuchElementException`. It will print the provided mapping in its `toString` method and
		  * any exception messages resulting from a lack of value. Note that all components passed to this
		  * instance will be aliased with this mapping's [[net.noresttherein.oldsql.schema.Mapping.export export]]
		  * method, which guarantees that the operative version of the component is being used for assembly
		  * (and thus it and its subcomponents have the final set of buffs), but is somewhat less efficient
		  * and might be completely unnecessary in most use cases.
		  */
		def apply() :ColumnValues[S, O] =
			new EmptyValues[S, O](mapping.toString) with ColumnValuesAliasing[S, O] with MappingAliasing[S, O] {
				override val mapping = factory.mapping
			}


		/** Creates a builder of a `ColumnValues` instance by collecting values for individual columns.
		  * All the columns passed to its `add` method are aliased to their export version by the mapping
		  * specified earlier and the created `ComponentValues` will likewise perform component aliasing.
		  */
		def newBuilder :ColumnValuesBuilder[S, O] =
			new ColumnValuesBuilder[S, O] {
				val mapping = factory.mapping
				var map = Map.empty[ColumnMapping[_, O], Any]

				override def addOpt[T](component :ColumnMapping[T, O], result :Opt[T]) = {
					if (result.isDefined)
						map = map.updated(component, result)
					this
				}

				override def result() :ColumnValues[S, O] = {
					val res = map; map = null
					ColumnValues(mapping)(res.withDefaultValue(null))
				}
			}

	}


	private[this] val EmptyValues = new EmptyValues






	trait ColumnValuesBuilder[S, O] {
		@inline final def add[T](component :ColumnMapping[T, O], value :T) :this.type = addOpt(component, Got(value))

		def addOpt[T](component :ColumnMapping[T, O], result :Opt[T]) :this.type

		def result() :ColumnValues[S, O]
	}


	private class ColumnValuesMapBuilder[S, O] extends ColumnValuesBuilder[S, O] {
		private var map = Map.empty[ColumnMapping[_, O], Any]

		override def addOpt[T](column :ColumnMapping[T, O], result :Opt[T]) :this.type = {
			if (result.isDefined)
				map = map.updated(column, result.get)
			this
		}

		override def result() :ColumnValuesMap[S, O] = new ColumnValuesMap(map)
	}






	private trait ImmutableColumnValues[S, O] extends ColumnValues[S, O] {
		override def clone() :ColumnValues[S, O] = this
	}



	/** Base trait for `ColumnValues` implementations which always pass themselves (after casting) to all
	  * components of the associated mapping.
	  */
	trait GlobalColumnValues[S, O] extends GlobalComponentValues[S, O] with ColumnValues[S, O] {
		override def /[T](extract :Extract[T]) :ColumnValues[T, O] = this.asColumnsOf[T]
		override def /[T](component :Component[T]) :ColumnValues[T, O] = this.asColumnsOf[T]
		override def /[T](path :ComponentPath[_ <: RefinedMapping[S, O], _ <: Component[T], S, T, O]) :ColumnValues[T, O] =
			this.asColumnsOf[T]
	}



	//this trait is currently unused, as we'd need to make column's optionally and assemble final
	trait SimpleColumnValues[S, O] extends SimpleComponentValues[S, O] {
		override def preset(root :RefinedMapping[S, O]) :Opt[S] = root match {
			case column :SimpleColumn[S @unchecked, O @unchecked] => defined(column)
			case _ => Lack
		}
	}



	/** A mix-in trait which maps every argument `Component[T]` into a possibly different `Component[T]`
	  * before delegating to the super method. This behaviour is limited only to the components of the associated
	  * mapping: the root mapping itself is not aliased this way. If it should be treated as other components,
	  * the `aliased` property should be used for assembly instead of this instance.
	  * This class is sticky, meaning it returns itself whenever `ColumnValues` for a child component is requested.
	  * This added functionality is used to substitute every component mapping used for assembly with its operative,
	  * `export` version as defined by the root mapping.
	  */
	trait ColumnValuesAliasing[S, O] extends ComponentValuesAliasing[S, O] with GlobalColumnValues[S, O] { outer =>
		override val aliasing :ColumnValues[S, O] = new AliasingValues with ColumnValues[S, O] {
			override def /[T](component :Component[T]) = outer.asColumnsOf[T]

			override def aliased(aliases :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValues[S, O] =
				outer.aliased(aliases).aliasing
		}

		override def aliased(aliases :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValuesAliasing[S, O] =
			new ColumnValuesAliasing[S, O] {
				override protected def alias[T](component :Component[T]) = outer.alias(aliases(component))
				override def preset(root :RefinedMapping[S, O]) = outer.preset(root)
				override def aliased(again :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValuesAliasing[S, O] =
					outer.aliased(again andThen aliases)
				override def toString = outer.toString + ".aliased"
			}

	}



	/** A late mix-in stackable trait adapting a `ComponentValues` implementation to `ColumnValues`.
	  * Overrides the `preset` method to only call super if the argument is a column.
	  */
	trait ColumnValuesAdapter[S, O] extends ColumnValues[S, O] {
		abstract override def preset(root :RefinedMapping[S, O]) :Opt[S] =
			if (root.isInstanceOf[ColumnMapping[_, _]]) super.preset(root)
			else Lack
	}



	/** A `ColumnValues` instance with a preset result (possibly `Lack`) for a single column. It is recognized
	  * by some of the other mapping's `++` methods.
	  */
	trait ColumnValue[S, X, O] extends GlobalColumnValues[S, O] {
		val column :ColumnMapping[X, O]
		def value :Opt[X]

		override def preset(root :RefinedMapping[S, O]) :Opt[S] =
			if (root eq column) value.asInstanceOf[Opt[S]] else Lack

		override def orElse(values :ComponentValues[S, O]) :ComponentValues[S, O] =
			if (value.isEmpty)
				values
			else
				values match {
					case cols :ColumnValues[S @unchecked, O @unchecked] => this orElse cols
					case comps :ComponentValuesMap[S @unchecked, O @unchecked] =>
						if (comps.map.isEmpty) this
						else ComponentValues(comps.map.updated(column, value.get))
					case comps :ComponentValuesNaturalMap[S @unchecked, O @unchecked] =>
						if (comps.map.isEmpty) this
						else ComponentValues(comps.map.updated[Self, X](column, value.get))
					case _ =>
						super.orElse(values)
				}

		override def orElse(values :ColumnValues[S, O]) :ColumnValues[S, O] =
			if (value.isEmpty)
				values
			else
				values match {
					case col :ColumnValue[S @unchecked, x, O @unchecked] =>
						if ((col.column eq column) || col.value.isEmpty)
							this
						else {
							val map = Map.empty[Column[_], Option[_]]
							ColumnValues(map.updated(column, value.get).updated(col.column, col.value.get))
						}
					case cols :ColumnValuesMap[S @unchecked, O @unchecked] =>
						if (cols.map.isEmpty) this
						else ColumnValues(cols.map.updated(column, value.get))
					case cols :ColumnValuesNaturalMap[S @unchecked, O @unchecked] =>
						if (cols.map.isEmpty) this
						else ColumnValues(cols.map.updated[Self, X](column, value.get))
					case _ =>
						super.orElse(values)
				}

		override def ++(values :ComponentValues[S, O]) :ComponentValues[S, O] =
			if (value.isEmpty) values else super.++(values)

		override def ++(values :ColumnValues[S, O]) :ColumnValues[S, O] =
			if (value.isEmpty) values else super.++(values)


		override def tiedTo[T](mapping :RefinedMapping[S, O]) :ColumnValues[T, O] = this.asColumnsOf[T]

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[ColumnValue[_, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :ColumnValue[_, _, _] if other canEqual this => other.column == column && other.value == value
			case _ => false
		}

		override def hashCode :Int = column.hashCode * 31 + value.hashCode
	}



	private class PresetColumnValue[S, T, O](override val column :ColumnMapping[T, O], result :Opt[T])
		extends ColumnValue[S, T, O] with ImmutableColumnValues[S, O] with Serializable
	{
		override def value = result

		override def toString = "{" + column + ":=" + result.getOrElse("-") + "}"
	}


	private class LazyColumnValue[S, T, O](override val column :ColumnMapping[T, O], private var init: () => Option[T])
		extends ColumnValue[S, T, O]
	{
		@volatile private var result :Option[T] = _
		private var cached :Option[T] = _

		override def value :Opt[T] = {
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

		override def clone() :ColumnValues[S, O] =
			if (value.isEmpty) empty
			else new PresetColumnValue(column, value)

		private def writeReplace = new PresetColumnValue(column, value)

		override def toString = {
			val res = result
			if (res != null) {
				cached = res
				"{" + column + ":=" + res.getOrElse("-") + "}"
			} else
				"{" + column + ":=?}"
		}
	}




	private class ColumnValuesNaturalMap[S, O](values :NaturalMap[MappingAt[O]#Column, Self])
		extends ComponentValuesNaturalMap[S, O](values.asInstanceOf[NaturalMap[MappingAt[O]#Component, Self]])
		   with ImmutableColumnValues[S, O] with GlobalColumnValues[S, O]
	{
		private def columnValues :NaturalMap[MappingAt[O]#Column, Self] = values

		override def preset(component :RefinedMapping[S, O]) = component match {
			case col :ColumnMapping[S @unchecked, O @unchecked] =>
				Opt(values.getOrElse[Self, S](col, null.asInstanceOf[S]))
			case _ => Lack
		}

		override def ++(other :ColumnValues[S, O]) = other match {
			case same :ColumnValuesNaturalMap[S @unchecked, O @unchecked] =>
				new ColumnValuesNaturalMap(values ++ same.columnValues)
			case single :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (single.value.isDefined)
					new ColumnValuesNaturalMap(values.updated[Self, t](single.column, single.value.get))
				else this
			case _ => super.++(other)
		}
	}


	private class ColumnValuesMap[S, O](values :Map[ColumnMapping[_, O], Any])
		extends ComponentValuesMap[S, O](values.asInstanceOf[Map[RefinedMapping[_, O], Any]])
		   with ImmutableColumnValues[S, O] with GlobalColumnValues[S, O]
	{
		private def columnValues :Map[ColumnMapping[_, O], Any] = values

		override def preset(component :RefinedMapping[S, O]) :Opt[S] = component match {
			case col :ColumnMapping[S @unchecked, O @unchecked] =>
				Opt(values.getOrElse(col, null)).asInstanceOf[Opt[S]]
			case _ => Lack
		}

		override def ++(other :ColumnValues[S, O]) = other match {
			case same :ColumnValuesMap[S @unchecked, O @unchecked] =>
				new ColumnValuesMap(values ++ same.columnValues)
			case single :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (single.value.isDefined)
					new ColumnValuesMap(values.updated(single.column, single.value.get))
				else this
			case _ => super.++(other)
		}
	}




	private class DedicatedColumnValues[S, O]
	              (values :NaturalMap[MappingAt[O]#Component, WithOrigin[O]#T], default :ColumnValues[S, O] = empty[S, O])
		extends DedicatedComponentValues[S, O](values, default) with ColumnValues[S, O]
	{
		override def presets :NaturalMap[MappingAt[O]#Component, WithOrigin[O]#T] = values
		override def defaults :ColumnValues[S, O] = default

		override def preset(root :RefinedMapping[S, O]) =
			if (root.isInstanceOf[ColumnMapping[_, _]]) {
				val res = values.getOrElse[WithOrigin[O]#T, S](root, null)
				if (res == null) Lack else res.preset(root)
			} else Lack

		override def /[T](component :Component[T]) :ColumnValues[T, O] = values.get(component) match {
			case Some(vals) => vals
			case _ => asColumnsOf[T]
		}

		override def ++(other :ColumnValues[S, O]) = other match {
			case custom :DedicatedColumnValues[S @unchecked, O @unchecked] =>
				new DedicatedColumnValues[S, O](values ++ custom.presets, default ++ custom.defaults)
			case singleton :ColumnValue[S @unchecked, t, O @unchecked] =>
				if (singleton.value.isDefined)
					new DedicatedColumnValues[S, O](
						values.updated[WithOrigin[O]#T, t](singleton.column, singleton.asColumnsOf[t]), default
					)
				else
					this
			case _ => super.++(other)
		}

		override def aliased(aliases :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValues[S, O] =
			new DedicatedColumnValues[S, O](values, default aliased aliases)

		override def tiedTo[X](mapping :RefinedMapping[S, O]) = asColumnsOf[X]

	}



	private class AliasedColumnValues[S, O]
	              (values :ColumnValues[S, O], alias :MappingAt[O]#Component =#> RefinedMapping[_, O]#Component)
		extends AliasedComponentValues[S, O](values, alias) with ColumnValues[S, O]
	{ outer =>
		def this(values :ColumnValues[S, O],
		         extracts :NaturalMap[MappingAt[O]#Component, RefinedMapping[_, O]#Extract]) =
			this(values, aliasing(extracts))

		def this(values :ColumnValues[S, O], root :RefinedMapping[_, O]) =
			this(values, aliasing(root))


		protected override def contents :ColumnValues[S, O] = values

		protected override val noAliasing :ColumnValues[S, O] = new ColumnValues[S, O] {
			override def preset(root :RefinedMapping[S, O]) :Opt[S] = values.preset(root)
			override def /[T](component :Component[T]) :ColumnValues[T, O] = outer / component
		}

		override def /[T](component :Component[T]) :ColumnValues[T, O] = aliases(component) match {
			case null => empty("No export version of component " + component + " in " + this)
			case export =>
				val next = values / export
				if (next eq values) noAliasing.asColumnsOf[T]
			else next.aliased(aliases)
		}

		override def clone() :ColumnValues[S, O] = {
			val clone = values.clone()
			if (clone eq values) this
			else clone.aliased(aliases)
		}

		override def ++(other :ColumnValues[S, O]) = other match {
			case _ :EmptyValues[_, _] => this
			case aliased :AliasedColumnValues[S @unchecked, O @unchecked] if aliases eq aliased.aliases =>
				(values ++ aliased.contents).aliased(aliases)
			case _ => orElse(other)
		}

		override def aliased(export :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValues[S, O] =
			new AliasedColumnValues(values, alias andThen export)
	}



	private class FallbackColumnValues[S, O](overrides :ColumnValues[S, O], fallback :ColumnValues[S, O])
		extends FallbackValues[S, O](overrides, fallback) with ColumnValues[S, O]
	{
		protected def decorate[T](first :ColumnValues[T, O], second :ColumnValues[T, O]) :ColumnValues[T, O] =
			if ((first eq overrides) & (second eq fallback)) asColumnsOf[T]
			else first orElse second

		override def /[T](component :Component[T]) :ColumnValues[T, O] =
			decorate(overrides / component, fallback / component)

		override def /[T](extract :Extract[T]) :ColumnValues[T, O] =
			decorate(overrides / extract, fallback / extract)

		override def clone() :ColumnValues[S, O] = decorate(overrides.clone(), fallback.clone())
	}



	private[haul] class EmptyValues[S, O](source : => String) extends GlobalColumnValues[S, O] {
		def this() = this("Empty")

		def this(mapping :RefinedMapping[S, O]) = this(mapping.toString)

		override def preset(root: RefinedMapping[S, O]): Opt[S] = Lack
		override def assemble(root: RefinedMapping[S, O]): Opt[S] = root.assemble(this)

		override def aliased(root :RefinedMapping[S, O]) :ColumnValues[S, O] = this
		override def aliased(export :MappingAt[O]#Component =#> MappingAt[O]#Component) :ColumnValues[S, O] = this
		override def aliased[T](extracts :NaturalMap[MappingAt[O]#Component, RefinedMapping[T, O]#Extract])
				:ColumnValues[S, O] =
			this

		override def selectivelyAliased(root :RefinedMapping[S, O]) :ColumnValues[S, O] = this
		override def selectivelyAliased[T](extracts :NaturalMap[MappingAt[O]#Component, RefinedMapping[T, O]#Extract])
				:ColumnValues[S, O] =
			this

		override def orElse(values: ComponentValues[S, O]): ComponentValues[S, O] = values
		override def orElse(values :ColumnValues[S, O]) :ColumnValues[S, O] = values


		override def tiedTo[X](mapping :RefinedMapping[S, O]) = asColumnsOf[X]

		override def clone() :ColumnValues[S, O] = {
			val name = source //evaluate the lazy init to lose any references
			if (name == "Empty") ColumnValues.empty[S, O]
			else new EmptyValues(name)
		}

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[EmptyValues[_, _]]

		override def equals(that :Any) :Boolean = that.isInstanceOf[EmptyValues[_, _]]

		override def hashCode :Int = getClass.hashCode

		override def toString :String = source
	}

}

