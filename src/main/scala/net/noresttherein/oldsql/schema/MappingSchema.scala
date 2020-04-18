package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication, ChainConcat}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.{Inc, INT}
import net.noresttherein.oldsql.schema.Mapping.{FreeOriginMapping, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.support.{ConstantMapping, LabeledMapping, LazyMapping, MappedMapping, MappingAdapter}
import net.noresttherein.oldsql.schema.MappingSchema.{ExtensibleNonEmptySchema, FlatMappedMappingSchema, FlatMappingSchema, GetLabeledComponent, GetSchemaComponent, MappedMappingSchema, MappingSchemaGuts, SchemaComponentLabel, SchemaFlattening}
import net.noresttherein.oldsql.schema.support.LabeledMapping.{Label, MappingLabel}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn, LabeledSchemaMapping, SchemaColumn}
import net.noresttherein.oldsql.slang.InferTypeParams.Conforms

import scala.annotation.{implicitNotFound, tailrec}
import scala.reflect.runtime.universe.TypeTag






/** A list of components of some `SchemaMapping` for subject type `S`, with all their types encoded in this class's type.
  * This is the full list of components, ignoring the fact that some of them might not be available or are optional
  * for some types of database operations.
  * Each component of type `T` additionally has a `ComponentExtractor[S, T, O]` associated with it by this instance,
  * which can be accessed using the [[net.noresttherein.oldsql.schema.MappingSchema#extractor extractor(component)]]
  * method. A schema itself is a mapping for the chain `R` containing the values of all of its components in order,
  * but is more typically used as the basis of a `SchemaMapping` instance for some entity type `S`.
  * This can happen either by directly mapping the chain of values `R` with its
  * [[net.noresttherein.oldsql.schema.MappingSchema#map map]] method, or indirectly in an enclosing mapping's
  * `assemble` method, where components in this schema can be individually accessed, without constructing
  * an intermediate chain of values. There is an automatically available implicit conversion from non-empty
  * schemas (where `C` and `R` are not empty) which add methods for retrieving its components:
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#last last]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#apply apply()]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaComponentAccessor#prev prev]].
  *
  * @tparam C a `Chain` listing the types of all components in this schema. All components must implement
  *           `SchemaMapping[_, _, _, O]`.
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam R a `Chain` containing the subject types of all components in the chain `C`, which is the subject type
  *           of this mapping.
  * @tparam S the entity type of an owning `SchemaMapping`.
  * @tparam O a marker type denoting the origin of the mapping used to distinguish between different instances
  *           of the same class but representing different tables or different occurrences of a table in the
  *           ''from'' clause of an SQL select.
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.FlatSchemaMapping]]
  */
trait MappingSchema[+C <: Chain, R <: Chain, S, O] extends FreeOriginMapping[R, O] {
	outer :MappingSchemaGuts[C, R, S, O] =>

	/** A shorthand alias for `SchemaColumn[O, T]` allowing reduced notation in the component type chain.
	  * At the same time it removes troublesome occurrences of the
	  * origin type `O` in other places than the type parameter itself.
	  */
	type ||[T] = SchemaColumn[T, O]

	/** A shorthand alias for `LabeledSchemaColumn[N, O, T]` allowing reduced notation `N @|| T`
	  * in the component type chain. At the same time it removes troublesome occurrences of the
	  * origin type `O` in other places than the type parameter itself.
	  */
	type @||[N <: Label, T] = LabeledSchemaColumn[N, T, O]

	/** A shorthand alias for `SchemaMapping[L, V, T, O]`, allowing reduced notation for components of this mapping
	  * in the component type chain. At the same time it removes troublesome occurrences of the
	  * origin type `O` in other places than the type parameter itself.
	  */
	type |*|[+L <: Chain, V <: Chain, T] = SchemaMapping[L, V, T, O]

	/** A shorthand alias for `LabeledSchemaMapping[N, L, V, T, O]`, allowing reduced notation for components
	  * of this mapping in the component type chain. At the same time it removes troublesome occurrences of the
	  * origin type `O` in other places than the type parameter itself.
	  */
	type @|*|[N <: Label, +L <: Chain, V <: Chain, T] = LabeledSchemaMapping[N, L, V, T, O]

	/** Component type of this schema, enforcing implementation of `SchemaMapping` of all components. */
	type Subschema[+L <: Chain,  V <: Chain, T] = SchemaMapping[L, V, T, O]

	/** Component type of this schema, enforcing implementation of `SchemaMapping` of all components. */
	type LabeledSubschema[N <: Label, +L <: Chain,  V <: Chain, T] = LabeledSchemaMapping[N, L, V, T, O]

	/** Fully typed list of components in this schema as a `Chain`. */
	def members :C



	/** Transforms this schema into an equivalent `FlatMappingSchema` by recursively replacing each component in the
	  * chain `C` with its columns. This process loses all information about replaced components and the new schema
	  * does not reference this instance in any way other than using the same column instances. It is however
	  * intended to be used as part of inlining of the enclosing mapping, which will retain the references to
	  * all components and use them for the assembly exactly as the enclosing mapping.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.flatten]]
	  */
	def flatten[U >: C <: Chain, IC <: Chain, IR <: Chain]
	          (implicit inliner :SchemaFlattening[U, R, S, O, IC, IR]) :FlatMappingSchema[IC, IR, S, O] =
		inliner(this)






	/** Returns the `ComponentExtractor` for the component labeled with the given string literal in the schema.
	  * If more than one component with the same label exist, the last occurrence is selected.
	  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
	  */
	def apply[N <: Label, T](label :N)(implicit get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O])
			:ComponentExtractor[S, T, O] =
		get.extractor(this, label)

	/** Returns the component labeled with the given string literal in the schema. If more than one component with
	  * the same label exist, the last occurrence in the component chain `C` is selected.
	  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[M <: LabeledMapping[N, T, O], N <: Label, T](label :N)(implicit get :GetLabeledComponent[C, R, M, N, T, O]) :M =
		get(this, label)



	/** Returns the `ComponentExtractor` for the component at the given position in the schema.
	  * @param idx an `Int` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
	  */
	def apply[I <: INT, T](idx :I)(implicit get :GetSchemaComponent[C, R, Component[T], I, T, O])
			:ComponentExtractor[S, T, O] =
		get.extractor(this, idx)

	/** Returns the component at the given position in the schema.
	  * @param idx an `Int` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[M <: Component[T], I <: INT, T](idx :I)(implicit get :GetSchemaComponent[C, R, M, I, T, O]) :M =
		get(this, idx)



	/** Adapts this schema for some other subject type `X` from which the value of the current enclosing mapping
	  * subject type `S` can be derived. This has the effect of composing the extractor for every component
	  * in this schema with the given function.
	  * @param extractor a function returning the owning mapping's subject value from some other value type.
	  * @tparam X the new target type for the enclosing mapping.
	  * @return An instance exactly equivalent to one where all method calls appending components used in building
	  *         of this schema have their passed extractor composed with the given function.
	  */
	def compose[X](extractor :X => S) :MappingSchema[C, R, X, O]

	/** Adapts this schema for some other subject type `X` from which the value of the current enclosing mapping
	  * subject type `S` can be derived. This has the effect of composing the extractor for every component
	  * in this schema with the given extractor.
	  * @param extractor an `Extractor` returning the owning mapping's subject value from some other value type.
	  * @tparam X the new target type for the enclosing mapping.
	  * @return An instance exactly equivalent to one where all method calls appending components used in building
	  *         of this schema have their passed extractor composed with the given extractor.
	  */
	def compose[X](extractor :X =?> S) :MappingSchema[C, R, X, O]



	/** The extractor returning the value for the given component from the enclosing mapping's subject type `S`.
	  * Note that this is different from the extractor returned by `this(component)`, as the latter retrieves
	  * the value from the chain of subject types of all components in the schema.
	  */
	def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O]



	/** Returns the chain with the values of all components from the subject value of the enclosing `SchemaMapping`.
	  * @return a chain of component values inside `Some` as long as all of them returned `Some`
	  *         from their `optionally` method, and `None` in the case when at least one of them didn't have a value
	  *         in the given subject.
	  */
	def unapply(subject :S) :Option[R]

	/** Returns the chain with the values of all components from the subject value of the enclosing `SchemaMapping`
	  * This method will ask the extractors given for all components to produce a value for that component.
	  * If at least one of them fails, a `NoSuchElementException` will be thrown.
	  * @return a chain of values for all components on the component chain `C`.
	  */
	def disassemble(subject :S) :R



	protected[schema] def selectorsList :List[(Component[_], Selector[_])]

	protected[schema] def extractorsList :List[(Component[_], ComponentExtractor[S, _, O])]

}






object MappingSchema {


	def apply[S, O] :ExtensibleFlatMappingSchema[@~, @~, S, O] = EmptySchema[S, O]



	/** Methods allowing positional access of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class MappingSchemaComponentAccessor[I <: Chain, L <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                             (private val self :MappingSchema[I ~ L, R ~ T, S, O]) extends AnyVal
	{
		/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
		def apply() :L = last

		/** The last component on the list. */
		def last :L = self.asInstanceOf[ExtensibleNonEmptySchema[I, L, R, T, S, O]].last

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :MappingSchema[I, R, S, O] = self.asInstanceOf[ExtensibleNonEmptySchema[I, L, R, T, S, O]].init
	}



	/** Methods allowing positional access of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class FlatMappingSchemaComponentAccessor[I <: Chain, L <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                                 (private val self :FlatMappingSchema[I ~ L, R ~ T, S, O])
		extends AnyVal
	{
		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :FlatMappingSchema[I, R, S, O] = self.asInstanceOf[ExtensibleFlatNonEmptySchema[I, L, R, T, S, O]].init
	}



	/** Extension class adding methods to string literals for retrieving from a schema the component with this label,
	  * its extractor and value. This class is not implicit, as it would need explicit import, and needs implicitly
	  * available `MappingSchema[C, _, S, O]` and `ComponentValues`; instead, an implicit conversion is available within
	  * the `AbstractSchemaMapping` class for the use of subclasses.
	  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
	  */
	class SchemaComponentLabel[C <: Chain, R <: Chain, V <: ComponentValues[_ <: TypedMapping[S, O]], N <: Label, S, O]
	                          (private val label :N) extends AnyVal
	{
		/** Retrieve the value of the component with this label in the implicit schema from implicit `ComponentValues`.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def unary_![T](implicit schema :MappingSchema[C, R, S, O],
		               get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O], pieces :V) :T =
			pieces(get.extractor(schema, label))

		/** Retrieve the optional value of the component with this label in the implicit schema from implicit
		  * `ComponentValues`. If more then one component with this label is present in the schema,
		  * the last (rightmost) one is taken.
		  */
		def ?[T](implicit schema :MappingSchema[C, R, S, O],
		         get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O], pieces :V) :Option[T] =
			pieces.get(get.extractor(schema, label))

		/** Get the component with this label from the implicit schema.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def unary_~[M <: LabeledMapping[N, T, O], T]
		           (implicit schema :MappingSchema[C, R, S, O], get :GetLabeledComponent[C, R, M, N, T, O]) :M =
			get(schema, label)

		/** Get the extractor for the component with this label from the implicit schema.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def ?>[T](implicit schema :MappingSchema[C, R, S, O],
		          get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O]) :ComponentExtractor[S, T, O] =
			get.extractor(schema, label)

	}






	/** A `MappingSchema` where every component is a column (extending the `SchemaColumn` interface). */
	trait FlatMappingSchema[+C <: Chain, R <: Chain, S, O] extends MappingSchema[C, R, S, O] {
		outer :MappingSchemaGuts[C, R, S, O] =>

		override def flatten[U >: C <: Chain, IC <: Chain, IR <: Chain]
		(implicit flatterer :SchemaFlattening[U, R, S, O, IC, IR]) :FlatMappingSchema[IC, IR, S, O] =
			this.asInstanceOf[FlatMappingSchema[IC, IR, S, O]]



		def compose[X](extractor :X => S) :FlatMappingSchema[C, R, X, O]

		def compose[X](extractor :X =?> S) :FlatMappingSchema[C, R, X, O]

	}






	trait ExtensibleMappingSchema[+C <: Chain, R <: Chain, S, O] extends MappingSchema[C, R, S, O] {
		this :MappingSchemaGuts[C, R, S, O] =>

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[L <: Chain, V <: Chain, T](component :Subschema[L, V, T], value :Extractor[S, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, |*|[L, V, T], R, T, S, O](this, component,
				ComponentExtractor(component, value.optional, value.requisite)
			)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[L <: Chain, V <: Chain, T](value :S => T, component :Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, |*|[L, V, T], R, T, S, O](
				this, component, ComponentExtractor.req(component)(value)
			)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[L <: Chain, V <: Chain, T](value :S => Option[T], component :Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, |*|[L, V, T], R, T, S, O](
				this, component, ComponentExtractor.opt(component)(value)
			)



		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, L <: Chain, V <: Chain, T](component :LabeledSubschema[N, L, V, T], value :Extractor[S, T])
				:ExtensibleMappingSchema[C ~ @|*|[N, L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, @|*|[N, L, V, T], R, T, S, O](
				this, component, ComponentExtractor(component, value.optional, value.requisite)
			)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[N <: Label, L <: Chain, V <: Chain, T](value :S => T, component :LabeledSubschema[N, L, V, T])
				:ExtensibleMappingSchema[C ~ @|*|[N, L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, @|*|[N, L, V, T], R, T, S, O](
				this, component, ComponentExtractor.req(component)(value)
			)

		/** Appends the given component to this schema.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping
		  *              as an `Option`. Whenever `None` is returned, a null values are written to the database for all
		  *              of the component's columns.
		  */
		def optcomp[N <: Label, L <: Chain, V <: Chain, T](value :S => Option[T], component :LabeledSubschema[N, L, V, T])
				:ExtensibleMappingSchema[C ~ @|*|[N, L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, @|*|[N, L, V, T], R, T, S, O](
				this, component, ComponentExtractor.opt(component)(value)
			)



		/** Appends a new column component to this schema. Full static type of the column will be encoded in the
		  * component chain of the returned schema.
		  */
		protected def col[M <: SchemaColumn[T, O], T](column :M, value :S =?> T)
				:ExtensibleMappingSchema[C ~ M, R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, M, R, T, S, O](this, column,
				ComponentExtractor(column, value.optional, value.requisite))

		//	def col[T :ColumnForm](name :String, value :S =?> T, buffs :Buff[T]*) :MappingSchema[C ~ ||[T], R ~ T, O, S] =
		//		comp(value, SchemaColumn[O, T](name, buffs :_*))

		/** Appends a new column to this schema with the given name. */
		def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(SchemaColumn[T, O](name, buffs :_*), Extractor.req(value))

		//	def col[T :ColumnForm](value :S =?> T, buffs :Buff[T]*)(implicit tpe :TypeTag[S]) :MappingSchema[C ~ ||[T], R ~ T, O, S] =
		//		comp(value, SchemaColumn[O, T](PropertyPath.nameOf(value.requisite getOrElse value.optional), buffs:_*))

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)

		/** Appends a new column of the given name to this schema.
		  * @param value a function returning the value for the column from the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(SchemaColumn[T, O](name, buffs:_*), Extractor(value))

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		/** Appends a new column labeled with its name to this schema.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs:_*), Extractor.req(value))

		/** Appends a new column labeled with its name to this schema.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs:_*), Extractor(value))






		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the
		  * chain of subjects of all top-level components of this schema.
		  * @param constructor a function accepting a chain with the values of all components as they appear in the
		  *                    components chain `C`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
		  */
		def map(constructor :R => S) :SchemaMapping[C, R, S, O] =
			new MappedMappingSchema[C, R, S, O](this, constructor)

		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the
		  * chain of subjects of all top-level components of this schema. This will result in slightly more efficient
		  * assembly than the other overloaded `map` method, as no chain with the values of all components will be assembled
		  * as an intermediate step.
		  * @param constructor a function which number of arguments and their types match the subject types of all
		  *                    components as listed by the chain `R`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
		  */
		def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :SchemaMapping[C, R, S, O] =
			map { v :R => v.feedTo(constructor) }



		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the
		  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may
		  * not produce the subject value for all input rows.
		  * @param constructor a function accepting a chain with the values of all components as they appear in the
		  *                    components chain `C`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
		  */
		def flatMap(constructor :R => Option[S]) :SchemaMapping[C, R, S, O] =
			new FlatMappedMappingSchema[C, R, S, O](this, constructor)

		/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
		  * provided with component and column definitions when building this schema for disassembly of its subject
		  * before writing to the database, and the function specified here for assembling its subject from the
		  * chain of subjects of all top-level components of this schema. Unlike `map`, this variant may not produce
		  * the subject value for all input rows. This will result in slightly more efficient assembly than the other
		  * overloaded `flatMap` method, as no chain with the values of all components will be assembled as an intermediate step.
		  * @param constructor a function which number of arguments and their types match the subject types of all
		  *                    components as listed by the chain `R`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
		  */
		def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :SchemaMapping[C, R, S, O] =
			flatMap { row :R => row.feedTo(constructor) }

	}






	trait ExtensibleFlatMappingSchema[+C <: Chain, R <: Chain, S, O]
		extends ExtensibleMappingSchema[C, R, S, O] with FlatMappingSchema[C, R, S, O]
	{
		outer :MappingSchemaGuts[C, R, S, O] =>


		override def col[M <: SchemaColumn[T, O], T](column :M, value :S =?> T)
				:ExtensibleFlatMappingSchema[C ~ M, R ~ T, S, O] =
			new ExtensibleFlatNonEmptySchema[C, M, R, T, S, O](this, column,
				                                     ComponentExtractor(column, value.optional, value.requisite))

		override def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(SchemaColumn[T, O](name, buffs:_*), value)

		override def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)



		override def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(SchemaColumn[T, O](name, buffs :_*), Extractor(value))

		override def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleFlatMappingSchema[C ~ ||[T], R ~ T, S, O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		override def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs :_*), value)

		override def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](name, buffs :_*), Extractor(value))



		override def map(constructor :R => S) :FlatSchemaMapping[C, R, S, O] =
			new MappedMappingSchema[C, R, S, O](this, constructor) with FlatSchemaMapping[C, R, S, O] {
				override val schema = outer
			}

		override def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :FlatSchemaMapping[C, R, S, O] =
			map { row :R => row.feedTo(constructor) }

		override def flatMap(constructor :R => Option[S]) :FlatSchemaMapping[C, R, S, O] =
			new FlatMappedMappingSchema[C, R, S, O](this, constructor) with FlatSchemaMapping[C, R, S, O] {
				override val schema = outer
			}

		override def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :FlatSchemaMapping[C, R, S, O] =
			flatMap { row :R => row feedTo constructor }

	}






/*
	implicit def MappingSchemaProjection[AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
	                                    (implicit alias :ComponentChainProjection[AC, S, A, BC, B])
			:OriginProjection[MappingSchema[AC, R, S, A], A, MappingSchema[BC, R, S, B], B] =
		Mapping.AnyOrigin()

	implicit def FlatMappingSchemaProjection[AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
	                                        (implicit alias :ComponentChainProjection[AC, S, A, BC, B])
			:OriginProjection[FlatMappingSchema[AC, R, S, A], A, FlatMappingSchema[BC, R, S, B], B] =
		Mapping.AnyOrigin()



	@implicitNotFound("Cannot alias component chain ${AC} from origin ${A}\nas ${BC} from origin ${B}:\n" +
	                  "no implicit ComponentChainProjection[${AC}, ${S}, ${A}, ${BC}, ${B}].\n" +
		              "Most likely reason is OriginProjection implicit conflict for one of the components. " +
		              "See net.noresttherein.oldsql.schema.Mapping.OriginProjection class documentation for more information.")
	final class ComponentChainProjection[-AC <: Chain, S, A, +BC <: Chain, B] extends (AC => BC) {
		override def apply(components :AC) :BC = components.asInstanceOf[BC]
	}

	object ComponentChainProjection {
		private[this] val alias :ComponentChainProjection[Chain, Any, Any, Chain, Any] = new ComponentChainProjection

		implicit def emptyComponentChainProjection[S, A, B] :ComponentChainProjection[@~, S, A, @~, B] =
			alias.asInstanceOf[ComponentChainProjection[@~, S, A, @~, B]]

		implicit def componentChainProjection[AC <: Chain, AM <: Mapping, S, A, BC <: Chain, BM <: Mapping, B]
		                                     (implicit init :ComponentChainProjection[AC, S, A, BC, B],
		                                      last :OriginProjection[AM, A, BM, B])
				:ComponentChainProjection[AC ~ AM, S, A, BC ~ BM, B] =
			init.asInstanceOf[ComponentChainProjection[AC ~ AM, S, A, BC ~ BM, B]]
	}
*/



	@implicitNotFound("Cannot concatenate schemas ${PC} + ${SC} (with subject types ${PR} + ${SR}). Possible reasons:\n" +
		"1) any of the schemas contains non-column components,\n" +
		"2) the type of any of the schemas is not fully known,\n" +
		"3) the subject types don't match the subject types of the components," +
		"4) result cannot be unified with provided concatenated type ${C} (${R}).")
	sealed trait ColumnSchemaConcat[PC <: Chain, PR <: Chain, SC <: Chain, SR <: Chain, S, O, C <: Chain, R <: Chain] {
		def apply(prefix :ExtensibleFlatMappingSchema[PC, PR, S, O], suffix :FlatMappingSchema[SC, SR, S, O])
				:ExtensibleFlatMappingSchema[C, R, S, O]
	}

	object ColumnSchemaConcat {
		private[this] val emptyCat = new ColumnSchemaConcat[Chain, Chain, @~, @~, Any, Any, Chain, Chain] {
			override def apply(prefix :ExtensibleFlatMappingSchema[Chain, Chain, Any, Any],
			                   suffix :FlatMappingSchema[@~, @~, Any, Any]) =
				prefix
		}

		implicit def concatEmpty[C <: Chain, R <: Chain, S, O] :ColumnSchemaConcat[C, R, @~, @~, S, O, C, R] =
			emptyCat.asInstanceOf[ColumnSchemaConcat[C, R, @~, @~, S, O, C, R]]

		implicit def concatColumns[PC <: Chain, PR <: Chain, SC <: Chain, SR <: Chain,
			                       M <: SchemaColumn[T, O], T, S, O, C <: Chain, R <: Chain]
		                          (implicit init :ColumnSchemaConcat[PC, PR, SC, SR, S, O, C, R])
				:ColumnSchemaConcat[PC, PR, SC ~ M, SR ~ T, S, O, C ~ M, R ~ T] =
			new ColumnSchemaConcat[PC, PR, SC ~ M, SR ~ T, S, O, C ~ M, R ~ T] {
				override def apply(prefix :ExtensibleFlatMappingSchema[PC, PR, S, O],
				                   suffix :FlatMappingSchema[SC ~ M, SR ~ T, S, O]) =
					init(prefix, suffix.prev).col(suffix.last, suffix.extractor(suffix.last))

			}
	}



	@implicitNotFound("Cannot inline schema ${C}\n (with subject type ${R}).\n Possible reasons: " +
	                  "the types are not fully known; subject types don't match the component types; " +
	                  "the result cannot be unified with result types ${IC} (${IR}).")
	abstract class SchemaFlattening[C <: Chain, R <: Chain, S, O, IC <: Chain, IR <: Chain] private[MappingSchema] {
		def apply(schema :MappingSchema[C, R, S, O]) :ExtensibleFlatMappingSchema[IC, IR, S, O]
	}

	sealed abstract class ComponentSchemaFlattening {

		private[this] final val empty = new SchemaFlattening[@~, @~, Any, Any, @~, @~] {
			override def apply(schema :MappingSchema[@~, @~, Any, Any]) =
				EmptySchema[Any, Any]
		}

		implicit def emptyFlattening[S, O] :SchemaFlattening[@~, @~, S, O, @~, @~] =
			empty.asInstanceOf[SchemaFlattening[@~, @~, S, O, @~, @~]]

		implicit def componentFlattening[C <: Chain, R <: Chain, PC <: Chain, PR <: Chain,
			                             M <: SchemaMapping[MC, MR, T, O], MC <: Chain, MR <: Chain, T, S, O,
		                                 SC <: Chain,  SR <: Chain, IC <: Chain, IR <: Chain]
		                                (implicit prefix :SchemaFlattening[C, R, S, O, PC, PR],
		                                 hint :Conforms[M, M, SchemaMapping[MC, MR, T, O]],
		                                 inline :SchemaFlattening[MC, MR, T, O, SC, SR],
		                                 concat :ColumnSchemaConcat[PC, PR, SC, SR, S, O, IC, IR])
				:SchemaFlattening[C ~ M, R ~ T, S, O, IC, IR] =
			new SchemaFlattening[C ~ M, R ~ T, S, O, IC, IR] {
				override def apply(schema :MappingSchema[C ~ M, R ~ T, S, O]) =
					concat(prefix(schema.prev), inline(schema.last.schema) compose schema.extractor(schema.last))
			}
	}

	object SchemaFlattening extends ComponentSchemaFlattening {

		implicit def columnFlattening[C <: Chain, R <: Chain, M <: SchemaColumn[T, O], T, S, O, IC <: Chain, IR <: Chain]
		                             (implicit init :SchemaFlattening[C, R, S, O, IC, IR])
				:SchemaFlattening[C ~ M, R ~ T, S, O, IC ~ M, IR ~ T] =
			new SchemaFlattening[C ~ M, R ~ T, S, O, IC ~ M, IR ~ T] {
				override def apply(schema :MappingSchema[C ~ M, R ~ T, S, O]) =
					init(schema.prev).col(schema.last, schema.extractor(schema.last))
			}
	}



	@implicitNotFound("No ${M} <: LabeledMapping[${N}, ${T}, ${O}] present in the schema ${C}\n(with values ${R}).")
	sealed abstract class GetLabeledComponent[-C <: Chain, R <: Chain, +M <: LabeledMapping[N, T, O], N <: Label, T, O] {
		def apply[S](schema :MappingSchema[C, R, S, O], label :N) :M
		def extractor[S](schema :MappingSchema[C, R, S, O], label :N) :ComponentExtractor[S, T, O]
	}

	object GetLabeledComponent {
		implicit def last[R <: Chain, M <: LabeledMapping[N, T, O], N <: Label, T, O]
				:GetLabeledComponent[Chain ~ M, R ~ T, M, N, T, O] =
			new GetLabeledComponent[Chain ~ M, R ~ T, M, N, T, O] {
				override def apply[S](schema :MappingSchema[Chain ~ M, R ~ T, S, O], label :N) = schema.last

				override def extractor[S](schema :MappingSchema[Chain ~ M, R ~ T, S, O], label :N) =
					schema.extractor(schema.last)
			}

		implicit def previous[C <: Chain, R <: Chain, M <: LabeledMapping[N, T, O], X, N <: Label, T, O]
		                     (implicit get :GetLabeledComponent[C, R, M, N, T, O])
				:GetLabeledComponent[C ~ TypedMapping[X, O], R ~ X, M, N, T, O] =
			new GetLabeledComponent[C ~ TypedMapping[X, O], R ~ X, M, N, T, O] {
				override def apply[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], label :N) =
					get(schema.prev, label)

				override def extractor[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], label :N) =
					get.extractor(schema.prev, label)
			}
	}



	@implicitNotFound("No component ${M} <: TypedMapping[${T}, ${O}] present at index ${I} in the schema ${C}\n(with values ${R}).")
	sealed abstract class GetSchemaComponent[-C <: Chain, R <: Chain, +M <: TypedMapping[T, O], I <: INT, T, O] {
		def apply[S](schema :MappingSchema[C, R, S, O], idx :I) :M
		def extractor[S](schema :MappingSchema[C, R, S, O], idx :I) :ComponentExtractor[S, T, O]
	}

	object GetSchemaComponent {
		implicit def singleton[M <: TypedMapping[T, O], O, T]
				:GetSchemaComponent[@~ ~ M, @~ ~ T, M, 0, T, O] =
			new GetSchemaComponent[@~ ~ M, @~ ~ T, M, 0, T, O] {
				override def apply[S](schema :MappingSchema[@~ ~ M, @~ ~ T, S, O], idx :0) = schema.last
				override def extractor[S](schema :MappingSchema[@~ ~ M, @~ ~ T, S, O], idx :0) =
					schema.extractor(schema.last)
			}

		implicit def last[C <: Chain, R <: Chain, M <: TypedMapping[T, O], I <: INT, J <: INT, T, O]
		                 (implicit inc :Inc[I, J], get :GetSchemaComponent[C, R, _, I, _, O])
				:GetSchemaComponent[C ~ M, R ~ T, M, J, T, O] =
			new GetSchemaComponent[C ~ M, R ~ T, M, J, T, O] {
				override def apply[S](schema :MappingSchema[C ~ M, R ~ T, S, O], idx :J) :M = schema.last
				override def extractor[S](schema :MappingSchema[C ~ M, R ~ T, S, O], idx :J) =
					schema.extractor(schema.last)
			}

		implicit def previous[C <: Chain, R <: Chain, M <: TypedMapping[T, O], X, I <: INT, T, O]
		                     (implicit get :GetSchemaComponent[C, R, M, I, T, O])
				:GetSchemaComponent[C ~ TypedMapping[X, O], R ~ X, M, I, T, O] =
			new GetSchemaComponent[C ~ TypedMapping[X, O], R ~ X, M, I, T, O] {
				override def apply[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], idx :I) =
					get(schema.prev, idx)

				override def extractor[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], idx :I) =
					get.extractor(schema.prev, idx)
			}
	}






	sealed trait MappingSchemaGuts[+C <: Chain, R <: Chain, S, O] extends MappingSchema[C, R, S, O]





	private[schema] class EmptySchema[S, O] extends ConstantMapping[@~, O](@~) with FlatMappingSchema[@~, @~, S, O]
		with MappingSchemaGuts[@~, @~, S, O]
	{
		override def members: @~ = @~

		private[this] val extractor :ComponentExtractor[S, @~, O] = ComponentExtractor.const(this)(@~)

		override def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O] =
			if (component eq this)
				extractor.asInstanceOf[ComponentExtractor[S, X, O]]
			else
				throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping schema.")

		override def unapply(subject :S): Option[@~] = Some(@~)

		override def disassemble(subject :S): @~ = @~

		override def compose[X](extractor :X => S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]

		override def compose[X](extractor :X =?> S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]

		protected[schema] override def selectorsList :Nil.type = Nil

		protected[schema] override def extractorsList :Nil.type = Nil
	}



	private[schema] class ExtensibleEmptySchema[S, O]
		extends EmptySchema[S, O] with ExtensibleFlatMappingSchema[@~, @~, S, O]



	object EmptySchema {
		private[this] val empty = new ExtensibleEmptySchema[Any, Any]

		def apply[S, O] :ExtensibleFlatMappingSchema[@~, @~, S, O] = empty.asInstanceOf[ExtensibleEmptySchema[S, O]]

		def unapply(schema :MappingSchema[_, _, _, _]) :Boolean = schema.isInstanceOf[EmptySchema[_, _]]
	}






	private[schema] class NonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                    (val init :MappingSchema[C, R, S, O], val last :M,
	                                     val extractor :ComponentExtractor[S, T, O])
		extends MappingSchemaGuts[C ~ M, R ~ T, S, O] with LazyMapping[R ~ T, O]
	{
		override def members :C ~ M = init.members ~ last

		override def unapply(subject :S) :Option[R ~ T] =
			for (i <- init.unapply(subject); l <- extractor.get(subject)) yield i ~ l

		override def disassemble(subject :S) :R ~ T = init.disassemble(subject) ~ extractor(subject)

		override def assemble(pieces :Pieces) :Option[R ~ T] =
			for (i <- pieces.get(initSelector); l <- pieces.get(lastSelector))
				yield i ~ l

		override def compose[X](extractor :X => S) :NonEmptySchema[C, M, R, T, X, O] =
			new NonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptySchema[C, M, R, T, X, O] =
			new NonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)



		private[this] val extractors = Lazy(extractorsList.toMap)

		protected[schema] override def extractorsList :List[(init.Component[_], ComponentExtractor[S, _, O])] =
			(last -> extractor) :: (init -> ComponentExtractor.opt(init)(init.unapply)) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose extractor) } reverse_:::
				init.extractorsList

		override def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O] =
			if (component eq last) extractor.asInstanceOf[ComponentExtractor[S, X, O]]
			else extractors.get(component).asInstanceOf[ComponentExtractor[S, X, O]]


		//these are extractors from the subject of this mapping, R ~ T, rather than S
		private[this] val selectors = Lazy(selectorsList.toMap)
		private[this] val initSelector = ComponentExtractor.req(init) { vs :(R ~ T) => vs.init }
		private[this] val lastSelector = ComponentExtractor.req(last) { vs :(R ~ T) => vs.last }

		protected[schema] override def selectorsList :List[(Component[_], Selector[_])] =
			(last -> lastSelector) :: (init, initSelector) ::
				last.subcomponents.toList.map { comp => (comp, last(comp) compose lastSelector) } reverse_:::
				init.selectorsList.map { case (comp, sel) => (comp, sel compose initSelector) }

		override def apply[X](component :Component[X]) :Selector[X] =
			selectors.get(component).asInstanceOf[Selector[X]]



		override val components :Unique[Component[_]] = Unique[Component[_]](init, last)

		override val subcomponents :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, _, _, O], subcomponents :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :NonEmptySchema[_, _, _, _, _, O] =>
						rec(multi.init, multi.init :: multi.last.subcomponents.toList ::: multi.last :: subcomponents)
					case _ => subcomponents
				}
			Unique.Lazy(rec(this, Nil))
		}

		override val columns :Unique[Component[_]] = {
			@tailrec def rec(schema :MappingSchema[_, _, _, O], columns :List[Component[_]]) :List[Component[_]] =
				schema match {
					case multi :NonEmptySchema[_, _, _, _, _, O] =>
						rec(multi.init, multi.last.columns.toList ::: columns)
					case _ => columns
				}
			Unique.Lazy(rec(this, Nil))
		}


	}



	private[schema] class ExtensibleNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
                          (init :MappingSchema[C, R, S, O], last :M, extractor :ComponentExtractor[S, T, O])
		extends NonEmptySchema[C, M, R, T, S, O](init, last, extractor) with ExtensibleMappingSchema[C ~ M, R ~ T, S, O]






	private[schema] class FlatNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                        (override val init :FlatMappingSchema[C, R, S, O], next :M,
	                                         get :ComponentExtractor[S, T, O])
		extends ExtensibleNonEmptySchema[C, M, R, T, S, O](init, next, get) with FlatMappingSchema[C ~ M, R ~ T, S, O]
	{
		override def compose[X](extractor :X => S) :FlatNonEmptySchema[C, M, R, T, X, O] =
			new FlatNonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatNonEmptySchema[C, M, R, T, X, O] =
			new FlatNonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)
	}



	private[schema] class ExtensibleFlatNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                      (init :FlatMappingSchema[C, R, S, O], next :M, get :ComponentExtractor[S, T, O])
		extends FlatNonEmptySchema[C, M, R, T, S, O](init, next, get) with ExtensibleFlatMappingSchema[C ~ M, R ~ T, S, O]






	private[MappingSchema] class MappedMappingSchema[C <: Chain, R <: Chain, S, O]
	                                                (override val schema :MappingSchema[C, R, S, O], constructor :R => S)
		extends ShallowAdapter[TypedMapping[R, O], R, S, O] with SchemaMapping[C, R, S, O]
	{
		override protected val egg = schema
		private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
			else schema.extractor(component)

		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtractor) map constructor

	}



	private[MappingSchema] class FlatMappedMappingSchema[C <: Chain, R <: Chain, S, O]
			(override val schema :MappingSchema[C, R, S, O], constructor :R => Option[S])
		extends ShallowAdapter[TypedMapping[R, O], R, S, O] with SchemaMapping[C, R, S, O]
	{
		override protected val egg = schema
		private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
			else schema.extractor(component)

		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtractor) flatMap constructor

	}

}


