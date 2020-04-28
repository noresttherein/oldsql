package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainApplication, ChainConcat}
import net.noresttherein.oldsql.model.PropertyPath
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema
import net.noresttherein.oldsql.schema.Mapping.{FreeOriginMapping, MappingFrom, OriginProjection, TypedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.support.{ConstantMapping, MappingAdapter, StableMapping}
import net.noresttherein.oldsql.schema.MappingSchema.{ExtensibleNonEmptySchema, FlatMappedMappingSchema, FlatMappingSchema, GetLabeledComponent, GetSchemaComponent, MappedMappingSchema, SchemaFlattening}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, MappingLabel}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatSchemaMapping, LabeledSchemaColumn, LabeledSchemaComponent, SchemaColumn}
import net.noresttherein.oldsql.schema.bits.LabeledMapping
import net.noresttherein.oldsql.schema.ComponentExtractor.ColumnExtractor
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
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#last last]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#apply apply()]],
  * [[net.noresttherein.oldsql.schema.MappingSchema.MappingSchemaMethods#prev prev]].
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
trait MappingSchema[+C <: Chain, R <: Chain, S, O] extends FreeOriginMapping[R, O] { outer =>

	protected val outerBuffs :Seq[Buff[S]] = Nil

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

	/** A shorthand alias for `LabeledSchemaComponent[N, L, V, T, O]`, allowing reduced notation for components
	  * of this mapping in the component type chain. At the same time it removes troublesome occurrences of the
	  * origin type `O` in other places than the type parameter itself.
	  */
	type @|*|[N <: Label, +L <: Chain, V <: Chain, T] = LabeledSchemaComponent[N, L, V, T, O]

	/** Component type of this schema, enforcing implementation of `SchemaMapping` of all components. */
	type Subschema[+L <: Chain,  V <: Chain, T] = SchemaMapping[L, V, T, O]

	/** Component type of this schema, enforcing implementation of `SchemaMapping` of all components. */
	type LabeledSubschema[N <: Label, +L <: Chain,  V <: Chain, T] = LabeledSchemaComponent[N, L, V, T, O]

	/** Fully typed list of components in this schema as a `Chain`. */
	def members :C



//	/** Transforms this schema into an equivalent `FlatMappingSchema` by recursively replacing each component in the
//	  * chain `C` with its columns. This process loses all information about replaced components and the new schema
//	  * does not reference this instance in any way other than using the same column instances. It is however
//	  * intended to be used as part of inlining of the enclosing mapping, which will retain the references to
//	  * all components and use them for the assembly exactly as the enclosing mapping.
//	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping#flatten]]
//	  */
//	def flatten[U >: C <: Chain, IC <: Chain, IR <: Chain]
//	          (implicit inliner :SchemaFlattening[U, R, S, O, IC, IR]) :FlatMappingSchema[IC, IR, S, O] =
//		inliner(this)






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
	def apply[I <: Numeral, T](idx :I)(implicit get :GetSchemaComponent[C, R, Component[T], I, T, O])
			:ComponentExtractor[S, T, O] =
		get.extractor(this, idx)

	/** Returns the component at the given position in the schema.
	  * @param idx an `Int` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[M <: Component[T], I <: Numeral, T](idx :I)(implicit get :GetSchemaComponent[C, R, M, I, T, O]) :M =
		get(this, idx)



	/** The extractor returning the value for the given component from the enclosing mapping's subject type `S`.
	  * Note that this is different from the extractor returned by `this(component)`, as the latter retrieves
	  * the value from the chain of subject types of all components in the schema.
	  */
	def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O]

	/** The extractor returning the value for the given column from the enclosing mapping's subject type `S`.
	  * Note that this is different from the extractor returned by `this(column)`, as the latter retrieves
	  * the value from the chain of subject types of all components in the schema.
	  */
	def extractor[X](column :Column[X]) :ColumnExtractor[S, X, O]



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



	/** Creates a `SchemaMapping` instance using this schema. The mapping will use the extractor functions
	  * provided with component and column definitions when building this schema for disassembly of its subject
	  * before writing to the database, and the function specified here for assembling its subject from the
	  * chain of subjects of all top-level components of this schema.
	  * @param constructor a function accepting a chain with the values of all components as they appear in the
	  *                    components chain `C`.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.flatMap]]
	  */
	def map(constructor :R => S) :SchemaMapping[C, R, S, O] =
		new MappedMappingSchema[C, R, S, O](this, constructor, outerBuffs)

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
		new FlatMappedMappingSchema[C, R, S, O](this, constructor, outerBuffs)

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



	protected[schema] def schemaExtractors :List[(Component[_], Selector[_])]

	protected[schema] def outerExtractors :List[(Component[_], ComponentExtractor[S, _, O])]

	protected[schema] def componentsReversed :List[Component[_]]

	protected[schema] def subcomponentsReversed :List[Component[_]]

	protected[schema] def columnsReversed :List[Column[_]]

	protected[schema] def component :Component[_]

	protected[schema] def init :MappingSchema[_, _, S, O]
}






object MappingSchema {
	//todo: examples in the doc.

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a `SchemaMapping`.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @tparam O an arbitrary marker 'origin' type used to differentiate between separate instances of the same class.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[S, O] :ExtensibleFlatMappingSchema[@~, @~, S, O] = EmptySchema[S, O]


	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a `SchemaMapping`. This no-arg, no-parenthesis method can be followed by an argument list
	  * with a variable number of buffs for the built mapping, which will be inherited by all columns and chosen
	  * components appended to the created schema. With the separation of the factory method into a separate
	  * applicable object the subject type `S` can be inferred from the provided buffs.
	  * @return an applicable object accepting a sequence of buffs and returning an empty, extensible mapping schema.
	  * @tparam O an arbitrary marker 'origin' type used to differentiate between separate instances of the same class.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
	  */
	def apply[O] :MappingSchemaBuilder[O] = new MappingSchemaBuilder[O] {}



	/** A simple factory of empty mapping schema instances.  */
	trait MappingSchemaBuilder[O] extends Any {
		/** An empty `MappingSchema` which can be expanded by appending new columns and components.
		  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
		  * creating a `SchemaMapping`.
		  * @return an applicable object accepting a sequence of buffs and returning an empty, extensible mapping schema.
		  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
		  *           extractors which retrieve their value from this type.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.ExtensibleFlatMappingSchema]]
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping]]
		  */
		def apply[S](buffs :Buff[S]*) :ExtensibleFlatMappingSchema[@~, @~, S, O] = EmptySchema[S, O](buffs)
	}



	/** Methods allowing positional access of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class MappingSchemaMethods[I <: Chain, L <: TypedMapping[_, O], R <: Chain, T, S, O]
	                                             (private val self :MappingSchema[I ~ L, R ~ T, S, O]) extends AnyVal
	{
		/** The last component on the list - same as `last` but more readable in code like `schema.prev.prev()`. */
		def apply() :L = last

		/** The last component on the list. */
		def last :L = self.component.asInstanceOf[L]

		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :MappingSchema[I, R, S, O] =
			self.init.asInstanceOf[MappingSchema[I, R, S, O]]



		/** Transforms this schema into an equivalent `FlatMappingSchema` by recursively replacing each component in the
		  * chain `C` with its columns. This process loses all information about replaced components and the new schema
		  * does not reference this instance in any way other than using the same column instances. It is however
		  * intended to be used as part of inlining of the enclosing mapping, which will retain the references to
		  * all components and use them for the assembly exactly as the enclosing mapping.
		  * This method is extracted from the schema class to an extension method to avoid conflict with the like
		  * method in `SchemaMapping` for `ChainMapping` and similar classes which implement both `SchemaMapping`
		  * and `MappingSchema` traits.
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping#flatten]]
		  */
		def flatten[IC <: Chain, IR <: Chain]
		           (implicit inliner :SchemaFlattening[I ~ L, R ~ T, S, O, IC, IR]) :FlatMappingSchema[IC, IR, S, O] =
			self match {
				case flat :FlatMappingSchema[_, _, _, _] => flat.asInstanceOf[FlatMappingSchema[IC, IR, S, O]]
				case _ => inliner(self)
			}


	}



	/** Methods allowing positional access of the components listed on `I ~ L` by a `MappingSchema`.  */
	implicit class FlatMappingSchemaMethods[I <: Chain, L <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                                 (private val self :FlatMappingSchema[I ~ L, R ~ T, S, O])
		extends AnyVal
	{
		/** The schema for the chain `I`, containing all components of this schema except for the last one. */
		def prev :FlatMappingSchema[I, R, S, O] = self.init.asInstanceOf[FlatMappingSchema[I, R, S, O]]
	}



	/** Extension class adding methods to string literals for retrieving from a schema the component with this label,
	  * its extractor and value. This class is not implicit, as it would need explicit import, and needs implicitly
	  * available `MappingSchema[C, _, S, O]` and `ComponentValues`; instead, an implicit conversion is available within
	  * the `AbstractSchemaMapping` class for the use of subclasses.
	  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
	  */
	class SchemaComponentLabels[C <: Chain, R <: Chain, V <: ComponentValues[_ <: TypedMapping[S, O]], N <: Label, S, O]
	                          (private val label :N) extends AnyVal
	{
		/** Retrieve the value of the component with this label in the implicit schema from implicit `ComponentValues`.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def unary_~[T](implicit schema :MappingSchema[C, R, S, O],
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
		def ^[M <: LabeledMapping[N, T, O], T]
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
	trait FlatMappingSchema[+C <: Chain, R <: Chain, S, O] extends MappingSchema[C, R, S, O] { outer =>

//		override def flatten[U >: C <: Chain, IC <: Chain, IR <: Chain]
//		                    (implicit flatterer :SchemaFlattening[U, R, S, O, IC, IR]) :FlatMappingSchema[IC, IR, S, O] =
//			this.asInstanceOf[FlatMappingSchema[IC, IR, S, O]]



		def compose[X](extractor :X => S) :FlatMappingSchema[C, R, X, O]

		def compose[X](extractor :X =?> S) :FlatMappingSchema[C, R, X, O]



		override def map(constructor :R => S) :FlatSchemaMapping[C, R, S, O] =
			new MappedFlatMappingSchema[C, R, S, O](this, constructor, outerBuffs)

		override def map[F](constructor :F)(implicit apply :ChainApplication[R, F, S]) :FlatSchemaMapping[C, R, S, O] =
			map { row :R => row.feedTo(constructor) }

		override def flatMap(constructor :R => Option[S]) :FlatSchemaMapping[C, R, S, O] =
			new FlatMappedFlatMappingSchema[C, R, S, O](this, constructor, outerBuffs)

		override def flatMap[F](constructor :F)(implicit apply :ChainApplication[R, F, Option[S]]) :FlatSchemaMapping[C, R, S, O] =
			flatMap { row :R => row feedTo constructor }


		protected[schema] override def init :FlatMappingSchema[_, _, S, O]
	}






	/** A `MappingSchema` with factory methods for schema columns and components used to build
	  * (in a purely functional way) a `MappingSchema` and a `SchemaMapping` by chaining calls.
	  * This is a separate class from the `MappingSchema` as different schema variants have slightly
	  * different methods with conflicting signatures.
	  */
	trait ExtensibleMappingSchema[+C <: Chain, R <: Chain, S, O] extends MappingSchema[C, R, S, O] {

		protected[schema] def conveyBuffs[T](extractor :S => T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) outerBuffs.flatMap(_.cascade(extractor))
			else if (outerBuffs.isEmpty) buffs
			else buffs ++: outerBuffs.flatMap(_.cascade(extractor))

		protected[schema] def conveyBuffs[T](extractor :S =?> T, buffs :Seq[Buff[T]]) :Seq[Buff[T]] =
			if (buffs.isEmpty) schema.cascadeBuffs(outerBuffs, toString)(extractor)
			else if (outerBuffs.isEmpty) buffs
			else buffs ++: schema.cascadeBuffs(outerBuffs, toString)(extractor)



		/** Appends a new component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def comp[L <: Chain, V <: Chain, T](value :S => T, buffs :Buff[T]*)
		                                   (component :Seq[Buff[T]] => Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			comp(value, component(conveyBuffs(value, buffs)))

		/** Appends the given component to this schema. This component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value an extractor returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[L <: Chain, V <: Chain, T](component :Subschema[L, V, T], value :Extractor[S, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, |*|[L, V, T], R, T, S, O](
				this, component, ComponentExtractor(component, value.optional, value.requisite)
			)

		/** Appends the given component to this schema. The component will not inherit any buffs associated
		  * with this instance and its outer mapping.
		  * @param component a `SchemaMapping`  with the same origin type `O` to add as the component.
		  * @param value a function returning the value of this component for the subject type `S` of an owning mapping.
		  */
		def comp[L <: Chain, V <: Chain, T](value :S => T, component :Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, |*|[L, V, T], R, T, S, O](
				this, component, ComponentExtractor.req(component)(value)
			)



		/** Appends a new component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def optcomp[L <: Chain, V <: Chain, T](value :S => Option[T], buffs :Buff[T]*)
		                                      (component :Seq[Buff[T]] => Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ |*|[L, V, T], R ~ T, S, O] =
			optcomp(value, component(conveyBuffs(Extractor(value), buffs)))

		/** Appends the given component to this schema. The component will not inherit any buffs associated
		  * with this instance and its outer mapping.
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



		/** Appends a new labeled component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def comp[N <: Label, L <: Chain, V <: Chain, T]
		        (label :N, value :S => T, buffs :Buff[T]*)(component :Seq[Buff[T]] => Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ @|*|[N, L, V, T], R ~ T, S, O] =
			comp(value, component(conveyBuffs(value, buffs)).:@[N](new ValueOf(label)))

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




		/** Appends a new labeled component to this schema. The component will inherit all buffs associated
		  * with the outer mapping for subject `S` which where given to this schema at initialization.
		  * Inherited buffs will follow any buffs passed to this method.
		  * @param label a string literal identifying the created component within this instance.
		  * @param value an extractor function returning the value of this component from the subject type `S` of
		  *              an owning mapping.
		  * @param buffs buffs to attach specifically to the created component.
		  * @param component constructor function for the component accepting the buffs which should be bestowed
		  *                  to the included component instance.
		  */
		def optcomp[N <: Label, L <: Chain, V <: Chain, T]
		           (label :N, value :S => Option[T], buffs :Buff[T]*)(component :Seq[Buff[T]] => Subschema[L, V, T])
				:ExtensibleMappingSchema[C ~ @|*|[N, L, V, T], R ~ T, S, O] =
			optcomp(value, label @: component(conveyBuffs(Extractor(value), buffs)))

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
		  * component chain of the returned schema. The column will not inherit the buffs associated with this schema.
		  */
		protected def col[M <: SchemaColumn[T, O], T](column :M, value :S =?> T)
				:ExtensibleMappingSchema[C ~ M, R ~ T, S, O] =
			new ExtensibleNonEmptySchema[C, M, R, T, S, O](this, column,
				ComponentExtractor(column, value.optional, value.requisite))

		/** Appends a new column to this schema with the given name. The column will have the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  */
		def col[T :ColumnForm](name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(SchemaColumn[T, O](name, conveyBuffs(value, buffs) :_*), Extractor.req(value))

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def col[T :ColumnForm](value :S => T, buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			col(PropertyPath.nameOf(value), value, buffs :_*)

		/** Appends a new column of the given name to this schema. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning the value for the column from the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
		{
			val extractor = Extractor(value)
			val column = SchemaColumn[T, O](name, conveyBuffs(extractor, buffs) :_*)
			col(column, extractor)
		}

		/** Appends a new column to this schema with the name being the reflected name of the zero-argument method
		  * called on the argument by the extractor function `value`. The column will receive the buffs specified here,
		  * followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param value a function returning a single property of the enclosing mapping's subject `S`.
		  */
		def optcol[T :ColumnForm](value :S => Option[T], buffs :Buff[T]*)(implicit tpe :TypeTag[S])
				:ExtensibleMappingSchema[C ~ ||[T], R ~ T, S, O] =
			optcol(PropertyPath.nameOf(value), value, buffs :_*)



		/** Appends a new column labeled with its name to this schema.  The column will receive the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def lbl[N <: Label, T :ColumnForm](name :N, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			lbl(name, name, value, buffs :_*)

		/** Appends to this schema a new column labeled with a string different from its name.
		  * The column will receive the buffs specified here, followed by any buffs conveyed by this schema,
		  * given to it at its initialization.
		  * @param label the label used to access the column in the schema.
		  * @param name the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def lbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
		{
			val column = LabeledSchemaColumn[N, T, O](label, name, conveyBuffs(value, buffs) :_*)
			col(column, Extractor.req(value))
		}



		/** Appends a new column labeled with its name to this schema.  The column will receive the buffs
		  * specified here, followed by any buffs conveyed by this schema, given to it at its initialization.
		  * @param name a string literal with the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def optlbl[N <: Label, T :ColumnForm](name :N, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			optlbl(name, name, value, buffs:_*)

		/** Appends to this schema a new column labeled with a string different from its name.
		  * The column will receive the buffs specified here, followed by any buffs conveyed by this schema,
		  * given to it at its initialization.
		  * @param label a string literal used to access the column in the schema.
		  * @param name the name of the column.
		  * @param value an extractor function returning the value for the column from the enclosing mapping's subject `S`.
		  * @param buffs a vararg list of buffs modifying the handling of the column.
		  * @tparam N the singleton type of the string literal used as the column name.
		  * @tparam T the mapped column type.
		  */
		def optlbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
		{
			val extractor = Extractor(value)
			col(LabeledSchemaColumn[N, T, O](label, name, conveyBuffs(extractor, buffs) :_*), extractor)
		}

	}






	/** A `FlatMappingSchema` with factory methods for schema columns and components used to build a
	  * `FlatMappingSchema` (and a `FlatSchemaMapping`) by chaining calls with component declarations.
	  * This class extends `ExtensibleMappingSchema`, but inherited non-column component factory methods
	  * switch back to building a general `MappingSchema` so that the process can diverge at any time.
	  */
	trait ExtensibleFlatMappingSchema[+C <: Chain, R <: Chain, S, O]
		extends ExtensibleMappingSchema[C, R, S, O] with FlatMappingSchema[C, R, S, O]
	{ outer =>


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

		override def lbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => T, buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](label, name, buffs :_*), value)

		override def optlbl[N <: Label, T :ColumnForm](label :N, name :String, value :S => Option[T], buffs :Buff[T]*)
				:ExtensibleFlatMappingSchema[C ~ (N @|| T), R ~ T, S, O] =
			col(LabeledSchemaColumn[N, T, O](label, name, buffs :_*), Extractor(value))

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
	sealed abstract class GetSchemaComponent[-C <: Chain, R <: Chain, +M <: TypedMapping[T, O], I <: Numeral, T, O] {
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

		implicit def last[C <: Chain, R <: Chain, M <: TypedMapping[T, O], I <: Numeral, J <: Numeral, T, O]
		                 (implicit inc :Inc[I, J], get :GetSchemaComponent[C, R, _, I, _, O])
				:GetSchemaComponent[C ~ M, R ~ T, M, J, T, O] =
			new GetSchemaComponent[C ~ M, R ~ T, M, J, T, O] {
				override def apply[S](schema :MappingSchema[C ~ M, R ~ T, S, O], idx :J) :M = schema.last
				override def extractor[S](schema :MappingSchema[C ~ M, R ~ T, S, O], idx :J) =
					schema.extractor(schema.last)
			}

		implicit def previous[C <: Chain, R <: Chain, M <: TypedMapping[T, O], X, I <: Numeral, T, O]
		                     (implicit get :GetSchemaComponent[C, R, M, I, T, O])
				:GetSchemaComponent[C ~ TypedMapping[X, O], R ~ X, M, I, T, O] =
			new GetSchemaComponent[C ~ TypedMapping[X, O], R ~ X, M, I, T, O] {
				override def apply[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], idx :I) =
					get(schema.prev, idx)

				override def extractor[S](schema :MappingSchema[C ~ TypedMapping[X, O], R ~ X, S, O], idx :I) =
					get.extractor(schema.prev, idx)
			}
	}






	private[schema] class EmptySchema[S, O] extends ConstantMapping[@~, O](@~) with FlatMappingSchema[@~, @~, S, O] {
		override def members: @~ = @~

		private[this] val extractor :ComponentExtractor[S, @~, O] = ComponentExtractor.const(this)(@~)

		override def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O] =
			if (component eq this)
				extractor.asInstanceOf[ComponentExtractor[S, X, O]]
			else
				throw new IllegalArgumentException(s"Component $component is not a part of this empty mapping schema.")

		override def extractor[X](column :Column[X]) :ColumnExtractor[S, X, O] =
			throw new IllegalArgumentException(s"Column $column is not a part of this empty mapping schema.")

		override def unapply(subject :S): Option[@~] = Some(@~)

		override def disassemble(subject :S): @~ = @~

		override def compose[X](extractor :X => S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]

		override def compose[X](extractor :X =?> S) :EmptySchema[X, O] =
			this.asInstanceOf[EmptySchema[X, O]]


		protected[schema] override def component :Nothing = throw new NoSuchElementException("EmptySchema.last")

		protected[schema] override def init :Nothing = throw new NoSuchElementException("EmptySchema.init")

		protected[schema] override def schemaExtractors :List[Nothing] = Nil

		protected[schema] override def outerExtractors :List[Nothing] = Nil

		protected[schema] override def componentsReversed :List[Nothing] = Nil

		protected[schema] override def subcomponentsReversed :List[Component[_]] = Nil

		protected[schema] override def columnsReversed :List[Column[_]] = Nil
	}



	private[schema] class ExtensibleEmptySchema[S, O](protected override val outerBuffs :Seq[Buff[S]] = Nil)
		extends EmptySchema[S, O] with ExtensibleFlatMappingSchema[@~, @~, S, O]



	object EmptySchema {
		private[this] val empty = new ExtensibleEmptySchema[Any, Any]

		def apply[S, O] :ExtensibleFlatMappingSchema[@~, @~, S, O] = empty.asInstanceOf[ExtensibleEmptySchema[S, O]]

		def apply[S, O](buffs :Seq[Buff[S]]) :ExtensibleFlatMappingSchema[@~, @~, S, O] =
			new ExtensibleEmptySchema[S, O](buffs)

		def unapply(schema :MappingSchema[_, _, _, _]) :Boolean = schema.isInstanceOf[EmptySchema[_, _]]
	}



	/** Base class for all non-empty schemas serving  as a list link responsible for a single component of the schema.
	  * The value of this component (and, what follows, the whole schema) is an arbitrary `Chain`, being
	  * a subclass of a chain variant `I`. As such, it combines the `Chain` of the preceding schema with the value
	  * of the last component `T` into a larger chain `R L U`, where `L` is the type constructor of the link for
	  * the particular chain type `I`.
	  * @param init the schema for the preceding components in the chain.
	  * @param component the last component in the chain, associated with this.
	  * @param extractor extracts the value of the last component from the subject of th owning mapping.
	  * @param lastValue extracts the value of the last component from the chain which is the subject of this mapping.
	  * @tparam I the upper bound of the chain variant which this schema's subject(s) conform to; it is the upper bound
	  *           for the `init` part of the chain link `L[_, _]`.
	  * @tparam L the link type constructor accepting the chain being a subtype of `I` of initial component subjects
	  *           the and an entry value ''derived'' from the subject of the last component.
	  * @tparam E the upper bound of all elements in the chains of type `I`, serving as a bound for the link type `L`.
	  * @tparam C the (regular) `Chain` listing the types of all preceding components in the schema.
	  * @tparam M the type of the last component, contained by this instance.
	  * @tparam R the `Chain` (possibly of some specific kind) derived from the values of all components in the schema
	  *           (but not necessarily consisting exactly of them), forming the initial part of subject type
	  *           of this mapping.
	  * @tparam T the subject type of the last component in this schema.
	  * @tparam U the last ''entry'' type of the chain which is the subject of this mapping, derived from `T`.
	  * @tparam S the subject type of the enclosing ('result') mapping (sometimes referred to as the 'owner' or 'parent').
	  * @tparam O the origin type serving as a discriminator between different instances by tagging them on the type level.
	  */
	private[schema] abstract class BaseNonEmptySchema[I <: Chain, L[+A <: I, +B <: E] <: A ~ B, E,
	                                                  +C <: Chain, +M <: TypedMapping[T, O], R <: I, T, U <: E, S, O]
	                                                 (override val init :MappingSchema[C, R, S, O], override val component :M,
	                                                  val extractor :ComponentExtractor[S, T, O], lastValue :R L U => T)
		extends MappingSchema[C ~ M, R L U, S, O] with StableMapping[R L U, O]
	{
		override def members :C ~ M = init.members ~ component

		override def unapply(subject :S) :Option[R L U] =
			for (i <- init.unapply(subject); l <- extractor.get(subject)) yield link(i, l)

		override def disassemble(subject :S) :R L U = link(init.disassemble(subject), extractor(subject))

		protected def link(init :R, last :T) :R L U

		override def assemble(pieces :Pieces) :Option[R L U] =
			for (i <- pieces.get(initSelector); l <- pieces.get(lastSelector))
				yield link(i, l)



		private[this] val extractors = Lazy {
			val extractors = outerExtractors
			val map = extractors.toMap
			if (map.size != extractors.size)
				throw new IllegalStateException(s"Schema $this contains duplicate components: $extractors")
			map
		}

		protected[schema] override def outerExtractors :List[(init.Component[_], ComponentExtractor[S, _, O])] =
			(component -> extractor) :: (init -> ComponentExtractor.opt(init)(init.unapply)) ::
				component.subcomponents.toList.map { comp => (comp, component(comp) compose extractor) } reverse_:::
				init.outerExtractors

		override def extractor[X](component :Component[X]) :ComponentExtractor[S, X, O] =
			if (component eq component) extractor.asInstanceOf[ComponentExtractor[S, X, O]]
			else extractors.get(component).asInstanceOf[ComponentExtractor[S, X, O]]

		override def extractor[X](column :Column[X]) :ColumnExtractor[S, X, O] =
			if (column eq component) extractor.asInstanceOf[ColumnExtractor[S, X, O]]
			else extractors.get(column).asInstanceOf[ColumnExtractor[S, X, O]]



		//these are extractors from the subject of this mapping, R ~ T, rather than S
		private[this] val selectors = Lazy {
			val extractors = schemaExtractors
			val map = extractors.toMap
			if (map.size != extractors.size)
				throw new IllegalStateException(s"Schema $this contains duplicate components: $extractors")
			map
		}
		private[this] val initSelector = ComponentExtractor.req(init) { vs :(R L U) => vs.init }
		private[this] val lastSelector = ComponentExtractor.req(component)(lastValue)

		protected[schema] override def schemaExtractors :List[(Component[_], Selector[_])] =
			(component -> lastSelector) :: (init, initSelector) ::
				component.subcomponents.toList.map { comp => (comp, component(comp) compose lastSelector) } reverse_:::
				init.schemaExtractors.map { case (comp, sel) => (comp, sel compose initSelector) }

		override def apply[X](component :Component[X]) :Selector[X] =
			selectors.get(component).asInstanceOf[Selector[X]]

		override def apply[X](column :Column[X]) :ColumnSelector[X] =
			selectors.get(column).asInstanceOf[ColumnSelector[X]]


		override val components :Unique[Component[_]] = //Unique[Component[_]](init, last)
			Unique.Lazy(componentsReversed.reverse)

		override protected[schema] def componentsReversed :List[Component[_]] =
			component :: init.subcomponentsReversed

		override protected[schema] def subcomponentsReversed :List[Component[_]] =
			component :: component.subcomponents.toList reverse_::: init :: init.subcomponentsReversed

		override val subcomponents :Unique[Component[_]] = Unique.Lazy(subcomponentsReversed.reverse)

		protected[schema] override def columnsReversed :List[Column[_]] =
			component.columns.toList reverse_::: init.columnsReversed

		override val columns :Unique[Column[_]] = Unique.Lazy(columnsReversed.reverse)

	}






	private[schema] class NonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                    (init :MappingSchema[C, R, S, O], last :M,
	                                     extractor :ComponentExtractor[S, T, O])
		extends BaseNonEmptySchema[Chain, ~, Any, C, M, R, T, T, S, O](init, last, extractor, _.last)
	{

		protected override def link(init :R, last :T) :R ~ T = init ~ last

		override def compose[X](extractor :X => S) :NonEmptySchema[C, M, R, T, X, O] =
			new NonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :NonEmptySchema[C, M, R, T, X, O] =
			new NonEmptySchema[C, M, R, T, X, O](init compose extractor, last, this.extractor compose extractor)

	}



	private[schema] class ExtensibleNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
                          (init :ExtensibleMappingSchema[C, R, S, O], last :M, extractor :ComponentExtractor[S, T, O])
		extends NonEmptySchema[C, M, R, T, S, O](init, last, extractor) with ExtensibleMappingSchema[C ~ M, R ~ T, S, O]
	{
		protected override val outerBuffs :Seq[Buff[S]] = init.outerBuffs
	}






	private[schema] class FlatNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                                        (override val init :FlatMappingSchema[C, R, S, O], next :M,
	                                         get :ComponentExtractor[S, T, O])
		extends NonEmptySchema[C, M, R, T, S, O](init, next, get) with FlatMappingSchema[C ~ M, R ~ T, S, O]
	{
		override def compose[X](extractor :X => S) :FlatNonEmptySchema[C, M, R, T, X, O] =
			new FlatNonEmptySchema[C, M, R, T, X, O](init compose extractor, component, this.extractor compose extractor)

		override def compose[X](extractor :X =?> S) :FlatNonEmptySchema[C, M, R, T, X, O] =
			new FlatNonEmptySchema[C, M, R, T, X, O](init compose extractor, component, this.extractor compose extractor)
	}



	private[schema] class ExtensibleFlatNonEmptySchema[+C <: Chain, +M <: TypedMapping[T, O], R <: Chain, T, S, O]
	                      (init :ExtensibleFlatMappingSchema[C, R, S, O], next :M, get :ComponentExtractor[S, T, O])
		extends FlatNonEmptySchema[C, M, R, T, S, O](init, next, get) with ExtensibleFlatMappingSchema[C ~ M, R ~ T, S, O]
	{
		protected override val outerBuffs :Seq[Buff[S]] = init.outerBuffs
	}






	private[schema] class MappedMappingSchema[+C <: Chain, R <: Chain, S, O]
	                      (override val schema :MappingSchema[C, R, S, O], constructor :R => S,
	                       override val buffs :Seq[Buff[S]] = Nil)
		extends ShallowAdapter[TypedMapping[R, O], R, S, O] with SchemaMapping[C, R, S, O]
	{
		override protected val egg = schema
		private[this] val schemaExtractor = ComponentExtractor.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtractor.asInstanceOf[Selector[T]]
			else schema.extractor(component)

		override def apply[T](column :Column[T]) :ColumnSelector[T] =
			schema.extractor(column)

		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtractor) map constructor

	}



	private[schema] class FlatMappedMappingSchema[+C <: Chain, R <: Chain, S, O]
			              (override val schema :MappingSchema[C, R, S, O], constructor :R => Option[S],
			               override val buffs :Seq[Buff[S]] = Nil)
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



	private[schema] class MappedFlatMappingSchema[+C <: Chain, R <: Chain, S, O]
	                      (override val schema :FlatMappingSchema[C, R, S, O], constructor :R => S,
	                       buffs :Seq[Buff[S]] = Nil)
		extends MappedMappingSchema[C, R, S, O](schema, constructor, buffs) with FlatSchemaMapping[C, R, S, O]



	private[schema] class FlatMappedFlatMappingSchema[+C <: Chain, R <: Chain, S, O]
	                      (override val schema :FlatMappingSchema[C, R, S, O], constructor :R => Option[S],
	                       buffs :Seq[Buff[S]] = Nil)
		extends FlatMappedMappingSchema[C, R, S, O](schema, constructor, buffs) with FlatSchemaMapping[C, R, S, O]


}


