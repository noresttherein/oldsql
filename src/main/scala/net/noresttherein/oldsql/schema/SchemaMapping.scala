package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.abacus.INT
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.Mapping.OriginProjection
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, MappingLabel}
import net.noresttherein.oldsql.schema.support.ComponentProxy.ShallowProxy
import net.noresttherein.oldsql.schema.bits.MappedMapping.FlatMappedMapping
import net.noresttherein.oldsql.schema.support.{LazyMapping, MappingAdapter, StaticMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.BaseColumn
import net.noresttherein.oldsql.schema.MappingSchema.{FlatMappingSchema, GetLabeledComponent, GetSchemaComponent, SchemaComponentLabel, SchemaFlattening}
import net.noresttherein.oldsql.schema.SchemaMapping.{FlatMappedSchemaMapping, FlatSchemaMapping, LabeledSchemaMapping, MappedSchemaMapping, SchemaMappingLabel}
import net.noresttherein.oldsql.schema.bits.{LabeledMapping, MappedMapping}






/** A `Mapping` for type `S` which has the types of all of its components encoded as a type parameter `C`.
  * While this makes the types of larger mappings quite verbose and inconvenient for passing explicitly,
  * it has a couple of advantages over implementations without that feature:
  *   - they can be created in a vey succinct way using the chained call building process:
  *     {{{
  *         case class Gun(make :String, model :String, caliber :Double)
  *         case class Human(gun :Gun, backup :Gun, secondBackup :Gun)
  *
  *         def GunSchema[O] = MappingSchema[Gun, O].col(_.make).col(_.model).col(_.caliber).map(Gun.apply)
  *         def HumanSchema[O] = MappingSchema[Human, O].comp(_.gun, GunSchema).comp(_.backup, GunSchema)
  *                                                     .comp(_.secondBackup, GunSchema).map(Human.apply)
  *     }}}
  *
  *   - they can be used in a type safe way to map rows of arbitrary select statements:
  *     {{{
  *         case class Human(gender :String, height :Int, weight :Int)
  *         def HumanSchema[O] = MappingSchema[Human, O].col(_.gender).col(height).col(_.weight).map(Human.apply)
  *         //can be used for the result of "select gender, avg(height), avg(weight) from humans group by gender"
  *     }}}
  *
  * Additionally, any `SchemaMapping` can be labeled with a `String` literal type using the `@:` and `:@` methods,
  * creating a [[net.noresttherein.oldsql.schema.SchemaMapping.LabeledSchemaMapping LabeledSchemaMapping]]
  * (or [[net.noresttherein.oldsql.schema.SchemaMapping.LabeledSchemaColumn LabeledSchemaColumn]]).
  * Components labeled in this way can be retrieved from the schema or this mapping using
  * [[net.noresttherein.oldsql.schema.SchemaMapping#/ this / label]] and
  * [[net.noresttherein.oldsql.schema.SchemaMapping#apply this(label)]]:
  * {{{
  *     def HumanSchema[O] = MappingSchema[Human, O].comp(_.gun, "gun" @: GunSchema[O])
  *                                                 .comp(_.backup, "backup" @: GunSchema[O])
  *                                                 .comp(_.secondBackup, "backup2" @: GunSchema[O]).map(Human.apply)
  *     val human = HumanSchema["human"]
  *     val firstGun = human / "gun" //:LabeledSchemaMapping[_, @~ ~ String ~ String ~ String, Human, "human"]
  *
  *     val backupExtractor = human("gun")
  *     def backupGun(human :Human) = backupExtractor(human)
  * }}}
  *
  * There are two basic ways of creating a `SchemaMapping`:
  *   - start with building a `MappingSchema` and map the result, as in the examples above;
  *   - extend `AbstractSchemaMapping` and implement `construct`, having access to all components of the schema.
  *
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  *           different fragments of a `ResultSet` when more than one copy is present.
  * @tparam R a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  *
  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
  * @see [[net.noresttherein.oldsql.schema.MappingSchema.FlatMappingSchema]]
  * @author Marcin Mościcki
  */
trait SchemaMapping[+C <:Chain, R <: Chain, S, O] extends GenericMapping[S, O] { outer =>
	/** The container of components of this mapping, itself being a mapping for the chain of values of said components. */
	val schema :MappingSchema[C, R, S, O]


	/** Rebases this mapping to the flat version of its schema, where every non-column component is recursively replaced
	  * with the full list of its columns. The new mapping will delegate its assembly to this instance, and the
	  * values of replaced components will be assembled from the column values.
	  */
	def flatten[U >: C <: Chain, IC <: Chain, IL <: Chain]
	           (implicit flatterer :SchemaFlattening[U, R, S, O, IC, IL]) :FlatSchemaMapping[IC, IL, S, O] =
		new ShallowProxy[S, O] with FlatSchemaMapping[IC, IL, S, O] {
			override val schema = flatterer(outer.schema)
			protected override val egg = outer
		}



	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.MappingSchema.apply[N<:Label] apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.MappingSchema.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * Note that this method can sometimes lead the compiler to erroneously infer a unique singleton type for the label,
	  * rather than the literal type denoted by the given string literal. In that case, you may wish to use
	  * the `:@` method instead, which takes the type parameter instead of the singleton value.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.:@]]
	  */
	def @:[N <: Label](label :N) :LabeledSchemaMapping[N, C, R, S, O] =
		new SchemaMappingLabel[N, SchemaMapping[C, R, S, O], C, R, S, O](label, this)

	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.MappingSchema#apply[N](label:N) apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.MappingSchema.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.@:]]
	  */
	def :@[N <: Label :ValueOf] :LabeledSchemaMapping[N, C, R, S, O] = valueOf[N] @: this


	override def apply[T](component :Component[T]) :Selector[T] =
		if (component eq schema)
			ComponentExtractor.req(schema)(schema.disassemble _).asInstanceOf[Selector[T]]
		else
			schema.extractor(component)



	/** Returns the `ComponentExtractor` for the component labeled with the given string literal in the schema.
	  * If more than one component with the same label exist, the last occurrence is selected.
	  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping./]]
	  */
	def apply[N <: Label, T](label :N)(implicit get :GetLabeledComponent[C, R, LabeledMapping[N, T, O], N, T, O])
			:Selector[T] =
		get.extractor(schema, label)

	/** Returns the component labeled with the given string literal in the schema. If more than one component with
	  * the same label exist, the last occurrence in the component chain `C` is selected.
	  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam N the string singleton type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.apply[N,T](label:N)]]
	  */
	def /[M <: LabeledMapping[N, T, O], N <: Label, T](label :N)(implicit get :GetLabeledComponent[C, R, M, N, T, O]) :M =
		get(schema, label)



	/** Returns the `ComponentExtractor` for the component at the given position in the schema.
	  * @param idx an `Int` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
	  */
	def apply[I <: INT, T](idx :I)(implicit get :GetSchemaComponent[C, R, Component[T], I, T, O])
			:ComponentExtractor[S, T, O] =
		get.extractor(schema, idx)

	/** Returns the component at the given position in the schema.
	  * @param idx an `Int` literal, or the value returned by `valueOf[N]` in generic code.
	  * @tparam M the full type of the returned component, as present on the component list `C`.
	  * @tparam I the `Int` literal type of the label key.
	  * @tparam T the subject type of the returned component.
	  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
	  */
	def /[M <: Component[T], I <: INT, T](idx :I)(implicit get :GetSchemaComponent[C, R, M, I, T, O]) :M =
		get(schema, idx)



	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :SchemaMapping[C, R, X, O] =
		new MappedSchemaMapping[C, R, S, X, O](this, there, back)

	override def flatMap[X](there :S => Option[X], back :X => Option[S])
	                       (implicit nulls :NullValue[X]) :SchemaMapping[C, R, X, O] =
		new FlatMappedSchemaMapping[C, R, S, X, O](this, there, back)
}






object SchemaMapping {

	/** A single-column schema mapping */
	trait SchemaColumn[S, O] extends FlatSchemaMapping[@~ ~ SchemaColumn[S, O], @~ ~ S, S, O] with ColumnMapping[S, O] {
		override val schema :FlatMappingSchema[@~ ~ SchemaColumn[S, O], @~ ~ S, S, O] =
			MappingSchema[S, O].col(this, ComponentExtractor.ident[S, O](this))
	}

	object SchemaColumn {
		def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :SchemaColumn[S, O] =
			new BaseColumn[S, O](name, buffs) with SchemaColumn[S, O]

		implicit def SchemaColumnProjection[S, A, B] :OriginProjection[SchemaColumn[S, A], A, SchemaColumn[S, B], B] =
			OriginProjection()

	}



	implicit def SchemaMappingProjection[AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
			(implicit alias :OriginProjection[MappingSchema[AC, R, S, A], A, MappingSchema[BC, R, S, B], B])
			:OriginProjection[SchemaMapping[AC, R, S, A], A, SchemaMapping[BC, R, S, B], B] =
		Mapping.AnyOrigin()

	implicit def FlatSchemaMappingProjection[AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
			(implicit alias :OriginProjection[FlatMappingSchema[AC, R, S, A], A, FlatMappingSchema[BC, R, S, B], B])
			:OriginProjection[FlatSchemaMapping[AC, R, S, A], A, FlatSchemaMapping[BC, R, S, B], B] =
		Mapping.AnyOrigin()

	implicit def LabeledSchemaMappingProjection[N <: Label, AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
			(implicit alias :OriginProjection[MappingSchema[AC, R, S, A], A, MappingSchema[BC, R, S, B], B])
			:OriginProjection[LabeledSchemaMapping[N, AC, R, S, A], A, LabeledSchemaMapping[N, BC, R, S, B], B] =
		Mapping.AnyOrigin()

	implicit def LabeledFlatSchemaMappingProjection[N <: Label, AC <: Chain, R <: Chain, S, A, BC <: Chain, B]
			(implicit alias :OriginProjection[FlatMappingSchema[AC, R, S, A], A, FlatMappingSchema[BC, R, S, B], B])
			:OriginProjection[LabeledFlatSchemaMapping[N, AC, R, S, A], A, LabeledFlatSchemaMapping[N, BC, R, S, B], B] =
		Mapping.AnyOrigin()






	/** A `SchemaMapping` variant which uses a `FlatSchemaMapping`, that is the component list `C` contains only
	  * `SchemaColumn`s. Note that the column chain `C` includes all columns of the columns in the mapping
	  * and thus might not be reflective of the select clause of a select statement for the subject type, or
	  * the updated column list updated with SQL update statements.
	  */
	trait FlatSchemaMapping[+C <: Chain, R <: Chain, S, O] extends SchemaMapping[C, R, S, O] { outer =>
		override val schema :FlatMappingSchema[C, R, S, O]

		override def flatten[U >: C <: Chain, IC <: Chain, IL <: Chain]
		                    (implicit flatterer :SchemaFlattening[U, R, S, O, IC, IL]) :FlatSchemaMapping[IC, IL, S, O] =
			this.asInstanceOf[FlatSchemaMapping[IC, IL, S, O]]



		override def @:[N <: Label](label :N) :LabeledFlatSchemaMapping[N, C, R, S, O] =
			new FlatSchemaMappingLabel[N, FlatSchemaMapping[C, R, S, O], C, R, S, O](label, this)

		override def :@[N <: Label :ValueOf] :LabeledFlatSchemaMapping[N, C, R, S, O] = valueOf[N] @: this



		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :FlatSchemaMapping[C, R, X, O] =
			new MappedFlatSchemaMapping[C, R, S, X, O](this, there, back)

		override def flatMap[X](there :S => Option[X], back :X => Option[S])
		                       (implicit nulls :NullValue[X]) :SchemaMapping[C, R, X, O] =
			new FlatMappedSchemaMapping[C, R, S, X, O](this, there, back)

	}






	trait LabeledSchemaMapping[N <: Label, +C <: Chain, R <: Chain, S, O]
		extends SchemaMapping[C, R, S, O] with LabeledMapping[N, S, O]

	trait LabeledFlatSchemaMapping[N <: Label, +C <: Chain, R <: Chain, S, O]
		extends FlatSchemaMapping[C, R, S, O] with LabeledSchemaMapping[N, C, R, S, O]






	trait LabeledSchemaColumn[N <: Label, S, O]
		extends SchemaColumn[S, O] with LabeledFlatSchemaMapping[N, @~ ~ LabeledSchemaColumn[N, S, O], @~ ~ S, S, O]
	{
		override val schema :FlatMappingSchema[@~ ~ LabeledSchemaColumn[N, S, O], @~ ~ S, S, O] =
			MappingSchema[S, O].col(this, ComponentExtractor.ident[S, O](this))
	}

	object LabeledSchemaColumn {
		def apply[N <: Label, S :ColumnForm, O](name :N, buffs :Buff[S]*) :LabeledSchemaColumn[N, S, O] =
			new BaseColumn[S, O](name, buffs) with LabeledSchemaColumn[N, S, O]

		implicit def LabeledSchemaColumnProjection[N <: Label, S, A, B]
		:OriginProjection[LabeledSchemaColumn[N, S, A], A, LabeledSchemaColumn[N, S, B], B] =
			OriginProjection()
	}






	private class SchemaMappingLabel[N <: Label, M <: SchemaMapping[C, R, S, O], +C <: Chain, R <: Chain, S, O]
	                                (label :N, egg :M)
		extends MappingLabel[N, M, S, O](label, egg) with LabeledSchemaMapping[N, C, R, S, O]
	{
		override val schema = egg.schema

		override def @:[L <: Label](label :L) :LabeledSchemaMapping[L, C, R, S, O] =
			new SchemaMappingLabel[L, M, C, R, S, O](label, egg)

	}

	private class FlatSchemaMappingLabel[N <: Label, M <: FlatSchemaMapping[C, R, S, O], +C <: Chain, R <: Chain, S, O]
	                                    (label :N, egg :M)
		extends SchemaMappingLabel[N, M, C, R, S, O](label, egg) with LabeledFlatSchemaMapping[N, C, R, S, O]
	{
		override val schema = egg.schema

		override def @:[L <: Label](label :L) :LabeledFlatSchemaMapping[L, C, R, S, O] =
			new FlatSchemaMappingLabel[L, M, C, R, S, O](label, egg)
	}






	private[schema] class MappedSchemaMapping[+C <: Chain, R <: Chain, S, T, O]
	                                         (override val egg :SchemaMapping[C, R, S, O],
	                                          override val map :S => T, override val unmap :T => S)
	                                         (implicit override val nulls :NullValue[T])
		extends MappedMapping[SchemaMapping[C, R, S, O], S, T, O] with MappingAdapter[SchemaMapping[C, R, S, O], T, O]
			with SchemaMapping[C, R, T, O]
	{
		override val schema :MappingSchema[C, R, T, O] = egg.schema compose unmap

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedSchemaMapping[C, R, S, X, O] =
			new MappedSchemaMapping[C, R, S, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, S, X, O] =
			new FlatMappedSchemaMapping[C, R, S, X, O](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	private[schema] class FlatMappedSchemaMapping[+C <: Chain, R <: Chain, S, T, O]
	                                             (mapping :SchemaMapping[C, R, S, O],
	                                              assemble :S => Option[T], disassemble :T => Option[S])
	                                             (implicit override val nulls :NullValue[T])
		extends FlatMappedMapping[SchemaMapping[C, R, S, O], S, T, O](mapping, assemble, disassemble, nulls)
			with MappingAdapter[SchemaMapping[C, R, S, O], T, O] with SchemaMapping[C, R, T, O]
	{
		override val schema :MappingSchema[C, R, T, O] = egg.schema compose Extractor(unmap)

		override def map[X](there :T => X, back :X => T)
		                   (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, S, X, O] =
			new FlatMappedSchemaMapping[C, R, S, X, O](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedSchemaMapping[C, R, S, X, O] =
			new FlatMappedSchemaMapping[C, R, S, X, O](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))
	}






	private[schema] class MappedFlatSchemaMapping[+C <: Chain, R <: Chain, S, T, O]
	                                             (override val egg :FlatSchemaMapping[C, R, S, O],
	                                              override val map :S => T, override val unmap :T => S)
	                                             (implicit override val nulls :NullValue[T])
		extends MappedMapping[FlatSchemaMapping[C, R, S, O], S, T, O] with MappingAdapter[FlatSchemaMapping[C, R, S, O], T, O]
			with FlatSchemaMapping[C, R, T, O]
	{
		override val schema :FlatMappingSchema[C, R, T, O] = egg.schema compose unmap

		override def map[X](there :T => X, back :X => T)(implicit nulls :NullValue[X]) :MappedFlatSchemaMapping[C, R, S, X, O] =
			new MappedFlatSchemaMapping[C, R, S, X, O](egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, S, X, O] =
			new FlatMappedFlatSchemaMapping[C, R, S, X, O](egg, map andThen there, back(_) map unmap)(flatMapNulls(there))
	}



	private[schema] class FlatMappedFlatSchemaMapping[+C <: Chain, R <: Chain, S, T, O]
	                                                 (override val egg :FlatSchemaMapping[C, R, S, O],
	                                                  assemble :S => Option[T], disassemble :T => Option[S])
	                                                 (implicit override val nulls :NullValue[T])
		extends FlatMappedMapping[FlatSchemaMapping[C, R, S, O], S, T, O](egg, assemble, disassemble, nulls)
			with MappingAdapter[FlatSchemaMapping[C, R, S, O], T, O] with FlatSchemaMapping[C, R, T, O]
	{
		override val schema :FlatMappingSchema[C, R, T, O] = egg.schema compose Extractor(unmap)

		override def map[X](there :T => X, back :X => T)
		                   (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, S, X, O] =
			new FlatMappedFlatSchemaMapping[C, R, S, X, O](egg, map(_) map there, back andThen unmap)(mapNulls(there))

		override def flatMap[X](there :T => Option[X], back :X => Option[T])
		                       (implicit nulls :NullValue[X]) :FlatMappedFlatSchemaMapping[C, R, S, X, O] =
			new FlatMappedFlatSchemaMapping[C, R, S, X, O](egg, map(_) flatMap there, back(_) flatMap unmap)(flatMapNulls(there))
	}



}






/** Base trait for `SchemaMapping` implementations which need individual access to their components during
  * the construction process. This class extends `StaticMapping`, meaning that within its
  * [[StaticMapping#construct construct(Pieces)]] method components are implicitly
  * converted into their values, allowing their direct use as arguments for the subject's constructor.
  * Note that accepting a `MappingSchema` as the parameter, all type parameters of this class can be usually inferred
  * automatically:
  * {{{
  *     case class Human(favoritePizza :String, agricolaRecord :Int)
  *     class Humans[O] extends AbstractSchemaMapping(
  *             MappingSchema[Human, O].col(_.favoritePizza).col(_.agricolaRecord)
  *     ){
  *         override def construct(implicit pieces :Pieces) :Human =
  *             Human(schema.prev(), schema.last)
  *     }
  * }}}
  * As the major motivation for picking this class, rather than mapping a `SchemaMapping` with a factory method
  * for the subject type, is free access to the components in the schema and/or more readable code in presence
  * of many columns of the same type, this class offers additional support for labeled components (classes
  * extending [[net.noresttherein.oldsql.schema.bits.LabeledMapping LabeledMapping]]). While `MappingSchema`
  * already provides methods for retrieving such components based on their attached label, this class goes one step
  * further and enriches `String` literals with methods for both retrieving a component or its selector ''and'' its
  * value, providing implicit `ComponentValues` for the mapping are available. These are written as:
  *   - `~"favoritePizza` for the component labeled `"favoritePizza"` itself,
  *   - `"favoritePizza".?>` for the `ComponentExtractor` for the labeled component,
  *   - `!"favoritePizza"` for the value of the component labeled `"favoritePizza"` within the `construct` method,
  *   - `"favoritePizza".?` for the value of such labeled component in an `Option` when within the `construct` method.
  * {{{
  *     class Humans[O] extends AbstractSchemaMapping(
  *         MappingSchema[Human, O].lbl("favoritePizza", _.favoritePizza).lbl("agricolaRecord", _.agricolaRecord)
  *     ){
  *         override def construct(implicit pieces :Pieces) :Human = Human(!"favoritePizza", !"agricolaRecord")
  *     }
  * }}}
  *
  *
  * @param contents the schema listing all components of this mapping.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam R a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  * @see [[net.noresttherein.oldsql.schema.MappingSchema.SchemaComponentLabel]]
  */
abstract class AbstractSchemaMapping[+C <: Chain, R <: Chain, S, O](contents :MappingSchema[C, R, S, O])
	extends SchemaMapping[C, R, S, O] with StaticMapping[S, O] with LazyMapping[S, O]
{
	implicit val schema :MappingSchema[C, R, S, O] = contents

	override val components :Unique[Component[_]] = schema.components
	override val subcomponents :Unique[Component[_]] = schema.subcomponents
	override val columns :Unique[Component[_]] = schema.columns


	/** Implicitly extends string literals with methods getting from the schema the (last) component
	  * with the given label, as well as getters for its value when an implicit `Pieces` instance is available
	  * (such as within this mapping's `construct` method).
	  */
	@inline
	implicit protected[this] def accessByLabel[N <: Label](label :N) :SchemaComponentLabel[C, R, Pieces, N, S, O] =
		new SchemaComponentLabel[C, R, Pieces, N, S, O](label)

}





