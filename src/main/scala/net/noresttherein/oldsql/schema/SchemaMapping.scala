package net.noresttherein.oldsql.schema

import net.noresttherein.oldsql
import net.noresttherein.oldsql.collection.Chain.{@~, ~}
import net.noresttherein.oldsql.collection.{Chain, NaturalMap, Unique}
import net.noresttherein.oldsql.morsels.Extractor
import net.noresttherein.oldsql.morsels.abacus.Numeral
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn, MappingLabel}
import net.noresttherein.oldsql.schema.support.{LazyMapping, MappingAdapter, StableMapping, StaticMapping}
import net.noresttherein.oldsql.schema.ColumnMapping.StandardColumn
import net.noresttherein.oldsql.schema.MappingSchema.{EmptySchema, ExtensibleFlatMappingSchema, FlatMappingSchema, GetLabeledComponent, GetSchemaComponent, SchemaFlattening}
import net.noresttherein.oldsql.schema.SchemaMapping.{@|-|, |-|, CustomizedSchemaMapping, FlatSchemaMapping, LabeledSchemaMapping, MappedSchemaMapping, SchemaComponentLabel, SchemaMappingTemplate}
import net.noresttherein.oldsql.schema.bits.{AbstractLabeledMapping, LabeledMapping, MappedMapping}
import net.noresttherein.oldsql.schema.SchemaMapping.LabeledSchemaColumn.LabeledMappedSchemaColumn
import net.noresttherein.oldsql.schema.SchemaMapping.SchemaColumn.{MappedSchemaColumn, SingleColumnSchemaMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.ShallowProxy
import net.noresttherein.oldsql.schema.Buff.{BuffType, ExplicitInsert, ExplicitQuery, ExplicitSelect, ExplicitUpdate, FlagBuffType, NoInsert, NoInsertByDefault, NoQuery, NoQueryByDefault, NoSelect, NoSelectByDefault, NoUpdate, NoUpdateByDefault, OptionalInsert, OptionalQuery, OptionalSelect, OptionalUpdate}
import net.noresttherein.oldsql.schema.support.MappingAdapter.ShallowAdapter
import net.noresttherein.oldsql.schema.Mapping.{MappingSeal, OriginProjection}
import net.noresttherein.oldsql.slang

//implicits:
import slang._




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
  * Additionally, any `SchemaMapping` can be labeled with a `String` literal type using the `labeled` methods,
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
  *     val firstGun = human / "gun" //:LabeledSchemaComponent[_, @~ ~ String ~ String ~ String, Human, "human"]
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
  * @tparam V a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  *
  * @see [[net.noresttherein.oldsql.schema.MappingSchema]]
  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
  * @see [[net.noresttherein.oldsql.schema.MappingSchema.FlatMappingSchema]]
  * @author Marcin Mościcki
  */
trait SchemaMapping[S, V <: Chain, C <:Chain, O] extends TypedMapping[S, O] with |-|[S, V, C] { outer =>

	//todo: this really reads better as a right-associative method
	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.MappingSchema#apply[N](label:N) apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.labeled]]
	  */
	override def apply[N <: Label :ValueOf] :LabeledSchemaMapping[N, S, V, C, O] = this labeled valueOf[N]

	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.MappingSchema.apply apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * Note that this method can sometimes lead the compiler to erroneously infer a unique singleton type for the label,
	  * rather than the literal type denoted by the given string literal. In that case, you may wish to use
	  * the `:@` method instead, which takes the type parameter instead of the singleton value.
	  * @see [[net.noresttherein.oldsql.schema.SchemaMapping#apply]]
	  */
	override def labeled[N <: Label](label :N) :LabeledSchemaMapping[N, S, V, C, O] =
		LabeledSchemaMapping(label, this)



	/** Rebases this mapping to the flat version of its schema, where every non-column component is recursively replaced
	  * with the full list of its columns. The new mapping will delegate its assembly to this instance, and the
	  * values of replaced components will be assembled from the column values.
	  */
	def flatten[FV <: Chain, FC <: Chain]
	           (implicit flatterer :SchemaFlattening[V, C, FV, FC]) :FlatSchemaMapping[S, FV, FC, O] =
		new ShallowProxy[S, O] with FlatSchemaMapping[S, FV, FC, O] {
			override val schema = outer.schema.flatten
			protected override val egg = outer
		}



/*
	private def customized[E <: Chain] //fixme: include must be a chain or result may contain not included optional componnts!
	                      (include :Iterable[Component[_]], ban :BuffType, explicit :BuffType,
	                       optional :BuffType, nonDefault :FlagBuffType)
	                      (implicit result :CustomizeSchema[S, V, C, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		new CustomizedSchemaMapping(this, result(schema, include, ban, explicit, optional, nonDefault))


	def forSelect[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forSelect[E](include)

	def forSelect[E <: Chain](include :Iterable[Component[_]])(implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		customized[E](include, NoSelect, ExplicitSelect, OptionalSelect, NoSelectByDefault)

	def forSelect[E <: Chain](implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forSelect[E](Nil)



	def forQuery[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                        (implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forQuery[E](include)

	def forQuery[E <: Chain](include :Iterable[Component[_]])(implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		customized[E](include, NoQuery, ExplicitQuery, OptionalQuery, NoQueryByDefault)

	def forQuery[E <: Chain](implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forQuery[E](Nil)



	def forUpdate[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forUpdate[E](include)

	def forUpdate[E <: Chain](include :Iterable[Component[_]])(implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		customized[E](include, NoUpdate, ExplicitUpdate, OptionalUpdate, NoUpdateByDefault)

	def forUpdate[E <: Chain](implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forUpdate[E](Nil)



	def forInsert[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forInsert[E](include)

	def forInsert[E <: Chain](include :Iterable[Component[_]])(implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		customized[E](include, NoInsert, ExplicitInsert, OptionalInsert, NoInsertByDefault)

	def forInsert[E <: Chain](implicit result :CustomizeSchema[C, V, S, O, E])
			:SchemaMapping[result.Components, result.Values, S, O] =
		forInsert[E](Nil)
*/

//	override def extracts :NaturalMap[Component, Extract] = schema.packedExtracts
//
//	override def columnExtracts :NaturalMap[Column, ColumnExtract] = schema.packedColumnExtracts



	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :SchemaMapping[X, V, C, O] =
		new MappedSchemaMapping(this, there, back)

	override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :SchemaMapping[X, V, C, O] =
		new MappedSchemaMapping(this, there, back)

	override def optMap[X](there :S => Option[X], back :X => Option[S])
	                      (implicit nulls :NullValue[X]) :SchemaMapping[X, V, C, O] =
		new MappedSchemaMapping(this, Extractor(there), Extractor(back))



	override def toString = sqlName getOrElse schema.members.toSeq.mkString("|-|[", ",", "]")
}






object SchemaMapping {

	/** Starts the process of building a `MappingSchema` and then a `SchemaMapping` by chained calls to component
	  * factory methods. This is the same as `MappingSchema[S, O]` and exists to somewhat alleviate the confusion
	  * between the two.
	  */
	def apply[S] :ExtensibleFlatMappingSchema[S, @~, @~, _] = EmptySchema()

	/** Starts the process of building a `MappingSchema` and then a `SchemaMapping` by chained calls to component
	  * factory methods. This is the same as `MappingSchema[O](buffs)` and exists to somewhat alleviate the confusion
	  * between the two.
	  * @param buffs a list of buffs for the built `SchemaMapping`, inherited by all columns and supporting components
	  *              of the created schema.
	  */
	def apply[S](buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, _] = EmptySchema(buffs)






	trait |-|[S, V <: Chain, C <: Chain] extends Mapping { self :MappingSeal => //self :SchemaMapping[S, V, C, _] =>

		override type Subject = S

		/** The subject type of the schema this mapping is based on. In default implementation, it is a chain
		  * of subject types of components in the `Components` chain. There are some specialized indexed implementation
		  * where each entry in the chain is a key-value pair, where values are the components' subject types and keys
		  * are some arbitrary literal types used as field names. Regardless of the details, on the ''n''-th position
		  * there is always a value containing the of the value of the ''n''-th component in the schema.
		  * Excluding components for the purpose of a particular database operation will likewise exclude the
		  * associated entries in the value chain, which will always stay consistent with the component chain.
		  */
		type Unpacked = V


		/** The chain listing components in this schema. In default implementation, these are all direct, non-synthetic
		  * components. Customizing this mapping by including and excluding certain components for the purpose of
		  * a particular SQL statement will produce `SchemaMapping` instances with schemas being subsequences
		  * of this schema. It follows that this list doesn't necessarily reflect a set of column particular
		  * to any single database operation and modification targeted at one type of access (such as update)
		  * may be invalid for another (such as select).
		  */
		type Components = C

		/** The full type of the schema this mapping is based on.
		  * This alias provides a convenient type declaration which in practical applications are much too verbose
		  * to write by hand. Note that this is the upper bound of the schema type, with `SchemaMapping` subclasses
		  * using subtypes of this type with additional features, such as `FlatMappingSchema` for `FlatSchemaMapping`.
		  */
		type Schema = MappingSchema[S, V, C, Origin]



		/** The container of components of this mapping, itself being a mapping for the chain of values of said components. */
		val schema :MappingSchema[S, V, C, Origin]



		/** Returns the `MappingExtract` for the component labeled with the given string literal in the schema.
		  * If more than one component with the same label exist, the last occurrence is selected.
		  * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
		  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
		  *            of this schema.
		  * @return a `MappingExtract` for the found component, wrapping the getter function from the `Subject` type.
		  * @tparam N the string singleton type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
		  */
		def apply[N <: Label, T](label :N)(implicit get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]])
				:MappingExtract[S, T, Origin] =
			get.extract(schema, label)

		/** Returns the component labeled with the given string literal in the schema. If more than one component with
		  * the same label exist, the last occurrence in the component chain `C` is selected. The return type
		  * is a projection from the base `|-|` (or its related subclass) of unknown `Origin` to a full
		  * `SchemaMapping` with the same `Origin` type as this instance.
	      * @param label a `String` literal, or the value returned by `valueOf[N]` in generic code.
		  * @param get an implicit witness to the existence of a subtype of `@|-|[N, T, _, _]` on the component list `C`
		  *            of this schema.
		  * @param projection an implicit specifying the appropriate `LabeledSchemaMapping[N, T, _, _, _]` subclass
		  *                   for the accessed type `M`.
		  * @return a `LabeledSchemaMapping[N, T, _, _, Origin]` or its subclass.
		  * @tparam N the string singleton type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @tparam M the full type of the returned component, as present on the component list `C`.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
		  */
		def /[N <: Label, T, M <: @|-|[N, T, _, _]]
		     (label :N)(implicit get :GetLabeledComponent[N, V, C, T, M], projection :OriginProjection[M])
				:projection.WithOrigin[Origin] =
			projection(get(schema, label))



		/** Returns the `MappingExtract` for the component at the given position in the schema.
		  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
		  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
		  *            on the component list `C`.
		  * @return a `MappingExtract` for the found component, wrapping the getter function from the `Subject` type.
		  * @tparam I the `Int` literal type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema./]]
		  */
		def apply[I <: Numeral, T](idx :I)(implicit get :GetSchemaComponent[I, V, C, T, |-|[T, _, _]])
				:MappingExtract[S, T, Origin] =
			get.extract(schema, idx)

		/** Returns the component at the given position in the schema. The return type is a projection
		  * from the base `|-|` (or its related subclass) of unknown `Origin` to a full `MappingSchema`
		  * with the same `Origin` type as this instance.
		  * @param idx a zero-based `Int` literal, or the value returned by `valueOf[I]` in generic code.
		  * @param get an implicit witness to the existence of a component `|-|[T, _, _]` at the `idx` position
		  *            on the component list `C`.
		  * @param projection an implicit specifying the proper `SchemaMapping` subclass for the accessed mapping `M`.
		  * @return a component at the `idx` position on the component list `C`
		  *         of a subtype of `SchemaMapping[T, _, _, O]`.
		  * @tparam M the full type of the returned component, as present on the component list `C`.
		  * @tparam I the `Int` literal type of the label key.
		  * @tparam T the subject type of the returned component.
		  * @see [[net.noresttherein.oldsql.schema.MappingSchema.apply[N,T](label:N)]]
		  */
		def /[I <: Numeral, T, M <: |-|[T, _, _]]
		     (idx :I)(implicit get :GetSchemaComponent[I, V, C, T, M], projection :OriginProjection[M])
				:projection.WithOrigin[Origin] =
			projection(get(schema, idx))



		/** Attaches a label type to this mapping, being the singleton type of the given string literal.
		  * A labeled component can be retrieved from the schema using its
		  * [[net.noresttherein.oldsql.schema.MappingSchema#apply[N](label:N) apply(label)]] method, or `String` extension
		  * methods provided by [[net.noresttherein.oldsql.schema.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
		  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
		  * subclasses.
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.|-|.labeled]]
		  */
		def apply[N <: Label :ValueOf]: @|-|[N, S, V, C]

		/** Attaches a label type to this mapping, being the singleton type of the given string literal.
		  * A labeled component can be retrieved from the schema using its
		  * [[net.noresttherein.oldsql.schema.MappingSchema.apply apply(label)]] method, or `String` extension
		  * methods provided by [[net.noresttherein.oldsql.schema.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
		  * methods, available in [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]'s
		  * subclasses.
		  * Note that this method can sometimes lead the compiler to erroneously infer a unique singleton type for the label,
		  * rather than the literal type denoted by the given string literal. In that case, you may wish to use
		  * the `:@` method instead, which takes the type parameter instead of the singleton value.
		  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.|-|.apply]]
		  */
		def labeled[N <: Label](label :N): @|-|[N, S, V, C]

	}



	object |-| {
		implicit def schemaMappingProjection[S, V <: Chain, C <: Chain]
				:OriginProjection[|-|[S, V, C]] { type WithOrigin[O] = SchemaMapping[S, V, C, O] } =
			Mapping.arbitraryOriginProjection[|-|[S, V, C], ({ type M[O] = SchemaMapping[S, V, C, O] })#M]
	}



	trait |||[S, V <: Chain, C <: Chain] extends |-|[S, V, C] { self :MappingSeal => }//{ self :FlatSchemaMapping[S, V, C, _] => }

	object ||| {
		implicit def flatSchemaMappingProjection[S, V <: Chain, C <: Chain]
				:OriginProjection[|||[S, V, C]] { type  WithOrigin[O] = FlatSchemaMapping[S, V, C, O] } =
			Mapping.arbitraryOriginProjection[|||[S, V, C], ({ type M[O] = FlatSchemaMapping[S, V, C, O] })#M]
	}



	trait ||[S] extends |||[S, @~ ~ S, @~ ~ ||[S]] { self :MappingSeal => //self :SchemaColumn[S, _] =>

		override def apply[L <: Label :ValueOf]: L @|| S = labeled(valueOf[L])

		override def labeled[L <: Label](label :L): L @|| S
	}

	object || {
		implicit def schemaColumnProjection[S] :OriginProjection[||[S]] { type WithOrigin[O] = SchemaColumn[S, O] } =
			Mapping.arbitraryOriginProjection[||[S], ({ type M[O] = SchemaColumn[S, O] })#M]
	}



	trait @|-|[L <: Label, S, V <: Chain, C <: Chain] extends |-|[S, V, C] with AbstractLabeledMapping[L] {
		self :MappingSeal =>
//		self :LabeledSchemaMapping[L, S, V, C, _] =>
	}

	object @|-| {
		implicit def labeledSchemaMappingProjection[L <: Label, S, V <: Chain, C <: Chain]
				:OriginProjection[@|-|[L, S, V, C]] { type WithOrigin[O] = LabeledSchemaMapping[L, S, V, C, O] } =
			Mapping.arbitraryOriginProjection[@|-|[L, S, V, C], ({ type M[O] = LabeledSchemaMapping[L, S, V, C, O] })#M]
	}



	trait @|||[L <: Label, S, V <: Chain, C <: Chain] extends |||[S, V, C] with @|-|[L, S, V, C] {
		self :MappingSeal =>
//		this :LabeledFlatSchemaMapping[L, S, V, C, _] =>
	}

	object @||| {
		implicit def labeledFlatSchemaMappingProjection[L <: Label, S, V <: Chain, C <: Chain]
				:OriginProjection[@|||[L, S, V, C]] { type WithOrigin[O] = LabeledFlatSchemaMapping[L, S, V, C, O] } =
			Mapping.arbitraryOriginProjection[@|||[L, S, V, C], ({ type M[O] = LabeledFlatSchemaMapping[L, S, V, C, O] })#M]
	}



	trait @||[L <: Label, S] extends ||[S] with @|||[L, S, @~ ~ S, @~ ~ ||[S]] {
		self :MappingSeal =>
//		self :LabeledSchemaColumn[L, S, _] =>
	}

	object @|| {
		implicit def labeledSchemaColumnProjection[L <: Label, S]
				:OriginProjection[@||[L, S]] { type WithOrigin[O] = LabeledSchemaColumn[L, S, O] } =
			Mapping.arbitraryOriginProjection[L @|| S, ({ type M[O] = LabeledSchemaColumn[L, S, O] })#M]
	}






	/** A `SchemaMapping` variant which uses a `FlatSchemaMapping`, that is the component list `C` contains only
	  * `SchemaColumn`s. Note that the column chain `C` includes all columns of the columns in the mapping
	  * and thus might not be reflective of the select clause of a select statement for the subject type, or
	  * the column list updated with SQL update statements.
	  */
	trait FlatSchemaMapping[S, V <: Chain, C <: Chain, O] extends SchemaMapping[S, V, C, O] { outer =>

		override val schema :FlatMappingSchema[S, V, C, O]

		override def flatten[FV <: Chain, FC <: Chain]
		                    (implicit flatterer :SchemaFlattening[V, C, FV, FC]) :FlatSchemaMapping[S, FV, FC, O] =
			this.asInstanceOf[FlatSchemaMapping[S, FV, FC, O]]



		override def apply[N <: Label :ValueOf] :LabeledFlatSchemaMapping[N, S, V, C, O] = labeled(valueOf[N])

		override def labeled[N <: Label](label :N) :LabeledFlatSchemaMapping[N, S, V, C, O] =
			LabeledSchemaMapping(label, this)



		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :FlatSchemaMapping[X, V, C, O] =
			new MappedFlatSchemaMapping(this, there, back)

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :FlatSchemaMapping[X, V, C, O] =
			new MappedFlatSchemaMapping(this, there, back)

		override def optMap[X](there :S => Option[X], back :X => Option[S])
		                      (implicit nulls :NullValue[X]) :FlatSchemaMapping[X, V, C, O] =
			new MappedFlatSchemaMapping(this, Extractor(there), Extractor(back))



		override def toString = sqlName getOrElse schema.members.toSeq.mkString("|||[", ",", "]")
	}



	/** A single-column schema mapping and a column of a schema mapping at the same time. */
	trait SchemaColumn[S, O] extends SingleColumnSchemaMapping[S, S, O] with ||[S] {

		override def apply[N <: Label :ValueOf] :LabeledSchemaColumn[N, S, O] = labeled(valueOf[N])

		override def labeled[N <: Label](label :N) :LabeledSchemaColumn[N, S, O] =
			LabeledSchemaColumn(label, name, buffs :_*)(form)
	}



	object SchemaColumn {

		def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :SchemaColumn[S, O] =
			new StandardColumn[S, O](name, buffs) with SchemaColumn[S, O] {

				override val schema :FlatMappingSchema[S, @~ ~ S, @~ ~ ||[S], O] =
					MappingSchema[S, O].col(this, Extractor.ident[S])
			}


		trait SingleColumnSchemaMapping[S, T, O]
			extends FlatSchemaMapping[S, @~ ~ T, @~ ~ ||[T], O] with ColumnMapping[S, O]
		{
			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :SingleColumnSchemaMapping[X, T, O] =
				new MappedSchemaColumn[X, T, O](schema compose back)(oldsql.schema.mapForm(form)(there, back))

			override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :SingleColumnSchemaMapping[X, T, O] =
				as(Extractor.req(there), Extractor.req(back))

			override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
					:SingleColumnSchemaMapping[X, T, O] =
				as(Extractor.opt(there), Extractor.opt(back))

			override def toString :String = super[ColumnMapping].toString
		}


		class MappedSchemaColumn[S, T, O](override val schema :FlatMappingSchema[S, @~ ~ T, @~ ~ ||[T], O])
		                                 (implicit override val form :ColumnForm[S])
			extends SingleColumnSchemaMapping[S, T, O]
		{
			protected val column = schema.last.withOrigin[O]
			override def name :String = column.name
		}

	}



	trait LabeledSchemaMapping[N <: Label, S, V <: Chain, C <: Chain, O]
		extends SchemaMapping[S, V, C, O] with LabeledMapping[N, S, O] with @|-|[N, S, V, C]


	trait LabeledFlatSchemaMapping[N <: Label, S, V <: Chain, C <: Chain, O]
		extends FlatSchemaMapping[S, V, C, O] with LabeledSchemaMapping[N, S, V, C, O] with @|||[N, S, V, C]


	object LabeledSchemaMapping {

		def apply[N <: Label, S, V <: Chain, C <: Chain, O](label :N, mapping :SchemaMapping[S, V, C, O])
				:LabeledSchemaMapping[N, S, V, C, O] =
			new LabeledSchemaComponent(label, mapping)

		def apply[N <: Label, S, V <: Chain, C <: Chain, O](label :N, mapping :FlatSchemaMapping[S, V, C, O])
				:LabeledFlatSchemaMapping[N, S, V, C, O] =
			new LabeledFlatSchemaComponent(label, mapping)


		private class LabeledSchemaComponent[N <: Label, S, V <: Chain, C <: Chain, M <: SchemaMapping[S, V, C, O], O]
		                                    (label :N, comp :M)
			extends MappingLabel[N, M, S, O](label, comp) with LabeledSchemaMapping[N, S, V, C, O]
		{
			override val schema = egg.schema

			override def labeled[L <: Label](label :L) :LabeledSchemaMapping[L, S, V, C, O] =
				new LabeledSchemaComponent[L, S, V, C, M, O](label, egg)

			override def toString :String =
				sqlName getOrElse schema.members.toSeq.mkString("'" + label + "@|-|[", ",", "]")
		}



		private class LabeledFlatSchemaComponent[N <: Label, S, V <: Chain, C <: Chain, M <: FlatSchemaMapping[S, V, C, O], O]
		                                        (label :N, egg :M)
			extends LabeledSchemaComponent[N, S, V, C, M, O](label, egg) with LabeledFlatSchemaMapping[N, S, V, C, O]
		{
			override val schema = egg.schema

			override def labeled[L <: Label](label :L) :LabeledFlatSchemaMapping[L, S, V, C, O] =
				new LabeledFlatSchemaComponent[L, S, V, C, M, O](label, egg)

			override def toString :String =
				sqlName getOrElse schema.members.toSeq.mkString("'" + label + "@|||[", ",", "]")
		}

	}



	trait LabeledSchemaColumn[N <: Label, S, O]
		extends SchemaColumn[S, O] with LabeledFlatSchemaMapping[N, S, @~ ~ S, @~ ~ ||[S], O] with @||[N, S]
		   with LabeledColumn[N, S, O]
	{
		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:LabeledMappedSchemaColumn[N, X, S, O] =
			new LabeledMappedSchemaColumn(schema compose back)(oldsql.schema.mapForm(form)(there, back))

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X])
				:LabeledMappedSchemaColumn[N, X, S, O] =
			as(Extractor.req(there), Extractor.req(back))

		override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
				:LabeledMappedSchemaColumn[N, X, S, O] =
			as(Extractor.opt(there), Extractor.opt(back))
	}



	object LabeledSchemaColumn {

		@inline def apply[N <: Label, S :ColumnForm, O](name :N, buffs :Buff[S]*) :LabeledSchemaColumn[N, S, O] =
			apply(name, name, buffs:_*)

		def apply[N <: Label, S :ColumnForm, O](label :N, name :String, buffs :Buff[S]*) :LabeledSchemaColumn[N, S, O] =
			new StandardColumn[S, O](name, buffs) with LabeledSchemaColumn[N, S, O] {
				override val schema :FlatMappingSchema[S, @~ ~ S, @~ ~ ||[S], O] =
					MappingSchema[S, O].col(this, ColumnExtract.ident[S, O](this))

				override def toString = "'" + label + "@||" + super[StandardColumn].toString
			}



		class LabeledMappedSchemaColumn[N <: Label, S, T, O](schema :FlatMappingSchema[S, @~ ~ T, @~ ~ ||[T], O])
		                                                    (implicit sqlForm :ColumnForm[S])
			extends MappedSchemaColumn[S, T, O](schema) with LabeledColumn[N, S, O]
				with LabeledFlatSchemaMapping[N, S, @~ ~ T, @~ ~ ||[T], O]
		{

			override def labeled[L <: Label](label :L) :LabeledMappedSchemaColumn[L, S, T, O] =
				new LabeledMappedSchemaColumn[L, S, T, O](schema)

			override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
					:LabeledMappedSchemaColumn[N, X, T, O] =
				new LabeledMappedSchemaColumn(schema compose back)(oldsql.schema.mapForm(form)(there, back))

			override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X])
					:LabeledMappedSchemaColumn[N, X, T, O] =
				as(Extractor.req(there), Extractor.req(back))

			override def optMap[X](there :S => Option[X], back :X => Option[S])(implicit nulls :NullValue[X])
					:LabeledMappedSchemaColumn[N, X, T, O] =
				as(Extractor.opt(there), Extractor.opt(back))

		}
	}






	/** Extension class adding methods to string literals for retrieving from a schema the component with this label,
	  * its extractor and value. This class is not implicit, as it would need explicit import, and needs implicitly
	  * available `MappingSchema[C, _, S, O]` and `ComponentValues`; instead, an implicit conversion is available within
	  * the `AbstractSchemaMapping` class for the use of subclasses.
	  * @see [[net.noresttherein.oldsql.schema.AbstractSchemaMapping]]
	  */
	class SchemaComponentLabel[N <: Label, S, V <: Chain, C <: Chain, O](private val label :N) extends AnyVal {

		/** Retrieve the value of the component with this label in the implicit schema from implicit `ComponentValues`.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def unary_~[T](implicit schema :MappingSchema[S, V, C, O],
		               get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]],
		               pieces :ComponentValues[S, O]) :T =
			pieces(get.extract(schema, label))

		/** Retrieve the optional value of the component with this label in the implicit schema from implicit
		  * `ComponentValues`. If more then one component with this label is present in the schema,
		  * the last (rightmost) one is taken.
		  */
		def ?[T](implicit schema :MappingSchema[S, V, C, O],
		         get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]], pieces :ComponentValues[S, O]) :Option[T] =
			pieces.get(get.extract(schema, label))

		/** Get the component with this label from the implicit schema.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def ^[T, M <: @|-|[N, T, _, _]]
		     (implicit schema :MappingSchema[S, V, C, O],
		      get :GetLabeledComponent[N, V, C, T, M], projection :OriginProjection[M]) :projection.WithOrigin[O] =
			projection(get(schema, label))

		/** Get the extractor for the component with this label from the implicit schema.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def ?>[T](implicit schema :MappingSchema[S, V, C, O],
		          get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]]) :MappingExtract[S, T, O] =
			get.extract(schema, label)

	}






	trait SchemaMappingTemplate[S, V <: Chain, C <: Chain, O]
		extends ShallowAdapter[MappingSchema[S, V, C, O], S, O] with SchemaMapping[S, V, C, O]
	{

		override def apply[T](component :Component[T]) :Extract[T] =
			if (component eq schema)
				MappingExtract.req(schema)(schema.disassemble _).asInstanceOf[Extract[T]]
			else
				schema.extract(component)

		override def apply[T](column :Column[T]) :ColumnExtract[T] = schema.extract(column)



		override def extracts :NaturalMap[Component, Extract] =
			schema.packedExtracts.updated[Extract, V](schema, MappingExtract.req(schema)(schema.disassemble))

		override def columnExtracts :NaturalMap[Column, ColumnExtract] =
			schema.packedColumnExtracts

	}






	private[schema] abstract class AbstractMappedSchema[S, V <: Chain, C <: Chain, O]
	                               (override val schema :MappingSchema[S, V, C, O], override val buffs :Seq[Buff[S]])
		extends SchemaMappingTemplate[S, V, C, O] with StableMapping[S, O]
	{
		override protected val egg = schema
		protected val schemaExtract :Extract[V] = MappingExtract.opt(schema)(schema.unapply)

		override def apply[T](component :Component[T]) =
			if (component eq schema) schemaExtract.asInstanceOf[Extract[T]]
			else extracts(component)

	}



	private[schema] class MappedSchema[S, V <: Chain, C <: Chain, O]
	                      (schema :MappingSchema[S, V, C, O], constructor :V => S, buffs :Seq[Buff[S]] = Nil)
		extends AbstractMappedSchema(schema, buffs)
	{
		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtract) map constructor
	}



	private[schema] class OptMappedSchema[S, V <: Chain, C <: Chain, O]
			              (schema :MappingSchema[S, V, C, O], constructor :V => Option[S], buffs :Seq[Buff[S]] = Nil)
		extends AbstractMappedSchema(schema, buffs)
	{
		override def assemble(pieces :Pieces) :Option[S] =
			pieces.get(schemaExtract) flatMap constructor
	}



	private[schema] class MappedFlatSchema[S, V <: Chain, C <: Chain, O]
	                      (override val schema :FlatMappingSchema[S, V, C, O], constructor :V => S,
	                       buffs :Seq[Buff[S]] = Nil)
		extends MappedSchema(schema, constructor, buffs) with FlatSchemaMapping[S, V, C, O]



	private[schema] class OptMappedFlatSchema[S, V <: Chain, C <: Chain, O]
	                      (override val schema :FlatMappingSchema[S, V, C, O], constructor :V => Option[S],
	                       buffs :Seq[Buff[S]] = Nil)
		extends OptMappedSchema(schema, constructor, buffs) with FlatSchemaMapping[S, V, C, O]






	private[schema] class CustomizedSchemaMapping[S, V <: Chain, C <: Chain, O]
	                                             (original :SchemaMapping[S, _ <: Chain, _ <: Chain, O],
	                                              override val schema :MappingSchema[S, V, C, O])
		extends SchemaMappingTemplate[S, V, C, O] with StableMapping[S, O]
	{
		protected override val egg = schema

		override def assemble(pieces :Pieces) = original.assemble(pieces)

//		override val extracts = original.extracts
	}


	private[schema] class CustomizedFlatSchemaMapping[S, V <: Chain, C <: Chain, O]
	                                                 (original :FlatSchemaMapping[S, _ <: Chain, _ <: Chain, O],
	                                                  override val schema :FlatMappingSchema[S, V, C, O])
		extends CustomizedSchemaMapping(original, schema) with FlatSchemaMapping[S, V, C, O]






	//these are very close to MappedSchema, OptMappedSchema, MappedFlatSchema, OptMappedFlatSchema,
	//but use T=>S instead of V=>S
	private[schema] class MappedSchemaMapping[S, T, V <: Chain, C <: Chain, O]
	                                         (override val egg :SchemaMapping[T, V, C, O],
	                                          override val map :T =?> S, override val unmap :S =?> T)
	                                         (implicit override val nulls :NullValue[S])
		extends MappedMapping[SchemaMapping[T, V, C, O], T, S, O] with MappingAdapter[SchemaMapping[T, V, C, O], S, O]
			with SchemaMapping[S, V, C, O]
	{
		override val schema :MappingSchema[S, V, C, O] = egg.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:MappedSchemaMapping[X, T, V, C, O] =
			new MappedSchemaMapping(egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X]) :MappedSchemaMapping[X, T, V, C, O] =
			new MappedSchemaMapping(egg, map andThen there, unmap compose back)(mapNulls(there))

		override def optMap[X](there :S => Option[X], back :X => Option[S])
		                      (implicit nulls :NullValue[X]) :MappedSchemaMapping[X, T, V, C, O] =
			as(Extractor(there), Extractor(back))

	}






	private[schema] class MappedFlatSchemaMapping[S, T, V <: Chain, C <: Chain, O]
	                                             (override val egg :FlatSchemaMapping[T, V, C, O],
	                                              override val map :T =?> S, override val unmap :S =?> T)
	                                             (implicit override val nulls :NullValue[S])
		extends MappedMapping[FlatSchemaMapping[T, V, C, O], T, S, O]
		   with MappingAdapter[FlatSchemaMapping[T, V, C, O], S, O] with FlatSchemaMapping[S, V, C, O]
	{
		override val schema :FlatMappingSchema[S, V, C, O] = egg.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:MappedFlatSchemaMapping[X, T, V, C, O] =
			new MappedFlatSchemaMapping(egg, map andThen there, back andThen unmap)(mapNulls(there))

		override def map[X](there :S => X, back :X => S)(implicit nulls :NullValue[X])
				:MappedFlatSchemaMapping[X, T, V, C, O] =
			new MappedFlatSchemaMapping(egg, map andThen there, unmap compose back)(mapNulls(there))

		override def optMap[X](there :S => Option[X], back :X => Option[S])
		                      (implicit nulls :NullValue[X]) :MappedFlatSchemaMapping[X, T, V, C, O] =
			as(Extractor(there), Extractor(back))
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
  * further and enriches `String` literals with methods for both retrieving a component or its extract ''and'' its
  * value, providing implicit `ComponentValues` for the mapping are available. These are written as:
  *   - `"favoritePizza".^` for the component labeled `"favoritePizza"` itself,
  *   - `"favoritePizza".?>` for the `MappingExtract` for the labeled component,
  *   - `~"favoritePizza"` for the value of the component labeled `"favoritePizza"` within the `construct` method,
  *   - `"favoritePizza".?` for the value of such a labeled component in an `Option` when within the `construct` method.
  * {{{
  *     class Humans[O] extends AbstractSchemaMapping(
  *         MappingSchema[Human, O].lbl("favoritePizza", _.favoritePizza).lbl("agricolaRecord", _.agricolaRecord)
  *     ){
  *         override def construct(implicit pieces :Pieces) :Human = Human(~"favoritePizza", ~"agricolaRecord")
  *     }
  * }}}
  *
  *
  * @param contents the schema listing all components of this mapping.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam V a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam S the subject type of this mapping.
  * @tparam O a label type serving to distinguish statically between mappings of the same class but mapping
  * @see [[net.noresttherein.oldsql.schema.SchemaMapping.SchemaComponentLabel]]
  */
abstract class AbstractSchemaMapping[S, V <: Chain, C <: Chain, O](contents :MappingSchema[S, V, C, O])
	extends SchemaMappingTemplate[S, V, C, O] with StaticMapping[S, O] with StableMapping[S, O]
{
	implicit val schema :MappingSchema[S, V, C, O] = contents
	override protected val egg = schema

//	override val extracts :NaturalMap[Component, Extract] = super[SchemaMappingTemplate].extracts
//	override val columnExtracts :NaturalMap[Column, ColumnExtract] = schema.packedColumnExtracts
//
//	override val components :Unique[Component[_]] = schema.components
//	override val subcomponents :Unique[Component[_]] = schema.subcomponents
//	override val columns :Unique[Column[_]] = schema.columns


	/** Implicitly extends string literals with methods getting from the schema the (last) component
	  * with the given label, as well as getters for its value when an implicit `Pieces` instance is available
	  * (such as within this mapping's `construct` method).
	  */
	@inline
	implicit protected[this] def accessByLabel[N <: Label](label :N) :SchemaComponentLabel[N, S, V, C, O] =
		new SchemaComponentLabel[N, S, V, C, O](label)



	override def toString = sqlName getOrElse this.unqualifiedClassName
}






/** A 'flat' variant of [[net.noresttherein.oldsql.schema.AbstractSchemaMapping AbstractSchemaMapping]]
  * (with only columns as components).
  */
abstract class AbstractFlatSchemaMapping[S, V <: Chain, C <: Chain, O](contents :FlatMappingSchema[S, V, C, O])
	extends AbstractSchemaMapping[S, V, C, O](contents) with FlatSchemaMapping[S, V, C, O]
{
	implicit override val schema :FlatMappingSchema[S, V, C, O] = contents
}




