package net.noresttherein.oldsql.schema.bits

import scala.annotation.implicitNotFound

import net.noresttherein.oldsql
import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.OperationType.{FILTER, INSERT, SELECT, UPDATE}
import net.noresttherein.oldsql.collection.{Chain, IndexedChain, NaturalMap, Unique}
import net.noresttherein.oldsql.collection.Chain.{@~, ~, ChainContains, ChainGet, ItemExists}
import net.noresttherein.oldsql.collection.IndexedChain.{:~, |~}
import net.noresttherein.oldsql.morsels.InferTypeParams
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.morsels.abacus.{Inc, Numeral}
import net.noresttherein.oldsql.schema.{Buff, ColumnForm, ColumnMapping, ComponentValues, Mapping, MappingExtract}
import net.noresttherein.oldsql.schema.ColumnMapping.{ColumnSupport, StableColumn}
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, OriginProjection, RefinedMapping}
import net.noresttherein.oldsql.schema.Mapping.OriginProjection.{ExactProjection, ProjectionDef}
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bases.{BaseMapping, StableMapping}
import net.noresttherein.oldsql.schema.bases.StaticMapping.StaticMappingTemplate
import net.noresttherein.oldsql.schema.bits.IndexedMappingSchema.{ExtensibleFlatIndexedSchema, ExtensibleIndexedSchema, FlatIndexedMappingSchema}
import net.noresttherein.oldsql.schema.bits.LabeledMapping.{Label, LabeledColumn, MappingLabel}
import net.noresttherein.oldsql.schema.bits.MappingSchema.{AlteredFlatSchema, AlteredSchema, EmptySchema, ExtensibleFlatMappingSchema, ExtensibleMappingSchema, FlatMappingSchema, GetLabeledComponent, MappingSchemaComponents, SchemaFlattening, SubjectConstructor}
import net.noresttherein.oldsql.schema.bits.SchemaMapping.{|-|, CustomizeSchema, DelegateSchemaMapping, FlatSchemaMapping, LabeledSchemaMapping, MappedSchemaMapping, SchemaMappingAdapter, SchemaMappingProxy}
import net.noresttherein.oldsql.schema.bits.SchemaMapping.CustomizeSchema.{ComponentsExist, FilterSchema}
import net.noresttherein.oldsql.schema.support.{AlteredMapping, ColumnMappingFactoryMethods, DelegateMapping, MappedMapping, MappingFactoryMethods, PrefixedMapping, AdjustedMapping}
import net.noresttherein.oldsql.schema.support.AdjustedMapping.{AdjustedMappingMerger, SpecificAdjustedMapping}
import net.noresttherein.oldsql.schema.support.DelegateMapping.ShallowDelegate
import net.noresttherein.oldsql.schema.support.MappingAdapter.{BaseAdapter, ComposedAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.support.MappingProxy.DirectProxy






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
  * creating a [[net.noresttherein.oldsql.schema.bits.SchemaMapping.LabeledSchemaMapping LabeledSchemaMapping]]
  * (or [[net.noresttherein.oldsql.schema.bits.SchemaMapping.LabeledSchemaColumn LabeledSchemaColumn]]).
  * Components labeled in this way can be retrieved from the schema or this mapping using
  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping./ this / label]] and
  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping.apply this(label)]]:
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
  * @tparam S the subject type of this mapping.
  * @tparam V a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  *           Consult [[net.noresttherein.oldsql.schema.Mapping.Origin Mapping.Origin]]
  *
  * @see [[net.noresttherein.oldsql.schema.bits.MappingSchema]]
  * @see [[net.noresttherein.oldsql.schema.bits.MappingSchema.FlatMappingSchema]]
  * @see [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping]]
  * @author Marcin Mościcki
  */
trait SchemaMapping[S, V <: Chain, C <:Chain, O]
	extends BaseMapping[S, O] with |-|[S, V, C]
	   with MappingFactoryMethods[({ type T[X] = SchemaMapping[X, V, C, O] })#T, S, O]
{ outer =>

	//todo: this really reads better as a right-associative method
	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema.apply[N](label:N) apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.labeled]]
	  */
	override def apply[N <: Label :ValueOf] :LabeledSchemaMapping[N, S, V, C, O] = this labeled valueOf[N]

	/** Attaches a label type to this mapping, being the singleton type of the given string literal.
	  * A labeled component can be retrieved from the schema using its
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema.apply apply(label)]] method, or `String` extension
	  * methods provided by [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
	  * methods, available in [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping AbstractSchemaMapping]]'s
	  * subclasses.
	  * Note that this method can sometimes lead the compiler to erroneously infer a unique singleton type for the label,
	  * rather than the literal type denoted by the given string literal. In that case, you may wish to use
	  * the `:@` method instead, which takes the type parameter instead of the singleton value.
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.apply]]
	  */
	override def labeled[N <: Label](label :N) :LabeledSchemaMapping[N, S, V, C, O] =
		LabeledSchemaMapping(label, this)



	/** Rebases this mapping to the flat version of its schema, where every non-column component is recursively replaced
	  * with the full list of its columns. The new mapping will delegate its assembly to this instance, and the
	  * values of replaced components will be assembled from the column values.
	  */
	def flatten[FV <: Chain, FC <: Chain]
	           (implicit flatterer :SchemaFlattening[V, C, FV, FC]) :FlatSchemaMapping[S, FV, FC, O] =
		new DirectProxy[S, O] with FlatSchemaMapping[S, FV, FC, O] {
			override val schema = outer.schema.flatten
			protected override val backer = outer
		}


	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]]) :SchemaMapping[S, V, C, O] =
		AdjustedMapping(this, include, exclude, alter)

	private[this] type Adapter[X] = SchemaMappingAdapter[this.type, S, X, V, C, O]

	protected[schema] def alter(includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
			:SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new AdjustedMapping[this.type, S, O](this)(includes, excludes)
			with DelegateAdapter[this.type, S, O] with SchemaMappingProxy[this.type, S, V, C, O]
			with AdjustedMappingMerger[Adapter, S, O]
		{   //in Scala 3, this method should become a constructor parameter
			protected override def adjustedMapping(includes :Unique[RefinedMapping[_, O]],
			                                       excludes :Unique[RefinedMapping[_, O]]) =
				outer.alter(includes, excludes)
		}


	def forSelect[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[SELECT, this.type, S, O, E]) :result.Result =
		forSelect[E](include)

	def forSelect[E <: Chain](include :Iterable[Component[_]])
	                         (implicit result :CustomizeSchema[SELECT, this.type, S, O, E]) :result.Result =
		customize[SELECT, E](include)

	def forSelect(implicit result :CustomizeSchema[SELECT, this.type, S, O, @~]) :result.Result =
		forSelect[@~](Nil)

	def forSelectExclude[E <: Chain](implicit result :CustomizeSchema[SELECT, this.type, S, O, E]) :result.Result =
		forSelect[E](Nil)


	def forFilter[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[FILTER, this.type, S, O, E]) :result.Result =
		forFilter[E](include)

	def forFilter[E <: Chain](include :Iterable[Component[_]])
	                         (implicit result :CustomizeSchema[FILTER, this.type, S, O, E]) :result.Result =
		customize[FILTER, E](include)

	def forFilter(implicit result :CustomizeSchema[FILTER, this.type, S, O, @~]) :result.Result =
		forFilter[@~](Nil)

	def forFilterExclude[E <: Chain](implicit result :CustomizeSchema[FILTER, this.type, S, O, E]) :result.Result =
		forFilter[E](Nil)


	def forUpdate[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[UPDATE, this.type, S, O, E]) :result.Result =
		forUpdate[E](include)

	def forUpdate[E <: Chain](include :Iterable[Component[_]])
	                         (implicit result :CustomizeSchema[UPDATE, this.type, S, O, E]) :result.Result =
		customize[UPDATE, E](include)

	def forUpdate(implicit result :CustomizeSchema[UPDATE, this.type, S, O, @~]) :result.Result =
		customize[UPDATE, @~](Nil)

	def forUpdateExclude[E <: Chain](implicit result :CustomizeSchema[UPDATE, this.type, S, O, E]) :result.Result =
		forUpdate[E](Nil)



	def forInsert[E <: Chain](include :Iterable[Component[_]], exclude :E)
	                         (implicit result :CustomizeSchema[INSERT, this.type, S, O, E]) :result.Result =
		forInsert[E](include)

	def forInsert[E <: Chain](include :Iterable[Component[_]])
	                         (implicit result :CustomizeSchema[INSERT, this.type, S, O, E]) :result.Result =
		customize[INSERT, E](include)

	def forInsert(implicit result :CustomizeSchema[INSERT, this.type, S, O, @~]) :result.Result =
		customize[INSERT, @~](Nil)

	def forInsertExclude[E <: Chain](implicit result :CustomizeSchema[INSERT, this.type, S, O, E]) :result.Result =
		forInsert[E](Nil)


	//fixme: include must be a chain or result may contain not included optional components!
	private def customize[A <: OperationType, E <: Chain](include :Iterable[Component[_]])
	                                                     (implicit op :A, result :CustomizeSchema[A, this.type, S, O, E])
			:result.Result =
		result(this, op, include)

	protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:SchemaMapping[S, V, C, O] =
		new AlteredMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with SchemaMappingProxy[this.type, S, V, C, O]


	override def prefixed(prefix :String) :SchemaMapping[S, V, C, O] =
		if (prefix.length == 0)
			this
		else
	        new PrefixedMapping[this.type, S, O](prefix, this) with DelegateSchemaMapping[S, V, C, O]


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :SchemaMapping[X, V, C, O] =
		new MappedSchemaMapping(this, there, back)


	override def toString :String = schema.members.toSeq.mkString("|-|[", ",", "]")
}






object SchemaMapping {

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * This is equivalent to `SchemaMapping[S, O]` except it doesn't take the origin type parameter for more
	  * convenient building of schema components, that is instances of
	  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-| |-|]], which have an unspecified `Origin`.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @return an [[net.noresttherein.oldsql.schema.bits.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.bits.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping]]
	  */
	def apply[S] :ExtensibleFlatMappingSchema[S, @~, @~, _] = EmptySchema()

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * This is equivalent to `SchemaMapping[S, O](buffs)` except it doesn't take the origin type parameter for more
	  * convenient building of schema components, that is instances of
	  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-| |-|]], which have an unspecified `Origin`.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @param buffs a list of `Buff`s for the created `SchemaMapping`. They will not show as the buffs
	  *              of the returned schema, but will be nevertheless inherited by all all columns appended to it
	  *              and those components for which factory functions will be given.
	  * @return an [[net.noresttherein.oldsql.schema.bits.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.bits.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping]]
	  */
	def apply[S](buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, _] = EmptySchema(buffs)

	/** An empty `MappingSchema` which can be expanded by appending new columns and components.
	  * At any point the chain of values with the components in the schema can be mapped to the subject type `S`,
	  * creating a [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]].
	  * {{{
	  *     case class Address(street :String, city :String, zip :String)
	  *     def addresses[O] = MappingSchema[Address, O].col("street", _.street).col("city", _.city).col("zip", _.zip)
	  *                                                 .map(Address.apply)
	  * }}}
	  * This is equivalent to `SchemaMapping[S, O](prefix, buffs)` except it doesn't take the origin type parameter
	  * for more convenient building of schema components, that is instances of
	  * [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-| |-|]], which have an unspecified `Origin`.
	  * @tparam S the subject type of a `SchemaMapping` over this schema. All components in the schema have
	  *           extractors which retrieve their value from this type.
	  * @param columnPrefix a `String` prepended to all column names. It will be also passed as an argument
	  *                     to all factory functions used to create new components for the schema.
	  * @param buffs a list of `Buff`s for the created `SchemaMapping`. They will not show as the buffs
	  *              of the returned schema, but will be nevertheless inherited by all all columns appended to it
	  *              and those components for which factory functions will be given.
	  * @return an [[net.noresttherein.oldsql.schema.bits.MappingSchema.ExtensibleFlatMappingSchema ExtensibleFlatMappingSchema]].
	  * @see [[net.noresttherein.oldsql.schema.bits.MappingSchema]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping]]
	  */
	def apply[S](columnPrefix :String, buffs :Buff[S]*) :ExtensibleFlatMappingSchema[S, @~, @~, _] =
		EmptySchema(columnPrefix, buffs)




	//implicit 'override' from |-| which will work for SchemaMapping subclasses as normal.
	implicit def genericSchemaMappingProjection[M[Q] <: SchemaMapping[S, _, _, Q], S, O]
	             (implicit types :M[O] <:< SchemaMapping[S, _, _, O]) :ProjectionDef[M[O], M, S] =
		OriginProjection.isomorphism[M, S, O]



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], with an unspecified `Origin` type.
	  * This makes it suited for inclusion in the [[net.noresttherein.oldsql.schema.bits.MappingSchema.Components Components]]
	  * list of the latter and [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components.
	  *
	  * Every instance of this must be also an instance of `SchemaMapping` and, as an exception, origin projection
	  * of this type (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]]`[O]`
	  * extension method) returns a `SchemaMapping[S, V, C, O]`. Subtypes exist for various specialized `Mapping` types
	  * which may occur as components in a `MappingSchema`.
	  * @tparam S the `Subject` type of this mapping.
	  * @tparam V the value chain, listing the value types of all components. In the default implementation,
	  *           it as a [[net.noresttherein.oldsql.collection.Chain Chain]] where every element is the `Subject` type
	  *           of the component at the same position in the component chain `C`.
	  *           However, in [[net.noresttherein.oldsql.schema.bits.IndexedMappingSchema IndexedMappingSchema]] and
	  *           [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]],
	  *           it is an [[net.noresttherein.oldsql.collection.IndexedChain IndexedChain]], where every entry
	  *           is the `L :~ S` pair, with `L` being the label type of the corresponding component,
	  *           and `S` its subject type. It is also the `Subject` type of the containing `MappingSchema`.
	  * @tparam C the component chain, with every entry being a subtype of `|-|`. It lists all the ''included''
	  *           components of this `Mapping` (and this mapping, from the initial version including all components,
	  *           can be modified into one with only ''selected'' components listed).
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.@|-|]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|||]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.@|||]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.||]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.@||]]
	  */
	trait |-|[S, V <: Chain, C <: Chain] extends MappingSchemaComponents {

		override type Subject = S
		override type Packed = S
		override type Unpacked = V
		override type Components = C
		override type Schema = MappingSchema[S, V, C, Origin]

		/** Type constructor accepting the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type for
		  * a [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] version of this mapping.
		  */
		type SchemaProjection[O] = MappingSchema[S, V, C, O]

		/** The container of components of this mapping, itself being a mapping for the chain of values of said components. */
		override val schema :MappingSchema[S, V, C, Origin]

		/** Attaches a label type to this mapping, being the singleton type of the given string literal.
		  * A labeled component can be retrieved from the schema using its
		  * [[net.noresttherein.oldsql.schema.bits.MappingSchema.apply[N](label:N) apply(label)]] method, or `String` extension
		  * methods provided by [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
		  * methods, available in [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping AbstractSchemaMapping]]'s
		  * subclasses.
		  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|.labeled]]
		  */
		def apply[N <: Label :ValueOf]: @|-|[N, S, V, C]

		/** Attaches a label type to this mapping, being the singleton type of the given string literal.
		  * A labeled component can be retrieved from the schema using its
		  * [[net.noresttherein.oldsql.schema.bits.MappingSchema.apply apply(label)]] method, or `String` extension
		  * methods provided by [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaComponentLabel SchemaComponentLabel]]'s
		  * methods, available in [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping AbstractSchemaMapping]]'s
		  * subclasses.
		  * Note that this method can sometimes lead the compiler to erroneously infer a unique singleton type for the label,
		  * rather than the literal type denoted by the given string literal. In that case, you may wish to use
		  * the `:@` method instead, which takes the type parameter instead of the singleton value.
		  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|.apply]]
		  */
		def labeled[N <: Label](label :N): @|-|[N, S, V, C]

	}



	object |-| {
		implicit def schemaMappingProjection[S, V <: Chain, C <: Chain]
				:ExactProjection[|-|[S, V, C]] { type WithOrigin[O] = SchemaMapping[S, V, C, O] } =
			OriginProjection.projectAs[|-|[S, V, C], ({ type M[O] = SchemaMapping[S, V, C, O] })#M]
	}



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping.FlatSchemaMapping FlatSchemaMapping]],
	  * with an unspecified `Origin` type. This makes it suited for inclusion
	  * in the [[net.noresttherein.oldsql.schema.bits.MappingSchema.Components Components]] list of
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] and
	  * and [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components.
	  *
	  * Every instance of this must be also an instance of `FlatSchemaMapping` and, as an exception, origin projection
	  * of this type (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]]`[O]`
	  * extension method) returns a `FlatSchemaMapping[S, V, C, O]`.
	  * @tparam S the `Subject` type of this mapping.
	  * @tparam V the value chain, listing the value types of all components. In the default implementation,
	  *           it as a [[net.noresttherein.oldsql.collection.Chain Chain]] where every element is the `Subject` type
	  *           of the component at the same position in the component chain `C`.
	  *           However, in [[net.noresttherein.oldsql.schema.bits.IndexedMappingSchema IndexedMappingSchema]] and
	  *           [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]],
	  *           it is an [[net.noresttherein.oldsql.collection.IndexedChain IndexedChain]], where every entry
	  *           is the `L :~ S` pair, with `L` being the label type of the corresponding component,
	  *           and `S` its subject type. It is also the `Subject` type of the containing `MappingSchema`.
	  * @tparam C the component chain, with every entry being a subtype of `|-|`. It lists all the ''included''
	  *           components of this `Mapping` (and this mapping, from the initial version including all components,
	  *           can be modified into one with only ''selected'' components listed).
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|]]
	  */
	trait |||[S, V <: Chain, C <: Chain] extends |-|[S, V, C] //{ self :FlatSchemaMapping[S, V, C, _] => }

	object ||| {
		implicit def flatSchemaMappingProjection[S, V <: Chain, C <: Chain]
				:ExactProjection[|||[S, V, C]] { type WithOrigin[O] = FlatSchemaMapping[S, V, C, O] } =
			OriginProjection.projectAs[|||[S, V, C], ({ type M[O] = FlatSchemaMapping[S, V, C, O] })#M]
	}



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaColumn SchemaColumn]],
	  * with an unspecified `Origin` type. This makes it suited for inclusion
	  * in the [[net.noresttherein.oldsql.schema.bits.MappingSchema.Components Components]] list of
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] and
	  * and [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components.
	  *
	  * Every instance of this type must be also an instance of `SchemaColumn` and, as an exception, origin projection
	  * of this type (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]]`[O]`
	  * extension method) returns a `SchemaColumn[S, O]`. Subtypes exist for various specialized `Mapping` types
	  * which may occur as components in a `MappingSchema`.
	  * @tparam S the `Subject` type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|]]
	  */
	trait ||[S] extends |||[S, @~, @~] {

		override def apply[L <: Label :ValueOf]: L @|| S = labeled(valueOf[L])

		override def labeled[L <: Label](label :L): L @|| S
	}

	object || {
		implicit def schemaColumnProjection[S] :ExactProjection[||[S]] { type WithOrigin[O] = SchemaColumn[S, O] } =
			OriginProjection.projectAs[||[S], ({ type M[O] = SchemaColumn[S, O] })#M]
	}



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping.LabeledSchemaMapping LabeledSchemaMapping]],
	  * with an unspecified `Origin` type. This makes it suited for inclusion
	  * in the [[net.noresttherein.oldsql.schema.bits.MappingSchema.Components Components]] list of
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] and
	  * and [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components. Being labeled, it is possible to access from the containing schema/mapping
	  * by providing the label `L`.
	  *
	  * Every instance of this must be also an instance of `FlatSchemaMapping` and, as an exception, origin projection
	  * of this type (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector.withOrigin withOrigin]]`[O]`
	  * extension method) returns a `FlatSchemaMapping[S, V, C, O]`.
	  * @tparam L a unique string literal identifying this component within the containing schema.
	  * @tparam S the `Subject` type of this mapping.
	  * @tparam V the value chain, listing the value types of all components. In the default implementation,
	  *           it as a [[net.noresttherein.oldsql.collection.Chain Chain]] where every element is the `Subject` type
	  *           of the component at the same position in the component chain `C`.
	  *           However, in [[net.noresttherein.oldsql.schema.bits.IndexedMappingSchema IndexedMappingSchema]] and
	  *           [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]],
	  *           it is an [[net.noresttherein.oldsql.collection.IndexedChain IndexedChain]], where every entry
	  *           is the `L :~ S` pair, with `L` being the label type of the corresponding component,
	  *           and `S` its subject type. It is also the `Subject` type of the containing `MappingSchema`.
	  * @tparam C the component chain, with every entry being a subtype of `|-|`. It lists all the ''included''
	  *           components of this `Mapping` (and this mapping, from the initial version including all components,
	  *           can be modified into one with only ''selected'' components listed).
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|]]
	  */
	trait @|-|[L <: Label, S, V <: Chain, C <: Chain] extends |-|[S, V, C] with AbstractLabeledMapping[L] {
		def label :L
//		self :LabeledSchemaMapping[L, S, V, C, _] =>
	}

	object @|-| {
		implicit def labeledSchemaMappingProjection[L <: Label, S, V <: Chain, C <: Chain]
				:ExactProjection[@|-|[L, S, V, C]] { type WithOrigin[O] = LabeledSchemaMapping[L, S, V, C, O] } =
			OriginProjection.projectAs[@|-|[L, S, V, C], ({ type M[O] = LabeledSchemaMapping[L, S, V, C, O] })#M]
	}



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping.LabeledFlatSchemaMapping LabeledFlatSchemaMapping]],
	  * with an unspecified `Origin` type. This makes it suited for inclusion
	  * in the [[net.noresttherein.oldsql.schema.bits.MappingSchema.Components Components]] list of
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] and
	  * and [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components. Being labeled, it is possible to access from the containing schema/mapping
	  * by providing the label `L`.
	  *
	  * Every instance of this must be also an instance of `LabeledFlatSchemaMapping` and, as an exception,
	  * origin projection of this type
	  * (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector#withOrigin withOrigin]]`[O]`
	  * extension method) returns a `LabeledFlatSchemaMapping[S, V, C, O]`.
	  * @tparam L a unique string literal identifying this component within the containing schema.
	  * @tparam S the `Subject` type of this mapping.
	  * @tparam V the value chain, listing the value types of all components. In the default implementation,
	  *           it as a [[net.noresttherein.oldsql.collection.Chain Chain]] where every element is the `Subject` type
	  *           of the component at the same position in the component chain `C`.
	  *           However, in [[net.noresttherein.oldsql.schema.bits.IndexedMappingSchema IndexedMappingSchema]] and
	  *           [[net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping IndexedSchemaMapping]],
	  *           it is an [[net.noresttherein.oldsql.collection.IndexedChain IndexedChain]], where every entry
	  *           is the `L :~ S` pair, with `L` being the label type of the corresponding component,
	  *           and `S` its subject type. It is also the `Subject` type of the containing `MappingSchema`.
	  * @tparam C the component chain, with every entry being a subtype of `|-|`. It lists all the ''included''
	  *           components of this `Mapping` (and this mapping, from the initial version including all components,
	  *           can be modified into one with only ''selected'' components listed).
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|]]
	  */
	trait @|||[L <: Label, S, V <: Chain, C <: Chain] extends |||[S, V, C] with @|-|[L, S, V, C]

	object @||| {
		implicit def labeledFlatSchemaMappingProjection[L <: Label, S, V <: Chain, C <: Chain]
				:ExactProjection[@|||[L, S, V, C]] { type WithOrigin[O] = LabeledFlatSchemaMapping[L, S, V, C, O] } =
			OriginProjection.projectAs[@|||[L, S, V, C], ({ type M[O] = LabeledFlatSchemaMapping[L, S, V, C, O] })#M]
	}



	/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping.LabeledSchemaColumn LabeledSchemaColumn]],
	  * with an unspecified `Origin` type. This makes it suited for inclusion
	  * in the [[net.noresttherein.oldsql.schema.bits.MappingSchema#Components Components]] list of
	  * [[net.noresttherein.oldsql.schema.bits.MappingSchema MappingSchema]] and
	  * and [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]], as it leaves
	  * the `Origin` type parameter of the two a free variable, not occurring anywhere else in their type,
	  * and thus the [[net.noresttherein.oldsql.schema.Mapping.OriginProjection origin projection]] of these types
	  * remains a simple cast to a type with a different `Origin` type parameter, without a need to also change
	  * the origins of individual components. Being labeled, it is possible to access from the containing schema/mapping
	  * by providing the label `L`.
	  *
	  * Every instance of this must be also an instance of `FlatSchemaMapping` and, as an exception, origin projection
	  * of this type (with the [[net.noresttherein.oldsql.schema.Mapping.MappingOriginProjector#withOrigin withOrigin]]`[O]`
	  * extension method) returns a `FlatSchemaMapping[S, V, C, O]`.
	  * @tparam L a unique string literal identifying this component within the containing schema.
	  * @tparam S the `Subject` type of this mapping.
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.|-|]]
	  */
	trait @||[L <: Label, S] extends ||[S] with @|||[L, S, @~, @~]

	object @|| {
		implicit def labeledSchemaColumnProjection[L <: Label, S]
				:ExactProjection[@||[L, S]] { type WithOrigin[O] = LabeledSchemaColumn[L, S, O] } =
			OriginProjection.projectAs[L @|| S, ({ type M[O] = LabeledSchemaColumn[L, S, O] })#M]
	}
//consider: an adapter labeling type to get rid of all the labeled special component types
//	trait @:[L <: Label, M <: MappingSchemaComponents]
//		extends SchemaMapping[M#Subject, M#Unpacked, M#Components, M#Origin] with LabeledMapping[L, M#Subject, M#Origin]
//		   with MappingAdapter[M, M#Subject, M#Origin] { this :AdapterSeal => }
	type IXI[S, V <: Chain, C <: Chain] = |-|[S, V, C]
	type III[S, V <: Chain, C <: Chain] = |||[S, V, C]
	type I[S] = ||[S]



	/** A `SchemaMapping` variant which uses a `FlatSchemaMapping`, that is the component list `C` contains only
	  * `SchemaColumn`s. Note that the column chain `C` includes all columns of the columns in the mapping
	  * and thus might not be reflective of the select clause of a select statement for the subject type, or
	  * the column list updated with SQL update statements.
	  */
	trait FlatSchemaMapping[S, V <: Chain, C <: Chain, O]
		extends SchemaMapping[S, V, C, O] with |||[S, V, C]
		   with MappingFactoryMethods[({ type T[X] = FlatSchemaMapping[X, V, C, O] })#T, S, O]
	{ outer =>
		override val schema :FlatMappingSchema[S, V, C, O]

		override def flatten[FV <: Chain, FC <: Chain]
		                    (implicit flatterer :SchemaFlattening[V, C, FV, FC]) :FlatSchemaMapping[S, FV, FC, O] =
			this.asInstanceOf[FlatSchemaMapping[S, FV, FC, O]]


		override def apply[N <: Label :ValueOf] :LabeledFlatSchemaMapping[N, S, V, C, O] = labeled(valueOf[N])

		override def labeled[N <: Label](label :N) :LabeledFlatSchemaMapping[N, S, V, C, O] =
			LabeledSchemaMapping(label, this)


		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatSchemaMapping[S, V, C, O] =
			AdjustedMapping(this, include, exclude, alter)

		private[this] type Adapter[X] = FlatSchemaMappingAdapter[this.type, S, X, V, C, O]

		protected[schema] override def alter(includes :Unique[RefinedMapping[_, O]], excludes :Unique[RefinedMapping[_, O]])
				:FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
			new AdjustedMapping[this.type, S, O](this)(includes, excludes)
				with DelegateAdapter[this.type, S, O] with FlatSchemaMappingProxy[this.type, S, V, C, O]
				with AdjustedMappingMerger[Adapter, S, O]
			{   //in Scala 3, this method should become a constructor parameter
				protected override def adjustedMapping(includes :Unique[RefinedMapping[_, O]],
				                                       excludes :Unique[RefinedMapping[_, O]]) =
					outer.alter(includes, excludes)
			}

		protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatSchemaMapping[S, V, C, O] =
			new AlteredMapping[this.type, S, O](this, op, include, exclude)
				with DelegateAdapter[this.type, S, O] with FlatSchemaMappingProxy[this.type, S, V, C, O]


		override def prefixed(prefix :String) :FlatSchemaMapping[S, V, C, O] =
			if (prefix.length == 0)
				this
			else
				new PrefixedMapping[this.type, S, O](prefix, this) with DelegateFlatSchemaMapping[S, V, C, O]


		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :FlatSchemaMapping[X, V, C, O] =
			new MappedFlatSchemaMapping(this, there, back)


		override def toString :String = schema.members.toSeq.mkString("|||[", ",", "]")
	}



	object FlatSchemaMapping {
		//implicit 'override' from ||| which will work for SchemaMapping subclasses as normal.
		implicit def genericFlatSchemaMappingProjection[M[Q] <: FlatSchemaMapping[S, _, _, Q], S, O]
		             (implicit types :M[O] <:< FlatSchemaMapping[S, _, _, O]) :ProjectionDef[M[O], M, S] =
			OriginProjection.isomorphism[M, S, O]


		implicit def implicitCustomizeFlatSchema[A <: OperationType, S, V <: Chain, C <: Chain,
		                                         E <: Chain, FV <: Chain, FC <: Chain, O]
		             (implicit filter :FilterSchema[FlatMappingSchema[S, V, C, O], E, _, FlatMappingSchema[S, FV, FC, O]],
		                       allExist :ComponentsExist[C, E])
				:CustomizeSchema[A, FlatSchemaMapping[S, V, C, O], S, O, E]
					{ type Values = FV; type Components = FC; type Result = FlatOperationSchema[A, S, FV, FC, O] } =
			new CustomizeSchema[A, FlatSchemaMapping[S, V, C, O], S, O, E] {
				override type Values = FV
				override type Components = FC
				override type Result = FlatOperationSchema[A, S, FV, FC, O]

				override def apply(mapping :FlatSchemaMapping[S, V, C, O], op :A, includes :Iterable[RefinedMapping[_, O]]) = {
					val schema = mapping.schema
					val filtered = filter(schema)
					val customized =
						if (filtered eq schema) filtered
						else new AlteredFlatSchema(schema, filtered, op, includes)
					new AlteredFlatSchemaMapping[A, S, FV, FC, O](mapping, customized)
				}
			}
	}



	/** A single-column schema mapping and a column of a schema mapping at the same time. */
	trait SchemaColumn[S, O] extends FlatSchemaMapping[S, @~, @~, O] with ||[S] with ColumnMapping[S, O]
		with ColumnMappingFactoryMethods[({ type A[X] = SchemaColumn[X, O] })#A, S, O]
	{
		override val schema :FlatMappingSchema[S, @~, @~, O] = MappingSchema[S, O]

		override def apply[N <: Label :ValueOf] :LabeledSchemaColumn[N, S, O] = labeled(valueOf[N])

		override def labeled[N <: Label](label :N) :LabeledSchemaColumn[N, S, O] =
			LabeledSchemaColumn(label, name, buffs :_*)(form)


		protected override def thisColumn :SchemaColumn[S, O] = this

		protected override def copy(name :String, buffs :Seq[Buff[S]]) :SchemaColumn[S, O] =
			SchemaColumn(name, buffs :_*)(form)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X]) :SchemaColumn[X, O] =
			SchemaColumn(name, oldsql.schema.mapBuffs(this)(there, back) :_*)(form.as(there)(back))

	}



	object SchemaColumn {

		def apply[S :ColumnForm, O](name :String, buffs :Buff[S]*) :SchemaColumn[S, O] =
			new ColumnSupport[S, O](name, buffs) with SchemaColumn[S, O] with StableColumn[S, O]

		//implicit 'override' from || which will work for SchemaMapping subclasses as normal.
		implicit def genericSchemaColumnProjection[M[Q] <: SchemaColumn[S, Q], S, O]
		             (implicit types :M[O] <:< SchemaColumn[S, O]) :ProjectionDef[M[O], M, S] =
			OriginProjection.isomorphism[M, S, O]
	}



	trait LabeledSchemaMapping[N <: Label, S, V <: Chain, C <: Chain, O]
		extends SchemaMapping[S, V, C, O] with LabeledMapping[N, S, O] with @|-|[N, S, V, C]



	object LabeledSchemaMapping {

		def apply[N <: Label, S, V <: Chain, C <: Chain, O](label :N, mapping :SchemaMapping[S, V, C, O])
				:LabeledSchemaMapping[N, S, V, C, O] =
			new LabeledSchemaComponent(label, mapping)

		def apply[N <: Label, S, V <: Chain, C <: Chain, O](label :N, mapping :FlatSchemaMapping[S, V, C, O])
				:LabeledFlatSchemaMapping[N, S, V, C, O] =
			new LabeledFlatSchemaComponent(label, mapping)


		//implicit 'override' from @|-| which will work for SchemaMapping subclasses as normal.
		implicit def genericLabeledSchemaMappingProjection[M[Q] <: LabeledSchemaMapping[_, S, _, _, Q], S, O]
		             (implicit types :M[O] <:< LabeledSchemaMapping[_, S, _, _, O]) :ProjectionDef[M[O], M, S] =
			OriginProjection.isomorphism[M, S, O]



		private class LabeledSchemaComponent[N <: Label, S, V <: Chain, C <: Chain, M <: SchemaMapping[S, V, C, O], O]
		                                    (override val label :N, comp :M)
			extends MappingLabel[N, M, S, O](label, comp) with LabeledSchemaMapping[N, S, V, C, O]
			   with SchemaMappingProxy[M, S, V, C, O]
		{
			override def labeled[L <: Label](label :L) :LabeledSchemaMapping[L, S, V, C, O] =
				new LabeledSchemaComponent[L, S, V, C, M, O](label, body)

			override def toString :String =
				schema.members.toSeq.mkString("'" + label + "@|-|[", ",", "]")
		}



		private class LabeledFlatSchemaComponent[N <: Label, S, V <: Chain, C <: Chain, M <: FlatSchemaMapping[S, V, C, O], O]
		                                        (label :N, backer :M)
			extends LabeledSchemaComponent[N, S, V, C, M, O](label, backer) with LabeledFlatSchemaMapping[N, S, V, C, O]
			   with FlatSchemaMappingProxy[M, S, V, C, O]
		{
			override def labeled[L <: Label](label :L) :LabeledFlatSchemaMapping[L, S, V, C, O] =
				new LabeledFlatSchemaComponent[L, S, V, C, M, O](label, body)

			override def toString :String = schema.members.toSeq.mkString("'" + label + "@|||[", ",", "]")
		}

	}



	trait LabeledFlatSchemaMapping[N <: Label, S, V <: Chain, C <: Chain, O]
		extends FlatSchemaMapping[S, V, C, O] with LabeledSchemaMapping[N, S, V, C, O] with @|||[N, S, V, C]



	object LabeledFlatSchemaMapping {
		def apply[N <: Label, S, V <: Chain, C <: Chain, O](label :N, mapping :FlatSchemaMapping[S, V, C, O])
				:LabeledFlatSchemaMapping[N, S, V, C, O] =
			LabeledSchemaMapping(label, mapping)

		//implicit 'override' from @||| which will work for SchemaMapping subclasses as normal.
		implicit def genericLabeledFlatSchemaMappingProjection[M[Q] <: LabeledFlatSchemaMapping[_, S, _, _, Q], S, O]
		             (implicit types :M[O] <:< LabeledFlatSchemaMapping[_, S, _, _, O]) :ProjectionDef[M[O], M, S] =
			OriginProjection.isomorphism[M, S, O]

	}



	trait LabeledSchemaColumn[N <: Label, S, O]
		extends SchemaColumn[S, O] with LabeledFlatSchemaMapping[N, S, @~, @~, O] with @||[N, S]
		   with LabeledColumn[N, S, O]
		   with ColumnMappingFactoryMethods[({ type A[X] = LabeledSchemaColumn[N, X, O] })#A, S, O]
	{
//		def label :N

		protected override def thisColumn :LabeledSchemaColumn[N, S, O] = this

		protected override def copy(name :String, buffs :Seq[Buff[S]]) :LabeledSchemaColumn[N, S, O] =
			LabeledSchemaColumn(label, name, buffs :_*)(form)

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:LabeledSchemaColumn[N, X, O] =
			LabeledSchemaColumn(label, name, oldsql.schema.mapBuffs(this)(there, back) :_*)(
				form.as(there)(back)
			)

		override def toString :String = "'" + label + "@||" + super[LabeledColumn].toString
	}



	object LabeledSchemaColumn {

		@inline def apply[N <: Label, S :ColumnForm, O](name :N, buffs :Buff[S]*) :LabeledSchemaColumn[N, S, O] =
			apply(name, name, buffs:_*)

		def apply[N <: Label, S :ColumnForm, O](lbl :N, name :String, buffs :Buff[S]*) :LabeledSchemaColumn[N, S, O] =
			new ColumnSupport[S, O](name, buffs) with LabeledSchemaColumn[N, S, O] with StableColumn[S, O] {
				override val label = lbl
			}

		//implicit 'override' from @|| which will work for SchemaMapping subclasses as normal.
		implicit def genericLabeledSchemaColumnProjection[M[Q] <: LabeledSchemaColumn[_, S, Q], S, O]
		             (implicit types :M[O] <:< LabeledSchemaColumn[_, S, O]) :ProjectionDef[M[O], M, S] =
			OriginProjection.isomorphism[M, S, O]

	}







	/** A `Mapping` implementation dedicated to a single database operation type `A`.
	  * It lists the components which should be included in the operation on the type level as the components chain `C`,
	  * and their values as the chain `V`.  Dedicated instances are obtained from a `SchemaMapping` via the customizing
	  * methods which include and exclude components: `forSelect`, `forFilter`, `forUpdate`, `forInsert`.
	  * The purpose of this separation is to prevent an instance dedicated to one operation type to be adapted
	  * to a second operation type, as mandatory components for the second operation type can be omitted
	  * by the first customization.
      * @tparam S the subject type of this mapping.
	  * @tparam V a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
	  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
	  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
	  *           but coming from different sources (especially different aliases for a table occurring more then once
	  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
	  *           included in a query can be used in the creation of SQL expressions used by that query.
	  *           Consult [[net.noresttherein.oldsql.schema.Mapping.Origin Mapping.Origin]]
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping]]
	  */
	trait OperationSchema[-A <: OperationType, S, V <: Chain, C <: Chain, O]
		extends BaseMapping[S, O] with MappingSchemaComponents
	{
		override type Packed = S
		override type Unpacked = V
		override type Components = C
	}

	/** A `Mapping` implementation dedicated to a single database operation type `A`.
	  * It lists the columns which should be included in the operation on the type level as the components chain `C`,
	  * with no non-column components.
	  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.FlatSchemaMapping]]
	  */
	trait FlatOperationSchema[-A <: OperationType, S, V <: Chain, C <: Chain, O] extends OperationSchema[A, S, V, C, O]






	/** Extension class adding methods to a string literal for retrieving from a schema the component with this label,
	  * its extractor and value. This class is not implicit, as it would need explicit import, and needs implicitly
	  * available `MappingSchema[C, _, S, O]` and `ComponentValues`; instead, an implicit conversion is available within
	  * the `AbstractSchemaMapping` class for the use by subclasses.
	  * @see [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping]]
	  */
	class SchemaComponentLabel[N <: Label, S, V <: Chain, C <: Chain, O](private val label :N) extends AnyVal {

		/** Retrieve the value of the component with this label in the implicit schema from implicit `ComponentValues`.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def unary_~[T](implicit schema :MappingSchema[S, V, C, O],
		               get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]], pieces :ComponentValues[S, O]) :T =
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
		      get :GetLabeledComponent[N, V, C, T, M], projection :OriginProjection[M, T]) :projection.WithOrigin[O] =
			projection(get(schema, label))

		/** Get the extractor for the component with this label from the implicit schema.
		  * If more then one component with this label is present in the schema, the last (rightmost) one is taken.
		  */
		def ?>[T](implicit schema :MappingSchema[S, V, C, O],
		          get :GetLabeledComponent[N, V, C, T, @|-|[N, T, _, _]]) :MappingExtract[S, T, O] =
			get.extract(schema, label)

	}






	/** Implements most missing `SchemaMapping` methods by delegating to the `schema` property.
	  * Must be mixed in ''after'' the initialization of the `schema` property.
	  */
	trait MappingSchemaDelegate[+M <: MappingSchema[S, V, C, O], S, V <: Chain, C <: Chain, O]
		extends ShallowDelegate[S, O] with DelegateMapping[M, S, O] with SchemaMapping[S, V, C, O]
	{
		override val schema :M = backer

		override def buffs :Seq[Buff[S]] = backer.outerBuffs


		protected val schemaExtract = MappingExtract.opt(schema)(schema.unapply)
		protected val selfExtract = MappingExtract.ident(this)

		override def apply[T](component :Component[T]) :Extract[T] =
			if (component eq schema) schemaExtract.asInstanceOf[Extract[T]]
			else if (component eq this) selfExtract.asInstanceOf[Extract[T]]
			else schema.extract(component)

		override def apply[T](column :Column[T]) :ColumnExtract[T] = schema.extract(column)



		override def extracts :NaturalMap[Component, Extract] =
			schema.packedExtracts.updated[Extract, V](schema, schemaExtract)

		override def columnExtracts :NaturalMap[Column, ColumnExtract] =
			schema.packedColumnExtracts

	}



	trait StaticSchemaMapping[+A[B <: RefinedMapping[S, O], T] <: SchemaMapping[T, V, C, O], +M <: MappingSchema[S, V, C, O],
		                      S, V <: Chain, C <: Chain, O]
		extends StaticMappingTemplate[A, S, O] with StableMapping
	{ this :MappingSchemaDelegate[M, S, V, C, O] =>

		implicit override val schema :M = backer

		/** Implicitly extends string literals with methods getting from the schema the (last) component
		  * with the given label, as well as getters for its value when an implicit `Pieces` instance is available
		  * (such as within this mapping's `construct` method).
		  */
		@inline implicit protected[this] def accessByLabel[N <: Label](label :N) :SchemaComponentLabel[N, S, V, C, O] =
			new SchemaComponentLabel[N, S, V, C, O](label)


		override def toString :String = mappingName
	}





	trait SchemaMappingAdapter[+M <: RefinedMapping[T, O], T, S, V <: Chain, C <: Chain, O]
		extends SchemaMapping[S, V, C, O] with BaseAdapter[M, S, O]
		   with MappingFactoryMethods[({ type A[X] = SchemaMappingAdapter[M, T, X, V, C, O] })#A, S, O]
	{
		private[this] type Adapter[X] = SchemaMappingAdapter[M, T, X, V, C, O]

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:SchemaMappingAdapter[M, T, S, V, C, O] =
			new AdjustedMapping[this.type, S, O](this, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateSchemaMapping[S, V, C, O]
				with SchemaMappingAdapter[M, T, S, V, C, O]
				with SpecificAdjustedMapping[Adapter, this.type, S, O]

		protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:SchemaMappingAdapter[M, T, S, V, C, O] =
			new AlteredMapping[this.type, S, O](this, op, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateSchemaMapping[S, V, C, O]
				with SchemaMappingAdapter[M, T, S, V, C, O]


		override def prefixed(prefix :String) :SchemaMappingAdapter[M, T, S, V, C, O] =
			if (prefix.length == 0)
				this
			else
                new PrefixedMapping[SchemaMappingAdapter[M, T, S, V, C, O], S, O](prefix, this)
	                with ComposedAdapter[M, S, S, O] with DelegateSchemaMapping[S, V, C, O]
					with SchemaMappingAdapter[M, T, S, V, C, O]



		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:SchemaMappingAdapter[M, T, X, V, C, O] =
			new MappedSchemaMapping[SchemaMappingAdapter[M, T, S, V, C, O], S, X, V, C, O](this, there, back)
				with ComposedAdapter[M, S, X, O] with SchemaMappingAdapter[M, T, X, V, C, O]
			{
				override def as[Z](there :X =?> Z, back :Z =?> X)(implicit nulls :NullValue[Z])
						:SchemaMappingAdapter[M, T, Z, V, C, O] =
					backer.as(map andThen there, back andThen unmap)
			}
	}



	trait SchemaMappingProxy[M <: SchemaMapping[S, V, C, O], S, V <: Chain, C <: Chain, O]
		extends SchemaMappingAdapter[M, S, S, V, C, O]
	{
		override val schema = body.schema
	}



	trait FlatSchemaMappingAdapter[M <: RefinedMapping[T, O], T, S, V <: Chain, C <: Chain, O]
		extends FlatSchemaMapping[S, V, C, O] with SchemaMappingAdapter[M, T, S, V, C, O]
		   with MappingFactoryMethods[({ type A[X] = FlatSchemaMappingAdapter[M, T, X, V, C, O] })#A, S, O]
	{
		private[this] type Adapter[X] = FlatSchemaMappingAdapter[M, T, X, V, C, O]

		override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatSchemaMappingAdapter[M, T, S, V, C, O] =
			new AdjustedMapping[this.type, S, O](this, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateFlatSchemaMapping[S, V, C, O]
				with FlatSchemaMappingAdapter[M, T, S, V, C, O]
				with SpecificAdjustedMapping[Adapter, this.type, S, O]

		protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
				:FlatSchemaMappingAdapter[M, T, S, V, C, O] =
			new AlteredMapping[this.type, S, O](this, op, include, exclude)
				with ComposedAdapter[M, S, S, O] with DelegateFlatSchemaMapping[S, V, C, O]
				with FlatSchemaMappingAdapter[M, T, S, V, C, O]


		override def prefixed(prefix :String) :FlatSchemaMappingAdapter[M, T, S, V, C, O] =
			if (prefix.length == 0)
				this
			else
				new PrefixedMapping[FlatSchemaMappingAdapter[M, T, S, V, C, O], S, O](prefix, this)
					with ComposedAdapter[M, S, S, O] with DelegateFlatSchemaMapping[S, V, C, O]
					with FlatSchemaMappingAdapter[M, T, S, V, C, O]



		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:FlatSchemaMappingAdapter[M, T, X, V, C, O] =
			new MappedFlatSchemaMapping[FlatSchemaMappingAdapter[M, T, S, V, C, O], S, X, V, C, O](this, there, back)
				with ComposedAdapter[M, S, X, O] with FlatSchemaMappingAdapter[M, T, X, V, C, O]
			{
				override def as[Z](there :X =?> Z, back :Z =?> X)(implicit nulls :NullValue[Z])
						:FlatSchemaMappingAdapter[M, T, Z, V, C, O] =
					backer.as(map andThen there, back andThen unmap)
			}
	}


	trait FlatSchemaMappingProxy[M <: FlatSchemaMapping[S, V, C, O], S, V <: Chain, C <: Chain, O]
		extends FlatSchemaMappingAdapter[M, S, S, V, C, O] with SchemaMappingProxy[M, S, V, C, O]
	{
		override val schema = body.schema
	}



	private trait DelegateSchemaMapping[S, V <: Chain, C <: Chain, O]
		extends DelegateMapping[SchemaMapping[S, V, C, O], S, O] with SchemaMapping[S, V, C, O]
	{
		override val schema = backer.schema
	}

	private trait DelegateFlatSchemaMapping[S, V <: Chain, C <: Chain, O]
		extends DelegateMapping[FlatSchemaMapping[S, V, C, O], S, O] with FlatSchemaMapping[S, V, C, O]
	{
		override val schema = backer.schema
	}







	@implicitNotFound("Can't exclude components ${E} from schema:\n${X}.\n " +
	                  "Exclude type parameter must be a concrete Chain subtype with only literal Int and String " +
	                  "element types denoting the indices/labels of top level components to exclude.\n" +
		              "Missing implicit CustomizeSchema[${X}, ${A}, ${S}, ${O}, ${E}]: enable -Xlog-implicits " +
	                  "for a more detailed reason.")
	abstract class CustomizeSchema[A <: OperationType, -X <: SchemaMapping[_, _ <: Chain, _ <: Chain, _], S, O, E <: Chain] {
		type Values <: Chain
		type Components <: Chain
		type Result <: OperationSchema[A, S, Values, Components, O]
		def apply(schema :X, op :A, include :Iterable[RefinedMapping[_, O]]) :Result
	}



	sealed abstract class LowPrioritySchemaCustomizationImplicits {

		implicit def includeInSchema[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain],
		                             E <: Chain, I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain, O]
		                            (implicit prefix :FilterSchema[MappingSchema[S, V, C, O], E, I,
			                                                       ExtensibleMappingSchema[S, FV, FC, O]])
				:FilterSchema[MappingSchema[S, V ~ T, C ~ M, O], E, J, ExtensibleMappingSchema[S, FV ~ T, FC ~ M, O]] =
			schema => prefix(schema.prev).append(schema.last, schema.lastExtract)

		implicit def includeInFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T],
			                             E <: Chain, I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain, O]
		                                (implicit prefix :FilterSchema[FlatMappingSchema[S, V, C, O], E, I,
			                                                           ExtensibleFlatMappingSchema[S, FV, FC, O]])
				:FilterSchema[FlatMappingSchema[S, V ~ T, C ~ M, O], E, J, ExtensibleFlatMappingSchema[S, FV ~ T, FC ~ M, O]] =
			schema => prefix(schema.prev).col(schema.last, schema.lastExtract)


		implicit def includeInIndexedSchema[S, V <: IndexedChain, C <: Chain, N <: Label, T, M <: @|-|[N, T, _ <: Chain, _ <: Chain],
		                                    E <: Chain, I <: Numeral, J <: Numeral, FV <: IndexedChain, FC <: Chain, O]
		                            (implicit prefix :FilterSchema[IndexedMappingSchema[S, V, C, O], E, I,
			                                                       ExtensibleIndexedSchema[S, FV, FC, O]])
				:FilterSchema[IndexedMappingSchema[S, V |~ (N :~ T), C ~ M, O], E, J,
				              ExtensibleIndexedSchema[S, FV |~ (N :~ T), FC ~ M, O]] =
			schema => prefix(schema.prev).append(schema.last, schema.lastExtract)

		implicit def includeInFlatIndexedSchema[S, V <: IndexedChain, C <: Chain, N <: Label, T, M <: @||[N, T],
			                                    E <: Chain, I <: Numeral, J <: Numeral, FV <: IndexedChain, FC <: Chain, O]
		                                       (implicit prefix :FilterSchema[FlatIndexedMappingSchema[S, V, C, O], E, I,
			                                                                  ExtensibleFlatIndexedSchema[S, FV, FC, O]])
				:FilterSchema[FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ M, O], E, J,
				              ExtensibleFlatIndexedSchema[S, FV |~ (N :~ T), FC ~ M, O]] =
			schema => prefix(schema.prev).col(schema.last, schema.lastExtract)

	}



	object CustomizeSchema extends LowPrioritySchemaCustomizationImplicits {

		@implicitNotFound("Component ${M} at index ${N} is not on the exclude list ${E}.\n+" +
		                  "Missing implicit ExcludeComponent[${M}, ${N}, ${E}.")
		class ExcludeComponent[M, N <: Numeral, E <: Chain] private[CustomizeSchema] ()

		private[this] val instance = new ExcludeComponent[Mapping, 0, Chain]

		implicit def excludeByIndex[M <: Mapping, N <: Numeral, E <: Chain]
		                           (implicit included :ChainContains[E, N]) :ExcludeComponent[M, N, E] =
			instance.asInstanceOf[ExcludeComponent[M, N, E]]

		implicit def excludeByLabel[M <: LabeledMapping[L, _, _], N <: Numeral, L <: Label, E <: Chain]
		             (implicit inferLabel :InferTypeParams[M, M, @|-|[L, _, _, _]], included :ChainContains[E, L])
				:ExcludeComponent[M, N, E] =
			instance.asInstanceOf[ExcludeComponent[M, N, E]]




		@implicitNotFound("Failed to exclude components ${E} from the component chain ${C}.\n" +
			              "This is typically caused by a non-existing component index or label string on the exclude list. " +
			              "No implicit for FilterSchema[${S}, ${V}, ${C}, ${E}, ${N}, ${FV}, ${FC}, ${O}].")
		abstract class FilterSchema[-X, E <: Chain, N <: Numeral, +R] {
			def apply(schema :X) :R
		}


		private[this] val filterEmpty
				:FilterSchema[MappingSchema[Any, @~, @~, Any], @~, 0, ExtensibleFlatMappingSchema[Any, @~, @~, Any]] =
			_ => EmptySchema()

		private[this] val filterEmptyIndexed
				:FilterSchema[IndexedMappingSchema[Any, @~, @~, Any], @~, 0, ExtensibleFlatIndexedSchema[Any, @~, @~, Any]] =
			_ => IndexedMappingSchema.apply

		implicit def filterEmptySchema[S, O, E <: Chain]
				:FilterSchema[MappingSchema[S, @~, @~, O], E, 0, ExtensibleFlatMappingSchema[S, @~, @~, O]] =
			filterEmpty.asInstanceOf[FilterSchema[MappingSchema[S, @~, @~, O], E, 0, ExtensibleFlatMappingSchema[S, @~, @~, O]]]

		implicit def filterEmptyIndexedSchema[S, O, E <: Chain]
				:FilterSchema[IndexedMappingSchema[S, @~, @~, O], E, 0, ExtensibleFlatIndexedSchema[S, @~, @~, O]] =
			filterEmptyIndexed.asInstanceOf[FilterSchema[IndexedMappingSchema[S, @~, @~, O], E, 0, ExtensibleFlatIndexedSchema[S, @~, @~, O]]]


		implicit def unfilteredSchema[X <: MappingSchema[_, _ <: Chain, _ <: Chain, _]]
				:FilterSchema[X, @~, _, X] =
			schema => schema


		implicit def excludeFromSchema[S, V <: Chain, C <: Chain, T, M <: |-|[T, _ <: Chain, _ <: Chain],
			                           E <: Chain, I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain, O]
		                              (implicit prefix :FilterSchema[MappingSchema[S, V, C, O], E, I,
			                                                         ExtensibleMappingSchema[S, FV, FC, O]],
		                               inc :Inc[I, J], exclude :ExcludeComponent[M, I, E])
				:FilterSchema[MappingSchema[S, V ~ T, C ~ M, O], E, J, ExtensibleMappingSchema[S, FV, FC, O]] =
			schema => prefix(schema.prev)

		implicit def excludeFromFlatSchema[S, V <: Chain, C <: Chain, T, M <: ||[T],
			                               E <: Chain, I <: Numeral, J <: Numeral, FV <: Chain, FC <: Chain, O]
		                                  (implicit prefix :FilterSchema[FlatMappingSchema[S, V, C, O], E, I,
			                                                             ExtensibleFlatMappingSchema[S, FV, FC, O]],
		                                   inc :Inc[I, J], exclude :ExcludeComponent[M, I, E])
				:FilterSchema[FlatMappingSchema[S, V ~ T, C ~ M, O], E, J, ExtensibleFlatMappingSchema[S, FV, FC, O]] =
			schema => prefix(schema.prev)

		implicit def excludeFromIndexedSchema[S, V <: IndexedChain, C <: Chain, N <: Label, T, M <: @|-|[N, T, _, _],
			                                  E <: Chain, I <: Numeral, J <: Numeral, FV <: IndexedChain, FC <: Chain, O]
		                                     (implicit prefix :FilterSchema[IndexedMappingSchema[S, V, C, O], E, I,
			                                                                ExtensibleIndexedSchema[S, FV, FC, O]],
		                                      inc :Inc[I, J], exclude :ExcludeComponent[M, I, E])
				:FilterSchema[IndexedMappingSchema[S, V |~ (N :~ T), C ~ M, O], E, J, ExtensibleIndexedSchema[S, FV, FC, O]] =
			schema => prefix(schema.prev)

		implicit def excludeFromFlatIndexedSchema[S, V <: IndexedChain, C <: Chain, N <: Label, T, M <: @||[N, T],
			                                      E <: Chain, I <: Numeral, J <: Numeral, FV <: IndexedChain, FC <: Chain, O]
		                                         (implicit prefix :FilterSchema[FlatIndexedMappingSchema[S, V, C, O], E, I,
			                                                                    ExtensibleFlatIndexedSchema[S, FV, FC, O]],
		                                          inc :Inc[I, J], exclude :ExcludeComponent[M, I, E])
				:FilterSchema[FlatIndexedMappingSchema[S, V |~ (N :~ T), C ~ M, O], E, J, ExtensibleFlatIndexedSchema[S, FV, FC, O]] =
			schema => prefix(schema.prev)




		@implicitNotFound("Not all items in chain ${E} identify components in chain ${C}. " +
			              "Valid members are String literals (component labels) and Int literals (component indices).")
		class ComponentsExist[C <: Chain, E <: Chain] private[CustomizeSchema] ()

		private[this] val exists = new ComponentsExist[Chain, Chain]

		implicit def noExclusions[C <: Chain] :ComponentsExist[C, @~] = exists.asInstanceOf[ComponentsExist[C, @~]]

		implicit def excludedIndex[C <: Chain, N <: Numeral, E <: Chain]
		                          (implicit previousExist :ComponentsExist[C, E], lastExists :ChainGet[C, N, _])
				:ComponentsExist[C, E ~ N] =
			exists.asInstanceOf[ComponentsExist[C, E ~ N]]

		implicit def excludeLabel[C <: Chain, L <: Label, E <: Chain]
		                         (implicit previousExist :ComponentsExist[C, E],
		                          lastExists :ItemExists[C, ({ type T[X] = X <:< @|-|[L, _, _, _] })#T, _])
				:ComponentsExist[C, E ~ L] =
			exists.asInstanceOf[ComponentsExist[C, E ~ L]]



		private[schema] def excluded[S, FV <: Chain, FC <: Chain, V <: Chain, C <: Chain, O]
		                            (filtered :MappingSchema[S, FV, FC, O], schema :MappingSchema[S, V, C, O]) =
		{
			val components = schema.members.toSeq.asInstanceOf[Seq[SchemaMapping[_, _, _, O]]]
			val remaining = filtered.members.toSeq.asInstanceOf[Seq[SchemaMapping[_, _, _, O]]].toSet
			components.filterNot(remaining)
		}

	}



	implicit def implicitCustomizeSchemaMapping[A <: OperationType, S, V <: Chain, C <: Chain,
	                                            E <: Chain, FV <: Chain, FC <: Chain, O]
	             (implicit filter :FilterSchema[MappingSchema[S, V, C, O], E, _, MappingSchema[S, FV, FC, O]],
	                       allExist :ComponentsExist[C, E])
			:CustomizeSchema[A, SchemaMapping[S, V, C, O], S, O, E]
				{ type Values = FV; type Components = FC; type Result = OperationSchema[A, S, FV, FC, O] } =
		new CustomizeSchema[A, SchemaMapping[S, V, C, O], S, O, E] {
			override type Values = FV
			override type Components = FC
			override type Result = OperationSchema[A, S, FV, FC, O]

			override def apply(mapping :SchemaMapping[S, V, C, O], op :A, includes :Iterable[RefinedMapping[_, O]]) = {
				val schema = mapping.schema
				val filtered = filter(schema)
				val customized =
					if (filtered eq schema) filtered
					else new AlteredSchema(schema, filtered, op, includes)
				new AlteredSchemaMapping[A, S, FV, FC, O](mapping, customized)
			}
		}






	private[schema] class MappedSchema[S, V <: Chain, C <: Chain, O, F]
	                                  (protected override val backer :MappingSchema[S, V, C, O],
	                                   constructor :F, override val buffs :Seq[Buff[S]] = Nil)
	                                  (implicit conversion :SubjectConstructor[S, V, C, O, F])
		extends MappingSchemaDelegate[MappingSchema[S, V, C, O], S, V, C, O] with StableMapping
	{
		private[this] val cons = conversion(schema, constructor)

		override def assemble(pieces :Pieces) :Option[S] = cons(pieces)
	}



	private[schema] class MappedFlatSchema[S, V <: Chain, C <: Chain, O, F]
	                      (protected override val backer :FlatMappingSchema[S, V, C, O],
	                       constructor :F, buffs :Seq[Buff[S]] = Nil)
	                      (implicit conversion :SubjectConstructor[S, V, C, O, F])
		extends MappedSchema(backer, constructor, buffs) with FlatSchemaMapping[S, V, C, O]
		   with MappingSchemaDelegate[FlatMappingSchema[S, V, C, O], S, V, C, O]






	private[schema] class AlteredSchemaMapping[A <: OperationType, S, V <: Chain, C <: Chain, O]
	                                          (original :SchemaMapping[S, _ <: Chain, _ <: Chain, O],
	                                           protected override val backer :MappingSchema[S, V, C, O])
		extends MappingSchemaDelegate[MappingSchema[S, V, C, O], S, V, C, O]
		   with OperationSchema[A, S, V, C, O] with StableMapping
	{
		override def assemble(pieces :Pieces) = original.assemble(pieces)
	}


	private[schema] class AlteredFlatSchemaMapping[A <: OperationType, S, V <: Chain, C <: Chain, O]
	                                              (original :FlatSchemaMapping[S, _ <: Chain, _ <: Chain, O],
	                                               protected override val backer :FlatMappingSchema[S, V, C, O])
		extends AlteredSchemaMapping(original, backer) with FlatSchemaMapping[S, V, C, O]
		   with MappingSchemaDelegate[FlatMappingSchema[S, V, C, O], S, V, C, O] with FlatOperationSchema[A, S, V, C, O]






	//these are very close to MappedSchema, OptMappedSchema, MappedFlatSchema, OptMappedFlatSchema,
	//but use T=>S instead of V=>S
	private[schema] class MappedSchemaMapping[+M <: SchemaMapping[T, V, C, O], T, S, V <: Chain, C <: Chain, O]
	                      (protected override val backer :M,
	                       protected override val map :T =?> S, protected override val unmap :S =?> T)
	                      (implicit protected override val nulls :NullValue[S])
		extends MappedMapping[T, S, O] with SchemaMapping[S, V, C, O]
	{
		override val schema :MappingSchema[S, V, C, O] = backer.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:SchemaMapping[X, V, C, O] =
			new MappedSchemaMapping[M, T, X, V, C, O](backer, map andThen there, back andThen unmap)(
			                                          nullValue extract there)
	}



	private[schema] class MappedFlatSchemaMapping[+M <: FlatSchemaMapping[T, V, C, O], T, S, V <: Chain, C <: Chain, O]
	                      (protected override val backer :M,
	                       protected override val map :T =?> S, protected override val unmap :S =?> T)
	                      (implicit protected override val nulls :NullValue[S])
		extends MappedMapping[T, S, O] with FlatSchemaMapping[S, V, C, O]
	{
		override val schema :FlatMappingSchema[S, V, C, O] = backer.schema compose unmap

		override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
				:FlatSchemaMapping[X, V, C, O] =
			new MappedFlatSchemaMapping[M, T, X, V, C, O](backer, map andThen there, back andThen unmap)(
			                                              nullValue extract there)
	}



}
