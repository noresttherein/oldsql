package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationType
import net.noresttherein.oldsql.collection.{Chain, Listing}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{ComponentSelection, RefinedMapping}
import net.noresttherein.oldsql.schema.support.{AdjustedMapping, AlteredMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.support.AdjustedMapping.SpecificAdjustedMapping
import net.noresttherein.oldsql.schema.support.MappingAdapter.DelegateAdapter
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.bits.MappingSchema
import net.noresttherein.oldsql.schema.bits.IndexedMappingSchema.FlatIndexedMappingSchema
import net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping.{FlatIndexedSchemaMapping, FlatIndexedSchemaMappingAdapter, FlatIndexedSchemaMappingProxy, MappedFlatIndexedSchemaMapping}
import net.noresttherein.oldsql.schema.bits.MappingSchema.FlatMappingSchema
import net.noresttherein.oldsql.schema.bits.SchemaMapping.{FlatSchemaMapping, FlatSchemaMappingAdapter, FlatSchemaMappingProxy, MappedFlatSchemaMapping, MappedSchemaMapping, MappingSchemaDelegate, SchemaMappingAdapter, SchemaMappingProxy, StaticSchemaMapping}






/** Base trait for `SchemaMapping` implementations which need individual access to their components during
  * the construction process. This class extends `StaticMapping`, meaning that within its
  * [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct(Pieces)]] method components are implicitly
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
  * @param backer the schema listing all components of this mapping.
  * @tparam S the subject type of this mapping.
  * @tparam V a `Chain` containing the types of all components in `C` in their exact order, forming a 'row schema'.
  * @tparam C a `Chain` containing the types of all components of this mapping in their exact order.
  *           different fragments of a `ResultSet`, when more than one copy is present.
  * @tparam O A marker 'Origin' type, used to distinguish between several instances of the same mapping class,
  *           but coming from different sources (especially different aliases for a table occurring more then once
  *           in a join). At the same time, it adds additional type safety by ensuring that only components of mappings
  *           included in a query can be used in the creation of SQL expressions used by that query.
  *           Consult [[net.noresttherein.oldsql.schema.Mapping.Origin Mapping.Origin]]
  * @see [[net.noresttherein.oldsql.schema.bits.SchemaMapping.SchemaComponentLabel]]
  */
abstract class AbstractSchemaMapping[S, V <: Chain, C <: Chain, O]
                                    (protected override val backer :MappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[MappingSchema[S, V, C, O], S, V, C, O]
	   with StaticSchemaMapping[({ type A[M <: RefinedMapping[S, O], X] = SchemaMappingAdapter[M, S, X, V, C, O] })#A,
		                        MappingSchema[S, V, C, O], S, V, C, O]
{
	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:SchemaMappingAdapter[this.type, S, S, V, C, O] =
		AdjustedMapping(this, include, exclude, alter)

	protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new AlteredMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with SchemaMappingProxy[this.type, S, V, C, O]

	override def prefixed(prefix :String) :SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[this.type, S, O](prefix, this)
			with DelegateAdapter[this.type, S, O] with SchemaMappingProxy[this.type, S, V, C, O]

	override def renamed(naming :String => String) :SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[this.type, S, O](this, naming)
			with DelegateAdapter[this.type, S, O] with SchemaMappingProxy[this.type, S, V, C, O]

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:SchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with SchemaMappingAdapter[this.type, S, X, V, C, O]

}






/** A 'flat' variant of [[net.noresttherein.oldsql.schema.bases.AbstractSchemaMapping AbstractSchemaMapping]]
  * (with only columns as components).
  */
abstract class AbstractFlatSchemaMapping[S, V <: Chain, C <: Chain, O]
                                        (protected override val backer :FlatMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[FlatMappingSchema[S, V, C, O], S, V, C, O] with FlatSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
			({ type A[M <: RefinedMapping[S, O], X] = FlatSchemaMappingAdapter[M, S, X, V, C, O] })#A,
			FlatMappingSchema[S, V, C, O], S, V, C, O]
{
	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		AdjustedMapping(this, include, exclude, alter)

	protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new AlteredMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with FlatSchemaMappingProxy[this.type, S, V, C, O]


	override def prefixed(prefix :String) :FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[this.type, S, O](prefix, this)
			with DelegateAdapter[this.type, S, O] with FlatSchemaMappingProxy[this.type, S, V, C, O]

	override def renamed(naming :String => String) :FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[this.type, S, O](this, naming)
			with DelegateAdapter[this.type, S, O] with FlatSchemaMappingProxy[this.type, S, V, C, O]


	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:FlatSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedFlatSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with FlatSchemaMappingAdapter[this.type, S, X, V, C, O]

}






abstract class AbstractFlatIndexedSchemaMapping[S, V <: Listing, C <: Chain, O]
                                               (protected override val backer :FlatIndexedMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
	   with FlatIndexedSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
			({ type A[M <: RefinedMapping[S, O], X] = FlatIndexedSchemaMappingAdapter[M, S, X, V, C, O] })#A,
			FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
{
	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		AdjustedMapping(this, include, exclude, alter)

	protected override def alter(op :OperationType, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new AlteredMapping[this.type, S, O](this, op, include, exclude)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def prefixed(prefix :String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[this.type, S, O](prefix, this)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def renamed(naming :String => String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[this.type, S, O](this, naming)
			with DelegateAdapter[this.type, S, O] with FlatIndexedSchemaMappingProxy[this.type, S, V, C, O]

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedFlatIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O]
}



