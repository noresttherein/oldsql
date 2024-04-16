package net.noresttherein.oldsql.schema.bases

import net.noresttherein.oldsql.OperationView
import net.noresttherein.oldsql.collection.{Chain, Listing}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.TypedMapping
import net.noresttherein.oldsql.schema.SQLForm.NullValue
import net.noresttherein.oldsql.schema.support.{PatchedMapping, PrefixedMapping, RenamedMapping}
import net.noresttherein.oldsql.schema.support.MappingAdapter.{AbstractDelegateAdapter, DelegateAdapter}
import net.noresttherein.oldsql.schema.bits.{IndexedMappingSchema, IndexedSchemaMapping, MappingSchema}
import net.noresttherein.oldsql.schema.bits.IndexedMappingSchema.FlatIndexedMappingSchema
import net.noresttherein.oldsql.schema.bits.IndexedSchemaMapping.{FlatIndexedSchemaMapping, FlatIndexedSchemaMappingAdapter, IndexedSchemaMappingAdapter, MappedFlatIndexedSchemaMapping, MappedIndexedSchemaMapping}
import net.noresttherein.oldsql.schema.bits.MappingSchema.FlatMappingSchema
import net.noresttherein.oldsql.schema.bits.SchemaMapping.{FlatSchemaMapping, FlatSchemaMappingAdapter, MappedFlatSchemaMapping, MappedSchemaMapping, MappingSchemaDelegate, SchemaMappingAdapter, StaticSchemaMapping}
import net.noresttherein.oldsql.schema.support.ReorderedMapping.ReorderedMappingAdapter






/** Base trait for [[net.noresttherein.oldsql.schema.bits.SchemaMapping SchemaMapping]] implementations
  * which need individual access to their components during the construction process. This class extends `StaticMapping`,
  * meaning that within its [[net.noresttherein.oldsql.schema.bases.StaticMapping.construct construct(Pieces)]] method
  * components are implicitly converted into their values, allowing their direct use as arguments
  * for the subject's constructor.
  *
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
  *   - `~"favoritePizza"` or `"favoritePizza"()` for the value of the component labeled `"favoritePizza"` within the `construct` method,
  *   - `"favoritePizza".?` for the value of such a labeled component in an `Opt` when within the `construct` method.
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
	   with StaticSchemaMapping[({ type A[M <: TypedMapping[S, O], s] = SchemaMappingAdapter[M, S, s, V, C, O] })#A,
		                        MappingSchema[S, V, C, O], S, V, C, O]
{
	//consider: these adapter implementations are *exactly* the same as in SchemaMapping,
	// only in SchemaMapping their return type is not an adapter. The problem with extracting it lies in the fact
	// that SchemaMapping must have their concrete implementations (as inherited from Mapping),
	// and we need to narrow the return type to SchemaMapping, but not SchemaMappingAdapter.
	// We can create non anonymous classes for these adapters and use them in both places, but it would actually
	// increase the amount of code.
	private trait Proxy
		extends SchemaMappingAdapter[this.type, S, S, V, C, O] with AbstractDelegateAdapter[this.type, S, O]
	{
		override val body   = AbstractSchemaMapping.this
		override val schema = AbstractSchemaMapping.this.schema
	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:SchemaMappingAdapter[this.type, S, S, V, C, O] =
		alter(include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PatchedMapping[S, O](this, op, include, exclude) with Proxy

	override def prefixed(prefix :String) :SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[S, O](prefix, this) with Proxy

	override def renamed(naming :String => String) :SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[S, O](this, naming) with Proxy

	override def reorder(permutation :IndexedSeq[Int]) :SchemaMappingAdapter[this.type, S, S, V, C, O] =
		new ReorderedMappingAdapter[this.type, S, O](this, permutation) with Proxy

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
			({ type A[M <: TypedMapping[S, O], X] = FlatSchemaMappingAdapter[M, S, X, V, C, O] })#A,
			FlatMappingSchema[S, V, C, O], S, V, C, O]
{
	private trait Proxy
		extends FlatSchemaMappingAdapter[this.type, S, S, V, C, O] with AbstractDelegateAdapter[this.type, S, O]
	{
		override val body   = AbstractFlatSchemaMapping.this
		override val schema = AbstractFlatSchemaMapping.this.schema
	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		alter(include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PatchedMapping[S, O](this, op, include, exclude) with Proxy

	override def prefixed(prefix :String) :FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[S, O](prefix, this) with Proxy

	override def renamed(naming :String => String) :FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[S, O](this, naming) with Proxy

	override def reorder(permutation :IndexedSeq[Int]) :FlatSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new ReorderedMappingAdapter[this.type, S, O](this, permutation) with Proxy

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:FlatSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedFlatSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with DelegateAdapter[this.type, X, O] with FlatSchemaMappingAdapter[this.type, S, X, V, C, O]
}






abstract class AbstractIndexedSchemaMapping[S, V <: Listing, C <: Chain, O]
                                           (protected override val backer :IndexedMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[IndexedMappingSchema[S, V, C, O], S, V, C, O] with IndexedSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
				({ type A[M <: TypedMapping[S, O], s] = IndexedSchemaMappingAdapter[M, S, s, V, C, O] })#A,
				IndexedMappingSchema[S, V, C, O], S, V, C, O]
{
	private trait Proxy
		extends IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] with AbstractDelegateAdapter[this.type, S, O]
	{
		override val body   = AbstractIndexedSchemaMapping.this
		override val schema = AbstractIndexedSchemaMapping.this.schema
	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		alter(include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PatchedMapping[S, O](this, op, include, exclude) with Proxy

	override def prefixed(prefix :String) :IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[S, O](prefix, this) with Proxy

	override def renamed(naming :String => String) :IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[S, O](this, naming) with Proxy

	override def reorder(permutation :IndexedSeq[Int]) :IndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new ReorderedMappingAdapter[this.type, S, O](this, permutation) with Proxy

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:IndexedSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with AbstractDelegateAdapter[this.type, X, O] with IndexedSchemaMappingAdapter[this.type, S, X, V, C, O]
		{
			override val body = AbstractIndexedSchemaMapping.this
		}
}




abstract class AbstractFlatIndexedSchemaMapping[S, V <: Listing, C <: Chain, O]
                                               (protected override val backer :FlatIndexedMappingSchema[S, V, C, O])
	extends MappingSchemaDelegate[FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
	   with FlatIndexedSchemaMapping[S, V, C, O]
	   with StaticSchemaMapping[
				({ type A[M <: TypedMapping[S, O], X] = FlatIndexedSchemaMappingAdapter[M, S, X, V, C, O] })#A,
				FlatIndexedMappingSchema[S, V, C, O], S, V, C, O]
{
	private trait Proxy
		extends FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] with AbstractDelegateAdapter[this.type, S, O]
	{
		override val body   = AbstractFlatIndexedSchemaMapping.this
		override val schema = AbstractFlatIndexedSchemaMapping.this.schema
	}

	override def apply(include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		alter(include, exclude)

	protected override def apply(op :OperationView, include :Iterable[Component[_]], exclude :Iterable[Component[_]])
			:FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PatchedMapping[S, O](this, op, include, exclude) with Proxy

	override def prefixed(prefix :String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new PrefixedMapping[S, O](prefix, this) with Proxy

	override def renamed(naming :String => String) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new RenamedMapping[S, O](this, naming) with Proxy

	override def reorder(permutation :IndexedSeq[Int]) :FlatIndexedSchemaMappingAdapter[this.type, S, S, V, C, O] =
		new ReorderedMappingAdapter[this.type, S, O](this, permutation) with Proxy

	override def as[X](there :S =?> X, back :X =?> S)(implicit nulls :NullValue[X])
			:FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O] =
		new MappedFlatIndexedSchemaMapping[this.type, S, X, V, C, O](this, there, back)
			with AbstractDelegateAdapter[this.type, X, O] with FlatIndexedSchemaMappingAdapter[this.type, S, X, V, C, O]
		{
			override val body = AbstractFlatIndexedSchemaMapping.this
		}
}
