package net.noresttherein.oldsql.schema.bits

import java.sql.{JDBCType, PreparedStatement}

import net.noresttherein.oldsql.collection.{NaturalMap, Unique}
import net.noresttherein.oldsql.exceptions.MissingKeyException
import net.noresttherein.oldsql.model.RelatedEntityFactory
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Optional}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping, OptimizedMappingAssembly}
import net.noresttherein.oldsql.schema.{cascadeBuffs, composeColumnExtracts, composeExtracts, Buff, ColumnForm, ColumnMapping, MappingExtract}
import net.noresttherein.oldsql.schema.ColumnMapping.{OptimizedColumn, SimpleColumn, StableColumn}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, RefinedMapping}
import net.noresttherein.oldsql.schema.Relation.RelVar
import net.noresttherein.oldsql.schema.SQLForm.{AbstractMappedForm, NullValue}
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.CounterpartComponent
import net.noresttherein.oldsql.schema.support.{CoveredMapping, MappedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueColumnProxy






/** A mapping of some reference-like type `S` which may contain values from another table. It is used to implement
  * foreign keys: it contains as its only component [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]],
  * which correspond to some other mapping [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]]
  * in [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]]. These two components are homomorphic,
  * with `key` containing a proxy component for every subcomponent (and column) of `target`. The foreign key `target`
  * however is ''not'' a component of this mapping. It should never be projected to
  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this instance and passed to
  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] used in the assembly of this mapping.
  * While the name mentions a foreign key, it can be used to reflect any kind of reference where value(s)
  * of some column or columns in one table should match values of mirror columns in another. In particular, this
  * includes the reverse foreign key mapping, where the `key` component is the primary key of the containing table,
  * while `target` the foreign key in a dependent table. Subject type `S` of this trait is fully generic, allowing
  * any kind of values which can be constructed from the subjects of the referenced table, from `table.Subject`,
  * through `Option[table.Subject]` to any collection type.
  * @author Marcin Mościcki
  */
trait ForeignKeyMapping[+C[X] <: MappingAt[X], K, S, O] extends BaseMapping[S, O] {
	/** [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the referenced component, differing
	  * from this instance's `Origin`.
	  */
	type Target

	/** Referenced table. */
	def table :RelVar[MappingAt]

	/** Referenced component (typically the primary key for a foreign key mapping) in
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]].
	  * It is ''not'' a component of this mapping, or any enclosing mapping, and should not be adapted
	  * to its `Origin` type.
	  */
	def target :RefinedMapping[K, Target] //:C[Target]

	/** The component for the local 'foreign' key itself, mirroring the referenced key
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]].
	  * It contains a column for every column and a similarly mirrored component for every component in `target`.
	  * The names of the columns may (and likely will) differ and depend on the naming scheme of this instance.
	  */
	def key :Component[K]

	/** Return the component of the local [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] mirroring
	  * the component of [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] returned by the
	  * given function. Returned component will be a proxy of said foreign component, relying
	  * on its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method, but having an independent
	  * buff list (inherited from this instance) and possibly different column names. If the mapping returned
	  * by the function is a [[net.noresttherein.oldsql.schema.ColumnMapping column]], returned mapping will also
	  * be a column.
	  * @param component a function returning some subcomponent of its argument which will be applied
	  *                  to the referenced component `target` in `table`.
	  * @param local an implicit evidence determining the returned type of this method, which will be either
	  *              a [[net.noresttherein.oldsql.schema.Mapping.Component component]] or a
	  *              [[net.noresttherein.oldsql.schema.Mapping.Column column]] with the same subject type as
	  *              mapping `M` returned by the `component` function and the same origin type as this mapping.
	  */
	def apply[M <: MappingAt[Target]]
	         (component :C[Target] => M)(implicit local :CounterpartComponent[M, Target]) :local.Component[O] //=
//		local(this, component(target))

	/** Returns the alias of the given (sub)component of the referenced key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in the local
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component.
	  * Returned component can be used in SQL expressions, in particular compared with the argument component.
	  */
	def local[T](remote :RefinedMapping[T, Target]) :Component[T]

	/** Returns the alias of the given column of the referenced key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in the local
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component.
	  * Returned column can be used in SQL expressions, in particular compared with the argument column.
	  */
	def local[T](remote :ColumnMapping[T, Target]) :Column[T]


	/** Create a mapping for a reference `R` to the table containing this component, which will represent the other
	  * side of the relationship. It represents following the 'primary key' `this.table / this.target` to all rows
	  * containing a matching foreign key (`this.key`). Returned mapping can only be used in the table
	  * referenced by this mapping - `this.table`.
	  * @tparam M the mapping type for rows of the table containing this component.
	  * @tparam E the entity type containing this mapping's reference subject `S` as a (possibly composite) property.
	  * @tparam T the value type referenced by the created mapping, which is some collection of `E` or `E` itself.
	  * @tparam R the reference type created by the passed factory and the subject type of the created mapping.
	  * @param table the table owning this component. It doesn't suffice that mapping `M` contains this mapping (type)
	  *              as its component, it must be the actual `RelVar` instance which produced
	  *              the row mapping instance `M` having this instance as its member (sub)component.
	  * @param factory a factory for references to collections of the entities mapped to this component's table.
	  * @param buffs the buffs for the created mapping. As the mapping will have the component `this.target`
	  *               as its component, most likely duplicating its occurrence in `this.table`, it almost certainly
	  *               should contain [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]].
	  * @throws UnsupportedOperationException if this instance represents the inverted side of the relationship,
	  *                                       that is was created through `target.inverse` (or one of the similar
	  *                                       factory methods in the companion object).
	  */
	def inverse[M[A] <: RefinedMapping[E, A], E, T, R]
	           (table :RelVar[M], factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
			:ForeignKeyMapping[C, K, R, Target]
}




trait ForeignKeyColumnMapping[K, S, O]
	extends ForeignKeyMapping[MappingOf[K]#ColumnProjection, K, S, O] with ColumnMapping[S, O]
{
	override def target :ColumnMapping[K, Target]
	override def key :Column[K]

	override def inverse[M[A] <: RefinedMapping[E, A], E, T, R]
	                    (table :RelVar[M], factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
			:ForeignKeyColumnMapping[K, R, Target]
}






object ForeignKeyMapping {
	private type KeyColumn[K] = { type M[X] = ColumnMapping[K, X] }

	/** Create a mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the subjects `K` of the component
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single direct component
	  * of the returned mapping. The key type can be any mapping, comprised of more than one column.
	  * Construction of a mapping for a true foreign key from table `Hobbits` to table `HobbitHoles` based
	  * on their addresses could look like
	  * {{{
	  *     val factory = Kin(Equal((_:HobbitHole).address)
	  *     ForeignKeyMapping[HobbitHoles, AddressMapping, Address, HobbitHole, Kin[HobbitHole], X, O](
	  *         "home_", factory)(HobbitHoles, _.address
	  *     )
	  * }}}
	  * Assuming that the extractor from the table `Hobbits` for this component is `(_:Hobbit).address`,
	  * then a query following this key will compare the columns of `key` and `target`, corresponding to properties
	  * `Hobbit.address.key` and `HobbitHole.address`.
	  * On the other hand, the reverse mapping from `HobbitHoles` to `Hobbits`, returning all hobbits living in
	  * a given hobbit hole, based the same foreign key, could look like:
	  * {{{
	  *     ForeignKeyMapping[Hobbits, AddressMapping, Address, Hobbit, Set[Hobbit], Kin[Set[Hobbit]], X, O](
	  *         _.substring(5), Kin(Equal{ hobbit :Hobbit => hobbit.address }.in[Set])(Hobbits, _.address
	  *     )
	  * }}}
	  *
	  * Under normal circumstances (a single query, not joined with the referenced table) the references will
	  * be equivalent to those created with the [[net.noresttherein.oldsql.model.RelatedEntityFactory.absent absent]]
	  * method of the factory (or `nonexistent`, if this component is nullable). The implementation however
	  * relies on caches of entities of tables included in the query, which can additionally span several
	  * executed ''selects'' within the same transaction. If, at any point when executing a query qroup,
	  * values of the referenced table with matching keys are found, they will be lazily resolved using
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.delayed delayed]] method of the factory.
	  * This delayed loading doesn't require an open transaction when the values are accessed.
	  *
	  * This factory method can be adapted to work with optional keys on both sides, in any combination.
	  *   1. If both the foreign key and referenced key are not null, then the factory argument can
	  *      be converted with method [[net.noresttherein.oldsql.model.RelatedEntityFactory.required required]]
	  *      to enforce that there are no [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]]
	  *      references - that all instances of `R` carry either the value `T`, or the key `K`, or both.
	  *      This is optional, as - assuming the columns of the created mapping are indeed not null -
	  *      method `nonexistent` will not be called when assembling values from a `ResultSet`, as a key will always
	  *      be present,
	  *   1. The case where the foreign key is optional (the columns of this component are nullable),
	  *      but the referenced key type `K` is not an `Option[_]`, then everything proceeds as before,
	  *      except the factory should return a valid value from its `nonexistent` method. This can always be done
	  *      by returning `None` and having an `Option` as the reference type: `R <: Option[_]`. Additionally,
	  *      a [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]]
	  *   1. If the referenced component is nullable and maps to a `K=:=Option[I]` then the key type used by the factory
	  *      must also be an `Option`; a factory of type `RelatedEntityFactory[K, E, T, R]` can always be adapted
	  *      to type `RelatedEntityFactory[Option[K], E, T, R]` using one of its
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optionalKeys optionalKeys]] method.
	  *      This formally requires the factory to have a valid `nonexistent` value, but the method will never
	  *      be called if this component will always return `Some`. Additionally, internal index on the referenced
	  *      component will be an optional one, ignoring `null` and `None` values when caching the results.
	  *   1. If both the foreign key and the referenced key are nullable and map to `Option[K]` (or a similar type),
	  *      then the situation is analogous to point 1), except that now the key is an `Option[K]`.
	  *      `RelatedEntityFactory[K, E, T, R]` methods
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optional optional]] and
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optionalKeys optionalKeys]] may be useful here
	  *      to lift the key type to `Option[K]`. Additionally, internal
	  *      caches will ignore all rows with `null`/`None` values, to avoid erroneous `null == null` matches.
	  *
	  * @tparam M the mapping for the referenced table, mapped to type `E`.
	  * @tparam C the mapping for the referenced component in `table`, mapped to type `K`.
	  * @tparam K the key type: the subject type of both the local key (subcomponent of the returned mapping)
	  *           and referenced key in `table` (returned by `pk`).
	  * @tparam E the referenced entity type; the value or values of this type will be included in the reference
	  *           values mapped by the returned mapping.
	  * @tparam T a type derived from `E`, values of which can be constructed from collections of `E`.
	  *           It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *           It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R the subject type of this mapping: a reference-like type which can hold values of `T`
	  *           and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam X the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O a mandatory, arbitrary origin type of the returned mapping.
	  * @param rename a function mapping column names from the referenced component to column names
	  *               in the created mapping.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying component to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs of the created mapping.
	  */
	def apply[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	         (rename :String => String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	         (table :RelVar[M], pk :M[X] => C[X]) :ForeignKeyMapping[C, K, R, O] =
		pk(table[X]) match {
			case col :ColumnMapping[_, _] =>
				val selector = pk.asInstanceOf[M[X] => ColumnMapping[K, X]]
				column[M, K, E, T, R, X, O](rename(col.name), factory, buffs :_*)(table, selector)
					.asInstanceOf[ForeignKeyMapping[C, K, R, O]]
			case _ =>
				new ForeignKeyRelationMapping[M, C, K, E, T, R, X, O](rename, buffs, factory)(table, pk)
		}

	/** Create a mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the subjects `K` of the component
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single direct component
	  * of the returned mapping. The key type can be any mapping, comprised of more than one column.
	  * This method delegates to the more generic, overloaded variant
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:RefinedMapping[E,A],C[A]<:RefinedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * See its documentation for additional documentation and examples.
	  * @tparam M the mapping for the referenced table, mapped to type `E`.
	  * @tparam C the mapping for the referenced component in `table`, mapped to type `K`.
	  * @tparam K the key type: the subject type of both the local key (subcomponent of the returned mapping)
	  *           and referenced key in `table` (returned by `pk`).
	  * @tparam E the referenced entity type; the value or values of this type will be included in the reference
	  *           values mapped by the returned mapping.
	  * @tparam T a type derived from `E`, values of which can be constructed from collections of `E`.
	  *           It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *           It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R the subject type of this mapping: a reference-like type which can hold values of `T`
	  *           and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam X the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O a mandatory, arbitrary origin type of the returned mapping.
	  * @param columnPrefix a `String` which will be prepended to names of all columns, being copies of columns
	  *                     of the referenced component, of the returned mapping.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying component to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs of the created mapping.
	  */
	def apply[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	         (columnPrefix :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	         (table :RelVar[M], pk :M[X] => C[X]) :ForeignKeyMapping[C, K, R, O] =
		apply[M, C, K, E, T, R, X, O](columnPrefix + _, factory, buffs :_*)(table, pk)

	/** Create a column mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the key type `K` of the column
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single 'subcolumn'
	  * of the returned column. See the documentation of
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:RefinedMapping[E,A],C[A]<:RefinedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * for additional information and examples.
	  * @tparam M the mapping for the referenced table, mapped to type `E`.
	  * @tparam K the key type: the subject type of both the local key (subcolumn of the returned column)
	  *           and referenced key in `table` (returned by `pk`).
	  * @tparam E the referenced entity type; the value or values of this type will be included in the reference
	  *           values mapped by the returned mapping.
	  * @tparam T a type derived from `E`, values of which can be constructed from collections of `E`.
	  *           It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *           It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R the subject type of this column: a reference-like type which can hold values of `T`
	  *           and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam X the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O a mandatory, arbitrary origin type of the returned mapping.
	  * @param name the name of the foreign key column.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying column to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs for the created column.
	  */
	def column[M[A] <: RefinedMapping[E, A], K, E, T, R, X, O]
	          (name :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	          (table :RelVar[M], pk :M[X] => ColumnMapping[K, X]) :ForeignKeyColumnMapping[K, R, O] =
		new ForeignKeyRelationColumnMapping[M, K, E, T, R, X, O](name, buffs, factory)(table, pk)

//	def optional[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
//	            (rename :String => String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
//	            (table :RelVar[M], pk :M[X] => C[X]) :ForeignKeyMapping[C, K, R, O] =
//		pk(table[X]) match {
//			case col :ColumnMapping[_, _] =>
//				val selector = pk.asInstanceOf[M[X] => ColumnMapping[K, X]]
//				optionalColumn[M, K, E, T, R, X, O](rename(col.name), factory, buffs :_*)(table, selector)
//					.asInstanceOf[ForeignKeyMapping[C, K, R, O]]
//			case _ =>
//				new ForeignKeyRelationMapping[M, C, K, E, T, R, X, O](rename, buffs, factory)(table, pk)
//					with OptionalForeignKeyMapping[M, C, K, E, T, R, X, O]
//		}
//
//	def optional[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
//	            (columnPrefix :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
//	            (table :RelVar[M], pk :M[X] => C[X]) :ForeignKeyMapping[C, K, R, O] =
//		optional[M, C, K, E, T, R, X, O](columnPrefix + _, factory, buffs :_*)(table, pk)
//
//	def optionalColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X, O]
//	                  (name :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
//	                  (table :RelVar[M], pk :M[X] => ColumnMapping[K, X]) :ForeignKeyColumnMapping[K, R, O] =
//		new ForeignKeyRelationColumnMapping[M, K, E, T, R, X, O](name, buffs, factory)(table, pk)
//			with OptionalForeignKeyMapping[M, KeyColumn[K]#M, K, E, T, R, X, O]

	/** Create a mapping for the other side of a relationship defined by a foreign key mapping.
	  * It will, similarly to the original foreign key mapping, use some reference type `R` to the table containing
	  * the foreign key. It represents following the referenced 'primary key' `key` to all rows
	  * containing a matching foreign key. The returned mapping can only be used in the table
	  * containing the original referenced key.
	  * @tparam M the mapping type for rows of the table containing the foreign key mapping referencing `key`.
	  * @tparam C the type of the referenced component, which is the 'primary key' side of the relationship
	  *           (not necessarily an actual primary key).
	  * @tparam K the key type: the subject of the referenced key,
	  *           the [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component of the foreign key
	  *           returned by `fk`, and the `key` component of the returned mapping.
	  * @tparam E the entity type containing both the key `K` and the collection referenced by the created mapping
	  *           as (possibly composite) properties.
	  * @tparam T the value type referenced by the created mapping, which is some collection of `E` or `E` itself.
	  * @tparam R the reference type created by the passed factory and the subject type of the created mapping.
	  * @tparam O the origin type of the created mapping and the whole table mapping containing the referenced key.
	  * @param key the original key referenced by the inverted foreign key returned by `fk`.
	  * @param factory a factory for references to collections of the entities mapped to `table`.
	  * @param buffs the buffs for the created mapping. As the mapping will have the component `key`
	  *              as its component, most likely duplicating its occurrence in the owning table, it almost certainly
	  *              should contain [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]].
	  * @param table the table owning the foreign key component.
	  * @param fk a function returning the foreign key subcomponent of the table referenced by the created mapping.
	  */
	def inverse[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	           (key :C[O], factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	           (table :RelVar[M], fk :M[X] => ForeignKeyMapping[C, K, _, X]) :ForeignKeyMapping[C, K, R, O] =
		new InverseForeignKeyMapping[M, C, K, E, T, R, X, O](buffs, key, factory)(table, fk)


	/** Create a mapping for the other side of a relationship defined by a foreign key mapping.
	  * It will, similarly to the original foreign key mapping, use some reference type `R` to the table containing
	  * the foreign key column. It represents following the referenced 'primary key' `key` to all rows
	  * containing a matching foreign key. The returned mapping can only be used in the table
	  * containing the original referenced key.
	  * @tparam M the mapping type for rows of the table containing the foreign key mapping referencing `key`.
	  * @tparam K the key type: the subject of the referenced key,
	  *           the [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component of the foreign key
	  *           returned by `fk`, and the `key` component of the returned mapping.
	  * @tparam E the entity type containing both the key `K` and the collection referenced by the created mapping
	  *           as (possibly composite) properties.
	  * @tparam T the value type referenced by the created mapping, which is some collection of `E` or `E` itself.
	  * @tparam R the reference type created by the passed factory and the subject type of the created column.
	  * @tparam O the origin type of the created mapping and the whole table mapping containing the referenced key.
	  * @param key the original key column (typically the primary key) referenced by the inverted foreign key
	  *            returned by `fk`.
	  * @param factory a factory for references to collections of the entities mapped to `table`.
	  * @param buffs the buffs for the created column. As the column will have `key` as its subcolumn, most likely
	  *              duplicating its occurrence in the owning table, it almost certainly
	  *              should contain [[net.noresttherein.oldsql.schema.Buff.Virtual Virtual]].
	  * @param table the table owning the foreign key column.
	  * @param fk a function returning the foreign key column of the table referenced by the created column.
	  */
	def inverseColumn[M[A] <: RefinedMapping[E, A], K, E, T, R, X, O]
	                 (key :ColumnMapping[K, O], factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	                 (table :RelVar[M], fk :M[X] => ForeignKeyColumnMapping[K, _, X]) :ForeignKeyColumnMapping[K, R, O] =
		new InverseForeignKeyColumnMapping[M, K, E, T, R, X, O](buffs, key, factory)(table, fk)



	private[oldsql] abstract class AbstractForeignKeyMapping
	                               [M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	                               (factory :RelatedEntityFactory[K, E, T, R])
	                               (override val table :RelVar[M], pk :M[X] => C[X])
		extends ForeignKeyMapping[C, K, R, O]
	{
		override type Target = X
		//todo: inline this laziness once we are comfortably sure of the implementation
		private[this] val lazyTarget = Lazy(pk(table[X]))
		override def target :C[X] = lazyTarget
		def references :RelatedEntityFactory[K, E, T, R] = factory

		override def apply[S <: MappingAt[X]]
		                  (component :C[X] => S)(implicit local :CounterpartComponent[S, X]) :local.Component[O] =
			local(this, component(lazyTarget))

		private[this] val composer = factory.result.composer

		override def assemble(pieces :Pieces) :Option[R] = pieces.get(key) match {
			case Some(k) => Some(factory.delayed(k, composer.attempt(pieces(table).all(lazyTarget, k))))
			case _ => None
		}


		override def inverse[N[A] <: RefinedMapping[S, A], S, Y, F]
		                    (table :RelVar[N], factory :RelatedEntityFactory[K, S, Y, F], buffs :Buff[F]*)
				:ForeignKeyMapping[C, K, F, X] =
			ForeignKeyMapping.inverse[N, C, K, S, Y, F, O, X](target, factory, buffs:_*)(table, _ => this)

		override def mappingName :String = "FK(" + target.mappingName + ")"
		override def toString :String = s"FK($table.$target):$factory"
	}



//	private[oldsql] trait OptionalForeignKeyMapping
//	                      [M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
//		extends AbstractForeignKeyMapping[M, C, K, E, T, R, X, O]
//	{
//		private val default = Some(references.nonexistent)
//
//		override def assemble(pieces :Pieces) :Option[R] = {
//			val res = super.assemble(pieces)
//			if (res.isDefined) res else default
//		}
//	}



	private[oldsql] class ForeignKeyRelationMapping
	                      [M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	                      (rename :String => String, override val buffs :Seq[Buff[R]],
	                       factory :RelatedEntityFactory[K, E, T, R])
	                      (table :RelVar[M], pk :M[X] => C[X])
		extends AbstractForeignKeyMapping[M, C, K, E, T, R, X, O](factory)(table, pk) with LazyMapping[R, O]
	{
		private[this] val lazyKey = Lazy {
			val keyBuffs = cascadeBuffs(this)(factory.keyOf _)
			new CoveredMapping[C[X], K, X, O](target, rename, keyBuffs)
		}
		override def key :Component[K] = lazyKey

		override def assemble(pieces :Pieces) = super[AbstractForeignKeyMapping].assemble(pieces)

		override def local[S](remote :RefinedMapping[S, Target]) :Component[S] = lazyKey.cover(remote)
		override def local[S](remote :ColumnMapping[S, Target]) :Column[S] = lazyKey.cover(remote)

		override lazy val (extracts, columnExtracts) = {
			val keyExtract = MappingExtract.opt(key)(factory.keyOf)
			(composeExtracts(keyExtract).updated[Extract, K](key, keyExtract) :ExtractMap) ->
				(composeColumnExtracts(keyExtract) :ColumnExtractMap)
		}

		override lazy val components :Unique[Component[_]] = Unique.single(key)
		override lazy val subcomponents :Unique[Component[_]] = key +: key.subcomponents
		override lazy val columns :Unique[Column[_]] = key.columns
	}



	private[oldsql] class ForeignKeyRelationColumnMapping[M[A] <: RefinedMapping[E, A], K, E, T, R, X, O]
	                      (override val name :String, override val buffs :Seq[Buff[R]],
	                       factory :RelatedEntityFactory[K, E, T, R])
	                      (override val table :RelVar[M], pk :M[X] => ColumnMapping[K, X])
		extends AbstractForeignKeyMapping[M, KeyColumn[K]#M, K, E, T, R, X, O](factory)(table, pk)
		   with ForeignKeyColumnMapping[K, R, O] with OptimizedColumn[R, O]
	{
		private[this] val lazyForm = Lazy(new ForeignKeyColumnForm[K, E, T, R](pk(table[X]).form, factory))
		override def form = lazyForm

		private[this] val lazyKey = Lazy {
			val keyBuffs = cascadeBuffs(this)(Optional(factory.keyOf _))
			if (target.isInstanceOf[SimpleColumn[_, _]])
				ColumnMapping[K, O](name, keyBuffs :_*)(target.form)
			else
				new OpaqueColumnProxy[K, O](target, name, keyBuffs)
		}
		override def key :Column[K] = lazyKey


		override def local[S](remote :RefinedMapping[S, X]) :Component[S] =
			if (remote eq target) key.asInstanceOf[Component[S]]
			else throw new IllegalArgumentException(s"$remote is not the referenced key column $target.")

		override def local[S](remote :ColumnMapping[S, X]) :Column[S] =
			if (remote eq target) key.asInstanceOf[Column[S]]
			else throw new IllegalArgumentException(s"$remote is not the referenced key column $target.")


		override def apply[S](component :Component[S]) :Extract[S] = extracts(component)
		override def apply[S](column :Column[S]) :ColumnExtract[S] = columnExtracts(column)

		override lazy val columnExtracts :NaturalMap[Column, ColumnExtract] = {
			composeColumnExtracts(key, Extractor.opt(factory.keyOf))
				.updated[ColumnExtract, R](this, super.apply(this))
		}

		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def inverse[N[A] <: RefinedMapping[S, A], S, Y, F]
		                    (table :RelVar[N], factory :RelatedEntityFactory[K, S, Y, F], buffs :Buff[F]*)
				:ForeignKeyColumnMapping[K, F, X] =
			ForeignKeyMapping.inverseColumn[N, K, S, Y, F, O, X](target, factory, buffs:_*)(table, _ => this)

		override def toString :String = super[AbstractForeignKeyMapping].toString
	}






	private abstract class AbstractInverseForeignKeyMapping
	                       [M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	                       (localKey :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                       (override val table :RelVar[M], foreignKey :M[X] => ForeignKeyMapping[C, K, _, X])
		extends ForeignKeyMapping[C, K, R, O]
	{
		override type Target = X
		private[this] val fk = Lazy { foreignKey(table[X]).key } //withOrigin doesn't work because C[A] is not BaseMapping
		//todo: what about buffs key has if it's virtual here?
		//todo: key will have two conflicting extracts in the outer mapping.
		override def target :RefinedMapping[K, X] = fk //localKey.asInstanceOf[C[X]] //withOrigin[X]
		override def key :C[O] = localKey

		override def apply[S <: MappingAt[X]]
		                  (component :C[X] => S)(implicit local :CounterpartComponent[S, X]) :local.Component[O] =
			local(this, component(key.asInstanceOf[C[X]]))

		override def local[S](remote :RefinedMapping[S, X]) :Component[S] = remote.withOrigin[O]
		override def local[S](remote :ColumnMapping[S, X]) :Column[S] = remote.withOrigin[O]

		private[this] val composer :ComposableFrom[T, E] = factory.result.composer

		override def assemble(pieces :Pieces) :Option[R] = pieces.get(localKey) match {
			case Some(k) => Some(factory.delayed(k, composer.attempt(pieces(table).all(fk, k))))
			case _ => Some(factory.nonexistent)
		}


		override def inverse[N[A] <: RefinedMapping[S, A], S, Y, F]
		                    (table :RelVar[N], factory :RelatedEntityFactory[K, S, Y, F], buffs :Buff[F]*) :Nothing =
			throw new UnsupportedOperationException("Cannot inverse inversed foreign key mapping " + this + ".")

		override def mappingName :String = "FK(" + key.mappingName + ").inverse"

		override def toString :String = s"InverseFK($key <- $table.${fk.get}):$factory"
	}



	private class InverseForeignKeyMapping[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O]
	                                      (override val buffs :Seq[Buff[R]],
	                                       protected override val backer :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                                      (table :RelVar[M], foreignKey :M[X] => ForeignKeyMapping[C, K, _, X])
		extends AbstractInverseForeignKeyMapping[M, C, K, E, T, R, X, O](backer, factory)(table, foreignKey)
		   with MappedMapping[K, R, O] with OptimizedMappingAssembly
	{
		override protected def map :K =?> R = factory.absent _
		override protected def unmap :R =?> K = factory.keyOf _

		protected override def nulls :NullValue[R] =
			if (factory.isRequired) NullValue.eval(factory.nonexistent)
			else NullValue(factory.nonexistent)
	}



	private class InverseForeignKeyColumnMapping[M[A] <: RefinedMapping[E, A], K, E, T, R, X, O]
	              (override val buffs :Seq[Buff[R]], key :ColumnMapping[K, O], factory :RelatedEntityFactory[K, E, T, R])
	              (table :RelVar[M], foreignKey :M[X] => ForeignKeyColumnMapping[K, _, X])
		extends AbstractInverseForeignKeyMapping[M, KeyColumn[K]#M, K, E, T, R, X, O](key, factory)(table, foreignKey)
		   with ForeignKeyColumnMapping[K, R, O] with StableColumn[R, O]
	{
		private[this] val lazyForm = Lazy(new ForeignKeyColumnForm[K, E, T, R](key.form, factory))
		override def form = lazyForm

		override def name :String = key.name
		override def target :ColumnMapping[K, X] = super.target.asInstanceOf[ColumnMapping[K, X]]

		override def apply[S](component :Component[S]) :Extract[S] = extracts(component)
		override def apply[S](column :Column[S]) :ColumnExtract[S] = columnExtracts(column)

		override lazy val columnExtracts = {
			composeColumnExtracts(key, Extractor.opt(factory.keyOf))
				.updated[ColumnExtract, R](this, super.apply(this))
		}

		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def toString :String = super[AbstractInverseForeignKeyMapping].toString
	}






	class ForeignKeyColumnForm[K, E, Y, R](referenced :ColumnForm[K], factory :RelatedEntityFactory[K, E, Y, R])
		extends AbstractMappedForm[K, R]()(referenced,
		                                   if (factory.isRequired) NullValue.eval(factory.nonexistent)
		                                   else NullValue(factory.nonexistent))
		   with ColumnForm[R]
	{
		private def form = referenced
		private def entities = factory
		private[this] val nullable = !factory.isRequired
		override protected def map(s :K) :R = factory.missing(s)
		override protected def unmap(t :R) :K = factory.forceKeyOutOf(t)

		override def set(statement :PreparedStatement, position :Int, value :R) :Unit =
			factory.keyOf(value) match {
				case Some(key) => referenced.set(statement, position, key)
				case _ if nullable => referenced.setNull(statement, position)
				case _ =>
					throw new MissingKeyException(toString + "no key in " + value + ".")
			}


		override def literal(value :R) :String =
			if (value == null)
				referenced.nullLiteral
			else
				factory.keyOf(value) match {
					case Some(key) => referenced.literal(key)
					case _ => referenced.nullLiteral
				}

		override def inlineLiteral(value :R) :String =
			if (value == null)
				referenced.inlineNullLiteral
			else
				factory.keyOf(value) match {
					case Some(key) => referenced.inlineLiteral(key)
					case _ => referenced.inlineNullLiteral
				}

		override def sqlType :JDBCType = referenced.sqlType

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case key :ForeignKeyColumnForm[_, _, _, _] if key canEqual this =>
				form == key.form && entities == entities
			case _ => false
		}

		override def hashCode :Int = referenced.hashCode * 31 + factory.hashCode

		override val toString = s"<FK[$factory]:$referenced>"
	}





	trait CounterpartComponent[-M <: MappingAt[X], X] {
		type Component[O] <: MappingAt[O]
		def apply[C[A] <: MappingAt[A], K, S, O]
		         (local :ForeignKeyMapping[C, K, S, O] { type Target = X }, remote :M) :Component[O]
	}

	implicit def counterpartComponent[T, X]
			:CounterpartComponent[RefinedMapping[T, X], X] { type Component[O] = RefinedMapping[T, O] } =
		new CounterpartComponent[RefinedMapping[T, X], X] {
			override type Component[O] = RefinedMapping[T, O]

			override def apply[C[A] <: MappingAt[A], K, S, O]
			                  (local :ForeignKeyMapping[C, K, S, O] { type Target = X },
			                   remote :RefinedMapping[T, X]) =
				local.local(remote)
		}


	implicit def counterpartColumn[T, X]
			:CounterpartComponent[ColumnMapping[T, X], X] { type Component[O] = ColumnMapping[T, O] } =
		new CounterpartComponent[ColumnMapping[T, X], X] {
			override type Component[O] = ColumnMapping[T, O]

			override def apply[C[A] <: MappingAt[A], K, S, O]
			                  (local :ForeignKeyMapping[C, K, S, O] { type Target = X },
			                   remote :ColumnMapping[T, X]) =
				local.local(remote)
		}


//	trait ForeignKeyMappingFactory[M[A] <: RefinedMapping[E, A], C[A] <: RefinedMapping[K, A], K, E, T, R, X, O] {
//		type ForeignKey <: ForeignKeyMapping[C, K, R, O]
//		def apply()
//	}
}