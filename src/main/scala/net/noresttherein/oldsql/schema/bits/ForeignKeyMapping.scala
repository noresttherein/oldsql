package net.noresttherein.oldsql.schema.bits

import java.sql.{JDBCType, PreparedStatement}

import scala.annotation.nowarn

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.exceptions.MissingKeyException
import net.noresttherein.oldsql.model.RelatedEntityFactory
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.RelatedEntityFactory.KeyExtractor
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.{=?>, Requisite}
import net.noresttherein.oldsql.schema.{composeColumnExtracts, composeExtracts, Buff, Buffs, ColumnExtract, ColumnForm, ColumnMapping, Mapping, MappingExtract, RelVar}
import net.noresttherein.oldsql.schema.ColumnMapping.{OptimizedColumn, SimpleColumn, StableColumn, TypedColumn}
import net.noresttherein.oldsql.schema.ColumnWriteForm.SingletonColumnWriteForm
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.SQLForm.{AbstractMappedForm, NullValue}
import net.noresttherein.oldsql.schema.bases.{BaseColumn, BaseMapping, LazyMapping, OptimizedMappingAssembly}
import net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.CounterpartComponent
import net.noresttherein.oldsql.schema.support.{CoveredMapping, MappedMapping}
import net.noresttherein.oldsql.schema.support.MappingProxy.OpaqueColumnProxy





/** Common super type of components mapping a relationship between two tables into a reference
  * linking the scala objects mapped to those tables. The subject type represents a unidirectional view
  * of the relationship, starting from the table mapped by the root mapping containing this component, and linking
  * to `table` property of this object. The subject type `S` of this mapping is a 'reference type' which consists
  * of two optional parts: a ''key'', that is a value from the owning table identifying a row or rows
  * in the target table, which is (typically) included in every object created by this mapping, and a ''value'',
  * that is either the subject type of the target table, or some collection of those subjects
  * for ''to-many'' relationships.
  *
  * This trait is of little use in itself, but allows to refer to both
  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping ForeignKeyMapping]] and
  * [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping JoinTableCollectionMapping]] through
  * a single interface.
  * @tparam T the mapping type of the referenced table.
  * @tparam S the subject type of this mapping, typically some [[net.noresttherein.oldsql.model.Kin Kin]] subtype
  *           or a similar reference-like type.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping.
  * @author Marcin Mościcki
  */
trait RelationshipMapping[+T[A] <: MappingAt[A], S, O] extends BaseMapping[S, O] {
	/** The 'key' type carried by the subject of the mapping, which is a subject type of a component in the owning table
	  * referencing (or referenced by) rows
	  * in the target [[net.noresttherein.oldsql.schema.bits.RelationshipMapping.table table]], either directly
	  * (a foreign key to the primary key in the referenced table or a primary key of this table referenced by
	  * a foreign key in the target table), or indirectly through a join table.
	  */
	type Key

	/** The table with entities in a relationship with the owning table. Row value(s) `T[_]#Subject`
	  * form the subject type `S` of this mapping.
	  */
	def table :RelVar[T]

	/** A component of this mapping with a subject type representing a column or columns in the owning table which
	  * identifies a set of rows in the target [[net.noresttherein.oldsql.schema.bits.RelationshipMapping.table table]]
	  * (possibly indirectly). This might be a component for a foreign key fully managed by this instance,
	  * or a 'symlink' to the primary key in the owning table (that is, a component declared directly
	  * by the owning mapping), in which case it will likely carry
	  * an [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff
	  * or be a [[net.noresttherein.oldsql.schema.bits.SymlinkMapping SymlinkMapping]].
	  */
	def key :Component[Key]

	/** Creates a reference instance of the subject type of this mapping wrapping the given key. */
	def forKey(key :Key) :S
}






/** Base trait for relationships joining two tables by matching isomorphic components between them.
  * It is a mapping for a reference-like type `S` which has two distinct, optional parts: a ''key'', which
  * identifies a row or rows in the other table, and/or a ''value'': an object of type derived from the referenced
  * table's entity type (the subject of the table's mapping). The latter can be simply the type mapped to a single row
  * of the table, or a collection combined from multiple rows. It works as an adapter to a single 'key' component
  * in the containing table, but also grants access to the referenced table matching key component.
  * It serves as an 'outside' interface of [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping ForeignKeyMapping]],
  * providing all information required to join two tables by matching the corresponding key values,
  * while keeping the type signature uncluttered. Instances (if not necessarily implementations) belong
  * to one of two distinct groups: components for foreign keys, and mappings of 'inverse foreign keys':
  * components for keys in the owning tables referenced by foreign keys from target tables.
  * @tparam T the mapping type of the referenced table.
  * @tparam S the subject type of this mapping, typically some [[net.noresttherein.oldsql.model.Kin Kin]] subtype
  *           or a similar reference-like type.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping.
  * @see [[net.noresttherein.oldsql.model.Kin]]
  * @author Marcin Mościcki
  */
trait DirectRelationshipMapping[+T[A] <: MappingAt[A], S, O] extends RelationshipMapping[T, S, O] {

	/** The key type: the subject type of both the referenced and local key components `target`, `key`. */
	override type Key

	/** [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the referenced component, differing
	  * from this instance's `Origin`.
	  */
	type TargetOrigin //todo: rename to ReferentOrigin

	/** Referenced table. */
	override def table :RelVar[T]

	/** Referenced component (typically the primary key for a foreign key mapping) in
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]].
	  * It is ''not'' a component of this mapping, or any enclosing mapping, and should not be adapted
	  * to its `Origin` type.
	  */ //todo: rename to referent
	def target :TypedMapping[Key, TargetOrigin]

	/** The component for the local 'foreign' key itself, mirroring the referenced key
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]].
	  * It contains a column for every column and a similarly mirrored component for every component in `target`.
	  * The names of the columns may (and likely will) differ and depend on the naming scheme of this instance.
	  * For inverse foreign key mappings, if the local key is the primary key of the enclosing table,
	  * or otherwise a component occurring independently elsewhere, there is a need to prevent from duplicate
	  * entries for this component in generated SQL. This is typically achieved either by granting it
	  * an [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff, or by excluding it altogether from
	  * this mapping's component lists (for example,
	  * by extending [[net.noresttherein.oldsql.schema.bits.SymlinkMapping SymlinkMapping]]).
	  */
	override def key :Component[Key]

	/** Creates the mapped reference for the given key. */
	override def forKey(key :Key) :S

	override def identical(that :Mapping) :Boolean = that match {
		case _ if this eq that => true
		case other :DirectRelationshipMapping[MappingAt, _, _] if canEqual(other) && other.canEqual(this) =>
			(key identical other.key) && (table isomorphic other.table) && (target identical other.target)
	}
}






/** A mapping of some reference-like type `S` which may contain values from another table. It represents a relationship
  * between two tables defined as equality of of values of the local
  * [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.key key]] component on one side,
  * and [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.target target]] in the referenced
  * [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.table table]] on the other one. It expands
  * [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping DirectRelationshipMapping]] with API allowing
  * access to subcomponents of the [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.key key]] component
  * without specifying if the original comes from this table or the referenced one.
  *
  * The single component [[net.noresttherein.oldsql.schema.bits.RelationshipMapping.key key]] of this mapping
  * is homomorphic with component mapping [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.target target]]
  * in the other table, containing a proxy component for every subcomponent (and column) of `target`.
  * The foreign key `target` however is ''not'' a component of this mapping. It should never be projected to
  * [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this instance and passed to
  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]] used in the assembly of this mapping.
  * In this way, the mapping can be used to implement both
  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping foreign keys]] and their inverse, where `key` is in fact
  * the referenced key in the relation (typically the primary key of this table), and the remote `target` is the real
  * foreign key as well as any natural joins between two tables. Note that the type of the key component `C` needs not
  * have `K` as its subject type and needs not to actually be `key`, which allows mismatched subject types
  * between tables, which nevertheless are equivalent in the database (such as `Option[K]` and `K`). Additionally,
  * subject type `S` of this trait is fully generic, allowing any kind of values which can be constructed
  * from the subjects of the referenced table, from `table.Subject`, through `Option[table.Subject]`
  * to any collection type.
  * @tparam T the mapping type of the referenced table.
  * @tparam C the original key mapping type defining the structure of both the referenced component
  *           and the local `key` component. For true foreign keys, it is the type of the referenced component,
  *           but for foreign key inverses it is the key in the containing table referenced by the true foreign key
  *           in the target table. This interface doesn't require either of the local
  *           [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.key key]] component or
  *           the [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.target target]] component
  *           to be of this type, nor that its subject type be the key type `K`. This opens the way
  *           for implementations where the key types differ.
  * @tparam K the key type - the subject type of the referenced component and its local counterpart.
  * @tparam S the subject type of this mapping, referencing all entities in the referenced table whose
  *           [[net.noresttherein.oldsql.schema.bits.JoinedEntityComponent.target target]] component's value
  *           matches the value of the local component
  *           [[net.noresttherein.oldsql.schema.bits.JoinedEntityComponent.key key]].
  *           It will typically be [[net.noresttherein.oldsql.model.Kin Kin]]`[T[_]#Subject]`
  *           or a similar type.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this component.
  * @see [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping]]
  */
trait JoinedEntityComponent[+T[A] <: MappingAt[A], +C[X] <: MappingAt[X], K, S, O]
	extends DirectRelationshipMapping[T, S, O]
{
	override type Key = K

	/** [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the `key` template. */
	type KeyOrigin

	/** Return the component of the local [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]]
	  * corresponding to the subcomponent of the key template mapping returned by the given function.
	  * For foreign keys, the returned component will be a proxy of said foreign component, relying
	  * on its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method, but having an independent
	  * buff list (inherited from this instance) and possibly different column names. For foreign key inverses, it will
	  * be the subcomponent itself. If the mapping returned by the function
	  * is a [[net.noresttherein.oldsql.schema.ColumnMapping column]], returned mapping will also be a column.
	  * @param component a function returning some subcomponent of its argument which will be applied
	  *                  to the original referenced key: `target` for true foreign keys and `key` for their inverses.
	  * @param local     an implicit evidence determining the returned type of this method, which will be either
	  *                  a [[net.noresttherein.oldsql.schema.Mapping.Component component]] or a
	  *                  [[net.noresttherein.oldsql.schema.Mapping.Column column]] with the same subject type as
	  *                  mapping `M` returned by the `component` function and the same origin type as this mapping.
	  */
	def apply[M <: MappingAt[KeyOrigin]]
	         (component :C[KeyOrigin] => M)(implicit local :CounterpartComponent[M, KeyOrigin]) :local.Component[O]


	/** Return the component of the referenced key
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] corresponding to the subcomponent
	  * of the key template mapping returned by the given function. For true foreign keys, this will be the subcomponent
	  * itself. For foreign key inverses, it will be a proxy of said foreign component, relying
	  * on its [[net.noresttherein.oldsql.schema.Mapping.assemble assemble]] method, but having an independent
	  * buff list (inherited from this instance) and possibly different column names. If the mapping returned
	  * by the function is a [[net.noresttherein.oldsql.schema.ColumnMapping column]], returned mapping will also
	  * be a column.
	  * @param component a function returning some subcomponent of its argument which will be applied
	  *                  to the original referenced key: `target` for true foreign keys and `key` for their inverses.
	  * @param remote an implicit evidence determining the returned type of this method, which will be either
	  *               a [[net.noresttherein.oldsql.schema.Mapping.Component component]] or a
	  *               [[net.noresttherein.oldsql.schema.Mapping.Column column]] with the same subject type as
	  *               mapping `M` returned by the `component` function and the same origin type as this mapping.
	  * @see [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply]]
	  */
	def foreign[M <: MappingAt[KeyOrigin]]
	           (component :C[KeyOrigin] => M)(implicit remote :CounterpartComponent[M, KeyOrigin])
			:remote.Component[TargetOrigin]

	/** Returns the alias of the given (sub)component of the referenced key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in the local
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component.
	  * Returned component can be used in SQL expressions, in particular compared with the argument component.
	  */
	def local[X](subKey :TypedMapping[X, KeyOrigin]) :Component[X]

	/** Returns the alias of the given column of the referenced key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] in the local
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] component.
	  * Returned column can be used in SQL expressions, in particular compared with the argument column.
	  */
	def local[X](subKey :TypedColumn[X, KeyOrigin]) :Column[X]

	/** Returns the alias of the given (sub)component of the local 'foreign' key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] in the referenced table's
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] component.
	  * Returned component can be used in SQL expressions, in particular compared with the argument component.
	  */
	def foreign[X](subKey :TypedMapping[X, KeyOrigin]) :TypedMapping[X, TargetOrigin]

	/** Returns the alias of the given column of the local 'foreign' key component
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] in the referenced table's
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]] component.
	  * Returned column can be used in SQL expressions, in particular compared with the argument column.
	  */
	def foreign[X](subKey :TypedColumn[X, KeyOrigin]) :TypedColumn[X, TargetOrigin]

}




/** A ''composite'' column mapping - one that contains another column as its component - which adapts a
  * 'key' column, mapping its subject `K` to a 'reference' type `S`.
  * The [[net.noresttherein.oldsql.schema.bits.JoinedEntityColumn.key key]] column is of the same type as
  * some column [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.target target]] in the
  * referenced [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.table table]]. It is used
  * to implement [[net.noresttherein.oldsql.schema.bits.ForeignKeyColumnMapping foreign keys]] and other relationships
  * defined by equality of these two columns, in particular the inverse side of a foreign key `target`.
  *
  * It is different from standard column [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter adapter]]
  * (which are already special in that they contain another column) because its subject type will be assembled not only
  * from the value of the column, but also [[net.noresttherein.oldsql.haul.FutureValues FutureValues]] cache,
  * potentially containing values of the referenced entity.
  */
trait JoinedEntityColumn[+T[A] <: MappingAt[A], K, S, O]
	extends JoinedEntityComponent[T, MappingOf[K]#ColumnProjection, K, S, O] with BaseColumn[S, O]
{
	override def key :Column[K]
	override def target :TypedColumn[K, TargetOrigin]
}






/** A mapping of some reference-like type `S` which may contain values from another table. It is an adapter
  * to component [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.key key]] mapping a foreign key
  * referencing component,  [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.target target]]
  * of [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.table table]]. These two components are homomorphic,
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
  * @tparam T the mapping type of the referenced table.
  * @tparam C the original key mapping type defining the structure of both the referenced component
  *           and the local `key` component. For true foreign keys, it is the type of the referenced component,
  *           but for foreign key inverses it is the key of the containing table referenced by the true foreign key.
  * @tparam K the key type - subject type of the referenced component and its local counterpart.
  * @tparam S a type derived from the subject type of the referenced table, referencing all rows with `target`
  *           component's value matching the value of the local `key` component.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of this component.
  */
trait ForeignKeyMapping[+T[A] <: MappingAt[A], +C[A] <: TypedMapping[K, A], K, S, O]
	extends JoinedEntityComponent[T, C, K, S, O]
{
	override def target :C[TargetOrigin]

	/** Create a mapping for a reference `R` to the table containing this component, which will represent the other
	  * side of the relationship. It represents following the 'primary key' `this.table / this.target` to all rows
	  * containing a matching foreign key (`this.key`). Returned mapping can only be used in the table
	  * referenced by this mapping - `this.table`.
	  * @tparam M the mapping type for rows of the table containing this component.
	  * @tparam E the entity type containing this mapping's reference subject `S` as a (possibly composite) property.
	  * @tparam X the value type referenced by the created mapping, which is some collection of `E` or `E` itself.
	  * @tparam R the reference type created by the passed factory and the subject type of the created mapping.
	  * @param table the table owning this component. It doesn't suffice that mapping `M` contains this mapping (type)
	  *              as its component, it must be the actual `RelVar` instance which produced
	  *              the row mapping instance `M` having this instance as its member (sub)component.
	  * @param factory a factory for references to collections of the entities mapped to this component's table.
	  * @param buffs the buffs for the created mapping. As the mapping will have the component `this.target`
	  *               as its component, most likely duplicating its occurrence in `this.table`, it almost certainly
	  *               should contain [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
	  * @throws UnsupportedOperationException if this instance represents the inverted side of the relationship,
	  *                                       that is was created through `target.inverse` (or one of the similar
	  *                                       factory methods in the companion object).
	  */
	def inverse[M[A] <: TypedMapping[E, A], E, X, R]
	           (table :RelVar[M], factory :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
			:JoinedEntityComponent[M, C, K, R, TargetOrigin] =
		inverse(table, factory, Buffs(buffs :_*))

	/** Create a mapping for a reference `R` to the table containing this component, which will represent the other
	  * side of the relationship. It represents following the 'primary key' `this.table / this.target` to all rows
	  * containing a matching foreign key (`this.key`). Returned mapping can only be used in the table
	  * referenced by this mapping - `this.table`.
	  * @tparam M the mapping type for rows of the table containing this component.
	  * @tparam E the entity type containing this mapping's reference subject `S` as a (possibly composite) property.
	  * @tparam X the value type referenced by the created mapping, which is some collection of `E` or `E` itself.
	  * @tparam R the reference type created by the passed factory and the subject type of the created mapping.
	  * @param table the table owning this component. It doesn't suffice that mapping `M` contains this mapping (type)
	  *              as its component, it must be the actual `RelVar` instance which produced
	  *              the row mapping instance `M` having this instance as its member (sub)component.
	  * @param factory a factory for references to collections of the entities mapped to this component's table.
	  * @param buffs the buffs for the created mapping. As the mapping will have the component `this.target`
	  *               as its component, most likely duplicating its occurrence in `this.table`, it almost certainly
	  *               should contain [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
	  * @throws UnsupportedOperationException if this instance represents the inverted side of the relationship,
	  *                                       that is was created through `target.inverse` (or one of the similar
	  *                                       factory methods in the companion object).
	  */
	def inverse[M[A] <: TypedMapping[E, A], E, X, R]
	           (table :RelVar[M], factory :RelatedEntityFactory[K, E, X, R], buffs :Buffs[R])
			:JoinedEntityComponent[M, C, K, R, TargetOrigin]
}




/** A mapping of a single column foreign key. It adapts another column 'key' of type `K`, which is the actual column
  * type shared with the referenced column `target` to some 'reference' type `S`. It is different from standard
  * column [[net.noresttherein.oldsql.schema.support.MappingAdapter.ColumnAdapter adapter]] (which are already special
  * in that they contain another column) because its subject type will be assembled not only from the value
  * of the column, but also [[net.noresttherein.oldsql.haul.FutureValues FutureValues]] cache, potentially containing
  * values of the referenced entity.
  */
trait ForeignKeyColumnMapping[+T[A] <: MappingAt[A], K, S, O]
	extends JoinedEntityColumn[T, K, S, O] with ForeignKeyMapping[T, MappingOf[K]#ColumnProjection, K, S, O]
{
	override def inverse[M[A] <: TypedMapping[E, A], E, X, R]
	                    (table :RelVar[M], factory :RelatedEntityFactory[K, E, X, R], buffs :Buff[R]*)
			:JoinedEntityColumn[M, K, R, TargetOrigin] =
		inverse(table, factory, Buffs(buffs :_*))

	override def inverse[M[A] <: TypedMapping[E, A], E, X, R]
	                    (table :RelVar[M], factory :RelatedEntityFactory[K, E, X, R], buffs :Buffs[R])
			:JoinedEntityColumn[M, K, R, TargetOrigin]
}






object ForeignKeyMapping {
	@nowarn private type KeyColumn[K] = { type M[X] = TypedColumn[K, X] } //the compiler doesn't see it's used!

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
	  * executed ''selects'' within the same transaction. If, at any point when executing a query group,
	  * values of the referenced table with matching keys are found, they will be lazily resolved using
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory.delay delay]] method of the factory.
	  * This delay loading doesn't require an open transaction when the values are accessed.
	  *
	  * This factory method can be adapted to work with optional keys on both sides, in any combination.
	  *   1. If both the foreign key and referenced key are not null, then the factory argument can
	  *      be converted with method [[net.noresttherein.oldsql.model.RelatedEntityFactory.required required]]
	  *      to enforce that there are no [[net.noresttherein.oldsql.model.RelatedEntityFactory.nonexistent nonexistent]]
	  *      references - that all instances of `R` carry either the value `T`, or the key `K`, or both.
	  *      This is optional, as - assuming the columns of the created mapping are indeed not null -
	  *      method `nonexistent` will not be called when assembling values from a `ResultSet`, as a key will always
	  *      be present.
	  *   1. The case where the foreign key is optional (the columns of this component are nullable),
	  *      but the referenced key type `K` is not an `Option[_]`, then everything proceeds as before,
	  *      except the factory should return a valid value from its `nonexistent` method. This can always be done
	  *      by returning `None` and having an `Option` as the reference type: `R <: Option[_]`. A non-optional
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]] can be turned into
	  *      an optional one using method [[net.noresttherein.oldsql.model.RelatedEntityFactory.optional optional]],
	  *      if needed. Additionally, adding a [[net.noresttherein.oldsql.schema.Buff.SelectDefault SelectDefault]]
	  *      with `None` value will cover the case of `null` key.
	  *   1. If the referenced component is nullable and maps to a `K=:=Option[I]` then the key type used by the factory
	  *      must also be an `Option`; a factory of type `RelatedEntityFactory[K, E, T, R]` can always be adapted
	  *      to type `RelatedEntityFactory[Option[K], E, T, R]` using one of its
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optionalKeys optionalKeys]] method.
	  *      This formally requires the factory to have a valid `nonexistent` value, but the method will never
	  *      be called if this component will always return `Some`. Additionally, the internal index on the referenced
	  *      component will be an optional one, ignoring `null` and `None` values when caching the results.
	  *   1. If both the foreign key and the referenced key are nullable and map to `Option[K]` (or a similar type),
	  *      then the situation is analogous to point 1), except that now the key is an `Option[K]`.
	  *      `RelatedEntityFactory[K, E, T, R]` methods
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optional optional]] and
	  *      [[net.noresttherein.oldsql.model.RelatedEntityFactory.optionalKeys optionalKeys]] may be useful here
	  *      to lift the key type to `Option[K]`. Additionally, internal
	  *      caches will ignore all rows with `null`/`None` values, to avoid erroneous `null == null` matches.
	  *
	  * @tparam M  the mapping for the referenced table, mapped to type `E`.
	  * @tparam C  the mapping for the referenced component in `table`, mapped to type `K`.
	  * @tparam K  the key type: the subject type of both the local key (subcomponent of the returned mapping)
	  *            and referenced key in `table` (returned by `pk`).
	  * @tparam E  the referenced entity type; the value or values of this type will be included in the reference
	  *            values mapped by the returned mapping.
	  * @tparam T  a type derived from `E`, values of which can be constructed from collections of `E`.
	  *            It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *            It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R  the subject type of this mapping: a reference-like type which can hold values of `T`
	  *            and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam MO the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O  a mandatory, arbitrary origin type of the returned mapping.
	  * @param rename a function mapping column names from the referenced component to column names
	  *               in the created mapping.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying component to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs of the created mapping.
	  */
	def apply[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	         (rename :String => String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buffs[R])
	         (table :RelVar[M], pk :M[MO] => C[MO]) :ForeignKeyMapping[M, C, K, R, O] =
		pk(table[MO]) match {
			case col :ColumnMapping =>
				val selector = pk.asInstanceOf[M[MO] => TypedColumn[K, MO]]
				column[M, K, E, T, R, MO, O](rename(col.name), factory, buffs)(table, selector)
					.asInstanceOf[ForeignKeyMapping[M, C, K, R, O]]
			case _ =>
				new RelatedEntityForeignKey[M, C, K, E, T, R, MO, O](rename, factory, buffs)(table, pk)
		}

	/** Create a mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the subjects `K` of the component
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single direct component
	  * of the returned mapping. The key type can be any mapping, comprised of more than one column.
	  *
	  * This method delegates to the more generic, overloaded variant
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:TypedMapping[E,A],C[A]<:TypedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * See its documentation for additional documentation and examples.*
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
	def apply[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, X, O]
	         (rename :String => String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	         (table :RelVar[M], pk :M[X] => C[X]) :ForeignKeyMapping[M, C, K, R, O] =
		pk(table[X]) match {
			case col :ColumnMapping =>
				val selector = pk.asInstanceOf[M[X] => TypedColumn[K, X]]
				column[M, K, E, T, R, X, O](rename(col.name), factory, buffs :_*)(table, selector)
					.asInstanceOf[ForeignKeyMapping[M, C, K, R, O]]
			case _ =>
				new RelatedEntityForeignKey[M, C, K, E, T, R, X, O](rename, factory, buffs)(table, pk)
		}


	/** Create a mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the subjects `K` of the component
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single direct component
	  * of the returned mapping. The key type can be any mapping, comprised of more than one column.
	  * This method delegates to the more generic, overloaded variant
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:TypedMapping[E,A],C[A]<:TypedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * See its documentation for additional documentation and examples.
	  * @tparam M  the mapping for the referenced table, mapped to type `E`.
	  * @tparam C  the mapping for the referenced component in `table`, mapped to type `K`.
	  * @tparam K  the key type: the subject type of both the local key (subcomponent of the returned mapping)
	  *            and referenced key in `table` (returned by `pk`).
	  * @tparam E  the referenced entity type; the value or values of this type will be included in the reference
	  *            values mapped by the returned mapping.
	  * @tparam T  a type derived from `E`, values of which can be constructed from collections of `E`.
	  *            It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *            It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R  the subject type of this mapping: a reference-like type which can hold values of `T`
	  *            and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam MO the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O  a mandatory, arbitrary origin type of the returned mapping.
	  * @param columnPrefix a `String` which will be prepended to names of all columns, being copies of columns
	  *                     of the referenced component, of the returned mapping.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying component to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs of the created mapping.
	  */
	def apply[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	         (columnPrefix :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	         (table :RelVar[M], pk :M[MO] => C[MO]) :ForeignKeyMapping[M, C, K, R, O] =
		apply[M, C, K, E, T, R, MO, O](columnPrefix + _, factory, buffs :_*)(table, pk)

	/** Create a column mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the key type `K` of the column
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single 'subcolumn'
	  * of the returned column. See the documentation of
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:TypedMapping[E,A],C[A]<:TypedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * for additional information and examples.
	  * @tparam M  the mapping for the referenced table, mapped to type `E`.
	  * @tparam K  the key type: the subject type of both the local key (subcolumn of the returned column)
	  *            and referenced key in `table` (returned by `pk`).
	  * @tparam E  the referenced entity type; the value or values of this type will be included in the reference
	  *            values mapped by the returned mapping.
	  * @tparam T  a type derived from `E`, values of which can be constructed from collections of `E`.
	  *            It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *            It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R  the subject type of this column: a reference-like type which can hold values of `T`
	  *            and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam MO the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O  a mandatory, arbitrary origin type of the returned mapping.
	  * @param name the name of the foreign key column.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying column to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs for the created column.
	  */
	def column[M[A] <: TypedMapping[E, A], K, E, T, R, MO, O]
	          (name :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buffs[R])
	          (table :RelVar[M], pk :M[MO] => TypedColumn[K, MO]) :ForeignKeyColumnMapping[M, K, R, O] =
		new RelatedEntityForeignKeyColumn[M, K, E, T, R, MO, O](name, factory, buffs)(table, pk)

	/** Create a column mapping for type `R`, referencing the subjects `E` of `table` with mapping `M[_]`
	  * as some type `X` derived from `E`. Referencing happens by matching the key type `K` of the column
	  * from `table` returned by function `pk` (which needs not to be a primary key), and the single 'subcolumn'
	  * of the returned column. See the documentation of
	  * [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.apply[M[A]<:TypedMapping[E,A],C[A]<:TypedMapping[K,A],K,E,T,R,X,O](rename:String=>String* apply]].
	  * for additional information and examples.
	  * @tparam M  the mapping for the referenced table, mapped to type `E`.
	  * @tparam K  the key type: the subject type of both the local key (subcolumn of the returned column)
	  *            and referenced key in `table` (returned by `pk`).
	  * @tparam E  the referenced entity type; the value or values of this type will be included in the reference
	  *            values mapped by the returned mapping.
	  * @tparam T  a type derived from `E`, values of which can be constructed from collections of `E`.
	  *            It is the type used to hold the referenced values inside the subjects of the returned mapping.
	  *            It is not directly used by the method in any capacity other as one of the type parameters of `factory`.
	  * @tparam R  the subject type of this column: a reference-like type which can hold values of `T`
	  *            and/or values of `K`, for example `Kin[T] =:= Kin[Set[E]]`.
	  * @tparam MO the origin type of the mapping of the referenced table and the referenced component.
	  * @tparam O  a mandatory, arbitrary origin type of the returned mapping.
	  * @param name the name of the foreign key column.
	  * @param factory a factory for the reference type `R` which is the subject type of the created mapping.
	  *                It is used to convert the key type `K` of the underlying column to a value which might
	  *                hold the mentioned key and/or the referenced entity from `table`.
	  * @param buffs buffs for the created column.
	  */
	def column[M[A] <: TypedMapping[E, A], K, E, T, R, MO, O]
	          (name :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	          (table :RelVar[M], pk :M[MO] => TypedColumn[K, MO]) :ForeignKeyColumnMapping[M, K, R, O] =
		new RelatedEntityForeignKeyColumn[M, K, E, T, R, MO, O](name, factory, buffs)(table, pk)

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
	  *              should contain [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
	  * @param table the table owning the foreign key component.
	  * @param fk a function returning the foreign key subcomponent of the table referenced by the created mapping.
	  */
	def inverse[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	           (key :C[O], factory :RelatedEntityFactory[K, E, T, R], buffs :Buffs[R])
	           (table :RelVar[M], fk :M[MO] => ForeignKeyMapping[MappingAt, C, K, _, MO])
			:JoinedEntityComponent[M, C, K, R, O] =
		new InverseForeignKeyMapping[M, C, K, E, T, R, MO, O](key, factory, buffs)(table, fk)

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
	  *              should contain [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
	  * @param table the table owning the foreign key component.
	  * @param fk a function returning the foreign key subcomponent of the table referenced by the created mapping.
	  */
	def inverse[M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	           (key :C[O], factory :RelatedEntityFactory[K, E, T, R], buffs :Buff[R]*)
	           (table :RelVar[M], fk :M[MO] => ForeignKeyMapping[MappingAt, C, K, _, MO])
			:JoinedEntityComponent[M, C, K, R, O] =
		new InverseForeignKeyMapping[M, C, K, E, T, R, MO, O](key, factory, buffs)(table, fk)


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
	  *              should contain [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff.
	  * @param table the table owning the foreign key column.
	  * @param fk a function returning the foreign key column of the table referenced by the created column.
	  */
	def inverseColumn[M[A] <: TypedMapping[E, A], K, E, T, R, MO, O]
	                 (key :TypedColumn[K, O], factory :RelatedEntityFactory[K, E, T, R], buffs :Buffs[R])
	                 (table :RelVar[M], fk :M[MO] => ForeignKeyColumnMapping[MappingAt, K, _, MO])
			:JoinedEntityColumn[M, K, R, O] =
		new InverseForeignKeyColumnMapping[M, K, E, T, R, MO, O](key, factory, buffs)(table, fk)



	private[oldsql] abstract class AbstractRelatedEntityForeignKey
	                               [M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	                               (factory :RelatedEntityFactory[K, E, T, R])
	                               (override val table :RelVar[M], pk :M[MO] => C[MO])
		extends ForeignKeyMapping[M, C, K, R, O]
	{   //todo: rejecting/resolving non-optional refs without a key (currently we always attempt to insert null)
		override type TargetOrigin = MO
		override type KeyOrigin = MO
		//consider: this component being lazy means key must be lazy, meaning extracts and all components lists
		// must be lazy, but adapters (and possibly other code) may eagerly refer to them on construction.
		private[this] val lazyTarget = Lazy(pk(table[MO]))
		override def target :C[MO] = lazyTarget.get
		def references :RelatedEntityFactory[K, E, T, R] = factory

		override def apply[S <: MappingAt[MO]]
		                  (component :C[MO] => S)(implicit local :CounterpartComponent[S, MO]) :local.Component[O] =
			local(this, component(target))

		override def foreign[S <: MappingAt[MO]]
		                    (component :C[MO] => S)(implicit remote :CounterpartComponent[S, MO]) :remote.Component[MO] =
			remote.foreign(this, component(target))

		override def foreign[S](subKey :TypedMapping[S, MO]) :TypedMapping[S, MO] = subKey
		override def foreign[S](subKey :TypedColumn[S, MO])  :TypedColumn[S, MO] = subKey


		override def forKey(key :K) :R = factory(key)

		private[this] val composer = factory.composition

		override def assemble(pieces :Pieces) :Opt[R] = pieces.get(key) match {
			case Got(k) => Got(factory.delay(k, pieces(table).all(lazyTarget.get, k).map(composer(_))))
			case _ => Lack
		}


		override def inverse[N[A] <: TypedMapping[S, A], S, Y, F]
		                    (table :RelVar[N], factory :RelatedEntityFactory[K, S, Y, F], buffs :Buffs[F])
				:JoinedEntityComponent[N, C, K, F, MO] =
			ForeignKeyMapping.inverse[N, C, K, S, Y, F, O, MO](target, factory, buffs)(table, _ => this)

		override def mappingName :String = s"FK($table):${target.mappingName}"
		override def toString :String = s"FK[$factory]($table.$target)"
	}



	/** Mapping of a foreign key represented by component `C` of the target table's mapping `M` to a 'reference'
	  * type `R`, using a specified [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]]
	  * to compose and decompose reference values from/to key values `K`
	  * (and [[net.noresttherein.oldsql.haul.FutureValues FutureValues]] for the referenced table of
	  * [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]).
	  */
	private[oldsql] class RelatedEntityForeignKey
	                      [M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	                      (rename :String => String, factory :RelatedEntityFactory[K, E, T, R],
	                       override val buffs :Buffs[R])
	                      (table :RelVar[M], pk :M[MO] => C[MO])
		extends AbstractRelatedEntityForeignKey[M, C, K, E, T, R, MO, O](factory)(table, pk) with LazyMapping[R, O]
	{
		def this(rename :String => String, factory :RelatedEntityFactory[K, E, T, R], buffs :Seq[Buff[R]])
		        (table :RelVar[M], pk :M[MO] => C[MO]) =
			this(rename, factory, Buffs(buffs :_*))(table, pk)

		private[this] val lazyKey = Lazy { //todo: make target lazy in CoveredMapping instead, then it will really be initialized only when assembling
			new CoveredMapping[C[MO], K, MO, O](target, rename, buffs.cascade(factory.forceKeyOutOf).declare())
		}
		override def key :Component[K] = lazyKey.get

		override def local[S](subKey :TypedMapping[S, MO]) :Component[S] = lazyKey.get.cover(subKey)
		override def local[S](subKey :TypedColumn[S, MO]) :Column[S] = lazyKey.get.cover(subKey)

		override lazy val (extracts, columnExtracts) = {
			val keyExtract = MappingExtract(key)(Extractor.Optional[R, K](factory.keyOf))
			(composeExtracts(keyExtract).updated[Extract, K](key, keyExtract) :ExtractMap) ->
				(composeColumnExtracts(keyExtract) :ColumnExtractMap)
		}

		override lazy val components :Unique[Component[_]] = Unique.single(key)
		override lazy val subcomponents :Unique[Component[_]] = key +: key.subcomponents
		override lazy val columns :Unique[Column[_]] = key.columns
	}



	/** Mapping of a foreign key column to a 'reference' type `R`, using a specified
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]] to compose and decompose reference
	  * values from/to key values `K` (and [[net.noresttherein.oldsql.haul.FutureValues FutureValues]]
	  * for the referenced table of [[net.noresttherein.oldsql.haul.ComponentValues ComponentValues]]).
	  */
	private[oldsql] class RelatedEntityForeignKeyColumn[M[A] <: TypedMapping[E, A], K, E, T, R, MO, O]
	                      (override val name :String, factory :RelatedEntityFactory[K, E, T, R],
	                       override val buffs :Buffs[R])
	                      (override val table :RelVar[M], pk :M[MO] => TypedColumn[K, MO])
		extends AbstractRelatedEntityForeignKey[M, KeyColumn[K]#M, K, E, T, R, MO, O](factory)(table, pk)
		   with ForeignKeyColumnMapping[M, K, R, O] with OptimizedColumn[R, O]
	{
		def this(name :String, factory :RelatedEntityFactory[K, E, T, R], buffs :Seq[Buff[R]])
		        (table :RelVar[M], pk :M[MO] => TypedColumn[K, MO]) =
			this(name, factory, Buffs(buffs :_*))(table, pk)

		private[this] val lazyForm = Lazy(new ForeignKeyColumnForm[K, E, T, R](pk(table[MO]).form, factory))
		override def form :ColumnForm[R] = lazyForm

		private[this] val lazyKey = Lazy {
			val keyBuffs = buffs.cascade(factory.forceKeyOutOf).declare()
			if (target.isInstanceOf[SimpleColumn[_, _]])
				ColumnMapping[K, O](name, keyBuffs)(target.form)
			else
				new OpaqueColumnProxy[K, MO, O](target, name, keyBuffs)
		}
		override def key :Column[K] = lazyKey.get

		override def local[S](subKey :TypedMapping[S, MO]) :Component[S] =
			if (subKey eq target) key.asInstanceOf[Component[S]]
			else throw new IllegalArgumentException(s"$subKey is not the referenced key column $target.")

		override def local[S](subKey :TypedColumn[S, MO]) :Column[S] =
			if (subKey eq target) key.asInstanceOf[Column[S]]
			else throw new IllegalArgumentException(s"$subKey is not the referenced key column $target.")

		override def inverse[N[A] <: TypedMapping[S, A], S, Y, F]
		                    (table :RelVar[N], factory :RelatedEntityFactory[K, S, Y, F], buffs :Buffs[F])
				:JoinedEntityColumn[N, K, F, MO] =
			ForeignKeyMapping.inverseColumn[N, K, S, Y, F, O, MO](target, factory, buffs)(table, _ => this)

		override lazy val columnExtracts :NaturalMap[Column, ColumnExtract] = {
			composeColumnExtracts(key, KeyExtractor(factory))
				.updated[ColumnExtract, R](this, ColumnExtract.ident(this))
		}
		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def apply[S](component :Component[S]) :ColumnExtract[S] = extracts(component)
		override def apply[S](column :Column[S]) :ColumnExtract[S] = columnExtracts(column)

		override def toString :String = super[AbstractRelatedEntityForeignKey].toString
	}






	//consider: extending EffectivelyEmptyMapping - subclasses in MappingFrame and SimpleMapping already do
	//todo: remember that the relationship will not be updated by normal means, but require additional inserts.
	private[oldsql] abstract class AbstractInverseForeignKeyMapping
	                               [M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	                               (localKey :C[O], factory :RelatedEntityFactory[K, E, T, R])
	                               (override val table :RelVar[M],
	                                foreignKey :M[MO] => ForeignKeyMapping[MappingAt, C, K, _, MO])
		extends JoinedEntityComponent[M, C, K, R, O]
	{
		override type TargetOrigin = MO
		override type KeyOrigin = O
		private[this] val fk = Lazy { foreignKey(table[MO]).key }
		//todo: what about buffs key has if it's virtual here?
		//todo: key will have two conflicting extracts in the outer mapping.
		override def target :TypedMapping[K, MO] = fk
		override def key :C[O] = localKey

		override def apply[S <: MappingAt[O]]
		                  (component :C[O] => S)(implicit local :CounterpartComponent[S, O]) :local.Component[O] =
			local(this, component(key))

		override def foreign[S <: MappingAt[O]]
		                    (component :C[O] => S)(implicit remote :CounterpartComponent[S, O]) :remote.Component[MO] =
			remote.foreign(this, component(key))

		override def local[S](subKey :TypedMapping[S, O]) :Component[S] = subKey
		override def local[S](subKey :TypedColumn[S, O]) :Column[S] = subKey

		override def foreign[S](subKey :TypedMapping[S, O]) :TypedMapping[S, MO] = {
			val remote = foreignKey(table[MO])
			remote.local(subKey.withOrigin[remote.KeyOrigin])
		}

		override def foreign[S](subKey :TypedColumn[S, O]) :TypedColumn[S, MO] = {
			val remote = foreignKey(table[MO])
			remote.local(subKey.withOrigin[remote.KeyOrigin])
		}


		override def forKey(key :K) :R = factory(key)

		private[this] val composer :ComposableFrom[T, E] = factory.composition

		override def assemble(pieces :Pieces) :Opt[R] = pieces.get(localKey) match {
			case Got(k) => Got(factory.delay(k, pieces(table).all(fk, k).map(composer(_))))
			case _ => Lack
		}


		override def mappingName :String = s"InverseFK($table):${key.mappingName}"
		override def toString :String = s"InverseFK[$factory]($table.${target.mappingName}):${key.mappingName}"
	}



	/** Mapping of a reference to any number of values from the target `table` whose foreign key (returned by
	  * `foreignKey` argument function) matches a key in the table containing this component. The values are
	  * assembled and disassembled with the help of `factory`
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]]. Note that as the local key
	  * `backer` (which should equal `foreignKey(table[MO]).target`) is most likely existing independently
	  * in the same table, this mapping will most likely require a buff switching off its persistence such as
	  * [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] or mixing in
	  * [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping EffectivelyEmptyMapping]].
	  */ //consider: careful with extracts: duplicates of an independent key are bad, but lack of any extract for non-existing key is worse.
	private[oldsql] class InverseForeignKeyMapping
	                      [M[A] <: TypedMapping[E, A], C[A] <: TypedMapping[K, A], K, E, T, R, MO, O]
	                      (protected override val backer :C[O],
	                       factory :RelatedEntityFactory[K, E, T, R], override val buffs :Buffs[R])
	                      (table :RelVar[M], foreignKey :M[MO] => ForeignKeyMapping[MappingAt, C, K, _, MO])
		extends AbstractInverseForeignKeyMapping[M, C, K, E, T, R, MO, O](backer, factory)(table, foreignKey)
		   with MappedMapping[K, R, O] with OptimizedMappingAssembly
	{
		def this(key :C[O], factory :RelatedEntityFactory[K, E, T, R], buffs :Seq[Buff[R]])
		        (table :RelVar[M], foreignKey :M[MO] => ForeignKeyMapping[MappingAt, C, K, _, MO]) =
			this(key, factory, Buffs(buffs :_*))(table, foreignKey)

		protected override def map :K =?> R = Requisite(factory.absent _)
		protected override def unmap :R =?> K = KeyExtractor(factory)

		protected override def nulls :NullValue[R] =
			if (factory.isRequired) NullValue.eval(factory.nonexistent)
			else NullValue(factory.nonexistent)
	}



	/** Mapping of a reference to any number of values from the target `table` whose foreign key (returned by
	  * `foreignKey` argument function) matches column `key` in the table containing this component. The values are
	  * assembled and disassembled with the help of `factory`
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]]. Note that as the local key
	  * `backer` (which should equal `foreignKey(table[MO]).target`) is most likely existing independently
	  * in the same table, this mapping will most likely require a buff switching off its persistence such as
	  * [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] or mixing in
	  * [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping EffectivelyEmptyMapping]].
	  */ //consider: does an inverse FK column even make sense? Will `key` always exist independently in the table?
	private[oldsql] class InverseForeignKeyColumnMapping[M[A] <: TypedMapping[E, A], K, E, T, R, X, O]
	                      (key :TypedColumn[K, O], factory :RelatedEntityFactory[K, E, T, R],
	                       override val buffs :Buffs[R])
	                      (table :RelVar[M], foreignKey :M[X] => ForeignKeyColumnMapping[MappingAt, K, _, X])
		extends AbstractInverseForeignKeyMapping[M, KeyColumn[K]#M, K, E, T, R, X, O](key, factory)(table, foreignKey)
		   with JoinedEntityColumn[M, K, R, O] with StableColumn[R, O]
	{
		def this(key :TypedColumn[K, O], factory :RelatedEntityFactory[K, E, T, R], buffs :Seq[Buff[R]])
		        (table :RelVar[M], foreignKey :M[X] => ForeignKeyColumnMapping[MappingAt, K, _, X]) =
			this(key, factory, Buffs(buffs :_*))(table, foreignKey)

		override val form :ColumnForm[R] = new ForeignKeyColumnForm[K, E, T, R](key.form, factory)
		override def name :String = key.name
		override def target :TypedColumn[K, X] = super.target.asInstanceOf[TypedColumn[K, X]]

		override lazy val columnExtracts :NaturalMap[Column, ColumnExtract] = {
			composeColumnExtracts(key, KeyExtractor(factory))
				.updated[ColumnExtract, R](this, super.apply(this))
		}

		override def extracts :NaturalMap[Component, ColumnExtract] =
			columnExtracts.asInstanceOf[NaturalMap[Component, ColumnExtract]]


		override def apply[S](component :Component[S]) :ColumnExtract[S] = extracts(component)
		override def apply[S](column :Column[S]) :ColumnExtract[S] = columnExtracts(column)

		override def toString :String = super[AbstractInverseForeignKeyMapping].toString
	}






	/** A [[net.noresttherein.oldsql.schema.ColumnForm ColumnForm]] for a reference type `R` representing an
	  * entity in another table pointed by a foreign key. It uses the provided
	  * [[net.noresttherein.oldsql.model.RelatedEntityFactory RelatedEntityFactory]] to map values read and written
	  * by the backing form `referenced`. All references ''will be absent'' (empty) and thus this form is useful
	  * mostly as a fallback or a stop gap.
	  */
	private class ForeignKeyColumnForm[K, E, Y, R](referenced :ColumnForm[K], factory :RelatedEntityFactory[K, E, Y, R])
		extends AbstractMappedForm[K, R]()(referenced) with SingletonColumnWriteForm[R] with ColumnForm[R]
	{
		override val nulls :NullValue[R] = //this may initialize the factory if its lazy
			if (factory.isRequired) NullValue.eval(factory.nonexistent)
			else NullValue(factory.nonexistent)

		private def entities = factory
//		private[this] val nullable = !factory.isRequired //factory may be lazy, don't init it too early
		protected override def map(s :K) :R = factory.missing(s)
		protected override def unmap(t :R) :K = factory.forceKeyOutOf(t)

		override def set(statement :PreparedStatement, position :Int, value :R) :Unit = {
			val key = if (value == null) Lack else factory.keyOf(value)
			key match {
				case Got(k) => referenced.set(statement, position, k)
				case _ if !factory.isRequired => referenced.setNull(statement, position)
				case _ =>
					throw new MissingKeyException(toString + "no key in " + value + ".")
			}
		}

		override def literal(value :R, inline :Boolean) :String =
			if (value == null)
				referenced.nullLiteral(inline)
			else
				factory.keyOf(value) match {
					case Got(key) => referenced.literal(key, inline)
					case _ => referenced.nullLiteral(inline)
				}

		override def columnLiterals(value :R) :Seq[String] =
			if (value == null)
				referenced.nullColumnLiterals
			else
				factory.keyOf(value) match {
					case Got(key) => referenced.columnLiterals(key)
					case _ => referenced.nullColumnLiterals
				}

		override def sqlType :JDBCType = referenced.sqlType

		override def nullSafe = this

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case key :ForeignKeyColumnForm[_, _, _, _] if key canEqual this =>
				form == key.form && entities == entities
			case _ => false
		}

		override def hashCode :Int = referenced.hashCode * 31 + factory.hashCode

		override val toString :String = s"<$factory[$referenced]FK>"
	}





	trait CounterpartComponent[-M <: MappingAt[X], X] {
		type Component[O] <: MappingAt[O]

		def apply[K, S, O]
		         (ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] { type KeyOrigin = X }, subKey :M) :Component[O]

		def foreign[K, S, O](ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] { type KeyOrigin = X }, subKey :M)
				:Component[ref.TargetOrigin]
	}

	implicit def counterpartComponent[T, X]
			:CounterpartComponent[TypedMapping[T, X], X] { type Component[O] = TypedMapping[T, O] } =
		new CounterpartComponent[TypedMapping[T, X], X] {
			override type Component[O] = TypedMapping[T, O]

			override def apply[K, S, O](ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] { type KeyOrigin = X },
			                            subKey :TypedMapping[T, X]) =
				ref.local(subKey)

			override def foreign[K, S, O](ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] {type KeyOrigin = X },
			                              subKey :TypedMapping[T, X]) =
				ref.foreign(subKey)
		}


	implicit def counterpartColumn[T, X]
			:CounterpartComponent[TypedColumn[T, X], X] { type Component[O] = TypedColumn[T, O] } =
		new CounterpartComponent[TypedColumn[T, X], X] {
			override type Component[O] = TypedColumn[T, O]

			override def apply[K, S, O](ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] { type KeyOrigin = X },
			                            subKey :TypedColumn[T, X]) =
				ref.local(subKey)

			override def foreign[K, S, O](ref :JoinedEntityComponent[MappingAt, MappingAt, K, S, O] {type KeyOrigin = X },
			                              subKey :TypedColumn[T, X]) =
				ref.foreign(subKey)
		}


}