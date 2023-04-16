package net.noresttherein.oldsql.schema.bits

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.{ComposedOf, GenericKinFactory, Kin, KinFactory, PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.Kin.{Derived, One}
import net.noresttherein.oldsql.model.KinFactory.DerivedKinFactory
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.{Buff, Buffs, MappingExtract, RelVar, composeExtracts}
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, MappingOf, TypedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping, StableMapping}
import net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping




/** Base trait for mappings implementing indirect relationships through a join table. This is a one way view
  * of the relationship from the table containing the component to the referenced table. It is typically used as a high
  * level interface for ''many-to-many'' relationships, but it can also hide a part of a ternary relationship
  * if the join table contains more than two foreign keys. The relationship is decomposed into two steps consisting
  * of a [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping DirectRelationshipMapping]] each.
  * Concrete implementations will most likely implement its subtype
  * [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping JoinTableCollectionMapping]]; this trait
  * is extracted as the minimal interface needed to navigate the relationship by the framework code.
  *
  * The implementation is split into two [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping DirectRelationshipMapping]]
  * components, the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.first first]] in this table,
  * referencing/referenced by rows in the join table,
  * and the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.second second]] in the join table,
  * referencing a row in the related [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.table table]].
  * The subject type `S` of this mapping is some kind of a reference type
  * (such as [[net.noresttherein.oldsql.model.Kin Kin]]) to a collection of entities in the related table.
  * It is composed by 'flattening' the above two references into a single value.
  *
  * Typical solutions will have the foreign keys in the join table, referencing primary keys (or other components)
  * of both tables forming the relationship. This means that most of the time,
  * the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.key key]] component of this mapping
  * will be an independently existing component in the same table and this mapping will be
  * [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping effectively empty]]: all columns in its
  * mapping subtree will be not persistent, as they will be handled by another part of this table. This is in no way
  * required though.
  *
  * Relationship mappings such as this instance and
  * `DirectRelationshipMapping`/[[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping ForeignKeyMapping]] are special
  * in that their persistence isn't by intention limited to the actual data mapped by the component (i.e., the ''key''):
  * if the value from the referenced table with matching keys will be read as part of the same transaction -
  * for example as part of a ''select'' joining the tables, but also in a separate ''select'' preceding, following
  * or concurrently executed to the one loading the component - the 'reference' subject `S` of this mapping will
  * contain the referenced values (in an all-or-nothing principle).
  *
  * @tparam J the mapping type of the join table.
  * @tparam T the mapping type of the other table in the relationship.
  * @tparam S the subject type of this mapping: a reference which can hold subjects of table mapping `T`.
  * @tparam O the [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of the mapping, identifying its source
  *           table in SQL expressions.
  * @see [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping]]
  * @author Marcin Mościcki
  */
trait BrokeredRelationshipMapping[J[A] <: MappingAt[A], T[A] <: MappingAt[A], S, O]
	extends RelationshipMapping[T, S, O]
{ outer =>
	/** A 'reference' type to the collection of all values from the join table `J` referencing the key.
	  * The 'value' of this reference is an instance
	  * of [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.SecondRef SecondRef]] or some type
	  * derived from it (such as a collection of such references). Classic ''many-to-many'' relationships using
	  * a join table will have this type wrap the primary key of the owning entity, referenced by the
	  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.Key foreign key]] in the 'join table'
	  * identifying the rows in the target table in the relationship with the owning entity.
	  */
	type FirstRef

	/** A 'reference' type to element or elements in
	  * the target [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.table table]], which are
	  * components of the subject of this mapping. In a classic ''many-to-many'' relationship using a join table
	  * this will be a reference type wrapping the foreign key in the join table to the target table, identifying
	  * a single entity `T[_]#Subject` in the relationship with the owning entity of this mapping.
	  */
	type SecondRef

	/** A marker [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of components of the join table. */
	type JoinOrigin = first.TargetOrigin

	/** A marker [[net.noresttherein.oldsql.schema.Mapping.Origin Origin]] type of components of the target table. */
	type TargetOrigin = second.TargetOrigin

	/** The subject type of the referenced key component in the target table (and the join table key referencing it). */
	type TargetKey = second.Key

	/** The subject type of the referenced key component in the join table
	  * (and the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.key foreign key]] component
	  * of this mapping).
	  */
	override type Key = first.Key

	/** A component mapping a 'reference to references to values' in the target table. The value of `FirstRef` is
	  * a derivative of [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.SecondRef SecondRef]]
	  * (typically, an `Iterable[SecondRef]` subtype), which when flattened, forms the collection of all entities
	  * in relationship with the owning entity included in the subject type `S` of this mapping (again, typically
	  * some `Iterable[T[_]#Subject]` subtype).
	  */
	val first :DirectRelationshipMapping[J, FirstRef, O]

	/** A component of the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.joinTable join table]]
	  * mapping `J` (not this mapping!), mapping a reference to a row or rows in the target table
	  * (for example, `Kin[T[_]#Subject]`).
	  */
	val second :DirectRelationshipMapping[T, SecondRef, JoinOrigin]

	/** A component of this mapping for the key of the subject reference type, that is the component in the owning table
	  * referenced by rows
	  * in the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.joinTable join table]].
	  * This typically is the primary key of the owning table, meaning that this component likely includes
	  * an [[net.noresttherein.oldsql.schema.Buff.Ignored Ignored]] buff making it a mirror of another component,
	  * or is otherwise excluded from database operations in order to prevent duplicate occurrences of the same
	  * column(s) in the SQL, for example by extending
	  * [[net.noresttherein.oldsql.schema.bits.SymlinkMapping SymlinkMapping]]. All reference subjects created
	  * by this mapping include a value of this key (possibly not public), used to identify and fetch the entities
	  * in the relationship with the owner of the reference.
	  */
	override def key :Component[Key]

	/** The key component of the join table matched with local component
	  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.key key]].
	  * @return [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.first first]]`.`[[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.key key]].
	  */
	def firstKey :TypedMapping[Key, JoinOrigin] = first.target

	/** The key component of the join table matched with the key component
	  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.target target]] in the related table `T`.
	  * @return [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.second second]]`.`[[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.key key]].
	  */
	def secondKey :TypedMapping[TargetKey, JoinOrigin] = second.key

	/** A component in the related table (typically its primary key) whose value is matched with foreign key
	  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.secondKey secondKey]] in the join table `J`.
	  * @return [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.second second]]`.`[[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.target target]]
	  */
	def target :TypedMapping[TargetKey, TargetOrigin] = second.target

	/** The join table, linking the enclosing table with the related table through foreign keys to both.
	  * @return [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.first first]]`.`[[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.table table]].
	  */
	def joinTable :RelVar[J] = first.table

	/** The related table.
	  * @return [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.second second]]`.`[[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping.table table]].
	  */
	override def table :RelVar[T] = second.table
}






/** Base trait for mappings of standard ''many-to-many'' relationships using a join table, that is one where
  * the subject of the join table is a value type - rather than an entity with an identity - which can be computed
  * (at least to the minimal required extent) solely based on the relationship. This is a mapping for a reference `R`,
  * a component of an entity on one side of the relationship, rather than the join table itself. The underlying columns
  * are typically already mapped by another component `C` (like the table's primary key), and thus it is a candidate
  * for marking with buffs preventing from its duplicate inclusion in ''insert'' and ''update'' statements.
  * Instead, update of the value of this component triggers a cascading ''delete''/''insert'' to the ''join table''
  * with mapping `J`. It also specifies the arity and direction of the constituent relationships between the tables
  * and the join table as ''to-one'' foreign keys from the join table to both of the entity tables.
  *
  * This trait is a slight extension of interface
  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping BrokeredRelationshipMapping]],
  * adding the mapping types of the keys.
  * @tparam J  The mapping type of rows in the join table, referencing the related table and
  *            the [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] subcomponent
  *            of this mapping.
  * @tparam T  The mapping type of the referenced table.
  * @tparam C  The mapping type of the key component in the enclosing table (typically the primary key),
  *            whose value is matched with some component in the join table `J`.
  *            It is a reference to a subcomponent of the wrapped component
  *            [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.first first]],
  *            mapping an intermediate reference to a collection of join table rows.
  * @tparam TC The mapping for the referenced key (typically the primary key) in the related table `T`.
  * @tparam K  The low level type mapped to the local
  *            [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] component and
  *            the foreign key in the join table referencing the former.
  * @tparam TK The low level key type mapped to the the matching component pair in the related table `T`
  *            and the join table `J`.
  * @tparam R  A reference type such as [[net.noresttherein.oldsql.model.Kin.Many Many]]`[T[_]#Subject]`
  *            being the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type o this mapping.
  * @tparam O  The [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping.
  */
trait JoinTableCollectionMapping[J[A] <: MappingAt[A], T[A] <: MappingAt[A],
                                C[A] <: TypedMapping[K, A], TC[A] <: TypedMapping[TK, A], K, TK, R, O]
	extends BrokeredRelationshipMapping[J, T, R, O]
{
	override def key    :C[O]
	override def target :TC[TargetOrigin] = second.target
	override val first  :JoinedEntityComponent[J, C, K, FirstRef, O]
	override val second :ForeignKeyMapping[T, TC, TK, SecondRef, JoinOrigin]
}




object JoinTableCollectionMapping {
	//todo: all this shit is in a serious need of documentation, while I still can somewhat understand what it does.
	//todo: fill the reference values from haul cache
	//todo: do we really need three implementations, rather then using the most generic one with appropriate factories?

	def apply[J[A] <: MappingAt[A], T[A] <: TypedMapping[E, A],
	          C[A] <: TypedMapping[K, A], TC[A] <: TypedMapping[TK, A], K, TK, E, S, JR, TR, R, TO, O]
	         (key :C[O], first :JoinedEntityComponent[J, C, K, JR, O])
	         (second :J[first.TargetOrigin] => ForeignKeyMapping[T, TC, TK, TR, first.TargetOrigin] { type TargetOrigin = TO })
	         (targetKey :T[TO] => TC[TO], factory :RelatedEntityFactory[JR, E, S, R], buffs :Buffs[R])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] =
		new JoinTableEntityMapping[J, T, C, TC, K, TK, E, S, JR, TR, R, first.TargetOrigin, TO, O](
			key, first)(second, targetKey, factory, buffs
		)

	def kin[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
	        C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, S, TR <: Kin[E], JO, O]
	       (joinTable :RelVar[J], source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	        target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	        linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	        buffs :Buffs[Kin[S]])
	       (implicit composite :S ComposedOf E, link :TypeTag[JE])
            :JoinTableCollectionMapping[J, T, C, TC, K, TK, Kin[S], O] =
		new JoinTableKinMapping[J, T, C, TC, K, TK, JE, E, S, TR, JO, O](
			joinTable, source, target, linkKin, targetKin, buffs
		)

	def many[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
	         C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A], K, TK, JE, E, S, TR <: Kin[E], JO, O]
	        (joinTable :RelVar[J], source :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	         target :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	         linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	         buffs :Buffs[Derived[E, S]])
	        (implicit composite :S ComposedOf E, link :TypeTag[JE])
            :JoinTableCollectionMapping[J, T, C, TC, K, TK, Derived[E, S], O] =
		new JoinTableManyMapping[J, T, C, TC, K, TK, JE, E, S, TR, JO, O](
			joinTable, source, target, linkKin, targetKin, buffs
		)



	abstract class JoinTableKinMappingBase
	               [J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
	                C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A],
	                K, TK, JE, E, S, TR <: Kin[E], R, JO, O]
	               (override val joinTable :RelVar[J], back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E])
	               (implicit composite :S ComposedOf E, link :TypeTag[JE])
		extends JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O]
	{
		override type FirstRef = Kin[Iterable[JE]]
		override type SecondRef = TR

		protected[JoinTableCollectionMapping] class Assembler {

			private[this] val backKey = back(joinTable[JO]).key
			private[this] val inboundFactory = linkKin //TableKin[K, JE, JO](joinTable, backKey).in[Iterable]
			private[this] val forwardKinExtract  = joinTable[JO](forward(joinTable[JO])) :JE =?> Kin[E]
			private[this] val forwardKinProperty = PropertyPath(joinTable[JO](forward(joinTable[JO])).force)
			private[this] val kinPropertyFactory =
				Kin.Property.required(inboundFactory, forwardKinExtract.force).in[Iterable]
			private[this] val factory = TeleKin.required[K, TK, E](kinPropertyFactory, targetKin).as[S]
			private[this] val localKey :C[O] = back(joinTable[JO]).target.withOrigin[O]
			private[this] val keyComposer = ComposableFrom.Iterable[Kin[E]]()
			private[this] val composer = composite.composer

			def absent(key :K) :Derived[E, S] = factory.absent(kinPropertyFactory.absent(key))

			def assemble(pieces :Pieces) :Opt[Derived[E, S]] = pieces.get(localKey) match {
				case Got(k) =>
					val key = Kin.Property(
						inboundFactory.delay(k, pieces(joinTable).all(backKey, k).toOption), forwardKinProperty
					)(keyComposer)
					Got(TeleKin[E, S](key)(composer))
				case _ => Lack
			}
		}
		//fixme: concurrency issue; should work though if all fields in Assembler a final
		@volatile private[this] var ass :Assembler = _
		private[this] var assCache :Assembler = _

		@inline final protected[JoinTableCollectionMapping] def assembler :Assembler = {
			var s = assCache
			if (s == null) {
				s = ass
				if (s == null) {
					s = new Assembler
					ass = s
				}
				assCache = s
			}
			s
		}

		override lazy val first :JoinedEntityComponent[J, C, K, Kin[Iterable[JE]], O] =
			back(joinTable[JO]).inverse[J, JE, Iterable[JE], Kin[Iterable[JE]]](joinTable, linkKin).withOrigin[O]

		override lazy val second = forward(joinTable[JO]).withOrigin[JoinOrigin]
		override lazy val key    = back(joinTable[JO]).target.withOrigin[O]
		override lazy val target :TC[TargetOrigin] = forward(joinTable[JO]).target.withOrigin[TargetOrigin]

		override def columnNamed(name :String) :Column[_] = first.columnNamed(name)

		override def mappingName = s"Many(->$joinTable->$table)"
		override def toString = s"Many[$composite]($key->$joinTable->$table.$target)"

	}



	class JoinTableKinMapping[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
	                          C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A],
	                          K, TK, JE, E, S, TR <: Kin[E], JO, O]
	                         (override val joinTable :RelVar[J], back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                          forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                          linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                          override val buffs :Buffs[Kin[S]])
	                         (implicit composite :S ComposedOf E, link :TypeTag[JE])
		extends JoinTableKinMappingBase[J, T, C, TC, K, TK, JE, E, S, TR, Kin[S], JO, O](
				joinTable, back, forward, linkKin, targetKin)
		   with EffectivelyEmptyMapping[Kin[S], O] with StableMapping
	{
		override def forKey(key :K) :Derived[E, S] = assembler.absent(key)

		override def assemble(pieces :Pieces) :Opt[Derived[E, S]] = assembler.assemble(pieces)
	}



	/** An implementation of a component mapping for a reference [[net.noresttherein.oldsql.model.Kin.Derived]]`[E, S]`
	  * to a collection (of type `S`) of entities `E` mapped by row mapping `T` of a table on the other side
	  * of a ''many-to-many'' relationship.
	  * @tparam C  The mapping type of the key component in the enclosing table (typically the primary key),
	  *            whose value is matched with some component in the join table `J`.
	  *            It is a reference to a subcomponent of the wrapped component
	  *            [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.first first]],
	  *            mapping an intermediate reference to a collection of join table rows.
	  * @tparam K  The low level type mapped to the local
	  *            [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] component and
	  *            the foreign key in the join table referencing the former.
	  * @tparam J  The mapping type of rows in the join table, referencing the related table and
	  *            the [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] subcomponent
	  *            of this mapping.
	  * @tparam JE A low level in-application representation of a single row in the join table `J`.
	  * @tparam JO An arbitrary origin type used to mark components of the join table `J`.
	  * @tparam TR A low level `Kin` reference mapped to the foreign key from the join table `J` referencing
	  *            the table `T` on the other side of the relationship.
	  * @tparam T  The mapping type of the referenced table.
	  * @tparam E  The referenced entity type, the subject type of the mapping `T` for rows in the referenced table.
	  * @tparam TC The mapping for the referenced key (typically the primary key) in the related table `T`.
	  * @tparam TK The low level key type mapped to the the matching component pair in the related table `T`
	  *            and the join table `J`.
	  * @tparam S  A collection of `E` (i.e., any type with type class
	  *            `S `[[net.noresttherein.oldsql.model.ComposedOf.CollectionOf CollectionOf]]` E`).
	  *            It is the 'content' type of the `Kin` reference mapped by this component.
	  * @tparam O  The [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping.
	  * @param joinTable A table containing foreign keys to both the table owning this component and table `T`
	  *                  on the other side of the relationship.
	  * @param back      A getter for the foreign key component in the join table pointing to a single row
	  *                  in the table owning this component.
	  * @param forward   A getter for the foreign key component in the join table pointing to a single row
	  *                  in table `T` on the other side of the relationship.
	  * @param linkKin   A factory for low-level references to subsets of rows in the join table defining
	  *                  the set of the referenced entities. It is used as the 'key' type of the references mapped
	  *                  by this component (i.e. content of 'absent' kin).
	  * @param targetKin A factory of `kin` representing the foreign key from the join table to the referenced
	  *                  table.
	  * @param buffs     The buffs of this component which, barring special circumstances, should include
	  *                  [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] and
	  *                  [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] (or a buff implying them).
	  */
	class JoinTableManyMapping[J[A] <: TypedMapping[JE, A], T[A] <: TypedMapping[E, A],
	                           C[A] <: BaseMapping[K, A], TC[A] <: BaseMapping[TK, A],
	                           K, TK, JE, E, S, TR <: Kin[E], JO, O]
	                          (override val joinTable :RelVar[J], back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	                           forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO],
	                           linkKin: => DerivedKinFactory[K, JE, Iterable[JE]], targetKin: => KinFactory[TK, E, E],
	                           override val buffs :Buffs[Derived[E, S]])
	                          (implicit composite :S ComposedOf E, link :TypeTag[JE])
		extends JoinTableKinMappingBase[J, T, C, TC, K, TK, JE, E, S, TR, Derived[E, S], JO, O](
				joinTable, back, forward, linkKin, targetKin)
		   with EffectivelyEmptyMapping[Derived[E, S], O] with StableMapping
	{
		override def forKey(key :K) :Derived[E, S] = assembler.absent(key)

		override def assemble(pieces :Pieces) :Opt[Derived[E, S]] = assembler.assemble(pieces)
	}



	/** A generic implementation of a component mapping for a reference type `R` to a collection `S` of entities `E`
	  * mapped by mapping `T` to rows in the table on the other side of the relationship.
	  * It wraps a component `C` for a [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]]
	  * of type `K`, referenced by rows in the join table. This is typically an independently existing component
	  * of the owning table, such as its primary key.
	  * @tparam J      The mapping type of rows in the join table, referencing the related table and
	  *                the [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] subcomponent
	  *                of this mapping.
	  * @tparam T      The mapping type of the referenced table on the other side of the relationship.
	  * @tparam E      The referenced entity type, the subject type of the mapping `T` for rows in the referenced table.
	  * @tparam C      The mapping type of the key component in the enclosing table (typically its primary key),
	  *                whose value is matched with some component in the join table `J`.
	  *                It is a reference to a subcomponent of the wrapped component
	  *                [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.first first]],
	  *                mapping an intermediate reference to a collection of join table rows.
	  * @tparam K      The low level type mapped to the local
	  *                [[net.noresttherein.oldsql.schema.bits.JoinTableCollectionMapping.key key]] component and
	  *                the foreign key in the join table referencing the former.
	  * @tparam TC     The mapping for the referenced key (typically the primary key) in the related table `T`.
	  * @tparam TK     The low level key type mapped to the a matching component pair in the related table `T`
	  *                and the join table `J`.
	  * @tparam JR     A low level reference type pointing to the rows in the join table whose (first) foreign key match
	  *                the value of the key `K` in the owning table.
	  *                It is the [[net.noresttherein.oldsql.schema.bits.ForeignKeyMapping.inverse inverse]] side
	  *                of the foreign key from the join table pointing to the owning entity of this component.
	  *                It constitutes the first 'leg' of the relationship.
	  * @tparam JO     An arbitrary origin type used by the join table mapping and its components.
	  * @tparam TR     A low level foreign key component of the mapping `J` of the join table, pointing
	  *                to a row in the table `T` on the other side of the relationship.
	  *                It constitutes the second 'leg' of the relationship.
	  * @tparam TO     An arbitrary origin type used by the referenced table mapping and its components.
	  * @tparam S      The 'content type' of the reference type `R` being the subject of this mapping, i.e. some kind
	  *                of a collection type containing entities `E` from the other side of the relationship.
	  * @tparam R      A reference type such as [[net.noresttherein.oldsql.model.Kin.Many Many]]`[E]`
	  *                being the [[net.noresttherein.oldsql.schema.Mapping.Subject subject]] type o this mapping.
	  * @tparam O      The [[net.noresttherein.oldsql.schema.Mapping.Origin origin]] type of this mapping.
	  * @param key     A component for a key in the owning table (typically its primary key) referenced by rows
	  *                in the join table.
	  * @param first   A low level component wrapping `key` and representing the link between it and a matching
	  *                foreign key in the join table, from the point of view of this component and its owning table.
	  *                Its subject `JR` is some sort of a reference type which can hold information identifying
	  *                a collection of rows (existing or future) in the join table.
	  * @param out     A component of a foreign key in the join table `J` for a reference type `TR` pointing
	  *                to a single row in the referenced table `T` on the other side of the relationship.
	  * @param tk      A getter for the component `TC` for the referenced key of the mapping `T` on the other side
	  *                of the relationship.
	  * @param factory A factory of references `R` being the subject type of this mapping, using references `JR`
	  *                to rows in the join table as its 'key' type (used in empty references).
	  * @param buffs   The buffs of this component which, barring special circumstances, should include
	  *                [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] and
	  *                [[net.noresttherein.oldsql.schema.Buff.NoUpdate NoUpdate]] (or a buff implying them).
	  */
	class JoinTableEntityMapping[J[A] <: MappingAt[A], T[A] <: TypedMapping[E, A],
	                             C[A] <: TypedMapping[K, A], TC[A] <: TypedMapping[TK, A],
	                             K, TK, E, S, JR, TR, R, JO, TO, O]
	                            (override val key :C[O],
	                             override val first :JoinedEntityComponent[J, C, K, JR, O] { type TargetOrigin = JO })
	                            (out :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO] { type TargetOrigin = TO },
	                             tk :T[TO] => TC[TO], factory :RelatedEntityFactory[JR, E, S, R],
	                             override val buffs :Buffs[R])
		extends JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] with LazyMapping[R, O]
	{
		override type FirstRef = JR
		override type SecondRef = TR
		private[this] val lazyTarget = Lazy(tk(table.row[second.TargetOrigin]))
		override lazy val second :ForeignKeyMapping[T, TC, TK, TR, JO] { type TargetOrigin = TO } =
			out(first.table.row[JoinOrigin])

		override def firstKey :TypedMapping[K, JoinOrigin] = first.target
		override def secondKey :TypedMapping[TK, JoinOrigin] = second.key
		override def target :TC[TO] = lazyTarget.get
		override def joinTable :RelVar[J] = first.table
		override def table :RelVar[T] = second.table

		override def forKey(key :K) :R = factory.absent(first.forKey(key))

		override def assemble(pieces :Pieces) :Opt[R] = pieces.get(first) match {
			case Got(ref) => Got(factory(ref))
			case _ => Lack
		}


		override lazy val extracts :NaturalMap[Component, Extract] = {
			val inboundExtract = MappingExtract(first)(Extractor.Optional[R, JR](factory.keyOf(_)))
			composeExtracts(inboundExtract).updated[Extract, JR](first, inboundExtract)
		}

		override def components :Unique[Component[_]] = Unique.single(first)

		override def mappingName = s"N-to-N(->$joinTable->$table)"
		override def toString = s"N-to-N[$factory]($key->$joinTable->$table.$target)"
	}

}

