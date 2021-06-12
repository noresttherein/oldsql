package net.noresttherein.oldsql.schema.bits

import scala.reflect.runtime.universe.TypeTag

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.{Got, Lack}
import net.noresttherein.oldsql.model.{ComposedOf, Kin, KinFactory, PropertyPath, RelatedEntityFactory}
import net.noresttherein.oldsql.model.ComposedOf.ComposableFrom
import net.noresttherein.oldsql.model.Kin.{Derived, Many}
import net.noresttherein.oldsql.model.KinFactory.DerivedKinFactory
import net.noresttherein.oldsql.morsels.{Extractor, Lazy}
import net.noresttherein.oldsql.morsels.Extractor.=?>
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping, StableMapping}
import net.noresttherein.oldsql.schema.{composeExtracts, Buff, Buffs, MappingExtract}
import net.noresttherein.oldsql.schema.RelVar
import net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping






trait JoinTableMapping[L[A] <: RefinedMapping[EL, A], R[A] <: RefinedMapping[ER, A],
                       CL[A] <: RefinedMapping[KL, A], CR[A] <: RefinedMapping[KR, A], KL, KR, EL, ER, S, O]
	extends BaseMapping[S, O]





/** Base trait for mappings implementing indirect relationships through a join table. This is a one way view
  * of the relationship from the table containing the component to the referenced table. It is typically used as a high
  * level interface for ''many-to-many'' relationships, but it can also hide a part of a ternary relationship
  * if the join table contains more than two foreign keys. The relationship is decomposed into two steps consisting
  * of a [[net.noresttherein.oldsql.schema.bits.DirectRelationshipMapping DirectRelationshipMapping]] each.
  * The first step is the reference to a collection of join table entries mapped by `J` - these will typically be
  * simply pairs of foreign keys, referencing both tables in the relationship, but can hold also other information,
  * such as ordering field if the subject type of this mapping (or its view from the other direction) is an ordered
  * collection, as well as additional foreign keys in case of non-binary relationships.
  *
  * Typical solutions will have the foreign keys in the join table, referencing primary keys (or other components)
  * of both tables forming the relationship. This means that most of the time,
  * the [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping.key key]] component of this mapping
  * will be an independently existing component in the same table and this mapping will be
  * [[net.noresttherein.oldsql.schema.support.EffectivelyEmptyMapping effectively empty]]: all columns in its
  * mapping subtree will be not persistent, as they will be handled by another part of this table. This is in no way
  * required though.
  *
  * Relationship mappings such as this instance and `DirectRelationshipMapping`/`ForeignKeyMapping` are special
  * in that their persistence isn't in intention limited to the actual data mapped by the component (i.e., the ''key''):
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
	/** The 'reference' type to the collection of all values from the join table `J` referencing the key  */
	type FirstRef
	type SecondRef
	type JoinOrigin = first.TargetOrigin
	type TargetOrigin = second.TargetOrigin
	type TargetKey = second.Key
	override type Key = first.Key
	val first :DirectRelationshipMapping[J, FirstRef, O]
	val second :DirectRelationshipMapping[T, SecondRef, JoinOrigin]
	def key :Component[Key]
	def firstKey :RefinedMapping[Key, JoinOrigin] = first.target
	def secondKey :RefinedMapping[TargetKey, JoinOrigin] = second.key
	def target :RefinedMapping[TargetKey, TargetOrigin] = second.target
	def joinTable :RelVar[J] = first.table
	def table :RelVar[T] = second.table
}






/** Base trait for mappings of standard ''many-to-many'' relationships using a join table, that is one where
  * the subject of the join table is a value type - rather than an entity with an identity - which can be computed
  * (at least to the minimal required extent) solely based on the relationship. It also specifies the arity and
  * direction of the constituent relationships between the tables and the join table as ''to-one'' foreign keys
  * from the join table to both of the entity tables.
  *
  * This trait is a slight expansion of interface
  * [[net.noresttherein.oldsql.schema.bits.BrokeredRelationshipMapping BrokeredRelationshipMapping]],
  * adding the mapping types of the keys.
  */
trait JoinTableCollectionMapping[J[A] <: MappingAt[A], T[A] <: MappingAt[A],
                                C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A], K, TK, R, O]
	extends BrokeredRelationshipMapping[J, T, R, O]
{
	override def key :C[O]
	override def target :TC[TargetOrigin] = second.target//.withOrigin[TargetOrigin]
	override val first :JoinedEntityComponent[J, C, K, FirstRef, O]
	override val second :ForeignKeyMapping[T, TC, TK, SecondRef, JoinOrigin]
}






object JoinTableCollectionMapping {

	def apply[J[A] <: MappingAt[A], T[A] <: RefinedMapping[E, A],
	          C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A], K, TK, E, S, JR, TR, R, TO, O]
	         (key :C[O], first :JoinedEntityComponent[J, C, K, JR, O])
	         (second :J[first.TargetOrigin] => ForeignKeyMapping[T, TC, TK, TR, first.TargetOrigin] { type TargetOrigin = TO })
	         (targetKey :T[TO] => TC[TO], factory :RelatedEntityFactory[JR, E, S, R], buffs :Buffs[R])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] =
		new JoinTableEntityMapping[J, T, C, TC, K, TK, E, S, JR, TR, R, first.TargetOrigin, TO, O](
			key, first)(second, targetKey, factory, buffs
		)

	def kin[J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
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

	def many[J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
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
	               [J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
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
			private[this] val forwardKinExtract = joinTable[JO](forward(joinTable[JO])) :JE =?> Kin[E]
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
		override lazy val key = back(joinTable[JO]).target.withOrigin[O]
		override lazy val target :TC[TargetOrigin] = forward(joinTable[JO]).target.withOrigin[TargetOrigin]

		override def columnNamed(name :String) :Column[_] = first.columnNamed(name)

		override def mappingName = s"Many(->$joinTable->$table)"
		override def toString = s"Many[$composite]($key->$joinTable->$table.$target)"

	}



	class JoinTableKinMapping[J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
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



	class JoinTableManyMapping[J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
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



	class JoinTableEntityMapping[J[A] <: MappingAt[A], T[A] <: RefinedMapping[E, A],
	                             C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A],
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

		override def firstKey :RefinedMapping[K, JoinOrigin] = first.target
		override def secondKey :RefinedMapping[TK, JoinOrigin] = second.key
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

