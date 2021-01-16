package net.noresttherein.oldsql.schema.bits

import net.noresttherein.oldsql.collection.{NaturalMap, Opt, Unique}
import net.noresttherein.oldsql.collection.Opt.Got
import net.noresttherein.oldsql.model.RelatedEntityFactory
import net.noresttherein.oldsql.morsels.Lazy
import net.noresttherein.oldsql.schema.Mapping.{MappingAt, RefinedMapping}
import net.noresttherein.oldsql.schema.bases.{BaseMapping, LazyMapping}
import net.noresttherein.oldsql.schema.{composeExtracts, Buff, MappingExtract}
import net.noresttherein.oldsql.schema.Relation.RelVar






trait JoinTableMapping[L[A] <: RefinedMapping[EL, A], R[A] <: RefinedMapping[ER, A],
                       CL[A] <: RefinedMapping[KL, A], CR[A] <: RefinedMapping[KR, A], KL, KR, EL, ER, S, O]
	extends BaseMapping[S, O]
{
	def left :ForeignKeyMapping[L, CL, KL, EL, O]
	def right :ForeignKeyMapping[R, CR, KR, ER, O]
}




//Liaison, Broker, Intermediate, connection
//trait BrokeredRelationshipMapping

trait BrokeredRelationshipMapping[J[A] <: MappingAt[A], T[A] <: MappingAt[A], S, O]
	extends RelationshipMapping[T, S, O]
{ outer =>
	type JoinOrigin = inbound.TargetOrigin
	type TargetOrigin = outbound.TargetOrigin
	override type Key = inbound.Key
	type TargetKey = outbound.Key
	type FirstRef
	type SecondRef
	val inbound :DirectRelationshipMapping[J, FirstRef, O]
	val outbound :DirectRelationshipMapping[T, SecondRef, JoinOrigin]
	def key :Component[Key] //= inbound.key
	def inboundKey :RefinedMapping[Key, JoinOrigin] //= inbound.target
	def outboundKey :RefinedMapping[TargetKey, JoinOrigin] //= outbound.key
	def target :RefinedMapping[TargetKey, TargetOrigin] //= outbound.target
	def joinTable :RelVar[J] //= inbound.table
	def table :RelVar[T] //= outbound.table
}



/**
  * @author Marcin Mościcki
  */
trait JoinTableCollectionMapping[J[A] <: MappingAt[A], T[A] <: MappingAt[A],
                                C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A], K, TK, R, O]
	extends BrokeredRelationshipMapping[J, T, R, O]
{
	override def key :C[O]
	override def target :TC[TargetOrigin]
	override val inbound :ForeignKeyMapping[J, C, K, FirstRef, O]
	override val outbound :ForeignKeyMapping[T, TC, TK, SecondRef, JoinOrigin]
}






object JoinTableCollectionMapping {

	def apply[J[A] <: MappingAt[A], T[A] <: RefinedMapping[E, A],
	          C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A], K, TK, E, S, JR, TR, R, TO, O]
	         (key :C[O], inbound :ForeignKeyMapping[J, C, K, JR, O])
	         (out :J[inbound.TargetOrigin] => ForeignKeyMapping[T, TC, TK, TR, inbound.TargetOrigin] { type TargetOrigin = TO })
	         (tk :T[TO] => TC[TO], factory :RelatedEntityFactory[JR, E, S, R], buffs :Seq[Buff[R]])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] =
		new JoinTableEntityMapping[J, T, C, TC, K, TK, E, S, JR, TR, R, inbound.TargetOrigin, TO, O](
			key, inbound)(out, tk, factory, buffs
		)

/*
	def apply[J[A] <: RefinedMapping[JE, A], T[A] <: RefinedMapping[E, A],
	          C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A], K, TK, JE, E, X, S, JR, TR, R, JO, TO, O]
	         (key :C[O], inboundFactory :RelatedEntityFactory[K, JE, X, JR], inboundBuffs :Seq[Buff[JR]])
	         (joinTable :RelVar[J], back :J[JO] => ForeignKeyMapping[MappingAt, C, K, _, JO],
	          forward :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO] { type TargetOrigin = TO })
	         (tk :T[TO] => TC[TO], buffs :Seq[Buff[R]])
			:JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] =
		JoinTableCollectionMapping[J, T, C, TC, K, TK, E, S, JR, TR, R, TO, O]( //not the factory method for the known origin type
			key, new InverseForeignKeyMapping[J, C, K, JE, X, JR, JO, O](key, inboundFactory, inboundBuffs)(joinTable, back)
//			key, ForeignKeyMapping.inverse(key, inboundFactory, inboundBuffs :_*)(joinTable, back)
		)(forward)(tk, ???, buffs)
*/



	class JoinTableEntityMapping[J[A] <: MappingAt[A], T[A] <: RefinedMapping[E, A],
	                             C[A] <: RefinedMapping[K, A], TC[A] <: RefinedMapping[TK, A],
	                             K, TK, E, S, JR, TR, R, JO, TO, O]
	                            (override val key :C[O],
	                             override val inbound :ForeignKeyMapping[J, C, K, JR, O] { type TargetOrigin = JO })
	                            (out :J[JO] => ForeignKeyMapping[T, TC, TK, TR, JO] { type TargetOrigin = TO },
	                             tk :T[TO] => TC[TO], factory :RelatedEntityFactory[JR, E, S, R],
	                             override val buffs :Seq[Buff[R]])
		extends JoinTableCollectionMapping[J, T, C, TC, K, TK, R, O] with LazyMapping[R, O]
	{
		override type FirstRef = JR
		override type SecondRef = TR
		private[this] val lazyTarget = Lazy(tk(table.row[outbound.TargetOrigin]))
		override lazy val outbound = out(inbound.table.row[JoinOrigin])
		override def inboundKey :RefinedMapping[K, JoinOrigin] = inbound.target
		override def outboundKey :RefinedMapping[TK, JoinOrigin] = outbound.key
		override def target :TC[TO] = lazyTarget.get
		override def joinTable :RelVar[J] = inbound.table
		override def table :RelVar[T] = outbound.table

		override def forKey(key :K) :R = factory.absent(inbound.forKey(key))

		override def assemble(pieces :Pieces) :Opt[R] = pieces.get(inbound) match {
			case Got(ref) => Got(factory(ref))
			case _ => None
		}


		override lazy val extracts :NaturalMap[Component, Extract] = {
			val inboundExtract = MappingExtract.opt(inbound)(factory.keyOf)
			composeExtracts(inboundExtract).updated[Extract, JR](inbound, inboundExtract)
		}

		override def components :Unique[Component[_]] = Unique.single(inbound)
	}




/*
	class IndirectlyRelatedEntityFactory[K, JE, JT, JR, TK, E, TR, C, R]
	                                    (keyFactory :RelatedEntityFactory[K, JE, JT, JR],
	                                     valueFactory :RelatedEntityFactory[TK, E, E, TR],
	                                     factory :RelatedEntityFactory[JR, E, C, R],
	                                     links :Iterable[TR] => Iterable[JE], reference :JE => TR)
	                                    (implicit result :C ComposedOf E)
		extends RelatedEntityFactory[JR, E, C, R] with
	{
		implicit override def composition :C ComposableFrom E = result.composer

		override def delay(key :JR, value : => Option[C]) :R = factory.delay(key, value)
		override def apply(key :JR, value :Option[C]) :R = factory(key, value)
		override def present(value :C) :R = factory.present(value)
		override def absent(key :JR) :R = factory(key)
		override def missing(key :JR) :R = factory(key)

		override def keyFor(value :C) :Option[JR] = keyFrom(result.decomposer(value))
		override def keyFrom(item :E) :Option[JR] = keyFrom(item::Nil)
		override def keyFrom(items :Iterable[E]) :Option[JR] =
			Some(keyFactory.present(keyFactory.composition(links(items.map(valueFactory.present)))))

		override def keyOf(ref :R) :Option[JR] = ref match {
			case this.factory()
			case Derived(TeleKin(k)) => Some(k.asInstanceOf[Kin[Iterable[Kin[E]]]])
			case Present(result.decomposer(values)) =>
				Some(keyFactory.present(values.map(valueFactory.present)))
			case _ => None
		}

		override def required :KinFactory[Kin[Iterable[Kin[E]]], E, T] =
			new TeleKinFactory[K, K2, E, T](keyFactory.required, valueFactory.required)
				with RequiredKinFactory[Kin[Iterable[Kin[E]]], E, T, Kin[T]]

		override def isRequired :Boolean = false


		override def equivalencyToken :Any = (keyFactory, valueFactory, factory.equivalencyToken)

		override def canEqual(that :Any) :Boolean = that.isInstanceOf[TeleKinFactory[_, _, _, _]]

		override def equals(that :Any) :Boolean = that match {
			case self :AnyRef if self eq this => true
			case other :TeleKinFactory[_, _, _, _] if other canEqual this =>
				other.equivalencyToken == equivalencyToken && other.result == result
			case _ => false
		}

		override def hashCode :Int = (keyFactory.hashCode * 31 + valueFactory.hashCode) * 31 + result.hashCode

		override def toString :String = factory.toString + "(" + keyFactory + "->" + valueFactory + ")"
//			result.toString + (if (isRequired) "" else "?") + "(" + keyFactory + " -> " + valueFactory + ")"
	}
*/

}

